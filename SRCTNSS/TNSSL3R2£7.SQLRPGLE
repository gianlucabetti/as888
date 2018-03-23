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
058000130408     D ZZ              S              4  0
058100091006     D Wdatael         S              8  0
058200091007     D w010a           S             10
058300130408     D w010b           S             10
058400110801     D PTIPOCLI        S             12
058500110801     D skicli          S            132
058600130408     D Wgiro           S              1  0
058700090204     d dataiso         s               d   datfmt(*iso)
058800090204     d waas            s                   like(tasaas)
058900090204     d wmgs            s                   Like(tasmgs)
059000090224     d w_peso          s                   Like(taspkf)
059100090204     d k_fil           s                   Like(wmpfil)
059200090204     d k_tip           s                   Like(wmptip)
059300090204     d k_dat           s                   Like(wmpdal)
059400090205     d k_trd           s                   Like(ar5trd) inz('TRS')
059500091006     d savccm          s                   Like(tasccm)
059600091006     d savctr          s                   Like(tasctr)
059700110715     d savkpjbu        s                   Like(kpjbu)
059800120903     d ks_tie          s                   Like(wsstie)
059900120903     d ks_tip          s                   Like(wsstip)
060000120903     d ks_lin          s                   Like(wsslin)
060100120903     d ks_sca          s                   Like(wsskgf)
060200111028     d
060300111028     d titas00ds     E DS                  extname(titas00f)
060400090601     d
060500111028     d wrkgetlista     s          25767    varying
060600120904     d*
060700120904      * ---------------------------------------------------------
060800120904     i*
060900120904     iwslss500
061000120904     I              wsstkg                      wsstkg_ol
061100120904     I              wsstmc                      wsstmc_ol
061200120904     I              wsstkge                     wsstkge_ol
061300120904     I              wsstmce                     wsstmce_ol
061400120904     I              wsstkgh                     wsstkgh_ol
061500120904     I              wsstmch                     wsstmch_ol
061600120904     i*
061700120904     iwslss550
061800120904     I              wsstkg                      wsstkg_ex
061900120904     I              wsstmc                      wsstmc_ex
062000120904     I              wsstkge                     wsstkge_ex
062100120904     I              wsstmce                     wsstmce_ex
062200120904     I              wsstkgh                     wsstkgh_ex
062300120904     I              wsstmch                     wsstmch_ex
062400120904      * ---------------------------------------------------------
062500970527     C* RICEVIMENTO PARAMETRI
062600970527     C     *ENTRY        PLIST
062700970527     C                   PARM                    KPJBA
062800970527     C                   MOVEL     §DAL          WAAS
062900970527     C                   MOVE      §DAL          WMGS
063000090601     C                   MOVE      §tipo         Wtipo             1
063100970528     C*
063200970529     C                   EXSR      CARINZ
063300130408     c
063400130408     c                   eval      WGiro=1
063500130408     c                   exsr      Elabora
063600130408     c
063700130408     c* per i clienti particolari eseguo nuovo  SQL  su tasksc
063800130408     c                   if        §stat= 'P'
063900130408     c                   eval      WGiro=2
064000130408     c                   exsr      PrepaSQL2
064100130408     c                   exsr      Elabora
064200130408     c                   endif
064300130408     c
064400090206     c*
064500090206     C* SCARICO DS SU FILE
064600090206     c                   exsr      scrive_file
064700110801     c                   if        §stat=' '
064800110801     c                   eval      ptipocli='EXTRA BUDGET'
064900110801     c                   else
065000110801     c                   eval      ptipocli='PARTICOLARI '
065100110801     c                   endif
065200110801     c
065300090206     c                   except    testa
065400110801     C*
065500110801     c                   z-add     1             k
065600110801     c                   clear                   conta             3 0
065700110801     c                   clear                   skicli
065800110801     c                   dow       ksc_ex(k)<>*blanks
065900110801     c
066000110801     c                   if        §stat=' '
066100110801
066200110801     c                   eval      skicli=%trim(skicli)+' '+
066300110801     c                             %subst(ksc_ex(k):1:7)
066400110801     c                   add       7             conta
066500110801     c*
066600110801     c                   if        %subst(ksc_ex(k):8:3)<>'   '
066700110801     c                   eval      skicli=%trim(skicli)+'/'+
066800110801     c                             %subst(ksc_ex(k):8:3)
066900110801     c                   add       4             conta
067000110801     c                   endif
067100110801     c                   add       1             conta
067200110801     c
067300110801     c
067400110801     c                   if        conta>120
067500110801     c                   except    detksc
067600110801     c                   clear                   conta
067700110801     c                   clear                   skicli
067800110801     c                   endif
067900110801     c
068000110801     c                   else
068100110801     c
068200110801     c                   eval      skicli=%trim(skicli)+' '+
068300110801     c                             %subst(ksc_ex(k):1:7)
068400110801     c                   add       1             conta
068500110801     c
068600110801     c                   if        conta=16
068700110801     c                   except    detksc
068800110801     c                   clear                   conta
068900110801     c                   clear                   skicli
069000110801     c                   endif
069100110801     c
069200110801     c                   endif
069300110801     c
069400110801     c                   add       1             k
069500110801     c                   enddo
069600110801     c
069700110801     c                   if        conta>0
069800110801     c                   except    detksc
069900110801     c                   endif
070000970528     C*
070100970528     C                   SETON                                        LR
070200970519     C*
070300130408     C**********************************************************
070400130408     c     Elabora       BEGSR
070500130408     C**********************************************************
070600130408     C*
070700130408     c                   clear                   savccm
070800130408     C* LETTURA TITAS
070900130408     c                   if        §stat=' '
071000130408     C     KTAS          SETLL     TITAS38C
071100130408     c                   else
071200130408
071300130408     C/EXEC SQL
071400130408     C+ PREPARE s1 FROM :wrkgetlista
071500130408     C/END-EXEC
071600130408     C
071700130408     C/EXEC SQL
071800130408     C+ DECLARE A1 CURSOR FOR S1
071900130408     C/END-EXEC
072000130408     C
072100130408     C/EXEC SQL
072200130408     C+ OPEN a1
072300130408     C/END-EXEC
072400130408
072500130408     c                   endif
072600130408     c
072700130408     C                   DO        *HIVAL
072800130408     c
072900130408     c                   if        §stat=' '
073000130408     C                   READ      TITAS38C
073100130408     c* Termina il ciclo se fine file o per data spedizione > data al
073200130408     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
073300130408     c                   leave
073400130408     c                   endif
073500130408     c
073600130408     c                   else
073700130408     C/EXEC SQL
073800130408     C+ FETCH NEXT FROM a1 INTO :titas00ds
073900130408     C/END-EXEC
074000130408
074100130408     c                   select
074200130408
074300130408     c                   when      sqlcod = 100
074400130408     C/EXEC SQL
074500130408     C+ CLOSE a1
074600130408     C/END-EXEC
074700130408     c                   leave
074800130408     c                   when      sqlcod < 0
074900130408     c                   seton                                        H1
075000130408     C/EXEC SQL
075100130408     C+ CLOSE a1
075200130408     C/END-EXEC
075300130408     c                   leave
075400130408     c                   endsl
075500130408     c
075600130408     c                   endif
075700130408     c
075800130408     c* Scarto in base al tipo bolla
075900130408     c                   if        %lookup(tastbl:tbo)=0
076000130408     c                   iter
076100130408     c                   endif
076200130408     C*
076300130408     c
076400130408     c* EDPES : 01/06/09
076500130408     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
076600130408     c***                z-add     taspkf        w_peso
076700130408     c* EDPES:  01/09/09
076800130408     c**  riutilizzo il peso VDL
076900130408     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
077000130408     c* 1. VDL totale
077100130408     c* 2. VDL parziale se > di TASPKF
077200130408     c* 3. TASPKF
077300130408     c                   clear                   w_peso
077400130408     c                   if        tasncp=tasncl or (tasncp>0 and taspkc>taspkf)
077500130408     c                   z-add     taspkc        w_peso
077600130408     c                   else
077700130408     c                   z-add     taspkf        w_peso
077800130408     c                   endif
077900130408     c
078000130408     c                   if        §stat=' '
078100130408     c                   exsr      sr_wfsmp
078200130408     c                   endif
078300130408     c
078400130408     c                   exsr      sr_wslss
078500130408     C                   ENDDO
078600130408     c                   ENDSR
078700090204     C**********************************************************
078800090204     C** Elaborazione per file wfsmp00f
078900090204     C**********************************************************
079000090204     C     sr_wfsmp      BEGSR
079100090204     c* linea di partenza
079200090206     c                   z-add     1             k
079300090206     c     taslnp        lookup    fil(k)                                 58
079400090206     c                   if        *in58
079500090206     c     k             occur     dsfil_smp
079600090205     c* . . . giornaliero
079700090206     c                   if        (tasaas*10000+tasmgs)=§al
079800090206     C                   add       1             Wmptsp_g
079900090206     C                   add       TASNCL        wmptcp_g
080000090224     C                   add       w_peso        Wmptkp_g
080100090206     C                   add       TASVLF        Wmptmp_g
080200090206     c                   endif
080300090204     c* . . . riepilogo
080400090206     C                   add       1             Wmptsp_r
080500090206     C                   add       TASNCL        wmptcp_r
080600090224     C                   add       w_peso        Wmptkp_r
080700090206     C                   add       TASVLF        Wmptmp_r
080800090206     c                   endif
080900090204     c* linea di arrivo
081000090206     c                   z-add     1             k
081100090206     c     taslna        lookup    fil(k)                                 58
081200090206     c                   if        *in58
081300090206     c     k             occur     dsfil_smp
081400090205     c* . . . giornaliero
081500090206     c                   if        (tasaas*10000+tasmgs)=§al
081600090206     C                   add       1             Wmptsa_g
081700090206     C                   add       TASNCL        wmptca_g
081800090224     C                   add       w_peso        Wmptka_g
081900090206     C                   add       TASVLF        Wmptma_g
082000090206     c                   endif
082100090205     c* . . . riepilogo
082200090206     C                   add       1             Wmptsa_r
082300090206     C                   add       TASNCL        wmptca_r
082400090224     C                   add       w_peso        Wmptka_r
082500090206     C                   add       TASVLF        Wmptma_r
082600090206     c                   endif
082700090710     c* terminal di partenza : esclusa la linea/terminal
082800090710     c                   if        tastfp<>taslnp
082900090206     c                   z-add     1             k
083000090206     c     tastfp        lookup    fil(k)                                 58
083100090206     c                   if        *in58
083200090206     c     k             occur     dsfil_smp
083300090205     c* . . . giornaliero
083400090206     c                   if        (tasaas*10000+tasmgs)=§al
083500090206     C                   add       1             Wmptspt_g
083600090206     C                   add       TASNCL        wmptcpt_g
083700090224     C                   add       w_peso        Wmptkpt_g
083800090206     C                   add       TASVLF        Wmptmpt_g
083900090206     c                   endif
084000090205     c* . . . riepilogo
084100090206     C                   add       1             Wmptspt_r
084200090206     C                   add       TASNCL        wmptcpt_r
084300090224     C                   add       w_peso        Wmptkpt_r
084400090206     C                   add       TASVLF        Wmptmpt_r
084500090206     c                   endif
084600090710     c                   endif
084700090710     c* terminal di partenza : tutto compresa la linea stessa
084800090710     c                   z-add     1             k
084900090710     c     tastfp        lookup    fil(k)                                 58
085000090710     c                   if        *in58
085100090710     c     k             occur     dsfil_smp
085200090710     c* . . . giornaliero
085300090710     c                   if        (tasaas*10000+tasmgs)=§al
085400090710     C                   add       1             Wmptsptc_g
085500090710     C                   add       TASNCL        wmptcptc_g
085600090710     C                   add       w_peso        Wmptkptc_g
085700090710     C                   add       TASVLF        Wmptmptc_g
085800090710     c                   endif
085900090710     c* . . . riepilogo
086000090710     C                   add       1             Wmptsptc_r
086100090710     C                   add       TASNCL        wmptcptc_r
086200090710     C                   add       w_peso        Wmptkptc_r
086300090710     C                   add       TASVLF        Wmptmptc_r
086400090710     c                   endif
086500090710     c
086600090710     c* terminal di arrivo : esclusa la linea/terminal
086700090710     c                   if        tastfa<>taslna
086800090206     c                   z-add     1             k
086900090206     c     tastfa        lookup    fil(k)                                 58
087000090206     c                   if        *in58
087100090206     c     k             occur     dsfil_smp
087200090205     c* . . . giornaliero
087300090206     c                   if        (tasaas*10000+tasmgs)=§al
087400090206     C                   add       1             Wmptsat_g
087500090206     C                   add       TASNCL        wmptcat_g
087600090224     C                   add       w_peso        Wmptkat_g
087700090206     C                   add       TASVLF        Wmptmat_g
087800090206     c                   endif
087900090205     c* . . . riepilogo
088000090206     C                   add       1             Wmptsat_r
088100090206     C                   add       TASNCL        wmptcat_r
088200090224     C                   add       w_peso        Wmptkat_r
088300090206     C                   add       TASVLF        Wmptmat_r
088400090205     c                   endif
088500090710     c                   endif
088600090710     c* terminal di arrivo : tutto compreso il terminal stesso
088700090710     c                   z-add     1             k
088800090710     c     tastfa        lookup    fil(k)                                 58
088900090710     c                   if        *in58
089000090710     c     k             occur     dsfil_smp
089100090710     c* . . . giornaliero
089200090710     c                   if        (tasaas*10000+tasmgs)=§al
089300090710     C                   add       1             Wmptsatc_g
089400090710     C                   add       TASNCL        wmptcatc_g
089500090710     C                   add       w_peso        Wmptkatc_g
089600090710     C                   add       TASVLF        Wmptmatc_g
089700090710     c                   endif
089800090710     c* . . . riepilogo
089900090710     C                   add       1             Wmptsatc_r
090000090710     C                   add       TASNCL        wmptcatc_r
090100090710     C                   add       w_peso        Wmptkatc_r
090200090710     C                   add       TASVLF        Wmptmatc_r
090300090710     c                   endif
090400090710     c
090500090204     c* filiali di transito
090600090209     c     kar5          chain     fiar531c
090700090205     c                   if        %found(fiar531c)
090800090205     c                   movel     ar5uni        dar5trs
090900090205     c*
091000090205     c                   eval      k_fil=§AR5FLP
091100090205     c                   exsr      agg_transiti
091200090205      *
091300090205     c                   if        §ar5fl2>0
091400090205     c                   eval      k_fil=§AR5FL2
091500090205     c                   exsr      agg_transiti
091600090205     c                   endif
091700090205      *
091800090205     c                   if        §ar5fl3>0
091900090205     c                   eval      k_fil=§AR5FL3
092000090205     c                   exsr      agg_transiti
092100090205     c                   endif
092200090205      *
092300090205     c                   if        §ar5fl4>0
092400090205     c                   eval      k_fil=§AR5FL4
092500090205     c                   exsr      agg_transiti
092600090205     c                   endif
092700090205      *
092800090205     c                   if        §ar5fl5>0
092900090205     c                   eval      k_fil=§AR5FL5
093000090205     c                   exsr      agg_transiti
093100090205     c                   endif
093200090205     c                   endif
093300090204     c
093400090204     C                   endsr
093500090205     C**********************************************************
093600090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
093700090205     C**********************************************************
093800090205     C     agg_transiti  BEGSR
093900090206     c                   z-add     1             k
094000090206     c     k_fil         lookup    fil(k)                                 58
094100090206     c                   if        *in58
094200090206     c     k             occur     dsfil_smp
094300090205     c* . . . giornaliero
094400090206     c                   if        (tasaas*10000+tasmgs)=§al
094500090206     C                   add       1             Wmptst_g
094600090206     C                   add       TASNCL        wmptct_g
094700090224     C                   add       w_peso        Wmptkt_g
094800090206     C                   add       TASVLF        Wmptmt_g
094900090206     c                   endif
095000090205     c* . . . riepilogo
095100090206     C                   add       1             Wmptst_r
095200090206     C                   add       TASNCL        wmptct_r
095300090224     C                   add       w_peso        Wmptkt_r
095400090206     C                   add       TASVLF        Wmptmt_r
095500090205     c                   endif
095600090205     c                   endsr
095700090204     C**********************************************************
095800090206     C** Riempie campi wfsmp00f
095900090204     C**********************************************************
096000090206     C     rie_campi     BEGSR
096100090204     c* profilo utente e data elaborazione
096200090204     c                   eval      wmpusr=knmus
096300090204     c                   eval      wmpdel=wdatael
096400090206     c*
096500090206     c                   eval      wmpall=§al
096600090204     c* area e descrizione area
096700090204     c                   eval      wmpcar=car(k)
096800090204     C                   MOVEL(P)  '05'          tblcod
096900090204     C                   MOVEL(P)  car(K)        tblkey
097000090204     C     KTAB          CHAIN     TABEL00F                           30
097100090204     C                   IF        %found(tabel00f) and tblflg=*blanks
097200090204     C                   MOVEL     TBLUNI        DS05
097300090204     C                   MOVEL     §05DES        wmpdar
097400090204     C                   endif
097500090204     c* filiale,descrizione e livello filiale
097600090204     c                   eval      wmpdil=dil(K)
097700090206     c                   eval      wmpfil=fil(K)
097800090206     c                   eval      wmplif=lpo(K)
097900090204     c* tipo filiale Italia/Estero
098000090205     c                   eval      wmptif=tif(K)
098100090206     c* spedizioni,colli, kg,volume
098200090206     c                   if        wmptip='G'
098300090206     c                   z-add     wmptsp_G      wmptsp
098400090206     c                   z-add     wmptcp_G      wmptcp
098500090206     c                   z-add(h)  wmptkp_G      wmptkp
098600090206     c                   z-add(h)  wmptmp_G      wmptmp
098700090710
098800090206     c                   z-add     wmptsa_G      wmptsa
098900090206     c                   z-add     wmptca_G      wmptca
099000090206     c                   z-add(h)  wmptka_G      wmptka
099100090206     c                   z-add(h)  wmptma_G      wmptma
099200090710
099300090206     c                   z-add     wmptspt_G     wmptspt
099400090206     c                   z-add     wmptcpt_G     wmptcpt
099500090206     c                   z-add(h)  wmptkpt_G     wmptkpt
099600090206     c                   z-add(h)  wmptmpt_G     wmptmpt
099700090710
099800090710     c                   z-add     wmptsptc_G    wmptsptc
099900090710     c                   z-add     wmptcptc_G    wmptcptc
100000090710     c                   z-add(h)  wmptkptc_G    wmptkptc
100100090710     c                   z-add(h)  wmptmptc_G    wmptmptc
100200090710
100300090206     c                   z-add     wmptsat_G     wmptsat
100400090206     c                   z-add     wmptcat_G     wmptcat
100500090206     c                   z-add(h)  wmptkat_G     wmptkat
100600090206     c                   z-add(h)  wmptmat_G     wmptmat
100700090710
100800090710     c                   z-add     wmptsatc_G    wmptsatc
100900090710     c                   z-add     wmptcatc_G    wmptcatc
101000090710     c                   z-add(h)  wmptkatc_G    wmptkatc
101100090710     c                   z-add(h)  wmptmatc_G    wmptmatc
101200090710
101300090206     c                   z-add     wmptst_G      wmptst
101400090206     c                   z-add     wmptct_G      wmptct
101500090206     c                   z-add(h)  wmptkt_G      wmptkt
101600090206     c                   z-add(h)  wmptmt_G      wmptmt
101700090206     c                   else
101800090206     c                   z-add     wmptsp_r      wmptsp
101900090206     c                   z-add     wmptcp_r      wmptcp
102000090206     c                   z-add(h)  wmptkp_r      wmptkp
102100090206     c                   z-add(h)  wmptmp_r      wmptmp
102200090710
102300090206     c                   z-add     wmptsa_r      wmptsa
102400090206     c                   z-add     wmptca_r      wmptca
102500090206     c                   z-add(h)  wmptka_r      wmptka
102600090206     c                   z-add(h)  wmptma_r      wmptma
102700090710
102800090206     c                   z-add     wmptspt_r     wmptspt
102900090206     c                   z-add     wmptcpt_r     wmptcpt
103000090206     c                   z-add(h)  wmptkpt_r     wmptkpt
103100090206     c                   z-add(h)  wmptmpt_r     wmptmpt
103200090710
103300090710     c                   z-add     wmptsptc_r    wmptsptc
103400090710     c                   z-add     wmptcptc_r    wmptcptc
103500090710     c                   z-add(h)  wmptkptc_r    wmptkptc
103600090710     c                   z-add(h)  wmptmptc_r    wmptmptc
103700090710
103800090206     c                   z-add     wmptsat_r     wmptsat
103900090206     c                   z-add     wmptcat_r     wmptcat
104000090206     c                   z-add(h)  wmptkat_r     wmptkat
104100090206     c                   z-add(h)  wmptmat_r     wmptmat
104200090710
104300090710     c                   z-add     wmptsatc_r    wmptsatc
104400090710     c                   z-add     wmptcatc_r    wmptcatc
104500090710     c                   z-add(h)  wmptkatc_r    wmptkatc
104600090710     c                   z-add(h)  wmptmatc_r    wmptmatc
104700090710
104800090206     c                   z-add     wmptst_r      wmptst
104900090206     c                   z-add     wmptct_r      wmptct
105000090206     c                   z-add(h)  wmptkt_r      wmptkt
105100090206     c                   z-add(h)  wmptmt_r      wmptmt
105200090710
105300090206     c                   endif
105400090204     c                   endsr
105500090204     C**********************************************************
105600120905     C** Elaborazione per file WFLSS60f
105700090204     C**********************************************************
105800090204     C     sr_wslss      BEGSR
105900090204     C                   SETOFF                                       545556
106000090204     C                   SETOFF                                       48  52
106100111129     c
106200121123     c
106300111129     c                   z-add     1             kk
106400111129     c     taslnp        lookup    fil(kk)                                98
106500111129     c
106600111129     c                   if        *in98
106700111129     c* Euroexpress o Fedex o DPD
106800111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
106900111129     c                   seton                                        54
107000111129     c                   endif
107100111129     c
107200111129     C**   TASLNP        COMP      300                                54        ESTERO
107300111129     C*+N54TASLNP        COMP      190                                    54    + DPD
107400111129     C**N54TASLNP        COMP      191                                    54    + DPD
107500111129     C**N54TASLNP        COMP      192                                    54    + DPD
107600111129     C**N54TASLNP        COMP      193                                    54    + DPD
107700111129     C**N54TASLNP        COMP      194                                    54    + DPD
107800111129     C**N54TASLNP        COMP      195                                    54    + DPD
107900111129     C**N54TASLNP        COMP      196                                    54    + DPD
108000111129     C**N54TASLNP        COMP      197                                    54    + DPD
108100090204     C*
108200100402     C****     TASLNP        COMP      180                                    48     POSTE
108300090204     C*
108400111129     c                   endif
108500111129     c***
108600121123     c* L I N E A   D I   A R R I V O
108700111129     c***
108800111129     c                   z-add     1             kk
108900111129     c     taslna        lookup    fil(kk)                                98
109000111129     c
109100111129     c                   if        *in98
109200111129     c* Euroexpress o Fedex o DPD
109300111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
109400111129     c                   seton                                        55
109500111129     c                   endif
109600111129     C*
109700111129     C**   TASLNA        COMP      300                                55
109800111129     C**N55TASLNA        COMP      190                                    55    + DPD
109900111129     C**N55TASLNA        COMP      191                                    55    + DPD
110000111129     C**N55TASLNA        COMP      192                                    55    + DPD
110100111129     C*+N55TASLNA        COMP      193                                    55    + DPD
110200111129     C**N55TASLNA        COMP      194                                    55    + DPD
110300111129     C**N55TASLNA        COMP      195                                    55    + DPD
110400111129     C**N55TASLNA        COMP      196                                    55    + DPD
110500111129     C*+N55TASLNA        COMP      197                                    55    + DPD
110600090204     C*
110700111129     c* Fedex
110800111129     c                   if        tif(kk) ='F'
110900111129     c                   seton                                        52
111000111129     c                   endif
111100111129     c
111200111129     C***  TASLNA        COMP      400                                    52     FEDEX
111300090204     C*
111400111129     c* DPD
111500111129     c                   if        tif(kk) ='D'
111600111129     c                   seton                                        56
111700111129     c                   endif
111800111129     c
111900111129     C**   TASLNA        COMP      190                                    56     DPD
112000111129     C**N56TASLNA        COMP      191                                    56     DPD
112100111129     C**N56TASLNA        COMP      192                                    56     DPD
112200111129     C**N56TASLNA        COMP      193                                    56     DPD
112300111129     C**N56TASLNA        COMP      194                                    56     DPD
112400111129     C**N56TASLNA        COMP      195                                    56     DPD
112500111129     C**N56TASLNA        COMP      196                                    56     DPD
112600111129     C**N56TASLNA        COMP      197                                    56     DPD
112700091006     c*
112800111129     c                   endif
112900121123     c
113000111129     c
113100091006     c* Verifico se si tratta di cliente extra budget --> 35 on
113200121121    1c                   if        §stat=' '
113300121121    2c                   if        tasccm<>savccm or tasctr<>savctr
113400121123     c                   z-add     1             EE
113500091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
113600091007     c     w010a         lookup    ksc_EX(EE)                             35
113700121121    3c                   if        not *in35
113800091007     c                   eval      %subst(w010a:8:3)='   '
113900091007     c     w010a         lookup    ksc_EX(EE)                             35
114000121121    3c                   endif
114100091006
114200091006     c                   eval      savccm=tasccm
114300091006     c                   eval      savctr=tasctr
114400121121    2c                   endif
114500110711     c
114600121121   x1c                   else
114700121121     c
114800121121     c* Per i clienti Particolari devo controllare il terminal della bolla se è quello
114900121121     c*   previsto
115000130408   2ac                   if        Wgiro=1
115100121121    2c                   if        tasccm<>savccm
115200121123     c                   z-add     1             EE
115300121121     c                   eval      w010a=%editc(tasccm:'X')+'   '
115400121121     c     w010a         lookup    ksc_EX(EE)                             35
115500121121     c
115600121121     c* E' impossibile che non lo trovi, ma se per caso succede, scarto la bolla
115700121121    3c                   if        *in35
115800121121     c                   eval      savccm=tasccm
115900121121   x3c                   else
116000121121     c                   clear                   savccm
116100121121    3c                   endif
116200121121    2c                   endif
116300130408   2ac                   endif
116400130408     c
116500130408     c* controllo il TASKSC per gli assegnati
116600130408   2ac                   if        Wgiro=2
116700130408     c                   z-add     1             EE
116800130408     c                   eval      w010a=%editc(tasksc:'X')+'   '
116900130408     c     w010a         lookup    ksc_EX(EE)                             35
117000130408     c
117100130408     c* E' impossibile che non lo trovi, ma se per caso succede, scarto la bolla
117200130408    3c                   if        *in35
117300130408     c
117400130408     c                   eval      savccm=tasksc
117500130408     c* Elaboro solo se il TASCCM non è compreso (altrimenti già elaborato nel primo giro)
117600130408     c                   z-add     1             ZZ
117700130408     c                   setoff                                       36
117800130408     c                   eval      w010b=%editc(tasccm:'X')+'   '
117900130408     c     w010b         lookup    ksc_EX(ZZ)                             36
118000130408     c                   if        *in36
118100130408     c                   setoff                                       3536
118200130408     c                   clear                   savccm
118300130408     c                   endif
118400130408     c
118500130408   x3c                   else
118600130408     c                   clear                   savccm
118700130408    3c                   endif
118800130408   2ac                   endif
118900121121     c
119000121121     c                   setoff                                       35
119100121123     c*
119200121123     c                   clear                   prtc_lnp          3 0
119300121123     c                   clear                   prtc_lna          3 0
119400121121     c
119500121121    2c                   if        savccm>0
119600121123     c                   select
119700121123     c*  cliente momentaneamente bloccato
119800121123    3c                   when      ksc_tr(EE)=888
119900121123     c* abilitato a tutto:  memorizzo per qualsiasi terminal
120000121123    3c                   when      ksc_tr(EE)=999
120100121121     c                   seton                                        35
120200121123     c                   eval      prtc_lnp=tastfp
120300121123     c                   eval      prtc_lna=tastfa
120400121123   x3c                   other
120500121123     c
120600121123     c* Abilito solo il terminal di partenza o di arrivo previsto dalla tabella
120700121123    4c                   if        tastfp=ksc_tr(EE)
120800121121     c                   seton                                        35
120900121123     c                   eval      prtc_lnp=tastfp
121000121121    4c                   endif
121100121123    c
121200121123    4c                   if        tastfa=ksc_tr(EE)
121300121123     c                   eval      prtc_lna=tastfa
121400121123     c                   seton                                        35
121500121123    4c                   endif
121600121123     c
121700121123    3c                   endsl
121800121121     c*
121900121121    2c                   endif
122000110711     c
122100121121    1c                   endif
122200121121     c
122300121121     c* Per i clienti Particolari --> 35 off bolla non da elaborare esco dal ciclo
122400121121     c*   visto che l'elaborazione è specifica solo per questi clienti
122500121121     c                   if        §stat='P' and not *in35
122600121121     c                   leavesr
122700121121     c                   endif
122800090204     C*
122900121123    1c                   if        §stat=' '
123000121123     c                   eval      prtc_lnp=taslnp
123100090204     C                   Z-ADD     1             K
123200090204     C     TASLNP        LOOKUP    FIL(K)                                 58
123300121123     c                   else
123400121123     c
123500121123     c* solo si tratta di terminal di partenza elaborabile procedo
123600121123     c                   if        prtc_lnp>0
123700121123     C                   Z-ADD     1             K
123800121123     C     prtc_lnp      LOOKUP    FIL(K)                                 58
123900121123     c                   endif
124000121123     c                   endif
124100121123     c
124200121123    1c                   if        prtc_lnp>0
124300121123     c
124400090204     C  N58              EXCEPT    ERR
124500090204     C     K             OCCUR     DSFIL
124600100402     C     K             OCCUR     DSFIL_e
124700100402     C     K             OCCUR     DSFIL_h
124800121123     c
124900090204     C  N54
125000090204     CANN55              DO                                                     ITALIA
125100100402     c* Spedizioni LNP Italia LNA Italia
125200100402     c                   exsr      speITA
125300090204     C                   ENDDO
125400100402     c
125500100402     c* Spedizioni IMPORT --> LNP Italia
125600090204     C   54              DO                                                     IMPORT
125700100402     c                   exsr      speIMPORT
125800090204     C                   ENDDO
125900100402     c
126000100402     c* Spedizioni lna EUROEXPRESS
126100090204     C   55
126200090204     CANN56
126300090204     CANN52              DO                                                     EXPORT
126400100402     c                   exsr      speEXPRESS
126500090204     C                   ENDDO
126600100402     c
126700100402     c* Spedizioni lna DPD
126800090204     C   55
126900090204     CAN 56              DO                                                     EXPORT
127000100402     c                   exsr      speDPD
127100090204     C                   ENDDO
127200100402     c
127300100402     c* Spedizioni lna FEDEX
127400090204     C   55
127500090204     CAN 52              DO                                                     EXPORT
127600100402     c                   exsr      speFEDEX
127700090204     C                   ENDDO
127800121123     c
127900121123    1c                   endif
128000100402     c
128100090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
128200121123    1c                   if        §stat=' '
128300121123     c                   eval      prtc_lna=taslna
128400090204     C                   Z-ADD     1             K
128500090204     C     TASLNA        LOOKUP    FIL(K)                                 58
128600121123     c                   else
128700121123     c
128800121123     c* solo si tratta di terminal di arrivo elaborabile procedo
128900121123     c                   if        prtc_lna>0
129000121123     C                   Z-ADD     1             K
129100121123     C     prtc_lna      LOOKUP    FIL(K)                                 58
129200121123     c                   endif
129300121123     c                   endif
129400121123     c
129500121123    1c                   if        prtc_lna>0
129600121123     c
129700090204     C  N58              EXCEPT    ERR
129800090204     C     K             OCCUR     DSFIL
129900100402     C     K             OCCUR     DSFIL_e
130000100402     C     K             OCCUR     DSFIL_h
130100100402     c
130200090204     C  N54
130300100402     CANN55              DO
130400100402     c* Spedizioni LNP Italia LNA Italia
130500100402     c                   exsr      speITA_LNA
130600090204     C                   ENDDO
130700100402     c
130800100402     c* spedizioni EXPORT --> lna estera
130900090204     C   55              DO                                                     ESTERO ASS.+FRA.
131000100402     c                   exsr      speEXPORT
131100090204     C                   ENDDO
131200121123     c
131300121123     c                   endif
131400121123     c
131500090204     c                   endsr
131600090206     C**********************************************************
131700090206     C** scarica ds multiple su file
131800090206     C**********************************************************
131900090206     C     scrive_file   BEGSR
132000090206     c*
132100090206     C                   Z-ADD     §AL           WSSALL
132200090206     c*
132300110711    1C                   DO        400           K
132400090206     C     K             OCCUR     DSFIL_smp
132500090206     C     K             OCCUR     DSFIL
132600100402     C     K             OCCUR     DSFIL_e
132700100402     C     K             OCCUR     DSFIL_h
132800110711     c
132900110711    2c                   if        §stat=' '
133000090206     c* WFSMP
133100090206     c* giornaliero
133200090601     c* Lo scrivo solo per tipo "G" o "R"
133300090601     c                   if        Wtipo='G' or Wtipo='R'
133400090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
133500090206     c                              wmptspt_g>0 or wmptsat_g>0 or
133600090206     c                              wmptst_g>0)
133700090206     c                   z-add     §al           wmpdal
133800090206     c                   eval      wmptip='G'
133900090206     c                   exsr      rie_campi
134000090206     c                   write     wfsmp000
134100090206     c                   endif
134200090601     c                   endif
134300090601     c
134400090601     c* riepilogo /precedente
134500090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
134600090601     c                   if        Wtipo='P' or Wtipo='R'
134700090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
134800090206     c                              wmptspt_r>0 or wmptsat_r>0 or
134900090206     c                              wmptst_r>0)
135000090206     c                   z-add     §dal          wmpdal
135100090601     c                   if        Wtipo='R'
135200090206     c                   eval      wmptip='R'
135300090601     c                   else
135400090601     c                   eval      wmptip='P'
135500090601     c                   endif
135600090206     c                   exsr      rie_campi
135700090206     c                   write     wfsmp000
135800090206     c                   endif
135900090601     c                   endif
136000110711     c
136100110711    2c                   endif
136200120905     c* WFLSS60F
136300120904     c                   clear                   wsskgf
136400090206     c* giornaliero
136500090601     c                   if        Wtipo='G' or Wtipo='R'
136600110711     c
136700110711    2c                   if        §stat=' '
136800090601     c
136900090206     C     WTSPIP        IFGT      *ZERO
137000090206     C                   Z-ADD     §al           WSSDAL
137100090206     c                   movel     'G'           WSSTIE
137200090206     C                   MOVE      'IP'          WSSTIP
137300090206     C                   Z-ADD     FIL(K)        WSSLIN
137400090206     C                   Z-ADD     WTSPIP        WSSTSP
137500090206     C                   Z-ADD     WTCLIP        WSSTCL
137600090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
137700090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
137800120904     c*old
137900120904     C                   Z-ADD(H)  WTKGIP        WSSTKG_ol
138000120904     C                   Z-ADD(H)  WTMCIP        WSSTMC_ol
138100100402     c* spedizioni "E"
138200100402     C                   Z-ADD     eWTSPIP       WSSTSPe
138300100402     C                   Z-ADD     eWTCLIP       WSSTCLe
138400100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
138500100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
138600120904     c* old
138700120904     C                   Z-ADD(H)  eWTKGIP       WSSTKGe_ol
138800120904     C                   Z-ADD(H)  eWTMCIP       WSSTMCe_ol
138900100402     c* spedizioni "H"
139000100402     C                   Z-ADD     hWTSPIP       WSSTSPh
139100100402     C                   Z-ADD     hWTCLIP       WSSTCLh
139200100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
139300100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
139400120904     c* old
139500120904     C                   Z-ADD(H)  hWTKGIP       WSSTKGh_ol
139600120904     C                   Z-ADD(H)  hWTMCIP       WSSTMCh_ol
139700100402
139800120905     C                   WRITE     WFLSS600
139900120904     C                   WRITE     WSLSS500
140000090206     C                   ENDIF
140100100402
140200090206     C     WTSPIA        IFGT      *ZERO
140300090206     C                   Z-ADD     §al           WSSDAL
140400090206     c                   movel     'G'           WSSTIE
140500090206     C                   MOVE      'IA'          WSSTIP
140600090206     C                   Z-ADD     FIL(K)        WSSLIN
140700090206     C                   Z-ADD     WTSPIA        WSSTSP
140800090206     C                   Z-ADD     WTCLIA        WSSTCL
140900090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
141000090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
141100120904     c* old
141200120904     C                   Z-ADD(H)  WTKGIA        WSSTKG_ol
141300120904     C                   Z-ADD(H)  WTMCIA        WSSTMC_ol
141400100402     c* spedizioni "E"
141500100402     C                   Z-ADD     eWTSPIA       WSSTSPe
141600100402     C                   Z-ADD     eWTCLIA       WSSTCLe
141700100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
141800100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
141900120904     c* old
142000120904     C                   Z-ADD(H)  eWTKGIA       WSSTKGe_ol
142100120904     C                   Z-ADD(H)  eWTMCIA       WSSTMCe_ol
142200100402     c* spedizioni "H"
142300100402     C                   Z-ADD     hWTSPIA       WSSTSPh
142400100402     C                   Z-ADD     hWTCLIA       WSSTCLh
142500100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
142600100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
142700120904     c* old
142800120904     C                   Z-ADD(H)  hWTKGIA       WSSTKGh_ol
142900120904     C                   Z-ADD(H)  hWTMCIA       WSSTMCh_ol
143000100402     c
143100120905     C                   WRITE     WFLSS600
143200120904     C                   WRITE     WSLSS500
143300090206     C                   ENDIF
143400100402     c
143500090206     C     WTSPEI        IFGT      *ZERO
143600090206     C                   Z-ADD     §al           WSSDAL
143700090206     c                   movel     'G'           WSSTIE
143800090206     C                   MOVE      'EI'          WSSTIP
143900090206     C                   Z-ADD     FIL(K)        WSSLIN
144000090206     C                   Z-ADD     WTSPEI        WSSTSP
144100090206     C                   Z-ADD     WTCLEI        WSSTCL
144200090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
144300090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
144400120904     c* old
144500120904     C                   Z-ADD(H)  WTKGEI        WSSTKG_ol
144600120904     C                   Z-ADD(H)  WTMCEI        WSSTMC_ol
144700100402     c* spedizioni "E"
144800100402     C                   Z-ADD     eWTSPEI       WSSTSPe
144900100402     C                   Z-ADD     eWTCLEI       WSSTCLe
145000100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
145100100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
145200120904     c* old
145300120904     C                   Z-ADD(H)  eWTKGEI       WSSTKGe_ol
145400120904     C                   Z-ADD(H)  eWTMCEI       WSSTMCe_ol
145500100402     c* spedizioni "H"
145600100402     C                   Z-ADD     hWTSPEI       WSSTSPh
145700100402     C                   Z-ADD     hWTCLEI       WSSTCLh
145800100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
145900100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
146000120904     c* old
146100120904     C                   Z-ADD(H)  hWTKGEI       WSSTKGh_ol
146200120904     C                   Z-ADD(H)  hWTMCEI       WSSTMCh_ol
146300100402     c
146400120905     C                   WRITE     WFLSS600
146500120904     C                   WRITE     WSLSS500
146600120904     C                   ENDIF
146700100402     c
146800090206     C     WTSPEE        IFGT      *ZERO
146900090206     C                   Z-ADD     §al           WSSDAL
147000090206     c                   movel     'G'           WSSTIE
147100090206     C                   MOVE      'EE'          WSSTIP
147200090206     C                   Z-ADD     FIL(K)        WSSLIN
147300090206     C                   Z-ADD     WTSPEE        WSSTSP
147400090206     C                   Z-ADD     WTCLEE        WSSTCL
147500090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
147600090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
147700120904     c* old
147800120904     C                   Z-ADD(H)  WTKGEE        WSSTKG_ol
147900120904     C                   Z-ADD(H)  WTMCEE        WSSTMC_ol
148000100402     c* spedizioni "E"
148100100402     C                   Z-ADD     eWTSPEE       WSSTSPe
148200100402     C                   Z-ADD     eWTCLEE       WSSTCLe
148300100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
148400100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
148500120904     c* old
148600120904     C                   Z-ADD(H)  eWTKGEE       WSSTKGe_ol
148700120904     C                   Z-ADD(H)  eWTMCEE       WSSTMCe_ol
148800100402     c* spedizioni "H"
148900100402     C                   Z-ADD     hWTSPEE       WSSTSPh
149000100402     C                   Z-ADD     hWTCLEE       WSSTCLh
149100100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
149200100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
149300120904     c* old
149400120904     C                   Z-ADD(H)  hWTKGEE       WSSTKGh_ol
149500120904     C                   Z-ADD(H)  hWTMCEE       WSSTMCh_ol
149600100402     c
149700120905     C                   WRITE     WFLSS600
149800120904     C                   WRITE     WSLSS500
149900090206     C                   ENDIF
150000090206     C*
150100090206     C     WTSPED        IFGT      *ZERO
150200090206     C                   Z-ADD     §al           WSSDAL
150300090206     c                   movel     'G'           WSSTIE
150400090206     C                   MOVE      'ED'          WSSTIP
150500090206     C                   Z-ADD     FIL(K)        WSSLIN
150600090206     C                   Z-ADD     WTSPED        WSSTSP
150700090206     C                   Z-ADD     WTCLED        WSSTCL
150800090206     C                   Z-ADD(H)  WTKGED        WSSTKG
150900090206     C                   Z-ADD(H)  WTMCED        WSSTMC
151000120904     c* old
151100120904     C                   Z-ADD(H)  WTKGED        WSSTKG_ol
151200120904     C                   Z-ADD(H)  WTMCED        WSSTMC_ol
151300100402     c* spedizioni "E"
151400100402     C                   Z-ADD     eWTSPED       WSSTSPe
151500100402     C                   Z-ADD     eWTCLED       WSSTCLe
151600100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
151700100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
151800120904     c* old
151900120904     C                   Z-ADD(H)  eWTKGED       WSSTKGe_ol
152000120904     C                   Z-ADD(H)  eWTMCED       WSSTMCe_ol
152100100402     c* spedizioni "H"
152200100402     C                   Z-ADD     hWTSPED       WSSTSPh
152300100402     C                   Z-ADD     hWTCLED       WSSTCLh
152400100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
152500100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
152600120904     c* old
152700120904     C                   Z-ADD(H)  hWTKGED       WSSTKGh_ol
152800120904     C                   Z-ADD(H)  hWTMCED       WSSTMCh_ol
152900100402     c
153000120905     C                   WRITE     WFLSS600
153100120904     C                   WRITE     WSLSS500
153200090206     C                   ENDIF
153300090206     C*
153400090206     C     WTSPEX        IFGT      *ZERO
153500090206     C                   Z-ADD     §al           WSSDAL
153600090206     c                   movel     'G'           WSSTIE
153700090206     C                   MOVE      'EX'          WSSTIP
153800090206     C                   Z-ADD     FIL(K)        WSSLIN
153900090206     C                   Z-ADD     WTSPEX        WSSTSP
154000090206     C                   Z-ADD     WTCLEX        WSSTCL
154100090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
154200090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
154300120904     c* old
154400120904     C                   Z-ADD(H)  WTKGEX        WSSTKG_ol
154500120904     C                   Z-ADD(H)  WTMCEX        WSSTMC_ol
154600100402     c* spedizioni "E"
154700100402     C                   Z-ADD     eWTSPEX       WSSTSPe
154800100402     C                   Z-ADD     eWTCLEX       WSSTCLe
154900100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
155000100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
155100120904     c* old
155200120904     C                   Z-ADD(H)  eWTKGEX       WSSTKGe_ol
155300120904     C                   Z-ADD(H)  eWTMCEX       WSSTMCe_ol
155400100402     c* spedizioni "H"
155500100402     C                   Z-ADD     hWTSPEX       WSSTSPh
155600100402     C                   Z-ADD     hWTCLEX       WSSTCLh
155700100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
155800100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
155900120904     c* old
156000120904     C                   Z-ADD(H)  hWTKGEX       WSSTKGh_ol
156100120904     C                   Z-ADD(H)  hWTMCEX       WSSTMCh_ol
156200100402     c
156300120905     C                   WRITE     WFLSS600
156400120904     C                   WRITE     WSLSS500
156500090206     C                   ENDIF
156600090206     C*
156700090206     C     WTSPEA        IFGT      *ZERO
156800090206     C                   Z-ADD     §al           WSSDAL
156900090206     c                   movel     'G'           WSSTIE
157000090206     C                   MOVE      'EA'          WSSTIP
157100090206     C                   Z-ADD     FIL(K)        WSSLIN
157200090206     C                   Z-ADD     WTSPEA        WSSTSP
157300090206     C                   Z-ADD     WTCLEA        WSSTCL
157400090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
157500090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
157600120904     c* old
157700120904     C                   Z-ADD(H)  WTKGEA        WSSTKG_ol
157800120904     C                   Z-ADD(H)  WTMCEA        WSSTMC_ol
157900100402     c* spedizioni "E"
158000100402     C                   Z-ADD     eWTSPEA       WSSTSPe
158100100402     C                   Z-ADD     eWTCLEA       WSSTCLe
158200100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
158300100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
158400120904     c* old
158500120904     C                   Z-ADD(H)  eWTKGEA       WSSTKGe_ol
158600120904     C                   Z-ADD(H)  eWTMCEA       WSSTMCe_ol
158700100402     c* spedizioni "H"
158800100402     C                   Z-ADD     hWTSPEA       WSSTSPh
158900100402     C                   Z-ADD     hWTCLEA       WSSTCLh
159000100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
159100100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
159200120904     c* old
159300120904     C                   Z-ADD(H)  hWTKGEA       WSSTKGh_ol
159400120904     C                   Z-ADD(H)  hWTMCEA       WSSTMCh_ol
159500100402     c
159600120905     C                   WRITE     WFLSS600
159700120904     C                   WRITE     WSLSS500
159800090206     C                   ENDIF
159900090206     C*
160000090206     C     WTSPPA        IFGT      *ZERO
160100090206     C                   Z-ADD     §al           WSSDAL
160200090206     c                   movel     'G'           WSSTIE
160300090206     C                   MOVE      'PA'          WSSTIP
160400090206     C                   Z-ADD     FIL(K)        WSSLIN
160500090206     C                   Z-ADD     WTSPPA        WSSTSP
160600090206     C                   Z-ADD     WTCLPA        WSSTCL
160700090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
160800090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
160900120904     c* old
161000120904     C                   Z-ADD(H)  WTKGPA        WSSTKG_ol
161100120904     C                   Z-ADD(H)  WTMCPA        WSSTMC_ol
161200100402     c* spedizioni "E"
161300100402     C                   Z-ADD     eWTSPPA       WSSTSPe
161400100402     C                   Z-ADD     eWTCLPA       WSSTCLe
161500100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
161600100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
161700120904     c* old
161800120904     C                   Z-ADD(H)  eWTKGPA       WSSTKGe_ol
161900120904     C                   Z-ADD(H)  eWTMCPA       WSSTMCe_ol
162000100402     c* spedizioni "H"
162100100402     C                   Z-ADD     hWTSPPA       WSSTSPh
162200100402     C                   Z-ADD     hWTCLPA       WSSTCLh
162300100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
162400100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
162500120904     c* old
162600120904     C                   Z-ADD(H)  hWTKGPA       WSSTKGh_ol
162700120904     C                   Z-ADD(H)  hWTMCPA       WSSTMCh_ol
162800100402     c
162900120905     C                   WRITE     WFLSS600
163000120904     C                   WRITE     WSLSS500
163100090206     C                   ENDIF
163200110711     c
163300110711     c                   endif
163400110715     c* Extra budget o clienti particolari
163500110715     c                   if        §stat=' '
163600110715     c                   movel     'G'           WSSTIE
163700110715     c                   else
163800110715     c                   movel     'A'           WSSTIE
163900110715     c                   endif
164000110715     c
164100091006     C     WTSPIP_e      IFGT      *ZERO
164200091007     C                   Z-ADD     §al           WSSDAL
164300091006     C                   MOVE      'IP'          WSSTIP
164400091006     C                   Z-ADD     FIL(K)        WSSLIN
164500091006     C                   Z-ADD     WTSPIP_e      WSSTSP
164600091006     C                   Z-ADD     WTCLIP_e      WSSTCL
164700120904     C                   Z-ADD(H)  WTKGIP_e      WSSTKG_ex
164800120904     C                   Z-ADD(H)  WTMCIP_e      WSSTMC_ex
164900100402     c* spedizioni "E"
165000100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
165100100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
165200120904     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe_ex
165300120904     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe_ex
165400100402     c* spedizioni "H"
165500100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
165600100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
165700120904     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh_ex
165800120904     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh_ex
165900100402     c
166000120903     C                   WRITE     wslss550
166100091006     C                   ENDIF
166200100402     c
166300091006     C     WTSPIA_e      IFGT      *ZERO
166400091007     C                   Z-ADD     §al           WSSDAL
166500110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
166600110715     c*****              movel     'G'           WSSTIE
166700091006     C                   MOVE      'IA'          WSSTIP
166800091006     C                   Z-ADD     FIL(K)        WSSLIN
166900091006     C                   Z-ADD     WTSPIA_e      WSSTSP
167000091006     C                   Z-ADD     WTCLIA_e      WSSTCL
167100120904     C                   Z-ADD(H)  WTKGIA_e      WSSTKG_ex
167200120904     C                   Z-ADD(H)  WTMCIA_e      WSSTMC_ex
167300100402     c* spedizioni "E"
167400100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
167500100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
167600120904     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe_ex
167700120904     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe_ex
167800100402     c* spedizioni "H"
167900100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
168000100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
168100120904     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh_ex
168200120904     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh_ex
168300100402     c
168400091006     C                   WRITE     WSLSS550
168500091006     C                   ENDIF
168600100402     c
168700091006     C     WTSPEI_e      IFGT      *ZERO
168800091007     C                   Z-ADD     §al           WSSDAL
168900110715     c******             movel     'G'           WSSTIE
169000091006     C                   MOVE      'EI'          WSSTIP
169100091006     C                   Z-ADD     FIL(K)        WSSLIN
169200091006     C                   Z-ADD     WTSPEI_e      WSSTSP
169300091006     C                   Z-ADD     WTCLEI_e      WSSTCL
169400120904     C                   Z-ADD(H)  WTKGEI_e      WSSTKG_ex
169500120904     C                   Z-ADD(H)  WTMCEI_e      WSSTMC_ex
169600100402     c* spedizioni "E"
169700100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
169800100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
169900120904     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe_ex
170000120904     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe_ex
170100100402     c* spedizioni "H"
170200100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
170300100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
170400120904     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh_ex
170500120904     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh_ex
170600100402     c
170700091006     C                   WRITE     WSLSS550
170800091006     C                   ENDIF
170900100402     c
171000091006     C     WTSPEE_e      IFGT      *ZERO
171100091007     C                   Z-ADD     §al           WSSDAL
171200110715     c******             movel     'G'           WSSTIE
171300091006     C                   MOVE      'EE'          WSSTIP
171400091006     C                   Z-ADD     FIL(K)        WSSLIN
171500091006     C                   Z-ADD     WTSPEE_e      WSSTSP
171600091006     C                   Z-ADD     WTCLEE_e      WSSTCL
171700120904     C                   Z-ADD(H)  WTKGEE_e      WSSTKG_ex
171800120904     C                   Z-ADD(H)  WTMCEE_e      WSSTMC_ex
171900100402     c* spedizioni "E"
172000100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
172100100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
172200120904     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe_ex
172300120904     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe_ex
172400100402     c* spedizioni "H"
172500100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
172600100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
172700120904     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh_ex
172800120904     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh_ex
172900100402     c
173000091006     C                   WRITE     WSLSS550
173100091006     C                   ENDIF
173200091006     C*
173300091006     C     WTSPED_e      IFGT      *ZERO
173400091007     C                   Z-ADD     §al           WSSDAL
173500110715     c*******            movel     'G'           WSSTIE
173600091006     C                   MOVE      'ED'          WSSTIP
173700091006     C                   Z-ADD     FIL(K)        WSSLIN
173800091006     C                   Z-ADD     WTSPED_e      WSSTSP
173900091006     C                   Z-ADD     WTCLED_e      WSSTCL
174000120904     C                   Z-ADD(H)  WTKGED_e      WSSTKG_ex
174100120904     C                   Z-ADD(H)  WTMCED_e      WSSTMC_ex
174200100402     c* spedizioni "E"
174300100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
174400100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
174500120904     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe_ex
174600120904     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe_ex
174700100402     c* spedizioni "H"
174800100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
174900100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
175000120904     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh_ex
175100120904     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh_ex
175200100402     c
175300091006     C                   WRITE     WSLSS550
175400091006     C                   ENDIF
175500091006     C*
175600091006     C     WTSPEX_e      IFGT      *ZERO
175700091007     C                   Z-ADD     §al           WSSDAL
175800110715     c*******            movel     'G'           WSSTIE
175900091006     C                   MOVE      'EX'          WSSTIP
176000091006     C                   Z-ADD     FIL(K)        WSSLIN
176100091006     C                   Z-ADD     WTSPEX_e      WSSTSP
176200091006     C                   Z-ADD     WTCLEX_e      WSSTCL
176300120904     C                   Z-ADD(H)  WTKGEX_e      WSSTKG_ex
176400120904     C                   Z-ADD(H)  WTMCEX_e      WSSTMC_ex
176500100402     c* spedizioni "E"
176600100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
176700100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
176800120904     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe_ex
176900120904     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe_ex
177000100402     c* spedizioni "H"
177100100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
177200100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
177300120904     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh_ex
177400120904     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh_ex
177500100402     c
177600091006     C                   WRITE     WSLSS550
177700091006     C                   ENDIF
177800091006     C*
177900091006     C     WTSPEA_e      IFGT      *ZERO
178000091007     C                   Z-ADD     §al           WSSDAL
178100110715     c******             movel     'G'           WSSTIE
178200091006     C                   MOVE      'EA'          WSSTIP
178300091006     C                   Z-ADD     FIL(K)        WSSLIN
178400091006     C                   Z-ADD     WTSPEA_e      WSSTSP
178500091006     C                   Z-ADD     WTCLEA_e      WSSTCL
178600120904     C                   Z-ADD(H)  WTKGEA_e      WSSTKG_ex
178700120904     C                   Z-ADD(H)  WTMCEA_e      WSSTMC_ex
178800100402     c* spedizioni "E"
178900100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
179000100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
179100120904     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe_ex
179200120904     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe_ex
179300100402     c* spedizioni "H"
179400100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
179500100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
179600120904     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh_ex
179700120904     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh_ex
179800100402     c
179900091006     C                   WRITE     WSLSS550
180000091006     C                   ENDIF
180100091006     C*
180200091006     C     WTSPPA_e      IFGT      *ZERO
180300091007     C                   Z-ADD     §al           WSSDAL
180400110715     c*******            movel     'G'           WSSTIE
180500091006     C                   MOVE      'PA'          WSSTIP
180600091006     C                   Z-ADD     FIL(K)        WSSLIN
180700091006     C                   Z-ADD     WTSPPA_e      WSSTSP
180800091006     C                   Z-ADD     WTCLPA_e      WSSTCL
180900120904     C                   Z-ADD(H)  WTKGPA_e      WSSTKG_ex
181000120904     C                   Z-ADD(H)  WTMCPA_e      WSSTMC_ex
181100100402     c* spedizioni "E"
181200100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
181300100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
181400120904     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe_ex
181500120904     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe_ex
181600100402     c* spedizioni "H"
181700100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
181800100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
181900120904     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh_ex
182000120904     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh_ex
182100100402     c
182200091006     C                   WRITE     WSLSS550
182300091006     C                   ENDIF
182400091006
182500090601     C                   ENDIF
182600090206     c*
182700090206     c* riepilogo
182800090601     c                   if        Wtipo='P' or Wtipo='R'
182900110715     c                   if        §stat=' '
183000110715
183100090206     C     WTSPIP_r      IFGT      *ZERO
183200090206     C                   Z-ADD     §dal          WSSDAL
183300090601     c**                 movel     'R'           WSSTIE
183400090601     c                   movel     Wtipo         WSSTIE
183500090206     C                   MOVE      'IP'          WSSTIP
183600090206     C                   Z-ADD     FIL(K)        WSSLIN
183700090206     C                   Z-ADD     WTSPIP_r      WSSTSP
183800090206     C                   Z-ADD     WTCLIP_r      WSSTCL
183900090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
184000090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
184100120904     c* old
184200120904     C                   Z-ADD(H)  WTKGIP_r      WSSTKG_ol
184300120904     C                   Z-ADD(H)  WTMCIP_r      WSSTMC_ol
184400100402     c* spedizioni "E"
184500100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
184600100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
184700100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
184800100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
184900120904     c* old
185000120904     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe_ol
185100120904     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe_ol
185200100402     c* spedizioni "H"
185300100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
185400100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
185500100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
185600100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
185700120904     c* old
185800120904     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh_ol
185900120904     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh_ol
186000100402     c
186100120905     C                   WRITE     WFLSS600
186200120904     C                   WRITE     WSLSS500
186300090206     C                   ENDIF
186400100402     c
186500090206     C     WTSPIA_r      IFGT      *ZERO
186600090206     C                   Z-ADD     §dal          WSSDAL
186700090601     c**                 movel     'R'           WSSTIE
186800090601     c                   movel     Wtipo         WSSTIE
186900090206     C                   MOVE      'IA'          WSSTIP
187000090206     C                   Z-ADD     FIL(K)        WSSLIN
187100090206     C                   Z-ADD     WTSPIA_r      WSSTSP
187200090206     C                   Z-ADD     WTCLIA_r      WSSTCL
187300090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
187400090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
187500120904     c* old
187600120904     C                   Z-ADD(H)  WTKGIA_r      WSSTKG_ol
187700120904     C                   Z-ADD(H)  WTMCIA_r      WSSTMC_ol
187800100402     c* spedizioni "E"
187900100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
188000100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
188100100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
188200100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
188300120904     c* old
188400120904     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe_ol
188500120904     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe_ol
188600100402     c* spedizioni "H"
188700100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
188800100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
188900100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
189000100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
189100120904     c* old
189200120904     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh_ol
189300120904     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh_ol
189400100402     c
189500120905     C                   WRITE     WFLSS600
189600120904     C                   WRITE     WSLSS500
189700090206     C                   ENDIF
189800100402
189900090206     C     WTSPEI_R      IFGT      *ZERO
190000090206     C                   Z-ADD     §dal          WSSDAL
190100090601     c**                 movel     'R'           WSSTIE
190200090601     c                   movel     Wtipo         WSSTIE
190300090206     C                   MOVE      'EI'          WSSTIP
190400090206     C                   Z-ADD     FIL(K)        WSSLIN
190500090206     C                   Z-ADD     WTSPEI_r      WSSTSP
190600090206     C                   Z-ADD     WTCLEI_r      WSSTCL
190700090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
190800090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
190900120904     c* old
191000120904     C                   Z-ADD(H)  WTKGEI_r      WSSTKG_ol
191100120904     C                   Z-ADD(H)  WTMCEI_r      WSSTMC_ol
191200100402     c* Spedizioni "E"
191300100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
191400100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
191500100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
191600100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
191700120904     c* old
191800120904     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe_ol
191900120904     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe_ol
192000100402     c* Spedizioni "H"
192100100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
192200100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
192300100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
192400100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
192500120904     c* old
192600120904     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh_ol
192700120904     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh_ol
192800100402     c
192900120905     C                   WRITE     WFLSS600
193000120904     C                   WRITE     WSLSS500
193100090206     C                   ENDIF
193200100402     c
193300090206     C     WTSPEE_r      IFGT      *ZERO
193400090206     C                   Z-ADD     §dal          WSSDAL
193500090601     c**                 movel     'R'           WSSTIE
193600090601     c                   movel     Wtipo         WSSTIE
193700090206     C                   MOVE      'EE'          WSSTIP
193800090206     C                   Z-ADD     FIL(K)        WSSLIN
193900090206     C                   Z-ADD     WTSPEE_r      WSSTSP
194000090206     C                   Z-ADD     WTCLEE_r      WSSTCL
194100090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
194200090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
194300120904     c* old
194400120904     C                   Z-ADD(H)  WTKGEE_r      WSSTKG_ol
194500120904     C                   Z-ADD(H)  WTMCEE_r      WSSTMC_ol
194600100402     c* Spedizioni "E"
194700100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
194800100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
194900100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
195000100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
195100120904     c* old
195200120904     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe_ol
195300120904     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe_ol
195400100402     c* Spedizioni "H"
195500100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
195600100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
195700100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
195800100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
195900120904     c* old
196000120904     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh_ol
196100120904     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh_ol
196200100402     c
196300120905     C                   WRITE     WFLSS600
196400120904     C                   WRITE     WSLSS500
196500090206     C                   ENDIF
196600090206     C*
196700090206     C     WTSPED_r      IFGT      *ZERO
196800090206     C                   Z-ADD     §dal          WSSDAL
196900090601     c**                 movel     'R'           WSSTIE
197000090601     c                   movel     Wtipo         WSSTIE
197100090206     C                   MOVE      'ED'          WSSTIP
197200090206     C                   Z-ADD     FIL(K)        WSSLIN
197300090206     C                   Z-ADD     WTSPED_r      WSSTSP
197400090206     C                   Z-ADD     WTCLED_r      WSSTCL
197500090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
197600090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
197700120904     c* old
197800120904     C                   Z-ADD(H)  WTKGED_r      WSSTKG_ol
197900120904     C                   Z-ADD(H)  WTMCED_r      WSSTMC_ol
198000100402     c* Spedizioni "E"
198100100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
198200100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
198300100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
198400100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
198500120904     c* old
198600120904     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe_ol
198700120904     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe_ol
198800100402     c* Spedizioni "H"
198900100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
199000100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
199100100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
199200100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
199300120904     c* old
199400120904     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh_ol
199500120904     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh_ol
199600100402     c
199700120905     C                   WRITE     WFLSS600
199800120904     C                   WRITE     WSLSS500
199900090206     C                   ENDIF
200000090206     C*
200100090206     C     WTSPEX_r      IFGT      *ZERO
200200090206     C                   Z-ADD     §dal          WSSDAL
200300090601     c**                 movel     'R'           WSSTIE
200400090601     c                   movel     Wtipo         WSSTIE
200500090206     C                   MOVE      'EX'          WSSTIP
200600090206     C                   Z-ADD     FIL(K)        WSSLIN
200700090206     C                   Z-ADD     WTSPEX_r      WSSTSP
200800090206     C                   Z-ADD     WTCLEX_r      WSSTCL
200900090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
201000090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
201100120904     c* old
201200120904     C                   Z-ADD(H)  WTKGEX_r      WSSTKG_ol
201300120904     C                   Z-ADD(H)  WTMCEX_r      WSSTMC_ol
201400100402     c* Spedizioni "E"
201500100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
201600100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
201700100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
201800100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
201900120904     c* old
202000120904     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe_ol
202100120904     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe_ol
202200100402     c* Spedizioni "H"
202300100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
202400100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
202500100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
202600100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
202700120904     c* old
202800120904     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh_ol
202900120904     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh_ol
203000100402     c
203100120905     C                   WRITE     WFLSS600
203200120904     C                   WRITE     WSLSS500
203300090206     C                   ENDIF
203400090206     C*
203500090206     C     WTSPEA_r      IFGT      *ZERO
203600090206     C                   Z-ADD     §dal          WSSDAL
203700090601     c**                 movel     'R'           WSSTIE
203800090601     c                   movel     Wtipo         WSSTIE
203900090206     C                   MOVE      'EA'          WSSTIP
204000090206     C                   Z-ADD     FIL(K)        WSSLIN
204100090206     C                   Z-ADD     WTSPEA_r      WSSTSP
204200090206     C                   Z-ADD     WTCLEA_r      WSSTCL
204300090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
204400090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
204500120904     c* old
204600120904     C                   Z-ADD(H)  WTKGEA_r      WSSTKG_ol
204700120904     C                   Z-ADD(H)  WTMCEA_r      WSSTMC_ol
204800100402     c* Spedizioni "E"
204900100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
205000100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
205100100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
205200100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
205300120904     c* old
205400120904     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe_ol
205500120904     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe_ol
205600100402     c* Spedizioni "H"
205700100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
205800100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
205900100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
206000100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
206100120904     c* old
206200120904     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh_ol
206300120904     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh_ol
206400100402     c
206500120905     C                   WRITE     WFLSS600
206600120904     C                   WRITE     WSLSS500
206700090206     C                   ENDIF
206800090206     C*
206900090206     C     WTSPPA_r      IFGT      *ZERO
207000090206     C                   Z-ADD     §dal          WSSDAL
207100090601     c**                 movel     'R'           WSSTIE
207200090601     c                   movel     Wtipo         WSSTIE
207300090206     C                   MOVE      'PA'          WSSTIP
207400090206     C                   Z-ADD     FIL(K)        WSSLIN
207500090206     C                   Z-ADD     WTSPPA_r      WSSTSP
207600090206     C                   Z-ADD     WTCLPA_r      WSSTCL
207700090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
207800090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
207900120904     c* old
208000120904     C                   Z-ADD(H)  WTKGPA_r      WSSTKG_ol
208100120904     C                   Z-ADD(H)  WTMCPA_r      WSSTMC_ol
208200100402     c* Spedizioni "E"
208300100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
208400100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
208500100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
208600100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
208700120904     c* old
208800120904     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe_ol
208900120904     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe_ol
209000100402     c* Spedizioni "H"
209100100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
209200100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
209300100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
209400100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
209500120904     c* old
209600120904     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh_ol
209700120904     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh_ol
209800100402     c
209900120905     C                   WRITE     WFLSS600
210000120904     C                   WRITE     wslss500
210100090206     C                   ENDIF
210200110715     c
210300110715     c                   endif
210400110715     c
210500110715     c* Extra budget e clienti particolari
210600110715     c                   movel     wtipo         tipoC             1
210700110715     c                   if        §stat<>' ' and wtipo='R'
210800110715     c                   eval      tipoC='B'
210900110715     c                   endif
211000110715     c                   if        §stat<>' ' and wtipo='P'
211100110715     c                   eval      tipoC='C'
211200110715     c                   endif
211300110715     c
211400091006     C     WTSPIP_re     IFGT      *ZERO
211500091006     C                   Z-ADD     §dal          WSSDAL
211600110715     c                   movel     TipoC         WSSTIE
211700091006     C                   MOVE      'IP'          WSSTIP
211800091006     C                   Z-ADD     FIL(K)        WSSLIN
211900091006     C                   Z-ADD     WTSPIP_re     WSSTSP
212000091006     C                   Z-ADD     WTCLIP_re     WSSTCL
212100120904     C                   Z-ADD(H)  WTKGIP_re     WSSTKG_ex
212200120904     C                   Z-ADD(H)  WTMCIP_re     WSSTMC_ex
212300100402     c* Spedizioni "E"
212400100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
212500100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
212600120904     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe_ex
212700120904     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe_ex
212800100402     c* Spedizioni "H"
212900100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
213000100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
213100120904     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh_ex
213200120904     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh_ex
213300100402     c
213400091006     C                   WRITE     WSLSS550
213500091006     C                   ENDIF
213600100402     c
213700091006     C     WTSPIA_re     IFGT      *ZERO
213800091006     C                   Z-ADD     §dal          WSSDAL
213900110715     c                   movel     tipoC         WSSTIE
214000091006     C                   MOVE      'IA'          WSSTIP
214100091006     C                   Z-ADD     FIL(K)        WSSLIN
214200091006     C                   Z-ADD     WTSPIA_re     WSSTSP
214300091006     C                   Z-ADD     WTCLIA_re     WSSTCL
214400120904     C                   Z-ADD(H)  WTKGIA_re     WSSTKG_ex
214500120904     C                   Z-ADD(H)  WTMCIA_re     WSSTMC_ex
214600100402     c* Spedizioni "E"
214700100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
214800100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
214900120904     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe_ex
215000120904     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe_ex
215100100402     c* Spedizioni "H"
215200100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
215300100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
215400120904     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh_ex
215500120904     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh_ex
215600100402     c
215700091006     C                   WRITE     WSLSS550
215800091006     C                   ENDIF
215900100402     c
216000091006     C     WTSPEI_Re     IFGT      *ZERO
216100091006     C                   Z-ADD     §dal          WSSDAL
216200110715     c                   movel     tipoC         WSSTIE
216300091006     C                   MOVE      'EI'          WSSTIP
216400091006     C                   Z-ADD     FIL(K)        WSSLIN
216500091006     C                   Z-ADD     WTSPEI_re     WSSTSP
216600091006     C                   Z-ADD     WTCLEI_re     WSSTCL
216700120904     C                   Z-ADD(H)  WTKGEI_re     WSSTKG_ex
216800120904     C                   Z-ADD(H)  WTMCEI_re     WSSTMC_ex
216900100402     c* Spedizioni "E"
217000100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
217100100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
217200120904     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe_ex
217300120904     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe_ex
217400100402     c* Spedizioni "H"
217500100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
217600100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
217700120904     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh_ex
217800120904     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh_ex
217900100402     c
218000091006     C                   WRITE     WSLSS550
218100091006     C                   ENDIF
218200100402     c
218300091006     C     WTSPEE_re     IFGT      *ZERO
218400091006     C                   Z-ADD     §dal          WSSDAL
218500110715     c                   movel     tipoC         WSSTIE
218600091006     C                   MOVE      'EE'          WSSTIP
218700091006     C                   Z-ADD     FIL(K)        WSSLIN
218800091006     C                   Z-ADD     WTSPEE_re     WSSTSP
218900091006     C                   Z-ADD     WTCLEE_re     WSSTCL
219000120904     C                   Z-ADD(H)  WTKGEE_re     WSSTKG_ex
219100120904     C                   Z-ADD(H)  WTMCEE_re     WSSTMC_ex
219200100402     c* Spedizioni "E"
219300100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
219400100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
219500120904     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe_ex
219600120904     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe_ex
219700100402     c* Spedizioni "h"
219800100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
219900100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
220000120904     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh_ex
220100120904     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh_ex
220200100402     c
220300091006     C                   WRITE     WSLSS550
220400091006     C                   ENDIF
220500091006     C*
220600091006     C     WTSPED_re     IFGT      *ZERO
220700091006     C                   Z-ADD     §dal          WSSDAL
220800110715     c                   movel     tipoC         WSSTIE
220900091006     C                   MOVE      'ED'          WSSTIP
221000091006     C                   Z-ADD     FIL(K)        WSSLIN
221100091006     C                   Z-ADD     WTSPED_re     WSSTSP
221200091006     C                   Z-ADD     WTCLED_re     WSSTCL
221300120904     C                   Z-ADD(H)  WTKGED_re     WSSTKG_ex
221400120904     C                   Z-ADD(H)  WTMCED_re     WSSTMC_ex
221500100402     c* Spedizioni "E"
221600100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
221700100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
221800120904     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe_ex
221900120904     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe_ex
222000100402     c* Spedizioni "H"
222100100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
222200100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
222300120904     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh_ex
222400120904     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh_ex
222500100402     c
222600091006     C                   WRITE     WSLSS550
222700091006     C                   ENDIF
222800091006     C*
222900091006     C     WTSPEX_re     IFGT      *ZERO
223000091006     C                   Z-ADD     §dal          WSSDAL
223100110715     c                   movel     tipoC         WSSTIE
223200091006     C                   MOVE      'EX'          WSSTIP
223300091006     C                   Z-ADD     FIL(K)        WSSLIN
223400091006     C                   Z-ADD     WTSPEX_re     WSSTSP
223500091006     C                   Z-ADD     WTCLEX_re     WSSTCL
223600120904     C                   Z-ADD(H)  WTKGEX_re     WSSTKG_ex
223700120904     C                   Z-ADD(H)  WTMCEX_re     WSSTMC_ex
223800100402     c* Spedizioni "E"
223900100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
224000100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
224100120904     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe_ex
224200120904     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe_ex
224300100402     c* Spedizioni "H"
224400100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
224500100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
224600120904     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh_ex
224700120904     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh_ex
224800100402     c
224900091006     C                   WRITE     WSLSS550
225000091006     C                   ENDIF
225100091006     C*
225200091006     C     WTSPEA_re     IFGT      *ZERO
225300091006     C                   Z-ADD     §dal          WSSDAL
225400110715     c                   movel     tipoC         WSSTIE
225500091006     C                   MOVE      'EA'          WSSTIP
225600091006     C                   Z-ADD     FIL(K)        WSSLIN
225700091006     C                   Z-ADD     WTSPEA_re     WSSTSP
225800091006     C                   Z-ADD     WTCLEA_re     WSSTCL
225900120904     C                   Z-ADD(H)  WTKGEA_re     WSSTKG_ex
226000120904     C                   Z-ADD(H)  WTMCEA_re     WSSTMC_ex
226100100402     c* Spedizioni "E"
226200100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
226300100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
226400120904     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe_ex
226500120904     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe_ex
226600100402     c* Spedizioni "H"
226700100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
226800100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
226900120904     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh_ex
227000120904     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh_ex
227100100402     c
227200091006     C                   WRITE     WSLSS550
227300091006     C                   ENDIF
227400091006     C*
227500091006     C     WTSPPA_re     IFGT      *ZERO
227600091006     C                   Z-ADD     §dal          WSSDAL
227700110715     c                   movel     tipoC         WSSTIE
227800091006     C                   MOVE      'PA'          WSSTIP
227900091006     C                   Z-ADD     FIL(K)        WSSLIN
228000091006     C                   Z-ADD     WTSPPA_re     WSSTSP
228100091006     C                   Z-ADD     WTCLPA_re     WSSTCL
228200120904     C                   Z-ADD(H)  WTKGPA_re     WSSTKG_ex
228300120904     C                   Z-ADD(H)  WTMCPA_re     WSSTMC_ex
228400100402     c* Spedizioni "E"
228500100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
228600100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
228700120904     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe_ex
228800120904     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe_ex
228900100402     c* Spedizioni "H"
229000100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
229100100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
229200120904     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh_ex
229300120904     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh_ex
229400100402     c
229500091006     C                   WRITE     WSLSS550
229600091006     C                   ENDIF
229700091006     C                   ENDIF
229800100402
229900090206     c                   enddo
230000090206     c                   endsr
230100970527     C**********************************************************
230200970529     C** CARICAMENTO DATI INIZIALI
230300970527     C**********************************************************
230400970529     C     CARINZ        BEGSR
230500970905     C                   SETOFF                                       90
230600970529     C     KTAS          KLIST
230700970529     C                   KFLD                    WAAS
230800970529     C                   KFLD                    WMGS
230900970529     C     KTAB          KLIST
231000970529     C                   KFLD                    TBLKUT
231100970529     C                   KFLD                    TBLCOD
231200970529     C                   KFLD                    TBLKEY
231300970529     C     KTB           KLIST
231400970529     C                   KFLD                    TBLKUT
231500970529     C                   KFLD                    TBLCOD
231600970529     C                   Z-ADD     1             TBLKUT
231700090204     c     kwfsmp        klist
231800090204     c                   kfld                    k_fil
231900090204     c                   kfld                    k_tip
232000090204     c                   kfld                    k_dat
232100090205     C     kar5          KLIST
232200090205     C                   KFLD                    tasaas
232300090205     C                   KFLD                    taslnp
232400090205     C                   KFLD                    tasnrs
232500090205     C                   KFLD                    tasnsp
232600090205     C                   KFLD                    k_trd
232700090209     C                   KFLD                    tasflp
232800120903     c     kss61         klist
232900120903     c                   kfld                    ks_tie
233000120903     c                   kfld                    ks_tip
233100120903     c                   kfld                    ks_lin
233200120903     c                   kfld                    ks_sca
233300120903     c
233400090915     c* apertura file di stampa congelato in SAVE
233500090915     C* ESEGUO OVRPRTF
233600090915     C                   Z-ADD     61            LUNG             15 5
233700090915     C                   MOVEL     *BLANKS       COMMAN           80
233800090915     C                   MOVEA     CMD           COMMAN
233900090915     C                   CALL      'QCMDEXC'                            H1
234000090915     C                   PARM                    COMMAN
234100090915     C                   PARM                    LUNG
234200090915     C*
234300090915     C                   OPEN      QSYSPRT
234400090915     C*
234500970529     C*
234600970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
234700970527     C** PULIZIA DS
234800121127     C                   DO        400           K                 4 0
234900970527     C     K             OCCUR     DSFIL
235000970527     C                   CLEAR                   DSFIL
235100970527     C                   END
235200121127     C                   DO        400           K
235300100402     C     K             OCCUR     DSFIL_e
235400100402     C                   CLEAR                   DSFIL_e
235500100402     C                   END
235600121127     C                   DO        400           K
235700100402     C     K             OCCUR     DSFIL_h
235800100402     C                   CLEAR                   DSFIL_h
235900100402     C                   END
236000121127     C                   DO        400           K
236100090206     C     K             OCCUR     DSFIL_smp
236200090206     C                   CLEAR                   DSFIL_smp
236300090206     C                   END
236400970527     C*
236500970527     C                   Z-ADD     0             K
236600970527     C     *LOVAL        SETLL     AZORG01L
236700970527     C                   DO        *HIVAL
236800090204     C                   READ      AZORG01L
236900090204     c                   if        %eof(azorg01l)
237000090204     c                   leave
237100090204     c                   endif
237200970527     C     ORGFAG        IFEQ      'F'
237300970527     C     ORGFAG        OREQ      'A'
237400970527     C                   ADD       1             K
237500970527     C                   MOVEL     ORGFIL        FIL(K)
237600090204     C                   MOVEL     ORGdes        DIL(K)
237700090204     C                   z-add     ORGcar        car(K)
237800090204     c                   movel     orgde8        og148
237900090206     c                   if        §oglpo='1' or §oglpo='2'
238000090206     c                   move      §oglpo        lpo(K)
238100090206     c                   else
238200090206     c                   z-add     2             lpo(K)
238300090206     c                   endif
238400090204     c                   movel     orgde3        og143
238500090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
238600090209     c**                 move      'E'           tif(K)
238700090209     c**                 else
238800090209     c**                 move      'I'           tif(K)
238900090209     c**                 endif
239000090209     c                   select
239100090209     c                   when      §ogntw='DPD'
239200090209     c                   move      'D'           tif(K)
239300090209     c                   when      §ogntw='EEX'
239400090209     c                   move      'X'           tif(K)
239500090209     c                   when      §ogntw='FED'
239600090209     c                   move      'F'           tif(K)
239700090209     c                   other
239800090209     c                   move      'I'           tif(K)
239900090209     c                   endsl
240000970527     C                   ENDIF
240100090204     C                   ENDDO
240200970529     C*
240300970529     C                   Z-ADD     0             K
240400970529     C                   MOVEL     'TB'          TBLCOD
240500970529     C     KTB           SETLL     TABEL00F
240600970529     C                   DO        *HIVAL
240700090204     C     KTB           READE     TABEL00F
240800090204     c                   if        %eof(tabel00f)
240900090204     c                   leave
241000090204     c                   endif
241100970529     C     TBLFLG        IFNE      '*'
241200970529     C                   MOVEL     TBLUNI        DSTB
241300970529     C     §TBRBL        IFEQ      'N'
241400970529     C     §TBRBL        OREQ      'S'
241500970529     C                   ADD       1             K
241600970529     C                   MOVEL     TBLKEY        TBO(K)
241700970529     C                   ENDIF
241800970529     C                   ENDIF
241900090204     C                   ENDDO
242000120903     c*
242100120903     c* Carico scaglioni di peso
242200120903     C                   Z-ADD     0             II
242300120903     C                   MOVEL     '2L'          TBLCOD
242400120903     C     KTB           SETLL     TABEL00F
242500120903    1C                   DO        *HIVAL
242600120903     C     KTB           READE     TABEL00F
242700120903    2c                   if        %eof(tabel00f)
242800120903     c                   leave
242900120903    2c                   endif
243000120903    2C     TBLFLG        IFNE      '*'
243100120903     C                   MOVEL     TBLUNI        DS2l
243200120903     C                   MOVEL     TBLKEY        WTM03             3
243300120904     C                   MOVEL     TBLKEY        WTM05             5
243400120903    3C     WTM03         IFEQ      'S9R'
243500120903     C                   ADD       1             II
243600120903     C                   MOVE      WTM05         FPR(II)
243700120903     C                   Z-ADD     §2LKGI        KIR(II)
243800120903     C                   Z-ADD     §2LKGF        KFR(II)
243900120903    3C                   ENDIF
244000120903    2C                   ENDIF
244100120903     C                   ENDDO
244200091006     C*
244300091006     c* Lettura TABELLA clienti extra budget
244400110711     c                   if        §stat=' '
244500091006     C                   Z-ADD     0             K
244600091006     C                   MOVEL     'CXB'         TBecod
244700091006     C     tbecod        SETLL     tntbe01l
244800091006     C                   DO        *HIVAL
244900091006     C     tbecod        READE     tntbe01l
245000091006     c                   if        %eof(tntbe01l)
245100091006     c                   leave
245200091006     c                   endif
245300091006
245400091006     C     tbeatb        IFeq      ' '
245500091006     C                   MOVEL     TBeuni        Dcxb
245600091006     C                   ADD       1             K
245700091006     C                   MOVEL     TBeKE1        ksc_ex(k)
245800091007     C                   MOVE      §cxbctr       ksc_ex(k)
245900091006     C                   ENDIF
246000091006     C                   ENDDO
246100110711     c
246200110711     c                   else
246300121121     c* Lettura TABELLA clienti Particolari
246400110711     C                   Z-ADD     0             K
246500110711     C                   MOVEL     'CXQ'         TBecod
246600110711     C     tbecod        SETLL     tntbe01l
246700110711     C                   DO        *HIVAL
246800110711     C     tbecod        READE     tntbe01l
246900110711     c                   if        %eof(tntbe01l)
247000110711     c                   leave
247100110711     c                   endif
247200110711
247300110711     C     tbeatb        IFeq      ' '
247400110711     C                   ADD       1             K
247500110711     C                   MOVEL     TBeKE2        ksc_ex(k)
247600121121     c                   movel     tbeuni        dcxq
247700121121     c                   if        §cxqterm>*zeros
247800121121     c                   movel     §cxqterm      ksc_tr(k)
247900121121     c                   else
248000121121     c                   movel     999           ksc_tr(k)
248100121121     c                   endif
248200121121     c
248300110711     C                   ENDIF
248400110711     C                   ENDDO
248500110711     c                   endif
248600110711     c*
248700110711     c* Controllo riempimento schiera
248800110715     c                   movel     kpjbu         savkpjbu
248900110711     c                   clear                   trul0sds
249000110711     c                   eval      i0stla='L'
249100110711     c                   eval      i0sski='KSC_EX'
249200110711     c                   eval      i0sele=%elem(ksc_ex)
249300110711     c                   eval      i0spie=k
249400110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
249500110711     c                   eval      i0ssif=knsif
249600110711     c                   eval      i0spgm='TNSSL3R2'
249700110711     c                   movel     trul0sds      kpjbu
249800110711     c                   call      'TRUL0SR'
249900110711     c                   parm                    kpjba
250000110715
250100110715     c                   movel     savkpjbu      kpjbu
250200090601     c
250300090204     c* data elaborazione nel formato AAAA/MM/GG
250400090204     c                   time                    dataiso
250500090204     c                   move      dataiso       wdatael
250600110711     c
250700110711     c                   if        §stat=' '
250800090601     c*
250900090601     c* Pulisco i file per i dati che devo rielaborare
251000090601     c                   eval      wrkgetlista =
251100120905     c                             'delete from WFLSS60F where wsstie= '''   +
251200090603     c                              Wtipo +  ''''
251300090601     c                   if        Wtipo='R'
251400090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
251500090603     c                              ' or  wsstie=''G'''
251600090601     c                   endif
251700090609     c*
251800090601     C/EXEC SQL
251900090601     C+ execute immediate :wrkgetlista
252000090601     C/END-EXEC
252100090609     c
252200120904     c*
252300120904     c* Pulisco i file per i dati che devo rielaborare
252400120904     c                   eval      wrkgetlista =
252500120904     c                             'delete from wslss50F where wsstie= '''   +
252600120904     c                              Wtipo +  ''''
252700120904     c                   if        Wtipo='R'
252800120904     c                   eval      wrkgetlista =%trim(wrkgetlista)+
252900120904     c                              ' or  wsstie=''G'''
253000120904     c                   endif
253100120904     c*
253200120904     C/EXEC SQL
253300120904     C+ execute immediate :wrkgetlista
253400120904     C/END-EXEC
253500120904     c*
253600090609     c* Pulisco i file per i dati che devo rielaborare
253700090609     c                   clear                   wrkgetlista
253800090609     c                   eval      wrkgetlista =
253900090609     c                             'delete from WFSMP00F where wmptip= '''   +
254000090609     c                              Wtipo +  ''''
254100090609     c                   if        Wtipo='R'
254200090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
254300090609     c                              ' or  wmptip=''G'''
254400090609     c                   endif
254500110711     c
254600090609     c*
254700090609     C/EXEC SQL
254800090609     C+ execute immediate :wrkgetlista
254900090609     C/END-EXEC
255000120904     c                   endif
255100091007     c*
255200091007     c* Pulisco il file clienti extra budget
255300091007     c                   clear                   wrkgetlista
255400110711     c
255500110711     c                   if        §stat=' '
255600091007     c                   eval      wrkgetlista =
255700120903     c                             'delete from wslss55F where wsstie= '''   +
255800091007     c                              Wtipo +  ''''
255900091007     c                   if        Wtipo='R'
256000091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
256100091007     c                              ' or  wsstie=''G'''
256200091007     c                   endif
256300111028     C/EXEC SQL
256400111028     C+ execute immediate :wrkgetlista
256500111028     C/END-EXEC
256600110711     c
256700110711     c                   else
256800111028     c
256900111028     c* Pulisco il file clienti particolari
257000110711     c                   select
257100110711     c                   when      Wtipo='R'
257200110711     c                   eval      wrkgetlista =
257300110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
257400110711     c                               wsstie= ''B'''
257500110711     c                   when      Wtipo='G'
257600110711     c                   eval      wrkgetlista =
257700110711     c                             'delete from WSLSS55F where wsstie= ''A'''
257800110711     c                   when      Wtipo='P'
257900110711     c                   eval      wrkgetlista =
258000110711     c                             'delete from WSLSS55F where wsstie= ''C'''
258100110711     c                   endsl
258200110711     c
258300091007     C/EXEC SQL
258400091007     C+ execute immediate :wrkgetlista
258500091007     C/END-EXEC
258600111028     c
258700111028     c* Preparo la lettura dei soli clienti particolari con SQL
258800111028     c                   clear                   wrkgetlista
258900111028     c                   eval      wrkgetlista =
259000111028     c                             'select * from titas00f where +
259100111028     c                             ((tasaas*10000) +tasmgs) >=' +
259200111028     c                             %editc(§dal:'X')+ ' and +
259300111028     c                             ((tasaas*10000) +tasmgs) <=' +
259400111028     c                             %editc(§al :'X')+ ' and tasccm in('
259500111028      * ciclo sulla sk dei cod bolla di recupero
259600121203     c                   Do        1100          XX
259700111028     c                   If        ksc_ex(XX) = *blanks
259800111028     c                   Leave
259900111028     c                   EndIf
260000111028     c
260100111028     c                   If        XX >1
260200111028     c                   eval      wrkgetlista =
260300111028     c                             %trim(wrkgetlista) + ','
260400111028     c                   EndIf
260500111028     c
260600111028     c                   eval      wrkgetlista =
260700111028     c                             %trim(wrkgetlista) + ' ''' +
260800111028     c                             %subst(ksc_ex(xx):1:7) + ''''
260900111028     c                   EndDo
261000111028     c
261100111028     c                   Eval      wrkgetlista =
261200111028     c                             %trim(wrkgetlista) + ')'
261300111028     c
261400111028     c
261500111028     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
261600111028     c                             'select * from titas10f where +
261700111028     c                             ((tasaas*10000) +tasmgs) >=' +
261800111028     c                             %editc(§dal:'X')+ ' and +
261900111028     c                             ((tasaas*10000) +tasmgs) <=' +
262000111028     c                             %editc(§al :'X')+ ' and tasccm in('
262100111028      * ciclo sulla sk dei cod bolla di recupero
262200121203     c                   Do        1100          XX                4 0
262300111028     c                   If        ksc_ex(XX) = *blanks
262400111028     c                   Leave
262500111028     c                   EndIf
262600111028     c
262700111028     c                   If        XX >1
262800111028     c                   eval      wrkgetlista =
262900111028     c                             %trim(wrkgetlista) + ','
263000111028     c                   EndIf
263100111028     c
263200111028     c                   eval      wrkgetlista =
263300111028     c                             %trim(wrkgetlista) + ' ''' +
263400111028     c                             %subst(ksc_ex(xx):1:7) + ''''
263500111028     c                   EndDo
263600111028     c
263700111028     c                   Eval      wrkgetlista =
263800111028     c                             %trim(wrkgetlista) + ')'
263900111028     c
264000111028     c                   EndIf
264100111028     c
264200970527     C                   ENDSR
264300100402     c****************************************************************************
264400130408     c     Prepasql2     BEGSR
264500130408     c* Preparo la lettura dei soli clienti particolari con SQL
264600130408     c                   clear                   wrkgetlista
264700130408     c                   eval      wrkgetlista =
264800130408     c                             'select * from titas00f where +
264900130408     c                             ((tasaas*10000) +tasmgs) >=' +
265000130408     c                             %editc(§dal:'X')+ ' and +
265100130408     c                             ((tasaas*10000) +tasmgs) <=' +
265200130408     c                             %editc(§al :'X')+ ' and substr(TASTBL, +
265300130408     c                             1, 1)=''A'' and tasksc in('
265400130408      * ciclo sulla sk dei cod bolla di recupero
265500130408     c                   Do        1100          XX
265600130408     c                   If        ksc_ex(XX) = *blanks
265700130408     c                   Leave
265800130408     c                   EndIf
265900130408     c
266000130408     c                   If        XX >1
266100130408     c                   eval      wrkgetlista =
266200130408     c                             %trim(wrkgetlista) + ','
266300130408     c                   EndIf
266400130408     c
266500130408     c                   eval      wrkgetlista =
266600130408     c                             %trim(wrkgetlista) + ' ''' +
266700130408     c                             %subst(ksc_ex(xx):1:7) + ''''
266800130408     c                   EndDo
266900130408     c
267000130408     c                   Eval      wrkgetlista =
267100130408     c                             %trim(wrkgetlista) + ')'
267200130408     c
267300130408     c
267400130408     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
267500130408     c                             'select * from titas10f where +
267600130408     c                             ((tasaas*10000) +tasmgs) >=' +
267700130408     c                             %editc(§dal:'X')+ ' and +
267800130408     c                             ((tasaas*10000) +tasmgs) <=' +
267900130408     c                             %editc(§al :'X')+ ' and substr(TASTBL, +
268000130408     c                             1, 1)=''A'' and tasksc in('
268100130408      * ciclo sulla sk dei cod bolla di recupero
268200130408     c                   Do        1100          XX                4 0
268300130408     c                   If        ksc_ex(XX) = *blanks
268400130408     c                   Leave
268500130408     c                   EndIf
268600130408     c
268700130408     c                   If        XX >1
268800130408     c                   eval      wrkgetlista =
268900130408     c                             %trim(wrkgetlista) + ','
269000130408     c                   EndIf
269100130408     c
269200130408     c                   eval      wrkgetlista =
269300130408     c                             %trim(wrkgetlista) + ' ''' +
269400130408     c                             %subst(ksc_ex(xx):1:7) + ''''
269500130408     c                   EndDo
269600130408     c
269700130408     c                   Eval      wrkgetlista =
269800130408     c                             %trim(wrkgetlista) + ')'
269900130408     c
270000130408     c
270100130408     C                   ENDSR
270200130408     c****************************************************************************
270300100402     c     SpeITA        BEGSR
270400100402     c* giornaliero
270500100402     c                   if        (tasaas*10000+tasmgs)=§al
270600100402     C                   ADD       1             WTSPIP
270700100402     C                   ADD       TASNCL        WTCLIP
270800100402     C                   ADD       w_peso        WTKGIP
270900100402     C                   ADD       TASVLF        WTMCIP
271000100402     c* tipo servizio  "E"
271100100402     c                   if        tastsp='E'
271200100402     C                   ADD       1             eWTSPIP
271300100402     C                   ADD       TASNCL        eWTCLIP
271400100402     C                   ADD       w_peso        eWTKGIP
271500100402     C                   ADD       TASVLF        eWTMCIP
271600100402     c                   endif
271700100402     c* tipo servizio  "H"
271800100402     c                   if        tastsp='H'
271900100402     C                   ADD       1             hWTSPIP
272000100402     C                   ADD       TASNCL        hWTCLIP
272100100402     C                   ADD       w_peso        hWTKGIP
272200100402     C                   ADD       TASVLF        hWTMCIP
272300100402     c                   endif
272400100402     c**
272500100402     c* Extra budget
272600100402     c                   if        *in35
272700100402     C                   ADD       1             WTSPIP_e
272800100402     C                   ADD       TASNCL        WTCLIP_e
272900100402     C                   ADD       w_peso        WTKGIP_e
273000100402     C                   ADD       TASVLF        WTMCIP_e
273100100402     c* tipo servizio  "E"
273200100402     c                   if        tastsp='E'
273300100402     C                   ADD       1             eWTSPIP_e
273400100402     C                   ADD       TASNCL        eWTCLIP_e
273500100402     C                   ADD       w_peso        eWTKGIP_e
273600100402     C                   ADD       TASVLF        eWTMCIP_e
273700100402     c                   endif
273800100402     c* tipo servizio  "H"
273900100402     c                   if        tastsp='H'
274000100402     C                   ADD       1             hWTSPIP_e
274100100402     C                   ADD       TASNCL        hWTCLIP_e
274200100402     C                   ADD       w_peso        hWTKGIP_e
274300100402     C                   ADD       TASVLF        hWTMCIP_e
274400100402     c                   endif
274500100402
274600100402     c                   endif
274700100402     c                   endif
274800100402     c* riepilogo
274900100402     C                   ADD       1             WTSPIP_r
275000100402     C                   ADD       TASNCL        WTCLIP_r
275100100402     C                   ADD       w_peso        WTKGIP_r
275200100402     C                   ADD       TASVLF        WTMCIP_r
275300100402     c* tipo servizio  "E"
275400100402     c                   if        tastsp='E'
275500100402     C                   ADD       1             eWTSPIP_r
275600100402     C                   ADD       TASNCL        eWTCLIP_r
275700100402     C                   ADD       w_peso        eWTKGIP_r
275800100402     C                   ADD       TASVLF        eWTMCIP_r
275900100402     c                   endif
276000100402     c* tipo servizio  "H"
276100100402     c                   if        tastsp='H'
276200100402     C                   ADD       1             hWTSPIP_r
276300100402     C                   ADD       TASNCL        hWTCLIP_r
276400100402     C                   ADD       w_peso        hWTKGIP_r
276500100402     C                   ADD       TASVLF        hWTMCIP_r
276600100402     c                   endif
276700100402     c* Extra budget
276800100402     c                   if        *in35
276900100402     C                   ADD       1             WTSPIP_re
277000100402     C                   ADD       TASNCL        WTCLIP_re
277100100402     C                   ADD       w_peso        WTKGIP_re
277200100402     C                   ADD       TASVLF        WTMCIP_re
277300100402     c* tipo servizio  "E"
277400100402     c                   if        tastsp='E'
277500100402     C                   ADD       1             eWTSPIP_re
277600100402     C                   ADD       TASNCL        eWTCLIP_re
277700100402     C                   ADD       w_peso        eWTKGIP_re
277800100402     C                   ADD       TASVLF        eWTMCIP_re
277900100402     c                   endif
278000100402     c* tipo servizio  "H"
278100100402     c                   if        tastsp='H'
278200100402     C                   ADD       1             hWTSPIP_re
278300100402     C                   ADD       TASNCL        hWTCLIP_re
278400100402     C                   ADD       w_peso        hWTKGIP_re
278500100402     C                   ADD       TASVLF        hWTMCIP_re
278600100402     c                   endif
278700100402     c
278800100402     c                   endif
278900120903     c
279000120903     c                   eval      ks_tip='IP'
279100120903     c                   exsr      spe_peso
279200120903     c
279300100402     c                   ENDSR
279400100402     c****************************************************************************
279500100402     c     SpeIMPORT     BEGSR
279600100402     c* giornaliero
279700100402     c                   if        (tasaas*10000+tasmgs)=§al
279800100402     C                   ADD       1             WTSPEI
279900100402     C                   ADD       TASNCL        WTCLEI
280000100402     C                   ADD       w_peso        WTKGEI
280100100402     C                   ADD       TASVLF        WTMCEI
280200100402     c* tipo servizio  "E"
280300100402     c                   if        tastsp='E'
280400100402     C                   ADD       1             eWTSPEI
280500100402     C                   ADD       TASNCL        eWTCLEI
280600100402     C                   ADD       w_peso        eWTKGEI
280700100402     C                   ADD       TASVLF        eWTMCEI
280800100402     c                   endif
280900100402     c* tipo servizio  "H"
281000100402     c                   if        tastsp='H'
281100100402     C                   ADD       1             hWTSPEI
281200100402     C                   ADD       TASNCL        hWTCLEI
281300100402     C                   ADD       w_peso        hWTKGEI
281400100402     C                   ADD       TASVLF        hWTMCEI
281500100402     c                   endif
281600100402     c* Extra Budget
281700100402     c                   if        *in35
281800100402     C                   ADD       1             WTSPEI_e
281900100402     C                   ADD       TASNCL        WTCLEI_e
282000100402     C                   ADD       w_peso        WTKGEI_e
282100100402     C                   ADD       TASVLF        WTMCEI_e
282200100402     c* tipo servizio  "E"
282300100402     c                   if        tastsp='E'
282400100402     C                   ADD       1             eWTSPEI_e
282500100402     C                   ADD       TASNCL        eWTCLEI_e
282600100402     C                   ADD       w_peso        eWTKGEI_e
282700100402     C                   ADD       TASVLF        eWTMCEI_e
282800100402     c                   endif
282900100402     c* tipo servizio  "H"
283000100402     c                   if        tastsp='H'
283100100402     C                   ADD       1             hWTSPEI_e
283200100402     C                   ADD       TASNCL        hWTCLEI_e
283300100402     C                   ADD       w_peso        hWTKGEI_e
283400100402     C                   ADD       TASVLF        hWTMCEI_e
283500100402     c                   endif
283600100402     c
283700100402     c                   endif
283800100402     c                   endif
283900100402     c* riepilogo
284000100402     C                   ADD       1             WTSPEI_r
284100100402     C                   ADD       TASNCL        WTCLEI_r
284200100402     C                   ADD       w_peso        WTKGEI_r
284300100402     C                   ADD       TASVLF        WTMCEI_r
284400100402     c* tipo servizio  "E"
284500100402     c                   if        tastsp='E'
284600100402     C                   ADD       1             eWTSPEI_r
284700100402     C                   ADD       TASNCL        eWTCLEI_r
284800100402     C                   ADD       w_peso        eWTKGEI_r
284900100402     C                   ADD       TASVLF        eWTMCEI_r
285000100402     c                   endif
285100100402     c* tipo servizio  "H"
285200100402     c                   if        tastsp='H'
285300100402     C                   ADD       1             hWTSPEI_r
285400100402     C                   ADD       TASNCL        hWTCLEI_r
285500100402     C                   ADD       w_peso        hWTKGEI_r
285600100402     C                   ADD       TASVLF        hWTMCEI_r
285700100402     c                   endif
285800100402     c* extra budget
285900100402     c                   if        *in35
286000100402     C                   ADD       1             WTSPEI_re
286100100402     C                   ADD       TASNCL        WTCLEI_re
286200100402     C                   ADD       w_peso        WTKGEI_re
286300100402     C                   ADD       TASVLF        WTMCEI_re
286400100402     c* tipo servizio  "E"
286500100402     c                   if        tastsp='E'
286600100402     C                   ADD       1             eWTSPEI_re
286700100402     C                   ADD       TASNCL        eWTCLEI_re
286800100402     C                   ADD       w_peso        eWTKGEI_re
286900100402     C                   ADD       TASVLF        eWTMCEI_re
287000100402     c                   endif
287100100402     c* tipo servizio  "H"
287200100402     c                   if        tastsp='H'
287300100402     C                   ADD       1             hWTSPEI_re
287400100402     C                   ADD       TASNCL        hWTCLEI_re
287500100402     C                   ADD       w_peso        hWTKGEI_re
287600100402     C                   ADD       TASVLF        hWTMCEI_re
287700100402     c                   endif
287800100402     c                   endif
287900120904     c
288000120904     c                   eval      ks_tip='EI'
288100120904     c                   exsr      spe_peso
288200120904     c
288300100402     c                   ENDSR
288400100402     c****************************************************************************
288500100402     c     speEXPRESS    BEGSR
288600100402     c* giornaliero
288700100402     c                   if        (tasaas*10000+tasmgs)=§al
288800100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
288900100402     C                   ADD       TASNCL        WTCLEE
289000100402     C                   ADD       w_peso        WTKGEE
289100100402     C                   ADD       TASVLF        WTMCEE
289200100402     c* tipo servizio  "E"
289300100402     c                   if        tastsp='E'
289400100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
289500100402     C                   ADD       TASNCL        eWTCLEE
289600100402     C                   ADD       w_peso        eWTKGEE
289700100402     C                   ADD       TASVLF        eWTMCEE
289800100402     c                   endif
289900100402     c* tipo servizio  "H"
290000100402     c                   if        tastsp='H'
290100100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
290200100402     C                   ADD       TASNCL        hWTCLEE
290300100402     C                   ADD       w_peso        hWTKGEE
290400100402     C                   ADD       TASVLF        hWTMCEE
290500100402     c                   endif
290600100402     c* extra budget
290700100402     c                   if        *in35
290800100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
290900100402     C                   ADD       TASNCL        WTCLEE_e
291000100402     C                   ADD       w_peso        WTKGEE_e
291100100402     C                   ADD       TASVLF        WTMCEE_e
291200100402     c* tipo servizio  "E"
291300100402     c                   if        tastsp='E'
291400100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
291500100402     C                   ADD       TASNCL        eWTCLEE_e
291600100402     C                   ADD       w_peso        eWTKGEE_e
291700100402     C                   ADD       TASVLF        eWTMCEE_e
291800100402     c                   endif
291900100402     c* tipo servizio  "H"
292000100402     c                   if        tastsp='H'
292100100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
292200100402     C                   ADD       TASNCL        hWTCLEE_e
292300100402     C                   ADD       w_peso        hWTKGEE_e
292400100402     C                   ADD       TASVLF        hWTMCEE_e
292500100402     c                   endif
292600100402     c
292700100402     c                   endif
292800100402     c                   endif
292900100402     c* riepilogo
293000100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
293100100402     C                   ADD       TASNCL        WTCLEE_r
293200100402     C                   ADD       w_peso        WTKGEE_r
293300100402     C                   ADD       TASVLF        WTMCEE_r
293400100402     c* tipo servizio  "E"
293500100402     c                   if        tastsp='E'
293600100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
293700100402     C                   ADD       TASNCL        eWTCLEE_r
293800100402     C                   ADD       w_peso        eWTKGEE_r
293900100402     C                   ADD       TASVLF        eWTMCEE_r
294000100402     c                   endif
294100100402     c* tipo servizio  "H"
294200100402     c                   if        tastsp='H'
294300100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
294400100402     C                   ADD       TASNCL        hWTCLEE_r
294500100402     C                   ADD       w_peso        hWTKGEE_r
294600100402     C                   ADD       TASVLF        hWTMCEE_r
294700100402     c                   endif
294800100402     c* extra budget
294900100402     c                   if        *in35
295000100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
295100100402     C                   ADD       TASNCL        WTCLEE_re
295200100402     C                   ADD       w_peso        WTKGEE_re
295300100402     C                   ADD       TASVLF        WTMCEE_re
295400100402     c* tipo servizio  "E"
295500100402     c                   if        tastsp='E'
295600100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
295700100402     C                   ADD       TASNCL        eWTCLEE_re
295800100402     C                   ADD       w_peso        eWTKGEE_re
295900100402     C                   ADD       TASVLF        eWTMCEE_re
296000100402     c                   endif
296100100402     c* tipo servizio  "H"
296200100402     c                   if        tastsp='H'
296300100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
296400100402     C                   ADD       TASNCL        hWTCLEE_re
296500100402     C                   ADD       w_peso        hWTKGEE_re
296600100402     C                   ADD       TASVLF        hWTMCEE_re
296700100402     c                   endif
296800100402     c                   endif
296900120904     c
297000120904     c                   eval      ks_tip='EE'
297100120904     c                   exsr      spe_peso
297200100402     c
297300100402     c                   ENDSR
297400100402     c****************************************************************************
297500100402     c     speDPD        BEGSR
297600100402     c* giornaliero
297700100402     c                   if        (tasaas*10000+tasmgs)=§al
297800100402     C                   ADD       1             WTSPED                         DPD
297900100402     C                   ADD       TASNCL        WTCLED
298000100402     C                   ADD       w_peso        WTKGED
298100100402     C                   ADD       TASVLF        WTMCED
298200100402     c* tipo servizio  "E"
298300100402     c                   if        tastsp='E'
298400100402     C                   ADD       1             eWTSPED                         DPD
298500100402     C                   ADD       TASNCL        eWTCLED
298600100402     C                   ADD       w_peso        eWTKGED
298700100402     C                   ADD       TASVLF        eWTMCED
298800100402     c                   endif
298900100402     c* tipo servizio  "H"
299000100402     c                   if        tastsp='H'
299100100402     C                   ADD       1             hWTSPED                         DPD
299200100402     C                   ADD       TASNCL        hWTCLED
299300100402     C                   ADD       w_peso        hWTKGED
299400100402     C                   ADD       TASVLF        hWTMCED
299500100402     c                   endif
299600100402     c* extra budget
299700100402     c                   if        *in35
299800100402     C                   ADD       1             WTSPED_e                       DPD
299900100402     C                   ADD       TASNCL        WTCLED_e
300000100402     C                   ADD       w_peso        WTKGED_e
300100100402     C                   ADD       TASVLF        WTMCED_e
300200100402     c* tipo servizio  "E"
300300100402     c                   if        tastsp='E'
300400100402     C                   ADD       1             eWTSPED_e                       DPD
300500100402     C                   ADD       TASNCL        eWTCLED_e
300600100402     C                   ADD       w_peso        eWTKGED_e
300700100402     C                   ADD       TASVLF        eWTMCED_e
300800100402     c                   endif
300900100402     c* tipo servizio  "H"
301000100402     c                   if        tastsp='H'
301100100402     C                   ADD       1             hWTSPED_e                       DPD
301200100402     C                   ADD       TASNCL        hWTCLED_e
301300100402     C                   ADD       w_peso        hWTKGED_e
301400100402     C                   ADD       TASVLF        hWTMCED_e
301500100402     c                   endif
301600100402     c
301700100402     c                   endif
301800100402     c                   endif
301900100402     c* riepilogo
302000100402     C                   ADD       1             WTSPED_r                       DPD
302100100402     C                   ADD       TASNCL        WTCLED_r
302200100402     C                   ADD       w_peso        WTKGED_r
302300100402     C                   ADD       TASVLF        WTMCED_r
302400100402     c* tipo servizio  "E"
302500100402     c                   if        tastsp='E'
302600100402     C                   ADD       1             eWTSPED_r                       DPD
302700100402     C                   ADD       TASNCL        eWTCLED_r
302800100402     C                   ADD       w_peso        eWTKGED_r
302900100402     C                   ADD       TASVLF        eWTMCED_r
303000100402     c                   endif
303100100402     c* tipo servizio  "H"
303200100402     c                   if        tastsp='H'
303300100402     C                   ADD       1             hWTSPED_r                       DPD
303400100402     C                   ADD       TASNCL        hWTCLED_r
303500100402     C                   ADD       w_peso        hWTKGED_r
303600100402     C                   ADD       TASVLF        hWTMCED_r
303700100402     c                   endif
303800100402     c* extra budget
303900100402     c                   if        *in35
304000100402     C                   ADD       1             WTSPED_re                       DPD
304100100402     C                   ADD       TASNCL        WTCLED_re
304200100402     C                   ADD       w_peso        WTKGED_re
304300100402     C                   ADD       TASVLF        WTMCED_re
304400100402     c* tipo servizio  "E"
304500100402     c                   if        tastsp='E'
304600100402     C                   ADD       1             eWTSPED_re                       DPD
304700100402     C                   ADD       TASNCL        eWTCLED_re
304800100402     C                   ADD       w_peso        eWTKGED_re
304900100402     C                   ADD       TASVLF        eWTMCED_re
305000100402     c                   endif
305100100402     c* tipo servizio  "H"
305200100402     c                   if        tastsp='H'
305300100402     C                   ADD       1             hWTSPED_re                       DPD
305400100402     C                   ADD       TASNCL        hWTCLED_re
305500100402     C                   ADD       w_peso        hWTKGED_re
305600100402     C                   ADD       TASVLF        hWTMCED_re
305700100402     c                   endif
305800100402     c                   endif
305900120904     c
306000120904     c                   eval      ks_tip='ED'
306100120904     c                   exsr      spe_peso
306200120904     c
306300100402     c                   ENDSR
306400100402     c****************************************************************************
306500100402     c     speFEDEX      BEGSR
306600100402     c* giornaliero
306700100402     c                   if        (tasaas*10000+tasmgs)=§al
306800100402     C                   ADD       1             WTSPEX                         FEDEX
306900100402     C                   ADD       TASNCL        WTCLEX
307000100402     C                   ADD       w_peso        WTKGEX
307100100402     C                   ADD       TASVLF        WTMCEX
307200100402     c* tipo servizio  "E"
307300100402     c                   if        tastsp='E'
307400100402     C                   ADD       1             eWTSPEX                         FEDEX
307500100402     C                   ADD       TASNCL        eWTCLEX
307600100402     C                   ADD       w_peso        eWTKGEX
307700100402     C                   ADD       TASVLF        eWTMCEX
307800100402     c                   endif
307900100402     c* tipo servizio  "H"
308000100402     c                   if        tastsp='H'
308100100402     C                   ADD       1             hWTSPEX                         FEDEX
308200100402     C                   ADD       TASNCL        hWTCLEX
308300100402     C                   ADD       w_peso        hWTKGEX
308400100402     C                   ADD       TASVLF        hWTMCEX
308500100402     c                   endif
308600100402     c* extra budget
308700100402     c                   if        *in35
308800100402     C                   ADD       1             WTSPEX_e                       FEDEX
308900100402     C                   ADD       TASNCL        WTCLEX_e
309000100402     C                   ADD       w_peso        WTKGEX_e
309100100402     C                   ADD       TASVLF        WTMCEX_e
309200100402     c* tipo servizio  "E"
309300100402     c                   if        tastsp='E'
309400100402     C                   ADD       1             eWTSPEX_e                       FEDEX
309500100402     C                   ADD       TASNCL        eWTCLEX_e
309600100402     C                   ADD       w_peso        eWTKGEX_e
309700100402     C                   ADD       TASVLF        eWTMCEX_e
309800100402     c                   endif
309900100402     c* tipo servizio  "H"
310000100402     c                   if        tastsp='H'
310100100402     C                   ADD       1             hWTSPEX_e                       FEDEX
310200100402     C                   ADD       TASNCL        hWTCLEX_e
310300100402     C                   ADD       w_peso        hWTKGEX_e
310400100402     C                   ADD       TASVLF        hWTMCEX_e
310500100402     c                   endif
310600100402     c                   endif
310700100402     c                   endif
310800100402     c* riepilogo
310900100402     C                   ADD       1             WTSPEX_r                       FEDEX
311000100402     C                   ADD       TASNCL        WTCLEX_r
311100100402     C                   ADD       w_peso        WTKGEX_r
311200100402     C                   ADD       TASVLF        WTMCEX_r
311300100402     c* tipo servizio  "E"
311400100402     c                   if        tastsp='E'
311500100402     C                   ADD       1             eWTSPEX_r                       FEDEX
311600100402     C                   ADD       TASNCL        eWTCLEX_r
311700100402     C                   ADD       w_peso        eWTKGEX_r
311800100402     C                   ADD       TASVLF        eWTMCEX_r
311900100402     c                   endif
312000100402     c* tipo servizio  "H"
312100100402     c                   if        tastsp='H'
312200100402     C                   ADD       1             hWTSPEX_r                       FEDEX
312300100402     C                   ADD       TASNCL        hWTCLEX_r
312400100402     C                   ADD       w_peso        hWTKGEX_r
312500100402     C                   ADD       TASVLF        hWTMCEX_r
312600100402     c                   endif
312700100402     c* Extra budget
312800100402     c                   if        *in35
312900100402     C                   ADD       1             WTSPEX_re                       FEDEX
313000100402     C                   ADD       TASNCL        WTCLEX_re
313100100402     C                   ADD       w_peso        WTKGEX_re
313200100402     C                   ADD       TASVLF        WTMCEX_re
313300100402     c* tipo servizio  "E"
313400100402     c                   if        tastsp='E'
313500100402     C                   ADD       1             eWTSPEX_re                       FEDEX
313600100402     C                   ADD       TASNCL        eWTCLEX_re
313700100402     C                   ADD       w_peso        eWTKGEX_re
313800100402     C                   ADD       TASVLF        eWTMCEX_re
313900100402     c                   endif
314000100402     c* tipo servizio  "H"
314100100402     c                   if        tastsp='H'
314200100402     C                   ADD       1             hWTSPEX_re                       FEDEX
314300100402     C                   ADD       TASNCL        hWTCLEX_re
314400100402     C                   ADD       w_peso        hWTKGEX_re
314500100402     C                   ADD       TASVLF        hWTMCEX_re
314600100402     c                   endif
314700100402     c
314800100402     c                   endif
314900120904     c
315000120904     c                   eval      ks_tip='EX'
315100120904     c                   exsr      spe_peso
315200120904     c
315300100402     c                   ENDSR
315400100402     c****************************************************************************
315500100402     c     speITA_LNA    BEGSR
315600100402     c* giornaliero
315700100402     c                   if        (tasaas*10000+tasmgs)=§al
315800100402     C                   ADD       1             WTSPIA
315900100402     C                   ADD       TASNCL        WTCLIA
316000100402     C                   ADD       W_peso        WTKGIA
316100100402     C                   ADD       TASVLF        WTMCIA
316200100402     c* tipo servizio  "E"
316300100402     c                   if        tastsp='E'
316400100402     C                   ADD       1             eWTSPIA
316500100402     C                   ADD       TASNCL        eWTCLIA
316600100402     C                   ADD       W_peso        eWTKGIA
316700100402     C                   ADD       TASVLF        eWTMCIA
316800100402     c                   endif
316900100402     c* tipo servizio  "H"
317000100402     c                   if        tastsp='H'
317100100402     C                   ADD       1             hWTSPIA
317200100402     C                   ADD       TASNCL        hWTCLIA
317300100402     C                   ADD       W_peso        hWTKGIA
317400100402     C                   ADD       TASVLF        hWTMCIA
317500100402     c                   endif
317600100402     c* extra budget
317700100402     c                   if        *in35
317800100402     C                   ADD       1             WTSPIA_e
317900100402     C                   ADD       TASNCL        WTCLIA_e
318000100402     C                   ADD       w_peso        WTKGIA_e
318100100402     C                   ADD       TASVLF        WTMCIA_e
318200100402     c* tipo servizio  "E"
318300100402     c                   if        tastsp='E'
318400100402     C                   ADD       1             eWTSPIA_e
318500100402     C                   ADD       TASNCL        eWTCLIA_e
318600100402     C                   ADD       w_peso        eWTKGIA_e
318700100402     C                   ADD       TASVLF        eWTMCIA_e
318800100402     c                   endif
318900100402     c* tipo servizio  "H"
319000100402     c                   if        tastsp='H'
319100100402     C                   ADD       1             hWTSPIA_e
319200100402     C                   ADD       TASNCL        hWTCLIA_e
319300100402     C                   ADD       w_peso        hWTKGIA_e
319400100402     C                   ADD       TASVLF        hWTMCIA_e
319500100402     c                   endif
319600100402     c
319700100402     c                   endif
319800100402     c                   endif
319900100402     c* riepilogo
320000100402     C                   ADD       1             WTSPIA_r
320100100402     C                   ADD       TASNCL        WTCLIA_r
320200100402     C                   ADD       w_peso        WTKGIA_r
320300100402     C                   ADD       TASVLF        WTMCIA_r
320400100402     c* tipo servizio  "E"
320500100402     c                   if        tastsp='E'
320600100402     C                   ADD       1             eWTSPIA_r
320700100402     C                   ADD       TASNCL        eWTCLIA_r
320800100402     C                   ADD       w_peso        eWTKGIA_r
320900100402     C                   ADD       TASVLF        eWTMCIA_r
321000100402     c                   endif
321100100402     c* tipo servizio  "H"
321200100402     c                   if        tastsp='H'
321300100402     C                   ADD       1             hWTSPIA_r
321400100402     C                   ADD       TASNCL        hWTCLIA_r
321500100402     C                   ADD       w_peso        hWTKGIA_r
321600100402     C                   ADD       TASVLF        hWTMCIA_r
321700100402     c                   endif
321800100402     c* extra budget
321900100402     c                   if        *in35
322000100402     C                   ADD       1             WTSPIA_re
322100100402     C                   ADD       TASNCL        WTCLIA_re
322200100402     C                   ADD       w_peso        WTKGIA_re
322300100402     C                   ADD       TASVLF        WTMCIA_re
322400100402     c* tipo servizio  "E"
322500100402     c                   if        tastsp='E'
322600100402     C                   ADD       1             eWTSPIA_re
322700100402     C                   ADD       TASNCL        eWTCLIA_re
322800100402     C                   ADD       w_peso        eWTKGIA_re
322900100402     C                   ADD       TASVLF        eWTMCIA_re
323000100402     c                   endif
323100100402     c* tipo servizio  "H"
323200100402     c                   if        tastsp='H'
323300100402     C                   ADD       1             hWTSPIA_re
323400100402     C                   ADD       TASNCL        hWTCLIA_re
323500100402     C                   ADD       w_peso        hWTKGIA_re
323600100402     C                   ADD       TASVLF        hWTMCIA_re
323700100402     c                   endif
323800100402     c                   endif
323900120904     c
324000120904     c                   eval      ks_tip='IA'
324100120904     c                   exsr      spe_peso
324200120904     c
324300100402     c                   ENDSR
324400100402     c****************************************************************************
324500100402     c     speEXPORT     BEGSR
324600100402     c* giornaliero
324700100402     c                   if        (tasaas*10000+tasmgs)=§al
324800100402     C                   ADD       1             WTSPEA
324900100402     C                   ADD       TASNCL        WTCLEA
325000100402     C                   ADD       w_peso        WTKGEA
325100100402     C                   ADD       TASVLF        WTMCEA
325200100402     c* tipo servizio  "E"
325300100402     c                   if        tastsp='E'
325400100402     C                   ADD       1             eWTSPEA
325500100402     C                   ADD       TASNCL        eWTCLEA
325600100402     C                   ADD       w_peso        eWTKGEA
325700100402     C                   ADD       TASVLF        eWTMCEA
325800100402     c                   endif
325900100402     c* tipo servizio  "H"
326000100402     c                   if        tastsp='H'
326100100402     C                   ADD       1             hWTSPEA
326200100402     C                   ADD       TASNCL        hWTCLEA
326300100402     C                   ADD       w_peso        hWTKGEA
326400100402     C                   ADD       TASVLF        hWTMCEA
326500100402     c                   endif
326600100402     c* extra budget
326700100402     c                   if        *in35
326800100402     C                   ADD       1             WTSPEA_e
326900100402     C                   ADD       TASNCL        WTCLEA_e
327000100402     C                   ADD       w_peso        WTKGEA_e
327100100402     C                   ADD       TASVLF        WTMCEA_e
327200100402     c* tipo servizio  "E"
327300100402     c                   if        tastsp='E'
327400100402     C                   ADD       1             eWTSPEA_e
327500100402     C                   ADD       TASNCL        eWTCLEA_e
327600100402     C                   ADD       w_peso        eWTKGEA_e
327700100402     C                   ADD       TASVLF        eWTMCEA_e
327800100402     c                   endif
327900100402     c* tipo servizio  "H"
328000100402     c                   if        tastsp='H'
328100100402     C                   ADD       1             hWTSPEA_e
328200100402     C                   ADD       TASNCL        hWTCLEA_e
328300100402     C                   ADD       w_peso        hWTKGEA_e
328400100402     C                   ADD       TASVLF        hWTMCEA_e
328500100402     c                   endif
328600100402     c
328700100402     c                   endif
328800100402     c                   endif
328900100402     c* riepilogo
329000100402     C                   ADD       1             WTSPEA_r
329100100402     C                   ADD       TASNCL        WTCLEA_r
329200100402     C                   ADD       w_peso        WTKGEA_r
329300100402     C                   ADD       TASVLF        WTMCEA_r
329400100402     c* tipo servizio  "E"
329500100402     c                   if        tastsp='E'
329600100402     C                   ADD       1             eWTSPEA_r
329700100402     C                   ADD       TASNCL        eWTCLEA_r
329800100402     C                   ADD       w_peso        eWTKGEA_r
329900100402     C                   ADD       TASVLF        eWTMCEA_r
330000100402     c                   endif
330100100402     c* tipo servizio  "H"
330200100402     c                   if        tastsp='H'
330300100402     C                   ADD       1             hWTSPEA_r
330400100402     C                   ADD       TASNCL        hWTCLEA_r
330500100402     C                   ADD       w_peso        hWTKGEA_r
330600100402     C                   ADD       TASVLF        hWTMCEA_r
330700100402     c                   endif
330800100402     c* extra budget
330900100402     c                   if        *in35
331000100402     C                   ADD       1             WTSPEA_re
331100100402     C                   ADD       TASNCL        WTCLEA_re
331200100402     C                   ADD       w_peso        WTKGEA_re
331300100402     C                   ADD       TASVLF        WTMCEA_re
331400100402     c* tipo servizio  "E"
331500100402     c                   if        tastsp='E'
331600100402     C                   ADD       1             eWTSPEA_re
331700100402     C                   ADD       TASNCL        eWTCLEA_re
331800100402     C                   ADD       w_peso        eWTKGEA_re
331900100402     C                   ADD       TASVLF        eWTMCEA_re
332000100402     c                   endif
332100100402     c* tipo servizio  "H"
332200100402     c                   if        tastsp='H'
332300100402     C                   ADD       1             hWTSPEA_re
332400100402     C                   ADD       TASNCL        hWTCLEA_re
332500100402     C                   ADD       w_peso        hWTKGEA_re
332600100402     C                   ADD       TASVLF        hWTMCEA_re
332700100402     c                   endif
332800100402     c
332900100402     c                   endif
333000120904     c
333100120904     c                   eval      ks_tip='EA'
333200120904     c                   exsr      spe_peso
333300120904     c
333400100402     c                   ENDSR
333500120903     c****************************************************************************
333600120903     c     SPE_peso      BEGSR
333700120903     c
333800120905     c* scaglioni di peso solo se richiesto
333900120905    1c                   if        §stat=' ' and §opz='4'
334000120903     c
334100120903     c* in base al peso trovo lo scaglione e faccio subito la write
334200120903     c                   z-add     1             II
334300120903     c                   clear                   ks_sca
334400120903     c                   dow       ks_sca  =0
334500120903     C     W_peso        IFGT      KIR(Ii)
334600120903     C     W_peso        ANDLE     KFR(Ii)
334700120904     C                   MOVEL     KFR(Ii)       ks_sca
334800120903     C                   endif
334900120903     c                   add       1             II
335000120903     c                   enddo
335100120903     c
335200120903     c* la write/update per peso la faccio subito
335300120903     c
335400120903    2c                   if        Wtipo='G' or Wtipo='R'
335500120903     c
335600120903    3c                   if        (tasaas*10000+tasmgs)=§al
335700120903     c                   eval      ks_tie='G'
335800120903     c                   eval      ks_lin=fil(k)
335900120905     c     kss61         chain     WFLSS61l
336000120903     c
336100120905     c                   if        not %found(WFLSS61l)
336200120905     c                   clear                   WFLSS600
336300120903     c                   eval      wsstie=ks_tie
336400120903     c                   eval      wsstip=ks_tip
336500120903     c                   eval      wsskgf=ks_sca
336600120904     c                   eval      wsslin=ks_lin
336700120903     c                   eval      wssdal=§al
336800120903     c                   eval      wssall=§al
336900120903     c                   endif
337000120903     c
337100120903     c                   exsr      Scrivi_pes
337200120903     c
337300120903    3c                   endif
337400120903     c
337500120903    2c                   endif
337600120903
337700120903    2c                   if        Wtipo='P' or Wtipo='R'
337800120903     c                   eval      ks_tie=wtipo
337900120903     c                   eval      ks_lin=fil(k)
338000120905     c     kss61         chain     WFLSS61l
338100120903     c
338200120905     c                   if        not %found(WFLSS61l)
338300120905     c                   clear                   WFLSS600
338400120903     c                   eval      wsstie=ks_tie
338500120903     c                   eval      wsstip=ks_tip
338600120903     c                   eval      wsskgf=ks_sca
338700120904     c                   eval      wsslin=ks_lin
338800120903     c                   eval      wssdal=§dal
338900120903     c                   eval      wssall=§al
339000120903     c                   endif
339100120903     c
339200120903     c                   exsr      Scrivi_pes
339300120903     c
339400120903    2c                   endif
339500120903     c
339600120903    1c                   endif
339700120903     c                   ENDSR
339800120903     c****************************************************************************
339900120903     c     Scrivi_pes    BEGSR
340000120903     c                   add       1             wsstsp
340100120903     C                   ADD       TASNCL        wsstcl
340200120903     C                   ADD       w_peso        wsstkg
340300120903     C                   ADD       TASVLF        wsstmc
340400120903     c                   if        tastsp='E'
340500120903     c                   add       1             wsstspe
340600120903     C                   ADD       TASNCL        wsstcle
340700120903     C                   ADD       w_peso        wsstkge
340800120903     C                   ADD       TASVLF        wsstmce
340900120903     c                   endif
341000120903     c                   if        tastsp='h'
341100120903     c                   add       1             wsstsph
341200120903     C                   ADD       TASNCL        wsstclh
341300120903     C                   ADD       w_peso        wsstkgh
341400120903     C                   ADD       TASVLF        wsstmch
341500120903     c                   endif
341600120903     c
341700120905     c                   if        not %found(WFLSS61l)
341800120905     c                   write     WFLSS600
341900120903     c                   else
342000120905     c                   update    WFLSS600
342100120903     c                   endif
342200120903     c
342300120903     c                   ENDSR
342400100402     c****************************************************************************
342500970519     OQSYSPRT   E            TESTA          2 02
342600090206     O                                         +  1 'Terminata elaborazione'
342700090206     o                                         +  2 'statistiche spedizioni'
342800090206     O                                         +  2 'DEL'
342900970519     O                       UDATE         Y   +  1
343000970528     O                       §DAL              +  5 '    /  /  '
343100970528     O                                         +  1 '>'
343200970528     O                       §AL               +  1 '    /  /  '
343300090206     O                                          120 'TNSSL3R1'
343400970519     O                       PAGE          Z    132
343500970519     O                                          128 'PAG.'
343600970529     O          E            TESTA          2
343700970529     O                                           14 'TIPI BOLLA OK:'
343800970529     O                       TBO               +  1
343900970529     O          E            ERR            1
344000970529     O                       TASLNP               3
344100970529     O                       TASLNA            +  1
344200970529     O                       TASAAS            +  1
344300970529     O                       TASMGS            +  1
344400970529     O                       TASNRS            +  1
344500970529     O                       TASNSP            +  1
344600970529     O                       TASTBL            +  1
344700110801     O          E            TESTA       1  1
344800110801     O                                            7 'Clienti'
344900110801     o                       Ptipocli         +   1
345000110801     O                                        +   1 ':'
345100110801     O          E            detksc         1
345200110801     o                       skicli             132
345300090915**
345400090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
