000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200091006     F* TNSSL3R2 H-----------------------------------------------------H
000300090204     F*-------   Statistiche spedizioni
000400080312     F*----------------------------------------------------------------H
000500090204      *? PER IL FILE WSLSS50F:
000600080312     F*? IL PGM NON CONTEGGIA LE SPEDIZIONI IMPORT PER LINEA DI ARRIVO
000700080312     F*? In un conteggio totale quindi di colli partenza e colli in arr
000800080312     F*? ivo ad una filiale, i colli IMPORT MANCANO
000900000000     F*----------------------------------------------------------------H
001000970527     FAZORG01L  IF   E           K DISK
001100990811     FTITAS38C  IF   E           K DISK
001200090205     Ffiar531c  IF   E           K DISK
001300970519     FTABEL00F  IF   E           K DISK
001400091006     Ftntbe01l  IF   E           K DISK
001500090206     FWSLSS50f  o  A E             DISK
001600091006     FWSLSS55f  o  A E             DISK    rename(wslss500:wslss550)
001700090206     FWfsmp00f  o  A E             DISK
001800090915     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)  usropn
001900980826     D FIL             S              3  0 DIM(400)                             FILIALI
002000090204     D DIL             S             20    DIM(400)                             descrizione filiali
002100090204     D car             S              3  0 DIM(400)                             codice area filiale
002200090204     D tif             S              1    DIM(400)                             tipo filiale
002300090204     D lpo             S              1  0 DIM(400)                             livello filiale
002400970529     D TBO             S              2    DIM(20)                              TIPI BOLLA OK
002500090915     D CMD             S              1    DIM(61) CTDATA PERRCD(61)
002600110711     D ksc_EX          S             10    DIM(500)
002700091006     d
002800970527     D KPJBA         E DS
002900090204     D  §DAL                 247    254  0
003000090204     D  §AL                  255    262  0
003100090601     D  §opz                 263    263
003200090601     D  §tipo                264    264
003300110711     D  §stat                265    265
003400090204
003500090204     D og143         E DS
003600090204     D og148         E DS
003700090204
003800970529     D DSTB          E DS
003900090204     D DS05          E DS
004000090205     D Dar5trs       E DS
004100091006     D dcxb          E DS
004200110711     d
004300110711     d* DS x controllo riempimento schiere
004400110711     d trul0sds      e ds
004500090204
004600980826     D DSFIL           DS                  OCCURS(400)
004700970527     D  WTSPIP                 1      8  0
004800970527     D  WTSPIA                 9     16  0
004900970527     D  WTSPEI                17     24  0
005000970527     D  WTSPEE                25     32  0
005100970527     D  WTCLIP                33     40  0
005200970527     D  WTCLIA                41     48  0
005300970527     D  WTCLEI                49     56  0
005400970527     D  WTCLEE                57     64  0
005500980826     D  WTKGIP                65     75  1
005600980826     D  WTKGIA                76     86  1
005700980826     D  WTKGEI                87     97  1
005800980826     D  WTKGEE                98    108  1
005900980826     D  WTMCIP               109    119  3
006000980826     D  WTMCIA               120    130  3
006100980826     D  WTMCEI               131    140  3
006200980826     D  WTMCEE               141    150  3
006300000223     D  WTSPEF               151    158  0
006400000223     D  WTCLEF               159    166  0
006500000223     D  WTKGEF               167    177  1
006600000223     D  WTMCEF               178    188  3
006700000607     D  WTSPEA               189    196  0
006800000607     D  WTCLEA               197    204  0
006900000607     D  WTKGEA               205    215  1
007000000607     D  WTMCEA               216    226  3
007100000607     D  WTSPED               227    234  0
007200000607     D  WTCLED               235    242  0
007300000607     D  WTKGED               243    253  1
007400000630     D  WTMCED               254    264  3
007500000630     D  WTSPPA               265    272  0
007600000630     D  WTCLPA               273    280  0
007700000630     D  WTKGPA               281    291  1
007800000630     D  WTMCPA               292    302  3
007900020321     D  WTSPEX               303    310  0
008000020321     D  WTCLEX               311    318  0
008100020321     D  WTKGEX               319    329  1
008200020321     D  WTMCEX               330    340  3
008300090206     D  WTSPIP_R                           like(WTSPIP)
008400090206     D  WTSPIA_r                           like(wtspia)
008500090206     D  WTSPEI_r                           like(wtspei)
008600090206     D  WTSPEE_r                           like(wtspee)
008700090206     D  WTCLIP_r                           like(wtclip)
008800090206     D  WTCLIA_r                           like(wtclia)
008900090206     D  WTCLEI_r                           like(wtclei)
009000090206     D  WTCLEE_r                           like(wtclee)
009100090206     D  WTKGIP_r                           like(wtkgip)
009200090206     D  WTKGIA_r                           like(wtkgia)
009300090206     D  WTKGEI_r                           like(wtkgei)
009400090206     D  WTKGEE_r                           like(wtkgee)
009500090206     D  WTMCIP_r                           like(wtmcip)
009600090206     D  WTMCIA_r                           like(wtmcia)
009700090206     D  WTMCEI_r                           like(wtmcei)
009800090206     D  WTMCEE_r                           like(wtmcee)
009900090206     D  WTSPEF_r                           like(wtspef)
010000090206     D  WTCLEF_r                           like(wtclef)
010100090206     D  WTKGEF_r                           like(wtkgef)
010200090206     D  WTMCEF_r                           like(wtmcef)
010300090206     D  WTSPEA_r                           like(wtspea)
010400090206     D  WTCLEA_r                           like(wtclea)
010500090206     D  WTKGEA_r                           like(wtkgea)
010600090206     D  WTMCEA_r                           like(wtmcea)
010700090206     D  WTSPED_r                           like(wtsped)
010800090206     D  WTCLED_r                           like(wtcled)
010900090206     D  WTKGED_r                           like(wtkged)
011000090206     D  WTMCED_r                           like(wtmced)
011100090206     D  WTSPPA_r                           like(wtsppa)
011200090206     D  WTCLPA_r                           like(wtclpa)
011300090206     D  WTKGPA_r                           like(wtkgpa)
011400090206     D  WTMCPA_r                           like(wtmcpa)
011500090206     D  WTSPEX_r                           like(wtspex)
011600090206     D  WTCLEX_r                           like(wtclex)
011700090206     D  WTKGEX_r                           like(wtkgex)
011800090206     D  WTMCEX_r                           like(wtmcex)
011900091006     d* Giornaliero extra budget
012000091006     D  WTSPIP_E                           like(WTSPIP)
012100091006     D  WTSPIA_e                           like(wtspia)
012200091006     D  WTSPEI_e                           like(wtspei)
012300091006     D  WTSPEE_e                           like(wtspee)
012400091006     D  WTCLIP_e                           like(wtclip)
012500091006     D  WTCLIA_e                           like(wtclia)
012600091006     D  WTCLEI_e                           like(wtclei)
012700091006     D  WTCLEE_e                           like(wtclee)
012800091006     D  WTKGIP_e                           like(wtkgip)
012900091006     D  WTKGIA_e                           like(wtkgia)
013000091006     D  WTKGEI_e                           like(wtkgei)
013100091006     D  WTKGEE_e                           like(wtkgee)
013200091006     D  WTMCIP_e                           like(wtmcip)
013300091006     D  WTMCIA_e                           like(wtmcia)
013400091006     D  WTMCEI_e                           like(wtmcei)
013500091006     D  WTMCEE_e                           like(wtmcee)
013600091006     D  WTSPEF_e                           like(wtspef)
013700091006     D  WTCLEF_e                           like(wtclef)
013800091006     D  WTKGEF_e                           like(wtkgef)
013900091006     D  WTMCEF_e                           like(wtmcef)
014000091006     D  WTSPEA_e                           like(wtspea)
014100091006     D  WTCLEA_e                           like(wtclea)
014200091006     D  WTKGEA_e                           like(wtkgea)
014300091006     D  WTMCEA_e                           like(wtmcea)
014400091006     D  WTSPED_e                           like(wtsped)
014500091006     D  WTCLED_e                           like(wtcled)
014600091006     D  WTKGED_e                           like(wtkged)
014700091006     D  WTMCED_e                           like(wtmced)
014800091006     D  WTSPPA_e                           like(wtsppa)
014900091006     D  WTCLPA_e                           like(wtclpa)
015000091006     D  WTKGPA_e                           like(wtkgpa)
015100091006     D  WTMCPA_e                           like(wtmcpa)
015200091006     D  WTSPEX_e                           like(wtspex)
015300091006     D  WTCLEX_e                           like(wtclex)
015400091006     D  WTKGEX_e                           like(wtkgex)
015500091006     D  WTMCEX_e                           like(wtmcex)
015600091006     d* Riepilogativo extra budget
015700091006     D  WTSPIP_re                          like(WTSPIP)
015800091006     D  WTSPIA_re                          like(wtspia)
015900091006     D  WTSPEI_re                          like(wtspei)
016000091006     D  WTSPEE_re                          like(wtspee)
016100091006     D  WTCLIP_re                          like(wtclip)
016200091006     D  WTCLIA_re                          like(wtclia)
016300091006     D  WTCLEI_re                          like(wtclei)
016400091006     D  WTCLEE_re                          like(wtclee)
016500091006     D  WTKGIP_re                          like(wtkgip)
016600091006     D  WTKGIA_re                          like(wtkgia)
016700091006     D  WTKGEI_re                          like(wtkgei)
016800091006     D  WTKGEE_re                          like(wtkgee)
016900091006     D  WTMCIP_re                          like(wtmcip)
017000091006     D  WTMCIA_re                          like(wtmcia)
017100091006     D  WTMCEI_re                          like(wtmcei)
017200091006     D  WTMCEE_re                          like(wtmcee)
017300091006     D  WTSPEF_re                          like(wtspef)
017400091006     D  WTCLEF_re                          like(wtclef)
017500091006     D  WTKGEF_re                          like(wtkgef)
017600091006     D  WTMCEF_re                          like(wtmcef)
017700091006     D  WTSPEA_re                          like(wtspea)
017800091006     D  WTCLEA_re                          like(wtclea)
017900091006     D  WTKGEA_re                          like(wtkgea)
018000091006     D  WTMCEA_re                          like(wtmcea)
018100091006     D  WTSPED_re                          like(wtsped)
018200091006     D  WTCLED_re                          like(wtcled)
018300091006     D  WTKGED_re                          like(wtkged)
018400091006     D  WTMCED_re                          like(wtmced)
018500091006     D  WTSPPA_re                          like(wtsppa)
018600091006     D  WTCLPA_re                          like(wtclpa)
018700091006     D  WTKGPA_re                          like(wtkgpa)
018800091006     D  WTMCPA_re                          like(wtmcpa)
018900091006     D  WTSPEX_re                          like(wtspex)
019000091006     D  WTCLEX_re                          like(wtclex)
019100091006     D  WTKGEX_re                          like(wtkgex)
019200091006     D  WTMCEX_re                          like(wtmcex)
019300100402     d
019400100402     D DSFIL_e         DS                  OCCURS(400)
019500100402     D  eWTSPIP                1      8  0
019600100402     D  eWTSPIA                9     16  0
019700100402     D  eWTSPEI               17     24  0
019800100402     D  eWTSPEE               25     32  0
019900100402     D  eWTCLIP               33     40  0
020000100402     D  eWTCLIA               41     48  0
020100100402     D  eWTCLEI               49     56  0
020200100402     D  eWTCLEE               57     64  0
020300100402     D  eWTKGIP               65     75  1
020400100402     D  eWTKGIA               76     86  1
020500100402     D  eWTKGEI               87     97  1
020600100402     D  eWTKGEE               98    108  1
020700100402     D  eWTMCIP              109    119  3
020800100402     D  eWTMCIA              120    130  3
020900100402     D  eWTMCEI              131    140  3
021000100402     D  eWTMCEE              141    150  3
021100100402     D  eWTSPEF              151    158  0
021200100402     D  eWTCLEF              159    166  0
021300100402     D  eWTKGEF              167    177  1
021400100402     D  eWTMCEF              178    188  3
021500100402     D  eWTSPEA              189    196  0
021600100402     D  eWTCLEA              197    204  0
021700100402     D  eWTKGEA              205    215  1
021800100402     D  eWTMCEA              216    226  3
021900100402     D  eWTSPED              227    234  0
022000100402     D  eWTCLED              235    242  0
022100100402     D  eWTKGED              243    253  1
022200100402     D  eWTMCED              254    264  3
022300100402     D  eWTSPPA              265    272  0
022400100402     D  eWTCLPA              273    280  0
022500100402     D  eWTKGPA              281    291  1
022600100402     D  eWTMCPA              292    302  3
022700100402     D  eWTSPEX              303    310  0
022800100402     D  eWTCLEX              311    318  0
022900100402     D  eWTKGEX              319    329  1
023000100402     D  eWTMCEX              330    340  3
023100100402     D  eWTSPIP_R                          like(WTSPIP)
023200100402     D  eWTSPIA_r                          like(wtspia)
023300100402     D  eWTSPEI_r                          like(wtspei)
023400100402     D  eWTSPEE_r                          like(wtspee)
023500100402     D  eWTCLIP_r                          like(wtclip)
023600100402     D  eWTCLIA_r                          like(wtclia)
023700100402     D  eWTCLEI_r                          like(wtclei)
023800100402     D  eWTCLEE_r                          like(wtclee)
023900100402     D  eWTKGIP_r                          like(wtkgip)
024000100402     D  eWTKGIA_r                          like(wtkgia)
024100100402     D  eWTKGEI_r                          like(wtkgei)
024200100402     D  eWTKGEE_r                          like(wtkgee)
024300100402     D  eWTMCIP_r                          like(wtmcip)
024400100402     D  eWTMCIA_r                          like(wtmcia)
024500100402     D  eWTMCEI_r                          like(wtmcei)
024600100402     D  eWTMCEE_r                          like(wtmcee)
024700100402     D  eWTSPEF_r                          like(wtspef)
024800100402     D  eWTCLEF_r                          like(wtclef)
024900100402     D  eWTKGEF_r                          like(wtkgef)
025000100402     D  eWTMCEF_r                          like(wtmcef)
025100100402     D  eWTSPEA_r                          like(wtspea)
025200100402     D  eWTCLEA_r                          like(wtclea)
025300100402     D  eWTKGEA_r                          like(wtkgea)
025400100402     D  eWTMCEA_r                          like(wtmcea)
025500100402     D  eWTSPED_r                          like(wtsped)
025600100402     D  eWTCLED_r                          like(wtcled)
025700100402     D  eWTKGED_r                          like(wtkged)
025800100402     D  eWTMCED_r                          like(wtmced)
025900100402     D  eWTSPPA_r                          like(wtsppa)
026000100402     D  eWTCLPA_r                          like(wtclpa)
026100100402     D  eWTKGPA_r                          like(wtkgpa)
026200100402     D  eWTMCPA_r                          like(wtmcpa)
026300100402     D  eWTSPEX_r                          like(wtspex)
026400100402     D  eWTCLEX_r                          like(wtclex)
026500100402     D  eWTKGEX_r                          like(wtkgex)
026600100402     D  eWTMCEX_r                          like(wtmcex)
026700100402     d* Giornaliero extra budget
026800100402     D  eWTSPIP_E                          like(WTSPIP)
026900100402     D  eWTSPIA_e                          like(wtspia)
027000100402     D  eWTSPEI_e                          like(wtspei)
027100100402     D  eWTSPEE_e                          like(wtspee)
027200100402     D  eWTCLIP_e                          like(wtclip)
027300100402     D  eWTCLIA_e                          like(wtclia)
027400100402     D  eWTCLEI_e                          like(wtclei)
027500100402     D  eWTCLEE_e                          like(wtclee)
027600100402     D  eWTKGIP_e                          like(wtkgip)
027700100402     D  eWTKGIA_e                          like(wtkgia)
027800100402     D  eWTKGEI_e                          like(wtkgei)
027900100402     D  eWTKGEE_e                          like(wtkgee)
028000100402     D  eWTMCIP_e                          like(wtmcip)
028100100402     D  eWTMCIA_e                          like(wtmcia)
028200100402     D  eWTMCEI_e                          like(wtmcei)
028300100402     D  eWTMCEE_e                          like(wtmcee)
028400100402     D  eWTSPEF_e                          like(wtspef)
028500100402     D  eWTCLEF_e                          like(wtclef)
028600100402     D  eWTKGEF_e                          like(wtkgef)
028700100402     D  eWTMCEF_e                          like(wtmcef)
028800100402     D  eWTSPEA_e                          like(wtspea)
028900100402     D  eWTCLEA_e                          like(wtclea)
029000100402     D  eWTKGEA_e                          like(wtkgea)
029100100402     D  eWTMCEA_e                          like(wtmcea)
029200100402     D  eWTSPED_e                          like(wtsped)
029300100402     D  eWTCLED_e                          like(wtcled)
029400100402     D  eWTKGED_e                          like(wtkged)
029500100402     D  eWTMCED_e                          like(wtmced)
029600100402     D  eWTSPPA_e                          like(wtsppa)
029700100402     D  eWTCLPA_e                          like(wtclpa)
029800100402     D  eWTKGPA_e                          like(wtkgpa)
029900100402     D  eWTMCPA_e                          like(wtmcpa)
030000100402     D  eWTSPEX_e                          like(wtspex)
030100100402     D  eWTCLEX_e                          like(wtclex)
030200100402     D  eWTKGEX_e                          like(wtkgex)
030300100402     D  eWTMCEX_e                          like(wtmcex)
030400100402     d* Riepilogativo extra budget
030500100402     D  eWTSPIP_re                         like(WTSPIP)
030600100402     D  eWTSPIA_re                         like(wtspia)
030700100402     D  eWTSPEI_re                         like(wtspei)
030800100402     D  eWTSPEE_re                         like(wtspee)
030900100402     D  eWTCLIP_re                         like(wtclip)
031000100402     D  eWTCLIA_re                         like(wtclia)
031100100402     D  eWTCLEI_re                         like(wtclei)
031200100402     D  eWTCLEE_re                         like(wtclee)
031300100402     D  eWTKGIP_re                         like(wtkgip)
031400100402     D  eWTKGIA_re                         like(wtkgia)
031500100402     D  eWTKGEI_re                         like(wtkgei)
031600100402     D  eWTKGEE_re                         like(wtkgee)
031700100402     D  eWTMCIP_re                         like(wtmcip)
031800100402     D  eWTMCIA_re                         like(wtmcia)
031900100402     D  eWTMCEI_re                         like(wtmcei)
032000100402     D  eWTMCEE_re                         like(wtmcee)
032100100402     D  eWTSPEF_re                         like(wtspef)
032200100402     D  eWTCLEF_re                         like(wtclef)
032300100402     D  eWTKGEF_re                         like(wtkgef)
032400100402     D  eWTMCEF_re                         like(wtmcef)
032500100402     D  eWTSPEA_re                         like(wtspea)
032600100402     D  eWTCLEA_re                         like(wtclea)
032700100402     D  eWTKGEA_re                         like(wtkgea)
032800100402     D  eWTMCEA_re                         like(wtmcea)
032900100402     D  eWTSPED_re                         like(wtsped)
033000100402     D  eWTCLED_re                         like(wtcled)
033100100402     D  eWTKGED_re                         like(wtkged)
033200100402     D  eWTMCED_re                         like(wtmced)
033300100402     D  eWTSPPA_re                         like(wtsppa)
033400100402     D  eWTCLPA_re                         like(wtclpa)
033500100402     D  eWTKGPA_re                         like(wtkgpa)
033600100402     D  eWTMCPA_re                         like(wtmcpa)
033700100402     D  eWTSPEX_re                         like(wtspex)
033800100402     D  eWTCLEX_re                         like(wtclex)
033900100402     D  eWTKGEX_re                         like(wtkgex)
034000100402     D  eWTMCEX_re                         like(wtmcex)
034100100402     d
034200100402     D DSFIL_h         DS                  OCCURS(400)
034300100402     D  hWTSPIP                1      8  0
034400100402     D  hWTSPIA                9     16  0
034500100402     D  hWTSPEI               17     24  0
034600100402     D  hWTSPEE               25     32  0
034700100402     D  hWTCLIP               33     40  0
034800100402     D  hWTCLIA               41     48  0
034900100402     D  hWTCLEI               49     56  0
035000100402     D  hWTCLEE               57     64  0
035100100402     D  hWTKGIP               65     75  1
035200100402     D  hWTKGIA               76     86  1
035300100402     D  hWTKGEI               87     97  1
035400100402     D  hWTKGEE               98    108  1
035500100402     D  hWTMCIP              109    119  3
035600100402     D  hWTMCIA              120    130  3
035700100402     D  hWTMCEI              131    140  3
035800100402     D  hWTMCEE              141    150  3
035900100402     D  hWTSPEF              151    158  0
036000100402     D  hWTCLEF              159    166  0
036100100402     D  hWTKGEF              167    177  1
036200100402     D  hWTMCEF              178    188  3
036300100402     D  hWTSPEA              189    196  0
036400100402     D  hWTCLEA              197    204  0
036500100402     D  hWTKGEA              205    215  1
036600100402     D  hWTMCEA              216    226  3
036700100402     D  hWTSPED              227    234  0
036800100402     D  hWTCLED              235    242  0
036900100402     D  hWTKGED              243    253  1
037000100402     D  hWTMCED              254    264  3
037100100402     D  hWTSPPA              265    272  0
037200100402     D  hWTCLPA              273    280  0
037300100402     D  hWTKGPA              281    291  1
037400100402     D  hWTMCPA              292    302  3
037500100402     D  hWTSPEX              303    310  0
037600100402     D  hWTCLEX              311    318  0
037700100402     D  hWTKGEX              319    329  1
037800100402     D  hWTMCEX              330    340  3
037900100402     D  hWTSPIP_R                          like(WTSPIP)
038000100402     D  hWTSPIA_r                          like(wtspia)
038100100402     D  hWTSPEI_r                          like(wtspei)
038200100402     D  hWTSPEE_r                          like(wtspee)
038300100402     D  hWTCLIP_r                          like(wtclip)
038400100402     D  hWTCLIA_r                          like(wtclia)
038500100402     D  hWTCLEI_r                          like(wtclei)
038600100402     D  hWTCLEE_r                          like(wtclee)
038700100402     D  hWTKGIP_r                          like(wtkgip)
038800100402     D  hWTKGIA_r                          like(wtkgia)
038900100402     D  hWTKGEI_r                          like(wtkgei)
039000100402     D  hWTKGEE_r                          like(wtkgee)
039100100402     D  hWTMCIP_r                          like(wtmcip)
039200100402     D  hWTMCIA_r                          like(wtmcia)
039300100402     D  hWTMCEI_r                          like(wtmcei)
039400100402     D  hWTMCEE_r                          like(wtmcee)
039500100402     D  hWTSPEF_r                          like(wtspef)
039600100402     D  hWTCLEF_r                          like(wtclef)
039700100402     D  hWTKGEF_r                          like(wtkgef)
039800100402     D  hWTMCEF_r                          like(wtmcef)
039900100402     D  hWTSPEA_r                          like(wtspea)
040000100402     D  hWTCLEA_r                          like(wtclea)
040100100402     D  hWTKGEA_r                          like(wtkgea)
040200100402     D  hWTMCEA_r                          like(wtmcea)
040300100402     D  hWTSPED_r                          like(wtsped)
040400100402     D  hWTCLED_r                          like(wtcled)
040500100402     D  hWTKGED_r                          like(wtkged)
040600100402     D  hWTMCED_r                          like(wtmced)
040700100402     D  hWTSPPA_r                          like(wtsppa)
040800100402     D  hWTCLPA_r                          like(wtclpa)
040900100402     D  hWTKGPA_r                          like(wtkgpa)
041000100402     D  hWTMCPA_r                          like(wtmcpa)
041100100402     D  hWTSPEX_r                          like(wtspex)
041200100402     D  hWTCLEX_r                          like(wtclex)
041300100402     D  hWTKGEX_r                          like(wtkgex)
041400100402     D  hWTMCEX_r                          like(wtmcex)
041500100402     d* Giornaliero extra budget
041600100402     D  hWTSPIP_E                          like(WTSPIP)
041700100402     D  hWTSPIA_e                          like(wtspia)
041800100402     D  hWTSPEI_e                          like(wtspei)
041900100402     D  hWTSPEE_e                          like(wtspee)
042000100402     D  hWTCLIP_e                          like(wtclip)
042100100402     D  hWTCLIA_e                          like(wtclia)
042200100402     D  hWTCLEI_e                          like(wtclei)
042300100402     D  hWTCLEE_e                          like(wtclee)
042400100402     D  hWTKGIP_e                          like(wtkgip)
042500100402     D  hWTKGIA_e                          like(wtkgia)
042600100402     D  hWTKGEI_e                          like(wtkgei)
042700100402     D  hWTKGEE_e                          like(wtkgee)
042800100402     D  hWTMCIP_e                          like(wtmcip)
042900100402     D  hWTMCIA_e                          like(wtmcia)
043000100402     D  hWTMCEI_e                          like(wtmcei)
043100100402     D  hWTMCEE_e                          like(wtmcee)
043200100402     D  hWTSPEF_e                          like(wtspef)
043300100402     D  hWTCLEF_e                          like(wtclef)
043400100402     D  hWTKGEF_e                          like(wtkgef)
043500100402     D  hWTMCEF_e                          like(wtmcef)
043600100402     D  hWTSPEA_e                          like(wtspea)
043700100402     D  hWTCLEA_e                          like(wtclea)
043800100402     D  hWTKGEA_e                          like(wtkgea)
043900100402     D  hWTMCEA_e                          like(wtmcea)
044000100402     D  hWTSPED_e                          like(wtsped)
044100100402     D  hWTCLED_e                          like(wtcled)
044200100402     D  hWTKGED_e                          like(wtkged)
044300100402     D  hWTMCED_e                          like(wtmced)
044400100402     D  hWTSPPA_e                          like(wtsppa)
044500100402     D  hWTCLPA_e                          like(wtclpa)
044600100402     D  hWTKGPA_e                          like(wtkgpa)
044700100402     D  hWTMCPA_e                          like(wtmcpa)
044800100402     D  hWTSPEX_e                          like(wtspex)
044900100402     D  hWTCLEX_e                          like(wtclex)
045000100402     D  hWTKGEX_e                          like(wtkgex)
045100100402     D  hWTMCEX_e                          like(wtmcex)
045200100402     d* Riepilogativo extra budget
045300100402     D  hWTSPIP_re                         like(WTSPIP)
045400100402     D  hWTSPIA_re                         like(wtspia)
045500100402     D  hWTSPEI_re                         like(wtspei)
045600100402     D  hWTSPEE_re                         like(wtspee)
045700100402     D  hWTCLIP_re                         like(wtclip)
045800100402     D  hWTCLIA_re                         like(wtclia)
045900100402     D  hWTCLEI_re                         like(wtclei)
046000100402     D  hWTCLEE_re                         like(wtclee)
046100100402     D  hWTKGIP_re                         like(wtkgip)
046200100402     D  hWTKGIA_re                         like(wtkgia)
046300100402     D  hWTKGEI_re                         like(wtkgei)
046400100402     D  hWTKGEE_re                         like(wtkgee)
046500100402     D  hWTMCIP_re                         like(wtmcip)
046600100402     D  hWTMCIA_re                         like(wtmcia)
046700100402     D  hWTMCEI_re                         like(wtmcei)
046800100402     D  hWTMCEE_re                         like(wtmcee)
046900100402     D  hWTSPEF_re                         like(wtspef)
047000100402     D  hWTCLEF_re                         like(wtclef)
047100100402     D  hWTKGEF_re                         like(wtkgef)
047200100402     D  hWTMCEF_re                         like(wtmcef)
047300100402     D  hWTSPEA_re                         like(wtspea)
047400100402     D  hWTCLEA_re                         like(wtclea)
047500100402     D  hWTKGEA_re                         like(wtkgea)
047600100402     D  hWTMCEA_re                         like(wtmcea)
047700100402     D  hWTSPED_re                         like(wtsped)
047800100402     D  hWTCLED_re                         like(wtcled)
047900100402     D  hWTKGED_re                         like(wtkged)
048000100402     D  hWTMCED_re                         like(wtmced)
048100100402     D  hWTSPPA_re                         like(wtsppa)
048200100402     D  hWTCLPA_re                         like(wtclpa)
048300100402     D  hWTKGPA_re                         like(wtkgpa)
048400100402     D  hWTMCPA_re                         like(wtmcpa)
048500100402     D  hWTSPEX_re                         like(wtspex)
048600100402     D  hWTCLEX_re                         like(wtclex)
048700100402     D  hWTKGEX_re                         like(wtkgex)
048800100402     D  hWTMCEX_re                         like(wtmcex)
048900100402     d
049000090206     d* dati giornaliero
049100091006     D DSFIL_smp       DS                  OCCURS(400)
049200090206     D  Wmptsp_g                           like(wmptsp)
049300090206     D  wmptcp_g                           like(wmptcp)
049400090206     D  wmptkp_g                     11  1
049500090206     D  wmptmp_g                     10  3
049600090710
049700090206     D  Wmptsa_g                           like(wmptsa)
049800090206     D  wmptca_g                           like(wmptca)
049900090206     D  wmptka_g                     11  1
050000090206     D  wmptma_g                     10  3
050100090710
050200090206     D  Wmptspt_g                          like(wmptspt)
050300090206     D  wmptcpt_g                          like(wmptcpt)
050400090206     D  wmptkpt_g                    11  1
050500090206     D  wmptmpt_g                    10  3
050600090710
050700090710     D  Wmptsptc_g                         like(wmptspt)
050800090710     D  wmptcptc_g                         like(wmptcpt)
050900090710     D  wmptkptc_g                   11  1
051000090710     D  wmptmptc_g                   10  3
051100090710
051200090206     D  Wmptsat_g                          like(wmptsat)
051300090206     D  wmptcat_g                          like(wmptcat)
051400090206     D  wmptkat_g                    11  1
051500090206     D  wmptmat_g                    10  3
051600090710     d
051700090710     D  Wmptsatc_g                         like(wmptsat)
051800090710     D  wmptcatc_g                         like(wmptcat)
051900090710     D  wmptkatc_g                   11  1
052000090710     D  wmptmatc_g                   10  3
052100090710
052200090206     D  Wmptst_g                           like(wmptst)
052300090206     D  wmptct_g                           like(wmptct)
052400090206     D  wmptkt_g                     11  1
052500090206     D  wmptmt_g                     10  3
052600090710
052700090206     d* dati riepilogo
052800090206     D  Wmptsp_r                           like(wmptsp)
052900090206     D  wmptcp_r                           like(wmptcp)
053000090206     D  wmptkp_r                     11  1
053100090206     D  wmptmp_r                     10  3
053200090710
053300090206     D  Wmptsa_r                           like(wmptsa)
053400090206     D  wmptca_r                           like(wmptca)
053500090206     D  wmptka_r                     11  1
053600090206     D  wmptma_r                     10  3
053700090710
053800090206     D  Wmptspt_r                          like(wmptspt)
053900090206     D  wmptcpt_r                          like(wmptcpt)
054000090206     D  wmptkpt_r                    11  1
054100090206     D  wmptmpt_r                    10  3
054200090710
054300090710     D  Wmptsptc_r                         like(wmptspt)
054400090710     D  wmptcptc_r                         like(wmptcpt)
054500090710     D  wmptkptc_r                   11  1
054600090710     D  wmptmptc_r                   10  3
054700090710
054800090206     D  Wmptsat_r                          like(wmptsat)
054900090206     D  wmptcat_r                          like(wmptcat)
055000090206     D  wmptkat_r                    11  1
055100090206     D  wmptmat_r                    10  3
055200090710
055300090710     D  Wmptsatc_r                         like(wmptsat)
055400090710     D  wmptcatc_r                         like(wmptcat)
055500090710     D  wmptkatc_r                   11  1
055600090710     D  wmptmatc_r                   10  3
055700090710
055800090206     D  Wmptst_r                           like(wmptst)
055900090206     D  wmptct_r                           like(wmptct)
056000090206     D  wmptkt_r                     11  1
056100090206     D  wmptmt_r                     10  3
056200090204
056300091006     D EE              S              3  0
056400091006     D Wdatael         S              8  0
056500091007     D w010a           S             10
056600110801     D PTIPOCLI        S             12
056700110801     D skicli          S            132
056800090204     d dataiso         s               d   datfmt(*iso)
056900090204     d waas            s                   like(tasaas)
057000090204     d wmgs            s                   Like(tasmgs)
057100090224     d w_peso          s                   Like(taspkf)
057200090204     d k_fil           s                   Like(wmpfil)
057300090204     d k_tip           s                   Like(wmptip)
057400090204     d k_dat           s                   Like(wmpdal)
057500090205     d k_trd           s                   Like(ar5trd) inz('TRS')
057600091006     d savccm          s                   Like(tasccm)
057700091006     d savctr          s                   Like(tasctr)
057800110715     d savkpjbu        s                   Like(kpjbu)
057900090601     d
058000090601     d wrkgetlista     s           4096    varying
058100970527     C* RICEVIMENTO PARAMETRI
058200970527     C     *ENTRY        PLIST
058300970527     C                   PARM                    KPJBA
058400970527     C                   MOVEL     §DAL          WAAS
058500970527     C                   MOVE      §DAL          WMGS
058600090601     C                   MOVE      §tipo         Wtipo             1
058700970528     C*
058800970529     C                   EXSR      CARINZ
058900951220     C*
059000990811     C* LETTURA TITAS
059100990811     C     KTAS          SETLL     TITAS38C
059200970527     C                   DO        *HIVAL
059300090204     C                   READ      TITAS38C
059400090204     c* Termina il ciclo se fine file o per data spedizione > data al
059500090204     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
059600090204     c                   leave
059700090204     c                   endif
059800090204     c* Scarto in base al tipo bolla
059900090204     c                   if        %lookup(tastbl:tbo)=0
060000090204     c                   iter
060100090204     c                   endif
060200970527     C*
060300090601     c
060400090601     c* EDPES : 01/06/09
060500090601     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
060600090901     c***                z-add     taspkf        w_peso
060700090901     c* EDPES:  01/09/09
060800090901     c**  riutilizzo il peso VDL
060900090901     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
061000090901     c* 1. VDL totale
061100090901     c* 2. VDL parziale se > di TASPKF
061200090901     c* 3. TASPKF
061300090901     c                   clear                   w_peso
061400090901     c                   if        tasncp=tasncl or (tasncp>0 and taspkc>taspkf)
061500090901     c                   z-add     taspkc        w_peso
061600090901     c                   else
061700090901     c                   z-add     taspkf        w_peso
061800090901     c                   endif
061900090601     c
062000110711     c                   if        §stat=' '
062100090204     c                   exsr      sr_wfsmp
062200110711     c                   endif
062300110711     c
062400090204     c                   exsr      sr_wslss
062500090204     C                   ENDDO
062600090206     c*
062700090206     C* SCARICO DS SU FILE
062800090206     c                   exsr      scrive_file
062900110801     c                   if        §stat=' '
063000110801     c                   eval      ptipocli='EXTRA BUDGET'
063100110801     c                   else
063200110801     c                   eval      ptipocli='PARTICOLARI '
063300110801     c                   endif
063400110801     c
063500090206     c                   except    testa
063600110801     C*
063700110801     c                   z-add     1             k
063800110801     c                   clear                   conta             3 0
063900110801     c                   clear                   skicli
064000110801     c                   dow       ksc_ex(k)<>*blanks
064100110801     c
064200110801     c                   if        §stat=' '
064300110801
064400110801     c                   eval      skicli=%trim(skicli)+' '+
064500110801     c                             %subst(ksc_ex(k):1:7)
064600110801     c                   add       7             conta
064700110801     c*
064800110801     c                   if        %subst(ksc_ex(k):8:3)<>'   '
064900110801     c                   eval      skicli=%trim(skicli)+'/'+
065000110801     c                             %subst(ksc_ex(k):8:3)
065100110801     c                   add       4             conta
065200110801     c                   endif
065300110801     c                   add       1             conta
065400110801     c
065500110801     c
065600110801     c                   if        conta>120
065700110801     c                   except    detksc
065800110801     c                   clear                   conta
065900110801     c                   clear                   skicli
066000110801     c                   endif
066100110801     c
066200110801     c                   else
066300110801     c
066400110801     c                   eval      skicli=%trim(skicli)+' '+
066500110801     c                             %subst(ksc_ex(k):1:7)
066600110801     c                   add       1             conta
066700110801     c
066800110801     c                   if        conta=16
066900110801     c                   except    detksc
067000110801     c                   clear                   conta
067100110801     c                   clear                   skicli
067200110801     c                   endif
067300110801     c
067400110801     c                   endif
067500110801     c
067600110801     c                   add       1             k
067700110801     c                   enddo
067800110801     c
067900110801     c                   if        conta>0
068000110801     c                   except    detksc
068100110801     c                   endif
068200970528     C*
068300970528     C                   SETON                                        LR
068400970519     C*
068500090204     C**********************************************************
068600090204     C** Elaborazione per file wfsmp00f
068700090204     C**********************************************************
068800090204     C     sr_wfsmp      BEGSR
068900090204     c* linea di partenza
069000090206     c                   z-add     1             k
069100090206     c     taslnp        lookup    fil(k)                                 58
069200090206     c                   if        *in58
069300090206     c     k             occur     dsfil_smp
069400090205     c* . . . giornaliero
069500090206     c                   if        (tasaas*10000+tasmgs)=§al
069600090206     C                   add       1             Wmptsp_g
069700090206     C                   add       TASNCL        wmptcp_g
069800090224     C                   add       w_peso        Wmptkp_g
069900090206     C                   add       TASVLF        Wmptmp_g
070000090206     c                   endif
070100090204     c* . . . riepilogo
070200090206     C                   add       1             Wmptsp_r
070300090206     C                   add       TASNCL        wmptcp_r
070400090224     C                   add       w_peso        Wmptkp_r
070500090206     C                   add       TASVLF        Wmptmp_r
070600090206     c                   endif
070700090204     c* linea di arrivo
070800090206     c                   z-add     1             k
070900090206     c     taslna        lookup    fil(k)                                 58
071000090206     c                   if        *in58
071100090206     c     k             occur     dsfil_smp
071200090205     c* . . . giornaliero
071300090206     c                   if        (tasaas*10000+tasmgs)=§al
071400090206     C                   add       1             Wmptsa_g
071500090206     C                   add       TASNCL        wmptca_g
071600090224     C                   add       w_peso        Wmptka_g
071700090206     C                   add       TASVLF        Wmptma_g
071800090206     c                   endif
071900090205     c* . . . riepilogo
072000090206     C                   add       1             Wmptsa_r
072100090206     C                   add       TASNCL        wmptca_r
072200090224     C                   add       w_peso        Wmptka_r
072300090206     C                   add       TASVLF        Wmptma_r
072400090206     c                   endif
072500090710     c* terminal di partenza : esclusa la linea/terminal
072600090710     c                   if        tastfp<>taslnp
072700090206     c                   z-add     1             k
072800090206     c     tastfp        lookup    fil(k)                                 58
072900090206     c                   if        *in58
073000090206     c     k             occur     dsfil_smp
073100090205     c* . . . giornaliero
073200090206     c                   if        (tasaas*10000+tasmgs)=§al
073300090206     C                   add       1             Wmptspt_g
073400090206     C                   add       TASNCL        wmptcpt_g
073500090224     C                   add       w_peso        Wmptkpt_g
073600090206     C                   add       TASVLF        Wmptmpt_g
073700090206     c                   endif
073800090205     c* . . . riepilogo
073900090206     C                   add       1             Wmptspt_r
074000090206     C                   add       TASNCL        wmptcpt_r
074100090224     C                   add       w_peso        Wmptkpt_r
074200090206     C                   add       TASVLF        Wmptmpt_r
074300090206     c                   endif
074400090710     c                   endif
074500090710     c* terminal di partenza : tutto compresa la linea stessa
074600090710     c                   z-add     1             k
074700090710     c     tastfp        lookup    fil(k)                                 58
074800090710     c                   if        *in58
074900090710     c     k             occur     dsfil_smp
075000090710     c* . . . giornaliero
075100090710     c                   if        (tasaas*10000+tasmgs)=§al
075200090710     C                   add       1             Wmptsptc_g
075300090710     C                   add       TASNCL        wmptcptc_g
075400090710     C                   add       w_peso        Wmptkptc_g
075500090710     C                   add       TASVLF        Wmptmptc_g
075600090710     c                   endif
075700090710     c* . . . riepilogo
075800090710     C                   add       1             Wmptsptc_r
075900090710     C                   add       TASNCL        wmptcptc_r
076000090710     C                   add       w_peso        Wmptkptc_r
076100090710     C                   add       TASVLF        Wmptmptc_r
076200090710     c                   endif
076300090710     c
076400090710     c* terminal di arrivo : esclusa la linea/terminal
076500090710     c                   if        tastfa<>taslna
076600090206     c                   z-add     1             k
076700090206     c     tastfa        lookup    fil(k)                                 58
076800090206     c                   if        *in58
076900090206     c     k             occur     dsfil_smp
077000090205     c* . . . giornaliero
077100090206     c                   if        (tasaas*10000+tasmgs)=§al
077200090206     C                   add       1             Wmptsat_g
077300090206     C                   add       TASNCL        wmptcat_g
077400090224     C                   add       w_peso        Wmptkat_g
077500090206     C                   add       TASVLF        Wmptmat_g
077600090206     c                   endif
077700090205     c* . . . riepilogo
077800090206     C                   add       1             Wmptsat_r
077900090206     C                   add       TASNCL        wmptcat_r
078000090224     C                   add       w_peso        Wmptkat_r
078100090206     C                   add       TASVLF        Wmptmat_r
078200090205     c                   endif
078300090710     c                   endif
078400090710     c* terminal di arrivo : tutto compreso il terminal stesso
078500090710     c                   z-add     1             k
078600090710     c     tastfa        lookup    fil(k)                                 58
078700090710     c                   if        *in58
078800090710     c     k             occur     dsfil_smp
078900090710     c* . . . giornaliero
079000090710     c                   if        (tasaas*10000+tasmgs)=§al
079100090710     C                   add       1             Wmptsatc_g
079200090710     C                   add       TASNCL        wmptcatc_g
079300090710     C                   add       w_peso        Wmptkatc_g
079400090710     C                   add       TASVLF        Wmptmatc_g
079500090710     c                   endif
079600090710     c* . . . riepilogo
079700090710     C                   add       1             Wmptsatc_r
079800090710     C                   add       TASNCL        wmptcatc_r
079900090710     C                   add       w_peso        Wmptkatc_r
080000090710     C                   add       TASVLF        Wmptmatc_r
080100090710     c                   endif
080200090710     c
080300090204     c* filiali di transito
080400090209     c     kar5          chain     fiar531c
080500090205     c                   if        %found(fiar531c)
080600090205     c                   movel     ar5uni        dar5trs
080700090205     c*
080800090205     c                   eval      k_fil=§AR5FLP
080900090205     c                   exsr      agg_transiti
081000090205      *
081100090205     c                   if        §ar5fl2>0
081200090205     c                   eval      k_fil=§AR5FL2
081300090205     c                   exsr      agg_transiti
081400090205     c                   endif
081500090205      *
081600090205     c                   if        §ar5fl3>0
081700090205     c                   eval      k_fil=§AR5FL3
081800090205     c                   exsr      agg_transiti
081900090205     c                   endif
082000090205      *
082100090205     c                   if        §ar5fl4>0
082200090205     c                   eval      k_fil=§AR5FL4
082300090205     c                   exsr      agg_transiti
082400090205     c                   endif
082500090205      *
082600090205     c                   if        §ar5fl5>0
082700090205     c                   eval      k_fil=§AR5FL5
082800090205     c                   exsr      agg_transiti
082900090205     c                   endif
083000090205     c                   endif
083100090204     c
083200090204     C                   endsr
083300090205     C**********************************************************
083400090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
083500090205     C**********************************************************
083600090205     C     agg_transiti  BEGSR
083700090206     c                   z-add     1             k
083800090206     c     k_fil         lookup    fil(k)                                 58
083900090206     c                   if        *in58
084000090206     c     k             occur     dsfil_smp
084100090205     c* . . . giornaliero
084200090206     c                   if        (tasaas*10000+tasmgs)=§al
084300090206     C                   add       1             Wmptst_g
084400090206     C                   add       TASNCL        wmptct_g
084500090224     C                   add       w_peso        Wmptkt_g
084600090206     C                   add       TASVLF        Wmptmt_g
084700090206     c                   endif
084800090205     c* . . . riepilogo
084900090206     C                   add       1             Wmptst_r
085000090206     C                   add       TASNCL        wmptct_r
085100090224     C                   add       w_peso        Wmptkt_r
085200090206     C                   add       TASVLF        Wmptmt_r
085300090205     c                   endif
085400090205     c                   endsr
085500090204     C**********************************************************
085600090206     C** Riempie campi wfsmp00f
085700090204     C**********************************************************
085800090206     C     rie_campi     BEGSR
085900090204     c* profilo utente e data elaborazione
086000090204     c                   eval      wmpusr=knmus
086100090204     c                   eval      wmpdel=wdatael
086200090206     c*
086300090206     c                   eval      wmpall=§al
086400090204     c* area e descrizione area
086500090204     c                   eval      wmpcar=car(k)
086600090204     C                   MOVEL(P)  '05'          tblcod
086700090204     C                   MOVEL(P)  car(K)        tblkey
086800090204     C     KTAB          CHAIN     TABEL00F                           30
086900090204     C                   IF        %found(tabel00f) and tblflg=*blanks
087000090204     C                   MOVEL     TBLUNI        DS05
087100090204     C                   MOVEL     §05DES        wmpdar
087200090204     C                   endif
087300090204     c* filiale,descrizione e livello filiale
087400090204     c                   eval      wmpdil=dil(K)
087500090206     c                   eval      wmpfil=fil(K)
087600090206     c                   eval      wmplif=lpo(K)
087700090204     c* tipo filiale Italia/Estero
087800090205     c                   eval      wmptif=tif(K)
087900090206     c* spedizioni,colli, kg,volume
088000090206     c                   if        wmptip='G'
088100090206     c                   z-add     wmptsp_G      wmptsp
088200090206     c                   z-add     wmptcp_G      wmptcp
088300090206     c                   z-add(h)  wmptkp_G      wmptkp
088400090206     c                   z-add(h)  wmptmp_G      wmptmp
088500090710
088600090206     c                   z-add     wmptsa_G      wmptsa
088700090206     c                   z-add     wmptca_G      wmptca
088800090206     c                   z-add(h)  wmptka_G      wmptka
088900090206     c                   z-add(h)  wmptma_G      wmptma
089000090710
089100090206     c                   z-add     wmptspt_G     wmptspt
089200090206     c                   z-add     wmptcpt_G     wmptcpt
089300090206     c                   z-add(h)  wmptkpt_G     wmptkpt
089400090206     c                   z-add(h)  wmptmpt_G     wmptmpt
089500090710
089600090710     c                   z-add     wmptsptc_G    wmptsptc
089700090710     c                   z-add     wmptcptc_G    wmptcptc
089800090710     c                   z-add(h)  wmptkptc_G    wmptkptc
089900090710     c                   z-add(h)  wmptmptc_G    wmptmptc
090000090710
090100090206     c                   z-add     wmptsat_G     wmptsat
090200090206     c                   z-add     wmptcat_G     wmptcat
090300090206     c                   z-add(h)  wmptkat_G     wmptkat
090400090206     c                   z-add(h)  wmptmat_G     wmptmat
090500090710
090600090710     c                   z-add     wmptsatc_G    wmptsatc
090700090710     c                   z-add     wmptcatc_G    wmptcatc
090800090710     c                   z-add(h)  wmptkatc_G    wmptkatc
090900090710     c                   z-add(h)  wmptmatc_G    wmptmatc
091000090710
091100090206     c                   z-add     wmptst_G      wmptst
091200090206     c                   z-add     wmptct_G      wmptct
091300090206     c                   z-add(h)  wmptkt_G      wmptkt
091400090206     c                   z-add(h)  wmptmt_G      wmptmt
091500090206     c                   else
091600090206     c                   z-add     wmptsp_r      wmptsp
091700090206     c                   z-add     wmptcp_r      wmptcp
091800090206     c                   z-add(h)  wmptkp_r      wmptkp
091900090206     c                   z-add(h)  wmptmp_r      wmptmp
092000090710
092100090206     c                   z-add     wmptsa_r      wmptsa
092200090206     c                   z-add     wmptca_r      wmptca
092300090206     c                   z-add(h)  wmptka_r      wmptka
092400090206     c                   z-add(h)  wmptma_r      wmptma
092500090710
092600090206     c                   z-add     wmptspt_r     wmptspt
092700090206     c                   z-add     wmptcpt_r     wmptcpt
092800090206     c                   z-add(h)  wmptkpt_r     wmptkpt
092900090206     c                   z-add(h)  wmptmpt_r     wmptmpt
093000090710
093100090710     c                   z-add     wmptsptc_r    wmptsptc
093200090710     c                   z-add     wmptcptc_r    wmptcptc
093300090710     c                   z-add(h)  wmptkptc_r    wmptkptc
093400090710     c                   z-add(h)  wmptmptc_r    wmptmptc
093500090710
093600090206     c                   z-add     wmptsat_r     wmptsat
093700090206     c                   z-add     wmptcat_r     wmptcat
093800090206     c                   z-add(h)  wmptkat_r     wmptkat
093900090206     c                   z-add(h)  wmptmat_r     wmptmat
094000090710
094100090710     c                   z-add     wmptsatc_r    wmptsatc
094200090710     c                   z-add     wmptcatc_r    wmptcatc
094300090710     c                   z-add(h)  wmptkatc_r    wmptkatc
094400090710     c                   z-add(h)  wmptmatc_r    wmptmatc
094500090710
094600090206     c                   z-add     wmptst_r      wmptst
094700090206     c                   z-add     wmptct_r      wmptct
094800090206     c                   z-add(h)  wmptkt_r      wmptkt
094900090206     c                   z-add(h)  wmptmt_r      wmptmt
095000090710
095100090206     c                   endif
095200090204     c                   endsr
095300090204     C**********************************************************
095400090204     C** Elaborazione per file wslss50f
095500090204     C**********************************************************
095600090204     C     sr_wslss      BEGSR
095700090204     C                   SETOFF                                       545556
095800090204     C                   SETOFF                                       48  52
095900090204     C     TASLNP        COMP      300                                54        ESTERO
096000090204     C  N54TASLNP        COMP      190                                    54    + DPD
096100090204     C  N54TASLNP        COMP      191                                    54    + DPD
096200090204     C  N54TASLNP        COMP      192                                    54    + DPD
096300090204     C  N54TASLNP        COMP      193                                    54    + DPD
096400090204     C  N54TASLNP        COMP      194                                    54    + DPD
096500090204     C  N54TASLNP        COMP      195                                    54    + DPD
096600090204     C  N54TASLNP        COMP      196                                    54    + DPD
096700090204     C  N54TASLNP        COMP      197                                    54    + DPD
096800090204     C*
096900100402     C****     TASLNP        COMP      180                                    48     POSTE
097000090204     C*
097100090204     C     TASLNA        COMP      300                                55
097200090204     C  N55TASLNA        COMP      190                                    55    + DPD
097300090204     C  N55TASLNA        COMP      191                                    55    + DPD
097400090204     C  N55TASLNA        COMP      192                                    55    + DPD
097500090204     C  N55TASLNA        COMP      193                                    55    + DPD
097600090204     C  N55TASLNA        COMP      194                                    55    + DPD
097700090204     C  N55TASLNA        COMP      195                                    55    + DPD
097800090204     C  N55TASLNA        COMP      196                                    55    + DPD
097900090204     C  N55TASLNA        COMP      197                                    55    + DPD
098000090204     C*
098100090204     C     TASLNA        COMP      400                                    52     FEDEX
098200090204     C*
098300090204     C     TASLNA        COMP      190                                    56     DPD
098400090204     C  N56TASLNA        COMP      191                                    56     DPD
098500090204     C  N56TASLNA        COMP      192                                    56     DPD
098600090204     C  N56TASLNA        COMP      193                                    56     DPD
098700090204     C  N56TASLNA        COMP      194                                    56     DPD
098800090204     C  N56TASLNA        COMP      195                                    56     DPD
098900090204     C  N56TASLNA        COMP      196                                    56     DPD
099000090204     C  N56TASLNA        COMP      197                                    56     DPD
099100091006     c*
099200091006     c* Verifico se si tratta di cliente extra budget --> 35 on
099300110711     c                   if        §stat=' '
099400091006     c                   z-add     1             EE
099500091006     c                   if        tasccm<>savccm or tasctr<>savctr
099600091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
099700091007     c     w010a         lookup    ksc_EX(EE)                             35
099800091007     c                   if        not *in35
099900091007     c                   eval      %subst(w010a:8:3)='   '
100000091007     c     w010a         lookup    ksc_EX(EE)                             35
100100091006     c                   endif
100200091006
100300091006     c                   eval      savccm=tasccm
100400091006     c                   eval      savctr=tasctr
100500091006     c                   endif
100600110711     c
100700110711     c                   else
100800110711     c*
100900110711     c                   z-add     1             EE
101000110711     c                   if        tasccm<>savccm
101100110711     c                   eval      w010a=%editc(tasccm:'X')+'   '
101200110711     c     w010a         lookup    ksc_EX(EE)                             35
101300110711     c                   eval      savccm=tasccm
101400110711     c                   endif
101500110711     c
101600110711     c                   endif
101700090204     C*
101800090204     C                   Z-ADD     1             K
101900090204     C     TASLNP        LOOKUP    FIL(K)                                 58
102000090204     C  N58              EXCEPT    ERR
102100090204     C     K             OCCUR     DSFIL
102200100402     C     K             OCCUR     DSFIL_e
102300100402     C     K             OCCUR     DSFIL_h
102400090204     C  N54
102500090204     CANN55              DO                                                     ITALIA
102600100402     c* Spedizioni LNP Italia LNA Italia
102700100402     c                   exsr      speITA
102800090204     C                   ENDDO
102900100402     c
103000100402     c* Spedizioni IMPORT --> LNP Italia
103100090204     C   54              DO                                                     IMPORT
103200100402     c                   exsr      speIMPORT
103300090204     C                   ENDDO
103400100402     c
103500100402     c* Spedizioni lna EUROEXPRESS
103600090204     C   55
103700090204     CANN56
103800090204     CANN52              DO                                                     EXPORT
103900100402     c                   exsr      speEXPRESS
104000090204     C                   ENDDO
104100100402     c
104200100402     c* Spedizioni lna DPD
104300090204     C   55
104400090204     CAN 56              DO                                                     EXPORT
104500100402     c                   exsr      speDPD
104600090204     C                   ENDDO
104700100402     c
104800100402     c* Spedizioni lna FEDEX
104900090204     C   55
105000090204     CAN 52              DO                                                     EXPORT
105100100402     c                   exsr      speFEDEX
105200090204     C                   ENDDO
105300100402     c
105400090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
105500090204     C                   Z-ADD     1             K
105600090204     C     TASLNA        LOOKUP    FIL(K)                                 58
105700090204     C  N58              EXCEPT    ERR
105800090204     C     K             OCCUR     DSFIL
105900100402     C     K             OCCUR     DSFIL_e
106000100402     C     K             OCCUR     DSFIL_h
106100100402     c
106200090204     C  N54
106300100402     CANN55              DO
106400100402     c* Spedizioni LNP Italia LNA Italia
106500100402     c                   exsr      speITA_LNA
106600090204     C                   ENDDO
106700100402     c
106800100402     c* spedizioni EXPORT --> lna estera
106900090204     C   55              DO                                                     ESTERO ASS.+FRA.
107000100402     c                   exsr      speEXPORT
107100090204     C                   ENDDO
107200090204     c                   endsr
107300090206     C**********************************************************
107400090206     C** scarica ds multiple su file
107500090206     C**********************************************************
107600090206     C     scrive_file   BEGSR
107700090206     c*
107800090206     C                   Z-ADD     §AL           WSSALL
107900090206     c*
108000110711    1C                   DO        400           K
108100090206     C     K             OCCUR     DSFIL_smp
108200090206     C     K             OCCUR     DSFIL
108300100402     C     K             OCCUR     DSFIL_e
108400100402     C     K             OCCUR     DSFIL_h
108500110711     c
108600110711    2c                   if        §stat=' '
108700090206     c* WFSMP
108800090206     c* giornaliero
108900090601     c* Lo scrivo solo per tipo "G" o "R"
109000090601     c                   if        Wtipo='G' or Wtipo='R'
109100090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
109200090206     c                              wmptspt_g>0 or wmptsat_g>0 or
109300090206     c                              wmptst_g>0)
109400090206     c                   z-add     §al           wmpdal
109500090206     c                   eval      wmptip='G'
109600090206     c                   exsr      rie_campi
109700090206     c                   write     wfsmp000
109800090206     c                   endif
109900090601     c                   endif
110000090601     c
110100090601     c* riepilogo /precedente
110200090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
110300090601     c                   if        Wtipo='P' or Wtipo='R'
110400090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
110500090206     c                              wmptspt_r>0 or wmptsat_r>0 or
110600090206     c                              wmptst_r>0)
110700090206     c                   z-add     §dal          wmpdal
110800090601     c                   if        Wtipo='R'
110900090206     c                   eval      wmptip='R'
111000090601     c                   else
111100090601     c                   eval      wmptip='P'
111200090601     c                   endif
111300090206     c                   exsr      rie_campi
111400090206     c                   write     wfsmp000
111500090206     c                   endif
111600090601     c                   endif
111700110711     c
111800110711    2c                   endif
111900090206     c* WSLSS50F
112000090206     c* giornaliero
112100090601     c                   if        Wtipo='G' or Wtipo='R'
112200110711     c
112300110711    2c                   if        §stat=' '
112400090601     c
112500090206     C     WTSPIP        IFGT      *ZERO
112600090206     C                   Z-ADD     §al           WSSDAL
112700090206     c                   movel     'G'           WSSTIE
112800090206     C                   MOVE      'IP'          WSSTIP
112900090206     C                   Z-ADD     FIL(K)        WSSLIN
113000090206     C                   Z-ADD     WTSPIP        WSSTSP
113100090206     C                   Z-ADD     WTCLIP        WSSTCL
113200090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
113300090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
113400100402     c* spedizioni "E"
113500100402     C                   Z-ADD     eWTSPIP       WSSTSPe
113600100402     C                   Z-ADD     eWTCLIP       WSSTCLe
113700100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
113800100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
113900100402     c* spedizioni "H"
114000100402     C                   Z-ADD     hWTSPIP       WSSTSPh
114100100402     C                   Z-ADD     hWTCLIP       WSSTCLh
114200100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
114300100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
114400100402
114500090206     C                   WRITE     WSLSS500
114600090206     C                   ENDIF
114700100402
114800090206     C     WTSPIA        IFGT      *ZERO
114900090206     C                   Z-ADD     §al           WSSDAL
115000090206     c                   movel     'G'           WSSTIE
115100090206     C                   MOVE      'IA'          WSSTIP
115200090206     C                   Z-ADD     FIL(K)        WSSLIN
115300090206     C                   Z-ADD     WTSPIA        WSSTSP
115400090206     C                   Z-ADD     WTCLIA        WSSTCL
115500090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
115600090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
115700100402     c* spedizioni "E"
115800100402     C                   Z-ADD     eWTSPIA       WSSTSPe
115900100402     C                   Z-ADD     eWTCLIA       WSSTCLe
116000100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
116100100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
116200100402     c* spedizioni "H"
116300100402     C                   Z-ADD     hWTSPIA       WSSTSPh
116400100402     C                   Z-ADD     hWTCLIA       WSSTCLh
116500100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
116600100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
116700100402     c
116800090206     C                   WRITE     WSLSS500
116900090206     C                   ENDIF
117000100402     c
117100090206     C     WTSPEI        IFGT      *ZERO
117200090206     C                   Z-ADD     §al           WSSDAL
117300090206     c                   movel     'G'           WSSTIE
117400090206     C                   MOVE      'EI'          WSSTIP
117500090206     C                   Z-ADD     FIL(K)        WSSLIN
117600090206     C                   Z-ADD     WTSPEI        WSSTSP
117700090206     C                   Z-ADD     WTCLEI        WSSTCL
117800090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
117900090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
118000100402     c* spedizioni "E"
118100100402     C                   Z-ADD     eWTSPEI       WSSTSPe
118200100402     C                   Z-ADD     eWTCLEI       WSSTCLe
118300100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
118400100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
118500100402     c* spedizioni "H"
118600100402     C                   Z-ADD     hWTSPEI       WSSTSPh
118700100402     C                   Z-ADD     hWTCLEI       WSSTCLh
118800100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
118900100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
119000100402     c
119100090206     C                   WRITE     WSLSS500
119200090206     C                   ENDIF
119300100402     c
119400090206     C     WTSPEE        IFGT      *ZERO
119500090206     C                   Z-ADD     §al           WSSDAL
119600090206     c                   movel     'G'           WSSTIE
119700090206     C                   MOVE      'EE'          WSSTIP
119800090206     C                   Z-ADD     FIL(K)        WSSLIN
119900090206     C                   Z-ADD     WTSPEE        WSSTSP
120000090206     C                   Z-ADD     WTCLEE        WSSTCL
120100090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
120200090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
120300100402     c* spedizioni "E"
120400100402     C                   Z-ADD     eWTSPEE       WSSTSPe
120500100402     C                   Z-ADD     eWTCLEE       WSSTCLe
120600100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
120700100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
120800100402     c* spedizioni "H"
120900100402     C                   Z-ADD     hWTSPEE       WSSTSPh
121000100402     C                   Z-ADD     hWTCLEE       WSSTCLh
121100100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
121200100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
121300100402     c
121400090206     C                   WRITE     WSLSS500
121500090206     C                   ENDIF
121600090206     C*
121700090206     C     WTSPED        IFGT      *ZERO
121800090206     C                   Z-ADD     §al           WSSDAL
121900090206     c                   movel     'G'           WSSTIE
122000090206     C                   MOVE      'ED'          WSSTIP
122100090206     C                   Z-ADD     FIL(K)        WSSLIN
122200090206     C                   Z-ADD     WTSPED        WSSTSP
122300090206     C                   Z-ADD     WTCLED        WSSTCL
122400090206     C                   Z-ADD(H)  WTKGED        WSSTKG
122500090206     C                   Z-ADD(H)  WTMCED        WSSTMC
122600100402     c* spedizioni "E"
122700100402     C                   Z-ADD     eWTSPED       WSSTSPe
122800100402     C                   Z-ADD     eWTCLED       WSSTCLe
122900100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
123000100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
123100100402     c* spedizioni "H"
123200100402     C                   Z-ADD     hWTSPED       WSSTSPh
123300100402     C                   Z-ADD     hWTCLED       WSSTCLh
123400100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
123500100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
123600100402     c
123700090206     C                   WRITE     WSLSS500
123800090206     C                   ENDIF
123900090206     C*
124000090206     C     WTSPEX        IFGT      *ZERO
124100090206     C                   Z-ADD     §al           WSSDAL
124200090206     c                   movel     'G'           WSSTIE
124300090206     C                   MOVE      'EX'          WSSTIP
124400090206     C                   Z-ADD     FIL(K)        WSSLIN
124500090206     C                   Z-ADD     WTSPEX        WSSTSP
124600090206     C                   Z-ADD     WTCLEX        WSSTCL
124700090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
124800090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
124900100402     c* spedizioni "E"
125000100402     C                   Z-ADD     eWTSPEX       WSSTSPe
125100100402     C                   Z-ADD     eWTCLEX       WSSTCLe
125200100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
125300100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
125400100402     c* spedizioni "H"
125500100402     C                   Z-ADD     hWTSPEX       WSSTSPh
125600100402     C                   Z-ADD     hWTCLEX       WSSTCLh
125700100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
125800100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
125900100402     c
126000090206     C                   WRITE     WSLSS500
126100090206     C                   ENDIF
126200090206     C*
126300090206     C     WTSPEA        IFGT      *ZERO
126400090206     C                   Z-ADD     §al           WSSDAL
126500090206     c                   movel     'G'           WSSTIE
126600090206     C                   MOVE      'EA'          WSSTIP
126700090206     C                   Z-ADD     FIL(K)        WSSLIN
126800090206     C                   Z-ADD     WTSPEA        WSSTSP
126900090206     C                   Z-ADD     WTCLEA        WSSTCL
127000090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
127100090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
127200100402     c* spedizioni "E"
127300100402     C                   Z-ADD     eWTSPEA       WSSTSPe
127400100402     C                   Z-ADD     eWTCLEA       WSSTCLe
127500100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
127600100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
127700100402     c* spedizioni "H"
127800100402     C                   Z-ADD     hWTSPEA       WSSTSPh
127900100402     C                   Z-ADD     hWTCLEA       WSSTCLh
128000100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
128100100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
128200100402     c
128300090206     C                   WRITE     WSLSS500
128400090206     C                   ENDIF
128500090206     C*
128600090206     C     WTSPPA        IFGT      *ZERO
128700090206     C                   Z-ADD     §al           WSSDAL
128800090206     c                   movel     'G'           WSSTIE
128900090206     C                   MOVE      'PA'          WSSTIP
129000090206     C                   Z-ADD     FIL(K)        WSSLIN
129100090206     C                   Z-ADD     WTSPPA        WSSTSP
129200090206     C                   Z-ADD     WTCLPA        WSSTCL
129300090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
129400090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
129500100402     c* spedizioni "E"
129600100402     C                   Z-ADD     eWTSPPA       WSSTSPe
129700100402     C                   Z-ADD     eWTCLPA       WSSTCLe
129800100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
129900100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
130000100402     c* spedizioni "H"
130100100402     C                   Z-ADD     hWTSPPA       WSSTSPh
130200100402     C                   Z-ADD     hWTCLPA       WSSTCLh
130300100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
130400100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
130500100402     c
130600090206     C                   WRITE     WSLSS500
130700090206     C                   ENDIF
130800110711     c
130900110711     c                   endif
131000110715     c* Extra budget o clienti particolari
131100110715     c                   if        §stat=' '
131200110715     c                   movel     'G'           WSSTIE
131300110715     c                   else
131400110715     c                   movel     'A'           WSSTIE
131500110715     c                   endif
131600110715     c
131700091006     C     WTSPIP_e      IFGT      *ZERO
131800091007     C                   Z-ADD     §al           WSSDAL
131900091006     C                   MOVE      'IP'          WSSTIP
132000091006     C                   Z-ADD     FIL(K)        WSSLIN
132100091006     C                   Z-ADD     WTSPIP_e      WSSTSP
132200091006     C                   Z-ADD     WTCLIP_e      WSSTCL
132300091006     C                   Z-ADD(H)  WTKGIP_e      WSSTKG
132400091006     C                   Z-ADD(H)  WTMCIP_e      WSSTMC
132500100402     c* spedizioni "E"
132600100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
132700100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
132800100402     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe
132900100402     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe
133000100402     c* spedizioni "H"
133100100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
133200100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
133300100402     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh
133400100402     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh
133500100402     c
133600091006     C                   WRITE     WSLSS550
133700091006     C                   ENDIF
133800100402     c
133900091006     C     WTSPIA_e      IFGT      *ZERO
134000091007     C                   Z-ADD     §al           WSSDAL
134100110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
134200110715     c*****              movel     'G'           WSSTIE
134300091006     C                   MOVE      'IA'          WSSTIP
134400091006     C                   Z-ADD     FIL(K)        WSSLIN
134500091006     C                   Z-ADD     WTSPIA_e      WSSTSP
134600091006     C                   Z-ADD     WTCLIA_e      WSSTCL
134700091006     C                   Z-ADD(H)  WTKGIA_e      WSSTKG
134800091006     C                   Z-ADD(H)  WTMCIA_e      WSSTMC
134900100402     c* spedizioni "E"
135000100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
135100100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
135200100402     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe
135300100402     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe
135400100402     c* spedizioni "H"
135500100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
135600100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
135700100402     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh
135800100402     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh
135900100402     c
136000091006     C                   WRITE     WSLSS550
136100091006     C                   ENDIF
136200100402     c
136300091006     C     WTSPEI_e      IFGT      *ZERO
136400091007     C                   Z-ADD     §al           WSSDAL
136500110715     c******             movel     'G'           WSSTIE
136600091006     C                   MOVE      'EI'          WSSTIP
136700091006     C                   Z-ADD     FIL(K)        WSSLIN
136800091006     C                   Z-ADD     WTSPEI_e      WSSTSP
136900091006     C                   Z-ADD     WTCLEI_e      WSSTCL
137000091006     C                   Z-ADD(H)  WTKGEI_e      WSSTKG
137100091006     C                   Z-ADD(H)  WTMCEI_e      WSSTMC
137200100402     c* spedizioni "E"
137300100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
137400100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
137500100402     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe
137600100402     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe
137700100402     c* spedizioni "H"
137800100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
137900100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
138000100402     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh
138100100402     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh
138200100402     c
138300091006     C                   WRITE     WSLSS550
138400091006     C                   ENDIF
138500100402     c
138600091006     C     WTSPEE_e      IFGT      *ZERO
138700091007     C                   Z-ADD     §al           WSSDAL
138800110715     c******             movel     'G'           WSSTIE
138900091006     C                   MOVE      'EE'          WSSTIP
139000091006     C                   Z-ADD     FIL(K)        WSSLIN
139100091006     C                   Z-ADD     WTSPEE_e      WSSTSP
139200091006     C                   Z-ADD     WTCLEE_e      WSSTCL
139300091006     C                   Z-ADD(H)  WTKGEE_e      WSSTKG
139400091006     C                   Z-ADD(H)  WTMCEE_e      WSSTMC
139500100402     c* spedizioni "E"
139600100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
139700100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
139800100402     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe
139900100402     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe
140000100402     c* spedizioni "H"
140100100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
140200100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
140300100402     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh
140400100402     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh
140500100402     c
140600091006     C                   WRITE     WSLSS550
140700091006     C                   ENDIF
140800091006     C*
140900091006     C     WTSPED_e      IFGT      *ZERO
141000091007     C                   Z-ADD     §al           WSSDAL
141100110715     c*******            movel     'G'           WSSTIE
141200091006     C                   MOVE      'ED'          WSSTIP
141300091006     C                   Z-ADD     FIL(K)        WSSLIN
141400091006     C                   Z-ADD     WTSPED_e      WSSTSP
141500091006     C                   Z-ADD     WTCLED_e      WSSTCL
141600091006     C                   Z-ADD(H)  WTKGED_e      WSSTKG
141700091006     C                   Z-ADD(H)  WTMCED_e      WSSTMC
141800100402     c* spedizioni "E"
141900100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
142000100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
142100100402     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe
142200100402     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe
142300100402     c* spedizioni "H"
142400100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
142500100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
142600100402     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh
142700100402     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh
142800100402     c
142900091006     C                   WRITE     WSLSS550
143000091006     C                   ENDIF
143100091006     C*
143200091006     C     WTSPEX_e      IFGT      *ZERO
143300091007     C                   Z-ADD     §al           WSSDAL
143400110715     c*******            movel     'G'           WSSTIE
143500091006     C                   MOVE      'EX'          WSSTIP
143600091006     C                   Z-ADD     FIL(K)        WSSLIN
143700091006     C                   Z-ADD     WTSPEX_e      WSSTSP
143800091006     C                   Z-ADD     WTCLEX_e      WSSTCL
143900091006     C                   Z-ADD(H)  WTKGEX_e      WSSTKG
144000091006     C                   Z-ADD(H)  WTMCEX_e      WSSTMC
144100100402     c* spedizioni "E"
144200100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
144300100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
144400100402     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe
144500100402     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe
144600100402     c* spedizioni "H"
144700100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
144800100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
144900100402     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh
145000100402     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh
145100100402     c
145200091006     C                   WRITE     WSLSS550
145300091006     C                   ENDIF
145400091006     C*
145500091006     C     WTSPEA_e      IFGT      *ZERO
145600091007     C                   Z-ADD     §al           WSSDAL
145700110715     c******             movel     'G'           WSSTIE
145800091006     C                   MOVE      'EA'          WSSTIP
145900091006     C                   Z-ADD     FIL(K)        WSSLIN
146000091006     C                   Z-ADD     WTSPEA_e      WSSTSP
146100091006     C                   Z-ADD     WTCLEA_e      WSSTCL
146200091006     C                   Z-ADD(H)  WTKGEA_e      WSSTKG
146300091006     C                   Z-ADD(H)  WTMCEA_e      WSSTMC
146400100402     c* spedizioni "E"
146500100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
146600100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
146700100402     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe
146800100402     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe
146900100402     c* spedizioni "H"
147000100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
147100100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
147200100402     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh
147300100402     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh
147400100402     c
147500091006     C                   WRITE     WSLSS550
147600091006     C                   ENDIF
147700091006     C*
147800091006     C     WTSPPA_e      IFGT      *ZERO
147900091007     C                   Z-ADD     §al           WSSDAL
148000110715     c*******            movel     'G'           WSSTIE
148100091006     C                   MOVE      'PA'          WSSTIP
148200091006     C                   Z-ADD     FIL(K)        WSSLIN
148300091006     C                   Z-ADD     WTSPPA_e      WSSTSP
148400091006     C                   Z-ADD     WTCLPA_e      WSSTCL
148500091006     C                   Z-ADD(H)  WTKGPA_e      WSSTKG
148600091006     C                   Z-ADD(H)  WTMCPA_e      WSSTMC
148700100402     c* spedizioni "E"
148800100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
148900100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
149000100402     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe
149100100402     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe
149200100402     c* spedizioni "H"
149300100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
149400100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
149500100402     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh
149600100402     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh
149700100402     c
149800091006     C                   WRITE     WSLSS550
149900091006     C                   ENDIF
150000091006
150100090601     C                   ENDIF
150200090206     c*
150300090206     c* riepilogo
150400090601     c                   if        Wtipo='P' or Wtipo='R'
150500110715     c                   if        §stat=' '
150600110715
150700090206     C     WTSPIP_r      IFGT      *ZERO
150800090206     C                   Z-ADD     §dal          WSSDAL
150900090601     c**                 movel     'R'           WSSTIE
151000090601     c                   movel     Wtipo         WSSTIE
151100090206     C                   MOVE      'IP'          WSSTIP
151200090206     C                   Z-ADD     FIL(K)        WSSLIN
151300090206     C                   Z-ADD     WTSPIP_r      WSSTSP
151400090206     C                   Z-ADD     WTCLIP_r      WSSTCL
151500090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
151600090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
151700100402     c* spedizioni "E"
151800100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
151900100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
152000100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
152100100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
152200100402     c* spedizioni "H"
152300100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
152400100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
152500100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
152600100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
152700100402     c
152800090206     C                   WRITE     WSLSS500
152900090206     C                   ENDIF
153000100402     c
153100090206     C     WTSPIA_r      IFGT      *ZERO
153200090206     C                   Z-ADD     §dal          WSSDAL
153300090601     c**                 movel     'R'           WSSTIE
153400090601     c                   movel     Wtipo         WSSTIE
153500090206     C                   MOVE      'IA'          WSSTIP
153600090206     C                   Z-ADD     FIL(K)        WSSLIN
153700090206     C                   Z-ADD     WTSPIA_r      WSSTSP
153800090206     C                   Z-ADD     WTCLIA_r      WSSTCL
153900090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
154000090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
154100100402     c* spedizioni "E"
154200100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
154300100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
154400100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
154500100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
154600100402     c* spedizioni "H"
154700100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
154800100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
154900100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
155000100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
155100100402     c
155200090206     C                   WRITE     WSLSS500
155300090206     C                   ENDIF
155400100402
155500090206     C     WTSPEI_R      IFGT      *ZERO
155600090206     C                   Z-ADD     §dal          WSSDAL
155700090601     c**                 movel     'R'           WSSTIE
155800090601     c                   movel     Wtipo         WSSTIE
155900090206     C                   MOVE      'EI'          WSSTIP
156000090206     C                   Z-ADD     FIL(K)        WSSLIN
156100090206     C                   Z-ADD     WTSPEI_r      WSSTSP
156200090206     C                   Z-ADD     WTCLEI_r      WSSTCL
156300090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
156400090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
156500100402     c* Spedizioni "E"
156600100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
156700100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
156800100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
156900100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
157000100402     c* Spedizioni "H"
157100100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
157200100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
157300100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
157400100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
157500100402     c
157600090206     C                   WRITE     WSLSS500
157700090206     C                   ENDIF
157800100402     c
157900090206     C     WTSPEE_r      IFGT      *ZERO
158000090206     C                   Z-ADD     §dal          WSSDAL
158100090601     c**                 movel     'R'           WSSTIE
158200090601     c                   movel     Wtipo         WSSTIE
158300090206     C                   MOVE      'EE'          WSSTIP
158400090206     C                   Z-ADD     FIL(K)        WSSLIN
158500090206     C                   Z-ADD     WTSPEE_r      WSSTSP
158600090206     C                   Z-ADD     WTCLEE_r      WSSTCL
158700090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
158800090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
158900100402     c* Spedizioni "E"
159000100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
159100100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
159200100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
159300100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
159400100402     c* Spedizioni "H"
159500100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
159600100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
159700100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
159800100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
159900100402     c
160000090206     C                   WRITE     WSLSS500
160100090206     C                   ENDIF
160200090206     C*
160300090206     C     WTSPED_r      IFGT      *ZERO
160400090206     C                   Z-ADD     §dal          WSSDAL
160500090601     c**                 movel     'R'           WSSTIE
160600090601     c                   movel     Wtipo         WSSTIE
160700090206     C                   MOVE      'ED'          WSSTIP
160800090206     C                   Z-ADD     FIL(K)        WSSLIN
160900090206     C                   Z-ADD     WTSPED_r      WSSTSP
161000090206     C                   Z-ADD     WTCLED_r      WSSTCL
161100090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
161200090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
161300100402     c* Spedizioni "E"
161400100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
161500100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
161600100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
161700100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
161800100402     c* Spedizioni "H"
161900100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
162000100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
162100100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
162200100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
162300100402     c
162400090206     C                   WRITE     WSLSS500
162500090206     C                   ENDIF
162600090206     C*
162700090206     C     WTSPEX_r      IFGT      *ZERO
162800090206     C                   Z-ADD     §dal          WSSDAL
162900090601     c**                 movel     'R'           WSSTIE
163000090601     c                   movel     Wtipo         WSSTIE
163100090206     C                   MOVE      'EX'          WSSTIP
163200090206     C                   Z-ADD     FIL(K)        WSSLIN
163300090206     C                   Z-ADD     WTSPEX_r      WSSTSP
163400090206     C                   Z-ADD     WTCLEX_r      WSSTCL
163500090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
163600090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
163700100402     c* Spedizioni "E"
163800100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
163900100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
164000100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
164100100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
164200100402     c* Spedizioni "H"
164300100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
164400100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
164500100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
164600100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
164700100402     c
164800090206     C                   WRITE     WSLSS500
164900090206     C                   ENDIF
165000090206     C*
165100090206     C     WTSPEA_r      IFGT      *ZERO
165200090206     C                   Z-ADD     §dal          WSSDAL
165300090601     c**                 movel     'R'           WSSTIE
165400090601     c                   movel     Wtipo         WSSTIE
165500090206     C                   MOVE      'EA'          WSSTIP
165600090206     C                   Z-ADD     FIL(K)        WSSLIN
165700090206     C                   Z-ADD     WTSPEA_r      WSSTSP
165800090206     C                   Z-ADD     WTCLEA_r      WSSTCL
165900090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
166000090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
166100100402     c* Spedizioni "E"
166200100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
166300100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
166400100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
166500100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
166600100402     c* Spedizioni "H"
166700100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
166800100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
166900100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
167000100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
167100100402     c
167200090206     C                   WRITE     WSLSS500
167300090206     C                   ENDIF
167400090206     C*
167500090206     C     WTSPPA_r      IFGT      *ZERO
167600090206     C                   Z-ADD     §dal          WSSDAL
167700090601     c**                 movel     'R'           WSSTIE
167800090601     c                   movel     Wtipo         WSSTIE
167900090206     C                   MOVE      'PA'          WSSTIP
168000090206     C                   Z-ADD     FIL(K)        WSSLIN
168100090206     C                   Z-ADD     WTSPPA_r      WSSTSP
168200090206     C                   Z-ADD     WTCLPA_r      WSSTCL
168300090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
168400090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
168500100402     c* Spedizioni "E"
168600100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
168700100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
168800100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
168900100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
169000100402     c* Spedizioni "H"
169100100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
169200100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
169300100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
169400100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
169500100402     c
169600090206     C                   WRITE     WSLSS500
169700090206     C                   ENDIF
169800110715     c
169900110715     c                   endif
170000110715     c
170100110715     c* Extra budget e clienti particolari
170200110715     c                   movel     wtipo         tipoC             1
170300110715     c                   if        §stat<>' ' and wtipo='R'
170400110715     c                   eval      tipoC='B'
170500110715     c                   endif
170600110715     c                   if        §stat<>' ' and wtipo='P'
170700110715     c                   eval      tipoC='C'
170800110715     c                   endif
170900110715     c
171000091006     C     WTSPIP_re     IFGT      *ZERO
171100091006     C                   Z-ADD     §dal          WSSDAL
171200110715     c                   movel     TipoC         WSSTIE
171300091006     C                   MOVE      'IP'          WSSTIP
171400091006     C                   Z-ADD     FIL(K)        WSSLIN
171500091006     C                   Z-ADD     WTSPIP_re     WSSTSP
171600091006     C                   Z-ADD     WTCLIP_re     WSSTCL
171700091006     C                   Z-ADD(H)  WTKGIP_re     WSSTKG
171800091006     C                   Z-ADD(H)  WTMCIP_re     WSSTMC
171900100402     c* Spedizioni "E"
172000100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
172100100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
172200100402     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe
172300100402     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe
172400100402     c* Spedizioni "H"
172500100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
172600100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
172700100402     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh
172800100402     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh
172900100402     c
173000091006     C                   WRITE     WSLSS550
173100091006     C                   ENDIF
173200100402     c
173300091006     C     WTSPIA_re     IFGT      *ZERO
173400091006     C                   Z-ADD     §dal          WSSDAL
173500110715     c                   movel     tipoC         WSSTIE
173600091006     C                   MOVE      'IA'          WSSTIP
173700091006     C                   Z-ADD     FIL(K)        WSSLIN
173800091006     C                   Z-ADD     WTSPIA_re     WSSTSP
173900091006     C                   Z-ADD     WTCLIA_re     WSSTCL
174000091006     C                   Z-ADD(H)  WTKGIA_re     WSSTKG
174100091006     C                   Z-ADD(H)  WTMCIA_re     WSSTMC
174200100402     c* Spedizioni "E"
174300100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
174400100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
174500100402     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe
174600100402     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe
174700100402     c* Spedizioni "H"
174800100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
174900100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
175000100402     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh
175100100402     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh
175200100402     c
175300091006     C                   WRITE     WSLSS550
175400091006     C                   ENDIF
175500100402     c
175600091006     C     WTSPEI_Re     IFGT      *ZERO
175700091006     C                   Z-ADD     §dal          WSSDAL
175800110715     c                   movel     tipoC         WSSTIE
175900091006     C                   MOVE      'EI'          WSSTIP
176000091006     C                   Z-ADD     FIL(K)        WSSLIN
176100091006     C                   Z-ADD     WTSPEI_re     WSSTSP
176200091006     C                   Z-ADD     WTCLEI_re     WSSTCL
176300091006     C                   Z-ADD(H)  WTKGEI_re     WSSTKG
176400091006     C                   Z-ADD(H)  WTMCEI_re     WSSTMC
176500100402     c* Spedizioni "E"
176600100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
176700100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
176800100402     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe
176900100402     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe
177000100402     c* Spedizioni "H"
177100100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
177200100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
177300100402     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh
177400100402     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh
177500100402     c
177600091006     C                   WRITE     WSLSS550
177700091006     C                   ENDIF
177800100402     c
177900091006     C     WTSPEE_re     IFGT      *ZERO
178000091006     C                   Z-ADD     §dal          WSSDAL
178100110715     c                   movel     tipoC         WSSTIE
178200091006     C                   MOVE      'EE'          WSSTIP
178300091006     C                   Z-ADD     FIL(K)        WSSLIN
178400091006     C                   Z-ADD     WTSPEE_re     WSSTSP
178500091006     C                   Z-ADD     WTCLEE_re     WSSTCL
178600091006     C                   Z-ADD(H)  WTKGEE_re     WSSTKG
178700091006     C                   Z-ADD(H)  WTMCEE_re     WSSTMC
178800100402     c* Spedizioni "E"
178900100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
179000100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
179100100402     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe
179200100402     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe
179300100402     c* Spedizioni "h"
179400100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
179500100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
179600100402     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh
179700100402     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh
179800100402     c
179900091006     C                   WRITE     WSLSS550
180000091006     C                   ENDIF
180100091006     C*
180200091006     C     WTSPED_re     IFGT      *ZERO
180300091006     C                   Z-ADD     §dal          WSSDAL
180400110715     c                   movel     tipoC         WSSTIE
180500091006     C                   MOVE      'ED'          WSSTIP
180600091006     C                   Z-ADD     FIL(K)        WSSLIN
180700091006     C                   Z-ADD     WTSPED_re     WSSTSP
180800091006     C                   Z-ADD     WTCLED_re     WSSTCL
180900091006     C                   Z-ADD(H)  WTKGED_re     WSSTKG
181000091006     C                   Z-ADD(H)  WTMCED_re     WSSTMC
181100100402     c* Spedizioni "E"
181200100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
181300100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
181400100402     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe
181500100402     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe
181600100402     c* Spedizioni "H"
181700100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
181800100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
181900100402     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh
182000100402     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh
182100100402     c
182200091006     C                   WRITE     WSLSS550
182300091006     C                   ENDIF
182400091006     C*
182500091006     C     WTSPEX_re     IFGT      *ZERO
182600091006     C                   Z-ADD     §dal          WSSDAL
182700110715     c                   movel     tipoC         WSSTIE
182800091006     C                   MOVE      'EX'          WSSTIP
182900091006     C                   Z-ADD     FIL(K)        WSSLIN
183000091006     C                   Z-ADD     WTSPEX_re     WSSTSP
183100091006     C                   Z-ADD     WTCLEX_re     WSSTCL
183200091006     C                   Z-ADD(H)  WTKGEX_re     WSSTKG
183300091006     C                   Z-ADD(H)  WTMCEX_re     WSSTMC
183400100402     c* Spedizioni "E"
183500100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
183600100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
183700100402     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe
183800100402     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe
183900100402     c* Spedizioni "H"
184000100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
184100100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
184200100402     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh
184300100402     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh
184400100402     c
184500091006     C                   WRITE     WSLSS550
184600091006     C                   ENDIF
184700091006     C*
184800091006     C     WTSPEA_re     IFGT      *ZERO
184900091006     C                   Z-ADD     §dal          WSSDAL
185000110715     c                   movel     tipoC         WSSTIE
185100091006     C                   MOVE      'EA'          WSSTIP
185200091006     C                   Z-ADD     FIL(K)        WSSLIN
185300091006     C                   Z-ADD     WTSPEA_re     WSSTSP
185400091006     C                   Z-ADD     WTCLEA_re     WSSTCL
185500091006     C                   Z-ADD(H)  WTKGEA_re     WSSTKG
185600091006     C                   Z-ADD(H)  WTMCEA_re     WSSTMC
185700100402     c* Spedizioni "E"
185800100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
185900100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
186000100402     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe
186100100402     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe
186200100402     c* Spedizioni "H"
186300100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
186400100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
186500100402     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh
186600100402     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh
186700100402     c
186800091006     C                   WRITE     WSLSS550
186900091006     C                   ENDIF
187000091006     C*
187100091006     C     WTSPPA_re     IFGT      *ZERO
187200091006     C                   Z-ADD     §dal          WSSDAL
187300110715     c                   movel     tipoC         WSSTIE
187400091006     C                   MOVE      'PA'          WSSTIP
187500091006     C                   Z-ADD     FIL(K)        WSSLIN
187600091006     C                   Z-ADD     WTSPPA_re     WSSTSP
187700091006     C                   Z-ADD     WTCLPA_re     WSSTCL
187800091006     C                   Z-ADD(H)  WTKGPA_re     WSSTKG
187900091006     C                   Z-ADD(H)  WTMCPA_re     WSSTMC
188000100402     c* Spedizioni "E"
188100100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
188200100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
188300100402     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe
188400100402     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe
188500100402     c* Spedizioni "H"
188600100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
188700100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
188800100402     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh
188900100402     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh
189000100402     c
189100091006     C                   WRITE     WSLSS550
189200091006     C                   ENDIF
189300091006     C                   ENDIF
189400100402
189500090206     c                   enddo
189600090206     c                   endsr
189700970527     C**********************************************************
189800970529     C** CARICAMENTO DATI INIZIALI
189900970527     C**********************************************************
190000970529     C     CARINZ        BEGSR
190100970905     C                   SETOFF                                       90
190200970529     C     KTAS          KLIST
190300970529     C                   KFLD                    WAAS
190400970529     C                   KFLD                    WMGS
190500970529     C     KTAB          KLIST
190600970529     C                   KFLD                    TBLKUT
190700970529     C                   KFLD                    TBLCOD
190800970529     C                   KFLD                    TBLKEY
190900970529     C     KTB           KLIST
191000970529     C                   KFLD                    TBLKUT
191100970529     C                   KFLD                    TBLCOD
191200970529     C                   Z-ADD     1             TBLKUT
191300090204     c     kwfsmp        klist
191400090204     c                   kfld                    k_fil
191500090204     c                   kfld                    k_tip
191600090204     c                   kfld                    k_dat
191700090205     C     kar5          KLIST
191800090205     C                   KFLD                    tasaas
191900090205     C                   KFLD                    taslnp
192000090205     C                   KFLD                    tasnrs
192100090205     C                   KFLD                    tasnsp
192200090205     C                   KFLD                    k_trd
192300090209     C                   KFLD                    tasflp
192400090915     c* apertura file di stampa congelato in SAVE
192500090915     C* ESEGUO OVRPRTF
192600090915     C                   Z-ADD     61            LUNG             15 5
192700090915     C                   MOVEL     *BLANKS       COMMAN           80
192800090915     C                   MOVEA     CMD           COMMAN
192900090915     C                   CALL      'QCMDEXC'                            H1
193000090915     C                   PARM                    COMMAN
193100090915     C                   PARM                    LUNG
193200090915     C*
193300090915     C                   OPEN      QSYSPRT
193400090915     C*
193500970529     C*
193600970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
193700970527     C** PULIZIA DS
193800980826     C                   DO        400           K                 3 0
193900970527     C     K             OCCUR     DSFIL
194000970527     C                   CLEAR                   DSFIL
194100970527     C                   END
194200100402     C                   DO        400           K                 3 0
194300100402     C     K             OCCUR     DSFIL_e
194400100402     C                   CLEAR                   DSFIL_e
194500100402     C                   END
194600100402     C                   DO        400           K                 3 0
194700100402     C     K             OCCUR     DSFIL_h
194800100402     C                   CLEAR                   DSFIL_h
194900100402     C                   END
195000090206     C                   DO        400           K                 3 0
195100090206     C     K             OCCUR     DSFIL_smp
195200090206     C                   CLEAR                   DSFIL_smp
195300090206     C                   END
195400970527     C*
195500970527     C                   Z-ADD     0             K
195600970527     C     *LOVAL        SETLL     AZORG01L
195700970527     C                   DO        *HIVAL
195800090204     C                   READ      AZORG01L
195900090204     c                   if        %eof(azorg01l)
196000090204     c                   leave
196100090204     c                   endif
196200970527     C     ORGFAG        IFEQ      'F'
196300970527     C     ORGFAG        OREQ      'A'
196400970527     C                   ADD       1             K
196500970527     C                   MOVEL     ORGFIL        FIL(K)
196600090204     C                   MOVEL     ORGdes        DIL(K)
196700090204     C                   z-add     ORGcar        car(K)
196800090204     c                   movel     orgde8        og148
196900090206     c                   if        §oglpo='1' or §oglpo='2'
197000090206     c                   move      §oglpo        lpo(K)
197100090206     c                   else
197200090206     c                   z-add     2             lpo(K)
197300090206     c                   endif
197400090204     c                   movel     orgde3        og143
197500090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
197600090209     c**                 move      'E'           tif(K)
197700090209     c**                 else
197800090209     c**                 move      'I'           tif(K)
197900090209     c**                 endif
198000090209     c                   select
198100090209     c                   when      §ogntw='DPD'
198200090209     c                   move      'D'           tif(K)
198300090209     c                   when      §ogntw='EEX'
198400090209     c                   move      'X'           tif(K)
198500090209     c                   when      §ogntw='FED'
198600090209     c                   move      'F'           tif(K)
198700090209     c                   other
198800090209     c                   move      'I'           tif(K)
198900090209     c                   endsl
199000970527     C                   ENDIF
199100090204     C                   ENDDO
199200970529     C*
199300970529     C                   Z-ADD     0             K
199400970529     C                   MOVEL     'TB'          TBLCOD
199500970529     C     KTB           SETLL     TABEL00F
199600970529     C                   DO        *HIVAL
199700090204     C     KTB           READE     TABEL00F
199800090204     c                   if        %eof(tabel00f)
199900090204     c                   leave
200000090204     c                   endif
200100970529     C     TBLFLG        IFNE      '*'
200200970529     C                   MOVEL     TBLUNI        DSTB
200300970529     C     §TBRBL        IFEQ      'N'
200400970529     C     §TBRBL        OREQ      'S'
200500970529     C                   ADD       1             K
200600970529     C                   MOVEL     TBLKEY        TBO(K)
200700970529     C                   ENDIF
200800970529     C                   ENDIF
200900090204     C                   ENDDO
201000091006     C*
201100091006     c* Lettura TABELLA clienti extra budget
201200110711     c                   if        §stat=' '
201300091006     C                   Z-ADD     0             K
201400091006     C                   MOVEL     'CXB'         TBecod
201500091006     C     tbecod        SETLL     tntbe01l
201600091006     C                   DO        *HIVAL
201700091006     C     tbecod        READE     tntbe01l
201800091006     c                   if        %eof(tntbe01l)
201900091006     c                   leave
202000091006     c                   endif
202100091006
202200091006     C     tbeatb        IFeq      ' '
202300091006     C                   MOVEL     TBeuni        Dcxb
202400091006     C                   ADD       1             K
202500091006     C                   MOVEL     TBeKE1        ksc_ex(k)
202600091007     C                   MOVE      §cxbctr       ksc_ex(k)
202700091006     C                   ENDIF
202800091006     C                   ENDDO
202900110711     c
203000110711     c                   else
203100110711     C                   Z-ADD     0             K
203200110711     C                   MOVEL     'CXQ'         TBecod
203300110711     C     tbecod        SETLL     tntbe01l
203400110711     C                   DO        *HIVAL
203500110711     C     tbecod        READE     tntbe01l
203600110711     c                   if        %eof(tntbe01l)
203700110711     c                   leave
203800110711     c                   endif
203900110711
204000110711     C     tbeatb        IFeq      ' '
204100110711     C                   ADD       1             K
204200110711     C                   MOVEL     TBeKE2        ksc_ex(k)
204300110711     C                   ENDIF
204400110711     C                   ENDDO
204500110711     c                   endif
204600110711     c*
204700110711     c* Controllo riempimento schiera
204800110715     c                   movel     kpjbu         savkpjbu
204900110711     c                   clear                   trul0sds
205000110711     c                   eval      i0stla='L'
205100110711     c                   eval      i0sski='KSC_EX'
205200110711     c                   eval      i0sele=%elem(ksc_ex)
205300110711     c                   eval      i0spie=k
205400110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
205500110711     c                   eval      i0ssif=knsif
205600110711     c                   eval      i0spgm='TNSSL3R2'
205700110711     c                   movel     trul0sds      kpjbu
205800110711     c                   call      'TRUL0SR'
205900110711     c                   parm                    kpjba
206000110715
206100110715     c                   movel     savkpjbu      kpjbu
206200090601     c
206300090204     c* data elaborazione nel formato AAAA/MM/GG
206400090204     c                   time                    dataiso
206500090204     c                   move      dataiso       wdatael
206600110711     c
206700110711     c                   if        §stat=' '
206800090601     c*
206900090601     c* Pulisco i file per i dati che devo rielaborare
207000090601     c                   eval      wrkgetlista =
207100090601     c                             'delete from WSLSS50F where wsstie= '''   +
207200090603     c                              Wtipo +  ''''
207300090601     c                   if        Wtipo='R'
207400090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
207500090603     c                              ' or  wsstie=''G'''
207600090601     c                   endif
207700090609     c*
207800090601     C/EXEC SQL
207900090601     C+ execute immediate :wrkgetlista
208000090601     C/END-EXEC
208100090609     c
208200090609     c* Pulisco i file per i dati che devo rielaborare
208300090609     c                   clear                   wrkgetlista
208400090609     c                   eval      wrkgetlista =
208500090609     c                             'delete from WFSMP00F where wmptip= '''   +
208600090609     c                              Wtipo +  ''''
208700090609     c                   if        Wtipo='R'
208800090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
208900090609     c                              ' or  wmptip=''G'''
209000090609     c                   endif
209100110711     c
209200110711     c                   endif
209300090609     c*
209400090609     C/EXEC SQL
209500090609     C+ execute immediate :wrkgetlista
209600090609     C/END-EXEC
209700091007     c*
209800091007     c* Pulisco il file clienti extra budget
209900091007     c                   clear                   wrkgetlista
210000110711     c
210100110711     c                   if        §stat=' '
210200091007     c                   eval      wrkgetlista =
210300091007     c                             'delete from WSLSS55F where wsstie= '''   +
210400091007     c                              Wtipo +  ''''
210500091007     c                   if        Wtipo='R'
210600091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
210700091007     c                              ' or  wsstie=''G'''
210800091007     c                   endif
210900110711     c
211000110711     c                   else
211100110711     c                   select
211200110711     c                   when      Wtipo='R'
211300110711     c                   eval      wrkgetlista =
211400110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
211500110711     c                               wsstie= ''B'''
211600110711     c                   when      Wtipo='G'
211700110711     c                   eval      wrkgetlista =
211800110711     c                             'delete from WSLSS55F where wsstie= ''A'''
211900110711     c                   when      Wtipo='P'
212000110711     c                   eval      wrkgetlista =
212100110711     c                             'delete from WSLSS55F where wsstie= ''C'''
212200110711     c                   endsl
212300110711     c
212400110711     c                   endif
212500091007     c*
212600091007     C/EXEC SQL
212700091007     C+ execute immediate :wrkgetlista
212800091007     C/END-EXEC
212900090601     c
213000970527     C                   ENDSR
213100100402     c****************************************************************************
213200100402     c     SpeITA        BEGSR
213300100402     c* giornaliero
213400100402     c                   if        (tasaas*10000+tasmgs)=§al
213500100402     C                   ADD       1             WTSPIP
213600100402     C                   ADD       TASNCL        WTCLIP
213700100402     C                   ADD       w_peso        WTKGIP
213800100402     C                   ADD       TASVLF        WTMCIP
213900100402     c* tipo servizio  "E"
214000100402     c                   if        tastsp='E'
214100100402     C                   ADD       1             eWTSPIP
214200100402     C                   ADD       TASNCL        eWTCLIP
214300100402     C                   ADD       w_peso        eWTKGIP
214400100402     C                   ADD       TASVLF        eWTMCIP
214500100402     c                   endif
214600100402     c* tipo servizio  "H"
214700100402     c                   if        tastsp='H'
214800100402     C                   ADD       1             hWTSPIP
214900100402     C                   ADD       TASNCL        hWTCLIP
215000100402     C                   ADD       w_peso        hWTKGIP
215100100402     C                   ADD       TASVLF        hWTMCIP
215200100402     c                   endif
215300100402     c**
215400100402     c* Extra budget
215500100402     c                   if        *in35
215600100402     C                   ADD       1             WTSPIP_e
215700100402     C                   ADD       TASNCL        WTCLIP_e
215800100402     C                   ADD       w_peso        WTKGIP_e
215900100402     C                   ADD       TASVLF        WTMCIP_e
216000100402     c* tipo servizio  "E"
216100100402     c                   if        tastsp='E'
216200100402     C                   ADD       1             eWTSPIP_e
216300100402     C                   ADD       TASNCL        eWTCLIP_e
216400100402     C                   ADD       w_peso        eWTKGIP_e
216500100402     C                   ADD       TASVLF        eWTMCIP_e
216600100402     c                   endif
216700100402     c* tipo servizio  "H"
216800100402     c                   if        tastsp='H'
216900100402     C                   ADD       1             hWTSPIP_e
217000100402     C                   ADD       TASNCL        hWTCLIP_e
217100100402     C                   ADD       w_peso        hWTKGIP_e
217200100402     C                   ADD       TASVLF        hWTMCIP_e
217300100402     c                   endif
217400100402
217500100402     c                   endif
217600100402     c                   endif
217700100402     c* riepilogo
217800100402     C                   ADD       1             WTSPIP_r
217900100402     C                   ADD       TASNCL        WTCLIP_r
218000100402     C                   ADD       w_peso        WTKGIP_r
218100100402     C                   ADD       TASVLF        WTMCIP_r
218200100402     c* tipo servizio  "E"
218300100402     c                   if        tastsp='E'
218400100402     C                   ADD       1             eWTSPIP_r
218500100402     C                   ADD       TASNCL        eWTCLIP_r
218600100402     C                   ADD       w_peso        eWTKGIP_r
218700100402     C                   ADD       TASVLF        eWTMCIP_r
218800100402     c                   endif
218900100402     c* tipo servizio  "H"
219000100402     c                   if        tastsp='H'
219100100402     C                   ADD       1             hWTSPIP_r
219200100402     C                   ADD       TASNCL        hWTCLIP_r
219300100402     C                   ADD       w_peso        hWTKGIP_r
219400100402     C                   ADD       TASVLF        hWTMCIP_r
219500100402     c                   endif
219600100402     c* Extra budget
219700100402     c                   if        *in35
219800100402     C                   ADD       1             WTSPIP_re
219900100402     C                   ADD       TASNCL        WTCLIP_re
220000100402     C                   ADD       w_peso        WTKGIP_re
220100100402     C                   ADD       TASVLF        WTMCIP_re
220200100402     c* tipo servizio  "E"
220300100402     c                   if        tastsp='E'
220400100402     C                   ADD       1             eWTSPIP_re
220500100402     C                   ADD       TASNCL        eWTCLIP_re
220600100402     C                   ADD       w_peso        eWTKGIP_re
220700100402     C                   ADD       TASVLF        eWTMCIP_re
220800100402     c                   endif
220900100402     c* tipo servizio  "H"
221000100402     c                   if        tastsp='H'
221100100402     C                   ADD       1             hWTSPIP_re
221200100402     C                   ADD       TASNCL        hWTCLIP_re
221300100402     C                   ADD       w_peso        hWTKGIP_re
221400100402     C                   ADD       TASVLF        hWTMCIP_re
221500100402     c                   endif
221600100402     c
221700100402     c                   endif
221800100402     c                   ENDSR
221900100402     c****************************************************************************
222000100402     c     SpeIMPORT     BEGSR
222100100402     c* giornaliero
222200100402     c                   if        (tasaas*10000+tasmgs)=§al
222300100402     C                   ADD       1             WTSPEI
222400100402     C                   ADD       TASNCL        WTCLEI
222500100402     C                   ADD       w_peso        WTKGEI
222600100402     C                   ADD       TASVLF        WTMCEI
222700100402     c* tipo servizio  "E"
222800100402     c                   if        tastsp='E'
222900100402     C                   ADD       1             eWTSPEI
223000100402     C                   ADD       TASNCL        eWTCLEI
223100100402     C                   ADD       w_peso        eWTKGEI
223200100402     C                   ADD       TASVLF        eWTMCEI
223300100402     c                   endif
223400100402     c* tipo servizio  "H"
223500100402     c                   if        tastsp='H'
223600100402     C                   ADD       1             hWTSPEI
223700100402     C                   ADD       TASNCL        hWTCLEI
223800100402     C                   ADD       w_peso        hWTKGEI
223900100402     C                   ADD       TASVLF        hWTMCEI
224000100402     c                   endif
224100100402     c* Extra Budget
224200100402     c                   if        *in35
224300100402     C                   ADD       1             WTSPEI_e
224400100402     C                   ADD       TASNCL        WTCLEI_e
224500100402     C                   ADD       w_peso        WTKGEI_e
224600100402     C                   ADD       TASVLF        WTMCEI_e
224700100402     c* tipo servizio  "E"
224800100402     c                   if        tastsp='E'
224900100402     C                   ADD       1             eWTSPEI_e
225000100402     C                   ADD       TASNCL        eWTCLEI_e
225100100402     C                   ADD       w_peso        eWTKGEI_e
225200100402     C                   ADD       TASVLF        eWTMCEI_e
225300100402     c                   endif
225400100402     c* tipo servizio  "H"
225500100402     c                   if        tastsp='H'
225600100402     C                   ADD       1             hWTSPEI_e
225700100402     C                   ADD       TASNCL        hWTCLEI_e
225800100402     C                   ADD       w_peso        hWTKGEI_e
225900100402     C                   ADD       TASVLF        hWTMCEI_e
226000100402     c                   endif
226100100402     c
226200100402     c                   endif
226300100402     c                   endif
226400100402     c* riepilogo
226500100402     C                   ADD       1             WTSPEI_r
226600100402     C                   ADD       TASNCL        WTCLEI_r
226700100402     C                   ADD       w_peso        WTKGEI_r
226800100402     C                   ADD       TASVLF        WTMCEI_r
226900100402     c* tipo servizio  "E"
227000100402     c                   if        tastsp='E'
227100100402     C                   ADD       1             eWTSPEI_r
227200100402     C                   ADD       TASNCL        eWTCLEI_r
227300100402     C                   ADD       w_peso        eWTKGEI_r
227400100402     C                   ADD       TASVLF        eWTMCEI_r
227500100402     c                   endif
227600100402     c* tipo servizio  "H"
227700100402     c                   if        tastsp='H'
227800100402     C                   ADD       1             hWTSPEI_r
227900100402     C                   ADD       TASNCL        hWTCLEI_r
228000100402     C                   ADD       w_peso        hWTKGEI_r
228100100402     C                   ADD       TASVLF        hWTMCEI_r
228200100402     c                   endif
228300100402     c* extra budget
228400100402     c                   if        *in35
228500100402     C                   ADD       1             WTSPEI_re
228600100402     C                   ADD       TASNCL        WTCLEI_re
228700100402     C                   ADD       w_peso        WTKGEI_re
228800100402     C                   ADD       TASVLF        WTMCEI_re
228900100402     c* tipo servizio  "E"
229000100402     c                   if        tastsp='E'
229100100402     C                   ADD       1             eWTSPEI_re
229200100402     C                   ADD       TASNCL        eWTCLEI_re
229300100402     C                   ADD       w_peso        eWTKGEI_re
229400100402     C                   ADD       TASVLF        eWTMCEI_re
229500100402     c                   endif
229600100402     c* tipo servizio  "H"
229700100402     c                   if        tastsp='H'
229800100402     C                   ADD       1             hWTSPEI_re
229900100402     C                   ADD       TASNCL        hWTCLEI_re
230000100402     C                   ADD       w_peso        hWTKGEI_re
230100100402     C                   ADD       TASVLF        hWTMCEI_re
230200100402     c                   endif
230300100402     c                   endif
230400100402     c                   ENDSR
230500100402     c****************************************************************************
230600100402     c     speEXPRESS    BEGSR
230700100402     c* giornaliero
230800100402     c                   if        (tasaas*10000+tasmgs)=§al
230900100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
231000100402     C                   ADD       TASNCL        WTCLEE
231100100402     C                   ADD       w_peso        WTKGEE
231200100402     C                   ADD       TASVLF        WTMCEE
231300100402     c* tipo servizio  "E"
231400100402     c                   if        tastsp='E'
231500100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
231600100402     C                   ADD       TASNCL        eWTCLEE
231700100402     C                   ADD       w_peso        eWTKGEE
231800100402     C                   ADD       TASVLF        eWTMCEE
231900100402     c                   endif
232000100402     c* tipo servizio  "H"
232100100402     c                   if        tastsp='H'
232200100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
232300100402     C                   ADD       TASNCL        hWTCLEE
232400100402     C                   ADD       w_peso        hWTKGEE
232500100402     C                   ADD       TASVLF        hWTMCEE
232600100402     c                   endif
232700100402     c* extra budget
232800100402     c                   if        *in35
232900100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
233000100402     C                   ADD       TASNCL        WTCLEE_e
233100100402     C                   ADD       w_peso        WTKGEE_e
233200100402     C                   ADD       TASVLF        WTMCEE_e
233300100402     c* tipo servizio  "E"
233400100402     c                   if        tastsp='E'
233500100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
233600100402     C                   ADD       TASNCL        eWTCLEE_e
233700100402     C                   ADD       w_peso        eWTKGEE_e
233800100402     C                   ADD       TASVLF        eWTMCEE_e
233900100402     c                   endif
234000100402     c* tipo servizio  "H"
234100100402     c                   if        tastsp='H'
234200100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
234300100402     C                   ADD       TASNCL        hWTCLEE_e
234400100402     C                   ADD       w_peso        hWTKGEE_e
234500100402     C                   ADD       TASVLF        hWTMCEE_e
234600100402     c                   endif
234700100402     c
234800100402     c                   endif
234900100402     c                   endif
235000100402     c* riepilogo
235100100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
235200100402     C                   ADD       TASNCL        WTCLEE_r
235300100402     C                   ADD       w_peso        WTKGEE_r
235400100402     C                   ADD       TASVLF        WTMCEE_r
235500100402     c* tipo servizio  "E"
235600100402     c                   if        tastsp='E'
235700100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
235800100402     C                   ADD       TASNCL        eWTCLEE_r
235900100402     C                   ADD       w_peso        eWTKGEE_r
236000100402     C                   ADD       TASVLF        eWTMCEE_r
236100100402     c                   endif
236200100402     c* tipo servizio  "H"
236300100402     c                   if        tastsp='H'
236400100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
236500100402     C                   ADD       TASNCL        hWTCLEE_r
236600100402     C                   ADD       w_peso        hWTKGEE_r
236700100402     C                   ADD       TASVLF        hWTMCEE_r
236800100402     c                   endif
236900100402     c* extra budget
237000100402     c                   if        *in35
237100100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
237200100402     C                   ADD       TASNCL        WTCLEE_re
237300100402     C                   ADD       w_peso        WTKGEE_re
237400100402     C                   ADD       TASVLF        WTMCEE_re
237500100402     c* tipo servizio  "E"
237600100402     c                   if        tastsp='E'
237700100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
237800100402     C                   ADD       TASNCL        eWTCLEE_re
237900100402     C                   ADD       w_peso        eWTKGEE_re
238000100402     C                   ADD       TASVLF        eWTMCEE_re
238100100402     c                   endif
238200100402     c* tipo servizio  "H"
238300100402     c                   if        tastsp='H'
238400100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
238500100402     C                   ADD       TASNCL        hWTCLEE_re
238600100402     C                   ADD       w_peso        hWTKGEE_re
238700100402     C                   ADD       TASVLF        hWTMCEE_re
238800100402     c                   endif
238900100402     c                   endif
239000100402     c
239100100402     c                   ENDSR
239200100402     c****************************************************************************
239300100402     c     speDPD        BEGSR
239400100402     c* giornaliero
239500100402     c                   if        (tasaas*10000+tasmgs)=§al
239600100402     C                   ADD       1             WTSPED                         DPD
239700100402     C                   ADD       TASNCL        WTCLED
239800100402     C                   ADD       w_peso        WTKGED
239900100402     C                   ADD       TASVLF        WTMCED
240000100402     c* tipo servizio  "E"
240100100402     c                   if        tastsp='E'
240200100402     C                   ADD       1             eWTSPED                         DPD
240300100402     C                   ADD       TASNCL        eWTCLED
240400100402     C                   ADD       w_peso        eWTKGED
240500100402     C                   ADD       TASVLF        eWTMCED
240600100402     c                   endif
240700100402     c* tipo servizio  "H"
240800100402     c                   if        tastsp='H'
240900100402     C                   ADD       1             hWTSPED                         DPD
241000100402     C                   ADD       TASNCL        hWTCLED
241100100402     C                   ADD       w_peso        hWTKGED
241200100402     C                   ADD       TASVLF        hWTMCED
241300100402     c                   endif
241400100402     c* extra budget
241500100402     c                   if        *in35
241600100402     C                   ADD       1             WTSPED_e                       DPD
241700100402     C                   ADD       TASNCL        WTCLED_e
241800100402     C                   ADD       w_peso        WTKGED_e
241900100402     C                   ADD       TASVLF        WTMCED_e
242000100402     c* tipo servizio  "E"
242100100402     c                   if        tastsp='E'
242200100402     C                   ADD       1             eWTSPED_e                       DPD
242300100402     C                   ADD       TASNCL        eWTCLED_e
242400100402     C                   ADD       w_peso        eWTKGED_e
242500100402     C                   ADD       TASVLF        eWTMCED_e
242600100402     c                   endif
242700100402     c* tipo servizio  "H"
242800100402     c                   if        tastsp='H'
242900100402     C                   ADD       1             hWTSPED_e                       DPD
243000100402     C                   ADD       TASNCL        hWTCLED_e
243100100402     C                   ADD       w_peso        hWTKGED_e
243200100402     C                   ADD       TASVLF        hWTMCED_e
243300100402     c                   endif
243400100402     c
243500100402     c                   endif
243600100402     c                   endif
243700100402     c* riepilogo
243800100402     C                   ADD       1             WTSPED_r                       DPD
243900100402     C                   ADD       TASNCL        WTCLED_r
244000100402     C                   ADD       w_peso        WTKGED_r
244100100402     C                   ADD       TASVLF        WTMCED_r
244200100402     c* tipo servizio  "E"
244300100402     c                   if        tastsp='E'
244400100402     C                   ADD       1             eWTSPED_r                       DPD
244500100402     C                   ADD       TASNCL        eWTCLED_r
244600100402     C                   ADD       w_peso        eWTKGED_r
244700100402     C                   ADD       TASVLF        eWTMCED_r
244800100402     c                   endif
244900100402     c* tipo servizio  "H"
245000100402     c                   if        tastsp='H'
245100100402     C                   ADD       1             hWTSPED_r                       DPD
245200100402     C                   ADD       TASNCL        hWTCLED_r
245300100402     C                   ADD       w_peso        hWTKGED_r
245400100402     C                   ADD       TASVLF        hWTMCED_r
245500100402     c                   endif
245600100402     c* extra budget
245700100402     c                   if        *in35
245800100402     C                   ADD       1             WTSPED_re                       DPD
245900100402     C                   ADD       TASNCL        WTCLED_re
246000100402     C                   ADD       w_peso        WTKGED_re
246100100402     C                   ADD       TASVLF        WTMCED_re
246200100402     c* tipo servizio  "E"
246300100402     c                   if        tastsp='E'
246400100402     C                   ADD       1             eWTSPED_re                       DPD
246500100402     C                   ADD       TASNCL        eWTCLED_re
246600100402     C                   ADD       w_peso        eWTKGED_re
246700100402     C                   ADD       TASVLF        eWTMCED_re
246800100402     c                   endif
246900100402     c* tipo servizio  "H"
247000100402     c                   if        tastsp='H'
247100100402     C                   ADD       1             hWTSPED_re                       DPD
247200100402     C                   ADD       TASNCL        hWTCLED_re
247300100402     C                   ADD       w_peso        hWTKGED_re
247400100402     C                   ADD       TASVLF        hWTMCED_re
247500100402     c                   endif
247600100402     c                   endif
247700100402     c                   ENDSR
247800100402     c****************************************************************************
247900100402     c     speFEDEX      BEGSR
248000100402     c* giornaliero
248100100402     c                   if        (tasaas*10000+tasmgs)=§al
248200100402     C                   ADD       1             WTSPEX                         FEDEX
248300100402     C                   ADD       TASNCL        WTCLEX
248400100402     C                   ADD       w_peso        WTKGEX
248500100402     C                   ADD       TASVLF        WTMCEX
248600100402     c* tipo servizio  "E"
248700100402     c                   if        tastsp='E'
248800100402     C                   ADD       1             eWTSPEX                         FEDEX
248900100402     C                   ADD       TASNCL        eWTCLEX
249000100402     C                   ADD       w_peso        eWTKGEX
249100100402     C                   ADD       TASVLF        eWTMCEX
249200100402     c                   endif
249300100402     c* tipo servizio  "H"
249400100402     c                   if        tastsp='H'
249500100402     C                   ADD       1             hWTSPEX                         FEDEX
249600100402     C                   ADD       TASNCL        hWTCLEX
249700100402     C                   ADD       w_peso        hWTKGEX
249800100402     C                   ADD       TASVLF        hWTMCEX
249900100402     c                   endif
250000100402     c* extra budget
250100100402     c                   if        *in35
250200100402     C                   ADD       1             WTSPEX_e                       FEDEX
250300100402     C                   ADD       TASNCL        WTCLEX_e
250400100402     C                   ADD       w_peso        WTKGEX_e
250500100402     C                   ADD       TASVLF        WTMCEX_e
250600100402     c* tipo servizio  "E"
250700100402     c                   if        tastsp='E'
250800100402     C                   ADD       1             eWTSPEX_e                       FEDEX
250900100402     C                   ADD       TASNCL        eWTCLEX_e
251000100402     C                   ADD       w_peso        eWTKGEX_e
251100100402     C                   ADD       TASVLF        eWTMCEX_e
251200100402     c                   endif
251300100402     c* tipo servizio  "H"
251400100402     c                   if        tastsp='H'
251500100402     C                   ADD       1             hWTSPEX_e                       FEDEX
251600100402     C                   ADD       TASNCL        hWTCLEX_e
251700100402     C                   ADD       w_peso        hWTKGEX_e
251800100402     C                   ADD       TASVLF        hWTMCEX_e
251900100402     c                   endif
252000100402     c                   endif
252100100402     c                   endif
252200100402     c* riepilogo
252300100402     C                   ADD       1             WTSPEX_r                       FEDEX
252400100402     C                   ADD       TASNCL        WTCLEX_r
252500100402     C                   ADD       w_peso        WTKGEX_r
252600100402     C                   ADD       TASVLF        WTMCEX_r
252700100402     c* tipo servizio  "E"
252800100402     c                   if        tastsp='E'
252900100402     C                   ADD       1             eWTSPEX_r                       FEDEX
253000100402     C                   ADD       TASNCL        eWTCLEX_r
253100100402     C                   ADD       w_peso        eWTKGEX_r
253200100402     C                   ADD       TASVLF        eWTMCEX_r
253300100402     c                   endif
253400100402     c* tipo servizio  "H"
253500100402     c                   if        tastsp='H'
253600100402     C                   ADD       1             hWTSPEX_r                       FEDEX
253700100402     C                   ADD       TASNCL        hWTCLEX_r
253800100402     C                   ADD       w_peso        hWTKGEX_r
253900100402     C                   ADD       TASVLF        hWTMCEX_r
254000100402     c                   endif
254100100402     c* Extra budget
254200100402     c                   if        *in35
254300100402     C                   ADD       1             WTSPEX_re                       FEDEX
254400100402     C                   ADD       TASNCL        WTCLEX_re
254500100402     C                   ADD       w_peso        WTKGEX_re
254600100402     C                   ADD       TASVLF        WTMCEX_re
254700100402     c* tipo servizio  "E"
254800100402     c                   if        tastsp='E'
254900100402     C                   ADD       1             eWTSPEX_re                       FEDEX
255000100402     C                   ADD       TASNCL        eWTCLEX_re
255100100402     C                   ADD       w_peso        eWTKGEX_re
255200100402     C                   ADD       TASVLF        eWTMCEX_re
255300100402     c                   endif
255400100402     c* tipo servizio  "H"
255500100402     c                   if        tastsp='H'
255600100402     C                   ADD       1             hWTSPEX_re                       FEDEX
255700100402     C                   ADD       TASNCL        hWTCLEX_re
255800100402     C                   ADD       w_peso        hWTKGEX_re
255900100402     C                   ADD       TASVLF        hWTMCEX_re
256000100402     c                   endif
256100100402     c
256200100402     c                   endif
256300100402     c                   ENDSR
256400100402     c****************************************************************************
256500100402     c     speITA_LNA    BEGSR
256600100402     c* giornaliero
256700100402     c                   if        (tasaas*10000+tasmgs)=§al
256800100402     C                   ADD       1             WTSPIA
256900100402     C                   ADD       TASNCL        WTCLIA
257000100402     C                   ADD       W_peso        WTKGIA
257100100402     C                   ADD       TASVLF        WTMCIA
257200100402     c* tipo servizio  "E"
257300100402     c                   if        tastsp='E'
257400100402     C                   ADD       1             eWTSPIA
257500100402     C                   ADD       TASNCL        eWTCLIA
257600100402     C                   ADD       W_peso        eWTKGIA
257700100402     C                   ADD       TASVLF        eWTMCIA
257800100402     c                   endif
257900100402     c* tipo servizio  "H"
258000100402     c                   if        tastsp='H'
258100100402     C                   ADD       1             hWTSPIA
258200100402     C                   ADD       TASNCL        hWTCLIA
258300100402     C                   ADD       W_peso        hWTKGIA
258400100402     C                   ADD       TASVLF        hWTMCIA
258500100402     c                   endif
258600100402     c* extra budget
258700100402     c                   if        *in35
258800100402     C                   ADD       1             WTSPIA_e
258900100402     C                   ADD       TASNCL        WTCLIA_e
259000100402     C                   ADD       w_peso        WTKGIA_e
259100100402     C                   ADD       TASVLF        WTMCIA_e
259200100402     c* tipo servizio  "E"
259300100402     c                   if        tastsp='E'
259400100402     C                   ADD       1             eWTSPIA_e
259500100402     C                   ADD       TASNCL        eWTCLIA_e
259600100402     C                   ADD       w_peso        eWTKGIA_e
259700100402     C                   ADD       TASVLF        eWTMCIA_e
259800100402     c                   endif
259900100402     c* tipo servizio  "H"
260000100402     c                   if        tastsp='H'
260100100402     C                   ADD       1             hWTSPIA_e
260200100402     C                   ADD       TASNCL        hWTCLIA_e
260300100402     C                   ADD       w_peso        hWTKGIA_e
260400100402     C                   ADD       TASVLF        hWTMCIA_e
260500100402     c                   endif
260600100402     c
260700100402     c                   endif
260800100402     c                   endif
260900100402     c* riepilogo
261000100402     C                   ADD       1             WTSPIA_r
261100100402     C                   ADD       TASNCL        WTCLIA_r
261200100402     C                   ADD       w_peso        WTKGIA_r
261300100402     C                   ADD       TASVLF        WTMCIA_r
261400100402     c* tipo servizio  "E"
261500100402     c                   if        tastsp='E'
261600100402     C                   ADD       1             eWTSPIA_r
261700100402     C                   ADD       TASNCL        eWTCLIA_r
261800100402     C                   ADD       w_peso        eWTKGIA_r
261900100402     C                   ADD       TASVLF        eWTMCIA_r
262000100402     c                   endif
262100100402     c* tipo servizio  "H"
262200100402     c                   if        tastsp='H'
262300100402     C                   ADD       1             hWTSPIA_r
262400100402     C                   ADD       TASNCL        hWTCLIA_r
262500100402     C                   ADD       w_peso        hWTKGIA_r
262600100402     C                   ADD       TASVLF        hWTMCIA_r
262700100402     c                   endif
262800100402     c* extra budget
262900100402     c                   if        *in35
263000100402     C                   ADD       1             WTSPIA_re
263100100402     C                   ADD       TASNCL        WTCLIA_re
263200100402     C                   ADD       w_peso        WTKGIA_re
263300100402     C                   ADD       TASVLF        WTMCIA_re
263400100402     c* tipo servizio  "E"
263500100402     c                   if        tastsp='E'
263600100402     C                   ADD       1             eWTSPIA_re
263700100402     C                   ADD       TASNCL        eWTCLIA_re
263800100402     C                   ADD       w_peso        eWTKGIA_re
263900100402     C                   ADD       TASVLF        eWTMCIA_re
264000100402     c                   endif
264100100402     c* tipo servizio  "H"
264200100402     c                   if        tastsp='H'
264300100402     C                   ADD       1             hWTSPIA_re
264400100402     C                   ADD       TASNCL        hWTCLIA_re
264500100402     C                   ADD       w_peso        hWTKGIA_re
264600100402     C                   ADD       TASVLF        hWTMCIA_re
264700100402     c                   endif
264800100402     c                   endif
264900100402     c                   ENDSR
265000100402     c****************************************************************************
265100100402     c     speEXPORT     BEGSR
265200100402     c* giornaliero
265300100402     c                   if        (tasaas*10000+tasmgs)=§al
265400100402     C                   ADD       1             WTSPEA
265500100402     C                   ADD       TASNCL        WTCLEA
265600100402     C                   ADD       w_peso        WTKGEA
265700100402     C                   ADD       TASVLF        WTMCEA
265800100402     c* tipo servizio  "E"
265900100402     c                   if        tastsp='E'
266000100402     C                   ADD       1             eWTSPEA
266100100402     C                   ADD       TASNCL        eWTCLEA
266200100402     C                   ADD       w_peso        eWTKGEA
266300100402     C                   ADD       TASVLF        eWTMCEA
266400100402     c                   endif
266500100402     c* tipo servizio  "H"
266600100402     c                   if        tastsp='H'
266700100402     C                   ADD       1             hWTSPEA
266800100402     C                   ADD       TASNCL        hWTCLEA
266900100402     C                   ADD       w_peso        hWTKGEA
267000100402     C                   ADD       TASVLF        hWTMCEA
267100100402     c                   endif
267200100402     c* extra budget
267300100402     c                   if        *in35
267400100402     C                   ADD       1             WTSPEA_e
267500100402     C                   ADD       TASNCL        WTCLEA_e
267600100402     C                   ADD       w_peso        WTKGEA_e
267700100402     C                   ADD       TASVLF        WTMCEA_e
267800100402     c* tipo servizio  "E"
267900100402     c                   if        tastsp='E'
268000100402     C                   ADD       1             eWTSPEA_e
268100100402     C                   ADD       TASNCL        eWTCLEA_e
268200100402     C                   ADD       w_peso        eWTKGEA_e
268300100402     C                   ADD       TASVLF        eWTMCEA_e
268400100402     c                   endif
268500100402     c* tipo servizio  "H"
268600100402     c                   if        tastsp='H'
268700100402     C                   ADD       1             hWTSPEA_e
268800100402     C                   ADD       TASNCL        hWTCLEA_e
268900100402     C                   ADD       w_peso        hWTKGEA_e
269000100402     C                   ADD       TASVLF        hWTMCEA_e
269100100402     c                   endif
269200100402     c
269300100402     c                   endif
269400100402     c                   endif
269500100402     c* riepilogo
269600100402     C                   ADD       1             WTSPEA_r
269700100402     C                   ADD       TASNCL        WTCLEA_r
269800100402     C                   ADD       w_peso        WTKGEA_r
269900100402     C                   ADD       TASVLF        WTMCEA_r
270000100402     c* tipo servizio  "E"
270100100402     c                   if        tastsp='E'
270200100402     C                   ADD       1             eWTSPEA_r
270300100402     C                   ADD       TASNCL        eWTCLEA_r
270400100402     C                   ADD       w_peso        eWTKGEA_r
270500100402     C                   ADD       TASVLF        eWTMCEA_r
270600100402     c                   endif
270700100402     c* tipo servizio  "H"
270800100402     c                   if        tastsp='H'
270900100402     C                   ADD       1             hWTSPEA_r
271000100402     C                   ADD       TASNCL        hWTCLEA_r
271100100402     C                   ADD       w_peso        hWTKGEA_r
271200100402     C                   ADD       TASVLF        hWTMCEA_r
271300100402     c                   endif
271400100402     c* extra budget
271500100402     c                   if        *in35
271600100402     C                   ADD       1             WTSPEA_re
271700100402     C                   ADD       TASNCL        WTCLEA_re
271800100402     C                   ADD       w_peso        WTKGEA_re
271900100402     C                   ADD       TASVLF        WTMCEA_re
272000100402     c* tipo servizio  "E"
272100100402     c                   if        tastsp='E'
272200100402     C                   ADD       1             eWTSPEA_re
272300100402     C                   ADD       TASNCL        eWTCLEA_re
272400100402     C                   ADD       w_peso        eWTKGEA_re
272500100402     C                   ADD       TASVLF        eWTMCEA_re
272600100402     c                   endif
272700100402     c* tipo servizio  "H"
272800100402     c                   if        tastsp='H'
272900100402     C                   ADD       1             hWTSPEA_re
273000100402     C                   ADD       TASNCL        hWTCLEA_re
273100100402     C                   ADD       w_peso        hWTKGEA_re
273200100402     C                   ADD       TASVLF        hWTMCEA_re
273300100402     c                   endif
273400100402     c
273500100402     c                   endif
273600100402     c                   ENDSR
273700100402     c****************************************************************************
273800970519     OQSYSPRT   E            TESTA          2 02
273900090206     O                                         +  1 'Terminata elaborazione'
274000090206     o                                         +  2 'statistiche spedizioni'
274100090206     O                                         +  2 'DEL'
274200970519     O                       UDATE         Y   +  1
274300970528     O                       §DAL              +  5 '    /  /  '
274400970528     O                                         +  1 '>'
274500970528     O                       §AL               +  1 '    /  /  '
274600090206     O                                          120 'TNSSL3R1'
274700970519     O                       PAGE          Z    132
274800970519     O                                          128 'PAG.'
274900970529     O          E            TESTA          2
275000970529     O                                           14 'TIPI BOLLA OK:'
275100970529     O                       TBO               +  1
275200970529     O          E            ERR            1
275300970529     O                       TASLNP               3
275400970529     O                       TASLNA            +  1
275500970529     O                       TASAAS            +  1
275600970529     O                       TASMGS            +  1
275700970529     O                       TASNRS            +  1
275800970529     O                       TASNSP            +  1
275900970529     O                       TASTBL            +  1
276000110801     O          E            TESTA       1  1
276100110801     O                                            7 'Clienti'
276200110801     o                       Ptipocli         +   1
276300110801     O                                        +   1 ':'
276400110801     O          E            detksc         1
276500110801     o                       skicli             132
276600090915**
276700090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
