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
002100111129     D car             S              3  0 DIM(400)                             codice area tif(ale
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
056300111129     D kk              S              3  0
056400111129     D EE              S              3  0
056500091006     D Wdatael         S              8  0
056600091007     D w010a           S             10
056700110801     D PTIPOCLI        S             12
056800110801     D skicli          S            132
056900090204     d dataiso         s               d   datfmt(*iso)
057000090204     d waas            s                   like(tasaas)
057100090204     d wmgs            s                   Like(tasmgs)
057200090224     d w_peso          s                   Like(taspkf)
057300090204     d k_fil           s                   Like(wmpfil)
057400090204     d k_tip           s                   Like(wmptip)
057500090204     d k_dat           s                   Like(wmpdal)
057600090205     d k_trd           s                   Like(ar5trd) inz('TRS')
057700091006     d savccm          s                   Like(tasccm)
057800091006     d savctr          s                   Like(tasctr)
057900110715     d savkpjbu        s                   Like(kpjbu)
058000111028     d
058100111028     d titas00ds     E DS                  extname(titas00f)
058200090601     d
058300111028     d wrkgetlista     s          25767    varying
058400970527     C* RICEVIMENTO PARAMETRI
058500970527     C     *ENTRY        PLIST
058600970527     C                   PARM                    KPJBA
058700970527     C                   MOVEL     §DAL          WAAS
058800970527     C                   MOVE      §DAL          WMGS
058900090601     C                   MOVE      §tipo         Wtipo             1
059000970528     C*
059100970529     C                   EXSR      CARINZ
059200951220     C*
059300990811     C* LETTURA TITAS
059400111028     c                   if        §stat=' '
059500990811     C     KTAS          SETLL     TITAS38C
059600111028     c                   else
059700111028
059800111028     C/EXEC SQL
059900111028     C+ PREPARE s1 FROM :wrkgetlista
060000111028     C/END-EXEC
060100111028     C
060200111028     C/EXEC SQL
060300111028     C+ DECLARE A1 CURSOR FOR S1
060400111028     C/END-EXEC
060500111028     C
060600111028     C/EXEC SQL
060700111028     C+ OPEN a1
060800111028     C/END-EXEC
060900111028
061000111028     c                   endif
061100111028     c
061200970527     C                   DO        *HIVAL
061300111028     c
061400111028     c                   if        §stat=' '
061500090204     C                   READ      TITAS38C
061600090204     c* Termina il ciclo se fine file o per data spedizione > data al
061700090204     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
061800090204     c                   leave
061900090204     c                   endif
062000111028     c
062100111028     c                   else
062200111028     C/EXEC SQL
062300111028     C+ FETCH NEXT FROM a1 INTO :titas00ds
062400111028     C/END-EXEC
062500111028
062600111028     c                   select
062700111028
062800111028     c                   when      sqlcod = 100
062900111028     C/EXEC SQL
063000111103     C+ CLOSE a1
063100111028     C/END-EXEC
063200111028     c                   leave
063300111028     c                   when      sqlcod < 0
063400111028     c                   seton                                        H1
063500111028     C/EXEC SQL
063600111103     C+ CLOSE a1
063700111028     C/END-EXEC
063800111028     c                   leave
063900111028     c                   endsl
064000111028     c
064100111028     c                   endif
064200111028     c
064300090204     c* Scarto in base al tipo bolla
064400090204     c                   if        %lookup(tastbl:tbo)=0
064500090204     c                   iter
064600090204     c                   endif
064700970527     C*
064800090601     c
064900090601     c* EDPES : 01/06/09
065000090601     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
065100090901     c***                z-add     taspkf        w_peso
065200090901     c* EDPES:  01/09/09
065300090901     c**  riutilizzo il peso VDL
065400090901     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
065500090901     c* 1. VDL totale
065600090901     c* 2. VDL parziale se > di TASPKF
065700090901     c* 3. TASPKF
065800090901     c                   clear                   w_peso
065900090901     c                   if        tasncp=tasncl or (tasncp>0 and taspkc>taspkf)
066000090901     c                   z-add     taspkc        w_peso
066100090901     c                   else
066200090901     c                   z-add     taspkf        w_peso
066300090901     c                   endif
066400090601     c
066500110711     c                   if        §stat=' '
066600090204     c                   exsr      sr_wfsmp
066700110711     c                   endif
066800110711     c
066900090204     c                   exsr      sr_wslss
067000090204     C                   ENDDO
067100090206     c*
067200090206     C* SCARICO DS SU FILE
067300090206     c                   exsr      scrive_file
067400110801     c                   if        §stat=' '
067500110801     c                   eval      ptipocli='EXTRA BUDGET'
067600110801     c                   else
067700110801     c                   eval      ptipocli='PARTICOLARI '
067800110801     c                   endif
067900110801     c
068000090206     c                   except    testa
068100110801     C*
068200110801     c                   z-add     1             k
068300110801     c                   clear                   conta             3 0
068400110801     c                   clear                   skicli
068500110801     c                   dow       ksc_ex(k)<>*blanks
068600110801     c
068700110801     c                   if        §stat=' '
068800110801
068900110801     c                   eval      skicli=%trim(skicli)+' '+
069000110801     c                             %subst(ksc_ex(k):1:7)
069100110801     c                   add       7             conta
069200110801     c*
069300110801     c                   if        %subst(ksc_ex(k):8:3)<>'   '
069400110801     c                   eval      skicli=%trim(skicli)+'/'+
069500110801     c                             %subst(ksc_ex(k):8:3)
069600110801     c                   add       4             conta
069700110801     c                   endif
069800110801     c                   add       1             conta
069900110801     c
070000110801     c
070100110801     c                   if        conta>120
070200110801     c                   except    detksc
070300110801     c                   clear                   conta
070400110801     c                   clear                   skicli
070500110801     c                   endif
070600110801     c
070700110801     c                   else
070800110801     c
070900110801     c                   eval      skicli=%trim(skicli)+' '+
071000110801     c                             %subst(ksc_ex(k):1:7)
071100110801     c                   add       1             conta
071200110801     c
071300110801     c                   if        conta=16
071400110801     c                   except    detksc
071500110801     c                   clear                   conta
071600110801     c                   clear                   skicli
071700110801     c                   endif
071800110801     c
071900110801     c                   endif
072000110801     c
072100110801     c                   add       1             k
072200110801     c                   enddo
072300110801     c
072400110801     c                   if        conta>0
072500110801     c                   except    detksc
072600110801     c                   endif
072700970528     C*
072800970528     C                   SETON                                        LR
072900970519     C*
073000090204     C**********************************************************
073100090204     C** Elaborazione per file wfsmp00f
073200090204     C**********************************************************
073300090204     C     sr_wfsmp      BEGSR
073400090204     c* linea di partenza
073500090206     c                   z-add     1             k
073600090206     c     taslnp        lookup    fil(k)                                 58
073700090206     c                   if        *in58
073800090206     c     k             occur     dsfil_smp
073900090205     c* . . . giornaliero
074000090206     c                   if        (tasaas*10000+tasmgs)=§al
074100090206     C                   add       1             Wmptsp_g
074200090206     C                   add       TASNCL        wmptcp_g
074300090224     C                   add       w_peso        Wmptkp_g
074400090206     C                   add       TASVLF        Wmptmp_g
074500090206     c                   endif
074600090204     c* . . . riepilogo
074700090206     C                   add       1             Wmptsp_r
074800090206     C                   add       TASNCL        wmptcp_r
074900090224     C                   add       w_peso        Wmptkp_r
075000090206     C                   add       TASVLF        Wmptmp_r
075100090206     c                   endif
075200090204     c* linea di arrivo
075300090206     c                   z-add     1             k
075400090206     c     taslna        lookup    fil(k)                                 58
075500090206     c                   if        *in58
075600090206     c     k             occur     dsfil_smp
075700090205     c* . . . giornaliero
075800090206     c                   if        (tasaas*10000+tasmgs)=§al
075900090206     C                   add       1             Wmptsa_g
076000090206     C                   add       TASNCL        wmptca_g
076100090224     C                   add       w_peso        Wmptka_g
076200090206     C                   add       TASVLF        Wmptma_g
076300090206     c                   endif
076400090205     c* . . . riepilogo
076500090206     C                   add       1             Wmptsa_r
076600090206     C                   add       TASNCL        wmptca_r
076700090224     C                   add       w_peso        Wmptka_r
076800090206     C                   add       TASVLF        Wmptma_r
076900090206     c                   endif
077000090710     c* terminal di partenza : esclusa la linea/terminal
077100090710     c                   if        tastfp<>taslnp
077200090206     c                   z-add     1             k
077300090206     c     tastfp        lookup    fil(k)                                 58
077400090206     c                   if        *in58
077500090206     c     k             occur     dsfil_smp
077600090205     c* . . . giornaliero
077700090206     c                   if        (tasaas*10000+tasmgs)=§al
077800090206     C                   add       1             Wmptspt_g
077900090206     C                   add       TASNCL        wmptcpt_g
078000090224     C                   add       w_peso        Wmptkpt_g
078100090206     C                   add       TASVLF        Wmptmpt_g
078200090206     c                   endif
078300090205     c* . . . riepilogo
078400090206     C                   add       1             Wmptspt_r
078500090206     C                   add       TASNCL        wmptcpt_r
078600090224     C                   add       w_peso        Wmptkpt_r
078700090206     C                   add       TASVLF        Wmptmpt_r
078800090206     c                   endif
078900090710     c                   endif
079000090710     c* terminal di partenza : tutto compresa la linea stessa
079100090710     c                   z-add     1             k
079200090710     c     tastfp        lookup    fil(k)                                 58
079300090710     c                   if        *in58
079400090710     c     k             occur     dsfil_smp
079500090710     c* . . . giornaliero
079600090710     c                   if        (tasaas*10000+tasmgs)=§al
079700090710     C                   add       1             Wmptsptc_g
079800090710     C                   add       TASNCL        wmptcptc_g
079900090710     C                   add       w_peso        Wmptkptc_g
080000090710     C                   add       TASVLF        Wmptmptc_g
080100090710     c                   endif
080200090710     c* . . . riepilogo
080300090710     C                   add       1             Wmptsptc_r
080400090710     C                   add       TASNCL        wmptcptc_r
080500090710     C                   add       w_peso        Wmptkptc_r
080600090710     C                   add       TASVLF        Wmptmptc_r
080700090710     c                   endif
080800090710     c
080900090710     c* terminal di arrivo : esclusa la linea/terminal
081000090710     c                   if        tastfa<>taslna
081100090206     c                   z-add     1             k
081200090206     c     tastfa        lookup    fil(k)                                 58
081300090206     c                   if        *in58
081400090206     c     k             occur     dsfil_smp
081500090205     c* . . . giornaliero
081600090206     c                   if        (tasaas*10000+tasmgs)=§al
081700090206     C                   add       1             Wmptsat_g
081800090206     C                   add       TASNCL        wmptcat_g
081900090224     C                   add       w_peso        Wmptkat_g
082000090206     C                   add       TASVLF        Wmptmat_g
082100090206     c                   endif
082200090205     c* . . . riepilogo
082300090206     C                   add       1             Wmptsat_r
082400090206     C                   add       TASNCL        wmptcat_r
082500090224     C                   add       w_peso        Wmptkat_r
082600090206     C                   add       TASVLF        Wmptmat_r
082700090205     c                   endif
082800090710     c                   endif
082900090710     c* terminal di arrivo : tutto compreso il terminal stesso
083000090710     c                   z-add     1             k
083100090710     c     tastfa        lookup    fil(k)                                 58
083200090710     c                   if        *in58
083300090710     c     k             occur     dsfil_smp
083400090710     c* . . . giornaliero
083500090710     c                   if        (tasaas*10000+tasmgs)=§al
083600090710     C                   add       1             Wmptsatc_g
083700090710     C                   add       TASNCL        wmptcatc_g
083800090710     C                   add       w_peso        Wmptkatc_g
083900090710     C                   add       TASVLF        Wmptmatc_g
084000090710     c                   endif
084100090710     c* . . . riepilogo
084200090710     C                   add       1             Wmptsatc_r
084300090710     C                   add       TASNCL        wmptcatc_r
084400090710     C                   add       w_peso        Wmptkatc_r
084500090710     C                   add       TASVLF        Wmptmatc_r
084600090710     c                   endif
084700090710     c
084800090204     c* filiali di transito
084900090209     c     kar5          chain     fiar531c
085000090205     c                   if        %found(fiar531c)
085100090205     c                   movel     ar5uni        dar5trs
085200090205     c*
085300090205     c                   eval      k_fil=§AR5FLP
085400090205     c                   exsr      agg_transiti
085500090205      *
085600090205     c                   if        §ar5fl2>0
085700090205     c                   eval      k_fil=§AR5FL2
085800090205     c                   exsr      agg_transiti
085900090205     c                   endif
086000090205      *
086100090205     c                   if        §ar5fl3>0
086200090205     c                   eval      k_fil=§AR5FL3
086300090205     c                   exsr      agg_transiti
086400090205     c                   endif
086500090205      *
086600090205     c                   if        §ar5fl4>0
086700090205     c                   eval      k_fil=§AR5FL4
086800090205     c                   exsr      agg_transiti
086900090205     c                   endif
087000090205      *
087100090205     c                   if        §ar5fl5>0
087200090205     c                   eval      k_fil=§AR5FL5
087300090205     c                   exsr      agg_transiti
087400090205     c                   endif
087500090205     c                   endif
087600090204     c
087700090204     C                   endsr
087800090205     C**********************************************************
087900090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
088000090205     C**********************************************************
088100090205     C     agg_transiti  BEGSR
088200090206     c                   z-add     1             k
088300090206     c     k_fil         lookup    fil(k)                                 58
088400090206     c                   if        *in58
088500090206     c     k             occur     dsfil_smp
088600090205     c* . . . giornaliero
088700090206     c                   if        (tasaas*10000+tasmgs)=§al
088800090206     C                   add       1             Wmptst_g
088900090206     C                   add       TASNCL        wmptct_g
089000090224     C                   add       w_peso        Wmptkt_g
089100090206     C                   add       TASVLF        Wmptmt_g
089200090206     c                   endif
089300090205     c* . . . riepilogo
089400090206     C                   add       1             Wmptst_r
089500090206     C                   add       TASNCL        wmptct_r
089600090224     C                   add       w_peso        Wmptkt_r
089700090206     C                   add       TASVLF        Wmptmt_r
089800090205     c                   endif
089900090205     c                   endsr
090000090204     C**********************************************************
090100090206     C** Riempie campi wfsmp00f
090200090204     C**********************************************************
090300090206     C     rie_campi     BEGSR
090400090204     c* profilo utente e data elaborazione
090500090204     c                   eval      wmpusr=knmus
090600090204     c                   eval      wmpdel=wdatael
090700090206     c*
090800090206     c                   eval      wmpall=§al
090900090204     c* area e descrizione area
091000090204     c                   eval      wmpcar=car(k)
091100090204     C                   MOVEL(P)  '05'          tblcod
091200090204     C                   MOVEL(P)  car(K)        tblkey
091300090204     C     KTAB          CHAIN     TABEL00F                           30
091400090204     C                   IF        %found(tabel00f) and tblflg=*blanks
091500090204     C                   MOVEL     TBLUNI        DS05
091600090204     C                   MOVEL     §05DES        wmpdar
091700090204     C                   endif
091800090204     c* filiale,descrizione e livello filiale
091900090204     c                   eval      wmpdil=dil(K)
092000090206     c                   eval      wmpfil=fil(K)
092100090206     c                   eval      wmplif=lpo(K)
092200090204     c* tipo filiale Italia/Estero
092300090205     c                   eval      wmptif=tif(K)
092400090206     c* spedizioni,colli, kg,volume
092500090206     c                   if        wmptip='G'
092600090206     c                   z-add     wmptsp_G      wmptsp
092700090206     c                   z-add     wmptcp_G      wmptcp
092800090206     c                   z-add(h)  wmptkp_G      wmptkp
092900090206     c                   z-add(h)  wmptmp_G      wmptmp
093000090710
093100090206     c                   z-add     wmptsa_G      wmptsa
093200090206     c                   z-add     wmptca_G      wmptca
093300090206     c                   z-add(h)  wmptka_G      wmptka
093400090206     c                   z-add(h)  wmptma_G      wmptma
093500090710
093600090206     c                   z-add     wmptspt_G     wmptspt
093700090206     c                   z-add     wmptcpt_G     wmptcpt
093800090206     c                   z-add(h)  wmptkpt_G     wmptkpt
093900090206     c                   z-add(h)  wmptmpt_G     wmptmpt
094000090710
094100090710     c                   z-add     wmptsptc_G    wmptsptc
094200090710     c                   z-add     wmptcptc_G    wmptcptc
094300090710     c                   z-add(h)  wmptkptc_G    wmptkptc
094400090710     c                   z-add(h)  wmptmptc_G    wmptmptc
094500090710
094600090206     c                   z-add     wmptsat_G     wmptsat
094700090206     c                   z-add     wmptcat_G     wmptcat
094800090206     c                   z-add(h)  wmptkat_G     wmptkat
094900090206     c                   z-add(h)  wmptmat_G     wmptmat
095000090710
095100090710     c                   z-add     wmptsatc_G    wmptsatc
095200090710     c                   z-add     wmptcatc_G    wmptcatc
095300090710     c                   z-add(h)  wmptkatc_G    wmptkatc
095400090710     c                   z-add(h)  wmptmatc_G    wmptmatc
095500090710
095600090206     c                   z-add     wmptst_G      wmptst
095700090206     c                   z-add     wmptct_G      wmptct
095800090206     c                   z-add(h)  wmptkt_G      wmptkt
095900090206     c                   z-add(h)  wmptmt_G      wmptmt
096000090206     c                   else
096100090206     c                   z-add     wmptsp_r      wmptsp
096200090206     c                   z-add     wmptcp_r      wmptcp
096300090206     c                   z-add(h)  wmptkp_r      wmptkp
096400090206     c                   z-add(h)  wmptmp_r      wmptmp
096500090710
096600090206     c                   z-add     wmptsa_r      wmptsa
096700090206     c                   z-add     wmptca_r      wmptca
096800090206     c                   z-add(h)  wmptka_r      wmptka
096900090206     c                   z-add(h)  wmptma_r      wmptma
097000090710
097100090206     c                   z-add     wmptspt_r     wmptspt
097200090206     c                   z-add     wmptcpt_r     wmptcpt
097300090206     c                   z-add(h)  wmptkpt_r     wmptkpt
097400090206     c                   z-add(h)  wmptmpt_r     wmptmpt
097500090710
097600090710     c                   z-add     wmptsptc_r    wmptsptc
097700090710     c                   z-add     wmptcptc_r    wmptcptc
097800090710     c                   z-add(h)  wmptkptc_r    wmptkptc
097900090710     c                   z-add(h)  wmptmptc_r    wmptmptc
098000090710
098100090206     c                   z-add     wmptsat_r     wmptsat
098200090206     c                   z-add     wmptcat_r     wmptcat
098300090206     c                   z-add(h)  wmptkat_r     wmptkat
098400090206     c                   z-add(h)  wmptmat_r     wmptmat
098500090710
098600090710     c                   z-add     wmptsatc_r    wmptsatc
098700090710     c                   z-add     wmptcatc_r    wmptcatc
098800090710     c                   z-add(h)  wmptkatc_r    wmptkatc
098900090710     c                   z-add(h)  wmptmatc_r    wmptmatc
099000090710
099100090206     c                   z-add     wmptst_r      wmptst
099200090206     c                   z-add     wmptct_r      wmptct
099300090206     c                   z-add(h)  wmptkt_r      wmptkt
099400090206     c                   z-add(h)  wmptmt_r      wmptmt
099500090710
099600090206     c                   endif
099700090204     c                   endsr
099800090204     C**********************************************************
099900090204     C** Elaborazione per file wslss50f
100000090204     C**********************************************************
100100090204     C     sr_wslss      BEGSR
100200090204     C                   SETOFF                                       545556
100300090204     C                   SETOFF                                       48  52
100400111129     c
100500111129     c                   z-add     1             kk
100600111129     c     taslnp        lookup    fil(kk)                                98
100700111129     c
100800111129     c                   if        *in98
100900111129     c* Euroexpress o Fedex o DPD
101000111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
101100111129     c                   seton                                        54
101200111129     c                   endif
101300111129     c
101400111129     C**   TASLNP        COMP      300                                54        ESTERO
101500111129     C*+N54TASLNP        COMP      190                                    54    + DPD
101600111129     C**N54TASLNP        COMP      191                                    54    + DPD
101700111129     C**N54TASLNP        COMP      192                                    54    + DPD
101800111129     C**N54TASLNP        COMP      193                                    54    + DPD
101900111129     C**N54TASLNP        COMP      194                                    54    + DPD
102000111129     C**N54TASLNP        COMP      195                                    54    + DPD
102100111129     C**N54TASLNP        COMP      196                                    54    + DPD
102200111129     C**N54TASLNP        COMP      197                                    54    + DPD
102300090204     C*
102400100402     C****     TASLNP        COMP      180                                    48     POSTE
102500090204     C*
102600111129     c                   endif
102700111129     c***
102800111129     c* L I N E A   D I   A R R  I V O
102900111129     c***
103000111129     c                   z-add     1             kk
103100111129     c     taslna        lookup    fil(kk)                                98
103200111129     c
103300111129     c                   if        *in98
103400111129     c* Euroexpress o Fedex o DPD
103500111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
103600111129     c                   seton                                        55
103700111129     c                   endif
103800111129     C*
103900111129     C**   TASLNA        COMP      300                                55
104000111129     C**N55TASLNA        COMP      190                                    55    + DPD
104100111129     C**N55TASLNA        COMP      191                                    55    + DPD
104200111129     C**N55TASLNA        COMP      192                                    55    + DPD
104300111129     C*+N55TASLNA        COMP      193                                    55    + DPD
104400111129     C**N55TASLNA        COMP      194                                    55    + DPD
104500111129     C**N55TASLNA        COMP      195                                    55    + DPD
104600111129     C**N55TASLNA        COMP      196                                    55    + DPD
104700111129     C*+N55TASLNA        COMP      197                                    55    + DPD
104800090204     C*
104900111129     c* Fedex
105000111129     c                   if        tif(kk) ='F'
105100111129     c                   seton                                        52
105200111129     c                   endif
105300111129     c
105400111129     C***  TASLNA        COMP      400                                    52     FEDEX
105500090204     C*
105600111129     c* DPD
105700111129     c                   if        tif(kk) ='D'
105800111129     c                   seton                                        56
105900111129     c                   endif
106000111129     c
106100111129     C**   TASLNA        COMP      190                                    56     DPD
106200111129     C**N56TASLNA        COMP      191                                    56     DPD
106300111129     C**N56TASLNA        COMP      192                                    56     DPD
106400111129     C**N56TASLNA        COMP      193                                    56     DPD
106500111129     C**N56TASLNA        COMP      194                                    56     DPD
106600111129     C**N56TASLNA        COMP      195                                    56     DPD
106700111129     C**N56TASLNA        COMP      196                                    56     DPD
106800111129     C**N56TASLNA        COMP      197                                    56     DPD
106900091006     c*
107000111129     c                   endif
107100111129     c
107200091006     c* Verifico se si tratta di cliente extra budget --> 35 on
107300110711     c                   if        §stat=' '
107400091006     c                   z-add     1             EE
107500091006     c                   if        tasccm<>savccm or tasctr<>savctr
107600091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
107700091007     c     w010a         lookup    ksc_EX(EE)                             35
107800091007     c                   if        not *in35
107900091007     c                   eval      %subst(w010a:8:3)='   '
108000091007     c     w010a         lookup    ksc_EX(EE)                             35
108100091006     c                   endif
108200091006
108300091006     c                   eval      savccm=tasccm
108400091006     c                   eval      savctr=tasctr
108500091006     c                   endif
108600110711     c
108700111028     c                   else
108800111028     c                   seton                                        35
108900110711     c*
109000111028     c**                 z-add     1             EE
109100111028     c**                 if        tasccm<>savccm
109200111028     c**                 eval      w010a=%editc(tasccm:'X')+'   '
109300111028     c**   w010a         lookup    ksc_EX(EE)                             35
109400111028     c**                 eval      savccm=tasccm
109500111028     c**                 endif
109600110711     c
109700110711     c                   endif
109800090204     C*
109900090204     C                   Z-ADD     1             K
110000090204     C     TASLNP        LOOKUP    FIL(K)                                 58
110100090204     C  N58              EXCEPT    ERR
110200090204     C     K             OCCUR     DSFIL
110300100402     C     K             OCCUR     DSFIL_e
110400100402     C     K             OCCUR     DSFIL_h
110500090204     C  N54
110600090204     CANN55              DO                                                     ITALIA
110700100402     c* Spedizioni LNP Italia LNA Italia
110800100402     c                   exsr      speITA
110900090204     C                   ENDDO
111000100402     c
111100100402     c* Spedizioni IMPORT --> LNP Italia
111200090204     C   54              DO                                                     IMPORT
111300100402     c                   exsr      speIMPORT
111400090204     C                   ENDDO
111500100402     c
111600100402     c* Spedizioni lna EUROEXPRESS
111700090204     C   55
111800090204     CANN56
111900090204     CANN52              DO                                                     EXPORT
112000100402     c                   exsr      speEXPRESS
112100090204     C                   ENDDO
112200100402     c
112300100402     c* Spedizioni lna DPD
112400090204     C   55
112500090204     CAN 56              DO                                                     EXPORT
112600100402     c                   exsr      speDPD
112700090204     C                   ENDDO
112800100402     c
112900100402     c* Spedizioni lna FEDEX
113000090204     C   55
113100090204     CAN 52              DO                                                     EXPORT
113200100402     c                   exsr      speFEDEX
113300090204     C                   ENDDO
113400100402     c
113500090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
113600090204     C                   Z-ADD     1             K
113700090204     C     TASLNA        LOOKUP    FIL(K)                                 58
113800090204     C  N58              EXCEPT    ERR
113900090204     C     K             OCCUR     DSFIL
114000100402     C     K             OCCUR     DSFIL_e
114100100402     C     K             OCCUR     DSFIL_h
114200100402     c
114300090204     C  N54
114400100402     CANN55              DO
114500100402     c* Spedizioni LNP Italia LNA Italia
114600100402     c                   exsr      speITA_LNA
114700090204     C                   ENDDO
114800100402     c
114900100402     c* spedizioni EXPORT --> lna estera
115000090204     C   55              DO                                                     ESTERO ASS.+FRA.
115100100402     c                   exsr      speEXPORT
115200090204     C                   ENDDO
115300090204     c                   endsr
115400090206     C**********************************************************
115500090206     C** scarica ds multiple su file
115600090206     C**********************************************************
115700090206     C     scrive_file   BEGSR
115800090206     c*
115900090206     C                   Z-ADD     §AL           WSSALL
116000090206     c*
116100110711    1C                   DO        400           K
116200090206     C     K             OCCUR     DSFIL_smp
116300090206     C     K             OCCUR     DSFIL
116400100402     C     K             OCCUR     DSFIL_e
116500100402     C     K             OCCUR     DSFIL_h
116600110711     c
116700110711    2c                   if        §stat=' '
116800090206     c* WFSMP
116900090206     c* giornaliero
117000090601     c* Lo scrivo solo per tipo "G" o "R"
117100090601     c                   if        Wtipo='G' or Wtipo='R'
117200090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
117300090206     c                              wmptspt_g>0 or wmptsat_g>0 or
117400090206     c                              wmptst_g>0)
117500090206     c                   z-add     §al           wmpdal
117600090206     c                   eval      wmptip='G'
117700090206     c                   exsr      rie_campi
117800090206     c                   write     wfsmp000
117900090206     c                   endif
118000090601     c                   endif
118100090601     c
118200090601     c* riepilogo /precedente
118300090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
118400090601     c                   if        Wtipo='P' or Wtipo='R'
118500090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
118600090206     c                              wmptspt_r>0 or wmptsat_r>0 or
118700090206     c                              wmptst_r>0)
118800090206     c                   z-add     §dal          wmpdal
118900090601     c                   if        Wtipo='R'
119000090206     c                   eval      wmptip='R'
119100090601     c                   else
119200090601     c                   eval      wmptip='P'
119300090601     c                   endif
119400090206     c                   exsr      rie_campi
119500090206     c                   write     wfsmp000
119600090206     c                   endif
119700090601     c                   endif
119800110711     c
119900110711    2c                   endif
120000090206     c* WSLSS50F
120100090206     c* giornaliero
120200090601     c                   if        Wtipo='G' or Wtipo='R'
120300110711     c
120400110711    2c                   if        §stat=' '
120500090601     c
120600090206     C     WTSPIP        IFGT      *ZERO
120700090206     C                   Z-ADD     §al           WSSDAL
120800090206     c                   movel     'G'           WSSTIE
120900090206     C                   MOVE      'IP'          WSSTIP
121000090206     C                   Z-ADD     FIL(K)        WSSLIN
121100090206     C                   Z-ADD     WTSPIP        WSSTSP
121200090206     C                   Z-ADD     WTCLIP        WSSTCL
121300090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
121400090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
121500100402     c* spedizioni "E"
121600100402     C                   Z-ADD     eWTSPIP       WSSTSPe
121700100402     C                   Z-ADD     eWTCLIP       WSSTCLe
121800100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
121900100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
122000100402     c* spedizioni "H"
122100100402     C                   Z-ADD     hWTSPIP       WSSTSPh
122200100402     C                   Z-ADD     hWTCLIP       WSSTCLh
122300100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
122400100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
122500100402
122600090206     C                   WRITE     WSLSS500
122700090206     C                   ENDIF
122800100402
122900090206     C     WTSPIA        IFGT      *ZERO
123000090206     C                   Z-ADD     §al           WSSDAL
123100090206     c                   movel     'G'           WSSTIE
123200090206     C                   MOVE      'IA'          WSSTIP
123300090206     C                   Z-ADD     FIL(K)        WSSLIN
123400090206     C                   Z-ADD     WTSPIA        WSSTSP
123500090206     C                   Z-ADD     WTCLIA        WSSTCL
123600090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
123700090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
123800100402     c* spedizioni "E"
123900100402     C                   Z-ADD     eWTSPIA       WSSTSPe
124000100402     C                   Z-ADD     eWTCLIA       WSSTCLe
124100100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
124200100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
124300100402     c* spedizioni "H"
124400100402     C                   Z-ADD     hWTSPIA       WSSTSPh
124500100402     C                   Z-ADD     hWTCLIA       WSSTCLh
124600100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
124700100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
124800100402     c
124900090206     C                   WRITE     WSLSS500
125000090206     C                   ENDIF
125100100402     c
125200090206     C     WTSPEI        IFGT      *ZERO
125300090206     C                   Z-ADD     §al           WSSDAL
125400090206     c                   movel     'G'           WSSTIE
125500090206     C                   MOVE      'EI'          WSSTIP
125600090206     C                   Z-ADD     FIL(K)        WSSLIN
125700090206     C                   Z-ADD     WTSPEI        WSSTSP
125800090206     C                   Z-ADD     WTCLEI        WSSTCL
125900090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
126000090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
126100100402     c* spedizioni "E"
126200100402     C                   Z-ADD     eWTSPEI       WSSTSPe
126300100402     C                   Z-ADD     eWTCLEI       WSSTCLe
126400100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
126500100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
126600100402     c* spedizioni "H"
126700100402     C                   Z-ADD     hWTSPEI       WSSTSPh
126800100402     C                   Z-ADD     hWTCLEI       WSSTCLh
126900100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
127000100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
127100100402     c
127200090206     C                   WRITE     WSLSS500
127300090206     C                   ENDIF
127400100402     c
127500090206     C     WTSPEE        IFGT      *ZERO
127600090206     C                   Z-ADD     §al           WSSDAL
127700090206     c                   movel     'G'           WSSTIE
127800090206     C                   MOVE      'EE'          WSSTIP
127900090206     C                   Z-ADD     FIL(K)        WSSLIN
128000090206     C                   Z-ADD     WTSPEE        WSSTSP
128100090206     C                   Z-ADD     WTCLEE        WSSTCL
128200090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
128300090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
128400100402     c* spedizioni "E"
128500100402     C                   Z-ADD     eWTSPEE       WSSTSPe
128600100402     C                   Z-ADD     eWTCLEE       WSSTCLe
128700100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
128800100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
128900100402     c* spedizioni "H"
129000100402     C                   Z-ADD     hWTSPEE       WSSTSPh
129100100402     C                   Z-ADD     hWTCLEE       WSSTCLh
129200100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
129300100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
129400100402     c
129500090206     C                   WRITE     WSLSS500
129600090206     C                   ENDIF
129700090206     C*
129800090206     C     WTSPED        IFGT      *ZERO
129900090206     C                   Z-ADD     §al           WSSDAL
130000090206     c                   movel     'G'           WSSTIE
130100090206     C                   MOVE      'ED'          WSSTIP
130200090206     C                   Z-ADD     FIL(K)        WSSLIN
130300090206     C                   Z-ADD     WTSPED        WSSTSP
130400090206     C                   Z-ADD     WTCLED        WSSTCL
130500090206     C                   Z-ADD(H)  WTKGED        WSSTKG
130600090206     C                   Z-ADD(H)  WTMCED        WSSTMC
130700100402     c* spedizioni "E"
130800100402     C                   Z-ADD     eWTSPED       WSSTSPe
130900100402     C                   Z-ADD     eWTCLED       WSSTCLe
131000100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
131100100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
131200100402     c* spedizioni "H"
131300100402     C                   Z-ADD     hWTSPED       WSSTSPh
131400100402     C                   Z-ADD     hWTCLED       WSSTCLh
131500100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
131600100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
131700100402     c
131800090206     C                   WRITE     WSLSS500
131900090206     C                   ENDIF
132000090206     C*
132100090206     C     WTSPEX        IFGT      *ZERO
132200090206     C                   Z-ADD     §al           WSSDAL
132300090206     c                   movel     'G'           WSSTIE
132400090206     C                   MOVE      'EX'          WSSTIP
132500090206     C                   Z-ADD     FIL(K)        WSSLIN
132600090206     C                   Z-ADD     WTSPEX        WSSTSP
132700090206     C                   Z-ADD     WTCLEX        WSSTCL
132800090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
132900090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
133000100402     c* spedizioni "E"
133100100402     C                   Z-ADD     eWTSPEX       WSSTSPe
133200100402     C                   Z-ADD     eWTCLEX       WSSTCLe
133300100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
133400100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
133500100402     c* spedizioni "H"
133600100402     C                   Z-ADD     hWTSPEX       WSSTSPh
133700100402     C                   Z-ADD     hWTCLEX       WSSTCLh
133800100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
133900100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
134000100402     c
134100090206     C                   WRITE     WSLSS500
134200090206     C                   ENDIF
134300090206     C*
134400090206     C     WTSPEA        IFGT      *ZERO
134500090206     C                   Z-ADD     §al           WSSDAL
134600090206     c                   movel     'G'           WSSTIE
134700090206     C                   MOVE      'EA'          WSSTIP
134800090206     C                   Z-ADD     FIL(K)        WSSLIN
134900090206     C                   Z-ADD     WTSPEA        WSSTSP
135000090206     C                   Z-ADD     WTCLEA        WSSTCL
135100090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
135200090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
135300100402     c* spedizioni "E"
135400100402     C                   Z-ADD     eWTSPEA       WSSTSPe
135500100402     C                   Z-ADD     eWTCLEA       WSSTCLe
135600100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
135700100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
135800100402     c* spedizioni "H"
135900100402     C                   Z-ADD     hWTSPEA       WSSTSPh
136000100402     C                   Z-ADD     hWTCLEA       WSSTCLh
136100100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
136200100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
136300100402     c
136400090206     C                   WRITE     WSLSS500
136500090206     C                   ENDIF
136600090206     C*
136700090206     C     WTSPPA        IFGT      *ZERO
136800090206     C                   Z-ADD     §al           WSSDAL
136900090206     c                   movel     'G'           WSSTIE
137000090206     C                   MOVE      'PA'          WSSTIP
137100090206     C                   Z-ADD     FIL(K)        WSSLIN
137200090206     C                   Z-ADD     WTSPPA        WSSTSP
137300090206     C                   Z-ADD     WTCLPA        WSSTCL
137400090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
137500090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
137600100402     c* spedizioni "E"
137700100402     C                   Z-ADD     eWTSPPA       WSSTSPe
137800100402     C                   Z-ADD     eWTCLPA       WSSTCLe
137900100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
138000100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
138100100402     c* spedizioni "H"
138200100402     C                   Z-ADD     hWTSPPA       WSSTSPh
138300100402     C                   Z-ADD     hWTCLPA       WSSTCLh
138400100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
138500100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
138600100402     c
138700090206     C                   WRITE     WSLSS500
138800090206     C                   ENDIF
138900110711     c
139000110711     c                   endif
139100110715     c* Extra budget o clienti particolari
139200110715     c                   if        §stat=' '
139300110715     c                   movel     'G'           WSSTIE
139400110715     c                   else
139500110715     c                   movel     'A'           WSSTIE
139600110715     c                   endif
139700110715     c
139800091006     C     WTSPIP_e      IFGT      *ZERO
139900091007     C                   Z-ADD     §al           WSSDAL
140000091006     C                   MOVE      'IP'          WSSTIP
140100091006     C                   Z-ADD     FIL(K)        WSSLIN
140200091006     C                   Z-ADD     WTSPIP_e      WSSTSP
140300091006     C                   Z-ADD     WTCLIP_e      WSSTCL
140400091006     C                   Z-ADD(H)  WTKGIP_e      WSSTKG
140500091006     C                   Z-ADD(H)  WTMCIP_e      WSSTMC
140600100402     c* spedizioni "E"
140700100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
140800100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
140900100402     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe
141000100402     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe
141100100402     c* spedizioni "H"
141200100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
141300100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
141400100402     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh
141500100402     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh
141600100402     c
141700091006     C                   WRITE     WSLSS550
141800091006     C                   ENDIF
141900100402     c
142000091006     C     WTSPIA_e      IFGT      *ZERO
142100091007     C                   Z-ADD     §al           WSSDAL
142200110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
142300110715     c*****              movel     'G'           WSSTIE
142400091006     C                   MOVE      'IA'          WSSTIP
142500091006     C                   Z-ADD     FIL(K)        WSSLIN
142600091006     C                   Z-ADD     WTSPIA_e      WSSTSP
142700091006     C                   Z-ADD     WTCLIA_e      WSSTCL
142800091006     C                   Z-ADD(H)  WTKGIA_e      WSSTKG
142900091006     C                   Z-ADD(H)  WTMCIA_e      WSSTMC
143000100402     c* spedizioni "E"
143100100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
143200100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
143300100402     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe
143400100402     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe
143500100402     c* spedizioni "H"
143600100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
143700100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
143800100402     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh
143900100402     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh
144000100402     c
144100091006     C                   WRITE     WSLSS550
144200091006     C                   ENDIF
144300100402     c
144400091006     C     WTSPEI_e      IFGT      *ZERO
144500091007     C                   Z-ADD     §al           WSSDAL
144600110715     c******             movel     'G'           WSSTIE
144700091006     C                   MOVE      'EI'          WSSTIP
144800091006     C                   Z-ADD     FIL(K)        WSSLIN
144900091006     C                   Z-ADD     WTSPEI_e      WSSTSP
145000091006     C                   Z-ADD     WTCLEI_e      WSSTCL
145100091006     C                   Z-ADD(H)  WTKGEI_e      WSSTKG
145200091006     C                   Z-ADD(H)  WTMCEI_e      WSSTMC
145300100402     c* spedizioni "E"
145400100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
145500100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
145600100402     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe
145700100402     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe
145800100402     c* spedizioni "H"
145900100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
146000100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
146100100402     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh
146200100402     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh
146300100402     c
146400091006     C                   WRITE     WSLSS550
146500091006     C                   ENDIF
146600100402     c
146700091006     C     WTSPEE_e      IFGT      *ZERO
146800091007     C                   Z-ADD     §al           WSSDAL
146900110715     c******             movel     'G'           WSSTIE
147000091006     C                   MOVE      'EE'          WSSTIP
147100091006     C                   Z-ADD     FIL(K)        WSSLIN
147200091006     C                   Z-ADD     WTSPEE_e      WSSTSP
147300091006     C                   Z-ADD     WTCLEE_e      WSSTCL
147400091006     C                   Z-ADD(H)  WTKGEE_e      WSSTKG
147500091006     C                   Z-ADD(H)  WTMCEE_e      WSSTMC
147600100402     c* spedizioni "E"
147700100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
147800100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
147900100402     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe
148000100402     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe
148100100402     c* spedizioni "H"
148200100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
148300100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
148400100402     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh
148500100402     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh
148600100402     c
148700091006     C                   WRITE     WSLSS550
148800091006     C                   ENDIF
148900091006     C*
149000091006     C     WTSPED_e      IFGT      *ZERO
149100091007     C                   Z-ADD     §al           WSSDAL
149200110715     c*******            movel     'G'           WSSTIE
149300091006     C                   MOVE      'ED'          WSSTIP
149400091006     C                   Z-ADD     FIL(K)        WSSLIN
149500091006     C                   Z-ADD     WTSPED_e      WSSTSP
149600091006     C                   Z-ADD     WTCLED_e      WSSTCL
149700091006     C                   Z-ADD(H)  WTKGED_e      WSSTKG
149800091006     C                   Z-ADD(H)  WTMCED_e      WSSTMC
149900100402     c* spedizioni "E"
150000100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
150100100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
150200100402     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe
150300100402     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe
150400100402     c* spedizioni "H"
150500100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
150600100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
150700100402     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh
150800100402     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh
150900100402     c
151000091006     C                   WRITE     WSLSS550
151100091006     C                   ENDIF
151200091006     C*
151300091006     C     WTSPEX_e      IFGT      *ZERO
151400091007     C                   Z-ADD     §al           WSSDAL
151500110715     c*******            movel     'G'           WSSTIE
151600091006     C                   MOVE      'EX'          WSSTIP
151700091006     C                   Z-ADD     FIL(K)        WSSLIN
151800091006     C                   Z-ADD     WTSPEX_e      WSSTSP
151900091006     C                   Z-ADD     WTCLEX_e      WSSTCL
152000091006     C                   Z-ADD(H)  WTKGEX_e      WSSTKG
152100091006     C                   Z-ADD(H)  WTMCEX_e      WSSTMC
152200100402     c* spedizioni "E"
152300100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
152400100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
152500100402     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe
152600100402     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe
152700100402     c* spedizioni "H"
152800100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
152900100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
153000100402     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh
153100100402     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh
153200100402     c
153300091006     C                   WRITE     WSLSS550
153400091006     C                   ENDIF
153500091006     C*
153600091006     C     WTSPEA_e      IFGT      *ZERO
153700091007     C                   Z-ADD     §al           WSSDAL
153800110715     c******             movel     'G'           WSSTIE
153900091006     C                   MOVE      'EA'          WSSTIP
154000091006     C                   Z-ADD     FIL(K)        WSSLIN
154100091006     C                   Z-ADD     WTSPEA_e      WSSTSP
154200091006     C                   Z-ADD     WTCLEA_e      WSSTCL
154300091006     C                   Z-ADD(H)  WTKGEA_e      WSSTKG
154400091006     C                   Z-ADD(H)  WTMCEA_e      WSSTMC
154500100402     c* spedizioni "E"
154600100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
154700100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
154800100402     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe
154900100402     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe
155000100402     c* spedizioni "H"
155100100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
155200100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
155300100402     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh
155400100402     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh
155500100402     c
155600091006     C                   WRITE     WSLSS550
155700091006     C                   ENDIF
155800091006     C*
155900091006     C     WTSPPA_e      IFGT      *ZERO
156000091007     C                   Z-ADD     §al           WSSDAL
156100110715     c*******            movel     'G'           WSSTIE
156200091006     C                   MOVE      'PA'          WSSTIP
156300091006     C                   Z-ADD     FIL(K)        WSSLIN
156400091006     C                   Z-ADD     WTSPPA_e      WSSTSP
156500091006     C                   Z-ADD     WTCLPA_e      WSSTCL
156600091006     C                   Z-ADD(H)  WTKGPA_e      WSSTKG
156700091006     C                   Z-ADD(H)  WTMCPA_e      WSSTMC
156800100402     c* spedizioni "E"
156900100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
157000100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
157100100402     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe
157200100402     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe
157300100402     c* spedizioni "H"
157400100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
157500100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
157600100402     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh
157700100402     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh
157800100402     c
157900091006     C                   WRITE     WSLSS550
158000091006     C                   ENDIF
158100091006
158200090601     C                   ENDIF
158300090206     c*
158400090206     c* riepilogo
158500090601     c                   if        Wtipo='P' or Wtipo='R'
158600110715     c                   if        §stat=' '
158700110715
158800090206     C     WTSPIP_r      IFGT      *ZERO
158900090206     C                   Z-ADD     §dal          WSSDAL
159000090601     c**                 movel     'R'           WSSTIE
159100090601     c                   movel     Wtipo         WSSTIE
159200090206     C                   MOVE      'IP'          WSSTIP
159300090206     C                   Z-ADD     FIL(K)        WSSLIN
159400090206     C                   Z-ADD     WTSPIP_r      WSSTSP
159500090206     C                   Z-ADD     WTCLIP_r      WSSTCL
159600090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
159700090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
159800100402     c* spedizioni "E"
159900100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
160000100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
160100100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
160200100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
160300100402     c* spedizioni "H"
160400100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
160500100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
160600100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
160700100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
160800100402     c
160900090206     C                   WRITE     WSLSS500
161000090206     C                   ENDIF
161100100402     c
161200090206     C     WTSPIA_r      IFGT      *ZERO
161300090206     C                   Z-ADD     §dal          WSSDAL
161400090601     c**                 movel     'R'           WSSTIE
161500090601     c                   movel     Wtipo         WSSTIE
161600090206     C                   MOVE      'IA'          WSSTIP
161700090206     C                   Z-ADD     FIL(K)        WSSLIN
161800090206     C                   Z-ADD     WTSPIA_r      WSSTSP
161900090206     C                   Z-ADD     WTCLIA_r      WSSTCL
162000090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
162100090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
162200100402     c* spedizioni "E"
162300100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
162400100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
162500100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
162600100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
162700100402     c* spedizioni "H"
162800100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
162900100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
163000100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
163100100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
163200100402     c
163300090206     C                   WRITE     WSLSS500
163400090206     C                   ENDIF
163500100402
163600090206     C     WTSPEI_R      IFGT      *ZERO
163700090206     C                   Z-ADD     §dal          WSSDAL
163800090601     c**                 movel     'R'           WSSTIE
163900090601     c                   movel     Wtipo         WSSTIE
164000090206     C                   MOVE      'EI'          WSSTIP
164100090206     C                   Z-ADD     FIL(K)        WSSLIN
164200090206     C                   Z-ADD     WTSPEI_r      WSSTSP
164300090206     C                   Z-ADD     WTCLEI_r      WSSTCL
164400090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
164500090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
164600100402     c* Spedizioni "E"
164700100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
164800100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
164900100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
165000100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
165100100402     c* Spedizioni "H"
165200100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
165300100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
165400100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
165500100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
165600100402     c
165700090206     C                   WRITE     WSLSS500
165800090206     C                   ENDIF
165900100402     c
166000090206     C     WTSPEE_r      IFGT      *ZERO
166100090206     C                   Z-ADD     §dal          WSSDAL
166200090601     c**                 movel     'R'           WSSTIE
166300090601     c                   movel     Wtipo         WSSTIE
166400090206     C                   MOVE      'EE'          WSSTIP
166500090206     C                   Z-ADD     FIL(K)        WSSLIN
166600090206     C                   Z-ADD     WTSPEE_r      WSSTSP
166700090206     C                   Z-ADD     WTCLEE_r      WSSTCL
166800090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
166900090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
167000100402     c* Spedizioni "E"
167100100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
167200100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
167300100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
167400100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
167500100402     c* Spedizioni "H"
167600100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
167700100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
167800100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
167900100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
168000100402     c
168100090206     C                   WRITE     WSLSS500
168200090206     C                   ENDIF
168300090206     C*
168400090206     C     WTSPED_r      IFGT      *ZERO
168500090206     C                   Z-ADD     §dal          WSSDAL
168600090601     c**                 movel     'R'           WSSTIE
168700090601     c                   movel     Wtipo         WSSTIE
168800090206     C                   MOVE      'ED'          WSSTIP
168900090206     C                   Z-ADD     FIL(K)        WSSLIN
169000090206     C                   Z-ADD     WTSPED_r      WSSTSP
169100090206     C                   Z-ADD     WTCLED_r      WSSTCL
169200090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
169300090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
169400100402     c* Spedizioni "E"
169500100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
169600100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
169700100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
169800100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
169900100402     c* Spedizioni "H"
170000100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
170100100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
170200100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
170300100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
170400100402     c
170500090206     C                   WRITE     WSLSS500
170600090206     C                   ENDIF
170700090206     C*
170800090206     C     WTSPEX_r      IFGT      *ZERO
170900090206     C                   Z-ADD     §dal          WSSDAL
171000090601     c**                 movel     'R'           WSSTIE
171100090601     c                   movel     Wtipo         WSSTIE
171200090206     C                   MOVE      'EX'          WSSTIP
171300090206     C                   Z-ADD     FIL(K)        WSSLIN
171400090206     C                   Z-ADD     WTSPEX_r      WSSTSP
171500090206     C                   Z-ADD     WTCLEX_r      WSSTCL
171600090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
171700090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
171800100402     c* Spedizioni "E"
171900100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
172000100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
172100100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
172200100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
172300100402     c* Spedizioni "H"
172400100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
172500100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
172600100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
172700100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
172800100402     c
172900090206     C                   WRITE     WSLSS500
173000090206     C                   ENDIF
173100090206     C*
173200090206     C     WTSPEA_r      IFGT      *ZERO
173300090206     C                   Z-ADD     §dal          WSSDAL
173400090601     c**                 movel     'R'           WSSTIE
173500090601     c                   movel     Wtipo         WSSTIE
173600090206     C                   MOVE      'EA'          WSSTIP
173700090206     C                   Z-ADD     FIL(K)        WSSLIN
173800090206     C                   Z-ADD     WTSPEA_r      WSSTSP
173900090206     C                   Z-ADD     WTCLEA_r      WSSTCL
174000090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
174100090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
174200100402     c* Spedizioni "E"
174300100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
174400100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
174500100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
174600100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
174700100402     c* Spedizioni "H"
174800100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
174900100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
175000100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
175100100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
175200100402     c
175300090206     C                   WRITE     WSLSS500
175400090206     C                   ENDIF
175500090206     C*
175600090206     C     WTSPPA_r      IFGT      *ZERO
175700090206     C                   Z-ADD     §dal          WSSDAL
175800090601     c**                 movel     'R'           WSSTIE
175900090601     c                   movel     Wtipo         WSSTIE
176000090206     C                   MOVE      'PA'          WSSTIP
176100090206     C                   Z-ADD     FIL(K)        WSSLIN
176200090206     C                   Z-ADD     WTSPPA_r      WSSTSP
176300090206     C                   Z-ADD     WTCLPA_r      WSSTCL
176400090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
176500090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
176600100402     c* Spedizioni "E"
176700100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
176800100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
176900100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
177000100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
177100100402     c* Spedizioni "H"
177200100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
177300100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
177400100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
177500100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
177600100402     c
177700090206     C                   WRITE     WSLSS500
177800090206     C                   ENDIF
177900110715     c
178000110715     c                   endif
178100110715     c
178200110715     c* Extra budget e clienti particolari
178300110715     c                   movel     wtipo         tipoC             1
178400110715     c                   if        §stat<>' ' and wtipo='R'
178500110715     c                   eval      tipoC='B'
178600110715     c                   endif
178700110715     c                   if        §stat<>' ' and wtipo='P'
178800110715     c                   eval      tipoC='C'
178900110715     c                   endif
179000110715     c
179100091006     C     WTSPIP_re     IFGT      *ZERO
179200091006     C                   Z-ADD     §dal          WSSDAL
179300110715     c                   movel     TipoC         WSSTIE
179400091006     C                   MOVE      'IP'          WSSTIP
179500091006     C                   Z-ADD     FIL(K)        WSSLIN
179600091006     C                   Z-ADD     WTSPIP_re     WSSTSP
179700091006     C                   Z-ADD     WTCLIP_re     WSSTCL
179800091006     C                   Z-ADD(H)  WTKGIP_re     WSSTKG
179900091006     C                   Z-ADD(H)  WTMCIP_re     WSSTMC
180000100402     c* Spedizioni "E"
180100100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
180200100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
180300100402     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe
180400100402     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe
180500100402     c* Spedizioni "H"
180600100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
180700100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
180800100402     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh
180900100402     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh
181000100402     c
181100091006     C                   WRITE     WSLSS550
181200091006     C                   ENDIF
181300100402     c
181400091006     C     WTSPIA_re     IFGT      *ZERO
181500091006     C                   Z-ADD     §dal          WSSDAL
181600110715     c                   movel     tipoC         WSSTIE
181700091006     C                   MOVE      'IA'          WSSTIP
181800091006     C                   Z-ADD     FIL(K)        WSSLIN
181900091006     C                   Z-ADD     WTSPIA_re     WSSTSP
182000091006     C                   Z-ADD     WTCLIA_re     WSSTCL
182100091006     C                   Z-ADD(H)  WTKGIA_re     WSSTKG
182200091006     C                   Z-ADD(H)  WTMCIA_re     WSSTMC
182300100402     c* Spedizioni "E"
182400100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
182500100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
182600100402     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe
182700100402     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe
182800100402     c* Spedizioni "H"
182900100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
183000100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
183100100402     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh
183200100402     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh
183300100402     c
183400091006     C                   WRITE     WSLSS550
183500091006     C                   ENDIF
183600100402     c
183700091006     C     WTSPEI_Re     IFGT      *ZERO
183800091006     C                   Z-ADD     §dal          WSSDAL
183900110715     c                   movel     tipoC         WSSTIE
184000091006     C                   MOVE      'EI'          WSSTIP
184100091006     C                   Z-ADD     FIL(K)        WSSLIN
184200091006     C                   Z-ADD     WTSPEI_re     WSSTSP
184300091006     C                   Z-ADD     WTCLEI_re     WSSTCL
184400091006     C                   Z-ADD(H)  WTKGEI_re     WSSTKG
184500091006     C                   Z-ADD(H)  WTMCEI_re     WSSTMC
184600100402     c* Spedizioni "E"
184700100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
184800100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
184900100402     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe
185000100402     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe
185100100402     c* Spedizioni "H"
185200100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
185300100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
185400100402     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh
185500100402     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh
185600100402     c
185700091006     C                   WRITE     WSLSS550
185800091006     C                   ENDIF
185900100402     c
186000091006     C     WTSPEE_re     IFGT      *ZERO
186100091006     C                   Z-ADD     §dal          WSSDAL
186200110715     c                   movel     tipoC         WSSTIE
186300091006     C                   MOVE      'EE'          WSSTIP
186400091006     C                   Z-ADD     FIL(K)        WSSLIN
186500091006     C                   Z-ADD     WTSPEE_re     WSSTSP
186600091006     C                   Z-ADD     WTCLEE_re     WSSTCL
186700091006     C                   Z-ADD(H)  WTKGEE_re     WSSTKG
186800091006     C                   Z-ADD(H)  WTMCEE_re     WSSTMC
186900100402     c* Spedizioni "E"
187000100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
187100100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
187200100402     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe
187300100402     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe
187400100402     c* Spedizioni "h"
187500100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
187600100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
187700100402     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh
187800100402     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh
187900100402     c
188000091006     C                   WRITE     WSLSS550
188100091006     C                   ENDIF
188200091006     C*
188300091006     C     WTSPED_re     IFGT      *ZERO
188400091006     C                   Z-ADD     §dal          WSSDAL
188500110715     c                   movel     tipoC         WSSTIE
188600091006     C                   MOVE      'ED'          WSSTIP
188700091006     C                   Z-ADD     FIL(K)        WSSLIN
188800091006     C                   Z-ADD     WTSPED_re     WSSTSP
188900091006     C                   Z-ADD     WTCLED_re     WSSTCL
189000091006     C                   Z-ADD(H)  WTKGED_re     WSSTKG
189100091006     C                   Z-ADD(H)  WTMCED_re     WSSTMC
189200100402     c* Spedizioni "E"
189300100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
189400100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
189500100402     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe
189600100402     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe
189700100402     c* Spedizioni "H"
189800100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
189900100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
190000100402     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh
190100100402     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh
190200100402     c
190300091006     C                   WRITE     WSLSS550
190400091006     C                   ENDIF
190500091006     C*
190600091006     C     WTSPEX_re     IFGT      *ZERO
190700091006     C                   Z-ADD     §dal          WSSDAL
190800110715     c                   movel     tipoC         WSSTIE
190900091006     C                   MOVE      'EX'          WSSTIP
191000091006     C                   Z-ADD     FIL(K)        WSSLIN
191100091006     C                   Z-ADD     WTSPEX_re     WSSTSP
191200091006     C                   Z-ADD     WTCLEX_re     WSSTCL
191300091006     C                   Z-ADD(H)  WTKGEX_re     WSSTKG
191400091006     C                   Z-ADD(H)  WTMCEX_re     WSSTMC
191500100402     c* Spedizioni "E"
191600100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
191700100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
191800100402     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe
191900100402     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe
192000100402     c* Spedizioni "H"
192100100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
192200100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
192300100402     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh
192400100402     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh
192500100402     c
192600091006     C                   WRITE     WSLSS550
192700091006     C                   ENDIF
192800091006     C*
192900091006     C     WTSPEA_re     IFGT      *ZERO
193000091006     C                   Z-ADD     §dal          WSSDAL
193100110715     c                   movel     tipoC         WSSTIE
193200091006     C                   MOVE      'EA'          WSSTIP
193300091006     C                   Z-ADD     FIL(K)        WSSLIN
193400091006     C                   Z-ADD     WTSPEA_re     WSSTSP
193500091006     C                   Z-ADD     WTCLEA_re     WSSTCL
193600091006     C                   Z-ADD(H)  WTKGEA_re     WSSTKG
193700091006     C                   Z-ADD(H)  WTMCEA_re     WSSTMC
193800100402     c* Spedizioni "E"
193900100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
194000100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
194100100402     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe
194200100402     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe
194300100402     c* Spedizioni "H"
194400100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
194500100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
194600100402     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh
194700100402     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh
194800100402     c
194900091006     C                   WRITE     WSLSS550
195000091006     C                   ENDIF
195100091006     C*
195200091006     C     WTSPPA_re     IFGT      *ZERO
195300091006     C                   Z-ADD     §dal          WSSDAL
195400110715     c                   movel     tipoC         WSSTIE
195500091006     C                   MOVE      'PA'          WSSTIP
195600091006     C                   Z-ADD     FIL(K)        WSSLIN
195700091006     C                   Z-ADD     WTSPPA_re     WSSTSP
195800091006     C                   Z-ADD     WTCLPA_re     WSSTCL
195900091006     C                   Z-ADD(H)  WTKGPA_re     WSSTKG
196000091006     C                   Z-ADD(H)  WTMCPA_re     WSSTMC
196100100402     c* Spedizioni "E"
196200100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
196300100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
196400100402     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe
196500100402     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe
196600100402     c* Spedizioni "H"
196700100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
196800100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
196900100402     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh
197000100402     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh
197100100402     c
197200091006     C                   WRITE     WSLSS550
197300091006     C                   ENDIF
197400091006     C                   ENDIF
197500100402
197600090206     c                   enddo
197700090206     c                   endsr
197800970527     C**********************************************************
197900970529     C** CARICAMENTO DATI INIZIALI
198000970527     C**********************************************************
198100970529     C     CARINZ        BEGSR
198200970905     C                   SETOFF                                       90
198300970529     C     KTAS          KLIST
198400970529     C                   KFLD                    WAAS
198500970529     C                   KFLD                    WMGS
198600970529     C     KTAB          KLIST
198700970529     C                   KFLD                    TBLKUT
198800970529     C                   KFLD                    TBLCOD
198900970529     C                   KFLD                    TBLKEY
199000970529     C     KTB           KLIST
199100970529     C                   KFLD                    TBLKUT
199200970529     C                   KFLD                    TBLCOD
199300970529     C                   Z-ADD     1             TBLKUT
199400090204     c     kwfsmp        klist
199500090204     c                   kfld                    k_fil
199600090204     c                   kfld                    k_tip
199700090204     c                   kfld                    k_dat
199800090205     C     kar5          KLIST
199900090205     C                   KFLD                    tasaas
200000090205     C                   KFLD                    taslnp
200100090205     C                   KFLD                    tasnrs
200200090205     C                   KFLD                    tasnsp
200300090205     C                   KFLD                    k_trd
200400090209     C                   KFLD                    tasflp
200500090915     c* apertura file di stampa congelato in SAVE
200600090915     C* ESEGUO OVRPRTF
200700090915     C                   Z-ADD     61            LUNG             15 5
200800090915     C                   MOVEL     *BLANKS       COMMAN           80
200900090915     C                   MOVEA     CMD           COMMAN
201000090915     C                   CALL      'QCMDEXC'                            H1
201100090915     C                   PARM                    COMMAN
201200090915     C                   PARM                    LUNG
201300090915     C*
201400090915     C                   OPEN      QSYSPRT
201500090915     C*
201600970529     C*
201700970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
201800970527     C** PULIZIA DS
201900980826     C                   DO        400           K                 3 0
202000970527     C     K             OCCUR     DSFIL
202100970527     C                   CLEAR                   DSFIL
202200970527     C                   END
202300100402     C                   DO        400           K                 3 0
202400100402     C     K             OCCUR     DSFIL_e
202500100402     C                   CLEAR                   DSFIL_e
202600100402     C                   END
202700100402     C                   DO        400           K                 3 0
202800100402     C     K             OCCUR     DSFIL_h
202900100402     C                   CLEAR                   DSFIL_h
203000100402     C                   END
203100090206     C                   DO        400           K                 3 0
203200090206     C     K             OCCUR     DSFIL_smp
203300090206     C                   CLEAR                   DSFIL_smp
203400090206     C                   END
203500970527     C*
203600970527     C                   Z-ADD     0             K
203700970527     C     *LOVAL        SETLL     AZORG01L
203800970527     C                   DO        *HIVAL
203900090204     C                   READ      AZORG01L
204000090204     c                   if        %eof(azorg01l)
204100090204     c                   leave
204200090204     c                   endif
204300970527     C     ORGFAG        IFEQ      'F'
204400970527     C     ORGFAG        OREQ      'A'
204500970527     C                   ADD       1             K
204600970527     C                   MOVEL     ORGFIL        FIL(K)
204700090204     C                   MOVEL     ORGdes        DIL(K)
204800090204     C                   z-add     ORGcar        car(K)
204900090204     c                   movel     orgde8        og148
205000090206     c                   if        §oglpo='1' or §oglpo='2'
205100090206     c                   move      §oglpo        lpo(K)
205200090206     c                   else
205300090206     c                   z-add     2             lpo(K)
205400090206     c                   endif
205500090204     c                   movel     orgde3        og143
205600090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
205700090209     c**                 move      'E'           tif(K)
205800090209     c**                 else
205900090209     c**                 move      'I'           tif(K)
206000090209     c**                 endif
206100090209     c                   select
206200090209     c                   when      §ogntw='DPD'
206300090209     c                   move      'D'           tif(K)
206400090209     c                   when      §ogntw='EEX'
206500090209     c                   move      'X'           tif(K)
206600090209     c                   when      §ogntw='FED'
206700090209     c                   move      'F'           tif(K)
206800090209     c                   other
206900090209     c                   move      'I'           tif(K)
207000090209     c                   endsl
207100970527     C                   ENDIF
207200090204     C                   ENDDO
207300970529     C*
207400970529     C                   Z-ADD     0             K
207500970529     C                   MOVEL     'TB'          TBLCOD
207600970529     C     KTB           SETLL     TABEL00F
207700970529     C                   DO        *HIVAL
207800090204     C     KTB           READE     TABEL00F
207900090204     c                   if        %eof(tabel00f)
208000090204     c                   leave
208100090204     c                   endif
208200970529     C     TBLFLG        IFNE      '*'
208300970529     C                   MOVEL     TBLUNI        DSTB
208400970529     C     §TBRBL        IFEQ      'N'
208500970529     C     §TBRBL        OREQ      'S'
208600970529     C                   ADD       1             K
208700970529     C                   MOVEL     TBLKEY        TBO(K)
208800970529     C                   ENDIF
208900970529     C                   ENDIF
209000090204     C                   ENDDO
209100091006     C*
209200091006     c* Lettura TABELLA clienti extra budget
209300110711     c                   if        §stat=' '
209400091006     C                   Z-ADD     0             K
209500091006     C                   MOVEL     'CXB'         TBecod
209600091006     C     tbecod        SETLL     tntbe01l
209700091006     C                   DO        *HIVAL
209800091006     C     tbecod        READE     tntbe01l
209900091006     c                   if        %eof(tntbe01l)
210000091006     c                   leave
210100091006     c                   endif
210200091006
210300091006     C     tbeatb        IFeq      ' '
210400091006     C                   MOVEL     TBeuni        Dcxb
210500091006     C                   ADD       1             K
210600091006     C                   MOVEL     TBeKE1        ksc_ex(k)
210700091007     C                   MOVE      §cxbctr       ksc_ex(k)
210800091006     C                   ENDIF
210900091006     C                   ENDDO
211000110711     c
211100110711     c                   else
211200110711     C                   Z-ADD     0             K
211300110711     C                   MOVEL     'CXQ'         TBecod
211400110711     C     tbecod        SETLL     tntbe01l
211500110711     C                   DO        *HIVAL
211600110711     C     tbecod        READE     tntbe01l
211700110711     c                   if        %eof(tntbe01l)
211800110711     c                   leave
211900110711     c                   endif
212000110711
212100110711     C     tbeatb        IFeq      ' '
212200110711     C                   ADD       1             K
212300110711     C                   MOVEL     TBeKE2        ksc_ex(k)
212400110711     C                   ENDIF
212500110711     C                   ENDDO
212600110711     c                   endif
212700110711     c*
212800110711     c* Controllo riempimento schiera
212900110715     c                   movel     kpjbu         savkpjbu
213000110711     c                   clear                   trul0sds
213100110711     c                   eval      i0stla='L'
213200110711     c                   eval      i0sski='KSC_EX'
213300110711     c                   eval      i0sele=%elem(ksc_ex)
213400110711     c                   eval      i0spie=k
213500110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
213600110711     c                   eval      i0ssif=knsif
213700110711     c                   eval      i0spgm='TNSSL3R2'
213800110711     c                   movel     trul0sds      kpjbu
213900110711     c                   call      'TRUL0SR'
214000110711     c                   parm                    kpjba
214100110715
214200110715     c                   movel     savkpjbu      kpjbu
214300090601     c
214400090204     c* data elaborazione nel formato AAAA/MM/GG
214500090204     c                   time                    dataiso
214600090204     c                   move      dataiso       wdatael
214700110711     c
214800110711     c                   if        §stat=' '
214900090601     c*
215000090601     c* Pulisco i file per i dati che devo rielaborare
215100090601     c                   eval      wrkgetlista =
215200090601     c                             'delete from WSLSS50F where wsstie= '''   +
215300090603     c                              Wtipo +  ''''
215400090601     c                   if        Wtipo='R'
215500090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
215600090603     c                              ' or  wsstie=''G'''
215700090601     c                   endif
215800090609     c*
215900090601     C/EXEC SQL
216000090601     C+ execute immediate :wrkgetlista
216100090601     C/END-EXEC
216200090609     c
216300090609     c* Pulisco i file per i dati che devo rielaborare
216400090609     c                   clear                   wrkgetlista
216500090609     c                   eval      wrkgetlista =
216600090609     c                             'delete from WFSMP00F where wmptip= '''   +
216700090609     c                              Wtipo +  ''''
216800090609     c                   if        Wtipo='R'
216900090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
217000090609     c                              ' or  wmptip=''G'''
217100090609     c                   endif
217200110711     c
217300110711     c                   endif
217400090609     c*
217500090609     C/EXEC SQL
217600090609     C+ execute immediate :wrkgetlista
217700090609     C/END-EXEC
217800091007     c*
217900091007     c* Pulisco il file clienti extra budget
218000091007     c                   clear                   wrkgetlista
218100110711     c
218200110711     c                   if        §stat=' '
218300091007     c                   eval      wrkgetlista =
218400091007     c                             'delete from WSLSS55F where wsstie= '''   +
218500091007     c                              Wtipo +  ''''
218600091007     c                   if        Wtipo='R'
218700091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
218800091007     c                              ' or  wsstie=''G'''
218900091007     c                   endif
219000111028     C/EXEC SQL
219100111028     C+ execute immediate :wrkgetlista
219200111028     C/END-EXEC
219300110711     c
219400110711     c                   else
219500111028     c
219600111028     c* Pulisco il file clienti particolari
219700110711     c                   select
219800110711     c                   when      Wtipo='R'
219900110711     c                   eval      wrkgetlista =
220000110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
220100110711     c                               wsstie= ''B'''
220200110711     c                   when      Wtipo='G'
220300110711     c                   eval      wrkgetlista =
220400110711     c                             'delete from WSLSS55F where wsstie= ''A'''
220500110711     c                   when      Wtipo='P'
220600110711     c                   eval      wrkgetlista =
220700110711     c                             'delete from WSLSS55F where wsstie= ''C'''
220800110711     c                   endsl
220900110711     c
221000091007     C/EXEC SQL
221100091007     C+ execute immediate :wrkgetlista
221200091007     C/END-EXEC
221300111028     c
221400111028     c* Preparo la lettura dei soli clienti particolari con SQL
221500111028     c                   clear                   wrkgetlista
221600111028     c                   eval      wrkgetlista =
221700111028     c                             'select * from titas00f where +
221800111028     c                             ((tasaas*10000) +tasmgs) >=' +
221900111028     c                             %editc(§dal:'X')+ ' and +
222000111028     c                             ((tasaas*10000) +tasmgs) <=' +
222100111028     c                             %editc(§al :'X')+ ' and tasccm in('
222200111028      * ciclo sulla sk dei cod bolla di recupero
222300111028     c                   Do        500           XX                3 0
222400111028     c                   If        ksc_ex(XX) = *blanks
222500111028     c                   Leave
222600111028     c                   EndIf
222700111028     c
222800111028     c                   If        XX >1
222900111028     c                   eval      wrkgetlista =
223000111028     c                             %trim(wrkgetlista) + ','
223100111028     c                   EndIf
223200111028     c
223300111028     c                   eval      wrkgetlista =
223400111028     c                             %trim(wrkgetlista) + ' ''' +
223500111028     c                             %subst(ksc_ex(xx):1:7) + ''''
223600111028     c                   EndDo
223700111028     c
223800111028     c                   Eval      wrkgetlista =
223900111028     c                             %trim(wrkgetlista) + ')'
224000111028     c
224100111028     c
224200111028     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
224300111028     c                             'select * from titas10f where +
224400111028     c                             ((tasaas*10000) +tasmgs) >=' +
224500111028     c                             %editc(§dal:'X')+ ' and +
224600111028     c                             ((tasaas*10000) +tasmgs) <=' +
224700111028     c                             %editc(§al :'X')+ ' and tasccm in('
224800111028      * ciclo sulla sk dei cod bolla di recupero
224900111028     c                   Do        500           XX                3 0
225000111028     c                   If        ksc_ex(XX) = *blanks
225100111028     c                   Leave
225200111028     c                   EndIf
225300111028     c
225400111028     c                   If        XX >1
225500111028     c                   eval      wrkgetlista =
225600111028     c                             %trim(wrkgetlista) + ','
225700111028     c                   EndIf
225800111028     c
225900111028     c                   eval      wrkgetlista =
226000111028     c                             %trim(wrkgetlista) + ' ''' +
226100111028     c                             %subst(ksc_ex(xx):1:7) + ''''
226200111028     c                   EndDo
226300111028     c
226400111028     c                   Eval      wrkgetlista =
226500111028     c                             %trim(wrkgetlista) + ')'
226600111028     c
226700111028     c                   EndIf
226800111028     c
226900970527     C                   ENDSR
227000100402     c****************************************************************************
227100100402     c     SpeITA        BEGSR
227200100402     c* giornaliero
227300100402     c                   if        (tasaas*10000+tasmgs)=§al
227400100402     C                   ADD       1             WTSPIP
227500100402     C                   ADD       TASNCL        WTCLIP
227600100402     C                   ADD       w_peso        WTKGIP
227700100402     C                   ADD       TASVLF        WTMCIP
227800100402     c* tipo servizio  "E"
227900100402     c                   if        tastsp='E'
228000100402     C                   ADD       1             eWTSPIP
228100100402     C                   ADD       TASNCL        eWTCLIP
228200100402     C                   ADD       w_peso        eWTKGIP
228300100402     C                   ADD       TASVLF        eWTMCIP
228400100402     c                   endif
228500100402     c* tipo servizio  "H"
228600100402     c                   if        tastsp='H'
228700100402     C                   ADD       1             hWTSPIP
228800100402     C                   ADD       TASNCL        hWTCLIP
228900100402     C                   ADD       w_peso        hWTKGIP
229000100402     C                   ADD       TASVLF        hWTMCIP
229100100402     c                   endif
229200100402     c**
229300100402     c* Extra budget
229400100402     c                   if        *in35
229500100402     C                   ADD       1             WTSPIP_e
229600100402     C                   ADD       TASNCL        WTCLIP_e
229700100402     C                   ADD       w_peso        WTKGIP_e
229800100402     C                   ADD       TASVLF        WTMCIP_e
229900100402     c* tipo servizio  "E"
230000100402     c                   if        tastsp='E'
230100100402     C                   ADD       1             eWTSPIP_e
230200100402     C                   ADD       TASNCL        eWTCLIP_e
230300100402     C                   ADD       w_peso        eWTKGIP_e
230400100402     C                   ADD       TASVLF        eWTMCIP_e
230500100402     c                   endif
230600100402     c* tipo servizio  "H"
230700100402     c                   if        tastsp='H'
230800100402     C                   ADD       1             hWTSPIP_e
230900100402     C                   ADD       TASNCL        hWTCLIP_e
231000100402     C                   ADD       w_peso        hWTKGIP_e
231100100402     C                   ADD       TASVLF        hWTMCIP_e
231200100402     c                   endif
231300100402
231400100402     c                   endif
231500100402     c                   endif
231600100402     c* riepilogo
231700100402     C                   ADD       1             WTSPIP_r
231800100402     C                   ADD       TASNCL        WTCLIP_r
231900100402     C                   ADD       w_peso        WTKGIP_r
232000100402     C                   ADD       TASVLF        WTMCIP_r
232100100402     c* tipo servizio  "E"
232200100402     c                   if        tastsp='E'
232300100402     C                   ADD       1             eWTSPIP_r
232400100402     C                   ADD       TASNCL        eWTCLIP_r
232500100402     C                   ADD       w_peso        eWTKGIP_r
232600100402     C                   ADD       TASVLF        eWTMCIP_r
232700100402     c                   endif
232800100402     c* tipo servizio  "H"
232900100402     c                   if        tastsp='H'
233000100402     C                   ADD       1             hWTSPIP_r
233100100402     C                   ADD       TASNCL        hWTCLIP_r
233200100402     C                   ADD       w_peso        hWTKGIP_r
233300100402     C                   ADD       TASVLF        hWTMCIP_r
233400100402     c                   endif
233500100402     c* Extra budget
233600100402     c                   if        *in35
233700100402     C                   ADD       1             WTSPIP_re
233800100402     C                   ADD       TASNCL        WTCLIP_re
233900100402     C                   ADD       w_peso        WTKGIP_re
234000100402     C                   ADD       TASVLF        WTMCIP_re
234100100402     c* tipo servizio  "E"
234200100402     c                   if        tastsp='E'
234300100402     C                   ADD       1             eWTSPIP_re
234400100402     C                   ADD       TASNCL        eWTCLIP_re
234500100402     C                   ADD       w_peso        eWTKGIP_re
234600100402     C                   ADD       TASVLF        eWTMCIP_re
234700100402     c                   endif
234800100402     c* tipo servizio  "H"
234900100402     c                   if        tastsp='H'
235000100402     C                   ADD       1             hWTSPIP_re
235100100402     C                   ADD       TASNCL        hWTCLIP_re
235200100402     C                   ADD       w_peso        hWTKGIP_re
235300100402     C                   ADD       TASVLF        hWTMCIP_re
235400100402     c                   endif
235500100402     c
235600100402     c                   endif
235700100402     c                   ENDSR
235800100402     c****************************************************************************
235900100402     c     SpeIMPORT     BEGSR
236000100402     c* giornaliero
236100100402     c                   if        (tasaas*10000+tasmgs)=§al
236200100402     C                   ADD       1             WTSPEI
236300100402     C                   ADD       TASNCL        WTCLEI
236400100402     C                   ADD       w_peso        WTKGEI
236500100402     C                   ADD       TASVLF        WTMCEI
236600100402     c* tipo servizio  "E"
236700100402     c                   if        tastsp='E'
236800100402     C                   ADD       1             eWTSPEI
236900100402     C                   ADD       TASNCL        eWTCLEI
237000100402     C                   ADD       w_peso        eWTKGEI
237100100402     C                   ADD       TASVLF        eWTMCEI
237200100402     c                   endif
237300100402     c* tipo servizio  "H"
237400100402     c                   if        tastsp='H'
237500100402     C                   ADD       1             hWTSPEI
237600100402     C                   ADD       TASNCL        hWTCLEI
237700100402     C                   ADD       w_peso        hWTKGEI
237800100402     C                   ADD       TASVLF        hWTMCEI
237900100402     c                   endif
238000100402     c* Extra Budget
238100100402     c                   if        *in35
238200100402     C                   ADD       1             WTSPEI_e
238300100402     C                   ADD       TASNCL        WTCLEI_e
238400100402     C                   ADD       w_peso        WTKGEI_e
238500100402     C                   ADD       TASVLF        WTMCEI_e
238600100402     c* tipo servizio  "E"
238700100402     c                   if        tastsp='E'
238800100402     C                   ADD       1             eWTSPEI_e
238900100402     C                   ADD       TASNCL        eWTCLEI_e
239000100402     C                   ADD       w_peso        eWTKGEI_e
239100100402     C                   ADD       TASVLF        eWTMCEI_e
239200100402     c                   endif
239300100402     c* tipo servizio  "H"
239400100402     c                   if        tastsp='H'
239500100402     C                   ADD       1             hWTSPEI_e
239600100402     C                   ADD       TASNCL        hWTCLEI_e
239700100402     C                   ADD       w_peso        hWTKGEI_e
239800100402     C                   ADD       TASVLF        hWTMCEI_e
239900100402     c                   endif
240000100402     c
240100100402     c                   endif
240200100402     c                   endif
240300100402     c* riepilogo
240400100402     C                   ADD       1             WTSPEI_r
240500100402     C                   ADD       TASNCL        WTCLEI_r
240600100402     C                   ADD       w_peso        WTKGEI_r
240700100402     C                   ADD       TASVLF        WTMCEI_r
240800100402     c* tipo servizio  "E"
240900100402     c                   if        tastsp='E'
241000100402     C                   ADD       1             eWTSPEI_r
241100100402     C                   ADD       TASNCL        eWTCLEI_r
241200100402     C                   ADD       w_peso        eWTKGEI_r
241300100402     C                   ADD       TASVLF        eWTMCEI_r
241400100402     c                   endif
241500100402     c* tipo servizio  "H"
241600100402     c                   if        tastsp='H'
241700100402     C                   ADD       1             hWTSPEI_r
241800100402     C                   ADD       TASNCL        hWTCLEI_r
241900100402     C                   ADD       w_peso        hWTKGEI_r
242000100402     C                   ADD       TASVLF        hWTMCEI_r
242100100402     c                   endif
242200100402     c* extra budget
242300100402     c                   if        *in35
242400100402     C                   ADD       1             WTSPEI_re
242500100402     C                   ADD       TASNCL        WTCLEI_re
242600100402     C                   ADD       w_peso        WTKGEI_re
242700100402     C                   ADD       TASVLF        WTMCEI_re
242800100402     c* tipo servizio  "E"
242900100402     c                   if        tastsp='E'
243000100402     C                   ADD       1             eWTSPEI_re
243100100402     C                   ADD       TASNCL        eWTCLEI_re
243200100402     C                   ADD       w_peso        eWTKGEI_re
243300100402     C                   ADD       TASVLF        eWTMCEI_re
243400100402     c                   endif
243500100402     c* tipo servizio  "H"
243600100402     c                   if        tastsp='H'
243700100402     C                   ADD       1             hWTSPEI_re
243800100402     C                   ADD       TASNCL        hWTCLEI_re
243900100402     C                   ADD       w_peso        hWTKGEI_re
244000100402     C                   ADD       TASVLF        hWTMCEI_re
244100100402     c                   endif
244200100402     c                   endif
244300100402     c                   ENDSR
244400100402     c****************************************************************************
244500100402     c     speEXPRESS    BEGSR
244600100402     c* giornaliero
244700100402     c                   if        (tasaas*10000+tasmgs)=§al
244800100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
244900100402     C                   ADD       TASNCL        WTCLEE
245000100402     C                   ADD       w_peso        WTKGEE
245100100402     C                   ADD       TASVLF        WTMCEE
245200100402     c* tipo servizio  "E"
245300100402     c                   if        tastsp='E'
245400100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
245500100402     C                   ADD       TASNCL        eWTCLEE
245600100402     C                   ADD       w_peso        eWTKGEE
245700100402     C                   ADD       TASVLF        eWTMCEE
245800100402     c                   endif
245900100402     c* tipo servizio  "H"
246000100402     c                   if        tastsp='H'
246100100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
246200100402     C                   ADD       TASNCL        hWTCLEE
246300100402     C                   ADD       w_peso        hWTKGEE
246400100402     C                   ADD       TASVLF        hWTMCEE
246500100402     c                   endif
246600100402     c* extra budget
246700100402     c                   if        *in35
246800100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
246900100402     C                   ADD       TASNCL        WTCLEE_e
247000100402     C                   ADD       w_peso        WTKGEE_e
247100100402     C                   ADD       TASVLF        WTMCEE_e
247200100402     c* tipo servizio  "E"
247300100402     c                   if        tastsp='E'
247400100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
247500100402     C                   ADD       TASNCL        eWTCLEE_e
247600100402     C                   ADD       w_peso        eWTKGEE_e
247700100402     C                   ADD       TASVLF        eWTMCEE_e
247800100402     c                   endif
247900100402     c* tipo servizio  "H"
248000100402     c                   if        tastsp='H'
248100100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
248200100402     C                   ADD       TASNCL        hWTCLEE_e
248300100402     C                   ADD       w_peso        hWTKGEE_e
248400100402     C                   ADD       TASVLF        hWTMCEE_e
248500100402     c                   endif
248600100402     c
248700100402     c                   endif
248800100402     c                   endif
248900100402     c* riepilogo
249000100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
249100100402     C                   ADD       TASNCL        WTCLEE_r
249200100402     C                   ADD       w_peso        WTKGEE_r
249300100402     C                   ADD       TASVLF        WTMCEE_r
249400100402     c* tipo servizio  "E"
249500100402     c                   if        tastsp='E'
249600100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
249700100402     C                   ADD       TASNCL        eWTCLEE_r
249800100402     C                   ADD       w_peso        eWTKGEE_r
249900100402     C                   ADD       TASVLF        eWTMCEE_r
250000100402     c                   endif
250100100402     c* tipo servizio  "H"
250200100402     c                   if        tastsp='H'
250300100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
250400100402     C                   ADD       TASNCL        hWTCLEE_r
250500100402     C                   ADD       w_peso        hWTKGEE_r
250600100402     C                   ADD       TASVLF        hWTMCEE_r
250700100402     c                   endif
250800100402     c* extra budget
250900100402     c                   if        *in35
251000100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
251100100402     C                   ADD       TASNCL        WTCLEE_re
251200100402     C                   ADD       w_peso        WTKGEE_re
251300100402     C                   ADD       TASVLF        WTMCEE_re
251400100402     c* tipo servizio  "E"
251500100402     c                   if        tastsp='E'
251600100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
251700100402     C                   ADD       TASNCL        eWTCLEE_re
251800100402     C                   ADD       w_peso        eWTKGEE_re
251900100402     C                   ADD       TASVLF        eWTMCEE_re
252000100402     c                   endif
252100100402     c* tipo servizio  "H"
252200100402     c                   if        tastsp='H'
252300100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
252400100402     C                   ADD       TASNCL        hWTCLEE_re
252500100402     C                   ADD       w_peso        hWTKGEE_re
252600100402     C                   ADD       TASVLF        hWTMCEE_re
252700100402     c                   endif
252800100402     c                   endif
252900100402     c
253000100402     c                   ENDSR
253100100402     c****************************************************************************
253200100402     c     speDPD        BEGSR
253300100402     c* giornaliero
253400100402     c                   if        (tasaas*10000+tasmgs)=§al
253500100402     C                   ADD       1             WTSPED                         DPD
253600100402     C                   ADD       TASNCL        WTCLED
253700100402     C                   ADD       w_peso        WTKGED
253800100402     C                   ADD       TASVLF        WTMCED
253900100402     c* tipo servizio  "E"
254000100402     c                   if        tastsp='E'
254100100402     C                   ADD       1             eWTSPED                         DPD
254200100402     C                   ADD       TASNCL        eWTCLED
254300100402     C                   ADD       w_peso        eWTKGED
254400100402     C                   ADD       TASVLF        eWTMCED
254500100402     c                   endif
254600100402     c* tipo servizio  "H"
254700100402     c                   if        tastsp='H'
254800100402     C                   ADD       1             hWTSPED                         DPD
254900100402     C                   ADD       TASNCL        hWTCLED
255000100402     C                   ADD       w_peso        hWTKGED
255100100402     C                   ADD       TASVLF        hWTMCED
255200100402     c                   endif
255300100402     c* extra budget
255400100402     c                   if        *in35
255500100402     C                   ADD       1             WTSPED_e                       DPD
255600100402     C                   ADD       TASNCL        WTCLED_e
255700100402     C                   ADD       w_peso        WTKGED_e
255800100402     C                   ADD       TASVLF        WTMCED_e
255900100402     c* tipo servizio  "E"
256000100402     c                   if        tastsp='E'
256100100402     C                   ADD       1             eWTSPED_e                       DPD
256200100402     C                   ADD       TASNCL        eWTCLED_e
256300100402     C                   ADD       w_peso        eWTKGED_e
256400100402     C                   ADD       TASVLF        eWTMCED_e
256500100402     c                   endif
256600100402     c* tipo servizio  "H"
256700100402     c                   if        tastsp='H'
256800100402     C                   ADD       1             hWTSPED_e                       DPD
256900100402     C                   ADD       TASNCL        hWTCLED_e
257000100402     C                   ADD       w_peso        hWTKGED_e
257100100402     C                   ADD       TASVLF        hWTMCED_e
257200100402     c                   endif
257300100402     c
257400100402     c                   endif
257500100402     c                   endif
257600100402     c* riepilogo
257700100402     C                   ADD       1             WTSPED_r                       DPD
257800100402     C                   ADD       TASNCL        WTCLED_r
257900100402     C                   ADD       w_peso        WTKGED_r
258000100402     C                   ADD       TASVLF        WTMCED_r
258100100402     c* tipo servizio  "E"
258200100402     c                   if        tastsp='E'
258300100402     C                   ADD       1             eWTSPED_r                       DPD
258400100402     C                   ADD       TASNCL        eWTCLED_r
258500100402     C                   ADD       w_peso        eWTKGED_r
258600100402     C                   ADD       TASVLF        eWTMCED_r
258700100402     c                   endif
258800100402     c* tipo servizio  "H"
258900100402     c                   if        tastsp='H'
259000100402     C                   ADD       1             hWTSPED_r                       DPD
259100100402     C                   ADD       TASNCL        hWTCLED_r
259200100402     C                   ADD       w_peso        hWTKGED_r
259300100402     C                   ADD       TASVLF        hWTMCED_r
259400100402     c                   endif
259500100402     c* extra budget
259600100402     c                   if        *in35
259700100402     C                   ADD       1             WTSPED_re                       DPD
259800100402     C                   ADD       TASNCL        WTCLED_re
259900100402     C                   ADD       w_peso        WTKGED_re
260000100402     C                   ADD       TASVLF        WTMCED_re
260100100402     c* tipo servizio  "E"
260200100402     c                   if        tastsp='E'
260300100402     C                   ADD       1             eWTSPED_re                       DPD
260400100402     C                   ADD       TASNCL        eWTCLED_re
260500100402     C                   ADD       w_peso        eWTKGED_re
260600100402     C                   ADD       TASVLF        eWTMCED_re
260700100402     c                   endif
260800100402     c* tipo servizio  "H"
260900100402     c                   if        tastsp='H'
261000100402     C                   ADD       1             hWTSPED_re                       DPD
261100100402     C                   ADD       TASNCL        hWTCLED_re
261200100402     C                   ADD       w_peso        hWTKGED_re
261300100402     C                   ADD       TASVLF        hWTMCED_re
261400100402     c                   endif
261500100402     c                   endif
261600100402     c                   ENDSR
261700100402     c****************************************************************************
261800100402     c     speFEDEX      BEGSR
261900100402     c* giornaliero
262000100402     c                   if        (tasaas*10000+tasmgs)=§al
262100100402     C                   ADD       1             WTSPEX                         FEDEX
262200100402     C                   ADD       TASNCL        WTCLEX
262300100402     C                   ADD       w_peso        WTKGEX
262400100402     C                   ADD       TASVLF        WTMCEX
262500100402     c* tipo servizio  "E"
262600100402     c                   if        tastsp='E'
262700100402     C                   ADD       1             eWTSPEX                         FEDEX
262800100402     C                   ADD       TASNCL        eWTCLEX
262900100402     C                   ADD       w_peso        eWTKGEX
263000100402     C                   ADD       TASVLF        eWTMCEX
263100100402     c                   endif
263200100402     c* tipo servizio  "H"
263300100402     c                   if        tastsp='H'
263400100402     C                   ADD       1             hWTSPEX                         FEDEX
263500100402     C                   ADD       TASNCL        hWTCLEX
263600100402     C                   ADD       w_peso        hWTKGEX
263700100402     C                   ADD       TASVLF        hWTMCEX
263800100402     c                   endif
263900100402     c* extra budget
264000100402     c                   if        *in35
264100100402     C                   ADD       1             WTSPEX_e                       FEDEX
264200100402     C                   ADD       TASNCL        WTCLEX_e
264300100402     C                   ADD       w_peso        WTKGEX_e
264400100402     C                   ADD       TASVLF        WTMCEX_e
264500100402     c* tipo servizio  "E"
264600100402     c                   if        tastsp='E'
264700100402     C                   ADD       1             eWTSPEX_e                       FEDEX
264800100402     C                   ADD       TASNCL        eWTCLEX_e
264900100402     C                   ADD       w_peso        eWTKGEX_e
265000100402     C                   ADD       TASVLF        eWTMCEX_e
265100100402     c                   endif
265200100402     c* tipo servizio  "H"
265300100402     c                   if        tastsp='H'
265400100402     C                   ADD       1             hWTSPEX_e                       FEDEX
265500100402     C                   ADD       TASNCL        hWTCLEX_e
265600100402     C                   ADD       w_peso        hWTKGEX_e
265700100402     C                   ADD       TASVLF        hWTMCEX_e
265800100402     c                   endif
265900100402     c                   endif
266000100402     c                   endif
266100100402     c* riepilogo
266200100402     C                   ADD       1             WTSPEX_r                       FEDEX
266300100402     C                   ADD       TASNCL        WTCLEX_r
266400100402     C                   ADD       w_peso        WTKGEX_r
266500100402     C                   ADD       TASVLF        WTMCEX_r
266600100402     c* tipo servizio  "E"
266700100402     c                   if        tastsp='E'
266800100402     C                   ADD       1             eWTSPEX_r                       FEDEX
266900100402     C                   ADD       TASNCL        eWTCLEX_r
267000100402     C                   ADD       w_peso        eWTKGEX_r
267100100402     C                   ADD       TASVLF        eWTMCEX_r
267200100402     c                   endif
267300100402     c* tipo servizio  "H"
267400100402     c                   if        tastsp='H'
267500100402     C                   ADD       1             hWTSPEX_r                       FEDEX
267600100402     C                   ADD       TASNCL        hWTCLEX_r
267700100402     C                   ADD       w_peso        hWTKGEX_r
267800100402     C                   ADD       TASVLF        hWTMCEX_r
267900100402     c                   endif
268000100402     c* Extra budget
268100100402     c                   if        *in35
268200100402     C                   ADD       1             WTSPEX_re                       FEDEX
268300100402     C                   ADD       TASNCL        WTCLEX_re
268400100402     C                   ADD       w_peso        WTKGEX_re
268500100402     C                   ADD       TASVLF        WTMCEX_re
268600100402     c* tipo servizio  "E"
268700100402     c                   if        tastsp='E'
268800100402     C                   ADD       1             eWTSPEX_re                       FEDEX
268900100402     C                   ADD       TASNCL        eWTCLEX_re
269000100402     C                   ADD       w_peso        eWTKGEX_re
269100100402     C                   ADD       TASVLF        eWTMCEX_re
269200100402     c                   endif
269300100402     c* tipo servizio  "H"
269400100402     c                   if        tastsp='H'
269500100402     C                   ADD       1             hWTSPEX_re                       FEDEX
269600100402     C                   ADD       TASNCL        hWTCLEX_re
269700100402     C                   ADD       w_peso        hWTKGEX_re
269800100402     C                   ADD       TASVLF        hWTMCEX_re
269900100402     c                   endif
270000100402     c
270100100402     c                   endif
270200100402     c                   ENDSR
270300100402     c****************************************************************************
270400100402     c     speITA_LNA    BEGSR
270500100402     c* giornaliero
270600100402     c                   if        (tasaas*10000+tasmgs)=§al
270700100402     C                   ADD       1             WTSPIA
270800100402     C                   ADD       TASNCL        WTCLIA
270900100402     C                   ADD       W_peso        WTKGIA
271000100402     C                   ADD       TASVLF        WTMCIA
271100100402     c* tipo servizio  "E"
271200100402     c                   if        tastsp='E'
271300100402     C                   ADD       1             eWTSPIA
271400100402     C                   ADD       TASNCL        eWTCLIA
271500100402     C                   ADD       W_peso        eWTKGIA
271600100402     C                   ADD       TASVLF        eWTMCIA
271700100402     c                   endif
271800100402     c* tipo servizio  "H"
271900100402     c                   if        tastsp='H'
272000100402     C                   ADD       1             hWTSPIA
272100100402     C                   ADD       TASNCL        hWTCLIA
272200100402     C                   ADD       W_peso        hWTKGIA
272300100402     C                   ADD       TASVLF        hWTMCIA
272400100402     c                   endif
272500100402     c* extra budget
272600100402     c                   if        *in35
272700100402     C                   ADD       1             WTSPIA_e
272800100402     C                   ADD       TASNCL        WTCLIA_e
272900100402     C                   ADD       w_peso        WTKGIA_e
273000100402     C                   ADD       TASVLF        WTMCIA_e
273100100402     c* tipo servizio  "E"
273200100402     c                   if        tastsp='E'
273300100402     C                   ADD       1             eWTSPIA_e
273400100402     C                   ADD       TASNCL        eWTCLIA_e
273500100402     C                   ADD       w_peso        eWTKGIA_e
273600100402     C                   ADD       TASVLF        eWTMCIA_e
273700100402     c                   endif
273800100402     c* tipo servizio  "H"
273900100402     c                   if        tastsp='H'
274000100402     C                   ADD       1             hWTSPIA_e
274100100402     C                   ADD       TASNCL        hWTCLIA_e
274200100402     C                   ADD       w_peso        hWTKGIA_e
274300100402     C                   ADD       TASVLF        hWTMCIA_e
274400100402     c                   endif
274500100402     c
274600100402     c                   endif
274700100402     c                   endif
274800100402     c* riepilogo
274900100402     C                   ADD       1             WTSPIA_r
275000100402     C                   ADD       TASNCL        WTCLIA_r
275100100402     C                   ADD       w_peso        WTKGIA_r
275200100402     C                   ADD       TASVLF        WTMCIA_r
275300100402     c* tipo servizio  "E"
275400100402     c                   if        tastsp='E'
275500100402     C                   ADD       1             eWTSPIA_r
275600100402     C                   ADD       TASNCL        eWTCLIA_r
275700100402     C                   ADD       w_peso        eWTKGIA_r
275800100402     C                   ADD       TASVLF        eWTMCIA_r
275900100402     c                   endif
276000100402     c* tipo servizio  "H"
276100100402     c                   if        tastsp='H'
276200100402     C                   ADD       1             hWTSPIA_r
276300100402     C                   ADD       TASNCL        hWTCLIA_r
276400100402     C                   ADD       w_peso        hWTKGIA_r
276500100402     C                   ADD       TASVLF        hWTMCIA_r
276600100402     c                   endif
276700100402     c* extra budget
276800100402     c                   if        *in35
276900100402     C                   ADD       1             WTSPIA_re
277000100402     C                   ADD       TASNCL        WTCLIA_re
277100100402     C                   ADD       w_peso        WTKGIA_re
277200100402     C                   ADD       TASVLF        WTMCIA_re
277300100402     c* tipo servizio  "E"
277400100402     c                   if        tastsp='E'
277500100402     C                   ADD       1             eWTSPIA_re
277600100402     C                   ADD       TASNCL        eWTCLIA_re
277700100402     C                   ADD       w_peso        eWTKGIA_re
277800100402     C                   ADD       TASVLF        eWTMCIA_re
277900100402     c                   endif
278000100402     c* tipo servizio  "H"
278100100402     c                   if        tastsp='H'
278200100402     C                   ADD       1             hWTSPIA_re
278300100402     C                   ADD       TASNCL        hWTCLIA_re
278400100402     C                   ADD       w_peso        hWTKGIA_re
278500100402     C                   ADD       TASVLF        hWTMCIA_re
278600100402     c                   endif
278700100402     c                   endif
278800100402     c                   ENDSR
278900100402     c****************************************************************************
279000100402     c     speEXPORT     BEGSR
279100100402     c* giornaliero
279200100402     c                   if        (tasaas*10000+tasmgs)=§al
279300100402     C                   ADD       1             WTSPEA
279400100402     C                   ADD       TASNCL        WTCLEA
279500100402     C                   ADD       w_peso        WTKGEA
279600100402     C                   ADD       TASVLF        WTMCEA
279700100402     c* tipo servizio  "E"
279800100402     c                   if        tastsp='E'
279900100402     C                   ADD       1             eWTSPEA
280000100402     C                   ADD       TASNCL        eWTCLEA
280100100402     C                   ADD       w_peso        eWTKGEA
280200100402     C                   ADD       TASVLF        eWTMCEA
280300100402     c                   endif
280400100402     c* tipo servizio  "H"
280500100402     c                   if        tastsp='H'
280600100402     C                   ADD       1             hWTSPEA
280700100402     C                   ADD       TASNCL        hWTCLEA
280800100402     C                   ADD       w_peso        hWTKGEA
280900100402     C                   ADD       TASVLF        hWTMCEA
281000100402     c                   endif
281100100402     c* extra budget
281200100402     c                   if        *in35
281300100402     C                   ADD       1             WTSPEA_e
281400100402     C                   ADD       TASNCL        WTCLEA_e
281500100402     C                   ADD       w_peso        WTKGEA_e
281600100402     C                   ADD       TASVLF        WTMCEA_e
281700100402     c* tipo servizio  "E"
281800100402     c                   if        tastsp='E'
281900100402     C                   ADD       1             eWTSPEA_e
282000100402     C                   ADD       TASNCL        eWTCLEA_e
282100100402     C                   ADD       w_peso        eWTKGEA_e
282200100402     C                   ADD       TASVLF        eWTMCEA_e
282300100402     c                   endif
282400100402     c* tipo servizio  "H"
282500100402     c                   if        tastsp='H'
282600100402     C                   ADD       1             hWTSPEA_e
282700100402     C                   ADD       TASNCL        hWTCLEA_e
282800100402     C                   ADD       w_peso        hWTKGEA_e
282900100402     C                   ADD       TASVLF        hWTMCEA_e
283000100402     c                   endif
283100100402     c
283200100402     c                   endif
283300100402     c                   endif
283400100402     c* riepilogo
283500100402     C                   ADD       1             WTSPEA_r
283600100402     C                   ADD       TASNCL        WTCLEA_r
283700100402     C                   ADD       w_peso        WTKGEA_r
283800100402     C                   ADD       TASVLF        WTMCEA_r
283900100402     c* tipo servizio  "E"
284000100402     c                   if        tastsp='E'
284100100402     C                   ADD       1             eWTSPEA_r
284200100402     C                   ADD       TASNCL        eWTCLEA_r
284300100402     C                   ADD       w_peso        eWTKGEA_r
284400100402     C                   ADD       TASVLF        eWTMCEA_r
284500100402     c                   endif
284600100402     c* tipo servizio  "H"
284700100402     c                   if        tastsp='H'
284800100402     C                   ADD       1             hWTSPEA_r
284900100402     C                   ADD       TASNCL        hWTCLEA_r
285000100402     C                   ADD       w_peso        hWTKGEA_r
285100100402     C                   ADD       TASVLF        hWTMCEA_r
285200100402     c                   endif
285300100402     c* extra budget
285400100402     c                   if        *in35
285500100402     C                   ADD       1             WTSPEA_re
285600100402     C                   ADD       TASNCL        WTCLEA_re
285700100402     C                   ADD       w_peso        WTKGEA_re
285800100402     C                   ADD       TASVLF        WTMCEA_re
285900100402     c* tipo servizio  "E"
286000100402     c                   if        tastsp='E'
286100100402     C                   ADD       1             eWTSPEA_re
286200100402     C                   ADD       TASNCL        eWTCLEA_re
286300100402     C                   ADD       w_peso        eWTKGEA_re
286400100402     C                   ADD       TASVLF        eWTMCEA_re
286500100402     c                   endif
286600100402     c* tipo servizio  "H"
286700100402     c                   if        tastsp='H'
286800100402     C                   ADD       1             hWTSPEA_re
286900100402     C                   ADD       TASNCL        hWTCLEA_re
287000100402     C                   ADD       w_peso        hWTKGEA_re
287100100402     C                   ADD       TASVLF        hWTMCEA_re
287200100402     c                   endif
287300100402     c
287400100402     c                   endif
287500100402     c                   ENDSR
287600100402     c****************************************************************************
287700970519     OQSYSPRT   E            TESTA          2 02
287800090206     O                                         +  1 'Terminata elaborazione'
287900090206     o                                         +  2 'statistiche spedizioni'
288000090206     O                                         +  2 'DEL'
288100970519     O                       UDATE         Y   +  1
288200970528     O                       §DAL              +  5 '    /  /  '
288300970528     O                                         +  1 '>'
288400970528     O                       §AL               +  1 '    /  /  '
288500090206     O                                          120 'TNSSL3R1'
288600970519     O                       PAGE          Z    132
288700970519     O                                          128 'PAG.'
288800970529     O          E            TESTA          2
288900970529     O                                           14 'TIPI BOLLA OK:'
289000970529     O                       TBO               +  1
289100970529     O          E            ERR            1
289200970529     O                       TASLNP               3
289300970529     O                       TASLNA            +  1
289400970529     O                       TASAAS            +  1
289500970529     O                       TASMGS            +  1
289600970529     O                       TASNRS            +  1
289700970529     O                       TASNSP            +  1
289800970529     O                       TASTBL            +  1
289900110801     O          E            TESTA       1  1
290000110801     O                                            7 'Clienti'
290100110801     o                       Ptipocli         +   1
290200110801     O                                        +   1 ':'
290300110801     O          E            detksc         1
290400110801     o                       skicli             132
290500090915**
290600090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
