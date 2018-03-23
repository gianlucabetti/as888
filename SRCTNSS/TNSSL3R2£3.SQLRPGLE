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
057900111028     d
058000111028     d titas00ds     E DS                  extname(titas00f)
058100090601     d
058200111028     d wrkgetlista     s          25767    varying
058300970527     C* RICEVIMENTO PARAMETRI
058400970527     C     *ENTRY        PLIST
058500970527     C                   PARM                    KPJBA
058600970527     C                   MOVEL     §DAL          WAAS
058700970527     C                   MOVE      §DAL          WMGS
058800090601     C                   MOVE      §tipo         Wtipo             1
058900970528     C*
059000970529     C                   EXSR      CARINZ
059100951220     C*
059200990811     C* LETTURA TITAS
059300111028     c                   if        §stat=' '
059400990811     C     KTAS          SETLL     TITAS38C
059500111028     c                   else
059600111028
059700111028     C/EXEC SQL
059800111028     C+ PREPARE s1 FROM :wrkgetlista
059900111028     C/END-EXEC
060000111028     C
060100111028     C/EXEC SQL
060200111028     C+ DECLARE A1 CURSOR FOR S1
060300111028     C/END-EXEC
060400111028     C
060500111028     C/EXEC SQL
060600111028     C+ OPEN a1
060700111028     C/END-EXEC
060800111028
060900111028     c                   endif
061000111028     c
061100970527     C                   DO        *HIVAL
061200111028     c
061300111028     c                   if        §stat=' '
061400090204     C                   READ      TITAS38C
061500090204     c* Termina il ciclo se fine file o per data spedizione > data al
061600090204     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
061700090204     c                   leave
061800090204     c                   endif
061900111028     c
062000111028     c                   else
062100111028     C/EXEC SQL
062200111028     C+ FETCH NEXT FROM a1 INTO :titas00ds
062300111028     C/END-EXEC
062400111028
062500111028     c                   select
062600111028
062700111028     c                   when      sqlcod = 100
062800111028     C/EXEC SQL
062900111103     C+ CLOSE a1
063000111028     C/END-EXEC
063100111028     c                   leave
063200111028     c                   when      sqlcod < 0
063300111028     c                   seton                                        H1
063400111028     C/EXEC SQL
063500111103     C+ CLOSE a1
063600111028     C/END-EXEC
063700111028     c                   leave
063800111028     c                   endsl
063900111028     c
064000111028     c                   endif
064100111028     c
064200090204     c* Scarto in base al tipo bolla
064300090204     c                   if        %lookup(tastbl:tbo)=0
064400090204     c                   iter
064500090204     c                   endif
064600970527     C*
064700090601     c
064800090601     c* EDPES : 01/06/09
064900090601     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
065000090901     c***                z-add     taspkf        w_peso
065100090901     c* EDPES:  01/09/09
065200090901     c**  riutilizzo il peso VDL
065300090901     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
065400090901     c* 1. VDL totale
065500090901     c* 2. VDL parziale se > di TASPKF
065600090901     c* 3. TASPKF
065700090901     c                   clear                   w_peso
065800090901     c                   if        tasncp=tasncl or (tasncp>0 and taspkc>taspkf)
065900090901     c                   z-add     taspkc        w_peso
066000090901     c                   else
066100090901     c                   z-add     taspkf        w_peso
066200090901     c                   endif
066300090601     c
066400110711     c                   if        §stat=' '
066500090204     c                   exsr      sr_wfsmp
066600110711     c                   endif
066700110711     c
066800090204     c                   exsr      sr_wslss
066900090204     C                   ENDDO
067000090206     c*
067100090206     C* SCARICO DS SU FILE
067200090206     c                   exsr      scrive_file
067300110801     c                   if        §stat=' '
067400110801     c                   eval      ptipocli='EXTRA BUDGET'
067500110801     c                   else
067600110801     c                   eval      ptipocli='PARTICOLARI '
067700110801     c                   endif
067800110801     c
067900090206     c                   except    testa
068000110801     C*
068100110801     c                   z-add     1             k
068200110801     c                   clear                   conta             3 0
068300110801     c                   clear                   skicli
068400110801     c                   dow       ksc_ex(k)<>*blanks
068500110801     c
068600110801     c                   if        §stat=' '
068700110801
068800110801     c                   eval      skicli=%trim(skicli)+' '+
068900110801     c                             %subst(ksc_ex(k):1:7)
069000110801     c                   add       7             conta
069100110801     c*
069200110801     c                   if        %subst(ksc_ex(k):8:3)<>'   '
069300110801     c                   eval      skicli=%trim(skicli)+'/'+
069400110801     c                             %subst(ksc_ex(k):8:3)
069500110801     c                   add       4             conta
069600110801     c                   endif
069700110801     c                   add       1             conta
069800110801     c
069900110801     c
070000110801     c                   if        conta>120
070100110801     c                   except    detksc
070200110801     c                   clear                   conta
070300110801     c                   clear                   skicli
070400110801     c                   endif
070500110801     c
070600110801     c                   else
070700110801     c
070800110801     c                   eval      skicli=%trim(skicli)+' '+
070900110801     c                             %subst(ksc_ex(k):1:7)
071000110801     c                   add       1             conta
071100110801     c
071200110801     c                   if        conta=16
071300110801     c                   except    detksc
071400110801     c                   clear                   conta
071500110801     c                   clear                   skicli
071600110801     c                   endif
071700110801     c
071800110801     c                   endif
071900110801     c
072000110801     c                   add       1             k
072100110801     c                   enddo
072200110801     c
072300110801     c                   if        conta>0
072400110801     c                   except    detksc
072500110801     c                   endif
072600970528     C*
072700970528     C                   SETON                                        LR
072800970519     C*
072900090204     C**********************************************************
073000090204     C** Elaborazione per file wfsmp00f
073100090204     C**********************************************************
073200090204     C     sr_wfsmp      BEGSR
073300090204     c* linea di partenza
073400090206     c                   z-add     1             k
073500090206     c     taslnp        lookup    fil(k)                                 58
073600090206     c                   if        *in58
073700090206     c     k             occur     dsfil_smp
073800090205     c* . . . giornaliero
073900090206     c                   if        (tasaas*10000+tasmgs)=§al
074000090206     C                   add       1             Wmptsp_g
074100090206     C                   add       TASNCL        wmptcp_g
074200090224     C                   add       w_peso        Wmptkp_g
074300090206     C                   add       TASVLF        Wmptmp_g
074400090206     c                   endif
074500090204     c* . . . riepilogo
074600090206     C                   add       1             Wmptsp_r
074700090206     C                   add       TASNCL        wmptcp_r
074800090224     C                   add       w_peso        Wmptkp_r
074900090206     C                   add       TASVLF        Wmptmp_r
075000090206     c                   endif
075100090204     c* linea di arrivo
075200090206     c                   z-add     1             k
075300090206     c     taslna        lookup    fil(k)                                 58
075400090206     c                   if        *in58
075500090206     c     k             occur     dsfil_smp
075600090205     c* . . . giornaliero
075700090206     c                   if        (tasaas*10000+tasmgs)=§al
075800090206     C                   add       1             Wmptsa_g
075900090206     C                   add       TASNCL        wmptca_g
076000090224     C                   add       w_peso        Wmptka_g
076100090206     C                   add       TASVLF        Wmptma_g
076200090206     c                   endif
076300090205     c* . . . riepilogo
076400090206     C                   add       1             Wmptsa_r
076500090206     C                   add       TASNCL        wmptca_r
076600090224     C                   add       w_peso        Wmptka_r
076700090206     C                   add       TASVLF        Wmptma_r
076800090206     c                   endif
076900090710     c* terminal di partenza : esclusa la linea/terminal
077000090710     c                   if        tastfp<>taslnp
077100090206     c                   z-add     1             k
077200090206     c     tastfp        lookup    fil(k)                                 58
077300090206     c                   if        *in58
077400090206     c     k             occur     dsfil_smp
077500090205     c* . . . giornaliero
077600090206     c                   if        (tasaas*10000+tasmgs)=§al
077700090206     C                   add       1             Wmptspt_g
077800090206     C                   add       TASNCL        wmptcpt_g
077900090224     C                   add       w_peso        Wmptkpt_g
078000090206     C                   add       TASVLF        Wmptmpt_g
078100090206     c                   endif
078200090205     c* . . . riepilogo
078300090206     C                   add       1             Wmptspt_r
078400090206     C                   add       TASNCL        wmptcpt_r
078500090224     C                   add       w_peso        Wmptkpt_r
078600090206     C                   add       TASVLF        Wmptmpt_r
078700090206     c                   endif
078800090710     c                   endif
078900090710     c* terminal di partenza : tutto compresa la linea stessa
079000090710     c                   z-add     1             k
079100090710     c     tastfp        lookup    fil(k)                                 58
079200090710     c                   if        *in58
079300090710     c     k             occur     dsfil_smp
079400090710     c* . . . giornaliero
079500090710     c                   if        (tasaas*10000+tasmgs)=§al
079600090710     C                   add       1             Wmptsptc_g
079700090710     C                   add       TASNCL        wmptcptc_g
079800090710     C                   add       w_peso        Wmptkptc_g
079900090710     C                   add       TASVLF        Wmptmptc_g
080000090710     c                   endif
080100090710     c* . . . riepilogo
080200090710     C                   add       1             Wmptsptc_r
080300090710     C                   add       TASNCL        wmptcptc_r
080400090710     C                   add       w_peso        Wmptkptc_r
080500090710     C                   add       TASVLF        Wmptmptc_r
080600090710     c                   endif
080700090710     c
080800090710     c* terminal di arrivo : esclusa la linea/terminal
080900090710     c                   if        tastfa<>taslna
081000090206     c                   z-add     1             k
081100090206     c     tastfa        lookup    fil(k)                                 58
081200090206     c                   if        *in58
081300090206     c     k             occur     dsfil_smp
081400090205     c* . . . giornaliero
081500090206     c                   if        (tasaas*10000+tasmgs)=§al
081600090206     C                   add       1             Wmptsat_g
081700090206     C                   add       TASNCL        wmptcat_g
081800090224     C                   add       w_peso        Wmptkat_g
081900090206     C                   add       TASVLF        Wmptmat_g
082000090206     c                   endif
082100090205     c* . . . riepilogo
082200090206     C                   add       1             Wmptsat_r
082300090206     C                   add       TASNCL        wmptcat_r
082400090224     C                   add       w_peso        Wmptkat_r
082500090206     C                   add       TASVLF        Wmptmat_r
082600090205     c                   endif
082700090710     c                   endif
082800090710     c* terminal di arrivo : tutto compreso il terminal stesso
082900090710     c                   z-add     1             k
083000090710     c     tastfa        lookup    fil(k)                                 58
083100090710     c                   if        *in58
083200090710     c     k             occur     dsfil_smp
083300090710     c* . . . giornaliero
083400090710     c                   if        (tasaas*10000+tasmgs)=§al
083500090710     C                   add       1             Wmptsatc_g
083600090710     C                   add       TASNCL        wmptcatc_g
083700090710     C                   add       w_peso        Wmptkatc_g
083800090710     C                   add       TASVLF        Wmptmatc_g
083900090710     c                   endif
084000090710     c* . . . riepilogo
084100090710     C                   add       1             Wmptsatc_r
084200090710     C                   add       TASNCL        wmptcatc_r
084300090710     C                   add       w_peso        Wmptkatc_r
084400090710     C                   add       TASVLF        Wmptmatc_r
084500090710     c                   endif
084600090710     c
084700090204     c* filiali di transito
084800090209     c     kar5          chain     fiar531c
084900090205     c                   if        %found(fiar531c)
085000090205     c                   movel     ar5uni        dar5trs
085100090205     c*
085200090205     c                   eval      k_fil=§AR5FLP
085300090205     c                   exsr      agg_transiti
085400090205      *
085500090205     c                   if        §ar5fl2>0
085600090205     c                   eval      k_fil=§AR5FL2
085700090205     c                   exsr      agg_transiti
085800090205     c                   endif
085900090205      *
086000090205     c                   if        §ar5fl3>0
086100090205     c                   eval      k_fil=§AR5FL3
086200090205     c                   exsr      agg_transiti
086300090205     c                   endif
086400090205      *
086500090205     c                   if        §ar5fl4>0
086600090205     c                   eval      k_fil=§AR5FL4
086700090205     c                   exsr      agg_transiti
086800090205     c                   endif
086900090205      *
087000090205     c                   if        §ar5fl5>0
087100090205     c                   eval      k_fil=§AR5FL5
087200090205     c                   exsr      agg_transiti
087300090205     c                   endif
087400090205     c                   endif
087500090204     c
087600090204     C                   endsr
087700090205     C**********************************************************
087800090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
087900090205     C**********************************************************
088000090205     C     agg_transiti  BEGSR
088100090206     c                   z-add     1             k
088200090206     c     k_fil         lookup    fil(k)                                 58
088300090206     c                   if        *in58
088400090206     c     k             occur     dsfil_smp
088500090205     c* . . . giornaliero
088600090206     c                   if        (tasaas*10000+tasmgs)=§al
088700090206     C                   add       1             Wmptst_g
088800090206     C                   add       TASNCL        wmptct_g
088900090224     C                   add       w_peso        Wmptkt_g
089000090206     C                   add       TASVLF        Wmptmt_g
089100090206     c                   endif
089200090205     c* . . . riepilogo
089300090206     C                   add       1             Wmptst_r
089400090206     C                   add       TASNCL        wmptct_r
089500090224     C                   add       w_peso        Wmptkt_r
089600090206     C                   add       TASVLF        Wmptmt_r
089700090205     c                   endif
089800090205     c                   endsr
089900090204     C**********************************************************
090000090206     C** Riempie campi wfsmp00f
090100090204     C**********************************************************
090200090206     C     rie_campi     BEGSR
090300090204     c* profilo utente e data elaborazione
090400090204     c                   eval      wmpusr=knmus
090500090204     c                   eval      wmpdel=wdatael
090600090206     c*
090700090206     c                   eval      wmpall=§al
090800090204     c* area e descrizione area
090900090204     c                   eval      wmpcar=car(k)
091000090204     C                   MOVEL(P)  '05'          tblcod
091100090204     C                   MOVEL(P)  car(K)        tblkey
091200090204     C     KTAB          CHAIN     TABEL00F                           30
091300090204     C                   IF        %found(tabel00f) and tblflg=*blanks
091400090204     C                   MOVEL     TBLUNI        DS05
091500090204     C                   MOVEL     §05DES        wmpdar
091600090204     C                   endif
091700090204     c* filiale,descrizione e livello filiale
091800090204     c                   eval      wmpdil=dil(K)
091900090206     c                   eval      wmpfil=fil(K)
092000090206     c                   eval      wmplif=lpo(K)
092100090204     c* tipo filiale Italia/Estero
092200090205     c                   eval      wmptif=tif(K)
092300090206     c* spedizioni,colli, kg,volume
092400090206     c                   if        wmptip='G'
092500090206     c                   z-add     wmptsp_G      wmptsp
092600090206     c                   z-add     wmptcp_G      wmptcp
092700090206     c                   z-add(h)  wmptkp_G      wmptkp
092800090206     c                   z-add(h)  wmptmp_G      wmptmp
092900090710
093000090206     c                   z-add     wmptsa_G      wmptsa
093100090206     c                   z-add     wmptca_G      wmptca
093200090206     c                   z-add(h)  wmptka_G      wmptka
093300090206     c                   z-add(h)  wmptma_G      wmptma
093400090710
093500090206     c                   z-add     wmptspt_G     wmptspt
093600090206     c                   z-add     wmptcpt_G     wmptcpt
093700090206     c                   z-add(h)  wmptkpt_G     wmptkpt
093800090206     c                   z-add(h)  wmptmpt_G     wmptmpt
093900090710
094000090710     c                   z-add     wmptsptc_G    wmptsptc
094100090710     c                   z-add     wmptcptc_G    wmptcptc
094200090710     c                   z-add(h)  wmptkptc_G    wmptkptc
094300090710     c                   z-add(h)  wmptmptc_G    wmptmptc
094400090710
094500090206     c                   z-add     wmptsat_G     wmptsat
094600090206     c                   z-add     wmptcat_G     wmptcat
094700090206     c                   z-add(h)  wmptkat_G     wmptkat
094800090206     c                   z-add(h)  wmptmat_G     wmptmat
094900090710
095000090710     c                   z-add     wmptsatc_G    wmptsatc
095100090710     c                   z-add     wmptcatc_G    wmptcatc
095200090710     c                   z-add(h)  wmptkatc_G    wmptkatc
095300090710     c                   z-add(h)  wmptmatc_G    wmptmatc
095400090710
095500090206     c                   z-add     wmptst_G      wmptst
095600090206     c                   z-add     wmptct_G      wmptct
095700090206     c                   z-add(h)  wmptkt_G      wmptkt
095800090206     c                   z-add(h)  wmptmt_G      wmptmt
095900090206     c                   else
096000090206     c                   z-add     wmptsp_r      wmptsp
096100090206     c                   z-add     wmptcp_r      wmptcp
096200090206     c                   z-add(h)  wmptkp_r      wmptkp
096300090206     c                   z-add(h)  wmptmp_r      wmptmp
096400090710
096500090206     c                   z-add     wmptsa_r      wmptsa
096600090206     c                   z-add     wmptca_r      wmptca
096700090206     c                   z-add(h)  wmptka_r      wmptka
096800090206     c                   z-add(h)  wmptma_r      wmptma
096900090710
097000090206     c                   z-add     wmptspt_r     wmptspt
097100090206     c                   z-add     wmptcpt_r     wmptcpt
097200090206     c                   z-add(h)  wmptkpt_r     wmptkpt
097300090206     c                   z-add(h)  wmptmpt_r     wmptmpt
097400090710
097500090710     c                   z-add     wmptsptc_r    wmptsptc
097600090710     c                   z-add     wmptcptc_r    wmptcptc
097700090710     c                   z-add(h)  wmptkptc_r    wmptkptc
097800090710     c                   z-add(h)  wmptmptc_r    wmptmptc
097900090710
098000090206     c                   z-add     wmptsat_r     wmptsat
098100090206     c                   z-add     wmptcat_r     wmptcat
098200090206     c                   z-add(h)  wmptkat_r     wmptkat
098300090206     c                   z-add(h)  wmptmat_r     wmptmat
098400090710
098500090710     c                   z-add     wmptsatc_r    wmptsatc
098600090710     c                   z-add     wmptcatc_r    wmptcatc
098700090710     c                   z-add(h)  wmptkatc_r    wmptkatc
098800090710     c                   z-add(h)  wmptmatc_r    wmptmatc
098900090710
099000090206     c                   z-add     wmptst_r      wmptst
099100090206     c                   z-add     wmptct_r      wmptct
099200090206     c                   z-add(h)  wmptkt_r      wmptkt
099300090206     c                   z-add(h)  wmptmt_r      wmptmt
099400090710
099500090206     c                   endif
099600090204     c                   endsr
099700090204     C**********************************************************
099800090204     C** Elaborazione per file wslss50f
099900090204     C**********************************************************
100000090204     C     sr_wslss      BEGSR
100100090204     C                   SETOFF                                       545556
100200090204     C                   SETOFF                                       48  52
100300090204     C     TASLNP        COMP      300                                54        ESTERO
100400090204     C  N54TASLNP        COMP      190                                    54    + DPD
100500090204     C  N54TASLNP        COMP      191                                    54    + DPD
100600090204     C  N54TASLNP        COMP      192                                    54    + DPD
100700090204     C  N54TASLNP        COMP      193                                    54    + DPD
100800090204     C  N54TASLNP        COMP      194                                    54    + DPD
100900090204     C  N54TASLNP        COMP      195                                    54    + DPD
101000090204     C  N54TASLNP        COMP      196                                    54    + DPD
101100090204     C  N54TASLNP        COMP      197                                    54    + DPD
101200090204     C*
101300100402     C****     TASLNP        COMP      180                                    48     POSTE
101400090204     C*
101500090204     C     TASLNA        COMP      300                                55
101600090204     C  N55TASLNA        COMP      190                                    55    + DPD
101700090204     C  N55TASLNA        COMP      191                                    55    + DPD
101800090204     C  N55TASLNA        COMP      192                                    55    + DPD
101900090204     C  N55TASLNA        COMP      193                                    55    + DPD
102000090204     C  N55TASLNA        COMP      194                                    55    + DPD
102100090204     C  N55TASLNA        COMP      195                                    55    + DPD
102200090204     C  N55TASLNA        COMP      196                                    55    + DPD
102300090204     C  N55TASLNA        COMP      197                                    55    + DPD
102400090204     C*
102500090204     C     TASLNA        COMP      400                                    52     FEDEX
102600090204     C*
102700090204     C     TASLNA        COMP      190                                    56     DPD
102800090204     C  N56TASLNA        COMP      191                                    56     DPD
102900090204     C  N56TASLNA        COMP      192                                    56     DPD
103000090204     C  N56TASLNA        COMP      193                                    56     DPD
103100090204     C  N56TASLNA        COMP      194                                    56     DPD
103200090204     C  N56TASLNA        COMP      195                                    56     DPD
103300090204     C  N56TASLNA        COMP      196                                    56     DPD
103400090204     C  N56TASLNA        COMP      197                                    56     DPD
103500091006     c*
103600091006     c* Verifico se si tratta di cliente extra budget --> 35 on
103700110711     c                   if        §stat=' '
103800091006     c                   z-add     1             EE
103900091006     c                   if        tasccm<>savccm or tasctr<>savctr
104000091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
104100091007     c     w010a         lookup    ksc_EX(EE)                             35
104200091007     c                   if        not *in35
104300091007     c                   eval      %subst(w010a:8:3)='   '
104400091007     c     w010a         lookup    ksc_EX(EE)                             35
104500091006     c                   endif
104600091006
104700091006     c                   eval      savccm=tasccm
104800091006     c                   eval      savctr=tasctr
104900091006     c                   endif
105000110711     c
105100111028     c                   else
105200111028     c                   seton                                        35
105300110711     c*
105400111028     c**                 z-add     1             EE
105500111028     c**                 if        tasccm<>savccm
105600111028     c**                 eval      w010a=%editc(tasccm:'X')+'   '
105700111028     c**   w010a         lookup    ksc_EX(EE)                             35
105800111028     c**                 eval      savccm=tasccm
105900111028     c**                 endif
106000110711     c
106100110711     c                   endif
106200090204     C*
106300090204     C                   Z-ADD     1             K
106400090204     C     TASLNP        LOOKUP    FIL(K)                                 58
106500090204     C  N58              EXCEPT    ERR
106600090204     C     K             OCCUR     DSFIL
106700100402     C     K             OCCUR     DSFIL_e
106800100402     C     K             OCCUR     DSFIL_h
106900090204     C  N54
107000090204     CANN55              DO                                                     ITALIA
107100100402     c* Spedizioni LNP Italia LNA Italia
107200100402     c                   exsr      speITA
107300090204     C                   ENDDO
107400100402     c
107500100402     c* Spedizioni IMPORT --> LNP Italia
107600090204     C   54              DO                                                     IMPORT
107700100402     c                   exsr      speIMPORT
107800090204     C                   ENDDO
107900100402     c
108000100402     c* Spedizioni lna EUROEXPRESS
108100090204     C   55
108200090204     CANN56
108300090204     CANN52              DO                                                     EXPORT
108400100402     c                   exsr      speEXPRESS
108500090204     C                   ENDDO
108600100402     c
108700100402     c* Spedizioni lna DPD
108800090204     C   55
108900090204     CAN 56              DO                                                     EXPORT
109000100402     c                   exsr      speDPD
109100090204     C                   ENDDO
109200100402     c
109300100402     c* Spedizioni lna FEDEX
109400090204     C   55
109500090204     CAN 52              DO                                                     EXPORT
109600100402     c                   exsr      speFEDEX
109700090204     C                   ENDDO
109800100402     c
109900090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
110000090204     C                   Z-ADD     1             K
110100090204     C     TASLNA        LOOKUP    FIL(K)                                 58
110200090204     C  N58              EXCEPT    ERR
110300090204     C     K             OCCUR     DSFIL
110400100402     C     K             OCCUR     DSFIL_e
110500100402     C     K             OCCUR     DSFIL_h
110600100402     c
110700090204     C  N54
110800100402     CANN55              DO
110900100402     c* Spedizioni LNP Italia LNA Italia
111000100402     c                   exsr      speITA_LNA
111100090204     C                   ENDDO
111200100402     c
111300100402     c* spedizioni EXPORT --> lna estera
111400090204     C   55              DO                                                     ESTERO ASS.+FRA.
111500100402     c                   exsr      speEXPORT
111600090204     C                   ENDDO
111700090204     c                   endsr
111800090206     C**********************************************************
111900090206     C** scarica ds multiple su file
112000090206     C**********************************************************
112100090206     C     scrive_file   BEGSR
112200090206     c*
112300090206     C                   Z-ADD     §AL           WSSALL
112400090206     c*
112500110711    1C                   DO        400           K
112600090206     C     K             OCCUR     DSFIL_smp
112700090206     C     K             OCCUR     DSFIL
112800100402     C     K             OCCUR     DSFIL_e
112900100402     C     K             OCCUR     DSFIL_h
113000110711     c
113100110711    2c                   if        §stat=' '
113200090206     c* WFSMP
113300090206     c* giornaliero
113400090601     c* Lo scrivo solo per tipo "G" o "R"
113500090601     c                   if        Wtipo='G' or Wtipo='R'
113600090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
113700090206     c                              wmptspt_g>0 or wmptsat_g>0 or
113800090206     c                              wmptst_g>0)
113900090206     c                   z-add     §al           wmpdal
114000090206     c                   eval      wmptip='G'
114100090206     c                   exsr      rie_campi
114200090206     c                   write     wfsmp000
114300090206     c                   endif
114400090601     c                   endif
114500090601     c
114600090601     c* riepilogo /precedente
114700090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
114800090601     c                   if        Wtipo='P' or Wtipo='R'
114900090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
115000090206     c                              wmptspt_r>0 or wmptsat_r>0 or
115100090206     c                              wmptst_r>0)
115200090206     c                   z-add     §dal          wmpdal
115300090601     c                   if        Wtipo='R'
115400090206     c                   eval      wmptip='R'
115500090601     c                   else
115600090601     c                   eval      wmptip='P'
115700090601     c                   endif
115800090206     c                   exsr      rie_campi
115900090206     c                   write     wfsmp000
116000090206     c                   endif
116100090601     c                   endif
116200110711     c
116300110711    2c                   endif
116400090206     c* WSLSS50F
116500090206     c* giornaliero
116600090601     c                   if        Wtipo='G' or Wtipo='R'
116700110711     c
116800110711    2c                   if        §stat=' '
116900090601     c
117000090206     C     WTSPIP        IFGT      *ZERO
117100090206     C                   Z-ADD     §al           WSSDAL
117200090206     c                   movel     'G'           WSSTIE
117300090206     C                   MOVE      'IP'          WSSTIP
117400090206     C                   Z-ADD     FIL(K)        WSSLIN
117500090206     C                   Z-ADD     WTSPIP        WSSTSP
117600090206     C                   Z-ADD     WTCLIP        WSSTCL
117700090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
117800090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
117900100402     c* spedizioni "E"
118000100402     C                   Z-ADD     eWTSPIP       WSSTSPe
118100100402     C                   Z-ADD     eWTCLIP       WSSTCLe
118200100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
118300100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
118400100402     c* spedizioni "H"
118500100402     C                   Z-ADD     hWTSPIP       WSSTSPh
118600100402     C                   Z-ADD     hWTCLIP       WSSTCLh
118700100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
118800100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
118900100402
119000090206     C                   WRITE     WSLSS500
119100090206     C                   ENDIF
119200100402
119300090206     C     WTSPIA        IFGT      *ZERO
119400090206     C                   Z-ADD     §al           WSSDAL
119500090206     c                   movel     'G'           WSSTIE
119600090206     C                   MOVE      'IA'          WSSTIP
119700090206     C                   Z-ADD     FIL(K)        WSSLIN
119800090206     C                   Z-ADD     WTSPIA        WSSTSP
119900090206     C                   Z-ADD     WTCLIA        WSSTCL
120000090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
120100090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
120200100402     c* spedizioni "E"
120300100402     C                   Z-ADD     eWTSPIA       WSSTSPe
120400100402     C                   Z-ADD     eWTCLIA       WSSTCLe
120500100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
120600100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
120700100402     c* spedizioni "H"
120800100402     C                   Z-ADD     hWTSPIA       WSSTSPh
120900100402     C                   Z-ADD     hWTCLIA       WSSTCLh
121000100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
121100100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
121200100402     c
121300090206     C                   WRITE     WSLSS500
121400090206     C                   ENDIF
121500100402     c
121600090206     C     WTSPEI        IFGT      *ZERO
121700090206     C                   Z-ADD     §al           WSSDAL
121800090206     c                   movel     'G'           WSSTIE
121900090206     C                   MOVE      'EI'          WSSTIP
122000090206     C                   Z-ADD     FIL(K)        WSSLIN
122100090206     C                   Z-ADD     WTSPEI        WSSTSP
122200090206     C                   Z-ADD     WTCLEI        WSSTCL
122300090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
122400090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
122500100402     c* spedizioni "E"
122600100402     C                   Z-ADD     eWTSPEI       WSSTSPe
122700100402     C                   Z-ADD     eWTCLEI       WSSTCLe
122800100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
122900100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
123000100402     c* spedizioni "H"
123100100402     C                   Z-ADD     hWTSPEI       WSSTSPh
123200100402     C                   Z-ADD     hWTCLEI       WSSTCLh
123300100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
123400100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
123500100402     c
123600090206     C                   WRITE     WSLSS500
123700090206     C                   ENDIF
123800100402     c
123900090206     C     WTSPEE        IFGT      *ZERO
124000090206     C                   Z-ADD     §al           WSSDAL
124100090206     c                   movel     'G'           WSSTIE
124200090206     C                   MOVE      'EE'          WSSTIP
124300090206     C                   Z-ADD     FIL(K)        WSSLIN
124400090206     C                   Z-ADD     WTSPEE        WSSTSP
124500090206     C                   Z-ADD     WTCLEE        WSSTCL
124600090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
124700090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
124800100402     c* spedizioni "E"
124900100402     C                   Z-ADD     eWTSPEE       WSSTSPe
125000100402     C                   Z-ADD     eWTCLEE       WSSTCLe
125100100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
125200100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
125300100402     c* spedizioni "H"
125400100402     C                   Z-ADD     hWTSPEE       WSSTSPh
125500100402     C                   Z-ADD     hWTCLEE       WSSTCLh
125600100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
125700100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
125800100402     c
125900090206     C                   WRITE     WSLSS500
126000090206     C                   ENDIF
126100090206     C*
126200090206     C     WTSPED        IFGT      *ZERO
126300090206     C                   Z-ADD     §al           WSSDAL
126400090206     c                   movel     'G'           WSSTIE
126500090206     C                   MOVE      'ED'          WSSTIP
126600090206     C                   Z-ADD     FIL(K)        WSSLIN
126700090206     C                   Z-ADD     WTSPED        WSSTSP
126800090206     C                   Z-ADD     WTCLED        WSSTCL
126900090206     C                   Z-ADD(H)  WTKGED        WSSTKG
127000090206     C                   Z-ADD(H)  WTMCED        WSSTMC
127100100402     c* spedizioni "E"
127200100402     C                   Z-ADD     eWTSPED       WSSTSPe
127300100402     C                   Z-ADD     eWTCLED       WSSTCLe
127400100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
127500100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
127600100402     c* spedizioni "H"
127700100402     C                   Z-ADD     hWTSPED       WSSTSPh
127800100402     C                   Z-ADD     hWTCLED       WSSTCLh
127900100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
128000100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
128100100402     c
128200090206     C                   WRITE     WSLSS500
128300090206     C                   ENDIF
128400090206     C*
128500090206     C     WTSPEX        IFGT      *ZERO
128600090206     C                   Z-ADD     §al           WSSDAL
128700090206     c                   movel     'G'           WSSTIE
128800090206     C                   MOVE      'EX'          WSSTIP
128900090206     C                   Z-ADD     FIL(K)        WSSLIN
129000090206     C                   Z-ADD     WTSPEX        WSSTSP
129100090206     C                   Z-ADD     WTCLEX        WSSTCL
129200090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
129300090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
129400100402     c* spedizioni "E"
129500100402     C                   Z-ADD     eWTSPEX       WSSTSPe
129600100402     C                   Z-ADD     eWTCLEX       WSSTCLe
129700100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
129800100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
129900100402     c* spedizioni "H"
130000100402     C                   Z-ADD     hWTSPEX       WSSTSPh
130100100402     C                   Z-ADD     hWTCLEX       WSSTCLh
130200100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
130300100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
130400100402     c
130500090206     C                   WRITE     WSLSS500
130600090206     C                   ENDIF
130700090206     C*
130800090206     C     WTSPEA        IFGT      *ZERO
130900090206     C                   Z-ADD     §al           WSSDAL
131000090206     c                   movel     'G'           WSSTIE
131100090206     C                   MOVE      'EA'          WSSTIP
131200090206     C                   Z-ADD     FIL(K)        WSSLIN
131300090206     C                   Z-ADD     WTSPEA        WSSTSP
131400090206     C                   Z-ADD     WTCLEA        WSSTCL
131500090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
131600090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
131700100402     c* spedizioni "E"
131800100402     C                   Z-ADD     eWTSPEA       WSSTSPe
131900100402     C                   Z-ADD     eWTCLEA       WSSTCLe
132000100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
132100100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
132200100402     c* spedizioni "H"
132300100402     C                   Z-ADD     hWTSPEA       WSSTSPh
132400100402     C                   Z-ADD     hWTCLEA       WSSTCLh
132500100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
132600100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
132700100402     c
132800090206     C                   WRITE     WSLSS500
132900090206     C                   ENDIF
133000090206     C*
133100090206     C     WTSPPA        IFGT      *ZERO
133200090206     C                   Z-ADD     §al           WSSDAL
133300090206     c                   movel     'G'           WSSTIE
133400090206     C                   MOVE      'PA'          WSSTIP
133500090206     C                   Z-ADD     FIL(K)        WSSLIN
133600090206     C                   Z-ADD     WTSPPA        WSSTSP
133700090206     C                   Z-ADD     WTCLPA        WSSTCL
133800090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
133900090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
134000100402     c* spedizioni "E"
134100100402     C                   Z-ADD     eWTSPPA       WSSTSPe
134200100402     C                   Z-ADD     eWTCLPA       WSSTCLe
134300100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
134400100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
134500100402     c* spedizioni "H"
134600100402     C                   Z-ADD     hWTSPPA       WSSTSPh
134700100402     C                   Z-ADD     hWTCLPA       WSSTCLh
134800100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
134900100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
135000100402     c
135100090206     C                   WRITE     WSLSS500
135200090206     C                   ENDIF
135300110711     c
135400110711     c                   endif
135500110715     c* Extra budget o clienti particolari
135600110715     c                   if        §stat=' '
135700110715     c                   movel     'G'           WSSTIE
135800110715     c                   else
135900110715     c                   movel     'A'           WSSTIE
136000110715     c                   endif
136100110715     c
136200091006     C     WTSPIP_e      IFGT      *ZERO
136300091007     C                   Z-ADD     §al           WSSDAL
136400091006     C                   MOVE      'IP'          WSSTIP
136500091006     C                   Z-ADD     FIL(K)        WSSLIN
136600091006     C                   Z-ADD     WTSPIP_e      WSSTSP
136700091006     C                   Z-ADD     WTCLIP_e      WSSTCL
136800091006     C                   Z-ADD(H)  WTKGIP_e      WSSTKG
136900091006     C                   Z-ADD(H)  WTMCIP_e      WSSTMC
137000100402     c* spedizioni "E"
137100100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
137200100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
137300100402     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe
137400100402     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe
137500100402     c* spedizioni "H"
137600100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
137700100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
137800100402     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh
137900100402     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh
138000100402     c
138100091006     C                   WRITE     WSLSS550
138200091006     C                   ENDIF
138300100402     c
138400091006     C     WTSPIA_e      IFGT      *ZERO
138500091007     C                   Z-ADD     §al           WSSDAL
138600110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
138700110715     c*****              movel     'G'           WSSTIE
138800091006     C                   MOVE      'IA'          WSSTIP
138900091006     C                   Z-ADD     FIL(K)        WSSLIN
139000091006     C                   Z-ADD     WTSPIA_e      WSSTSP
139100091006     C                   Z-ADD     WTCLIA_e      WSSTCL
139200091006     C                   Z-ADD(H)  WTKGIA_e      WSSTKG
139300091006     C                   Z-ADD(H)  WTMCIA_e      WSSTMC
139400100402     c* spedizioni "E"
139500100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
139600100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
139700100402     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe
139800100402     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe
139900100402     c* spedizioni "H"
140000100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
140100100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
140200100402     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh
140300100402     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh
140400100402     c
140500091006     C                   WRITE     WSLSS550
140600091006     C                   ENDIF
140700100402     c
140800091006     C     WTSPEI_e      IFGT      *ZERO
140900091007     C                   Z-ADD     §al           WSSDAL
141000110715     c******             movel     'G'           WSSTIE
141100091006     C                   MOVE      'EI'          WSSTIP
141200091006     C                   Z-ADD     FIL(K)        WSSLIN
141300091006     C                   Z-ADD     WTSPEI_e      WSSTSP
141400091006     C                   Z-ADD     WTCLEI_e      WSSTCL
141500091006     C                   Z-ADD(H)  WTKGEI_e      WSSTKG
141600091006     C                   Z-ADD(H)  WTMCEI_e      WSSTMC
141700100402     c* spedizioni "E"
141800100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
141900100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
142000100402     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe
142100100402     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe
142200100402     c* spedizioni "H"
142300100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
142400100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
142500100402     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh
142600100402     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh
142700100402     c
142800091006     C                   WRITE     WSLSS550
142900091006     C                   ENDIF
143000100402     c
143100091006     C     WTSPEE_e      IFGT      *ZERO
143200091007     C                   Z-ADD     §al           WSSDAL
143300110715     c******             movel     'G'           WSSTIE
143400091006     C                   MOVE      'EE'          WSSTIP
143500091006     C                   Z-ADD     FIL(K)        WSSLIN
143600091006     C                   Z-ADD     WTSPEE_e      WSSTSP
143700091006     C                   Z-ADD     WTCLEE_e      WSSTCL
143800091006     C                   Z-ADD(H)  WTKGEE_e      WSSTKG
143900091006     C                   Z-ADD(H)  WTMCEE_e      WSSTMC
144000100402     c* spedizioni "E"
144100100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
144200100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
144300100402     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe
144400100402     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe
144500100402     c* spedizioni "H"
144600100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
144700100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
144800100402     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh
144900100402     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh
145000100402     c
145100091006     C                   WRITE     WSLSS550
145200091006     C                   ENDIF
145300091006     C*
145400091006     C     WTSPED_e      IFGT      *ZERO
145500091007     C                   Z-ADD     §al           WSSDAL
145600110715     c*******            movel     'G'           WSSTIE
145700091006     C                   MOVE      'ED'          WSSTIP
145800091006     C                   Z-ADD     FIL(K)        WSSLIN
145900091006     C                   Z-ADD     WTSPED_e      WSSTSP
146000091006     C                   Z-ADD     WTCLED_e      WSSTCL
146100091006     C                   Z-ADD(H)  WTKGED_e      WSSTKG
146200091006     C                   Z-ADD(H)  WTMCED_e      WSSTMC
146300100402     c* spedizioni "E"
146400100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
146500100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
146600100402     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe
146700100402     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe
146800100402     c* spedizioni "H"
146900100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
147000100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
147100100402     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh
147200100402     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh
147300100402     c
147400091006     C                   WRITE     WSLSS550
147500091006     C                   ENDIF
147600091006     C*
147700091006     C     WTSPEX_e      IFGT      *ZERO
147800091007     C                   Z-ADD     §al           WSSDAL
147900110715     c*******            movel     'G'           WSSTIE
148000091006     C                   MOVE      'EX'          WSSTIP
148100091006     C                   Z-ADD     FIL(K)        WSSLIN
148200091006     C                   Z-ADD     WTSPEX_e      WSSTSP
148300091006     C                   Z-ADD     WTCLEX_e      WSSTCL
148400091006     C                   Z-ADD(H)  WTKGEX_e      WSSTKG
148500091006     C                   Z-ADD(H)  WTMCEX_e      WSSTMC
148600100402     c* spedizioni "E"
148700100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
148800100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
148900100402     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe
149000100402     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe
149100100402     c* spedizioni "H"
149200100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
149300100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
149400100402     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh
149500100402     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh
149600100402     c
149700091006     C                   WRITE     WSLSS550
149800091006     C                   ENDIF
149900091006     C*
150000091006     C     WTSPEA_e      IFGT      *ZERO
150100091007     C                   Z-ADD     §al           WSSDAL
150200110715     c******             movel     'G'           WSSTIE
150300091006     C                   MOVE      'EA'          WSSTIP
150400091006     C                   Z-ADD     FIL(K)        WSSLIN
150500091006     C                   Z-ADD     WTSPEA_e      WSSTSP
150600091006     C                   Z-ADD     WTCLEA_e      WSSTCL
150700091006     C                   Z-ADD(H)  WTKGEA_e      WSSTKG
150800091006     C                   Z-ADD(H)  WTMCEA_e      WSSTMC
150900100402     c* spedizioni "E"
151000100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
151100100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
151200100402     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe
151300100402     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe
151400100402     c* spedizioni "H"
151500100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
151600100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
151700100402     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh
151800100402     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh
151900100402     c
152000091006     C                   WRITE     WSLSS550
152100091006     C                   ENDIF
152200091006     C*
152300091006     C     WTSPPA_e      IFGT      *ZERO
152400091007     C                   Z-ADD     §al           WSSDAL
152500110715     c*******            movel     'G'           WSSTIE
152600091006     C                   MOVE      'PA'          WSSTIP
152700091006     C                   Z-ADD     FIL(K)        WSSLIN
152800091006     C                   Z-ADD     WTSPPA_e      WSSTSP
152900091006     C                   Z-ADD     WTCLPA_e      WSSTCL
153000091006     C                   Z-ADD(H)  WTKGPA_e      WSSTKG
153100091006     C                   Z-ADD(H)  WTMCPA_e      WSSTMC
153200100402     c* spedizioni "E"
153300100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
153400100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
153500100402     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe
153600100402     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe
153700100402     c* spedizioni "H"
153800100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
153900100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
154000100402     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh
154100100402     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh
154200100402     c
154300091006     C                   WRITE     WSLSS550
154400091006     C                   ENDIF
154500091006
154600090601     C                   ENDIF
154700090206     c*
154800090206     c* riepilogo
154900090601     c                   if        Wtipo='P' or Wtipo='R'
155000110715     c                   if        §stat=' '
155100110715
155200090206     C     WTSPIP_r      IFGT      *ZERO
155300090206     C                   Z-ADD     §dal          WSSDAL
155400090601     c**                 movel     'R'           WSSTIE
155500090601     c                   movel     Wtipo         WSSTIE
155600090206     C                   MOVE      'IP'          WSSTIP
155700090206     C                   Z-ADD     FIL(K)        WSSLIN
155800090206     C                   Z-ADD     WTSPIP_r      WSSTSP
155900090206     C                   Z-ADD     WTCLIP_r      WSSTCL
156000090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
156100090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
156200100402     c* spedizioni "E"
156300100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
156400100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
156500100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
156600100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
156700100402     c* spedizioni "H"
156800100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
156900100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
157000100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
157100100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
157200100402     c
157300090206     C                   WRITE     WSLSS500
157400090206     C                   ENDIF
157500100402     c
157600090206     C     WTSPIA_r      IFGT      *ZERO
157700090206     C                   Z-ADD     §dal          WSSDAL
157800090601     c**                 movel     'R'           WSSTIE
157900090601     c                   movel     Wtipo         WSSTIE
158000090206     C                   MOVE      'IA'          WSSTIP
158100090206     C                   Z-ADD     FIL(K)        WSSLIN
158200090206     C                   Z-ADD     WTSPIA_r      WSSTSP
158300090206     C                   Z-ADD     WTCLIA_r      WSSTCL
158400090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
158500090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
158600100402     c* spedizioni "E"
158700100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
158800100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
158900100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
159000100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
159100100402     c* spedizioni "H"
159200100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
159300100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
159400100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
159500100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
159600100402     c
159700090206     C                   WRITE     WSLSS500
159800090206     C                   ENDIF
159900100402
160000090206     C     WTSPEI_R      IFGT      *ZERO
160100090206     C                   Z-ADD     §dal          WSSDAL
160200090601     c**                 movel     'R'           WSSTIE
160300090601     c                   movel     Wtipo         WSSTIE
160400090206     C                   MOVE      'EI'          WSSTIP
160500090206     C                   Z-ADD     FIL(K)        WSSLIN
160600090206     C                   Z-ADD     WTSPEI_r      WSSTSP
160700090206     C                   Z-ADD     WTCLEI_r      WSSTCL
160800090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
160900090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
161000100402     c* Spedizioni "E"
161100100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
161200100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
161300100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
161400100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
161500100402     c* Spedizioni "H"
161600100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
161700100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
161800100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
161900100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
162000100402     c
162100090206     C                   WRITE     WSLSS500
162200090206     C                   ENDIF
162300100402     c
162400090206     C     WTSPEE_r      IFGT      *ZERO
162500090206     C                   Z-ADD     §dal          WSSDAL
162600090601     c**                 movel     'R'           WSSTIE
162700090601     c                   movel     Wtipo         WSSTIE
162800090206     C                   MOVE      'EE'          WSSTIP
162900090206     C                   Z-ADD     FIL(K)        WSSLIN
163000090206     C                   Z-ADD     WTSPEE_r      WSSTSP
163100090206     C                   Z-ADD     WTCLEE_r      WSSTCL
163200090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
163300090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
163400100402     c* Spedizioni "E"
163500100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
163600100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
163700100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
163800100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
163900100402     c* Spedizioni "H"
164000100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
164100100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
164200100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
164300100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
164400100402     c
164500090206     C                   WRITE     WSLSS500
164600090206     C                   ENDIF
164700090206     C*
164800090206     C     WTSPED_r      IFGT      *ZERO
164900090206     C                   Z-ADD     §dal          WSSDAL
165000090601     c**                 movel     'R'           WSSTIE
165100090601     c                   movel     Wtipo         WSSTIE
165200090206     C                   MOVE      'ED'          WSSTIP
165300090206     C                   Z-ADD     FIL(K)        WSSLIN
165400090206     C                   Z-ADD     WTSPED_r      WSSTSP
165500090206     C                   Z-ADD     WTCLED_r      WSSTCL
165600090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
165700090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
165800100402     c* Spedizioni "E"
165900100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
166000100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
166100100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
166200100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
166300100402     c* Spedizioni "H"
166400100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
166500100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
166600100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
166700100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
166800100402     c
166900090206     C                   WRITE     WSLSS500
167000090206     C                   ENDIF
167100090206     C*
167200090206     C     WTSPEX_r      IFGT      *ZERO
167300090206     C                   Z-ADD     §dal          WSSDAL
167400090601     c**                 movel     'R'           WSSTIE
167500090601     c                   movel     Wtipo         WSSTIE
167600090206     C                   MOVE      'EX'          WSSTIP
167700090206     C                   Z-ADD     FIL(K)        WSSLIN
167800090206     C                   Z-ADD     WTSPEX_r      WSSTSP
167900090206     C                   Z-ADD     WTCLEX_r      WSSTCL
168000090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
168100090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
168200100402     c* Spedizioni "E"
168300100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
168400100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
168500100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
168600100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
168700100402     c* Spedizioni "H"
168800100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
168900100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
169000100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
169100100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
169200100402     c
169300090206     C                   WRITE     WSLSS500
169400090206     C                   ENDIF
169500090206     C*
169600090206     C     WTSPEA_r      IFGT      *ZERO
169700090206     C                   Z-ADD     §dal          WSSDAL
169800090601     c**                 movel     'R'           WSSTIE
169900090601     c                   movel     Wtipo         WSSTIE
170000090206     C                   MOVE      'EA'          WSSTIP
170100090206     C                   Z-ADD     FIL(K)        WSSLIN
170200090206     C                   Z-ADD     WTSPEA_r      WSSTSP
170300090206     C                   Z-ADD     WTCLEA_r      WSSTCL
170400090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
170500090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
170600100402     c* Spedizioni "E"
170700100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
170800100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
170900100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
171000100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
171100100402     c* Spedizioni "H"
171200100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
171300100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
171400100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
171500100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
171600100402     c
171700090206     C                   WRITE     WSLSS500
171800090206     C                   ENDIF
171900090206     C*
172000090206     C     WTSPPA_r      IFGT      *ZERO
172100090206     C                   Z-ADD     §dal          WSSDAL
172200090601     c**                 movel     'R'           WSSTIE
172300090601     c                   movel     Wtipo         WSSTIE
172400090206     C                   MOVE      'PA'          WSSTIP
172500090206     C                   Z-ADD     FIL(K)        WSSLIN
172600090206     C                   Z-ADD     WTSPPA_r      WSSTSP
172700090206     C                   Z-ADD     WTCLPA_r      WSSTCL
172800090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
172900090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
173000100402     c* Spedizioni "E"
173100100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
173200100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
173300100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
173400100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
173500100402     c* Spedizioni "H"
173600100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
173700100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
173800100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
173900100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
174000100402     c
174100090206     C                   WRITE     WSLSS500
174200090206     C                   ENDIF
174300110715     c
174400110715     c                   endif
174500110715     c
174600110715     c* Extra budget e clienti particolari
174700110715     c                   movel     wtipo         tipoC             1
174800110715     c                   if        §stat<>' ' and wtipo='R'
174900110715     c                   eval      tipoC='B'
175000110715     c                   endif
175100110715     c                   if        §stat<>' ' and wtipo='P'
175200110715     c                   eval      tipoC='C'
175300110715     c                   endif
175400110715     c
175500091006     C     WTSPIP_re     IFGT      *ZERO
175600091006     C                   Z-ADD     §dal          WSSDAL
175700110715     c                   movel     TipoC         WSSTIE
175800091006     C                   MOVE      'IP'          WSSTIP
175900091006     C                   Z-ADD     FIL(K)        WSSLIN
176000091006     C                   Z-ADD     WTSPIP_re     WSSTSP
176100091006     C                   Z-ADD     WTCLIP_re     WSSTCL
176200091006     C                   Z-ADD(H)  WTKGIP_re     WSSTKG
176300091006     C                   Z-ADD(H)  WTMCIP_re     WSSTMC
176400100402     c* Spedizioni "E"
176500100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
176600100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
176700100402     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe
176800100402     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe
176900100402     c* Spedizioni "H"
177000100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
177100100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
177200100402     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh
177300100402     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh
177400100402     c
177500091006     C                   WRITE     WSLSS550
177600091006     C                   ENDIF
177700100402     c
177800091006     C     WTSPIA_re     IFGT      *ZERO
177900091006     C                   Z-ADD     §dal          WSSDAL
178000110715     c                   movel     tipoC         WSSTIE
178100091006     C                   MOVE      'IA'          WSSTIP
178200091006     C                   Z-ADD     FIL(K)        WSSLIN
178300091006     C                   Z-ADD     WTSPIA_re     WSSTSP
178400091006     C                   Z-ADD     WTCLIA_re     WSSTCL
178500091006     C                   Z-ADD(H)  WTKGIA_re     WSSTKG
178600091006     C                   Z-ADD(H)  WTMCIA_re     WSSTMC
178700100402     c* Spedizioni "E"
178800100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
178900100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
179000100402     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe
179100100402     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe
179200100402     c* Spedizioni "H"
179300100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
179400100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
179500100402     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh
179600100402     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh
179700100402     c
179800091006     C                   WRITE     WSLSS550
179900091006     C                   ENDIF
180000100402     c
180100091006     C     WTSPEI_Re     IFGT      *ZERO
180200091006     C                   Z-ADD     §dal          WSSDAL
180300110715     c                   movel     tipoC         WSSTIE
180400091006     C                   MOVE      'EI'          WSSTIP
180500091006     C                   Z-ADD     FIL(K)        WSSLIN
180600091006     C                   Z-ADD     WTSPEI_re     WSSTSP
180700091006     C                   Z-ADD     WTCLEI_re     WSSTCL
180800091006     C                   Z-ADD(H)  WTKGEI_re     WSSTKG
180900091006     C                   Z-ADD(H)  WTMCEI_re     WSSTMC
181000100402     c* Spedizioni "E"
181100100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
181200100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
181300100402     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe
181400100402     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe
181500100402     c* Spedizioni "H"
181600100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
181700100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
181800100402     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh
181900100402     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh
182000100402     c
182100091006     C                   WRITE     WSLSS550
182200091006     C                   ENDIF
182300100402     c
182400091006     C     WTSPEE_re     IFGT      *ZERO
182500091006     C                   Z-ADD     §dal          WSSDAL
182600110715     c                   movel     tipoC         WSSTIE
182700091006     C                   MOVE      'EE'          WSSTIP
182800091006     C                   Z-ADD     FIL(K)        WSSLIN
182900091006     C                   Z-ADD     WTSPEE_re     WSSTSP
183000091006     C                   Z-ADD     WTCLEE_re     WSSTCL
183100091006     C                   Z-ADD(H)  WTKGEE_re     WSSTKG
183200091006     C                   Z-ADD(H)  WTMCEE_re     WSSTMC
183300100402     c* Spedizioni "E"
183400100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
183500100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
183600100402     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe
183700100402     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe
183800100402     c* Spedizioni "h"
183900100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
184000100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
184100100402     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh
184200100402     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh
184300100402     c
184400091006     C                   WRITE     WSLSS550
184500091006     C                   ENDIF
184600091006     C*
184700091006     C     WTSPED_re     IFGT      *ZERO
184800091006     C                   Z-ADD     §dal          WSSDAL
184900110715     c                   movel     tipoC         WSSTIE
185000091006     C                   MOVE      'ED'          WSSTIP
185100091006     C                   Z-ADD     FIL(K)        WSSLIN
185200091006     C                   Z-ADD     WTSPED_re     WSSTSP
185300091006     C                   Z-ADD     WTCLED_re     WSSTCL
185400091006     C                   Z-ADD(H)  WTKGED_re     WSSTKG
185500091006     C                   Z-ADD(H)  WTMCED_re     WSSTMC
185600100402     c* Spedizioni "E"
185700100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
185800100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
185900100402     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe
186000100402     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe
186100100402     c* Spedizioni "H"
186200100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
186300100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
186400100402     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh
186500100402     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh
186600100402     c
186700091006     C                   WRITE     WSLSS550
186800091006     C                   ENDIF
186900091006     C*
187000091006     C     WTSPEX_re     IFGT      *ZERO
187100091006     C                   Z-ADD     §dal          WSSDAL
187200110715     c                   movel     tipoC         WSSTIE
187300091006     C                   MOVE      'EX'          WSSTIP
187400091006     C                   Z-ADD     FIL(K)        WSSLIN
187500091006     C                   Z-ADD     WTSPEX_re     WSSTSP
187600091006     C                   Z-ADD     WTCLEX_re     WSSTCL
187700091006     C                   Z-ADD(H)  WTKGEX_re     WSSTKG
187800091006     C                   Z-ADD(H)  WTMCEX_re     WSSTMC
187900100402     c* Spedizioni "E"
188000100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
188100100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
188200100402     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe
188300100402     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe
188400100402     c* Spedizioni "H"
188500100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
188600100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
188700100402     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh
188800100402     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh
188900100402     c
189000091006     C                   WRITE     WSLSS550
189100091006     C                   ENDIF
189200091006     C*
189300091006     C     WTSPEA_re     IFGT      *ZERO
189400091006     C                   Z-ADD     §dal          WSSDAL
189500110715     c                   movel     tipoC         WSSTIE
189600091006     C                   MOVE      'EA'          WSSTIP
189700091006     C                   Z-ADD     FIL(K)        WSSLIN
189800091006     C                   Z-ADD     WTSPEA_re     WSSTSP
189900091006     C                   Z-ADD     WTCLEA_re     WSSTCL
190000091006     C                   Z-ADD(H)  WTKGEA_re     WSSTKG
190100091006     C                   Z-ADD(H)  WTMCEA_re     WSSTMC
190200100402     c* Spedizioni "E"
190300100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
190400100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
190500100402     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe
190600100402     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe
190700100402     c* Spedizioni "H"
190800100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
190900100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
191000100402     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh
191100100402     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh
191200100402     c
191300091006     C                   WRITE     WSLSS550
191400091006     C                   ENDIF
191500091006     C*
191600091006     C     WTSPPA_re     IFGT      *ZERO
191700091006     C                   Z-ADD     §dal          WSSDAL
191800110715     c                   movel     tipoC         WSSTIE
191900091006     C                   MOVE      'PA'          WSSTIP
192000091006     C                   Z-ADD     FIL(K)        WSSLIN
192100091006     C                   Z-ADD     WTSPPA_re     WSSTSP
192200091006     C                   Z-ADD     WTCLPA_re     WSSTCL
192300091006     C                   Z-ADD(H)  WTKGPA_re     WSSTKG
192400091006     C                   Z-ADD(H)  WTMCPA_re     WSSTMC
192500100402     c* Spedizioni "E"
192600100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
192700100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
192800100402     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe
192900100402     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe
193000100402     c* Spedizioni "H"
193100100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
193200100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
193300100402     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh
193400100402     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh
193500100402     c
193600091006     C                   WRITE     WSLSS550
193700091006     C                   ENDIF
193800091006     C                   ENDIF
193900100402
194000090206     c                   enddo
194100090206     c                   endsr
194200970527     C**********************************************************
194300970529     C** CARICAMENTO DATI INIZIALI
194400970527     C**********************************************************
194500970529     C     CARINZ        BEGSR
194600970905     C                   SETOFF                                       90
194700970529     C     KTAS          KLIST
194800970529     C                   KFLD                    WAAS
194900970529     C                   KFLD                    WMGS
195000970529     C     KTAB          KLIST
195100970529     C                   KFLD                    TBLKUT
195200970529     C                   KFLD                    TBLCOD
195300970529     C                   KFLD                    TBLKEY
195400970529     C     KTB           KLIST
195500970529     C                   KFLD                    TBLKUT
195600970529     C                   KFLD                    TBLCOD
195700970529     C                   Z-ADD     1             TBLKUT
195800090204     c     kwfsmp        klist
195900090204     c                   kfld                    k_fil
196000090204     c                   kfld                    k_tip
196100090204     c                   kfld                    k_dat
196200090205     C     kar5          KLIST
196300090205     C                   KFLD                    tasaas
196400090205     C                   KFLD                    taslnp
196500090205     C                   KFLD                    tasnrs
196600090205     C                   KFLD                    tasnsp
196700090205     C                   KFLD                    k_trd
196800090209     C                   KFLD                    tasflp
196900090915     c* apertura file di stampa congelato in SAVE
197000090915     C* ESEGUO OVRPRTF
197100090915     C                   Z-ADD     61            LUNG             15 5
197200090915     C                   MOVEL     *BLANKS       COMMAN           80
197300090915     C                   MOVEA     CMD           COMMAN
197400090915     C                   CALL      'QCMDEXC'                            H1
197500090915     C                   PARM                    COMMAN
197600090915     C                   PARM                    LUNG
197700090915     C*
197800090915     C                   OPEN      QSYSPRT
197900090915     C*
198000970529     C*
198100970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
198200970527     C** PULIZIA DS
198300980826     C                   DO        400           K                 3 0
198400970527     C     K             OCCUR     DSFIL
198500970527     C                   CLEAR                   DSFIL
198600970527     C                   END
198700100402     C                   DO        400           K                 3 0
198800100402     C     K             OCCUR     DSFIL_e
198900100402     C                   CLEAR                   DSFIL_e
199000100402     C                   END
199100100402     C                   DO        400           K                 3 0
199200100402     C     K             OCCUR     DSFIL_h
199300100402     C                   CLEAR                   DSFIL_h
199400100402     C                   END
199500090206     C                   DO        400           K                 3 0
199600090206     C     K             OCCUR     DSFIL_smp
199700090206     C                   CLEAR                   DSFIL_smp
199800090206     C                   END
199900970527     C*
200000970527     C                   Z-ADD     0             K
200100970527     C     *LOVAL        SETLL     AZORG01L
200200970527     C                   DO        *HIVAL
200300090204     C                   READ      AZORG01L
200400090204     c                   if        %eof(azorg01l)
200500090204     c                   leave
200600090204     c                   endif
200700970527     C     ORGFAG        IFEQ      'F'
200800970527     C     ORGFAG        OREQ      'A'
200900970527     C                   ADD       1             K
201000970527     C                   MOVEL     ORGFIL        FIL(K)
201100090204     C                   MOVEL     ORGdes        DIL(K)
201200090204     C                   z-add     ORGcar        car(K)
201300090204     c                   movel     orgde8        og148
201400090206     c                   if        §oglpo='1' or §oglpo='2'
201500090206     c                   move      §oglpo        lpo(K)
201600090206     c                   else
201700090206     c                   z-add     2             lpo(K)
201800090206     c                   endif
201900090204     c                   movel     orgde3        og143
202000090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
202100090209     c**                 move      'E'           tif(K)
202200090209     c**                 else
202300090209     c**                 move      'I'           tif(K)
202400090209     c**                 endif
202500090209     c                   select
202600090209     c                   when      §ogntw='DPD'
202700090209     c                   move      'D'           tif(K)
202800090209     c                   when      §ogntw='EEX'
202900090209     c                   move      'X'           tif(K)
203000090209     c                   when      §ogntw='FED'
203100090209     c                   move      'F'           tif(K)
203200090209     c                   other
203300090209     c                   move      'I'           tif(K)
203400090209     c                   endsl
203500970527     C                   ENDIF
203600090204     C                   ENDDO
203700970529     C*
203800970529     C                   Z-ADD     0             K
203900970529     C                   MOVEL     'TB'          TBLCOD
204000970529     C     KTB           SETLL     TABEL00F
204100970529     C                   DO        *HIVAL
204200090204     C     KTB           READE     TABEL00F
204300090204     c                   if        %eof(tabel00f)
204400090204     c                   leave
204500090204     c                   endif
204600970529     C     TBLFLG        IFNE      '*'
204700970529     C                   MOVEL     TBLUNI        DSTB
204800970529     C     §TBRBL        IFEQ      'N'
204900970529     C     §TBRBL        OREQ      'S'
205000970529     C                   ADD       1             K
205100970529     C                   MOVEL     TBLKEY        TBO(K)
205200970529     C                   ENDIF
205300970529     C                   ENDIF
205400090204     C                   ENDDO
205500091006     C*
205600091006     c* Lettura TABELLA clienti extra budget
205700110711     c                   if        §stat=' '
205800091006     C                   Z-ADD     0             K
205900091006     C                   MOVEL     'CXB'         TBecod
206000091006     C     tbecod        SETLL     tntbe01l
206100091006     C                   DO        *HIVAL
206200091006     C     tbecod        READE     tntbe01l
206300091006     c                   if        %eof(tntbe01l)
206400091006     c                   leave
206500091006     c                   endif
206600091006
206700091006     C     tbeatb        IFeq      ' '
206800091006     C                   MOVEL     TBeuni        Dcxb
206900091006     C                   ADD       1             K
207000091006     C                   MOVEL     TBeKE1        ksc_ex(k)
207100091007     C                   MOVE      §cxbctr       ksc_ex(k)
207200091006     C                   ENDIF
207300091006     C                   ENDDO
207400110711     c
207500110711     c                   else
207600110711     C                   Z-ADD     0             K
207700110711     C                   MOVEL     'CXQ'         TBecod
207800110711     C     tbecod        SETLL     tntbe01l
207900110711     C                   DO        *HIVAL
208000110711     C     tbecod        READE     tntbe01l
208100110711     c                   if        %eof(tntbe01l)
208200110711     c                   leave
208300110711     c                   endif
208400110711
208500110711     C     tbeatb        IFeq      ' '
208600110711     C                   ADD       1             K
208700110711     C                   MOVEL     TBeKE2        ksc_ex(k)
208800110711     C                   ENDIF
208900110711     C                   ENDDO
209000110711     c                   endif
209100110711     c*
209200110711     c* Controllo riempimento schiera
209300110715     c                   movel     kpjbu         savkpjbu
209400110711     c                   clear                   trul0sds
209500110711     c                   eval      i0stla='L'
209600110711     c                   eval      i0sski='KSC_EX'
209700110711     c                   eval      i0sele=%elem(ksc_ex)
209800110711     c                   eval      i0spie=k
209900110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
210000110711     c                   eval      i0ssif=knsif
210100110711     c                   eval      i0spgm='TNSSL3R2'
210200110711     c                   movel     trul0sds      kpjbu
210300110711     c                   call      'TRUL0SR'
210400110711     c                   parm                    kpjba
210500110715
210600110715     c                   movel     savkpjbu      kpjbu
210700090601     c
210800090204     c* data elaborazione nel formato AAAA/MM/GG
210900090204     c                   time                    dataiso
211000090204     c                   move      dataiso       wdatael
211100110711     c
211200110711     c                   if        §stat=' '
211300090601     c*
211400090601     c* Pulisco i file per i dati che devo rielaborare
211500090601     c                   eval      wrkgetlista =
211600090601     c                             'delete from WSLSS50F where wsstie= '''   +
211700090603     c                              Wtipo +  ''''
211800090601     c                   if        Wtipo='R'
211900090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
212000090603     c                              ' or  wsstie=''G'''
212100090601     c                   endif
212200090609     c*
212300090601     C/EXEC SQL
212400090601     C+ execute immediate :wrkgetlista
212500090601     C/END-EXEC
212600090609     c
212700090609     c* Pulisco i file per i dati che devo rielaborare
212800090609     c                   clear                   wrkgetlista
212900090609     c                   eval      wrkgetlista =
213000090609     c                             'delete from WFSMP00F where wmptip= '''   +
213100090609     c                              Wtipo +  ''''
213200090609     c                   if        Wtipo='R'
213300090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
213400090609     c                              ' or  wmptip=''G'''
213500090609     c                   endif
213600110711     c
213700110711     c                   endif
213800090609     c*
213900090609     C/EXEC SQL
214000090609     C+ execute immediate :wrkgetlista
214100090609     C/END-EXEC
214200091007     c*
214300091007     c* Pulisco il file clienti extra budget
214400091007     c                   clear                   wrkgetlista
214500110711     c
214600110711     c                   if        §stat=' '
214700091007     c                   eval      wrkgetlista =
214800091007     c                             'delete from WSLSS55F where wsstie= '''   +
214900091007     c                              Wtipo +  ''''
215000091007     c                   if        Wtipo='R'
215100091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
215200091007     c                              ' or  wsstie=''G'''
215300091007     c                   endif
215400111028     C/EXEC SQL
215500111028     C+ execute immediate :wrkgetlista
215600111028     C/END-EXEC
215700110711     c
215800110711     c                   else
215900111028     c
216000111028     c* Pulisco il file clienti particolari
216100110711     c                   select
216200110711     c                   when      Wtipo='R'
216300110711     c                   eval      wrkgetlista =
216400110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
216500110711     c                               wsstie= ''B'''
216600110711     c                   when      Wtipo='G'
216700110711     c                   eval      wrkgetlista =
216800110711     c                             'delete from WSLSS55F where wsstie= ''A'''
216900110711     c                   when      Wtipo='P'
217000110711     c                   eval      wrkgetlista =
217100110711     c                             'delete from WSLSS55F where wsstie= ''C'''
217200110711     c                   endsl
217300110711     c
217400091007     C/EXEC SQL
217500091007     C+ execute immediate :wrkgetlista
217600091007     C/END-EXEC
217700111028     c
217800111028     c* Preparo la lettura dei soli clienti particolari con SQL
217900111028     c                   clear                   wrkgetlista
218000111028     c                   eval      wrkgetlista =
218100111028     c                             'select * from titas00f where +
218200111028     c                             ((tasaas*10000) +tasmgs) >=' +
218300111028     c                             %editc(§dal:'X')+ ' and +
218400111028     c                             ((tasaas*10000) +tasmgs) <=' +
218500111028     c                             %editc(§al :'X')+ ' and tasccm in('
218600111028      * ciclo sulla sk dei cod bolla di recupero
218700111028     c                   Do        500           XX                3 0
218800111028     c                   If        ksc_ex(XX) = *blanks
218900111028     c                   Leave
219000111028     c                   EndIf
219100111028     c
219200111028     c                   If        XX >1
219300111028     c                   eval      wrkgetlista =
219400111028     c                             %trim(wrkgetlista) + ','
219500111028     c                   EndIf
219600111028     c
219700111028     c                   eval      wrkgetlista =
219800111028     c                             %trim(wrkgetlista) + ' ''' +
219900111028     c                             %subst(ksc_ex(xx):1:7) + ''''
220000111028     c                   EndDo
220100111028     c
220200111028     c                   Eval      wrkgetlista =
220300111028     c                             %trim(wrkgetlista) + ')'
220400111028     c
220500111028     c
220600111028     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
220700111028     c                             'select * from titas10f where +
220800111028     c                             ((tasaas*10000) +tasmgs) >=' +
220900111028     c                             %editc(§dal:'X')+ ' and +
221000111028     c                             ((tasaas*10000) +tasmgs) <=' +
221100111028     c                             %editc(§al :'X')+ ' and tasccm in('
221200111028      * ciclo sulla sk dei cod bolla di recupero
221300111028     c                   Do        500           XX                3 0
221400111028     c                   If        ksc_ex(XX) = *blanks
221500111028     c                   Leave
221600111028     c                   EndIf
221700111028     c
221800111028     c                   If        XX >1
221900111028     c                   eval      wrkgetlista =
222000111028     c                             %trim(wrkgetlista) + ','
222100111028     c                   EndIf
222200111028     c
222300111028     c                   eval      wrkgetlista =
222400111028     c                             %trim(wrkgetlista) + ' ''' +
222500111028     c                             %subst(ksc_ex(xx):1:7) + ''''
222600111028     c                   EndDo
222700111028     c
222800111028     c                   Eval      wrkgetlista =
222900111028     c                             %trim(wrkgetlista) + ')'
223000111028     c
223100111028     c                   EndIf
223200111028     c
223300970527     C                   ENDSR
223400100402     c****************************************************************************
223500100402     c     SpeITA        BEGSR
223600100402     c* giornaliero
223700100402     c                   if        (tasaas*10000+tasmgs)=§al
223800100402     C                   ADD       1             WTSPIP
223900100402     C                   ADD       TASNCL        WTCLIP
224000100402     C                   ADD       w_peso        WTKGIP
224100100402     C                   ADD       TASVLF        WTMCIP
224200100402     c* tipo servizio  "E"
224300100402     c                   if        tastsp='E'
224400100402     C                   ADD       1             eWTSPIP
224500100402     C                   ADD       TASNCL        eWTCLIP
224600100402     C                   ADD       w_peso        eWTKGIP
224700100402     C                   ADD       TASVLF        eWTMCIP
224800100402     c                   endif
224900100402     c* tipo servizio  "H"
225000100402     c                   if        tastsp='H'
225100100402     C                   ADD       1             hWTSPIP
225200100402     C                   ADD       TASNCL        hWTCLIP
225300100402     C                   ADD       w_peso        hWTKGIP
225400100402     C                   ADD       TASVLF        hWTMCIP
225500100402     c                   endif
225600100402     c**
225700100402     c* Extra budget
225800100402     c                   if        *in35
225900100402     C                   ADD       1             WTSPIP_e
226000100402     C                   ADD       TASNCL        WTCLIP_e
226100100402     C                   ADD       w_peso        WTKGIP_e
226200100402     C                   ADD       TASVLF        WTMCIP_e
226300100402     c* tipo servizio  "E"
226400100402     c                   if        tastsp='E'
226500100402     C                   ADD       1             eWTSPIP_e
226600100402     C                   ADD       TASNCL        eWTCLIP_e
226700100402     C                   ADD       w_peso        eWTKGIP_e
226800100402     C                   ADD       TASVLF        eWTMCIP_e
226900100402     c                   endif
227000100402     c* tipo servizio  "H"
227100100402     c                   if        tastsp='H'
227200100402     C                   ADD       1             hWTSPIP_e
227300100402     C                   ADD       TASNCL        hWTCLIP_e
227400100402     C                   ADD       w_peso        hWTKGIP_e
227500100402     C                   ADD       TASVLF        hWTMCIP_e
227600100402     c                   endif
227700100402
227800100402     c                   endif
227900100402     c                   endif
228000100402     c* riepilogo
228100100402     C                   ADD       1             WTSPIP_r
228200100402     C                   ADD       TASNCL        WTCLIP_r
228300100402     C                   ADD       w_peso        WTKGIP_r
228400100402     C                   ADD       TASVLF        WTMCIP_r
228500100402     c* tipo servizio  "E"
228600100402     c                   if        tastsp='E'
228700100402     C                   ADD       1             eWTSPIP_r
228800100402     C                   ADD       TASNCL        eWTCLIP_r
228900100402     C                   ADD       w_peso        eWTKGIP_r
229000100402     C                   ADD       TASVLF        eWTMCIP_r
229100100402     c                   endif
229200100402     c* tipo servizio  "H"
229300100402     c                   if        tastsp='H'
229400100402     C                   ADD       1             hWTSPIP_r
229500100402     C                   ADD       TASNCL        hWTCLIP_r
229600100402     C                   ADD       w_peso        hWTKGIP_r
229700100402     C                   ADD       TASVLF        hWTMCIP_r
229800100402     c                   endif
229900100402     c* Extra budget
230000100402     c                   if        *in35
230100100402     C                   ADD       1             WTSPIP_re
230200100402     C                   ADD       TASNCL        WTCLIP_re
230300100402     C                   ADD       w_peso        WTKGIP_re
230400100402     C                   ADD       TASVLF        WTMCIP_re
230500100402     c* tipo servizio  "E"
230600100402     c                   if        tastsp='E'
230700100402     C                   ADD       1             eWTSPIP_re
230800100402     C                   ADD       TASNCL        eWTCLIP_re
230900100402     C                   ADD       w_peso        eWTKGIP_re
231000100402     C                   ADD       TASVLF        eWTMCIP_re
231100100402     c                   endif
231200100402     c* tipo servizio  "H"
231300100402     c                   if        tastsp='H'
231400100402     C                   ADD       1             hWTSPIP_re
231500100402     C                   ADD       TASNCL        hWTCLIP_re
231600100402     C                   ADD       w_peso        hWTKGIP_re
231700100402     C                   ADD       TASVLF        hWTMCIP_re
231800100402     c                   endif
231900100402     c
232000100402     c                   endif
232100100402     c                   ENDSR
232200100402     c****************************************************************************
232300100402     c     SpeIMPORT     BEGSR
232400100402     c* giornaliero
232500100402     c                   if        (tasaas*10000+tasmgs)=§al
232600100402     C                   ADD       1             WTSPEI
232700100402     C                   ADD       TASNCL        WTCLEI
232800100402     C                   ADD       w_peso        WTKGEI
232900100402     C                   ADD       TASVLF        WTMCEI
233000100402     c* tipo servizio  "E"
233100100402     c                   if        tastsp='E'
233200100402     C                   ADD       1             eWTSPEI
233300100402     C                   ADD       TASNCL        eWTCLEI
233400100402     C                   ADD       w_peso        eWTKGEI
233500100402     C                   ADD       TASVLF        eWTMCEI
233600100402     c                   endif
233700100402     c* tipo servizio  "H"
233800100402     c                   if        tastsp='H'
233900100402     C                   ADD       1             hWTSPEI
234000100402     C                   ADD       TASNCL        hWTCLEI
234100100402     C                   ADD       w_peso        hWTKGEI
234200100402     C                   ADD       TASVLF        hWTMCEI
234300100402     c                   endif
234400100402     c* Extra Budget
234500100402     c                   if        *in35
234600100402     C                   ADD       1             WTSPEI_e
234700100402     C                   ADD       TASNCL        WTCLEI_e
234800100402     C                   ADD       w_peso        WTKGEI_e
234900100402     C                   ADD       TASVLF        WTMCEI_e
235000100402     c* tipo servizio  "E"
235100100402     c                   if        tastsp='E'
235200100402     C                   ADD       1             eWTSPEI_e
235300100402     C                   ADD       TASNCL        eWTCLEI_e
235400100402     C                   ADD       w_peso        eWTKGEI_e
235500100402     C                   ADD       TASVLF        eWTMCEI_e
235600100402     c                   endif
235700100402     c* tipo servizio  "H"
235800100402     c                   if        tastsp='H'
235900100402     C                   ADD       1             hWTSPEI_e
236000100402     C                   ADD       TASNCL        hWTCLEI_e
236100100402     C                   ADD       w_peso        hWTKGEI_e
236200100402     C                   ADD       TASVLF        hWTMCEI_e
236300100402     c                   endif
236400100402     c
236500100402     c                   endif
236600100402     c                   endif
236700100402     c* riepilogo
236800100402     C                   ADD       1             WTSPEI_r
236900100402     C                   ADD       TASNCL        WTCLEI_r
237000100402     C                   ADD       w_peso        WTKGEI_r
237100100402     C                   ADD       TASVLF        WTMCEI_r
237200100402     c* tipo servizio  "E"
237300100402     c                   if        tastsp='E'
237400100402     C                   ADD       1             eWTSPEI_r
237500100402     C                   ADD       TASNCL        eWTCLEI_r
237600100402     C                   ADD       w_peso        eWTKGEI_r
237700100402     C                   ADD       TASVLF        eWTMCEI_r
237800100402     c                   endif
237900100402     c* tipo servizio  "H"
238000100402     c                   if        tastsp='H'
238100100402     C                   ADD       1             hWTSPEI_r
238200100402     C                   ADD       TASNCL        hWTCLEI_r
238300100402     C                   ADD       w_peso        hWTKGEI_r
238400100402     C                   ADD       TASVLF        hWTMCEI_r
238500100402     c                   endif
238600100402     c* extra budget
238700100402     c                   if        *in35
238800100402     C                   ADD       1             WTSPEI_re
238900100402     C                   ADD       TASNCL        WTCLEI_re
239000100402     C                   ADD       w_peso        WTKGEI_re
239100100402     C                   ADD       TASVLF        WTMCEI_re
239200100402     c* tipo servizio  "E"
239300100402     c                   if        tastsp='E'
239400100402     C                   ADD       1             eWTSPEI_re
239500100402     C                   ADD       TASNCL        eWTCLEI_re
239600100402     C                   ADD       w_peso        eWTKGEI_re
239700100402     C                   ADD       TASVLF        eWTMCEI_re
239800100402     c                   endif
239900100402     c* tipo servizio  "H"
240000100402     c                   if        tastsp='H'
240100100402     C                   ADD       1             hWTSPEI_re
240200100402     C                   ADD       TASNCL        hWTCLEI_re
240300100402     C                   ADD       w_peso        hWTKGEI_re
240400100402     C                   ADD       TASVLF        hWTMCEI_re
240500100402     c                   endif
240600100402     c                   endif
240700100402     c                   ENDSR
240800100402     c****************************************************************************
240900100402     c     speEXPRESS    BEGSR
241000100402     c* giornaliero
241100100402     c                   if        (tasaas*10000+tasmgs)=§al
241200100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
241300100402     C                   ADD       TASNCL        WTCLEE
241400100402     C                   ADD       w_peso        WTKGEE
241500100402     C                   ADD       TASVLF        WTMCEE
241600100402     c* tipo servizio  "E"
241700100402     c                   if        tastsp='E'
241800100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
241900100402     C                   ADD       TASNCL        eWTCLEE
242000100402     C                   ADD       w_peso        eWTKGEE
242100100402     C                   ADD       TASVLF        eWTMCEE
242200100402     c                   endif
242300100402     c* tipo servizio  "H"
242400100402     c                   if        tastsp='H'
242500100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
242600100402     C                   ADD       TASNCL        hWTCLEE
242700100402     C                   ADD       w_peso        hWTKGEE
242800100402     C                   ADD       TASVLF        hWTMCEE
242900100402     c                   endif
243000100402     c* extra budget
243100100402     c                   if        *in35
243200100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
243300100402     C                   ADD       TASNCL        WTCLEE_e
243400100402     C                   ADD       w_peso        WTKGEE_e
243500100402     C                   ADD       TASVLF        WTMCEE_e
243600100402     c* tipo servizio  "E"
243700100402     c                   if        tastsp='E'
243800100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
243900100402     C                   ADD       TASNCL        eWTCLEE_e
244000100402     C                   ADD       w_peso        eWTKGEE_e
244100100402     C                   ADD       TASVLF        eWTMCEE_e
244200100402     c                   endif
244300100402     c* tipo servizio  "H"
244400100402     c                   if        tastsp='H'
244500100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
244600100402     C                   ADD       TASNCL        hWTCLEE_e
244700100402     C                   ADD       w_peso        hWTKGEE_e
244800100402     C                   ADD       TASVLF        hWTMCEE_e
244900100402     c                   endif
245000100402     c
245100100402     c                   endif
245200100402     c                   endif
245300100402     c* riepilogo
245400100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
245500100402     C                   ADD       TASNCL        WTCLEE_r
245600100402     C                   ADD       w_peso        WTKGEE_r
245700100402     C                   ADD       TASVLF        WTMCEE_r
245800100402     c* tipo servizio  "E"
245900100402     c                   if        tastsp='E'
246000100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
246100100402     C                   ADD       TASNCL        eWTCLEE_r
246200100402     C                   ADD       w_peso        eWTKGEE_r
246300100402     C                   ADD       TASVLF        eWTMCEE_r
246400100402     c                   endif
246500100402     c* tipo servizio  "H"
246600100402     c                   if        tastsp='H'
246700100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
246800100402     C                   ADD       TASNCL        hWTCLEE_r
246900100402     C                   ADD       w_peso        hWTKGEE_r
247000100402     C                   ADD       TASVLF        hWTMCEE_r
247100100402     c                   endif
247200100402     c* extra budget
247300100402     c                   if        *in35
247400100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
247500100402     C                   ADD       TASNCL        WTCLEE_re
247600100402     C                   ADD       w_peso        WTKGEE_re
247700100402     C                   ADD       TASVLF        WTMCEE_re
247800100402     c* tipo servizio  "E"
247900100402     c                   if        tastsp='E'
248000100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
248100100402     C                   ADD       TASNCL        eWTCLEE_re
248200100402     C                   ADD       w_peso        eWTKGEE_re
248300100402     C                   ADD       TASVLF        eWTMCEE_re
248400100402     c                   endif
248500100402     c* tipo servizio  "H"
248600100402     c                   if        tastsp='H'
248700100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
248800100402     C                   ADD       TASNCL        hWTCLEE_re
248900100402     C                   ADD       w_peso        hWTKGEE_re
249000100402     C                   ADD       TASVLF        hWTMCEE_re
249100100402     c                   endif
249200100402     c                   endif
249300100402     c
249400100402     c                   ENDSR
249500100402     c****************************************************************************
249600100402     c     speDPD        BEGSR
249700100402     c* giornaliero
249800100402     c                   if        (tasaas*10000+tasmgs)=§al
249900100402     C                   ADD       1             WTSPED                         DPD
250000100402     C                   ADD       TASNCL        WTCLED
250100100402     C                   ADD       w_peso        WTKGED
250200100402     C                   ADD       TASVLF        WTMCED
250300100402     c* tipo servizio  "E"
250400100402     c                   if        tastsp='E'
250500100402     C                   ADD       1             eWTSPED                         DPD
250600100402     C                   ADD       TASNCL        eWTCLED
250700100402     C                   ADD       w_peso        eWTKGED
250800100402     C                   ADD       TASVLF        eWTMCED
250900100402     c                   endif
251000100402     c* tipo servizio  "H"
251100100402     c                   if        tastsp='H'
251200100402     C                   ADD       1             hWTSPED                         DPD
251300100402     C                   ADD       TASNCL        hWTCLED
251400100402     C                   ADD       w_peso        hWTKGED
251500100402     C                   ADD       TASVLF        hWTMCED
251600100402     c                   endif
251700100402     c* extra budget
251800100402     c                   if        *in35
251900100402     C                   ADD       1             WTSPED_e                       DPD
252000100402     C                   ADD       TASNCL        WTCLED_e
252100100402     C                   ADD       w_peso        WTKGED_e
252200100402     C                   ADD       TASVLF        WTMCED_e
252300100402     c* tipo servizio  "E"
252400100402     c                   if        tastsp='E'
252500100402     C                   ADD       1             eWTSPED_e                       DPD
252600100402     C                   ADD       TASNCL        eWTCLED_e
252700100402     C                   ADD       w_peso        eWTKGED_e
252800100402     C                   ADD       TASVLF        eWTMCED_e
252900100402     c                   endif
253000100402     c* tipo servizio  "H"
253100100402     c                   if        tastsp='H'
253200100402     C                   ADD       1             hWTSPED_e                       DPD
253300100402     C                   ADD       TASNCL        hWTCLED_e
253400100402     C                   ADD       w_peso        hWTKGED_e
253500100402     C                   ADD       TASVLF        hWTMCED_e
253600100402     c                   endif
253700100402     c
253800100402     c                   endif
253900100402     c                   endif
254000100402     c* riepilogo
254100100402     C                   ADD       1             WTSPED_r                       DPD
254200100402     C                   ADD       TASNCL        WTCLED_r
254300100402     C                   ADD       w_peso        WTKGED_r
254400100402     C                   ADD       TASVLF        WTMCED_r
254500100402     c* tipo servizio  "E"
254600100402     c                   if        tastsp='E'
254700100402     C                   ADD       1             eWTSPED_r                       DPD
254800100402     C                   ADD       TASNCL        eWTCLED_r
254900100402     C                   ADD       w_peso        eWTKGED_r
255000100402     C                   ADD       TASVLF        eWTMCED_r
255100100402     c                   endif
255200100402     c* tipo servizio  "H"
255300100402     c                   if        tastsp='H'
255400100402     C                   ADD       1             hWTSPED_r                       DPD
255500100402     C                   ADD       TASNCL        hWTCLED_r
255600100402     C                   ADD       w_peso        hWTKGED_r
255700100402     C                   ADD       TASVLF        hWTMCED_r
255800100402     c                   endif
255900100402     c* extra budget
256000100402     c                   if        *in35
256100100402     C                   ADD       1             WTSPED_re                       DPD
256200100402     C                   ADD       TASNCL        WTCLED_re
256300100402     C                   ADD       w_peso        WTKGED_re
256400100402     C                   ADD       TASVLF        WTMCED_re
256500100402     c* tipo servizio  "E"
256600100402     c                   if        tastsp='E'
256700100402     C                   ADD       1             eWTSPED_re                       DPD
256800100402     C                   ADD       TASNCL        eWTCLED_re
256900100402     C                   ADD       w_peso        eWTKGED_re
257000100402     C                   ADD       TASVLF        eWTMCED_re
257100100402     c                   endif
257200100402     c* tipo servizio  "H"
257300100402     c                   if        tastsp='H'
257400100402     C                   ADD       1             hWTSPED_re                       DPD
257500100402     C                   ADD       TASNCL        hWTCLED_re
257600100402     C                   ADD       w_peso        hWTKGED_re
257700100402     C                   ADD       TASVLF        hWTMCED_re
257800100402     c                   endif
257900100402     c                   endif
258000100402     c                   ENDSR
258100100402     c****************************************************************************
258200100402     c     speFEDEX      BEGSR
258300100402     c* giornaliero
258400100402     c                   if        (tasaas*10000+tasmgs)=§al
258500100402     C                   ADD       1             WTSPEX                         FEDEX
258600100402     C                   ADD       TASNCL        WTCLEX
258700100402     C                   ADD       w_peso        WTKGEX
258800100402     C                   ADD       TASVLF        WTMCEX
258900100402     c* tipo servizio  "E"
259000100402     c                   if        tastsp='E'
259100100402     C                   ADD       1             eWTSPEX                         FEDEX
259200100402     C                   ADD       TASNCL        eWTCLEX
259300100402     C                   ADD       w_peso        eWTKGEX
259400100402     C                   ADD       TASVLF        eWTMCEX
259500100402     c                   endif
259600100402     c* tipo servizio  "H"
259700100402     c                   if        tastsp='H'
259800100402     C                   ADD       1             hWTSPEX                         FEDEX
259900100402     C                   ADD       TASNCL        hWTCLEX
260000100402     C                   ADD       w_peso        hWTKGEX
260100100402     C                   ADD       TASVLF        hWTMCEX
260200100402     c                   endif
260300100402     c* extra budget
260400100402     c                   if        *in35
260500100402     C                   ADD       1             WTSPEX_e                       FEDEX
260600100402     C                   ADD       TASNCL        WTCLEX_e
260700100402     C                   ADD       w_peso        WTKGEX_e
260800100402     C                   ADD       TASVLF        WTMCEX_e
260900100402     c* tipo servizio  "E"
261000100402     c                   if        tastsp='E'
261100100402     C                   ADD       1             eWTSPEX_e                       FEDEX
261200100402     C                   ADD       TASNCL        eWTCLEX_e
261300100402     C                   ADD       w_peso        eWTKGEX_e
261400100402     C                   ADD       TASVLF        eWTMCEX_e
261500100402     c                   endif
261600100402     c* tipo servizio  "H"
261700100402     c                   if        tastsp='H'
261800100402     C                   ADD       1             hWTSPEX_e                       FEDEX
261900100402     C                   ADD       TASNCL        hWTCLEX_e
262000100402     C                   ADD       w_peso        hWTKGEX_e
262100100402     C                   ADD       TASVLF        hWTMCEX_e
262200100402     c                   endif
262300100402     c                   endif
262400100402     c                   endif
262500100402     c* riepilogo
262600100402     C                   ADD       1             WTSPEX_r                       FEDEX
262700100402     C                   ADD       TASNCL        WTCLEX_r
262800100402     C                   ADD       w_peso        WTKGEX_r
262900100402     C                   ADD       TASVLF        WTMCEX_r
263000100402     c* tipo servizio  "E"
263100100402     c                   if        tastsp='E'
263200100402     C                   ADD       1             eWTSPEX_r                       FEDEX
263300100402     C                   ADD       TASNCL        eWTCLEX_r
263400100402     C                   ADD       w_peso        eWTKGEX_r
263500100402     C                   ADD       TASVLF        eWTMCEX_r
263600100402     c                   endif
263700100402     c* tipo servizio  "H"
263800100402     c                   if        tastsp='H'
263900100402     C                   ADD       1             hWTSPEX_r                       FEDEX
264000100402     C                   ADD       TASNCL        hWTCLEX_r
264100100402     C                   ADD       w_peso        hWTKGEX_r
264200100402     C                   ADD       TASVLF        hWTMCEX_r
264300100402     c                   endif
264400100402     c* Extra budget
264500100402     c                   if        *in35
264600100402     C                   ADD       1             WTSPEX_re                       FEDEX
264700100402     C                   ADD       TASNCL        WTCLEX_re
264800100402     C                   ADD       w_peso        WTKGEX_re
264900100402     C                   ADD       TASVLF        WTMCEX_re
265000100402     c* tipo servizio  "E"
265100100402     c                   if        tastsp='E'
265200100402     C                   ADD       1             eWTSPEX_re                       FEDEX
265300100402     C                   ADD       TASNCL        eWTCLEX_re
265400100402     C                   ADD       w_peso        eWTKGEX_re
265500100402     C                   ADD       TASVLF        eWTMCEX_re
265600100402     c                   endif
265700100402     c* tipo servizio  "H"
265800100402     c                   if        tastsp='H'
265900100402     C                   ADD       1             hWTSPEX_re                       FEDEX
266000100402     C                   ADD       TASNCL        hWTCLEX_re
266100100402     C                   ADD       w_peso        hWTKGEX_re
266200100402     C                   ADD       TASVLF        hWTMCEX_re
266300100402     c                   endif
266400100402     c
266500100402     c                   endif
266600100402     c                   ENDSR
266700100402     c****************************************************************************
266800100402     c     speITA_LNA    BEGSR
266900100402     c* giornaliero
267000100402     c                   if        (tasaas*10000+tasmgs)=§al
267100100402     C                   ADD       1             WTSPIA
267200100402     C                   ADD       TASNCL        WTCLIA
267300100402     C                   ADD       W_peso        WTKGIA
267400100402     C                   ADD       TASVLF        WTMCIA
267500100402     c* tipo servizio  "E"
267600100402     c                   if        tastsp='E'
267700100402     C                   ADD       1             eWTSPIA
267800100402     C                   ADD       TASNCL        eWTCLIA
267900100402     C                   ADD       W_peso        eWTKGIA
268000100402     C                   ADD       TASVLF        eWTMCIA
268100100402     c                   endif
268200100402     c* tipo servizio  "H"
268300100402     c                   if        tastsp='H'
268400100402     C                   ADD       1             hWTSPIA
268500100402     C                   ADD       TASNCL        hWTCLIA
268600100402     C                   ADD       W_peso        hWTKGIA
268700100402     C                   ADD       TASVLF        hWTMCIA
268800100402     c                   endif
268900100402     c* extra budget
269000100402     c                   if        *in35
269100100402     C                   ADD       1             WTSPIA_e
269200100402     C                   ADD       TASNCL        WTCLIA_e
269300100402     C                   ADD       w_peso        WTKGIA_e
269400100402     C                   ADD       TASVLF        WTMCIA_e
269500100402     c* tipo servizio  "E"
269600100402     c                   if        tastsp='E'
269700100402     C                   ADD       1             eWTSPIA_e
269800100402     C                   ADD       TASNCL        eWTCLIA_e
269900100402     C                   ADD       w_peso        eWTKGIA_e
270000100402     C                   ADD       TASVLF        eWTMCIA_e
270100100402     c                   endif
270200100402     c* tipo servizio  "H"
270300100402     c                   if        tastsp='H'
270400100402     C                   ADD       1             hWTSPIA_e
270500100402     C                   ADD       TASNCL        hWTCLIA_e
270600100402     C                   ADD       w_peso        hWTKGIA_e
270700100402     C                   ADD       TASVLF        hWTMCIA_e
270800100402     c                   endif
270900100402     c
271000100402     c                   endif
271100100402     c                   endif
271200100402     c* riepilogo
271300100402     C                   ADD       1             WTSPIA_r
271400100402     C                   ADD       TASNCL        WTCLIA_r
271500100402     C                   ADD       w_peso        WTKGIA_r
271600100402     C                   ADD       TASVLF        WTMCIA_r
271700100402     c* tipo servizio  "E"
271800100402     c                   if        tastsp='E'
271900100402     C                   ADD       1             eWTSPIA_r
272000100402     C                   ADD       TASNCL        eWTCLIA_r
272100100402     C                   ADD       w_peso        eWTKGIA_r
272200100402     C                   ADD       TASVLF        eWTMCIA_r
272300100402     c                   endif
272400100402     c* tipo servizio  "H"
272500100402     c                   if        tastsp='H'
272600100402     C                   ADD       1             hWTSPIA_r
272700100402     C                   ADD       TASNCL        hWTCLIA_r
272800100402     C                   ADD       w_peso        hWTKGIA_r
272900100402     C                   ADD       TASVLF        hWTMCIA_r
273000100402     c                   endif
273100100402     c* extra budget
273200100402     c                   if        *in35
273300100402     C                   ADD       1             WTSPIA_re
273400100402     C                   ADD       TASNCL        WTCLIA_re
273500100402     C                   ADD       w_peso        WTKGIA_re
273600100402     C                   ADD       TASVLF        WTMCIA_re
273700100402     c* tipo servizio  "E"
273800100402     c                   if        tastsp='E'
273900100402     C                   ADD       1             eWTSPIA_re
274000100402     C                   ADD       TASNCL        eWTCLIA_re
274100100402     C                   ADD       w_peso        eWTKGIA_re
274200100402     C                   ADD       TASVLF        eWTMCIA_re
274300100402     c                   endif
274400100402     c* tipo servizio  "H"
274500100402     c                   if        tastsp='H'
274600100402     C                   ADD       1             hWTSPIA_re
274700100402     C                   ADD       TASNCL        hWTCLIA_re
274800100402     C                   ADD       w_peso        hWTKGIA_re
274900100402     C                   ADD       TASVLF        hWTMCIA_re
275000100402     c                   endif
275100100402     c                   endif
275200100402     c                   ENDSR
275300100402     c****************************************************************************
275400100402     c     speEXPORT     BEGSR
275500100402     c* giornaliero
275600100402     c                   if        (tasaas*10000+tasmgs)=§al
275700100402     C                   ADD       1             WTSPEA
275800100402     C                   ADD       TASNCL        WTCLEA
275900100402     C                   ADD       w_peso        WTKGEA
276000100402     C                   ADD       TASVLF        WTMCEA
276100100402     c* tipo servizio  "E"
276200100402     c                   if        tastsp='E'
276300100402     C                   ADD       1             eWTSPEA
276400100402     C                   ADD       TASNCL        eWTCLEA
276500100402     C                   ADD       w_peso        eWTKGEA
276600100402     C                   ADD       TASVLF        eWTMCEA
276700100402     c                   endif
276800100402     c* tipo servizio  "H"
276900100402     c                   if        tastsp='H'
277000100402     C                   ADD       1             hWTSPEA
277100100402     C                   ADD       TASNCL        hWTCLEA
277200100402     C                   ADD       w_peso        hWTKGEA
277300100402     C                   ADD       TASVLF        hWTMCEA
277400100402     c                   endif
277500100402     c* extra budget
277600100402     c                   if        *in35
277700100402     C                   ADD       1             WTSPEA_e
277800100402     C                   ADD       TASNCL        WTCLEA_e
277900100402     C                   ADD       w_peso        WTKGEA_e
278000100402     C                   ADD       TASVLF        WTMCEA_e
278100100402     c* tipo servizio  "E"
278200100402     c                   if        tastsp='E'
278300100402     C                   ADD       1             eWTSPEA_e
278400100402     C                   ADD       TASNCL        eWTCLEA_e
278500100402     C                   ADD       w_peso        eWTKGEA_e
278600100402     C                   ADD       TASVLF        eWTMCEA_e
278700100402     c                   endif
278800100402     c* tipo servizio  "H"
278900100402     c                   if        tastsp='H'
279000100402     C                   ADD       1             hWTSPEA_e
279100100402     C                   ADD       TASNCL        hWTCLEA_e
279200100402     C                   ADD       w_peso        hWTKGEA_e
279300100402     C                   ADD       TASVLF        hWTMCEA_e
279400100402     c                   endif
279500100402     c
279600100402     c                   endif
279700100402     c                   endif
279800100402     c* riepilogo
279900100402     C                   ADD       1             WTSPEA_r
280000100402     C                   ADD       TASNCL        WTCLEA_r
280100100402     C                   ADD       w_peso        WTKGEA_r
280200100402     C                   ADD       TASVLF        WTMCEA_r
280300100402     c* tipo servizio  "E"
280400100402     c                   if        tastsp='E'
280500100402     C                   ADD       1             eWTSPEA_r
280600100402     C                   ADD       TASNCL        eWTCLEA_r
280700100402     C                   ADD       w_peso        eWTKGEA_r
280800100402     C                   ADD       TASVLF        eWTMCEA_r
280900100402     c                   endif
281000100402     c* tipo servizio  "H"
281100100402     c                   if        tastsp='H'
281200100402     C                   ADD       1             hWTSPEA_r
281300100402     C                   ADD       TASNCL        hWTCLEA_r
281400100402     C                   ADD       w_peso        hWTKGEA_r
281500100402     C                   ADD       TASVLF        hWTMCEA_r
281600100402     c                   endif
281700100402     c* extra budget
281800100402     c                   if        *in35
281900100402     C                   ADD       1             WTSPEA_re
282000100402     C                   ADD       TASNCL        WTCLEA_re
282100100402     C                   ADD       w_peso        WTKGEA_re
282200100402     C                   ADD       TASVLF        WTMCEA_re
282300100402     c* tipo servizio  "E"
282400100402     c                   if        tastsp='E'
282500100402     C                   ADD       1             eWTSPEA_re
282600100402     C                   ADD       TASNCL        eWTCLEA_re
282700100402     C                   ADD       w_peso        eWTKGEA_re
282800100402     C                   ADD       TASVLF        eWTMCEA_re
282900100402     c                   endif
283000100402     c* tipo servizio  "H"
283100100402     c                   if        tastsp='H'
283200100402     C                   ADD       1             hWTSPEA_re
283300100402     C                   ADD       TASNCL        hWTCLEA_re
283400100402     C                   ADD       w_peso        hWTKGEA_re
283500100402     C                   ADD       TASVLF        hWTMCEA_re
283600100402     c                   endif
283700100402     c
283800100402     c                   endif
283900100402     c                   ENDSR
284000100402     c****************************************************************************
284100970519     OQSYSPRT   E            TESTA          2 02
284200090206     O                                         +  1 'Terminata elaborazione'
284300090206     o                                         +  2 'statistiche spedizioni'
284400090206     O                                         +  2 'DEL'
284500970519     O                       UDATE         Y   +  1
284600970528     O                       §DAL              +  5 '    /  /  '
284700970528     O                                         +  1 '>'
284800970528     O                       §AL               +  1 '    /  /  '
284900090206     O                                          120 'TNSSL3R1'
285000970519     O                       PAGE          Z    132
285100970519     O                                          128 'PAG.'
285200970529     O          E            TESTA          2
285300970529     O                                           14 'TIPI BOLLA OK:'
285400970529     O                       TBO               +  1
285500970529     O          E            ERR            1
285600970529     O                       TASLNP               3
285700970529     O                       TASLNA            +  1
285800970529     O                       TASAAS            +  1
285900970529     O                       TASMGS            +  1
286000970529     O                       TASNRS            +  1
286100970529     O                       TASNSP            +  1
286200970529     O                       TASTBL            +  1
286300110801     O          E            TESTA       1  1
286400110801     O                                            7 'Clienti'
286500110801     o                       Ptipocli         +   1
286600110801     O                                        +   1 ':'
286700110801     O          E            detksc         1
286800110801     o                       skicli             132
286900090915**
287000090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
