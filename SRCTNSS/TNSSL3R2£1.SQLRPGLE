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
002600091007     D ksc_EX          S             10    DIM(100)                             TIPI BOLLA OK
002700091006     d
002800970527     D KPJBA         E DS
002900090204     D  §DAL                 247    254  0
003000090204     D  §AL                  255    262  0
003100090601     D  §opz                 263    263
003200090601     D  §tipo                264    264
003300090204
003400090204     D og143         E DS
003500090204     D og148         E DS
003600090204
003700970529     D DSTB          E DS
003800090204     D DS05          E DS
003900090205     D Dar5trs       E DS
004000091006     D dcxb          E DS
004100090204
004200980826     D DSFIL           DS                  OCCURS(400)
004300970527     D  WTSPIP                 1      8  0
004400970527     D  WTSPIA                 9     16  0
004500970527     D  WTSPEI                17     24  0
004600970527     D  WTSPEE                25     32  0
004700970527     D  WTCLIP                33     40  0
004800970527     D  WTCLIA                41     48  0
004900970527     D  WTCLEI                49     56  0
005000970527     D  WTCLEE                57     64  0
005100980826     D  WTKGIP                65     75  1
005200980826     D  WTKGIA                76     86  1
005300980826     D  WTKGEI                87     97  1
005400980826     D  WTKGEE                98    108  1
005500980826     D  WTMCIP               109    119  3
005600980826     D  WTMCIA               120    130  3
005700980826     D  WTMCEI               131    140  3
005800980826     D  WTMCEE               141    150  3
005900000223     D  WTSPEF               151    158  0
006000000223     D  WTCLEF               159    166  0
006100000223     D  WTKGEF               167    177  1
006200000223     D  WTMCEF               178    188  3
006300000607     D  WTSPEA               189    196  0
006400000607     D  WTCLEA               197    204  0
006500000607     D  WTKGEA               205    215  1
006600000607     D  WTMCEA               216    226  3
006700000607     D  WTSPED               227    234  0
006800000607     D  WTCLED               235    242  0
006900000607     D  WTKGED               243    253  1
007000000630     D  WTMCED               254    264  3
007100000630     D  WTSPPA               265    272  0
007200000630     D  WTCLPA               273    280  0
007300000630     D  WTKGPA               281    291  1
007400000630     D  WTMCPA               292    302  3
007500020321     D  WTSPEX               303    310  0
007600020321     D  WTCLEX               311    318  0
007700020321     D  WTKGEX               319    329  1
007800020321     D  WTMCEX               330    340  3
007900090206     D  WTSPIP_R                           like(WTSPIP)
008000090206     D  WTSPIA_r                           like(wtspia)
008100090206     D  WTSPEI_r                           like(wtspei)
008200090206     D  WTSPEE_r                           like(wtspee)
008300090206     D  WTCLIP_r                           like(wtclip)
008400090206     D  WTCLIA_r                           like(wtclia)
008500090206     D  WTCLEI_r                           like(wtclei)
008600090206     D  WTCLEE_r                           like(wtclee)
008700090206     D  WTKGIP_r                           like(wtkgip)
008800090206     D  WTKGIA_r                           like(wtkgia)
008900090206     D  WTKGEI_r                           like(wtkgei)
009000090206     D  WTKGEE_r                           like(wtkgee)
009100090206     D  WTMCIP_r                           like(wtmcip)
009200090206     D  WTMCIA_r                           like(wtmcia)
009300090206     D  WTMCEI_r                           like(wtmcei)
009400090206     D  WTMCEE_r                           like(wtmcee)
009500090206     D  WTSPEF_r                           like(wtspef)
009600090206     D  WTCLEF_r                           like(wtclef)
009700090206     D  WTKGEF_r                           like(wtkgef)
009800090206     D  WTMCEF_r                           like(wtmcef)
009900090206     D  WTSPEA_r                           like(wtspea)
010000090206     D  WTCLEA_r                           like(wtclea)
010100090206     D  WTKGEA_r                           like(wtkgea)
010200090206     D  WTMCEA_r                           like(wtmcea)
010300090206     D  WTSPED_r                           like(wtsped)
010400090206     D  WTCLED_r                           like(wtcled)
010500090206     D  WTKGED_r                           like(wtkged)
010600090206     D  WTMCED_r                           like(wtmced)
010700090206     D  WTSPPA_r                           like(wtsppa)
010800090206     D  WTCLPA_r                           like(wtclpa)
010900090206     D  WTKGPA_r                           like(wtkgpa)
011000090206     D  WTMCPA_r                           like(wtmcpa)
011100090206     D  WTSPEX_r                           like(wtspex)
011200090206     D  WTCLEX_r                           like(wtclex)
011300090206     D  WTKGEX_r                           like(wtkgex)
011400090206     D  WTMCEX_r                           like(wtmcex)
011500091006     d* Giornaliero extra budget
011600091006     D  WTSPIP_E                           like(WTSPIP)
011700091006     D  WTSPIA_e                           like(wtspia)
011800091006     D  WTSPEI_e                           like(wtspei)
011900091006     D  WTSPEE_e                           like(wtspee)
012000091006     D  WTCLIP_e                           like(wtclip)
012100091006     D  WTCLIA_e                           like(wtclia)
012200091006     D  WTCLEI_e                           like(wtclei)
012300091006     D  WTCLEE_e                           like(wtclee)
012400091006     D  WTKGIP_e                           like(wtkgip)
012500091006     D  WTKGIA_e                           like(wtkgia)
012600091006     D  WTKGEI_e                           like(wtkgei)
012700091006     D  WTKGEE_e                           like(wtkgee)
012800091006     D  WTMCIP_e                           like(wtmcip)
012900091006     D  WTMCIA_e                           like(wtmcia)
013000091006     D  WTMCEI_e                           like(wtmcei)
013100091006     D  WTMCEE_e                           like(wtmcee)
013200091006     D  WTSPEF_e                           like(wtspef)
013300091006     D  WTCLEF_e                           like(wtclef)
013400091006     D  WTKGEF_e                           like(wtkgef)
013500091006     D  WTMCEF_e                           like(wtmcef)
013600091006     D  WTSPEA_e                           like(wtspea)
013700091006     D  WTCLEA_e                           like(wtclea)
013800091006     D  WTKGEA_e                           like(wtkgea)
013900091006     D  WTMCEA_e                           like(wtmcea)
014000091006     D  WTSPED_e                           like(wtsped)
014100091006     D  WTCLED_e                           like(wtcled)
014200091006     D  WTKGED_e                           like(wtkged)
014300091006     D  WTMCED_e                           like(wtmced)
014400091006     D  WTSPPA_e                           like(wtsppa)
014500091006     D  WTCLPA_e                           like(wtclpa)
014600091006     D  WTKGPA_e                           like(wtkgpa)
014700091006     D  WTMCPA_e                           like(wtmcpa)
014800091006     D  WTSPEX_e                           like(wtspex)
014900091006     D  WTCLEX_e                           like(wtclex)
015000091006     D  WTKGEX_e                           like(wtkgex)
015100091006     D  WTMCEX_e                           like(wtmcex)
015200091006     d* Riepilogativo extra budget
015300091006     D  WTSPIP_re                          like(WTSPIP)
015400091006     D  WTSPIA_re                          like(wtspia)
015500091006     D  WTSPEI_re                          like(wtspei)
015600091006     D  WTSPEE_re                          like(wtspee)
015700091006     D  WTCLIP_re                          like(wtclip)
015800091006     D  WTCLIA_re                          like(wtclia)
015900091006     D  WTCLEI_re                          like(wtclei)
016000091006     D  WTCLEE_re                          like(wtclee)
016100091006     D  WTKGIP_re                          like(wtkgip)
016200091006     D  WTKGIA_re                          like(wtkgia)
016300091006     D  WTKGEI_re                          like(wtkgei)
016400091006     D  WTKGEE_re                          like(wtkgee)
016500091006     D  WTMCIP_re                          like(wtmcip)
016600091006     D  WTMCIA_re                          like(wtmcia)
016700091006     D  WTMCEI_re                          like(wtmcei)
016800091006     D  WTMCEE_re                          like(wtmcee)
016900091006     D  WTSPEF_re                          like(wtspef)
017000091006     D  WTCLEF_re                          like(wtclef)
017100091006     D  WTKGEF_re                          like(wtkgef)
017200091006     D  WTMCEF_re                          like(wtmcef)
017300091006     D  WTSPEA_re                          like(wtspea)
017400091006     D  WTCLEA_re                          like(wtclea)
017500091006     D  WTKGEA_re                          like(wtkgea)
017600091006     D  WTMCEA_re                          like(wtmcea)
017700091006     D  WTSPED_re                          like(wtsped)
017800091006     D  WTCLED_re                          like(wtcled)
017900091006     D  WTKGED_re                          like(wtkged)
018000091006     D  WTMCED_re                          like(wtmced)
018100091006     D  WTSPPA_re                          like(wtsppa)
018200091006     D  WTCLPA_re                          like(wtclpa)
018300091006     D  WTKGPA_re                          like(wtkgpa)
018400091006     D  WTMCPA_re                          like(wtmcpa)
018500091006     D  WTSPEX_re                          like(wtspex)
018600091006     D  WTCLEX_re                          like(wtclex)
018700091006     D  WTKGEX_re                          like(wtkgex)
018800091006     D  WTMCEX_re                          like(wtmcex)
018900100402     d
019000100402     D DSFIL_e         DS                  OCCURS(400)
019100100402     D  eWTSPIP                1      8  0
019200100402     D  eWTSPIA                9     16  0
019300100402     D  eWTSPEI               17     24  0
019400100402     D  eWTSPEE               25     32  0
019500100402     D  eWTCLIP               33     40  0
019600100402     D  eWTCLIA               41     48  0
019700100402     D  eWTCLEI               49     56  0
019800100402     D  eWTCLEE               57     64  0
019900100402     D  eWTKGIP               65     75  1
020000100402     D  eWTKGIA               76     86  1
020100100402     D  eWTKGEI               87     97  1
020200100402     D  eWTKGEE               98    108  1
020300100402     D  eWTMCIP              109    119  3
020400100402     D  eWTMCIA              120    130  3
020500100402     D  eWTMCEI              131    140  3
020600100402     D  eWTMCEE              141    150  3
020700100402     D  eWTSPEF              151    158  0
020800100402     D  eWTCLEF              159    166  0
020900100402     D  eWTKGEF              167    177  1
021000100402     D  eWTMCEF              178    188  3
021100100402     D  eWTSPEA              189    196  0
021200100402     D  eWTCLEA              197    204  0
021300100402     D  eWTKGEA              205    215  1
021400100402     D  eWTMCEA              216    226  3
021500100402     D  eWTSPED              227    234  0
021600100402     D  eWTCLED              235    242  0
021700100402     D  eWTKGED              243    253  1
021800100402     D  eWTMCED              254    264  3
021900100402     D  eWTSPPA              265    272  0
022000100402     D  eWTCLPA              273    280  0
022100100402     D  eWTKGPA              281    291  1
022200100402     D  eWTMCPA              292    302  3
022300100402     D  eWTSPEX              303    310  0
022400100402     D  eWTCLEX              311    318  0
022500100402     D  eWTKGEX              319    329  1
022600100402     D  eWTMCEX              330    340  3
022700100402     D  eWTSPIP_R                          like(WTSPIP)
022800100402     D  eWTSPIA_r                          like(wtspia)
022900100402     D  eWTSPEI_r                          like(wtspei)
023000100402     D  eWTSPEE_r                          like(wtspee)
023100100402     D  eWTCLIP_r                          like(wtclip)
023200100402     D  eWTCLIA_r                          like(wtclia)
023300100402     D  eWTCLEI_r                          like(wtclei)
023400100402     D  eWTCLEE_r                          like(wtclee)
023500100402     D  eWTKGIP_r                          like(wtkgip)
023600100402     D  eWTKGIA_r                          like(wtkgia)
023700100402     D  eWTKGEI_r                          like(wtkgei)
023800100402     D  eWTKGEE_r                          like(wtkgee)
023900100402     D  eWTMCIP_r                          like(wtmcip)
024000100402     D  eWTMCIA_r                          like(wtmcia)
024100100402     D  eWTMCEI_r                          like(wtmcei)
024200100402     D  eWTMCEE_r                          like(wtmcee)
024300100402     D  eWTSPEF_r                          like(wtspef)
024400100402     D  eWTCLEF_r                          like(wtclef)
024500100402     D  eWTKGEF_r                          like(wtkgef)
024600100402     D  eWTMCEF_r                          like(wtmcef)
024700100402     D  eWTSPEA_r                          like(wtspea)
024800100402     D  eWTCLEA_r                          like(wtclea)
024900100402     D  eWTKGEA_r                          like(wtkgea)
025000100402     D  eWTMCEA_r                          like(wtmcea)
025100100402     D  eWTSPED_r                          like(wtsped)
025200100402     D  eWTCLED_r                          like(wtcled)
025300100402     D  eWTKGED_r                          like(wtkged)
025400100402     D  eWTMCED_r                          like(wtmced)
025500100402     D  eWTSPPA_r                          like(wtsppa)
025600100402     D  eWTCLPA_r                          like(wtclpa)
025700100402     D  eWTKGPA_r                          like(wtkgpa)
025800100402     D  eWTMCPA_r                          like(wtmcpa)
025900100402     D  eWTSPEX_r                          like(wtspex)
026000100402     D  eWTCLEX_r                          like(wtclex)
026100100402     D  eWTKGEX_r                          like(wtkgex)
026200100402     D  eWTMCEX_r                          like(wtmcex)
026300100402     d* Giornaliero extra budget
026400100402     D  eWTSPIP_E                          like(WTSPIP)
026500100402     D  eWTSPIA_e                          like(wtspia)
026600100402     D  eWTSPEI_e                          like(wtspei)
026700100402     D  eWTSPEE_e                          like(wtspee)
026800100402     D  eWTCLIP_e                          like(wtclip)
026900100402     D  eWTCLIA_e                          like(wtclia)
027000100402     D  eWTCLEI_e                          like(wtclei)
027100100402     D  eWTCLEE_e                          like(wtclee)
027200100402     D  eWTKGIP_e                          like(wtkgip)
027300100402     D  eWTKGIA_e                          like(wtkgia)
027400100402     D  eWTKGEI_e                          like(wtkgei)
027500100402     D  eWTKGEE_e                          like(wtkgee)
027600100402     D  eWTMCIP_e                          like(wtmcip)
027700100402     D  eWTMCIA_e                          like(wtmcia)
027800100402     D  eWTMCEI_e                          like(wtmcei)
027900100402     D  eWTMCEE_e                          like(wtmcee)
028000100402     D  eWTSPEF_e                          like(wtspef)
028100100402     D  eWTCLEF_e                          like(wtclef)
028200100402     D  eWTKGEF_e                          like(wtkgef)
028300100402     D  eWTMCEF_e                          like(wtmcef)
028400100402     D  eWTSPEA_e                          like(wtspea)
028500100402     D  eWTCLEA_e                          like(wtclea)
028600100402     D  eWTKGEA_e                          like(wtkgea)
028700100402     D  eWTMCEA_e                          like(wtmcea)
028800100402     D  eWTSPED_e                          like(wtsped)
028900100402     D  eWTCLED_e                          like(wtcled)
029000100402     D  eWTKGED_e                          like(wtkged)
029100100402     D  eWTMCED_e                          like(wtmced)
029200100402     D  eWTSPPA_e                          like(wtsppa)
029300100402     D  eWTCLPA_e                          like(wtclpa)
029400100402     D  eWTKGPA_e                          like(wtkgpa)
029500100402     D  eWTMCPA_e                          like(wtmcpa)
029600100402     D  eWTSPEX_e                          like(wtspex)
029700100402     D  eWTCLEX_e                          like(wtclex)
029800100402     D  eWTKGEX_e                          like(wtkgex)
029900100402     D  eWTMCEX_e                          like(wtmcex)
030000100402     d* Riepilogativo extra budget
030100100402     D  eWTSPIP_re                         like(WTSPIP)
030200100402     D  eWTSPIA_re                         like(wtspia)
030300100402     D  eWTSPEI_re                         like(wtspei)
030400100402     D  eWTSPEE_re                         like(wtspee)
030500100402     D  eWTCLIP_re                         like(wtclip)
030600100402     D  eWTCLIA_re                         like(wtclia)
030700100402     D  eWTCLEI_re                         like(wtclei)
030800100402     D  eWTCLEE_re                         like(wtclee)
030900100402     D  eWTKGIP_re                         like(wtkgip)
031000100402     D  eWTKGIA_re                         like(wtkgia)
031100100402     D  eWTKGEI_re                         like(wtkgei)
031200100402     D  eWTKGEE_re                         like(wtkgee)
031300100402     D  eWTMCIP_re                         like(wtmcip)
031400100402     D  eWTMCIA_re                         like(wtmcia)
031500100402     D  eWTMCEI_re                         like(wtmcei)
031600100402     D  eWTMCEE_re                         like(wtmcee)
031700100402     D  eWTSPEF_re                         like(wtspef)
031800100402     D  eWTCLEF_re                         like(wtclef)
031900100402     D  eWTKGEF_re                         like(wtkgef)
032000100402     D  eWTMCEF_re                         like(wtmcef)
032100100402     D  eWTSPEA_re                         like(wtspea)
032200100402     D  eWTCLEA_re                         like(wtclea)
032300100402     D  eWTKGEA_re                         like(wtkgea)
032400100402     D  eWTMCEA_re                         like(wtmcea)
032500100402     D  eWTSPED_re                         like(wtsped)
032600100402     D  eWTCLED_re                         like(wtcled)
032700100402     D  eWTKGED_re                         like(wtkged)
032800100402     D  eWTMCED_re                         like(wtmced)
032900100402     D  eWTSPPA_re                         like(wtsppa)
033000100402     D  eWTCLPA_re                         like(wtclpa)
033100100402     D  eWTKGPA_re                         like(wtkgpa)
033200100402     D  eWTMCPA_re                         like(wtmcpa)
033300100402     D  eWTSPEX_re                         like(wtspex)
033400100402     D  eWTCLEX_re                         like(wtclex)
033500100402     D  eWTKGEX_re                         like(wtkgex)
033600100402     D  eWTMCEX_re                         like(wtmcex)
033700100402     d
033800100402     D DSFIL_h         DS                  OCCURS(400)
033900100402     D  hWTSPIP                1      8  0
034000100402     D  hWTSPIA                9     16  0
034100100402     D  hWTSPEI               17     24  0
034200100402     D  hWTSPEE               25     32  0
034300100402     D  hWTCLIP               33     40  0
034400100402     D  hWTCLIA               41     48  0
034500100402     D  hWTCLEI               49     56  0
034600100402     D  hWTCLEE               57     64  0
034700100402     D  hWTKGIP               65     75  1
034800100402     D  hWTKGIA               76     86  1
034900100402     D  hWTKGEI               87     97  1
035000100402     D  hWTKGEE               98    108  1
035100100402     D  hWTMCIP              109    119  3
035200100402     D  hWTMCIA              120    130  3
035300100402     D  hWTMCEI              131    140  3
035400100402     D  hWTMCEE              141    150  3
035500100402     D  hWTSPEF              151    158  0
035600100402     D  hWTCLEF              159    166  0
035700100402     D  hWTKGEF              167    177  1
035800100402     D  hWTMCEF              178    188  3
035900100402     D  hWTSPEA              189    196  0
036000100402     D  hWTCLEA              197    204  0
036100100402     D  hWTKGEA              205    215  1
036200100402     D  hWTMCEA              216    226  3
036300100402     D  hWTSPED              227    234  0
036400100402     D  hWTCLED              235    242  0
036500100402     D  hWTKGED              243    253  1
036600100402     D  hWTMCED              254    264  3
036700100402     D  hWTSPPA              265    272  0
036800100402     D  hWTCLPA              273    280  0
036900100402     D  hWTKGPA              281    291  1
037000100402     D  hWTMCPA              292    302  3
037100100402     D  hWTSPEX              303    310  0
037200100402     D  hWTCLEX              311    318  0
037300100402     D  hWTKGEX              319    329  1
037400100402     D  hWTMCEX              330    340  3
037500100402     D  hWTSPIP_R                          like(WTSPIP)
037600100402     D  hWTSPIA_r                          like(wtspia)
037700100402     D  hWTSPEI_r                          like(wtspei)
037800100402     D  hWTSPEE_r                          like(wtspee)
037900100402     D  hWTCLIP_r                          like(wtclip)
038000100402     D  hWTCLIA_r                          like(wtclia)
038100100402     D  hWTCLEI_r                          like(wtclei)
038200100402     D  hWTCLEE_r                          like(wtclee)
038300100402     D  hWTKGIP_r                          like(wtkgip)
038400100402     D  hWTKGIA_r                          like(wtkgia)
038500100402     D  hWTKGEI_r                          like(wtkgei)
038600100402     D  hWTKGEE_r                          like(wtkgee)
038700100402     D  hWTMCIP_r                          like(wtmcip)
038800100402     D  hWTMCIA_r                          like(wtmcia)
038900100402     D  hWTMCEI_r                          like(wtmcei)
039000100402     D  hWTMCEE_r                          like(wtmcee)
039100100402     D  hWTSPEF_r                          like(wtspef)
039200100402     D  hWTCLEF_r                          like(wtclef)
039300100402     D  hWTKGEF_r                          like(wtkgef)
039400100402     D  hWTMCEF_r                          like(wtmcef)
039500100402     D  hWTSPEA_r                          like(wtspea)
039600100402     D  hWTCLEA_r                          like(wtclea)
039700100402     D  hWTKGEA_r                          like(wtkgea)
039800100402     D  hWTMCEA_r                          like(wtmcea)
039900100402     D  hWTSPED_r                          like(wtsped)
040000100402     D  hWTCLED_r                          like(wtcled)
040100100402     D  hWTKGED_r                          like(wtkged)
040200100402     D  hWTMCED_r                          like(wtmced)
040300100402     D  hWTSPPA_r                          like(wtsppa)
040400100402     D  hWTCLPA_r                          like(wtclpa)
040500100402     D  hWTKGPA_r                          like(wtkgpa)
040600100402     D  hWTMCPA_r                          like(wtmcpa)
040700100402     D  hWTSPEX_r                          like(wtspex)
040800100402     D  hWTCLEX_r                          like(wtclex)
040900100402     D  hWTKGEX_r                          like(wtkgex)
041000100402     D  hWTMCEX_r                          like(wtmcex)
041100100402     d* Giornaliero extra budget
041200100402     D  hWTSPIP_E                          like(WTSPIP)
041300100402     D  hWTSPIA_e                          like(wtspia)
041400100402     D  hWTSPEI_e                          like(wtspei)
041500100402     D  hWTSPEE_e                          like(wtspee)
041600100402     D  hWTCLIP_e                          like(wtclip)
041700100402     D  hWTCLIA_e                          like(wtclia)
041800100402     D  hWTCLEI_e                          like(wtclei)
041900100402     D  hWTCLEE_e                          like(wtclee)
042000100402     D  hWTKGIP_e                          like(wtkgip)
042100100402     D  hWTKGIA_e                          like(wtkgia)
042200100402     D  hWTKGEI_e                          like(wtkgei)
042300100402     D  hWTKGEE_e                          like(wtkgee)
042400100402     D  hWTMCIP_e                          like(wtmcip)
042500100402     D  hWTMCIA_e                          like(wtmcia)
042600100402     D  hWTMCEI_e                          like(wtmcei)
042700100402     D  hWTMCEE_e                          like(wtmcee)
042800100402     D  hWTSPEF_e                          like(wtspef)
042900100402     D  hWTCLEF_e                          like(wtclef)
043000100402     D  hWTKGEF_e                          like(wtkgef)
043100100402     D  hWTMCEF_e                          like(wtmcef)
043200100402     D  hWTSPEA_e                          like(wtspea)
043300100402     D  hWTCLEA_e                          like(wtclea)
043400100402     D  hWTKGEA_e                          like(wtkgea)
043500100402     D  hWTMCEA_e                          like(wtmcea)
043600100402     D  hWTSPED_e                          like(wtsped)
043700100402     D  hWTCLED_e                          like(wtcled)
043800100402     D  hWTKGED_e                          like(wtkged)
043900100402     D  hWTMCED_e                          like(wtmced)
044000100402     D  hWTSPPA_e                          like(wtsppa)
044100100402     D  hWTCLPA_e                          like(wtclpa)
044200100402     D  hWTKGPA_e                          like(wtkgpa)
044300100402     D  hWTMCPA_e                          like(wtmcpa)
044400100402     D  hWTSPEX_e                          like(wtspex)
044500100402     D  hWTCLEX_e                          like(wtclex)
044600100402     D  hWTKGEX_e                          like(wtkgex)
044700100402     D  hWTMCEX_e                          like(wtmcex)
044800100402     d* Riepilogativo extra budget
044900100402     D  hWTSPIP_re                         like(WTSPIP)
045000100402     D  hWTSPIA_re                         like(wtspia)
045100100402     D  hWTSPEI_re                         like(wtspei)
045200100402     D  hWTSPEE_re                         like(wtspee)
045300100402     D  hWTCLIP_re                         like(wtclip)
045400100402     D  hWTCLIA_re                         like(wtclia)
045500100402     D  hWTCLEI_re                         like(wtclei)
045600100402     D  hWTCLEE_re                         like(wtclee)
045700100402     D  hWTKGIP_re                         like(wtkgip)
045800100402     D  hWTKGIA_re                         like(wtkgia)
045900100402     D  hWTKGEI_re                         like(wtkgei)
046000100402     D  hWTKGEE_re                         like(wtkgee)
046100100402     D  hWTMCIP_re                         like(wtmcip)
046200100402     D  hWTMCIA_re                         like(wtmcia)
046300100402     D  hWTMCEI_re                         like(wtmcei)
046400100402     D  hWTMCEE_re                         like(wtmcee)
046500100402     D  hWTSPEF_re                         like(wtspef)
046600100402     D  hWTCLEF_re                         like(wtclef)
046700100402     D  hWTKGEF_re                         like(wtkgef)
046800100402     D  hWTMCEF_re                         like(wtmcef)
046900100402     D  hWTSPEA_re                         like(wtspea)
047000100402     D  hWTCLEA_re                         like(wtclea)
047100100402     D  hWTKGEA_re                         like(wtkgea)
047200100402     D  hWTMCEA_re                         like(wtmcea)
047300100402     D  hWTSPED_re                         like(wtsped)
047400100402     D  hWTCLED_re                         like(wtcled)
047500100402     D  hWTKGED_re                         like(wtkged)
047600100402     D  hWTMCED_re                         like(wtmced)
047700100402     D  hWTSPPA_re                         like(wtsppa)
047800100402     D  hWTCLPA_re                         like(wtclpa)
047900100402     D  hWTKGPA_re                         like(wtkgpa)
048000100402     D  hWTMCPA_re                         like(wtmcpa)
048100100402     D  hWTSPEX_re                         like(wtspex)
048200100402     D  hWTCLEX_re                         like(wtclex)
048300100402     D  hWTKGEX_re                         like(wtkgex)
048400100402     D  hWTMCEX_re                         like(wtmcex)
048500100402     d
048600090206     d* dati giornaliero
048700091006     D DSFIL_smp       DS                  OCCURS(400)
048800090206     D  Wmptsp_g                           like(wmptsp)
048900090206     D  wmptcp_g                           like(wmptcp)
049000090206     D  wmptkp_g                     11  1
049100090206     D  wmptmp_g                     10  3
049200090710
049300090206     D  Wmptsa_g                           like(wmptsa)
049400090206     D  wmptca_g                           like(wmptca)
049500090206     D  wmptka_g                     11  1
049600090206     D  wmptma_g                     10  3
049700090710
049800090206     D  Wmptspt_g                          like(wmptspt)
049900090206     D  wmptcpt_g                          like(wmptcpt)
050000090206     D  wmptkpt_g                    11  1
050100090206     D  wmptmpt_g                    10  3
050200090710
050300090710     D  Wmptsptc_g                         like(wmptspt)
050400090710     D  wmptcptc_g                         like(wmptcpt)
050500090710     D  wmptkptc_g                   11  1
050600090710     D  wmptmptc_g                   10  3
050700090710
050800090206     D  Wmptsat_g                          like(wmptsat)
050900090206     D  wmptcat_g                          like(wmptcat)
051000090206     D  wmptkat_g                    11  1
051100090206     D  wmptmat_g                    10  3
051200090710     d
051300090710     D  Wmptsatc_g                         like(wmptsat)
051400090710     D  wmptcatc_g                         like(wmptcat)
051500090710     D  wmptkatc_g                   11  1
051600090710     D  wmptmatc_g                   10  3
051700090710
051800090206     D  Wmptst_g                           like(wmptst)
051900090206     D  wmptct_g                           like(wmptct)
052000090206     D  wmptkt_g                     11  1
052100090206     D  wmptmt_g                     10  3
052200090710
052300090206     d* dati riepilogo
052400090206     D  Wmptsp_r                           like(wmptsp)
052500090206     D  wmptcp_r                           like(wmptcp)
052600090206     D  wmptkp_r                     11  1
052700090206     D  wmptmp_r                     10  3
052800090710
052900090206     D  Wmptsa_r                           like(wmptsa)
053000090206     D  wmptca_r                           like(wmptca)
053100090206     D  wmptka_r                     11  1
053200090206     D  wmptma_r                     10  3
053300090710
053400090206     D  Wmptspt_r                          like(wmptspt)
053500090206     D  wmptcpt_r                          like(wmptcpt)
053600090206     D  wmptkpt_r                    11  1
053700090206     D  wmptmpt_r                    10  3
053800090710
053900090710     D  Wmptsptc_r                         like(wmptspt)
054000090710     D  wmptcptc_r                         like(wmptcpt)
054100090710     D  wmptkptc_r                   11  1
054200090710     D  wmptmptc_r                   10  3
054300090710
054400090206     D  Wmptsat_r                          like(wmptsat)
054500090206     D  wmptcat_r                          like(wmptcat)
054600090206     D  wmptkat_r                    11  1
054700090206     D  wmptmat_r                    10  3
054800090710
054900090710     D  Wmptsatc_r                         like(wmptsat)
055000090710     D  wmptcatc_r                         like(wmptcat)
055100090710     D  wmptkatc_r                   11  1
055200090710     D  wmptmatc_r                   10  3
055300090710
055400090206     D  Wmptst_r                           like(wmptst)
055500090206     D  wmptct_r                           like(wmptct)
055600090206     D  wmptkt_r                     11  1
055700090206     D  wmptmt_r                     10  3
055800090204
055900091006     D EE              S              3  0
056000091006     D Wdatael         S              8  0
056100091007     D w010a           S             10
056200090204     d dataiso         s               d   datfmt(*iso)
056300090204     d waas            s                   like(tasaas)
056400090204     d wmgs            s                   Like(tasmgs)
056500090224     d w_peso          s                   Like(taspkf)
056600090204     d k_fil           s                   Like(wmpfil)
056700090204     d k_tip           s                   Like(wmptip)
056800090204     d k_dat           s                   Like(wmpdal)
056900090205     d k_trd           s                   Like(ar5trd) inz('TRS')
057000091006     d savccm          s                   Like(tasccm)
057100091006     d savctr          s                   Like(tasctr)
057200090601     d
057300090601     d wrkgetlista     s           4096    varying
057400970527     C* RICEVIMENTO PARAMETRI
057500970527     C     *ENTRY        PLIST
057600970527     C                   PARM                    KPJBA
057700970527     C                   MOVEL     §DAL          WAAS
057800970527     C                   MOVE      §DAL          WMGS
057900090601     C                   MOVE      §tipo         Wtipo             1
058000970528     C*
058100970529     C                   EXSR      CARINZ
058200951220     C*
058300990811     C* LETTURA TITAS
058400990811     C     KTAS          SETLL     TITAS38C
058500970527     C                   DO        *HIVAL
058600090204     C                   READ      TITAS38C
058700090204     c* Termina il ciclo se fine file o per data spedizione > data al
058800090204     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
058900090204     c                   leave
059000090204     c                   endif
059100090204     c* Scarto in base al tipo bolla
059200090204     c                   if        %lookup(tastbl:tbo)=0
059300090204     c                   iter
059400090204     c                   endif
059500970527     C*
059600090601     c
059700090601     c* EDPES : 01/06/09
059800090601     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
059900090901     c***                z-add     taspkf        w_peso
060000090901     c* EDPES:  01/09/09
060100090901     c**  riutilizzo il peso VDL
060200090901     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
060300090901     c* 1. VDL totale
060400090901     c* 2. VDL parziale se > di TASPKF
060500090901     c* 3. TASPKF
060600090901     c                   clear                   w_peso
060700090901     c                   if        tasncp=tasncl or (tasncp>0 and taspkc>taspkf)
060800090901     c                   z-add     taspkc        w_peso
060900090901     c                   else
061000090901     c                   z-add     taspkf        w_peso
061100090901     c                   endif
061200090601     c
061300090204     c                   exsr      sr_wfsmp
061400090204     c                   exsr      sr_wslss
061500090204     C                   ENDDO
061600090206     c*
061700090206     C* SCARICO DS SU FILE
061800090206     c                   exsr      scrive_file
061900090206     c                   except    testa
062000970528     C*
062100970528     C                   SETON                                        LR
062200970519     C*
062300090204     C**********************************************************
062400090204     C** Elaborazione per file wfsmp00f
062500090204     C**********************************************************
062600090204     C     sr_wfsmp      BEGSR
062700090204     c* linea di partenza
062800090206     c                   z-add     1             k
062900090206     c     taslnp        lookup    fil(k)                                 58
063000090206     c                   if        *in58
063100090206     c     k             occur     dsfil_smp
063200090205     c* . . . giornaliero
063300090206     c                   if        (tasaas*10000+tasmgs)=§al
063400090206     C                   add       1             Wmptsp_g
063500090206     C                   add       TASNCL        wmptcp_g
063600090224     C                   add       w_peso        Wmptkp_g
063700090206     C                   add       TASVLF        Wmptmp_g
063800090206     c                   endif
063900090204     c* . . . riepilogo
064000090206     C                   add       1             Wmptsp_r
064100090206     C                   add       TASNCL        wmptcp_r
064200090224     C                   add       w_peso        Wmptkp_r
064300090206     C                   add       TASVLF        Wmptmp_r
064400090206     c                   endif
064500090204     c* linea di arrivo
064600090206     c                   z-add     1             k
064700090206     c     taslna        lookup    fil(k)                                 58
064800090206     c                   if        *in58
064900090206     c     k             occur     dsfil_smp
065000090205     c* . . . giornaliero
065100090206     c                   if        (tasaas*10000+tasmgs)=§al
065200090206     C                   add       1             Wmptsa_g
065300090206     C                   add       TASNCL        wmptca_g
065400090224     C                   add       w_peso        Wmptka_g
065500090206     C                   add       TASVLF        Wmptma_g
065600090206     c                   endif
065700090205     c* . . . riepilogo
065800090206     C                   add       1             Wmptsa_r
065900090206     C                   add       TASNCL        wmptca_r
066000090224     C                   add       w_peso        Wmptka_r
066100090206     C                   add       TASVLF        Wmptma_r
066200090206     c                   endif
066300090710     c* terminal di partenza : esclusa la linea/terminal
066400090710     c                   if        tastfp<>taslnp
066500090206     c                   z-add     1             k
066600090206     c     tastfp        lookup    fil(k)                                 58
066700090206     c                   if        *in58
066800090206     c     k             occur     dsfil_smp
066900090205     c* . . . giornaliero
067000090206     c                   if        (tasaas*10000+tasmgs)=§al
067100090206     C                   add       1             Wmptspt_g
067200090206     C                   add       TASNCL        wmptcpt_g
067300090224     C                   add       w_peso        Wmptkpt_g
067400090206     C                   add       TASVLF        Wmptmpt_g
067500090206     c                   endif
067600090205     c* . . . riepilogo
067700090206     C                   add       1             Wmptspt_r
067800090206     C                   add       TASNCL        wmptcpt_r
067900090224     C                   add       w_peso        Wmptkpt_r
068000090206     C                   add       TASVLF        Wmptmpt_r
068100090206     c                   endif
068200090710     c                   endif
068300090710     c* terminal di partenza : tutto compresa la linea stessa
068400090710     c                   z-add     1             k
068500090710     c     tastfp        lookup    fil(k)                                 58
068600090710     c                   if        *in58
068700090710     c     k             occur     dsfil_smp
068800090710     c* . . . giornaliero
068900090710     c                   if        (tasaas*10000+tasmgs)=§al
069000090710     C                   add       1             Wmptsptc_g
069100090710     C                   add       TASNCL        wmptcptc_g
069200090710     C                   add       w_peso        Wmptkptc_g
069300090710     C                   add       TASVLF        Wmptmptc_g
069400090710     c                   endif
069500090710     c* . . . riepilogo
069600090710     C                   add       1             Wmptsptc_r
069700090710     C                   add       TASNCL        wmptcptc_r
069800090710     C                   add       w_peso        Wmptkptc_r
069900090710     C                   add       TASVLF        Wmptmptc_r
070000090710     c                   endif
070100090710     c
070200090710     c* terminal di arrivo : esclusa la linea/terminal
070300090710     c                   if        tastfa<>taslna
070400090206     c                   z-add     1             k
070500090206     c     tastfa        lookup    fil(k)                                 58
070600090206     c                   if        *in58
070700090206     c     k             occur     dsfil_smp
070800090205     c* . . . giornaliero
070900090206     c                   if        (tasaas*10000+tasmgs)=§al
071000090206     C                   add       1             Wmptsat_g
071100090206     C                   add       TASNCL        wmptcat_g
071200090224     C                   add       w_peso        Wmptkat_g
071300090206     C                   add       TASVLF        Wmptmat_g
071400090206     c                   endif
071500090205     c* . . . riepilogo
071600090206     C                   add       1             Wmptsat_r
071700090206     C                   add       TASNCL        wmptcat_r
071800090224     C                   add       w_peso        Wmptkat_r
071900090206     C                   add       TASVLF        Wmptmat_r
072000090205     c                   endif
072100090710     c                   endif
072200090710     c* terminal di arrivo : tutto compreso il terminal stesso
072300090710     c                   z-add     1             k
072400090710     c     tastfa        lookup    fil(k)                                 58
072500090710     c                   if        *in58
072600090710     c     k             occur     dsfil_smp
072700090710     c* . . . giornaliero
072800090710     c                   if        (tasaas*10000+tasmgs)=§al
072900090710     C                   add       1             Wmptsatc_g
073000090710     C                   add       TASNCL        wmptcatc_g
073100090710     C                   add       w_peso        Wmptkatc_g
073200090710     C                   add       TASVLF        Wmptmatc_g
073300090710     c                   endif
073400090710     c* . . . riepilogo
073500090710     C                   add       1             Wmptsatc_r
073600090710     C                   add       TASNCL        wmptcatc_r
073700090710     C                   add       w_peso        Wmptkatc_r
073800090710     C                   add       TASVLF        Wmptmatc_r
073900090710     c                   endif
074000090710     c
074100090204     c* filiali di transito
074200090209     c     kar5          chain     fiar531c
074300090205     c                   if        %found(fiar531c)
074400090205     c                   movel     ar5uni        dar5trs
074500090205     c*
074600090205     c                   eval      k_fil=§AR5FLP
074700090205     c                   exsr      agg_transiti
074800090205      *
074900090205     c                   if        §ar5fl2>0
075000090205     c                   eval      k_fil=§AR5FL2
075100090205     c                   exsr      agg_transiti
075200090205     c                   endif
075300090205      *
075400090205     c                   if        §ar5fl3>0
075500090205     c                   eval      k_fil=§AR5FL3
075600090205     c                   exsr      agg_transiti
075700090205     c                   endif
075800090205      *
075900090205     c                   if        §ar5fl4>0
076000090205     c                   eval      k_fil=§AR5FL4
076100090205     c                   exsr      agg_transiti
076200090205     c                   endif
076300090205      *
076400090205     c                   if        §ar5fl5>0
076500090205     c                   eval      k_fil=§AR5FL5
076600090205     c                   exsr      agg_transiti
076700090205     c                   endif
076800090205     c                   endif
076900090204     c
077000090204     C                   endsr
077100090205     C**********************************************************
077200090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
077300090205     C**********************************************************
077400090205     C     agg_transiti  BEGSR
077500090206     c                   z-add     1             k
077600090206     c     k_fil         lookup    fil(k)                                 58
077700090206     c                   if        *in58
077800090206     c     k             occur     dsfil_smp
077900090205     c* . . . giornaliero
078000090206     c                   if        (tasaas*10000+tasmgs)=§al
078100090206     C                   add       1             Wmptst_g
078200090206     C                   add       TASNCL        wmptct_g
078300090224     C                   add       w_peso        Wmptkt_g
078400090206     C                   add       TASVLF        Wmptmt_g
078500090206     c                   endif
078600090205     c* . . . riepilogo
078700090206     C                   add       1             Wmptst_r
078800090206     C                   add       TASNCL        wmptct_r
078900090224     C                   add       w_peso        Wmptkt_r
079000090206     C                   add       TASVLF        Wmptmt_r
079100090205     c                   endif
079200090205     c                   endsr
079300090204     C**********************************************************
079400090206     C** Riempie campi wfsmp00f
079500090204     C**********************************************************
079600090206     C     rie_campi     BEGSR
079700090204     c* profilo utente e data elaborazione
079800090204     c                   eval      wmpusr=knmus
079900090204     c                   eval      wmpdel=wdatael
080000090206     c*
080100090206     c                   eval      wmpall=§al
080200090204     c* area e descrizione area
080300090204     c                   eval      wmpcar=car(k)
080400090204     C                   MOVEL(P)  '05'          tblcod
080500090204     C                   MOVEL(P)  car(K)        tblkey
080600090204     C     KTAB          CHAIN     TABEL00F                           30
080700090204     C                   IF        %found(tabel00f) and tblflg=*blanks
080800090204     C                   MOVEL     TBLUNI        DS05
080900090204     C                   MOVEL     §05DES        wmpdar
081000090204     C                   endif
081100090204     c* filiale,descrizione e livello filiale
081200090204     c                   eval      wmpdil=dil(K)
081300090206     c                   eval      wmpfil=fil(K)
081400090206     c                   eval      wmplif=lpo(K)
081500090204     c* tipo filiale Italia/Estero
081600090205     c                   eval      wmptif=tif(K)
081700090206     c* spedizioni,colli, kg,volume
081800090206     c                   if        wmptip='G'
081900090206     c                   z-add     wmptsp_G      wmptsp
082000090206     c                   z-add     wmptcp_G      wmptcp
082100090206     c                   z-add(h)  wmptkp_G      wmptkp
082200090206     c                   z-add(h)  wmptmp_G      wmptmp
082300090710
082400090206     c                   z-add     wmptsa_G      wmptsa
082500090206     c                   z-add     wmptca_G      wmptca
082600090206     c                   z-add(h)  wmptka_G      wmptka
082700090206     c                   z-add(h)  wmptma_G      wmptma
082800090710
082900090206     c                   z-add     wmptspt_G     wmptspt
083000090206     c                   z-add     wmptcpt_G     wmptcpt
083100090206     c                   z-add(h)  wmptkpt_G     wmptkpt
083200090206     c                   z-add(h)  wmptmpt_G     wmptmpt
083300090710
083400090710     c                   z-add     wmptsptc_G    wmptsptc
083500090710     c                   z-add     wmptcptc_G    wmptcptc
083600090710     c                   z-add(h)  wmptkptc_G    wmptkptc
083700090710     c                   z-add(h)  wmptmptc_G    wmptmptc
083800090710
083900090206     c                   z-add     wmptsat_G     wmptsat
084000090206     c                   z-add     wmptcat_G     wmptcat
084100090206     c                   z-add(h)  wmptkat_G     wmptkat
084200090206     c                   z-add(h)  wmptmat_G     wmptmat
084300090710
084400090710     c                   z-add     wmptsatc_G    wmptsatc
084500090710     c                   z-add     wmptcatc_G    wmptcatc
084600090710     c                   z-add(h)  wmptkatc_G    wmptkatc
084700090710     c                   z-add(h)  wmptmatc_G    wmptmatc
084800090710
084900090206     c                   z-add     wmptst_G      wmptst
085000090206     c                   z-add     wmptct_G      wmptct
085100090206     c                   z-add(h)  wmptkt_G      wmptkt
085200090206     c                   z-add(h)  wmptmt_G      wmptmt
085300090206     c                   else
085400090206     c                   z-add     wmptsp_r      wmptsp
085500090206     c                   z-add     wmptcp_r      wmptcp
085600090206     c                   z-add(h)  wmptkp_r      wmptkp
085700090206     c                   z-add(h)  wmptmp_r      wmptmp
085800090710
085900090206     c                   z-add     wmptsa_r      wmptsa
086000090206     c                   z-add     wmptca_r      wmptca
086100090206     c                   z-add(h)  wmptka_r      wmptka
086200090206     c                   z-add(h)  wmptma_r      wmptma
086300090710
086400090206     c                   z-add     wmptspt_r     wmptspt
086500090206     c                   z-add     wmptcpt_r     wmptcpt
086600090206     c                   z-add(h)  wmptkpt_r     wmptkpt
086700090206     c                   z-add(h)  wmptmpt_r     wmptmpt
086800090710
086900090710     c                   z-add     wmptsptc_r    wmptsptc
087000090710     c                   z-add     wmptcptc_r    wmptcptc
087100090710     c                   z-add(h)  wmptkptc_r    wmptkptc
087200090710     c                   z-add(h)  wmptmptc_r    wmptmptc
087300090710
087400090206     c                   z-add     wmptsat_r     wmptsat
087500090206     c                   z-add     wmptcat_r     wmptcat
087600090206     c                   z-add(h)  wmptkat_r     wmptkat
087700090206     c                   z-add(h)  wmptmat_r     wmptmat
087800090710
087900090710     c                   z-add     wmptsatc_r    wmptsatc
088000090710     c                   z-add     wmptcatc_r    wmptcatc
088100090710     c                   z-add(h)  wmptkatc_r    wmptkatc
088200090710     c                   z-add(h)  wmptmatc_r    wmptmatc
088300090710
088400090206     c                   z-add     wmptst_r      wmptst
088500090206     c                   z-add     wmptct_r      wmptct
088600090206     c                   z-add(h)  wmptkt_r      wmptkt
088700090206     c                   z-add(h)  wmptmt_r      wmptmt
088800090710
088900090206     c                   endif
089000090204     c                   endsr
089100090204     C**********************************************************
089200090204     C** Elaborazione per file wslss50f
089300090204     C**********************************************************
089400090204     C     sr_wslss      BEGSR
089500090204     C                   SETOFF                                       545556
089600090204     C                   SETOFF                                       48  52
089700090204     C     TASLNP        COMP      300                                54        ESTERO
089800090204     C  N54TASLNP        COMP      190                                    54    + DPD
089900090204     C  N54TASLNP        COMP      191                                    54    + DPD
090000090204     C  N54TASLNP        COMP      192                                    54    + DPD
090100090204     C  N54TASLNP        COMP      193                                    54    + DPD
090200090204     C  N54TASLNP        COMP      194                                    54    + DPD
090300090204     C  N54TASLNP        COMP      195                                    54    + DPD
090400090204     C  N54TASLNP        COMP      196                                    54    + DPD
090500090204     C  N54TASLNP        COMP      197                                    54    + DPD
090600090204     C*
090700100402     C****     TASLNP        COMP      180                                    48     POSTE
090800090204     C*
090900090204     C     TASLNA        COMP      300                                55
091000090204     C  N55TASLNA        COMP      190                                    55    + DPD
091100090204     C  N55TASLNA        COMP      191                                    55    + DPD
091200090204     C  N55TASLNA        COMP      192                                    55    + DPD
091300090204     C  N55TASLNA        COMP      193                                    55    + DPD
091400090204     C  N55TASLNA        COMP      194                                    55    + DPD
091500090204     C  N55TASLNA        COMP      195                                    55    + DPD
091600090204     C  N55TASLNA        COMP      196                                    55    + DPD
091700090204     C  N55TASLNA        COMP      197                                    55    + DPD
091800090204     C*
091900090204     C     TASLNA        COMP      400                                    52     FEDEX
092000090204     C*
092100090204     C     TASLNA        COMP      190                                    56     DPD
092200090204     C  N56TASLNA        COMP      191                                    56     DPD
092300090204     C  N56TASLNA        COMP      192                                    56     DPD
092400090204     C  N56TASLNA        COMP      193                                    56     DPD
092500090204     C  N56TASLNA        COMP      194                                    56     DPD
092600090204     C  N56TASLNA        COMP      195                                    56     DPD
092700090204     C  N56TASLNA        COMP      196                                    56     DPD
092800090204     C  N56TASLNA        COMP      197                                    56     DPD
092900091006     c*
093000091006     c* Verifico se si tratta di cliente extra budget --> 35 on
093100091006     c                   z-add     1             EE
093200091006     c                   if        tasccm<>savccm or tasctr<>savctr
093300091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
093400091007     c     w010a         lookup    ksc_EX(EE)                             35
093500091007     c                   if        not *in35
093600091007     c                   eval      %subst(w010a:8:3)='   '
093700091007     c     w010a         lookup    ksc_EX(EE)                             35
093800091006     c                   endif
093900091006
094000091006     c                   eval      savccm=tasccm
094100091006     c                   eval      savctr=tasctr
094200091006     c                   endif
094300090204     C*
094400090204     C                   Z-ADD     1             K
094500090204     C     TASLNP        LOOKUP    FIL(K)                                 58
094600090204     C  N58              EXCEPT    ERR
094700090204     C     K             OCCUR     DSFIL
094800100402     C     K             OCCUR     DSFIL_e
094900100402     C     K             OCCUR     DSFIL_h
095000090204     C  N54
095100090204     CANN55              DO                                                     ITALIA
095200100402     c* Spedizioni LNP Italia LNA Italia
095300100402     c                   exsr      speITA
095400090204     C                   ENDDO
095500100402     c
095600100402     c* Spedizioni IMPORT --> LNP Italia
095700090204     C   54              DO                                                     IMPORT
095800100402     c                   exsr      speIMPORT
095900090204     C                   ENDDO
096000100402     c
096100100402     c* Spedizioni lna EUROEXPRESS
096200090204     C   55
096300090204     CANN56
096400090204     CANN52              DO                                                     EXPORT
096500100402     c                   exsr      speEXPRESS
096600090204     C                   ENDDO
096700100402     c
096800100402     c* Spedizioni lna DPD
096900090204     C   55
097000090204     CAN 56              DO                                                     EXPORT
097100100402     c                   exsr      speDPD
097200090204     C                   ENDDO
097300100402     c
097400100402     c* Spedizioni lna FEDEX
097500090204     C   55
097600090204     CAN 52              DO                                                     EXPORT
097700100402     c                   exsr      speFEDEX
097800090204     C                   ENDDO
097900100402     c
098000090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
098100090204     C                   Z-ADD     1             K
098200090204     C     TASLNA        LOOKUP    FIL(K)                                 58
098300090204     C  N58              EXCEPT    ERR
098400090204     C     K             OCCUR     DSFIL
098500100402     C     K             OCCUR     DSFIL_e
098600100402     C     K             OCCUR     DSFIL_h
098700100402     c
098800090204     C  N54
098900100402     CANN55              DO
099000100402     c* Spedizioni LNP Italia LNA Italia
099100100402     c                   exsr      speITA_LNA
099200090204     C                   ENDDO
099300100402     c
099400100402     c* spedizioni EXPORT --> lna estera
099500090204     C   55              DO                                                     ESTERO ASS.+FRA.
099600100402     c                   exsr      speEXPORT
099700090204     C                   ENDDO
099800090204     c                   endsr
099900090206     C**********************************************************
100000090206     C** scarica ds multiple su file
100100090206     C**********************************************************
100200090206     C     scrive_file   BEGSR
100300090206     c*
100400090206     C                   Z-ADD     §AL           WSSALL
100500090206     c*
100600090206     C                   DO        400           K
100700090206     C     K             OCCUR     DSFIL_smp
100800090206     C     K             OCCUR     DSFIL
100900100402     C     K             OCCUR     DSFIL_e
101000100402     C     K             OCCUR     DSFIL_h
101100090206     c* WFSMP
101200090206     c* giornaliero
101300090601     c* Lo scrivo solo per tipo "G" o "R"
101400090601     c                   if        Wtipo='G' or Wtipo='R'
101500090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
101600090206     c                              wmptspt_g>0 or wmptsat_g>0 or
101700090206     c                              wmptst_g>0)
101800090206     c                   z-add     §al           wmpdal
101900090206     c                   eval      wmptip='G'
102000090206     c                   exsr      rie_campi
102100090206     c                   write     wfsmp000
102200090206     c                   endif
102300090601     c                   endif
102400090601     c
102500090601     c* riepilogo /precedente
102600090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
102700090601     c                   if        Wtipo='P' or Wtipo='R'
102800090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
102900090206     c                              wmptspt_r>0 or wmptsat_r>0 or
103000090206     c                              wmptst_r>0)
103100090206     c                   z-add     §dal          wmpdal
103200090601     c                   if        Wtipo='R'
103300090206     c                   eval      wmptip='R'
103400090601     c                   else
103500090601     c                   eval      wmptip='P'
103600090601     c                   endif
103700090206     c                   exsr      rie_campi
103800090206     c                   write     wfsmp000
103900090206     c                   endif
104000090601     c                   endif
104100090206     c* WSLSS50F
104200090206     c* giornaliero
104300090601     c                   if        Wtipo='G' or Wtipo='R'
104400090601     c
104500090206     C     WTSPIP        IFGT      *ZERO
104600090206     C                   Z-ADD     §al           WSSDAL
104700090206     c                   movel     'G'           WSSTIE
104800090206     C                   MOVE      'IP'          WSSTIP
104900090206     C                   Z-ADD     FIL(K)        WSSLIN
105000090206     C                   Z-ADD     WTSPIP        WSSTSP
105100090206     C                   Z-ADD     WTCLIP        WSSTCL
105200090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
105300090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
105400100402     c* spedizioni "E"
105500100402     C                   Z-ADD     eWTSPIP       WSSTSPe
105600100402     C                   Z-ADD     eWTCLIP       WSSTCLe
105700100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
105800100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
105900100402     c* spedizioni "H"
106000100402     C                   Z-ADD     hWTSPIP       WSSTSPh
106100100402     C                   Z-ADD     hWTCLIP       WSSTCLh
106200100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
106300100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
106400100402
106500090206     C                   WRITE     WSLSS500
106600090206     C                   ENDIF
106700100402
106800090206     C     WTSPIA        IFGT      *ZERO
106900090206     C                   Z-ADD     §al           WSSDAL
107000090206     c                   movel     'G'           WSSTIE
107100090206     C                   MOVE      'IA'          WSSTIP
107200090206     C                   Z-ADD     FIL(K)        WSSLIN
107300090206     C                   Z-ADD     WTSPIA        WSSTSP
107400090206     C                   Z-ADD     WTCLIA        WSSTCL
107500090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
107600090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
107700100402     c* spedizioni "E"
107800100402     C                   Z-ADD     eWTSPIA       WSSTSPe
107900100402     C                   Z-ADD     eWTCLIA       WSSTCLe
108000100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
108100100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
108200100402     c* spedizioni "H"
108300100402     C                   Z-ADD     hWTSPIA       WSSTSPh
108400100402     C                   Z-ADD     hWTCLIA       WSSTCLh
108500100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
108600100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
108700100402     c
108800090206     C                   WRITE     WSLSS500
108900090206     C                   ENDIF
109000100402     c
109100090206     C     WTSPEI        IFGT      *ZERO
109200090206     C                   Z-ADD     §al           WSSDAL
109300090206     c                   movel     'G'           WSSTIE
109400090206     C                   MOVE      'EI'          WSSTIP
109500090206     C                   Z-ADD     FIL(K)        WSSLIN
109600090206     C                   Z-ADD     WTSPEI        WSSTSP
109700090206     C                   Z-ADD     WTCLEI        WSSTCL
109800090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
109900090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
110000100402     c* spedizioni "E"
110100100402     C                   Z-ADD     eWTSPEI       WSSTSPe
110200100402     C                   Z-ADD     eWTCLEI       WSSTCLe
110300100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
110400100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
110500100402     c* spedizioni "H"
110600100402     C                   Z-ADD     hWTSPEI       WSSTSPh
110700100402     C                   Z-ADD     hWTCLEI       WSSTCLh
110800100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
110900100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
111000100402     c
111100090206     C                   WRITE     WSLSS500
111200090206     C                   ENDIF
111300100402     c
111400090206     C     WTSPEE        IFGT      *ZERO
111500090206     C                   Z-ADD     §al           WSSDAL
111600090206     c                   movel     'G'           WSSTIE
111700090206     C                   MOVE      'EE'          WSSTIP
111800090206     C                   Z-ADD     FIL(K)        WSSLIN
111900090206     C                   Z-ADD     WTSPEE        WSSTSP
112000090206     C                   Z-ADD     WTCLEE        WSSTCL
112100090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
112200090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
112300100402     c* spedizioni "E"
112400100402     C                   Z-ADD     eWTSPEE       WSSTSPe
112500100402     C                   Z-ADD     eWTCLEE       WSSTCLe
112600100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
112700100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
112800100402     c* spedizioni "H"
112900100402     C                   Z-ADD     hWTSPEE       WSSTSPh
113000100402     C                   Z-ADD     hWTCLEE       WSSTCLh
113100100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
113200100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
113300100402     c
113400090206     C                   WRITE     WSLSS500
113500090206     C                   ENDIF
113600090206     C*
113700090206     C     WTSPED        IFGT      *ZERO
113800090206     C                   Z-ADD     §al           WSSDAL
113900090206     c                   movel     'G'           WSSTIE
114000090206     C                   MOVE      'ED'          WSSTIP
114100090206     C                   Z-ADD     FIL(K)        WSSLIN
114200090206     C                   Z-ADD     WTSPED        WSSTSP
114300090206     C                   Z-ADD     WTCLED        WSSTCL
114400090206     C                   Z-ADD(H)  WTKGED        WSSTKG
114500090206     C                   Z-ADD(H)  WTMCED        WSSTMC
114600100402     c* spedizioni "E"
114700100402     C                   Z-ADD     eWTSPED       WSSTSPe
114800100402     C                   Z-ADD     eWTCLED       WSSTCLe
114900100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
115000100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
115100100402     c* spedizioni "H"
115200100402     C                   Z-ADD     hWTSPED       WSSTSPh
115300100402     C                   Z-ADD     hWTCLED       WSSTCLh
115400100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
115500100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
115600100402     c
115700090206     C                   WRITE     WSLSS500
115800090206     C                   ENDIF
115900090206     C*
116000090206     C     WTSPEX        IFGT      *ZERO
116100090206     C                   Z-ADD     §al           WSSDAL
116200090206     c                   movel     'G'           WSSTIE
116300090206     C                   MOVE      'EX'          WSSTIP
116400090206     C                   Z-ADD     FIL(K)        WSSLIN
116500090206     C                   Z-ADD     WTSPEX        WSSTSP
116600090206     C                   Z-ADD     WTCLEX        WSSTCL
116700090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
116800090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
116900100402     c* spedizioni "E"
117000100402     C                   Z-ADD     eWTSPEX       WSSTSPe
117100100402     C                   Z-ADD     eWTCLEX       WSSTCLe
117200100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
117300100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
117400100402     c* spedizioni "H"
117500100402     C                   Z-ADD     hWTSPEX       WSSTSPh
117600100402     C                   Z-ADD     hWTCLEX       WSSTCLh
117700100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
117800100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
117900100402     c
118000090206     C                   WRITE     WSLSS500
118100090206     C                   ENDIF
118200090206     C*
118300090206     C     WTSPEA        IFGT      *ZERO
118400090206     C                   Z-ADD     §al           WSSDAL
118500090206     c                   movel     'G'           WSSTIE
118600090206     C                   MOVE      'EA'          WSSTIP
118700090206     C                   Z-ADD     FIL(K)        WSSLIN
118800090206     C                   Z-ADD     WTSPEA        WSSTSP
118900090206     C                   Z-ADD     WTCLEA        WSSTCL
119000090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
119100090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
119200100402     c* spedizioni "E"
119300100402     C                   Z-ADD     eWTSPEA       WSSTSPe
119400100402     C                   Z-ADD     eWTCLEA       WSSTCLe
119500100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
119600100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
119700100402     c* spedizioni "H"
119800100402     C                   Z-ADD     hWTSPEA       WSSTSPh
119900100402     C                   Z-ADD     hWTCLEA       WSSTCLh
120000100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
120100100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
120200100402     c
120300090206     C                   WRITE     WSLSS500
120400090206     C                   ENDIF
120500090206     C*
120600090206     C     WTSPPA        IFGT      *ZERO
120700090206     C                   Z-ADD     §al           WSSDAL
120800090206     c                   movel     'G'           WSSTIE
120900090206     C                   MOVE      'PA'          WSSTIP
121000090206     C                   Z-ADD     FIL(K)        WSSLIN
121100090206     C                   Z-ADD     WTSPPA        WSSTSP
121200090206     C                   Z-ADD     WTCLPA        WSSTCL
121300090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
121400090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
121500100402     c* spedizioni "E"
121600100402     C                   Z-ADD     eWTSPPA       WSSTSPe
121700100402     C                   Z-ADD     eWTCLPA       WSSTCLe
121800100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
121900100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
122000100402     c* spedizioni "H"
122100100402     C                   Z-ADD     hWTSPPA       WSSTSPh
122200100402     C                   Z-ADD     hWTCLPA       WSSTCLh
122300100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
122400100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
122500100402     c
122600090206     C                   WRITE     WSLSS500
122700090206     C                   ENDIF
122800091006     c* Extra budget
122900091006     C     WTSPIP_e      IFGT      *ZERO
123000091007     C                   Z-ADD     §al           WSSDAL
123100091006     c                   movel     'G'           WSSTIE
123200091006     C                   MOVE      'IP'          WSSTIP
123300091006     C                   Z-ADD     FIL(K)        WSSLIN
123400091006     C                   Z-ADD     WTSPIP_e      WSSTSP
123500091006     C                   Z-ADD     WTCLIP_e      WSSTCL
123600091006     C                   Z-ADD(H)  WTKGIP_e      WSSTKG
123700091006     C                   Z-ADD(H)  WTMCIP_e      WSSTMC
123800100402     c* spedizioni "E"
123900100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
124000100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
124100100402     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe
124200100402     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe
124300100402     c* spedizioni "H"
124400100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
124500100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
124600100402     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh
124700100402     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh
124800100402     c
124900091006     C                   WRITE     WSLSS550
125000091006     C                   ENDIF
125100100402     c
125200091006     C     WTSPIA_e      IFGT      *ZERO
125300091007     C                   Z-ADD     §al           WSSDAL
125400091006     c                   movel     'G'           WSSTIE
125500091006     C                   MOVE      'IA'          WSSTIP
125600091006     C                   Z-ADD     FIL(K)        WSSLIN
125700091006     C                   Z-ADD     WTSPIA_e      WSSTSP
125800091006     C                   Z-ADD     WTCLIA_e      WSSTCL
125900091006     C                   Z-ADD(H)  WTKGIA_e      WSSTKG
126000091006     C                   Z-ADD(H)  WTMCIA_e      WSSTMC
126100100402     c* spedizioni "E"
126200100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
126300100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
126400100402     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe
126500100402     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe
126600100402     c* spedizioni "H"
126700100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
126800100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
126900100402     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh
127000100402     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh
127100100402     c
127200091006     C                   WRITE     WSLSS550
127300091006     C                   ENDIF
127400100402     c
127500091006     C     WTSPEI_e      IFGT      *ZERO
127600091007     C                   Z-ADD     §al           WSSDAL
127700091006     c                   movel     'G'           WSSTIE
127800091006     C                   MOVE      'EI'          WSSTIP
127900091006     C                   Z-ADD     FIL(K)        WSSLIN
128000091006     C                   Z-ADD     WTSPEI_e      WSSTSP
128100091006     C                   Z-ADD     WTCLEI_e      WSSTCL
128200091006     C                   Z-ADD(H)  WTKGEI_e      WSSTKG
128300091006     C                   Z-ADD(H)  WTMCEI_e      WSSTMC
128400100402     c* spedizioni "E"
128500100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
128600100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
128700100402     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe
128800100402     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe
128900100402     c* spedizioni "H"
129000100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
129100100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
129200100402     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh
129300100402     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh
129400100402     c
129500091006     C                   WRITE     WSLSS550
129600091006     C                   ENDIF
129700100402     c
129800091006     C     WTSPEE_e      IFGT      *ZERO
129900091007     C                   Z-ADD     §al           WSSDAL
130000091006     c                   movel     'G'           WSSTIE
130100091006     C                   MOVE      'EE'          WSSTIP
130200091006     C                   Z-ADD     FIL(K)        WSSLIN
130300091006     C                   Z-ADD     WTSPEE_e      WSSTSP
130400091006     C                   Z-ADD     WTCLEE_e      WSSTCL
130500091006     C                   Z-ADD(H)  WTKGEE_e      WSSTKG
130600091006     C                   Z-ADD(H)  WTMCEE_e      WSSTMC
130700100402     c* spedizioni "E"
130800100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
130900100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
131000100402     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe
131100100402     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe
131200100402     c* spedizioni "H"
131300100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
131400100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
131500100402     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh
131600100402     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh
131700100402     c
131800091006     C                   WRITE     WSLSS550
131900091006     C                   ENDIF
132000091006     C*
132100091006     C     WTSPED_e      IFGT      *ZERO
132200091007     C                   Z-ADD     §al           WSSDAL
132300091006     c                   movel     'G'           WSSTIE
132400091006     C                   MOVE      'ED'          WSSTIP
132500091006     C                   Z-ADD     FIL(K)        WSSLIN
132600091006     C                   Z-ADD     WTSPED_e      WSSTSP
132700091006     C                   Z-ADD     WTCLED_e      WSSTCL
132800091006     C                   Z-ADD(H)  WTKGED_e      WSSTKG
132900091006     C                   Z-ADD(H)  WTMCED_e      WSSTMC
133000100402     c* spedizioni "E"
133100100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
133200100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
133300100402     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe
133400100402     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe
133500100402     c* spedizioni "H"
133600100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
133700100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
133800100402     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh
133900100402     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh
134000100402     c
134100091006     C                   WRITE     WSLSS550
134200091006     C                   ENDIF
134300091006     C*
134400091006     C     WTSPEX_e      IFGT      *ZERO
134500091007     C                   Z-ADD     §al           WSSDAL
134600091006     c                   movel     'G'           WSSTIE
134700091006     C                   MOVE      'EX'          WSSTIP
134800091006     C                   Z-ADD     FIL(K)        WSSLIN
134900091006     C                   Z-ADD     WTSPEX_e      WSSTSP
135000091006     C                   Z-ADD     WTCLEX_e      WSSTCL
135100091006     C                   Z-ADD(H)  WTKGEX_e      WSSTKG
135200091006     C                   Z-ADD(H)  WTMCEX_e      WSSTMC
135300100402     c* spedizioni "E"
135400100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
135500100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
135600100402     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe
135700100402     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe
135800100402     c* spedizioni "H"
135900100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
136000100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
136100100402     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh
136200100402     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh
136300100402     c
136400091006     C                   WRITE     WSLSS550
136500091006     C                   ENDIF
136600091006     C*
136700091006     C     WTSPEA_e      IFGT      *ZERO
136800091007     C                   Z-ADD     §al           WSSDAL
136900091006     c                   movel     'G'           WSSTIE
137000091006     C                   MOVE      'EA'          WSSTIP
137100091006     C                   Z-ADD     FIL(K)        WSSLIN
137200091006     C                   Z-ADD     WTSPEA_e      WSSTSP
137300091006     C                   Z-ADD     WTCLEA_e      WSSTCL
137400091006     C                   Z-ADD(H)  WTKGEA_e      WSSTKG
137500091006     C                   Z-ADD(H)  WTMCEA_e      WSSTMC
137600100402     c* spedizioni "E"
137700100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
137800100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
137900100402     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe
138000100402     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe
138100100402     c* spedizioni "H"
138200100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
138300100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
138400100402     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh
138500100402     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh
138600100402     c
138700091006     C                   WRITE     WSLSS550
138800091006     C                   ENDIF
138900091006     C*
139000091006     C     WTSPPA_e      IFGT      *ZERO
139100091007     C                   Z-ADD     §al           WSSDAL
139200091006     c                   movel     'G'           WSSTIE
139300091006     C                   MOVE      'PA'          WSSTIP
139400091006     C                   Z-ADD     FIL(K)        WSSLIN
139500091006     C                   Z-ADD     WTSPPA_e      WSSTSP
139600091006     C                   Z-ADD     WTCLPA_e      WSSTCL
139700091006     C                   Z-ADD(H)  WTKGPA_e      WSSTKG
139800091006     C                   Z-ADD(H)  WTMCPA_e      WSSTMC
139900100402     c* spedizioni "E"
140000100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
140100100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
140200100402     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe
140300100402     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe
140400100402     c* spedizioni "H"
140500100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
140600100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
140700100402     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh
140800100402     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh
140900100402     c
141000091006     C                   WRITE     WSLSS550
141100091006     C                   ENDIF
141200091006
141300090601     C                   ENDIF
141400090206     c*
141500090206     c* riepilogo
141600090601     c                   if        Wtipo='P' or Wtipo='R'
141700090206     C     WTSPIP_r      IFGT      *ZERO
141800090206     C                   Z-ADD     §dal          WSSDAL
141900090601     c**                 movel     'R'           WSSTIE
142000090601     c                   movel     Wtipo         WSSTIE
142100090206     C                   MOVE      'IP'          WSSTIP
142200090206     C                   Z-ADD     FIL(K)        WSSLIN
142300090206     C                   Z-ADD     WTSPIP_r      WSSTSP
142400090206     C                   Z-ADD     WTCLIP_r      WSSTCL
142500090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
142600090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
142700100402     c* spedizioni "E"
142800100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
142900100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
143000100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
143100100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
143200100402     c* spedizioni "H"
143300100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
143400100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
143500100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
143600100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
143700100402     c
143800090206     C                   WRITE     WSLSS500
143900090206     C                   ENDIF
144000100402     c
144100090206     C     WTSPIA_r      IFGT      *ZERO
144200090206     C                   Z-ADD     §dal          WSSDAL
144300090601     c**                 movel     'R'           WSSTIE
144400090601     c                   movel     Wtipo         WSSTIE
144500090206     C                   MOVE      'IA'          WSSTIP
144600090206     C                   Z-ADD     FIL(K)        WSSLIN
144700090206     C                   Z-ADD     WTSPIA_r      WSSTSP
144800090206     C                   Z-ADD     WTCLIA_r      WSSTCL
144900090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
145000090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
145100100402     c* spedizioni "E"
145200100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
145300100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
145400100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
145500100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
145600100402     c* spedizioni "H"
145700100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
145800100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
145900100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
146000100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
146100100402     c
146200090206     C                   WRITE     WSLSS500
146300090206     C                   ENDIF
146400100402
146500090206     C     WTSPEI_R      IFGT      *ZERO
146600090206     C                   Z-ADD     §dal          WSSDAL
146700090601     c**                 movel     'R'           WSSTIE
146800090601     c                   movel     Wtipo         WSSTIE
146900090206     C                   MOVE      'EI'          WSSTIP
147000090206     C                   Z-ADD     FIL(K)        WSSLIN
147100090206     C                   Z-ADD     WTSPEI_r      WSSTSP
147200090206     C                   Z-ADD     WTCLEI_r      WSSTCL
147300090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
147400090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
147500100402     c* Spedizioni "E"
147600100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
147700100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
147800100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
147900100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
148000100402     c* Spedizioni "H"
148100100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
148200100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
148300100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
148400100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
148500100402     c
148600090206     C                   WRITE     WSLSS500
148700090206     C                   ENDIF
148800100402     c
148900090206     C     WTSPEE_r      IFGT      *ZERO
149000090206     C                   Z-ADD     §dal          WSSDAL
149100090601     c**                 movel     'R'           WSSTIE
149200090601     c                   movel     Wtipo         WSSTIE
149300090206     C                   MOVE      'EE'          WSSTIP
149400090206     C                   Z-ADD     FIL(K)        WSSLIN
149500090206     C                   Z-ADD     WTSPEE_r      WSSTSP
149600090206     C                   Z-ADD     WTCLEE_r      WSSTCL
149700090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
149800090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
149900100402     c* Spedizioni "E"
150000100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
150100100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
150200100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
150300100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
150400100402     c* Spedizioni "H"
150500100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
150600100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
150700100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
150800100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
150900100402     c
151000090206     C                   WRITE     WSLSS500
151100090206     C                   ENDIF
151200090206     C*
151300090206     C     WTSPED_r      IFGT      *ZERO
151400090206     C                   Z-ADD     §dal          WSSDAL
151500090601     c**                 movel     'R'           WSSTIE
151600090601     c                   movel     Wtipo         WSSTIE
151700090206     C                   MOVE      'ED'          WSSTIP
151800090206     C                   Z-ADD     FIL(K)        WSSLIN
151900090206     C                   Z-ADD     WTSPED_r      WSSTSP
152000090206     C                   Z-ADD     WTCLED_r      WSSTCL
152100090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
152200090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
152300100402     c* Spedizioni "E"
152400100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
152500100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
152600100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
152700100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
152800100402     c* Spedizioni "H"
152900100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
153000100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
153100100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
153200100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
153300100402     c
153400090206     C                   WRITE     WSLSS500
153500090206     C                   ENDIF
153600090206     C*
153700090206     C     WTSPEX_r      IFGT      *ZERO
153800090206     C                   Z-ADD     §dal          WSSDAL
153900090601     c**                 movel     'R'           WSSTIE
154000090601     c                   movel     Wtipo         WSSTIE
154100090206     C                   MOVE      'EX'          WSSTIP
154200090206     C                   Z-ADD     FIL(K)        WSSLIN
154300090206     C                   Z-ADD     WTSPEX_r      WSSTSP
154400090206     C                   Z-ADD     WTCLEX_r      WSSTCL
154500090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
154600090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
154700100402     c* Spedizioni "E"
154800100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
154900100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
155000100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
155100100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
155200100402     c* Spedizioni "H"
155300100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
155400100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
155500100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
155600100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
155700100402     c
155800090206     C                   WRITE     WSLSS500
155900090206     C                   ENDIF
156000090206     C*
156100090206     C     WTSPEA_r      IFGT      *ZERO
156200090206     C                   Z-ADD     §dal          WSSDAL
156300090601     c**                 movel     'R'           WSSTIE
156400090601     c                   movel     Wtipo         WSSTIE
156500090206     C                   MOVE      'EA'          WSSTIP
156600090206     C                   Z-ADD     FIL(K)        WSSLIN
156700090206     C                   Z-ADD     WTSPEA_r      WSSTSP
156800090206     C                   Z-ADD     WTCLEA_r      WSSTCL
156900090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
157000090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
157100100402     c* Spedizioni "E"
157200100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
157300100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
157400100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
157500100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
157600100402     c* Spedizioni "H"
157700100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
157800100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
157900100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
158000100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
158100100402     c
158200090206     C                   WRITE     WSLSS500
158300090206     C                   ENDIF
158400090206     C*
158500090206     C     WTSPPA_r      IFGT      *ZERO
158600090206     C                   Z-ADD     §dal          WSSDAL
158700090601     c**                 movel     'R'           WSSTIE
158800090601     c                   movel     Wtipo         WSSTIE
158900090206     C                   MOVE      'PA'          WSSTIP
159000090206     C                   Z-ADD     FIL(K)        WSSLIN
159100090206     C                   Z-ADD     WTSPPA_r      WSSTSP
159200090206     C                   Z-ADD     WTCLPA_r      WSSTCL
159300090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
159400090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
159500100402     c* Spedizioni "E"
159600100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
159700100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
159800100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
159900100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
160000100402     c* Spedizioni "H"
160100100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
160200100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
160300100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
160400100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
160500100402     c
160600090206     C                   WRITE     WSLSS500
160700090206     C                   ENDIF
160800091006     c* Extra budget
160900091006     C     WTSPIP_re     IFGT      *ZERO
161000091006     C                   Z-ADD     §dal          WSSDAL
161100091006     c                   movel     Wtipo         WSSTIE
161200091006     C                   MOVE      'IP'          WSSTIP
161300091006     C                   Z-ADD     FIL(K)        WSSLIN
161400091006     C                   Z-ADD     WTSPIP_re     WSSTSP
161500091006     C                   Z-ADD     WTCLIP_re     WSSTCL
161600091006     C                   Z-ADD(H)  WTKGIP_re     WSSTKG
161700091006     C                   Z-ADD(H)  WTMCIP_re     WSSTMC
161800100402     c* Spedizioni "E"
161900100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
162000100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
162100100402     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe
162200100402     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe
162300100402     c* Spedizioni "H"
162400100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
162500100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
162600100402     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh
162700100402     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh
162800100402     c
162900091006     C                   WRITE     WSLSS550
163000091006     C                   ENDIF
163100100402     c
163200091006     C     WTSPIA_re     IFGT      *ZERO
163300091006     C                   Z-ADD     §dal          WSSDAL
163400091006     c                   movel     Wtipo         WSSTIE
163500091006     C                   MOVE      'IA'          WSSTIP
163600091006     C                   Z-ADD     FIL(K)        WSSLIN
163700091006     C                   Z-ADD     WTSPIA_re     WSSTSP
163800091006     C                   Z-ADD     WTCLIA_re     WSSTCL
163900091006     C                   Z-ADD(H)  WTKGIA_re     WSSTKG
164000091006     C                   Z-ADD(H)  WTMCIA_re     WSSTMC
164100100402     c* Spedizioni "E"
164200100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
164300100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
164400100402     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe
164500100402     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe
164600100402     c* Spedizioni "H"
164700100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
164800100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
164900100402     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh
165000100402     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh
165100100402     c
165200091006     C                   WRITE     WSLSS550
165300091006     C                   ENDIF
165400100402     c
165500091006     C     WTSPEI_Re     IFGT      *ZERO
165600091006     C                   Z-ADD     §dal          WSSDAL
165700091006     c                   movel     Wtipo         WSSTIE
165800091006     C                   MOVE      'EI'          WSSTIP
165900091006     C                   Z-ADD     FIL(K)        WSSLIN
166000091006     C                   Z-ADD     WTSPEI_re     WSSTSP
166100091006     C                   Z-ADD     WTCLEI_re     WSSTCL
166200091006     C                   Z-ADD(H)  WTKGEI_re     WSSTKG
166300091006     C                   Z-ADD(H)  WTMCEI_re     WSSTMC
166400100402     c* Spedizioni "E"
166500100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
166600100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
166700100402     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe
166800100402     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe
166900100402     c* Spedizioni "H"
167000100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
167100100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
167200100402     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh
167300100402     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh
167400100402     c
167500091006     C                   WRITE     WSLSS550
167600091006     C                   ENDIF
167700100402     c
167800091006     C     WTSPEE_re     IFGT      *ZERO
167900091006     C                   Z-ADD     §dal          WSSDAL
168000091006     c                   movel     Wtipo         WSSTIE
168100091006     C                   MOVE      'EE'          WSSTIP
168200091006     C                   Z-ADD     FIL(K)        WSSLIN
168300091006     C                   Z-ADD     WTSPEE_re     WSSTSP
168400091006     C                   Z-ADD     WTCLEE_re     WSSTCL
168500091006     C                   Z-ADD(H)  WTKGEE_re     WSSTKG
168600091006     C                   Z-ADD(H)  WTMCEE_re     WSSTMC
168700100402     c* Spedizioni "E"
168800100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
168900100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
169000100402     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe
169100100402     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe
169200100402     c* Spedizioni "h"
169300100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
169400100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
169500100402     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh
169600100402     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh
169700100402     c
169800091006     C                   WRITE     WSLSS550
169900091006     C                   ENDIF
170000091006     C*
170100091006     C     WTSPED_re     IFGT      *ZERO
170200091006     C                   Z-ADD     §dal          WSSDAL
170300091006     c                   movel     Wtipo         WSSTIE
170400091006     C                   MOVE      'ED'          WSSTIP
170500091006     C                   Z-ADD     FIL(K)        WSSLIN
170600091006     C                   Z-ADD     WTSPED_re     WSSTSP
170700091006     C                   Z-ADD     WTCLED_re     WSSTCL
170800091006     C                   Z-ADD(H)  WTKGED_re     WSSTKG
170900091006     C                   Z-ADD(H)  WTMCED_re     WSSTMC
171000100402     c* Spedizioni "E"
171100100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
171200100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
171300100402     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe
171400100402     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe
171500100402     c* Spedizioni "H"
171600100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
171700100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
171800100402     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh
171900100402     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh
172000100402     c
172100091006     C                   WRITE     WSLSS550
172200091006     C                   ENDIF
172300091006     C*
172400091006     C     WTSPEX_re     IFGT      *ZERO
172500091006     C                   Z-ADD     §dal          WSSDAL
172600091006     c                   movel     Wtipo         WSSTIE
172700091006     C                   MOVE      'EX'          WSSTIP
172800091006     C                   Z-ADD     FIL(K)        WSSLIN
172900091006     C                   Z-ADD     WTSPEX_re     WSSTSP
173000091006     C                   Z-ADD     WTCLEX_re     WSSTCL
173100091006     C                   Z-ADD(H)  WTKGEX_re     WSSTKG
173200091006     C                   Z-ADD(H)  WTMCEX_re     WSSTMC
173300100402     c* Spedizioni "E"
173400100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
173500100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
173600100402     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe
173700100402     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe
173800100402     c* Spedizioni "H"
173900100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
174000100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
174100100402     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh
174200100402     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh
174300100402     c
174400091006     C                   WRITE     WSLSS550
174500091006     C                   ENDIF
174600091006     C*
174700091006     C     WTSPEA_re     IFGT      *ZERO
174800091006     C                   Z-ADD     §dal          WSSDAL
174900091006     c                   movel     Wtipo         WSSTIE
175000091006     C                   MOVE      'EA'          WSSTIP
175100091006     C                   Z-ADD     FIL(K)        WSSLIN
175200091006     C                   Z-ADD     WTSPEA_re     WSSTSP
175300091006     C                   Z-ADD     WTCLEA_re     WSSTCL
175400091006     C                   Z-ADD(H)  WTKGEA_re     WSSTKG
175500091006     C                   Z-ADD(H)  WTMCEA_re     WSSTMC
175600100402     c* Spedizioni "E"
175700100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
175800100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
175900100402     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe
176000100402     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe
176100100402     c* Spedizioni "H"
176200100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
176300100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
176400100402     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh
176500100402     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh
176600100402     c
176700091006     C                   WRITE     WSLSS550
176800091006     C                   ENDIF
176900091006     C*
177000091006     C     WTSPPA_re     IFGT      *ZERO
177100091006     C                   Z-ADD     §dal          WSSDAL
177200091006     c                   movel     Wtipo         WSSTIE
177300091006     C                   MOVE      'PA'          WSSTIP
177400091006     C                   Z-ADD     FIL(K)        WSSLIN
177500091006     C                   Z-ADD     WTSPPA_re     WSSTSP
177600091006     C                   Z-ADD     WTCLPA_re     WSSTCL
177700091006     C                   Z-ADD(H)  WTKGPA_re     WSSTKG
177800091006     C                   Z-ADD(H)  WTMCPA_re     WSSTMC
177900100402     c* Spedizioni "E"
178000100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
178100100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
178200100402     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe
178300100402     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe
178400100402     c* Spedizioni "H"
178500100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
178600100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
178700100402     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh
178800100402     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh
178900100402     c
179000091006     C                   WRITE     WSLSS550
179100091006     C                   ENDIF
179200091006     C                   ENDIF
179300100402
179400090206     c                   enddo
179500090206     c                   endsr
179600970527     C**********************************************************
179700970529     C** CARICAMENTO DATI INIZIALI
179800970527     C**********************************************************
179900970529     C     CARINZ        BEGSR
180000970905     C                   SETOFF                                       90
180100970529     C     KTAS          KLIST
180200970529     C                   KFLD                    WAAS
180300970529     C                   KFLD                    WMGS
180400970529     C     KTAB          KLIST
180500970529     C                   KFLD                    TBLKUT
180600970529     C                   KFLD                    TBLCOD
180700970529     C                   KFLD                    TBLKEY
180800970529     C     KTB           KLIST
180900970529     C                   KFLD                    TBLKUT
181000970529     C                   KFLD                    TBLCOD
181100970529     C                   Z-ADD     1             TBLKUT
181200090204     c     kwfsmp        klist
181300090204     c                   kfld                    k_fil
181400090204     c                   kfld                    k_tip
181500090204     c                   kfld                    k_dat
181600090205     C     kar5          KLIST
181700090205     C                   KFLD                    tasaas
181800090205     C                   KFLD                    taslnp
181900090205     C                   KFLD                    tasnrs
182000090205     C                   KFLD                    tasnsp
182100090205     C                   KFLD                    k_trd
182200090209     C                   KFLD                    tasflp
182300090915     c* apertura file di stampa congelato in SAVE
182400090915     C* ESEGUO OVRPRTF
182500090915     C                   Z-ADD     61            LUNG             15 5
182600090915     C                   MOVEL     *BLANKS       COMMAN           80
182700090915     C                   MOVEA     CMD           COMMAN
182800090915     C                   CALL      'QCMDEXC'                            H1
182900090915     C                   PARM                    COMMAN
183000090915     C                   PARM                    LUNG
183100090915     C*
183200090915     C                   OPEN      QSYSPRT
183300090915     C*
183400970529     C*
183500970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
183600970527     C** PULIZIA DS
183700980826     C                   DO        400           K                 3 0
183800970527     C     K             OCCUR     DSFIL
183900970527     C                   CLEAR                   DSFIL
184000970527     C                   END
184100100402     C                   DO        400           K                 3 0
184200100402     C     K             OCCUR     DSFIL_e
184300100402     C                   CLEAR                   DSFIL_e
184400100402     C                   END
184500100402     C                   DO        400           K                 3 0
184600100402     C     K             OCCUR     DSFIL_h
184700100402     C                   CLEAR                   DSFIL_h
184800100402     C                   END
184900090206     C                   DO        400           K                 3 0
185000090206     C     K             OCCUR     DSFIL_smp
185100090206     C                   CLEAR                   DSFIL_smp
185200090206     C                   END
185300970527     C*
185400970527     C                   Z-ADD     0             K
185500970527     C     *LOVAL        SETLL     AZORG01L
185600970527     C                   DO        *HIVAL
185700090204     C                   READ      AZORG01L
185800090204     c                   if        %eof(azorg01l)
185900090204     c                   leave
186000090204     c                   endif
186100970527     C     ORGFAG        IFEQ      'F'
186200970527     C     ORGFAG        OREQ      'A'
186300970527     C                   ADD       1             K
186400970527     C                   MOVEL     ORGFIL        FIL(K)
186500090204     C                   MOVEL     ORGdes        DIL(K)
186600090204     C                   z-add     ORGcar        car(K)
186700090204     c                   movel     orgde8        og148
186800090206     c                   if        §oglpo='1' or §oglpo='2'
186900090206     c                   move      §oglpo        lpo(K)
187000090206     c                   else
187100090206     c                   z-add     2             lpo(K)
187200090206     c                   endif
187300090204     c                   movel     orgde3        og143
187400090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
187500090209     c**                 move      'E'           tif(K)
187600090209     c**                 else
187700090209     c**                 move      'I'           tif(K)
187800090209     c**                 endif
187900090209     c                   select
188000090209     c                   when      §ogntw='DPD'
188100090209     c                   move      'D'           tif(K)
188200090209     c                   when      §ogntw='EEX'
188300090209     c                   move      'X'           tif(K)
188400090209     c                   when      §ogntw='FED'
188500090209     c                   move      'F'           tif(K)
188600090209     c                   other
188700090209     c                   move      'I'           tif(K)
188800090209     c                   endsl
188900970527     C                   ENDIF
189000090204     C                   ENDDO
189100970529     C*
189200970529     C                   Z-ADD     0             K
189300970529     C                   MOVEL     'TB'          TBLCOD
189400970529     C     KTB           SETLL     TABEL00F
189500970529     C                   DO        *HIVAL
189600090204     C     KTB           READE     TABEL00F
189700090204     c                   if        %eof(tabel00f)
189800090204     c                   leave
189900090204     c                   endif
190000970529     C     TBLFLG        IFNE      '*'
190100970529     C                   MOVEL     TBLUNI        DSTB
190200970529     C     §TBRBL        IFEQ      'N'
190300970529     C     §TBRBL        OREQ      'S'
190400970529     C                   ADD       1             K
190500970529     C                   MOVEL     TBLKEY        TBO(K)
190600970529     C                   ENDIF
190700970529     C                   ENDIF
190800090204     C                   ENDDO
190900091006     C*
191000091006     c* Lettura TABELLA clienti extra budget
191100091006     C                   Z-ADD     0             K
191200091006     C                   MOVEL     'CXB'         TBecod
191300091006     C     tbecod        SETLL     tntbe01l
191400091006     C                   DO        *HIVAL
191500091006     C     tbecod        READE     tntbe01l
191600091006     c                   if        %eof(tntbe01l)
191700091006     c                   leave
191800091006     c                   endif
191900091006
192000091006     C     tbeatb        IFeq      ' '
192100091006     C                   MOVEL     TBeuni        Dcxb
192200091006     C                   ADD       1             K
192300091006     C                   MOVEL     TBeKE1        ksc_ex(k)
192400091007     C                   MOVE      §cxbctr       ksc_ex(k)
192500091006     C                   ENDIF
192600091006     C                   ENDDO
192700090601     c
192800090204     c* data elaborazione nel formato AAAA/MM/GG
192900090204     c                   time                    dataiso
193000090204     c                   move      dataiso       wdatael
193100090601     c*
193200090601     c* Pulisco i file per i dati che devo rielaborare
193300090601     c                   eval      wrkgetlista =
193400090601     c                             'delete from WSLSS50F where wsstie= '''   +
193500090603     c                              Wtipo +  ''''
193600090601     c                   if        Wtipo='R'
193700090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
193800090603     c                              ' or  wsstie=''G'''
193900090601     c                   endif
194000090609     c*
194100090601     C/EXEC SQL
194200090601     C+ execute immediate :wrkgetlista
194300090601     C/END-EXEC
194400090609     c
194500090609     c* Pulisco i file per i dati che devo rielaborare
194600090609     c                   clear                   wrkgetlista
194700090609     c                   eval      wrkgetlista =
194800090609     c                             'delete from WFSMP00F where wmptip= '''   +
194900090609     c                              Wtipo +  ''''
195000090609     c                   if        Wtipo='R'
195100090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
195200090609     c                              ' or  wmptip=''G'''
195300090609     c                   endif
195400090609     c*
195500090609     C/EXEC SQL
195600090609     C+ execute immediate :wrkgetlista
195700090609     C/END-EXEC
195800091007     c*
195900091007     c* Pulisco il file clienti extra budget
196000091007     c                   clear                   wrkgetlista
196100091007     c                   eval      wrkgetlista =
196200091007     c                             'delete from WSLSS55F where wsstie= '''   +
196300091007     c                              Wtipo +  ''''
196400091007     c                   if        Wtipo='R'
196500091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
196600091007     c                              ' or  wsstie=''G'''
196700091007     c                   endif
196800091007     c*
196900091007     C/EXEC SQL
197000091007     C+ execute immediate :wrkgetlista
197100091007     C/END-EXEC
197200090601     c
197300970527     C                   ENDSR
197400100402     c****************************************************************************
197500100402     c     SpeITA        BEGSR
197600100402     c* giornaliero
197700100402     c                   if        (tasaas*10000+tasmgs)=§al
197800100402     C                   ADD       1             WTSPIP
197900100402     C                   ADD       TASNCL        WTCLIP
198000100402     C                   ADD       w_peso        WTKGIP
198100100402     C                   ADD       TASVLF        WTMCIP
198200100402     c* tipo servizio  "E"
198300100402     c                   if        tastsp='E'
198400100402     C                   ADD       1             eWTSPIP
198500100402     C                   ADD       TASNCL        eWTCLIP
198600100402     C                   ADD       w_peso        eWTKGIP
198700100402     C                   ADD       TASVLF        eWTMCIP
198800100402     c                   endif
198900100402     c* tipo servizio  "H"
199000100402     c                   if        tastsp='H'
199100100402     C                   ADD       1             hWTSPIP
199200100402     C                   ADD       TASNCL        hWTCLIP
199300100402     C                   ADD       w_peso        hWTKGIP
199400100402     C                   ADD       TASVLF        hWTMCIP
199500100402     c                   endif
199600100402     c**
199700100402     c* Extra budget
199800100402     c                   if        *in35
199900100402     C                   ADD       1             WTSPIP_e
200000100402     C                   ADD       TASNCL        WTCLIP_e
200100100402     C                   ADD       w_peso        WTKGIP_e
200200100402     C                   ADD       TASVLF        WTMCIP_e
200300100402     c* tipo servizio  "E"
200400100402     c                   if        tastsp='E'
200500100402     C                   ADD       1             eWTSPIP_e
200600100402     C                   ADD       TASNCL        eWTCLIP_e
200700100402     C                   ADD       w_peso        eWTKGIP_e
200800100402     C                   ADD       TASVLF        eWTMCIP_e
200900100402     c                   endif
201000100402     c* tipo servizio  "H"
201100100402     c                   if        tastsp='H'
201200100402     C                   ADD       1             hWTSPIP_e
201300100402     C                   ADD       TASNCL        hWTCLIP_e
201400100402     C                   ADD       w_peso        hWTKGIP_e
201500100402     C                   ADD       TASVLF        hWTMCIP_e
201600100402     c                   endif
201700100402
201800100402     c                   endif
201900100402     c                   endif
202000100402     c* riepilogo
202100100402     C                   ADD       1             WTSPIP_r
202200100402     C                   ADD       TASNCL        WTCLIP_r
202300100402     C                   ADD       w_peso        WTKGIP_r
202400100402     C                   ADD       TASVLF        WTMCIP_r
202500100402     c* tipo servizio  "E"
202600100402     c                   if        tastsp='E'
202700100402     C                   ADD       1             eWTSPIP_r
202800100402     C                   ADD       TASNCL        eWTCLIP_r
202900100402     C                   ADD       w_peso        eWTKGIP_r
203000100402     C                   ADD       TASVLF        eWTMCIP_r
203100100402     c                   endif
203200100402     c* tipo servizio  "H"
203300100402     c                   if        tastsp='H'
203400100402     C                   ADD       1             hWTSPIP_r
203500100402     C                   ADD       TASNCL        hWTCLIP_r
203600100402     C                   ADD       w_peso        hWTKGIP_r
203700100402     C                   ADD       TASVLF        hWTMCIP_r
203800100402     c                   endif
203900100402     c* Extra budget
204000100402     c                   if        *in35
204100100402     C                   ADD       1             WTSPIP_re
204200100402     C                   ADD       TASNCL        WTCLIP_re
204300100402     C                   ADD       w_peso        WTKGIP_re
204400100402     C                   ADD       TASVLF        WTMCIP_re
204500100402     c* tipo servizio  "E"
204600100402     c                   if        tastsp='E'
204700100402     C                   ADD       1             eWTSPIP_re
204800100402     C                   ADD       TASNCL        eWTCLIP_re
204900100402     C                   ADD       w_peso        eWTKGIP_re
205000100402     C                   ADD       TASVLF        eWTMCIP_re
205100100402     c                   endif
205200100402     c* tipo servizio  "H"
205300100402     c                   if        tastsp='H'
205400100402     C                   ADD       1             hWTSPIP_re
205500100402     C                   ADD       TASNCL        hWTCLIP_re
205600100402     C                   ADD       w_peso        hWTKGIP_re
205700100402     C                   ADD       TASVLF        hWTMCIP_re
205800100402     c                   endif
205900100402     c
206000100402     c                   endif
206100100402     c                   ENDSR
206200100402     c****************************************************************************
206300100402     c     SpeIMPORT     BEGSR
206400100402     c* giornaliero
206500100402     c                   if        (tasaas*10000+tasmgs)=§al
206600100402     C                   ADD       1             WTSPEI
206700100402     C                   ADD       TASNCL        WTCLEI
206800100402     C                   ADD       w_peso        WTKGEI
206900100402     C                   ADD       TASVLF        WTMCEI
207000100402     c* tipo servizio  "E"
207100100402     c                   if        tastsp='E'
207200100402     C                   ADD       1             eWTSPEI
207300100402     C                   ADD       TASNCL        eWTCLEI
207400100402     C                   ADD       w_peso        eWTKGEI
207500100402     C                   ADD       TASVLF        eWTMCEI
207600100402     c                   endif
207700100402     c* tipo servizio  "H"
207800100402     c                   if        tastsp='H'
207900100402     C                   ADD       1             hWTSPEI
208000100402     C                   ADD       TASNCL        hWTCLEI
208100100402     C                   ADD       w_peso        hWTKGEI
208200100402     C                   ADD       TASVLF        hWTMCEI
208300100402     c                   endif
208400100402     c* Extra Budget
208500100402     c                   if        *in35
208600100402     C                   ADD       1             WTSPEI_e
208700100402     C                   ADD       TASNCL        WTCLEI_e
208800100402     C                   ADD       w_peso        WTKGEI_e
208900100402     C                   ADD       TASVLF        WTMCEI_e
209000100402     c* tipo servizio  "E"
209100100402     c                   if        tastsp='E'
209200100402     C                   ADD       1             eWTSPEI_e
209300100402     C                   ADD       TASNCL        eWTCLEI_e
209400100402     C                   ADD       w_peso        eWTKGEI_e
209500100402     C                   ADD       TASVLF        eWTMCEI_e
209600100402     c                   endif
209700100402     c* tipo servizio  "H"
209800100402     c                   if        tastsp='H'
209900100402     C                   ADD       1             hWTSPEI_e
210000100402     C                   ADD       TASNCL        hWTCLEI_e
210100100402     C                   ADD       w_peso        hWTKGEI_e
210200100402     C                   ADD       TASVLF        hWTMCEI_e
210300100402     c                   endif
210400100402     c
210500100402     c                   endif
210600100402     c                   endif
210700100402     c* riepilogo
210800100402     C                   ADD       1             WTSPEI_r
210900100402     C                   ADD       TASNCL        WTCLEI_r
211000100402     C                   ADD       w_peso        WTKGEI_r
211100100402     C                   ADD       TASVLF        WTMCEI_r
211200100402     c* tipo servizio  "E"
211300100402     c                   if        tastsp='E'
211400100402     C                   ADD       1             eWTSPEI_r
211500100402     C                   ADD       TASNCL        eWTCLEI_r
211600100402     C                   ADD       w_peso        eWTKGEI_r
211700100402     C                   ADD       TASVLF        eWTMCEI_r
211800100402     c                   endif
211900100402     c* tipo servizio  "H"
212000100402     c                   if        tastsp='H'
212100100402     C                   ADD       1             hWTSPEI_r
212200100402     C                   ADD       TASNCL        hWTCLEI_r
212300100402     C                   ADD       w_peso        hWTKGEI_r
212400100402     C                   ADD       TASVLF        hWTMCEI_r
212500100402     c                   endif
212600100402     c* extra budget
212700100402     c                   if        *in35
212800100402     C                   ADD       1             WTSPEI_re
212900100402     C                   ADD       TASNCL        WTCLEI_re
213000100402     C                   ADD       w_peso        WTKGEI_re
213100100402     C                   ADD       TASVLF        WTMCEI_re
213200100402     c* tipo servizio  "E"
213300100402     c                   if        tastsp='E'
213400100402     C                   ADD       1             eWTSPEI_re
213500100402     C                   ADD       TASNCL        eWTCLEI_re
213600100402     C                   ADD       w_peso        eWTKGEI_re
213700100402     C                   ADD       TASVLF        eWTMCEI_re
213800100402     c                   endif
213900100402     c* tipo servizio  "H"
214000100402     c                   if        tastsp='H'
214100100402     C                   ADD       1             hWTSPEI_re
214200100402     C                   ADD       TASNCL        hWTCLEI_re
214300100402     C                   ADD       w_peso        hWTKGEI_re
214400100402     C                   ADD       TASVLF        hWTMCEI_re
214500100402     c                   endif
214600100402     c                   endif
214700100402     c                   ENDSR
214800100402     c****************************************************************************
214900100402     c     speEXPRESS    BEGSR
215000100402     c* giornaliero
215100100402     c                   if        (tasaas*10000+tasmgs)=§al
215200100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
215300100402     C                   ADD       TASNCL        WTCLEE
215400100402     C                   ADD       w_peso        WTKGEE
215500100402     C                   ADD       TASVLF        WTMCEE
215600100402     c* tipo servizio  "E"
215700100402     c                   if        tastsp='E'
215800100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
215900100402     C                   ADD       TASNCL        eWTCLEE
216000100402     C                   ADD       w_peso        eWTKGEE
216100100402     C                   ADD       TASVLF        eWTMCEE
216200100402     c                   endif
216300100402     c* tipo servizio  "H"
216400100402     c                   if        tastsp='H'
216500100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
216600100402     C                   ADD       TASNCL        hWTCLEE
216700100402     C                   ADD       w_peso        hWTKGEE
216800100402     C                   ADD       TASVLF        hWTMCEE
216900100402     c                   endif
217000100402     c* extra budget
217100100402     c                   if        *in35
217200100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
217300100402     C                   ADD       TASNCL        WTCLEE_e
217400100402     C                   ADD       w_peso        WTKGEE_e
217500100402     C                   ADD       TASVLF        WTMCEE_e
217600100402     c* tipo servizio  "E"
217700100402     c                   if        tastsp='E'
217800100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
217900100402     C                   ADD       TASNCL        eWTCLEE_e
218000100402     C                   ADD       w_peso        eWTKGEE_e
218100100402     C                   ADD       TASVLF        eWTMCEE_e
218200100402     c                   endif
218300100402     c* tipo servizio  "H"
218400100402     c                   if        tastsp='H'
218500100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
218600100402     C                   ADD       TASNCL        hWTCLEE_e
218700100402     C                   ADD       w_peso        hWTKGEE_e
218800100402     C                   ADD       TASVLF        hWTMCEE_e
218900100402     c                   endif
219000100402     c
219100100402     c                   endif
219200100402     c                   endif
219300100402     c* riepilogo
219400100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
219500100402     C                   ADD       TASNCL        WTCLEE_r
219600100402     C                   ADD       w_peso        WTKGEE_r
219700100402     C                   ADD       TASVLF        WTMCEE_r
219800100402     c* tipo servizio  "E"
219900100402     c                   if        tastsp='E'
220000100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
220100100402     C                   ADD       TASNCL        eWTCLEE_r
220200100402     C                   ADD       w_peso        eWTKGEE_r
220300100402     C                   ADD       TASVLF        eWTMCEE_r
220400100402     c                   endif
220500100402     c* tipo servizio  "H"
220600100402     c                   if        tastsp='H'
220700100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
220800100402     C                   ADD       TASNCL        hWTCLEE_r
220900100402     C                   ADD       w_peso        hWTKGEE_r
221000100402     C                   ADD       TASVLF        hWTMCEE_r
221100100402     c                   endif
221200100402     c* extra budget
221300100402     c                   if        *in35
221400100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
221500100402     C                   ADD       TASNCL        WTCLEE_re
221600100402     C                   ADD       w_peso        WTKGEE_re
221700100402     C                   ADD       TASVLF        WTMCEE_re
221800100402     c* tipo servizio  "E"
221900100402     c                   if        tastsp='E'
222000100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
222100100402     C                   ADD       TASNCL        eWTCLEE_re
222200100402     C                   ADD       w_peso        eWTKGEE_re
222300100402     C                   ADD       TASVLF        eWTMCEE_re
222400100402     c                   endif
222500100402     c* tipo servizio  "H"
222600100402     c                   if        tastsp='H'
222700100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
222800100402     C                   ADD       TASNCL        hWTCLEE_re
222900100402     C                   ADD       w_peso        hWTKGEE_re
223000100402     C                   ADD       TASVLF        hWTMCEE_re
223100100402     c                   endif
223200100402     c                   endif
223300100402     c
223400100402     c                   ENDSR
223500100402     c****************************************************************************
223600100402     c     speDPD        BEGSR
223700100402     c* giornaliero
223800100402     c                   if        (tasaas*10000+tasmgs)=§al
223900100402     C                   ADD       1             WTSPED                         DPD
224000100402     C                   ADD       TASNCL        WTCLED
224100100402     C                   ADD       w_peso        WTKGED
224200100402     C                   ADD       TASVLF        WTMCED
224300100402     c* tipo servizio  "E"
224400100402     c                   if        tastsp='E'
224500100402     C                   ADD       1             eWTSPED                         DPD
224600100402     C                   ADD       TASNCL        eWTCLED
224700100402     C                   ADD       w_peso        eWTKGED
224800100402     C                   ADD       TASVLF        eWTMCED
224900100402     c                   endif
225000100402     c* tipo servizio  "H"
225100100402     c                   if        tastsp='H'
225200100402     C                   ADD       1             hWTSPED                         DPD
225300100402     C                   ADD       TASNCL        hWTCLED
225400100402     C                   ADD       w_peso        hWTKGED
225500100402     C                   ADD       TASVLF        hWTMCED
225600100402     c                   endif
225700100402     c* extra budget
225800100402     c                   if        *in35
225900100402     C                   ADD       1             WTSPED_e                       DPD
226000100402     C                   ADD       TASNCL        WTCLED_e
226100100402     C                   ADD       w_peso        WTKGED_e
226200100402     C                   ADD       TASVLF        WTMCED_e
226300100402     c* tipo servizio  "E"
226400100402     c                   if        tastsp='E'
226500100402     C                   ADD       1             eWTSPED_e                       DPD
226600100402     C                   ADD       TASNCL        eWTCLED_e
226700100402     C                   ADD       w_peso        eWTKGED_e
226800100402     C                   ADD       TASVLF        eWTMCED_e
226900100402     c                   endif
227000100402     c* tipo servizio  "H"
227100100402     c                   if        tastsp='H'
227200100402     C                   ADD       1             hWTSPED_e                       DPD
227300100402     C                   ADD       TASNCL        hWTCLED_e
227400100402     C                   ADD       w_peso        hWTKGED_e
227500100402     C                   ADD       TASVLF        hWTMCED_e
227600100402     c                   endif
227700100402     c
227800100402     c                   endif
227900100402     c                   endif
228000100402     c* riepilogo
228100100402     C                   ADD       1             WTSPED_r                       DPD
228200100402     C                   ADD       TASNCL        WTCLED_r
228300100402     C                   ADD       w_peso        WTKGED_r
228400100402     C                   ADD       TASVLF        WTMCED_r
228500100402     c* tipo servizio  "E"
228600100402     c                   if        tastsp='E'
228700100402     C                   ADD       1             eWTSPED_r                       DPD
228800100402     C                   ADD       TASNCL        eWTCLED_r
228900100402     C                   ADD       w_peso        eWTKGED_r
229000100402     C                   ADD       TASVLF        eWTMCED_r
229100100402     c                   endif
229200100402     c* tipo servizio  "H"
229300100402     c                   if        tastsp='H'
229400100402     C                   ADD       1             hWTSPED_r                       DPD
229500100402     C                   ADD       TASNCL        hWTCLED_r
229600100402     C                   ADD       w_peso        hWTKGED_r
229700100402     C                   ADD       TASVLF        hWTMCED_r
229800100402     c                   endif
229900100402     c* extra budget
230000100402     c                   if        *in35
230100100402     C                   ADD       1             WTSPED_re                       DPD
230200100402     C                   ADD       TASNCL        WTCLED_re
230300100402     C                   ADD       w_peso        WTKGED_re
230400100402     C                   ADD       TASVLF        WTMCED_re
230500100402     c* tipo servizio  "E"
230600100402     c                   if        tastsp='E'
230700100402     C                   ADD       1             eWTSPED_re                       DPD
230800100402     C                   ADD       TASNCL        eWTCLED_re
230900100402     C                   ADD       w_peso        eWTKGED_re
231000100402     C                   ADD       TASVLF        eWTMCED_re
231100100402     c                   endif
231200100402     c* tipo servizio  "H"
231300100402     c                   if        tastsp='H'
231400100402     C                   ADD       1             hWTSPED_re                       DPD
231500100402     C                   ADD       TASNCL        hWTCLED_re
231600100402     C                   ADD       w_peso        hWTKGED_re
231700100402     C                   ADD       TASVLF        hWTMCED_re
231800100402     c                   endif
231900100402     c                   endif
232000100402     c                   ENDSR
232100100402     c****************************************************************************
232200100402     c     speFEDEX      BEGSR
232300100402     c* giornaliero
232400100402     c                   if        (tasaas*10000+tasmgs)=§al
232500100402     C                   ADD       1             WTSPEX                         FEDEX
232600100402     C                   ADD       TASNCL        WTCLEX
232700100402     C                   ADD       w_peso        WTKGEX
232800100402     C                   ADD       TASVLF        WTMCEX
232900100402     c* tipo servizio  "E"
233000100402     c                   if        tastsp='E'
233100100402     C                   ADD       1             eWTSPEX                         FEDEX
233200100402     C                   ADD       TASNCL        eWTCLEX
233300100402     C                   ADD       w_peso        eWTKGEX
233400100402     C                   ADD       TASVLF        eWTMCEX
233500100402     c                   endif
233600100402     c* tipo servizio  "H"
233700100402     c                   if        tastsp='H'
233800100402     C                   ADD       1             hWTSPEX                         FEDEX
233900100402     C                   ADD       TASNCL        hWTCLEX
234000100402     C                   ADD       w_peso        hWTKGEX
234100100402     C                   ADD       TASVLF        hWTMCEX
234200100402     c                   endif
234300100402     c* extra budget
234400100402     c                   if        *in35
234500100402     C                   ADD       1             WTSPEX_e                       FEDEX
234600100402     C                   ADD       TASNCL        WTCLEX_e
234700100402     C                   ADD       w_peso        WTKGEX_e
234800100402     C                   ADD       TASVLF        WTMCEX_e
234900100402     c* tipo servizio  "E"
235000100402     c                   if        tastsp='E'
235100100402     C                   ADD       1             eWTSPEX_e                       FEDEX
235200100402     C                   ADD       TASNCL        eWTCLEX_e
235300100402     C                   ADD       w_peso        eWTKGEX_e
235400100402     C                   ADD       TASVLF        eWTMCEX_e
235500100402     c                   endif
235600100402     c* tipo servizio  "H"
235700100402     c                   if        tastsp='H'
235800100402     C                   ADD       1             hWTSPEX_e                       FEDEX
235900100402     C                   ADD       TASNCL        hWTCLEX_e
236000100402     C                   ADD       w_peso        hWTKGEX_e
236100100402     C                   ADD       TASVLF        hWTMCEX_e
236200100402     c                   endif
236300100402     c                   endif
236400100402     c                   endif
236500100402     c* riepilogo
236600100402     C                   ADD       1             WTSPEX_r                       FEDEX
236700100402     C                   ADD       TASNCL        WTCLEX_r
236800100402     C                   ADD       w_peso        WTKGEX_r
236900100402     C                   ADD       TASVLF        WTMCEX_r
237000100402     c* tipo servizio  "E"
237100100402     c                   if        tastsp='E'
237200100402     C                   ADD       1             eWTSPEX_r                       FEDEX
237300100402     C                   ADD       TASNCL        eWTCLEX_r
237400100402     C                   ADD       w_peso        eWTKGEX_r
237500100402     C                   ADD       TASVLF        eWTMCEX_r
237600100402     c                   endif
237700100402     c* tipo servizio  "H"
237800100402     c                   if        tastsp='H'
237900100402     C                   ADD       1             hWTSPEX_r                       FEDEX
238000100402     C                   ADD       TASNCL        hWTCLEX_r
238100100402     C                   ADD       w_peso        hWTKGEX_r
238200100402     C                   ADD       TASVLF        hWTMCEX_r
238300100402     c                   endif
238400100402     c* Extra budget
238500100402     c                   if        *in35
238600100402     C                   ADD       1             WTSPEX_re                       FEDEX
238700100402     C                   ADD       TASNCL        WTCLEX_re
238800100402     C                   ADD       w_peso        WTKGEX_re
238900100402     C                   ADD       TASVLF        WTMCEX_re
239000100402     c* tipo servizio  "E"
239100100402     c                   if        tastsp='E'
239200100402     C                   ADD       1             eWTSPEX_re                       FEDEX
239300100402     C                   ADD       TASNCL        eWTCLEX_re
239400100402     C                   ADD       w_peso        eWTKGEX_re
239500100402     C                   ADD       TASVLF        eWTMCEX_re
239600100402     c                   endif
239700100402     c* tipo servizio  "H"
239800100402     c                   if        tastsp='H'
239900100402     C                   ADD       1             hWTSPEX_re                       FEDEX
240000100402     C                   ADD       TASNCL        hWTCLEX_re
240100100402     C                   ADD       w_peso        hWTKGEX_re
240200100402     C                   ADD       TASVLF        hWTMCEX_re
240300100402     c                   endif
240400100402     c
240500100402     c                   endif
240600100402     c                   ENDSR
240700100402     c****************************************************************************
240800100402     c     speITA_LNA    BEGSR
240900100402     c* giornaliero
241000100402     c                   if        (tasaas*10000+tasmgs)=§al
241100100402     C                   ADD       1             WTSPIA
241200100402     C                   ADD       TASNCL        WTCLIA
241300100402     C                   ADD       W_peso        WTKGIA
241400100402     C                   ADD       TASVLF        WTMCIA
241500100402     c* tipo servizio  "E"
241600100402     c                   if        tastsp='E'
241700100402     C                   ADD       1             eWTSPIA
241800100402     C                   ADD       TASNCL        eWTCLIA
241900100402     C                   ADD       W_peso        eWTKGIA
242000100402     C                   ADD       TASVLF        eWTMCIA
242100100402     c                   endif
242200100402     c* tipo servizio  "H"
242300100402     c                   if        tastsp='H'
242400100402     C                   ADD       1             hWTSPIA
242500100402     C                   ADD       TASNCL        hWTCLIA
242600100402     C                   ADD       W_peso        hWTKGIA
242700100402     C                   ADD       TASVLF        hWTMCIA
242800100402     c                   endif
242900100402     c* extra budget
243000100402     c                   if        *in35
243100100402     C                   ADD       1             WTSPIA_e
243200100402     C                   ADD       TASNCL        WTCLIA_e
243300100402     C                   ADD       w_peso        WTKGIA_e
243400100402     C                   ADD       TASVLF        WTMCIA_e
243500100402     c* tipo servizio  "E"
243600100402     c                   if        tastsp='E'
243700100402     C                   ADD       1             eWTSPIA_e
243800100402     C                   ADD       TASNCL        eWTCLIA_e
243900100402     C                   ADD       w_peso        eWTKGIA_e
244000100402     C                   ADD       TASVLF        eWTMCIA_e
244100100402     c                   endif
244200100402     c* tipo servizio  "H"
244300100402     c                   if        tastsp='H'
244400100402     C                   ADD       1             hWTSPIA_e
244500100402     C                   ADD       TASNCL        hWTCLIA_e
244600100402     C                   ADD       w_peso        hWTKGIA_e
244700100402     C                   ADD       TASVLF        hWTMCIA_e
244800100402     c                   endif
244900100402     c
245000100402     c                   endif
245100100402     c                   endif
245200100402     c* riepilogo
245300100402     C                   ADD       1             WTSPIA_r
245400100402     C                   ADD       TASNCL        WTCLIA_r
245500100402     C                   ADD       w_peso        WTKGIA_r
245600100402     C                   ADD       TASVLF        WTMCIA_r
245700100402     c* tipo servizio  "E"
245800100402     c                   if        tastsp='E'
245900100402     C                   ADD       1             eWTSPIA_r
246000100402     C                   ADD       TASNCL        eWTCLIA_r
246100100402     C                   ADD       w_peso        eWTKGIA_r
246200100402     C                   ADD       TASVLF        eWTMCIA_r
246300100402     c                   endif
246400100402     c* tipo servizio  "H"
246500100402     c                   if        tastsp='H'
246600100402     C                   ADD       1             hWTSPIA_r
246700100402     C                   ADD       TASNCL        hWTCLIA_r
246800100402     C                   ADD       w_peso        hWTKGIA_r
246900100402     C                   ADD       TASVLF        hWTMCIA_r
247000100402     c                   endif
247100100402     c* extra budget
247200100402     c                   if        *in35
247300100402     C                   ADD       1             WTSPIA_re
247400100402     C                   ADD       TASNCL        WTCLIA_re
247500100402     C                   ADD       w_peso        WTKGIA_re
247600100402     C                   ADD       TASVLF        WTMCIA_re
247700100402     c* tipo servizio  "E"
247800100402     c                   if        tastsp='E'
247900100402     C                   ADD       1             eWTSPIA_re
248000100402     C                   ADD       TASNCL        eWTCLIA_re
248100100402     C                   ADD       w_peso        eWTKGIA_re
248200100402     C                   ADD       TASVLF        eWTMCIA_re
248300100402     c                   endif
248400100402     c* tipo servizio  "H"
248500100402     c                   if        tastsp='H'
248600100402     C                   ADD       1             hWTSPIA_re
248700100402     C                   ADD       TASNCL        hWTCLIA_re
248800100402     C                   ADD       w_peso        hWTKGIA_re
248900100402     C                   ADD       TASVLF        hWTMCIA_re
249000100402     c                   endif
249100100402     c                   endif
249200100402     c                   ENDSR
249300100402     c****************************************************************************
249400100402     c     speEXPORT     BEGSR
249500100402     c* giornaliero
249600100402     c                   if        (tasaas*10000+tasmgs)=§al
249700100402     C                   ADD       1             WTSPEA
249800100402     C                   ADD       TASNCL        WTCLEA
249900100402     C                   ADD       w_peso        WTKGEA
250000100402     C                   ADD       TASVLF        WTMCEA
250100100402     c* tipo servizio  "E"
250200100402     c                   if        tastsp='E'
250300100402     C                   ADD       1             eWTSPEA
250400100402     C                   ADD       TASNCL        eWTCLEA
250500100402     C                   ADD       w_peso        eWTKGEA
250600100402     C                   ADD       TASVLF        eWTMCEA
250700100402     c                   endif
250800100402     c* tipo servizio  "H"
250900100402     c                   if        tastsp='H'
251000100402     C                   ADD       1             hWTSPEA
251100100402     C                   ADD       TASNCL        hWTCLEA
251200100402     C                   ADD       w_peso        hWTKGEA
251300100402     C                   ADD       TASVLF        hWTMCEA
251400100402     c                   endif
251500100402     c* extra budget
251600100402     c                   if        *in35
251700100402     C                   ADD       1             WTSPEA_e
251800100402     C                   ADD       TASNCL        WTCLEA_e
251900100402     C                   ADD       w_peso        WTKGEA_e
252000100402     C                   ADD       TASVLF        WTMCEA_e
252100100402     c* tipo servizio  "E"
252200100402     c                   if        tastsp='E'
252300100402     C                   ADD       1             eWTSPEA_e
252400100402     C                   ADD       TASNCL        eWTCLEA_e
252500100402     C                   ADD       w_peso        eWTKGEA_e
252600100402     C                   ADD       TASVLF        eWTMCEA_e
252700100402     c                   endif
252800100402     c* tipo servizio  "H"
252900100402     c                   if        tastsp='H'
253000100402     C                   ADD       1             hWTSPEA_e
253100100402     C                   ADD       TASNCL        hWTCLEA_e
253200100402     C                   ADD       w_peso        hWTKGEA_e
253300100402     C                   ADD       TASVLF        hWTMCEA_e
253400100402     c                   endif
253500100402     c
253600100402     c                   endif
253700100402     c                   endif
253800100402     c* riepilogo
253900100402     C                   ADD       1             WTSPEA_r
254000100402     C                   ADD       TASNCL        WTCLEA_r
254100100402     C                   ADD       w_peso        WTKGEA_r
254200100402     C                   ADD       TASVLF        WTMCEA_r
254300100402     c* tipo servizio  "E"
254400100402     c                   if        tastsp='E'
254500100402     C                   ADD       1             eWTSPEA_r
254600100402     C                   ADD       TASNCL        eWTCLEA_r
254700100402     C                   ADD       w_peso        eWTKGEA_r
254800100402     C                   ADD       TASVLF        eWTMCEA_r
254900100402     c                   endif
255000100402     c* tipo servizio  "H"
255100100402     c                   if        tastsp='H'
255200100402     C                   ADD       1             hWTSPEA_r
255300100402     C                   ADD       TASNCL        hWTCLEA_r
255400100402     C                   ADD       w_peso        hWTKGEA_r
255500100402     C                   ADD       TASVLF        hWTMCEA_r
255600100402     c                   endif
255700100402     c* extra budget
255800100402     c                   if        *in35
255900100402     C                   ADD       1             WTSPEA_re
256000100402     C                   ADD       TASNCL        WTCLEA_re
256100100402     C                   ADD       w_peso        WTKGEA_re
256200100402     C                   ADD       TASVLF        WTMCEA_re
256300100402     c* tipo servizio  "E"
256400100402     c                   if        tastsp='E'
256500100402     C                   ADD       1             eWTSPEA_re
256600100402     C                   ADD       TASNCL        eWTCLEA_re
256700100402     C                   ADD       w_peso        eWTKGEA_re
256800100402     C                   ADD       TASVLF        eWTMCEA_re
256900100402     c                   endif
257000100402     c* tipo servizio  "H"
257100100402     c                   if        tastsp='H'
257200100402     C                   ADD       1             hWTSPEA_re
257300100402     C                   ADD       TASNCL        hWTCLEA_re
257400100402     C                   ADD       w_peso        hWTKGEA_re
257500100402     C                   ADD       TASVLF        hWTMCEA_re
257600100402     c                   endif
257700100402     c
257800100402     c                   endif
257900100402     c                   ENDSR
258000100402     c****************************************************************************
258100970519     OQSYSPRT   E            TESTA          2 02
258200090206     O                                         +  1 'Terminata elaborazione'
258300090206     o                                         +  2 'statistiche spedizioni'
258400090206     O                                         +  2 'DEL'
258500970519     O                       UDATE         Y   +  1
258600970528     O                       §DAL              +  5 '    /  /  '
258700970528     O                                         +  1 '>'
258800970528     O                       §AL               +  1 '    /  /  '
258900090206     O                                          120 'TNSSL3R1'
259000970519     O                       PAGE          Z    132
259100970519     O                                          128 'PAG.'
259200970529     O          E            TESTA          2
259300970529     O                                           14 'TIPI BOLLA OK:'
259400970529     O                       TBO               +  1
259500970529     O          E            ERR            1
259600970529     O                       TASLNP               3
259700970529     O                       TASLNA            +  1
259800970529     O                       TASAAS            +  1
259900970529     O                       TASMGS            +  1
260000970529     O                       TASNRS            +  1
260100970529     O                       TASNSP            +  1
260200970529     O                       TASTBL            +  1
260300090915**
260400090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
