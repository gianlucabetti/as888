000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200091006     F* TNSSL3R2 H-----------------------------------------------------H
000300090204     F*-------   Statistiche spedizioni
000400080312     F*----------------------------------------------------------------H
000500120903      *? PER IL FILE WSLSS60F:
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
002700110711     D ksc_EX          S             10    DIM(500)
002800120903     D* SCHIERA TABELLA FASCIE DI PESO (2L)
002900120903     D FPR             S              2    DIM(20)
003000120903     D KIR             S              6  0 DIM(20)
003100120903     D KFR             S              6  0 DIM(20)
003200091006     d
003300970527     D KPJBA         E DS
003400090204     D  §DAL                 247    254  0
003500090204     D  §AL                  255    262  0
003600090601     D  §opz                 263    263
003700090601     D  §tipo                264    264
003800110711     D  §stat                265    265
003900090204
004000090204     D og143         E DS
004100090204     D og148         E DS
004200090204
004300120903     D ds2l          E DS
004400970529     D DSTB          E DS
004500090204     D DS05          E DS
004600090205     D Dar5trs       E DS
004700091006     D dcxb          E DS
004800110711     d
004900110711     d* DS x controllo riempimento schiere
005000110711     d trul0sds      e ds
005100090204
005200980826     D DSFIL           DS                  OCCURS(400)
005300970527     D  WTSPIP                 1      8  0
005400970527     D  WTSPIA                 9     16  0
005500970527     D  WTSPEI                17     24  0
005600970527     D  WTSPEE                25     32  0
005700970527     D  WTCLIP                33     40  0
005800970527     D  WTCLIA                41     48  0
005900970527     D  WTCLEI                49     56  0
006000970527     D  WTCLEE                57     64  0
006100980826     D  WTKGIP                65     75  1
006200980826     D  WTKGIA                76     86  1
006300980826     D  WTKGEI                87     97  1
006400980826     D  WTKGEE                98    108  1
006500980826     D  WTMCIP               109    119  3
006600980826     D  WTMCIA               120    130  3
006700980826     D  WTMCEI               131    140  3
006800980826     D  WTMCEE               141    150  3
006900000223     D  WTSPEF               151    158  0
007000000223     D  WTCLEF               159    166  0
007100000223     D  WTKGEF               167    177  1
007200000223     D  WTMCEF               178    188  3
007300000607     D  WTSPEA               189    196  0
007400000607     D  WTCLEA               197    204  0
007500000607     D  WTKGEA               205    215  1
007600000607     D  WTMCEA               216    226  3
007700000607     D  WTSPED               227    234  0
007800000607     D  WTCLED               235    242  0
007900000607     D  WTKGED               243    253  1
008000000630     D  WTMCED               254    264  3
008100000630     D  WTSPPA               265    272  0
008200000630     D  WTCLPA               273    280  0
008300000630     D  WTKGPA               281    291  1
008400000630     D  WTMCPA               292    302  3
008500020321     D  WTSPEX               303    310  0
008600020321     D  WTCLEX               311    318  0
008700020321     D  WTKGEX               319    329  1
008800020321     D  WTMCEX               330    340  3
008900090206     D  WTSPIP_R                           like(WTSPIP)
009000090206     D  WTSPIA_r                           like(wtspia)
009100090206     D  WTSPEI_r                           like(wtspei)
009200090206     D  WTSPEE_r                           like(wtspee)
009300090206     D  WTCLIP_r                           like(wtclip)
009400090206     D  WTCLIA_r                           like(wtclia)
009500090206     D  WTCLEI_r                           like(wtclei)
009600090206     D  WTCLEE_r                           like(wtclee)
009700090206     D  WTKGIP_r                           like(wtkgip)
009800090206     D  WTKGIA_r                           like(wtkgia)
009900090206     D  WTKGEI_r                           like(wtkgei)
010000090206     D  WTKGEE_r                           like(wtkgee)
010100090206     D  WTMCIP_r                           like(wtmcip)
010200090206     D  WTMCIA_r                           like(wtmcia)
010300090206     D  WTMCEI_r                           like(wtmcei)
010400090206     D  WTMCEE_r                           like(wtmcee)
010500090206     D  WTSPEF_r                           like(wtspef)
010600090206     D  WTCLEF_r                           like(wtclef)
010700090206     D  WTKGEF_r                           like(wtkgef)
010800090206     D  WTMCEF_r                           like(wtmcef)
010900090206     D  WTSPEA_r                           like(wtspea)
011000090206     D  WTCLEA_r                           like(wtclea)
011100090206     D  WTKGEA_r                           like(wtkgea)
011200090206     D  WTMCEA_r                           like(wtmcea)
011300090206     D  WTSPED_r                           like(wtsped)
011400090206     D  WTCLED_r                           like(wtcled)
011500090206     D  WTKGED_r                           like(wtkged)
011600090206     D  WTMCED_r                           like(wtmced)
011700090206     D  WTSPPA_r                           like(wtsppa)
011800090206     D  WTCLPA_r                           like(wtclpa)
011900090206     D  WTKGPA_r                           like(wtkgpa)
012000090206     D  WTMCPA_r                           like(wtmcpa)
012100090206     D  WTSPEX_r                           like(wtspex)
012200090206     D  WTCLEX_r                           like(wtclex)
012300090206     D  WTKGEX_r                           like(wtkgex)
012400090206     D  WTMCEX_r                           like(wtmcex)
012500091006     d* Giornaliero extra budget
012600091006     D  WTSPIP_E                           like(WTSPIP)
012700091006     D  WTSPIA_e                           like(wtspia)
012800091006     D  WTSPEI_e                           like(wtspei)
012900091006     D  WTSPEE_e                           like(wtspee)
013000091006     D  WTCLIP_e                           like(wtclip)
013100091006     D  WTCLIA_e                           like(wtclia)
013200091006     D  WTCLEI_e                           like(wtclei)
013300091006     D  WTCLEE_e                           like(wtclee)
013400091006     D  WTKGIP_e                           like(wtkgip)
013500091006     D  WTKGIA_e                           like(wtkgia)
013600091006     D  WTKGEI_e                           like(wtkgei)
013700091006     D  WTKGEE_e                           like(wtkgee)
013800091006     D  WTMCIP_e                           like(wtmcip)
013900091006     D  WTMCIA_e                           like(wtmcia)
014000091006     D  WTMCEI_e                           like(wtmcei)
014100091006     D  WTMCEE_e                           like(wtmcee)
014200091006     D  WTSPEF_e                           like(wtspef)
014300091006     D  WTCLEF_e                           like(wtclef)
014400091006     D  WTKGEF_e                           like(wtkgef)
014500091006     D  WTMCEF_e                           like(wtmcef)
014600091006     D  WTSPEA_e                           like(wtspea)
014700091006     D  WTCLEA_e                           like(wtclea)
014800091006     D  WTKGEA_e                           like(wtkgea)
014900091006     D  WTMCEA_e                           like(wtmcea)
015000091006     D  WTSPED_e                           like(wtsped)
015100091006     D  WTCLED_e                           like(wtcled)
015200091006     D  WTKGED_e                           like(wtkged)
015300091006     D  WTMCED_e                           like(wtmced)
015400091006     D  WTSPPA_e                           like(wtsppa)
015500091006     D  WTCLPA_e                           like(wtclpa)
015600091006     D  WTKGPA_e                           like(wtkgpa)
015700091006     D  WTMCPA_e                           like(wtmcpa)
015800091006     D  WTSPEX_e                           like(wtspex)
015900091006     D  WTCLEX_e                           like(wtclex)
016000091006     D  WTKGEX_e                           like(wtkgex)
016100091006     D  WTMCEX_e                           like(wtmcex)
016200091006     d* Riepilogativo extra budget
016300091006     D  WTSPIP_re                          like(WTSPIP)
016400091006     D  WTSPIA_re                          like(wtspia)
016500091006     D  WTSPEI_re                          like(wtspei)
016600091006     D  WTSPEE_re                          like(wtspee)
016700091006     D  WTCLIP_re                          like(wtclip)
016800091006     D  WTCLIA_re                          like(wtclia)
016900091006     D  WTCLEI_re                          like(wtclei)
017000091006     D  WTCLEE_re                          like(wtclee)
017100091006     D  WTKGIP_re                          like(wtkgip)
017200091006     D  WTKGIA_re                          like(wtkgia)
017300091006     D  WTKGEI_re                          like(wtkgei)
017400091006     D  WTKGEE_re                          like(wtkgee)
017500091006     D  WTMCIP_re                          like(wtmcip)
017600091006     D  WTMCIA_re                          like(wtmcia)
017700091006     D  WTMCEI_re                          like(wtmcei)
017800091006     D  WTMCEE_re                          like(wtmcee)
017900091006     D  WTSPEF_re                          like(wtspef)
018000091006     D  WTCLEF_re                          like(wtclef)
018100091006     D  WTKGEF_re                          like(wtkgef)
018200091006     D  WTMCEF_re                          like(wtmcef)
018300091006     D  WTSPEA_re                          like(wtspea)
018400091006     D  WTCLEA_re                          like(wtclea)
018500091006     D  WTKGEA_re                          like(wtkgea)
018600091006     D  WTMCEA_re                          like(wtmcea)
018700091006     D  WTSPED_re                          like(wtsped)
018800091006     D  WTCLED_re                          like(wtcled)
018900091006     D  WTKGED_re                          like(wtkged)
019000091006     D  WTMCED_re                          like(wtmced)
019100091006     D  WTSPPA_re                          like(wtsppa)
019200091006     D  WTCLPA_re                          like(wtclpa)
019300091006     D  WTKGPA_re                          like(wtkgpa)
019400091006     D  WTMCPA_re                          like(wtmcpa)
019500091006     D  WTSPEX_re                          like(wtspex)
019600091006     D  WTCLEX_re                          like(wtclex)
019700091006     D  WTKGEX_re                          like(wtkgex)
019800091006     D  WTMCEX_re                          like(wtmcex)
019900100402     d
020000100402     D DSFIL_e         DS                  OCCURS(400)
020100100402     D  eWTSPIP                1      8  0
020200100402     D  eWTSPIA                9     16  0
020300100402     D  eWTSPEI               17     24  0
020400100402     D  eWTSPEE               25     32  0
020500100402     D  eWTCLIP               33     40  0
020600100402     D  eWTCLIA               41     48  0
020700100402     D  eWTCLEI               49     56  0
020800100402     D  eWTCLEE               57     64  0
020900100402     D  eWTKGIP               65     75  1
021000100402     D  eWTKGIA               76     86  1
021100100402     D  eWTKGEI               87     97  1
021200100402     D  eWTKGEE               98    108  1
021300100402     D  eWTMCIP              109    119  3
021400100402     D  eWTMCIA              120    130  3
021500100402     D  eWTMCEI              131    140  3
021600100402     D  eWTMCEE              141    150  3
021700100402     D  eWTSPEF              151    158  0
021800100402     D  eWTCLEF              159    166  0
021900100402     D  eWTKGEF              167    177  1
022000100402     D  eWTMCEF              178    188  3
022100100402     D  eWTSPEA              189    196  0
022200100402     D  eWTCLEA              197    204  0
022300100402     D  eWTKGEA              205    215  1
022400100402     D  eWTMCEA              216    226  3
022500100402     D  eWTSPED              227    234  0
022600100402     D  eWTCLED              235    242  0
022700100402     D  eWTKGED              243    253  1
022800100402     D  eWTMCED              254    264  3
022900100402     D  eWTSPPA              265    272  0
023000100402     D  eWTCLPA              273    280  0
023100100402     D  eWTKGPA              281    291  1
023200100402     D  eWTMCPA              292    302  3
023300100402     D  eWTSPEX              303    310  0
023400100402     D  eWTCLEX              311    318  0
023500100402     D  eWTKGEX              319    329  1
023600100402     D  eWTMCEX              330    340  3
023700100402     D  eWTSPIP_R                          like(WTSPIP)
023800100402     D  eWTSPIA_r                          like(wtspia)
023900100402     D  eWTSPEI_r                          like(wtspei)
024000100402     D  eWTSPEE_r                          like(wtspee)
024100100402     D  eWTCLIP_r                          like(wtclip)
024200100402     D  eWTCLIA_r                          like(wtclia)
024300100402     D  eWTCLEI_r                          like(wtclei)
024400100402     D  eWTCLEE_r                          like(wtclee)
024500100402     D  eWTKGIP_r                          like(wtkgip)
024600100402     D  eWTKGIA_r                          like(wtkgia)
024700100402     D  eWTKGEI_r                          like(wtkgei)
024800100402     D  eWTKGEE_r                          like(wtkgee)
024900100402     D  eWTMCIP_r                          like(wtmcip)
025000100402     D  eWTMCIA_r                          like(wtmcia)
025100100402     D  eWTMCEI_r                          like(wtmcei)
025200100402     D  eWTMCEE_r                          like(wtmcee)
025300100402     D  eWTSPEF_r                          like(wtspef)
025400100402     D  eWTCLEF_r                          like(wtclef)
025500100402     D  eWTKGEF_r                          like(wtkgef)
025600100402     D  eWTMCEF_r                          like(wtmcef)
025700100402     D  eWTSPEA_r                          like(wtspea)
025800100402     D  eWTCLEA_r                          like(wtclea)
025900100402     D  eWTKGEA_r                          like(wtkgea)
026000100402     D  eWTMCEA_r                          like(wtmcea)
026100100402     D  eWTSPED_r                          like(wtsped)
026200100402     D  eWTCLED_r                          like(wtcled)
026300100402     D  eWTKGED_r                          like(wtkged)
026400100402     D  eWTMCED_r                          like(wtmced)
026500100402     D  eWTSPPA_r                          like(wtsppa)
026600100402     D  eWTCLPA_r                          like(wtclpa)
026700100402     D  eWTKGPA_r                          like(wtkgpa)
026800100402     D  eWTMCPA_r                          like(wtmcpa)
026900100402     D  eWTSPEX_r                          like(wtspex)
027000100402     D  eWTCLEX_r                          like(wtclex)
027100100402     D  eWTKGEX_r                          like(wtkgex)
027200100402     D  eWTMCEX_r                          like(wtmcex)
027300100402     d* Giornaliero extra budget
027400100402     D  eWTSPIP_E                          like(WTSPIP)
027500100402     D  eWTSPIA_e                          like(wtspia)
027600100402     D  eWTSPEI_e                          like(wtspei)
027700100402     D  eWTSPEE_e                          like(wtspee)
027800100402     D  eWTCLIP_e                          like(wtclip)
027900100402     D  eWTCLIA_e                          like(wtclia)
028000100402     D  eWTCLEI_e                          like(wtclei)
028100100402     D  eWTCLEE_e                          like(wtclee)
028200100402     D  eWTKGIP_e                          like(wtkgip)
028300100402     D  eWTKGIA_e                          like(wtkgia)
028400100402     D  eWTKGEI_e                          like(wtkgei)
028500100402     D  eWTKGEE_e                          like(wtkgee)
028600100402     D  eWTMCIP_e                          like(wtmcip)
028700100402     D  eWTMCIA_e                          like(wtmcia)
028800100402     D  eWTMCEI_e                          like(wtmcei)
028900100402     D  eWTMCEE_e                          like(wtmcee)
029000100402     D  eWTSPEF_e                          like(wtspef)
029100100402     D  eWTCLEF_e                          like(wtclef)
029200100402     D  eWTKGEF_e                          like(wtkgef)
029300100402     D  eWTMCEF_e                          like(wtmcef)
029400100402     D  eWTSPEA_e                          like(wtspea)
029500100402     D  eWTCLEA_e                          like(wtclea)
029600100402     D  eWTKGEA_e                          like(wtkgea)
029700100402     D  eWTMCEA_e                          like(wtmcea)
029800100402     D  eWTSPED_e                          like(wtsped)
029900100402     D  eWTCLED_e                          like(wtcled)
030000100402     D  eWTKGED_e                          like(wtkged)
030100100402     D  eWTMCED_e                          like(wtmced)
030200100402     D  eWTSPPA_e                          like(wtsppa)
030300100402     D  eWTCLPA_e                          like(wtclpa)
030400100402     D  eWTKGPA_e                          like(wtkgpa)
030500100402     D  eWTMCPA_e                          like(wtmcpa)
030600100402     D  eWTSPEX_e                          like(wtspex)
030700100402     D  eWTCLEX_e                          like(wtclex)
030800100402     D  eWTKGEX_e                          like(wtkgex)
030900100402     D  eWTMCEX_e                          like(wtmcex)
031000100402     d* Riepilogativo extra budget
031100100402     D  eWTSPIP_re                         like(WTSPIP)
031200100402     D  eWTSPIA_re                         like(wtspia)
031300100402     D  eWTSPEI_re                         like(wtspei)
031400100402     D  eWTSPEE_re                         like(wtspee)
031500100402     D  eWTCLIP_re                         like(wtclip)
031600100402     D  eWTCLIA_re                         like(wtclia)
031700100402     D  eWTCLEI_re                         like(wtclei)
031800100402     D  eWTCLEE_re                         like(wtclee)
031900100402     D  eWTKGIP_re                         like(wtkgip)
032000100402     D  eWTKGIA_re                         like(wtkgia)
032100100402     D  eWTKGEI_re                         like(wtkgei)
032200100402     D  eWTKGEE_re                         like(wtkgee)
032300100402     D  eWTMCIP_re                         like(wtmcip)
032400100402     D  eWTMCIA_re                         like(wtmcia)
032500100402     D  eWTMCEI_re                         like(wtmcei)
032600100402     D  eWTMCEE_re                         like(wtmcee)
032700100402     D  eWTSPEF_re                         like(wtspef)
032800100402     D  eWTCLEF_re                         like(wtclef)
032900100402     D  eWTKGEF_re                         like(wtkgef)
033000100402     D  eWTMCEF_re                         like(wtmcef)
033100100402     D  eWTSPEA_re                         like(wtspea)
033200100402     D  eWTCLEA_re                         like(wtclea)
033300100402     D  eWTKGEA_re                         like(wtkgea)
033400100402     D  eWTMCEA_re                         like(wtmcea)
033500100402     D  eWTSPED_re                         like(wtsped)
033600100402     D  eWTCLED_re                         like(wtcled)
033700100402     D  eWTKGED_re                         like(wtkged)
033800100402     D  eWTMCED_re                         like(wtmced)
033900100402     D  eWTSPPA_re                         like(wtsppa)
034000100402     D  eWTCLPA_re                         like(wtclpa)
034100100402     D  eWTKGPA_re                         like(wtkgpa)
034200100402     D  eWTMCPA_re                         like(wtmcpa)
034300100402     D  eWTSPEX_re                         like(wtspex)
034400100402     D  eWTCLEX_re                         like(wtclex)
034500100402     D  eWTKGEX_re                         like(wtkgex)
034600100402     D  eWTMCEX_re                         like(wtmcex)
034700100402     d
034800100402     D DSFIL_h         DS                  OCCURS(400)
034900100402     D  hWTSPIP                1      8  0
035000100402     D  hWTSPIA                9     16  0
035100100402     D  hWTSPEI               17     24  0
035200100402     D  hWTSPEE               25     32  0
035300100402     D  hWTCLIP               33     40  0
035400100402     D  hWTCLIA               41     48  0
035500100402     D  hWTCLEI               49     56  0
035600100402     D  hWTCLEE               57     64  0
035700100402     D  hWTKGIP               65     75  1
035800100402     D  hWTKGIA               76     86  1
035900100402     D  hWTKGEI               87     97  1
036000100402     D  hWTKGEE               98    108  1
036100100402     D  hWTMCIP              109    119  3
036200100402     D  hWTMCIA              120    130  3
036300100402     D  hWTMCEI              131    140  3
036400100402     D  hWTMCEE              141    150  3
036500100402     D  hWTSPEF              151    158  0
036600100402     D  hWTCLEF              159    166  0
036700100402     D  hWTKGEF              167    177  1
036800100402     D  hWTMCEF              178    188  3
036900100402     D  hWTSPEA              189    196  0
037000100402     D  hWTCLEA              197    204  0
037100100402     D  hWTKGEA              205    215  1
037200100402     D  hWTMCEA              216    226  3
037300100402     D  hWTSPED              227    234  0
037400100402     D  hWTCLED              235    242  0
037500100402     D  hWTKGED              243    253  1
037600100402     D  hWTMCED              254    264  3
037700100402     D  hWTSPPA              265    272  0
037800100402     D  hWTCLPA              273    280  0
037900100402     D  hWTKGPA              281    291  1
038000100402     D  hWTMCPA              292    302  3
038100100402     D  hWTSPEX              303    310  0
038200100402     D  hWTCLEX              311    318  0
038300100402     D  hWTKGEX              319    329  1
038400100402     D  hWTMCEX              330    340  3
038500100402     D  hWTSPIP_R                          like(WTSPIP)
038600100402     D  hWTSPIA_r                          like(wtspia)
038700100402     D  hWTSPEI_r                          like(wtspei)
038800100402     D  hWTSPEE_r                          like(wtspee)
038900100402     D  hWTCLIP_r                          like(wtclip)
039000100402     D  hWTCLIA_r                          like(wtclia)
039100100402     D  hWTCLEI_r                          like(wtclei)
039200100402     D  hWTCLEE_r                          like(wtclee)
039300100402     D  hWTKGIP_r                          like(wtkgip)
039400100402     D  hWTKGIA_r                          like(wtkgia)
039500100402     D  hWTKGEI_r                          like(wtkgei)
039600100402     D  hWTKGEE_r                          like(wtkgee)
039700100402     D  hWTMCIP_r                          like(wtmcip)
039800100402     D  hWTMCIA_r                          like(wtmcia)
039900100402     D  hWTMCEI_r                          like(wtmcei)
040000100402     D  hWTMCEE_r                          like(wtmcee)
040100100402     D  hWTSPEF_r                          like(wtspef)
040200100402     D  hWTCLEF_r                          like(wtclef)
040300100402     D  hWTKGEF_r                          like(wtkgef)
040400100402     D  hWTMCEF_r                          like(wtmcef)
040500100402     D  hWTSPEA_r                          like(wtspea)
040600100402     D  hWTCLEA_r                          like(wtclea)
040700100402     D  hWTKGEA_r                          like(wtkgea)
040800100402     D  hWTMCEA_r                          like(wtmcea)
040900100402     D  hWTSPED_r                          like(wtsped)
041000100402     D  hWTCLED_r                          like(wtcled)
041100100402     D  hWTKGED_r                          like(wtkged)
041200100402     D  hWTMCED_r                          like(wtmced)
041300100402     D  hWTSPPA_r                          like(wtsppa)
041400100402     D  hWTCLPA_r                          like(wtclpa)
041500100402     D  hWTKGPA_r                          like(wtkgpa)
041600100402     D  hWTMCPA_r                          like(wtmcpa)
041700100402     D  hWTSPEX_r                          like(wtspex)
041800100402     D  hWTCLEX_r                          like(wtclex)
041900100402     D  hWTKGEX_r                          like(wtkgex)
042000100402     D  hWTMCEX_r                          like(wtmcex)
042100100402     d* Giornaliero extra budget
042200100402     D  hWTSPIP_E                          like(WTSPIP)
042300100402     D  hWTSPIA_e                          like(wtspia)
042400100402     D  hWTSPEI_e                          like(wtspei)
042500100402     D  hWTSPEE_e                          like(wtspee)
042600100402     D  hWTCLIP_e                          like(wtclip)
042700100402     D  hWTCLIA_e                          like(wtclia)
042800100402     D  hWTCLEI_e                          like(wtclei)
042900100402     D  hWTCLEE_e                          like(wtclee)
043000100402     D  hWTKGIP_e                          like(wtkgip)
043100100402     D  hWTKGIA_e                          like(wtkgia)
043200100402     D  hWTKGEI_e                          like(wtkgei)
043300100402     D  hWTKGEE_e                          like(wtkgee)
043400100402     D  hWTMCIP_e                          like(wtmcip)
043500100402     D  hWTMCIA_e                          like(wtmcia)
043600100402     D  hWTMCEI_e                          like(wtmcei)
043700100402     D  hWTMCEE_e                          like(wtmcee)
043800100402     D  hWTSPEF_e                          like(wtspef)
043900100402     D  hWTCLEF_e                          like(wtclef)
044000100402     D  hWTKGEF_e                          like(wtkgef)
044100100402     D  hWTMCEF_e                          like(wtmcef)
044200100402     D  hWTSPEA_e                          like(wtspea)
044300100402     D  hWTCLEA_e                          like(wtclea)
044400100402     D  hWTKGEA_e                          like(wtkgea)
044500100402     D  hWTMCEA_e                          like(wtmcea)
044600100402     D  hWTSPED_e                          like(wtsped)
044700100402     D  hWTCLED_e                          like(wtcled)
044800100402     D  hWTKGED_e                          like(wtkged)
044900100402     D  hWTMCED_e                          like(wtmced)
045000100402     D  hWTSPPA_e                          like(wtsppa)
045100100402     D  hWTCLPA_e                          like(wtclpa)
045200100402     D  hWTKGPA_e                          like(wtkgpa)
045300100402     D  hWTMCPA_e                          like(wtmcpa)
045400100402     D  hWTSPEX_e                          like(wtspex)
045500100402     D  hWTCLEX_e                          like(wtclex)
045600100402     D  hWTKGEX_e                          like(wtkgex)
045700100402     D  hWTMCEX_e                          like(wtmcex)
045800100402     d* Riepilogativo extra budget
045900100402     D  hWTSPIP_re                         like(WTSPIP)
046000100402     D  hWTSPIA_re                         like(wtspia)
046100100402     D  hWTSPEI_re                         like(wtspei)
046200100402     D  hWTSPEE_re                         like(wtspee)
046300100402     D  hWTCLIP_re                         like(wtclip)
046400100402     D  hWTCLIA_re                         like(wtclia)
046500100402     D  hWTCLEI_re                         like(wtclei)
046600100402     D  hWTCLEE_re                         like(wtclee)
046700100402     D  hWTKGIP_re                         like(wtkgip)
046800100402     D  hWTKGIA_re                         like(wtkgia)
046900100402     D  hWTKGEI_re                         like(wtkgei)
047000100402     D  hWTKGEE_re                         like(wtkgee)
047100100402     D  hWTMCIP_re                         like(wtmcip)
047200100402     D  hWTMCIA_re                         like(wtmcia)
047300100402     D  hWTMCEI_re                         like(wtmcei)
047400100402     D  hWTMCEE_re                         like(wtmcee)
047500100402     D  hWTSPEF_re                         like(wtspef)
047600100402     D  hWTCLEF_re                         like(wtclef)
047700100402     D  hWTKGEF_re                         like(wtkgef)
047800100402     D  hWTMCEF_re                         like(wtmcef)
047900100402     D  hWTSPEA_re                         like(wtspea)
048000100402     D  hWTCLEA_re                         like(wtclea)
048100100402     D  hWTKGEA_re                         like(wtkgea)
048200100402     D  hWTMCEA_re                         like(wtmcea)
048300100402     D  hWTSPED_re                         like(wtsped)
048400100402     D  hWTCLED_re                         like(wtcled)
048500100402     D  hWTKGED_re                         like(wtkged)
048600100402     D  hWTMCED_re                         like(wtmced)
048700100402     D  hWTSPPA_re                         like(wtsppa)
048800100402     D  hWTCLPA_re                         like(wtclpa)
048900100402     D  hWTKGPA_re                         like(wtkgpa)
049000100402     D  hWTMCPA_re                         like(wtmcpa)
049100100402     D  hWTSPEX_re                         like(wtspex)
049200100402     D  hWTCLEX_re                         like(wtclex)
049300100402     D  hWTKGEX_re                         like(wtkgex)
049400100402     D  hWTMCEX_re                         like(wtmcex)
049500100402     d
049600090206     d* dati giornaliero
049700091006     D DSFIL_smp       DS                  OCCURS(400)
049800090206     D  Wmptsp_g                           like(wmptsp)
049900090206     D  wmptcp_g                           like(wmptcp)
050000090206     D  wmptkp_g                     11  1
050100090206     D  wmptmp_g                     10  3
050200090710
050300090206     D  Wmptsa_g                           like(wmptsa)
050400090206     D  wmptca_g                           like(wmptca)
050500090206     D  wmptka_g                     11  1
050600090206     D  wmptma_g                     10  3
050700090710
050800090206     D  Wmptspt_g                          like(wmptspt)
050900090206     D  wmptcpt_g                          like(wmptcpt)
051000090206     D  wmptkpt_g                    11  1
051100090206     D  wmptmpt_g                    10  3
051200090710
051300090710     D  Wmptsptc_g                         like(wmptspt)
051400090710     D  wmptcptc_g                         like(wmptcpt)
051500090710     D  wmptkptc_g                   11  1
051600090710     D  wmptmptc_g                   10  3
051700090710
051800090206     D  Wmptsat_g                          like(wmptsat)
051900090206     D  wmptcat_g                          like(wmptcat)
052000090206     D  wmptkat_g                    11  1
052100090206     D  wmptmat_g                    10  3
052200090710     d
052300090710     D  Wmptsatc_g                         like(wmptsat)
052400090710     D  wmptcatc_g                         like(wmptcat)
052500090710     D  wmptkatc_g                   11  1
052600090710     D  wmptmatc_g                   10  3
052700090710
052800090206     D  Wmptst_g                           like(wmptst)
052900090206     D  wmptct_g                           like(wmptct)
053000090206     D  wmptkt_g                     11  1
053100090206     D  wmptmt_g                     10  3
053200090710
053300090206     d* dati riepilogo
053400090206     D  Wmptsp_r                           like(wmptsp)
053500090206     D  wmptcp_r                           like(wmptcp)
053600090206     D  wmptkp_r                     11  1
053700090206     D  wmptmp_r                     10  3
053800090710
053900090206     D  Wmptsa_r                           like(wmptsa)
054000090206     D  wmptca_r                           like(wmptca)
054100090206     D  wmptka_r                     11  1
054200090206     D  wmptma_r                     10  3
054300090710
054400090206     D  Wmptspt_r                          like(wmptspt)
054500090206     D  wmptcpt_r                          like(wmptcpt)
054600090206     D  wmptkpt_r                    11  1
054700090206     D  wmptmpt_r                    10  3
054800090710
054900090710     D  Wmptsptc_r                         like(wmptspt)
055000090710     D  wmptcptc_r                         like(wmptcpt)
055100090710     D  wmptkptc_r                   11  1
055200090710     D  wmptmptc_r                   10  3
055300090710
055400090206     D  Wmptsat_r                          like(wmptsat)
055500090206     D  wmptcat_r                          like(wmptcat)
055600090206     D  wmptkat_r                    11  1
055700090206     D  wmptmat_r                    10  3
055800090710
055900090710     D  Wmptsatc_r                         like(wmptsat)
056000090710     D  wmptcatc_r                         like(wmptcat)
056100090710     D  wmptkatc_r                   11  1
056200090710     D  wmptmatc_r                   10  3
056300090710
056400090206     D  Wmptst_r                           like(wmptst)
056500090206     D  wmptct_r                           like(wmptct)
056600090206     D  wmptkt_r                     11  1
056700090206     D  wmptmt_r                     10  3
056800090204
056900120903     D II              S              3  0
057000120903     D kk              S              3  0
057100111129     D EE              S              3  0
057200091006     D Wdatael         S              8  0
057300091007     D w010a           S             10
057400110801     D PTIPOCLI        S             12
057500110801     D skicli          S            132
057600090204     d dataiso         s               d   datfmt(*iso)
057700090204     d waas            s                   like(tasaas)
057800090204     d wmgs            s                   Like(tasmgs)
057900090224     d w_peso          s                   Like(taspkf)
058000090204     d k_fil           s                   Like(wmpfil)
058100090204     d k_tip           s                   Like(wmptip)
058200090204     d k_dat           s                   Like(wmpdal)
058300090205     d k_trd           s                   Like(ar5trd) inz('TRS')
058400091006     d savccm          s                   Like(tasccm)
058500091006     d savctr          s                   Like(tasctr)
058600110715     d savkpjbu        s                   Like(kpjbu)
058700120903     d ks_tie          s                   Like(wsstie)
058800120903     d ks_tip          s                   Like(wsstip)
058900120903     d ks_lin          s                   Like(wsslin)
059000120903     d ks_sca          s                   Like(wsskgf)
059100111028     d
059200111028     d titas00ds     E DS                  extname(titas00f)
059300090601     d
059400111028     d wrkgetlista     s          25767    varying
059500120904     d*
059600120904      * ---------------------------------------------------------
059700120904     i*
059800120904     iwslss500
059900120904     I              wsstkg                      wsstkg_ol
060000120904     I              wsstmc                      wsstmc_ol
060100120904     I              wsstkge                     wsstkge_ol
060200120904     I              wsstmce                     wsstmce_ol
060300120904     I              wsstkgh                     wsstkgh_ol
060400120904     I              wsstmch                     wsstmch_ol
060500120904     i*
060600120904     iwslss550
060700120904     I              wsstkg                      wsstkg_ex
060800120904     I              wsstmc                      wsstmc_ex
060900120904     I              wsstkge                     wsstkge_ex
061000120904     I              wsstmce                     wsstmce_ex
061100120904     I              wsstkgh                     wsstkgh_ex
061200120904     I              wsstmch                     wsstmch_ex
061300120904      * ---------------------------------------------------------
061400970527     C* RICEVIMENTO PARAMETRI
061500970527     C     *ENTRY        PLIST
061600970527     C                   PARM                    KPJBA
061700970527     C                   MOVEL     §DAL          WAAS
061800970527     C                   MOVE      §DAL          WMGS
061900090601     C                   MOVE      §tipo         Wtipo             1
062000970528     C*
062100970529     C                   EXSR      CARINZ
062200951220     C*
062300990811     C* LETTURA TITAS
062400111028     c                   if        §stat=' '
062500990811     C     KTAS          SETLL     TITAS38C
062600111028     c                   else
062700111028
062800111028     C/EXEC SQL
062900111028     C+ PREPARE s1 FROM :wrkgetlista
063000111028     C/END-EXEC
063100111028     C
063200111028     C/EXEC SQL
063300111028     C+ DECLARE A1 CURSOR FOR S1
063400111028     C/END-EXEC
063500111028     C
063600111028     C/EXEC SQL
063700111028     C+ OPEN a1
063800111028     C/END-EXEC
063900111028
064000111028     c                   endif
064100111028     c
064200970527     C                   DO        *HIVAL
064300111028     c
064400111028     c                   if        §stat=' '
064500090204     C                   READ      TITAS38C
064600090204     c* Termina il ciclo se fine file o per data spedizione > data al
064700090204     C                   if        %eof(titas38c) or ((tasaas*10000+tasmgs)>§al)
064800090204     c                   leave
064900090204     c                   endif
065000111028     c
065100111028     c                   else
065200111028     C/EXEC SQL
065300111028     C+ FETCH NEXT FROM a1 INTO :titas00ds
065400111028     C/END-EXEC
065500111028
065600111028     c                   select
065700111028
065800111028     c                   when      sqlcod = 100
065900111028     C/EXEC SQL
066000111103     C+ CLOSE a1
066100111028     C/END-EXEC
066200111028     c                   leave
066300111028     c                   when      sqlcod < 0
066400111028     c                   seton                                        H1
066500111028     C/EXEC SQL
066600111103     C+ CLOSE a1
066700111028     C/END-EXEC
066800111028     c                   leave
066900111028     c                   endsl
067000111028     c
067100111028     c                   endif
067200111028     c
067300090204     c* Scarto in base al tipo bolla
067400090204     c                   if        %lookup(tastbl:tbo)=0
067500090204     c                   iter
067600090204     c                   endif
067700970527     C*
067800090601     c
067900090601     c* EDPES : 01/06/09
068000090601     c* Per ora riutilizzo solo il peso bollettato/variato e non VDL
068100090901     c***                z-add     taspkf        w_peso
068200090901     c* EDPES:  01/09/09
068300090901     c**  riutilizzo il peso VDL
068400090901     c* imposto il peso che sarà da memorizzare secondo il seguente ordine:
068500090901     c* 1. VDL totale
068600090901     c* 2. VDL parziale se > di TASPKF
068700090901     c* 3. TASPKF
068800090901     c                   clear                   w_peso
068900090901     c                   if        tasncp=tasncl or (tasncp>0 and taspkc>taspkf)
069000090901     c                   z-add     taspkc        w_peso
069100090901     c                   else
069200090901     c                   z-add     taspkf        w_peso
069300090901     c                   endif
069400090601     c
069500110711     c                   if        §stat=' '
069600090204     c                   exsr      sr_wfsmp
069700110711     c                   endif
069800110711     c
069900090204     c                   exsr      sr_wslss
070000090204     C                   ENDDO
070100090206     c*
070200090206     C* SCARICO DS SU FILE
070300090206     c                   exsr      scrive_file
070400110801     c                   if        §stat=' '
070500110801     c                   eval      ptipocli='EXTRA BUDGET'
070600110801     c                   else
070700110801     c                   eval      ptipocli='PARTICOLARI '
070800110801     c                   endif
070900110801     c
071000090206     c                   except    testa
071100110801     C*
071200110801     c                   z-add     1             k
071300110801     c                   clear                   conta             3 0
071400110801     c                   clear                   skicli
071500110801     c                   dow       ksc_ex(k)<>*blanks
071600110801     c
071700110801     c                   if        §stat=' '
071800110801
071900110801     c                   eval      skicli=%trim(skicli)+' '+
072000110801     c                             %subst(ksc_ex(k):1:7)
072100110801     c                   add       7             conta
072200110801     c*
072300110801     c                   if        %subst(ksc_ex(k):8:3)<>'   '
072400110801     c                   eval      skicli=%trim(skicli)+'/'+
072500110801     c                             %subst(ksc_ex(k):8:3)
072600110801     c                   add       4             conta
072700110801     c                   endif
072800110801     c                   add       1             conta
072900110801     c
073000110801     c
073100110801     c                   if        conta>120
073200110801     c                   except    detksc
073300110801     c                   clear                   conta
073400110801     c                   clear                   skicli
073500110801     c                   endif
073600110801     c
073700110801     c                   else
073800110801     c
073900110801     c                   eval      skicli=%trim(skicli)+' '+
074000110801     c                             %subst(ksc_ex(k):1:7)
074100110801     c                   add       1             conta
074200110801     c
074300110801     c                   if        conta=16
074400110801     c                   except    detksc
074500110801     c                   clear                   conta
074600110801     c                   clear                   skicli
074700110801     c                   endif
074800110801     c
074900110801     c                   endif
075000110801     c
075100110801     c                   add       1             k
075200110801     c                   enddo
075300110801     c
075400110801     c                   if        conta>0
075500110801     c                   except    detksc
075600110801     c                   endif
075700970528     C*
075800970528     C                   SETON                                        LR
075900970519     C*
076000090204     C**********************************************************
076100090204     C** Elaborazione per file wfsmp00f
076200090204     C**********************************************************
076300090204     C     sr_wfsmp      BEGSR
076400090204     c* linea di partenza
076500090206     c                   z-add     1             k
076600090206     c     taslnp        lookup    fil(k)                                 58
076700090206     c                   if        *in58
076800090206     c     k             occur     dsfil_smp
076900090205     c* . . . giornaliero
077000090206     c                   if        (tasaas*10000+tasmgs)=§al
077100090206     C                   add       1             Wmptsp_g
077200090206     C                   add       TASNCL        wmptcp_g
077300090224     C                   add       w_peso        Wmptkp_g
077400090206     C                   add       TASVLF        Wmptmp_g
077500090206     c                   endif
077600090204     c* . . . riepilogo
077700090206     C                   add       1             Wmptsp_r
077800090206     C                   add       TASNCL        wmptcp_r
077900090224     C                   add       w_peso        Wmptkp_r
078000090206     C                   add       TASVLF        Wmptmp_r
078100090206     c                   endif
078200090204     c* linea di arrivo
078300090206     c                   z-add     1             k
078400090206     c     taslna        lookup    fil(k)                                 58
078500090206     c                   if        *in58
078600090206     c     k             occur     dsfil_smp
078700090205     c* . . . giornaliero
078800090206     c                   if        (tasaas*10000+tasmgs)=§al
078900090206     C                   add       1             Wmptsa_g
079000090206     C                   add       TASNCL        wmptca_g
079100090224     C                   add       w_peso        Wmptka_g
079200090206     C                   add       TASVLF        Wmptma_g
079300090206     c                   endif
079400090205     c* . . . riepilogo
079500090206     C                   add       1             Wmptsa_r
079600090206     C                   add       TASNCL        wmptca_r
079700090224     C                   add       w_peso        Wmptka_r
079800090206     C                   add       TASVLF        Wmptma_r
079900090206     c                   endif
080000090710     c* terminal di partenza : esclusa la linea/terminal
080100090710     c                   if        tastfp<>taslnp
080200090206     c                   z-add     1             k
080300090206     c     tastfp        lookup    fil(k)                                 58
080400090206     c                   if        *in58
080500090206     c     k             occur     dsfil_smp
080600090205     c* . . . giornaliero
080700090206     c                   if        (tasaas*10000+tasmgs)=§al
080800090206     C                   add       1             Wmptspt_g
080900090206     C                   add       TASNCL        wmptcpt_g
081000090224     C                   add       w_peso        Wmptkpt_g
081100090206     C                   add       TASVLF        Wmptmpt_g
081200090206     c                   endif
081300090205     c* . . . riepilogo
081400090206     C                   add       1             Wmptspt_r
081500090206     C                   add       TASNCL        wmptcpt_r
081600090224     C                   add       w_peso        Wmptkpt_r
081700090206     C                   add       TASVLF        Wmptmpt_r
081800090206     c                   endif
081900090710     c                   endif
082000090710     c* terminal di partenza : tutto compresa la linea stessa
082100090710     c                   z-add     1             k
082200090710     c     tastfp        lookup    fil(k)                                 58
082300090710     c                   if        *in58
082400090710     c     k             occur     dsfil_smp
082500090710     c* . . . giornaliero
082600090710     c                   if        (tasaas*10000+tasmgs)=§al
082700090710     C                   add       1             Wmptsptc_g
082800090710     C                   add       TASNCL        wmptcptc_g
082900090710     C                   add       w_peso        Wmptkptc_g
083000090710     C                   add       TASVLF        Wmptmptc_g
083100090710     c                   endif
083200090710     c* . . . riepilogo
083300090710     C                   add       1             Wmptsptc_r
083400090710     C                   add       TASNCL        wmptcptc_r
083500090710     C                   add       w_peso        Wmptkptc_r
083600090710     C                   add       TASVLF        Wmptmptc_r
083700090710     c                   endif
083800090710     c
083900090710     c* terminal di arrivo : esclusa la linea/terminal
084000090710     c                   if        tastfa<>taslna
084100090206     c                   z-add     1             k
084200090206     c     tastfa        lookup    fil(k)                                 58
084300090206     c                   if        *in58
084400090206     c     k             occur     dsfil_smp
084500090205     c* . . . giornaliero
084600090206     c                   if        (tasaas*10000+tasmgs)=§al
084700090206     C                   add       1             Wmptsat_g
084800090206     C                   add       TASNCL        wmptcat_g
084900090224     C                   add       w_peso        Wmptkat_g
085000090206     C                   add       TASVLF        Wmptmat_g
085100090206     c                   endif
085200090205     c* . . . riepilogo
085300090206     C                   add       1             Wmptsat_r
085400090206     C                   add       TASNCL        wmptcat_r
085500090224     C                   add       w_peso        Wmptkat_r
085600090206     C                   add       TASVLF        Wmptmat_r
085700090205     c                   endif
085800090710     c                   endif
085900090710     c* terminal di arrivo : tutto compreso il terminal stesso
086000090710     c                   z-add     1             k
086100090710     c     tastfa        lookup    fil(k)                                 58
086200090710     c                   if        *in58
086300090710     c     k             occur     dsfil_smp
086400090710     c* . . . giornaliero
086500090710     c                   if        (tasaas*10000+tasmgs)=§al
086600090710     C                   add       1             Wmptsatc_g
086700090710     C                   add       TASNCL        wmptcatc_g
086800090710     C                   add       w_peso        Wmptkatc_g
086900090710     C                   add       TASVLF        Wmptmatc_g
087000090710     c                   endif
087100090710     c* . . . riepilogo
087200090710     C                   add       1             Wmptsatc_r
087300090710     C                   add       TASNCL        wmptcatc_r
087400090710     C                   add       w_peso        Wmptkatc_r
087500090710     C                   add       TASVLF        Wmptmatc_r
087600090710     c                   endif
087700090710     c
087800090204     c* filiali di transito
087900090209     c     kar5          chain     fiar531c
088000090205     c                   if        %found(fiar531c)
088100090205     c                   movel     ar5uni        dar5trs
088200090205     c*
088300090205     c                   eval      k_fil=§AR5FLP
088400090205     c                   exsr      agg_transiti
088500090205      *
088600090205     c                   if        §ar5fl2>0
088700090205     c                   eval      k_fil=§AR5FL2
088800090205     c                   exsr      agg_transiti
088900090205     c                   endif
089000090205      *
089100090205     c                   if        §ar5fl3>0
089200090205     c                   eval      k_fil=§AR5FL3
089300090205     c                   exsr      agg_transiti
089400090205     c                   endif
089500090205      *
089600090205     c                   if        §ar5fl4>0
089700090205     c                   eval      k_fil=§AR5FL4
089800090205     c                   exsr      agg_transiti
089900090205     c                   endif
090000090205      *
090100090205     c                   if        §ar5fl5>0
090200090205     c                   eval      k_fil=§AR5FL5
090300090205     c                   exsr      agg_transiti
090400090205     c                   endif
090500090205     c                   endif
090600090204     c
090700090204     C                   endsr
090800090205     C**********************************************************
090900090205     C** Aggiornamento/scrittura giornaliero e riepil. x transiti
091000090205     C**********************************************************
091100090205     C     agg_transiti  BEGSR
091200090206     c                   z-add     1             k
091300090206     c     k_fil         lookup    fil(k)                                 58
091400090206     c                   if        *in58
091500090206     c     k             occur     dsfil_smp
091600090205     c* . . . giornaliero
091700090206     c                   if        (tasaas*10000+tasmgs)=§al
091800090206     C                   add       1             Wmptst_g
091900090206     C                   add       TASNCL        wmptct_g
092000090224     C                   add       w_peso        Wmptkt_g
092100090206     C                   add       TASVLF        Wmptmt_g
092200090206     c                   endif
092300090205     c* . . . riepilogo
092400090206     C                   add       1             Wmptst_r
092500090206     C                   add       TASNCL        wmptct_r
092600090224     C                   add       w_peso        Wmptkt_r
092700090206     C                   add       TASVLF        Wmptmt_r
092800090205     c                   endif
092900090205     c                   endsr
093000090204     C**********************************************************
093100090206     C** Riempie campi wfsmp00f
093200090204     C**********************************************************
093300090206     C     rie_campi     BEGSR
093400090204     c* profilo utente e data elaborazione
093500090204     c                   eval      wmpusr=knmus
093600090204     c                   eval      wmpdel=wdatael
093700090206     c*
093800090206     c                   eval      wmpall=§al
093900090204     c* area e descrizione area
094000090204     c                   eval      wmpcar=car(k)
094100090204     C                   MOVEL(P)  '05'          tblcod
094200090204     C                   MOVEL(P)  car(K)        tblkey
094300090204     C     KTAB          CHAIN     TABEL00F                           30
094400090204     C                   IF        %found(tabel00f) and tblflg=*blanks
094500090204     C                   MOVEL     TBLUNI        DS05
094600090204     C                   MOVEL     §05DES        wmpdar
094700090204     C                   endif
094800090204     c* filiale,descrizione e livello filiale
094900090204     c                   eval      wmpdil=dil(K)
095000090206     c                   eval      wmpfil=fil(K)
095100090206     c                   eval      wmplif=lpo(K)
095200090204     c* tipo filiale Italia/Estero
095300090205     c                   eval      wmptif=tif(K)
095400090206     c* spedizioni,colli, kg,volume
095500090206     c                   if        wmptip='G'
095600090206     c                   z-add     wmptsp_G      wmptsp
095700090206     c                   z-add     wmptcp_G      wmptcp
095800090206     c                   z-add(h)  wmptkp_G      wmptkp
095900090206     c                   z-add(h)  wmptmp_G      wmptmp
096000090710
096100090206     c                   z-add     wmptsa_G      wmptsa
096200090206     c                   z-add     wmptca_G      wmptca
096300090206     c                   z-add(h)  wmptka_G      wmptka
096400090206     c                   z-add(h)  wmptma_G      wmptma
096500090710
096600090206     c                   z-add     wmptspt_G     wmptspt
096700090206     c                   z-add     wmptcpt_G     wmptcpt
096800090206     c                   z-add(h)  wmptkpt_G     wmptkpt
096900090206     c                   z-add(h)  wmptmpt_G     wmptmpt
097000090710
097100090710     c                   z-add     wmptsptc_G    wmptsptc
097200090710     c                   z-add     wmptcptc_G    wmptcptc
097300090710     c                   z-add(h)  wmptkptc_G    wmptkptc
097400090710     c                   z-add(h)  wmptmptc_G    wmptmptc
097500090710
097600090206     c                   z-add     wmptsat_G     wmptsat
097700090206     c                   z-add     wmptcat_G     wmptcat
097800090206     c                   z-add(h)  wmptkat_G     wmptkat
097900090206     c                   z-add(h)  wmptmat_G     wmptmat
098000090710
098100090710     c                   z-add     wmptsatc_G    wmptsatc
098200090710     c                   z-add     wmptcatc_G    wmptcatc
098300090710     c                   z-add(h)  wmptkatc_G    wmptkatc
098400090710     c                   z-add(h)  wmptmatc_G    wmptmatc
098500090710
098600090206     c                   z-add     wmptst_G      wmptst
098700090206     c                   z-add     wmptct_G      wmptct
098800090206     c                   z-add(h)  wmptkt_G      wmptkt
098900090206     c                   z-add(h)  wmptmt_G      wmptmt
099000090206     c                   else
099100090206     c                   z-add     wmptsp_r      wmptsp
099200090206     c                   z-add     wmptcp_r      wmptcp
099300090206     c                   z-add(h)  wmptkp_r      wmptkp
099400090206     c                   z-add(h)  wmptmp_r      wmptmp
099500090710
099600090206     c                   z-add     wmptsa_r      wmptsa
099700090206     c                   z-add     wmptca_r      wmptca
099800090206     c                   z-add(h)  wmptka_r      wmptka
099900090206     c                   z-add(h)  wmptma_r      wmptma
100000090710
100100090206     c                   z-add     wmptspt_r     wmptspt
100200090206     c                   z-add     wmptcpt_r     wmptcpt
100300090206     c                   z-add(h)  wmptkpt_r     wmptkpt
100400090206     c                   z-add(h)  wmptmpt_r     wmptmpt
100500090710
100600090710     c                   z-add     wmptsptc_r    wmptsptc
100700090710     c                   z-add     wmptcptc_r    wmptcptc
100800090710     c                   z-add(h)  wmptkptc_r    wmptkptc
100900090710     c                   z-add(h)  wmptmptc_r    wmptmptc
101000090710
101100090206     c                   z-add     wmptsat_r     wmptsat
101200090206     c                   z-add     wmptcat_r     wmptcat
101300090206     c                   z-add(h)  wmptkat_r     wmptkat
101400090206     c                   z-add(h)  wmptmat_r     wmptmat
101500090710
101600090710     c                   z-add     wmptsatc_r    wmptsatc
101700090710     c                   z-add     wmptcatc_r    wmptcatc
101800090710     c                   z-add(h)  wmptkatc_r    wmptkatc
101900090710     c                   z-add(h)  wmptmatc_r    wmptmatc
102000090710
102100090206     c                   z-add     wmptst_r      wmptst
102200090206     c                   z-add     wmptct_r      wmptct
102300090206     c                   z-add(h)  wmptkt_r      wmptkt
102400090206     c                   z-add(h)  wmptmt_r      wmptmt
102500090710
102600090206     c                   endif
102700090204     c                   endsr
102800090204     C**********************************************************
102900120905     C** Elaborazione per file WFLSS60f
103000090204     C**********************************************************
103100090204     C     sr_wslss      BEGSR
103200090204     C                   SETOFF                                       545556
103300090204     C                   SETOFF                                       48  52
103400111129     c
103500111129     c                   z-add     1             kk
103600111129     c     taslnp        lookup    fil(kk)                                98
103700111129     c
103800111129     c                   if        *in98
103900111129     c* Euroexpress o Fedex o DPD
104000111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
104100111129     c                   seton                                        54
104200111129     c                   endif
104300111129     c
104400111129     C**   TASLNP        COMP      300                                54        ESTERO
104500111129     C*+N54TASLNP        COMP      190                                    54    + DPD
104600111129     C**N54TASLNP        COMP      191                                    54    + DPD
104700111129     C**N54TASLNP        COMP      192                                    54    + DPD
104800111129     C**N54TASLNP        COMP      193                                    54    + DPD
104900111129     C**N54TASLNP        COMP      194                                    54    + DPD
105000111129     C**N54TASLNP        COMP      195                                    54    + DPD
105100111129     C**N54TASLNP        COMP      196                                    54    + DPD
105200111129     C**N54TASLNP        COMP      197                                    54    + DPD
105300090204     C*
105400100402     C****     TASLNP        COMP      180                                    48     POSTE
105500090204     C*
105600111129     c                   endif
105700111129     c***
105800111129     c* L I N E A   D I   A R R  I V O
105900111129     c***
106000111129     c                   z-add     1             kk
106100111129     c     taslna        lookup    fil(kk)                                98
106200111129     c
106300111129     c                   if        *in98
106400111129     c* Euroexpress o Fedex o DPD
106500111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
106600111129     c                   seton                                        55
106700111129     c                   endif
106800111129     C*
106900111129     C**   TASLNA        COMP      300                                55
107000111129     C**N55TASLNA        COMP      190                                    55    + DPD
107100111129     C**N55TASLNA        COMP      191                                    55    + DPD
107200111129     C**N55TASLNA        COMP      192                                    55    + DPD
107300111129     C*+N55TASLNA        COMP      193                                    55    + DPD
107400111129     C**N55TASLNA        COMP      194                                    55    + DPD
107500111129     C**N55TASLNA        COMP      195                                    55    + DPD
107600111129     C**N55TASLNA        COMP      196                                    55    + DPD
107700111129     C*+N55TASLNA        COMP      197                                    55    + DPD
107800090204     C*
107900111129     c* Fedex
108000111129     c                   if        tif(kk) ='F'
108100111129     c                   seton                                        52
108200111129     c                   endif
108300111129     c
108400111129     C***  TASLNA        COMP      400                                    52     FEDEX
108500090204     C*
108600111129     c* DPD
108700111129     c                   if        tif(kk) ='D'
108800111129     c                   seton                                        56
108900111129     c                   endif
109000111129     c
109100111129     C**   TASLNA        COMP      190                                    56     DPD
109200111129     C**N56TASLNA        COMP      191                                    56     DPD
109300111129     C**N56TASLNA        COMP      192                                    56     DPD
109400111129     C**N56TASLNA        COMP      193                                    56     DPD
109500111129     C**N56TASLNA        COMP      194                                    56     DPD
109600111129     C**N56TASLNA        COMP      195                                    56     DPD
109700111129     C**N56TASLNA        COMP      196                                    56     DPD
109800111129     C**N56TASLNA        COMP      197                                    56     DPD
109900091006     c*
110000111129     c                   endif
110100111129     c
110200091006     c* Verifico se si tratta di cliente extra budget --> 35 on
110300110711     c                   if        §stat=' '
110400091006     c                   z-add     1             EE
110500091006     c                   if        tasccm<>savccm or tasctr<>savctr
110600091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
110700091007     c     w010a         lookup    ksc_EX(EE)                             35
110800091007     c                   if        not *in35
110900091007     c                   eval      %subst(w010a:8:3)='   '
111000091007     c     w010a         lookup    ksc_EX(EE)                             35
111100091006     c                   endif
111200091006
111300091006     c                   eval      savccm=tasccm
111400091006     c                   eval      savctr=tasctr
111500091006     c                   endif
111600110711     c
111700111028     c                   else
111800111028     c                   seton                                        35
111900110711     c*
112000111028     c**                 z-add     1             EE
112100111028     c**                 if        tasccm<>savccm
112200111028     c**                 eval      w010a=%editc(tasccm:'X')+'   '
112300111028     c**   w010a         lookup    ksc_EX(EE)                             35
112400111028     c**                 eval      savccm=tasccm
112500111028     c**                 endif
112600110711     c
112700110711     c                   endif
112800090204     C*
112900090204     C                   Z-ADD     1             K
113000090204     C     TASLNP        LOOKUP    FIL(K)                                 58
113100090204     C  N58              EXCEPT    ERR
113200090204     C     K             OCCUR     DSFIL
113300100402     C     K             OCCUR     DSFIL_e
113400100402     C     K             OCCUR     DSFIL_h
113500090204     C  N54
113600090204     CANN55              DO                                                     ITALIA
113700100402     c* Spedizioni LNP Italia LNA Italia
113800100402     c                   exsr      speITA
113900090204     C                   ENDDO
114000100402     c
114100100402     c* Spedizioni IMPORT --> LNP Italia
114200090204     C   54              DO                                                     IMPORT
114300100402     c                   exsr      speIMPORT
114400090204     C                   ENDDO
114500100402     c
114600100402     c* Spedizioni lna EUROEXPRESS
114700090204     C   55
114800090204     CANN56
114900090204     CANN52              DO                                                     EXPORT
115000100402     c                   exsr      speEXPRESS
115100090204     C                   ENDDO
115200100402     c
115300100402     c* Spedizioni lna DPD
115400090204     C   55
115500090204     CAN 56              DO                                                     EXPORT
115600100402     c                   exsr      speDPD
115700090204     C                   ENDDO
115800100402     c
115900100402     c* Spedizioni lna FEDEX
116000090204     C   55
116100090204     CAN 52              DO                                                     EXPORT
116200100402     c                   exsr      speFEDEX
116300090204     C                   ENDDO
116400100402     c
116500090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
116600090204     C                   Z-ADD     1             K
116700090204     C     TASLNA        LOOKUP    FIL(K)                                 58
116800090204     C  N58              EXCEPT    ERR
116900090204     C     K             OCCUR     DSFIL
117000100402     C     K             OCCUR     DSFIL_e
117100100402     C     K             OCCUR     DSFIL_h
117200100402     c
117300090204     C  N54
117400100402     CANN55              DO
117500100402     c* Spedizioni LNP Italia LNA Italia
117600100402     c                   exsr      speITA_LNA
117700090204     C                   ENDDO
117800100402     c
117900100402     c* spedizioni EXPORT --> lna estera
118000090204     C   55              DO                                                     ESTERO ASS.+FRA.
118100100402     c                   exsr      speEXPORT
118200090204     C                   ENDDO
118300090204     c                   endsr
118400090206     C**********************************************************
118500090206     C** scarica ds multiple su file
118600090206     C**********************************************************
118700090206     C     scrive_file   BEGSR
118800090206     c*
118900090206     C                   Z-ADD     §AL           WSSALL
119000090206     c*
119100110711    1C                   DO        400           K
119200090206     C     K             OCCUR     DSFIL_smp
119300090206     C     K             OCCUR     DSFIL
119400100402     C     K             OCCUR     DSFIL_e
119500100402     C     K             OCCUR     DSFIL_h
119600110711     c
119700110711    2c                   if        §stat=' '
119800090206     c* WFSMP
119900090206     c* giornaliero
120000090601     c* Lo scrivo solo per tipo "G" o "R"
120100090601     c                   if        Wtipo='G' or Wtipo='R'
120200090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
120300090206     c                              wmptspt_g>0 or wmptsat_g>0 or
120400090206     c                              wmptst_g>0)
120500090206     c                   z-add     §al           wmpdal
120600090206     c                   eval      wmptip='G'
120700090206     c                   exsr      rie_campi
120800090206     c                   write     wfsmp000
120900090206     c                   endif
121000090601     c                   endif
121100090601     c
121200090601     c* riepilogo /precedente
121300090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
121400090601     c                   if        Wtipo='P' or Wtipo='R'
121500090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
121600090206     c                              wmptspt_r>0 or wmptsat_r>0 or
121700090206     c                              wmptst_r>0)
121800090206     c                   z-add     §dal          wmpdal
121900090601     c                   if        Wtipo='R'
122000090206     c                   eval      wmptip='R'
122100090601     c                   else
122200090601     c                   eval      wmptip='P'
122300090601     c                   endif
122400090206     c                   exsr      rie_campi
122500090206     c                   write     wfsmp000
122600090206     c                   endif
122700090601     c                   endif
122800110711     c
122900110711    2c                   endif
123000120905     c* WFLSS60F
123100120904     c                   clear                   wsskgf
123200090206     c* giornaliero
123300090601     c                   if        Wtipo='G' or Wtipo='R'
123400110711     c
123500110711    2c                   if        §stat=' '
123600090601     c
123700090206     C     WTSPIP        IFGT      *ZERO
123800090206     C                   Z-ADD     §al           WSSDAL
123900090206     c                   movel     'G'           WSSTIE
124000090206     C                   MOVE      'IP'          WSSTIP
124100090206     C                   Z-ADD     FIL(K)        WSSLIN
124200090206     C                   Z-ADD     WTSPIP        WSSTSP
124300090206     C                   Z-ADD     WTCLIP        WSSTCL
124400090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
124500090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
124600120904     c*old
124700120904     C                   Z-ADD(H)  WTKGIP        WSSTKG_ol
124800120904     C                   Z-ADD(H)  WTMCIP        WSSTMC_ol
124900100402     c* spedizioni "E"
125000100402     C                   Z-ADD     eWTSPIP       WSSTSPe
125100100402     C                   Z-ADD     eWTCLIP       WSSTCLe
125200100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
125300100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
125400120904     c* old
125500120904     C                   Z-ADD(H)  eWTKGIP       WSSTKGe_ol
125600120904     C                   Z-ADD(H)  eWTMCIP       WSSTMCe_ol
125700100402     c* spedizioni "H"
125800100402     C                   Z-ADD     hWTSPIP       WSSTSPh
125900100402     C                   Z-ADD     hWTCLIP       WSSTCLh
126000100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
126100100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
126200120904     c* old
126300120904     C                   Z-ADD(H)  hWTKGIP       WSSTKGh_ol
126400120904     C                   Z-ADD(H)  hWTMCIP       WSSTMCh_ol
126500100402
126600120905     C                   WRITE     WFLSS600
126700120904     C                   WRITE     WSLSS500
126800090206     C                   ENDIF
126900100402
127000090206     C     WTSPIA        IFGT      *ZERO
127100090206     C                   Z-ADD     §al           WSSDAL
127200090206     c                   movel     'G'           WSSTIE
127300090206     C                   MOVE      'IA'          WSSTIP
127400090206     C                   Z-ADD     FIL(K)        WSSLIN
127500090206     C                   Z-ADD     WTSPIA        WSSTSP
127600090206     C                   Z-ADD     WTCLIA        WSSTCL
127700090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
127800090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
127900120904     c* old
128000120904     C                   Z-ADD(H)  WTKGIA        WSSTKG_ol
128100120904     C                   Z-ADD(H)  WTMCIA        WSSTMC_ol
128200100402     c* spedizioni "E"
128300100402     C                   Z-ADD     eWTSPIA       WSSTSPe
128400100402     C                   Z-ADD     eWTCLIA       WSSTCLe
128500100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
128600100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
128700120904     c* old
128800120904     C                   Z-ADD(H)  eWTKGIA       WSSTKGe_ol
128900120904     C                   Z-ADD(H)  eWTMCIA       WSSTMCe_ol
129000100402     c* spedizioni "H"
129100100402     C                   Z-ADD     hWTSPIA       WSSTSPh
129200100402     C                   Z-ADD     hWTCLIA       WSSTCLh
129300100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
129400100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
129500120904     c* old
129600120904     C                   Z-ADD(H)  hWTKGIA       WSSTKGh_ol
129700120904     C                   Z-ADD(H)  hWTMCIA       WSSTMCh_ol
129800100402     c
129900120905     C                   WRITE     WFLSS600
130000120904     C                   WRITE     WSLSS500
130100090206     C                   ENDIF
130200100402     c
130300090206     C     WTSPEI        IFGT      *ZERO
130400090206     C                   Z-ADD     §al           WSSDAL
130500090206     c                   movel     'G'           WSSTIE
130600090206     C                   MOVE      'EI'          WSSTIP
130700090206     C                   Z-ADD     FIL(K)        WSSLIN
130800090206     C                   Z-ADD     WTSPEI        WSSTSP
130900090206     C                   Z-ADD     WTCLEI        WSSTCL
131000090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
131100090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
131200120904     c* old
131300120904     C                   Z-ADD(H)  WTKGEI        WSSTKG_ol
131400120904     C                   Z-ADD(H)  WTMCEI        WSSTMC_ol
131500100402     c* spedizioni "E"
131600100402     C                   Z-ADD     eWTSPEI       WSSTSPe
131700100402     C                   Z-ADD     eWTCLEI       WSSTCLe
131800100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
131900100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
132000120904     c* old
132100120904     C                   Z-ADD(H)  eWTKGEI       WSSTKGe_ol
132200120904     C                   Z-ADD(H)  eWTMCEI       WSSTMCe_ol
132300100402     c* spedizioni "H"
132400100402     C                   Z-ADD     hWTSPEI       WSSTSPh
132500100402     C                   Z-ADD     hWTCLEI       WSSTCLh
132600100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
132700100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
132800120904     c* old
132900120904     C                   Z-ADD(H)  hWTKGEI       WSSTKGh_ol
133000120904     C                   Z-ADD(H)  hWTMCEI       WSSTMCh_ol
133100100402     c
133200120905     C                   WRITE     WFLSS600
133300120904     C                   WRITE     WSLSS500
133400120904     C                   ENDIF
133500100402     c
133600090206     C     WTSPEE        IFGT      *ZERO
133700090206     C                   Z-ADD     §al           WSSDAL
133800090206     c                   movel     'G'           WSSTIE
133900090206     C                   MOVE      'EE'          WSSTIP
134000090206     C                   Z-ADD     FIL(K)        WSSLIN
134100090206     C                   Z-ADD     WTSPEE        WSSTSP
134200090206     C                   Z-ADD     WTCLEE        WSSTCL
134300090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
134400090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
134500120904     c* old
134600120904     C                   Z-ADD(H)  WTKGEE        WSSTKG_ol
134700120904     C                   Z-ADD(H)  WTMCEE        WSSTMC_ol
134800100402     c* spedizioni "E"
134900100402     C                   Z-ADD     eWTSPEE       WSSTSPe
135000100402     C                   Z-ADD     eWTCLEE       WSSTCLe
135100100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
135200100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
135300120904     c* old
135400120904     C                   Z-ADD(H)  eWTKGEE       WSSTKGe_ol
135500120904     C                   Z-ADD(H)  eWTMCEE       WSSTMCe_ol
135600100402     c* spedizioni "H"
135700100402     C                   Z-ADD     hWTSPEE       WSSTSPh
135800100402     C                   Z-ADD     hWTCLEE       WSSTCLh
135900100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
136000100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
136100120904     c* old
136200120904     C                   Z-ADD(H)  hWTKGEE       WSSTKGh_ol
136300120904     C                   Z-ADD(H)  hWTMCEE       WSSTMCh_ol
136400100402     c
136500120905     C                   WRITE     WFLSS600
136600120904     C                   WRITE     WSLSS500
136700090206     C                   ENDIF
136800090206     C*
136900090206     C     WTSPED        IFGT      *ZERO
137000090206     C                   Z-ADD     §al           WSSDAL
137100090206     c                   movel     'G'           WSSTIE
137200090206     C                   MOVE      'ED'          WSSTIP
137300090206     C                   Z-ADD     FIL(K)        WSSLIN
137400090206     C                   Z-ADD     WTSPED        WSSTSP
137500090206     C                   Z-ADD     WTCLED        WSSTCL
137600090206     C                   Z-ADD(H)  WTKGED        WSSTKG
137700090206     C                   Z-ADD(H)  WTMCED        WSSTMC
137800120904     c* old
137900120904     C                   Z-ADD(H)  WTKGED        WSSTKG_ol
138000120904     C                   Z-ADD(H)  WTMCED        WSSTMC_ol
138100100402     c* spedizioni "E"
138200100402     C                   Z-ADD     eWTSPED       WSSTSPe
138300100402     C                   Z-ADD     eWTCLED       WSSTCLe
138400100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
138500100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
138600120904     c* old
138700120904     C                   Z-ADD(H)  eWTKGED       WSSTKGe_ol
138800120904     C                   Z-ADD(H)  eWTMCED       WSSTMCe_ol
138900100402     c* spedizioni "H"
139000100402     C                   Z-ADD     hWTSPED       WSSTSPh
139100100402     C                   Z-ADD     hWTCLED       WSSTCLh
139200100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
139300100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
139400120904     c* old
139500120904     C                   Z-ADD(H)  hWTKGED       WSSTKGh_ol
139600120904     C                   Z-ADD(H)  hWTMCED       WSSTMCh_ol
139700100402     c
139800120905     C                   WRITE     WFLSS600
139900120904     C                   WRITE     WSLSS500
140000090206     C                   ENDIF
140100090206     C*
140200090206     C     WTSPEX        IFGT      *ZERO
140300090206     C                   Z-ADD     §al           WSSDAL
140400090206     c                   movel     'G'           WSSTIE
140500090206     C                   MOVE      'EX'          WSSTIP
140600090206     C                   Z-ADD     FIL(K)        WSSLIN
140700090206     C                   Z-ADD     WTSPEX        WSSTSP
140800090206     C                   Z-ADD     WTCLEX        WSSTCL
140900090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
141000090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
141100120904     c* old
141200120904     C                   Z-ADD(H)  WTKGEX        WSSTKG_ol
141300120904     C                   Z-ADD(H)  WTMCEX        WSSTMC_ol
141400100402     c* spedizioni "E"
141500100402     C                   Z-ADD     eWTSPEX       WSSTSPe
141600100402     C                   Z-ADD     eWTCLEX       WSSTCLe
141700100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
141800100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
141900120904     c* old
142000120904     C                   Z-ADD(H)  eWTKGEX       WSSTKGe_ol
142100120904     C                   Z-ADD(H)  eWTMCEX       WSSTMCe_ol
142200100402     c* spedizioni "H"
142300100402     C                   Z-ADD     hWTSPEX       WSSTSPh
142400100402     C                   Z-ADD     hWTCLEX       WSSTCLh
142500100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
142600100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
142700120904     c* old
142800120904     C                   Z-ADD(H)  hWTKGEX       WSSTKGh_ol
142900120904     C                   Z-ADD(H)  hWTMCEX       WSSTMCh_ol
143000100402     c
143100120905     C                   WRITE     WFLSS600
143200120904     C                   WRITE     WSLSS500
143300090206     C                   ENDIF
143400090206     C*
143500090206     C     WTSPEA        IFGT      *ZERO
143600090206     C                   Z-ADD     §al           WSSDAL
143700090206     c                   movel     'G'           WSSTIE
143800090206     C                   MOVE      'EA'          WSSTIP
143900090206     C                   Z-ADD     FIL(K)        WSSLIN
144000090206     C                   Z-ADD     WTSPEA        WSSTSP
144100090206     C                   Z-ADD     WTCLEA        WSSTCL
144200090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
144300090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
144400120904     c* old
144500120904     C                   Z-ADD(H)  WTKGEA        WSSTKG_ol
144600120904     C                   Z-ADD(H)  WTMCEA        WSSTMC_ol
144700100402     c* spedizioni "E"
144800100402     C                   Z-ADD     eWTSPEA       WSSTSPe
144900100402     C                   Z-ADD     eWTCLEA       WSSTCLe
145000100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
145100100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
145200120904     c* old
145300120904     C                   Z-ADD(H)  eWTKGEA       WSSTKGe_ol
145400120904     C                   Z-ADD(H)  eWTMCEA       WSSTMCe_ol
145500100402     c* spedizioni "H"
145600100402     C                   Z-ADD     hWTSPEA       WSSTSPh
145700100402     C                   Z-ADD     hWTCLEA       WSSTCLh
145800100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
145900100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
146000120904     c* old
146100120904     C                   Z-ADD(H)  hWTKGEA       WSSTKGh_ol
146200120904     C                   Z-ADD(H)  hWTMCEA       WSSTMCh_ol
146300100402     c
146400120905     C                   WRITE     WFLSS600
146500120904     C                   WRITE     WSLSS500
146600090206     C                   ENDIF
146700090206     C*
146800090206     C     WTSPPA        IFGT      *ZERO
146900090206     C                   Z-ADD     §al           WSSDAL
147000090206     c                   movel     'G'           WSSTIE
147100090206     C                   MOVE      'PA'          WSSTIP
147200090206     C                   Z-ADD     FIL(K)        WSSLIN
147300090206     C                   Z-ADD     WTSPPA        WSSTSP
147400090206     C                   Z-ADD     WTCLPA        WSSTCL
147500090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
147600090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
147700120904     c* old
147800120904     C                   Z-ADD(H)  WTKGPA        WSSTKG_ol
147900120904     C                   Z-ADD(H)  WTMCPA        WSSTMC_ol
148000100402     c* spedizioni "E"
148100100402     C                   Z-ADD     eWTSPPA       WSSTSPe
148200100402     C                   Z-ADD     eWTCLPA       WSSTCLe
148300100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
148400100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
148500120904     c* old
148600120904     C                   Z-ADD(H)  eWTKGPA       WSSTKGe_ol
148700120904     C                   Z-ADD(H)  eWTMCPA       WSSTMCe_ol
148800100402     c* spedizioni "H"
148900100402     C                   Z-ADD     hWTSPPA       WSSTSPh
149000100402     C                   Z-ADD     hWTCLPA       WSSTCLh
149100100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
149200100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
149300120904     c* old
149400120904     C                   Z-ADD(H)  hWTKGPA       WSSTKGh_ol
149500120904     C                   Z-ADD(H)  hWTMCPA       WSSTMCh_ol
149600100402     c
149700120905     C                   WRITE     WFLSS600
149800120904     C                   WRITE     WSLSS500
149900090206     C                   ENDIF
150000110711     c
150100110711     c                   endif
150200110715     c* Extra budget o clienti particolari
150300110715     c                   if        §stat=' '
150400110715     c                   movel     'G'           WSSTIE
150500110715     c                   else
150600110715     c                   movel     'A'           WSSTIE
150700110715     c                   endif
150800110715     c
150900091006     C     WTSPIP_e      IFGT      *ZERO
151000091007     C                   Z-ADD     §al           WSSDAL
151100091006     C                   MOVE      'IP'          WSSTIP
151200091006     C                   Z-ADD     FIL(K)        WSSLIN
151300091006     C                   Z-ADD     WTSPIP_e      WSSTSP
151400091006     C                   Z-ADD     WTCLIP_e      WSSTCL
151500120904     C                   Z-ADD(H)  WTKGIP_e      WSSTKG_ex
151600120904     C                   Z-ADD(H)  WTMCIP_e      WSSTMC_ex
151700100402     c* spedizioni "E"
151800100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
151900100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
152000120904     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe_ex
152100120904     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe_ex
152200100402     c* spedizioni "H"
152300100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
152400100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
152500120904     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh_ex
152600120904     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh_ex
152700100402     c
152800120903     C                   WRITE     wslss550
152900091006     C                   ENDIF
153000100402     c
153100091006     C     WTSPIA_e      IFGT      *ZERO
153200091007     C                   Z-ADD     §al           WSSDAL
153300110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
153400110715     c*****              movel     'G'           WSSTIE
153500091006     C                   MOVE      'IA'          WSSTIP
153600091006     C                   Z-ADD     FIL(K)        WSSLIN
153700091006     C                   Z-ADD     WTSPIA_e      WSSTSP
153800091006     C                   Z-ADD     WTCLIA_e      WSSTCL
153900120904     C                   Z-ADD(H)  WTKGIA_e      WSSTKG_ex
154000120904     C                   Z-ADD(H)  WTMCIA_e      WSSTMC_ex
154100100402     c* spedizioni "E"
154200100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
154300100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
154400120904     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe_ex
154500120904     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe_ex
154600100402     c* spedizioni "H"
154700100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
154800100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
154900120904     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh_ex
155000120904     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh_ex
155100100402     c
155200091006     C                   WRITE     WSLSS550
155300091006     C                   ENDIF
155400100402     c
155500091006     C     WTSPEI_e      IFGT      *ZERO
155600091007     C                   Z-ADD     §al           WSSDAL
155700110715     c******             movel     'G'           WSSTIE
155800091006     C                   MOVE      'EI'          WSSTIP
155900091006     C                   Z-ADD     FIL(K)        WSSLIN
156000091006     C                   Z-ADD     WTSPEI_e      WSSTSP
156100091006     C                   Z-ADD     WTCLEI_e      WSSTCL
156200120904     C                   Z-ADD(H)  WTKGEI_e      WSSTKG_ex
156300120904     C                   Z-ADD(H)  WTMCEI_e      WSSTMC_ex
156400100402     c* spedizioni "E"
156500100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
156600100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
156700120904     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe_ex
156800120904     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe_ex
156900100402     c* spedizioni "H"
157000100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
157100100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
157200120904     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh_ex
157300120904     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh_ex
157400100402     c
157500091006     C                   WRITE     WSLSS550
157600091006     C                   ENDIF
157700100402     c
157800091006     C     WTSPEE_e      IFGT      *ZERO
157900091007     C                   Z-ADD     §al           WSSDAL
158000110715     c******             movel     'G'           WSSTIE
158100091006     C                   MOVE      'EE'          WSSTIP
158200091006     C                   Z-ADD     FIL(K)        WSSLIN
158300091006     C                   Z-ADD     WTSPEE_e      WSSTSP
158400091006     C                   Z-ADD     WTCLEE_e      WSSTCL
158500120904     C                   Z-ADD(H)  WTKGEE_e      WSSTKG_ex
158600120904     C                   Z-ADD(H)  WTMCEE_e      WSSTMC_ex
158700100402     c* spedizioni "E"
158800100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
158900100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
159000120904     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe_ex
159100120904     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe_ex
159200100402     c* spedizioni "H"
159300100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
159400100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
159500120904     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh_ex
159600120904     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh_ex
159700100402     c
159800091006     C                   WRITE     WSLSS550
159900091006     C                   ENDIF
160000091006     C*
160100091006     C     WTSPED_e      IFGT      *ZERO
160200091007     C                   Z-ADD     §al           WSSDAL
160300110715     c*******            movel     'G'           WSSTIE
160400091006     C                   MOVE      'ED'          WSSTIP
160500091006     C                   Z-ADD     FIL(K)        WSSLIN
160600091006     C                   Z-ADD     WTSPED_e      WSSTSP
160700091006     C                   Z-ADD     WTCLED_e      WSSTCL
160800120904     C                   Z-ADD(H)  WTKGED_e      WSSTKG_ex
160900120904     C                   Z-ADD(H)  WTMCED_e      WSSTMC_ex
161000100402     c* spedizioni "E"
161100100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
161200100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
161300120904     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe_ex
161400120904     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe_ex
161500100402     c* spedizioni "H"
161600100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
161700100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
161800120904     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh_ex
161900120904     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh_ex
162000100402     c
162100091006     C                   WRITE     WSLSS550
162200091006     C                   ENDIF
162300091006     C*
162400091006     C     WTSPEX_e      IFGT      *ZERO
162500091007     C                   Z-ADD     §al           WSSDAL
162600110715     c*******            movel     'G'           WSSTIE
162700091006     C                   MOVE      'EX'          WSSTIP
162800091006     C                   Z-ADD     FIL(K)        WSSLIN
162900091006     C                   Z-ADD     WTSPEX_e      WSSTSP
163000091006     C                   Z-ADD     WTCLEX_e      WSSTCL
163100120904     C                   Z-ADD(H)  WTKGEX_e      WSSTKG_ex
163200120904     C                   Z-ADD(H)  WTMCEX_e      WSSTMC_ex
163300100402     c* spedizioni "E"
163400100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
163500100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
163600120904     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe_ex
163700120904     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe_ex
163800100402     c* spedizioni "H"
163900100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
164000100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
164100120904     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh_ex
164200120904     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh_ex
164300100402     c
164400091006     C                   WRITE     WSLSS550
164500091006     C                   ENDIF
164600091006     C*
164700091006     C     WTSPEA_e      IFGT      *ZERO
164800091007     C                   Z-ADD     §al           WSSDAL
164900110715     c******             movel     'G'           WSSTIE
165000091006     C                   MOVE      'EA'          WSSTIP
165100091006     C                   Z-ADD     FIL(K)        WSSLIN
165200091006     C                   Z-ADD     WTSPEA_e      WSSTSP
165300091006     C                   Z-ADD     WTCLEA_e      WSSTCL
165400120904     C                   Z-ADD(H)  WTKGEA_e      WSSTKG_ex
165500120904     C                   Z-ADD(H)  WTMCEA_e      WSSTMC_ex
165600100402     c* spedizioni "E"
165700100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
165800100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
165900120904     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe_ex
166000120904     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe_ex
166100100402     c* spedizioni "H"
166200100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
166300100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
166400120904     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh_ex
166500120904     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh_ex
166600100402     c
166700091006     C                   WRITE     WSLSS550
166800091006     C                   ENDIF
166900091006     C*
167000091006     C     WTSPPA_e      IFGT      *ZERO
167100091007     C                   Z-ADD     §al           WSSDAL
167200110715     c*******            movel     'G'           WSSTIE
167300091006     C                   MOVE      'PA'          WSSTIP
167400091006     C                   Z-ADD     FIL(K)        WSSLIN
167500091006     C                   Z-ADD     WTSPPA_e      WSSTSP
167600091006     C                   Z-ADD     WTCLPA_e      WSSTCL
167700120904     C                   Z-ADD(H)  WTKGPA_e      WSSTKG_ex
167800120904     C                   Z-ADD(H)  WTMCPA_e      WSSTMC_ex
167900100402     c* spedizioni "E"
168000100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
168100100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
168200120904     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe_ex
168300120904     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe_ex
168400100402     c* spedizioni "H"
168500100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
168600100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
168700120904     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh_ex
168800120904     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh_ex
168900100402     c
169000091006     C                   WRITE     WSLSS550
169100091006     C                   ENDIF
169200091006
169300090601     C                   ENDIF
169400090206     c*
169500090206     c* riepilogo
169600090601     c                   if        Wtipo='P' or Wtipo='R'
169700110715     c                   if        §stat=' '
169800110715
169900090206     C     WTSPIP_r      IFGT      *ZERO
170000090206     C                   Z-ADD     §dal          WSSDAL
170100090601     c**                 movel     'R'           WSSTIE
170200090601     c                   movel     Wtipo         WSSTIE
170300090206     C                   MOVE      'IP'          WSSTIP
170400090206     C                   Z-ADD     FIL(K)        WSSLIN
170500090206     C                   Z-ADD     WTSPIP_r      WSSTSP
170600090206     C                   Z-ADD     WTCLIP_r      WSSTCL
170700090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
170800090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
170900120904     c* old
171000120904     C                   Z-ADD(H)  WTKGIP_r      WSSTKG_ol
171100120904     C                   Z-ADD(H)  WTMCIP_r      WSSTMC_ol
171200100402     c* spedizioni "E"
171300100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
171400100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
171500100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
171600100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
171700120904     c* old
171800120904     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe_ol
171900120904     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe_ol
172000100402     c* spedizioni "H"
172100100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
172200100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
172300100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
172400100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
172500120904     c* old
172600120904     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh_ol
172700120904     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh_ol
172800100402     c
172900120905     C                   WRITE     WFLSS600
173000120904     C                   WRITE     WSLSS500
173100090206     C                   ENDIF
173200100402     c
173300090206     C     WTSPIA_r      IFGT      *ZERO
173400090206     C                   Z-ADD     §dal          WSSDAL
173500090601     c**                 movel     'R'           WSSTIE
173600090601     c                   movel     Wtipo         WSSTIE
173700090206     C                   MOVE      'IA'          WSSTIP
173800090206     C                   Z-ADD     FIL(K)        WSSLIN
173900090206     C                   Z-ADD     WTSPIA_r      WSSTSP
174000090206     C                   Z-ADD     WTCLIA_r      WSSTCL
174100090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
174200090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
174300120904     c* old
174400120904     C                   Z-ADD(H)  WTKGIA_r      WSSTKG_ol
174500120904     C                   Z-ADD(H)  WTMCIA_r      WSSTMC_ol
174600100402     c* spedizioni "E"
174700100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
174800100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
174900100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
175000100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
175100120904     c* old
175200120904     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe_ol
175300120904     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe_ol
175400100402     c* spedizioni "H"
175500100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
175600100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
175700100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
175800100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
175900120904     c* old
176000120904     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh_ol
176100120904     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh_ol
176200100402     c
176300120905     C                   WRITE     WFLSS600
176400120904     C                   WRITE     WSLSS500
176500090206     C                   ENDIF
176600100402
176700090206     C     WTSPEI_R      IFGT      *ZERO
176800090206     C                   Z-ADD     §dal          WSSDAL
176900090601     c**                 movel     'R'           WSSTIE
177000090601     c                   movel     Wtipo         WSSTIE
177100090206     C                   MOVE      'EI'          WSSTIP
177200090206     C                   Z-ADD     FIL(K)        WSSLIN
177300090206     C                   Z-ADD     WTSPEI_r      WSSTSP
177400090206     C                   Z-ADD     WTCLEI_r      WSSTCL
177500090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
177600090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
177700120904     c* old
177800120904     C                   Z-ADD(H)  WTKGEI_r      WSSTKG_ol
177900120904     C                   Z-ADD(H)  WTMCEI_r      WSSTMC_ol
178000100402     c* Spedizioni "E"
178100100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
178200100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
178300100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
178400100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
178500120904     c* old
178600120904     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe_ol
178700120904     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe_ol
178800100402     c* Spedizioni "H"
178900100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
179000100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
179100100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
179200100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
179300120904     c* old
179400120904     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh_ol
179500120904     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh_ol
179600100402     c
179700120905     C                   WRITE     WFLSS600
179800120904     C                   WRITE     WSLSS500
179900090206     C                   ENDIF
180000100402     c
180100090206     C     WTSPEE_r      IFGT      *ZERO
180200090206     C                   Z-ADD     §dal          WSSDAL
180300090601     c**                 movel     'R'           WSSTIE
180400090601     c                   movel     Wtipo         WSSTIE
180500090206     C                   MOVE      'EE'          WSSTIP
180600090206     C                   Z-ADD     FIL(K)        WSSLIN
180700090206     C                   Z-ADD     WTSPEE_r      WSSTSP
180800090206     C                   Z-ADD     WTCLEE_r      WSSTCL
180900090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
181000090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
181100120904     c* old
181200120904     C                   Z-ADD(H)  WTKGEE_r      WSSTKG_ol
181300120904     C                   Z-ADD(H)  WTMCEE_r      WSSTMC_ol
181400100402     c* Spedizioni "E"
181500100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
181600100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
181700100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
181800100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
181900120904     c* old
182000120904     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe_ol
182100120904     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe_ol
182200100402     c* Spedizioni "H"
182300100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
182400100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
182500100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
182600100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
182700120904     c* old
182800120904     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh_ol
182900120904     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh_ol
183000100402     c
183100120905     C                   WRITE     WFLSS600
183200120904     C                   WRITE     WSLSS500
183300090206     C                   ENDIF
183400090206     C*
183500090206     C     WTSPED_r      IFGT      *ZERO
183600090206     C                   Z-ADD     §dal          WSSDAL
183700090601     c**                 movel     'R'           WSSTIE
183800090601     c                   movel     Wtipo         WSSTIE
183900090206     C                   MOVE      'ED'          WSSTIP
184000090206     C                   Z-ADD     FIL(K)        WSSLIN
184100090206     C                   Z-ADD     WTSPED_r      WSSTSP
184200090206     C                   Z-ADD     WTCLED_r      WSSTCL
184300090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
184400090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
184500120904     c* old
184600120904     C                   Z-ADD(H)  WTKGED_r      WSSTKG_ol
184700120904     C                   Z-ADD(H)  WTMCED_r      WSSTMC_ol
184800100402     c* Spedizioni "E"
184900100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
185000100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
185100100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
185200100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
185300120904     c* old
185400120904     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe_ol
185500120904     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe_ol
185600100402     c* Spedizioni "H"
185700100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
185800100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
185900100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
186000100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
186100120904     c* old
186200120904     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh_ol
186300120904     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh_ol
186400100402     c
186500120905     C                   WRITE     WFLSS600
186600120904     C                   WRITE     WSLSS500
186700090206     C                   ENDIF
186800090206     C*
186900090206     C     WTSPEX_r      IFGT      *ZERO
187000090206     C                   Z-ADD     §dal          WSSDAL
187100090601     c**                 movel     'R'           WSSTIE
187200090601     c                   movel     Wtipo         WSSTIE
187300090206     C                   MOVE      'EX'          WSSTIP
187400090206     C                   Z-ADD     FIL(K)        WSSLIN
187500090206     C                   Z-ADD     WTSPEX_r      WSSTSP
187600090206     C                   Z-ADD     WTCLEX_r      WSSTCL
187700090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
187800090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
187900120904     c* old
188000120904     C                   Z-ADD(H)  WTKGEX_r      WSSTKG_ol
188100120904     C                   Z-ADD(H)  WTMCEX_r      WSSTMC_ol
188200100402     c* Spedizioni "E"
188300100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
188400100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
188500100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
188600100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
188700120904     c* old
188800120904     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe_ol
188900120904     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe_ol
189000100402     c* Spedizioni "H"
189100100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
189200100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
189300100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
189400100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
189500120904     c* old
189600120904     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh_ol
189700120904     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh_ol
189800100402     c
189900120905     C                   WRITE     WFLSS600
190000120904     C                   WRITE     WSLSS500
190100090206     C                   ENDIF
190200090206     C*
190300090206     C     WTSPEA_r      IFGT      *ZERO
190400090206     C                   Z-ADD     §dal          WSSDAL
190500090601     c**                 movel     'R'           WSSTIE
190600090601     c                   movel     Wtipo         WSSTIE
190700090206     C                   MOVE      'EA'          WSSTIP
190800090206     C                   Z-ADD     FIL(K)        WSSLIN
190900090206     C                   Z-ADD     WTSPEA_r      WSSTSP
191000090206     C                   Z-ADD     WTCLEA_r      WSSTCL
191100090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
191200090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
191300120904     c* old
191400120904     C                   Z-ADD(H)  WTKGEA_r      WSSTKG_ol
191500120904     C                   Z-ADD(H)  WTMCEA_r      WSSTMC_ol
191600100402     c* Spedizioni "E"
191700100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
191800100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
191900100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
192000100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
192100120904     c* old
192200120904     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe_ol
192300120904     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe_ol
192400100402     c* Spedizioni "H"
192500100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
192600100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
192700100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
192800100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
192900120904     c* old
193000120904     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh_ol
193100120904     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh_ol
193200100402     c
193300120905     C                   WRITE     WFLSS600
193400120904     C                   WRITE     WSLSS500
193500090206     C                   ENDIF
193600090206     C*
193700090206     C     WTSPPA_r      IFGT      *ZERO
193800090206     C                   Z-ADD     §dal          WSSDAL
193900090601     c**                 movel     'R'           WSSTIE
194000090601     c                   movel     Wtipo         WSSTIE
194100090206     C                   MOVE      'PA'          WSSTIP
194200090206     C                   Z-ADD     FIL(K)        WSSLIN
194300090206     C                   Z-ADD     WTSPPA_r      WSSTSP
194400090206     C                   Z-ADD     WTCLPA_r      WSSTCL
194500090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
194600090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
194700120904     c* old
194800120904     C                   Z-ADD(H)  WTKGPA_r      WSSTKG_ol
194900120904     C                   Z-ADD(H)  WTMCPA_r      WSSTMC_ol
195000100402     c* Spedizioni "E"
195100100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
195200100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
195300100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
195400100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
195500120904     c* old
195600120904     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe_ol
195700120904     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe_ol
195800100402     c* Spedizioni "H"
195900100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
196000100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
196100100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
196200100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
196300120904     c* old
196400120904     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh_ol
196500120904     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh_ol
196600100402     c
196700120905     C                   WRITE     WFLSS600
196800120904     C                   WRITE     wslss500
196900090206     C                   ENDIF
197000110715     c
197100110715     c                   endif
197200110715     c
197300110715     c* Extra budget e clienti particolari
197400110715     c                   movel     wtipo         tipoC             1
197500110715     c                   if        §stat<>' ' and wtipo='R'
197600110715     c                   eval      tipoC='B'
197700110715     c                   endif
197800110715     c                   if        §stat<>' ' and wtipo='P'
197900110715     c                   eval      tipoC='C'
198000110715     c                   endif
198100110715     c
198200091006     C     WTSPIP_re     IFGT      *ZERO
198300091006     C                   Z-ADD     §dal          WSSDAL
198400110715     c                   movel     TipoC         WSSTIE
198500091006     C                   MOVE      'IP'          WSSTIP
198600091006     C                   Z-ADD     FIL(K)        WSSLIN
198700091006     C                   Z-ADD     WTSPIP_re     WSSTSP
198800091006     C                   Z-ADD     WTCLIP_re     WSSTCL
198900120904     C                   Z-ADD(H)  WTKGIP_re     WSSTKG_ex
199000120904     C                   Z-ADD(H)  WTMCIP_re     WSSTMC_ex
199100100402     c* Spedizioni "E"
199200100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
199300100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
199400120904     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe_ex
199500120904     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe_ex
199600100402     c* Spedizioni "H"
199700100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
199800100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
199900120904     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh_ex
200000120904     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh_ex
200100100402     c
200200091006     C                   WRITE     WSLSS550
200300091006     C                   ENDIF
200400100402     c
200500091006     C     WTSPIA_re     IFGT      *ZERO
200600091006     C                   Z-ADD     §dal          WSSDAL
200700110715     c                   movel     tipoC         WSSTIE
200800091006     C                   MOVE      'IA'          WSSTIP
200900091006     C                   Z-ADD     FIL(K)        WSSLIN
201000091006     C                   Z-ADD     WTSPIA_re     WSSTSP
201100091006     C                   Z-ADD     WTCLIA_re     WSSTCL
201200120904     C                   Z-ADD(H)  WTKGIA_re     WSSTKG_ex
201300120904     C                   Z-ADD(H)  WTMCIA_re     WSSTMC_ex
201400100402     c* Spedizioni "E"
201500100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
201600100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
201700120904     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe_ex
201800120904     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe_ex
201900100402     c* Spedizioni "H"
202000100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
202100100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
202200120904     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh_ex
202300120904     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh_ex
202400100402     c
202500091006     C                   WRITE     WSLSS550
202600091006     C                   ENDIF
202700100402     c
202800091006     C     WTSPEI_Re     IFGT      *ZERO
202900091006     C                   Z-ADD     §dal          WSSDAL
203000110715     c                   movel     tipoC         WSSTIE
203100091006     C                   MOVE      'EI'          WSSTIP
203200091006     C                   Z-ADD     FIL(K)        WSSLIN
203300091006     C                   Z-ADD     WTSPEI_re     WSSTSP
203400091006     C                   Z-ADD     WTCLEI_re     WSSTCL
203500120904     C                   Z-ADD(H)  WTKGEI_re     WSSTKG_ex
203600120904     C                   Z-ADD(H)  WTMCEI_re     WSSTMC_ex
203700100402     c* Spedizioni "E"
203800100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
203900100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
204000120904     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe_ex
204100120904     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe_ex
204200100402     c* Spedizioni "H"
204300100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
204400100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
204500120904     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh_ex
204600120904     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh_ex
204700100402     c
204800091006     C                   WRITE     WSLSS550
204900091006     C                   ENDIF
205000100402     c
205100091006     C     WTSPEE_re     IFGT      *ZERO
205200091006     C                   Z-ADD     §dal          WSSDAL
205300110715     c                   movel     tipoC         WSSTIE
205400091006     C                   MOVE      'EE'          WSSTIP
205500091006     C                   Z-ADD     FIL(K)        WSSLIN
205600091006     C                   Z-ADD     WTSPEE_re     WSSTSP
205700091006     C                   Z-ADD     WTCLEE_re     WSSTCL
205800120904     C                   Z-ADD(H)  WTKGEE_re     WSSTKG_ex
205900120904     C                   Z-ADD(H)  WTMCEE_re     WSSTMC_ex
206000100402     c* Spedizioni "E"
206100100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
206200100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
206300120904     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe_ex
206400120904     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe_ex
206500100402     c* Spedizioni "h"
206600100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
206700100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
206800120904     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh_ex
206900120904     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh_ex
207000100402     c
207100091006     C                   WRITE     WSLSS550
207200091006     C                   ENDIF
207300091006     C*
207400091006     C     WTSPED_re     IFGT      *ZERO
207500091006     C                   Z-ADD     §dal          WSSDAL
207600110715     c                   movel     tipoC         WSSTIE
207700091006     C                   MOVE      'ED'          WSSTIP
207800091006     C                   Z-ADD     FIL(K)        WSSLIN
207900091006     C                   Z-ADD     WTSPED_re     WSSTSP
208000091006     C                   Z-ADD     WTCLED_re     WSSTCL
208100120904     C                   Z-ADD(H)  WTKGED_re     WSSTKG_ex
208200120904     C                   Z-ADD(H)  WTMCED_re     WSSTMC_ex
208300100402     c* Spedizioni "E"
208400100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
208500100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
208600120904     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe_ex
208700120904     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe_ex
208800100402     c* Spedizioni "H"
208900100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
209000100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
209100120904     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh_ex
209200120904     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh_ex
209300100402     c
209400091006     C                   WRITE     WSLSS550
209500091006     C                   ENDIF
209600091006     C*
209700091006     C     WTSPEX_re     IFGT      *ZERO
209800091006     C                   Z-ADD     §dal          WSSDAL
209900110715     c                   movel     tipoC         WSSTIE
210000091006     C                   MOVE      'EX'          WSSTIP
210100091006     C                   Z-ADD     FIL(K)        WSSLIN
210200091006     C                   Z-ADD     WTSPEX_re     WSSTSP
210300091006     C                   Z-ADD     WTCLEX_re     WSSTCL
210400120904     C                   Z-ADD(H)  WTKGEX_re     WSSTKG_ex
210500120904     C                   Z-ADD(H)  WTMCEX_re     WSSTMC_ex
210600100402     c* Spedizioni "E"
210700100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
210800100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
210900120904     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe_ex
211000120904     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe_ex
211100100402     c* Spedizioni "H"
211200100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
211300100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
211400120904     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh_ex
211500120904     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh_ex
211600100402     c
211700091006     C                   WRITE     WSLSS550
211800091006     C                   ENDIF
211900091006     C*
212000091006     C     WTSPEA_re     IFGT      *ZERO
212100091006     C                   Z-ADD     §dal          WSSDAL
212200110715     c                   movel     tipoC         WSSTIE
212300091006     C                   MOVE      'EA'          WSSTIP
212400091006     C                   Z-ADD     FIL(K)        WSSLIN
212500091006     C                   Z-ADD     WTSPEA_re     WSSTSP
212600091006     C                   Z-ADD     WTCLEA_re     WSSTCL
212700120904     C                   Z-ADD(H)  WTKGEA_re     WSSTKG_ex
212800120904     C                   Z-ADD(H)  WTMCEA_re     WSSTMC_ex
212900100402     c* Spedizioni "E"
213000100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
213100100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
213200120904     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe_ex
213300120904     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe_ex
213400100402     c* Spedizioni "H"
213500100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
213600100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
213700120904     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh_ex
213800120904     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh_ex
213900100402     c
214000091006     C                   WRITE     WSLSS550
214100091006     C                   ENDIF
214200091006     C*
214300091006     C     WTSPPA_re     IFGT      *ZERO
214400091006     C                   Z-ADD     §dal          WSSDAL
214500110715     c                   movel     tipoC         WSSTIE
214600091006     C                   MOVE      'PA'          WSSTIP
214700091006     C                   Z-ADD     FIL(K)        WSSLIN
214800091006     C                   Z-ADD     WTSPPA_re     WSSTSP
214900091006     C                   Z-ADD     WTCLPA_re     WSSTCL
215000120904     C                   Z-ADD(H)  WTKGPA_re     WSSTKG_ex
215100120904     C                   Z-ADD(H)  WTMCPA_re     WSSTMC_ex
215200100402     c* Spedizioni "E"
215300100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
215400100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
215500120904     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe_ex
215600120904     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe_ex
215700100402     c* Spedizioni "H"
215800100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
215900100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
216000120904     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh_ex
216100120904     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh_ex
216200100402     c
216300091006     C                   WRITE     WSLSS550
216400091006     C                   ENDIF
216500091006     C                   ENDIF
216600100402
216700090206     c                   enddo
216800090206     c                   endsr
216900970527     C**********************************************************
217000970529     C** CARICAMENTO DATI INIZIALI
217100970527     C**********************************************************
217200970529     C     CARINZ        BEGSR
217300970905     C                   SETOFF                                       90
217400970529     C     KTAS          KLIST
217500970529     C                   KFLD                    WAAS
217600970529     C                   KFLD                    WMGS
217700970529     C     KTAB          KLIST
217800970529     C                   KFLD                    TBLKUT
217900970529     C                   KFLD                    TBLCOD
218000970529     C                   KFLD                    TBLKEY
218100970529     C     KTB           KLIST
218200970529     C                   KFLD                    TBLKUT
218300970529     C                   KFLD                    TBLCOD
218400970529     C                   Z-ADD     1             TBLKUT
218500090204     c     kwfsmp        klist
218600090204     c                   kfld                    k_fil
218700090204     c                   kfld                    k_tip
218800090204     c                   kfld                    k_dat
218900090205     C     kar5          KLIST
219000090205     C                   KFLD                    tasaas
219100090205     C                   KFLD                    taslnp
219200090205     C                   KFLD                    tasnrs
219300090205     C                   KFLD                    tasnsp
219400090205     C                   KFLD                    k_trd
219500090209     C                   KFLD                    tasflp
219600120903     c     kss61         klist
219700120903     c                   kfld                    ks_tie
219800120903     c                   kfld                    ks_tip
219900120903     c                   kfld                    ks_lin
220000120903     c                   kfld                    ks_sca
220100120903     c
220200090915     c* apertura file di stampa congelato in SAVE
220300090915     C* ESEGUO OVRPRTF
220400090915     C                   Z-ADD     61            LUNG             15 5
220500090915     C                   MOVEL     *BLANKS       COMMAN           80
220600090915     C                   MOVEA     CMD           COMMAN
220700090915     C                   CALL      'QCMDEXC'                            H1
220800090915     C                   PARM                    COMMAN
220900090915     C                   PARM                    LUNG
221000090915     C*
221100090915     C                   OPEN      QSYSPRT
221200090915     C*
221300970529     C*
221400970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
221500970527     C** PULIZIA DS
221600980826     C                   DO        400           K                 3 0
221700970527     C     K             OCCUR     DSFIL
221800970527     C                   CLEAR                   DSFIL
221900970527     C                   END
222000100402     C                   DO        400           K                 3 0
222100100402     C     K             OCCUR     DSFIL_e
222200100402     C                   CLEAR                   DSFIL_e
222300100402     C                   END
222400100402     C                   DO        400           K                 3 0
222500100402     C     K             OCCUR     DSFIL_h
222600100402     C                   CLEAR                   DSFIL_h
222700100402     C                   END
222800090206     C                   DO        400           K                 3 0
222900090206     C     K             OCCUR     DSFIL_smp
223000090206     C                   CLEAR                   DSFIL_smp
223100090206     C                   END
223200970527     C*
223300970527     C                   Z-ADD     0             K
223400970527     C     *LOVAL        SETLL     AZORG01L
223500970527     C                   DO        *HIVAL
223600090204     C                   READ      AZORG01L
223700090204     c                   if        %eof(azorg01l)
223800090204     c                   leave
223900090204     c                   endif
224000970527     C     ORGFAG        IFEQ      'F'
224100970527     C     ORGFAG        OREQ      'A'
224200970527     C                   ADD       1             K
224300970527     C                   MOVEL     ORGFIL        FIL(K)
224400090204     C                   MOVEL     ORGdes        DIL(K)
224500090204     C                   z-add     ORGcar        car(K)
224600090204     c                   movel     orgde8        og148
224700090206     c                   if        §oglpo='1' or §oglpo='2'
224800090206     c                   move      §oglpo        lpo(K)
224900090206     c                   else
225000090206     c                   z-add     2             lpo(K)
225100090206     c                   endif
225200090204     c                   movel     orgde3        og143
225300090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
225400090209     c**                 move      'E'           tif(K)
225500090209     c**                 else
225600090209     c**                 move      'I'           tif(K)
225700090209     c**                 endif
225800090209     c                   select
225900090209     c                   when      §ogntw='DPD'
226000090209     c                   move      'D'           tif(K)
226100090209     c                   when      §ogntw='EEX'
226200090209     c                   move      'X'           tif(K)
226300090209     c                   when      §ogntw='FED'
226400090209     c                   move      'F'           tif(K)
226500090209     c                   other
226600090209     c                   move      'I'           tif(K)
226700090209     c                   endsl
226800970527     C                   ENDIF
226900090204     C                   ENDDO
227000970529     C*
227100970529     C                   Z-ADD     0             K
227200970529     C                   MOVEL     'TB'          TBLCOD
227300970529     C     KTB           SETLL     TABEL00F
227400970529     C                   DO        *HIVAL
227500090204     C     KTB           READE     TABEL00F
227600090204     c                   if        %eof(tabel00f)
227700090204     c                   leave
227800090204     c                   endif
227900970529     C     TBLFLG        IFNE      '*'
228000970529     C                   MOVEL     TBLUNI        DSTB
228100970529     C     §TBRBL        IFEQ      'N'
228200970529     C     §TBRBL        OREQ      'S'
228300970529     C                   ADD       1             K
228400970529     C                   MOVEL     TBLKEY        TBO(K)
228500970529     C                   ENDIF
228600970529     C                   ENDIF
228700090204     C                   ENDDO
228800120903     c*
228900120903     c* Carico scaglioni di peso
229000120903     C                   Z-ADD     0             II
229100120903     C                   MOVEL     '2L'          TBLCOD
229200120903     C     KTB           SETLL     TABEL00F
229300120903    1C                   DO        *HIVAL
229400120903     C     KTB           READE     TABEL00F
229500120903    2c                   if        %eof(tabel00f)
229600120903     c                   leave
229700120903    2c                   endif
229800120903    2C     TBLFLG        IFNE      '*'
229900120903     C                   MOVEL     TBLUNI        DS2l
230000120903     C                   MOVEL     TBLKEY        WTM03             3
230100120904     C                   MOVEL     TBLKEY        WTM05             5
230200120903    3C     WTM03         IFEQ      'S9R'
230300120903     C                   ADD       1             II
230400120903     C                   MOVE      WTM05         FPR(II)
230500120903     C                   Z-ADD     §2LKGI        KIR(II)
230600120903     C                   Z-ADD     §2LKGF        KFR(II)
230700120903    3C                   ENDIF
230800120903    2C                   ENDIF
230900120903     C                   ENDDO
231000091006     C*
231100091006     c* Lettura TABELLA clienti extra budget
231200110711     c                   if        §stat=' '
231300091006     C                   Z-ADD     0             K
231400091006     C                   MOVEL     'CXB'         TBecod
231500091006     C     tbecod        SETLL     tntbe01l
231600091006     C                   DO        *HIVAL
231700091006     C     tbecod        READE     tntbe01l
231800091006     c                   if        %eof(tntbe01l)
231900091006     c                   leave
232000091006     c                   endif
232100091006
232200091006     C     tbeatb        IFeq      ' '
232300091006     C                   MOVEL     TBeuni        Dcxb
232400091006     C                   ADD       1             K
232500091006     C                   MOVEL     TBeKE1        ksc_ex(k)
232600091007     C                   MOVE      §cxbctr       ksc_ex(k)
232700091006     C                   ENDIF
232800091006     C                   ENDDO
232900110711     c
233000110711     c                   else
233100110711     C                   Z-ADD     0             K
233200110711     C                   MOVEL     'CXQ'         TBecod
233300110711     C     tbecod        SETLL     tntbe01l
233400110711     C                   DO        *HIVAL
233500110711     C     tbecod        READE     tntbe01l
233600110711     c                   if        %eof(tntbe01l)
233700110711     c                   leave
233800110711     c                   endif
233900110711
234000110711     C     tbeatb        IFeq      ' '
234100110711     C                   ADD       1             K
234200110711     C                   MOVEL     TBeKE2        ksc_ex(k)
234300110711     C                   ENDIF
234400110711     C                   ENDDO
234500110711     c                   endif
234600110711     c*
234700110711     c* Controllo riempimento schiera
234800110715     c                   movel     kpjbu         savkpjbu
234900110711     c                   clear                   trul0sds
235000110711     c                   eval      i0stla='L'
235100110711     c                   eval      i0sski='KSC_EX'
235200110711     c                   eval      i0sele=%elem(ksc_ex)
235300110711     c                   eval      i0spie=k
235400110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
235500110711     c                   eval      i0ssif=knsif
235600110711     c                   eval      i0spgm='TNSSL3R2'
235700110711     c                   movel     trul0sds      kpjbu
235800110711     c                   call      'TRUL0SR'
235900110711     c                   parm                    kpjba
236000110715
236100110715     c                   movel     savkpjbu      kpjbu
236200090601     c
236300090204     c* data elaborazione nel formato AAAA/MM/GG
236400090204     c                   time                    dataiso
236500090204     c                   move      dataiso       wdatael
236600110711     c
236700110711     c                   if        §stat=' '
236800090601     c*
236900090601     c* Pulisco i file per i dati che devo rielaborare
237000090601     c                   eval      wrkgetlista =
237100120905     c                             'delete from WFLSS60F where wsstie= '''   +
237200090603     c                              Wtipo +  ''''
237300090601     c                   if        Wtipo='R'
237400090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
237500090603     c                              ' or  wsstie=''G'''
237600090601     c                   endif
237700090609     c*
237800090601     C/EXEC SQL
237900090601     C+ execute immediate :wrkgetlista
238000090601     C/END-EXEC
238100090609     c
238200120904     c*
238300120904     c* Pulisco i file per i dati che devo rielaborare
238400120904     c                   eval      wrkgetlista =
238500120904     c                             'delete from wslss50F where wsstie= '''   +
238600120904     c                              Wtipo +  ''''
238700120904     c                   if        Wtipo='R'
238800120904     c                   eval      wrkgetlista =%trim(wrkgetlista)+
238900120904     c                              ' or  wsstie=''G'''
239000120904     c                   endif
239100120904     c*
239200120904     C/EXEC SQL
239300120904     C+ execute immediate :wrkgetlista
239400120904     C/END-EXEC
239500120904     c*
239600090609     c* Pulisco i file per i dati che devo rielaborare
239700090609     c                   clear                   wrkgetlista
239800090609     c                   eval      wrkgetlista =
239900090609     c                             'delete from WFSMP00F where wmptip= '''   +
240000090609     c                              Wtipo +  ''''
240100090609     c                   if        Wtipo='R'
240200090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
240300090609     c                              ' or  wmptip=''G'''
240400090609     c                   endif
240500110711     c
240600090609     c*
240700090609     C/EXEC SQL
240800090609     C+ execute immediate :wrkgetlista
240900090609     C/END-EXEC
241000120904     c                   endif
241100091007     c*
241200091007     c* Pulisco il file clienti extra budget
241300091007     c                   clear                   wrkgetlista
241400110711     c
241500110711     c                   if        §stat=' '
241600091007     c                   eval      wrkgetlista =
241700120903     c                             'delete from wslss55F where wsstie= '''   +
241800091007     c                              Wtipo +  ''''
241900091007     c                   if        Wtipo='R'
242000091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
242100091007     c                              ' or  wsstie=''G'''
242200091007     c                   endif
242300111028     C/EXEC SQL
242400111028     C+ execute immediate :wrkgetlista
242500111028     C/END-EXEC
242600110711     c
242700110711     c                   else
242800111028     c
242900111028     c* Pulisco il file clienti particolari
243000110711     c                   select
243100110711     c                   when      Wtipo='R'
243200110711     c                   eval      wrkgetlista =
243300110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
243400110711     c                               wsstie= ''B'''
243500110711     c                   when      Wtipo='G'
243600110711     c                   eval      wrkgetlista =
243700110711     c                             'delete from WSLSS55F where wsstie= ''A'''
243800110711     c                   when      Wtipo='P'
243900110711     c                   eval      wrkgetlista =
244000110711     c                             'delete from WSLSS55F where wsstie= ''C'''
244100110711     c                   endsl
244200110711     c
244300091007     C/EXEC SQL
244400091007     C+ execute immediate :wrkgetlista
244500091007     C/END-EXEC
244600111028     c
244700111028     c* Preparo la lettura dei soli clienti particolari con SQL
244800111028     c                   clear                   wrkgetlista
244900111028     c                   eval      wrkgetlista =
245000111028     c                             'select * from titas00f where +
245100111028     c                             ((tasaas*10000) +tasmgs) >=' +
245200111028     c                             %editc(§dal:'X')+ ' and +
245300111028     c                             ((tasaas*10000) +tasmgs) <=' +
245400111028     c                             %editc(§al :'X')+ ' and tasccm in('
245500111028      * ciclo sulla sk dei cod bolla di recupero
245600111028     c                   Do        500           XX                3 0
245700111028     c                   If        ksc_ex(XX) = *blanks
245800111028     c                   Leave
245900111028     c                   EndIf
246000111028     c
246100111028     c                   If        XX >1
246200111028     c                   eval      wrkgetlista =
246300111028     c                             %trim(wrkgetlista) + ','
246400111028     c                   EndIf
246500111028     c
246600111028     c                   eval      wrkgetlista =
246700111028     c                             %trim(wrkgetlista) + ' ''' +
246800111028     c                             %subst(ksc_ex(xx):1:7) + ''''
246900111028     c                   EndDo
247000111028     c
247100111028     c                   Eval      wrkgetlista =
247200111028     c                             %trim(wrkgetlista) + ')'
247300111028     c
247400111028     c
247500111028     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
247600111028     c                             'select * from titas10f where +
247700111028     c                             ((tasaas*10000) +tasmgs) >=' +
247800111028     c                             %editc(§dal:'X')+ ' and +
247900111028     c                             ((tasaas*10000) +tasmgs) <=' +
248000111028     c                             %editc(§al :'X')+ ' and tasccm in('
248100111028      * ciclo sulla sk dei cod bolla di recupero
248200111028     c                   Do        500           XX                3 0
248300111028     c                   If        ksc_ex(XX) = *blanks
248400111028     c                   Leave
248500111028     c                   EndIf
248600111028     c
248700111028     c                   If        XX >1
248800111028     c                   eval      wrkgetlista =
248900111028     c                             %trim(wrkgetlista) + ','
249000111028     c                   EndIf
249100111028     c
249200111028     c                   eval      wrkgetlista =
249300111028     c                             %trim(wrkgetlista) + ' ''' +
249400111028     c                             %subst(ksc_ex(xx):1:7) + ''''
249500111028     c                   EndDo
249600111028     c
249700111028     c                   Eval      wrkgetlista =
249800111028     c                             %trim(wrkgetlista) + ')'
249900111028     c
250000111028     c                   EndIf
250100111028     c
250200970527     C                   ENDSR
250300100402     c****************************************************************************
250400100402     c     SpeITA        BEGSR
250500100402     c* giornaliero
250600100402     c                   if        (tasaas*10000+tasmgs)=§al
250700100402     C                   ADD       1             WTSPIP
250800100402     C                   ADD       TASNCL        WTCLIP
250900100402     C                   ADD       w_peso        WTKGIP
251000100402     C                   ADD       TASVLF        WTMCIP
251100100402     c* tipo servizio  "E"
251200100402     c                   if        tastsp='E'
251300100402     C                   ADD       1             eWTSPIP
251400100402     C                   ADD       TASNCL        eWTCLIP
251500100402     C                   ADD       w_peso        eWTKGIP
251600100402     C                   ADD       TASVLF        eWTMCIP
251700100402     c                   endif
251800100402     c* tipo servizio  "H"
251900100402     c                   if        tastsp='H'
252000100402     C                   ADD       1             hWTSPIP
252100100402     C                   ADD       TASNCL        hWTCLIP
252200100402     C                   ADD       w_peso        hWTKGIP
252300100402     C                   ADD       TASVLF        hWTMCIP
252400100402     c                   endif
252500100402     c**
252600100402     c* Extra budget
252700100402     c                   if        *in35
252800100402     C                   ADD       1             WTSPIP_e
252900100402     C                   ADD       TASNCL        WTCLIP_e
253000100402     C                   ADD       w_peso        WTKGIP_e
253100100402     C                   ADD       TASVLF        WTMCIP_e
253200100402     c* tipo servizio  "E"
253300100402     c                   if        tastsp='E'
253400100402     C                   ADD       1             eWTSPIP_e
253500100402     C                   ADD       TASNCL        eWTCLIP_e
253600100402     C                   ADD       w_peso        eWTKGIP_e
253700100402     C                   ADD       TASVLF        eWTMCIP_e
253800100402     c                   endif
253900100402     c* tipo servizio  "H"
254000100402     c                   if        tastsp='H'
254100100402     C                   ADD       1             hWTSPIP_e
254200100402     C                   ADD       TASNCL        hWTCLIP_e
254300100402     C                   ADD       w_peso        hWTKGIP_e
254400100402     C                   ADD       TASVLF        hWTMCIP_e
254500100402     c                   endif
254600100402
254700100402     c                   endif
254800100402     c                   endif
254900100402     c* riepilogo
255000100402     C                   ADD       1             WTSPIP_r
255100100402     C                   ADD       TASNCL        WTCLIP_r
255200100402     C                   ADD       w_peso        WTKGIP_r
255300100402     C                   ADD       TASVLF        WTMCIP_r
255400100402     c* tipo servizio  "E"
255500100402     c                   if        tastsp='E'
255600100402     C                   ADD       1             eWTSPIP_r
255700100402     C                   ADD       TASNCL        eWTCLIP_r
255800100402     C                   ADD       w_peso        eWTKGIP_r
255900100402     C                   ADD       TASVLF        eWTMCIP_r
256000100402     c                   endif
256100100402     c* tipo servizio  "H"
256200100402     c                   if        tastsp='H'
256300100402     C                   ADD       1             hWTSPIP_r
256400100402     C                   ADD       TASNCL        hWTCLIP_r
256500100402     C                   ADD       w_peso        hWTKGIP_r
256600100402     C                   ADD       TASVLF        hWTMCIP_r
256700100402     c                   endif
256800100402     c* Extra budget
256900100402     c                   if        *in35
257000100402     C                   ADD       1             WTSPIP_re
257100100402     C                   ADD       TASNCL        WTCLIP_re
257200100402     C                   ADD       w_peso        WTKGIP_re
257300100402     C                   ADD       TASVLF        WTMCIP_re
257400100402     c* tipo servizio  "E"
257500100402     c                   if        tastsp='E'
257600100402     C                   ADD       1             eWTSPIP_re
257700100402     C                   ADD       TASNCL        eWTCLIP_re
257800100402     C                   ADD       w_peso        eWTKGIP_re
257900100402     C                   ADD       TASVLF        eWTMCIP_re
258000100402     c                   endif
258100100402     c* tipo servizio  "H"
258200100402     c                   if        tastsp='H'
258300100402     C                   ADD       1             hWTSPIP_re
258400100402     C                   ADD       TASNCL        hWTCLIP_re
258500100402     C                   ADD       w_peso        hWTKGIP_re
258600100402     C                   ADD       TASVLF        hWTMCIP_re
258700100402     c                   endif
258800100402     c
258900100402     c                   endif
259000120903     c
259100120903     c                   eval      ks_tip='IP'
259200120903     c                   exsr      spe_peso
259300120903     c
259400100402     c                   ENDSR
259500100402     c****************************************************************************
259600100402     c     SpeIMPORT     BEGSR
259700100402     c* giornaliero
259800100402     c                   if        (tasaas*10000+tasmgs)=§al
259900100402     C                   ADD       1             WTSPEI
260000100402     C                   ADD       TASNCL        WTCLEI
260100100402     C                   ADD       w_peso        WTKGEI
260200100402     C                   ADD       TASVLF        WTMCEI
260300100402     c* tipo servizio  "E"
260400100402     c                   if        tastsp='E'
260500100402     C                   ADD       1             eWTSPEI
260600100402     C                   ADD       TASNCL        eWTCLEI
260700100402     C                   ADD       w_peso        eWTKGEI
260800100402     C                   ADD       TASVLF        eWTMCEI
260900100402     c                   endif
261000100402     c* tipo servizio  "H"
261100100402     c                   if        tastsp='H'
261200100402     C                   ADD       1             hWTSPEI
261300100402     C                   ADD       TASNCL        hWTCLEI
261400100402     C                   ADD       w_peso        hWTKGEI
261500100402     C                   ADD       TASVLF        hWTMCEI
261600100402     c                   endif
261700100402     c* Extra Budget
261800100402     c                   if        *in35
261900100402     C                   ADD       1             WTSPEI_e
262000100402     C                   ADD       TASNCL        WTCLEI_e
262100100402     C                   ADD       w_peso        WTKGEI_e
262200100402     C                   ADD       TASVLF        WTMCEI_e
262300100402     c* tipo servizio  "E"
262400100402     c                   if        tastsp='E'
262500100402     C                   ADD       1             eWTSPEI_e
262600100402     C                   ADD       TASNCL        eWTCLEI_e
262700100402     C                   ADD       w_peso        eWTKGEI_e
262800100402     C                   ADD       TASVLF        eWTMCEI_e
262900100402     c                   endif
263000100402     c* tipo servizio  "H"
263100100402     c                   if        tastsp='H'
263200100402     C                   ADD       1             hWTSPEI_e
263300100402     C                   ADD       TASNCL        hWTCLEI_e
263400100402     C                   ADD       w_peso        hWTKGEI_e
263500100402     C                   ADD       TASVLF        hWTMCEI_e
263600100402     c                   endif
263700100402     c
263800100402     c                   endif
263900100402     c                   endif
264000100402     c* riepilogo
264100100402     C                   ADD       1             WTSPEI_r
264200100402     C                   ADD       TASNCL        WTCLEI_r
264300100402     C                   ADD       w_peso        WTKGEI_r
264400100402     C                   ADD       TASVLF        WTMCEI_r
264500100402     c* tipo servizio  "E"
264600100402     c                   if        tastsp='E'
264700100402     C                   ADD       1             eWTSPEI_r
264800100402     C                   ADD       TASNCL        eWTCLEI_r
264900100402     C                   ADD       w_peso        eWTKGEI_r
265000100402     C                   ADD       TASVLF        eWTMCEI_r
265100100402     c                   endif
265200100402     c* tipo servizio  "H"
265300100402     c                   if        tastsp='H'
265400100402     C                   ADD       1             hWTSPEI_r
265500100402     C                   ADD       TASNCL        hWTCLEI_r
265600100402     C                   ADD       w_peso        hWTKGEI_r
265700100402     C                   ADD       TASVLF        hWTMCEI_r
265800100402     c                   endif
265900100402     c* extra budget
266000100402     c                   if        *in35
266100100402     C                   ADD       1             WTSPEI_re
266200100402     C                   ADD       TASNCL        WTCLEI_re
266300100402     C                   ADD       w_peso        WTKGEI_re
266400100402     C                   ADD       TASVLF        WTMCEI_re
266500100402     c* tipo servizio  "E"
266600100402     c                   if        tastsp='E'
266700100402     C                   ADD       1             eWTSPEI_re
266800100402     C                   ADD       TASNCL        eWTCLEI_re
266900100402     C                   ADD       w_peso        eWTKGEI_re
267000100402     C                   ADD       TASVLF        eWTMCEI_re
267100100402     c                   endif
267200100402     c* tipo servizio  "H"
267300100402     c                   if        tastsp='H'
267400100402     C                   ADD       1             hWTSPEI_re
267500100402     C                   ADD       TASNCL        hWTCLEI_re
267600100402     C                   ADD       w_peso        hWTKGEI_re
267700100402     C                   ADD       TASVLF        hWTMCEI_re
267800100402     c                   endif
267900100402     c                   endif
268000120904     c
268100120904     c                   eval      ks_tip='EI'
268200120904     c                   exsr      spe_peso
268300120904     c
268400100402     c                   ENDSR
268500100402     c****************************************************************************
268600100402     c     speEXPRESS    BEGSR
268700100402     c* giornaliero
268800100402     c                   if        (tasaas*10000+tasmgs)=§al
268900100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
269000100402     C                   ADD       TASNCL        WTCLEE
269100100402     C                   ADD       w_peso        WTKGEE
269200100402     C                   ADD       TASVLF        WTMCEE
269300100402     c* tipo servizio  "E"
269400100402     c                   if        tastsp='E'
269500100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
269600100402     C                   ADD       TASNCL        eWTCLEE
269700100402     C                   ADD       w_peso        eWTKGEE
269800100402     C                   ADD       TASVLF        eWTMCEE
269900100402     c                   endif
270000100402     c* tipo servizio  "H"
270100100402     c                   if        tastsp='H'
270200100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
270300100402     C                   ADD       TASNCL        hWTCLEE
270400100402     C                   ADD       w_peso        hWTKGEE
270500100402     C                   ADD       TASVLF        hWTMCEE
270600100402     c                   endif
270700100402     c* extra budget
270800100402     c                   if        *in35
270900100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
271000100402     C                   ADD       TASNCL        WTCLEE_e
271100100402     C                   ADD       w_peso        WTKGEE_e
271200100402     C                   ADD       TASVLF        WTMCEE_e
271300100402     c* tipo servizio  "E"
271400100402     c                   if        tastsp='E'
271500100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
271600100402     C                   ADD       TASNCL        eWTCLEE_e
271700100402     C                   ADD       w_peso        eWTKGEE_e
271800100402     C                   ADD       TASVLF        eWTMCEE_e
271900100402     c                   endif
272000100402     c* tipo servizio  "H"
272100100402     c                   if        tastsp='H'
272200100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
272300100402     C                   ADD       TASNCL        hWTCLEE_e
272400100402     C                   ADD       w_peso        hWTKGEE_e
272500100402     C                   ADD       TASVLF        hWTMCEE_e
272600100402     c                   endif
272700100402     c
272800100402     c                   endif
272900100402     c                   endif
273000100402     c* riepilogo
273100100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
273200100402     C                   ADD       TASNCL        WTCLEE_r
273300100402     C                   ADD       w_peso        WTKGEE_r
273400100402     C                   ADD       TASVLF        WTMCEE_r
273500100402     c* tipo servizio  "E"
273600100402     c                   if        tastsp='E'
273700100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
273800100402     C                   ADD       TASNCL        eWTCLEE_r
273900100402     C                   ADD       w_peso        eWTKGEE_r
274000100402     C                   ADD       TASVLF        eWTMCEE_r
274100100402     c                   endif
274200100402     c* tipo servizio  "H"
274300100402     c                   if        tastsp='H'
274400100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
274500100402     C                   ADD       TASNCL        hWTCLEE_r
274600100402     C                   ADD       w_peso        hWTKGEE_r
274700100402     C                   ADD       TASVLF        hWTMCEE_r
274800100402     c                   endif
274900100402     c* extra budget
275000100402     c                   if        *in35
275100100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
275200100402     C                   ADD       TASNCL        WTCLEE_re
275300100402     C                   ADD       w_peso        WTKGEE_re
275400100402     C                   ADD       TASVLF        WTMCEE_re
275500100402     c* tipo servizio  "E"
275600100402     c                   if        tastsp='E'
275700100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
275800100402     C                   ADD       TASNCL        eWTCLEE_re
275900100402     C                   ADD       w_peso        eWTKGEE_re
276000100402     C                   ADD       TASVLF        eWTMCEE_re
276100100402     c                   endif
276200100402     c* tipo servizio  "H"
276300100402     c                   if        tastsp='H'
276400100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
276500100402     C                   ADD       TASNCL        hWTCLEE_re
276600100402     C                   ADD       w_peso        hWTKGEE_re
276700100402     C                   ADD       TASVLF        hWTMCEE_re
276800100402     c                   endif
276900100402     c                   endif
277000120904     c
277100120904     c                   eval      ks_tip='EE'
277200120904     c                   exsr      spe_peso
277300100402     c
277400100402     c                   ENDSR
277500100402     c****************************************************************************
277600100402     c     speDPD        BEGSR
277700100402     c* giornaliero
277800100402     c                   if        (tasaas*10000+tasmgs)=§al
277900100402     C                   ADD       1             WTSPED                         DPD
278000100402     C                   ADD       TASNCL        WTCLED
278100100402     C                   ADD       w_peso        WTKGED
278200100402     C                   ADD       TASVLF        WTMCED
278300100402     c* tipo servizio  "E"
278400100402     c                   if        tastsp='E'
278500100402     C                   ADD       1             eWTSPED                         DPD
278600100402     C                   ADD       TASNCL        eWTCLED
278700100402     C                   ADD       w_peso        eWTKGED
278800100402     C                   ADD       TASVLF        eWTMCED
278900100402     c                   endif
279000100402     c* tipo servizio  "H"
279100100402     c                   if        tastsp='H'
279200100402     C                   ADD       1             hWTSPED                         DPD
279300100402     C                   ADD       TASNCL        hWTCLED
279400100402     C                   ADD       w_peso        hWTKGED
279500100402     C                   ADD       TASVLF        hWTMCED
279600100402     c                   endif
279700100402     c* extra budget
279800100402     c                   if        *in35
279900100402     C                   ADD       1             WTSPED_e                       DPD
280000100402     C                   ADD       TASNCL        WTCLED_e
280100100402     C                   ADD       w_peso        WTKGED_e
280200100402     C                   ADD       TASVLF        WTMCED_e
280300100402     c* tipo servizio  "E"
280400100402     c                   if        tastsp='E'
280500100402     C                   ADD       1             eWTSPED_e                       DPD
280600100402     C                   ADD       TASNCL        eWTCLED_e
280700100402     C                   ADD       w_peso        eWTKGED_e
280800100402     C                   ADD       TASVLF        eWTMCED_e
280900100402     c                   endif
281000100402     c* tipo servizio  "H"
281100100402     c                   if        tastsp='H'
281200100402     C                   ADD       1             hWTSPED_e                       DPD
281300100402     C                   ADD       TASNCL        hWTCLED_e
281400100402     C                   ADD       w_peso        hWTKGED_e
281500100402     C                   ADD       TASVLF        hWTMCED_e
281600100402     c                   endif
281700100402     c
281800100402     c                   endif
281900100402     c                   endif
282000100402     c* riepilogo
282100100402     C                   ADD       1             WTSPED_r                       DPD
282200100402     C                   ADD       TASNCL        WTCLED_r
282300100402     C                   ADD       w_peso        WTKGED_r
282400100402     C                   ADD       TASVLF        WTMCED_r
282500100402     c* tipo servizio  "E"
282600100402     c                   if        tastsp='E'
282700100402     C                   ADD       1             eWTSPED_r                       DPD
282800100402     C                   ADD       TASNCL        eWTCLED_r
282900100402     C                   ADD       w_peso        eWTKGED_r
283000100402     C                   ADD       TASVLF        eWTMCED_r
283100100402     c                   endif
283200100402     c* tipo servizio  "H"
283300100402     c                   if        tastsp='H'
283400100402     C                   ADD       1             hWTSPED_r                       DPD
283500100402     C                   ADD       TASNCL        hWTCLED_r
283600100402     C                   ADD       w_peso        hWTKGED_r
283700100402     C                   ADD       TASVLF        hWTMCED_r
283800100402     c                   endif
283900100402     c* extra budget
284000100402     c                   if        *in35
284100100402     C                   ADD       1             WTSPED_re                       DPD
284200100402     C                   ADD       TASNCL        WTCLED_re
284300100402     C                   ADD       w_peso        WTKGED_re
284400100402     C                   ADD       TASVLF        WTMCED_re
284500100402     c* tipo servizio  "E"
284600100402     c                   if        tastsp='E'
284700100402     C                   ADD       1             eWTSPED_re                       DPD
284800100402     C                   ADD       TASNCL        eWTCLED_re
284900100402     C                   ADD       w_peso        eWTKGED_re
285000100402     C                   ADD       TASVLF        eWTMCED_re
285100100402     c                   endif
285200100402     c* tipo servizio  "H"
285300100402     c                   if        tastsp='H'
285400100402     C                   ADD       1             hWTSPED_re                       DPD
285500100402     C                   ADD       TASNCL        hWTCLED_re
285600100402     C                   ADD       w_peso        hWTKGED_re
285700100402     C                   ADD       TASVLF        hWTMCED_re
285800100402     c                   endif
285900100402     c                   endif
286000120904     c
286100120904     c                   eval      ks_tip='ED'
286200120904     c                   exsr      spe_peso
286300120904     c
286400100402     c                   ENDSR
286500100402     c****************************************************************************
286600100402     c     speFEDEX      BEGSR
286700100402     c* giornaliero
286800100402     c                   if        (tasaas*10000+tasmgs)=§al
286900100402     C                   ADD       1             WTSPEX                         FEDEX
287000100402     C                   ADD       TASNCL        WTCLEX
287100100402     C                   ADD       w_peso        WTKGEX
287200100402     C                   ADD       TASVLF        WTMCEX
287300100402     c* tipo servizio  "E"
287400100402     c                   if        tastsp='E'
287500100402     C                   ADD       1             eWTSPEX                         FEDEX
287600100402     C                   ADD       TASNCL        eWTCLEX
287700100402     C                   ADD       w_peso        eWTKGEX
287800100402     C                   ADD       TASVLF        eWTMCEX
287900100402     c                   endif
288000100402     c* tipo servizio  "H"
288100100402     c                   if        tastsp='H'
288200100402     C                   ADD       1             hWTSPEX                         FEDEX
288300100402     C                   ADD       TASNCL        hWTCLEX
288400100402     C                   ADD       w_peso        hWTKGEX
288500100402     C                   ADD       TASVLF        hWTMCEX
288600100402     c                   endif
288700100402     c* extra budget
288800100402     c                   if        *in35
288900100402     C                   ADD       1             WTSPEX_e                       FEDEX
289000100402     C                   ADD       TASNCL        WTCLEX_e
289100100402     C                   ADD       w_peso        WTKGEX_e
289200100402     C                   ADD       TASVLF        WTMCEX_e
289300100402     c* tipo servizio  "E"
289400100402     c                   if        tastsp='E'
289500100402     C                   ADD       1             eWTSPEX_e                       FEDEX
289600100402     C                   ADD       TASNCL        eWTCLEX_e
289700100402     C                   ADD       w_peso        eWTKGEX_e
289800100402     C                   ADD       TASVLF        eWTMCEX_e
289900100402     c                   endif
290000100402     c* tipo servizio  "H"
290100100402     c                   if        tastsp='H'
290200100402     C                   ADD       1             hWTSPEX_e                       FEDEX
290300100402     C                   ADD       TASNCL        hWTCLEX_e
290400100402     C                   ADD       w_peso        hWTKGEX_e
290500100402     C                   ADD       TASVLF        hWTMCEX_e
290600100402     c                   endif
290700100402     c                   endif
290800100402     c                   endif
290900100402     c* riepilogo
291000100402     C                   ADD       1             WTSPEX_r                       FEDEX
291100100402     C                   ADD       TASNCL        WTCLEX_r
291200100402     C                   ADD       w_peso        WTKGEX_r
291300100402     C                   ADD       TASVLF        WTMCEX_r
291400100402     c* tipo servizio  "E"
291500100402     c                   if        tastsp='E'
291600100402     C                   ADD       1             eWTSPEX_r                       FEDEX
291700100402     C                   ADD       TASNCL        eWTCLEX_r
291800100402     C                   ADD       w_peso        eWTKGEX_r
291900100402     C                   ADD       TASVLF        eWTMCEX_r
292000100402     c                   endif
292100100402     c* tipo servizio  "H"
292200100402     c                   if        tastsp='H'
292300100402     C                   ADD       1             hWTSPEX_r                       FEDEX
292400100402     C                   ADD       TASNCL        hWTCLEX_r
292500100402     C                   ADD       w_peso        hWTKGEX_r
292600100402     C                   ADD       TASVLF        hWTMCEX_r
292700100402     c                   endif
292800100402     c* Extra budget
292900100402     c                   if        *in35
293000100402     C                   ADD       1             WTSPEX_re                       FEDEX
293100100402     C                   ADD       TASNCL        WTCLEX_re
293200100402     C                   ADD       w_peso        WTKGEX_re
293300100402     C                   ADD       TASVLF        WTMCEX_re
293400100402     c* tipo servizio  "E"
293500100402     c                   if        tastsp='E'
293600100402     C                   ADD       1             eWTSPEX_re                       FEDEX
293700100402     C                   ADD       TASNCL        eWTCLEX_re
293800100402     C                   ADD       w_peso        eWTKGEX_re
293900100402     C                   ADD       TASVLF        eWTMCEX_re
294000100402     c                   endif
294100100402     c* tipo servizio  "H"
294200100402     c                   if        tastsp='H'
294300100402     C                   ADD       1             hWTSPEX_re                       FEDEX
294400100402     C                   ADD       TASNCL        hWTCLEX_re
294500100402     C                   ADD       w_peso        hWTKGEX_re
294600100402     C                   ADD       TASVLF        hWTMCEX_re
294700100402     c                   endif
294800100402     c
294900100402     c                   endif
295000120904     c
295100120904     c                   eval      ks_tip='EX'
295200120904     c                   exsr      spe_peso
295300120904     c
295400100402     c                   ENDSR
295500100402     c****************************************************************************
295600100402     c     speITA_LNA    BEGSR
295700100402     c* giornaliero
295800100402     c                   if        (tasaas*10000+tasmgs)=§al
295900100402     C                   ADD       1             WTSPIA
296000100402     C                   ADD       TASNCL        WTCLIA
296100100402     C                   ADD       W_peso        WTKGIA
296200100402     C                   ADD       TASVLF        WTMCIA
296300100402     c* tipo servizio  "E"
296400100402     c                   if        tastsp='E'
296500100402     C                   ADD       1             eWTSPIA
296600100402     C                   ADD       TASNCL        eWTCLIA
296700100402     C                   ADD       W_peso        eWTKGIA
296800100402     C                   ADD       TASVLF        eWTMCIA
296900100402     c                   endif
297000100402     c* tipo servizio  "H"
297100100402     c                   if        tastsp='H'
297200100402     C                   ADD       1             hWTSPIA
297300100402     C                   ADD       TASNCL        hWTCLIA
297400100402     C                   ADD       W_peso        hWTKGIA
297500100402     C                   ADD       TASVLF        hWTMCIA
297600100402     c                   endif
297700100402     c* extra budget
297800100402     c                   if        *in35
297900100402     C                   ADD       1             WTSPIA_e
298000100402     C                   ADD       TASNCL        WTCLIA_e
298100100402     C                   ADD       w_peso        WTKGIA_e
298200100402     C                   ADD       TASVLF        WTMCIA_e
298300100402     c* tipo servizio  "E"
298400100402     c                   if        tastsp='E'
298500100402     C                   ADD       1             eWTSPIA_e
298600100402     C                   ADD       TASNCL        eWTCLIA_e
298700100402     C                   ADD       w_peso        eWTKGIA_e
298800100402     C                   ADD       TASVLF        eWTMCIA_e
298900100402     c                   endif
299000100402     c* tipo servizio  "H"
299100100402     c                   if        tastsp='H'
299200100402     C                   ADD       1             hWTSPIA_e
299300100402     C                   ADD       TASNCL        hWTCLIA_e
299400100402     C                   ADD       w_peso        hWTKGIA_e
299500100402     C                   ADD       TASVLF        hWTMCIA_e
299600100402     c                   endif
299700100402     c
299800100402     c                   endif
299900100402     c                   endif
300000100402     c* riepilogo
300100100402     C                   ADD       1             WTSPIA_r
300200100402     C                   ADD       TASNCL        WTCLIA_r
300300100402     C                   ADD       w_peso        WTKGIA_r
300400100402     C                   ADD       TASVLF        WTMCIA_r
300500100402     c* tipo servizio  "E"
300600100402     c                   if        tastsp='E'
300700100402     C                   ADD       1             eWTSPIA_r
300800100402     C                   ADD       TASNCL        eWTCLIA_r
300900100402     C                   ADD       w_peso        eWTKGIA_r
301000100402     C                   ADD       TASVLF        eWTMCIA_r
301100100402     c                   endif
301200100402     c* tipo servizio  "H"
301300100402     c                   if        tastsp='H'
301400100402     C                   ADD       1             hWTSPIA_r
301500100402     C                   ADD       TASNCL        hWTCLIA_r
301600100402     C                   ADD       w_peso        hWTKGIA_r
301700100402     C                   ADD       TASVLF        hWTMCIA_r
301800100402     c                   endif
301900100402     c* extra budget
302000100402     c                   if        *in35
302100100402     C                   ADD       1             WTSPIA_re
302200100402     C                   ADD       TASNCL        WTCLIA_re
302300100402     C                   ADD       w_peso        WTKGIA_re
302400100402     C                   ADD       TASVLF        WTMCIA_re
302500100402     c* tipo servizio  "E"
302600100402     c                   if        tastsp='E'
302700100402     C                   ADD       1             eWTSPIA_re
302800100402     C                   ADD       TASNCL        eWTCLIA_re
302900100402     C                   ADD       w_peso        eWTKGIA_re
303000100402     C                   ADD       TASVLF        eWTMCIA_re
303100100402     c                   endif
303200100402     c* tipo servizio  "H"
303300100402     c                   if        tastsp='H'
303400100402     C                   ADD       1             hWTSPIA_re
303500100402     C                   ADD       TASNCL        hWTCLIA_re
303600100402     C                   ADD       w_peso        hWTKGIA_re
303700100402     C                   ADD       TASVLF        hWTMCIA_re
303800100402     c                   endif
303900100402     c                   endif
304000120904     c
304100120904     c                   eval      ks_tip='IA'
304200120904     c                   exsr      spe_peso
304300120904     c
304400100402     c                   ENDSR
304500100402     c****************************************************************************
304600100402     c     speEXPORT     BEGSR
304700100402     c* giornaliero
304800100402     c                   if        (tasaas*10000+tasmgs)=§al
304900100402     C                   ADD       1             WTSPEA
305000100402     C                   ADD       TASNCL        WTCLEA
305100100402     C                   ADD       w_peso        WTKGEA
305200100402     C                   ADD       TASVLF        WTMCEA
305300100402     c* tipo servizio  "E"
305400100402     c                   if        tastsp='E'
305500100402     C                   ADD       1             eWTSPEA
305600100402     C                   ADD       TASNCL        eWTCLEA
305700100402     C                   ADD       w_peso        eWTKGEA
305800100402     C                   ADD       TASVLF        eWTMCEA
305900100402     c                   endif
306000100402     c* tipo servizio  "H"
306100100402     c                   if        tastsp='H'
306200100402     C                   ADD       1             hWTSPEA
306300100402     C                   ADD       TASNCL        hWTCLEA
306400100402     C                   ADD       w_peso        hWTKGEA
306500100402     C                   ADD       TASVLF        hWTMCEA
306600100402     c                   endif
306700100402     c* extra budget
306800100402     c                   if        *in35
306900100402     C                   ADD       1             WTSPEA_e
307000100402     C                   ADD       TASNCL        WTCLEA_e
307100100402     C                   ADD       w_peso        WTKGEA_e
307200100402     C                   ADD       TASVLF        WTMCEA_e
307300100402     c* tipo servizio  "E"
307400100402     c                   if        tastsp='E'
307500100402     C                   ADD       1             eWTSPEA_e
307600100402     C                   ADD       TASNCL        eWTCLEA_e
307700100402     C                   ADD       w_peso        eWTKGEA_e
307800100402     C                   ADD       TASVLF        eWTMCEA_e
307900100402     c                   endif
308000100402     c* tipo servizio  "H"
308100100402     c                   if        tastsp='H'
308200100402     C                   ADD       1             hWTSPEA_e
308300100402     C                   ADD       TASNCL        hWTCLEA_e
308400100402     C                   ADD       w_peso        hWTKGEA_e
308500100402     C                   ADD       TASVLF        hWTMCEA_e
308600100402     c                   endif
308700100402     c
308800100402     c                   endif
308900100402     c                   endif
309000100402     c* riepilogo
309100100402     C                   ADD       1             WTSPEA_r
309200100402     C                   ADD       TASNCL        WTCLEA_r
309300100402     C                   ADD       w_peso        WTKGEA_r
309400100402     C                   ADD       TASVLF        WTMCEA_r
309500100402     c* tipo servizio  "E"
309600100402     c                   if        tastsp='E'
309700100402     C                   ADD       1             eWTSPEA_r
309800100402     C                   ADD       TASNCL        eWTCLEA_r
309900100402     C                   ADD       w_peso        eWTKGEA_r
310000100402     C                   ADD       TASVLF        eWTMCEA_r
310100100402     c                   endif
310200100402     c* tipo servizio  "H"
310300100402     c                   if        tastsp='H'
310400100402     C                   ADD       1             hWTSPEA_r
310500100402     C                   ADD       TASNCL        hWTCLEA_r
310600100402     C                   ADD       w_peso        hWTKGEA_r
310700100402     C                   ADD       TASVLF        hWTMCEA_r
310800100402     c                   endif
310900100402     c* extra budget
311000100402     c                   if        *in35
311100100402     C                   ADD       1             WTSPEA_re
311200100402     C                   ADD       TASNCL        WTCLEA_re
311300100402     C                   ADD       w_peso        WTKGEA_re
311400100402     C                   ADD       TASVLF        WTMCEA_re
311500100402     c* tipo servizio  "E"
311600100402     c                   if        tastsp='E'
311700100402     C                   ADD       1             eWTSPEA_re
311800100402     C                   ADD       TASNCL        eWTCLEA_re
311900100402     C                   ADD       w_peso        eWTKGEA_re
312000100402     C                   ADD       TASVLF        eWTMCEA_re
312100100402     c                   endif
312200100402     c* tipo servizio  "H"
312300100402     c                   if        tastsp='H'
312400100402     C                   ADD       1             hWTSPEA_re
312500100402     C                   ADD       TASNCL        hWTCLEA_re
312600100402     C                   ADD       w_peso        hWTKGEA_re
312700100402     C                   ADD       TASVLF        hWTMCEA_re
312800100402     c                   endif
312900100402     c
313000100402     c                   endif
313100120904     c
313200120904     c                   eval      ks_tip='EA'
313300120904     c                   exsr      spe_peso
313400120904     c
313500100402     c                   ENDSR
313600120903     c****************************************************************************
313700120903     c     SPE_peso      BEGSR
313800120903     c
313900120905     c* scaglioni di peso solo se richiesto
314000120905    1c                   if        §stat=' ' and §opz='4'
314100120903     c
314200120903     c* in base al peso trovo lo scaglione e faccio subito la write
314300120903     c                   z-add     1             II
314400120903     c                   clear                   ks_sca
314500120903     c                   dow       ks_sca  =0
314600120903     C     W_peso        IFGT      KIR(Ii)
314700120903     C     W_peso        ANDLE     KFR(Ii)
314800120904     C                   MOVEL     KFR(Ii)       ks_sca
314900120903     C                   endif
315000120903     c                   add       1             II
315100120903     c                   enddo
315200120903     c
315300120903     c* la write/update per peso la faccio subito
315400120903     c
315500120903    2c                   if        Wtipo='G' or Wtipo='R'
315600120903     c
315700120903    3c                   if        (tasaas*10000+tasmgs)=§al
315800120903     c                   eval      ks_tie='G'
315900120903     c                   eval      ks_lin=fil(k)
316000120905     c     kss61         chain     WFLSS61l
316100120903     c
316200120905     c                   if        not %found(WFLSS61l)
316300120905     c                   clear                   WFLSS600
316400120903     c                   eval      wsstie=ks_tie
316500120903     c                   eval      wsstip=ks_tip
316600120903     c                   eval      wsskgf=ks_sca
316700120904     c                   eval      wsslin=ks_lin
316800120903     c                   eval      wssdal=§al
316900120903     c                   eval      wssall=§al
317000120903     c                   endif
317100120903     c
317200120903     c                   exsr      Scrivi_pes
317300120903     c
317400120903    3c                   endif
317500120903     c
317600120903    2c                   endif
317700120903
317800120903    2c                   if        Wtipo='P' or Wtipo='R'
317900120903     c                   eval      ks_tie=wtipo
318000120903     c                   eval      ks_lin=fil(k)
318100120905     c     kss61         chain     WFLSS61l
318200120903     c
318300120905     c                   if        not %found(WFLSS61l)
318400120905     c                   clear                   WFLSS600
318500120903     c                   eval      wsstie=ks_tie
318600120903     c                   eval      wsstip=ks_tip
318700120903     c                   eval      wsskgf=ks_sca
318800120904     c                   eval      wsslin=ks_lin
318900120903     c                   eval      wssdal=§dal
319000120903     c                   eval      wssall=§al
319100120903     c                   endif
319200120903     c
319300120903     c                   exsr      Scrivi_pes
319400120903     c
319500120903    2c                   endif
319600120903     c
319700120903    1c                   endif
319800120903     c                   ENDSR
319900120903     c****************************************************************************
320000120903     c     Scrivi_pes    BEGSR
320100120903     c                   add       1             wsstsp
320200120903     C                   ADD       TASNCL        wsstcl
320300120903     C                   ADD       w_peso        wsstkg
320400120903     C                   ADD       TASVLF        wsstmc
320500120903     c                   if        tastsp='E'
320600120903     c                   add       1             wsstspe
320700120903     C                   ADD       TASNCL        wsstcle
320800120903     C                   ADD       w_peso        wsstkge
320900120903     C                   ADD       TASVLF        wsstmce
321000120903     c                   endif
321100120903     c                   if        tastsp='h'
321200120903     c                   add       1             wsstsph
321300120903     C                   ADD       TASNCL        wsstclh
321400120903     C                   ADD       w_peso        wsstkgh
321500120903     C                   ADD       TASVLF        wsstmch
321600120903     c                   endif
321700120903     c
321800120905     c                   if        not %found(WFLSS61l)
321900120905     c                   write     WFLSS600
322000120903     c                   else
322100120905     c                   update    WFLSS600
322200120903     c                   endif
322300120903     c
322400120903     c                   ENDSR
322500100402     c****************************************************************************
322600970519     OQSYSPRT   E            TESTA          2 02
322700090206     O                                         +  1 'Terminata elaborazione'
322800090206     o                                         +  2 'statistiche spedizioni'
322900090206     O                                         +  2 'DEL'
323000970519     O                       UDATE         Y   +  1
323100970528     O                       §DAL              +  5 '    /  /  '
323200970528     O                                         +  1 '>'
323300970528     O                       §AL               +  1 '    /  /  '
323400090206     O                                          120 'TNSSL3R1'
323500970519     O                       PAGE          Z    132
323600970519     O                                          128 'PAG.'
323700970529     O          E            TESTA          2
323800970529     O                                           14 'TIPI BOLLA OK:'
323900970529     O                       TBO               +  1
324000970529     O          E            ERR            1
324100970529     O                       TASLNP               3
324200970529     O                       TASLNA            +  1
324300970529     O                       TASAAS            +  1
324400970529     O                       TASMGS            +  1
324500970529     O                       TASNRS            +  1
324600970529     O                       TASNSP            +  1
324700970529     O                       TASTBL            +  1
324800110801     O          E            TESTA       1  1
324900110801     O                                            7 'Clienti'
325000110801     o                       Ptipocli         +   1
325100110801     O                                        +   1 ':'
325200110801     O          E            detksc         1
325300110801     o                       skicli             132
325400090915**
325500090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
