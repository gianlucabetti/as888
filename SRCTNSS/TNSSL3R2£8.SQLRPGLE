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
107100090204     C*
107200111129     c                   endif
107300111129     c***
107400121123     c* L I N E A   D I   A R R I V O
107500111129     c***
107600111129     c                   z-add     1             kk
107700111129     c     taslna        lookup    fil(kk)                                98
107800111129     c
107900111129     c                   if        *in98
108000111129     c* Euroexpress o Fedex o DPD
108100111129     c                   if        tif(kk) ='F' or tif(kk)='X' or tif(kk)='D'
108200111129     c                   seton                                        55
108300111129     c                   endif
108400111129     C*
108500090204     C*
108600111129     c* Fedex
108700111129     c                   if        tif(kk) ='F'
108800111129     c                   seton                                        52
108900111129     c                   endif
109000111129     c
109100111129     C***  TASLNA        COMP      400                                    52     FEDEX
109200090204     C*
109300111129     c* DPD
109400111129     c                   if        tif(kk) ='D'
109500111129     c                   seton                                        56
109600111129     c                   endif
109700091006     c*
109800111129     c                   endif
109900121123     c
110000111129     c
110100091006     c* Verifico se si tratta di cliente extra budget --> 35 on
110200121121    1c                   if        §stat=' '
110300121121    2c                   if        tasccm<>savccm or tasctr<>savctr
110400121123     c                   z-add     1             EE
110500091007     c                   eval      w010a=%editc(tasccm:'X')+%editc(tasctr:'X')
110600091007     c     w010a         lookup    ksc_EX(EE)                             35
110700121121    3c                   if        not *in35
110800091007     c                   eval      %subst(w010a:8:3)='   '
110900091007     c     w010a         lookup    ksc_EX(EE)                             35
111000121121    3c                   endif
111100091006
111200091006     c                   eval      savccm=tasccm
111300091006     c                   eval      savctr=tasctr
111400121121    2c                   endif
111500110711     c
111600121121   x1c                   else
111700121121     c
111800121121     c* Per i clienti Particolari devo controllare il terminal della bolla se è quello
111900121121     c*   previsto
112000130408   2ac                   if        Wgiro=1
112100121121    2c                   if        tasccm<>savccm
112200121123     c                   z-add     1             EE
112300121121     c                   eval      w010a=%editc(tasccm:'X')+'   '
112400121121     c     w010a         lookup    ksc_EX(EE)                             35
112500121121     c
112600121121     c* E' impossibile che non lo trovi, ma se per caso succede, scarto la bolla
112700121121    3c                   if        *in35
112800121121     c                   eval      savccm=tasccm
112900121121   x3c                   else
113000121121     c                   clear                   savccm
113100121121    3c                   endif
113200121121    2c                   endif
113300130408   2ac                   endif
113400130408     c
113500130408     c* controllo il TASKSC per gli assegnati
113600130408   2ac                   if        Wgiro=2
113700130408     c                   z-add     1             EE
113800130408     c                   eval      w010a=%editc(tasksc:'X')+'   '
113900130408     c     w010a         lookup    ksc_EX(EE)                             35
114000130408     c
114100130408     c* E' impossibile che non lo trovi, ma se per caso succede, scarto la bolla
114200130408    3c                   if        *in35
114300130408     c
114400130408     c                   eval      savccm=tasksc
114500130408     c* Elaboro solo se il TASCCM non è compreso (altrimenti già elaborato nel primo giro)
114600130408     c                   z-add     1             ZZ
114700130408     c                   setoff                                       36
114800130408     c                   eval      w010b=%editc(tasccm:'X')+'   '
114900130408     c     w010b         lookup    ksc_EX(ZZ)                             36
115000130408     c                   if        *in36
115100130408     c                   setoff                                       3536
115200130408     c                   clear                   savccm
115300130408     c                   endif
115400130408     c
115500130408   x3c                   else
115600130408     c                   clear                   savccm
115700130408    3c                   endif
115800130408   2ac                   endif
115900121121     c
116000121121     c                   setoff                                       35
116100121123     c*
116200121123     c                   clear                   prtc_lnp          3 0
116300121123     c                   clear                   prtc_lna          3 0
116400121121     c
116500121121    2c                   if        savccm>0
116600121123     c                   select
116700121123     c*  cliente momentaneamente bloccato
116800121123    3c                   when      ksc_tr(EE)=888
116900121123     c* abilitato a tutto:  memorizzo per qualsiasi terminal
117000121123    3c                   when      ksc_tr(EE)=999
117100121121     c                   seton                                        35
117200121123     c                   eval      prtc_lnp=tastfp
117300121123     c                   eval      prtc_lna=tastfa
117400121123   x3c                   other
117500121123     c
117600121123     c* Abilito solo il terminal di partenza o di arrivo previsto dalla tabella
117700121123    4c                   if        tastfp=ksc_tr(EE)
117800121121     c                   seton                                        35
117900121123     c                   eval      prtc_lnp=tastfp
118000121121    4c                   endif
118100121123    c
118200121123    4c                   if        tastfa=ksc_tr(EE)
118300121123     c                   eval      prtc_lna=tastfa
118400121123     c                   seton                                        35
118500121123    4c                   endif
118600121123     c
118700121123    3c                   endsl
118800121121     c*
118900121121    2c                   endif
119000110711     c
119100121121    1c                   endif
119200121121     c
119300121121     c* Per i clienti Particolari --> 35 off bolla non da elaborare esco dal ciclo
119400121121     c*   visto che l'elaborazione è specifica solo per questi clienti
119500121121     c                   if        §stat='P' and not *in35
119600121121     c                   leavesr
119700121121     c                   endif
119800090204     C*
119900121123    1c                   if        §stat=' '
120000121123     c                   eval      prtc_lnp=taslnp
120100090204     C                   Z-ADD     1             K
120200090204     C     TASLNP        LOOKUP    FIL(K)                                 58
120300121123     c                   else
120400121123     c
120500121123     c* solo si tratta di terminal di partenza elaborabile procedo
120600121123     c                   if        prtc_lnp>0
120700121123     C                   Z-ADD     1             K
120800121123     C     prtc_lnp      LOOKUP    FIL(K)                                 58
120900121123     c                   endif
121000121123     c                   endif
121100121123     c
121200121123    1c                   if        prtc_lnp>0
121300121123     c
121400090204     C  N58              EXCEPT    ERR
121500090204     C     K             OCCUR     DSFIL
121600100402     C     K             OCCUR     DSFIL_e
121700100402     C     K             OCCUR     DSFIL_h
121800121123     c
121900090204     C  N54
122000090204     CANN55              DO                                                     ITALIA
122100100402     c* Spedizioni LNP Italia LNA Italia
122200100402     c                   exsr      speITA
122300090204     C                   ENDDO
122400100402     c
122500100402     c* Spedizioni IMPORT --> LNP Italia
122600090204     C   54              DO                                                     IMPORT
122700100402     c                   exsr      speIMPORT
122800090204     C                   ENDDO
122900100402     c
123000100402     c* Spedizioni lna EUROEXPRESS
123100090204     C   55
123200090204     CANN56
123300090204     CANN52              DO                                                     EXPORT
123400100402     c                   exsr      speEXPRESS
123500090204     C                   ENDDO
123600100402     c
123700100402     c* Spedizioni lna DPD
123800090204     C   55
123900090204     CAN 56              DO                                                     EXPORT
124000100402     c                   exsr      speDPD
124100090204     C                   ENDDO
124200100402     c
124300100402     c* Spedizioni lna FEDEX
124400090204     C   55
124500090204     CAN 52              DO                                                     EXPORT
124600100402     c                   exsr      speFEDEX
124700090204     C                   ENDDO
124800121123     c
124900121123    1c                   endif
125000100402     c
125100090204     C* MEMORIZZAZIONE PER LINEA ARRIVO
125200121123    1c                   if        §stat=' '
125300121123     c                   eval      prtc_lna=taslna
125400090204     C                   Z-ADD     1             K
125500090204     C     TASLNA        LOOKUP    FIL(K)                                 58
125600121123     c                   else
125700121123     c
125800121123     c* solo si tratta di terminal di arrivo elaborabile procedo
125900121123     c                   if        prtc_lna>0
126000121123     C                   Z-ADD     1             K
126100121123     C     prtc_lna      LOOKUP    FIL(K)                                 58
126200121123     c                   endif
126300121123     c                   endif
126400121123     c
126500121123    1c                   if        prtc_lna>0
126600121123     c
126700090204     C  N58              EXCEPT    ERR
126800090204     C     K             OCCUR     DSFIL
126900100402     C     K             OCCUR     DSFIL_e
127000100402     C     K             OCCUR     DSFIL_h
127100100402     c
127200090204     C  N54
127300100402     CANN55              DO
127400100402     c* Spedizioni LNP Italia LNA Italia
127500100402     c                   exsr      speITA_LNA
127600090204     C                   ENDDO
127700100402     c
127800100402     c* spedizioni EXPORT --> lna estera
127900090204     C   55              DO                                                     ESTERO ASS.+FRA.
128000100402     c                   exsr      speEXPORT
128100090204     C                   ENDDO
128200121123     c
128300121123     c                   endif
128400121123     c
128500090204     c                   endsr
128600090206     C**********************************************************
128700090206     C** scarica ds multiple su file
128800090206     C**********************************************************
128900090206     C     scrive_file   BEGSR
129000090206     c*
129100090206     C                   Z-ADD     §AL           WSSALL
129200090206     c*
129300110711    1C                   DO        400           K
129400090206     C     K             OCCUR     DSFIL_smp
129500090206     C     K             OCCUR     DSFIL
129600100402     C     K             OCCUR     DSFIL_e
129700100402     C     K             OCCUR     DSFIL_h
129800110711     c
129900110711    2c                   if        §stat=' '
130000090206     c* WFSMP
130100090206     c* giornaliero
130200090601     c* Lo scrivo solo per tipo "G" o "R"
130300090601     c                   if        Wtipo='G' or Wtipo='R'
130400090206     c                   if        (wmptsp_g>0 or wmptsa_g>0 or
130500090206     c                              wmptspt_g>0 or wmptsat_g>0 or
130600090206     c                              wmptst_g>0)
130700090206     c                   z-add     §al           wmpdal
130800090206     c                   eval      wmptip='G'
130900090206     c                   exsr      rie_campi
131000090206     c                   write     wfsmp000
131100090206     c                   endif
131200090601     c                   endif
131300090601     c
131400090601     c* riepilogo /precedente
131500090601     c* Lo scrivo solo per tipo "R"  se "P" lo scrivo con "P"
131600090601     c                   if        Wtipo='P' or Wtipo='R'
131700090206     c                   if        (wmptsp_r>0 or wmptsa_r>0 or
131800090206     c                              wmptspt_r>0 or wmptsat_r>0 or
131900090206     c                              wmptst_r>0)
132000090206     c                   z-add     §dal          wmpdal
132100090601     c                   if        Wtipo='R'
132200090206     c                   eval      wmptip='R'
132300090601     c                   else
132400090601     c                   eval      wmptip='P'
132500090601     c                   endif
132600090206     c                   exsr      rie_campi
132700090206     c                   write     wfsmp000
132800090206     c                   endif
132900090601     c                   endif
133000110711     c
133100110711    2c                   endif
133200120905     c* WFLSS60F
133300120904     c                   clear                   wsskgf
133400090206     c* giornaliero
133500090601     c                   if        Wtipo='G' or Wtipo='R'
133600110711     c
133700110711    2c                   if        §stat=' '
133800090601     c
133900090206     C     WTSPIP        IFGT      *ZERO
134000090206     C                   Z-ADD     §al           WSSDAL
134100090206     c                   movel     'G'           WSSTIE
134200090206     C                   MOVE      'IP'          WSSTIP
134300090206     C                   Z-ADD     FIL(K)        WSSLIN
134400090206     C                   Z-ADD     WTSPIP        WSSTSP
134500090206     C                   Z-ADD     WTCLIP        WSSTCL
134600090206     C                   Z-ADD(H)  WTKGIP        WSSTKG
134700090206     C                   Z-ADD(H)  WTMCIP        WSSTMC
134800120904     c*old
134900120904     C                   Z-ADD(H)  WTKGIP        WSSTKG_ol
135000120904     C                   Z-ADD(H)  WTMCIP        WSSTMC_ol
135100100402     c* spedizioni "E"
135200100402     C                   Z-ADD     eWTSPIP       WSSTSPe
135300100402     C                   Z-ADD     eWTCLIP       WSSTCLe
135400100402     C                   Z-ADD(H)  eWTKGIP       WSSTKGe
135500100402     C                   Z-ADD(H)  eWTMCIP       WSSTMCe
135600120904     c* old
135700120904     C                   Z-ADD(H)  eWTKGIP       WSSTKGe_ol
135800120904     C                   Z-ADD(H)  eWTMCIP       WSSTMCe_ol
135900100402     c* spedizioni "H"
136000100402     C                   Z-ADD     hWTSPIP       WSSTSPh
136100100402     C                   Z-ADD     hWTCLIP       WSSTCLh
136200100402     C                   Z-ADD(H)  hWTKGIP       WSSTKGh
136300100402     C                   Z-ADD(H)  hWTMCIP       WSSTMCh
136400120904     c* old
136500120904     C                   Z-ADD(H)  hWTKGIP       WSSTKGh_ol
136600120904     C                   Z-ADD(H)  hWTMCIP       WSSTMCh_ol
136700100402
136800120905     C                   WRITE     WFLSS600
136900120904     C                   WRITE     WSLSS500
137000090206     C                   ENDIF
137100100402
137200090206     C     WTSPIA        IFGT      *ZERO
137300090206     C                   Z-ADD     §al           WSSDAL
137400090206     c                   movel     'G'           WSSTIE
137500090206     C                   MOVE      'IA'          WSSTIP
137600090206     C                   Z-ADD     FIL(K)        WSSLIN
137700090206     C                   Z-ADD     WTSPIA        WSSTSP
137800090206     C                   Z-ADD     WTCLIA        WSSTCL
137900090206     C                   Z-ADD(H)  WTKGIA        WSSTKG
138000090206     C                   Z-ADD(H)  WTMCIA        WSSTMC
138100120904     c* old
138200120904     C                   Z-ADD(H)  WTKGIA        WSSTKG_ol
138300120904     C                   Z-ADD(H)  WTMCIA        WSSTMC_ol
138400100402     c* spedizioni "E"
138500100402     C                   Z-ADD     eWTSPIA       WSSTSPe
138600100402     C                   Z-ADD     eWTCLIA       WSSTCLe
138700100402     C                   Z-ADD(H)  eWTKGIA       WSSTKGe
138800100402     C                   Z-ADD(H)  eWTMCIA       WSSTMCe
138900120904     c* old
139000120904     C                   Z-ADD(H)  eWTKGIA       WSSTKGe_ol
139100120904     C                   Z-ADD(H)  eWTMCIA       WSSTMCe_ol
139200100402     c* spedizioni "H"
139300100402     C                   Z-ADD     hWTSPIA       WSSTSPh
139400100402     C                   Z-ADD     hWTCLIA       WSSTCLh
139500100402     C                   Z-ADD(H)  hWTKGIA       WSSTKGh
139600100402     C                   Z-ADD(H)  hWTMCIA       WSSTMCh
139700120904     c* old
139800120904     C                   Z-ADD(H)  hWTKGIA       WSSTKGh_ol
139900120904     C                   Z-ADD(H)  hWTMCIA       WSSTMCh_ol
140000100402     c
140100120905     C                   WRITE     WFLSS600
140200120904     C                   WRITE     WSLSS500
140300090206     C                   ENDIF
140400100402     c
140500090206     C     WTSPEI        IFGT      *ZERO
140600090206     C                   Z-ADD     §al           WSSDAL
140700090206     c                   movel     'G'           WSSTIE
140800090206     C                   MOVE      'EI'          WSSTIP
140900090206     C                   Z-ADD     FIL(K)        WSSLIN
141000090206     C                   Z-ADD     WTSPEI        WSSTSP
141100090206     C                   Z-ADD     WTCLEI        WSSTCL
141200090206     C                   Z-ADD(H)  WTKGEI        WSSTKG
141300090206     C                   Z-ADD(H)  WTMCEI        WSSTMC
141400120904     c* old
141500120904     C                   Z-ADD(H)  WTKGEI        WSSTKG_ol
141600120904     C                   Z-ADD(H)  WTMCEI        WSSTMC_ol
141700100402     c* spedizioni "E"
141800100402     C                   Z-ADD     eWTSPEI       WSSTSPe
141900100402     C                   Z-ADD     eWTCLEI       WSSTCLe
142000100402     C                   Z-ADD(H)  eWTKGEI       WSSTKGe
142100100402     C                   Z-ADD(H)  eWTMCEI       WSSTMCe
142200120904     c* old
142300120904     C                   Z-ADD(H)  eWTKGEI       WSSTKGe_ol
142400120904     C                   Z-ADD(H)  eWTMCEI       WSSTMCe_ol
142500100402     c* spedizioni "H"
142600100402     C                   Z-ADD     hWTSPEI       WSSTSPh
142700100402     C                   Z-ADD     hWTCLEI       WSSTCLh
142800100402     C                   Z-ADD(H)  hWTKGEI       WSSTKGh
142900100402     C                   Z-ADD(H)  hWTMCEI       WSSTMCh
143000120904     c* old
143100120904     C                   Z-ADD(H)  hWTKGEI       WSSTKGh_ol
143200120904     C                   Z-ADD(H)  hWTMCEI       WSSTMCh_ol
143300100402     c
143400120905     C                   WRITE     WFLSS600
143500120904     C                   WRITE     WSLSS500
143600120904     C                   ENDIF
143700100402     c
143800090206     C     WTSPEE        IFGT      *ZERO
143900090206     C                   Z-ADD     §al           WSSDAL
144000090206     c                   movel     'G'           WSSTIE
144100090206     C                   MOVE      'EE'          WSSTIP
144200090206     C                   Z-ADD     FIL(K)        WSSLIN
144300090206     C                   Z-ADD     WTSPEE        WSSTSP
144400090206     C                   Z-ADD     WTCLEE        WSSTCL
144500090206     C                   Z-ADD(H)  WTKGEE        WSSTKG
144600090206     C                   Z-ADD(H)  WTMCEE        WSSTMC
144700120904     c* old
144800120904     C                   Z-ADD(H)  WTKGEE        WSSTKG_ol
144900120904     C                   Z-ADD(H)  WTMCEE        WSSTMC_ol
145000100402     c* spedizioni "E"
145100100402     C                   Z-ADD     eWTSPEE       WSSTSPe
145200100402     C                   Z-ADD     eWTCLEE       WSSTCLe
145300100402     C                   Z-ADD(H)  eWTKGEE       WSSTKGe
145400100402     C                   Z-ADD(H)  eWTMCEE       WSSTMCe
145500120904     c* old
145600120904     C                   Z-ADD(H)  eWTKGEE       WSSTKGe_ol
145700120904     C                   Z-ADD(H)  eWTMCEE       WSSTMCe_ol
145800100402     c* spedizioni "H"
145900100402     C                   Z-ADD     hWTSPEE       WSSTSPh
146000100402     C                   Z-ADD     hWTCLEE       WSSTCLh
146100100402     C                   Z-ADD(H)  hWTKGEE       WSSTKGh
146200100402     C                   Z-ADD(H)  hWTMCEE       WSSTMCh
146300120904     c* old
146400120904     C                   Z-ADD(H)  hWTKGEE       WSSTKGh_ol
146500120904     C                   Z-ADD(H)  hWTMCEE       WSSTMCh_ol
146600100402     c
146700120905     C                   WRITE     WFLSS600
146800120904     C                   WRITE     WSLSS500
146900090206     C                   ENDIF
147000090206     C*
147100090206     C     WTSPED        IFGT      *ZERO
147200090206     C                   Z-ADD     §al           WSSDAL
147300090206     c                   movel     'G'           WSSTIE
147400090206     C                   MOVE      'ED'          WSSTIP
147500090206     C                   Z-ADD     FIL(K)        WSSLIN
147600090206     C                   Z-ADD     WTSPED        WSSTSP
147700090206     C                   Z-ADD     WTCLED        WSSTCL
147800090206     C                   Z-ADD(H)  WTKGED        WSSTKG
147900090206     C                   Z-ADD(H)  WTMCED        WSSTMC
148000120904     c* old
148100120904     C                   Z-ADD(H)  WTKGED        WSSTKG_ol
148200120904     C                   Z-ADD(H)  WTMCED        WSSTMC_ol
148300100402     c* spedizioni "E"
148400100402     C                   Z-ADD     eWTSPED       WSSTSPe
148500100402     C                   Z-ADD     eWTCLED       WSSTCLe
148600100402     C                   Z-ADD(H)  eWTKGED       WSSTKGe
148700100402     C                   Z-ADD(H)  eWTMCED       WSSTMCe
148800120904     c* old
148900120904     C                   Z-ADD(H)  eWTKGED       WSSTKGe_ol
149000120904     C                   Z-ADD(H)  eWTMCED       WSSTMCe_ol
149100100402     c* spedizioni "H"
149200100402     C                   Z-ADD     hWTSPED       WSSTSPh
149300100402     C                   Z-ADD     hWTCLED       WSSTCLh
149400100402     C                   Z-ADD(H)  hWTKGED       WSSTKGh
149500100402     C                   Z-ADD(H)  hWTMCED       WSSTMCh
149600120904     c* old
149700120904     C                   Z-ADD(H)  hWTKGED       WSSTKGh_ol
149800120904     C                   Z-ADD(H)  hWTMCED       WSSTMCh_ol
149900100402     c
150000120905     C                   WRITE     WFLSS600
150100120904     C                   WRITE     WSLSS500
150200090206     C                   ENDIF
150300090206     C*
150400090206     C     WTSPEX        IFGT      *ZERO
150500090206     C                   Z-ADD     §al           WSSDAL
150600090206     c                   movel     'G'           WSSTIE
150700090206     C                   MOVE      'EX'          WSSTIP
150800090206     C                   Z-ADD     FIL(K)        WSSLIN
150900090206     C                   Z-ADD     WTSPEX        WSSTSP
151000090206     C                   Z-ADD     WTCLEX        WSSTCL
151100090206     C                   Z-ADD(H)  WTKGEX        WSSTKG
151200090206     C                   Z-ADD(H)  WTMCEX        WSSTMC
151300120904     c* old
151400120904     C                   Z-ADD(H)  WTKGEX        WSSTKG_ol
151500120904     C                   Z-ADD(H)  WTMCEX        WSSTMC_ol
151600100402     c* spedizioni "E"
151700100402     C                   Z-ADD     eWTSPEX       WSSTSPe
151800100402     C                   Z-ADD     eWTCLEX       WSSTCLe
151900100402     C                   Z-ADD(H)  eWTKGEX       WSSTKGe
152000100402     C                   Z-ADD(H)  eWTMCEX       WSSTMCe
152100120904     c* old
152200120904     C                   Z-ADD(H)  eWTKGEX       WSSTKGe_ol
152300120904     C                   Z-ADD(H)  eWTMCEX       WSSTMCe_ol
152400100402     c* spedizioni "H"
152500100402     C                   Z-ADD     hWTSPEX       WSSTSPh
152600100402     C                   Z-ADD     hWTCLEX       WSSTCLh
152700100402     C                   Z-ADD(H)  hWTKGEX       WSSTKGh
152800100402     C                   Z-ADD(H)  hWTMCEX       WSSTMCh
152900120904     c* old
153000120904     C                   Z-ADD(H)  hWTKGEX       WSSTKGh_ol
153100120904     C                   Z-ADD(H)  hWTMCEX       WSSTMCh_ol
153200100402     c
153300120905     C                   WRITE     WFLSS600
153400120904     C                   WRITE     WSLSS500
153500090206     C                   ENDIF
153600090206     C*
153700090206     C     WTSPEA        IFGT      *ZERO
153800090206     C                   Z-ADD     §al           WSSDAL
153900090206     c                   movel     'G'           WSSTIE
154000090206     C                   MOVE      'EA'          WSSTIP
154100090206     C                   Z-ADD     FIL(K)        WSSLIN
154200090206     C                   Z-ADD     WTSPEA        WSSTSP
154300090206     C                   Z-ADD     WTCLEA        WSSTCL
154400090206     C                   Z-ADD(H)  WTKGEA        WSSTKG
154500090206     C                   Z-ADD(H)  WTMCEA        WSSTMC
154600120904     c* old
154700120904     C                   Z-ADD(H)  WTKGEA        WSSTKG_ol
154800120904     C                   Z-ADD(H)  WTMCEA        WSSTMC_ol
154900100402     c* spedizioni "E"
155000100402     C                   Z-ADD     eWTSPEA       WSSTSPe
155100100402     C                   Z-ADD     eWTCLEA       WSSTCLe
155200100402     C                   Z-ADD(H)  eWTKGEA       WSSTKGe
155300100402     C                   Z-ADD(H)  eWTMCEA       WSSTMCe
155400120904     c* old
155500120904     C                   Z-ADD(H)  eWTKGEA       WSSTKGe_ol
155600120904     C                   Z-ADD(H)  eWTMCEA       WSSTMCe_ol
155700100402     c* spedizioni "H"
155800100402     C                   Z-ADD     hWTSPEA       WSSTSPh
155900100402     C                   Z-ADD     hWTCLEA       WSSTCLh
156000100402     C                   Z-ADD(H)  hWTKGEA       WSSTKGh
156100100402     C                   Z-ADD(H)  hWTMCEA       WSSTMCh
156200120904     c* old
156300120904     C                   Z-ADD(H)  hWTKGEA       WSSTKGh_ol
156400120904     C                   Z-ADD(H)  hWTMCEA       WSSTMCh_ol
156500100402     c
156600120905     C                   WRITE     WFLSS600
156700120904     C                   WRITE     WSLSS500
156800090206     C                   ENDIF
156900090206     C*
157000090206     C     WTSPPA        IFGT      *ZERO
157100090206     C                   Z-ADD     §al           WSSDAL
157200090206     c                   movel     'G'           WSSTIE
157300090206     C                   MOVE      'PA'          WSSTIP
157400090206     C                   Z-ADD     FIL(K)        WSSLIN
157500090206     C                   Z-ADD     WTSPPA        WSSTSP
157600090206     C                   Z-ADD     WTCLPA        WSSTCL
157700090206     C                   Z-ADD(H)  WTKGPA        WSSTKG
157800090206     C                   Z-ADD(H)  WTMCPA        WSSTMC
157900120904     c* old
158000120904     C                   Z-ADD(H)  WTKGPA        WSSTKG_ol
158100120904     C                   Z-ADD(H)  WTMCPA        WSSTMC_ol
158200100402     c* spedizioni "E"
158300100402     C                   Z-ADD     eWTSPPA       WSSTSPe
158400100402     C                   Z-ADD     eWTCLPA       WSSTCLe
158500100402     C                   Z-ADD(H)  eWTKGPA       WSSTKGe
158600100402     C                   Z-ADD(H)  eWTMCPA       WSSTMCe
158700120904     c* old
158800120904     C                   Z-ADD(H)  eWTKGPA       WSSTKGe_ol
158900120904     C                   Z-ADD(H)  eWTMCPA       WSSTMCe_ol
159000100402     c* spedizioni "H"
159100100402     C                   Z-ADD     hWTSPPA       WSSTSPh
159200100402     C                   Z-ADD     hWTCLPA       WSSTCLh
159300100402     C                   Z-ADD(H)  hWTKGPA       WSSTKGh
159400100402     C                   Z-ADD(H)  hWTMCPA       WSSTMCh
159500120904     c* old
159600120904     C                   Z-ADD(H)  hWTKGPA       WSSTKGh_ol
159700120904     C                   Z-ADD(H)  hWTMCPA       WSSTMCh_ol
159800100402     c
159900120905     C                   WRITE     WFLSS600
160000120904     C                   WRITE     WSLSS500
160100090206     C                   ENDIF
160200110711     c
160300110711     c                   endif
160400110715     c* Extra budget o clienti particolari
160500110715     c                   if        §stat=' '
160600110715     c                   movel     'G'           WSSTIE
160700110715     c                   else
160800110715     c                   movel     'A'           WSSTIE
160900110715     c                   endif
161000110715     c
161100091006     C     WTSPIP_e      IFGT      *ZERO
161200091007     C                   Z-ADD     §al           WSSDAL
161300091006     C                   MOVE      'IP'          WSSTIP
161400091006     C                   Z-ADD     FIL(K)        WSSLIN
161500091006     C                   Z-ADD     WTSPIP_e      WSSTSP
161600091006     C                   Z-ADD     WTCLIP_e      WSSTCL
161700120904     C                   Z-ADD(H)  WTKGIP_e      WSSTKG_ex
161800120904     C                   Z-ADD(H)  WTMCIP_e      WSSTMC_ex
161900100402     c* spedizioni "E"
162000100402     C                   Z-ADD     eWTSPIP_e     WSSTSPe
162100100402     C                   Z-ADD     eWTCLIP_e     WSSTCLe
162200120904     C                   Z-ADD(H)  eWTKGIP_e     WSSTKGe_ex
162300120904     C                   Z-ADD(H)  eWTMCIP_e     WSSTMCe_ex
162400100402     c* spedizioni "H"
162500100402     C                   Z-ADD     hWTSPIP_e     WSSTSPh
162600100402     C                   Z-ADD     hWTCLIP_e     WSSTCLh
162700120904     C                   Z-ADD(H)  hWTKGIP_e     WSSTKGh_ex
162800120904     C                   Z-ADD(H)  hWTMCIP_e     WSSTMCh_ex
162900100402     c
163000120903     C                   WRITE     wslss550
163100091006     C                   ENDIF
163200100402     c
163300091006     C     WTSPIA_e      IFGT      *ZERO
163400091007     C                   Z-ADD     §al           WSSDAL
163500110715     c* Es - non importa riscrivere il falg: non da clear e già impostato sopra
163600110715     c*****              movel     'G'           WSSTIE
163700091006     C                   MOVE      'IA'          WSSTIP
163800091006     C                   Z-ADD     FIL(K)        WSSLIN
163900091006     C                   Z-ADD     WTSPIA_e      WSSTSP
164000091006     C                   Z-ADD     WTCLIA_e      WSSTCL
164100120904     C                   Z-ADD(H)  WTKGIA_e      WSSTKG_ex
164200120904     C                   Z-ADD(H)  WTMCIA_e      WSSTMC_ex
164300100402     c* spedizioni "E"
164400100402     C                   Z-ADD     eWTSPIA_e     WSSTSPe
164500100402     C                   Z-ADD     eWTCLIA_e     WSSTCLe
164600120904     C                   Z-ADD(H)  eWTKGIA_e     WSSTKGe_ex
164700120904     C                   Z-ADD(H)  eWTMCIA_e     WSSTMCe_ex
164800100402     c* spedizioni "H"
164900100402     C                   Z-ADD     hWTSPIA_e     WSSTSPh
165000100402     C                   Z-ADD     hWTCLIA_e     WSSTCLh
165100120904     C                   Z-ADD(H)  hWTKGIA_e     WSSTKGh_ex
165200120904     C                   Z-ADD(H)  hWTMCIA_e     WSSTMCh_ex
165300100402     c
165400091006     C                   WRITE     WSLSS550
165500091006     C                   ENDIF
165600100402     c
165700091006     C     WTSPEI_e      IFGT      *ZERO
165800091007     C                   Z-ADD     §al           WSSDAL
165900110715     c******             movel     'G'           WSSTIE
166000091006     C                   MOVE      'EI'          WSSTIP
166100091006     C                   Z-ADD     FIL(K)        WSSLIN
166200091006     C                   Z-ADD     WTSPEI_e      WSSTSP
166300091006     C                   Z-ADD     WTCLEI_e      WSSTCL
166400120904     C                   Z-ADD(H)  WTKGEI_e      WSSTKG_ex
166500120904     C                   Z-ADD(H)  WTMCEI_e      WSSTMC_ex
166600100402     c* spedizioni "E"
166700100402     C                   Z-ADD     eWTSPEI_e     WSSTSPe
166800100402     C                   Z-ADD     eWTCLEI_e     WSSTCLe
166900120904     C                   Z-ADD(H)  eWTKGEI_e     WSSTKGe_ex
167000120904     C                   Z-ADD(H)  eWTMCEI_e     WSSTMCe_ex
167100100402     c* spedizioni "H"
167200100402     C                   Z-ADD     hWTSPEI_e     WSSTSPh
167300100402     C                   Z-ADD     hWTCLEI_e     WSSTCLh
167400120904     C                   Z-ADD(H)  hWTKGEI_e     WSSTKGh_ex
167500120904     C                   Z-ADD(H)  hWTMCEI_e     WSSTMCh_ex
167600100402     c
167700091006     C                   WRITE     WSLSS550
167800091006     C                   ENDIF
167900100402     c
168000091006     C     WTSPEE_e      IFGT      *ZERO
168100091007     C                   Z-ADD     §al           WSSDAL
168200110715     c******             movel     'G'           WSSTIE
168300091006     C                   MOVE      'EE'          WSSTIP
168400091006     C                   Z-ADD     FIL(K)        WSSLIN
168500091006     C                   Z-ADD     WTSPEE_e      WSSTSP
168600091006     C                   Z-ADD     WTCLEE_e      WSSTCL
168700120904     C                   Z-ADD(H)  WTKGEE_e      WSSTKG_ex
168800120904     C                   Z-ADD(H)  WTMCEE_e      WSSTMC_ex
168900100402     c* spedizioni "E"
169000100402     C                   Z-ADD     eWTSPEE_e     WSSTSPe
169100100402     C                   Z-ADD     eWTCLEE_e     WSSTCLe
169200120904     C                   Z-ADD(H)  eWTKGEE_e     WSSTKGe_ex
169300120904     C                   Z-ADD(H)  eWTMCEE_e     WSSTMCe_ex
169400100402     c* spedizioni "H"
169500100402     C                   Z-ADD     hWTSPEE_e     WSSTSPh
169600100402     C                   Z-ADD     hWTCLEE_e     WSSTCLh
169700120904     C                   Z-ADD(H)  hWTKGEE_e     WSSTKGh_ex
169800120904     C                   Z-ADD(H)  hWTMCEE_e     WSSTMCh_ex
169900100402     c
170000091006     C                   WRITE     WSLSS550
170100091006     C                   ENDIF
170200091006     C*
170300091006     C     WTSPED_e      IFGT      *ZERO
170400091007     C                   Z-ADD     §al           WSSDAL
170500110715     c*******            movel     'G'           WSSTIE
170600091006     C                   MOVE      'ED'          WSSTIP
170700091006     C                   Z-ADD     FIL(K)        WSSLIN
170800091006     C                   Z-ADD     WTSPED_e      WSSTSP
170900091006     C                   Z-ADD     WTCLED_e      WSSTCL
171000120904     C                   Z-ADD(H)  WTKGED_e      WSSTKG_ex
171100120904     C                   Z-ADD(H)  WTMCED_e      WSSTMC_ex
171200100402     c* spedizioni "E"
171300100402     C                   Z-ADD     eWTSPED_e     WSSTSPe
171400100402     C                   Z-ADD     eWTCLED_e     WSSTCLe
171500120904     C                   Z-ADD(H)  eWTKGED_e     WSSTKGe_ex
171600120904     C                   Z-ADD(H)  eWTMCED_e     WSSTMCe_ex
171700100402     c* spedizioni "H"
171800100402     C                   Z-ADD     hWTSPED_e     WSSTSPh
171900100402     C                   Z-ADD     hWTCLED_e     WSSTCLh
172000120904     C                   Z-ADD(H)  hWTKGED_e     WSSTKGh_ex
172100120904     C                   Z-ADD(H)  hWTMCED_e     WSSTMCh_ex
172200100402     c
172300091006     C                   WRITE     WSLSS550
172400091006     C                   ENDIF
172500091006     C*
172600091006     C     WTSPEX_e      IFGT      *ZERO
172700091007     C                   Z-ADD     §al           WSSDAL
172800110715     c*******            movel     'G'           WSSTIE
172900091006     C                   MOVE      'EX'          WSSTIP
173000091006     C                   Z-ADD     FIL(K)        WSSLIN
173100091006     C                   Z-ADD     WTSPEX_e      WSSTSP
173200091006     C                   Z-ADD     WTCLEX_e      WSSTCL
173300120904     C                   Z-ADD(H)  WTKGEX_e      WSSTKG_ex
173400120904     C                   Z-ADD(H)  WTMCEX_e      WSSTMC_ex
173500100402     c* spedizioni "E"
173600100402     C                   Z-ADD     eWTSPEX_e     WSSTSPe
173700100402     C                   Z-ADD     eWTCLEX_e     WSSTCLe
173800120904     C                   Z-ADD(H)  eWTKGEX_e     WSSTKGe_ex
173900120904     C                   Z-ADD(H)  eWTMCEX_e     WSSTMCe_ex
174000100402     c* spedizioni "H"
174100100402     C                   Z-ADD     hWTSPEX_e     WSSTSPh
174200100402     C                   Z-ADD     hWTCLEX_e     WSSTCLh
174300120904     C                   Z-ADD(H)  hWTKGEX_e     WSSTKGh_ex
174400120904     C                   Z-ADD(H)  hWTMCEX_e     WSSTMCh_ex
174500100402     c
174600091006     C                   WRITE     WSLSS550
174700091006     C                   ENDIF
174800091006     C*
174900091006     C     WTSPEA_e      IFGT      *ZERO
175000091007     C                   Z-ADD     §al           WSSDAL
175100110715     c******             movel     'G'           WSSTIE
175200091006     C                   MOVE      'EA'          WSSTIP
175300091006     C                   Z-ADD     FIL(K)        WSSLIN
175400091006     C                   Z-ADD     WTSPEA_e      WSSTSP
175500091006     C                   Z-ADD     WTCLEA_e      WSSTCL
175600120904     C                   Z-ADD(H)  WTKGEA_e      WSSTKG_ex
175700120904     C                   Z-ADD(H)  WTMCEA_e      WSSTMC_ex
175800100402     c* spedizioni "E"
175900100402     C                   Z-ADD     eWTSPEA_e     WSSTSPe
176000100402     C                   Z-ADD     eWTCLEA_e     WSSTCLe
176100120904     C                   Z-ADD(H)  eWTKGEA_e     WSSTKGe_ex
176200120904     C                   Z-ADD(H)  eWTMCEA_e     WSSTMCe_ex
176300100402     c* spedizioni "H"
176400100402     C                   Z-ADD     hWTSPEA_e     WSSTSPh
176500100402     C                   Z-ADD     hWTCLEA_e     WSSTCLh
176600120904     C                   Z-ADD(H)  hWTKGEA_e     WSSTKGh_ex
176700120904     C                   Z-ADD(H)  hWTMCEA_e     WSSTMCh_ex
176800100402     c
176900091006     C                   WRITE     WSLSS550
177000091006     C                   ENDIF
177100091006     C*
177200091006     C     WTSPPA_e      IFGT      *ZERO
177300091007     C                   Z-ADD     §al           WSSDAL
177400110715     c*******            movel     'G'           WSSTIE
177500091006     C                   MOVE      'PA'          WSSTIP
177600091006     C                   Z-ADD     FIL(K)        WSSLIN
177700091006     C                   Z-ADD     WTSPPA_e      WSSTSP
177800091006     C                   Z-ADD     WTCLPA_e      WSSTCL
177900120904     C                   Z-ADD(H)  WTKGPA_e      WSSTKG_ex
178000120904     C                   Z-ADD(H)  WTMCPA_e      WSSTMC_ex
178100100402     c* spedizioni "E"
178200100402     C                   Z-ADD     eWTSPPA_e     WSSTSPe
178300100402     C                   Z-ADD     eWTCLPA_e     WSSTCLe
178400120904     C                   Z-ADD(H)  eWTKGPA_e     WSSTKGe_ex
178500120904     C                   Z-ADD(H)  eWTMCPA_e     WSSTMCe_ex
178600100402     c* spedizioni "H"
178700100402     C                   Z-ADD     hWTSPPA_e     WSSTSPh
178800100402     C                   Z-ADD     hWTCLPA_e     WSSTCLh
178900120904     C                   Z-ADD(H)  hWTKGPA_e     WSSTKGh_ex
179000120904     C                   Z-ADD(H)  hWTMCPA_e     WSSTMCh_ex
179100100402     c
179200091006     C                   WRITE     WSLSS550
179300091006     C                   ENDIF
179400091006
179500090601     C                   ENDIF
179600090206     c*
179700090206     c* riepilogo
179800090601     c                   if        Wtipo='P' or Wtipo='R'
179900110715     c                   if        §stat=' '
180000110715
180100090206     C     WTSPIP_r      IFGT      *ZERO
180200090206     C                   Z-ADD     §dal          WSSDAL
180300090601     c**                 movel     'R'           WSSTIE
180400090601     c                   movel     Wtipo         WSSTIE
180500090206     C                   MOVE      'IP'          WSSTIP
180600090206     C                   Z-ADD     FIL(K)        WSSLIN
180700090206     C                   Z-ADD     WTSPIP_r      WSSTSP
180800090206     C                   Z-ADD     WTCLIP_r      WSSTCL
180900090206     C                   Z-ADD(H)  WTKGIP_r      WSSTKG
181000090206     C                   Z-ADD(H)  WTMCIP_r      WSSTMC
181100120904     c* old
181200120904     C                   Z-ADD(H)  WTKGIP_r      WSSTKG_ol
181300120904     C                   Z-ADD(H)  WTMCIP_r      WSSTMC_ol
181400100402     c* spedizioni "E"
181500100402     C                   Z-ADD     eWTSPIP_r     WSSTSPe
181600100402     C                   Z-ADD     eWTCLIP_r     WSSTCLe
181700100402     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe
181800100402     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe
181900120904     c* old
182000120904     C                   Z-ADD(H)  eWTKGIP_r     WSSTKGe_ol
182100120904     C                   Z-ADD(H)  eWTMCIP_r     WSSTMCe_ol
182200100402     c* spedizioni "H"
182300100402     C                   Z-ADD     hWTSPIP_r     WSSTSPh
182400100402     C                   Z-ADD     hWTCLIP_r     WSSTCLh
182500100402     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh
182600100402     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh
182700120904     c* old
182800120904     C                   Z-ADD(H)  hWTKGIP_r     WSSTKGh_ol
182900120904     C                   Z-ADD(H)  hWTMCIP_r     WSSTMCh_ol
183000100402     c
183100120905     C                   WRITE     WFLSS600
183200120904     C                   WRITE     WSLSS500
183300090206     C                   ENDIF
183400100402     c
183500090206     C     WTSPIA_r      IFGT      *ZERO
183600090206     C                   Z-ADD     §dal          WSSDAL
183700090601     c**                 movel     'R'           WSSTIE
183800090601     c                   movel     Wtipo         WSSTIE
183900090206     C                   MOVE      'IA'          WSSTIP
184000090206     C                   Z-ADD     FIL(K)        WSSLIN
184100090206     C                   Z-ADD     WTSPIA_r      WSSTSP
184200090206     C                   Z-ADD     WTCLIA_r      WSSTCL
184300090206     C                   Z-ADD(H)  WTKGIA_r      WSSTKG
184400090206     C                   Z-ADD(H)  WTMCIA_r      WSSTMC
184500120904     c* old
184600120904     C                   Z-ADD(H)  WTKGIA_r      WSSTKG_ol
184700120904     C                   Z-ADD(H)  WTMCIA_r      WSSTMC_ol
184800100402     c* spedizioni "E"
184900100402     C                   Z-ADD     eWTSPIA_r     WSSTSPe
185000100402     C                   Z-ADD     eWTCLIA_r     WSSTCLe
185100100402     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe
185200100402     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe
185300120904     c* old
185400120904     C                   Z-ADD(H)  eWTKGIA_r     WSSTKGe_ol
185500120904     C                   Z-ADD(H)  eWTMCIA_r     WSSTMCe_ol
185600100402     c* spedizioni "H"
185700100402     C                   Z-ADD     hWTSPIA_r     WSSTSPh
185800100402     C                   Z-ADD     hWTCLIA_r     WSSTCLh
185900100402     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh
186000100402     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh
186100120904     c* old
186200120904     C                   Z-ADD(H)  hWTKGIA_r     WSSTKGh_ol
186300120904     C                   Z-ADD(H)  hWTMCIA_r     WSSTMCh_ol
186400100402     c
186500120905     C                   WRITE     WFLSS600
186600120904     C                   WRITE     WSLSS500
186700090206     C                   ENDIF
186800100402
186900090206     C     WTSPEI_R      IFGT      *ZERO
187000090206     C                   Z-ADD     §dal          WSSDAL
187100090601     c**                 movel     'R'           WSSTIE
187200090601     c                   movel     Wtipo         WSSTIE
187300090206     C                   MOVE      'EI'          WSSTIP
187400090206     C                   Z-ADD     FIL(K)        WSSLIN
187500090206     C                   Z-ADD     WTSPEI_r      WSSTSP
187600090206     C                   Z-ADD     WTCLEI_r      WSSTCL
187700090206     C                   Z-ADD(H)  WTKGEI_r      WSSTKG
187800090206     C                   Z-ADD(H)  WTMCEI_r      WSSTMC
187900120904     c* old
188000120904     C                   Z-ADD(H)  WTKGEI_r      WSSTKG_ol
188100120904     C                   Z-ADD(H)  WTMCEI_r      WSSTMC_ol
188200100402     c* Spedizioni "E"
188300100402     C                   Z-ADD     eWTSPEI_r     WSSTSPe
188400100402     C                   Z-ADD     eWTCLEI_r     WSSTCLe
188500100402     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe
188600100402     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe
188700120904     c* old
188800120904     C                   Z-ADD(H)  eWTKGEI_r     WSSTKGe_ol
188900120904     C                   Z-ADD(H)  eWTMCEI_r     WSSTMCe_ol
189000100402     c* Spedizioni "H"
189100100402     C                   Z-ADD     hWTSPEI_r     WSSTSPh
189200100402     C                   Z-ADD     hWTCLEI_r     WSSTCLh
189300100402     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh
189400100402     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh
189500120904     c* old
189600120904     C                   Z-ADD(H)  hWTKGEI_r     WSSTKGh_ol
189700120904     C                   Z-ADD(H)  hWTMCEI_r     WSSTMCh_ol
189800100402     c
189900120905     C                   WRITE     WFLSS600
190000120904     C                   WRITE     WSLSS500
190100090206     C                   ENDIF
190200100402     c
190300090206     C     WTSPEE_r      IFGT      *ZERO
190400090206     C                   Z-ADD     §dal          WSSDAL
190500090601     c**                 movel     'R'           WSSTIE
190600090601     c                   movel     Wtipo         WSSTIE
190700090206     C                   MOVE      'EE'          WSSTIP
190800090206     C                   Z-ADD     FIL(K)        WSSLIN
190900090206     C                   Z-ADD     WTSPEE_r      WSSTSP
191000090206     C                   Z-ADD     WTCLEE_r      WSSTCL
191100090206     C                   Z-ADD(H)  WTKGEE_r      WSSTKG
191200090206     C                   Z-ADD(H)  WTMCEE_r      WSSTMC
191300120904     c* old
191400120904     C                   Z-ADD(H)  WTKGEE_r      WSSTKG_ol
191500120904     C                   Z-ADD(H)  WTMCEE_r      WSSTMC_ol
191600100402     c* Spedizioni "E"
191700100402     C                   Z-ADD     eWTSPEE_r     WSSTSPe
191800100402     C                   Z-ADD     eWTCLEE_r     WSSTCLe
191900100402     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe
192000100402     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe
192100120904     c* old
192200120904     C                   Z-ADD(H)  eWTKGEE_r     WSSTKGe_ol
192300120904     C                   Z-ADD(H)  eWTMCEE_r     WSSTMCe_ol
192400100402     c* Spedizioni "H"
192500100402     C                   Z-ADD     hWTSPEE_r     WSSTSPh
192600100402     C                   Z-ADD     hWTCLEE_r     WSSTCLh
192700100402     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh
192800100402     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh
192900120904     c* old
193000120904     C                   Z-ADD(H)  hWTKGEE_r     WSSTKGh_ol
193100120904     C                   Z-ADD(H)  hWTMCEE_r     WSSTMCh_ol
193200100402     c
193300120905     C                   WRITE     WFLSS600
193400120904     C                   WRITE     WSLSS500
193500090206     C                   ENDIF
193600090206     C*
193700090206     C     WTSPED_r      IFGT      *ZERO
193800090206     C                   Z-ADD     §dal          WSSDAL
193900090601     c**                 movel     'R'           WSSTIE
194000090601     c                   movel     Wtipo         WSSTIE
194100090206     C                   MOVE      'ED'          WSSTIP
194200090206     C                   Z-ADD     FIL(K)        WSSLIN
194300090206     C                   Z-ADD     WTSPED_r      WSSTSP
194400090206     C                   Z-ADD     WTCLED_r      WSSTCL
194500090206     C                   Z-ADD(H)  WTKGED_r      WSSTKG
194600090206     C                   Z-ADD(H)  WTMCED_r      WSSTMC
194700120904     c* old
194800120904     C                   Z-ADD(H)  WTKGED_r      WSSTKG_ol
194900120904     C                   Z-ADD(H)  WTMCED_r      WSSTMC_ol
195000100402     c* Spedizioni "E"
195100100402     C                   Z-ADD     eWTSPED_r     WSSTSPe
195200100402     C                   Z-ADD     eWTCLED_r     WSSTCLe
195300100402     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe
195400100402     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe
195500120904     c* old
195600120904     C                   Z-ADD(H)  eWTKGED_r     WSSTKGe_ol
195700120904     C                   Z-ADD(H)  eWTMCED_r     WSSTMCe_ol
195800100402     c* Spedizioni "H"
195900100402     C                   Z-ADD     hWTSPED_r     WSSTSPh
196000100402     C                   Z-ADD     hWTCLED_r     WSSTCLh
196100100402     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh
196200100402     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh
196300120904     c* old
196400120904     C                   Z-ADD(H)  hWTKGED_r     WSSTKGh_ol
196500120904     C                   Z-ADD(H)  hWTMCED_r     WSSTMCh_ol
196600100402     c
196700120905     C                   WRITE     WFLSS600
196800120904     C                   WRITE     WSLSS500
196900090206     C                   ENDIF
197000090206     C*
197100090206     C     WTSPEX_r      IFGT      *ZERO
197200090206     C                   Z-ADD     §dal          WSSDAL
197300090601     c**                 movel     'R'           WSSTIE
197400090601     c                   movel     Wtipo         WSSTIE
197500090206     C                   MOVE      'EX'          WSSTIP
197600090206     C                   Z-ADD     FIL(K)        WSSLIN
197700090206     C                   Z-ADD     WTSPEX_r      WSSTSP
197800090206     C                   Z-ADD     WTCLEX_r      WSSTCL
197900090206     C                   Z-ADD(H)  WTKGEX_r      WSSTKG
198000090206     C                   Z-ADD(H)  WTMCEX_r      WSSTMC
198100120904     c* old
198200120904     C                   Z-ADD(H)  WTKGEX_r      WSSTKG_ol
198300120904     C                   Z-ADD(H)  WTMCEX_r      WSSTMC_ol
198400100402     c* Spedizioni "E"
198500100402     C                   Z-ADD     eWTSPEX_r     WSSTSPe
198600100402     C                   Z-ADD     eWTCLEX_r     WSSTCLe
198700100402     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe
198800100402     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe
198900120904     c* old
199000120904     C                   Z-ADD(H)  eWTKGEX_r     WSSTKGe_ol
199100120904     C                   Z-ADD(H)  eWTMCEX_r     WSSTMCe_ol
199200100402     c* Spedizioni "H"
199300100402     C                   Z-ADD     hWTSPEX_r     WSSTSPh
199400100402     C                   Z-ADD     hWTCLEX_r     WSSTCLh
199500100402     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh
199600100402     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh
199700120904     c* old
199800120904     C                   Z-ADD(H)  hWTKGEX_r     WSSTKGh_ol
199900120904     C                   Z-ADD(H)  hWTMCEX_r     WSSTMCh_ol
200000100402     c
200100120905     C                   WRITE     WFLSS600
200200120904     C                   WRITE     WSLSS500
200300090206     C                   ENDIF
200400090206     C*
200500090206     C     WTSPEA_r      IFGT      *ZERO
200600090206     C                   Z-ADD     §dal          WSSDAL
200700090601     c**                 movel     'R'           WSSTIE
200800090601     c                   movel     Wtipo         WSSTIE
200900090206     C                   MOVE      'EA'          WSSTIP
201000090206     C                   Z-ADD     FIL(K)        WSSLIN
201100090206     C                   Z-ADD     WTSPEA_r      WSSTSP
201200090206     C                   Z-ADD     WTCLEA_r      WSSTCL
201300090206     C                   Z-ADD(H)  WTKGEA_r      WSSTKG
201400090206     C                   Z-ADD(H)  WTMCEA_r      WSSTMC
201500120904     c* old
201600120904     C                   Z-ADD(H)  WTKGEA_r      WSSTKG_ol
201700120904     C                   Z-ADD(H)  WTMCEA_r      WSSTMC_ol
201800100402     c* Spedizioni "E"
201900100402     C                   Z-ADD     eWTSPEA_r     WSSTSPe
202000100402     C                   Z-ADD     eWTCLEA_r     WSSTCLe
202100100402     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe
202200100402     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe
202300120904     c* old
202400120904     C                   Z-ADD(H)  eWTKGEA_r     WSSTKGe_ol
202500120904     C                   Z-ADD(H)  eWTMCEA_r     WSSTMCe_ol
202600100402     c* Spedizioni "H"
202700100402     C                   Z-ADD     hWTSPEA_r     WSSTSPh
202800100402     C                   Z-ADD     hWTCLEA_r     WSSTCLh
202900100402     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh
203000100402     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh
203100120904     c* old
203200120904     C                   Z-ADD(H)  hWTKGEA_r     WSSTKGh_ol
203300120904     C                   Z-ADD(H)  hWTMCEA_r     WSSTMCh_ol
203400100402     c
203500120905     C                   WRITE     WFLSS600
203600120904     C                   WRITE     WSLSS500
203700090206     C                   ENDIF
203800090206     C*
203900090206     C     WTSPPA_r      IFGT      *ZERO
204000090206     C                   Z-ADD     §dal          WSSDAL
204100090601     c**                 movel     'R'           WSSTIE
204200090601     c                   movel     Wtipo         WSSTIE
204300090206     C                   MOVE      'PA'          WSSTIP
204400090206     C                   Z-ADD     FIL(K)        WSSLIN
204500090206     C                   Z-ADD     WTSPPA_r      WSSTSP
204600090206     C                   Z-ADD     WTCLPA_r      WSSTCL
204700090206     C                   Z-ADD(H)  WTKGPA_r      WSSTKG
204800090206     C                   Z-ADD(H)  WTMCPA_r      WSSTMC
204900120904     c* old
205000120904     C                   Z-ADD(H)  WTKGPA_r      WSSTKG_ol
205100120904     C                   Z-ADD(H)  WTMCPA_r      WSSTMC_ol
205200100402     c* Spedizioni "E"
205300100402     C                   Z-ADD     eWTSPPA_r     WSSTSPe
205400100402     C                   Z-ADD     eWTCLPA_r     WSSTCLe
205500100402     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe
205600100402     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe
205700120904     c* old
205800120904     C                   Z-ADD(H)  eWTKGPA_r     WSSTKGe_ol
205900120904     C                   Z-ADD(H)  eWTMCPA_r     WSSTMCe_ol
206000100402     c* Spedizioni "H"
206100100402     C                   Z-ADD     hWTSPPA_r     WSSTSPh
206200100402     C                   Z-ADD     hWTCLPA_r     WSSTCLh
206300100402     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh
206400100402     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh
206500120904     c* old
206600120904     C                   Z-ADD(H)  hWTKGPA_r     WSSTKGh_ol
206700120904     C                   Z-ADD(H)  hWTMCPA_r     WSSTMCh_ol
206800100402     c
206900120905     C                   WRITE     WFLSS600
207000120904     C                   WRITE     wslss500
207100090206     C                   ENDIF
207200110715     c
207300110715     c                   endif
207400110715     c
207500110715     c* Extra budget e clienti particolari
207600110715     c                   movel     wtipo         tipoC             1
207700110715     c                   if        §stat<>' ' and wtipo='R'
207800110715     c                   eval      tipoC='B'
207900110715     c                   endif
208000110715     c                   if        §stat<>' ' and wtipo='P'
208100110715     c                   eval      tipoC='C'
208200110715     c                   endif
208300110715     c
208400091006     C     WTSPIP_re     IFGT      *ZERO
208500091006     C                   Z-ADD     §dal          WSSDAL
208600110715     c                   movel     TipoC         WSSTIE
208700091006     C                   MOVE      'IP'          WSSTIP
208800091006     C                   Z-ADD     FIL(K)        WSSLIN
208900091006     C                   Z-ADD     WTSPIP_re     WSSTSP
209000091006     C                   Z-ADD     WTCLIP_re     WSSTCL
209100120904     C                   Z-ADD(H)  WTKGIP_re     WSSTKG_ex
209200120904     C                   Z-ADD(H)  WTMCIP_re     WSSTMC_ex
209300100402     c* Spedizioni "E"
209400100402     C                   Z-ADD     eWTSPIP_re    WSSTSPe
209500100402     C                   Z-ADD     eWTCLIP_re    WSSTCLe
209600120904     C                   Z-ADD(H)  eWTKGIP_re    WSSTKGe_ex
209700120904     C                   Z-ADD(H)  eWTMCIP_re    WSSTMCe_ex
209800100402     c* Spedizioni "H"
209900100402     C                   Z-ADD     hWTSPIP_re    WSSTSPh
210000100402     C                   Z-ADD     hWTCLIP_re    WSSTCLh
210100120904     C                   Z-ADD(H)  hWTKGIP_re    WSSTKGh_ex
210200120904     C                   Z-ADD(H)  hWTMCIP_re    WSSTMCh_ex
210300100402     c
210400091006     C                   WRITE     WSLSS550
210500091006     C                   ENDIF
210600100402     c
210700091006     C     WTSPIA_re     IFGT      *ZERO
210800091006     C                   Z-ADD     §dal          WSSDAL
210900110715     c                   movel     tipoC         WSSTIE
211000091006     C                   MOVE      'IA'          WSSTIP
211100091006     C                   Z-ADD     FIL(K)        WSSLIN
211200091006     C                   Z-ADD     WTSPIA_re     WSSTSP
211300091006     C                   Z-ADD     WTCLIA_re     WSSTCL
211400120904     C                   Z-ADD(H)  WTKGIA_re     WSSTKG_ex
211500120904     C                   Z-ADD(H)  WTMCIA_re     WSSTMC_ex
211600100402     c* Spedizioni "E"
211700100402     C                   Z-ADD     eWTSPIA_re    WSSTSPe
211800100402     C                   Z-ADD     eWTCLIA_re    WSSTCLe
211900120904     C                   Z-ADD(H)  eWTKGIA_re    WSSTKGe_ex
212000120904     C                   Z-ADD(H)  eWTMCIA_re    WSSTMCe_ex
212100100402     c* Spedizioni "H"
212200100402     C                   Z-ADD     hWTSPIA_re    WSSTSPh
212300100402     C                   Z-ADD     hWTCLIA_re    WSSTCLh
212400120904     C                   Z-ADD(H)  hWTKGIA_re    WSSTKGh_ex
212500120904     C                   Z-ADD(H)  hWTMCIA_re    WSSTMCh_ex
212600100402     c
212700091006     C                   WRITE     WSLSS550
212800091006     C                   ENDIF
212900100402     c
213000091006     C     WTSPEI_Re     IFGT      *ZERO
213100091006     C                   Z-ADD     §dal          WSSDAL
213200110715     c                   movel     tipoC         WSSTIE
213300091006     C                   MOVE      'EI'          WSSTIP
213400091006     C                   Z-ADD     FIL(K)        WSSLIN
213500091006     C                   Z-ADD     WTSPEI_re     WSSTSP
213600091006     C                   Z-ADD     WTCLEI_re     WSSTCL
213700120904     C                   Z-ADD(H)  WTKGEI_re     WSSTKG_ex
213800120904     C                   Z-ADD(H)  WTMCEI_re     WSSTMC_ex
213900100402     c* Spedizioni "E"
214000100402     C                   Z-ADD     eWTSPEI_re    WSSTSPe
214100100402     C                   Z-ADD     eWTCLEI_re    WSSTCLe
214200120904     C                   Z-ADD(H)  eWTKGEI_re    WSSTKGe_ex
214300120904     C                   Z-ADD(H)  eWTMCEI_re    WSSTMCe_ex
214400100402     c* Spedizioni "H"
214500100402     C                   Z-ADD     hWTSPEI_re    WSSTSPh
214600100402     C                   Z-ADD     hWTCLEI_re    WSSTCLh
214700120904     C                   Z-ADD(H)  hWTKGEI_re    WSSTKGh_ex
214800120904     C                   Z-ADD(H)  hWTMCEI_re    WSSTMCh_ex
214900100402     c
215000091006     C                   WRITE     WSLSS550
215100091006     C                   ENDIF
215200100402     c
215300091006     C     WTSPEE_re     IFGT      *ZERO
215400091006     C                   Z-ADD     §dal          WSSDAL
215500110715     c                   movel     tipoC         WSSTIE
215600091006     C                   MOVE      'EE'          WSSTIP
215700091006     C                   Z-ADD     FIL(K)        WSSLIN
215800091006     C                   Z-ADD     WTSPEE_re     WSSTSP
215900091006     C                   Z-ADD     WTCLEE_re     WSSTCL
216000120904     C                   Z-ADD(H)  WTKGEE_re     WSSTKG_ex
216100120904     C                   Z-ADD(H)  WTMCEE_re     WSSTMC_ex
216200100402     c* Spedizioni "E"
216300100402     C                   Z-ADD     eWTSPEE_re    WSSTSPe
216400100402     C                   Z-ADD     eWTCLEE_re    WSSTCLe
216500120904     C                   Z-ADD(H)  eWTKGEE_re    WSSTKGe_ex
216600120904     C                   Z-ADD(H)  eWTMCEE_re    WSSTMCe_ex
216700100402     c* Spedizioni "h"
216800100402     C                   Z-ADD     hWTSPEE_re    WSSTSPh
216900100402     C                   Z-ADD     hWTCLEE_re    WSSTCLh
217000120904     C                   Z-ADD(H)  hWTKGEE_re    WSSTKGh_ex
217100120904     C                   Z-ADD(H)  hWTMCEE_re    WSSTMCh_ex
217200100402     c
217300091006     C                   WRITE     WSLSS550
217400091006     C                   ENDIF
217500091006     C*
217600091006     C     WTSPED_re     IFGT      *ZERO
217700091006     C                   Z-ADD     §dal          WSSDAL
217800110715     c                   movel     tipoC         WSSTIE
217900091006     C                   MOVE      'ED'          WSSTIP
218000091006     C                   Z-ADD     FIL(K)        WSSLIN
218100091006     C                   Z-ADD     WTSPED_re     WSSTSP
218200091006     C                   Z-ADD     WTCLED_re     WSSTCL
218300120904     C                   Z-ADD(H)  WTKGED_re     WSSTKG_ex
218400120904     C                   Z-ADD(H)  WTMCED_re     WSSTMC_ex
218500100402     c* Spedizioni "E"
218600100402     C                   Z-ADD     eWTSPED_re    WSSTSPe
218700100402     C                   Z-ADD     eWTCLED_re    WSSTCLe
218800120904     C                   Z-ADD(H)  eWTKGED_re    WSSTKGe_ex
218900120904     C                   Z-ADD(H)  eWTMCED_re    WSSTMCe_ex
219000100402     c* Spedizioni "H"
219100100402     C                   Z-ADD     hWTSPED_re    WSSTSPh
219200100402     C                   Z-ADD     hWTCLED_re    WSSTCLh
219300120904     C                   Z-ADD(H)  hWTKGED_re    WSSTKGh_ex
219400120904     C                   Z-ADD(H)  hWTMCED_re    WSSTMCh_ex
219500100402     c
219600091006     C                   WRITE     WSLSS550
219700091006     C                   ENDIF
219800091006     C*
219900091006     C     WTSPEX_re     IFGT      *ZERO
220000091006     C                   Z-ADD     §dal          WSSDAL
220100110715     c                   movel     tipoC         WSSTIE
220200091006     C                   MOVE      'EX'          WSSTIP
220300091006     C                   Z-ADD     FIL(K)        WSSLIN
220400091006     C                   Z-ADD     WTSPEX_re     WSSTSP
220500091006     C                   Z-ADD     WTCLEX_re     WSSTCL
220600120904     C                   Z-ADD(H)  WTKGEX_re     WSSTKG_ex
220700120904     C                   Z-ADD(H)  WTMCEX_re     WSSTMC_ex
220800100402     c* Spedizioni "E"
220900100402     C                   Z-ADD     eWTSPEX_re    WSSTSPe
221000100402     C                   Z-ADD     eWTCLEX_re    WSSTCLe
221100120904     C                   Z-ADD(H)  eWTKGEX_re    WSSTKGe_ex
221200120904     C                   Z-ADD(H)  eWTMCEX_re    WSSTMCe_ex
221300100402     c* Spedizioni "H"
221400100402     C                   Z-ADD     hWTSPEX_re    WSSTSPh
221500100402     C                   Z-ADD     hWTCLEX_re    WSSTCLh
221600120904     C                   Z-ADD(H)  hWTKGEX_re    WSSTKGh_ex
221700120904     C                   Z-ADD(H)  hWTMCEX_re    WSSTMCh_ex
221800100402     c
221900091006     C                   WRITE     WSLSS550
222000091006     C                   ENDIF
222100091006     C*
222200091006     C     WTSPEA_re     IFGT      *ZERO
222300091006     C                   Z-ADD     §dal          WSSDAL
222400110715     c                   movel     tipoC         WSSTIE
222500091006     C                   MOVE      'EA'          WSSTIP
222600091006     C                   Z-ADD     FIL(K)        WSSLIN
222700091006     C                   Z-ADD     WTSPEA_re     WSSTSP
222800091006     C                   Z-ADD     WTCLEA_re     WSSTCL
222900120904     C                   Z-ADD(H)  WTKGEA_re     WSSTKG_ex
223000120904     C                   Z-ADD(H)  WTMCEA_re     WSSTMC_ex
223100100402     c* Spedizioni "E"
223200100402     C                   Z-ADD     eWTSPEA_re    WSSTSPe
223300100402     C                   Z-ADD     eWTCLEA_re    WSSTCLe
223400120904     C                   Z-ADD(H)  eWTKGEA_re    WSSTKGe_ex
223500120904     C                   Z-ADD(H)  eWTMCEA_re    WSSTMCe_ex
223600100402     c* Spedizioni "H"
223700100402     C                   Z-ADD     hWTSPEA_re    WSSTSPh
223800100402     C                   Z-ADD     hWTCLEA_re    WSSTCLh
223900120904     C                   Z-ADD(H)  hWTKGEA_re    WSSTKGh_ex
224000120904     C                   Z-ADD(H)  hWTMCEA_re    WSSTMCh_ex
224100100402     c
224200091006     C                   WRITE     WSLSS550
224300091006     C                   ENDIF
224400091006     C*
224500091006     C     WTSPPA_re     IFGT      *ZERO
224600091006     C                   Z-ADD     §dal          WSSDAL
224700110715     c                   movel     tipoC         WSSTIE
224800091006     C                   MOVE      'PA'          WSSTIP
224900091006     C                   Z-ADD     FIL(K)        WSSLIN
225000091006     C                   Z-ADD     WTSPPA_re     WSSTSP
225100091006     C                   Z-ADD     WTCLPA_re     WSSTCL
225200120904     C                   Z-ADD(H)  WTKGPA_re     WSSTKG_ex
225300120904     C                   Z-ADD(H)  WTMCPA_re     WSSTMC_ex
225400100402     c* Spedizioni "E"
225500100402     C                   Z-ADD     eWTSPPA_re    WSSTSPe
225600100402     C                   Z-ADD     eWTCLPA_re    WSSTCLe
225700120904     C                   Z-ADD(H)  eWTKGPA_re    WSSTKGe_ex
225800120904     C                   Z-ADD(H)  eWTMCPA_re    WSSTMCe_ex
225900100402     c* Spedizioni "H"
226000100402     C                   Z-ADD     hWTSPPA_re    WSSTSPh
226100100402     C                   Z-ADD     hWTCLPA_re    WSSTCLh
226200120904     C                   Z-ADD(H)  hWTKGPA_re    WSSTKGh_ex
226300120904     C                   Z-ADD(H)  hWTMCPA_re    WSSTMCh_ex
226400100402     c
226500091006     C                   WRITE     WSLSS550
226600091006     C                   ENDIF
226700091006     C                   ENDIF
226800100402
226900090206     c                   enddo
227000090206     c                   endsr
227100970527     C**********************************************************
227200970529     C** CARICAMENTO DATI INIZIALI
227300970527     C**********************************************************
227400970529     C     CARINZ        BEGSR
227500970905     C                   SETOFF                                       90
227600970529     C     KTAS          KLIST
227700970529     C                   KFLD                    WAAS
227800970529     C                   KFLD                    WMGS
227900970529     C     KTAB          KLIST
228000970529     C                   KFLD                    TBLKUT
228100970529     C                   KFLD                    TBLCOD
228200970529     C                   KFLD                    TBLKEY
228300970529     C     KTB           KLIST
228400970529     C                   KFLD                    TBLKUT
228500970529     C                   KFLD                    TBLCOD
228600970529     C                   Z-ADD     1             TBLKUT
228700090204     c     kwfsmp        klist
228800090204     c                   kfld                    k_fil
228900090204     c                   kfld                    k_tip
229000090204     c                   kfld                    k_dat
229100090205     C     kar5          KLIST
229200090205     C                   KFLD                    tasaas
229300090205     C                   KFLD                    taslnp
229400090205     C                   KFLD                    tasnrs
229500090205     C                   KFLD                    tasnsp
229600090205     C                   KFLD                    k_trd
229700090209     C                   KFLD                    tasflp
229800120903     c     kss61         klist
229900120903     c                   kfld                    ks_tie
230000120903     c                   kfld                    ks_tip
230100120903     c                   kfld                    ks_lin
230200120903     c                   kfld                    ks_sca
230300120903     c
230400090915     c* apertura file di stampa congelato in SAVE
230500090915     C* ESEGUO OVRPRTF
230600090915     C                   Z-ADD     61            LUNG             15 5
230700090915     C                   MOVEL     *BLANKS       COMMAN           80
230800090915     C                   MOVEA     CMD           COMMAN
230900090915     C                   CALL      'QCMDEXC'                            H1
231000090915     C                   PARM                    COMMAN
231100090915     C                   PARM                    LUNG
231200090915     C*
231300090915     C                   OPEN      QSYSPRT
231400090915     C*
231500970529     C*
231600970529     C** CARICAMENTO DS MULTIPLA  X DATI FILIALE
231700970527     C** PULIZIA DS
231800121127     C                   DO        400           K                 4 0
231900970527     C     K             OCCUR     DSFIL
232000970527     C                   CLEAR                   DSFIL
232100970527     C                   END
232200121127     C                   DO        400           K
232300100402     C     K             OCCUR     DSFIL_e
232400100402     C                   CLEAR                   DSFIL_e
232500100402     C                   END
232600121127     C                   DO        400           K
232700100402     C     K             OCCUR     DSFIL_h
232800100402     C                   CLEAR                   DSFIL_h
232900100402     C                   END
233000121127     C                   DO        400           K
233100090206     C     K             OCCUR     DSFIL_smp
233200090206     C                   CLEAR                   DSFIL_smp
233300090206     C                   END
233400970527     C*
233500970527     C                   Z-ADD     0             K
233600970527     C     *LOVAL        SETLL     AZORG01L
233700970527     C                   DO        *HIVAL
233800090204     C                   READ      AZORG01L
233900090204     c                   if        %eof(azorg01l)
234000090204     c                   leave
234100090204     c                   endif
234200970527     C     ORGFAG        IFEQ      'F'
234300970527     C     ORGFAG        OREQ      'A'
234400970527     C                   ADD       1             K
234500970527     C                   MOVEL     ORGFIL        FIL(K)
234600090204     C                   MOVEL     ORGdes        DIL(K)
234700090204     C                   z-add     ORGcar        car(K)
234800090204     c                   movel     orgde8        og148
234900090206     c                   if        §oglpo='1' or §oglpo='2'
235000090206     c                   move      §oglpo        lpo(K)
235100090206     c                   else
235200090206     c                   z-add     2             lpo(K)
235300090206     c                   endif
235400090204     c                   movel     orgde3        og143
235500090209     c**                 if        §ogntw='DPD' or §ogntw='EEX' or §ogntw='FED'
235600090209     c**                 move      'E'           tif(K)
235700090209     c**                 else
235800090209     c**                 move      'I'           tif(K)
235900090209     c**                 endif
236000090209     c                   select
236100090209     c                   when      §ogntw='DPD'
236200090209     c                   move      'D'           tif(K)
236300090209     c                   when      §ogntw='EEX'
236400090209     c                   move      'X'           tif(K)
236500090209     c                   when      §ogntw='FED'
236600090209     c                   move      'F'           tif(K)
236700090209     c                   other
236800090209     c                   move      'I'           tif(K)
236900090209     c                   endsl
237000970527     C                   ENDIF
237100090204     C                   ENDDO
237200970529     C*
237300970529     C                   Z-ADD     0             K
237400970529     C                   MOVEL     'TB'          TBLCOD
237500970529     C     KTB           SETLL     TABEL00F
237600970529     C                   DO        *HIVAL
237700090204     C     KTB           READE     TABEL00F
237800090204     c                   if        %eof(tabel00f)
237900090204     c                   leave
238000090204     c                   endif
238100970529     C     TBLFLG        IFNE      '*'
238200970529     C                   MOVEL     TBLUNI        DSTB
238300970529     C     §TBRBL        IFEQ      'N'
238400970529     C     §TBRBL        OREQ      'S'
238500970529     C                   ADD       1             K
238600970529     C                   MOVEL     TBLKEY        TBO(K)
238700970529     C                   ENDIF
238800970529     C                   ENDIF
238900090204     C                   ENDDO
239000120903     c*
239100120903     c* Carico scaglioni di peso
239200120903     C                   Z-ADD     0             II
239300120903     C                   MOVEL     '2L'          TBLCOD
239400120903     C     KTB           SETLL     TABEL00F
239500120903    1C                   DO        *HIVAL
239600120903     C     KTB           READE     TABEL00F
239700120903    2c                   if        %eof(tabel00f)
239800120903     c                   leave
239900120903    2c                   endif
240000120903    2C     TBLFLG        IFNE      '*'
240100120903     C                   MOVEL     TBLUNI        DS2l
240200120903     C                   MOVEL     TBLKEY        WTM03             3
240300120904     C                   MOVEL     TBLKEY        WTM05             5
240400120903    3C     WTM03         IFEQ      'S9R'
240500120903     C                   ADD       1             II
240600120903     C                   MOVE      WTM05         FPR(II)
240700120903     C                   Z-ADD     §2LKGI        KIR(II)
240800120903     C                   Z-ADD     §2LKGF        KFR(II)
240900120903    3C                   ENDIF
241000120903    2C                   ENDIF
241100120903     C                   ENDDO
241200091006     C*
241300091006     c* Lettura TABELLA clienti extra budget
241400110711     c                   if        §stat=' '
241500091006     C                   Z-ADD     0             K
241600091006     C                   MOVEL     'CXB'         TBecod
241700091006     C     tbecod        SETLL     tntbe01l
241800091006     C                   DO        *HIVAL
241900091006     C     tbecod        READE     tntbe01l
242000091006     c                   if        %eof(tntbe01l)
242100091006     c                   leave
242200091006     c                   endif
242300091006
242400091006     C     tbeatb        IFeq      ' '
242500091006     C                   MOVEL     TBeuni        Dcxb
242600091006     C                   ADD       1             K
242700091006     C                   MOVEL     TBeKE1        ksc_ex(k)
242800091007     C                   MOVE      §cxbctr       ksc_ex(k)
242900091006     C                   ENDIF
243000091006     C                   ENDDO
243100110711     c
243200110711     c                   else
243300121121     c* Lettura TABELLA clienti Particolari
243400110711     C                   Z-ADD     0             K
243500110711     C                   MOVEL     'CXQ'         TBecod
243600110711     C     tbecod        SETLL     tntbe01l
243700110711     C                   DO        *HIVAL
243800110711     C     tbecod        READE     tntbe01l
243900110711     c                   if        %eof(tntbe01l)
244000110711     c                   leave
244100110711     c                   endif
244200110711
244300110711     C     tbeatb        IFeq      ' '
244400110711     C                   ADD       1             K
244500110711     C                   MOVEL     TBeKE2        ksc_ex(k)
244600121121     c                   movel     tbeuni        dcxq
244700121121     c                   if        §cxqterm>*zeros
244800121121     c                   movel     §cxqterm      ksc_tr(k)
244900121121     c                   else
245000121121     c                   movel     999           ksc_tr(k)
245100121121     c                   endif
245200121121     c
245300110711     C                   ENDIF
245400110711     C                   ENDDO
245500110711     c                   endif
245600110711     c*
245700110711     c* Controllo riempimento schiera
245800110715     c                   movel     kpjbu         savkpjbu
245900110711     c                   clear                   trul0sds
246000110711     c                   eval      i0stla='L'
246100110711     c                   eval      i0sski='KSC_EX'
246200110711     c                   eval      i0sele=%elem(ksc_ex)
246300110711     c                   eval      i0spie=k
246400110711     c                   eval      i0sfile='Cienti tab CXB-CXQ'
246500110711     c                   eval      i0ssif=knsif
246600110711     c                   eval      i0spgm='TNSSL3R2'
246700110711     c                   movel     trul0sds      kpjbu
246800110711     c                   call      'TRUL0SR'
246900110711     c                   parm                    kpjba
247000110715
247100110715     c                   movel     savkpjbu      kpjbu
247200090601     c
247300090204     c* data elaborazione nel formato AAAA/MM/GG
247400090204     c                   time                    dataiso
247500090204     c                   move      dataiso       wdatael
247600110711     c
247700110711     c                   if        §stat=' '
247800090601     c*
247900090601     c* Pulisco i file per i dati che devo rielaborare
248000090601     c                   eval      wrkgetlista =
248100120905     c                             'delete from WFLSS60F where wsstie= '''   +
248200090603     c                              Wtipo +  ''''
248300090601     c                   if        Wtipo='R'
248400090601     c                   eval      wrkgetlista =%trim(wrkgetlista)+
248500090603     c                              ' or  wsstie=''G'''
248600090601     c                   endif
248700090609     c*
248800090601     C/EXEC SQL
248900090601     C+ execute immediate :wrkgetlista
249000090601     C/END-EXEC
249100090609     c
249200120904     c*
249300120904     c* Pulisco i file per i dati che devo rielaborare
249400120904     c                   eval      wrkgetlista =
249500120904     c                             'delete from wslss50F where wsstie= '''   +
249600120904     c                              Wtipo +  ''''
249700120904     c                   if        Wtipo='R'
249800120904     c                   eval      wrkgetlista =%trim(wrkgetlista)+
249900120904     c                              ' or  wsstie=''G'''
250000120904     c                   endif
250100120904     c*
250200120904     C/EXEC SQL
250300120904     C+ execute immediate :wrkgetlista
250400120904     C/END-EXEC
250500120904     c*
250600090609     c* Pulisco i file per i dati che devo rielaborare
250700090609     c                   clear                   wrkgetlista
250800090609     c                   eval      wrkgetlista =
250900090609     c                             'delete from WFSMP00F where wmptip= '''   +
251000090609     c                              Wtipo +  ''''
251100090609     c                   if        Wtipo='R'
251200090609     c                   eval      wrkgetlista =%trim(wrkgetlista)+
251300090609     c                              ' or  wmptip=''G'''
251400090609     c                   endif
251500110711     c
251600090609     c*
251700090609     C/EXEC SQL
251800090609     C+ execute immediate :wrkgetlista
251900090609     C/END-EXEC
252000120904     c                   endif
252100091007     c*
252200091007     c* Pulisco il file clienti extra budget
252300091007     c                   clear                   wrkgetlista
252400110711     c
252500110711     c                   if        §stat=' '
252600091007     c                   eval      wrkgetlista =
252700120903     c                             'delete from wslss55F where wsstie= '''   +
252800091007     c                              Wtipo +  ''''
252900091007     c                   if        Wtipo='R'
253000091007     c                   eval      wrkgetlista =%trim(wrkgetlista)+
253100091007     c                              ' or  wsstie=''G'''
253200091007     c                   endif
253300111028     C/EXEC SQL
253400111028     C+ execute immediate :wrkgetlista
253500111028     C/END-EXEC
253600110711     c
253700110711     c                   else
253800111028     c
253900111028     c* Pulisco il file clienti particolari
254000110711     c                   select
254100110711     c                   when      Wtipo='R'
254200110711     c                   eval      wrkgetlista =
254300110715     c                             'delete from WSLSS55F where wsstie= ''A'' or-
254400110711     c                               wsstie= ''B'''
254500110711     c                   when      Wtipo='G'
254600110711     c                   eval      wrkgetlista =
254700110711     c                             'delete from WSLSS55F where wsstie= ''A'''
254800110711     c                   when      Wtipo='P'
254900110711     c                   eval      wrkgetlista =
255000110711     c                             'delete from WSLSS55F where wsstie= ''C'''
255100110711     c                   endsl
255200110711     c
255300091007     C/EXEC SQL
255400091007     C+ execute immediate :wrkgetlista
255500091007     C/END-EXEC
255600111028     c
255700111028     c* Preparo la lettura dei soli clienti particolari con SQL
255800111028     c                   clear                   wrkgetlista
255900111028     c                   eval      wrkgetlista =
256000111028     c                             'select * from titas00f where +
256100111028     c                             ((tasaas*10000) +tasmgs) >=' +
256200111028     c                             %editc(§dal:'X')+ ' and +
256300111028     c                             ((tasaas*10000) +tasmgs) <=' +
256400111028     c                             %editc(§al :'X')+ ' and tasccm in('
256500111028      * ciclo sulla sk dei cod bolla di recupero
256600121203     c                   Do        1100          XX
256700111028     c                   If        ksc_ex(XX) = *blanks
256800111028     c                   Leave
256900111028     c                   EndIf
257000111028     c
257100111028     c                   If        XX >1
257200111028     c                   eval      wrkgetlista =
257300111028     c                             %trim(wrkgetlista) + ','
257400111028     c                   EndIf
257500111028     c
257600111028     c                   eval      wrkgetlista =
257700111028     c                             %trim(wrkgetlista) + ' ''' +
257800111028     c                             %subst(ksc_ex(xx):1:7) + ''''
257900111028     c                   EndDo
258000111028     c
258100111028     c                   Eval      wrkgetlista =
258200111028     c                             %trim(wrkgetlista) + ')'
258300111028     c
258400111028     c
258500111028     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
258600111028     c                             'select * from titas10f where +
258700111028     c                             ((tasaas*10000) +tasmgs) >=' +
258800111028     c                             %editc(§dal:'X')+ ' and +
258900111028     c                             ((tasaas*10000) +tasmgs) <=' +
259000111028     c                             %editc(§al :'X')+ ' and tasccm in('
259100111028      * ciclo sulla sk dei cod bolla di recupero
259200121203     c                   Do        1100          XX                4 0
259300111028     c                   If        ksc_ex(XX) = *blanks
259400111028     c                   Leave
259500111028     c                   EndIf
259600111028     c
259700111028     c                   If        XX >1
259800111028     c                   eval      wrkgetlista =
259900111028     c                             %trim(wrkgetlista) + ','
260000111028     c                   EndIf
260100111028     c
260200111028     c                   eval      wrkgetlista =
260300111028     c                             %trim(wrkgetlista) + ' ''' +
260400111028     c                             %subst(ksc_ex(xx):1:7) + ''''
260500111028     c                   EndDo
260600111028     c
260700111028     c                   Eval      wrkgetlista =
260800111028     c                             %trim(wrkgetlista) + ')'
260900111028     c
261000111028     c                   EndIf
261100111028     c
261200970527     C                   ENDSR
261300100402     c****************************************************************************
261400130408     c     Prepasql2     BEGSR
261500130408     c* Preparo la lettura dei soli clienti particolari con SQL
261600130408     c                   clear                   wrkgetlista
261700130408     c                   eval      wrkgetlista =
261800130408     c                             'select * from titas00f where +
261900130408     c                             ((tasaas*10000) +tasmgs) >=' +
262000130408     c                             %editc(§dal:'X')+ ' and +
262100130408     c                             ((tasaas*10000) +tasmgs) <=' +
262200130408     c                             %editc(§al :'X')+ ' and substr(TASTBL, +
262300130408     c                             1, 1)=''A'' and tasksc in('
262400130408      * ciclo sulla sk dei cod bolla di recupero
262500130408     c                   Do        1100          XX
262600130408     c                   If        ksc_ex(XX) = *blanks
262700130408     c                   Leave
262800130408     c                   EndIf
262900130408     c
263000130408     c                   If        XX >1
263100130408     c                   eval      wrkgetlista =
263200130408     c                             %trim(wrkgetlista) + ','
263300130408     c                   EndIf
263400130408     c
263500130408     c                   eval      wrkgetlista =
263600130408     c                             %trim(wrkgetlista) + ' ''' +
263700130408     c                             %subst(ksc_ex(xx):1:7) + ''''
263800130408     c                   EndDo
263900130408     c
264000130408     c                   Eval      wrkgetlista =
264100130408     c                             %trim(wrkgetlista) + ')'
264200130408     c
264300130408     c
264400130408     c                   eval      wrkgetlista =%trimr(wrkgetlista)+' union '+
264500130408     c                             'select * from titas10f where +
264600130408     c                             ((tasaas*10000) +tasmgs) >=' +
264700130408     c                             %editc(§dal:'X')+ ' and +
264800130408     c                             ((tasaas*10000) +tasmgs) <=' +
264900130408     c                             %editc(§al :'X')+ ' and substr(TASTBL, +
265000130408     c                             1, 1)=''A'' and tasksc in('
265100130408      * ciclo sulla sk dei cod bolla di recupero
265200130408     c                   Do        1100          XX                4 0
265300130408     c                   If        ksc_ex(XX) = *blanks
265400130408     c                   Leave
265500130408     c                   EndIf
265600130408     c
265700130408     c                   If        XX >1
265800130408     c                   eval      wrkgetlista =
265900130408     c                             %trim(wrkgetlista) + ','
266000130408     c                   EndIf
266100130408     c
266200130408     c                   eval      wrkgetlista =
266300130408     c                             %trim(wrkgetlista) + ' ''' +
266400130408     c                             %subst(ksc_ex(xx):1:7) + ''''
266500130408     c                   EndDo
266600130408     c
266700130408     c                   Eval      wrkgetlista =
266800130408     c                             %trim(wrkgetlista) + ')'
266900130408     c
267000130408     c
267100130408     C                   ENDSR
267200130408     c****************************************************************************
267300100402     c     SpeITA        BEGSR
267400100402     c* giornaliero
267500100402     c                   if        (tasaas*10000+tasmgs)=§al
267600100402     C                   ADD       1             WTSPIP
267700100402     C                   ADD       TASNCL        WTCLIP
267800100402     C                   ADD       w_peso        WTKGIP
267900100402     C                   ADD       TASVLF        WTMCIP
268000100402     c* tipo servizio  "E"
268100100402     c                   if        tastsp='E'
268200100402     C                   ADD       1             eWTSPIP
268300100402     C                   ADD       TASNCL        eWTCLIP
268400100402     C                   ADD       w_peso        eWTKGIP
268500100402     C                   ADD       TASVLF        eWTMCIP
268600100402     c                   endif
268700100402     c* tipo servizio  "H"
268800100402     c                   if        tastsp='H'
268900100402     C                   ADD       1             hWTSPIP
269000100402     C                   ADD       TASNCL        hWTCLIP
269100100402     C                   ADD       w_peso        hWTKGIP
269200100402     C                   ADD       TASVLF        hWTMCIP
269300100402     c                   endif
269400100402     c**
269500100402     c* Extra budget
269600100402     c                   if        *in35
269700100402     C                   ADD       1             WTSPIP_e
269800100402     C                   ADD       TASNCL        WTCLIP_e
269900100402     C                   ADD       w_peso        WTKGIP_e
270000100402     C                   ADD       TASVLF        WTMCIP_e
270100100402     c* tipo servizio  "E"
270200100402     c                   if        tastsp='E'
270300100402     C                   ADD       1             eWTSPIP_e
270400100402     C                   ADD       TASNCL        eWTCLIP_e
270500100402     C                   ADD       w_peso        eWTKGIP_e
270600100402     C                   ADD       TASVLF        eWTMCIP_e
270700100402     c                   endif
270800100402     c* tipo servizio  "H"
270900100402     c                   if        tastsp='H'
271000100402     C                   ADD       1             hWTSPIP_e
271100100402     C                   ADD       TASNCL        hWTCLIP_e
271200100402     C                   ADD       w_peso        hWTKGIP_e
271300100402     C                   ADD       TASVLF        hWTMCIP_e
271400100402     c                   endif
271500100402
271600100402     c                   endif
271700100402     c                   endif
271800100402     c* riepilogo
271900100402     C                   ADD       1             WTSPIP_r
272000100402     C                   ADD       TASNCL        WTCLIP_r
272100100402     C                   ADD       w_peso        WTKGIP_r
272200100402     C                   ADD       TASVLF        WTMCIP_r
272300100402     c* tipo servizio  "E"
272400100402     c                   if        tastsp='E'
272500100402     C                   ADD       1             eWTSPIP_r
272600100402     C                   ADD       TASNCL        eWTCLIP_r
272700100402     C                   ADD       w_peso        eWTKGIP_r
272800100402     C                   ADD       TASVLF        eWTMCIP_r
272900100402     c                   endif
273000100402     c* tipo servizio  "H"
273100100402     c                   if        tastsp='H'
273200100402     C                   ADD       1             hWTSPIP_r
273300100402     C                   ADD       TASNCL        hWTCLIP_r
273400100402     C                   ADD       w_peso        hWTKGIP_r
273500100402     C                   ADD       TASVLF        hWTMCIP_r
273600100402     c                   endif
273700100402     c* Extra budget
273800100402     c                   if        *in35
273900100402     C                   ADD       1             WTSPIP_re
274000100402     C                   ADD       TASNCL        WTCLIP_re
274100100402     C                   ADD       w_peso        WTKGIP_re
274200100402     C                   ADD       TASVLF        WTMCIP_re
274300100402     c* tipo servizio  "E"
274400100402     c                   if        tastsp='E'
274500100402     C                   ADD       1             eWTSPIP_re
274600100402     C                   ADD       TASNCL        eWTCLIP_re
274700100402     C                   ADD       w_peso        eWTKGIP_re
274800100402     C                   ADD       TASVLF        eWTMCIP_re
274900100402     c                   endif
275000100402     c* tipo servizio  "H"
275100100402     c                   if        tastsp='H'
275200100402     C                   ADD       1             hWTSPIP_re
275300100402     C                   ADD       TASNCL        hWTCLIP_re
275400100402     C                   ADD       w_peso        hWTKGIP_re
275500100402     C                   ADD       TASVLF        hWTMCIP_re
275600100402     c                   endif
275700100402     c
275800100402     c                   endif
275900120903     c
276000120903     c                   eval      ks_tip='IP'
276100120903     c                   exsr      spe_peso
276200120903     c
276300100402     c                   ENDSR
276400100402     c****************************************************************************
276500100402     c     SpeIMPORT     BEGSR
276600100402     c* giornaliero
276700100402     c                   if        (tasaas*10000+tasmgs)=§al
276800100402     C                   ADD       1             WTSPEI
276900100402     C                   ADD       TASNCL        WTCLEI
277000100402     C                   ADD       w_peso        WTKGEI
277100100402     C                   ADD       TASVLF        WTMCEI
277200100402     c* tipo servizio  "E"
277300100402     c                   if        tastsp='E'
277400100402     C                   ADD       1             eWTSPEI
277500100402     C                   ADD       TASNCL        eWTCLEI
277600100402     C                   ADD       w_peso        eWTKGEI
277700100402     C                   ADD       TASVLF        eWTMCEI
277800100402     c                   endif
277900100402     c* tipo servizio  "H"
278000100402     c                   if        tastsp='H'
278100100402     C                   ADD       1             hWTSPEI
278200100402     C                   ADD       TASNCL        hWTCLEI
278300100402     C                   ADD       w_peso        hWTKGEI
278400100402     C                   ADD       TASVLF        hWTMCEI
278500100402     c                   endif
278600100402     c* Extra Budget
278700100402     c                   if        *in35
278800100402     C                   ADD       1             WTSPEI_e
278900100402     C                   ADD       TASNCL        WTCLEI_e
279000100402     C                   ADD       w_peso        WTKGEI_e
279100100402     C                   ADD       TASVLF        WTMCEI_e
279200100402     c* tipo servizio  "E"
279300100402     c                   if        tastsp='E'
279400100402     C                   ADD       1             eWTSPEI_e
279500100402     C                   ADD       TASNCL        eWTCLEI_e
279600100402     C                   ADD       w_peso        eWTKGEI_e
279700100402     C                   ADD       TASVLF        eWTMCEI_e
279800100402     c                   endif
279900100402     c* tipo servizio  "H"
280000100402     c                   if        tastsp='H'
280100100402     C                   ADD       1             hWTSPEI_e
280200100402     C                   ADD       TASNCL        hWTCLEI_e
280300100402     C                   ADD       w_peso        hWTKGEI_e
280400100402     C                   ADD       TASVLF        hWTMCEI_e
280500100402     c                   endif
280600100402     c
280700100402     c                   endif
280800100402     c                   endif
280900100402     c* riepilogo
281000100402     C                   ADD       1             WTSPEI_r
281100100402     C                   ADD       TASNCL        WTCLEI_r
281200100402     C                   ADD       w_peso        WTKGEI_r
281300100402     C                   ADD       TASVLF        WTMCEI_r
281400100402     c* tipo servizio  "E"
281500100402     c                   if        tastsp='E'
281600100402     C                   ADD       1             eWTSPEI_r
281700100402     C                   ADD       TASNCL        eWTCLEI_r
281800100402     C                   ADD       w_peso        eWTKGEI_r
281900100402     C                   ADD       TASVLF        eWTMCEI_r
282000100402     c                   endif
282100100402     c* tipo servizio  "H"
282200100402     c                   if        tastsp='H'
282300100402     C                   ADD       1             hWTSPEI_r
282400100402     C                   ADD       TASNCL        hWTCLEI_r
282500100402     C                   ADD       w_peso        hWTKGEI_r
282600100402     C                   ADD       TASVLF        hWTMCEI_r
282700100402     c                   endif
282800100402     c* extra budget
282900100402     c                   if        *in35
283000100402     C                   ADD       1             WTSPEI_re
283100100402     C                   ADD       TASNCL        WTCLEI_re
283200100402     C                   ADD       w_peso        WTKGEI_re
283300100402     C                   ADD       TASVLF        WTMCEI_re
283400100402     c* tipo servizio  "E"
283500100402     c                   if        tastsp='E'
283600100402     C                   ADD       1             eWTSPEI_re
283700100402     C                   ADD       TASNCL        eWTCLEI_re
283800100402     C                   ADD       w_peso        eWTKGEI_re
283900100402     C                   ADD       TASVLF        eWTMCEI_re
284000100402     c                   endif
284100100402     c* tipo servizio  "H"
284200100402     c                   if        tastsp='H'
284300100402     C                   ADD       1             hWTSPEI_re
284400100402     C                   ADD       TASNCL        hWTCLEI_re
284500100402     C                   ADD       w_peso        hWTKGEI_re
284600100402     C                   ADD       TASVLF        hWTMCEI_re
284700100402     c                   endif
284800100402     c                   endif
284900120904     c
285000120904     c                   eval      ks_tip='EI'
285100120904     c                   exsr      spe_peso
285200120904     c
285300100402     c                   ENDSR
285400100402     c****************************************************************************
285500100402     c     speEXPRESS    BEGSR
285600100402     c* giornaliero
285700100402     c                   if        (tasaas*10000+tasmgs)=§al
285800100402     C                   ADD       1             WTSPEE                         EUROEXPRESS
285900100402     C                   ADD       TASNCL        WTCLEE
286000100402     C                   ADD       w_peso        WTKGEE
286100100402     C                   ADD       TASVLF        WTMCEE
286200100402     c* tipo servizio  "E"
286300100402     c                   if        tastsp='E'
286400100402     C                   ADD       1             eWTSPEE                         EUROEXPRESS
286500100402     C                   ADD       TASNCL        eWTCLEE
286600100402     C                   ADD       w_peso        eWTKGEE
286700100402     C                   ADD       TASVLF        eWTMCEE
286800100402     c                   endif
286900100402     c* tipo servizio  "H"
287000100402     c                   if        tastsp='H'
287100100402     C                   ADD       1             hWTSPEE                         EUROEXPRESS
287200100402     C                   ADD       TASNCL        hWTCLEE
287300100402     C                   ADD       w_peso        hWTKGEE
287400100402     C                   ADD       TASVLF        hWTMCEE
287500100402     c                   endif
287600100402     c* extra budget
287700100402     c                   if        *in35
287800100402     C                   ADD       1             WTSPEE_e                       EUROEXPRESS
287900100402     C                   ADD       TASNCL        WTCLEE_e
288000100402     C                   ADD       w_peso        WTKGEE_e
288100100402     C                   ADD       TASVLF        WTMCEE_e
288200100402     c* tipo servizio  "E"
288300100402     c                   if        tastsp='E'
288400100402     C                   ADD       1             eWTSPEE_e                       EUROEXPRESS
288500100402     C                   ADD       TASNCL        eWTCLEE_e
288600100402     C                   ADD       w_peso        eWTKGEE_e
288700100402     C                   ADD       TASVLF        eWTMCEE_e
288800100402     c                   endif
288900100402     c* tipo servizio  "H"
289000100402     c                   if        tastsp='H'
289100100402     C                   ADD       1             hWTSPEE_e                       EUROEXPRESS
289200100402     C                   ADD       TASNCL        hWTCLEE_e
289300100402     C                   ADD       w_peso        hWTKGEE_e
289400100402     C                   ADD       TASVLF        hWTMCEE_e
289500100402     c                   endif
289600100402     c
289700100402     c                   endif
289800100402     c                   endif
289900100402     c* riepilogo
290000100402     C                   ADD       1             WTSPEE_r                       EUROEXPRESS
290100100402     C                   ADD       TASNCL        WTCLEE_r
290200100402     C                   ADD       w_peso        WTKGEE_r
290300100402     C                   ADD       TASVLF        WTMCEE_r
290400100402     c* tipo servizio  "E"
290500100402     c                   if        tastsp='E'
290600100402     C                   ADD       1             eWTSPEE_r                       EUROEXPRESS
290700100402     C                   ADD       TASNCL        eWTCLEE_r
290800100402     C                   ADD       w_peso        eWTKGEE_r
290900100402     C                   ADD       TASVLF        eWTMCEE_r
291000100402     c                   endif
291100100402     c* tipo servizio  "H"
291200100402     c                   if        tastsp='H'
291300100402     C                   ADD       1             hWTSPEE_r                       EUROEXPRESS
291400100402     C                   ADD       TASNCL        hWTCLEE_r
291500100402     C                   ADD       w_peso        hWTKGEE_r
291600100402     C                   ADD       TASVLF        hWTMCEE_r
291700100402     c                   endif
291800100402     c* extra budget
291900100402     c                   if        *in35
292000100402     C                   ADD       1             WTSPEE_re                       EUROEXPRESS
292100100402     C                   ADD       TASNCL        WTCLEE_re
292200100402     C                   ADD       w_peso        WTKGEE_re
292300100402     C                   ADD       TASVLF        WTMCEE_re
292400100402     c* tipo servizio  "E"
292500100402     c                   if        tastsp='E'
292600100402     C                   ADD       1             eWTSPEE_re                       EUROEXPRESS
292700100402     C                   ADD       TASNCL        eWTCLEE_re
292800100402     C                   ADD       w_peso        eWTKGEE_re
292900100402     C                   ADD       TASVLF        eWTMCEE_re
293000100402     c                   endif
293100100402     c* tipo servizio  "H"
293200100402     c                   if        tastsp='H'
293300100402     C                   ADD       1             hWTSPEE_re                       EUROEXPRESS
293400100402     C                   ADD       TASNCL        hWTCLEE_re
293500100402     C                   ADD       w_peso        hWTKGEE_re
293600100402     C                   ADD       TASVLF        hWTMCEE_re
293700100402     c                   endif
293800100402     c                   endif
293900120904     c
294000120904     c                   eval      ks_tip='EE'
294100120904     c                   exsr      spe_peso
294200100402     c
294300100402     c                   ENDSR
294400100402     c****************************************************************************
294500100402     c     speDPD        BEGSR
294600100402     c* giornaliero
294700100402     c                   if        (tasaas*10000+tasmgs)=§al
294800100402     C                   ADD       1             WTSPED                         DPD
294900100402     C                   ADD       TASNCL        WTCLED
295000100402     C                   ADD       w_peso        WTKGED
295100100402     C                   ADD       TASVLF        WTMCED
295200100402     c* tipo servizio  "E"
295300100402     c                   if        tastsp='E'
295400100402     C                   ADD       1             eWTSPED                         DPD
295500100402     C                   ADD       TASNCL        eWTCLED
295600100402     C                   ADD       w_peso        eWTKGED
295700100402     C                   ADD       TASVLF        eWTMCED
295800100402     c                   endif
295900100402     c* tipo servizio  "H"
296000100402     c                   if        tastsp='H'
296100100402     C                   ADD       1             hWTSPED                         DPD
296200100402     C                   ADD       TASNCL        hWTCLED
296300100402     C                   ADD       w_peso        hWTKGED
296400100402     C                   ADD       TASVLF        hWTMCED
296500100402     c                   endif
296600100402     c* extra budget
296700100402     c                   if        *in35
296800100402     C                   ADD       1             WTSPED_e                       DPD
296900100402     C                   ADD       TASNCL        WTCLED_e
297000100402     C                   ADD       w_peso        WTKGED_e
297100100402     C                   ADD       TASVLF        WTMCED_e
297200100402     c* tipo servizio  "E"
297300100402     c                   if        tastsp='E'
297400100402     C                   ADD       1             eWTSPED_e                       DPD
297500100402     C                   ADD       TASNCL        eWTCLED_e
297600100402     C                   ADD       w_peso        eWTKGED_e
297700100402     C                   ADD       TASVLF        eWTMCED_e
297800100402     c                   endif
297900100402     c* tipo servizio  "H"
298000100402     c                   if        tastsp='H'
298100100402     C                   ADD       1             hWTSPED_e                       DPD
298200100402     C                   ADD       TASNCL        hWTCLED_e
298300100402     C                   ADD       w_peso        hWTKGED_e
298400100402     C                   ADD       TASVLF        hWTMCED_e
298500100402     c                   endif
298600100402     c
298700100402     c                   endif
298800100402     c                   endif
298900100402     c* riepilogo
299000100402     C                   ADD       1             WTSPED_r                       DPD
299100100402     C                   ADD       TASNCL        WTCLED_r
299200100402     C                   ADD       w_peso        WTKGED_r
299300100402     C                   ADD       TASVLF        WTMCED_r
299400100402     c* tipo servizio  "E"
299500100402     c                   if        tastsp='E'
299600100402     C                   ADD       1             eWTSPED_r                       DPD
299700100402     C                   ADD       TASNCL        eWTCLED_r
299800100402     C                   ADD       w_peso        eWTKGED_r
299900100402     C                   ADD       TASVLF        eWTMCED_r
300000100402     c                   endif
300100100402     c* tipo servizio  "H"
300200100402     c                   if        tastsp='H'
300300100402     C                   ADD       1             hWTSPED_r                       DPD
300400100402     C                   ADD       TASNCL        hWTCLED_r
300500100402     C                   ADD       w_peso        hWTKGED_r
300600100402     C                   ADD       TASVLF        hWTMCED_r
300700100402     c                   endif
300800100402     c* extra budget
300900100402     c                   if        *in35
301000100402     C                   ADD       1             WTSPED_re                       DPD
301100100402     C                   ADD       TASNCL        WTCLED_re
301200100402     C                   ADD       w_peso        WTKGED_re
301300100402     C                   ADD       TASVLF        WTMCED_re
301400100402     c* tipo servizio  "E"
301500100402     c                   if        tastsp='E'
301600100402     C                   ADD       1             eWTSPED_re                       DPD
301700100402     C                   ADD       TASNCL        eWTCLED_re
301800100402     C                   ADD       w_peso        eWTKGED_re
301900100402     C                   ADD       TASVLF        eWTMCED_re
302000100402     c                   endif
302100100402     c* tipo servizio  "H"
302200100402     c                   if        tastsp='H'
302300100402     C                   ADD       1             hWTSPED_re                       DPD
302400100402     C                   ADD       TASNCL        hWTCLED_re
302500100402     C                   ADD       w_peso        hWTKGED_re
302600100402     C                   ADD       TASVLF        hWTMCED_re
302700100402     c                   endif
302800100402     c                   endif
302900120904     c
303000120904     c                   eval      ks_tip='ED'
303100120904     c                   exsr      spe_peso
303200120904     c
303300100402     c                   ENDSR
303400100402     c****************************************************************************
303500100402     c     speFEDEX      BEGSR
303600100402     c* giornaliero
303700100402     c                   if        (tasaas*10000+tasmgs)=§al
303800100402     C                   ADD       1             WTSPEX                         FEDEX
303900100402     C                   ADD       TASNCL        WTCLEX
304000100402     C                   ADD       w_peso        WTKGEX
304100100402     C                   ADD       TASVLF        WTMCEX
304200100402     c* tipo servizio  "E"
304300100402     c                   if        tastsp='E'
304400100402     C                   ADD       1             eWTSPEX                         FEDEX
304500100402     C                   ADD       TASNCL        eWTCLEX
304600100402     C                   ADD       w_peso        eWTKGEX
304700100402     C                   ADD       TASVLF        eWTMCEX
304800100402     c                   endif
304900100402     c* tipo servizio  "H"
305000100402     c                   if        tastsp='H'
305100100402     C                   ADD       1             hWTSPEX                         FEDEX
305200100402     C                   ADD       TASNCL        hWTCLEX
305300100402     C                   ADD       w_peso        hWTKGEX
305400100402     C                   ADD       TASVLF        hWTMCEX
305500100402     c                   endif
305600100402     c* extra budget
305700100402     c                   if        *in35
305800100402     C                   ADD       1             WTSPEX_e                       FEDEX
305900100402     C                   ADD       TASNCL        WTCLEX_e
306000100402     C                   ADD       w_peso        WTKGEX_e
306100100402     C                   ADD       TASVLF        WTMCEX_e
306200100402     c* tipo servizio  "E"
306300100402     c                   if        tastsp='E'
306400100402     C                   ADD       1             eWTSPEX_e                       FEDEX
306500100402     C                   ADD       TASNCL        eWTCLEX_e
306600100402     C                   ADD       w_peso        eWTKGEX_e
306700100402     C                   ADD       TASVLF        eWTMCEX_e
306800100402     c                   endif
306900100402     c* tipo servizio  "H"
307000100402     c                   if        tastsp='H'
307100100402     C                   ADD       1             hWTSPEX_e                       FEDEX
307200100402     C                   ADD       TASNCL        hWTCLEX_e
307300100402     C                   ADD       w_peso        hWTKGEX_e
307400100402     C                   ADD       TASVLF        hWTMCEX_e
307500100402     c                   endif
307600100402     c                   endif
307700100402     c                   endif
307800100402     c* riepilogo
307900100402     C                   ADD       1             WTSPEX_r                       FEDEX
308000100402     C                   ADD       TASNCL        WTCLEX_r
308100100402     C                   ADD       w_peso        WTKGEX_r
308200100402     C                   ADD       TASVLF        WTMCEX_r
308300100402     c* tipo servizio  "E"
308400100402     c                   if        tastsp='E'
308500100402     C                   ADD       1             eWTSPEX_r                       FEDEX
308600100402     C                   ADD       TASNCL        eWTCLEX_r
308700100402     C                   ADD       w_peso        eWTKGEX_r
308800100402     C                   ADD       TASVLF        eWTMCEX_r
308900100402     c                   endif
309000100402     c* tipo servizio  "H"
309100100402     c                   if        tastsp='H'
309200100402     C                   ADD       1             hWTSPEX_r                       FEDEX
309300100402     C                   ADD       TASNCL        hWTCLEX_r
309400100402     C                   ADD       w_peso        hWTKGEX_r
309500100402     C                   ADD       TASVLF        hWTMCEX_r
309600100402     c                   endif
309700100402     c* Extra budget
309800100402     c                   if        *in35
309900100402     C                   ADD       1             WTSPEX_re                       FEDEX
310000100402     C                   ADD       TASNCL        WTCLEX_re
310100100402     C                   ADD       w_peso        WTKGEX_re
310200100402     C                   ADD       TASVLF        WTMCEX_re
310300100402     c* tipo servizio  "E"
310400100402     c                   if        tastsp='E'
310500100402     C                   ADD       1             eWTSPEX_re                       FEDEX
310600100402     C                   ADD       TASNCL        eWTCLEX_re
310700100402     C                   ADD       w_peso        eWTKGEX_re
310800100402     C                   ADD       TASVLF        eWTMCEX_re
310900100402     c                   endif
311000100402     c* tipo servizio  "H"
311100100402     c                   if        tastsp='H'
311200100402     C                   ADD       1             hWTSPEX_re                       FEDEX
311300100402     C                   ADD       TASNCL        hWTCLEX_re
311400100402     C                   ADD       w_peso        hWTKGEX_re
311500100402     C                   ADD       TASVLF        hWTMCEX_re
311600100402     c                   endif
311700100402     c
311800100402     c                   endif
311900120904     c
312000120904     c                   eval      ks_tip='EX'
312100120904     c                   exsr      spe_peso
312200120904     c
312300100402     c                   ENDSR
312400100402     c****************************************************************************
312500100402     c     speITA_LNA    BEGSR
312600100402     c* giornaliero
312700100402     c                   if        (tasaas*10000+tasmgs)=§al
312800100402     C                   ADD       1             WTSPIA
312900100402     C                   ADD       TASNCL        WTCLIA
313000100402     C                   ADD       W_peso        WTKGIA
313100100402     C                   ADD       TASVLF        WTMCIA
313200100402     c* tipo servizio  "E"
313300100402     c                   if        tastsp='E'
313400100402     C                   ADD       1             eWTSPIA
313500100402     C                   ADD       TASNCL        eWTCLIA
313600100402     C                   ADD       W_peso        eWTKGIA
313700100402     C                   ADD       TASVLF        eWTMCIA
313800100402     c                   endif
313900100402     c* tipo servizio  "H"
314000100402     c                   if        tastsp='H'
314100100402     C                   ADD       1             hWTSPIA
314200100402     C                   ADD       TASNCL        hWTCLIA
314300100402     C                   ADD       W_peso        hWTKGIA
314400100402     C                   ADD       TASVLF        hWTMCIA
314500100402     c                   endif
314600100402     c* extra budget
314700100402     c                   if        *in35
314800100402     C                   ADD       1             WTSPIA_e
314900100402     C                   ADD       TASNCL        WTCLIA_e
315000100402     C                   ADD       w_peso        WTKGIA_e
315100100402     C                   ADD       TASVLF        WTMCIA_e
315200100402     c* tipo servizio  "E"
315300100402     c                   if        tastsp='E'
315400100402     C                   ADD       1             eWTSPIA_e
315500100402     C                   ADD       TASNCL        eWTCLIA_e
315600100402     C                   ADD       w_peso        eWTKGIA_e
315700100402     C                   ADD       TASVLF        eWTMCIA_e
315800100402     c                   endif
315900100402     c* tipo servizio  "H"
316000100402     c                   if        tastsp='H'
316100100402     C                   ADD       1             hWTSPIA_e
316200100402     C                   ADD       TASNCL        hWTCLIA_e
316300100402     C                   ADD       w_peso        hWTKGIA_e
316400100402     C                   ADD       TASVLF        hWTMCIA_e
316500100402     c                   endif
316600100402     c
316700100402     c                   endif
316800100402     c                   endif
316900100402     c* riepilogo
317000100402     C                   ADD       1             WTSPIA_r
317100100402     C                   ADD       TASNCL        WTCLIA_r
317200100402     C                   ADD       w_peso        WTKGIA_r
317300100402     C                   ADD       TASVLF        WTMCIA_r
317400100402     c* tipo servizio  "E"
317500100402     c                   if        tastsp='E'
317600100402     C                   ADD       1             eWTSPIA_r
317700100402     C                   ADD       TASNCL        eWTCLIA_r
317800100402     C                   ADD       w_peso        eWTKGIA_r
317900100402     C                   ADD       TASVLF        eWTMCIA_r
318000100402     c                   endif
318100100402     c* tipo servizio  "H"
318200100402     c                   if        tastsp='H'
318300100402     C                   ADD       1             hWTSPIA_r
318400100402     C                   ADD       TASNCL        hWTCLIA_r
318500100402     C                   ADD       w_peso        hWTKGIA_r
318600100402     C                   ADD       TASVLF        hWTMCIA_r
318700100402     c                   endif
318800100402     c* extra budget
318900100402     c                   if        *in35
319000100402     C                   ADD       1             WTSPIA_re
319100100402     C                   ADD       TASNCL        WTCLIA_re
319200100402     C                   ADD       w_peso        WTKGIA_re
319300100402     C                   ADD       TASVLF        WTMCIA_re
319400100402     c* tipo servizio  "E"
319500100402     c                   if        tastsp='E'
319600100402     C                   ADD       1             eWTSPIA_re
319700100402     C                   ADD       TASNCL        eWTCLIA_re
319800100402     C                   ADD       w_peso        eWTKGIA_re
319900100402     C                   ADD       TASVLF        eWTMCIA_re
320000100402     c                   endif
320100100402     c* tipo servizio  "H"
320200100402     c                   if        tastsp='H'
320300100402     C                   ADD       1             hWTSPIA_re
320400100402     C                   ADD       TASNCL        hWTCLIA_re
320500100402     C                   ADD       w_peso        hWTKGIA_re
320600100402     C                   ADD       TASVLF        hWTMCIA_re
320700100402     c                   endif
320800100402     c                   endif
320900120904     c
321000120904     c                   eval      ks_tip='IA'
321100120904     c                   exsr      spe_peso
321200120904     c
321300100402     c                   ENDSR
321400100402     c****************************************************************************
321500100402     c     speEXPORT     BEGSR
321600100402     c* giornaliero
321700100402     c                   if        (tasaas*10000+tasmgs)=§al
321800100402     C                   ADD       1             WTSPEA
321900100402     C                   ADD       TASNCL        WTCLEA
322000100402     C                   ADD       w_peso        WTKGEA
322100100402     C                   ADD       TASVLF        WTMCEA
322200100402     c* tipo servizio  "E"
322300100402     c                   if        tastsp='E'
322400100402     C                   ADD       1             eWTSPEA
322500100402     C                   ADD       TASNCL        eWTCLEA
322600100402     C                   ADD       w_peso        eWTKGEA
322700100402     C                   ADD       TASVLF        eWTMCEA
322800100402     c                   endif
322900100402     c* tipo servizio  "H"
323000100402     c                   if        tastsp='H'
323100100402     C                   ADD       1             hWTSPEA
323200100402     C                   ADD       TASNCL        hWTCLEA
323300100402     C                   ADD       w_peso        hWTKGEA
323400100402     C                   ADD       TASVLF        hWTMCEA
323500100402     c                   endif
323600100402     c* extra budget
323700100402     c                   if        *in35
323800100402     C                   ADD       1             WTSPEA_e
323900100402     C                   ADD       TASNCL        WTCLEA_e
324000100402     C                   ADD       w_peso        WTKGEA_e
324100100402     C                   ADD       TASVLF        WTMCEA_e
324200100402     c* tipo servizio  "E"
324300100402     c                   if        tastsp='E'
324400100402     C                   ADD       1             eWTSPEA_e
324500100402     C                   ADD       TASNCL        eWTCLEA_e
324600100402     C                   ADD       w_peso        eWTKGEA_e
324700100402     C                   ADD       TASVLF        eWTMCEA_e
324800100402     c                   endif
324900100402     c* tipo servizio  "H"
325000100402     c                   if        tastsp='H'
325100100402     C                   ADD       1             hWTSPEA_e
325200100402     C                   ADD       TASNCL        hWTCLEA_e
325300100402     C                   ADD       w_peso        hWTKGEA_e
325400100402     C                   ADD       TASVLF        hWTMCEA_e
325500100402     c                   endif
325600100402     c
325700100402     c                   endif
325800100402     c                   endif
325900100402     c* riepilogo
326000100402     C                   ADD       1             WTSPEA_r
326100100402     C                   ADD       TASNCL        WTCLEA_r
326200100402     C                   ADD       w_peso        WTKGEA_r
326300100402     C                   ADD       TASVLF        WTMCEA_r
326400100402     c* tipo servizio  "E"
326500100402     c                   if        tastsp='E'
326600100402     C                   ADD       1             eWTSPEA_r
326700100402     C                   ADD       TASNCL        eWTCLEA_r
326800100402     C                   ADD       w_peso        eWTKGEA_r
326900100402     C                   ADD       TASVLF        eWTMCEA_r
327000100402     c                   endif
327100100402     c* tipo servizio  "H"
327200100402     c                   if        tastsp='H'
327300100402     C                   ADD       1             hWTSPEA_r
327400100402     C                   ADD       TASNCL        hWTCLEA_r
327500100402     C                   ADD       w_peso        hWTKGEA_r
327600100402     C                   ADD       TASVLF        hWTMCEA_r
327700100402     c                   endif
327800100402     c* extra budget
327900100402     c                   if        *in35
328000100402     C                   ADD       1             WTSPEA_re
328100100402     C                   ADD       TASNCL        WTCLEA_re
328200100402     C                   ADD       w_peso        WTKGEA_re
328300100402     C                   ADD       TASVLF        WTMCEA_re
328400100402     c* tipo servizio  "E"
328500100402     c                   if        tastsp='E'
328600100402     C                   ADD       1             eWTSPEA_re
328700100402     C                   ADD       TASNCL        eWTCLEA_re
328800100402     C                   ADD       w_peso        eWTKGEA_re
328900100402     C                   ADD       TASVLF        eWTMCEA_re
329000100402     c                   endif
329100100402     c* tipo servizio  "H"
329200100402     c                   if        tastsp='H'
329300100402     C                   ADD       1             hWTSPEA_re
329400100402     C                   ADD       TASNCL        hWTCLEA_re
329500100402     C                   ADD       w_peso        hWTKGEA_re
329600100402     C                   ADD       TASVLF        hWTMCEA_re
329700100402     c                   endif
329800100402     c
329900100402     c                   endif
330000120904     c
330100120904     c                   eval      ks_tip='EA'
330200120904     c                   exsr      spe_peso
330300120904     c
330400100402     c                   ENDSR
330500120903     c****************************************************************************
330600120903     c     SPE_peso      BEGSR
330700120903     c
330800120905     c* scaglioni di peso solo se richiesto
330900120905    1c                   if        §stat=' ' and §opz='4'
331000120903     c
331100120903     c* in base al peso trovo lo scaglione e faccio subito la write
331200120903     c                   z-add     1             II
331300120903     c                   clear                   ks_sca
331400120903     c                   dow       ks_sca  =0
331500120903     C     W_peso        IFGT      KIR(Ii)
331600120903     C     W_peso        ANDLE     KFR(Ii)
331700120904     C                   MOVEL     KFR(Ii)       ks_sca
331800120903     C                   endif
331900120903     c                   add       1             II
332000120903     c                   enddo
332100120903     c
332200120903     c* la write/update per peso la faccio subito
332300120903     c
332400120903    2c                   if        Wtipo='G' or Wtipo='R'
332500120903     c
332600120903    3c                   if        (tasaas*10000+tasmgs)=§al
332700120903     c                   eval      ks_tie='G'
332800120903     c                   eval      ks_lin=fil(k)
332900120905     c     kss61         chain     WFLSS61l
333000120903     c
333100120905     c                   if        not %found(WFLSS61l)
333200120905     c                   clear                   WFLSS600
333300120903     c                   eval      wsstie=ks_tie
333400120903     c                   eval      wsstip=ks_tip
333500120903     c                   eval      wsskgf=ks_sca
333600120904     c                   eval      wsslin=ks_lin
333700120903     c                   eval      wssdal=§al
333800120903     c                   eval      wssall=§al
333900120903     c                   endif
334000120903     c
334100120903     c                   exsr      Scrivi_pes
334200120903     c
334300120903    3c                   endif
334400120903     c
334500120903    2c                   endif
334600120903
334700120903    2c                   if        Wtipo='P' or Wtipo='R'
334800120903     c                   eval      ks_tie=wtipo
334900120903     c                   eval      ks_lin=fil(k)
335000120905     c     kss61         chain     WFLSS61l
335100120903     c
335200120905     c                   if        not %found(WFLSS61l)
335300120905     c                   clear                   WFLSS600
335400120903     c                   eval      wsstie=ks_tie
335500120903     c                   eval      wsstip=ks_tip
335600120903     c                   eval      wsskgf=ks_sca
335700120904     c                   eval      wsslin=ks_lin
335800120903     c                   eval      wssdal=§dal
335900120903     c                   eval      wssall=§al
336000120903     c                   endif
336100120903     c
336200120903     c                   exsr      Scrivi_pes
336300120903     c
336400120903    2c                   endif
336500120903     c
336600120903    1c                   endif
336700120903     c                   ENDSR
336800120903     c****************************************************************************
336900120903     c     Scrivi_pes    BEGSR
337000120903     c                   add       1             wsstsp
337100120903     C                   ADD       TASNCL        wsstcl
337200120903     C                   ADD       w_peso        wsstkg
337300120903     C                   ADD       TASVLF        wsstmc
337400120903     c                   if        tastsp='E'
337500120903     c                   add       1             wsstspe
337600120903     C                   ADD       TASNCL        wsstcle
337700120903     C                   ADD       w_peso        wsstkge
337800120903     C                   ADD       TASVLF        wsstmce
337900120903     c                   endif
338000120903     c                   if        tastsp='h'
338100120903     c                   add       1             wsstsph
338200120903     C                   ADD       TASNCL        wsstclh
338300120903     C                   ADD       w_peso        wsstkgh
338400120903     C                   ADD       TASVLF        wsstmch
338500120903     c                   endif
338600120903     c
338700120905     c                   if        not %found(WFLSS61l)
338800120905     c                   write     WFLSS600
338900120903     c                   else
339000120905     c                   update    WFLSS600
339100120903     c                   endif
339200120903     c
339300120903     c                   ENDSR
339400100402     c****************************************************************************
339500970519     OQSYSPRT   E            TESTA          2 02
339600090206     O                                         +  1 'Terminata elaborazione'
339700090206     o                                         +  2 'statistiche spedizioni'
339800090206     O                                         +  2 'DEL'
339900970519     O                       UDATE         Y   +  1
340000970528     O                       §DAL              +  5 '    /  /  '
340100970528     O                                         +  1 '>'
340200970528     O                       §AL               +  1 '    /  /  '
340300090206     O                                          120 'TNSSL3R1'
340400970519     O                       PAGE          Z    132
340500970519     O                                          128 'PAG.'
340600970529     O          E            TESTA          2
340700970529     O                                           14 'TIPI BOLLA OK:'
340800970529     O                       TBO               +  1
340900970529     O          E            ERR            1
341000970529     O                       TASLNP               3
341100970529     O                       TASLNA            +  1
341200970529     O                       TASAAS            +  1
341300970529     O                       TASMGS            +  1
341400970529     O                       TASNRS            +  1
341500970529     O                       TASNSP            +  1
341600970529     O                       TASTBL            +  1
341700110801     O          E            TESTA       1  1
341800110801     O                                            7 'Clienti'
341900110801     o                       Ptipocli         +   1
342000110801     O                                        +   1 ':'
342100110801     O          E            detksc         1
342200110801     o                       skicli             132
342300090915**
342400090915OVRPRTF FILE(QSYSPRT) SAVE(*YES) HOLD(*YES)
