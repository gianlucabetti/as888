000100110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200141106     h dftactgrp(*no) actgrp(*caller) bnddir('TRBM':'UBBNDDIR':'TRUL')
000300110617
000400110617      *---------------------------------------------------------------*
000500140207     fazorg01l  if   e           k disk
000600140211     ffnarb01l  if   e           k disk
000700140210     ffiar901l  if   e           k disk
000800140210     ftabel00f  if   e           k disk
000900140212     ftntbe01l  if   e           k disk
001000130309
001100140212
001200140212     D FNLV81R         PR
001300140207     D                                     extpgm('FNLV81R')
001400140212     D fnlv81ds                            likeds(fnlv81ds)
001500140213
001600140213
001700140414     D FIDNA6R         PR
001800140414     D                                     extpgm('FIDNA6R')
001900140414     D fidna6ds                            likeds(fidna6ds)
002000140212
002100140212
002200140311     D TRULORSR        PR
002300140311     D                                     extpgm('TRULORSR')
002400140311     D kpjba                               likeds(kpjba)
002500140311     D trulORsds                           likeds(trulORsds)
002600140311     D trulOR2ds                           likeds(trulOR2ds)
002700141009
002800141009
002900141009     D TRUL28R0        PR
003000141009     D                                     extpgm('TRUL28R0')
003100141009     D trul28ds0                           likeds(trul28ds0)
003200140212
003300140207
003400140207     D TRUL33R         PR
003500140207     D                                     extpgm('TRUL33R')
003600140207     D kpjba                               likeds(kpjba)
003700140211
003800140211
003900140211     D FNLRJ6R         PR
004000140211     D                                     extpgm('FNLRJ6R')
004100140211     D fnlrj6ds                            likeds(fnlrj6ds)
004200140212
004300140212
004400140212     D FNLV82R         PR
004500140212     D                                     extpgm('FNLV82R')
004600140212     D fnlv82ds                            likeds(fnlv82ds)
004700140213
004800140213
004900140213     D XGIOLAV         PR
005000140213     D                                     extpgm('XGIOLAV')
005100140213     D xgiolavds                           likeds(xgiolavds)
005200141010
005300141010
005400141010     D TRULVPOR        PR
005500141010     D                                     extpgm('TRULVPOR')
005600141010     D trulvpods                           likeds(TRULVPODS)
005700140212
005800140212
005900140212     D FNLSALESR       PR
006000140212     D                                     extpgm('FNLSALESR')
006100140212     D InChiudi                            like(ALESInChiudi)
006200140212     D InOpz                               like(ALESInOpz)
006300140212     D InKSU                               like(ALESInKSU)
006400140212     D InTIP                               like(ALESInTIP)
006500140212     D InTSC                               like(ALESInTSC)
006600140212     D InPGM                               like(ALESInPGM)
006700170323     D InPRG                               like(ALESInPRG)
006800140213     D InDS_SMS                            likeds(ALESInDS_SMS)
006900140212     D OutPRG                              like(ALESOutPRG)
007000140212     D OutEsito                            like(ALESOutEsito)
007100141106
007200141106
007300141106     D UBSPESTS        PR
007400141106     D                                     extpgm('UBSPESTS')
007500141106     D pInAnno                             like(pInAnno)
007600141106     D pInLineaPar                         like(pInLineaPar)
007700141106     D pInSerie                            like(pInSerie)
007800141106     D pInNumSped                          like(pInNumSped)
007900141106     D pInLingua                           like(pInLingua)
008000141106     D pInMaiuscolo                        like(pInMaiuscolo)
008100141106     D pOutLblTyp                          like(pOutLblTyp)
008200141106     D pOutRblBO                           like(pOutRblBO)
008300141106     D pOutStsSped                         like(pOutStsSped)
008400141106     D pOutDtConsRich                      like(pOutDtConsRich)
008500141106     D pOutDtApGiac                        like(pOutDtApGiac)
008600141106     D pOutDtLAvviso                       like(pOutDtLAvviso)
008700141106     D pOutDescSts                         like(pOutDescSts)
008800141106     D pOutDtSts                           like(pOutDtSts)
008900141106     D pOutErrore                          like(pOutErrore)
009000141106     D pOutCA                              like(pOutCA)
009100141106     D pOutDtPrGiac                        like(pOutDtPrGiac)
009200141106     D pOutDtChGiac                        like(pOutDtChGiac)
009300140212
009400140212
009500140212     D TIS701C1        PR
009600140212     D                                     extpgm('TIS701C1')
009700140212     D wEml                                like(wEml)
009800140212     D wCcEml                              like(wCcEml)
009900140212     D wOgg                                like(wOgg)
010000140212     D wMsg                                like(wMsg)
010100140212     D wSndSts                             like(wSndSts)
010200140212
010300140212
010400130309
010500140207     D kpjba         e ds                  inz
010600140212     D ds3A          e ds                  inz
010700140401     D og129         e ds                  inz
010800141016     D og130         e ds                  inz
010900150929     D og143         e ds                  inz
011000140212     D xgiolavds     e ds                  inz
011100140414     D fidna6ds      e ds                  inz
011200140212     D trul33ds      e ds                  inz
011300140212     D fnlv81ds      e ds                  inz
011400140212     D fnlv55ds      e ds                  inz
011500140212     D fnlrj6ds      e ds                  inz
011600140212     D fnlv82ds      e ds                  inz
011700141009     D trul28ds0     e ds                  qualified inz
011800140311     D trulORsds     e ds                  inz
011900140311     D trulOR2ds     e ds                  inz
012000140311     D tnsd99ds      e ds
012100140212     D fisia00f      e ds                  inz
012200140212     D §siaRRN         s             10I 0 inz
012300140212     D dsiaRIC       e ds                  inz
012400140214     D dsiaFLO       e ds                  inz
012500140214     D dsiaMSG       e ds                  inz
012600150930     D dsiaICA       e ds                  inz
012700170208     D dsiaGIA       e ds                  inz
012800150929     D diore01       e ds                  inz
012900150929     D dntw          e ds                  inz
013000170222     D UBRSMALDS     E DS                  QUALIFIED
013100140214     D wAAS            s                   like(siaAAS) inz
013200140212     D wDivid          s                   like(InDivid) inz
013300140212     D wSQL            s           2048A   varying inz
013400140212     D wLNA            s                   like(arbLNA) inz
013500140212     D wLNA_A          s              3A   inz
013600140207     D wTipoInvio      s              1A   inz
013700140212     D wCbc            s              2    dim(100) inz
013800140212     D wCbcIdx         s              3  0 inz
013900140212     D w§CM1VAR        s                   like(§CM1VAR)  inz
014000140212     D w§CM1PROG       s                   like(§CM1PROG) inz
014100141015     D wVPOReml        s              6A   inz
014200141015     D wVPORsms        s              6A   inz
014300140212     D string          s            200A   varying inz
014400140212     D strTRBM         s          65535A   varying inz
014500150313     D api             s              1A   inz('''')
014600140212     D wEml            s            253    inz
014700140212     D wCcEml          s            253    inz
014800140212     D wOgg            s             44    inz
014900140212     D wMsg            s           5000    inz
015000140212     D wSndSts         s              1    inz
015100140212     D pEsito          s             10I 0 inz
015200140612     D wTesto          s           1600    varying inz
015300170222     d wRSM_Eml        s             60A   inz
015400170222     d wRSM_Sms        s             60A   inz
015500141106
015600141106     d pInAnno         s              4s 0 inz
015700141106     d pInLineaPar     s              3s 0 inz
015800141106     d pInSerie        s              2s 0 inz
015900141106     d pInNumSped      s              7s 0 inz
016000141106     d pInLingua       s              2    inz
016100141106     d pInMaiuscolo    s              1    inz
016200141106     d pOutLblTyp      s              1    inz
016300141106     d pOutRblBO       s              1    inz
016400141106     d pOutStsSped     s              2    inz
016500141106     d pOutDtConsRich  s              8S 0 inz
016600141106     d pOutDtApGiac    s              8s 0 inz
016700141106     d pOutDtLAvviso   s              8s 0 inz
016800141106     d pOutDescSts     s            128    inz
016900141106     d pOutDtSts       s              8S 0 inz
017000141106     d pOutErrore      s              1    inz
017100141106     d pOutCA          s              1    inz
017200141106     d pOutDtPrGiac    s              8s 0 inz
017300141106     d pOutDtChGiac    s              8s 0 inz
017400141106
017500141106
017600141009     D BRTcode         ds                  qualified
017700141009     D   aas4                         4  0
017800141009     D   lnp                          3  0
017900141009     D   nrs                          2  0
018000141009     D   nsp                          7  0
018100141009     D   chkdgt1                      1  0
018200141009     D   code                         3  0
018300141009     D   chkdgt2                      1  0
018400141009     D   BRTcode               3     21
018500141010
018600141010
018700141010      // DS per filiali abilitate al deploy
018800141010     D TRULVPODS     e ds
018900141010     D  skFilOk               16    765    dim(250)
019000141014     D skFilOkEml      s                   like(skFilOk) dim(250) inz
019100141014     D skFilOkSms      s                   like(skFilOk) dim(250) inz
019200140212
019300140212
019400140212     D* DS di output
019500140213     D  ALESInDS_SMS   ds                  qualified inz
019600140212     D   i_MittSMS                   30
019700140212     D   i_IdSMS                     36
019800140212     D   i_cell                      20
019900140212     D   i_message                 1600
020000140212
020100140212
020200140212     D ALESInChiudi    s              1A   inz
020300140212     D ALESInOpz       s              1A   inz
020400140212     D ALESInKSU       s              8A   inz('0BART001')
020500140212     D ALESInTIP       s              2A   inz('SM')
020600140212     D ALESInTSC       s              2A   inz('WW')
020700170323     D ALESInPGM       s             10A   inz
020800170323     D ALESInPRG       s             10A   inz
020900140212     D ALESOutPRG      s             10A   inz
021000140212     D ALESOutEsito    s              1A   inz
021100170323     D sVGDPRG         s             10A   inz
021200140212
021300140212
021400140210
021500140210     D* Variabili di "mapping"
021600140213     D wRicTipo        s                   like(§SIACHI)  inz
021700140212     D wRicData        s             10A                  inz
021800140212     D wRicOra         s              5A                  inz
021900170208     D wGiaData        s             10A                  inz
022000140211     D wLnaDescr       s                   like(orgDES)   inz
022100140211     D wLnaIndir       s                   like(orgIND)   inz
022200140212     D wLnaCAP         s              9A                  inz
022300140210     D wLnaLocal       s                   like(orgLOC)   inz
022400140211     D wLnaProv        s                   like(orgPRO)   inz
022500140211     D wLnaTel         s                   like(orgTEL)   inz
022600140210     D wLnaEmail       s                   like(orgDD2)   inz
022700141016     D wLnaOraMMD_A    s              5A                  inz
022800141016     D wLnaOraMMA_A    s              5A                  inz
022900141016     D wLnaOraMPD_A    s              5A                  inz
023000141016     D wLnaOraMPA_A    s              5A                  inz
023100141016     D wLnaOraTMD_A    s              5A                  inz
023200141016     D wLnaOraTMA_A    s              5A                  inz
023300141016     D wLnaOraTPD_A    s              5A                  inz
023400141016     D wLnaOraTPA_A    s              5A                  inz
023500140211     D wMitIndir       s                   like(arbINM)   inz
023600140211     D wMitCAP         s                   like(arbCAM)   inz
023700140211     D wMitLocal       s                   like(arbLOM)   inz
023800140211     D wMitProv        s                   like(arbPRM)   inz
023900140210     D wSpedDataA      s             10                   inz
024000150601     D wDataLimitFD    s             20                   inz
024100141014     D wBRTcode        s             19                   inz
024200141014     D wBRTcodeLong    s             25                   inz
024300140210     D wSpedNum        s             12                   inz
024400140210     D wSpedRmn        s             15                   inz
024500140211     D wSpedRma        s                   like(arbRMA)   inz
024600140211     D wSpedRsd        s                   like(arbRSD)   inz
024700140211     D wSpedInd        s                   like(arbIND)   inz
024800170208     D wSpedCAP        s                   like(arbCAD)   inz
024900140211     D wSpedLocal      s                   like(arbLOD)   inz
025000140211     D wSpedProv       s                   like(arbPRD)   inz
025100140210     D wSpedColli      s              5A                  inz
025200140211     D wSpedKg         s              8A                  inz
025300140211     D wSpedCAS        s             20A                  inz
025400140212     D wSpedDataConsA  s             20A                  inz
025500151001     D wSpedDataConsD  s             20A                  inz
025600140211     D wSpedConsOrari  s             50A                  inz
025700150530     D wSpedConsOrar2  s             50A                  inz
025800140212     D wSpedConsOraDa  s              5A                  inz
025900140212     D wSpedConsOraA   s              5A                  inz
026000140212     D wDestEmail      s             71A                  inz
026100140212     D wDestMobile     s             16A                  inz
026200140324     D wDescRA         s            256A                  inz
026300140311     D wOggiSMS        s             50A                  inz
026400140317     D wDataOraNext    s             14S 0                inz
026500140317     D wDataOraCorr    s             14S 0                inz
026600150929     D skFilExt        s                   like(orgFIL) dim(500) inz
026700150929     D ixFilExt        s              3  0 inz
026800170208     D wLinkFil        s                   inz like(tbeUNI)
026900140210
027000140211
027100140212     D wEsito          s              1A   inz
027200140212     D wMessaggio      s            256A   varying inz
027300140212
027400140218     D wFlgNoOrari     s               N   inz('0')
027500140214     D wFlgTest        s               N   inz('0')
027600140211     D wFlgContrAss    s               N   inz('0')
027700140213     D wFlgSMSsent     s               N   inz('0')
027800140507     D wFlgFDAutMitt   s               N   inz('0')
027900140211
028000140212     D* Flag "globali"
028100140212     D wFlgTabelle     s               N   inz('0')
028200120619
028300140207
028400140207      // ? PROTOTIPI ?
028500140207      /copy gaitrasrc/srcprotopr,trbmbypsr
028600140207      /copy gaitrasrc/srcprotopr,fnlv55r
028700140311      /copy gaitrasrc/srcprotopr,tnsd99r
028800170222      /copy gaitrasrc/srcProtoPr,UBRSMALR
028900110617
029000140207
029100110617
029200110627     C     *ENTRY        PLIST
029300140207     C                   PARM                    InChiudi          1
029400140207     C                   PARM                    InDivis           2 0
029500140207     C                   PARM                    InDivid           2 0
029600140207     C                   PARM                    InTIPA            3
029700130309
029800110617      /free
029900130309
030000140207       // imposto settaggi base di sql
030100140207       exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
030200140207
030300140212       // gestione chiusura *pgm
030400140207       if InChiudi='S' or %shtdn;
030500140207          // eseguo chiusure a driver "aperti"
030600140211          exsr sr_Chiusura;
030700110627       else;
030800140210          // carico le tabelle necessarie (se non già caricate)
030900140212          if not wFlgTabelle;
031000140210             exsr sr_Cartab;
031100140210          endif;
031200141010
031300141010          // carico le filiali per rilasci pilotati
031400141010          exsr sr_VPOR;
031500140210
031600140207          // procedo con l'elaborazione
031700140207          exsr sr_Elabora;
031800140207
031900130309          eval *inrt = *on;
032000110627       endif;
032100130308
032200130308
032300110617       //-------------------------------------------------------------*
032400140214       //Lettura file "spia" alert FISIA00F                           *
032500110617       //-------------------------------------------------------------*
032600140207       Begsr sr_Elabora;
032700170323
032800170323         // Inizio la transazione di download
032900170329         //ALESInOpz = 'S';
033000170329         //callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
033100170329         //                    ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
033200170329         //                    ALESOutPRG:ALESOutEsito);
033300170329         //if %error OR ALESOutEsito <> *blanks;
033400170329         //   *in40 = *off;
033500170329         //   wMessaggio = 'KO da driver FNLSALESR';
033600170329         //else;
033700170329         //   sVGDPRG = ALESOutPRG;
033800170329         //endif;
033900170329
034000170329         sVGDPRG = 'XX00000000';
034100170329
034200140212
034300170329         // Verifico se richiesto filtro per particolare tipologia di alert
034400141009         select;
034500141009         when InChiudi = 'T';
034600140315               wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
034700140315                      ' where siaSTS = ''0'' ' +
034800140315                      ' and substr(siaFLO, 1, 1) = ''T'' ' +
034900150930                  //  ' and siaPTY < 050 ' +
035000140805                      ' order by siaPTY ' +
035100141020                  //    ' fetch first 200 rows only' +
035200140805                      ' for read only';
035300141009         when InChiudi = 'W';
035400141009               wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
035500141009                      ' where siaSTS = ''0'' ' +
035600141009                      ' and substr(siaFLO, 1, 1) = ''W'' ' +
035700150930                 //   ' and siaPTY < 050 ' +
035800141009                      ' order by siaPTY ' +
035900141020                 //   ' fetch first 200 rows only' +
036000141009                      ' for read only';
036100141009         when InChiudi = *blanks;
036200140315             if InTIPA <> *blanks;
036300140315                  wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
036400140315                         ' where siaSTS = ''0'' ' +
036500140315                         ' and siaTIPA=' + InTIPA +
036600150930                 //      ' and siaPTY < 050 ' +
036700140805                         ' order by siaPTY ' +
036800141020                 //      ' fetch first 200 rows only' +
036900140805                         ' for read only';
037000140315             else;
037100140315                  wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
037200140315                         ' where siaSTS = ''0'' ' +
037300150930                 //      ' and siaPTY < 050 ' +
037400170213                 //      ' and siaTIPA<>''AGD''' +
037500140805                         ' order by siaPTY ' +
037600141020                 //      ' fetch first 200 rows only' +
037700140805                         ' for read only';
037800140315             endif;
037900141009         endsl;
038000140207
038100140207         // preparo il cursore del recordset da leggere
038200140207         exec sql  prepare s1 from :wSQL;
038300140207         exec sql  declare c1 cursor for s1;
038400140207         exec sql  open    c1;
038500140207
038600140210         // ciclo su tutto il recordset estratto
038700140212         exec sql fetch next from c1 into :fisia00f, :§siaRRN;
038800140207         dow sqlcod=0;
038900140207
039000140207           // controllo costantemente se richiesta chiusura del sottosistema
039100140207           if %shtdn;
039200140212              InChiudi = 'S';
039300140211              exsr sr_Chiusura;
039400140207              leavesr;
039500140207           endif;
039600140207
039700140207           // Se richiesta elaborazione parallela gestisco
039800140207           exsr sr_ElaPar;
039900140207
040000140207           // se OK => procedo
040100140207           if *in40;
040200140212              // inizializzazioni dati necessari per gli alerts
040300140212              exsr sr_InzDati;
040400140212
040500140508              // verifiche iniziali (tra cui se richiesto test)
040600140508              exsr sr_exeInit;
040700140214
040800140207              // effettuo controlli pre invio
040900140214              if not wFlgTest;
041000140214                 exsr sr_ChkSend;
041100140214              endif;
041200140207           endif;
041300140207
041400140207           // se OK => procedo
041500140207           if *in40;
041600140210              // reperisco i dati necessari
041700140210              exsr sr_RtvDati;
041800141010              exsr sr_ChkSend2;
041900140207           endif;
042000140207
042100140207           // verifico il tipo alert corrente
042200140207           if *in40;
042300140207              select;
042400140207                when siaTIPA = 'RIC' and wTipoInvio = 'M';
042500140212                     exsr sr_RIC_Email;
042600140207                when siaTIPA = 'RIC' and wTipoInvio = 'S';
042700140212                     exsr sr_RIC_SMS;
042800150930                when siaTIPA = 'RIC' and wTipoInvio = 'B';
042900150930                     exsr sr_RIC_Email;
043000150930                     exsr sr_RIC_SMS;
043100140207                when siaTIPA = 'FD'  and wTipoInvio = 'M';
043200140213                     exsr sr_FD_Email;
043300140207                when siaTIPA = 'FD'  and wTipoInvio = 'S';
043400140213                     exsr sr_FD_SMS;
043500140214                when siaTIPA = 'FD'  and wTipoInvio = 'B';
043600140214                     exsr sr_FD_Email;
043700140214                     exsr sr_FD_SMS;
043800150930                when (siaTIPA = 'CL1' or siaTIPA = 'CL2' or siaTIPA = 'CL3')
043900150930                      and wTipoInvio = 'M';
044000150930                     exsr sr_CL_Email;
044100150930                when (siaTIPA = 'CL1' or siaTIPA = 'CL2' or siaTIPA = 'CL3')
044200150930                      and wTipoInvio = 'S';
044300150930                     exsr sr_CL_SMS;
044400150930                when (siaTIPA = 'CL1' or siaTIPA = 'CL2' or siaTIPA = 'CL3')
044500150930                      and wTipoInvio = 'B';
044600150930                     exsr sr_CL_Email;
044700151013                     // exsr sr_CL_SMS;
044800170208                when siaTIPA = 'AGD' and wTipoInvio = 'M';
044900170209                     exsr sr_AGD_Email;
045000170208                when siaTIPA = 'AGD' and wTipoInvio = 'S';
045100170209                     exsr sr_AGD_SMS;
045200170208                when siaTIPA = 'AGD' and wTipoInvio = 'B';
045300170209                     exsr sr_AGD_Email;
045400170209                     exsr sr_AGD_SMS;
045500140207                other;
045600140214                     *in40 = *off;
045700140212                     wMessaggio = 'Valore SIATIPA non previsto: '+siaTIPA;
045800140212                     exsr snd_Alert;
045900140207              endsl;
046000140207           endif;
046100140207
046200140211
046300140211           // Verifico se record elaborato OK e aggiorno esito
046400140211           if *in40;
046500140218              wEsito = '1';
046600140217              select;
046700140217                when wTipoInvio = 'S';
046800140217                     wMessaggio = 'Alert inviato, SMS a: ' + %trim(wDestMobile);
046900140217                when wTipoInvio = 'M';
047000140217                     wMessaggio = 'Alert inviato, email a: '+ %trim(wDestEmail);
047100140217                when wTipoInvio = 'B';
047200140217                     wMessaggio = 'Alert inviato, SMS/email a: '+
047300140217                                  %trim(wDestMobile) + '/' + %trim(wDestEmail);
047400140217              endsl;
047500140214              if not wFlgTest;
047600140322                 select;
047700140322                   when wTipoInvio = 'S';
047800140322                        wDescRA = %trim(wDestMobile);
047900140322                        exsr sr_InsRA;
048000140322                   when wTipoInvio = 'M';
048100140322                        wDescRA = %trim(wDestEmail);
048200140322                        exsr sr_InsRA;
048300140322                   when wTipoInvio = 'B';
048400140324                        wDescRA = %trim(wDestMobile)+' e: '+
048500140324                                  %trim(wDestEmail);
048600140322                        exsr sr_InsRA;
048700140322                 endsl;
048800140214              endif;
048900140211           else;
049000140218              if *in41;
049100140218                 wEsito = '0';
049200140218              else;
049300140218                 wEsito = 'E';
049400140218              endif;
049500140211           endif;
049600141013           exsr sr_UpdFISIA;
049700140211
049800140212           exec sql fetch next from c1 into :fisia00f, :§siaRRN;
049900140207         enddo;
050000140207
050100140207         exec sql  close   c1;
050200140212
050300170323
050400170323         // Sancisco la transazione di download
050500170323         ALESInOpz = 'E';
050600170323         ALESInPRG = sVGDPRG;
050700170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
050800170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
050900170323                             ALESOutPRG:ALESOutEsito);
051000170323         if %error OR ALESOutEsito <> *blanks;
051100170323            *in40 = *off;
051200170323            wMessaggio = 'KO da driver FNLSALESR';
051300170323         endif;
051400130308
051500140207       endsr;
051600140212
051700140212
051800140212       //-------------------------------------------------------------*
051900140212       //Aggiornamento record di FISIA corrente                       *
052000140212       //-------------------------------------------------------------*
052100140212       Begsr sr_UpdFISIA;
052200141013
052300141013         if §SIAFLOTIP = 'T' or §SIAFLOTIP = 'W';
052400141013            wMessaggio = dsiamsg + %trim(wMessaggio);
052500141013         endif;
052600140212
052700140212         exec sql  update fisia00f set
052800140212                          siaSTS   = :wEsito,
052900140212                          siaMSG   = :wMessaggio,
053000140212                          siaDAORE = current_timestamp
053100140212                   where  rrn(fisia00f) = :§siaRRN AND
053200140213                          siaSTS = '0';
053300140212
053400140212       endsr;
053500150929
053600150929
053700140211
053800140211       //-------------------------------------------------------------*
053900140211       //inizializzazioni                                             *
054000140211       //-------------------------------------------------------------*
054100140212       Begsr sr_InzDati;
054200140212
054300140214          KNMUS = 'QPGMR';
054400140214
054500140212          clear dsiaRIC;
054600140214          clear dsiaFLO;
054700140214          clear dsiaMSG;
054800150930          clear dsiaICA;
054900170208          clear dsiaGIA;
055000140212          clear ds3A;
055100140401          clear og129;
055200141016          clear og130;
055300140212          clear xgiolavds;
055400140212          clear fnlv81ds;
055500140212          clear fnlv55ds;
055600140212          clear fnlrj6ds;
055700140212          clear fnlv82ds;
055800141009          clear trul28ds0;
055900140311          clear trulORsds;
056000140311          clear trulOR2ds;
056100150730          clear diore01;
056200140612          clear wTesto;
056300140725          clear wRSM_Eml;
056400140725          clear wRSM_Sms;
056500140212
056600140212          wSpedDataConsA = 'domani';
056700151001          wSpedDataConsD = *blanks;
056800140213          wSpedConsOrari = 'alla stessa ora';
056900150530          wSpedConsOrar2 = 'alla stessa ora';
057000140212
057100140212          reset ALESInChiudi;
057200140212          reset ALESInOpz;
057300140212          reset ALESInKSU;
057400140212          reset ALESInTIP;
057500140212          reset ALESInTSC;
057600140212          reset ALESInPGM;
057700170323          reset ALESInPRG;
057800140212          reset ALESInDS_SMS;
057900140212          reset ALESOutPRG;
058000140212          reset ALESOutEsito;
058100140212
058200141015          clear wEml;
058300141015          clear wCcEml;
058400141015          clear wOgg;
058500141015          clear wMsg;
058600141015          clear wSndSts;
058700140212
058800141015          clear wTipoInvio;
058900141015          clear wEsito;
059000141015          clear wMessaggio;
059100140212
059200141015          clear wRicTipo;
059300141015          clear wRicData;
059400141015          clear wRicOra;
059500170208          clear wGiaData;
059600141015          clear wLnaDescr;
059700141015          clear wLnaIndir;
059800141015          clear wLnaCAP;
059900141015          clear wLnaLocal;
060000141015          clear wLnaProv;
060100141015          clear wLnaTel;
060200141015          clear wLnaEmail;
060300141016          clear wLnaOraMMD_A;
060400141016          clear wLnaOraMMA_A;
060500141016          clear wLnaOraMPD_A;
060600141016          clear wLnaOraMPA_A;
060700141016          clear wLnaOraTMD_A;
060800141016          clear wLnaOraTMA_A;
060900141016          clear wLnaOraTPD_A;
061000141016          clear wLnaOraTPA_A;
061100141015          clear wMitIndir;
061200141015          clear wMitCAP;
061300141015          clear wMitLocal;
061400141015          clear wMitProv;
061500141015          clear wSpedDataA;
061600150530          clear wDataLimitFD;
061700141015          clear wBRTcode;
061800141015          clear wBRTcodeLong;
061900141015          clear wSpedNum;
062000141015          clear wSpedRmn;
062100141015          clear wSpedRma;
062200141015          clear wSpedRsd;
062300141015          clear wSpedInd;
062400170208          clear wSpedCAP;
062500141015          clear wSpedLocal;
062600141015          clear wSpedProv;
062700141015          clear wSpedColli;
062800141015          clear wSpedKg;
062900141015          clear wSpedCAS;
063000141015          clear wSpedConsOraDa;
063100141015          clear wSpedConsOraA;
063200141015          clear wDestEmail;
063300141015          clear wDestMobile;
063400141015          clear wDescRA;
063500140211
063600141015          clear wFlgNoOrari;
063700141015          clear wFlgTest;
063800141015          clear wFlgContrAss;
063900141015          clear wFlgSMSsent;
064000141015          clear wFlgFDAutMitt;
064100140211
064200140211       endsr;
064300140214
064400140214
064500140214       //-------------------------------------------------------------*
064600140214       //Verifica operazioni per elaborazione record di TEST          *
064700140214       //-------------------------------------------------------------*
064800140508       Begsr sr_exeInit;
064900140214
065000140214          wAAS = siaAAS;
065100140214          dsiaFLO = siaFLO;
065200140508
065300140508
065400141009          if §SIAFLOTIP = 'T' or §SIAFLOTIP = 'W';
065500140214             wFlgTest = *on;
065600141013             dsiaMSG = siaMSG;
065700140214             wAAS = §SIAMSGAAS;
065800140214             wDestEmail  = %trim(§SIAMSGEML);
065900140214             wDestMobile = %trim(§SIAMSGSMS);
066000140214             select;
066100140214               when wDestEmail <> *blanks AND wDestMobile <> *blanks;
066200140214                    wTipoInvio = 'B';
066300140214               when wDestEmail <> *blanks;
066400140214                    wTipoInvio = 'M';
066500140214               when wDestMobile <> *blanks;
066600140214                    wTipoInvio = 'S';
066700140214               other;
066800140214                    *in40 = *off;
066900140214                    wMessaggio = 'Valore SIATIPA non previsto: '+siaTIPA;
067000140214                    exsr snd_Alert;
067100140214             endsl;
067200140214          endif;
067300140214
067400140214       endsr;
067500140210
067600140210
067700140210       //-------------------------------------------------------------*
067800140210       //gestione operazioni di chiusura                              *
067900140210       //-------------------------------------------------------------*
068000140212       Begsr sr_Chiusura;
068100140210
068200140211          eval *inlr = *on;
068300140210
068400140210       endsr;
068500130401
068600130401
068700130401       //-------------------------------------------------------------*
068800130401       //gestione elaborazione parallela                              *
068900130401       //-------------------------------------------------------------*
069000140207       Begsr sr_ElaPar;
069100130401
069200130401           *in40 = *off;                  // record KO
069300140207           if InDivis > *zeros AND InDivid > *zeros;
069400140207              wDivid = InDivid - 1;
069500140207              if %rem(siaNSP:InDivis) = wDivid;
069600130401                 *in40 = *on;             // record OK
069700130401              endif;
069800130401           else;
069900130401              *in40 = *on;                // record OK
070000130401           endif;
070100130401
070200130401       endsr;
070300140207
070400140207
070500140207       //-------------------------------------------------------------*
070600140207       //Effettuo controlli pre invio                                 *
070700140207       //-------------------------------------------------------------*
070800140207       Begsr sr_ChkSend;
070900140207
071000140212           *in40 = *on;                   // record OK
071100140218           *in41 = *off;
071200140207
071300140212           // 1° reperimento email/mobile
071400140214           iLV81AAS = wAAS;
071500140212           iLV81LNP = siaLNP;
071600140212           iLV81NRS = siaNRS;
071700140212           iLV81NSP = siaNSP;
071800140322           iLV81TRC = 'E';
071900140214           callP(e) FNLV81R (fnlv81ds);
072000140212           if not %error AND oLV81ERR <> 'N';
072100140207              // verifico se richiesto invio email o SMS
072200140207              select;
072300140322                 when oLV81EML <> *blanks AND oLV81TEL <> *blanks;
072400140322                      wTipoInvio = 'B';
072500140322                      wDestEmail = oLV81EML;
072600140322                      wDestMobile = oLV81TEL;
072700140322                 when oLV81EML <> *blanks AND oLV81TEL  = *blanks;
072800140212                      wTipoInvio = 'M';
072900140212                      wDestEmail = oLV81EML;
073000140322                 when oLV81TEL <> *blanks AND oLV81EML  = *blanks;
073100140207                      wTipoInvio = 'S';
073200140212                      wDestMobile = oLV81TEL;
073300140212                 other;
073400140212                      *in40 = *off;
073500140212                      wMessaggio = 'Assenti sia email che mobile';
073600140207              endsl;
073700140212           else;
073800140212              *in40 = *off;
073900140212              wMessaggio = 'KO da driver FNLV81R';
074000140207           endif;
074100140212
074200140207
074300140207           // 2° alert RIC mai inviato
074400140212           if *in40 AND siaTIPA = 'RIC';
074500141104              iLV82AAS  = wAAS;
074600141104              iLV82LNP  = siaLNP;
074700141104              iLV82NRS  = siaNRS;
074800141104              iLV82NSP  = siaNSP;
074900141104              iLV82TIPA = siaTIPA;
075000140212              callP(e) FNLV82R (fnlv82ds);
075100140212              if not %error AND oLV82STS <> '1';
075200140212                 *in40 = *on;
075300140212              else;
075400140212                 *in40 = *off;
075500140212                 wMessaggio = 'KO da driver FNLV82R';
075600140212              endif;
075700140212           endif;
075800140212
075900140207
076000140207           // 3° se FFD presenza fisica in arrivo tutti i colli
076100140212           if *in40 AND siaTIPA = 'FD';
076200140211              iLRJ6TRIC = 'A';
076300140214              iLRJ6AAS  = wAAS;
076400140211              iLRJ6LNP  = siaLNP;
076500140211              iLRJ6NRS  = siaNRS;
076600140211              iLRJ6NSP  = siaNSP;
076700140211              callP(e) FNLRJ6R (fnlrj6ds);
076800140218              if %error OR oLRJ6ERR <> *blanks;
076900140218                 *in40 = *off;
077000140218                 wMessaggio = 'KO da driver FNLRJ6R';
077100140218              else;
077200140218                 if oLRJ6FALL='S';
077300140218                    *in40 = *on;
077400140218                 else;
077500140218                    *in40 = *off;
077600140218                    *in41 = *on;
077700140218                 endif;
077800140218              endif;
077900140211           endif;
078000140207
078100140207       endsr;
078200141010
078300141010
078400141010       //-------------------------------------------------------------*
078500141010       //Effettuo controlli pre invio 2                               *
078600141010       //-------------------------------------------------------------*
078700141014       Begsr sr_ChkSend2;
078800141010
078900141021           // 4° spedizione già consegnata
079000141021           if *in40;
079100151012              if arbDCM > 0 AND arbCCA = *blanks;
079200141021                 *in40 = *off;
079300141021                 wMessaggio = 'Spedizione già consegnata';
079400141021              endif;
079500141021           endif;
079600141021
079700141021
079800170208           // 6° se filiale (LNA) corrente è da deplayare
079900141013           if *in40;
080000141013              select;
080100141015                when wTipoInvio='M' AND wVPOReml='VECCHIO';
080200141015                  *in40  = *off;
080300141015                  *in41  = *on;
080400141015
080500141015                when wTipoInvio='S' AND wVPORsms='VECCHIO';
080600141015                  *in40  = *off;
080700141015                  *in41  = *on;
080800141015
080900141015                when wTipoInvio='B' AND (wVPOReml<>'NUOVO' OR
081000141015                                         wVPORsms<>'NUOVO');
081100141015                  *in40  = *off;
081200141015                  *in41  = *on;
081300141015              endsl;
081400141015           endif;
081500141015
081600141015
081700141015           if *in40;
081800141015              select;
081900141015                when wTipoInvio='M' AND wVPOReml='MISTO';
082000141014                   if  %lookup(%editc(arbLNA:'X'):skFilOkEml) > 0;
082100141015                   // A modo "vecchio" asteriscare l' "else"
082200141013                   else;
082300141013                     *in40 = *off;
082400141013                     *in41 = *on;
082500141013                   endif;
082600141014
082700141015                when wTipoInvio='S' AND wVPORsms='MISTO';
082800141014                   if  %lookup(%editc(arbLNA:'X'):skFilOkSms) > 0;
082900141014                   // A modo "vecchio" asteriscare l' "else"
083000141014                   else;
083100141014                     *in40 = *off;
083200141014                     *in41 = *on;
083300141014                   endif;
083400141013              endsl;
083500141010           endif;
083600141107
083700141107
083800150929           // 7° spedizione in giacenza o con danno
083900141107           if *in40;
084000141107             pInAnno      = arbAAS;
084100141107             pInLineaPar  = arbLNP;
084200141107             pInSerie     = arbNRS;
084300141107             pInNumSped   = arbNSP;
084400141107             pInLingua    = 'it';
084500141107             pInMaiuscolo = '1';
084600141107             callP(e) UBSPESTS (
084700141107                       pInAnno        :
084800141107                       pInLineaPar    :
084900141107                       pInSerie       :
085000141107                       pInNumSped     :
085100141107                       pInLingua      :
085200141107                       pInMaiuscolo   :
085300141107                       pOutLblTyp     :
085400141107                       pOutRblBO      :
085500141107                       pOutStsSped    :
085600141107                       pOutDtConsRich :
085700141107                       pOutDtApGiac   :
085800141107                       pOutDtLAvviso  :
085900141107                       pOutDescSts    :
086000141107                       pOutDtSts      :
086100141107                       pOutErrore     :
086200141107                       pOutCA         :
086300141107                       pOutDtPrGiac   :
086400141107                       pOutDtChGiac   );
086500141107              if pOutErrore = *zeros AND pOutDescSts = 'IN GIACENZA';
086600141107                 *in40 = *off;
086700141107                 wMessaggio = 'Spedizione in giacenza';
086800141107              endif;
086900141107              if pOutErrore = *zeros AND pOutCA = 'S';
087000141107                 *in40 = *off;
087100141107                 wMessaggio = 'Spedizione con pratica danno';
087200141107              endif;
087300141107           endif;
087400141010
087500150929
087600150929           // 8° spedizione "export"
087700150929           if *in40;
087800150929              if  %lookup(arbLNA:skFilExt) > 0;
087900150929                 *in40 = *off;
088000150929                 wMessaggio = 'Spedizione EXPORT: alert bloccato';
088100150929              endif;
088200150929           endif;
088300170209
088400170209
088500170209           // 9° alert LAVV ma spedizione "già" FFD
088600170209           if *in40;
088700170209              if siaTIPA = 'RIC' AND arbFFD = 'S';
088800170209                 *in40 = *off;
088900170209                 wMessaggio = 'Spedizione già in fermo deposito';
089000170209              endif;
089100170209           endif;
089200150929
089300141010       endsr;
089400140207
089500170208
089600140207
089700140207       //-------------------------------------------------------------*
089800140207       //Reperimento dati                                             *
089900140207       //-------------------------------------------------------------*
090000140207       Begsr sr_RtvDati;
090100140210
090200140211        // Reperisco i dati bolla in arrivo
090300140214        chain (wAAS:siaLNP:siaNRS:siaNSP) fnarb01l;
090400140210
090500140211        if %found(fnarb01l);
090600140725
090700140725           // --- EMAIL ---
090800170222           // Verifico se per cliente corrente presente forzatura
090900170222           // per Rag.Soc. invio alerts
091000170222           UBRSMALDS.iTpRic   = 'E';
091100170222           UBRSMALDS.iOrigine = 'A';
091200170222           UBRSMALDS.iAAS     = arbAAS;
091300170222           UBRSMALDS.iLNP     = arbLNP;
091400170222           UBRSMALDS.iNRS     = arbNRS;
091500170222           UBRSMALDS.iNSP     = arbNSP;
091600170222           UBRSMALR_S ( UBRSMALDS );
091700170222           if UBRSMALDS.oEsito = *zeros;
091800170222              wRSM_Eml = %trim(UBRSMALDS.oRagSoc);
091900170222           else;
092000170222              wRSM_Eml = %trim(arbRSM);
092100170222           endif;
092200140725
092300140725           // --- SMS ---
092400170222           // Verifico se per cliente corrente presente forzatura
092500170222           // per Rag.Soc. invio alerts
092600170222           UBRSMALDS.iTpRic   = 'S';
092700170222           UBRSMALDS.iOrigine = 'A';
092800170222           UBRSMALDS.iAAS     = arbAAS;
092900170222           UBRSMALDS.iLNP     = arbLNP;
093000170222           UBRSMALDS.iNRS     = arbNRS;
093100170222           UBRSMALDS.iNSP     = arbNSP;
093200170222           UBRSMALR_S ( UBRSMALDS );
093300170222           if UBRSMALDS.oEsito = *zeros;
093400170222              wRSM_Sms = %trim(UBRSMALDS.oRagSoc);
093500170222           else;
093600170222              wRSM_Sms = %trim(arbRSM);
093700170222           endif;
093800140210
093900170208           // Reperisco vari dati necessari per gli alerts
094000140210           select;
094100140210           when siaTIPA = 'RIC';
094200140212              dsiaRIC  = siaNOTE;
094300140213              wRicTipo = §SIACHI;
094400140218              if wRicTipo = 'S';
094500140218                 wFlgNoOrari = *on;
094600140218              endif;
094700140514              if §SIAFD = 'S';
094800140514                 wFlgFDAutMitt = *on;
094900140514              endif;
095000140213              wRicData = %trim(%editw(%dec(%date(§SIADEV):*eur):'  /  /    '));
095100140212              wRicOra  = %trim(%editw(§SIAHEV:'  :  '));
095200140210           endsl;
095300140210
095400140210           // Se la linea di arrivo è in £6 cerco la sua capofila
095500140210           clear wLNA;
095600140211           d55lin = arbLNA;
095700140210           d55tpt = '6';
095800140211           d55drf = arbAAS*10000+arbMGS;
095900140210           fnlv55r(fnlv55ds);
096000140210           if d55tfa <> *zeros;
096100140212              wLNA = d55tfa;
096200140210           else;
096300140211              wLNA = arbLNA;
096400140210           endif;
096500170208           wLNA_A = %editc(wLNA:'X');
096600140210
096700140210           // chaino file accessori che mi servono per il passaggio info
096800140210           // chaino contrassegno se presente
096900140212           ar9CAS = *zeros;
097000140211           if %lookup(arbCBO:wCbc) > 0;
097100140214              chain (wAAS:siaLNP:siaNRS:siaNSP) fiar901l;
097200140212              if %found(fiar901l);
097300140212                 wFlgContrAss = *on;
097400140212              endif;
097500140210           endif;
097600140210
097700140210           // chaino azorg della linea di arrivo
097800140210           chain (wLNA) azorg01l;
097900140210
098000140212           if %found(azorg01l) AND orgFVA = *blanks;
098100140311
098200140311              // reperisco gli orari di consegna previsti
098300140311              clear tnsd99ds;
098400140311              d98tbo='P';
098500140311              d98aas=arbaas;
098600140311              d98lnp=arblnp;
098700140311              d98nrs=arbnrs;
098800140311              d98nsp=arbnsp;
098900140526              //if arbtsp='D';
099000140526              //   I98TSP_FOR='C';
099100140526              //endif;
099200140311              tnsd99r(tnsd99ds);
099300140526              if d98dci>0;
099400140526                 IOREDTA = d98dci;
099500140311              endif;
099600150530
099700150601              if d98DEE > 0 AND (d98spcDEE = 'FFD' OR
099800150601                                 d98spcDEE = 'FD1' OR
099900150530                                 d98spcDEE = 'FD2');
100000150601                 wDataLimitFD = 'entro il ' +
100100150601                           %subst(%editc(d98DEE:'X'):7:2) + '/' +
100200150601                           %subst(%editc(d98DEE:'X'):5:2);
100300150601              else;
100400150604                 wDataLimitFD = *blanks;
100500150601              endif;
100600150601
100700140311
100800140311              IOREFIL  = arbLNA;
100900140311              IORECAP  = arbCAD;
101000140311              IORELOC  = arbLOD;
101100140311              IORENAR  = arbNZD;
101200140311              IORETSER = 'C';
101300140311              IOREDDC  = arbDDC;
101400140311              IORENDC  = arbNDC;
101500140311              IORETFP  = arbTFP;
101600140311              IORETFA  = arbTFA;
101700140311              IOREDTI  = arbDTI;
101800140526              //IOREHTI  = arbHTI;
101900140526              IOREHTI  = *zeros;
102000140311              IOREDCR  = arbDCR;
102100140311              IOREHCR  = arbHCR;
102200140311              IORETCR  = arbTCR;
102300140311              IOREDEI  = d98DEI;
102400140311              IOREOEI  = d98OEI;
102500140311              IORETSP  = arbTSP;
102600150730              §IORETRAZC = %editc(D98TTC:'X');
102700150730              §IORECONSC = %editc(D98TCC:'X');
102800150730              IOREFLO  = diore01;
102900140311
103000140311              callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
103100140311              if not %error AND OOREERR = *blanks;
103200140311                wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
103300140311                wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
103400150530                wSpedConsOrari = '('+ wSpedConsOraDa + '  ' +
103500150530                                      wSpedConsOraA + ')';
103600150530                wSpedConsOrar2 = 'dalle ' + wSpedConsOraDa +
103700150530                                 ' alle ' + wSpedConsOraA;
103800140311              endif;
103900140319
104000140319              // Reperisco la data consegna prevista corrente
104100140319              ixgldata = %dec(%date():*iso);
104200140319              ixglfil  = wLNA;
104300140319              ixglpa   = 'A';
104400140319              ixgladd  = 'S';
104500140319              ixglgg   = 1;
104600140319              ixgllav  = 'S';
104700140319              callP(e) XGIOLAV (xgiolavds);
104800140319              if not %error AND
104900140319                 oxglerr = *blanks and oxgldata > *zeros;
105000140319                  if oxgldata <> %dec(%date()+%days(1):*iso);
105100150530                     wSpedDataConsA ='il '+%subst(%editc(oxgldata:'X'):7:2)+'/'+
105200150530                                           %subst(%editc(oxgldata:'X'):5:2)+'/'+
105300150530                                           %subst(%editc(oxgldata:'X'):1:4);
105400140319                 endif;
105500151001                 wSpedDataConsD ='il '+%subst(%editc(oxgldata:'X'):7:2)+'/'+
105600151001                                       %subst(%editc(oxgldata:'X'):5:2)+'/'+
105700151001                                       %subst(%editc(oxgldata:'X'):1:4);
105800140319              endif;
105900140312
106000140312              // verifico se data-ora prossima consegna indicativa è
106100140317              // inferiore alla data-ora corrente
106200140319              wDataOraNext = (oxgldata*10000+OOR2STFS)*100;
106300140319              wDataOraCorr = %dec(%date():*iso)*1000000+%dec(%time():*iso);
106400140319              if wDataOraNext <= wDataOraCorr;
106500140319                 *in40 = *off;
106600140319                 wMessaggio = 'Data/ora prossima consegna indicativa ' +
106700140417                              'inferiore a data/ora corrente';
106800140319              else;
106900140212
107000140212              wLnaDescr   = orgDES;
107100140210              wLnaIndir   = orgIND;
107200140212              wLnaCAP     = %editc(orgCPF:'X');
107300140210              wLnaLocal   = orgLOC;
107400140210              wLnaProv    = orgPRO;
107500140210              wLnaTel     = orgTEL;
107600140210              wLnaEmail   = orgDD2;
107700141016
107800140401              og129       = orgDC9;
107900140401              if §OGORFMD <> *zeros AND §OGORFMD <> *blanks;
108000141016                 wLnaOraMMD_A = %subst(§OGORFMD:1:2)+':'+%subst(§OGORFMD:3:2);
108100140213              endif;
108200140401              if §OGORFMA <> *zeros AND §OGORFMA <> *blanks;
108300141016                 wLnaOraMMA_A = %subst(§OGORFMA:1:2)+':'+%subst(§OGORFMA:3:2);
108400140213              endif;
108500140401              if §OGORFPD <> *zeros AND §OGORFPD <> *blanks;
108600141016                 wLnaOraMPD_A = %subst(§OGORFPD:1:2)+':'+%subst(§OGORFPD:3:2);
108700140213              endif;
108800140401              if §OGORFPA <> *zeros AND §OGORFPA <> *blanks;
108900141016                 wLnaOraMPA_A = %subst(§OGORFPA:1:2)+':'+%subst(§OGORFPA:3:2);
109000140213              endif;
109100141016
109200141016              og130       = orgDD0;
109300141016              if §OGORAMD <> *zeros AND §OGORAMD <> *blanks;
109400141016                 wLnaOraTMD_A = %subst(§OGORAMD:1:2)+':'+%subst(§OGORAMD:3:2);
109500141016              endif;
109600141016              if §OGORAMA <> *zeros AND §OGORAMA <> *blanks;
109700141016                 wLnaOraTMA_A = %subst(§OGORAMA:1:2)+':'+%subst(§OGORAMA:3:2);
109800141016              endif;
109900141016              if §OGORAPD <> *zeros AND §OGORAPD <> *blanks;
110000141016                 wLnaOraTPD_A = %subst(§OGORAPD:1:2)+':'+%subst(§OGORAPD:3:2);
110100141016              endif;
110200141016              if §OGORAPA <> *zeros AND §OGORAPA <> *blanks;
110300141016                 wLnaOraTPA_A = %subst(§OGORAPA:1:2)+':'+%subst(§OGORAPA:3:2);
110400141016              endif;
110500140210
110600140212              wMitIndir  = arbINM;
110700140212              wMitCAP    = arbCAM;
110800140211              wMitLocal  = arbLOM;
110900140211              wMitProv   = arbPRM;
111000140213              wSpedDataA = %trim(%editw(%dec(%date(arbAAS*10000+arbMGS):*eur)
111100140211                           :'  /  /    '));
111200141009
111300141009              // Calcolo il BRTcode (tramite apposito driver)
111400141009              clear BRTcode;
111500141009              BRTcode.aas4    = arbAAS;
111600141009              BRTcode.lnp     = arbLNP;
111700141009              BRTcode.nrs     = arbNRS;
111800141009              BRTcode.nsp     = arbNSP;
111900141009
112000141009              trul28ds0.i280COD = BRTcode.BRTcode;
112100141009              callP(e) TRUL28R0 (trul28ds0);
112200141009
112300141009              // Se BRTcode errato => esito KO
112400141009              if %error OR trul28ds0.o280err = '1';
112500141009              else;
112600141014                 wBRTcode     = %subst(trul28ds0.o280cod:1:5) +
112700141014                                %subst(trul28ds0.o280cod:6:5) +
112800141014                                %subst(trul28ds0.o280cod:11:5)+
112900141014                                %subst(trul28ds0.o280cod:16:4);
113000141014                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +'  '+
113100141014                                %subst(trul28ds0.o280cod:6:5) +'  '+
113200141014                                %subst(trul28ds0.o280cod:11:5)+'  '+
113300141014                                %subst(trul28ds0.o280cod:16:4);
113400141009              endif;
113500141009
113600141009
113700141009              wSpedNum   = %trim(%editc(arbLNP:'X')) +
113800141009                           %trim(%editc(arbNRS:'X')) +
113900141009                           %trim(%editc(arbNSP:'X'));
114000140211              wSpedRmn   = %trim(%editc(arbRMN:'Z'));
114100140211              wSpedRma   = %trim(arbRMA);
114200140211              wSpedRsd   = %trim(arbRSD);
114300140211              wSpedInd   = %trim(arbIND);
114400170208              wSpedCAP   = %trim(arbCAD);
114500140211              wSpedLocal = %trim(arbLOD);
114600140211              wSpedProv  = %trim(arbPRD);
114700140211              wSpedColli = %trim(%editc(arbNCL:'Z'));
114800140211              wSpedKg    = %trim(%editc(arbPKB:'2'));
114900141009              wSpedCAS   = %trim(%editc(ar9CAS:'1'))+'  '+ar9VCA;
115000140210
115100140210              // Stacco subito un progressivo per la email corrente
115200140212              exsr sr_rtvPrg;
115300140312
115400140312              endif;
115500140210
115600140212           else;
115700140212              *in40 = *off;
115800140212              wMessaggio = 'AZORG per ' + %editc(wLNA:'X') + ' non trovato';
115900140212           endif;
116000140923
116100140923
116200140923           // Per Fermi Deposito se spedizione già in distinta ...
116300140923           // ... aspetto a mandare l'alert
116400140923           if siaTIPA = 'FD' and arbDCM=*zeros and
116500140923                                 arbDDC>*zeros and
116600140923                                 arbNDC>*zeros;
116700140923              *in40 = *off;
116800140923              *in41 = *on;
116900140923           endif;
117000140210
117100140212        else;
117200140212           *in40 = *off;
117300140212           wMessaggio = 'FNARB non trovato';
117400140212        endif;
117500140210
117600140207       endsr;
117700130321
117800140210
117900140210       //-------------------------------------------------------------*
118000140210       //caricamento codici bolla con c/a                              *
118100140210       //-------------------------------------------------------------*
118200140210       Begsr sr_Cartab;
118300140210
118400140212          wFlgTabelle = *on;
118500140212
118600140210          // Leggo tabella "3A" per caricare schiera dei codici bolla
118700140210          // con contrassegno
118800140210          wCbcIdx=0;
118900140210          clear wCbc;
119000140210          setll (1:'3A') tabel00f;
119100140210          reade (1:'3A') tabel00f;
119200140210
119300140210          dow not %eof(tabel00f);
119400140210             ds3a=tbluni;
119500140210             if §3afca>0;
119600140212                wCbcIdx=wCbcIdx+1;
119700140210                wCbc(wCbcIdx)=tblkey;
119800140210             endif;
119900140210             reade (1:'3A') tabel00f;
120000140210          enddo;
120100140210
120200150929
120300150929          // Carico la lista di filiali "estere" (secondo il network)
120400150929          clear skFilExt;
120500150929          clear ixFilExt;
120600150929          setll (*zeros) azorg01l;
120700150929          read azorg01l;
120800150929
120900150929          dow not %eof(azorg01l);
121000150929             if ORGFVA = *blanks;
121100150929                og143 = orgDE3;
121200150929                chain ('NTW':§OGNTW) tntbe01l;
121300150929                if %found(tntbe01l);
121400150929                   dntw = tbeuni;
121500150929                   if §NTWFIE = 'E';
121600150929                      ixFilExt = ixFilExt + 1;
121700150929                      skFilExt(ixFilExt) = orgFIL;
121800150929                   endif;
121900150929                endif;
122000150929             endif;
122100150929             read azorg01l;
122200150929          enddo;
122300150929
122400150929       endsr;
122500141010
122600170208
122700141010
122800141010       //-------------------------------------------------------------*
122900141010       //caricamento filiali abilitate al deploy                      *
123000141010       //-------------------------------------------------------------*
123100141010       Begsr sr_VPOR;
123200141010
123300141015          clear wVPOReml;
123400141015          clear wVPORsms;
123500141010
123600141014          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
123700141010          clear TRULVPODS;
123800170208 xxx      IVPOke1 = 'DECOFINEWEM';
123900141010          TRULVPOR (trulvpods);
124000141014          skFilOkEml = skFilOk;
124100141010
124200141015          select;
124300141015            // se al primo elemento c'è 999 => tutto a modo nuovo
124400141015            when skFilOkEml(1) = '999';
124500141015                 wVPOReml = 'NUOVO';
124600141015
124700141015            // se al primo elemento c'è *blanks => tutto a modo vecchio
124800141015            when skFilOkEml(1) = *blanks;
124900141015                 wVPOReml = 'VECCHIO';
125000141015
125100141015            other;
125200141015                 wVPOReml = 'MISTO';
125300141015          endsl;
125400141014
125500141014          // richiamo il Pgm per reperire filiali abilitate al nuovo alert sms
125600141014          clear TRULVPODS;
125700141014          IVPOke1 = 'DECOFINEWSM';
125800141014          TRULVPOR (trulvpods);
125900141014          skFilOkSms = skFilOk;
126000141015
126100141015          select;
126200141015            // se al primo elemento c'è 999 => tutto a modo nuovo
126300141015            when skFilOkSms(1) = '999';
126400141015                 wVPORsms = 'NUOVO';
126500141015
126600141015            // se al primo elemento c'è *blanks => tutto a modo vecchio
126700141015            when skFilOkSms(1) = *blanks;
126800141015                 wVPORsms = 'VECCHIO';
126900141015
127000141015            other;
127100141015                 wVPORsms = 'MISTO';
127200141015          endsl;
127300141014
127400141010       endsr;
127500140212
127600140212
127700140212       //-------------------------------------------------------------*
127800140212       //Gestione invio tipo alert RIC via EMAIL                      *
127900140212       //-------------------------------------------------------------*
128000140212       Begsr sr_RIC_Email;
128100140212
128200140212         clear indmrabyps;
128300140212
128400141009         w§CM1VAR = '*OBJM*BRT - Avviso di tentata consegna ' +
128500140213                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
128600170208         chain ('MRA':'TRBMBYPSR':'ALE_RIC') tntbe01l;
128700140212
128800140212          if %found(tntbe01l);
128900140212           indmrabyps=tbeuni;
129000140212           exsr sr_trtcm1ds;
129100140212
129200140212           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
129300140725           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
129400140311           strTRBM=strTRBM+'§§Tag03§§=='+%trim(wMitIndir) +'||';
129500140212           strTRBM=strTRBM+'§§Tag04§§=='+%trim(wMitCAP)   +'||';
129600140212           strTRBM=strTRBM+'§§Tag05§§=='+%trim(wMitLocal) +'||';
129700140212           strTRBM=strTRBM+'§§Tag06§§=='+%trim(wMitProv)  +'||';
129800141009           strTRBM=strTRBM+'§§Tag08§§=='+
129900161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
130000141009           strTRBM=strTRBM+wBRTcode;
130100140212           strTRBM=strTRBM+'&urltype=a'+'||';
130200140212           strTRBM=strTRBM+'§§Tag10§§=='+%trim(wSpedNum)  +'||';
130300140212           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
130400140212           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
130500140212           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
130600140212           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
130700140212           if wFlgContrAss;
130800140212              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
130900140212           else;
131000141009              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
131100140212           endif;
131200140212           strTRBM=strTRBM+'§§Tag16§§=='+%trim(wLnaDescr)+'||';
131300140212           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wLnaIndir)+'||';
131400140212           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wLnaCAP)  +'||';
131500140212           strTRBM=strTRBM+'§§Tag19§§=='+%trim(wLnaLocal)+'||';
131600140213           strTRBM=strTRBM+'§§Tag20§§=='+'('+%trim(wLnaProv)+')'+'||';
131700140212           strTRBM=strTRBM+'§§Tag21§§=='+%trim(wLnaTel)  +'||';
131800140212           strTRBM=strTRBM+'§§Tag22§§=='+%trim(wLnaEmail)+'@brt.it'+'||';
131900140212           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
132000140212           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedLocal) +'  '
132100140212                                        +%trim(wSpedProv)+'||';
132200140212           strTRBM=strTRBM+'§§Tag30§§=='+%trim(wRicData)+'||';
132300140218           if wFlgNoOrari;
132400140218              strTRBM=strTRBM+'§§Tag31§§=='+''+'||';
132500140218           else;
132600141010              strTRBM=strTRBM+'§§Tag31§§=='+'alle '+%trim(wRicOra) +'||';
132700140218           endif;
132800140212           strTRBM=strTRBM+'§§Tag32§§=='+%trim(wSpedDataConsA)+'||';
132900150530           strTRBM=strTRBM+'§§Tag33§§=='+%trim(wSpedConsOrar2)+'||';
133000140213           strTRBM=strTRBM+'§§Tag34§§=='+%trim(wSpedDataA) +'||';
133100140212
133200141016           strTRBM=strTRBM+'§§Tag36§§=='+%trim(wLnaOraTMD_A)+'||';
133300141016           strTRBM=strTRBM+'§§Tag37§§=='+%trim(wLnaOraTMA_A) +'||';
133400141016           if wLnaOraTPD_A <> *blanks;
133500141016              strTRBM=strTRBM+'§§Tag38§§=='+' e  ' + %trim(wLnaOraTPD_A)+
133600141016                                            ' - ' + %trim(wLnaOraTPA_A)+'||';
133700141009           else;
133800141009              strTRBM=strTRBM+'§§Tag38§§=='+''+'||';
133900141009           endif;
134000141009
134100140507
134200140509           if not wFlgFDAutMitt;
134300140508              strTRBM=strTRBM+'§§Tag39§§=='+''+'||';
134400140507           else;
134500140509              strTRBM=strTRBM+'§§Tag39§§=='+
134600140509                 '  -  Per disposizione del mittente'+
134700140507                 ' lo svincolo della merce presso i nostri magazzini sarà' +
134800140507                 ' possibile al ricevimento di specifica autorizzazione' +
134900140507                 '||';
135000140507           endif;
135100140507
135200141014           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
135300140212
135400140212           if Bartmail_insert(strTRBM:indmrabyps:
135500140212                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
135600140612                wTesto =
135700140612                   'Un nuovo tentativo di consegna verrà effettuato ' +
135800140612                   %trim(wSpedDataConsA) + ' indicativamente ' +
135900150530                   %trim(wSpedConsOrar2);
136000140612
136100140212           else;
136200140212              *in40 = *off;
136300140212              wMessaggio = 'Errore in push-entry in BartMail';
136400140212           endif;
136500140212
136600140212          else;
136700140212            *in40 = *off;
136800170208            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_RIC non trovata';
136900140212          endif;
137000140212
137100140212       endsr;
137200140213
137300140213
137400140213       //-------------------------------------------------------------*
137500140213       //Gestione invio tipo alert FD  via EMAIL                      *
137600140213       //-------------------------------------------------------------*
137700141016       Begsr sr_FD_Email;
137800140213
137900140213         clear indmrabyps;
138000140213
138100140311         w§CM1VAR = '*OBJM*BRT - Avviso disponibilità merce a magazzino ' +
138200140213                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
138300170208         chain ('MRA':'TRBMBYPSR':'ALE_FD') tntbe01l;
138400140213
138500140213          if %found(tntbe01l);
138600140213           indmrabyps=tbeuni;
138700140213           exsr sr_trtcm1ds;
138800140213
138900140213           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
139000140725           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
139100140213           strTRBM=strTRBM+'§§Tag03§§=='+%trim(wMitIndir) +'||';
139200140213           strTRBM=strTRBM+'§§Tag04§§=='+%trim(wMitCAP)   +'||';
139300140213           strTRBM=strTRBM+'§§Tag05§§=='+%trim(wMitLocal) +'||';
139400140213           strTRBM=strTRBM+'§§Tag06§§=='+%trim(wMitProv)  +'||';
139500141016           strTRBM=strTRBM+'§§Tag08§§=='+
139600161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
139700141016           strTRBM=strTRBM+wBRTcode;
139800141016           strTRBM=strTRBM+'&urltype=a'+'||';
139900140213           strTRBM=strTRBM+'§§Tag10§§=='+%trim(wSpedNum)  +'||';
140000140213           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
140100140213           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
140200140213           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
140300140213           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
140400140213           if wFlgContrAss;
140500140213              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
140600140213              strTRBM=strTRBM+'§§Tag40§§=='+
140700140213                      'Le ricordiamo che all''atto del ritiro è '  +
140800141117                      'previsto il pagamento del <b> contrassegno di ' +
140900141117                      %trim(wSpedCAS)+'</b>.'+'||';
141000140213           else;
141100141009              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
141200140213              strTRBM=strTRBM+'§§Tag40§§=='+''+'||';
141300140213           endif;
141400140213           strTRBM=strTRBM+'§§Tag16§§=='+%trim(wLnaDescr)+'||';
141500140213           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wLnaIndir)+'||';
141600140213           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wLnaCAP)  +'||';
141700140213           strTRBM=strTRBM+'§§Tag19§§=='+%trim(wLnaLocal)+'||';
141800140213           strTRBM=strTRBM+'§§Tag20§§=='+'('+%trim(wLnaProv)+')'+'||';
141900140213           strTRBM=strTRBM+'§§Tag21§§=='+%trim(wLnaTel)  +'||';
142000140213           strTRBM=strTRBM+'§§Tag22§§=='+%trim(wLnaEmail)+'@brt.it'+'||';
142100170719           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
142200170719           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedCAP)+' '+
142300170719                 %trim(wSpedLocal)+'  '+%trim(wSpedProv)+'||';
142400140213           strTRBM=strTRBM+'§§Tag34§§=='+%trim(wSpedDataA) +'||';
142500141016           strTRBM=strTRBM+'§§Tag36§§=='+%trim(wLnaOraTMD_A)+'||';
142600141016           strTRBM=strTRBM+'§§Tag37§§=='+%trim(wLnaOraTMA_A) +'||';
142700141016           if wLnaOraTPD_A <> *blanks;
142800141016              strTRBM=strTRBM+'§§Tag38§§=='+' e  ' + %trim(wLnaOraTPD_A)+
142900141016                                            ' - '  + %trim(wLnaOraTPA_A)+'||';
143000140213           else;
143100140213              strTRBM=strTRBM+'§§Tag38§§=='+''+'||';
143200140213           endif;
143300141016
143400141016           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
143500141016
143600141016           strTRBM=strTRBM+'§§Tag42§§=='+%trim(wLnaOraMMD_A)+'||';
143700141016           strTRBM=strTRBM+'§§Tag43§§=='+%trim(wLnaOraMMA_A) +'||';
143800141016           if wLnaOraMPD_A <> *blanks;
143900141016              strTRBM=strTRBM+'§§Tag44§§=='+' e  ' + %trim(wLnaOraMPD_A)+
144000141016                                            ' - '  + %trim(wLnaOraMPA_A)+'||';
144100141016           else;
144200141016              strTRBM=strTRBM+'§§Tag44§§=='+''+'||';
144300141016           endif;
144400150530
144500150601           if wDataLimitFD <> *blanks;
144600150601              strTRBM=strTRBM+'§§Tag45§§=='+%trim(wDataLimitFD)+'||';
144700150601           else;
144800150601              strTRBM=strTRBM+'§§Tag45§§=='+''+'||';
144900150601           endif;
145000140507
145100140213
145200140213           if Bartmail_insert(strTRBM:indmrabyps:
145300140213                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
145400140612              wTesto =
145500140612                  'La informiamo che la spedizione sotto indicata a Lei ' +
145600140612                  'diretta, per la quale il mittente ha disposto il fermo ' +
145700140612                  'deposito presso i ns magazzini, è disponibile per il ' +
145800140612                  'ritiro.';
145900140612
146000140213           else;
146100140213              *in40 = *off;
146200140213              wMessaggio = 'Errore in push-entry in BartMail';
146300140213           endif;
146400140213
146500140213          else;
146600140213            *in40 = *off;
146700170208            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_FD non trovata';
146800140213          endif;
146900140213
147000140213       endsr;
147100170208
147200170208
147300170208
147400170208       //-------------------------------------------------------------*
147500170208       //Gestione invio tipo alert AGD via EMAIL                      *
147600170208       //-------------------------------------------------------------*
147700170208       Begsr sr_AGD_Email;
147800170208
147900170208         clear indmrabyps;
148000170208
148100170208         dsiaGIA = siaNOTE;
148200170214
148300170214         // Controllo dati della giacenza (obbligatori)
148400170214         if §SIADAP   > *zeros  AND
148500170214            §SIACMC  <> *blanks AND
148600170214            §SIACMCD <> *blanks;
148700170214
148800170208         wGiaData = %trim(%editw(%dec(%date(§SIADAP):*eur):'  /  /    '));
148900170208
149000170208         chain ('UFI':wLNA_A) tntbe01l;
149100170208         if %found(tntbe01l);
149200170208            wLinkFil = %trim(tbeuni);
149300170208         else;
149400170208            clear wLinkFil;
149500170208         endif;
149600170208
149700170208         w§CM1VAR = '*OBJM*BRT - Avviso di MANCATA CONSEGNA ' +
149800170208                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
149900170208         chain ('MRA':'TRBMBYPSR':'ALE_AGD') tntbe01l;
150000170208
150100170208          if %found(tntbe01l);
150200170208           indmrabyps=tbeuni;
150300170208           exsr sr_trtcm1ds;
150400170208
150500170208           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
150600170208           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
150700170208
150800170208           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
150900170208           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
151000170208           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
151100170208           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
151200170208           if wFlgContrAss;
151300170208              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
151400170208           else;
151500170208              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
151600170208           endif;
151700170209
151800170209           strTRBM=strTRBM+'§§Tag21§§=='+%trim(wLnaTel)  +'||';
151900170209           strTRBM=strTRBM+'§§Tag22§§=='+%trim(wLnaEmail)+'@brt.it'+'||';
152000170208
152100170208           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
152200170208           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedCAP)+' '+
152300170208                 %trim(wSpedLocal)+'  '+%trim(wSpedProv)+'||';
152400170208           strTRBM=strTRBM+'§§Tag30§§=='+%trim(wGiaData)+'||';
152500170208
152600170208           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
152700170208
152800170208           strTRBM=strTRBM+'§§Tag50§§=='+'<b>'+%trim(§SIACMCD)+'</b>'+'||';
152900170208           strTRBM=strTRBM+'§§Tag51§§=='+
153000170208               ' <a href="'+%trim(wLinkFil)+'">'+%trim(wLnaDescr)+'</a>'+'||';
153100170208           strTRBM=strTRBM+'§§Tag52§§=='+
153200170208                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
153300170208           strTRBM=strTRBM+wBRTcode;
153400170208           strTRBM=strTRBM+'&urltype=a'+'||';
153500170208
153600170208           if Bartmail_insert(strTRBM:indmrabyps:
153700170208                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
153800170208                wTesto =
153900170208                   'La merce è in giacenza presso la filiale di ' +
154000170208                   %trim(wLnaDescr) + ' in attesa di disposizioni.';
154100170208
154200170208           else;
154300170208              *in40 = *off;
154400170208              wMessaggio = 'Errore in push-entry in BartMail';
154500170208           endif;
154600170208
154700170208          else;
154800170208            *in40 = *off;
154900170208            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_AGD non trovata';
155000170208          endif;
155100170214
155200170214         else;
155300170214           *in40 = *off;
155400170214           wMessaggio = 'Errore dati in DSIAGIA';
155500170214
155600170214         endif;
155700170208
155800170208       endsr;
155900170208
156000140212
156100140212
156200140212       //-------------------------------------------------------------*
156300140212       //Gestione invio tipo alert RIC via SMS                        *
156400140212       //-------------------------------------------------------------*
156500140212       Begsr sr_RIC_SMS;
156600140212
156700140212         // aggiungo SMS da inviare
156800140311         if wFlgNoOrari;
156900141009            wOggiSMS = 'Oggi';
157000140311         else;
157100141009            wOggiSMS = 'Ore ' + %trim(wRicOra);
157200140311         endif;
157300140213
157400140213         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
157500140213         ALESInDS_SMS.i_message =
157600150530                %trim(wOggiSMS) + ' tentata consegna spedizione di '           +
157700150530                %trim(wRSM_Sms) + '. '                                         +
157800150603                'Ripasseremo ' + %trim(%subst(wSpedDataConsA:1:8)) + ' '       +
157900150530                %trim(wSpedConsOrari) + '. '                                   +
158000170508                'Puoi modificare la ns programmazione su www.brt.it '          +
158100150530                ' BRTcode ' + wBRTcode;
158200140612         wTesto = ALESInDS_SMS.i_message;
158300140212
158400140213         ALESInDS_SMS.i_IdSMS   =
158500140213                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
158600140214                     %editc(wAAS:'X')+%editc(siaLNP:'X')     +
158700140213                     %editc(siaNRS:'X')+%editc(siaNSP:'X')     + '_' + 'ARIC';
158800140213
158900140213         ALESInDS_SMS.i_cell    = wDestMobile;
159000151002         if not wFlgTest;
159100150930  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
159200140214         endif;
159300140212
159400140212
159500140213         ALESInOpz = 'W';
159600170323         ALESInPRG = sVGDPRG;
159700140212         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
159800170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
159900140731                             ALESOutPRG:ALESOutEsito);
160000140212         if %error OR ALESOutEsito <> *blanks;
160100140212            *in40 = *off;
160200140212            wMessaggio = 'KO da driver FNLSALESR';
160300140213         else;
160400140213            wFlgSMSsent = *on;
160500140212         endif;
160600140212
160700140212       endsr;
160800140213
160900140213
161000140213       //-------------------------------------------------------------*
161100140213       //Gestione invio tipo alert FD  via SMS                        *
161200140213       //-------------------------------------------------------------*
161300140213       Begsr sr_FD_SMS;
161400140213
161500140213         // aggiungo SMS da inviare
161600140213
161700140213         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
161800140213         ALESInDS_SMS.i_message =
161900170508         'La tua spedizione da ' + %trim(wRSM_Sms) + ' e'+api+ ' disponibile'  +
162000150604                ' per il ritiro ' + %trim(%subst(wDataLimitFD:1:14))           +
162100170508                ' presso la filiale di '                                       +
162200150530                %trim(wLnaLocal) + ' (' + %trim(wLnaProv) + ') in '            +
162300150604                %trim(wLnaIndir) + ' - Tracking su www.brt.it'                 +
162400150530                '  BRTcode ' + %trim(wBRTcode);
162500140612         wTesto = ALESInDS_SMS.i_message;
162600140311
162700140213         ALESInDS_SMS.i_IdSMS   =
162800140213                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
162900140214                     %editc(wAAS:'X')+%editc(siaLNP:'X')     +
163000140530                     %editc(siaNRS:'X')+%editc(siaNSP:'X')     + '_' + 'AFFD';
163100140213
163200140213         ALESInDS_SMS.i_cell    = wDestMobile;
163300151002         if not wFlgTest;
163400140217  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
163500140214         endif;
163600140213
163700140213
163800140213         ALESInOpz = 'W';
163900170323         ALESInPRG = sVGDPRG;
164000170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
164100170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
164200170323                             ALESOutPRG:ALESOutEsito);
164300140213         if %error OR ALESOutEsito <> *blanks;
164400140213            *in40 = *off;
164500140213            wMessaggio = 'KO da driver FNLSALESR';
164600140213         else;
164700140213            wFlgSMSsent = *on;
164800140213         endif;
164900140213
165000140213       endsr;
165100150930
165200150930
165300150930       //-------------------------------------------------------------*
165400150930       //Gestione invio tipo alert CL1 / CL2 / CL3 via SMS            *
165500150930       //-------------------------------------------------------------*
165600150930       Begsr sr_CL_SMS;
165700150930
165800150930         // aggiungo SMS da inviare
165900150930
166000150930         dsiaICA = siaNOTE;
166100150930         wSpedConsOraDa = %trim(%editw(§SIANOPCD:'  :  '));
166200150930         wSpedConsOraA  = %trim(%editw(§SIANOPCA:'  :  '));
166300150930         wSpedConsOrari = '('+ wSpedConsOraDa + '  ' +
166400150930                               wSpedConsOraA + ')';
166500150930
166600150930         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
166700150930         ALESInDS_SMS.i_message =
166800150930                'Per cause operative, la consegna da ' +
166900150930                %trim(wRSM_Sms) + ' e'' stata riprogrammata al ' +
167000150930                %subst(%editc(§SIANDPC:'X'):7:2) + '/' +
167100150930                %subst(%editc(§SIANDPC:'X'):5:2) + ' ' +
167200170508                %trim(wSpedConsOrari)+'. Puoi modificare la nostra ' +
167300170508                ' programmazione su www.brt.it' +
167400150930                ' BRTcode ' + wBRTcode;
167500150930         wTesto = ALESInDS_SMS.i_message;
167600150930
167700150930         ALESInDS_SMS.i_IdSMS   =
167800150930                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
167900150930                     %editc(wAAS:'X')+%editc(siaLNP:'X') +
168000150930                     %editc(siaNRS:'X')+%editc(siaNSP:'X') + '_' +
168100150930                     'A' + %trim(siaTIPA);
168200150930
168300150930         ALESInDS_SMS.i_cell    = wDestMobile;
168400151002         if not wFlgTest;
168500150930  xxx     //  ALESInDS_SMS.i_cell    = '+393473441969';
168600150930         endif;
168700150930
168800150930
168900150930         ALESInOpz = 'W';
169000170323         ALESInPRG = sVGDPRG;
169100170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
169200170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
169300170323                             ALESOutPRG:ALESOutEsito);
169400150930         if %error OR ALESOutEsito <> *blanks;
169500150930            *in40 = *off;
169600150930            wMessaggio = 'KO da driver FNLSALESR';
169700150930         else;
169800150930            wFlgSMSsent = *on;
169900150930         endif;
170000150930
170100150930       endsr;
170200150930
170300150930
170400150930       //-------------------------------------------------------------*
170500150930       //Gestione invio tipo alert CL1 / CL2 / CL3 via EMAIL          *
170600150930       //-------------------------------------------------------------*
170700151001       Begsr sr_CL_EMAIL;
170800151001
170900151001         clear indmrabyps;
171000151001
171100151001         dsiaICA = siaNOTE;
171200151001         wSpedConsOraDa = %trim(%editw(§SIANOPCD:'  :  '));
171300151001         wSpedConsOraA  = %trim(%editw(§SIANOPCA:'  :  '));
171400151001         wSpedConsOrar2 = 'dalle ' + wSpedConsOraDa +
171500151001                          ' alle ' + wSpedConsOraA;
171600151001         wSpedDataConsD =
171700151001                %subst(%editc(§SIANDPC:'X'):7:2) + '/' +
171800151001                %subst(%editc(§SIANDPC:'X'):5:2) + '/' +
171900151001                %subst(%editc(§SIANDPC:'X'):1:4);
172000151001
172100151001         w§CM1VAR = '*OBJM*BRT - Variazione data consegna ' +
172200151001                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
172300151001         chain ('MRA':'TRBMBYPSR':'ALE_VDC') tntbe01l;
172400151001
172500151001          if %found(tntbe01l);
172600151001           indmrabyps=tbeuni;
172700151001           exsr sr_trtcm1ds;
172800151001
172900151001           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
173000151001           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
173100151001           strTRBM=strTRBM+'§§Tag08§§=='+
173200161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
173300151001           strTRBM=strTRBM+wBRTcode;
173400151001           strTRBM=strTRBM+'&urltype=a'+'||';
173500151005           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
173600151005           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
173700151001           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
173800151001           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
173900151001           if wFlgContrAss;
174000151001              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
174100151001           else;
174200151001              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
174300151001           endif;
174400151001           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
174500151001           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedLocal) +'  '
174600151001                                        +%trim(wSpedProv)+'||';
174700151001           strTRBM=strTRBM+'§§Tag16§§=='+%trim(wLnaDescr)+'||';
174800151001           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wLnaIndir)+'||';
174900151001           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wLnaCAP)  +'||';
175000151001           strTRBM=strTRBM+'§§Tag19§§=='+%trim(wLnaLocal)+'||';
175100151001           strTRBM=strTRBM+'§§Tag20§§=='+'('+%trim(wLnaProv)+')'+'||';
175200151001           strTRBM=strTRBM+'§§Tag32§§=='+%trim(wSpedDataConsD)+'||';
175300151001           strTRBM=strTRBM+'§§Tag33§§=='+%trim(wSpedConsOrar2)+'||';
175400151001           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
175500151001
175600151001           if Bartmail_insert(strTRBM:indmrabyps:
175700151001                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
175800151001                wTesto =
175900151001                   'Consegna riprogrammata per il ' +
176000151001                   %trim(wSpedDataConsD) + ' indicativamente ' +
176100151001                   %trim(wSpedConsOrar2);
176200151001
176300151001           else;
176400151001              *in40 = *off;
176500151001              wMessaggio = 'Errore in push-entry in BartMail';
176600151001           endif;
176700151001
176800151001          else;
176900151001            *in40 = *off;
177000151001            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_VDC non trovata';
177100151001          endif;
177200150930
177300150930       endsr;
177400170208
177500170208
177600170208
177700170208       //-------------------------------------------------------------*
177800170208       //Gestione invio tipo alert AGD via SMS                        *
177900170208       //-------------------------------------------------------------*
178000170208       Begsr sr_AGD_SMS;
178100170208
178200170208         dsiaGIA = siaNOTE;
178300170208         wGiaData = %trim(%editw(%dec(%date(§SIADAP):*eur):'  /  /    '));
178400170208
178500170208         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
178600170324           ALESInDS_SMS.i_message =
178700170324                  'Spedizione da ' + %trim(wRSM_Sms) + ' non consegnata il ' +
178800170324                  %trim(wGiaData) + ' - ' + %trim(§SIACMCD) + '. ' +
178900170508                  'Puoi indicare disposizioni di RICONSEGNA su www.brt.it ' +
179000170324                  'BRTcode ' + wBRTcode;
179100170324        // ALESInDS_SMS.i_message =
179200170324        //        'Spedizione da ' + %trim(wRSM_Sms) + ' non consegnata il ' +
179300170324        //        %trim(wGiaData) + ' - ' + %trim(§SIACMCD) + '. ' +
179400170324        //        'Per chiedere la riconsegna contatti il Servizio Clienti: ' +
179500170324        //        %trim(wLnaEmail)+'@brt.it' + ' Tel ' + %trim(wLnaTel) + '  ' +
179600170324        //        'BRTcode ' + wBRTcode;
179700170209
179800170208         wTesto = ALESInDS_SMS.i_message;
179900170208
180000170208         ALESInDS_SMS.i_IdSMS   =
180100170208                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
180200170208                     %editc(wAAS:'X')+%editc(siaLNP:'X')     +
180300170407                     %editc(siaNRS:'X')+%editc(siaNSP:'X')     + '_' + 'AAGD';
180400170208
180500170208         ALESInDS_SMS.i_cell    = wDestMobile;
180600170208         if not wFlgTest;
180700170208  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
180800170208         endif;
180900170208
181000170208
181100170208         ALESInOpz = 'W';
181200170323         ALESInPRG = sVGDPRG;
181300170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
181400170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
181500170323                             ALESOutPRG:ALESOutEsito);
181600170208         if %error OR ALESOutEsito <> *blanks;
181700170208            *in40 = *off;
181800170208            wMessaggio = 'KO da driver FNLSALESR';
181900170208         else;
182000170208            wFlgSMSsent = *on;
182100170208         endif;
182200170208
182300170208       endsr;
182400151001
182500140212
182600140212
182700140212       //-------------------------------------------------------------*
182800140212       //Valorizzazione trtcm1ds                                      *
182900140212       //-------------------------------------------------------------*
183000140212        Begsr sr_trtcm1ds;
183100140212
183200140212           clear intrtcm1ds;
183300151002           string=§mraemlmit;
183400140212           if %scan('*PO':§mraemlmit) > *zeros;
183500140212              string=%replace(wLNA_A:§mraemlmit:
183600140212                              %scan('*PO':§mraemlmit:1):3);
183700140212           endif;
183800140212           exsr sr_chkstr;
183900140212           §CM1MITT=string;
184000140212           string=wDestEmail;
184100151002           if not wFlgTest;
184200171006  xxx       //      string='email_di_test@brt.it';
184300140214           endif;
184400140212           exsr sr_chkstr;
184500140212           §CM1DST=string;
184600140212           §CM1TIPS=§mrareg;
184700140212           if §mrapo<> '*PO';
184800140212              §CM1PO=§mrapo;
184900140212           else;
185000140212              §CM1PO=%editc(arbLNP:'X');
185100140212           endif;
185200140212           §CM1VAR=w§CM1VAR;
185300140212           §CM1STS='0';
185400140212           §CM1IDP=§mraidpro;
185500140212           §CM1PROG=w§CM1PROG;
185600140212           §CM1ATT='01';
185700140212           §CM1TOTATT='01';
185800140212
185900140212        endsr;
186000140212
186100140212
186200140212       //-------------------------------------------------------------*
186300140212       //controllo stringa                                            *
186400140212       //-------------------------------------------------------------*
186500140212        Begsr sr_chkstr;
186600140212           string=%xlate('||':'  ':string);
186700140212           string=%xlate('==':'  ':string);
186800140212        endsr;
186900140212
187000140212
187100140212       //-------------------------------------------------------------*
187200140212       //Invio email di alert                                         *
187300140212       //-------------------------------------------------------------*
187400140214       Begsr snd_Alert;
187500140212
187600140212          wEml   = 'cedalert@brt.it';
187700140212          wOgg   = 'Anomalia invio alert destinatari ' + w§CM1PROG;
187800140214          wMsg   = ' ID BartMail   '     + w§CM1PROG             + ':/N' +
187900140214                   ' siaAAS        '     + %editc(siaAAS:'X')    + ':/N' +
188000140212                   ' siaLNP        '     + %editc(siaLNP:'X')    + ':/N' +
188100140212                   ' siaNRS        '     + %editc(siaNRS:'X')    + ':/N' +
188200140212                   ' siaNSP        '     + %editc(siaNSP:'X')    + ':/N' +
188300140212                   ' siaTIPA       '     + %trim(siaTIPA)        + ':/N' +
188400140212                   ' Email         '     + %trim(wDestEmail)     + ':/N' +
188500140212                   ' Mobile        '     + %trim(wDestMobile)    + ':/N' +
188600140214                   ' Errore        '     + wEsito+' '
188700140214                                         + %trim(wMessaggio) + ':/N';
188800140212
188900140212          callP(e) TIS701C1 (wEml : wCcEml : wOgg : wMsg : wSndSts);
189000140212
189100140212        endsr;
189200140213
189300140213
189400140213       //-------------------------------------------------------------*
189500140213       //inserimento Richiesta Assistenza                             *
189600140213       //-------------------------------------------------------------*
189700170208        Begsr sr_InsRA;
189800140213
189900140213           *in50 = *off;
190000140213
190100140414           clear fidna6ds;
190200140213           select;
190300140213             when siaTIPA = 'RIC';
190400140414                iDA06MAD = ' 81';
190500140213                  select;
190600140213                    when wRicTipo = 'P';
190700140414                       iDA06NO1 = 'Alert inviato da chiusura PDA a:';
190800140218                    when wRicTipo = 'S';
190900140414                       iDA06NO1 = 'Alert inviato da chiusura PDA (SPC) a:';
191000140213                    when wRicTipo = 'C';
191100140414                       iDA06NO1 = 'Alert inviato da Chiusura Distinta a:';
191200140213                    other;
191300140414                       iDA06NO1 = 'Alert inviato a:';
191400140213                  endsl;
191500140213             when siaTIPA = 'FD';
191600140414                iDA06MAD = ' 80';
191700140414                iDA06NO1 = 'Alert inviato a:';
191800151001             when siaTIPA = 'CL1';
191900151001                iDA06MAD = ' 84';
192000151002                iDA06NO1 = 'IMP/DISGUIDO';
192100151001             when siaTIPA = 'CL2';
192200151001                iDA06MAD = ' 84';
192300151001                iDA06NO1 = 'DISGUIDO';
192400151001             when siaTIPA = 'CL3';
192500151001                iDA06MAD = ' 84';
192600170208                iDA06NO1 = 'NON IN DISTINTA';
192700170208             when siaTIPA = 'AGD';
192800170208                iDA06MAD = ' 87';
192900170208                iDA06NO1 = 'Alert inviato a:';
193000140213             other;
193100140213                  *in50 = *on;
193200140213           endsl;
193300140908
193400140908           select;
193500140908             when wTipoInvio = 'S';
193600140908                  IDA06RSC = 'SMS';
193700140908             when wTipoInvio = 'M';
193800140908                  IDA06RSC = 'EMAIL';
193900140908             when wTipoInvio = 'B';
194000140908                  IDA06RSC = 'SMS/EMAIL';
194100140908           endsl;
194200140213
194300140414           iDA06TOR = 'S';
194400140414           iDA06OGG = %trim(wSpedNum)+%trim(%editc(wAAS:'X'));
194500140414           iDA06UTE = 'QPGMR';
194600140414           iDA06FIL = arbLNA;
194700140612           iDA06NO1 = %trim(iDA06NO1) + ' ' + %trim(wDescRA) +
194800140612                      '  -  ' + wTesto;
194900140213
195000140213           if not *in50;
195100140414              callP(e) FIDNA6R (fidna6ds);
195200140213              if %error;
195300140213                 *in40 = *off;
195400140414                 wMessaggio = 'KO da driver FIDNA6R';
195500140213              endif;
195600140213           endif;
195700140213
195800140213        endsr;
195900140212
196000140212
196100140212       //-------------------------------------------------------------*
196200140212       //reperimento progressivo                                      *
196300140212       //-------------------------------------------------------------*
196400140212        Begsr sr_rtvPrg;
196500140212
196600140212           clear trul33ds;
196700140212           w§CM1PROG=*all'0';
196800140212           i33Cnu = 24;
196900140212           i33num = 1;
197000140212           kpjbu = trul33ds;
197100140212           trul33r(kpjba);
197200140212           trul33ds = kpjbu;
197300140212           w§CM1PROG=%subst(%editc(o33nri:'X'):9:7);
197400140212
197500140212        endsr;
197600140212
197700140210
