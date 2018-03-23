000100110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200141106     h dftactgrp(*no) actgrp(*caller) bnddir('TRBM':'UBBNDDIR':'TRUL')
000300110617
000400110617      *---------------------------------------------------------------*
000500140207     fazorg01l  if   e           k disk
000600140211     ffnarb01l  if   e           k disk
000700140210     ffiar901l  if   e           k disk
000800140210     ftabel00f  if   e           k disk
000900140212     ftntbe01l  if   e           k disk
001000180207     f****fiapd01l  if   e           k disk
001100130309
001200140212
001300140212     D FNLV81R         PR
001400140207     D                                     extpgm('FNLV81R')
001500140212     D fnlv81ds                            likeds(fnlv81ds)
001600140213
001700140213
001800140414     D FIDNA6R         PR
001900140414     D                                     extpgm('FIDNA6R')
002000140414     D fidna6ds                            likeds(fidna6ds)
002100140212
002200140212
002300140311     D TRULORSR        PR
002400140311     D                                     extpgm('TRULORSR')
002500140311     D kpjba                               likeds(kpjba)
002600140311     D trulORsds                           likeds(trulORsds)
002700140311     D trulOR2ds                           likeds(trulOR2ds)
002800141009
002900141009
003000141009     D TRUL28R0        PR
003100141009     D                                     extpgm('TRUL28R0')
003200141009     D trul28ds0                           likeds(trul28ds0)
003300140212
003400140207
003500140207     D TRUL33R         PR
003600140207     D                                     extpgm('TRUL33R')
003700140207     D kpjba                               likeds(kpjba)
003800140211
003900140211
004000140211     D FNLRJ6R         PR
004100140211     D                                     extpgm('FNLRJ6R')
004200140211     D fnlrj6ds                            likeds(fnlrj6ds)
004300140212
004400140212
004500140212     D FNLV82R         PR
004600140212     D                                     extpgm('FNLV82R')
004700140212     D fnlv82ds                            likeds(fnlv82ds)
004800140213
004900140213
005000140213     D XGIOLAV         PR
005100140213     D                                     extpgm('XGIOLAV')
005200140213     D xgiolavds                           likeds(xgiolavds)
005300141010
005400141010
005500141010     D TRULVPOR        PR
005600141010     D                                     extpgm('TRULVPOR')
005700141010     D trulvpods                           likeds(TRULVPODS)
005800140212
005900140212
006000140212     D FNLSALESR       PR
006100140212     D                                     extpgm('FNLSALESR')
006200140212     D InChiudi                            like(ALESInChiudi)
006300140212     D InOpz                               like(ALESInOpz)
006400140212     D InKSU                               like(ALESInKSU)
006500140212     D InTIP                               like(ALESInTIP)
006600140212     D InTSC                               like(ALESInTSC)
006700140212     D InPGM                               like(ALESInPGM)
006800170323     D InPRG                               like(ALESInPRG)
006900140213     D InDS_SMS                            likeds(ALESInDS_SMS)
007000140212     D OutPRG                              like(ALESOutPRG)
007100140212     D OutEsito                            like(ALESOutEsito)
007200141106
007300141106
007400141106     D UBSPESTS        PR
007500141106     D                                     extpgm('UBSPESTS')
007600141106     D pInAnno                             like(pInAnno)
007700141106     D pInLineaPar                         like(pInLineaPar)
007800141106     D pInSerie                            like(pInSerie)
007900141106     D pInNumSped                          like(pInNumSped)
008000141106     D pInLingua                           like(pInLingua)
008100141106     D pInMaiuscolo                        like(pInMaiuscolo)
008200141106     D pOutLblTyp                          like(pOutLblTyp)
008300141106     D pOutRblBO                           like(pOutRblBO)
008400141106     D pOutStsSped                         like(pOutStsSped)
008500141106     D pOutDtConsRich                      like(pOutDtConsRich)
008600141106     D pOutDtApGiac                        like(pOutDtApGiac)
008700141106     D pOutDtLAvviso                       like(pOutDtLAvviso)
008800141106     D pOutDescSts                         like(pOutDescSts)
008900141106     D pOutDtSts                           like(pOutDtSts)
009000141106     D pOutErrore                          like(pOutErrore)
009100141106     D pOutCA                              like(pOutCA)
009200141106     D pOutDtPrGiac                        like(pOutDtPrGiac)
009300141106     D pOutDtChGiac                        like(pOutDtChGiac)
009400140212
009500140212
009600140212     D TIS701C1        PR
009700140212     D                                     extpgm('TIS701C1')
009800140212     D wEml                                like(wEml)
009900140212     D wCcEml                              like(wCcEml)
010000140212     D wOgg                                like(wOgg)
010100140212     D wMsg                                like(wMsg)
010200140212     D wSndSts                             like(wSndSts)
010300140212
010400140212
010500130309
010600140207     D kpjba         e ds                  inz
010700140212     D ds3A          e ds                  inz
010800140401     D og129         e ds                  inz
010900141016     D og130         e ds                  inz
011000150929     D og143         e ds                  inz
011100140212     D xgiolavds     e ds                  inz
011200140414     D fidna6ds      e ds                  inz
011300140212     D trul33ds      e ds                  inz
011400140212     D fnlv81ds      e ds                  inz
011500140212     D fnlv55ds      e ds                  inz
011600140212     D fnlrj6ds      e ds                  inz
011700140212     D fnlv82ds      e ds                  inz
011800141009     D trul28ds0     e ds                  qualified inz
011900140311     D trulORsds     e ds                  inz
012000140311     D trulOR2ds     e ds                  inz
012100140311     D tnsd99ds      e ds
012200140212     D fisia00f      e ds                  inz
012300140212     D §siaRRN         s             10I 0 inz
012400140212     D dsiaRIC       e ds                  inz
012500140214     D dsiaFLO       e ds                  inz
012600140214     D dsiaMSG       e ds                  inz
012700150930     D dsiaICA       e ds                  inz
012800170208     D dsiaGIA       e ds                  inz
012900150929     D diore01       e ds                  inz
013000150929     D dntw          e ds                  inz
013100170222     D UBRSMALDS     E DS                  QUALIFIED
013200140214     D wAAS            s                   like(siaAAS) inz
013300140212     D wDivid          s                   like(InDivid) inz
013400140212     D wSQL            s           2048A   varying inz
013500140212     D wLNA            s                   like(arbLNA) inz
013600140212     D wLNA_A          s              3A   inz
013700140207     D wTipoInvio      s              1A   inz
013800140212     D wCbc            s              2    dim(100) inz
013900140212     D wCbcIdx         s              3  0 inz
014000140212     D w§CM1VAR        s                   like(§CM1VAR)  inz
014100140212     D w§CM1PROG       s                   like(§CM1PROG) inz
014200141015     D wVPOReml        s              6A   inz
014300141015     D wVPORsms        s              6A   inz
014400140212     D string          s            200A   varying inz
014500140212     D strTRBM         s          65535A   varying inz
014600150313     D api             s              1A   inz('''')
014700140212     D wEml            s            253    inz
014800140212     D wCcEml          s            253    inz
014900140212     D wOgg            s             44    inz
015000140212     D wMsg            s           5000    inz
015100140212     D wSndSts         s              1    inz
015200140212     D pEsito          s             10I 0 inz
015300140612     D wTesto          s           1600    varying inz
015400170222     d wRSM_Eml        s             60A   inz
015500170222     d wRSM_Sms        s             60A   inz
015600141106
015700141106     d pInAnno         s              4s 0 inz
015800141106     d pInLineaPar     s              3s 0 inz
015900141106     d pInSerie        s              2s 0 inz
016000141106     d pInNumSped      s              7s 0 inz
016100141106     d pInLingua       s              2    inz
016200141106     d pInMaiuscolo    s              1    inz
016300141106     d pOutLblTyp      s              1    inz
016400141106     d pOutRblBO       s              1    inz
016500141106     d pOutStsSped     s              2    inz
016600141106     d pOutDtConsRich  s              8S 0 inz
016700141106     d pOutDtApGiac    s              8s 0 inz
016800141106     d pOutDtLAvviso   s              8s 0 inz
016900141106     d pOutDescSts     s            128    inz
017000141106     d pOutDtSts       s              8S 0 inz
017100141106     d pOutErrore      s              1    inz
017200141106     d pOutCA          s              1    inz
017300141106     d pOutDtPrGiac    s              8s 0 inz
017400141106     d pOutDtChGiac    s              8s 0 inz
017500141106
017600141106
017700141009     D BRTcode         ds                  qualified
017800141009     D   aas4                         4  0
017900141009     D   lnp                          3  0
018000141009     D   nrs                          2  0
018100141009     D   nsp                          7  0
018200141009     D   chkdgt1                      1  0
018300141009     D   code                         3  0
018400141009     D   chkdgt2                      1  0
018500141009     D   BRTcode               3     21
018600141010
018700141010
018800141010      // DS per filiali abilitate al deploy
018900141010     D TRULVPODS     e ds
019000141010     D  skFilOk               16    765    dim(250)
019100141014     D skFilOkEml      s                   like(skFilOk) dim(250) inz
019200141014     D skFilOkSms      s                   like(skFilOk) dim(250) inz
019300140212
019400140212
019500140212     D* DS di output
019600140213     D  ALESInDS_SMS   ds                  qualified inz
019700140212     D   i_MittSMS                   30
019800140212     D   i_IdSMS                     36
019900140212     D   i_cell                      20
020000140212     D   i_message                 1600
020100140212
020200140212
020300140212     D ALESInChiudi    s              1A   inz
020400140212     D ALESInOpz       s              1A   inz
020500140212     D ALESInKSU       s              8A   inz('0BART001')
020600140212     D ALESInTIP       s              2A   inz('SM')
020700140212     D ALESInTSC       s              2A   inz('WW')
020800170323     D ALESInPGM       s             10A   inz
020900170323     D ALESInPRG       s             10A   inz
021000140212     D ALESOutPRG      s             10A   inz
021100140212     D ALESOutEsito    s              1A   inz
021200170323     D sVGDPRG         s             10A   inz
021300140212
021400140212
021500140210
021600140210     D* Variabili di "mapping"
021700140213     D wRicTipo        s                   like(§SIACHI)  inz
021800140212     D wRicData        s             10A                  inz
021900140212     D wRicOra         s              5A                  inz
022000170208     D wGiaData        s             10A                  inz
022100140211     D wLnaDescr       s                   like(orgDES)   inz
022200140211     D wLnaIndir       s                   like(orgIND)   inz
022300140212     D wLnaCAP         s              9A                  inz
022400140210     D wLnaLocal       s                   like(orgLOC)   inz
022500140211     D wLnaProv        s                   like(orgPRO)   inz
022600140211     D wLnaTel         s                   like(orgTEL)   inz
022700140210     D wLnaEmail       s                   like(orgDD2)   inz
022800141016     D wLnaOraMMD_A    s              5A                  inz
022900141016     D wLnaOraMMA_A    s              5A                  inz
023000141016     D wLnaOraMPD_A    s              5A                  inz
023100141016     D wLnaOraMPA_A    s              5A                  inz
023200141016     D wLnaOraTMD_A    s              5A                  inz
023300141016     D wLnaOraTMA_A    s              5A                  inz
023400141016     D wLnaOraTPD_A    s              5A                  inz
023500141016     D wLnaOraTPA_A    s              5A                  inz
023600140211     D wMitIndir       s                   like(arbINM)   inz
023700140211     D wMitCAP         s                   like(arbCAM)   inz
023800140211     D wMitLocal       s                   like(arbLOM)   inz
023900140211     D wMitProv        s                   like(arbPRM)   inz
024000140210     D wSpedDataA      s             10                   inz
024100150601     D wDataLimitFD    s             20                   inz
024200141014     D wBRTcode        s             19                   inz
024300141014     D wBRTcodeLong    s             25                   inz
024400140210     D wSpedNum        s             12                   inz
024500140210     D wSpedRmn        s             15                   inz
024600140211     D wSpedRma        s                   like(arbRMA)   inz
024700140211     D wSpedRsd        s                   like(arbRSD)   inz
024800140211     D wSpedInd        s                   like(arbIND)   inz
024900170208     D wSpedCAP        s                   like(arbCAD)   inz
025000140211     D wSpedLocal      s                   like(arbLOD)   inz
025100140211     D wSpedProv       s                   like(arbPRD)   inz
025200140210     D wSpedColli      s              5A                  inz
025300140211     D wSpedKg         s              8A                  inz
025400140211     D wSpedCAS        s             20A                  inz
025500140212     D wSpedDataConsA  s             20A                  inz
025600151001     D wSpedDataConsD  s             20A                  inz
025700140211     D wSpedConsOrari  s             50A                  inz
025800150530     D wSpedConsOrar2  s             50A                  inz
025900140212     D wSpedConsOraDa  s              5A                  inz
026000140212     D wSpedConsOraA   s              5A                  inz
026100140212     D wDestEmail      s             71A                  inz
026200140212     D wDestMobile     s             16A                  inz
026300140324     D wDescRA         s            256A                  inz
026400140311     D wOggiSMS        s             50A                  inz
026500140317     D wDataOraNext    s             14S 0                inz
026600140317     D wDataOraCorr    s             14S 0                inz
026700150929     D skFilExt        s                   like(orgFIL) dim(500) inz
026800150929     D ixFilExt        s              3  0 inz
026900170208     D wLinkFil        s                   inz like(tbeUNI)
027000140210
027100140211
027200140212     D wEsito          s              1A   inz
027300140212     D wMessaggio      s            256A   varying inz
027400140212
027500140218     D wFlgNoOrari     s               N   inz('0')
027600140214     D wFlgTest        s               N   inz('0')
027700140211     D wFlgContrAss    s               N   inz('0')
027800140213     D wFlgSMSsent     s               N   inz('0')
027900140507     D wFlgFDAutMitt   s               N   inz('0')
028000140211
028100140212     D* Flag "globali"
028200140212     D wFlgTabelle     s               N   inz('0')
028300120619
028400140207
028500140207      // ? PROTOTIPI ?
028600140207      /copy gaitrasrc/srcprotopr,trbmbypsr
028700140207      /copy gaitrasrc/srcprotopr,fnlv55r
028800140311      /copy gaitrasrc/srcprotopr,tnsd99r
028900170222      /copy gaitrasrc/srcProtoPr,UBRSMALR
029000110617
029100140207
029200110617
029300110627     C     *ENTRY        PLIST
029400140207     C                   PARM                    InChiudi          1
029500140207     C                   PARM                    InDivis           2 0
029600140207     C                   PARM                    InDivid           2 0
029700140207     C                   PARM                    InTIPA            3
029800130309
029900110617      /free
030000130309
030100140207       // imposto settaggi base di sql
030200140207       exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
030300140207
030400140212       // gestione chiusura *pgm
030500140207       if InChiudi='S' or %shtdn;
030600140207          // eseguo chiusure a driver "aperti"
030700140211          exsr sr_Chiusura;
030800110627       else;
030900140210          // carico le tabelle necessarie (se non già caricate)
031000140212          if not wFlgTabelle;
031100140210             exsr sr_Cartab;
031200140210          endif;
031300141010
031400141010          // carico le filiali per rilasci pilotati
031500141010          exsr sr_VPOR;
031600140210
031700140207          // procedo con l'elaborazione
031800140207          exsr sr_Elabora;
031900140207
032000130309          eval *inrt = *on;
032100110627       endif;
032200130308
032300130308
032400110617       //-------------------------------------------------------------*
032500140214       //Lettura file "spia" alert FISIA00F                           *
032600110617       //-------------------------------------------------------------*
032700140207       Begsr sr_Elabora;
032800170323
032900170323         // Inizio la transazione di download
033000170329         //ALESInOpz = 'S';
033100170329         //callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
033200170329         //                    ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
033300170329         //                    ALESOutPRG:ALESOutEsito);
033400170329         //if %error OR ALESOutEsito <> *blanks;
033500170329         //   *in40 = *off;
033600170329         //   wMessaggio = 'KO da driver FNLSALESR';
033700170329         //else;
033800170329         //   sVGDPRG = ALESOutPRG;
033900170329         //endif;
034000170329
034100170329         sVGDPRG = 'XX00000000';
034200170329
034300140212
034400170329         // Verifico se richiesto filtro per particolare tipologia di alert
034500141009         select;
034600141009         when InChiudi = 'T';
034700140315               wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
034800140315                      ' where siaSTS = ''0'' ' +
034900140315                      ' and substr(siaFLO, 1, 1) = ''T'' ' +
035000150930                  //  ' and siaPTY < 050 ' +
035100140805                      ' order by siaPTY ' +
035200141020                  //    ' fetch first 200 rows only' +
035300140805                      ' for read only';
035400141009         when InChiudi = 'W';
035500141009               wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
035600141009                      ' where siaSTS = ''0'' ' +
035700141009                      ' and substr(siaFLO, 1, 1) = ''W'' ' +
035800150930                 //   ' and siaPTY < 050 ' +
035900141009                      ' order by siaPTY ' +
036000141020                 //   ' fetch first 200 rows only' +
036100141009                      ' for read only';
036200141009         when InChiudi = *blanks;
036300140315             if InTIPA <> *blanks;
036400140315                  wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
036500140315                         ' where siaSTS = ''0'' ' +
036600140315                         ' and siaTIPA=' + InTIPA +
036700150930                 //      ' and siaPTY < 050 ' +
036800140805                         ' order by siaPTY ' +
036900141020                 //      ' fetch first 200 rows only' +
037000140805                         ' for read only';
037100140315             else;
037200140315                  wSQL = 'select fisia00f.*, rrn(fisia00f) from fisia00f ' +
037300140315                         ' where siaSTS = ''0'' ' +
037400150930                 //      ' and siaPTY < 050 ' +
037500170213                 //      ' and siaTIPA<>''AGD''' +
037600140805                         ' order by siaPTY ' +
037700141020                 //      ' fetch first 200 rows only' +
037800140805                         ' for read only';
037900140315             endif;
038000141009         endsl;
038100140207
038200140207         // preparo il cursore del recordset da leggere
038300140207         exec sql  prepare s1 from :wSQL;
038400140207         exec sql  declare c1 cursor for s1;
038500140207         exec sql  open    c1;
038600140207
038700140210         // ciclo su tutto il recordset estratto
038800140212         exec sql fetch next from c1 into :fisia00f, :§siaRRN;
038900140207         dow sqlcod=0;
039000140207
039100140207           // controllo costantemente se richiesta chiusura del sottosistema
039200140207           if %shtdn;
039300140212              InChiudi = 'S';
039400140211              exsr sr_Chiusura;
039500140207              leavesr;
039600140207           endif;
039700140207
039800140207           // Se richiesta elaborazione parallela gestisco
039900140207           exsr sr_ElaPar;
040000140207
040100140207           // se OK => procedo
040200140207           if *in40;
040300140212              // inizializzazioni dati necessari per gli alerts
040400140212              exsr sr_InzDati;
040500140212
040600140508              // verifiche iniziali (tra cui se richiesto test)
040700140508              exsr sr_exeInit;
040800140214
040900140207              // effettuo controlli pre invio
041000140214              if not wFlgTest;
041100140214                 exsr sr_ChkSend;
041200140214              endif;
041300140207           endif;
041400140207
041500140207           // se OK => procedo
041600140207           if *in40;
041700140210              // reperisco i dati necessari
041800140210              exsr sr_RtvDati;
041900141010              exsr sr_ChkSend2;
042000140207           endif;
042100140207
042200140207           // verifico il tipo alert corrente
042300140207           if *in40;
042400140207              select;
042500140207                when siaTIPA = 'RIC' and wTipoInvio = 'M';
042600140212                     exsr sr_RIC_Email;
042700140207                when siaTIPA = 'RIC' and wTipoInvio = 'S';
042800140212                     exsr sr_RIC_SMS;
042900150930                when siaTIPA = 'RIC' and wTipoInvio = 'B';
043000150930                     exsr sr_RIC_Email;
043100150930                     exsr sr_RIC_SMS;
043200140207                when siaTIPA = 'FD'  and wTipoInvio = 'M';
043300140213                     exsr sr_FD_Email;
043400140207                when siaTIPA = 'FD'  and wTipoInvio = 'S';
043500140213                     exsr sr_FD_SMS;
043600140214                when siaTIPA = 'FD'  and wTipoInvio = 'B';
043700140214                     exsr sr_FD_Email;
043800140214                     exsr sr_FD_SMS;
043900150930                when (siaTIPA = 'CL1' or siaTIPA = 'CL2' or siaTIPA = 'CL3')
044000150930                      and wTipoInvio = 'M';
044100150930                     exsr sr_CL_Email;
044200150930                when (siaTIPA = 'CL1' or siaTIPA = 'CL2' or siaTIPA = 'CL3')
044300150930                      and wTipoInvio = 'S';
044400150930                     exsr sr_CL_SMS;
044500150930                when (siaTIPA = 'CL1' or siaTIPA = 'CL2' or siaTIPA = 'CL3')
044600150930                      and wTipoInvio = 'B';
044700150930                     exsr sr_CL_Email;
044800151013                     // exsr sr_CL_SMS;
044900170208                when siaTIPA = 'AGD' and wTipoInvio = 'M';
045000170209                     exsr sr_AGD_Email;
045100170208                when siaTIPA = 'AGD' and wTipoInvio = 'S';
045200170209                     exsr sr_AGD_SMS;
045300170208                when siaTIPA = 'AGD' and wTipoInvio = 'B';
045400170209                     exsr sr_AGD_Email;
045500170209                     exsr sr_AGD_SMS;
045600140207                other;
045700140214                     *in40 = *off;
045800140212                     wMessaggio = 'Valore SIATIPA non previsto: '+siaTIPA;
045900140212                     exsr snd_Alert;
046000140207              endsl;
046100140207           endif;
046200140207
046300140211
046400140211           // Verifico se record elaborato OK e aggiorno esito
046500140211           if *in40;
046600140218              wEsito = '1';
046700140217              select;
046800140217                when wTipoInvio = 'S';
046900140217                     wMessaggio = 'Alert inviato, SMS a: ' + %trim(wDestMobile);
047000140217                when wTipoInvio = 'M';
047100140217                     wMessaggio = 'Alert inviato, email a: '+ %trim(wDestEmail);
047200140217                when wTipoInvio = 'B';
047300140217                     wMessaggio = 'Alert inviato, SMS/email a: '+
047400140217                                  %trim(wDestMobile) + '/' + %trim(wDestEmail);
047500140217              endsl;
047600140214              if not wFlgTest;
047700140322                 select;
047800140322                   when wTipoInvio = 'S';
047900140322                        wDescRA = %trim(wDestMobile);
048000140322                        exsr sr_InsRA;
048100140322                   when wTipoInvio = 'M';
048200140322                        wDescRA = %trim(wDestEmail);
048300140322                        exsr sr_InsRA;
048400140322                   when wTipoInvio = 'B';
048500140324                        wDescRA = %trim(wDestMobile)+' e: '+
048600140324                                  %trim(wDestEmail);
048700140322                        exsr sr_InsRA;
048800140322                 endsl;
048900140214              endif;
049000140211           else;
049100140218              if *in41;
049200140218                 wEsito = '0';
049300140218              else;
049400140218                 wEsito = 'E';
049500140218              endif;
049600140211           endif;
049700141013           exsr sr_UpdFISIA;
049800140211
049900140212           exec sql fetch next from c1 into :fisia00f, :§siaRRN;
050000140207         enddo;
050100140207
050200140207         exec sql  close   c1;
050300140212
050400170323
050500170323         // Sancisco la transazione di download
050600170323         ALESInOpz = 'E';
050700170323         ALESInPRG = sVGDPRG;
050800170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
050900170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
051000170323                             ALESOutPRG:ALESOutEsito);
051100170323         if %error OR ALESOutEsito <> *blanks;
051200170323            *in40 = *off;
051300170323            wMessaggio = 'KO da driver FNLSALESR';
051400170323         endif;
051500130308
051600140207       endsr;
051700140212
051800140212
051900140212       //-------------------------------------------------------------*
052000140212       //Aggiornamento record di FISIA corrente                       *
052100140212       //-------------------------------------------------------------*
052200140212       Begsr sr_UpdFISIA;
052300141013
052400141013         if §SIAFLOTIP = 'T' or §SIAFLOTIP = 'W';
052500141013            wMessaggio = dsiamsg + %trim(wMessaggio);
052600141013         endif;
052700140212
052800140212         exec sql  update fisia00f set
052900140212                          siaSTS   = :wEsito,
053000140212                          siaMSG   = :wMessaggio,
053100140212                          siaDAORE = current_timestamp
053200140212                   where  rrn(fisia00f) = :§siaRRN AND
053300140213                          siaSTS = '0';
053400140212
053500140212       endsr;
053600150929
053700150929
053800140211
053900140211       //-------------------------------------------------------------*
054000140211       //inizializzazioni                                             *
054100140211       //-------------------------------------------------------------*
054200140212       Begsr sr_InzDati;
054300140212
054400140214          KNMUS = 'QPGMR';
054500140214
054600140212          clear dsiaRIC;
054700140214          clear dsiaFLO;
054800140214          clear dsiaMSG;
054900150930          clear dsiaICA;
055000170208          clear dsiaGIA;
055100140212          clear ds3A;
055200140401          clear og129;
055300141016          clear og130;
055400140212          clear xgiolavds;
055500140212          clear fnlv81ds;
055600140212          clear fnlv55ds;
055700140212          clear fnlrj6ds;
055800140212          clear fnlv82ds;
055900141009          clear trul28ds0;
056000140311          clear trulORsds;
056100140311          clear trulOR2ds;
056200150730          clear diore01;
056300140612          clear wTesto;
056400140725          clear wRSM_Eml;
056500140725          clear wRSM_Sms;
056600140212
056700140212          wSpedDataConsA = 'domani';
056800151001          wSpedDataConsD = *blanks;
056900140213          wSpedConsOrari = 'alla stessa ora';
057000150530          wSpedConsOrar2 = 'alla stessa ora';
057100140212
057200140212          reset ALESInChiudi;
057300140212          reset ALESInOpz;
057400140212          reset ALESInKSU;
057500140212          reset ALESInTIP;
057600140212          reset ALESInTSC;
057700140212          reset ALESInPGM;
057800170323          reset ALESInPRG;
057900140212          reset ALESInDS_SMS;
058000140212          reset ALESOutPRG;
058100140212          reset ALESOutEsito;
058200140212
058300141015          clear wEml;
058400141015          clear wCcEml;
058500141015          clear wOgg;
058600141015          clear wMsg;
058700141015          clear wSndSts;
058800140212
058900141015          clear wTipoInvio;
059000141015          clear wEsito;
059100141015          clear wMessaggio;
059200140212
059300141015          clear wRicTipo;
059400141015          clear wRicData;
059500141015          clear wRicOra;
059600170208          clear wGiaData;
059700141015          clear wLnaDescr;
059800141015          clear wLnaIndir;
059900141015          clear wLnaCAP;
060000141015          clear wLnaLocal;
060100141015          clear wLnaProv;
060200141015          clear wLnaTel;
060300141015          clear wLnaEmail;
060400141016          clear wLnaOraMMD_A;
060500141016          clear wLnaOraMMA_A;
060600141016          clear wLnaOraMPD_A;
060700141016          clear wLnaOraMPA_A;
060800141016          clear wLnaOraTMD_A;
060900141016          clear wLnaOraTMA_A;
061000141016          clear wLnaOraTPD_A;
061100141016          clear wLnaOraTPA_A;
061200141015          clear wMitIndir;
061300141015          clear wMitCAP;
061400141015          clear wMitLocal;
061500141015          clear wMitProv;
061600141015          clear wSpedDataA;
061700150530          clear wDataLimitFD;
061800141015          clear wBRTcode;
061900141015          clear wBRTcodeLong;
062000141015          clear wSpedNum;
062100141015          clear wSpedRmn;
062200141015          clear wSpedRma;
062300141015          clear wSpedRsd;
062400141015          clear wSpedInd;
062500170208          clear wSpedCAP;
062600141015          clear wSpedLocal;
062700141015          clear wSpedProv;
062800141015          clear wSpedColli;
062900141015          clear wSpedKg;
063000141015          clear wSpedCAS;
063100141015          clear wSpedConsOraDa;
063200141015          clear wSpedConsOraA;
063300141015          clear wDestEmail;
063400141015          clear wDestMobile;
063500141015          clear wDescRA;
063600140211
063700141015          clear wFlgNoOrari;
063800141015          clear wFlgTest;
063900141015          clear wFlgContrAss;
064000141015          clear wFlgSMSsent;
064100141015          clear wFlgFDAutMitt;
064200140211
064300140211       endsr;
064400140214
064500140214
064600140214       //-------------------------------------------------------------*
064700140214       //Verifica operazioni per elaborazione record di TEST          *
064800140214       //-------------------------------------------------------------*
064900140508       Begsr sr_exeInit;
065000140214
065100140214          wAAS = siaAAS;
065200140214          dsiaFLO = siaFLO;
065300140508
065400140508
065500141009          if §SIAFLOTIP = 'T' or §SIAFLOTIP = 'W';
065600140214             wFlgTest = *on;
065700141013             dsiaMSG = siaMSG;
065800140214             wAAS = §SIAMSGAAS;
065900140214             wDestEmail  = %trim(§SIAMSGEML);
066000140214             wDestMobile = %trim(§SIAMSGSMS);
066100140214             select;
066200140214               when wDestEmail <> *blanks AND wDestMobile <> *blanks;
066300140214                    wTipoInvio = 'B';
066400140214               when wDestEmail <> *blanks;
066500140214                    wTipoInvio = 'M';
066600140214               when wDestMobile <> *blanks;
066700140214                    wTipoInvio = 'S';
066800140214               other;
066900140214                    *in40 = *off;
067000140214                    wMessaggio = 'Valore SIATIPA non previsto: '+siaTIPA;
067100140214                    exsr snd_Alert;
067200140214             endsl;
067300140214          endif;
067400140214
067500140214       endsr;
067600140210
067700140210
067800140210       //-------------------------------------------------------------*
067900140210       //gestione operazioni di chiusura                              *
068000140210       //-------------------------------------------------------------*
068100140212       Begsr sr_Chiusura;
068200140210
068300140211          eval *inlr = *on;
068400140210
068500140210       endsr;
068600130401
068700130401
068800130401       //-------------------------------------------------------------*
068900130401       //gestione elaborazione parallela                              *
069000130401       //-------------------------------------------------------------*
069100140207       Begsr sr_ElaPar;
069200130401
069300130401           *in40 = *off;                  // record KO
069400140207           if InDivis > *zeros AND InDivid > *zeros;
069500140207              wDivid = InDivid - 1;
069600140207              if %rem(siaNSP:InDivis) = wDivid;
069700130401                 *in40 = *on;             // record OK
069800130401              endif;
069900130401           else;
070000130401              *in40 = *on;                // record OK
070100130401           endif;
070200130401
070300130401       endsr;
070400140207
070500140207
070600140207       //-------------------------------------------------------------*
070700140207       //Effettuo controlli pre invio                                 *
070800140207       //-------------------------------------------------------------*
070900140207       Begsr sr_ChkSend;
071000140207
071100140212           *in40 = *on;                   // record OK
071200140218           *in41 = *off;
071300140207
071400140212           // 1° reperimento email/mobile
071500140214           iLV81AAS = wAAS;
071600140212           iLV81LNP = siaLNP;
071700140212           iLV81NRS = siaNRS;
071800140212           iLV81NSP = siaNSP;
071900140322           iLV81TRC = 'E';
072000140214           callP(e) FNLV81R (fnlv81ds);
072100140212           if not %error AND oLV81ERR <> 'N';
072200140207              // verifico se richiesto invio email o SMS
072300140207              select;
072400140322                 when oLV81EML <> *blanks AND oLV81TEL <> *blanks;
072500140322                      wTipoInvio = 'B';
072600140322                      wDestEmail = oLV81EML;
072700140322                      wDestMobile = oLV81TEL;
072800140322                 when oLV81EML <> *blanks AND oLV81TEL  = *blanks;
072900140212                      wTipoInvio = 'M';
073000140212                      wDestEmail = oLV81EML;
073100140322                 when oLV81TEL <> *blanks AND oLV81EML  = *blanks;
073200140207                      wTipoInvio = 'S';
073300140212                      wDestMobile = oLV81TEL;
073400140212                 other;
073500140212                      *in40 = *off;
073600140212                      wMessaggio = 'Assenti sia email che mobile';
073700140207              endsl;
073800140212           else;
073900140212              *in40 = *off;
074000140212              wMessaggio = 'KO da driver FNLV81R';
074100140207           endif;
074200140212
074300140207
074400140207           // 2° alert RIC mai inviato
074500140212           if *in40 AND siaTIPA = 'RIC';
074600141104              iLV82AAS  = wAAS;
074700141104              iLV82LNP  = siaLNP;
074800141104              iLV82NRS  = siaNRS;
074900141104              iLV82NSP  = siaNSP;
075000141104              iLV82TIPA = siaTIPA;
075100140212              callP(e) FNLV82R (fnlv82ds);
075200140212              if not %error AND oLV82STS <> '1';
075300140212                 *in40 = *on;
075400140212              else;
075500140212                 *in40 = *off;
075600140212                 wMessaggio = 'KO da driver FNLV82R';
075700140212              endif;
075800140212           endif;
075900140212
076000140207
076100140207           // 3° se FFD presenza fisica in arrivo tutti i colli
076200140212           if *in40 AND siaTIPA = 'FD';
076300140211              iLRJ6TRIC = 'A';
076400140214              iLRJ6AAS  = wAAS;
076500140211              iLRJ6LNP  = siaLNP;
076600140211              iLRJ6NRS  = siaNRS;
076700140211              iLRJ6NSP  = siaNSP;
076800140211              callP(e) FNLRJ6R (fnlrj6ds);
076900140218              if %error OR oLRJ6ERR <> *blanks;
077000140218                 *in40 = *off;
077100140218                 wMessaggio = 'KO da driver FNLRJ6R';
077200140218              else;
077300140218                 if oLRJ6FALL='S';
077400140218                    *in40 = *on;
077500140218                 else;
077600140218                    *in40 = *off;
077700140218                    *in41 = *on;
077800140218                 endif;
077900140218              endif;
078000140211           endif;
078100140207
078200140207       endsr;
078300141010
078400141010
078500141010       //-------------------------------------------------------------*
078600141010       //Effettuo controlli pre invio 2                               *
078700141010       //-------------------------------------------------------------*
078800141014       Begsr sr_ChkSend2;
078900141010
079000141021           // 4° spedizione già consegnata
079100180208           // testa la  consegna anomala perchè per controalert devo
079200180208           //  inviare anche se ha la cons. anomala ( es dirottamento)
079300180208           // non lo deve fare per FD
079400141021           if *in40;
079500151012              if arbDCM > 0 AND arbCCA = *blanks;
079600141021                 *in40 = *off;
079700141021                 wMessaggio = 'Spedizione già consegnata';
079800141021              endif;
079900141021           endif;
080000180208
080100180208           if *in40 and siatipa='FD' and
080200180208                 arbDCM > 0 AND arbCCA <> *blanks;
080300180208                 *in40 = *off;
080400180208                 wMessaggio = 'Spedizione chiusa: non avviso per FD';
080500180208           endif;
080600141021
080700141021
080800170208           // 6° se filiale (LNA) corrente è da deplayare
080900141013           if *in40;
081000141013              select;
081100141015                when wTipoInvio='M' AND wVPOReml='VECCHIO';
081200141015                  *in40  = *off;
081300141015                  *in41  = *on;
081400141015
081500141015                when wTipoInvio='S' AND wVPORsms='VECCHIO';
081600141015                  *in40  = *off;
081700141015                  *in41  = *on;
081800141015
081900141015                when wTipoInvio='B' AND (wVPOReml<>'NUOVO' OR
082000141015                                         wVPORsms<>'NUOVO');
082100141015                  *in40  = *off;
082200141015                  *in41  = *on;
082300141015              endsl;
082400141015           endif;
082500141015
082600141015
082700141015           if *in40;
082800141015              select;
082900141015                when wTipoInvio='M' AND wVPOReml='MISTO';
083000141014                   if  %lookup(%editc(arbLNA:'X'):skFilOkEml) > 0;
083100141015                   // A modo "vecchio" asteriscare l' "else"
083200141013                   else;
083300141013                     *in40 = *off;
083400141013                     *in41 = *on;
083500141013                   endif;
083600141014
083700141015                when wTipoInvio='S' AND wVPORsms='MISTO';
083800141014                   if  %lookup(%editc(arbLNA:'X'):skFilOkSms) > 0;
083900141014                   // A modo "vecchio" asteriscare l' "else"
084000141014                   else;
084100141014                     *in40 = *off;
084200141014                     *in41 = *on;
084300141014                   endif;
084400141013              endsl;
084500141010           endif;
084600141107
084700141107
084800150929           // 7° spedizione in giacenza o con danno
084900180206           //  ES - ignoro se in giacenza: devo inviare alert per le giacenze
085000180111           //  ed è praticamente impossibile che un RIC vada via che la sped
085100180206           //  è già in giacenza
085200141107           if *in40;
085300141107             pInAnno      = arbAAS;
085400141107             pInLineaPar  = arbLNP;
085500141107             pInSerie     = arbNRS;
085600141107             pInNumSped   = arbNSP;
085700141107             pInLingua    = 'it';
085800141107             pInMaiuscolo = '1';
085900141107             callP(e) UBSPESTS (
086000141107                       pInAnno        :
086100141107                       pInLineaPar    :
086200141107                       pInSerie       :
086300141107                       pInNumSped     :
086400141107                       pInLingua      :
086500141107                       pInMaiuscolo   :
086600141107                       pOutLblTyp     :
086700141107                       pOutRblBO      :
086800141107                       pOutStsSped    :
086900141107                       pOutDtConsRich :
087000141107                       pOutDtApGiac   :
087100141107                       pOutDtLAvviso  :
087200141107                       pOutDescSts    :
087300141107                       pOutDtSts      :
087400141107                       pOutErrore     :
087500141107                       pOutCA         :
087600141107                       pOutDtPrGiac   :
087700141107                       pOutDtChGiac   );
087800180206                 if pOutErrore = *zeros AND pOutDescSts = 'IN GIACENZA'
087900180207                 and siatipa<>'AGD' and pOutDtChGiac=*zeros ;
088000180206                    *in40 = *off;
088100180206                    wMessaggio = 'Spedizione in giacenza';
088200180206                 endif;
088300180207
088400180207                 if pOutErrore = *zeros AND pOutDescSts = 'IN GIACENZA'
088500180207                 and siatipa= 'AGD' and pOutDtChGiac>*zeros ;
088600180207                    *in40 = *off;
088700180207                    wMessaggio = 'Spedizione non più in giacenza';
088800180207                 endif;
088900180111
089000141107              if pOutErrore = *zeros AND pOutCA = 'S';
089100141107                 *in40 = *off;
089200141107                 wMessaggio = 'Spedizione con pratica danno';
089300141107              endif;
089400141107           endif;
089500141010
089600150929
089700150929           // 8° spedizione "export"
089800150929           if *in40;
089900150929              if  %lookup(arbLNA:skFilExt) > 0;
090000150929                 *in40 = *off;
090100150929                 wMessaggio = 'Spedizione EXPORT: alert bloccato';
090200150929              endif;
090300150929           endif;
090400170209
090500170209
090600170209           // 9° alert LAVV ma spedizione "già" FFD
090700170209           if *in40;
090800170209              if siaTIPA = 'RIC' AND arbFFD = 'S';
090900170209                 *in40 = *off;
091000170209                 wMessaggio = 'Spedizione già in fermo deposito';
091100170209              endif;
091200170209           endif;
091300150929
091400141010       endsr;
091500140207
091600170208
091700140207
091800140207       //-------------------------------------------------------------*
091900140207       //Reperimento dati                                             *
092000140207       //-------------------------------------------------------------*
092100140207       Begsr sr_RtvDati;
092200140210
092300140211        // Reperisco i dati bolla in arrivo
092400140214        chain (wAAS:siaLNP:siaNRS:siaNSP) fnarb01l;
092500140210
092600140211        if %found(fnarb01l);
092700140725
092800140725           // --- EMAIL ---
092900170222           // Verifico se per cliente corrente presente forzatura
093000170222           // per Rag.Soc. invio alerts
093100170222           UBRSMALDS.iTpRic   = 'E';
093200170222           UBRSMALDS.iOrigine = 'A';
093300170222           UBRSMALDS.iAAS     = arbAAS;
093400170222           UBRSMALDS.iLNP     = arbLNP;
093500170222           UBRSMALDS.iNRS     = arbNRS;
093600170222           UBRSMALDS.iNSP     = arbNSP;
093700170222           UBRSMALR_S ( UBRSMALDS );
093800170222           if UBRSMALDS.oEsito = *zeros;
093900170222              wRSM_Eml = %trim(UBRSMALDS.oRagSoc);
094000170222           else;
094100170222              wRSM_Eml = %trim(arbRSM);
094200170222           endif;
094300140725
094400140725           // --- SMS ---
094500170222           // Verifico se per cliente corrente presente forzatura
094600170222           // per Rag.Soc. invio alerts
094700170222           UBRSMALDS.iTpRic   = 'S';
094800170222           UBRSMALDS.iOrigine = 'A';
094900170222           UBRSMALDS.iAAS     = arbAAS;
095000170222           UBRSMALDS.iLNP     = arbLNP;
095100170222           UBRSMALDS.iNRS     = arbNRS;
095200170222           UBRSMALDS.iNSP     = arbNSP;
095300170222           UBRSMALR_S ( UBRSMALDS );
095400170222           if UBRSMALDS.oEsito = *zeros;
095500170222              wRSM_Sms = %trim(UBRSMALDS.oRagSoc);
095600170222           else;
095700170222              wRSM_Sms = %trim(arbRSM);
095800170222           endif;
095900140210
096000170208           // Reperisco vari dati necessari per gli alerts
096100140210           select;
096200140210           when siaTIPA = 'RIC';
096300140212              dsiaRIC  = siaNOTE;
096400140213              wRicTipo = §SIACHI;
096500140218              if wRicTipo = 'S';
096600140218                 wFlgNoOrari = *on;
096700140218              endif;
096800140514              if §SIAFD = 'S';
096900140514                 wFlgFDAutMitt = *on;
097000140514              endif;
097100140213              wRicData = %trim(%editw(%dec(%date(§SIADEV):*eur):'  /  /    '));
097200140212              wRicOra  = %trim(%editw(§SIAHEV:'  :  '));
097300140210           endsl;
097400140210
097500140210           // Se la linea di arrivo è in £6 cerco la sua capofila
097600140210           clear wLNA;
097700140211           d55lin = arbLNA;
097800140210           d55tpt = '6';
097900140211           d55drf = arbAAS*10000+arbMGS;
098000140210           fnlv55r(fnlv55ds);
098100140210           if d55tfa <> *zeros;
098200140212              wLNA = d55tfa;
098300140210           else;
098400140211              wLNA = arbLNA;
098500140210           endif;
098600170208           wLNA_A = %editc(wLNA:'X');
098700140210
098800140210           // chaino file accessori che mi servono per il passaggio info
098900140210           // chaino contrassegno se presente
099000140212           ar9CAS = *zeros;
099100140211           if %lookup(arbCBO:wCbc) > 0;
099200140214              chain (wAAS:siaLNP:siaNRS:siaNSP) fiar901l;
099300140212              if %found(fiar901l);
099400140212                 wFlgContrAss = *on;
099500140212              endif;
099600140210           endif;
099700140210
099800140210           // chaino azorg della linea di arrivo
099900140210           chain (wLNA) azorg01l;
100000140210
100100140212           if %found(azorg01l) AND orgFVA = *blanks;
100200140311
100300140311              // reperisco gli orari di consegna previsti
100400140311              clear tnsd99ds;
100500140311              d98tbo='P';
100600140311              d98aas=arbaas;
100700140311              d98lnp=arblnp;
100800140311              d98nrs=arbnrs;
100900140311              d98nsp=arbnsp;
101000140526              //if arbtsp='D';
101100140526              //   I98TSP_FOR='C';
101200140526              //endif;
101300140311              tnsd99r(tnsd99ds);
101400140526              if d98dci>0;
101500140526                 IOREDTA = d98dci;
101600140311              endif;
101700150530
101800150601              if d98DEE > 0 AND (d98spcDEE = 'FFD' OR
101900150601                                 d98spcDEE = 'FD1' OR
102000150530                                 d98spcDEE = 'FD2');
102100150601                 wDataLimitFD = 'entro il ' +
102200150601                           %subst(%editc(d98DEE:'X'):7:2) + '/' +
102300150601                           %subst(%editc(d98DEE:'X'):5:2);
102400150601              else;
102500150604                 wDataLimitFD = *blanks;
102600150601              endif;
102700150601
102800140311
102900140311              IOREFIL  = arbLNA;
103000140311              IORECAP  = arbCAD;
103100140311              IORELOC  = arbLOD;
103200140311              IORENAR  = arbNZD;
103300140311              IORETSER = 'C';
103400140311              IOREDDC  = arbDDC;
103500140311              IORENDC  = arbNDC;
103600140311              IORETFP  = arbTFP;
103700140311              IORETFA  = arbTFA;
103800140311              IOREDTI  = arbDTI;
103900140526              //IOREHTI  = arbHTI;
104000140526              IOREHTI  = *zeros;
104100140311              IOREDCR  = arbDCR;
104200140311              IOREHCR  = arbHCR;
104300140311              IORETCR  = arbTCR;
104400140311              IOREDEI  = d98DEI;
104500140311              IOREOEI  = d98OEI;
104600140311              IORETSP  = arbTSP;
104700150730              §IORETRAZC = %editc(D98TTC:'X');
104800150730              §IORECONSC = %editc(D98TCC:'X');
104900150730              IOREFLO  = diore01;
105000140311
105100140311              callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
105200140311              if not %error AND OOREERR = *blanks;
105300140311                wSpedConsOraDa = %trim(%editw(OOR2STIS:'  :  '));
105400140311                wSpedConsOraA  = %trim(%editw(OOR2STFS:'  :  '));
105500150530                wSpedConsOrari = '('+ wSpedConsOraDa + '  ' +
105600150530                                      wSpedConsOraA + ')';
105700150530                wSpedConsOrar2 = 'dalle ' + wSpedConsOraDa +
105800150530                                 ' alle ' + wSpedConsOraA;
105900140311              endif;
106000140319
106100140319              // Reperisco la data consegna prevista corrente
106200140319              ixgldata = %dec(%date():*iso);
106300140319              ixglfil  = wLNA;
106400140319              ixglpa   = 'A';
106500140319              ixgladd  = 'S';
106600140319              ixglgg   = 1;
106700140319              ixgllav  = 'S';
106800140319              callP(e) XGIOLAV (xgiolavds);
106900140319              if not %error AND
107000140319                 oxglerr = *blanks and oxgldata > *zeros;
107100140319                  if oxgldata <> %dec(%date()+%days(1):*iso);
107200150530                     wSpedDataConsA ='il '+%subst(%editc(oxgldata:'X'):7:2)+'/'+
107300150530                                           %subst(%editc(oxgldata:'X'):5:2)+'/'+
107400150530                                           %subst(%editc(oxgldata:'X'):1:4);
107500140319                 endif;
107600151001                 wSpedDataConsD ='il '+%subst(%editc(oxgldata:'X'):7:2)+'/'+
107700151001                                       %subst(%editc(oxgldata:'X'):5:2)+'/'+
107800151001                                       %subst(%editc(oxgldata:'X'):1:4);
107900140319              endif;
108000140312
108100140312              // verifico se data-ora prossima consegna indicativa è
108200140317              // inferiore alla data-ora corrente
108300140319              wDataOraNext = (oxgldata*10000+OOR2STFS)*100;
108400140319              wDataOraCorr = %dec(%date():*iso)*1000000+%dec(%time():*iso);
108500140319              if wDataOraNext <= wDataOraCorr;
108600140319                 *in40 = *off;
108700140319                 wMessaggio = 'Data/ora prossima consegna indicativa ' +
108800140417                              'inferiore a data/ora corrente';
108900140319              else;
109000140212
109100140212              wLnaDescr   = orgDES;
109200140210              wLnaIndir   = orgIND;
109300140212              wLnaCAP     = %editc(orgCPF:'X');
109400140210              wLnaLocal   = orgLOC;
109500140210              wLnaProv    = orgPRO;
109600140210              wLnaTel     = orgTEL;
109700140210              wLnaEmail   = orgDD2;
109800141016
109900140401              og129       = orgDC9;
110000140401              if §OGORFMD <> *zeros AND §OGORFMD <> *blanks;
110100141016                 wLnaOraMMD_A = %subst(§OGORFMD:1:2)+':'+%subst(§OGORFMD:3:2);
110200140213              endif;
110300140401              if §OGORFMA <> *zeros AND §OGORFMA <> *blanks;
110400141016                 wLnaOraMMA_A = %subst(§OGORFMA:1:2)+':'+%subst(§OGORFMA:3:2);
110500140213              endif;
110600140401              if §OGORFPD <> *zeros AND §OGORFPD <> *blanks;
110700141016                 wLnaOraMPD_A = %subst(§OGORFPD:1:2)+':'+%subst(§OGORFPD:3:2);
110800140213              endif;
110900140401              if §OGORFPA <> *zeros AND §OGORFPA <> *blanks;
111000141016                 wLnaOraMPA_A = %subst(§OGORFPA:1:2)+':'+%subst(§OGORFPA:3:2);
111100140213              endif;
111200141016
111300141016              og130       = orgDD0;
111400141016              if §OGORAMD <> *zeros AND §OGORAMD <> *blanks;
111500141016                 wLnaOraTMD_A = %subst(§OGORAMD:1:2)+':'+%subst(§OGORAMD:3:2);
111600141016              endif;
111700141016              if §OGORAMA <> *zeros AND §OGORAMA <> *blanks;
111800141016                 wLnaOraTMA_A = %subst(§OGORAMA:1:2)+':'+%subst(§OGORAMA:3:2);
111900141016              endif;
112000141016              if §OGORAPD <> *zeros AND §OGORAPD <> *blanks;
112100141016                 wLnaOraTPD_A = %subst(§OGORAPD:1:2)+':'+%subst(§OGORAPD:3:2);
112200141016              endif;
112300141016              if §OGORAPA <> *zeros AND §OGORAPA <> *blanks;
112400141016                 wLnaOraTPA_A = %subst(§OGORAPA:1:2)+':'+%subst(§OGORAPA:3:2);
112500141016              endif;
112600140210
112700140212              wMitIndir  = arbINM;
112800140212              wMitCAP    = arbCAM;
112900140211              wMitLocal  = arbLOM;
113000140211              wMitProv   = arbPRM;
113100140213              wSpedDataA = %trim(%editw(%dec(%date(arbAAS*10000+arbMGS):*eur)
113200140211                           :'  /  /    '));
113300141009
113400141009              // Calcolo il BRTcode (tramite apposito driver)
113500141009              clear BRTcode;
113600141009              BRTcode.aas4    = arbAAS;
113700141009              BRTcode.lnp     = arbLNP;
113800141009              BRTcode.nrs     = arbNRS;
113900141009              BRTcode.nsp     = arbNSP;
114000141009
114100141009              trul28ds0.i280COD = BRTcode.BRTcode;
114200141009              callP(e) TRUL28R0 (trul28ds0);
114300141009
114400141009              // Se BRTcode errato => esito KO
114500141009              if %error OR trul28ds0.o280err = '1';
114600141009              else;
114700141014                 wBRTcode     = %subst(trul28ds0.o280cod:1:5) +
114800141014                                %subst(trul28ds0.o280cod:6:5) +
114900141014                                %subst(trul28ds0.o280cod:11:5)+
115000141014                                %subst(trul28ds0.o280cod:16:4);
115100141014                 wBRTcodeLong = %subst(trul28ds0.o280cod:1:5) +'  '+
115200141014                                %subst(trul28ds0.o280cod:6:5) +'  '+
115300141014                                %subst(trul28ds0.o280cod:11:5)+'  '+
115400141014                                %subst(trul28ds0.o280cod:16:4);
115500141009              endif;
115600141009
115700141009
115800141009              wSpedNum   = %trim(%editc(arbLNP:'X')) +
115900141009                           %trim(%editc(arbNRS:'X')) +
116000141009                           %trim(%editc(arbNSP:'X'));
116100140211              wSpedRmn   = %trim(%editc(arbRMN:'Z'));
116200140211              wSpedRma   = %trim(arbRMA);
116300140211              wSpedRsd   = %trim(arbRSD);
116400140211              wSpedInd   = %trim(arbIND);
116500170208              wSpedCAP   = %trim(arbCAD);
116600140211              wSpedLocal = %trim(arbLOD);
116700140211              wSpedProv  = %trim(arbPRD);
116800140211              wSpedColli = %trim(%editc(arbNCL:'Z'));
116900140211              wSpedKg    = %trim(%editc(arbPKB:'2'));
117000141009              wSpedCAS   = %trim(%editc(ar9CAS:'1'))+'  '+ar9VCA;
117100140210
117200140210              // Stacco subito un progressivo per la email corrente
117300140212              exsr sr_rtvPrg;
117400140312
117500140312              endif;
117600140210
117700140212           else;
117800140212              *in40 = *off;
117900140212              wMessaggio = 'AZORG per ' + %editc(wLNA:'X') + ' non trovato';
118000140212           endif;
118100140923
118200140923
118300140923           // Per Fermi Deposito se spedizione già in distinta ...
118400140923           // ... aspetto a mandare l'alert
118500180206           // solo per distinta di aut reale
118600140923           if siaTIPA = 'FD' and arbDCM=*zeros and
118700140923                                 arbDDC>*zeros and
118800140923                                 arbNDC>*zeros;
118900180207           // chain ('A':arbpdc) fiapd01l ;
119000180207           //  if %found(fiapd01l) and apdfcm='S'  ;
119100180207
119200180207           // non è possibile fidarsi della distinta fittizia,
119300180207           // perchè le filiali fanno pasticci sistemando cose "strane"
119400180207           // e mettendole in Fd, per cui
119500180207           //  devo per forza chiodare ksc 1020002 serie 01
119600180207
119700180207           if arbksc=1020002 and arbnrs=01   ;
119800180206           else  ;
119900140923              *in40 = *off;
120000140923              *in41 = *on;
120100140923           endif;
120200180206           endif;
120300140210
120400140212        else;
120500140212           *in40 = *off;
120600140212           wMessaggio = 'FNARB non trovato';
120700140212        endif;
120800140210
120900140207       endsr;
121000130321
121100140210
121200140210       //-------------------------------------------------------------*
121300140210       //caricamento codici bolla con c/a                              *
121400140210       //-------------------------------------------------------------*
121500140210       Begsr sr_Cartab;
121600140210
121700140212          wFlgTabelle = *on;
121800140212
121900140210          // Leggo tabella "3A" per caricare schiera dei codici bolla
122000140210          // con contrassegno
122100140210          wCbcIdx=0;
122200140210          clear wCbc;
122300140210          setll (1:'3A') tabel00f;
122400140210          reade (1:'3A') tabel00f;
122500140210
122600140210          dow not %eof(tabel00f);
122700140210             ds3a=tbluni;
122800140210             if §3afca>0;
122900140212                wCbcIdx=wCbcIdx+1;
123000140210                wCbc(wCbcIdx)=tblkey;
123100140210             endif;
123200140210             reade (1:'3A') tabel00f;
123300140210          enddo;
123400140210
123500150929
123600150929          // Carico la lista di filiali "estere" (secondo il network)
123700150929          clear skFilExt;
123800150929          clear ixFilExt;
123900150929          setll (*zeros) azorg01l;
124000150929          read azorg01l;
124100150929
124200150929          dow not %eof(azorg01l);
124300150929             if ORGFVA = *blanks;
124400150929                og143 = orgDE3;
124500150929                chain ('NTW':§OGNTW) tntbe01l;
124600150929                if %found(tntbe01l);
124700150929                   dntw = tbeuni;
124800150929                   if §NTWFIE = 'E';
124900150929                      ixFilExt = ixFilExt + 1;
125000150929                      skFilExt(ixFilExt) = orgFIL;
125100150929                   endif;
125200150929                endif;
125300150929             endif;
125400150929             read azorg01l;
125500150929          enddo;
125600150929
125700150929       endsr;
125800141010
125900170208
126000141010
126100141010       //-------------------------------------------------------------*
126200141010       //caricamento filiali abilitate al deploy                      *
126300141010       //-------------------------------------------------------------*
126400141010       Begsr sr_VPOR;
126500141010
126600141015          clear wVPOReml;
126700141015          clear wVPORsms;
126800141010
126900141014          // richiamo il Pgm per reperire filiali abilitate al nuovo alert email
127000141010          clear TRULVPODS;
127100170208 xxx      IVPOke1 = 'DECOFINEWEM';
127200141010          TRULVPOR (trulvpods);
127300141014          skFilOkEml = skFilOk;
127400141010
127500141015          select;
127600141015            // se al primo elemento c'è 999 => tutto a modo nuovo
127700141015            when skFilOkEml(1) = '999';
127800141015                 wVPOReml = 'NUOVO';
127900141015
128000141015            // se al primo elemento c'è *blanks => tutto a modo vecchio
128100141015            when skFilOkEml(1) = *blanks;
128200141015                 wVPOReml = 'VECCHIO';
128300141015
128400141015            other;
128500141015                 wVPOReml = 'MISTO';
128600141015          endsl;
128700141014
128800141014          // richiamo il Pgm per reperire filiali abilitate al nuovo alert sms
128900141014          clear TRULVPODS;
129000141014          IVPOke1 = 'DECOFINEWSM';
129100141014          TRULVPOR (trulvpods);
129200141014          skFilOkSms = skFilOk;
129300141015
129400141015          select;
129500141015            // se al primo elemento c'è 999 => tutto a modo nuovo
129600141015            when skFilOkSms(1) = '999';
129700141015                 wVPORsms = 'NUOVO';
129800141015
129900141015            // se al primo elemento c'è *blanks => tutto a modo vecchio
130000141015            when skFilOkSms(1) = *blanks;
130100141015                 wVPORsms = 'VECCHIO';
130200141015
130300141015            other;
130400141015                 wVPORsms = 'MISTO';
130500141015          endsl;
130600141014
130700141010       endsr;
130800140212
130900140212
131000140212       //-------------------------------------------------------------*
131100140212       //Gestione invio tipo alert RIC via EMAIL                      *
131200140212       //-------------------------------------------------------------*
131300140212       Begsr sr_RIC_Email;
131400140212
131500140212         clear indmrabyps;
131600140212
131700141009         w§CM1VAR = '*OBJM*BRT - Avviso di tentata consegna ' +
131800140213                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
131900170208         chain ('MRA':'TRBMBYPSR':'ALE_RIC') tntbe01l;
132000140212
132100140212          if %found(tntbe01l);
132200140212           indmrabyps=tbeuni;
132300140212           exsr sr_trtcm1ds;
132400140212
132500140212           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
132600140725           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
132700140311           strTRBM=strTRBM+'§§Tag03§§=='+%trim(wMitIndir) +'||';
132800140212           strTRBM=strTRBM+'§§Tag04§§=='+%trim(wMitCAP)   +'||';
132900140212           strTRBM=strTRBM+'§§Tag05§§=='+%trim(wMitLocal) +'||';
133000140212           strTRBM=strTRBM+'§§Tag06§§=='+%trim(wMitProv)  +'||';
133100141009           strTRBM=strTRBM+'§§Tag08§§=='+
133200161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
133300141009           strTRBM=strTRBM+wBRTcode;
133400140212           strTRBM=strTRBM+'&urltype=a'+'||';
133500140212           strTRBM=strTRBM+'§§Tag10§§=='+%trim(wSpedNum)  +'||';
133600140212           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
133700140212           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
133800140212           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
133900140212           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
134000140212           if wFlgContrAss;
134100140212              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
134200140212           else;
134300141009              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
134400140212           endif;
134500140212           strTRBM=strTRBM+'§§Tag16§§=='+%trim(wLnaDescr)+'||';
134600140212           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wLnaIndir)+'||';
134700140212           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wLnaCAP)  +'||';
134800140212           strTRBM=strTRBM+'§§Tag19§§=='+%trim(wLnaLocal)+'||';
134900140213           strTRBM=strTRBM+'§§Tag20§§=='+'('+%trim(wLnaProv)+')'+'||';
135000140212           strTRBM=strTRBM+'§§Tag21§§=='+%trim(wLnaTel)  +'||';
135100140212           strTRBM=strTRBM+'§§Tag22§§=='+%trim(wLnaEmail)+'@brt.it'+'||';
135200140212           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
135300140212           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedLocal) +'  '
135400140212                                        +%trim(wSpedProv)+'||';
135500140212           strTRBM=strTRBM+'§§Tag30§§=='+%trim(wRicData)+'||';
135600140218           if wFlgNoOrari;
135700140218              strTRBM=strTRBM+'§§Tag31§§=='+''+'||';
135800140218           else;
135900141010              strTRBM=strTRBM+'§§Tag31§§=='+'alle '+%trim(wRicOra) +'||';
136000140218           endif;
136100140212           strTRBM=strTRBM+'§§Tag32§§=='+%trim(wSpedDataConsA)+'||';
136200150530           strTRBM=strTRBM+'§§Tag33§§=='+%trim(wSpedConsOrar2)+'||';
136300140213           strTRBM=strTRBM+'§§Tag34§§=='+%trim(wSpedDataA) +'||';
136400140212
136500141016           strTRBM=strTRBM+'§§Tag36§§=='+%trim(wLnaOraTMD_A)+'||';
136600141016           strTRBM=strTRBM+'§§Tag37§§=='+%trim(wLnaOraTMA_A) +'||';
136700141016           if wLnaOraTPD_A <> *blanks;
136800141016              strTRBM=strTRBM+'§§Tag38§§=='+' e  ' + %trim(wLnaOraTPD_A)+
136900141016                                            ' - ' + %trim(wLnaOraTPA_A)+'||';
137000141009           else;
137100141009              strTRBM=strTRBM+'§§Tag38§§=='+''+'||';
137200141009           endif;
137300141009
137400140507
137500140509           if not wFlgFDAutMitt;
137600140508              strTRBM=strTRBM+'§§Tag39§§=='+''+'||';
137700140507           else;
137800140509              strTRBM=strTRBM+'§§Tag39§§=='+
137900140509                 '  -  Per disposizione del mittente'+
138000140507                 ' lo svincolo della merce presso i nostri magazzini sarà' +
138100140507                 ' possibile al ricevimento di specifica autorizzazione' +
138200140507                 '||';
138300140507           endif;
138400140507
138500141014           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
138600140212
138700140212           if Bartmail_insert(strTRBM:indmrabyps:
138800140212                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
138900140612                wTesto =
139000140612                   'Un nuovo tentativo di consegna verrà effettuato ' +
139100140612                   %trim(wSpedDataConsA) + ' indicativamente ' +
139200150530                   %trim(wSpedConsOrar2);
139300140612
139400140212           else;
139500140212              *in40 = *off;
139600140212              wMessaggio = 'Errore in push-entry in BartMail';
139700140212           endif;
139800140212
139900140212          else;
140000140212            *in40 = *off;
140100170208            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_RIC non trovata';
140200140212          endif;
140300140212
140400140212       endsr;
140500140213
140600140213
140700140213       //-------------------------------------------------------------*
140800140213       //Gestione invio tipo alert FD  via EMAIL                      *
140900140213       //-------------------------------------------------------------*
141000141016       Begsr sr_FD_Email;
141100140213
141200140213         clear indmrabyps;
141300140213
141400140311         w§CM1VAR = '*OBJM*BRT - Avviso disponibilità merce a magazzino ' +
141500140213                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
141600170208         chain ('MRA':'TRBMBYPSR':'ALE_FD') tntbe01l;
141700140213
141800140213          if %found(tntbe01l);
141900140213           indmrabyps=tbeuni;
142000140213           exsr sr_trtcm1ds;
142100140213
142200140213           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
142300140725           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
142400140213           strTRBM=strTRBM+'§§Tag03§§=='+%trim(wMitIndir) +'||';
142500140213           strTRBM=strTRBM+'§§Tag04§§=='+%trim(wMitCAP)   +'||';
142600140213           strTRBM=strTRBM+'§§Tag05§§=='+%trim(wMitLocal) +'||';
142700140213           strTRBM=strTRBM+'§§Tag06§§=='+%trim(wMitProv)  +'||';
142800141016           strTRBM=strTRBM+'§§Tag08§§=='+
142900161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
143000141016           strTRBM=strTRBM+wBRTcode;
143100141016           strTRBM=strTRBM+'&urltype=a'+'||';
143200140213           strTRBM=strTRBM+'§§Tag10§§=='+%trim(wSpedNum)  +'||';
143300140213           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
143400140213           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
143500140213           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
143600140213           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
143700140213           if wFlgContrAss;
143800140213              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
143900140213              strTRBM=strTRBM+'§§Tag40§§=='+
144000140213                      'Le ricordiamo che all''atto del ritiro è '  +
144100141117                      'previsto il pagamento del <b> contrassegno di ' +
144200141117                      %trim(wSpedCAS)+'</b>.'+'||';
144300140213           else;
144400141009              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
144500140213              strTRBM=strTRBM+'§§Tag40§§=='+''+'||';
144600140213           endif;
144700140213           strTRBM=strTRBM+'§§Tag16§§=='+%trim(wLnaDescr)+'||';
144800140213           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wLnaIndir)+'||';
144900140213           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wLnaCAP)  +'||';
145000140213           strTRBM=strTRBM+'§§Tag19§§=='+%trim(wLnaLocal)+'||';
145100140213           strTRBM=strTRBM+'§§Tag20§§=='+'('+%trim(wLnaProv)+')'+'||';
145200140213           strTRBM=strTRBM+'§§Tag21§§=='+%trim(wLnaTel)  +'||';
145300140213           strTRBM=strTRBM+'§§Tag22§§=='+%trim(wLnaEmail)+'@brt.it'+'||';
145400170719           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
145500170719           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedCAP)+' '+
145600170719                 %trim(wSpedLocal)+'  '+%trim(wSpedProv)+'||';
145700140213           strTRBM=strTRBM+'§§Tag34§§=='+%trim(wSpedDataA) +'||';
145800141016           strTRBM=strTRBM+'§§Tag36§§=='+%trim(wLnaOraTMD_A)+'||';
145900141016           strTRBM=strTRBM+'§§Tag37§§=='+%trim(wLnaOraTMA_A) +'||';
146000141016           if wLnaOraTPD_A <> *blanks;
146100141016              strTRBM=strTRBM+'§§Tag38§§=='+' e  ' + %trim(wLnaOraTPD_A)+
146200141016                                            ' - '  + %trim(wLnaOraTPA_A)+'||';
146300140213           else;
146400140213              strTRBM=strTRBM+'§§Tag38§§=='+''+'||';
146500140213           endif;
146600141016
146700141016           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
146800141016
146900141016           strTRBM=strTRBM+'§§Tag42§§=='+%trim(wLnaOraMMD_A)+'||';
147000141016           strTRBM=strTRBM+'§§Tag43§§=='+%trim(wLnaOraMMA_A) +'||';
147100141016           if wLnaOraMPD_A <> *blanks;
147200141016              strTRBM=strTRBM+'§§Tag44§§=='+' e  ' + %trim(wLnaOraMPD_A)+
147300141016                                            ' - '  + %trim(wLnaOraMPA_A)+'||';
147400141016           else;
147500141016              strTRBM=strTRBM+'§§Tag44§§=='+''+'||';
147600141016           endif;
147700150530
147800150601           if wDataLimitFD <> *blanks;
147900150601              strTRBM=strTRBM+'§§Tag45§§=='+%trim(wDataLimitFD)+'||';
148000150601           else;
148100150601              strTRBM=strTRBM+'§§Tag45§§=='+''+'||';
148200150601           endif;
148300140507
148400140213
148500140213           if Bartmail_insert(strTRBM:indmrabyps:
148600140213                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
148700140612              wTesto =
148800140612                  'La informiamo che la spedizione sotto indicata a Lei ' +
148900140612                  'diretta, per la quale il mittente ha disposto il fermo ' +
149000140612                  'deposito presso i ns magazzini, è disponibile per il ' +
149100140612                  'ritiro.';
149200140612
149300140213           else;
149400140213              *in40 = *off;
149500140213              wMessaggio = 'Errore in push-entry in BartMail';
149600140213           endif;
149700140213
149800140213          else;
149900140213            *in40 = *off;
150000170208            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_FD non trovata';
150100140213          endif;
150200140213
150300140213       endsr;
150400170208
150500170208
150600170208
150700170208       //-------------------------------------------------------------*
150800170208       //Gestione invio tipo alert AGD via EMAIL                      *
150900170208       //-------------------------------------------------------------*
151000170208       Begsr sr_AGD_Email;
151100170208
151200170208         clear indmrabyps;
151300170208
151400170208         dsiaGIA = siaNOTE;
151500170214
151600170214         // Controllo dati della giacenza (obbligatori)
151700170214         if §SIADAP   > *zeros  AND
151800170214            §SIACMC  <> *blanks AND
151900170214            §SIACMCD <> *blanks;
152000170214
152100170208         wGiaData = %trim(%editw(%dec(%date(§SIADAP):*eur):'  /  /    '));
152200170208
152300170208         chain ('UFI':wLNA_A) tntbe01l;
152400170208         if %found(tntbe01l);
152500170208            wLinkFil = %trim(tbeuni);
152600170208         else;
152700170208            clear wLinkFil;
152800170208         endif;
152900170208
153000170208         w§CM1VAR = '*OBJM*BRT - Avviso di MANCATA CONSEGNA ' +
153100170208                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
153200170208         chain ('MRA':'TRBMBYPSR':'ALE_AGD') tntbe01l;
153300170208
153400170208          if %found(tntbe01l);
153500170208           indmrabyps=tbeuni;
153600170208           exsr sr_trtcm1ds;
153700170208
153800170208           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
153900170208           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
154000170208
154100170208           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
154200170208           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
154300170208           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
154400170208           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
154500170208           if wFlgContrAss;
154600170208              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
154700170208           else;
154800170208              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
154900170208           endif;
155000170209
155100170209           strTRBM=strTRBM+'§§Tag21§§=='+%trim(wLnaTel)  +'||';
155200170209           strTRBM=strTRBM+'§§Tag22§§=='+%trim(wLnaEmail)+'@brt.it'+'||';
155300170208
155400170208           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
155500170208           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedCAP)+' '+
155600170208                 %trim(wSpedLocal)+'  '+%trim(wSpedProv)+'||';
155700170208           strTRBM=strTRBM+'§§Tag30§§=='+%trim(wGiaData)+'||';
155800170208
155900170208           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
156000170208
156100170208           strTRBM=strTRBM+'§§Tag50§§=='+'<b>'+%trim(§SIACMCD)+'</b>'+'||';
156200170208           strTRBM=strTRBM+'§§Tag51§§=='+
156300170208               ' <a href="'+%trim(wLinkFil)+'">'+%trim(wLnaDescr)+'</a>'+'||';
156400170208           strTRBM=strTRBM+'§§Tag52§§=='+
156500170208                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
156600170208           strTRBM=strTRBM+wBRTcode;
156700170208           strTRBM=strTRBM+'&urltype=a'+'||';
156800170208
156900170208           if Bartmail_insert(strTRBM:indmrabyps:
157000170208                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
157100170208                wTesto =
157200170208                   'La merce è in giacenza presso la filiale di ' +
157300170208                   %trim(wLnaDescr) + ' in attesa di disposizioni.';
157400170208
157500170208           else;
157600170208              *in40 = *off;
157700170208              wMessaggio = 'Errore in push-entry in BartMail';
157800170208           endif;
157900170208
158000170208          else;
158100170208            *in40 = *off;
158200170208            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_AGD non trovata';
158300170208          endif;
158400170214
158500170214         else;
158600170214           *in40 = *off;
158700170214           wMessaggio = 'Errore dati in DSIAGIA';
158800170214
158900170214         endif;
159000170208
159100170208       endsr;
159200170208
159300140212
159400140212
159500140212       //-------------------------------------------------------------*
159600140212       //Gestione invio tipo alert RIC via SMS                        *
159700140212       //-------------------------------------------------------------*
159800140212       Begsr sr_RIC_SMS;
159900140212
160000140212         // aggiungo SMS da inviare
160100140311         if wFlgNoOrari;
160200141009            wOggiSMS = 'Oggi';
160300140311         else;
160400141009            wOggiSMS = 'Ore ' + %trim(wRicOra);
160500140311         endif;
160600140213
160700140213         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
160800140213         ALESInDS_SMS.i_message =
160900150530                %trim(wOggiSMS) + ' tentata consegna spedizione di '           +
161000150530                %trim(wRSM_Sms) + '. '                                         +
161100150603                'Ripasseremo ' + %trim(%subst(wSpedDataConsA:1:8)) + ' '       +
161200150530                %trim(wSpedConsOrari) + '. '                                   +
161300170508                'Puoi modificare la ns programmazione su www.brt.it '          +
161400150530                ' BRTcode ' + wBRTcode;
161500140612         wTesto = ALESInDS_SMS.i_message;
161600140212
161700140213         ALESInDS_SMS.i_IdSMS   =
161800140213                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
161900140214                     %editc(wAAS:'X')+%editc(siaLNP:'X')     +
162000140213                     %editc(siaNRS:'X')+%editc(siaNSP:'X')     + '_' + 'ARIC';
162100140213
162200140213         ALESInDS_SMS.i_cell    = wDestMobile;
162300151002         if not wFlgTest;
162400150930  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
162500140214         endif;
162600140212
162700140212
162800140213         ALESInOpz = 'W';
162900170323         ALESInPRG = sVGDPRG;
163000140212         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
163100170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
163200140731                             ALESOutPRG:ALESOutEsito);
163300140212         if %error OR ALESOutEsito <> *blanks;
163400140212            *in40 = *off;
163500140212            wMessaggio = 'KO da driver FNLSALESR';
163600140213         else;
163700140213            wFlgSMSsent = *on;
163800140212         endif;
163900140212
164000140212       endsr;
164100140213
164200140213
164300140213       //-------------------------------------------------------------*
164400140213       //Gestione invio tipo alert FD  via SMS                        *
164500140213       //-------------------------------------------------------------*
164600140213       Begsr sr_FD_SMS;
164700140213
164800140213         // aggiungo SMS da inviare
164900140213
165000140213         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
165100140213         ALESInDS_SMS.i_message =
165200170508         'La tua spedizione da ' + %trim(wRSM_Sms) + ' e'+api+ ' disponibile'  +
165300150604                ' per il ritiro ' + %trim(%subst(wDataLimitFD:1:14))           +
165400170508                ' presso la filiale di '                                       +
165500150530                %trim(wLnaLocal) + ' (' + %trim(wLnaProv) + ') in '            +
165600150604                %trim(wLnaIndir) + ' - Tracking su www.brt.it'                 +
165700150530                '  BRTcode ' + %trim(wBRTcode);
165800140612         wTesto = ALESInDS_SMS.i_message;
165900140311
166000140213         ALESInDS_SMS.i_IdSMS   =
166100140213                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
166200140214                     %editc(wAAS:'X')+%editc(siaLNP:'X')     +
166300140530                     %editc(siaNRS:'X')+%editc(siaNSP:'X')     + '_' + 'AFFD';
166400140213
166500140213         ALESInDS_SMS.i_cell    = wDestMobile;
166600151002         if not wFlgTest;
166700140217  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
166800140214         endif;
166900140213
167000140213
167100140213         ALESInOpz = 'W';
167200170323         ALESInPRG = sVGDPRG;
167300170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
167400170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
167500170323                             ALESOutPRG:ALESOutEsito);
167600140213         if %error OR ALESOutEsito <> *blanks;
167700140213            *in40 = *off;
167800140213            wMessaggio = 'KO da driver FNLSALESR';
167900140213         else;
168000140213            wFlgSMSsent = *on;
168100140213         endif;
168200140213
168300140213       endsr;
168400150930
168500150930
168600150930       //-------------------------------------------------------------*
168700150930       //Gestione invio tipo alert CL1 / CL2 / CL3 via SMS            *
168800150930       //-------------------------------------------------------------*
168900150930       Begsr sr_CL_SMS;
169000150930
169100150930         // aggiungo SMS da inviare
169200150930
169300150930         dsiaICA = siaNOTE;
169400150930         wSpedConsOraDa = %trim(%editw(§SIANOPCD:'  :  '));
169500150930         wSpedConsOraA  = %trim(%editw(§SIANOPCA:'  :  '));
169600150930         wSpedConsOrari = '('+ wSpedConsOraDa + '  ' +
169700150930                               wSpedConsOraA + ')';
169800150930
169900150930         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
170000150930         ALESInDS_SMS.i_message =
170100150930                'Per cause operative, la consegna da ' +
170200150930                %trim(wRSM_Sms) + ' e'' stata riprogrammata al ' +
170300150930                %subst(%editc(§SIANDPC:'X'):7:2) + '/' +
170400150930                %subst(%editc(§SIANDPC:'X'):5:2) + ' ' +
170500170508                %trim(wSpedConsOrari)+'. Puoi modificare la nostra ' +
170600170508                ' programmazione su www.brt.it' +
170700150930                ' BRTcode ' + wBRTcode;
170800150930         wTesto = ALESInDS_SMS.i_message;
170900150930
171000150930         ALESInDS_SMS.i_IdSMS   =
171100150930                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
171200150930                     %editc(wAAS:'X')+%editc(siaLNP:'X') +
171300150930                     %editc(siaNRS:'X')+%editc(siaNSP:'X') + '_' +
171400150930                     'A' + %trim(siaTIPA);
171500150930
171600150930         ALESInDS_SMS.i_cell    = wDestMobile;
171700151002         if not wFlgTest;
171800150930  xxx     //  ALESInDS_SMS.i_cell    = '+393473441969';
171900150930         endif;
172000150930
172100150930
172200150930         ALESInOpz = 'W';
172300170323         ALESInPRG = sVGDPRG;
172400170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
172500170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
172600170323                             ALESOutPRG:ALESOutEsito);
172700150930         if %error OR ALESOutEsito <> *blanks;
172800150930            *in40 = *off;
172900150930            wMessaggio = 'KO da driver FNLSALESR';
173000150930         else;
173100150930            wFlgSMSsent = *on;
173200150930         endif;
173300150930
173400150930       endsr;
173500150930
173600150930
173700150930       //-------------------------------------------------------------*
173800150930       //Gestione invio tipo alert CL1 / CL2 / CL3 via EMAIL          *
173900150930       //-------------------------------------------------------------*
174000151001       Begsr sr_CL_EMAIL;
174100151001
174200151001         clear indmrabyps;
174300151001
174400151001         dsiaICA = siaNOTE;
174500151001         wSpedConsOraDa = %trim(%editw(§SIANOPCD:'  :  '));
174600151001         wSpedConsOraA  = %trim(%editw(§SIANOPCA:'  :  '));
174700151001         wSpedConsOrar2 = 'dalle ' + wSpedConsOraDa +
174800151001                          ' alle ' + wSpedConsOraA;
174900151001         wSpedDataConsD =
175000151001                %subst(%editc(§SIANDPC:'X'):7:2) + '/' +
175100151001                %subst(%editc(§SIANDPC:'X'):5:2) + '/' +
175200151001                %subst(%editc(§SIANDPC:'X'):1:4);
175300151001
175400151001         w§CM1VAR = '*OBJM*BRT - Variazione data consegna ' +
175500151001                    %editc(siaLNP:'X')+%editc(siaNRS:'X')+%editc(siaNSP:'X');
175600151001         chain ('MRA':'TRBMBYPSR':'ALE_VDC') tntbe01l;
175700151001
175800151001          if %found(tntbe01l);
175900151001           indmrabyps=tbeuni;
176000151001           exsr sr_trtcm1ds;
176100151001
176200151001           strTRBM='§§Tag01§§=='+%trim(wSpedRsd)          +'||';
176300151001           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wRSM_Eml)  +'||';
176400151001           strTRBM=strTRBM+'§§Tag08§§=='+
176500161003                       'http://vas.brt.it/vas/sped_det_show.htm?brtCode=';
176600151001           strTRBM=strTRBM+wBRTcode;
176700151001           strTRBM=strTRBM+'&urltype=a'+'||';
176800151005           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wSpedRmn)  +'||';
176900151005           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wSpedRma)  +'||';
177000151001           strTRBM=strTRBM+'§§Tag13§§=='+%trim(wSpedColli)+'||';
177100151001           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wSpedKg)   +'||';
177200151001           if wFlgContrAss;
177300151001              strTRBM=strTRBM+'§§Tag15§§=='+%trim(wSpedCAS)+'||';
177400151001           else;
177500151001              strTRBM=strTRBM+'§§Tag15§§=='+'0,00'+'||';
177600151001           endif;
177700151001           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wSpedInd)   +'||';
177800151001           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wSpedLocal) +'  '
177900151001                                        +%trim(wSpedProv)+'||';
178000151001           strTRBM=strTRBM+'§§Tag16§§=='+%trim(wLnaDescr)+'||';
178100151001           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wLnaIndir)+'||';
178200151001           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wLnaCAP)  +'||';
178300151001           strTRBM=strTRBM+'§§Tag19§§=='+%trim(wLnaLocal)+'||';
178400151001           strTRBM=strTRBM+'§§Tag20§§=='+'('+%trim(wLnaProv)+')'+'||';
178500151001           strTRBM=strTRBM+'§§Tag32§§=='+%trim(wSpedDataConsD)+'||';
178600151001           strTRBM=strTRBM+'§§Tag33§§=='+%trim(wSpedConsOrar2)+'||';
178700151001           strTRBM=strTRBM+'§§Tag41§§=='+%trim(wBRTcodeLong)  +'||';
178800151001
178900151001           if Bartmail_insert(strTRBM:indmrabyps:
179000151001                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
179100151001                wTesto =
179200151001                   'Consegna riprogrammata per il ' +
179300151001                   %trim(wSpedDataConsD) + ' indicativamente ' +
179400151001                   %trim(wSpedConsOrar2);
179500151001
179600151001           else;
179700151001              *in40 = *off;
179800151001              wMessaggio = 'Errore in push-entry in BartMail';
179900151001           endif;
180000151001
180100151001          else;
180200151001            *in40 = *off;
180300151001            wMessaggio = 'Tabella MRA - TRBMBYPSR ALE_VDC non trovata';
180400151001          endif;
180500150930
180600150930       endsr;
180700170208
180800170208
180900170208
181000170208       //-------------------------------------------------------------*
181100170208       //Gestione invio tipo alert AGD via SMS                        *
181200170208       //-------------------------------------------------------------*
181300170208       Begsr sr_AGD_SMS;
181400170208
181500170208         dsiaGIA = siaNOTE;
181600170208         wGiaData = %trim(%editw(%dec(%date(§SIADAP):*eur):'  /  /    '));
181700170208
181800170208         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
181900170324           ALESInDS_SMS.i_message =
182000170324                  'Spedizione da ' + %trim(wRSM_Sms) + ' non consegnata il ' +
182100170324                  %trim(wGiaData) + ' - ' + %trim(§SIACMCD) + '. ' +
182200170508                  'Puoi indicare disposizioni di RICONSEGNA su www.brt.it ' +
182300170324                  'BRTcode ' + wBRTcode;
182400170324        // ALESInDS_SMS.i_message =
182500170324        //        'Spedizione da ' + %trim(wRSM_Sms) + ' non consegnata il ' +
182600170324        //        %trim(wGiaData) + ' - ' + %trim(§SIACMCD) + '. ' +
182700170324        //        'Per chiedere la riconsegna contatti il Servizio Clienti: ' +
182800170324        //        %trim(wLnaEmail)+'@brt.it' + ' Tel ' + %trim(wLnaTel) + '  ' +
182900170324        //        'BRTcode ' + wBRTcode;
183000170209
183100170208         wTesto = ALESInDS_SMS.i_message;
183200170208
183300170208         ALESInDS_SMS.i_IdSMS   =
183400170208                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
183500170208                     %editc(wAAS:'X')+%editc(siaLNP:'X')     +
183600170407                     %editc(siaNRS:'X')+%editc(siaNSP:'X')     + '_' + 'AAGD';
183700170208
183800170208         ALESInDS_SMS.i_cell    = wDestMobile;
183900170208         if not wFlgTest;
184000170208  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
184100170208         endif;
184200170208
184300170208
184400170208         ALESInOpz = 'W';
184500170323         ALESInPRG = sVGDPRG;
184600170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
184700170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
184800170323                             ALESOutPRG:ALESOutEsito);
184900170208         if %error OR ALESOutEsito <> *blanks;
185000170208            *in40 = *off;
185100170208            wMessaggio = 'KO da driver FNLSALESR';
185200170208         else;
185300170208            wFlgSMSsent = *on;
185400170208         endif;
185500170208
185600170208       endsr;
185700151001
185800140212
185900140212
186000140212       //-------------------------------------------------------------*
186100140212       //Valorizzazione trtcm1ds                                      *
186200140212       //-------------------------------------------------------------*
186300140212        Begsr sr_trtcm1ds;
186400140212
186500140212           clear intrtcm1ds;
186600151002           string=§mraemlmit;
186700140212           if %scan('*PO':§mraemlmit) > *zeros;
186800140212              string=%replace(wLNA_A:§mraemlmit:
186900140212                              %scan('*PO':§mraemlmit:1):3);
187000140212           endif;
187100140212           exsr sr_chkstr;
187200140212           §CM1MITT=string;
187300140212           string=wDestEmail;
187400151002           if not wFlgTest;
187500171006  xxx       //      string='email_di_test@brt.it';
187600140214           endif;
187700140212           exsr sr_chkstr;
187800140212           §CM1DST=string;
187900140212           §CM1TIPS=§mrareg;
188000140212           if §mrapo<> '*PO';
188100140212              §CM1PO=§mrapo;
188200140212           else;
188300140212              §CM1PO=%editc(arbLNP:'X');
188400140212           endif;
188500140212           §CM1VAR=w§CM1VAR;
188600140212           §CM1STS='0';
188700140212           §CM1IDP=§mraidpro;
188800140212           §CM1PROG=w§CM1PROG;
188900140212           §CM1ATT='01';
189000140212           §CM1TOTATT='01';
189100140212
189200140212        endsr;
189300140212
189400140212
189500140212       //-------------------------------------------------------------*
189600140212       //controllo stringa                                            *
189700140212       //-------------------------------------------------------------*
189800140212        Begsr sr_chkstr;
189900140212           string=%xlate('||':'  ':string);
190000140212           string=%xlate('==':'  ':string);
190100140212        endsr;
190200140212
190300140212
190400140212       //-------------------------------------------------------------*
190500140212       //Invio email di alert                                         *
190600140212       //-------------------------------------------------------------*
190700140214       Begsr snd_Alert;
190800140212
190900140212          wEml   = 'cedalert@brt.it';
191000140212          wOgg   = 'Anomalia invio alert destinatari ' + w§CM1PROG;
191100140214          wMsg   = ' ID BartMail   '     + w§CM1PROG             + ':/N' +
191200140214                   ' siaAAS        '     + %editc(siaAAS:'X')    + ':/N' +
191300140212                   ' siaLNP        '     + %editc(siaLNP:'X')    + ':/N' +
191400140212                   ' siaNRS        '     + %editc(siaNRS:'X')    + ':/N' +
191500140212                   ' siaNSP        '     + %editc(siaNSP:'X')    + ':/N' +
191600140212                   ' siaTIPA       '     + %trim(siaTIPA)        + ':/N' +
191700140212                   ' Email         '     + %trim(wDestEmail)     + ':/N' +
191800140212                   ' Mobile        '     + %trim(wDestMobile)    + ':/N' +
191900140214                   ' Errore        '     + wEsito+' '
192000140214                                         + %trim(wMessaggio) + ':/N';
192100140212
192200140212          callP(e) TIS701C1 (wEml : wCcEml : wOgg : wMsg : wSndSts);
192300140212
192400140212        endsr;
192500140213
192600140213
192700140213       //-------------------------------------------------------------*
192800140213       //inserimento Richiesta Assistenza                             *
192900140213       //-------------------------------------------------------------*
193000170208        Begsr sr_InsRA;
193100140213
193200140213           *in50 = *off;
193300140213
193400140414           clear fidna6ds;
193500140213           select;
193600140213             when siaTIPA = 'RIC';
193700140414                iDA06MAD = ' 81';
193800140213                  select;
193900140213                    when wRicTipo = 'P';
194000140414                       iDA06NO1 = 'Alert inviato da chiusura PDA a:';
194100140218                    when wRicTipo = 'S';
194200140414                       iDA06NO1 = 'Alert inviato da chiusura PDA (SPC) a:';
194300140213                    when wRicTipo = 'C';
194400140414                       iDA06NO1 = 'Alert inviato da Chiusura Distinta a:';
194500140213                    other;
194600140414                       iDA06NO1 = 'Alert inviato a:';
194700140213                  endsl;
194800140213             when siaTIPA = 'FD';
194900140414                iDA06MAD = ' 80';
195000140414                iDA06NO1 = 'Alert inviato a:';
195100151001             when siaTIPA = 'CL1';
195200151001                iDA06MAD = ' 84';
195300151002                iDA06NO1 = 'IMP/DISGUIDO';
195400151001             when siaTIPA = 'CL2';
195500151001                iDA06MAD = ' 84';
195600151001                iDA06NO1 = 'DISGUIDO';
195700151001             when siaTIPA = 'CL3';
195800151001                iDA06MAD = ' 84';
195900170208                iDA06NO1 = 'NON IN DISTINTA';
196000170208             when siaTIPA = 'AGD';
196100170208                iDA06MAD = ' 87';
196200170208                iDA06NO1 = 'Alert inviato a:';
196300140213             other;
196400140213                  *in50 = *on;
196500140213           endsl;
196600140908
196700140908           select;
196800140908             when wTipoInvio = 'S';
196900140908                  IDA06RSC = 'SMS';
197000140908             when wTipoInvio = 'M';
197100140908                  IDA06RSC = 'EMAIL';
197200140908             when wTipoInvio = 'B';
197300140908                  IDA06RSC = 'SMS/EMAIL';
197400140908           endsl;
197500140213
197600140414           iDA06TOR = 'S';
197700140414           iDA06OGG = %trim(wSpedNum)+%trim(%editc(wAAS:'X'));
197800140414           iDA06UTE = 'QPGMR';
197900140414           iDA06FIL = arbLNA;
198000140612           iDA06NO1 = %trim(iDA06NO1) + ' ' + %trim(wDescRA) +
198100140612                      '  -  ' + wTesto;
198200140213
198300140213           if not *in50;
198400140414              callP(e) FIDNA6R (fidna6ds);
198500140213              if %error;
198600140213                 *in40 = *off;
198700140414                 wMessaggio = 'KO da driver FIDNA6R';
198800140213              endif;
198900140213           endif;
199000140213
199100140213        endsr;
199200140212
199300140212
199400140212       //-------------------------------------------------------------*
199500140212       //reperimento progressivo                                      *
199600140212       //-------------------------------------------------------------*
199700140212        Begsr sr_rtvPrg;
199800140212
199900140212           clear trul33ds;
200000140212           w§CM1PROG=*all'0';
200100140212           i33Cnu = 24;
200200140212           i33num = 1;
200300140212           kpjbu = trul33ds;
200400140212           trul33r(kpjba);
200500140212           trul33ds = kpjbu;
200600140212           w§CM1PROG=%subst(%editc(o33nri:'X'):9:7);
200700140212
200800140212        endsr;
200900140212
201000140210
