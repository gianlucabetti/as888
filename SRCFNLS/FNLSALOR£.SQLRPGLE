000100110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200140212     h dftactgrp(*no) actgrp(*caller) bnddir('TRBM':'UBBNDDIR')
000300110617
000400110617      *---------------------------------------------------------------*
000500140207     fazorg01l  if   e           k disk
000600140417     ffnorm01l  if   e           k disk
000700140418     ffnore01l  if   e           k disk
000800140210     ftabel00f  if   e           k disk
000900140212     ftntbe01l  if   e           k disk
001000130309
001100140212
001200140417     D FIOR900R        PR
001300140417     D                                     extpgm('FIOR900R')
001400140417     D fior900ds                           likeds(fior900ds)
001500151106
001600151106
001700151106     D FIOR902R        PR
001800151106     D                                     extpgm('FIOR902R')
001900151106     D fior902ds                           likeds(fior902ds)
002000140213
002100140213
002200140414     D FIDNA6R         PR
002300140414     D                                     extpgm('FIDNA6R')
002400140414     D fidna6ds                            likeds(fidna6ds)
002500140212
002600140212
002700140311     D TRULORSR        PR
002800140311     D                                     extpgm('TRULORSR')
002900140311     D kpjba                               likeds(kpjba)
003000140311     D trulORsds                           likeds(trulORsds)
003100140311     D trulOR2ds                           likeds(trulOR2ds)
003200140212
003300140207
003400140207     D TRUL33R         PR
003500140207     D                                     extpgm('TRUL33R')
003600140207     D kpjba                               likeds(kpjba)
003700160413
003800160413
003900160413     D TRULDSCR        PR
004000160413     D                                     extpgm('TRULDSCR')
004100160413     D TRULDSCDS                           likeds(TRULDSCDS)
004200140212
004300140212
004400140212     D FNLSALESR       PR
004500140212     D                                     extpgm('FNLSALESR')
004600140212     D InChiudi                            like(ALESInChiudi)
004700140212     D InOpz                               like(ALESInOpz)
004800140212     D InKSU                               like(ALESInKSU)
004900140212     D InTIP                               like(ALESInTIP)
005000140212     D InTSC                               like(ALESInTSC)
005100140212     D InPGM                               like(ALESInPGM)
005200170323     D InPRG                               like(ALESInPRG)
005300140213     D InDS_SMS                            likeds(ALESInDS_SMS)
005400140212     D OutPRG                              like(ALESOutPRG)
005500140212     D OutEsito                            like(ALESOutEsito)
005600140212
005700140212
005800140212     D TIS701C1        PR
005900140212     D                                     extpgm('TIS701C1')
006000140212     D wEml                                like(wEml)
006100140212     D wCcEml                              like(wCcEml)
006200140212     D wOgg                                like(wOgg)
006300140212     D wMsg                                like(wMsg)
006400140212     D wSndSts                             like(wSndSts)
006500140212
006600140212
006700130309
006800140207     D kpjba         e ds                  inz
006900150929     D og143         e ds                  inz
007000150929     D dntw          e ds                  inz
007100140414     D fidna6ds      e ds                  inz
007200140212     D trul33ds      e ds                  inz
007300140417     D fior900ds     e ds                  inz
007400151106     D fior902ds     e ds                  inz
007500140417     D fnlv55ds      e ds                  inz
007600140311     D trulORsds     e ds                  inz
007700140311     D trulOR2ds     e ds                  inz
007800160413     D trulDSCds     e ds                  inz
007900140417     D fisao00f      e ds                  inz
008000140417     D §saoRRN         s             10I 0 inz
008100140417     D dsaoFLO       e ds                  inz
008200140417     D dsaoMSG       e ds                  inz
008300140417     D wPOE            s                   like(saoPOE) inz
008400140212     D wDivid          s                   like(InDivid) inz
008500140212     D wSQL            s           2048A   varying inz
008600140417     D wPOR            s                   like(ormPOR) inz
008700140417     D wPOR_A          s              3A   inz
008800140422     D wAAS            s              2A   inz
008900140207     D wTipoInvio      s              1A   inz
009000140212     D w§CM1VAR        s                   like(§CM1VAR)  inz
009100140212     D w§CM1PROG       s                   like(§CM1PROG) inz
009200140212     D string          s            200A   varying inz
009300140212     D strTRBM         s          65535A   varying inz
009400140212     D wEml            s            253    inz
009500140212     D wCcEml          s            253    inz
009600140212     D wOgg            s             44    inz
009700140212     D wMsg            s           5000    inz
009800140212     D wSndSts         s              1    inz
009900140212     D pEsito          s             10I 0 inz
010000140612     D wTesto          s           1600    varying inz
010100150929     D skFilExt        s                   like(orgFIL) dim(500) inz
010200150929     D ixFilExt        s              3  0 inz
010300140212
010400140212
010500140212     D* DS di output
010600140213     D  ALESInDS_SMS   ds                  qualified inz
010700140212     D   i_MittSMS                   30
010800140212     D   i_IdSMS                     36
010900140212     D   i_cell                      20
011000140212     D   i_message                 1600
011100140212
011200140212
011300140212     D ALESInChiudi    s              1A   inz
011400140212     D ALESInOpz       s              1A   inz
011500140212     D ALESInKSU       s              8A   inz('0BART001')
011600140212     D ALESInTIP       s              2A   inz('SM')
011700140212     D ALESInTSC       s              2A   inz('WW')
011800170323     D ALESInPGM       s             10A   inz
011900170323     D ALESInPRG       s             10A   inz
012000140212     D ALESOutPRG      s             10A   inz
012100140212     D ALESOutEsito    s              1A   inz
012200170323     D sVGDPRG         s             10A   inz
012300140212
012400140212
012500140210
012600140417     D* Variabili di "mapping"
012700140212     D wRicData        s             10A                  inz
012800140212     D wRicOra         s              5A                  inz
012900140417     D wPorDescr       s                   like(orgDES)   inz
013000140417     D wPorTel         s                   like(orgTEL)   inz
013100140417     D wPorEmail       s                   like(orgDD2)   inz
013200140417     D wPorOraMD_A     s              5A                  inz
013300140417     D wPorOraMA_A     s              5A                  inz
013400140417     D wPorOraPD_A     s              5A                  inz
013500140417     D wPorOraPA_A     s              5A                  inz
013600140417     D wOrdRagSoc      s                   like(ormRSO)   inz
013700140417     D wOrdIndir       s                   like(ormINO)   inz
013800140417     D wOrdCAP         s                   like(ormCAO)   inz
013900140417     D wOrdLocal       s                   like(ormLOO)   inz
014000140417     D wOrdProv        s                   like(ormPRO)   inz
014100140417     D wRitiroData     s             20                   inz
014200140418     D wRitiroOra      s              5                   inz
014300140417     D wRitNum         s             17                   inz
014400140417     D wRitRfa         s                   like(ormRFA)   inz
014500140417     D wRitRsr         s                   like(ormRSR)   inz
014600140417     D wRitInr         s                   like(ormINR)   inz
014700140417     D wRitLor         s                   like(ormLOR)   inz
014800160413     D wRitCar         s                   like(ormCAR)   inz
014900140417     D wRitPrr         s                   like(ormPRR)   inz
015000140417     D wRitRer         s                   like(ormRER)   inz
015100140418     D wRitTer         s                   like(ormTER)   inz
015200140418     D wRitNOTE1       s                   like(ormNO1)   inz
015300140418     D wRitNOTE2       s                   like(ormNO2)   inz
015400140418     D wRitNOTEORD     s                   like(oreDATI)  inz
015500160414     D wRitPRENOTA     s             10A                  inz
015600140417     D wRitColli       s              5A                  inz
015700160414     D wRitBancali     s              5A                  inz
015800140417     D wRitKg          s              8A                  inz
015900160414     D wRitNatMerce    s             35A                  inz
016000140417     D wRitiroOrari    s             50A                  inz
016100150530     D wRitiroOrariS   s             50A                  inz
016200140417     D wRitiroOraDa    s              5A                  inz
016300140417     D wRitiroOraA     s              5A                  inz
016400160413     D wApertOrari     s            100A                  inz
016500160413     D wApertOraDA1    s              5A                  inz
016600160413     D wApertOraAA1    s              5A                  inz
016700160413     D wApertOraDA2    s              5A                  inz
016800160413     D wApertOraAA2    s              5A                  inz
016900160413     D wPORitOrari     s            100A                  inz
017000160413     D wPORitOraDA1    s              5A                  inz
017100160413     D wPORitOraAA1    s              5A                  inz
017200160413     D wPORitOraDA2    s              5A                  inz
017300160413     D wPORitOraAA2    s              5A                  inz
017400140417     D wRitEmail       s             71A                  inz
017500140417     D wRitMobile      s             16A                  inz
017600140324     D wDescRA         s            256A                  inz
017700140317     D wDataOraNext    s             14S 0                inz
017800140417     D wDataOraCorr    s             14S 0                inz
017900140210
018000140211
018100140212     D wEsito          s              1A   inz
018200140212     D wMessaggio      s            256A   varying inz
018300140212
018400140214     D wFlgTest        s               N   inz('0')
018500140211     D wFlgContrAss    s               N   inz('0')
018600140213     D wFlgSMSsent     s               N   inz('0')
018700150930
018800150930     D* Flag "globali"
018900150930     D wFlgTabelle     s               N   inz('0')
019000140211
019100120619
019200140207
019300140207      // ? PROTOTIPI ?
019400140207      /copy gaitrasrc/srcprotopr,trbmbypsr
019500140417      /copy gaitrasrc/srcprotopr,fnlv55r
019600110617
019700140207
019800110617
019900110627     C     *ENTRY        PLIST
020000140207     C                   PARM                    InChiudi          1
020100140207     C                   PARM                    InDivis           2 0
020200140207     C                   PARM                    InDivid           2 0
020300140207     C                   PARM                    InTIPA            3
020400130309
020500110617      /free
020600130309
020700140207       // imposto settaggi base di sql
020800140207       exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
020900140207
021000140212       // gestione chiusura *pgm
021100140207       if InChiudi='S' or %shtdn;
021200140207          // eseguo chiusure a driver "aperti"
021300140211          exsr sr_Chiusura;
021400110627       else;
021500140210
021600150930          // Carico le tabelle necessarie (se non già caricate)
021700150930          if not wFlgTabelle;
021800150930             exsr sr_Cartab;
021900150930          endif;
022000150929
022100140207          // procedo con l'elaborazione
022200140207          exsr sr_Elabora;
022300140207
022400130309          eval *inrt = *on;
022500110627       endif;
022600130308
022700130308
022800110617       //-------------------------------------------------------------*
022900140417       //Lettura file "spia" alert FISAO00F                           *
023000110617       //-------------------------------------------------------------*
023100140207       Begsr sr_Elabora;
023200130401
023300170323         // Inizio la transazione di download
023400170329         //ALESInOpz = 'S';
023500170329         //callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
023600170329         //                    ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
023700170329         //                    ALESOutPRG:ALESOutEsito);
023800170329         //if %error OR ALESOutEsito <> *blanks;
023900170329         //   *in40 = *off;
024000170329         //   wMessaggio = 'KO da driver FNLSALESR';
024100170329         //else;
024200170329         //   sVGDPRG = ALESOutPRG;
024300170329         //endif;
024400170329
024500170329         sVGDPRG = 'XX00000000';
024600140214
024700140212
024800140207         // verifico se richiesto filtro per particolare tipologia di alert
024900140315         if InChiudi = 'T';
025000140417               wSQL = 'select fisao00f.*, rrn(fisao00f) from fisao00f ' +
025100140417                      ' where saoSTS = ''0'' ' +
025200140417                      ' and substr(saoFLO, 1, 1) = ''T'' ' +
025300150929                      ' and saoPTY < 050 ' +
025400140805                      ' order by saoPTY ' +
025500141020                 //   ' fetch first 200 rows only' +
025600140805                      ' for read only';
025700140315         else;
025800140315             if InTIPA <> *blanks;
025900140417                  wSQL = 'select fisao00f.*, rrn(fisao00f) from fisao00f ' +
026000140417                         ' where saoSTS = ''0'' ' +
026100170831                         ' and saoTIPA=''' + InTIPA + '''' +
026200150929                         ' and saoPTY < 050 ' +
026300140805                         ' order by saoPTY ' +
026400141020                 //      ' fetch first 200 rows only' +
026500140805                         ' for read only';
026600140315             else;
026700140417                  wSQL = 'select fisao00f.*, rrn(fisao00f) from fisao00f ' +
026800140417                         ' where saoSTS = ''0'' ' +
026900150929                         ' and saoPTY < 050 ' +
027000140805                         ' order by saoPTY ' +
027100141020                 //      ' fetch first 200 rows only' +
027200140805                         ' for read only';
027300140315             endif;
027400140315         endif;
027500140207
027600140207         // preparo il cursore del recordset da leggere
027700140207         exec sql  prepare s1 from :wSQL;
027800140207         exec sql  declare c1 cursor for s1;
027900140207         exec sql  open    c1;
028000140207
028100140210         // ciclo su tutto il recordset estratto
028200140417         exec sql fetch next from c1 into :fisao00f, :§saoRRN;
028300140207         dow sqlcod=0;
028400140207
028500140207           // controllo costantemente se richiesta chiusura del sottosistema
028600140207           if %shtdn;
028700140212              InChiudi = 'S';
028800140211              exsr sr_Chiusura;
028900140207              leavesr;
029000140207           endif;
029100140207
029200140207           // Se richiesta elaborazione parallela gestisco
029300140207           exsr sr_ElaPar;
029400140207
029500140207           // se OK => procedo
029600140207           if *in40;
029700140212              // inizializzazioni dati necessari per gli alerts
029800140212              exsr sr_InzDati;
029900140212
030000140214              // verifico se trattasi di record di test
030100151109              exsr sr_RtvTest;
030200140214
030300140207              // effettuo controlli pre invio
030400151109              select;
030500151109                 when NOT wFlgTest AND %subst(saoTIPA:1:1) = 'A';
030600151109                      exsr sr_ChkSendAlert;
030700151109                 when NOT wFlgTest AND %subst(saoTIPA:1:1) = 'C';
030800151109                      exsr sr_ChkSendConf;
030900151109                 when     wFlgTest AND %subst(saoTIPA:1:1) = 'A';
031000151109                      exsr sr_ChkSendAlert;
031100151109                      exsr sr_OvrDatiTest;
031200151109                 when     wFlgTest AND %subst(saoTIPA:1:1) = 'C';
031300151109                      exsr sr_ChkSendConf;
031400151109                      exsr sr_OvrDatiTest;
031500151109                 other;
031600151109                      *in40 = *off;                  // record KO
031700151109              endsl;
031800151109
031900140207           endif;
032000140207
032100140207           // se OK => procedo
032200140207           if *in40;
032300140210              // reperisco i dati necessari
032400140210              exsr sr_RtvDati;
032500140207           endif;
032600140207
032700140207           // verifico il tipo alert corrente
032800140207           if *in40;
032900140207              select;
033000140417                when saoTIPA = 'AVV' and wTipoInvio = 'M';
033100140417                     exsr sr_AVV_Email;
033200140417                when saoTIPA = 'AVV' and wTipoInvio = 'S';
033300140417                     exsr sr_AVV_SMS;
033400140417                when saoTIPA = 'AVV' and wTipoInvio = 'B';
033500140417                     exsr sr_AVV_Email;
033600140417                     exsr sr_AVV_SMS;
033700151106                when saoTIPA = 'CON' and wTipoInvio = 'M';
033800151106                     exsr sr_CON_Email;
033900160413                when saoTIPA = 'CON' and wTipoInvio = 'S';
034000160413                     exsr sr_CON_SMS;
034100160413                when saoTIPA = 'CON' and wTipoInvio = 'B';
034200160413                     exsr sr_CON_Email;
034300160413                     exsr sr_CON_SMS;
034400140207                other;
034500140214                     *in40 = *off;
034600140417                     wMessaggio = 'Valore SAOTIPA non previsto: '+saoTIPA;
034700140212                     exsr snd_Alert;
034800140207              endsl;
034900140207           endif;
035000140207
035100140211
035200140211           // Verifico se record elaborato OK e aggiorno esito
035300140211           if *in40;
035400140218              wEsito = '1';
035500140217              select;
035600140217                when wTipoInvio = 'S';
035700140417                     wMessaggio = 'Alert inviato, SMS a: ' + %trim(wRitMobile);
035800140217                when wTipoInvio = 'M';
035900140417                     wMessaggio = 'Alert inviato, email a: '+ %trim(wRitEmail);
036000140217                when wTipoInvio = 'B';
036100140217                     wMessaggio = 'Alert inviato, SMS/email a: '+
036200140417                                  %trim(wRitMobile) + '/' + %trim(wRitEmail);
036300140217              endsl;
036400140214              if not wFlgTest;
036500140322                 select;
036600140322                   when wTipoInvio = 'S';
036700140417                        wDescRA = %trim(wRitMobile);
036800140322                        exsr sr_InsRA;
036900140322                   when wTipoInvio = 'M';
037000140417                        wDescRA = %trim(wRitEmail);
037100140322                        exsr sr_InsRA;
037200140322                   when wTipoInvio = 'B';
037300140417                        wDescRA = %trim(wRitMobile)+' e: '+
037400140417                                  %trim(wRitEmail);
037500140322                        exsr sr_InsRA;
037600140322                 endsl;
037700140214              endif;
037800140211           else;
037900140218              if *in41;
038000140218                 wEsito = '0';
038100140218              else;
038200140218                 wEsito = 'E';
038300140218              endif;
038400140211           endif;
038500140417           exsr sr_UpdFISAO;
038600140211
038700140417           exec sql fetch next from c1 into :fisao00f, :§saoRRN;
038800140207         enddo;
038900140207
039000140207         exec sql  close   c1;
039100170323
039200170323
039300170323         // Sancisco la transazione di download
039400170323         ALESInOpz = 'E';
039500170323         ALESInPRG = sVGDPRG;
039600170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
039700170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
039800170323                             ALESOutPRG:ALESOutEsito);
039900170323         if %error OR ALESOutEsito <> *blanks;
040000170323            *in40 = *off;
040100170323            wMessaggio = 'KO da driver FNLSALESR';
040200170323         endif;
040300130308
040400140207       endsr;
040500140417
040600140212
040700140212       //-------------------------------------------------------------*
040800140417       //Aggiornamento record di FISAO corrente                       *
040900140212       //-------------------------------------------------------------*
041000140417       Begsr sr_UpdFISAO;
041100140212
041200140417         exec sql  update fisao00f set
041300140417                          saoSTS   = :wEsito,
041400140417                          saoMSG   = :wMessaggio,
041500140417                          saoDAORE = current_timestamp
041600140417                   where  rrn(fisao00f) = :§saoRRN AND
041700140417                          saoSTS = '0';
041800140212
041900140212       endsr;
042000150929
042100150929
042200150929       //-------------------------------------------------------------*
042300150929       //caricamento codici bolla con c/a                              *
042400150929       //-------------------------------------------------------------*
042500150929       Begsr sr_Cartab;
042600150930
042700150930          wFlgTabelle = *on;
042800150929
042900150929          // Carico la lista di filiali "estere" (secondo il network)
043000150929          clear skFilExt;
043100150929          clear ixFilExt;
043200150929          setll (*zeros) azorg01l;
043300150929          read azorg01l;
043400150929
043500150929          dow not %eof(azorg01l);
043600150929             if ORGFVA = *blanks;
043700150929                og143 = orgDE3;
043800150929                chain ('NTW':§OGNTW) tntbe01l;
043900150929                if %found(tntbe01l);
044000150929                   dntw = tbeuni;
044100150929                   if §NTWFIE = 'E';
044200150929                      ixFilExt = ixFilExt + 1;
044300150929                      skFilExt(ixFilExt) = orgFIL;
044400150929                   endif;
044500150929                endif;
044600150929             endif;
044700150929             read azorg01l;
044800150929          enddo;
044900150929
045000150929       endsr;
045100140211
045200140211
045300140211       //-------------------------------------------------------------*
045400140211       //inizializzazioni                                             *
045500140211       //-------------------------------------------------------------*
045600140212       Begsr sr_InzDati;
045700140212
045800140214          KNMUS = 'QPGMR';
045900140214
046000140417          clear dsaoFLO;
046100140417          clear dsaoMSG;
046200140417          clear fior900ds;
046300151106          clear fior902ds;
046400140417          clear fnlv55ds;
046500140311          clear trulORsds;
046600140311          clear trulOR2ds;
046700160413          clear trulDSCds;
046800140612          clear wTesto;
046900140212
047000140417          wRitiroData  = 'domani';
047100140418          wRitiroOra   = '09:00';
047200140417          wRitiroOrari = 'in giornata';
047300150530          wRitiroOrariS = 'in giornata';
047400160413
047500160413          clear wApertOrari;
047600160413          clear wApertOraDA1;
047700160413          clear wApertOraAA1;
047800160413          clear wApertOraDA2;
047900160413          clear wApertOraAA2;
048000160413
048100160413          clear wPORitOrari;
048200160413          clear wPORitOraDA1;
048300160413          clear wPORitOraAA1;
048400160413          clear wPORitOraDA2;
048500160413          clear wPORitOraAA2;
048600140212
048700140212          reset ALESInChiudi;
048800140212          reset ALESInOpz;
048900140212          reset ALESInKSU;
049000140212          reset ALESInTIP;
049100140212          reset ALESInTSC;
049200140212          reset ALESInPGM;
049300170323          reset ALESInPRG;
049400140212          reset ALESInDS_SMS;
049500140212          reset ALESOutPRG;
049600140212          reset ALESOutEsito;
049700140212
049800140212          reset wEml;
049900140212          reset wCcEml;
050000140212          reset wOgg;
050100140212          reset wMsg;
050200140212          reset wSndSts;
050300140212
050400140214          reset wTipoInvio;
050500140212          reset wEsito;
050600140212          reset wMessaggio;
050700140212
050800140417          reset wRicData;
050900140211          reset wRicOra;
051000140417          reset wPorDescr;
051100140417          reset wPorTel;
051200140417          reset wPorEmail;
051300140417          reset wPorOraMD_A;
051400140417          reset wPorOraMA_A;
051500140417          reset wPorOraPD_A;
051600140417          reset wPorOraPA_A;
051700140417          reset wOrdRagSoc;
051800140417          reset wOrdIndir;
051900140417          reset wOrdCAP;
052000140417          reset wOrdLocal;
052100140417          reset wOrdProv;
052200140417          reset wRitNum;
052300140417          reset wRitRfa;
052400140417          reset wRitRsr;
052500140417          reset wRitInr;
052600140417          reset wRitLor;
052700160413          reset wRitCar;
052800140417          reset wRitPrr;
052900140417          reset wRitRer;
053000140417          reset wRitTer;
053100140418          reset wRitNOTE1;
053200140418          reset wRitNOTE2;
053300140418          reset wRitNOTEORD;
053400160414          reset wRitPRENOTA;
053500140417          reset wRitColli;
053600160414          reset wRitBancali;
053700160414          reset wRitNatMerce;
053800140417          reset wRitKg;
053900140417          reset wRitiroOraDa;
054000140417          reset wRitiroOraA;
054100140417          reset wRitEmail;
054200140417          reset wRitMobile;
054300140417          reset wDescRA;
054400140211
054500140214          reset wFlgTest;
054600140213          reset wFlgSMSsent;
054700140211
054800140211       endsr;
054900140214
055000140214
055100140214       //-------------------------------------------------------------*
055200151109       //Verifica se trattasi di elaborazione record di TEST          *
055300140214       //-------------------------------------------------------------*
055400151109       Begsr sr_RtvTest;
055500140214
055600151109          wPOE = saoPOE;
055700140417          dsaoFLO = saoFLO;
055800140214
055900140417          if §SAOFLOTIP = 'T';
056000151109             wFlgTest = *on;
056100151109             dsaoMSG = saoMSG;
056200151109             wPOE = §SAOMSGPOE;
056300140214          endif;
056400140214
056500140214       endsr;
056600151109
056700151109
056800151109       //-------------------------------------------------------------*
056900151109       //Reperisce informazioni per elaborazione record di TEST       *
057000151109       //-------------------------------------------------------------*
057100151109       Begsr sr_OvrDatiTest;
057200151109
057300151109          if §SAOFLOTIP = 'T';
057400151109             dsaoMSG = saoMSG;
057500151109             wPOE = §SAOMSGPOE;
057600151109             wRitEmail  = %trim(§SAOMSGEML);
057700151109             wRitMobile = %trim(§SAOMSGSMS);
057800151109             select;
057900151109               when wRitEmail <> *blanks AND wRitMobile <> *blanks;
058000151109                    wTipoInvio = 'B';
058100151109               when wRitEmail <> *blanks;
058200151109                    wTipoInvio = 'M';
058300151109               when wRitMobile <> *blanks;
058400151109                    wTipoInvio = 'S';
058500151109               other;
058600151109                    *in40 = *off;
058700151109                    wMessaggio = 'Valore SAOTIPA non previsto: '+saoTIPA;
058800151109                    exsr snd_Alert;
058900151109             endsl;
059000151109          endif;
059100151109
059200151109       endsr;
059300140210
059400140210
059500140210       //-------------------------------------------------------------*
059600140210       //gestione operazioni di chiusura                              *
059700140210       //-------------------------------------------------------------*
059800140212       Begsr sr_Chiusura;
059900140210
060000140211          eval *inlr = *on;
060100140210
060200140210       endsr;
060300130401
060400130401
060500130401       //-------------------------------------------------------------*
060600130401       //gestione elaborazione parallela                              *
060700130401       //-------------------------------------------------------------*
060800140207       Begsr sr_ElaPar;
060900130401
061000130401           *in40 = *off;                  // record KO
061100140207           if InDivis > *zeros AND InDivid > *zeros;
061200140207              wDivid = InDivid - 1;
061300140417              if %rem(saoNOR:InDivis) = wDivid;
061400130401                 *in40 = *on;             // record OK
061500130401              endif;
061600130401           else;
061700130401              *in40 = *on;                // record OK
061800130401           endif;
061900130401
062000130401       endsr;
062100140207
062200140207
062300140207       //-------------------------------------------------------------*
062400151106       //Effettuo controlli pre invio - per tipi "ALERT"              *
062500140207       //-------------------------------------------------------------*
062600151106       Begsr sr_ChkSendAlert;
062700140207
062800140212           *in40 = *on;                   // record OK
062900140218           *in41 = *off;
063000160502
063100140212           // 1° reperimento email/mobile
063200140417           iOR900TRC  = 'A';
063300140417           iOR900POE  = wPOE;
063400140417           iOR900NSR  = saoNSR;
063500140417           iOR900NOR  = saoNOR;
063600140417           iOR900NRV  = saoNRV;
063700140417
063800140417           callP(e) FIOR900R (fior900ds);
063900140417           if not %error AND oOR900ERR <> 'N';
064000140207              // verifico se richiesto invio email o SMS
064100140207              select;
064200140417                 when oOR900MAIL <> *blanks AND oOR900SMS  <> *blanks;
064300140322                      wTipoInvio = 'B';
064400140417                      wRitEmail = oOR900MAIL;
064500140417                      wRitMobile = oOR900SMS;
064600140417                 when oOR900MAIL <> *blanks AND oOR900SMS   = *blanks;
064700140212                      wTipoInvio = 'M';
064800140417                      wRitEmail = oOR900MAIL;
064900140417                 when oOR900SMS  <> *blanks AND oOR900MAIL  = *blanks;
065000140207                      wTipoInvio = 'S';
065100140417                      wRitMobile = oOR900SMS;
065200140212                 other;
065300140212                      *in40 = *off;
065400140212                      wMessaggio = 'Assenti sia email che mobile';
065500140207              endsl;
065600140212           else;
065700140212              *in40 = *off;
065800140417              wMessaggio = 'KO da driver FIOR900R';
065900140207           endif;
066000140207
066100140207       endsr;
066200151106
066300151106
066400151106       //-------------------------------------------------------------*
066500151106       //Effettuo controlli pre invio - per tipi "CONFERMA"           *
066600151106       //-------------------------------------------------------------*
066700151106       Begsr sr_ChkSendConf;
066800151106
066900151106           *in40 = *on;                   // record OK
067000151106           *in41 = *off;
067100151106
067200151106           // Reperisco i dati necessari al alert di conferma
067300151106           // accettazione ORM da internet richiamando l'apposito driver
067400151106           iOR902TLA  = *blanks;
067500151106           iOR902POE  = wPOE;
067600151106           iOR902NSR  = saoNSR;
067700151106           iOR902NOR  = saoNOR;
067800151106           iOR902NRV  = saoNRV;
067900151106
068000151106           callP(e) FIOR902R (fior902ds);
068100151106           if oOR902ERR = *blanks;
068200151106           //  if not %error AND oOR902ERR = *blanks;
068300160502
068400160502              // verifico se richiesto invio email o SMS
068500160502              select;
068600160502                 when oOR902MAIL <> *blanks AND oOR902SMS  <> *blanks;
068700160502                      wTipoInvio = 'B';
068800160502                      wRitEmail  = oOR902MAIL;
068900160502                      wRitMobile = oOR902SMS;
069000160502                 when oOR902MAIL <> *blanks AND oOR902SMS   = *blanks;
069100160502                      wTipoInvio = 'M';
069200160502                      wRitEmail  = oOR902MAIL;
069300160502                 when oOR902SMS  <> *blanks AND oOR902MAIL  = *blanks;
069400160502                      wTipoInvio = 'S';
069500160502                      wRitMobile = oOR902SMS;
069600160502                 other;
069700160502                      *in40 = *off;
069800160502                      wMessaggio = 'Assenti sia email che mobile';
069900160502              endsl;
070000151106
070100151109              wRitiroData = 'il ' + %subst(%editc(oOR902DAR:'X'):7:2)+'/'+
070200151109                                    %subst(%editc(oOR902DAR:'X'):5:2)+'/'+
070300151109                                    %subst(%editc(oOR902DAR:'X'):1:4);
070400151106              wRitiroOraDa = %trim(%editw(oOR902ORI:'  :  '));
070500151106              wRitiroOraA  = %trim(%editw(oOR902ORF:'  :  '));
070600151106              wRitiroOrari = 'dalle ' + wRitiroOraDa +
070700151106                             ' alle ' + wRitiroOraA;
070800160413
070900160414              if oOR902DAM > *zeros OR oOR902AAM > *zeros OR
071000160414                 oOR902DPM > *zeros OR oOR902APM > *zeros;
071100160414                 wApertOraDA1 = %trim(%editw(oOR902DAM:'  :  '));
071200160414                 wApertOraAA1 = %trim(%editw(oOR902AAM:'  :  '));
071300160414                 wApertOraDA2 = %trim(%editw(oOR902DPM:'  :  '));
071400160414                 wApertOraAA2 = %trim(%editw(oOR902APM:'  :  '));
071500160414                 if oOR902DPM = *zeros AND oOR902APM = *zeros;
071600160414                    wApertOrari = 'dalle ' + wApertOraDA1 +
071700160414                                  ' alle ' + wApertOraAA1;
071800160414                 else;
071900160414                    wApertOrari = 'dalle ' + wApertOraDA1 +
072000160414                                  ' alle ' + wApertOraAA1 + ' ; ' +
072100160414                                  'dalle ' + wApertOraDA2 +
072200160414                                  ' alle ' + wApertOraAA2;
072300160414                 endif;
072400160414                 wApertOrari = 'Orari apertura: ' +
072500160414                               %trim(wApertOrari);
072600160414              endif;
072700160413
072800160414              wPORitOraDA1 = %subst(OOR902POR1:1:2) + ':' +
072900160414                             %subst(OOR902POR1:3:2);
073000160414              wPORitOraAA1 = %subst(OOR902POR2:1:2) + ':' +
073100160414                             %subst(OOR902POR2:3:2);
073200160414              wPORitOraDA2 = %subst(OOR902POR3:1:2) + ':' +
073300160414                             %subst(OOR902POR3:3:2);
073400160414              wPORitOraAA2 = %subst(OOR902POR4:1:2) + ':' +
073500160414                             %subst(OOR902POR4:3:2);
073600160414              if (oOR902POR3 = *zeros  AND oOR902POR4 = *zeros)  OR
073700160414                 (oOR902POR3 = *blanks AND oOR902POR4 = *blanks);
073800160414                  wPORitOrari = 'dalle ' + wPORitOraDA1 +
073900160414                                ' alle ' + wPORitOraAA1;
074000160413              else;
074100160414                  wPORitOrari = 'dalle ' + wPORitOraDA1 +
074200160414                                ' alle ' + wPORitOraAA1 + ' e ' +
074300160414                                'dalle ' + wPORitOraDA2 +
074400160414                                ' alle ' + wPORitOraAA2;
074500160413              endif;
074600151106
074700151106           else;
074800151106              *in40 = *off;
074900151106              wMessaggio = 'KO da driver FIOR902R';
075000151106           endif;
075100151106
075200151106       endsr;
075300151106
075400140207
075500140207       //-------------------------------------------------------------*
075600140207       //Reperimento dati                                             *
075700140207       //-------------------------------------------------------------*
075800140207       Begsr sr_RtvDati;
075900140210
076000140418        // Reperisco i dati del ORM corrente
076100140417        chain (wPOE:saoNSR:saoNOR:saoNRV) fnorm01l;
076200140210
076300140417        if %found(fnorm01l);
076400140418
076500160414           // Reperisco le note di ritiro ORM corrente da ext. tipo record 'N'
076600140418           chain (wPOE:saoNSR:saoNOR:saoNRV:'N') fnore01l;
076700140418           if %found(fnore01l);
076800140418              wRitNOTEORD = %trim(oreDATI);
076900140418           endif;
077000160414
077100160414           // Reperisco numero prenotazione ORM corrente da ext.tipo record 'NP'
077200160414           chain (wPOE:saoNSR:saoNOR:saoNRV:'NP') fnore01l;
077300160414           if %found(fnore01l);
077400160414              wRitPRENOTA = %trim(oreDATI);
077500160414           endif;
077600140418
077700140210
077800140210           // Se la linea di arrivo è in £6 cerco la sua capofila
077900140417           clear wPOR;
078000140417           d55lin = ormPOR;
078100140210           d55tpt = '6';
078200140417           d55drf = ormDAR;
078300140210           fnlv55r(fnlv55ds);
078400140210           if d55tfa <> *zeros;
078500140417              wPOR = d55tfa;
078600140210           else;
078700140417              wPOR = ormPOR;
078800140210           endif;
078900150929
079000150929
079100150929           // No ORM "export"
079200150929           if *in40;
079300160413              if  %subst(saoTIPA:1:1) = 'A' AND
079400160413                  %lookup(ormPOR:skFilExt) > 0;
079500150929                 *in40 = *off;
079600150929                 wMessaggio = 'Ritiro EXPORT: alert bloccato';
079700150929              endif;
079800150929           endif;
079900150929
080000150929
080100150929           // Se ritiro NON è "export"
080200150929           if *in40;
080300140210
080400140422           clear wAAS;
080500140422           wAAS = %subst(%trim(%editc(ormDAO:'X')):3:2);
080600140422
080700160413
080800160413           // Reperisco i dati del Assistenza Clienti di una filiale
080900160413           iULDSCFIL = wPOR;
081000160413           trulDSCr(trulDSCds);
081100160413           if oULDSCERR = *blanks;
081200140311
081300140417              IOREDTA  = ormDAR;
081400140417              IOREFIL  = ormPOR;
081500140417              IORECAP  = ormCAR;
081600140417              IORELOC  = ormLOR;
081700140417              IORENAR  = ormNAR;
081800140417              IORETSER = 'R';
081900140417              IOREDDC  = ormDDC;
082000140417              IORENDC  = ormNDC;
082100140417              IORETFP  = wPOR;
082200140417              IORETFA  = wPOR;
082300140311
082400140311              callP(e) TRULORSR (kpjba:trulORsds:trulOR2ds);
082500140311              if not %error AND OOREERR = *blanks;
082600161117                 if ORMrmp = *blanks ;
082700161117                    wRitiroOraDa = %trim(%editw(OOR2STIS:'  :  '));
082800161117                    wRitiroOraA  = %trim(%editw(OOR2STFS:'  :  '));
082900161117                 else;
083000161117                    wRitiroOraDa = %trim(%editw(OOR2MIIS:'  :  '));
083100161117                    wRitiroOraA  = %trim(%editw(OOR2MXFS:'  :  '));
083200161117                 endif;
083300161117                 wRitiroOrari = 'dalle ' + wRitiroOraDa +
083400161117                                ' alle ' + wRitiroOraA;
083500161117                 wRitiroOrariS = '('+wRitiroOraDa+' - '+wRitiroOraA+')';
083600140311              endif;
083700140319
083800140312
083900140312              // verifico se data-ora prossima consegna indicativa è
084000140317              // inferiore alla data-ora corrente
084100140417              wDataOraNext = (ormDAR*10000+OOR2STFS)*100;
084200140319              wDataOraCorr = %dec(%date():*iso)*1000000+%dec(%time():*iso);
084300160502              if %subst(saoTIPA:1:1) = 'A' AND
084400160502                 wDataOraNext <= wDataOraCorr AND §SAOFLOTIP <> 'T';
084500140319                 *in40 = *off;
084600140417                 wMessaggio = 'Data/ora ritiro indicativa ' +
084700140417                              'inferiore a data/ora corrente';
084800140319              else;
084900140212
085000160413              wPorDescr   = oULDSCDESC;
085100160413              wPorTel     = oULDSCTEL;
085200160413              wPorEmail   = oULDSCMAIL;
085300160414              if  %subst(saoTIPA:1:1) = 'A';
085400160414
085500160414              if oULDSCAMDA <> *blanks AND oULDSCAMDA <> *zeros;
085600160414                wPorOraMD_A = %subst(oULDSCAMDA:1:2)+':'+%subst(oULDSCAMDA:3:2);
085700160414              endif;
085800160414              if oULDSCAMA  <> *blanks AND oULDSCAMA  <> *zeros;
085900160414                wPorOraMA_A = %subst(oULDSCAMA :1:2)+':'+%subst(oULDSCAMA :3:2);
086000160413              endif;
086100160414              if oULDSCPMDA <> *blanks AND oULDSCPMDA <> *zeros;
086200160414                wPorOraPD_A = %subst(oULDSCPMDA:1:2)+':'+%subst(oULDSCPMDA:3:2);
086300160413              endif;
086400160414              if oULDSCPMA  <> *blanks AND oULDSCPMA  <> *zeros;
086500160414                wPorOraPA_A = %subst(oULDSCPMA :1:2)+':'+%subst(oULDSCPMA :3:2);
086600160413              endif;
086700160414              if (oULDSCPMDA = *zeros  AND oULDSCPMA  = *zeros)  OR
086800160414                 (oULDSCPMDA = *blanks AND oULDSCPMA  = *blanks);
086900160414                  wPORitOrari = 'dalle ' + wPorOraMD_A +
087000160414                                ' alle ' + wPorOraMA_A;
087100160414              else;
087200160414                  wPORitOrari = 'dalle ' + wPorOraMD_A +
087300160414                                ' alle ' + wPorOraMA_A + ' e ' +
087400160414                                'dalle ' + wPorOraMD_A +
087500160414                                ' alle ' + wPorOraMA_A;
087600160414              endif;
087700160414
087800160414              endif;
087900160414
088000160414              wOrdRagSoc = ormRSO;
088100160414              wOrdIndir  = ormINO;
088200160414              wOrdCAP    = ormCAO;
088300140417              wOrdLocal  = ormLOO;
088400140417              wOrdProv   = ormPRO;
088500150530              if ormDAR > *zeros;
088600150530                 wRitiroData = 'il '+ %subst(%editc(ormDAR:'X'):7:2) + '/' +
088700150530                                      %subst(%editc(ormDAR:'X'):5:2) + '/' +
088800150530                                      %subst(%editc(ormDAR:'X'):1:4);
088900150530              endif;
089000140418              wRitiroOra = %trim(%editw(ormORR:'  :  '));
089100140417              wRitNum    = %trim(%editc(ormPOE:'X')) +'/'+
089200140417                           %trim(%editc(ormNSR:'X')) +'/'+
089300140417                           %trim(%editc(ormNOR:'X')) +'/'+
089400140417                           %trim(%editc(ormNRV:'X'));
089500140417              wRitRfa    = %trim(ormRFA);
089600140417              wRitRsr    = %trim(ormRSR);
089700140417              wRitInr    = %trim(ormINR);
089800140417              wRitLor    = %trim(ormLOR);
089900160414              wRitCar    = %trim(ormCAR);
090000140417              wRitPrr    = %trim(ormPRR);
090100140417              wRitRer    = %trim(ormRER);
090200140417              wRitTer    = %trim(ormTER);
090300140418              wRitNOTE1  = %trim(ormNO1);
090400140418              wRitNOTE2  = %trim(ormNO2);
090500140417              wRitColli  = %trim(%editc(ormNCL:'Z'));
090600160414              wRitBancali= %trim(%editc(ormBNC:'Z'));
090700140417              wRitKg     = %trim(%editc(ormPKG:'2'));
090800160414              wRitNatMerce = %trim(ormNAM);
090900160414
091000160414              // Forzatura per Num.Prenotazione => se vuoto considero Num.ORM
091100160414              if oOR902NPR = *blanks;
091200160414                 oOR902NPR = %trim(%editc(ormPOE:'X')) +
091300160414                             %trim(%editc(ormNOR:'X'));
091400160414              endif;
091500160414              if wRitPRENOTA = *blanks;
091600160414                 wRitPRENOTA = %trim(%editc(ormPOE:'X')) +
091700160414                               %trim(%editc(ormNOR:'X'));
091800160414              endif;
091900140210
092000140210              // Stacco subito un progressivo per la email corrente
092100140212              exsr sr_rtvPrg;
092200140312
092300140312              endif;
092400140210
092500140212           else;
092600140212              *in40 = *off;
092700140417              wMessaggio = 'AZORG per ' + %editc(wPOR:'X') + ' non trovato';
092800140212           endif;
092900140210
093000150929           endif;
093100150929
093200140212        else;
093300140212           *in40 = *off;
093400140417           wMessaggio = 'FNORM non trovato';
093500140212        endif;
093600140210
093700140207       endsr;
093800140212
093900140212
094000140212       //-------------------------------------------------------------*
094100140417       //Gestione invio tipo alert AVV via EMAIL                      *
094200140212       //-------------------------------------------------------------*
094300140417       Begsr sr_AVV_Email;
094400140212
094500140212         clear indmrabyps;
094600140212
094700140417         w§CM1VAR = '*OBJM*BRT - Avviso di ritiro merce ' + %editc(saoPOE:'X')+
094800140417                     %editc(saoNSR:'X')+%editc(saoNOR:'X')+%editc(saoNRV:'X');
094900140417         chain ('MRA':'TRBMBYPSR':'ALO_AVV') tntbe01l;
095000140212
095100140212          if %found(tntbe01l);
095200140212           indmrabyps=tbeuni;
095300140212           exsr sr_trtcm1ds;
095400160414
095500160414
095600140417           strTRBM='§§Tag01§§=='+%trim(wRitRsr)          +'||';
095700140417           strTRBM=strTRBM+'§§Tag02§§=='+%trim(wOrdRagSoc)+'||';
095800140417           strTRBM=strTRBM+'§§Tag03§§=='+%trim(wOrdIndir) +'||';
095900140417           strTRBM=strTRBM+'§§Tag04§§=='+%trim(wOrdCAP)   +'||';
096000140417           strTRBM=strTRBM+'§§Tag05§§=='+%trim(wOrdLocal) +'||';
096100140417           strTRBM=strTRBM+'§§Tag06§§=='+%trim(wOrdProv)  +'||';
096200160413           strTRBM=strTRBM+'§§Tag07§§=='+%trim(wRitInr) + '<BR>' +
096300160413                                         %trim(wRitCar) + '   ' +
096400160413                                         %trim(wRitLor) + '   ' +
096500140417                                         %trim(wRitPrr)+'||';
096600140417           strTRBM=strTRBM+'§§Tag08§§=='+%trim(wRitRer)  +'||';
096700160414           strTRBM=strTRBM+'§§Tag09§§=='+%trim(wRitRfa)  +'||';
096800160414
096900160414           if wRitColli <> *blanks;
097000160414              strTRBM=strTRBM+'§§Tag10§§=='+'Colli: ' +
097100160414                               %trim(wRitColli) + '||';
097200160414           else;
097300160414              strTRBM=strTRBM+'§§Tag10§§=='+'Bancali: ' +
097400160414                               %trim(wRitBancali) + '||';
097500160414           endif;
097600160414
097700160414           strTRBM=strTRBM+'§§Tag11§§=='+%trim(wRitKg)+'||';
097800160414           strTRBM=strTRBM+'§§Tag12§§=='+%trim(wRitNatMerce)+'||';
097900160414           strTRBM=strTRBM+'§§Tag14§§=='+%trim(wRitTer)  +'||';
098000160414           strTRBM=strTRBM+'§§Tag15§§=='+%trim(wRitNum)  +'||';
098100160414
098200140418           if %trim(wRitNOTE1)+%trim(wRitNOTE2)+%trim(wRitNOTEORD)<>*blanks;
098300140418              strTRBM=strTRBM+'§§Tag16§§=='+'Note di ritiro: ' +
098400140418                                           %trim(wRitNOTE1)    + ' ' +
098500140418                                           %trim(wRitNOTE2)    + '<BR>' +
098600140418                                           %trim(wRitNOTEORD)  +'||';
098700140418           else;
098800140418              strTRBM=strTRBM+'§§Tag16§§=='+''+'||';
098900140418           endif;
099000140418
099100160414           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wRitiroData)+'||';
099200160414           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wRitiroOrari)+'||';
099300160414
099400160414           if %trim(wApertOrari) <> *blanks;
099500160414              strTRBM=strTRBM+'§§Tag22§§=='+%trim(wApertOrari)  +'||';
099600160414           else;
099700160414              strTRBM=strTRBM+'§§Tag22§§=='+''+'||';
099800160414           endif;
099900160414
100000160414           strTRBM=strTRBM+'§§Tag23§§=='+%trim(wRitPRENOTA)+'||';
100100160414           strTRBM=strTRBM+'§§Tag24§§=='+%trim(wPorDescr)+'||';
100200160414           strTRBM=strTRBM+'§§Tag25§§=='+%trim(wPorEmail)+'@brt.it'+'||';
100300160414           strTRBM=strTRBM+'§§Tag26§§=='+%trim(wPorTel)  +'||';
100400160414           strTRBM=strTRBM+'§§Tag27§§=='+%trim(wPORitOrari)  +'||';
100500140212
100600140212           if Bartmail_insert(strTRBM:indmrabyps:
100700140212                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
100800140612                wTesto =
100900150530                  'Il ritiro è previsto ' + %trim(wRitiroData) +
101000140612                  '  indicativamente ' + %trim(wRitiroOrari);
101100140612
101200140212           else;
101300140212              *in40 = *off;
101400140212              wMessaggio = 'Errore in push-entry in BartMail';
101500140212           endif;
101600140212
101700140212          else;
101800140212            *in40 = *off;
101900140417            wMessaggio = 'Tabella MRA - TRBMBYPSR ALO_AVV non trovata';
102000140212          endif;
102100140212
102200140212       endsr;
102300140213
102400151106
102500151106
102600151106       //-------------------------------------------------------------*
102700151106       //Gestione invio tipo alert CON via EMAIL                      *
102800151106       //-------------------------------------------------------------*
102900151106       Begsr sr_CON_Email;
103000151106
103100151106         clear indmrabyps;
103200151106
103300151106         w§CM1VAR = '*OBJM*BRT - Conferma prenotazione ritiro ' +
103400151106                     %editc(saoPOE:'X')+
103500151106                     %editc(saoNSR:'X')+%editc(saoNOR:'X')+%editc(saoNRV:'X');
103600151106         chain ('MRA':'TRBMBYPSR':'CAO_AVV') tntbe01l;
103700151106
103800151106          if %found(tntbe01l);
103900151106           indmrabyps=tbeuni;
104000151106           exsr sr_trtcm1ds;
104100160414
104200151106
104300151106           strTRBM='§§Tag01§§=='+%trim(oOR902RSO)              +'||';
104400151106           strTRBM=strTRBM+'§§Tag02§§=='+%trim(oOR902DPOR)     +'||';
104500151106           strTRBM=strTRBM+'§§Tag03§§=='+%trim(oOR902RSR)      +'||';
104600151106           strTRBM=strTRBM+'§§Tag04§§=='+%trim(oOR902INR)      +'||';
104700151106           strTRBM=strTRBM+'§§Tag05§§=='+%trim(oOR902CAR)      +'||';
104800151106           strTRBM=strTRBM+'§§Tag06§§=='+%trim(oOR902LOR)      +'||';
104900151106           strTRBM=strTRBM+'§§Tag07§§=='+%trim(oOR902PRR)      +'||';
105000151106           strTRBM=strTRBM+'§§Tag08§§=='+%trim(oOR902NAR)      +'||';
105100151106           strTRBM=strTRBM+'§§Tag09§§=='+%trim(oOR902RFA)      +'||';
105200151106           if oOR902NCL > *zeros;
105300160414              strTRBM=strTRBM+'§§Tag10§§=='+'Colli: ' +
105400160414                               %trim(%char(oOR902NCL)) + '||';
105500151106           else;
105600160414              strTRBM=strTRBM+'§§Tag10§§=='+'Bancali: ' +
105700160414                               %trim(%char(oOR902BNC)) + '||';
105800151106           endif;
105900151106           strTRBM=strTRBM+'§§Tag11§§=='+%trim(%char(oOR902PKG)) +'||';
106000151106           strTRBM=strTRBM+'§§Tag12§§=='+%trim(oOR902NAM)        +'||';
106100151109           strTRBM=strTRBM+'§§Tag13§§=='+%trim(oOR902REF)        +'||';
106200151106           strTRBM=strTRBM+'§§Tag14§§=='+%trim(oOR902TER)        +'||';
106300151106           strTRBM=strTRBM+'§§Tag15§§=='+%trim(oOR902DPAG)       +'||';
106400151106
106500151106           if %trim(oOR902NO1)+%trim(oOR902NO2)<>*blanks;
106600151106              strTRBM=strTRBM+'§§Tag16§§=='+'Note di ritiro: ' +
106700151106                                           %trim(oOR902NO1)    + ' ' +
106800151106                                           %trim(oOR902NO2)    +'||';
106900151106           else;
107000151106              strTRBM=strTRBM+'§§Tag16§§=='+''+'||';
107100151106           endif;
107200151106
107300151106           strTRBM=strTRBM+'§§Tag17§§=='+%trim(wRitiroData)  +'||';
107400151106           strTRBM=strTRBM+'§§Tag18§§=='+%trim(wRitiroOraDa) +'||';
107500151106           strTRBM=strTRBM+'§§Tag19§§=='+%trim(wRitiroOraA)  +'||';
107600151106           strTRBM=strTRBM+'§§Tag20§§=='+%trim(wRitNum)     +'||';
107700160413
107800160413           strTRBM=strTRBM+'§§Tag22§§=='+%trim(wApertOrari)  +'||';
107900160413           strTRBM=strTRBM+'§§Tag23§§=='+%trim(oOR902NPR)    +'||';
108000160413           strTRBM=strTRBM+'§§Tag24§§=='+%trim(oOR902DPOR)   +'||';
108100160414           strTRBM=strTRBM+'§§Tag25§§=='+%trim(oOR902MPOR) +
108200160414                                                   '@brt.it' +'||';
108300160413           strTRBM=strTRBM+'§§Tag26§§=='+%trim(oOR902TPOR)   +'||';
108400160413           strTRBM=strTRBM+'§§Tag27§§=='+%trim(wPORitOrari)  +'||';
108500151106
108600151106
108700151106           if Bartmail_insert(strTRBM:indmrabyps:
108800151106                       intrtcm1ds:0:pEsito)=0 and pEsito=0;
108900151106                wTesto =
109000151106                  'Il ritiro è previsto ' + %trim(wRitiroData) +
109100151106                  '  indicativamente ' + %trim(wRitiroOrari);
109200151106
109300151106           else;
109400151106              *in40 = *off;
109500151106              wMessaggio = 'Errore in push-entry in BartMail';
109600151106           endif;
109700151106
109800151106          else;
109900151106            *in40 = *off;
110000151106            wMessaggio = 'Tabella MRA - TRBMBYPSR CAO_AVV non trovata';
110100151106          endif;
110200151106
110300151106       endsr;
110400140212
110500140212
110600140212       //-------------------------------------------------------------*
110700140417       //Gestione invio tipo alert AVV via SMS                        *
110800140212       //-------------------------------------------------------------*
110900140417       Begsr sr_AVV_SMS;
111000140212
111100140213
111200140213         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
111300140213         ALESInDS_SMS.i_message =
111400150530               %trim(wOrdRagSoc)+' ci ha incaricato di effettuare ritiro da  '+
111500160413               %trim(wRitRsr) + '  ' + %trim(wRitInr) + '  ' + %trim(wRitCar) +
111600160415               '  ' + %trim(wRitLor) + ' (' + %trim(wRitPrr) + ') ' +
111700160413               %subst(%trim(wRitiroData):1:8)+' '+%trim(wRitiroOrariS) + '. ' +
111800170508               'Per eventuali modifiche contatta il Servizio Clienti ' +
111900160414               %trim(wPorTel) +
112000160414               '. Prenotazione n. ' + %trim(wRitPRENOTA);
112100160414
112200140612         wTesto = 'Ritiro previsto ' +
112300150530               %subst(%trim(wRitiroData):1:8)+' '+%trim(wRitiroOrariS) + '. ';
112400140612
112500140212
112600140422         ALESInDS_SMS.i_IdSMS   =
112700140213                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
112800140422                     wAAS +
112900140417                     %editc(wPOE:'X')+%editc(saoNSR:'X')+
113000140417                     %editc(saoNOR:'X')+%editc(saoNRV:'X')     + '_' + 'OAVV';
113100140213
113200140417         ALESInDS_SMS.i_cell    = wRitMobile;
113300140214         if not wFlgTest;
113400140217  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
113500140214         endif;
113600140212
113700140212
113800140213         ALESInOpz = 'W';
113900170323         ALESInPRG = sVGDPRG;
114000170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
114100170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
114200170323                             ALESOutPRG:ALESOutEsito);
114300140212         if %error OR ALESOutEsito <> *blanks;
114400140212            *in40 = *off;
114500140212            wMessaggio = 'KO da driver FNLSALESR';
114600140213         else;
114700140213            wFlgSMSsent = *on;
114800140212         endif;
114900140212
115000140212       endsr;
115100160413
115200160413
115300160413       //-------------------------------------------------------------*
115400160413       //Gestione invio tipo alert CON via SMS                        *
115500160413       //-------------------------------------------------------------*
115600160413       Begsr sr_CON_SMS;
115700160413
115800160413
115900160413         ALESInDS_SMS.i_MittSMS = 'BRT SpA';
116000160413         ALESInDS_SMS.i_message =
116100160413               'Confermiamo ritiro c/o ' + %trim(oOR902RSR) + '  ' +
116200160413                                           %trim(oOR902INR) + '  ' +
116300160413                                           %trim(oOR902CAR) + '  ' +
116400160413                                           %trim(oOR902LOR) + '  ' +
116500160413                                           %trim(oOR902PRR) + '  ' +
116600160413               %subst(%trim(wRitiroData):1:8)+' '+%trim(wRitiroOrariS) + '. ' +
116700170508               'Per eventuali modifiche contatta il Servizio Clienti ' +
116800160414               %trim(oOR902TPOR) +
116900160414               '. Prenotazione n. ' + %trim(oOR902NPR);
117000160413
117100160414         wTesto = 'Conferma ritiro. Previsto ' +
117200160413               %subst(%trim(wRitiroData):1:8)+' '+%trim(wRitiroOrariS) + '. ';
117300160413
117400160413
117500160413         ALESInDS_SMS.i_IdSMS   =
117600160413                     %subst(%char(%dec(%timestamp:*ISO)):1:14) + '_' +
117700160413                     wAAS +
117800160413                     %editc(wPOE:'X')+%editc(saoNSR:'X')+
117900160413                     %editc(saoNOR:'X')+%editc(saoNRV:'X')     + '_' + 'OCON';
118000160413
118100160413         ALESInDS_SMS.i_cell    = %trim(wRitMobile);
118200160413         if not wFlgTest;
118300160413  xxx     //    ALESInDS_SMS.i_cell    = '+393473441969';
118400160413         endif;
118500160413
118600160413
118700160413         ALESInOpz = 'W';
118800170323         ALESInPRG = sVGDPRG;
118900170323         callP(e) FNLSALESR (ALESInChiudi:ALESInOpz:ALESInKSU:ALESInTIP:
119000170323                             ALESInTSC:ALESInPGM:ALESInPRG:ALESInDS_SMS:
119100170323                             ALESOutPRG:ALESOutEsito);
119200160413         if %error OR ALESOutEsito <> *blanks;
119300160413            *in40 = *off;
119400160413            wMessaggio = 'KO da driver FNLSALESR';
119500160413         else;
119600160413            wFlgSMSsent = *on;
119700160413         endif;
119800160413
119900160413       endsr;
120000140213
120100140212
120200140417
120300140212
120400140212       //-------------------------------------------------------------*
120500140212       //Valorizzazione trtcm1ds                                      *
120600140212       //-------------------------------------------------------------*
120700140212        Begsr sr_trtcm1ds;
120800140212
120900140212           clear intrtcm1ds;
121000140417           wPOR_A = %editc(wPOR:'X');
121100140212           if %scan('*PO':§mraemlmit) > *zeros;
121200140417              string=%replace(wPOR_A:§mraemlmit:
121300140212                              %scan('*PO':§mraemlmit:1):3);
121400140212           endif;
121500140212           exsr sr_chkstr;
121600140212           §CM1MITT=string;
121700140417           string=wRitEmail;
121800140214           if not wFlgTest;
121900171006  xxx       //      string='email_di_test@brt.it';
122000140214           endif;
122100140212           exsr sr_chkstr;
122200140212           §CM1DST=string;
122300140212           §CM1TIPS=§mrareg;
122400140212           if §mrapo<> '*PO';
122500140212              §CM1PO=§mrapo;
122600140212           else;
122700140417              §CM1PO=%editc(ormPOR:'X');
122800140212           endif;
122900140212           §CM1VAR=w§CM1VAR;
123000140212           §CM1STS='0';
123100140212           §CM1IDP=§mraidpro;
123200140212           §CM1PROG=w§CM1PROG;
123300140212           §CM1ATT='01';
123400140212           §CM1TOTATT='01';
123500140212
123600140212        endsr;
123700140212
123800140212
123900140212       //-------------------------------------------------------------*
124000140212       //controllo stringa                                            *
124100140212       //-------------------------------------------------------------*
124200140212        Begsr sr_chkstr;
124300140212           string=%xlate('||':'  ':string);
124400140212           string=%xlate('==':'  ':string);
124500140212        endsr;
124600140212
124700140212
124800140212       //-------------------------------------------------------------*
124900140212       //Invio email di alert                                         *
125000140212       //-------------------------------------------------------------*
125100140214       Begsr snd_Alert;
125200140212
125300140212          wEml   = 'cedalert@brt.it';
125400140212          wOgg   = 'Anomalia invio alert destinatari ' + w§CM1PROG;
125500140214          wMsg   = ' ID BartMail   '     + w§CM1PROG             + ':/N' +
125600140417                   ' saoPOE        '     + %editc(saoPOE:'X')    + ':/N' +
125700140417                   ' saoNSR        '     + %editc(saoNSR:'X')    + ':/N' +
125800140417                   ' saoNOR        '     + %editc(saoNOR:'X')    + ':/N' +
125900140417                   ' saoNRV        '     + %editc(saoNRV:'X')    + ':/N' +
126000140417                   ' saoTIPA       '     + %trim(saoTIPA)        + ':/N' +
126100140417                   ' Email         '     + %trim(wRitEmail)      + ':/N' +
126200140417                   ' Mobile        '     + %trim(wRitMobile)     + ':/N' +
126300140214                   ' Errore        '     + wEsito+' '
126400140214                                         + %trim(wMessaggio) + ':/N';
126500140212
126600140212          callP(e) TIS701C1 (wEml : wCcEml : wOgg : wMsg : wSndSts);
126700140212
126800140212        endsr;
126900140213
127000140213
127100140213       //-------------------------------------------------------------*
127200140213       //inserimento Richiesta Assistenza                             *
127300140213       //-------------------------------------------------------------*
127400140213        Begsr sr_InsRA;
127500140213
127600140213           *in50 = *off;
127700140213
127800140414           clear fidna6ds;
127900140213           select;
128000140417             when saoTIPA = 'AVV';
128100140417                iDA06MAD = ' 83';
128200140417                iDA06NO1 = 'Alert inviato a: ';
128300151106             when saoTIPA = 'CON';
128400151106                iDA06MAD = ' 85';
128500151106                iDA06NO1 = 'Alert inviato a: ';
128600140213             other;
128700140417                *in50 = *on;
128800140213           endsl;
128900140908
129000140908           select;
129100140908             when wTipoInvio = 'S';
129200140908                  IDA06RSC = 'SMS';
129300140908             when wTipoInvio = 'M';
129400140908                  IDA06RSC = 'EMAIL';
129500140908             when wTipoInvio = 'B';
129600140908                  IDA06RSC = 'SMS/EMAIL';
129700140908           endsl;
129800140213
129900140520           iDA06TOR = 'O';
130000140520           iDA06OGG = %trim(%editc(ormPOE:'X')) +
130100140520                      %trim(%editc(ormNSR:'X')) +
130200140520                      %trim(%editc(ormNOR:'X')) +
130300140520                      %trim(%editc(ormNRV:'X'));
130400140414           iDA06UTE = 'QPGMR';
130500140520           iDA06FIL = ormPOE;
130600140612           iDA06NO1 = %trim(iDA06NO1) + ' ' + %trim(wDescRA) +
130700140612                      '  -  ' + wTesto;
130800140213
130900140213           if not *in50;
131000140414              callP(e) FIDNA6R (fidna6ds);
131100140213              if %error;
131200140213                 *in40 = *off;
131300140414                 wMessaggio = 'KO da driver FIDNA6R';
131400140213              endif;
131500140213           endif;
131600140213
131700140213        endsr;
131800140212
131900140212
132000140212       //-------------------------------------------------------------*
132100140212       //reperimento progressivo                                      *
132200140212       //-------------------------------------------------------------*
132300140212        Begsr sr_rtvPrg;
132400140212
132500140212           clear trul33ds;
132600140212           w§CM1PROG=*all'0';
132700140212           i33Cnu = 24;
132800140212           i33num = 1;
132900140212           kpjbu = trul33ds;
133000140212           trul33r(kpjba);
133100140212           trul33ds = kpjbu;
133200140212           w§CM1PROG=%subst(%editc(o33nri:'X'):9:7);
133300140212
133400140212        endsr;
133500140212
133600140210
