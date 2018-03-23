000100121123       //==============================================================
000200121123       //?Dimensione media del collo su VDL.                           ?
000300121123       //==============================================================
000400121123
000500121123       //--------------------------------------------------------------
000600121123       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121123       //--------------------------------------------------------------
000800121123
000900121123     /*PRM  dbgview(*source)
001000121123     /*PRM  commit(*none)
001100121123     /*CMD  ovrdbf file(FLTR700F) tofile(FILTRAPRD/FLTR700F) +
001200121123     /*CMD         ovrscope(*calllvl)
001300121123     /*END  dltovr file(FLTR700F) lvl(*)
001400121123     /*END
001500121123
001600121123       //--------------------------------------------------------------
001700121123       //?Specifiche di controllo.                                     ?
001800121123       //--------------------------------------------------------------
001900121123
002000121123     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002100121123     h dftactgrp(*no)
002200121123     h alwnull(*inputonly)
002300121123
002400121123       //--------------------------------------------------------------
002500121123       //?Dichiarazione file.                                          ?
002600121123       //--------------------------------------------------------------
002700121123
002800121123       // -?Organigramma?
002900121123     fAZORG01L  if   e           k disk
003000121123
003100121123       // -?Video?
003200121123     fTIST01D   cf   e             workstn
003300121123     f                                     indds(IndDspF)
003400121123     f                                     infds(InfDspF)
003500121123
003600121123       //--------------------------------------------------------------
003700121123       //?Definizione costanti.                                        ?
003800121123       //--------------------------------------------------------------
003900121123
004000121123       // -?Costante per controllo "caratteri solo numerici"?
004100121123     d c_Digits        c                   const('0123456789')
004200121123
004300121123       // -?Tasti funzionali a video?
004400121123     d c_F01           c                   const(x'31')
004500121123     d c_F02           c                   const(x'32')
004600121123     d c_F03           c                   const(x'33')
004700121123     d c_F04           c                   const(x'34')
004800121123     d c_F05           c                   const(x'35')
004900121123     d c_F06           c                   const(x'36')
005000121123     d c_F07           c                   const(x'37')
005100121123     d c_F08           c                   const(x'38')
005200121123     d c_F09           c                   const(x'39')
005300121123     d c_F10           c                   const(x'3A')
005400121123     d c_F11           c                   const(x'3B')
005500121123     d c_F12           c                   const(x'3C')
005600121123     d c_F13           c                   const(x'B1')
005700121123     d c_F14           c                   const(x'B2')
005800121123     d c_F15           c                   const(x'B3')
005900121123     d c_F16           c                   const(x'B4')
006000121123     d c_F17           c                   const(x'B5')
006100121123     d c_F18           c                   const(x'B6')
006200121123     d c_F19           c                   const(x'B7')
006300121123     d c_F20           c                   const(x'B8')
006400121123     d c_F21           c                   const(x'B9')
006500121123     d c_F22           c                   const(x'BA')
006600121123     d c_F23           c                   const(x'BB')
006700121123     d c_F24           c                   const(x'BC')
006800121123     d c_Enter         c                   const(x'F1')
006900121123     d c_RollDown      c                   const(x'F4')
007000121123     d c_RollUp        c                   const(x'F5')
007100121123
007200121123       //--------------------------------------------------------------
007300121123       //?Definizione schiere.                                         ?
007400121123       //--------------------------------------------------------------
007500121123
007600121123       // -?Messaggi di errore?
007700121123     d sk_Msg          s             78    dim( 7)  ctdata  perrcd(1)
007800121123
007900121123       // -?Dimensioni numeriche di un singolo collo
008000121123     d sk_Dim          s              4  3 dim( 3)  inz
008100121123
008200121123       //--------------------------------------------------------------
008300121123       //?Definizione aree dati.                                       ?
008400121123       //--------------------------------------------------------------
008500121123
008600121123       // -?Dati utente?
008700121123     d §AzUte        e ds                  extname(AZUTE00F)
008800121123     d                                     dtaara
008900121123     d §DatiUte      e ds                  extname(dDatiUte)
009000121123     d                                     dtaara
009100121123
009200121123       //--------------------------------------------------------------
009300121123       //?Definizione strutture dati.                                  ?
009400121123       //--------------------------------------------------------------
009500121123
009600121123       // -?Status ds?
009700121123     d Status         sds
009800121123     d   SDSpgm          *proc
009900121123
010000121123       // -?InfDS?
010100121123     d InfDspF         ds
010200121123     d   dsp_aid             369    369a
010300121123
010400121123       // -?Indicatori su DspF?
010500121123     d IndDspF         ds                  inz
010600121123         // -?Abilitazione tasti funzionali?
010700121123     d   F6Attivo                      n   overlay(IndDspF : 06)
010800121123     d   F12Attivo                     n   overlay(IndDspF : 12)
010900121123         // -?Emissione messaggio di errore?
011000121123     d   ErrMessage                    n   overlay(IndDspF : 28)
011100121123         // -?Indicatori per Attibuti di visualizzazione?
011200121123         // -?Posizionamento cursore & segnalazione errore?
011300121123     d   PosCurFIL                     n   overlay(IndDspF : 51)
011400121123     d   PosCurNPG                     n   overlay(IndDspF : 52)
011500121123     d   PosCurDLI                     n   overlay(IndDspF : 53)
011600121123     d   PosCurDLF                     n   overlay(IndDspF : 54)
011700121123         //   -?Riemissione videata?
011800121123     d   ErrGenerico                   n   overlay(IndDspF : 99)
011900121123
012000121123       // -?Parametri ricevuti?
012100121123     d KPJBA         e ds
012200121123
012300121123       // -?Dati estratti via SQL?
012400121123     d FLTR700F      e ds                  based(nullPtr)
012500121123     d                                     qualified
012600121123     d ds_SQL          ds                  inz
012700121123     d   sql_PD4                           inz  like(FLTR700F.TR7pd4)
012800121123
012900121123       // -?Barcode letto da Volume?
013000121123     d TR7pd4_ds       ds            30    inz  qualified
013100121123     d   TR7dim_null1                11    inz
013200121123     d   TR7dim1                      4    inz                                  12-15
013300121123     d   TR7dim2                      4    inz                                  16-19
013400121123     d   TR7dim3                      4    inz                                  20-23
013500121123     d   TR7dim_null2                 7    inz
013600121123
013700121123       //--------------------------------------------------------------
013800121123       //?Definizione variabili globali.                               ?
013900121123       //--------------------------------------------------------------
014000121123
014100121123       // -?Flags booleani?
014200121123     d $Fine           s               n   inz
014300121123     d $InzD01         s               n   inz(*on)
014400121123     d $InzD02         s               n   inz(*on)
014500121123     d $EoF            s               n   inz
014600121123
014700121123       // -?Variabili per la gestione del video?
014800121123     d $Video          s              2    inz('D1')
014900121123
015000121123       // -?Stringa SQL da eseguire?
015100121123     d wSql            s           4096    inz  varying
015200121123
015300121123       // -?Campi data?
015400121123     d wDate           s              8  0 inz
015500121123     d wDate_Iso       s               d   inz  datfmt(*iso)
015600121123
015700121123       // -?Campi di comodo?
015800121123     d W1Ctli          s                   inz  like(FLTR700F.TR7tim)
015900121123     d W1Ctlf          s                   inz  like(FLTR700F.TR7tim)
016000121123     d W2Cdla          s              9  3 inz
016100121123     d W2Cdlb          s              9  3 inz
016200121123     d W2Cdlc          s              9  3 inz
016300121123
016400121123       //--------------------------------------------------------------
016500121123       //?Definizione prototipi procedure.                             ?
016600121123       //--------------------------------------------------------------
016700121123
016800121123       // -?Reperimento dati utente?
016900121123     d TIBS34ds      e ds
017000121123      /copy gaitrasrc/srcProtoPR,TIBS34R
017100121123
017200121123       // -?Controllo ed inversione date?
017300121123     d WLBdat          ds                  inz
017400121123     d  G08dat                        8  0 inz
017500121123     d  G08inv                        8  0 inz
017600121123     d  G08err                        1    inz('3')
017700121123     d  G08tgi                        5  0 inz
017800121123      /copy gaitrasrc/srcProtoPr,XSRDA8
017900121123
018000121123       //--------------------------------------------------------------
018100121123       //?Definizione key-list.                                        ?
018200121123       //--------------------------------------------------------------
018300121123
018400121123
018500121123       //--------------------------------------------------------------
018600121123       //?M A I N - L I N E                                            ?
018700121123       //--------------------------------------------------------------
018800121123
018900121123     c     *Entry        plist
019000121123     c                   parm                    KPJBA
019100121123
019200121123      /free
019300121123
019400121123       // -?Operazioni iniziali?
019500121123       exsr  sr_RoutInz;
019600121123
019700121123       // -?Ciclo di gestione del file video?
019800121123       DOW  $Fine = *off;
019900121123
020000121123         select;
020100121123
020200121123           // -?Gestione 1ª videata?
020300121123           when $Video = 'D1';
020400121123             exsr  sr_GesD01;
020500121123
020600121123           // -?Gestione 2ª videata?
020700121123           when $Video = 'D2';
020800121123             exsr  sr_GesD02;
020900121123
021000121123           // -?? ? ??
021100121123           other;
021200121123             $Fine = *on;
021300121123
021400121123         endsl;
021500121123
021600121123       ENDDO;
021700121123
021800121123       // -?Operazioni finali?
021900121123       exsr  sr_RoutEnd;
022000121123
022100121123       //--------------------------------------------------------------
022200121123       //?Operazioni iniziali.                                         ?
022300121123       //--------------------------------------------------------------
022400121123       BEGSR  sr_RoutInz;
022500121123
022600121123         // -?Reperimento dati job?
022700121123         exsr  sr_DatiJob;
022800121123
022900121123         // -?Impostazione nome programma a video?
023000121123         V1Tpgm = SDSpgm;
023100121123
023200121123         // -?Reperimento data odierna?
023300121123         wDate = %int( %subst( %char( %dec( %timestamp() ) )
023400121123                               : 1 : 8 ) );
023500121123
023600121123       ENDSR;
023700121123
023800121123       //--------------------------------------------------------------
023900121123       //?Reperimento Dati del job (Utente/Operativi).                 ?
024000121123       //--------------------------------------------------------------
024100121123       BEGSR  sr_DatiJob;
024200121123
024300121123         in(E) §AzUte;
024400121123         if NOT %error;
024500121123           in(E) §DatiUte;
024600121123         endif;
024700121123         if %error or RSut = *blanks;
024800121123           clear TIBS34ds;
024900121123           tibs34r ( tibs34ds );
025000121123           in §AzUte;
025100121123           in §DatiUte;
025200121123         endif;
025300121123
025400121123       ENDSR;
025500121123
025600121123       //--------------------------------------------------------------
025700121123       //?Gestione videata D01.                                        ?
025800121123       //--------------------------------------------------------------
025900121123       BEGSR  sr_GesD01;
026000121123
026100121123         // -?Inizializzazione videata?
026200121123         if  $InzD01 = *on;
026300121123           exsr  sr_InzD01;
026400121123           $InzD01 = *off;
026500121123         endif;
026600121123
026700121123         // -?(Dis)Abilitazione tasti funzionali?
026800121123         F6Attivo  = *on;
026900121123         F12Attivo = *off;
027000121123
027100121123         // -?Emissione videata?
027200121123         write  ST01T01;
027300121123         write  ST01P01;
027400121123         exfmt  ST01D01;
027500121123
027600121123         clear  VIDmsg;
027700121123         reset  ErrMessage;
027800121123         reset  ErrGenerico;
027900121123
028000121123         SELECT;
028100121123
028200121123           // -?F3=Fine?
028300121123           WHEN  dsp_aid = c_F03;
028400121123             $Fine = *on;
028500121123
028600121123           // -?Invio, F6=Conferma?
028700121123           OTHER;
028800121123             exsr  sr_CtrD01;
028900121123             if  ErrGenerico = *off  and  dsp_aid = c_F06;
029000121123               $Video    = 'D2';
029100121123               $InzD02   = *on;
029200121123             endif;
029300121123
029400121123         ENDSL;
029500121123
029600121123       ENDSR;
029700121123
029800121123       //--------------------------------------------------------------
029900121123       //?Inizializzazione videata D01.                                ?
030000121123       //--------------------------------------------------------------
030100121123       BEGSR  sr_InzD01;
030200121123
030300121123         clear  ST01D01;
030400121123
030500121123       ENDSR;
030600121123
030700121123       //--------------------------------------------------------------
030800121123       //?Controllo dati immessi nella videata D01.                    ?
030900121123       //--------------------------------------------------------------
031000121123       BEGSR  sr_CtrD01;
031100121123
031200121123         // -?Spegnimento indicatori di posizionamento cursore?
031300121123         %subst(IndDspF : 28) = *off;
031400121123
031500121123         // -?Controllo filiale?
031600121123         clear  V1Dfil;
031700121123         if  V1Cfil <> *zero;
031800121123           chain  ( V1Cfil )  AZORG;
031900121123           if  %found(AZORG01L);
032000121123             V1Dfil = ORGdes;
032100121123           else;
032200121123             V1Dfil = *all'? ';
032300121123             ErrGenerico = *on;
032400121123             ErrMessage  = *on;
032500121123             PosCurFIL   = *on;
032600121123             VIDmsg = sk_Msg(01);
032700121123             leavesr;
032800121123           endif;
032900121123         endif;
033000121123
033100121123         // -?Controllo tipo turno?
033200121123         if  V1Cnpg <> *blank  and  V1Cnpg <> 'P'  and
033300121123             V1Cnpg <> '2'     and  V1Cnpg <> '5';
033400121123           ErrGenerico = *on;
033500121123           ErrMessage  = *on;
033600121123           PosCurNPG   = *on;
033700121123           VIDmsg = sk_Msg(02);
033800121123           leavesr;
033900121123         endif;
034000121123
034100121123         // -?Controllo data lettura iniziale?
034200121123         clear  WLBdat;
034300121123         if  V1Cdli = *zero;
034400121123           ErrGenerico = *on;
034500121123           ErrMessage  = *on;
034600121123           PosCurDLI   = *on;
034700121123           VIDmsg = sk_Msg(03);
034800121123           leavesr;
034900121123         else;
035000121123           G08dat = V1Cdli;
035100121123           xsrda8 ( WLBdat );
035200121123           if  G08err =  *on;
035300121123             ErrGenerico = *on;
035400121123             ErrMessage  = *on;
035500121123             PosCurDLI   = *on;
035600121123             VIDmsg = sk_Msg(04);
035700121123             leavesr;
035800121123           endif;
035900121123           V1Cdli = G08dat;
036000121123         endif;
036100121123         W1Ctli = %editc( G08inv : 'X' ) + '000000';
036200121123
036300121123         // ·?Data lettura iniziale precedente di oltre 14 gg quella attuale?
036400121123         if  G08inv < %dec( %date( wDate : *iso ) - %days(14) );
036500121123           ErrGenerico = *on;
036600121123           ErrMessage  = *on;
036700121123           PosCurDLI   = *on;
036800121123           VIDmsg = sk_Msg(05);
036900121123           leavesr;
037000121123         endif;
037100121123
037200121123         // -?Controllo data lettura finale?
037300121123         clear  WLBdat;
037400121123         if  V1Cdlf = *zero;
037500121123           ErrGenerico = *on;
037600121123           ErrMessage  = *on;
037700121123           PosCurDLF   = *on;
037800121123           VIDmsg = sk_Msg(03);
037900121123           leavesr;
038000121123         else;
038100121123           G08dat = V1Cdlf;
038200121123           xsrda8 ( WLBdat );
038300121123           if  G08err =  *on;
038400121123             ErrGenerico = *on;
038500121123             ErrMessage  = *on;
038600121123             PosCurDLF   = *on;
038700121123             VIDmsg = sk_Msg(04);
038800121123             leavesr;
038900121123           endif;
039000121123           V1Cdlf = G08dat;
039100121123         endif;
039200121123         W1Ctlf = %editc( G08inv : 'X' ) + '999999';
039300121123
039400121123         // -?Controllo range date lettura?
039500121123         if  W1Ctli > W1Ctlf;
039600121123           ErrGenerico = *on;
039700121123           ErrMessage  = *on;
039800121123           PosCurDLI   = *on;
039900121123           VIDmsg = sk_Msg(06);
040000121123           leavesr;
040100121123         endif;
040200121123
040300121123       ENDSR;
040400121123
040500121123       //--------------------------------------------------------------
040600121123       //?Gestione videata D02.                                        ?
040700121123       //--------------------------------------------------------------
040800121123       BEGSR  sr_GesD02;
040900121123
041000121123         // -?Inizializzazione videata?
041100121123         if  $InzD02 = *on;
041200121123           exsr  sr_InzD02;
041300121123           $InzD02 = *off;
041400121123         endif;
041500121123
041600121123         // -?(Dis)Abilitazione tasti funzionali?
041700121123         F6attivo  = *off;
041800121123         F12attivo = *on;
041900121123
042000121123         // -?Emissione Testata e Piede con tasti funzionali abilitati?
042100121123         if  Not  ErrMessage;
042200121123           write  ST01T01;
042300121123           write  ST01D01;
042400121123         endif;
042500121123         write  ST01P01;
042600121123         write  Protect;
042700121123
042800121123         // -?Emissione videata?
042900121123         exfmt  ST01D02;
043000121123
043100121123         clear  VIDmsg;
043200121123         reset  ErrMessage;
043300121123         reset  ErrGenerico;
043400121123
043500121123         SELECT;
043600121123
043700121123           // -?F3=Fine?
043800121123           WHEN  dsp_aid = c_F03;
043900121123             $Fine = *on;
044000121123
044100121123           // -?F12=Ritorno?
044200121123           WHEN  dsp_aid = c_F12;
044300121123             $Video = 'D1';
044400121123
044500121123         ENDSL;
044600121123
044700121123       ENDSR;
044800121123
044900121123       //--------------------------------------------------------------
045000121123       //?Inizializzazione videata D02.                                ?
045100121123       //--------------------------------------------------------------
045200121123       BEGSR  sr_InzD02;
045300121123
045400121123         // -?Pulizia videata?
045500121123         clear  ST01D02;
045600121123
045700121123         // -?Compilazione stringa SQL?
045800121123         wSql = 'select TR7pd4 from ';
045900121123         if  %subst( KNSIF : 7 : 1 ) = 'P';
046000121123           wSQL += 'FILTRAPRD';
046100121123         else;
046200121123           wSQL += 'FILTRA201';
046300121123         endif;
046400121123         wSql += '/FLTR700F where TR7pd4 <> ''''';
046500121123
046600121123         IF  V1Cfil <> *zero  or  V1Cnpg <> *blank  or
046700121123             V1Cdli <> *zero  or  V1Cdlf <> *zero;
046800121123
046900121123           if  V1Cfil <> *zero;
047000121123             wSql  += ' and TR7fil = ''' + %editc( V1Cfil : 'X' ) + '''';
047100121123           endif;
047200121123
047300121123           if  V1Cnpg <> *blank;
047400121123             select;
047500121127               when  V1Cnpg = '2'  or  V1Cnpg = '5';
047600121127                 wSql += ' and TR7ctl = ''' + V1Cnpg + ''' +
047700121127                           and TR7020 = ''20''';
047800121123               when  V1Cnpg = 'P';
047900121127                 wSql += ' and TR7ctl = ''2'' +
048000121127                           and TR7020 = ''23''';
048100121123             endsl;
048200121126           else;
048300121126             wSql  += ' and ((TR7ctl in (''2'', ''5'') and +
048400121127                              TR7020 in (''20'', ''23''))';
048500121123           endif;
048600121123
048700121123           if  V1Cdli <> *zero  or  V1Cdlf <> *zero;
048800121123             wSql  += ' and TR7tim between ''' + W1Ctli +
048900121123                                   ''' and ''' + W1Ctlf + '''';
049000121123           endif;
049100121123
049200121123         ENDIF;
049300121123
049400121123         clear  $EoF;
049500121126         clear  W2Cdla;
049600121126         clear  W2Cdlb;
049700121126         clear  W2Cdlc;
049800121123
049900121123         // -?Apertura cursore?
050000121123         exsr  sr_OpenCursor;
050100121123
050200121123         // -?Lettura cursore?
050300121123         DoW  Not $EoF;
050400121123           exsr  sr_ReadCursor;
050500121123         EndDo;
050600121123
050700121123         // -?Chiusura cursore?
050800121123         exsr  sr_CloseCursor;
050900121123
051000121123       ENDSR;
051100121123
051200121123       //--------------------------------------------------------------
051300121123       //?Apertura cursore C1.                                         ?
051400121123       //--------------------------------------------------------------
051500121123       BEGSR  sr_OpenCursor;
051600121123
051700121123         // -?Dichiarazione cursore?
051800121123         exec sql   prepare S1   from :wSQL;
051900121123         exec sql   declare C1   cursor for S1;
052000121123
052100121123         // -?Apertura del cursore?
052200121123         exec sql   open C1;
052300121123
052400121123         if  SQLcode < *zero;
052500121123           $EoF = *on;
052600121123           ErrGenerico = *on;
052700121123           ErrMessage  = *on;
052800121123           VIDmsg = sk_Msg(07);
052900121123         endif;
053000121123
053100121123       ENDSR;
053200121123
053300121123       //--------------------------------------------------------------
053400121123       //?Lettura cursore C1.                                          ?
053500121123       //--------------------------------------------------------------
053600121123       BEGSR  sr_ReadCursor;
053700121123
053800121123         clear  ds_SQL;
053900121123
054000121123         exec sql   fetch next   from C1   into :ds_SQL;
054100121123
054200121123         select;
054300121123           when  SQLcode = 100;
054400121123             $EoF = *on;
054500121126             if  V2Cncl <> *zero;
054600121126               eval(h) V2Cdla = W2Cdla / V2Cncl;
054700121126               eval(h) V2Cdlb = W2Cdlb / V2Cncl;
054800121126               eval(h) V2Cdlc = W2Cdlc / V2Cncl;
054900121126             endif;
055000121123           when  SQLcode < *zero;
055100121123             $Eof = *on;
055200121123             ErrGenerico = *on;
055300121123             ErrMessage  = *on;
055400121123             VIDmsg = sk_Msg(07);
055500121123           other;
055600121123             exsr  sr_Somma;
055700121123         endsl;
055800121123
055900121123       ENDSR;
056000121123
056100121123       //--------------------------------------------------------------
056200121123       //?Chiusura cursore C1.                                         ?
056300121123       //--------------------------------------------------------------
056400121123       BEGSR  sr_CloseCursor;
056500121123
056600121123         // -?Chiusura del cursore?
056700121123         exec sql   close C1;
056800121123
056900121123       ENDSR;
057000121123
057100121123       //--------------------------------------------------------------
057200121123       //?Somma delle dimensioni e del numero dei colli.               ?
057300121123       //--------------------------------------------------------------
057400121123       BEGSR  sr_Somma;
057500121123
057600121123         // -?Barcode letto da Volume?
057700121123         TR7pd4_ds = sql_PD4;
057800121123         clear  sk_Dim;
057900121123
058000121123         // -?Selezione delle sole letture con dimensioni?
058100121123         if  %check( c_Digits : TR7pd4_ds.TR7dim1 ) > *zero  or
058200121123             %check( c_Digits : TR7pd4_ds.TR7dim2 ) > *zero  or
058300121123             %check( c_Digits : TR7pd4_ds.TR7dim3 ) > *zero;
058400121123           leavesr;
058500121123         endif;
058600121123         if  TR7pd4_ds.TR7dim1 = *zero  and
058700121123             TR7pd4_ds.TR7dim2 = *zero  and
058800121123             TR7pd4_ds.TR7dim3 = *zero;
058900121123           leavesr;
059000121123         endif;
059100121123
059200121123         // -?Somma delle dimensioni?
059300121123         sk_Dim(1) = %int( TR7pd4_ds.TR7dim1 ) / 1000;
059400121123         sk_Dim(2) = %int( TR7pd4_ds.TR7dim2 ) / 1000;
059500121123         sk_Dim(3) = %int( TR7pd4_ds.TR7dim3 ) / 1000;
059600121123         sorta  sk_Dim;
059700121123         W2Cdla += sk_Dim(3);
059800121123         W2Cdlb += sk_Dim(2);
059900121123         W2Cdlc += sk_Dim(1);
060000121123
060100121123         // -?Incremento del numero colli?
060200121123         V2Cncl += 1;
060300121123
060400121123       ENDSR;
060500121123
060600121123       //--------------------------------------------------------------
060700121123       //?Operazioni finali.                                           ?
060800121123       //--------------------------------------------------------------
060900121123       BEGSR  sr_RoutEnd;
061000121123
061100121123         // -?Uscita dal *pgm?
061200121123         return;
061300121123
061400121123       ENDSR;
061500121123
061600121123      /end-free
061700121123
061800121123       //--------------------------------------------------------------
061900121123       //?Schiere a tempo di compilazione.                             ?
062000121123       //--------------------------------------------------------------
062100121123
062200121123** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
062300121123Codice filiale errato                                                          1
062400121123Tipo turno errato                                                              2
062500121123Data lettura obbligatoria                                                      3
062600121123Data lettura formalmente errata                                                4
062700121123La data lettura iniziale NON può precedere di oltre 2 settimane quella attuale 5
062800121123Data lettura iniziale successiva a quella finale                               6
062900121123Rilevato errore SQL: contattare il CED                                         7
