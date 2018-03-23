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
007700170904     d sk_Msg          s             78    dim( 8)  ctdata  perrcd(1)
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
011700170904     d   PosCurOLI                     n   overlay(IndDspF : 55)
011800170904     d   PosCurOLF                     n   overlay(IndDspF : 56)
011900121123         //   -?Riemissione videata?
012000121123     d   ErrGenerico                   n   overlay(IndDspF : 99)
012100121123
012200121123       // -?Parametri ricevuti?
012300121123     d KPJBA         e ds
012400121123
012500121123       // -?Dati estratti via SQL?
012600121123     d FLTR700F      e ds                  based(nullPtr)
012700121123     d                                     qualified
012800121123     d ds_SQL          ds                  inz
012900121123     d   sql_PD4                           inz  like(FLTR700F.TR7pd4)
013000121123
013100121123       // -?Barcode letto da Volume?
013200121123     d TR7pd4_ds       ds            30    inz  qualified
013300121123     d   TR7dim_null1                11    inz
013400121123     d   TR7dim1                      4    inz                                  12-15
013500121123     d   TR7dim2                      4    inz                                  16-19
013600121123     d   TR7dim3                      4    inz                                  20-23
013700121123     d   TR7dim_null2                 7    inz
013800170904
013900170904       // -?Orari a video?
014000170904     d wHhMm_ds        ds             4    qualified  inz
014100170904     d   wHH                          2s 0 inz
014200170904     d   wMM                          2s 0 inz
014300121123
014400121123       //--------------------------------------------------------------
014500121123       //?Definizione variabili globali.                               ?
014600121123       //--------------------------------------------------------------
014700121123
014800121123       // -?Flags booleani?
014900121123     d $Fine           s               n   inz
015000121123     d $InzD01         s               n   inz(*on)
015100121123     d $InzD02         s               n   inz(*on)
015200121123     d $EoF            s               n   inz
015300121123
015400121123       // -?Variabili per la gestione del video?
015500121123     d $Video          s              2    inz('D1')
015600121123
015700121123       // -?Stringa SQL da eseguire?
015800121123     d wSql            s           4096    inz  varying
015900121123
016000121123       // -?Campi data?
016100121123     d wDate           s              8  0 inz
016200121123     d wDate_Iso       s               d   inz  datfmt(*iso)
016300121123
016400121123       // -?Campi di comodo?
016500121123     d W1Ctli          s                   inz  like(FLTR700F.TR7tim)
016600121123     d W1Ctlf          s                   inz  like(FLTR700F.TR7tim)
016700121123     d W2Cdla          s              9  3 inz
016800121123     d W2Cdlb          s              9  3 inz
016900121123     d W2Cdlc          s              9  3 inz
017000121123
017100121123       //--------------------------------------------------------------
017200121123       //?Definizione prototipi procedure.                             ?
017300121123       //--------------------------------------------------------------
017400121123
017500121123       // -?Reperimento dati utente?
017600121123     d TIBS34ds      e ds
017700121123      /copy gaitrasrc/srcProtoPR,TIBS34R
017800121123
017900121123       // -?Controllo ed inversione date?
018000121123     d WLBdat          ds                  inz
018100121123     d  G08dat                        8  0 inz
018200121123     d  G08inv                        8  0 inz
018300121123     d  G08err                        1    inz('3')
018400121123     d  G08tgi                        5  0 inz
018500121123      /copy gaitrasrc/srcProtoPr,XSRDA8
018600121123
018700121123       //--------------------------------------------------------------
018800121123       //?Definizione key-list.                                        ?
018900121123       //--------------------------------------------------------------
019000121123
019100121123
019200121123       //--------------------------------------------------------------
019300121123       //?M A I N - L I N E                                            ?
019400121123       //--------------------------------------------------------------
019500121123
019600121123     c     *Entry        plist
019700121123     c                   parm                    KPJBA
019800121123
019900121123      /free
020000121123
020100121123       // -?Operazioni iniziali?
020200121123       exsr  sr_RoutInz;
020300121123
020400121123       // -?Ciclo di gestione del file video?
020500121123       DOW  $Fine = *off;
020600121123
020700121123         select;
020800121123
020900121123           // -?Gestione 1ª videata?
021000121123           when $Video = 'D1';
021100121123             exsr  sr_GesD01;
021200121123
021300121123           // -?Gestione 2ª videata?
021400121123           when $Video = 'D2';
021500121123             exsr  sr_GesD02;
021600121123
021700121123           // -?? ? ??
021800121123           other;
021900121123             $Fine = *on;
022000121123
022100121123         endsl;
022200121123
022300121123       ENDDO;
022400121123
022500121123       // -?Operazioni finali?
022600121123       exsr  sr_RoutEnd;
022700121123
022800121123       //--------------------------------------------------------------
022900121123       //?Operazioni iniziali.                                         ?
023000121123       //--------------------------------------------------------------
023100121123       BEGSR  sr_RoutInz;
023200121123
023300121123         // -?Reperimento dati job?
023400121123         exsr  sr_DatiJob;
023500121123
023600121123         // -?Impostazione nome programma a video?
023700121123         V1Tpgm = SDSpgm;
023800121123
023900121123         // -?Reperimento data odierna?
024000121123         wDate = %int( %subst( %char( %dec( %timestamp() ) )
024100121123                               : 1 : 8 ) );
024200121123
024300121123       ENDSR;
024400121123
024500121123       //--------------------------------------------------------------
024600121123       //?Reperimento Dati del job (Utente/Operativi).                 ?
024700121123       //--------------------------------------------------------------
024800121123       BEGSR  sr_DatiJob;
024900121123
025000121123         in(E) §AzUte;
025100121123         if NOT %error;
025200121123           in(E) §DatiUte;
025300121123         endif;
025400121123         if %error or RSut = *blanks;
025500121123           clear TIBS34ds;
025600121123           tibs34r ( tibs34ds );
025700121123           in §AzUte;
025800121123           in §DatiUte;
025900121123         endif;
026000121123
026100121123       ENDSR;
026200121123
026300121123       //--------------------------------------------------------------
026400121123       //?Gestione videata D01.                                        ?
026500121123       //--------------------------------------------------------------
026600121123       BEGSR  sr_GesD01;
026700121123
026800121123         // -?Inizializzazione videata?
026900121123         if  $InzD01 = *on;
027000121123           exsr  sr_InzD01;
027100121123           $InzD01 = *off;
027200121123         endif;
027300121123
027400121123         // -?(Dis)Abilitazione tasti funzionali?
027500121123         F6Attivo  = *on;
027600121123         F12Attivo = *off;
027700121123
027800121123         // -?Emissione videata?
027900121123         write  ST01T01;
028000121123         write  ST01P01;
028100121123         exfmt  ST01D01;
028200121123
028300121123         clear  VIDmsg;
028400121123         reset  ErrMessage;
028500121123         reset  ErrGenerico;
028600121123
028700121123         SELECT;
028800121123
028900121123           // -?F3=Fine?
029000121123           WHEN  dsp_aid = c_F03;
029100121123             $Fine = *on;
029200121123
029300121123           // -?Invio, F6=Conferma?
029400121123           OTHER;
029500121123             exsr  sr_CtrD01;
029600121123             if  ErrGenerico = *off  and  dsp_aid = c_F06;
029700121123               $Video    = 'D2';
029800121123               $InzD02   = *on;
029900121123             endif;
030000121123
030100121123         ENDSL;
030200121123
030300121123       ENDSR;
030400121123
030500121123       //--------------------------------------------------------------
030600121123       //?Inizializzazione videata D01.                                ?
030700121123       //--------------------------------------------------------------
030800121123       BEGSR  sr_InzD01;
030900121123
031000121123         clear  ST01D01;
031100170904
031200170904         V1Colf = 2359;
031300121123
031400121123       ENDSR;
031500121123
031600121123       //--------------------------------------------------------------
031700121123       //?Controllo dati immessi nella videata D01.                    ?
031800121123       //--------------------------------------------------------------
031900121123       BEGSR  sr_CtrD01;
032000121123
032100121123         // -?Spegnimento indicatori di posizionamento cursore?
032200121123         %subst(IndDspF : 28) = *off;
032300121123
032400121123         // -?Controllo filiale?
032500121123         clear  V1Dfil;
032600121123         if  V1Cfil <> *zero;
032700121123           chain  ( V1Cfil )  AZORG;
032800121123           if  %found(AZORG01L);
032900121123             V1Dfil = ORGdes;
033000121123           else;
033100121123             V1Dfil = *all'? ';
033200121123             ErrGenerico = *on;
033300121123             ErrMessage  = *on;
033400121123             PosCurFIL   = *on;
033500121123             VIDmsg = sk_Msg(01);
033600121123             leavesr;
033700121123           endif;
033800121123         endif;
033900121123
034000121123         // -?Controllo tipo turno?
034100121123         if  V1Cnpg <> *blank  and  V1Cnpg <> 'P'  and
034200121123             V1Cnpg <> '2'     and  V1Cnpg <> '5';
034300121123           ErrGenerico = *on;
034400121123           ErrMessage  = *on;
034500121123           PosCurNPG   = *on;
034600121123           VIDmsg = sk_Msg(02);
034700121123           leavesr;
034800121123         endif;
034900121123
035000121123         // -?Controllo data lettura iniziale?
035100121123         clear  WLBdat;
035200121123         if  V1Cdli = *zero;
035300121123           ErrGenerico = *on;
035400121123           ErrMessage  = *on;
035500121123           PosCurDLI   = *on;
035600121123           VIDmsg = sk_Msg(03);
035700121123           leavesr;
035800121123         else;
035900121123           G08dat = V1Cdli;
036000121123           xsrda8 ( WLBdat );
036100121123           if  G08err =  *on;
036200121123             ErrGenerico = *on;
036300121123             ErrMessage  = *on;
036400121123             PosCurDLI   = *on;
036500121123             VIDmsg = sk_Msg(04);
036600121123             leavesr;
036700121123           endif;
036800121123           V1Cdli = G08dat;
036900121123         endif;
037000170904         //*·W1Ctli = %editc( G08inv : 'X' ) + '000000';
037100121123
037200121123         // ·?Data lettura iniziale precedente di oltre 14 gg quella attuale?
037300121123         if  G08inv < %dec( %date( wDate : *iso ) - %days(14) );
037400121123           ErrGenerico = *on;
037500121123           ErrMessage  = *on;
037600121123           PosCurDLI   = *on;
037700121123           VIDmsg = sk_Msg(05);
037800121123           leavesr;
037900121123         endif;
038000170904
038100170904         // -?Controllo ora lettura iniziale?
038200170904         if  V1Coli <> *zero;
038300170904           wHhMm_ds = %editc( V1Coli : 'X' );
038400170904           if  wHhMm_ds.wHH < 00  or  wHhMm_ds.wHH > 23  or
038500170904               wHhMm_ds.wMM < 00  or  wHhMm_ds.wMM > 59;
038600170904             ErrGenerico = *on;
038700170904             ErrMessage  = *on;
038800170904             PosCurOLI   = *on;
038900170904             VIDmsg = sk_Msg(08);
039000170904             leavesr;
039100170904           endif;
039200170904         endif;
039300170904
039400170904         // ·?Impostazione Data/Ora lettura iniziale?
039500170904         W1Ctli = %editc( G08inv : 'X' ) + %editc( V1Coli : 'X' ) + '00';
039600121123
039700121123         // -?Controllo data lettura finale?
039800121123         clear  WLBdat;
039900121123         if  V1Cdlf = *zero;
040000121123           ErrGenerico = *on;
040100121123           ErrMessage  = *on;
040200121123           PosCurDLF   = *on;
040300121123           VIDmsg = sk_Msg(03);
040400121123           leavesr;
040500121123         else;
040600121123           G08dat = V1Cdlf;
040700121123           xsrda8 ( WLBdat );
040800121123           if  G08err =  *on;
040900121123             ErrGenerico = *on;
041000121123             ErrMessage  = *on;
041100121123             PosCurDLF   = *on;
041200121123             VIDmsg = sk_Msg(04);
041300121123             leavesr;
041400121123           endif;
041500121123           V1Cdlf = G08dat;
041600121123         endif;
041700170904         //*·W1Ctlf = %editc( G08inv : 'X' ) + '999999';
041800170904
041900170904         // -?Controllo ora lettura finale?
042000170904         if  V1Colf <> *zero;
042100170904           wHhMm_ds = %editc( V1Colf : 'X' );
042200170904           if  wHhMm_ds.wHH < 00  or  wHhMm_ds.wHH > 23  or
042300170904               wHhMm_ds.wMM < 00  or  wHhMm_ds.wMM > 59;
042400170904             ErrGenerico = *on;
042500170904             ErrMessage  = *on;
042600170904             PosCurOLF   = *on;
042700170904             VIDmsg = sk_Msg(08);
042800170904             leavesr;
042900170904           endif;
043000170904         endif;
043100170904
043200170904         // ·?Impostazione Data/Ora lettura finale?
043300170904         W1Ctlf = %editc( G08inv : 'X' ) + %editc( V1Colf : 'X' ) + '99';
043400121123
043500170904         // -?Controllo range data/ora lettura?
043600170904         if  W1Ctli > W1Ctlf;
043700170904           ErrGenerico = *on;
043800170904           ErrMessage  = *on;
043900170904           PosCurDLI   = *on;
044000170904           VIDmsg = sk_Msg(06);
044100170904           leavesr;
044200170904         endif;
044300121123
044400121123       ENDSR;
044500121123
044600121123       //--------------------------------------------------------------
044700121123       //?Gestione videata D02.                                        ?
044800121123       //--------------------------------------------------------------
044900121123       BEGSR  sr_GesD02;
045000121123
045100121123         // -?Inizializzazione videata?
045200121123         if  $InzD02 = *on;
045300121123           exsr  sr_InzD02;
045400121123           $InzD02 = *off;
045500121123         endif;
045600121123
045700121123         // -?(Dis)Abilitazione tasti funzionali?
045800121123         F6attivo  = *off;
045900121123         F12attivo = *on;
046000121123
046100121123         // -?Emissione Testata e Piede con tasti funzionali abilitati?
046200121123         if  Not  ErrMessage;
046300121123           write  ST01T01;
046400121123           write  ST01D01;
046500121123         endif;
046600121123         write  ST01P01;
046700121123         write  Protect;
046800121123
046900121123         // -?Emissione videata?
047000121123         exfmt  ST01D02;
047100121123
047200121123         clear  VIDmsg;
047300121123         reset  ErrMessage;
047400121123         reset  ErrGenerico;
047500121123
047600121123         SELECT;
047700121123
047800121123           // -?F3=Fine?
047900121123           WHEN  dsp_aid = c_F03;
048000121123             $Fine = *on;
048100121123
048200121123           // -?F12=Ritorno?
048300121123           WHEN  dsp_aid = c_F12;
048400121123             $Video = 'D1';
048500121123
048600121123         ENDSL;
048700121123
048800121123       ENDSR;
048900121123
049000121123       //--------------------------------------------------------------
049100121123       //?Inizializzazione videata D02.                                ?
049200121123       //--------------------------------------------------------------
049300121123       BEGSR  sr_InzD02;
049400121123
049500121123         // -?Pulizia videata?
049600121123         clear  ST01D02;
049700121123
049800121123         // -?Compilazione stringa SQL?
049900121123         wSql = 'select TR7pd4 from ';
050000121123         if  %subst( KNSIF : 7 : 1 ) = 'P';
050100121123           wSQL += 'FILTRAPRD';
050200121123         else;
050300121123           wSQL += 'FILTRA201';
050400121123         endif;
050500121123         wSql += '/FLTR700F where TR7pd4 <> ''''';
050600121123
050700121123         IF  V1Cfil <> *zero  or  V1Cnpg <> *blank  or
050800121123             V1Cdli <> *zero  or  V1Cdlf <> *zero;
050900121123
051000121123           if  V1Cfil <> *zero;
051100121123             wSql  += ' and TR7fil = ''' + %editc( V1Cfil : 'X' ) + '''';
051200121123           endif;
051300121123
051400121123           if  V1Cnpg <> *blank;
051500121123             select;
051600121127               when  V1Cnpg = '2'  or  V1Cnpg = '5';
051700121127                 wSql += ' and TR7ctl = ''' + V1Cnpg + ''' +
051800121127                           and TR7020 = ''20''';
051900121123               when  V1Cnpg = 'P';
052000121127                 wSql += ' and TR7ctl = ''2'' +
052100121127                           and TR7020 = ''23''';
052200121123             endsl;
052300121126           else;
052400121126             wSql  += ' and ((TR7ctl in (''2'', ''5'') and +
052500121127                              TR7020 in (''20'', ''23''))';
052600121123           endif;
052700121123
052800121123           if  V1Cdli <> *zero  or  V1Cdlf <> *zero;
052900121123             wSql  += ' and TR7tim between ''' + W1Ctli +
053000121123                                   ''' and ''' + W1Ctlf + '''';
053100121123           endif;
053200121123
053300121123         ENDIF;
053400121123
053500121123         clear  $EoF;
053600121126         clear  W2Cdla;
053700121126         clear  W2Cdlb;
053800121126         clear  W2Cdlc;
053900121123
054000121123         // -?Apertura cursore?
054100121123         exsr  sr_OpenCursor;
054200121123
054300121123         // -?Lettura cursore?
054400121123         DoW  Not $EoF;
054500121123           exsr  sr_ReadCursor;
054600121123         EndDo;
054700121123
054800121123         // -?Chiusura cursore?
054900121123         exsr  sr_CloseCursor;
055000121123
055100121123       ENDSR;
055200121123
055300121123       //--------------------------------------------------------------
055400121123       //?Apertura cursore C1.                                         ?
055500121123       //--------------------------------------------------------------
055600121123       BEGSR  sr_OpenCursor;
055700121123
055800121123         // -?Dichiarazione cursore?
055900121123         exec sql   prepare S1   from :wSQL;
056000121123         exec sql   declare C1   cursor for S1;
056100121123
056200121123         // -?Apertura del cursore?
056300121123         exec sql   open C1;
056400121123
056500121123         if  SQLcode < *zero;
056600121123           $EoF = *on;
056700121123           ErrGenerico = *on;
056800121123           ErrMessage  = *on;
056900121123           VIDmsg = sk_Msg(07);
057000121123         endif;
057100121123
057200121123       ENDSR;
057300121123
057400121123       //--------------------------------------------------------------
057500121123       //?Lettura cursore C1.                                          ?
057600121123       //--------------------------------------------------------------
057700121123       BEGSR  sr_ReadCursor;
057800121123
057900121123         clear  ds_SQL;
058000121123
058100121123         exec sql   fetch next   from C1   into :ds_SQL;
058200121123
058300121123         select;
058400121123           when  SQLcode = 100;
058500121123             $EoF = *on;
058600121126             if  V2Cncl <> *zero;
058700121126               eval(h) V2Cdla = W2Cdla / V2Cncl;
058800121126               eval(h) V2Cdlb = W2Cdlb / V2Cncl;
058900121126               eval(h) V2Cdlc = W2Cdlc / V2Cncl;
059000121126             endif;
059100121123           when  SQLcode < *zero;
059200121123             $Eof = *on;
059300121123             ErrGenerico = *on;
059400121123             ErrMessage  = *on;
059500121123             VIDmsg = sk_Msg(07);
059600121123           other;
059700121123             exsr  sr_Somma;
059800121123         endsl;
059900121123
060000121123       ENDSR;
060100121123
060200121123       //--------------------------------------------------------------
060300121123       //?Chiusura cursore C1.                                         ?
060400121123       //--------------------------------------------------------------
060500121123       BEGSR  sr_CloseCursor;
060600121123
060700121123         // -?Chiusura del cursore?
060800121123         exec sql   close C1;
060900121123
061000121123       ENDSR;
061100121123
061200121123       //--------------------------------------------------------------
061300121123       //?Somma delle dimensioni e del numero dei colli.               ?
061400121123       //--------------------------------------------------------------
061500121123       BEGSR  sr_Somma;
061600121123
061700121123         // -?Barcode letto da Volume?
061800121123         TR7pd4_ds = sql_PD4;
061900121123         clear  sk_Dim;
062000121123
062100121123         // -?Selezione delle sole letture con dimensioni?
062200121123         if  %check( c_Digits : TR7pd4_ds.TR7dim1 ) > *zero  or
062300121123             %check( c_Digits : TR7pd4_ds.TR7dim2 ) > *zero  or
062400121123             %check( c_Digits : TR7pd4_ds.TR7dim3 ) > *zero;
062500121123           leavesr;
062600121123         endif;
062700121123         if  TR7pd4_ds.TR7dim1 = *zero  and
062800121123             TR7pd4_ds.TR7dim2 = *zero  and
062900121123             TR7pd4_ds.TR7dim3 = *zero;
063000121123           leavesr;
063100121123         endif;
063200121123
063300121123         // -?Somma delle dimensioni?
063400121123         sk_Dim(1) = %int( TR7pd4_ds.TR7dim1 ) / 1000;
063500121123         sk_Dim(2) = %int( TR7pd4_ds.TR7dim2 ) / 1000;
063600121123         sk_Dim(3) = %int( TR7pd4_ds.TR7dim3 ) / 1000;
063700121123         sorta  sk_Dim;
063800121123         W2Cdla += sk_Dim(3);
063900121123         W2Cdlb += sk_Dim(2);
064000121123         W2Cdlc += sk_Dim(1);
064100121123
064200121123         // -?Incremento del numero colli?
064300121123         V2Cncl += 1;
064400121123
064500121123       ENDSR;
064600121123
064700121123       //--------------------------------------------------------------
064800121123       //?Operazioni finali.                                           ?
064900121123       //--------------------------------------------------------------
065000121123       BEGSR  sr_RoutEnd;
065100121123
065200121123         // -?Uscita dal *pgm?
065300121123         return;
065400121123
065500121123       ENDSR;
065600121123
065700121123      /end-free
065800121123
065900121123       //--------------------------------------------------------------
066000121123       //?Schiere a tempo di compilazione.                             ?
066100121123       //--------------------------------------------------------------
066200121123
066300121123** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
066400121123Codice filiale errato                                                          1
066500121123Tipo turno errato                                                              2
066600121123Data lettura obbligatoria                                                      3
066700121123Data lettura formalmente errata                                                4
066800121123La data lettura iniziale NON può precedere di oltre 2 settimane quella attuale 5
066900170904Data/Ora lettura iniziale successive a quella finale                           6
067000121123Rilevato errore SQL: contattare il CED                                         7
067100170904Ora lettura formalmente errata                                                 8
