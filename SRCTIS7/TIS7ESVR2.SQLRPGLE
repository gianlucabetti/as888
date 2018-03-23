000100111017       //==============================================================
000200111020       //?Interrogazione ERRORI Storico Easy-Sped - Versioni           ?
000300111017       //==============================================================
000400111017
000500111017       //--------------------------------------------------------------
000600111017       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700111017       //--------------------------------------------------------------
000800111017
000900111017     /*PRM dbgview(*source)
001000120214     /*CMD ovrdbf file(WFESV00F) tofile(GAITRAAZP/WFESV00F) +
001100120214     /*CMD        ovrscope(*calllvl)
001200120214     /*END dltovr file(WFESV00F) lvl(*)
001300111017     /*END
001400111017
001500111017       //--------------------------------------------------------------
001600111017       //?Specifiche di controllo.                                     ?
001700111017       //--------------------------------------------------------------
001800111017
001900111017     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000111109     h dftactgrp(*no)
002100111017     h alwnull(*inputonly)
002200111017
002300111017       //--------------------------------------------------------------
002400111017       //?Dichiarazione file.                                          ?
002500111017       //--------------------------------------------------------------
002600111109
002700111109       // -?Organigramma?
002800111109     fAZORG01L  if   e           k disk
002900111017
003000111017       // -?Immagini Lettere di Vettura?
003100111017     fWFESV00F  Uf   e             disk    usropn
003200111017
003300111017       // -?Video Gestione?
003400111020     fTIS7ESVD2 cf   e             workstn
003500111020     f                                     sfile(S7ESV2S01 : S01nrr)
003600111109     f                                     sfile(S7ESV2SW1 : SW1nrr)
003700111017     f                                     indds(IndDspF)
003800111017     f                                     infds(InfDspF)
003900111017
004000111017       //--------------------------------------------------------------
004100111017       //?Definizione costanti.                                        ?
004200111017       //--------------------------------------------------------------
004300111018
004400111018       // -?Costante con la versione di Easy-Sped che prevede controlli?
004500111018       //  ?sul Tipo Aggiornamento Web?
004600111018     d c_VersEsySp     c                   const('4.0.4 ')
004700111017
004800111017       // -?Numero di record in ciascuna videata di subfile?
004900111123     d c_SflPag        c                   const(16)
005000111017
005100111017       // -?Numero massimo di record gestibili nel subfile?
005200111017     d c_MaxRec        c                   const(9999)
005300111017
005400111017       // -?Tasti funzionali a video?
005500111017     d c_F01           c                   const(x'31')
005600111017     d c_F02           c                   const(x'32')
005700111017     d c_F03           c                   const(x'33')
005800111017     d c_F04           c                   const(x'34')
005900111017     d c_F05           c                   const(x'35')
006000111017     d c_F06           c                   const(x'36')
006100111017     d c_F07           c                   const(x'37')
006200111017     d c_F08           c                   const(x'38')
006300111017     d c_F09           c                   const(x'39')
006400111017     d c_F10           c                   const(x'3A')
006500111017     d c_F11           c                   const(x'3B')
006600111017     d c_F12           c                   const(x'3C')
006700111017     d c_F13           c                   const(x'B1')
006800111017     d c_F14           c                   const(x'B2')
006900111017     d c_F15           c                   const(x'B3')
007000111017     d c_F16           c                   const(x'B4')
007100111017     d c_F17           c                   const(x'B5')
007200111017     d c_F18           c                   const(x'B6')
007300111017     d c_F19           c                   const(x'B7')
007400111017     d c_F20           c                   const(x'B8')
007500111017     d c_F21           c                   const(x'B9')
007600111017     d c_F22           c                   const(x'BA')
007700111017     d c_F23           c                   const(x'BB')
007800111017     d c_F24           c                   const(x'BC')
007900111017     d c_Enter         c                   const(x'F1')
008000111017     d c_RollDown      c                   const(x'F4')
008100111017     d c_RollUp        c                   const(x'F5')
008200120716
008300120716       // -?Costante per controllo "caratteri solo numerici"?
008400120716     d c_Digits        c                   const('0123456789')
008500111017
008600111017       // -?Attributi di Visualizzazione?
008700111017     d c_DspAtrNorm    c                   const(x'A0')
008800111017     d c_DspAtrRed     c                   const(x'A8')
008900111017     d c_DspAtr_HI     c                   const(x'A2')
009000111017     d c_DspAtr_RI     c                   const(x'A1')
009100111017     d c_DspAtrRed_RI  c                   const(x'A9')
009200111017
009300111017       //--------------------------------------------------------------
009400111017       //?Definizione schiere.                                         ?
009500111017       //--------------------------------------------------------------
009600111018
009700111018       // -?Tipi Aggiornamento Web?
009800111207     d sk_TypWeb       s              2    dim(08)  ctdata  perrcd(1)
009900111207     d sk_TypWebD      s             30    dim(08)  alt(sk_TypWeb)
010000111017
010100111017       // -?Codici Errore in WFESV00F?
010200111207     d sk_Err          s                   like(WESerr)
010300111123     d                                     dim(05)  ctdata  perrcd(1)
010400111207     d sk_ErrD         s             98    dim(05)  alt(sk_Err)
010500111017
010600111017       // -?Messaggi di errore?
010700111207     d sk_Msg          s             78    dim(14)  ctdata  perrcd(1)
010800111017
010900111017       //--------------------------------------------------------------
011000111017       //?Definizione aree dati.                                       ?
011100111017       //--------------------------------------------------------------
011200111017
011300111017       // -?Dati utente?
011400111017     d §AzUte        e ds                  extname(AZUTE00F)
011500111017     d                                     dtaara
011600111017     d §DatiUte      e ds                  extname(dDatiUte)
011700111017     d                                     dtaara
011800111017
011900111017       //--------------------------------------------------------------
012000111017       //?Definizione strutture dati.                                  ?
012100111017       //--------------------------------------------------------------
012200111017
012300111017       // -?Status ds?
012400111017     d Status         sds
012500111017     d   SDSpgm          *proc
012600111017     d   SDSusr              254    263
012700111017
012800111017       // -?InfDS?
012900111017     d InfDspF         ds
013000111017     d   dsp_aid             369    369a
013100111017     d   sfl_rrn             376    377i 0
013200111017     d   min_nrr             378    379i 0
013300111017     d   num_rcds            380    381i 0
013400111017
013500111017       // -?Indicatori su DspF?
013600111017     d IndDspF         ds                  inz
013700111017         // -?Abilitazione tasti funzionali?
013800121120     d   F6Attivo                      n   overlay(IndDspF : 06)
013900111017     d   F8Attivo                      n   overlay(IndDspF : 08)
014000111017     d   F9Attivo                      n   overlay(IndDspF : 09)
014100121120     d   F16Attivo                     n   overlay(IndDspF : 16)
014200111017         // -?Emissione messaggio di errore?
014300111017     d   ErrMessage                    n   overlay(IndDspF : 28)
014400111017         // -?Indicatori di gestione del subfile?
014500111017     d   SflDsp_N                      n   overlay(IndDspF : 30)
014600111017     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
014700111017     d   SflNxtChg                     n   overlay(IndDspF : 32)
014800111017     d   SflEnd                        n   overlay(IndDspF : 33)
014900111109     d   WSflDsp_N                     n   overlay(IndDspF : 34)
015000111109     d   WSflDspCtl_N                  n   overlay(IndDspF : 35)
015100111109     d   WSflNxtChg                    n   overlay(IndDspF : 36)
015200111109     d   WSflEnd                       n   overlay(IndDspF : 37)
015300111017         // -?Indicatori per attibuti di visualizzazione?
015400111017     d   NotDisplayed                  n   overlay(IndDspF : 40)
015500120716     d   Evid_Obj                      n   overlay(IndDspF : 41)
015600111017         // -?Posizionamento cursore & Segnalazione errore?
015700111017     d   PosCurKSC                     n   overlay(IndDspF : 50)
015800111017     d   PosCurErr1                    n   overlay(IndDspF : 51)
015900111017     d   PosCurErr2                    n   overlay(IndDspF : 52)
016000111017     d   PosCurErr3                    n   overlay(IndDspF : 53)
016100111017     d   PosCurErr4                    n   overlay(IndDspF : 54)
016200111017     d   PosCurErr5                    n   overlay(IndDspF : 55)
016300111207     d*//PosCurErr6                    n   overlay(IndDspF : 56)
016400111207     d*//PosCurErr7                    n   overlay(IndDspF : 57)
016500111207     d*//PosCurErr8                    n   overlay(IndDspF : 58)
016600111207     d*//PosCurErr9                    n   overlay(IndDspF : 59)
016700111207     d*//PosCurErrA                    n   overlay(IndDspF : 60)
016800111109     d   PosCurDTO                     n   overlay(IndDspF : 61)
016900111109     d   PosCurTGO                     n   overlay(IndDspF : 62)
017000111109     d   PosCurNSOL                    n   overlay(IndDspF : 63)
017100111109     d   PosCurNSOL0                   n   overlay(IndDspF : 64)
017200111109     d   PosCurAPO                     n   overlay(IndDspF : 65)
017300111109     d   PosCurBLOC                    n   overlay(IndDspF : 66)
017400111109     d   PosCurEVER                    n   overlay(IndDspF : 67)
017500111109     d   PosCurNRS                     n   overlay(IndDspF : 68)
017600111017     d   PosCurOPZ                     n   overlay(IndDspF : 70)
017700111017         // -?Riemissione videata?
017800111017     d   ErrGenerico                   n   overlay(IndDspF : 99)
017900111017
018000111017       // -?Codici Errore a video?
018100111017     d V1Cerr          ds                  inz
018200111017     d   V1Cerr1                           inz
018300111017     d   V1Cerr2                           inz
018400111017     d   V1Cerr3                           inz
018500111017     d   V1Cerr4                           inz
018600111017     d   V1Cerr5                           inz
018700111207     d     sk_V1Cerr                       inz  like(V1Cerr1)  dim(05)
018800111207     d                                          overlay(V1Cerr : 1)
018900111207     d*//V1Cerr6                           inz
019000111207     d*//V1Cerr7                           inz
019100111207     d*//V1Cerr8                           inz
019200111207     d*//V1Cerr9                           inz
019300111207     d*//V1CerrA                           inz
019400111207     d*//  sk_V1Cerr                       inz  like(V1Cerr1)  dim(10)
019500111207     d*//                                       overlay(V1Cerr : 1)
019600111017
019700111017       // -?Parametri ricevuti?
019800111017     d KPJBA         e ds
019900111017
020000111017       // -?Dati estratti via SQL?
020100111017     d wWESrrn         s             11  0 inz
020200111017     d WFESVds       e ds                  inz  extname(WFESV00F)
020300111018     d                                          qualified
020400111017
020500111017       //--------------------------------------------------------------
020600111017       //?Definizione variabili globali.                               ?
020700111017       //--------------------------------------------------------------
020800111017
020900111017       // -?Flags booleani?
021000111017     d $Fine           s               n   inz
021100111017     d $EoF            s               n   inz
021200111017     d $Err            s               n   inz
021300111017     d $ErrCmd         s               n   inz
021400111017     d $InzD01         s               n   inz(*on)
021500111017     d $InzD02         s               n   inz(*on)
021600111017     d $InzS01         s               n   inz(*on)
021700111109     d $InzSW1         s               n   inz(*on)
021800111017
021900111017       // -?Indici di schiera?
022000111017     d xx              s              3  0 inz
022100111017
022200111017       // -?Variabili per la gestione del video?
022300111017     d $Video          s              2    inz('D1')
022400111018     d $VideoPrec      s              2    inz
022500111017     d W_SflPag        s              3  0 inz
022600111017     d wPag            s              4  0 inz
022700111017     d wRig            s              3  0 inz
022800111109     d SW1nrr          s                   like(C1RcdNbr) inz
022900111017     d S01nrr          s                   like(C1RcdNbr) inz
023000111017     d SavS01csr       s                   like(C1RcdNbr) inz
023100111017     d wOrdSfl         s              1  0 inz
023200111017
023300111017       // -?Stringa SQL da eseguire?
023400111017     d wSQL            s           2048    Varying        inz
023500111017
023600111017       // -?Campi di comodo?
023700111017     d wDate_Iso       s               d   datfmt(*iso)   inz
023800111017     d W1Cdto          s                   like(V1Cdto)   inz
023900111017     d wCountSel       s              3  0 inz
024000111017     d wCountErr       s              3  0 inz
024100111017
024200111017       //--------------------------------------------------------------
024300111017       //?Definizione prototipi procedure.                             ?
024400111017       //--------------------------------------------------------------
024500111017
024600111017       // -?Reperimento dati utente?
024700111017     d TIBS34ds      e ds                  inz
024800111017      /copy gaitrasrc/srcProtoPR,TIBS34R
024900111017
025000111017       // -?Caricamento filiali gestite dall'utente?
025100111017     d TRUL31ds      e ds                  inz
025200111017     d   $POg                 10    759    inz   dim(250)
025300111017      /copy gaitrasrc/srcProtoPR,TRUL31R
025400111017
025500111017       // -?Reperimento dati tabelle?
025600111017      /copy gaitrasrc/srcProtoPI,TRULTAB
025700111017      /copy gaitrasrc/srcProtoPR,TRULTAB
025800111018
025900120514       // -?Gestione tab. "3C" = Invio dati -Bollettazione-?
026000111018     d trtb28r         pr                  extpgm('TRTB28R')
026100111018     d   kpjba                             likeds(kpjba)
026200111017
026300120716       // -?Controllo/Decodifica cliente?
026400111017      /copy gaitrasrc/srcProtoPI,TIBS69R
026500111017      /copy gaitrasrc/srcProtoPR,TIBS69R
026600120514
026700120716       // -?Interrogazione file di Log Strategi - Upload?
026800120514     d Prm_TIS760ds    ds
026900120514     d  DEPop0                        3    inz('P05')
027000120514     d  DEPksc                        8
027100120514     d  DEPtip                        2
027200120514     d*//  DEPnfil                      30
027300120514      /copy gaitrasrc/srcProtoPR,TIS760R
027400111017
027500111017       // -?Controllo ed inversione date?
027600111017     d WLBdat          ds                  inz
027700111017     d  G08dat                        8  0 inz
027800111017     d  G08inv                        8  0 inz
027900111017     d  G08err                        1    inz('3')
028000111017     d  G08tgi                        5  0 inz
028100111017      /copy gaitrasrc/srcProtoPr,XSRDA8
028200111017
028300120716       // -?API QCAPCMD (Process Commands)?
028400111017     d Qcmd            s           2048    inz  varying
028500111017      /copy qSysInc/qRpgleSrc,QCAPCMD
028600111017      /copy gaitrasrc/srcProtoPR,QCAPCMD
028700111018
028800111018       // -?Parametri gestione errori API.?
028900111018      /copy qsysinc/qRpgleSrc,QUSEC
029000111017
029100111017       //--------------------------------------------------------------
029200111017       //?Definizione key-list.                                        ?
029300111017       //--------------------------------------------------------------
029400111017
029500111017
029600111017       //--------------------------------------------------------------
029700111017       //?M A I N - L I N E                                            ?
029800111017       //--------------------------------------------------------------
029900111017
030000111017     c     *Entry        plist
030100111017     c                   parm                    KPJBA
030200111017
030300111017      /free
030400111017
030500111017       // -?Operazioni iniziali?
030600111017       exsr sr_RoutInz;
030700111017
030800111017       // -?Gestione videate?
030900111017       DoW  $Fine = *off;
031000111017
031100111017         select;
031200111017           // -?Gestione videata D1?
031300111017           when $Video = 'D1';
031400111017             exsr sr_GesD01;
031500111017           // -?Gestione subfile S1?
031600111017           when $Video = 'S1';
031700111017             exsr sr_GesS01;
031800111017           // -?Gestione videata D2?
031900111017           //  ?(D2 gestita all'interno del ciclo opzioni in S1)?
032000111017           //when $Video = 'D2';
032100111017           //  exsr sr_GesD02;
032200111018           // -?Gestione window W1?
032300111018           when $Video = 'W1';
032400111109             exsr sr_GesSW1;
032500111017           // -?? ? ? ? ??
032600111017           other;
032700111017             $Fine = *on;
032800111017         endsl;
032900111017       enddo;
033000111017
033100111017       // -?Operazioni finali?
033200111017       exsr sr_RoutEnd;
033300111017
033400111017       //--------------------------------------------------------------
033500111017       //?Operazioni iniziali.                                         ?
033600111017       //--------------------------------------------------------------
033700111017       BEGSR  sr_RoutInz;
033800111017
033900111017         *inLR = *on;
034000111017
034100111017         // -?Impostazione opzioni per SQL?
034200111017         exec sql   set  option  DynUsrPrf = *Owner,
034300111017                                 CloSqlCsr = *EndMod;
034400111017
034500111017         // -?Reperimento dati job?
034600111017         exsr  sr_DatiJob;
034700111017
034800111017         // -?Impostazione nome programma a video?
034900111017         V1Tpgm = SDSpgm;
035000111017
035100111017         // -?Caricamento filiali gestibili dall'utente?
035200111017         clear  TRUL31ds;
035300111017         I31abi = UTEaut;
035400111017         I31cdi = DUTdis;
035500111017         I31car = DUTare;
035600111017         I31cpo = DUTpou;
035700111017         trul31r ( KPJBA : TRUL31ds );
035800111017         // -?In caso di errore si uscirà DOPO l'emissione della?
035900111017         //  ?videata D01?
036000111017         //if  O31pog <= *zero;
036100111017         //  $Fine = *on;
036200111017         //endif;
036300111017
036400111017       ENDSR;
036500111017
036600111017       //--------------------------------------------------------------
036700111017       //?Reperimento Dati del job (Utente/Operativi).                 ?
036800111017       //--------------------------------------------------------------
036900111017       BEGSR  sr_DatiJob;
037000111017
037100111017         in(e) §AzUte;
037200111017         if NOT %error;
037300111017           in(e) §DatiUte;
037400111017         endif;
037500111017         if %error or RSut = *blank;
037600111017           tibs34r ( tibs34ds );
037700111017           in §AzUte;
037800111017           in §DatiUte;
037900111017         endif;
038000111017
038100111017       ENDSR;
038200111017
038300111017       //--------------------------------------------------------------
038400111017       //?Gestione videata D01 - Filtro.                               ?
038500111017       //--------------------------------------------------------------
038600111017       BEGSR  sr_GesD01;
038700111017
038800111017         // -?Inizializzazione videata?
038900111017         if  $InzD01 = *on;
039000111017           exsr  sr_InzD01;
039100111017           $InzD01 = *off;
039200111017         endif;
039300111017
039400111017         // -?Emissione Testata?
039500111020         write  S7ESV2T01;
039600111017
039700111017         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
039800111020         exfmt  S7ESV2D01;
039900111017
040000111017         clear  V1Dmsg;
040100111017         reset  ErrMessage;
040200111017         reset  ErrGenerico;
040300111017
040400111017         SELECT;
040500111018
040600111018           // -?Sistema Informativo di filiale => Fine?
040700111018           WHEN  %subst( KNSIF : 1 : 3 ) = 'FIL'  or
040800111018                 %subst( KNSIF : 7 : 2 ) = 'PF';
040900111018             $Fine = *on;
041000111017
041100111017           // -?Nessuna filiale gestibile dall'utente?
041200111017           WHEN  O31pog <= *zero;
041300111017             $Fine = *on;
041400111018
041500111018           // -?F1=Codici Errore?
041600111018           WHEN  dsp_aid = c_F01;
041700111018             $VideoPrec = $Video;
041800111018             $Video     = 'W1';
041900111109             $InzSW1    = *on;
042000111017
042100111017           // -?F3=Fine?
042200111017           WHEN  dsp_aid = c_F03;
042300111017             $Fine = *on;
042400111017
042500111017           // -?Invio?
042600111017           OTHER;
042700111017             exsr sr_CtrD01;
042800111017             if  ErrGenerico = *off;
042900111018               $VideoPrec = $Video;
043000111018               $Video     = 'S1';
043100111109               $InzS01    = *on;
043200111017             endif;
043300111017
043400111017         ENDSL;
043500111017
043600111017       ENDSR;
043700111017
043800111017       //--------------------------------------------------------------
043900111017       //?Inizializzazione videata D01 - Filtro di lancio.             ?
044000111017       //--------------------------------------------------------------
044100111017       BEGSR  sr_InzD01;
044200111017
044300111020         clear  S7ESV2D01;
044400111017
044500111017         //if  %subst( KNMUS : 1 : 3 ) <> 'EDP'  and
044600111017         //    %subst( KNSIF : 1 : 3 ) =  'FIL';
044700111017         //  V1Hfgs = DUTpou;
044800111017         //endif;
044900111017
045000111017         exsr  sr_CtrD01;
045100111017
045200111017       ENDSR;
045300111017
045400111017       //--------------------------------------------------------------
045500111017       //?Controllo dati immessi nella videata D01.                    ?
045600111017       //--------------------------------------------------------------
045700111017       BEGSR  sr_CtrD01;
045800111017
045900111017         %subst(IndDspF : 50) = *off;
046000111018
046100111018         // -?Controllo Sistema Informativo per verificare?
046200111018         //  ?se Sede o Filiale?
046300111018         if  %subst(KNSIF:1:3) = 'FIL'  or
046400111018             %subst(KNSIF:7:2) = 'PF';
046500111018           ErrMessage  = *on;
046600111018           ErrGenerico = *on;
046700111207           V1Dmsg = sk_Msg(01);
046800111018           leavesr;
046900111018         endif;
047000111017
047100111017         // -?Nessuna filiale gestibile dall'utente?
047200111017         if  O31pog <= *zero;
047300111017           ErrMessage  = *on;
047400111017           ErrGenerico = *on;
047500111207           V1Dmsg = sk_Msg(02);
047600111017           leavesr;
047700111017         endif;
047800111017
047900111017         // -?Codici Errore?
048000111017         if  V1Cerr <> *blank;
048100111207           For  xx = 1  To  %elem( sk_V1Cerr );
048200111207             if  sk_V1Cerr(xx) <> *blank  and
048300111207                 %lookup(sk_V1Cerr(xx) : sk_Err) = *zero;
048400111017               %subst(IndDspF : 50+xx) = *on;
048500111017               ErrMessage  = *on;
048600111017               ErrGenerico = *on;
048700111207               V1Dmsg = sk_Msg(03);
048800111017               leavesr;
048900111017             endif;
049000111017           EndFor;
049100111017         endif;
049200111017
049300111017         // -?Data occorrenza?
049400111017         clear  WLBdat;
049500111017         if  V1Cdto <> *zero;
049600111017           G08dat = V1Cdto;
049700111017           xsrda8 ( WLBdat );
049800111017           if  G08err =  *on;
049900111017             PosCurDTO   = *on;
050000111017             ErrMessage  = *on;
050100111017             ErrGenerico = *on;
050200111207             V1Dmsg = sk_Msg(04);
050300111017             leavesr;
050400111017           endif;
050500111017           V1Cdto = G08dat;
050600111017         endif;
050700111017         W1Cdto = G08inv;
050800111024
050900111024         // -?Totale giorni occorrenza?
051000111024         if  V1Ctgo < *zero;
051100111024           PosCurTGO   = *on;
051200111024           ErrMessage  = *on;
051300111024           ErrGenerico = *on;
051400111207           V1Dmsg = sk_Msg(05);
051500111024           leavesr;
051600111024         endif;
051700111109
051800111109         // -?Numero solleciti inviati?
051900111109         if  V1Cnsol < *zero;
052000111109           PosCurNSOL  = *on;
052100111109           ErrMessage  = *on;
052200111109           ErrGenerico = *on;
052300111207           V1Dmsg = sk_Msg(06);
052400111109           leavesr;
052500111109         endif;
052600111109
052700111109         // -?Selezione clienti SENZA solleciti inviati?
052800111109         select;
052900111109           when  V1Cnsol0 <> 'S'  and  V1Cnsol0 <> *blank;
053000111109             PosCurNSOL0 = *on;
053100111109             ErrMessage  = *on;
053200111109             ErrGenerico = *on;
053300111207             V1Dmsg = sk_Msg(07);
053400111109             leavesr;
053500111109           when  V1Cnsol0 = 'S'  and  V1Cnsol > *zero;
053600111109             PosCurNSOL0 = *on;
053700111109             ErrMessage  = *on;
053800111109             ErrGenerico = *on;
053900111207             V1Dmsg = sk_Msg(08);
054000111109             leavesr;
054100111109         endsl;
054200111109
054300111109         // -?Aree Poc di invio solleciti?
054400111109         select;
054500111109           when  V1Capo = *zero;
054600111122           when  V1Cnsol0 = 'S';
054700111122             PosCurAPO   = *on;
054800111122             ErrMessage  = *on;
054900111122             ErrGenerico = *on;
055000111207             V1Dmsg = sk_Msg(09);
055100111122             leavesr;
055200111109           when  V1Capo < *zero;
055300111109             PosCurAPO   = *on;
055400111109             ErrMessage  = *on;
055500111109             ErrGenerico = *on;
055600111207             V1Dmsg = sk_Msg(10);
055700111109             leavesr;
055800111109           when  V1Capo > *zero;
055900111109             chain  ( V1Capo )  AZORG;
056000111109             if  Not %found(AZORG01L)  or
056100111109                 ORGfva <> *blank      or
056200111109                (ORGfag <> 'F'  and  ORGfag <> 'A');
056300111109               PosCurAPO   = *on;
056400111109               ErrMessage  = *on;
056500111109               ErrGenerico = *on;
056600111207               V1Dmsg = sk_Msg(10);
056700111109               leavesr;
056800111109             endif;
056900111109         endsl;
057000111109
057100111109         // -?Selezione errori con invio sollecito bloccato?
057200111109         if  V1Cbloc <> *blank  and
057300111109             V1Cbloc <> 'S'     and
057400111109             V1Cbloc <> 'N';
057500111109           PosCurBLOC  = *on;
057600111109           ErrMessage  = *on;
057700111109           ErrGenerico = *on;
057800111207           V1Dmsg = sk_Msg(11);
057900111109           leavesr;
058000111109         endif;
058100111024
058200111024         // -?Numero serie?
058300111024         if  V1Cnrs < *zero;
058400111024           PosCurNRS   = *on;
058500111024           ErrMessage  = *on;
058600111024           ErrGenerico = *on;
058700111207           V1Dmsg = sk_Msg(12);
058800111024           leavesr;
058900111024         endif;
059000111017
059100111017       ENDSR;
059200111017
059300111017       //--------------------------------------------------------------
059400111017       //?Gestione subfile S01 - Subfile con elenco record WFESV00F.   ?
059500111017       //--------------------------------------------------------------
059600111017       BEGSR  sr_GesS01;
059700111017
059800111017         // -?Inizializzazione subfile?
059900111017         if  $InzS01;
060000111017           exsr  sr_InzS01;
060100111017           $InzS01 = *off;
060200111017         endif;
060300111017
060400111017         // -?Abilitazione CONDIZIONATE tasti funzionali in P01 (per C01)?
060500111017         F8Attivo  = (S01nrr > *zero);
060600111017         F9Attivo  = F8Attivo;
060700111017
060800111017         // -?Emissione Testata e Piede con tasti funzionali abilitati?
060900111020         write  S7ESV2T01;
061000111020         write  S7ESV2P01;
061100111017
061200111017         // -?Posizionamento cursore?
061300111017         if  C1CsrRrn > *zero;
061400111017           C1RcdNbr = C1CsrRrn;
061500111017         else;
061600111017           C1RcdNbr = 1;
061700111020           write  S7ESV2S00;             //(no rec.)?
061800111017         endif;
061900111017
062000111017         // -?Emissione videata?
062100111020         exfmt  S7ESV2C01;
062200111017
062300111017         reset  ErrMessage;
062400111017         reset  ErrGenerico;
062500111017         clear  V1Dmsg;
062600111017
062700111017         SELECT;
062800111018
062900111018           // -?F1=Window Codici Errore?
063000111018           WHEN  dsp_aid = c_F01;
063100111018             $VideoPrec = $Video;
063200111018             $Video     = 'W1';
063300111017
063400111017           // -?F3=Fine?
063500111017           WHEN  dsp_aid = c_F03;
063600111017             exsr  sr_CloseCursor;
063700111017             $Fine = *on;
063800111017
063900111017           // -?F5=Aggiornamento?
064000111017           WHEN  dsp_aid = c_F05;
064100111017             exsr  sr_CloseCursor;
064200111017             reset  $InzS01;
064300111017
064400111017           // -?F8=Ordinamento Sfl?
064500111017           WHEN  dsp_aid = c_F08;
064600111017             exsr  sr_F08S01;
064700111017
064800111017           // -?F12=Ritorno?
064900111017           WHEN  dsp_aid = c_F12;
065000111017             exsr  sr_CloseCursor;
065100111018             $Video = 'D1';
065200111017
065300111017           // -?Roll-Up?
065400111017           WHEN  dsp_aid = c_RollUp;
065500111017             if  S01nrr >= c_MaxRec;
065600111017               ErrMessage  = *on;
065700111017               ErrGenerico = *on;
065800111207               V1Dmsg = sk_Msg(13);
065900111017             else;
066000111017               exsr sr_RollUpS01;
066100111017             endif;
066200111017
066300111017           // -?SubFile vuoto?
066400111017           WHEN  S01nrr = *zero;
066500111017
066600111017           // -?Invio?
066700111017           OTHER;
066800111017             // -?Gestione opzioni?
066900111017             exsr  sr_OpzS01;
067000111017             if  ErrGenerico;
067100111017               leavesr;
067200111017             endif;
067300111017
067400111017         ENDSL;
067500111017
067600111017       ENDSR;
067700111017
067800111017       //--------------------------------------------------------------
067900111017       //?Inizializzazione subfile S01.                                ?
068000111017       //--------------------------------------------------------------
068100111017       BEGSR  sr_InzS01;
068200111017
068300111017         // -?Spegnimento degli indicatori di errore?
068400111017         %subst(IndDspF : 50) = *off;
068500111017
068600111017         // -?Pulizia subfile?
068700111017         SflDsp_N    = *on;
068800111017         SflDspCtl_N = *on;
068900111020         write  S7ESV2C01;
069000111017         SflDspCtl_N = *off;
069100111017         SflEnd      = *off;
069200111017
069300111017         clear  W_SflPag;
069400111017         clear  C1RcdNbr;
069500111017         clear  C1CsrRrn;
069600111017         clear  S01nrr;
069700111017         clear  V1Dmsg;
069800111017         ErrMessage  = *off;
069900111017         ErrGenerico = *off;
070000111017         $Eof = *off;
070100111017         $Err = *off;
070200111017
070300111017         // -?Preparazione stringa SQL?
070400111017         exsr  sr_PrepSQL;
070500111017
070600111017         // -?Caricamento della 1ª pagina di dati del subfile?
070700111017         exsr  sr_OpenCursor;
070800111017         exsr  sr_ReadCursor;
070900111017         exsr  sr_RollUpS01;
071000111017
071100111017         // -?Impaginazione subfile?
071200111017         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
071300111017         if S01nrr > *zero;
071400111017           C1RcdNbr  = 1;
071500111017           C1CsrRrn  = 1;
071600111017         else;
071700111017           clear C1RcdNbr;
071800111017         endif;
071900111017
072000111017         // -?Impostazione F8 nel piede del subfile?
072100111017         select;
072200111018           when  wOrdSfl = 0;
072300111017             V1PF08 = 'F8=Ord. x Cod.Errore';
072400111018           when  wOrdSfl = 1;
072500111017             V1PF08 = 'F8=Ord.x Data 1° Err';
072600111018           when  wOrdSfl = 2;
072700111017             V1PF08 = 'F8=Ord.x Data UltErr';
072800111018           when  wOrdSfl = 3;
072900111017             V1PF08 = 'F8=Ord. x Tot.gg.Err';
073000111018           when  wOrdSfl = 4;
073100111129             //V1PF08 = 'F8=Ord. x Num.Sollec';
073200111129             V1PF08 = 'F8=Ord. x Area Poc  ';
073300111109           when  wOrdSfl = 5;
073400111109             V1PF08 = 'F8=Ord. x Vers.Capp.';
073500111109           when  wOrdSfl = 6;
073600111017             V1PF08 = 'F8=Ord. x Num.Serie ';
073700111109           when  wOrdSfl = 7;
073800111018             V1PF08 = 'F8=Ord.x Cod.Cliente';
073900111017         endsl;
074000111017
074100111017       ENDSR;
074200111017
074300111017       //--------------------------------------------------------------
074400111017       //?Caricamento singola pagina nel subfile S01.                  ?
074500111017       //--------------------------------------------------------------
074600111017       BEGSR  sr_RollUpS01;
074700111017
074800111017         S01nrr    = (W_SflPag * c_SflPag);
074900111017         W_SflPag += 1;
075000111017         SflNxtChg = *off;
075100111017
075200111017         // -?Ciclo di caricamento dati nel subfile?
075300111017         DoW  S01nrr < (W_SflPag * c_SflPag)  and
075400111017              S01nrr < c_MaxRec               and
075500111017             (Not $EoF  and  Not $Err);
075600111017
075700111020           clear  S7ESV2S01;
075800111017
075900111017           exsr  sr_RieS01;
076000111017
076100111017           // -?Scrittura record nel subfile S01?
076200111017           S01nrr += 1;
076300111020           write  S7ESV2S01;
076400111017
076500111017           // -?Lettura record successivo?
076600111017           exsr  sr_ReadCursor;
076700111017
076800111017         EndDo;
076900111017
077000111017         // -?Visualizzazione del SFL (se ci sono dati)?
077100111017         SflDsp_N = (S01nrr <= *zero);
077200111017
077300111017         // -?Attivazione del SFLEND?
077400111017         //SflEnd = (S01nrr >= c_MaxRec  or  $EoF  or  $Err);
077500111017         SflEnd = ($EoF  or  $Err);
077600111017         if  S01nrr >= c_MaxRec;
077700111017           ErrMessage  = *on;
077800111017           ErrGenerico = *on;
077900111207           V1Dmsg = sk_Msg(13);
078000111017         endif;
078100111017
078200111017         // -?Posizionamento cursore al 1° rcd della pagina?
078300111017         if  S01nrr > *zero;
078400111017           wPag     = S01nrr / c_SflPag;
078500111017           wRig     = S01nrr - (c_SflPag * wPag);
078600111017           C1RcdNbr = wPag * c_SflPag;
078700111017           if  wRig > *zero;
078800111017             C1RcdNbr += 1;
078900111017           else;
079000111017             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
079100111017           endif;
079200111017         else;
079300111017           clear  C1RcdNbr;
079400111017         endif;
079500111017
079600111017         C1CsrRrn = C1RcdNbr;
079700111017
079800111017       ENDSR;
079900111017
080000111017       //--------------------------------------------------------------
080100111017       //?Valorizzazione campi del singolo record nel subfile S01.     ?
080200111017       //--------------------------------------------------------------
080300111017       BEGSR  sr_RieS01;
080400111017
080500111017           // -?Campi Hidden?
080600111017           S1Hrrn     = wWESrrn;
080700111205           S1Hdt1     = WFESVds.WESdt1;
080800111205           S1Hdt9     = WFESVds.WESdt9;
080900111205           S1Hbloc    = WFESVds.WESbloc;
081000111017
081100111017           // -?Campi di Output?
081200111018           S1Cksc   = WFESVds.WESksc;
081300111018           S1Cerr   = WFESVds.WESerr;
081400111018           if  WFESVds.WESdt1 <> *zero;
081500111017             reset  WLBdat;
081600111018             G08inv = WFESVds.WESdt1;
081700111017             xsrda8 ( WLBdat );
081800111017             if  G08err = *off;
081900111017               S1Cdt1 = G08dat;
082000111017             endif;
082100111018             S1Hdt1 = WFESVds.WESdt1;
082200111017           endif;
082300111018           if  WFESVds.WESdt9 <> *zero;
082400111017             reset  WLBdat;
082500111018             G08inv = WFESVds.WESdt9;
082600111017             xsrda8 ( WLBdat );
082700111017             if  G08err = *off;
082800111017               S1Cdt9 = G08dat;
082900111017             endif;
083000111018             S1Hdt9 = WFESVds.WESdt9;
083100111017           endif;
083200111018           S1Ctgo   = WFESVds.WEStgo;
083300111129           //S1Cnsol  = WFESVds.WESnsol;
083400111129           S1Capo   = WFESVds.WESapo;
083500111109           S1Ccver  = WFESVds.WEScver;
083600111018           S1Cnrs   = WFESVds.WESnrs;
083700111018           S1Csncd  = WFESVds.WESsncd;
083800111018           S1Csnca  = WFESVds.WESsnca;
083900111018           S1Csncc  = WFESVds.WESsncc;
084000111018           S1Dmsg   = WFESVds.WESmsg;
084100111017
084200111017           // -?Impostazione attributi di visualizzazione?
084300111205           if  WFESVds.WESbloc = *blank;
084400111205             DA00 = c_DspAtrNorm;
084500111205             DA01 = c_DspAtrNorm;
084600111205           else;
084700111205             DA00 = c_DspAtr_RI;
084800111205             DA01 = c_DspAtr_RI;
084900111205           endif;
085000111018           DA02 = c_DspAtrNorm;
085100111018           DA03 = c_DspAtrNorm;
085200111018           DA04 = c_DspAtrNorm;
085300111018           DA05 = c_DspAtrNorm;
085400111018           DA06 = c_DspAtrNorm;
085500111109           DA07 = c_DspAtrNorm;
085600111017           select;
085700111017             // -?Ordinamento per Cod. Cliente?
085800111017             when  wOrdSfl = 0;
085900111205               if  WFESVds.WESbloc = *blank;
086000111205                 DA00 = c_DspAtrRed;
086100111205               else;
086200111205                 DA00 = c_DspAtrRed_RI;
086300111205               endif;
086400111017             // -?Ordinamento per Cod. Errore / Cod. Cliente?
086500111017             when  wOrdSfl = 1;
086600111205               if  WFESVds.WESbloc = *blank;
086700111205                 DA01 = c_DspAtrRed;
086800111205               else;
086900111205                 DA01 = c_DspAtrRed_RI;
087000111205               endif;
087100111017             // -?Ordinamento per Data 1° Errore?
087200111017             when  wOrdSfl = 2;
087300111017               DA02 = c_DspAtrRed;
087400111017             // -?Ordinamento per data Ultimo Errore?
087500111017             when  wOrdSfl = 3;
087600111017               DA03 = c_DspAtrRed;
087700111017             // -?Ordinamento per Tot. Giorni con Errore?
087800111017             when  wOrdSfl = 4;
087900111017               DA04 = c_DspAtrRed;
088000111129             //// -?Ordinamento per Numero Solleciti?
088100111129             // -?Ordinamento per Area Poc?
088200111018             when  wOrdSfl = 5;
088300111018               DA05 = c_DspAtrRed;
088400111109             // -?Ordinamento per Versione Cappario?
088500111109             when  wOrdSfl = 6;
088600111109               DA06 = c_DspAtrRed;
088700111017             // -?Ordinamento per Serie / Ultimo Segnacollo?
088800111109             when  wOrdSfl = 7;
088900111109               DA07 = c_DspAtrRed;
089000111017           endsl;
089100111017
089200111017       ENDSR;
089300111017
089400111017       //--------------------------------------------------------------
089500111017       //?Gestione opzioni del subfile S01.                            ?
089600111017       //--------------------------------------------------------------
089700111017       BEGSR  sr_OpzS01;
089800111017
089900111017         clear  SavS01csr;
090000111017
090100111017         // -?Ciclo di lettura subfile?
090200111020         readc  S7ESV2S01;
090300111017
090400111020         DoW  Not %eof(TIS7ESVD2);
090500111017
090600111017           SflNxtChg = *off;
090700111017           %subst(IndDspF : 50) = *off;
090800111017           C1RcdNbr = S01nrr;
090900111017
091000111017           select;
091100111017
091200121120             // -?2=Gestione, 5=Visualizzazione?
091300121120             when  S1Copz = '2'  or  S1Copz = '5';
091400111205               exsr  sr_OpzS01_02;
091500111017
091600111017             // -?Nessuna opzione?
091700111017             when  S1Copz = *blank;
091800111017
091900111017             // -?Opzione errata?
092000111017             other;
092100111017               ErrMessage  = *on;
092200111017               ErrGenerico = *on;
092300111017               PosCurOPZ   = *on;
092400111207               V1Dmsg = sk_Msg(14);
092500111017
092600111017           endsl;
092700111017
092800111017           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
092900111017           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
093000111017           if  S1Copz  <> *blank   and
093100111017              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
093200111017             SavS01csr   = C1RcdNbr;
093300111017           endif;
093400111017
093500111017           // -?Aggiornamento sfl?
093600111017           select;
093700111017             when  ErrMessage;
093800111017               SflNxtChg = *on;
093900111017               C1CsrRrn  = C1RcdNbr;
094000111017             when  ErrGenerico;
094100111017               C1CsrRrn  = C1RcdNbr;
094200111017               clear  S1Copz;
094300111017             other;
094400111017               clear  S1Copz;
094500111017           endsl;
094600111205           exsr  sr_DspAttrS01;
094700111020           UPDATE  S7ESV2S01;
094800111017           NotDisplayed  = *off;
094900111017           if  ErrGenerico   or   ErrMessage;
095000111017             leave;
095100111017           endif;
095200111017
095300111020           readc  S7ESV2S01;
095400111017
095500111017         ENDDO;
095600111017
095700111017         // -?Riposiziono il cursore sul record contenente la prima opz.?
095800111017         //  ?(se non sono stati rilevati errori)?
095900111017         if  NOT ErrMessage   and   NOT ErrGenerico   and
096000111017             SavS01csr > *zero;
096100111017           C1CsrRrn = SavS01csr;
096200111017         endif;
096300111017
096400111017         // -?Se richiesto il ri-caricamento del subfile: occorre?
096500111017         //  ?prima chiudere il cursore SQL (attualmente aperto)?
096600111017         if  $InzS01 = *on;
096700111017           exsr  sr_CloseCursor;
096800111017         endif;
096900111017
097000111017       ENDSR;
097100111205
097200111205       //--------------------------------------------------------------
097300111205       //?Impostazione attributi di visualizzazione nel subfile S1.    ?
097400111205       //--------------------------------------------------------------
097500111205       BEGSR  sr_DspAttrS01;
097600111205
097700111205         Select;
097800111205
097900111205           // -?Ord. 0 = Codice Cliente?
098000111205           When  wOrdSfl = 0  and  S1Hbloc = *blank;
098100111205             DA00 = c_DspAtrRed;
098200111207             DA01 = c_DspAtrNorm;
098300111205           When  wOrdSfl = 0  and  S1Hbloc = 'S';
098400111205             DA00 = c_DspAtrRed_RI;
098500111205             DA01 = c_DspAtr_RI;
098600111205
098700111205           // -?Ord. 1 = Codice Errore?
098800111205           When  wOrdSfl = 1  and  S1Hbloc = *blank;
098900111205             DA00 = c_DspAtrNorm;
099000111205             DA01 = c_DspAtrRed;
099100111205           When  wOrdSfl = 1  and  S1Hbloc = 'S';
099200111205             DA00 = c_DspAtr_RI;
099300111205             DA01 = c_DspAtrRed_RI;
099400111205
099500111205           // -?Altri ordinamenti?
099600111205           When  wOrdSfl > 1  and  S1Hbloc = *blank;
099700111205             DA00 = c_DspAtrNorm;
099800111205             DA01 = c_DspAtrNorm;
099900111205           When  wOrdSfl > 1  and  S1Hbloc = 'S';
100000111205             DA00 = c_DspAtr_RI;
100100111205             DA01 = c_DspAtr_RI;
100200111205
100300111205         EndSl;
100400111205
100500111205       ENDSR;
100600111017
100700111017       //--------------------------------------------------------------
100800111017       //?Gestione tasto funzionale F8 = Ordinamento subfile.          ?
100900111017       //--------------------------------------------------------------
101000111017       BEGSR  sr_F08S01;
101100111017
101200111017         exsr  sr_CloseCursor;
101300111017
101400111017         reset  $InzS01;
101500111017
101600111109         if  wOrdSfl < 7;
101700111017           wOrdSfl += 1;
101800111017         else;
101900111017           clear  wOrdSfl;
102000111017         endif;
102100111017
102200111017       ENDSR;
102300111017
102400111017       //--------------------------------------------------------------
102500111205       //?Gestione singolo Errore del file WFESV00F.                   ?
102600111017       //--------------------------------------------------------------
102700111205       BEGSR  sr_OpzS01_02;
102800111017
102900111018         $VideoPrec = $Video;
103000111018         $Video     = 'D2';
103100111018         $InzD02    = *on;
103200111017
103300111017         // -?Per uscire dal ciclo di gestione opzioni e gestire la?
103400111017         //  ?videata D2 dal ciclo principale?
103500111017         //  ?=> Ma sorgono problemi con eventuali altre opzioni già?
103600111017         //     ?inserite?
103700111017         //ErrGenerico = *on;
103800111017
103900111017         // -?Per gestire ogni singola videata D2 da dentro il ciclo?
104000111017         //  ?di gestione opzioni:?
104100111017         DoW  $Video = 'D2';
104200111017           exsr  sr_GesD02;
104300111017         EndDo;
104400111017
104500111017       ENDSR;
104600111017
104700111017       //--------------------------------------------------------------
104800111017       //?Gestione videata D02 - Visualizzazione singolo ERR di WFESV. ?
104900111017       //--------------------------------------------------------------
105000111017       BEGSR  sr_GesD02;
105100111017
105200111017         // -?Inizializzazione videata?
105300111017         if  $InzD02 = *on;
105400111017           exsr  sr_InzD02;
105500111017           $InzD02 = *off;
105600111017         endif;
105700121120
105800121120         // -?(Dis)Abilitazione tasti funzionali?
105900121120         F6Attivo = (S1Copz = '2');
106000121120         F16Attivo = F6Attivo;
106100111017
106200111017         // -?Emissione Testata?
106300111020         write  S7ESV2T01;
106400111017
106500111017         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
106600121120         if  S1Copz = '2';
106700121120           exfmt  S7ESV2D02;
106800121120         else;
106900121120           write  S7ESV2D02;
107000121120           exfmt  Protect;
107100121120         endif;
107200111017
107300111017         reset  ErrMessage;
107400111017         reset  ErrGenerico;
107500111017         clear  V1Dmsg;
107600111017         NotDisplayed = *off;
107700111017
107800111017         SELECT;
107900111017
108000111017           // -?F3=Fine?
108100111017           WHEN  dsp_aid = c_F03;
108200111017             ErrGenerico = *on;
108300111018             $Video = $VideoPrec;
108400111104
108500111104           // -?F6=Aggiornamento flag "Blocco dell'invio eMail al Poc"?
108600111104           WHEN  dsp_aid = c_F06;
108700111104             chain(E)  S1Hrrn  WFESV000;
108800111104             select;
108900111104               when  Not %found(WFESV00F);
109000111104                 ErrGenerico = *on;
109100111104                 ErrMessage  = *on;
109200111104                 V1Dmsg = 'Record NON trovato';
109300111104               when  %error;
109400111104                 ErrGenerico = *on;
109500111104                 ErrMessage  = *on;
109600111104                 V1Dmsg = 'Aggiornamento NON riuscito';
109700111104               other;
109800111104                 WESbloc = V2Cbloc;
109900111104                 UPDATE  WFESV000;
110000111205                 S1Hbloc = WESbloc;
110100111104                 $Video = $VideoPrec;
110200111104                 //reset  $InzS01;
110300111104             endsl;
110400111018
110500111018           // -?F9=Tabella "3C"?
110600111018           WHEN  dsp_aid = c_F09;
110700111018             exsr  sr_Tab3C;
110800111017
110900111017           // -?F12=Ritorno?
111000111017           WHEN  dsp_aid = c_F12;
111100111018             $Video = $VideoPrec;
111200111017
111300111017           // -?F16=Cancellazione?
111400111017           WHEN  dsp_aid = c_F16;
111500111017             delete(E)  S1Hrrn  WFESV000;
111600111017             select;
111700111017               when  Not %found(WFESV00F);
111800111018                 //$Video = 'S1';
111900111018                 $Video = $VideoPrec;
112000111017                 reset  $InzS01;
112100111017               when  %error;
112200111017                 ErrGenerico = *on;
112300111017                 ErrMessage  = *on;
112400111017                 V1Dmsg = 'Cancellazione NON riuscita';
112500111017               other;
112600111020                 //clear  S7ESV2S01;
112700111017                 NotDisplayed = *on;
112800111018                 //$Video = 'S1';
112900111018                 $Video = $VideoPrec;
113000111017                 reset  $InzS01;
113100111017             endsl;
113200120514
113300120514           // -?F20=Interrogazione log Upload cliente?
113400120514           WHEN  dsp_aid = c_F20;
113500120514             exsr  sr_CallTIS760;
113600120514
113700120514           // -?F21=Interrogazione log Upload EasyWEB?
113800120515           //WHEN  dsp_aid = c_F21;
113900120515           //  exsr  sr_CallTIS760;
114000111017
114100111017         ENDSL;
114200111017
114300111017       ENDSR;
114400111017
114500111017       //--------------------------------------------------------------
114600111017       //?Inizializzazione videata D02 - Visualizzazione singolo ERR.  ?
114700111017       //--------------------------------------------------------------
114800111017       BEGSR  sr_InzD02;
114900111017
115000111020         clear  S7ESV2D02;
115100111017
115200111017         // -?Eventuale apertura del file WFESV00F?
115300111017         if  Not %open(WFESV00F);
115400111017           open  WFESV00F;
115500111017         endif;
115600111017
115700111017         // -?Aggancio record x reperimento dati?
115800111017         chain(n)  S1Hrrn  WFESV000;
115900111017
116000111017         if  Not %found(WFESV00F);
116100111017           leavesr;
116200111017         endif;
116300111017
116400111017         V2Cksc   = WESksc;
116500111017         V2Cerr   = WESerr;
116600111017
116700111017         V2Dmsg   = WESmsg;
116800111017
116900111017         if  WESdt1 > *zero;
117000111017           reset  WLBdat;
117100111017           G08inv = WESdt1;
117200111017           xsrda8 ( WLBdat );
117300111017           if  G08err = *off;
117400111017             V2Cdt1 = G08dat;
117500111017           endif;
117600111017         endif;
117700111017         if  WESdt9 > *zero;
117800111017           reset  WLBdat;
117900111017           G08inv = WESdt9;
118000111017           xsrda8 ( WLBdat );
118100111017           if  G08err = *off;
118200111017             V2Cdt9 = G08dat;
118300111017           endif;
118400111017         endif;
118500111017
118600111017         V2Ctgo   = WEStgo;
118700111104         V2Cnsol  = WESnsol;
118800111104         V2Capo   = WESapo;
118900111121         if  WESdusol > *zero;
119000111121           reset  WLBdat;
119100111121           G08inv = WESdusol;
119200111121           xsrda8 ( WLBdat );
119300111121           if  G08err = *off;
119400111121             V2Cdusol = G08dat;
119500111121           endif;
119600111121         endif;
119700111104         V2Cbloc  = WESbloc;
119800111104
119900111017         V2Cever  = WESever;
120000111017         V2Ccver  = WEScver;
120100111018         if  WEScdsc > *zero;
120200111018           reset  WLBdat;
120300111018           G08inv = WEScdsc;
120400111018           xsrda8 ( WLBdat );
120500111018           if  G08err = *off;
120600111018             V2Ccdsc = G08dat;
120700111018           endif;
120800111018         endif;
120900111017         V2Csncd  = WESsncd;
121000111017         V2Csnca  = WESsnca;
121100111017         V2Csncc  = WESsncc;
121200111017         V2Cnrs   = WESnrs;
121300111017         V2Cobj   = WESobj;
121400111017         V2Ccorpo = WEScorpo;
121500111017         V2Ctypweb  = WEStypweb;
121600111017         V2Ccvermax = WEScvermax;
121700111017         V2Ccddemax = WEScddemax;
121800111018         if  WEScddemax > *zero;
121900111018           reset  WLBdat;
122000111018           G08inv = WEScddemax;
122100111018           xsrda8 ( WLBdat );
122200111018           if  G08err = *off;
122300111018             V2Ccddemax = G08dat;
122400111018           endif;
122500111018         endif;
122600111018         if  WEScdscmax > *zero;
122700111018           reset  WLBdat;
122800111018           G08inv = WEScdscmax;
122900111018           xsrda8 ( WLBdat );
123000111018           if  G08err = *off;
123100111018             V2Ccdscmax = G08dat;
123200111018           endif;
123300111018         endif;
123400111017
123500111017         // -?Decodifica cliente?
123600111017         clear  TIBS69ds;
123700111017         I69sif = knsif;
123800111017         I69kcc = DUTkci;
123900111128         I69kac = V2Cksc;
124000111017         tibs69r( tibs69ds :
124100111017                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
124200111017         if  O69err = *on;
124300111207           //VIDmsg = sk_Msg(??);
124400111017           //PosCurKSC   = *on;
124500111017           //ErrMessage  = *on;
124600111017           //ErrGenerico = *on;
124700111017           //leavesr;
124800111018           V2Dksc = *all'? ';
124900111017         else;
125000111018           V2Dksc = %subst( ACOrag : 1 : %len(V2Dksc) );
125100111017         endif;
125200120508
125300120508         // -?Decodifica Poc?
125400120508         chain  ( V2Capo )  AZORG;
125500120508         if  %found(AZORG01L)  and
125600120508             ORGfva = *blank;
125700120508           if  %len( %trimr( ORGdes ) ) <= %size( V2Dapo );
125800120508             V2Dapo  = ORGdes;
125900120508           else;
126000120508             V2Dapo2 = ORGdes;
126100120508           endif;
126200120508         endif;
126300111018
126400111018         // -?Decodifica tipo aggiornamento web?
126500111018         if  WEStypweb <> *blank   or   WESever >= c_VersEsySp;
126600111207           xx = %lookup ( WEStypweb : sk_TypWeb );
126700111018           if  xx > *zero;
126800111207             V2Dtypweb = sk_TypWebD(xx);
126900111018           else;
127000111018             V2Dtypweb = *all'? ';
127100111018           endif;
127200111018         endif;
127300120716
127400120716         // -?Decodifica cliente in "Oggetto e-mail"?
127500120716         if  V2Cobj <> *blank   and
127600120716             %check( c_Digits : %trim( V2Cobj ) ) = *zero;
127700120716           Evid_Obj = (%int( %trim( V2Cobj ) ) <> V2Cksc);
127800120716           if  Evid_Obj;
127900120716             clear  TIBS69ds;
128000120716             I69sif = knsif;
128100120716             I69kcc = DUTkci;
128200120716             I69kac = %int( %trim( V2Cobj ) );
128300120716             tibs69r( tibs69ds :
128400120716                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
128500120716             if  O69err = *on;
128600120716               V2Dobj = *all'? ';
128700120716             else;
128800120716               V2Dobj = %subst( ACOrag : 1 : %len( V2Dobj ) );
128900120716             endif;
129000120716           else;
129100120716             V2Dobj = V2Dksc;
129200120716           endif;
129300120716         endif;
129400111017
129500111017       ENDSR;
129600111018
129700111018       //--------------------------------------------------------------
129800111109       //?Gestione window-subfile SW1 - Elenco Codici Errore.          ?
129900111018       //--------------------------------------------------------------
130000111109       BEGSR  sr_GesSW1;
130100111109
130200111109         // -?Inizializzazione subfile?
130300111109         if  $InzSW1;
130400111109           exsr  sr_InzSW1;
130500111109           $InzSW1 = *off;
130600111109         endif;
130700111109
130800111109         // -?Posizionamento cursore?
130900111109         //CWRcdNbr = 1;
131000111018
131100111018         // -?Emissione Window?
131200111109         write  S7ESV2PW1;
131300111109         exfmt  S7ESV2CW1;
131400111018
131500111018         reset  ErrMessage;
131600111018         reset  ErrGenerico;
131700111018         clear  V1Dmsg;
131800111018
131900111018         // -?F12=Ritorno?
132000111018         if  dsp_aid = c_F12;
132100111018           $Video = $VideoPrec;
132200111018         endif;
132300111018
132400111018       ENDSR;
132500111109
132600111109       //--------------------------------------------------------------
132700111109       //?Inizializzazione window-subfile S01.                         ?
132800111109       //--------------------------------------------------------------
132900111109       BEGSR  sr_InzSW1;
133000111109
133100111109         // -?Pulizia subfile?
133200111109         WSflDsp_N    = *on;
133300111109         WSflDspCtl_N = *on;
133400111109         write  S7ESV2CW1;
133500111109         WSflDspCtl_N = *off;
133600111109         WSflEnd      = *off;
133700111109
133800111109         //clear  CWRcdNbr;
133900111109         //clear  CWCsrRrn;
134000111109         clear  SW1nrr;
134100111109         clear  V1Dmsg;
134200111109         ErrMessage  = *off;
134300111109         ErrGenerico = *off;
134400111109
134500111109         // -?Caricamento completo del subfile?
134600111207         For  SW1nrr = 1  To  %elem(sk_Err);
134700111207           SW1err = sk_Err(SW1nrr);
134800111207           SW1msg = sk_ErrD(SW1nrr);
134900111109           write  S7ESV2SW1;
135000111109         EndFor;
135100111109
135200111109         // -?Visualizzazione del SFL (se ci sono dati)?
135300111109         WSflDsp_N = (SW1nrr <= *zero);
135400111109
135500111109         // -?Attivazione del SFLEND?
135600111207         WSflEnd = (SW1nrr >= %elem(sk_Err));
135700111109
135800111109         // -?Impaginazione subfile e?
135900111109         //  ?posizionamento cursore al 1° rcd del subfile?
136000111109         //if  SW1nrr > *zero;
136100111109         //  CWRcdNbr = 1;
136200111109         //else;
136300111109         //  clear  CWRcdNbr;
136400111109         //endif;
136500111109         //
136600111109         //CWCsrRrn = CWRcdNbr;
136700111109
136800111109       ENDSR;
136900111018
137000111018       //--------------------------------------------------------------
137100111018       //?Richiamo *pgm TRTB28R per gestione tabella "3C".             ?
137200111018       //--------------------------------------------------------------
137300111018       BEGSR  sr_Tab3C;
137400111018
137500111018         KPJBU = %editc( V2Cksc : 'X' );
137600111018
137700111018         trtb28r ( kpjba );
137800111018
137900111018       ENDSR;
138000120514
138100120514       //--------------------------------------------------------------
138200120514       //?Richiamo *pgm TIS760R per interrogazione Strategi Upload Log.?
138300120514       //--------------------------------------------------------------
138400120514       BEGSR  sr_CallTIS760;
138500120514
138600120514         reset  Prm_TIS760ds;
138700120515         //DEPop0 = 'P05';         ?(già così)?
138800120514         //select;
138900120514         //  when  dsp_aid = c_F20;
139000120514             DEPksc = '0' + %editc( V2Cksc : 'X' );
139100120514         //  when  dsp_aid = c_F21;
139200120514         //    DEPksc = '0EASYWEB';
139300120514         //    DEPnfil = %editc( V2Cksc : 'X' );
139400120514         //endsl;
139500120515         //clear  DEPtip;          ?(già così)?
139600120514
139700120514         kpjbu = Prm_TIS760ds;
139800120514
139900120514         tis760r ( kpjba );
140000120514
140100120514       ENDSR;
140200111017
140300111017       //--------------------------------------------------------------
140400111017       //?Preparazione stringa SQL S1.                                 ?
140500111017       //--------------------------------------------------------------
140600111017       BEGSR  sr_PrepSQL;
140700111017
140800111017         clear  wCountSel;
140900111017         wSQL = 'select rrn(WFESV00F), WFESV00F.* from WFESV00F';
141000111017
141100111017         // -?Selezione per filiale cliente (gestibile dall'utente)?
141200111017         if  %subst( Knmus : 1 : 3 ) <> 'EDP';
141300111017           if  wCountSel = *zero;
141400111017             wSQL += ' where';
141500111017           else;
141600111017             wSQL += ' and';
141700111017           endif;
141800111017           wSQL += ' int( substr( digits( WESksc ), 1, 3 ) ) in (';
141900111017           For  xx = 1  To  %elem($POg);
142000111017             if  $POg(xx) <= *zero;
142100111017               leave;
142200111017             endif;
142300111017             if  xx > 1;
142400111017               wSQL += ', ';
142500111017             endif;
142600111017             wSQL += $POg(xx);
142700111017           EndFor;
142800111017           wSQL += ')';
142900111017           wCountSel += 1;
143000111017         endif;
143100111031
143200111109         // -?Selezione per codice Cliente?
143300111031         if  V1Cksc <> *zero;
143400111031           if  wCountSel = *zero;
143500111031             wSQL += ' where';
143600111031           else;
143700111031             wSQL += ' and';
143800111031           endif;
143900111031           wSQL += ' WESksc = ' + %triml( %editc( V1Cksc : '3' ) );
144000111031           wCountSel += 1;
144100111031         endif;
144200111017
144300111017         // -?Selezione per codice Errore?
144400111017         if  V1Cerr <> *blank;
144500111017           if  wCountSel = *zero;
144600111017             wSQL += ' where';
144700111017           else;
144800111017             wSQL += ' and';
144900111017           endif;
145000111017           wSQL += ' WESerr in (';
145100111017           clear  wCountErr;
145200111207           For  xx = 1  To  %elem(sk_V1Cerr);
145300111207             if  sk_V1Cerr(xx) <> *blank;
145400111017               if  wCountErr > *zero;
145500111017                 wSQL += ', ';
145600111017               endif;
145700111207               wSQL += '''' + sk_V1Cerr(xx) + '''';
145800111017               wCountErr += 1;
145900111017             endif;
146000111017           EndFor;
146100111017           wSQL += ')';
146200111017           wCountSel += 1;
146300111017         endif;
146400111017
146500111017         // -?Selezione per data occorrenza?
146600111017         if  V1Cdto > *zero;
146700111017           if  wCountSel = *zero;
146800111017             wSQL += ' where';
146900111017           else;
147000111017             wSQL += ' and';
147100111017           endif;
147200111018           wSQL += ' ' + %char(W1Cdto) + ' between WESdt1 and WESdt9';
147300111017           wCountSel += 1;
147400111017         endif;
147500111024
147600111024         // -?Selezione per totale giorni occorrenza?
147700111024         if  V1Ctgo <> *zero;
147800111024           if  wCountSel = *zero;
147900111024             wSQL += ' where';
148000111024           else;
148100111024             wSQL += ' and';
148200111024           endif;
148300111024           wSQL += ' WEStgo >= ' + %char( V1Ctgo );
148400111024           wCountSel += 1;
148500111024         endif;
148600111109
148700111109         // -?Selezione per numero solleciti inviati?
148800111109         if  V1Cnsol > *zero;
148900111109           if  wCountSel = *zero;
149000111109             wSQL += ' where';
149100111109           else;
149200111109             wSQL += ' and';
149300111109           endif;
149400111109           wSQL += ' WESnsol >= ' + %trim( %editc( V1Cnsol : '3' ) );
149500111109           wCountSel += 1;
149600111109         endif;
149700111109
149800111109         // -?Selezione clienti senza solleciti inviati?
149900111109         if  V1Cnsol0 = 'S';
150000111109           if  wCountSel = *zero;
150100111109             wSQL += ' where';
150200111109           else;
150300111109             wSQL += ' and';
150400111109           endif;
150500111109           wSQL += ' WESnsol = 0';
150600111109           wCountSel += 1;
150700111109         endif;
150800111109
150900111109         // -?Selezione per area Poc di invio sollecito?
151000111109         if  V1Capo > *zero;
151100111109           if  wCountSel = *zero;
151200111109             wSQL += ' where';
151300111109           else;
151400111109             wSQL += ' and';
151500111109           endif;
151600111109           wSQL += ' WESapo = ' + %trim( %editc( V1Capo : '3' ) );
151700111109           wCountSel += 1;
151800111109         endif;
151900111109
152000111109         // -?Selezione per Errori con invio sollecito bloccato?
152100111109         if  V1Cbloc <> *blank;
152200111109           if  wCountSel = *zero;
152300111109             wSQL += ' where';
152400111109           else;
152500111109             wSQL += ' and';
152600111109           endif;
152700111109           if  V1Cbloc = 'S';
152800111109             wSQL += ' WESbloc = ''S''';
152900111109           else;
153000111109             wSQL += ' WESbloc = '' ''';
153100111109           endif;
153200111109           wCountSel += 1;
153300111109         endif;
153400111024
153500111109         // -?Selezione per Versione Cappario?
153600111109         if  V1Ccver <> *zero;
153700111024           if  wCountSel = *zero;
153800111024             wSQL += ' where';
153900111024           else;
154000111024             wSQL += ' and';
154100111024           endif;
154200111109           wSQL += ' WEScver = ' + %trim( %editc( V1Ccver : '3' ) );
154300111024           wCountSel += 1;
154400111024         endif;
154500111024
154600111024         // -?Selezione per numero serie?
154700111024         if  V1Cnrs <> *zero;
154800111024           if  wCountSel = *zero;
154900111024             wSQL += ' where';
155000111024           else;
155100111024             wSQL += ' and';
155200111024           endif;
155300111024           wSQL += ' WESnrs = ' + %trimr( %char( V1Cnrs ) );
155400111024           wCountSel += 1;
155500111024         endif;
155600111017
155700111017         // -?Ordinamento?
155800111017         select;
155900111017           // -?Ordinamento per Codice Cliente?
156000111017           when  wOrdSfl = 0;
156100111017             wSQL += ' order by WESksc, WESerr';
156200111017           // -?Ordinamento per Codice Errore?
156300111017           when  wOrdSfl = 1;
156400111017             wSQL += ' order by WESerr, WESksc';
156500111017           // -?Ordinamento per Data 1° Errore?
156600111017           when  wOrdSfl = 2;
156700111017             wSQL += ' order by WESdt1, WESksc, WESerr';
156800111017           // -?Ordinamento per Data Ultimo errore?
156900111017           when  wOrdSfl = 3;
157000111017             wSQL += ' order by WESdt9 desc, WESksc, WESerr';
157100111017           // -?Ordinamento per Totale Giorni Occorrenza?
157200111017           when  wOrdSfl = 4;
157300111017             wSQL += ' order by WEStgo, WESksc, WESerr';
157400111129           //// -?Ordinamento per Numero Solleciti?
157500111129           // -?Ordinamento per Area Poc?
157600111018           when  wOrdSfl = 5;
157700111129             //wSQL += ' order by WESnsol, WESksc, WESerr';
157800111129             wSQL += ' order by WESapo, WESksc, WESerr';
157900111109           // -?Ordinamento per Versione Cappario?
158000111109           when  wOrdSfl = 6;
158100111109             wSQL += ' order by WEScver, WESksc, WESerr';
158200111017           // -?Ordinamento per Serie/Segnacollo?
158300111109           when  wOrdSfl = 7;
158400111017             wSQL += ' order by WESnrs, WESsncd, WESsnca desc';
158500111017         endsl;
158600111017
158700111017         wSQL += ' for fetch only';
158800111017
158900111017       ENDSR;
159000111017
159100111017       //--------------------------------------------------------------
159200111017       //?Apertura cursore C1.                                         ?
159300111017       //--------------------------------------------------------------
159400111017       BEGSR  sr_OpenCursor;
159500111017
159600111017         // -?Dichiarazione cursore?
159700111017         exec sql   prepare S1   from :wSQL;
159800111017         exec sql   declare C1   cursor for S1;
159900111017
160000111017         // -?Apertura del cursore?
160100111017         exec sql   open C1;
160200111017
160300111017         if  SQLcode < *zero;
160400111017           $Err = *on;
160500111017           $EoF = *on;
160600111017           exsr  sr_PrintErrSQL;
160700111017         endif;
160800111017
160900111017       ENDSR;
161000111017
161100111017       //--------------------------------------------------------------
161200111017       //?Chiusura cursore C1.                                         ?
161300111017       //--------------------------------------------------------------
161400111017       BEGSR  sr_CloseCursor;
161500111017
161600111017         // -?Chiusura del cursore?
161700111017         exec sql   close C1;
161800111017
161900111017       ENDSR;
162000111017
162100111017       //--------------------------------------------------------------
162200111017       //?Lettura cursore C1.                                          ?
162300111017       //--------------------------------------------------------------
162400111017       BEGSR  sr_ReadCursor;
162500111017
162600111017         clear  WFESVds;
162700111017
162800111017         exec sql   fetch next   from C1   into :wWESrrn, :WFESVds;
162900111017
163000111017         select;
163100111017           when  SQLcode = 100;
163200111017             $EoF = *on;
163300111017           when  SQLcode < *zero;
163400111017             $Err = *on;
163500111017             $Eof = *on;
163600111017             exsr  sr_PrintErrSQL;
163700111017         endsl;
163800111017
163900111017       ENDSR;
164000111017
164100111017       //--------------------------------------------------------------
164200111017       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
164300111017       //--------------------------------------------------------------
164400111017       BEGSR  sr_PrintErrSQL;
164500111017
164600111017         // -?Stampa del Dump?
164700111017         Dump(A);
164800111017
164900111017         // -?Stampa del Job-Log?
165000111017         Qcmd = 'DSPJOBLOG job(*) output(*print)';
165100111017         exsr  sr_ExecCmd;
165200111017
165300111017         // -?Chiusura del programma?
165400111017         exsr  sr_RoutEnd;
165500111017
165600111017       ENDSR;
165700111017
165800111017       //--------------------------------------------------------------
165900111017       //?Esecuzione del comando (già impostato).                      ?
166000111017       //--------------------------------------------------------------
166100111017       BEGSR  sr_ExecCmd;
166200111017
166300111017         clear Qcap0100;
166400111017         Qcabcsdh = *off;
166500111017         Qcapa    = *off;
166600111017         Qcacmdss = *off;
166700111017         Qcaerved = *allX'00';
166800111017
166900111017         clear Qusec;
167000111017         Qusbprv  = %size(Qusec);
167100111017
167200111017         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
167300111017                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
167400111017                           0 : 0 : Qusec);
167500111017
167600111017         // -?Invio *msg per segnalazione errore?
167700111017         //if  Qusei <> *blank;
167800111017         //  ...;
167900111017         //endif;
168000111017
168100111017       ENDSR;
168200111017
168300111017       //--------------------------------------------------------------
168400111017       //?Operazioni finali.                                           ?
168500111017       //--------------------------------------------------------------
168600111017       BEGSR  sr_RoutEnd;
168700111017
168800111017         // -?Chiusura archivi?
168900111017         if  %open(WFESV00F);
169000111017           close  WFESV00F;
169100111017         endif;
169200111017
169300111017         // -?Chiusura applicazioni?
169400111017         reset  TIBS69ds;
169500111017         tibs69r( tibs69ds :
169600111017                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
169700111017
169800111017         // -?Uscita dal *pgm?
169900111017         return;
170000111017
170100111017       ENDSR;
170200111017
170300111017      /end-free
170400111017
170500111017       //--------------------------------------------------------------
170600111017       //?Schiere a tempo di compilazione.                             ?
170700111017       //--------------------------------------------------------------
170800111017
170900111207** -?sk_TypWeb / sk_TypWebD?---*?Tipi Aggiornamento Web?
1710001110180  - Funzionalità NON attiva
1711001110181  - Funzionalità attiva
1712001110182  - Framework 1.0
1713001110184  - Framework 1.1
1714001110186  - Framework 1.1 & 1.0
1715001110188  - Framework 2.0
17160011101810 - Framework 2.0 & 1.0
17170011101812 - Framework 2.0 & 1.1
171800111207** -?sk_Err / sk_ErrD:?Codici ERRORE e relative descrizioni?------------*
17190011110901=Il cliente NON è iscritto per invio dati.                            ?               ATTENZIONE:
17200012021402=Ricevuti dati da cliente che risulta abilitato ad EasySped WEB.      ?               ATTENZIONE:
17210012021403=Cliente NON abilitato né al supporto EasySped, né ad EasySped WEB.   ?               ATTENZIONE:
17220011112904=Ricevuti dati da cliente con serie diversa da quella abilitata.      ?               ATTENZIONE:
17230011112305=Ricevuti dati con versione cappario diversa da quella in vigore.     ?               ATTENZIONE:
172400111207** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
172500111018Funzione eseguibile solo in ambiente di SEDE                                   1
172600111018Nessuna filiale gestibile dall'utente.  Premere "Invio" per uscire.            2
172700111207Codice Errore errato.  Previsti: da 01 a 05                                    3
172800111018Data formalmente errata                                                        4
172900111024Totale giorni occorrenza NON può essere negativo                               5
173000111109Numero solleciti inviati NON può essere negativo                               6
173100111109Selezione "SENZA  solleciti inviati" errata: S=Sì, _=No                        7
173200111109Selezione alternativa a quella per "Numero solleciti inviati"                  8
173300111122Selezione alternativa a quella per "SENZA  solleciti inviati"                  9
173400111122Area Poc di invio solleciti errata                                            10
173500111122Selezione "Errori con invio sollecito bloccato" errata: _/S/N                 11
173600111122Numero serie NON può essere negativo                                          12
173700111122SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)        13
173800111122Opzione errata                                                                14
