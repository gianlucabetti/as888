000100101223       //==============================================================
000200101223       //?FNLR65R * Interrogazione LOG Immagini LdV                    ?
000300101223       //==============================================================
000400101223
000500101223       //--------------------------------------------------------------
000600101223       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700101223       //--------------------------------------------------------------
000800101223
000900120703     /*PRM  dbgview(*source)
001000110401     /*CMD  ovrdbf file(FNARB01L) tofile(FILTRAPRD/FNARB01L) +
001100110401     /*CMD         ovrscope(*calllvl)
001200110401     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001300110401     /*CMD         ovrscope(*calllvl)
001400110401     /*END  dltovr file(FNARB01L TITAS30C) lvl(*)
001500101223     /*END
001600101223
001700101223       //--------------------------------------------------------------
001800101223       //?Specifiche di controllo.                                     ?
001900101223       //--------------------------------------------------------------
002000101223
002100101223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002200110209     h dftactgrp(*no) bnddir('TRUL')
002300101223     h alwnull(*inputonly)
002400101223
002500101223       //--------------------------------------------------------------
002600101223       //?Dichiarazione file.                                          ?
002700101223       //--------------------------------------------------------------
002800110103
002900110216       // -?Bolle Arrivo?
003000110216     fFNARB01L  if   e           k disk    usropn
003100110216       // -?Bolle Sede?
003200110216     fTITAS30C  if   e           k disk    usropn
003300110216
003400110208       // -?Immagini Lettere di Vettura?
003500110208     fFIILV00F  Uf   e             disk    usropn
003600101223
003700101223       // -?Video Gestione?
003800101223     fFNLR65D   cf   e             workstn
003900101223     f                                     sfile(LR65S01 : S01nrr)
004000101223     f                                     indds(IndDspF)
004100101223     f                                     infds(InfDspF)
004200101223
004300101223       //--------------------------------------------------------------
004400101223       //?Definizione costanti.                                        ?
004500101223       //--------------------------------------------------------------
004600101223
004700101223       // -?Numero di record in ciascuna videata di subfile?
004800101223     d c_SflPag        c                   const(16)
004900101223
005000110228       // -?Numero massimo di record gestibili nel subfile?
005100110228     d c_MaxRec        c                   const(9999)
005200110218
005300110218       // -?Estensione dell'immagine della LdV?
005400110218     d c_Extension     c                   const('.tif')
005500101223
005600101223       // -?Tasti funzionali a video?
005700101223     d c_F01           c                   const(x'31')
005800101223     d c_F02           c                   const(x'32')
005900101223     d c_F03           c                   const(x'33')
006000101223     d c_F04           c                   const(x'34')
006100101223     d c_F05           c                   const(x'35')
006200101223     d c_F06           c                   const(x'36')
006300101223     d c_F07           c                   const(x'37')
006400101223     d c_F08           c                   const(x'38')
006500101223     d c_F09           c                   const(x'39')
006600101223     d c_F10           c                   const(x'3A')
006700101223     d c_F11           c                   const(x'3B')
006800101223     d c_F12           c                   const(x'3C')
006900101223     d c_F13           c                   const(x'B1')
007000101223     d c_F14           c                   const(x'B2')
007100101223     d c_F15           c                   const(x'B3')
007200101223     d c_F16           c                   const(x'B4')
007300101223     d c_F17           c                   const(x'B5')
007400101223     d c_F18           c                   const(x'B6')
007500101223     d c_F19           c                   const(x'B7')
007600101223     d c_F20           c                   const(x'B8')
007700101223     d c_F21           c                   const(x'B9')
007800101223     d c_F22           c                   const(x'BA')
007900101223     d c_F23           c                   const(x'BB')
008000101223     d c_F24           c                   const(x'BC')
008100101223     d c_Enter         c                   const(x'F1')
008200101223     d c_RollDown      c                   const(x'F4')
008300101223     d c_RollUp        c                   const(x'F5')
008400101223
008500101223       // -?Attributi di Visualizzazione?
008600101223     d c_DspAtrNorm    c                   const(x'A0')
008700101223     d c_DspAtrRed     c                   const(x'A8')
008800110215     d c_DspAtr_HI     c                   const(x'A2')
008900110215     d c_DspAtr_RI     c                   const(x'A1')
009000110215     d c_DspAtrRed_RI  c                   const(x'A9')
009100101223
009200101223       //--------------------------------------------------------------
009300101223       //?Definizione schiere.                                         ?
009400101223       //--------------------------------------------------------------
009500110330
009600110330       // -?Codici LOG in FIILV00F?
009700110330     d $CoL            s                   like(ILVcol)
009800130110     d                                     dim(10)  ctdata  perrcd(1)
009900101223
010000101223       // -?Messaggi di errore?
010100130110     d $Msg            s             78    dim(10)  ctdata  perrcd(1)
010200101223
010300101223       //--------------------------------------------------------------
010400101223       //?Definizione aree dati.                                       ?
010500101223       //--------------------------------------------------------------
010600101223
010700101223       // -?Dati utente?
010800101223     d §AzUte        e ds                  extname(AZUTE00F)
010900101223     d                                     dtaara
011000101223     d §DatiUte      e ds                  extname(dDatiUte)
011100101223     d                                     dtaara
011200101223
011300101223       //--------------------------------------------------------------
011400101223       //?Definizione strutture dati.                                  ?
011500101223       //--------------------------------------------------------------
011600101223
011700101223       // -?Status ds?
011800101223     d Status         sds
011900110207     d   SDSpgm          *proc
012000110207     d   SDSusr              254    263
012100101223
012200101223       // -?InfDS?
012300101223     d InfDspF         ds
012400101223     d   dsp_aid             369    369a
012500101223     d   sfl_rrn             376    377i 0
012600101223     d   min_nrr             378    379i 0
012700101223     d   num_rcds            380    381i 0
012800101223
012900101223       // -?Indicatori su DspF?
013000101223     d IndDspF         ds                  inz
013100101223         // -?Abilitazione tasti funzionali?
013200110118     d   F8Attivo                      n   overlay(IndDspF : 08)
013300110209     d   F9Attivo                      n   overlay(IndDspF : 09)
013400110216     d   F19Attivo                     n   overlay(IndDspF : 19)
013500120703     d   F21Attivo                     n   overlay(IndDspF : 21)
013600101223         // -?Emissione messaggio di errore?
013700101223     d   ErrMessage                    n   overlay(IndDspF : 28)
013800101223         // -?Indicatori di gestione del subfile?
013900101223     d   SflDsp_N                      n   overlay(IndDspF : 30)
014000101223     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
014100101223     d   SflNxtChg                     n   overlay(IndDspF : 32)
014200101223     d   SflEnd                        n   overlay(IndDspF : 33)
014300110208         // -?Indicatori per attibuti di visualizzazione?
014400110922     d   NotDisplayed                  n   overlay(IndDspF : 40)
014500110216     d   EvidCol                       n   overlay(IndDspF : 41)
014600110216     d   EvidRisult                    n   overlay(IndDspF : 42)
014700110216     d   F9Filiale                     n   overlay(IndDspF : 49)
014800101223         // -?Posizionamento cursore & Segnalazione errore?
014900101223     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015000110208     d   PosCurFGS                     n   overlay(IndDspF : 51)
015100110208     d   PosCurLNP                     n   overlay(IndDspF : 52)
015200110405     d   PosCurE00                     n   overlay(IndDspF : 53)
015300130227     d   PosCurDRI                     n   overlay(IndDspF : 54)
015400130429     d   PosCurDRF                     n   overlay(IndDspF : 55)
015500130429     d   PosCurCOL1                    n   overlay(IndDspF : 56)
015600130429     d   PosCurCOL2                    n   overlay(IndDspF : 57)
015700130429     d   PosCurCOL3                    n   overlay(IndDspF : 58)
015800130429     d   PosCurCOL4                    n   overlay(IndDspF : 59)
015900130429     d   PosCurCOL5                    n   overlay(IndDspF : 60)
016000130429     d   PosCurCOL6                    n   overlay(IndDspF : 61)
016100130429     d   PosCurCOL7                    n   overlay(IndDspF : 62)
016200130429     d   PosCurCOL8                    n   overlay(IndDspF : 63)
016300130429     d   PosCurCOL9                    n   overlay(IndDspF : 64)
016400110222         // -?Riemissione videata?
016500101223     d   ErrGenerico                   n   overlay(IndDspF : 99)
016600110330
016700110330       // -?Codici Log a video?
016800110330     d V1Ccol          ds                  inz
016900110330     d   V1Ccol1                           inz
017000110330     d   V1Ccol2                           inz
017100110330     d   V1Ccol3                           inz
017200110330     d   V1Ccol4                           inz
017300110330     d   V1Ccol5                           inz
017400110330     d   V1Ccol6                           inz
017500110330     d   V1Ccol7                           inz
017600130429     d   V1Ccol8                           inz
017700130429     d   V1Ccol9                           inz
017800130429     d     $V1Ccol                         inz  like(V1Ccol1)  dim(9)
017900110330     d                                          overlay(V1Ccol : 1)
018000110218
018100110218       // -?Vizualizzazione immagine LdV?
018200110218     d ds_KEYsped      ds            14    inz  qualified
018300110218     d   DKSaas20                     2s 0 inz
018400110218     d   DKSlnp                       3s 0 inz
018500110218     d   DKSnrs                       2s 0 inz
018600110218     d   DKSnsp                       7s 0 inz
018700110218     d     DKSaalnp            1      5
018800101223
018900101223       // -?Parametri ricevuti?
019000101223     d KPJBA         e ds
019100120703
019200120703       // -?Altri dati in FIILV00F.ILVLIBERO?
019300120703     d dILVlibero    e ds                  inz
019400130412
019500130412       // -?Tab. "ILV" = RootFolders per immagini LdV?
019600130412     d dILV          e ds                  inz  qualified
019700101223
019800101223       // -?Dati estratti via SQL?
019900110208     d wILVrrn         s             11  0 inz
020000110216     d FIILVds       e ds                  inz  extname(FIILV00F)
020100110329     d                                          prefix( SQL : 3 )
020200101223
020300101223       //--------------------------------------------------------------
020400101223       //?Definizione variabili globali.                               ?
020500101223       //--------------------------------------------------------------
020600101223
020700101223       // -?Flags booleani?
020800101223     d $Fine           s               n   inz
020900101223     d $EoF            s               n   inz
021000101223     d $Err            s               n   inz
021100110218     d $ErrCmd         s               n   inz
021200101223     d $InzD01         s               n   inz(*on)
021300101223     d $InzS01         s               n   inz(*on)
021400120703     d $InzD02         s               n   inz(*on)
021500120703     d $InzW02         s               n   inz(*on)
021600110124
021700110124       // -?Indici di schiera?
021800110124     d xx              s              3  0 inz
021900101223
022000101223       // -?Variabili per la gestione del video?
022100101223     d $Video          s              2    inz('D1')
022200101223     d W_SflPag        s              3  0 inz
022300101223     d wPag            s              4  0 inz
022400101223     d wRig            s              3  0 inz
022500101223     d S01nrr          s                   like(C1RcdNbr) inz
022600101223     d SavS01csr       s                   like(C1RcdNbr) inz
022700110216     d wOrdSfl         s              1  0 inz
022800101223
022900101223       // -?Stringa SQL da eseguire?
023000101223     d wSQL            s           2048    Varying        inz
023100130412
023200130412       // -?Nome del sistema IBM?
023300130412     d currSysNeta     s              8a   inz
023400101223
023500101223       // -?Campi di comodo?
023600101223     d wDate_Iso       s               d   datfmt(*iso)   inz
023700130227     d W1Cdri          s                   like(V1Cdri)   inz
023800130227     d W1Cdrf          s                   like(V1Cdrf)   inz
023900101223     d wCountSel       s              3  0 inz
024000110330     d wCountCol       s              3  0 inz
024100101223
024200101223       //--------------------------------------------------------------
024300101223       //?Definizione prototipi procedure.                             ?
024400101223       //--------------------------------------------------------------
024500130412
024600130412       // -?Reperimento NETA sistema AS/400 corrente?
024700130412      /copy gaitrasrc/srcProtoPR,UBRTVNETA
024800101223
024900101223       // -?Reperimento dati utente?
025000101223     d TIBS34ds      e ds                  inz
025100101223      /copy gaitrasrc/srcProtoPR,TIBS34R
025200110124
025300110124       // -?Caricamento filiali gestite dall'utente?
025400110124     d TRUL31ds      e ds                  inz
025500110124     d   $POg                 10    759    inz   dim(250)
025600110124      /copy gaitrasrc/srcProtoPR,TRUL31R
025700110209
025800110209       // -?Reperimento dati tabelle?
025900110209      /copy gaitrasrc/srcProtoPI,TRULTAB
026000110209      /copy gaitrasrc/srcProtoPR,TRULTAB
026100101223
026200101223       // -?Controllo ed inversione date?
026300101223     d WLBdat          ds                  inz
026400101223     d  G08dat                        8  0 inz
026500101223     d  G08inv                        8  0 inz
026600101223     d  G08err                        1    inz('3')
026700101223     d  G08tgi                        5  0 inz
026800101223      /copy gaitrasrc/srcProtoPr,XSRDA8
026900110311
027000110311       // -?Interrogazione LOG BartAUT?
027100110311     d FNLVP2ds      e ds                  inz  qualified
027200110311      /copy gaitrasrc/srcProtoPR,FNLVP2R
027300110216
027400110216       // -?Interrogazione Bolle Arrivi?
027500110216      /copy gaitrasrc/srcProtoPI,FNLR36R
027600110216      /copy gaitrasrc/srcProtoPR,FNLR36R
027700110216
027800110216       // -?Interrogazione Bolle Sede?
027900110216     d TNSB51ds      e ds                  inz  qualified
028000110216      /copy gaitrasrc/srcProtoPR,TNSB51R
028100101223
028200101223       // -?Parametri gestione errori API.?
028300101223      /copy qsysinc/qRpgleSrc,QUSEC
028400101223       // -?Parametri API QCAPCMD (Process Commands)?
028500101223     d Qcmd            s           2048    inz  varying
028600101223      /copy qSysInc/qRpgleSrc,QCAPCMD
028700101223       // -?API QCAPCMD (Process Commands)?
028800101223      /copy gaitrasrc/srcProtoPR,QCAPCMD
028900101223
029000101223       //--------------------------------------------------------------
029100101223       //?Definizione key-list.                                        ?
029200101223       //--------------------------------------------------------------
029300110216
029400110216       // -?File FNARB01L?
029500110216     d k04fnarb01    e ds                  extname(FNARB01L:*key)
029600110216     d                                     prefix(k_)
029700110216     d                                     inz
029800101223
029900110216       // -?File TITAS30C?
030000110216     d k05titas30    e ds                  extname(TITAS30C:*key)
030100110216     d                                     prefix(k_)
030200110216     d                                     inz
030300101223
030400101223       //--------------------------------------------------------------
030500101223       //?M A I N - L I N E                                            ?
030600101223       //--------------------------------------------------------------
030700101223
030800101223     c     *Entry        plist
030900101223     c                   parm                    KPJBA
031000101223
031100101223      /free
031200101223
031300101223       // -?Operazioni iniziali?
031400101223       exsr sr_RoutInz;
031500101223
031600101223       // -?Gestione videate?
031700101223       DoW  $Fine = *off;
031800101223
031900101223         select;
032000101223           // -?Gestione videata D1?
032100101223           when $Video = 'D1';
032200101223             exsr sr_GesD01;
032300101223           // -?Gestione subfile S1?
032400101223           when $Video = 'S1';
032500101223             exsr sr_GesS01;
032600110103           // -?Gestione videata D2?
032700110216           //  ?(D2 gestita all'interno del ciclo opzioni in S1)?
032800110216           //when $Video = 'D2';
032900110216           //  exsr sr_GesD02;
033000101223           // -?? ? ? ? ??
033100101223           other;
033200101223             $Fine = *on;
033300101223         endsl;
033400101223       enddo;
033500101223
033600101223       // -?Operazioni finali?
033700101223       exsr sr_RoutEnd;
033800101223
033900101223       //--------------------------------------------------------------
034000101223       //?Operazioni iniziali.                                         ?
034100101223       //--------------------------------------------------------------
034200101223       BEGSR  sr_RoutInz;
034300101223
034400101223         *inLR = *on;
034500101223
034600101223         // -?Impostazione opzioni per SQL?
034700101223         exec sql   set  option  DynUsrPrf = *Owner,
034800101223                                 CloSqlCsr = *EndMod;
034900101223
035000101223         // -?Reperimento dati job?
035100101223         exsr  sr_DatiJob;
035200101223
035300101223         // -?Impostazione nome programma a video?
035400101223         V1Tpgm = SDSpgm;
035500110124
035600110124         // -?Caricamento filiali gestibili dall'utente?
035700110124         clear  TRUL31ds;
035800110124         I31abi = UTEaut;
035900110124         I31cdi = DUTdis;
036000110124         I31car = DUTare;
036100110124         I31cpo = DUTpou;
036200110124         trul31r ( KPJBA : TRUL31ds );
036300110222         // -?In caso di errore si uscirà DOPO l'emissione della?
036400110222         //  ?videata D01?
036500110124         //if  O31pog <= *zero;
036600110124         //  $Fine = *on;
036700110124         //endif;
036800130412
036900130412         // -?Reperimento nome sistema IBM?
037000130412         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
037100130412           $Fine = *on;
037200130412           leavesr;
037300130412         endif;
037400130412
037500130412         // -?Reperimento percorsi in base al nome del sistema?
037600130412         clear  dILV;
037700130412         if  getTabella ( c_Tntbe : 'ILV' :
037800130412                          %subst( currSysNetA : 1 : 6 ) : 'LINEA' :
037900130412                          *omit : *omit :
038000130412                          *omit : *omit :
038100130412                          *omit : *omit : *omit : *omit :
038200130412                          *omit : *omit : *omit : *omit :
038300130412                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
038400130412                        = *zero       AND
038500130412             ds_TNTBE.TBEatb = *blank;
038600130412           dILV = ds_TNTBE.TBEuni;
038700130412         endif;
038800110218
038900110218         // -?Attivazione visualizzazione immagini LdV?
039000110218         Qcmd = 'STRPCO pcta(*no)';
039100110218         exsr  sr_ExecCmd;
039200101223
039300101223       ENDSR;
039400101223
039500101223       //--------------------------------------------------------------
039600101223       //?Reperimento Dati del job (Utente/Operativi).                 ?
039700101223       //--------------------------------------------------------------
039800101223       BEGSR  sr_DatiJob;
039900101223
040000101223         in(e) §AzUte;
040100101223         if NOT %error;
040200101223           in(e) §DatiUte;
040300101223         endif;
040400101223         if %error or RSut = *blank;
040500101223           tibs34r ( tibs34ds );
040600101223           in §AzUte;
040700101223           in §DatiUte;
040800101223         endif;
040900101223
041000101223       ENDSR;
041100101223
041200101223       //--------------------------------------------------------------
041300110329       //?Gestione videata D01 - Filtro.                               ?
041400101223       //--------------------------------------------------------------
041500101223       BEGSR  sr_GesD01;
041600101223
041700101223         // -?Inizializzazione videata?
041800101223         if  $InzD01 = *on;
041900101223           exsr  sr_InzD01;
042000101223           $InzD01 = *off;
042100101223         endif;
042200101223
042300110208         // -?Emissione Testata?
042400101223         write  LR65T01;
042500101223
042600110208         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
042700101223         exfmt  LR65D01;
042800101223
042900101223         clear  V1Dmsg;
043000101223         reset  ErrMessage;
043100101223         reset  ErrGenerico;
043200101223
043300101223         SELECT;
043400110124
043500110124           // -?Nessuna filiale gestibile dall'utente?
043600110124           WHEN  O31pog <= *zero;
043700110124             $Fine = *on;
043800101223
043900101223           // -?F3=Fine?
044000101223           WHEN  dsp_aid = c_F03;
044100101223             $Fine = *on;
044200101223
044300101223           // -?Invio?
044400101223           OTHER;
044500101223             exsr sr_CtrD01;
044600101223             if  ErrGenerico = *off;
044700101223               $Video  = 'S1';
044800101223               $Inzs01 = *on;
044900101223             endif;
045000101223
045100101223         ENDSL;
045200101223
045300101223       ENDSR;
045400101223
045500101223       //--------------------------------------------------------------
045600110103       //?Inizializzazione videata D01 - Filtro di lancio.             ?
045700101223       //--------------------------------------------------------------
045800101223       BEGSR  sr_InzD01;
045900101223
046000101223         clear  LR65D01;
046100110127
046200110217         if  %subst( KNMUS : 1 : 3 ) <> 'EDP'  and
046300110217             %subst( KNSIF : 1 : 3 ) =  'FIL';
046400110217           V1Cfgs = DUTpou;
046500110217         endif;
046600110222
046700110222         exsr  sr_CtrD01;
046800101223
046900101223       ENDSR;
047000101223
047100101223       //--------------------------------------------------------------
047200101223       //?Controllo dati immessi nella videata D01.                    ?
047300101223       //--------------------------------------------------------------
047400101223       BEGSR  sr_CtrD01;
047500101223
047600101223         %subst(IndDspF : 50) = *off;
047700110124
047800110124         // -?Nessuna filiale gestibile dall'utente?
047900110124         if  O31pog <= *zero;
048000110124           ErrMessage  = *on;
048100110124           ErrGenerico = *on;
048200110222           V1Dmsg = $Msg(01);
048300110124           leavesr;
048400110124         endif;
048500110228
048600110228         // -?Selezioni parziali errate?
048700110228         select;
048800110228           when  V1Cfgs = *zero  and  V1Cndc <> *zero;
048900110228             PosCurFGS   = *on;
049000110228             ErrMessage  = *on;
049100110228             ErrGenerico = *on;
049200110228             V1Dmsg = $Msg(02);
049300110228             leavesr;
049400110228           when  (V1Clnp <> *zero  or
049500110228                  V1Cnrs <> *zero  or
049600110228                  V1Cnsp <> *zero)   and
049700110228                 (V1Clnp = *zero   or
049800110228                  V1Cnsp = *zero   or
049900110228                  V1Caas = *zero);
050000110228             PosCurLNP   = *on;
050100110228             ErrMessage  = *on;
050200110228             ErrGenerico = *on;
050300110228             V1Dmsg = $Msg(03);
050400110228             leavesr;
050500110228         endsl;
050600110228
050700110228         // -?Sistemazione anno della spedizione?
050800110228         if  (V1Clnp <> *zero  or
050900110228              V1Cnrs <> *zero  or
051000110228              V1Cnsp <> *zero);
051100110228           select;
051200110228             //when  V1Caas = *zero;
051300110228             //  V1Caas = *year;
051400110228             when  V1Caas < 100;
051500110228               if  V1Caas > 60;
051600110228                 V1Caas += 1900;
051700110228               else;
051800110228                 V1Caas += 2000;
051900110228               endif;
052000110228           endsl;
052100131212         //else;
052200131212         //  clear  V1Caas;
052300110228         endif;
052400110330
052500110330         // -?Codici Log?
052600110330         if  V1Ccol <> *blank;
052700110330           For  xx = 1  To  %elem($V1Ccol);
052800110330             if  $V1Ccol(xx) <> *blank  and
052900110330                 %lookup($V1Ccol(xx) : $CoL) = *zero;
053000130429               %subst(IndDspF : 55+xx) = *on;
053100110330               ErrMessage  = *on;
053200110330               ErrGenerico = *on;
053300110330               V1Dmsg = $Msg(04);
053400110330               leavesr;
053500110330             endif;
053600110330           EndFor;
053700110330         endif;
053800110330
053900110330         // -?Esclusione record da elaborare?
054000110405         select;
054100110405           when  V1Ce00 <> 'S'  and  V1Ce00 <> *blank;
054200110405             PosCurE00   = *on;
054300110405             ErrMessage  = *on;
054400110405             ErrGenerico = *on;
054500110405             V1Dmsg = $Msg(05);
054600110405             leavesr;
054700110405           when  V1Ce00 = 'S'  and  %lookup('00' : $V1Ccol) > *zero;
054800110405             PosCurE00   = *on;
054900110405             ErrMessage  = *on;
055000110405             ErrGenerico = *on;
055100110405             V1Dmsg = $Msg(06);
055200110405             leavesr;
055300110405         endsl;
055400110127
055500130227         // -?Data elaborazione firma su AS/400 - dal?
055600110127         clear  WLBdat;
055700130227         if  V1Cdri <> *zero;
055800130227           G08dat = V1Cdri;
055900110127           xsrda8 ( WLBdat );
056000110127           if  G08err =  *on;
056100130227             PosCurDRI   = *on;
056200110127             ErrMessage  = *on;
056300110127             ErrGenerico = *on;
056400110405             V1Dmsg = $Msg(07);
056500110127             leavesr;
056600110127           endif;
056700130227           V1Cdri = G08dat;
056800110127         endif;
056900130227         W1Cdri = G08inv;
057000130227
057100130227         // -?Data elaborazione firma su AS/400 - al?
057200130227         clear  WLBdat;
057300130227         if  V1Cdrf <> *zero;
057400130227           G08dat = V1Cdrf;
057500130227           xsrda8 ( WLBdat );
057600130227           if  G08err =  *on;
057700130227             PosCurDRF   = *on;
057800130227             ErrMessage  = *on;
057900130227             ErrGenerico = *on;
058000130227             V1Dmsg = $Msg(07);
058100130227             leavesr;
058200130227           endif;
058300130227           V1Cdrf = G08dat;
058400130227         endif;
058500130227         W1Cdrf = G08inv;
058600130227
058700130227         // -?Corretta sequenza delle date?
058800130227         if  V1Cdri > *zero  and  V1Cdrf > *zero  and  W1Cdri > W1Cdrf;
058900130227           PosCurDRI   = *on;
059000130227           ErrMessage  = *on;
059100130227           ErrGenerico = *on;
059200130227           V1Dmsg = $Msg(08);
059300130227           leavesr;
059400130227         endif;
059500101223
059600101223       ENDSR;
059700101223
059800101223       //--------------------------------------------------------------
059900110216       //?Gestione subfile S01 - Subfile con elenco record FIILV00F.   ?
060000101223       //--------------------------------------------------------------
060100101223       BEGSR  sr_GesS01;
060200101223
060300101223         // -?Inizializzazione subfile?
060400110121         if  $InzS01  and  Not ErrGenerico;
060500101223           exsr  sr_InzS01;
060600101223           $InzS01 = *off;
060700101223         endif;
060800110124
060900110124         // -?Abilitazione CONDIZIONATE tasti funzionali in P01 (per C01)?
061000110216         F8Attivo  = (S01nrr > *zero);
061100110216         F9Attivo  = F8Attivo;
061200101223
061300101223         // -?Emissione Testata e Piede con tasti funzionali abilitati?
061400101223         write  LR65T01;
061500101223         write  LR65P01;
061600101223
061700101223         // -?Posizionamento cursore?
061800101223         if  C1CsrRrn > *zero;
061900101223           C1RcdNbr = C1CsrRrn;
062000101223         else;
062100101223           C1RcdNbr = 1;
062200101223           write  LR65S00;             //(no rec.)?
062300101223         endif;
062400101223
062500101223         // -?Emissione videata?
062600101223         exfmt  LR65C01;
062700101223
062800101223         reset  ErrMessage;
062900101223         reset  ErrGenerico;
063000101223         clear  V1Dmsg;
063100101223
063200101223         SELECT;
063300101223
063400101223           // -?F3=Fine?
063500101223           WHEN  dsp_aid = c_F03;
063600101223             exsr  sr_CloseCursor;
063700101223             $Fine = *on;
063800110208
063900110208           // -?F5=Aggiornamento?
064000110208           WHEN  dsp_aid = c_F05;
064100110208             exsr  sr_CloseCursor;
064200110208             reset  $InzS01;
064300110117
064400110117           // -?F8=Ordinamento Sfl?
064500110117           WHEN  dsp_aid = c_F08;
064600110117             exsr  sr_F08S01;
064700101223
064800101223           // -?F12=Ritorno?
064900101223           WHEN  dsp_aid = c_F12;
065000101223             exsr  sr_CloseCursor;
065100101223             $Video = 'D1';
065200101223
065300101223           // -?Roll-Up?
065400101223           WHEN  dsp_aid = c_RollUp;
065500110228             if  S01nrr >= c_MaxRec;
065600110228               ErrMessage  = *on;
065700110228               ErrGenerico = *on;
065800130227               V1Dmsg = $Msg(10);
065900110228             else;
066000110228               exsr sr_RollUpS01;
066100110228             endif;
066200101223
066300101223           // -?SubFile vuoto?
066400101223           WHEN  S01nrr = *zero;
066500101223
066600101223           // -?Invio?
066700101223           OTHER;
066800101223             // -?Gestione opzioni?
066900101223             exsr  sr_OpzS01;
067000101223             if  ErrGenerico;
067100101223               leavesr;
067200101223             endif;
067300101223
067400101223         ENDSL;
067500101223
067600101223       ENDSR;
067700101223
067800101223       //--------------------------------------------------------------
067900110103       //?Inizializzazione subfile S01.                                ?
068000101223       //--------------------------------------------------------------
068100101223       BEGSR  sr_InzS01;
068200101223
068300101223         // -?Spegnimento degli indicatori di errore?
068400101223         %subst(IndDspF : 50) = *off;
068500101223
068600101223         // -?Pulizia subfile?
068700101223         SflDsp_N    = *on;
068800101223         SflDspCtl_N = *on;
068900101223         write  LR65C01;
069000101223         SflDspCtl_N = *off;
069100101223         SflEnd      = *off;
069200101223
069300101223         clear  W_SflPag;
069400101223         clear  C1RcdNbr;
069500101223         clear  C1CsrRrn;
069600101223         clear  S01nrr;
069700101223         clear  V1Dmsg;
069800101223         ErrMessage  = *off;
069900101223         ErrGenerico = *off;
070000110105         $Eof = *off;
070100110105         $Err = *off;
070200101223
070300101223         // -?Preparazione stringa SQL?
070400101223         exsr  sr_PrepSQL;
070500101223
070600101223         // -?Caricamento della 1ª pagina di dati del subfile?
070700101223         exsr  sr_OpenCursor;
070800101223         exsr  sr_ReadCursor;
070900101223         exsr  sr_RollUpS01;
071000101223
071100101223         // -?Impaginazione subfile?
071200101223         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
071300101223         if S01nrr > *zero;
071400101223           C1RcdNbr  = 1;
071500101223           C1CsrRrn  = 1;
071600101223         else;
071700101223           clear C1RcdNbr;
071800101223         endif;
071900110117
072000110117         // -?Impostazione F8 nel piede del subfile?
072100110117         select;
072200110216           when  wOrdSfl = 0;
072300110216             V1PF08 = 'F8=Ord. x Distinta  ';
072400110216           when  wOrdSfl = 1;
072500110209             V1PF08 = 'F8=Ord. x Spedizione';
072600110216           when  wOrdSfl = 2;
072700110301             V1PF08 = 'F8=Ord. x Cod.Log   ';
072800110216           when  wOrdSfl = 3;
072900110216             V1PF08 = 'F8=Ord. x Risult.Out';
073000110216           when  wOrdSfl = 4;
073100110216             V1PF08 = 'F8=Ord. x Elab.Firma';
073200110117         endsl;
073300101223
073400101223       ENDSR;
073500101223
073600101223       //--------------------------------------------------------------
073700110103       //?Caricamento singola pagina nel subfile S01.                  ?
073800101223       //--------------------------------------------------------------
073900101223       BEGSR  sr_RollUpS01;
074000101223
074100101223         S01nrr    = (W_SflPag * c_SflPag);
074200101223         W_SflPag += 1;
074300101223         SflNxtChg = *off;
074400101223
074500101223         // -?Ciclo di caricamento dati nel subfile?
074600101223         DoW  S01nrr < (W_SflPag * c_SflPag)  and
074700101223              S01nrr < c_MaxRec               and
074800101223             (Not $EoF  and  Not $Err);
074900101223
075000101223           clear  LR65S01;
075100110329
075200110329           exsr  sr_RieS01;
075300101223
075400101223           // -?Scrittura record nel subfile S01?
075500101223           S01nrr += 1;
075600101223           write  LR65S01;
075700101223
075800101223           // -?Lettura record successivo?
075900101223           exsr  sr_ReadCursor;
076000101223
076100101223         EndDo;
076200101223
076300101223         // -?Visualizzazione del SFL (se ci sono dati)?
076400101223         SflDsp_N = (S01nrr <= *zero);
076500101223
076600101223         // -?Attivazione del SFLEND?
076700110228         //SflEnd = (S01nrr >= c_MaxRec  or  $EoF  or  $Err);
076800110228         SflEnd = ($EoF  or  $Err);
076900110228         if  S01nrr >= c_MaxRec;
077000110228           ErrMessage  = *on;
077100110228           ErrGenerico = *on;
077200130227           V1Dmsg = $Msg(10);
077300110228         endif;
077400101223
077500101223         // -?Posizionamento cursore al 1° rcd della pagina?
077600101223         if  S01nrr > *zero;
077700101223           wPag     = S01nrr / c_SflPag;
077800101223           wRig     = S01nrr - (c_SflPag * wPag);
077900101223           C1RcdNbr = wPag * c_SflPag;
078000101223           if  wRig > *zero;
078100101223             C1RcdNbr += 1;
078200101223           else;
078300101223             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
078400101223           endif;
078500101223         else;
078600101223           clear  C1RcdNbr;
078700101223         endif;
078800101223
078900101223         C1CsrRrn = C1RcdNbr;
079000101223
079100101223       ENDSR;
079200110329
079300110329       //--------------------------------------------------------------
079400110329       //?Valorizzazione campi del singolo record nel subfile S01.     ?
079500110329       //--------------------------------------------------------------
079600110329       BEGSR  sr_RieS01;
079700110329
079800110329           // -?Campi Hidden?
079900110329           S1Hrrn     = wILVrrn;
080000110329           S1HprmSpe  = SQLprmSpe;
080100110329           S1HprmDst  = SQLprmDst;
080200110329           S1HprmTimP = SQLprmTimP;
080300110329           S1HtimD    = SQLtimD;
080400110329           S1HtimA    = SQLtimA;
080500110329
080600110329           // -?Campi di Output?
080700110329           S1Cdist    = %editc( SQLfgs : 'X' ) + ' ' +
080800110329                        %editc( SQLndc : '3' );
080900110329           S1Csped    = %editc( SQLlnp : 'X' ) + ' ' +
081000110329                        %editc( SQLnrs : 'Z' ) + ' ' +
081100110329                        %editc( SQLnsp : '3' );
081200110329           S1Caas     = SQLaas;
081300110329           S1Ccol     = SQLcol;
081400110329           S1Cnot     = SQLnot;
081500110329           S1Crisult  = SQLrisult;
081600110329           if  SQLtimA <> *zero;
081700110329             reset  WLBdat;
081800110329             G08inv = %int( %subst( %editc( SQLtimA : 'X' ) : 1 : 8 ) );
081900110329             xsrda8 ( WLBdat );
082000110329             if  G08err = *off;
082100110329               S1CtimAd = G08dat;
082200110329             endif;
082300110329             S1CtimAh  = %int( %subst( %editc( SQLtimA : 'X' ) : 9 ) );
082400110329           endif;
082500110329           S1CflgSpc  = SQLflgSpc;
082600110329           S1CnoCDst  = SQLnoCDst;
082700110329           S1CfirPda  = SQLfirPda;
082800110329           S1Cscan    = SQLscan;
082900110329
083000110329           // -?Impostazione attributi di visualizzazione?
083100110329           select;
083200110329
083300110329             // -?Ordinamento per Data-OraElabFirma/Distinta/Spedizione?
083400110329             when  wOrdSfl = 0;
083500110329               DA00 = c_DspAtrRed;
083600110329               DA01 = c_DspAtrNorm;
083700110329               DA02 = c_DspAtrNorm;
083800110329               if  S1Ccol >= '10';
083900110329                 DA03 = c_DspAtr_HI;
084000110329               else;
084100110329                 DA03 = c_DspAtrNorm;
084200110329               endif;
084300110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
084400110329                 DA04 = c_DspAtr_RI;
084500110329               else;
084600110329                 DA04 = c_DspAtrNorm;
084700110329               endif;
084800110329
084900110329             // -?Ordinamento per Distinta/Spedizione?
085000110329             when  wOrdSfl = 1;
085100110329               DA00 = c_DspAtrNorm;
085200110329               DA01 = c_DspAtrRed;
085300110329               DA02 = c_DspAtrNorm;
085400110329               if  S1Ccol >= '10';
085500110329                 DA03 = c_DspAtr_HI;
085600110329               else;
085700110329                 DA03 = c_DspAtrNorm;
085800110329               endif;
085900110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
086000110329                 DA04 = c_DspAtr_RI;
086100110329               else;
086200110329                 DA04 = c_DspAtrNorm;
086300110329               endif;
086400110329
086500110329             // -?Ordinamento per Spedizione/Distinta?
086600110329             when  wOrdSfl = 2;
086700110329               DA00 = c_DspAtrNorm;
086800110329               DA01 = c_DspAtrNorm;
086900110329               DA02 = c_DspAtrRed;
087000110329               if  S1Ccol >= '10';
087100110329                 DA03 = c_DspAtr_HI;
087200110329               else;
087300110329                 DA03 = c_DspAtrNorm;
087400110329               endif;
087500110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
087600110329                 DA04 = c_DspAtr_RI;
087700110329               else;
087800110329                 DA04 = c_DspAtrNorm;
087900110329               endif;
088000110329
088100110329             // -?Ordinamento per Cod.Log/Spedizione/Distinta/Risultato?
088200110329             when  wOrdSfl = 3;
088300110329               DA00 = c_DspAtrNorm;
088400110329               DA01 = c_DspAtrNorm;
088500110329               DA02 = c_DspAtrNorm;
088600110329               if  S1Ccol >= '10';
088700110329                 DA03 = c_DspAtrRed_RI;
088800110329               else;
088900110329                 DA03 = c_DspAtrRed;
089000110329               endif;
089100110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
089200110329                 DA04 = c_DspAtr_RI;
089300110329               else;
089400110329                 DA04 = c_DspAtrNorm;
089500110329               endif;
089600110329
089700110329             // -?Ordinamento per Risultato/Spedizione/Distinta/Cod.Log?
089800110329             when  wOrdSfl = 4;
089900110329               DA00 = c_DspAtrNorm;
090000110329               DA01 = c_DspAtrNorm;
090100110329               DA02 = c_DspAtrNorm;
090200110329               if  S1Ccol >= '10';
090300110329                 DA03 = c_DspAtr_HI;
090400110329               else;
090500110329                 DA03 = c_DspAtrNorm;
090600110329               endif;
090700110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
090800110329                 DA04 = c_DspAtrRed_RI;
090900110329               else;
091000110329                 DA04 = c_DspAtrRed;
091100110329               endif;
091200110329
091300110329             // -?Nessun ordinamento?
091400110329             other;
091500110329               DA00 = c_DspAtrNorm;
091600110329               DA01 = c_DspAtrNorm;
091700110329               DA02 = c_DspAtrNorm;
091800110329               if  S1Ccol >= '10';
091900110329                 DA03 = c_DspAtr_HI;
092000110329               else;
092100110329                 DA03 = c_DspAtrNorm;
092200110329               endif;
092300110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
092400110329                 DA04 = c_DspAtr_RI;
092500110329               else;
092600110329                 DA04 = c_DspAtrNorm;
092700110329               endif;
092800110329           endsl;
092900110329
093000110329       ENDSR;
093100101223
093200101223       //--------------------------------------------------------------
093300101223       //?Gestione opzioni del subfile S01.                            ?
093400101223       //--------------------------------------------------------------
093500101223       BEGSR  sr_OpzS01;
093600101223
093700101223         clear  SavS01csr;
093800101223
093900101223         // -?Ciclo di lettura subfile?
094000101223         readc  LR65S01;
094100101223
094200101223         DoW  Not %eof(FNLR65D);
094300101223
094400101223           SflNxtChg = *off;
094500101223           %subst(IndDspF : 50) = *off;
094600101223           C1RcdNbr = S01nrr;
094700101223
094800101223           select;
094900101223
095000101223             // -?5=Visualizzazione?
095100101223             when  S1Copz = '5';
095200110216               exsr  sr_OpzS01_05;
095300101223
095400101223             // -?Nessuna opzione?
095500101223             when  S1Copz = *blank;
095600101223
095700101223             // -?Opzione errata?
095800101223             other;
095900101223               ErrMessage  = *on;
096000101223               ErrGenerico = *on;
096100101223               PosCurOPZ   = *on;
096200130227               V1Dmsg = $Msg(09);
096300101223
096400101223           endsl;
096500101223
096600101223           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
096700101223           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
096800101223           if  S1Copz  <> *blank   and
096900101223              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
097000101223             SavS01csr   = C1RcdNbr;
097100101223           endif;
097200101223
097300101223           // -?Aggiornamento sfl?
097400101223           select;
097500101223             when  ErrMessage;
097600101223               SflNxtChg = *on;
097700101223               C1CsrRrn  = C1RcdNbr;
097800101223             when  ErrGenerico;
097900101223               C1CsrRrn  = C1RcdNbr;
098000101223               clear  S1Copz;
098100101223             other;
098200101223               clear  S1Copz;
098300101223           endsl;
098400101223           UPDATE  LR65S01;
098500110922           NotDisplayed  = *off;
098600101223           if  ErrGenerico   or   ErrMessage;
098700101223             leave;
098800101223           endif;
098900101223
099000101223           readc  LR65S01;
099100101223
099200101223         ENDDO;
099300101223
099400101223         // -?Riposiziono il cursore sul record contenente la prima opz.?
099500101223         //  ?(se non sono stati rilevati errori)?
099600101223         if  NOT ErrMessage   and   NOT ErrGenerico   and
099700101223             SavS01csr > *zero;
099800101223           C1CsrRrn = SavS01csr;
099900101223         endif;
100000110216
100100110216         // -?Se richiesto il ri-caricamento del subfile: occorre?
100200110216         //  ?prima chiudere il cursore SQL (attualmente aperto)?
100300110216         if  $InzS01 = *on;
100400110216           exsr  sr_CloseCursor;
100500110216         endif;
100600101223
100700101223       ENDSR;
100800110117
100900110117       //--------------------------------------------------------------
101000110117       //?Gestione tasto funzionale F8 = Ordinamento subfile.          ?
101100110117       //--------------------------------------------------------------
101200110117       BEGSR  sr_F08S01;
101300110117
101400110117         exsr  sr_CloseCursor;
101500110117
101600110117         reset  $InzS01;
101700110117
101800110216         if  wOrdSfl < 4;
101900110117           wOrdSfl += 1;
102000110117         else;
102100110117           clear  wOrdSfl;
102200110117         endif;
102300110117
102400110117       ENDSR;
102500101223
102600101223       //--------------------------------------------------------------
102700110216       //?Visualizzazione singolo LOG del file FIILV00F.               ?
102800101223       //--------------------------------------------------------------
102900110216       BEGSR  sr_OpzS01_05;
103000110103
103100110103         $Video  = 'D2';
103200110103         $InzD02 = *on;
103300110104
103400110104         // -?Per uscire dal ciclo di gestione opzioni e gestire la?
103500110216         //  ?videata D2 dal ciclo principale?
103600110216         //  ?=> Ma sorgono problemi con eventuali altre opzioni già?
103700110216         //     ?inserite?
103800110104         //ErrGenerico = *on;
103900110104
104000110104         // -?Per gestire ogni singola videata D2 da dentro il ciclo?
104100110104         //  ?di gestione opzioni:?
104200110216         DoW  $Video = 'D2';
104300110201           exsr  sr_GesD02;
104400110201         EndDo;
104500110103
104600110103       ENDSR;
104700110103
104800110103       //--------------------------------------------------------------
104900110329       //?Gestione videata D02 - Visualizzazione singolo LOG di FIILV. ?
105000110103       //--------------------------------------------------------------
105100110103       BEGSR  sr_GesD02;
105200110103
105300110103         // -?Inizializzazione videata?
105400110103         if  $InzD02 = *on;
105500110103           exsr  sr_InzD02;
105600110103           $InzD02 = *off;
105700110103         endif;
105800110103
105900110208         // -?Emissione Testata?
106000110103         write  LR65T01;
106100110103
106200110208         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
106300110103         exfmt  LR65D02;
106400110103
106500110103         reset  ErrMessage;
106600110103         reset  ErrGenerico;
106700110103         clear  V1Dmsg;
106800110922         NotDisplayed = *off;
106900110103
107000110103         SELECT;
107100110103
107200110103           // -?F3=Fine?
107300110103           WHEN  dsp_aid = c_F03;
107400110121             ErrGenerico = *on;
107500110216             $Video = 'S1';
107600110311
107700110311           // -?F8=Interrogazione LOG BartAUT?
107800110311           WHEN  dsp_aid = c_F08;
107900110311             clear  FNLVP2ds;
108000110316             if  V2prmDst > *zero;
108100110316               FNLVP2ds.DS2fgs   = %int( %subst( V2prmDst : 1 : 3 ) );
108200110321               FNLVP2ds.DS2ndc   = %subst( V2prmDst : 4 );
108300110321             else;
108400110321               FNLVP2ds.DS2fgs   = V2fgs;
108500110321               FNLVP2ds.DS2ndc   = %editc( V2ndc : 'X' );
108600110321             endif;
108700110321             if  V2prmDst > *zero;
108800110321               FNLVP2ds.DS2idDoc = V2prmSpe;
108900110321             else;
109000110321               FNLVP2ds.DS2idDoc = %editc( V2aas : 'X' ) +
109100110321                                   %editc( V2lnp : 'X' ) +
109200110321                                   %editc( V2nrs : 'X' ) +
109300110321                                   %editc( V2nsp : 'X' );
109400110321             endif;
109500110311             kpjbu = FNLVP2ds;
109600110311             fnlvP2r ( kpjba );
109700110311             FNLVP2ds = kpjbu;
109800110311             if  FNLVP2ds.DS2esito = '1';         //- Uscita con F3?
109900110311               ErrGenerico = *on;
110000110311               $Video = 'S1';
110100110311             endif;
110200110216
110300110216           // -?F9=Bolla Sede/Arrivo?
110400110216           WHEN  dsp_aid = c_F09;
110500110216             If  %subst(knsif : 1 : 3) = 'GAI';   //- Sede?
110600110216               clear  TNSB51ds;
110700110223               TNSB51ds.I51aas = k_TASaas;
110800110223               TNSB51ds.I51lnp = k_TASlnp;
110900110223               TNSB51ds.I51nrs = k_TASnrs;
111000110223               TNSB51ds.I51nsp = k_TASnsp;
111100110223               TNSB51ds.I51tbl = TAStbl;
111200110216               kpjbu = TNSB51ds;
111300110216               tnsb51r ( kpjba );
111400110216               TNSB51ds = kpjbu;
111500110216               if  TNSB51ds.O51f03 = *on;
111600110216                 $Video = 'S1';
111700110216                 ErrGenerico = *on;
111800110216               endif;
111900110216             Else;                                //- Filiale?
112000110216               clear  FNLR36ds;
112100110223               FNLR36ds.LR36aas = k_ARBaas;
112200110223               FNLR36ds.LR36lnp = k_ARBlnp;
112300110223               FNLR36ds.LR36nrs = k_ARBnrs;
112400110223               FNLR36ds.LR36nsp = k_ARBnsp;
112500110216               FNLR36ds.LR36flg = '1';
112600110216               kpjbu = FNLR36ds;
112700110216               fnlr36r ( kpjba );
112800110216               FNLR36ds = kpjbu;
112900110216               if  FNLR36ds.LR36f03 = *on;
113000110216                 $Video = 'S1';
113100110216                 ErrGenerico = *on;
113200110216               endif;
113300110216             EndIf;
113400110103
113500110103           // -?F12=Ritorno?
113600110103           WHEN  dsp_aid = c_F12;
113700110103             $Video = 'S1';
113800110120
113900110120           // -?F16=Cancellazione?
114000110120           WHEN  dsp_aid = c_F16;
114100110308             delete(E)  S1Hrrn  FIILV000;
114200110308             select;
114300110308               when  Not %found(FIILV00F);
114400110308                 $Video = 'S1';
114500110308                 reset  $InzS01;
114600110308               when  %error;
114700110308                 ErrGenerico = *on;
114800110308                 ErrMessage  = *on;
114900110308                 V1Dmsg = 'Cancellazione NON riuscita';
115000110308               other;
115100110308                 //clear  LR65S01;
115200110922                 NotDisplayed = *on;
115300110308                 $Video = 'S1';
115400110308                 reset  $InzS01;
115500110308             endsl;
115600110216
115700110216           // -?F19=Immagine LdV?
115800110216           WHEN  dsp_aid = c_F19;
115900110216             clear  ds_KeySped;
116000110216             ds_KeySped.DKSaas20 =
116100110216                          %int( %subst( %editc( ILVaas : 'X' ) : 3 ) );
116200110216             ds_KeySped.DKSlnp = ILVlnp;
116300110216             ds_KeySped.DKSnrs = ILVnrs;
116400110216             ds_KeySped.DKSnsp = ILVnsp;
116500110218
116600110218             Qcmd = 'STRPCCMD pccmd(''';
116700130412             //**IF  %subst( knsif : 7 : 1 ) = 'P';
116800130412             //**  if  V2col = '01';
116900130412             //**    Qcmd += '\\CedPdaTest.pda\LdvImagesTest\';
117000130412             //**  else;
117100130412             //**    Qcmd += '\\CedPdaTest.pda\LdvImages\';
117200130412             //**  endif;
117300130412             //**ELSE;
117400130412             //**  //if  V2col = '01';
117500130412             //**  //  Qcmd += '\\CedLagBo\Filiali\0pda\';
117600130412             //**  //else;
117700130412             //**    Qcmd += '\\CedPdaBo1.pda\LdvImages\';
117800130412             //**  //endif;
117900130412             //**ENDIF;
118000130412             if  V2col = '01';
118100130412               Qcmd += %trim( dILV.§ILVimgOk );
118200130412             else;
118300130412               Qcmd += %trim( dILV.§ILVimgSca );
118400130412             endif;
118500110218             Qcmd += ds_KeySped.DKSaalnp + '\' + ds_KeySped +
118600110218                     c_Extension + ''') pause(*no)';
118700110218             exsr  sr_ExecCmd;
118800120703
118900120703           // -?F21=Altri dati?
119000120703           WHEN  dsp_aid = c_F21;
119100120703             $Video  = 'W2';
119200120703             $InzW02 = *on;
119300120703             DoW  $Video = 'W2';
119400120703               exsr  sr_GesW02;
119500120703             EndDo;
119600110103
119700110103         ENDSL;
119800101223
119900101223       ENDSR;
120000110103
120100110103       //--------------------------------------------------------------
120200110103       //?Inizializzazione videata D02 - Visualizzazione dettaglio.    ?
120300110103       //--------------------------------------------------------------
120400110103       BEGSR  sr_InzD02;
120500110103
120600110103         clear  LR65D02;
120700110216         clear  ARBdcm;
120800110103
120900110216         // -?Eventuale apertura del file FIILV00F?
121000110207         if  Not %open(FIILV00F);
121100110207           open  FIILV00F;
121200110103         endif;
121300110120
121400110121         // -?Aggancio record x reperimento dati?
121500110207         chain(n)  S1Hrrn  FIILV000;
121600110120
121700110207         if  Not %found(FIILV00F);
121800110103           leavesr;
121900110103         endif;
122000110103
122100110207         V2fgs    = ILVfgs;
122200110207         V2ndc    = ILVndc;
122300110207
122400110207         V2lnp    = ILVlnp;
122500110207         V2nrs    = ILVnrs;
122600110207         V2nsp    = ILVnsp;
122700110207         V2aas    = ILVaas;
122800110207
122900110208         if  ILVtimD > *zero;
123000110208           reset  WLBdat;
123100110208           G08inv = %int( %subst( %editc( ILVtimD : 'X' ) : 1 : 8 ) );
123200110208           xsrda8 ( WLBdat );
123300110208           if  G08err = *off;
123400110208             V2timDd = G08dat;
123500110208           endif;
123600110208           V2timDh  = %int( %subst( %editc( ILVtimD : 'X' ) : 9 ) );
123700110208         endif;
123800110207
123900110222         V2flgspc = ILVflgspc;
124000110207         V2riserv = ILVriserv;
124100110207         V2notaut = ILVnotaut;
124200110325
124300110325         V2nocdst = ILVnocdst;
124400110325         V2firpda = ILVfirpda;
124500110325         V2scan   = ILVscan;
124600110207
124700110207         V2col    = ILVcol;
124800110207         V2not    = ILVnot;
124900110207
125000110222         //if  %len( %trim( ILVprmSPE ) ) = %size( ILVprmSPE );
125100110222         //  V2prmSPE = %subst( ILVprmSPE :  1 : 4 ) + ' ' +
125200110222         //             %subst( ILVprmSPE :  5 : 3 ) + '/' +
125300110222         //             %subst( ILVprmSPE :  8 : 2 ) + '/' +
125400110222         //             %subst( ILVprmSPE : 10 );
125500110222         //else;
125600110207           V2prmSPE = ILVprmSPE;
125700110222         //endif;
125800110207
125900110222         //if  %len( %trim( ILVprmDST ) ) = %size( ILVprmDST );
126000110222         //  V2prmDST = %subst( ILVprmDST : 1 : 3 ) + '/' +
126100110222         //             %subst( ILVprmDST : 4 );
126200110222         //else;
126300110207           V2prmDST = ILVprmDST;
126400110222         //endif;
126500110207
126600110222         //if  %len( %trim( ILVprmTIMp ) ) = %size( ILVprmTIMp );
126700110222         //  V2prmTIMp = %subst( ILVprmTIMp :  1 : 4 ) + '/' +
126800110222         //              %subst( ILVprmTIMp :  5 : 2 ) + '/' +
126900110222         //              %subst( ILVprmTIMp :  7 : 2 ) + '-' +
127000110222         //              %subst( ILVprmTIMp :  9 : 2 ) + ':' +
127100110222         //              %subst( ILVprmTIMp : 11 : 2 ) + ':' +
127200110222         //              %subst( ILVprmTIMp : 13 );
127300110222         //else;
127400110207           V2prmTIMp = ILVprmTIMp;
127500110222         //endif;
127600110208
127700110208         if  ILVtimA > *zero;
127800110208           reset  WLBdat;
127900110208           G08inv = %int( %subst( %editc( ILVtimA : 'X' ) : 1 : 8 ) );
128000110208           xsrda8 ( WLBdat );
128100110208           if  G08err = *off;
128200110208             V2timAd = G08dat;
128300110208           endif;
128400110208           V2timAh  = %int( %subst( %editc( ILVtimA : 'X' ) : 9 ) );
128500110208         endif;
128600110207
128700110207         V2risult = ILVrisult;
128800110104
128900110104         // -?Impostazione attributi di visualizzazione?
129000110216         EvidCol    = ( ILVcol    >= '10' );
129100110216         EvidRisult = ( ILVrisult <> *blank  and  ILVrisult <> 'OK' );
129200110216
129300110216         // -?Verifica se abilitare F9 in Sede o in Filiale?
129400110216         F9Filiale = ( %subst(knsif : 1 : 3) = 'FIL' );
129500110216
129600110216         // -?Verifica se presente spedizione in filiale?
129700110216         IF  F9Filiale;
129800110216           if  Not %open(FNARB01L);
129900110216             open  FNARB01L;
130000110216           endif;
130100110216           k_ARBaas = ILVaas;
130200110216           k_ARBlnp = ILVlnp;
130300110216           k_ARBnrs = ILVnrs;
130400110216           k_ARBnsp = ILVnsp;
130500110222           setll  %kds( k04fnarb01 )  FNARB000;
130600110223         // -?Verifica se presente spedizione in sede?
130700110223         //  ?(chain per reperire TASTBL)?
130800110216         ELSE;
130900110216           if  Not %open(TITAS30C);
131000110216             open  TITAS30C;
131100110216           endif;
131200110216           k_TASaas = ILVaas;
131300110216           k_TASlnp = ILVlnp;
131400110216           k_TASnrs = ILVnrs;
131500110216           k_TASnsp = ILVnsp;
131600110223           chain  %kds( k05titas30 : 4)  TITAS30C;
131700110216         ENDIF;
131800110216
131900110216         // -?Attivazione (condizionata) tasti funzionali?
132000110223         F9Attivo  = (( Not F9Filiale  and  %found(TITAS30C) )  OR
132100110222                      ( F9Filiale      and  %equal(FNARB01L) ));
132200110222         F19Attivo = ( ILVcol > '01'  and  ILVrisult = 'OK' );
132300120703         F21Attivo = ( ILVlibero <> *blank );
132400110103
132500110103       ENDSR;
132600120703
132700120703       //--------------------------------------------------------------
132800120703       //?Gestione window W02 - Visualizzazione dati aggiuntivi.       ?
132900120703       //--------------------------------------------------------------
133000120703       BEGSR  sr_GesW02;
133100120703
133200120703         // -?Inizializzazione videata?
133300120703         if  $InzW02 = *on;
133400120703           exsr  sr_InzW02;
133500120703           $InzW02 = *off;
133600120703         endif;
133700120703
133800120703         // -?Emissione Window?
133900120703         exfmt  LR65W02;
134000120703
134100120703         // -?F12=Ritorno?
134200120703         if  dsp_aid = c_F12;
134300120703           $Video = 'D2';
134400120703         endif;
134500120703
134600120703       ENDSR;
134700120703
134800120703       //--------------------------------------------------------------
134900120703       //?Inizializzazione window W02 - Visualizzaz. dati aggiuntivi.  ?
135000120703       //--------------------------------------------------------------
135100120703       BEGSR  sr_InzW02;
135200120703
135300120703         dILVlibero = ILVlibero;
135400120703
135500120703         clear  LR65W02;
135600120703
135700120703         W2Chmp = %subst( §ILVhmp : 1 : 2 ) + ':' +
135800120703                  %subst( §ILVhmp : 3 : 2 );
135900120703
136000120703       ENDSR;
136100110216
136200110216       //--------------------------------------------------------------
136300110216       //?Preparazione stringa SQL S1.                                 ?
136400110216       //--------------------------------------------------------------
136500110216       BEGSR  sr_PrepSQL;
136600110216
136700110216         clear  wCountSel;
136800110216         wSQL = 'select rrn(FIILV00F), FIILV00F.* from FIILV00F';
136900110216
137000110216         // -?Selezione per filiale distinta (gestibile dall'utente)?
137100110216         if  %subst( Knmus : 1 : 3 ) <> 'EDP';
137200110216           if  wCountSel = *zero;
137300110217             wSQL += ' where';
137400110216           else;
137500110217             wSQL += ' and';
137600110216           endif;
137700110216           wSQL += ' ILVfgs in (';
137800110216           For  xx = 1  To  %elem($POg);
137900110216             if  $POg(xx) <= *zero;
138000110216               leave;
138100110216             endif;
138200110216             if  xx > 1;
138300110216               wSQL += ', ';
138400110216             endif;
138500110216             wSQL += $POg(xx);
138600110216           EndFor;
138700110216           wSQL += ')';
138800110217           wCountSel += 1;
138900110216         endif;
139000110216
139100110217         // -?Selezione per filiale distinta?
139200110217         if  V1Cfgs > *zero;
139300110217           if  wCountSel = *zero;
139400110217             wSQL += ' where';
139500110217           else;
139600110217             wSQL += ' and';
139700110217           endif;
139800110217           wSQL += ' ( substr( ILVprmDST, 1, 3 ) = ''' +
139900110217                          %editc( V1Cfgs : 'X' ) + ''' or +
140000110217                       ILVfgs = ' + %triml( %editc( V1Cfgs : '3' ) ) +
140100110217                   ' )';
140200110217           wCountSel += 1;
140300110217         endif;
140400110217
140500110216         // -?Selezione per numero distinta?
140600110217         if  V1Cndc > *zero;
140700110216           if  wCountSel = *zero;
140800110217             wSQL += ' where';
140900110216           else;
141000110217             wSQL += ' and';
141100110216           endif;
141200110217           wSQL += ' ( substr( ILVprmDST, 4 ) = ''' +
141300110225                       %editc( V1Cndc : 'X' ) + ''' or +
141400110217                       ILVndc = ' + %triml( %editc( V1Cndc : '3' ) ) +
141500110217                   ' )';
141600110216           wCountSel += 1;
141700110216         endif;
141800110216
141900110216         // -?Selezione per numero spedizione?
142000110217         //if  V1Clnp > *zero  and  V1Cnsp > *zero  and  V1Caas > *zero;
142100110217         //  if  wCountSel = *zero;
142200110217         //    wSQL += ' where';
142300110217         //  else;
142400110217         //    wSQL += ' and';
142500110217         //  endif;
142600110217         //  wSQL += ' ( ILVprmSPE = ''' + %editc( V1Caas : 'X' )
142700110217         //                              + %editc( V1Clnp : 'X' )
142800110217         //                              + %editc( V1Cnrs : 'X' )
142900110217         //                              + %editc( V1Cnsp : 'X' ) + ''' +
143000110217         //           or ( ILVaas = ' + %trim( %editc( V1Caas : '3' ) ) +
143100110217         //          ' and ILVlnp = ' + %trim( %editc( V1Clnp : '3' ) ) +
143200110217         //          ' and ILVnrs = ' + %trim( %editc( V1Cnrs : '3' ) ) +
143300110217         //          ' and ILVnsp = ' + %trim( %editc( V1Cnsp : '3' ) ) +
143400110217         //          ' ) )';
143500110217         //  wCountSel += 1;
143600110217         //endif;
143700110217         // -?Selezione per linea di partenza?
143800110217         if  V1Clnp > *zero;
143900110217           if  wCountSel = *zero;
144000110217             wSQL += ' where';
144100110217           else;
144200110217             wSQL += ' and';
144300110217           endif;
144400110217           wSQL += ' ( substr( ILVprmDST, 5, 3 ) = ''' +
144500110217                          %editc( V1Clnp : 'X' ) + ''' or +
144600110217                       ILVlnp = ' + %triml( %editc( V1Clnp : '3' ) ) +
144700110217                   ' )';
144800110217           wCountSel += 1;
144900110217         endif;
145000110217         // -?Selezione per numero serie?
145100110217         if  V1Cnrs > *zero;
145200110217           if  wCountSel = *zero;
145300110217             wSQL += ' where';
145400110217           else;
145500110217             wSQL += ' and';
145600110217           endif;
145700110217           wSQL += ' ( substr( ILVprmDST, 8, 2 ) = ''' +
145800110222                          %editc( V1Cnrs : 'X' ) + ''' or +
145900110217                       ILVnrs = ' + %triml( %editc( V1Cnrs : '3' ) ) +
146000110217                   ' )';
146100110217           wCountSel += 1;
146200110217         endif;
146300110217         // -?Selezione per numero spedizione?
146400110217         if  V1Cnsp > *zero;
146500110217           if  wCountSel = *zero;
146600110217             wSQL += ' where';
146700110217           else;
146800110217             wSQL += ' and';
146900110217           endif;
147000110217           wSQL += ' ( substr( ILVprmDST, 10, 7 ) = ''' +
147100110222                          %editc( V1Cnsp : 'X' ) + ''' or +
147200110217                       ILVnsp = ' + %triml( %editc( V1Cnsp : '3' ) ) +
147300110217                   ' )';
147400110217           wCountSel += 1;
147500110217         endif;
147600110222         // -?Selezione per anno spedizione?
147700110222         if  V1Caas > *zero;
147800110222           if  wCountSel = *zero;
147900110222             wSQL += ' where';
148000110222           else;
148100110222             wSQL += ' and';
148200110222           endif;
148300110222           wSQL += ' ( substr( ILVprmDST, 1, 4 ) = ''' +
148400110222                          %editc( V1Caas : 'X' ) + ''' or +
148500110222                       ILVaas = ' + %triml( %editc( V1Caas : '3' ) ) +
148600110222                   ' )';
148700110222           wCountSel += 1;
148800110222         endif;
148900110330
149000110330         // -?Selezione per codice Log?
149100110330         if  V1Ccol <> *blank;
149200110330           if  wCountSel = *zero;
149300110330             wSQL += ' where';
149400110330           else;
149500110330             wSQL += ' and';
149600110330           endif;
149700110330           wSQL += ' ILVcol in (';
149800110330           clear  wCountCol;
149900110330           For  xx = 1  To  %elem($V1Ccol);
150000110330             if  $V1Ccol(xx) <> *blank;
150100110330               if  wCountCol > *zero;
150200110330                 wSQL += ', ';
150300110330               endif;
150400110330               wSQL += '''' + $V1Ccol(xx) + '''';
150500110330               wCountCol += 1;
150600110330             endif;
150700110330           EndFor;
150800110330           wSQL += ')';
150900110330           wCountSel += 1;
151000110330         endif;
151100110216
151200110222         // -?Esclusione per codice LOG "00" (ancora da elaborare)?
151300110222         if  V1Ce00 <> *blank;
151400110216           if  wCountSel = *zero;
151500110217             wSQL += ' where';
151600110216           else;
151700110217             wSQL += ' and';
151800110216           endif;
151900110222           wSQL += ' ILVcol <> ''00''';
152000110216           wCountSel += 1;
152100110216         endif;
152200110216
152300110216         // -?Selezione per risultato di output?
152400110222         //if  V1Crisult <> *blank;
152500110222         //  if  wCountSel = *zero;
152600110222         //    wSQL += ' where';
152700110222         //  else;
152800110222         //    wSQL += ' and';
152900110222         //  endif;
153000110222         //  wSQL += ' ILVrisult = ''' + V1Crisult + '''';
153100110222         //  wCountSel += 1;
153200110222         //endif;
153300110216
153400130227         // -?Selezione per data elaborazione firma su AS/400 (dal - al)?
153500130227         select;
153600130227           when  V1Cdri > *zero  and  V1Cdrf = *zero;
153700130227             if  wCountSel = *zero;
153800130227               wSQL += ' where';
153900130227             else;
154000130227               wSQL += ' and';
154100130227             endif;
154200130227             wSQL += ' ILVtima >= ' + %triml( %editc( W1Cdri : 'Z' ) )
154300130227                                    + '000000';
154400130227             wCountSel += 1;
154500130227           when  V1Cdri = *zero  and  V1Cdrf > *zero;
154600130227             if  wCountSel = *zero;
154700130227               wSQL += ' where';
154800130227             else;
154900130227               wSQL += ' and';
155000130227             endif;
155100130227             wSQL += ' ILVtima <= ' + %triml( %editc( W1Cdrf : 'Z' ) )
155200130227                                    + '999999';
155300130227             wCountSel += 1;
155400130227           when  V1Cdri > *zero  and  V1Cdrf > *zero;
155500130227             if  wCountSel = *zero;
155600130227               wSQL += ' where';
155700130227             else;
155800130227               wSQL += ' and';
155900130227             endif;
156000130227             wSQL += ' ILVtima between ' + %triml( %editc( W1Cdri : 'Z' ) )
156100130227                                         + '000000 and '
156200130227                                         + %triml( %editc( W1Cdrf : 'Z' ) )
156300130227                                         + '999999';
156400130227             wCountSel += 1;
156500130227         endsl;
156600110216
156700110216         // -?Ordinamento?
156800110216         select;
156900110216           // -?Ordinamento per Data/Ora Elab. Firma/Distinta/Spedizione?
157000110216           when  wOrdSfl = 0;
157100110301             wSQL += ' order by ILVtimA desc, +
157200110216                                ILVfgs, ILVndc, +
157300110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp';
157400110216           // -?Ordinamento per Distinta/Spedizione?
157500110216           when  wOrdSfl = 1;
157600110216             wSQL += ' order by ILVfgs, ILVndc, +
157700110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp';
157800110216           // -?Ordinamento per Spedizione/Distinta?
157900110216           when  wOrdSfl = 2;
158000110216             wSQL += ' order by ILVaas, ILVlnp, ILVnrs, ILVnsp, +
158100110216                                ILVfgs, ILVndc';
158200110216           // -?Ordinamento per Cod.Log/Spedizione/Distinta/Risultato?
158300110216           when  wOrdSfl = 3;
158400110330             wSQL += ' order by ILVcol desc, +
158500110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp, +
158600110216                                ILVfgs, ILVndc, +
158700110216                                ILVrisult';
158800110216           // -?Ordinamento per Risultato/Spedizione/Distinta/Cod.Log?
158900110216           when  wOrdSfl = 4;
159000110216             wSQL += ' order by ILVrisult, +
159100110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp, +
159200110216                                ILVfgs, ILVndc, +
159300110216                                ILVcol';
159400110216         endsl;
159500110216
159600110216         wSQL += ' for fetch only';
159700110216
159800110216       ENDSR;
159900110216
160000110216       //--------------------------------------------------------------
160100110216       //?Apertura cursore C1.                                         ?
160200110216       //--------------------------------------------------------------
160300110216       BEGSR  sr_OpenCursor;
160400110216
160500110216         // -?Dichiarazione cursore?
160600110216         exec sql   prepare S1   from :wSQL;
160700110216         exec sql   declare C1   cursor for S1;
160800110216
160900110216         // -?Apertura del cursore?
161000110216         exec sql   open C1;
161100110216
161200110216         if  SQLcode < *zero;
161300110216           $Err = *on;
161400110216           $EoF = *on;
161500110216           exsr  sr_PrintErrSQL;
161600110216         endif;
161700110216
161800110216       ENDSR;
161900110216
162000110216       //--------------------------------------------------------------
162100110216       //?Chiusura cursore C1.                                         ?
162200110216       //--------------------------------------------------------------
162300110216       BEGSR  sr_CloseCursor;
162400110216
162500110216         // -?Chiusura del cursore?
162600110216         exec sql   close C1;
162700110216
162800110216       ENDSR;
162900110216
163000110216       //--------------------------------------------------------------
163100110216       //?Lettura cursore C1.                                          ?
163200110216       //--------------------------------------------------------------
163300110216       BEGSR  sr_ReadCursor;
163400110216
163500110216         clear  FIILVds;
163600110216
163700110216         exec sql   fetch next   from C1   into :wILVrrn, :FIILVds;
163800110216
163900110216         select;
164000110216           when  SQLcode = 100;
164100110216             $EoF = *on;
164200110216           when  SQLcode < *zero;
164300110216             $Err = *on;
164400110216             $Eof = *on;
164500110216             exsr  sr_PrintErrSQL;
164600110216         endsl;
164700110216
164800110216       ENDSR;
164900110216
165000110216       //--------------------------------------------------------------
165100110216       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
165200110216       //--------------------------------------------------------------
165300110216       BEGSR  sr_PrintErrSQL;
165400110216
165500110216         // -?Stampa del Dump?
165600110216         Dump(A);
165700110216
165800110216         // -?Stampa del Job-Log?
165900110216         Qcmd = 'DSPJOBLOG job(*) output(*print)';
166000110216         exsr  sr_ExecCmd;
166100110216
166200110216         // -?Chiusura del programma?
166300110216         exsr  sr_RoutEnd;
166400110216
166500110216       ENDSR;
166600110216
166700110216       //--------------------------------------------------------------
166800110216       //?Esecuzione del comando (già impostato).                      ?
166900110216       //--------------------------------------------------------------
167000110216       BEGSR  sr_ExecCmd;
167100110216
167200110216         clear Qcap0100;
167300110216         Qcabcsdh = *off;
167400110216         Qcapa    = *off;
167500110216         Qcacmdss = *off;
167600110216         Qcaerved = *allX'00';
167700110216
167800110216         clear Qusec;
167900110216         Qusbprv  = %size(Qusec);
168000110216
168100110216         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
168200110216                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
168300110216                           0 : 0 : Qusec);
168400110216
168500110216         // -?Invio *msg per segnalazione errore?
168600110216         //if  Qusei <> *blank;
168700110216         //  ...;
168800110216         //endif;
168900110216
169000110216       ENDSR;
169100101223
169200101223       //--------------------------------------------------------------
169300101223       //?Operazioni finali.                                           ?
169400101223       //--------------------------------------------------------------
169500101223       BEGSR  sr_RoutEnd;
169600110103
169700110103         // -?Chiusura archivi?
169800110207         if  %open(FIILV00F);
169900110207           close  FIILV00F;
170000110103         endif;
170100110216         if  %open(FNARB01L);
170200110216           close  FNARB01L;
170300110216         endif;
170400110216         if  %open(TITAS30C);
170500110216           close  TITAS30C;
170600110216         endif;
170700101223
170800101223         // -?Uscita dal *pgm?
170900101223         return;
171000101223
171100101223       ENDSR;
171200101223
171300101223      /end-free
171400101223
171500101223       //--------------------------------------------------------------
171600101223       //?Schiere a tempo di compilazione.                             ?
171700101223       //--------------------------------------------------------------
171800101223
171900110330** -?$CoL:?Codici LOG?
17200011033000 Spedizione in distinta chiusa - da elaborare
17210011033001 Spedizione elaborata
17220011033008 Sped. non consegnata in dist. - immagine a SCARTO
17230011033009 LdV da scannerizzare - immagine a SCARTO
17240011033021 Parametri di input errati - immagine NON creata
17250011033022 Distinta non trovata/annullata - immagine a SCARTO
17260011033023 Sped./Dist. non trovata in FIILV - immag. a SCARTO
17270011033025 Sped. non trovata in FNARB - immagine NON creata
17280013011051 BOLLA RECUPERO - Immagine già generata
17290013011052 BOLLA RECUPERO - Spedizione non trovata in FNARB
173000101223** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
173100110222Nessuna filiale gestibile dall'utente.  Premere "Invio" per uscire.            1
173200110228Inserire anche la filiale della distinta                                       2
173300110228Inserire il numero completo della spedizione: LNP, (NRS,) NSP e AAS            3
173400130429Codice Log errato. Previsti: 00, 01, 08, 09, 21, 22, 23, 25, 51 e 52.          4
173500110405Flag "Esclusione record da elaborare" errato: S=Sì, _=No                       5
173600110405Esclusione rec. da elaborare INCOMPATIBILE con selezione codice Log "00"       6
173700110405Data formalmente errata                                                        7
173800130227Data iniziale (dal) successiva a quella finale (al)                            8
173900130227Opzione errata                                                                 9
174000130227SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)        10
