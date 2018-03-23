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
010100170707     d $Msg            s             78    dim(11)  ctdata  perrcd(1)
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
014700170808     d   F19As888                      n   overlay(IndDspF : 48)
014800110216     d   F9Filiale                     n   overlay(IndDspF : 49)
014900101223         // -?Posizionamento cursore & Segnalazione errore?
015000101223     d   PosCurOPZ                     n   overlay(IndDspF : 50)
015100110208     d   PosCurFGS                     n   overlay(IndDspF : 51)
015200110208     d   PosCurLNP                     n   overlay(IndDspF : 52)
015300110405     d   PosCurE00                     n   overlay(IndDspF : 53)
015400130227     d   PosCurDRI                     n   overlay(IndDspF : 54)
015500130429     d   PosCurDRF                     n   overlay(IndDspF : 55)
015600130429     d   PosCurCOL1                    n   overlay(IndDspF : 56)
015700130429     d   PosCurCOL2                    n   overlay(IndDspF : 57)
015800130429     d   PosCurCOL3                    n   overlay(IndDspF : 58)
015900130429     d   PosCurCOL4                    n   overlay(IndDspF : 59)
016000130429     d   PosCurCOL5                    n   overlay(IndDspF : 60)
016100130429     d   PosCurCOL6                    n   overlay(IndDspF : 61)
016200130429     d   PosCurCOL7                    n   overlay(IndDspF : 62)
016300130429     d   PosCurCOL8                    n   overlay(IndDspF : 63)
016400130429     d   PosCurCOL9                    n   overlay(IndDspF : 64)
016500170707     d   PosCurAPL                     n   overlay(IndDspF : 65)
016600110222         // -?Riemissione videata?
016700101223     d   ErrGenerico                   n   overlay(IndDspF : 99)
016800110330
016900110330       // -?Codici Log a video?
017000110330     d V1Ccol          ds                  inz
017100110330     d   V1Ccol1                           inz
017200110330     d   V1Ccol2                           inz
017300110330     d   V1Ccol3                           inz
017400110330     d   V1Ccol4                           inz
017500110330     d   V1Ccol5                           inz
017600110330     d   V1Ccol6                           inz
017700110330     d   V1Ccol7                           inz
017800130429     d   V1Ccol8                           inz
017900130429     d   V1Ccol9                           inz
018000130429     d     $V1Ccol                         inz  like(V1Ccol1)  dim(9)
018100110330     d                                          overlay(V1Ccol : 1)
018200110218
018300110218       // -?Vizualizzazione immagine LdV?
018400110218     d ds_KEYsped      ds            14    inz  qualified
018500110218     d   DKSaas20                     2s 0 inz
018600110218     d   DKSlnp                       3s 0 inz
018700110218     d   DKSnrs                       2s 0 inz
018800110218     d   DKSnsp                       7s 0 inz
018900110218     d     DKSaalnp            1      5
019000101223
019100101223       // -?Parametri ricevuti?
019200101223     d KPJBA         e ds
019300120703
019400120703       // -?Altri dati in FIILV00F.ILVLIBERO?
019500170707     d dILVlibero    e ds                  inz  qualified
019600130412
019700170802       // -?Tab. "ILV" = RootFolders per immagini LdV - DS Group?
019800130412     d dILV          e ds                  inz  qualified
019900170802       // -?Tab. "ILG" = RootFolders per immagini LdV - Gulliver?
020000170802     d dILG          e ds                  inz  qualified
020100170802     d                                          extname(dILV)
020200101223
020300101223       // -?Dati estratti via SQL?
020400110208     d wILVrrn         s             11  0 inz
020500110216     d FIILVds       e ds                  inz  extname(FIILV00F)
020600110329     d                                          prefix( SQL : 3 )
020700101223
020800101223       //--------------------------------------------------------------
020900101223       //?Definizione variabili globali.                               ?
021000101223       //--------------------------------------------------------------
021100101223
021200101223       // -?Flags booleani?
021300101223     d $Fine           s               n   inz
021400101223     d $EoF            s               n   inz
021500101223     d $Err            s               n   inz
021600110218     d $ErrCmd         s               n   inz
021700101223     d $InzD01         s               n   inz(*on)
021800101223     d $InzS01         s               n   inz(*on)
021900120703     d $InzD02         s               n   inz(*on)
022000120703     d $InzW02         s               n   inz(*on)
022100110124
022200110124       // -?Indici di schiera?
022300110124     d xx              s              3  0 inz
022400101223
022500101223       // -?Variabili per la gestione del video?
022600101223     d $Video          s              2    inz('D1')
022700101223     d W_SflPag        s              3  0 inz
022800101223     d wPag            s              4  0 inz
022900101223     d wRig            s              3  0 inz
023000101223     d S01nrr          s                   like(C1RcdNbr) inz
023100101223     d SavS01csr       s                   like(C1RcdNbr) inz
023200110216     d wOrdSfl         s              1  0 inz
023300101223
023400101223       // -?Stringa SQL da eseguire?
023500101223     d wSQL            s           2048    Varying        inz
023600130412
023700130412       // -?Nome del sistema IBM?
023800130412     d currSysNeta     s              8a   inz
023900101223
024000101223       // -?Campi di comodo?
024100101223     d wDate_Iso       s               d   datfmt(*iso)   inz
024200130227     d W1Cdri          s                   like(V1Cdri)   inz
024300130227     d W1Cdrf          s                   like(V1Cdrf)   inz
024400101223     d wCountSel       s              3  0 inz
024500110330     d wCountCol       s              3  0 inz
024600101223
024700101223       //--------------------------------------------------------------
024800101223       //?Definizione prototipi procedure.                             ?
024900101223       //--------------------------------------------------------------
025000130412
025100130412       // -?Reperimento NETA sistema AS/400 corrente?
025200130412      /copy gaitrasrc/srcProtoPR,UBRTVNETA
025300101223
025400101223       // -?Reperimento dati utente?
025500101223     d TIBS34ds      e ds                  inz
025600101223      /copy gaitrasrc/srcProtoPR,TIBS34R
025700110124
025800110124       // -?Caricamento filiali gestite dall'utente?
025900110124     d TRUL31ds      e ds                  inz
026000110124     d   $POg                 10    759    inz   dim(250)
026100110124      /copy gaitrasrc/srcProtoPR,TRUL31R
026200110209
026300110209       // -?Reperimento dati tabelle?
026400110209      /copy gaitrasrc/srcProtoPI,TRULTAB
026500110209      /copy gaitrasrc/srcProtoPR,TRULTAB
026600101223
026700101223       // -?Controllo ed inversione date?
026800101223     d WLBdat          ds                  inz
026900101223     d  G08dat                        8  0 inz
027000101223     d  G08inv                        8  0 inz
027100101223     d  G08err                        1    inz('3')
027200101223     d  G08tgi                        5  0 inz
027300101223      /copy gaitrasrc/srcProtoPr,XSRDA8
027400110311
027500110311       // -?Interrogazione LOG BartAUT?
027600110311     d FNLVP2ds      e ds                  inz  qualified
027700110311      /copy gaitrasrc/srcProtoPR,FNLVP2R
027800110216
027900110216       // -?Interrogazione Bolle Arrivi?
028000110216      /copy gaitrasrc/srcProtoPI,FNLR36R
028100110216      /copy gaitrasrc/srcProtoPR,FNLR36R
028200110216
028300110216       // -?Interrogazione Bolle Sede?
028400110216     d TNSB51ds      e ds                  inz  qualified
028500110216      /copy gaitrasrc/srcProtoPR,TNSB51R
028600101223
028700101223       // -?Parametri gestione errori API.?
028800101223      /copy qsysinc/qRpgleSrc,QUSEC
028900101223       // -?Parametri API QCAPCMD (Process Commands)?
029000101223     d Qcmd            s           2048    inz  varying
029100101223      /copy qSysInc/qRpgleSrc,QCAPCMD
029200101223       // -?API QCAPCMD (Process Commands)?
029300101223      /copy gaitrasrc/srcProtoPR,QCAPCMD
029400101223
029500101223       //--------------------------------------------------------------
029600101223       //?Definizione key-list.                                        ?
029700101223       //--------------------------------------------------------------
029800110216
029900110216       // -?File FNARB01L?
030000110216     d k04fnarb01    e ds                  extname(FNARB01L:*key)
030100110216     d                                     prefix(k_)
030200110216     d                                     inz
030300101223
030400110216       // -?File TITAS30C?
030500110216     d k05titas30    e ds                  extname(TITAS30C:*key)
030600110216     d                                     prefix(k_)
030700110216     d                                     inz
030800101223
030900101223       //--------------------------------------------------------------
031000101223       //?M A I N - L I N E                                            ?
031100101223       //--------------------------------------------------------------
031200101223
031300101223     c     *Entry        plist
031400101223     c                   parm                    KPJBA
031500101223
031600101223      /free
031700101223
031800101223       // -?Operazioni iniziali?
031900101223       exsr sr_RoutInz;
032000101223
032100101223       // -?Gestione videate?
032200101223       DoW  $Fine = *off;
032300101223
032400101223         select;
032500101223           // -?Gestione videata D1?
032600101223           when $Video = 'D1';
032700101223             exsr sr_GesD01;
032800101223           // -?Gestione subfile S1?
032900101223           when $Video = 'S1';
033000101223             exsr sr_GesS01;
033100110103           // -?Gestione videata D2?
033200110216           //  ?(D2 gestita all'interno del ciclo opzioni in S1)?
033300110216           //when $Video = 'D2';
033400110216           //  exsr sr_GesD02;
033500101223           // -?? ? ? ? ??
033600101223           other;
033700101223             $Fine = *on;
033800101223         endsl;
033900101223       enddo;
034000101223
034100101223       // -?Operazioni finali?
034200101223       exsr sr_RoutEnd;
034300101223
034400101223       //--------------------------------------------------------------
034500101223       //?Operazioni iniziali.                                         ?
034600101223       //--------------------------------------------------------------
034700101223       BEGSR  sr_RoutInz;
034800101223
034900101223         *inLR = *on;
035000101223
035100101223         // -?Impostazione opzioni per SQL?
035200101223         exec sql   set  option  DynUsrPrf = *Owner,
035300101223                                 CloSqlCsr = *EndMod;
035400101223
035500101223         // -?Reperimento dati job?
035600101223         exsr  sr_DatiJob;
035700101223
035800101223         // -?Impostazione nome programma a video?
035900101223         V1Tpgm = SDSpgm;
036000110124
036100110124         // -?Caricamento filiali gestibili dall'utente?
036200110124         clear  TRUL31ds;
036300110124         I31abi = UTEaut;
036400110124         I31cdi = DUTdis;
036500110124         I31car = DUTare;
036600110124         I31cpo = DUTpou;
036700110124         trul31r ( KPJBA : TRUL31ds );
036800110222         // -?In caso di errore si uscirà DOPO l'emissione della?
036900110222         //  ?videata D01?
037000110124         //if  O31pog <= *zero;
037100110124         //  $Fine = *on;
037200110124         //endif;
037300130412
037400130412         // -?Reperimento nome sistema IBM?
037500130412         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
037600130412           $Fine = *on;
037700130412           leavesr;
037800130412         endif;
037900130412
038000130412         // -?Reperimento percorsi in base al nome del sistema?
038100130412         clear  dILV;
038200130412         if  getTabella ( c_Tntbe : 'ILV' :
038300130412                          %subst( currSysNetA : 1 : 6 ) : 'LINEA' :
038400130412                          *omit : *omit :
038500130412                          *omit : *omit :
038600130412                          *omit : *omit : *omit : *omit :
038700130412                          *omit : *omit : *omit : *omit :
038800130412                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
038900130412                        = *zero       AND
039000130412             ds_TNTBE.TBEatb = *blank;
039100130412           dILV = ds_TNTBE.TBEuni;
039200130412         endif;
039300170802
039400170802         clear  dILG;
039500170802         if  getTabella ( c_Tntbe : 'ILG' :
039600170802                          %subst( currSysNetA : 1 : 6 ) : 'LINEA' :
039700170802                          *omit : *omit :
039800170802                          *omit : *omit :
039900170802                          *omit : *omit : *omit : *omit :
040000170802                          *omit : *omit : *omit : *omit :
040100170802                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
040200170802                        = *zero       AND
040300170802             ds_TNTBE.TBEatb = *blank;
040400170802           dILG = ds_TNTBE.TBEuni;
040500170802         endif;
040600110218
040700110218         // -?Attivazione visualizzazione immagini LdV?
040800110218         Qcmd = 'STRPCO pcta(*no)';
040900110218         exsr  sr_ExecCmd;
041000101223
041100101223       ENDSR;
041200101223
041300101223       //--------------------------------------------------------------
041400101223       //?Reperimento Dati del job (Utente/Operativi).                 ?
041500101223       //--------------------------------------------------------------
041600101223       BEGSR  sr_DatiJob;
041700101223
041800101223         in(e) §AzUte;
041900101223         if NOT %error;
042000101223           in(e) §DatiUte;
042100101223         endif;
042200101223         if %error or RSut = *blank;
042300101223           tibs34r ( tibs34ds );
042400101223           in §AzUte;
042500101223           in §DatiUte;
042600101223         endif;
042700101223
042800101223       ENDSR;
042900101223
043000101223       //--------------------------------------------------------------
043100110329       //?Gestione videata D01 - Filtro.                               ?
043200101223       //--------------------------------------------------------------
043300101223       BEGSR  sr_GesD01;
043400101223
043500101223         // -?Inizializzazione videata?
043600101223         if  $InzD01 = *on;
043700101223           exsr  sr_InzD01;
043800101223           $InzD01 = *off;
043900101223         endif;
044000101223
044100110208         // -?Emissione Testata?
044200101223         write  LR65T01;
044300101223
044400110208         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
044500101223         exfmt  LR65D01;
044600101223
044700101223         clear  V1Dmsg;
044800101223         reset  ErrMessage;
044900101223         reset  ErrGenerico;
045000101223
045100101223         SELECT;
045200110124
045300110124           // -?Nessuna filiale gestibile dall'utente?
045400110124           WHEN  O31pog <= *zero;
045500110124             $Fine = *on;
045600101223
045700101223           // -?F3=Fine?
045800101223           WHEN  dsp_aid = c_F03;
045900101223             $Fine = *on;
046000101223
046100101223           // -?Invio?
046200101223           OTHER;
046300101223             exsr sr_CtrD01;
046400101223             if  ErrGenerico = *off;
046500101223               $Video  = 'S1';
046600101223               $Inzs01 = *on;
046700101223             endif;
046800101223
046900101223         ENDSL;
047000101223
047100101223       ENDSR;
047200101223
047300101223       //--------------------------------------------------------------
047400110103       //?Inizializzazione videata D01 - Filtro di lancio.             ?
047500101223       //--------------------------------------------------------------
047600101223       BEGSR  sr_InzD01;
047700101223
047800101223         clear  LR65D01;
047900110127
048000110217         if  %subst( KNMUS : 1 : 3 ) <> 'EDP'  and
048100110217             %subst( KNSIF : 1 : 3 ) =  'FIL';
048200110217           V1Cfgs = DUTpou;
048300110217         endif;
048400110222
048500110222         exsr  sr_CtrD01;
048600101223
048700101223       ENDSR;
048800101223
048900101223       //--------------------------------------------------------------
049000101223       //?Controllo dati immessi nella videata D01.                    ?
049100101223       //--------------------------------------------------------------
049200101223       BEGSR  sr_CtrD01;
049300101223
049400101223         %subst(IndDspF : 50) = *off;
049500110124
049600110124         // -?Nessuna filiale gestibile dall'utente?
049700110124         if  O31pog <= *zero;
049800110124           ErrMessage  = *on;
049900110124           ErrGenerico = *on;
050000110222           V1Dmsg = $Msg(01);
050100110124           leavesr;
050200110124         endif;
050300110228
050400110228         // -?Selezioni parziali errate?
050500110228         select;
050600110228           when  V1Cfgs = *zero  and  V1Cndc <> *zero;
050700110228             PosCurFGS   = *on;
050800110228             ErrMessage  = *on;
050900110228             ErrGenerico = *on;
051000110228             V1Dmsg = $Msg(02);
051100110228             leavesr;
051200110228           when  (V1Clnp <> *zero  or
051300110228                  V1Cnrs <> *zero  or
051400110228                  V1Cnsp <> *zero)   and
051500110228                 (V1Clnp = *zero   or
051600110228                  V1Cnsp = *zero   or
051700110228                  V1Caas = *zero);
051800110228             PosCurLNP   = *on;
051900110228             ErrMessage  = *on;
052000110228             ErrGenerico = *on;
052100110228             V1Dmsg = $Msg(03);
052200110228             leavesr;
052300110228         endsl;
052400110228
052500110228         // -?Sistemazione anno della spedizione?
052600110228         if  (V1Clnp <> *zero  or
052700110228              V1Cnrs <> *zero  or
052800110228              V1Cnsp <> *zero);
052900110228           select;
053000110228             //when  V1Caas = *zero;
053100110228             //  V1Caas = *year;
053200110228             when  V1Caas < 100;
053300110228               if  V1Caas > 60;
053400110228                 V1Caas += 1900;
053500110228               else;
053600110228                 V1Caas += 2000;
053700110228               endif;
053800110228           endsl;
053900131212         //else;
054000131212         //  clear  V1Caas;
054100110228         endif;
054200110330
054300110330         // -?Codici Log?
054400110330         if  V1Ccol <> *blank;
054500110330           For  xx = 1  To  %elem($V1Ccol);
054600110330             if  $V1Ccol(xx) <> *blank  and
054700110330                 %lookup($V1Ccol(xx) : $CoL) = *zero;
054800130429               %subst(IndDspF : 55+xx) = *on;
054900110330               ErrMessage  = *on;
055000110330               ErrGenerico = *on;
055100110330               V1Dmsg = $Msg(04);
055200110330               leavesr;
055300110330             endif;
055400110330           EndFor;
055500110330         endif;
055600110330
055700110330         // -?Esclusione record da elaborare?
055800110405         select;
055900110405           when  V1Ce00 <> 'S'  and  V1Ce00 <> *blank;
056000110405             PosCurE00   = *on;
056100110405             ErrMessage  = *on;
056200110405             ErrGenerico = *on;
056300110405             V1Dmsg = $Msg(05);
056400110405             leavesr;
056500110405           when  V1Ce00 = 'S'  and  %lookup('00' : $V1Ccol) > *zero;
056600110405             PosCurE00   = *on;
056700110405             ErrMessage  = *on;
056800110405             ErrGenerico = *on;
056900110405             V1Dmsg = $Msg(06);
057000110405             leavesr;
057100110405         endsl;
057200110127
057300130227         // -?Data elaborazione firma su AS/400 - dal?
057400110127         clear  WLBdat;
057500130227         if  V1Cdri <> *zero;
057600130227           G08dat = V1Cdri;
057700110127           xsrda8 ( WLBdat );
057800110127           if  G08err =  *on;
057900130227             PosCurDRI   = *on;
058000110127             ErrMessage  = *on;
058100110127             ErrGenerico = *on;
058200110405             V1Dmsg = $Msg(07);
058300110127             leavesr;
058400110127           endif;
058500130227           V1Cdri = G08dat;
058600110127         endif;
058700130227         W1Cdri = G08inv;
058800130227
058900130227         // -?Data elaborazione firma su AS/400 - al?
059000130227         clear  WLBdat;
059100130227         if  V1Cdrf <> *zero;
059200130227           G08dat = V1Cdrf;
059300130227           xsrda8 ( WLBdat );
059400130227           if  G08err =  *on;
059500130227             PosCurDRF   = *on;
059600130227             ErrMessage  = *on;
059700130227             ErrGenerico = *on;
059800130227             V1Dmsg = $Msg(07);
059900130227             leavesr;
060000130227           endif;
060100130227           V1Cdrf = G08dat;
060200130227         endif;
060300130227         W1Cdrf = G08inv;
060400130227
060500130227         // -?Corretta sequenza delle date?
060600130227         if  V1Cdri > *zero  and  V1Cdrf > *zero  and  W1Cdri > W1Cdrf;
060700130227           PosCurDRI   = *on;
060800130227           ErrMessage  = *on;
060900130227           ErrGenerico = *on;
061000130227           V1Dmsg = $Msg(08);
061100130227           leavesr;
061200130227         endif;
061300170707
061400170707         // -?Codice Applicazione per immagine?
061500170707         if  V1Capl <> *blank  and
061600170707             V1Capl <> 'D'     and
061700170707             V1Capl <> 'G';
061800170707           PosCurAPL   = *on;
061900170707           ErrMessage  = *on;
062000170707           ErrGenerico = *on;
062100170707           V1Dmsg = $Msg(11);
062200170707           leavesr;
062300170707         endif;
062400101223
062500101223       ENDSR;
062600101223
062700101223       //--------------------------------------------------------------
062800110216       //?Gestione subfile S01 - Subfile con elenco record FIILV00F.   ?
062900101223       //--------------------------------------------------------------
063000101223       BEGSR  sr_GesS01;
063100101223
063200101223         // -?Inizializzazione subfile?
063300110121         if  $InzS01  and  Not ErrGenerico;
063400101223           exsr  sr_InzS01;
063500101223           $InzS01 = *off;
063600101223         endif;
063700110124
063800110124         // -?Abilitazione CONDIZIONATE tasti funzionali in P01 (per C01)?
063900110216         F8Attivo  = (S01nrr > *zero);
064000110216         F9Attivo  = F8Attivo;
064100101223
064200101223         // -?Emissione Testata e Piede con tasti funzionali abilitati?
064300101223         write  LR65T01;
064400101223         write  LR65P01;
064500101223
064600101223         // -?Posizionamento cursore?
064700101223         if  C1CsrRrn > *zero;
064800101223           C1RcdNbr = C1CsrRrn;
064900101223         else;
065000101223           C1RcdNbr = 1;
065100101223           write  LR65S00;             //(no rec.)?
065200101223         endif;
065300101223
065400101223         // -?Emissione videata?
065500101223         exfmt  LR65C01;
065600101223
065700101223         reset  ErrMessage;
065800101223         reset  ErrGenerico;
065900101223         clear  V1Dmsg;
066000101223
066100101223         SELECT;
066200101223
066300101223           // -?F3=Fine?
066400101223           WHEN  dsp_aid = c_F03;
066500101223             exsr  sr_CloseCursor;
066600101223             $Fine = *on;
066700110208
066800110208           // -?F5=Aggiornamento?
066900110208           WHEN  dsp_aid = c_F05;
067000110208             exsr  sr_CloseCursor;
067100110208             reset  $InzS01;
067200110117
067300110117           // -?F8=Ordinamento Sfl?
067400110117           WHEN  dsp_aid = c_F08;
067500110117             exsr  sr_F08S01;
067600101223
067700101223           // -?F12=Ritorno?
067800101223           WHEN  dsp_aid = c_F12;
067900101223             exsr  sr_CloseCursor;
068000101223             $Video = 'D1';
068100101223
068200101223           // -?Roll-Up?
068300101223           WHEN  dsp_aid = c_RollUp;
068400110228             if  S01nrr >= c_MaxRec;
068500110228               ErrMessage  = *on;
068600110228               ErrGenerico = *on;
068700130227               V1Dmsg = $Msg(10);
068800110228             else;
068900110228               exsr sr_RollUpS01;
069000110228             endif;
069100101223
069200101223           // -?SubFile vuoto?
069300101223           WHEN  S01nrr = *zero;
069400101223
069500101223           // -?Invio?
069600101223           OTHER;
069700101223             // -?Gestione opzioni?
069800101223             exsr  sr_OpzS01;
069900101223             if  ErrGenerico;
070000101223               leavesr;
070100101223             endif;
070200101223
070300101223         ENDSL;
070400101223
070500101223       ENDSR;
070600101223
070700101223       //--------------------------------------------------------------
070800110103       //?Inizializzazione subfile S01.                                ?
070900101223       //--------------------------------------------------------------
071000101223       BEGSR  sr_InzS01;
071100101223
071200101223         // -?Spegnimento degli indicatori di errore?
071300101223         %subst(IndDspF : 50) = *off;
071400101223
071500101223         // -?Pulizia subfile?
071600101223         SflDsp_N    = *on;
071700101223         SflDspCtl_N = *on;
071800101223         write  LR65C01;
071900101223         SflDspCtl_N = *off;
072000101223         SflEnd      = *off;
072100101223
072200101223         clear  W_SflPag;
072300101223         clear  C1RcdNbr;
072400101223         clear  C1CsrRrn;
072500101223         clear  S01nrr;
072600101223         clear  V1Dmsg;
072700101223         ErrMessage  = *off;
072800101223         ErrGenerico = *off;
072900110105         $Eof = *off;
073000110105         $Err = *off;
073100101223
073200101223         // -?Preparazione stringa SQL?
073300101223         exsr  sr_PrepSQL;
073400101223
073500101223         // -?Caricamento della 1ª pagina di dati del subfile?
073600101223         exsr  sr_OpenCursor;
073700101223         exsr  sr_ReadCursor;
073800101223         exsr  sr_RollUpS01;
073900101223
074000101223         // -?Impaginazione subfile?
074100101223         //  ?(forzatura impaginazione sul 1° rec. del subfile)?
074200101223         if S01nrr > *zero;
074300101223           C1RcdNbr  = 1;
074400101223           C1CsrRrn  = 1;
074500101223         else;
074600101223           clear C1RcdNbr;
074700101223         endif;
074800110117
074900110117         // -?Impostazione F8 nel piede del subfile?
075000110117         select;
075100110216           when  wOrdSfl = 0;
075200110216             V1PF08 = 'F8=Ord. x Distinta  ';
075300110216           when  wOrdSfl = 1;
075400110209             V1PF08 = 'F8=Ord. x Spedizione';
075500110216           when  wOrdSfl = 2;
075600110301             V1PF08 = 'F8=Ord. x Cod.Log   ';
075700110216           when  wOrdSfl = 3;
075800110216             V1PF08 = 'F8=Ord. x Risult.Out';
075900110216           when  wOrdSfl = 4;
076000110216             V1PF08 = 'F8=Ord. x Elab.Firma';
076100110117         endsl;
076200101223
076300101223       ENDSR;
076400101223
076500101223       //--------------------------------------------------------------
076600110103       //?Caricamento singola pagina nel subfile S01.                  ?
076700101223       //--------------------------------------------------------------
076800101223       BEGSR  sr_RollUpS01;
076900101223
077000101223         S01nrr    = (W_SflPag * c_SflPag);
077100101223         W_SflPag += 1;
077200101223         SflNxtChg = *off;
077300101223
077400101223         // -?Ciclo di caricamento dati nel subfile?
077500101223         DoW  S01nrr < (W_SflPag * c_SflPag)  and
077600101223              S01nrr < c_MaxRec               and
077700101223             (Not $EoF  and  Not $Err);
077800101223
077900101223           clear  LR65S01;
078000110329
078100110329           exsr  sr_RieS01;
078200101223
078300101223           // -?Scrittura record nel subfile S01?
078400101223           S01nrr += 1;
078500101223           write  LR65S01;
078600101223
078700101223           // -?Lettura record successivo?
078800101223           exsr  sr_ReadCursor;
078900101223
079000101223         EndDo;
079100101223
079200101223         // -?Visualizzazione del SFL (se ci sono dati)?
079300101223         SflDsp_N = (S01nrr <= *zero);
079400101223
079500101223         // -?Attivazione del SFLEND?
079600110228         //SflEnd = (S01nrr >= c_MaxRec  or  $EoF  or  $Err);
079700110228         SflEnd = ($EoF  or  $Err);
079800110228         if  S01nrr >= c_MaxRec;
079900110228           ErrMessage  = *on;
080000110228           ErrGenerico = *on;
080100130227           V1Dmsg = $Msg(10);
080200110228         endif;
080300101223
080400101223         // -?Posizionamento cursore al 1° rcd della pagina?
080500101223         if  S01nrr > *zero;
080600101223           wPag     = S01nrr / c_SflPag;
080700101223           wRig     = S01nrr - (c_SflPag * wPag);
080800101223           C1RcdNbr = wPag * c_SflPag;
080900101223           if  wRig > *zero;
081000101223             C1RcdNbr += 1;
081100101223           else;
081200101223             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
081300101223           endif;
081400101223         else;
081500101223           clear  C1RcdNbr;
081600101223         endif;
081700101223
081800101223         C1CsrRrn = C1RcdNbr;
081900101223
082000101223       ENDSR;
082100110329
082200110329       //--------------------------------------------------------------
082300110329       //?Valorizzazione campi del singolo record nel subfile S01.     ?
082400110329       //--------------------------------------------------------------
082500110329       BEGSR  sr_RieS01;
082600110329
082700110329           // -?Campi Hidden?
082800110329           S1Hrrn     = wILVrrn;
082900110329           S1HprmSpe  = SQLprmSpe;
083000110329           S1HprmDst  = SQLprmDst;
083100110329           S1HprmTimP = SQLprmTimP;
083200110329           S1HtimD    = SQLtimD;
083300110329           S1HtimA    = SQLtimA;
083400110329
083500110329           // -?Campi di Output?
083600110329           S1Cdist    = %editc( SQLfgs : 'X' ) + ' ' +
083700110329                        %editc( SQLndc : '3' );
083800110329           S1Csped    = %editc( SQLlnp : 'X' ) + ' ' +
083900110329                        %editc( SQLnrs : 'Z' ) + ' ' +
084000110329                        %editc( SQLnsp : '3' );
084100110329           S1Caas     = SQLaas;
084200110329           S1Ccol     = SQLcol;
084300110329           S1Cnot     = SQLnot;
084400110329           S1Crisult  = SQLrisult;
084500110329           if  SQLtimA <> *zero;
084600110329             reset  WLBdat;
084700110329             G08inv = %int( %subst( %editc( SQLtimA : 'X' ) : 1 : 8 ) );
084800110329             xsrda8 ( WLBdat );
084900110329             if  G08err = *off;
085000110329               S1CtimAd = G08dat;
085100110329             endif;
085200170707             S1CtimAh = %int( %subst( %editc( SQLtimA : 'X' ) : 9 : 4 ) );
085300110329           endif;
085400110329           S1CflgSpc  = SQLflgSpc;
085500110329           S1CnoCDst  = SQLnoCDst;
085600110329           S1CfirPda  = SQLfirPda;
085700110329           S1Cscan    = SQLscan;
085800170707
085900170707           dILVlibero = SQLlibero;
086000170707           S1Capl     = dILVlibero.§ILVapl;
086100110329
086200110329           // -?Impostazione attributi di visualizzazione?
086300110329           select;
086400110329
086500110329             // -?Ordinamento per Data-OraElabFirma/Distinta/Spedizione?
086600110329             when  wOrdSfl = 0;
086700110329               DA00 = c_DspAtrRed;
086800110329               DA01 = c_DspAtrNorm;
086900110329               DA02 = c_DspAtrNorm;
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
088100110329             // -?Ordinamento per Distinta/Spedizione?
088200110329             when  wOrdSfl = 1;
088300110329               DA00 = c_DspAtrNorm;
088400110329               DA01 = c_DspAtrRed;
088500110329               DA02 = c_DspAtrNorm;
088600110329               if  S1Ccol >= '10';
088700110329                 DA03 = c_DspAtr_HI;
088800110329               else;
088900110329                 DA03 = c_DspAtrNorm;
089000110329               endif;
089100110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
089200110329                 DA04 = c_DspAtr_RI;
089300110329               else;
089400110329                 DA04 = c_DspAtrNorm;
089500110329               endif;
089600110329
089700110329             // -?Ordinamento per Spedizione/Distinta?
089800110329             when  wOrdSfl = 2;
089900110329               DA00 = c_DspAtrNorm;
090000110329               DA01 = c_DspAtrNorm;
090100110329               DA02 = c_DspAtrRed;
090200110329               if  S1Ccol >= '10';
090300110329                 DA03 = c_DspAtr_HI;
090400110329               else;
090500110329                 DA03 = c_DspAtrNorm;
090600110329               endif;
090700110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
090800110329                 DA04 = c_DspAtr_RI;
090900110329               else;
091000110329                 DA04 = c_DspAtrNorm;
091100110329               endif;
091200110329
091300110329             // -?Ordinamento per Cod.Log/Spedizione/Distinta/Risultato?
091400110329             when  wOrdSfl = 3;
091500110329               DA00 = c_DspAtrNorm;
091600110329               DA01 = c_DspAtrNorm;
091700110329               DA02 = c_DspAtrNorm;
091800110329               if  S1Ccol >= '10';
091900110329                 DA03 = c_DspAtrRed_RI;
092000110329               else;
092100110329                 DA03 = c_DspAtrRed;
092200110329               endif;
092300110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
092400110329                 DA04 = c_DspAtr_RI;
092500110329               else;
092600110329                 DA04 = c_DspAtrNorm;
092700110329               endif;
092800110329
092900110329             // -?Ordinamento per Risultato/Spedizione/Distinta/Cod.Log?
093000110329             when  wOrdSfl = 4;
093100110329               DA00 = c_DspAtrNorm;
093200110329               DA01 = c_DspAtrNorm;
093300110329               DA02 = c_DspAtrNorm;
093400110329               if  S1Ccol >= '10';
093500110329                 DA03 = c_DspAtr_HI;
093600110329               else;
093700110329                 DA03 = c_DspAtrNorm;
093800110329               endif;
093900110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
094000110329                 DA04 = c_DspAtrRed_RI;
094100110329               else;
094200110329                 DA04 = c_DspAtrRed;
094300110329               endif;
094400110329
094500110329             // -?Nessun ordinamento?
094600110329             other;
094700110329               DA00 = c_DspAtrNorm;
094800110329               DA01 = c_DspAtrNorm;
094900110329               DA02 = c_DspAtrNorm;
095000110329               if  S1Ccol >= '10';
095100110329                 DA03 = c_DspAtr_HI;
095200110329               else;
095300110329                 DA03 = c_DspAtrNorm;
095400110329               endif;
095500110329               if  S1Crisult <> *blank  and  S1Crisult <> 'OK';
095600110329                 DA04 = c_DspAtr_RI;
095700110329               else;
095800110329                 DA04 = c_DspAtrNorm;
095900110329               endif;
096000110329           endsl;
096100110329
096200110329       ENDSR;
096300101223
096400101223       //--------------------------------------------------------------
096500101223       //?Gestione opzioni del subfile S01.                            ?
096600101223       //--------------------------------------------------------------
096700101223       BEGSR  sr_OpzS01;
096800101223
096900101223         clear  SavS01csr;
097000101223
097100101223         // -?Ciclo di lettura subfile?
097200101223         readc  LR65S01;
097300101223
097400101223         DoW  Not %eof(FNLR65D);
097500101223
097600101223           SflNxtChg = *off;
097700101223           %subst(IndDspF : 50) = *off;
097800101223           C1RcdNbr = S01nrr;
097900101223
098000101223           select;
098100101223
098200101223             // -?5=Visualizzazione?
098300101223             when  S1Copz = '5';
098400110216               exsr  sr_OpzS01_05;
098500101223
098600101223             // -?Nessuna opzione?
098700101223             when  S1Copz = *blank;
098800101223
098900101223             // -?Opzione errata?
099000101223             other;
099100101223               ErrMessage  = *on;
099200101223               ErrGenerico = *on;
099300101223               PosCurOPZ   = *on;
099400130227               V1Dmsg = $Msg(09);
099500101223
099600101223           endsl;
099700101223
099800101223           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
099900101223           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
100000101223           if  S1Copz  <> *blank   and
100100101223              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
100200101223             SavS01csr   = C1RcdNbr;
100300101223           endif;
100400101223
100500101223           // -?Aggiornamento sfl?
100600101223           select;
100700101223             when  ErrMessage;
100800101223               SflNxtChg = *on;
100900101223               C1CsrRrn  = C1RcdNbr;
101000101223             when  ErrGenerico;
101100101223               C1CsrRrn  = C1RcdNbr;
101200101223               clear  S1Copz;
101300101223             other;
101400101223               clear  S1Copz;
101500101223           endsl;
101600101223           UPDATE  LR65S01;
101700110922           NotDisplayed  = *off;
101800101223           if  ErrGenerico   or   ErrMessage;
101900101223             leave;
102000101223           endif;
102100101223
102200101223           readc  LR65S01;
102300101223
102400101223         ENDDO;
102500101223
102600101223         // -?Riposiziono il cursore sul record contenente la prima opz.?
102700101223         //  ?(se non sono stati rilevati errori)?
102800101223         if  NOT ErrMessage   and   NOT ErrGenerico   and
102900101223             SavS01csr > *zero;
103000101223           C1CsrRrn = SavS01csr;
103100101223         endif;
103200110216
103300110216         // -?Se richiesto il ri-caricamento del subfile: occorre?
103400110216         //  ?prima chiudere il cursore SQL (attualmente aperto)?
103500110216         if  $InzS01 = *on;
103600110216           exsr  sr_CloseCursor;
103700110216         endif;
103800101223
103900101223       ENDSR;
104000110117
104100110117       //--------------------------------------------------------------
104200110117       //?Gestione tasto funzionale F8 = Ordinamento subfile.          ?
104300110117       //--------------------------------------------------------------
104400110117       BEGSR  sr_F08S01;
104500110117
104600110117         exsr  sr_CloseCursor;
104700110117
104800110117         reset  $InzS01;
104900110117
105000110216         if  wOrdSfl < 4;
105100110117           wOrdSfl += 1;
105200110117         else;
105300110117           clear  wOrdSfl;
105400110117         endif;
105500110117
105600110117       ENDSR;
105700101223
105800101223       //--------------------------------------------------------------
105900110216       //?Visualizzazione singolo LOG del file FIILV00F.               ?
106000101223       //--------------------------------------------------------------
106100110216       BEGSR  sr_OpzS01_05;
106200110103
106300110103         $Video  = 'D2';
106400110103         $InzD02 = *on;
106500110104
106600110104         // -?Per uscire dal ciclo di gestione opzioni e gestire la?
106700110216         //  ?videata D2 dal ciclo principale?
106800110216         //  ?=> Ma sorgono problemi con eventuali altre opzioni già?
106900110216         //     ?inserite?
107000110104         //ErrGenerico = *on;
107100110104
107200110104         // -?Per gestire ogni singola videata D2 da dentro il ciclo?
107300110104         //  ?di gestione opzioni:?
107400110216         DoW  $Video = 'D2';
107500110201           exsr  sr_GesD02;
107600110201         EndDo;
107700110103
107800110103       ENDSR;
107900110103
108000110103       //--------------------------------------------------------------
108100110329       //?Gestione videata D02 - Visualizzazione singolo LOG di FIILV. ?
108200110103       //--------------------------------------------------------------
108300110103       BEGSR  sr_GesD02;
108400110103
108500110103         // -?Inizializzazione videata?
108600110103         if  $InzD02 = *on;
108700110103           exsr  sr_InzD02;
108800110103           $InzD02 = *off;
108900110103         endif;
109000110103
109100110208         // -?Emissione Testata?
109200110103         write  LR65T01;
109300110103
109400110208         // -?Emissione videata (con Piede = tasti funzionali abilitati)?
109500110103         exfmt  LR65D02;
109600110103
109700110103         reset  ErrMessage;
109800110103         reset  ErrGenerico;
109900110103         clear  V1Dmsg;
110000110922         NotDisplayed = *off;
110100110103
110200110103         SELECT;
110300110103
110400110103           // -?F3=Fine?
110500110103           WHEN  dsp_aid = c_F03;
110600110121             ErrGenerico = *on;
110700110216             $Video = 'S1';
110800110311
110900110311           // -?F8=Interrogazione LOG BartAUT?
111000110311           WHEN  dsp_aid = c_F08;
111100110311             clear  FNLVP2ds;
111200110316             if  V2prmDst > *zero;
111300110316               FNLVP2ds.DS2fgs   = %int( %subst( V2prmDst : 1 : 3 ) );
111400110321               FNLVP2ds.DS2ndc   = %subst( V2prmDst : 4 );
111500110321             else;
111600110321               FNLVP2ds.DS2fgs   = V2fgs;
111700110321               FNLVP2ds.DS2ndc   = %editc( V2ndc : 'X' );
111800110321             endif;
111900110321             if  V2prmDst > *zero;
112000110321               FNLVP2ds.DS2idDoc = V2prmSpe;
112100110321             else;
112200110321               FNLVP2ds.DS2idDoc = %editc( V2aas : 'X' ) +
112300110321                                   %editc( V2lnp : 'X' ) +
112400110321                                   %editc( V2nrs : 'X' ) +
112500110321                                   %editc( V2nsp : 'X' );
112600110321             endif;
112700110311             kpjbu = FNLVP2ds;
112800110311             fnlvP2r ( kpjba );
112900110311             FNLVP2ds = kpjbu;
113000110311             if  FNLVP2ds.DS2esito = '1';         //- Uscita con F3?
113100110311               ErrGenerico = *on;
113200110311               $Video = 'S1';
113300110311             endif;
113400110216
113500110216           // -?F9=Bolla Sede/Arrivo?
113600110216           WHEN  dsp_aid = c_F09;
113700110216             If  %subst(knsif : 1 : 3) = 'GAI';   //- Sede?
113800110216               clear  TNSB51ds;
113900110223               TNSB51ds.I51aas = k_TASaas;
114000110223               TNSB51ds.I51lnp = k_TASlnp;
114100110223               TNSB51ds.I51nrs = k_TASnrs;
114200110223               TNSB51ds.I51nsp = k_TASnsp;
114300110223               TNSB51ds.I51tbl = TAStbl;
114400110216               kpjbu = TNSB51ds;
114500110216               tnsb51r ( kpjba );
114600110216               TNSB51ds = kpjbu;
114700110216               if  TNSB51ds.O51f03 = *on;
114800110216                 $Video = 'S1';
114900110216                 ErrGenerico = *on;
115000110216               endif;
115100110216             Else;                                //- Filiale?
115200110216               clear  FNLR36ds;
115300110223               FNLR36ds.LR36aas = k_ARBaas;
115400110223               FNLR36ds.LR36lnp = k_ARBlnp;
115500110223               FNLR36ds.LR36nrs = k_ARBnrs;
115600110223               FNLR36ds.LR36nsp = k_ARBnsp;
115700110216               FNLR36ds.LR36flg = '1';
115800110216               kpjbu = FNLR36ds;
115900110216               fnlr36r ( kpjba );
116000110216               FNLR36ds = kpjbu;
116100110216               if  FNLR36ds.LR36f03 = *on;
116200110216                 $Video = 'S1';
116300110216                 ErrGenerico = *on;
116400110216               endif;
116500110216             EndIf;
116600110103
116700110103           // -?F12=Ritorno?
116800110103           WHEN  dsp_aid = c_F12;
116900110103             $Video = 'S1';
117000110120
117100110120           // -?F16=Cancellazione?
117200110120           WHEN  dsp_aid = c_F16;
117300110308             delete(E)  S1Hrrn  FIILV000;
117400110308             select;
117500110308               when  Not %found(FIILV00F);
117600110308                 $Video = 'S1';
117700110308                 reset  $InzS01;
117800110308               when  %error;
117900110308                 ErrGenerico = *on;
118000110308                 ErrMessage  = *on;
118100110308                 V1Dmsg = 'Cancellazione NON riuscita';
118200110308               other;
118300110308                 //clear  LR65S01;
118400110922                 NotDisplayed = *on;
118500110308                 $Video = 'S1';
118600110308                 reset  $InzS01;
118700110308             endsl;
118800110216
118900110216           // -?F19=Immagine LdV?
119000110216           WHEN  dsp_aid = c_F19;
119100110216             clear  ds_KeySped;
119200110216             ds_KeySped.DKSaas20 =
119300110216                          %int( %subst( %editc( ILVaas : 'X' ) : 3 ) );
119400110216             ds_KeySped.DKSlnp = ILVlnp;
119500110216             ds_KeySped.DKSnrs = ILVnrs;
119600110216             ds_KeySped.DKSnsp = ILVnsp;
119700110218
119800110218             Qcmd = 'STRPCCMD pccmd(''';
119900130412             //**IF  %subst( knsif : 7 : 1 ) = 'P';
120000130412             //**  if  V2col = '01';
120100130412             //**    Qcmd += '\\CedPdaTest.pda\LdvImagesTest\';
120200130412             //**  else;
120300130412             //**    Qcmd += '\\CedPdaTest.pda\LdvImages\';
120400130412             //**  endif;
120500130412             //**ELSE;
120600130412             //**  //if  V2col = '01';
120700130412             //**  //  Qcmd += '\\CedLagBo\Filiali\0pda\';
120800130412             //**  //else;
120900130412             //**    Qcmd += '\\CedPdaBo1.pda\LdvImages\';
121000130412             //**  //endif;
121100130412             //**ENDIF;
121200130412             if  V2col = '01';
121300170802               if  dILVlibero.§ILVapl = 'G';
121400170802                 Qcmd += %trim( dILG.§ILVimgOk );
121500170802               else;
121600170802                 Qcmd += %trim( dILV.§ILVimgOk );
121700170802               endif;
121800130412             else;
121900170802               if  dILVlibero.§ILVapl = 'G';
122000170802                 Qcmd += %trim( dILG.§ILVimgSca );
122100170802               else;
122200170802                 Qcmd += %trim( dILV.§ILVimgSca );
122300170802               endif;
122400130412             endif;
122500110218             Qcmd += ds_KeySped.DKSaalnp + '\' + ds_KeySped +
122600110218                     c_Extension + ''') pause(*no)';
122700110218             exsr  sr_ExecCmd;
122800120703
122900120703           // -?F21=Altri dati?
123000120703           WHEN  dsp_aid = c_F21;
123100120703             $Video  = 'W2';
123200120703             $InzW02 = *on;
123300120703             DoW  $Video = 'W2';
123400120703               exsr  sr_GesW02;
123500120703             EndDo;
123600110103
123700110103         ENDSL;
123800101223
123900101223       ENDSR;
124000110103
124100110103       //--------------------------------------------------------------
124200110103       //?Inizializzazione videata D02 - Visualizzazione dettaglio.    ?
124300110103       //--------------------------------------------------------------
124400110103       BEGSR  sr_InzD02;
124500110103
124600110103         clear  LR65D02;
124700110216         clear  ARBdcm;
124800110103
124900110216         // -?Eventuale apertura del file FIILV00F?
125000110207         if  Not %open(FIILV00F);
125100110207           open  FIILV00F;
125200110103         endif;
125300110120
125400110121         // -?Aggancio record x reperimento dati?
125500110207         chain(n)  S1Hrrn  FIILV000;
125600110120
125700110207         if  Not %found(FIILV00F);
125800110103           leavesr;
125900110103         endif;
126000110103
126100110207         V2fgs    = ILVfgs;
126200110207         V2ndc    = ILVndc;
126300110207
126400110207         V2lnp    = ILVlnp;
126500110207         V2nrs    = ILVnrs;
126600110207         V2nsp    = ILVnsp;
126700110207         V2aas    = ILVaas;
126800110207
126900110208         if  ILVtimD > *zero;
127000110208           reset  WLBdat;
127100110208           G08inv = %int( %subst( %editc( ILVtimD : 'X' ) : 1 : 8 ) );
127200110208           xsrda8 ( WLBdat );
127300110208           if  G08err = *off;
127400110208             V2timDd = G08dat;
127500110208           endif;
127600110208           V2timDh  = %int( %subst( %editc( ILVtimD : 'X' ) : 9 ) );
127700110208         endif;
127800110207
127900110222         V2flgspc = ILVflgspc;
128000110207         V2riserv = ILVriserv;
128100110207         V2notaut = ILVnotaut;
128200110325
128300110325         V2nocdst = ILVnocdst;
128400110325         V2firpda = ILVfirpda;
128500110325         V2scan   = ILVscan;
128600110207
128700110207         V2col    = ILVcol;
128800110207         V2not    = ILVnot;
128900170707
129000170707         dILVlibero = ILVlibero;
129100170707         select;
129200170707           when  dILVlibero.§ILVapl = 'D';
129300170707             V2apl  = 'DS Group';
129400170707           when  dILVlibero.§ILVapl = 'G';
129500170707             V2apl  = 'Gulliver';
129600170707           other;
129700170707             V2apl  = dILVlibero.§ILVapl;
129800170707         endsl;
129900110207
130000110222         //if  %len( %trim( ILVprmSPE ) ) = %size( ILVprmSPE );
130100110222         //  V2prmSPE = %subst( ILVprmSPE :  1 : 4 ) + ' ' +
130200110222         //             %subst( ILVprmSPE :  5 : 3 ) + '/' +
130300110222         //             %subst( ILVprmSPE :  8 : 2 ) + '/' +
130400110222         //             %subst( ILVprmSPE : 10 );
130500110222         //else;
130600110207           V2prmSPE = ILVprmSPE;
130700110222         //endif;
130800110207
130900110222         //if  %len( %trim( ILVprmDST ) ) = %size( ILVprmDST );
131000110222         //  V2prmDST = %subst( ILVprmDST : 1 : 3 ) + '/' +
131100110222         //             %subst( ILVprmDST : 4 );
131200110222         //else;
131300110207           V2prmDST = ILVprmDST;
131400110222         //endif;
131500110207
131600110222         //if  %len( %trim( ILVprmTIMp ) ) = %size( ILVprmTIMp );
131700110222         //  V2prmTIMp = %subst( ILVprmTIMp :  1 : 4 ) + '/' +
131800110222         //              %subst( ILVprmTIMp :  5 : 2 ) + '/' +
131900110222         //              %subst( ILVprmTIMp :  7 : 2 ) + '-' +
132000110222         //              %subst( ILVprmTIMp :  9 : 2 ) + ':' +
132100110222         //              %subst( ILVprmTIMp : 11 : 2 ) + ':' +
132200110222         //              %subst( ILVprmTIMp : 13 );
132300110222         //else;
132400110207           V2prmTIMp = ILVprmTIMp;
132500110222         //endif;
132600110208
132700110208         if  ILVtimA > *zero;
132800110208           reset  WLBdat;
132900110208           G08inv = %int( %subst( %editc( ILVtimA : 'X' ) : 1 : 8 ) );
133000110208           xsrda8 ( WLBdat );
133100110208           if  G08err = *off;
133200110208             V2timAd = G08dat;
133300110208           endif;
133400110208           V2timAh  = %int( %subst( %editc( ILVtimA : 'X' ) : 9 ) );
133500110208         endif;
133600110207
133700110207         V2risult = ILVrisult;
133800110104
133900110104         // -?Impostazione attributi di visualizzazione?
134000110216         EvidCol    = ( ILVcol    >= '10' );
134100110216         EvidRisult = ( ILVrisult <> *blank  and  ILVrisult <> 'OK' );
134200110216
134300110216         // -?Verifica se abilitare F9 in Sede o in Filiale?
134400110216         F9Filiale = ( %subst(knsif : 1 : 3) = 'FIL' );
134500110216
134600110216         // -?Verifica se presente spedizione in filiale?
134700110216         IF  F9Filiale;
134800110216           if  Not %open(FNARB01L);
134900110216             open  FNARB01L;
135000110216           endif;
135100110216           k_ARBaas = ILVaas;
135200110216           k_ARBlnp = ILVlnp;
135300110216           k_ARBnrs = ILVnrs;
135400110216           k_ARBnsp = ILVnsp;
135500110222           setll  %kds( k04fnarb01 )  FNARB000;
135600110223         // -?Verifica se presente spedizione in sede?
135700110223         //  ?(chain per reperire TASTBL)?
135800110216         ELSE;
135900110216           if  Not %open(TITAS30C);
136000110216             open  TITAS30C;
136100110216           endif;
136200110216           k_TASaas = ILVaas;
136300110216           k_TASlnp = ILVlnp;
136400110216           k_TASnrs = ILVnrs;
136500110216           k_TASnsp = ILVnsp;
136600110223           chain  %kds( k05titas30 : 4)  TITAS30C;
136700110216         ENDIF;
136800110216
136900110216         // -?Attivazione (condizionata) tasti funzionali?
137000110223         F9Attivo  = (( Not F9Filiale  and  %found(TITAS30C) )  OR
137100110222                      ( F9Filiale      and  %equal(FNARB01L) ));
137200110222         F19Attivo = ( ILVcol > '01'  and  ILVrisult = 'OK' );
137300170707         F21Attivo = ( dILVlibero.§ILVhmp <> *blank );
137400170808
137500170808         // -?Su AS888 F19=Immagine LdV lo si può attivare anche se?
137600170808         //  ?la spedizione risulta elaborata regolarmente (Log = "01")?
137700170808         //  ?perchè tanto le immagini finiscono nella stessa directory?
137800170808         F19As888  = ( Not F19Attivo  and
137900170808                       %subst( currSysNetA : 1 : 6 ) = 'AS888 '  and
138000170808                       ILVcol = '01'  and  ILVrisult = 'OK' );
138100170808         if  F19As888;
138200170808           F19Attivo = *on;
138300170808         endif;
138400110103
138500110103       ENDSR;
138600120703
138700120703       //--------------------------------------------------------------
138800120703       //?Gestione window W02 - Visualizzazione dati aggiuntivi.       ?
138900120703       //--------------------------------------------------------------
139000120703       BEGSR  sr_GesW02;
139100120703
139200120703         // -?Inizializzazione videata?
139300120703         if  $InzW02 = *on;
139400120703           exsr  sr_InzW02;
139500120703           $InzW02 = *off;
139600120703         endif;
139700120703
139800120703         // -?Emissione Window?
139900120703         exfmt  LR65W02;
140000120703
140100120703         // -?F12=Ritorno?
140200120703         if  dsp_aid = c_F12;
140300120703           $Video = 'D2';
140400120703         endif;
140500120703
140600120703       ENDSR;
140700120703
140800120703       //--------------------------------------------------------------
140900120703       //?Inizializzazione window W02 - Visualizzaz. dati aggiuntivi.  ?
141000120703       //--------------------------------------------------------------
141100120703       BEGSR  sr_InzW02;
141200120703
141300120703         dILVlibero = ILVlibero;
141400120703
141500120703         clear  LR65W02;
141600120703
141700170707         W2Chmp = %subst( dILVlibero.§ILVhmp : 1 : 2 ) + ':' +
141800170707                  %subst( dILVlibero.§ILVhmp : 3 : 2 );
141900120703
142000120703       ENDSR;
142100110216
142200110216       //--------------------------------------------------------------
142300110216       //?Preparazione stringa SQL S1.                                 ?
142400110216       //--------------------------------------------------------------
142500110216       BEGSR  sr_PrepSQL;
142600110216
142700110216         clear  wCountSel;
142800110216         wSQL = 'select rrn(FIILV00F), FIILV00F.* from FIILV00F';
142900110216
143000110216         // -?Selezione per filiale distinta (gestibile dall'utente)?
143100110216         if  %subst( Knmus : 1 : 3 ) <> 'EDP';
143200110216           if  wCountSel = *zero;
143300110217             wSQL += ' where';
143400110216           else;
143500110217             wSQL += ' and';
143600110216           endif;
143700110216           wSQL += ' ILVfgs in (';
143800110216           For  xx = 1  To  %elem($POg);
143900110216             if  $POg(xx) <= *zero;
144000110216               leave;
144100110216             endif;
144200110216             if  xx > 1;
144300110216               wSQL += ', ';
144400110216             endif;
144500110216             wSQL += $POg(xx);
144600110216           EndFor;
144700110216           wSQL += ')';
144800110217           wCountSel += 1;
144900110216         endif;
145000110216
145100110217         // -?Selezione per filiale distinta?
145200110217         if  V1Cfgs > *zero;
145300110217           if  wCountSel = *zero;
145400110217             wSQL += ' where';
145500110217           else;
145600110217             wSQL += ' and';
145700110217           endif;
145800110217           wSQL += ' ( substr( ILVprmDST, 1, 3 ) = ''' +
145900110217                          %editc( V1Cfgs : 'X' ) + ''' or +
146000110217                       ILVfgs = ' + %triml( %editc( V1Cfgs : '3' ) ) +
146100110217                   ' )';
146200110217           wCountSel += 1;
146300110217         endif;
146400110217
146500110216         // -?Selezione per numero distinta?
146600110217         if  V1Cndc > *zero;
146700110216           if  wCountSel = *zero;
146800110217             wSQL += ' where';
146900110216           else;
147000110217             wSQL += ' and';
147100110216           endif;
147200110217           wSQL += ' ( substr( ILVprmDST, 4 ) = ''' +
147300110225                       %editc( V1Cndc : 'X' ) + ''' or +
147400110217                       ILVndc = ' + %triml( %editc( V1Cndc : '3' ) ) +
147500110217                   ' )';
147600110216           wCountSel += 1;
147700110216         endif;
147800110216
147900110216         // -?Selezione per numero spedizione?
148000110217         //if  V1Clnp > *zero  and  V1Cnsp > *zero  and  V1Caas > *zero;
148100110217         //  if  wCountSel = *zero;
148200110217         //    wSQL += ' where';
148300110217         //  else;
148400110217         //    wSQL += ' and';
148500110217         //  endif;
148600110217         //  wSQL += ' ( ILVprmSPE = ''' + %editc( V1Caas : 'X' )
148700110217         //                              + %editc( V1Clnp : 'X' )
148800110217         //                              + %editc( V1Cnrs : 'X' )
148900110217         //                              + %editc( V1Cnsp : 'X' ) + ''' +
149000110217         //           or ( ILVaas = ' + %trim( %editc( V1Caas : '3' ) ) +
149100110217         //          ' and ILVlnp = ' + %trim( %editc( V1Clnp : '3' ) ) +
149200110217         //          ' and ILVnrs = ' + %trim( %editc( V1Cnrs : '3' ) ) +
149300110217         //          ' and ILVnsp = ' + %trim( %editc( V1Cnsp : '3' ) ) +
149400110217         //          ' ) )';
149500110217         //  wCountSel += 1;
149600110217         //endif;
149700110217         // -?Selezione per linea di partenza?
149800110217         if  V1Clnp > *zero;
149900110217           if  wCountSel = *zero;
150000110217             wSQL += ' where';
150100110217           else;
150200110217             wSQL += ' and';
150300110217           endif;
150400110217           wSQL += ' ( substr( ILVprmDST, 5, 3 ) = ''' +
150500110217                          %editc( V1Clnp : 'X' ) + ''' or +
150600110217                       ILVlnp = ' + %triml( %editc( V1Clnp : '3' ) ) +
150700110217                   ' )';
150800110217           wCountSel += 1;
150900110217         endif;
151000110217         // -?Selezione per numero serie?
151100110217         if  V1Cnrs > *zero;
151200110217           if  wCountSel = *zero;
151300110217             wSQL += ' where';
151400110217           else;
151500110217             wSQL += ' and';
151600110217           endif;
151700110217           wSQL += ' ( substr( ILVprmDST, 8, 2 ) = ''' +
151800110222                          %editc( V1Cnrs : 'X' ) + ''' or +
151900110217                       ILVnrs = ' + %triml( %editc( V1Cnrs : '3' ) ) +
152000110217                   ' )';
152100110217           wCountSel += 1;
152200110217         endif;
152300110217         // -?Selezione per numero spedizione?
152400110217         if  V1Cnsp > *zero;
152500110217           if  wCountSel = *zero;
152600110217             wSQL += ' where';
152700110217           else;
152800110217             wSQL += ' and';
152900110217           endif;
153000110217           wSQL += ' ( substr( ILVprmDST, 10, 7 ) = ''' +
153100110222                          %editc( V1Cnsp : 'X' ) + ''' or +
153200110217                       ILVnsp = ' + %triml( %editc( V1Cnsp : '3' ) ) +
153300110217                   ' )';
153400110217           wCountSel += 1;
153500110217         endif;
153600110222         // -?Selezione per anno spedizione?
153700110222         if  V1Caas > *zero;
153800110222           if  wCountSel = *zero;
153900110222             wSQL += ' where';
154000110222           else;
154100110222             wSQL += ' and';
154200110222           endif;
154300110222           wSQL += ' ( substr( ILVprmDST, 1, 4 ) = ''' +
154400110222                          %editc( V1Caas : 'X' ) + ''' or +
154500110222                       ILVaas = ' + %triml( %editc( V1Caas : '3' ) ) +
154600110222                   ' )';
154700110222           wCountSel += 1;
154800110222         endif;
154900110330
155000110330         // -?Selezione per codice Log?
155100110330         if  V1Ccol <> *blank;
155200110330           if  wCountSel = *zero;
155300110330             wSQL += ' where';
155400110330           else;
155500110330             wSQL += ' and';
155600110330           endif;
155700110330           wSQL += ' ILVcol in (';
155800110330           clear  wCountCol;
155900110330           For  xx = 1  To  %elem($V1Ccol);
156000110330             if  $V1Ccol(xx) <> *blank;
156100110330               if  wCountCol > *zero;
156200110330                 wSQL += ', ';
156300110330               endif;
156400110330               wSQL += '''' + $V1Ccol(xx) + '''';
156500110330               wCountCol += 1;
156600110330             endif;
156700110330           EndFor;
156800110330           wSQL += ')';
156900110330           wCountSel += 1;
157000110330         endif;
157100110216
157200110222         // -?Esclusione per codice LOG "00" (ancora da elaborare)?
157300110222         if  V1Ce00 <> *blank;
157400110216           if  wCountSel = *zero;
157500110217             wSQL += ' where';
157600110216           else;
157700110217             wSQL += ' and';
157800110216           endif;
157900110222           wSQL += ' ILVcol <> ''00''';
158000110216           wCountSel += 1;
158100110216         endif;
158200110216
158300110216         // -?Selezione per risultato di output?
158400110222         //if  V1Crisult <> *blank;
158500110222         //  if  wCountSel = *zero;
158600110222         //    wSQL += ' where';
158700110222         //  else;
158800110222         //    wSQL += ' and';
158900110222         //  endif;
159000110222         //  wSQL += ' ILVrisult = ''' + V1Crisult + '''';
159100110222         //  wCountSel += 1;
159200110222         //endif;
159300110216
159400130227         // -?Selezione per data elaborazione firma su AS/400 (dal - al)?
159500130227         select;
159600130227           when  V1Cdri > *zero  and  V1Cdrf = *zero;
159700130227             if  wCountSel = *zero;
159800130227               wSQL += ' where';
159900130227             else;
160000130227               wSQL += ' and';
160100130227             endif;
160200130227             wSQL += ' ILVtima >= ' + %triml( %editc( W1Cdri : 'Z' ) )
160300130227                                    + '000000';
160400130227             wCountSel += 1;
160500130227           when  V1Cdri = *zero  and  V1Cdrf > *zero;
160600130227             if  wCountSel = *zero;
160700130227               wSQL += ' where';
160800130227             else;
160900130227               wSQL += ' and';
161000130227             endif;
161100130227             wSQL += ' ILVtima <= ' + %triml( %editc( W1Cdrf : 'Z' ) )
161200130227                                    + '999999';
161300130227             wCountSel += 1;
161400130227           when  V1Cdri > *zero  and  V1Cdrf > *zero;
161500130227             if  wCountSel = *zero;
161600130227               wSQL += ' where';
161700130227             else;
161800130227               wSQL += ' and';
161900130227             endif;
162000130227             wSQL += ' ILVtima between ' + %triml( %editc( W1Cdri : 'Z' ) )
162100130227                                         + '000000 and '
162200130227                                         + %triml( %editc( W1Cdrf : 'Z' ) )
162300130227                                         + '999999';
162400130227             wCountSel += 1;
162500130227         endsl;
162600170707
162700170707         // -?Selezione per codice Applicazione (D=Ds Group, G=Gulliver)?
162800170707         if  V1Capl <> *blank;
162900170707           if  wCountSel = *zero;
163000170707             wSQL += ' where';
163100170707           else;
163200170707             wSQL += ' and';
163300170707           endif;
163400170707           wSQL += ' substr(ILVlibero, 5, 1) = ''' + V1Capl + '''';
163500170707           wCountSel += 1;
163600170707         endif;
163700110216
163800110216         // -?Ordinamento?
163900110216         select;
164000110216           // -?Ordinamento per Data/Ora Elab. Firma/Distinta/Spedizione?
164100110216           when  wOrdSfl = 0;
164200110301             wSQL += ' order by ILVtimA desc, +
164300110216                                ILVfgs, ILVndc, +
164400110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp';
164500110216           // -?Ordinamento per Distinta/Spedizione?
164600110216           when  wOrdSfl = 1;
164700110216             wSQL += ' order by ILVfgs, ILVndc, +
164800110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp';
164900110216           // -?Ordinamento per Spedizione/Distinta?
165000110216           when  wOrdSfl = 2;
165100110216             wSQL += ' order by ILVaas, ILVlnp, ILVnrs, ILVnsp, +
165200110216                                ILVfgs, ILVndc';
165300110216           // -?Ordinamento per Cod.Log/Spedizione/Distinta/Risultato?
165400110216           when  wOrdSfl = 3;
165500110330             wSQL += ' order by ILVcol desc, +
165600110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp, +
165700110216                                ILVfgs, ILVndc, +
165800110216                                ILVrisult';
165900110216           // -?Ordinamento per Risultato/Spedizione/Distinta/Cod.Log?
166000110216           when  wOrdSfl = 4;
166100110216             wSQL += ' order by ILVrisult, +
166200110216                                ILVaas, ILVlnp, ILVnrs, ILVnsp, +
166300110216                                ILVfgs, ILVndc, +
166400110216                                ILVcol';
166500110216         endsl;
166600110216
166700110216         wSQL += ' for fetch only';
166800110216
166900110216       ENDSR;
167000110216
167100110216       //--------------------------------------------------------------
167200110216       //?Apertura cursore C1.                                         ?
167300110216       //--------------------------------------------------------------
167400110216       BEGSR  sr_OpenCursor;
167500110216
167600110216         // -?Dichiarazione cursore?
167700110216         exec sql   prepare S1   from :wSQL;
167800110216         exec sql   declare C1   cursor for S1;
167900110216
168000110216         // -?Apertura del cursore?
168100110216         exec sql   open C1;
168200110216
168300110216         if  SQLcode < *zero;
168400110216           $Err = *on;
168500110216           $EoF = *on;
168600110216           exsr  sr_PrintErrSQL;
168700110216         endif;
168800110216
168900110216       ENDSR;
169000110216
169100110216       //--------------------------------------------------------------
169200110216       //?Chiusura cursore C1.                                         ?
169300110216       //--------------------------------------------------------------
169400110216       BEGSR  sr_CloseCursor;
169500110216
169600110216         // -?Chiusura del cursore?
169700110216         exec sql   close C1;
169800110216
169900110216       ENDSR;
170000110216
170100110216       //--------------------------------------------------------------
170200110216       //?Lettura cursore C1.                                          ?
170300110216       //--------------------------------------------------------------
170400110216       BEGSR  sr_ReadCursor;
170500110216
170600110216         clear  FIILVds;
170700110216
170800110216         exec sql   fetch next   from C1   into :wILVrrn, :FIILVds;
170900110216
171000110216         select;
171100110216           when  SQLcode = 100;
171200110216             $EoF = *on;
171300110216           when  SQLcode < *zero;
171400110216             $Err = *on;
171500110216             $Eof = *on;
171600110216             exsr  sr_PrintErrSQL;
171700110216         endsl;
171800110216
171900110216       ENDSR;
172000110216
172100110216       //--------------------------------------------------------------
172200110216       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
172300110216       //--------------------------------------------------------------
172400110216       BEGSR  sr_PrintErrSQL;
172500110216
172600110216         // -?Stampa del Dump?
172700110216         Dump(A);
172800110216
172900110216         // -?Stampa del Job-Log?
173000110216         Qcmd = 'DSPJOBLOG job(*) output(*print)';
173100110216         exsr  sr_ExecCmd;
173200110216
173300110216         // -?Chiusura del programma?
173400110216         exsr  sr_RoutEnd;
173500110216
173600110216       ENDSR;
173700110216
173800110216       //--------------------------------------------------------------
173900110216       //?Esecuzione del comando (già impostato).                      ?
174000110216       //--------------------------------------------------------------
174100110216       BEGSR  sr_ExecCmd;
174200110216
174300110216         clear Qcap0100;
174400110216         Qcabcsdh = *off;
174500110216         Qcapa    = *off;
174600110216         Qcacmdss = *off;
174700110216         Qcaerved = *allX'00';
174800110216
174900110216         clear Qusec;
175000110216         Qusbprv  = %size(Qusec);
175100110216
175200110216         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
175300110216                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
175400110216                           0 : 0 : Qusec);
175500110216
175600110216         // -?Invio *msg per segnalazione errore?
175700110216         //if  Qusei <> *blank;
175800110216         //  ...;
175900110216         //endif;
176000110216
176100110216       ENDSR;
176200101223
176300101223       //--------------------------------------------------------------
176400101223       //?Operazioni finali.                                           ?
176500101223       //--------------------------------------------------------------
176600101223       BEGSR  sr_RoutEnd;
176700110103
176800110103         // -?Chiusura archivi?
176900110207         if  %open(FIILV00F);
177000110207           close  FIILV00F;
177100110103         endif;
177200110216         if  %open(FNARB01L);
177300110216           close  FNARB01L;
177400110216         endif;
177500110216         if  %open(TITAS30C);
177600110216           close  TITAS30C;
177700110216         endif;
177800101223
177900101223         // -?Uscita dal *pgm?
178000101223         return;
178100101223
178200101223       ENDSR;
178300101223
178400101223      /end-free
178500101223
178600101223       //--------------------------------------------------------------
178700101223       //?Schiere a tempo di compilazione.                             ?
178800101223       //--------------------------------------------------------------
178900101223
179000110330** -?$CoL:?Codici LOG?
17910011033000 Spedizione in distinta chiusa - da elaborare
17920011033001 Spedizione elaborata
17930011033008 Sped. non consegnata in dist. - immagine a SCARTO
17940011033009 LdV da scannerizzare - immagine a SCARTO
17950011033021 Parametri di input errati - immagine NON creata
17960011033022 Distinta non trovata/annullata - immagine a SCARTO
17970011033023 Sped./Dist. non trovata in FIILV - immag. a SCARTO
17980011033025 Sped. non trovata in FNARB - immagine NON creata
17990013011051 BOLLA RECUPERO - Immagine già generata
18000013011052 BOLLA RECUPERO - Spedizione non trovata in FNARB
180100101223** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
180200110222Nessuna filiale gestibile dall'utente.  Premere "Invio" per uscire.            1
180300110228Inserire anche la filiale della distinta                                       2
180400110228Inserire il numero completo della spedizione: LNP, (NRS,) NSP e AAS            3
180500130429Codice Log errato. Previsti: 00, 01, 08, 09, 21, 22, 23, 25, 51 e 52.          4
180600110405Flag "Esclusione record da elaborare" errato: S=Sì, _=No                       5
180700110405Esclusione rec. da elaborare INCOMPATIBILE con selezione codice Log "00"       6
180800110405Data formalmente errata                                                        7
180900130227Data iniziale (dal) successiva a quella finale (al)                            8
181000130227Opzione errata                                                                 9
181100130227SUPERATO IL NUMERO MASSIMO DI RECORD VISUALIZZABILI NEL SUBFILE (9999)        10
181200170707Codice Applicazione errato                                                    11
