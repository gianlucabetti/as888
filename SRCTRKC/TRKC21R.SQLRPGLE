000100141104       //==============================================================
000200150112       //?TRKC21R * Interrogazione clienti in Campagna                 ?
000300141104       //==============================================================
000400141104
000500141104       //--------------------------------------------------------------
000600141104       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700141104       //--------------------------------------------------------------
000800141104
000900141104     /*PRM  dbgview(*source)
001000141104     /*END
001100141104
001200141104       //--------------------------------------------------------------
001300141104       //?Specifiche di controllo.                                     ?
001400141104       //--------------------------------------------------------------
001500141104
001600141110     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700141104     h dftactgrp(*no) actgrp(*caller)
001800141106     h alwnull(*inputonly)
001900141104
002000141104       //--------------------------------------------------------------
002100141104       //?Dichiarazione file.                                          ?
002200141104       //--------------------------------------------------------------
002300141104
002400141104       // -?Campagne Commerciali?
002500141104     fTICMP01L  if   e           k disk
002600141104
002700141104       // -?Clienti in Campagne Commerciali?
002800141104     fTICMC01L  if   e           k disk
002900141104
003000141104       // -?Fase Avanzamento Campagne Commerciali?
003100141104     fTICMF01L  if   e           k disk
003200141118
003300141124       // -?Trattative?
003400141124     fTIVIS01L  if   e           k disk
003500141203
003600141203       // -?Anagrafica Clienti Potenziali?
003700141203     fTNCPO01L  if   e           k disk
003800141201
003900141201       // -?File Info Commerciali?
004000141201     fTICPI01L  if   e           k disk
004100141104
004200141104       // -?Anagrafica Clienti?
004300141104     fCNACO00F  if   e           k disk
004400141104     fCNCLP00F  if   e           k disk
004500141106
004600141106       // -?Anagrafica Commerciali?
004700141106     fAZCMM01L  if   e           k disk
004800141104
004900141104       // -?Organigramma filiale/aziendale?
005000141104     fAZORG01L  if   e           k disk
005100141104
005200141104       // -?Tabelle?
005300141104     fTABEL00F  if   e           k disk
005400141104
005500141104       // -?Video Gestione?
005600150112     fTRKC21D   cf   e             workstn
005700150112     f                                     sfile( KC21S01 : S01nrr )
005800141104     f                                     indds( IndDspF )
005900141104     f                                     infds( InfDspF )
006000141104
006100141104       //--------------------------------------------------------------
006200141104       //?Definizione costanti.                                        ?
006300141104       //--------------------------------------------------------------
006400141104
006500141105       // -?Codice autorizzazione profilo Tariffe Clienti?
006600141105     d c_Aut_TC        c                   const('§utegtc')
006700141106
006800141106       // -?Codice Utente fisso (in tabelle, anagrafiche, ecc.)?
006900141106     d c_KUT           c                   const(1)
007000141105
007100141104       // -?Numero di record in ciascuna videata di subfile?
007200150226     d c_SflPag        c                   const(16)
007300141104
007400141104       // -?Numero massimo di record gestibili in un SubFile?
007500141104     d c_MaxRec        c                   const(9999)
007600141104
007700141104       // -?Tasti funzionali a video?
007800141104     d c_F01           c                   const(x'31')
007900141104     d c_F02           c                   const(x'32')
008000141104     d c_F03           c                   const(x'33')
008100141104     d c_F04           c                   const(x'34')
008200141104     d c_F05           c                   const(x'35')
008300141104     d c_F06           c                   const(x'36')
008400141104     d c_F07           c                   const(x'37')
008500141104     d c_F08           c                   const(x'38')
008600141104     d c_F09           c                   const(x'39')
008700141104     d c_F10           c                   const(x'3A')
008800141104     d c_F11           c                   const(x'3B')
008900141104     d c_F12           c                   const(x'3C')
009000141104     d c_F13           c                   const(x'B1')
009100141104     d c_F14           c                   const(x'B2')
009200141104     d c_F15           c                   const(x'B3')
009300141104     d c_F16           c                   const(x'B4')
009400141104     d c_F17           c                   const(x'B5')
009500141104     d c_F18           c                   const(x'B6')
009600141104     d c_F19           c                   const(x'B7')
009700141104     d c_F20           c                   const(x'B8')
009800141104     d c_F21           c                   const(x'B9')
009900141104     d c_F22           c                   const(x'BA')
010000141104     d c_F23           c                   const(x'BB')
010100141104     d c_F24           c                   const(x'BC')
010200141104     d c_Enter         c                   const(x'F1')
010300141104     d c_RollDown      c                   const(x'F4')
010400141104     d c_RollUp        c                   const(x'F5')
010500141104
010600141104       // -?Attributi di Visualizzazione?
010700141104     d c_DspAtr_Norm   c                   const(x'20')
010800141104     d c_DspAtr_RI     c                   const(x'21')
010900141104     d c_DspAtr_HI     c                   const(x'22')
011000141104     d c_DspAtr_ND     c                   const(x'27')
011100141104     d c_DspAtr_BL     c                   const(x'28')
011200141104
011300141104       // -?Costante per controllo "caratteri solo numerici"?
011400141104     d c_Digits        c                   const('0123456789')
011500141104
011600141104       //--------------------------------------------------------------
011700141104       //?Definizione schiere.                                         ?
011800141104       //--------------------------------------------------------------
011900141104
012000141104       // -?Messaggi di errore?
012100141110     d sk_Msg          s             78    dim(14)  ctdata  perrcd( 1)
012200141104
012300141104       // -?Commerciali e Unificanti?
012400141212     d c_Max_Cmm       c                   const(9999)
012500141104     d sk_Cmm          s                   like(CMMcod)
012600141106     d                                     dim(c_Max_Cmm)  inz
012700141104     d sk_CmmU         s                   like(CMMuni)
012800141106     d                                     dim(c_Max_Cmm)  inz
012900141104
013000141104       // -?Filiali?
013100141104     d c_Max_Fil       c                   const(350)
013200141104     d sk_Fil          s                   like(ORGfil)
013300141106     d                                     dim(c_Max_Fil)  inz
013400141104     d sk_Car          s                   like(ORGcar)
013500141106     d                                     dim(c_Max_Fil)  inz(*hival)
013600141104
013700141104       // -?Importanza clienti da tabella "IC"?
013800141104     d sk_Ic           s              1    dim(10)
013900141104
014000141104
014100141104       //--------------------------------------------------------------
014200141104       //?Definizione aree dati.                                       ?
014300141104       //--------------------------------------------------------------
014400141104
014500141104       // -?Dati utente?
014600141104     d §AzUte        e ds                  extname(AZUTE00F)
014700141104     d                                     dtaara
014800141104     d §DatiUte      e ds                  extname(dDatiUte)
014900141104     d                                     dtaara
015000141104
015100141104       //--------------------------------------------------------------
015200141104       //?Definizione strutture dati.                                  ?
015300141104       //--------------------------------------------------------------
015400141104
015500141104       // -?Status ds?
015600141105     d Status         sds
015700141104     d   SDSpgm          *proc
015800141104
015900141104       // -?InfDS?
016000141104     d InfDspF         ds
016100141104     d   dsp_aid             369    369a
016200141104     d*//sfl_rrn             376    377i 0
016300141104     d*//min_nrr             378    379i 0
016400141104     d*//num_rcds            380    381i 0
016500141104
016600141104       // -?Indicatori su DspF?
016700141104     d IndDspF         ds
016800141104         // -?Emissione messaggio di errore?
016900141104     d   errMessage                    n   overlay( IndDspF : 28 )
017000141105         // -?Indicatori di gestione del SubFile?
017100141104     d   SflDsp_N                      n   overlay( IndDspF : 30 )
017200141104     d   SflDspCtl_N                   n   overlay( IndDspF : 31 )
017300141104     d   SflNxtChg                     n   overlay( IndDspF : 32 )
017400141104     d   SflEnd                        n   overlay( IndDspF : 33 )
017500141105         // -?Condizionamento campi a video?
017600141105     d   VisualKSU                     n   overlay( IndDspF : 41 )
017700141105     d   VisualKSC                     n   overlay( IndDspF : 42 )
017800141105     d   VisualCPO                     n   overlay( IndDspF : 43 )
017900141111     d   VisualCAR                     n   overlay( IndDspF : 44 )
018000141104         // -?Posizionamento cursore & segnalazione errore?
018100141104     d   PosCurOpz                     n   overlay( IndDspF : 50 )
018200141104     d   PosCurNCM                     n   overlay( IndDspF : 51 )
018300141104     d   PosCurKSU                     n   overlay( IndDspF : 52 )
018400141104     d   PosCurKSC                     n   overlay( IndDspF : 53 )
018500141104     d   PosCurCPO                     n   overlay( IndDspF : 54 )
018600141104     d   PosCurOSF                     n   overlay( IndDspF : 55 )
018700141104     d   PosCurCIC1                    n   overlay( IndDspF : 56 )
018800141104     d   PosCurCIC2                    n   overlay( IndDspF : 57 )
018900141104     d   PosCurCIC3                    n   overlay( IndDspF : 58 )
019000141104     d   PosCurCIC4                    n   overlay( IndDspF : 59 )
019100141104     d   PosCurCIC5                    n   overlay( IndDspF : 60 )
019200141104     d   PosCurFLT                     n   overlay( IndDspF : 61 )
019300141104     d   PosCurCAR                     n   overlay( IndDspF : 62 )
019400141105     d   PosCurCMU                     n   overlay( IndDspF : 63 )
019500141104         // -?Riemissione videata?
019600141104     d   errGenerico                   n   overlay( IndDspF : 99 )
019700141104
019800141104       // -?Campo unico per Importanza Clienti?
019900141105     d V01cic_ds       ds                  inz
020000141104     d   V01cic1                           inz
020100141104     d   V01cic2                           inz
020200141104     d   V01cic3                           inz
020300141104     d   V01cic4                           inz
020400141104     d   V01cic5                           inz
020500141105     d     sk_V01cic                       overlay( V01cic_ds : 1 )
020600141106     d                                     like(V01cic1)  dim(5)
020700141106     d                                     inz
020800141128
020900141201      *// // -?Campo unico per Categoria Merceologica?
021000150112     d*// KC20sct_ds      ds                  inz
021100150112     d*//   KC20sct1                          inz
021200150112     d*//   KC20sct2                          inz
021300150112     d*//   KC20sct3                          inz
021400150112     d*//     sk_KC20sct                      overlay( KC20sct_ds : 1 )
021500150112     d*//                                     like(KC20sct1)  dim(3)
021600141201     d*//                                     inz
021700141128
021800141201      *// // -?Campo unico per Paesi Estero?
021900150112     d*// KC20est_ds      ds                  inz
022000150112     d*//   KC20est1                          inz
022100150112     d*//   KC20est2                          inz
022200150112     d*//   KC20est3                          inz
022300150112     d*//     sk_KC20est                      overlay( KC20est_ds : 1 )
022400150112     d*//                                     like(KC20est1)  dim(3)
022500141201     d*//                                     inz
022600141128
022700141128       // -?Campo unico per Concorrenti?
022800150112     d*// KC20conc_ds     ds                  inz
022900150112     d*//   KC20conc1                         inz
023000150112     d*//   KC20conc2                         inz
023100150112     d*//   KC20conc3                         inz
023200150112     d*//     sk_KC20conc...
023300150112     d*//                                     overlay( KC20conc_ds : 1 )
023400150112     d*//                                     like(KC20conc1)  dim(3)
023500141201     d*//                                     inz
023600141128
023700141201      *// // -?Campo unico per Concorrenti Logistica?
023800150112     d*// KC20concL_ds    ds                  inz
023900150112     d*//   KC20concL1                        inz
024000150112     d*//   KC20concL2                        inz
024100150112     d*//   KC20concL3                        inz
024200150112     d*//     sk_KC20concL...
024300150112     d*//                                     overlay( KC20concL_ds : 1 )
024400150112     d*//                                     like(KC20concL1)  dim(3)
024500141201     d*//                                     inz
024600141104
024700141106       // -?Dati estratti via SQL?
024800141106     d wSQL_ds         ds                  inz  qualified
024900141106     d   SQLcli                            inz  like(ACOksc)
025000141118     d   SQLlib                            inz  like(ACOlib)
025100141106     d   SQLpot                            inz  like(CPOcpo)
025200141106     d   SQLrsc                            inz  like(ACOrag)
025300141201     d   SQLabl                            inz  like(ACOabl)
025400141107     d   SQLordclv                    1A   inz
025500141106     d   SQLclv                            inz  like(CLPclv)
025600141106     d   SQLcmm                            inz  like(CLPage)
025700141106
025800141106     d TICMC_ds      e ds                  extname(TICMC00F)
025900141106     d                                     inz  qualified
026000141104
026100141104       // -?Parametri ricevuti?
026200141104     d KPJBA         e ds
026300150112     d TRKC21ds      e ds
026400150112     d TRKC20ds      e ds
026500141201         // ·?Campo unico per Importanza Clienti?
026600150112     d   KC20cic_ds                   5    overlay( TRKC20ds :  57 )
026700150112     d     sk_KC20cic                      overlay( KC20cic_ds )
026800150112     d                                     like(KC20cic1)   dim(5)
026900141201         // ·?Campo unico per Categoria Merceologica?
027000150112     d   KC20sct_ds                   9    overlay( TRKC20ds : 145)
027100150112     d     sk_KC20sct                      overlay( KC20sct_ds )
027200150112     d                                     like(KC20sct1)   dim(3)
027300141201         // ·?Campo unico per Paesi Estero?
027400150112     d   KC20est_ds                  12    overlay( TRKC20ds : 231)
027500150112     d     sk_KC20est                      overlay( KC20est_ds )
027600150112     d                                     like(KC20est1)   dim(3)
027700141201         // ·?Campo unico per Concorrenti?
027800150112     d   KC20conc_ds                 12    overlay( TRKC20ds : 252)
027900150112     d     sk_KC20conc...
028000150112     d                                     overlay( KC20conc_ds )
028100150112     d                                     like(KC20conc1)  dim(3)
028200141201         // -?Campo unico per Concorrenti Logistica?
028300150112     d   KC20concL_ds                12    overlay( TRKC20ds : 276)
028400150112     d     sk_KC20concL...
028500150112     d                                     overlay( KC20concL_ds )
028600150112     d                                     like(KC20conL1)  dim(3)
028700141106
028800141106       // -?Ds AZORG per recupero NTW?
028900141106     d Og143         e ds                  inz  qualified
029000141104
029100141104       // -?Tabella 05 = Codici Area?
029200141104     d ds05          e ds                  inz
029300141107
029400141107      *// // -?Tabella IC = Importanza Clienti?
029500141107     d*// dsIC          e ds                  qualified  based(nullPtr)
029600141118
029700141118       // -?Comodo x Riferimenti per l'estrazione dati via SQL?
029800141124     d*// TIVIS_ds      e ds                  extname(TIVIS00F)
029900141124     d*//                                     qualified  based(nullPtr)
030000141118     d TIATC_ds      e ds                  extname(TIATC00F)
030100141118     d                                     qualified  based(nullPtr)
030200141104
030300141104       //--------------------------------------------------------------
030400141104       //?Definizione variabili globali.                               ?
030500141104       //--------------------------------------------------------------
030600141104
030700141104       // -?Flags booleani?
030800141104     d $Fine           s               n   inz
030900141104     d $InzD01         s               n   inz(*on)
031000141113     d $InzS01         s               n   inz(*on)
031100141104     d $EoF            s               n   inz
031200141104     d $RecOK          s               n   inz
031300141128     d $First          s               n   inz
031400141128     d $TTR            s               n   inz
031500141128     d $Estero         s               n   inz
031600141128     d $Conc           s               n   inz
031700141128     d $ConcLog        s               n   inz
031800141104
031900141104       // -?Indici di schiera?
032000150127     d xx              s              5  0 inz
032100150127     d yy              s              5  0 inz
032200141104
032300141104       // -?Campi associati al video?
032400141104     d $Video          s              2    inz('D1')
032500141104     d S01nrr          s              4  0 inz
032600141106     d w_SflPag        s              3  0 inz
032700141104     d wPag            s              4  0 inz
032800141104     d wRig            s              3  0 inz
032900141104
033000141118       // -?Stringhe SQL da eseguire?
033100141104     d wSQL            s           2048    varying        inz
033200141118     d wSQL_tmp        s           2048    varying        inz
033300141118
033400141118       // -?Altri dati estratti via SQL?
033500141118     d wACOrag         s                   inz  like(ACOrag)
033600141124     d*// wVIScpo         s                   inz  like(TIVIS_ds.VIScpo)
033700141124     d*// wVISksc         s                   inz  like(TIVIS_ds.VISksc)
033800141118     d wATCdco         s                   inz  like(TIATC_ds.ATCdco)
033900141118     d wATCcad         s                   inz  like(TIATC_ds.ATCcad)
034000141105
034100141105       // -?Ordinamento richiesto?
034200141105     d wOrd            s              1  0 inz
034300141110
034400141114       // -?Conteggio delle opzioni 1=Selezione inserite?
034500141110     d wCountOpz1      s              1  0 inz
034600141118
034700141118       // -?Campi data?
034800141124     d wData_DMY       s               d   inz  datfmt(*DMY)
034900141201
035000141201       // -?Campi di Comodo?
035100141201     d wCodUni         s                   inz  like(ACOksc)
035200141104
035300141104       //--------------------------------------------------------------
035400141104       //?Definizione prototipi procedure usate.                       ?
035500141104       //--------------------------------------------------------------
035600141104
035700141104       // -?Reperimento dati utente?
035800141104     d TIBS34ds      e ds                  inz
035900141104      /copy gaitrasrc/srcProtoPR,TIBS34R
036000141104
036100141104       // -?Controllo Abilitazioni?
036200141104     d TNTAA1ds      e ds                  inz
036300141104      /copy gaitrasrc/srcProtoPR,TNTAA1C
036400141201
036500141201       // -?Reperimento Unificante Padre?
036600141201     d TIBS10ds      e ds                  inz
036700141201     d*//skc                  21   5520    dim(500)
036800141201      /copy gaitrasrc/srcProtoPR,TIBS10R
036900141111
037000141111       // -?Interrogazione Campagne?
037100150113     d TRKC02ds      e ds                  inz
037200150113     d trkc02r         pr                  extpgm('TRKC02R')
037300141111     d   kpjba                             likeds(kpjba)
037400150113     d   trkc02ds                          likeds(TRKC02ds)
037500141104
037600141104       // -?Caricamento Filiali gestite?
037700141104     d TRUL31ds      e ds                  inz
037800141106     d   sk_POG                       3    overlay( O31pog )  dim(250)
037900141104      /copy gaitrasrc/srcProtoPR,TRUL31R
038000141104
038100141104       // -?Selezione di un Commerciale (unificante e non)?
038200141104      /copy gaitrasrc/srcprotoPI,TRMK43R_1
038300141104      /copy gaitrasrc/srcprotoPR,TRMK43R
038400141111
038500141111       // -?Interrogazione Anagrafica Clienti?
038600141111     d TNTAI2ds      e ds                  inz
038700141111     d TNTAI2R         pr                  extpgm('TNTAI2R')
038800141111     d   kPJbA                             likeds(KPJBA)
038900141111     d   tntaI2ds                          likeds(TNTAI2ds)
039000141104
039100141113       // -?Ricerca Tabelle (selezione multipla) - TABEL00F?
039200141104     d idTabella       s              2    inz
039300141104     d Ordinamento     s              1    inz
039400141104     d idElemento      s              8    inz
039500141104     d TastoUscita     s              1    inz
039600141104      /copy gaitrasrc/srcProtoPR,TRUL19R
039700141113
039800141113      *// // -?Ricerca/Controllo tabelle - TNTBE00F?
039900141113     d*// TIBS02ds      e ds                  inz
040000141113      *///copy gaitrasrc/srcProtoPR,TIBS02R
040100141104
040200141113      *// // -?Reperimento dati tabelle (TABEL00F & TNTBE00F)?
040300141106      *///copy gaitrasrc/srcProtoPI,TRULTAB
040400141106      *///copy gaitrasrc/srcProtoPR,TRULTAB
040500141106
040600141106       // -?API QCAPCMD (Process Commands)?
040700141106     d Qcmd            s           2048    inz  varying
040800141106      /copy qSysInc/qRpgleSrc,QCAPCMD
040900141106      /copy gaitrasrc/srcProtoPR,QCAPCMD
041000141106
041100141106       // -?Parametri gestione errori API.?
041200141106      /copy qsysinc/qrpglesrc,QUSEC
041300141104
041400141104       //--------------------------------------------------------------
041500141104       //?Definizione key-list.                                        ?
041600141104       //--------------------------------------------------------------
041700141104
041800141106       // -?File TICMP01L?
041900141106     d keyTICMP01    e ds                  extname( TICMP01L : *key )
042000141112     d                                     prefix(k_)  inz
042100141106
042200141106       // -?File TICMC01L?
042300141106     d keyTICMC01    e ds                  extname( TICMC01L : *key )
042400141112     d                                     prefix(k_)  inz
042500141106
042600141106       // -?File TICMF01L?
042700141106     d keyTICMF01    e ds                  extname( TICMF01L : *key )
042800141112     d                                     prefix(k_)  inz
042900141118
043000141124       // -?File TIVIS01L?
043100141124     d keyTIVIS01    e ds                  extname( TIVIS01L : *key )
043200141124     d                                     prefix(k_)  inz
043300141106
043400141106       // -?File CNACO00F / CNCLP00F?
043500141104     d keyCNACO00    e ds                  extname( CNACO00F : *key )
043600141112     d                                     prefix(k_)  inz
043700141105
043800141105       // -?File TNCPO01L?
043900141105     d keyTNCPO01    e ds                  extname( TNCPO01L : *key )
044000141112     d                                     prefix(k_)  inz
044100141104
044200141104       // -?File TABEL00F?
044300141104     d keyTABEL00    e ds                  extname( TABEL00F : *key )
044400141112     d                                     prefix(k_)  inz
044500141104
044600141104       // -?File AZCMM01L?
044700141104     d keyAZCMM01    e ds                  extname( AZCMM01L : *key )
044800141112     d                                     prefix(k_)  inz
044900141104
045000141104       //--------------------------------------------------------------
045100141104       //?M A I N - L I N E                                            ?
045200141104       //--------------------------------------------------------------
045300141104
045400141104     c     *Entry        plist
045500141104     c                   parm                    KPJBA
045600150112     c                   parm                    TRKC21ds
045700150112     c                   parm                    TRKC20ds
045800141104
045900141104      /free
046000141104
046100141104       // -?Operazioni iniziali?
046200141104       exsr  sr_RoutInz;
046300141104
046400141104       // -?Ciclo di gestione del file video?
046500141104       DoW  $Fine = *off;
046600141104
046700141104         select;
046800141106
046900141104           // -?Gestione videata D1 (parzializzazioni)?
047000141104           when  $Video = 'D1';
047100141104             exsr  sr_GesD01;
047200141106
047300141107           // -?Gestione videata S1 (subfile)?
047400141104           when  $Video = 'S1';
047500141104             exsr  sr_GesS01;
047600141106
047700141104           // -?? ? ??
047800141104           other;
047900141104             $Fine = *on;
048000141106
048100141104         endsl;
048200141104
048300141104       EndDo;
048400141104
048500141104       // -?Operazioni finali?
048600141104       exsr  sr_RoutEnd;
048700141104
048800141104       //--------------------------------------------------------------
048900141104       //?Operazioni iniziali.                                         ?
049000141104       //--------------------------------------------------------------
049100141104       BEGSR  sr_RoutInz;
049200141105
049300141105         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
049400141104
049500141104         // -?Impostazione chiusura?
049600141104         *inLR = *on;
049700141104
049800141104         // -?Pulizia parametri di solo Output?
049900150112         clear  oKC21ncm;
050000150112         clear  oKC21ksu;
050100150112         clear  oKC21ksc;
050200150112         clear  oKC21cpo;
050300150112         clear  oKC21fxx;
050400150112         clear  oKC21err;
050500150112         clear  oKC21msg;
050600141104
050700141104         // -?Impostazione nome programma a video?
050800141104         T01pgm = SDSpgm;
050900141104
051000141104         // -?Impostazione campi chiave fissi?
051100141106         k_TBLkut = c_KUT;
051200141104
051300141104         // -?Reperimento dati job?
051400141104         exsr  sr_DatiJob;
051500150112         if  iKC21abi = *blank;
051600150112           iKC21abi = UTEaut;
051700141106         endif;
051800141105
051900141105         // -?Controllo Autorizzazione dell'Utente alle Tariffe Clienti?
052000141128         //  ?(GIÀ ESEGUITO DAL CHIAMANTE - vedi TNTA87R e TRMK18R)?
052100141112         //reset  TNTAA1ds;
052200141112         //ITAA1caut = c_Aut_TC;
052300141112         //kPJbU = TNTAA1ds;
052400141112         //TNTAA1C ( kPJbA );
052500141112         //TNTAA1ds = kPJbU;
052600141112         //If  oTAA1fabi = 'N';
052700141112         //  $Fine    = *on;
052800150112         //  oKC21err = *on;
052900150112         //  oKC21msg = sk_Msg(01);
053000141112         //  leavesr;
053100141112         //EndIf;
053200141104
053300141105         // -?Caricamento filiali abilitate al profilo con autorizzaz.?
053400141105         //  ?a gestione Tariffe Clienti (ricevuta dal *pgm chiamante)?
053500141104         clear  TRUL31ds;
053600150112         I31abi = iKC21abi;
053700141104         I31cdi = DUTdis;
053800141104         I31car = DUTare;
053900141104         I31cpo = DUTpou;
054000141106         TRUL31R ( kpjba : TRUL31ds );
054100141104         If  O31pog <= *zero;
054200141104           $Fine    = *on;
054300150112           oKC21err = *on;
054400150112           oKC21msg = sk_Msg(01);
054500141104           leavesr;
054600141104         EndIf;
054700141104
054800141105         // -?Preparazione (da AZCMM) elenco dei commerciali:?
054900141104         //  ?1) di filiale abilitata?
055000141104         //  ?2) con comm.le unificante di filiale abilitata?
055100141111         //  ?(serve per il controllo del Comm.le Unificante)?
055200141106         clear  sk_Cmm;
055300141106         clear  sk_CmmU;
055400141106         clear  xx;
055500141106
055600141111         setll  (*loval)  AZCMM000;
055700141111         read   AZCMM000;
055800141111
055900141111         DoW  NOT %eof(AZCMM01L);
056000141111
056100141111           If  %lookup( %subst( %editc( CMMcod : 'X' ) : 1 : 3 ) :
056200141111                        sk_POG ) > *zero  or
056300141111               %lookup( %subst( %editc( CMMuni : 'X' ) : 1 : 3 ) :
056400141111                        sk_POG ) > *zero;
056500141111             xx += 1;
056600141111             sk_Cmm(xx)  = CMMcod;
056700141212             sk_CmmU(xx) = CMMuni;
056800141111           EndIf;
056900141111
057000141111           read   AZCMM000;
057100141111
057200141111         EndDo;
057300141104
057400141104         // -?Intabellamento Filiali e relative Aree?
057500141104         clear  xx;
057600141104         setll  *loval  AZORG;
057700141104         read  AZORG;
057800141104
057900141104         DoW  NOT  %eof(AZORG01L);
058000141104
058100141104           if  ORGfva = *blank;
058200141104             xx += 1;
058300141104             sk_Fil(xx) = ORGfil;
058400141104             sk_Car(xx) = ORGcar;
058500141104           endif;
058600141104
058700141104           read  AZORG;
058800141104
058900141104         EndDo;
059000141104
059100141105         // -?Intabellamento Importanza Clienti?
059200141104         clear  xx;
059300141104         k_TBLcod = 'IC';
059400141104         setll  %kds( keyTABEL00 : 2 )  TABEL;
059500141104         reade  %kds( keyTABEL00 : 2 )  TABEL;
059600141104
059700141104         DoW Not %eof(TABEL00F);
059800141104
059900141104           if  TBLflg = *blank;
060000141104             xx += 1;
060100141104             sk_IC(xx) = %subst( TBLkey : 1 : 1 );
060200141104           endif;
060300141104
060400141104           reade  %kds( keyTABEL00 : 2 )  TABEL;
060500141104
060600141104         EndDo;
060700141128
060800141128         // -?Preparazione videata D01 con i parametri ricevuti?
060900141128         exsr  sr_InzD01;
061000141128         $InzD01 = *off;
061100141128
061200150112         V01ncm  = %editc( iKC21ncm : 'Z' );
061300150112         V01ksu  = iKC21ksu;
061400150112         V01ksc  = iKC21ksc;
061500150112         V01cpo  = iKC21cpo;
061600141128
061700141128         IF  %parms() > 2;
061800141128
061900150112           if  KC20osf = 'I'  or  KC20osf = 'R';
062000150112             V01osf   = KC20osf;
062100141128           endif;
062200150112           V01rag    = KC20rag;
062300150112           V01cic_ds = KC20cic_ds;
062400150112           V01flt    = KC20flt;
062500150112           if  KC20car <> *zero;
062600150112             V01car   = %editc( KC20car : 'X' );
062700141128           endif;
062800150112           if  KC20cmu <> *zero;
062900150112             V01cmu   = %editc( KC20cmu : 'X' );
063000141128           endif;
063100150112           if  KC20tcm = 'S'  or  KC20tcm = 'N';
063200150112             V01tcm   = KC20tcm;
063300141128           endif;
063400150127           if  KC20eat = 'S'  or  KC20eat = 'N';
063500150127             V01eat   = KC20eat;
063600150127           endif;
063700141128
063800141128           // -?Controllo dati nella videata D01?
063900141128           exsr  sr_CtrD01;
064000141128           if  ErrMessage;
064100150112             oKC21err = *on;
064200150112             oKC21msg = V1Dmsg;
064300141128             $Fine = *on;
064400141128           endif;
064500141201           $Video  = 'S1';
064600141201           $InzS01 = *on;
064700141128
064800141128         ENDIF;
064900141104
065000141104       ENDSR;
065100141104
065200141104       //--------------------------------------------------------------
065300141104       //?Reperimento Dati del job (Utente/Operativi).                 ?
065400141104       //--------------------------------------------------------------
065500141104       BEGSR  sr_DatiJob;
065600141104
065700141104         in(e) §AzUte;
065800141104         if NOT %error;
065900141104           in(e) §DatiUte;
066000141104         endif;
066100141104         if %error or RSut = *blanks;
066200141104           clear TIBS34ds;
066300141104           tibs34r ( tibs34ds );
066400141104           in §AzUte;
066500141104           in §DatiUte;
066600141104         endif;
066700141104
066800141104       ENDSR;
066900141104
067000141104       //--------------------------------------------------------------
067100141104       //?Gestione videata D01.                                        ?
067200141104       //--------------------------------------------------------------
067300141104       BEGSR  sr_GesD01;
067400141104
067500141105         // -?Inizializzazione videata?
067600141105         select;
067700141105           when  $InzD01 = *on;
067800141105             exsr  sr_InzD01;
067900141105             $InzD01 = *off;
068000141105           when  errGenerico = *off;
068100141105             clear  V01ksu;
068200141105             clear  V01ksc;
068300141105             clear  V01cpo;
068400141105         endsl;
068500141105
068600141105         // -?Emissione testata?
068700150112         write  KC21T01;
068800141104
068900141105         // -?Emissione videata?
069000150112         exfmt  KC21D01;
069100141105
069200141105         clear  errGenerico;
069300141105         clear  errMessage;
069400141105         clear  V1Dmsg;
069500141104
069600141105         Select;
069700141104
069800141105           // -?F3=Fine?
069900141105           when  dsp_aid = c_F03;
070000150112             oKC21fxx = '03';
070100141105             $Fine  = *on;
070200141104
070300141105           // -?Invio?
070400141105           Other;
070500141105             exsr  sr_CtrD01;
070600141105             if  errGenerico;
070700141104               leavesr;
070800141104             endif;
070900141105             $Video  = 'S1';
071000141104             $InzS01 = *on;
071100141105
071200141105         EndSl;
071300141104
071400141104       ENDSR;
071500141104
071600141104       //--------------------------------------------------------------
071700141105       //?Inizializzazione videata D01.                                ?
071800141104       //--------------------------------------------------------------
071900141105       BEGSR  sr_InzD01;
072000141104
072100150112         clear  KC21D01;
072200141105
072300150112         If  iKC21ncm <> *zero;
072400141111
072500141111           // -?SE ricevuta una Campagna Commerciale: verifica quale?
072600141111           //  ?cliente richiedere a video (D01) e da visualizzare nel?
072700141111           //  ?subfile (S01) in base a quello impostato nel 1° rec.?
072800141111           //  ?della Campagna Commerciale?
072900141111           clear  keyTICMC01;
073000150112           k_CMCncm = iKC21ncm;
073100141111           chain  %kds( keyTICMC01 : 1 )  TICMC000;
073200141111           if  %found(TICMC01L);
073300141111             VisualKSU = ( CMCksu <> *zero );
073400141111             VisualKSC = ( CMCksc <> *zero );
073500141111             VisualCPO = ( CMCcpo <> *zero );
073600141111           endif;
073700141111
073800141111         Else;
073900141111
074000141111           // -?Impostazione del cliente da richiedere a video (D01) e?
074100141111           //  ?da visualizzare nel subfile (S01) in base a quello?
074200141111           //  ?ricevuto come parametro?
074300150112           VisualKSU = ( iKC21ksu <> *zero );
074400150112           VisualKSC = ( iKC21ksc <> *zero );
074500150112           VisualCPO = ( iKC21cpo <> *zero );
074600141111
074700141111         EndIf;
074800141105
074900141105         // -?Richiesta di default: cod. Cliente Unificante?
075000141105         if  Not VisualKSU  and  Not VisualKSC  and  Not VisualCPO;
075100141105           VisualKSU = *on;
075200141105         endif;
075300141111
075400141111         // -?Abilitazione richiesta Area?
075500141111         VisualCAR = ( oTAA1cabi = 'AZ'  or  oTAA1cabi = 'DI' );
075600141128
075700141128
075800141128         // -?Campagna Commerciale?
075900150112         if  iKC21ncm > *zero;
076000150112           V01ncm = %editc( iKC21ncm : '3' );
076100141128         else;
076200141128           V01ncm = '?';
076300141128           PosCurNCM   = *on;
076400141128           errGenerico = *on;
076500141128         endif;
076600141106
076700141106         // -?Codice Cliente?
076800150112         V01ksu = iKC21ksu;
076900150112         V01ksc = iKC21ksc;
077000150112         V01cpo = iKC21cpo;
077100141105
077200141105         // -?Ordinamento di default: per Importanza Cliente?
077300141105         V01osf = 'I';
077400141104
077500141104       ENDSR;
077600141105
077700141105       //--------------------------------------------------------------
077800141105       //?Controllo dati in videata D01.                               ?
077900141105       //--------------------------------------------------------------
078000141105       BEGSR  sr_CtrD01;
078100141105
078200141105         // -?Spegnimento degli indicatori di errore?
078300141105         %subst(IndDspF : 50) = *off;
078400141105
078500141105         // -?Campagna Commerciale?
078600141106         clear  V1Dncm;
078700141111
078800141106         // ·?Ricerca?
078900141106         If  %scan( '?' : V01ncm ) > *zero;
079000141106           clear  V01ncm;
079100150113           clear  TRKC02ds;
079200150113           iKC02ric = 'R';
079300150113           trkc02r ( kpjba : TRKC02ds );
079400150113           if  oKC02ncm <> *zero;
079500150113             V01ncm = %editc( oKC02ncm : '3' );
079600141111           endif;
079700141112           PosCurNCM   = *on;
079800141112           errGenerico = *on;
079900150113           if  oKC02err = *on;
080000141114             ErrMessage = *on;
080100150113             V1Dmsg = oKC02msg;
080200141114             leavesr;
080300141114           endif;
080400141112           // ·?Meglio eseguire il controllo per verificare quale?
080500141112           //  ?cliente richiedere?
080600141112           //leavesr;
080700141106         EndIf;
080800141111
080900141106         // ·?Controllo?
081000141106         if  V01ncm = *blank;
081100141106           V01ncm = *zero;
081200141106         endif;
081300141111
081400141106         select;
081500141113           when  V01ncm = *blank  or  %trim( V01ncm ) = *zero;
081600141111             clear  V01ncm;
081700141106             errGenerico = *on;
081800141106             errMessage  = *on;
081900141106             PosCurNCM   = *on;
082000141106             V1Dmsg = sk_Msg(02);
082100141106             leavesr;
082200141113           when  %check( c_Digits : %trim( V01ncm ) ) > *zero;
082300141106             errGenerico = *on;
082400141106             errMessage  = *on;
082500141106             PosCurNCM   = *on;
082600141106             V1Dmsg = sk_Msg(02);
082700141106             leavesr;
082800141106         endsl;
082900141111
083000141113         If  k_CMPncm <> %int( %trim( V01ncm ) );
083100141112
083200141112           clear  keyTICMP01;
083300141113           k_CMPncm = %int( %trim( V01ncm ) );
083400141112           chain  %kds( keyTICMP01 )  TICMP000;
083500141112           if  Not %found( TICMP01L );
083600141112             errGenerico = *on;
083700141112             errMessage  = *on;
083800141112             PosCurNCM   = *on;
083900141112             V1Dmsg = sk_Msg(02);
084000141112             leavesr;
084100141112           endif;
084200141112           //V1Dncm = CMPdes;       //?(dopo, sotto, oltre la "If")?
084300141112
084400141112           // -?RE-impostazione del cliente in Campagna Commerciale?
084500141112           //  ?(da richiedere a video - D01 - e da visualizzare nel?
084600141112           //  ?subfile - S01)?
084700141112           clear  keyTICMC01;
084800141112           k_CMCncm = k_CMPncm;
084900141112           chain  %kds( keyTICMC01 : 1 )  TICMC000;
085000141112           if  %found(TICMC01L);      // ·?(l'esistenza è scontata!)?
085100141112             VisualKSU = ( CMCksu <> *zero );
085200141112             VisualKSC = ( CMCksc <> *zero );
085300141112             VisualCPO = ( CMCcpo <> *zero );
085400141112           endif;
085500141112
085600141112         EndIf;
085700141112
085800141112         V1Dncm   = CMPdes;
085900150112         oKC21ncm = %int( %trim( V01ncm ) );
086000141112
086100141112         // -?Eseguita ricerca su Campagna Commeriale?
086200141112         if  errGenerico = *on;
086300141112           leavesr;
086400141112         endif;
086500141105
086600141105         // -?Singolo cliente in Campagna Commerciale?
086700141105         If  ( VisualKSU  and  V01ksu <> *zero )  or
086800141105             ( VisualKSC  and  V01ksc <> *zero )  or
086900141105             ( VisualCPO  and  V01cpo <> *zero );
087000141105
087100141105           clear  keyTICMC01;
087200141113           k_CMCncm = %int( %trim( V01ncm ) );
087300141105           k_CMCksu = V01ksu;
087400141105           k_CMCksc = V01ksc;
087500141105           k_CMCcpo = V01cpo;
087600141105           setll  %kds( keyTICMC01 )  TICMC000;
087700141105           if  Not %equal( TICMC01L );
087800141105             errGenerico = *on;
087900141105             errMessage  = *on;
088000141105             select;
088100141105               when  VisualKSU  and  V01ksu <> *zero;
088200141105                 PosCurKSU = *on;
088300141105               when  VisualKSC  and  V01ksc <> *zero;
088400141105                 PosCurKSC = *on;
088500141105               when  VisualCPO  and  V01cpo <> *zero;
088600141105                 PosCurCPO = *on;
088700141105             endsl;
088800141105             V1Dmsg = sk_Msg(03);
088900141105             leavesr;
089000141105           endif;
089100141105
089200141105           // ·?SE richiesto singolo cliente: nessun altro controllo?
089300141105           leavesr;
089400141105
089500141105         EndIf;
089600141105
089700141105         // -?Ordinamento Elenco?
089800141105         if  V01osf <> 'R'  and  V01osf <> 'I';
089900141105           errGenerico = *on;
090000141105           errMessage  = *on;
090100141105           PosCurOSF   = *on;
090200141105           V1Dmsg = sk_Msg(04);
090300141105           leavesr;
090400141105         endif;
090500141105
090600141105         // -?Codice Importanza Cliente?
090700141201         IF  V01cic_ds <> *blanks;
090800141201           For  xx = 1  To  %elem( sk_V01cic );
090900141201
091000141201             IF  sk_V01cic( xx ) <> *blanks;
091100141201
091200141201               // ·?Ricerca?
091300141201               If  %scan( '?' : sk_V01cic( xx ) ) > *zero;
091400141201                 clear  sk_V01cic( xx );
091500141201                 idTabella = 'IC';
091600141201                 Tabel_Ricerca ( idTabella  : Ordinamento :
091700141201                                 idElemento : tastoUscita );
091800141201                 sk_V01cic( xx ) = idElemento;
091900141201                 %subst( IndDspF : 55 + xx : 1 ) = *on;
092000141201                 errGenerico = *on;
092100141201               EndIf;
092200141201
092300141201               // ·?Controllo?
092400141201               If  %lookup( sk_V01cic( xx ) : sk_IC ) = *zero;
092500141201                 %subst( IndDspF : 55 + xx : 1 ) = *on;
092600141201                 errGenerico = *on;
092700141201                 errMessage  = *on;
092800141201                 V1Dmsg = sk_Msg(05);
092900141201                 leavesr;
093000141201               EndIf;
093100141201
093200141201             ENDIF;
093300141201
093400141201           EndFor;
093500141201         ENDIF;
093600141105
093700141112         // -?Eseguita almeno una ricerca su Codice Importanza Cliente?
093800141105         if  errGenerico = *on;
093900141105           leavesr;
094000141105         endif;
094100141105
094200141105         // -?Filiale Appartenenza?
094300141105         If  V01flt > *zero;
094400141105           chain  ( V01flt )  AZORG;
094500141105           if  Not %found(AZORG01L)  or  ORGfva <> *blank;
094600141105             errGenerico = *on;
094700141105             errMessage  = *on;
094800141105             PosCurFLT   = *on;
094900141105             V1Dmsg = sk_Msg(06);
095000141105             leavesr;
095100141105           endif;
095200141105         EndIf;
095300141105
095400141105         // -?Codice Area?
095500141105         clear  V1Dcar;
095600141105         if  V01car = *zero;
095700141105           clear  V01car;
095800141105         endif;
095900141105         k_TBLcod = '05';
096000141105         k_TBLkey = V01car;
096100141105         Select;
096200141105
096300141105           // -?Ricerca?
096400141105           When  %scan( '?' : V01car ) > *zero;
096500141105             clear  V01car;
096600141105             //clear w030a;
096700141105             //x§taber( k_TBLkut : k_TBLcod : k_TBLkey : w030a);
096800141105             //if  k_TBLkey <> *blank;
096900141105             //  V01car = k_TBLkey;
097000141106             //  V1Dcar = %subst( w030a : 1 : %len(V1Dcar) );
097100141105             //endif;
097200141105             idTabella = k_TBLcod;
097300141105             Tabel_Ricerca ( idTabella  : Ordinamento :
097400141105                             idElemento : tastoUscita );
097500141105             V01car      = idElemento;
097600141105             PosCurCAR   = *on;
097700141105             errGenerico = *on;
097800141105             leavesr;
097900141105
098000141105           // ·?Controllo?
098100141105           When  V01car <> *blank;
098200141105             clear  ds05;
098300141105             chain  %kds( keyTABEL00 )  TABEL;
098400141105             if  Not %found(TABEL00F)  or  TBLflg <> *blank;
098500141105               errGenerico = *on;
098600141105               errMessage  = *on;
098700141105               PosCurCAR   = *on;
098800141105               V1Dmsg = sk_Msg(07);
098900141105               leavesr;
099000141105             endif;
099100141105             ds05   = TBLuni;
099200141106             V1Dcar = §05des;
099300141105
099400141105         EndSl;
099500141105
099600141107         // -?Commerciale Unificante?
099700141107         clear  V1Dcmu;
099800141107         if  V01cmu = *zero;
099900141107           clear  V01cmu;
100000141107         endif;
100100141105         Select ;
100200141105
100300141105           // ·?Non inserito?
100400141105           When  V01cmu = *blank  or  V01cmu = *zero;
100500141105
100600141105           // ·?Ricerca?
100700141105           When  %scan( '?' : V01cmu ) > *zero;
100800141107             clear  V01cmu;
100900141105             clear  TRMK43ds;
101000141105             iMK43solU = 'S';
101100141105             iMK43fil  = DUTpou;
101200141105             TRMK43R ( kpjba : TRMK43ds );
101300141105             if  oMK43err = *off  and  oMK43fxx = *blank;
101400141105               V01cmu  = %editc( oMK43cmm : 'X' );
101500141106               V1Dcmu  = oMK43des;
101600141105             endif;
101700141105             ErrGenerico = *on;
101800141105             PosCurCMU   = *on;
101900141114             if  oMK43err = *on;
102000141114               ErrMessage = *on;
102100141114               V1Dmsg = oMK43msg;
102200141114             endif;
102300141105             leavesr;
102400141105
102500141105           // ·?Controllo numericità?
102600141105           When  %check( c_Digits : V01cmu ) > *zero;
102700141105             ErrMessage  = *on;
102800141105             ErrGenerico = *on;
102900141105             PosCurCMU   = *on;
103000141105             V1Dmsg = sk_Msg(08);
103100141105             leavesr;
103200141105
103300141105           // ·?Controllo validità / particolarità di attività?
103400141105           Other;
103500141105             k_CMMcod = %int( V01cmu );
103600141105             chain  %kds(keyAZCMM01)  AZCMM000;
103700141105             if  NOT %found(AZCMM01L)  or
103800141105                 CMMpar <> *blank;       // ·?Verifica SE codice attivo?
103900141105               errGenerico = *on;
104000141105               errMessage  = *on;
104100141105               PosCurCMU   = *on;
104200141105               V1Dmsg = sk_Msg(08);
104300141105               leavesr;
104400141105             endif;
104500141106             V1Dcmu = CMMdes;
104600141105
104700141105         EndSl;
104800141105
104900141105         IF  V01cmu > *zero;
105000141105
105100141105           // -?Abilitazione utente al commerciale?
105200141105           reset  TNTAA1ds;
105300141105           ITAA1caut = c_Aut_TC;
105400141105           ITAA1cmm  = %int( V01cmu );
105500141105           kpjbu     = TNTAA1ds;
105600141105           TNTAA1C ( kpjba );
105700141105           TNTAA1ds = kpjbu;
105800141105           if  oTAA1fabi = 'N';
105900141105             errGenerico = *on;
106000141105             errMessage  = *on;
106100141105             PosCurCMU   = *on;
106200141105             V1Dmsg = sk_Msg(08);
106300141105             leavesr;
106400141105           endif;
106500141105
106600141105           // -?Selezionabili SOLO commerciali unificanti?
106700141105           if  %int( V01cmu ) <> CMMuni;
106800141105             errGenerico = *on;
106900141105             errMessage  = *on;
107000141105             PosCurCMU   = *on;
107100141105             V1Dmsg = sk_Msg(09);
107200141105             leavesr;
107300141105           endif;
107400141105
107500141105         ENDIF;
107600141105
107700141105       ENDSR;
107800141104
107900141104       //--------------------------------------------------------------
108000141105       //?Gestione subfile S01.                                        ?
108100141104       //--------------------------------------------------------------
108200141105       BEGSR  sr_GesS01;
108300141104
108400141105         // -?Inizializzazione videata?
108500141104         if  $InzS01 = *on;
108600141105           exsr  sr_InzS01;
108700141105           $InzS01  = *off;
108800141104         endif;
108900141104
109000141105         // -?Emissione Testata & Piede con tasti funzionali abilitati?
109100150112         write  KC21T01;
109200150112         write  KC21P01;
109300141105
109400141105         // -?Posizionamento cursore?
109500141105         //  ?C1CsrRrn contiene il numero di riga del SubFile su cui?
109600141105         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
109700141105         //  ?si visualizza la pagina che vedeva l'utente quando ha?
109800141105         //  ?premuto l'ultimo tasto.?
109900141105         if  C1CsrRrn > *zero;
110000141105           C1RcdNbr = C1CsrRrn;
110100141105         else;
110200141105           //C1RcdNbr = 1;
110300150112           //write  KC21S00;          //?(no rec. - SOLO visualizzazione)?
110400141105           // -?Se non è stato caricato nulla: si riemette la videata D01?
110500141201           //  ?(o, SE richiamato: esce dal *pgm)?
110600141201           If  %parms() = 2;
110700141201             $Video = 'D1';
110800141201             errGenerico = *on;
110900141201             errMessage  = *on;
111000141201             V1Dmsg = sk_Msg(11);
111100141201           Else;
111200150112             oKC21err = *on;
111300150112             oKC21msg = sk_Msg(11);
111400141201             $Fine    = *on;
111500141201           EndIf;
111600141105           // -?Chiusura del cursore?
111700141105           exsr  sr_CloseCursor;
111800141105           leavesr;
111900141105         endif;
112000141104
112100141105         // -?Emissione videata?
112200150112         exfmt  KC21C01;
112300141105
112400141105         clear  V1Dmsg;
112500141105         reset  errMessage;
112600141105         reset  errGenerico;
112700141104
112800141104         SELECT;
112900141104
113000141105           // -?F3=Fine?
113100141104           WHEN  dsp_aid = c_F03;
113200141105             exsr  sr_CloseCursor;
113300150112             oKC21fxx = '03';
113400141105             $Fine    = *on;
113500141104
113600141105           // -?F12=Ritorno?
113700141104           WHEN  dsp_aid = c_F12;
113800141105             exsr  sr_CloseCursor;
113900141201             if  %parms() = 2;
114000141201               $Video = 'D1';
114100141201             else;
114200150112               oKC21fxx = '12';
114300141201               $Fine    = *on;
114400141201             endif;
114500141104
114600141105           // -?Roll-Up?
114700141104           WHEN  dsp_aid = c_RollUp;
114800141105             // -?Chiusura del cursore?
114900141105             // -?Verifica raggiungimento del limite di records registrabili?
115000141105             //  ?nel subfile (9999)?
115100141104             if  S01nrr = *hival;
115200141104               errGenerico = *on;
115300141105               errMessage  = *on;
115400141107               V1Dmsg = sk_Msg(12);
115500141105               leavesr;
115600141105             endif;
115700141105             exsr  sr_RollUpS01;
115800141105
115900141105           // -?SubFile vuoto?
116000141105           When  S01nrr = *zero;
116100141104
116200141105           // -?Invio?
116300141104           OTHER;
116400141105             // -?Gestione opzioni?
116500141105             exsr  sr_OpzS01;
116600141104             if  errGenerico = *on;
116700141104               leavesr;
116800141104             endif;
116900141104
117000141104         ENDSL;
117100141104
117200141104       ENDSR;
117300141104
117400141104       //--------------------------------------------------------------
117500141105       //?Inizializzazione subfile S01.                                ?
117600141104       //--------------------------------------------------------------
117700141105       BEGSR  sr_InzS01;
117800141106
117900141106         // -?Impostazione dati nel SubFile-Control?
118000141113         C01ncm = %int( %trim( V01ncm ) );
118100141104
118200141105         // -?Pulizia subfile?
118300141104         SflDsp_N    = *on;
118400141104         SflDspCtl_N = *on;
118500150112         write  KC21C01;
118600141104         SflDspCtl_N = *off;
118700141104         SflEnd      = *off;
118800141105
118900141105         SflNxtChg   = *off;
119000141104
119100141106         clear  w_SflPag;
119200141105         clear  C1RcdNbr;
119300141105         clear  C1CsrRrn;
119400141105         clear  S01nrr;
119500141105         clear  V1Dmsg;
119600141104         errMessage  = *off;
119700141104         errGenerico = *off;
119800141105
119900141105         // -?Spegnimento degli indicatori di errore?
120000141105         %subst(IndDspF : 50) = *off;
120100141105
120200141105         // -?Impostazione del tipo di ordinamento del SubFile?
120300141105         clear  wOrd;
120400141106         select;
120500141105           // ·?0 = Singolo codice cliente?
120600141105           when  ( VisualKSU  and  V01ksu <> *zero )  or
120700141105                 ( VisualKSC  and  V01ksc <> *zero )  or
120800141105                 ( VisualCPO  and  V01cpo <> *zero );
120900141105           // ·?1 = per Ragione Sociale?
121000141105           when  V01osf = 'R';
121100141105             wOrd = 1;
121200141105           // ·?2 = per Importanza Cliente?
121300141105           when  V01osf = 'I';
121400141105             wOrd = 2;
121500141105         endsl;
121600141104
121700141105         // -?Ciclo di caricamento dati nel subfile (singola pagina)?
121800141104         $EoF = *off;
121900141105
122000141105         // -?Richiesta selezione per singolo codice Cliente?
122100141105         If  wOrd = *zero;
122200141105           clear  keyTICMC01;
122300141106           k_CMCncm = C01ncm;
122400141105           k_CMCksu = V01ksu;
122500141105           k_CMCksc = V01ksc;
122600141105           k_CMCcpo = V01cpo;
122700141105           setll  %kds( keyTICMC01 )  TICMC000;
122800141105         // -?Richiesto ordinamento per Ragione Sociale?
122900141105         //                      ?o per Importanza Cliente?
123000141105         Else;
123100141106           // -?Preparazione SQL da eseguire?
123200141105           exsr  sr_PrepSQL;
123300141106           // -?Apertura del cursore?
123400141106           exsr  sr_OpenCursor;
123500141105         EndIf;
123600141104
123700141106         // -?Lettura del file?
123800141105         exsr  sr_ReadRec;
123900141104
124000141105         DoW  Not $EoF  and  S01nrr < c_SflPag;
124100141105           exsr  sr_RollUpS01;
124200141105         EndDo;
124300141105
124400141105
124500141105         // -?Visualizzazione del SFL (se ci sono dati)?
124600141105         SflDsp_N  = (S01nrr <= *zero);
124700141105
124800141105         // -?Attivazione del SFLEND?
124900141105         SflEnd = ( $EoF = *on );
125000141105
125100141105         // -?Posizionamento subfile sul suo 1° record?
125200141105         if  S01nrr > *zero;
125300141105           C1RcdNbr = 1;
125400141105           C1CsrRrn = C1RcdNbr;
125500141105         else;
125600141105           clear  C1RcdNbr;
125700141105         endif;
125800141104
125900141104       ENDSR;
126000141104
126100141104       //--------------------------------------------------------------
126200141106       //?Caricamento singola pagine SubFile S01.                      ?
126300141104       //--------------------------------------------------------------
126400141106       BEGSR  sr_RollUpS01;
126500141104
126600141106         S01nrr    = (w_SflPag * c_SflPag);
126700141106         w_SflPag += 1;
126800141104         SflNxtChg = *off;
126900141104
127000141106         // -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
127100141106         DoW  $EoF   = *off                   and
127200141106              S01nrr < (w_SflPag * c_SflPag)  and
127300141104              S01nrr < *hival;
127400141104
127500141106           // -?Caricamento dati nel record del subfile?
127600141106           if  $RecOK;
127700141106             exsr  sr_RieRecS01;
127800141106             S01nrr += 1;
127900150112             write KC21S01;
128000141106           endif;
128100141104
128200141106           // -?Lettura record successivo nell'archivio?
128300141104           exsr sr_ReadRec;
128400141104
128500141106         EndDo;
128600141104
128700141106         // -?Visualizzazione del SFL (se ci sono dati)?
128800141106         SflDsp_N = (S01nrr <= *zero);
128900141104
129000141106         // -?Attivazione (eventuale) del SFLEND?
129100141104         SflEnd   = ($EoF = *on);
129200141104
129300141106         // -?Posizionamento cursore al 1° rcd della pagina?
129400141104         if  S01nrr > *zero;
129500141106           wPag     = S01nrr / c_SflPag;
129600141106           wRig     = S01nrr - (c_SflPag * wPag);
129700141106           C1RcdNbr = wPag * c_SflPag;
129800141106           if  wRig > *zero;
129900141106             C1RcdNbr = C1RcdNbr + 1;
130000141104           else;
130100141106             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
130200141104           endif;
130300141104         else;
130400141106           clear  C1RcdNbr;
130500141104         endif;
130600141104
130700141106         C1CsrRrn = C1RcdNbr;
130800141104
130900141104       ENDSR;
131000141104
131100141104       //--------------------------------------------------------------
131200141106       //?Caricamento singolo record nel SubFile S01.                  ?
131300141104       //--------------------------------------------------------------
131400141106       BEGSR  sr_RieRecS01;
131500141104
131600150112         clear  KC21S01;
131700141110
131800141110         // -?Spegnimento degli indicatori di errore?
131900141110         %subst(IndDspF : 50) = *off;
132000141104
132100141113         // -?Campi Hidden?
132200141113
132300141113         // -?Codice Cliente in elaborazione?
132400141107         if  wOrd = *zero;
132500141107           SH1ksu = CMCksu;
132600141107           SH1ksc = CMCksc;
132700141107           SH1cpo = CMCcpo;
132800141107         else;
132900141107           SH1ksu = TICMC_ds.CMCksu;
133000141107           SH1ksc = TICMC_ds.CMCksc;
133100141107           SH1cpo = TICMC_ds.CMCcpo;
133200141107         endif;
133300141107
133400141107         // -?Decodifica Cliente?
133500141127         //  ?(GIÀ eseguita nella subr. sr_SelezRec)?
133600141112         Select;
133700141113
133800141113           // ·?Se c'è un ordinamento: questi dati sono già stati?
133900141113           //  ?reperiti via SQL?
134000141113           When  wOrd <> *zero;
134100141118             wACOrag  = wSQL_ds.SQLrsc;
134200141112
134300141112           // ·?Campagna Commerciale per Potenziali?
134400141112           When  SH1cpo <> *zero;
134500141127             //clear  keyTNCPO01;
134600141127             //k_CPOcpo = SH1cpo;
134700141127             //chain  %kds( keyTNCPO01 )  TNCPO000;
134800141118             wACOrag  = CPOrag;
134900141112
135000141112           // ·?Campagna Commerciale per Clienti?
135100141112           When  SH1ksc <> *zero;
135200141127             //clear  keyCNACO00;
135300141127             //k_ACOkut = c_KUT;
135400141127             //k_ACOkcc = DUTkci;
135500141127             //k_ACOksc = SH1ksc;
135600141127             //chain  %kds( keyCNACO00 )  CNACO000;
135700141127             //chain  %kds( keyCNACO00 )  CNCLP000;
135800141118             wACOrag  = ACOrag;
135900141112
136000141112           // -?Campagna Commerciale per Unificanti?
136100141112           When  SH1ksu <> *zero;
136200141127             //clear  keyCNACO00;
136300141127             //k_ACOkut = c_KUT;
136400141127             //k_ACOkcc = DUTkci;
136500141127             //k_ACOksc = SH1ksu;
136600141127             //chain  %kds( keyCNACO00 )  CNACO000;
136700141127             //chain  %kds( keyCNACO00 )  CNCLP000;
136800141118             wACOrag  = ACOrag;
136900141112
137000141112         EndSl;
137100141120
137200141120         if  wOrd = *zero;
137300141120           SH1cpo2 = ACOlib;
137400141120           SH1ksc2 = ACOksc;
137500141120         else;
137600141120           SH1cpo2 = wSQL_ds.SQLlib;
137700141120           SH1ksc2 = wSQL_ds.SQLcli;
137800141120         endif;
137900141106
138000141113         // -?Campi Output?
138100141107
138200141113         // -?Codice Cliente?
138300141106         select;
138400141201           //when  SH1cpo > *zero; ?(per i potenziali serve un altro subfile)?
138500141113           //  S01cli = SH1cpo;
138600141107           when  SH1ksc > *zero;
138700141113             S01cli = SH1ksc;
138800141107           when  SH1ksu > *zero;
138900141113             S01cli = SH1ksu;
139000141106         endsl;
139100141107
139200141107         if  wOrd = *zero;
139300141113           // -?Ragione Sociale?
139400141118           S01rag  = %subst( wACOrag : 1 : %len( S01rag ) );
139500141113           // -?Importanza Cliente?
139600141107           if  SH1cpo <> *zero;
139700141107             S01cic = CPOclv;
139800141107           else;
139900141107             S01cic = CLPclv;
140000141107           endif;
140100141107         else;
140200141113           // -?Ragione Sociale?
140300141107           S01rag  = %subst( wSQL_ds.SQLrsc : 1 : %len( S01rag ) );
140400141113           // -?Importanza Cliente?
140500141107           S01cic = wSQL_ds.SQLclv;
140600141107         endif;
140700141113
140800141114         // -?% Obiettivo:?
140900141114         clear  keyTICMF01;
141000141114         k_CMFncm = C01ncm;
141100141114         k_CMFksu = SH1ksu;
141200141114         k_CMFksc = SH1ksc;
141300141114         k_CMFcpo = SH1cpo;
141400141118         // ·?Finale (Sede) - fase 30?
141500141114         k_CMFacm = ' 30';
141600141114         chain  %kds( keyTICMF01 : 5 )  TICMF000;
141700141126         if  Not %found( TICMF01L );
141800141126           // ·?Iniziale (Sede) - fase 10?
141900141126           k_CMFacm = ' 10';
142000141126           chain  %kds( keyTICMF01 : 5 )  TICMF000;
142100141126         endif;
142200141114         if  %found( TICMF01L );
142300141118           S01pea = CMFpea;
142400141114         endif;
142500141118
142600141124         // -?Trattative Campagna avviate?
142700141124         exsr  sr_TrattativeCampagna;
142800141118
142900141118         // -?Data Ultimo Appuntamento?
143000141120         exsr  sr_DataUltimoAppuntamento;
143100141118
143200141124         // -?Prossima Attività?
143300141127         ////  ?(se i "Clienti con Attività in Corso" sono da SCARTARE:?
143400141127         ////  ? qui si arriva SOLO SE il cliente NON ha attività)?
143500150127         //if  V01eat = *blank;
143600141127         //  exsr  sr_ProssimaAttivita;
143700141127         //endif;
143800141127         //  ?L'attività è già stata reperita nella subr. "sr_SelezRec".?
143900141127         S01cad = wATCcad;
144000141124
144100141124         // -?Trattative in Corso?
144200141125         exsr  sr_TrattativeInCorso;
144300141113
144400141113         // -?Attributi di visualizzazione?
144500141113
144600141117         exsr  sr_SetAttrS01;
144700141104
144800141104       ENDSR;
144900141104
145000141104       //--------------------------------------------------------------
145100141106       //?Gestione opzioni nel SubFile S01.                            ?
145200141104       //--------------------------------------------------------------
145300141106       BEGSR  sr_OpzS01;
145400141104
145500141110         clear  wCountOpz1;
145600141110
145700141106         // -?Ciclo di lettura subfile?
145800150112         readc  KC21S01;
145900141104
146000150112         DOW  NOT  %eof(TRKC21D);
146100141104
146200141106           %subst(IndDspF : 50) = *off;
146300141104           SflNxtChg = *off;
146400141106           C1RcdNbr  = S01nrr;
146500141104
146600141106           Select;
146700141104
146800141106             // -?Nessuna opzione?
146900141106             When  S01opz = *blank;
147000141104
147100141110             // -?1 = Scelta?
147200141110             When  S01opz = '1';
147300141113               // -?Gestione Attività o Trattative?
147400150112               If  iKC21ric = 'A'  or  iKC21ric = 'T';
147500141113                 if  SH1ksu <> *zero  or  SH1ksc <> *zero;
147600141113                   exsr  sr_Call_TNTAI2R;
147700141113                 endif;
147800141113               // -?Selezione (singola)?
147900141113               Else;
148000141113                 wCountOpz1 += 1;
148100141113                 If  wCountOpz1 = 1;
148200150112                   oKC21ncm = C01ncm;
148300150112                   oKC21ksu = SH1ksu;
148400150112                   oKC21ksc = SH1ksc;
148500150112                   oKC21cpo = SH1cpo;
148600141113                   // ·?Si prosegue con il controllo di eventuali altre?
148700141113                   //  ?selezioni (da segnalare come errate)?
148800141113                   //$Fine = *on;
148900141113                   //leavesr;
149000141113                 Else;
149100141113                   ErrGenerico = *on;
149200141113                   ErrMessage  = *on;
149300141113                   PosCurOpz   = *on;
149400141113                   select;
149500150112                     when  oKC21cpo <> *zero;
149600141113                       V1Dmsg = %trimR( sk_Msg(14) ) + ' ' +
149700150112                                %editc( oKC21cpo : 'X' );
149800150112                     when  oKC21ksc <> *zero;
149900141113                       V1Dmsg = %trimR( sk_Msg(14) ) + ' ' +
150000150112                                %editc( oKC21ksc : 'X' );
150100150112                     when  oKC21ksu <> *zero;
150200141113                       V1Dmsg = %trimR( sk_Msg(14) ) + ' ' +
150300150112                                %editc( oKC21ksu : 'X' );
150400141113                   endsl;
150500150112                   //clear  oKC21ncm;
150600150112                   clear  oKC21ksu;
150700150112                   clear  oKC21ksc;
150800150112                   clear  oKC21cpo;
150900141113                 EndIf;
151000141113               EndIf;
151100141104
151200141106             // -?? = Opzione NON valida?
151300141106             Other;
151400141104               ErrGenerico = *on;
151500141106               ErrMessage  = *on;
151600141104               PosCurOpz   = *on;
151700141107               V1Dmsg = sk_Msg(13);
151800141104
151900141106           EndSl;
152000141104
152100141106           // -?Aggiornamento subfile?
152200141106           Select;
152300141106             When  ErrMessage;
152400141104               SflNxtChg = *on;
152500141106               C1CsrRrn  = C1RcdNbr;
152600141106             When  ErrGenerico;
152700141106               SflNxtChg = *on;
152800141106               C1CsrRrn  = C1RcdNbr;
152900141110               clear  S01opz;
153000141110             When  S01opz = '1';
153100141110               SflNxtChg = *on;
153200141110               C1CsrRrn  = C1RcdNbr;
153300141106             Other;
153400141106               C1CsrRrn  = C1RcdNbr;
153500141106               clear  S01opz;
153600141106           EndSl;
153700141104
153800141117           exsr  sr_SetAttrS01;
153900141117
154000150112           update KC21S01;
154100141104
154200141106           // -?Uscita dal ciclo di lettura?
154300141110           if  ErrMessage  or  ErrGenerico;
154400141104             leave;
154500141106           endif;
154600141104
154700141106           // -?Lettura rec. variato sucessivo?
154800150112           readc KC21S01;
154900141104
155000141106         EndDo;
155100141110
155200141113         // -?Richiesta Selezione  e  Scelto un cliente?
155300141113         //  ?(e nessun errore) => Uscita?
155400150112         if  iKC21ric = *blank  and  wCountOpz1 = 1  and
155500141113              Not ErrMessage    and  Not ErrGenerico;
155600141113           $Fine = *on;
155700141110         endif;
155800141104
155900141104       ENDSR;
156000141117
156100141117       //--------------------------------------------------------------
156200141117       //?Impostaz. attributi di visualizzazione in singolo rec. S01.  ?
156300141117       //--------------------------------------------------------------
156400141117       BEGSR  sr_SetAttrS01;
156500141117
156600141117         // -?Per evidenziare l'ordinamento?
156700141117         SP1da0 = c_DspAtr_Norm;
156800141117         SP1da1 = c_DspAtr_Norm;
156900141117         SP1da2 = c_DspAtr_Norm;
157000141117         select;
157100141117           when  wOrd = *zero;
157200141117             SP1da0 = c_DspAtr_HI;
157300141117           when  wOrd = 1;
157400141117             SP1da1 = c_DspAtr_HI;
157500141117           when  wOrd = 2;
157600141117             SP1da2 = c_DspAtr_HI;
157700141117         endsl;
157800141117
157900141117         // -?Per evidenziare SE il cliente è annullato in anagrafica?
158000141117         //  ?(SE specificato un codice cliente, altrimenti i clienti?
158100141117         //  ? annullati vengono già scartati via SQL)?
158200141117         if  wOrd = *zero  and  ACOflg <> *blank;
158300141117           SP1da1 = c_dspatr_BL;
158400141117         endif;
158500141117
158600141117       ENDSR;
158700141111
158800141111       //--------------------------------------------------------------
158900141111       //?Richiamo *pgm Interrogazione Cliente.                        ?
159000141111       //--------------------------------------------------------------
159100141111       BEGSR  sr_Call_TNTAI2R;
159200141111
159300141111         clear  TNTAI2ds;
159400150112         iTAI2ric  = iKC21ric;
159500141111         if  SH1ksc <> *zero;
159600141111           iTAI2ksc  = SH1ksc;
159700141111         else;
159800141111           iTAI2ksc  = SH1ksu;
159900141111         endif;
160000141111         iTAI2comD = V1Dncm;
160100141111
160200141111         tntaI2r ( kpjba : TNTAI2ds );
160300141111
160400141111         Select;
160500141114           // -?Rilevato errore?
160600141111           When  oTAI2err = *on;
160700141111             ErrGenerico = *on;
160800141111             ErrMessage  = *on;
160900141111             V1Dmsg = oTAI2msg;
161000141114           // -?Uscita con F3?
161100141111           When  oTAI2f03 = *on;
161200141113             clear  S01opz;
161300141114             ErrGenerico  = *on;
161400141114             //$Fine = *on;
161500141114           // -?Uscita con F12?
161600141111           When  oTAI2f12 = *on;
161700141113             clear  S01opz;
161800141114           // -?Uscita "regolare"?
161900141111           Other;
162000141113             clear  S01opz;
162100141111         EndSl;
162200141120
162300141120
162400141120         If  NOT ErrMessage;
162500141120
162600141124           // -?Aggiornamento "Trattative Campagna avviate" nel subfile?
162700141124           exsr  sr_TrattativeCampagna;
162800141125           if  ( V01tcm = 'N'  and  S01tcm = 'S' )  or
162900141125               ( V01tcm = 'S'  and  S01tcm = *blank );
163000141125             $InzS01 = *on;
163100141125             // -?Chiusura del cursore?
163200141125             exsr  sr_CloseCursor;
163300141125           endif;
163400141120
163500141124           // -?Aggiornamento "Data Ultimo Appuntamento" nel subfile?
163600141120           exsr  sr_DataUltimoAppuntamento;
163700141120
163800141124           // -?Aggiornamento "Prossima Attività da fare" nel subfile?
163900141124           exsr  sr_ProssimaAttivita;
164000150127           if  ( V01eat = 'N'  and  wATCcad <> *blank )  or
164100150127               ( V01eat = 'S'  and  wATCcad =  *blank );
164200141125             $InzS01 = *on;
164300141125             // -?Chiusura del cursore?
164400141125             exsr  sr_CloseCursor;
164500141125           endif;
164600141124
164700141124           // -?Aggiornamento "Trattative in Corso" nel subfile?
164800141124           exsr  sr_TrattativeInCorso;
164900141120
165000141120         EndIf;
165100141111
165200141111       ENDSR;
165300141201
165400141201       //--------------------------------------------------------------
165500141201       //?Preparazione stringa SQL per l'estrazione dei dati.          ?
165600141201       //--------------------------------------------------------------
165700141201       BEGSR  sr_PrepSQL;
165800141201
165900141201         SELECT;
166000141201
166100141201           // -?Richiesto ordinamento per Ragione Sociale?
166200141201           WHEN  wOrd = 1;
166300141201
166400141201             // ·?Richiesti Clienti (Unificanti o Figli)?
166500141201             If  VisualKSU  or  VisualKSC;
166600141201
166700141201               wSql = 'with +
166800141201                         F1 as +
166900141201                           (select ACOksc, ACOlib, 0, ACOrag, ACOabl, +
167000141201                                   ''0'', CLPclv, CLPage +
167100141201                              from CNACO00F left outer join CNCLP00F +
167200141201                                on ACOkut = CLPkut and +
167300141201                                   ACOkcc = CLPkcc and +
167400141201                                   ACOksc = CLPksc +
167500141201                                            left outer join CNIND00F +
167600141201                                on ACOkut = INDkut and +
167700141201                                   ACOkcc = INDkcc and +
167800141201                                   ACOksc = INDksc +
167900141201                             where ACOflg = '' '' +
168000141201                               and ACOkut = ' + %char( c_KUT ) +
168100141201                             ' and ACOkcc = ' +
168200141201                                   %trimL( %editc( DUTkci : '3' ) );
168300141201               exsr  sr_AddSelezAnagrCli;
168400141201               wSql += ')';
168500141201
168600141201             // ·?Richiesti Potenziali?
168700141201             Else;
168800141201
168900141201               wSql = 'with +
169000141201                         F1 as +
169100141201                           (select 0, CPOcpo, CPOcpo, CPOrag, '' '', +
169200141201                                   ''0'', CPOclv, CPOcmm +
169300141201                              from TNCPO00F +
169400141201                             where CPOatb = '' '')';
169500141201
169600141201             EndIf;
169700141201
169800141201             wSQL += ' select TICMC00F.*, F1.* +
169900141201                         from TICMC00F inner join F1 +
170000141201                           on';
170100141201
170200141201           // -?Richiesto ordinamento per Importanza Cliente?
170300141201           WHEN  wOrd = 2;
170400141201
170500141201             If  VisualKSU  or  VisualKSC;
170600141201
170700141201               wSql = 'with +
170800141201                         Tab_IC as +
170900141201                           (select TBLkey, substr(TBLuni, 31, 1) as wICord +
171000141201                              from TABEL00F +
171100141201                             where TBLflg = '' '' +
171200141201                               and TBLkut = ' + %char( c_KUT ) +
171300141201                             ' and TBLcod = ''IC''), +
171400141201                         F2 as +
171500141201                           (select ACOksc, ACOlib, 0, ACOrag, ACOabl, +
171600141201                                   case CLPclv when '' '' then ''0'' +
171700141201                                               else wICord end as wICord, +
171800141201                                   CLPclv, CLPage +
171900141201                              from CNACO00F left outer join CNCLP00F +
172000141201                                on ACOkut = CLPkut and +
172100141201                                   ACOkcc = CLPkcc and +
172200141201                                   ACOksc = CLPksc +
172300141201                                            left outer join CNIND00F +
172400141201                                on ACOkut = INDkut and +
172500141201                                   ACOkcc = INDkcc and +
172600141201                                   ACOksc = INDksc +
172700141201                                            left outer join Tab_IC +
172800141201                                on CLPclv = TBLkey +
172900141201                             where ACOflg = '' '' +
173000141201                               and ACOkut = ' + %char( c_KUT ) +
173100141201                             ' and ACOkcc = ' +
173200141201                                   %trimL( %editc( DUTkci : '3' ) );
173300141201               exsr  sr_AddSelezAnagrCli;
173400141201               wSql += ')';
173500141201
173600141201             Else;
173700141201
173800141201               wSql = 'with +
173900141201                         Tab_IC as +
174000141201                           (select TBLkey, substr(TBLuni, 31, 1) as wICord +
174100141201                              from TABEL00F +
174200141201                             where TBLflg = '' '' +
174300141201                               and TBLkut = ' + %char( c_KUT ) +
174400141201                             ' and TBLcod = ''IC''), +
174500141201                         F2 as +
174600141201                           (select 0, CPOcpo, CPOcpo, CPOrag, '' '', +
174700141201                                   case CPOclv when '' '' then ''0'' +
174800141201                                               else wICord and as wICord, +
174900141201                                   CPOclv, CPOcmm +
175000141201                              from TNCPO00F left outer join Tab_IC +
175100141201                                on CPOclv = TBLkey +
175200141201                             where CPOatb = '' '')';
175300141201
175400141201             EndIf;
175500141201
175600141201             wSQL += ' select TICMC00F.*, F2.* +
175700141201                         from TICMC00F inner join F2 +
175800141201                           on';
175900141201
176000141201         ENDSL;
176100141201
176200141201         select;
176300141201           when  VisualKSU;
176400141201             wSql += ' CMCksu = ACOksc';
176500141201           when  VisualKSC;
176600141201             wSql += ' CMCksc = ACOksc';
176700141201           when  VisualCPO;
176800141201             wSql += ' CMCcpo = CPOcpo';
176900141201         endsl;
177000141201
177100141201
177200141201         // -?Aggiunta SELEZIONI inserite a video:?
177300141201
177400141201         // ·?Campagna Commerciale?
177500141201         wSQL += ' where CMCncm = ' + %trim( V01ncm );
177600141201
177700141201
177800141201         // -?ORDINAMENTO?
177900141201         Select;
178000141201
178100141201           // ·?per Ragione Sociale?
178200141201           When  wOrd = 1;
178300141201             select;
178400141201               when  VisualKSU;
178500141201                 wSql += ' order by ACOrag, CMCksu';
178600141201               when  VisualKSC;
178700141201                 wSql += ' order by ACOrag, CMCksc';
178800141201               when  VisualCPO;
178900141201                 wSql += ' order by CPOrag, CMCcpo';
179000141201             endsl;
179100141201
179200141201           // ·?per Importanza Cliente?
179300141201           When  wOrd = 2;
179400141201             select;
179500141201               when  VisualKSU;
179600141201                 wSql += ' order by wICord desc, CMCksu';
179700141201               when  VisualKSC;
179800141201                 wSql += ' order by wICord desc, CMCksc';
179900141201               when  VisualCPO;
180000141201                 wSql += ' order by wICord desc, CMCcpo';
180100141201             endsl;
180200141201         EndSl;
180300141201
180400141201         wSQL += ' for fetch only';
180500141201
180600141201       ENDSR;
180700141201
180800141201       //--------------------------------------------------------------
180900141201       //?Aggiunta delle selezioni sui dati anagrafici del cliente.    ?
181000141201       //--------------------------------------------------------------
181100141201       BEGSR  sr_AddSelezAnagrCli;
181200141201
181300141201         // ·?Ragione Sociale?
181400141201         If  V01rag <> *blank;
181500141201           xx = %len( %trimR( V01rag ) );
181600141201           wSQL += ' and ACOrag like ''' + %trimR( V01rag ) + '%''';
181700141201         Endif;
181800141201
181900141201         // ·?Codice Importanza?
182000141201         If  V01cic_ds <> *blank;
182100141201
182200141201           wSQL += ' and CLPclv <> '' '' and CLPclv in (';
182300141201           clear  yy;
182400141201
182500141201           For  xx = 1  By 1  To %elem( sk_V01cic );
182600141201
182700141201             if  sk_V01cic(xx) <> *blank;
182800141201               if  yy > *zero;
182900141201                 wSQL += ', ';
183000141201               endif;
183100141201               yy += 1;
183200141201               wSQL += '''' + sk_V01cic(xx) + '''';
183300141201             endif;
183400141201
183500141201           EndFor;
183600141201
183700141201           wSQL += ')';
183800141201
183900141201         EndIf;
184000141201
184100141201         // ·?Filiale di Appartenenza?
184200141201         If  V01flt <> *zero;
184300141201           wSQL += ' and decimal(substr(digits(ACOksc), 1, 3)) = ' +
184400141201                   %trimL( %editc( V01flt : '3' ) );
184500141201         EndIf;
184600141201
184700141201         // ·?Area di Appartenenza?
184800141201         If  V01car <> *blank;
184900141201
185000141201           wSQL += ' and decimal(substr(digits(ACOksc), 1, 3)) in(';
185100141201           clear  yy;
185200141201
185300141201           For  xx = 1  By 1  To %elem( sk_Car );
185400141201
185500141201             if  sk_Car(xx) = *hival;
185600141201               leave;
185700141201             endif;
185800141201
185900141201             if  %editc( sk_Car(xx) : 'X' ) = V01car;
186000141201               if  yy > *zero;
186100141201                 wSQL += ', ';
186200141201               endif;
186300141201               yy += 1;
186400141201               wSQL += %editc( sk_Fil(xx) : 'X' );
186500141201             endif;
186600141201
186700141201           EndFor;
186800141201
186900141201           // ·?Nessuna filiale caricata?
187000141201           //  ?(se ne forza una "000" - inesistente - per non mandare?
187100141201           //  ? l'SQL in errore)?
187200141201           if  yy = *zero;
187300141201             wSQL += '000';
187400141201           endif;
187500141201
187600141201           wSQL += ')';
187700141201
187800141201         EndIf;
187900141201
188000141201         // ·?Commerciale Unificante?
188100141201         If  V01cmu <> *blank  and  V01cmu <> *zero;
188200141201
188300141201            wSQL += ' and CLPage in(';
188400141201            clear  yy;
188500141201
188600141201            For  xx = 1  By 1  To %elem( sk_CmmU );
188700141201
188800141201              if  %editc( sk_CmmU(xx) : 'X' ) = V01cmu;
188900141201                if  yy > *zero;
189000141201                  wSQL += ', ';
189100141201                endif;
189200141201                yy += 1;
189300141201                wSQL += %trimL( %editc( sk_Cmm(xx) : '3' ) );
189400141201              endif;
189500141201
189600141201            EndFor;
189700141201
189800141201           // ·?Nessun commerciale caricato?
189900141201           //  ?(si imposta l'unificante immesso a video per non?
190000141201           //  ? mandare l'SQL in errore)?
190100141201           if  yy = *zero;
190200141201             wSQL += %trimL( V01cmu );
190300141201           endif;
190400141201
190500141201           wSQL += ')';
190600141201
190700141201         EndIf;
190800141201
190900141201
191000150112         // -?Altre Parzializzazioni ricevute (da TRKC20R)?
191100141201         //  ?relative all'anagrafica?
191200141201         IF  %parms() > 2;
191300141201
191400141201           // ·?Località?
191500150112           if  KC20loc <> *blank;
191600150112             wSQL += ' and INDcit like ''' + %trimR( KC20loc ) + '%''';
191700141201           endif;
191800141201
191900141201           // ·?Provincia?
192000150112           if  KC20prv <> *blank;
192100150112             wSQL += ' and INDprv = ''' + %trimR( KC20prv ) + '''';
192200141201           endif;
192300141201
192400141201           // ·?C.A.P.?
192500150112           if  KC20cap <> *blank;
192600150112             wSQL += ' and INDcae = ''' + %trimR( KC20cap ) + '''';
192700141201           endif;
192800141201
192900141201           // ·?Partita Iva?
193000150112           if  KC20piv <> *blank;
193100150112             wSQL += ' and INDiva = ''' + %trimR( KC20piv ) + '''';
193200141201           endif;
193300141201
193400141201           // ·?Codice Fiscale?
193500150112           if  KC20cdf <> *blank;
193600150112             wSQL += ' and INDcdf = ''' + %trimR( KC20cdf ) + '''';
193700141201           endif;
193800141201
193900141201           // ·?Categoria Merceologica?
194000150112           //If  KC20sct_ds <> *blank;
194100150112           If  sk_KC20sct(1) + sk_KC20sct(2) + sk_KC20sct(3) <> *zero;
194200141201
194300141201             wSQL += ' and ACOitc in (';
194400141201             clear  yy;
194500141201
194600150112             For  xx = 1  To %elem( sk_KC20sct );
194700141201
194800150112               if  sk_KC20sct(xx) <> *zero;
194900141201                 if  yy > *zero;
195000141201                   wSQL += ', ';
195100141201                 endif;
195200141201                 yy += 1;
195300150112                 wSQL += %trim( %editc( sk_KC20sct(xx) : '3' ) );
195400141201               endif;
195500141201
195600141201             EndFor;
195700141201
195800141201             wSQL += ')';
195900141201
196000141201           EndIf;
196100141201
196200141201           // ·?Bloccati?
196300141201           select;
196400150112             when  KC20blc = 'N';
196500141201               wSQL += ' and ACOabl = '' ''';
196600150112             when  KC20blc = 'S';
196700141201               wSQL += ' and ACOabl <> '' ''';
196800141201           endsl;
196900141201
197000141201         ENDIF;
197100141201
197200141201       ENDSR;
197300141201
197400141201       //--------------------------------------------------------------
197500141201       //?Lettura singolo record in base all'ordinamento.              ?
197600141201       //--------------------------------------------------------------
197700141201       BEGSR  sr_ReadRec;
197800141201
197900141201         $RecOK = *off;
198000141201
198100141201         DoW  Not $EoF  and  Not $RecOK;
198200141201
198300141201           // -?Ordinamento per Codice Cliente?
198400141201           If  wOrd = *zero;
198500141201
198600141201             reade  %kds( keyTICMC01 )  TICMC000;
198700141201             $EoF = ( %eof(TICMC01L) );
198800141201
198900141201             if  Not $EoF;
199000141201               //chain  ( ACOkut : ACOkcc : ACOksc )  CNIND000;
199100141201               chain  ( ACOkut : ACOkcc : ACOksc )  CNCLP000;
199200141201             endif;
199300141201
199400141201           // -?Ordinamento per Ragione Sociale o Importanza Cliente?
199500141201           Else;
199600141201
199700141201               exsr  sr_ReadCursor;
199800141201
199900141201           EndIf;
200000141201
200100141201           // -?Selezione record in base alle autorizzazioni dell'utente?
200200141201           //                         ?e alle parzializzazioni?
200300141201           if  Not $EoF;
200400141201             exsr sr_SelezRec;
200500141201           endif;
200600141201
200700141201         EndDo;
200800141201
200900141201       ENDSR;
201000141201
201100141201       //--------------------------------------------------------------
201200141201       //?Selezione record in base alle autorizzazioni dell'utente     ?
201300141201       //?                       e alle parzializzazioni.              ?
201400141201       //--------------------------------------------------------------
201500141201       BEGSR  sr_SelezRec;
201600141201
201700141201         $RecOK = *off;
201800141201
201900141201         select;
202000141201
202100141201           // -?Esclusione Codice Cartello 888?
202200141201           when    wOrd   = *zero  AND  (
202300141201                 ( CMCksu > *zero  and
202400141201                   %subst( %editc( CMCksu : 'X' ) : 1 : 3 ) = '888' )  OR
202500141201                 ( CMCksc > *zero  and
202600141201                   %subst( %editc( CMCksc : 'X' ) : 1 : 3 ) = '888' ) );
202700141201             leavesr;
202800141201           when  wOrd > *zero  and  wSQL_ds.SQLcli > *zero  and
202900141201                 %subst( %editc( wSQL_ds.SQLcli : 'X' ) : 1 : 3 ) = '888';
203000141201             leavesr;
203100141201
203200141201           // -?Esclusione Codice FIL. + 0000?
203300141201           when    wOrd   = *zero  AND  (
203400141201                 ( CMCksu > *zero  and
203500141201                   %subst( %editc( CMCksu : 'X' ) : 4 : 4 ) = '0000' )  OR
203600141201                 ( CMCksc > *zero  and
203700141201                   %subst( %editc( CMCksc : 'X' ) : 4 : 4 ) = '0000' ) );
203800141201             leavesr;
203900141201           when  wOrd > *zero  and  wSQL_ds.SQLcli > *zero  and
204000141201                 %subst( %editc( wSQL_ds.SQLcli : 'X' ) : 4 : 4 ) = '0000';
204100141201             leavesr;
204200141201
204300141201         endsl;
204400141201
204500141201
204600141201         // -?Impostazione chiave di accesso per Num.Campagna/Cliente?
204700141201         clear  keyTICMF01;
204800141201         If  wOrd = *zero;
204900141201           k_CMFncm = CMCncm;
205000141201           k_CMFksu = CMCksu;
205100141201           k_CMFksc = CMCksc;
205200141201           k_CMFcpo = CMCcpo;
205300141201         Else;
205400141201           k_CMFncm = TICMC_ds.CMCncm;
205500141201           k_CMFksu = TICMC_ds.CMCksu;
205600141201           k_CMFksc = TICMC_ds.CMCksc;
205700141201           k_CMFcpo = TICMC_ds.CMCcpo;
205800141201         EndIf;
205900141201
206000141201
206100141201         // -?Esclusione Clienti con network "LOG" e "XXX"?
206200141201         If  ( wOrd = *zero  and  k_CMFksu       > *zero )  or
206300141201             ( wOrd = *zero  and  k_CMFksc       > *zero )  or
206400141201             ( wOrd > *zero  and  wSQL_ds.SQLcli > *zero );
206500141201           clear  Og143;
206600141201           select;
206700141201             when  wOrd = *zero  and  k_CMFksc > *zero;
206800141201               ORGfil = k_CMFksc / 10000;
206900141201             when  wOrd = *zero  and  k_CMFksu > *zero;
207000141201               ORGfil = k_CMFksu / 10000;
207100141201             when  wOrd > *zero  and  wSQL_ds.SQLcli > *zero;
207200141201               ORGfil = wSQL_ds.SQLcli / 10000;
207300141201           endsl;
207400141201           chain  ( ORGfil )  AZORG;
207500141201           if  %found(AZORG01L);
207600141201             Og143 = ORGde3;
207700141201           endif;
207800141201           if  Og143.§ogNTW = 'LOG'  or  Og143.§ogNTW = 'XXX';
207900141201             leavesr;
208000141201           endif;
208100141201         EndIf;
208200141201
208300141201         // -?Esclusione Clienti NON in gestione?
208400141201         clear  TNTAA1ds;
208500141201         iTAA1caut = '§utecli';
208600141201         select;
208700141201           when  k_CMFcpo <> *zero;
208800141201             iTAA1cpo = k_CMFcpo;
208900141201           when  k_CMFksc <> *zero;
209000141201             iTAA1ksc = k_CMFksc;
209100141201           when  k_CMFksu <> *zero;
209200141201             iTAA1ksc = k_CMFksu;
209300141201         endsl;
209400141201
209500141201         kpjbu = TNTAA1ds;
209600141201         TNTAA1C ( KPJBA );
209700141201         TNTAA1ds = kpjbu;
209800141201         if  oTAA1fabi = 'N';
209900141201           // -?Verifico con il commerciale?
210000141202           if  wOrd = *zero;
210100141202             xx =  %lookup( CLPage : sk_Cmm );
210200141202           else;
210300141202             xx =  %lookup( wSQL_ds.SQLcmm : sk_Cmm );
210400141202           endif;
210500141201           if  xx = *zero;
210600141201             leavesr;
210700141201           endif;
210800141201         endif;
210900141201
211000141201
211100141201         // -?Reperimento Commerciale del cliente?
211200141201         //  ?(SE reperita Campagna per singolo cliente)?
211300141201         If  wOrd = *zero;
211400141201
211500141201           Select;
211600141201
211700141201             // ·?Campagna Commerciale per Potenziali?
211800141201             When  k_CMFcpo <> *zero;
211900141201               clear  keyTNCPO01;
212000141201               k_CPOcpo = k_CMFcpo;
212100141201               chain  %kds( keyTNCPO01 )  TNCPO000;
212200141201               CLPage   = CPOcmm;
212300141201
212400141201             // ·?Campagna Commerciale per Clienti?
212500141201             When  k_CMFksc <> *zero;
212600141201               clear  keyCNACO00;
212700141201               k_ACOkut = c_KUT;
212800141201               k_ACOkcc = DUTkci;
212900141201               k_ACOksc = k_CMFksc;
213000141201               chain  %kds( keyCNACO00 )  CNACO000;
213100141201               chain  %kds( keyCNACO00 )  CNCLP000;
213200141201
213300141201             // -?Campagna Commerciale per Unificanti?
213400141201             When  k_CMFksu <> *zero;
213500141201               clear  keyCNACO00;
213600141201               k_ACOkut = c_KUT;
213700141201               k_ACOkcc = DUTkci;
213800141201               k_ACOksc = k_CMFksu;
213900141201               chain  %kds( keyCNACO00 )  CNACO000;
214000141201               chain  %kds( keyCNACO00 )  CNCLP000;
214100141201
214200141201           EndSl;
214300141201
214400141201         Else;
214500141201
214600141201           CLPage = wSQL_ds.SQLcmm;
214700141201
214800141201         EndIf;
214900141201
215000141201         // -?Selezione Clienti con/senza Trattativa Campagna Avviata?
215100150226         //  ?o FORZATA (chiave "principale" già impostata)?
215200141201         If  V01tcm <> *blank;
215300141201           k_CMFacm = ' TR';
215400141201           setll  %kds( keyTICMF01 : 5 )  TICMF000;
215500150226           if  Not %equal( TICMF01L );
215600150226             k_CMFacm = ' TF';
215700150226             setll  %kds( keyTICMF01 : 5 )  TICMF000;
215800150226           endif;
215900150226           if  ( Not %equal( TICMF01L )  and  V01tcm = 'S' )  or
216000150226                   ( %equal( TICMF01L )  and  V01tcm = 'N' );
216100150226             leavesr;
216200141201           endif;
216300141201         EndIf;
216400141201
216500141201         // -?Esclusione Clienti con Attività in Corso?
216600141201         if  wOrd = *zero;
216700141201           SH1cpo2 = ACOlib;
216800141201           SH1ksc2 = ACOksc;
216900141201         else;
217000141201           SH1cpo2 = wSQL_ds.SQLlib;
217100141201           SH1ksc2 = wSQL_ds.SQLcli;
217200141201         endif;
217300141201         exsr  sr_ProssimaAttivita;
217400150127         if  ( V01eat = 'N'  and  wATCcad <> *blank )  or
217500150127             ( V01eat = 'S'  and  wATCcad =  *blank );
217600141201           leavesr;
217700141201         endif;
217800141201
217900141201
218000141201         // -?NON ricevute altre parzializzazioni?
218100141201         If  %parms() = 2;
218200141201           $RecOK = *on;
218300141201           leavesr;
218400141201         EndIf;
218500141201
218600141201
218700141201         // -?Controllo trattative in corso?
218800150112         IF  KC20ttr <> *blank;
218900141201
219000141201           $TTR = *off;
219100141201           clear  keyTIVIS01;
219200141202           k_VIScpo = SH1cpo2;
219300141201           setll  %kds( keyTIVIS01 )  TIVIS000;
219400150127           reade  %kds( keyTIVIS01 : 1 )  TIVIS000;
219500141201
219600141201           DoW  Not %eof(TIVIS01L);
219700141201
219800150226             if  VISksc = SH1ksc2  and  VISffz = *blank
219900150226                                   and  VISdch = *zero;
220000141201               $TTR = *on;
220100141201               leave;
220200141201             endif;
220300141201
220400150127             reade  %kds( keyTIVIS01 : 1 )  TIVIS000;
220500141201
220600141201           EndDo;
220700141201
220800150127           if  ( KC20ttr = 'S'  and  Not $TTR )  or
220900150127               ( KC20ttr = 'N'  and  $TTR );
221000141201             leavesr;
221100141201           endif;
221200141201
221300141201         ENDIF;
221400141201
221500141201
221600141201         // -?Parzializzazioni INFO commerciali?
221700141201
221800141201         // ·?Affidato a BRT?
221900150112         If  KC20brtI > *zero;
222000141202           chain  ( SH1cpo2 : '10 ' : '_BAR' )  TICPI000;
222100141201           select;
222200141201             when  Not %found(TICPI01L);
222300141201               leavesr ;
222400150112             when  CPIval < KC20brtI  or  CPIval > KC20brtF;
222500141201              leavesr ;
222600141201           endsl;
222700141201         Endif;
222800141201
222900141201         // ·?NON affidato a BRT: Italia-Parcel?
223000150112         If  KC20nrfI > *zero;
223100141202           chain  ( SH1cpo2 : 'NRF' )  TICPI000;
223200141201           select;
223300141201             when  Not %found(TICPI01L);
223400141201               leavesr ;
223500150112             when  CPIpft < KC20nrfI  or  CPIpft > KC20nrfF;
223600141201              leavesr ;
223700141201           endsl;
223800141201         EndIf;
223900141201
224000141201         // ·?NON affidato a BRT: Italia-Messaggeria?
224100150112         If  KC20nroI > *zero;
224200141202           chain  ( SH1cpo2 : 'NRO' )  TICPI000;
224300141201           select;
224400141201             when  Not %found(TICPI01L);
224500141201               leavesr ;
224600150112             when  CPIpft < KC20nroI  or  CPIpft > KC20nroF;
224700141201              leavesr ;
224800141201           endsl;
224900141201         EndIf;
225000141201
225100141201         // ·?NON affidato a BRT: Estero-Via Aerea?
225200150112         If  KC20nerI > *zero;
225300141202           chain  ( SH1cpo2 : 'NER' )  TICPI000;
225400141201           select;
225500141201             when  Not %found(TICPI01L);
225600141201               leavesr ;
225700150112             when  CPIpft < KC20nerI  or  CPIpft > KC20nerF;
225800141201              leavesr ;
225900141201           endsl;
226000141201         EndIf;
226100141201
226200141201         // ·?NON affidato a BRT: Estero-Servizio Camionistico?
226300150112         If  KC20naeI > *zero;
226400141202           chain  ( SH1cpo2 : 'NAE' )  TICPI000;
226500141201           select;
226600141201             when  Not %found(TICPI01L);
226700141201               leavesr ;
226800150112             when  CPIpft < KC20naeI  or  CPIpft > KC20naeF;
226900141201              leavesr ;
227000141201           endsl;
227100141201         EndIf;
227200141201
227300141201         // ·?NON affidato a BRT: Altro (es. Completi, ADR)?
227400150112         If  KC20ntrI > *zero;
227500141202           chain  ( SH1cpo2 : 'NTR' )  TICPI000;
227600141201           select;
227700141201             when  Not %found(TICPI01L);
227800141201               leavesr ;
227900150112             when  CPIpft < KC20ntrI  or  CPIpft > KC20ntrF;
228000141201              leavesr ;
228100141201           endsl;
228200141201         EndIf;
228300141201
228400141201         // ·?Paesi Estero?
228500150112         If  KC20est_ds <> *blank;
228600141201
228700141201           $Estero = *off;
228800141201
228900150112           For  xx = 1  To  %elem( sk_KC20est );
229000141201
229100150112             If  sk_KC20est( xx ) <> *blank;
229200141201
229300150112               chain  ( SH1cpo2 : '50' : sk_KC20est(xx) )  TICPI000;
229400141201               if  %found(TICPI01L)  and  CPIval > *zero;
229500141201                 $Estero = *on;
229600141201                 leave;
229700141201               endif;
229800141201
229900141201             EndIf;
230000141201
230100141201           EndFor;
230200141201
230300141201           if  Not $Estero;
230400141201             leavesr;
230500141201           endif;
230600141201
230700141201         EndIf;
230800141201
230900141201         // ·?Peso medio a spedizione?
231000150112         If  KC20pkgI > *zero;
231100141202           chain  ( SH1cpo2 : 'KMS' )  TICPI000;
231200141201           select;
231300141201             when  Not %found(TICPI01L);
231400141201               leavesr ;
231500150112             when  CPIval < KC20pkgI  or  CPIval > KC20pkgF;
231600141201              leavesr ;
231700141201           endsl;
231800141201         EndIf;
231900141201
232000141201         // ·?Spedizioni 10:30/PRIORITY?
232100150112         If  KC20shp > *blank;
232200141202           chain  ( SH1cpo2 : 'HPR' )  TICPI000;
232300141201           select;
232400141201             when  Not %found(TICPI01L);
232500141201               leavesr ;
232600141201             when  CPIval = *zero;
232700141201              leavesr ;
232800141201           endsl;
232900141201         EndIf;
233000141201
233100141201         // ·?Spedizioni E-COMMERCE?
233200150112         If  KC20secm > *blank;
233300141202           chain  ( SH1cpo2 : 'ECM' )  TICPI000;
233400141201           select;
233500141201             when  Not %found(TICPI01L);
233600141201               leavesr ;
233700141201             when  CPIval = *zero;
233800141201              leavesr ;
233900141201           endsl;
234000141201         EndIf;
234100141201
234200141201         // ·?Spedizioni SECURE BOX?
234300150112         If  KC20sbx <> *blank;
234400141202           chain  ( SH1cpo2 : 'SBX' )  TICPI000;
234500141201           select;
234600141201             when  Not %found(TICPI01L);
234700141201               leavesr ;
234800141201             when  CPIfsn <> 'S';
234900141201              leavesr ;
235000141201           endsl;
235100141201         EndIf;
235200141201
235300141201         // ·?Concorrenti?
235400150112         If  KC20conc_ds <> *blank;
235500141201
235600141201           $Conc = *off;
235700141201
235800150112           For  xx = 1  To  %elem( sk_KC20conc );
235900141201
236000150112             If  sk_KC20conc( xx ) <> *blank;
236100141201
236200150112               chain  ( SH1cpo2 : '10' : sk_KC20conc(xx) )  TICPI000;
236300141201               if  %found(TICPI01L)  and  CPIval > *zero;
236400141201                 $Conc = *on;
236500141201                 leave;
236600141201               endif;
236700141201
236800141201             EndIf;
236900141201
237000141201           EndFor;
237100141201
237200141201           if  Not $Conc;
237300141201             leavesr;
237400141201           endif;
237500141201
237600141201         EndIf;
237700141201
237800141201         // ·?Logistica: INTERESSATI S/N?
237900150112         If  KC20int <> *blank;
238000141202           chain  ( SH1cpo2 : 'LOG' )  TICPI000;
238100141201           select;
238200141201             when  Not %found(TICPI01L);
238300141201               leavesr ;
238400150112             when  CPIfsn = KC20int;
238500141201               leavesr ;
238600141201             // ·?Verifica delle date di inserimento delle INFO?
238700150112             when (KC20dil1  > *zero     or  KC20dil2  > *zero)  and
238800150112                  (CPIdim    < KC20dil1  or  CPIdim    > KC20dil2);
238900141201               leavesr ;
239000141201           endsl;
239100141201         EndIf;
239200141201
239300141201         // ·?Logistica: OUTSOURCING INTERESSATI S/N?
239400150112         If  KC20los <> *blank;
239500141202           chain  ( SH1cpo2 : 'OUT' )  TICPI000;
239600141201           select;
239700141201             when  Not %found(TICPI01L);
239800141201               leavesr ;
239900150112             when  CPIfsn <> KC20los;
240000141201              leavesr ;
240100141201           endsl;
240200141201         EndIf;
240300141201
240400141201         // ·?Concorrenti di LOGISTICA?
240500150112         If  KC20concL_ds <> *blank;
240600141201
240700141201           $ConcLog = *off;
240800141201
240900150112           For  xx = 1  To  %elem( sk_KC20concL );
241000141201
241100150112             If  sk_KC20concL( xx ) <> *blank;
241200141201
241300150112               chain  ( SH1cpo2 : '40' : sk_KC20concL(xx) )  TICPI000;
241400141201               if  %found(TICPI01L);
241500141201                 $ConcLog = *on;
241600141201                 leave;
241700141201               endif;
241800141201
241900141201             EndIf;
242000141201
242100141201           EndFor;
242200141201
242300141201           if  Not $ConcLog;
242400141201             leavesr;
242500141201           endif;
242600141201
242700141201         EndIf;
242800141201
242900141201         // ·?Reperimento Unificante?
243000141201         //  ?(serve comunque per il reperimento delle Campagne)?
243100141201         clear  TIBS10ds;
243200141201         D10tle = 'ST';
243300141201         D10paf = 'P';
243400141202         D10cod = SH1ksc2;
243500141201         TIBS10R ( TIBS10ds );
243600141201         if  D10err = *blank  and  D10cop > *zero;
243700141201           wCodUni = D10cop;
243800141201         else;
243900141202           wCodUni = SH1ksc2;
244000141201         endif;
244100141201
244200141201         // ·?Parzializzazione dei soli codici unificanti?
244300150112         if  KC20uni <> *blank  and
244400150112             ( ( KC20uni = 'S'  and  wCodUni <> SH1ksc2 )  or
244500150112               ( KC20uni = 'N'  and  wCodUni  = SH1ksc2 ) );
244600141201             leavesr;
244700141201         endif;
244800141201
244900141201         // ·?Cliente in Campagna (qui lo è sicuro!)?
245000141201
245100141201
245200141201         // -?=> Record OK!?
245300141201         $RecOK = *on;
245400141201
245500141201       ENDSR;
245600141120
245700141120       //--------------------------------------------------------------
245800141124       //?Reperimento Trattative Campagna avviate.                     ?
245900141120       //--------------------------------------------------------------
246000141124       BEGSR  sr_TrattativeCampagna;
246100141120
246200150226         // -?Controllo Trattativa in corso?
246300141124         clear  keyTICMF01;
246400141124         k_CMFncm = C01ncm;
246500141124         k_CMFksu = SH1ksu;
246600141124         k_CMFksc = SH1ksc;
246700141124         k_CMFcpo = SH1cpo;
246800141124         k_CMFacm = ' TR';
246900141124
247000141124         setll  %kds( keyTICMF01 : 5 )  TICMF000;
247100141124
247200141124         if  %equal( TICMF01L );
247300141124           S01tcm = 'S';
247400150226           leavesr;
247500141124         endif;
247600150226
247700150226         // -?Controllo Trattativa Forzata?
247800150226         //  ?(SE non esiste Trattativa in corso)?
247900150226         k_CMFacm = ' TF';
248000150226
248100150226         setll  %kds( keyTICMF01 : 5 )  TICMF000;
248200150226
248300150226         if  %equal( TICMF01L );
248400150226           S01tcm = '*';
248500150226           leavesr;
248600150226         endif;
248700141120
248800141120       ENDSR;
248900141120
249000141120       //--------------------------------------------------------------
249100141120       //?Reperimento Data Ultimo Appuntamento.                        ?
249200141120       //--------------------------------------------------------------
249300141120       BEGSR  sr_DataUltimoAppuntamento;
249400141120
249500141126         clear  wATCdco;
249600141126
249700141120         wSQL_tmp = 'select ATCdco +
249800141120                       from TIATC00F +
249900141124                      where ATCcpo = ' + %trim( %editc( SH1cpo2 : '3' ) ) +
250000141124                      ' and ATCksc = ' + %trim( %editc( SH1ksc2 : '3' ) ) +
250100141124                      ' and ATCtat = ''A'' +
250200141124                        and ATCest = ''S'' +
250300141124                        and ATCcad = ''91'' +
250400141124                      order by ATCdco desc, ATChco desc +
250500141124                        for fetch only';
250600141120
250700141124         exec sql   prepare S2   from :wSQL_tmp;
250800141124         exec sql   declare C2   cursor for S2;
250900141124         exec sql   open C2;
251000141120
251100141120         If  SQLcode < *zero;
251200141120           exsr  sr_PrintErrSQL;
251300141127           clear  S01uap;
251400141120         Else;
251500141120
251600141124           exec sql   fetch next   from C2   into :wATCdco;
251700141120
251800141120           select;
251900141120             when  SQLcode = 100;
252000141120             when  SQLcode < *zero;
252100141120               exsr  sr_PrintErrSQL;
252200141127               clear  S01uap;
252300141120             other;
252400141124               wData_DMY = %date( wATCdco : *iso );
252500141124               S01uap    = %dec( wData_DMY );
252600141120           endsl;
252700141120
252800141120         EndIf;
252900141120
253000141124         exec sql   close C2;
253100141120
253200141120       ENDSR;
253300141120
253400141120       //--------------------------------------------------------------
253500141124       //?Reperimento Prossima Attività commerciale.                   ?
253600141120       //--------------------------------------------------------------
253700141124       BEGSR  sr_ProssimaAttivita;
253800141120
253900141126         clear  wATCcad;
254000141126
254100141120         wSQL_tmp = 'select ATCcad +
254200141120                       from TIATC00F +
254300141124                      where ATCcpo = ' + %trim( %editc( SH1cpo2 : '3' ) ) +
254400141124                      ' and ATCksc = ' + %trim( %editc( SH1ksc2 : '3' ) ) +
254500141124                      ' and ATCdco = 0 +
254600141124                      order by ATCdad, ATChda +
254700141124                        for fetch only';
254800141120
254900141124         exec sql   prepare S3   from :wSQL_tmp;
255000141124         exec sql   declare C3   cursor for S3;
255100141124         exec sql   open C3;
255200141120
255300141120         If  SQLcode < *zero;
255400141120           exsr  sr_PrintErrSQL;
255500141127           wATCcad = *all'?';
255600141120         Else;
255700141120
255800141124           exec sql   fetch next   from C3   into :wATCcad;
255900141120
256000141120           select;
256100141120             when  SQLcode = 100;
256200141120             when  SQLcode < *zero;
256300141120               exsr  sr_PrintErrSQL;
256400141127               wATCcad = *all'?';
256500141120           endsl;
256600141120
256700141120         EndIf;
256800141127
256900141127         S01cad = wATCcad;
257000141120
257100141124         exec sql   close C3;
257200141120
257300141120       ENDSR;
257400141124
257500141124       //--------------------------------------------------------------
257600141124       //?Reperimento Trattative in Corso.                             ?
257700141124       //--------------------------------------------------------------
257800141124       BEGSR  sr_TrattativeInCorso;
257900141124
258000141124         clear  keyTIVIS01;
258100141124         k_VIScpo = SH1cpo2;
258200141124         //k_VISksc = SH1ksc2;
258300141124
258400141124         setll  %kds( keyTIVIS01 )  TIVIS000;
258500150127         reade  %kds( keyTIVIS01 : 1 )  TIVIS000;
258600141124
258700141124         DoW  Not %eof(TIVIS01L);
258800141124
258900150226           if  VISksc = SH1ksc2  and  VISffz = *blank
259000150226                                 and  VISdch = *zero;
259100141124             S01ttr = 'S';
259200141124             leavesr;
259300141124           endif;
259400141124
259500150127           reade  %kds( keyTIVIS01 : 1 )  TIVIS000;
259600141124
259700141124         EndDo;
259800141124
259900141124       ENDSR;
260000141106
260100141106       //--------------------------------------------------------------
260200141106       //?Apertura cursore SQL.                                        ?
260300141106       //--------------------------------------------------------------
260400141106       BEGSR  sr_OpenCursor;
260500141106
260600141106         // -?Dichiarazione cursore?
260700141106         exec sql   prepare S1   from :wSQL;
260800141106         exec sql   declare C1   cursor for S1;
260900141106
261000141106         if  SQLcode < *zero;
261100141106           $EoF = *on;
261200141106           exsr  sr_PrintErrSQL;
261300141106         endif;
261400141106
261500141106         // -?Apertura del cursore?
261600141106         exec sql   open C1;
261700141106
261800141106         if  SQLcode < *zero;
261900141106           $EoF = *on;
262000141106           exsr  sr_PrintErrSQL;
262100141106         endif;
262200141106
262300141106       ENDSR;
262400141106
262500141106       //--------------------------------------------------------------
262600141106       //?Lettura cursore.                                             ?
262700141106       //--------------------------------------------------------------
262800141106       BEGSR  sr_ReadCursor;
262900141106
263000141106         clear  wSQL_ds;
263100141106
263200141106         exec sql   fetch next   from C1   into :TICMC_ds, :wSQL_ds;
263300141106
263400141106         select;
263500141106           when  SQLcode = 100;
263600141106             $EoF = *on;
263700141106           when  SQLcode < *zero;
263800141106             $EoF = *on;
263900141106             exsr  sr_PrintErrSQL;
264000141106         endsl;
264100141106
264200141106       ENDSR;
264300141106
264400141106       //--------------------------------------------------------------
264500141106       //?Chiusura cursore SQL.                                        ?
264600141106       //--------------------------------------------------------------
264700141106       BEGSR  sr_CloseCursor;
264800141106
264900141106         // -?Chiusura del cursore?
265000141106         exec sql   close C1;
265100141106
265200141106       ENDSR;
265300141105
265400141105       //--------------------------------------------------------------
265500141105       //?Stampa segnalazione dell'errore rilevato via SQL.            ?
265600141105       //--------------------------------------------------------------
265700141105       BEGSR  sr_PrintErrSQL;
265800141105
265900141105         // -?Stampa del Dump?
266000141105         Dump(A);
266100141105
266200141105         // -?Stampa del Job-Log?
266300141105         Qcmd = 'DSPJOBLOG job(*) output(*print)';
266400141105         exsr  sr_ExecCmd;
266500141105
266600141105         // -?Chiusura del programma?
266700150112         oKC21err = *on;
266800150112         oKC21msg = sk_Msg(10);
266900141105         exsr  sr_RoutEnd;
267000141105
267100141105       ENDSR;
267200141105
267300141105       //--------------------------------------------------------------
267400141105       //?Esecuzione del comando (già impostato).                      ?
267500141105       //--------------------------------------------------------------
267600141105       BEGSR  sr_ExecCmd;
267700141105
267800141105         clear Qcap0100;
267900141105         Qcabcsdh = *off;
268000141105         Qcapa    = *off;
268100141105         Qcacmdss = *off;
268200141105         Qcaerved = *allX'00';
268300141105
268400141105         clear Qusec;
268500141105         Qusbprv  = %size(Qusec);
268600141105
268700141105         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
268800141105                           %size(Qcap0100) : 'CPOP0100' : *omit :
268900141105                           0 : 0 : Qusec);
269000141105
269100141105         //if  Qusei <> *blank;
269200141105         //  ...;
269300141105         //endif;
269400141105
269500141105       ENDSR;
269600141104
269700141104       //--------------------------------------------------------------
269800141106       //?Operazioni finali.                                           ?
269900141104       //--------------------------------------------------------------
270000141106       BEGSR  sr_RoutEnd;
270100141104
270200141106         // -?Chiusura applicazioni precedentemente aperte?
270300141106         clear  TNTAA1ds;
270400141106         iTAA1tla = 'C';
270500141106         clear  KPJBU;
270600141106         KPJBU = TNTAA1ds;
270700141106         TNTAA1C ( kpjba );
270800141106
270900141106         // -?Chiusura *pgm?
271000141104         return;
271100141104
271200141104       ENDSR;
271300141104
271400141104      /end-free
271500141104
271600141104       //--------------------------------------------------------------
271700141104       //?Schiere a tempo di compilazione.
271800141104       //--------------------------------------------------------------
271900141104
272000141105** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
272100141106Utente non abilitato alla funzione                                              1
272200141105Campagna Commerciale errata                                                     2
272300141105Cliente non trovato nella campagna commerciale indicata                         3
272400141106Ordinamento errato                                                              4
272500141106Codice importanza cliente errato                                                5
272600141106Filiale di Appartenenza errata                                                  6
272700141106Codice area errato                                                              7
272800141106Codice commerciale errato o non in gestione all'utente                          8
272900141106Richiedere solo commerciali UNIFICANTI                                          9
273000141107Rilevato errore in estrazione dati: consultare la stampa del LOG del lavoro    10
273100141107Non è stato trovato nessun cliente                                             11
273200141118Raggiunto il n° massimo dei clienti visualizzabili a video                     12
273300141107Opzione errata                                                                 13
273400141110Ammessa una sola scelta e già scelto il cliente                                14
