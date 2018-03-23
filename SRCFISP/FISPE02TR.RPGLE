000100100125      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF)
000200100125     /*PRM dftactgrp(*no) actgrp(QILE)
000300100125     /*END
000400100125
000500100125       //--------------------------------------------------------------
000600100125       //?TEST |STORE PROCEDURE| EasySpedWeb                           ?
000700100125       //--------------------------------------------------------------
000800100125
000900100125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000100125     h dftactgrp(*no) actgrp('QILE')
001100100125
001200100125       //--------------------------------------------------------------
001300100125       //?Dichiarazione file.                                          ?
001400100125       //--------------------------------------------------------------
001500100125
001600100125       //?- Video?
001700100125     fFISPE02TD cf   e             workstn
001800100125     f                                     indds(IndDspF)
001900100125     f                                     infds(InfDspF)
002000100125
002100100125       //--------------------------------------------------------------
002200100125       //?Definizione costanti.                                        ?
002300100125       //--------------------------------------------------------------
002400100125
002500100125       //?- Tasti funzionali a video?
002600100125     d c_F01           c                   const(x'31')
002700100125     d c_F02           c                   const(x'32')
002800100125     d c_F03           c                   const(x'33')
002900100125     d c_F04           c                   const(x'34')
003000100125     d c_F05           c                   const(x'35')
003100100125     d c_F06           c                   const(x'36')
003200100125     d c_F07           c                   const(x'37')
003300100125     d c_F08           c                   const(x'38')
003400100125     d c_F09           c                   const(x'39')
003500100125     d c_F10           c                   const(x'3A')
003600100125     d c_F11           c                   const(x'3B')
003700100125     d c_F12           c                   const(x'3C')
003800100125     d c_F13           c                   const(x'B1')
003900100125     d c_F14           c                   const(x'B2')
004000100125     d c_F15           c                   const(x'B3')
004100100125     d c_F16           c                   const(x'B4')
004200100125     d c_F17           c                   const(x'B5')
004300100125     d c_F18           c                   const(x'B6')
004400100125     d c_F19           c                   const(x'B7')
004500100125     d c_F20           c                   const(x'B8')
004600100125     d c_F21           c                   const(x'B9')
004700100125     d c_F22           c                   const(x'BA')
004800100125     d c_F23           c                   const(x'BB')
004900100125     d c_F24           c                   const(x'BC')
005000100125     d c_Enter         c                   const(x'F1')
005100100125     d c_RollDown      c                   const(x'F4')
005200100125     d c_RollUp        c                   const(x'F5')
005300100125
005400100125       //--------------------------------------------------------------
005500100125       //?Definizione schiere.                                         ?
005600100125       //--------------------------------------------------------------
005700100125
005800100125
005900100125       //--------------------------------------------------------------
006000100125       //?Definizione aree dati.                                       ?
006100100125       //--------------------------------------------------------------
006200100125
006300100125       //?- Dati utente?
006400100125     d §AzUte        e ds                  extname(AZUTE00F)
006500100125     d                                     dtaara
006600100125     d §DatiUte      e ds                  extname(dDatiUte)
006700100125     d                                     dtaara
006800100125
006900100125       //--------------------------------------------------------------
007000100125       //?Definizione strutture dati.                                  ?
007100100125       //--------------------------------------------------------------
007200100125
007300100125       //?- Status ds?
007400100125     d Status         sds
007500100125     d  SDSpgm           *proc
007600100125     d  SDSprm           *parms
007700100125     d  SDSusr               254    263
007800100125
007900100125       //?- InfDS?
008000100125     d InfDspF         ds
008100100125     d  dsp_aid              369    369a                                        AID byte
008200100125
008300100125       //?- Indicatori su DspF?
008400100125     d IndDspF         ds
008500100125        //?- Indicatori di errore?
008600100125     d  ErrMessage                    1n   overlay(IndDspF : 28)
008700100125     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008800100125
008900100125       //?- Parametri ricevuti?
009000100125     d KPJBA         e ds
009100100125
009200100125       //?- Parametri x Controllo profilo utenti?
009300100125     d TIBS34ds      e ds
009400100125
009500100125       //--------------------------------------------------------------
009600100125       //?Definizione variabili globali.                               ?
009700100125       //--------------------------------------------------------------
009800100125
009900100125       //?- Parametri per FISPE02R: EasySpedWeb: S.P.  TISIT5R+TISI95R?
010000100412     d iTYPE           s              1    inz
010100100125     d iLANG           s              3    inz
010200100505     d iDATA           s              8s 0 inz
010300100125     d iNTW            s              1    inz
010400100505     d iLNP            s              3s 0 inz
010500100125     d iNZD            s              3    inz
010600100125     d iPRD            s              2    inz
010700100125     d iCAD            s              9    inz
010800100125     d iLOD            s             35    inz
010900100125     d iIND            s             35    inz
011000100125     d iRSD            s             35    inz
011100100505     d iNCL            s              5s 0 inz
011200100505     d iPKB            s              7s 1 inz
011300100505     d iVLB            s              5s 3 inz
011400100125     d iFFD            s              1    inz
011500100125     d iTSP            s              1    inz
011600100125     d iTC1            s              1    inz
011700100125     d iTC2            s              1    inz
011800100505     d iCBO            s              2    inz
011900100125      *
012000100505     d oTFA            s              3s 0 inz
012100100505     d oLNA            s              3s 0 inz
012200100505     d oZNC            s              2s 0 inz
012300100125     d oLNPD           s             20    inz
012400100125     d oLNAD           s             20    inz
012500100125     d oERR            s              1    inz
012600100125     d oMSG            s            256    inz
012700100125     d oTAG            s             30    inz
012800100125      *
012900100125     d RTNesito        s             10I 0 inz
013000100125     d RTNopcode       s             10    inz
013100100125     d RTNstatus       s             10I 0 inz
013200100125
013300100125       //?- Flags booleani?
013400100125     d $Fine           s               n   inz(*off)
013500100125     d $InzD01         s               n   inz(*on)
013600100505
013700100505       //?- Campi di comodo?
013800100505     d wTimeI          s               z   inz
013900100125
014000100125       //--------------------------------------------------------------
014100100125       //?Definizione procedure usate.                                 ?
014200100125       //--------------------------------------------------------------
014300100125
014400100125       //?- Reperimento dati utente?
014500100125     d tibs34r         pr                  extpgm('TIBS34R')
014600100125     d   tibs34ds                          likeds(TIBS34ds)
014700100125
014800100125       //?- Esecuzione comando di sistema?
014900100125     d qCmdExc         pr                  extpgm('QCMDEXC')
015000100125     d   Qcmd                       512    const  options(*varsize)
015100100125     d   Qlen                        15  5 const
015200100125
015300100125       //?- Stored Procedure per EasySpedWeb?
015400100125     d fispE02r        pr                  extpgm('FISPE02R')
015500100412     d   i_TYPE                            like(iTYPE)
015600100125     d   i_LANG                            like(iLANG)
015700100125     d   i_DATA                            like(iDATA)
015800100125     d   i_NTW                             like(iNTW)
015900100125     d   i_LNP                             like(iLNP)
016000100125     d   i_NZD                             like(iNZD)
016100100125     d   i_PRD                             like(iPRD)
016200100125     d   i_CAD                             like(iCAD)
016300100125     d   i_LOD                             like(iLOD)
016400100125     d   i_IND                             like(iIND)
016500100125     d   i_RSD                             like(iRSD)
016600100125     d   i_NCL                             like(iNCL)
016700100125     d   i_PKB                             like(iPKB)
016800100125     d   i_VLB                             like(iVLB)
016900100125     d   i_FFD                             like(iFFD)
017000100125     d   i_TSP                             like(iTSP)
017100100125     d   i_TC1                             like(iTC1)
017200100125     d   i_TC2                             like(iTC2)
017300100505     d   i_CBO                             like(iCBO)
017400100125     d   o_TFA                             like(oTFA)
017500100125     d   o_LNA                             like(oLNA)
017600100125     d   o_ZNC                             like(oZNC)
017700100125     d   o_LNPD                            like(oLNPD)
017800100125     d   o_LNAD                            like(oLNAD)
017900100125     d   o_ERR                             like(oERR)
018000100125     d   o_MSG                             like(oMSG)
018100100125     d   o_TAG                             like(oTAG)
018200100125     d   RTN_esito                         like(RTNesito)
018300100125     d   RTN_opcode                        like(RTNopcode)
018400100125     d   RTN_status                        like(RTNstatus)
018500100125
018600100125       //--------------------------------------------------------------
018700100125       //?Definizione key-list.                                        ?
018800100125       //--------------------------------------------------------------
018900100125
019000100125
019100100125       //--------------------------------------------------------------
019200100125       //?Riepilogo indicatori.                                        ?
019300100125       //--------------------------------------------------------------
019400100125
019500100125     c     *Entry        plist
019600100125     c                   parm                    KPJBA
019700100125
019800100125      /free
019900100125
020000100125       //?- Start?
020100100125       *inLR = *on;
020200100125
020300100125       //?- Elab?
020400100125       dow  $Fine = *off;
020500100125         exsr sr_GesD01;
020600100125       enddo;
020700100125
020800100125       //?- End?
020900100125       return;
021000100125
021100100125       //--------------------------------------------------------------
021200100125       //?Operazioni iniziali                                          ?
021300100125       //--------------------------------------------------------------
021400100125       BEGSR *InzSR;
021500100125
021600100125         //?- Reperimento dati utente?
021700100125         exsr sr_DatiJob;
021800100125
021900100125         //?- Impostazione nome programma in testata a video?
022000100125         VTDpgm = SDSpgm;
022100100125
022200100125       ENDSR;
022300100125
022400100125       //--------------------------------------------------------------
022500100125       //?Reperimento Dati del job (Utente/Operativi).                 ?
022600100125       //--------------------------------------------------------------
022700100125       BEGSR sr_DatiJob;
022800100125
022900100125         in(E) §AzUte;
023000100125         if NOT %error;
023100100125           in(E) §DatiUte;
023200100125         endif;
023300100125         if %error or RSut = *blanks;
023400100125           clear TIBS34ds;
023500100125           tibs34r(tibs34ds);
023600100125           in §AzUte;
023700100125           in §DatiUte;
023800100125         endif;
023900100125
024000100125       ENDSR;
024100100125
024200100125       //--------------------------------------------------------------
024300100125       //?Gestione videata D01                                         ?
024400100125       //--------------------------------------------------------------
024500100125       BEGSR sr_GesD01;
024600100125
024700100125         //?- Inizializzazione videata?
024800100125         if  $InzD01 = *on;
024900100125           clear SPE02D01;
025000100125           $InzD01  = *off;
025100100125         endif;
025200100125
025300100125         //?- Emissione videata?
025400100125         if  ErrMessage = *off;
025500100125           write SPE02T01;
025600100125           write SPE02Z01;
025700100125         endif;
025800100125
025900100125         exfmt SPE02D01;
026000100125
026100100125         reset ErrMessage;
026200100125         reset ErrGenerico;
026300100125         clear V1Dmsg;
026400100125
026500100125         clear V1Dlin;
026600100125
026700100125         SELECT;
026800100125
026900100125           //?- F3=Fine?
027000100125           WHEN  dsp_aid = c_F03;
027100100125             $Fine = *on;
027200100125
027300100125           //?- Invio o F6=Conferma?
027400100125           OTHER;
027500100125             exsr sr_CtrD01;
027600100125             select;
027700100125               //?- rilevato errore?
027800100125               when  ErrGenerico = *on;
027900100125               //?- non premuto F6?
028000100507               //when  dsp_aid <> c_F06;
028100100125               //?- elaborazione dati immessi?
028200100125               other;
028300100125                 exsr Call_FISPE02R;
028400100125                 //reset $InzD01;
028500100125             endsl;
028600100125
028700100125         ENDSL;
028800100125
028900100125       ENDSR;
029000100125
029100100125       //--------------------------------------------------------------
029200100125       //?Richiamo stored procedure per EasySpedWeb                    ?
029300100125       //--------------------------------------------------------------
029400100125       BEGSR Call_FISPE02R;
029500100125
029600100125         clear  oTFA;
029700100125         clear  oLNA;
029800100125         clear  oZNC;
029900100125         clear  oLNPD;
030000100125         clear  oLNAD;
030100100125         clear  oERR;
030200100125         clear  oMSG;
030300100125         clear  oTAG;
030400100125         clear  RTNesito;
030500100125         clear  RTNopcode;
030600100125         clear  RTNstatus;
030700100505
030800100505         wTimeI = %timestamp();
030900100125
031000100412         fispE02r ( iTYPE :
031100100412                    iLANG :
031200100125                    iDATA :
031300100125                    iNTW :
031400100125                    iLNP :
031500100125                    iNZD :
031600100125                    iPRD :
031700100125                    iCAD :
031800100125                    iLOD :
031900100125                    iIND :
032000100125                    iRSD :
032100100125                    iNCL :
032200100125                    iPKB :
032300100125                    iVLB :
032400100125                    iFFD :
032500100125                    iTSP :
032600100125                    iTC1 :
032700100125                    iTC2 :
032800100505                    iCBO :
032900100125                    oTFA :
033000100125                    oLNA :
033100100125                    oZNC :
033200100125                    oLNPD :
033300100125                    oLNAD :
033400100125                    oERR :
033500100125                    oMSG :
033600100125                    oTAG :
033700100125                    RTNesito :
033800100125                    RTNopcode :
033900100125                    RTNstatus );
034000100505
034100100505         V1Dtim = %diff( %timestamp() : wTimeI : *MS );
034200100125
034300100125         voMSG = %subst( oMSG : 1 : %len(voMSG) );
034400100125         vRTNesito  = RTNesito;
034500100125         vRTNstatus = RTNstatus;
034600100125
034700100125         write  Protect;
034800100125         exfmt  SPE02D02;
034900100125
035000100125         //?- F3=Fine?
035100100125         if  dsp_aid = c_F03;
035200100125           $Fine = *on;
035300100125           leavesr;
035400100125         endif;
035500100125
035600100125       ENDSR;
035700100125
035800100125       //--------------------------------------------------------------
035900100125       //?Controllo dati in videata D01                                ?
036000100125       //--------------------------------------------------------------
036100100125       BEGSR sr_CtrD01;
036200100125
036300100125         %subst(IndDspF : 40) = *off;
036400100125
036500100125       ENDSR;
036600100125
036700100125      /end-free
