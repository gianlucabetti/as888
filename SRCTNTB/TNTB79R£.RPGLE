000100100303       //==============================================================
000200100205       //?Gestione tabella "3EW" (Dati EasyWEB per "postazione")       ?
000300100303       //==============================================================
000400100303
000500100303       //--------------------------------------------------------------
000600100303       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100303       //--------------------------------------------------------------
000800100303
000900111024     /*PRM  dbgview(*source)
001000111024     /*CMD  ovrdbf file(FNBLP00F) tofile(FILTRAPRD/FNBLP00F) +
001100111024     /*CMD         ovrscope(*calllvl)
001200111024     /*END  dltovr file(FNBLP00F) lvl(*)
001300100303     /*END
001400100303
001500100303       //--------------------------------------------------------------
001600100303       //?Specifiche di controllo.                                     ?
001700100303       //--------------------------------------------------------------
001800100205
001900100205     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000100330     h dftactgrp(*no)
002100101125     h bnddir('UBBNDDIR':'TRUL')
002200100205
002300100205       //--------------------------------------------------------------
002400100205       //?Dichiarazione file.                                          ?
002500100205       //--------------------------------------------------------------
002600100205
002700100205       // -?Organigramma?
002800100210     fAZORG01L  if   e           k disk
002900121102
003000121102       // -?Profili utente?
003100121102     fKPPRF01L  if   e           k disk    usropn
003200100205
003300100205       // -?Tabelle?
003400100518     fTABEL00F  Uf   e           k disk    extfile(wLibFile1)
003500100518     f                                     usropn
003600100205     fTNTBE01L  Uf A e           k disk    extfile(wLibFile)
003700100205     f                                     usropn
003800100205
003900100205       // -?Video?
004000100205     fTNTB79D   cf   e             workstn
004100100205     f                                     indds(IndDspF)
004200100205     f                                     infds(InfDspF)
004300100205
004400100205       //--------------------------------------------------------------
004500100205       //?Definizione costanti.                                        ?
004600100205       //--------------------------------------------------------------
004700100210
004800100210       // -?Costante codice trattamento merce x supporto "EasyWEB"?
004900100210     d c_CTMxEasyWeb   c                   const('2 ')
005000100205
005100100205       // -?Codice tabella in gestione?
005200100205     d c_Tab           c                   const('3EW')
005300100210
005400100210       // -?Codice utente x TABEL00F?
005500100210     d c_Kut           c                   const(1)
005600100205
005700100205       // -?Tasti funzionali a video?
005800100205     d c_F01           c                   const(x'31')
005900100205     d c_F02           c                   const(x'32')
006000100205     d c_F03           c                   const(x'33')
006100100205     d c_F04           c                   const(x'34')
006200100205     d c_F05           c                   const(x'35')
006300100205     d c_F06           c                   const(x'36')
006400100205     d c_F07           c                   const(x'37')
006500100205     d c_F08           c                   const(x'38')
006600100205     d c_F09           c                   const(x'39')
006700100205     d c_F10           c                   const(x'3A')
006800100205     d c_F11           c                   const(x'3B')
006900100205     d c_F12           c                   const(x'3C')
007000100205     d c_F13           c                   const(x'B1')
007100100205     d c_F14           c                   const(x'B2')
007200100205     d c_F15           c                   const(x'B3')
007300100205     d c_F16           c                   const(x'B4')
007400100205     d c_F17           c                   const(x'B5')
007500100205     d c_F18           c                   const(x'B6')
007600100205     d c_F19           c                   const(x'B7')
007700100205     d c_F20           c                   const(x'B8')
007800100205     d c_F21           c                   const(x'B9')
007900100205     d c_F22           c                   const(x'BA')
008000100205     d c_F23           c                   const(x'BB')
008100100205     d c_F24           c                   const(x'BC')
008200100205     d c_Enter         c                   const(x'F1')
008300100205     d c_RollDown      c                   const(x'F4')
008400100205     d c_RollUp        c                   const(x'F5')
008500100209
008600100209       // -?Costanti per la definizione delle schiere con i nomi?
008700100209       //  ?degli iSeries da elaborare e delle relative librerie?
008800100209     d c_NrSyst        c                   const(2)
008900100209     d c_NrLibr        c                   const(2)
009000100209
009100100209       // -?Costante per controllo "caratteri solo numerici"?
009200100209     d c_Digits        c                   const('0123456789')
009300110926
009400110926       // -?Supporto Cliente a Bartolini "EasySpedWEB"?
009500110926     d c_Supp_ESWEB    c                   const('ESWEB')
009600100205
009700100205       //--------------------------------------------------------------
009800100205       //?Definizione schiere.                                         ?
009900100205       //--------------------------------------------------------------
010000100209
010100100209       // -?iSeries  &  Librerie con entrambi i file tabelle?
010200100209     d $iSystem        s                   like(currSysNetA)
010300100209     d                                     dim(c_NrSyst)
010400100209     d                                     ctdata   perrcd( 1)
010500100209     d $Libraries      s                   like(ds_Libr)
010600100209     d                                     dim(c_NrSyst)
010700100209     d                                     alt($iSystem)
010800100205
010900100205       // -?Messaggi di errore?
011000150305     d $Msg            s             78    dim(38)  ctdata  perrcd( 1)
011100100205
011200100205       //--------------------------------------------------------------
011300100205       //?Definizione aree dati.                                       ?
011400100205       //--------------------------------------------------------------
011500100205
011600100205       // -?Dati utente?
011700100205     d §AzUte        e ds                  extname(AZUTE00F)
011800100205     d                                     dtaara
011900100205     d §DatiUte      e ds                  extname(dDatiUte)
012000100205     d                                     dtaara
012100100205
012200100205       //--------------------------------------------------------------
012300100205       //?Definizione strutture dati.                                  ?
012400100205       //--------------------------------------------------------------
012500100205
012600100205       // -?Status ds?
012700100205     d Status         sds
012800100205     d  SDSpgm           *proc
012900100205
013000100205       // -?InfDS?
013100100205     d InfDspF         ds
013200100205     d   dsp_aid             369    369a
013300100205
013400100205       // -?Indicatori su DspF?
013500100205     d IndDspF         ds                  inz
013600100205         // -?Abilitazione tasti funzionali?
013700100210     d   F3Attivo                      n   overlay(IndDspF : 03)
013800100210     d   F4Attivo                      n   overlay(IndDspF : 04)
013900100205     d   F5Attivo                      n   overlay(IndDspF : 05)
014000100205     d   F6Attivo                      n   overlay(IndDspF : 06)
014100100211     d   F8Attivo                      n   overlay(IndDspF : 08)
014200110930     d   F9Attivo                      n   overlay(IndDspF : 09)
014300100210     d   F10Attivo                     n   overlay(IndDspF : 10)
014400100210     d   F11Attivo                     n   overlay(IndDspF : 11)
014500100205     d   F12Attivo                     n   overlay(IndDspF : 12)
014600121102     d   F13Attivo                     n   overlay(IndDspF : 13)
014700111025     d   F14Attivo                     n   overlay(IndDspF : 14)
014800120202     d   F15Attivo                     n   overlay(IndDspF : 15)
014900100205     d   F16Attivo                     n   overlay(IndDspF : 16)
015000130827     d   F17Attivo                     n   overlay(IndDspF : 17)
015100100205         // -?Emissione messaggio di errore?
015200100205     d   ErrMessage                    n   overlay(IndDspF : 28)
015300120202         // -?Indicatori per Attibuti di visualizzazione?
015400120202     d   VisBloAnn                     n   overlay(IndDspF : 40)
015500120202     d   $FlsNrsIn3CL                  n   overlay(IndDspF : 41)
015600120814     d   ProtAnnAppl                   n   overlay(IndDspF : 42)
015700100205         // -?Posizionamento cursore & segnalazione errore?
015800100205     d   PosCurKSU                     n   overlay(IndDspF : 51)
015900100205     d   PosCurSUN                     n   overlay(IndDspF : 52)
016000100302     d   PosCurLNP                     n   overlay(IndDspF : 53)
016100100302     d   PosCurFLS                     n   overlay(IndDspF : 54)
016200100302     d   PosCurNRS                     n   overlay(IndDspF : 55)
016300100302     d   PosCurNSI                     n   overlay(IndDspF : 56)
016400100302     d   PosCurCTM                     n   overlay(IndDspF : 57)
016500100302     d   PosCurMAD                     n   overlay(IndDspF : 58)
016600100302     d   PosCurAEW                     n   overlay(IndDspF : 59)
016700120814     d   PosCurFAA                     n   overlay(IndDspF : 61)
016800120814     d   PosCurDAA                     n   overlay(IndDspF : 62)
016900120814     d   PosCurD3ncl                   n   overlay(IndDspF : 65)
017000120814     d   PosCurD3nrs                   n   overlay(IndDspF : 66)
017100140123     d   PosCurD3ncd                   n   overlay(IndDspF : 67)
017200120814     d   PosCurD3nca                   n   overlay(IndDspF : 68)
017300100205         //   -?Riemissione videata?
017400100205     d   ErrGenerico                   n   overlay(IndDspF : 99)
017500100209
017600100209       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
017700100209     d ds_Libr         ds                  inz
017800100209     d  $Libr                        10    dim(c_NrLibr) inz
017900100205
018000100205       // -?Parametri ricevuti?
018100100205     d KPJBA         e ds
018200100211     d TNTB79ds      e ds                  inz
018300100205
018400100205       // -?Parametri per Reperimento dati utente?
018500100205     d TIBS34ds      e ds
018600100205
018700100205       // -?Dati record principale della tabella?
018800100205     d TNTBEds       e ds                  inz  extname(TNTBE00F)
018900100205     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
019000100205     d                                          prefix(TBX:3)
019100100209
019200100209       // -?Tabelle usate:?
019300100209       // ·?"1B" = Codici Trattamento Merce?
019400100209     d ds1B          e ds                  inz
019500100518       // ·?"3C" = Clienti con dischetto (che inviano bolle in partenza)?
019600110422     d ds3C          e ds                  inz  qualified
019700110930       // ·?"3CE" = Versioni Easy-Sped per cliente?
019800110930     d d3CE          e ds                  inz  qualified
019900110422       // -?"3CP" = Serie parziali assegnate al cliente?
020000110422     d d3CP          e ds                  inz  qualified
020100100414       // -?"3EW" = Dati EasyWeb per "postazione"?
020200100414     d d3EW          e ds                  inz  qualified
020300100205
020400100205       //--------------------------------------------------------------
020500100205       //?Definizione variabili globali.                               ?
020600100205       //--------------------------------------------------------------
020700100205
020800100205       // -?Parametri di input facoltativi?
020900100205     d p_TNTB79ds      s                   like(TNTB79ds)
021000100205
021100100205       // -?Flags booleani?
021200100205     d $Fine           s               n   inz
021300100205     d $InzD01         s               n   inz(*on)
021400100205     d $InzD02         s               n   inz(*on)
021500100205     d $InzD03         s               n   inz(*on)
021600110930     d $InzW01         s               n   inz(*on)
021700111018     d $InzW02         s               n   inz(*on)
021800121102     d $InzW03         s               n   inz(*on)
021900100209     d $Protect        s               n   inz
022000100218     d $MsgMAD         s               n   inz
022100110927     d SaveErrMessage  s               n   inz
022200110927     d SaveErrGeneric  s               n   inz
022300140718     d $CtrlRngSgnCl   s               n   inz
022400100205
022500100205       // -?Indici di schiera?
022600100210     d xx              s              3  0 inz
022700100205
022800100205       // -?Variabili per la gestione del video?
022900100205     d $Video          s              2    inz('D1')
023000100209
023100100209       // -?Memo per controllo variazioni a video?
023200101125     d SaveNRS         s                   like(V1Cnrs)  inz
023300121127     d SavNRS          s                   like(V3Cnrs)
023400121127     d SavNCD          s                   like(V3Cncd)
023500121127     d SavNCA          s                   like(V3Cnca)
023600150305     d Save_mad        s                   like(V1Cmad)  inz
023700100209
023800100209       // -?Nome del sistema?
023900100209     d currSysNeta     s              8a   inz
024000100209
024100100209       // -?Nome esteso Libreria/File dei 2 file tabella?
024200100209     d wLibFile        s             21a   inz
024300100518     d wLibFile1       s             21a   inz
024400100205
024500100205       // -?Campi di comodo?
024600100209     d w_iSystem       s              1  0 inz
024700100209     d w_SisInf        s              3  0 inz
024800100210     d wDate           s              8  0 inz
024900100205     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
025000130827     d wDate_ISO       s               d   datfmt(*iso)   inz(*loval)
025100140718     d*// wV3Cncx         s                   like(V3Cncd)  inz
025200140718     d wCncA           s              8  0 inz
025300110422
025400110422       // -?Nr. segnacollo iniziale (da tab."3CP")?
025500110422     d wNSI            s                   like(d3CP.§3CPal)  inz
025600100205
025700100205       //--------------------------------------------------------------
025800100205       //?Definizione prototipi procedure.                             ?
025900100205       //--------------------------------------------------------------
026000100209
026100100209       // -?Reperimento NETA sistema AS/400 corrente?
026200100210      /copy gaitrasrc/srcProtoPR,UBRTVNETA
026300100210
026400100210       // -?Reperimento legami in tab. "3C"?
026500100210      /copy gaitrasrc/srcProtoPI,UBLEG3C
026600100210      /copy gaitrasrc/srcProtoPR,UBLEG3C
026700110422
026800110422       // -?Reperimento n° serie & range segnacolli?
026900110422      /copy gaitrasrc/srcProtoPI,UBNRSRSC
027000110422      /copy gaitrasrc/srcProtoPR,UBNRSRSC
027100111018
027200111018       // -?Reperimento ultimo segnacollo utilizzato?
027300111018      /copy gaitrasrc/srcProtoPI,ubLastNSC
027400111018      /copy gaitrasrc/srcProtoPR,ubLastNSC
027500100205
027600100205       // -?Reperimento dati utente?
027700100205      /copy gaitrasrc/srcProtoPR,TIBS34R
027800100209
027900100209       // -?Ricerca filiale in organigramma?
028000100209      /copy gaitrasrc/srcProtoPI,TNSD24R
028100100209      /copy gaitrasrc/srcProtoPR,TNSD24R
028200100209
028300120202       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
028400120202     d c_TABEL         c                   const('1')
028500120202       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
028600120202     d c_TNTBE         c                   const('2')
028700100209       // -?Reperimento dati tabelle?
028800100209      /copy gaitrasrc/srcProtoPR,TRULTAB
028900100210
029000100210       // -?Ricerca vecchie tabelle (TABEL00F)?
029100100210      /copy gaitrasrc/srcProtoPI,X§TABER
029200100210     d TABELds       e ds                  extname(TABEL00F)   inz
029300100210      /copy gaitrasrc/srcProtoPR,X§TABER
029400100205
029500100205       // -?Controllo/Decodifica cliente?
029600100217      /copy gaitrasrc/srcProtoPI,TIBS69R
029700100205      /copy gaitrasrc/srcProtoPR,TIBS69R
029800100210
029900100210       // -?Selezione cliente da tab. "3EW"?
030000100211      /copy gaitrasrc/srcProtoPI,TNTB79R1
030100100210      /copy gaitrasrc/srcProtoPR,TNTB79R1
030200100208
030300100218       // -?Gestione clienti abilitati a STRATEGI?
030400100218     d TIS720ds      e ds                  inz
030500100218     d   S20op0      e                     inz('P01')
030600100218     d   S20f03      e                     inz(*off)
030700100218     d   S20f12      e                     inz(*off)
030800100218     d   S20err      e                     inz(*off)
030900100208      /copy gaitrasrc/srcProtoPR,TIS724R
031000100205
031100100205       // -?Verifica abilitazione STRATEGI?
031200100205      /copy gaitrasrc/srcProtoPI,TIS7701R
031300120725     d SavRpyEsito     s                   like(rpyEsito)    inz
031400100205     d TIVSSds       e ds                  extname(TIVSS00F) qualified
031500120725     d                                     inz
031600120725     d SavTIVSSds    e ds                  extname(TIVSS00F) qualified
031700120725     d                                     inz
031800100205      /copy gaitrasrc/srcProtoPR,TIS7701R
031900100205
032000100218       // -?Attribuzione/Verifica numero serie libero per filiale?
032100100211      /copy gaitrasrc/srcProtoPI,TRTB28R2
032200100210      /copy gaitrasrc/srcProtoPR,TRTB28R2
032300100205
032400100218       // -?Attribuzione/Verifica numero serie parziale libero per fil.?
032500100218     d TNTB54ds      e ds                  inz
032600100205      /copy gaitrasrc/srcProtoPR,TNTB54R
032700100211
032800100211       // -?Interrogazione legami tabelle VAS?
032900100218     d TRTB06ds      e ds                  inz
033000100211     d trtb06r         pr                  extpgm('TRTB06R')
033100100211     d   kpjba                             likeds(KPJBA)
033200120814
033300120814       // -?Controllo/Inversione data?
033400120814     d WLBdat          ds                  inz
033500120814     d  G08dat                        8  0 inz
033600120814     d  G08inv                        8  0 inz
033700120814     d  G08err                        1    inz('3')
033800120814     d  G08tgi                        5  0 inz
033900120814      /copy gaitrasrc/srcProtoPR,XSRDA8
034000100205
034100100205       //--------------------------------------------------------------
034200100205       //?Definizione key-list.                                        ?
034300100205       //--------------------------------------------------------------
034400100205
034500100205       // -?File TNTBE01L?
034600100205     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
034700100205     d                                     prefix(k_)   inz
034800100518
034900100518       // -?File TABEL00F?
035000100518     d k03tabel00    e ds                  extname(TABEL00F : *key)
035100100518     d                                     prefix(k_)   inz
035200100205
035300100205       //--------------------------------------------------------------
035400100205       //?M A I N - L I N E                                            ?
035500100205       //--------------------------------------------------------------
035600100205
035700100205     c     *Entry        plist
035800100205     c                   parm                    KPJBA
035900100205     c                   parm                    p_TNTB79ds
036000100205
036100100205      /free
036200100205
036300100205       // -?Operazioni iniziali?
036400100205       exsr  sr_RoutInz;
036500100205
036600100205       // -?Ciclo di gestione del file video?
036700100205       DOW  $Fine = *off;
036800100205
036900100205         select;
037000100205
037100100205           // -?Richiesta chiave di accesso?
037200100205           when $Video = 'D1';
037300100205             exsr  sr_GesD01;
037400100205
037500100205           // -?Manutenzione dati della singola tabella?
037600100205           when $Video = 'D2';
037700100205             exsr  sr_GesD02;
037800100205
037900100205           // -?Ricerca serie segnacolli parziale?
038000100205           when $Video = 'D3';
038100100205             exsr  sr_GesD03;
038200110930
038300110930           // -?Interrogazione dati EasySped?
038400110930           when $Video = 'W1';
038500110930             exsr  sr_GesW01;
038600111018
038700111018           // -?Reperimento ultimo segnacollo?
038800111018           when $Video = 'W2';
038900111018             exsr  sr_GesW02;
039000121102
039100121102           // -?Visualizzazione profilo utente ultima modifica?
039200121102           when $Video = 'W3';
039300121102             exsr  sr_GesW03;
039400100205
039500100205           // -?? ? ??
039600100205           other;
039700100205             $Fine = *on;
039800100205
039900100205         endsl;
040000100205
040100100205       ENDDO;
040200100205
040300100205       // -?Operazioni finali?
040400100205       exsr  sr_RoutEnd;
040500100205
040600100205       //--------------------------------------------------------------
040700100205       //?Operazioni iniziali.                                         ?
040800100205       //--------------------------------------------------------------
040900100205       BEGSR  *InzSr;
041000100205
041100100205         // -?Reperimento dati job?
041200100205         exsr  sr_DatiJob;
041300100205
041400100205         // -?Impostazione nome programma a video?
041500100205         V1Tpgm = SDSpgm;
041600100210
041700100210         // -?Reperimento data odierna?
041800100210         wDate = %int( %subst( %char( %dec( %timestamp() ) )
041900100210                               : 1 : 8 ) );
042000100205
042100100205       ENDSR;
042200100205
042300100205       //--------------------------------------------------------------
042400100205       //?Operazioni iniziali.                                         ?
042500100205       //--------------------------------------------------------------
042600100205       BEGSR  sr_RoutInz;
042700100205
042800100205         // -?Verifica se ricevuti parametri?
042900100205         if  %parms() > 1;
043000100205           TNTB79ds = p_TNTB79ds;
043100100205         else;
043200100205           clear  TNTB79ds;
043300100205         endif;
043400100205
043500100205         // -?Impostazione iniziale parametri di output?
043600100211         clear  o3EWuni;
043700100211         clear  o3EWfxx;
043800100211         o3EWerr = *off;
043900100211         clear  o3EWmsg;
044000100205
044100100205         // -?Impostazione chiusura?
044200100205         select;
044300100211           when  %parms() > 1   and   i3EWtla = *blank;
044400100205             *inRT = *on;
044500100211           when  %parms() > 1   and   i3EWtla = 'L';
044600100205             *inLR = *on;
044700100211           when  %parms() > 1   and   i3EWtla = 'C';
044800100205             *inLR = *on;
044900100205             $Fine = *on;
045000100205             leavesr;
045100100205           other;
045200100205             *inLR = *on;
045300100205         endsl;
045400100209
045500100209         // -?Verifica del sistema AS/400 corrente?
045600100209         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
045700100209           $Fine = *on;
045800100209           leavesr;
045900100209         endif;
046000100209
046100100209         // -?Impostazione elenco librerie in cui gestire le tabelle?
046200100209         //  ?(a seconda del sistema in cui si stà lavorando)?
046300100209         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
046400100209         if  w_iSystem = *zero;
046500100209           $Fine = *on;
046600100209           leavesr;
046700100209         endif;
046800100210
046900100210         // -?1ª apertura file TABEL00F del 1° S.I. (sede)?
047000100210         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
047100100210           w_SisInf = 1;
047200100210           exsr  sr_OpenFileTab;
047300100210         endif;
047400100205
047500100205         // -?Aggancio dati generali della tabella in esame?
047600100205         clear  k_TBEcod;
047700100205         k_TBEke1 = *zero;
047800100205         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
047900100205                = c_Tab;
048000100205         clear  k_TBEke2;
048100100205         clear  k_TBElin;
048200100205         k_TBEsif = KNSIF;
048300100205         chain(n)  %kds(k05tntbe01)  TNTBE000;
048400100205         if  not  %found(TNTBE01L);
048500100205           clear  k_TBEsif;
048600100205           chain(n)  %kds(k05tntbe01)  TNTBE000;
048700100205         endif;
048800100205         if  %found(TNTBE01L);
048900100205           xTNTBEds = TNTBEds;
049000100205         else;
049100100205           clear  xTNTBEds;
049200100205         endif;
049300100205
049400100205         // -?Impostazione iniziale / pulizia dei campi chiave?
049500100205         k_TBEcod = c_Tab;
049600100205         clear  k_TBEke1;
049700100205         clear  k_TBEke2;
049800100205         clear  k_TBElin;
049900100205         clear  k_TBEsif;
050000100205
050100100205         // -?Se ricevuti parametri: inizializzazione della videata D01?
050200100205         //                         ?e  passaggio alla videata D02?
050300100205         if  %parms() > 1;
050400100205           exsr  sr_InzD01;
050500100205           $InzD01 = *off;
050600110926           //exsr  sr_CtrD01;      ?(già eseguita in sr_InzD01)?
050700100208           if  Not  ErrMessage;
050800100208             $InzD02 = *on;
050900100208             $Video  = 'D2';
051000110926           //else;
051100110926           //  o3EWerr = *on;      ?(già eseguito in sr_InzD01)?
051200110926           //  o3EWmsg = V1Dmsg;   ?(già eseguito in sr_InzD01)?
051300110926           //  $Fine   = *on;      ?(già eseguito in sr_InzD01)?
051400100208           endif;
051500100205         endif;
051600100205
051700100205       ENDSR;
051800100205
051900100205       //--------------------------------------------------------------
052000100205       //?Reperimento Dati del job (Utente/Operativi).                 ?
052100100205       //--------------------------------------------------------------
052200100205       BEGSR  sr_DatiJob;
052300100205
052400100205         in(E) §AzUte;
052500100205         if NOT %error;
052600100205           in(E) §DatiUte;
052700100205         endif;
052800100205         if %error or RSut = *blanks;
052900100205           clear TIBS34ds;
053000100205           tibs34r ( tibs34ds );
053100100205           in §AzUte;
053200100205           in §DatiUte;
053300100205         endif;
053400100205
053500100205       ENDSR;
053600100205
053700100205       //--------------------------------------------------------------
053800100518       //?Gestione videata D01.                                        ?
053900100205       //--------------------------------------------------------------
054000100205       BEGSR  sr_GesD01;
054100100205
054200100205         // -?Inizializzazione videata?
054300100205         if  $InzD01 = *on;
054400100205           exsr  sr_InzD01;
054500100205           $InzD01 = *off;
054600100205         endif;
054700100312         clear  V1Topz;
054800100312         clear  V1Tblo;
054900120202         VisBloAnn = *off;
055000100205
055100100205         // -?(Dis)Abilitazione tasti funzionali?
055200100205         F5Attivo  = *off;
055300100205         F6Attivo  = *off;
055400100205         F12Attivo = *off;
055500100205         F16Attivo = *off;
055600130827         F17Attivo = *off;
055700100205
055800100205         // -?Emissione videata?
055900100205         write  TB79T01;
056000100210         write  TB79P01;
056100100205         exfmt  TB79D01;
056200100205
056300100205         clear  V1Dmsg;
056400100205         reset  ErrMessage;
056500100205         reset  ErrGenerico;
056600100205
056700100205         SELECT;
056800100205
056900100205           // -?F3=Fine?
057000100205           WHEN  dsp_aid = c_F03;
057100100205             $Fine = *on;
057200100208
057300100208           // -?F4=Ricerca cliente STRATEGI?
057400100208           WHEN  dsp_aid = c_F04;
057500100208             exsr  sr_CallTIS724;
057600100205
057700100205           // -?Invio?
057800100205           OTHER;
057900100205             exsr  sr_CtrD01;
058000100205             if  ErrGenerico = *off;
058100100205               $Video    = 'D2';
058200100205               $InzD02   = *on;
058300100205             endif;
058400100205
058500100205         ENDSL;
058600100205
058700100205       ENDSR;
058800100205
058900100205       //--------------------------------------------------------------
059000100518       //?Inizializzazione videata D01.                                ?
059100100205       //--------------------------------------------------------------
059200100205       BEGSR  sr_InzD01;
059300100205
059400100205         clear  TB79D01;
059500100205
059600100209         if  %parms() > 1;
059700100205
059800100211           V1Cksu = i3EWksu;
059900100211           V1Csun = i3EWsun;
060000100205           exsr  sr_CtrD01;
060100100205           if  ErrGenerico;
060200100211             o3EWerr = *on;
060300100211             o3EWmsg = V1Dmsg;
060400100205             exsr  sr_RoutEnd;
060500100205           endif;
060600100209
060700100209         else;
060800100209
060900100209           V1Cksu = '?';
061000100205
061100100205         endif;
061200100210
061300100210         // -?Impostazione indicatori per abilitazione tasti funzionali?
061400100210         exsr  sr_AbilitFxxD01;
061500100205
061600100205       ENDSR;
061700100210
061800100210       //--------------------------------------------------------------
061900100518       //?Abilitazione condizionata dei tasti funzionali per D01.      ?
062000100210       //--------------------------------------------------------------
062100100210       BEGSR  sr_AbilitFxxD01;
062200100210
062300100210         // -?F4=Interrogazione clienti STRATEGI?
062400100210         F4Attivo = ( %subst( KNSIF : 1 : 6 ) = 'GAITRA' );
062500100210
062600100210       ENDSR;
062700100205
062800100205       //--------------------------------------------------------------
062900100518       //?Controllo dati immessi nella videata D01.                    ?
063000100205       //--------------------------------------------------------------
063100100205       BEGSR  sr_CtrD01;
063200100205
063300100205         // -?Spegnimento indicatori di posizionamento cursore?
063400100205         %subst(IndDspF : 28) = *off;
063500100209
063600120814         reset  $Protect;
063700100205
063800100205         SELECT;
063900100205
064000100205           // -?Ricerca dati già inseriti?
064100100205           WHEN  %scan('?' : V1Cksu) > *zero;
064200100205             clear  V1Cksu;
064300100330             clear  V1Csun;
064400100211             clear  TNTB79ds1;
064500100211             tntb79r1 ( kpjba : TNTB79ds1 );
064600100211             if  TNTB79ds1.o3EWfxx = *blank;
064700100211               V1Cksu = TNTB79ds1.o3EWksu;
064800100211               V1Csun = TNTB79ds1.o3EWsun;
064900100211               //d3EW = TNTB79ds1.o3EWuni;
065000100205             endif;
065100100205             errGenerico = *on;
065200100210             PosCurKSU   = *on;
065300100205             leavesr;
065400100205
065500100205           // -?Controllo immissione codice completo?
065600100205           WHEN  V1Cksu = *blank   or   V1Csun = *blank;
065700100205             ErrMessage  = *on;
065800100205             ErrGenerico = *on;
065900100210             PosCurKSU   = *on;
066000100205             V1Dmsg = $Msg(01);
066100100205             leavesr;
066200100205
066300100205         ENDSL;
066400100205
066500110422         // -?Decodifica cliente (KSU)?
066600100205         clear  TIBS69ds;
066700100205         I69sif = knsif;
066800100205         I69kcc = DUTkci;
066900100210         I69kac = %int( %subst( V1Cksu : 2 ) );
067000100205         tibs69r( tibs69ds :
067100100205                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
067200100205         if  O69err = *on;
067300100205           V1Dmsg = $Msg(02);
067400100210           PosCurKSU   = *on;
067500100205           ErrMessage  = *on;
067600100205           ErrGenerico = *on;
067700100205           leavesr;
067800100205         endif;
067900100210         V1Dksu = %subst( ACOrag : 1 : %len(V1Dksu) );
068000121205
068100121205         // -?Reperimento tab. "3C"?
068200121205         //  ?(per verificarne supporto Cli=>Brt e padre)?
068300130827         clear  ds3C;
068400121205         ds_TNTBE.TBEke1 = %subst( V1Cksu : 2 );
068500121205         if  getTabella ( c_Tabel : '3C'  : ds_TNTBE.TBEke1 :
068600121205                          *omit : *omit : *omit :
068700121205                          *omit : *omit :
068800121205                          *omit : *omit : *omit : *omit :
068900121205                          *omit : *omit : *omit : *omit :
069000121205                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
069100121205                        = *zero      AND
069200121205             ds_TNTBE.TBEatb = *blank;
069300121205           ds3C   = ds_TNTBE.TBEuni;
069400121205         endif;
069500130827
069600130827         // -?Reperimento tab. "3EW"?
069700130827         //  ?(per verificarne l'eventuale annullamento applicativo)?
069800130827         clear  d3EW;
069900130827         if  getTabella ( c_Tntbe : '3EW' : V1Cksu : V1Csun :
070000130827                          *omit : *omit :
070100130827                          *omit : *omit :
070200130827                          *omit : *omit : *omit : *omit :
070300130827                          *omit : *omit : *omit : *omit :
070400130827                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
070500130827                          = *zero      AND
070600130827               ds_TNTBE.TBEatb = *blank;
070700130827           d3EW = ds_TNTBE.TBEuni;
070800130827         endif;
070900100205
071000100205         // -?Controllo abilitazione a STRATEGI del cliente "PADRE"?
071100100205         clear  rpyEsito;
071200130903         Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : V1Cksu : 'EW' :
071300130903                                  *blank : *zero : TIVSSds :
071400130903                                  rpyEsito );
071500130903         if  (rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
071600130903                                  or   TIVSSds.VSSksu = *zero)
071700130903                                 AND   d3EW.§3EWdaa   > *zero;
071800130827           wDate_Iso = %date( %int(d3EW.§3EWdaa) : *iso ) - %days(1);
071900130827           Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : V1Cksu : 'EW' :
072000130827                                    *blank : %dec(wDate_Iso) : TIVSSds :
072100130827                                    rpyEsito );
072200130827         endif;
072300120725         SavTIVSSds  = TIVSSds;
072400120725         SavRpyEsito = rpyEsito;
072500100210         select;
072600110926           when  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
072700110926                                     or   TIVSSds.VSSksu = *zero;
072800121205             if  ds3C.§3Ccba = c_Supp_ESWEB  and
072900121205                 ds3C.§3Ccks = %int( %subst( V1Cksu : 2 ) );
073000121205               V1Dmsg = %trimr( $Msg(05) ) +
073100121205                        ': modificare cliente in tab. "3C"';
073200121205               PosCurKSU   = *on;
073300121205               ErrMessage  = *on;
073400121205               ErrGenerico = *on;
073500121205               leavesr;
073600121205             endif;
073700110926           //when  TIVSSds.VSSdsc < wDate;
073800110926           //  V1Dmsg = $Msg(06);
073900110926           //  PosCurKSU   = *on;
074000110926           //  ErrMessage  = *on;
074100110926           //  ErrGenerico = *on;
074200110926           //  leavesr;
074300100210           //when  TIVSSds.VSSdde > wDate;
074400100210           //  V1Dmsg = $Msg(07);
074500100210           //  PosCurKSU   = *on;
074600100210           //  ErrMessage  = *on;
074700100210           //  ErrGenerico = *on;
074800100210           //  leavesr;
074900100210         endsl;
075000120725
075100120725         // -?Controllo abilitazione a STRATEGI del cliente "PADRE"?
075200120725         //  ?(con Strategi User Number)?
075300120725         if  V1Csun <> SavTIVSSds.VSSsun;
075400120725           clear  rpyEsito;
075500120725           Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : V1Cksu : 'EW' :
075600120725                                    V1Csun : *zero : TIVSSds :
075700120725                                    rpyEsito );
075800120725         endif;
075900100210
076000100210         // -?Apertura file TABEL00F del 1° S.I. (sede)?
076100100210         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
076200100210           w_SisInf = 1;
076300100210           exsr  sr_OpenFileTab;
076400100210         endif;
076500100210
076600100210         // -?Verifica esistenza del record in TNTBE?
076700100210         k_TBEke1 = V1Cksu;
076800100210         k_TBEke2 = V1Csun;
076900100211         setll  %kds(k05tntbe01)  TNTBE000;
077000100210
077100100210         // -?Il *pgm, da menù (con solo KPJBA come parm), può solo?
077200100210         //  ?manutenzionare - NON inserire?
077300100211         if  NOT %equal(TNTBE01L)   and   %parms() = 1;
077400100210           V1Dmsg = $Msg(03);
077500100210           PosCurKSU   = *on;
077600100210           ErrMessage  = *on;
077700100210           ErrGenerico = *on;
077800100210           leavesr;
077900100210         endif;
078000110422
078100110422         // -?Controllo cliente (KSU)?
078200110422         select;
078300110422           // -?Cliente annullato?
078400110422           when  ACOflg <> *blank   and   NOT %equal(TNTBE01L);
078500110422             V1Dmsg = $Msg(04);
078600110422             PosCurKSU   = *on;
078700110422             ErrMessage  = *on;
078800110422             ErrGenerico = *on;
078900110422             leavesr;
079000110422           // -?Cliente bloccato?
079100130315           when  ACOabl <> *blanks  and   NOT %equal(TNTBE01L);
079200110422             V1Dmsg = $Msg(04);
079300110422             PosCurKSU   = *on;
079400110422             ErrMessage  = *on;
079500110422             ErrGenerico = *on;
079600110422             leavesr;
079700110422         endsl;
079800100205
079900100205       ENDSR;
080000100208
080100100208       //--------------------------------------------------------------
080200100208       //?Chiamata alla gestione dei clienti abilitati ai servizi      ?
080300100518       //?  STRATEGI.                                                  ?
080400100208       //--------------------------------------------------------------
080500100208       BEGSR  sr_CallTIS724;
080600100208
080700100208         reset  TIS720ds;
080800100208         kpjbu = TIS720ds;
080900100208
081000100208         tis724r ( kpjba );
081100100208
081200100208         TIS720ds = kpjbu;
081300100208         if  S20f03 = *off   and   S20f12 = *off;
081400100208           V1Cksu = S20ksc;
081500100208           V1Csun = S20sun;
081600100208         endif;
081700100208
081800100208       ENDSR;
081900100205
082000100205       //--------------------------------------------------------------
082100100205       //?Apertura dei files tabelle nel sistema informativo impostato.?
082200100205       //--------------------------------------------------------------
082300100205       BEGSR  sr_OpenFileTab;
082400100205
082500100205         // -?Chiusura (eventuale) file tabelle?
082600100205         if  %open(TNTBE01L);
082700100205           close  TNTBE01L;
082800100205         endif;
082900100518         if  %open(TABEL00F);
083000100518           close  TABEL00F;
083100100518         endif;
083200100205
083300100205         // -?Apertura file tabelle?
083400100210         ds_Libr  = $Libraries(w_iSystem);
083500100205         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
083600100205         open  TNTBE01L;
083700100518         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
083800100518         open  TABEL00F;
083900100205
084000100205       ENDSR;
084100100205
084200100205       //--------------------------------------------------------------
084300100518       //?Gestione videata D02.                                        ?
084400100205       //--------------------------------------------------------------
084500100205       BEGSR  sr_GesD02;
084600100205
084700100205         // -?Inizializzazione videata?
084800100205         if  $InzD02 = *on;
084900100205           exsr  sr_InzD02;
085000100205           $InzD02 = *off;
085100100205         endif;
085200120816
085300120816         // -?Impostazione indicatori per abilitazione tasti funzionali?
085400120816         exsr  sr_AbilitFxxD02;
085500100205
085600100205         // -?Emissione Testata e Piede con tasti funzionali abilitati?
085700110927         SaveErrMessage = ErrMessage;
085800110927         SaveErrGeneric = ErrGenerico;
085900110927         clear  ErrMessage;
086000110927         clear  ErrGenerico;
086100110927         write  TB79T01;
086200110927         write  TB79D01;
086300100210         write  TB79P02;
086400100210         write  Protect;
086500100205
086600100205         // -?Emissione videata?
086700100209         if  Not $Protect;
086800110927           ErrMessage  = SaveErrMessage;
086900110927           ErrGenerico = SaveErrGeneric;
087000100209           exfmt  TB79D02;
087100100209         else;
087200100209           write  TB79D02;
087300110927           ErrMessage  = SaveErrMessage;
087400110927           ErrGenerico = SaveErrGeneric;
087500100209           exfmt  Protect;
087600100209         endif;
087700100205
087800110926         clear  V1Dmsg;
087900110926         reset  ErrMessage;
088000110926         reset  ErrGenerico;
088100100205
088200100205         SELECT;
088300100205
088400100205           // -?F3=Fine?
088500100205           WHEN  dsp_aid = c_F03;
088600100205             unlock TNTBE01L;
088700100205             $Fine = *on;
088800100211
088900110930           // -?F8=Interrogazione legami tabelle VAS?
089000100211           WHEN  dsp_aid = c_F08;
089100100211             exsr  sr_F08D02;
089200110930
089300110930           // -?F9=Interrogazione informazioni EasySped?
089400110930           WHEN  dsp_aid = c_F09;
089500110930             $Video = 'W1';
089600100210
089700100210           // -?F10=Ricerca Serie (globale)?
089800100210           WHEN  dsp_aid = c_F10;
089900100210             exsr  sr_F10D02;
090000100210
090100100210           // -?F11=Ricerca Serie Parziale?
090200100210           WHEN  dsp_aid = c_F11;
090300100210             exsr  sr_F11D02;
090400100205
090500100205           // -?F12=Ritorno?
090600100205           WHEN  dsp_aid = c_F12;
090700100205             unlock TNTBE01L;
090800100205             reset  $Video;
090900100205             if  %parms() > 1;
091000100205               $Fine = *on;
091100100205             endif;
091200121102
091300121102           // -?F13=Visualizzazione profilo Utente Ultima Modifica?
091400121102           WHEN  dsp_aid = c_F13;
091500121102             $Video = 'W3';
091600111018
091700111025           // -?F14=Visualizzazione Ultimo Segnacollo?
091800111024           //      ?(reperito da FNBLP)?
091900111025           WHEN  dsp_aid = c_F14;
092000111018             $Video  = 'W2';
092100111018             $InzW02 = *on;
092200120202
092300120202           // -?F15=Conferma serie in tab. "3CL"?
092400120202           WHEN  dsp_aid = c_F15;
092500120202             V1Cnsi = 1;
092600120202             V1Cnsf = *all'9';
092700120202             F15Attivo = *off;
092800100205
092900100205           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
093000130827           //  ?F17=Annullamento Applicativo?
093100100205           OTHER;
093200100208             // -?Controlli eseguiti NON se F16=Annullamento?
093300130827             //                      ?né se F17=Annull.Applic.?
093400130827             if  dsp_aid <> c_F16  and  dsp_aid <> c_F17;
093500100205               exsr  sr_CtrD02;
093600100208             // -?Controlli eseguiti SOLO se F16=Annullamento?
093700100210             //else;
093800100210             //  exsr  sr_CtrANN;
093900100208             endif;
094000100208             if  ErrGenerico;
094100100208               leavesr;
094200100208             endif;
094300100208             // -?Aggiornamento?
094400100208             if  dsp_aid = c_F05   or
094500100208                 dsp_aid = c_F06   or
094600130827                 dsp_aid = c_F16   or
094700130827                 dsp_aid = c_F17;
094800100208               exsr  sr_UpdateAll;
094900100215               select;
095000100215                 // -?Rilevato errore in fase di aggiornamento?
095100100215                 when  ErrGenerico = *on;
095200100215                   leavesr;
095300100215                 // -?Uscita dal programma SE richiamato?
095400100215                 when  %parms() > 1;
095500100215                   o3EWksu = TBEke1;
095600100215                   o3EWsun = TBEke2;
095700100215                   o3EWuni = TBEuni;
095800100215                   $Fine = *on;
095900100215                 // -?Videata iniziale altrimenti?
096000100215                 other;
096100100215                   $Video  = 'D1';
096200100215                   $InzD01 = *on;
096300100215               endsl;
096400100208             endif;
096500100205
096600100205         ENDSL;
096700100205
096800100205       ENDSR;
096900100205
097000100205       //--------------------------------------------------------------
097100100518       //?Inizializzazione videata D02.                                ?
097200100205       //--------------------------------------------------------------
097300100205       BEGSR  sr_InzD02;
097400100209
097500140718         reset  $CtrlRngSgnCl;
097600100209         reset  $Protect;
097700100218         reset  $MsgMAD;
097800120202         reset  VisBloAnn;
097900120202         $FlsNrsIn3CL = *off;
098000120202         F15Attivo    = *off;
098100100205
098200100205         // -?Spegnimento degli indicatori di errore?
098300100205         %subst(IndDspF : 50) = *off;
098400100205
098500100205         // -?Pulizia videata?
098600100205         clear  TB79D02;
098700100205
098800100205         // -?Reperimento dati del supporto dati da clienti a Bartolini?
098900100205         k_TBEke1 = V1Cksu;
099000100205         k_TBEke2 = V1Csun;
099100100205         chain  %kds(k05tntbe01)  TNTBE000;
099200100210
099300100210         // -?Pulizia campi per controllo forzatura errore?
099400101125         clear  SaveNRS;
099500121127         reset  SavNRS;
099600121127         reset  SavNCD;
099700121127         reset  SavNCA;
099800100205
099900100209         // -?Caricamento dati assegnati alla postazione EasyWEB?
100000100205         if  %found(TNTBE01L);
100100100205           exsr  sr_RieD02;
100200100205         else;
100300100205           V1Cfls = %int( %subst( V1Cksu : 2 : 3 ) );
100400100210           V1Cctm = c_CTMxEasyWeb;
100500100908           V1Cmad = 1000;
100600150305           Save_mad = V1Cmad;
100700100302           V1Caew = 'N';
100800100514           V1Cupd = *blank;
100900100205         endif;
101000100205
101100100209         // -?Impostazione testata?
101200100312         //clear  V1Topz;              (già fatto in sr_GesD01)
101300100312         //clear  V1Tblo;              (già fatto in sr_GesD01)
101400100205         select;
101500100209           when  Not %found(TNTBE01L);
101600100209             V1Topz = '  INSERIMENTO  ';
101700100312           when  TBEatb <> *blank;
101800100209             V1Topz = '  RIPRISTINO   ';
101900110926           when  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
102000120725                                     or   TIVSSds.VSSksu = *zero  or
102100120725                 SavRpyEsito <> *zero   or   SavTIVSSds.VSSksu = *blank
102200120725                                        or   SavTIVSSds.VSSksu = *zero;
102300110926             V1Topz = ' ANNULLAMENTO  ';
102400100205           other;
102500100209             V1Topz = '   MODIFICA    ';
102600100205         endsl;
102700100209
102800100209         select;
102900100209           // -?Codice annullato?
103000100209           when  ACOflg <> *blank;
103100100209             if  %found(TNTBE01L);
103200100210               V1Tblo   = 'CODICE ANNULLATO';
103300100209               $Protect = *on;
103400100209             endif;
103500100209           // -?Codice bloccato?
103600130315           when  ACOabl <> *blanks;
103700100209             if  %found(TNTBE01L);
103800100210               V1Tblo   = 'CODICE  BLOCCATO';
103900100209             endif;
104000100209         endsl;
104100120202
104200120202         // ->?Visualizzazione "CLIENTE BLOCCATO" in testata?
104300120202         VisBloAnn = (V1Tblo <> *blank);
104400120202
104500120202         // ->?Numero Serie & Range Segnacolli in tab. "3CL"?
104600120202         ds_TNTBE.TBEke1 = %editc( V1Cfls : 'X' );
104700120202         ds_TNTBE.TBEke2 = %editc( V1Cnrs : 'X' );
104800120202         $FlsNrsIn3CL = ( V1Cnsi = 1  and  V1Cnsf = *all'9'  and
104900120202                        getTabella ( c_Tntbe : '3CL' : ds_TNTBE.TBEke1 :
105000120202                                     ds_TNTBE.TBEke2 : *omit : *omit :
105100120202                                     *omit : *omit :
105200120202                                     *omit : *omit : *omit : *omit :
105300120202                                     *omit : *omit : *omit : *omit :
105400120202                                     ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
105500120202                                   = *zero      AND
105600120202                        ds_TNTBE.TBEatb = *blank );
105700110926
105800111017         // -?Cliente NON più/ancora abilitatto ad EasyWEB?
105900120725         Select;
106000120814           //When  d3EW.§3EWfaa <> *blank;
106100120814           //  $Protect    = *on;
106200120725           When  SavRpyEsito <> *zero   or   SavTIVSSds.VSSksu = *blank
106300120725                                        or   SavTIVSSds.VSSksu = *zero;
106400120725             $Protect    = *on;
106500120725             V1Dmsg = $Msg(05);
106600120725             ErrMessage  = *on;
106700120725             ErrGenerico = *on;
106800120726           When  SavRpyEsito =  *zero   and
106900120726                (rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
107000120726                                     or   TIVSSds.VSSksu = *zero);
107100120725             $Protect    = *on;
107200120725             V1Dmsg = %trimr( $Msg(05) ) +
107300120725                      ' con questo User Number - vedi ' + SavTIVSSds.VSSsun;
107400120725             ErrMessage  = *on;
107500120725             ErrGenerico = *on;
107600120725         EndSl;
107700100210
107800100210         // -?Verifica esistenza tab. "3C" per il cliente in attivazione?
107900100210         exsr  sr_CtrlLegami3C;
108000100205
108100100205         // -?Impostazione indicatori per abilitazione tasti funzionali?
108200120816         //  ?(meglio farlo sempre e comunque - NON solo in sr_InzD02)?
108300120816         //exsr  sr_AbilitFxxD02;
108400100205
108500100205       ENDSR;
108600100210
108700100210       //--------------------------------------------------------------
108800100518       //?Verifica esistenza tab. "3C" per il cliente in attivazione.  ?
108900100210       //--------------------------------------------------------------
109000100210       BEGSR  sr_CtrlLegami3C;
109100100210
109200100210         // -?Se cliente NON trovato in tab. "3C": OK?
109300110422         //  ?(quasi impossibile)?
109400100210         if  ubLeg3C_Rtv ( %int( %subst( V1Cksu : 2 ) ) :
109500100210                           ubLeg3C_FlgPF :
109600100210                           ubLeg3C_Padre :
109700100210                           ubLeg3C_SKC :
109800100210                           ubLeg3C_SKEW ) < *zero;
109900100210           leavesr;
110000100210         endif;
110100100210
110200100210         // -?SE *pgm richiamato per creazione "3EW":?
110300100210         //  ?verifica se il cliente da attivare esiste GIÀ in "3C"?
110400110422         //  ?(praticamente scontato)?
110500110422         If  %parms() > 1;
110600110422
110700100216           select;
110800100216             when  sch_SKC(2) = *zero;
110900100216             when  %int(V1Cksu) = ubLeg3C_Padre;
111000110422               V1Dtxt1 = 'Cliente già in tab. "3C" come unificante';
111100100216             other;
111200110422               V1Dtxt1 = 'Cliente già in tab. "3C" con padre ' +
111300110422                         %editc( ubLeg3C_Padre : 'X' );
111400100216           endsl;
111500110422
111600110422           // -?Verifica serie e range segnacolli in tab. "3C" e "3CP"?
111700110422           if  ubNrsRsc_Rtv ( %int( %subst( V1Cksu : 2 ) ) :
111800110422                              ubNrsRsc_NRS : ubNrsRsc_NSI : ubNrsRsc_NSF )
111900110422                            < *zero;
112000110422             clear  ubNrsRsc_NRS;
112100110422             clear  ubNrsRsc_NSI;
112200110422             clear  ubNrsRsc_NSF;
112300110422           endif;
112400110422
112500110422           if  ubNrsRsc_NRS <> V1Cnrs  or  ubNrsRsc_NSI <> V1Cnsi;
112600110422             if  V1Dtxt1 = *blank;
112700110422               V1Dtxt1 = 'Cliente già in tab. "3C" con';
112800110422             else;
112900110422               V1Dtxt1 = %trimr(V1Dtxt1) + ', ';
113000110422             endif;
113100110422             V1Dtxt1 = %trimr(V1Dtxt1) + ' serie ' +
113200110422                       %editc( ubNrsRsc_NRS : 'X' );
113300110422           endif;
113400110422           if  ubNrsRsc_NSI <> V1Cnsi;
113500110422             V1Dtxt1 = %trimr(V1Dtxt1) + ' e range ' +
113600110422                       %trim( %editc( ubNrsRsc_NSI : 'Z' ) ) + '-' +
113700110422                       %trim( %editc( ubNrsRsc_NSF : 'Z' ) );
113800110422           endif;
113900110422
114000110422           if  Not %found(TNTBE01L);
114100110422             V1Cnrs = ubNrsRsc_NRS;
114200110422             V1Cnsi = ubNrsRsc_NSI;
114300110422             V1Cnsf = ubNrsRsc_NSF;
114400110422           endif;
114500110422
114600110422         EndIf;
114700100210
114800100210         // -?Comunque:?
114900100210         //  ?verifica se in "3C" ci sono figli NON EasyWeb legati a?
115000100211         //  ?padre EasyWeb (ed eventualmente li visualizza).?
115100100210         For  xx = 2  To  %elem(sch_SKC);
115200100210           select;
115300100210             when  sch_SKC(xx) = *zero;
115400100210               leave;
115500100210             when  sch_SKEW(xx) = *off;
115600100210               select;
115700100215                 when  V1Dtxt2 = *blank;
115800100215                   //V1Dtxt2 = 'In tab. "3C" esistono figli NON EasyWeb +
115900100215                   //           legati a questo unificante: ' +
116000100215                   //          %editc( sch_SKC(xx) : 'X' );
116100100215                   V1Dtxt2 = 'Elenco figli NON EasyWeb (in tab. "3C") +
116200100215                              legati a questo unificante: ' +
116300100215                             %editc( sch_SKC(xx) : 'X' );
116400100210                 when  %len(V1Dtxt2) - %len(%trimr(V1Dtxt2)) >= 10;
116500100210                   V1Dtxt2 = %trimr(V1Dtxt2) + ', ' +
116600100210                             %editc( sch_SKC(xx) : 'X' );
116700100211                 when  V1Dtxt3 = *blank;
116800100211                   V1Dtxt2 = %trimr(V1Dtxt2) + ',';
116900100211                   V1Dtxt3 = %editc( sch_SKC(xx) : 'X' );
117000100211                 when  %len(V1Dtxt3) - %len(%trimr(V1Dtxt3)) >= 10;
117100100211                   V1Dtxt3 = %trimr(V1Dtxt3) + ', ' +
117200100211                             %editc( sch_SKC(xx) : 'X' );
117300100211                 other;
117400100211                   V1Dtxt3 = %trimr(V1Dtxt3) + ', ...';
117500100211                   leave;
117600100210               endsl;
117700100210           endsl;
117800100210         EndFor;
117900100210
118000100210         // -?Eventuale spostamento del 2° testo in 1ª posizione?
118100100210         if  V1Dtxt1 = *blank   and   V1Dtxt2 <> *blank;
118200100210           V1Dtxt1 = V1Dtxt2;
118300100211           V1Dtxt2 = V1Dtxt3;
118400100211           clear  V1Dtxt3;
118500100210         endif;
118600100210
118700100210         // -?Eventuale impostazione del "titolo" iniziale?
118800100210         if  V1Dtxt1 <> *blank;
118900100210           V1Dtxt0 = 'ATTENZIONE:';
119000100210         endif;
119100100210
119200100210       ENDSR;
119300100205
119400100205       //--------------------------------------------------------------
119500100205       //?Caricamento dati del singolo record della tab. "3EW"         ?
119600100518       //?nella videata D02.                                           ?
119700100205       //--------------------------------------------------------------
119800100205       BEGSR  sr_RieD02;
119900100205
120000100205         clear  TB79D02;
120100100205
120200100205         d3EW = TBEuni;
120300100205
120400100205         // -?Dati della tab. "3EW"?
120500100330         V1Clnp = %editc( d3EW.§3EWlnp : 'X' );
120600100330         V1Cfls = d3EW.§3EWfls;
120700100330         V1Cnrs = d3EW.§3EWnrs;
120800100330         V1Cnsi = d3EW.§3EWnsi;
120900100330         V1Cnsf = d3EW.§3EWnsf;
121000100330         V1Cctm = d3EW.§3EWctm;
121100100330         V1Cmad = d3EW.§3EWmad;
121200150305         Save_mad = V1Cmad;
121300100330         V1Caew = d3EW.§3EWaew;
121400100330         V1Cupd = d3EW.§3EWupd;
121500120814         V1Cfaa = d3EW.§3EWfaa;
121600120814         if  d3EW.§3EWdaa > *zero;
121700120814           wDate_Eur = %date( %int(d3EW.§3EWdaa) : *iso );
121800120814           V1Cdaa = %dec(wDate_Eur);
121900120814         endif;
122000120814
122100120814         // -?Impostazione indicatori?
122200120814         ProtAnnAppl = (d3EW.§3EWfaa = *blank);
122300100205
122400100205         // -?Decodifiche?
122500100205         exsr  sr_CtrD02;
122600100205         clear  V1Dmsg;
122700100205         reset  ErrMessage;
122800100205         reset  ErrGenerico;
122900100205         %subst(IndDspF : 50) = *off;
123000100205
123100100205       ENDSR;
123200100205
123300100205       //--------------------------------------------------------------
123400100205       //?Settaggio indicatori nella videata D02 per abilitazione      ?
123500100518       //?  tasti funzionali.                                          ?
123600100205       //--------------------------------------------------------------
123700100210       BEGSR  sr_AbilitFxxD02;
123800100205
123900100205         // -?F3=Fine (se NON richiamato da altro *pgm)?
124000110926         F3Attivo  = (%parms() = 1);
124100100205
124200100205         // -?F5=Ripristino?
124300100210         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
124400100209                                         and   Not $Protect);
124500100205
124600100205         // -?F6=Conferma?
124700100205         F6Attivo  = (NOT F5Attivo);
124800100211
124900100211         // -?F8=Interrogazione legami tabelle VAS?
125000100211         F8Attivo  = *on;
125100110930
125200110930         // -?F9=Interrogazione informazioni EasySped?
125300110930         F9Attivo = (ds3C.§3Ccba = 'ESYSP'   or   ds3C.§3Ccba = 'ESVAL'   or
125400110930                     getTabella ( c_Tntbe : '3CE' : %subst(V1Cksu : 2) :
125500110930                                  *blank: *omit : *omit :
125600110930                                  *omit : *omit :
125700110930                                  *omit : *omit : *omit : *omit :
125800110930                                  *omit : *omit : *omit : *omit :
125900110930                                  ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
126000110930                                = *zero      AND
126100110930                     ds_TNTBE.TBEatb = *blank);
126200100209
126300130827         // -?F10=Ricerca serie?
126400130827         //  ?(Anche se Filiale/Serie Segnacolli in tab. "3CL")?
126500120202         //F10Attivo = (Not $FlsNrsIn3CL);
126600120816         //F10Attivo = *on;
126700120816         F10Attivo = (V1Cfaa = *blank);
126800100209
126900130827         // -?F11=Ricerca serie parziale?
127000100209         F11Attivo = (F10Attivo);
127100100205
127200111017         // -?F12=Ritorno (se NON richiamato da altro *pgm  o?
127300111017         //               ?se già esistente in tab. "3EW"   o?
127400111017         //               ?se NON abilitato ad EasySpedWEB)?
127500111017         F12Attivo = (%parms() = 1   or   %found(TNTBE01L)
127600111017                                     or   $Protect);
127700100205
127800110927         // -?F16=Annullamento (se NON richiamato da altro *pgm &?
127900110927         //                    ?se abilitazione scaduta &?
128000130320         //                    ?se non più EasyWEB o con padre diverso  o?
128100130320         //                    ?se ancora EasyWEB ma con S.U.N. diverso)?
128200110926         //F16Attivo = *off;
128300110926         F16Attivo = ( %found(TNTBE01L)   and   TBEatb = *blank  and
128400110926                       %parms() =  1      and
128500130320                     ((rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
128600120725                                           or   TIVSSds.VSSksu = *zero   or
128700120725                       SavRpyEsito <> *zero   or   SavTIVSSds.VSSksu = *blank
128800130320                                              or   SavTIVSSds.VSSksu = *zero)
128900110926                      and    (ds3C.§3Ccba      <> c_Supp_EsWeb   or
129000130320                       %editc(ds3C.§3Ccks:'X') <> %subst(V1Cksu : 2))    or
129100130320                     ((rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
129200130320                                           or   TIVSSds.VSSksu = *zero)  and
129300130320                       SavRpyEsito = *zero)) );
129400111025         if  F16Attivo;
129500111025           F6Attivo = *off;
129600111025         endif;
129700121102
129800121102         // -?F13=Profilo utente ultima modifica?
129900121102         F13Attivo  = (%found(TNTBE01L));
130000111018
130100111025         // -?F14=Reperimento ultimo segnacollo?
130200111025         F14Attivo  = *on;
130300120202
130400130827         // -?F15=Conferma serie totale comune (da tab. "3CL")?
130500130827         //  ?(NON qui: potrebbe già essere acceso!)?
130600120202         //F15Attivo = *off;
130700130827
130800130827         // -?F17=Annullamento Applicativo?
130900130827         F17Attivo = (%parms() = 1         and
131000130827                      %found(TNTBE01L)     and  TBEatb  <> 'A'  and
131100130827                      d3EW.§3EWfaa <> 'A'  and  ds3C.§3Crag = *blank);
131200100205
131300100205       ENDSR;
131400100205
131500100205       //--------------------------------------------------------------
131600100518       //?Controllo dati videata D02.                                  ?
131700100205       //--------------------------------------------------------------
131800100205       BEGSR  sr_CtrD02;
131900120202
132000120202         F15Attivo = *off;
132100100205
132200100205         // -?Spegnimento degli indicatori di errore?
132300100205         %subst(IndDspF : 50) = *off;
132400120202
132500120202         // -?Spegnimento indicatori per attributi di visualizzazione?
132600120202         $FlsNrsIn3CL = *off;
132700110926
132800110927         // -?Cliente NON più abilitato ad EasyWEB?
132900120725         if  SavRpyEsito <> *zero   or   SavTIVSSds.VSSksu = *blank
133000120725                                    or   SavTIVSSds.VSSksu = *zero;
133100110926           V1Dmsg = $Msg(05);
133200110926           ErrMessage  = *on;
133300110926           ErrGenerico = *on;
133400110927           //leavesr;
133500110926         endif;
133600120725
133700120725         // -?Cliente (con Strategi User Number) NON più abilitato ad EasyWEB?
133800120726         if  SavRpyEsito =  *zero   and
133900120726            (rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
134000120726                                 or   TIVSSds.VSSksu = *zero);
134100120725           V1Dmsg = %trimr( $Msg(05) ) +
134200120725                    ' con questo User Number - vedi ' + SavTIVSSds.VSSsun;
134300120725           ErrMessage  = *on;
134400120725           ErrGenerico = *on;
134500120725           //leavesr;
134600120725         endif;
134700100205
134800100205         // -?Linea di partenza?
134900100205         clear  V1Dlnp;
135000100205         select;
135100100205           //   ·?obbligatoria?
135200100205           when  V1Clnp = *blank;
135300100205             ErrMessage  = *on;
135400100205             ErrGenerico = *on;
135500100205             PosCurLNP   = *on;
135600100302             V1Dmsg = $Msg(17);
135700100205             leavesr;
135800100205           //   ·?richiesta ricerca?
135900100210           when  %scan( '?' : V1Clnp ) > *zero;
136000100205             tnsd24r ( p_FIL : p_FAG : p_DES : p_DIT );
136100100205             V1Clnp = p_FIL;
136200100205             V1Dlnp = p_DES;
136300100205           //   ·?controllo?
136400100205           other;
136500100205             chain (%int( V1Clnp ))  AZORG;
136600100210             if  %found(AZORG01L)   and   ORGfva = *blank;
136700100205               V1Dlnp = ORGdes;
136800100205             else;
136900100205               ErrMessage  = *on;
137000100205               ErrGenerico = *on;
137100100205               PosCurLNP   = *on;
137200100302               V1Dmsg = $Msg(18);
137300100205               leavesr;
137400100205             endif;
137500100205         endsl;
137600100205
137700100205         // -?Filiale segnacollo?
137800100205         clear  V1Dfls;
137900100210         V1Cfls = %int( %subst( V1Cksu : 2 : 3 ) );
138000100205         select;
138100100205           //   ·?obbligatoria?
138200100210           when  V1Cfls <= *zero;
138300100205             ErrMessage  = *on;
138400100205             ErrGenerico = *on;
138500100209             PosCurFLS   = *on;
138600100302             V1Dmsg = $Msg(19);
138700100205             leavesr;
138800100205           //   ·?controllo?
138900100205           other;
139000100210             chain  (V1Cfls)  AZORG;
139100100210             if  %found(AZORG01L)   and   ORGfva = *blank;
139200100205               V1Dfls = ORGdes;
139300100205             else;
139400100205               ErrMessage  = *on;
139500100205               ErrGenerico = *on;
139600100209               PosCurFLS   = *on;
139700100302               V1Dmsg = $Msg(20);
139800100205               leavesr;
139900100205             endif;
140000100205         endsl;
140100100205
140200100209         // -?Numero serie & Range segnacolli assegnati?
140300100402         if  V1Cnrs > *zero   and
140400100402             V1Cnsi = *zero   and   V1Cnsf = *zero;
140500100402           V1Cnsi = 1;
140600100402           V1Cnsf = *all'9';
140700100402         endif;
140800100402
140900120202         // ->?Serie obbligatoria?
141000120202         If  V1Cnrs = *zero;
141100120202           V1Dmsg = $Msg(21);
141200120202           PosCurNRS   = *on;
141300120202           ErrMessage  = *on;
141400120202           ErrGenerico = *on;
141500120202           leavesr;
141600120202         // ->?Controlli per serie, sia essa completa o parziale?
141700120202         //   ?(controlla prima serie parziale, poi completa)?
141800120202         Else;
141900120202           V3Cnrs = V1Cnrs;
142000120202           V3Cncd = V1Cnsi;
142100120202           V3Cnca = V1Cnsf;
142200120202           exsr  sr_Ctrl_NRS_SCx;
142300120202           if  ErrGenerico = *on;
142400120202             leavesr;
142500120202           endif;
142600120202         EndIf;
142700100205
142800100205         // -?Codice trattamento merce?
142900100209         clear  V1Dctm;
143000100209         // ->?Ricerca?
143100100209         if  %scan( '?' : V1Cctm ) > *zero;
143200100209           clear  V1Cctm;
143300100210           X§Tkut = c_Kut;
143400100209           X§Tcod = '1B';
143500100209           clear  X§Tkey;
143600100209           clear  X§Tdes;
143700100209           x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
143800100209           V1Cctm = %subst(X§Tkey : 1 : %len(V1Cctm));
143900100209           V1Dctm = X§Tdes;
144000100209           ErrGenerico = *on;
144100100209           leavesr;
144200100209         // ->?Controllo / Decodifica?
144300100209         else;
144400100209           ds_TNTBE.TBEke1 = V1Cctm;
144500100209           if  getTabella ( c_Tabel : '1B'  : ds_TNTBE.TBEke1 :
144600100209                            *omit : *omit : *omit :
144700100209                            *omit : *omit :
144800100209                            *omit : *omit : *omit : *omit :
144900100209                            *omit : *omit : *omit : *omit :
145000100209                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
145100100209                          = *zero      AND
145200100209               ds_TNTBE.TBEatb = *blank;
145300100209             ds1B   = ds_TNTBE.TBEuni;
145400100209             V1Dctm = §1Bdes;
145500100209           else;
145600100209             V1Dctm = *all'? ';
145700100304             V1Dmsg = $Msg(23);
145800100209             PosCurCTM   = *on;
145900100209             ErrMessage  = *on;
146000100209             ErrGenerico = *on;
146100100209             leavesr;
146200100209           endif;
146300100205
146400100209         endif;
146500100218
146600100218         // -?Numero max anagrafiche destinatari?
146700100218         select;
146800100218           // ->?Controllo da NON eseguire in fase di inizializzazione?
146900100218           when  $InzD02;
147000100218           // ->?Non ammessi numeri negativi?
147100100218           when  V1Cmad < *zero;
147200100218             reset  $MsgMAD;
147300100304             V1Dmsg = $Msg(24);
147400100218             PosCurMAD   = *on;
147500100218             ErrMessage  = *on;
147600100218             ErrGenerico = *on;
147700100218             leavesr;
147800100218           // ->?Messaggio di avvertimento se *zero?
147900100218           when  V1Cmad = *zero   and   Not $MsgMAD;
148000100218             $MsgMAD     = *on;
148100100304             V1Dmsg = $Msg(25);
148200100218             PosCurMAD   = *on;
148300100218             ErrMessage  = *on;
148400100218             ErrGenerico = *on;
148500100218             leavesr;
148600150305           // ->?Messaggio di avvertimento se 9999 ?
148700150305           when  V1Cmad > 9999   and   Not $MsgMAD;
148800150305             $MsgMAD     = *on;
148900150305             V1Dmsg = $Msg(37);
149000150305             PosCurMAD   = *on;
149100150305             ErrMessage  = *on;
149200150305             ErrGenerico = *on;
149300150305             leavesr;
149400150305           // ->?Messaggio di avvertimento se valore diminuito
149500150305           when  V1Cmad < Save_mad   and   Not $MsgMAD;
149600150305             $MsgMAD     = *on;
149700150305             V1Dmsg = $Msg(38);
149800150305             PosCurMAD   = *on;
149900150305             ErrMessage  = *on;
150000150305             ErrGenerico = *on;
150100150305             leavesr;
150200100218           // ->?Inserito un numero > *zero?
150300150305           // ->?e non > 9999 e non diminuito
150400150305           when  V1Cmad > *zero
150500150305           and   V1Cmad <= 9999
150600150305           and   V1Cmad >= Save_mad;
150700100218             reset  $MsgMAD;
150800150305             Save_mad = V1Cmad;
150900100218         endsl;
151000100302
151100100302         // -?Flag abilitazione a creare utenti EasyWEB di tipo?
151200100302         //  ?"bollettisti"?
151300100331         if  V1Caew <> 'N'   and   V1Caew <> 'S';
151400100302           ErrMessage  = *on;
151500100302           ErrGenerico = *on;
151600100302           PosCurAEW   = *on;
151700100304           V1Dmsg = $Msg(26);
151800100302           leavesr;
151900100302         endif;
152000120814
152100120814         // -?Flag Annullamento Applicativo?
152200120814         select;
152300120814           when  V1Cfaa <> *blank  and  V1Cfaa <> 'A';
152400120814             ErrGenerico = *on;
152500120814             ErrMessage  = *on;
152600120814             PosCurFAA   = *on;
152700120814             V1Dmsg = $Msg(30);
152800120814             leavesr;
152900120814           when  V1Cfaa      = 'A'  and
153000120814                 ds3C.§3Ccba = c_Supp_ESWEB;
153100120814             ErrGenerico = *on;
153200120814             ErrMessage  = *on;
153300120814             PosCurFAA   = *on;
153400120814             V1Dmsg = $Msg(31);
153500120814             leavesr;
153600120814         endsl;
153700120814
153800120814         // -?Data Annullamento Applicativo?
153900120814         select;
154000120814           when  V1Cfaa  = *blank  and  V1Cdaa <> *zero;
154100120814             ErrGenerico = *on;
154200120814             ErrMessage  = *on;
154300120814             PosCurDAA   = *on;
154400120814             V1Dmsg = $Msg(32);
154500120814             leavesr;
154600120814           when  V1Cfaa <> *blank  and  V1Cdaa  = *zero;
154700120814             ErrGenerico = *on;
154800120814             ErrMessage  = *on;
154900120814             PosCurFAA   = *on;
155000120814             V1Dmsg = $Msg(32);
155100120814             leavesr;
155200120814         endsl;
155300120814         if  V1Cdaa > *zero;
155400120814           clear WLBdat;
155500120814           G08dat = V1Cdaa;
155600120814           XSRDA8(WLBdat);
155700120814           if  G08err = *on;
155800120814             ErrGenerico = *on;
155900120814             ErrMessage  = *on;
156000120814             PosCurDAA   = *on;
156100120814             V1Dmsg = $Msg(33);
156200120814             leavesr;
156300120814           else;
156400120814             V1Cdaa = G08dat;
156500120814           endif;
156600120814         endif;
156700130827
156800130827         // -?Controllo abilitazione a STRATEGI ad oggi?
156900130827         //  ?(se tolto l'annullamento applicativo)?
157000130827         if  d3EW.§3EWdaa > *zero  and  V1Cdaa = *zero;
157100130827           clear  rpyEsito;
157200130827           Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : V1Cksu : 'EW' :
157300130827                                    *blank : *zero : TIVSSds :
157400130827                                    rpyEsito );
157500130827           if  rpyEsito <> *zero  or  TIVSSds.VSSksu = *blank
157600130827                                  or  TIVSSds.VSSksu = *zero;
157700130827             V1Dmsg = $Msg(05);
157800130827             PosCurFAA   = *on;
157900130827             ErrMessage  = *on;
158000130827             ErrGenerico = *on;
158100130827             leavesr;
158200130827           endif;
158300130827         endif;
158400100205
158500100205       ENDSR;
158600101125
158700101125       //--------------------------------------------------------------
158800101125       //?Controllo range segnacolli per filiale e serie.              ?
158900101125       //--------------------------------------------------------------
159000101125       BEGSR  sr_Ctrl_NRS_SCx;
159100101125
159200101125         clear  TNTB54ds;
159300101125         I54tel = 'C';
159400101125         I54fil = %editc( V1Cfls : 'X' );
159500101125         I54ksc = %subst( V1Cksu : 2 );
159600101125         I54nrs = %editc( V3Cnrs : 'X' );
159700101125         I54dal = V3Cncd;
159800101125         I54al  = V3Cnca;
159900101125
160000101125         kpjbu  = TNTB54ds;
160100110209         tntb54r ( kpjba : ubLeg3C_SKC );
160200101125         TNTB54ds = kpjbu;
160300101125
160400101125         SELECT;
160500120202
160600120202           // ->?Serie già assegnata come completa (in tab. "3CL")?
160700120202           //   ?La segnalazione qui fa scartare gli altri controlli di?
160800120202           //   ?TRTB28R2 !?
160900120202           WHEN  O54err = 'A'  and  (V3Cncd > 1  or
161000120202                                     V3Cnca < *hival);
161100120202             V1Dmsg = $Msg(29);
161200120202             PosCurNRS   = *on;
161300120202             ErrMessage  = *on;
161400120202             ErrGenerico = *on;
161500120202             F15Attivo   = *on;
161600120202             leavesr;
161700101125
161800101125           // ->?errore SQL?
161900101125           WHEN  O54err = 'S';
162000101125             V1Dmsg = $Msg(14);
162100101125             PosCurNRS   = *on;
162200101125             ErrMessage  = *on;
162300101125             ErrGenerico = *on;
162400101125             leavesr;
162500101125
162600101125           // ->?serie già presente come parziale?
162700120202           //   ?(non nel padre)?
162800120202           WHEN  O54err =  '1'  and
162900120202                 %lookup( %int(O54ksc) : sch_SKC ) = *zero;
163000101125             if  O54dal >  1   or   O54al <  *hival;
163100101125               V1Dmsg = %trimr($Msg(22)) + ' ' + O54ksc;
163200101125             else;
163300101125               V1Dmsg = %trimr($Msg(16)) + ' ' + O54ksc;
163400101125             endif;
163500101125             PosCurNRS   = *on;
163600101125             ErrMessage  = *on;
163700101125             ErrGenerico = *on;
163800101125             leavesr;
163900120113
164000120113           // ->?Serie NON ammessa (99)?
164100120113           WHEN  O54err = '9';
164200120113             V1Dmsg = $Msg(28);
164300120113             PosCurNRS   = *on;
164400120113             ErrMessage  = *on;
164500120113             ErrGenerico = *on;
164600120113             leavesr;
164700101125
164800101125           // ->?verifica se serie già presente come completa;?
164900101125           OTHER;
165000120202             //If  O54prz = *blank;
165100120202             If  ( (V1Cfls  > *zero  and  ParFIL <> %int(V1Cfls))   or
165200120202                   (V1Cfls <= *zero  and  ParFIL <>
165300120202                                          %int(%trim(V1Cksu))/10000)  or
165400120202                  ParNRS <> V3Cnrs   or
165500120202                  ParERR <> *blank )   and
165600120202                 (V3Cncd = 1  and  V3Cnca = 9999999);
165700101125               clear  TRTB28ds2;
165800101125               ParFIL = V1Cfls;
165900101125               ParNRS = V3Cnrs;
166000101125               ParELB = 'C';
166100101125               ParKSC = %subst( V1Cksu : 2 );
166200111215               trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
166300120202
166400120113               select;
166500120202
166600120113                 // ->?Serie NON ammessa (99)?
166700120113                 when  ParERR = '9';
166800120113                   V1Dmsg = $Msg(28);
166900120113                   PosCurNRS   = *on;
167000120113                   ErrMessage  = *on;
167100120113                   ErrGenerico = *on;
167200120113                   leavesr;
167300120202
167400120113                 // ->?errore bloccante SE con padre diverso?
167500120113                 // ->?errore forzabile SE con stesso padre?
167600120202                 //   ?(NON più: ora solo se con padre diverso)?
167700120113                 when  ParERR =  'E'     and
167800120202                       %lookup( %int(OUTksc) : sch_SKC ) = *zero;
167900120113                   V1Dmsg = %trimr($Msg(16)) + ' ' + OUTksc;
168000120113                   PosCurNRS   = *on;
168100120113                   ErrMessage  = *on;
168200120113                   ErrGenerico = *on;
168300120113                   leavesr;
168400120202
168500120202                 // ->?Serie già assegnata come completa (in tab. "3CL")?
168600120202                 when  O54err = 'A'  and  (V1Cnsi > 1  or
168700120202                                           V1Cnsf < *hival);
168800120202                   V1Dmsg = $Msg(29);
168900120202                   PosCurNRS   = *on;
169000120202                   ErrMessage  = *on;
169100120202                   ErrGenerico = *on;
169200120202                   F15Attivo   = *on;
169300120202                   leavesr;
169400120202
169500120113               endsl;
169600120202
169700101125             EndIf;
169800101125
169900101125         ENDSL;
170000121129
170100121129         // -?SE modificato il supporto Cli-BRT (in o da "ESWEB")?
170200121129         //  ?occorre cambiare range?
170300121129         //  ?(lasciando il "vecchio" "bloccato" in tab. "3EW" - se da?
170400121129         //  ?"ESWEB")?
170500140828         If  (%found(TNTBE01L)              and
170600140828              ds3C.§3Ccba  <> c_Supp_ESWEB  and
170700121220              ubNrsRsc_NRS =  V1Cnrs        and
170800121220              ubNrsRsc_NSI =  V1Cnsi        and
170900121220              ubNrsRsc_NSF =  V1Cnsf);
171000121129           V1Dmsg = $Msg(35);
171100121129           PosCurNRS   = *on;
171200121129           ErrMessage  = *on;
171300121129           ErrGenerico = *on;
171400121129           leavesr;
171500121129         EndIf;
171600121127
171700121127         // -?ULTIMO CONTROLLO (BYPASSABILE):?
171800121127         //  ?Range "anomalo"?
171900140718         //IF  $Video =  'D3'   and  (  V3Cnrs <> SavNRS  or
172000140718         //    V3Cncd <> SavNCD  or     V3Cnca <> SavNCA )   and  (
172100140718         //    %subst( %editc( V3Cncd : 'X' ) : %len(V3Cncd) : 1 ) <> '1'
172200140718         //                      or  (
172300140718         //    %subst( %editc( V3Cnca : 'X' ) : %len(V3Cnca) : 1 ) <> '0'
172400140718         //                     and     V3Cnca <> *hival ) );
172500140718         //
172600140718         //  SavNRS = V3Cnrs;
172700140718         //  SavNCD = V3Cncd;
172800140718         //  SavNCA = V3Cnca;
172900140718         //
173000140718         //  Select;
173100140718         //
173200140718         //    When  %subst(%editc(V3Cncd:'X'):%len(V3Cncd):1) <> '1' and
173300140718         //          %subst(%editc(V3Cnca:'X'):%len(V3Cnca):1) <> '0' and
173400140718         //          V3Cnca <> *hival;
173500140718         //      V1Dmsg = %trimr($Msg(34)) +
173600140718         //      //*      ' non comincia con "...1" +
173700140718         //      //*        né termina con "...0". +
173800140718         //      //*        Invio.';
173900140718         //               %subst(%editc(V3Cncd:'X'):1:%len(V3Cncd)-1)+'1'
174000140718         //             + ' né termina con';
174100140718         //      wV3Cncx = V3Cnca + 1;
174200140718         //      if  %subst(%editc(wV3Cncx:'X'):%len(wV3Cncx)-1:2) = '00';
174300140718         //        V1Dmsg = %trimr(V1Dmsg) + ' ' +
174400140718         //               %subst(%editc(wV3Cncx:'X'):1:%len(wV3Cncx));
174500140718         //      else;
174600140718         //        V1Dmsg = %trimr(V1Dmsg) + ' ' +
174700140718         //               %subst(%editc(V3Cnca:'X'):1:%len(V3Cnca)-1)+'0';
174800140718         //      endif;
174900140718         //      V1Dmsg = %trimr(V1Dmsg) + '. Invio.';
175000140718         //      PosCurD3ncd = *on;
175100140718         //
175200140718         //    When  %subst(%editc(V3Cncd:'X'):%len(V3Cncd):1) <> '1';
175300140718         //      V1Dmsg = %trimr($Msg(34)) +
175400140718         //      //*      ' dovrebbe cominciare con "...1". +
175500140718         //      //*        Premere Invio.';
175600140718         //               ' dovrebbe cominciare con ' +
175700140718         //               %subst(%editc(V3Cncd:'X'):1:%len(V3Cncd)-1)+'1'
175800140718         //             + '. Premere Invio.';
175900140718         //      PosCurD3ncd = *on;
176000140718         //
176100140718         //    When  %subst(%editc(V3Cnca:'X'):%len(V3Cnca):1) <> '0' and
176200140718         //          V3Cnca <> *hival;
176300140718         //      V1Dmsg = %trimr($Msg(34)) +
176400140718         //      //*      ' dovrebbe terminare con "...0". +
176500140718         //      //*        Premere Invio.';
176600140718         //               ' dovrebbe terminare con';
176700140718         //      wV3Cncx = V3Cnca + 1;
176800140718         //      if  %subst(%editc(wV3Cncx:'X'):%len(wV3Cncx)-1:2) = '00';
176900140718         //        V1Dmsg = %trimr(V1Dmsg) + ' ' +
177000140718         //               %subst(%editc(wV3Cncx:'X'):1:%len(wV3Cncx));
177100140718         //      else;
177200140718         //        V1Dmsg = %trimr(V1Dmsg) + ' ' +
177300140718         //               %subst(%editc(V3Cnca:'X'):1:%len(V3Cnca)-1)+'0';
177400140718         //      endif;
177500140718         //      V1Dmsg = %trimr(V1Dmsg) + '.  Premere Invio.';
177600140718         //      PosCurD3nca = *on;
177700140718         //
177800140718         //  EndSl;
177900140718         //
178000140718         //  ErrMessage  = *on;
178100140718         //  ErrGenerico = *on;
178200140718         //  leavesr;
178300140718         //
178400140718         //ENDIF;
178500101125
178600101125       ENDSR;
178700100208
178800100208       //--------------------------------------------------------------
178900100208       //?Controllo annullabilità del record nella videata D02.        ?
179000100208       //--------------------------------------------------------------
179100100210       BEGSR  sr_CtrANN;
179200100210
179300100210         %subst(IndDspF : 50) = *off;
179400100210
179500100210       ENDSR;
179600100211
179700100211       //--------------------------------------------------------------
179800100518       //?Gestione tasto funzionale F8=Interrog. legami tab. VAS.      ?
179900100211       //--------------------------------------------------------------
180000110930       BEGSR  sr_F08D02;
180100100211
180200100211         clear  trtb06ds;
180300100211         D06ksc = %int( %subst( V1Cksu : 2 ) );
180400100211         D063C  = 'S';
180500100211
180600100211         kpjbu = trtb06ds;
180700100211
180800100211         trtb06r ( kpjba );
180900100211
181000100211       ENDSR;
181100100209
181200100209       //--------------------------------------------------------------
181300100209       //?Gestione tasto funzionale F10=Ricerca Serie (in D02).        ?
181400100209       //--------------------------------------------------------------
181500100209       BEGSR  sr_F10D02;
181600100209
181700100209         // -?NON modificabile manualmente premendo F10?
181800101125         if  V1Cnrs <> *zero   and  V1Cnrs <> SaveNRS;
181900101125           SaveNRS = V1Cnrs;
182000101125           V1Dmsg  = $Msg(08);
182100100209           PosCurNRS   = *on;
182200100209           ErrMessage  = *on;
182300100209           ErrGenerico = *on;
182400100209           leavesr;
182500100209         endif;
182600100209
182700100209         // -?Ricerca?
182800100209         clear  TRTB28ds2;
182900100210         ParFIL = %int(V1Clnp);
183000100209         ParELB = 'R';
183100111215         trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
183200120113         select;
183300120113           // ->?Serie NON ammessa (99)?
183400120113           when  ParERR = '9';
183500120113             V1Dmsg = $Msg(28);
183600120113             PosCurNRS   = *on;
183700120113             ErrMessage  = *on;
183800120113             ErrGenerico = *on;
183900120113             leavesr;
184000120113           // ->?Non reperibile una nuova serie?
184100120113           when  ParERR = 'E';
184200120113             V1Dmsg = $Msg(09);
184300120113             PosCurNRS   = *on;
184400120113             ErrMessage  = *on;
184500120113             ErrGenerico = *on;
184600120113             leavesr;
184700120113           // ->?Serie valida?
184800120113           other;
184900120113             V1Cnrs = ParNRS;
185000120113             V1Cnsi = 1;
185100120113             V1Cnsf = *hival;
185200120113         endsl;
185300100209
185400100209       ENDSR;
185500100209
185600100209       //--------------------------------------------------------------
185700100209       //?Gestione tasto funzionale F11=Ricerca Serie Parziale (in D02)?
185800100209       //--------------------------------------------------------------
185900100209       BEGSR  sr_F11D02;
186000100209
186100100209         $Video = 'D3';
186200100209         reset  $InzD03;
186300100209
186400100209       ENDSR;
186500100205
186600100205       //--------------------------------------------------------------
186700100209       //?Gestione videata D03 (Ricerca serie parziale).               ?
186800100205       //--------------------------------------------------------------
186900100205       BEGSR  sr_GesD03;
187000100205
187100100205         // -?Inizializzazione window?
187200100205         if  $InzD03 = *on;
187300100205           clear  TB79D03;
187400100205           $InzD03 = *off;
187500100205         endif;
187600100205
187700100205         // -?Emissione window?
187800100205         if  Not $Protect;
187900100205           exfmt  TB79D03;
188000100205         else;
188100100205           write  TB79D03;
188200100205           exfmt  Protect;
188300100205         endif;
188400100205         reset  ErrMessage;
188500100205         reset  ErrGenerico;
188600100205         clear  V1Dmsg;
188700100205
188800100205         // -?F12=Ritorno?
188900100205         if  dsp_aid = c_F12;
189000100205           $Video = 'D2';
189100100205           leavesr;
189200100205         endif;
189300100205
189400100205         // -?Invio?
189500140903         $CtrlRngSgnCl = *off;
189600100205         exsr  sr_CtrD03;
189700140718         if  $CtrlRngSgnCl;
189800140718           exsr  sr_CtrD03;
189900140718         endif;
190000100205         if  NOT ErrGenerico;
190100100205           // ->?Ritorno alla videata D02?
190200100205           $Video = 'D2';
190300100205           leavesr;
190400100205         endif;
190500100205
190600100205       ENDSR;
190700100205
190800100205       //--------------------------------------------------------------
190900100205       //?Controllo dati immessi nella window D03.                     ?
191000100205       //--------------------------------------------------------------
191100100205       BEGSR  sr_CtrD03;
191200100205
191300100205         // -?Spegnimento degli indicatori di errore?
191400140123         %subst(IndDspF : 65) = *off;
191500140903         //$CtrlRngSgnCl = *off;
191600100205
191700100205         select;
191800100205
191900100205           // -?Effettuare una selezione?
192000100205           when  V3Cncl = *zero   and   V3Cnrs = *zero;
192100100210             V1Dmsg = $Msg(10);
192200100205             PosCurD3ncl = *on;
192300100205             ErrMessage  = *on;
192400100205             ErrGenerico = *on;
192500100205             leavesr;
192600100205
192700100205           // -?Selezionare Numero colli OPPURE Segnacollo da-al?
192800100205           when  V3Cncl <> *zero   and   V3Cnrs <> *zero;
192900100210             V1Dmsg = $Msg(11);
193000100205             PosCurD3ncl = *on;
193100100205             ErrMessage  = *on;
193200100205             ErrGenerico = *on;
193300100205             leavesr;
193400100205
193500100205           // -?Controllo range segnacolli?
193600100205           when  V3Cnrs <> *zero   and   V3Cnca = *zero;
193700100210             V1Dmsg = $Msg(12);
193800100205             PosCurD3nca = *on;
193900100205             ErrMessage  = *on;
194000100205             ErrGenerico = *on;
194100100205             leavesr;
194200100205           when  V3Cnrs <> *zero   and   V3Cncd > V3Cnca;
194300100210             V1Dmsg = $Msg(13);
194400100205             PosCurD3ncd = *on;
194500100205             ErrMessage  = *on;
194600100205             ErrGenerico = *on;
194700100205             leavesr;
194800100205
194900100205         endsl;
195000100514
195100100514         // -?Sostituzione collo inziale del range con 1 se 0?
195200100514         if  V3Cnrs <> *zero   and   V3Cncd <= *zero;
195300100514           V3Cncd = 1;
195400100514           write  TB79D03;
195500100514         endif;
195600140123
195700140718         // -?Numero colli richiesto:?
195800140718         //  ?almeno 10.000 e comunque multiplo di 5.000?
195900140123         select;
196000140718           when  V3Cncl > *zero   and   V3Cncl < 10000;
196100140718             V1Dmsg = %trimr($Msg(34)) +
196200140718                      ' DEVE prevedere almeno 10.000 colli';
196300140718             PosCurD3ncl = *on;
196400140718             ErrMessage  = *on;
196500140718             ErrGenerico = *on;
196600140718             leavesr;
196700140718           when  V3Cncl > 10000   and   %rem( V3Cncl : 5000 ) > *zero;
196800140123             V1Dmsg = %trimr($Msg(34)) +
196900140718                      ' NON è multiplo di 5.000';
197000140123             PosCurD3ncl = *on;
197100140123             ErrMessage  = *on;
197200140123             ErrGenerico = *on;
197300140123             leavesr;
197400140718           when  V3Cncd > *zero;
197500140718           //when  V3Cncd > *zero   and
197600140718           //      %rem( V3Cnca - V3Cncd + 1 : 1000 ) > *zero;
197700140903             // -?Controllo da NON fare SE inserito il range dall'utente?
197800140903             //  ?(su richiesta di Mirko)?
197900140903             if  $CtrlRngSgnCl   and
198000140903                 %rem( V3Cncd - 1 : 5000 ) > *zero;
198100140903               V1Dmsg = %trimr($Msg(36));
198200140903               PosCurD3ncd = *on;
198300140903               ErrMessage  = *on;
198400140903               ErrGenerico = *on;
198500140903               leavesr;
198600140903             endif;
198700140718             if V3CncA = *hival;
198800140718               wCncA = V3CncA + 1;
198900140718             else;
199000140718               wCncA = V3CncA;
199100140718             endif;
199200140828             if  wCncA - V3CncD + 1 < 10000;
199300140828               V1Dmsg = %trimr($Msg(34)) +
199400140828                        ' DEVE prevedere almeno 10.000 colli';
199500140828               PosCurD3nca = *on;
199600140828               ErrMessage  = *on;
199700140828               ErrGenerico = *on;
199800140828               leavesr;
199900140828             endif;
200000140917             // -?Controllo da NON fare SE inserito il range dall'utente?
200100140917             //  ?(su richiesta di Mirko)?
200200140917             if  $CtrlRngSgnCl   and
200300140917                 %rem( wCncA - V3CncD + 1 : 5000 ) > *zero;
200400140718               V1Dmsg = %trimr($Msg(34)) +
200500140718                       ' NON è multiplo di 5.000';
200600140718               PosCurD3ncd = *on;
200700140718               ErrMessage  = *on;
200800140718               ErrGenerico = *on;
200900140718               leavesr;
201000140718             endif;
201100140123         endsl;
201200100205
201300100205         // -?Ricerca serie parziale?
201400100205         // ->?Selezione per numero colli?
201500100205         IF  V3Cncl > *zero;
201600100205
201700100205           clear  TNTB54ds;
201800100215           I54tel = 'R';
201900100215           I54fil = %editc( V1Cfls : 'X' );
202000100205           I54nrs = %editc( V1Cnrs : 'X' );
202100100205           I54tot = V3Cncl;
202200100205           kpjbu  = TNTB54ds;
202300110209           tntb54r ( kpjba : ubLeg3C_SKC );
202400100205           TNTB54ds = kpjbu;
202500100205
202600100205           select;
202700100205             // ->?errore SQL?
202800100205             when  O54err = 'S';
202900100210               V1Dmsg = $Msg(14);
203000100205               PosCurD3nrs = *on;
203100100205               ErrMessage  = *on;
203200100205               ErrGenerico = *on;
203300100205               leavesr;
203400100205             // ->?serie già presente ma parziale: errore bloccante?
203500100205             when  O54err = '1';
203600100210               V1Dmsg = $Msg(15);
203700100205               PosCurD3nrs = *on;
203800100205               ErrMessage  = *on;
203900100205               ErrGenerico = *on;
204000100205               leavesr;
204100120113             // ->?Serie NON ammessa (99)?
204200120113             when  O54err = '9';
204300120113               V1Dmsg = $Msg(28);
204400120113               PosCurD3nrs = *on;
204500120113               ErrMessage  = *on;
204600120113               ErrGenerico = *on;
204700120113               leavesr;
204800100205           endsl;
204900100205
205000100205           If  O54dal <> *zero;
205100140718             if  SavNRS <> O54nrs  or
205200140718                 SavNCD <> O54dal  or
205300140718                 SavNCA <> O54al;
205400140718               $CtrlRngSgnCl = *on;
205500140718               SavNRS = O54nrs;
205600140718               SavNCD = O54dal;
205700140718               SavNCA = O54al;
205800140718               clear  V3Cncl;
205900140718               V3Cnrs = O54nrs;
206000140718               V3Cncd = O54dal;
206100140718               V3Cnca = O54al;
206200140718               leavesr;
206300140718             endif;
206400100205             V1Cnrs = O54nrs;
206500100209             V1Cnsi = O54dal;
206600100209             V1Cnsf = O54al;
206700100205           Else;
206800100205             // ->?SE non è stata trovata la disponibilità di serie?
206900100205             //   ?parziale => ricerca per serie completa?
207000100205             clear  TRTB28ds2;
207100100210             ParFIL = V1Cfls;
207200100205             ParELB = 'R';
207300111215             trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
207400120113             select;
207500120113               // -?Serie NON ammessa (99)?
207600120113               when  ParERR = '9';
207700120113                 V1Dmsg = $Msg(28);
207800120113                 PosCurD3nrs = *on;
207900120113                 ErrMessage  = *on;
208000120113                 ErrGenerico = *on;
208100120113                 leavesr;
208200120113               // -?Non trovata serie completa libera?
208300120113               when  ParERR = 'E';
208400120113                 V1Dmsg = $Msg(09);
208500120113                 PosCurD3nrs = *on;
208600120113                 ErrMessage  = *on;
208700120113                 ErrGenerico = *on;
208800120113                 leavesr;
208900120113               // -?Trovata serie completa?
209000120113               other;
209100120113                 V1Cnrs = ParNRS;
209200120113                 V1Cnsi = 1;
209300120113                 V1Cnsf = V3Cncl;
209400120113             endsl;
209500100205           EndIf;
209600100205
209700100205         // ->?Selezione per segnacolli dal/al?
209800100205         ELSE;
209900101125
210000101125           exsr  sr_Ctrl_NRS_SCx;
210100101125           if  ErrGenerico = *on;
210200101125             PosCurD3nrs = PosCurNRS;
210300101125             leavesr;
210400101125           endif;
210500100205
210600100209           V1Cnrs = V3Cnrs;
210700100209           V1Cnsi = V3Cncd;
210800100209           V1Cnsf = V3Cnca;
210900100205
211000100205         ENDIF;
211100100205
211200100205       ENDSR;
211300110930
211400110930       //--------------------------------------------------------------
211500110930       //?Gestione videata W01 (Interrogazione legami EasySped).       ?
211600110930       //--------------------------------------------------------------
211700110930       BEGSR  sr_GesW01;
211800110930
211900110930         // -?Inizializzazione window?
212000110930         if  $InzW01 = *on;
212100110930           exsr  sr_InzW01;
212200110930           $InzW01 = *on;
212300110930         endif;
212400110930
212500110930         // -?Emissione window?
212600110930         exfmt  TB79W01;
212700110930
212800110930         // -?F12=Ritorno?
212900110930         if  dsp_aid = c_F12;
213000110930           $Video  = 'D2';
213100110930         endif;
213200110930
213300110930       ENDSR;
213400110930
213500110930       //--------------------------------------------------------------
213600110930       //?Inizializzazione window W01.                                 ?
213700110930       //--------------------------------------------------------------
213800110930       BEGSR  sr_InzW01;
213900110930
214000110930         clear  TB79W01;
214100110930
214200110930         clear  d3CE;
214300110930
214400110930         if  V1Cnrs = *zero;
214500110930           leavesr;
214600110930         endif;
214700110930
214800110930         // -?Reperimento tab. "3CE" per cliente in esame?
214900110930         if  getTabella ( c_Tntbe : '3CE' : %subst(V1Cksu : 2) :
215000110930                          *blank: *omit : *omit :
215100110930                          *omit : *omit :
215200110930                          *omit : *omit : *omit : *omit :
215300110930                          *omit : *omit : *omit : *omit :
215400110930                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
215500110930                          = *zero      AND
215600110930               ds_TNTBE.TBEatb = *blank;
215700110930           d3CE = ds_TNTBE.TBEuni;
215800110930         endif;
215900110930
216000110930         // -?Impostazione campi a video?
216100110930         if  d3CE.§3CEedat > *zero;
216200110930           wDate_Eur = %date( d3CE.§3CEedat : *iso );
216300110930           W1edat = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
216400110930                      ( %subdt(wDate_Eur : *months) * 10000 ) +
216500110930                      ( %subdt(wDate_Eur : *years) ) );
216600110930         endif;
216700110930         W1ever = d3CE.§3CEever;
216800110930         W1cver = d3CE.§3CEcver;
216900110930         if  d3CE.§3CEcdsc > *zero;
217000110930           wDate_Eur = %date( d3CE.§3CEcdsc : *iso );
217100110930           W1cdsc = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
217200110930                      ( %subdt(wDate_Eur : *months) * 10000 ) +
217300110930                      ( %subdt(wDate_Eur : *years) ) );
217400110930         endif;
217500110930         W1sncd = d3CE.§3CEsncd;
217600110930         W1snca = d3CE.§3CEsnca;
217700110930         W1sncc = d3CE.§3CEsncc;
217800110930         W1obj     = d3CE.§3CEobj;
217900110930         W1corpo   = d3CE.§3CEcorpo;
218000110930         W1typweb  = d3CE.§3CEtypweb;
218100110930         W1nrs3CE  = d3CE.§3CEnrs;
218200110930         W1cvermax = d3CE.§3CEcvermx;
218300110930         if  d3CE.§3CEcddemx > *zero;
218400110930           wDate_Eur = %date( d3CE.§3CEcddemx : *iso );
218500110930           W1cddemax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
218600110930                         ( %subdt(wDate_Eur : *months) * 10000 ) +
218700110930                         ( %subdt(wDate_Eur : *years) ) );
218800110930         endif;
218900110930         if  d3CE.§3CEcdscmx > *zero;
219000110930           wDate_Eur = %date( d3CE.§3CEcdscmx : *iso );
219100110930           W1cdscmax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
219200110930                         ( %subdt(wDate_Eur : *months) * 10000 ) +
219300110930                         ( %subdt(wDate_Eur : *years) ) );
219400110930         endif;
219500120316         W1esvsrv = d3CE.§3CEesvsrv;
219600110930
219700110930         // -?Decodifica il Tipo Aggiornamento Web?
219800110930         select;
219900110930           when  d3CE.§3CEtypweb = *blank;
220000110930           when  d3CE.§3CEtypweb = '0 ';
220100110930             W1typwebD = 'Funzionalità NON attiva';
220200110930           when  d3CE.§3CEtypweb = '1 ';
220300110930             W1typwebD = 'Funzionalità attiva';
220400110930           when  d3CE.§3CEtypweb = '2 ';
220500110930             W1typwebD = 'Framework 1.0';
220600110930           when  d3CE.§3CEtypweb = '4 ';
220700110930             W1typwebD = 'Framework 1.1';
220800110930           when  d3CE.§3CEtypweb = '6 ';
220900110930             W1typwebD = 'Framework 1.1 & 1.0';
221000110930           when  d3CE.§3CEtypweb = '8 ';
221100110930             W1typwebD = 'Framework 2.0';
221200110930           when  d3CE.§3CEtypweb = '10';
221300110930             W1typwebD = 'Framework 2.0 & 1.0';
221400110930           when  d3CE.§3CEtypweb = '12';
221500110930             W1typwebD = 'Framework 2.0 & 1.1';
221600110930           other;
221700110930             W1typwebD = *all'? ';
221800110930         endsl;
221900110930
222000110930       ENDSR;
222100111018
222200111018       //--------------------------------------------------------------
222300111018       //?Gestione videata W02 (Interrogazione legami EasySped).       ?
222400111018       //--------------------------------------------------------------
222500111018       BEGSR  sr_GesW02;
222600111018
222700111018         // -?Inizializzazione window?
222800111018         if  $InzW02 = *on;
222900111021           write  TB79W02a;
223000111018           exsr  sr_InzW02;
223100140304           $InzW02 = *off;
223200111018         endif;
223300111018
223400111018         // -?Emissione window?
223500111021         exfmt  TB79W02b;
223600111018
223700111018         // -?F12=Ritorno?
223800111018         if  dsp_aid = c_F12;
223900111018           $Video  = 'D2';
224000111018         endif;
224100111018
224200111018       ENDSR;
224300111018
224400111018       //--------------------------------------------------------------
224500111018       //?Inizializzazione window W02.                                 ?
224600111018       //--------------------------------------------------------------
224700111018       BEGSR  sr_InzW02;
224800111018
224900111021         clear  TB79W02b;
225000111018
225100111018         // -?Reperimento ultimo segnacollo usato?
225200121121         if  sch_SKC(1) = *zero;
225300121121           sch_SKC(1) = %int( %subst( V1Cksu : 2 ) );
225400121121         endif;
225500111018         if  ubLastNSC_Rtv ( V1Cnrs : V1Cfls :
225600111024                             ubLeg3C_SKC : *omit :
225700140225                             pOutMaxNSC : pOutMaxLNP :
225800140225                             pOutMaxAAS : pOutMaxMGS :
225900140225                             pOutMaxNSP :
226000140225                             pOutLastNSC : pOutLastLNP :
226100140225                             pOutLastAAS : pOutLastMGS :
226200140225                             pOutLastNSP ) < *zero;
226300111018           V1Dmsg = 'ERRORE nel reperim. dell''ultimo segnacollo +
226400111018                     utilizzato. Verificare in stampa.';
226500111018           errMessage  = *on;
226600111018           errGenerico = *on;
226700111018           leavesr;
226800111018         endif;
226900111018
227000111018         // -?Impostazione campi a video?
227100121121         W2Cfls  = V1Cfls;
227200121121         W2Cnrs  = V1Cnrs;
227300121121         W2Ccks  = %int( %subst( V1Cksu : 2 ) );
227400140225         W2CnscM = pOutMaxNSC;
227500140225         if  pOutMaxAAS > *zero  and  pOutMaxMGS > *zero;
227600140225           W2DgmasM  = (pOutMaxAAS * 10000) + pOutMaxMGS;
227700140225           wDate_Eur = %date( W2DgmasM : *iso );
227800140225           W2DgmasM  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
227900111018                         ( %subdt(wDate_Eur : *months) * 10000 ) +
228000111018                         ( %subdt(wDate_Eur : *years) ) );
228100111018         endif;
228200140225         W2CnscL = pOutLastNSC;
228300140225         if  pOutLastAAS > *zero  and  pOutLastMGS > *zero;
228400140225           W2DgmasL  = (pOutLastAAS * 10000) + pOutLastMGS;
228500140225           wDate_Eur = %date( W2DgmasL : *iso );
228600140225           W2DgmasL  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
228700140225                         ( %subdt(wDate_Eur : *months) * 10000 ) +
228800140225                         ( %subdt(wDate_Eur : *years) ) );
228900140225         endif;
229000111018
229100111018       ENDSR;
229200121102
229300121102       //--------------------------------------------------------------
229400121102       //?Gestione videata W03 (Visualizzaz. utente ultima modifica).  ?
229500121102       //--------------------------------------------------------------
229600121102       BEGSR  sr_GesW03;
229700121102
229800121102         // -?Inizializzazione window?
229900121102         if  $InzW03 = *on;
230000121102           exsr  sr_InzW03;
230100121102           $InzW03 = *on;
230200121102         endif;
230300121102
230400121102         // -?Emissione window?
230500121102         exfmt  TB79W03;
230600121102
230700121102         // -?F12=Ritorno?
230800121102         if  dsp_aid = c_F12;
230900121102           $Video  = 'D2';
231000121102         endif;
231100121102
231200121102       ENDSR;
231300121102
231400121102       //--------------------------------------------------------------
231500121102       //?Inizializzazione window W03.                                 ?
231600121102       //--------------------------------------------------------------
231700121102       BEGSR  sr_InzW03;
231800121102
231900121102         clear  TB79W03;
232000121102
232100121102         // -?Impostazione campi a video?
232200121102         W3uum = d3EW.§3EWuum;
232300121102         if  TBEdtr > *zero;
232400121102           wDate_Eur = %date( TBEdtr : *iso );
232500121102           W3dtr = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
232600121102                     ( %subdt(wDate_Eur : *months) * 10000 ) +
232700121102                     ( %subdt(wDate_Eur : *years) ) );
232800121102         endif;
232900121102
233000121102         // -?Decodifica profilo utente ultima modifica?
233100121102         if  not %open(KPPRF01L);
233200121102           open  KPPRF01L;
233300121102         endif;
233400121102         chain  (d3EW.§3EWuum)  KPPRF000;
233500121102         if  %found(KPPRF01L);
233600121102           W3uumD = PRFtxt;
233700121102         endif;
233800121102         close  KPPRF01L;
233900121102
234000121102       ENDSR;
234100100209
234200100209       //--------------------------------------------------------------
234300100209       //?Aggiornam. tab. "3EW" nei file di tutti i sistemi informativi?
234400100209       //--------------------------------------------------------------
234500100209       BEGSR  sr_UpdateAll;
234600100209
234700100209         // -?Ciclo di elaborazione per ogni sistema informativo?
234800100209         For  w_SisInf = 1  To  %elem($Libr);
234900100209
235000100209           // -?Apertura degli archivi?
235100100209           if  w_SisInf > 1;
235200100209             exsr  sr_OpenFileTab;
235300100209           endif;
235400100209
235500100209           // -?Aggiornamento tab. "3EW"?
235600100209           exsr  sr_UpdTNTBE;
235700100518
235800100518           // -?Aggiornamento tab. "3C"?
235900100518           exsr  sr_UpdTABEL;
236000100209
236100100209         EndFor;
236200100209
236300100209       ENDSR;
236400100205
236500100205       //--------------------------------------------------------------
236600100518       //?Aggiornamento records TNTBE00F (tab. "3EW").                 ?
236700100205       //--------------------------------------------------------------
236800100205       BEGSR  sr_UpdTNTBE;
236900100209
237000100209         //? N.B.                                                      ?
237100100209         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
237200100209         //   trasmissione (vedi flag TBEFTR).                        ?
237300100209         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
237400100209         //   subito nello stesso file di entrambi i S.I. - in due    ?
237500100209         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
237600100205
237700100209         // -?RI-aggancio record da aggiornare?
237800100209         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
237900110422         // =>?RI-aggancio record da aggiornare COMUNQUE?
238000110422         //   ?perchè possono essre state fatte letture della tab."3CP"?
238100110422         //   ?(SE si è in inserimento)?
238200110422         //if  w_SisInf > 1;
238300100209           k_TBEcod = c_Tab;
238400100210           k_TBEke1 = V1Cksu;
238500100210           k_TBEke2 = V1Csun;
238600100209           clear  k_TBElin;
238700100209           clear  k_TBEsif;
238800100209           //k_TBElin = TBXlin;
238900100209           //k_TBEsif = TBXsif;
239000100209           chain  %kds(K05tntbe01)  TNTBE000;
239100110422         //endif;
239200100205
239300100209         // -?Aggiornamento tab. "3EW"?
239400100209         select;
239500100205
239600100205           // -?F5=Ripristino?
239700100209           when  dsp_aid = c_F05;
239800100330             exsr  sr_RieRecTBE;
239900100209             select;
240000100312               when  %found(TNTBE01L)   and   TBEatb <> *blank;
240100100209                 clear  TBEatb;
240200100209                 //_______________
240300100209                 UPDATE  TNTBE000;
240400100209                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
240500100209               // -?Record appena cancellato fisicamente?
240600100209               when  NOT %found(TNTBE01L);
240700110725                 clear  TBEatb;
240800100209                 //_______________
240900100209                 WRITE   TNTBE000;
241000100209                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
241100100209             endsl;
241200100205
241300100209           // -?F6 = Aggiornamento / Inserimento?
241400100209           when  dsp_aid = c_F06;
241500100209             exsr  sr_RieRecTBE;
241600110725             clear  TBEatb;
241700100205             if  %found(TNTBE01L);
241800100205               //_____________
241900100205               UPDATE TNTBE000;
242000100205               //¯¯¯¯¯¯¯¯¯¯¯¯¯
242100100205             else;
242200100205               //_____________
242300100205               WRITE  TNTBE000;
242400100205               //¯¯¯¯¯¯¯¯¯¯¯¯¯
242500100205             endif;
242600100209
242700100209           // -?F16=Annullamento?
242800100209           when  dsp_aid = c_F16;
242900100312             if  %found(TNTBE01L)   and   TBEatb <> 'A';
243000121102               //d3EW = TBEuni;         ?(già fatto)?
243100121102               d3EW.§3EWuum = DUTute;
243200121102               TBEuni = d3EW;
243300100209               TBEatb = 'A';
243400100209               //TBEftt = W1ftt;
243500100209               TBEftt = 'S';
243600100209               clear  TBEftr;
243700100209               //_______________
243800100209               UPDATE  TNTBE000;
243900100209               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
244000100209             endif;
244100130827
244200130827           // -?F17=Annullamento Applicativo?
244300130827           when  dsp_aid = c_F17;
244400130827             if  %found(TNTBE01L)   and   TBEatb <> 'A'
244500130827                                    and   D3EW.§3EWfaa <> 'A';
244600130827               //d3EW = TBEuni;         ?(già fatto)?
244700130827               exsr  sr_RieRecTBE;
244800130827               d3EW.§3EWfaa = 'A';
244900130827               d3EW.§3EWdaa = %editc( wDate : 'X' );
245000130827               d3EW.§3EWuum = DUTute;
245100130827               TBEuni = d3EW;
245200130827               //_____________
245300130827               UPDATE TNTBE000;
245400130827               //¯¯¯¯¯¯¯¯¯¯¯¯¯
245500130827             endif;
245600100205
245700100209         endsl;
245800100205
245900100205       ENDSR;
246000100209
246100100209       //--------------------------------------------------------------
246200100518       //?Caricamento dati in tab. "3EW".                              ?
246300100209       //--------------------------------------------------------------
246400100209       BEGSR  sr_RieRecTBE;
246500100209
246600100209         // -?Impostazione campi chiave se inserimento?
246700100209         if  NOT %found(TNTBE01L);
246800100209           clear TNTBE000;
246900100209           TBEcod = k_TBEcod;
247000100209           TBEke1 = k_TBEke1;
247100100209           TBEke2 = k_TBEke2;
247200100209           TBElin = k_TBElin;
247300100209           TBEsif = k_TBEsif;
247400100209         endif;
247500100209
247600100209         // -?Impostazione dati tabella da aggiornare?
247700100209         exsr  sr_Rie3EW;
247800100209         TBEuni = d3EW;
247900100209
248000100209         // -?Impostazione altri campi del record?
248100100209         TBEapl = TBXapl;
248200100518         //clear  TBEatb;       //Sarà impostato caso per caso...?
248300100209         //TBEftt = W1ftt;
248400100209         TBEftt = 'S';
248500100209         clear  TBEflt;
248600100209         if  w_SisInf = 1;
248700100209           TBEftr = 'T';
248800100209         else;
248900100209           TBEftr = 'R';
249000100209         endif;
249100100209         TBEdtr = wDate;
249200100209
249300100209       ENDSR;
249400100205
249500100205       //--------------------------------------------------------------
249600100518       //?Caricamento dati in tab. "3EW".                              ?
249700100205       //--------------------------------------------------------------
249800100209       BEGSR  sr_Rie3EW;
249900100209
250000100209         clear  d3EW;
250100100209
250200100330         d3EW.§3EWlnp = %int(V1Clnp);
250300100330         d3EW.§3EWfls = V1Cfls;
250400100330         d3EW.§3EWnrs = V1Cnrs;
250500100330         d3EW.§3EWnsi = V1Cnsi;
250600100330         d3EW.§3EWnsf = V1Cnsf;
250700100330         d3EW.§3EWctm = V1Cctm;
250800100330         d3EW.§3EWmad = V1Cmad;
250900100330         d3EW.§3EWaew = V1Caew;
251000100330         d3EW.§3EWupd = V1Cupd;
251100120814         d3EW.§3EWfaa = V1Cfaa;
251200120814         if  V1Cdaa > *zero;
251300120814           //clear WLBdat;             //(già fatto in sr_CtrD02)?
251400120814           //G08dat = V1Cdaa;          //(già fatto in sr_CtrD02)?
251500120814           //XSRDA8(WLBdat);           //(già fatto in sr_CtrD02)?
251600120814           //if  G08err = *off;        //(già fatto in sr_CtrD02)?
251700120814             d3EW.§3EWdaa = %editc( G08inv : 'X' );
251800120814           //endif;
251900120814         endif;
252000121102         d3EW.§3EWuum = DUTute;
252100100209
252200100209       ENDSR;
252300100518
252400100518       //--------------------------------------------------------------
252500100518       //?Aggiornamento record nel file TABEL00F (tab. "3C").          ?
252600100518       //--------------------------------------------------------------
252700100518       BEGSR  sr_UpdTABEL;
252800100518
252900100518         //? N.B.                                                      ?
253000100518         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
253100100518         //   trasmissione (vedi flag TBEFTR).                        ?
253200100518         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
253300100518         //   subito nello stesso file di entrambi i S.I. - in due    ?
253400100518         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
253500100518
253600100518         // -?Aggancio record da aggiornare?
253700100518         k_TBLkut = 1;
253800100518         k_TBLcod = '3C';
253900100518         k_TBLkey = %subst( V1Cksu : 2 );
254000100518         chain  %kds(k03tabel00)  TABEL;
254100100518
254200100518         // -?Aggiornamento tab. "3C"?
254300100518         if  %found(TABEL00F);
254400100518           ds3C   = TBLuni;
254500121220           if  ds3C.§3Ccba <> c_Supp_ESWEB;
254600121220             unlock  TABEL00F;
254700121220             leavesr;
254800121220           endif;
254900110422           ds3C.§3Cnrs = V1Cnrs;
255000100518           TBLuni = ds3C;
255100100518         endif;
255200100518
255300100518         select;
255400100518
255500100518           // -?Record NON trovato?
255600100518           when  NOT %found(TABEL00F);
255700100518             leavesr;
255800100518
255900100518           // -?Record annullato (verrà presto cancellato fisicamente)?
256000100518           when  TBLflg <> *blank;
256100100518             leavesr;
256200100518
256300100518           // -?F5 = Ripristino (NON gestito)?
256400100518           when  dsp_aid = c_F05;
256500100518             //if  TBLflg <> *blank;
256600100518             //  clear  TBLflg;
256700100518             //  clear  TBLftr;
256800100518             //  clear  TBLdtr;
256900100518             //  //____________
257000100518             //  update  TABEL;
257100100518             //  //¯¯¯¯¯¯¯¯¯¯¯¯
257200100518             //endif;
257300100518
257400100518           // -?F6 = Aggiornamento (NO Inserimento)?
257500100518           when  dsp_aid = c_F06;
257600100518             TBLftt = '1';
257700100518             clear  TBLflt;
257800100518             if  w_SisInf = 1;
257900100518               TBLftr = 'T';
258000100518             else;
258100100518               TBLftr = 'R';
258200100518             endif;
258300100518             TBLdtr = %int( %subst( %editc(wDate : 'X') :
258400100518                                    %len(wDate) - %len(TBLdtr) + 1 :
258500100518                                    %len(TBLdtr) ) );
258600100518             //____________
258700100518             update  TABEL;
258800100518             //¯¯¯¯¯¯¯¯¯¯¯¯
258900100518
259000100518           // -?F16 = Annullamento (NON gestito)?
259100100518           when  dsp_aid = c_F16;
259200100518             //if  TBLflg = *blank;
259300100518             //  TBLflg = '*';
259400100518             //  clear  TBLftr;
259500100518             //  clear  TBLdtr;
259600100518             //  //____________
259700100518             //  update  TABEL;
259800100518             //  //¯¯¯¯¯¯¯¯¯¯¯¯
259900100518             //endif;
260000100518
260100100518         endsl;
260200100518
260300100518       ENDSR;
260400100205
260500100205       //--------------------------------------------------------------
260600100205       //?Operazioni finali.                                           ?
260700100205       //--------------------------------------------------------------
260800100205       BEGSR  sr_RoutEnd;
260900100205
261000100205         // -?Chiusura funzioni precedentemente aperte?
261100100217         reset  TIBS69ds;
261200100217         //I69tla = 'C';         ?(già così)?
261300100205         tibs69r( tibs69ds :
261400100205                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
261500100211
261600100211         clear  TNTB79ds1;
261700100211         TNTB79ds1.i3EWtla = 'C';
261800100211         tntb79r1 ( kpjba : tntb79ds1 );
261900100211
262000100211         cloTabella ( c_Tabel );
262100110422
262200110422         if  %parms() > 1;
262300110422           ubNrsRsc_Close ();
262400110422         endif;
262500100205
262600100208         Selettore_Record_TIVSS ( 'SETONLR' );
262700100210
262800100210         ubLeg3C_Close ();
262900100215
263000100215         // -?Restituzione eventuali parametri?
263100100215         if  %parms() > 1;
263200100215           p_TNTB79ds = TNTB79ds;
263300100215         endif;
263400100205
263500100205         // -?Chiusura pgm?
263600100205         return;
263700100205
263800100205       ENDSR;
263900100205
264000100205      /end-free
264100100205
264200100205       //--------------------------------------------------------------
264300100205       //?Definizione schiere a tempo di compilazione                  ?
264400100205       //--------------------------------------------------------------
264500100205
264600100210** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
264700100205SETRAS  GAITRAGRU FILTRAGRU
264800100205AS888   GAITRAGRPSFILTRAGRPF
264900100210** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
265000100205Immettere entrambi i codici cliente: codice Padre + Strategi User Number        1
265100100205Cliente NON presente in anagrafica                                              2
265200100210Cliente NON presente in tab. "3EW" - non è consentito l'inserimento da menù     3
265300100210Cliente annullato o bloccato                                                    4
265400100210Cliente NON abilitato a STRATEGI                                                5
265500121127Abilitazione del cliente a STRATEGI  scaduta                                   -- NO
265600121127Abilitazione del cliente a STRATEGI  NON ancora valida                         -- NO
265700100210Numero SERIE già presente: premere F10 per richiederne uno nuovo                8
265800100210Non ci sono più numeri di SERIE attribuibili tra 1 e 99                         9
265900100210Effettuare una selezione                                                       10
266000100210Numero Colli inseribile alternativamente alla Serie ed ai relativi Segnacolli  11
266100100210Numero segnacolli "AL" obbligatorio                                            12
266200100210Range segnacolli errato (DAL > AL)                                             13
266300100304Errore istruzione SQL - Contattare il CED                                      14
266400100210Segnacolli NON disponibili per la serie indicata                               15
266500100210SERIE già assegnata al cliente                                                 16
266600100302Linea di partenza obbligatoria                                                 17
266700100302Linea di partenza errata                                                       18
266800100302Filiale segnacollo obbligatoria                                                19
266900100302Filiale segnacollo errata                                                      20
267000100302Numero di serie obbligatorio.                                                  21
267100100304SERIE già assegnata parzialmente al cliente                                    22
267200100304Codice Trattamento Merce errato                                                23
267300100304NON ammessi numeri negativi                                                    24
267400100304ATTENZIONE: il cliente NON potrà salvarsi i destinatari                        25
267500100304Flag abilitazione a creare utenti EasyWeb di tipo "bollettisti" errato         26
267600121127Cliente NON annullabile perchè padre di altri clienti abilitati ad EasyWeb     -- NO
267700120113SERIE non gestibile                                                            28
267800120202SERIE già assegnata  e  NON utilizzabile parzialmente                          29
267900120814Flag annullamento applicativo errato: " "=Attivo, "A"=Annullato                30
268000120814Flag annullamento applicativo NON impostabile per cliente ancora EasySped-WEB  31
268100120814Flag e Data annullamento applicativo: impostare entrambi o nessuno dei due     32
268200120814Data annullamento applicativo errata o mancante                                33
268300121127Range segnacolli ANOMALO:                                                      34
268400121129SERIE/RANGE segnacolli NON più attribuibili al cliente se si cambia supporto   35
268500140718Il segnacollo iniziale DEVE essere multiplo di 5.000 (+ 1)                     36
268600150305ATTENZIONE: valore per max anagrafiche destinatari molto elevato               37
268700150305ATTENZIONE: diminuire il valore max destinatari potrebbe causare problemi      38
