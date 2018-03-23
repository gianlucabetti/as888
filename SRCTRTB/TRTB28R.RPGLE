000100091103       //==============================================================
000200091127       //?TRTB28R * Manutenzione tab. "3C" = Invio dati BOLLETTAZIONE  ?
000300170322       //==============================================================
000400100312
000500100312       //--------------------------------------------------------------
000600100312       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700100312       //--------------------------------------------------------------
000800100312
000900120518     /*PRM  dbgview(*source)
001000120518     /*CMD  ovrdbf file(FNBLP00F) tofile(FILTRAPRD/FNBLP00F) +
001100120518     /*CMD         ovrscope(*calllvl)
001200120518     /*END  dltovr file(FNBLP00F) lvl(*)
001300100312     /*END
001400100312
001500100312       //--------------------------------------------------------------
001600100312       //?Specifiche di controllo.                                     ?
001700100312       //--------------------------------------------------------------
001800091102
001900091102     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000170825     h dftactgrp(*no)
002100101005     h bnddir('UBBNDDIR':'TRUL')
002200091102
002300091102       //--------------------------------------------------------------
002400091102       //?Dichiarazione file.                                          ?
002500091102       //--------------------------------------------------------------
002600091102
002700091126       // -?Organigramma filiale/aziendale?
002800091102     fAZORG01L  if   e           k disk
002900091102
003000091126       // -?Tabella "3C"?
003100091102     fTABEL00F  Uf A e           k disk
003200091109     f                                     extfile(wLibFile1)  usropn
003300091126       // -?Tabelle "3CE" e "3CP"?
003400091102     fTNTBE01L  Uf A e           k disk
003500091109     f                                     extfile(wLibFile2)  usropn
003600091102
003700091126       // -?Video Gestione?
003800091127     fTRTB28D   cf   e             workstn
003900091102     f                                     indds(IndDspF)
004000091102     f                                     infds(InfDspF)
004100091102
004200091102       //--------------------------------------------------------------
004300091102       //?Definizione costanti.                                        ?
004400091102       //--------------------------------------------------------------
004500091102
004600091126       // -?Codice tabella in gestione?
004700091104     d c_Tab           c                   const('3C')
004800091102
004900091126       // -?Codice utente?
005000091102     d c_Kut           c                   const(1)
005100091102
005200091126       // -?Costante utente "EDP"?
005300091102     d c_Edp           c                   const('EDP')
005400120214
005500120214       // -?Costante Supporto "EasySped"?
005600120214     d c_EasySped      c                   const('ESYSP')
005700091124
005800091126       // -?Costante Supporto "EasyWEB"?
005900091124     d c_EasyWEB       c                   const('ESWEB')
006000091126
006100091126       // -?Costante codice trattamento merce x supporto "EasyWEB"?
006200091127     d c_CTMxEasyWeb   c                   const('2 ')
006300091104
006400091126       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
006500091104     d c_TABEL         c                   const('1')
006600091126       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
006700091104     d c_TNTBE         c                   const('2')
006800091102
006900091126       // -?Costante per controllo "caratteri solo numerici"?
007000091106     d c_Digits        c                   const('0123456789')
007100091105
007200091126       // -?Costanti per la definizione delle schiere con i nomi?
007300091126       //  ?degli iSeries da elaborare e delle relative librerie?
007400091105     d c_NrSyst        c                   const(2)
007500091105     d c_NrLibr        c                   const(2)
007600091102
007700091126       // -?Tasti funzionali a video?
007800091102     d c_F01           c                   const(x'31')
007900091102     d c_F02           c                   const(x'32')
008000091102     d c_F03           c                   const(x'33')
008100091103     d c_F04           c                   const(x'34')
008200091102     d c_F05           c                   const(x'35')
008300091102     d c_F06           c                   const(x'36')
008400091102     d c_F07           c                   const(x'37')
008500091102     d c_F08           c                   const(x'38')
008600091102     d c_F09           c                   const(x'39')
008700091102     d c_F10           c                   const(x'3A')
008800091103     d c_F11           c                   const(x'3B')
008900091102     d c_F12           c                   const(x'3C')
009000091102     d c_F13           c                   const(x'B1')
009100091102     d c_F14           c                   const(x'B2')
009200091102     d c_F15           c                   const(x'B3')
009300091102     d c_F16           c                   const(x'B4')
009400091102     d c_F17           c                   const(x'B5')
009500091102     d c_F18           c                   const(x'B6')
009600091102     d c_F19           c                   const(x'B7')
009700091102     d c_F20           c                   const(x'B8')
009800091102     d c_F21           c                   const(x'B9')
009900091102     d c_F22           c                   const(x'BA')
010000091102     d c_F23           c                   const(x'BB')
010100091102     d c_F24           c                   const(x'BC')
010200091102     d c_Enter         c                   const(x'F1')
010300091102     d c_RollDown      c                   const(x'F4')
010400091102     d c_RollUp        c                   const(x'F5')
010500091102
010600091102       //--------------------------------------------------------------
010700091102       //?Definizione schiere.                                         ?
010800091102       //--------------------------------------------------------------
010900091103
011000091126       // -?iSeries  &  Librerie con entrambi i file tabelle?
011100091105     d $iSystem        s                   like(currSysNetA)
011200091105     d                                     dim(c_NrSyst)
011300091105     d                                     ctdata   perrcd( 1)
011400100212     d $Libraries      s                   like(ds_Libr)
011500091105     d                                     dim(c_NrSyst)
011600091106     d                                     alt($iSystem)
011700091102
011800091126       // -?Messaggi di errore?
011900160317     d $Msg            s             78    dim(49) ctdata perrcd(1)
012000110629
012100110629       // -?Codici clienti della famiglia originale in tab. "3C"?
012200110629     d sch_SKC_Orig    s              7p 0 dim(800) inz
012300091102
012400091102       //--------------------------------------------------------------
012500091102       //?Definizione aree dati.                                       ?
012600091102       //--------------------------------------------------------------
012700091102
012800091126       // -?Dati utente?
012900091102     d §AzUte        e ds                  extname(AZUTE00F)
013000091102     d                                     dtaara
013100091102     d §DatiUte      e ds                  extname(dDatiUte)
013200091102     d                                     dtaara
013300091102
013400091102       //--------------------------------------------------------------
013500091102       //?Definizione strutture dati.                                  ?
013600091102       //--------------------------------------------------------------
013700091102
013800091126       // -?Status ds?
013900091102     d Status         sds
014000120605     d   SDSpgm          *proc
014100091102
014200091126       // -?InfDS?
014300091102     d InfDspF         ds
014400091102     d   dsp_aid             369    369a
014500170321     d wDsp_Aid        s                   like(dsp_aid)
014600091102
014700091126       // -?Indicatori su DspF?
014800091102     d IndDspF         ds                  inz
014900091126         // -?Abilitazione tasti funzionali?
015000110311     d   F1Attivo                      n   overlay(IndDspF : 01)
015100121128     d   F2Attivo                      n   overlay(IndDspF : 02)
015200121128     d   F3Attivo                      n   overlay(IndDspF : 03)
015300091102     d   F5Attivo                      n   overlay(IndDspF : 05)
015400091102     d   F6Attivo                      n   overlay(IndDspF : 06)
015500091214     d   F8Attivo                      n   overlay(IndDspF : 08)
015600120517     d   F9Attivo                      n   overlay(IndDspF : 09)
015700091102     d   F10Attivo                     n   overlay(IndDspF : 10)
015800091102     d   F11Attivo                     n   overlay(IndDspF : 11)
015900091102     d   F12Attivo                     n   overlay(IndDspF : 12)
016000100212     d   F13Attivo                     n   overlay(IndDspF : 13)
016100111025     d   F14Attivo                     n   overlay(IndDspF : 14)
016200091102     d   F16Attivo                     n   overlay(IndDspF : 16)
016300091102     d   F17Attivo                     n   overlay(IndDspF : 17)
016400091102     d   F18Attivo                     n   overlay(IndDspF : 18)
016500091102     d   F19Attivo                     n   overlay(IndDspF : 19)
016600091102     d   F20Attivo                     n   overlay(IndDspF : 20)
016700091102     d   F21Attivo                     n   overlay(IndDspF : 21)
016800091102     d   F22Attivo                     n   overlay(IndDspF : 22)
016900091120     d   RlUpAttivo                    n   overlay(IndDspF : 25)
017000170321     d   W3_3CLAttiva                  n   overlay(IndDspF : 30)
017100091126         // -?Emissione messaggio di errore?
017200091102     d   ErrMessage                    n   overlay(IndDspF : 28)
017300091126         // -?Indicatori per Attibuti di visualizzazione?
017400091102     d   VisBloAnn                     n   overlay(IndDspF : 40)
017500091124     d   VisInfoES                     n   overlay(IndDspF : 41)
017600091125     d   ProtectNRS                    n   overlay(IndDspF : 42)
017700091125     d   ProtectCTM                    n   overlay(IndDspF : 43)
017800100211     d   Protect_3EW                   n   overlay(IndDspF : 44)
017900100212     d   Reverse_3EW                   n   overlay(IndDspF : 45)
018000120130     d   $FlsNrsIn3CL                  n   overlay(IndDspF : 46)
018100150127     d   VisLNP3EW                     n   overlay(IndDspF : 47)
018200091126         // -?Posizionamento cursore & Segnalazione errore?
018300091125     d   PosCurCLI                     n   overlay(IndDspF : 51)
018400091125     d   PosCurFIL                     n   overlay(IndDspF : 52)
018500091125     d   PosCurCKS                     n   overlay(IndDspF : 53)
018600091125     d   PosCurCBA                     n   overlay(IndDspF : 54)
018700091125     d   PosCurNRS                     n   overlay(IndDspF : 55)
018800091125     d   PosCurCTM                     n   overlay(IndDspF : 56)
018900091125     d   PosCurBAC                     n   overlay(IndDspF : 57)
019000091222     d   PosCurBC2                     n   overlay(IndDspF : 58)
019100091222     d   PosCurABD                     n   overlay(IndDspF : 59)
019200091104     d   PosCurLRM                     n   overlay(IndDspF : 71)
019300091104     d   PosCurALM                     n   overlay(IndDspF : 72)
019400091104     d   PosCurNOT                     n   overlay(IndDspF : 73)
019500091104     d   PosCurABC                     n   overlay(IndDspF : 74)
019600091104     d   PosCurARC                     n   overlay(IndDspF : 75)
019700091104     d   PosCurAVX                     n   overlay(IndDspF : 76)
019800091102     d   PosCurW1ncl                   n   overlay(IndDspF : 81)
019900091102     d   PosCurW1nrs                   n   overlay(IndDspF : 82)
020000091102     d   PosCurW1ncd                   n   overlay(IndDspF : 83)
020100091102     d   PosCurW1nca                   n   overlay(IndDspF : 84)
020200101028     d   PosCurW4sun                   n   overlay(IndDspF : 85)
020300160317     d   PosCurAmb                     n   overlay(IndDspF : 86)
020400091126         //   -?Riemissione videata?
020500091102     d   ErrGenerico                   n   overlay(IndDspF : 99)
020600091102
020700091126       // -?Parametri ricevuti?
020800091102     d KPJBA         e ds
020900091102
021000091126       // -?Tabelle usate:?
021100091126       // ·?"1B" = Codici Trattamento Merce?
021200091103     d ds1B          e ds                  inz
021300091126       // ·?"3C" = Clienti che inviano bolle partenza?
021400091102     d ds3C          e ds                  inz
021500091124     d ds3C_Padre    e ds                  inz  extname(ds3C)
021600091124     d                                          prefix(P_)
021700100921     d ds3C_Old      e ds                  inz  extname(ds3C)
021800100921     d                                          prefix(Old_)
021900091126       // ·?"3CE" = Versioni Easy-Sped per cliente?
022000091102     d d3CE          e ds                  inz
022100091126       // ·?"3CP" = Serie parziali assegnate a clienti?
022200091102     d d3CP          e ds                  inz
022300110311     d d3CP_Padre    e ds                  inz  extname(d3CP)
022400110311     d                                          qualified
022500100211       // ·?"3EW" = Dati assegnati alla postazione EasyWeb?
022600100326     d d3EW          e ds                  inz  qualified
022700110311     d d3EW_Padre    e ds                  inz  extname(d3EW)
022800110311     d                                          qualified
022900121112     d d3EW_Old      e ds                  inz  extname(d3EW)
023000121112     d                                          qualified
023100120814     d d3EW_Temp     e ds                  inz  extname(d3EW)
023200120814     d                                          qualified
023300110629
023400110629       // -?Abilitazione al servizio internet del padre originale?
023500110629     d TIVSSds_Orig  e ds                  inz  extname(TIVSS00F)
023600110629     d                                          qualified
023700091106
023800091126       // -?Tracciato record file TNTBE00F per dati di base?
023900091126       //  ?della tab. "3CP"?
024000091106     d xTNTBE_3CPds  e ds                  inz  extname(TNTBE00F)
024100091106     d                                          prefix(X3CP : 3)
024200100401
024300100401       // -?Tracciato record file TNTBE00F per dati di base?
024400100401       //  ?della tab. "3EW"?
024500100401     d xTNTBE_3EWds  e ds                  inz  extname(TNTBE00F)
024600100401     d                                          prefix(X3EW : 3)
024700091105
024800091126       // -?Ridefinizione elenco librerie in elaborare le tabelle?
024900091105     d ds_Libr         ds                  inz
025000120605     d   $Libr                       10    dim(c_NrLibr) inz
025100091102
025200091102       //--------------------------------------------------------------
025300091102       //?Definizione variabili globali.                               ?
025400091102       //--------------------------------------------------------------
025500091102
025600091126       // -?Flags booleani?
025700091102     d $Fine           s               n   inz
025800091102     d $InzD01         s               n   inz(*on)
025900091102     d $InzD02         s               n   inz(*on)
026000091104     d $InzW01         s               n   inz(*on)
026100091104     d $InzW02         s               n   inz(*on)
026200101028     d $InzW04         s               n   inz(*on)
026300111011     d $InzW05         s               n   inz(*on)
026400091102     d $Called         s               n   inz
026500100514     d $3EWupd         s               n   inz
026600120807     d $TrTb06         s               n   inz
026700091102     d $Protect        s               n   inz
026800091106     d $Found_3CP      s               n   inz
026900100312     d $Found_TIVSS    s               n   inz
027000120605     d $Annull_3EW     s               n   inz
027100091106     d $KB4            s               n   inz
027200091125     d $TIS7701        s               n   inz
027300100212     d $EasyWeb        s               n   inz
027400120518     d $Bypass_01      s               n   inz
027500121128     d $F2inD2         s               n   inz
027600140718     d $CtrlRngSgnCl   s               n   inz
027700091102
027800091126       // -?Variabili per la gestione del video?
027900091102     d $Video          s              2    inz('D1')
028000091106     d $VideoPrec      s                   like($Video)  inz
028100091102
028200091126       // -?Codice cliente eventualmente ricevuto?
028300091104     d W0Ccli          s                   like(V1Ccli)  inz
028400100211
028500100211       // -?Conteggio dei rec. in tab. "3EW" per l'unficante?
028600120725     d wCount_3EW      s              3  0 inz(*loval)
028700110311
028800110311       // -?Segnacollo iniziale per padre (da tab."3CP")?
028900110311     d wNSI_Padre      s                   like(V2Cncd)  inz
029000091102
029100091126       // -?Memo per controllo variazioni a video?
029200100623     d SavNRS          s                   like(V2Cnrs)  inz(*hival)
029300120131     d*// SavNCD          s                   like(V2Cncd)  inz(*hival)
029400120131     d*// SavNCA          s                   like(V2Cnca)  inz(*hival)
029500100623     d SavABD          s                   like(V2Cabd)  inz(*hival)
029600100623     d SavCKS          s                   like(V2Ccks)  inz(*hival)
029700100623     d SavCBA          s                   like(V2Ccba)  inz(*hival)
029800100623     d SavCTM          s                   like(V2Cctm)  inz(*hival)
029900120725     d SaveFLS         s                   like(V2Dfls)  inz
030000150127     d SaveLNP         s                   like(V23EWLNP)  inz
030100120725     d SaveNRS         s                   like(V2Cnrs)  inz
030200120725     d SaveNCD         s                   like(V2Cncd)  inz
030300120725     d SaveNCA         s                   like(V2Cnca)  inz
030400120725     d SaveCTM         s                   like(V2Cctm)  inz
030500140718     d SavW1Cnrs       s                   like(W1Cnrs)  inz
030600140718     d SavW1Cncd       s                   like(W1Cncd)  inz
030700140718     d SavW1Cnca       s                   like(W1Cnca)  inz
030800121220     d Save_3CP_nrs    s                   like(V2Cnrs)  inz
030900121220     d Save_3CP_nsi    s                   like(V2Cncd)  inz
031000121220     d Save_3CP_nsf    s                   like(V2Cnca)  inz
031100100623     d SavTBLkey_tmp   s                   like(TBLkey)  inz(*hival)
031200110314     d SavCliTab3CPEW  s             10a                 inz(*hival)
031300110422     d SavF1Attivo     s               n                 inz
031400160125     d Save3CLNrS      s                   like(I54Nrs)  inz
031500160125     d Save3CLLNP      s                   like(I54Fil)  inz
031600091106
031700091126       // -?Nome del sistema?
031800091106     d currSysNeta     s              8a   inz
031900091105
032000091126       // -?Nome esteso Libreria/File dei 2 file tabella?
032100091109     d wLibFile1       s             21a   inz
032200091109     d wLibFile2       s             21a   inz
032300091102
032400091126       // -?Campi di comodo?
032500091105     d wDate_EUR       s               d   datfmt(*eur)  inz
032600130520     d wDate_DMY       s               d   datfmt(*dmy)  inz
032700091105     d wDate           s              8  0 inz
032800130520     d wDate6          s              6  0 inz
032900091105     d w_iSystem       s              1  0 inz
033000091105     d w_SisInf        s              3  0 inz
033100110616     d wVSSksu         s                   like(TIVSSds.VSSksu)
033200170321     d wForzatura      s               n
033300110616     d                                     inz
033400140718     d*// wW1Cncx         s                   like(W1Cncd)  inz
033500140317     d wCnca           s              8  0 inz
033600170825
033700170825       // parametri per moduli VCP
033800170825       // nome contatore
033900171019     d iVCPTpID        s             15a
034000171019       // nome contatore
034100171019     d iVCPID          s             15a
034200170825       // 0=OK -1=errore generico nell'elaborazione -2=errore nei parametri inpu
034300170825     d oVCPEsito       s              2s 0
034400091102
034500091102       //--------------------------------------------------------------
034600091102       //?Definizione prototipi procedure.                             ?
034700091102       //--------------------------------------------------------------
034800091126
034900091126       // -?Reperimento dati utente?
035000110209     d TIBS34ds      e ds                  inz
035100091102      /copy gaitrasrc/srcProtoPR,TIBS34R
035200091102
035300110209       // -?Parametri per pgm interrogazione tabelle (TABEL)?
035400100308      /copy gaitrasrc/srcProtoPI,X§TABER
035500110209       // -?Interrogazione tabelle (TABEL)?
035600091102      /copy gaitrasrc/srcProtoPR,X§TABER
035700110209       // -?Parametri per Ricerca/Controllo tabelle (TNTBE)?
035800110209     d TIBS02ds      e ds                  inz
035900110209     d   T02mod      e                     inz('R')
036000110209     d   T02cod      e                     inz('3CS')
036100110209       // -?Ricerca/Controllo tabelle (TNTBE)?
036200091106      /copy gaitrasrc/srcProtoPR,TIBS02R
036300091126
036400091126       // -?Reperimento dati tabelle?
036500091104      /copy gaitrasrc/srcProtoPR,TRULTAB
036600091126
036700110209       // -?Parametri per pgm controllo/decodifica cliente?
036800110209      /copy gaitrasrc/srcProtoPI,TIBS69R
036900091126       // -?Controllo/Decodifica cliente?
037000091124      /copy gaitrasrc/srcProtoPR,TIBS69R
037100170825
037200170825       // Contatori Procedure - CRT
037300170825       /copy gaitrasrc/srcProtoPr,UBVCP0R_02
037400171019       // Contatori Procedure - ANN
037500171019       /copy gaitrasrc/srcProtoPr,UBVCP0R_03
037600170825       // Contatori Procedure - CTL
037700170825       /copy gaitrasrc/srcProtoPr,UBVCP0R_04
037800171019       // Contatori Procedure - UPD
037900171019       /copy gaitrasrc/srcProtoPr,UBVCP0R_05
038000171019       // Contatori Procedure - RIP
038100171019       /copy gaitrasrc/srcProtoPr,UBVCP0R_06
038200130520
038300130520       // -?Parametri per pgm ricerca data blocco cliente?
038400130520     d TRULBLCds     e ds                  inz
038500130520       // -?Ricerca data blocco cliente?
038600130520      /copy gaitrasrc/srcProtoPR,TRULBLCR
038700091102
038800100308       // -?Parametri per pgm interrogazione sottoconti?
038900100308      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
039000091126       // -?Interrogazione sottoconti?
039100091124      /copy gaitrasrc/srcProtoPR,XALFA3BR
039200100212
039300100308       // -?Parametri per verifica abilitazione STRATEGI?
039400100308      /copy gaitrasrc/srcProtoPI,TIS7701R
039500100308     d TIVSSds       e ds                  extname(TIVSS00F) qualified
039600100212       // -?Verifica abilitazione STRATEGI?
039700100212      /copy gaitrasrc/srcProtoPR,TIS7701R
039800100212
039900100308       // -?Parametri per reperimento legami in tab. "3C"?
040000100308      /copy gaitrasrc/srcProtoPI,UBLEG3C
040100100212       // -?Reperimento legami in tab. "3C"?
040200100212      /copy gaitrasrc/srcProtoPR,UBLEG3C
040300100326
040400100326       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
040500130604     d wRtnEsito       s             10i 0 inz
040600130605      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
040700091102
040800091126       // -?Gestione scambio dati Cons.Giac.Disp. con clienti?
040900091124      /copy gaitrasrc/srcProtoPR,TRTB40R
041000091102
041100091126       // -?Gestione scambio dati C/A e fattura con clienti?
041200091124      /copy gaitrasrc/srcProtoPR,TRTB44R
041300091126
041400091126       // -?Multimembro clienti VAB?
041500091124      /copy gaitrasrc/srcProtoPR,TRTB83R
041600091102
041700091126       // -?Gestione paticolarità DISK "C"?
041800091124      /copy gaitrasrc/srcProtoPR,TNTB10R
041900091102
042000091126       // -?Gestione serie per DISK "C" con prefisso "CC"?
042100091124      /copy gaitrasrc/srcProtoPR,TNTB13R
042200091102
042300091126       // -?Attribuzione numero serie parziale libero per filiale?
042400110209     d TNTB54ds      e ds                  inz
042500091124      /copy gaitrasrc/srcProtoPR,TNTB54R
042600091102
042700111215       // -?Attribuzione numero serie libero x filiale?
042800111215      /copy gaitrasrc/srcProtoPI,TRTB28R2
042900111215      /copy gaitrasrc/srcProtoPR,TRTB28R2
043000111215
043100111215       // -?Interrogazione tabella "3C" (oltre 9.999 record)?
043200111215     d trtb28r0        pr                  extpgm('TRTB28R0')
043300111215     d   kpjba                             likeds(KPJBA)
043400091214
043500091214       // -?Interrogazione legami tabelle VAS?
043600110209     d TRTB06ds      e ds                  inz
043700120605     d   d063C       e                     inz('S')
043800120605     d   d063K       e                     inz('S')
043900120605     d   d063R       e                     inz('S')
044000120605     d   d064C       e                     inz('S')
044100120605     d   d063Q       e                     inz('S')
044200120605     d   d06VAP      e                     inz('S')
044300120605     d   d06DSC      e                     inz('S')
044400120806     d   d06LAC      e                     inz('S')
044500120806     d   d06OSR      e                     inz('S')
044600091214     d trtb06r         pr                  extpgm('TRTB06R')
044700091214     d   kpjba                             likeds(KPJBA)
044800100211
044900100211       // -?Interrogazione clienti in tab. "3EW"?
045000110209     d tntb79ds      e ds                  qualified   inz
045100100211      /copy gaitrasrc/srcProtoPR,TNTB79R1
045200111011
045300111011       // -?Reperimento ultimo segnacollo utilizzato?
045400111011      /copy gaitrasrc/srcProtoPI,ubLastNSC
045500111011      /copy gaitrasrc/srcProtoPR,ubLastNSC
045600121128
045700121128       // -?"Stacca" figli dall'unificante in manutenzione?
045800121128     d TRTB28ds4     e ds                  inz
045900121128     d   oTB28err    e                     inz(*off)
046000121128     d trtb28r4        pr                  extpgm('TRTB28R4')
046100121128     d   kpjba                             likeds(KPJBA)
046200091105
046300091126       // -?Reperimento NETA sistema AS/400 corrente?
046400111011      /copy gaitrasrc/srcProtoPR,UBRTVNETA
046500130604
046600130605      *// // -?Parametri API QCAPCMD (Process Commands)?
046700130605     d*// Qcmd            s           2048    inz  varying
046800130605      *///copy qSysInc/qRpgleSrc,QCAPCMD
046900130605      *// // -?API QCAPCMD (Process Commands)?
047000130605      *///copy gaitrasrc/srcProtoPR,QCAPCMD
047100130605      *//
047200130605      *// // -?Parametri gestione errori API.?
047300130605      *///copy qsysinc/qrpglesrc,QUSEC
047400091102
047500091102       //--------------------------------------------------------------
047600091102       //?Definizione key-list.                                        ?
047700091102       //--------------------------------------------------------------
047800091102
047900091126       // -?File TABEL00F?
048000091102     d k03tabel00    e ds                  extname(TABEL00F : *key)
048100091102     d                                     prefix(k_)   inz
048200091102
048300091126       // -?File TNTBE01L?
048400091102     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
048500091102     d                                     prefix(k_)   inz
048600091102
048700091102       //--------------------------------------------------------------
048800091102       //?M A I N - L I N E                                            ?
048900091102       //--------------------------------------------------------------
049000091102
049100091102     c     *Entry        plist
049200091102     c                   parm                    KPJBA
049300091102
049400091102      /free
049500091102
049600091126       // -?Operazioni iniziali?
049700091126       exsr sr_RoutInz;
049800091102
049900091126       // -?Gestione videate?
050000091102       DoW  $Fine = *off;
050100091102
050200091102         select;
050300100308           // -?Gestione videata D1 (richiesta codice cliente)?
050400091102           when $Video = 'D1';
050500091102             exsr sr_GesD01;
050600100308           // -?Gestione videata D2 (manutenzione dati cliente)?
050700091102           when $Video = 'D2';
050800091102             exsr sr_GesD02;
050900100308           // -?Gestione videata D3 (manutenzione altri dati cliente)?
051000091102           when $Video = 'D3';
051100091102             exsr sr_GesD03;
051200100308           // -?Gestione videata W1 (ricerca serie segnacolli parziale)?
051300091104           when $Video = 'W1';
051400091104             exsr sr_GesW01;
051500120605           // -?Gestione videata W2 (dati tab. "3CE")?
051600091104           when $Video = 'W2';
051700091104             exsr sr_GesW02;
051800120605           // -?Gestione videata W3 (messaggo di avviso) - NON QUI?
051900120605           //when $Video = 'W3';
052000120605           //  exsr sr_GesW03;
052100101027           // -?Gestione videata W4 (richiesta cod. cliente Strategi)?
052200101027           when $Video = 'W4';
052300101027             exsr sr_GesW04;
052400111011           // -?Gestione videata W5 (ultimo segnacollo reperito)?
052500111011           when $Video = 'W5';
052600111011             exsr sr_GesW05;
052700100308           // -?? ? ? ? ??
052800091102           other;
052900091102             $Fine = *on;
053000091102         endsl;
053100091102
053200091102       enddo;
053300091102
053400091126       // -?Operazioni finali?
053500091102       exsr sr_RoutEnd;
053600091102
053700091102       //--------------------------------------------------------------
053800091102       //?Operazioni iniziali.                                         ?
053900091102       //--------------------------------------------------------------
054000091102       BEGSR  sr_RoutInz;
054100091102
054200091102         *inLR = *on;
054300091105
054400091126         // -?Verifica del sistema AS/400 corrente?
054500091105         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
054600091105           $Fine = *on;
054700091105           leavesr;
054800091105         endif;
054900091105
055000091126         // -?Impostazione elenco librerie in cui gestire le tabelle?
055100091126         //  ?(a seconda del sistema in cui si stà lavorando)?
055200091105         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
055300091105         if  w_iSystem = *zero;
055400091105           $Fine = *on;
055500091105           leavesr;
055600091105         endif;
055700091102
055800091126         // -?Reperimento dati job?
055900091102         exsr  sr_DatiJob;
056000091102
056100091126         // -?Impostazione nome programma a video?
056200091102         V1Tpgm = SDSpgm;
056300091102
056400091126         // -?Impostazione campi "fissi"?
056500091102         k_TBLkut = c_Kut;
056600091102         k_TBLcod = c_Tab;
056700091102
056800091126         // -?Verifica se *pgm richiamato:?
056900100308         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
057000100308         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
057100091102         $Called = (%subst( KPJBU : 1 : %len(V1Ccli) ) <> *blank);
057200100514         $3EWupd = ($Called  and
057300100514                    %subst( KPJBU : %len(V1Ccli) + 1 : 1 ) = 'S');
057400120807         $TrTb06 = ($Called  and
057500120807                    %subst( KPJBU : %len(V1Ccli) + 1 : %len(SDSpgm) )
057600120807                    = 'TRTB06R   ');
057700091102         if  $Called = *on   and
057800091106             %check( c_Digits : %subst( KPJBU : 1 :
057900091104                                       %len(V1Ccli) ) ) = *zero;
058000091104           W0Ccli = %subst( KPJBU : 1 : %len(V1Ccli) );
058100091102           //$InzD01 = *off;
058200091102           //exsr  sr_InzD01;
058300091102           //exsr  sr_CtrD01;
058400091102           //if  Not ErrGenerico;
058500091102           //  $Video = 'D2';
058600091102           //endif;
058700091102         endif;
058800091105
058900091126         // -?Reperimento data odierna?
059000091105         wDate = %int( %subst( %char( %dec( %timestamp() ) )
059100091105                               : 1 : 8 ) );
059200091106
059300091126         // -?Aggancio dati generali della tabella "3CP"?
059400091106         clear  xTNTBE_3CPds;
059500091106         X3CPke1 = *zero;
059600091106         %subst( X3CPke1 : %len(X3CPke1) - 3 + 1 : 3 ) = '3CP';
059700091106         getTabella ( c_Tntbe : *blank : X3CPke1 :
059800091106                      *blank  : *blank : *blank  :
059900091106                      *omit : *omit :
060000091106                      *omit : *omit : *omit : X3CPapl :
060100091106                      *omit : *omit : *omit : *omit :
060200091106                      *omit : X3CPuni );
060300100401
060400100401         // -?Aggancio dati generali della tabella "3EW"?
060500100401         clear  xTNTBE_3EWds;
060600100401         X3EWke1 = *zero;
060700101028         %subst( X3EWke1 : %len(X3EWke1) - 3 + 1 : 3 ) = '3EW';
060800100401         getTabella ( c_Tntbe : *blank : X3EWke1 :
060900100401                      *blank  : *blank : *blank  :
061000100401                      *omit : *omit :
061100100401                      *omit : *omit : *omit : X3EWapl :
061200100401                      *omit : *omit : *omit : *omit :
061300100401                      *omit : X3EWuni );
061400100211
061500091109         cloTabella( c_Tntbe );
061600091102
061700091102       ENDSR;
061800091102
061900091102       //--------------------------------------------------------------
062000091102       //?Reperimento Dati del job (Utente/Operativi).                 ?
062100091102       //--------------------------------------------------------------
062200091102       BEGSR  sr_DatiJob;
062300091102
062400091102         in(e) §AzUte;
062500091102         if NOT %error;
062600091102           in(e) §DatiUte;
062700091102         endif;
062800091102         if %error or RSut = *blank;
062900091102           tibs34r ( tibs34ds );
063000091102           in §AzUte;
063100091102           in §DatiUte;
063200091102         endif;
063300091102
063400091102       ENDSR;
063500091102
063600091102       //--------------------------------------------------------------
063700091102       //?Operazioni finali.                                           ?
063800091102       //--------------------------------------------------------------
063900091102       BEGSR  sr_RoutEnd;
064000091104
064100091104         cloTabella();
064200091102
064300110209         reset  TIBS69ds;
064400091102         tibs69r( tibs69ds :
064500091102                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
064600091125
064700091125         if  $TIS7701;
064800091125           Selettore_Record_TIVSS ( 'SETONLR' );
064900091125         endif;
065000100212
065100100212         ubLeg3C_Close ();
065200091102
065300091102         return;
065400091102
065500091102       ENDSR;
065600091102
065700091102       //--------------------------------------------------------------
065800091102       //?Gestione videata D01.                                        ?
065900091102       //--------------------------------------------------------------
066000091102       BEGSR  sr_GesD01;
066100091102
066200091126         // -?Inizializzazione videata?
066300091102         if  $InzD01 = *on;
066400091102           exsr  sr_InzD01;
066500091102           $InzD01 = *off;
066600091102         endif;
066700091102
066800091126         // -?SE ricevuto codice cliente NON si emette la prima videata?
066900091104         if  $Called   and   W0Ccli > *zero;
067000091102           exsr  sr_CtrD01;
067100091102           if  Not ErrGenerico;
067200091102             $Video  = 'D2';
067300091102             $InzD02 = *on;
067400091102             leavesr;
067500091102           else;
067600091104             clear W0Ccli;
067700091102           endif;
067800091102         endif;
067900091102
068000091126         // -?Emissione Testata?
068100091127         write  TB28T1;
068200091102
068300091126         // -?Emissione videata?
068400091127         exfmt  TB28D1;
068500091102
068600091104         clear  VIDmsg;
068700091102         reset  ErrMessage;
068800091102         reset  ErrGenerico;
068900091102
069000091102         SELECT;
069100091102
069200091126           // -?F3=Fine?
069300091102           WHEN  dsp_aid = c_F03;
069400091102             $Fine = *on;
069500091102
069600091126           // -?F17=Gestione serie per DISK "C" con prefisso "CC"?
069700091126           // -?F18=Gestione paticolarità DISK "C"?
069800091126           // -?F19=Multimembro clienti VAB?
069900091126           // -?F20=Gestione scambio dati principali?
070000091126           // -?F21=Gestione scambio dati Cons.Giac.Disp. con clienti?
070100091126           // -?F22=Gestione scambio dati C/A e fattura con clienti?
070200091102           WHEN  dsp_aid = c_F17   or   dsp_aid = c_F18   or
070300091102                 dsp_aid = c_F19   or   dsp_aid = c_F20   or
070400091102                 dsp_aid = c_F21   or   dsp_aid = c_F22;
070500091102             exsr  sr_FxxD01;
070600091102
070700091126           // -?Invio?
070800091102           OTHER;
070900091102             exsr sr_CtrD01;
071000091102             if  ErrGenerico = *off;
071100091102               $Video    = 'D2';
071200091102               $InzD02   = *on;
071300091102             endif;
071400091102
071500091102         ENDSL;
071600091102
071700091102       ENDSR;
071800091102
071900091102       //--------------------------------------------------------------
072000091102       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
072100091102       //--------------------------------------------------------------
072200091102       BEGSR  sr_InzD01;
072300091102
072400091102         clear  V1Topz;
072500091102         clear  BloAnn;
072600091120         VisBloAnn = *off;
072700091127         clear  TB28D1;
072800091102
072900091126         // -?Se è stato richiamato con un codice cliente numerico, si?
073000091126         //  ?imposta anche tale codice a video?
073100091102         select;
073200091104           when  $Called   and   W0Ccli > *zero;
073300091104             V1Ccli = W0Ccli;
073400091102           when  Not $Called;
073500091102             V1Ccli = '?';
073600091102         endsl;
073700091102
073800091126         // -?Abilitazione tasti funzionali?
073900091102         exsr  sr_AbilFxxD01;
074000091102
074100091102       ENDSR;
074200091102
074300091102       //--------------------------------------------------------------
074400091102       //?Abilitazione tasti funzionali in P01 (per D01).              ?
074500091102       //--------------------------------------------------------------
074600091102       BEGSR  sr_AbilFxxD01;
074700091102
074800091126         // -?Se è stato richiamato: nei primi 7 caratteri della KPJBU?
074900091126         //  ?c'è il codice cliente => NON devono essere abilitati i?
075000091126         //  ?tasti funzionali F17, F18, F19, F20, F21 e F22.?
075100091102
075200091126         // ->?F17 = Gestione serie per disk "C" con prefisso "CC"?
075300091102         F17Attivo = (Not $Called);
075400091102
075500091126         // ->?F18 = Gestione particolarità per disk "C"?
075600091102         F18Attivo = (Not $Called);
075700091102
075800091126         // ->?F19 = Multimembro clienti VAB?
075900091102         F19Attivo = (Not $Called);
076000091102
076100091126         // ->?F20 = Gestione scambio dati principali?
076200111207         //F20Attivo = (Not $Called);
076300111207         F20Attivo = *on;
076400091102
076500091126         // ->?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
076600091102         F21Attivo = (Not $Called);
076700091102
076800091126         // ->?F22 = Gestione scambio dati C/A e fattura con clienti?
076900091102         F22Attivo = (Not $Called);
077000091102
077100091102       ENDSR;
077200091102
077300091102       //--------------------------------------------------------------
077400091102       //?Gestione tasti funzionali F17,F18,F19,F20,F21 e F22 in D01.  ?
077500091102       //--------------------------------------------------------------
077600091102       BEGSR  sr_FxxD01;
077700091102
077800091126         // -?Il chiamato RICHIEDE un codice numerico per verificare?
077900091126         //  ?se è stato richiamato?
078000091102         if  V1Ccli = *blank;
078100091102           KPJBU = *zero;
078200091102         else;
078300091102           KPJBU = V1Ccli;
078400091102         endif;
078500091102
078600091102         select;
078700091102
078800091126           // -?F17=Gestione serie per DISK "C" con prefisso "CC"?
078900091102           when  dsp_aid = c_F17;
079000091102             tntb13r ( kpjba );
079100091102
079200091126           // -?F18=Gestione paticolarità DISK "C"?
079300091102           when  dsp_aid = c_F18;
079400091102             tntb10r ( kpjba );
079500091102
079600091126           // -?F19=Multimembro clienti VAB?
079700091102           when  dsp_aid = c_F19;
079800091102             trtb83r ( kpjba );
079900091102
080000091126           // -?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
080100091102           when  dsp_aid = c_F21;
080200091102             trtb40r ( kpjba );
080300091102
080400091126           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
080500091102           when  dsp_aid = c_F22;
080600091102             trtb44r ( kpjba );
080700091102
080800091102         endsl;
080900091102
081000091102       ENDSR;
081100091102
081200091102       //--------------------------------------------------------------
081300091102       //?Controllo dati immessi nella videata D01.                    ?
081400091102       //--------------------------------------------------------------
081500091102       BEGSR  sr_CtrD01;
081600091102
081700091102         %subst(IndDspF : 50) = *off;
081800091103         reset  $Protect;
081900091102
082000091126         // -?Filiale di ricerca?
082100091102         if  V1Cfil > *zero;
082200091102           chain  (V1Cfil)  AZORG;
082300091102           if  NOT %found(AZORG01L);
082400091104             VIDmsg = $Msg(01);
082500091102             PosCurFIL   = *on;
082600091102             ErrMessage  = *on;
082700091102             ErrGenerico = *on;
082800091102             leavesr;
082900091102           endif;
083000091102         endif;
083100091102
083200091102         SELECT;
083300091102
083400091126           // -?Ricerca codice cliente?
083500091102           WHEN  %scan('?' : V1Ccli) > *zeros;
083600110211             //X§Tkut = c_Kut;
083700110211             //X§Tcod = c_Tab;
083800110211             //if  V1Cfil > *zero;
083900110211             //  X§Tkey = %editc( V1Cfil : 'X' );
084000110211             //else;
084100110211             //  clear  X§Tkey;
084200110211             //endif;
084300110211             //clear  X§Tdes;
084400110211             //x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
084500110211             //V1Ccli = %subst(X§Tkey : 1 : %len(V1Ccli));
084600110211             // -?Superato il numero limite di record caricabili nel?
084700110211             //  ?subfile a video!?
084800110211             kpjbu = %editc( V1Cfil : 'X' );
084900110211             trtb28r0 ( kpjba );
085000110211             V1Ccli = kpjbu;
085100091102             ErrGenerico = *on;
085200091102             leavesr;
085300091102
085400091126           // -?Ricerca alfabetica?
085500091102           WHEN  V1Dcli <> *blank   and
085600091102                (V1Ccli =  *blank   or   V1Ccli = *zero);
085700091102             xParDut = RSut;
085800091102             xParKut = 1;
085900091102             xParRag = V1Dcli;
086000091102             xParKcc = DUTkci;
086100091102             xParSta = *zero;
086200091102             xParFlr = %editc( V1Cfil : 'X');
086300091102             clear  xParDit;
086400091102             xParNum = 1;
086500091102             clear  xParKcm;
086600091102             clear  xParKsm;
086700091102             clear  xParKdm;
086800091102             clear  xParEsci;
086900091102             clear  xParErr;
087000091102             clear  xParIva;
087100091102             clear  xParCdf;
087200091102             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
087300091102                        xParSta : xParFlr : xParDit : xParNum :
087400091102                        xParKcm : xParKsm : xParKdm : xParEsci :
087500091102                        xParErr : xParIva : xParCdf );
087600091102             if  xParSta <> -1   and   xParErr = *blank;
087700091102               V1Ccli = xParKsm;
087800091102               V1Dcli = xParRag;
087900091102             endif;
088000091102             ErrGenerico = *on;
088100091102             leavesr;
088200091102
088300091126           // -?Controllo immissione codice cliente?
088400091102           WHEN  V1Ccli = *blank  or  V1Ccli  = *zero;
088500091104             VIDmsg = $Msg(02);
088600091102             PosCurCLI   = *on;
088700091102             ErrMessage  = *on;
088800091102             ErrGenerico = *on;
088900091102             leavesr;
089000091102
089100091126           // -?Controllo immisione solo caratteri numerici?
089200091106           WHEN  %check( c_Digits : V1Ccli ) > *zero;
089300091104             VIDmsg = $Msg(03);
089400091102             PosCurCLI   = *on;
089500091102             ErrMessage  = *on;
089600091102             ErrGenerico = *on;
089700091102             leavesr;
089800091102
089900091102         ENDSL;
090000091102
090100091126         // -?Controllo / decodifica cliente?
090200091102         clear  TIBS69ds;
090300091104         I69sif = knsif;
090400091102         I69kcc = DUTkci;
090500091102         I69kac = %int(V1Ccli);
090600091102         tibs69r( tibs69ds :
090700091102                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
090800091102         if  O69err = *on;
090900091104           VIDmsg = $Msg(04);
091000091102           PosCurCLI   = *on;
091100091102           ErrMessage  = *on;
091200091102           ErrGenerico = *on;
091300091102           leavesr;
091400091102         endif;
091500091102         V1Dcli = %subst( ACOrag : 1 : %len(V1Dcli) );
091600091103
091700091126         // -?Apertura file TABEL00F del 1° S.I. (sede)?
091800091125         if  w_SisInf <> 1   or   Not %open(TABEL00F);
091900091125           w_SisInf = 1;
092000091125           exsr  sr_OpenFileTab;
092100091125         endif;
092200091102
092300091126         // -?Verifica esistenza del record in TABEL?
092400091102         k_TBLkey = V1Ccli;
092500091102
092600091103         chain  %kds(k03tabel00)  TABEL;
092700091102
092800091102         select;
092900091102
093000100312           // -?Cliente annullato?
093100091103           when  ACOflg <> *blank   and   NOT %found(TABEL00F);
093200091104             VIDmsg = $Msg(05);
093300091103             PosCurCLI   = *on;
093400091103             ErrMessage  = *on;
093500091103             ErrGenerico = *on;
093600091103             leavesr;
093700091102
093800100312           // -?Cliente bloccato?
093900130315           when  ACOabl <> *blanks  and   NOT %found(TABEL00F);
094000091104             VIDmsg = $Msg(05);
094100091103             PosCurCLI   = *on;
094200091103             ErrMessage  = *on;
094300091103             ErrGenerico = *on;
094400091103             leavesr;
094500091102
094600091102         endsl;
094700091102
094800091102       ENDSR;
094900091103
095000091103       //--------------------------------------------------------------
095100091103       //?Apertura dei files tabelle nel sistema informativo impostato.?
095200091103       //--------------------------------------------------------------
095300091103       BEGSR  sr_OpenFileTab;
095400091103
095500091126         // -?Chiusura (eventuale) archivi?
095600091103         if  %open(TABEL00F);
095700091103           close  TABEL00F;
095800091103         endif;
095900091103         if  %open(TNTBE01L);
096000091103           close  TNTBE01L;
096100091103         endif;
096200091109         cloTabella();
096300091103
096400091126         // -?Apertura archivi?
096500100212         ds_Libr  = $Libraries(w_iSystem);
096600091109         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
096700091103         open  TABEL00F;
096800091109         wLibFile2 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
096900091103         open  TNTBE01L;
097000091106
097100091106       ENDSR;
097200091102
097300091102       //--------------------------------------------------------------
097400091102       //?Gestione videata D02.                                        ?
097500091102       //--------------------------------------------------------------
097600091102       BEGSR  sr_GesD02;
097700091102
097800091126         // -?Inizializzazione videata?
097900091102         if  $InzD02 = *on;
098000091102           exsr  sr_InzD02;
098100091102           $InzD02 = *off;
098200091102         endif;
098300091125
098400091126         // -?Impostazione attributi di visualizzazione?
098500091125         exsr  sr_AttrVisD02;
098600091120
098700091126         // -?(Ri)Abilitazione tasti funzionali?
098800091120         exsr  sr_AbilFxxD02;
098900091102
099000091126         // -?Emissione Testata e Piede?
099100091127         write  TB28T1;
099200091127         write  TB28P2;
099300091102
099400091126         // -?Emissione videata?
099500120726         select;
099600120726           when  $Video <> 'D2';
099700120726             clear  ErrMessage;
099800120726             write  TB28D2;
099900120726             leavesr;
100000120726           when  Not $Protect;
100100120726             exfmt  TB28D2;
100200120726           other;
100300120726             write  TB28D2;
100400120726             exfmt  Protect;
100500120726         endsl;
100600091120
100700120202         clear  VIDmsg;
100800091102         reset  ErrMessage;
100900091102         reset  ErrGenerico;
101000091120
101100091102         SELECT;
101200110311
101300110311           // -?F1=Ricezione Serie e Range segnacolli dal padre?
101400120305           //     ?& pure il Supporto Cliente=>BRT?
101500120601           //     ?& pure il Codice Trattamento Merce?
101600110311           WHEN  dsp_aid = c_F01;
101700120601             exsr  sr_F01D02;
101800121128
101900121128           // -?F2="Stacca" figli?
102000121128           WHEN  dsp_aid = c_F02;
102100121128             exsr  sr_F02D02;
102200091102
102300091126           // -?F3=Fine?
102400091102           WHEN  dsp_aid = c_F03;
102500091102             unlock  TABEL00F;
102600091103             unlock  TNTBE01L;
102700091102             $Fine = *on;
102800091214
102900111011           // -?F8=Interrogazione legami tabelle VAS?
103000091214           WHEN  dsp_aid = c_F08;
103100091214             exsr  sr_F08D02;
103200120517
103300120517           // -?F9=Aggiornamento legami tabella "3C"?
103400120517           //  ?(del padre "originale")?
103500120517           WHEN  dsp_aid = c_F09;
103600120517             exsr  sr_ubLeg3C_Orig;
103700091104
103800091126           // -?F10=Ricerca Serie (globale)?
103900091109           WHEN  dsp_aid = c_F10;
104000091109             exsr  sr_F10D02;
104100091104
104200091126           // -?F11=Ricerca Serie Parziale?
104300091109           WHEN  dsp_aid = c_F11;
104400091109             $VideoPrec = $Video;
104500091109             $Video     = 'W1';
104600091109             reset  $InzW01;
104700091102
104800091126           // -?F12=Ritorno?
104900091102           WHEN  dsp_aid = c_F12;
105000091102             unlock  TABEL00F;
105100091103             unlock  TNTBE01L;
105200091125             clear  V1Dcli;
105300091102             $Video = 'D1';
105400100211
105500100211           // -?F13=Interrog. tab. "3EW"?
105600100211           WHEN  dsp_aid = c_F13;
105700100211             exsr  sr_F13D02;
105800111025
105900111025           // -?F14=Visualizzazione ultimo segnacollo?
106000111025           WHEN  dsp_aid = c_F14;
106100111025             $VideoPrec = $Video;
106200111025             $Video     = 'W5';
106300111025             reset  $InzW05;
106400091102
106500091126           // -?F19=Gestione multimembro clienti VAB?
106600091126           // -?F20=Gestione scambio dati principali     con clienti?
106700091126           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
106800091126           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
106900091104           WHEN  dsp_aid = c_F19   or
107000091104                 dsp_aid = c_F20   or
107100091102                 dsp_aid = c_F21   or
107200091102                 dsp_aid = c_F22;
107300091102             exsr  sr_FxxD02;
107400091105
107500091126           // -?RollUp=Videata successiva?
107600100331           WHEN  dsp_aid = c_RollUp   and   V2Cabd <> *blank;
107700091106             $VideoPrec  = $Video;
107800091105             $Video      = 'D3';
107900091104
108000091126           // -?"?" x interrogazione dati tab. "3CP"?
108100091104           WHEN  V2F3CE = '?';
108200091106             $VideoPrec = $Video;
108300091104             $Video     = 'W2';
108400091104             reset  $InzW02;
108500091102
108600091126           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
108700091102           OTHER;
108800100312             // -?Controlli eseguiti NON se F16=Annullamento?
108900091102             if  dsp_aid <> c_F16;
109000091102               exsr sr_CtrD02;
109100091106               if  Not ErrGenerico;
109200091106                 exsr sr_CtrD03;
109300091106                 if  ErrGenerico = *on;
109400091106                   $Video = 'D3';
109500091106                 endif;
109600091106               endif;
109700100312             // -?Controlli eseguiti SOLO se F16=Annullamento?
109800091214             else;
109900100312               exsr sr_CtrlANN;
110000091102             endif;
110100091214             if  ErrGenerico = *on;
110200091214               leavesr;
110300091214             endif;
110400091120             select;
110500100331               // -?Enter/F5/F6 => Videata successiva?
110600100331               //  ?               (se accorpamento bolle)?
110700100331               when  (dsp_aid = c_Enter   or   dsp_aid = c_F05   or
110800100331                      dsp_aid = c_F06)   and   V2Cabd  <> *blank;
110900100331                 $VideoPrec = $Video;
111000100331                 $Video = 'D3';
111100100312               // -?Aggiornamento?
111200091120               when  dsp_aid = c_F05   or
111300091120                     dsp_aid = c_F06   or
111400091120                     dsp_aid = c_F16;
111500091120                 exsr  sr_UpdateAll;
111600091120                 if  ErrGenerico = *on;
111700091120                   leavesr;
111800091120                 endif;
111900100331                 // ·?Uscita dal programma SE richiamato?
112000091120                 if  $Called = *on   and   W0Ccli > *zero;
112100091120                   $Fine = *on;
112200100331                 // ·?Videata iniziale altrimenti?
112300091120                 else;
112400091120                   $Video  = 'D1';
112500091120                   $InzD01 = *on;
112600091120                 endif;
112700091120             endsl;
112800091102
112900091102         ENDSL;
113000091102
113100091126         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
113200091105         if  $Video = 'D1';
113300091102           clear  V1Topz;
113400091102           clear  BloAnn;
113500091120           VisBloAnn = *off;
113600091102         endif;
113700091102
113800091102       ENDSR;
113900091102
114000091102       //--------------------------------------------------------------
114100091105       //?Inizializzazione dati nella videata D02.                     ?
114200091102       //--------------------------------------------------------------
114300091102       BEGSR  sr_InzD02;
114400091102
114500091102         reset  $Protect;
114600091125         reset  $Found_3CP;
114700100312         reset  $Found_TIVSS;
114800120605         reset  $Annull_3EW;
114900100212         reset  $EasyWeb;
115000091125         reset  VisBloAnn;
115100091125         reset  ProtectNRS;
115200091125         reset  ProtectCTM;
115300100407         reset  $Bypass_01;
115400121128         reset  $F2inD2;
115500140718         reset  $CtrlRngSgnCl;
115600110422         reset  SavF1Attivo;
115700120725         reset  wCount_3EW;
115800120725         clear  rqsKSU;
115900100312         clear  TIVSSds;
116000110629         clear  TIVSSds_Orig;
116100120813         clear  ParFIL;
116200120813         clear  ParNRS;
116300120813         reset  SavNRS;
116400120813         reset  SavABD;
116500120813         reset  SavCKS;
116600120813         reset  SavCBA;
116700120813         reset  SavCTM;
116800120813         reset  SavTBLkey_tmp;
116900120813         reset  SavCliTab3CPEW;
117000120813         clear  SaveFLS;
117100120813         clear  SaveNRS;
117200120813         clear  SaveNCD;
117300120813         clear  SaveNCA;
117400120813         clear  SaveCTM;
117500140718         clear  SavW1Cnrs;
117600140718         clear  SavW1Cncd;
117700140718         clear  SavW1Cnca;
117800121220         clear  Save_3CP_nrs;
117900121220         clear  Save_3CP_nsi;
118000121220         clear  Save_3CP_nsf;
118100101022         clear  ubLeg3C_FlgPF;
118200101022         clear  ubLeg3C_Padre;
118300101022         clear  ubLeg3C_SKC;
118400101022         clear  ubLeg3C_SKEW;
118500110629         clear  sch_SKC_Orig;
118600170320         clear  Save3CLNrS  ;
118700170320         clear  Save3CLLNP  ;
118800120130         Protect_3EW  = *off;
118900120130         Reverse_3EW  = *off;
119000120130         $FlsNrsIn3CL = *off;
119100120130         F13Attivo    = *off;
119200091125
119300091127         clear  TB28D2;
119400100312         clear  ds3C;
119500100921         clear  ds3C_Old;
119600121112         clear  d3EW_Old;
119700091102
119800091126         // -?Impostazione testata?
119900091102         clear  V1Topz;
120000091102         clear  BloAnn;
120100091102         select;
120200091102           when  Not %found(TABEL00F);
120300091102             V1Topz = '  INSERIMENTO  ';
120400091102           when  TBLflg = '*';
120500091102             V1Topz = '  RIPRISTINO   ';
120600091102           other;
120700091102             V1Topz = '   MODIFICA    ';
120800091102         endsl;
120900091102         select;
121000100312           // -?Codice annullato?
121100091102           when  ACOflg <> *blank;
121200091102             if  %found(TABEL00F);
121300130528               BloAnn   = 'CODICE ANNULLATO';
121400091102               $Protect = *on;
121500091102             endif;
121600100312           // -?Codice bloccato?
121700130528           when  ACOabl <> *blanks;
121800130528             BloAnn   = 'CODICE  BLOCCATO';
121900130520             clear  TRULBLCds;
122000130520             iBLCkcc = DUTkci;
122100130520             iBLCksc = %int(V1Ccli);
122200130520             trulBLCr ( kpjba : trulBLCds );
122300130520             if  oBLCdav > 00010101;
122400130520               wDate_Dmy = %date( oBLCdav : *iso );
122500130520               wDate6    = %dec( wDate_Dmy );
122600130520               BloAnn    = 'BLOCCATO IL ' +
122700130520                           %editw ( wDate6 : '  /  /  ');
122800130520             endif;
122900091102         endsl;
123000091102
123100091102
123200091126         // -?Impostazione dettaglio?
123300091102         V2Ccli = %int(V1Ccli);
123400091106         V2Crag = %subst( ACOrag : 1 : %len(V2Crag) );
123500091102
123600091126         // -?Solo se Modifica/Annullamento/Ripristino?
123700091102         IF  %found(TABEL00F);
123800091102
123900100921           ds3C_Old = TBLuni;
124000100921
124100091102           ds3C = TBLuni;
124200091102           V2Cnrs = §3Cnrs;
124300100120           if  §3Cfls <> *zero;
124400100120             V2Dfls = %editc( §3Cfls : 'X' );
124500100120           endif;
124600091102           V2Cokd = §3Cokd;
124700110311           //V2Crag = §3Crag;
124800091102           V2Clrm = §3Clrm;
124900091102           V2Calm = §3Calm;
125000091102           V2Cnot = §3Cnot;
125100091102           V2Cabc = §3Cabc;
125200091102           V2Cdkc = §3Cdkc;
125300091102           V2Cavx = §3Cavx;
125400091102           V2Cabd = §3Cabd;
125500091102           V2Caus = §3Caus;
125600091102           V2Carc = §3Carc;
125700091102           V2Cmvr = §3Cmvr;
125800091102           V2Ccba = §3Ccba;
125900091102           V2Cbac = §3Cbac;
126000091222           V2Cbc2 = §3Cbc2;
126100091102           V2Cctm = §3Cctm;
126200091102           V2Cfg1 = §3Cfg1;
126300160317           V2Amb  = §3CCSAMB;
126400091102           V2Cfsp = §3Cfsp;
126500160317           V2TFP  = §3CCSTFP;
126600091102           V2Ccks = §3Ccks;
126700091102           V2Cnwb = §3Cnwb;
126800091102           V2Cnwc = §3Cnwc;
126900091102           V2Crsb = §3Crsb;
127000091102           V2Cpes = §3Cpes;
127100100212
127200120517           // -?Reperimento famiglia completa "originale"?
127300120517           exsr  sr_ubLeg3C_Orig;
127400091102
127500091126           // -?Impostazione / Reperimento serie?
127600100312           // ->?PRIMA impostata con il default...?
127700091102           V2Cncd = 1;
127800091102           V2Cnca = *all'9';
127900100312           // ->?POI eventualmente reperita come parziale?
128000091102           clear  d3CP;
128100100215           clear  d3EW;
128200100215           select;
128300100215             when  V2Ccba = c_EasyWeb;
128400120130               // -?(il reperimento di tali dati verrà fatto dopo,?
128500120130               //   ?nella subr. sr_CtrlCBA)?
128600100215             when  V2Cnrs > *zero;
128700100215               k_TBEcod = '3CP';
128800100215               k_TBEke1 = V1Ccli;
128900100215               setll  %kds( k05tntbe01 : 2 )  TNTBE000;
129000100215               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
129100100215               dow  Not %eof(TNTBE01L);
129200100215                 if  %subst(TBEke2:1:2) = %editc(V2Cnrs:'X') AND
129300100215                   ( (TBLflg = '*'      and   TBEatb = 'A')   or
129400100215                     (TBLflg = *blank   and   TBEatb = *blank) );
129500100215                   d3CP = TBEuni;
129600100215                   V2Cncd = %int( %subst( TBEke2 : 3 : 7 ) );
129700100215                   V2Cnca = §3CPal;
129800100215                   $Found_3CP = *on;
129900121220                   Save_3CP_nrs = V2Cnrs;
130000121220                   Save_3CP_nsi = V2Cncd;
130100121220                   Save_3CP_nsf = V2Cnca;
130200100215                   leave;
130300100215                 endif;
130400100215                 reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
130500100215               enddo;
130600100215           endsl;
130700120726
130800120726           // -?Reperimento chiave completa per tab. "3EW"?
130900120726           //  ?se padre non trovato nel file TIVSS non può essere in?
131000120726           //  ?  tab. "3EW"?
131100120726           clear  rqsSUN;
131200120726           //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
131300120726           if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
131400120726             exsr  sr_TIS7701_x_TIVSS;
131500120726           endif;
131600120726           if  $Found_TIVSS;
131700120726             TIVSSds_Orig = TIVSSds;
131800120726           endif;
131900120726
132000120726           // -?Controllo esistenza padre in tab. "3EW" pur non essendo?
132100120726           //  ?più abilitato ad EasyWEB su STRATEGI?
132200120726           IF  NOT $Found_TIVSS        and
132300120726               //Old_§3Ccks = V2Ccli   and
132400120726               Old_§3Ccba = c_EasyWEB;
132500120726
132600120726             // ·?Reperimento dati "EasyWEB" del padre da tab. "3EW"?
132700120726             //  ?(serve per sapere se l'abilitazione ad EasyWEB del?
132800120726             //  ?padre originale è stata fatta scadere su STRATEGI,?
132900120726             //  ?ma esiste ancora la tab. "3EW")?
133000120726             ds_TNTBE.TBEke1 = '0' + %editc( Old_§3Ccks : 'X' );
133100120726             if  getTabella ( c_Tntbe : '3EW' : ds_TNTBE.TBEke1 :
133200120726                              *omit : *blank : *blank :
133300120726                              *omit : *omit :
133400120726                              *omit : *omit : *omit : *omit :
133500120726                              *omit : *omit : *omit : *omit :
133600120726                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
133700120726                          = *zero      AND
133800120726                 ds_TNTBE.TBEatb = *blank;
133900121112               d3EW_Old  = ds_TNTBE.TBEuni;
134000120814               d3EW_Temp = ds_TNTBE.TBEuni;
134100120814               if  d3EW_Temp.§3EWfaa = *blank;
134200120814                 // ·?Padre già disabilitato ad EasySped-WEB ma ancora?
134300120814                 //  ?in tab. "3EW": occorre annullare la tab. "3EW"?
134400120814                 $Annull_3EW = (Not $Found_TIVSS);
134500120814               endif;
134600120726             endif;
134700120726
134800120726           ENDIF;
134900091102
135000100312           // -?Decodifiche:?
135100091125
135200100312           // ->?Cliente Unificante?
135300120605           //clear  V2Dcks;           ?(già così)?
135400091125           exsr  sr_CtrlCKS;
135500091125           if  ErrGenerico;
135600120813             reset  SavCKS;
135700091125             clear  VIDmsg;
135800091125             clear  PosCurCKS;
135900091125             clear  ErrMessage;
136000091125             clear  ErrGenerico;
136100110818           else;
136200110818             // ->?Abilitazione F1=Numero Serie & Range Segnacolli?
136300110818             exsr  sr_CtrlRngPadre;
136400110818             if  ErrGenerico;
136500120813               reset  SavCKS;
136600110818               clear  ErrGenerico;
136700120813             else;
136800120813               SavCKS = V2Ccks;
136900110818             endif;
137000091125           endif;
137100091106
137200120305           // ->?Supporto Cliente a BRT?
137300091106           exsr  sr_CtrlCBA;
137400121112           if  wCount_3EW = 1;
137500121112             d3EW_Old = d3EW;
137600121112           endif;
137700091106           if  ErrGenerico;
137800120813             reset  SavCBA;
137900110201             reset  $Bypass_01;
138000100312             reset  $EasyWeb;
138100091106             clear  VIDmsg;
138200091106             clear  PosCurCBA;
138300091126             clear  PosCurNRS;
138400120208             clear  PosCurCKS;
138500091106             clear  ErrMessage;
138600091106             clear  ErrGenerico;
138700120813           else;
138800120813             SavCBA = V2Ccba;
138900091106           endif;
139000110311
139100110311           // ->?Numero Serie & Range Segnacolli?
139200110818           //   ?(GIÀ FATTO)?
139300110818           //exsr  sr_CtrlRngPadre;
139400110818           //if  ErrGenerico;
139500110818           //  clear  ErrGenerico;
139600120130           //endif;
139700091125
139800100312           // ->?Codice Trattamento Merce?
139900091125           exsr  sr_CtrlCTM;
140000091125           if  ErrGenerico;
140100120813             reset  SavCTM;
140200091125             clear  VIDmsg;
140300091125             clear  PosCurCTM;
140400091125             clear  ErrMessage;
140500091125             clear  ErrGenerico;
140600120813           else;
140700120813             SavCTM = V2Cctm;
140800091125           endif;
140900120208
141000120208           // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
141100120208           //   ?se NON reperito il range segnacolli per EasyWEB?
141200120726           if  V2Ccba   =  c_EasyWEB    and
141300120726              (rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
141400120726                                   or   TIVSSds.VSSksu = *zero);
141500120725             VIDmsg = $Msg(14);
141600120725             if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
141700120803                 V2Cncd = 1          and  V2Cnca = *all'9';
141800120725               // -?Abilitazione F13=Interrog. tab. "3EW"?
141900120725               F13Attivo = *on;
142000120725               //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
142100120725               VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
142200120725             endif;
142300120725             PosCurCKS   = *on;
142400120725             ErrMessage  = *on;
142500120725             ErrGenerico = *on;
142600120726             //leavesr;
142700120208           endif;
142800120725
142900120725           // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
143000120726           //   ?se cliente presente più di una sola volta nella "3EW"?
143100120726           //   ?(errore "alternativo" al precedente: meglio non uscire?
143200120726           //   ?dalla subroutine per eseguire comunque la pulizia dei?
143300120726           //   ?campi sottostante)?
143400120726           //if  wCount_3EW > 1;
143500120726           //  // -?Abilitazione F13=Interrog. tab. "3EW"?
143600120726           //  F13Attivo   = *on;
143700120726           //  VIDmsg = $Msg(40);
143800120726           //  //PosCurNRS   = *on;
143900120726           //  PosCurCKS   = *on;
144000120726           //  ErrMessage  = *on;
144100120726           //  ErrGenerico = *on;
144200120726           //  leavesr;
144300120726           //endif;
144400120726           if  wCount_3EW > 1;
144500150319             //$VideoPrec  = $Video;       ?(già fatto)?
144600150319             $Video = 'W4';
144700120726             reset  $InzW04;
144800120726             ErrGenerico = *on;
144900120726             //leavesr;
145000120726           endif;
145100091102
145200091102         ELSE;
145300091102
145400100211           V2Cfg1 = 'N';
145500100211           V2Cfsp = 'N';
145600100211           V2Cnwb = 'S';
145700100211           V2Cnwc = 'L';
145800100211           V2Cncd = 1;
145900100211           V2Cnca = *all'9';
146000091102
146100091102         ENDIF;
146200091102
146300091126         // -?Pulizia campi per controllo forzatura errore?
146400100623         reset  SavNRS;
146500120131         //reset  SavNCD;
146600120131         //reset  SavNCA;
146700100623         reset  SavABD;
146800120813         //reset  SavCKS;
146900120813         //reset  SavCBA;
147000120813         //reset  SavCTM;
147100100623         reset  SavTBLkey_tmp;
147200110311         reset  SavCliTab3CPEW;
147300091102
147400091102       ENDSR;
147500101026
147600101026       //--------------------------------------------------------------
147700101026       //?Reperimento dati di TIVSS tramite TIS7701R.                  ?
147800101026       //--------------------------------------------------------------
147900101026       BEGSR  sr_TIS7701_x_TIVSS;
148000101026
148100101026         $TIS7701 = *on;
148200101026
148300120725         rqsKSU = '0' + %editc( V2Ccks : 'X' );
148400101026         clear  rpyEsito;
148500101026         Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
148600101028                                  rqsSUN : *zero : TIVSSds :
148700101026                                  rpyEsito );
148800101026
148900101026         $Found_TIVSS = (rpyEsito = *zero  and  TIVSSds.VSSksu > *zero);
149000101026
149100101026       ENDSR;
149200091125
149300091125       //--------------------------------------------------------------
149400091125       //?Impostazione attributi di visualizzazione in D02.            ?
149500091125       //--------------------------------------------------------------
149600091125       BEGSR  sr_AttrVisD02;
149700091125
149800091126         // ->?Visualizzazione "CLIENTE BLOCCATO" in testata?
149900091125         VisBloAnn = (BloAnn <> *blank);
150000091125
150100091126         // ->?Permessa interrogazione tab. "3CE"?
150200110930         //VisInfoES = (§3Ccba = 'ESYSP'   or   §3Ccba = 'ESVAL');
150300110930         VisInfoES = (§3Ccba = 'ESYSP'   or   §3Ccba = 'ESVAL'   or
150400110930                      getTabella ( c_Tntbe : '3CE' : V1Ccli :
150500110930                                   *blank: *omit : *omit :
150600110930                                   *omit : *omit :
150700110930                                   *omit : *omit : *omit : *omit :
150800110930                                   *omit : *omit : *omit : *omit :
150900110930                                   ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
151000110930                                 = *zero      AND
151100110930                      ds_TNTBE.TBEatb = *blank);
151200091125
151300091126         // ->?Protezione "Numero Serie"?
151400091127         ProtectNRS = (V2Ccli <> V2Ccks   and   V2Ccba = c_EasyWEB
151500091127                                          and   V2Ccba = p_§3Ccba);
151600091125
151700091126         // ->?Protezione "Codice Trattamento Merce"?
151800120305         //   ?se Supporto Cliente a BRT = 'ESWEB' ANCHE per il padre?
151900091126         ProtectCTM = ((V2Ccba = c_EasyWEB  and  V2Ccks = V2Ccli)   OR
152000091126                       (V2Ccba = c_EasyWEB  and  V2Cctm = p_§3Cctm  and
152100091126                      p_§3Ccba = c_EasyWEB));
152200120130
152300120130         // ->?Numero Serie & Range Segnacolli in tab. "3CL"?
152400120130         ds_TNTBE.TBEke1 = V2Dfls;
152500120130         ds_TNTBE.TBEke2 = %editc( V2Cnrs : 'X' );
152600120130         $FlsNrsIn3CL = ( V2Cncd = 1  and  V2Cnca = *all'9'  and
152700120130                        getTabella ( c_Tntbe : '3CL' : ds_TNTBE.TBEke1 :
152800120130                                     ds_TNTBE.TBEke2 : *omit : *omit :
152900120130                                     *omit : *omit :
153000120130                                     *omit : *omit : *omit : *omit :
153100120130                                     *omit : *omit : *omit : *omit :
153200120130                                     ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
153300120130                                   = *zero      AND
153400120130                        ds_TNTBE.TBEatb = *blank );
153500150127
153600150127         // ->?Filiale gest. se supporto clienta a BRT = ESWEB presa da tab.3EW
153700150127         VisLNP3EW = (§3Ccba = 'ESWEB');
153800091125
153900091125       ENDSR;
154000091102
154100091102       //--------------------------------------------------------------
154200091102       //?Abilitazione tasti funzionali in P02 (per D02 e D03).        ?
154300091102       //--------------------------------------------------------------
154400091102       BEGSR  sr_AbilFxxD02;
154500110311
154600110311         // ->?F1 = Reperimento Serie e Range segnacolli dal padre?
154700110311         //F1Attivo = ...vedi subr. sr_CtrlRngPadre
154800110422         // ·?Comunque salvo lo stato attuale...?
154900110422         SavF1Attivo = F1Attivo;
155000121128
155100121128         // ->?F2 = "Stacca" figli?
155200130103         //F2Attivo = (%found(TABEL00F)   and   TBLflg = *blank       and
155300130103         //            $Video = 'D2'      and   V2Ccli = §3Ccks       and
155400130103         //            sch_skC(2) > *zero);
155500130103         //          //$Video = 'D2'      and   V2Ccli = Old_§3Ccks   and
155600130103         //          //sch_skC_Orig(2) > *zero);
155700130103         F2Attivo = *off;
155800121128
155900121128         // ->?F3 = Fine?
156000121128         F3Attivo = (Not $F2inD2);
156100091102
156200091126         // ->?F5 = Ripristino?
156300091102         F5Attivo = (%found(TABEL00F)   and   TBLflg <> *blank);
156400091102
156500091126         // ->?F6 = Conferma?
156600091120         F6Attivo = ( ( ( %found(TABEL00F)  and  TBLflg = *blank  and
156700091120                          Not $Protect )    OR   Not %found(TABEL00F) )
156800091120                                            AND
156900091120                      ( ( $Video = 'D2'     and  V2Cabd = *blank )
157000091120                                            OR   $Video = 'D3'));
157100091214
157200091214         // ->?F8 = Interrogazazione legami tabelle VAS?
157300120807         F8Attivo =  (Not $TrTb06   and
157400120807                     ($Video = 'D2'  or  $Video = 'W4'));
157500120517
157600120517         // ->?F9 = Aggiornamento legami tabella "3C"?
157700120803         F9Attivo =  (($Video = 'D2'  or  $Video = 'W4')
157800120803                      and  NOT $Protect);
157900091102
158000091126         // ->?F10 = Ricerca serie?
158100120113         //F10Attivo = (Not F5Attivo   and   $Video <> 'D3'
158200120113         //                            and   V2Ccba <> c_EasyWEB);
158300120113         //   ?(anche in fase di ripristino)?
158400120202         //F10Attivo = ($Video <> 'D3'  and  V2Ccba <> c_EasyWEB
158500120202         //                             and  Not $FlsNrsIn3CL);
158600120202         //   ?(anche se Filiale/Serie Segnacolli in tab. "3CL")?
158700120803         F10Attivo = (($Video <> 'D3'  and  V2Ccba <> c_EasyWEB)
158800120803                      and  NOT $Protect);
158900091102
159000091126         // ->?F11 = Ricerca serie parziale?
159100091120         F11Attivo = (F10Attivo);
159200091102
159300091126         // ->?F12 = Ritorno?
159400121128         F12Attivo = ((Not $Called   or   $Video = 'D3')  and
159500121128                       Not $F2inD2);
159600120517
159700120517         // ->?F13 = Interrogazione tab. "3EW"?
159800120517         //   ?(NON qui: abilitazione condizionata dal fallito reperim.?
159900120517         //   ? del range segnacolli in tab. "3EW" - in D2)?
160000120517         ////F13Attivo = *off;
160100120517
160200120518         // ->?F14 = Visualizzazione ultimo segnacollo?
160300120726         F14Attivo = ($Video = 'D2'  or  $Video = 'W4');
160400091102
160500091126         // ->?F16 = Annullamento?
160600100216         //F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank  and
160700100216         //             %subst( KNMUS : 1 : 3 ) = c_EDP           and
160800100216         //            (V2Ccli <> §3Ccks   or
160900100216         //            (V2Ccli =  §3Ccks   and   sch_SKC(2) = *zero)));
161000100216         //   ?(si abilita anche per cliente padre con figli, per?
161100100216         //    ?visualizzarne poi i figli nel messaggio di errore)?
161200100216         F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank  and
161300100216                      %subst( KNMUS : 1 : 3 ) = c_EDP);
161400091120
161500091126         // ->?F19 = VAB multimembro?
161600091120         F19Attivo = ($Video <> 'D3');
161700091102
161800120517         // -?Abilitazione F20, F21 e F21 SE NON richiamato:?
161900091126         // ->?F20 = Multimembro clienti VAB?
162000111207         //F20Attivo = (Not $Called   and   $Video <> 'D3');
162100111207         F20Attivo = ($Video <> 'D3');
162200091126         // ->?F21 = Gestione dati Consegna/Giacenza per clienti?
162300091120         F21Attivo = (Not $Called   and   $Video <> 'D3');
162400091126         // ->?F22 = Gestione dati C/A e Fattura per clienti?
162500091120         F22Attivo = (Not $Called   and   $Video <> 'D3');
162600091120
162700091126         // ->?RollUp (se impostato l'"accorpamento bolle")?
162800091120         RlUpAttivo = (V2Cabd <> *blank);
162900091102
163000091102       ENDSR;
163100120601
163200120601       //--------------------------------------------------------------
163300120601       //?Gestione tasto funzionale F1=Impostazione dati dal padre:    ?
163400120601       //?· Serie segnacolli                                           ?
163500120601       //?· Range segnacolli                                           ?
163600120601       //?· Supporto Cliente => BRT                                    ?
163700120601       //?· Codice Trattamento Merce                                   ?
163800120601       //--------------------------------------------------------------
163900120601       BEGSR  sr_F01D02;
164000120601
164100120601         // -?Supporto Cliente => BRT?
164200120601         V2Ccba = p_§3Ccba;
164300120601         // -?Numero Serie Segnacolli?
164400120601         V2Cnrs = p_§3Cnrs;
164500120927         // -?Filiale/Range Segnacolli?
164600120601         if  p_§3Ccba = c_EasyWEB;
164700120927           V2Dfls = %editc( d3EW_Padre.§3EWfls : 'X' );
164800120601           V2Cncd = d3EW_Padre.§3EWnsi;
164900120601           V2Cnca = d3EW_Padre.§3EWnsf;
165000150127           V23EWLNP = %editc( d3EW_Padre.§3EWLNP : 'X' );
165100120601         else;
165200121218           V2Dfls = %subst( %editc( V2Ccli : 'X' ) : 1 : 3 );
165300120601           V2Cncd = wNSI_Padre;
165400120601           V2Cnca = d3CP_Padre.§3CPal;
165500150127           V23EWLNP = *blank;
165600120601         endif;
165700120601         if  V2Cncd = *zero  and  V2Cnca = *zero;
165800120601           V2Cncd = 1;
165900120601           V2Cnca = *all'9';
166000120601         endif;
166100120601         // -?Codice Trattamento Merce?
166200120927         if  p_§3Ccba = c_EasyWEB;
166300120927           V2Cctm = d3EW_Padre.§3EWctm;
166400120927         else;
166500120927           V2Cctm = p_§3Cctm;
166600120927         endif;
166700140123
166800140123         // -?"Forzature" richiestemi da Mirko C.?
166900140123         If  V2Ccba = c_EasyWEB;
167000140123           // -?Supporto BRT a Cliente?
167100140123           if  V2Ccli = V2Ccks;
167200140123             V2Cbac = 'EMAIL';
167300140123           endif;
167400140123           // -?Abilitazione Serie in Bollettazione?
167500140123           V2Cfg1 = 'S';
167600140123         EndIf;
167700120601
167800120601         // -?DISabilitazione F1=Reperimento Serie e Range segnacolli?
167900120601         //                     ?dal padre?
168000120601         F1Attivo = *off;
168100120601
168200120601       ENDSR;
168300121128
168400121128       //--------------------------------------------------------------
168500121128       //?Gestione tasto funzionale F2="Stacca" figli                  ?
168600121128       //--------------------------------------------------------------
168700121128       BEGSR  sr_F02D02;
168800121128
168900121128         reset  TRTB28ds4;
169000121205         //iTB28ksuA = V2Ccks;
169100121205         iTB28ksuA = §3Ccks;
169200121128         kpjbu = TRTB28ds4;
169300121204
169400121204         unlock  TABEL00F;
169500121128
169600121128         TRTB28R4 ( kpjba );
169700121206
169800121206         chain  %kds(k03tabel00)  TABEL;
169900121206         //ds3C = TBLuni;           ?(già così)?
170000121128
170100121128         TRTB28ds4 = kpjbu;
170200121128
170300121128         Select;
170400121128           // -?Restituito errore?
170500121128           When  oTB28err =  *on;
170600121128             VIDmsg = oTB28msg;
170700121128             PosCurCKS   = *on;
170800121128             ErrMessage  = *on;
170900121128             ErrGenerico = *on;
171000121128             leavesr;
171100121128           // -?Premuto F6: aggiornamento legami tabella "3C" e?
171200121128           //              ?riesecuzione controllo range segnacolli?
171300121128           When  oTB28fxx =  '06';
171400121128             $F2inD2 = *on;
171500121204             V2Ccks  = oTB28ksuN;
171600121204             clear  V2Cbac;
171700121128             exsr  sr_ubLeg3C_Orig;
171800121128             clear  ubLeg3C_Padre;
171900121128             exsr  sr_Ctrl_NRS_SCx;
172000121128             if  ErrGenerico = *on;
172100121128               leavesr;
172200121128             endif;
172300121204             exsr  sr_CtrD02;
172400121204             if  ErrGenerico = *on;
172500121204               leavesr;
172600121204             endif;
172700121205             F2Attivo = *off;
172800121128         EndSl;
172900121128
173000121128       ENDSR;
173100091214
173200091214       //--------------------------------------------------------------
173300091214       //?Gestione tasto funzionale F8=Interrog. legami tab. VAS       ?
173400091214       //--------------------------------------------------------------
173500091214       BEGSR  sr_F08D02;
173600091214
173700100211         clear  trtb06ds;
173800100211         D06ksc = %int(V1Ccli);
173900100211
174000100211         kpjbu = trtb06ds;
174100091214
174200091214         trtb06r ( kpjba );
174300091214
174400091214       ENDSR;
174500091104
174600091104       //--------------------------------------------------------------
174700091104       //?Gestione tasto funzionale F10=Ricerca Serie (in D02).        ?
174800091104       //--------------------------------------------------------------
174900091104       BEGSR  sr_F10D02;
175000091104
175100091126         // -?NON modificabile manualmente premendo F10?
175200091104         if  V2Cnrs <> *zero   and  V2Cnrs <> SavNRS;
175300091104           SavNRS = V2Cnrs;
175400091125           VIDmsg = $Msg(26);
175500091104           PosCurNRS   = *on;
175600091104           ErrMessage  = *on;
175700091104           ErrGenerico = *on;
175800091104           leavesr;
175900091104         endif;
176000111215
176100111215         // -?Reperimento famiglia completa?
176200111215         //  ?(se premuto F10 PRIMA di Invio, la famiglia?
176300111215         //  ? NON è ancora stata reperita)?
176400120104         exsr  sr_ubLeg3C;
176500091104
176600091126         // -?Ricerca?
176700091104         clear  TRTB28ds2;
176800100215         if  V2Dfls > *zero;
176900100215           ParFIL = %int(V2Dfls);
177000100215         else;
177100100215           ParFIL = V2Ccli / 10000;
177200100215         endif;
177300091104         ParELB = 'R';
177400111215         trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
177500120113         select;
177600120113           // ->?Serie NON ammessa (99)?
177700120113           when  ParERR = '9';
177800120113             VIDmsg = $Msg(43);
177900120113             PosCurNRS   = *on;
178000120113             ErrMessage  = *on;
178100120113             ErrGenerico = *on;
178200120113             leavesr;
178300120113           // ->?Non reperibile una nuova serie?
178400120113           when  ParERR <> *blank;
178500120113             VIDmsg = $Msg(27);
178600120113             PosCurNRS   = *on;
178700120113             ErrMessage  = *on;
178800120113             ErrGenerico = *on;
178900120113             leavesr;
179000120113           // ->?Reperita serie completa?
179100120113           other;
179200120113             V2Cnrs = ParNRS;
179300120113             V2Cncd = 1;
179400120113             V2Cnca = *hival;
179500120113         endsl;
179600091109
179700091126         // -?Ritorno alla videata D02?
179800091109         $VideoPrec = $Video;
179900091109         $Video     = 'D2';
180000091104
180100091104       ENDSR;
180200100211
180300100211       //--------------------------------------------------------------
180400100211       //?Gestione tasto funzionale F13=Interrog. tab. "3EW"           ?
180500100211       //--------------------------------------------------------------
180600100211       BEGSR  sr_F13D02;
180700100211
180800100212         clear  tntb79ds;
180900100212         TNTB79ds.i3EWksu = '0' + %editc( V2Ccks : 'X' );
181000100211
181100100212         tntb79r1 ( kpjba : TNTB79ds );
181200101029
181300101029         if  TNTB79ds.o3EWfxx = *blank   and
181400101029             TNTB79ds.o3EWksu = '0' + %editc( V2Ccks : 'X' );
181500120215           d3EW   = TNTB79ds.o3EWuni;
181600120215           V2Dfls = %editc( d3EW.§3EWfls : 'X' );
181700150127           V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
181800120215           V2Cnrs = d3EW.§3EWnrs;
181900120215           V2Cncd = d3EW.§3EWnsi;
182000120215           V2Cnca = d3EW.§3EWnsf;
182100120215           V2Cctm = d3EW.§3EWctm;
182200120816           if  d3EW.§3EWfaa = *blank;
182300120816             wCount_3EW = 1;
182400120816             rqsSUN = TNTB79ds.o3EWsun;
182500120816             exsr  sr_TIS7701_x_TIVSS;
182600120816             $Bypass_01 = *on;
182700120816           //else;
182800120816           //  VIDmsg = $Msg(44);
182900120816           //  PosCurNRS   = *on;
183000120816           //  ErrMessage  = *on;
183100120816           //  ErrGenerico = *on;
183200120816           //  leavesr;
183300120816           endif;
183400101029         endif;
183500100211
183600100211       ENDSR;
183700091102
183800091102       //--------------------------------------------------------------
183900091104       //?Gestione tasti funzionali F19, F20, F21 e F22 in D02.        ?
184000091102       //--------------------------------------------------------------
184100091102       BEGSR  sr_FxxD02;
184200091102
184300091126         // -?Il chiamato RICHIEDE un codice numerico per verificare?
184400091126         //  ?se è stato richiamato?
184500091102         if  V1Ccli = *blank;
184600091102           KPJBU = *zero;
184700091102         else;
184800091102           KPJBU = V1Ccli;
184900091102         endif;
185000091102
185100091102         select;
185200091104
185300091126           // -?F17 = Gestione serie per Disk "C" con prefisso "CC"?
185400091104           when  dsp_aid = c_F17;
185500091104             tntb13r ( kpjba );
185600091104
185700091126           // -?F18 = Gestione particolarità Disk "C"?
185800091104           when  dsp_aid = c_F18;
185900091104             tntb10r ( kpjba );
186000091104
186100091126           // -?F19 = Gestione multimembro clienti VAB?
186200091104           when  dsp_aid = c_F19;
186300091104             trtb83r ( kpjba );
186400091102
186500091126           // -?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
186600091102           when  dsp_aid = c_F21;
186700091102             trtb40r ( kpjba );
186800091102
186900091126           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
187000091102           when  dsp_aid = c_F22;
187100091102             trtb44r ( kpjba );
187200091102
187300091102         endsl;
187400091102
187500091102       ENDSR;
187600091214
187700091214       //--------------------------------------------------------------
187800091214       //?Controllo annullabilità del record nella videata D02.        ?
187900091214       //--------------------------------------------------------------
188000100312       BEGSR  sr_CtrlANN;
188100091214
188200091214         %subst(IndDspF : 50) = *off;
188300091214
188400091214         // -?Se cliente Unificante: NON deve avere figli?
188500100322         if  %found(TNTBE01L)   and   V2Ccli = §3Ccks   and
188600120326             sch_SKC_Orig(2) > *zero;
188700120517           // -?Su richiesta di Mirko: si riesegue il caricamento dei?
188800120517           //  ?legami (potrebbero essere stati annullati tutti i figli?
188900120517           //  ?nel frattempo - da un'altra sessione video)?
189000120517           exsr  sr_ubLeg3C_Orig;
189100120517           if  sch_SKC_Orig(2) > *zero;
189200120517             VIDmsg = %trimr($Msg(35)) + ' ' +
189300120517                      %editc( sch_SKC_Orig(2) : 'X' );
189400120517             if  sch_SKC_Orig(3) > *zero;
189500120517               VIDmsg = %trimr(VIDmsg) + ' ed altri';
189600120517             endif;
189700120517             PosCurCKS   = *on;
189800120517             ErrMessage  = *on;
189900120517             ErrGenerico = *on;
190000120517             leavesr;
190100120517           endif;
190200091214         endif;
190300120214
190400120214         // -?NON consentito l'annullamento (da qui) SE cliente?
190500120329         //  ?PADRE abilitato ad EasySepd-WEB su Strategi?
190600120215         //  ?(e NON richiamato proprio per quello)?
190700120329         //  ?Un cliente figlio è annullabile anche se abilitato?
190800120329         //    ?ad EasyWEB (quindi con padre abilitato ad EasyWEB)!?
190900120214         If  Not  $3EWupd         and
191000120214             §3Ccba = c_EasyWEB   and
191100120329             §3Ccks = V2Ccli      and
191200120214             %editc(§3Ccks : 'X') = %subst(TIVSSds_Orig.VSSksu : 2);
191300120214           VIDmsg = $Msg(32);
191400120214           ErrMessage  = *on;
191500120214           ErrGenerico = *on;
191600120214           leavesr;
191700120214         EndIf;
191800101027
191900101027         // -?Se cliente Unificante presente con più codici Strategi,?
192000101027         //  ?occorre selezionare quello corretto?
192100120214         //If  wCount_3EW > 1;
192200120214         //  $VideoPrec = $Video;
192300120214         //  $Video     = 'W4';
192400120214         //  reset  $InzW04;
192500120214         //EndIf;
192600091214
192700091214       ENDSR;
192800091102
192900091102       //--------------------------------------------------------------
193000091102       //?Controllo dati immessi nella videata D02.                    ?
193100091102       //--------------------------------------------------------------
193200091102       BEGSR  sr_CtrD02;
193300091104
193400091104         %subst(IndDspF : 50) = *off;
193500100212
193600100212         // -?Spegnimento indicatori che:?
193700120305         //  ?1) memorizza l'impostaz. "Supporto Cliente a BRT"?
193800100212         //     ?EasyWeb [al cambiamento] (per fare certi controlli?
193900100212         //     ?solo la 1ª volta)?
194000100212         //  ?2) protegge i campi reperiti dal padre (FLS, NRS, Range?
194100100212         //     ?Segnacolli e CTM)?
194200100212         //  ?3) evidenziano i campi reperiti dal padre (FLS, NRS, Range?
194300100212         //     ?Segnacolli e CTM)?
194400100216         if  V2Ccks <> I69kac   and   $EasyWeb;
194500100312           reset  $EasyWeb;
194600100216         endif;
194700100212         if  V2Ccba <> c_EasyWeb;
194800100312           reset  $EasyWeb;
194900100212           Protect_3EW = *off;
195000100212         endif;
195100120130         Reverse_3EW  = *off;
195200120130         $FlsNrsIn3CL = *off;
195300091125
195400091126         // -?Ricerche?
195500091125         exsr  sr_Search;
195600091124
195700091126         // -?Cliente Unificante?
195800120510         //  ?N.B.: è meglio resettare il campo di controllo (SAVCKS)?
195900120510         //  ?per rieseguire i controlli anche nel caso in cui la 1ª?
196000120510         //  ?modifica abbia avuto esito negativo (errore).?
196100110121         if  V2Ccks <> SavCKS  or
196200110121             V2Ccks <> I69kac;
196300120510           reset  SavCKS;
196400120104           clear  ParFIL;
196500120104           clear  ParNRS;
196600100312           exsr  sr_CtrlCKS;
196700100312           if  ErrGenerico;
196800100312             leavesr;
196900100312           endif;
197000100312         endif;
197100160317
197200120305         // -?Supporto Cliente a BRT?
197300120510         //  ?N.B.: è meglio resettare il campo di controllo (SAVCBA)?
197400120510         //  ?per rieseguire i controlli anche nel caso in cui la 1ª?
197500120510         //  ?modifica abbia avuto esito negativo (errore).?
197600100312         if  V2Ccks <> SavCKS   or   V2Ccba <> SavCBA;
197700120510           reset  SavCBA;
197800100312           exsr  sr_CtrlCBA;
197900100312           if  ErrGenerico;
198000100312             leavesr;
198100100312           endif;
198200100312         endif;
198300120125
198400120125         // -?Memorizzazione codici già controllati con esito positivo?
198500120125         //  ?per evitare di ripetere tali controlli...?
198600120125         SavCBA = V2Ccba;
198700120125         SavCKS = V2Ccks;
198800091103
198900091126         // -?Codice Trattamento Merce?
199000100312         if  V2Cctm <> SavCTM;
199100100312           exsr  sr_CtrlCTM;
199200100312           if  ErrGenerico;
199300100312             leavesr;
199400100312           endif;
199500120125           SavCTM = V2Cctm;
199600100312         endif;
199700110311
199800110311         // -?Abilitazione F1 per?
199900110311         //  ?Numero Serie & Range Segnacolli dal padre?
200000110311         exsr  sr_CtrlRngPadre;
200100120515         //if  ErrGenerico;         ?(meglio proseguire...)?
200200120515         //  leavesr;               ?(meglio proseguire...)?
200300120515         //endif;                   ?(meglio proseguire...)?
200400091103
200500091126         // -?Numero Serie?
200600091126         // ->?Obbligatorio (se CTM da cliente Flag6)?
200700100312         if  V2Cnrs = *zero   and   §1Bfg6 = 'C';
200800091125           VIDmsg = $Msg(17);
200900091103           PosCurNRS   = *on;
201000091103           ErrMessage  = *on;
201100091103           ErrGenerico = *on;
201200091103           leavesr;
201300091103         endif;
201400091103
201500120202         Select;
201600120202
201700120202           // ->?Nessuna serie immessa?
201800120202           When  V2Cnrs = *zero;
201900120202             V2Cncd = 1;
202000120202             V2Cnca = *hival;
202100100329
202200120202           // ->?Controlli per serie completa o parziale?
202300120202           //   ?(controlla prima serie parziale, poi completa)?
202400120202           Other;
202500120202             W1Cnrs = V2Cnrs;
202600120202             W1Cncd = V2Cncd;
202700120202             W1Cnca = V2Cnca;
202800120202             exsr  sr_Ctrl_NRS_SCx;
202900120202             if  ErrGenerico = *on;
203000120202               leavesr;
203100120202             endif;
203200120202
203300120202         EndSl;
203400100111
203500100111         // -?Filiale Segnacollo?
203600100111         select;
203700100111           when  V2Cnrs = *zero;
203800100111             clear  V2Dfls;
203900100111           //when  §3Cfls <> *zero;
204000100120           //  V2Dfls = %editc( §3Cfls : 'X' );
204100100326           when  V2Ccba = c_EasyWEB   and   d3EW.§3EWfls > *zero;
204200100326             V2Dfls = %editc( d3EW.§3EWfls : 'X' );
204300150127             if  d3EW.§3EWLNP > *zero;
204400150127               V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
204500150127             endif;
204600100111           other;
204700100120             V2Dfls = %subst( V1Ccli : 1 : %len(V2Dfls) );
204800150127               V23EWLNP = *blank;
204900100111         endsl;
205000091104
205100120305         // -?Supporto BRT a Cliente?
205200091201         select;
205300100312           // -?Obbligatorio per cliente unificante con serie?
205400091201           when  V2Cbac =  *blank   and
205500091201                 V2Ccks =  V2Ccli   and
205600091201                 V2Cnrs <> *zero;
205700091201             VIDmsg = $Msg(34);
205800091201             PosCurBAC   = *on;
205900091201             ErrMessage  = *on;
206000091201             ErrGenerico = *on;
206100091201             leavesr;
206200100312           // -?Indicabile SOLO sul cliente unificante?
206300091201           when  V2Cbac <> *blank   and
206400091201                 V2Ccks <> V2Ccli;
206500091201             VIDmsg = $Msg(22);
206600091201             PosCurBAC   = *on;
206700091201             ErrMessage  = *on;
206800091201             ErrGenerico = *on;
206900091201             leavesr;
207000091201         endsl;
207100091124
207200091126         // -?"Accorpamento bolle" diverso dal quello del padre?
207300091127         if  V2Ccks <> V2Ccli     and
207400100312             V2Cabd <> p_§3Cabd   and   V2Cabd <> SavABD;
207500091125           SavABD = V2Cabd;
207600091125           VIDmsg = %trimr($Msg(23)) + ' "' + p_§3Cabd +
207700091125                    '" » "Invio" per forzare';
207800091125           PosCurABD   = *on;
207900091124           ErrMessage  = *on;
208000091124           ErrGenerico = *on;
208100091124           leavesr;
208200091124         endif;
208300120605
208400120605         // -?Emissione messaggio di avvertimento della tab. "3EW"?
208500120605         //  ?ancora attiva nonostante manchi l'abilitazione ad EasyWeb?
208600120605         If  $Annull_3EW   and
208700120605               // -?Non un solo codice in TIVSS?
208800120605             ( wCount_3EW <> 1        or
208900120605               // -?Nessun codice selezionato da TIVSS?
209000120605               W4Csun = *zero  or  W4Csun <> TIVSSds.VSSsun);
209100120605           $VideoPrec = $Video;
209200120605           $Video = 'W3';
209300120605           clear  TB28W3;
209400120605           W3txt2 = 'Annullare ' + %editc( Old_§3Ccks : 'X' ) +
209500120605                    ' in tab. "3EW".';
209600120802           W3txt3 = 'Premere Invio per uscire.';
209700120802           exfmt  TB28W3;
209800120605           reset  $Annull_3EW;
209900120605           $Video = $VideoPrec;
210000150319           clear  $VideoPrec;
210100120605         EndIf;
210200120726
210300120726         // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
210400120726         //   ?se cliente presente più di una sola volta nella "3EW"?
210500120726         //   ?(errore "alternativo" al precedente: meglio non uscire?
210600120726         //   ?dalla subroutine per eseguire comunque la pulizia dei?
210700120726         //   ?campi sottostante)?
210800120726         //if  wCount_3EW > 1;
210900120726         //  // -?Abilitazione F13=Interrog. tab. "3EW"?
211000120726         //  F13Attivo   = *on;
211100120726         //  VIDmsg = $Msg(40);
211200120726         //  //PosCurNRS   = *on;
211300120726         //  PosCurCKS   = *on;
211400120726         //  ErrMessage  = *on;
211500120726         //  ErrGenerico = *on;
211600120726         //  leavesr;
211700120726         //endif;
211800120726         if  wCount_3EW > 1;
211900120726           $VideoPrec  = $Video;
212000120726           $Video      = 'W4';
212100120726           reset  $InzW04;
212200120726           ErrGenerico = *on;
212300120726           leavesr;
212400120726         endif;
212500160317
212600160317         // -?Cappario semplificato ?
212700160317         //  ?Ambito e Tertminal partenza vanno valorizzati entrambi o
212800160317         //  ?lasciati vuoti entrambi
212900160317         if  V2Amb <> *blank and V2TFP = 0  OR
213000160317             V2Amb =  *blank and V2TFP <>0;
213100160317           VIDmsg = %trimr($Msg(48));
213200160317           PosCurAMB   = *on;
213300160317           ErrMessage  = *on;
213400160317           ErrGenerico = *on;
213500160317           leavesr;
213600160317         endif;
213700160317
213800160317         // -?Terminal partenza?
213900160321         // 999 non esiste ma vale tutti
214000160321         if  V2Amb <> *blank and V2TFP > 0
214100160321             and V2TFP <> 999;
214200160317           chain  (V2TFP)  AZORG;
214300160317           if  NOT %found(AZORG01L) OR
214400160317               %found(AZORG01L) and %subst(ORGDE8:1:1)<>'1';
214500160317             VIDmsg = $Msg(49);
214600160317             PosCurAMB   = *on;
214700160317             ErrMessage  = *on;
214800160317             ErrGenerico = *on;
214900160317             leavesr;
215000160317           endif;
215100160317         endif;
215200100312
215300100312         // -?Memorizzazione codici già controllati con esito positivo?
215400100312         //  ?per evitare di ripetere tali controlli...?
215500100312         //  ?(se non sono stati rilevati errori: in caso di errore?
215600100312         //  ?si sarebbe usciti prima dalla subr.!)?
215700120125         //  ?Meglio spostare tali memorizzazioni subito DOPO i loro?
215800120125         //  ?controlli specifici: questa memorizzazione salterebbe?
215900120125         //  ?in caso di errore ad un controllo legato ad un campo?
216000120125         //  ?della loro struttura dati (vedi §1BFG6).?
216100120125         //SavCKS = V2Ccks;             ?(già fatto)?
216200120125         //SavCBA = V2Ccba;             ?(già fatto)?
216300120125         //SavCTM = V2Cctm;             ?(già fatto)?
216400091104
216500091104       ENDSR;
216600101014
216700101014       //--------------------------------------------------------------
216800101014       //?Controllo range segnacolli per filiale e serie.              ?
216900101014       //--------------------------------------------------------------
217000101014       BEGSR  sr_Ctrl_NRS_SCx;
217100110121
217200110207         // -?Reperimento famiglia completa?
217300120517         //  ?(se premuto F11  PRIMA  di Invio, la famiglia?
217400110121         //  ? NON è ancora stata reperita)?
217500120104         exsr  sr_ubLeg3C;
217600101014
217700110207         // -?Controllo range segnacolli?
217800101014         clear  TNTB54ds;
217900101014         if  V2Dfls > *zero;
218000101014           I54fil = V2Dfls;
218100101014         else;
218200101014           I54fil = %subst( V1Ccli : 1 : %len(I54fil) );
218300101014         endif;
218400101014         I54ksc = V1Ccli;
218500101014         I54tel = 'C';
218600101014         I54nrs = %editc( W1Cnrs : 'X' );
218700101014         I54dal = W1Cncd;
218800101014         I54al  = W1Cnca;
218900101014
219000101014         kpjbu  = TNTB54ds;
219100110207         tntb54r ( kpjba : ubLeg3C_SKC );
219200101014         TNTB54ds = kpjbu;
219300101014
219400101014         SELECT;
219500120131
219600120131           // ->?Serie già assegnata come completa (in tab. "3CL")?
219700120131           //   ?La segnalazione qui scarterebbe gli altri controlli di?
219800120131           //   ?TRTB28R2 !?
219900120131           //WHEN  O54err = 'A';
220000120131           //  //$FlsNrsIn3CL = *on;
220100120131           //  ErrGenerico = *on;
220200120131           //  leavesr;
220300160125           // WHEN  O54err = 'A'  and  (W1Cncd > 1  or
220400160125           //                          W1Cnca < *hival)
220500170321           // ri-aggiunto ma forzabile
220600160125           WHEN  O54err = 'A'
220700160125                               and (I54Nrs<>Save3CLNRS or
220800160125                                    I54Fil<>Save3CLLNP)   ;
220900120131             VIDmsg = $Msg(20);
221000160125             VIDmsg = %trimr(VIDmsg) +
221100160125                        '. Premere "Invio" per forzare';
221200160125             Save3CLNRS = I54NrS;
221300160125             Save3CLLNP = I54Fil;
221400170320             ErrMessage  = *on;
221500170320             ErrGenerico = *on;
221600170320             leavesr;
221700170321           // se richiesta la forzatura
221800170321           WHEN  O54err = 'A'
221900170321                               and (I54Nrs=Save3CLNRS and
222000170321                                    I54Fil=Save3CLLNP)
222100170321            // ma il supporto non è SNA (logistica)
222200170321            and V2CCBA <> 'SNA';
222300170321             // emetto una window per una doppia sicurezza
222400170321             W3_3CLAttiva = *on;
222500170321             $VideoPrec = $Video;
222600170321             $Video = 'W3';
222700170321             clear  TB28W3;
222800170321             //      12345678901234567890123456789012345
222900170321             W3txt1='Serie già assegnata in tabella 3CL';
223000170321             W3txt2='ma tipo Supporto Cliente non SNA.';
223100170321             W3txt3='Procedere SOLO se necessario.';
223200170321             dou dsp_aid = c_F06
223300170321              or dsp_aid = c_F12;
223400170321               exfmt  TB28W3;
223500170321               select;
223600170321                when dsp_aid = c_F12;
223700170321                 //Dsp_Aid  = c_Enter ;
223800170321                 wForzatura = *off  ;
223900170321                 leave;
224000170321                when dsp_aid = c_F06;
224100170321                 wForzatura = *on   ;
224200170321                other;
224300170321                 wForzatura = *off  ;
224400170321               endsl;
224500170321             enddo;
224600170321             $Video = $VideoPrec;
224700170321             clear  $VideoPrec;
224800170321             clear  Save3CLNrS  ;
224900170321             clear  Save3CLLNP  ;
225000170321             W3_3CLAttiva = *off;
225100170321
225200160125             //select;
225300160125             //  when  $Video = 'D2';
225400160125             //    PosCurNRS   = *on;
225500160125             //  when  $Video = 'W1';
225600160125             //    PosCurW1nrs = *on;
225700160125             //endsl;
225800160125             //ErrMessage  = *on;
225900160125             //ErrGenerico = *on;
226000160125             //leavesr;
226100101014
226200101014           // ->?errore SQL?
226300101014           WHEN  O54err = 'S';
226400101014             VIDmsg = $Msg(18);
226500101014             select;
226600101014               when  $Video = 'D2';
226700101014                 PosCurNRS   = *on;
226800101014               when  $Video = 'W1';
226900101014                 PosCurW1nrs = *on;
227000101014             endsl;
227100101014             ErrMessage  = *on;
227200101014             ErrGenerico = *on;
227300101014             leavesr;
227400101014
227500101014           // ->?serie già presente come parziale?
227600110121           //   ?(non nel padre)?
227700110121           WHEN  O54err =  '1'      and
227800120131                 // (W1Cnrs <> SavNRS   or
227900120131                 //  W1Cncd <> SavNCD   or
228000120131                 //  W1Cnca <> SavNCA)  and
228100110121                 %lookup( %int(O54ksc) : sch_SKC ) = *zero;
228200101014             if  O54dal >  1   or   O54al <  *hival;
228300101014               VIDmsg = %trimr($Msg(19)) + ' ' + O54ksc;
228400101014             else;
228500101014               VIDmsg = %trimr($Msg(21)) + ' ' + O54ksc;
228600101014             endif;
228700101014             select;
228800101014               when  $Video = 'D2';
228900101014                 PosCurNRS   = *on;
229000101014               when  $Video = 'W1';
229100101014                 PosCurW1nrs = *on;
229200101014             endsl;
229300101014             ErrMessage  = *on;
229400101014             ErrGenerico = *on;
229500101014
229600101014             // ->?errore forzabile SE con stesso padre?
229700110121             //   ?(NON più: errore scartato all'inizio)?
229800110121             //if  %lookup( %int(O54ksc) : sch_SKC ) > *zero;
229900110121             //  SavNRS = W1Cnrs;
230000110121             //  SavNCD = W1Cncd;
230100110121             //  SavNCA = W1Cnca;
230200110121             //  VIDmsg = %trimr(VIDmsg) +
230300110121             //          '. Premere "Invio" x forzare';
230400110121             //endif;
230500101014             leavesr;
230600120113
230700120113           // ->?Serie NON ammessa (99)?
230800120113           WHEN  O54err = '9';
230900120113             VIDmsg = $Msg(43);
231000120113             select;
231100120113               when  $Video = 'D2';
231200120113                 PosCurNRS   = *on;
231300120113               when  $Video = 'W1';
231400120113                 PosCurW1nrs = *on;
231500120113             endsl;
231600120113             ErrMessage  = *on;
231700120113             ErrGenerico = *on;
231800120113             leavesr;
231900101014
232000101014           // ->?verifica se serie già presente come completa;?
232100101014           OTHER;
232200120104             //If  O54prz = *blank;
232300120104             If  ( (V2Dfls  > *zero  and  ParFIL <> %int(V2Dfls))   or
232400120202                   (V2Dfls <= *zero  and  ParFIL <> V2Ccli / 10000) or
232500120202                  ParNRS <> W1Cnrs   or
232600120202                  ParERR <> *blank )   and
232700120202                 (W1Cncd = 1  and  W1Cnca = 9999999);
232800101014               clear  TRTB28ds2;
232900101014               if  V2Dfls > *zero;
233000101014                 ParFIL = %int(V2Dfls);
233100101014               else;
233200101014                 ParFIL = V2Ccli / 10000;
233300101014               endif;
233400101014               ParNRS = W1Cnrs;
233500101014               ParELB = 'C';
233600101014               ParKSC = %editc( V2Ccli : 'X' );
233700111215               trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
233800101014
233900120113               select;
234000120113
234100120113                 // ->?Serie NON ammessa (99)?
234200120113                 when  ParERR = '9';
234300120113                   VIDmsg = $Msg(43);
234400120113                   select;
234500120113                     when  $Video = 'D2';
234600120113                       PosCurNRS   = *on;
234700120113                     when  $Video = 'W1';
234800120113                       PosCurW1nrs = *on;
234900120113                   endsl;
235000120113                   ErrMessage  = *on;
235100120113                   ErrGenerico = *on;
235200120113                   leavesr;
235300120113
235400120113                 // ->?errore bloccante SE con padre diverso?
235500120113                 // ->?errore forzabile SE con stesso padre?
235600120113                 //   ?(NON più: ora solo se con padre diverso)?
235700120113                 when  ParERR =  'E'      and
235800120131                       // (W1Cnrs <> SavNRS   or
235900120131                       //  W1Cncd <> SavNCD   or
236000120131                       //  W1Cnca <> SavNCA)  and
236100120113                       %lookup( %int(OUTksc) : sch_SKC ) = *zero;
236200120113                   VIDmsg = %trimr($Msg(21)) + ' ' + OUTksc;
236300120113                   select;
236400120113                     when  $Video = 'D2';
236500120113                       PosCurNRS   = *on;
236600120113                     when  $Video = 'W1';
236700120113                       PosCurW1nrs = *on;
236800120113                   endsl;
236900120113                   ErrMessage  = *on;
237000120113                   ErrGenerico = *on;
237100120113                   // ->?errore forzabile SE con stesso padre?
237200120113                   //if  %lookup( %int(OUTksc) : sch_SKC ) > *zero;
237300120113                   //  SavNRS = W1Cnrs;
237400120113                   //  SavNCD = W1Cncd;
237500120113                   //  SavNCA = W1Cnca;
237600120113                   //  VIDmsg = %trimr(VIDmsg) +
237700120113                   //          '. Premere "Invio" per forzare';
237800120113                   //endif;
237900120113                   leavesr;
238000120131
238100120131                 // ->?Serie già assegnata come completa (in tab. "3CL")?
238200120131                 //when  ParERR = 'A';
238300120131                 //  //$FlsNrsIn3CL = *on;
238400120131                 //  ErrGenerico = *on;
238500120131                 //  leavesr;
238600160125                 when  O54err = 'A'
238700160125                                     and (I54Nrs<>Save3CLNRS or
238800160125                                          I54Fil<>Save3CLLNP)   ;
238900160125                 //when  O54err = 'A'  and  (W1Cncd > 1  or
239000160125                 //                          W1Cnca < *hival)
239100160125                   VIDmsg = $Msg(20);
239200160125                   VIDmsg = %trimr(VIDmsg) +
239300160125                             '. Premere "Invio" per forzare';
239400160125                   Save3CLNRS = I54NrS;
239500160125                   Save3CLLNP = I54Fil;
239600170320                   ErrMessage  = *on;
239700170320                   ErrGenerico = *on;
239800170320                   leavesr;
239900160125                   //select;
240000160125                   //  when  $Video = 'D2';
240100160125                   //    PosCurNRS   = *on;
240200160125                   //  when  $Video = 'W1';
240300160125                   //    PosCurW1nrs = *on;
240400160125                   //endsl;
240500160125                   //ErrMessage  = *on;
240600160125                   //ErrGenerico = *on;
240700160125                   //leavesr;
240800120113
240900120113               endsl;
241000120131
241100120131               // ->?Serie già assegnata come completa (in tab. "3CL")?
241200120131               //   ?- rilevata dal *pgm TNTB54R -?
241300120131               //   ?(da segnalare in assenza di altri errori)?
241400120131               //if  O54err = 'A';
241500120131               //  //$FlsNrsIn3CL = *on;
241600120131               //  ErrGenerico = *on;
241700120131               //  leavesr;
241800120131               //endif;
241900101014
242000120104             EndIf;
242100101014
242200101014         ENDSL;
242300121112
242400121220         // -?SE modificato il supporto Cli-BRT (da o in "ESWEB")?
242500121112         //  ?occorre cambiare range?
242600121112         //  ?(lasciando il "vecchio" "bloccato" in tab. "3EW")?
242700121220         If  (Old_§3Ccba =  c_EasyWEB   and
242800121220              V2Ccba     <> c_EasyWEB    or
242900121220              Old_§3Ccba <> c_EasyWEB   and
243000121220              V2Ccba     =  c_EasyWEB)  AND
243100121220             (Old_§3Ccba =  c_EasyWEB         and
243200121220             ($Video     =  'D2'              and
243300121220              V2Cnrs     =  d3EW_Old.§3EWnrs  and
243400121220              V2Cncd     =  d3EW_Old.§3EWnsi  and
243500121220              V2Cnca     =  d3EW_Old.§3EWnsf  or
243600121220              $Video     =  'W1'              and
243700121220              W1Cnrs     =  d3EW_Old.§3EWnrs  and
243800121220              W1Cncd     =  d3EW_Old.§3EWnsi  and
243900121220              W1Cnca     =  d3EW_Old.§3EWnsf)  OR
244000121220              Old_§3Ccba <> c_EasyWEB         and
244100121220             ($Video     =  'D2'              and
244200121220              V2Cnrs     =  Save_3CP_nrs      and
244300121220              V2Cncd     =  Save_3CP_nsi      and
244400121220              V2Cnca     =  Save_3CP_nsf      or
244500121220              $Video     =  'W1'              and
244600121220              W1Cnrs     =  Save_3CP_nrs      and
244700121220              W1Cncd     =  Save_3CP_nsi      and
244800121220              W1Cnca     =  Save_3CP_nsf));
244900121128           //VIDmsg = $Msg(45);
245000121128           VIDmsg = $Msg(44);
245100121112           select;
245200121112             when  $Video = 'D2';
245300121112               PosCurNRS   = *on;
245400121112             when  $Video = 'W1';
245500121112               PosCurW1nrs = *on;
245600121112           endsl;
245700121112           ErrMessage  = *on;
245800121112           ErrGenerico = *on;
245900121112           leavesr;
246000121112         EndIf;
246100121127
246200121128         // -?ULTIMO CONTROLLO (BYPASSABILE) in fase di ricerca range?
246300121128         //  ?parziale: Range "anomalo"?
246400140718         //IF  $Video =  'W1'   and
246500140718         //    ( W1Cnrs <> SavW1Cnrs  or  W1Cncd <> SavW1Cncd
246600140718         //                           or  W1Cnca <> SavW1Cnca )  and  (
246700140718         //    %subst( %editc( W1Cncd : 'X' ) : %len(W1Cncd) : 1 ) <> '1'
246800140718         //                         or  (
246900140718         //    %subst( %editc( W1Cnca : 'X' ) : %len(W1Cnca) : 1 ) <> '0'
247000140718         //                        and  W1Cnca <> *hival ) );
247100140718         //
247200140718         //  SavW1Cnrs = W1Cnrs;
247300140718         //  SavW1Cncd = W1Cncd;
247400140718         //  SavW1Cnca = W1Cnca;
247500140718         //
247600140718         //  Select;
247700140718         //
247800140718         //    When  %subst(%editc(W1Cncd:'X'):%len(W1Cncd):1) <> '1' and
247900140718         //          %subst(%editc(W1Cnca:'X'):%len(W1Cnca):1) <> '0' and
248000140718         //          W1Cnca <> *hival;
248100140718         //      VIDmsg = %trimr($Msg(46)) +
248200140718         //      //       ' non comincia con "...1" +
248300140718         //      //         né termina con "...0". +
248400140718         //               ' non inizia con ' +
248500140718         //               %subst(%editc(W1Cncd:'X'):1:%len(W1Cncd)-1)+'1'
248600140718         //             + ' né termina con';
248700140718         //      wW1Cncx = W1Cnca + 1;
248800140718         //      if  %subst(%editc(wW1Cncx:'X'):%len(wW1Cncx)-1:2) = '00';
248900140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
249000140718         //               %subst(%editc(wW1Cncx:'X'):1:%len(wW1Cncx));
249100140718         //      else;
249200140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
249300140718         //               %subst(%editc(W1Cnca:'X'):1:%len(W1Cnca)-1)+'0';
249400140718         //      endif;
249500140718         //      VIDmsg = %trimr(VIDmsg) + '. Invio.';
249600140718         //      PosCurW1ncd = *on;
249700140718         //
249800140718         //    When  %subst(%editc(W1Cncd:'X'):%len(W1Cncd):1) <> '1';
249900140718         //      VIDmsg = %trimr($Msg(46)) +
250000140718         //      //       ' dovrebbe cominciare con "...1". +
250100140718         //               ' dovrebbe cominciare con ' +
250200140718         //               %subst(%editc(W1Cncd:'X'):1:%len(W1Cncd)-1)+'1'
250300140718         //             + '. Premere Invio.';
250400140718         //      PosCurW1ncd = *on;
250500140718         //
250600140718         //    When  %subst(%editc(W1Cnca:'X'):%len(W1Cnca):1) <> '0' and
250700140718         //          W1Cnca <> *hival;
250800140718         //      VIDmsg = %trimr($Msg(46)) +
250900140718         //      //       ' dovrebbe terminare con "...0". +
251000140718         //               ' dovrebbe terminare con';
251100140718         //      wW1Cncx = W1Cnca + 1;
251200140718         //      if  %subst(%editc(wW1Cncx:'X'):%len(wW1Cncx)-1:2) = '00';
251300140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
251400140718         //               %subst(%editc(wW1Cncx:'X'):1:%len(wW1Cncx));
251500140718         //      else;
251600140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
251700140718         //               %subst(%editc(W1Cnca:'X'):1:%len(W1Cnca)-1)+'0';
251800140718         //      endif;
251900140718         //      VIDmsg = %trimr(VIDmsg) + '.  Premere Invio.';
252000140718         //      PosCurW1nca = *on;
252100140718         //
252200140718         //  EndSl;
252300140718         //
252400140718         //  ErrMessage  = *on;
252500140718         //  ErrGenerico = *on;
252600140718         //  leavesr;
252700140718         //
252800140718         //ENDIF;
252900101014
253000101014       ENDSR;
253100091105
253200091105       //--------------------------------------------------------------
253300091105       //?Gestione videata D03.                                        ?
253400091105       //--------------------------------------------------------------
253500091105       BEGSR  sr_GesD03;
253600091105
253700091126         // -?Inizializzazione videata?
253800091105         if  $InzD02 = *on;
253900091105           exsr  sr_InzD02;
254000091105           $InzD02 = *off;
254100091105         endif;
254200091120
254300091126         // -?(Dis)Abilitazione tasti funzionali?
254400091120         exsr  sr_AbilFxxD02;
254500091105
254600091126         // -?Emissione Testata e Piede?
254700091127         write  TB28T1;
254800091127         write  TB28P2;
254900091105
255000091126         // -?Emissione videata?
255100091105         if  Not $Protect;
255200091127           exfmt  TB28D3;
255300091105         else;
255400091127           write  TB28D3;
255500091105           exfmt  Protect;
255600091105         endif;
255700091120
255800091105         reset  ErrMessage;
255900091105         reset  ErrGenerico;
256000091105         clear  VIDmsg;
256100091120
256200091105         SELECT;
256300091105
256400091126           // -?F3=Fine?
256500091105           WHEN  dsp_aid = c_F03;
256600091105             unlock  TABEL00F;
256700091105             unlock  TNTBE01L;
256800091105             $Fine = *on;
256900091105
257000091126           // -?F10=Ricerca Serie (globale)?
257100091109           WHEN  dsp_aid = c_F10;
257200091109             exsr  sr_F10D02;
257300091105
257400091126           // -?F11=Ricerca Serie Parziale?
257500091109           WHEN  dsp_aid = c_F11;
257600091109             $VideoPrec = $Video;
257700091109             $Video     = 'W1';
257800091109             reset  $InzW01;
257900091105
258000091126           // -?F12=Ritorno?
258100091105           WHEN  dsp_aid = c_F12;
258200150319             $Video = 'D2';
258300150319             clear  $VideoPrec;
258400091105
258500091126           // -?F19=Gestione multimembro clienti VAB?
258600091126           // -?F20=Gestione scambio dati principali     con clienti?
258700091126           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
258800091126           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
258900091105           WHEN  dsp_aid = c_F19   or
259000091105                 dsp_aid = c_F20   or
259100091105                 dsp_aid = c_F21   or
259200091105                 dsp_aid = c_F22;
259300091105             exsr  sr_FxxD02;
259400091105
259500091126           // -?RollDown=Videata precedente?
259600091105           WHEN  dsp_aid = c_RollDown;
259700150319             $Video = 'D2';
259800150319             clear  $VideoPrec;
259900091105
260000091126           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
260100091105           OTHER;
260200091126             // -?Controlli eseguiti NON se F16=Annullamento?
260300091105             if  dsp_aid <> c_F16;
260400091105               exsr sr_CtrD03;
260500091105               if  Not ErrGenerico;
260600091105                 exsr sr_CtrD02;
260700091106                 if  ErrGenerico = *on;
260800091106                   $Video = 'D2';
260900091106                 endif;
261000091105               endif;
261100091105               if  ErrGenerico = *on;
261200091105                 leavesr;
261300091105               endif;
261400091105             endif;
261500091126             // -?Aggiornamento?
261600091105             if  dsp_aid = c_F05   or
261700091105                 dsp_aid = c_F06   or
261800091105                 dsp_aid = c_F16;
261900091105               exsr  sr_UpdateAll;
262000091105               if  ErrGenerico = *on;
262100091105                 leavesr;
262200091105               endif;
262300091126               // -?Uscita dal programma SE richiamato?
262400091105               if  $Called = *on   and   W0Ccli > *zero;
262500091105                 $Fine = *on;
262600091126               // -?Videata iniziale altrimenti?
262700091105               else;
262800091105                 $Video  = 'D1';
262900091105                 $InzD01 = *on;
263000091105               endif;
263100091105             endif;
263200091105
263300091105         ENDSL;
263400091105
263500091126         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
263600091105         if  $Video = 'D1';
263700091105           clear  V1Topz;
263800091105           clear  BloAnn;
263900091120           VisBloAnn = *off;
264000091105         endif;
264100091105
264200091105       ENDSR;
264300091105
264400091104       //--------------------------------------------------------------
264500091104       //?Controllo dati immessi nella videata D03.                    ?
264600091104       //--------------------------------------------------------------
264700091104       BEGSR  sr_CtrD03;
264800091104
264900091105         %subst(IndDspF : 71) = *off;
265000091104
265100091126         // -?Dati per accorpamento bolle?
265200091126         if  V2Cabd <> *blank;
265300091126           // ->?Lunghezza riferimento originale mittente obbligatoria?
265400091104           if  V2Clrm <= *zero   and   V2Calm <> *blank;
265500091125             VIDmsg = $Msg(24);
265600091104             PosCurLRM   = *on;
265700091104             ErrMessage  = *on;
265800091104             ErrGenerico = *on;
265900091104             leavesr;
266000091104           endif;
266100091104         else;
266200091126           // ->?Controllo campi subordinati all'accorpamento bolle?
266300091104           select;
266400091104             when  V2Clrm <> *zero;
266500091125               VIDmsg = $Msg(25);
266600091104               PosCurLRM   = *on;
266700091104               ErrMessage  = *on;
266800091104               ErrGenerico = *on;
266900091104               leavesr;
267000091104             when  V2Calm <> *blank;
267100091125               VIDmsg = $Msg(25);
267200091104               PosCurALM   = *on;
267300091104               ErrMessage  = *on;
267400091104               ErrGenerico = *on;
267500091104               leavesr;
267600091104             when  V2Cnot <> *blank;
267700091125               VIDmsg = $Msg(25);
267800091104               PosCurNOT   = *on;
267900091104               ErrMessage  = *on;
268000091104               ErrGenerico = *on;
268100091104               leavesr;
268200091104             when  V2Cabc <> *blank;
268300091125               VIDmsg = $Msg(25);
268400091104               PosCurABC   = *on;
268500091104               ErrMessage  = *on;
268600091104               ErrGenerico = *on;
268700091104               leavesr;
268800091104             when  V2Carc <> *blank;
268900091125               VIDmsg = $Msg(25);
269000091104               PosCurARC   = *on;
269100091104               ErrMessage  = *on;
269200091104               ErrGenerico = *on;
269300091104               leavesr;
269400091104             when  V2Cavx <> *blank;
269500091125               VIDmsg = $Msg(25);
269600091104               PosCurAVX   = *on;
269700091104               ErrMessage  = *on;
269800091104               ErrGenerico = *on;
269900091104               leavesr;
270000091106           endsl;
270100091106         endif;
270200091102
270300091102       ENDSR;
270400091125
270500091125       //--------------------------------------------------------------
270600091125       //?Ricerche.                                                    ?
270700091125       //--------------------------------------------------------------
270800091125       BEGSR  sr_Search;
270900091125
271000091125         select;
271100091125
271200120305           // -?Supporto Cliente a BRT?
271300091125           when  %scan( '?' : V2Ccba ) > *zero;
271400091125             clear  V2Ccba;
271500091125             clear  V2Dcba;
271600091125             reset  TIBS02ds;
271700120605             //T02mod = 'R';                //?(già così)?
271800120605             //T02cod = '3CS';              //?(già così)?
271900091125             T02sif = knsif;
272000091125             TNTBE_RicercaControllo (kpjba : TIBS02ds);
272100091125             if  T02err = *blank;
272200091125               V2Ccba = T02ke1;
272300091125               V2Dcba = %subst( T02uni : 1 : %len(V2Dcba) );
272400091126               // -?Controlli specifici sull'abilitazione ad EasyWEB?
272500091126               if  V2Ccba = c_EasyWEB;
272600091126                 exsr  sr_CtrlEasyWEB;
272700120725                 Reverse_3EW = ( Not $Inzd02   and
272800120725                               ( SaveFLS <> V2Dfls  or
272900120725                                 SaveNRS <> V2Cnrs  or
273000120725                                 SaveNCD <> V2Cncd  or
273100120725                                 SaveNCA <> V2Cnca  or
273200120725                                 SaveCTM <> V2Cctm ) );
273300091126                 if  ErrGenerico;
273400091126                   leavesr;
273500091126                 endif;
273600110311               else;
273700110311                 exsr  sr_CtrlRngPadre;
273800110422                 // -?Qui NON serve: viene COMUNQUE acceso dopo!?
273900110422                 //if  ErrGenerico;
274000110422                 //  clear  ErrGenerico;
274100110422                 //endif;
274200091126               endif;
274300091125             else;
274400091125               errMessage = *on;
274500091125               VIDmsg = T02msg;
274600091125             endif;
274700091125             errGenerico = *on;
274800091125             PosCurCBA   = *on;
274900091125             leavesr;
275000091125
275100091126           // -?Codice Trattamento Merce?
275200091125           when  %scan( '?' : V2Cctm ) > *zero;
275300091125             clear  V2Cctm;
275400091125             clear  V2Dctm;
275500091125             X§Tkut = c_Kut;
275600091125             X§Tcod = '1B';
275700091125             clear  X§Tkey;
275800091125             clear  X§Tdes;
275900091125             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
276000091125             V2Cctm = %subst(X§Tkey : 1 : %len(V2Cctm));
276100091125             ErrGenerico = *on;
276200091125             leavesr;
276300091125
276400091125         endsl;
276500091125
276600091125       ENDSR;
276700091124
276800091124       //--------------------------------------------------------------
276900091124       //?Controllo/Decodifica Cliente Unificante.                     ?
277000091124       //--------------------------------------------------------------
277100091126       BEGSR  sr_CtrlCKS;
277200091124
277300091124         //clear  V2Dcks;         (NO perchè é i/o !!!)
277400110311         clear  wNSI_Padre;
277500110311         clear  d3CP_Padre;
277600110311         clear  d3EW_Padre;
277700110421         reset  SavCliTab3CPEW;
277800091124
277900091124         SELECT;
278000091124
278100091126           // ->?Ricerca alfabetica?
278200091124           WHEN  V2Ccks = *zero   and   V2Dcks <> *blank;
278300091124             xParDut = RSut;
278400091124             xParKut = 1;
278500091124             xParRag = V2Dcks;
278600091124             xParKcc = DUTkci;
278700091124             xParSta = 9;
278800091124             xParFlr = %editc( V1Cfil : 'X');
278900091124             clear  xParDit;
279000091124             xParNum = 1;
279100091124             clear  xParKcm;
279200091124             clear  xParKsm;
279300091124             clear  xParKdm;
279400091124             clear  xParEsci;
279500091124             clear  xParErr;
279600091124             clear  xParIva;
279700091124             clear  xParCdf;
279800091124             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
279900091124                        xParSta : xParFlr : xParDit : xParNum :
280000091124                        xParKcm : xParKsm : xParKdm : xParEsci :
280100091124                        xParErr : xParIva : xParCdf );
280200091124             if  xParSta <> -1   and   xParErr = *blank;
280300091124               V2Ccks = %int( %subst( xParKsm : 1 : %len(V2Ccks) ) );
280400091124               V2Dcks = xParRag;
280500091124             endif;
280600091124             ErrGenerico = *on;
280700091124             leavesr;
280800091124
280900091126           // ->?Non inserito?
281000091124           WHEN  V2Ccks = *zero;
281100091124             VIDmsg = $Msg(06);
281200091124             PosCurCKS   = *on;
281300091124             ErrMessage  = *on;
281400091124             ErrGenerico = *on;
281500091124             leavesr;
281600091124
281700091126           // ->?Controllo / Decodifica?
281800091124           OTHER;
281900091126             if  V2Ccks <> I69kac;
282000091126               clear  TIBS69ds;
282100091126               I69sif = knsif;
282200091126               I69kcc = DUTkci;
282300091126               I69kac = V2Ccks;
282400091126               tibs69r( tibs69ds :
282500091126                        ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
282600091126             endif;
282700091124             if  O69err = *on;
282800091124               VIDmsg = $Msg(07);
282900091124               PosCurCKS   = *on;
283000091124               ErrMessage  = *on;
283100091124               ErrGenerico = *on;
283200091124               leavesr;
283300091124             endif;
283400091124             V2Dcks = %subst( ACOrag : 1 : %len(V2Dcks) );
283500091124
283600091126             // -?SE diverso da V2Ccli: controllo se possibile?
283700091124             clear  ds3C_padre;
283800091124             If  V2Ccks <> V2Ccli;
283900091124
284000100203               // -?Cliente unificante NON in tab. "3C"?
284100091124               if  getTabella ( c_Tabel : '3C' : %editc(V2Ccks:'X') :
284200091124                                *omit : *omit : *omit :
284300091124                                *omit : *omit :
284400091124                                *omit : *omit : *omit : *omit :
284500091124                                *omit : *omit : *omit : *omit :
284600091124                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
284700091124                              < *zero       OR
284800091124                   ds_TNTBE.TBEatb <> *blank;
284900091124                 VIDmsg = $Msg(08);
285000091124                 PosCurCKS   = *on;
285100091124                 ErrMessage  = *on;
285200091124                 ErrGenerico = *on;
285300091124                 leavesr;
285400091124               else;
285500120214                 ds3C_Padre = ds_TNTBE.TBEuni;
285600091124               endif;
285700100203
285800100203               // -?Cliente unificante già figlio?
285900100203               if  V2Ccks <> p_§3Ccks;
286000100203                 VIDmsg = %trimr($Msg(07)) + ': anch''esso è figlio';
286100100203                 PosCurCKS   = *on;
286200100203                 ErrMessage  = *on;
286300100203                 ErrGenerico = *on;
286400100203                 leavesr;
286500100203               endif;
286600091124
286700100917               // -?Cliente unificante a video diverso da quello già?
286800100917               //  ?memorizzato: verifica fattibilità (SE il vecchio?
286900120215               //  ?era padre di se stesso: NON deve avere figli)?
287000110302               if  %found(TABEL00F)   and
287100100917                   V2Ccks <> §3Ccks   and   V2Ccli = §3Ccks;
287200120605
287300100917                 // -?Padre con figli: NON può diventare figlio?
287400120517                 if  sch_SKC_Orig(2) > *zero;
287500100917                   VIDmsg = %trimr($Msg(09)) + ' ' +
287600120517                      %editc( sch_SKC_Orig(2) : 'X' );
287700120517                   if  sch_SKC_Orig(3) > 1;
287800100917                     VIDmsg = %trimr(VIDmsg) + ' ed altri';
287900100917                   endif;
288000100917                   PosCurCKS   = *on;
288100100917                   ErrMessage  = *on;
288200100917                   ErrGenerico = *on;
288300100917                   leavesr;
288400100917                 endif;
288500120214                 // -?Padre già abilitato ad EasySped-WEB: non può?
288600120214                 //  ?diventare figlio?
288700120215                 if  Not  $3EWupd         and
288800120215                     §3Ccba =  c_EasyWEB  and
288900120214                     §3Ccks <> V2Ccks     and
289000120605                     %editc( §3Ccks : 'X' ) =
289100120605                       %subst( TIVSSds_Orig.VSSksu : 2 );
289200120215                   VIDmsg = %trimr( $Msg(32) ) + ' ' +
289300120215                            %editc( §3Ccks : 'X' );
289400120214                   PosCurCKS   = *on;
289500120214                   ErrMessage  = *on;
289600120214                   ErrGenerico = *on;
289700120214                   leavesr;
289800120214                 endif;
289900091214               endif;
290000091124
290100091124             Else;
290200091124
290300091124               ds3C_Padre = ds3C;
290400091124
290500091124             EndIf;
290600091124
290700091124         ENDSL;
290800091124
290900100312         // -?Reperimento chiave completa per tab. "3EW" alla?
291000100312         //  ?variazione del cliente unificante?
291100100312         //  ?se padre non trovato nel file TIVSS non può essere in?
291200100312         //  ?  tab. "3EW"?
291300120725         //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
291400120725         if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
291500101028           clear  rqsSUN;
291600101026           exsr  sr_TIS7701_x_TIVSS;
291700100312         endif;
291800100322
291900100322         // -?Reperimento famiglia completa?
292000120104         exsr  sr_ubLeg3C;
292100091124
292200091124       ENDSR;
292300091106
292400091106       //--------------------------------------------------------------
292500120305       //?Controllo/Decodifica Supporto Cliente a BRT.                 ?
292600091106       //--------------------------------------------------------------
292700091106       BEGSR  sr_CtrlCBA;
292800091106
292900091106         clear  V2Dcba;
293000091106
293100091126         // ->?Vuoto?
293200091125         if  V2Ccba = *blank;
293300091125
293400091125           if  V2Ccks = V2Ccli;
293500091125             VIDmsg = $Msg(11);
293600091125             PosCurCBA   = *on;
293700091125             ErrMessage  = *on;
293800091125             ErrGenerico = *on;
293900091125             leavesr;
294000091125           endif;
294100091106
294200100211         // ->?Controllo / Decodifica?
294300091125         else;
294400091125           ds_TNTBE.TBEke1 = V2Ccba;
294500091125           if  getTabella ( c_Tntbe : '3CS' : ds_TNTBE.TBEke1 :
294600091125                            *omit   : *omit : *omit :
294700091125                            *omit : *omit :
294800091125                            *omit : *omit : *omit : *omit :
294900091125                            *omit : *omit : *omit : *omit :
295000091125                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
295100091125                          = *zero      AND
295200091125               ds_TNTBE.TBEatb = *blank;
295300091125             V2Dcba = %subst( ds_TNTBE.TBEuni : 1 : %len( V2Dcba ) );
295400091125           else;
295500091127             V2Dcba = *all'? ';
295600091125             VIDmsg = $Msg(12);
295700091125             PosCurCBA   = *on;
295800091125             ErrMessage  = *on;
295900091125             ErrGenerico = *on;
296000091125             leavesr;
296100091125           endif;
296200091106
296300091125         endif;
296400091124
296500091126         // -?Controlli specifici sull'abilitazione ad EasyWEB?
296600091125         if  V2Ccba = c_EasyWEB;
296700091126           exsr  sr_CtrlEasyWEB;
296800120725           Reverse_3EW = ( Not $Inzd02   and
296900120725                         ( SaveFLS <> V2Dfls  or
297000120725                           SaveNRS <> V2Cnrs  or
297100120725                           SaveNCD <> V2Cncd  or
297200120725                           SaveNCA <> V2Cnca  or
297300120725                           SaveCTM <> V2Cctm ) );
297400091124           if  ErrGenerico;
297500091124             leavesr;
297600091124           endif;
297700100211         else;
297800100211           exsr  sr_Ctrl3EW;
297900100211           if  ErrGenerico;
298000100211             leavesr;
298100100211           endif;
298200091124         endif;
298300091106
298400091106       ENDSR;
298500091124
298600091124       //--------------------------------------------------------------
298700091124       //?Controlli specifici sull'abilitazione ad EasyWEB             ?
298800120215       //?(se messo/lasciato supporto EasySped-WEB)                    ?
298900150127       //--------------------------------------------------------------
299000091126       BEGSR  sr_CtrlEasyWEB;
299100150127
299200150127         V23EWLnP = *blank;
299300091127
299400120305         // -?"Supporto Cliente a BRT" diverso da quello del "PADRE"?
299500091127         if  V2Ccks <> V2Ccli   and   p_§3Ccba <> c_EasyWEB;
299600091127           VIDmsg = %trimr($Msg(13)) + c_EasyWEB + '": "' +
299700091127                    p_§3Ccba + '"';
299800091127           PosCurCKS   = *on;
299900091127           ErrMessage  = *on;
300000091127           ErrGenerico = *on;
300100091127           leavesr;
300200091127         endif;
300300091124
300400091126         // -?Controllo abilitazione a STRATEGI del cliente "PADRE"?
300500120725         //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
300600120725         if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
300700101028           clear  rqsSUN;
300800101026           exsr  sr_TIS7701_x_TIVSS;
300900100312         endif;
301000120725         //select;
301100120725         //  when  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
301200120725         //                            or   TIVSSds.VSSksu = *zero;
301300120725         //    VIDmsg = $Msg(14);
301400120725         //    if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
301500120725         //        V2Cncd = 1          and  V2Cnca = *all'9';
301600120725         //      //F13Attivo = *on;       ?(già così)?
301700120725         //      //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
301800120725         //      VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
301900120725         //    endif;
302000120725         //    PosCurCKS   = *on;
302100120725         //    ErrMessage  = *on;
302200120725         //    ErrGenerico = *on;
302300120725         //    leavesr;
302400120725         //  // -?Abilitazione scaduta?
302500120725         //  //  ?(IMPOSSIBILE: TIS7701R avrebbe restituito errore!)?
302600120725         //  //when  TIVSSds.VSSdsc < wDate;
302700120725         //  //  VIDmsg = $Msg(37);
302800120725         //  //  PosCurCBA   = *on;
302900120725         //  //  ErrMessage  = *on;
303000120725         //  //  ErrGenerico = *on;
303100120725         //  //  leavesr;
303200120725         //endsl;
303300091125
303400100212
303500100212         // -Azioni specifiche per cliente abilitato ad EasyWeb?
303600100212
303700100212         IF  $EasyWeb = *off;
303800100212
303900100212           $EasyWeb = *on;
304000100212
304100100212           // -?Protezione campi già impostati in tab. "3EW"?
304200100212           Protect_3EW = *on;
304300100212           // -?Reverse image x campi già impostati in tab. "3EW"?
304400120725           //  ?(SOTTO, solo se cambiati rispetto a quelli già?
304500120725           //   ?visualizzati)?
304600120725           //if  Not $Inzd02;
304700120725           //  Reverse_3EW = *on;
304800120725           //endif;
304900100212
305000100212           // -?Pulizia campi già impostati in tab. "3EW"?
305100120725           SaveFLS = V2Dfls;
305200150127           SaveLNP = V23EWLNP;
305300120725           SaveNRS = V2Cnrs;
305400120725           SaveNCD = V2Cncd;
305500120725           SaveNCA = V2Cnca;
305600120725           SaveCTM = V2Cctm;
305700120725           clear  V2Dfls;
305800150127           clear  V23EWLNP;
305900120725           clear  V2Cnrs;
306000120725           clear  V2Cncd;
306100120725           clear  V2Cnca;
306200120725           clear  V2Cctm;
306300120725           //clear  V2Dctm;
306400100212
306500100212           // -?Verifica se padre presente una sola volta in tab. "3EW"?
306600120725           ////if  V2Ccks <> SavCKS;
306700121015           //if  wCount_3EW     <= *zero                         or
306800121015           //    TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' )  or
306900121015           //    TIVSSds.VSSsun <= *zero;
307000120726             exsr  sr_Count3EW;
307100121015           //endif;
307200100212
307300120725           Select;
307400120725
307500120725             // -?Padre NON presente in tab. "3EW"?
307600120725             When  TBLflg <> *blank   and   wCount_3EW = *zero;
307700120725               reset  $EasyWeb;
307800120725               clear  V2Dctm;
307900120725               VIDmsg = $Msg(38);
308000120725               PosCurCKS   = *on;
308100120725               ErrMessage  = *on;
308200120725               ErrGenerico = *on;
308300120725               leavesr;
308400120816
308500120816             // -?Padre presente ma NON attivo in tab. "3EW"?
308600120816             When  wCount_3EW = *zero;
308700120816               // -?Abilitazione F13=Interrog. tab. "3EW"?
308800120816               if  Not  F13Attivo;
308900120816                 F13Attivo   = *on;
309000120816               endif;
309100120725
309200120725             // -?Reperimento dati del codice padre da tab. "3EW"?
309300120725             //  ?(se presente una sola volta)?
309400120725             When  wCount_3EW = 1;
309500120725               // -?Impostazione campi protetti con i valori del padre?
309600120725               if  d3EW.§3EWfls <> *zero;
309700120725                 V2Dfls = %editc( d3EW.§3EWfls : 'X' );
309800120725               endif;
309900150127               if  d3EW.§3EWLNP <> *zero;
310000150127                 V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
310100150127               endif;
310200120725               V2Cnrs = d3EW.§3EWnrs;
310300120725               V2Cncd = d3EW.§3EWnsi;
310400120725               V2Cnca = d3EW.§3EWnsf;
310500120725               V2Cctm = d3EW.§3EWctm;
310600140123               //if  V2Cctm <> SaveCTM;
310700140123               //  clear  V2Dctm;
310800140123               //endif;
310900140123               // -?"Forzature" richiestemi da Mirko C.?
311000140123               If  V2Ccba = c_EasyWEB;
311100140123                 // -?Supporto BRT a Cliente = "EMAIL"?
311200140123                 if  V2Ccli = V2Ccks;
311300140123                   V2Cbac = 'EMAIL';
311400140123                 endif;
311500140123                 // -?Abilitazione Serie in Bollettazione = "S"?
311600140123                 V2Cfg1 = 'S';
311700140123               EndIf;
311800120725               // -?AVVERTIMENTO forzatura dati dal padre?
311900120725               //   ·?filiale segnacolli?
312000120725               //   ·?numero serie segnacolli?
312100120725               //   ·?range segnacolli?
312200120725               //   ·?codice trattamento merce?
312300120927               if  Not $InzD02        and
312400120927                   V2Ccks  <> V2Ccli  and
312500120927                  (SaveFLS <> V2Dfls  or
312600150127                   SaveLNP <> V23EWLNP or
312700150127                   SaveNRS <> V2Cnrs  or
312800120927                   SaveNCD <> V2Cncd  or
312900120927                   SaveNCA <> V2Cnca  or
313000120927                   SaveCTM <> V2Cctm);
313100120725                 VIDmsg = $Msg(39);
313200120725                 ErrMessage  = *on;
313300120725                 ErrGenerico = *on;
313400120725                 leavesr;
313500120725               endif;
313600120725
313700120725             // -?AVVISO e consentita l'interrogazione della tab. "3EW"?
313800120725             //  ?(se presente più di una sola volta)?
313900120725             When  wCount_3EW > 1;
314000120725               // -?Impostazione campi protetti con i valori del padre?
314100120725               if  d3EW.§3EWfls <> *zero;
314200120725                 V2Dfls = %editc( d3EW.§3EWfls : 'X' );
314300120725               endif;
314400150127               if  d3EW.§3EWLNP <> *zero;
314500150127                 V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
314600150127               endif;
314700120725               V2Cnrs = d3EW.§3EWnrs;
314800120725               V2Cncd = d3EW.§3EWnsi;
314900120725               V2Cnca = d3EW.§3EWnsf;
315000120725               V2Cctm = d3EW.§3EWctm;
315100120725               if  V2Cctm <> SaveCTM;
315200120725                 clear  V2Dctm;
315300120725               endif;
315400120725               // -?Abilitazione F13=Interrog. tab. "3EW"?
315500120725               if  Not  F13Attivo;
315600120725                 F13Attivo   = *on;
315700120725                 VIDmsg = $Msg(40);
315800120725                 //PosCurNRS   = *on;
315900120725                 PosCurCKS   = *on;
316000120725                 ErrMessage  = *on;
316100120725                 ErrGenerico = *on;
316200120725                 leavesr;
316300120725               endif;
316400120725
316500120725           EndSl;
316600120725
316700120725           Reverse_3EW = ( Not $Inzd02   and
316800120725                         ( SaveFLS <> V2Dfls  or
316900150127                           SaveLNP <> V23EWLNP or
317000150127                           SaveNRS <> V2Cnrs  or
317100120725                           SaveNCD <> V2Cncd  or
317200120725                           SaveNCA <> V2Cnca  or
317300120725                           SaveCTM <> V2Cctm ) );
317400120725
317500120725         ENDIF;
317600120725
317700120725         If  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
317800120725                                 or   TIVSSds.VSSksu = *zero;
317900120725           VIDmsg = $Msg(14);
318000120725           if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
318100120725               V2Cncd = 1          and  V2Cnca = *all'9';
318200120725             //F13Attivo = *on;       ?(già così)?
318300120725             //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
318400120725             VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
318500120725           endif;
318600120725           PosCurCKS   = *on;
318700120725           ErrMessage  = *on;
318800120725           ErrGenerico = *on;
318900120725           leavesr;
319000120725         EndIf;
319100091124
319200091126         // -?Serie obbligatoria?
319300091124         if  V2Cnrs = *zero;
319400091130           VIDmsg = %trimr($Msg(15)) + ' "' + c_EasyWEB + '"';
319500120816           // -?Già abilitato F13=Interrog. tab. "3EW"?
319600120816           if  F13Attivo;
319700120816             VIDmsg = %trimr( VIDmsg ) + ': F13 per selezionare';
319800120816           endif;
319900091124           PosCurNRS   = *on;
320000091124           ErrMessage  = *on;
320100091124           ErrGenerico = *on;
320200091124           leavesr;
320300091124         endif;
320400091124
320500091124       ENDSR;
320600100211
320700100211       //--------------------------------------------------------------
320800120215       //?Verifica abilitazione cliente (SE unificante) in tab. "3EW"  ?
320900120215       //?(SE tolto supporto EasySped-WEB / lasciato altro)            ?
321000100211       //--------------------------------------------------------------
321100100211       BEGSR  sr_Ctrl3EW;
321200100407
321300120215         // -?Controlli da fare SOLO per cliente padre?
321400100407         if  V2Ccli <> V2Ccks;
321500100407           leavesr;
321600100407         endif;
321700101029
321800101029         // -?Se cliente Unificante presente con più codici Strategi,?
321900120725         //  ?occorre selezionare quello corretto?
322000120725         if  wCount_3EW < *zero;
322100120725           exsr  sr_Count3EW;
322200120725         endif;
322300101029         If  wCount_3EW > 1;
322400101029           $VideoPrec  = $Video;
322500101029           $Video      = 'W4';
322600101029           reset  $InzW04;
322700101029           ErrGenerico = *on;
322800101029           leavesr;
322900101029         EndIf;
323000100304
323100120305         // -?Abbandonare il "Supporto cliente a BRT" "ESWEB" è?
323200101216         //  ?impossibile se cliente padre con figli "ESWEB"?
323300101216         if  V2Ccli =  sch_SKC(1)   and   sch_SKC(2) > *zero      and
323400101216             V2Ccba <> c_EasyWeb    and   §3Ccba     = c_EasyWeb  and
323500101216             %lookup( *on : sch_SKEW : 2 ) > *zero;
323600100216           VIDmsg = $Msg(41);
323700100212           PosCurCBA   = *on;
323800100212           ErrMessage  = *on;
323900100212           ErrGenerico = *on;
324000100212           leavesr;
324100100212         endif;
324200100212
324300100211         // -?Reperimento chiave completa per tab. "3EW"?
324400100211         //  ?se cliente non trovato nel file TIVSS non può essere?
324500100304         //  ?  in tab. "3EW" - probabilmente è figlio (quindi OK)?
324600100215         //  ?(GIÀ FATTO NELLA SUBR. DI INIZIALIZZAZIONE "sr_InzD02")?
324700120725         //  ?=> PUÒ, PUÒ essere in tab. "3EW"...!?
324800120725         //  ?Potrebbe essere già stata fatta scadere l'abilitazione?
324900120725         //  ?ad EasySpedWEB (nel file TIVSS), lasciando "ESWEB" come?
325000120725         //  ?supporto qui in "3C": $Found_TIVSS sarebbe *off !!!?
325100120725         //  ?Ma i controlli qui successivi NON avrebbero senso.?
325200100312         if  NOT  $Found_TIVSS;
325300100215           leavesr;
325400100215         endif;
325500100312
325600120305         // -?Per abbandonare il "Supporto cliente a BRT" "ESWEB"?
325700100312         //  ?occorre che l'applicazione risulti scaduta?
325800120215         //  ?=> OCCORRERÀ ANNULLARE LA TAB. "3EW" A MANO !!!?
325900120215         //                                      ?(da *pgm TNTB79R)?
326000120725         //  ?=> TIVSSds.VSSdsc non può essere < *date:?
326100120725         //     ?sarebbe $Found_TIVSS = *off !!!?
326200120725         //if  V2Ccba        <> c_EasyWeb   and
326300120725         //    §3Ccba         = c_EasyWeb   and
326400120725         //    TIVSSds.VSSdsc < wDate;
326500120725         //  leavesr;
326600120725         //endif;
326700100407
326800120215         // -?Verifica esistenza del padre in tab. "3EW"?
326900120215         //  ?Occorre PRIMA eliminare l'abilitazione a STRATEGI per?
327000120215         //  ?  EasySped-WEB del cliente;?
327100120215         //  ?poi (con questo *pgm, richiamato da TIS725R1) il supporto?
327200120215         //  ?  cliente a BRT sarà modificabile!
327300120215         if  Not  $3EWupd                                  AND
327400120215             getTabella ( c_Tntbe : '3EW' : TIVSSds.VSSksu :
327500100407                          TIVSSds.VSSsun : *blank : *blank :
327600100407                          *omit : *omit :
327700100407                          *omit : *omit : *omit : *omit :
327800100407                          *omit : *omit : *omit : *omit :
327900120814                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni ) = *zero
328000120814             AND  ds_TNTBE.TBEatb = *blank;
328100120814           d3EW_Temp = ds_TNTBE.TBEuni;
328200120814           if  d3EW_Temp.§3EWFAA = *blank;
328300120814             VIDmsg = $Msg(32);
328400120814             PosCurCBA   = *on;
328500120814             ErrMessage  = *on;
328600120814             ErrGenerico = *on;
328700120814             leavesr;
328800120814           endif;
328900100407         endif;
329000100211
329100100211       ENDSR;
329200110311
329300110311       //--------------------------------------------------------------
329400110311       //?Verifica se range segnacolli del padre è diverso             ?
329500110311       //--------------------------------------------------------------
329600110311       BEGSR  sr_CtrlRngPadre;
329700110311
329800110311         F1Attivo = *off;
329900110311
330000110311         // -?Controlli da fare solo per cliente figlio?
330100110311         if  V2Ccli = V2Ccks;
330200110311           leavesr;
330300110311         endif;
330400110311
330500120305         // -?Controlli da fare solo per supporto cliente -> BRT?
330600110311         //  ?diverso da "ESWEB"?
330700110311         if  V2Ccba = c_EasyWEB;
330800110311           leavesr;
330900110311         endif;
331000110311
331100110311         // -?Padre "EasyWEB"?
331200110311         IF  p_§3Ccba = c_EasyWEB;
331300110311
331400110311           // -?Reperimento codice completo STRATEGI del cliente "PADRE"?
331500120725           //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
331600120725           if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
331700110311             clear  rqsSUN;
331800110311             exsr  sr_TIS7701_x_TIVSS;
331900110311           endif;
332000110311
332100110314           // -?Reperimento dati dal padre dalla tab. "3EW"?
332200110314           if  SavCliTab3CPEW <> %editc( V2Ccks : 'X' ) + '3EW';
332300110314             SavCliTab3CPEW = %editc( V2Ccks : 'X' ) + '3EW';
332400110311             if  getTabella ( c_Tntbe : '3EW' : TIVSSds.VSSksu :
332500110311                              TIVSSds.VSSsun : *blank : *blank :
332600110311                              *omit : *omit :
332700110311                              *omit : *omit : *omit : *omit :
332800110311                              *omit : *omit : *omit : *omit :
332900120605                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
333000120605                            = *zero;
333100110311               d3EW_Padre = ds_TNTBE.TBEuni;
333200110311             endif;
333300110311           endif;
333400110311
333500110311         // -?Padre NON "EasyWEB"?
333600110311         ELSE;
333700110311
333800110314           // -?Reperimento dati dal padre dalla tab. "3CP"?
333900110314           if  SavCliTab3CPEW <> %editc( V2Ccks : 'X' ) + '3CP';
334000110314             SavCliTab3CPEW = %editc( V2Ccks : 'X' ) + '3CP';
334100110311             k_TBEcod = '3CP';
334200110311             k_TBEke1 = %editc( V2Ccks : 'X' );
334300110311             setll  %kds( k05tntbe01 : 2 )  TNTBE000;
334400110311             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
334500110311             dow  Not %eof(TNTBE01L);
334600110311               if  %subst(TBEke2:1:2) = %editc(p_§3Cnrs:'X') and
334700110311                   TBEatb = *blank;
334800110311                 wNSI_Padre = %int( %subst( TBEke2 : 3 : 7 ) );
334900110311                 d3CP_Padre = TBEuni;
335000110311                 leave;
335100110311               endif;
335200110311               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
335300110311             enddo;
335400110311           endif;
335500110311
335600110311         ENDIF;
335700110311
335800110311
335900110311         // -?Abilitazione F1=Reperimento Serie e Segnacolli da Padre?
336000110421         if   p_§3Ccba   <> V2Ccba            OR
336100110421
336200110421              p_§3Cnrs   <> V2Cnrs            OR
336300110311
336400110421            ( p_§3Ccba   =  c_EasyWEB         and
336500110311             (d3EW_Padre.§3EWnsi <> V2Cncd    or
336600110311              d3EW_Padre.§3EWnsf <> V2Cnca) ) OR
336700110311
336800110421            ( p_§3Ccba   <> c_EasyWEB         and
336900110311             (wNSI_Padre <> V2Cncd            and
337000110311              wNSI_Padre <> *zero             and
337100110311              V2Cncd     <> 1)                or
337200110311             (d3CP_Padre.§3CPal  <> V2Cnca    and
337300110311              d3CP_Padre.§3CPal  <> *zero     and
337400110311              V2Cnca     <> *all'9') );
337500110311
337600110311           F1Attivo = *on;
337700110422           if  Not SavF1Attivo;
337800110422             ErrGenerico = *on;
337900110422           endif;
338000110311
338100110311         endif;
338200110311
338300110311       ENDSR;
338400120725
338500120725       //--------------------------------------------------------------
338600120725       //?Reperimento dati da tab. "3EW" del cliente e conteggio dei   ?
338700120725       //?record per lo stesso unificante.                             ?
338800120725       //--------------------------------------------------------------
338900120725       BEGSR  sr_Count3EW;
339000120725
339100120725         clear  d3EW;
339200120725         clear  wCount_3EW;
339300120725
339400120725         k_TBEcod = '3EW';
339500120725         k_TBEke1 = '0' + %editc( V2Ccks : 'X' );
339600120725         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
339700120725         reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
339800120725
339900120725         DoW  Not %eof(TNTBE01L);
340000120814
340100120814           d3EW_Temp = TBEuni;
340200120725
340300120814           if  (TBEatb = *blank   and   d3EW_Temp.§3EWfaa = *blank)   or
340400121119               TBLflg <> *blank   or
340500121119               (Not %found(TABEL00F)  and   d3EW_Temp.§3EWfaa <> *blank);
340600120725
340700120725             wCount_3EW += 1;
340800120725
340900120725             if  (TIVSSds.VSSsun <= *zero  and  wCount_3EW = 1)  or
341000120725                 TBEke2 = TIVSSds.VSSsun;
341100120725               d3EW = TBEuni;
341200120725             endif;
341300120725
341400120725           endif;
341500120725
341600120725           reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
341700120725
341800120725         EndDo;
341900120726
342000120726
342100120726         If  wCount_3EW = 1;
342200120726
342300120726           TNTB79ds.o3EWksu = TBEke1;
342400120726           TNTB79ds.o3EWsun = TBEke2;
342500120726           TNTB79ds.o3EWuni = d3EW;
342600120726           wCount_3EW = 1;
342700120726           rqsSUN = TNTB79ds.o3EWsun;
342800120726           exsr  sr_TIS7701_x_TIVSS;
342900120726           $Bypass_01 = *on;
343000120726
343100120726         EndIf;
343200120725
343300120725       ENDSR;
343400091124
343500091124       //--------------------------------------------------------------
343600091124       //?Controllo/Decodifica Codice Trattamento Merce.               ?
343700091124       //--------------------------------------------------------------
343800091124       BEGSR  sr_CtrlCTM;
343900091124
344000091124         clear  V2Dctm;
344100091124
344200091126         // ->?Controllo / Decodifica?
344300091125         ds_TNTBE.TBEke1 = V2Cctm;
344400091125         if  getTabella ( c_Tabel : '1B'  : ds_TNTBE.TBEke1 :
344500091125                          *omit   : *omit : *omit :
344600091125                          *omit : *omit :
344700091125                          *omit : *omit : *omit : *omit :
344800091125                          *omit : *omit : *omit : *omit :
344900091125                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
345000091125                        = *zero      AND
345100091125             ds_TNTBE.TBEatb = *blank;
345200091125           ds1B   = ds_TNTBE.TBEuni;
345300091125           V2Dctm = §1Bdes;
345400091125         else;
345500091125           V2Dctm = *all'? ';
345600091125           VIDmsg = $Msg(16);
345700091125           PosCurCTM   = *on;
345800091125           ErrMessage  = *on;
345900091125           ErrGenerico = *on;
346000091125           leavesr;
346100091125         endif;
346200091124
346300091124       ENDSR;
346400091104
346500091104       //--------------------------------------------------------------
346600091104       //?Gestione videata W01.                                        ?
346700091104       //--------------------------------------------------------------
346800091104       BEGSR  sr_GesW01;
346900091104
347000091126         // -?Inizializzazione window?
347100091104         if  $InzW01 = *on;
347200091127           clear  TB28W1;
347300091126           $InzW01 = *off;
347400091104         endif;
347500091104
347600091126         // -?Emissione window?
347700091104         if  Not $Protect;
347800091127           exfmt  TB28W1;
347900091104         else;
348000091127           write  TB28W1;
348100091104           exfmt  Protect;
348200091104         endif;
348300091104         reset  ErrMessage;
348400091104         reset  ErrGenerico;
348500091104         clear  VIDmsg;
348600091104
348700091126         // -?F12=Ritorno?
348800091104         if  dsp_aid = c_F12;
348900091104           $Video = $VideoPrec;
349000150319           clear  $VideoPrec;
349100091109           leavesr;
349200091104         endif;
349300091104
349400091126         // -?Invio?
349500140903         $CtrlRngSgnCl = *off;
349600091104         exsr  sr_CtrW01;
349700140718         if  $CtrlRngSgnCl;
349800140718           exsr  sr_CtrW01;
349900140718         endif;
350000091109         if  NOT ErrGenerico;
350100091109           // ->?Ritorno alla videata D02?
350200150319           $Video = 'D2';
350300150319           clear  $VideoPrec;
350400091109           leavesr;
350500091104         endif;
350600091104
350700091104       ENDSR;
350800091104
350900091104       //--------------------------------------------------------------
351000091104       //?Controllo dati immessi nella window W01.                     ?
351100091104       //--------------------------------------------------------------
351200091104       BEGSR  sr_CtrW01;
351300091104
351400091104         %subst(IndDspF : 81) = *off;
351500140903         //$CtrlRngSgnCl = *off;
351600091104
351700100407         select;
351800100407
351900100407           // -?Effettuare una selezione?
352000100407           when  W1Cncl = *zero   and   W1Cnrs = *zero;
352100100407             VIDmsg = $Msg(28);
352200100407             PosCurW1ncl = *on;
352300100407             ErrMessage  = *on;
352400100407             ErrGenerico = *on;
352500100407             leavesr;
352600100407
352700100407           // -?Selezionare Numero colli OPPURE Segnacollo da-al?
352800100407           when  W1Cncl <> *zero   and   W1Cnrs <> *zero;
352900100407             VIDmsg = $Msg(29);
353000100407             PosCurW1ncl = *on;
353100100407             ErrMessage  = *on;
353200100407             ErrGenerico = *on;
353300100407             leavesr;
353400100407
353500100407           // -?Controllo range segnacolli?
353600100407           when  W1Cnrs <> *zero   and   W1Cnca = *zero;
353700100407             VIDmsg = $Msg(30);
353800100407             PosCurW1nca = *on;
353900100407             ErrMessage  = *on;
354000100407             ErrGenerico = *on;
354100100407             leavesr;
354200100407           when  W1Cnrs <> *zero   and   W1Cncd > W1Cnca;
354300100407             VIDmsg = $Msg(31);
354400100407             PosCurW1ncd = *on;
354500100407             ErrMessage  = *on;
354600100407             ErrGenerico = *on;
354700100407             leavesr;
354800100407
354900100407         endsl;
355000100514
355100100514         // -?Sostituzione collo inziale del range con 1 se 0?
355200100514         if  W1Cnrs <> *zero   and   W1Cncd <= *zero;
355300100514           W1Cncd = 1;
355400100514           write  TB28W1;
355500100514           if  $Protect;
355600100514             write  Protect;
355700100514           endif;
355800100514         endif;
355900140123
356000140718         // -?Numero colli richiesto:?
356100140718         //  ?almeno 10.000 e comunque multiplo di 5.000?
356200140123         select;
356300140718           when  W1Cncl > *zero   and   W1Cncl < 10000;
356400140718             VIDmsg = %trimr($Msg(46)) +
356500140718                      ' DEVE prevedere almeno 10.000 colli';
356600140718             PosCurW1ncl = *on;
356700140718             ErrMessage  = *on;
356800140718             ErrGenerico = *on;
356900140718             leavesr;
357000140718           when  W1Cncl > 10000   and   %rem( W1Cncl : 5000 ) > *zero;
357100140123             VIDmsg = %trimr($Msg(46)) +
357200140718                      ' NON è multiplo di 5.000';
357300140123             PosCurW1ncl = *on;
357400140123             ErrMessage  = *on;
357500140123             ErrGenerico = *on;
357600140123             leavesr;
357700140317           when  W1Cncd > *zero;
357800140317       //  when  W1Cncd > *zero   and
357900140317       //        %rem( W1Cnca - W1Cncd + 1 : 1000 ) > *zero;
358000140903             // -?Controllo da NON fare SE inserito il range dall'utente?
358100140903             //  ?(su richiesta di Mirko)?
358200140903             if  $CtrlRngSgnCl   and
358300140903                 %rem( W1Cncd - 1 : 5000 ) > *zero;
358400140903               VIDmsg = %trimr($Msg(47));
358500140903               PosCurW1ncd = *on;
358600140903               ErrMessage  = *on;
358700140903               ErrGenerico = *on;
358800140903               leavesr;
358900140903             endif;
359000140317             if W1CncA = *hival;
359100140317               wCncA = W1CncA + 1;
359200140317             else;
359300140317               wCncA = W1CncA;
359400140317             endif;
359500140903             if  wCncA - W1CncD + 1 < 10000;
359600140903               VIDmsg = %trimr($Msg(46)) +
359700140903                        ' DEVE prevedere almeno 10.000 colli';
359800140903               PosCurW1nca = *on;
359900140903               ErrMessage  = *on;
360000140903               ErrGenerico = *on;
360100140903               leavesr;
360200140903             endif;
360300140917             // -?Controllo da NON fare SE inserito il range dall'utente?
360400140917             //  ?(su richiesta di Mirko)?
360500140917             if  $CtrlRngSgnCl   and
360600140917                 %rem( wCncA - W1CncD + 1 : 5000 ) > *zero;
360700140317               VIDmsg = %trimr($Msg(46)) +
360800140718                       ' NON è multiplo di 5.000';
360900140317               PosCurW1ncd = *on;
361000140317               ErrMessage  = *on;
361100140317               ErrGenerico = *on;
361200140317               leavesr;
361300140317             endif;
361400140123         endsl;
361500091104
361600091126         // -?Ricerca serie parziale?
361700091126         // ->?Selezione per numero colli?
361800091104         IF  W1Cncl > *zero;
361900091104
362000091104           clear  TNTB54ds;
362100100215           if  V2Dfls > *zero;
362200100215             I54fil = V2Dfls;
362300100215           else;
362400100215             I54fil = %subst( V1Ccli : 1 : %len(I54fil) );
362500100215           endif;
362600091104           I54tel = 'R';
362700091104           I54nrs = %editc( V2Cnrs : 'X' );
362800091104           I54tot = W1Cncl;
362900091104           kpjbu  = TNTB54ds;
363000110207           tntb54r ( kpjba : ubLeg3C_SKC );
363100091104           TNTB54ds = kpjbu;
363200091104
363300091104           select;
363400091126             // ->?errore SQL?
363500091104             when  O54err = 'S';
363600091125               VIDmsg = $Msg(18);
363700091104               PosCurW1nrs = *on;
363800091104               ErrMessage  = *on;
363900091104               ErrGenerico = *on;
364000091104               leavesr;
364100091126             // ->?serie già presente ma parziale: errore bloccante?
364200091104             when  O54err = '1';
364300091126               VIDmsg = $Msg(33);
364400091104               PosCurW1nrs = *on;
364500091104               ErrMessage  = *on;
364600091104               ErrGenerico = *on;
364700091104               leavesr;
364800120113             // ->?Serie NON ammessa (99)?
364900120113             when  O54err = '9';
365000120113               VIDmsg = $Msg(43);
365100120113               PosCurW1nrs = *on;
365200120113               ErrMessage  = *on;
365300120113               ErrGenerico = *on;
365400120113               leavesr;
365500091104           endsl;
365600091104
365700091104           If  O54dal <> *zero;
365800140718             if  SavW1Cnrs <> O54nrs  or
365900140718                 SavW1Cncd <> O54dal  or
366000140718                 SavW1Cnca <> O54al;
366100140718               $CtrlRngSgnCl = *on;
366200140718               SavW1Cnrs = O54nrs;
366300140718               SavW1Cncd = O54dal;
366400140718               SavW1Cnca = O54al;
366500140718               clear  W1Cncl;
366600140718               W1Cnrs = O54nrs;
366700140718               W1Cncd = O54dal;
366800140718               W1Cnca = O54al;
366900140718               leavesr;
367000140718             endif;
367100121128             V2Dfls = I54fil;
367200091106             V2Cnrs = O54nrs;
367300091104             V2Cncd = O54dal;
367400140718             V2Cnca = O54al;
367500091104           Else;
367600091126             // ->?SE non è stata trovata la disponibilità di serie?
367700091126             //   ?parziale => ricerca per serie completa?
367800091104             clear  TRTB28ds2;
367900100215             if  V2Dfls > *zero;
368000100215               ParFIL = %int(V2Dfls);
368100100215             else;
368200100215               ParFIL = V2Ccli / 10000;
368300100215             endif;
368400091104             ParELB = 'R';
368500111215             trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
368600120113             select;
368700120113               // ->?Serie NON ammessa (99)?
368800120113               when  ParERR = '9';
368900120113                 VIDmsg = $Msg(43);
369000120113                 PosCurW1nrs = *on;
369100120113                 ErrMessage  = *on;
369200120113                 ErrGenerico = *on;
369300120113                 leavesr;
369400091126               // -?Non trovata serie completa libera?
369500120113               when  ParERR <> *blank;
369600120113                 VIDmsg = $Msg(27);
369700120113                 PosCurW1nrs = *on;
369800120113                 ErrMessage  = *on;
369900120113                 ErrGenerico = *on;
370000120113                 leavesr;
370100120113               other;
370200121128                 V2Dfls = %editc( ParFIL : 'X' );
370300120113                 V2Cnrs = ParNRS;
370400120113                 V2Cncd = 1;
370500120113                 V2Cnca = W1Cncl;
370600120113             endsl;
370700091104           EndIf;
370800091104
370900091126         // ->?Selezione per segnacolli dal/al?
371000091104         ELSE;
371100091104
371200101014           exsr  sr_Ctrl_NRS_SCx;
371300170321           if  ErrGenerico = *on
371400170321           // oppure ho premuto F12 (dalla finestra di forzatura)
371500170321            or dsp_aid = c_F12;
371600101014             leavesr;
371700101014           endif;
371800091104
371900121128           V2Dfls = I54fil;
372000091104           V2Cnrs = W1Cnrs;
372100091104           V2Cncd = W1Cncd;
372200091104           V2Cnca = W1Cnca;
372300091104
372400091104         ENDIF;
372500091104
372600091104       ENDSR;
372700091104
372800091104       //--------------------------------------------------------------
372900091104       //?Gestione window W02.                                         ?
373000091104       //--------------------------------------------------------------
373100091104       BEGSR  sr_GesW02;
373200091104
373300091126         // -?Inizializzazione window?
373400091104         if  $InzW02 = *on;
373500091104           exsr  sr_InzW02;
373600091104           $InzW02 = *on;
373700091104         endif;
373800091104
373900091126         // -?Emissione window?
374000091127         exfmt  TB28W2;
374100091104
374200091126         // -?F12=Ritorno?
374300091104         if  dsp_aid = c_F12;
374400091106           $Video = $VideoPrec;
374500150319           clear  $VideoPrec;
374600091104           clear  V2F3CE;
374700091104         endif;
374800091104
374900091104       ENDSR;
375000091104
375100091104       //--------------------------------------------------------------
375200091104       //?Inizializzazione window W02.                                 ?
375300091104       //--------------------------------------------------------------
375400091104       BEGSR  sr_InzW02;
375500091104
375600091127         clear  TB28W2;
375700091104
375800091104         clear  d3CE;
375900091104
376000120913         //if  V2Cnrs = *zero;
376100120913         //  leavesr;
376200120913         //endif;
376300091104
376400091126         // -?Reperimento tab. "3CE" per cliente in esame?
376500091104         if  getTabella ( c_Tntbe : '3CE' : V1Ccli :
376600091104                          *blank: *omit : *omit :
376700091104                          *omit : *omit :
376800091104                          *omit : *omit : *omit : *omit :
376900091104                          *omit : *omit : *omit : *omit :
377000091104                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
377100091106                          = *zero      AND
377200091104               ds_TNTBE.TBEatb = *blank;
377300091109           d3CE = ds_TNTBE.TBEuni;
377400091104         endif;
377500091104
377600091126         // -?Impostazione campi a video?
377700091104         if  §3CEedat > *zeros;
377800091104           wDate_Eur = %date( §3CEedat : *iso );
377900091104           W2edat = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
378000091105                      ( %subdt(wDate_Eur : *months) * 10000 ) +
378100091105                      ( %subdt(wDate_Eur : *years) ) );
378200091104         endif;
378300091104         W2ever = §3CEever;
378400091104         W2cver = §3CEcver;
378500091104         if  §3CEcdsc > *zeros;
378600091104           wDate_Eur = %date( §3CEcdsc : *iso );
378700091104           W2cdsc = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
378800091104                      ( %subdt(wDate_Eur : *months) * 10000 ) +
378900091105                      ( %subdt(wDate_Eur : *years) ) );
379000091104         endif;
379100091104         W2sncd = §3CEsncd;
379200091104         W2snca = §3CEsnca;
379300091104         W2sncc = §3CEsncc;
379400091104         W2obj    = §3CEobj;
379500091104         W2corpo  = §3CEcorpo;
379600091104         W2typweb = §3CEtypweb;
379700091104         W2nrs3CE  = §3CEnrs;
379800091104         W2cvermax = §3CEcvermx;
379900091104         if  §3CEcddemx > *zeros;
380000091104           wDate_Eur = %date( §3CEcddemx : *iso );
380100091104           W2cddemax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
380200091104                         ( %subdt(wDate_Eur : *months) * 10000 ) +
380300091104                         ( %subdt(wDate_Eur : *years) ) );
380400091104         endif;
380500091104         if  §3CEcdscmx > *zeros;
380600091104           wDate_Eur = %date( §3CEcdscmx : *iso );
380700091104           W2cdscmax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
380800091104                         ( %subdt(wDate_Eur : *months) * 10000 ) +
380900091104                         ( %subdt(wDate_Eur : *years) ) );
381000091104         endif;
381100120316         W2esvsrv = §3CEesvsrv;
381200091104
381300091126         // -?Decodifica il Tipo Aggiornamento Web?
381400091104         select;
381500091104           when  §3CEtypweb = *blank;
381600100107           when  §3CEtypweb = '0 ';
381700100107             W2typwebD = 'Funzionalità NON attiva';
381800091104           when  §3CEtypweb = '1 ';
381900091104             W2typwebD = 'Funzionalità attiva';
382000100107           when  §3CEtypweb = '2 ';
382100100107             W2typwebD = 'Framework 1.0';
382200100107           when  §3CEtypweb = '4 ';
382300100107             W2typwebD = 'Framework 1.1';
382400100107           when  §3CEtypweb = '6 ';
382500100107             W2typwebD = 'Framework 1.1 & 1.0';
382600100107           when  §3CEtypweb = '8 ';
382700100107             W2typwebD = 'Framework 2.0';
382800100107           when  §3CEtypweb = '10';
382900100107             W2typwebD = 'Framework 2.0 & 1.0';
383000100107           when  §3CEtypweb = '12';
383100100107             W2typwebD = 'Framework 2.0 & 1.1';
383200091104           other;
383300091104             W2typwebD = *all'? ';
383400091104         endsl;
383500091104
383600091104       ENDSR;
383700091104
383800091104       //--------------------------------------------------------------
383900091104       //?Gestione window W03.                                         ?
384000091104       //--------------------------------------------------------------
384100091104       BEGSR  sr_GesW03;
384200091104
384300091126         // -?Inizializzazione window?
384400091106         exsr  sr_InzW03;
384500091104
384600091126         // -?Emissione window?
384700091104         if  (W3txt1 + W3txt2 + W3txt3) <> *blank;
384800091127           exfmt  TB28W3;
384900091104         endif;
385000091104
385100091126         // -?Enter=Continua?
385200091106         $Video = $VideoPrec;
385300150319         clear  $VideoPrec;
385400091104
385500091104       ENDSR;
385600091104
385700091104       //--------------------------------------------------------------
385800091104       //?Inizializzazione window W03.                                 ?
385900091104       //--------------------------------------------------------------
386000091104       BEGSR  sr_InzW03;
386100091104
386200091127         clear  TB28W3;
386300091104
386400091104         if  V2Cnot = *blank;
386500091104           leavesr;
386600091104         endif;
386700091104
386800091126         // -?Verifica esistenza tab. "KB4" per cliente in esame?
386900091104         $KB4 = ( getTabella (c_Tntbe : 'KB4' : V1Ccli :
387000091111                              *omit : *omit : *omit :
387100091104                              *omit : *omit :
387200091104                              *omit : *omit : *omit : *omit :
387300091104                              *omit : *omit : *omit : *omit :
387400091104                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni)
387500091104                              = *zero );
387600091104         select;
387700091104
387800091104           when  *inKE      and
387900091104                (NOT $KB4   or   ds_TNTBE.TBEatb <> *blank);
388000091104             W3txt2 = 'Abilitare il cliente in tab. "KB4".';
388100091104
388200091104           when  *inKF      and
388300091104                (NOT $KB4   or   ds_TNTBE.TBEatb <> *blank);
388400091104             W3txt2 = 'Abilitare il cliente in tab. "KB4".';
388500091104
388600091104           when  *inKQ  and
388700091104                ($KB4   and   ds_TNTBE.TBEatb = *blank);
388800091104             W3txt2 = 'Annullare il cliente in tab. "KB4".';
388900091104
389000091104         endsl;
389100091104
389200091104       ENDSR;
389300101027
389400101027       //--------------------------------------------------------------
389500101027       //?Gestione window W04.                                         ?
389600101027       //--------------------------------------------------------------
389700101027       BEGSR  sr_GesW04;
389800101027
389900101027         // -?Inizializzazione window?
390000101027         if  $InzW04 = *on;
390100101027           clear  TB28W4;
390200101027           W4Csun  = '?';
390300101027           $InzW04 = *off;
390400101027         endif;
390500101027
390600101027         // -?Emissione window?
390700101027         if  Not $Protect;
390800101027           exfmt  TB28W4;
390900101027         else;
391000101027           write  TB28W4;
391100101027           exfmt  Protect;
391200101027         endif;
391300101027         reset  ErrMessage;
391400101027         reset  ErrGenerico;
391500101027         clear  VIDmsg;
391600101027
391700101027         // -?F12=Ritorno?
391800101027         if  dsp_aid = c_F12;
391900110725           clear  W4Csun;
392000101027           $Video = $VideoPrec;
392100150319           clear  $VideoPrec;
392200101027           leavesr;
392300101027         endif;
392400101027
392500101027         // -?Invio?
392600101027         exsr  sr_CtrW04;
392700101027         if  NOT ErrGenerico;
392800101027           TIVSSds.VSSksu = '0' + %editc(V2Ccks : 'X');
392900101027           TIVSSds.VSSsun = W4Csun;
393000101027           // ->?Ritorno alla videata precedente?
393100101027           $Video = $VideoPrec;
393200150319           clear  $VideoPrec;
393300101027           leavesr;
393400101027         endif;
393500101027
393600101027       ENDSR;
393700101027
393800101027       //--------------------------------------------------------------
393900101027       //?Controllo dati immessi nella window W04.                     ?
394000101027       //--------------------------------------------------------------
394100101027       BEGSR  sr_CtrW04;
394200101027
394300101027         %subst(IndDspF : 81) = *off;
394400101027
394500101027         Select;
394600101027
394700101027           // -?Ricerca Strategi User Number?
394800101028           When  %scan('?' : W4Csun) > *zeros;
394900101028             clear  W4Csun;
395000101027             exsr  sr_F13D02;
395100101027             select;
395200101028               when  TNTB79ds.o3EWfxx <> *blank;
395300120726                 ErrGenerico = *on;
395400101028               when  TNTB79ds.o3EWerr = *on;
395500101028                 VIDmsg = TNTB79ds.o3EWmsg;
395600101028                 PosCurW4sun = *on;
395700101027                 ErrMessage  = *on;
395800101027                 ErrGenerico = *on;
395900120726                 leavesr;
396000101028               when  TNTB79ds.o3EWksu = '0' + %editc(V2Ccks : 'X');
396100101028                 W4Csun = TNTB79ds.o3EWsun;
396200101027             endsl;
396300101027
396400101027           // -?Effettuare una selezione?
396500101028           When  W4Csun = *blank  or  W4Csun = *zero;
396600101028             VIDmsg = $Msg(42);
396700101028             PosCurW4sun = *on;
396800101027             ErrMessage  = *on;
396900101027             ErrGenerico = *on;
397000101027             leavesr;
397100101027
397200101027           // -?Controllo Strategi User Number selezionato?
397300101027           Other;
397400101028             rqsSUN = W4Csun;
397500101028             exsr  sr_TIS7701_x_TIVSS;
397600101028             select;
397700101028               // -?Codice cliente Strategi errato?
397800101028               when  rpyEsito <> *zero  or  TIVSSds.VSSksu = *blank
397900101028                                        or  TIVSSds.VSSksu = *zero;
398000101028                 VIDmsg = $Msg(14);
398100101028                 PosCurW4sun = *on;
398200101028                 ErrMessage  = *on;
398300101028                 ErrGenerico = *on;
398400101028                 leavesr;
398500101028               // -?Abilitazione scaduta?
398600101028               when  TIVSSds.VSSdsc < wDate;
398700101028                 VIDmsg = $Msg(37);
398800101028                 PosCurW4sun = *on;
398900101028                 ErrMessage  = *on;
399000101028                 ErrGenerico = *on;
399100101028                 leavesr;
399200101028             endsl;
399300101027
399400101027         EndSl;
399500101027
399600101027       ENDSR;
399700111011
399800111011       //--------------------------------------------------------------
399900111011       //?Gestione window W05.                                         ?
400000111011       //--------------------------------------------------------------
400100111011       BEGSR  sr_GesW05;
400200111011
400300111011         // -?Inizializzazione window?
400400111011         if  $InzW05 = *on;
400500111021           write  TB28W5a;
400600111011           exsr  sr_InzW05;
400700111011           $InzW05 = *off;
400800111011         endif;
400900111011
401000111011         // -?Emissione window?
401100111021         exfmt  TB28W5b;
401200111011
401300111011         // -?F12=Ritorno?
401400111011         if  dsp_aid = c_F12;
401500111011           $Video = $VideoPrec;
401600150319           clear  $VideoPrec;
401700111011         endif;
401800111011
401900111011       ENDSR;
402000111011
402100111011       //--------------------------------------------------------------
402200111011       //?Inizializzazione window W05.                                 ?
402300111011       //--------------------------------------------------------------
402400111011       BEGSR  sr_InzW05;
402500111011
402600111021         clear  TB28W5b;
402700111011
402800111011         // -?Reperimento ultimo segnacollo usato?
402900111024         if  V2Dfls > *zero;
403000121121           pInFilSgncl = %int( V2Dfls );
403100111024         else;
403200121121           pInFilSgncl = V2Ccli / 10000;
403300111024         endif;
403400121121         if  sch_SKC(1) = *zero;
403500121121           sch_SKC(1) = V2Ccks;
403600121121         endif;
403700121121         if  ubLastNSC_Rtv ( V2Cnrs : pInFilSgncl :
403800111024                             ubLeg3C_SKC : *omit :
403900140225                             pOutMaxNSC : pOutMaxLNP :
404000140225                             pOutMaxAAS : pOutMaxMGS :
404100140225                             pOutMaxNSP :
404200140225                             pOutLastNSC : pOutLastLNP :
404300140225                             pOutLastAAS : pOutLastMGS :
404400140225                             pOutLastNSP ) < *zero;
404500111011           VIDmsg = 'ERRORE nel reperim. dell''ultimo segnacollo +
404600111011                     utilizzato. Verificare in stampa.';
404700111011           errMessage  = *on;
404800111011           errGenerico = *on;
404900111011           leavesr;
405000111011         endif;
405100111011
405200111011         // -?Impostazione campi a video?
405300121121         W5Cfls  = pInFilSgncl;
405400121121         W5Cnrs  = V2Cnrs;
405500121121         W5Ccks  = V2Ccks;
405600140225         W5CnscM = pOutMaxNSC;
405700140225         if  pOutMaxAAS > *zero  and  pOutMaxMGS > *zero;
405800140225           W5DgmasM  = (pOutMaxAAS * 10000) + pOutMaxMGS;
405900140225           wDate_Eur = %date( W5DgmasM : *iso );
406000140225           W5DgmasM  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
406100111018                         ( %subdt(wDate_Eur : *months) * 10000 ) +
406200111018                         ( %subdt(wDate_Eur : *years) ) );
406300111018         endif;
406400140225         W5CnscL = pOutLastNSC;
406500140225         if  pOutLastAAS > *zero  and  pOutLastMGS > *zero;
406600140225           W5DgmasL  = (pOutLastAAS * 10000) + pOutLastMGS;
406700140225           wDate_Eur = %date( W5DgmasL : *iso );
406800140225           W5DgmasL  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
406900140225                         ( %subdt(wDate_Eur : *months) * 10000 ) +
407000140225                         ( %subdt(wDate_Eur : *years) ) );
407100140225         endif;
407200111011
407300111011       ENDSR;
407400120517
407500120517       //--------------------------------------------------------------
407600120517       //?Reperimento famiglia completa dell'unificante "originale"    ?
407700120517       //--------------------------------------------------------------
407800120517       BEGSR  sr_ubLeg3C_Orig;
407900120517
408000120517         ubLeg3C_Rtv ( §3Ccks :
408100120517                       ubLeg3C_FlgPF :
408200120517                       ubLeg3C_Padre :
408300120517                       ubLeg3C_SKC :
408400120517                       ubLeg3C_SKEW );
408500120517
408600120517         sch_SKC_Orig = sch_SKC;
408700120517
408800120517       ENDSR;
408900120104
409000120104       //--------------------------------------------------------------
409100120104       //?Reperimento famiglia completa del cliente in esame           ?
409200120104       //--------------------------------------------------------------
409300120104       BEGSR  sr_ubLeg3C;
409400120104
409500120104         // -?Reperimento famiglia completa dalla tab. "3C"?
409600120517         //  ?(se modificato il padre)?
409700120104         if  V2Ccks <> ubLeg3C_Padre;
409800120104           ubLeg3C_Rtv ( V2Ccks :
409900120104                         ubLeg3C_FlgPF :
410000120104                         ubLeg3C_Padre :
410100120104                         ubLeg3C_skC :
410200120104                         ubLeg3C_skEW );
410300120104         endif;
410400120104
410500120104         // -?"Sistemazione" della stessa se modificato il padre?
410600120104         //  ?impostandolo a se stesso?
410700120517         //  ?N.B.: non essendo ancora stato premuto F6, il padre?
410800120517         //        ?reperito risulta ancora l'ultimo registrato...?
410900120104         if  ubLeg3C_FlgPF = 'F'  and  ubLeg3C_Padre <> V2Ccks
411000120104                                  and  V2Ccli = V2Ccks;
411100120104           ubLeg3C_FlgPF  = 'P';
411200120104           ubLeg3C_Padre  = V2Ccks;
411300120104           clear  ubLeg3C_skC;
411400120104           clear  ubLeg3C_skEW;
411500120104           sch_skC(1)  = V2Ccks;
411600120104           sch_skEW(1) = (§3Ccba = c_EasyWeb);
411700120104         endif;
411800120104
411900120104       ENDSR;
412000091105
412100091105       //--------------------------------------------------------------
412200091105       //?Aggiornam.rec. di tutti i file in tutti i sistemi informativi?
412300091105       //--------------------------------------------------------------
412400091105       BEGSR  sr_UpdateAll;
412500091105
412600091126         // -?Ciclo di elaborazione per ogni sistema informativo?
412700091105         For  w_SisInf = 1  To  %elem($Libr);
412800091105
412900091126           // -?Apertura degli archivi?
413000091105           if  w_SisInf > 1;
413100091105             exsr  sr_OpenFileTab;
413200091105           endif;
413300091105
413400091126           // -?Aggiornamento tab. "3C"?
413500091105           exsr  sr_UpdTABEL;
413600091105
413700100330           // -?Aggiornamento tab. "3CE", "3CP" e "3EW"?
413800091105           exsr  sr_UpdTNTBE;
413900091105
414000091105         EndFor;
414100100330
414200121119         // -?Aggiornamento flag §3EWUPD in tab. "3EW"?
414300110616         //  ?(i 2 sistemi informativi sono gestiti dal chiamato)?
414400130604         clear  wRtnEsito;
414500121119         Select;
414600121119
414700121119           // -?F6=Conferma (modifica/inserimento)?
414800121119           When  Not  $3EWupd   and   dsp_aid = c_F06;
414900121119             // ·?del vecchio unificante (se variato unificante)?
415000121119             if  %found(TABEL00F)   and  Old_§3Ccks <> V2Ccks
415100121119                                    and  Old_§3Ccba  = c_EasyWEB;
415200121119               wVSSksu = '0' + %editc( Old_§3Ccks : 'X' );
415300130604               //Ub§3EWupd_Upd ( wVSSksu : *omit : 'A' : *off );
415400130604               wRtnEsito = Ub§3EWupd_Upd ( wVSSksu : TIVSSds_Orig.VSSsun :
415500130604                                           'A' : *off );
415600130605               //if  wRtnEsito < *zero;
415700130605               //  exsr  sr_Send_eMail;
415800130605               //endif;
415900121119             endif;
416000121119             // ·?dell'unificante attuale?
416100121119             //  ?(messo, tolto o lasciato EasyWEB)?
416200121119             if  V2Ccba = c_EasyWEB  or  Old_§3Ccba = c_EasyWEB;
416300130604               wRtnEsito = Ub§3EWupd_Upd ( TIVSSds.VSSksu : TIVSSds.VSSsun :
416400130604                                           'A' : *off );
416500130605               //if  wRtnEsito < *zero;
416600130605               //  exsr  sr_Send_eMail;
416700130605               //endif;
416800121119             endif;
416900121119
417000121119           // -?F16=Annullamento?
417100121119           When  Not  $3EWupd   and   dsp_aid = c_F16   and
417200121119                 Old_§3Ccba = c_EasyWEB;
417300121119             wVSSksu = '0' + %editc( Old_§3Ccks : 'X' );
417400130604             wRtnEsito = Ub§3EWupd_Upd ( wVSSksu : TIVSSds_Orig.VSSsun :
417500130604                                         'A' : *off );
417600130605             //if  wRtnEsito < *zero;
417700130605             //  exsr  sr_Send_eMail;
417800130605             //endif;
417900120207
418000121119           // -?F5=Ripristino?
418100121119           When  Not  $3EWupd   and   dsp_aid = c_F05   and
418200121119                 V2Ccba = c_EasyWEB;
418300130604             wRtnEsito = Ub§3EWupd_Upd ( TIVSSds.VSSksu : TIVSSds.VSSsun :
418400130604                                         'A' : *off );
418500130605             //if  wRtnEsito < *zero;
418600130605             //  exsr  sr_Send_eMail;
418700130605             //endif;
418800121119         EndSl;
418900091105
419000091126         // -?Messaggio di avviso: allineare la tab. "KB4"!?
419100091109         if  dsp_aid = c_F05  or  dsp_aid = c_F06  or  dsp_aid = c_F16;
419200091106           $VideoPrec = $Video;
419300091105           $Video = 'W3';
419400091105           exsr  sr_GesW03;
419500091105         endif;
419600091105
419700091105       ENDSR;
419800091130
419900091130       //--------------------------------------------------------------
420000091130       //?Riempimento struttura dati ds3C.                             ?
420100091130       //--------------------------------------------------------------
420200091130       BEGSR  sr_RieDS3C;
420300091130
420400091130         clear  ds3C;
420500100212
420600100212         // -?Impostazione campi per NON "EasyWeb"?
420700100212         if  V2Ccba <> c_EasyWEB;
420800100518           //§3Cnrs = V2Cnrs;         ?(da memorizzare comunque)?
420900100518           if  V2Cnrs > *zero;
421000100212             §3Cfls = %int( V2Dfls );
421100100212           endif;
421200100212           §3Cctm = V2Cctm;
421300100212         endif;
421400100212
421500100212         // -?Impostazione altri campi (per tutti)?
421600100518         §3Cnrs = V2Cnrs;
421700091130         §3Cokd = V2Cokd;
421800091130         §3Crag = V2Crag;
421900091130         §3Clrm = V2Clrm;
422000091130         §3Calm = V2Calm;
422100091130         §3Cnot = V2Cnot;
422200091130         §3Cabc = V2Cabc;
422300091130         §3Cdkc = V2Cdkc;
422400091130         §3Cavx = V2Cavx;
422500091130         §3Cabd = V2Cabd;
422600091130         §3Caus = V2Caus;
422700091130         §3Carc = V2Carc;
422800091130         §3Cmvr = V2Cmvr;
422900091222         §3Cbc2 = V2Cbc2;
423000091130         §3Ccba = V2Ccba;
423100091130         §3Cbac = V2Cbac;
423200091130         §3Cfg1 = V2Cfg1;
423300160317         §3CCSAMB = V2Amb;
423400091130         §3Cfsp = V2Cfsp;
423500160317         §3CCSTFP = V2TFP;
423600091130         §3Ccks = V2Ccks;
423700091130         §3Cnwb = V2Cnwb;
423800091130         §3Cnwc = V2Cnwc;
423900091130         §3Crsb = V2Crsb;
424000091130         §3Cpes = V2Cpes;
424100091130
424200091130       ENDSR;
424300091102
424400091102       //--------------------------------------------------------------
424500091102       //?Aggiornamento record nel file TABEL00F.                      ?
424600091102       //--------------------------------------------------------------
424700091102       BEGSR  sr_UpdTABEL;
424800091106
424900091106         //? N.B.                                                      ?
425000091106         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
425100091106         //   trasmissione (vedi flag TBEFTR).                        ?
425200091106         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
425300091106         //   subito nello stesso file di entrambi i S.I. - in due    ?
425400091106         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
425500091103
425600091105         // -?Aggiornam. tab. "3C" = Invio dati -BOLLETTAZIONE-?   ?
425700091105
425800091126         // -?RI-aggancio record da aggiornare?
425900091126         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
426000091105         if  w_SisInf > 1;
426100121204           k_TBLkut = c_Kut;
426200121204           k_TBLcod = c_Tab;
426300091105           k_TBLkey = V1Ccli;
426400091105           chain  %kds(k03tabel00)  TABEL;
426500091105         endif;
426600091102
426700171020         // controllo se il rcd di TABEL precedente alla modifica aveva come
426800171020         // Supporto cliente VASWS e ora non ce l'ha più
426900171020         // in questo caso devo annullare il contatore
427000171020         if §3Ccba = 'VASWS' and V2CCBA <> 'VASWS';
427100171020           iVCPTpId = §3CCBA;
427200171020           iVCPId = %editc(V2CCKS:'X');
427300171020           ubvcp0r_ann(iVCPTpId:iVCPId:
427400171020                   oVCPEsito);
427500171020         endif;
427600171020
427700091102         select;
427800091102
427900091126           // -?F5 = Ripristino?
428000091105           when  dsp_aid = c_F05;
428100100407             if  Not %found(TABEL00F);
428200100407               clear  TABEL;
428300100407               TBLkut = c_Kut;
428400100407               TBLcod = c_Tab;
428500100407               TBLkey = k_TBLkey;
428600100407             endif;
428700100407             exsr  sr_RieDS3C;
428800100407             TBLuni = ds3C;
428900091130             select;
429000121128               //when  %found(TABEL00F)   and   TBLflg = '*';
429100121128               when  %found(TABEL00F);
429200091130                 clear  TBLflg;
429300091130                 clear  TBLftr;
429400091130                 clear  TBLdtr;
429500091130                 //____________
429600091130                 update  TABEL;
429700091130                 //¯¯¯¯¯¯¯¯¯¯¯¯
429800091130               // - Record appena cancellato fisicamente
429900091130               when  NOT %found(TABEL00F);
430000091130                 TBLftt = '1';
430100091130                 clear  TBLflt;
430200091130                 if  w_SisInf = 1;
430300091130                   TBLftr = 'T';
430400091130                 else;
430500091130                   TBLftr = 'R';
430600091130                 endif;
430700091130                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
430800091130                                        %len(wDate) - %len(TBLdtr) + 1 :
430900091130                                        %len(TBLdtr) ) );
431000091130                 //____________
431100091130                 write   TABEL;
431200091130                 //¯¯¯¯¯¯¯¯¯¯¯¯
431300091130             endsl;
431400091102
431500091126           // -?F6 = Aggiornamento / Inserimento?
431600091102           when  dsp_aid = c_F06;
431700091102             if  Not %found(TABEL00F);
431800091106               clear  TABEL;
431900091102               TBLkut = c_Kut;
432000091102               TBLcod = c_Tab;
432100091102               TBLkey = k_TBLkey;
432200091102             endif;
432300121128             clear  TBLflg;
432400091130             exsr  sr_RieDS3C;
432500091106             TBLuni = ds3C;
432600091102
432700091102             TBLftt = '1';
432800091105             clear  TBLflt;
432900091105             if  w_SisInf = 1;
433000091105               TBLftr = 'T';
433100091105             else;
433200091105               TBLftr = 'R';
433300091105             endif;
433400091111             TBLdtr = %int( %subst( %editc(wDate : 'X') :
433500091111                                    %len(wDate) - %len(TBLdtr) + 1 :
433600091111                                    %len(TBLdtr) ) );
433700091102
433800091102             if  %found(TABEL00F);
433900091102               //____________
434000091102               update  TABEL;
434100091102               //¯¯¯¯¯¯¯¯¯¯¯¯
434200091102             else;
434300091102               //____________
434400091102               write   TABEL;
434500091102               //¯¯¯¯¯¯¯¯¯¯¯¯
434600091102             endif;
434700091102
434800091126           // -?F16 = Annullamento?
434900091105           when  dsp_aid = c_F16;
435000091130             if  %found(TABEL00F)   and   TBLflg   = *blank;
435100120116               if  old_§3Ccba = c_EasyWEB;
435200120116                 clear  old_§3Cnrs;
435300120116                 TBLuni = ds3C_old;
435400120116               endif;
435500091106               TBLflg = '*';
435600091106               clear  TBLftr;
435700091106               clear  TBLdtr;
435800091106               //____________
435900091106               update  TABEL;
436000091106               //¯¯¯¯¯¯¯¯¯¯¯¯
436100091106             endif;
436200091102
436300091102         endsl;
436400091102
436500091102       ENDSR;
436600091103
436700091103       //--------------------------------------------------------------
436800091103       //?Aggiornamento altri records nel file TNTBE00F.               ?
436900091103       //--------------------------------------------------------------
437000091103       BEGSR  sr_UpdTNTBE;
437100091106
437200091106         //? N.B.                                                      ?
437300091106         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
437400091106         //   trasmissione (vedi flag TBEFTR).                        ?
437500091106         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
437600091106         //   subito nello stesso file di entrambi i S.I. - in due    ?
437700091106         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
437800091105
437900091103         // -?Aggiornam. tab. "3CP" = estensione per serie parziale?   ?
438000091103
438100091105         Select;
438200091105
438300091126           // -?F6 = Aggiornamento / Inserimento?
438400100215           When  dsp_aid = c_F06   and   V2Ccba <> c_EasyWEB;
438500100402             exsr  sr_Update3CP;
438600171018             // se il "Supporto Cliente a BRT" è VASWS (supporto usato per
438700171018             //  gestioni con WS tipo TELECOM)
438800170825             if V2Ccba = 'VASWS';
438900171018               // devo creare le serie parziali come contatori
439000170825               exsr  sr_UpdateVCP;
439100170825             endif;
439200100921           When  dsp_aid = c_F06   and   V2Ccba     =  c_EasyWEB
439300100921                                   and   Old_§3Ccba <> c_EasyWEB;
439400100215             exsr  sr_Annull3CP;
439500101026
439600101026           // -?F5 = Ripristino?
439700101026           When  dsp_aid = c_F05   and   V2Ccba <> c_EasyWEB;
439800101026             exsr  sr_Update3CP;
439900171019             // se il "Supporto Cliente a BRT" è VASWS (supporto usato per
440000171019             //  gestioni con WS tipo TELECOM)
440100171019             if V2Ccba = 'VASWS';
440200171019               //devo ripristinare il contatore
440300171019               exsr  sr_RiprisVCP;
440400171019             endif;
440500091105
440600091126           // -?F16 = Annullamento?
440700101026           When  dsp_aid = c_F16   and   $Found_3CP;
440800091105             exsr  sr_Annull3CP;
440900171019             // se il "Supporto Cliente a BRT" è VASWS (supporto usato per
441000171019             //  gestioni con WS tipo TELECOM)
441100171019             if V2Ccba = 'VASWS';
441200171019               //devo annullare il contatore
441300171019               exsr  sr_AnnullVCP;
441400171019             endif;
441500091105
441600091105         EndSl;
441700100215
441800100331         // -?Aggiorn. tab. "3EW" = dati assegnati alla postaz. EasyWeb?   ?
441900100215
442000101026         Select;
442100100215
442200100215           // -?F5 = Ripristino?
442300100331           //  ?(dati EasyWeb gestibili solo sul padre)?
442400120207           //  ?(il padre deve risultare ancora/nuovamente abilitato ad?
442500120207           //  ?EasyWEB - vedi TIVSSds)?
442600120207           When  dsp_aid = c_F05    and
442700120207                 V2Ccli  = V2Ccks   and   V2Ccba = c_EasyWEB    and
442800120207                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
442900120207                 TIVSSds.VSSsun > *zero;
443000120207             exsr  sr_Ripris3EW;
443100100215
443200100215           // -?F6 = Aggiornamento / Inserimento?
443300101026           //  ?(dati EasyWeb gestibili solo sul padre, MA DAL *PGM?
443400101027           //  ?TNTB79R - NON da questo!!!)?
443500101026           //When  dsp_aid = c_F06    and
443600101026           //      V2Ccli  = V2Ccks   and   V2Ccba = c_EasyWEB;
443700101026           //  exsr  sr_Update3EW;
443800101027
443900101027           // -?F6 = Aggiornamento?
444000121205           //  ?Abbandonato il "Supporto cliente a BRT" "ESWEB"?
444100121205           //  ?(essendo scaduta l'applicazione):?OCCORRE ANNULLARE?
444200121205           //  ?LA TAB. "3EW" (attenzione ai figli) !!!?
444300121205           //  (i controlli sono già stati eseguiti)?
444400121205           //  ?SE mantenuto lo stesso range segnacolli:?
444500130507           //    ?=> si annulla la tab. "3EW" (già riportata in "3CP")?
444600121205           //  ?SE cambiato il range segnacolli:?
444700121205           //    ?=> si "blocca" (annullam. applicativo) la tab. "3EW"?
444800130507           When  dsp_aid    = c_F06   and  V2Ccba    <> c_EasyWeb  and
444900130507                 Old_§3Ccks = V2Ccli  and  Old_§3Ccba = c_EasyWeb;
445000101027             select;
445100110725               // -?Un solo codice in TIVSS?
445200101027               when  wCount_3EW = 1;
445300120816                 if  V2Cnrs = d3EW.§3EWnrs  and
445400120816                     V2Cncd = d3EW.§3EWnsi  and
445500120816                     V2Cnca = d3EW.§3EWnsf;
445600120814                   exsr  sr_Annull3EW;
445700120814                 else;
445800120814                   exsr  sr_AnnApp3EW;
445900120814                 endif;
446000110725               // -?Un codice selezionato da TIVSS?
446100110725               when  W4Csun > *zero  and  W4Csun = TIVSSds.VSSsun;
446200120816                 if  V2Cnrs = d3EW.§3EWnrs  and
446300120816                     V2Cncd = d3EW.§3EWnsi  and
446400120816                     V2Cnca = d3EW.§3EWnsf;
446500120814                   exsr  sr_Annull3EW;
446600120814                 else;
446700120814                   exsr  sr_AnnApp3EW;
446800120814                 endif;
446900101027             endsl;
447000130507           //  ?Cambiato unificante - sempre EasyWeb?
447100130507           //  ?(se prima padre di se stesso e senza altri figli)?
447200130507           When  dsp_aid     = c_F06   and  V2Ccba     = c_EasyWeb  and
447300130507                 Old_§3Ccks <> V2Ccks  and  Old_§3Ccba = c_EasyWeb  and
447400130507                 Old_§3Ccks  = V2Ccli  and  sch_SKC_Orig(2) = *zero;
447500130507             select;
447600130507               // -?Un solo codice in TIVSS?
447700130507               when  wCount_3EW = 1;
447800130507                 exsr  sr_AnnApp3EW;
447900130507               // -?Un codice selezionato da TIVSS?
448000130507               when  W4Csun > *zero  and  W4Csun = TIVSSds.VSSsun;
448100130507                 exsr  sr_AnnApp3EW;
448200130507             endsl;
448300120816           //  ?Ritorno al "Supporto cliente a BRT" = "ESWEB"?
448400120816           //  ?(mantenendo lo stesso unificante)?
448500120816           When  dsp_aid = c_F06       and
448600120816                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
448700121205                 V2Ccli  = Old_§3Ccks  and  Old_§3Ccba <> c_EasyWeb  and
448800120816                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
448900120816                 TIVSSds.VSSsun > *zero;
449000120816             exsr  sr_Ripris3EW;
449100121119           When  dsp_aid = c_F06       and  Not %found(TABEL00F)     and
449200121119                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
449300121119                 d3EW_Temp.§3EWfaa <> *blank                    and
449400121119                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
449500121119                 TIVSSds.VSSsun > *zero;
449600121119             exsr  sr_Ripris3EW;
449700121205           // -?O passaggio a se stesso come unificante?
449800121205           //  ?(già abilitato ad EasySped-WEB)?
449900121205           When  dsp_aid = c_F06       and
450000121205                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
450100121205                 V2Ccli <> Old_§3Ccks  and
450200121205                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
450300121205                 TIVSSds.VSSsun > *zero;
450400121205             exsr  sr_Ripris3EW;
450500100331
450600100331           // -?F16 = Annullamento di un cliente senza dati in "3EW":?
450700100331           //  ?nessun record da annullare?
450800101026           //When  dsp_aid = c_F16      and  Not $Found_TIVSS   and
450900101026           //      Old.§3Ccks = V2Ccli  and  Old.§3Ccba = c_EasyWeb;
451000100215
451100100215           // -?F16 = Annullamento?
451200100215           //  ?(impossibile se codice padre)?
451300101027           //  (i controlli sono già stati eseguiti)?
451400101026           When  dsp_aid    = c_F16   and
451500101028                 Old_§3Ccks = V2Ccli  and  Old_§3Ccba = c_EasyWEB;
451600120814             //exsr  sr_Annull3EW;
451700120814             exsr  sr_AnnApp3EW;
451800100215
451900101026         EndSl;
452000091105
452100091103
452200091103         // -?Aggiornam. tab. "3CE" = estensione per versioni Easy-Sped?
452300091105
452400091126         // -?Rerimento record da aggiornare?
452500091103         k_TBEcod = '3CE';
452600091105         k_TBEke1 = V1Ccli;
452700091111         //clear  k_TBEke2;
452800091111         chain  %kds( k05TNTBE01 : 2 )  TNTBE000;
452900091105
453000091105         select;
453100091105
453200091126           // -?F5 = Ripristino?
453300091105           when  dsp_aid = c_F05;
453400091130             select;
453500091130               when  %found(TNTBE01L)   and   TBEatb  = 'A';
453600091130                 clear  TBEatb;
453700091130                 clear  TBEftr;
453800091130                 //_______________
453900091130                 update  TNTBE000;
454000091130                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
454100100331               // -?Record appena cancellato fisicamente?
454200100331               //  ?(NON scrive nulla: si aspetta il prossimo aggior-?
454300100331               //  ? namento dal file TIESV - vedi *pgm. TNTB67R).?
454400100331               when  NOT %found(TNTBE01L);
454500100331               //clear  TNTBE000;
454600100331               //TBEcod = k_TBEcod;
454700100331               //TBEke1 = k_TBEke1;
454800100331               //clear  d3CE;
454900100331               //TBEuni = d3CE;
455000100331               //TBEftt = 'S';
455100100331               //clear  TBEflt;
455200100331               //if  w_SisInf = 1;
455300100331               //  TBEftr = 'T';
455400100331               //else;
455500100331               //  TBEftr = 'R';
455600100331               //endif;
455700100331               //TBEdtr = wDate;
455800100331               ////_______________
455900100331               //write   TNTBE000;
456000100331               ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
456100091130             endsl;
456200091105
456300091126           // -?F6 = Aggiornamento / Inserimento?
456400100331           //  ?(NON scrive nulla: si aspetta il prossimo aggior-?
456500100331           //  ? namento dal file TIESV - vedi *pgm. TNTB67R).?
456600100331           when  dsp_aid = c_F06;
456700100331           //if  Not %found(TNTBE01L);
456800100331           //  clear  TNTBE000;
456900100331           //  TBEcod = k_TBEcod;
457000100331           //  TBEke1 = k_TBEke1;
457100100331           //endif;
457200100331           //clear  d3CE;
457300100331           //TBEuni = d3CE;
457400100331           //TBEftt = 'S';
457500100331           //clear  TBEflt;
457600100331           //if  w_SisInf = 1;
457700100331           //  TBEftr = 'T';
457800100331           //else;
457900100331           //  TBEftr = 'R';
458000100331           //endif;
458100100331           //TBEdtr = wDate;
458200100331           //
458300100331           //if  %found(TNTBE01L);
458400100331           //  //_______________
458500100331           //  update  TNTBE000;
458600100331           //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
458700100331           //else;
458800100331           //  //_______________
458900100331           //  write   TNTBE000;
459000100331           //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
459100100331           //endif;
459200091105
459300091126           // -?F16 = Annullamento?
459400091105           when  dsp_aid = c_F16;
459500091130             if  %found(TNTBE01L)   and   TBEatb <> 'A';
459600091105               TBEatb = 'A';
459700091105               clear  TBEftr;
459800091105               //_______________
459900091105               update  TNTBE000;
460000091105               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
460100091105             endif;
460200091106
460300091106         endsl;
460400091103
460500091103       ENDSR;
460600091105
460700091105       //--------------------------------------------------------------
460800091106       //?Aggiornamento / Inserimento dati in tab. "3CP"               ?
460900091106       //?(estensione per serie parziale)                              ?
461000091105       //--------------------------------------------------------------
461100100402       BEGSR  sr_Update3CP;
461200091102
461300091106         $Found_3CP = *off;
461400091106
461500091106         k_TBEcod = '3CP';
461600091106         k_TBEke1 = V1Ccli;
461700091111         //clear  k_TBEke2;
461800091106         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
461900091106         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
462000091106
462100091106         // -?Aggiornamento (o annullamento) serie parziale?
462200091106         DOW  Not %eof(TNTBE01L);
462300091106
462400091106           Select;
462500091106
462600091126             // -?Vecchia serie parziale nel file (già annullata)?
462700100212             //  ?con serie completa a video  o?
462800120305             //  ?con "Supporto cliente a BRT" = "ESWEB"?
462900091106             When  TBEatb = 'A'     and
463000120731                  (V2Cnrs = *zero    or
463100120731                  (V2Cncd = 1       and   V2Cnca  = *hival)  or
463200100212                   V2Ccba = c_EasyWEB);
463300100212
463400100212             // -?Vecchia serie parziale nel file (da annullare)?
463500120305             //  ?con "Supporto cliente a BRT" = "ESWEB"?
463600100212             When  V2Ccba = c_EasyWEB;
463700100212               TBEatb = 'A';
463800100212               clear  TBEftr;
463900100212               //_______________
464000100212               update  TNTBE000;
464100100212               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
464200091106
464300091126             // -?Vecchia serie parziale nel file (da annullare)?
464400091126             //  ?con serie completa a video?
464500120731             When  V2Cnrs = *zero    or
464600120731                  (V2Cncd = 1       and   V2Cnca  = *hival);
464700091106               TBEatb = 'A';
464800091106               clear  TBEftr;
464900091106               //_______________
465000091106               update  TNTBE000;
465100091106               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
465200091106
465300091126             // -?Aggiornamento serie parziale o annullamento vecchia?
465400091126             //  ?serie parziale SE variata TBEKE2: serie e/o numero?
465500091126             //  ?segnacollo iniziale?
465600120731             When  V2Cnrs > *zero   and
465700120731                  (V2Cncd > 1       or    V2Cnca <  *hival);
465800091106               d3CP = TBEuni;
465900091106
466000091106               IF  %editc(V2Cnrs : 'X') + %editc(V2Cncd : 'X') =
466100091106                   %subst(TBEke2 : 1 : %len(%editc(V2Cnrs : 'X'))) +
466200091106                   %subst(TBEke2 : %len(%editc(V2Cnrs : 'X')) + 1 :
466300091106                                   %len(%editc(V2Cncd : 'X')));
466400091106                 $Found_3CP = *on;
466500091106
466600091106                 If  TBEatb = 'A'   or   §3CPal <> V2Cnca;
466700091106
466800091126                 // -=»>?Aggiornamento vecchia serie parziale?
466900091106                   if  §3CPal <> V2Cnca;
467000091106                     §3CPal = V2Cnca;
467100091106                     TBEuni = d3CP;
467200091106                   endif;
467300091106                   if  TBEatb = 'A';
467400091106                     clear  TBEatb;
467500091106                     clear  TBEftr;
467600091106                   endif;
467700091106                   //_______________
467800091106                   update  TNTBE000;
467900091106                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
468000091106                 EndIf;
468100091106
468200091106               ELSE;
468300091106
468400091126                 // -=»>?Annullamento vecchia serie parziale con?
468500091126                 //     ?serie e/o numero segnacollo iniziale diversi?
468600091106                 if  TBEatb = *blank;
468700091106                   TBEatb = 'A';
468800091106                   clear  TBEftr;
468900091106                   //_______________
469000091106                   update  TNTBE000;
469100091106                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
469200091106                 endif;
469300091106
469400091106               ENDIF;
469500091106
469600091106           EndSl;
469700091106
469800091106           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
469900091106
470000091106         ENDDO;
470100091106
470200091106         // -?Nuova serie parziale a video?
470300091126         //  ?(inserimento  o  variazione serie e/o segnacollo iniziale)?
470400120731         IF  V2Cnrs > *zero   and
470500120731            (V2Cncd > 1       or    V2Cnca <  *hival)  and
470600091106             Not $Found_3CP;
470700091106
470800091126           // ->?Scrittura nuova serie?
470900091106           clear  TNTBE000;
471000091106           TBEapl = X3CPapl;
471100091106           TBEcod = %subst( X3CPke1 : %len(X3CPke1) - 3 + 1 : 3 );
471200091106           TBEke1 = V1Ccli;
471300091106           TBEke2 = %editc(V2Cnrs : 'X') + %editc(V2Cncd : 'X');
471400091106           clear  d3CP;
471500091106           §3CPal = V2Cnca;
471600091106           TBEuni = d3CP;
471700091106           TBEftt = 'S';
471800091106           if  w_SisInf = 1;
471900091106             TBEftr = 'T';
472000091106           else;
472100091106             TBEftr = 'R';
472200091106           endif;
472300091106           TBEdtr = wDate;
472400091106           //______________
472500091106           write  TNTBE000;
472600091106           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
472700091106
472800091106         ENDIF;
472900091105
473000091105       ENDSR;
473100170824
473200170824       //--------------------------------------------------------------
473300170824       //?Aggiornamento / Inserimento dati in num. "VCP"               ?
473400170824       //?se Supporto clienti a BRT = VASWS                            ?
473500170824       //--------------------------------------------------------------
473600170824       BEGSR  sr_UpdateVCP;
473700170824
473800170824         // controllo se esiste o meno il prog.
473900171019         iVCPTpId = V2CCBA;
474000171019         iVCPId = %editc(V2CCKS:'X');
474100171019         ubvcp0r_ctl(iVCPTpId:iVCPId:0:oVCPEsito);
474200170824
474300171019         // il prog. esiste, lo modifico (proc. _UPD)
474400170825         if oVCPEsito = 0;
474500171019           ubvcp0r_upd(iVCPTpId:iVCPId:0:
474600171019                     V2CNRS*10000000+V2CNCD:
474700171019                     V2CNRS*10000000+V2CNCA:
474800171019                     1:1000:oVCPEsito);
474900171019         else;
475000171019           // il prog. NON esiste per cui lo creo (proc. _CRT)
475100171019           ubvcp0r_crt(iVCPTpId:iVCPId:0:
475200171018                     V2CNRS*10000000+V2CNCD:
475300171018                     V2CNRS*10000000+V2CNCA:
475400171018                     1:1000:oVCPEsito);
475500171019         endif;
475600170824
475700170824       ENDSR;
475800171019
475900171019       //--------------------------------------------------------------
476000171019       //?Annullamento dati in num. "VCP"               ?
476100171019       //?se Supporto clienti a BRT = VASWS                            ?
476200171019       //--------------------------------------------------------------
476300171019       BEGSR  sr_AnnullVCP;
476400171019
476500171019         // NON controllo se esiste o meno il prog. perché lo fa la proc.
476600171019         iVCPTpId = V2CCBA;
476700171019         iVCPId = %editc(V2CCKS:'X');
476800171019         ubvcp0r_ann(iVCPTpId:iVCPId:
476900171019                   oVCPEsito);
477000171019
477100171019       ENDSR;
477200171019
477300171019       //--------------------------------------------------------------
477400171019       //?Ripristino dati in num. "VCP"               ?
477500171019       //?se Supporto clienti a BRT = VASWS                            ?
477600171019       //--------------------------------------------------------------
477700171019       BEGSR  sr_RiprisVCP;
477800171019
477900171019         // NON controllo se esiste o meno il prog. perché lo fa la proc.
478000171019         iVCPTpId = V2CCBA;
478100171019         iVCPId = %editc(V2CCKS:'X');
478200171019         ubvcp0r_rip(iVCPTpId:iVCPId:
478300171019                   oVCPEsito);
478400171019
478500171019       ENDSR;
478600091105
478700091105       //--------------------------------------------------------------
478800091105       //?Annullamento dati in tab. "3CP"                              ?
478900091106       //?(estensione per serie parziale)                              ?
479000091105       //--------------------------------------------------------------
479100091105       BEGSR  sr_Annull3CP;
479200091105
479300091106         // -?Annullamento serie parziale?
479400091106         k_TBEcod = '3CP';
479500091106         k_TBEke1 = V1Ccli;
479600091106         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
479700091106         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
479800091106
479900091106         DoW  Not %eof(TNTBE01L);
480000091106
480100091106           if  TBEatb <> 'A';
480200091106             TBEatb = 'A';
480300091106             clear  TBEftr;
480400091106             //_______________
480500091106             update  TNTBE000;
480600091106             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
480700091106           endif;
480800091106
480900091106           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
481000091106
481100091106         EndDo;
481200091105
481300091105       ENDSR;
481400100215
481500100215       //--------------------------------------------------------------
481600100215       //?Ripristino dati in tab. "3EW"                                ?
481700100215       //?(dati assegnati alla postazione "EasyWEB")                   ?
481800100215       //--------------------------------------------------------------
481900120207       BEGSR  sr_Ripris3EW;
482000120207
482100120207         // -?Ripristino dati assegnati alla postazione "EasyWEB"?
482200120207         //k_TBEcod = '3EW';
482300120207         k_TBEcod = %subst( X3EWke1 : %len(X3EWke1) - 3 + 1 : 3 );
482400120207         k_TBEke1 = TIVSSds.VSSksu;
482500120207         k_TBEke2 = TIVSSds.VSSsun;
482600120207         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
482700120816         if  %found(TNTBE01L)   and   TBEatb = *blank;
482800120816           d3EW_Temp = TBEuni;
482900120816         endif;
483000120207
483100120207         Select;
483200120207
483300120207           // -?Ripristino record?
483400120207           When  %found(TNTBE01L)   and   TBEatb = 'A';
483500120816             clear  d3EW.§3EWfaa;
483600120816             clear  d3EW.§3EWdaa;
483700121102             d3EW.§3EWuum = DUTute;
483800120816             TBEuni = d3EW;
483900120207             clear  TBEatb;
484000120207             clear  TBEftr;
484100120207             //_______________
484200120207             update  TNTBE000;
484300120207             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
484400120816
484500120816           // -?Ripristino applicativo?
484600120816           When  %found(TNTBE01L)   and   TBEatb = *blank  and
484700120816                 d3EW_Temp.§3EWfaa <> *blank;
484800120816             //clear  d3EW;   ?(la ds deve risultare già valorizzata)?
484900120816             //...            ?(la ds deve risultare già valorizzata)?
485000120816             clear  d3EW.§3EWfaa;
485100120816             clear  d3EW.§3EWdaa;
485200121102             d3EW.§3EWuum = DUTute;
485300120816             TBEuni = d3EW;
485400120816             TBEftt = 'S';
485500120816             if  w_SisInf = 1;
485600120816               TBEftr = 'T';
485700120816             else;
485800120816               TBEftr = 'R';
485900120816             endif;
486000120816             TBEdtr = wDate;
486100120816             //_______________
486200120816             update  TNTBE000;
486300120816             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
486400120207
486500120207           // -?Scrittura record (appena cancellato fisicamente)?
486600120207           When  Not %found(TNTBE01L);
486700120207             clear  TNTBE000;
486800120207             TBEapl = X3EWapl;
486900120207             TBEcod = k_TBEcod;
487000120207             TBEke1 = k_TBEke1;
487100120207             TBEke2 = k_TBEke2;
487200121102             //clear  d3EW;   ?(la ds deve risultare già valorizzata)?
487300121102             //...            ?(la ds deve risultare già valorizzata)?
487400120207             TBEuni = d3EW;
487500120207             TBEftt = 'S';
487600120207             if  w_SisInf = 1;
487700120207               TBEftr = 'T';
487800120207             else;
487900120207               TBEftr = 'R';
488000120207             endif;
488100120207             TBEdtr = wDate;
488200120207             //______________
488300120207             write  TNTBE000;
488400120207             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
488500120207
488600120207         EndSl;
488700120207
488800120207       ENDSR;
488900100322
489000100322       //--------------------------------------------------------------
489100100402       //?Aggiornamento dati segnacolli in tab. "3EW"                  ?
489200100322       //?(dati assegnati alla postazione "EasyWEB")                   ?
489300100322       //--------------------------------------------------------------
489400100517       //BEGSR  sr_Update3EW;
489500100401
489600100402         // -?Aggiornamento dati assegnati alla postazione "EasyWeb"?
489700100402         //  ?(segnacolli)?
489800100402         //  ?N.B. - Il flag §3EWUPD verrà aggiornato DOPO, con la?
489900100402         //         ?procedura UB§3EWUPD.?
490000100517       //k_TBEcod = '3EW';
490100100517       //k_TBEke1 = TIVSSds.VSSksu;
490200100517       //k_TBEke2 = TIVSSds.VSSsun;
490300100517       //chain  %kds( k05tntbe01 : 3 )  TNTBE000;
490400100517       //
490500100517       //if  %found(TNTBE01L);
490600100517       //
490700100517       //  d3EW    = TBEuni;
490800100517       //  if  V2Dfls > *zero;
490900100517       //    d3EW.§3EWfls = %int(V2Dfls);
491000100517       //  else;
491100100517       //    clear  d3EW.§3EWfls;
491200100517       //  endif;
491300100517       //  d3EW.§3EWnrs = V2Cnrs;
491400100517       //  d3EW.§3EWnsi = V2Cncd;
491500100517       //  d3EW.§3EWnsf = V2Cnca;
491600100517       //  d3EW.§3EWctm = V2Cctm;
491700100517       //  TBEuni = d3EW;
491800100517       //  TBEapl = X3EWapl;
491900100517       //  TBEftt = 'S';
492000100517       //  clear  TBEflt;
492100100517       //  if  w_SisInf = 1;
492200100517       //    TBEftr = 'T';
492300100517       //  else;
492400100517       //    TBEftr = 'R';
492500100517       //  endif;
492600100517       //  TBEdtr = wDate;
492700100517       //  //_______________
492800100517       //  update  TNTBE000;
492900100517       //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
493000100517       //
493100100517       //endif;
493200100401
493300100517       //ENDSR;
493400100215
493500100215       //--------------------------------------------------------------
493600100215       //?Annullamento dati in tab. "3EW"                              ?
493700100215       //?(dati assegnati alla postazione "EasyWEB")                   ?
493800100215       //--------------------------------------------------------------
493900101026       BEGSR  sr_Annull3EW;
494000100215
494100100215         // -?Annullamento dati assegnati alla postazione "EasyWEB"?
494200101026         k_TBEcod = '3EW';
494300120518         select;
494400120518           when  $Bypass_01  and  wCount_3EW = 1;
494500120726             //k_TBEke1 = TNTB79ds.i3EWksu;
494600120726             k_TBEke1 = TNTB79ds.o3EWksu;
494700120518             k_TBEke2 = TNTB79ds.o3EWsun;
494800120726           when  Old_§3Ccks = V2Ccks  and  TIVSSds.VSSksu > *zero
494900120726                                      and  TIVSSds.VSSsun > *zero;
495000120518             k_TBEke1 = TIVSSds.VSSksu;
495100120518             k_TBEke2 = TIVSSds.VSSsun;
495200120518           other;
495300120518             k_TBEke1 = TIVSSds_Orig.VSSksu;
495400120518             k_TBEke2 = TIVSSds_Orig.VSSsun;
495500120518         endsl;
495600101026         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
495700101026
495800101026         if  %found(TNTBE01L)   and   TBEatb <> 'A';
495900101026
496000121102           d3EW.§3EWuum = DUTute;
496100121102           TBEuni = d3EW;
496200101026           TBEatb = 'A';
496300101026           clear  TBEftr;
496400101026           //_______________
496500101026           update  TNTBE000;
496600101026           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
496700101026
496800101026         endif;
496900100215
497000101026       ENDSR;
497100120814
497200120814       //--------------------------------------------------------------
497300120814       //?Annullamento Applicativo EasySped-WEB in tab. "3EW"          ?
497400120814       //?(dati assegnati alla postazione "EasyWEB")                   ?
497500120814       //--------------------------------------------------------------
497600120814       BEGSR  sr_AnnApp3EW;
497700120814
497800120814         // -?Annullamento "applicativo" dei dati assegnati alla?
497900120814         //  ?postazione "EasyWEB"?
498000120814         k_TBEcod = '3EW';
498100120814         select;
498200120816           when  wCount_3EW = 1  and
498300120816                 ( $Bypass_01    or  (TNTB79ds.o3EWksu > *zero  and
498400120816                                      TNTB79ds.o3EWsun > *zero) );
498500120814             k_TBEke1 = TNTB79ds.o3EWksu;
498600120814             k_TBEke2 = TNTB79ds.o3EWsun;
498700120814           when  Old_§3Ccks = V2Ccks  and  TIVSSds.VSSksu > *zero
498800120814                                      and  TIVSSds.VSSsun > *zero;
498900120814             k_TBEke1 = TIVSSds.VSSksu;
499000120814             k_TBEke2 = TIVSSds.VSSsun;
499100120814           other;
499200120814             k_TBEke1 = TIVSSds_Orig.VSSksu;
499300120814             k_TBEke2 = TIVSSds_Orig.VSSsun;
499400120814         endsl;
499500120814         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
499600120814
499700120814         If  %found(TNTBE01L)   and   TBEatb <> 'A';
499800120814
499900120814           d3EW   = TBEuni;
500000120814
500100120814           if  d3EW.§3EWfaa = *blank;
500200120814
500300120814             d3EW.§3EWfaa = 'A';
500400120814             d3EW.§3EWdaa = %editc( wDate : 'X' );
500500121102             d3EW.§3EWuum = DUTute;
500600120814             TBEuni = d3EW;
500700120814             TBEapl = X3EWapl;
500800120814             TBEftt = 'S';
500900120814             clear  TBEflt;
501000120814             if  w_SisInf = 1;
501100120814               TBEftr = 'T';
501200120814             else;
501300120814               TBEftr = 'R';
501400120814             endif;
501500120814             TBEdtr = wDate;
501600120814             //_______________
501700120814             update  TNTBE000;
501800120814             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
501900120814
502000120814           endif;
502100120814
502200120814         EndIf;
502300120814
502400120814       ENDSR;
502500130604
502600130604       //--------------------------------------------------------------
502700130604       //?Invio e-mail con errore rilevato in fase di aggiornamento    ?
502800130604       //?flag D3EW.§3EWUPD in tab. "3EW" (vedi UB§3EWUPD).            ?
502900130604       //--------------------------------------------------------------
503000130605       //BEGSR  sr_Send_eMail;
503100130605       //
503200130605       //  Qcmd = 'SNDDST type(*lmsg) +
503300130605       //          tointnet((''stefano.merighi@brt.it'')) +
503400130605       //          dstd(''Errore in aggiornamento flag §3EWUPD'') +
503500130605       //          longmsg(''Errore ' + %trim( %editc( wRtnEsito : '1') ) +
503600130605       //                  ' ricevuto dal *pgm ' + %trimr(SDSpgm) + ')';
503700130605       //
503800130605       //  exsr  sr_ExecCmd;
503900130605       //
504000130605       //ENDSR;
504100130604
504200130604       //--------------------------------------------------------------
504300130604       //?Esecuzione del comando (già impostato).                      ?
504400130604       //--------------------------------------------------------------
504500130605       //BEGSR  sr_ExecCmd;
504600130605       //
504700130605       //  clear Qcap0100;
504800130605       //  Qcabcsdh = *off;
504900130605       //  Qcapa    = *off;
505000130605       //  Qcacmdss = *off;
505100130605       //  Qcaerved = *allX'00';
505200130605       //
505300130605       //  clear Qusec;
505400130605       //  Qusbprv  = %size(Qusec);
505500130605       //
505600130605       //  ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
505700130605       //                    %size(Qcap0100) : 'CPOP0100' : *omit :
505800130605       //                    0 : 0 : Qusec);
505900130605       //
506000130605       //  //if  Qusei <> *blank;
506100130605       //  //  ...;
506200130605       //  //endif;
506300130605       //
506400130605       //ENDSR;
506500091105
506600091102      /end-free
506700091102
506800091102       //--------------------------------------------------------------
506900091102       //?Schiere a tempo di compilazione.                             ?
507000091102       //--------------------------------------------------------------
507100091105
507200140718** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
507300091127SETRAS  GAITRAGRU FILTRAGRU
507400091105AS888   GAITRAGRPSFILTRAGRPF
507500140718** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
507600091124Filiale inesistente                                                            1
507700091124Campo obbligatorio                                                             2
507800091124Ammessi SOLO caratteri numerici                                                3
507900091124Cliente NON presente in anagrafica                                             4
508000130508Tabella inesistente  e  Cliente annullato o bloccato                           5
508100091124Codice cliente UNIFICANTE obbligatorio                                         6
508200091124Codice cliente UNIFICANTE errato                                               7
508300091124Cliente UNIFICANTE non presente in tab. "3C"                                   8
508400091215Codice cliente NON modificabile perché unificante con figli:                   9
508500120305Richiesto il "Supporto cliente a BRT" del cliente UNIFICANTE:                 --
508600120305"Supporto Cliente a BRT" obbligatorio                                         11
508700120305"Supporto Cliente a BRT" errato                                               12
508800120305Cliente UNIFICANTE con "Supporto Cliente a BRT" NON "                         13
508900121112Cliente UNIFICANTE non abilitato a STRATEGI per "EasySped-WEB"                14
509000120305SERIE obbligatoria per cliente con supporto a BRT                             15
509100091125Codice Trattamento Merce errato                                               16
509200091125SERIE obbligatoria per cliente che si assegna il num. spedizione o segnacollo 17
509300091125Errore istruzione SQL-Contattare il CED                                       18
509400091125SERIE già assegnata parzialmente al cliente                                   19
509500160125SERIE già assegnata completamente in tab.3CL                                  20
509600091125SERIE già assegnata al cliente                                                21
509700120305"Supporto BRT a Cliente" indicabile SOLO sul cliente unificante               22
509800091127"Accorpamento bolle" diverso sul cliente UNIFICANTE:                          23
509900140826Lunghezza Riferimento obbligatoria SE richiesto un suo allineamento           24
510000091125Campo NON impostabile SE NON richiesto l'accorpamento bolle                   25
510100091125Numero SERIE già presente: premere F10 per richiederne uno nuovo              26
510200120113Non ci sono più numeri di SERIE attribuibili tra 1 e 98                       27
510300091125Effettuare una selezione                                                      28
510400091125Numero Colli inseribile alternativamente alla Serie ed ai relativi Segnacolli 29
510500091125Numero segnacolli "AL" obbligatorio                                           30
510600091125Range segnacolli errato (DAL > AL)                                            31
510700121112Occorre far scadere l'abilitaz. a STRATEGI per "EasySped-WEB" dell'unificante 32
510800091126Segnacolli NON disponibili per la serie indicata                              33
510900120305"Supporto BRT a Cliente" obbligatorio per UNIFICANTE con SERIE                34
511000091215Cliente NON annullabile perché unificante con figli:                          35
511100121112Cliente già abilitato ad EasyWEB in tab. "3EW"; premere "Invio" per forzare   36
511200121112Abilitazione "EasySped-WEB" scaduta per il cliente UNIFICANTE                 37
511300100216Cliente UNIFICANTE non presente in tab. "3EW"                                 38
511400100216Sostituiti dati evidenziati con quelli dell'UNIFICANTE (già in tab. "3EW")    39
511500100216Cliente UNIFICANTE presente più volte in tab. "3EW" - Premere F13 per elenco  40
511600120305"Supporto Cliente a BRT" ESWEB NON modificabile su cliente UNIFICANTE         41
511700101028Codice cliente STRATEGI obbligatorio                                          42
511800120113SERIE non gestibile                                                           43
511900121128SERIE/RANGE segnacolli NON più attribuibili al cliente se si cambia supporto  44
512000121112SERIE / Range segnacolli NON più attribuibili: già attribuiti ad EasySped-WEB 45
512100121128Range segnacolli ANOMALO:                                                     46
512200140718Il segnacollo iniziale DEVE essere multiplo di 5.000 (+ 1)                    47
512300160317Ambito e terminal partenza vanno valorizzati entrambi o vuoti entrambi        48
512400160317Terminal partenza deve essere filiale di 1° livello                           49
