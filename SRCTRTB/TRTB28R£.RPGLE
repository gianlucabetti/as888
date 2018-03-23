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
002000091103     h dftactgrp(*no)
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
033600091102
033700091102       //--------------------------------------------------------------
033800091102       //?Definizione prototipi procedure.                             ?
033900091102       //--------------------------------------------------------------
034000091126
034100091126       // -?Reperimento dati utente?
034200110209     d TIBS34ds      e ds                  inz
034300091102      /copy gaitrasrc/srcProtoPR,TIBS34R
034400091102
034500110209       // -?Parametri per pgm interrogazione tabelle (TABEL)?
034600100308      /copy gaitrasrc/srcProtoPI,X§TABER
034700110209       // -?Interrogazione tabelle (TABEL)?
034800091102      /copy gaitrasrc/srcProtoPR,X§TABER
034900110209       // -?Parametri per Ricerca/Controllo tabelle (TNTBE)?
035000110209     d TIBS02ds      e ds                  inz
035100110209     d   T02mod      e                     inz('R')
035200110209     d   T02cod      e                     inz('3CS')
035300110209       // -?Ricerca/Controllo tabelle (TNTBE)?
035400091106      /copy gaitrasrc/srcProtoPR,TIBS02R
035500091126
035600091126       // -?Reperimento dati tabelle?
035700091104      /copy gaitrasrc/srcProtoPR,TRULTAB
035800091126
035900110209       // -?Parametri per pgm controllo/decodifica cliente?
036000110209      /copy gaitrasrc/srcProtoPI,TIBS69R
036100091126       // -?Controllo/Decodifica cliente?
036200091124      /copy gaitrasrc/srcProtoPR,TIBS69R
036300130520
036400130520       // -?Parametri per pgm ricerca data blocco cliente?
036500130520     d TRULBLCds     e ds                  inz
036600130520       // -?Ricerca data blocco cliente?
036700130520      /copy gaitrasrc/srcProtoPR,TRULBLCR
036800091102
036900100308       // -?Parametri per pgm interrogazione sottoconti?
037000100308      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
037100091126       // -?Interrogazione sottoconti?
037200091124      /copy gaitrasrc/srcProtoPR,XALFA3BR
037300100212
037400100308       // -?Parametri per verifica abilitazione STRATEGI?
037500100308      /copy gaitrasrc/srcProtoPI,TIS7701R
037600100308     d TIVSSds       e ds                  extname(TIVSS00F) qualified
037700100212       // -?Verifica abilitazione STRATEGI?
037800100212      /copy gaitrasrc/srcProtoPR,TIS7701R
037900100212
038000100308       // -?Parametri per reperimento legami in tab. "3C"?
038100100308      /copy gaitrasrc/srcProtoPI,UBLEG3C
038200100212       // -?Reperimento legami in tab. "3C"?
038300100212      /copy gaitrasrc/srcProtoPR,UBLEG3C
038400100326
038500100326       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
038600130604     d wRtnEsito       s             10i 0 inz
038700130605      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
038800091102
038900091126       // -?Gestione scambio dati Cons.Giac.Disp. con clienti?
039000091124      /copy gaitrasrc/srcProtoPR,TRTB40R
039100091102
039200091126       // -?Gestione scambio dati C/A e fattura con clienti?
039300091124      /copy gaitrasrc/srcProtoPR,TRTB44R
039400091126
039500091126       // -?Multimembro clienti VAB?
039600091124      /copy gaitrasrc/srcProtoPR,TRTB83R
039700091102
039800091126       // -?Gestione paticolarità DISK "C"?
039900091124      /copy gaitrasrc/srcProtoPR,TNTB10R
040000091102
040100091126       // -?Gestione serie per DISK "C" con prefisso "CC"?
040200091124      /copy gaitrasrc/srcProtoPR,TNTB13R
040300091102
040400091126       // -?Attribuzione numero serie parziale libero per filiale?
040500110209     d TNTB54ds      e ds                  inz
040600091124      /copy gaitrasrc/srcProtoPR,TNTB54R
040700091102
040800111215       // -?Attribuzione numero serie libero x filiale?
040900111215      /copy gaitrasrc/srcProtoPI,TRTB28R2
041000111215      /copy gaitrasrc/srcProtoPR,TRTB28R2
041100111215
041200111215       // -?Interrogazione tabella "3C" (oltre 9.999 record)?
041300111215     d trtb28r0        pr                  extpgm('TRTB28R0')
041400111215     d   kpjba                             likeds(KPJBA)
041500091214
041600091214       // -?Interrogazione legami tabelle VAS?
041700110209     d TRTB06ds      e ds                  inz
041800120605     d   d063C       e                     inz('S')
041900120605     d   d063K       e                     inz('S')
042000120605     d   d063R       e                     inz('S')
042100120605     d   d064C       e                     inz('S')
042200120605     d   d063Q       e                     inz('S')
042300120605     d   d06VAP      e                     inz('S')
042400120605     d   d06DSC      e                     inz('S')
042500120806     d   d06LAC      e                     inz('S')
042600120806     d   d06OSR      e                     inz('S')
042700091214     d trtb06r         pr                  extpgm('TRTB06R')
042800091214     d   kpjba                             likeds(KPJBA)
042900100211
043000100211       // -?Interrogazione clienti in tab. "3EW"?
043100110209     d tntb79ds      e ds                  qualified   inz
043200100211      /copy gaitrasrc/srcProtoPR,TNTB79R1
043300111011
043400111011       // -?Reperimento ultimo segnacollo utilizzato?
043500111011      /copy gaitrasrc/srcProtoPI,ubLastNSC
043600111011      /copy gaitrasrc/srcProtoPR,ubLastNSC
043700121128
043800121128       // -?"Stacca" figli dall'unificante in manutenzione?
043900121128     d TRTB28ds4     e ds                  inz
044000121128     d   oTB28err    e                     inz(*off)
044100121128     d trtb28r4        pr                  extpgm('TRTB28R4')
044200121128     d   kpjba                             likeds(KPJBA)
044300091105
044400091126       // -?Reperimento NETA sistema AS/400 corrente?
044500111011      /copy gaitrasrc/srcProtoPR,UBRTVNETA
044600130604
044700130605      *// // -?Parametri API QCAPCMD (Process Commands)?
044800130605     d*// Qcmd            s           2048    inz  varying
044900130605      *///copy qSysInc/qRpgleSrc,QCAPCMD
045000130605      *// // -?API QCAPCMD (Process Commands)?
045100130605      *///copy gaitrasrc/srcProtoPR,QCAPCMD
045200130605      *//
045300130605      *// // -?Parametri gestione errori API.?
045400130605      *///copy qsysinc/qrpglesrc,QUSEC
045500091102
045600091102       //--------------------------------------------------------------
045700091102       //?Definizione key-list.                                        ?
045800091102       //--------------------------------------------------------------
045900091102
046000091126       // -?File TABEL00F?
046100091102     d k03tabel00    e ds                  extname(TABEL00F : *key)
046200091102     d                                     prefix(k_)   inz
046300091102
046400091126       // -?File TNTBE01L?
046500091102     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
046600091102     d                                     prefix(k_)   inz
046700091102
046800091102       //--------------------------------------------------------------
046900091102       //?M A I N - L I N E                                            ?
047000091102       //--------------------------------------------------------------
047100091102
047200091102     c     *Entry        plist
047300091102     c                   parm                    KPJBA
047400091102
047500091102      /free
047600091102
047700091126       // -?Operazioni iniziali?
047800091126       exsr sr_RoutInz;
047900091102
048000091126       // -?Gestione videate?
048100091102       DoW  $Fine = *off;
048200091102
048300091102         select;
048400100308           // -?Gestione videata D1 (richiesta codice cliente)?
048500091102           when $Video = 'D1';
048600091102             exsr sr_GesD01;
048700100308           // -?Gestione videata D2 (manutenzione dati cliente)?
048800091102           when $Video = 'D2';
048900091102             exsr sr_GesD02;
049000100308           // -?Gestione videata D3 (manutenzione altri dati cliente)?
049100091102           when $Video = 'D3';
049200091102             exsr sr_GesD03;
049300100308           // -?Gestione videata W1 (ricerca serie segnacolli parziale)?
049400091104           when $Video = 'W1';
049500091104             exsr sr_GesW01;
049600120605           // -?Gestione videata W2 (dati tab. "3CE")?
049700091104           when $Video = 'W2';
049800091104             exsr sr_GesW02;
049900120605           // -?Gestione videata W3 (messaggo di avviso) - NON QUI?
050000120605           //when $Video = 'W3';
050100120605           //  exsr sr_GesW03;
050200101027           // -?Gestione videata W4 (richiesta cod. cliente Strategi)?
050300101027           when $Video = 'W4';
050400101027             exsr sr_GesW04;
050500111011           // -?Gestione videata W5 (ultimo segnacollo reperito)?
050600111011           when $Video = 'W5';
050700111011             exsr sr_GesW05;
050800100308           // -?? ? ? ? ??
050900091102           other;
051000091102             $Fine = *on;
051100091102         endsl;
051200091102
051300091102       enddo;
051400091102
051500091126       // -?Operazioni finali?
051600091102       exsr sr_RoutEnd;
051700091102
051800091102       //--------------------------------------------------------------
051900091102       //?Operazioni iniziali.                                         ?
052000091102       //--------------------------------------------------------------
052100091102       BEGSR  sr_RoutInz;
052200091102
052300091102         *inLR = *on;
052400091105
052500091126         // -?Verifica del sistema AS/400 corrente?
052600091105         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
052700091105           $Fine = *on;
052800091105           leavesr;
052900091105         endif;
053000091105
053100091126         // -?Impostazione elenco librerie in cui gestire le tabelle?
053200091126         //  ?(a seconda del sistema in cui si stà lavorando)?
053300091105         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
053400091105         if  w_iSystem = *zero;
053500091105           $Fine = *on;
053600091105           leavesr;
053700091105         endif;
053800091102
053900091126         // -?Reperimento dati job?
054000091102         exsr  sr_DatiJob;
054100091102
054200091126         // -?Impostazione nome programma a video?
054300091102         V1Tpgm = SDSpgm;
054400091102
054500091126         // -?Impostazione campi "fissi"?
054600091102         k_TBLkut = c_Kut;
054700091102         k_TBLcod = c_Tab;
054800091102
054900091126         // -?Verifica se *pgm richiamato:?
055000100308         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
055100100308         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
055200091102         $Called = (%subst( KPJBU : 1 : %len(V1Ccli) ) <> *blank);
055300100514         $3EWupd = ($Called  and
055400100514                    %subst( KPJBU : %len(V1Ccli) + 1 : 1 ) = 'S');
055500120807         $TrTb06 = ($Called  and
055600120807                    %subst( KPJBU : %len(V1Ccli) + 1 : %len(SDSpgm) )
055700120807                    = 'TRTB06R   ');
055800091102         if  $Called = *on   and
055900091106             %check( c_Digits : %subst( KPJBU : 1 :
056000091104                                       %len(V1Ccli) ) ) = *zero;
056100091104           W0Ccli = %subst( KPJBU : 1 : %len(V1Ccli) );
056200091102           //$InzD01 = *off;
056300091102           //exsr  sr_InzD01;
056400091102           //exsr  sr_CtrD01;
056500091102           //if  Not ErrGenerico;
056600091102           //  $Video = 'D2';
056700091102           //endif;
056800091102         endif;
056900091105
057000091126         // -?Reperimento data odierna?
057100091105         wDate = %int( %subst( %char( %dec( %timestamp() ) )
057200091105                               : 1 : 8 ) );
057300091106
057400091126         // -?Aggancio dati generali della tabella "3CP"?
057500091106         clear  xTNTBE_3CPds;
057600091106         X3CPke1 = *zero;
057700091106         %subst( X3CPke1 : %len(X3CPke1) - 3 + 1 : 3 ) = '3CP';
057800091106         getTabella ( c_Tntbe : *blank : X3CPke1 :
057900091106                      *blank  : *blank : *blank  :
058000091106                      *omit : *omit :
058100091106                      *omit : *omit : *omit : X3CPapl :
058200091106                      *omit : *omit : *omit : *omit :
058300091106                      *omit : X3CPuni );
058400100401
058500100401         // -?Aggancio dati generali della tabella "3EW"?
058600100401         clear  xTNTBE_3EWds;
058700100401         X3EWke1 = *zero;
058800101028         %subst( X3EWke1 : %len(X3EWke1) - 3 + 1 : 3 ) = '3EW';
058900100401         getTabella ( c_Tntbe : *blank : X3EWke1 :
059000100401                      *blank  : *blank : *blank  :
059100100401                      *omit : *omit :
059200100401                      *omit : *omit : *omit : X3EWapl :
059300100401                      *omit : *omit : *omit : *omit :
059400100401                      *omit : X3EWuni );
059500100211
059600091109         cloTabella( c_Tntbe );
059700091102
059800091102       ENDSR;
059900091102
060000091102       //--------------------------------------------------------------
060100091102       //?Reperimento Dati del job (Utente/Operativi).                 ?
060200091102       //--------------------------------------------------------------
060300091102       BEGSR  sr_DatiJob;
060400091102
060500091102         in(e) §AzUte;
060600091102         if NOT %error;
060700091102           in(e) §DatiUte;
060800091102         endif;
060900091102         if %error or RSut = *blank;
061000091102           tibs34r ( tibs34ds );
061100091102           in §AzUte;
061200091102           in §DatiUte;
061300091102         endif;
061400091102
061500091102       ENDSR;
061600091102
061700091102       //--------------------------------------------------------------
061800091102       //?Operazioni finali.                                           ?
061900091102       //--------------------------------------------------------------
062000091102       BEGSR  sr_RoutEnd;
062100091104
062200091104         cloTabella();
062300091102
062400110209         reset  TIBS69ds;
062500091102         tibs69r( tibs69ds :
062600091102                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
062700091125
062800091125         if  $TIS7701;
062900091125           Selettore_Record_TIVSS ( 'SETONLR' );
063000091125         endif;
063100100212
063200100212         ubLeg3C_Close ();
063300091102
063400091102         return;
063500091102
063600091102       ENDSR;
063700091102
063800091102       //--------------------------------------------------------------
063900091102       //?Gestione videata D01.                                        ?
064000091102       //--------------------------------------------------------------
064100091102       BEGSR  sr_GesD01;
064200091102
064300091126         // -?Inizializzazione videata?
064400091102         if  $InzD01 = *on;
064500091102           exsr  sr_InzD01;
064600091102           $InzD01 = *off;
064700091102         endif;
064800091102
064900091126         // -?SE ricevuto codice cliente NON si emette la prima videata?
065000091104         if  $Called   and   W0Ccli > *zero;
065100091102           exsr  sr_CtrD01;
065200091102           if  Not ErrGenerico;
065300091102             $Video  = 'D2';
065400091102             $InzD02 = *on;
065500091102             leavesr;
065600091102           else;
065700091104             clear W0Ccli;
065800091102           endif;
065900091102         endif;
066000091102
066100091126         // -?Emissione Testata?
066200091127         write  TB28T1;
066300091102
066400091126         // -?Emissione videata?
066500091127         exfmt  TB28D1;
066600091102
066700091104         clear  VIDmsg;
066800091102         reset  ErrMessage;
066900091102         reset  ErrGenerico;
067000091102
067100091102         SELECT;
067200091102
067300091126           // -?F3=Fine?
067400091102           WHEN  dsp_aid = c_F03;
067500091102             $Fine = *on;
067600091102
067700091126           // -?F17=Gestione serie per DISK "C" con prefisso "CC"?
067800091126           // -?F18=Gestione paticolarità DISK "C"?
067900091126           // -?F19=Multimembro clienti VAB?
068000091126           // -?F20=Gestione scambio dati principali?
068100091126           // -?F21=Gestione scambio dati Cons.Giac.Disp. con clienti?
068200091126           // -?F22=Gestione scambio dati C/A e fattura con clienti?
068300091102           WHEN  dsp_aid = c_F17   or   dsp_aid = c_F18   or
068400091102                 dsp_aid = c_F19   or   dsp_aid = c_F20   or
068500091102                 dsp_aid = c_F21   or   dsp_aid = c_F22;
068600091102             exsr  sr_FxxD01;
068700091102
068800091126           // -?Invio?
068900091102           OTHER;
069000091102             exsr sr_CtrD01;
069100091102             if  ErrGenerico = *off;
069200091102               $Video    = 'D2';
069300091102               $InzD02   = *on;
069400091102             endif;
069500091102
069600091102         ENDSL;
069700091102
069800091102       ENDSR;
069900091102
070000091102       //--------------------------------------------------------------
070100091102       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
070200091102       //--------------------------------------------------------------
070300091102       BEGSR  sr_InzD01;
070400091102
070500091102         clear  V1Topz;
070600091102         clear  BloAnn;
070700091120         VisBloAnn = *off;
070800091127         clear  TB28D1;
070900091102
071000091126         // -?Se è stato richiamato con un codice cliente numerico, si?
071100091126         //  ?imposta anche tale codice a video?
071200091102         select;
071300091104           when  $Called   and   W0Ccli > *zero;
071400091104             V1Ccli = W0Ccli;
071500091102           when  Not $Called;
071600091102             V1Ccli = '?';
071700091102         endsl;
071800091102
071900091126         // -?Abilitazione tasti funzionali?
072000091102         exsr  sr_AbilFxxD01;
072100091102
072200091102       ENDSR;
072300091102
072400091102       //--------------------------------------------------------------
072500091102       //?Abilitazione tasti funzionali in P01 (per D01).              ?
072600091102       //--------------------------------------------------------------
072700091102       BEGSR  sr_AbilFxxD01;
072800091102
072900091126         // -?Se è stato richiamato: nei primi 7 caratteri della KPJBU?
073000091126         //  ?c'è il codice cliente => NON devono essere abilitati i?
073100091126         //  ?tasti funzionali F17, F18, F19, F20, F21 e F22.?
073200091102
073300091126         // ->?F17 = Gestione serie per disk "C" con prefisso "CC"?
073400091102         F17Attivo = (Not $Called);
073500091102
073600091126         // ->?F18 = Gestione particolarità per disk "C"?
073700091102         F18Attivo = (Not $Called);
073800091102
073900091126         // ->?F19 = Multimembro clienti VAB?
074000091102         F19Attivo = (Not $Called);
074100091102
074200091126         // ->?F20 = Gestione scambio dati principali?
074300111207         //F20Attivo = (Not $Called);
074400111207         F20Attivo = *on;
074500091102
074600091126         // ->?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
074700091102         F21Attivo = (Not $Called);
074800091102
074900091126         // ->?F22 = Gestione scambio dati C/A e fattura con clienti?
075000091102         F22Attivo = (Not $Called);
075100091102
075200091102       ENDSR;
075300091102
075400091102       //--------------------------------------------------------------
075500091102       //?Gestione tasti funzionali F17,F18,F19,F20,F21 e F22 in D01.  ?
075600091102       //--------------------------------------------------------------
075700091102       BEGSR  sr_FxxD01;
075800091102
075900091126         // -?Il chiamato RICHIEDE un codice numerico per verificare?
076000091126         //  ?se è stato richiamato?
076100091102         if  V1Ccli = *blank;
076200091102           KPJBU = *zero;
076300091102         else;
076400091102           KPJBU = V1Ccli;
076500091102         endif;
076600091102
076700091102         select;
076800091102
076900091126           // -?F17=Gestione serie per DISK "C" con prefisso "CC"?
077000091102           when  dsp_aid = c_F17;
077100091102             tntb13r ( kpjba );
077200091102
077300091126           // -?F18=Gestione paticolarità DISK "C"?
077400091102           when  dsp_aid = c_F18;
077500091102             tntb10r ( kpjba );
077600091102
077700091126           // -?F19=Multimembro clienti VAB?
077800091102           when  dsp_aid = c_F19;
077900091102             trtb83r ( kpjba );
078000091102
078100091126           // -?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
078200091102           when  dsp_aid = c_F21;
078300091102             trtb40r ( kpjba );
078400091102
078500091126           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
078600091102           when  dsp_aid = c_F22;
078700091102             trtb44r ( kpjba );
078800091102
078900091102         endsl;
079000091102
079100091102       ENDSR;
079200091102
079300091102       //--------------------------------------------------------------
079400091102       //?Controllo dati immessi nella videata D01.                    ?
079500091102       //--------------------------------------------------------------
079600091102       BEGSR  sr_CtrD01;
079700091102
079800091102         %subst(IndDspF : 50) = *off;
079900091103         reset  $Protect;
080000091102
080100091126         // -?Filiale di ricerca?
080200091102         if  V1Cfil > *zero;
080300091102           chain  (V1Cfil)  AZORG;
080400091102           if  NOT %found(AZORG01L);
080500091104             VIDmsg = $Msg(01);
080600091102             PosCurFIL   = *on;
080700091102             ErrMessage  = *on;
080800091102             ErrGenerico = *on;
080900091102             leavesr;
081000091102           endif;
081100091102         endif;
081200091102
081300091102         SELECT;
081400091102
081500091126           // -?Ricerca codice cliente?
081600091102           WHEN  %scan('?' : V1Ccli) > *zeros;
081700110211             //X§Tkut = c_Kut;
081800110211             //X§Tcod = c_Tab;
081900110211             //if  V1Cfil > *zero;
082000110211             //  X§Tkey = %editc( V1Cfil : 'X' );
082100110211             //else;
082200110211             //  clear  X§Tkey;
082300110211             //endif;
082400110211             //clear  X§Tdes;
082500110211             //x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
082600110211             //V1Ccli = %subst(X§Tkey : 1 : %len(V1Ccli));
082700110211             // -?Superato il numero limite di record caricabili nel?
082800110211             //  ?subfile a video!?
082900110211             kpjbu = %editc( V1Cfil : 'X' );
083000110211             trtb28r0 ( kpjba );
083100110211             V1Ccli = kpjbu;
083200091102             ErrGenerico = *on;
083300091102             leavesr;
083400091102
083500091126           // -?Ricerca alfabetica?
083600091102           WHEN  V1Dcli <> *blank   and
083700091102                (V1Ccli =  *blank   or   V1Ccli = *zero);
083800091102             xParDut = RSut;
083900091102             xParKut = 1;
084000091102             xParRag = V1Dcli;
084100091102             xParKcc = DUTkci;
084200091102             xParSta = *zero;
084300091102             xParFlr = %editc( V1Cfil : 'X');
084400091102             clear  xParDit;
084500091102             xParNum = 1;
084600091102             clear  xParKcm;
084700091102             clear  xParKsm;
084800091102             clear  xParKdm;
084900091102             clear  xParEsci;
085000091102             clear  xParErr;
085100091102             clear  xParIva;
085200091102             clear  xParCdf;
085300091102             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
085400091102                        xParSta : xParFlr : xParDit : xParNum :
085500091102                        xParKcm : xParKsm : xParKdm : xParEsci :
085600091102                        xParErr : xParIva : xParCdf );
085700091102             if  xParSta <> -1   and   xParErr = *blank;
085800091102               V1Ccli = xParKsm;
085900091102               V1Dcli = xParRag;
086000091102             endif;
086100091102             ErrGenerico = *on;
086200091102             leavesr;
086300091102
086400091126           // -?Controllo immissione codice cliente?
086500091102           WHEN  V1Ccli = *blank  or  V1Ccli  = *zero;
086600091104             VIDmsg = $Msg(02);
086700091102             PosCurCLI   = *on;
086800091102             ErrMessage  = *on;
086900091102             ErrGenerico = *on;
087000091102             leavesr;
087100091102
087200091126           // -?Controllo immisione solo caratteri numerici?
087300091106           WHEN  %check( c_Digits : V1Ccli ) > *zero;
087400091104             VIDmsg = $Msg(03);
087500091102             PosCurCLI   = *on;
087600091102             ErrMessage  = *on;
087700091102             ErrGenerico = *on;
087800091102             leavesr;
087900091102
088000091102         ENDSL;
088100091102
088200091126         // -?Controllo / decodifica cliente?
088300091102         clear  TIBS69ds;
088400091104         I69sif = knsif;
088500091102         I69kcc = DUTkci;
088600091102         I69kac = %int(V1Ccli);
088700091102         tibs69r( tibs69ds :
088800091102                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
088900091102         if  O69err = *on;
089000091104           VIDmsg = $Msg(04);
089100091102           PosCurCLI   = *on;
089200091102           ErrMessage  = *on;
089300091102           ErrGenerico = *on;
089400091102           leavesr;
089500091102         endif;
089600091102         V1Dcli = %subst( ACOrag : 1 : %len(V1Dcli) );
089700091103
089800091126         // -?Apertura file TABEL00F del 1° S.I. (sede)?
089900091125         if  w_SisInf <> 1   or   Not %open(TABEL00F);
090000091125           w_SisInf = 1;
090100091125           exsr  sr_OpenFileTab;
090200091125         endif;
090300091102
090400091126         // -?Verifica esistenza del record in TABEL?
090500091102         k_TBLkey = V1Ccli;
090600091102
090700091103         chain  %kds(k03tabel00)  TABEL;
090800091102
090900091102         select;
091000091102
091100100312           // -?Cliente annullato?
091200091103           when  ACOflg <> *blank   and   NOT %found(TABEL00F);
091300091104             VIDmsg = $Msg(05);
091400091103             PosCurCLI   = *on;
091500091103             ErrMessage  = *on;
091600091103             ErrGenerico = *on;
091700091103             leavesr;
091800091102
091900100312           // -?Cliente bloccato?
092000130315           when  ACOabl <> *blanks  and   NOT %found(TABEL00F);
092100091104             VIDmsg = $Msg(05);
092200091103             PosCurCLI   = *on;
092300091103             ErrMessage  = *on;
092400091103             ErrGenerico = *on;
092500091103             leavesr;
092600091102
092700091102         endsl;
092800091102
092900091102       ENDSR;
093000091103
093100091103       //--------------------------------------------------------------
093200091103       //?Apertura dei files tabelle nel sistema informativo impostato.?
093300091103       //--------------------------------------------------------------
093400091103       BEGSR  sr_OpenFileTab;
093500091103
093600091126         // -?Chiusura (eventuale) archivi?
093700091103         if  %open(TABEL00F);
093800091103           close  TABEL00F;
093900091103         endif;
094000091103         if  %open(TNTBE01L);
094100091103           close  TNTBE01L;
094200091103         endif;
094300091109         cloTabella();
094400091103
094500091126         // -?Apertura archivi?
094600100212         ds_Libr  = $Libraries(w_iSystem);
094700091109         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
094800091103         open  TABEL00F;
094900091109         wLibFile2 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
095000091103         open  TNTBE01L;
095100091106
095200091106       ENDSR;
095300091102
095400091102       //--------------------------------------------------------------
095500091102       //?Gestione videata D02.                                        ?
095600091102       //--------------------------------------------------------------
095700091102       BEGSR  sr_GesD02;
095800091102
095900091126         // -?Inizializzazione videata?
096000091102         if  $InzD02 = *on;
096100091102           exsr  sr_InzD02;
096200091102           $InzD02 = *off;
096300091102         endif;
096400091125
096500091126         // -?Impostazione attributi di visualizzazione?
096600091125         exsr  sr_AttrVisD02;
096700091120
096800091126         // -?(Ri)Abilitazione tasti funzionali?
096900091120         exsr  sr_AbilFxxD02;
097000091102
097100091126         // -?Emissione Testata e Piede?
097200091127         write  TB28T1;
097300091127         write  TB28P2;
097400091102
097500091126         // -?Emissione videata?
097600120726         select;
097700120726           when  $Video <> 'D2';
097800120726             clear  ErrMessage;
097900120726             write  TB28D2;
098000120726             leavesr;
098100120726           when  Not $Protect;
098200120726             exfmt  TB28D2;
098300120726           other;
098400120726             write  TB28D2;
098500120726             exfmt  Protect;
098600120726         endsl;
098700091120
098800120202         clear  VIDmsg;
098900091102         reset  ErrMessage;
099000091102         reset  ErrGenerico;
099100091120
099200091102         SELECT;
099300110311
099400110311           // -?F1=Ricezione Serie e Range segnacolli dal padre?
099500120305           //     ?& pure il Supporto Cliente=>BRT?
099600120601           //     ?& pure il Codice Trattamento Merce?
099700110311           WHEN  dsp_aid = c_F01;
099800120601             exsr  sr_F01D02;
099900121128
100000121128           // -?F2="Stacca" figli?
100100121128           WHEN  dsp_aid = c_F02;
100200121128             exsr  sr_F02D02;
100300091102
100400091126           // -?F3=Fine?
100500091102           WHEN  dsp_aid = c_F03;
100600091102             unlock  TABEL00F;
100700091103             unlock  TNTBE01L;
100800091102             $Fine = *on;
100900091214
101000111011           // -?F8=Interrogazione legami tabelle VAS?
101100091214           WHEN  dsp_aid = c_F08;
101200091214             exsr  sr_F08D02;
101300120517
101400120517           // -?F9=Aggiornamento legami tabella "3C"?
101500120517           //  ?(del padre "originale")?
101600120517           WHEN  dsp_aid = c_F09;
101700120517             exsr  sr_ubLeg3C_Orig;
101800091104
101900091126           // -?F10=Ricerca Serie (globale)?
102000091109           WHEN  dsp_aid = c_F10;
102100091109             exsr  sr_F10D02;
102200091104
102300091126           // -?F11=Ricerca Serie Parziale?
102400091109           WHEN  dsp_aid = c_F11;
102500091109             $VideoPrec = $Video;
102600091109             $Video     = 'W1';
102700091109             reset  $InzW01;
102800091102
102900091126           // -?F12=Ritorno?
103000091102           WHEN  dsp_aid = c_F12;
103100091102             unlock  TABEL00F;
103200091103             unlock  TNTBE01L;
103300091125             clear  V1Dcli;
103400091102             $Video = 'D1';
103500100211
103600100211           // -?F13=Interrog. tab. "3EW"?
103700100211           WHEN  dsp_aid = c_F13;
103800100211             exsr  sr_F13D02;
103900111025
104000111025           // -?F14=Visualizzazione ultimo segnacollo?
104100111025           WHEN  dsp_aid = c_F14;
104200111025             $VideoPrec = $Video;
104300111025             $Video     = 'W5';
104400111025             reset  $InzW05;
104500091102
104600091126           // -?F19=Gestione multimembro clienti VAB?
104700091126           // -?F20=Gestione scambio dati principali     con clienti?
104800091126           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
104900091126           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
105000091104           WHEN  dsp_aid = c_F19   or
105100091104                 dsp_aid = c_F20   or
105200091102                 dsp_aid = c_F21   or
105300091102                 dsp_aid = c_F22;
105400091102             exsr  sr_FxxD02;
105500091105
105600091126           // -?RollUp=Videata successiva?
105700100331           WHEN  dsp_aid = c_RollUp   and   V2Cabd <> *blank;
105800091106             $VideoPrec  = $Video;
105900091105             $Video      = 'D3';
106000091104
106100091126           // -?"?" x interrogazione dati tab. "3CP"?
106200091104           WHEN  V2F3CE = '?';
106300091106             $VideoPrec = $Video;
106400091104             $Video     = 'W2';
106500091104             reset  $InzW02;
106600091102
106700091126           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
106800091102           OTHER;
106900100312             // -?Controlli eseguiti NON se F16=Annullamento?
107000091102             if  dsp_aid <> c_F16;
107100091102               exsr sr_CtrD02;
107200091106               if  Not ErrGenerico;
107300091106                 exsr sr_CtrD03;
107400091106                 if  ErrGenerico = *on;
107500091106                   $Video = 'D3';
107600091106                 endif;
107700091106               endif;
107800100312             // -?Controlli eseguiti SOLO se F16=Annullamento?
107900091214             else;
108000100312               exsr sr_CtrlANN;
108100091102             endif;
108200091214             if  ErrGenerico = *on;
108300091214               leavesr;
108400091214             endif;
108500091120             select;
108600100331               // -?Enter/F5/F6 => Videata successiva?
108700100331               //  ?               (se accorpamento bolle)?
108800100331               when  (dsp_aid = c_Enter   or   dsp_aid = c_F05   or
108900100331                      dsp_aid = c_F06)   and   V2Cabd  <> *blank;
109000100331                 $VideoPrec = $Video;
109100100331                 $Video = 'D3';
109200100312               // -?Aggiornamento?
109300091120               when  dsp_aid = c_F05   or
109400091120                     dsp_aid = c_F06   or
109500091120                     dsp_aid = c_F16;
109600091120                 exsr  sr_UpdateAll;
109700091120                 if  ErrGenerico = *on;
109800091120                   leavesr;
109900091120                 endif;
110000100331                 // ·?Uscita dal programma SE richiamato?
110100091120                 if  $Called = *on   and   W0Ccli > *zero;
110200091120                   $Fine = *on;
110300100331                 // ·?Videata iniziale altrimenti?
110400091120                 else;
110500091120                   $Video  = 'D1';
110600091120                   $InzD01 = *on;
110700091120                 endif;
110800091120             endsl;
110900091102
111000091102         ENDSL;
111100091102
111200091126         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
111300091105         if  $Video = 'D1';
111400091102           clear  V1Topz;
111500091102           clear  BloAnn;
111600091120           VisBloAnn = *off;
111700091102         endif;
111800091102
111900091102       ENDSR;
112000091102
112100091102       //--------------------------------------------------------------
112200091105       //?Inizializzazione dati nella videata D02.                     ?
112300091102       //--------------------------------------------------------------
112400091102       BEGSR  sr_InzD02;
112500091102
112600091102         reset  $Protect;
112700091125         reset  $Found_3CP;
112800100312         reset  $Found_TIVSS;
112900120605         reset  $Annull_3EW;
113000100212         reset  $EasyWeb;
113100091125         reset  VisBloAnn;
113200091125         reset  ProtectNRS;
113300091125         reset  ProtectCTM;
113400100407         reset  $Bypass_01;
113500121128         reset  $F2inD2;
113600140718         reset  $CtrlRngSgnCl;
113700110422         reset  SavF1Attivo;
113800120725         reset  wCount_3EW;
113900120725         clear  rqsKSU;
114000100312         clear  TIVSSds;
114100110629         clear  TIVSSds_Orig;
114200120813         clear  ParFIL;
114300120813         clear  ParNRS;
114400120813         reset  SavNRS;
114500120813         reset  SavABD;
114600120813         reset  SavCKS;
114700120813         reset  SavCBA;
114800120813         reset  SavCTM;
114900120813         reset  SavTBLkey_tmp;
115000120813         reset  SavCliTab3CPEW;
115100120813         clear  SaveFLS;
115200120813         clear  SaveNRS;
115300120813         clear  SaveNCD;
115400120813         clear  SaveNCA;
115500120813         clear  SaveCTM;
115600140718         clear  SavW1Cnrs;
115700140718         clear  SavW1Cncd;
115800140718         clear  SavW1Cnca;
115900121220         clear  Save_3CP_nrs;
116000121220         clear  Save_3CP_nsi;
116100121220         clear  Save_3CP_nsf;
116200101022         clear  ubLeg3C_FlgPF;
116300101022         clear  ubLeg3C_Padre;
116400101022         clear  ubLeg3C_SKC;
116500101022         clear  ubLeg3C_SKEW;
116600110629         clear  sch_SKC_Orig;
116700170320         clear  Save3CLNrS  ;
116800170320         clear  Save3CLLNP  ;
116900120130         Protect_3EW  = *off;
117000120130         Reverse_3EW  = *off;
117100120130         $FlsNrsIn3CL = *off;
117200120130         F13Attivo    = *off;
117300091125
117400091127         clear  TB28D2;
117500100312         clear  ds3C;
117600100921         clear  ds3C_Old;
117700121112         clear  d3EW_Old;
117800091102
117900091126         // -?Impostazione testata?
118000091102         clear  V1Topz;
118100091102         clear  BloAnn;
118200091102         select;
118300091102           when  Not %found(TABEL00F);
118400091102             V1Topz = '  INSERIMENTO  ';
118500091102           when  TBLflg = '*';
118600091102             V1Topz = '  RIPRISTINO   ';
118700091102           other;
118800091102             V1Topz = '   MODIFICA    ';
118900091102         endsl;
119000091102         select;
119100100312           // -?Codice annullato?
119200091102           when  ACOflg <> *blank;
119300091102             if  %found(TABEL00F);
119400130528               BloAnn   = 'CODICE ANNULLATO';
119500091102               $Protect = *on;
119600091102             endif;
119700100312           // -?Codice bloccato?
119800130528           when  ACOabl <> *blanks;
119900130528             BloAnn   = 'CODICE  BLOCCATO';
120000130520             clear  TRULBLCds;
120100130520             iBLCkcc = DUTkci;
120200130520             iBLCksc = %int(V1Ccli);
120300130520             trulBLCr ( kpjba : trulBLCds );
120400130520             if  oBLCdav > 00010101;
120500130520               wDate_Dmy = %date( oBLCdav : *iso );
120600130520               wDate6    = %dec( wDate_Dmy );
120700130520               BloAnn    = 'BLOCCATO IL ' +
120800130520                           %editw ( wDate6 : '  /  /  ');
120900130520             endif;
121000091102         endsl;
121100091102
121200091102
121300091126         // -?Impostazione dettaglio?
121400091102         V2Ccli = %int(V1Ccli);
121500091106         V2Crag = %subst( ACOrag : 1 : %len(V2Crag) );
121600091102
121700091126         // -?Solo se Modifica/Annullamento/Ripristino?
121800091102         IF  %found(TABEL00F);
121900091102
122000100921           ds3C_Old = TBLuni;
122100100921
122200091102           ds3C = TBLuni;
122300091102           V2Cnrs = §3Cnrs;
122400100120           if  §3Cfls <> *zero;
122500100120             V2Dfls = %editc( §3Cfls : 'X' );
122600100120           endif;
122700091102           V2Cokd = §3Cokd;
122800110311           //V2Crag = §3Crag;
122900091102           V2Clrm = §3Clrm;
123000091102           V2Calm = §3Calm;
123100091102           V2Cnot = §3Cnot;
123200091102           V2Cabc = §3Cabc;
123300091102           V2Cdkc = §3Cdkc;
123400091102           V2Cavx = §3Cavx;
123500091102           V2Cabd = §3Cabd;
123600091102           V2Caus = §3Caus;
123700091102           V2Carc = §3Carc;
123800091102           V2Cmvr = §3Cmvr;
123900091102           V2Ccba = §3Ccba;
124000091102           V2Cbac = §3Cbac;
124100091222           V2Cbc2 = §3Cbc2;
124200091102           V2Cctm = §3Cctm;
124300091102           V2Cfg1 = §3Cfg1;
124400160317           V2Amb  = §3CCSAMB;
124500091102           V2Cfsp = §3Cfsp;
124600160317           V2TFP  = §3CCSTFP;
124700091102           V2Ccks = §3Ccks;
124800091102           V2Cnwb = §3Cnwb;
124900091102           V2Cnwc = §3Cnwc;
125000091102           V2Crsb = §3Crsb;
125100091102           V2Cpes = §3Cpes;
125200100212
125300120517           // -?Reperimento famiglia completa "originale"?
125400120517           exsr  sr_ubLeg3C_Orig;
125500091102
125600091126           // -?Impostazione / Reperimento serie?
125700100312           // ->?PRIMA impostata con il default...?
125800091102           V2Cncd = 1;
125900091102           V2Cnca = *all'9';
126000100312           // ->?POI eventualmente reperita come parziale?
126100091102           clear  d3CP;
126200100215           clear  d3EW;
126300100215           select;
126400100215             when  V2Ccba = c_EasyWeb;
126500120130               // -?(il reperimento di tali dati verrà fatto dopo,?
126600120130               //   ?nella subr. sr_CtrlCBA)?
126700100215             when  V2Cnrs > *zero;
126800100215               k_TBEcod = '3CP';
126900100215               k_TBEke1 = V1Ccli;
127000100215               setll  %kds( k05tntbe01 : 2 )  TNTBE000;
127100100215               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
127200100215               dow  Not %eof(TNTBE01L);
127300100215                 if  %subst(TBEke2:1:2) = %editc(V2Cnrs:'X') AND
127400100215                   ( (TBLflg = '*'      and   TBEatb = 'A')   or
127500100215                     (TBLflg = *blank   and   TBEatb = *blank) );
127600100215                   d3CP = TBEuni;
127700100215                   V2Cncd = %int( %subst( TBEke2 : 3 : 7 ) );
127800100215                   V2Cnca = §3CPal;
127900100215                   $Found_3CP = *on;
128000121220                   Save_3CP_nrs = V2Cnrs;
128100121220                   Save_3CP_nsi = V2Cncd;
128200121220                   Save_3CP_nsf = V2Cnca;
128300100215                   leave;
128400100215                 endif;
128500100215                 reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
128600100215               enddo;
128700100215           endsl;
128800120726
128900120726           // -?Reperimento chiave completa per tab. "3EW"?
129000120726           //  ?se padre non trovato nel file TIVSS non può essere in?
129100120726           //  ?  tab. "3EW"?
129200120726           clear  rqsSUN;
129300120726           //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
129400120726           if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
129500120726             exsr  sr_TIS7701_x_TIVSS;
129600120726           endif;
129700120726           if  $Found_TIVSS;
129800120726             TIVSSds_Orig = TIVSSds;
129900120726           endif;
130000120726
130100120726           // -?Controllo esistenza padre in tab. "3EW" pur non essendo?
130200120726           //  ?più abilitato ad EasyWEB su STRATEGI?
130300120726           IF  NOT $Found_TIVSS        and
130400120726               //Old_§3Ccks = V2Ccli   and
130500120726               Old_§3Ccba = c_EasyWEB;
130600120726
130700120726             // ·?Reperimento dati "EasyWEB" del padre da tab. "3EW"?
130800120726             //  ?(serve per sapere se l'abilitazione ad EasyWEB del?
130900120726             //  ?padre originale è stata fatta scadere su STRATEGI,?
131000120726             //  ?ma esiste ancora la tab. "3EW")?
131100120726             ds_TNTBE.TBEke1 = '0' + %editc( Old_§3Ccks : 'X' );
131200120726             if  getTabella ( c_Tntbe : '3EW' : ds_TNTBE.TBEke1 :
131300120726                              *omit : *blank : *blank :
131400120726                              *omit : *omit :
131500120726                              *omit : *omit : *omit : *omit :
131600120726                              *omit : *omit : *omit : *omit :
131700120726                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
131800120726                          = *zero      AND
131900120726                 ds_TNTBE.TBEatb = *blank;
132000121112               d3EW_Old  = ds_TNTBE.TBEuni;
132100120814               d3EW_Temp = ds_TNTBE.TBEuni;
132200120814               if  d3EW_Temp.§3EWfaa = *blank;
132300120814                 // ·?Padre già disabilitato ad EasySped-WEB ma ancora?
132400120814                 //  ?in tab. "3EW": occorre annullare la tab. "3EW"?
132500120814                 $Annull_3EW = (Not $Found_TIVSS);
132600120814               endif;
132700120726             endif;
132800120726
132900120726           ENDIF;
133000091102
133100100312           // -?Decodifiche:?
133200091125
133300100312           // ->?Cliente Unificante?
133400120605           //clear  V2Dcks;           ?(già così)?
133500091125           exsr  sr_CtrlCKS;
133600091125           if  ErrGenerico;
133700120813             reset  SavCKS;
133800091125             clear  VIDmsg;
133900091125             clear  PosCurCKS;
134000091125             clear  ErrMessage;
134100091125             clear  ErrGenerico;
134200110818           else;
134300110818             // ->?Abilitazione F1=Numero Serie & Range Segnacolli?
134400110818             exsr  sr_CtrlRngPadre;
134500110818             if  ErrGenerico;
134600120813               reset  SavCKS;
134700110818               clear  ErrGenerico;
134800120813             else;
134900120813               SavCKS = V2Ccks;
135000110818             endif;
135100091125           endif;
135200091106
135300120305           // ->?Supporto Cliente a BRT?
135400091106           exsr  sr_CtrlCBA;
135500121112           if  wCount_3EW = 1;
135600121112             d3EW_Old = d3EW;
135700121112           endif;
135800091106           if  ErrGenerico;
135900120813             reset  SavCBA;
136000110201             reset  $Bypass_01;
136100100312             reset  $EasyWeb;
136200091106             clear  VIDmsg;
136300091106             clear  PosCurCBA;
136400091126             clear  PosCurNRS;
136500120208             clear  PosCurCKS;
136600091106             clear  ErrMessage;
136700091106             clear  ErrGenerico;
136800120813           else;
136900120813             SavCBA = V2Ccba;
137000091106           endif;
137100110311
137200110311           // ->?Numero Serie & Range Segnacolli?
137300110818           //   ?(GIÀ FATTO)?
137400110818           //exsr  sr_CtrlRngPadre;
137500110818           //if  ErrGenerico;
137600110818           //  clear  ErrGenerico;
137700120130           //endif;
137800091125
137900100312           // ->?Codice Trattamento Merce?
138000091125           exsr  sr_CtrlCTM;
138100091125           if  ErrGenerico;
138200120813             reset  SavCTM;
138300091125             clear  VIDmsg;
138400091125             clear  PosCurCTM;
138500091125             clear  ErrMessage;
138600091125             clear  ErrGenerico;
138700120813           else;
138800120813             SavCTM = V2Cctm;
138900091125           endif;
139000120208
139100120208           // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
139200120208           //   ?se NON reperito il range segnacolli per EasyWEB?
139300120726           if  V2Ccba   =  c_EasyWEB    and
139400120726              (rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
139500120726                                   or   TIVSSds.VSSksu = *zero);
139600120725             VIDmsg = $Msg(14);
139700120725             if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
139800120803                 V2Cncd = 1          and  V2Cnca = *all'9';
139900120725               // -?Abilitazione F13=Interrog. tab. "3EW"?
140000120725               F13Attivo = *on;
140100120725               //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
140200120725               VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
140300120725             endif;
140400120725             PosCurCKS   = *on;
140500120725             ErrMessage  = *on;
140600120725             ErrGenerico = *on;
140700120726             //leavesr;
140800120208           endif;
140900120725
141000120725           // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
141100120726           //   ?se cliente presente più di una sola volta nella "3EW"?
141200120726           //   ?(errore "alternativo" al precedente: meglio non uscire?
141300120726           //   ?dalla subroutine per eseguire comunque la pulizia dei?
141400120726           //   ?campi sottostante)?
141500120726           //if  wCount_3EW > 1;
141600120726           //  // -?Abilitazione F13=Interrog. tab. "3EW"?
141700120726           //  F13Attivo   = *on;
141800120726           //  VIDmsg = $Msg(40);
141900120726           //  //PosCurNRS   = *on;
142000120726           //  PosCurCKS   = *on;
142100120726           //  ErrMessage  = *on;
142200120726           //  ErrGenerico = *on;
142300120726           //  leavesr;
142400120726           //endif;
142500120726           if  wCount_3EW > 1;
142600150319             //$VideoPrec  = $Video;       ?(già fatto)?
142700150319             $Video = 'W4';
142800120726             reset  $InzW04;
142900120726             ErrGenerico = *on;
143000120726             //leavesr;
143100120726           endif;
143200091102
143300091102         ELSE;
143400091102
143500100211           V2Cfg1 = 'N';
143600100211           V2Cfsp = 'N';
143700100211           V2Cnwb = 'S';
143800100211           V2Cnwc = 'L';
143900100211           V2Cncd = 1;
144000100211           V2Cnca = *all'9';
144100091102
144200091102         ENDIF;
144300091102
144400091126         // -?Pulizia campi per controllo forzatura errore?
144500100623         reset  SavNRS;
144600120131         //reset  SavNCD;
144700120131         //reset  SavNCA;
144800100623         reset  SavABD;
144900120813         //reset  SavCKS;
145000120813         //reset  SavCBA;
145100120813         //reset  SavCTM;
145200100623         reset  SavTBLkey_tmp;
145300110311         reset  SavCliTab3CPEW;
145400091102
145500091102       ENDSR;
145600101026
145700101026       //--------------------------------------------------------------
145800101026       //?Reperimento dati di TIVSS tramite TIS7701R.                  ?
145900101026       //--------------------------------------------------------------
146000101026       BEGSR  sr_TIS7701_x_TIVSS;
146100101026
146200101026         $TIS7701 = *on;
146300101026
146400120725         rqsKSU = '0' + %editc( V2Ccks : 'X' );
146500101026         clear  rpyEsito;
146600101026         Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
146700101028                                  rqsSUN : *zero : TIVSSds :
146800101026                                  rpyEsito );
146900101026
147000101026         $Found_TIVSS = (rpyEsito = *zero  and  TIVSSds.VSSksu > *zero);
147100101026
147200101026       ENDSR;
147300091125
147400091125       //--------------------------------------------------------------
147500091125       //?Impostazione attributi di visualizzazione in D02.            ?
147600091125       //--------------------------------------------------------------
147700091125       BEGSR  sr_AttrVisD02;
147800091125
147900091126         // ->?Visualizzazione "CLIENTE BLOCCATO" in testata?
148000091125         VisBloAnn = (BloAnn <> *blank);
148100091125
148200091126         // ->?Permessa interrogazione tab. "3CE"?
148300110930         //VisInfoES = (§3Ccba = 'ESYSP'   or   §3Ccba = 'ESVAL');
148400110930         VisInfoES = (§3Ccba = 'ESYSP'   or   §3Ccba = 'ESVAL'   or
148500110930                      getTabella ( c_Tntbe : '3CE' : V1Ccli :
148600110930                                   *blank: *omit : *omit :
148700110930                                   *omit : *omit :
148800110930                                   *omit : *omit : *omit : *omit :
148900110930                                   *omit : *omit : *omit : *omit :
149000110930                                   ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
149100110930                                 = *zero      AND
149200110930                      ds_TNTBE.TBEatb = *blank);
149300091125
149400091126         // ->?Protezione "Numero Serie"?
149500091127         ProtectNRS = (V2Ccli <> V2Ccks   and   V2Ccba = c_EasyWEB
149600091127                                          and   V2Ccba = p_§3Ccba);
149700091125
149800091126         // ->?Protezione "Codice Trattamento Merce"?
149900120305         //   ?se Supporto Cliente a BRT = 'ESWEB' ANCHE per il padre?
150000091126         ProtectCTM = ((V2Ccba = c_EasyWEB  and  V2Ccks = V2Ccli)   OR
150100091126                       (V2Ccba = c_EasyWEB  and  V2Cctm = p_§3Cctm  and
150200091126                      p_§3Ccba = c_EasyWEB));
150300120130
150400120130         // ->?Numero Serie & Range Segnacolli in tab. "3CL"?
150500120130         ds_TNTBE.TBEke1 = V2Dfls;
150600120130         ds_TNTBE.TBEke2 = %editc( V2Cnrs : 'X' );
150700120130         $FlsNrsIn3CL = ( V2Cncd = 1  and  V2Cnca = *all'9'  and
150800120130                        getTabella ( c_Tntbe : '3CL' : ds_TNTBE.TBEke1 :
150900120130                                     ds_TNTBE.TBEke2 : *omit : *omit :
151000120130                                     *omit : *omit :
151100120130                                     *omit : *omit : *omit : *omit :
151200120130                                     *omit : *omit : *omit : *omit :
151300120130                                     ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
151400120130                                   = *zero      AND
151500120130                        ds_TNTBE.TBEatb = *blank );
151600150127
151700150127         // ->?Filiale gest. se supporto clienta a BRT = ESWEB presa da tab.3EW
151800150127         VisLNP3EW = (§3Ccba = 'ESWEB');
151900091125
152000091125       ENDSR;
152100091102
152200091102       //--------------------------------------------------------------
152300091102       //?Abilitazione tasti funzionali in P02 (per D02 e D03).        ?
152400091102       //--------------------------------------------------------------
152500091102       BEGSR  sr_AbilFxxD02;
152600110311
152700110311         // ->?F1 = Reperimento Serie e Range segnacolli dal padre?
152800110311         //F1Attivo = ...vedi subr. sr_CtrlRngPadre
152900110422         // ·?Comunque salvo lo stato attuale...?
153000110422         SavF1Attivo = F1Attivo;
153100121128
153200121128         // ->?F2 = "Stacca" figli?
153300130103         //F2Attivo = (%found(TABEL00F)   and   TBLflg = *blank       and
153400130103         //            $Video = 'D2'      and   V2Ccli = §3Ccks       and
153500130103         //            sch_skC(2) > *zero);
153600130103         //          //$Video = 'D2'      and   V2Ccli = Old_§3Ccks   and
153700130103         //          //sch_skC_Orig(2) > *zero);
153800130103         F2Attivo = *off;
153900121128
154000121128         // ->?F3 = Fine?
154100121128         F3Attivo = (Not $F2inD2);
154200091102
154300091126         // ->?F5 = Ripristino?
154400091102         F5Attivo = (%found(TABEL00F)   and   TBLflg <> *blank);
154500091102
154600091126         // ->?F6 = Conferma?
154700091120         F6Attivo = ( ( ( %found(TABEL00F)  and  TBLflg = *blank  and
154800091120                          Not $Protect )    OR   Not %found(TABEL00F) )
154900091120                                            AND
155000091120                      ( ( $Video = 'D2'     and  V2Cabd = *blank )
155100091120                                            OR   $Video = 'D3'));
155200091214
155300091214         // ->?F8 = Interrogazazione legami tabelle VAS?
155400120807         F8Attivo =  (Not $TrTb06   and
155500120807                     ($Video = 'D2'  or  $Video = 'W4'));
155600120517
155700120517         // ->?F9 = Aggiornamento legami tabella "3C"?
155800120803         F9Attivo =  (($Video = 'D2'  or  $Video = 'W4')
155900120803                      and  NOT $Protect);
156000091102
156100091126         // ->?F10 = Ricerca serie?
156200120113         //F10Attivo = (Not F5Attivo   and   $Video <> 'D3'
156300120113         //                            and   V2Ccba <> c_EasyWEB);
156400120113         //   ?(anche in fase di ripristino)?
156500120202         //F10Attivo = ($Video <> 'D3'  and  V2Ccba <> c_EasyWEB
156600120202         //                             and  Not $FlsNrsIn3CL);
156700120202         //   ?(anche se Filiale/Serie Segnacolli in tab. "3CL")?
156800120803         F10Attivo = (($Video <> 'D3'  and  V2Ccba <> c_EasyWEB)
156900120803                      and  NOT $Protect);
157000091102
157100091126         // ->?F11 = Ricerca serie parziale?
157200091120         F11Attivo = (F10Attivo);
157300091102
157400091126         // ->?F12 = Ritorno?
157500121128         F12Attivo = ((Not $Called   or   $Video = 'D3')  and
157600121128                       Not $F2inD2);
157700120517
157800120517         // ->?F13 = Interrogazione tab. "3EW"?
157900120517         //   ?(NON qui: abilitazione condizionata dal fallito reperim.?
158000120517         //   ? del range segnacolli in tab. "3EW" - in D2)?
158100120517         ////F13Attivo = *off;
158200120517
158300120518         // ->?F14 = Visualizzazione ultimo segnacollo?
158400120726         F14Attivo = ($Video = 'D2'  or  $Video = 'W4');
158500091102
158600091126         // ->?F16 = Annullamento?
158700100216         //F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank  and
158800100216         //             %subst( KNMUS : 1 : 3 ) = c_EDP           and
158900100216         //            (V2Ccli <> §3Ccks   or
159000100216         //            (V2Ccli =  §3Ccks   and   sch_SKC(2) = *zero)));
159100100216         //   ?(si abilita anche per cliente padre con figli, per?
159200100216         //    ?visualizzarne poi i figli nel messaggio di errore)?
159300100216         F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank  and
159400100216                      %subst( KNMUS : 1 : 3 ) = c_EDP);
159500091120
159600091126         // ->?F19 = VAB multimembro?
159700091120         F19Attivo = ($Video <> 'D3');
159800091102
159900120517         // -?Abilitazione F20, F21 e F21 SE NON richiamato:?
160000091126         // ->?F20 = Multimembro clienti VAB?
160100111207         //F20Attivo = (Not $Called   and   $Video <> 'D3');
160200111207         F20Attivo = ($Video <> 'D3');
160300091126         // ->?F21 = Gestione dati Consegna/Giacenza per clienti?
160400091120         F21Attivo = (Not $Called   and   $Video <> 'D3');
160500091126         // ->?F22 = Gestione dati C/A e Fattura per clienti?
160600091120         F22Attivo = (Not $Called   and   $Video <> 'D3');
160700091120
160800091126         // ->?RollUp (se impostato l'"accorpamento bolle")?
160900091120         RlUpAttivo = (V2Cabd <> *blank);
161000091102
161100091102       ENDSR;
161200120601
161300120601       //--------------------------------------------------------------
161400120601       //?Gestione tasto funzionale F1=Impostazione dati dal padre:    ?
161500120601       //?· Serie segnacolli                                           ?
161600120601       //?· Range segnacolli                                           ?
161700120601       //?· Supporto Cliente => BRT                                    ?
161800120601       //?· Codice Trattamento Merce                                   ?
161900120601       //--------------------------------------------------------------
162000120601       BEGSR  sr_F01D02;
162100120601
162200120601         // -?Supporto Cliente => BRT?
162300120601         V2Ccba = p_§3Ccba;
162400120601         // -?Numero Serie Segnacolli?
162500120601         V2Cnrs = p_§3Cnrs;
162600120927         // -?Filiale/Range Segnacolli?
162700120601         if  p_§3Ccba = c_EasyWEB;
162800120927           V2Dfls = %editc( d3EW_Padre.§3EWfls : 'X' );
162900120601           V2Cncd = d3EW_Padre.§3EWnsi;
163000120601           V2Cnca = d3EW_Padre.§3EWnsf;
163100150127           V23EWLNP = %editc( d3EW_Padre.§3EWLNP : 'X' );
163200120601         else;
163300121218           V2Dfls = %subst( %editc( V2Ccli : 'X' ) : 1 : 3 );
163400120601           V2Cncd = wNSI_Padre;
163500120601           V2Cnca = d3CP_Padre.§3CPal;
163600150127           V23EWLNP = *blank;
163700120601         endif;
163800120601         if  V2Cncd = *zero  and  V2Cnca = *zero;
163900120601           V2Cncd = 1;
164000120601           V2Cnca = *all'9';
164100120601         endif;
164200120601         // -?Codice Trattamento Merce?
164300120927         if  p_§3Ccba = c_EasyWEB;
164400120927           V2Cctm = d3EW_Padre.§3EWctm;
164500120927         else;
164600120927           V2Cctm = p_§3Cctm;
164700120927         endif;
164800140123
164900140123         // -?"Forzature" richiestemi da Mirko C.?
165000140123         If  V2Ccba = c_EasyWEB;
165100140123           // -?Supporto BRT a Cliente?
165200140123           if  V2Ccli = V2Ccks;
165300140123             V2Cbac = 'EMAIL';
165400140123           endif;
165500140123           // -?Abilitazione Serie in Bollettazione?
165600140123           V2Cfg1 = 'S';
165700140123         EndIf;
165800120601
165900120601         // -?DISabilitazione F1=Reperimento Serie e Range segnacolli?
166000120601         //                     ?dal padre?
166100120601         F1Attivo = *off;
166200120601
166300120601       ENDSR;
166400121128
166500121128       //--------------------------------------------------------------
166600121128       //?Gestione tasto funzionale F2="Stacca" figli                  ?
166700121128       //--------------------------------------------------------------
166800121128       BEGSR  sr_F02D02;
166900121128
167000121128         reset  TRTB28ds4;
167100121205         //iTB28ksuA = V2Ccks;
167200121205         iTB28ksuA = §3Ccks;
167300121128         kpjbu = TRTB28ds4;
167400121204
167500121204         unlock  TABEL00F;
167600121128
167700121128         TRTB28R4 ( kpjba );
167800121206
167900121206         chain  %kds(k03tabel00)  TABEL;
168000121206         //ds3C = TBLuni;           ?(già così)?
168100121128
168200121128         TRTB28ds4 = kpjbu;
168300121128
168400121128         Select;
168500121128           // -?Restituito errore?
168600121128           When  oTB28err =  *on;
168700121128             VIDmsg = oTB28msg;
168800121128             PosCurCKS   = *on;
168900121128             ErrMessage  = *on;
169000121128             ErrGenerico = *on;
169100121128             leavesr;
169200121128           // -?Premuto F6: aggiornamento legami tabella "3C" e?
169300121128           //              ?riesecuzione controllo range segnacolli?
169400121128           When  oTB28fxx =  '06';
169500121128             $F2inD2 = *on;
169600121204             V2Ccks  = oTB28ksuN;
169700121204             clear  V2Cbac;
169800121128             exsr  sr_ubLeg3C_Orig;
169900121128             clear  ubLeg3C_Padre;
170000121128             exsr  sr_Ctrl_NRS_SCx;
170100121128             if  ErrGenerico = *on;
170200121128               leavesr;
170300121128             endif;
170400121204             exsr  sr_CtrD02;
170500121204             if  ErrGenerico = *on;
170600121204               leavesr;
170700121204             endif;
170800121205             F2Attivo = *off;
170900121128         EndSl;
171000121128
171100121128       ENDSR;
171200091214
171300091214       //--------------------------------------------------------------
171400091214       //?Gestione tasto funzionale F8=Interrog. legami tab. VAS       ?
171500091214       //--------------------------------------------------------------
171600091214       BEGSR  sr_F08D02;
171700091214
171800100211         clear  trtb06ds;
171900100211         D06ksc = %int(V1Ccli);
172000100211
172100100211         kpjbu = trtb06ds;
172200091214
172300091214         trtb06r ( kpjba );
172400091214
172500091214       ENDSR;
172600091104
172700091104       //--------------------------------------------------------------
172800091104       //?Gestione tasto funzionale F10=Ricerca Serie (in D02).        ?
172900091104       //--------------------------------------------------------------
173000091104       BEGSR  sr_F10D02;
173100091104
173200091126         // -?NON modificabile manualmente premendo F10?
173300091104         if  V2Cnrs <> *zero   and  V2Cnrs <> SavNRS;
173400091104           SavNRS = V2Cnrs;
173500091125           VIDmsg = $Msg(26);
173600091104           PosCurNRS   = *on;
173700091104           ErrMessage  = *on;
173800091104           ErrGenerico = *on;
173900091104           leavesr;
174000091104         endif;
174100111215
174200111215         // -?Reperimento famiglia completa?
174300111215         //  ?(se premuto F10 PRIMA di Invio, la famiglia?
174400111215         //  ? NON è ancora stata reperita)?
174500120104         exsr  sr_ubLeg3C;
174600091104
174700091126         // -?Ricerca?
174800091104         clear  TRTB28ds2;
174900100215         if  V2Dfls > *zero;
175000100215           ParFIL = %int(V2Dfls);
175100100215         else;
175200100215           ParFIL = V2Ccli / 10000;
175300100215         endif;
175400091104         ParELB = 'R';
175500111215         trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
175600120113         select;
175700120113           // ->?Serie NON ammessa (99)?
175800120113           when  ParERR = '9';
175900120113             VIDmsg = $Msg(43);
176000120113             PosCurNRS   = *on;
176100120113             ErrMessage  = *on;
176200120113             ErrGenerico = *on;
176300120113             leavesr;
176400120113           // ->?Non reperibile una nuova serie?
176500120113           when  ParERR <> *blank;
176600120113             VIDmsg = $Msg(27);
176700120113             PosCurNRS   = *on;
176800120113             ErrMessage  = *on;
176900120113             ErrGenerico = *on;
177000120113             leavesr;
177100120113           // ->?Reperita serie completa?
177200120113           other;
177300120113             V2Cnrs = ParNRS;
177400120113             V2Cncd = 1;
177500120113             V2Cnca = *hival;
177600120113         endsl;
177700091109
177800091126         // -?Ritorno alla videata D02?
177900091109         $VideoPrec = $Video;
178000091109         $Video     = 'D2';
178100091104
178200091104       ENDSR;
178300100211
178400100211       //--------------------------------------------------------------
178500100211       //?Gestione tasto funzionale F13=Interrog. tab. "3EW"           ?
178600100211       //--------------------------------------------------------------
178700100211       BEGSR  sr_F13D02;
178800100211
178900100212         clear  tntb79ds;
179000100212         TNTB79ds.i3EWksu = '0' + %editc( V2Ccks : 'X' );
179100100211
179200100212         tntb79r1 ( kpjba : TNTB79ds );
179300101029
179400101029         if  TNTB79ds.o3EWfxx = *blank   and
179500101029             TNTB79ds.o3EWksu = '0' + %editc( V2Ccks : 'X' );
179600120215           d3EW   = TNTB79ds.o3EWuni;
179700120215           V2Dfls = %editc( d3EW.§3EWfls : 'X' );
179800150127           V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
179900120215           V2Cnrs = d3EW.§3EWnrs;
180000120215           V2Cncd = d3EW.§3EWnsi;
180100120215           V2Cnca = d3EW.§3EWnsf;
180200120215           V2Cctm = d3EW.§3EWctm;
180300120816           if  d3EW.§3EWfaa = *blank;
180400120816             wCount_3EW = 1;
180500120816             rqsSUN = TNTB79ds.o3EWsun;
180600120816             exsr  sr_TIS7701_x_TIVSS;
180700120816             $Bypass_01 = *on;
180800120816           //else;
180900120816           //  VIDmsg = $Msg(44);
181000120816           //  PosCurNRS   = *on;
181100120816           //  ErrMessage  = *on;
181200120816           //  ErrGenerico = *on;
181300120816           //  leavesr;
181400120816           endif;
181500101029         endif;
181600100211
181700100211       ENDSR;
181800091102
181900091102       //--------------------------------------------------------------
182000091104       //?Gestione tasti funzionali F19, F20, F21 e F22 in D02.        ?
182100091102       //--------------------------------------------------------------
182200091102       BEGSR  sr_FxxD02;
182300091102
182400091126         // -?Il chiamato RICHIEDE un codice numerico per verificare?
182500091126         //  ?se è stato richiamato?
182600091102         if  V1Ccli = *blank;
182700091102           KPJBU = *zero;
182800091102         else;
182900091102           KPJBU = V1Ccli;
183000091102         endif;
183100091102
183200091102         select;
183300091104
183400091126           // -?F17 = Gestione serie per Disk "C" con prefisso "CC"?
183500091104           when  dsp_aid = c_F17;
183600091104             tntb13r ( kpjba );
183700091104
183800091126           // -?F18 = Gestione particolarità Disk "C"?
183900091104           when  dsp_aid = c_F18;
184000091104             tntb10r ( kpjba );
184100091104
184200091126           // -?F19 = Gestione multimembro clienti VAB?
184300091104           when  dsp_aid = c_F19;
184400091104             trtb83r ( kpjba );
184500091102
184600091126           // -?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
184700091102           when  dsp_aid = c_F21;
184800091102             trtb40r ( kpjba );
184900091102
185000091126           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
185100091102           when  dsp_aid = c_F22;
185200091102             trtb44r ( kpjba );
185300091102
185400091102         endsl;
185500091102
185600091102       ENDSR;
185700091214
185800091214       //--------------------------------------------------------------
185900091214       //?Controllo annullabilità del record nella videata D02.        ?
186000091214       //--------------------------------------------------------------
186100100312       BEGSR  sr_CtrlANN;
186200091214
186300091214         %subst(IndDspF : 50) = *off;
186400091214
186500091214         // -?Se cliente Unificante: NON deve avere figli?
186600100322         if  %found(TNTBE01L)   and   V2Ccli = §3Ccks   and
186700120326             sch_SKC_Orig(2) > *zero;
186800120517           // -?Su richiesta di Mirko: si riesegue il caricamento dei?
186900120517           //  ?legami (potrebbero essere stati annullati tutti i figli?
187000120517           //  ?nel frattempo - da un'altra sessione video)?
187100120517           exsr  sr_ubLeg3C_Orig;
187200120517           if  sch_SKC_Orig(2) > *zero;
187300120517             VIDmsg = %trimr($Msg(35)) + ' ' +
187400120517                      %editc( sch_SKC_Orig(2) : 'X' );
187500120517             if  sch_SKC_Orig(3) > *zero;
187600120517               VIDmsg = %trimr(VIDmsg) + ' ed altri';
187700120517             endif;
187800120517             PosCurCKS   = *on;
187900120517             ErrMessage  = *on;
188000120517             ErrGenerico = *on;
188100120517             leavesr;
188200120517           endif;
188300091214         endif;
188400120214
188500120214         // -?NON consentito l'annullamento (da qui) SE cliente?
188600120329         //  ?PADRE abilitato ad EasySepd-WEB su Strategi?
188700120215         //  ?(e NON richiamato proprio per quello)?
188800120329         //  ?Un cliente figlio è annullabile anche se abilitato?
188900120329         //    ?ad EasyWEB (quindi con padre abilitato ad EasyWEB)!?
189000120214         If  Not  $3EWupd         and
189100120214             §3Ccba = c_EasyWEB   and
189200120329             §3Ccks = V2Ccli      and
189300120214             %editc(§3Ccks : 'X') = %subst(TIVSSds_Orig.VSSksu : 2);
189400120214           VIDmsg = $Msg(32);
189500120214           ErrMessage  = *on;
189600120214           ErrGenerico = *on;
189700120214           leavesr;
189800120214         EndIf;
189900101027
190000101027         // -?Se cliente Unificante presente con più codici Strategi,?
190100101027         //  ?occorre selezionare quello corretto?
190200120214         //If  wCount_3EW > 1;
190300120214         //  $VideoPrec = $Video;
190400120214         //  $Video     = 'W4';
190500120214         //  reset  $InzW04;
190600120214         //EndIf;
190700091214
190800091214       ENDSR;
190900091102
191000091102       //--------------------------------------------------------------
191100091102       //?Controllo dati immessi nella videata D02.                    ?
191200091102       //--------------------------------------------------------------
191300091102       BEGSR  sr_CtrD02;
191400091104
191500091104         %subst(IndDspF : 50) = *off;
191600100212
191700100212         // -?Spegnimento indicatori che:?
191800120305         //  ?1) memorizza l'impostaz. "Supporto Cliente a BRT"?
191900100212         //     ?EasyWeb [al cambiamento] (per fare certi controlli?
192000100212         //     ?solo la 1ª volta)?
192100100212         //  ?2) protegge i campi reperiti dal padre (FLS, NRS, Range?
192200100212         //     ?Segnacolli e CTM)?
192300100212         //  ?3) evidenziano i campi reperiti dal padre (FLS, NRS, Range?
192400100212         //     ?Segnacolli e CTM)?
192500100216         if  V2Ccks <> I69kac   and   $EasyWeb;
192600100312           reset  $EasyWeb;
192700100216         endif;
192800100212         if  V2Ccba <> c_EasyWeb;
192900100312           reset  $EasyWeb;
193000100212           Protect_3EW = *off;
193100100212         endif;
193200120130         Reverse_3EW  = *off;
193300120130         $FlsNrsIn3CL = *off;
193400091125
193500091126         // -?Ricerche?
193600091125         exsr  sr_Search;
193700091124
193800091126         // -?Cliente Unificante?
193900120510         //  ?N.B.: è meglio resettare il campo di controllo (SAVCKS)?
194000120510         //  ?per rieseguire i controlli anche nel caso in cui la 1ª?
194100120510         //  ?modifica abbia avuto esito negativo (errore).?
194200110121         if  V2Ccks <> SavCKS  or
194300110121             V2Ccks <> I69kac;
194400120510           reset  SavCKS;
194500120104           clear  ParFIL;
194600120104           clear  ParNRS;
194700100312           exsr  sr_CtrlCKS;
194800100312           if  ErrGenerico;
194900100312             leavesr;
195000100312           endif;
195100100312         endif;
195200160317
195300120305         // -?Supporto Cliente a BRT?
195400120510         //  ?N.B.: è meglio resettare il campo di controllo (SAVCBA)?
195500120510         //  ?per rieseguire i controlli anche nel caso in cui la 1ª?
195600120510         //  ?modifica abbia avuto esito negativo (errore).?
195700100312         if  V2Ccks <> SavCKS   or   V2Ccba <> SavCBA;
195800120510           reset  SavCBA;
195900100312           exsr  sr_CtrlCBA;
196000100312           if  ErrGenerico;
196100100312             leavesr;
196200100312           endif;
196300100312         endif;
196400120125
196500120125         // -?Memorizzazione codici già controllati con esito positivo?
196600120125         //  ?per evitare di ripetere tali controlli...?
196700120125         SavCBA = V2Ccba;
196800120125         SavCKS = V2Ccks;
196900091103
197000091126         // -?Codice Trattamento Merce?
197100100312         if  V2Cctm <> SavCTM;
197200100312           exsr  sr_CtrlCTM;
197300100312           if  ErrGenerico;
197400100312             leavesr;
197500100312           endif;
197600120125           SavCTM = V2Cctm;
197700100312         endif;
197800110311
197900110311         // -?Abilitazione F1 per?
198000110311         //  ?Numero Serie & Range Segnacolli dal padre?
198100110311         exsr  sr_CtrlRngPadre;
198200120515         //if  ErrGenerico;         ?(meglio proseguire...)?
198300120515         //  leavesr;               ?(meglio proseguire...)?
198400120515         //endif;                   ?(meglio proseguire...)?
198500091103
198600091126         // -?Numero Serie?
198700091126         // ->?Obbligatorio (se CTM da cliente Flag6)?
198800100312         if  V2Cnrs = *zero   and   §1Bfg6 = 'C';
198900091125           VIDmsg = $Msg(17);
199000091103           PosCurNRS   = *on;
199100091103           ErrMessage  = *on;
199200091103           ErrGenerico = *on;
199300091103           leavesr;
199400091103         endif;
199500091103
199600120202         Select;
199700120202
199800120202           // ->?Nessuna serie immessa?
199900120202           When  V2Cnrs = *zero;
200000120202             V2Cncd = 1;
200100120202             V2Cnca = *hival;
200200100329
200300120202           // ->?Controlli per serie completa o parziale?
200400120202           //   ?(controlla prima serie parziale, poi completa)?
200500120202           Other;
200600120202             W1Cnrs = V2Cnrs;
200700120202             W1Cncd = V2Cncd;
200800120202             W1Cnca = V2Cnca;
200900120202             exsr  sr_Ctrl_NRS_SCx;
201000120202             if  ErrGenerico = *on;
201100120202               leavesr;
201200120202             endif;
201300120202
201400120202         EndSl;
201500100111
201600100111         // -?Filiale Segnacollo?
201700100111         select;
201800100111           when  V2Cnrs = *zero;
201900100111             clear  V2Dfls;
202000100111           //when  §3Cfls <> *zero;
202100100120           //  V2Dfls = %editc( §3Cfls : 'X' );
202200100326           when  V2Ccba = c_EasyWEB   and   d3EW.§3EWfls > *zero;
202300100326             V2Dfls = %editc( d3EW.§3EWfls : 'X' );
202400150127             if  d3EW.§3EWLNP > *zero;
202500150127               V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
202600150127             endif;
202700100111           other;
202800100120             V2Dfls = %subst( V1Ccli : 1 : %len(V2Dfls) );
202900150127               V23EWLNP = *blank;
203000100111         endsl;
203100091104
203200120305         // -?Supporto BRT a Cliente?
203300091201         select;
203400100312           // -?Obbligatorio per cliente unificante con serie?
203500091201           when  V2Cbac =  *blank   and
203600091201                 V2Ccks =  V2Ccli   and
203700091201                 V2Cnrs <> *zero;
203800091201             VIDmsg = $Msg(34);
203900091201             PosCurBAC   = *on;
204000091201             ErrMessage  = *on;
204100091201             ErrGenerico = *on;
204200091201             leavesr;
204300100312           // -?Indicabile SOLO sul cliente unificante?
204400091201           when  V2Cbac <> *blank   and
204500091201                 V2Ccks <> V2Ccli;
204600091201             VIDmsg = $Msg(22);
204700091201             PosCurBAC   = *on;
204800091201             ErrMessage  = *on;
204900091201             ErrGenerico = *on;
205000091201             leavesr;
205100091201         endsl;
205200091124
205300091126         // -?"Accorpamento bolle" diverso dal quello del padre?
205400091127         if  V2Ccks <> V2Ccli     and
205500100312             V2Cabd <> p_§3Cabd   and   V2Cabd <> SavABD;
205600091125           SavABD = V2Cabd;
205700091125           VIDmsg = %trimr($Msg(23)) + ' "' + p_§3Cabd +
205800091125                    '" » "Invio" per forzare';
205900091125           PosCurABD   = *on;
206000091124           ErrMessage  = *on;
206100091124           ErrGenerico = *on;
206200091124           leavesr;
206300091124         endif;
206400120605
206500120605         // -?Emissione messaggio di avvertimento della tab. "3EW"?
206600120605         //  ?ancora attiva nonostante manchi l'abilitazione ad EasyWeb?
206700120605         If  $Annull_3EW   and
206800120605               // -?Non un solo codice in TIVSS?
206900120605             ( wCount_3EW <> 1        or
207000120605               // -?Nessun codice selezionato da TIVSS?
207100120605               W4Csun = *zero  or  W4Csun <> TIVSSds.VSSsun);
207200120605           $VideoPrec = $Video;
207300120605           $Video = 'W3';
207400120605           clear  TB28W3;
207500120605           W3txt2 = 'Annullare ' + %editc( Old_§3Ccks : 'X' ) +
207600120605                    ' in tab. "3EW".';
207700120802           W3txt3 = 'Premere Invio per uscire.';
207800120802           exfmt  TB28W3;
207900120605           reset  $Annull_3EW;
208000120605           $Video = $VideoPrec;
208100150319           clear  $VideoPrec;
208200120605         EndIf;
208300120726
208400120726         // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
208500120726         //   ?se cliente presente più di una sola volta nella "3EW"?
208600120726         //   ?(errore "alternativo" al precedente: meglio non uscire?
208700120726         //   ?dalla subroutine per eseguire comunque la pulizia dei?
208800120726         //   ?campi sottostante)?
208900120726         //if  wCount_3EW > 1;
209000120726         //  // -?Abilitazione F13=Interrog. tab. "3EW"?
209100120726         //  F13Attivo   = *on;
209200120726         //  VIDmsg = $Msg(40);
209300120726         //  //PosCurNRS   = *on;
209400120726         //  PosCurCKS   = *on;
209500120726         //  ErrMessage  = *on;
209600120726         //  ErrGenerico = *on;
209700120726         //  leavesr;
209800120726         //endif;
209900120726         if  wCount_3EW > 1;
210000120726           $VideoPrec  = $Video;
210100120726           $Video      = 'W4';
210200120726           reset  $InzW04;
210300120726           ErrGenerico = *on;
210400120726           leavesr;
210500120726         endif;
210600160317
210700160317         // -?Cappario semplificato ?
210800160317         //  ?Ambito e Tertminal partenza vanno valorizzati entrambi o
210900160317         //  ?lasciati vuoti entrambi
211000160317         if  V2Amb <> *blank and V2TFP = 0  OR
211100160317             V2Amb =  *blank and V2TFP <>0;
211200160317           VIDmsg = %trimr($Msg(48));
211300160317           PosCurAMB   = *on;
211400160317           ErrMessage  = *on;
211500160317           ErrGenerico = *on;
211600160317           leavesr;
211700160317         endif;
211800160317
211900160317         // -?Terminal partenza?
212000160321         // 999 non esiste ma vale tutti
212100160321         if  V2Amb <> *blank and V2TFP > 0
212200160321             and V2TFP <> 999;
212300160317           chain  (V2TFP)  AZORG;
212400160317           if  NOT %found(AZORG01L) OR
212500160317               %found(AZORG01L) and %subst(ORGDE8:1:1)<>'1';
212600160317             VIDmsg = $Msg(49);
212700160317             PosCurAMB   = *on;
212800160317             ErrMessage  = *on;
212900160317             ErrGenerico = *on;
213000160317             leavesr;
213100160317           endif;
213200160317         endif;
213300100312
213400100312         // -?Memorizzazione codici già controllati con esito positivo?
213500100312         //  ?per evitare di ripetere tali controlli...?
213600100312         //  ?(se non sono stati rilevati errori: in caso di errore?
213700100312         //  ?si sarebbe usciti prima dalla subr.!)?
213800120125         //  ?Meglio spostare tali memorizzazioni subito DOPO i loro?
213900120125         //  ?controlli specifici: questa memorizzazione salterebbe?
214000120125         //  ?in caso di errore ad un controllo legato ad un campo?
214100120125         //  ?della loro struttura dati (vedi §1BFG6).?
214200120125         //SavCKS = V2Ccks;             ?(già fatto)?
214300120125         //SavCBA = V2Ccba;             ?(già fatto)?
214400120125         //SavCTM = V2Cctm;             ?(già fatto)?
214500091104
214600091104       ENDSR;
214700101014
214800101014       //--------------------------------------------------------------
214900101014       //?Controllo range segnacolli per filiale e serie.              ?
215000101014       //--------------------------------------------------------------
215100101014       BEGSR  sr_Ctrl_NRS_SCx;
215200110121
215300110207         // -?Reperimento famiglia completa?
215400120517         //  ?(se premuto F11  PRIMA  di Invio, la famiglia?
215500110121         //  ? NON è ancora stata reperita)?
215600120104         exsr  sr_ubLeg3C;
215700101014
215800110207         // -?Controllo range segnacolli?
215900101014         clear  TNTB54ds;
216000101014         if  V2Dfls > *zero;
216100101014           I54fil = V2Dfls;
216200101014         else;
216300101014           I54fil = %subst( V1Ccli : 1 : %len(I54fil) );
216400101014         endif;
216500101014         I54ksc = V1Ccli;
216600101014         I54tel = 'C';
216700101014         I54nrs = %editc( W1Cnrs : 'X' );
216800101014         I54dal = W1Cncd;
216900101014         I54al  = W1Cnca;
217000101014
217100101014         kpjbu  = TNTB54ds;
217200110207         tntb54r ( kpjba : ubLeg3C_SKC );
217300101014         TNTB54ds = kpjbu;
217400101014
217500101014         SELECT;
217600120131
217700120131           // ->?Serie già assegnata come completa (in tab. "3CL")?
217800120131           //   ?La segnalazione qui scarterebbe gli altri controlli di?
217900120131           //   ?TRTB28R2 !?
218000120131           //WHEN  O54err = 'A';
218100120131           //  //$FlsNrsIn3CL = *on;
218200120131           //  ErrGenerico = *on;
218300120131           //  leavesr;
218400160125           // WHEN  O54err = 'A'  and  (W1Cncd > 1  or
218500160125           //                          W1Cnca < *hival)
218600170321           // ri-aggiunto ma forzabile
218700160125           WHEN  O54err = 'A'
218800160125                               and (I54Nrs<>Save3CLNRS or
218900160125                                    I54Fil<>Save3CLLNP)   ;
219000120131             VIDmsg = $Msg(20);
219100160125             VIDmsg = %trimr(VIDmsg) +
219200160125                        '. Premere "Invio" per forzare';
219300160125             Save3CLNRS = I54NrS;
219400160125             Save3CLLNP = I54Fil;
219500170320             ErrMessage  = *on;
219600170320             ErrGenerico = *on;
219700170320             leavesr;
219800170321           // se richiesta la forzatura
219900170321           WHEN  O54err = 'A'
220000170321                               and (I54Nrs=Save3CLNRS and
220100170321                                    I54Fil=Save3CLLNP)
220200170321            // ma il supporto non è SNA (logistica)
220300170321            and V2CCBA <> 'SNA';
220400170321             // emetto una window per una doppia sicurezza
220500170321             W3_3CLAttiva = *on;
220600170321             $VideoPrec = $Video;
220700170321             $Video = 'W3';
220800170321             clear  TB28W3;
220900170321             //      12345678901234567890123456789012345
221000170321             W3txt1='Serie già assegnata in tabella 3CL';
221100170321             W3txt2='ma tipo Supporto Cliente non SNA.';
221200170321             W3txt3='Procedere SOLO se necessario.';
221300170321             dou dsp_aid = c_F06
221400170321              or dsp_aid = c_F12;
221500170321               exfmt  TB28W3;
221600170321               select;
221700170321                when dsp_aid = c_F12;
221800170321                 //Dsp_Aid  = c_Enter ;
221900170321                 wForzatura = *off  ;
222000170321                 leave;
222100170321                when dsp_aid = c_F06;
222200170321                 wForzatura = *on   ;
222300170321                other;
222400170321                 wForzatura = *off  ;
222500170321               endsl;
222600170321             enddo;
222700170321             $Video = $VideoPrec;
222800170321             clear  $VideoPrec;
222900170321             clear  Save3CLNrS  ;
223000170321             clear  Save3CLLNP  ;
223100170321             W3_3CLAttiva = *off;
223200170321
223300160125             //select;
223400160125             //  when  $Video = 'D2';
223500160125             //    PosCurNRS   = *on;
223600160125             //  when  $Video = 'W1';
223700160125             //    PosCurW1nrs = *on;
223800160125             //endsl;
223900160125             //ErrMessage  = *on;
224000160125             //ErrGenerico = *on;
224100160125             //leavesr;
224200101014
224300101014           // ->?errore SQL?
224400101014           WHEN  O54err = 'S';
224500101014             VIDmsg = $Msg(18);
224600101014             select;
224700101014               when  $Video = 'D2';
224800101014                 PosCurNRS   = *on;
224900101014               when  $Video = 'W1';
225000101014                 PosCurW1nrs = *on;
225100101014             endsl;
225200101014             ErrMessage  = *on;
225300101014             ErrGenerico = *on;
225400101014             leavesr;
225500101014
225600101014           // ->?serie già presente come parziale?
225700110121           //   ?(non nel padre)?
225800110121           WHEN  O54err =  '1'      and
225900120131                 // (W1Cnrs <> SavNRS   or
226000120131                 //  W1Cncd <> SavNCD   or
226100120131                 //  W1Cnca <> SavNCA)  and
226200110121                 %lookup( %int(O54ksc) : sch_SKC ) = *zero;
226300101014             if  O54dal >  1   or   O54al <  *hival;
226400101014               VIDmsg = %trimr($Msg(19)) + ' ' + O54ksc;
226500101014             else;
226600101014               VIDmsg = %trimr($Msg(21)) + ' ' + O54ksc;
226700101014             endif;
226800101014             select;
226900101014               when  $Video = 'D2';
227000101014                 PosCurNRS   = *on;
227100101014               when  $Video = 'W1';
227200101014                 PosCurW1nrs = *on;
227300101014             endsl;
227400101014             ErrMessage  = *on;
227500101014             ErrGenerico = *on;
227600101014
227700101014             // ->?errore forzabile SE con stesso padre?
227800110121             //   ?(NON più: errore scartato all'inizio)?
227900110121             //if  %lookup( %int(O54ksc) : sch_SKC ) > *zero;
228000110121             //  SavNRS = W1Cnrs;
228100110121             //  SavNCD = W1Cncd;
228200110121             //  SavNCA = W1Cnca;
228300110121             //  VIDmsg = %trimr(VIDmsg) +
228400110121             //          '. Premere "Invio" x forzare';
228500110121             //endif;
228600101014             leavesr;
228700120113
228800120113           // ->?Serie NON ammessa (99)?
228900120113           WHEN  O54err = '9';
229000120113             VIDmsg = $Msg(43);
229100120113             select;
229200120113               when  $Video = 'D2';
229300120113                 PosCurNRS   = *on;
229400120113               when  $Video = 'W1';
229500120113                 PosCurW1nrs = *on;
229600120113             endsl;
229700120113             ErrMessage  = *on;
229800120113             ErrGenerico = *on;
229900120113             leavesr;
230000101014
230100101014           // ->?verifica se serie già presente come completa;?
230200101014           OTHER;
230300120104             //If  O54prz = *blank;
230400120104             If  ( (V2Dfls  > *zero  and  ParFIL <> %int(V2Dfls))   or
230500120202                   (V2Dfls <= *zero  and  ParFIL <> V2Ccli / 10000) or
230600120202                  ParNRS <> W1Cnrs   or
230700120202                  ParERR <> *blank )   and
230800120202                 (W1Cncd = 1  and  W1Cnca = 9999999);
230900101014               clear  TRTB28ds2;
231000101014               if  V2Dfls > *zero;
231100101014                 ParFIL = %int(V2Dfls);
231200101014               else;
231300101014                 ParFIL = V2Ccli / 10000;
231400101014               endif;
231500101014               ParNRS = W1Cnrs;
231600101014               ParELB = 'C';
231700101014               ParKSC = %editc( V2Ccli : 'X' );
231800111215               trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
231900101014
232000120113               select;
232100120113
232200120113                 // ->?Serie NON ammessa (99)?
232300120113                 when  ParERR = '9';
232400120113                   VIDmsg = $Msg(43);
232500120113                   select;
232600120113                     when  $Video = 'D2';
232700120113                       PosCurNRS   = *on;
232800120113                     when  $Video = 'W1';
232900120113                       PosCurW1nrs = *on;
233000120113                   endsl;
233100120113                   ErrMessage  = *on;
233200120113                   ErrGenerico = *on;
233300120113                   leavesr;
233400120113
233500120113                 // ->?errore bloccante SE con padre diverso?
233600120113                 // ->?errore forzabile SE con stesso padre?
233700120113                 //   ?(NON più: ora solo se con padre diverso)?
233800120113                 when  ParERR =  'E'      and
233900120131                       // (W1Cnrs <> SavNRS   or
234000120131                       //  W1Cncd <> SavNCD   or
234100120131                       //  W1Cnca <> SavNCA)  and
234200120113                       %lookup( %int(OUTksc) : sch_SKC ) = *zero;
234300120113                   VIDmsg = %trimr($Msg(21)) + ' ' + OUTksc;
234400120113                   select;
234500120113                     when  $Video = 'D2';
234600120113                       PosCurNRS   = *on;
234700120113                     when  $Video = 'W1';
234800120113                       PosCurW1nrs = *on;
234900120113                   endsl;
235000120113                   ErrMessage  = *on;
235100120113                   ErrGenerico = *on;
235200120113                   // ->?errore forzabile SE con stesso padre?
235300120113                   //if  %lookup( %int(OUTksc) : sch_SKC ) > *zero;
235400120113                   //  SavNRS = W1Cnrs;
235500120113                   //  SavNCD = W1Cncd;
235600120113                   //  SavNCA = W1Cnca;
235700120113                   //  VIDmsg = %trimr(VIDmsg) +
235800120113                   //          '. Premere "Invio" per forzare';
235900120113                   //endif;
236000120113                   leavesr;
236100120131
236200120131                 // ->?Serie già assegnata come completa (in tab. "3CL")?
236300120131                 //when  ParERR = 'A';
236400120131                 //  //$FlsNrsIn3CL = *on;
236500120131                 //  ErrGenerico = *on;
236600120131                 //  leavesr;
236700160125                 when  O54err = 'A'
236800160125                                     and (I54Nrs<>Save3CLNRS or
236900160125                                          I54Fil<>Save3CLLNP)   ;
237000160125                 //when  O54err = 'A'  and  (W1Cncd > 1  or
237100160125                 //                          W1Cnca < *hival)
237200160125                   VIDmsg = $Msg(20);
237300160125                   VIDmsg = %trimr(VIDmsg) +
237400160125                             '. Premere "Invio" per forzare';
237500160125                   Save3CLNRS = I54NrS;
237600160125                   Save3CLLNP = I54Fil;
237700170320                   ErrMessage  = *on;
237800170320                   ErrGenerico = *on;
237900170320                   leavesr;
238000160125                   //select;
238100160125                   //  when  $Video = 'D2';
238200160125                   //    PosCurNRS   = *on;
238300160125                   //  when  $Video = 'W1';
238400160125                   //    PosCurW1nrs = *on;
238500160125                   //endsl;
238600160125                   //ErrMessage  = *on;
238700160125                   //ErrGenerico = *on;
238800160125                   //leavesr;
238900120113
239000120113               endsl;
239100120131
239200120131               // ->?Serie già assegnata come completa (in tab. "3CL")?
239300120131               //   ?- rilevata dal *pgm TNTB54R -?
239400120131               //   ?(da segnalare in assenza di altri errori)?
239500120131               //if  O54err = 'A';
239600120131               //  //$FlsNrsIn3CL = *on;
239700120131               //  ErrGenerico = *on;
239800120131               //  leavesr;
239900120131               //endif;
240000101014
240100120104             EndIf;
240200101014
240300101014         ENDSL;
240400121112
240500121220         // -?SE modificato il supporto Cli-BRT (da o in "ESWEB")?
240600121112         //  ?occorre cambiare range?
240700121112         //  ?(lasciando il "vecchio" "bloccato" in tab. "3EW")?
240800121220         If  (Old_§3Ccba =  c_EasyWEB   and
240900121220              V2Ccba     <> c_EasyWEB    or
241000121220              Old_§3Ccba <> c_EasyWEB   and
241100121220              V2Ccba     =  c_EasyWEB)  AND
241200121220             (Old_§3Ccba =  c_EasyWEB         and
241300121220             ($Video     =  'D2'              and
241400121220              V2Cnrs     =  d3EW_Old.§3EWnrs  and
241500121220              V2Cncd     =  d3EW_Old.§3EWnsi  and
241600121220              V2Cnca     =  d3EW_Old.§3EWnsf  or
241700121220              $Video     =  'W1'              and
241800121220              W1Cnrs     =  d3EW_Old.§3EWnrs  and
241900121220              W1Cncd     =  d3EW_Old.§3EWnsi  and
242000121220              W1Cnca     =  d3EW_Old.§3EWnsf)  OR
242100121220              Old_§3Ccba <> c_EasyWEB         and
242200121220             ($Video     =  'D2'              and
242300121220              V2Cnrs     =  Save_3CP_nrs      and
242400121220              V2Cncd     =  Save_3CP_nsi      and
242500121220              V2Cnca     =  Save_3CP_nsf      or
242600121220              $Video     =  'W1'              and
242700121220              W1Cnrs     =  Save_3CP_nrs      and
242800121220              W1Cncd     =  Save_3CP_nsi      and
242900121220              W1Cnca     =  Save_3CP_nsf));
243000121128           //VIDmsg = $Msg(45);
243100121128           VIDmsg = $Msg(44);
243200121112           select;
243300121112             when  $Video = 'D2';
243400121112               PosCurNRS   = *on;
243500121112             when  $Video = 'W1';
243600121112               PosCurW1nrs = *on;
243700121112           endsl;
243800121112           ErrMessage  = *on;
243900121112           ErrGenerico = *on;
244000121112           leavesr;
244100121112         EndIf;
244200121127
244300121128         // -?ULTIMO CONTROLLO (BYPASSABILE) in fase di ricerca range?
244400121128         //  ?parziale: Range "anomalo"?
244500140718         //IF  $Video =  'W1'   and
244600140718         //    ( W1Cnrs <> SavW1Cnrs  or  W1Cncd <> SavW1Cncd
244700140718         //                           or  W1Cnca <> SavW1Cnca )  and  (
244800140718         //    %subst( %editc( W1Cncd : 'X' ) : %len(W1Cncd) : 1 ) <> '1'
244900140718         //                         or  (
245000140718         //    %subst( %editc( W1Cnca : 'X' ) : %len(W1Cnca) : 1 ) <> '0'
245100140718         //                        and  W1Cnca <> *hival ) );
245200140718         //
245300140718         //  SavW1Cnrs = W1Cnrs;
245400140718         //  SavW1Cncd = W1Cncd;
245500140718         //  SavW1Cnca = W1Cnca;
245600140718         //
245700140718         //  Select;
245800140718         //
245900140718         //    When  %subst(%editc(W1Cncd:'X'):%len(W1Cncd):1) <> '1' and
246000140718         //          %subst(%editc(W1Cnca:'X'):%len(W1Cnca):1) <> '0' and
246100140718         //          W1Cnca <> *hival;
246200140718         //      VIDmsg = %trimr($Msg(46)) +
246300140718         //      //       ' non comincia con "...1" +
246400140718         //      //         né termina con "...0". +
246500140718         //               ' non inizia con ' +
246600140718         //               %subst(%editc(W1Cncd:'X'):1:%len(W1Cncd)-1)+'1'
246700140718         //             + ' né termina con';
246800140718         //      wW1Cncx = W1Cnca + 1;
246900140718         //      if  %subst(%editc(wW1Cncx:'X'):%len(wW1Cncx)-1:2) = '00';
247000140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
247100140718         //               %subst(%editc(wW1Cncx:'X'):1:%len(wW1Cncx));
247200140718         //      else;
247300140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
247400140718         //               %subst(%editc(W1Cnca:'X'):1:%len(W1Cnca)-1)+'0';
247500140718         //      endif;
247600140718         //      VIDmsg = %trimr(VIDmsg) + '. Invio.';
247700140718         //      PosCurW1ncd = *on;
247800140718         //
247900140718         //    When  %subst(%editc(W1Cncd:'X'):%len(W1Cncd):1) <> '1';
248000140718         //      VIDmsg = %trimr($Msg(46)) +
248100140718         //      //       ' dovrebbe cominciare con "...1". +
248200140718         //               ' dovrebbe cominciare con ' +
248300140718         //               %subst(%editc(W1Cncd:'X'):1:%len(W1Cncd)-1)+'1'
248400140718         //             + '. Premere Invio.';
248500140718         //      PosCurW1ncd = *on;
248600140718         //
248700140718         //    When  %subst(%editc(W1Cnca:'X'):%len(W1Cnca):1) <> '0' and
248800140718         //          W1Cnca <> *hival;
248900140718         //      VIDmsg = %trimr($Msg(46)) +
249000140718         //      //       ' dovrebbe terminare con "...0". +
249100140718         //               ' dovrebbe terminare con';
249200140718         //      wW1Cncx = W1Cnca + 1;
249300140718         //      if  %subst(%editc(wW1Cncx:'X'):%len(wW1Cncx)-1:2) = '00';
249400140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
249500140718         //               %subst(%editc(wW1Cncx:'X'):1:%len(wW1Cncx));
249600140718         //      else;
249700140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
249800140718         //               %subst(%editc(W1Cnca:'X'):1:%len(W1Cnca)-1)+'0';
249900140718         //      endif;
250000140718         //      VIDmsg = %trimr(VIDmsg) + '.  Premere Invio.';
250100140718         //      PosCurW1nca = *on;
250200140718         //
250300140718         //  EndSl;
250400140718         //
250500140718         //  ErrMessage  = *on;
250600140718         //  ErrGenerico = *on;
250700140718         //  leavesr;
250800140718         //
250900140718         //ENDIF;
251000101014
251100101014       ENDSR;
251200091105
251300091105       //--------------------------------------------------------------
251400091105       //?Gestione videata D03.                                        ?
251500091105       //--------------------------------------------------------------
251600091105       BEGSR  sr_GesD03;
251700091105
251800091126         // -?Inizializzazione videata?
251900091105         if  $InzD02 = *on;
252000091105           exsr  sr_InzD02;
252100091105           $InzD02 = *off;
252200091105         endif;
252300091120
252400091126         // -?(Dis)Abilitazione tasti funzionali?
252500091120         exsr  sr_AbilFxxD02;
252600091105
252700091126         // -?Emissione Testata e Piede?
252800091127         write  TB28T1;
252900091127         write  TB28P2;
253000091105
253100091126         // -?Emissione videata?
253200091105         if  Not $Protect;
253300091127           exfmt  TB28D3;
253400091105         else;
253500091127           write  TB28D3;
253600091105           exfmt  Protect;
253700091105         endif;
253800091120
253900091105         reset  ErrMessage;
254000091105         reset  ErrGenerico;
254100091105         clear  VIDmsg;
254200091120
254300091105         SELECT;
254400091105
254500091126           // -?F3=Fine?
254600091105           WHEN  dsp_aid = c_F03;
254700091105             unlock  TABEL00F;
254800091105             unlock  TNTBE01L;
254900091105             $Fine = *on;
255000091105
255100091126           // -?F10=Ricerca Serie (globale)?
255200091109           WHEN  dsp_aid = c_F10;
255300091109             exsr  sr_F10D02;
255400091105
255500091126           // -?F11=Ricerca Serie Parziale?
255600091109           WHEN  dsp_aid = c_F11;
255700091109             $VideoPrec = $Video;
255800091109             $Video     = 'W1';
255900091109             reset  $InzW01;
256000091105
256100091126           // -?F12=Ritorno?
256200091105           WHEN  dsp_aid = c_F12;
256300150319             $Video = 'D2';
256400150319             clear  $VideoPrec;
256500091105
256600091126           // -?F19=Gestione multimembro clienti VAB?
256700091126           // -?F20=Gestione scambio dati principali     con clienti?
256800091126           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
256900091126           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
257000091105           WHEN  dsp_aid = c_F19   or
257100091105                 dsp_aid = c_F20   or
257200091105                 dsp_aid = c_F21   or
257300091105                 dsp_aid = c_F22;
257400091105             exsr  sr_FxxD02;
257500091105
257600091126           // -?RollDown=Videata precedente?
257700091105           WHEN  dsp_aid = c_RollDown;
257800150319             $Video = 'D2';
257900150319             clear  $VideoPrec;
258000091105
258100091126           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
258200091105           OTHER;
258300091126             // -?Controlli eseguiti NON se F16=Annullamento?
258400091105             if  dsp_aid <> c_F16;
258500091105               exsr sr_CtrD03;
258600091105               if  Not ErrGenerico;
258700091105                 exsr sr_CtrD02;
258800091106                 if  ErrGenerico = *on;
258900091106                   $Video = 'D2';
259000091106                 endif;
259100091105               endif;
259200091105               if  ErrGenerico = *on;
259300091105                 leavesr;
259400091105               endif;
259500091105             endif;
259600091126             // -?Aggiornamento?
259700091105             if  dsp_aid = c_F05   or
259800091105                 dsp_aid = c_F06   or
259900091105                 dsp_aid = c_F16;
260000091105               exsr  sr_UpdateAll;
260100091105               if  ErrGenerico = *on;
260200091105                 leavesr;
260300091105               endif;
260400091126               // -?Uscita dal programma SE richiamato?
260500091105               if  $Called = *on   and   W0Ccli > *zero;
260600091105                 $Fine = *on;
260700091126               // -?Videata iniziale altrimenti?
260800091105               else;
260900091105                 $Video  = 'D1';
261000091105                 $InzD01 = *on;
261100091105               endif;
261200091105             endif;
261300091105
261400091105         ENDSL;
261500091105
261600091126         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
261700091105         if  $Video = 'D1';
261800091105           clear  V1Topz;
261900091105           clear  BloAnn;
262000091120           VisBloAnn = *off;
262100091105         endif;
262200091105
262300091105       ENDSR;
262400091105
262500091104       //--------------------------------------------------------------
262600091104       //?Controllo dati immessi nella videata D03.                    ?
262700091104       //--------------------------------------------------------------
262800091104       BEGSR  sr_CtrD03;
262900091104
263000091105         %subst(IndDspF : 71) = *off;
263100091104
263200091126         // -?Dati per accorpamento bolle?
263300091126         if  V2Cabd <> *blank;
263400091126           // ->?Lunghezza riferimento originale mittente obbligatoria?
263500091104           if  V2Clrm <= *zero   and   V2Calm <> *blank;
263600091125             VIDmsg = $Msg(24);
263700091104             PosCurLRM   = *on;
263800091104             ErrMessage  = *on;
263900091104             ErrGenerico = *on;
264000091104             leavesr;
264100091104           endif;
264200091104         else;
264300091126           // ->?Controllo campi subordinati all'accorpamento bolle?
264400091104           select;
264500091104             when  V2Clrm <> *zero;
264600091125               VIDmsg = $Msg(25);
264700091104               PosCurLRM   = *on;
264800091104               ErrMessage  = *on;
264900091104               ErrGenerico = *on;
265000091104               leavesr;
265100091104             when  V2Calm <> *blank;
265200091125               VIDmsg = $Msg(25);
265300091104               PosCurALM   = *on;
265400091104               ErrMessage  = *on;
265500091104               ErrGenerico = *on;
265600091104               leavesr;
265700091104             when  V2Cnot <> *blank;
265800091125               VIDmsg = $Msg(25);
265900091104               PosCurNOT   = *on;
266000091104               ErrMessage  = *on;
266100091104               ErrGenerico = *on;
266200091104               leavesr;
266300091104             when  V2Cabc <> *blank;
266400091125               VIDmsg = $Msg(25);
266500091104               PosCurABC   = *on;
266600091104               ErrMessage  = *on;
266700091104               ErrGenerico = *on;
266800091104               leavesr;
266900091104             when  V2Carc <> *blank;
267000091125               VIDmsg = $Msg(25);
267100091104               PosCurARC   = *on;
267200091104               ErrMessage  = *on;
267300091104               ErrGenerico = *on;
267400091104               leavesr;
267500091104             when  V2Cavx <> *blank;
267600091125               VIDmsg = $Msg(25);
267700091104               PosCurAVX   = *on;
267800091104               ErrMessage  = *on;
267900091104               ErrGenerico = *on;
268000091104               leavesr;
268100091106           endsl;
268200091106         endif;
268300091102
268400091102       ENDSR;
268500091125
268600091125       //--------------------------------------------------------------
268700091125       //?Ricerche.                                                    ?
268800091125       //--------------------------------------------------------------
268900091125       BEGSR  sr_Search;
269000091125
269100091125         select;
269200091125
269300120305           // -?Supporto Cliente a BRT?
269400091125           when  %scan( '?' : V2Ccba ) > *zero;
269500091125             clear  V2Ccba;
269600091125             clear  V2Dcba;
269700091125             reset  TIBS02ds;
269800120605             //T02mod = 'R';                //?(già così)?
269900120605             //T02cod = '3CS';              //?(già così)?
270000091125             T02sif = knsif;
270100091125             TNTBE_RicercaControllo (kpjba : TIBS02ds);
270200091125             if  T02err = *blank;
270300091125               V2Ccba = T02ke1;
270400091125               V2Dcba = %subst( T02uni : 1 : %len(V2Dcba) );
270500091126               // -?Controlli specifici sull'abilitazione ad EasyWEB?
270600091126               if  V2Ccba = c_EasyWEB;
270700091126                 exsr  sr_CtrlEasyWEB;
270800120725                 Reverse_3EW = ( Not $Inzd02   and
270900120725                               ( SaveFLS <> V2Dfls  or
271000120725                                 SaveNRS <> V2Cnrs  or
271100120725                                 SaveNCD <> V2Cncd  or
271200120725                                 SaveNCA <> V2Cnca  or
271300120725                                 SaveCTM <> V2Cctm ) );
271400091126                 if  ErrGenerico;
271500091126                   leavesr;
271600091126                 endif;
271700110311               else;
271800110311                 exsr  sr_CtrlRngPadre;
271900110422                 // -?Qui NON serve: viene COMUNQUE acceso dopo!?
272000110422                 //if  ErrGenerico;
272100110422                 //  clear  ErrGenerico;
272200110422                 //endif;
272300091126               endif;
272400091125             else;
272500091125               errMessage = *on;
272600091125               VIDmsg = T02msg;
272700091125             endif;
272800091125             errGenerico = *on;
272900091125             PosCurCBA   = *on;
273000091125             leavesr;
273100091125
273200091126           // -?Codice Trattamento Merce?
273300091125           when  %scan( '?' : V2Cctm ) > *zero;
273400091125             clear  V2Cctm;
273500091125             clear  V2Dctm;
273600091125             X§Tkut = c_Kut;
273700091125             X§Tcod = '1B';
273800091125             clear  X§Tkey;
273900091125             clear  X§Tdes;
274000091125             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
274100091125             V2Cctm = %subst(X§Tkey : 1 : %len(V2Cctm));
274200091125             ErrGenerico = *on;
274300091125             leavesr;
274400091125
274500091125         endsl;
274600091125
274700091125       ENDSR;
274800091124
274900091124       //--------------------------------------------------------------
275000091124       //?Controllo/Decodifica Cliente Unificante.                     ?
275100091124       //--------------------------------------------------------------
275200091126       BEGSR  sr_CtrlCKS;
275300091124
275400091124         //clear  V2Dcks;         (NO perchè é i/o !!!)
275500110311         clear  wNSI_Padre;
275600110311         clear  d3CP_Padre;
275700110311         clear  d3EW_Padre;
275800110421         reset  SavCliTab3CPEW;
275900091124
276000091124         SELECT;
276100091124
276200091126           // ->?Ricerca alfabetica?
276300091124           WHEN  V2Ccks = *zero   and   V2Dcks <> *blank;
276400091124             xParDut = RSut;
276500091124             xParKut = 1;
276600091124             xParRag = V2Dcks;
276700091124             xParKcc = DUTkci;
276800091124             xParSta = 9;
276900091124             xParFlr = %editc( V1Cfil : 'X');
277000091124             clear  xParDit;
277100091124             xParNum = 1;
277200091124             clear  xParKcm;
277300091124             clear  xParKsm;
277400091124             clear  xParKdm;
277500091124             clear  xParEsci;
277600091124             clear  xParErr;
277700091124             clear  xParIva;
277800091124             clear  xParCdf;
277900091124             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
278000091124                        xParSta : xParFlr : xParDit : xParNum :
278100091124                        xParKcm : xParKsm : xParKdm : xParEsci :
278200091124                        xParErr : xParIva : xParCdf );
278300091124             if  xParSta <> -1   and   xParErr = *blank;
278400091124               V2Ccks = %int( %subst( xParKsm : 1 : %len(V2Ccks) ) );
278500091124               V2Dcks = xParRag;
278600091124             endif;
278700091124             ErrGenerico = *on;
278800091124             leavesr;
278900091124
279000091126           // ->?Non inserito?
279100091124           WHEN  V2Ccks = *zero;
279200091124             VIDmsg = $Msg(06);
279300091124             PosCurCKS   = *on;
279400091124             ErrMessage  = *on;
279500091124             ErrGenerico = *on;
279600091124             leavesr;
279700091124
279800091126           // ->?Controllo / Decodifica?
279900091124           OTHER;
280000091126             if  V2Ccks <> I69kac;
280100091126               clear  TIBS69ds;
280200091126               I69sif = knsif;
280300091126               I69kcc = DUTkci;
280400091126               I69kac = V2Ccks;
280500091126               tibs69r( tibs69ds :
280600091126                        ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
280700091126             endif;
280800091124             if  O69err = *on;
280900091124               VIDmsg = $Msg(07);
281000091124               PosCurCKS   = *on;
281100091124               ErrMessage  = *on;
281200091124               ErrGenerico = *on;
281300091124               leavesr;
281400091124             endif;
281500091124             V2Dcks = %subst( ACOrag : 1 : %len(V2Dcks) );
281600091124
281700091126             // -?SE diverso da V2Ccli: controllo se possibile?
281800091124             clear  ds3C_padre;
281900091124             If  V2Ccks <> V2Ccli;
282000091124
282100100203               // -?Cliente unificante NON in tab. "3C"?
282200091124               if  getTabella ( c_Tabel : '3C' : %editc(V2Ccks:'X') :
282300091124                                *omit : *omit : *omit :
282400091124                                *omit : *omit :
282500091124                                *omit : *omit : *omit : *omit :
282600091124                                *omit : *omit : *omit : *omit :
282700091124                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
282800091124                              < *zero       OR
282900091124                   ds_TNTBE.TBEatb <> *blank;
283000091124                 VIDmsg = $Msg(08);
283100091124                 PosCurCKS   = *on;
283200091124                 ErrMessage  = *on;
283300091124                 ErrGenerico = *on;
283400091124                 leavesr;
283500091124               else;
283600120214                 ds3C_Padre = ds_TNTBE.TBEuni;
283700091124               endif;
283800100203
283900100203               // -?Cliente unificante già figlio?
284000100203               if  V2Ccks <> p_§3Ccks;
284100100203                 VIDmsg = %trimr($Msg(07)) + ': anch''esso è figlio';
284200100203                 PosCurCKS   = *on;
284300100203                 ErrMessage  = *on;
284400100203                 ErrGenerico = *on;
284500100203                 leavesr;
284600100203               endif;
284700091124
284800100917               // -?Cliente unificante a video diverso da quello già?
284900100917               //  ?memorizzato: verifica fattibilità (SE il vecchio?
285000120215               //  ?era padre di se stesso: NON deve avere figli)?
285100110302               if  %found(TABEL00F)   and
285200100917                   V2Ccks <> §3Ccks   and   V2Ccli = §3Ccks;
285300120605
285400100917                 // -?Padre con figli: NON può diventare figlio?
285500120517                 if  sch_SKC_Orig(2) > *zero;
285600100917                   VIDmsg = %trimr($Msg(09)) + ' ' +
285700120517                      %editc( sch_SKC_Orig(2) : 'X' );
285800120517                   if  sch_SKC_Orig(3) > 1;
285900100917                     VIDmsg = %trimr(VIDmsg) + ' ed altri';
286000100917                   endif;
286100100917                   PosCurCKS   = *on;
286200100917                   ErrMessage  = *on;
286300100917                   ErrGenerico = *on;
286400100917                   leavesr;
286500100917                 endif;
286600120214                 // -?Padre già abilitato ad EasySped-WEB: non può?
286700120214                 //  ?diventare figlio?
286800120215                 if  Not  $3EWupd         and
286900120215                     §3Ccba =  c_EasyWEB  and
287000120214                     §3Ccks <> V2Ccks     and
287100120605                     %editc( §3Ccks : 'X' ) =
287200120605                       %subst( TIVSSds_Orig.VSSksu : 2 );
287300120215                   VIDmsg = %trimr( $Msg(32) ) + ' ' +
287400120215                            %editc( §3Ccks : 'X' );
287500120214                   PosCurCKS   = *on;
287600120214                   ErrMessage  = *on;
287700120214                   ErrGenerico = *on;
287800120214                   leavesr;
287900120214                 endif;
288000091214               endif;
288100091124
288200091124             Else;
288300091124
288400091124               ds3C_Padre = ds3C;
288500091124
288600091124             EndIf;
288700091124
288800091124         ENDSL;
288900091124
289000100312         // -?Reperimento chiave completa per tab. "3EW" alla?
289100100312         //  ?variazione del cliente unificante?
289200100312         //  ?se padre non trovato nel file TIVSS non può essere in?
289300100312         //  ?  tab. "3EW"?
289400120725         //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
289500120725         if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
289600101028           clear  rqsSUN;
289700101026           exsr  sr_TIS7701_x_TIVSS;
289800100312         endif;
289900100322
290000100322         // -?Reperimento famiglia completa?
290100120104         exsr  sr_ubLeg3C;
290200091124
290300091124       ENDSR;
290400091106
290500091106       //--------------------------------------------------------------
290600120305       //?Controllo/Decodifica Supporto Cliente a BRT.                 ?
290700091106       //--------------------------------------------------------------
290800091106       BEGSR  sr_CtrlCBA;
290900091106
291000091106         clear  V2Dcba;
291100091106
291200091126         // ->?Vuoto?
291300091125         if  V2Ccba = *blank;
291400091125
291500091125           if  V2Ccks = V2Ccli;
291600091125             VIDmsg = $Msg(11);
291700091125             PosCurCBA   = *on;
291800091125             ErrMessage  = *on;
291900091125             ErrGenerico = *on;
292000091125             leavesr;
292100091125           endif;
292200091106
292300100211         // ->?Controllo / Decodifica?
292400091125         else;
292500091125           ds_TNTBE.TBEke1 = V2Ccba;
292600091125           if  getTabella ( c_Tntbe : '3CS' : ds_TNTBE.TBEke1 :
292700091125                            *omit   : *omit : *omit :
292800091125                            *omit : *omit :
292900091125                            *omit : *omit : *omit : *omit :
293000091125                            *omit : *omit : *omit : *omit :
293100091125                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
293200091125                          = *zero      AND
293300091125               ds_TNTBE.TBEatb = *blank;
293400091125             V2Dcba = %subst( ds_TNTBE.TBEuni : 1 : %len( V2Dcba ) );
293500091125           else;
293600091127             V2Dcba = *all'? ';
293700091125             VIDmsg = $Msg(12);
293800091125             PosCurCBA   = *on;
293900091125             ErrMessage  = *on;
294000091125             ErrGenerico = *on;
294100091125             leavesr;
294200091125           endif;
294300091106
294400091125         endif;
294500091124
294600091126         // -?Controlli specifici sull'abilitazione ad EasyWEB?
294700091125         if  V2Ccba = c_EasyWEB;
294800091126           exsr  sr_CtrlEasyWEB;
294900120725           Reverse_3EW = ( Not $Inzd02   and
295000120725                         ( SaveFLS <> V2Dfls  or
295100120725                           SaveNRS <> V2Cnrs  or
295200120725                           SaveNCD <> V2Cncd  or
295300120725                           SaveNCA <> V2Cnca  or
295400120725                           SaveCTM <> V2Cctm ) );
295500091124           if  ErrGenerico;
295600091124             leavesr;
295700091124           endif;
295800100211         else;
295900100211           exsr  sr_Ctrl3EW;
296000100211           if  ErrGenerico;
296100100211             leavesr;
296200100211           endif;
296300091124         endif;
296400091106
296500091106       ENDSR;
296600091124
296700091124       //--------------------------------------------------------------
296800091124       //?Controlli specifici sull'abilitazione ad EasyWEB             ?
296900120215       //?(se messo/lasciato supporto EasySped-WEB)                    ?
297000150127       //--------------------------------------------------------------
297100091126       BEGSR  sr_CtrlEasyWEB;
297200150127
297300150127         V23EWLnP = *blank;
297400091127
297500120305         // -?"Supporto Cliente a BRT" diverso da quello del "PADRE"?
297600091127         if  V2Ccks <> V2Ccli   and   p_§3Ccba <> c_EasyWEB;
297700091127           VIDmsg = %trimr($Msg(13)) + c_EasyWEB + '": "' +
297800091127                    p_§3Ccba + '"';
297900091127           PosCurCKS   = *on;
298000091127           ErrMessage  = *on;
298100091127           ErrGenerico = *on;
298200091127           leavesr;
298300091127         endif;
298400091124
298500091126         // -?Controllo abilitazione a STRATEGI del cliente "PADRE"?
298600120725         //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
298700120725         if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
298800101028           clear  rqsSUN;
298900101026           exsr  sr_TIS7701_x_TIVSS;
299000100312         endif;
299100120725         //select;
299200120725         //  when  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
299300120725         //                            or   TIVSSds.VSSksu = *zero;
299400120725         //    VIDmsg = $Msg(14);
299500120725         //    if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
299600120725         //        V2Cncd = 1          and  V2Cnca = *all'9';
299700120725         //      //F13Attivo = *on;       ?(già così)?
299800120725         //      //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
299900120725         //      VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
300000120725         //    endif;
300100120725         //    PosCurCKS   = *on;
300200120725         //    ErrMessage  = *on;
300300120725         //    ErrGenerico = *on;
300400120725         //    leavesr;
300500120725         //  // -?Abilitazione scaduta?
300600120725         //  //  ?(IMPOSSIBILE: TIS7701R avrebbe restituito errore!)?
300700120725         //  //when  TIVSSds.VSSdsc < wDate;
300800120725         //  //  VIDmsg = $Msg(37);
300900120725         //  //  PosCurCBA   = *on;
301000120725         //  //  ErrMessage  = *on;
301100120725         //  //  ErrGenerico = *on;
301200120725         //  //  leavesr;
301300120725         //endsl;
301400091125
301500100212
301600100212         // -Azioni specifiche per cliente abilitato ad EasyWeb?
301700100212
301800100212         IF  $EasyWeb = *off;
301900100212
302000100212           $EasyWeb = *on;
302100100212
302200100212           // -?Protezione campi già impostati in tab. "3EW"?
302300100212           Protect_3EW = *on;
302400100212           // -?Reverse image x campi già impostati in tab. "3EW"?
302500120725           //  ?(SOTTO, solo se cambiati rispetto a quelli già?
302600120725           //   ?visualizzati)?
302700120725           //if  Not $Inzd02;
302800120725           //  Reverse_3EW = *on;
302900120725           //endif;
303000100212
303100100212           // -?Pulizia campi già impostati in tab. "3EW"?
303200120725           SaveFLS = V2Dfls;
303300150127           SaveLNP = V23EWLNP;
303400120725           SaveNRS = V2Cnrs;
303500120725           SaveNCD = V2Cncd;
303600120725           SaveNCA = V2Cnca;
303700120725           SaveCTM = V2Cctm;
303800120725           clear  V2Dfls;
303900150127           clear  V23EWLNP;
304000120725           clear  V2Cnrs;
304100120725           clear  V2Cncd;
304200120725           clear  V2Cnca;
304300120725           clear  V2Cctm;
304400120725           //clear  V2Dctm;
304500100212
304600100212           // -?Verifica se padre presente una sola volta in tab. "3EW"?
304700120725           ////if  V2Ccks <> SavCKS;
304800121015           //if  wCount_3EW     <= *zero                         or
304900121015           //    TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' )  or
305000121015           //    TIVSSds.VSSsun <= *zero;
305100120726             exsr  sr_Count3EW;
305200121015           //endif;
305300100212
305400120725           Select;
305500120725
305600120725             // -?Padre NON presente in tab. "3EW"?
305700120725             When  TBLflg <> *blank   and   wCount_3EW = *zero;
305800120725               reset  $EasyWeb;
305900120725               clear  V2Dctm;
306000120725               VIDmsg = $Msg(38);
306100120725               PosCurCKS   = *on;
306200120725               ErrMessage  = *on;
306300120725               ErrGenerico = *on;
306400120725               leavesr;
306500120816
306600120816             // -?Padre presente ma NON attivo in tab. "3EW"?
306700120816             When  wCount_3EW = *zero;
306800120816               // -?Abilitazione F13=Interrog. tab. "3EW"?
306900120816               if  Not  F13Attivo;
307000120816                 F13Attivo   = *on;
307100120816               endif;
307200120725
307300120725             // -?Reperimento dati del codice padre da tab. "3EW"?
307400120725             //  ?(se presente una sola volta)?
307500120725             When  wCount_3EW = 1;
307600120725               // -?Impostazione campi protetti con i valori del padre?
307700120725               if  d3EW.§3EWfls <> *zero;
307800120725                 V2Dfls = %editc( d3EW.§3EWfls : 'X' );
307900120725               endif;
308000150127               if  d3EW.§3EWLNP <> *zero;
308100150127                 V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
308200150127               endif;
308300120725               V2Cnrs = d3EW.§3EWnrs;
308400120725               V2Cncd = d3EW.§3EWnsi;
308500120725               V2Cnca = d3EW.§3EWnsf;
308600120725               V2Cctm = d3EW.§3EWctm;
308700140123               //if  V2Cctm <> SaveCTM;
308800140123               //  clear  V2Dctm;
308900140123               //endif;
309000140123               // -?"Forzature" richiestemi da Mirko C.?
309100140123               If  V2Ccba = c_EasyWEB;
309200140123                 // -?Supporto BRT a Cliente = "EMAIL"?
309300140123                 if  V2Ccli = V2Ccks;
309400140123                   V2Cbac = 'EMAIL';
309500140123                 endif;
309600140123                 // -?Abilitazione Serie in Bollettazione = "S"?
309700140123                 V2Cfg1 = 'S';
309800140123               EndIf;
309900120725               // -?AVVERTIMENTO forzatura dati dal padre?
310000120725               //   ·?filiale segnacolli?
310100120725               //   ·?numero serie segnacolli?
310200120725               //   ·?range segnacolli?
310300120725               //   ·?codice trattamento merce?
310400120927               if  Not $InzD02        and
310500120927                   V2Ccks  <> V2Ccli  and
310600120927                  (SaveFLS <> V2Dfls  or
310700150127                   SaveLNP <> V23EWLNP or
310800150127                   SaveNRS <> V2Cnrs  or
310900120927                   SaveNCD <> V2Cncd  or
311000120927                   SaveNCA <> V2Cnca  or
311100120927                   SaveCTM <> V2Cctm);
311200120725                 VIDmsg = $Msg(39);
311300120725                 ErrMessage  = *on;
311400120725                 ErrGenerico = *on;
311500120725                 leavesr;
311600120725               endif;
311700120725
311800120725             // -?AVVISO e consentita l'interrogazione della tab. "3EW"?
311900120725             //  ?(se presente più di una sola volta)?
312000120725             When  wCount_3EW > 1;
312100120725               // -?Impostazione campi protetti con i valori del padre?
312200120725               if  d3EW.§3EWfls <> *zero;
312300120725                 V2Dfls = %editc( d3EW.§3EWfls : 'X' );
312400120725               endif;
312500150127               if  d3EW.§3EWLNP <> *zero;
312600150127                 V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
312700150127               endif;
312800120725               V2Cnrs = d3EW.§3EWnrs;
312900120725               V2Cncd = d3EW.§3EWnsi;
313000120725               V2Cnca = d3EW.§3EWnsf;
313100120725               V2Cctm = d3EW.§3EWctm;
313200120725               if  V2Cctm <> SaveCTM;
313300120725                 clear  V2Dctm;
313400120725               endif;
313500120725               // -?Abilitazione F13=Interrog. tab. "3EW"?
313600120725               if  Not  F13Attivo;
313700120725                 F13Attivo   = *on;
313800120725                 VIDmsg = $Msg(40);
313900120725                 //PosCurNRS   = *on;
314000120725                 PosCurCKS   = *on;
314100120725                 ErrMessage  = *on;
314200120725                 ErrGenerico = *on;
314300120725                 leavesr;
314400120725               endif;
314500120725
314600120725           EndSl;
314700120725
314800120725           Reverse_3EW = ( Not $Inzd02   and
314900120725                         ( SaveFLS <> V2Dfls  or
315000150127                           SaveLNP <> V23EWLNP or
315100150127                           SaveNRS <> V2Cnrs  or
315200120725                           SaveNCD <> V2Cncd  or
315300120725                           SaveNCA <> V2Cnca  or
315400120725                           SaveCTM <> V2Cctm ) );
315500120725
315600120725         ENDIF;
315700120725
315800120725         If  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
315900120725                                 or   TIVSSds.VSSksu = *zero;
316000120725           VIDmsg = $Msg(14);
316100120725           if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
316200120725               V2Cncd = 1          and  V2Cnca = *all'9';
316300120725             //F13Attivo = *on;       ?(già così)?
316400120725             //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
316500120725             VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
316600120725           endif;
316700120725           PosCurCKS   = *on;
316800120725           ErrMessage  = *on;
316900120725           ErrGenerico = *on;
317000120725           leavesr;
317100120725         EndIf;
317200091124
317300091126         // -?Serie obbligatoria?
317400091124         if  V2Cnrs = *zero;
317500091130           VIDmsg = %trimr($Msg(15)) + ' "' + c_EasyWEB + '"';
317600120816           // -?Già abilitato F13=Interrog. tab. "3EW"?
317700120816           if  F13Attivo;
317800120816             VIDmsg = %trimr( VIDmsg ) + ': F13 per selezionare';
317900120816           endif;
318000091124           PosCurNRS   = *on;
318100091124           ErrMessage  = *on;
318200091124           ErrGenerico = *on;
318300091124           leavesr;
318400091124         endif;
318500091124
318600091124       ENDSR;
318700100211
318800100211       //--------------------------------------------------------------
318900120215       //?Verifica abilitazione cliente (SE unificante) in tab. "3EW"  ?
319000120215       //?(SE tolto supporto EasySped-WEB / lasciato altro)            ?
319100100211       //--------------------------------------------------------------
319200100211       BEGSR  sr_Ctrl3EW;
319300100407
319400120215         // -?Controlli da fare SOLO per cliente padre?
319500100407         if  V2Ccli <> V2Ccks;
319600100407           leavesr;
319700100407         endif;
319800101029
319900101029         // -?Se cliente Unificante presente con più codici Strategi,?
320000120725         //  ?occorre selezionare quello corretto?
320100120725         if  wCount_3EW < *zero;
320200120725           exsr  sr_Count3EW;
320300120725         endif;
320400101029         If  wCount_3EW > 1;
320500101029           $VideoPrec  = $Video;
320600101029           $Video      = 'W4';
320700101029           reset  $InzW04;
320800101029           ErrGenerico = *on;
320900101029           leavesr;
321000101029         EndIf;
321100100304
321200120305         // -?Abbandonare il "Supporto cliente a BRT" "ESWEB" è?
321300101216         //  ?impossibile se cliente padre con figli "ESWEB"?
321400101216         if  V2Ccli =  sch_SKC(1)   and   sch_SKC(2) > *zero      and
321500101216             V2Ccba <> c_EasyWeb    and   §3Ccba     = c_EasyWeb  and
321600101216             %lookup( *on : sch_SKEW : 2 ) > *zero;
321700100216           VIDmsg = $Msg(41);
321800100212           PosCurCBA   = *on;
321900100212           ErrMessage  = *on;
322000100212           ErrGenerico = *on;
322100100212           leavesr;
322200100212         endif;
322300100212
322400100211         // -?Reperimento chiave completa per tab. "3EW"?
322500100211         //  ?se cliente non trovato nel file TIVSS non può essere?
322600100304         //  ?  in tab. "3EW" - probabilmente è figlio (quindi OK)?
322700100215         //  ?(GIÀ FATTO NELLA SUBR. DI INIZIALIZZAZIONE "sr_InzD02")?
322800120725         //  ?=> PUÒ, PUÒ essere in tab. "3EW"...!?
322900120725         //  ?Potrebbe essere già stata fatta scadere l'abilitazione?
323000120725         //  ?ad EasySpedWEB (nel file TIVSS), lasciando "ESWEB" come?
323100120725         //  ?supporto qui in "3C": $Found_TIVSS sarebbe *off !!!?
323200120725         //  ?Ma i controlli qui successivi NON avrebbero senso.?
323300100312         if  NOT  $Found_TIVSS;
323400100215           leavesr;
323500100215         endif;
323600100312
323700120305         // -?Per abbandonare il "Supporto cliente a BRT" "ESWEB"?
323800100312         //  ?occorre che l'applicazione risulti scaduta?
323900120215         //  ?=> OCCORRERÀ ANNULLARE LA TAB. "3EW" A MANO !!!?
324000120215         //                                      ?(da *pgm TNTB79R)?
324100120725         //  ?=> TIVSSds.VSSdsc non può essere < *date:?
324200120725         //     ?sarebbe $Found_TIVSS = *off !!!?
324300120725         //if  V2Ccba        <> c_EasyWeb   and
324400120725         //    §3Ccba         = c_EasyWeb   and
324500120725         //    TIVSSds.VSSdsc < wDate;
324600120725         //  leavesr;
324700120725         //endif;
324800100407
324900120215         // -?Verifica esistenza del padre in tab. "3EW"?
325000120215         //  ?Occorre PRIMA eliminare l'abilitazione a STRATEGI per?
325100120215         //  ?  EasySped-WEB del cliente;?
325200120215         //  ?poi (con questo *pgm, richiamato da TIS725R1) il supporto?
325300120215         //  ?  cliente a BRT sarà modificabile!
325400120215         if  Not  $3EWupd                                  AND
325500120215             getTabella ( c_Tntbe : '3EW' : TIVSSds.VSSksu :
325600100407                          TIVSSds.VSSsun : *blank : *blank :
325700100407                          *omit : *omit :
325800100407                          *omit : *omit : *omit : *omit :
325900100407                          *omit : *omit : *omit : *omit :
326000120814                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni ) = *zero
326100120814             AND  ds_TNTBE.TBEatb = *blank;
326200120814           d3EW_Temp = ds_TNTBE.TBEuni;
326300120814           if  d3EW_Temp.§3EWFAA = *blank;
326400120814             VIDmsg = $Msg(32);
326500120814             PosCurCBA   = *on;
326600120814             ErrMessage  = *on;
326700120814             ErrGenerico = *on;
326800120814             leavesr;
326900120814           endif;
327000100407         endif;
327100100211
327200100211       ENDSR;
327300110311
327400110311       //--------------------------------------------------------------
327500110311       //?Verifica se range segnacolli del padre è diverso             ?
327600110311       //--------------------------------------------------------------
327700110311       BEGSR  sr_CtrlRngPadre;
327800110311
327900110311         F1Attivo = *off;
328000110311
328100110311         // -?Controlli da fare solo per cliente figlio?
328200110311         if  V2Ccli = V2Ccks;
328300110311           leavesr;
328400110311         endif;
328500110311
328600120305         // -?Controlli da fare solo per supporto cliente -> BRT?
328700110311         //  ?diverso da "ESWEB"?
328800110311         if  V2Ccba = c_EasyWEB;
328900110311           leavesr;
329000110311         endif;
329100110311
329200110311         // -?Padre "EasyWEB"?
329300110311         IF  p_§3Ccba = c_EasyWEB;
329400110311
329500110311           // -?Reperimento codice completo STRATEGI del cliente "PADRE"?
329600120725           //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
329700120725           if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
329800110311             clear  rqsSUN;
329900110311             exsr  sr_TIS7701_x_TIVSS;
330000110311           endif;
330100110311
330200110314           // -?Reperimento dati dal padre dalla tab. "3EW"?
330300110314           if  SavCliTab3CPEW <> %editc( V2Ccks : 'X' ) + '3EW';
330400110314             SavCliTab3CPEW = %editc( V2Ccks : 'X' ) + '3EW';
330500110311             if  getTabella ( c_Tntbe : '3EW' : TIVSSds.VSSksu :
330600110311                              TIVSSds.VSSsun : *blank : *blank :
330700110311                              *omit : *omit :
330800110311                              *omit : *omit : *omit : *omit :
330900110311                              *omit : *omit : *omit : *omit :
331000120605                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
331100120605                            = *zero;
331200110311               d3EW_Padre = ds_TNTBE.TBEuni;
331300110311             endif;
331400110311           endif;
331500110311
331600110311         // -?Padre NON "EasyWEB"?
331700110311         ELSE;
331800110311
331900110314           // -?Reperimento dati dal padre dalla tab. "3CP"?
332000110314           if  SavCliTab3CPEW <> %editc( V2Ccks : 'X' ) + '3CP';
332100110314             SavCliTab3CPEW = %editc( V2Ccks : 'X' ) + '3CP';
332200110311             k_TBEcod = '3CP';
332300110311             k_TBEke1 = %editc( V2Ccks : 'X' );
332400110311             setll  %kds( k05tntbe01 : 2 )  TNTBE000;
332500110311             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
332600110311             dow  Not %eof(TNTBE01L);
332700110311               if  %subst(TBEke2:1:2) = %editc(p_§3Cnrs:'X') and
332800110311                   TBEatb = *blank;
332900110311                 wNSI_Padre = %int( %subst( TBEke2 : 3 : 7 ) );
333000110311                 d3CP_Padre = TBEuni;
333100110311                 leave;
333200110311               endif;
333300110311               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
333400110311             enddo;
333500110311           endif;
333600110311
333700110311         ENDIF;
333800110311
333900110311
334000110311         // -?Abilitazione F1=Reperimento Serie e Segnacolli da Padre?
334100110421         if   p_§3Ccba   <> V2Ccba            OR
334200110421
334300110421              p_§3Cnrs   <> V2Cnrs            OR
334400110311
334500110421            ( p_§3Ccba   =  c_EasyWEB         and
334600110311             (d3EW_Padre.§3EWnsi <> V2Cncd    or
334700110311              d3EW_Padre.§3EWnsf <> V2Cnca) ) OR
334800110311
334900110421            ( p_§3Ccba   <> c_EasyWEB         and
335000110311             (wNSI_Padre <> V2Cncd            and
335100110311              wNSI_Padre <> *zero             and
335200110311              V2Cncd     <> 1)                or
335300110311             (d3CP_Padre.§3CPal  <> V2Cnca    and
335400110311              d3CP_Padre.§3CPal  <> *zero     and
335500110311              V2Cnca     <> *all'9') );
335600110311
335700110311           F1Attivo = *on;
335800110422           if  Not SavF1Attivo;
335900110422             ErrGenerico = *on;
336000110422           endif;
336100110311
336200110311         endif;
336300110311
336400110311       ENDSR;
336500120725
336600120725       //--------------------------------------------------------------
336700120725       //?Reperimento dati da tab. "3EW" del cliente e conteggio dei   ?
336800120725       //?record per lo stesso unificante.                             ?
336900120725       //--------------------------------------------------------------
337000120725       BEGSR  sr_Count3EW;
337100120725
337200120725         clear  d3EW;
337300120725         clear  wCount_3EW;
337400120725
337500120725         k_TBEcod = '3EW';
337600120725         k_TBEke1 = '0' + %editc( V2Ccks : 'X' );
337700120725         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
337800120725         reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
337900120725
338000120725         DoW  Not %eof(TNTBE01L);
338100120814
338200120814           d3EW_Temp = TBEuni;
338300120725
338400120814           if  (TBEatb = *blank   and   d3EW_Temp.§3EWfaa = *blank)   or
338500121119               TBLflg <> *blank   or
338600121119               (Not %found(TABEL00F)  and   d3EW_Temp.§3EWfaa <> *blank);
338700120725
338800120725             wCount_3EW += 1;
338900120725
339000120725             if  (TIVSSds.VSSsun <= *zero  and  wCount_3EW = 1)  or
339100120725                 TBEke2 = TIVSSds.VSSsun;
339200120725               d3EW = TBEuni;
339300120725             endif;
339400120725
339500120725           endif;
339600120725
339700120725           reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
339800120725
339900120725         EndDo;
340000120726
340100120726
340200120726         If  wCount_3EW = 1;
340300120726
340400120726           TNTB79ds.o3EWksu = TBEke1;
340500120726           TNTB79ds.o3EWsun = TBEke2;
340600120726           TNTB79ds.o3EWuni = d3EW;
340700120726           wCount_3EW = 1;
340800120726           rqsSUN = TNTB79ds.o3EWsun;
340900120726           exsr  sr_TIS7701_x_TIVSS;
341000120726           $Bypass_01 = *on;
341100120726
341200120726         EndIf;
341300120725
341400120725       ENDSR;
341500091124
341600091124       //--------------------------------------------------------------
341700091124       //?Controllo/Decodifica Codice Trattamento Merce.               ?
341800091124       //--------------------------------------------------------------
341900091124       BEGSR  sr_CtrlCTM;
342000091124
342100091124         clear  V2Dctm;
342200091124
342300091126         // ->?Controllo / Decodifica?
342400091125         ds_TNTBE.TBEke1 = V2Cctm;
342500091125         if  getTabella ( c_Tabel : '1B'  : ds_TNTBE.TBEke1 :
342600091125                          *omit   : *omit : *omit :
342700091125                          *omit : *omit :
342800091125                          *omit : *omit : *omit : *omit :
342900091125                          *omit : *omit : *omit : *omit :
343000091125                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
343100091125                        = *zero      AND
343200091125             ds_TNTBE.TBEatb = *blank;
343300091125           ds1B   = ds_TNTBE.TBEuni;
343400091125           V2Dctm = §1Bdes;
343500091125         else;
343600091125           V2Dctm = *all'? ';
343700091125           VIDmsg = $Msg(16);
343800091125           PosCurCTM   = *on;
343900091125           ErrMessage  = *on;
344000091125           ErrGenerico = *on;
344100091125           leavesr;
344200091125         endif;
344300091124
344400091124       ENDSR;
344500091104
344600091104       //--------------------------------------------------------------
344700091104       //?Gestione videata W01.                                        ?
344800091104       //--------------------------------------------------------------
344900091104       BEGSR  sr_GesW01;
345000091104
345100091126         // -?Inizializzazione window?
345200091104         if  $InzW01 = *on;
345300091127           clear  TB28W1;
345400091126           $InzW01 = *off;
345500091104         endif;
345600091104
345700091126         // -?Emissione window?
345800091104         if  Not $Protect;
345900091127           exfmt  TB28W1;
346000091104         else;
346100091127           write  TB28W1;
346200091104           exfmt  Protect;
346300091104         endif;
346400091104         reset  ErrMessage;
346500091104         reset  ErrGenerico;
346600091104         clear  VIDmsg;
346700091104
346800091126         // -?F12=Ritorno?
346900091104         if  dsp_aid = c_F12;
347000091104           $Video = $VideoPrec;
347100150319           clear  $VideoPrec;
347200091109           leavesr;
347300091104         endif;
347400091104
347500091126         // -?Invio?
347600140903         $CtrlRngSgnCl = *off;
347700091104         exsr  sr_CtrW01;
347800140718         if  $CtrlRngSgnCl;
347900140718           exsr  sr_CtrW01;
348000140718         endif;
348100091109         if  NOT ErrGenerico;
348200091109           // ->?Ritorno alla videata D02?
348300150319           $Video = 'D2';
348400150319           clear  $VideoPrec;
348500091109           leavesr;
348600091104         endif;
348700091104
348800091104       ENDSR;
348900091104
349000091104       //--------------------------------------------------------------
349100091104       //?Controllo dati immessi nella window W01.                     ?
349200091104       //--------------------------------------------------------------
349300091104       BEGSR  sr_CtrW01;
349400091104
349500091104         %subst(IndDspF : 81) = *off;
349600140903         //$CtrlRngSgnCl = *off;
349700091104
349800100407         select;
349900100407
350000100407           // -?Effettuare una selezione?
350100100407           when  W1Cncl = *zero   and   W1Cnrs = *zero;
350200100407             VIDmsg = $Msg(28);
350300100407             PosCurW1ncl = *on;
350400100407             ErrMessage  = *on;
350500100407             ErrGenerico = *on;
350600100407             leavesr;
350700100407
350800100407           // -?Selezionare Numero colli OPPURE Segnacollo da-al?
350900100407           when  W1Cncl <> *zero   and   W1Cnrs <> *zero;
351000100407             VIDmsg = $Msg(29);
351100100407             PosCurW1ncl = *on;
351200100407             ErrMessage  = *on;
351300100407             ErrGenerico = *on;
351400100407             leavesr;
351500100407
351600100407           // -?Controllo range segnacolli?
351700100407           when  W1Cnrs <> *zero   and   W1Cnca = *zero;
351800100407             VIDmsg = $Msg(30);
351900100407             PosCurW1nca = *on;
352000100407             ErrMessage  = *on;
352100100407             ErrGenerico = *on;
352200100407             leavesr;
352300100407           when  W1Cnrs <> *zero   and   W1Cncd > W1Cnca;
352400100407             VIDmsg = $Msg(31);
352500100407             PosCurW1ncd = *on;
352600100407             ErrMessage  = *on;
352700100407             ErrGenerico = *on;
352800100407             leavesr;
352900100407
353000100407         endsl;
353100100514
353200100514         // -?Sostituzione collo inziale del range con 1 se 0?
353300100514         if  W1Cnrs <> *zero   and   W1Cncd <= *zero;
353400100514           W1Cncd = 1;
353500100514           write  TB28W1;
353600100514           if  $Protect;
353700100514             write  Protect;
353800100514           endif;
353900100514         endif;
354000140123
354100140718         // -?Numero colli richiesto:?
354200140718         //  ?almeno 10.000 e comunque multiplo di 5.000?
354300140123         select;
354400140718           when  W1Cncl > *zero   and   W1Cncl < 10000;
354500140718             VIDmsg = %trimr($Msg(46)) +
354600140718                      ' DEVE prevedere almeno 10.000 colli';
354700140718             PosCurW1ncl = *on;
354800140718             ErrMessage  = *on;
354900140718             ErrGenerico = *on;
355000140718             leavesr;
355100140718           when  W1Cncl > 10000   and   %rem( W1Cncl : 5000 ) > *zero;
355200140123             VIDmsg = %trimr($Msg(46)) +
355300140718                      ' NON è multiplo di 5.000';
355400140123             PosCurW1ncl = *on;
355500140123             ErrMessage  = *on;
355600140123             ErrGenerico = *on;
355700140123             leavesr;
355800140317           when  W1Cncd > *zero;
355900140317       //  when  W1Cncd > *zero   and
356000140317       //        %rem( W1Cnca - W1Cncd + 1 : 1000 ) > *zero;
356100140903             // -?Controllo da NON fare SE inserito il range dall'utente?
356200140903             //  ?(su richiesta di Mirko)?
356300140903             if  $CtrlRngSgnCl   and
356400140903                 %rem( W1Cncd - 1 : 5000 ) > *zero;
356500140903               VIDmsg = %trimr($Msg(47));
356600140903               PosCurW1ncd = *on;
356700140903               ErrMessage  = *on;
356800140903               ErrGenerico = *on;
356900140903               leavesr;
357000140903             endif;
357100140317             if W1CncA = *hival;
357200140317               wCncA = W1CncA + 1;
357300140317             else;
357400140317               wCncA = W1CncA;
357500140317             endif;
357600140903             if  wCncA - W1CncD + 1 < 10000;
357700140903               VIDmsg = %trimr($Msg(46)) +
357800140903                        ' DEVE prevedere almeno 10.000 colli';
357900140903               PosCurW1nca = *on;
358000140903               ErrMessage  = *on;
358100140903               ErrGenerico = *on;
358200140903               leavesr;
358300140903             endif;
358400140917             // -?Controllo da NON fare SE inserito il range dall'utente?
358500140917             //  ?(su richiesta di Mirko)?
358600140917             if  $CtrlRngSgnCl   and
358700140917                 %rem( wCncA - W1CncD + 1 : 5000 ) > *zero;
358800140317               VIDmsg = %trimr($Msg(46)) +
358900140718                       ' NON è multiplo di 5.000';
359000140317               PosCurW1ncd = *on;
359100140317               ErrMessage  = *on;
359200140317               ErrGenerico = *on;
359300140317               leavesr;
359400140317             endif;
359500140123         endsl;
359600091104
359700091126         // -?Ricerca serie parziale?
359800091126         // ->?Selezione per numero colli?
359900091104         IF  W1Cncl > *zero;
360000091104
360100091104           clear  TNTB54ds;
360200100215           if  V2Dfls > *zero;
360300100215             I54fil = V2Dfls;
360400100215           else;
360500100215             I54fil = %subst( V1Ccli : 1 : %len(I54fil) );
360600100215           endif;
360700091104           I54tel = 'R';
360800091104           I54nrs = %editc( V2Cnrs : 'X' );
360900091104           I54tot = W1Cncl;
361000091104           kpjbu  = TNTB54ds;
361100110207           tntb54r ( kpjba : ubLeg3C_SKC );
361200091104           TNTB54ds = kpjbu;
361300091104
361400091104           select;
361500091126             // ->?errore SQL?
361600091104             when  O54err = 'S';
361700091125               VIDmsg = $Msg(18);
361800091104               PosCurW1nrs = *on;
361900091104               ErrMessage  = *on;
362000091104               ErrGenerico = *on;
362100091104               leavesr;
362200091126             // ->?serie già presente ma parziale: errore bloccante?
362300091104             when  O54err = '1';
362400091126               VIDmsg = $Msg(33);
362500091104               PosCurW1nrs = *on;
362600091104               ErrMessage  = *on;
362700091104               ErrGenerico = *on;
362800091104               leavesr;
362900120113             // ->?Serie NON ammessa (99)?
363000120113             when  O54err = '9';
363100120113               VIDmsg = $Msg(43);
363200120113               PosCurW1nrs = *on;
363300120113               ErrMessage  = *on;
363400120113               ErrGenerico = *on;
363500120113               leavesr;
363600091104           endsl;
363700091104
363800091104           If  O54dal <> *zero;
363900140718             if  SavW1Cnrs <> O54nrs  or
364000140718                 SavW1Cncd <> O54dal  or
364100140718                 SavW1Cnca <> O54al;
364200140718               $CtrlRngSgnCl = *on;
364300140718               SavW1Cnrs = O54nrs;
364400140718               SavW1Cncd = O54dal;
364500140718               SavW1Cnca = O54al;
364600140718               clear  W1Cncl;
364700140718               W1Cnrs = O54nrs;
364800140718               W1Cncd = O54dal;
364900140718               W1Cnca = O54al;
365000140718               leavesr;
365100140718             endif;
365200121128             V2Dfls = I54fil;
365300091106             V2Cnrs = O54nrs;
365400091104             V2Cncd = O54dal;
365500140718             V2Cnca = O54al;
365600091104           Else;
365700091126             // ->?SE non è stata trovata la disponibilità di serie?
365800091126             //   ?parziale => ricerca per serie completa?
365900091104             clear  TRTB28ds2;
366000100215             if  V2Dfls > *zero;
366100100215               ParFIL = %int(V2Dfls);
366200100215             else;
366300100215               ParFIL = V2Ccli / 10000;
366400100215             endif;
366500091104             ParELB = 'R';
366600111215             trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
366700120113             select;
366800120113               // ->?Serie NON ammessa (99)?
366900120113               when  ParERR = '9';
367000120113                 VIDmsg = $Msg(43);
367100120113                 PosCurW1nrs = *on;
367200120113                 ErrMessage  = *on;
367300120113                 ErrGenerico = *on;
367400120113                 leavesr;
367500091126               // -?Non trovata serie completa libera?
367600120113               when  ParERR <> *blank;
367700120113                 VIDmsg = $Msg(27);
367800120113                 PosCurW1nrs = *on;
367900120113                 ErrMessage  = *on;
368000120113                 ErrGenerico = *on;
368100120113                 leavesr;
368200120113               other;
368300121128                 V2Dfls = %editc( ParFIL : 'X' );
368400120113                 V2Cnrs = ParNRS;
368500120113                 V2Cncd = 1;
368600120113                 V2Cnca = W1Cncl;
368700120113             endsl;
368800091104           EndIf;
368900091104
369000091126         // ->?Selezione per segnacolli dal/al?
369100091104         ELSE;
369200091104
369300101014           exsr  sr_Ctrl_NRS_SCx;
369400170321           if  ErrGenerico = *on
369500170321           // oppure ho premuto F12 (dalla finestra di forzatura)
369600170321            or dsp_aid = c_F12;
369700101014             leavesr;
369800101014           endif;
369900091104
370000121128           V2Dfls = I54fil;
370100091104           V2Cnrs = W1Cnrs;
370200091104           V2Cncd = W1Cncd;
370300091104           V2Cnca = W1Cnca;
370400091104
370500091104         ENDIF;
370600091104
370700091104       ENDSR;
370800091104
370900091104       //--------------------------------------------------------------
371000091104       //?Gestione window W02.                                         ?
371100091104       //--------------------------------------------------------------
371200091104       BEGSR  sr_GesW02;
371300091104
371400091126         // -?Inizializzazione window?
371500091104         if  $InzW02 = *on;
371600091104           exsr  sr_InzW02;
371700091104           $InzW02 = *on;
371800091104         endif;
371900091104
372000091126         // -?Emissione window?
372100091127         exfmt  TB28W2;
372200091104
372300091126         // -?F12=Ritorno?
372400091104         if  dsp_aid = c_F12;
372500091106           $Video = $VideoPrec;
372600150319           clear  $VideoPrec;
372700091104           clear  V2F3CE;
372800091104         endif;
372900091104
373000091104       ENDSR;
373100091104
373200091104       //--------------------------------------------------------------
373300091104       //?Inizializzazione window W02.                                 ?
373400091104       //--------------------------------------------------------------
373500091104       BEGSR  sr_InzW02;
373600091104
373700091127         clear  TB28W2;
373800091104
373900091104         clear  d3CE;
374000091104
374100120913         //if  V2Cnrs = *zero;
374200120913         //  leavesr;
374300120913         //endif;
374400091104
374500091126         // -?Reperimento tab. "3CE" per cliente in esame?
374600091104         if  getTabella ( c_Tntbe : '3CE' : V1Ccli :
374700091104                          *blank: *omit : *omit :
374800091104                          *omit : *omit :
374900091104                          *omit : *omit : *omit : *omit :
375000091104                          *omit : *omit : *omit : *omit :
375100091104                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
375200091106                          = *zero      AND
375300091104               ds_TNTBE.TBEatb = *blank;
375400091109           d3CE = ds_TNTBE.TBEuni;
375500091104         endif;
375600091104
375700091126         // -?Impostazione campi a video?
375800091104         if  §3CEedat > *zeros;
375900091104           wDate_Eur = %date( §3CEedat : *iso );
376000091104           W2edat = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
376100091105                      ( %subdt(wDate_Eur : *months) * 10000 ) +
376200091105                      ( %subdt(wDate_Eur : *years) ) );
376300091104         endif;
376400091104         W2ever = §3CEever;
376500091104         W2cver = §3CEcver;
376600091104         if  §3CEcdsc > *zeros;
376700091104           wDate_Eur = %date( §3CEcdsc : *iso );
376800091104           W2cdsc = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
376900091104                      ( %subdt(wDate_Eur : *months) * 10000 ) +
377000091105                      ( %subdt(wDate_Eur : *years) ) );
377100091104         endif;
377200091104         W2sncd = §3CEsncd;
377300091104         W2snca = §3CEsnca;
377400091104         W2sncc = §3CEsncc;
377500091104         W2obj    = §3CEobj;
377600091104         W2corpo  = §3CEcorpo;
377700091104         W2typweb = §3CEtypweb;
377800091104         W2nrs3CE  = §3CEnrs;
377900091104         W2cvermax = §3CEcvermx;
378000091104         if  §3CEcddemx > *zeros;
378100091104           wDate_Eur = %date( §3CEcddemx : *iso );
378200091104           W2cddemax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
378300091104                         ( %subdt(wDate_Eur : *months) * 10000 ) +
378400091104                         ( %subdt(wDate_Eur : *years) ) );
378500091104         endif;
378600091104         if  §3CEcdscmx > *zeros;
378700091104           wDate_Eur = %date( §3CEcdscmx : *iso );
378800091104           W2cdscmax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
378900091104                         ( %subdt(wDate_Eur : *months) * 10000 ) +
379000091104                         ( %subdt(wDate_Eur : *years) ) );
379100091104         endif;
379200120316         W2esvsrv = §3CEesvsrv;
379300091104
379400091126         // -?Decodifica il Tipo Aggiornamento Web?
379500091104         select;
379600091104           when  §3CEtypweb = *blank;
379700100107           when  §3CEtypweb = '0 ';
379800100107             W2typwebD = 'Funzionalità NON attiva';
379900091104           when  §3CEtypweb = '1 ';
380000091104             W2typwebD = 'Funzionalità attiva';
380100100107           when  §3CEtypweb = '2 ';
380200100107             W2typwebD = 'Framework 1.0';
380300100107           when  §3CEtypweb = '4 ';
380400100107             W2typwebD = 'Framework 1.1';
380500100107           when  §3CEtypweb = '6 ';
380600100107             W2typwebD = 'Framework 1.1 & 1.0';
380700100107           when  §3CEtypweb = '8 ';
380800100107             W2typwebD = 'Framework 2.0';
380900100107           when  §3CEtypweb = '10';
381000100107             W2typwebD = 'Framework 2.0 & 1.0';
381100100107           when  §3CEtypweb = '12';
381200100107             W2typwebD = 'Framework 2.0 & 1.1';
381300091104           other;
381400091104             W2typwebD = *all'? ';
381500091104         endsl;
381600091104
381700091104       ENDSR;
381800091104
381900091104       //--------------------------------------------------------------
382000091104       //?Gestione window W03.                                         ?
382100091104       //--------------------------------------------------------------
382200091104       BEGSR  sr_GesW03;
382300091104
382400091126         // -?Inizializzazione window?
382500091106         exsr  sr_InzW03;
382600091104
382700091126         // -?Emissione window?
382800091104         if  (W3txt1 + W3txt2 + W3txt3) <> *blank;
382900091127           exfmt  TB28W3;
383000091104         endif;
383100091104
383200091126         // -?Enter=Continua?
383300091106         $Video = $VideoPrec;
383400150319         clear  $VideoPrec;
383500091104
383600091104       ENDSR;
383700091104
383800091104       //--------------------------------------------------------------
383900091104       //?Inizializzazione window W03.                                 ?
384000091104       //--------------------------------------------------------------
384100091104       BEGSR  sr_InzW03;
384200091104
384300091127         clear  TB28W3;
384400091104
384500091104         if  V2Cnot = *blank;
384600091104           leavesr;
384700091104         endif;
384800091104
384900091126         // -?Verifica esistenza tab. "KB4" per cliente in esame?
385000091104         $KB4 = ( getTabella (c_Tntbe : 'KB4' : V1Ccli :
385100091111                              *omit : *omit : *omit :
385200091104                              *omit : *omit :
385300091104                              *omit : *omit : *omit : *omit :
385400091104                              *omit : *omit : *omit : *omit :
385500091104                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni)
385600091104                              = *zero );
385700091104         select;
385800091104
385900091104           when  *inKE      and
386000091104                (NOT $KB4   or   ds_TNTBE.TBEatb <> *blank);
386100091104             W3txt2 = 'Abilitare il cliente in tab. "KB4".';
386200091104
386300091104           when  *inKF      and
386400091104                (NOT $KB4   or   ds_TNTBE.TBEatb <> *blank);
386500091104             W3txt2 = 'Abilitare il cliente in tab. "KB4".';
386600091104
386700091104           when  *inKQ  and
386800091104                ($KB4   and   ds_TNTBE.TBEatb = *blank);
386900091104             W3txt2 = 'Annullare il cliente in tab. "KB4".';
387000091104
387100091104         endsl;
387200091104
387300091104       ENDSR;
387400101027
387500101027       //--------------------------------------------------------------
387600101027       //?Gestione window W04.                                         ?
387700101027       //--------------------------------------------------------------
387800101027       BEGSR  sr_GesW04;
387900101027
388000101027         // -?Inizializzazione window?
388100101027         if  $InzW04 = *on;
388200101027           clear  TB28W4;
388300101027           W4Csun  = '?';
388400101027           $InzW04 = *off;
388500101027         endif;
388600101027
388700101027         // -?Emissione window?
388800101027         if  Not $Protect;
388900101027           exfmt  TB28W4;
389000101027         else;
389100101027           write  TB28W4;
389200101027           exfmt  Protect;
389300101027         endif;
389400101027         reset  ErrMessage;
389500101027         reset  ErrGenerico;
389600101027         clear  VIDmsg;
389700101027
389800101027         // -?F12=Ritorno?
389900101027         if  dsp_aid = c_F12;
390000110725           clear  W4Csun;
390100101027           $Video = $VideoPrec;
390200150319           clear  $VideoPrec;
390300101027           leavesr;
390400101027         endif;
390500101027
390600101027         // -?Invio?
390700101027         exsr  sr_CtrW04;
390800101027         if  NOT ErrGenerico;
390900101027           TIVSSds.VSSksu = '0' + %editc(V2Ccks : 'X');
391000101027           TIVSSds.VSSsun = W4Csun;
391100101027           // ->?Ritorno alla videata precedente?
391200101027           $Video = $VideoPrec;
391300150319           clear  $VideoPrec;
391400101027           leavesr;
391500101027         endif;
391600101027
391700101027       ENDSR;
391800101027
391900101027       //--------------------------------------------------------------
392000101027       //?Controllo dati immessi nella window W04.                     ?
392100101027       //--------------------------------------------------------------
392200101027       BEGSR  sr_CtrW04;
392300101027
392400101027         %subst(IndDspF : 81) = *off;
392500101027
392600101027         Select;
392700101027
392800101027           // -?Ricerca Strategi User Number?
392900101028           When  %scan('?' : W4Csun) > *zeros;
393000101028             clear  W4Csun;
393100101027             exsr  sr_F13D02;
393200101027             select;
393300101028               when  TNTB79ds.o3EWfxx <> *blank;
393400120726                 ErrGenerico = *on;
393500101028               when  TNTB79ds.o3EWerr = *on;
393600101028                 VIDmsg = TNTB79ds.o3EWmsg;
393700101028                 PosCurW4sun = *on;
393800101027                 ErrMessage  = *on;
393900101027                 ErrGenerico = *on;
394000120726                 leavesr;
394100101028               when  TNTB79ds.o3EWksu = '0' + %editc(V2Ccks : 'X');
394200101028                 W4Csun = TNTB79ds.o3EWsun;
394300101027             endsl;
394400101027
394500101027           // -?Effettuare una selezione?
394600101028           When  W4Csun = *blank  or  W4Csun = *zero;
394700101028             VIDmsg = $Msg(42);
394800101028             PosCurW4sun = *on;
394900101027             ErrMessage  = *on;
395000101027             ErrGenerico = *on;
395100101027             leavesr;
395200101027
395300101027           // -?Controllo Strategi User Number selezionato?
395400101027           Other;
395500101028             rqsSUN = W4Csun;
395600101028             exsr  sr_TIS7701_x_TIVSS;
395700101028             select;
395800101028               // -?Codice cliente Strategi errato?
395900101028               when  rpyEsito <> *zero  or  TIVSSds.VSSksu = *blank
396000101028                                        or  TIVSSds.VSSksu = *zero;
396100101028                 VIDmsg = $Msg(14);
396200101028                 PosCurW4sun = *on;
396300101028                 ErrMessage  = *on;
396400101028                 ErrGenerico = *on;
396500101028                 leavesr;
396600101028               // -?Abilitazione scaduta?
396700101028               when  TIVSSds.VSSdsc < wDate;
396800101028                 VIDmsg = $Msg(37);
396900101028                 PosCurW4sun = *on;
397000101028                 ErrMessage  = *on;
397100101028                 ErrGenerico = *on;
397200101028                 leavesr;
397300101028             endsl;
397400101027
397500101027         EndSl;
397600101027
397700101027       ENDSR;
397800111011
397900111011       //--------------------------------------------------------------
398000111011       //?Gestione window W05.                                         ?
398100111011       //--------------------------------------------------------------
398200111011       BEGSR  sr_GesW05;
398300111011
398400111011         // -?Inizializzazione window?
398500111011         if  $InzW05 = *on;
398600111021           write  TB28W5a;
398700111011           exsr  sr_InzW05;
398800111011           $InzW05 = *off;
398900111011         endif;
399000111011
399100111011         // -?Emissione window?
399200111021         exfmt  TB28W5b;
399300111011
399400111011         // -?F12=Ritorno?
399500111011         if  dsp_aid = c_F12;
399600111011           $Video = $VideoPrec;
399700150319           clear  $VideoPrec;
399800111011         endif;
399900111011
400000111011       ENDSR;
400100111011
400200111011       //--------------------------------------------------------------
400300111011       //?Inizializzazione window W05.                                 ?
400400111011       //--------------------------------------------------------------
400500111011       BEGSR  sr_InzW05;
400600111011
400700111021         clear  TB28W5b;
400800111011
400900111011         // -?Reperimento ultimo segnacollo usato?
401000111024         if  V2Dfls > *zero;
401100121121           pInFilSgncl = %int( V2Dfls );
401200111024         else;
401300121121           pInFilSgncl = V2Ccli / 10000;
401400111024         endif;
401500121121         if  sch_SKC(1) = *zero;
401600121121           sch_SKC(1) = V2Ccks;
401700121121         endif;
401800121121         if  ubLastNSC_Rtv ( V2Cnrs : pInFilSgncl :
401900111024                             ubLeg3C_SKC : *omit :
402000140225                             pOutMaxNSC : pOutMaxLNP :
402100140225                             pOutMaxAAS : pOutMaxMGS :
402200140225                             pOutMaxNSP :
402300140225                             pOutLastNSC : pOutLastLNP :
402400140225                             pOutLastAAS : pOutLastMGS :
402500140225                             pOutLastNSP ) < *zero;
402600111011           VIDmsg = 'ERRORE nel reperim. dell''ultimo segnacollo +
402700111011                     utilizzato. Verificare in stampa.';
402800111011           errMessage  = *on;
402900111011           errGenerico = *on;
403000111011           leavesr;
403100111011         endif;
403200111011
403300111011         // -?Impostazione campi a video?
403400121121         W5Cfls  = pInFilSgncl;
403500121121         W5Cnrs  = V2Cnrs;
403600121121         W5Ccks  = V2Ccks;
403700140225         W5CnscM = pOutMaxNSC;
403800140225         if  pOutMaxAAS > *zero  and  pOutMaxMGS > *zero;
403900140225           W5DgmasM  = (pOutMaxAAS * 10000) + pOutMaxMGS;
404000140225           wDate_Eur = %date( W5DgmasM : *iso );
404100140225           W5DgmasM  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
404200111018                         ( %subdt(wDate_Eur : *months) * 10000 ) +
404300111018                         ( %subdt(wDate_Eur : *years) ) );
404400111018         endif;
404500140225         W5CnscL = pOutLastNSC;
404600140225         if  pOutLastAAS > *zero  and  pOutLastMGS > *zero;
404700140225           W5DgmasL  = (pOutLastAAS * 10000) + pOutLastMGS;
404800140225           wDate_Eur = %date( W5DgmasL : *iso );
404900140225           W5DgmasL  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
405000140225                         ( %subdt(wDate_Eur : *months) * 10000 ) +
405100140225                         ( %subdt(wDate_Eur : *years) ) );
405200140225         endif;
405300111011
405400111011       ENDSR;
405500120517
405600120517       //--------------------------------------------------------------
405700120517       //?Reperimento famiglia completa dell'unificante "originale"    ?
405800120517       //--------------------------------------------------------------
405900120517       BEGSR  sr_ubLeg3C_Orig;
406000120517
406100120517         ubLeg3C_Rtv ( §3Ccks :
406200120517                       ubLeg3C_FlgPF :
406300120517                       ubLeg3C_Padre :
406400120517                       ubLeg3C_SKC :
406500120517                       ubLeg3C_SKEW );
406600120517
406700120517         sch_SKC_Orig = sch_SKC;
406800120517
406900120517       ENDSR;
407000120104
407100120104       //--------------------------------------------------------------
407200120104       //?Reperimento famiglia completa del cliente in esame           ?
407300120104       //--------------------------------------------------------------
407400120104       BEGSR  sr_ubLeg3C;
407500120104
407600120104         // -?Reperimento famiglia completa dalla tab. "3C"?
407700120517         //  ?(se modificato il padre)?
407800120104         if  V2Ccks <> ubLeg3C_Padre;
407900120104           ubLeg3C_Rtv ( V2Ccks :
408000120104                         ubLeg3C_FlgPF :
408100120104                         ubLeg3C_Padre :
408200120104                         ubLeg3C_skC :
408300120104                         ubLeg3C_skEW );
408400120104         endif;
408500120104
408600120104         // -?"Sistemazione" della stessa se modificato il padre?
408700120104         //  ?impostandolo a se stesso?
408800120517         //  ?N.B.: non essendo ancora stato premuto F6, il padre?
408900120517         //        ?reperito risulta ancora l'ultimo registrato...?
409000120104         if  ubLeg3C_FlgPF = 'F'  and  ubLeg3C_Padre <> V2Ccks
409100120104                                  and  V2Ccli = V2Ccks;
409200120104           ubLeg3C_FlgPF  = 'P';
409300120104           ubLeg3C_Padre  = V2Ccks;
409400120104           clear  ubLeg3C_skC;
409500120104           clear  ubLeg3C_skEW;
409600120104           sch_skC(1)  = V2Ccks;
409700120104           sch_skEW(1) = (§3Ccba = c_EasyWeb);
409800120104         endif;
409900120104
410000120104       ENDSR;
410100091105
410200091105       //--------------------------------------------------------------
410300091105       //?Aggiornam.rec. di tutti i file in tutti i sistemi informativi?
410400091105       //--------------------------------------------------------------
410500091105       BEGSR  sr_UpdateAll;
410600091105
410700091126         // -?Ciclo di elaborazione per ogni sistema informativo?
410800091105         For  w_SisInf = 1  To  %elem($Libr);
410900091105
411000091126           // -?Apertura degli archivi?
411100091105           if  w_SisInf > 1;
411200091105             exsr  sr_OpenFileTab;
411300091105           endif;
411400091105
411500091126           // -?Aggiornamento tab. "3C"?
411600091105           exsr  sr_UpdTABEL;
411700091105
411800100330           // -?Aggiornamento tab. "3CE", "3CP" e "3EW"?
411900091105           exsr  sr_UpdTNTBE;
412000091105
412100091105         EndFor;
412200100330
412300121119         // -?Aggiornamento flag §3EWUPD in tab. "3EW"?
412400110616         //  ?(i 2 sistemi informativi sono gestiti dal chiamato)?
412500130604         clear  wRtnEsito;
412600121119         Select;
412700121119
412800121119           // -?F6=Conferma (modifica/inserimento)?
412900121119           When  Not  $3EWupd   and   dsp_aid = c_F06;
413000121119             // ·?del vecchio unificante (se variato unificante)?
413100121119             if  %found(TABEL00F)   and  Old_§3Ccks <> V2Ccks
413200121119                                    and  Old_§3Ccba  = c_EasyWEB;
413300121119               wVSSksu = '0' + %editc( Old_§3Ccks : 'X' );
413400130604               //Ub§3EWupd_Upd ( wVSSksu : *omit : 'A' : *off );
413500130604               wRtnEsito = Ub§3EWupd_Upd ( wVSSksu : TIVSSds_Orig.VSSsun :
413600130604                                           'A' : *off );
413700130605               //if  wRtnEsito < *zero;
413800130605               //  exsr  sr_Send_eMail;
413900130605               //endif;
414000121119             endif;
414100121119             // ·?dell'unificante attuale?
414200121119             //  ?(messo, tolto o lasciato EasyWEB)?
414300121119             if  V2Ccba = c_EasyWEB  or  Old_§3Ccba = c_EasyWEB;
414400130604               wRtnEsito = Ub§3EWupd_Upd ( TIVSSds.VSSksu : TIVSSds.VSSsun :
414500130604                                           'A' : *off );
414600130605               //if  wRtnEsito < *zero;
414700130605               //  exsr  sr_Send_eMail;
414800130605               //endif;
414900121119             endif;
415000121119
415100121119           // -?F16=Annullamento?
415200121119           When  Not  $3EWupd   and   dsp_aid = c_F16   and
415300121119                 Old_§3Ccba = c_EasyWEB;
415400121119             wVSSksu = '0' + %editc( Old_§3Ccks : 'X' );
415500130604             wRtnEsito = Ub§3EWupd_Upd ( wVSSksu : TIVSSds_Orig.VSSsun :
415600130604                                         'A' : *off );
415700130605             //if  wRtnEsito < *zero;
415800130605             //  exsr  sr_Send_eMail;
415900130605             //endif;
416000120207
416100121119           // -?F5=Ripristino?
416200121119           When  Not  $3EWupd   and   dsp_aid = c_F05   and
416300121119                 V2Ccba = c_EasyWEB;
416400130604             wRtnEsito = Ub§3EWupd_Upd ( TIVSSds.VSSksu : TIVSSds.VSSsun :
416500130604                                         'A' : *off );
416600130605             //if  wRtnEsito < *zero;
416700130605             //  exsr  sr_Send_eMail;
416800130605             //endif;
416900121119         EndSl;
417000091105
417100091126         // -?Messaggio di avviso: allineare la tab. "KB4"!?
417200091109         if  dsp_aid = c_F05  or  dsp_aid = c_F06  or  dsp_aid = c_F16;
417300091106           $VideoPrec = $Video;
417400091105           $Video = 'W3';
417500091105           exsr  sr_GesW03;
417600091105         endif;
417700091105
417800091105       ENDSR;
417900091130
418000091130       //--------------------------------------------------------------
418100091130       //?Riempimento struttura dati ds3C.                             ?
418200091130       //--------------------------------------------------------------
418300091130       BEGSR  sr_RieDS3C;
418400091130
418500091130         clear  ds3C;
418600100212
418700100212         // -?Impostazione campi per NON "EasyWeb"?
418800100212         if  V2Ccba <> c_EasyWEB;
418900100518           //§3Cnrs = V2Cnrs;         ?(da memorizzare comunque)?
419000100518           if  V2Cnrs > *zero;
419100100212             §3Cfls = %int( V2Dfls );
419200100212           endif;
419300100212           §3Cctm = V2Cctm;
419400100212         endif;
419500100212
419600100212         // -?Impostazione altri campi (per tutti)?
419700100518         §3Cnrs = V2Cnrs;
419800091130         §3Cokd = V2Cokd;
419900091130         §3Crag = V2Crag;
420000091130         §3Clrm = V2Clrm;
420100091130         §3Calm = V2Calm;
420200091130         §3Cnot = V2Cnot;
420300091130         §3Cabc = V2Cabc;
420400091130         §3Cdkc = V2Cdkc;
420500091130         §3Cavx = V2Cavx;
420600091130         §3Cabd = V2Cabd;
420700091130         §3Caus = V2Caus;
420800091130         §3Carc = V2Carc;
420900091130         §3Cmvr = V2Cmvr;
421000091222         §3Cbc2 = V2Cbc2;
421100091130         §3Ccba = V2Ccba;
421200091130         §3Cbac = V2Cbac;
421300091130         §3Cfg1 = V2Cfg1;
421400160317         §3CCSAMB = V2Amb;
421500091130         §3Cfsp = V2Cfsp;
421600160317         §3CCSTFP = V2TFP;
421700091130         §3Ccks = V2Ccks;
421800091130         §3Cnwb = V2Cnwb;
421900091130         §3Cnwc = V2Cnwc;
422000091130         §3Crsb = V2Crsb;
422100091130         §3Cpes = V2Cpes;
422200091130
422300091130       ENDSR;
422400091102
422500091102       //--------------------------------------------------------------
422600091102       //?Aggiornamento record nel file TABEL00F.                      ?
422700091102       //--------------------------------------------------------------
422800091102       BEGSR  sr_UpdTABEL;
422900091106
423000091106         //? N.B.                                                      ?
423100091106         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
423200091106         //   trasmissione (vedi flag TBEFTR).                        ?
423300091106         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
423400091106         //   subito nello stesso file di entrambi i S.I. - in due    ?
423500091106         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
423600091103
423700091105         // -?Aggiornam. tab. "3C" = Invio dati -BOLLETTAZIONE-?   ?
423800091105
423900091126         // -?RI-aggancio record da aggiornare?
424000091126         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
424100091105         if  w_SisInf > 1;
424200121204           k_TBLkut = c_Kut;
424300121204           k_TBLcod = c_Tab;
424400091105           k_TBLkey = V1Ccli;
424500091105           chain  %kds(k03tabel00)  TABEL;
424600091105         endif;
424700091102
424800091102         select;
424900091102
425000091126           // -?F5 = Ripristino?
425100091105           when  dsp_aid = c_F05;
425200100407             if  Not %found(TABEL00F);
425300100407               clear  TABEL;
425400100407               TBLkut = c_Kut;
425500100407               TBLcod = c_Tab;
425600100407               TBLkey = k_TBLkey;
425700100407             endif;
425800100407             exsr  sr_RieDS3C;
425900100407             TBLuni = ds3C;
426000091130             select;
426100121128               //when  %found(TABEL00F)   and   TBLflg = '*';
426200121128               when  %found(TABEL00F);
426300091130                 clear  TBLflg;
426400091130                 clear  TBLftr;
426500091130                 clear  TBLdtr;
426600091130                 //____________
426700091130                 update  TABEL;
426800091130                 //¯¯¯¯¯¯¯¯¯¯¯¯
426900091130               // - Record appena cancellato fisicamente
427000091130               when  NOT %found(TABEL00F);
427100091130                 TBLftt = '1';
427200091130                 clear  TBLflt;
427300091130                 if  w_SisInf = 1;
427400091130                   TBLftr = 'T';
427500091130                 else;
427600091130                   TBLftr = 'R';
427700091130                 endif;
427800091130                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
427900091130                                        %len(wDate) - %len(TBLdtr) + 1 :
428000091130                                        %len(TBLdtr) ) );
428100091130                 //____________
428200091130                 write   TABEL;
428300091130                 //¯¯¯¯¯¯¯¯¯¯¯¯
428400091130             endsl;
428500091102
428600091126           // -?F6 = Aggiornamento / Inserimento?
428700091102           when  dsp_aid = c_F06;
428800091102             if  Not %found(TABEL00F);
428900091106               clear  TABEL;
429000091102               TBLkut = c_Kut;
429100091102               TBLcod = c_Tab;
429200091102               TBLkey = k_TBLkey;
429300091102             endif;
429400121128             clear  TBLflg;
429500091130             exsr  sr_RieDS3C;
429600091106             TBLuni = ds3C;
429700091102
429800091102             TBLftt = '1';
429900091105             clear  TBLflt;
430000091105             if  w_SisInf = 1;
430100091105               TBLftr = 'T';
430200091105             else;
430300091105               TBLftr = 'R';
430400091105             endif;
430500091111             TBLdtr = %int( %subst( %editc(wDate : 'X') :
430600091111                                    %len(wDate) - %len(TBLdtr) + 1 :
430700091111                                    %len(TBLdtr) ) );
430800091102
430900091102             if  %found(TABEL00F);
431000091102               //____________
431100091102               update  TABEL;
431200091102               //¯¯¯¯¯¯¯¯¯¯¯¯
431300091102             else;
431400091102               //____________
431500091102               write   TABEL;
431600091102               //¯¯¯¯¯¯¯¯¯¯¯¯
431700091102             endif;
431800091102
431900091126           // -?F16 = Annullamento?
432000091105           when  dsp_aid = c_F16;
432100091130             if  %found(TABEL00F)   and   TBLflg   = *blank;
432200120116               if  old_§3Ccba = c_EasyWEB;
432300120116                 clear  old_§3Cnrs;
432400120116                 TBLuni = ds3C_old;
432500120116               endif;
432600091106               TBLflg = '*';
432700091106               clear  TBLftr;
432800091106               clear  TBLdtr;
432900091106               //____________
433000091106               update  TABEL;
433100091106               //¯¯¯¯¯¯¯¯¯¯¯¯
433200091106             endif;
433300091102
433400091102         endsl;
433500091102
433600091102       ENDSR;
433700091103
433800091103       //--------------------------------------------------------------
433900091103       //?Aggiornamento altri records nel file TNTBE00F.               ?
434000091103       //--------------------------------------------------------------
434100091103       BEGSR  sr_UpdTNTBE;
434200091106
434300091106         //? N.B.                                                      ?
434400091106         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
434500091106         //   trasmissione (vedi flag TBEFTR).                        ?
434600091106         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
434700091106         //   subito nello stesso file di entrambi i S.I. - in due    ?
434800091106         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
434900091105
435000091103         // -?Aggiornam. tab. "3CP" = estensione per serie parziale?   ?
435100091103
435200091105         Select;
435300091105
435400091126           // -?F6 = Aggiornamento / Inserimento?
435500100215           When  dsp_aid = c_F06   and   V2Ccba <> c_EasyWEB;
435600100402             exsr  sr_Update3CP;
435700100921           When  dsp_aid = c_F06   and   V2Ccba     =  c_EasyWEB
435800100921                                   and   Old_§3Ccba <> c_EasyWEB;
435900100215             exsr  sr_Annull3CP;
436000101026
436100101026           // -?F5 = Ripristino?
436200101026           When  dsp_aid = c_F05   and   V2Ccba <> c_EasyWEB;
436300101026             exsr  sr_Update3CP;
436400091105
436500091126           // -?F16 = Annullamento?
436600101026           When  dsp_aid = c_F16   and   $Found_3CP;
436700091105             exsr  sr_Annull3CP;
436800091105
436900091105         EndSl;
437000100215
437100100331         // -?Aggiorn. tab. "3EW" = dati assegnati alla postaz. EasyWeb?   ?
437200100215
437300101026         Select;
437400100215
437500100215           // -?F5 = Ripristino?
437600100331           //  ?(dati EasyWeb gestibili solo sul padre)?
437700120207           //  ?(il padre deve risultare ancora/nuovamente abilitato ad?
437800120207           //  ?EasyWEB - vedi TIVSSds)?
437900120207           When  dsp_aid = c_F05    and
438000120207                 V2Ccli  = V2Ccks   and   V2Ccba = c_EasyWEB    and
438100120207                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
438200120207                 TIVSSds.VSSsun > *zero;
438300120207             exsr  sr_Ripris3EW;
438400100215
438500100215           // -?F6 = Aggiornamento / Inserimento?
438600101026           //  ?(dati EasyWeb gestibili solo sul padre, MA DAL *PGM?
438700101027           //  ?TNTB79R - NON da questo!!!)?
438800101026           //When  dsp_aid = c_F06    and
438900101026           //      V2Ccli  = V2Ccks   and   V2Ccba = c_EasyWEB;
439000101026           //  exsr  sr_Update3EW;
439100101027
439200101027           // -?F6 = Aggiornamento?
439300121205           //  ?Abbandonato il "Supporto cliente a BRT" "ESWEB"?
439400121205           //  ?(essendo scaduta l'applicazione):?OCCORRE ANNULLARE?
439500121205           //  ?LA TAB. "3EW" (attenzione ai figli) !!!?
439600121205           //  (i controlli sono già stati eseguiti)?
439700121205           //  ?SE mantenuto lo stesso range segnacolli:?
439800130507           //    ?=> si annulla la tab. "3EW" (già riportata in "3CP")?
439900121205           //  ?SE cambiato il range segnacolli:?
440000121205           //    ?=> si "blocca" (annullam. applicativo) la tab. "3EW"?
440100130507           When  dsp_aid    = c_F06   and  V2Ccba    <> c_EasyWeb  and
440200130507                 Old_§3Ccks = V2Ccli  and  Old_§3Ccba = c_EasyWeb;
440300101027             select;
440400110725               // -?Un solo codice in TIVSS?
440500101027               when  wCount_3EW = 1;
440600120816                 if  V2Cnrs = d3EW.§3EWnrs  and
440700120816                     V2Cncd = d3EW.§3EWnsi  and
440800120816                     V2Cnca = d3EW.§3EWnsf;
440900120814                   exsr  sr_Annull3EW;
441000120814                 else;
441100120814                   exsr  sr_AnnApp3EW;
441200120814                 endif;
441300110725               // -?Un codice selezionato da TIVSS?
441400110725               when  W4Csun > *zero  and  W4Csun = TIVSSds.VSSsun;
441500120816                 if  V2Cnrs = d3EW.§3EWnrs  and
441600120816                     V2Cncd = d3EW.§3EWnsi  and
441700120816                     V2Cnca = d3EW.§3EWnsf;
441800120814                   exsr  sr_Annull3EW;
441900120814                 else;
442000120814                   exsr  sr_AnnApp3EW;
442100120814                 endif;
442200101027             endsl;
442300130507           //  ?Cambiato unificante - sempre EasyWeb?
442400130507           //  ?(se prima padre di se stesso e senza altri figli)?
442500130507           When  dsp_aid     = c_F06   and  V2Ccba     = c_EasyWeb  and
442600130507                 Old_§3Ccks <> V2Ccks  and  Old_§3Ccba = c_EasyWeb  and
442700130507                 Old_§3Ccks  = V2Ccli  and  sch_SKC_Orig(2) = *zero;
442800130507             select;
442900130507               // -?Un solo codice in TIVSS?
443000130507               when  wCount_3EW = 1;
443100130507                 exsr  sr_AnnApp3EW;
443200130507               // -?Un codice selezionato da TIVSS?
443300130507               when  W4Csun > *zero  and  W4Csun = TIVSSds.VSSsun;
443400130507                 exsr  sr_AnnApp3EW;
443500130507             endsl;
443600120816           //  ?Ritorno al "Supporto cliente a BRT" = "ESWEB"?
443700120816           //  ?(mantenendo lo stesso unificante)?
443800120816           When  dsp_aid = c_F06       and
443900120816                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
444000121205                 V2Ccli  = Old_§3Ccks  and  Old_§3Ccba <> c_EasyWeb  and
444100120816                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
444200120816                 TIVSSds.VSSsun > *zero;
444300120816             exsr  sr_Ripris3EW;
444400121119           When  dsp_aid = c_F06       and  Not %found(TABEL00F)     and
444500121119                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
444600121119                 d3EW_Temp.§3EWfaa <> *blank                    and
444700121119                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
444800121119                 TIVSSds.VSSsun > *zero;
444900121119             exsr  sr_Ripris3EW;
445000121205           // -?O passaggio a se stesso come unificante?
445100121205           //  ?(già abilitato ad EasySped-WEB)?
445200121205           When  dsp_aid = c_F06       and
445300121205                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
445400121205                 V2Ccli <> Old_§3Ccks  and
445500121205                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
445600121205                 TIVSSds.VSSsun > *zero;
445700121205             exsr  sr_Ripris3EW;
445800100331
445900100331           // -?F16 = Annullamento di un cliente senza dati in "3EW":?
446000100331           //  ?nessun record da annullare?
446100101026           //When  dsp_aid = c_F16      and  Not $Found_TIVSS   and
446200101026           //      Old.§3Ccks = V2Ccli  and  Old.§3Ccba = c_EasyWeb;
446300100215
446400100215           // -?F16 = Annullamento?
446500100215           //  ?(impossibile se codice padre)?
446600101027           //  (i controlli sono già stati eseguiti)?
446700101026           When  dsp_aid    = c_F16   and
446800101028                 Old_§3Ccks = V2Ccli  and  Old_§3Ccba = c_EasyWEB;
446900120814             //exsr  sr_Annull3EW;
447000120814             exsr  sr_AnnApp3EW;
447100100215
447200101026         EndSl;
447300091105
447400091103
447500091103         // -?Aggiornam. tab. "3CE" = estensione per versioni Easy-Sped?
447600091105
447700091126         // -?Rerimento record da aggiornare?
447800091103         k_TBEcod = '3CE';
447900091105         k_TBEke1 = V1Ccli;
448000091111         //clear  k_TBEke2;
448100091111         chain  %kds( k05TNTBE01 : 2 )  TNTBE000;
448200091105
448300091105         select;
448400091105
448500091126           // -?F5 = Ripristino?
448600091105           when  dsp_aid = c_F05;
448700091130             select;
448800091130               when  %found(TNTBE01L)   and   TBEatb  = 'A';
448900091130                 clear  TBEatb;
449000091130                 clear  TBEftr;
449100091130                 //_______________
449200091130                 update  TNTBE000;
449300091130                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
449400100331               // -?Record appena cancellato fisicamente?
449500100331               //  ?(NON scrive nulla: si aspetta il prossimo aggior-?
449600100331               //  ? namento dal file TIESV - vedi *pgm. TNTB67R).?
449700100331               when  NOT %found(TNTBE01L);
449800100331               //clear  TNTBE000;
449900100331               //TBEcod = k_TBEcod;
450000100331               //TBEke1 = k_TBEke1;
450100100331               //clear  d3CE;
450200100331               //TBEuni = d3CE;
450300100331               //TBEftt = 'S';
450400100331               //clear  TBEflt;
450500100331               //if  w_SisInf = 1;
450600100331               //  TBEftr = 'T';
450700100331               //else;
450800100331               //  TBEftr = 'R';
450900100331               //endif;
451000100331               //TBEdtr = wDate;
451100100331               ////_______________
451200100331               //write   TNTBE000;
451300100331               ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
451400091130             endsl;
451500091105
451600091126           // -?F6 = Aggiornamento / Inserimento?
451700100331           //  ?(NON scrive nulla: si aspetta il prossimo aggior-?
451800100331           //  ? namento dal file TIESV - vedi *pgm. TNTB67R).?
451900100331           when  dsp_aid = c_F06;
452000100331           //if  Not %found(TNTBE01L);
452100100331           //  clear  TNTBE000;
452200100331           //  TBEcod = k_TBEcod;
452300100331           //  TBEke1 = k_TBEke1;
452400100331           //endif;
452500100331           //clear  d3CE;
452600100331           //TBEuni = d3CE;
452700100331           //TBEftt = 'S';
452800100331           //clear  TBEflt;
452900100331           //if  w_SisInf = 1;
453000100331           //  TBEftr = 'T';
453100100331           //else;
453200100331           //  TBEftr = 'R';
453300100331           //endif;
453400100331           //TBEdtr = wDate;
453500100331           //
453600100331           //if  %found(TNTBE01L);
453700100331           //  //_______________
453800100331           //  update  TNTBE000;
453900100331           //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
454000100331           //else;
454100100331           //  //_______________
454200100331           //  write   TNTBE000;
454300100331           //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
454400100331           //endif;
454500091105
454600091126           // -?F16 = Annullamento?
454700091105           when  dsp_aid = c_F16;
454800091130             if  %found(TNTBE01L)   and   TBEatb <> 'A';
454900091105               TBEatb = 'A';
455000091105               clear  TBEftr;
455100091105               //_______________
455200091105               update  TNTBE000;
455300091105               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
455400091105             endif;
455500091106
455600091106         endsl;
455700091103
455800091103       ENDSR;
455900091105
456000091105       //--------------------------------------------------------------
456100091106       //?Aggiornamento / Inserimento dati in tab. "3CP"               ?
456200091106       //?(estensione per serie parziale)                              ?
456300091105       //--------------------------------------------------------------
456400100402       BEGSR  sr_Update3CP;
456500091102
456600091106         $Found_3CP = *off;
456700091106
456800091106         k_TBEcod = '3CP';
456900091106         k_TBEke1 = V1Ccli;
457000091111         //clear  k_TBEke2;
457100091106         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
457200091106         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
457300091106
457400091106         // -?Aggiornamento (o annullamento) serie parziale?
457500091106         DOW  Not %eof(TNTBE01L);
457600091106
457700091106           Select;
457800091106
457900091126             // -?Vecchia serie parziale nel file (già annullata)?
458000100212             //  ?con serie completa a video  o?
458100120305             //  ?con "Supporto cliente a BRT" = "ESWEB"?
458200091106             When  TBEatb = 'A'     and
458300120731                  (V2Cnrs = *zero    or
458400120731                  (V2Cncd = 1       and   V2Cnca  = *hival)  or
458500100212                   V2Ccba = c_EasyWEB);
458600100212
458700100212             // -?Vecchia serie parziale nel file (da annullare)?
458800120305             //  ?con "Supporto cliente a BRT" = "ESWEB"?
458900100212             When  V2Ccba = c_EasyWEB;
459000100212               TBEatb = 'A';
459100100212               clear  TBEftr;
459200100212               //_______________
459300100212               update  TNTBE000;
459400100212               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
459500091106
459600091126             // -?Vecchia serie parziale nel file (da annullare)?
459700091126             //  ?con serie completa a video?
459800120731             When  V2Cnrs = *zero    or
459900120731                  (V2Cncd = 1       and   V2Cnca  = *hival);
460000091106               TBEatb = 'A';
460100091106               clear  TBEftr;
460200091106               //_______________
460300091106               update  TNTBE000;
460400091106               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
460500091106
460600091126             // -?Aggiornamento serie parziale o annullamento vecchia?
460700091126             //  ?serie parziale SE variata TBEKE2: serie e/o numero?
460800091126             //  ?segnacollo iniziale?
460900120731             When  V2Cnrs > *zero   and
461000120731                  (V2Cncd > 1       or    V2Cnca <  *hival);
461100091106               d3CP = TBEuni;
461200091106
461300091106               IF  %editc(V2Cnrs : 'X') + %editc(V2Cncd : 'X') =
461400091106                   %subst(TBEke2 : 1 : %len(%editc(V2Cnrs : 'X'))) +
461500091106                   %subst(TBEke2 : %len(%editc(V2Cnrs : 'X')) + 1 :
461600091106                                   %len(%editc(V2Cncd : 'X')));
461700091106                 $Found_3CP = *on;
461800091106
461900091106                 If  TBEatb = 'A'   or   §3CPal <> V2Cnca;
462000091106
462100091126                 // -=»>?Aggiornamento vecchia serie parziale?
462200091106                   if  §3CPal <> V2Cnca;
462300091106                     §3CPal = V2Cnca;
462400091106                     TBEuni = d3CP;
462500091106                   endif;
462600091106                   if  TBEatb = 'A';
462700091106                     clear  TBEatb;
462800091106                     clear  TBEftr;
462900091106                   endif;
463000091106                   //_______________
463100091106                   update  TNTBE000;
463200091106                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
463300091106                 EndIf;
463400091106
463500091106               ELSE;
463600091106
463700091126                 // -=»>?Annullamento vecchia serie parziale con?
463800091126                 //     ?serie e/o numero segnacollo iniziale diversi?
463900091106                 if  TBEatb = *blank;
464000091106                   TBEatb = 'A';
464100091106                   clear  TBEftr;
464200091106                   //_______________
464300091106                   update  TNTBE000;
464400091106                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
464500091106                 endif;
464600091106
464700091106               ENDIF;
464800091106
464900091106           EndSl;
465000091106
465100091106           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
465200091106
465300091106         ENDDO;
465400091106
465500091106         // -?Nuova serie parziale a video?
465600091126         //  ?(inserimento  o  variazione serie e/o segnacollo iniziale)?
465700120731         IF  V2Cnrs > *zero   and
465800120731            (V2Cncd > 1       or    V2Cnca <  *hival)  and
465900091106             Not $Found_3CP;
466000091106
466100091126           // ->?Scrittura nuova serie?
466200091106           clear  TNTBE000;
466300091106           TBEapl = X3CPapl;
466400091106           TBEcod = %subst( X3CPke1 : %len(X3CPke1) - 3 + 1 : 3 );
466500091106           TBEke1 = V1Ccli;
466600091106           TBEke2 = %editc(V2Cnrs : 'X') + %editc(V2Cncd : 'X');
466700091106           clear  d3CP;
466800091106           §3CPal = V2Cnca;
466900091106           TBEuni = d3CP;
467000091106           TBEftt = 'S';
467100091106           if  w_SisInf = 1;
467200091106             TBEftr = 'T';
467300091106           else;
467400091106             TBEftr = 'R';
467500091106           endif;
467600091106           TBEdtr = wDate;
467700091106           //______________
467800091106           write  TNTBE000;
467900091106           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
468000091106
468100091106         ENDIF;
468200091105
468300091105       ENDSR;
468400091105
468500091105       //--------------------------------------------------------------
468600091105       //?Annullamento dati in tab. "3CP"                              ?
468700091106       //?(estensione per serie parziale)                              ?
468800091105       //--------------------------------------------------------------
468900091105       BEGSR  sr_Annull3CP;
469000091105
469100091106         // -?Annullamento serie parziale?
469200091106         k_TBEcod = '3CP';
469300091106         k_TBEke1 = V1Ccli;
469400091106         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
469500091106         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
469600091106
469700091106         DoW  Not %eof(TNTBE01L);
469800091106
469900091106           if  TBEatb <> 'A';
470000091106             TBEatb = 'A';
470100091106             clear  TBEftr;
470200091106             //_______________
470300091106             update  TNTBE000;
470400091106             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
470500091106           endif;
470600091106
470700091106           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
470800091106
470900091106         EndDo;
471000091105
471100091105       ENDSR;
471200100215
471300100215       //--------------------------------------------------------------
471400100215       //?Ripristino dati in tab. "3EW"                                ?
471500100215       //?(dati assegnati alla postazione "EasyWEB")                   ?
471600100215       //--------------------------------------------------------------
471700120207       BEGSR  sr_Ripris3EW;
471800120207
471900120207         // -?Ripristino dati assegnati alla postazione "EasyWEB"?
472000120207         //k_TBEcod = '3EW';
472100120207         k_TBEcod = %subst( X3EWke1 : %len(X3EWke1) - 3 + 1 : 3 );
472200120207         k_TBEke1 = TIVSSds.VSSksu;
472300120207         k_TBEke2 = TIVSSds.VSSsun;
472400120207         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
472500120816         if  %found(TNTBE01L)   and   TBEatb = *blank;
472600120816           d3EW_Temp = TBEuni;
472700120816         endif;
472800120207
472900120207         Select;
473000120207
473100120207           // -?Ripristino record?
473200120207           When  %found(TNTBE01L)   and   TBEatb = 'A';
473300120816             clear  d3EW.§3EWfaa;
473400120816             clear  d3EW.§3EWdaa;
473500121102             d3EW.§3EWuum = DUTute;
473600120816             TBEuni = d3EW;
473700120207             clear  TBEatb;
473800120207             clear  TBEftr;
473900120207             //_______________
474000120207             update  TNTBE000;
474100120207             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
474200120816
474300120816           // -?Ripristino applicativo?
474400120816           When  %found(TNTBE01L)   and   TBEatb = *blank  and
474500120816                 d3EW_Temp.§3EWfaa <> *blank;
474600120816             //clear  d3EW;   ?(la ds deve risultare già valorizzata)?
474700120816             //...            ?(la ds deve risultare già valorizzata)?
474800120816             clear  d3EW.§3EWfaa;
474900120816             clear  d3EW.§3EWdaa;
475000121102             d3EW.§3EWuum = DUTute;
475100120816             TBEuni = d3EW;
475200120816             TBEftt = 'S';
475300120816             if  w_SisInf = 1;
475400120816               TBEftr = 'T';
475500120816             else;
475600120816               TBEftr = 'R';
475700120816             endif;
475800120816             TBEdtr = wDate;
475900120816             //_______________
476000120816             update  TNTBE000;
476100120816             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
476200120207
476300120207           // -?Scrittura record (appena cancellato fisicamente)?
476400120207           When  Not %found(TNTBE01L);
476500120207             clear  TNTBE000;
476600120207             TBEapl = X3EWapl;
476700120207             TBEcod = k_TBEcod;
476800120207             TBEke1 = k_TBEke1;
476900120207             TBEke2 = k_TBEke2;
477000121102             //clear  d3EW;   ?(la ds deve risultare già valorizzata)?
477100121102             //...            ?(la ds deve risultare già valorizzata)?
477200120207             TBEuni = d3EW;
477300120207             TBEftt = 'S';
477400120207             if  w_SisInf = 1;
477500120207               TBEftr = 'T';
477600120207             else;
477700120207               TBEftr = 'R';
477800120207             endif;
477900120207             TBEdtr = wDate;
478000120207             //______________
478100120207             write  TNTBE000;
478200120207             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
478300120207
478400120207         EndSl;
478500120207
478600120207       ENDSR;
478700100322
478800100322       //--------------------------------------------------------------
478900100402       //?Aggiornamento dati segnacolli in tab. "3EW"                  ?
479000100322       //?(dati assegnati alla postazione "EasyWEB")                   ?
479100100322       //--------------------------------------------------------------
479200100517       //BEGSR  sr_Update3EW;
479300100401
479400100402         // -?Aggiornamento dati assegnati alla postazione "EasyWeb"?
479500100402         //  ?(segnacolli)?
479600100402         //  ?N.B. - Il flag §3EWUPD verrà aggiornato DOPO, con la?
479700100402         //         ?procedura UB§3EWUPD.?
479800100517       //k_TBEcod = '3EW';
479900100517       //k_TBEke1 = TIVSSds.VSSksu;
480000100517       //k_TBEke2 = TIVSSds.VSSsun;
480100100517       //chain  %kds( k05tntbe01 : 3 )  TNTBE000;
480200100517       //
480300100517       //if  %found(TNTBE01L);
480400100517       //
480500100517       //  d3EW    = TBEuni;
480600100517       //  if  V2Dfls > *zero;
480700100517       //    d3EW.§3EWfls = %int(V2Dfls);
480800100517       //  else;
480900100517       //    clear  d3EW.§3EWfls;
481000100517       //  endif;
481100100517       //  d3EW.§3EWnrs = V2Cnrs;
481200100517       //  d3EW.§3EWnsi = V2Cncd;
481300100517       //  d3EW.§3EWnsf = V2Cnca;
481400100517       //  d3EW.§3EWctm = V2Cctm;
481500100517       //  TBEuni = d3EW;
481600100517       //  TBEapl = X3EWapl;
481700100517       //  TBEftt = 'S';
481800100517       //  clear  TBEflt;
481900100517       //  if  w_SisInf = 1;
482000100517       //    TBEftr = 'T';
482100100517       //  else;
482200100517       //    TBEftr = 'R';
482300100517       //  endif;
482400100517       //  TBEdtr = wDate;
482500100517       //  //_______________
482600100517       //  update  TNTBE000;
482700100517       //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
482800100517       //
482900100517       //endif;
483000100401
483100100517       //ENDSR;
483200100215
483300100215       //--------------------------------------------------------------
483400100215       //?Annullamento dati in tab. "3EW"                              ?
483500100215       //?(dati assegnati alla postazione "EasyWEB")                   ?
483600100215       //--------------------------------------------------------------
483700101026       BEGSR  sr_Annull3EW;
483800100215
483900100215         // -?Annullamento dati assegnati alla postazione "EasyWEB"?
484000101026         k_TBEcod = '3EW';
484100120518         select;
484200120518           when  $Bypass_01  and  wCount_3EW = 1;
484300120726             //k_TBEke1 = TNTB79ds.i3EWksu;
484400120726             k_TBEke1 = TNTB79ds.o3EWksu;
484500120518             k_TBEke2 = TNTB79ds.o3EWsun;
484600120726           when  Old_§3Ccks = V2Ccks  and  TIVSSds.VSSksu > *zero
484700120726                                      and  TIVSSds.VSSsun > *zero;
484800120518             k_TBEke1 = TIVSSds.VSSksu;
484900120518             k_TBEke2 = TIVSSds.VSSsun;
485000120518           other;
485100120518             k_TBEke1 = TIVSSds_Orig.VSSksu;
485200120518             k_TBEke2 = TIVSSds_Orig.VSSsun;
485300120518         endsl;
485400101026         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
485500101026
485600101026         if  %found(TNTBE01L)   and   TBEatb <> 'A';
485700101026
485800121102           d3EW.§3EWuum = DUTute;
485900121102           TBEuni = d3EW;
486000101026           TBEatb = 'A';
486100101026           clear  TBEftr;
486200101026           //_______________
486300101026           update  TNTBE000;
486400101026           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
486500101026
486600101026         endif;
486700100215
486800101026       ENDSR;
486900120814
487000120814       //--------------------------------------------------------------
487100120814       //?Annullamento Applicativo EasySped-WEB in tab. "3EW"          ?
487200120814       //?(dati assegnati alla postazione "EasyWEB")                   ?
487300120814       //--------------------------------------------------------------
487400120814       BEGSR  sr_AnnApp3EW;
487500120814
487600120814         // -?Annullamento "applicativo" dei dati assegnati alla?
487700120814         //  ?postazione "EasyWEB"?
487800120814         k_TBEcod = '3EW';
487900120814         select;
488000120816           when  wCount_3EW = 1  and
488100120816                 ( $Bypass_01    or  (TNTB79ds.o3EWksu > *zero  and
488200120816                                      TNTB79ds.o3EWsun > *zero) );
488300120814             k_TBEke1 = TNTB79ds.o3EWksu;
488400120814             k_TBEke2 = TNTB79ds.o3EWsun;
488500120814           when  Old_§3Ccks = V2Ccks  and  TIVSSds.VSSksu > *zero
488600120814                                      and  TIVSSds.VSSsun > *zero;
488700120814             k_TBEke1 = TIVSSds.VSSksu;
488800120814             k_TBEke2 = TIVSSds.VSSsun;
488900120814           other;
489000120814             k_TBEke1 = TIVSSds_Orig.VSSksu;
489100120814             k_TBEke2 = TIVSSds_Orig.VSSsun;
489200120814         endsl;
489300120814         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
489400120814
489500120814         If  %found(TNTBE01L)   and   TBEatb <> 'A';
489600120814
489700120814           d3EW   = TBEuni;
489800120814
489900120814           if  d3EW.§3EWfaa = *blank;
490000120814
490100120814             d3EW.§3EWfaa = 'A';
490200120814             d3EW.§3EWdaa = %editc( wDate : 'X' );
490300121102             d3EW.§3EWuum = DUTute;
490400120814             TBEuni = d3EW;
490500120814             TBEapl = X3EWapl;
490600120814             TBEftt = 'S';
490700120814             clear  TBEflt;
490800120814             if  w_SisInf = 1;
490900120814               TBEftr = 'T';
491000120814             else;
491100120814               TBEftr = 'R';
491200120814             endif;
491300120814             TBEdtr = wDate;
491400120814             //_______________
491500120814             update  TNTBE000;
491600120814             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
491700120814
491800120814           endif;
491900120814
492000120814         EndIf;
492100120814
492200120814       ENDSR;
492300130604
492400130604       //--------------------------------------------------------------
492500130604       //?Invio e-mail con errore rilevato in fase di aggiornamento    ?
492600130604       //?flag D3EW.§3EWUPD in tab. "3EW" (vedi UB§3EWUPD).            ?
492700130604       //--------------------------------------------------------------
492800130605       //BEGSR  sr_Send_eMail;
492900130605       //
493000130605       //  Qcmd = 'SNDDST type(*lmsg) +
493100130605       //          tointnet((''stefano.merighi@brt.it'')) +
493200130605       //          dstd(''Errore in aggiornamento flag §3EWUPD'') +
493300130605       //          longmsg(''Errore ' + %trim( %editc( wRtnEsito : '1') ) +
493400130605       //                  ' ricevuto dal *pgm ' + %trimr(SDSpgm) + ')';
493500130605       //
493600130605       //  exsr  sr_ExecCmd;
493700130605       //
493800130605       //ENDSR;
493900130604
494000130604       //--------------------------------------------------------------
494100130604       //?Esecuzione del comando (già impostato).                      ?
494200130604       //--------------------------------------------------------------
494300130605       //BEGSR  sr_ExecCmd;
494400130605       //
494500130605       //  clear Qcap0100;
494600130605       //  Qcabcsdh = *off;
494700130605       //  Qcapa    = *off;
494800130605       //  Qcacmdss = *off;
494900130605       //  Qcaerved = *allX'00';
495000130605       //
495100130605       //  clear Qusec;
495200130605       //  Qusbprv  = %size(Qusec);
495300130605       //
495400130605       //  ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
495500130605       //                    %size(Qcap0100) : 'CPOP0100' : *omit :
495600130605       //                    0 : 0 : Qusec);
495700130605       //
495800130605       //  //if  Qusei <> *blank;
495900130605       //  //  ...;
496000130605       //  //endif;
496100130605       //
496200130605       //ENDSR;
496300091105
496400091102      /end-free
496500091102
496600091102       //--------------------------------------------------------------
496700091102       //?Schiere a tempo di compilazione.                             ?
496800091102       //--------------------------------------------------------------
496900091105
497000140718** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
497100091127SETRAS  GAITRAGRU FILTRAGRU
497200091105AS888   GAITRAGRPSFILTRAGRPF
497300140718** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
497400091124Filiale inesistente                                                            1
497500091124Campo obbligatorio                                                             2
497600091124Ammessi SOLO caratteri numerici                                                3
497700091124Cliente NON presente in anagrafica                                             4
497800130508Tabella inesistente  e  Cliente annullato o bloccato                           5
497900091124Codice cliente UNIFICANTE obbligatorio                                         6
498000091124Codice cliente UNIFICANTE errato                                               7
498100091124Cliente UNIFICANTE non presente in tab. "3C"                                   8
498200091215Codice cliente NON modificabile perché unificante con figli:                   9
498300120305Richiesto il "Supporto cliente a BRT" del cliente UNIFICANTE:                 --
498400120305"Supporto Cliente a BRT" obbligatorio                                         11
498500120305"Supporto Cliente a BRT" errato                                               12
498600120305Cliente UNIFICANTE con "Supporto Cliente a BRT" NON "                         13
498700121112Cliente UNIFICANTE non abilitato a STRATEGI per "EasySped-WEB"                14
498800120305SERIE obbligatoria per cliente con supporto a BRT                             15
498900091125Codice Trattamento Merce errato                                               16
499000091125SERIE obbligatoria per cliente che si assegna il num. spedizione o segnacollo 17
499100091125Errore istruzione SQL-Contattare il CED                                       18
499200091125SERIE già assegnata parzialmente al cliente                                   19
499300160125SERIE già assegnata completamente in tab.3CL                                  20
499400091125SERIE già assegnata al cliente                                                21
499500120305"Supporto BRT a Cliente" indicabile SOLO sul cliente unificante               22
499600091127"Accorpamento bolle" diverso sul cliente UNIFICANTE:                          23
499700140826Lunghezza Riferimento obbligatoria SE richiesto un suo allineamento           24
499800091125Campo NON impostabile SE NON richiesto l'accorpamento bolle                   25
499900091125Numero SERIE già presente: premere F10 per richiederne uno nuovo              26
500000120113Non ci sono più numeri di SERIE attribuibili tra 1 e 98                       27
500100091125Effettuare una selezione                                                      28
500200091125Numero Colli inseribile alternativamente alla Serie ed ai relativi Segnacolli 29
500300091125Numero segnacolli "AL" obbligatorio                                           30
500400091125Range segnacolli errato (DAL > AL)                                            31
500500121112Occorre far scadere l'abilitaz. a STRATEGI per "EasySped-WEB" dell'unificante 32
500600091126Segnacolli NON disponibili per la serie indicata                              33
500700120305"Supporto BRT a Cliente" obbligatorio per UNIFICANTE con SERIE                34
500800091215Cliente NON annullabile perché unificante con figli:                          35
500900121112Cliente già abilitato ad EasyWEB in tab. "3EW"; premere "Invio" per forzare   36
501000121112Abilitazione "EasySped-WEB" scaduta per il cliente UNIFICANTE                 37
501100100216Cliente UNIFICANTE non presente in tab. "3EW"                                 38
501200100216Sostituiti dati evidenziati con quelli dell'UNIFICANTE (già in tab. "3EW")    39
501300100216Cliente UNIFICANTE presente più volte in tab. "3EW" - Premere F13 per elenco  40
501400120305"Supporto Cliente a BRT" ESWEB NON modificabile su cliente UNIFICANTE         41
501500101028Codice cliente STRATEGI obbligatorio                                          42
501600120113SERIE non gestibile                                                           43
501700121128SERIE/RANGE segnacolli NON più attribuibili al cliente se si cambia supporto  44
501800121112SERIE / Range segnacolli NON più attribuibili: già attribuiti ad EasySped-WEB 45
501900121128Range segnacolli ANOMALO:                                                     46
502000140718Il segnacollo iniziale DEVE essere multiplo di 5.000 (+ 1)                    47
502100160317Ambito e terminal partenza vanno valorizzati entrambi o vuoti entrambi        48
502200160317Terminal partenza deve essere filiale di 1° livello                           49
