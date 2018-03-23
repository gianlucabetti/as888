000100091103       //==============================================================
000200091127       //?TRTB28R * Manutenzione tab. "3C" = Invio dati BOLLETTAZIONE  ?
000300091103       //==============================================================
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
014500091102
014600091126       // -?Indicatori su DspF?
014700091102     d IndDspF         ds                  inz
014800091126         // -?Abilitazione tasti funzionali?
014900110311     d   F1Attivo                      n   overlay(IndDspF : 01)
015000121128     d   F2Attivo                      n   overlay(IndDspF : 02)
015100121128     d   F3Attivo                      n   overlay(IndDspF : 03)
015200091102     d   F5Attivo                      n   overlay(IndDspF : 05)
015300091102     d   F6Attivo                      n   overlay(IndDspF : 06)
015400091214     d   F8Attivo                      n   overlay(IndDspF : 08)
015500120517     d   F9Attivo                      n   overlay(IndDspF : 09)
015600091102     d   F10Attivo                     n   overlay(IndDspF : 10)
015700091102     d   F11Attivo                     n   overlay(IndDspF : 11)
015800091102     d   F12Attivo                     n   overlay(IndDspF : 12)
015900100212     d   F13Attivo                     n   overlay(IndDspF : 13)
016000111025     d   F14Attivo                     n   overlay(IndDspF : 14)
016100091102     d   F16Attivo                     n   overlay(IndDspF : 16)
016200091102     d   F17Attivo                     n   overlay(IndDspF : 17)
016300091102     d   F18Attivo                     n   overlay(IndDspF : 18)
016400091102     d   F19Attivo                     n   overlay(IndDspF : 19)
016500091102     d   F20Attivo                     n   overlay(IndDspF : 20)
016600091102     d   F21Attivo                     n   overlay(IndDspF : 21)
016700091102     d   F22Attivo                     n   overlay(IndDspF : 22)
016800091120     d   RlUpAttivo                    n   overlay(IndDspF : 25)
016900091126         // -?Emissione messaggio di errore?
017000091102     d   ErrMessage                    n   overlay(IndDspF : 28)
017100091126         // -?Indicatori per Attibuti di visualizzazione?
017200091102     d   VisBloAnn                     n   overlay(IndDspF : 40)
017300091124     d   VisInfoES                     n   overlay(IndDspF : 41)
017400091125     d   ProtectNRS                    n   overlay(IndDspF : 42)
017500091125     d   ProtectCTM                    n   overlay(IndDspF : 43)
017600100211     d   Protect_3EW                   n   overlay(IndDspF : 44)
017700100212     d   Reverse_3EW                   n   overlay(IndDspF : 45)
017800120130     d   $FlsNrsIn3CL                  n   overlay(IndDspF : 46)
017900150127     d   VisLNP3EW                     n   overlay(IndDspF : 47)
018000091126         // -?Posizionamento cursore & Segnalazione errore?
018100091125     d   PosCurCLI                     n   overlay(IndDspF : 51)
018200091125     d   PosCurFIL                     n   overlay(IndDspF : 52)
018300091125     d   PosCurCKS                     n   overlay(IndDspF : 53)
018400091125     d   PosCurCBA                     n   overlay(IndDspF : 54)
018500091125     d   PosCurNRS                     n   overlay(IndDspF : 55)
018600091125     d   PosCurCTM                     n   overlay(IndDspF : 56)
018700091125     d   PosCurBAC                     n   overlay(IndDspF : 57)
018800091222     d   PosCurBC2                     n   overlay(IndDspF : 58)
018900091222     d   PosCurABD                     n   overlay(IndDspF : 59)
019000091104     d   PosCurLRM                     n   overlay(IndDspF : 71)
019100091104     d   PosCurALM                     n   overlay(IndDspF : 72)
019200091104     d   PosCurNOT                     n   overlay(IndDspF : 73)
019300091104     d   PosCurABC                     n   overlay(IndDspF : 74)
019400091104     d   PosCurARC                     n   overlay(IndDspF : 75)
019500091104     d   PosCurAVX                     n   overlay(IndDspF : 76)
019600091102     d   PosCurW1ncl                   n   overlay(IndDspF : 81)
019700091102     d   PosCurW1nrs                   n   overlay(IndDspF : 82)
019800091102     d   PosCurW1ncd                   n   overlay(IndDspF : 83)
019900091102     d   PosCurW1nca                   n   overlay(IndDspF : 84)
020000101028     d   PosCurW4sun                   n   overlay(IndDspF : 85)
020100160317     d   PosCurAmb                     n   overlay(IndDspF : 86)
020200091126         //   -?Riemissione videata?
020300091102     d   ErrGenerico                   n   overlay(IndDspF : 99)
020400091102
020500091126       // -?Parametri ricevuti?
020600091102     d KPJBA         e ds
020700091102
020800091126       // -?Tabelle usate:?
020900091126       // ·?"1B" = Codici Trattamento Merce?
021000091103     d ds1B          e ds                  inz
021100091126       // ·?"3C" = Clienti che inviano bolle partenza?
021200091102     d ds3C          e ds                  inz
021300091124     d ds3C_Padre    e ds                  inz  extname(ds3C)
021400091124     d                                          prefix(P_)
021500100921     d ds3C_Old      e ds                  inz  extname(ds3C)
021600100921     d                                          prefix(Old_)
021700091126       // ·?"3CE" = Versioni Easy-Sped per cliente?
021800091102     d d3CE          e ds                  inz
021900091126       // ·?"3CP" = Serie parziali assegnate a clienti?
022000091102     d d3CP          e ds                  inz
022100110311     d d3CP_Padre    e ds                  inz  extname(d3CP)
022200110311     d                                          qualified
022300100211       // ·?"3EW" = Dati assegnati alla postazione EasyWeb?
022400100326     d d3EW          e ds                  inz  qualified
022500110311     d d3EW_Padre    e ds                  inz  extname(d3EW)
022600110311     d                                          qualified
022700121112     d d3EW_Old      e ds                  inz  extname(d3EW)
022800121112     d                                          qualified
022900120814     d d3EW_Temp     e ds                  inz  extname(d3EW)
023000120814     d                                          qualified
023100110629
023200110629       // -?Abilitazione al servizio internet del padre originale?
023300110629     d TIVSSds_Orig  e ds                  inz  extname(TIVSS00F)
023400110629     d                                          qualified
023500091106
023600091126       // -?Tracciato record file TNTBE00F per dati di base?
023700091126       //  ?della tab. "3CP"?
023800091106     d xTNTBE_3CPds  e ds                  inz  extname(TNTBE00F)
023900091106     d                                          prefix(X3CP : 3)
024000100401
024100100401       // -?Tracciato record file TNTBE00F per dati di base?
024200100401       //  ?della tab. "3EW"?
024300100401     d xTNTBE_3EWds  e ds                  inz  extname(TNTBE00F)
024400100401     d                                          prefix(X3EW : 3)
024500091105
024600091126       // -?Ridefinizione elenco librerie in elaborare le tabelle?
024700091105     d ds_Libr         ds                  inz
024800120605     d   $Libr                       10    dim(c_NrLibr) inz
024900091102
025000091102       //--------------------------------------------------------------
025100091102       //?Definizione variabili globali.                               ?
025200091102       //--------------------------------------------------------------
025300091102
025400091126       // -?Flags booleani?
025500091102     d $Fine           s               n   inz
025600091102     d $InzD01         s               n   inz(*on)
025700091102     d $InzD02         s               n   inz(*on)
025800091104     d $InzW01         s               n   inz(*on)
025900091104     d $InzW02         s               n   inz(*on)
026000101028     d $InzW04         s               n   inz(*on)
026100111011     d $InzW05         s               n   inz(*on)
026200091102     d $Called         s               n   inz
026300100514     d $3EWupd         s               n   inz
026400120807     d $TrTb06         s               n   inz
026500091102     d $Protect        s               n   inz
026600091106     d $Found_3CP      s               n   inz
026700100312     d $Found_TIVSS    s               n   inz
026800120605     d $Annull_3EW     s               n   inz
026900091106     d $KB4            s               n   inz
027000091125     d $TIS7701        s               n   inz
027100100212     d $EasyWeb        s               n   inz
027200120518     d $Bypass_01      s               n   inz
027300121128     d $F2inD2         s               n   inz
027400140718     d $CtrlRngSgnCl   s               n   inz
027500091102
027600091126       // -?Variabili per la gestione del video?
027700091102     d $Video          s              2    inz('D1')
027800091106     d $VideoPrec      s                   like($Video)  inz
027900091102
028000091126       // -?Codice cliente eventualmente ricevuto?
028100091104     d W0Ccli          s                   like(V1Ccli)  inz
028200100211
028300100211       // -?Conteggio dei rec. in tab. "3EW" per l'unficante?
028400120725     d wCount_3EW      s              3  0 inz(*loval)
028500110311
028600110311       // -?Segnacollo iniziale per padre (da tab."3CP")?
028700110311     d wNSI_Padre      s                   like(V2Cncd)  inz
028800091102
028900091126       // -?Memo per controllo variazioni a video?
029000100623     d SavNRS          s                   like(V2Cnrs)  inz(*hival)
029100120131     d*// SavNCD          s                   like(V2Cncd)  inz(*hival)
029200120131     d*// SavNCA          s                   like(V2Cnca)  inz(*hival)
029300100623     d SavABD          s                   like(V2Cabd)  inz(*hival)
029400100623     d SavCKS          s                   like(V2Ccks)  inz(*hival)
029500100623     d SavCBA          s                   like(V2Ccba)  inz(*hival)
029600100623     d SavCTM          s                   like(V2Cctm)  inz(*hival)
029700120725     d SaveFLS         s                   like(V2Dfls)  inz
029800150127     d SaveLNP         s                   like(V23EWLNP)  inz
029900120725     d SaveNRS         s                   like(V2Cnrs)  inz
030000120725     d SaveNCD         s                   like(V2Cncd)  inz
030100120725     d SaveNCA         s                   like(V2Cnca)  inz
030200120725     d SaveCTM         s                   like(V2Cctm)  inz
030300140718     d SavW1Cnrs       s                   like(W1Cnrs)  inz
030400140718     d SavW1Cncd       s                   like(W1Cncd)  inz
030500140718     d SavW1Cnca       s                   like(W1Cnca)  inz
030600121220     d Save_3CP_nrs    s                   like(V2Cnrs)  inz
030700121220     d Save_3CP_nsi    s                   like(V2Cncd)  inz
030800121220     d Save_3CP_nsf    s                   like(V2Cnca)  inz
030900100623     d SavTBLkey_tmp   s                   like(TBLkey)  inz(*hival)
031000110314     d SavCliTab3CPEW  s             10a                 inz(*hival)
031100110422     d SavF1Attivo     s               n                 inz
031200160125     d Save3CLNrS      s                   like(I54Nrs)  inz
031300160125     d Save3CLLNP      s                   like(I54Fil)  inz
031400091106
031500091126       // -?Nome del sistema?
031600091106     d currSysNeta     s              8a   inz
031700091105
031800091126       // -?Nome esteso Libreria/File dei 2 file tabella?
031900091109     d wLibFile1       s             21a   inz
032000091109     d wLibFile2       s             21a   inz
032100091102
032200091126       // -?Campi di comodo?
032300091105     d wDate_EUR       s               d   datfmt(*eur)  inz
032400130520     d wDate_DMY       s               d   datfmt(*dmy)  inz
032500091105     d wDate           s              8  0 inz
032600130520     d wDate6          s              6  0 inz
032700091105     d w_iSystem       s              1  0 inz
032800091105     d w_SisInf        s              3  0 inz
032900110616     d wVSSksu         s                   like(TIVSSds.VSSksu)
033000110616     d                                     inz
033100140718     d*// wW1Cncx         s                   like(W1Cncd)  inz
033200140317     d wCnca           s              8  0 inz
033300091102
033400091102       //--------------------------------------------------------------
033500091102       //?Definizione prototipi procedure.                             ?
033600091102       //--------------------------------------------------------------
033700091126
033800091126       // -?Reperimento dati utente?
033900110209     d TIBS34ds      e ds                  inz
034000091102      /copy gaitrasrc/srcProtoPR,TIBS34R
034100091102
034200110209       // -?Parametri per pgm interrogazione tabelle (TABEL)?
034300100308      /copy gaitrasrc/srcProtoPI,X§TABER
034400110209       // -?Interrogazione tabelle (TABEL)?
034500091102      /copy gaitrasrc/srcProtoPR,X§TABER
034600110209       // -?Parametri per Ricerca/Controllo tabelle (TNTBE)?
034700110209     d TIBS02ds      e ds                  inz
034800110209     d   T02mod      e                     inz('R')
034900110209     d   T02cod      e                     inz('3CS')
035000110209       // -?Ricerca/Controllo tabelle (TNTBE)?
035100091106      /copy gaitrasrc/srcProtoPR,TIBS02R
035200091126
035300091126       // -?Reperimento dati tabelle?
035400091104      /copy gaitrasrc/srcProtoPR,TRULTAB
035500091126
035600110209       // -?Parametri per pgm controllo/decodifica cliente?
035700110209      /copy gaitrasrc/srcProtoPI,TIBS69R
035800091126       // -?Controllo/Decodifica cliente?
035900091124      /copy gaitrasrc/srcProtoPR,TIBS69R
036000130520
036100130520       // -?Parametri per pgm ricerca data blocco cliente?
036200130520     d TRULBLCds     e ds                  inz
036300130520       // -?Ricerca data blocco cliente?
036400130520      /copy gaitrasrc/srcProtoPR,TRULBLCR
036500091102
036600100308       // -?Parametri per pgm interrogazione sottoconti?
036700100308      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
036800091126       // -?Interrogazione sottoconti?
036900091124      /copy gaitrasrc/srcProtoPR,XALFA3BR
037000100212
037100100308       // -?Parametri per verifica abilitazione STRATEGI?
037200100308      /copy gaitrasrc/srcProtoPI,TIS7701R
037300100308     d TIVSSds       e ds                  extname(TIVSS00F) qualified
037400100212       // -?Verifica abilitazione STRATEGI?
037500100212      /copy gaitrasrc/srcProtoPR,TIS7701R
037600100212
037700100308       // -?Parametri per reperimento legami in tab. "3C"?
037800100308      /copy gaitrasrc/srcProtoPI,UBLEG3C
037900100212       // -?Reperimento legami in tab. "3C"?
038000100212      /copy gaitrasrc/srcProtoPR,UBLEG3C
038100100326
038200100326       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
038300130604     d wRtnEsito       s             10i 0 inz
038400130605      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
038500091102
038600091126       // -?Gestione scambio dati Cons.Giac.Disp. con clienti?
038700091124      /copy gaitrasrc/srcProtoPR,TRTB40R
038800091102
038900091126       // -?Gestione scambio dati C/A e fattura con clienti?
039000091124      /copy gaitrasrc/srcProtoPR,TRTB44R
039100091126
039200091126       // -?Multimembro clienti VAB?
039300091124      /copy gaitrasrc/srcProtoPR,TRTB83R
039400091102
039500091126       // -?Gestione paticolarità DISK "C"?
039600091124      /copy gaitrasrc/srcProtoPR,TNTB10R
039700091102
039800091126       // -?Gestione serie per DISK "C" con prefisso "CC"?
039900091124      /copy gaitrasrc/srcProtoPR,TNTB13R
040000091102
040100091126       // -?Attribuzione numero serie parziale libero per filiale?
040200110209     d TNTB54ds      e ds                  inz
040300091124      /copy gaitrasrc/srcProtoPR,TNTB54R
040400091102
040500111215       // -?Attribuzione numero serie libero x filiale?
040600111215      /copy gaitrasrc/srcProtoPI,TRTB28R2
040700111215      /copy gaitrasrc/srcProtoPR,TRTB28R2
040800111215
040900111215       // -?Interrogazione tabella "3C" (oltre 9.999 record)?
041000111215     d trtb28r0        pr                  extpgm('TRTB28R0')
041100111215     d   kpjba                             likeds(KPJBA)
041200091214
041300091214       // -?Interrogazione legami tabelle VAS?
041400110209     d TRTB06ds      e ds                  inz
041500120605     d   d063C       e                     inz('S')
041600120605     d   d063K       e                     inz('S')
041700120605     d   d063R       e                     inz('S')
041800120605     d   d064C       e                     inz('S')
041900120605     d   d063Q       e                     inz('S')
042000120605     d   d06VAP      e                     inz('S')
042100120605     d   d06DSC      e                     inz('S')
042200120806     d   d06LAC      e                     inz('S')
042300120806     d   d06OSR      e                     inz('S')
042400091214     d trtb06r         pr                  extpgm('TRTB06R')
042500091214     d   kpjba                             likeds(KPJBA)
042600100211
042700100211       // -?Interrogazione clienti in tab. "3EW"?
042800110209     d tntb79ds      e ds                  qualified   inz
042900100211      /copy gaitrasrc/srcProtoPR,TNTB79R1
043000111011
043100111011       // -?Reperimento ultimo segnacollo utilizzato?
043200111011      /copy gaitrasrc/srcProtoPI,ubLastNSC
043300111011      /copy gaitrasrc/srcProtoPR,ubLastNSC
043400121128
043500121128       // -?"Stacca" figli dall'unificante in manutenzione?
043600121128     d TRTB28ds4     e ds                  inz
043700121128     d   oTB28err    e                     inz(*off)
043800121128     d trtb28r4        pr                  extpgm('TRTB28R4')
043900121128     d   kpjba                             likeds(KPJBA)
044000091105
044100091126       // -?Reperimento NETA sistema AS/400 corrente?
044200111011      /copy gaitrasrc/srcProtoPR,UBRTVNETA
044300130604
044400130605      *// // -?Parametri API QCAPCMD (Process Commands)?
044500130605     d*// Qcmd            s           2048    inz  varying
044600130605      *///copy qSysInc/qRpgleSrc,QCAPCMD
044700130605      *// // -?API QCAPCMD (Process Commands)?
044800130605      *///copy gaitrasrc/srcProtoPR,QCAPCMD
044900130605      *//
045000130605      *// // -?Parametri gestione errori API.?
045100130605      *///copy qsysinc/qrpglesrc,QUSEC
045200091102
045300091102       //--------------------------------------------------------------
045400091102       //?Definizione key-list.                                        ?
045500091102       //--------------------------------------------------------------
045600091102
045700091126       // -?File TABEL00F?
045800091102     d k03tabel00    e ds                  extname(TABEL00F : *key)
045900091102     d                                     prefix(k_)   inz
046000091102
046100091126       // -?File TNTBE01L?
046200091102     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
046300091102     d                                     prefix(k_)   inz
046400091102
046500091102       //--------------------------------------------------------------
046600091102       //?M A I N - L I N E                                            ?
046700091102       //--------------------------------------------------------------
046800091102
046900091102     c     *Entry        plist
047000091102     c                   parm                    KPJBA
047100091102
047200091102      /free
047300091102
047400091126       // -?Operazioni iniziali?
047500091126       exsr sr_RoutInz;
047600091102
047700091126       // -?Gestione videate?
047800091102       DoW  $Fine = *off;
047900091102
048000091102         select;
048100100308           // -?Gestione videata D1 (richiesta codice cliente)?
048200091102           when $Video = 'D1';
048300091102             exsr sr_GesD01;
048400100308           // -?Gestione videata D2 (manutenzione dati cliente)?
048500091102           when $Video = 'D2';
048600091102             exsr sr_GesD02;
048700100308           // -?Gestione videata D3 (manutenzione altri dati cliente)?
048800091102           when $Video = 'D3';
048900091102             exsr sr_GesD03;
049000100308           // -?Gestione videata W1 (ricerca serie segnacolli parziale)?
049100091104           when $Video = 'W1';
049200091104             exsr sr_GesW01;
049300120605           // -?Gestione videata W2 (dati tab. "3CE")?
049400091104           when $Video = 'W2';
049500091104             exsr sr_GesW02;
049600120605           // -?Gestione videata W3 (messaggo di avviso) - NON QUI?
049700120605           //when $Video = 'W3';
049800120605           //  exsr sr_GesW03;
049900101027           // -?Gestione videata W4 (richiesta cod. cliente Strategi)?
050000101027           when $Video = 'W4';
050100101027             exsr sr_GesW04;
050200111011           // -?Gestione videata W5 (ultimo segnacollo reperito)?
050300111011           when $Video = 'W5';
050400111011             exsr sr_GesW05;
050500100308           // -?? ? ? ? ??
050600091102           other;
050700091102             $Fine = *on;
050800091102         endsl;
050900091102
051000091102       enddo;
051100091102
051200091126       // -?Operazioni finali?
051300091102       exsr sr_RoutEnd;
051400091102
051500091102       //--------------------------------------------------------------
051600091102       //?Operazioni iniziali.                                         ?
051700091102       //--------------------------------------------------------------
051800091102       BEGSR  sr_RoutInz;
051900091102
052000091102         *inLR = *on;
052100091105
052200091126         // -?Verifica del sistema AS/400 corrente?
052300091105         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
052400091105           $Fine = *on;
052500091105           leavesr;
052600091105         endif;
052700091105
052800091126         // -?Impostazione elenco librerie in cui gestire le tabelle?
052900091126         //  ?(a seconda del sistema in cui si stà lavorando)?
053000091105         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
053100091105         if  w_iSystem = *zero;
053200091105           $Fine = *on;
053300091105           leavesr;
053400091105         endif;
053500091102
053600091126         // -?Reperimento dati job?
053700091102         exsr  sr_DatiJob;
053800091102
053900091126         // -?Impostazione nome programma a video?
054000091102         V1Tpgm = SDSpgm;
054100091102
054200091126         // -?Impostazione campi "fissi"?
054300091102         k_TBLkut = c_Kut;
054400091102         k_TBLcod = c_Tab;
054500091102
054600091126         // -?Verifica se *pgm richiamato:?
054700100308         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
054800100308         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
054900091102         $Called = (%subst( KPJBU : 1 : %len(V1Ccli) ) <> *blank);
055000100514         $3EWupd = ($Called  and
055100100514                    %subst( KPJBU : %len(V1Ccli) + 1 : 1 ) = 'S');
055200120807         $TrTb06 = ($Called  and
055300120807                    %subst( KPJBU : %len(V1Ccli) + 1 : %len(SDSpgm) )
055400120807                    = 'TRTB06R   ');
055500091102         if  $Called = *on   and
055600091106             %check( c_Digits : %subst( KPJBU : 1 :
055700091104                                       %len(V1Ccli) ) ) = *zero;
055800091104           W0Ccli = %subst( KPJBU : 1 : %len(V1Ccli) );
055900091102           //$InzD01 = *off;
056000091102           //exsr  sr_InzD01;
056100091102           //exsr  sr_CtrD01;
056200091102           //if  Not ErrGenerico;
056300091102           //  $Video = 'D2';
056400091102           //endif;
056500091102         endif;
056600091105
056700091126         // -?Reperimento data odierna?
056800091105         wDate = %int( %subst( %char( %dec( %timestamp() ) )
056900091105                               : 1 : 8 ) );
057000091106
057100091126         // -?Aggancio dati generali della tabella "3CP"?
057200091106         clear  xTNTBE_3CPds;
057300091106         X3CPke1 = *zero;
057400091106         %subst( X3CPke1 : %len(X3CPke1) - 3 + 1 : 3 ) = '3CP';
057500091106         getTabella ( c_Tntbe : *blank : X3CPke1 :
057600091106                      *blank  : *blank : *blank  :
057700091106                      *omit : *omit :
057800091106                      *omit : *omit : *omit : X3CPapl :
057900091106                      *omit : *omit : *omit : *omit :
058000091106                      *omit : X3CPuni );
058100100401
058200100401         // -?Aggancio dati generali della tabella "3EW"?
058300100401         clear  xTNTBE_3EWds;
058400100401         X3EWke1 = *zero;
058500101028         %subst( X3EWke1 : %len(X3EWke1) - 3 + 1 : 3 ) = '3EW';
058600100401         getTabella ( c_Tntbe : *blank : X3EWke1 :
058700100401                      *blank  : *blank : *blank  :
058800100401                      *omit : *omit :
058900100401                      *omit : *omit : *omit : X3EWapl :
059000100401                      *omit : *omit : *omit : *omit :
059100100401                      *omit : X3EWuni );
059200100211
059300091109         cloTabella( c_Tntbe );
059400091102
059500091102       ENDSR;
059600091102
059700091102       //--------------------------------------------------------------
059800091102       //?Reperimento Dati del job (Utente/Operativi).                 ?
059900091102       //--------------------------------------------------------------
060000091102       BEGSR  sr_DatiJob;
060100091102
060200091102         in(e) §AzUte;
060300091102         if NOT %error;
060400091102           in(e) §DatiUte;
060500091102         endif;
060600091102         if %error or RSut = *blank;
060700091102           tibs34r ( tibs34ds );
060800091102           in §AzUte;
060900091102           in §DatiUte;
061000091102         endif;
061100091102
061200091102       ENDSR;
061300091102
061400091102       //--------------------------------------------------------------
061500091102       //?Operazioni finali.                                           ?
061600091102       //--------------------------------------------------------------
061700091102       BEGSR  sr_RoutEnd;
061800091104
061900091104         cloTabella();
062000091102
062100110209         reset  TIBS69ds;
062200091102         tibs69r( tibs69ds :
062300091102                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
062400091125
062500091125         if  $TIS7701;
062600091125           Selettore_Record_TIVSS ( 'SETONLR' );
062700091125         endif;
062800100212
062900100212         ubLeg3C_Close ();
063000091102
063100091102         return;
063200091102
063300091102       ENDSR;
063400091102
063500091102       //--------------------------------------------------------------
063600091102       //?Gestione videata D01.                                        ?
063700091102       //--------------------------------------------------------------
063800091102       BEGSR  sr_GesD01;
063900091102
064000091126         // -?Inizializzazione videata?
064100091102         if  $InzD01 = *on;
064200091102           exsr  sr_InzD01;
064300091102           $InzD01 = *off;
064400091102         endif;
064500091102
064600091126         // -?SE ricevuto codice cliente NON si emette la prima videata?
064700091104         if  $Called   and   W0Ccli > *zero;
064800091102           exsr  sr_CtrD01;
064900091102           if  Not ErrGenerico;
065000091102             $Video  = 'D2';
065100091102             $InzD02 = *on;
065200091102             leavesr;
065300091102           else;
065400091104             clear W0Ccli;
065500091102           endif;
065600091102         endif;
065700091102
065800091126         // -?Emissione Testata?
065900091127         write  TB28T1;
066000091102
066100091126         // -?Emissione videata?
066200091127         exfmt  TB28D1;
066300091102
066400091104         clear  VIDmsg;
066500091102         reset  ErrMessage;
066600091102         reset  ErrGenerico;
066700091102
066800091102         SELECT;
066900091102
067000091126           // -?F3=Fine?
067100091102           WHEN  dsp_aid = c_F03;
067200091102             $Fine = *on;
067300091102
067400091126           // -?F17=Gestione serie per DISK "C" con prefisso "CC"?
067500091126           // -?F18=Gestione paticolarità DISK "C"?
067600091126           // -?F19=Multimembro clienti VAB?
067700091126           // -?F20=Gestione scambio dati principali?
067800091126           // -?F21=Gestione scambio dati Cons.Giac.Disp. con clienti?
067900091126           // -?F22=Gestione scambio dati C/A e fattura con clienti?
068000091102           WHEN  dsp_aid = c_F17   or   dsp_aid = c_F18   or
068100091102                 dsp_aid = c_F19   or   dsp_aid = c_F20   or
068200091102                 dsp_aid = c_F21   or   dsp_aid = c_F22;
068300091102             exsr  sr_FxxD01;
068400091102
068500091126           // -?Invio?
068600091102           OTHER;
068700091102             exsr sr_CtrD01;
068800091102             if  ErrGenerico = *off;
068900091102               $Video    = 'D2';
069000091102               $InzD02   = *on;
069100091102             endif;
069200091102
069300091102         ENDSL;
069400091102
069500091102       ENDSR;
069600091102
069700091102       //--------------------------------------------------------------
069800091102       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
069900091102       //--------------------------------------------------------------
070000091102       BEGSR  sr_InzD01;
070100091102
070200091102         clear  V1Topz;
070300091102         clear  BloAnn;
070400091120         VisBloAnn = *off;
070500091127         clear  TB28D1;
070600091102
070700091126         // -?Se è stato richiamato con un codice cliente numerico, si?
070800091126         //  ?imposta anche tale codice a video?
070900091102         select;
071000091104           when  $Called   and   W0Ccli > *zero;
071100091104             V1Ccli = W0Ccli;
071200091102           when  Not $Called;
071300091102             V1Ccli = '?';
071400091102         endsl;
071500091102
071600091126         // -?Abilitazione tasti funzionali?
071700091102         exsr  sr_AbilFxxD01;
071800091102
071900091102       ENDSR;
072000091102
072100091102       //--------------------------------------------------------------
072200091102       //?Abilitazione tasti funzionali in P01 (per D01).              ?
072300091102       //--------------------------------------------------------------
072400091102       BEGSR  sr_AbilFxxD01;
072500091102
072600091126         // -?Se è stato richiamato: nei primi 7 caratteri della KPJBU?
072700091126         //  ?c'è il codice cliente => NON devono essere abilitati i?
072800091126         //  ?tasti funzionali F17, F18, F19, F20, F21 e F22.?
072900091102
073000091126         // ->?F17 = Gestione serie per disk "C" con prefisso "CC"?
073100091102         F17Attivo = (Not $Called);
073200091102
073300091126         // ->?F18 = Gestione particolarità per disk "C"?
073400091102         F18Attivo = (Not $Called);
073500091102
073600091126         // ->?F19 = Multimembro clienti VAB?
073700091102         F19Attivo = (Not $Called);
073800091102
073900091126         // ->?F20 = Gestione scambio dati principali?
074000111207         //F20Attivo = (Not $Called);
074100111207         F20Attivo = *on;
074200091102
074300091126         // ->?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
074400091102         F21Attivo = (Not $Called);
074500091102
074600091126         // ->?F22 = Gestione scambio dati C/A e fattura con clienti?
074700091102         F22Attivo = (Not $Called);
074800091102
074900091102       ENDSR;
075000091102
075100091102       //--------------------------------------------------------------
075200091102       //?Gestione tasti funzionali F17,F18,F19,F20,F21 e F22 in D01.  ?
075300091102       //--------------------------------------------------------------
075400091102       BEGSR  sr_FxxD01;
075500091102
075600091126         // -?Il chiamato RICHIEDE un codice numerico per verificare?
075700091126         //  ?se è stato richiamato?
075800091102         if  V1Ccli = *blank;
075900091102           KPJBU = *zero;
076000091102         else;
076100091102           KPJBU = V1Ccli;
076200091102         endif;
076300091102
076400091102         select;
076500091102
076600091126           // -?F17=Gestione serie per DISK "C" con prefisso "CC"?
076700091102           when  dsp_aid = c_F17;
076800091102             tntb13r ( kpjba );
076900091102
077000091126           // -?F18=Gestione paticolarità DISK "C"?
077100091102           when  dsp_aid = c_F18;
077200091102             tntb10r ( kpjba );
077300091102
077400091126           // -?F19=Multimembro clienti VAB?
077500091102           when  dsp_aid = c_F19;
077600091102             trtb83r ( kpjba );
077700091102
077800091126           // -?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
077900091102           when  dsp_aid = c_F21;
078000091102             trtb40r ( kpjba );
078100091102
078200091126           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
078300091102           when  dsp_aid = c_F22;
078400091102             trtb44r ( kpjba );
078500091102
078600091102         endsl;
078700091102
078800091102       ENDSR;
078900091102
079000091102       //--------------------------------------------------------------
079100091102       //?Controllo dati immessi nella videata D01.                    ?
079200091102       //--------------------------------------------------------------
079300091102       BEGSR  sr_CtrD01;
079400091102
079500091102         %subst(IndDspF : 50) = *off;
079600091103         reset  $Protect;
079700091102
079800091126         // -?Filiale di ricerca?
079900091102         if  V1Cfil > *zero;
080000091102           chain  (V1Cfil)  AZORG;
080100091102           if  NOT %found(AZORG01L);
080200091104             VIDmsg = $Msg(01);
080300091102             PosCurFIL   = *on;
080400091102             ErrMessage  = *on;
080500091102             ErrGenerico = *on;
080600091102             leavesr;
080700091102           endif;
080800091102         endif;
080900091102
081000091102         SELECT;
081100091102
081200091126           // -?Ricerca codice cliente?
081300091102           WHEN  %scan('?' : V1Ccli) > *zeros;
081400110211             //X§Tkut = c_Kut;
081500110211             //X§Tcod = c_Tab;
081600110211             //if  V1Cfil > *zero;
081700110211             //  X§Tkey = %editc( V1Cfil : 'X' );
081800110211             //else;
081900110211             //  clear  X§Tkey;
082000110211             //endif;
082100110211             //clear  X§Tdes;
082200110211             //x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
082300110211             //V1Ccli = %subst(X§Tkey : 1 : %len(V1Ccli));
082400110211             // -?Superato il numero limite di record caricabili nel?
082500110211             //  ?subfile a video!?
082600110211             kpjbu = %editc( V1Cfil : 'X' );
082700110211             trtb28r0 ( kpjba );
082800110211             V1Ccli = kpjbu;
082900091102             ErrGenerico = *on;
083000091102             leavesr;
083100091102
083200091126           // -?Ricerca alfabetica?
083300091102           WHEN  V1Dcli <> *blank   and
083400091102                (V1Ccli =  *blank   or   V1Ccli = *zero);
083500091102             xParDut = RSut;
083600091102             xParKut = 1;
083700091102             xParRag = V1Dcli;
083800091102             xParKcc = DUTkci;
083900091102             xParSta = *zero;
084000091102             xParFlr = %editc( V1Cfil : 'X');
084100091102             clear  xParDit;
084200091102             xParNum = 1;
084300091102             clear  xParKcm;
084400091102             clear  xParKsm;
084500091102             clear  xParKdm;
084600091102             clear  xParEsci;
084700091102             clear  xParErr;
084800091102             clear  xParIva;
084900091102             clear  xParCdf;
085000091102             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
085100091102                        xParSta : xParFlr : xParDit : xParNum :
085200091102                        xParKcm : xParKsm : xParKdm : xParEsci :
085300091102                        xParErr : xParIva : xParCdf );
085400091102             if  xParSta <> -1   and   xParErr = *blank;
085500091102               V1Ccli = xParKsm;
085600091102               V1Dcli = xParRag;
085700091102             endif;
085800091102             ErrGenerico = *on;
085900091102             leavesr;
086000091102
086100091126           // -?Controllo immissione codice cliente?
086200091102           WHEN  V1Ccli = *blank  or  V1Ccli  = *zero;
086300091104             VIDmsg = $Msg(02);
086400091102             PosCurCLI   = *on;
086500091102             ErrMessage  = *on;
086600091102             ErrGenerico = *on;
086700091102             leavesr;
086800091102
086900091126           // -?Controllo immisione solo caratteri numerici?
087000091106           WHEN  %check( c_Digits : V1Ccli ) > *zero;
087100091104             VIDmsg = $Msg(03);
087200091102             PosCurCLI   = *on;
087300091102             ErrMessage  = *on;
087400091102             ErrGenerico = *on;
087500091102             leavesr;
087600091102
087700091102         ENDSL;
087800091102
087900091126         // -?Controllo / decodifica cliente?
088000091102         clear  TIBS69ds;
088100091104         I69sif = knsif;
088200091102         I69kcc = DUTkci;
088300091102         I69kac = %int(V1Ccli);
088400091102         tibs69r( tibs69ds :
088500091102                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
088600091102         if  O69err = *on;
088700091104           VIDmsg = $Msg(04);
088800091102           PosCurCLI   = *on;
088900091102           ErrMessage  = *on;
089000091102           ErrGenerico = *on;
089100091102           leavesr;
089200091102         endif;
089300091102         V1Dcli = %subst( ACOrag : 1 : %len(V1Dcli) );
089400091103
089500091126         // -?Apertura file TABEL00F del 1° S.I. (sede)?
089600091125         if  w_SisInf <> 1   or   Not %open(TABEL00F);
089700091125           w_SisInf = 1;
089800091125           exsr  sr_OpenFileTab;
089900091125         endif;
090000091102
090100091126         // -?Verifica esistenza del record in TABEL?
090200091102         k_TBLkey = V1Ccli;
090300091102
090400091103         chain  %kds(k03tabel00)  TABEL;
090500091102
090600091102         select;
090700091102
090800100312           // -?Cliente annullato?
090900091103           when  ACOflg <> *blank   and   NOT %found(TABEL00F);
091000091104             VIDmsg = $Msg(05);
091100091103             PosCurCLI   = *on;
091200091103             ErrMessage  = *on;
091300091103             ErrGenerico = *on;
091400091103             leavesr;
091500091102
091600100312           // -?Cliente bloccato?
091700130315           when  ACOabl <> *blanks  and   NOT %found(TABEL00F);
091800091104             VIDmsg = $Msg(05);
091900091103             PosCurCLI   = *on;
092000091103             ErrMessage  = *on;
092100091103             ErrGenerico = *on;
092200091103             leavesr;
092300091102
092400091102         endsl;
092500091102
092600091102       ENDSR;
092700091103
092800091103       //--------------------------------------------------------------
092900091103       //?Apertura dei files tabelle nel sistema informativo impostato.?
093000091103       //--------------------------------------------------------------
093100091103       BEGSR  sr_OpenFileTab;
093200091103
093300091126         // -?Chiusura (eventuale) archivi?
093400091103         if  %open(TABEL00F);
093500091103           close  TABEL00F;
093600091103         endif;
093700091103         if  %open(TNTBE01L);
093800091103           close  TNTBE01L;
093900091103         endif;
094000091109         cloTabella();
094100091103
094200091126         // -?Apertura archivi?
094300100212         ds_Libr  = $Libraries(w_iSystem);
094400091109         wLibFile1 = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
094500091103         open  TABEL00F;
094600091109         wLibFile2 = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
094700091103         open  TNTBE01L;
094800091106
094900091106       ENDSR;
095000091102
095100091102       //--------------------------------------------------------------
095200091102       //?Gestione videata D02.                                        ?
095300091102       //--------------------------------------------------------------
095400091102       BEGSR  sr_GesD02;
095500091102
095600091126         // -?Inizializzazione videata?
095700091102         if  $InzD02 = *on;
095800091102           exsr  sr_InzD02;
095900091102           $InzD02 = *off;
096000091102         endif;
096100091125
096200091126         // -?Impostazione attributi di visualizzazione?
096300091125         exsr  sr_AttrVisD02;
096400091120
096500091126         // -?(Ri)Abilitazione tasti funzionali?
096600091120         exsr  sr_AbilFxxD02;
096700091102
096800091126         // -?Emissione Testata e Piede?
096900091127         write  TB28T1;
097000091127         write  TB28P2;
097100091102
097200091126         // -?Emissione videata?
097300120726         select;
097400120726           when  $Video <> 'D2';
097500120726             clear  ErrMessage;
097600120726             write  TB28D2;
097700120726             leavesr;
097800120726           when  Not $Protect;
097900120726             exfmt  TB28D2;
098000120726           other;
098100120726             write  TB28D2;
098200120726             exfmt  Protect;
098300120726         endsl;
098400091120
098500120202         clear  VIDmsg;
098600091102         reset  ErrMessage;
098700091102         reset  ErrGenerico;
098800091120
098900091102         SELECT;
099000110311
099100110311           // -?F1=Ricezione Serie e Range segnacolli dal padre?
099200120305           //     ?& pure il Supporto Cliente=>BRT?
099300120601           //     ?& pure il Codice Trattamento Merce?
099400110311           WHEN  dsp_aid = c_F01;
099500120601             exsr  sr_F01D02;
099600121128
099700121128           // -?F2="Stacca" figli?
099800121128           WHEN  dsp_aid = c_F02;
099900121128             exsr  sr_F02D02;
100000091102
100100091126           // -?F3=Fine?
100200091102           WHEN  dsp_aid = c_F03;
100300091102             unlock  TABEL00F;
100400091103             unlock  TNTBE01L;
100500091102             $Fine = *on;
100600091214
100700111011           // -?F8=Interrogazione legami tabelle VAS?
100800091214           WHEN  dsp_aid = c_F08;
100900091214             exsr  sr_F08D02;
101000120517
101100120517           // -?F9=Aggiornamento legami tabella "3C"?
101200120517           //  ?(del padre "originale")?
101300120517           WHEN  dsp_aid = c_F09;
101400120517             exsr  sr_ubLeg3C_Orig;
101500091104
101600091126           // -?F10=Ricerca Serie (globale)?
101700091109           WHEN  dsp_aid = c_F10;
101800091109             exsr  sr_F10D02;
101900091104
102000091126           // -?F11=Ricerca Serie Parziale?
102100091109           WHEN  dsp_aid = c_F11;
102200091109             $VideoPrec = $Video;
102300091109             $Video     = 'W1';
102400091109             reset  $InzW01;
102500091102
102600091126           // -?F12=Ritorno?
102700091102           WHEN  dsp_aid = c_F12;
102800091102             unlock  TABEL00F;
102900091103             unlock  TNTBE01L;
103000091125             clear  V1Dcli;
103100091102             $Video = 'D1';
103200100211
103300100211           // -?F13=Interrog. tab. "3EW"?
103400100211           WHEN  dsp_aid = c_F13;
103500100211             exsr  sr_F13D02;
103600111025
103700111025           // -?F14=Visualizzazione ultimo segnacollo?
103800111025           WHEN  dsp_aid = c_F14;
103900111025             $VideoPrec = $Video;
104000111025             $Video     = 'W5';
104100111025             reset  $InzW05;
104200091102
104300091126           // -?F19=Gestione multimembro clienti VAB?
104400091126           // -?F20=Gestione scambio dati principali     con clienti?
104500091126           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
104600091126           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
104700091104           WHEN  dsp_aid = c_F19   or
104800091104                 dsp_aid = c_F20   or
104900091102                 dsp_aid = c_F21   or
105000091102                 dsp_aid = c_F22;
105100091102             exsr  sr_FxxD02;
105200091105
105300091126           // -?RollUp=Videata successiva?
105400100331           WHEN  dsp_aid = c_RollUp   and   V2Cabd <> *blank;
105500091106             $VideoPrec  = $Video;
105600091105             $Video      = 'D3';
105700091104
105800091126           // -?"?" x interrogazione dati tab. "3CP"?
105900091104           WHEN  V2F3CE = '?';
106000091106             $VideoPrec = $Video;
106100091104             $Video     = 'W2';
106200091104             reset  $InzW02;
106300091102
106400091126           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
106500091102           OTHER;
106600100312             // -?Controlli eseguiti NON se F16=Annullamento?
106700091102             if  dsp_aid <> c_F16;
106800091102               exsr sr_CtrD02;
106900091106               if  Not ErrGenerico;
107000091106                 exsr sr_CtrD03;
107100091106                 if  ErrGenerico = *on;
107200091106                   $Video = 'D3';
107300091106                 endif;
107400091106               endif;
107500100312             // -?Controlli eseguiti SOLO se F16=Annullamento?
107600091214             else;
107700100312               exsr sr_CtrlANN;
107800091102             endif;
107900091214             if  ErrGenerico = *on;
108000091214               leavesr;
108100091214             endif;
108200091120             select;
108300100331               // -?Enter/F5/F6 => Videata successiva?
108400100331               //  ?               (se accorpamento bolle)?
108500100331               when  (dsp_aid = c_Enter   or   dsp_aid = c_F05   or
108600100331                      dsp_aid = c_F06)   and   V2Cabd  <> *blank;
108700100331                 $VideoPrec = $Video;
108800100331                 $Video = 'D3';
108900100312               // -?Aggiornamento?
109000091120               when  dsp_aid = c_F05   or
109100091120                     dsp_aid = c_F06   or
109200091120                     dsp_aid = c_F16;
109300091120                 exsr  sr_UpdateAll;
109400091120                 if  ErrGenerico = *on;
109500091120                   leavesr;
109600091120                 endif;
109700100331                 // ·?Uscita dal programma SE richiamato?
109800091120                 if  $Called = *on   and   W0Ccli > *zero;
109900091120                   $Fine = *on;
110000100331                 // ·?Videata iniziale altrimenti?
110100091120                 else;
110200091120                   $Video  = 'D1';
110300091120                   $InzD01 = *on;
110400091120                 endif;
110500091120             endsl;
110600091102
110700091102         ENDSL;
110800091102
110900091126         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
111000091105         if  $Video = 'D1';
111100091102           clear  V1Topz;
111200091102           clear  BloAnn;
111300091120           VisBloAnn = *off;
111400091102         endif;
111500091102
111600091102       ENDSR;
111700091102
111800091102       //--------------------------------------------------------------
111900091105       //?Inizializzazione dati nella videata D02.                     ?
112000091102       //--------------------------------------------------------------
112100091102       BEGSR  sr_InzD02;
112200091102
112300091102         reset  $Protect;
112400091125         reset  $Found_3CP;
112500100312         reset  $Found_TIVSS;
112600120605         reset  $Annull_3EW;
112700100212         reset  $EasyWeb;
112800091125         reset  VisBloAnn;
112900091125         reset  ProtectNRS;
113000091125         reset  ProtectCTM;
113100100407         reset  $Bypass_01;
113200121128         reset  $F2inD2;
113300140718         reset  $CtrlRngSgnCl;
113400110422         reset  SavF1Attivo;
113500120725         reset  wCount_3EW;
113600120725         clear  rqsKSU;
113700100312         clear  TIVSSds;
113800110629         clear  TIVSSds_Orig;
113900120813         clear  ParFIL;
114000120813         clear  ParNRS;
114100120813         reset  SavNRS;
114200120813         reset  SavABD;
114300120813         reset  SavCKS;
114400120813         reset  SavCBA;
114500120813         reset  SavCTM;
114600120813         reset  SavTBLkey_tmp;
114700120813         reset  SavCliTab3CPEW;
114800120813         clear  SaveFLS;
114900120813         clear  SaveNRS;
115000120813         clear  SaveNCD;
115100120813         clear  SaveNCA;
115200120813         clear  SaveCTM;
115300140718         clear  SavW1Cnrs;
115400140718         clear  SavW1Cncd;
115500140718         clear  SavW1Cnca;
115600121220         clear  Save_3CP_nrs;
115700121220         clear  Save_3CP_nsi;
115800121220         clear  Save_3CP_nsf;
115900101022         clear  ubLeg3C_FlgPF;
116000101022         clear  ubLeg3C_Padre;
116100101022         clear  ubLeg3C_SKC;
116200101022         clear  ubLeg3C_SKEW;
116300110629         clear  sch_SKC_Orig;
116400120130         Protect_3EW  = *off;
116500120130         Reverse_3EW  = *off;
116600120130         $FlsNrsIn3CL = *off;
116700120130         F13Attivo    = *off;
116800091125
116900091127         clear  TB28D2;
117000100312         clear  ds3C;
117100100921         clear  ds3C_Old;
117200121112         clear  d3EW_Old;
117300091102
117400091126         // -?Impostazione testata?
117500091102         clear  V1Topz;
117600091102         clear  BloAnn;
117700091102         select;
117800091102           when  Not %found(TABEL00F);
117900091102             V1Topz = '  INSERIMENTO  ';
118000091102           when  TBLflg = '*';
118100091102             V1Topz = '  RIPRISTINO   ';
118200091102           other;
118300091102             V1Topz = '   MODIFICA    ';
118400091102         endsl;
118500091102         select;
118600100312           // -?Codice annullato?
118700091102           when  ACOflg <> *blank;
118800091102             if  %found(TABEL00F);
118900130528               BloAnn   = 'CODICE ANNULLATO';
119000091102               $Protect = *on;
119100091102             endif;
119200100312           // -?Codice bloccato?
119300130528           when  ACOabl <> *blanks;
119400130528             BloAnn   = 'CODICE  BLOCCATO';
119500130520             clear  TRULBLCds;
119600130520             iBLCkcc = DUTkci;
119700130520             iBLCksc = %int(V1Ccli);
119800130520             trulBLCr ( kpjba : trulBLCds );
119900130520             if  oBLCdav > 00010101;
120000130520               wDate_Dmy = %date( oBLCdav : *iso );
120100130520               wDate6    = %dec( wDate_Dmy );
120200130520               BloAnn    = 'BLOCCATO IL ' +
120300130520                           %editw ( wDate6 : '  /  /  ');
120400130520             endif;
120500091102         endsl;
120600091102
120700091102
120800091126         // -?Impostazione dettaglio?
120900091102         V2Ccli = %int(V1Ccli);
121000091106         V2Crag = %subst( ACOrag : 1 : %len(V2Crag) );
121100091102
121200091126         // -?Solo se Modifica/Annullamento/Ripristino?
121300091102         IF  %found(TABEL00F);
121400091102
121500100921           ds3C_Old = TBLuni;
121600100921
121700091102           ds3C = TBLuni;
121800091102           V2Cnrs = §3Cnrs;
121900100120           if  §3Cfls <> *zero;
122000100120             V2Dfls = %editc( §3Cfls : 'X' );
122100100120           endif;
122200091102           V2Cokd = §3Cokd;
122300110311           //V2Crag = §3Crag;
122400091102           V2Clrm = §3Clrm;
122500091102           V2Calm = §3Calm;
122600091102           V2Cnot = §3Cnot;
122700091102           V2Cabc = §3Cabc;
122800091102           V2Cdkc = §3Cdkc;
122900091102           V2Cavx = §3Cavx;
123000091102           V2Cabd = §3Cabd;
123100091102           V2Caus = §3Caus;
123200091102           V2Carc = §3Carc;
123300091102           V2Cmvr = §3Cmvr;
123400091102           V2Ccba = §3Ccba;
123500091102           V2Cbac = §3Cbac;
123600091222           V2Cbc2 = §3Cbc2;
123700091102           V2Cctm = §3Cctm;
123800091102           V2Cfg1 = §3Cfg1;
123900160317           V2Amb  = §3CCSAMB;
124000091102           V2Cfsp = §3Cfsp;
124100160317           V2TFP  = §3CCSTFP;
124200091102           V2Ccks = §3Ccks;
124300091102           V2Cnwb = §3Cnwb;
124400091102           V2Cnwc = §3Cnwc;
124500091102           V2Crsb = §3Crsb;
124600091102           V2Cpes = §3Cpes;
124700100212
124800120517           // -?Reperimento famiglia completa "originale"?
124900120517           exsr  sr_ubLeg3C_Orig;
125000091102
125100091126           // -?Impostazione / Reperimento serie?
125200100312           // ->?PRIMA impostata con il default...?
125300091102           V2Cncd = 1;
125400091102           V2Cnca = *all'9';
125500100312           // ->?POI eventualmente reperita come parziale?
125600091102           clear  d3CP;
125700100215           clear  d3EW;
125800100215           select;
125900100215             when  V2Ccba = c_EasyWeb;
126000120130               // -?(il reperimento di tali dati verrà fatto dopo,?
126100120130               //   ?nella subr. sr_CtrlCBA)?
126200100215             when  V2Cnrs > *zero;
126300100215               k_TBEcod = '3CP';
126400100215               k_TBEke1 = V1Ccli;
126500100215               setll  %kds( k05tntbe01 : 2 )  TNTBE000;
126600100215               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
126700100215               dow  Not %eof(TNTBE01L);
126800100215                 if  %subst(TBEke2:1:2) = %editc(V2Cnrs:'X') AND
126900100215                   ( (TBLflg = '*'      and   TBEatb = 'A')   or
127000100215                     (TBLflg = *blank   and   TBEatb = *blank) );
127100100215                   d3CP = TBEuni;
127200100215                   V2Cncd = %int( %subst( TBEke2 : 3 : 7 ) );
127300100215                   V2Cnca = §3CPal;
127400100215                   $Found_3CP = *on;
127500121220                   Save_3CP_nrs = V2Cnrs;
127600121220                   Save_3CP_nsi = V2Cncd;
127700121220                   Save_3CP_nsf = V2Cnca;
127800100215                   leave;
127900100215                 endif;
128000100215                 reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
128100100215               enddo;
128200100215           endsl;
128300120726
128400120726           // -?Reperimento chiave completa per tab. "3EW"?
128500120726           //  ?se padre non trovato nel file TIVSS non può essere in?
128600120726           //  ?  tab. "3EW"?
128700120726           clear  rqsSUN;
128800120726           //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
128900120726           if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
129000120726             exsr  sr_TIS7701_x_TIVSS;
129100120726           endif;
129200120726           if  $Found_TIVSS;
129300120726             TIVSSds_Orig = TIVSSds;
129400120726           endif;
129500120726
129600120726           // -?Controllo esistenza padre in tab. "3EW" pur non essendo?
129700120726           //  ?più abilitato ad EasyWEB su STRATEGI?
129800120726           IF  NOT $Found_TIVSS        and
129900120726               //Old_§3Ccks = V2Ccli   and
130000120726               Old_§3Ccba = c_EasyWEB;
130100120726
130200120726             // ·?Reperimento dati "EasyWEB" del padre da tab. "3EW"?
130300120726             //  ?(serve per sapere se l'abilitazione ad EasyWEB del?
130400120726             //  ?padre originale è stata fatta scadere su STRATEGI,?
130500120726             //  ?ma esiste ancora la tab. "3EW")?
130600120726             ds_TNTBE.TBEke1 = '0' + %editc( Old_§3Ccks : 'X' );
130700120726             if  getTabella ( c_Tntbe : '3EW' : ds_TNTBE.TBEke1 :
130800120726                              *omit : *blank : *blank :
130900120726                              *omit : *omit :
131000120726                              *omit : *omit : *omit : *omit :
131100120726                              *omit : *omit : *omit : *omit :
131200120726                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
131300120726                          = *zero      AND
131400120726                 ds_TNTBE.TBEatb = *blank;
131500121112               d3EW_Old  = ds_TNTBE.TBEuni;
131600120814               d3EW_Temp = ds_TNTBE.TBEuni;
131700120814               if  d3EW_Temp.§3EWfaa = *blank;
131800120814                 // ·?Padre già disabilitato ad EasySped-WEB ma ancora?
131900120814                 //  ?in tab. "3EW": occorre annullare la tab. "3EW"?
132000120814                 $Annull_3EW = (Not $Found_TIVSS);
132100120814               endif;
132200120726             endif;
132300120726
132400120726           ENDIF;
132500091102
132600100312           // -?Decodifiche:?
132700091125
132800100312           // ->?Cliente Unificante?
132900120605           //clear  V2Dcks;           ?(già così)?
133000091125           exsr  sr_CtrlCKS;
133100091125           if  ErrGenerico;
133200120813             reset  SavCKS;
133300091125             clear  VIDmsg;
133400091125             clear  PosCurCKS;
133500091125             clear  ErrMessage;
133600091125             clear  ErrGenerico;
133700110818           else;
133800110818             // ->?Abilitazione F1=Numero Serie & Range Segnacolli?
133900110818             exsr  sr_CtrlRngPadre;
134000110818             if  ErrGenerico;
134100120813               reset  SavCKS;
134200110818               clear  ErrGenerico;
134300120813             else;
134400120813               SavCKS = V2Ccks;
134500110818             endif;
134600091125           endif;
134700091106
134800120305           // ->?Supporto Cliente a BRT?
134900091106           exsr  sr_CtrlCBA;
135000121112           if  wCount_3EW = 1;
135100121112             d3EW_Old = d3EW;
135200121112           endif;
135300091106           if  ErrGenerico;
135400120813             reset  SavCBA;
135500110201             reset  $Bypass_01;
135600100312             reset  $EasyWeb;
135700091106             clear  VIDmsg;
135800091106             clear  PosCurCBA;
135900091126             clear  PosCurNRS;
136000120208             clear  PosCurCKS;
136100091106             clear  ErrMessage;
136200091106             clear  ErrGenerico;
136300120813           else;
136400120813             SavCBA = V2Ccba;
136500091106           endif;
136600110311
136700110311           // ->?Numero Serie & Range Segnacolli?
136800110818           //   ?(GIÀ FATTO)?
136900110818           //exsr  sr_CtrlRngPadre;
137000110818           //if  ErrGenerico;
137100110818           //  clear  ErrGenerico;
137200120130           //endif;
137300091125
137400100312           // ->?Codice Trattamento Merce?
137500091125           exsr  sr_CtrlCTM;
137600091125           if  ErrGenerico;
137700120813             reset  SavCTM;
137800091125             clear  VIDmsg;
137900091125             clear  PosCurCTM;
138000091125             clear  ErrMessage;
138100091125             clear  ErrGenerico;
138200120813           else;
138300120813             SavCTM = V2Cctm;
138400091125           endif;
138500120208
138600120208           // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
138700120208           //   ?se NON reperito il range segnacolli per EasyWEB?
138800120726           if  V2Ccba   =  c_EasyWEB    and
138900120726              (rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
139000120726                                   or   TIVSSds.VSSksu = *zero);
139100120725             VIDmsg = $Msg(14);
139200120725             if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
139300120803                 V2Cncd = 1          and  V2Cnca = *all'9';
139400120725               // -?Abilitazione F13=Interrog. tab. "3EW"?
139500120725               F13Attivo = *on;
139600120725               //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
139700120725               VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
139800120725             endif;
139900120725             PosCurCKS   = *on;
140000120725             ErrMessage  = *on;
140100120725             ErrGenerico = *on;
140200120726             //leavesr;
140300120208           endif;
140400120725
140500120725           // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
140600120726           //   ?se cliente presente più di una sola volta nella "3EW"?
140700120726           //   ?(errore "alternativo" al precedente: meglio non uscire?
140800120726           //   ?dalla subroutine per eseguire comunque la pulizia dei?
140900120726           //   ?campi sottostante)?
141000120726           //if  wCount_3EW > 1;
141100120726           //  // -?Abilitazione F13=Interrog. tab. "3EW"?
141200120726           //  F13Attivo   = *on;
141300120726           //  VIDmsg = $Msg(40);
141400120726           //  //PosCurNRS   = *on;
141500120726           //  PosCurCKS   = *on;
141600120726           //  ErrMessage  = *on;
141700120726           //  ErrGenerico = *on;
141800120726           //  leavesr;
141900120726           //endif;
142000120726           if  wCount_3EW > 1;
142100150319             //$VideoPrec  = $Video;       ?(già fatto)?
142200150319             $Video = 'W4';
142300120726             reset  $InzW04;
142400120726             ErrGenerico = *on;
142500120726             //leavesr;
142600120726           endif;
142700091102
142800091102         ELSE;
142900091102
143000100211           V2Cfg1 = 'N';
143100100211           V2Cfsp = 'N';
143200100211           V2Cnwb = 'S';
143300100211           V2Cnwc = 'L';
143400100211           V2Cncd = 1;
143500100211           V2Cnca = *all'9';
143600091102
143700091102         ENDIF;
143800091102
143900091126         // -?Pulizia campi per controllo forzatura errore?
144000100623         reset  SavNRS;
144100120131         //reset  SavNCD;
144200120131         //reset  SavNCA;
144300100623         reset  SavABD;
144400120813         //reset  SavCKS;
144500120813         //reset  SavCBA;
144600120813         //reset  SavCTM;
144700100623         reset  SavTBLkey_tmp;
144800110311         reset  SavCliTab3CPEW;
144900091102
145000091102       ENDSR;
145100101026
145200101026       //--------------------------------------------------------------
145300101026       //?Reperimento dati di TIVSS tramite TIS7701R.                  ?
145400101026       //--------------------------------------------------------------
145500101026       BEGSR  sr_TIS7701_x_TIVSS;
145600101026
145700101026         $TIS7701 = *on;
145800101026
145900120725         rqsKSU = '0' + %editc( V2Ccks : 'X' );
146000101026         clear  rpyEsito;
146100101026         Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
146200101028                                  rqsSUN : *zero : TIVSSds :
146300101026                                  rpyEsito );
146400101026
146500101026         $Found_TIVSS = (rpyEsito = *zero  and  TIVSSds.VSSksu > *zero);
146600101026
146700101026       ENDSR;
146800091125
146900091125       //--------------------------------------------------------------
147000091125       //?Impostazione attributi di visualizzazione in D02.            ?
147100091125       //--------------------------------------------------------------
147200091125       BEGSR  sr_AttrVisD02;
147300091125
147400091126         // ->?Visualizzazione "CLIENTE BLOCCATO" in testata?
147500091125         VisBloAnn = (BloAnn <> *blank);
147600091125
147700091126         // ->?Permessa interrogazione tab. "3CE"?
147800110930         //VisInfoES = (§3Ccba = 'ESYSP'   or   §3Ccba = 'ESVAL');
147900110930         VisInfoES = (§3Ccba = 'ESYSP'   or   §3Ccba = 'ESVAL'   or
148000110930                      getTabella ( c_Tntbe : '3CE' : V1Ccli :
148100110930                                   *blank: *omit : *omit :
148200110930                                   *omit : *omit :
148300110930                                   *omit : *omit : *omit : *omit :
148400110930                                   *omit : *omit : *omit : *omit :
148500110930                                   ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
148600110930                                 = *zero      AND
148700110930                      ds_TNTBE.TBEatb = *blank);
148800091125
148900091126         // ->?Protezione "Numero Serie"?
149000091127         ProtectNRS = (V2Ccli <> V2Ccks   and   V2Ccba = c_EasyWEB
149100091127                                          and   V2Ccba = p_§3Ccba);
149200091125
149300091126         // ->?Protezione "Codice Trattamento Merce"?
149400120305         //   ?se Supporto Cliente a BRT = 'ESWEB' ANCHE per il padre?
149500091126         ProtectCTM = ((V2Ccba = c_EasyWEB  and  V2Ccks = V2Ccli)   OR
149600091126                       (V2Ccba = c_EasyWEB  and  V2Cctm = p_§3Cctm  and
149700091126                      p_§3Ccba = c_EasyWEB));
149800120130
149900120130         // ->?Numero Serie & Range Segnacolli in tab. "3CL"?
150000120130         ds_TNTBE.TBEke1 = V2Dfls;
150100120130         ds_TNTBE.TBEke2 = %editc( V2Cnrs : 'X' );
150200120130         $FlsNrsIn3CL = ( V2Cncd = 1  and  V2Cnca = *all'9'  and
150300120130                        getTabella ( c_Tntbe : '3CL' : ds_TNTBE.TBEke1 :
150400120130                                     ds_TNTBE.TBEke2 : *omit : *omit :
150500120130                                     *omit : *omit :
150600120130                                     *omit : *omit : *omit : *omit :
150700120130                                     *omit : *omit : *omit : *omit :
150800120130                                     ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
150900120130                                   = *zero      AND
151000120130                        ds_TNTBE.TBEatb = *blank );
151100150127
151200150127         // ->?Filiale gest. se supporto clienta a BRT = ESWEB presa da tab.3EW
151300150127         VisLNP3EW = (§3Ccba = 'ESWEB');
151400091125
151500091125       ENDSR;
151600091102
151700091102       //--------------------------------------------------------------
151800091102       //?Abilitazione tasti funzionali in P02 (per D02 e D03).        ?
151900091102       //--------------------------------------------------------------
152000091102       BEGSR  sr_AbilFxxD02;
152100110311
152200110311         // ->?F1 = Reperimento Serie e Range segnacolli dal padre?
152300110311         //F1Attivo = ...vedi subr. sr_CtrlRngPadre
152400110422         // ·?Comunque salvo lo stato attuale...?
152500110422         SavF1Attivo = F1Attivo;
152600121128
152700121128         // ->?F2 = "Stacca" figli?
152800130103         //F2Attivo = (%found(TABEL00F)   and   TBLflg = *blank       and
152900130103         //            $Video = 'D2'      and   V2Ccli = §3Ccks       and
153000130103         //            sch_skC(2) > *zero);
153100130103         //          //$Video = 'D2'      and   V2Ccli = Old_§3Ccks   and
153200130103         //          //sch_skC_Orig(2) > *zero);
153300130103         F2Attivo = *off;
153400121128
153500121128         // ->?F3 = Fine?
153600121128         F3Attivo = (Not $F2inD2);
153700091102
153800091126         // ->?F5 = Ripristino?
153900091102         F5Attivo = (%found(TABEL00F)   and   TBLflg <> *blank);
154000091102
154100091126         // ->?F6 = Conferma?
154200091120         F6Attivo = ( ( ( %found(TABEL00F)  and  TBLflg = *blank  and
154300091120                          Not $Protect )    OR   Not %found(TABEL00F) )
154400091120                                            AND
154500091120                      ( ( $Video = 'D2'     and  V2Cabd = *blank )
154600091120                                            OR   $Video = 'D3'));
154700091214
154800091214         // ->?F8 = Interrogazazione legami tabelle VAS?
154900120807         F8Attivo =  (Not $TrTb06   and
155000120807                     ($Video = 'D2'  or  $Video = 'W4'));
155100120517
155200120517         // ->?F9 = Aggiornamento legami tabella "3C"?
155300120803         F9Attivo =  (($Video = 'D2'  or  $Video = 'W4')
155400120803                      and  NOT $Protect);
155500091102
155600091126         // ->?F10 = Ricerca serie?
155700120113         //F10Attivo = (Not F5Attivo   and   $Video <> 'D3'
155800120113         //                            and   V2Ccba <> c_EasyWEB);
155900120113         //   ?(anche in fase di ripristino)?
156000120202         //F10Attivo = ($Video <> 'D3'  and  V2Ccba <> c_EasyWEB
156100120202         //                             and  Not $FlsNrsIn3CL);
156200120202         //   ?(anche se Filiale/Serie Segnacolli in tab. "3CL")?
156300120803         F10Attivo = (($Video <> 'D3'  and  V2Ccba <> c_EasyWEB)
156400120803                      and  NOT $Protect);
156500091102
156600091126         // ->?F11 = Ricerca serie parziale?
156700091120         F11Attivo = (F10Attivo);
156800091102
156900091126         // ->?F12 = Ritorno?
157000121128         F12Attivo = ((Not $Called   or   $Video = 'D3')  and
157100121128                       Not $F2inD2);
157200120517
157300120517         // ->?F13 = Interrogazione tab. "3EW"?
157400120517         //   ?(NON qui: abilitazione condizionata dal fallito reperim.?
157500120517         //   ? del range segnacolli in tab. "3EW" - in D2)?
157600120517         ////F13Attivo = *off;
157700120517
157800120518         // ->?F14 = Visualizzazione ultimo segnacollo?
157900120726         F14Attivo = ($Video = 'D2'  or  $Video = 'W4');
158000091102
158100091126         // ->?F16 = Annullamento?
158200100216         //F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank  and
158300100216         //             %subst( KNMUS : 1 : 3 ) = c_EDP           and
158400100216         //            (V2Ccli <> §3Ccks   or
158500100216         //            (V2Ccli =  §3Ccks   and   sch_SKC(2) = *zero)));
158600100216         //   ?(si abilita anche per cliente padre con figli, per?
158700100216         //    ?visualizzarne poi i figli nel messaggio di errore)?
158800100216         F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank  and
158900100216                      %subst( KNMUS : 1 : 3 ) = c_EDP);
159000091120
159100091126         // ->?F19 = VAB multimembro?
159200091120         F19Attivo = ($Video <> 'D3');
159300091102
159400120517         // -?Abilitazione F20, F21 e F21 SE NON richiamato:?
159500091126         // ->?F20 = Multimembro clienti VAB?
159600111207         //F20Attivo = (Not $Called   and   $Video <> 'D3');
159700111207         F20Attivo = ($Video <> 'D3');
159800091126         // ->?F21 = Gestione dati Consegna/Giacenza per clienti?
159900091120         F21Attivo = (Not $Called   and   $Video <> 'D3');
160000091126         // ->?F22 = Gestione dati C/A e Fattura per clienti?
160100091120         F22Attivo = (Not $Called   and   $Video <> 'D3');
160200091120
160300091126         // ->?RollUp (se impostato l'"accorpamento bolle")?
160400091120         RlUpAttivo = (V2Cabd <> *blank);
160500091102
160600091102       ENDSR;
160700120601
160800120601       //--------------------------------------------------------------
160900120601       //?Gestione tasto funzionale F1=Impostazione dati dal padre:    ?
161000120601       //?· Serie segnacolli                                           ?
161100120601       //?· Range segnacolli                                           ?
161200120601       //?· Supporto Cliente => BRT                                    ?
161300120601       //?· Codice Trattamento Merce                                   ?
161400120601       //--------------------------------------------------------------
161500120601       BEGSR  sr_F01D02;
161600120601
161700120601         // -?Supporto Cliente => BRT?
161800120601         V2Ccba = p_§3Ccba;
161900120601         // -?Numero Serie Segnacolli?
162000120601         V2Cnrs = p_§3Cnrs;
162100120927         // -?Filiale/Range Segnacolli?
162200120601         if  p_§3Ccba = c_EasyWEB;
162300120927           V2Dfls = %editc( d3EW_Padre.§3EWfls : 'X' );
162400120601           V2Cncd = d3EW_Padre.§3EWnsi;
162500120601           V2Cnca = d3EW_Padre.§3EWnsf;
162600150127           V23EWLNP = %editc( d3EW_Padre.§3EWLNP : 'X' );
162700120601         else;
162800121218           V2Dfls = %subst( %editc( V2Ccli : 'X' ) : 1 : 3 );
162900120601           V2Cncd = wNSI_Padre;
163000120601           V2Cnca = d3CP_Padre.§3CPal;
163100150127           V23EWLNP = *blank;
163200120601         endif;
163300120601         if  V2Cncd = *zero  and  V2Cnca = *zero;
163400120601           V2Cncd = 1;
163500120601           V2Cnca = *all'9';
163600120601         endif;
163700120601         // -?Codice Trattamento Merce?
163800120927         if  p_§3Ccba = c_EasyWEB;
163900120927           V2Cctm = d3EW_Padre.§3EWctm;
164000120927         else;
164100120927           V2Cctm = p_§3Cctm;
164200120927         endif;
164300140123
164400140123         // -?"Forzature" richiestemi da Mirko C.?
164500140123         If  V2Ccba = c_EasyWEB;
164600140123           // -?Supporto BRT a Cliente?
164700140123           if  V2Ccli = V2Ccks;
164800140123             V2Cbac = 'EMAIL';
164900140123           endif;
165000140123           // -?Abilitazione Serie in Bollettazione?
165100140123           V2Cfg1 = 'S';
165200140123         EndIf;
165300120601
165400120601         // -?DISabilitazione F1=Reperimento Serie e Range segnacolli?
165500120601         //                     ?dal padre?
165600120601         F1Attivo = *off;
165700120601
165800120601       ENDSR;
165900121128
166000121128       //--------------------------------------------------------------
166100121128       //?Gestione tasto funzionale F2="Stacca" figli                  ?
166200121128       //--------------------------------------------------------------
166300121128       BEGSR  sr_F02D02;
166400121128
166500121128         reset  TRTB28ds4;
166600121205         //iTB28ksuA = V2Ccks;
166700121205         iTB28ksuA = §3Ccks;
166800121128         kpjbu = TRTB28ds4;
166900121204
167000121204         unlock  TABEL00F;
167100121128
167200121128         TRTB28R4 ( kpjba );
167300121206
167400121206         chain  %kds(k03tabel00)  TABEL;
167500121206         //ds3C = TBLuni;           ?(già così)?
167600121128
167700121128         TRTB28ds4 = kpjbu;
167800121128
167900121128         Select;
168000121128           // -?Restituito errore?
168100121128           When  oTB28err =  *on;
168200121128             VIDmsg = oTB28msg;
168300121128             PosCurCKS   = *on;
168400121128             ErrMessage  = *on;
168500121128             ErrGenerico = *on;
168600121128             leavesr;
168700121128           // -?Premuto F6: aggiornamento legami tabella "3C" e?
168800121128           //              ?riesecuzione controllo range segnacolli?
168900121128           When  oTB28fxx =  '06';
169000121128             $F2inD2 = *on;
169100121204             V2Ccks  = oTB28ksuN;
169200121204             clear  V2Cbac;
169300121128             exsr  sr_ubLeg3C_Orig;
169400121128             clear  ubLeg3C_Padre;
169500121128             exsr  sr_Ctrl_NRS_SCx;
169600121128             if  ErrGenerico = *on;
169700121128               leavesr;
169800121128             endif;
169900121204             exsr  sr_CtrD02;
170000121204             if  ErrGenerico = *on;
170100121204               leavesr;
170200121204             endif;
170300121205             F2Attivo = *off;
170400121128         EndSl;
170500121128
170600121128       ENDSR;
170700091214
170800091214       //--------------------------------------------------------------
170900091214       //?Gestione tasto funzionale F8=Interrog. legami tab. VAS       ?
171000091214       //--------------------------------------------------------------
171100091214       BEGSR  sr_F08D02;
171200091214
171300100211         clear  trtb06ds;
171400100211         D06ksc = %int(V1Ccli);
171500100211
171600100211         kpjbu = trtb06ds;
171700091214
171800091214         trtb06r ( kpjba );
171900091214
172000091214       ENDSR;
172100091104
172200091104       //--------------------------------------------------------------
172300091104       //?Gestione tasto funzionale F10=Ricerca Serie (in D02).        ?
172400091104       //--------------------------------------------------------------
172500091104       BEGSR  sr_F10D02;
172600091104
172700091126         // -?NON modificabile manualmente premendo F10?
172800091104         if  V2Cnrs <> *zero   and  V2Cnrs <> SavNRS;
172900091104           SavNRS = V2Cnrs;
173000091125           VIDmsg = $Msg(26);
173100091104           PosCurNRS   = *on;
173200091104           ErrMessage  = *on;
173300091104           ErrGenerico = *on;
173400091104           leavesr;
173500091104         endif;
173600111215
173700111215         // -?Reperimento famiglia completa?
173800111215         //  ?(se premuto F10 PRIMA di Invio, la famiglia?
173900111215         //  ? NON è ancora stata reperita)?
174000120104         exsr  sr_ubLeg3C;
174100091104
174200091126         // -?Ricerca?
174300091104         clear  TRTB28ds2;
174400100215         if  V2Dfls > *zero;
174500100215           ParFIL = %int(V2Dfls);
174600100215         else;
174700100215           ParFIL = V2Ccli / 10000;
174800100215         endif;
174900091104         ParELB = 'R';
175000111215         trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
175100120113         select;
175200120113           // ->?Serie NON ammessa (99)?
175300120113           when  ParERR = '9';
175400120113             VIDmsg = $Msg(43);
175500120113             PosCurNRS   = *on;
175600120113             ErrMessage  = *on;
175700120113             ErrGenerico = *on;
175800120113             leavesr;
175900120113           // ->?Non reperibile una nuova serie?
176000120113           when  ParERR <> *blank;
176100120113             VIDmsg = $Msg(27);
176200120113             PosCurNRS   = *on;
176300120113             ErrMessage  = *on;
176400120113             ErrGenerico = *on;
176500120113             leavesr;
176600120113           // ->?Reperita serie completa?
176700120113           other;
176800120113             V2Cnrs = ParNRS;
176900120113             V2Cncd = 1;
177000120113             V2Cnca = *hival;
177100120113         endsl;
177200091109
177300091126         // -?Ritorno alla videata D02?
177400091109         $VideoPrec = $Video;
177500091109         $Video     = 'D2';
177600091104
177700091104       ENDSR;
177800100211
177900100211       //--------------------------------------------------------------
178000100211       //?Gestione tasto funzionale F13=Interrog. tab. "3EW"           ?
178100100211       //--------------------------------------------------------------
178200100211       BEGSR  sr_F13D02;
178300100211
178400100212         clear  tntb79ds;
178500100212         TNTB79ds.i3EWksu = '0' + %editc( V2Ccks : 'X' );
178600100211
178700100212         tntb79r1 ( kpjba : TNTB79ds );
178800101029
178900101029         if  TNTB79ds.o3EWfxx = *blank   and
179000101029             TNTB79ds.o3EWksu = '0' + %editc( V2Ccks : 'X' );
179100120215           d3EW   = TNTB79ds.o3EWuni;
179200120215           V2Dfls = %editc( d3EW.§3EWfls : 'X' );
179300150127           V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
179400120215           V2Cnrs = d3EW.§3EWnrs;
179500120215           V2Cncd = d3EW.§3EWnsi;
179600120215           V2Cnca = d3EW.§3EWnsf;
179700120215           V2Cctm = d3EW.§3EWctm;
179800120816           if  d3EW.§3EWfaa = *blank;
179900120816             wCount_3EW = 1;
180000120816             rqsSUN = TNTB79ds.o3EWsun;
180100120816             exsr  sr_TIS7701_x_TIVSS;
180200120816             $Bypass_01 = *on;
180300120816           //else;
180400120816           //  VIDmsg = $Msg(44);
180500120816           //  PosCurNRS   = *on;
180600120816           //  ErrMessage  = *on;
180700120816           //  ErrGenerico = *on;
180800120816           //  leavesr;
180900120816           endif;
181000101029         endif;
181100100211
181200100211       ENDSR;
181300091102
181400091102       //--------------------------------------------------------------
181500091104       //?Gestione tasti funzionali F19, F20, F21 e F22 in D02.        ?
181600091102       //--------------------------------------------------------------
181700091102       BEGSR  sr_FxxD02;
181800091102
181900091126         // -?Il chiamato RICHIEDE un codice numerico per verificare?
182000091126         //  ?se è stato richiamato?
182100091102         if  V1Ccli = *blank;
182200091102           KPJBU = *zero;
182300091102         else;
182400091102           KPJBU = V1Ccli;
182500091102         endif;
182600091102
182700091102         select;
182800091104
182900091126           // -?F17 = Gestione serie per Disk "C" con prefisso "CC"?
183000091104           when  dsp_aid = c_F17;
183100091104             tntb13r ( kpjba );
183200091104
183300091126           // -?F18 = Gestione particolarità Disk "C"?
183400091104           when  dsp_aid = c_F18;
183500091104             tntb10r ( kpjba );
183600091104
183700091126           // -?F19 = Gestione multimembro clienti VAB?
183800091104           when  dsp_aid = c_F19;
183900091104             trtb83r ( kpjba );
184000091102
184100091126           // -?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
184200091102           when  dsp_aid = c_F21;
184300091102             trtb40r ( kpjba );
184400091102
184500091126           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
184600091102           when  dsp_aid = c_F22;
184700091102             trtb44r ( kpjba );
184800091102
184900091102         endsl;
185000091102
185100091102       ENDSR;
185200091214
185300091214       //--------------------------------------------------------------
185400091214       //?Controllo annullabilità del record nella videata D02.        ?
185500091214       //--------------------------------------------------------------
185600100312       BEGSR  sr_CtrlANN;
185700091214
185800091214         %subst(IndDspF : 50) = *off;
185900091214
186000091214         // -?Se cliente Unificante: NON deve avere figli?
186100100322         if  %found(TNTBE01L)   and   V2Ccli = §3Ccks   and
186200120326             sch_SKC_Orig(2) > *zero;
186300120517           // -?Su richiesta di Mirko: si riesegue il caricamento dei?
186400120517           //  ?legami (potrebbero essere stati annullati tutti i figli?
186500120517           //  ?nel frattempo - da un'altra sessione video)?
186600120517           exsr  sr_ubLeg3C_Orig;
186700120517           if  sch_SKC_Orig(2) > *zero;
186800120517             VIDmsg = %trimr($Msg(35)) + ' ' +
186900120517                      %editc( sch_SKC_Orig(2) : 'X' );
187000120517             if  sch_SKC_Orig(3) > *zero;
187100120517               VIDmsg = %trimr(VIDmsg) + ' ed altri';
187200120517             endif;
187300120517             PosCurCKS   = *on;
187400120517             ErrMessage  = *on;
187500120517             ErrGenerico = *on;
187600120517             leavesr;
187700120517           endif;
187800091214         endif;
187900120214
188000120214         // -?NON consentito l'annullamento (da qui) SE cliente?
188100120329         //  ?PADRE abilitato ad EasySepd-WEB su Strategi?
188200120215         //  ?(e NON richiamato proprio per quello)?
188300120329         //  ?Un cliente figlio è annullabile anche se abilitato?
188400120329         //    ?ad EasyWEB (quindi con padre abilitato ad EasyWEB)!?
188500120214         If  Not  $3EWupd         and
188600120214             §3Ccba = c_EasyWEB   and
188700120329             §3Ccks = V2Ccli      and
188800120214             %editc(§3Ccks : 'X') = %subst(TIVSSds_Orig.VSSksu : 2);
188900120214           VIDmsg = $Msg(32);
189000120214           ErrMessage  = *on;
189100120214           ErrGenerico = *on;
189200120214           leavesr;
189300120214         EndIf;
189400101027
189500101027         // -?Se cliente Unificante presente con più codici Strategi,?
189600101027         //  ?occorre selezionare quello corretto?
189700120214         //If  wCount_3EW > 1;
189800120214         //  $VideoPrec = $Video;
189900120214         //  $Video     = 'W4';
190000120214         //  reset  $InzW04;
190100120214         //EndIf;
190200091214
190300091214       ENDSR;
190400091102
190500091102       //--------------------------------------------------------------
190600091102       //?Controllo dati immessi nella videata D02.                    ?
190700091102       //--------------------------------------------------------------
190800091102       BEGSR  sr_CtrD02;
190900091104
191000091104         %subst(IndDspF : 50) = *off;
191100100212
191200100212         // -?Spegnimento indicatori che:?
191300120305         //  ?1) memorizza l'impostaz. "Supporto Cliente a BRT"?
191400100212         //     ?EasyWeb [al cambiamento] (per fare certi controlli?
191500100212         //     ?solo la 1ª volta)?
191600100212         //  ?2) protegge i campi reperiti dal padre (FLS, NRS, Range?
191700100212         //     ?Segnacolli e CTM)?
191800100212         //  ?3) evidenziano i campi reperiti dal padre (FLS, NRS, Range?
191900100212         //     ?Segnacolli e CTM)?
192000100216         if  V2Ccks <> I69kac   and   $EasyWeb;
192100100312           reset  $EasyWeb;
192200100216         endif;
192300100212         if  V2Ccba <> c_EasyWeb;
192400100312           reset  $EasyWeb;
192500100212           Protect_3EW = *off;
192600100212         endif;
192700120130         Reverse_3EW  = *off;
192800120130         $FlsNrsIn3CL = *off;
192900091125
193000091126         // -?Ricerche?
193100091125         exsr  sr_Search;
193200091124
193300091126         // -?Cliente Unificante?
193400120510         //  ?N.B.: è meglio resettare il campo di controllo (SAVCKS)?
193500120510         //  ?per rieseguire i controlli anche nel caso in cui la 1ª?
193600120510         //  ?modifica abbia avuto esito negativo (errore).?
193700110121         if  V2Ccks <> SavCKS  or
193800110121             V2Ccks <> I69kac;
193900120510           reset  SavCKS;
194000120104           clear  ParFIL;
194100120104           clear  ParNRS;
194200100312           exsr  sr_CtrlCKS;
194300100312           if  ErrGenerico;
194400100312             leavesr;
194500100312           endif;
194600100312         endif;
194700160317
194800120305         // -?Supporto Cliente a BRT?
194900120510         //  ?N.B.: è meglio resettare il campo di controllo (SAVCBA)?
195000120510         //  ?per rieseguire i controlli anche nel caso in cui la 1ª?
195100120510         //  ?modifica abbia avuto esito negativo (errore).?
195200100312         if  V2Ccks <> SavCKS   or   V2Ccba <> SavCBA;
195300120510           reset  SavCBA;
195400100312           exsr  sr_CtrlCBA;
195500100312           if  ErrGenerico;
195600100312             leavesr;
195700100312           endif;
195800100312         endif;
195900120125
196000120125         // -?Memorizzazione codici già controllati con esito positivo?
196100120125         //  ?per evitare di ripetere tali controlli...?
196200120125         SavCBA = V2Ccba;
196300120125         SavCKS = V2Ccks;
196400091103
196500091126         // -?Codice Trattamento Merce?
196600100312         if  V2Cctm <> SavCTM;
196700100312           exsr  sr_CtrlCTM;
196800100312           if  ErrGenerico;
196900100312             leavesr;
197000100312           endif;
197100120125           SavCTM = V2Cctm;
197200100312         endif;
197300110311
197400110311         // -?Abilitazione F1 per?
197500110311         //  ?Numero Serie & Range Segnacolli dal padre?
197600110311         exsr  sr_CtrlRngPadre;
197700120515         //if  ErrGenerico;         ?(meglio proseguire...)?
197800120515         //  leavesr;               ?(meglio proseguire...)?
197900120515         //endif;                   ?(meglio proseguire...)?
198000091103
198100091126         // -?Numero Serie?
198200091126         // ->?Obbligatorio (se CTM da cliente Flag6)?
198300100312         if  V2Cnrs = *zero   and   §1Bfg6 = 'C';
198400091125           VIDmsg = $Msg(17);
198500091103           PosCurNRS   = *on;
198600091103           ErrMessage  = *on;
198700091103           ErrGenerico = *on;
198800091103           leavesr;
198900091103         endif;
199000091103
199100120202         Select;
199200120202
199300120202           // ->?Nessuna serie immessa?
199400120202           When  V2Cnrs = *zero;
199500120202             V2Cncd = 1;
199600120202             V2Cnca = *hival;
199700100329
199800120202           // ->?Controlli per serie completa o parziale?
199900120202           //   ?(controlla prima serie parziale, poi completa)?
200000120202           Other;
200100120202             W1Cnrs = V2Cnrs;
200200120202             W1Cncd = V2Cncd;
200300120202             W1Cnca = V2Cnca;
200400120202             exsr  sr_Ctrl_NRS_SCx;
200500120202             if  ErrGenerico = *on;
200600120202               leavesr;
200700120202             endif;
200800120202
200900120202         EndSl;
201000100111
201100100111         // -?Filiale Segnacollo?
201200100111         select;
201300100111           when  V2Cnrs = *zero;
201400100111             clear  V2Dfls;
201500100111           //when  §3Cfls <> *zero;
201600100120           //  V2Dfls = %editc( §3Cfls : 'X' );
201700100326           when  V2Ccba = c_EasyWEB   and   d3EW.§3EWfls > *zero;
201800100326             V2Dfls = %editc( d3EW.§3EWfls : 'X' );
201900150127             if  d3EW.§3EWLNP > *zero;
202000150127               V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
202100150127             endif;
202200100111           other;
202300100120             V2Dfls = %subst( V1Ccli : 1 : %len(V2Dfls) );
202400150127               V23EWLNP = *blank;
202500100111         endsl;
202600091104
202700120305         // -?Supporto BRT a Cliente?
202800091201         select;
202900100312           // -?Obbligatorio per cliente unificante con serie?
203000091201           when  V2Cbac =  *blank   and
203100091201                 V2Ccks =  V2Ccli   and
203200091201                 V2Cnrs <> *zero;
203300091201             VIDmsg = $Msg(34);
203400091201             PosCurBAC   = *on;
203500091201             ErrMessage  = *on;
203600091201             ErrGenerico = *on;
203700091201             leavesr;
203800100312           // -?Indicabile SOLO sul cliente unificante?
203900091201           when  V2Cbac <> *blank   and
204000091201                 V2Ccks <> V2Ccli;
204100091201             VIDmsg = $Msg(22);
204200091201             PosCurBAC   = *on;
204300091201             ErrMessage  = *on;
204400091201             ErrGenerico = *on;
204500091201             leavesr;
204600091201         endsl;
204700091124
204800091126         // -?"Accorpamento bolle" diverso dal quello del padre?
204900091127         if  V2Ccks <> V2Ccli     and
205000100312             V2Cabd <> p_§3Cabd   and   V2Cabd <> SavABD;
205100091125           SavABD = V2Cabd;
205200091125           VIDmsg = %trimr($Msg(23)) + ' "' + p_§3Cabd +
205300091125                    '" » "Invio" per forzare';
205400091125           PosCurABD   = *on;
205500091124           ErrMessage  = *on;
205600091124           ErrGenerico = *on;
205700091124           leavesr;
205800091124         endif;
205900120605
206000120605         // -?Emissione messaggio di avvertimento della tab. "3EW"?
206100120605         //  ?ancora attiva nonostante manchi l'abilitazione ad EasyWeb?
206200120605         If  $Annull_3EW   and
206300120605               // -?Non un solo codice in TIVSS?
206400120605             ( wCount_3EW <> 1        or
206500120605               // -?Nessun codice selezionato da TIVSS?
206600120605               W4Csun = *zero  or  W4Csun <> TIVSSds.VSSsun);
206700120605           $VideoPrec = $Video;
206800120605           $Video = 'W3';
206900120605           clear  TB28W3;
207000120605           W3txt2 = 'Annullare ' + %editc( Old_§3Ccks : 'X' ) +
207100120605                    ' in tab. "3EW".';
207200120802           W3txt3 = 'Premere Invio per uscire.';
207300120802           exfmt  TB28W3;
207400120605           reset  $Annull_3EW;
207500120605           $Video = $VideoPrec;
207600150319           clear  $VideoPrec;
207700120605         EndIf;
207800120726
207900120726         // ->?AVVISO e consentita l'interrogazione della tab. "3EW"?
208000120726         //   ?se cliente presente più di una sola volta nella "3EW"?
208100120726         //   ?(errore "alternativo" al precedente: meglio non uscire?
208200120726         //   ?dalla subroutine per eseguire comunque la pulizia dei?
208300120726         //   ?campi sottostante)?
208400120726         //if  wCount_3EW > 1;
208500120726         //  // -?Abilitazione F13=Interrog. tab. "3EW"?
208600120726         //  F13Attivo   = *on;
208700120726         //  VIDmsg = $Msg(40);
208800120726         //  //PosCurNRS   = *on;
208900120726         //  PosCurCKS   = *on;
209000120726         //  ErrMessage  = *on;
209100120726         //  ErrGenerico = *on;
209200120726         //  leavesr;
209300120726         //endif;
209400120726         if  wCount_3EW > 1;
209500120726           $VideoPrec  = $Video;
209600120726           $Video      = 'W4';
209700120726           reset  $InzW04;
209800120726           ErrGenerico = *on;
209900120726           leavesr;
210000120726         endif;
210100160317
210200160317         // -?Cappario semplificato ?
210300160317         //  ?Ambito e Tertminal partenza vanno valorizzati entrambi o
210400160317         //  ?lasciati vuoti entrambi
210500160317         if  V2Amb <> *blank and V2TFP = 0  OR
210600160317             V2Amb =  *blank and V2TFP <>0;
210700160317           VIDmsg = %trimr($Msg(48));
210800160317           PosCurAMB   = *on;
210900160317           ErrMessage  = *on;
211000160317           ErrGenerico = *on;
211100160317           leavesr;
211200160317         endif;
211300160317
211400160317         // -?Terminal partenza?
211500160321         // 999 non esiste ma vale tutti
211600160321         if  V2Amb <> *blank and V2TFP > 0
211700160321             and V2TFP <> 999;
211800160317           chain  (V2TFP)  AZORG;
211900160317           if  NOT %found(AZORG01L) OR
212000160317               %found(AZORG01L) and %subst(ORGDE8:1:1)<>'1';
212100160317             VIDmsg = $Msg(49);
212200160317             PosCurAMB   = *on;
212300160317             ErrMessage  = *on;
212400160317             ErrGenerico = *on;
212500160317             leavesr;
212600160317           endif;
212700160317         endif;
212800100312
212900100312         // -?Memorizzazione codici già controllati con esito positivo?
213000100312         //  ?per evitare di ripetere tali controlli...?
213100100312         //  ?(se non sono stati rilevati errori: in caso di errore?
213200100312         //  ?si sarebbe usciti prima dalla subr.!)?
213300120125         //  ?Meglio spostare tali memorizzazioni subito DOPO i loro?
213400120125         //  ?controlli specifici: questa memorizzazione salterebbe?
213500120125         //  ?in caso di errore ad un controllo legato ad un campo?
213600120125         //  ?della loro struttura dati (vedi §1BFG6).?
213700120125         //SavCKS = V2Ccks;             ?(già fatto)?
213800120125         //SavCBA = V2Ccba;             ?(già fatto)?
213900120125         //SavCTM = V2Cctm;             ?(già fatto)?
214000091104
214100091104       ENDSR;
214200101014
214300101014       //--------------------------------------------------------------
214400101014       //?Controllo range segnacolli per filiale e serie.              ?
214500101014       //--------------------------------------------------------------
214600101014       BEGSR  sr_Ctrl_NRS_SCx;
214700110121
214800110207         // -?Reperimento famiglia completa?
214900120517         //  ?(se premuto F11  PRIMA  di Invio, la famiglia?
215000110121         //  ? NON è ancora stata reperita)?
215100120104         exsr  sr_ubLeg3C;
215200101014
215300110207         // -?Controllo range segnacolli?
215400101014         clear  TNTB54ds;
215500101014         if  V2Dfls > *zero;
215600101014           I54fil = V2Dfls;
215700101014         else;
215800101014           I54fil = %subst( V1Ccli : 1 : %len(I54fil) );
215900101014         endif;
216000101014         I54ksc = V1Ccli;
216100101014         I54tel = 'C';
216200101014         I54nrs = %editc( W1Cnrs : 'X' );
216300101014         I54dal = W1Cncd;
216400101014         I54al  = W1Cnca;
216500101014
216600101014         kpjbu  = TNTB54ds;
216700110207         tntb54r ( kpjba : ubLeg3C_SKC );
216800101014         TNTB54ds = kpjbu;
216900101014
217000101014         SELECT;
217100120131
217200120131           // ->?Serie già assegnata come completa (in tab. "3CL")?
217300120131           //   ?La segnalazione qui scarterebbe gli altri controlli di?
217400120131           //   ?TRTB28R2 !?
217500120131           //WHEN  O54err = 'A';
217600120131           //  //$FlsNrsIn3CL = *on;
217700120131           //  ErrGenerico = *on;
217800120131           //  leavesr;
217900160125           // WHEN  O54err = 'A'  and  (W1Cncd > 1  or
218000160125           //                          W1Cnca < *hival)
218100160125           WHEN  O54err = 'A'
218200160125                               and (I54Nrs<>Save3CLNRS or
218300160125                                    I54Fil<>Save3CLLNP)   ;
218400120131             VIDmsg = $Msg(20);
218500160125             VIDmsg = %trimr(VIDmsg) +
218600160125                        '. Premere "Invio" per forzare';
218700160125             Save3CLNRS = I54NrS;
218800160125             Save3CLLNP = I54Fil;
218900160125             //select;
219000160125             //  when  $Video = 'D2';
219100160125             //    PosCurNRS   = *on;
219200160125             //  when  $Video = 'W1';
219300160125             //    PosCurW1nrs = *on;
219400160125             //endsl;
219500160125             //ErrMessage  = *on;
219600160125             //ErrGenerico = *on;
219700160125             //leavesr;
219800101014
219900101014           // ->?errore SQL?
220000101014           WHEN  O54err = 'S';
220100101014             VIDmsg = $Msg(18);
220200101014             select;
220300101014               when  $Video = 'D2';
220400101014                 PosCurNRS   = *on;
220500101014               when  $Video = 'W1';
220600101014                 PosCurW1nrs = *on;
220700101014             endsl;
220800101014             ErrMessage  = *on;
220900101014             ErrGenerico = *on;
221000101014             leavesr;
221100101014
221200101014           // ->?serie già presente come parziale?
221300110121           //   ?(non nel padre)?
221400110121           WHEN  O54err =  '1'      and
221500120131                 // (W1Cnrs <> SavNRS   or
221600120131                 //  W1Cncd <> SavNCD   or
221700120131                 //  W1Cnca <> SavNCA)  and
221800110121                 %lookup( %int(O54ksc) : sch_SKC ) = *zero;
221900101014             if  O54dal >  1   or   O54al <  *hival;
222000101014               VIDmsg = %trimr($Msg(19)) + ' ' + O54ksc;
222100101014             else;
222200101014               VIDmsg = %trimr($Msg(21)) + ' ' + O54ksc;
222300101014             endif;
222400101014             select;
222500101014               when  $Video = 'D2';
222600101014                 PosCurNRS   = *on;
222700101014               when  $Video = 'W1';
222800101014                 PosCurW1nrs = *on;
222900101014             endsl;
223000101014             ErrMessage  = *on;
223100101014             ErrGenerico = *on;
223200101014
223300101014             // ->?errore forzabile SE con stesso padre?
223400110121             //   ?(NON più: errore scartato all'inizio)?
223500110121             //if  %lookup( %int(O54ksc) : sch_SKC ) > *zero;
223600110121             //  SavNRS = W1Cnrs;
223700110121             //  SavNCD = W1Cncd;
223800110121             //  SavNCA = W1Cnca;
223900110121             //  VIDmsg = %trimr(VIDmsg) +
224000110121             //          '. Premere "Invio" x forzare';
224100110121             //endif;
224200101014             leavesr;
224300120113
224400120113           // ->?Serie NON ammessa (99)?
224500120113           WHEN  O54err = '9';
224600120113             VIDmsg = $Msg(43);
224700120113             select;
224800120113               when  $Video = 'D2';
224900120113                 PosCurNRS   = *on;
225000120113               when  $Video = 'W1';
225100120113                 PosCurW1nrs = *on;
225200120113             endsl;
225300120113             ErrMessage  = *on;
225400120113             ErrGenerico = *on;
225500120113             leavesr;
225600101014
225700101014           // ->?verifica se serie già presente come completa;?
225800101014           OTHER;
225900120104             //If  O54prz = *blank;
226000120104             If  ( (V2Dfls  > *zero  and  ParFIL <> %int(V2Dfls))   or
226100120202                   (V2Dfls <= *zero  and  ParFIL <> V2Ccli / 10000) or
226200120202                  ParNRS <> W1Cnrs   or
226300120202                  ParERR <> *blank )   and
226400120202                 (W1Cncd = 1  and  W1Cnca = 9999999);
226500101014               clear  TRTB28ds2;
226600101014               if  V2Dfls > *zero;
226700101014                 ParFIL = %int(V2Dfls);
226800101014               else;
226900101014                 ParFIL = V2Ccli / 10000;
227000101014               endif;
227100101014               ParNRS = W1Cnrs;
227200101014               ParELB = 'C';
227300101014               ParKSC = %editc( V2Ccli : 'X' );
227400111215               trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
227500101014
227600120113               select;
227700120113
227800120113                 // ->?Serie NON ammessa (99)?
227900120113                 when  ParERR = '9';
228000120113                   VIDmsg = $Msg(43);
228100120113                   select;
228200120113                     when  $Video = 'D2';
228300120113                       PosCurNRS   = *on;
228400120113                     when  $Video = 'W1';
228500120113                       PosCurW1nrs = *on;
228600120113                   endsl;
228700120113                   ErrMessage  = *on;
228800120113                   ErrGenerico = *on;
228900120113                   leavesr;
229000120113
229100120113                 // ->?errore bloccante SE con padre diverso?
229200120113                 // ->?errore forzabile SE con stesso padre?
229300120113                 //   ?(NON più: ora solo se con padre diverso)?
229400120113                 when  ParERR =  'E'      and
229500120131                       // (W1Cnrs <> SavNRS   or
229600120131                       //  W1Cncd <> SavNCD   or
229700120131                       //  W1Cnca <> SavNCA)  and
229800120113                       %lookup( %int(OUTksc) : sch_SKC ) = *zero;
229900120113                   VIDmsg = %trimr($Msg(21)) + ' ' + OUTksc;
230000120113                   select;
230100120113                     when  $Video = 'D2';
230200120113                       PosCurNRS   = *on;
230300120113                     when  $Video = 'W1';
230400120113                       PosCurW1nrs = *on;
230500120113                   endsl;
230600120113                   ErrMessage  = *on;
230700120113                   ErrGenerico = *on;
230800120113                   // ->?errore forzabile SE con stesso padre?
230900120113                   //if  %lookup( %int(OUTksc) : sch_SKC ) > *zero;
231000120113                   //  SavNRS = W1Cnrs;
231100120113                   //  SavNCD = W1Cncd;
231200120113                   //  SavNCA = W1Cnca;
231300120113                   //  VIDmsg = %trimr(VIDmsg) +
231400120113                   //          '. Premere "Invio" per forzare';
231500120113                   //endif;
231600120113                   leavesr;
231700120131
231800120131                 // ->?Serie già assegnata come completa (in tab. "3CL")?
231900120131                 //when  ParERR = 'A';
232000120131                 //  //$FlsNrsIn3CL = *on;
232100120131                 //  ErrGenerico = *on;
232200120131                 //  leavesr;
232300160125                 when  O54err = 'A'
232400160125                                     and (I54Nrs<>Save3CLNRS or
232500160125                                          I54Fil<>Save3CLLNP)   ;
232600160125                 //when  O54err = 'A'  and  (W1Cncd > 1  or
232700160125                 //                          W1Cnca < *hival)
232800160125                   VIDmsg = $Msg(20);
232900160125                   VIDmsg = %trimr(VIDmsg) +
233000160125                             '. Premere "Invio" per forzare';
233100160125                   Save3CLNRS = I54NrS;
233200160125                   Save3CLLNP = I54Fil;
233300160125                   //select;
233400160125                   //  when  $Video = 'D2';
233500160125                   //    PosCurNRS   = *on;
233600160125                   //  when  $Video = 'W1';
233700160125                   //    PosCurW1nrs = *on;
233800160125                   //endsl;
233900160125                   //ErrMessage  = *on;
234000160125                   //ErrGenerico = *on;
234100160125                   //leavesr;
234200120113
234300120113               endsl;
234400120131
234500120131               // ->?Serie già assegnata come completa (in tab. "3CL")?
234600120131               //   ?- rilevata dal *pgm TNTB54R -?
234700120131               //   ?(da segnalare in assenza di altri errori)?
234800120131               //if  O54err = 'A';
234900120131               //  //$FlsNrsIn3CL = *on;
235000120131               //  ErrGenerico = *on;
235100120131               //  leavesr;
235200120131               //endif;
235300101014
235400120104             EndIf;
235500101014
235600101014         ENDSL;
235700121112
235800121220         // -?SE modificato il supporto Cli-BRT (da o in "ESWEB")?
235900121112         //  ?occorre cambiare range?
236000121112         //  ?(lasciando il "vecchio" "bloccato" in tab. "3EW")?
236100121220         If  (Old_§3Ccba =  c_EasyWEB   and
236200121220              V2Ccba     <> c_EasyWEB    or
236300121220              Old_§3Ccba <> c_EasyWEB   and
236400121220              V2Ccba     =  c_EasyWEB)  AND
236500121220             (Old_§3Ccba =  c_EasyWEB         and
236600121220             ($Video     =  'D2'              and
236700121220              V2Cnrs     =  d3EW_Old.§3EWnrs  and
236800121220              V2Cncd     =  d3EW_Old.§3EWnsi  and
236900121220              V2Cnca     =  d3EW_Old.§3EWnsf  or
237000121220              $Video     =  'W1'              and
237100121220              W1Cnrs     =  d3EW_Old.§3EWnrs  and
237200121220              W1Cncd     =  d3EW_Old.§3EWnsi  and
237300121220              W1Cnca     =  d3EW_Old.§3EWnsf)  OR
237400121220              Old_§3Ccba <> c_EasyWEB         and
237500121220             ($Video     =  'D2'              and
237600121220              V2Cnrs     =  Save_3CP_nrs      and
237700121220              V2Cncd     =  Save_3CP_nsi      and
237800121220              V2Cnca     =  Save_3CP_nsf      or
237900121220              $Video     =  'W1'              and
238000121220              W1Cnrs     =  Save_3CP_nrs      and
238100121220              W1Cncd     =  Save_3CP_nsi      and
238200121220              W1Cnca     =  Save_3CP_nsf));
238300121128           //VIDmsg = $Msg(45);
238400121128           VIDmsg = $Msg(44);
238500121112           select;
238600121112             when  $Video = 'D2';
238700121112               PosCurNRS   = *on;
238800121112             when  $Video = 'W1';
238900121112               PosCurW1nrs = *on;
239000121112           endsl;
239100121112           ErrMessage  = *on;
239200121112           ErrGenerico = *on;
239300121112           leavesr;
239400121112         EndIf;
239500121127
239600121128         // -?ULTIMO CONTROLLO (BYPASSABILE) in fase di ricerca range?
239700121128         //  ?parziale: Range "anomalo"?
239800140718         //IF  $Video =  'W1'   and
239900140718         //    ( W1Cnrs <> SavW1Cnrs  or  W1Cncd <> SavW1Cncd
240000140718         //                           or  W1Cnca <> SavW1Cnca )  and  (
240100140718         //    %subst( %editc( W1Cncd : 'X' ) : %len(W1Cncd) : 1 ) <> '1'
240200140718         //                         or  (
240300140718         //    %subst( %editc( W1Cnca : 'X' ) : %len(W1Cnca) : 1 ) <> '0'
240400140718         //                        and  W1Cnca <> *hival ) );
240500140718         //
240600140718         //  SavW1Cnrs = W1Cnrs;
240700140718         //  SavW1Cncd = W1Cncd;
240800140718         //  SavW1Cnca = W1Cnca;
240900140718         //
241000140718         //  Select;
241100140718         //
241200140718         //    When  %subst(%editc(W1Cncd:'X'):%len(W1Cncd):1) <> '1' and
241300140718         //          %subst(%editc(W1Cnca:'X'):%len(W1Cnca):1) <> '0' and
241400140718         //          W1Cnca <> *hival;
241500140718         //      VIDmsg = %trimr($Msg(46)) +
241600140718         //      //       ' non comincia con "...1" +
241700140718         //      //         né termina con "...0". +
241800140718         //               ' non inizia con ' +
241900140718         //               %subst(%editc(W1Cncd:'X'):1:%len(W1Cncd)-1)+'1'
242000140718         //             + ' né termina con';
242100140718         //      wW1Cncx = W1Cnca + 1;
242200140718         //      if  %subst(%editc(wW1Cncx:'X'):%len(wW1Cncx)-1:2) = '00';
242300140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
242400140718         //               %subst(%editc(wW1Cncx:'X'):1:%len(wW1Cncx));
242500140718         //      else;
242600140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
242700140718         //               %subst(%editc(W1Cnca:'X'):1:%len(W1Cnca)-1)+'0';
242800140718         //      endif;
242900140718         //      VIDmsg = %trimr(VIDmsg) + '. Invio.';
243000140718         //      PosCurW1ncd = *on;
243100140718         //
243200140718         //    When  %subst(%editc(W1Cncd:'X'):%len(W1Cncd):1) <> '1';
243300140718         //      VIDmsg = %trimr($Msg(46)) +
243400140718         //      //       ' dovrebbe cominciare con "...1". +
243500140718         //               ' dovrebbe cominciare con ' +
243600140718         //               %subst(%editc(W1Cncd:'X'):1:%len(W1Cncd)-1)+'1'
243700140718         //             + '. Premere Invio.';
243800140718         //      PosCurW1ncd = *on;
243900140718         //
244000140718         //    When  %subst(%editc(W1Cnca:'X'):%len(W1Cnca):1) <> '0' and
244100140718         //          W1Cnca <> *hival;
244200140718         //      VIDmsg = %trimr($Msg(46)) +
244300140718         //      //       ' dovrebbe terminare con "...0". +
244400140718         //               ' dovrebbe terminare con';
244500140718         //      wW1Cncx = W1Cnca + 1;
244600140718         //      if  %subst(%editc(wW1Cncx:'X'):%len(wW1Cncx)-1:2) = '00';
244700140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
244800140718         //               %subst(%editc(wW1Cncx:'X'):1:%len(wW1Cncx));
244900140718         //      else;
245000140718         //        VIDmsg = %trimr(VIDmsg) + ' ' +
245100140718         //               %subst(%editc(W1Cnca:'X'):1:%len(W1Cnca)-1)+'0';
245200140718         //      endif;
245300140718         //      VIDmsg = %trimr(VIDmsg) + '.  Premere Invio.';
245400140718         //      PosCurW1nca = *on;
245500140718         //
245600140718         //  EndSl;
245700140718         //
245800140718         //  ErrMessage  = *on;
245900140718         //  ErrGenerico = *on;
246000140718         //  leavesr;
246100140718         //
246200140718         //ENDIF;
246300101014
246400101014       ENDSR;
246500091105
246600091105       //--------------------------------------------------------------
246700091105       //?Gestione videata D03.                                        ?
246800091105       //--------------------------------------------------------------
246900091105       BEGSR  sr_GesD03;
247000091105
247100091126         // -?Inizializzazione videata?
247200091105         if  $InzD02 = *on;
247300091105           exsr  sr_InzD02;
247400091105           $InzD02 = *off;
247500091105         endif;
247600091120
247700091126         // -?(Dis)Abilitazione tasti funzionali?
247800091120         exsr  sr_AbilFxxD02;
247900091105
248000091126         // -?Emissione Testata e Piede?
248100091127         write  TB28T1;
248200091127         write  TB28P2;
248300091105
248400091126         // -?Emissione videata?
248500091105         if  Not $Protect;
248600091127           exfmt  TB28D3;
248700091105         else;
248800091127           write  TB28D3;
248900091105           exfmt  Protect;
249000091105         endif;
249100091120
249200091105         reset  ErrMessage;
249300091105         reset  ErrGenerico;
249400091105         clear  VIDmsg;
249500091120
249600091105         SELECT;
249700091105
249800091126           // -?F3=Fine?
249900091105           WHEN  dsp_aid = c_F03;
250000091105             unlock  TABEL00F;
250100091105             unlock  TNTBE01L;
250200091105             $Fine = *on;
250300091105
250400091126           // -?F10=Ricerca Serie (globale)?
250500091109           WHEN  dsp_aid = c_F10;
250600091109             exsr  sr_F10D02;
250700091105
250800091126           // -?F11=Ricerca Serie Parziale?
250900091109           WHEN  dsp_aid = c_F11;
251000091109             $VideoPrec = $Video;
251100091109             $Video     = 'W1';
251200091109             reset  $InzW01;
251300091105
251400091126           // -?F12=Ritorno?
251500091105           WHEN  dsp_aid = c_F12;
251600150319             $Video = 'D2';
251700150319             clear  $VideoPrec;
251800091105
251900091126           // -?F19=Gestione multimembro clienti VAB?
252000091126           // -?F20=Gestione scambio dati principali     con clienti?
252100091126           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
252200091126           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
252300091105           WHEN  dsp_aid = c_F19   or
252400091105                 dsp_aid = c_F20   or
252500091105                 dsp_aid = c_F21   or
252600091105                 dsp_aid = c_F22;
252700091105             exsr  sr_FxxD02;
252800091105
252900091126           // -?RollDown=Videata precedente?
253000091105           WHEN  dsp_aid = c_RollDown;
253100150319             $Video = 'D2';
253200150319             clear  $VideoPrec;
253300091105
253400091126           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
253500091105           OTHER;
253600091126             // -?Controlli eseguiti NON se F16=Annullamento?
253700091105             if  dsp_aid <> c_F16;
253800091105               exsr sr_CtrD03;
253900091105               if  Not ErrGenerico;
254000091105                 exsr sr_CtrD02;
254100091106                 if  ErrGenerico = *on;
254200091106                   $Video = 'D2';
254300091106                 endif;
254400091105               endif;
254500091105               if  ErrGenerico = *on;
254600091105                 leavesr;
254700091105               endif;
254800091105             endif;
254900091126             // -?Aggiornamento?
255000091105             if  dsp_aid = c_F05   or
255100091105                 dsp_aid = c_F06   or
255200091105                 dsp_aid = c_F16;
255300091105               exsr  sr_UpdateAll;
255400091105               if  ErrGenerico = *on;
255500091105                 leavesr;
255600091105               endif;
255700091126               // -?Uscita dal programma SE richiamato?
255800091105               if  $Called = *on   and   W0Ccli > *zero;
255900091105                 $Fine = *on;
256000091126               // -?Videata iniziale altrimenti?
256100091105               else;
256200091105                 $Video  = 'D1';
256300091105                 $InzD01 = *on;
256400091105               endif;
256500091105             endif;
256600091105
256700091105         ENDSL;
256800091105
256900091126         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
257000091105         if  $Video = 'D1';
257100091105           clear  V1Topz;
257200091105           clear  BloAnn;
257300091120           VisBloAnn = *off;
257400091105         endif;
257500091105
257600091105       ENDSR;
257700091105
257800091104       //--------------------------------------------------------------
257900091104       //?Controllo dati immessi nella videata D03.                    ?
258000091104       //--------------------------------------------------------------
258100091104       BEGSR  sr_CtrD03;
258200091104
258300091105         %subst(IndDspF : 71) = *off;
258400091104
258500091126         // -?Dati per accorpamento bolle?
258600091126         if  V2Cabd <> *blank;
258700091126           // ->?Lunghezza riferimento originale mittente obbligatoria?
258800091104           if  V2Clrm <= *zero   and   V2Calm <> *blank;
258900091125             VIDmsg = $Msg(24);
259000091104             PosCurLRM   = *on;
259100091104             ErrMessage  = *on;
259200091104             ErrGenerico = *on;
259300091104             leavesr;
259400091104           endif;
259500091104         else;
259600091126           // ->?Controllo campi subordinati all'accorpamento bolle?
259700091104           select;
259800091104             when  V2Clrm <> *zero;
259900091125               VIDmsg = $Msg(25);
260000091104               PosCurLRM   = *on;
260100091104               ErrMessage  = *on;
260200091104               ErrGenerico = *on;
260300091104               leavesr;
260400091104             when  V2Calm <> *blank;
260500091125               VIDmsg = $Msg(25);
260600091104               PosCurALM   = *on;
260700091104               ErrMessage  = *on;
260800091104               ErrGenerico = *on;
260900091104               leavesr;
261000091104             when  V2Cnot <> *blank;
261100091125               VIDmsg = $Msg(25);
261200091104               PosCurNOT   = *on;
261300091104               ErrMessage  = *on;
261400091104               ErrGenerico = *on;
261500091104               leavesr;
261600091104             when  V2Cabc <> *blank;
261700091125               VIDmsg = $Msg(25);
261800091104               PosCurABC   = *on;
261900091104               ErrMessage  = *on;
262000091104               ErrGenerico = *on;
262100091104               leavesr;
262200091104             when  V2Carc <> *blank;
262300091125               VIDmsg = $Msg(25);
262400091104               PosCurARC   = *on;
262500091104               ErrMessage  = *on;
262600091104               ErrGenerico = *on;
262700091104               leavesr;
262800091104             when  V2Cavx <> *blank;
262900091125               VIDmsg = $Msg(25);
263000091104               PosCurAVX   = *on;
263100091104               ErrMessage  = *on;
263200091104               ErrGenerico = *on;
263300091104               leavesr;
263400091106           endsl;
263500091106         endif;
263600091102
263700091102       ENDSR;
263800091125
263900091125       //--------------------------------------------------------------
264000091125       //?Ricerche.                                                    ?
264100091125       //--------------------------------------------------------------
264200091125       BEGSR  sr_Search;
264300091125
264400091125         select;
264500091125
264600120305           // -?Supporto Cliente a BRT?
264700091125           when  %scan( '?' : V2Ccba ) > *zero;
264800091125             clear  V2Ccba;
264900091125             clear  V2Dcba;
265000091125             reset  TIBS02ds;
265100120605             //T02mod = 'R';                //?(già così)?
265200120605             //T02cod = '3CS';              //?(già così)?
265300091125             T02sif = knsif;
265400091125             TNTBE_RicercaControllo (kpjba : TIBS02ds);
265500091125             if  T02err = *blank;
265600091125               V2Ccba = T02ke1;
265700091125               V2Dcba = %subst( T02uni : 1 : %len(V2Dcba) );
265800091126               // -?Controlli specifici sull'abilitazione ad EasyWEB?
265900091126               if  V2Ccba = c_EasyWEB;
266000091126                 exsr  sr_CtrlEasyWEB;
266100120725                 Reverse_3EW = ( Not $Inzd02   and
266200120725                               ( SaveFLS <> V2Dfls  or
266300120725                                 SaveNRS <> V2Cnrs  or
266400120725                                 SaveNCD <> V2Cncd  or
266500120725                                 SaveNCA <> V2Cnca  or
266600120725                                 SaveCTM <> V2Cctm ) );
266700091126                 if  ErrGenerico;
266800091126                   leavesr;
266900091126                 endif;
267000110311               else;
267100110311                 exsr  sr_CtrlRngPadre;
267200110422                 // -?Qui NON serve: viene COMUNQUE acceso dopo!?
267300110422                 //if  ErrGenerico;
267400110422                 //  clear  ErrGenerico;
267500110422                 //endif;
267600091126               endif;
267700091125             else;
267800091125               errMessage = *on;
267900091125               VIDmsg = T02msg;
268000091125             endif;
268100091125             errGenerico = *on;
268200091125             PosCurCBA   = *on;
268300091125             leavesr;
268400091125
268500091126           // -?Codice Trattamento Merce?
268600091125           when  %scan( '?' : V2Cctm ) > *zero;
268700091125             clear  V2Cctm;
268800091125             clear  V2Dctm;
268900091125             X§Tkut = c_Kut;
269000091125             X§Tcod = '1B';
269100091125             clear  X§Tkey;
269200091125             clear  X§Tdes;
269300091125             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
269400091125             V2Cctm = %subst(X§Tkey : 1 : %len(V2Cctm));
269500091125             ErrGenerico = *on;
269600091125             leavesr;
269700091125
269800091125         endsl;
269900091125
270000091125       ENDSR;
270100091124
270200091124       //--------------------------------------------------------------
270300091124       //?Controllo/Decodifica Cliente Unificante.                     ?
270400091124       //--------------------------------------------------------------
270500091126       BEGSR  sr_CtrlCKS;
270600091124
270700091124         //clear  V2Dcks;         (NO perchè é i/o !!!)
270800110311         clear  wNSI_Padre;
270900110311         clear  d3CP_Padre;
271000110311         clear  d3EW_Padre;
271100110421         reset  SavCliTab3CPEW;
271200091124
271300091124         SELECT;
271400091124
271500091126           // ->?Ricerca alfabetica?
271600091124           WHEN  V2Ccks = *zero   and   V2Dcks <> *blank;
271700091124             xParDut = RSut;
271800091124             xParKut = 1;
271900091124             xParRag = V2Dcks;
272000091124             xParKcc = DUTkci;
272100091124             xParSta = 9;
272200091124             xParFlr = %editc( V1Cfil : 'X');
272300091124             clear  xParDit;
272400091124             xParNum = 1;
272500091124             clear  xParKcm;
272600091124             clear  xParKsm;
272700091124             clear  xParKdm;
272800091124             clear  xParEsci;
272900091124             clear  xParErr;
273000091124             clear  xParIva;
273100091124             clear  xParCdf;
273200091124             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
273300091124                        xParSta : xParFlr : xParDit : xParNum :
273400091124                        xParKcm : xParKsm : xParKdm : xParEsci :
273500091124                        xParErr : xParIva : xParCdf );
273600091124             if  xParSta <> -1   and   xParErr = *blank;
273700091124               V2Ccks = %int( %subst( xParKsm : 1 : %len(V2Ccks) ) );
273800091124               V2Dcks = xParRag;
273900091124             endif;
274000091124             ErrGenerico = *on;
274100091124             leavesr;
274200091124
274300091126           // ->?Non inserito?
274400091124           WHEN  V2Ccks = *zero;
274500091124             VIDmsg = $Msg(06);
274600091124             PosCurCKS   = *on;
274700091124             ErrMessage  = *on;
274800091124             ErrGenerico = *on;
274900091124             leavesr;
275000091124
275100091126           // ->?Controllo / Decodifica?
275200091124           OTHER;
275300091126             if  V2Ccks <> I69kac;
275400091126               clear  TIBS69ds;
275500091126               I69sif = knsif;
275600091126               I69kcc = DUTkci;
275700091126               I69kac = V2Ccks;
275800091126               tibs69r( tibs69ds :
275900091126                        ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
276000091126             endif;
276100091124             if  O69err = *on;
276200091124               VIDmsg = $Msg(07);
276300091124               PosCurCKS   = *on;
276400091124               ErrMessage  = *on;
276500091124               ErrGenerico = *on;
276600091124               leavesr;
276700091124             endif;
276800091124             V2Dcks = %subst( ACOrag : 1 : %len(V2Dcks) );
276900091124
277000091126             // -?SE diverso da V2Ccli: controllo se possibile?
277100091124             clear  ds3C_padre;
277200091124             If  V2Ccks <> V2Ccli;
277300091124
277400100203               // -?Cliente unificante NON in tab. "3C"?
277500091124               if  getTabella ( c_Tabel : '3C' : %editc(V2Ccks:'X') :
277600091124                                *omit : *omit : *omit :
277700091124                                *omit : *omit :
277800091124                                *omit : *omit : *omit : *omit :
277900091124                                *omit : *omit : *omit : *omit :
278000091124                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
278100091124                              < *zero       OR
278200091124                   ds_TNTBE.TBEatb <> *blank;
278300091124                 VIDmsg = $Msg(08);
278400091124                 PosCurCKS   = *on;
278500091124                 ErrMessage  = *on;
278600091124                 ErrGenerico = *on;
278700091124                 leavesr;
278800091124               else;
278900120214                 ds3C_Padre = ds_TNTBE.TBEuni;
279000091124               endif;
279100100203
279200100203               // -?Cliente unificante già figlio?
279300100203               if  V2Ccks <> p_§3Ccks;
279400100203                 VIDmsg = %trimr($Msg(07)) + ': anch''esso è figlio';
279500100203                 PosCurCKS   = *on;
279600100203                 ErrMessage  = *on;
279700100203                 ErrGenerico = *on;
279800100203                 leavesr;
279900100203               endif;
280000091124
280100100917               // -?Cliente unificante a video diverso da quello già?
280200100917               //  ?memorizzato: verifica fattibilità (SE il vecchio?
280300120215               //  ?era padre di se stesso: NON deve avere figli)?
280400110302               if  %found(TABEL00F)   and
280500100917                   V2Ccks <> §3Ccks   and   V2Ccli = §3Ccks;
280600120605
280700100917                 // -?Padre con figli: NON può diventare figlio?
280800120517                 if  sch_SKC_Orig(2) > *zero;
280900100917                   VIDmsg = %trimr($Msg(09)) + ' ' +
281000120517                      %editc( sch_SKC_Orig(2) : 'X' );
281100120517                   if  sch_SKC_Orig(3) > 1;
281200100917                     VIDmsg = %trimr(VIDmsg) + ' ed altri';
281300100917                   endif;
281400100917                   PosCurCKS   = *on;
281500100917                   ErrMessage  = *on;
281600100917                   ErrGenerico = *on;
281700100917                   leavesr;
281800100917                 endif;
281900120214                 // -?Padre già abilitato ad EasySped-WEB: non può?
282000120214                 //  ?diventare figlio?
282100120215                 if  Not  $3EWupd         and
282200120215                     §3Ccba =  c_EasyWEB  and
282300120214                     §3Ccks <> V2Ccks     and
282400120605                     %editc( §3Ccks : 'X' ) =
282500120605                       %subst( TIVSSds_Orig.VSSksu : 2 );
282600120215                   VIDmsg = %trimr( $Msg(32) ) + ' ' +
282700120215                            %editc( §3Ccks : 'X' );
282800120214                   PosCurCKS   = *on;
282900120214                   ErrMessage  = *on;
283000120214                   ErrGenerico = *on;
283100120214                   leavesr;
283200120214                 endif;
283300091214               endif;
283400091124
283500091124             Else;
283600091124
283700091124               ds3C_Padre = ds3C;
283800091124
283900091124             EndIf;
284000091124
284100091124         ENDSL;
284200091124
284300100312         // -?Reperimento chiave completa per tab. "3EW" alla?
284400100312         //  ?variazione del cliente unificante?
284500100312         //  ?se padre non trovato nel file TIVSS non può essere in?
284600100312         //  ?  tab. "3EW"?
284700120725         //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
284800120725         if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
284900101028           clear  rqsSUN;
285000101026           exsr  sr_TIS7701_x_TIVSS;
285100100312         endif;
285200100322
285300100322         // -?Reperimento famiglia completa?
285400120104         exsr  sr_ubLeg3C;
285500091124
285600091124       ENDSR;
285700091106
285800091106       //--------------------------------------------------------------
285900120305       //?Controllo/Decodifica Supporto Cliente a BRT.                 ?
286000091106       //--------------------------------------------------------------
286100091106       BEGSR  sr_CtrlCBA;
286200091106
286300091106         clear  V2Dcba;
286400091106
286500091126         // ->?Vuoto?
286600091125         if  V2Ccba = *blank;
286700091125
286800091125           if  V2Ccks = V2Ccli;
286900091125             VIDmsg = $Msg(11);
287000091125             PosCurCBA   = *on;
287100091125             ErrMessage  = *on;
287200091125             ErrGenerico = *on;
287300091125             leavesr;
287400091125           endif;
287500091106
287600100211         // ->?Controllo / Decodifica?
287700091125         else;
287800091125           ds_TNTBE.TBEke1 = V2Ccba;
287900091125           if  getTabella ( c_Tntbe : '3CS' : ds_TNTBE.TBEke1 :
288000091125                            *omit   : *omit : *omit :
288100091125                            *omit : *omit :
288200091125                            *omit : *omit : *omit : *omit :
288300091125                            *omit : *omit : *omit : *omit :
288400091125                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
288500091125                          = *zero      AND
288600091125               ds_TNTBE.TBEatb = *blank;
288700091125             V2Dcba = %subst( ds_TNTBE.TBEuni : 1 : %len( V2Dcba ) );
288800091125           else;
288900091127             V2Dcba = *all'? ';
289000091125             VIDmsg = $Msg(12);
289100091125             PosCurCBA   = *on;
289200091125             ErrMessage  = *on;
289300091125             ErrGenerico = *on;
289400091125             leavesr;
289500091125           endif;
289600091106
289700091125         endif;
289800091124
289900091126         // -?Controlli specifici sull'abilitazione ad EasyWEB?
290000091125         if  V2Ccba = c_EasyWEB;
290100091126           exsr  sr_CtrlEasyWEB;
290200120725           Reverse_3EW = ( Not $Inzd02   and
290300120725                         ( SaveFLS <> V2Dfls  or
290400120725                           SaveNRS <> V2Cnrs  or
290500120725                           SaveNCD <> V2Cncd  or
290600120725                           SaveNCA <> V2Cnca  or
290700120725                           SaveCTM <> V2Cctm ) );
290800091124           if  ErrGenerico;
290900091124             leavesr;
291000091124           endif;
291100100211         else;
291200100211           exsr  sr_Ctrl3EW;
291300100211           if  ErrGenerico;
291400100211             leavesr;
291500100211           endif;
291600091124         endif;
291700091106
291800091106       ENDSR;
291900091124
292000091124       //--------------------------------------------------------------
292100091124       //?Controlli specifici sull'abilitazione ad EasyWEB             ?
292200120215       //?(se messo/lasciato supporto EasySped-WEB)                    ?
292300150127       //--------------------------------------------------------------
292400091126       BEGSR  sr_CtrlEasyWEB;
292500150127
292600150127         V23EWLnP = *blank;
292700091127
292800120305         // -?"Supporto Cliente a BRT" diverso da quello del "PADRE"?
292900091127         if  V2Ccks <> V2Ccli   and   p_§3Ccba <> c_EasyWEB;
293000091127           VIDmsg = %trimr($Msg(13)) + c_EasyWEB + '": "' +
293100091127                    p_§3Ccba + '"';
293200091127           PosCurCKS   = *on;
293300091127           ErrMessage  = *on;
293400091127           ErrGenerico = *on;
293500091127           leavesr;
293600091127         endif;
293700091124
293800091126         // -?Controllo abilitazione a STRATEGI del cliente "PADRE"?
293900120725         //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
294000120725         if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
294100101028           clear  rqsSUN;
294200101026           exsr  sr_TIS7701_x_TIVSS;
294300100312         endif;
294400120725         //select;
294500120725         //  when  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
294600120725         //                            or   TIVSSds.VSSksu = *zero;
294700120725         //    VIDmsg = $Msg(14);
294800120725         //    if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
294900120725         //        V2Cncd = 1          and  V2Cnca = *all'9';
295000120725         //      //F13Attivo = *on;       ?(già così)?
295100120725         //      //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
295200120725         //      VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
295300120725         //    endif;
295400120725         //    PosCurCKS   = *on;
295500120725         //    ErrMessage  = *on;
295600120725         //    ErrGenerico = *on;
295700120725         //    leavesr;
295800120725         //  // -?Abilitazione scaduta?
295900120725         //  //  ?(IMPOSSIBILE: TIS7701R avrebbe restituito errore!)?
296000120725         //  //when  TIVSSds.VSSdsc < wDate;
296100120725         //  //  VIDmsg = $Msg(37);
296200120725         //  //  PosCurCBA   = *on;
296300120725         //  //  ErrMessage  = *on;
296400120725         //  //  ErrGenerico = *on;
296500120725         //  //  leavesr;
296600120725         //endsl;
296700091125
296800100212
296900100212         // -Azioni specifiche per cliente abilitato ad EasyWeb?
297000100212
297100100212         IF  $EasyWeb = *off;
297200100212
297300100212           $EasyWeb = *on;
297400100212
297500100212           // -?Protezione campi già impostati in tab. "3EW"?
297600100212           Protect_3EW = *on;
297700100212           // -?Reverse image x campi già impostati in tab. "3EW"?
297800120725           //  ?(SOTTO, solo se cambiati rispetto a quelli già?
297900120725           //   ?visualizzati)?
298000120725           //if  Not $Inzd02;
298100120725           //  Reverse_3EW = *on;
298200120725           //endif;
298300100212
298400100212           // -?Pulizia campi già impostati in tab. "3EW"?
298500120725           SaveFLS = V2Dfls;
298600150127           SaveLNP = V23EWLNP;
298700120725           SaveNRS = V2Cnrs;
298800120725           SaveNCD = V2Cncd;
298900120725           SaveNCA = V2Cnca;
299000120725           SaveCTM = V2Cctm;
299100120725           clear  V2Dfls;
299200150127           clear  V23EWLNP;
299300120725           clear  V2Cnrs;
299400120725           clear  V2Cncd;
299500120725           clear  V2Cnca;
299600120725           clear  V2Cctm;
299700120725           //clear  V2Dctm;
299800100212
299900100212           // -?Verifica se padre presente una sola volta in tab. "3EW"?
300000120725           ////if  V2Ccks <> SavCKS;
300100121015           //if  wCount_3EW     <= *zero                         or
300200121015           //    TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' )  or
300300121015           //    TIVSSds.VSSsun <= *zero;
300400120726             exsr  sr_Count3EW;
300500121015           //endif;
300600100212
300700120725           Select;
300800120725
300900120725             // -?Padre NON presente in tab. "3EW"?
301000120725             When  TBLflg <> *blank   and   wCount_3EW = *zero;
301100120725               reset  $EasyWeb;
301200120725               clear  V2Dctm;
301300120725               VIDmsg = $Msg(38);
301400120725               PosCurCKS   = *on;
301500120725               ErrMessage  = *on;
301600120725               ErrGenerico = *on;
301700120725               leavesr;
301800120816
301900120816             // -?Padre presente ma NON attivo in tab. "3EW"?
302000120816             When  wCount_3EW = *zero;
302100120816               // -?Abilitazione F13=Interrog. tab. "3EW"?
302200120816               if  Not  F13Attivo;
302300120816                 F13Attivo   = *on;
302400120816               endif;
302500120725
302600120725             // -?Reperimento dati del codice padre da tab. "3EW"?
302700120725             //  ?(se presente una sola volta)?
302800120725             When  wCount_3EW = 1;
302900120725               // -?Impostazione campi protetti con i valori del padre?
303000120725               if  d3EW.§3EWfls <> *zero;
303100120725                 V2Dfls = %editc( d3EW.§3EWfls : 'X' );
303200120725               endif;
303300150127               if  d3EW.§3EWLNP <> *zero;
303400150127                 V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
303500150127               endif;
303600120725               V2Cnrs = d3EW.§3EWnrs;
303700120725               V2Cncd = d3EW.§3EWnsi;
303800120725               V2Cnca = d3EW.§3EWnsf;
303900120725               V2Cctm = d3EW.§3EWctm;
304000140123               //if  V2Cctm <> SaveCTM;
304100140123               //  clear  V2Dctm;
304200140123               //endif;
304300140123               // -?"Forzature" richiestemi da Mirko C.?
304400140123               If  V2Ccba = c_EasyWEB;
304500140123                 // -?Supporto BRT a Cliente = "EMAIL"?
304600140123                 if  V2Ccli = V2Ccks;
304700140123                   V2Cbac = 'EMAIL';
304800140123                 endif;
304900140123                 // -?Abilitazione Serie in Bollettazione = "S"?
305000140123                 V2Cfg1 = 'S';
305100140123               EndIf;
305200120725               // -?AVVERTIMENTO forzatura dati dal padre?
305300120725               //   ·?filiale segnacolli?
305400120725               //   ·?numero serie segnacolli?
305500120725               //   ·?range segnacolli?
305600120725               //   ·?codice trattamento merce?
305700120927               if  Not $InzD02        and
305800120927                   V2Ccks  <> V2Ccli  and
305900120927                  (SaveFLS <> V2Dfls  or
306000150127                   SaveLNP <> V23EWLNP or
306100150127                   SaveNRS <> V2Cnrs  or
306200120927                   SaveNCD <> V2Cncd  or
306300120927                   SaveNCA <> V2Cnca  or
306400120927                   SaveCTM <> V2Cctm);
306500120725                 VIDmsg = $Msg(39);
306600120725                 ErrMessage  = *on;
306700120725                 ErrGenerico = *on;
306800120725                 leavesr;
306900120725               endif;
307000120725
307100120725             // -?AVVISO e consentita l'interrogazione della tab. "3EW"?
307200120725             //  ?(se presente più di una sola volta)?
307300120725             When  wCount_3EW > 1;
307400120725               // -?Impostazione campi protetti con i valori del padre?
307500120725               if  d3EW.§3EWfls <> *zero;
307600120725                 V2Dfls = %editc( d3EW.§3EWfls : 'X' );
307700120725               endif;
307800150127               if  d3EW.§3EWLNP <> *zero;
307900150127                 V23EWLNP = %editc( d3EW.§3EWLNP : 'X' );
308000150127               endif;
308100120725               V2Cnrs = d3EW.§3EWnrs;
308200120725               V2Cncd = d3EW.§3EWnsi;
308300120725               V2Cnca = d3EW.§3EWnsf;
308400120725               V2Cctm = d3EW.§3EWctm;
308500120725               if  V2Cctm <> SaveCTM;
308600120725                 clear  V2Dctm;
308700120725               endif;
308800120725               // -?Abilitazione F13=Interrog. tab. "3EW"?
308900120725               if  Not  F13Attivo;
309000120725                 F13Attivo   = *on;
309100120725                 VIDmsg = $Msg(40);
309200120725                 //PosCurNRS   = *on;
309300120725                 PosCurCKS   = *on;
309400120725                 ErrMessage  = *on;
309500120725                 ErrGenerico = *on;
309600120725                 leavesr;
309700120725               endif;
309800120725
309900120725           EndSl;
310000120725
310100120725           Reverse_3EW = ( Not $Inzd02   and
310200120725                         ( SaveFLS <> V2Dfls  or
310300150127                           SaveLNP <> V23EWLNP or
310400150127                           SaveNRS <> V2Cnrs  or
310500120725                           SaveNCD <> V2Cncd  or
310600120725                           SaveNCA <> V2Cnca  or
310700120725                           SaveCTM <> V2Cctm ) );
310800120725
310900120725         ENDIF;
311000120725
311100120725         If  rpyEsito <> *zero   or   TIVSSds.VSSksu = *blank
311200120725                                 or   TIVSSds.VSSksu = *zero;
311300120725           VIDmsg = $Msg(14);
311400120725           if  V2Ccba = c_EasyWEB  and  V2Dfls = *blank   and
311500120725               V2Cncd = 1          and  V2Cnca = *all'9';
311600120725             //F13Attivo = *on;       ?(già così)?
311700120725             //VIDmsg = %trimr( VIDmsg ) + ': selezionare da F13 ';
311800120725             VIDmsg = %trimr( VIDmsg ) + ': F13 per segnacolli ';
311900120725           endif;
312000120725           PosCurCKS   = *on;
312100120725           ErrMessage  = *on;
312200120725           ErrGenerico = *on;
312300120725           leavesr;
312400120725         EndIf;
312500091124
312600091126         // -?Serie obbligatoria?
312700091124         if  V2Cnrs = *zero;
312800091130           VIDmsg = %trimr($Msg(15)) + ' "' + c_EasyWEB + '"';
312900120816           // -?Già abilitato F13=Interrog. tab. "3EW"?
313000120816           if  F13Attivo;
313100120816             VIDmsg = %trimr( VIDmsg ) + ': F13 per selezionare';
313200120816           endif;
313300091124           PosCurNRS   = *on;
313400091124           ErrMessage  = *on;
313500091124           ErrGenerico = *on;
313600091124           leavesr;
313700091124         endif;
313800091124
313900091124       ENDSR;
314000100211
314100100211       //--------------------------------------------------------------
314200120215       //?Verifica abilitazione cliente (SE unificante) in tab. "3EW"  ?
314300120215       //?(SE tolto supporto EasySped-WEB / lasciato altro)            ?
314400100211       //--------------------------------------------------------------
314500100211       BEGSR  sr_Ctrl3EW;
314600100407
314700120215         // -?Controlli da fare SOLO per cliente padre?
314800100407         if  V2Ccli <> V2Ccks;
314900100407           leavesr;
315000100407         endif;
315100101029
315200101029         // -?Se cliente Unificante presente con più codici Strategi,?
315300120725         //  ?occorre selezionare quello corretto?
315400120725         if  wCount_3EW < *zero;
315500120725           exsr  sr_Count3EW;
315600120725         endif;
315700101029         If  wCount_3EW > 1;
315800101029           $VideoPrec  = $Video;
315900101029           $Video      = 'W4';
316000101029           reset  $InzW04;
316100101029           ErrGenerico = *on;
316200101029           leavesr;
316300101029         EndIf;
316400100304
316500120305         // -?Abbandonare il "Supporto cliente a BRT" "ESWEB" è?
316600101216         //  ?impossibile se cliente padre con figli "ESWEB"?
316700101216         if  V2Ccli =  sch_SKC(1)   and   sch_SKC(2) > *zero      and
316800101216             V2Ccba <> c_EasyWeb    and   §3Ccba     = c_EasyWeb  and
316900101216             %lookup( *on : sch_SKEW : 2 ) > *zero;
317000100216           VIDmsg = $Msg(41);
317100100212           PosCurCBA   = *on;
317200100212           ErrMessage  = *on;
317300100212           ErrGenerico = *on;
317400100212           leavesr;
317500100212         endif;
317600100212
317700100211         // -?Reperimento chiave completa per tab. "3EW"?
317800100211         //  ?se cliente non trovato nel file TIVSS non può essere?
317900100304         //  ?  in tab. "3EW" - probabilmente è figlio (quindi OK)?
318000100215         //  ?(GIÀ FATTO NELLA SUBR. DI INIZIALIZZAZIONE "sr_InzD02")?
318100120725         //  ?=> PUÒ, PUÒ essere in tab. "3EW"...!?
318200120725         //  ?Potrebbe essere già stata fatta scadere l'abilitazione?
318300120725         //  ?ad EasySpedWEB (nel file TIVSS), lasciando "ESWEB" come?
318400120725         //  ?supporto qui in "3C": $Found_TIVSS sarebbe *off !!!?
318500120725         //  ?Ma i controlli qui successivi NON avrebbero senso.?
318600100312         if  NOT  $Found_TIVSS;
318700100215           leavesr;
318800100215         endif;
318900100312
319000120305         // -?Per abbandonare il "Supporto cliente a BRT" "ESWEB"?
319100100312         //  ?occorre che l'applicazione risulti scaduta?
319200120215         //  ?=> OCCORRERÀ ANNULLARE LA TAB. "3EW" A MANO !!!?
319300120215         //                                      ?(da *pgm TNTB79R)?
319400120725         //  ?=> TIVSSds.VSSdsc non può essere < *date:?
319500120725         //     ?sarebbe $Found_TIVSS = *off !!!?
319600120725         //if  V2Ccba        <> c_EasyWeb   and
319700120725         //    §3Ccba         = c_EasyWeb   and
319800120725         //    TIVSSds.VSSdsc < wDate;
319900120725         //  leavesr;
320000120725         //endif;
320100100407
320200120215         // -?Verifica esistenza del padre in tab. "3EW"?
320300120215         //  ?Occorre PRIMA eliminare l'abilitazione a STRATEGI per?
320400120215         //  ?  EasySped-WEB del cliente;?
320500120215         //  ?poi (con questo *pgm, richiamato da TIS725R1) il supporto?
320600120215         //  ?  cliente a BRT sarà modificabile!
320700120215         if  Not  $3EWupd                                  AND
320800120215             getTabella ( c_Tntbe : '3EW' : TIVSSds.VSSksu :
320900100407                          TIVSSds.VSSsun : *blank : *blank :
321000100407                          *omit : *omit :
321100100407                          *omit : *omit : *omit : *omit :
321200100407                          *omit : *omit : *omit : *omit :
321300120814                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni ) = *zero
321400120814             AND  ds_TNTBE.TBEatb = *blank;
321500120814           d3EW_Temp = ds_TNTBE.TBEuni;
321600120814           if  d3EW_Temp.§3EWFAA = *blank;
321700120814             VIDmsg = $Msg(32);
321800120814             PosCurCBA   = *on;
321900120814             ErrMessage  = *on;
322000120814             ErrGenerico = *on;
322100120814             leavesr;
322200120814           endif;
322300100407         endif;
322400100211
322500100211       ENDSR;
322600110311
322700110311       //--------------------------------------------------------------
322800110311       //?Verifica se range segnacolli del padre è diverso             ?
322900110311       //--------------------------------------------------------------
323000110311       BEGSR  sr_CtrlRngPadre;
323100110311
323200110311         F1Attivo = *off;
323300110311
323400110311         // -?Controlli da fare solo per cliente figlio?
323500110311         if  V2Ccli = V2Ccks;
323600110311           leavesr;
323700110311         endif;
323800110311
323900120305         // -?Controlli da fare solo per supporto cliente -> BRT?
324000110311         //  ?diverso da "ESWEB"?
324100110311         if  V2Ccba = c_EasyWEB;
324200110311           leavesr;
324300110311         endif;
324400110311
324500110311         // -?Padre "EasyWEB"?
324600110311         IF  p_§3Ccba = c_EasyWEB;
324700110311
324800110311           // -?Reperimento codice completo STRATEGI del cliente "PADRE"?
324900120725           //if  TIVSSds.VSSksu <> '0' + %editc( V2Ccks : 'X' );
325000120725           if  rqsKSU <> '0' + %editc( V2Ccks : 'X' );
325100110311             clear  rqsSUN;
325200110311             exsr  sr_TIS7701_x_TIVSS;
325300110311           endif;
325400110311
325500110314           // -?Reperimento dati dal padre dalla tab. "3EW"?
325600110314           if  SavCliTab3CPEW <> %editc( V2Ccks : 'X' ) + '3EW';
325700110314             SavCliTab3CPEW = %editc( V2Ccks : 'X' ) + '3EW';
325800110311             if  getTabella ( c_Tntbe : '3EW' : TIVSSds.VSSksu :
325900110311                              TIVSSds.VSSsun : *blank : *blank :
326000110311                              *omit : *omit :
326100110311                              *omit : *omit : *omit : *omit :
326200110311                              *omit : *omit : *omit : *omit :
326300120605                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
326400120605                            = *zero;
326500110311               d3EW_Padre = ds_TNTBE.TBEuni;
326600110311             endif;
326700110311           endif;
326800110311
326900110311         // -?Padre NON "EasyWEB"?
327000110311         ELSE;
327100110311
327200110314           // -?Reperimento dati dal padre dalla tab. "3CP"?
327300110314           if  SavCliTab3CPEW <> %editc( V2Ccks : 'X' ) + '3CP';
327400110314             SavCliTab3CPEW = %editc( V2Ccks : 'X' ) + '3CP';
327500110311             k_TBEcod = '3CP';
327600110311             k_TBEke1 = %editc( V2Ccks : 'X' );
327700110311             setll  %kds( k05tntbe01 : 2 )  TNTBE000;
327800110311             reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
327900110311             dow  Not %eof(TNTBE01L);
328000110311               if  %subst(TBEke2:1:2) = %editc(p_§3Cnrs:'X') and
328100110311                   TBEatb = *blank;
328200110311                 wNSI_Padre = %int( %subst( TBEke2 : 3 : 7 ) );
328300110311                 d3CP_Padre = TBEuni;
328400110311                 leave;
328500110311               endif;
328600110311               reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
328700110311             enddo;
328800110311           endif;
328900110311
329000110311         ENDIF;
329100110311
329200110311
329300110311         // -?Abilitazione F1=Reperimento Serie e Segnacolli da Padre?
329400110421         if   p_§3Ccba   <> V2Ccba            OR
329500110421
329600110421              p_§3Cnrs   <> V2Cnrs            OR
329700110311
329800110421            ( p_§3Ccba   =  c_EasyWEB         and
329900110311             (d3EW_Padre.§3EWnsi <> V2Cncd    or
330000110311              d3EW_Padre.§3EWnsf <> V2Cnca) ) OR
330100110311
330200110421            ( p_§3Ccba   <> c_EasyWEB         and
330300110311             (wNSI_Padre <> V2Cncd            and
330400110311              wNSI_Padre <> *zero             and
330500110311              V2Cncd     <> 1)                or
330600110311             (d3CP_Padre.§3CPal  <> V2Cnca    and
330700110311              d3CP_Padre.§3CPal  <> *zero     and
330800110311              V2Cnca     <> *all'9') );
330900110311
331000110311           F1Attivo = *on;
331100110422           if  Not SavF1Attivo;
331200110422             ErrGenerico = *on;
331300110422           endif;
331400110311
331500110311         endif;
331600110311
331700110311       ENDSR;
331800120725
331900120725       //--------------------------------------------------------------
332000120725       //?Reperimento dati da tab. "3EW" del cliente e conteggio dei   ?
332100120725       //?record per lo stesso unificante.                             ?
332200120725       //--------------------------------------------------------------
332300120725       BEGSR  sr_Count3EW;
332400120725
332500120725         clear  d3EW;
332600120725         clear  wCount_3EW;
332700120725
332800120725         k_TBEcod = '3EW';
332900120725         k_TBEke1 = '0' + %editc( V2Ccks : 'X' );
333000120725         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
333100120725         reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
333200120725
333300120725         DoW  Not %eof(TNTBE01L);
333400120814
333500120814           d3EW_Temp = TBEuni;
333600120725
333700120814           if  (TBEatb = *blank   and   d3EW_Temp.§3EWfaa = *blank)   or
333800121119               TBLflg <> *blank   or
333900121119               (Not %found(TABEL00F)  and   d3EW_Temp.§3EWfaa <> *blank);
334000120725
334100120725             wCount_3EW += 1;
334200120725
334300120725             if  (TIVSSds.VSSsun <= *zero  and  wCount_3EW = 1)  or
334400120725                 TBEke2 = TIVSSds.VSSsun;
334500120725               d3EW = TBEuni;
334600120725             endif;
334700120725
334800120725           endif;
334900120725
335000120725           reade(N)  %kds( k05tntbe01 : 2 )  TNTBE000;
335100120725
335200120725         EndDo;
335300120726
335400120726
335500120726         If  wCount_3EW = 1;
335600120726
335700120726           TNTB79ds.o3EWksu = TBEke1;
335800120726           TNTB79ds.o3EWsun = TBEke2;
335900120726           TNTB79ds.o3EWuni = d3EW;
336000120726           wCount_3EW = 1;
336100120726           rqsSUN = TNTB79ds.o3EWsun;
336200120726           exsr  sr_TIS7701_x_TIVSS;
336300120726           $Bypass_01 = *on;
336400120726
336500120726         EndIf;
336600120725
336700120725       ENDSR;
336800091124
336900091124       //--------------------------------------------------------------
337000091124       //?Controllo/Decodifica Codice Trattamento Merce.               ?
337100091124       //--------------------------------------------------------------
337200091124       BEGSR  sr_CtrlCTM;
337300091124
337400091124         clear  V2Dctm;
337500091124
337600091126         // ->?Controllo / Decodifica?
337700091125         ds_TNTBE.TBEke1 = V2Cctm;
337800091125         if  getTabella ( c_Tabel : '1B'  : ds_TNTBE.TBEke1 :
337900091125                          *omit   : *omit : *omit :
338000091125                          *omit : *omit :
338100091125                          *omit : *omit : *omit : *omit :
338200091125                          *omit : *omit : *omit : *omit :
338300091125                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
338400091125                        = *zero      AND
338500091125             ds_TNTBE.TBEatb = *blank;
338600091125           ds1B   = ds_TNTBE.TBEuni;
338700091125           V2Dctm = §1Bdes;
338800091125         else;
338900091125           V2Dctm = *all'? ';
339000091125           VIDmsg = $Msg(16);
339100091125           PosCurCTM   = *on;
339200091125           ErrMessage  = *on;
339300091125           ErrGenerico = *on;
339400091125           leavesr;
339500091125         endif;
339600091124
339700091124       ENDSR;
339800091104
339900091104       //--------------------------------------------------------------
340000091104       //?Gestione videata W01.                                        ?
340100091104       //--------------------------------------------------------------
340200091104       BEGSR  sr_GesW01;
340300091104
340400091126         // -?Inizializzazione window?
340500091104         if  $InzW01 = *on;
340600091127           clear  TB28W1;
340700091126           $InzW01 = *off;
340800091104         endif;
340900091104
341000091126         // -?Emissione window?
341100091104         if  Not $Protect;
341200091127           exfmt  TB28W1;
341300091104         else;
341400091127           write  TB28W1;
341500091104           exfmt  Protect;
341600091104         endif;
341700091104         reset  ErrMessage;
341800091104         reset  ErrGenerico;
341900091104         clear  VIDmsg;
342000091104
342100091126         // -?F12=Ritorno?
342200091104         if  dsp_aid = c_F12;
342300091104           $Video = $VideoPrec;
342400150319           clear  $VideoPrec;
342500091109           leavesr;
342600091104         endif;
342700091104
342800091126         // -?Invio?
342900140903         $CtrlRngSgnCl = *off;
343000091104         exsr  sr_CtrW01;
343100140718         if  $CtrlRngSgnCl;
343200140718           exsr  sr_CtrW01;
343300140718         endif;
343400091109         if  NOT ErrGenerico;
343500091109           // ->?Ritorno alla videata D02?
343600150319           $Video = 'D2';
343700150319           clear  $VideoPrec;
343800091109           leavesr;
343900091104         endif;
344000091104
344100091104       ENDSR;
344200091104
344300091104       //--------------------------------------------------------------
344400091104       //?Controllo dati immessi nella window W01.                     ?
344500091104       //--------------------------------------------------------------
344600091104       BEGSR  sr_CtrW01;
344700091104
344800091104         %subst(IndDspF : 81) = *off;
344900140903         //$CtrlRngSgnCl = *off;
345000091104
345100100407         select;
345200100407
345300100407           // -?Effettuare una selezione?
345400100407           when  W1Cncl = *zero   and   W1Cnrs = *zero;
345500100407             VIDmsg = $Msg(28);
345600100407             PosCurW1ncl = *on;
345700100407             ErrMessage  = *on;
345800100407             ErrGenerico = *on;
345900100407             leavesr;
346000100407
346100100407           // -?Selezionare Numero colli OPPURE Segnacollo da-al?
346200100407           when  W1Cncl <> *zero   and   W1Cnrs <> *zero;
346300100407             VIDmsg = $Msg(29);
346400100407             PosCurW1ncl = *on;
346500100407             ErrMessage  = *on;
346600100407             ErrGenerico = *on;
346700100407             leavesr;
346800100407
346900100407           // -?Controllo range segnacolli?
347000100407           when  W1Cnrs <> *zero   and   W1Cnca = *zero;
347100100407             VIDmsg = $Msg(30);
347200100407             PosCurW1nca = *on;
347300100407             ErrMessage  = *on;
347400100407             ErrGenerico = *on;
347500100407             leavesr;
347600100407           when  W1Cnrs <> *zero   and   W1Cncd > W1Cnca;
347700100407             VIDmsg = $Msg(31);
347800100407             PosCurW1ncd = *on;
347900100407             ErrMessage  = *on;
348000100407             ErrGenerico = *on;
348100100407             leavesr;
348200100407
348300100407         endsl;
348400100514
348500100514         // -?Sostituzione collo inziale del range con 1 se 0?
348600100514         if  W1Cnrs <> *zero   and   W1Cncd <= *zero;
348700100514           W1Cncd = 1;
348800100514           write  TB28W1;
348900100514           if  $Protect;
349000100514             write  Protect;
349100100514           endif;
349200100514         endif;
349300140123
349400140718         // -?Numero colli richiesto:?
349500140718         //  ?almeno 10.000 e comunque multiplo di 5.000?
349600140123         select;
349700140718           when  W1Cncl > *zero   and   W1Cncl < 10000;
349800140718             VIDmsg = %trimr($Msg(46)) +
349900140718                      ' DEVE prevedere almeno 10.000 colli';
350000140718             PosCurW1ncl = *on;
350100140718             ErrMessage  = *on;
350200140718             ErrGenerico = *on;
350300140718             leavesr;
350400140718           when  W1Cncl > 10000   and   %rem( W1Cncl : 5000 ) > *zero;
350500140123             VIDmsg = %trimr($Msg(46)) +
350600140718                      ' NON è multiplo di 5.000';
350700140123             PosCurW1ncl = *on;
350800140123             ErrMessage  = *on;
350900140123             ErrGenerico = *on;
351000140123             leavesr;
351100140317           when  W1Cncd > *zero;
351200140317       //  when  W1Cncd > *zero   and
351300140317       //        %rem( W1Cnca - W1Cncd + 1 : 1000 ) > *zero;
351400140903             // -?Controllo da NON fare SE inserito il range dall'utente?
351500140903             //  ?(su richiesta di Mirko)?
351600140903             if  $CtrlRngSgnCl   and
351700140903                 %rem( W1Cncd - 1 : 5000 ) > *zero;
351800140903               VIDmsg = %trimr($Msg(47));
351900140903               PosCurW1ncd = *on;
352000140903               ErrMessage  = *on;
352100140903               ErrGenerico = *on;
352200140903               leavesr;
352300140903             endif;
352400140317             if W1CncA = *hival;
352500140317               wCncA = W1CncA + 1;
352600140317             else;
352700140317               wCncA = W1CncA;
352800140317             endif;
352900140903             if  wCncA - W1CncD + 1 < 10000;
353000140903               VIDmsg = %trimr($Msg(46)) +
353100140903                        ' DEVE prevedere almeno 10.000 colli';
353200140903               PosCurW1nca = *on;
353300140903               ErrMessage  = *on;
353400140903               ErrGenerico = *on;
353500140903               leavesr;
353600140903             endif;
353700140917             // -?Controllo da NON fare SE inserito il range dall'utente?
353800140917             //  ?(su richiesta di Mirko)?
353900140917             if  $CtrlRngSgnCl   and
354000140917                 %rem( wCncA - W1CncD + 1 : 5000 ) > *zero;
354100140317               VIDmsg = %trimr($Msg(46)) +
354200140718                       ' NON è multiplo di 5.000';
354300140317               PosCurW1ncd = *on;
354400140317               ErrMessage  = *on;
354500140317               ErrGenerico = *on;
354600140317               leavesr;
354700140317             endif;
354800140123         endsl;
354900091104
355000091126         // -?Ricerca serie parziale?
355100091126         // ->?Selezione per numero colli?
355200091104         IF  W1Cncl > *zero;
355300091104
355400091104           clear  TNTB54ds;
355500100215           if  V2Dfls > *zero;
355600100215             I54fil = V2Dfls;
355700100215           else;
355800100215             I54fil = %subst( V1Ccli : 1 : %len(I54fil) );
355900100215           endif;
356000091104           I54tel = 'R';
356100091104           I54nrs = %editc( V2Cnrs : 'X' );
356200091104           I54tot = W1Cncl;
356300091104           kpjbu  = TNTB54ds;
356400110207           tntb54r ( kpjba : ubLeg3C_SKC );
356500091104           TNTB54ds = kpjbu;
356600091104
356700091104           select;
356800091126             // ->?errore SQL?
356900091104             when  O54err = 'S';
357000091125               VIDmsg = $Msg(18);
357100091104               PosCurW1nrs = *on;
357200091104               ErrMessage  = *on;
357300091104               ErrGenerico = *on;
357400091104               leavesr;
357500091126             // ->?serie già presente ma parziale: errore bloccante?
357600091104             when  O54err = '1';
357700091126               VIDmsg = $Msg(33);
357800091104               PosCurW1nrs = *on;
357900091104               ErrMessage  = *on;
358000091104               ErrGenerico = *on;
358100091104               leavesr;
358200120113             // ->?Serie NON ammessa (99)?
358300120113             when  O54err = '9';
358400120113               VIDmsg = $Msg(43);
358500120113               PosCurW1nrs = *on;
358600120113               ErrMessage  = *on;
358700120113               ErrGenerico = *on;
358800120113               leavesr;
358900091104           endsl;
359000091104
359100091104           If  O54dal <> *zero;
359200140718             if  SavW1Cnrs <> O54nrs  or
359300140718                 SavW1Cncd <> O54dal  or
359400140718                 SavW1Cnca <> O54al;
359500140718               $CtrlRngSgnCl = *on;
359600140718               SavW1Cnrs = O54nrs;
359700140718               SavW1Cncd = O54dal;
359800140718               SavW1Cnca = O54al;
359900140718               clear  W1Cncl;
360000140718               W1Cnrs = O54nrs;
360100140718               W1Cncd = O54dal;
360200140718               W1Cnca = O54al;
360300140718               leavesr;
360400140718             endif;
360500121128             V2Dfls = I54fil;
360600091106             V2Cnrs = O54nrs;
360700091104             V2Cncd = O54dal;
360800140718             V2Cnca = O54al;
360900091104           Else;
361000091126             // ->?SE non è stata trovata la disponibilità di serie?
361100091126             //   ?parziale => ricerca per serie completa?
361200091104             clear  TRTB28ds2;
361300100215             if  V2Dfls > *zero;
361400100215               ParFIL = %int(V2Dfls);
361500100215             else;
361600100215               ParFIL = V2Ccli / 10000;
361700100215             endif;
361800091104             ParELB = 'R';
361900111215             trtb28r2 ( TRTB28ds2 : ubLeg3C_SKC );
362000120113             select;
362100120113               // ->?Serie NON ammessa (99)?
362200120113               when  ParERR = '9';
362300120113                 VIDmsg = $Msg(43);
362400120113                 PosCurW1nrs = *on;
362500120113                 ErrMessage  = *on;
362600120113                 ErrGenerico = *on;
362700120113                 leavesr;
362800091126               // -?Non trovata serie completa libera?
362900120113               when  ParERR <> *blank;
363000120113                 VIDmsg = $Msg(27);
363100120113                 PosCurW1nrs = *on;
363200120113                 ErrMessage  = *on;
363300120113                 ErrGenerico = *on;
363400120113                 leavesr;
363500120113               other;
363600121128                 V2Dfls = %editc( ParFIL : 'X' );
363700120113                 V2Cnrs = ParNRS;
363800120113                 V2Cncd = 1;
363900120113                 V2Cnca = W1Cncl;
364000120113             endsl;
364100091104           EndIf;
364200091104
364300091126         // ->?Selezione per segnacolli dal/al?
364400091104         ELSE;
364500091104
364600101014           exsr  sr_Ctrl_NRS_SCx;
364700101014           if  ErrGenerico = *on;
364800101014             leavesr;
364900101014           endif;
365000091104
365100121128           V2Dfls = I54fil;
365200091104           V2Cnrs = W1Cnrs;
365300091104           V2Cncd = W1Cncd;
365400091104           V2Cnca = W1Cnca;
365500091104
365600091104         ENDIF;
365700091104
365800091104       ENDSR;
365900091104
366000091104       //--------------------------------------------------------------
366100091104       //?Gestione window W02.                                         ?
366200091104       //--------------------------------------------------------------
366300091104       BEGSR  sr_GesW02;
366400091104
366500091126         // -?Inizializzazione window?
366600091104         if  $InzW02 = *on;
366700091104           exsr  sr_InzW02;
366800091104           $InzW02 = *on;
366900091104         endif;
367000091104
367100091126         // -?Emissione window?
367200091127         exfmt  TB28W2;
367300091104
367400091126         // -?F12=Ritorno?
367500091104         if  dsp_aid = c_F12;
367600091106           $Video = $VideoPrec;
367700150319           clear  $VideoPrec;
367800091104           clear  V2F3CE;
367900091104         endif;
368000091104
368100091104       ENDSR;
368200091104
368300091104       //--------------------------------------------------------------
368400091104       //?Inizializzazione window W02.                                 ?
368500091104       //--------------------------------------------------------------
368600091104       BEGSR  sr_InzW02;
368700091104
368800091127         clear  TB28W2;
368900091104
369000091104         clear  d3CE;
369100091104
369200120913         //if  V2Cnrs = *zero;
369300120913         //  leavesr;
369400120913         //endif;
369500091104
369600091126         // -?Reperimento tab. "3CE" per cliente in esame?
369700091104         if  getTabella ( c_Tntbe : '3CE' : V1Ccli :
369800091104                          *blank: *omit : *omit :
369900091104                          *omit : *omit :
370000091104                          *omit : *omit : *omit : *omit :
370100091104                          *omit : *omit : *omit : *omit :
370200091104                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
370300091106                          = *zero      AND
370400091104               ds_TNTBE.TBEatb = *blank;
370500091109           d3CE = ds_TNTBE.TBEuni;
370600091104         endif;
370700091104
370800091126         // -?Impostazione campi a video?
370900091104         if  §3CEedat > *zeros;
371000091104           wDate_Eur = %date( §3CEedat : *iso );
371100091104           W2edat = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
371200091105                      ( %subdt(wDate_Eur : *months) * 10000 ) +
371300091105                      ( %subdt(wDate_Eur : *years) ) );
371400091104         endif;
371500091104         W2ever = §3CEever;
371600091104         W2cver = §3CEcver;
371700091104         if  §3CEcdsc > *zeros;
371800091104           wDate_Eur = %date( §3CEcdsc : *iso );
371900091104           W2cdsc = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
372000091104                      ( %subdt(wDate_Eur : *months) * 10000 ) +
372100091105                      ( %subdt(wDate_Eur : *years) ) );
372200091104         endif;
372300091104         W2sncd = §3CEsncd;
372400091104         W2snca = §3CEsnca;
372500091104         W2sncc = §3CEsncc;
372600091104         W2obj    = §3CEobj;
372700091104         W2corpo  = §3CEcorpo;
372800091104         W2typweb = §3CEtypweb;
372900091104         W2nrs3CE  = §3CEnrs;
373000091104         W2cvermax = §3CEcvermx;
373100091104         if  §3CEcddemx > *zeros;
373200091104           wDate_Eur = %date( §3CEcddemx : *iso );
373300091104           W2cddemax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
373400091104                         ( %subdt(wDate_Eur : *months) * 10000 ) +
373500091104                         ( %subdt(wDate_Eur : *years) ) );
373600091104         endif;
373700091104         if  §3CEcdscmx > *zeros;
373800091104           wDate_Eur = %date( §3CEcdscmx : *iso );
373900091104           W2cdscmax = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
374000091104                         ( %subdt(wDate_Eur : *months) * 10000 ) +
374100091104                         ( %subdt(wDate_Eur : *years) ) );
374200091104         endif;
374300120316         W2esvsrv = §3CEesvsrv;
374400091104
374500091126         // -?Decodifica il Tipo Aggiornamento Web?
374600091104         select;
374700091104           when  §3CEtypweb = *blank;
374800100107           when  §3CEtypweb = '0 ';
374900100107             W2typwebD = 'Funzionalità NON attiva';
375000091104           when  §3CEtypweb = '1 ';
375100091104             W2typwebD = 'Funzionalità attiva';
375200100107           when  §3CEtypweb = '2 ';
375300100107             W2typwebD = 'Framework 1.0';
375400100107           when  §3CEtypweb = '4 ';
375500100107             W2typwebD = 'Framework 1.1';
375600100107           when  §3CEtypweb = '6 ';
375700100107             W2typwebD = 'Framework 1.1 & 1.0';
375800100107           when  §3CEtypweb = '8 ';
375900100107             W2typwebD = 'Framework 2.0';
376000100107           when  §3CEtypweb = '10';
376100100107             W2typwebD = 'Framework 2.0 & 1.0';
376200100107           when  §3CEtypweb = '12';
376300100107             W2typwebD = 'Framework 2.0 & 1.1';
376400091104           other;
376500091104             W2typwebD = *all'? ';
376600091104         endsl;
376700091104
376800091104       ENDSR;
376900091104
377000091104       //--------------------------------------------------------------
377100091104       //?Gestione window W03.                                         ?
377200091104       //--------------------------------------------------------------
377300091104       BEGSR  sr_GesW03;
377400091104
377500091126         // -?Inizializzazione window?
377600091106         exsr  sr_InzW03;
377700091104
377800091126         // -?Emissione window?
377900091104         if  (W3txt1 + W3txt2 + W3txt3) <> *blank;
378000091127           exfmt  TB28W3;
378100091104         endif;
378200091104
378300091126         // -?Enter=Continua?
378400091106         $Video = $VideoPrec;
378500150319         clear  $VideoPrec;
378600091104
378700091104       ENDSR;
378800091104
378900091104       //--------------------------------------------------------------
379000091104       //?Inizializzazione window W03.                                 ?
379100091104       //--------------------------------------------------------------
379200091104       BEGSR  sr_InzW03;
379300091104
379400091127         clear  TB28W3;
379500091104
379600091104         if  V2Cnot = *blank;
379700091104           leavesr;
379800091104         endif;
379900091104
380000091126         // -?Verifica esistenza tab. "KB4" per cliente in esame?
380100091104         $KB4 = ( getTabella (c_Tntbe : 'KB4' : V1Ccli :
380200091111                              *omit : *omit : *omit :
380300091104                              *omit : *omit :
380400091104                              *omit : *omit : *omit : *omit :
380500091104                              *omit : *omit : *omit : *omit :
380600091104                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni)
380700091104                              = *zero );
380800091104         select;
380900091104
381000091104           when  *inKE      and
381100091104                (NOT $KB4   or   ds_TNTBE.TBEatb <> *blank);
381200091104             W3txt2 = 'Abilitare il cliente in tab. "KB4".';
381300091104
381400091104           when  *inKF      and
381500091104                (NOT $KB4   or   ds_TNTBE.TBEatb <> *blank);
381600091104             W3txt2 = 'Abilitare il cliente in tab. "KB4".';
381700091104
381800091104           when  *inKQ  and
381900091104                ($KB4   and   ds_TNTBE.TBEatb = *blank);
382000091104             W3txt2 = 'Annullare il cliente in tab. "KB4".';
382100091104
382200091104         endsl;
382300091104
382400091104       ENDSR;
382500101027
382600101027       //--------------------------------------------------------------
382700101027       //?Gestione window W04.                                         ?
382800101027       //--------------------------------------------------------------
382900101027       BEGSR  sr_GesW04;
383000101027
383100101027         // -?Inizializzazione window?
383200101027         if  $InzW04 = *on;
383300101027           clear  TB28W4;
383400101027           W4Csun  = '?';
383500101027           $InzW04 = *off;
383600101027         endif;
383700101027
383800101027         // -?Emissione window?
383900101027         if  Not $Protect;
384000101027           exfmt  TB28W4;
384100101027         else;
384200101027           write  TB28W4;
384300101027           exfmt  Protect;
384400101027         endif;
384500101027         reset  ErrMessage;
384600101027         reset  ErrGenerico;
384700101027         clear  VIDmsg;
384800101027
384900101027         // -?F12=Ritorno?
385000101027         if  dsp_aid = c_F12;
385100110725           clear  W4Csun;
385200101027           $Video = $VideoPrec;
385300150319           clear  $VideoPrec;
385400101027           leavesr;
385500101027         endif;
385600101027
385700101027         // -?Invio?
385800101027         exsr  sr_CtrW04;
385900101027         if  NOT ErrGenerico;
386000101027           TIVSSds.VSSksu = '0' + %editc(V2Ccks : 'X');
386100101027           TIVSSds.VSSsun = W4Csun;
386200101027           // ->?Ritorno alla videata precedente?
386300101027           $Video = $VideoPrec;
386400150319           clear  $VideoPrec;
386500101027           leavesr;
386600101027         endif;
386700101027
386800101027       ENDSR;
386900101027
387000101027       //--------------------------------------------------------------
387100101027       //?Controllo dati immessi nella window W04.                     ?
387200101027       //--------------------------------------------------------------
387300101027       BEGSR  sr_CtrW04;
387400101027
387500101027         %subst(IndDspF : 81) = *off;
387600101027
387700101027         Select;
387800101027
387900101027           // -?Ricerca Strategi User Number?
388000101028           When  %scan('?' : W4Csun) > *zeros;
388100101028             clear  W4Csun;
388200101027             exsr  sr_F13D02;
388300101027             select;
388400101028               when  TNTB79ds.o3EWfxx <> *blank;
388500120726                 ErrGenerico = *on;
388600101028               when  TNTB79ds.o3EWerr = *on;
388700101028                 VIDmsg = TNTB79ds.o3EWmsg;
388800101028                 PosCurW4sun = *on;
388900101027                 ErrMessage  = *on;
389000101027                 ErrGenerico = *on;
389100120726                 leavesr;
389200101028               when  TNTB79ds.o3EWksu = '0' + %editc(V2Ccks : 'X');
389300101028                 W4Csun = TNTB79ds.o3EWsun;
389400101027             endsl;
389500101027
389600101027           // -?Effettuare una selezione?
389700101028           When  W4Csun = *blank  or  W4Csun = *zero;
389800101028             VIDmsg = $Msg(42);
389900101028             PosCurW4sun = *on;
390000101027             ErrMessage  = *on;
390100101027             ErrGenerico = *on;
390200101027             leavesr;
390300101027
390400101027           // -?Controllo Strategi User Number selezionato?
390500101027           Other;
390600101028             rqsSUN = W4Csun;
390700101028             exsr  sr_TIS7701_x_TIVSS;
390800101028             select;
390900101028               // -?Codice cliente Strategi errato?
391000101028               when  rpyEsito <> *zero  or  TIVSSds.VSSksu = *blank
391100101028                                        or  TIVSSds.VSSksu = *zero;
391200101028                 VIDmsg = $Msg(14);
391300101028                 PosCurW4sun = *on;
391400101028                 ErrMessage  = *on;
391500101028                 ErrGenerico = *on;
391600101028                 leavesr;
391700101028               // -?Abilitazione scaduta?
391800101028               when  TIVSSds.VSSdsc < wDate;
391900101028                 VIDmsg = $Msg(37);
392000101028                 PosCurW4sun = *on;
392100101028                 ErrMessage  = *on;
392200101028                 ErrGenerico = *on;
392300101028                 leavesr;
392400101028             endsl;
392500101027
392600101027         EndSl;
392700101027
392800101027       ENDSR;
392900111011
393000111011       //--------------------------------------------------------------
393100111011       //?Gestione window W05.                                         ?
393200111011       //--------------------------------------------------------------
393300111011       BEGSR  sr_GesW05;
393400111011
393500111011         // -?Inizializzazione window?
393600111011         if  $InzW05 = *on;
393700111021           write  TB28W5a;
393800111011           exsr  sr_InzW05;
393900111011           $InzW05 = *off;
394000111011         endif;
394100111011
394200111011         // -?Emissione window?
394300111021         exfmt  TB28W5b;
394400111011
394500111011         // -?F12=Ritorno?
394600111011         if  dsp_aid = c_F12;
394700111011           $Video = $VideoPrec;
394800150319           clear  $VideoPrec;
394900111011         endif;
395000111011
395100111011       ENDSR;
395200111011
395300111011       //--------------------------------------------------------------
395400111011       //?Inizializzazione window W05.                                 ?
395500111011       //--------------------------------------------------------------
395600111011       BEGSR  sr_InzW05;
395700111011
395800111021         clear  TB28W5b;
395900111011
396000111011         // -?Reperimento ultimo segnacollo usato?
396100111024         if  V2Dfls > *zero;
396200121121           pInFilSgncl = %int( V2Dfls );
396300111024         else;
396400121121           pInFilSgncl = V2Ccli / 10000;
396500111024         endif;
396600121121         if  sch_SKC(1) = *zero;
396700121121           sch_SKC(1) = V2Ccks;
396800121121         endif;
396900121121         if  ubLastNSC_Rtv ( V2Cnrs : pInFilSgncl :
397000111024                             ubLeg3C_SKC : *omit :
397100140225                             pOutMaxNSC : pOutMaxLNP :
397200140225                             pOutMaxAAS : pOutMaxMGS :
397300140225                             pOutMaxNSP :
397400140225                             pOutLastNSC : pOutLastLNP :
397500140225                             pOutLastAAS : pOutLastMGS :
397600140225                             pOutLastNSP ) < *zero;
397700111011           VIDmsg = 'ERRORE nel reperim. dell''ultimo segnacollo +
397800111011                     utilizzato. Verificare in stampa.';
397900111011           errMessage  = *on;
398000111011           errGenerico = *on;
398100111011           leavesr;
398200111011         endif;
398300111011
398400111011         // -?Impostazione campi a video?
398500121121         W5Cfls  = pInFilSgncl;
398600121121         W5Cnrs  = V2Cnrs;
398700121121         W5Ccks  = V2Ccks;
398800140225         W5CnscM = pOutMaxNSC;
398900140225         if  pOutMaxAAS > *zero  and  pOutMaxMGS > *zero;
399000140225           W5DgmasM  = (pOutMaxAAS * 10000) + pOutMaxMGS;
399100140225           wDate_Eur = %date( W5DgmasM : *iso );
399200140225           W5DgmasM  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
399300111018                         ( %subdt(wDate_Eur : *months) * 10000 ) +
399400111018                         ( %subdt(wDate_Eur : *years) ) );
399500111018         endif;
399600140225         W5CnscL = pOutLastNSC;
399700140225         if  pOutLastAAS > *zero  and  pOutLastMGS > *zero;
399800140225           W5DgmasL  = (pOutLastAAS * 10000) + pOutLastMGS;
399900140225           wDate_Eur = %date( W5DgmasL : *iso );
400000140225           W5DgmasL  = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
400100140225                         ( %subdt(wDate_Eur : *months) * 10000 ) +
400200140225                         ( %subdt(wDate_Eur : *years) ) );
400300140225         endif;
400400111011
400500111011       ENDSR;
400600120517
400700120517       //--------------------------------------------------------------
400800120517       //?Reperimento famiglia completa dell'unificante "originale"    ?
400900120517       //--------------------------------------------------------------
401000120517       BEGSR  sr_ubLeg3C_Orig;
401100120517
401200120517         ubLeg3C_Rtv ( §3Ccks :
401300120517                       ubLeg3C_FlgPF :
401400120517                       ubLeg3C_Padre :
401500120517                       ubLeg3C_SKC :
401600120517                       ubLeg3C_SKEW );
401700120517
401800120517         sch_SKC_Orig = sch_SKC;
401900120517
402000120517       ENDSR;
402100120104
402200120104       //--------------------------------------------------------------
402300120104       //?Reperimento famiglia completa del cliente in esame           ?
402400120104       //--------------------------------------------------------------
402500120104       BEGSR  sr_ubLeg3C;
402600120104
402700120104         // -?Reperimento famiglia completa dalla tab. "3C"?
402800120517         //  ?(se modificato il padre)?
402900120104         if  V2Ccks <> ubLeg3C_Padre;
403000120104           ubLeg3C_Rtv ( V2Ccks :
403100120104                         ubLeg3C_FlgPF :
403200120104                         ubLeg3C_Padre :
403300120104                         ubLeg3C_skC :
403400120104                         ubLeg3C_skEW );
403500120104         endif;
403600120104
403700120104         // -?"Sistemazione" della stessa se modificato il padre?
403800120104         //  ?impostandolo a se stesso?
403900120517         //  ?N.B.: non essendo ancora stato premuto F6, il padre?
404000120517         //        ?reperito risulta ancora l'ultimo registrato...?
404100120104         if  ubLeg3C_FlgPF = 'F'  and  ubLeg3C_Padre <> V2Ccks
404200120104                                  and  V2Ccli = V2Ccks;
404300120104           ubLeg3C_FlgPF  = 'P';
404400120104           ubLeg3C_Padre  = V2Ccks;
404500120104           clear  ubLeg3C_skC;
404600120104           clear  ubLeg3C_skEW;
404700120104           sch_skC(1)  = V2Ccks;
404800120104           sch_skEW(1) = (§3Ccba = c_EasyWeb);
404900120104         endif;
405000120104
405100120104       ENDSR;
405200091105
405300091105       //--------------------------------------------------------------
405400091105       //?Aggiornam.rec. di tutti i file in tutti i sistemi informativi?
405500091105       //--------------------------------------------------------------
405600091105       BEGSR  sr_UpdateAll;
405700091105
405800091126         // -?Ciclo di elaborazione per ogni sistema informativo?
405900091105         For  w_SisInf = 1  To  %elem($Libr);
406000091105
406100091126           // -?Apertura degli archivi?
406200091105           if  w_SisInf > 1;
406300091105             exsr  sr_OpenFileTab;
406400091105           endif;
406500091105
406600091126           // -?Aggiornamento tab. "3C"?
406700091105           exsr  sr_UpdTABEL;
406800091105
406900100330           // -?Aggiornamento tab. "3CE", "3CP" e "3EW"?
407000091105           exsr  sr_UpdTNTBE;
407100091105
407200091105         EndFor;
407300100330
407400121119         // -?Aggiornamento flag §3EWUPD in tab. "3EW"?
407500110616         //  ?(i 2 sistemi informativi sono gestiti dal chiamato)?
407600130604         clear  wRtnEsito;
407700121119         Select;
407800121119
407900121119           // -?F6=Conferma (modifica/inserimento)?
408000121119           When  Not  $3EWupd   and   dsp_aid = c_F06;
408100121119             // ·?del vecchio unificante (se variato unificante)?
408200121119             if  %found(TABEL00F)   and  Old_§3Ccks <> V2Ccks
408300121119                                    and  Old_§3Ccba  = c_EasyWEB;
408400121119               wVSSksu = '0' + %editc( Old_§3Ccks : 'X' );
408500130604               //Ub§3EWupd_Upd ( wVSSksu : *omit : 'A' : *off );
408600130604               wRtnEsito = Ub§3EWupd_Upd ( wVSSksu : TIVSSds_Orig.VSSsun :
408700130604                                           'A' : *off );
408800130605               //if  wRtnEsito < *zero;
408900130605               //  exsr  sr_Send_eMail;
409000130605               //endif;
409100121119             endif;
409200121119             // ·?dell'unificante attuale?
409300121119             //  ?(messo, tolto o lasciato EasyWEB)?
409400121119             if  V2Ccba = c_EasyWEB  or  Old_§3Ccba = c_EasyWEB;
409500130604               wRtnEsito = Ub§3EWupd_Upd ( TIVSSds.VSSksu : TIVSSds.VSSsun :
409600130604                                           'A' : *off );
409700130605               //if  wRtnEsito < *zero;
409800130605               //  exsr  sr_Send_eMail;
409900130605               //endif;
410000121119             endif;
410100121119
410200121119           // -?F16=Annullamento?
410300121119           When  Not  $3EWupd   and   dsp_aid = c_F16   and
410400121119                 Old_§3Ccba = c_EasyWEB;
410500121119             wVSSksu = '0' + %editc( Old_§3Ccks : 'X' );
410600130604             wRtnEsito = Ub§3EWupd_Upd ( wVSSksu : TIVSSds_Orig.VSSsun :
410700130604                                         'A' : *off );
410800130605             //if  wRtnEsito < *zero;
410900130605             //  exsr  sr_Send_eMail;
411000130605             //endif;
411100120207
411200121119           // -?F5=Ripristino?
411300121119           When  Not  $3EWupd   and   dsp_aid = c_F05   and
411400121119                 V2Ccba = c_EasyWEB;
411500130604             wRtnEsito = Ub§3EWupd_Upd ( TIVSSds.VSSksu : TIVSSds.VSSsun :
411600130604                                         'A' : *off );
411700130605             //if  wRtnEsito < *zero;
411800130605             //  exsr  sr_Send_eMail;
411900130605             //endif;
412000121119         EndSl;
412100091105
412200091126         // -?Messaggio di avviso: allineare la tab. "KB4"!?
412300091109         if  dsp_aid = c_F05  or  dsp_aid = c_F06  or  dsp_aid = c_F16;
412400091106           $VideoPrec = $Video;
412500091105           $Video = 'W3';
412600091105           exsr  sr_GesW03;
412700091105         endif;
412800091105
412900091105       ENDSR;
413000091130
413100091130       //--------------------------------------------------------------
413200091130       //?Riempimento struttura dati ds3C.                             ?
413300091130       //--------------------------------------------------------------
413400091130       BEGSR  sr_RieDS3C;
413500091130
413600091130         clear  ds3C;
413700100212
413800100212         // -?Impostazione campi per NON "EasyWeb"?
413900100212         if  V2Ccba <> c_EasyWEB;
414000100518           //§3Cnrs = V2Cnrs;         ?(da memorizzare comunque)?
414100100518           if  V2Cnrs > *zero;
414200100212             §3Cfls = %int( V2Dfls );
414300100212           endif;
414400100212           §3Cctm = V2Cctm;
414500100212         endif;
414600100212
414700100212         // -?Impostazione altri campi (per tutti)?
414800100518         §3Cnrs = V2Cnrs;
414900091130         §3Cokd = V2Cokd;
415000091130         §3Crag = V2Crag;
415100091130         §3Clrm = V2Clrm;
415200091130         §3Calm = V2Calm;
415300091130         §3Cnot = V2Cnot;
415400091130         §3Cabc = V2Cabc;
415500091130         §3Cdkc = V2Cdkc;
415600091130         §3Cavx = V2Cavx;
415700091130         §3Cabd = V2Cabd;
415800091130         §3Caus = V2Caus;
415900091130         §3Carc = V2Carc;
416000091130         §3Cmvr = V2Cmvr;
416100091222         §3Cbc2 = V2Cbc2;
416200091130         §3Ccba = V2Ccba;
416300091130         §3Cbac = V2Cbac;
416400091130         §3Cfg1 = V2Cfg1;
416500160317         §3CCSAMB = V2Amb;
416600091130         §3Cfsp = V2Cfsp;
416700160317         §3CCSTFP = V2TFP;
416800091130         §3Ccks = V2Ccks;
416900091130         §3Cnwb = V2Cnwb;
417000091130         §3Cnwc = V2Cnwc;
417100091130         §3Crsb = V2Crsb;
417200091130         §3Cpes = V2Cpes;
417300091130
417400091130       ENDSR;
417500091102
417600091102       //--------------------------------------------------------------
417700091102       //?Aggiornamento record nel file TABEL00F.                      ?
417800091102       //--------------------------------------------------------------
417900091102       BEGSR  sr_UpdTABEL;
418000091106
418100091106         //? N.B.                                                      ?
418200091106         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
418300091106         //   trasmissione (vedi flag TBEFTR).                        ?
418400091106         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
418500091106         //   subito nello stesso file di entrambi i S.I. - in due    ?
418600091106         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
418700091103
418800091105         // -?Aggiornam. tab. "3C" = Invio dati -BOLLETTAZIONE-?   ?
418900091105
419000091126         // -?RI-aggancio record da aggiornare?
419100091126         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
419200091105         if  w_SisInf > 1;
419300121204           k_TBLkut = c_Kut;
419400121204           k_TBLcod = c_Tab;
419500091105           k_TBLkey = V1Ccli;
419600091105           chain  %kds(k03tabel00)  TABEL;
419700091105         endif;
419800091102
419900091102         select;
420000091102
420100091126           // -?F5 = Ripristino?
420200091105           when  dsp_aid = c_F05;
420300100407             if  Not %found(TABEL00F);
420400100407               clear  TABEL;
420500100407               TBLkut = c_Kut;
420600100407               TBLcod = c_Tab;
420700100407               TBLkey = k_TBLkey;
420800100407             endif;
420900100407             exsr  sr_RieDS3C;
421000100407             TBLuni = ds3C;
421100091130             select;
421200121128               //when  %found(TABEL00F)   and   TBLflg = '*';
421300121128               when  %found(TABEL00F);
421400091130                 clear  TBLflg;
421500091130                 clear  TBLftr;
421600091130                 clear  TBLdtr;
421700091130                 //____________
421800091130                 update  TABEL;
421900091130                 //¯¯¯¯¯¯¯¯¯¯¯¯
422000091130               // - Record appena cancellato fisicamente
422100091130               when  NOT %found(TABEL00F);
422200091130                 TBLftt = '1';
422300091130                 clear  TBLflt;
422400091130                 if  w_SisInf = 1;
422500091130                   TBLftr = 'T';
422600091130                 else;
422700091130                   TBLftr = 'R';
422800091130                 endif;
422900091130                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
423000091130                                        %len(wDate) - %len(TBLdtr) + 1 :
423100091130                                        %len(TBLdtr) ) );
423200091130                 //____________
423300091130                 write   TABEL;
423400091130                 //¯¯¯¯¯¯¯¯¯¯¯¯
423500091130             endsl;
423600091102
423700091126           // -?F6 = Aggiornamento / Inserimento?
423800091102           when  dsp_aid = c_F06;
423900091102             if  Not %found(TABEL00F);
424000091106               clear  TABEL;
424100091102               TBLkut = c_Kut;
424200091102               TBLcod = c_Tab;
424300091102               TBLkey = k_TBLkey;
424400091102             endif;
424500121128             clear  TBLflg;
424600091130             exsr  sr_RieDS3C;
424700091106             TBLuni = ds3C;
424800091102
424900091102             TBLftt = '1';
425000091105             clear  TBLflt;
425100091105             if  w_SisInf = 1;
425200091105               TBLftr = 'T';
425300091105             else;
425400091105               TBLftr = 'R';
425500091105             endif;
425600091111             TBLdtr = %int( %subst( %editc(wDate : 'X') :
425700091111                                    %len(wDate) - %len(TBLdtr) + 1 :
425800091111                                    %len(TBLdtr) ) );
425900091102
426000091102             if  %found(TABEL00F);
426100091102               //____________
426200091102               update  TABEL;
426300091102               //¯¯¯¯¯¯¯¯¯¯¯¯
426400091102             else;
426500091102               //____________
426600091102               write   TABEL;
426700091102               //¯¯¯¯¯¯¯¯¯¯¯¯
426800091102             endif;
426900091102
427000091126           // -?F16 = Annullamento?
427100091105           when  dsp_aid = c_F16;
427200091130             if  %found(TABEL00F)   and   TBLflg   = *blank;
427300120116               if  old_§3Ccba = c_EasyWEB;
427400120116                 clear  old_§3Cnrs;
427500120116                 TBLuni = ds3C_old;
427600120116               endif;
427700091106               TBLflg = '*';
427800091106               clear  TBLftr;
427900091106               clear  TBLdtr;
428000091106               //____________
428100091106               update  TABEL;
428200091106               //¯¯¯¯¯¯¯¯¯¯¯¯
428300091106             endif;
428400091102
428500091102         endsl;
428600091102
428700091102       ENDSR;
428800091103
428900091103       //--------------------------------------------------------------
429000091103       //?Aggiornamento altri records nel file TNTBE00F.               ?
429100091103       //--------------------------------------------------------------
429200091103       BEGSR  sr_UpdTNTBE;
429300091106
429400091106         //? N.B.                                                      ?
429500091106         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
429600091106         //   trasmissione (vedi flag TBEFTR).                        ?
429700091106         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
429800091106         //   subito nello stesso file di entrambi i S.I. - in due    ?
429900091106         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
430000091105
430100091103         // -?Aggiornam. tab. "3CP" = estensione per serie parziale?   ?
430200091103
430300091105         Select;
430400091105
430500091126           // -?F6 = Aggiornamento / Inserimento?
430600100215           When  dsp_aid = c_F06   and   V2Ccba <> c_EasyWEB;
430700100402             exsr  sr_Update3CP;
430800100921           When  dsp_aid = c_F06   and   V2Ccba     =  c_EasyWEB
430900100921                                   and   Old_§3Ccba <> c_EasyWEB;
431000100215             exsr  sr_Annull3CP;
431100101026
431200101026           // -?F5 = Ripristino?
431300101026           When  dsp_aid = c_F05   and   V2Ccba <> c_EasyWEB;
431400101026             exsr  sr_Update3CP;
431500091105
431600091126           // -?F16 = Annullamento?
431700101026           When  dsp_aid = c_F16   and   $Found_3CP;
431800091105             exsr  sr_Annull3CP;
431900091105
432000091105         EndSl;
432100100215
432200100331         // -?Aggiorn. tab. "3EW" = dati assegnati alla postaz. EasyWeb?   ?
432300100215
432400101026         Select;
432500100215
432600100215           // -?F5 = Ripristino?
432700100331           //  ?(dati EasyWeb gestibili solo sul padre)?
432800120207           //  ?(il padre deve risultare ancora/nuovamente abilitato ad?
432900120207           //  ?EasyWEB - vedi TIVSSds)?
433000120207           When  dsp_aid = c_F05    and
433100120207                 V2Ccli  = V2Ccks   and   V2Ccba = c_EasyWEB    and
433200120207                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
433300120207                 TIVSSds.VSSsun > *zero;
433400120207             exsr  sr_Ripris3EW;
433500100215
433600100215           // -?F6 = Aggiornamento / Inserimento?
433700101026           //  ?(dati EasyWeb gestibili solo sul padre, MA DAL *PGM?
433800101027           //  ?TNTB79R - NON da questo!!!)?
433900101026           //When  dsp_aid = c_F06    and
434000101026           //      V2Ccli  = V2Ccks   and   V2Ccba = c_EasyWEB;
434100101026           //  exsr  sr_Update3EW;
434200101027
434300101027           // -?F6 = Aggiornamento?
434400121205           //  ?Abbandonato il "Supporto cliente a BRT" "ESWEB"?
434500121205           //  ?(essendo scaduta l'applicazione):?OCCORRE ANNULLARE?
434600121205           //  ?LA TAB. "3EW" (attenzione ai figli) !!!?
434700121205           //  (i controlli sono già stati eseguiti)?
434800121205           //  ?SE mantenuto lo stesso range segnacolli:?
434900130507           //    ?=> si annulla la tab. "3EW" (già riportata in "3CP")?
435000121205           //  ?SE cambiato il range segnacolli:?
435100121205           //    ?=> si "blocca" (annullam. applicativo) la tab. "3EW"?
435200130507           When  dsp_aid    = c_F06   and  V2Ccba    <> c_EasyWeb  and
435300130507                 Old_§3Ccks = V2Ccli  and  Old_§3Ccba = c_EasyWeb;
435400101027             select;
435500110725               // -?Un solo codice in TIVSS?
435600101027               when  wCount_3EW = 1;
435700120816                 if  V2Cnrs = d3EW.§3EWnrs  and
435800120816                     V2Cncd = d3EW.§3EWnsi  and
435900120816                     V2Cnca = d3EW.§3EWnsf;
436000120814                   exsr  sr_Annull3EW;
436100120814                 else;
436200120814                   exsr  sr_AnnApp3EW;
436300120814                 endif;
436400110725               // -?Un codice selezionato da TIVSS?
436500110725               when  W4Csun > *zero  and  W4Csun = TIVSSds.VSSsun;
436600120816                 if  V2Cnrs = d3EW.§3EWnrs  and
436700120816                     V2Cncd = d3EW.§3EWnsi  and
436800120816                     V2Cnca = d3EW.§3EWnsf;
436900120814                   exsr  sr_Annull3EW;
437000120814                 else;
437100120814                   exsr  sr_AnnApp3EW;
437200120814                 endif;
437300101027             endsl;
437400130507           //  ?Cambiato unificante - sempre EasyWeb?
437500130507           //  ?(se prima padre di se stesso e senza altri figli)?
437600130507           When  dsp_aid     = c_F06   and  V2Ccba     = c_EasyWeb  and
437700130507                 Old_§3Ccks <> V2Ccks  and  Old_§3Ccba = c_EasyWeb  and
437800130507                 Old_§3Ccks  = V2Ccli  and  sch_SKC_Orig(2) = *zero;
437900130507             select;
438000130507               // -?Un solo codice in TIVSS?
438100130507               when  wCount_3EW = 1;
438200130507                 exsr  sr_AnnApp3EW;
438300130507               // -?Un codice selezionato da TIVSS?
438400130507               when  W4Csun > *zero  and  W4Csun = TIVSSds.VSSsun;
438500130507                 exsr  sr_AnnApp3EW;
438600130507             endsl;
438700120816           //  ?Ritorno al "Supporto cliente a BRT" = "ESWEB"?
438800120816           //  ?(mantenendo lo stesso unificante)?
438900120816           When  dsp_aid = c_F06       and
439000120816                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
439100121205                 V2Ccli  = Old_§3Ccks  and  Old_§3Ccba <> c_EasyWeb  and
439200120816                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
439300120816                 TIVSSds.VSSsun > *zero;
439400120816             exsr  sr_Ripris3EW;
439500121119           When  dsp_aid = c_F06       and  Not %found(TABEL00F)     and
439600121119                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
439700121119                 d3EW_Temp.§3EWfaa <> *blank                    and
439800121119                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
439900121119                 TIVSSds.VSSsun > *zero;
440000121119             exsr  sr_Ripris3EW;
440100121205           // -?O passaggio a se stesso come unificante?
440200121205           //  ?(già abilitato ad EasySped-WEB)?
440300121205           When  dsp_aid = c_F06       and
440400121205                 V2Ccli  = V2Ccks      and  V2Ccba     =  c_EasyWEB  and
440500121205                 V2Ccli <> Old_§3Ccks  and
440600121205                 TIVSSds.VSSksu = '0' + %editc( V2Ccli : 'X' )  and
440700121205                 TIVSSds.VSSsun > *zero;
440800121205             exsr  sr_Ripris3EW;
440900100331
441000100331           // -?F16 = Annullamento di un cliente senza dati in "3EW":?
441100100331           //  ?nessun record da annullare?
441200101026           //When  dsp_aid = c_F16      and  Not $Found_TIVSS   and
441300101026           //      Old.§3Ccks = V2Ccli  and  Old.§3Ccba = c_EasyWeb;
441400100215
441500100215           // -?F16 = Annullamento?
441600100215           //  ?(impossibile se codice padre)?
441700101027           //  (i controlli sono già stati eseguiti)?
441800101026           When  dsp_aid    = c_F16   and
441900101028                 Old_§3Ccks = V2Ccli  and  Old_§3Ccba = c_EasyWEB;
442000120814             //exsr  sr_Annull3EW;
442100120814             exsr  sr_AnnApp3EW;
442200100215
442300101026         EndSl;
442400091105
442500091103
442600091103         // -?Aggiornam. tab. "3CE" = estensione per versioni Easy-Sped?
442700091105
442800091126         // -?Rerimento record da aggiornare?
442900091103         k_TBEcod = '3CE';
443000091105         k_TBEke1 = V1Ccli;
443100091111         //clear  k_TBEke2;
443200091111         chain  %kds( k05TNTBE01 : 2 )  TNTBE000;
443300091105
443400091105         select;
443500091105
443600091126           // -?F5 = Ripristino?
443700091105           when  dsp_aid = c_F05;
443800091130             select;
443900091130               when  %found(TNTBE01L)   and   TBEatb  = 'A';
444000091130                 clear  TBEatb;
444100091130                 clear  TBEftr;
444200091130                 //_______________
444300091130                 update  TNTBE000;
444400091130                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
444500100331               // -?Record appena cancellato fisicamente?
444600100331               //  ?(NON scrive nulla: si aspetta il prossimo aggior-?
444700100331               //  ? namento dal file TIESV - vedi *pgm. TNTB67R).?
444800100331               when  NOT %found(TNTBE01L);
444900100331               //clear  TNTBE000;
445000100331               //TBEcod = k_TBEcod;
445100100331               //TBEke1 = k_TBEke1;
445200100331               //clear  d3CE;
445300100331               //TBEuni = d3CE;
445400100331               //TBEftt = 'S';
445500100331               //clear  TBEflt;
445600100331               //if  w_SisInf = 1;
445700100331               //  TBEftr = 'T';
445800100331               //else;
445900100331               //  TBEftr = 'R';
446000100331               //endif;
446100100331               //TBEdtr = wDate;
446200100331               ////_______________
446300100331               //write   TNTBE000;
446400100331               ////¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
446500091130             endsl;
446600091105
446700091126           // -?F6 = Aggiornamento / Inserimento?
446800100331           //  ?(NON scrive nulla: si aspetta il prossimo aggior-?
446900100331           //  ? namento dal file TIESV - vedi *pgm. TNTB67R).?
447000100331           when  dsp_aid = c_F06;
447100100331           //if  Not %found(TNTBE01L);
447200100331           //  clear  TNTBE000;
447300100331           //  TBEcod = k_TBEcod;
447400100331           //  TBEke1 = k_TBEke1;
447500100331           //endif;
447600100331           //clear  d3CE;
447700100331           //TBEuni = d3CE;
447800100331           //TBEftt = 'S';
447900100331           //clear  TBEflt;
448000100331           //if  w_SisInf = 1;
448100100331           //  TBEftr = 'T';
448200100331           //else;
448300100331           //  TBEftr = 'R';
448400100331           //endif;
448500100331           //TBEdtr = wDate;
448600100331           //
448700100331           //if  %found(TNTBE01L);
448800100331           //  //_______________
448900100331           //  update  TNTBE000;
449000100331           //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
449100100331           //else;
449200100331           //  //_______________
449300100331           //  write   TNTBE000;
449400100331           //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
449500100331           //endif;
449600091105
449700091126           // -?F16 = Annullamento?
449800091105           when  dsp_aid = c_F16;
449900091130             if  %found(TNTBE01L)   and   TBEatb <> 'A';
450000091105               TBEatb = 'A';
450100091105               clear  TBEftr;
450200091105               //_______________
450300091105               update  TNTBE000;
450400091105               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
450500091105             endif;
450600091106
450700091106         endsl;
450800091103
450900091103       ENDSR;
451000091105
451100091105       //--------------------------------------------------------------
451200091106       //?Aggiornamento / Inserimento dati in tab. "3CP"               ?
451300091106       //?(estensione per serie parziale)                              ?
451400091105       //--------------------------------------------------------------
451500100402       BEGSR  sr_Update3CP;
451600091102
451700091106         $Found_3CP = *off;
451800091106
451900091106         k_TBEcod = '3CP';
452000091106         k_TBEke1 = V1Ccli;
452100091111         //clear  k_TBEke2;
452200091106         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
452300091106         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
452400091106
452500091106         // -?Aggiornamento (o annullamento) serie parziale?
452600091106         DOW  Not %eof(TNTBE01L);
452700091106
452800091106           Select;
452900091106
453000091126             // -?Vecchia serie parziale nel file (già annullata)?
453100100212             //  ?con serie completa a video  o?
453200120305             //  ?con "Supporto cliente a BRT" = "ESWEB"?
453300091106             When  TBEatb = 'A'     and
453400120731                  (V2Cnrs = *zero    or
453500120731                  (V2Cncd = 1       and   V2Cnca  = *hival)  or
453600100212                   V2Ccba = c_EasyWEB);
453700100212
453800100212             // -?Vecchia serie parziale nel file (da annullare)?
453900120305             //  ?con "Supporto cliente a BRT" = "ESWEB"?
454000100212             When  V2Ccba = c_EasyWEB;
454100100212               TBEatb = 'A';
454200100212               clear  TBEftr;
454300100212               //_______________
454400100212               update  TNTBE000;
454500100212               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
454600091106
454700091126             // -?Vecchia serie parziale nel file (da annullare)?
454800091126             //  ?con serie completa a video?
454900120731             When  V2Cnrs = *zero    or
455000120731                  (V2Cncd = 1       and   V2Cnca  = *hival);
455100091106               TBEatb = 'A';
455200091106               clear  TBEftr;
455300091106               //_______________
455400091106               update  TNTBE000;
455500091106               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
455600091106
455700091126             // -?Aggiornamento serie parziale o annullamento vecchia?
455800091126             //  ?serie parziale SE variata TBEKE2: serie e/o numero?
455900091126             //  ?segnacollo iniziale?
456000120731             When  V2Cnrs > *zero   and
456100120731                  (V2Cncd > 1       or    V2Cnca <  *hival);
456200091106               d3CP = TBEuni;
456300091106
456400091106               IF  %editc(V2Cnrs : 'X') + %editc(V2Cncd : 'X') =
456500091106                   %subst(TBEke2 : 1 : %len(%editc(V2Cnrs : 'X'))) +
456600091106                   %subst(TBEke2 : %len(%editc(V2Cnrs : 'X')) + 1 :
456700091106                                   %len(%editc(V2Cncd : 'X')));
456800091106                 $Found_3CP = *on;
456900091106
457000091106                 If  TBEatb = 'A'   or   §3CPal <> V2Cnca;
457100091106
457200091126                 // -=»>?Aggiornamento vecchia serie parziale?
457300091106                   if  §3CPal <> V2Cnca;
457400091106                     §3CPal = V2Cnca;
457500091106                     TBEuni = d3CP;
457600091106                   endif;
457700091106                   if  TBEatb = 'A';
457800091106                     clear  TBEatb;
457900091106                     clear  TBEftr;
458000091106                   endif;
458100091106                   //_______________
458200091106                   update  TNTBE000;
458300091106                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
458400091106                 EndIf;
458500091106
458600091106               ELSE;
458700091106
458800091126                 // -=»>?Annullamento vecchia serie parziale con?
458900091126                 //     ?serie e/o numero segnacollo iniziale diversi?
459000091106                 if  TBEatb = *blank;
459100091106                   TBEatb = 'A';
459200091106                   clear  TBEftr;
459300091106                   //_______________
459400091106                   update  TNTBE000;
459500091106                   //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
459600091106                 endif;
459700091106
459800091106               ENDIF;
459900091106
460000091106           EndSl;
460100091106
460200091106           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
460300091106
460400091106         ENDDO;
460500091106
460600091106         // -?Nuova serie parziale a video?
460700091126         //  ?(inserimento  o  variazione serie e/o segnacollo iniziale)?
460800120731         IF  V2Cnrs > *zero   and
460900120731            (V2Cncd > 1       or    V2Cnca <  *hival)  and
461000091106             Not $Found_3CP;
461100091106
461200091126           // ->?Scrittura nuova serie?
461300091106           clear  TNTBE000;
461400091106           TBEapl = X3CPapl;
461500091106           TBEcod = %subst( X3CPke1 : %len(X3CPke1) - 3 + 1 : 3 );
461600091106           TBEke1 = V1Ccli;
461700091106           TBEke2 = %editc(V2Cnrs : 'X') + %editc(V2Cncd : 'X');
461800091106           clear  d3CP;
461900091106           §3CPal = V2Cnca;
462000091106           TBEuni = d3CP;
462100091106           TBEftt = 'S';
462200091106           if  w_SisInf = 1;
462300091106             TBEftr = 'T';
462400091106           else;
462500091106             TBEftr = 'R';
462600091106           endif;
462700091106           TBEdtr = wDate;
462800091106           //______________
462900091106           write  TNTBE000;
463000091106           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
463100091106
463200091106         ENDIF;
463300091105
463400091105       ENDSR;
463500091105
463600091105       //--------------------------------------------------------------
463700091105       //?Annullamento dati in tab. "3CP"                              ?
463800091106       //?(estensione per serie parziale)                              ?
463900091105       //--------------------------------------------------------------
464000091105       BEGSR  sr_Annull3CP;
464100091105
464200091106         // -?Annullamento serie parziale?
464300091106         k_TBEcod = '3CP';
464400091106         k_TBEke1 = V1Ccli;
464500091106         setll  %kds( k05tntbe01 : 2 )  TNTBE000;
464600091106         reade  %kds( k05tntbe01 : 2 )  TNTBE000;
464700091106
464800091106         DoW  Not %eof(TNTBE01L);
464900091106
465000091106           if  TBEatb <> 'A';
465100091106             TBEatb = 'A';
465200091106             clear  TBEftr;
465300091106             //_______________
465400091106             update  TNTBE000;
465500091106             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
465600091106           endif;
465700091106
465800091106           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
465900091106
466000091106         EndDo;
466100091105
466200091105       ENDSR;
466300100215
466400100215       //--------------------------------------------------------------
466500100215       //?Ripristino dati in tab. "3EW"                                ?
466600100215       //?(dati assegnati alla postazione "EasyWEB")                   ?
466700100215       //--------------------------------------------------------------
466800120207       BEGSR  sr_Ripris3EW;
466900120207
467000120207         // -?Ripristino dati assegnati alla postazione "EasyWEB"?
467100120207         //k_TBEcod = '3EW';
467200120207         k_TBEcod = %subst( X3EWke1 : %len(X3EWke1) - 3 + 1 : 3 );
467300120207         k_TBEke1 = TIVSSds.VSSksu;
467400120207         k_TBEke2 = TIVSSds.VSSsun;
467500120207         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
467600120816         if  %found(TNTBE01L)   and   TBEatb = *blank;
467700120816           d3EW_Temp = TBEuni;
467800120816         endif;
467900120207
468000120207         Select;
468100120207
468200120207           // -?Ripristino record?
468300120207           When  %found(TNTBE01L)   and   TBEatb = 'A';
468400120816             clear  d3EW.§3EWfaa;
468500120816             clear  d3EW.§3EWdaa;
468600121102             d3EW.§3EWuum = DUTute;
468700120816             TBEuni = d3EW;
468800120207             clear  TBEatb;
468900120207             clear  TBEftr;
469000120207             //_______________
469100120207             update  TNTBE000;
469200120207             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
469300120816
469400120816           // -?Ripristino applicativo?
469500120816           When  %found(TNTBE01L)   and   TBEatb = *blank  and
469600120816                 d3EW_Temp.§3EWfaa <> *blank;
469700120816             //clear  d3EW;   ?(la ds deve risultare già valorizzata)?
469800120816             //...            ?(la ds deve risultare già valorizzata)?
469900120816             clear  d3EW.§3EWfaa;
470000120816             clear  d3EW.§3EWdaa;
470100121102             d3EW.§3EWuum = DUTute;
470200120816             TBEuni = d3EW;
470300120816             TBEftt = 'S';
470400120816             if  w_SisInf = 1;
470500120816               TBEftr = 'T';
470600120816             else;
470700120816               TBEftr = 'R';
470800120816             endif;
470900120816             TBEdtr = wDate;
471000120816             //_______________
471100120816             update  TNTBE000;
471200120816             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
471300120207
471400120207           // -?Scrittura record (appena cancellato fisicamente)?
471500120207           When  Not %found(TNTBE01L);
471600120207             clear  TNTBE000;
471700120207             TBEapl = X3EWapl;
471800120207             TBEcod = k_TBEcod;
471900120207             TBEke1 = k_TBEke1;
472000120207             TBEke2 = k_TBEke2;
472100121102             //clear  d3EW;   ?(la ds deve risultare già valorizzata)?
472200121102             //...            ?(la ds deve risultare già valorizzata)?
472300120207             TBEuni = d3EW;
472400120207             TBEftt = 'S';
472500120207             if  w_SisInf = 1;
472600120207               TBEftr = 'T';
472700120207             else;
472800120207               TBEftr = 'R';
472900120207             endif;
473000120207             TBEdtr = wDate;
473100120207             //______________
473200120207             write  TNTBE000;
473300120207             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
473400120207
473500120207         EndSl;
473600120207
473700120207       ENDSR;
473800100322
473900100322       //--------------------------------------------------------------
474000100402       //?Aggiornamento dati segnacolli in tab. "3EW"                  ?
474100100322       //?(dati assegnati alla postazione "EasyWEB")                   ?
474200100322       //--------------------------------------------------------------
474300100517       //BEGSR  sr_Update3EW;
474400100401
474500100402         // -?Aggiornamento dati assegnati alla postazione "EasyWeb"?
474600100402         //  ?(segnacolli)?
474700100402         //  ?N.B. - Il flag §3EWUPD verrà aggiornato DOPO, con la?
474800100402         //         ?procedura UB§3EWUPD.?
474900100517       //k_TBEcod = '3EW';
475000100517       //k_TBEke1 = TIVSSds.VSSksu;
475100100517       //k_TBEke2 = TIVSSds.VSSsun;
475200100517       //chain  %kds( k05tntbe01 : 3 )  TNTBE000;
475300100517       //
475400100517       //if  %found(TNTBE01L);
475500100517       //
475600100517       //  d3EW    = TBEuni;
475700100517       //  if  V2Dfls > *zero;
475800100517       //    d3EW.§3EWfls = %int(V2Dfls);
475900100517       //  else;
476000100517       //    clear  d3EW.§3EWfls;
476100100517       //  endif;
476200100517       //  d3EW.§3EWnrs = V2Cnrs;
476300100517       //  d3EW.§3EWnsi = V2Cncd;
476400100517       //  d3EW.§3EWnsf = V2Cnca;
476500100517       //  d3EW.§3EWctm = V2Cctm;
476600100517       //  TBEuni = d3EW;
476700100517       //  TBEapl = X3EWapl;
476800100517       //  TBEftt = 'S';
476900100517       //  clear  TBEflt;
477000100517       //  if  w_SisInf = 1;
477100100517       //    TBEftr = 'T';
477200100517       //  else;
477300100517       //    TBEftr = 'R';
477400100517       //  endif;
477500100517       //  TBEdtr = wDate;
477600100517       //  //_______________
477700100517       //  update  TNTBE000;
477800100517       //  //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
477900100517       //
478000100517       //endif;
478100100401
478200100517       //ENDSR;
478300100215
478400100215       //--------------------------------------------------------------
478500100215       //?Annullamento dati in tab. "3EW"                              ?
478600100215       //?(dati assegnati alla postazione "EasyWEB")                   ?
478700100215       //--------------------------------------------------------------
478800101026       BEGSR  sr_Annull3EW;
478900100215
479000100215         // -?Annullamento dati assegnati alla postazione "EasyWEB"?
479100101026         k_TBEcod = '3EW';
479200120518         select;
479300120518           when  $Bypass_01  and  wCount_3EW = 1;
479400120726             //k_TBEke1 = TNTB79ds.i3EWksu;
479500120726             k_TBEke1 = TNTB79ds.o3EWksu;
479600120518             k_TBEke2 = TNTB79ds.o3EWsun;
479700120726           when  Old_§3Ccks = V2Ccks  and  TIVSSds.VSSksu > *zero
479800120726                                      and  TIVSSds.VSSsun > *zero;
479900120518             k_TBEke1 = TIVSSds.VSSksu;
480000120518             k_TBEke2 = TIVSSds.VSSsun;
480100120518           other;
480200120518             k_TBEke1 = TIVSSds_Orig.VSSksu;
480300120518             k_TBEke2 = TIVSSds_Orig.VSSsun;
480400120518         endsl;
480500101026         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
480600101026
480700101026         if  %found(TNTBE01L)   and   TBEatb <> 'A';
480800101026
480900121102           d3EW.§3EWuum = DUTute;
481000121102           TBEuni = d3EW;
481100101026           TBEatb = 'A';
481200101026           clear  TBEftr;
481300101026           //_______________
481400101026           update  TNTBE000;
481500101026           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
481600101026
481700101026         endif;
481800100215
481900101026       ENDSR;
482000120814
482100120814       //--------------------------------------------------------------
482200120814       //?Annullamento Applicativo EasySped-WEB in tab. "3EW"          ?
482300120814       //?(dati assegnati alla postazione "EasyWEB")                   ?
482400120814       //--------------------------------------------------------------
482500120814       BEGSR  sr_AnnApp3EW;
482600120814
482700120814         // -?Annullamento "applicativo" dei dati assegnati alla?
482800120814         //  ?postazione "EasyWEB"?
482900120814         k_TBEcod = '3EW';
483000120814         select;
483100120816           when  wCount_3EW = 1  and
483200120816                 ( $Bypass_01    or  (TNTB79ds.o3EWksu > *zero  and
483300120816                                      TNTB79ds.o3EWsun > *zero) );
483400120814             k_TBEke1 = TNTB79ds.o3EWksu;
483500120814             k_TBEke2 = TNTB79ds.o3EWsun;
483600120814           when  Old_§3Ccks = V2Ccks  and  TIVSSds.VSSksu > *zero
483700120814                                      and  TIVSSds.VSSsun > *zero;
483800120814             k_TBEke1 = TIVSSds.VSSksu;
483900120814             k_TBEke2 = TIVSSds.VSSsun;
484000120814           other;
484100120814             k_TBEke1 = TIVSSds_Orig.VSSksu;
484200120814             k_TBEke2 = TIVSSds_Orig.VSSsun;
484300120814         endsl;
484400120814         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
484500120814
484600120814         If  %found(TNTBE01L)   and   TBEatb <> 'A';
484700120814
484800120814           d3EW   = TBEuni;
484900120814
485000120814           if  d3EW.§3EWfaa = *blank;
485100120814
485200120814             d3EW.§3EWfaa = 'A';
485300120814             d3EW.§3EWdaa = %editc( wDate : 'X' );
485400121102             d3EW.§3EWuum = DUTute;
485500120814             TBEuni = d3EW;
485600120814             TBEapl = X3EWapl;
485700120814             TBEftt = 'S';
485800120814             clear  TBEflt;
485900120814             if  w_SisInf = 1;
486000120814               TBEftr = 'T';
486100120814             else;
486200120814               TBEftr = 'R';
486300120814             endif;
486400120814             TBEdtr = wDate;
486500120814             //_______________
486600120814             update  TNTBE000;
486700120814             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
486800120814
486900120814           endif;
487000120814
487100120814         EndIf;
487200120814
487300120814       ENDSR;
487400130604
487500130604       //--------------------------------------------------------------
487600130604       //?Invio e-mail con errore rilevato in fase di aggiornamento    ?
487700130604       //?flag D3EW.§3EWUPD in tab. "3EW" (vedi UB§3EWUPD).            ?
487800130604       //--------------------------------------------------------------
487900130605       //BEGSR  sr_Send_eMail;
488000130605       //
488100130605       //  Qcmd = 'SNDDST type(*lmsg) +
488200130605       //          tointnet((''stefano.merighi@brt.it'')) +
488300130605       //          dstd(''Errore in aggiornamento flag §3EWUPD'') +
488400130605       //          longmsg(''Errore ' + %trim( %editc( wRtnEsito : '1') ) +
488500130605       //                  ' ricevuto dal *pgm ' + %trimr(SDSpgm) + ')';
488600130605       //
488700130605       //  exsr  sr_ExecCmd;
488800130605       //
488900130605       //ENDSR;
489000130604
489100130604       //--------------------------------------------------------------
489200130604       //?Esecuzione del comando (già impostato).                      ?
489300130604       //--------------------------------------------------------------
489400130605       //BEGSR  sr_ExecCmd;
489500130605       //
489600130605       //  clear Qcap0100;
489700130605       //  Qcabcsdh = *off;
489800130605       //  Qcapa    = *off;
489900130605       //  Qcacmdss = *off;
490000130605       //  Qcaerved = *allX'00';
490100130605       //
490200130605       //  clear Qusec;
490300130605       //  Qusbprv  = %size(Qusec);
490400130605       //
490500130605       //  ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
490600130605       //                    %size(Qcap0100) : 'CPOP0100' : *omit :
490700130605       //                    0 : 0 : Qusec);
490800130605       //
490900130605       //  //if  Qusei <> *blank;
491000130605       //  //  ...;
491100130605       //  //endif;
491200130605       //
491300130605       //ENDSR;
491400091105
491500091102      /end-free
491600091102
491700091102       //--------------------------------------------------------------
491800091102       //?Schiere a tempo di compilazione.                             ?
491900091102       //--------------------------------------------------------------
492000091105
492100140718** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
492200091127SETRAS  GAITRAGRU FILTRAGRU
492300091105AS888   GAITRAGRPSFILTRAGRPF
492400140718** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
492500091124Filiale inesistente                                                            1
492600091124Campo obbligatorio                                                             2
492700091124Ammessi SOLO caratteri numerici                                                3
492800091124Cliente NON presente in anagrafica                                             4
492900130508Tabella inesistente  e  Cliente annullato o bloccato                           5
493000091124Codice cliente UNIFICANTE obbligatorio                                         6
493100091124Codice cliente UNIFICANTE errato                                               7
493200091124Cliente UNIFICANTE non presente in tab. "3C"                                   8
493300091215Codice cliente NON modificabile perché unificante con figli:                   9
493400120305Richiesto il "Supporto cliente a BRT" del cliente UNIFICANTE:                 --
493500120305"Supporto Cliente a BRT" obbligatorio                                         11
493600120305"Supporto Cliente a BRT" errato                                               12
493700120305Cliente UNIFICANTE con "Supporto Cliente a BRT" NON "                         13
493800121112Cliente UNIFICANTE non abilitato a STRATEGI per "EasySped-WEB"                14
493900120305SERIE obbligatoria per cliente con supporto a BRT                             15
494000091125Codice Trattamento Merce errato                                               16
494100091125SERIE obbligatoria per cliente che si assegna il num. spedizione o segnacollo 17
494200091125Errore istruzione SQL-Contattare il CED                                       18
494300091125SERIE già assegnata parzialmente al cliente                                   19
494400160125SERIE già assegnata completamente in tab.3CL                                  20
494500091125SERIE già assegnata al cliente                                                21
494600120305"Supporto BRT a Cliente" indicabile SOLO sul cliente unificante               22
494700091127"Accorpamento bolle" diverso sul cliente UNIFICANTE:                          23
494800140826Lunghezza Riferimento obbligatoria SE richiesto un suo allineamento           24
494900091125Campo NON impostabile SE NON richiesto l'accorpamento bolle                   25
495000091125Numero SERIE già presente: premere F10 per richiederne uno nuovo              26
495100120113Non ci sono più numeri di SERIE attribuibili tra 1 e 98                       27
495200091125Effettuare una selezione                                                      28
495300091125Numero Colli inseribile alternativamente alla Serie ed ai relativi Segnacolli 29
495400091125Numero segnacolli "AL" obbligatorio                                           30
495500091125Range segnacolli errato (DAL > AL)                                            31
495600121112Occorre far scadere l'abilitaz. a STRATEGI per "EasySped-WEB" dell'unificante 32
495700091126Segnacolli NON disponibili per la serie indicata                              33
495800120305"Supporto BRT a Cliente" obbligatorio per UNIFICANTE con SERIE                34
495900091215Cliente NON annullabile perché unificante con figli:                          35
496000121112Cliente già abilitato ad EasyWEB in tab. "3EW"; premere "Invio" per forzare   36
496100121112Abilitazione "EasySped-WEB" scaduta per il cliente UNIFICANTE                 37
496200100216Cliente UNIFICANTE non presente in tab. "3EW"                                 38
496300100216Sostituiti dati evidenziati con quelli dell'UNIFICANTE (già in tab. "3EW")    39
496400100216Cliente UNIFICANTE presente più volte in tab. "3EW" - Premere F13 per elenco  40
496500120305"Supporto Cliente a BRT" ESWEB NON modificabile su cliente UNIFICANTE         41
496600101028Codice cliente STRATEGI obbligatorio                                          42
496700120113SERIE non gestibile                                                           43
496800121128SERIE/RANGE segnacolli NON più attribuibili al cliente se si cambia supporto  44
496900121112SERIE / Range segnacolli NON più attribuibili: già attribuiti ad EasySped-WEB 45
497000121128Range segnacolli ANOMALO:                                                     46
497100140718Il segnacollo iniziale DEVE essere multiplo di 5.000 (+ 1)                    47
497200160317Ambito e terminal partenza vanno valorizzati entrambi o vuoti entrambi        48
497300160317Terminal partenza deve essere filiale di 1° livello                           49
