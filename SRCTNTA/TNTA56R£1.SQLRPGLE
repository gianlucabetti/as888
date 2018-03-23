000100090601      *PARMS DFTACTGRP(*NO) ACTGRP(*caller) OPTION(*NOXREF)
000200090320      *PARMS CLOSQLCSR(*ENDMOD)
000300090320      *PARMS DYNUSRPRF(*OWNER)
000400100617
000500100617     /*PRM closqlcsr(*endmod) dynusrprf(*owner)
000600100617     /*PRM dbgview(*source) commit(*none)
000700130806     /*CMD ovrdbf file(TITAS41C) tofile(GAITRAGRPS/TITAS41C) +
000800130806     /*CMD        ovrscope(*calllvl)
000900130806     /*END dltovr file(TITAS41C) lvl(*)
001000100617     /*END
001100100617
001200080206      //--------------------------------------------------------------
001300091210      //?TNTA56R - ESITAZIONE OFFERTE COMMERCIALI
001400080206      //--------------------------------------------------------------
001500080206
001600090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700090601     h dftactgrp(*no) actgrp(*caller)
001800100617     h alwnull(*inputonly)
001900080206
002000080206      //---------------------------------------------------------------
002100080206      //?Dichiarazione file.
002200080206      //---------------------------------------------------------------
002300050704
002400090721      // - Archivio Trattative Commerciali
002500090721     fTIVIS05L  uf a e           k disk
002600100305      // - Archivio offerte in trattativa
002700110112     fTIVOF11L  uf   e           k disk
002800091207      // - Anagrafica Clienti
002900100804     fCNACO00F  If   e           k disk
003000091207      // - Anagrafica Clienti
003100100804     fCNIND00F  If   e           k disk
003200100429      // - Anagrafica Clienti
003300100429     fCNIND02L  if   e           k disk    rename(cnind000:cnind02)
003400100429     f                                     prefix(ww_)
003500091207      // - Anagrafica Clienti
003600100804     fCNCLP00F  If   e           k disk
003700091207      // - Anagrafica Clienti
003800100804     fFNCLS01L  If   e           k disk
003900100119      // - Anagrafica Provvisoria Trattative
004000100804     fTFACO00F  If   e           k disk    rename(cnaco000:tfaco000)
004100100119      // - Anagrafica Provvisoria Trattative
004200100804     fTFIND00F  If   e           k disk    rename(cnind000:tfind000)
004300100119      // - Anagrafica Provvisoria Trattative
004400100804     fTFCLP00F  If   e           k disk    rename(cnclp000:tfclp000)
004500100119      // - Anagrafica Provvisoria Trattative
004600100804     fTFCLS01L  If   e           k disk    rename(fncls000:tfcls000)
004700100119      // - Anagrafica Potenziale
004800100119     fTNCPO01L  uf   e           k disk
004900100120
005000100120     fTNCPO05L  if   e           k disk    rename(tncpo000:tncpo005)
005100100120     f                                     prefix(c_)
005200100120     fTNCPO06L  if   e           k disk    rename(tncpo000:tncpo006)
005300100120     f                                     prefix(c_)
005400080208      // - Organigramma filiale/aziendale
005500080206     fAZORG01L  if   e           k disk
005600080208      // - Tabelle
005700080206     fTABEL00F  if   e           k disk
005800130806      // - Anagrafica Commerciali
005900130806     fAZCMM01L  if   e           k disk
006000091123      // - Attività
006100100127     fTIATC04L  if   e           k disk
006200100127     fTIATC01L  uf   e           k disk    rename(tiatc000:tiatc001)
006300100111      // - Attività legami
006400100111     fTIATL01L  uf   e           k disk
006500091207      // - Testata Offerte
006600091207     fTNOFM01L  uf   e           k disk    rename(tntam000:tnofm000)
006700091207     fTNTAM01L  uf A e           k disk
006800100120     fTNTAM04L  if   e           k disk    rename(tntam000:tntam004)
006900091207      // - Dettagio Offerte
007000091207     fTIOFD01L  uf   e           k disk    rename(titad000:tiofd000)
007100091207     fTITAD04L  uf A e           k disk
007200091207      // - Testata tar.Particolari Offerte
007300091207     fTIOPT01L  uf   e           k disk    rename(titpt000:tiopt000)
007400091207     fTITPT01L  uf A e           k disk
007500091207      // - Dettaglio tar.Particolari Offerte
007600091207     fTIOPD01L  uf   e           k disk    rename(titpd000:tiopd000)
007700091207     fTITPD01L  uf A e           k disk
007800091207      // - Tar.Giacenza Offerte
007900091207     fTIOGC01L  uf   e           k disk    rename(titgc000:tiogc000)
008000091207     fTITGC01L  uf A e           k disk
008100100122      // - Variazioni tariffe
008200100122     fTITAV00F  o  a e             disk
008300100122
008400100122     fTivss02l  if   e           k disk
008500100119      // - Anagrafico Banche
008600100119     FCNABI00F  IF   E           K DISK
008700100119      // - Numeratore
008800100119     fFLNUF01L  uf a e           k DISK
008900100119      // - Note
009000100424     fTFNTC01L  uf a e           k disk
009100100108      // - Note
009200100108     fTICPN03L  uf   e           k disk
009300130319      // - Bolle di sede
009400130320     fTITAS41C  if   e           k disk    usropn extfile(wFLib)
009500090721      // - Video Gestione trattative commerciali
009600091207     fTNTA56D   cf   e             workstn
009700091209     f                                     sfile(TA56S01 : S01nrr)
009800100118     f                                     sfile(TA56S05 : S05nrr)
009900100728     f                                     sfile(TA56WS10 : S10nrr)
010000080208     f                                     indds(IndDspF)
010100080206     f                                     infds(InfDspF)
010200080206
010300080206      //---------------------------------------------------------------
010400090406      //?Definizione costanti.
010500080206      //---------------------------------------------------------------
010600050519
010700080207      // - Numero di record in ciascuna videata di subfile
010800090804     d c_SflPag1       c                   const(07)
010900090721     d c_SflPag2       c                   const(11)
011000080207
011100080207      // - Tasti funzionali a video
011200080207     d c_F01           c                   const(x'31')
011300080207     d c_F02           c                   const(x'32')
011400080207     d c_F03           c                   const(x'33')
011500090406     d c_F04           c                   const(x'34')
011600080207     d c_F05           c                   const(x'35')
011700080207     d c_F06           c                   const(x'36')
011800080207     d c_F07           c                   const(x'37')
011900080207     d c_F08           c                   const(x'38')
012000080207     d c_F09           c                   const(x'39')
012100080207     d c_F10           c                   const(x'3A')
012200090303     d c_F11           c                   const(x'3B')
012300080207     d c_F12           c                   const(x'3C')
012400080207     d c_F13           c                   const(x'B1')
012500080207     d c_F14           c                   const(x'B2')
012600080207     d c_F15           c                   const(x'B3')
012700080207     d c_F16           c                   const(x'B4')
012800080207     d c_F17           c                   const(x'B5')
012900080207     d c_F18           c                   const(x'B6')
013000080207     d c_F19           c                   const(x'B7')
013100080207     d c_F20           c                   const(x'B8')
013200080207     d c_F21           c                   const(x'B9')
013300080207     d c_F22           c                   const(x'BA')
013400080207     d c_F23           c                   const(x'BB')
013500080207     d c_F24           c                   const(x'BC')
013600080207     d c_Enter         c                   const(x'F1')
013700080207     d c_RollDown      c                   const(x'F4')
013800080207     d c_RollUp        c                   const(x'F5')
013900080214
014000080214      // - Attributi di visualizzazione
014100080215      //  -  DspAtr() - Normale
014200080214     d C_dspatr_Norm   c                   const(x'20')
014300080215      //  -  DspAtr(RI)
014400080214     d C_dspatr_RI     c                   const(x'21')
014500081021     d C_dspatr_HI     c                   const(x'22')
014600080215      //  -  DspAtr(ND)
014700080214     d C_dspatr_ND     c                   const(x'27')
014800080215      //  -  DspAtr(BL) / Color(Red)
014900080214     d C_dspatr_BL     c                   const(x'28')
015000090722
015100100817
015200100817     d Digits          c                   const('0123456789')
015300120712
015400120712     d wMss            c                   'Si sta bloccando la Convalida -
015500120712     d                                     Offerte: SI PREGA DI USCIRE -
015600120712     d                                     dal lavoro!'
015700120712
015800120712     d wMsw            c                   'La Convalida Offerte è -
015900120712     d                                     bloccata riprovare più tardi.'
016000100817
016100080206      //---------------------------------------------------------------
016200080206      //?Definizione schiere.
016300080206      //---------------------------------------------------------------
016400080206
016500080206      // - Messaggi di errore
016600100805     d $Msg            s             78    dim(35) ctdata perrcd(1)
016700080207      // - Filiali
016800080207     d C_Max_Fil       c                   const(350)
016900090723     d $Fil            s                   like(ORGfil)
017000080208     d                                     dim(C_Max_Fil)  inz
017100080207     d $Car            s                   like(ORGcar)
017200080207     d                                     dim(C_Max_Fil)  inz
017300090811
017400081020      *
017500090813      // Schiera tipo Tariffa e relativa descrizione
017600090813     d ctr             s              1    dim(30)
017700090813     d dtr             s             10    dim(30)
017800100120     d
017900110112
018000100122      // Schiera tipo Nota
018100100122     d tno             s              2    dim(50)
018200100122     d tdup            s              1    dim(50)
018300100122     d ttdp            s              1    dim(50)
018400100122     d
018500100120      // Schiera Tariffa particoleri non + utilizzabili
018600100120     d TParEli         s              2    dim(20)
018700100120     d TParDat         s              8  0 dim(20)
018800100120     d
018900100120
019000080206      //---------------------------------------------------------------
019100080206      //?Definizione aree dati.
019200080206      //---------------------------------------------------------------
019300080206
019400080206      // - Dati utente
019500080206     d §AzUte        e ds                  extname(AZUTE00F)
019600080206     d                                     dtaara
019700080206     d §DatiUte      e ds                  extname(dDatiUte)
019800080206     d                                     dtaara
019900080206
020000080206      //---------------------------------------------------------------
020100080206      //?Definizione strutture dati.
020200080206      //---------------------------------------------------------------
020300080206
020400080206      // - Status
020500080206     d Psds           sds
020600100122     d   SDSpgm          *proc
020700100122     d   SDSjob              244    253
020800100122     d   SDSuse              254    263
020900080206
021000080206      // - InfDS
021100080206     d InfDspF         ds
021200080207     d  dsp_aid              369    369a
021300130806     d**sfl_rrn              376    377i 0
021400130806     d**min_nrr              378    379i 0
021500130806     d**num_rcds             380    381i 0
021600080206
021700080206      // - Indicatori su DspF
021800080208     d IndDspF         ds
021900100118        // - Indicatori di gestione del subfile 1
022000091207     d  SflDsp_N                      1n   overlay(IndDspF : 20)
022100091207     d  SflDspCtl_N                   1n   overlay(IndDspF : 21)
022200091207     d  SflNxtChg                     1n   overlay(IndDspF : 22)
022300091209     d  SflEnd                        1n   overlay(IndDspF : 23)
022400100118        // - Indicatori di gestione del subfile 5
022500100118     d  SflDsp5_N                     1n   overlay(IndDspF : 30)
022600100118     d  SflDspCtl5_N                  1n   overlay(IndDspF : 31)
022700100118     d  SflNxtChg5                    1n   overlay(IndDspF : 32)
022800100118     d  SflEnd5                       1n   overlay(IndDspF : 33)
022900100728        // - Indicatori di gestione del subfile 10
023000100728     d  SflDsp10_N                    1n   overlay(IndDspF : 60)
023100100728     d  SflDspCtl10_N                 1n   overlay(IndDspF : 61)
023200100728     d  SflNxtChg10                   1n   overlay(IndDspF : 62)
023300100728     d  SflEnd10                      1n   overlay(IndDspF : 63)
023400090422        // - Indicatori di errore 1° videata
023500080206     d  errMessage                    1n   overlay(IndDspF : 28)
023600090529        // - Indicatori di errore subfile
023700091209     d  PosCurOpz                     1n   overlay(IndDspF : 50)
023800100805        // - Indicatori di errore subfile   10
023900100805     d  PosCurOpz10                   1n   overlay(IndDspF : 70)
024000090422        // - Indicatori di errore generico
024100080206     d  errGenerico                   1n   overlay(IndDspF : 99)
024200090721        // - Indicatori di visualizzazione
024300100424     d  Esitata                       1n   overlay(IndDspF : 10)
024400100424     d  Tipo_nuova                    1n   overlay(IndDspF : 11)
024500091120     d  Note10_tar                    1n   overlay(IndDspF : 12)
024600100114     d  accettata                     1n   overlay(IndDspF : 13)
024700100114     d  convalidata                   1n   overlay(IndDspF : 14)
024800100118     d  Ges_convalida                 1n   overlay(IndDspF : 15)
024900100201     d  Ana_provvis                   1n   overlay(IndDspF : 16)
025000100424     d  Disatt_f11                    1n   overlay(IndDspF : 17)
025100100122     d
025200100122     d  Protez_ksc                    1n   overlay(IndDspF : 51)
025300100119     d  Vis_pagamento                 1n   overlay(IndDspf : 52)
025400100119     d  Vis_codfisc                   1n   overlay(IndDspf : 53)
025500100120     d  Blink_Ivae                    1n   overlay(IndDspf : 54)
025600121128     d  Vis_comm                      1n   overlay(IndDspf : 74)
025700100114        // - Indicatori di errore window accettazione
025800100114     d  PosCurDpa                     1n   overlay(IndDspF : 41)
025900100114     d  PosCurNsa                     1n   overlay(IndDspF : 42)
026000100118     d  PosCurKsc                     1n   overlay(IndDspF : 43)
026100100118     d  PosCurDra                     1n   overlay(IndDspF : 44)
026200100118     d  PosCurIva                     1n   overlay(IndDspF : 45)
026300100118     d  PosCurCdf                     1n   overlay(IndDspF : 46)
026400100118     d  PosCurCdp                     1n   overlay(IndDspF : 47)
026500100118     d  PosCurAbi                     1n   overlay(IndDspF : 48)
026600100118     d  PosCurCab                     1n   overlay(IndDspF : 49)
026700100707     d  PosCurDstt                    1n   overlay(IndDspF : 71)
026800100707     d  PosCurDdto                    1n   overlay(IndDspF : 72)
026900100707     d  PosCurDsto                    1n   overlay(IndDspF : 73)
027000121129     d  PosCurKSC12                   1n   overlay(IndDspF : 75)
027100121129     d  PosCurCMM12                   1n   overlay(IndDspF : 76)
027200080206
027300090407     d WindDspF        ds                  inz
027400100708     d   WindDspFa             1     39    inz(*zero)
027500100708     d   WindDspFb            40     69    inz(*zero)
027600100708     d   WindDspFc            70     99    inz(*zero)
027700090319
027800100115     d wlbdat          ds                  inz
027900100115     d  g02dat                 1      8  0
028000100115     d  g02inv                 9     16  0
028100100115     d  g02err                17     17
028200100115     d  g02tgi                18     22  0
028300100115
028400091209      // - Parametri TNTA01R2
028500091209     d Ta01r2ds        ds                  inz
028600091209     d Ta01nrv                 2      8  0
028700091209     d ta01prg                 9     11  0
028800091209     d ta01ccc                12     15  0
028900091209     d ta01rgs                16     63
029000091209     d ta01ctr                64     66  0
029100091209     d ta01cli                67     73  0
029200100122     d ta01flg                74     74
029300100122      * ta01all = 'A' ---> rcd allocato
029400100122     d ta01all                95     95
029500100122     d ta01cpo               156    161P 0
029600100122     d* FLAG DI RITORNO
029700100122     d ta01rit               162    162
029800100122     d* PARIST = 'E' ---> SCATTO ISTAT INESISTENTE ERRORE
029900100122     d ta01ist               178    178
030000100122     d* PARTRAT = 'T' ---> Richiamo come trattativa
030100100122     d ta01trat              179    179
030200100122
030300100122      // - Richiamo pgm stampa testi lettere              - FNLV43R -
030400100122     d PARAM           DS
030500100122      // Numero visita e progressivo
030600100122     d  PARNRV                 3      9  0 INZ
030700100122      // Codice Cliente
030800100122     d  PARCLI                10     16  0
030900100122      // Codice cliente potenziale
031000100122     d  PARCPO                17     27  0 INZ
031100100122      // AMBIENTE STAMPA TESTI  ====>  "C" = CLIENTI
031200100122     d  PARAST                99     99    INZ('C')
031300100122      // COD TARIFFA E PROGRESSIVO PER STAMPA TARIFFA  -TNTA43R
031400100122     D  PA2CTR               100    102  0 INZ
031500100122     D  PARPRG               103    105  0 INZ
031600100122      // FLAG PROVENIENZA       ====>  "3" = CONVALIDA OFFERTE
031700100122     D  PARFLG               106    106    INZ('3')
031800100122      // RAGIONE SOCIALE
031900100122     D  PARRGS               107    152
032000091209
032100100122      * Richiamo Tnta82r abilitazione clienti ai servizi ON LINE
032200100122     d Parm82          ds
032300100122     d  parric                 1      1
032400100122     d  parksc                 2      8
032500100122
032600080206      // - Parametri ricevuti
032700080206     d KPJBA         e ds
032800091207     d TNTA56DS      e ds
032900080206
033000080206      // - Reperimento dati utente
033100080206     d TIBS34ds      e ds
033200080206     d dUte01        e ds
033300081020     d TRUL31DS      e ds
033400120712
033500120712       // -?Gestione rcd allocato?
033600120712     d TRUL82ds      e ds
033700160803
033800160803       // -?Storicizzazione variazioni
033900160803     d TRMK30DS      e ds
034000160803     d TNCPO_30      e ds                  extname(TNCPO00F) inz
034100160803     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
034200160803     d TICPI_30      e ds                  extname(TICPI00F) inz
034300100426
034400100111      // - Tabella CCO = Causale contatto
034500100111     d dcco          e ds                  inz
034600130806
034700130806      // - Tabella LAT = Autorizzazione
034800130806     d dlat          e ds                  inz
034900130806
035000130806      // - Ds date statistiche
035100130806     d dSdf          e ds
035200130806
035300130806      // - Tabella TTR = Tipi trattative
035400130806     d dttr          e ds                  inz
035500100111
035600100429      // - Tabella FA  = Condizioni di pagamento
035700100429     d dsfa          e ds                  inz
035800100429
035900100429      // - Tabella 4W  = Stato del credito
036000100429     d ds4w          e ds                  inz
036100100312
036200100122      // - Tabella 1T = Tipi note
036300100122     d ds1t          e ds                  inz
036400100121
036500100119      // - Ds CPORST Potenziale
036600100119     d dcpo01        e ds                  inz
036700090724
036800100617      // - Gestione Contatti clienti/potenziali
036900100201     d TNTA12DS      e ds                  inz
037000100201
037100090810      // - Gestione anagrafica clienti
037200090810     d tnta60ds      e ds                  inz
037300090317
037400150223
037500150223      // - Inglobamneto tariffe particolari e controlli
037600150223     d Tnta25ds      e ds
037700150223     d Tnta25tad     e ds                  inz
037800150223     d Tnta25tgc     e ds                  inz
037900150223     d Tnta25tpd     e ds                  inz
038000150223     d Tnta25tpt     e ds                  inz
038100150223
038200100122      // - Convalida dati tipo
038300100122     d tnta33ds      e ds                  inz
038400100122
038500100617      // - Gestione Note tariffe/offerte (10)
038600090813     d TNTA28DS      e ds                  inz
038700100617
038800100617      // - Gestione/interrogazione Informazioni trattativa
038900100617     d TNTA41ds      e ds                  inz
039000090814
039100100209      // - Annulla dati tipo
039200100209     d TNTA47ds      e ds
039300100420
039400100505      // - Creazione anagrafica in convalida
039500100505     d TNTA55ds      e ds
039600100505
039700100505      // -  Controllo autorizzazione
039800100121     d TNTAA1DS      e ds                  inz
039900100114
040000100209      // - Gestione nuove attività
040100100209     d TRMK17ds      e ds                  inz
040200100209
040300100209      // -  Gestione Note clienti/potenziali
040400100209     d TRMK20ds      e ds                  inz
040500100209
040600100209      // - Interrogazione info commerciali
040700100209     d TRMK50ds      e ds                  inz
040800100209
040900090814      // -  Controllo partita IVA
041000151015     d XCFIVA1DS     e ds                  inz
041100100420
041200100420      // - Calcolo e Controllo validità delle date
041300100420     d Xgiolavds     e ds                  inz
041400090813
041500091209      // - Ricerca/Controllo tabelle
041600091209     d TIBS02ds      e ds                  inz
041700091209     d   T02Mod      e                     inz('C')
041800100122
041900100209      // - Clienti unificanti
042000100209     d TIBS10ds      e ds
042100100209
042200100119       // - Reperimento dati anagrafici
042300100119     d TIBS69ds      e ds
042400100121     d DS_cnaco      e ds                  inz extname(CNACO00F) prefix(w_)
042500100121     d DS_cnind      e ds                  inz extname(CNIND00F) prefix(w_)
042600100121     d DS_cnclp      e ds                  inz extname(CNCLP00F) prefix(w_)
042700100121     d DS_fncls      e ds                  inz extname(FNCLS00F) prefix(w_)
042800100425
042900080206      //---------------------------------------------------------------
043000080206      //?Definizione variabili globali.
043100080206      //---------------------------------------------------------------
043200080206
043300080206      // - Flags booleani
043400080208     d $Fine           s               n   inz(*off)
043500100204     d $Fine2          s               n   inz(*off)
043600100204     d $Fine4          s               n   inz(*off)
043700100204     d $Fine5          s               n   inz(*off)
043800100624     d $Fine7          s               n   inz(*off)
043900100625     d $Fine8          s               n   inz(*off)
044000100728     d $Fine9          s               n   inz(*off)
044100100728     d $Fine10         s               n   inz(*off)
044200100817     d $Fine11         s               n   inz(*off)
044300080208     d $InzS01         s               n   inz(*on)
044400100113     d $Inzw03         s               n   inz(*off)
044500100118     d $InzS05         s               n   inz(*off)
044600080206     d $ErrGrave       s               n   inz(*off)
044700080207     d $EoF            s               n   inz(*off)
044800080208     d $RecOK          s               n   inz(*off)
044900100108     d $Offerta        s               n   inz(*off)
045000100115     d $Esitata        s               n   inz(*off)
045100100119     d Wtibs69         s               n   inz(*off)
045200100122     d Wta33           s               n   inz(*off)
045300100122     d Wpsw            s               n   inz(*off)
045400100120     d wforzaiva       s               n   inz(*off)
045500100429     d wforzacon       s               n   inz(*off)
045600100122     d Nuovo_cli       s               n   inz(*off)
045700100426     d Mai_codific     s               n   inz(*off)
045800110112     d TTR_coninfot    s               n   inz(*off)
045900110112     d TTR_accettaz    s               n   inz(*off)
046000120125     d w_ChiudiT       s               n   inz(*off)
046100121128     d wFine12         s               n   inz(*off)
046200121128     d wInzW12         s               n   inz(*off)
046300090609
046400080206
046500080206      // - Indici di schiera
046600080208     d xx              s              3  0 inz
046700080206
046800080207      // - Campi associati al video
046900090722     d $Video          s              2    inz('S1')
047000080208     d S01nrr          s              4  0 inz
047100091209     d W_SflPag        s              3  0 inz
047200130806     d*// wPag            s              4  0 inz
047300130806     d*// wRig            s              3  0 inz
047400100118     d S05nrr          s              4  0 inz
047500100118     d W_SflPag5       s              3  0 inz
047600100728     d S10nrr          s              4  0 inz
047700100728     d W_SflPag10      s              3  0 inz
047800080206
047900090401      // - Campi di comodo data
048000090401     d  data_eur       s               d   Datfmt(*eur)
048100090401     d  data_iso       s               d   Datfmt(*iso)
048200090401     d  data_dmy       s               d   Datfmt(*dmy)
048300090811     d  data_ymd       s               d   datfmt(*ymd)
048400090401
048500080206      // - Campi di comodo
048600090406     d wAbi            s                   like(UTEaut)  inz
048700100122     d  datacor8       s              8  0
048800100122     d  datacor6       s              6  0
048900100119     d wagenzia        s             16
049000100119     d wbanca          s             20
049100100119     d wpos            s              2  0
049200100119     d wpos1           s              2  0
049300100119     d wpos2           s              2  0
049400100120     d wdata           s              6  0
049500100121     d wdata8          s              8  0
049600100119     d wiva            s                   like(indiva)
049700100119     d wcdf            s                   like(indcdf)
049800100120     d saviva          s                   like(indiva)
049900100120     d savcdf          s                   like(indcdf)
050000100120     d cod_forzcdf     s                   like(cpocdf) inz
050100100120     d cod_forziva     s                   like(cpopiv) inz
050200100120     d savrag          s                   like(cporag)
050300100120     d codice          s                   like(indiva) inz
050400100518     d sav_codice      s                   like(indiva) inz
050500100120     d savabi          s                   like(indabi)
050600100120     d savcab          s                   like(indcab)
050700100119     d wsta            s                   like(indsta)
050800100122     d wprg            s                   like(tamprg)
050900100128     d sav_ctr         s                   like(s01ctr)
051000100128     d sav_ver         s                   like(s01prg)
051100100425     d Keynrv          s                   like(vofnrv)
051200100425     d Keyctr          s                   like(vofctr)
051300100805     d Newksc          s                   like(vofksc)
051400100805     d Oldksc          s                   like(vofksc)
051500130806     d sav_rgf         s                   like(CMMuni)
051600100120     d wmod            s              1
051700100121     d numrec          s              4  0
051800100202     d numrec2         s              4  0
051900100805     d numrec10        s              4  0
052000100805     d scelta_rcd      s              4  0
052100100805     d fil_cmm         s              3  0
052200090323     d
052300130319     d wDataSP         s              8  0
052400150417     d savcdp          s                   like(C05cdp)
052500090722
052600090724      // campi di comodo date
052700100115     d wrkdpa          s              8  0
052800100120     d wrkdra          s              8  0
052900100707     d wrkddtt         s              8  0
053000100707     d wrkdstt         s              8  0
053100100707     d wrkddto         s              8  0
053200100707     d wrkdsto         s              8  0
053300130319
053400130319       // -?Campi libreria e file per TITAS41C
053500130319     d wFLib           s             21
053600130319     d wLib            s             10
053700090406
053800091216      // - Campi di comodo per esitazione trattativa
053900091216     D  N_off_trat     s              7  0
054000091216     D  N_off_hold     s              7  0
054100091216     D  N_off_conv     s              7  0
054200100625     D  N_off_acce     s              7  0
054300100126     D  N_off_daesi    s              7  0
054400100107
054500100107      // - Campi di comodo per esitazione trattativa
054600100107     D  causale        s                   like(atccad)
054700091216     D
054800100817
054900100817      // - Campi di comodo decodifica commerciale
055000100817     D  comcmm         s                   like(c01cmm)
055100100817     D  comcmmd        s                   like(c01cmmd)
055200100818      // - Campi di comodo per convalida commerciale differente da trattativa
055300100818     D  comcon         s                   like(c01cmm)
055400120712
055500120712      // - Campi di comodo x allocazione file anagrafici
055600120712     d cnaco_rrn       s             15  0 inz
055700120712     d cnind_rrn       s             15  0 inz
055800120712     d cnclp_rrn       s             15  0 inz
055900120712     d fncls_rrn       s             15  0 inz
056000120712     d tfaco_rrn       s             15  0 inz
056100120712     d tfind_rrn       s             15  0 inz
056200120712     d tfclp_rrn       s             15  0 inz
056300120712     d tfcls_rrn       s             15  0 inz
056400120712     d wRrn            s                   like(UL82§rrn) inz
056500120712     d wFil            s                   like(UL82§fil) inz
056600120712     d wWin            s                   like(UL82§win) inz('S')
056700120712     d wF7             s                   like(UL82§f7)  inz('S')
056800080208
056900090508      //---------------------------------------------------------------
057000090508      //?Definizione procedure usate.
057100090508      //---------------------------------------------------------------
057200080206
057300100122      * Stampa testi lettera
057400100122     d FNLV43R         pr                  extpgm('FNLV43R')
057500100122     d  kpjba                              likeds(kpjba)
057600120510
057700120510      * Gestione tariffe
057800120510     d TNTA01R2        pr                  extpgm('TNTA01R2')
057900120510     d  kpjba                              likeds(kpjba)
058000100122
058100150223      * Controllo tariffe particolari e fuel
058200150223     d TNTA25R         pr                  extpgm('TNTA25R')
058300150223     d  kpjba                              likeds(kpjba)
058400150223     d  tnta25ds                           likeds(Tnta25ds)
058500150223     d  Tnta25tad                          likeds(Tnta25tad)
058600150223     d  Tnta25tgc                          likeds(Tnta25tgc)
058700150223     d  Tnta25tpd                          likeds(Tnta25tpd)
058800150223     d  Tnta25tpt                          likeds(Tnta25tpt)
058900150223
059000100122      * Abilitazione clienti ai servizi on line
059100100122     d TNTA82R         pr                  extpgm('TNTA82R')
059200100122     d  kpjba                              likeds(kpjba)
059300100122
059400100209      * Annullo dati tipo
059500100209     d TNTA47R         pr                  extpgm('TNTA47R')
059600100209     d  kpjba                              likeds(kpjba)
059700100420
059800100505      * Creazione anagrafica clienti in convalida
059900100505     d TNTA55R         pr                  extpgm('TNTA55R')
060000100505     d  kpjba                              likeds(kpjba)
060100100505     d  tnta55ds                           likeds(Tnta55ds)
060200160218
060300160218       // -?Ctrl tipologia Tariffa tra tariffe/offerte del cliente?
060400160218     d TNTAT1ds      e ds                  inz
060500160218      /copy gaitrasrc/srcProtoPR,TNTAT1R
060600120712
060700120712       // -?Gestione rcd allocato?
060800120712     d TRUL82R         pr                  extpgm('TRUL82R')
060900120712     d  trul82ds                           likeds(trul82ds)
061000100505
061100100420      // - Aggiunge/Toglie gg/mm dalla data
061200100420     d Xgiolav         pr                  extpgm('XGIOLAV')
061300100420     d  xgiolavds                          likeds(xgiolavds)
061400160803
061500160803       // -?Storicizzazione Variazioni Potenziale
061600160803     d TRMK30R         pr                  extpgm('TRMK30R')
061700160803     d  trmk30ds                           likeds(trmk30ds)
061800160803     d  tncpo_30                           likeds(tncpo_30)
061900160803     d  tncpo1_30                          likeds(tncpo1_30)
062000160803     d  ticpi_30                           likeds(ticpi_30)
062100100420
062200100420      //---------------------------------------------------------------
062300100420      //?prototipi
062400100420      //---------------------------------------------------------------
062500100420
062600080626      /copy gaitrasrc/srcprotopr,tibs02r
062700100122      /copy gaitrasrc/srcprotopr,tibs10r
062800080626      /copy gaitrasrc/srcprotopr,tibs34r
062900100119      /copy gaitrasrc/srcprotopr,tibs69r
063000100201      /copy gaitrasrc/srcprotopr,tnta12r
063100100114      /copy gaitrasrc/srcprotopr,tnta28r
063200100122      /copy gaitrasrc/srcprotopr,tnta33r
063300100617      /copy gaitrasrc/srcprotopr,tnta41r
063400100114      /copy gaitrasrc/srcprotopr,tnta60r
063500091022      /copy gaitrasrc/srcprotopr,trmk17c
063600100201      /copy gaitrasrc/srcprotopr,trmk20r
063700100201      /copy gaitrasrc/srcprotopr,trmk50r
063800130806      /copy gaitrasrc/srcprotoPI,TRMK43R_1
063900130806      /copy gaitrasrc/srcprotoPR,TRMK43R
064000081020      /copy gaitrasrc/srcprotopr,trul31r
064100100115      /copy gaitrasrc/srcprotopr,xsrda8
064200151015      /copy gaitrasrc/srcprotopr,xcfivar1
064300100119      /copy gaitrasrc/srcprotopr,tntaa1c
064400090508
064500080206      //---------------------------------------------------------------
064600080206      //?Definizione key-list.
064700080206      //---------------------------------------------------------------
064800080206
064900130806       // -?File AZCMM01L?
065000130806     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
065100130806     d                                     prefix(k_)
065200080206
065300080206      //---------------------------------------------------------------
065400080206      //?Riepilogo indicatori.
065500080206      //---------------------------------------------------------------
065600100201      // - Indicatori di visualizzazione
065700100201      // 12    : note 10 legate alle tariffe
065800100201      // 16    : F7 Anagrafica provvisoria
065900100201      // 51    : Proteggo il codice cliente
066000100201      // 52    : Visualizza i dati relativi al codice pagamento
066100100201      // 53    : Visualizza i dati relativi al codice fiscale
066200100201      // 54    : Lampeggia la parte dell'iva estera
066300090518      // - Comuni
066400080207      // 28    : Emissione messaggio di errore a video
066500091209      // - C01/S01
066600090722      // 20    : Condiziona SFLDSP    (*not)
066700090722      // 21    : Condiziona SFLDSPCTL (*not)
066800090722      // 20+21 : Condiziona SFLCLR
066900090722      // 22    : Condiziona SFLNXTCHG in S02
067000090722      // 23    : Condiziona SFLEND
067100100201      // - C05/S05
067200100201      // 30    : Condiziona SFLDSP    (*not)
067300100201      // 31    : Condiziona SFLDSPCTL (*not)
067400100201      // 30+31 : Condiziona SFLCLR
067500100201      // 32    : Condiziona SFLNXTCHG in S02
067600100201      // 33    : Condiziona SFLEND
067700100201
067800100201      // - posizionamento cursore
067900100201      // 41    : Errore: Posizione Data presunto affidamento
068000100201      // 42    : Errore: Posizione Spedizioni presunto affidamento
068100100201      // 43    : Errore: Posizione Codice cliente
068200100201      // 44    : Errore: Posizione Data reale affidamento
068300100201      // 45    : Errore: Posizione Iva
068400100201      // 46    : Errore: Posizione Codice fiscale
068500100201      // 47    : Errore: Posizione Codice pagamento
068600100201      // 48    : Errore: Posizione Codice ABI
068700100201      // 49    : Errore: Posizione Codice CAB
068800100201      // 50    : Errore: Posizione cursore opzioni subfile 1
068900080207      // - Comuni
069000080207      // 99    : Generico di Errore
069100080206      //---------------------------------------------------------------
069200080206
069300080206      //---------------------------------------------------------------
069400080206      //?M A I N - L I N E
069500080206      //---------------------------------------------------------------
069600080206
069700080206     c     *Entry        plist
069800080206     c                   parm                    KPJBA
069900091209     c                   parm                    tnta56ds
070000080206
070100080206      /free
070200080206
070300080206       // Operazioni iniziali
070400080206       exsr RoutInz;
070500090601
070600080206       // Gestione video
070700080206       DOW $Fine = *off;
070800080206         select;
070900100113       // Gestione video SELEZIONE
071000080206           when $Video = 'S1';
071100080206             exsr GesS01;
071200100113       // Gestione video conferma chiusura
071300100113           when $Video = 'W3';
071400100113             exsr GesW03;
071500100114       // Gestione video accettazione offerta
071600100114           when $Video = 'W4';
071700100114             exsr GesW04;
071800100118       // Gestione video convalida offerte
071900100118           when $Video = 'S5';
072000100118             exsr GesS05;
072100090428           other;
072200100204             $Fine = *on;
072300080206         endsl;
072400080206       ENDDO;
072500080206
072600080206       // Operazioni finali
072700080206       exsr RoutEnd;
072800080206
072900090722       //--------------------------------------------------------------
073000090722       //?Gestione videata S01
073100090722       //--------------------------------------------------------------
073200090722       BEGSR GesS01;
073300090722
073400090722         // Inizializzazione videata
073500090722         if  $InzS01 = *on;
073600090722            exsr InzS01;
073700090722            $InzS01  = *off;
073800090722         endif;
073900090722
074000090722         // Posizionamento cursore
074100090722         if  C01csr  > *zero;
074200090722           C01rcd = C01csr;
074300090722         endif;
074400090722
074500090722         // Emissione Testata e Piede con tasti funzionali abilitati
074600091209           write  TA56T01;
074700091209           write  TA56P01;
074800090722
074900090722
075000090722         // Emissione videata
075100091209         exfmt  TA56C01;
075200090722         reset errMessage;
075300090722         reset errGenerico;
075400090722         clear V1Dmsg;
075500090722
075600090722         SELECT;
075700090804
075800100201         // - F08=Note tariffa
075900100201           WHEN  dsp_aid = c_F08;
076000100201             exsr F08S01;
076100100201
076200100201         // - F02=Rubrica
076300100201           WHEN  dsp_aid = c_F02;
076400100201             exsr F02S01 ;
076500100201
076600100201         // - F7=Anagrafica
076700100201           WHEN  dsp_aid = c_F07;
076800100201             exsr F07S01;
076900100201
077000100201         // - F14=Informazioni Commerciali
077100100201           WHEN  dsp_aid = c_F14;
077200100201             exsr F14S01;
077300100617
077400100617         // - F15=Informazioni Trattaativa
077500100617           WHEN  dsp_aid = c_F15;
077600110112             If  TTR_coninfot = *off ;
077700110112                 errMessage  = *on;
077800110112                 errGenerico = *on;
077900110112                 V1Dmsg = $Msg(32);
078000110112                 leavesr;
078100110112             endif;
078200100617             exsr F15S01;
078300100201
078400100201         // - F18=Note
078500100201           WHEN  dsp_aid = c_F18;
078600100201             exsr F18S01 ;
078700100201
078800090804         // - F10=Inserisce nuove attività
078900090804           WHEN  dsp_aid = c_F10;
079000090804             exsr F10S01;
079100090722
079200090722         // - F12=Ritorno
079300090722           WHEN  dsp_aid = c_F12;
079400090722             exsr F12S01;
079500091123             if  errGenerico = *on;
079600091123               leavesr;
079700091123             endif;
079800090722
079900090722         // - Roll-Up
080000090722           WHEN  dsp_aid = c_RollUp;
080100090722
080200090722         // Invio
080300090722           OTHER;
080400090807         // leggo il subfile solo se ho dei records
080500090807             If s01nrr > *zeros ;
080600090807                exsr OpzS01;
080700100520                if  errGenerico = *on ;
080800100520                    leavesr;
080900100520                endif;
081000090807             endif;
081100100319         // verifico se devo richiedere la chiusura della trattativa
081200100625         // oppure la pulizia delle attività 30
081300100319         // Richiamo la routine che verifica ed esita la trattativa
081400100319             exsr Sr_EsiTrat ;
081500100625
081600100625       // Se non ho + OFFERTE da accettare o convalidare verifico se ho ancora
081700100625       // attività 30 aperte
081800100625             If N_off_acce = N_off_daesi or N_off_daesi = 0 ;
081900100625                exsr Sr_Att_30 ;
082000100625             endif ;
082100100625
082200100319       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
082300100319             If N_off_daesi= 0 ;
082400100319       // emetto la videata di richiesta di conferma
082500100319                $video = 'W3' ;
082600100319                $inzw03 = *on  ;
082700100319                leavesr ;
082800100319
082900100319             Endif ;
083000090811
083100090722         ENDSL;
083200090722
083300090722       ENDSR;
083400090722
083500100625       //--------------------------------------------------------------
083600100625       //?Verifico se ho delle attività 30 ancora da eseguire
083700100625       //--------------------------------------------------------------
083800100625       BEGSR sr_Att_30 ;
083900100625
084000100625       // verifico se esistono attività 30 ancora aperte per questa
084100100625       // visita
084200100625
084300100625         atccpo = ch1cpo ;
084400100625         atcksc = c01ksc ;
084500100625         atcnrv = c01nrv ;
084600100625         atcdco = 0 ;
084700100625         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
084800100625         reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
084900100625         dow not %eof(tiatc04l);
085000100625
085100100628       // se stessa attività che m ha portato qui non controllo
085200100628                  If atctat = ita56tat and atcatn = ita56atn and
085300100628                     atcatnp = ita56atnp ;
085400100628                  else ;
085500100628                     if atccad = '30' ;
085600100625       // se esiste almeno un'attività 30 richiedo a video se devo
085700100625       // cancellarla
085800100628                        exsr  Gesw08 ;
085900100628                        leavesr ;
086000100628                     endif ;
086100100625                  endif ;
086200100625             reade   (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
086300100625         enddo ;
086400100625
086500100625       ENDSR ;
086600100625
086700100201       //--------------------------------------------------------------
086800100201       //?Gestione tasto funzionale F14 da videata S01
086900100201       //?F14=Informazioni Commerciali
087000100201       //--------------------------------------------------------------
087100100201       BEGSR F14S01;
087200100201
087300100201         // Richiamo il pgm informazioni Commerciali
087400100201         clear     trmk50ds   ;
087500100201         i50cpo=ch1cpo ;
087600100201         i50mod='G'   ;
087700100201         i50rag=c01rag   ;
087800100201         i50ifotot = §cpoifotot ;
087900100201         i50pgm='TNTA56R' ;
088000100924         i50obl = 'S' ;
088100120125
088200120510       //?Se siamo in chiusura trattativa è una chiusura con esito positivo le
088300120510       //?info vanno richiamate con:
088400120510       //?--> trattativa di tipo N la % affido a BRT va obbligatoriamente
088500120510       //?    aumentata
088600120510       //?--> trattativa di tipo I la % affido a BRT va obbligatoriamente
088700120510       //?    aumentata solo se = 0 altrimenti è facoltativo l'aumento
088800120510         IF  VISesi = 'P' and w_ChiudiT;
088900120510           IF  VIStpv = 'N';
089000120510             I50atb = '+';
089100120510           ENDIF;
089200120510           IF  VIStpv = 'I';
089300120510             I50atb = '%';
089400120510           ENDIF;
089500120510         ENDIF;
089600100201
089700100201         TRMK50R  (kpjba:trmk50ds) ;
089800100201
089900100201       ENDSR;
090000100617
090100100617       //--------------------------------------------------------------
090200100617       //?Gestione tasto funzionale F15 da videata S01
090300100617       //?F15=Informazioni trattativa
090400100617       //--------------------------------------------------------------
090500100617       BEGSR F15S01;
090600100617
090700100617         // Richiamo il pgm informazioni Trattativa
090800100617         clear  TNTA41ds;
090900100617         I41nrv    = C01nrv;
091000100617         I41rag    = C01rag;
091100100617         I41ifotot = VISinfot;
091200120706         I41mod    = 'G';
091300100617         I41pgm    = 'TNTA56R';
091400101221         I41tpv    = VIStpv;
091500100617
091600100617         tnta41r (kpjba : TNTA41ds);
091700100617
091800100617         // Salvataggio campo delle infotrattative
091900101005         if O41f06 = 'S'and visdch = 0 ;
092000100630              visinfot = o41ifotot ;
092100100630              update tivis000 ;
092200100630       // rialloco il record
092300100630              chain  ita56nrv tivis05l ;
092400100617         endif ;
092500120706         if O41f06 = 'S'and visdch > 0 ;
092600120706            chain  ita56nrv tivis05l ;
092700120706               IF  %found(TIVIS05L);
092800120706                   VISinfot = o41ifotot ;
092900120706                   update TIVIS000;
093000120706               ENDIF;
093100120706         endif ;
093200100617
093300100617       ENDSR;
093400100201
093500100201       //--------------------------------------------------------------
093600100201       //?Gestione tasto funzionale F02 da videata S01
093700100201       //?F02=Rubrica
093800100201       //--------------------------------------------------------------
093900100201       BEGSR F02S01;
094000100201
094100100201
094200100201         if c01ksc = 0 ;
094300100201            clear tnta12ds   ;
094400100201            ta12apl='T'   ;
094500100201            ta12nrv=c01nrv ;
094600100201            ta12rag=c01rag ;
094700100201            TNTA12R  (kpjba:TNTA12ds);
094800100201            If ta12err <> ' ';
094900100201               errMessage  = *on;
095000100201               errGenerico = *on;
095100100201               V1Dmsg = ta12msg ;
095200100201               leavesr;
095300100201            endif;
095400100201
095500100201         else ;
095600100201
095700100201            clear tnta12ds   ;
095800100201            ta12apl='C'   ;
095900100201            ta12ksc=c01ksc ;
096000100201            TNTA12R  (kpjba:TNTA12ds);
096100100201            If ta12err <> ' ';
096200100201               errMessage  = *on;
096300100201               errGenerico = *on;
096400100201               V1Dmsg = ta12msg ;
096500100201               leavesr;
096600100201            endif;
096700100201
096800100201         endif;
096900100201
097000100201
097100100201       ENDSR;
097200100201
097300090722       //--------------------------------------------------------------
097400090722       //?Inizializzazione videata S01
097500090722       //--------------------------------------------------------------
097600090722       BEGSR InzS01;
097700090722
097800090722       // Pulizia subfile
097900091209         exsr pulsfl ;
098000100617
098100091111
098200090722       // valorizzo i campi del video
098300090722            c01nrv = visnrv ;
098400090722            c01rag = visrag ;
098500090807            c01ksc = visksc ;
098600090722            data_eur = %date(visdat:*iso);
098700090722            c01dta = %dec(data_eur);
098800100121            c01cmm = viscmm ;
098900100121            comcmm = c01cmm  ;
099000090722            exsr Dec_com ;
099100130806            c01cmmd = CMMdes ;
099200100817        // salvo l'unificante
099300130806            sav_rgf = CMMuni ;
099400100817
099500090722            c01tpv = vistpv;
099600090722            c01esi = visesi;
099700100424            esitata = (visesi <> ' ' ) ;
099800100424            tipo_nuova = (vistpv = 'N' ) ;
099900090722
100000090722        // decodifico il tipo trattativa
100100100312             clear dTTR;
100200100312             clear TIBS02ds;
100300100312             T02mod = 'C';
100400100312             T02cod = 'TTR';
100500100312             T02ke1 = vistpv   ;
100600100312             T02sif = KNSIF;
100700100312             TNTBE_RicercaControllo  (kpjba : tibs02ds);
100800100312             IF  T02err = *blanks;
100900100312                 dTTR = t02uni ;
101000100312                 c01tpvd = §ttrdes;
101100110112         // imposto i flag per gestione delle info e offerte
101200110112                 TTR_coninfot = §ttrinfo = 'S' ;
101300110112                 TTR_accettaz = §ttracc = 'S' ;
101400100312             endif ;
101500091209
101600091209         // recupero i codice del potenziale da tfaco se esiste il record
101700091209         // altrimenti da cnaco con il codice cliente
101800091209             acokut = 1 ;
101900091209             acokcc = dutkci ;
102000091209             acoksc = c01nrv ;
102100100804             chain    (acokut:acokcc:acoksc) Tfaco00f ;
102200091209             If not %found(tfaco00f)  ;
102300091209                acoksc = c01ksc ;
102400100804                chain    (acokut:acokcc:acoksc) Cnaco00f ;
102500091209                if not %found(cnaco00f) ;
102600091209                   clear acolib;
102700091209                endif ;
102800100201             else ;
102900100201                ana_provvis = *on ;
103000091209             endif ;
103100091209             ch1cpo = acolib ;
103200100119         // recupero dati informativi
103300100804           chain(n) (acolib) Tncpo01l ;
103400100119           If %found(Tncpo01l) ;
103500100119              dcpo01 = Cporst ;
103600100119           else;
103700100119              clear dcpo01 ;
103800100119           endif ;
103900110210         // verifico se il potenziale è un codificato o un mai codificato/perso
104000110210           Mai_codific = *off;
104100110210
104200110210           If CpoFls = 'C' ;
104300110210              Mai_codific = *off;
104400110502           else ;
104500110502              Mai_codific = *on ;
104600110210           endif ;
104700091209
104800090722
104900091209        // recupero le offerte legate
105000110112           setll c01nrv Tivof11l ;
105100091210
105200091209       // Caricamento dei dati da presentare nel sfl
105300091209             $EoF = *off;
105400091209         exsr sr_ReadRec;
105500091209
105600091209         dow  $EoF   = *off ;
105700091209           exsr RollUpS01;
105800091209         enddo;
105900091209
106000091209       // Impaginazione subfile
106100091209       // -> forza l'impaginazione sul 1° rec. del subfile
106200091209         if S01nrr > *zero;
106300091209           C01rcd  = 1;
106400091209           C01csr  = 1;
106500091209         else;
106600091209           clear C01rcd;
106700091209         endif;
106800090722
106900090722       ENDSR;
107000091215
107100090722       //--------------------------------------------------------------
107200090722       //?Decodifica Commerciale
107300090722       //--------------------------------------------------------------
107400090722       BEGSR Dec_com ;
107500090722
107600090722        // decodifico il commerciale
107700130806               chain  (ComCmm)  AZCMM000;
107800130806               if  %found(AZCMM01L);
107900130806                 ComCmmd = CMMdes;
108000090722               endif;
108100090722
108200090722       ENDSR ;
108300090722
108400090722       //--------------------------------------------------------------
108500090722       //?Pulizia SFL01
108600090722       //--------------------------------------------------------------
108700091209       BEGSR Pulsfl ;
108800090722
108900090722       // Pulizia subfile
109000091209         SflDsp_N    = *on;
109100091209         SflDspCtl_N = *on;
109200091209         write  TA56C01;
109300091209         SflDspCtl_N = *off;
109400091209         SflEnd      = *off;
109500090722
109600090722
109700091209         clear W_SflPag ;
109800090722         clear C01rcd;
109900090722         clear C01csr;
110000090722         clear S01nrr;
110100090722         clear V1Dmsg;
110200090722         errMessage  = *off;
110300090722         errGenerico = *off;
110400090722         WindDspF    = IndDspF;
110500090722         reset WindDspFb;
110600090722         IndDspF     = WindDspF;
110700090722
110800090722       ENDSR ;
110900090722
111000090722       //--------------------------------------------------------------
111100090722       //?Caricamento di tutto il sfl
111200090722       //--------------------------------------------------------------
111300090722       BEGSR RollUpS01;
111400090722
111500090722         clear  s01nrr;
111600091209         SflNxtChg = *off;
111700090722
111800090722         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
111900090722         DOW  $EoF   = *off  and
112000090722              S01nrr < *hival;
112100090722
112200090722         // - Caricamento dati nel record del subfile
112300090722           exsr  RieS01;
112400090722           If $recok = *on ;
112500090722              S01nrr += 1;
112600091209              write TA56S01;
112700090722           endif ;
112800090722
112900090722         // - Lettura record successivo nell'archivio
113000090722           exsr sr_ReadRec;
113100090722
113200090722         ENDDO;
113300090722
113400090722         // Visualizzazione del SFL (se ci sono dati)
113500091209         SflDsp_N = (S01nrr <= *zeros);
113600090722
113700090722         // Attivazione (eventuale) del SFLEND
113800091209         SflEnd  =  *on;
113900090722
114000090722         // Posizionamento cursore al 1° rcd della pagina
114100090722         if  S01nrr > *zero;
114200091209         //  wPag   = S01nrr / C_SflPag ;
114300091209         //  wRig   = S01nrr - (C_SflPag  * wPag );
114400091209         //  C01rcd = wPag  * C_SflPag ;
114500091209         //  if  wRig  > *zeros;
114600090722         //    C01rcd = C01rcd + 1;
114700090722         //  else;
114800091209         //    C01rcd = C01rcd - C_SflPag  + 1;
114900090722         //  endif;
115000090722             c01rcd = 1;
115100090722         else;
115200090722           clear  C01rcd;
115300090722         endif;
115400090722
115500090722         C01csr = C01rcd;
115600090722
115700090722       ENDSR;
115800090722
115900090722       //--------------------------------------------------------------
116000090722       //?Lettura record successivo
116100090722       //--------------------------------------------------------------
116200090722       BEGSR sr_ReadRec;
116300090722
116400091209             $RecOK  = *off;
116500090722
116600110112             reade(n) visnrv tivof11l ;
116700090722
116800110112             if  %eof(tivof11l) ;
116900091209                 $Eof  = *on;
117000091209             else ;
117100100422                 If vofeso <> '*' ;
117200100422                    $RecOK  = *on;
117300100422                 endif;
117400091209             endif;
117500091209
117600090722       ENDSR;
117700090722
117800090722       //--------------------------------------------------------------
117900090722       //?Caricamento singolo record nel SubFile S01
118000090722       //--------------------------------------------------------------
118100090722       BEGSR RieS01;
118200090722
118300091209         clear  ta56S01;
118400091209         s01ctr = vofctr ;
118500091209         s01prg = vofprg;
118600091209         s01tpt = voftpt ;
118700091209
118800091209         // verifico se esiste veramente il record tariffa relativo
118900091209
119000091209             tamksc = vofnrv ;
119100091209             tamctr = vofctr ;
119200091209             tamprg = vofprg ;
119300091214             chain(n) (tamksc:tamctr:tamprg) tnofm01l ;
119400091209             If not %found(tnofm01l) ;
119500091209                   sh1conv = 'S' ;
119600100126             else ;
119700100126                   sh1conv = ' ' ;
119800091209             endif;
119900091209
120000100107         // valorizzo campo esito offerta
120100100312         select ;
120200100312         when vofeso = 'H' ;
120300110118              s01eso = 'Congelata  ';
120400100312         when vofeso = 'A' ;
120500110118              s01eso = 'Accettata  ';
120600100312         when vofeso = 'C' ;
120700110118              s01eso = 'Convalidata';
120800100312         Endsl ;
120900100107
121000100312         sh1eso = vofeso ;
121100100312
121200091209         // valorizzo descrizione tipo tariffa
121300091209            xx = %lookup ((%subst(%editc(tamctr:'X'):1:1)):ctr) ;
121400091209            if xx > 0 ;
121500091209               s01dtr = dtr(xx) ;
121600091209            endif ;
121700100114
121800100114            s01dcv = tamdcv ;
121900091209
122000091209         // cerco se ci sono delle note legate  all'offerta
122100091209
122200091209                 ntcapl = 'T' ;
122300100112         // se già convalidata non cerco le note
122400100112                 if sh1conv = ' ' ;
122500100111                    ntcnk1 = %editc(dutkci: 'X') + %editc(vofnrv:'X');
122600091209                 endif ;
122700091209
122800091209                 ntcnk2 = %editc(vofctr: 'X') ;
122900091209                 ntctnt = '10' ;
123000091209                 setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
123100091209                 If %equal(tfntc01l) ;
123200091209                    s01not = 'SI' ;
123300091209                    note10_tar = *on ;
123400100112                    sh1n12 = '1' ;
123500091209                 else ;
123600091209                    note10_tar = *off ;
123700100112                    sh1n12 = ' ' ;
123800091209         // provo con chiave generica con solo la trattativa
123900091209                    clear ntcnk2 ;
124000091209                    setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
124100091209                    If %equal(tfntc01l) ;
124200091209                       s01not = 'SI' ;
124300091209                    else ;
124400091209                       s01not = '  ' ;
124500091209                    endif ;
124600091215                 EndIf ;
124700100112         // Numero spedizioni e data presunto affidamento se non convalidata
124800100112              if Sh1conv = ' ' ;
124900091209
125000091210                 If vofdpa <> *zeros ;
125100110118                    data_eur= %date(vofdpa:*iso) ;
125200110118                    s01dpa = %dec(data_eur)  ;
125300091210                    data_eur= %date(vofdpa:*iso) ;
125400100312                    sh1dpa = %dec(data_eur) ;
125500091210                 endif ;
125600100112              else ;
125700100112                 If vofdra <> *zeros ;
125800110118                    data_eur= %date(vofdra:*iso) ;
125900110118                    s01dpa = %dec(data_eur)  ;
126000100312                    data_eur= %date(vofdra:*iso) ;
126100100312                    sh1dpa = %dec(data_eur) ;
126200100112                 endif ;
126300100112              endif ;
126400091209       ENDSR;
126500090722
126600090804       //--------------------------------------------------------------
126700090804       //?Gestione tasto funzionale F10 da videata S01
126800090804       //?F10=Inserisci nuove attività
126900090804       //--------------------------------------------------------------
127000090804       BEGSR F10S01;
127100090804
127200091022         // Richiamo il pgm trmk17c
127300091022             clear trmk17ds ;
127400100107             imk17fcmt = '1'  ;
127500100107             imk17cmt = 'S'  ;
127600100107         // devo recuperare i dati da registrare nell'attività futura
127700100107             imk17cpo = ch1cpo;
127800100107             imk17ksc = c01ksc ;
127900100107             imk17nrv = c01nrv ;
128000100121             imk17cmm = c01cmm ;
128100100303             imk17co3 = ita56com ;
128200090804
128300091022             trmk17c (kpjba : trmk17ds) ;
128400100107             $inzs01 = *on ;
128500090804
128600090804       ENDSR;
128700080206
128800100201       //--------------------------------------------------------------
128900100201       //?Gestione tasto funzionale F08
129000100201       //?F08=Note Tariffe
129100100201       //--------------------------------------------------------------
129200100201       BEGSR F08S01;
129300100201
129400100201         // Richiamo il pgm gestione note tariffe/offerte
129500100201         clear tnta28ds ;
129600100201
129700100201         ita28ela = 'M' ;
129800100201         ita28tip = 'T' ;
129900100201         ita28cod = c01nrv ;
130000100201         ita28rsc = c01rag ;
130100100201         clear kpjbu ;
130200100201         kpjbu = tnta28ds ;
130300100201         tnta28r (kpjba) ;
130400100203         $inzs01 = *on ;
130500100201
130600100201       ENDSR;
130700100201
130800100201       //--------------------------------------------------------------
130900100201       //?Gestione tasto funzionale F18 da videata S01
131000100201       //?F18=Note
131100100201       //--------------------------------------------------------------
131200100201       BEGSR F18S01;
131300100201
131400100201         // Richiamo il pgm gestione note TRMK20R
131500100201         clear trmk20ds ;
131600110511         imk20tla = 'L' ;
131700100201         imk20flm = 'V';
131800100201         imk20cpo = ch1cpo ;
131900100201         imk20ksc = c01ksc ;
132000100201         imk20nrv = c01nrv ;
132100100201         imk20rsc = c01rag ;
132200100201         Trmk20r ( kpjba : trmk20ds );
132300100201
132400100201       ENDSR;
132500100201
132600100201       //--------------------------------------------------------------
132700100201       //?Gestione  anagrafiche provvisorie
132800100201       //?
132900100201       //--------------------------------------------------------------
133000100201       BEGSR F07S01  ;
133100100201
133200100201          clear Tnta60ds ;
133300100201          ta60flg = 'T';
133400100201          ta60nrv = c01nrv ;
133500100201          ta60cmm = c01cmm ;
133600100201
133700100201          tnta60r (kpjba:tnta60ds) ;
133800100201
133900100201       // gestisco i dati di ritorno
134000100201
134100100201          if ta60err <> *blanks ;
134200100201             errMessage  = *on;
134300100201             errGenerico = *on;
134400100201             V1Dmsg = ta60msg;
134500100201             leavesr;
134600100201          EndIf ;
134700100201
134800100201       ENDSR;
134900100201
135000080207       //--------------------------------------------------------------
135100080207       //?Gestione tasto funzionale F12 da videata S01
135200080207       //?F12=Ritorno
135300080207       //--------------------------------------------------------------
135400080207       BEGSR F12S01;
135500080207
135600100115         // Ritorno al programma chiamante e rolbk se non esitato
135700100115         If $esitata = *off ;
135800100115            ota56f12 = 'S';
135900100204            $fine  = *on ;
136000100817         endif ;
136100100115
136200091123         // verifico se ci sono delle attività aperte a fronte di questa
136300100817         // trattativa MA SOLO se è aperta altrimenti errore ed ho esitato
136400091123
136500100817         If visdch = 0 and $esitata = *on ;
136600100127            atccpo = ch1cpo ;
136700100127            atcksc = c01ksc ;
136800100127            atcnrv = c01nrv ;
136900100127            atcdco = 0 ;
137000100127              setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
137100100127              If not %equal ;
137200100817                 // errMessage  = *on;
137300100817                 // errGenerico = *on;
137400100817                 // V1Dmsg = $Msg(03);
137500100817                 Exsr Gesw11 ;
137600100914                setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
137700101005                If  not %EQUAL ;
137800100914                   leavesr;
137900100914                endif ;
138000100127              endif;
138100091123
138200100127         endif ;
138300100115
138400100115         $fine  = *on ;
138500080207
138600080207       ENDSR;
138700090810
138800100817
138900100817       //--------------------------------------------------------------
139000100817       //?Gestione videata W11 Guida alla creazione NUOVE ATTIVITA'
139100100817       //--------------------------------------------------------------
139200100817       BEGSR GesW11;
139300100817
139400100817         $fine11 = *off ;
139500100817
139600100817         dow $Fine11 =  *off ;
139700100817         // Emissione Finestra
139800100817             exfmt  TA56W11;
139900100817
140000100817         SELECT;
140100100817
140200100817         // - F10=Creazione attività
140300100817           WHEN  dsp_aid = c_F10;
140400100817                exsr F10S01 ;
140500100817                $inzs01 = *on ;
140600100817                $video = 'S1' ;
140700100817                $fine11 = *on ;
140800100817
140900100817         ENDSL;
141000100817       ENDDO;
141100100817
141200100817       ENDSR;
141300100817
141400090916       //--------------------------------------------------------------
141500091209       //?Gestione opzioni subfile S01
141600090916       //--------------------------------------------------------------
141700091209       BEGSR OpzS01;
141800090916
141900100202         numrec2= 0 ;
142000100202         Dow 1 = 1 ;
142100100202             numrec2 += 1;
142200100202             chain numrec2 ta56s01 ;
142300100202             If not %found(Tnta56d) ;
142400100202                leave ;
142500100202             endif ;
142600090916
142700100202                SflNxtChg= *off;
142800100202                WindDspF  = IndDspF;
142900100202                reset WindDspFb;
143000100202                IndDspF   = WindDspF;
143100100202                C01rcd    = S01nrr;
143200090916
143300090916           SELECT;
143400090916
143500090916             // - Nessuna opzione
143600091209             WHEN  S01opz  = *blank;
143700090916
143800090916             // - Precedente segnalazione di errore
143900100312             // WHEN  S01opz  = 'E';                 TOLTA IN QUANTO E ELIMINA
144000100312             //   clear S01opz;
144100090916
144200091209             // - 2 = Visualizzazione tariffa/ Manutenzione offerta
144300091209             WHEN  S01opz  = '2' and sh1conv <> 'S';
144400090916
144500090917                  clear ta01r2ds ;
144600090917
144700091209                  ta01ctr = s01ctr ;
144800091209                  ta01prg = s01prg ;
144900090916                  exsr Sr_ta01r2  ;
145000090916
145100100312             // - E = Elimina offerta
145200100312             WHEN  S01opz  = 'E' and sh1conv <> 'S';
145300091209
145400100113             // emetto la videata di richiesta di conferma
145500100204                  exsr GesW02 ;
145600091209
145700110112             // - A = Accetta offerta solo se il tipo trattativa lo prevede
145800110112             WHEN  S01opz  = 'A' and sh1conv <> 'S' and TTR_accettaz = *off ;
145900100312               errMessage  = *on;
146000100312               errGenerico = *on;
146100100312               PosCurOpz   = *on;
146200100312               V1Dmsg = $Msg(21);
146300100312
146400091209             // - A = Accetta offerta
146500091209             WHEN  S01opz  = 'A' and sh1conv <> 'S';
146600091209
146700100128             // verifico se ci sono altre versioni della stessa offerta
146800100128             // che devono essere annullate o congelate prima di accettare
146900100128             // una versione
147000100128                  exsr Sr_ctrAcc ;
147100100114
147200100128                  If errmessage = *off;
147300100114             // emetto la videata di richiesta di conferma
147400100204                     exsr Gesw04 ;
147500100128                  endif ;
147600100114
147700091209             // - C = Convalida Offerta
147800091209             WHEN  S01opz  = 'C' and sh1conv <> 'S';
147900100312
148000160219               // -?SE si sta convalidando un'offerta per una trattativa?
148100160219               //  ?diversa da "N" = Nuova ...?
148200160219               //*If  VIStpv <> 'N';
148300160219               If  C01tpv <> 'N';
148400160219
148500160219                 // -?Verifica se il cliente ha già tariffe con lo?
148600160219                 //  ?stesso codice MA con Tipo e/o Network diverso?
148700160219                 clear  TNTAT1ds;
148800160219                 iTAT1ksc  = C01ksc;
148900160219                 iTAT1tipo = *blank;
149000160219                 iTAT1ctr  = S01ctr;
149100160219                 select;
149200160219                   when  S01tpt = 'D';
149300160219                     iTAT1ntw  = 'DPD';
149400160222                   when  S01tpt = 'E';
149500160219                     iTAT1ntw  = 'EEX';
149600160219                   when  S01tpt = 'F';
149700160219                     iTAT1ntw  = 'FED';
149800160219                   //other;
149900160219                   //  clear  iTAT1ntw;
150000160219                 endsl;
150100160224                 iTAT1pgm  = SDSpgm;
150200160224                 KPJBU     = TNTAT1ds;
150300160219                 tntaT1r ( kpjba );
150400160224                 TNTAT1ds  = KPJBU;
150500160219                 if  oTAT1err = *on;
150600160219                   ErrGenerico = *on;
150700160219                   ErrMessage  = *on;
150800160219                   PosCurOpz   = *on;
150900160224                   V1Dmsg  = oTAT1msg;
151000160219                 endif;
151100160219
151200160219               EndIf;
151300160219
151400100312             // se si tratta di tipo trattativa di tipo A prima controllo
151500100312             // come se fossi in accettazione
151600110114             // se il tipo trattativa non prevede accettazione
151700110114             // faccio i controlli come se la facessero
151800160222                  if TTR_Accettaz = *off  and  ErrMessage   = *off;
151900100312                     exsr Sr_CtrAcc ;
152000100312                  endif ;
152100100312
152200100312                  If errmessage = *off;
152300100118             // Verifico che non ci siano degli errori e verifico se ci sono
152400100118             // altre offerte da convalidare
152500100312                     exsr Sr_ConvOff  ;
152600100312                  endif ;
152700091209
152800091209             // - H = Congela Offerta
152900100422             WHEN  S01opz  = 'H' and sh1conv <> 'S' and sh1eso <> 'C' ;
153000091209
153100091209                  exsr Sr_CongeOff  ;
153200091209
153300090916             // - ? = Opzione NON valida
153400090916             OTHER;
153500090916               errMessage  = *on;
153600090916               errGenerico = *on;
153700091209               PosCurOpz   = *on;
153800091215               V1Dmsg = $Msg(02);
153900090916
154000090916           ENDSL;
154100090916
154200090916           // Aggiornamento sfl
154300090916           select;
154400090916             when  errMessage  = *on;
154500091209               SflNxtChg  = *on;
154600091209               C01csr    = C01rcd;
154700090916             when  errGenerico = *on;
154800091209               C01csr    = C01rcd;
154900100118               clear  S01opz;
155000090916             other;
155100091209               C01csr    = C01rcd;
155200091209               clear  S01opz;
155300090916           endsl;
155400090916
155500100201           If  sh1n12 = '1' ;
155600100201               note10_tar = *on ;
155700100204           else ;
155800100204               note10_tar = *off ;
155900100201           endif ;
156000100201
156100100204           ///////////update TA56S01;
156200090916
156300090916           if  errMessage = *on  or  errGenerico = *on;
156400090916             leave;
156500090916           endif;
156600090916
156700090916         ENDDO;
156800100204
156900100204           if  errMessage = *off and errGenerico = *off ;
157000100204             $inzs01 = *on ;
157100100204           endif;
157200090916
157300090916       ENDSR;
157400090916
157500090814       //--------------------------------------------------------------
157600090814       //?Richiamo del TNTA01R2
157700090814       //--------------------------------------------------------------
157800090814       BEGSR sr_ta01r2 ;
157900090814
158000090814                   ta01nrv = c01nrv ;
158100090814                   ta01flg = '1' ;
158200090814                   ta01ccc = dutkci ;
158300090814                   ta01cli = c01ksc ;
158400090814                   ta01cpo = ch1cpo ;
158500090916                   ta01rgs = c01rag ;
158600090917                   ta01trat = 'T' ;
158700090814                   clear kpjbu ;
158800090814                   kpjbu = ta01r2ds ;
158900090923
159000091209                       Tnta01r2 (kpjba ) ;
159100090923
159200090814       ENDSR ;
159300091209
159400091209       //--------------------------------------------------------------
159500091209       //?Annullamento Offerta
159600091209       //--------------------------------------------------------------
159700091209       BEGSR sr_AnnOff ;
159800091209
159900091215
160000091215       // Richiamo la routine che deleta l'offerta
160100091215             exsr Sr_DelOfm ;
160200100108
160300100108       // Richiamo la routine che deleta le note 10 offerta  se le ha
160400100108       // a livello di offerta
160500100112             If   s01not = 'SI' and sh1n12 = '1' ;
160600100425                  KeyNrv = c01nrv ;
160700100425                  KeyCtr = s01ctr ;
160800100108                  exsr Sr_DelNote10;
160900100108             endif ;
161000091215
161100100107       // Cancello attività e note attività legate all'offerta
161200100111             $offerta = *on ;
161300100107             exsr Sr_Delatc ;
161400100111             $offerta = *off;
161500091215
161600100422       // Cancello record su Tivof se data presentazione offerta maggiore
161700100422       // dell'ultima statistica altrimenti annullo logicamente
161800100712       // non faccio + il controllo della data ma tengo tutte le offerte
161900110112            chain (tamksc:tamctr:tamprg) Tivof11l ;
162000110112            If %found(Tivof11l) ;
162100100712       //      If %dec(%subst(%EDITC(vofdpo:'X'):1:6):6:0) <= §sdfstcu ;
162200100422                  vofeso = '*' ;
162300100422                  update tivof000 ;
162400100712        //     else ;
162500100712        //        delete tivof000;
162600100712        //     endif ;
162700100115               $Esitata = *on ;
162800100107            endif;
162900100107
163000091216       // Richiamo la routine che verifica ed esita la trattativa
163100091215             exsr Sr_EsiTrat ;
163200100107
163300100112       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
163400100126             If N_off_daesi= 0 ;
163500100113       // emetto la videata di richiesta di conferma
163600100113                $video = 'W3' ;
163700100113                $inzw03 = *on  ;
163800100113                leavesr ;
163900100107
164000100107             Endif ;
164100100107
164200091215
164300100111          $inzs01 = *on  ;
164400100115          $video  = 'S1' ;
164500100111
164600091209       ENDSR ;
164700091215
164800091215       //--------------------------------------------------------------
164900091215       //?Conferma Annullamento offerta
165000091215       //--------------------------------------------------------------
165100091215       BEGSR GesW02;
165200100312
165300100312       // imposto i campi della videata
165400100312         w02ctr = s01ctr ;
165500100312         w02prg = s01prg ;
165600100312         w02tpt = s01tpt ;
165700100312         w02dtr = s01dtr ;
165800100312         w02dcv = s01dcv ;
165900100426         clear  w02conf ;
166000100312
166100100204         Dow $fine2 = *off ;
166200100112
166300091215         // Emissione Finestra di conferma
166400091215         exfmt  TA56W02;
166500091215         reset errMessage;
166600091215         reset errGenerico;
166700091215         clear V1Dmsg;
166800091215
166900091215         SELECT;
167000091215
167100091215         // - F12=Ritorno
167200091215           WHEN  dsp_aid = c_F12;
167300100126
167400100126                clear s01opz ;
167500100201
167600100201                If  sh1n12 = '1' ;
167700100201                    note10_tar = *on ;
167800100204                else ;
167900100204                    note10_tar = *off ;
168000100201                endif ;
168100100201
168200100126                update ta56s01 ;
168300100204                leave ;
168400091215
168500091215         // - F06=Conferma
168600091215           WHEN  dsp_aid = c_F06  ;
168700091215
168800091215         // Verifico se confermo annullamento
168900100503                If W02Conf = 'S' ;
169000100113                   exsr Sr_AnnOff ;
169100091215                endif ;
169200100126
169300100126                clear s01opz ;
169400100201
169500100204                If  sh1n12 = '1' ;
169600100204                    note10_tar = *on ;
169700100204                else ;
169800100204                    note10_tar = *off ;
169900100204                endif ;
170000100201
170100100126                update ta56s01 ;
170200100204                leave ;
170300100126
170400091215         ENDSL;
170500100112
170600100204         ENDDO;
170700091215
170800091215       ENDSR;
170900091215
171000100112       //--------------------------------------------------------------
171100100112       //?Richiesta chiusura trattativa
171200100112       //--------------------------------------------------------------
171300100112       BEGSR GesW03;
171400100112
171500120125         w_ChiudiT = *off;
171600100112
171700100112         // Emissione Finestra di conferma
171800100112         exfmt  TA56W03;
171900100112         reset errMessage;
172000100112         reset errGenerico;
172100100113         reset $Inzw03 ;
172200100112         clear V1Dmsg;
172300100112
172400100112         SELECT;
172500100112
172600100112
172700100112         // - F06=Conferma
172800100112           WHEN  dsp_aid = c_F06  ;
172900120125
173000120125           w_ChiudiT = *on;
173100100112
173200100112         // Verifico se confermo chiusura trattativa
173300100503             If W03Conf = 'S' ;
173400100115                If visesi = '  ' ;
173500100114         // verifico se ce nè almeno una convalidata è positiva altrimenti
173600100114         // è negativa
173700100115                   If N_Off_conv > 0 ;
173800100114                         visesi = 'P ' ;
173900100115                   else ;
174000100319                         visesi = '27' ;
174100100115                   endif ;
174200100115                endif ;
174300100113                   data_iso = (%date()) ;
174400100113                   visdch = %dec(data_iso) ;
174500100113                   update tivis000 ;
174600100114       // propongo una nuova attività di richiamo con causale 27 se esito
174700100114       // negativo
174800100319                If visesi = '27' ;
174900100115                   causale = '27' ;
175000100113       // Richiamo la routine che crea nuove attività
175100100115                   exsr Sr_CreaAtt ;
175200100112                endif ;
175300100924
175400100924         // richiamo i  programmi info trattativa e commerciali
175500110112         // le info le richiedo se la trattaiva le prevede
175600110112                If TTR_coninfot = *ON ;
175700110112                   exsr f15s01 ;
175800110112                Endif ;
175900110112
176000100924                exsr f14s01 ;
176100100115
176200100520                $Fine = *on;
176300100520             endif ;
176400100520
176500100520         // Verifico se confermo chiusura trattativa
176600100520             If W03Conf = 'N' ;
176700100520                $Fine = *on;
176800100520             endif ;
176900100112
177000100112         ENDSL;
177100100112
177200100112       ENDSR;
177300100112
177400091215       //--------------------------------------------------------------
177500100209       //?Deleto  Offerta e i suoi dati tipo
177600091215       //--------------------------------------------------------------
177700091215       BEGSR sr_DelOfm ;
177800091215
177900100209
178000100209         Tamksc = c01nrv ;
178100100209         Tamctr = s01ctr ;
178200100209         Tamprg = s01prg ;
178300100209
178400100209
178500100209         // annullo fatio tipo legati all'offerta
178600100209         clear tnta47ds ;
178700100209         d47tup = '2' ;
178800100209         d47cto = 'O' ;
178900100209         d47dsf = 'S' ;
179000100209         d47ksc = tamksc ;
179100100209         d47ctr = tamctr ;
179200100209         d47prg = tamprg ;
179300100209         kpjbu = tnta47ds ;
179400100209         tnta47r (kpjba) ;
179500100209
179600100209         chain (tamksc:tamctr:tamprg) Tiogc01l ;
179700100209         If %found(Tiogc01l) ;
179800100209            delete tiogc000;
179900100209         endif;
180000100209
180100100209         setll (tamksc:tamctr:tamprg) Tiopd01l ;
180200100209         reade (tamksc:tamctr:tamprg) Tiopd01l ;
180300100209         Dow not %eof ;
180400100209             delete  tiopd000 ;
180500100209             reade (tamksc:tamctr:tamprg) Tiopd01l  ;
180600100209         enddo ;
180700100209
180800100209
180900100209         setll (tamksc:tamctr:tamprg) Tiopt01l ;
181000100209         reade (tamksc:tamctr:tamprg) Tiopt01l ;
181100100209         Dow not %eof ;
181200100209             delete  tiopt000 ;
181300100209             reade (tamksc:tamctr:tamprg) Tiopt01l  ;
181400100209         enddo ;
181500100209
181600100209
181700100209         setll (tamksc:tamctr:tamprg) Tiofd01l ;
181800100209         reade (tamksc:tamctr:tamprg) Tiofd01l ;
181900100209         Dow not %eof ;
182000100209             delete  tiofd000 ;
182100100209             reade (tamksc:tamctr:tamprg) Tiofd01l ;
182200100209         enddo ;
182300100209
182400100209         chain (tamksc:tamctr:tamprg) tnofm01l ;
182500100209         If %found(tnofm01l) ;
182600100209            delete tnofm000;
182700100209         endif;
182800091215
182900091215       ENDSR ;
183000100114
183100100114       //--------------------------------------------------------------
183200100114       //?Richiesta Accettazione offerta
183300100114       //--------------------------------------------------------------
183400100114       BEGSR GesW04;
183500100114
183600100115         // Inizializzazione videata
183700100115            exsr Inzw04;
183800100115
183900100204         // Gestione video
184000100204         DOW $Fine4 = *off;
184100100114         // Emissione Finestra di accettazione offerta
184200100204             exfmt  TA56W04;
184300100204             reset errMessage;
184400100204             reset errGenerico;
184500100204             clear w04msg;
184600100114
184700100114         SELECT;
184800100114
184900100114         // - F12=Ritorno
185000100114           WHEN  dsp_aid = c_F12;
185100100204             $Fine4 = *on ;
185200100114
185300100114         // - F06=Accetta
185400100114           WHEN  dsp_aid = c_F06  ;
185500100115             Exsr SR_ctrw04 ;
185600100115             if  errGenerico = *on;
185700100204                 iter ;
185800100115             endif;
185900100115
186000100115         // aggiorno i record per l'accettazione
186100100115             exsr Sr_AccOff ;
186200100126
186300100126                clear s01opz ;
186400100204                If  sh1n12 = '1' ;
186500100204                    note10_tar = *on ;
186600100204                else ;
186700100204                    note10_tar = *off ;
186800100204                endif ;
186900100126                update ta56s01 ;
187000100126
187100100115
187200100115         // riemetto la  videata
187300100204             $inzs01 = *on  ;
187400100204             $Fine4 = *on ;
187500100115             $video  = 'S1' ;
187600100115
187700100115           OTHER ;
187800100115
187900100115             Exsr SR_ctrw04 ;
188000100115             if  errGenerico = *on;
188100100204                 iter   ;
188200100115             endif;
188300100114
188400100114         ENDSL;
188500100114
188600100204         ENDDO;
188700100204
188800100114       ENDSR;
188900100115
189000100115       //--------------------------------------------------------------
189100100115       //?Carico videata Accettazione Offerta
189200100115       //--------------------------------------------------------------
189300100115       BEGSR Inzw04 ;
189400100115
189500100115          clear Ta56w04 ;
189600100115          w04ctr = s01ctr ;
189700100115          w04prg = s01prg ;
189800100115          w04tpt = s01tpt ;
189900100115          w04dtr = s01dtr ;
190000100115          w04dcv = s01dcv ;
190100100312          w04dpa = sh1dpa ;
190200100503          w04sis = ' ' ;
190300100204          $fine4 = *off ;
190400100114
190500100115       ENDSR;
190600100115
190700100115       //--------------------------------------------------------------
190800100115       //?Controllo videata Accettazione Offerta
190900100115       //--------------------------------------------------------------
191000100115       BEGSR Sr_CtrW04 ;
191100100115
191200100115            WindDspF = IndDspF;
191300100115            reset WindDspFb;
191400100115            IndDspF  = WindDspF;
191500100115
191600100115        // verifico se inserito numero spedizione affidateci
191700100519        // campo non più obbligatorio NICOLA 19-05-10
191800100519         // If    W04Nsa = 0  ;
191900100519         //   errMessage  = *on;
192000100519         //   errGenerico = *on;
192100100519         //   PosCurNsa   = *on;
192200100519         //   w04msg = $Msg(05);
192300100519         //   leavesr;
192400100519         // endif;
192500100316
192600100316        // Controllo data presunto affidamento
192700100316           If    W04Dpa <> 0;
192800100316             clear wlbdat;
192900100316             g02dat = W04Dpa;
193000100316             xsrda8(wlbdat);
193100100316             if g02err = '1';
193200100316               errMessage  = *on;
193300100316               errGenerico = *on;
193400100316               PosCurDpa   = *on;
193500100316               w04msg = $Msg(04);
193600100316               leavesr;
193700100316             endif;
193800100316
193900100316        // Verfiico che non sia inferiore all'anno 2000 e superiore al 2039
194000100316           If    G02Inv < 20000101 or G02Inv > 20391231 ;
194100100316               errMessage  = *on;
194200100316               errGenerico = *on;
194300100316               PosCurDpa   = *on;
194400100316               w04msg = $Msg(04);
194500100316               leavesr;
194600100316             endif;
194700100316
194800100316             w04dpa = g02dat;
194900100316             wrkdpa = g02inv;
195000100316           else ;
195100100316             errMessage  = *on;
195200100316             errGenerico = *on;
195300100316             PosCurDpa   = *on;
195400100316             w04msg = $Msg(04);
195500100316             leavesr;
195600100316           endif;
195700100323
195800100323        // Verifico che non sia inferiore ad oggi
195900100323           If    G02Inv < datacor8 ;
196000100323               errMessage  = *on;
196100100323               errGenerico = *on;
196200100323               PosCurDpa   = *on;
196300100323               w04msg = $Msg(24);
196400100323               leavesr;
196500100323             endif;
196600100323
196700100316
196800100115
196900100115       ENDSR;
197000100115
197100100624       //--------------------------------------------------------------
197200100624       //?Segnalazione errore incongruenza date deorrenza e scadenza offerta
197300100624       //--------------------------------------------------------------
197400100624       BEGSR GesW07;
197500100624
197600100624         // Inizializzazione videata
197700100624            exsr Inzw07;
197800130321
197900130321       //?Imposto già l'errore se emetto la window per data decorrenza
198000130321       //?offerta minore data ultima spedizione fatturata
198100130321            IF  sh5ddt < wDataSp;
198200130321              ErrMessage  = *on;
198300130321              ErrGenerico = *on;
198400130321              PosCurDdto  = *on;
198500130321              W07msg = 'Decorrenza Offerta <= Ultima Sped. Fatt. ' +
198600130321                        %subst(%editc(wDataSP:'X'):7:2) + '/' +
198700130321                        %subst(%editc(wDataSP:'X'):5:2) + '/' +
198800130321                        %subst(%editc(wDataSP:'X'):1:4);
198900130321            ENDIF;
199000100624
199100100624         // Gestione video
199200100624         DOW $Fine7 = *off;
199300100625         // Emissione Finestra di incongruenza date offerta tariffa
199400100624             exfmt  TA56W07;
199500100624             reset errMessage;
199600100624             reset errGenerico;
199700100624             clear w07msg;
199800100624
199900100624         SELECT;
200000100624
200100100624         // - F12=Ritorno
200200100624           WHEN  dsp_aid = c_F12;
200300100713             errGenerico = *on;
200400100624             $Fine7 = *on ;
200500100624
200600100624         // - F06=Modifica tariffa
200700100624           WHEN  dsp_aid = c_F06  ;
200800100707                 exsr Sr_Ctrw07 ;
200900100707             if  errGenerico = *on;
201000100707                 iter   ;
201100100707             endif;
201200100707
201300100707         // aggiorno le date decorrenza
201400100707             exsr Sr_Aggdate ;
201500100707             if  errGenerico = *on;
201600100707                 iter   ;
201700100707             endif;
201800100708         // se non ci sono errori aggiorno la date del subfile
201900100708                // mi salvo la data decorrenza e scadenza tariffa
202000100708                   sh5ddt = WRKddto;
202100100708                   sh5dst = WRKdsto;
202200100708                   data_eur= %date(sh5ddt:*iso) ;
202300100708                   s05ddt = %dec(data_eur) ;
202400100708                   update ta56s05 ;
202500100707
202600100624                  $Fine7 = *on ;
202700100707
202800100707           OTHER ;
202900100707
203000100707             Exsr SR_ctrw07 ;
203100100707             if  errGenerico = *on;
203200100707                 iter   ;
203300100707             endif;
203400100624
203500100624         ENDSL;
203600100624
203700100624         ENDDO;
203800100624
203900100624       ENDSR;
204000100624
204100100624       //--------------------------------------------------------------
204200100624       //?Carico videata Segnalazione errore date decor/scad Offerta
204300100624       //--------------------------------------------------------------
204400100624       BEGSR Inzw07 ;
204500100624
204600100624          clear Ta56w07 ;
204700100624          w07ctr = s05ctr ;
204800100624          w07prg = sh5prg ;
204900100624          w07tpt = s05tpt ;
205000100624          w07dcv = s05dcv ;
205100100624
205200100624        // Imposto le date
205300100624          data_eur = %date(sh5ddt :*iso);
205400100624          w07ddto = %dec(data_eur)  ;
205500100624
205600100707          data_eur = %date(sh5dst :*iso);
205700100707          w07dsto = %dec(data_eur)  ;
205800100707
205900100624          data_eur = %date(tamdst :*iso);
206000100624          w07dstt = %dec(data_eur)  ;
206100100624
206200100707          data_eur = %date(tamddt :*iso);
206300100707          w07ddtt = %dec(data_eur)  ;
206400100707          wrkddtt = tamddt          ;
206500100707
206600100624          $fine7 = *off ;
206700100624
206800100624       ENDSR;
206900100624
207000100707       //--------------------------------------------------------------
207100100707       //?Controllo videata Incongruenza data decorrenza scadenza
207200100707       //--------------------------------------------------------------
207300100707       BEGSR Sr_CtrW07 ;
207400100707
207500100707            WindDspF = IndDspF;
207600100708            reset WindDspFc;
207700100707            IndDspF  = WindDspF;
207800100707
207900100707
208000100707        // Controllo data scadenza tariffa
208100100707           If    W07Dstt <> 0;
208200100707             clear wlbdat;
208300100707             g02dat = W07Dstt;
208400100707             xsrda8(wlbdat);
208500100707             if g02err = '1';
208600100707               errMessage  = *on;
208700100707               errGenerico = *on;
208800100707               PosCurDstt  = *on;
208900100707               w07msg = $Msg(04);
209000100707               leavesr;
209100100707             endif;
209200100707
209300100707             w07dstt = g02dat;
209400100707             wrkdstt = g02inv;
209500130319
209600130319       //?se <= data ultima spedizione fatturata
209700130319       //?non posso metterla più bassa di quella che c'è
209800130319       //?già sul file
209900130319         IF  TAMdst  <= wDataSP and wrkdstt <> TAMdst and
210000130319             wrkdstt <  TAMdst;
210100130319           ErrMessage  = *on;
210200130319           ErrGenerico = *on;
210300130319           PosCurDstt  = *on;
210400130320           W07msg = 'Scadenza Tariffa <= '+
210500130319                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
210600130319                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
210700130319                    %subst(%editc(wDataSP:'X'):1:4) + ' ' +
210800130319                    'non si può anticipare.';
210900130319           leavesr;
211000130319         ENDIF;
211100130319       //?se > data ultima spedizione fatturata
211200130319       //?posso impostarla solo >= alla data ultima sp.fatturata
211300130319         IF  TAMdst  > wDataSP and wrkdstt <> TAMdst and
211400130319             wrkdstt < wDataSP;
211500130319           ErrMessage  = *on;
211600130319           ErrGenerico = *on;
211700130319           PosCurDstt  = *on;
211800130320           W07msg = 'Scadenza Tariffa < Ultima Sped. Fatt. ' +
211900130319                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
212000130319                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
212100130320                    %subst(%editc(wDataSP:'X'):1:4);
212200130319           leavesr;
212300130319         ENDIF;
212400130319
212500100707           else ;
212600100707             errMessage  = *on;
212700100707             errGenerico = *on;
212800100707             PosCurDstt  = *on;
212900100707             w07msg = $Msg(04);
213000100707             leavesr;
213100100707           endif;
213200100707
213300100707
213400100707        // Controllo data decorrenza offerta
213500100707           If    W07Ddto <> 0;
213600100707             clear wlbdat;
213700100707             g02dat = W07Ddto;
213800100707             xsrda8(wlbdat);
213900100707             if g02err = '1';
214000100707               errMessage  = *on;
214100100707               errGenerico = *on;
214200100707               PosCurDdto  = *on;
214300100707               w07msg = $Msg(04);
214400100707               leavesr;
214500100707             endif;
214600100707
214700100707             w07ddto = g02dat;
214800100707             wrkddto = g02inv;
214900130320       //?deve essere maggiore della data ultima sp. fatturata
215000130320           IF  wrkddto <= wDataSP;
215100130320             ErrMessage  = *on;
215200130320             ErrGenerico = *on;
215300130320             PosCurDdto  = *on;
215400130320             W07msg = 'Decorrenza Offerta <= Ultima Sped. Fatt. ' +
215500130320                       %subst(%editc(wDataSP:'X'):7:2) + '/' +
215600130320                       %subst(%editc(wDataSP:'X'):5:2) + '/' +
215700130320                       %subst(%editc(wDataSP:'X'):1:4);
215800130320             leavesr;
215900130320           ENDIF;
216000100707           else ;
216100100707             errMessage  = *on;
216200100707             errGenerico = *on;
216300100707             PosCurDdto  = *on;
216400100707             w07msg = $Msg(04);
216500100707             leavesr;
216600100707           endif;
216700100707
216800100707        // Controllo data scadenza offerta
216900100707           If    W07Dsto <> 0;
217000100707             clear wlbdat;
217100100707             g02dat = W07Dsto;
217200100707             xsrda8(wlbdat);
217300100707             if g02err = '1';
217400100707               errMessage  = *on;
217500100707               errGenerico = *on;
217600100707               PosCurDsto  = *on;
217700100707               w07msg = $Msg(04);
217800100707               leavesr;
217900100707             endif;
218000100707
218100100707             w07dsto = g02dat;
218200100707             wrkdsto = g02inv;
218300100707           else ;
218400100707             errMessage  = *on;
218500100707             errGenerico = *on;
218600100707             PosCurDsto  = *on;
218700100707             w07msg = $Msg(04);
218800100707             leavesr;
218900100707           endif;
219000100707
219100100707              data_iso = %date(wrkdstt:*iso) ;
219200100707              data_iso = data_iso + %days(1);
219300100707              wdata8=%dec(data_iso);
219400100707         // verifico le date decorrenza e scadenza se corrette
219500100707              if wrkdstt > wrkddto or
219600100707         // data scadenza tariffa   + 1 diversa da data decorrenza
219700100707                 wdata8 <> wrkddto ;
219800100707
219900100707                 errMessage  = *on;
220000100707                 errGenerico = *on;
220100100707                 PosCurDstt  = *on;
220200100707                 w07msg = $Msg(16);
220300100707                 leavesr;
220400100707              endif;
220500100707
220600100707         // verifico le date decorrenza e scadenza tariffa se corrette
220700100707              if wrkdstt < wrkddtt ;
220800100707
220900100707                 errMessage  = *on;
221000100707                 errGenerico = *on;
221100100707                 PosCurDstt  = *on;
221200100707                 w07msg = $Msg(27);
221300100707                 leavesr;
221400100707              endif;
221500100707
221600100707         // verifico le date decorrenza e scadenza offerta se corrette
221700100707              if wrkdsto < wrkddto ;
221800100707
221900100707                 errMessage  = *on;
222000100707                 errGenerico = *on;
222100100707                 PosCurDsto  = *on;
222200100707                 w07msg = $Msg(27);
222300100707                 leavesr;
222400100707              endif;
222500100707
222600100707       ENDSR;
222700100707
222800100707       //--------------------------------------------------------------
222900100707       //?Aggiorno date decorrenza scadenza tariffa / offerta
223000100707       //--------------------------------------------------------------
223100100707       BEGSR Sr_Aggdate;
223200100707
223300100707       // se modificate le date decorrenza e scadenza tariffa aggiorno TNTAM
223400100707          If Tamddt <> wrkddtt or tamdst <> wrkdstt ;
223500100707             chain(e) (c05ksc:s05ctr:tamprg) Tntam01l ;
223600100707             If %error ;
223700100707                 errMessage  = *on;
223800100707                 errGenerico = *on;
223900100707                 PosCurDstt  = *on;
224000100707                 w07msg = $Msg(28);
224100100707                 leavesr;
224200100707             endif;
224300100707             If %found(tntam01l) ;
224400100707                tamddt = wrkddtt ;
224500100707                tamdst = wrkdstt ;
224600100707                tamdtr = datacor8 ;
224700100707                tamftr = ' ' ;
224800100707                tamduv = datacor8 ;
224900100707                update tntam000 ;
225000100707             endif ;
225100100707          endif ;
225200100707
225300100707       // se modificate le date decorrenza e scadenza offerta aggiorno TNOFM
225400100707          If s05ddt <> w07ddto or sh5dst <> w07dsto ;
225500100707             chain(e) (c01nrv:s05ctr:sh5prg) tnofm01l ;
225600100707             If %error ;
225700100707                 errMessage  = *on;
225800100707                 errGenerico = *on;
225900100707                 PosCurDdto  = *on;
226000100707                 w07msg = $Msg(29);
226100100707                 leavesr;
226200100707             endif;
226300100707             If %found(tnofm01l) ;
226400100707                tamddt = wrkddto ;
226500100707                tamdst = wrkdsto ;
226600100707                tamdtr = datacor8 ;
226700100707                tamftr = ' ' ;
226800100707                tamduv = datacor8 ;
226900100707                update tnofm000 ;
227000100707             endif ;
227100100707          endif ;
227200100707
227300100707       ENDSR;
227400100707
227500100625       //--------------------------------------------------------------
227600100625       //?Segnalazione di attività 30 da annullare
227700100625       //--------------------------------------------------------------
227800100625       BEGSR GesW08;
227900100625
228000100625         // Inizializzazione videata
228100100625            exsr Inzw08;
228200100625
228300100625         // Gestione video
228400100625         DOW $Fine8 = *off;
228500100625         // Emissione Finestra di attività 30 da annullare
228600100625             exfmt  TA56W08;
228700100625             reset errMessage;
228800100625             reset errGenerico;
228900100625
229000100625         SELECT;
229100100625
229200100625         // - F06=Conferma annullamento attività
229300100628           WHEN  dsp_aid = c_F06  ;
229400100628                 if w08conf = 'S' ;
229500100625          // richiamo la routine che deleta le attività 30 e le sue note
229600100628                    Exsr Sr_Del_30 ;
229700100625                  $Fine8 = *on ;
229800100628                 endif ;
229900100628
230000100628                 if w08conf = 'N' ;
230100100628          // richiamo la routine che deleta le attività 30 e le sue note
230200100628                  $Fine8 = *on ;
230300100628                 endif ;
230400100625
230500100625         ENDSL;
230600100625
230700100625         ENDDO;
230800100625
230900100625       ENDSR;
231000100625
231100100625       //--------------------------------------------------------------
231200100625       //?Carico videata Segnalazione attività 30 da annullare
231300100625       //--------------------------------------------------------------
231400100625       BEGSR Inzw08 ;
231500100625
231600100625         clear Ta56w08 ;
231700100625         clear TIBS02ds;
231800100625         clear dCCO;
231900100625         T02mod = 'C';
232000100625         T02cod = 'CCO';
232100100625         T02ke1 = '30';
232200100625         T02sif = KNSIF;
232300100625         TNTBE_RicercaControllo (kpjba : tibs02ds);
232400100625         IF  T02err = *blanks;
232500100625           dCCO = T02uni;
232600100625         ENDIF;
232700100625         W08att = '30 - ' + §CCOdes;
232800100625         W08att = %trim(W08att) + ' aperte ?';
232900100625         W08conf = ' ';
233000100625
233100100625          $fine8 = *off ;
233200100625
233300100625       ENDSR;
233400091216       //--------------------------------------------------------------
233500091216       //?Verifica situazione Trattativa ed esitazione trattativa
233600091216       //--------------------------------------------------------------
233700091216       BEGSR sr_EsiTrat ;
233800091216
233900100107         clear N_off_trat;          // numero totale offerte
234000100107         clear N_off_hold;          // numero offerte congelate
234100100107         clear N_off_conv;          // numero offerte convalidate
234200100625         clear N_off_acce;          // numero offerte accettate
234300100126         clear N_off_daesi;         // numero offerte da esitare
234400100107
234500100107       // verifico  le offerte legate alla trattativa
234600091216         vofnrv = c01nrv ;
234700110112         setll vofnrv Tivof11l ;
234800110112         reade(n) vofnrv Tivof11l ;
234900110112         dow not %eof(tivof11l) ;
235000100107
235100100422       // se non annullata sommo il numero delle offerte per trattativa
235200100422             If vofeso <> '*' ;
235300100422                N_off_trat = N_off_trat + 1 ;
235400100422             endif ;
235500100111       // congelate
235600100107             If vofeso = 'H' ;
235700100107                N_off_hold = N_off_hold + 1 ;
235800100107             endif ;
235900100111       // convalidate
236000100107             If vofeso = 'C' ;
236100100107                N_off_conv = N_off_conv + 1 ;
236200100107             endif ;
236300100625       // accettate
236400100625             If vofeso = 'A' ;
236500100625                N_off_acce = N_off_acce + 1 ;
236600100625             endif ;
236700100126        // accettate oppure ancora da esitare
236800100126             If vofeso = 'A' or vofeso = ' ' ;
236900100126                N_off_Daesi = N_off_daesi + 1 ;
237000100107             endif ;
237100091216
237200110112             reade(n) vofnrv Tivof11l ;
237300100107         enddo ;
237400091216       ENDSR ;
237500091216
237600100107
237700100107       //--------------------------------------------------------------
237800100108       //?Deleto  le attività legate alla singola trattativa o offerta
237900100107       //--------------------------------------------------------------
238000100107       BEGSR sr_DelAtc ;
238100100108
238200100108       // verifico se devo cancellare le sole attività legate all'offerta
238300100108       // oppure alla trattativa in genere
238400100108
238500100108         atccpo = ch1cpo ;
238600100108         atcksc = c01ksc ;
238700100108         atcnrv = c01nrv ;
238800100108         atcdco = 0 ;
238900100108         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
239000100127         reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
239100100108         dow not %eof(tiatc04l);
239200100108
239300100108             SELECT ;
239400100111       // solo attività offerta ?
239500100111             WHEN $offerta = *on ;
239600100111                  If atcctr <> *blank and atcprg <> *blank  and
239700100111                     s01ctr = %dec(atcctr:3:0) and s01prg = %dec(atcprg:3:0) ;
239800100127                     delete (atctat:atcatn:atcatnp) tiatc001 ;
239900100203       // cancello eventuali legami attività
240000100127                     delete (atcatn:atcatnp) tiatl01l ;
240100100111       // se l'attività era un progressivo uno vado a cancellare le note
240200100111       // con progressivo 0
240300100108                     If atcatnp = 1 ;
240400100127                        exsr Sr_DelNote ;
240500100108                     endif ;
240600100108                  endif ;
240700100111       // tutte attività trattativa  ?
240800100108             OTHER ;
240900100127                  delete (atctat:atcatn:atcatnp) tiatc001 ;
241000100125       // cancello eventuali legami attività
241100100127                  delete (atcatn:atcatnp) tiatl01l ;
241200100111       // se l'attività era un progressivo 1   vado a cancellare le note
241300100111       // con progressivo 0
241400100108                  If atcatnp = 1 ;
241500100127                     exsr Sr_DelNote ;
241600100108                  endif ;
241700100108
241800100108             endsl ;
241900100108
242000100127             reade   (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
242100100108         enddo ;
242200100108
242300100107       ENDSR ;
242400100108
242500100108       //--------------------------------------------------------------
242600100108       //?Deleto  le note legate alle attività ancora da eseguire !!!!!
242700100108       //--------------------------------------------------------------
242800100108       BEGSR sr_DelNote;
242900100108
243000100108       // Deleto le sole note relative ad una attività da fare con progressivo
243100100108       // 1
243200100108
243300100108         cpncpo = atccpo ;
243400100108         cpntat = atctat ;
243500100108         cpnatn = atcatn ;
243600100108         cpnatnp= 0 ;
243700100108         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
243800100108         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
243900100108         dow not %eof(ticpn03l);
244000100108             delete ticpn000;
244100100108             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
244200100108         enddo ;
244300100108
244400100108       ENDSR ;
244500100108
244600100108       //--------------------------------------------------------------
244700100108       //?Deleto  le note 10
244800100108       //--------------------------------------------------------------
244900100108       BEGSR sr_DelNote10;
245000100108
245100100108       // Deleto le sole note 10 legate all'offerta se non esistono altri
245200100108       // progressivi
245300100108
245400100108       // prima verifico se ho altri progressivi
245500100108
245600100425         setll (Keynrv:Keyctr) Tnofm01l ;
245700100108         If not %equal ;
245800100108
245900100108       // preparo la chiave
246000100108            ntcapl = 'T' ;
246100100425            ntcnk1 = %editc(dutkci: 'X') + %editc(Keynrv:'X');
246200100425            ntcnk2 = %editc(Keyctr: 'X') ;
246300100108            ntctnt = '10' ;
246400100108            setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
246500100108            reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
246600100108            dow not %eof(tfntc01l) ;
246700100111                delete tfntc;
246800100108                reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
246900100108            enddo ;
247000100108
247100100108         endif ;
247200100108
247300100108       ENDSR ;
247400100108
247500100628
247600100628       //--------------------------------------------------------------
247700100628       //?Deleto  le attività 30 legata alla trattativa
247800100628       //--------------------------------------------------------------
247900100628       BEGSR sr_Del_30 ;
248000100628
248100100628
248200100628         atccpo = ch1cpo ;
248300100628         atcksc = c01ksc ;
248400100628         atcnrv = c01nrv ;
248500100628         atcdco = 0 ;
248600100628         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
248700100628         reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
248800100628         dow not %eof(tiatc04l);
248900100628
249000100628             SELECT ;
249100100628       // se si tratta della stessa attività che mi ha portato qui non deleto
249200100628             When atctat=ita56tat and atcatn=ita56atn and atcatnp=ita56atnp;
249300100628
249400100628             When atccad = '30' ;
249500100628                  delete (atctat:atcatn:atcatnp) tiatc001 ;
249600100628       // cancello eventuali legami attività
249700100628                  delete (atcatn:atcatnp) tiatl01l ;
249800100628       // se l'attività era un progressivo 1   vado a cancellare le note
249900100628       // con progressivo 0
250000100628                  If atcatnp = 1 ;
250100100628                     exsr Sr_DelNote ;
250200100628                  endif ;
250300100628
250400100628             endsl ;
250500100628
250600100628             reade   (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
250700100628         enddo ;
250800100628
250900100628       ENDSR ;
251000100628
251100091209       //--------------------------------------------------------------
251200091209       //?Accettazione Offerta
251300091209       //--------------------------------------------------------------
251400091209       BEGSR sr_AccOff ;
251500091209
251600100115       // aggiorno l'esito in tivof ea anche i dati di presunto
251700100115       // affidamento
251800100115
251900100115         Tamksc = c01nrv ;
252000100115         Tamctr = s01ctr ;
252100100115         Tamprg = s01prg ;
252200110112         chain (tamksc:tamctr:tamprg) tivof11l ;
252300110112         If %found(tivof11l) ;
252400100115            vofeso = 'A ' ;
252500100115            vofdpa = wrkdpa  ;
252600100115            vofnsa = w04nsa  ;
252700100115            update tivof000 ;
252800100115            $Esitata = *on ;
252900100115         endif ;
253000100126
253100100126       // aggiorno copia firmata su tnofm
253200100126         chain (tamksc:tamctr:tamprg) tnofm01l ;
253300100126         If %found(tnofm01l) ;
253400100126            tamsis = w04sis ;
253500100126            update tnofm000;
253600100126         endif ;
253700100126
253800100115
253900100115       // prima di scrivere una nuova attività legata alla offerta in questione
254000100115       // devo annullare eventuali attività già legate all'offerta
254100100115         $offerta = *on ;
254200100115         exsr Sr_DelAtc ;
254300100115         $offerta = *off;
254400100115
254500100115       // propongo una nuova attività di richiamo con causale 31 cliente
254600100115       // caldo
254700100115         causale = '31' ;
254800100115
254900100426       // Richiamo la routine che crea nuove attività
255000100426               exsr Sr_CreaAtt ;
255100100426
255200100115
255300091209       ENDSR ;
255400100118
255500091209       //--------------------------------------------------------------
255600091209       //?Congelamento Offerta
255700091209       //--------------------------------------------------------------
255800091209       BEGSR sr_CongeOff ;
255900091209
256000100113         // Aggiorno il record tivof
256100100113
256200100113         Tamksc = c01nrv ;
256300100113         Tamctr = s01ctr ;
256400100113         Tamprg = s01prg ;
256500110112         chain (tamksc:tamctr:tamprg) tivof11l ;
256600110112         If %found(tivof11l) ;
256700100422            if vofeso <> '*' and vofeso <> 'C';
256800100422               vofeso = 'H' ;
256900100114               data_iso = (%date()) ;
257000100114               vofdch = %dec(data_iso) ;
257100100114               update tivof000 ;
257200100115               $Esitata = *on ;
257300100114            endif ;
257400100114         endif ;
257500100114
257600100128       // Cancello attività e note attività legate all'offerta
257700100128             $offerta = *on ;
257800100128             exsr Sr_Delatc ;
257900100128             $offerta = *off;
258000100128
258100100114         // verifico in che situazione si trova la trattativa
258200100114             exsr Sr_EsiTrat ;
258300100114
258400100114       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
258500100126             If N_off_daesi= 0 ;
258600100114       // emetto la videata di richiesta di conferma
258700100114                $video = 'W3' ;
258800100114                $inzw03 = *on  ;
258900100114                leavesr ;
259000100114
259100100114             Endif ;
259200100114
259300100114
259400100114          $inzs01 = *on  ;
259500100115          $video  = 'S1' ;
259600091209       ENDSR ;
259700091209
259800100108       //--------------------------------------------------------------
259900100312       //?Propongo la creazione di una nuova attività  solo se sono
260000100312       //?su trattativa senza codice cliente
260100100108       //--------------------------------------------------------------
260200100111       BEGSR Sr_CreaAtt ;
260300100312
260400100425         // verifico se trattativa solo su potenziale e mai codificato
260500110117         // oppure devo scrivere l'attività 31
260600110117        If (c01ksc = 0  and mai_codific = *on) or
260700110117           (causale = '31') ;
260800100108
260900100108         // Richiamo il pgm trmk17c
261000100108             clear trmk17ds ;
261100100108             imk17fcmt = '1'  ;
261200100108             imk17cmt = 'S'  ;
261300100108         // devo recuperare i dati da registrare nell'attività futura
261400100108             imk17cpo = ch1cpo;
261500100108             imk17ksc = c01ksc ;
261600100108             imk17nrv = c01nrv ;
261700100121             imk17cmm = c01cmm ;
261800100303             imk17co3 = ita56com ;
261900100111
262000100115         // Se la causale è valorizzata propongo i campi nella ds
262100100111            If causale <> '  ' ;
262200100111
262300100111               clear  dCCO;
262400100111               clear  TIBS02ds;
262500100111               T02mod = 'C';
262600100111               T02cod = 'CCO';
262700100111               T02ke1 = causale ;
262800100111               T02sif = KNSIF;
262900100111               TNTBE_RicercaControllo  (kpjba : tibs02ds);
263000100111               if  T02err = *blanks;
263100100111                   dcco = t02uni ;
263200100111               endif;
263300100111             // se è valorizzato il tipo attività associata la propongo
263400100111             // proteggendola
263500100111               If  §ccoatt <> ' ' ;
263600100111                   imk17att = §ccoatt ;
263700100111                   imK17patt = 'S' ;
263800100111               endif ;
263900100108
264000100111             // valorizzo la causale e la proteggo
264100100111               imk17cau = causale ;
264200100111               imk17pcau = 'S' ;
264300100111
264400100115               If causale = '27' ;
264500100115             // se la causale 27
264600100111             // se ci sono giorni di scadenza futuri li propongo nel calcolo
264700100111             // data scadenza
264800100924                  If §ccoupm <> ' ' and
264900100924                     §ccogio > 0;
265000100420                     clear XGIOLAVDS;
265100100420                     IXGLdata = %dec(%date());
265200100924                     If §ccoupm = '+' ;
265300100924                        IXGLadd  = 'S';
265400100924                     else ;
265500100924                        IXGLsub  = 'S';
265600100924                     endif;
265700100420                     IXGLgg   = §CCOgio;
265800100420                     IXGLpa   = 'P';
265900100420                     If  c01cmm > *zeros;
266000100420                         IXGLfil = %dec(%subst(%editc(c01cmm:'X'):1:3):3:0);
266100100420                     else ;
266200100420                         IXGLfil = DUTpou;
266300100420                     Endif;
266400100420                     XGIOLAV (xgiolavds);
266500100420                     If  OXGLerr = *blanks;
266600100420                         imk17dad = OXGLdata;
266700100420                     Endif;
266800100115                  endif;
266900100924                  If §ccoupm = '=' ;
267000100924                    imk17dad = %dec(%date());
267100100924                  Endif ;
267200100127                  imk17nrv = 0 ;
267300100224             // inibisco F12
267400100224                  imk17f12 = 'S' ;
267500100115               endif;
267600100115
267700100115               If causale = '31' ;
267800100115             // se la causale 31
267900100115             // la data futura viene calcolata sommando 15 alla data di presunta
268000100115             // affidamento
268100100924                  If §ccoupm <> ' ' and
268200100924                     §ccogio > 0;
268300100420                     clear XGIOLAVDS;
268400100420                     IXGLdata = vofdpa ;
268500100924                     If §ccoupm = '+' ;
268600100924                        IXGLadd  = 'S';
268700100924                     else ;
268800100924                        IXGLsub  = 'S';
268900100924                     endif;
269000100420                     IXGLgg   = §CCOgio;
269100100420                     IXGLpa   = 'P';
269200100420                     If  c01cmm > *zeros;
269300100420                         IXGLfil = %dec(%subst(%editc(c01cmm:'X'):1:3):3:0);
269400100420                     else ;
269500100420                         IXGLfil = DUTpou;
269600100420                     Endif;
269700100420                     XGIOLAV (xgiolavds);
269800100420                     If  OXGLerr = *blanks;
269900100420                         imk17dad = OXGLdata;
270000100420                     Endif;
270100100924                  Endif ;
270200100924                  If §ccoupm = '=' ;
270300100924                    imk17dad = vofdpa;
270400100924                  Endif ;
270500100115               endif;
270600100126
270700100126               If causale = '32' ;
270800100126             // se la causale 32
270900100126             // pulisco il campo della trattativa
271000100126                  imk17nrv = 0 ;
271100100126                  imk17ksc = tamksc ;
271200100127             // la data futura viene calcolata sommando 15 alla data di reale
271300100126             // affidamento
271400100924                If §ccoupm <> ' ' and
271500100924                  §ccogio > 0;
271600100420                  clear XGIOLAVDS;
271700100420                  IXGLdata = vofdra ;
271800100924                  If §ccoupm = '+' ;
271900100924                     IXGLadd  = 'S';
272000100924                  else ;
272100100924                     IXGLsub  = 'S';
272200100924                  endif;
272300100420                  IXGLgg   = §CCOgio;
272400100420                  IXGLpa   = 'P';
272500100420                  If  c01cmm > *zeros;
272600100420                      IXGLfil = %dec(%subst(%editc(c01cmm:'X'):1:3):3:0);
272700100420                  else ;
272800100420                      IXGLfil = DUTpou;
272900100420                  Endif;
273000100420                  XGIOLAV (xgiolavds);
273100100420                  If  OXGLerr = *blanks;
273200100420                      imk17dad = OXGLdata;
273300100420                  Endif;
273400100924                Endif ;
273500100924                If §ccoupm = '=' ;
273600100924                  imk17dad = vofdra;
273700100924                Endif ;
273800100420               endif;
273900100115             endif;
274000100115
274100100108             trmk17c (kpjba : trmk17ds) ;
274200100108
274300100426             endif ;
274400100108       ENDSR;
274500100114
274600100203
274700100128       //--------------------------------------------------------------
274800100128       //?Controllo in fase di accettazione se esistono altre versioni
274900100128       //?da annullare o da congelare
275000100128       //--------------------------------------------------------------
275100100128       BEGSR Sr_CtrAcc  ;
275200100128
275300100128         // rilascio il record precedentemente letto
275400100204           If  sh1n12 = '1' ;
275500100204               note10_tar = *on ;
275600100204           else ;
275700100204               note10_tar = *off ;
275800100204           endif ;
275900100202           update ta56s01 ;
276000100202
276100100204         // mi salvo il codice offerta e versione che si vuole accettare
276200100128         sav_ctr = s01ctr ;
276300100128         sav_ver = s01prg ;
276400100128
276500100128
276600100128         // verifico leggendo tutti i records del subfile se esistono
276700100128         // più versioni della stessa offerta
276800100128         numrec= 0 ;
276900100128         Dow 1 = 1 ;
277000100128             numrec += 1;
277100100128             chain numrec ta56s01 ;
277200100128             If not %found(Tnta56d) ;
277300100128                leave ;
277400100128             endif ;
277500100128
277600100312          //   if s01opz = 'E' ;
277700100312          //      clear s01opz ;
277800100312          //   endif ;
277900100128         // verifico se si tratta della stessa offerta / versione
278000100128             if s01ctr = sav_ctr and s01prg = sav_ver ;
278100100128                iter ;
278200100128             endif ;
278300100128
278400100128         // verifico se si tratta di un'altra offerta
278500100128             if s01ctr <> sav_ctr ;
278600100128                iter ;
278700100128             endif ;
278800100128
278900100128         // verifico se si tratta della stessa offerta ma è già stata Congelata
279000100312             if s01ctr = sav_ctr and sh1eso = 'H' ;
279100100128                iter ;
279200100128             endif ;
279300100128
279400100128         // se si tratta della stessa offerta ed è già stata selezionata
279500100128         // un'opzione di accettazione do errore oppure è già stata accettata
279600100128         // in passato
279700111214         // o un'opzione di convalida o è già stata convalidata in passato
279800111214             if s01ctr = sav_ctr and ((s01opz = 'A' or sh1eso = 'A') or
279900111214                                     (s01opz = 'C' or sh1eso = 'C'));
280000100128                errMessage  = *on;
280100100128                errGenerico = *on;
280200100128                PosCurOpz   = *on;
280300100312         // se si tratta di convalida  metto un'altro msg
280400100312                If s01opz = 'C';
280500111214                   V1Dmsg = $Msg(22);
280600100312                else ;
280700100312                   V1Dmsg = $Msg(19);
280800100312                endif ;
280900100204                If  sh1n12 = '1' ;
281000100204                    note10_tar = *on ;
281100100204                else ;
281200100204                    note10_tar = *off ;
281300100204                endif ;
281400100128                update ta56s01 ;
281500100128                leave ;
281600100128             endif ;
281700100128
281800100128         // se si tratta della stessa offerta ed è già stata selezionata
281900100128         // un'opzione di annullamento o congelamento tutto ok
282000100128             if s01ctr = sav_ctr and (s01opz = '4' or s01opz = 'H');
282100100128                iter ;
282200100128             endif ;
282300100128
282400100128         // se si tratta della stessa offerta ed è già stata selezionata
282500100128         // un'opzione di annullamento o congelamento tutto ok
282600100128             if s01ctr = sav_ctr and s01opz = ' ' ;
282700100128                errMessage  = *on;
282800100128                errGenerico = *on;
282900100128                PosCurOpz   = *on;
283000100312         // se si tratta di convalida  metto un'altro msg
283100111214                if TTR_Accettaz = *off ;
283200111214                   V1Dmsg = $Msg(23);
283300100312                else ;
283400100312                   V1Dmsg = $Msg(20);
283500100312                endif ;
283600100204                If  sh1n12 = '1' ;
283700100204                    note10_tar = *on ;
283800100204                else ;
283900100204                    note10_tar = *off ;
284000100204                endif ;
284100100128                update ta56s01 ;
284200100128                leave ;
284300100128             endif ;
284400100128
284500100128         Enddo ;
284600100128
284700100128         // riaggancio il record letto in precedenza
284800100128         chain c01rcd ta56s01;
284900140221
285000140221       // Se non ho avuto un errore precedente controllo la tariffa
285100140221       // particolare Fuel Surcharge
285200140221         IF  not errMessage;
285300140221           exsr ctrFuel;
285400140221         ENDIF;
285500100128
285600100128       ENDSR ;
285700100128
285800140221       //--------------------------------------------------------------
285900140221       //?Controllo la tariffa particolare Fuel
286000140221       //--------------------------------------------------------------
286100140221       BEGSR ctrFuel;
286200140221
286300140221       // Se trovo tariffa particolare Fuel e la data del prezzo base
286400140221       // di riferimento è maggiore a oggi devo dare errore
286500140221
286600140221         TPTftc = 'f ';
286700140221         chain(n) (C01nrv:S01ctr:S01prg:TPTftc) TIOPT01L;
286800140221    3    IF  %found and TPTatb = *blanks and TPTdpb > datacor8;
286900140221           errMessage  = *on;
287000140221           errGenerico = *on;
287100140221           PosCurOpz   = *on;
287200140221           V1Dmsg = $msg(33);
287300140221           IF  sh1n12 = '1';
287400140221             note10_tar = *on;
287500140221           ELSE;
287600140221             note10_tar = *off;
287700140221           ENDIF;
287800140221           update ta56s01;
287900140221         ENDIF;
288000140221
288100140221       ENDSR;
288200100115
288300100118       //--------------------------------------------------------------
288400100118       //?Convalida Offerte
288500100118       //--------------------------------------------------------------
288600100118       BEGSR Sr_ConvOff ;
288700100118
288800100118         // rilascio il record precedentemente letto
288900100204         If  sh1n12 = '1' ;
289000100204             note10_tar = *on ;
289100100204         else ;
289200100204             note10_tar = *off ;
289300100204         endif ;
289400100118         update ta56s01 ;
289500100118
289600100118         // pulisco il subfile della convalida
289700100118         exsr pulsfl5 ;
289800100118         clear s05nrr ;
289900100118         SflNxtChg5= *off;
290000100505
290100100505         // pulisco i campi di comodo
290200100805         clear newksc ;
290300100518         wforzacon = *off ;
290400100118
290500100118         // verifico leggendo tutti i records del subfile se esistono
290600100120         // più convalide e verifico che non ci siano degli errori
290700100118         numrec= 1 ;
290800100118         Dow 1 = 1 ;
290900100118             chain numrec ta56s01 ;
291000100121             If not %found(Tnta56d) ;
291100100118                leave ;
291200100118             endif ;
291300100118
291400100312         // se opzione = C  convalida verifico se ci sono degli errori se
291500100312         // non esito una trattativa di tipo Aumento
291600110114             If s01opz = 'C' and  sh1eso <> 'A' and TTR_accettaz = *on ;
291700100118                errMessage  = *on;
291800100118                errGenerico = *on;
291900100118                PosCurOpz   = *on;
292000100118                V1Dmsg = $Msg(06);
292100100126                s01opz = 'C' ;
292200100204                If  sh1n12 = '1' ;
292300100204                    note10_tar = *on ;
292400100204                else ;
292500100204                    note10_tar = *off ;
292600100204                endif ;
292700100121                update ta56s01 ;
292800100118                leave ;
292900100121             endif ;
293000100121             If s01opz = 'C'  ;
293100100118                if s05nrr < *hival ;
293200100118                   S05nrr += 1;
293300100118                   s05ctr = s01ctr ;
293400100121                   sh5prg = s01prg ;
293500100118                   s05tpt = s01tpt ;
293600100118                   s05dtr = s01dtr ;
293700100118                   s05dcv = s01dcv ;
293800100118
293900100118                   tamksc = c01nrv ;
294000100118                   tamctr = s01ctr ;
294100100118                   tamprg = s01prg ;
294200100118                   chain(n) (tamksc:tamctr:tamprg) tnofm01l ;
294300100118                   If %found(tnofm01l) ;
294400100118                      s05sis = tamsis  ;
294500100203                      data_eur= %date(tamddt:*iso) ;
294600100203                      s05ddt = %dec(data_eur) ;
294700100118                   endif;
294800100118
294900100118                // mi salvo il numero record del primo subfile
295000100118                   sh5nrr = s01nrr ;
295100100118
295200100120                // mi salvo la data decorrenza e scadenza tariffa
295300100120                   sh5ddt = tamddt ;
295400100120                   sh5dst = tamdst ;
295500100120
295600100118                   write TA56S05;
295700100118                endif ;
295800100118
295900100204                If  sh1n12 = '1' ;
296000100204                    note10_tar = *on ;
296100100204                else ;
296200100204                    note10_tar = *off ;
296300100204                endif ;
296400100507
296500100507                clear s01opz ;
296600100118             update ta56s01 ;
296700100121             endif ;
296800100118
296900100118             numrec += 1;
297000100118
297100100118         Enddo ;
297200100118
297300100121         // riaggancio il record letto in precedenza
297400100121         chain c01rcd ta56s01;
297500100729
297600100729
297700100729         // se non ci sono errori emetto la videata altrimenti torno al sfl
297800100729         // precedente
297900100118         If ErrGenerico = *off ;
298000100204            exsr Gess05 ;
298100100118         endif;
298200100118
298300100118       ENDSR ;
298400100729
298500100729       //--------------------------------------------------------------
298600100729       //?Gestione scelta codice cliente per la convalida
298700100729       //--------------------------------------------------------------
298800100729       BEGSR GesS10;
298900100729
299000100729         // Inizializzazione videata
299100100729            exsr Inzs10;
299200100729
299300100804         Dow $fine10 = *off ;
299400100729         // Emissione Testata e Piede con tasti funzionali abilitati
299500100804           write  TA56W10;
299600100729
299700100729
299800100729         // Emissione videata
299900100804         exfmt  TA56WC10;
300000100729         reset errMessage;
300100100729         reset errGenerico;
300200100729
300300100729         SELECT;
300400100729
300500100729         // - F12=Ritorno
300600100729           WHEN  dsp_aid = c_F12;
300700100804             $video = 'S5';
300800100729             leave ;
300900100729
301000100729         // - F06=Convalida
301100100729           WHEN  dsp_aid = c_F06  ;
301200100804             Exsr SR_ctrS10 ;
301300100729             if  errGenerico = *on;
301400100729                 iter   ;
301500100729             endif;
301600100729
301700100804         // Elaboro la scelta del codice cliente per la convalida
301800100805             exsr F06W10;
301900100817        // se ritorna l'errore generico acceso vuol dire che è stato dato
302000100817        // f12 e devo riemettere la videata
302100100817             If errGenerico = *on ;
302200100817                iter    ;
302300100817             Endif ;
302400100729
302500100804             $fine10= *on ;
302600100729
302700100729           OTHER ;
302800100729
302900100804             Exsr SR_ctrs10 ;
303000100729             if  errGenerico = *on;
303100100729                 iter   ;
303200100729             endif;
303300100729
303400100729         ENDSL;
303500100729
303600100729         ENDDO;
303700100729
303800100729       ENDSR;
303900100729
304000100729       //--------------------------------------------------------------
304100100729       //?Carico subfile con le scelte dei codici clienti da convalidare
304200100729       //--------------------------------------------------------------
304300100729       BEGSR Inzs10 ;
304400100729
304500100729            $fine10 = *off ;
304600100730
304700100730       // Pulizia subfile
304800100730         SflDsp10_N   = *on;
304900100730         SflDspCtl10_N= *on;
305000100730         write  TA56WC10;
305100100730         SflDspCtl10_N= *off;
305200100730         SflEnd10     = *off;
305300100730
305400100730
305500100730         clear W_SflPag10;
305600100730         clear C10rcd;
305700100730         clear C10csr;
305800100730         clear S10nrr;
305900100816         clear w10opz;
306000100730         SflNxtChg10= *off;
306100100730         errMessage  = *off;
306200100730         errGenerico = *off;
306300100730
306400100729
306500100729         // proporre le scelte di codifica in base al tipo visita
306600100729         // ed alla presenza o meno di altri codici a cui appoggiarsi
306700100729
306800100730         // se si tratta di una trattativa di tipo nuovo :
306900100729         // 1) propongo un nuovo codice con il commerciale della trattativa
307000100729         // 2) propongo un nuovo codice con commerciale a scelta
307100100729         // 3) se già convalidata propongo l'elenco dei codici presenti in tivof
307200100729
307300100729            If vistpv = 'N'  ;
307400100730                   S10nrr = 1;
307500100730                   w10des = 'Nuovo cliente legato al comm.le ' ;
307600100730                   w10des = %trim(w10des) + ' ' + %editc(c01cmm: 'X');
307700100730                   w10hcmm = c01cmm ;
307800100730                   clear w10hksc ;
307900121128                   clear W10Hpre;
308000100730                   write TA56WS10;
308100100730
308200100730         // se nuovo aggiungo anche l'opzione di scelta nuovo codice con
308300100730         // commerciale diverso
308400100730                   S10nrr += 1;
308500100924                   w10des = 'Nuovo cliente legato ad altro comm.le ' ;
308600100730                   clear w10hcmm ;
308700100730                   clear w10hksc ;
308800121128                   clear W10Hpre;
308900100730                   write TA56WS10;
309000100730
309100100730             endif ;
309200100730
309300100730         // se si tratta di una trattativa di tipo non  nuovo :
309400100730         // 1) propongo codice cliente della trattativa
309500100730         // 2) propongo un nuovo codice con il commerciale della trattativa
309600100730         // 3) propongo un nuovo codice con commerciale a scelta
309700100730         // 4) se già convalidata propongo l'elenco dei codici presenti in tivof
309800100730
309900100730            If vistpv <> 'N'  ;
310000100730                   S10nrr = 1;
310100100730                   w10des = 'Aggiungi al cliente della Trattativa' ;
310200100730                   w10des = %trim(w10des) + ' ' + %editc(c01ksc: 'X');
310300100730                   w10hcmm = c01cmm ;
310400100730                   w10hksc = c01ksc ;
310500121128                   clear W10Hpre;
310600100730                   write TA56WS10;
310700100730
310800100730         // propongo un nuovo codice con il commerciale della trattativa
310900100730                   S10nrr += 1;
311000100730                   w10des = 'Nuovo cliente legato al comm.le ' ;
311100100730                   w10des = %trim(w10des) + ' ' + %editc(c01cmm: 'X');
311200100730                   w10hcmm = c01cmm ;
311300100730                   clear w10hksc ;
311400121128                   clear W10Hpre;
311500100730                   write TA56WS10;
311600100730
311700100730         // propongo un nuovo codice con il commerciale della trattativa
311800100730                   S10nrr += 1;
311900100730                   w10des = 'Nuovo cliente legato a nuovo comm.le ' ;
312000100730                   clear w10hcmm ;
312100100730                   clear w10hksc ;
312200121128                   clear W10Hpre;
312300100730                   write TA56WS10;
312400100730
312500100730             endif ;
312600121128
312700121128         //?Per qualsiasi tipo trattativa
312800121129         //?proposta di nuovo codice già prenotato
312900121128                   S10nrr += 1;
313000121129                   w10des = 'Nuovo codice già prenotato';
313100121128                   clear w10hcmm ;
313200121128                   clear w10hksc ;
313300121128                   W10Hpre = 'S';
313400121128                   write TA56WS10;
313500100730
313600100730         // sia per trattativa di tipo nuovo che no propongo anche i codici
313700100730         // clienti delle offerte gia convalidate se ce ne sono
313800100730            If visesi =  'P'  ;
313900100730
314000100805               exec sql  declare C1  cursor for
314100100730                   select distinct vofksc
314200100730                   from tivof00f where vofnrv = :visnrv and vofeso = 'C' and
314300110112                   vofdco > 0 and vofctr <> 999 ;
314400100730
314500100805               exec sql open C1;
314600100730               dow  sqlcod = 0;
314700100730                    exec sql fetch next from C1  into :vofksc;
314800100730                    IF sqlcod = 100 or sqlcod < 0;
314900100805                       leave  ;
315000100730                    endif;
315100100730            // se cliente diverso da quello della trattativa lo propongo
315200100730                    If vofksc <> c01ksc ;
315300100730                       S10nrr += 1;
315400100730                       w10des = 'Aggiungi la tariffa al cliente' ;
315500100730                       w10des = %trim(w10des) + ' ' + %editc(vofksc: 'X');
315600100730                       clear w10hcmm ;
315700121128                       clear W10Hpre;
315800100730                       w10hksc = vofksc ;
315900100730                       write TA56WS10;
316000100730                    endif ;
316100100730               Enddo ;
316200100730
316300100805               exec sql close C1 ;
316400100730
316500100730            Endif ;
316600100730
316700100729
316800100729         // Visualizzazione del SFL (se ci sono dati)
316900100805            SflDsp10_N = (S10nrr <= *zeros);
317000100729
317100100805         // Attivazione (eventuale) del SFLEND
317200100805            SflEnd10=  *on;
317300100729
317400100729         // Posizionamento cursore al 1° rcd della pagina
317500100805            if  S10nrr > *zero;
317600100805                c10rcd = 1;
317700100805            else;
317800100805                clear  C10rcd;
317900100805            endif;
318000100729
318100100729       ENDSR;
318200100729
318300100118
318400100805       //--------------------------------------------------------------
318500100805       //?Controllo videata scelta Codice cliente
318600100805       //--------------------------------------------------------------
318700100805       BEGSR Sr_Ctrs10 ;
318800100805
318900100805           reset errMessage ;
319000100805           reset errGenerico;
319100100805           reset Poscuropz10 ;
319200100805           clear numrec10 ;
319300100805           clear scelta_rcd ;
319400100805           clear v10msg  ;
319500100805
319600100805        // Eseguo i controlli sul codice cliente da utilizzare per la convalida
319700100805
319800100805       numrec10 = 1 ;
319900100805       chain numrec10 Ta56WS10 ;
320000100805
320100100805         DOW %found(TNTA56D);
320200100805
320300100805           C10rcd = S10nrr;
320400100805
320500100805           SELECT;
320600100805
320700100805         // verifico se fatta + di una scelta
320800100805          When w10opz <> ' ' and w10opz <> '1' ;
320900100805               errMessage  = *on;
321000100805               errGenerico = *on;
321100100805               PosCurOpz10 = *on;
321200100805               V10msg = $Msg(02);
321300100805
321400100805         // se ho fatto una scelta verifico se ne ho fatta più di una
321500100805          When w10opz = '1' and scelta_rcd  > 0 ;
321600100805               errMessage  = *on;
321700100805               errGenerico = *on;
321800100805               PosCurOpz10 = *on;
321900100805               V10msg = $Msg(29);
322000100805
322100100805         // se ho fatto una scelta la memorizzo
322200100805          When w10opz = '1' and scelta_rcd  = 0 ;
322300100805               eval scelta_rcd = numrec10 ;
322400100805
322500100805          ENDSL ;
322600100805
322700100805           if  errMessage = *on  or  errGenerico = *on;
322800100805             leavesr ;
322900100805           endif;
323000100805
323100100805           numrec10 += 1;
323200100805           chain numrec10 Ta56WS10 ;
323300100805          enddo;
323400100805
323500100805       // se non ho scelto nulla
323600100805          If scelta_rcd = 0 ;
323700100805             errMessage  = *on;
323800100805             errGenerico = *on;
323900100805             PosCurOpz10 = *on;
324000100805             V10msg = $Msg(30);
324100100805          endif ;
324200100805
324300100805       ENDSR;
324400100805
324500100805       //--------------------------------------------------------------
324600100805       //?Effettuata scelta codice per la convalida F6
324700100805       //--------------------------------------------------------------
324800100805       BEGSR F06W10  ;
324900100805
325000100805          clear fil_cmm ;
325100100805
325200100805       // aggancio il record del subfile 10 scelto
325300100805          chain scelta_rcd ta56ws10 ;
325400100805
325500100805          If not %found(TNTA56D) ;
325600100805             $video = 'S5';
325700100805             leavesr ;
325800100805          Endif ;
325900100805
326000100805       // verifico che codice cliente devo attribuire
326100100805
326200100805           SELECT ;
326300121128
326400121129       //?Scelta di nuovo codice prenotato
326500121128           WHEN W10Hpre = 'S';
326600121128             wInzW12 = *on;
326700121128             exsr gesW12;
326800121128         //?se torna errore generico vuol dire che è stato dato
326900121128         //?F12 e quindi devo tornare alla videata di selezione codici
327000121128             IF  ErrGenerico;
327100121128               leavesr;
327200121128             ENDIF;
327300121128         //?se tutto ok vado avanti con la creazione del nuovo codice
327400121128             c05ksc = W12ksc;
327500121129             IF  Vis_comm;
327600121129               comcon = %dec(W12cmm:7:0);
327700121129             ELSE;
327800121129               comcon = C01cmm;
327900121129             ENDIF;
328000100805
328100100805       // se codice commerciale diverso da 0 e codice cliente =  a 0
328200100805       // ricerco un nuovo codice con commerciale passato (trattativa)
328300100805           When w10hcmm > 0 and w10hksc = 0 ;
328400100805                fil_cmm = %dec(%subst(%EDITC(W10hcmm:'X'):1:3):3:0);
328500100805                exsr Sr_Newcli ;
328600100805                c05ksc = newksc ;
328700100818                comcon = c01cmm ;
328800100805
328900100805
329000100805       // se codice commerciale uguale a 0 e codice cliente =  a 0
329100100805       // ricerco un nuovo codice con commerciale nuovo da richiedere
329200100805           When w10hcmm = 0 and w10hksc = 0 ;
329300100816                  exsr Gesw09 ;
329400100818        // se ritorna l'errore generico acceso vuol dire che è stato dato
329500100817        // f12 e devo tornare alla video di selezione codici
329600100817                  If errGenerico = *on ;
329700100817                     leavesr ;
329800100817                  Endif ;
329900100817
330000100817        // se tutto ok recupero la filiale del commerciale inserito per
330100100817        // recuperare nuovo codice cliente
330200100817                  If w09cmm <> *blanks and w09cmm <> *zeros;
330300100817                     fil_cmm = %dec(%subst(W09cmm:1:3):3:0);
330400100816                     exsr Sr_Newcli ;
330500100816                     c05ksc = newksc ;
330600100818                     comcon = %dec(w09cmm:7:0);
330700100816                  endif ;
330800100805
330900100805       // se codice commerciale = a 0 e codice cliente diverso da 0
331000100805       // appoggio le offerte da convalidare su di una anagrafica già esistente
331100100816           When w10hcmm = 0 and w10hksc > 0 ;
331200100805                c05ksc = W10hksc ;
331300100818                comcon = c01cmm ;
331400100805
331500100805           Endsl ;
331600100805
331700100805         // Aggancio anagrafiche provvisorie e non
331800100805            exsr Sr_AggAna ;
331900100805         // verifico il codice pagamento
332000100805            Exsr Sr_CodPag ;
332100100805
332200100805             $video = 'S5';
332300100805             $inzs05 = *on ;
332400100805
332500100805       ENDSR;
332600100805
332700100118       //--------------------------------------------------------------
332800100118       //?Gestione Videata Convalida Offerta
332900100118       //--------------------------------------------------------------
333000100118       BEGSR GesS05;
333100100118
333200100118         // Inizializzazione videata
333300100118            exsr Inzs05;
333400100204
333500100204         Dow $fine5 = *off ;
333600100120         // Emissione Testata e Piede con tasti funzionali abilitati
333700100204           write  TA56T01;
333800100204           write  TA56P05;
333900100120
334000100120
334100100120         // Emissione videata
334200100120         exfmt  TA56C05;
334300100120         reset errMessage;
334400100120         reset errGenerico;
334500100120         clear V1Dmsg;
334600100120
334700100120         SELECT;
334800100118
334900100118         // - F12=Ritorno
335000100118           WHEN  dsp_aid = c_F12;
335100100118             $video = 'S1';
335200100204             leave ;
335300100424
335400100424         // - F11=Attribuisco un nuovo codice
335500100424           WHEN  dsp_aid = c_F11;
335600100805             exsr Gess10 ;
335700100805                 iter   ;
335800100118
335900100120         // - F06=Convalida
336000100118           WHEN  dsp_aid = c_F06  ;
336100100120             Exsr SR_ctrS05 ;
336200100118             if  errGenerico = *on;
336300100204                 iter   ;
336400100118             endif;
336500100118
336600100203         // aggiorno i record per la convalida
336700100121             exsr Sr_F6ConvOff;
336800100312
336900100224             $fine5 = *on ;
337000100118
337100100118           OTHER ;
337200100118
337300100120             Exsr SR_ctrs05 ;
337400100118             if  errGenerico = *on;
337500100204                 iter   ;
337600100118             endif;
337700100118
337800100118         ENDSL;
337900100118
338000100204         ENDDO;
338100100204
338200100118       ENDSR;
338300100118
338400100118       //--------------------------------------------------------------
338500100118       //?Carico videata Convalida Offerte  PARTE ALTA
338600100118       //--------------------------------------------------------------
338700100118       BEGSR Inzs05 ;
338800100118
338900100224            $fine5 = *off ;
339000100518            wforzacon = *off ;
339100100119         // inizializzo alcuni campi chiave
339200100119            acokut = 1 ;
339300100119            acokcc = dutkci ;
339400100424            clear coldksc ;
339500100424            clear c05ksc ;
339600100424            disatt_f11 = *off ;
339700100804            protez_ksc = *on ;
339800100119
339900100118         // proporre il codice cliente  in base al tipo visita
340000100118         // ed alla presenza o meno di altri codici a cui appoggiarsi
340100100119
340200100119         // se si tratta di una visita di tipo nuovo e non è mai stata
340300100119         // convalidata una offerta prima ad ora propongo un nuovo codice
340400100119         // e lo proteggo
340500100119
340600100424            If vistpv = 'N'  and visesi = ' ' ;
340700100805               fil_cmm = %dec(%subst(%EDITC(c01cmm:'X'):1:3):3:0);
340800100119               exsr Sr_NewCli ;
340900100424               c05ksc = newksc ;
341000100818               comcon = c01cmm ;
341100100510               PoscurKsc = *off ;
341200100518               wforzacon = *off ;
341300100424            endif ;
341400100805
341500100805         // se si tratta di una visita di tipo nuovo ed è  stata
341600100805         // convalidata una offerta  propongo l'ultimo codice
341700100805         // convalidato
341800100805
341900100805            If vistpv = 'N'  and visesi = 'P' ;
342000100805               exsr sr_reccli ;
342100100805               c05ksc = oldksc ;
342200100818               comcon = c01cmm ;
342300100805               PoscurKsc = *off ;
342400100805               wforzacon = *off ;
342500100805            endif ;
342600100119
342700100424         // se visita non nuova imposto il codice cliente della trattativa
342800100119         // per adesso protetto
342900100119            If vistpv <> 'N' ;
343000100119               c05ksc = c01ksc ;
343100100818               comcon = c01cmm ;
343200100119            endif ;
343300100518
343400100119         // Come data reale affidamento propongo la data di presunto affidamento
343500100119         // o la data del giorno ?
343600100119            c05dra = *date  ;
343700100118
343800100424         // Aggancio anagrafiche provvisorie e non
343900100424            exsr Sr_AggAna ;
344000100119         // verifico il codice pagamento
344100100119            Exsr Sr_CodPag ;
344200100120
344300100121         // Visualizzazione del SFL (se ci sono dati)
344400100121         SflDsp5_N = (S05nrr <= *zeros);
344500100121
344600100121         // Attivazione (eventuale) del SFLEND
344700100121         SflEnd5 =  *on;
344800100121
344900100121         // Posizionamento cursore al 1° rcd della pagina
345000100121         if  S05nrr > *zero;
345100100121             c05rcd = 1;
345200100121         else;
345300100121           clear  C05rcd;
345400100121         endif;
345500100121
345600100118       ENDSR;
345700100805
345800100805       //--------------------------------------------------------------
345900100805       //?Recupero l'ultimo codice convalidato
346000100805       //--------------------------------------------------------------
346100100805       BEGSR Sr_RecCli ;
346200100805
346300100805         exec sql
346400100805          declare C2  cursor for
346500100805          select vofksc
346600100805          from tivof00f where vofnrv = :visnrv and vofeso = 'C' and
346700110112          vofdco > 0  and vofctr <> 999 order by vofdco desc ;
346800100805
346900100805         exec sql
347000100805           open C2;
347100100805           IF sqlcod = 0;
347200100805           exec sql
347300100805             fetch next from C2  into :vofksc;
347400100805             IF sqlcod = 100 or sqlcod < 0;
347500100805               leavesr ;
347600100805             endif;
347700100805            Oldksc = vofksc ;
347800100805           ENDIF;
347900100805
348000100805         exec sql close C2 ;
348100100805
348200100805       ENDSR;
348300100805
348400100424       //--------------------------------------------------------------
348500100424       //?Aggancio Anagrafiche provvisorie e non
348600100424       //--------------------------------------------------------------
348700100424       BEGSR Sr_AggAna ;
348800100424
348900100424         // Aggancio anagrafiche provvisorie e non
349000100424         select ;
349100100424
349200100424         // se tipo visita N e mai esistata
349300100424         When vistpv = 'N' and visesi = ' ' ;
349400100804            chain    (acokut:acokcc:c01nrv) tfaco00f ;
349500100424            If %found (tfaco00f) ;
349600100804               chain    (acokut:acokcc:c01nrv) tfind00f ;
349700100804               chain    (acokut:acokcc:c01nrv) tfclp00f ;
349800100424            endif ;
349900100424
350000100424         // se tipo visita N ed esitata visualizzo anagrafica cod proposto
350100100424         When vistpv = 'N' and visesi = 'P' ;
350200100804            chain    (acokut:acokcc:c05ksc) cnaco00f ;
350300100506            If %found (cnaco00f) ;
350400100804               chain    (acokut:acokcc:c05ksc) cnind00f ;
350500100804               chain    (acokut:acokcc:c05ksc) cnclp00f ;
350600100506            else ;
350700100804               chain    (acokut:acokcc:c01nrv) tfaco00f ;
350800100506               If %found (tfaco00f) ;
350900100804                  chain    (acokut:acokcc:c01nrv) tfind00f ;
351000100804                  chain    (acokut:acokcc:c01nrv) tfclp00f ;
351100100506               endif ;
351200100424            endif ;
351300100424
351400100424         // se tipo visita diversa da N ed esito P anagrafica cod proposto
351500110113         When vistpv <> 'N' and visesi = 'P' ;
351600100804               chain    (acokut:acokcc:c05ksc) cnaco00f ;
351700100424               If %found (cnaco00f) ;
351800100804                  chain    (acokut:acokcc:c05ksc) cnind00f ;
351900100804                  chain    (acokut:acokcc:c05ksc) cnclp00f ;
352000100506               else ;
352100100804                  chain    (acokut:acokcc:c01nrv) tfaco00f ;
352200100506                  If %found (tfaco00f) ;
352300100804                     chain    (acokut:acokcc:c01nrv) tfind00f ;
352400100804                     chain    (acokut:acokcc:c01nrv) tfclp00f ;
352500100506                  else  ;
352600100804                     chain    (acokut:acokcc:c01ksc) cnaco00f ;
352700100506                     If %found (cnaco00f) ;
352800100804                        chain    (acokut:acokcc:c01ksc) cnind00f ;
352900100804                        chain    (acokut:acokcc:c01ksc) cnclp00f ;
353000100506                     endif;
353100100506                 endif ;
353200100424               endif;
353300100424
353400100424         // se tipo visita diversa da N e no esito prima tfaco poi cnaco
353500110113         When vistpv <> 'N' and visesi = ' ' ;
353600100804            chain    (acokut:acokcc:c01nrv) tfaco00f ;
353700100424            If %found (tfaco00f) ;
353800100804               chain    (acokut:acokcc:c01nrv) tfind00f ;
353900100804               chain    (acokut:acokcc:c01nrv) tfclp00f ;
354000100424            else  ;
354100100804               chain    (acokut:acokcc:c01ksc) cnaco00f ;
354200100424               If %found (cnaco00f) ;
354300100804                  chain    (acokut:acokcc:c01ksc) cnind00f ;
354400100804                  chain    (acokut:acokcc:c01ksc) cnclp00f ;
354500100424               endif;
354600100424            endif ;
354700100424
354800100424        Endsl ;
354900100424
355000100424       ENDSR;
355100100424
355200100119       //--------------------------------------------------------------
355300100119       //?Controllo condizioni pagamento
355400100119       //--------------------------------------------------------------
355500100119       BEGSR Sr_CodPag ;
355600100119
355700100122            Vis_pagamento = *off;
355800100122
355900100119            clear dsfa ;
356000100119            Tblkut = 1 ;
356100100119            tblcod = 'FA';
356200100316            tblkey = %subst(indcdp:4:3) + '1' ;
356300100119            chain (Tblkut:tblcod:tblkey) tabel00f ;
356400100119
356500100119            If %found(tabel00f) ;
356600100119               dsfa = tbluni ;
356700100122            else ;
356800100122               Vis_pagamento = *on ;
356900100119            endif ;
357000100119
357100100119         // Verifico se pagamento con ricevuta bancaria
357200100119         // obbligatorio codice ABI CAB e descrizione bancaria
357300100316            If §fatpg = '1' and (indabi = 0 and indcab = 0)
357400100316               or (indbna = *blank and (indabi<>0 or indcab<>0));
357500100119               Vis_pagamento = *on ;
357600100316               c05cdp = %subst(indcdp:4:3);
357700100119               c05abi = indabi ;
357800100119               c05cab = indcab ;
357900150417               savcdp = C05cdp;
358000100120         // aggancio anagrafico banche per decodifica banca
358100100119               chain (indabi:indcab) cnabi00f ;
358200100119               If %found(cnabi00f) and abiann <> '*' ;
358300100119         // richiamo la routine di decodifica banca d'appoggio
358400100119                  exsr sr_Crebna ;
358500100119               endif ;
358600100119            endif;
358700100119
358800100119         // Imposto la Partita IVA
358900100121            If %subst(indiva:1:2) >= '00' ;
359000100119               c05ivai =  indiva ;
359100100119            Else ;
359200100312               c05ivai = %subst(indiva:3:14) ;
359300100312               c05ivae = %subst(indiva:1:2) ;
359400100119            endif ;
359500100119
359600100119            wiva = indiva ;
359700100120            saviva = indiva ;
359800100119
359900100119         // Imposto il Codice Fiscale
360000100119
360100100119            c05cdf = indcdf ;
360200100119            Wcdf = indcdf ;
360300100120            Savcdf = indcdf ;
360400100119
360500100119         // se il sottoconto intestazione fattura è diverso dal codice cliente
360600100119         // devo reperire la nazione del sottoconto fatturazione
360700100119
360800100119            if clpscf>0 and clpscf<>visksc ;
360900100119
361000100119               clear  wsta  ;
361100100119               clear  tibs69ds;
361200100119               I69kin = clpscf;
361300100119               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
361400100119               wtibs69 = *On  ;
361500100119               if  o69err=*blanks and w_indflg=*blanks ;
361600100119                   wsta=w_indsta;
361700100119               endif ;
361800100119            else  ;
361900100119               wsta=indsta ;
362000100119            endif ;
362100100119
362200100119          // se cliente estero non visualizzo il codice fiscale
362300100119            Vis_CodFisc=(indsta = *blanks and wsta = *blanks);
362400100119
362500100119       ENDSR ;
362600100119
362700100119       //--------------------------------------------------------------
362800100119       //?Decodifica Banca d'appoggio
362900100119       //--------------------------------------------------------------
363000100119       BEGSR Sr_Crebna ;
363100100119
363200100119          If  abiabi <> 99999 or abicab <> 99999 ;
363300100119                Clear wagenzia ;
363400100119                Clear wbanca ;
363500100119                wbanca = abiist ;
363600100119         // Compongo il nome della banca con località o comune
363700100119            If  abiage = *Blanks ;
363800100119                Select ;
363900100119                When  abiloc <> *Blanks ;
364000100119                      wagenzia = abiloc ;
364100100119                When  abicom <> *Blanks ;
364200100119                      wagenzia = abicom ;
364300100119                EndSl ;
364400100119         // Compongo il nome della banca con agenzia + località o comune
364500100119            Else ;
364600100119         // Cerco il primo byte vuoto dell'agenzia
364700100119                wpos = %checkr(' ':abiage);
364800100119
364900100119                wagenzia = %subst(abiage:1:wpos);
365000100119                wpos1 = wpos + 2 ;
365100100119                wpos2 = 16 - wpos1 ;
365200100119         // Se wpos2 inferiore a zero lo imposto a 0
365300100119                If wpos2 < *Zeros ;
365400100119                   Clear wpos2 ;
365500100119                EndIf ;
365600100119         // Se posso aggiungere anche solo 1 carattere procedo
365700100119                If wpos > 1 and wpos < 16 ;
365800100119         // Cerco di riempire quello che resta con la località o il comune
365900100119                   Select ;
366000100119                   When  abiloc <> *Blanks ;
366100100119                         wagenzia = %trim(wagenzia) + ' ' +
366200100119                                    %subst(abiloc:1:wpos2);
366300100119                   When  abicom <> *Blanks ;
366400100119                         wagenzia = %trim(wagenzia) + ' ' +
366500100119                                    %subst(abicom:1:wpos2);
366600100119                   EndSl ;
366700100119                EndIf ;
366800100119
366900100119            EndIf ;
367000100120
367100100120         // imposto il campo a video
367200100120            c05bna = wbanca + wagenzia ;
367300100120
367400100119          EndIf;
367500100119
367600100119       ENDSR ;
367700100119
367800100816       //--------------------------------------------------------------
367900100816       //?Routine di richiesta nuovo commerciale
368000100816       //--------------------------------------------------------------
368100100816       BEGSR GesW09;
368200100816
368300100816         // Inizializzazione videata
368400100816            exsr Inzw09;
368500100816
368600100816         // Gestione video
368700100816         DOW $Fine9 = *off;
368800100816         // Emissione Finestra di accettazione offerta
368900100816             exfmt  TA56W09;
369000100816             reset errMessage;
369100100816             reset errGenerico;
369200100816             clear w09msg;
369300100816
369400100816         SELECT;
369500100816
369600100816         // - F12=Ritorno
369700100816           WHEN  dsp_aid = c_F12;
369800100816             $Fine9 = *on ;
369900100817             errGenerico = *on;
370000100816
370100100816         // - F06=Accetta
370200100816           WHEN  dsp_aid = c_F06  ;
370300100816             Exsr SR_ctrw09 ;
370400100816             if  errGenerico = *on;
370500100816                 iter ;
370600100816             endif;
370700100816
370800100816
370900100816         // riemetto la  videata
371000100816             $Fine9 = *on ;
371100100816             $video  = 'S5' ;
371200100816
371300100816           OTHER ;
371400100816
371500100816             Exsr SR_ctrw09 ;
371600100816             if  errGenerico = *on;
371700100816                 iter   ;
371800100816             endif;
371900100816
372000100816         ENDSL;
372100100816
372200100816         ENDDO;
372300100816
372400100816       ENDSR;
372500100816
372600100816       //--------------------------------------------------------------
372700100816       //?Carico videata Richiesta Commerciale
372800100816       //--------------------------------------------------------------
372900100816       BEGSR Inzw09 ;
373000100816
373100100817          clear w09cmm ;
373200100817          clear w09cmmd ;
373300100816          $fine9 = *off ;
373400100816
373500100816       ENDSR;
373600100816
373700100816       //--------------------------------------------------------------
373800100816       //?Controllo videata Nuovo commerciale
373900100816       //--------------------------------------------------------------
374000100816       BEGSR Sr_CtrW09 ;
374100100816
374200100817         // Controllo codice commerciale (? Ricerca)
374300100817           If    %scan('?' : W09cmm) > 0  ;
374400100817             errGenerico = *on;
374500100917          // se esiste  il commerciale esterno ricerco con la filiale
374600100917          // di quest'ultimo
374700130806             clear  TRMK43ds;
374800100917               If  c01cmm > *zeros;
374900130806                 iMK43fil = %int(%subst(%editc(c01cmm:'X'):1:3));
375000100917               endif ;
375100130806             TRMK43R (kpjba : TRMK43ds);
375200130806             if  oMK43err = *off  and  oMK43fxx = *blank;
375300130806               w09cmm  = %editc( oMK43cmm : 'X' );
375400130806               w09cmmd = oMK43des;
375500130806             endif;
375600100817             leavesr;
375700100817           endif;
375800100817
375900100817         // Controllo codice commerciale esterno
376000100817           If    w09cmm = *blanks or w09cmm = *zeros;
376100100817             errMessage  = *on;
376200100817             errGenerico = *on;
376300100817             W09msg = $Msg(25);
376400100817             leavesr;
376500100817           endif;
376600100817
376700100817           IF  %check(digits:w09cmm) > 0;
376800100817             errMessage  = *on;
376900100817             errGenerico = *on;
377000100914             w09msg = $Msg(31);
377100100817             leavesr;
377200100817           ENDIF;
377300100817
377400130806             k_CMMcod = %int(w09cmm);
377500130806             chain  %kds(k_azcmm01)  AZCMM000;
377600130806             if  NOT  %found(AZCMM01L);
377700100817               errMessage  = *on;
377800100817               errGenerico = *on;
377900100914               w09msg = $Msg(31);
378000100817               leavesr;
378100100817             else;
378200130806               w09cmmd = CMMdes;
378300100817             endif;
378400111129
378500111129         //?commerciale valido no vari o clienti inattivi
378600130806           IF  CMMpar <> *blanks;
378700111129             ErrMessage  = *on;
378800111129             ErrGenerico = *on;
378900111129             w09msg = $Msg(31);
379000111129             leavesr;
379100111129           ENDIF;
379200111129
379300111129         //?commerciale valido data inizio e fine attività
379400130806           IF  CMMdtIni > datacor8  or
379500130806               CMMdtFin < datacor8;
379600111129             ErrMessage  = *on;
379700111129             ErrGenerico = *on;
379800111129             w09msg = $Msg(31);
379900111129             leavesr;
380000111129           ENDIF;
380100100817
380200100817
380300100817           // verifico se ha lo stesso commerciale unificante del commerciale
380400100817           // esterno della trattativa
380500130806             If sav_rgf <> CMMuni ;
380600100817               ErrMessage  = *on;
380700100817               ErrGenerico = *on;
380800100817               w09msg      = $Msg(26);
380900100817               leavesr;
381000100817             ENDIF;
381100100816
381200100816       ENDSR;
381300121128
381400121128       //--------------------------------------------------------------
381500121128       //?Routine di richiesta nuovo codice già prenotato.
381600121128       //--------------------------------------------------------------
381700121128       BEGSR GesW12;
381800121128
381900121128       //?Inizializzazione videata
382000121128         IF  wInzW12;
382100121128           exsr InzW12;
382200121128           wInzW12 = *off;
382300121128         ENDIF;
382400121128
382500121128       //?Gestione video
382600121128         DOW  not wFine12;
382700121128         //?Emissione Finestra di richiesta codice prenotato
382800121128           exfmt TA56W12;
382900121128           reset errMessage;
383000121128           reset errGenerico;
383100121128           clear W12msg;
383200121128
383300121128           SELECT;
383400121128
383500121128           //?- F12=Ritorno
383600121128             WHEN  dsp_aid = c_F12;
383700121128               wFine12 = *on ;
383800121128               ErrGenerico = *on;
383900121128
384000121128           //?- F06=Conferma
384100121128             WHEN  dsp_aid = c_F06  ;
384200121128               exsr CtrW12;
384300121128               IF  ErrGenerico;
384400121128                 iter;
384500121128               ENDIF;
384600121128             //?Riemetto la  videata
384700121128               wFine12 = *on;
384800121128               $video  = 'S5';
384900121128
385000121128             OTHER;
385100121128
385200121128           //?Controllo dati videata
385300121128               exsr CtrW12;
385400121128               IF  ErrGenerico;
385500121128                 iter;
385600121128               ENDIF;
385700121128
385800121128           ENDSL;
385900121128         ENDDO;
386000121128
386100121128       ENDSR;
386200121128
386300121128       //--------------------------------------------------------------
386400121128       //?Imposto i dati videata Nuovo codice già prenotato.
386500121128       //--------------------------------------------------------------
386600121128       BEGSR InzW12;
386700121128
386800121128          clear W12ksc;
386900121128          clear W12cmm;
387000121128          clear W12cmmd;
387100121128          wFine12 = *off;
387200121128          Vis_comm = *off;
387300121128
387400121128       ENDSR;
387500121128
387600121128       //--------------------------------------------------------------
387700121128       //?Controllo videata Nuovo codice già prenotato.
387800121128       //--------------------------------------------------------------
387900121128       BEGSR CtrW12;
388000121128
388100121128       //?Controllo codice cliente
388200121128         IF  W12ksc = 0;
388300121128           ErrGenerico = *on;
388400121128           ErrMessage  = *on;
388500121129           PosCurKSC12 = *on;
388600121128           W12msg = 'Immettere il codice cliente';
388700121128           leavesr;
388800121128         ENDIF;
388900121128
389000130806         chain  (1:DUTkci:W12ksc) CNACO00F;
389100121128         IF  %found(CNACO00F);
389200121128           ErrGenerico = *on;
389300121128           ErrMessage  = *on;
389400121129           PosCurKSC12 = *on;
389500121128           W12msg = 'Codice già presente in anagrafica';
389600121128           leavesr;
389700121128         ENDIF;
389800121128
389900121128       //?Se ho visualizzato il commerciale lo controllo
390000121128         IF  Vis_comm;
390100121128           exsr CtrW12cmm;
390200121128           leavesr;
390300121128         ENDIF;
390400121128
390500121128       //?Se cliente è ok controllo che il commerciale della trattativa
390600121128       //?abbia il cod. filiale = alla fil. cod. cliente
390700121128       //?in caso contrario devo richiedere il cod. commerciale
390800121128         IF  %subst(%editc(W12ksc:'X'):1:3) <>
390900121128             %subst(%editc(C01cmm:'X'):1:3);
391000121128           ErrGenerico = *on;
391100121129           PosCurCMM12 = *on;
391200121128           Vis_comm = *on;
391300121128           leavesr;
391400121128         ENDIF;
391500121128
391600121128       ENDSR;
391700121128
391800121128       //--------------------------------------------------------------
391900121128       //?Controllo commerciale videata nuovo codice già prenotato.
392000121128       //--------------------------------------------------------------
392100121128       BEGSR CtrW12cmm;
392200121128
392300121128       //?Ricerca codice commeriale
392400121128         IF  %scan('?' : W12cmm) > 0;
392500121128           ErrGenerico = *on;
392600130806           clear  TRMK43ds;
392700121128         //?ricerco con filiale cliente immesso a video
392800130806           iMK43fil = %int(%subst(%editc(W12ksc:'X'):1:3));
392900130806           TRMK43R (kpjba : TRMK43ds);
393000130806           if  oMK43err = *off  and  oMK43fxx = *blank;
393100130806             w12cmm  = %editc( oMK43cmm : 'X' );
393200130806             w12cmmd = oMK43des;
393300130806           endif;
393400121128           leavesr;
393500121128         ENDIF;
393600121128
393700121129       //?Campo obbligatorio
393800121128         IF  W12cmm = *blanks or W12cmm = *zeros;
393900121128           ErrMessage  = *on;
394000121128           ErrGenerico = *on;
394100121129           PosCurCMM12 = *on;
394200121128           W12msg = $Msg(25);
394300121128           leavesr;
394400121128         ENDIF;
394500121128
394600121129       //?Accetto solo numeri
394700121128         IF  %check(digits:W12cmm) > 0;
394800121128           ErrMessage  = *on;
394900121128           ErrGenerico = *on;
395000121129           PosCurCMM12 = *on;
395100121128           W12msg = $Msg(31);
395200121128           leavesr;
395300121128         ENDIF;
395400121129
395500121129       //?Deve essere della stessa filiale del cliente
395600121129         IF  %subst(%editc(W12ksc:'X'):1:3) <>
395700121129             %subst(W12cmm:1:3);
395800121129           ErrMessage  = *on;
395900121129           ErrGenerico = *on;
396000121129           PosCurCMM12 = *on;
396100121129           W12msg = $Msg(31);
396200121129           leavesr;
396300121129         ENDIF;
396400121128
396500121129       //?Controllo validità codice
396600130806         k_CMMcod = %int(w12cmm);
396700130806         chain  %kds(k_azcmm01)  AZCMM000;
396800130806         IF  NOT  %found(AZCMM01L);
396900121128           ErrMessage  = *on;
397000121128           ErrGenerico = *on;
397100121129           PosCurCMM12 = *on;
397200121128           W12msg = $Msg(31);
397300121128           leavesr;
397400121128         ENDIF;
397500130806         W12cmmd = CMMdes;
397600121128
397700121128       //?Commerciale valido no vari o clienti inattivi
397800130806         IF  CMMpar <> *blanks;
397900121128           ErrMessage  = *on;
398000121128           ErrGenerico = *on;
398100121129           PosCurCMM12 = *on;
398200121128           W12msg = $Msg(31);
398300121128           leavesr;
398400121128         ENDIF;
398500121128
398600121128       //?commerciale valido data inizio e fine attività
398700130806         IF  CMMdtIni > datacor8  or
398800130806             CMMdtFin < datacor8;
398900121128           ErrMessage  = *on;
399000121128           ErrGenerico = *on;
399100121129           PosCurCMM12 = *on;
399200121128           W12msg = $Msg(31);
399300121128           leavesr;
399400121128         ENDIF;
399500121128
399600121128       //?verifico se ha lo stesso unificante del commerciale
399700121128       //?della trattativa
399800130806         IF  sav_rgf <> CMMuni;
399900121128           ErrMessage  = *on;
400000121128           ErrGenerico = *on;
400100121129           PosCurCMM12 = *on;
400200121128           W12msg = $Msg(26);
400300121128           leavesr;
400400121128         ENDIF;
400500121128
400600121128       ENDSR;
400700100816
400800100118       //--------------------------------------------------------------
400900100118       //?Controllo videata Convalida  Offerte
401000100118       //--------------------------------------------------------------
401100100118       BEGSR Sr_Ctrs05 ;
401200100118
401300100121           reset errMessage ;
401400100121           reset errGenerico;
401500100121           reset PosCurDra  ;
401600100121           reset PosCurIva  ;
401700100121           reset PosCurCdf  ;
401800100121           reset PosCurCdp  ;
401900100121           reset PosCurAbi  ;
402000100121           reset PosCurCab  ;
402100100424           reset PosCurKsc  ;
402200100121           clear v1dmsg ;
402300100424
402400100120        // Controllo data reale affidamento
402500100120           If    C05Dra <> 0;
402600100118             clear wlbdat;
402700100120             g02dat = C05Dra;
402800100118             xsrda8(wlbdat);
402900100118             if g02err = '1';
403000100118               errMessage  = *on;
403100100118               errGenerico = *on;
403200100120               PosCurDra   = *on;
403300100120               v1dmsg = $Msg(04);
403400100118               leavesr;
403500100118             endif;
403600100118
403700100118        // Verfiico che non sia inferiore all'anno 2000 e superiore al 2039
403800100118           If    G02Inv < 20000101 or G02Inv > 20391231 ;
403900100118               errMessage  = *on;
404000100118               errGenerico = *on;
404100100120               PosCurDra   = *on;
404200100120               v1dmsg = $Msg(04);
404300100118               leavesr;
404400100120            endif;
404500100118
404600100120             c05dra = g02dat;
404700100120             wrkdra = g02inv;
404800100118           else ;
404900100118             errMessage  = *on;
405000100118             errGenerico = *on;
405100100120             PosCurDra   = *on;
405200100120             V1dmsg = $Msg(04);
405300100118             leavesr;
405400100118           endif;
405500100118
405600100120        // Partita IVA obbligatoria
405700100120        // Non è obbligatoria se nazione Italia e sottoconto intestazione fat-
405800100120        // tura ha nazione estera
405900151015        // Controllo la Partita IVA
406000151015           wmod = 'P' ;
406100151015           wiva = c05ivae + c05ivai ;
406200151015           ExSr Sr_ctriva ;
406300151015           If errMessage  = *on;
406400151015              leavesr;
406500151015           Endif ;
406600151015
406700100120         If c05ivai = *blanks and c05ivae = *blanks ;
406800151015
406900151015              If  (indsta<>*blanks or wsta=*blanks)
407000151015                  and xcfivaflg<>9 ;
407100100120                  errMessage  = *on;
407200100120                  errGenerico = *on;
407300100120                  PosCurIva = *on ;
407400100120                  V1dmsg = $Msg(07);
407500100120                  leavesr;
407600100120              endif;
407700100120           Else;
407800100120        // se non impostato devo riportare il codice iso della partita iva che
407900100120        // mi viene passato dal pgm di controllo
408000100121           if c05ivae <> %subst(xcfivapar: 1: 2)  ;
408100100121              c05ivae = %subst(xcfivapar: 1: 2) ;
408200100120              Blink_ivae = *on  ;
408300100120              leavesr ;
408400100120           endif ;
408500100120        // Errore forzabile se p.iva già presente su altro potenziale
408600100120        // se sul potenziale manca la p.iva
408700100120           IF cpocpo>0 and cpopiv=*blanks
408800100120                       and wiva<>saviva
408900100120                       and Wiva<>cod_forziva
409000100120                       and c05ivae<>'$$';
409100100120              clear savrag ;
409200100120              if c05ivae<>*blanks ;
409300100120                 codice=Wiva;
409400100120              else;
409500100120                 codice=c05ivai ;
409600100120              endif;
409700100120              exsr sr_PivAltroP ;
409800100120              exsr sr_msgiva ;
409900100120              If errMessage  = *on;
410000100120                 leavesr;
410100100120              Endif ;
410200100120           Endif ;
410300100120
410400100429        // Controllo se partita Iva presente su potenziale in contenzioso
410500100518           Exsr Sr_ctrstc ;
410600151015
410700151015         Endif ;
410800100429
410900100120        // Controllo il codice fiscale
411000100120         wmod = 'C' ;
411100100120         exsr sr_ctriva ;
411200100120         If errMessage  = *on;
411300100120            leavesr;
411400100120         Endif ;
411500100120
411600100120         If C05cdf = *blanks ;
411700100120
411800100120            if xcfivaflg<>9 and vis_codfisc = *on ;
411900100120               errMessage  = *on;
412000100120               errGenerico = *on;
412100100120               PosCurCdf   = *on;
412200100120               V1dmsg = $Msg(08);
412300100120               leavesr;
412400100120            endif;
412500100120
412600100120         else ;
412700100120
412800100120        // Errore forzabile se cod.fisc. già presente su altro potenziale
412900100120        // se sul potenziale manca la p.iva
413000100120            If ch1cpo>0 and cpocdf=*blanks
413100100120                        and C05cdf<>savcdf
413200100120                        and C05cdf<>cod_forzcdf ;
413300100120               clear savrag ;
413400100120               codice=c05cdf;
413500100120               exsr Sr_CdfAltroP ;
413600100120               exsr Sr_msgcdf  ;
413700100120               If errMessage  = *on;
413800100120                  leavesr;
413900100120               Endif ;
414000100120            Endif ;
414100100120         Endif;
414200100120
414300100120        // Se visualizzo le condizioni di pagamento eseguo anche i controlli
414400100120         If Vis_pagamento = *on ;
414500100120            exsr Sr_ctrPag ;
414600100120            If errMessage  = *on;
414700100120               leavesr;
414800100120            Endif ;
414900100120         Endif ;
415000120710
415100120710        //?Controllo se anagrafica provvisoria e non è allocata da altro
415200120710        //?utente, se così blocco la convalida, in questo modo evitiamo
415300120710        //?di avere degli aggiornamenti zoppi.
415400120710          exsr CtrAllocAna;
415500120710          IF  ErrMessage;
415600120710            leavesr;
415700120710          ENDIF;
415800100120
415900100120        // Eseguo i controlli sulle offerte da convalidare
416000100120         Exsr ctrsfl5 ;
416100100120         If errMessage  = *on;
416200100120            leavesr;
416300100120         Endif ;
416400100118
416500100118       ENDSR;
416600100118
416700100118       //--------------------------------------------------------------
416800100118       //?Pulizia SFL05
416900100118       //--------------------------------------------------------------
417000100118       BEGSR Pulsfl5;
417100100118
417200100118       // Pulizia subfile
417300100121         SflDsp5_N   = *on;
417400100121         SflDspCtl5_N= *on;
417500100118         write  TA56C05;
417600100121         SflDspCtl5_N= *off;
417700100118         SflEnd5     = *off;
417800100118
417900100118
418000100118         clear W_SflPag5;
418100100118         clear C05rcd;
418200100118         clear C05csr;
418300100118         clear S05nrr;
418400100118         clear V1Dmsg;
418500100118         errMessage  = *off;
418600100118         errGenerico = *off;
418700100118
418800100118       ENDSR ;
418900100118
419000100120       //--------------------------------------------------------------
419100100120       //?Controllo SFL05
419200100120       //--------------------------------------------------------------
419300100120       BEGSR Ctrsfl5;
419400100120
419500100120       // leggo il subfile e per ogni offerta verifico se su cliente
419600100120       // esistente la decorrenza e scadenza e se ci sono tariffe
419700100120       // particolari obsolete
419800100120
419900100128       numrec= 1 ;
420000100128       chain numrec Ta56S05 ;
420100100120
420200100128         DOW %found(TNTA56D);
420300100120
420400100120           C05rcd = S05nrr;
420500100120
420600100120         // verifico se con il coidce cliente proposto e codice offerta
420700100120         // ho già una offerta ed anche le sue date decorrenza e scadenza
420800100120           Chain (c05ksc:s05ctr) Tntam04l ;
420900100120           If %found(tntam04l) ;
421000100624              data_iso = %date(tamdst:*iso) ;
421100100624              data_iso = data_iso + %days(1);
421200100624              wdata8=%dec(data_iso);
421300130320
421400130320         //?Cerco la data ultima spedizione fatturata con la tariffa in
421500130320         //?modifica
421600130320            clear wDataSP;
421700130320            clear TASaas;
421800130320            clear TASmgs;
421900130320            setgt  (TAMKSC:TAMCTR) TITAS41C;
422000130320            readpe (TAMKSC:TAMCTR) TITAS41C;
422100130320            wDataSP = TASaas * 10000 + TASmgs;
422200130320
422300100120         // verifico le date decorrenza e scadenza se corrette
422400130320              if tamdst > sh5ddt or wdata8 <> sh5ddt
422500130320                 or sh5ddt < wDataSp;
422600100624         // emetto la finestra di richiesta se si desidera modificare
422700100624         // le date di scadenza della tariffa precedente
422800100624                 exsr  GesW07 ;
422900100708                 If errGenerico = *on;
423000100708                    leavesr ;
423100100708                 Endif ;
423200100624              endif ;
423300100120           endif ;
423400100120
423500100120        // verifico se l'offerta si porta dietro delle tariffe particolari
423600100120        // che non si utilizzano più
423700100120          xx = 1 ;
423800100120          dow  TParEli(XX)<>'  ' ;
423900100121               chain (c01nrv:s05ctr:sh5prg:tpareli(xx)) tiopt01l ;
424000100120               if  %found(tiopt01l) and tptatb=' ' ;
424100100120        // verifico la data di scadenza della tariffa che deve essere
424200100120        // <= alla data di eliminaziuone della tariffa particolare
424300100120                   if sh5dst>TPardat(XX) ;
424400100120        // Errore
424500100121                       V1dmsg=$msg(17) ;
424600100121                       %subst(v1Dmsg:9:3)= %editc(s05ctr:'X') ;
424700100121                       %subst(v1Dmsg:49:2)=tpareli(XX)  ;
424800100120
424900100120                       data_dmy= %date(tpardat(xx):*iso) ;
425000100121                       wdata  = %dec(data_dmy)  ;
425100100121                       %subst(v1dmsg:71:8) =(%editw(wdata:'  /  /  '));
425200100120                       data_dmy= %date(sh5dst:*iso) ;
425300100121                       wdata  = %dec(data_dmy)  ;
425400100121                       %subst(v1dmsg:22:8) =(%editw(wdata:'  /  /  '));
425500100120                       errMessage  = *on;
425600100120                       errGenerico = *on;
425700100120                       leavesr ;
425800100120                   endif;
425900100120               endif ;
426000100120
426100100120               xx += 1;
426200100120          enddo;
426300100121
426400100121           if  errMessage = *on  or  errGenerico = *on;
426500100121             leave;
426600100121           endif;
426700100121
426800100128           numrec += 1;
426900100128           chain numrec Ta56S05 ;
427000100120          enddo;
427100100120
427200100120       ENDSR;
427300100120
427400100120
427500100119       //--------------------------------------------------------------
427600100119       //?Recupero il primo codice cliente libero
427700100119       //--------------------------------------------------------------
427800100119       BEGSR Sr_NewCli ;
427900100119
428000100817        // pulisco il newksc
428100100817          clear newksc ;
428200100119        // preparo la chiave
428300100119          nufaaa = 0 ;
428400100805          nuffil = fil_cmm ;
428500100119          nufcnu = dutkci ;
428600100119          chain (nufaaa:nufcnu:nuffil) flnuf01l ;
428700100119          if not %found(flnuf01l);
428800100119             clear flnuf    ;
428900100119             nufaaa = 0 ;
429000100805             nuffil = fil_cmm ;
429100100119             nufcnu = dutkci ;
429200100119             write flnuf   ;
429300100119             chain (nufaaa:nufcnu:nuffil) flnuf01l ;
429400100119          endif ;
429500100424          Dow newksc = 0 ;
429600100119              nufnum +=1 ;
429700110303              if nufnum = 10000;
429800100119                 nufnum = 1 ;
429900100119              endif ;
430000100119
430100100119          // verifico se esiste già un cliente con questo codice
430200100119              acoksc = (nuffil*10000) + nufnum ;
430300100119              setll (acokut:acokcc:acoksc) cnaco00f ;
430400100119              if  not %equal(cnaco00f) ;
430500100424                  newksc = acoksc ;
430600100119                  update flnuf    ;
430700100119              endif ;
430800100119          enddo ;
430900100119
431000100119       ENDSR ;
431100100429
431200100429       //--------------------------------------------------------------
431300100429       //?Controllo stato del credito sullo stessa P.IVA
431400100429       //--------------------------------------------------------------
431500100429       BEGSR Sr_Ctrstc ;
431600100429
431700100429       // - controllo se c'è già un cliente con la stessa p.IVA
431800100429       //
431900100518if  1   If  C05Ivae =  '$$' ;
432000100429            leavesr ;
432100100429        endif ;
432200100429
432300100429        If  C05Ivae = *Blanks ;
432400100429            codice = c05ivai ;
432500100429        else ;
432600100429            codice = c05ivae + c05ivai ;
432700100429        endif ;
432800100518        If codice <> sav_codice ;
432900100518           wforzacon = *off ;
433000100518           sav_codice = codice ;
433100100518        endif ;
433200100518
433300100429        setll (acokut:acokcc:codice) cnind02l ;
433400100429
433500100429        //  non trovo nessun altra partita IVA esco
433600100429
433700100429        If  Not %Equal ;
433800100429            Leavesr ;
433900100429        EndIf ;
434000100429        reade (acokut:acokcc:codice) cnind02l ;
434100100429do  2   Dow not %eof(cnind02l);
434200100429        //  deve avere codice cliente diverso
434300100429            If  ww_indksc <> c05ksc ;
434400100429               clear  tibs69ds;
434500100429               I69kcp = WW_indksc ;
434600100429               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
434700100429               wtibs69 = *On  ;
434800100429        // errore forzabile per stato del credito trovato
434900100429        // emesso con window e f6 di conferma
435000100429                 If w_clpcon <> *Blanks and wforzacon = *Off ;
435100100429        // decodifico stato del credito
435200100429                    clear ds4w ;
435300100429                    Tblkut = 1 ;
435400100429                    tblcod = '4W';
435500100429                    tblkey = W_clpcon ;
435600100429                    w6ccon = %subst(w_clpcon:2:2) ;
435700100429                    chain (Tblkut:tblcod:tblkey) tabel00f ;
435800100429
435900100429                    if %found(tabel00f) and tblflg = *blanks ;
436000100429                       ds4w = tbluni ;
436100100429                    endIf ;
436200100429                    w6dcon = §4wdes ;
436300100429                    dow 1=1 ;
436400100429                        exfmt  ta56w06  ;
436500100518                        If dsp_aid = c_F06 ;
436600100429                           leave ;
436700100429                        endif ;
436800100429                    enddo ;
436900100429                    wforzacon = *On ;
437000100429                    leave ;
437100100429                 EndIf;
437200100429             EndIf;
437300100518          reade (acokut:acokcc:codice) cnind02l ;
437400100429         EndDo;
437500100429       ENDSR;
437600120710
437700120710       //--------------------------------------------------------------
437800120710       //?Controllo se anagrafica allocata
437900120710       //--------------------------------------------------------------
438000120710       BEGSR CtrAllocAna;
438100120712
438200120712       //?Con N. Trattativa su anagrafica provvisoria
438300120712         IF  C01nrv > 0;
438400120712         //?TFACO
438500120712           exec sql
438600120712             select rrn(TFACO00F) into :tfaco_rrn
438700120712             from   TFACO00F
438800120712             where  ACOkut = 1 and ACOkcc = 151 and
438900120712                    ACOksc = :C01nrv;
439000120712           IF  tfaco_rrn > 0;
439100120712             wRrn = tfaco_rrn;
439200120712             wFil = 'TFACO00F';
439300120712             exsr CallTrul82;
439400120712             IF  UL82§sts = 'A';
439500120712               leavesr;
439600120712             ENDIF;
439700120712           ENDIF;
439800120712         //?TFIND
439900120712           exec sql
440000120712             select rrn(TFIND00F) into :tfind_rrn
440100120712             from   TFIND00F
440200120712             where  INDkut = 1 and INDkcc = 151 and
440300120712                    INDksc = :C01nrv;
440400120712           IF  tfind_rrn > 0;
440500120712             wRrn = tfind_rrn;
440600120712             wFil = 'TFIND00F';
440700120712             exsr CallTrul82;
440800120712             IF  UL82§sts = 'A';
440900120712               leavesr;
441000120712             ENDIF;
441100120712           ENDIF;
441200120712         //?TFCLP
441300120712           exec sql
441400120712             select rrn(TFCLP00F) into :tfclp_rrn
441500120712             from   TFCLP00F
441600120712             where  CLPkut = 1 and CLPkcc = 151 and
441700120712                    CLPksc = :C01nrv;
441800120712           IF  tfclp_rrn > 0;
441900120712             wRrn = tfclp_rrn;
442000120712             wFil = 'TFCLP00F';
442100120712             exsr CallTrul82;
442200120712             IF  UL82§sts = 'A';
442300120712               leavesr;
442400120712             ENDIF;
442500120712           ENDIF;
442600120712         //?TFCLS
442700120712           exec sql
442800120712             select rrn(TFCLS00F) into :tfcls_rrn
442900120712             from   TFCLS00F
443000120712             where  CLSksc = :C01nrv;
443100120712           IF  tfcls_rrn > 0;
443200120712             wRrn = tfcls_rrn;
443300120712             wFil = 'TFCLS00F';
443400120712             exsr CallTrul82;
443500120712             IF  UL82§sts = 'A';
443600120712               leavesr;
443700120712             ENDIF;
443800120712           ENDIF;
443900120712         ENDIF;
444000120712
444100120712       //?Con Codice cliente proposto su anagrafica
444200120712         IF  C05ksc > 0;
444300120712         //?CNACO
444400120712           exec sql
444500120712             select rrn(CNACO00F) into :cnaco_rrn
444600120712             from   CNACO00F
444700120712             where  ACOkut = 1 and ACOkcc = 151 and
444800120712                    ACOksc = :C05ksc;
444900120712           IF  cnaco_rrn > 0;
445000120712             wRrn = cnaco_rrn;
445100120712             wFil = 'CNACO00F';
445200120712             exsr CallTrul82;
445300120712             IF  UL82§sts = 'A';
445400120712               leavesr;
445500120712             ENDIF;
445600120712           ENDIF;
445700120712         //?CNIND
445800120712           exec sql
445900120712             select rrn(CNIND00F) into :cnind_rrn
446000120712             from   CNIND00F
446100120712             where  INDkut = 1 and INDkcc = 151 and
446200120712                    INDksc = :C05ksc;
446300120712           IF  cnind_rrn > 0;
446400120712             wRrn = cnind_rrn;
446500120712             wFil = 'CNIND00F';
446600120712             exsr CallTrul82;
446700120712             IF  UL82§sts = 'A';
446800120712               leavesr;
446900120712             ENDIF;
447000120712           ENDIF;
447100120712         //?CNCLP
447200120712           exec sql
447300120712             select rrn(CNCLP00F) into :cnclp_rrn
447400120712             from   CNCLP00F
447500120712             where  CLPkut = 1 and CLPkcc = 151 and
447600120712                    CLPksc = :C05ksc;
447700120712           IF  cnclp_rrn > 0;
447800120712             wRrn = cnclp_rrn;
447900120712             wFil = 'CNCLP00F';
448000120712             exsr CallTrul82;
448100120712             IF  UL82§sts = 'A';
448200120712               leavesr;
448300120712             ENDIF;
448400120712           ENDIF;
448500120712         //?FNCLS
448600120712           exec sql
448700120712             select rrn(FNCLS00F) into :fncls_rrn
448800120712             from   FNCLS00F
448900120712             where  CLSksc = :C05ksc;
449000120712           IF  fncls_rrn > 0;
449100120712             wRrn = fncls_rrn;
449200120712             wFil = 'FNCLS00F';
449300120712             exsr CallTrul82;
449400120712             IF  UL82§sts = 'A';
449500120712               leavesr;
449600120712             ENDIF;
449700120712           ENDIF;
449800120712         ENDIF;
449900120712
450000120712       //?Con Codice cliente trattativa su anagrafica
450100120712         IF  C01ksc > 0;
450200120712         //?CNACO
450300120712           exec sql
450400120712             select rrn(CNACO00F) into :cnaco_rrn
450500120712             from   CNACO00F
450600120712             where  ACOkut = 1 and ACOkcc = 151 and
450700120712                    ACOksc = :C01ksc;
450800120712           IF  cnaco_rrn > 0;
450900120712             wRrn = cnaco_rrn;
451000120712             wFil = 'CNACO00F';
451100120712             exsr CallTrul82;
451200120712             IF  UL82§sts = 'A';
451300120712               leavesr;
451400120712             ENDIF;
451500120712           ENDIF;
451600120712         //?CNIND
451700120712           exec sql
451800120712             select rrn(CNIND00F) into :cnind_rrn
451900120712             from   CNIND00F
452000120712             where  INDkut = 1 and INDkcc = 151 and
452100120712                    INDksc = :C01ksc;
452200120712           IF  cnind_rrn > 0;
452300120712             wRrn = cnind_rrn;
452400120712             wFil = 'CNIND00F';
452500120712             exsr CallTrul82;
452600120712             IF  UL82§sts = 'A';
452700120712               leavesr;
452800120712             ENDIF;
452900120712           ENDIF;
453000120712         //?CNCLP
453100120712           exec sql
453200120712             select rrn(CNCLP00F) into :cnclp_rrn
453300120712             from   CNCLP00F
453400120712             where  CLPkut = 1 and CLPkcc = 151 and
453500120712                    CLPksc = :C01ksc;
453600120712           IF  cnclp_rrn > 0;
453700120712             wRrn = cnclp_rrn;
453800120712             wFil = 'CNCLP00F';
453900120712             exsr CallTrul82;
454000120712             IF  UL82§sts = 'A';
454100120712               leavesr;
454200120712             ENDIF;
454300120712           ENDIF;
454400120712         //?FNCLS
454500120712           exec sql
454600120712             select rrn(FNCLS00F) into :fncls_rrn
454700120712             from   FNCLS00F
454800120712             where  CLSksc = :C01ksc;
454900120712           IF  fncls_rrn > 0;
455000120712             wRrn = fncls_rrn;
455100120712             wFil = 'FNCLS00F';
455200120712             exsr CallTrul82;
455300120712             IF  UL82§sts = 'A';
455400120712               leavesr;
455500120712             ENDIF;
455600120712           ENDIF;
455700120712         ENDIF;
455800120712
455900120712       ENDSR;
456000120712
456100120712       //--------------------------------------------------------------
456200120712       //?Richiamo TRUL82R.
456300120712       //--------------------------------------------------------------
456400120712       BEGSR CallTRUL82;
456500120712
456600120712         clear TRUL82DS;
456700120712         UL82§rrn = wRrn;
456800120712         UL82§fil = wFil;
456900120712         UL82§win = wWin;
457000120712         UL82§f7  = wF7;
457100120712         UL82§mss = wMss;
457200120712         UL82§msw = wMsw;
457300120712         callp(e) TRUL82R (trul82ds);
457400120712         IF  UL82§sts = 'A';
457500120712           ErrMessage  = *on;
457600120712           ErrGenerico = *on;
457700120712           V1Dmsg      = UL82§msw;
457800120712         ENDIF;
457900120710
458000120710       ENDSR;
458100100429
458200100121       //--------------------------------------------------------------
458300100121       //?Conferma  SFL05
458400100121       //--------------------------------------------------------------
458500100122       BEGSR Sr_F6ConvOff ;
458600100121
458700100121       // leggo il subfile e per ogni tariffa convalido i dati
458800100121       s05nrr = 1 ;
458900100121       chain s05nrr ta56s05 ;
459000100121       Dow %found(tnta56d) ;
459100100121
459200100121       // aggancio il file testate offerta per aggiornare i dati
459300100121       chain (c01nrv:s05ctr:sh5prg) Tnofm01l ;
459400100121       If %found(tnofm01l) ;
459500100121          tamsis = s05sis ;
459600100121          update tnofm000 ;
459700100121       endif;
459800100121
459900100122       // gestione anagrafica solo se devo creare un nuovo cliente
460000100424       If s05nrr = 1 ;
460100100122          exsr sr_Anagra ;
460200100203       // esito la  trattativa in positivo
460300100203          visesi = 'P' ;
460400100203          update tivis000 ;
460500100203       // rialloco il record
460600100203          chain  ita56nrv tivis05l ;
460700100122       endif ;
460800100122
460900100122       // gestione tariffe
461000100122       exsr Sr_tariffe ;
461100100122
461200100424       // gestione note
461300100424       exsr sr_Note ;
461400100424
461500100122       // Cancello attività e note attività legate all'offerta
461600100122             $offerta = *on ;
461700100122             exsr Sr_Delatc ;
461800100122             $offerta = *off;
461900100122
462000100122       // aggiorno i dati su TIVOF
462100110112       chain (c01nrv:s05ctr:sh5prg) tivof11l;
462200100122       vofksc = tamksc ;
462300101026       vofprt = wprg   ;
462400100122       vofeso = 'C' ;
462500100122       vofdra = wrkdra ;
462600100122       vofdco = datacor8 ;
462700100122       update tivof000 ;
462800100126       $Esitata = *on ;
462900100126
463000100126       // aggancio il record del subfile precedente e lo aggiorno
463100100126       chain sh5nrr ta56s01 ;
463200100126       If %found(tnta56d) ;
463300100126          clear s01opz ;
463400100312          sh1dpa = c05dra ;
463500100312          data_dmy = %date(c05dra :*eur);
463600100312          s01dpa  = %dec(data_dmy)  ;
463700100126          sh1conv = *on ;
463800100312          sh1eso = 'C' ;
463900100204          If  sh1n12 = '1' ;
464000100204              note10_tar = *on ;
464100100204          else ;
464200100204              note10_tar = *off ;
464300100204          endif ;
464400100126          update ta56s01 ;
464500100126       endif ;
464600100126
464700100126       s05nrr +=1 ;
464800100126       chain s05nrr ta56s05 ;
464900100121
465000100122       enddo ;
465100100122
465200110118       // se si tratta di codifica di un nuovo cliente
465300100122       // fine lettura e convalida offerte emetto videata per stampa lettera
465400110118       If Nuovo_cli = *on  ;
465500110118          parcli = c05ksc ;
465600110118          parrgs = c01rag ;
465700110118          kpjbu = param ;
465800110118          fnlv43r (kpjba) ;
465900110118       Endif ;
466000100122
466100100122       // richiamo il pgm gestione anagrafica clienti
466200100122       Clear     tnta60ds ;
466300100122       ta60flg = 'C'   ;
466400100122       ta60nrv = c05ksc;
466500100122       TNTA60R (Kpjba:tnta60ds);
466600100122       //  devo abilitare il cliente ai servizi ON LINE
466700100122       //  se cliente nuovo
466800100122       //  se cliente non nuovo ma lui o suo papà (WW) non sono ancora abilitati
466900100122       ExSr   Sr_Psw  ;
467000100122
467100100122       // propongo una nuova attività di richiamo con causale 32 cliente
467200100122       // acuisito
467300100126         causale = '32' ;
467400100122
467500100122       // Richiamo la routine che crea nuove attività
467600100122         exsr Sr_CreaAtt ;
467700100122
467800100122
467900100122       // Richiamo la routine che verifica ed esita la trattativa
468000100122             exsr Sr_EsiTrat ;
468100100122
468200100122       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
468300100126             If N_off_daesi= 0 ;
468400100122       // emetto la videata di richiesta di conferma
468500100122                $video = 'W3' ;
468600100122                $inzw03 = *on  ;
468700100122                leavesr ;
468800100122
468900100122             Endif ;
469000100122
469100100122
469200100122          $inzs01 = *on  ;
469300100122          $video  = 'S1' ;
469400100122
469500100122
469600100122
469700100121       ENDSR ;
469800100121
469900100121       //--------------------------------------------------------------
470000100121       //?Gestione Anagrafiche
470100100121       //--------------------------------------------------------------
470200100121       BEGSR Sr_anagra ;
470300100121
470400100504       clear tnta55ds ;
470500100505       ita55nrv = c01nrv ;
470600100505       ita55tpv = c01tpv ;
470700100505       ita55ksc = c01ksc ;
470800100505       ita55nwksc = c05ksc ;
470900100917       If c01tpv = 'N' ;
471000100505       If c01esi = ' ';
471100100505          ita55prima = 'SI' ;
471200100505       endif ;
471300100917       endif;
471400100917       // se tipo visita di aumento verifico se ho già confermato
471500101007       // sul cliente che devo confermare
471600101007       //      If c01tpv = 'A' ;
471700100917
471800100917         exec sql
471900100917          declare t2  cursor for
472000100917          select vofksc
472100100917          from tivof00f where vofnrv = :visnrv and vofeso = 'C' and
472200110112          vofdco > 0   and vofksc = :c05ksc and vofctr <> 999 ;
472300100917
472400100917         exec sql
472500100917           open t2;
472600100917           IF sqlcod = 0;
472700100917           exec sql
472800100917             fetch next from t2  into :vofksc;
472900100917             IF sqlcod = 100 or sqlcod < 0;
473000100917               ita55prima = 'SI' ;
473100100917             else ;
473200100917               ita55prima = '  ' ;
473300100917             endif ;
473400100917           Endif;
473500100917
473600100917         exec sql close t2 ;
473700101007       //   endif ;
473800100917
473900100505       If Vis_pagamento  = *on ;
474000100505          ita55pag = '1' ;
474100100505          ita55abi = c05abi ;
474200100505          ita55cab = c05cab ;
474300100520          ita55cdp = '000' + c05cdp ;
474400100505       endif ;
474500100504
474600100505           If c05ivae = *Blanks  ;
474700100505              ita55iva = c05ivai ;
474800100505           else ;
474900100505              ita55iva = c05ivae + c05ivai ;
475000100505           EndIf;
475100100505
475200100505       ita55cdf = c05cdf ;
475300100818       ita55cmm = comcon ;
475400100505       ita55cpo = ch1cpo ;
475500100505       Tnta55r (kpjba:tnta55ds) ;
475600100505
475700100505        // verifico se ho creato nuovo cliente
475800100505       If ota55new = '1' ;
475900100505          Nuovo_cli = *on  ;
476000100505       else ;
476100100505          Nuovo_cli = *off ;
476200100505       endif ;
476300100505
476400100122       // aggiorno anagrafica clienti potenziali
476500100122
476600100122       If ch1cpo > 0 ;
476700100122          chain ch1cpo tncpo01l ;
476800100122          If %found(tncpo01l) ;
476900160803           //?Salvo l'immagine precedente dell'anagrafica potenziale
477000160803            exsr ImmaginePrima;
477100100122        // partita IVA
477200100122             If cpopiv = *blanks and indiva <> *blanks;
477300160831                //cpopiv = indiva ;
477400160831                cpopiv = ita55iva;
477500100316                cpodtr = datacor8 ;
477600100122             endif ;
477700100122        // codice fiscale
477800100122             If cpocdf = *blanks and indcdf <> *blanks;
477900160831                //cpocdf = indcdf ;
478000160831                cpocdf = ita55cdf;
478100100316                cpodtr = datacor8 ;
478200100122             endif ;
478300100122
478400100122             update tncpo000 ;
478500160803
478600160803           //?Scrivo la Variazione fatta sull'anagrafica potenziale
478700160803            exsr ScriviVar;
478800100122          endif ;
478900100122        endif ;
479000100122
479100100121       ENDSR ;
479200100122
479300100122       //--------------------------------------------------------------
479400100122       //?Gestione TARIFFE
479500100122       //--------------------------------------------------------------
479600100122       BEGSR Sr_Tariffe ;
479700100122
479800100122       // verifico se esiste già una tariffa annullata e ci vado sopra
479900100122       chain (c05ksc:s05ctr) Tntam04l ;
480000100122       // non trovato nessun progressivo per quella tariffa imposto
480100100122       // il nuovo progressivo a zero
480200100316       If not %found(tntam04l);
480300100122          wprg   = 0 ;
480400100122       else ;
480500100122          wprg   = tamprg + 1 ;
480600100122       endif ;
480700100122       chain (c05ksc:s05ctr:wprg) Tntam01l ;
480800100122
480900100122       // aggancio il file testate offerta per aggiornare i dati
481000100122       chain (c01nrv:s05ctr:sh5prg) Tnofm01l ;
481100100122
481200100122       tamprg = wprg ;
481300100122       tamatb = ' ' ;
481400100122       tamftr = ' ' ;
481500100316       tamduv = datacor8 ;
481600100316       tamdtr = datacor8 ;
481700100122       tamksc = c05ksc ;
481800100122       if not %found(tntam01l) ;
481900100122          write tntam000 ;
482000100122       else ;
482100100122          update tntam000 ;
482200100122       endif ;
482300100122       delete tnofm000 ;
482400100122
482500100122       // scrivo lo storico
482600100122
482700100122       Clear Titav000     ;
482800100122       TavKsc = TamKsc    ;
482900100122       TavCtr = TamCtr    ;
483000100122       TavPrg = TamPrg    ;
483100100122       TavTrd = 'TES'     ;
483200100122       TavCta = 'O'       ;
483300100316       TavDav = datacor8  ;
483400100122       TavOrv = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
483500110325       TavNoj = sdsjob    ;
483600100122       TavPru = sdsuse    ;
483700100316       TavDtr = datacor8  ;
483800100122       Write     Titav000 ;
483900100122
484000100122       // scrivo dettaglio tariffe
484100100122       // prima verifico se ci sono dei record da cancellare
484200100122       // con il nuovo progressivo
484300100122       setll (c05ksc:s05ctr:wprg) Titad04l ;
484400100122       reade (c05ksc:s05ctr:wprg) Titad04l ;
484500100122
484600100122       dow not %eof(titad04l) ;
484700100122           delete titad000 ;
484800100122           reade (c05ksc:s05ctr:wprg) Titad04l ;
484900100122       enddo ;
485000100122
485100100122       // scrivo il dettaglio
485200100122       setll (c01nrv:s05ctr:sh5prg) Tiofd01l ;
485300100122       reade (c01nrv:s05ctr:sh5prg) Tiofd01l ;
485400100122
485500100122       dow not %eof(tiofd01l) ;
485600100122           tadftr = ' ';
485700100316           taddtr = datacor8 ;
485800100122           tadksc = c05ksc ;
485900100122           tadprg = wprg ;
486000100122           write titad000 ;
486100100122           delete tiofd000 ;
486200100122           reade (c01nrv:s05ctr:sh5prg) Tiofd01l ;
486300100122       enddo ;
486400100122
486500100122       // scrivo testata   tariffe particolari
486600100122       // prima verifico se ci sono dei record da cancellare
486700100122       // con il nuovo progressivo
486800100122       setll (c05ksc:s05ctr:wprg) Titpt01l ;
486900100122       reade (c05ksc:s05ctr:wprg) Titpt01l ;
487000100122
487100100122       dow not %eof(titpt01l) ;
487200100122           delete titpt000 ;
487300100122           reade (c05ksc:s05ctr:wprg) Titpt01l ;
487400100122       enddo ;
487500100122
487600100122       // scrivo il dettaglio
487700100122       setll (c01nrv:s05ctr:sh5prg) Tiopt01l ;
487800100122       reade (c01nrv:s05ctr:sh5prg) Tiopt01l ;
487900100122
488000100122       dow not %eof(tiopt01l) ;
488100100122           tptftr = ' ';
488200100316           tptdtr = datacor8 ;
488300100316           tptduv = datacor8 ;
488400100122           tptksc = c05ksc ;
488500100122           tptprg = wprg ;
488600100122           write titpt000 ;
488700100122           delete tiopt000 ;
488800100122           reade (c01nrv:s05ctr:sh5prg) Tiopt01l ;
488900100122       enddo ;
489000100122
489100100122       // scrivo dettaglio tariffe particolari
489200100122       // prima verifico se ci sono dei record da cancellare
489300100122       // con il nuovo progressivo
489400100122       setll (c05ksc:s05ctr:wprg) Titpd01l ;
489500100122       reade (c05ksc:s05ctr:wprg) Titpd01l ;
489600100122
489700100122       dow not %eof(titpd01l) ;
489800100122           delete titpd000 ;
489900100122           reade (c05ksc:s05ctr:wprg) Titpd01l ;
490000100122       enddo ;
490100100122
490200100122       // scrivo il dettaglio
490300100122       setll (c01nrv:s05ctr:sh5prg) Tiopd01l ;
490400100122       reade (c01nrv:s05ctr:sh5prg) Tiopd01l ;
490500100122
490600100122       dow not %eof(tiopd01l) ;
490700100122           tpdftr = ' ';
490800100316           tpddtr = datacor8 ;
490900100122           tpdksc = c05ksc ;
491000100122           tpdprg = wprg ;
491100100122           write titpd000 ;
491200100122           delete tiopd000 ;
491300100122           reade (c01nrv:s05ctr:sh5prg) Tiopd01l ;
491400100122       enddo ;
491500100122
491600100122       // scrivo tariffe di giacenza
491700100122       // prima verifico se ci sono dei record da cancellare
491800100122       // con il nuovo progressivo
491900100122       chain (c05ksc:s05ctr:wprg) Titgc01l ;
492000100122
492100100122       If %found(titgc01l) ;
492200100122           delete titgc000 ;
492300100122           reade (c05ksc:s05ctr:wprg) Titgc01l ;
492400100122       endif ;
492500100122
492600100122       // scrivo il dettaglio
492700100122       chain (c01nrv:s05ctr:sh5prg) Tiogc01l ;
492800100122
492900100122       If %found(tiogc01l) ;
493000100122           tgcftr = ' ';
493100100316           tgcdtr = datacor8 ;
493200100316           tgcduv = datacor8 ;
493300100122           tgcksc = c05ksc ;
493400100316           tgcprg = wprg ;
493500100122           write titgc000 ;
493600100126           delete tiogc000 ;
493700100122       endif ;
493800150223
493900150223       // richiamo il programma di inglobamento per verificare
494000150223       // la presenza o meno di tutte le tariffe particolari
494100150223       // obbligatorie e il fuel se corretto in  base alla
494200150223       // tabella VMA negazione VMA
494300150223       clear Tnta25ds ;
494400150223       clear Tnta25tad;
494500150223       clear Tnta25tgc;
494600150223       clear Tnta25tpd;
494700150223       clear Tnta25tpt;
494800150223       ta25tipo = 'T' ;
494900150223       ta25ksco = tamksc ;
495000150223       ta25ctro = tamctr ;
495100150223       ta25prgo = tamprg ;
495200150223       ta25tipn = 'T' ;
495300150223       ta25kscn = tamksc ;
495400150223       ta25ctrn = tamctr ;
495500150302       ta25prgn = %editc(tamprg:'X');
495600150223       ta25tam = 'N' ;
495700150223       ta25fie = tamfie ;
495800150223       ta25tam = 'N' ;
495900150223       ta25tad = 'N' ;
496000150223       ta25tpt = 'N' ;
496100150223       ta25tpd = 'N' ;
496200150223       ta25tgc = 'N' ;
496300150223       ta25cat = 'N' ;
496400150223       ta25inglo = 'S';
496500150223       Tnta25r (Kpjba:Tnta25ds:Tnta25tad:Tnta25tgc:Tnta25tpd:Tnta25tpt);
496600100122
496700100122       // creao i dati per il conto economico
496800100122       clear tnta33ds ;
496900100122       wta33 = *on ;
497000100122       d33nrv = c01nrv ;
497100100122       d33ksc = c05ksc;
497200100122       d33ctr = tamctr ;
497300100122       d33prg = tamprg ;
497400140919       D33pr2 = SH5prg;
497500100122       kpjbu = tnta33ds ;
497600100122       tnta33r (Kpjba) ;
497700100122
497800100122       ENDSR ;
497900100122
498000100122       //--------------------------------------------------------------
498100100122       //?Gestione Note
498200100122       //--------------------------------------------------------------
498300100122       BEGSR Sr_Note    ;
498400100424
498500100424         xx=%lookup('10' : tno) ;
498600100424       // cerco le note 10 relative al codice tariffa
498700100424         ntcapl = 'T' ;
498800100425         ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
498900100424         ntcnk2 = %editc(s05ctr: 'X') ;
499000100424         ntctnt = '10' ;
499100100424         setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
499200100424         If %equal(tfntc01l) ;
499300100424       // verifico se sono da duplicare nell'eventualità che ci
499400100424       // siano altre note già legate a cliente e tariffa
499500100424           if  tdup(xx) = 'S' and ttdp(xx) = 'R' ;
499600100424          // in caso di replace sulle note già esistenti
499700100424          // deleto le note 10 codice tariffa del cliente in convalida
499800100424                 ntcapl = 'C' ;
499900100424                 ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
500000100424                 ntcnk2 = %editc(tamctr: 'X') ;
500100100424                 ntctnt = '10' ;
500200100424                 setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
500300100424                 reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
500400100424                 dow not %eof(tfntc01l) ;
500500100424                     delete tfntc ;
500600100424                     reade  (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
500700100424                 enddo ;
500800100424           endif ;
500900100424           if  tdup(xx) = 'S'  ;
501000100424        // ora leggo le note 10 trattativa e le scrivo con il ksc tariffa
501100100424               ntcapl = 'T' ;
501200100425               ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
501300100424               ntcnk2 = %editc(s05ctr: 'X') ;
501400100424               ntctnt = '10' ;
501500100424               setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
501600100424               reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
501700100424               dow not %eof(tfntc01l) ;
501800100424                   ntcapl = 'C' ;
501900100424                   ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
502000100424                   ntcntr = datacor6;
502100100424                   write tfntc ;
502200100424                   reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
502300100424               enddo ;
502400100424            endif ;
502500100424
502600100424        // verifico e deleto note 10 legate all'offerta se non sono
502700100424        // presenti altri progressivi
502800100425            keynrv = c01nrv ;
502900100425            Keyctr = s05ctr ;
503000100424            exsr Sr_Delnote10 ;
503100100424
503200100424          else  ;
503300100424        // se non trovate note 10 legate alla tariffa cerco quelle
503400100424        // legate al cliente (tariffa a blank)
503500100424
503600100424         ntcapl = 'T' ;
503700100425         ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
503800100424         clear ntcnk2 ;
503900100424         ntctnt = '10' ;
504000100424         setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
504100100424         If %equal(tfntc01l) ;
504200100424        // verifico se sono da duplicare nell'eventualità che ci
504300100424        // siano altre note già legate a cliente e tariffa
504400100424           if  tdup(xx) = 'S' and ttdp(xx) = 'R' ;
504500100424          // in caso di replace sulle note già esistenti
504600100424          // deleto le note 10 codice tariffa del cliente in convalida
504700100424                 ntcapl = 'C' ;
504800100424                 ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
504900100424                 ntcnk2 = %editc(tamctr: 'X') ;
505000100424                 ntctnt = '10' ;
505100100424                 setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
505200100424                 reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
505300100424                 dow not %eof(tfntc01l) ;
505400100424                     delete tfntc ;
505500100424                     reade  (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
505600100424                 enddo ;
505700100424           endif ;
505800100424           if  tdup(xx) = 'S'  ;
505900100424         // ora leggo le note 10 trattativa e le scrivo con il ksc tariffa
506000100424               ntcapl = 'T' ;
506100100425               ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
506200100424               clear ntcnk2 ;
506300100424               ntctnt = '10' ;
506400100424               setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
506500100424               reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
506600100424               dow not %eof(tfntc01l) ;
506700100424                   ntcapl = 'C' ;
506800100424                   ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
506900100424                   ntcnk2 = %editc(tamctr: 'X') ;
507000100424                   ntcntr = datacor6;
507100100424                   write tfntc ;
507200100424                   reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
507300100424               enddo ;
507400100424            endif ;
507500100424          endif ;
507600100424         endif ;
507700100122
507800100122       ENDSR ;
507900160803
508000160803       //--------------------------------------------------------------
508100160803       //?Scrivo l'immagine Precedente.
508200160803       //--------------------------------------------------------------
508300160803       BEGSR ImmaginePrima;
508400160803
508500160803         clear TRMK30DS;
508600160803         IMK30immag = 'P';
508700160803         IMK30tvp = 'B';
508800160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
508900160803
509000160803       ENDSR;
509100160803
509200160803       //--------------------------------------------------------------
509300160803       //?Scrivo Storico Variazioni.
509400160803       //--------------------------------------------------------------
509500160803       BEGSR ScriviVar;
509600160803
509700160803         clear TRMK30DS;
509800160803         IMK30pru = knmus;
509900160803         IMK30noj = knmeb;
510000160803         IMK30pgm = 'TNTA56R';
510100160803         IMK30immag = 'D';
510200160803         IMK30cta = 'M';
510300160803         IMK30tvp = 'B';
510400160803
510500160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
510600160803
510700160803       ENDSR;
510800100424
510900100115       //--------------------------------------------------------------
511000100115       //?Operazioni iniziali.
511100100115       //--------------------------------------------------------------
511200100115       BEGSR RoutInz;
511300100115
511400100115         // Impostazione campi "fissi"
511500100115         T01pgm = SDSpgm;
511600100115         TBLkut = 1;
511700100115
511800100115         // Reperimento dati job
511900100115         exsr DatiJob;
512000100115
512100100115         // Verifica errori e autorità profilo
512200100115         clear  wAbi;
512300100115         clear  dLAT;
512400100115         select;
512500100115         // - Se ho errori nei dati utente esco dal pgm
512600100115           when  DUTerr = 'E';
512700100115             $Fine = *on;
512800100115         // - Se non c'è l'abilitazione:
512900100115         //   - se 1° livello, abilitazioni al terminal
513000100115         //   - se 2° livello, abilitazioni al punto operativo
513100100115         //   - se sede è impossibile ma se capita mando a fine il pgm
513200100115           when  UTEaut = *blank;
513300100115             select;
513400100115               when  DUTlpo = '1';
513500100115                 wAbi  = 'TP';
513600100115               when  DUTlpo = '2';
513700100115                 wAbi  = 'PO';
513800100115               when  DUTlpo = 'S';
513900100115                 $Fine = *on;
514000100115             endsl;
514100100115         // - Altrimenti si caricano le abilitazioni del profilo
514200100115           other;
514300100115             dUTE01 = UTEfaf;
514400100115             if  §UTEpot <> *blank;
514500100115               wAbi = §UTEpot;
514600100115             else;
514700100115               wAbi = UTEaut;
514800100115             endif;
514900100115         ENDSL;
515000100115
515100100115         // Controllo se ok l'abilitazione dell'utente
515200100115         reset TIBS02ds;
515300100115         T02sif = knsif;
515400100115         T02cod = 'LAT';
515500100115         T02ke1 = wAbi;
515600100115         TNTBE_RicercaControllo  (kpjba : tibs02ds);
515700100115
515800100115         if  T02err  = *blank;
515900100115           dLAT = T02uni;
516000100115         endif;
516100100115
516200100115         // Errore o utente non abilitato
516300100115         if  T02err <> *blanks   or
516400100115             §LATabi = 'S';
516500100115           $ErrGrave   = *on;
516600100115           errMessage  = *on;
516700100115           errGenerico = *on;
516800100115           V1Dmsg = $Msg(01);
516900100115           leavesr;
517000100115         endif;
517100100115
517200100115         clear trul31ds  ;
517300100115         i31abi=wabi   ;
517400100115         I31cdi = DUTdis   ;
517500100115         I31car = DUTare   ;
517600100115         I31cpo = DUTpou   ;
517700100115         callp TRUL31R (kpjba:trul31ds)   ;
517800100115
517900100115         if o31pog<=*zeros  ;
518000100115           $ErrGrave   = *on;
518100100115           errMessage  = *on;
518200100115           errGenerico = *on;
518300100115           V1Dmsg = $Msg(01);
518400100115           leavesr;
518500100115         endif;
518600100115
518700100115
518800100115         // Intabellamento filiali e relative aree
518900100115         setll *loval AZORG;
519000100115         read  AZORG;
519100100115         dow  NOT  %eof(AZORG01L);
519200100115           if  ORGfva = *blank;
519300100115             xx += 1;
519400100115             $Fil(xx) = ORGfil;
519500100115             $Car(xx) = ORGcar;
519600100115           endif;
519700100115           read  AZORG;
519800100115         enddo;
519900100115
520000100115       // Reperisco data corrente
520100100115             data_iso = (%date());
520200100122             datacor8 = %dec(data_iso);
520300100125             data_ymd = (%date());
520400100121             datacor6 = %dec(data_ymd) ;
520500100115
520600100115        //  Carico schiera con tabella TR (tipo tariffa))
520700100115             Clear   xx;
520800100115             TBLcod = 'TR';
520900100115             setll (TBLkut : TBLcod) TABEL;
521000100115             reade (TBLkut : TBLcod) TABEL;
521100100115             dow not %eof(TABEL00F);
521200100115                 If tblflg = ' ' and tblkey <> *blanks ;
521300100115                    xx = xx + 1 ;
521400100115                    ctr(xx) = tblkey ;
521500100115                    dtr(xx) = tbluni;
521600100115                 endif ;
521700100115                 reade (TBLkut : TBLcod) TABEL;
521800100115             EndDo ;
521900100122
522000100122        //  Carico schiera con tabella 1T (tipo nota)
522100100122             Clear   xx;
522200100122             TBLcod = '1T';
522300100122             setll (TBLkut : TBLcod) TABEL;
522400100122             reade (TBLkut : TBLcod) TABEL;
522500100122             dow not %eof(TABEL00F);
522600100122                 If tblflg = ' ' and tblkey <> *blanks ;
522700100122                    xx = xx + 1 ;
522800100122                    ds1t = tbluni ;
522900100122                    tno(xx) = tblkey ;
523000100122                    tdup(xx) = §1tdup;
523100100122                    ttdp(xx) = §1ttdp;
523200100122                 endif ;
523300100122                 reade (TBLkut : TBLcod) TABEL;
523400100122             EndDo ;
523500100115
523600100115       // verifico i dati passati
523700100115
523800100115       // se impostato il numero visita aggancio il file direttamente
523900100115         If ita56nrv > 0 ;
524000100115            chain    ita56nrv tivis05l ;
524100100115
524200100115       // se trattativa non trovata vado a fine
524300100115            If not %found(tivis05l) ;
524400100115               $fine = *on ;
524500100115               ota56err = 'E' ;
524600100115               ota56msg = 'Trattativa inesistente' ;
524700100115               leavesr ;
524800100115            endif;
524900100119
525000100119       // se trovata verifico se chiusa
525100100119            If visdch > 0 ;
525200100119               $fine = *on ;
525300100119               ota56err = 'E' ;
525400100119               ota56msg = 'Trattativa Chiusa' ;
525500100119               leavesr ;
525600100119            endif ;
525700100119
525800100119       // se trovata verifico se fittizia
525900100413            If visffz = 'S' ;
526000100119               $fine = *on ;
526100100119               ota56err = 'E' ;
526200100119               ota56msg = 'Trattativa Fittizia';
526300100119               leavesr ;
526400100119            endif ;
526500100119
526600100119       // se senza offerte ancora da esitare
526700100316       //   setll (visnrv) tnofm01l ;
526800100316       //   If not %equal(tnofm01l) ;
526900100316       //      $fine = *on ;
527000100316       //      ota56err = 'E' ;
527100100316       //      ota56msg = 'Trattativa senza Offerte' ;
527200100316       //      leavesr ;
527300100316       //   endif ;
527400100115
527500100115         endif;
527600100119
527700100119       // verifico se utente abilitato alla gestione della trattativa
527800100119         clear tntaa1ds;
527900100119         itaa1nrv =visnrv ;
528000100119         itaa1caut='§UTEGTC';
528100100119         clear kpjbu;
528200100119         kpjbu = tntaa1ds ;
528300100119         Tntaa1c (kpjba) ;
528400100119         tntaa1ds = kpjbu ;
528500100119
528600100119         SELECT ;
528700100119        //   Errore --> chiamare il ced di sede
528800100119         When  otaa1err='S';
528900100119               $fine = *on ;
529000100119               ota56err = 'E' ;
529100100121               ota56msg = $msg(18)  ;
529200100119               leavesr ;
529300100119
529400100119        // Non abilitato
529500100119           when otaa1fabi='N' ;
529600100119               $fine = *on ;
529700100119               ota56err = 'E' ;
529800100119               ota56msg = 'Visita non in gestione all''utente';
529900100119               leavesr ;
530000100119
530100100119           ENDSL;
530200100422
530300100422       // recupero date statistiche
530400100422         clear dsdf ;
530500100422         reset TIBS02ds;
530600100422         T02sif = knsif;
530700100422         T02cod = 'SDF';
530800100422         T02ke1 = '046    ' ;
530900100422         TNTBE_RicercaControllo  (kpjba : tibs02ds);
531000100422
531100100422         if  T02err  = *blank;
531200100422           dSDF = T02uni;
531300100422         endif;
531400130319
531500130319       //?Imposto la libreria per il TITAS
531600130319         IF  %subst(knsif : 7 : 1) = 'P';
531700130319           wLib = 'GAITRAGRPS';
531800130319         ELSE;
531900130319           wLib = 'GAITRAGRU';
532000130319         ENDIF;
532100130319       //?Apro il file
532200130319         wFLib = %trim(wLib) + '/TITAS41C';
532300130319         open  TITAS41C;
532400100422
532500100115       ENDSR;
532600100115
532700100120       //--------------------------------------------------------------
532800100120       //?Routine di controllo PARTITA IVA
532900100120       //--------------------------------------------------------------
533000100120       BEGSR  Sr_ctriva ;
533100100120
533200100120        // controllo il codice fiscale e la partita iva
533300151015         clear xcfiva1ds;
533400100120        // controllo la partita IVA
533500100120         if  Wmod = 'P' ;
533600100120             xcfivamod = wmod ;
533700100121             xcfivapar = c05ivae + c05ivai ;
533800100120         endif ;
533900151015
534000100120        // controllo il codice fiscale
534100151015        //  passando anche la p.iva  - serve per san marino -
534200100120         if  wmod = 'C';
534300100120             clear xcfivamod ;
534400100120             xcfivapar = c05cdf ;
534500151015             xcfivapiva=wiva    ;
534600100120         endif ;
534700151015
534800100120        // dati comuni ai due tipi di controllo
534900100120         xcfivanar = indsta ;
535000100120         xcfivaprv = indprv ;
535100151015         xcfivacap = indcae ;
535200151015         xcfivaloc = indcit ;
535300151015
535400100120        // richiamo il pgm
535500151015         XCFIVAR1 (xcfiva1ds);
535600100120
535700100120         if xcfivaflg < *zeros and wmod = 'C' ;
535800100120            errMessage  = *on;
535900100120            errGenerico = *on;
536000100120            PosCurCdf = *on ;
536100100120            V1dmsg = xcfivamsg;
536200100120            leavesr;
536300100120         endif ;
536400100120        // -> errore forzabile
536500100120         if wmod = 'P' ;
536600100120            if xcfivaflg = -9 and wforzaiva = *off ;
536700100120               errMessage  = *on;
536800100120               errGenerico = *on;
536900100120               PosCurIva = *on ;
537000100120               V1dmsg = xcfivamsg;
537100100120               V1dmsg = %trim(V1dmsg) + ' Enter x forzare';
537200100120               wforzaiva = *on ;
537300100120               leavesr;
537400100120            endif ;
537500100120        // -> errore bloccante
537600100120            if xcfivaflg < *zeros and xcfivaflg <> -9;
537700100120               errMessage  = *on;
537800100120               errGenerico = *on;
537900100120               PosCurIva = *on ;
538000100120               V1dmsg = xcfivamsg;
538100100120               leavesr;
538200100120            endif ;
538300100120         EndIf;
538400100120
538500100120       ENDSR ;
538600100120
538700100120       //--------------------------------------------------------------
538800100120       //?Routine di controllo presenza codice su altro potenziale
538900100120       //?con lo stesso codice fiscale
539000100120       //--------------------------------------------------------------
539100100120       BEGSR  Sr_CdfAltroP ;
539200100120
539300100120             setll Codice tncpo05l ;
539400100120             reade Codice tncpo05l ;
539500100120             dow not %eof(tncpo05l);
539600100120       // se trovato un record "filiale memorizzo ragione sociale del potenz.
539700100120                   savrag=c_cporag ;
539800100120                   reade Codice tncpo05l ;
539900100120             enddo;
540000100120             if not %eof (tncpo05l) ;
540100100120                savrag=c_cporag;
540200100120             endif ;
540300100120
540400100120       ENDSR ;
540500100120
540600100120       //--------------------------------------------------------------
540700100120       //?Routine di controllo presenza codice su altro potenziale
540800100120       //?con la stessa partita IVA
540900100120       //--------------------------------------------------------------
541000100121       BEGSR  Sr_PivAltroP ;
541100100120
541200100120             setll Codice tncpo06l ;
541300100120             reade Codice tncpo06l ;
541400100120             dow not %eof(tncpo06l);
541500100120       // se trovato un record "filiale memorizzo ragione sociale del potenz.
541600100120                   savrag=c_cporag ;
541700100120                   reade Codice tncpo06l ;
541800100120             enddo;
541900100120             if not %eof (tncpo06l) ;
542000100120                savrag=c_cporag;
542100100120             endif ;
542200100120
542300100120       ENDSR ;
542400100120
542500100120       //--------------------------------------------------------------
542600100120       //?Messaggio di errore codice fiscale già presente per altro
542700100120       //?potenziale
542800100120       //--------------------------------------------------------------
542900100120       BEGSR  Sr_Msgcdf ;
543000100120
543100100120        // Errore forzabile se trovato un altro potenziale con lo stesso
543200100120        // codice fiscale
543300100120           if savrag<>*blanks ;
543400100120              cod_forzcdf=codice  ;
543500100120              errMessage  = *on;
543600100120              errGenerico = *on;
543700100120              PosCurCdf = *on ;
543800100120              V1dmsg = $msg(10);
543900100120              leavesr;
544000100120           endif;
544100100120
544200100120       ENDSR ;
544300100120
544400100120       //--------------------------------------------------------------
544500100120       //?Messaggio di errore Partita Iva  già presente per altro
544600100120       //?potenziale
544700100120       //--------------------------------------------------------------
544800100120       BEGSR  Sr_Msgiva ;
544900100120
545000100120        // Errore forzabile se trovato un altro potenziale con lo stesso
545100100120        // codice fiscale
545200100120           if savrag<>*blanks ;
545300100120              cod_forziva=wiva  ;
545400100120              errMessage  = *on;
545500100120              errGenerico = *on;
545600100120              PosCurIva = *on ;
545700100120              V1dmsg = $msg(09);
545800100120              leavesr;
545900100120           endif;
546000100120
546100100120       ENDSR ;
546200100120
546300100120       //--------------------------------------------------------------
546400100120       //?Controllo le condizioni di pagamento
546500100120       //--------------------------------------------------------------
546600100120       BEGSR  Sr_CtrPag ;
546700100120
546800100120            clear dsfa ;
546900100120            Tblkut = 1 ;
547000100120            tblcod = 'FA';
547100100120            tblkey = c05cdp + '1' ;
547200100120            chain (Tblkut:tblcod:tblkey) tabel00f ;
547300100120
547400100120            If %found(tabel00f) ;
547500100120               dsfa = tbluni ;
547600100120            else ;
547700100120              errMessage  = *on;
547800100120              errGenerico = *on;
547900100120              PosCurCdp = *on ;
548000100120              V1dmsg = $msg(11);
548100100120              leavesr;
548200100120            endif ;
548300150416
548400150416       //?Se tipo pagamento gestibile solo da sede errore
548500150417           IF  C05cdp <> savcdp and §fauti = 'S';
548600150416             ErrMessage  = *on;
548700150416             ErrGenerico = *on;
548800150416             PosCurCdp = *on ;
548900150416             V1dmsg = $msg(34);
549000150416             leavesr;
549100150416           ENDIF;
549200100120
549300100121        // verifico i codici ABI e CAB
549400100120            If §fatpg = '1' and c05abi = 0 and c05cab = 0 ;
549500100120               errMessage  = *on;
549600100120               errGenerico = *on;
549700100120               PosCurAbi = *on ;
549800100120               V1dmsg = $msg(12);
549900100120               leavesr;
550000100120            endif ;
550100100120
550200100121        // controllo ABI CAB
550300100120            exsr Sr_ctrabicab ;
550400100120            If errGenerico = *on ;
550500100120               leavesr ;
550600100120            endif ;
550700100120
550800100120       ENDSR ;
550900100120
551000100120       //--------------------------------------------------------------
551100100120       //?Controllo i codici ABI e CAB
551200100120       //--------------------------------------------------------------
551300100120       BEGSR  Sr_CtrAbiCab ;
551400100120
551500100121        // se già controllati non torno più a controllare
551600100121            if c05abi = savabi and c05cab = savcab ;
551700100120               leavesr ;
551800100120            endif ;
551900100120
552000100121        // verifico se entrambi i codici ABI e CAB sono stati inseriti
552100100120            If c05abi = 0 and c05cab <> 0 ;
552200100120               errMessage  = *on;
552300100120               errGenerico = *on;
552400100120               PosCurAbi = *on ;
552500100120               V1dmsg = $msg(13);
552600100120               leavesr;
552700100120            endif ;
552800100120
552900100120            If c05abi <> 0 and c05cab = 0 ;
553000100120               errMessage  = *on;
553100100120               errGenerico = *on;
553200100120               PosCurCab = *on ;
553300100120               V1dmsg = $msg(15);
553400100120               leavesr;
553500100120            endif ;
553600100120         // aggancio anagrafico banche per verifica esistenza codici
553700100120            savabi = c05abi ;
553800100120            savcab = c05cab ;
553900100120
554000100120            chain (c05abi:c05cab) cnabi00f ;
554100100120            If not %found(cnabi00f) or abiann = '*' ;
554200100120                errMessage  = *on;
554300100120                errGenerico = *on;
554400100120                PosCurCab = *on ;
554500100120                V1dmsg = $msg(14);
554600100120                leavesr;
554700100120            endif ;
554800100120
554900100120            If %found(cnabi00f) ;
555000100120               exsr Sr_Crebna ;
555100100120            Endif ;
555200100120
555300100120       ENDSR ;
555400100120
555500100122       //--------------------------------------------------------------
555600100122       //?Stampa servizi ON LINE se offerta
555700100122       //--------------------------------------------------------------
555800100122       BEGSR  Sr_psw  ;
555900100122
556000100122        wpsw = *Off ;
556100100122
556200100122       // Se cliente nuovo
556300100122        If Nuovo_cli = *on ;
556400100122       //   richiamo sempre il pgm x abilitare il cliente
556500100122           wpsw = *on ;
556600100122        else ;
556700100122       //   Se cliente non nuovo
556800100122       //   stampo se non abilitato o se non è figlio di papà abilitato
556900100607        vssksu = '0' + %editc(c05ksc:'X')  ;
557000100122        vssisv = 'TT' ;
557100100122        Setll (vssksu:vssisv) Tivss02l ;
557200100122        If  not %Equal(Tivss02l) ;
557300100122            Clear Tibs10ds ;
557400100122            d10tle = 'WW';
557500100122            d10paf = 'P' ;
557600100122            d10cod = c05ksc ;
557700100608            d10drf = datacor8 ;
557800100122            TIBS10R (tibs10ds);
557900100122            IF  d10err = *blanks and d10cop > 0;
558000100607                vssksu = %subst(%editc(d10cop:'X'):4:8);
558100100122                vssisv = 'TT' ;
558200100122                Setll (vssksu:vssisv) Tivss02l ;
558300100122                If not %Equal(Tivss02l) ;
558400100122                   wpsw = *On ;
558500100122                EndIf ;
558600100122            EndIf ;
558700100122            If  d10err <> *Blanks ;
558800100122                wpsw = *On ;
558900100122            EndIf;
559000100122        EndIf;
559100100122        EndIf;
559200100122
559300100122       If  wpsw = *On ;
559400100122           parric = 'S' ;
559500100122           parksc = %editc(c05ksc:'X');
559600100122           Kpjbu = Parm82 ;
559700100122           Tnta82r (Kpjba);
559800100122       EndIf ;
559900100122
560000100122       ENDSR ;
560100100426
560200100115       //--------------------------------------------------------------
560300100115       //?Reperimento Dati del job (Utente/Operativi).
560400100115       //--------------------------------------------------------------
560500100115       BEGSR DatiJob;
560600100115
560700100115         in(E) §AzUte;
560800100115         if NOT %error;
560900100115           in(E) §DatiUte;
561000100115         endif;
561100100115         if %error or RSut = *blanks;
561200100115           clear TIBS34ds;
561300100115           tibs34r(tibs34ds);
561400100115           in §AzUte;
561500100115           in §DatiUte;
561600100115         endif;
561700100115
561800100115       ENDSR;
561900100108
562000080206       //--------------------------------------------------------------
562100080206       //?Operazioni finali.
562200080206       //--------------------------------------------------------------
562300080206       BEGSR RoutEnd;
562400160803
562500160803       //?Chiudo file storicizzazione variazioni potenziale
562600160803         clear TRMK30DS;
562700160803         clear tncpo_30;
562800160803         clear tncpo1_30;
562900160803         clear ticpi_30;
563000160803         IMK30tla = 'C';
563100160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
563200090806
563300080206         *inLR = *on;
563400080206         return;
563500080206
563600080206       ENDSR;
563700080206
563800080206      /end-free
563900080206       //--------------------------------------------------------------
564000080206       //?Schiere a tempo di compilazione.
564100080206       //--------------------------------------------------------------
564200080206
564300080206** - $MSG -------------------------------------------------------------------*
564400090724Utente non abilitato alla gestione del potenziale                              1
564500091215Opzione non valida                                                             2
564600091215Non esistono attività aperte legate alla trattativa                            3
564700100115Data Errata                                                                    4
564800100115Inserire il numero spedizioni presunto affidamento                             5
564900100118Non si può convalidare un'offerta NON ACCETTATA                                6
565000100120Immettere la Partita IVA!                                                      7
565100100120Immettere il codice fiscale!                                                   8
565200100120Partita Iva già presente su altri potenziali. Enter per forzare                9
565300100120Cod.fiscale già presente su altri potenziali. Enter per forzare               10
565400100120Codice di Pagamento non codificato                                            11
565500100120ABI + CAB Obbligatori per pagamento con Ricevuta Bancaria                     12
565600100120Codice ABI Obbligatorio se immesso codice CAB                                 13
565700100120ABI + CAB Inesistenti: Premere ENTER per Forzare                              14
565800100120Codice CAB Obbligatorio se immesso codice ABI                                 15
565900100707Incongruenza data decorrenza con data scadenza preced.                        16
566000100120Offerta xxx scadenza xx/xx/xx ha TariffaPartic."XX"da eliminare entro xx/xx/xx 7
566100100121Errori nella ricerca abilitazioni utente: telefonare al CED di sede!!         18
566200100128Non si possono accettare più versioni della stessa offerta                    19
566300100128Prima di accettare un'offerta annullare o congelare le altre versioni         20
566400110112Per questo tipo trattativa non si accetta l'offerta ma si convalida           21
566500100312Non si possono convalidare più versioni della stessa offerta                  22
566600100312Prima di convalidare un'offerta annullare o congelare le altre versioni       23
566700100323Data errata minore della data odierna                                         24
566800100817Codice commerciale errato                                                     25
566900100817Inserire comm.le con lo stesso unificante del comm.le trattativa              26
567000100707Data Scadenza Incongruente con Data Decorrenza                                27
567100100707Tariffa in modifica da altro utente riprovare più tardi                       28
567200100805Effettuare una sola selezione  !!                                             29
567300100805Effettuare almeno una selezione !!                                            30
567400100914Commerciale errato o non in gestione all'utente                               31
567500110112Per questa trattativa non si possono inserire Info con F15                    32
567600140221ATTENZIONE!!! Prezzo base Fuel mai rilevato settimanalmente dal Ministero.    33
567700150416Codice pagamento utilizzabile SOLO dalla SEDE                                 34
