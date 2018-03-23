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
025600170410     d  Pro_pagamento                 1n   overlay(IndDspf : 55)
025700121128     d  Vis_comm                      1n   overlay(IndDspf : 74)
025800100114        // - Indicatori di errore window accettazione
025900100114     d  PosCurDpa                     1n   overlay(IndDspF : 41)
026000100114     d  PosCurNsa                     1n   overlay(IndDspF : 42)
026100100118     d  PosCurKsc                     1n   overlay(IndDspF : 43)
026200100118     d  PosCurDra                     1n   overlay(IndDspF : 44)
026300100118     d  PosCurIva                     1n   overlay(IndDspF : 45)
026400100118     d  PosCurCdf                     1n   overlay(IndDspF : 46)
026500100118     d  PosCurCdp                     1n   overlay(IndDspF : 47)
026600100118     d  PosCurAbi                     1n   overlay(IndDspF : 48)
026700100118     d  PosCurCab                     1n   overlay(IndDspF : 49)
026800100707     d  PosCurDstt                    1n   overlay(IndDspF : 71)
026900100707     d  PosCurDdto                    1n   overlay(IndDspF : 72)
027000100707     d  PosCurDsto                    1n   overlay(IndDspF : 73)
027100121129     d  PosCurKSC12                   1n   overlay(IndDspF : 75)
027200121129     d  PosCurCMM12                   1n   overlay(IndDspF : 76)
027300080206
027400090407     d WindDspF        ds                  inz
027500100708     d   WindDspFa             1     39    inz(*zero)
027600100708     d   WindDspFb            40     69    inz(*zero)
027700100708     d   WindDspFc            70     99    inz(*zero)
027800090319
027900100115     d wlbdat          ds                  inz
028000100115     d  g02dat                 1      8  0
028100100115     d  g02inv                 9     16  0
028200100115     d  g02err                17     17
028300100115     d  g02tgi                18     22  0
028400100115
028500091209      // - Parametri TNTA01R2
028600091209     d Ta01r2ds        ds                  inz
028700091209     d Ta01nrv                 2      8  0
028800091209     d ta01prg                 9     11  0
028900091209     d ta01ccc                12     15  0
029000091209     d ta01rgs                16     63
029100091209     d ta01ctr                64     66  0
029200091209     d ta01cli                67     73  0
029300100122     d ta01flg                74     74
029400100122      * ta01all = 'A' ---> rcd allocato
029500100122     d ta01all                95     95
029600100122     d ta01cpo               156    161P 0
029700100122     d* FLAG DI RITORNO
029800100122     d ta01rit               162    162
029900100122     d* PARIST = 'E' ---> SCATTO ISTAT INESISTENTE ERRORE
030000100122     d ta01ist               178    178
030100100122     d* PARTRAT = 'T' ---> Richiamo come trattativa
030200100122     d ta01trat              179    179
030300100122
030400100122      // - Richiamo pgm stampa testi lettere              - FNLV43R -
030500100122     d PARAM           DS
030600100122      // Numero visita e progressivo
030700100122     d  PARNRV                 3      9  0 INZ
030800100122      // Codice Cliente
030900100122     d  PARCLI                10     16  0
031000100122      // Codice cliente potenziale
031100100122     d  PARCPO                17     27  0 INZ
031200100122      // AMBIENTE STAMPA TESTI  ====>  "C" = CLIENTI
031300100122     d  PARAST                99     99    INZ('C')
031400100122      // COD TARIFFA E PROGRESSIVO PER STAMPA TARIFFA  -TNTA43R
031500100122     D  PA2CTR               100    102  0 INZ
031600100122     D  PARPRG               103    105  0 INZ
031700100122      // FLAG PROVENIENZA       ====>  "3" = CONVALIDA OFFERTE
031800100122     D  PARFLG               106    106    INZ('3')
031900100122      // RAGIONE SOCIALE
032000100122     D  PARRGS               107    152
032100091209
032200100122      * Richiamo Tnta82r abilitazione clienti ai servizi ON LINE
032300100122     d Parm82          ds
032400100122     d  parric                 1      1
032500100122     d  parksc                 2      8
032600100122
032700080206      // - Parametri ricevuti
032800080206     d KPJBA         e ds
032900091207     d TNTA56DS      e ds
033000080206
033100080206      // - Reperimento dati utente
033200080206     d TIBS34ds      e ds
033300080206     d dUte01        e ds
033400081020     d TRUL31DS      e ds
033500120712
033600120712       // -?Gestione rcd allocato?
033700120712     d TRUL82ds      e ds
033800160803
033900160803       // -?Storicizzazione variazioni
034000160803     d TRMK30DS      e ds
034100160803     d TNCPO_30      e ds                  extname(TNCPO00F) inz
034200160803     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
034300160803     d TICPI_30      e ds                  extname(TICPI00F) inz
034400100426
034500100111      // - Tabella CCO = Causale contatto
034600100111     d dcco          e ds                  inz
034700130806
034800130806      // - Tabella LAT = Autorizzazione
034900130806     d dlat          e ds                  inz
035000130806
035100130806      // - Ds date statistiche
035200130806     d dSdf          e ds
035300130806
035400130806      // - Tabella TTR = Tipi trattative
035500130806     d dttr          e ds                  inz
035600100111
035700100429      // - Tabella FA  = Condizioni di pagamento
035800100429     d dsfa          e ds                  inz
035900100429
036000100429      // - Tabella 4W  = Stato del credito
036100100429     d ds4w          e ds                  inz
036200100312
036300100122      // - Tabella 1T = Tipi note
036400100122     d ds1t          e ds                  inz
036500100121
036600100119      // - Ds CPORST Potenziale
036700100119     d dcpo01        e ds                  inz
036800090724
036900100617      // - Gestione Contatti clienti/potenziali
037000100201     d TNTA12DS      e ds                  inz
037100100201
037200090810      // - Gestione anagrafica clienti
037300090810     d tnta60ds      e ds                  inz
037400090317
037500150223
037600150223      // - Inglobamneto tariffe particolari e controlli
037700150223     d Tnta25ds      e ds
037800150223     d Tnta25tad     e ds                  inz
037900150223     d Tnta25tgc     e ds                  inz
038000150223     d Tnta25tpd     e ds                  inz
038100150223     d Tnta25tpt     e ds                  inz
038200150223
038300100122      // - Convalida dati tipo
038400100122     d tnta33ds      e ds                  inz
038500100122
038600100617      // - Gestione Note tariffe/offerte (10)
038700090813     d TNTA28DS      e ds                  inz
038800100617
038900100617      // - Gestione/interrogazione Informazioni trattativa
039000100617     d TNTA41ds      e ds                  inz
039100090814
039200100209      // - Annulla dati tipo
039300100209     d TNTA47ds      e ds
039400100420
039500100505      // - Creazione anagrafica in convalida
039600100505     d TNTA55ds      e ds
039700100505
039800100505      // -  Controllo autorizzazione
039900100121     d TNTAA1DS      e ds                  inz
040000100114
040100100209      // - Gestione nuove attività
040200100209     d TRMK17ds      e ds                  inz
040300100209
040400100209      // -  Gestione Note clienti/potenziali
040500100209     d TRMK20ds      e ds                  inz
040600100209
040700100209      // - Interrogazione info commerciali
040800100209     d TRMK50ds      e ds                  inz
040900100209
041000090814      // -  Controllo partita IVA
041100151015     d XCFIVA1DS     e ds                  inz
041200100420
041300100420      // - Calcolo e Controllo validità delle date
041400100420     d Xgiolavds     e ds                  inz
041500090813
041600091209      // - Ricerca/Controllo tabelle
041700091209     d TIBS02ds      e ds                  inz
041800091209     d   T02Mod      e                     inz('C')
041900100122
042000100209      // - Clienti unificanti
042100100209     d TIBS10ds      e ds
042200100209
042300100119       // - Reperimento dati anagrafici
042400100119     d TIBS69ds      e ds
042500100121     d DS_cnaco      e ds                  inz extname(CNACO00F) prefix(w_)
042600100121     d DS_cnind      e ds                  inz extname(CNIND00F) prefix(w_)
042700100121     d DS_cnclp      e ds                  inz extname(CNCLP00F) prefix(w_)
042800100121     d DS_fncls      e ds                  inz extname(FNCLS00F) prefix(w_)
042900100425
043000080206      //---------------------------------------------------------------
043100080206      //?Definizione variabili globali.
043200080206      //---------------------------------------------------------------
043300080206
043400080206      // - Flags booleani
043500080208     d $Fine           s               n   inz(*off)
043600100204     d $Fine2          s               n   inz(*off)
043700100204     d $Fine4          s               n   inz(*off)
043800100204     d $Fine5          s               n   inz(*off)
043900100624     d $Fine7          s               n   inz(*off)
044000100625     d $Fine8          s               n   inz(*off)
044100100728     d $Fine9          s               n   inz(*off)
044200100728     d $Fine10         s               n   inz(*off)
044300100817     d $Fine11         s               n   inz(*off)
044400080208     d $InzS01         s               n   inz(*on)
044500100113     d $Inzw03         s               n   inz(*off)
044600100118     d $InzS05         s               n   inz(*off)
044700080206     d $ErrGrave       s               n   inz(*off)
044800080207     d $EoF            s               n   inz(*off)
044900080208     d $RecOK          s               n   inz(*off)
045000100108     d $Offerta        s               n   inz(*off)
045100100115     d $Esitata        s               n   inz(*off)
045200100119     d Wtibs69         s               n   inz(*off)
045300100122     d Wta33           s               n   inz(*off)
045400100122     d Wpsw            s               n   inz(*off)
045500100120     d wforzaiva       s               n   inz(*off)
045600100429     d wforzacon       s               n   inz(*off)
045700100122     d Nuovo_cli       s               n   inz(*off)
045800100426     d Mai_codific     s               n   inz(*off)
045900110112     d TTR_coninfot    s               n   inz(*off)
046000110112     d TTR_accettaz    s               n   inz(*off)
046100120125     d w_ChiudiT       s               n   inz(*off)
046200121128     d wFine12         s               n   inz(*off)
046300121128     d wInzW12         s               n   inz(*off)
046301171214     d ClienteSofferenza...
046302171214     d                 s               n   inz(*off)
046303171220     d NuovoCliente    s               n   inz(*off)
046500080206
046600080206      // - Indici di schiera
046700080208     d xx              s              3  0 inz
046800080206
046900080207      // - Campi associati al video
047000090722     d $Video          s              2    inz('S1')
047100080208     d S01nrr          s              4  0 inz
047200091209     d W_SflPag        s              3  0 inz
047300130806     d*// wPag            s              4  0 inz
047400130806     d*// wRig            s              3  0 inz
047500100118     d S05nrr          s              4  0 inz
047600100118     d W_SflPag5       s              3  0 inz
047700100728     d S10nrr          s              4  0 inz
047800100728     d W_SflPag10      s              3  0 inz
047900080206
048000090401      // - Campi di comodo data
048100090401     d  data_eur       s               d   Datfmt(*eur)
048200090401     d  data_iso       s               d   Datfmt(*iso)
048300090401     d  data_dmy       s               d   Datfmt(*dmy)
048400090811     d  data_ymd       s               d   datfmt(*ymd)
048500090401
048600080206      // - Campi di comodo
048700090406     d wAbi            s                   like(UTEaut)  inz
048800100122     d  datacor8       s              8  0
048900100122     d  datacor6       s              6  0
049000100119     d wagenzia        s             16
049100100119     d wbanca          s             20
049200100119     d wpos            s              2  0
049300100119     d wpos1           s              2  0
049400100119     d wpos2           s              2  0
049500100120     d wdata           s              6  0
049600100121     d wdata8          s              8  0
049700100119     d wiva            s                   like(indiva)
049800100119     d wcdf            s                   like(indcdf)
049900100120     d saviva          s                   like(indiva)
050000100120     d savcdf          s                   like(indcdf)
050100100120     d cod_forzcdf     s                   like(cpocdf) inz
050200100120     d cod_forziva     s                   like(cpopiv) inz
050300100120     d savrag          s                   like(cporag)
050400100120     d codice          s                   like(indiva) inz
050500100518     d sav_codice      s                   like(indiva) inz
050600100120     d savabi          s                   like(indabi)
050700100120     d savcab          s                   like(indcab)
050800100119     d wsta            s                   like(indsta)
050900100122     d wprg            s                   like(tamprg)
051000100128     d sav_ctr         s                   like(s01ctr)
051100100128     d sav_ver         s                   like(s01prg)
051200100425     d Keynrv          s                   like(vofnrv)
051300100425     d Keyctr          s                   like(vofctr)
051400100805     d Newksc          s                   like(vofksc)
051500100805     d Oldksc          s                   like(vofksc)
051600130806     d sav_rgf         s                   like(CMMuni)
051700100120     d wmod            s              1
051800100121     d numrec          s              4  0
051900100202     d numrec2         s              4  0
052000100805     d numrec10        s              4  0
052100100805     d scelta_rcd      s              4  0
052200100805     d fil_cmm         s              3  0
052300090323     d
052400130319     d wDataSP         s              8  0
052500150417     d savcdp          s                   like(C05cdp)
052600090722
052700090724      // campi di comodo date
052800100115     d wrkdpa          s              8  0
052900100120     d wrkdra          s              8  0
053000100707     d wrkddtt         s              8  0
053100100707     d wrkdstt         s              8  0
053200100707     d wrkddto         s              8  0
053300100707     d wrkdsto         s              8  0
053400130319
053500130319       // -?Campi libreria e file per TITAS41C
053600130319     d wFLib           s             21
053700130319     d wLib            s             10
053800090406
053900091216      // - Campi di comodo per esitazione trattativa
054000091216     D  N_off_trat     s              7  0
054100091216     D  N_off_hold     s              7  0
054200091216     D  N_off_conv     s              7  0
054300100625     D  N_off_acce     s              7  0
054400100126     D  N_off_daesi    s              7  0
054500100107
054600100107      // - Campi di comodo per esitazione trattativa
054700100107     D  causale        s                   like(atccad)
054800091216     D
054900100817
055000100817      // - Campi di comodo decodifica commerciale
055100100817     D  comcmm         s                   like(c01cmm)
055200100817     D  comcmmd        s                   like(c01cmmd)
055300100818      // - Campi di comodo per convalida commerciale differente da trattativa
055400100818     D  comcon         s                   like(c01cmm)
055500120712
055600120712      // - Campi di comodo x allocazione file anagrafici
055700120712     d cnaco_rrn       s             15  0 inz
055800120712     d cnind_rrn       s             15  0 inz
055900120712     d cnclp_rrn       s             15  0 inz
056000120712     d fncls_rrn       s             15  0 inz
056100120712     d tfaco_rrn       s             15  0 inz
056200120712     d tfind_rrn       s             15  0 inz
056300120712     d tfclp_rrn       s             15  0 inz
056400120712     d tfcls_rrn       s             15  0 inz
056500120712     d wRrn            s                   like(UL82§rrn) inz
056600120712     d wFil            s                   like(UL82§fil) inz
056700120712     d wWin            s                   like(UL82§win) inz('S')
056800120712     d wF7             s                   like(UL82§f7)  inz('S')
056900080208
057000090508      //---------------------------------------------------------------
057100090508      //?Definizione procedure usate.
057200090508      //---------------------------------------------------------------
057300080206
057400100122      * Stampa testi lettera
057500100122     d FNLV43R         pr                  extpgm('FNLV43R')
057600100122     d  kpjba                              likeds(kpjba)
057700120510
057800120510      * Gestione tariffe
057900120510     d TNTA01R2        pr                  extpgm('TNTA01R2')
058000120510     d  kpjba                              likeds(kpjba)
058100100122
058200150223      * Controllo tariffe particolari e fuel
058300150223     d TNTA25R         pr                  extpgm('TNTA25R')
058400150223     d  kpjba                              likeds(kpjba)
058500150223     d  tnta25ds                           likeds(Tnta25ds)
058600150223     d  Tnta25tad                          likeds(Tnta25tad)
058700150223     d  Tnta25tgc                          likeds(Tnta25tgc)
058800150223     d  Tnta25tpd                          likeds(Tnta25tpd)
058900150223     d  Tnta25tpt                          likeds(Tnta25tpt)
059000150223
059100100122      * Abilitazione clienti ai servizi on line
059200100122     d TNTA82R         pr                  extpgm('TNTA82R')
059300100122     d  kpjba                              likeds(kpjba)
059400100122
059500100209      * Annullo dati tipo
059600100209     d TNTA47R         pr                  extpgm('TNTA47R')
059700100209     d  kpjba                              likeds(kpjba)
059800100420
059900100505      * Creazione anagrafica clienti in convalida
060000100505     d TNTA55R         pr                  extpgm('TNTA55R')
060100100505     d  kpjba                              likeds(kpjba)
060200100505     d  tnta55ds                           likeds(Tnta55ds)
060300160218
060400160218       // -?Ctrl tipologia Tariffa tra tariffe/offerte del cliente?
060500160218     d TNTAT1ds      e ds                  inz
060600160218      /copy gaitrasrc/srcProtoPR,TNTAT1R
060700120712
060800120712       // -?Gestione rcd allocato?
060900120712     d TRUL82R         pr                  extpgm('TRUL82R')
061000120712     d  trul82ds                           likeds(trul82ds)
061100100505
061200100420      // - Aggiunge/Toglie gg/mm dalla data
061300100420     d Xgiolav         pr                  extpgm('XGIOLAV')
061400100420     d  xgiolavds                          likeds(xgiolavds)
061500160803
061600160803       // -?Storicizzazione Variazioni Potenziale
061700160803     d TRMK30R         pr                  extpgm('TRMK30R')
061800160803     d  trmk30ds                           likeds(trmk30ds)
061900160803     d  tncpo_30                           likeds(tncpo_30)
062000160803     d  tncpo1_30                          likeds(tncpo1_30)
062100160803     d  ticpi_30                           likeds(ticpi_30)
062200100420
062300100420      //---------------------------------------------------------------
062400100420      //?prototipi
062500100420      //---------------------------------------------------------------
062600100420
062700080626      /copy gaitrasrc/srcprotopr,tibs02r
062800100122      /copy gaitrasrc/srcprotopr,tibs10r
062900080626      /copy gaitrasrc/srcprotopr,tibs34r
063000100119      /copy gaitrasrc/srcprotopr,tibs69r
063100100201      /copy gaitrasrc/srcprotopr,tnta12r
063200100114      /copy gaitrasrc/srcprotopr,tnta28r
063300100122      /copy gaitrasrc/srcprotopr,tnta33r
063400100617      /copy gaitrasrc/srcprotopr,tnta41r
063500100114      /copy gaitrasrc/srcprotopr,tnta60r
063600091022      /copy gaitrasrc/srcprotopr,trmk17c
063700100201      /copy gaitrasrc/srcprotopr,trmk20r
063800100201      /copy gaitrasrc/srcprotopr,trmk50r
063900130806      /copy gaitrasrc/srcprotoPI,TRMK43R_1
064000130806      /copy gaitrasrc/srcprotoPR,TRMK43R
064100081020      /copy gaitrasrc/srcprotopr,trul31r
064200100115      /copy gaitrasrc/srcprotopr,xsrda8
064300151015      /copy gaitrasrc/srcprotopr,xcfivar1
064400100119      /copy gaitrasrc/srcprotopr,tntaa1c
064500090508
064600080206      //---------------------------------------------------------------
064700080206      //?Definizione key-list.
064800080206      //---------------------------------------------------------------
064900080206
065000130806       // -?File AZCMM01L?
065100130806     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
065200130806     d                                     prefix(k_)
065300080206
065400080206      //---------------------------------------------------------------
065500080206      //?Riepilogo indicatori.
065600080206      //---------------------------------------------------------------
065700100201      // - Indicatori di visualizzazione
065800100201      // 12    : note 10 legate alle tariffe
065900100201      // 16    : F7 Anagrafica provvisoria
066000100201      // 51    : Proteggo il codice cliente
066100100201      // 52    : Visualizza i dati relativi al codice pagamento
066200100201      // 53    : Visualizza i dati relativi al codice fiscale
066300100201      // 54    : Lampeggia la parte dell'iva estera
066400090518      // - Comuni
066500080207      // 28    : Emissione messaggio di errore a video
066600091209      // - C01/S01
066700090722      // 20    : Condiziona SFLDSP    (*not)
066800090722      // 21    : Condiziona SFLDSPCTL (*not)
066900090722      // 20+21 : Condiziona SFLCLR
067000090722      // 22    : Condiziona SFLNXTCHG in S02
067100090722      // 23    : Condiziona SFLEND
067200100201      // - C05/S05
067300100201      // 30    : Condiziona SFLDSP    (*not)
067400100201      // 31    : Condiziona SFLDSPCTL (*not)
067500100201      // 30+31 : Condiziona SFLCLR
067600100201      // 32    : Condiziona SFLNXTCHG in S02
067700100201      // 33    : Condiziona SFLEND
067800100201
067900100201      // - posizionamento cursore
068000100201      // 41    : Errore: Posizione Data presunto affidamento
068100100201      // 42    : Errore: Posizione Spedizioni presunto affidamento
068200100201      // 43    : Errore: Posizione Codice cliente
068300100201      // 44    : Errore: Posizione Data reale affidamento
068400100201      // 45    : Errore: Posizione Iva
068500100201      // 46    : Errore: Posizione Codice fiscale
068600100201      // 47    : Errore: Posizione Codice pagamento
068700100201      // 48    : Errore: Posizione Codice ABI
068800100201      // 49    : Errore: Posizione Codice CAB
068900100201      // 50    : Errore: Posizione cursore opzioni subfile 1
069000080207      // - Comuni
069100080207      // 99    : Generico di Errore
069200080206      //---------------------------------------------------------------
069300080206
069400080206      //---------------------------------------------------------------
069500080206      //?M A I N - L I N E
069600080206      //---------------------------------------------------------------
069700080206
069800080206     c     *Entry        plist
069900080206     c                   parm                    KPJBA
070000091209     c                   parm                    tnta56ds
070100080206
070200080206      /free
070300080206
070400080206       // Operazioni iniziali
070500080206       exsr RoutInz;
070600090601
070700080206       // Gestione video
070800080206       DOW $Fine = *off;
070900080206         select;
071000100113       // Gestione video SELEZIONE
071100080206           when $Video = 'S1';
071200080206             exsr GesS01;
071300100113       // Gestione video conferma chiusura
071400100113           when $Video = 'W3';
071500100113             exsr GesW03;
071600100114       // Gestione video accettazione offerta
071700100114           when $Video = 'W4';
071800100114             exsr GesW04;
071900100118       // Gestione video convalida offerte
072000100118           when $Video = 'S5';
072100100118             exsr GesS05;
072200090428           other;
072300100204             $Fine = *on;
072400080206         endsl;
072500080206       ENDDO;
072600080206
072700080206       // Operazioni finali
072800080206       exsr RoutEnd;
072900080206
073000090722       //--------------------------------------------------------------
073100090722       //?Gestione videata S01
073200090722       //--------------------------------------------------------------
073300090722       BEGSR GesS01;
073400090722
073500090722         // Inizializzazione videata
073600090722         if  $InzS01 = *on;
073700090722            exsr InzS01;
073800090722            $InzS01  = *off;
073900090722         endif;
074000090722
074100090722         // Posizionamento cursore
074200090722         if  C01csr  > *zero;
074300090722           C01rcd = C01csr;
074400090722         endif;
074500090722
074600090722         // Emissione Testata e Piede con tasti funzionali abilitati
074700091209           write  TA56T01;
074800091209           write  TA56P01;
074900090722
075000090722
075100090722         // Emissione videata
075200091209         exfmt  TA56C01;
075300090722         reset errMessage;
075400090722         reset errGenerico;
075500090722         clear V1Dmsg;
075600090722
075700090722         SELECT;
075800090804
075900100201         // - F08=Note tariffa
076000100201           WHEN  dsp_aid = c_F08;
076100100201             exsr F08S01;
076200100201
076300100201         // - F02=Rubrica
076400100201           WHEN  dsp_aid = c_F02;
076500100201             exsr F02S01 ;
076600100201
076700100201         // - F7=Anagrafica
076800100201           WHEN  dsp_aid = c_F07;
076900100201             exsr F07S01;
077000100201
077100100201         // - F14=Informazioni Commerciali
077200100201           WHEN  dsp_aid = c_F14;
077300100201             exsr F14S01;
077400100617
077500100617         // - F15=Informazioni Trattaativa
077600100617           WHEN  dsp_aid = c_F15;
077700110112             If  TTR_coninfot = *off ;
077800110112                 errMessage  = *on;
077900110112                 errGenerico = *on;
078000110112                 V1Dmsg = $Msg(32);
078100110112                 leavesr;
078200110112             endif;
078300100617             exsr F15S01;
078400100201
078500100201         // - F18=Note
078600100201           WHEN  dsp_aid = c_F18;
078700100201             exsr F18S01 ;
078800100201
078900090804         // - F10=Inserisce nuove attività
079000090804           WHEN  dsp_aid = c_F10;
079100090804             exsr F10S01;
079200090722
079300090722         // - F12=Ritorno
079400090722           WHEN  dsp_aid = c_F12;
079500090722             exsr F12S01;
079600091123             if  errGenerico = *on;
079700091123               leavesr;
079800091123             endif;
079900090722
080000090722         // - Roll-Up
080100090722           WHEN  dsp_aid = c_RollUp;
080200090722
080300090722         // Invio
080400090722           OTHER;
080500090807         // leggo il subfile solo se ho dei records
080600090807             If s01nrr > *zeros ;
080700090807                exsr OpzS01;
080800100520                if  errGenerico = *on ;
080900100520                    leavesr;
081000100520                endif;
081100090807             endif;
081200100319         // verifico se devo richiedere la chiusura della trattativa
081300100625         // oppure la pulizia delle attività 30
081400100319         // Richiamo la routine che verifica ed esita la trattativa
081500100319             exsr Sr_EsiTrat ;
081600100625
081700100625       // Se non ho + OFFERTE da accettare o convalidare verifico se ho ancora
081800100625       // attività 30 aperte
081900100625             If N_off_acce = N_off_daesi or N_off_daesi = 0 ;
082000100625                exsr Sr_Att_30 ;
082100100625             endif ;
082200100625
082300100319       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
082400100319             If N_off_daesi= 0 ;
082500100319       // emetto la videata di richiesta di conferma
082600100319                $video = 'W3' ;
082700100319                $inzw03 = *on  ;
082800100319                leavesr ;
082900100319
083000100319             Endif ;
083100090811
083200090722         ENDSL;
083300090722
083400090722       ENDSR;
083500090722
083600100625       //--------------------------------------------------------------
083700100625       //?Verifico se ho delle attività 30 ancora da eseguire
083800100625       //--------------------------------------------------------------
083900100625       BEGSR sr_Att_30 ;
084000100625
084100100625       // verifico se esistono attività 30 ancora aperte per questa
084200100625       // visita
084300100625
084400100625         atccpo = ch1cpo ;
084500100625         atcksc = c01ksc ;
084600100625         atcnrv = c01nrv ;
084700100625         atcdco = 0 ;
084800100625         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
084900100625         reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
085000100625         dow not %eof(tiatc04l);
085100100625
085200100628       // se stessa attività che m ha portato qui non controllo
085300100628                  If atctat = ita56tat and atcatn = ita56atn and
085400100628                     atcatnp = ita56atnp ;
085500100628                  else ;
085600100628                     if atccad = '30' ;
085700100625       // se esiste almeno un'attività 30 richiedo a video se devo
085800100625       // cancellarla
085900100628                        exsr  Gesw08 ;
086000100628                        leavesr ;
086100100628                     endif ;
086200100625                  endif ;
086300100625             reade   (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
086400100625         enddo ;
086500100625
086600100625       ENDSR ;
086700100625
086800100201       //--------------------------------------------------------------
086900100201       //?Gestione tasto funzionale F14 da videata S01
087000100201       //?F14=Informazioni Commerciali
087100100201       //--------------------------------------------------------------
087200100201       BEGSR F14S01;
087300100201
087400100201         // Richiamo il pgm informazioni Commerciali
087500100201         clear     trmk50ds   ;
087600100201         i50cpo=ch1cpo ;
087700100201         i50mod='G'   ;
087800100201         i50rag=c01rag   ;
087900100201         i50ifotot = §cpoifotot ;
088000100201         i50pgm='TNTA56R' ;
088100100924         i50obl = 'S' ;
088200120125
088300120510       //?Se siamo in chiusura trattativa è una chiusura con esito positivo le
088400120510       //?info vanno richiamate con:
088500120510       //?--> trattativa di tipo N la % affido a BRT va obbligatoriamente
088600120510       //?    aumentata
088700120510       //?--> trattativa di tipo I la % affido a BRT va obbligatoriamente
088800120510       //?    aumentata solo se = 0 altrimenti è facoltativo l'aumento
088900120510         IF  VISesi = 'P' and w_ChiudiT;
089000120510           IF  VIStpv = 'N';
089100120510             I50atb = '+';
089200120510           ENDIF;
089300120510           IF  VIStpv = 'I';
089400120510             I50atb = '%';
089500120510           ENDIF;
089600120510         ENDIF;
089700100201
089800100201         TRMK50R  (kpjba:trmk50ds) ;
089900100201
090000100201       ENDSR;
090100100617
090200100617       //--------------------------------------------------------------
090300100617       //?Gestione tasto funzionale F15 da videata S01
090400100617       //?F15=Informazioni trattativa
090500100617       //--------------------------------------------------------------
090600100617       BEGSR F15S01;
090700100617
090800100617         // Richiamo il pgm informazioni Trattativa
090900100617         clear  TNTA41ds;
091000100617         I41nrv    = C01nrv;
091100100617         I41rag    = C01rag;
091200100617         I41ifotot = VISinfot;
091300120706         I41mod    = 'G';
091400100617         I41pgm    = 'TNTA56R';
091500101221         I41tpv    = VIStpv;
091600100617
091700100617         tnta41r (kpjba : TNTA41ds);
091800100617
091900100617         // Salvataggio campo delle infotrattative
092000101005         if O41f06 = 'S'and visdch = 0 ;
092100100630              visinfot = o41ifotot ;
092200100630              update tivis000 ;
092300100630       // rialloco il record
092400100630              chain  ita56nrv tivis05l ;
092500100617         endif ;
092600120706         if O41f06 = 'S'and visdch > 0 ;
092700120706            chain  ita56nrv tivis05l ;
092800120706               IF  %found(TIVIS05L);
092900120706                   VISinfot = o41ifotot ;
093000120706                   update TIVIS000;
093100120706               ENDIF;
093200120706         endif ;
093300100617
093400100617       ENDSR;
093500100201
093600100201       //--------------------------------------------------------------
093700100201       //?Gestione tasto funzionale F02 da videata S01
093800100201       //?F02=Rubrica
093900100201       //--------------------------------------------------------------
094000100201       BEGSR F02S01;
094100100201
094200100201
094300100201         if c01ksc = 0 ;
094400100201            clear tnta12ds   ;
094500100201            ta12apl='T'   ;
094600100201            ta12nrv=c01nrv ;
094700100201            ta12rag=c01rag ;
094800100201            TNTA12R  (kpjba:TNTA12ds);
094900100201            If ta12err <> ' ';
095000100201               errMessage  = *on;
095100100201               errGenerico = *on;
095200100201               V1Dmsg = ta12msg ;
095300100201               leavesr;
095400100201            endif;
095500100201
095600100201         else ;
095700100201
095800100201            clear tnta12ds   ;
095900100201            ta12apl='C'   ;
096000100201            ta12ksc=c01ksc ;
096100100201            TNTA12R  (kpjba:TNTA12ds);
096200100201            If ta12err <> ' ';
096300100201               errMessage  = *on;
096400100201               errGenerico = *on;
096500100201               V1Dmsg = ta12msg ;
096600100201               leavesr;
096700100201            endif;
096800100201
096900100201         endif;
097000100201
097100100201
097200100201       ENDSR;
097300100201
097400090722       //--------------------------------------------------------------
097500090722       //?Inizializzazione videata S01
097600090722       //--------------------------------------------------------------
097700090722       BEGSR InzS01;
097800090722
097900090722       // Pulizia subfile
098000091209         exsr pulsfl ;
098100100617
098200091111
098300090722       // valorizzo i campi del video
098400090722            c01nrv = visnrv ;
098500090722            c01rag = visrag ;
098600090807            c01ksc = visksc ;
098700090722            data_eur = %date(visdat:*iso);
098800090722            c01dta = %dec(data_eur);
098900100121            c01cmm = viscmm ;
099000100121            comcmm = c01cmm  ;
099100090722            exsr Dec_com ;
099200130806            c01cmmd = CMMdes ;
099300100817        // salvo l'unificante
099400130806            sav_rgf = CMMuni ;
099500100817
099600090722            c01tpv = vistpv;
099700090722            c01esi = visesi;
099800100424            esitata = (visesi <> ' ' ) ;
099900100424            tipo_nuova = (vistpv = 'N' ) ;
100000090722
100100090722        // decodifico il tipo trattativa
100200100312             clear dTTR;
100300100312             clear TIBS02ds;
100400100312             T02mod = 'C';
100500100312             T02cod = 'TTR';
100600100312             T02ke1 = vistpv   ;
100700100312             T02sif = KNSIF;
100800100312             TNTBE_RicercaControllo  (kpjba : tibs02ds);
100900100312             IF  T02err = *blanks;
101000100312                 dTTR = t02uni ;
101100100312                 c01tpvd = §ttrdes;
101200110112         // imposto i flag per gestione delle info e offerte
101300110112                 TTR_coninfot = §ttrinfo = 'S' ;
101400110112                 TTR_accettaz = §ttracc = 'S' ;
101500100312             endif ;
101600091209
101700091209         // recupero i codice del potenziale da tfaco se esiste il record
101800091209         // altrimenti da cnaco con il codice cliente
101900091209             acokut = 1 ;
102000091209             acokcc = dutkci ;
102100091209             acoksc = c01nrv ;
102200100804             chain    (acokut:acokcc:acoksc) Tfaco00f ;
102300091209             If not %found(tfaco00f)  ;
102400091209                acoksc = c01ksc ;
102500100804                chain    (acokut:acokcc:acoksc) Cnaco00f ;
102600091209                if not %found(cnaco00f) ;
102700091209                   clear acolib;
102800091209                endif ;
102900100201             else ;
103000100201                ana_provvis = *on ;
103100091209             endif ;
103200091209             ch1cpo = acolib ;
103300100119         // recupero dati informativi
103400100804           chain(n) (acolib) Tncpo01l ;
103500100119           If %found(Tncpo01l) ;
103600100119              dcpo01 = Cporst ;
103700100119           else;
103800100119              clear dcpo01 ;
103900100119           endif ;
104000110210         // verifico se il potenziale è un codificato o un mai codificato/perso
104100110210           Mai_codific = *off;
104200110210
104300110210           If CpoFls = 'C' ;
104400110210              Mai_codific = *off;
104500110502           else ;
104600110502              Mai_codific = *on ;
104700110210           endif ;
104800091209
104900090722
105000091209        // recupero le offerte legate
105100110112           setll c01nrv Tivof11l ;
105200091210
105300091209       // Caricamento dei dati da presentare nel sfl
105400091209             $EoF = *off;
105500091209         exsr sr_ReadRec;
105600091209
105700091209         dow  $EoF   = *off ;
105800091209           exsr RollUpS01;
105900091209         enddo;
106000091209
106100091209       // Impaginazione subfile
106200091209       // -> forza l'impaginazione sul 1° rec. del subfile
106300091209         if S01nrr > *zero;
106400091209           C01rcd  = 1;
106500091209           C01csr  = 1;
106600091209         else;
106700091209           clear C01rcd;
106800091209         endif;
106900090722
107000090722       ENDSR;
107100091215
107200090722       //--------------------------------------------------------------
107300090722       //?Decodifica Commerciale
107400090722       //--------------------------------------------------------------
107500090722       BEGSR Dec_com ;
107600090722
107700090722        // decodifico il commerciale
107800130806               chain  (ComCmm)  AZCMM000;
107900130806               if  %found(AZCMM01L);
108000130806                 ComCmmd = CMMdes;
108100090722               endif;
108200090722
108300090722       ENDSR ;
108400090722
108500090722       //--------------------------------------------------------------
108600090722       //?Pulizia SFL01
108700090722       //--------------------------------------------------------------
108800091209       BEGSR Pulsfl ;
108900090722
109000090722       // Pulizia subfile
109100091209         SflDsp_N    = *on;
109200091209         SflDspCtl_N = *on;
109300091209         write  TA56C01;
109400091209         SflDspCtl_N = *off;
109500091209         SflEnd      = *off;
109600090722
109700090722
109800091209         clear W_SflPag ;
109900090722         clear C01rcd;
110000090722         clear C01csr;
110100090722         clear S01nrr;
110200090722         clear V1Dmsg;
110300090722         errMessage  = *off;
110400090722         errGenerico = *off;
110500090722         WindDspF    = IndDspF;
110600090722         reset WindDspFb;
110700090722         IndDspF     = WindDspF;
110800090722
110900090722       ENDSR ;
111000090722
111100090722       //--------------------------------------------------------------
111200090722       //?Caricamento di tutto il sfl
111300090722       //--------------------------------------------------------------
111400090722       BEGSR RollUpS01;
111500090722
111600090722         clear  s01nrr;
111700091209         SflNxtChg = *off;
111800090722
111900090722         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
112000090722         DOW  $EoF   = *off  and
112100090722              S01nrr < *hival;
112200090722
112300090722         // - Caricamento dati nel record del subfile
112400090722           exsr  RieS01;
112500090722           If $recok = *on ;
112600090722              S01nrr += 1;
112700091209              write TA56S01;
112800090722           endif ;
112900090722
113000090722         // - Lettura record successivo nell'archivio
113100090722           exsr sr_ReadRec;
113200090722
113300090722         ENDDO;
113400090722
113500090722         // Visualizzazione del SFL (se ci sono dati)
113600091209         SflDsp_N = (S01nrr <= *zeros);
113700090722
113800090722         // Attivazione (eventuale) del SFLEND
113900091209         SflEnd  =  *on;
114000090722
114100090722         // Posizionamento cursore al 1° rcd della pagina
114200090722         if  S01nrr > *zero;
114300091209         //  wPag   = S01nrr / C_SflPag ;
114400091209         //  wRig   = S01nrr - (C_SflPag  * wPag );
114500091209         //  C01rcd = wPag  * C_SflPag ;
114600091209         //  if  wRig  > *zeros;
114700090722         //    C01rcd = C01rcd + 1;
114800090722         //  else;
114900091209         //    C01rcd = C01rcd - C_SflPag  + 1;
115000090722         //  endif;
115100090722             c01rcd = 1;
115200090722         else;
115300090722           clear  C01rcd;
115400090722         endif;
115500090722
115600090722         C01csr = C01rcd;
115700090722
115800090722       ENDSR;
115900090722
116000090722       //--------------------------------------------------------------
116100090722       //?Lettura record successivo
116200090722       //--------------------------------------------------------------
116300090722       BEGSR sr_ReadRec;
116400090722
116500091209             $RecOK  = *off;
116600090722
116700110112             reade(n) visnrv tivof11l ;
116800090722
116900110112             if  %eof(tivof11l) ;
117000091209                 $Eof  = *on;
117100091209             else ;
117200100422                 If vofeso <> '*' ;
117300100422                    $RecOK  = *on;
117400100422                 endif;
117500091209             endif;
117600091209
117700090722       ENDSR;
117800090722
117900090722       //--------------------------------------------------------------
118000090722       //?Caricamento singolo record nel SubFile S01
118100090722       //--------------------------------------------------------------
118200090722       BEGSR RieS01;
118300090722
118400091209         clear  ta56S01;
118500091209         s01ctr = vofctr ;
118600091209         s01prg = vofprg;
118700091209         s01tpt = voftpt ;
118800091209
118900091209         // verifico se esiste veramente il record tariffa relativo
119000091209
119100091209             tamksc = vofnrv ;
119200091209             tamctr = vofctr ;
119300091209             tamprg = vofprg ;
119400091214             chain(n) (tamksc:tamctr:tamprg) tnofm01l ;
119500091209             If not %found(tnofm01l) ;
119600091209                   sh1conv = 'S' ;
119700100126             else ;
119800100126                   sh1conv = ' ' ;
119900091209             endif;
120000091209
120100100107         // valorizzo campo esito offerta
120200100312         select ;
120300100312         when vofeso = 'H' ;
120400110118              s01eso = 'Congelata  ';
120500100312         when vofeso = 'A' ;
120600110118              s01eso = 'Accettata  ';
120700100312         when vofeso = 'C' ;
120800110118              s01eso = 'Convalidata';
120900100312         Endsl ;
121000100107
121100100312         sh1eso = vofeso ;
121200100312
121300091209         // valorizzo descrizione tipo tariffa
121400091209            xx = %lookup ((%subst(%editc(tamctr:'X'):1:1)):ctr) ;
121500091209            if xx > 0 ;
121600091209               s01dtr = dtr(xx) ;
121700091209            endif ;
121800100114
121900100114            s01dcv = tamdcv ;
122000091209
122100091209         // cerco se ci sono delle note legate  all'offerta
122200091209
122300091209                 ntcapl = 'T' ;
122400100112         // se già convalidata non cerco le note
122500100112                 if sh1conv = ' ' ;
122600100111                    ntcnk1 = %editc(dutkci: 'X') + %editc(vofnrv:'X');
122700091209                 endif ;
122800091209
122900091209                 ntcnk2 = %editc(vofctr: 'X') ;
123000091209                 ntctnt = '10' ;
123100091209                 setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
123200091209                 If %equal(tfntc01l) ;
123300091209                    s01not = 'SI' ;
123400091209                    note10_tar = *on ;
123500100112                    sh1n12 = '1' ;
123600091209                 else ;
123700091209                    note10_tar = *off ;
123800100112                    sh1n12 = ' ' ;
123900091209         // provo con chiave generica con solo la trattativa
124000091209                    clear ntcnk2 ;
124100091209                    setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
124200091209                    If %equal(tfntc01l) ;
124300091209                       s01not = 'SI' ;
124400091209                    else ;
124500091209                       s01not = '  ' ;
124600091209                    endif ;
124700091215                 EndIf ;
124800100112         // Numero spedizioni e data presunto affidamento se non convalidata
124900100112              if Sh1conv = ' ' ;
125000091209
125100091210                 If vofdpa <> *zeros ;
125200110118                    data_eur= %date(vofdpa:*iso) ;
125300110118                    s01dpa = %dec(data_eur)  ;
125400091210                    data_eur= %date(vofdpa:*iso) ;
125500100312                    sh1dpa = %dec(data_eur) ;
125600091210                 endif ;
125700100112              else ;
125800100112                 If vofdra <> *zeros ;
125900110118                    data_eur= %date(vofdra:*iso) ;
126000110118                    s01dpa = %dec(data_eur)  ;
126100100312                    data_eur= %date(vofdra:*iso) ;
126200100312                    sh1dpa = %dec(data_eur) ;
126300100112                 endif ;
126400100112              endif ;
126500091209       ENDSR;
126600090722
126700090804       //--------------------------------------------------------------
126800090804       //?Gestione tasto funzionale F10 da videata S01
126900090804       //?F10=Inserisci nuove attività
127000090804       //--------------------------------------------------------------
127100090804       BEGSR F10S01;
127200090804
127300091022         // Richiamo il pgm trmk17c
127400091022             clear trmk17ds ;
127500100107             imk17fcmt = '1'  ;
127600100107             imk17cmt = 'S'  ;
127700100107         // devo recuperare i dati da registrare nell'attività futura
127800100107             imk17cpo = ch1cpo;
127900100107             imk17ksc = c01ksc ;
128000100107             imk17nrv = c01nrv ;
128100100121             imk17cmm = c01cmm ;
128200100303             imk17co3 = ita56com ;
128300090804
128400091022             trmk17c (kpjba : trmk17ds) ;
128500100107             $inzs01 = *on ;
128600090804
128700090804       ENDSR;
128800080206
128900100201       //--------------------------------------------------------------
129000100201       //?Gestione tasto funzionale F08
129100100201       //?F08=Note Tariffe
129200100201       //--------------------------------------------------------------
129300100201       BEGSR F08S01;
129400100201
129500100201         // Richiamo il pgm gestione note tariffe/offerte
129600100201         clear tnta28ds ;
129700100201
129800100201         ita28ela = 'M' ;
129900100201         ita28tip = 'T' ;
130000100201         ita28cod = c01nrv ;
130100100201         ita28rsc = c01rag ;
130200100201         clear kpjbu ;
130300100201         kpjbu = tnta28ds ;
130400100201         tnta28r (kpjba) ;
130500100203         $inzs01 = *on ;
130600100201
130700100201       ENDSR;
130800100201
130900100201       //--------------------------------------------------------------
131000100201       //?Gestione tasto funzionale F18 da videata S01
131100100201       //?F18=Note
131200100201       //--------------------------------------------------------------
131300100201       BEGSR F18S01;
131400100201
131500100201         // Richiamo il pgm gestione note TRMK20R
131600100201         clear trmk20ds ;
131700110511         imk20tla = 'L' ;
131800100201         imk20flm = 'V';
131900100201         imk20cpo = ch1cpo ;
132000100201         imk20ksc = c01ksc ;
132100100201         imk20nrv = c01nrv ;
132200100201         imk20rsc = c01rag ;
132300100201         Trmk20r ( kpjba : trmk20ds );
132400100201
132500100201       ENDSR;
132600100201
132700100201       //--------------------------------------------------------------
132800100201       //?Gestione  anagrafiche provvisorie
132900100201       //?
133000100201       //--------------------------------------------------------------
133100100201       BEGSR F07S01  ;
133200100201
133300100201          clear Tnta60ds ;
133400100201          ta60flg = 'T';
133500100201          ta60nrv = c01nrv ;
133600100201          ta60cmm = c01cmm ;
133700100201
133800100201          tnta60r (kpjba:tnta60ds) ;
133900100201
134000100201       // gestisco i dati di ritorno
134100100201
134200100201          if ta60err <> *blanks ;
134300100201             errMessage  = *on;
134400100201             errGenerico = *on;
134500100201             V1Dmsg = ta60msg;
134600100201             leavesr;
134700100201          EndIf ;
134800100201
134900100201       ENDSR;
135000100201
135100080207       //--------------------------------------------------------------
135200080207       //?Gestione tasto funzionale F12 da videata S01
135300080207       //?F12=Ritorno
135400080207       //--------------------------------------------------------------
135500080207       BEGSR F12S01;
135600080207
135700100115         // Ritorno al programma chiamante e rolbk se non esitato
135800100115         If $esitata = *off ;
135900100115            ota56f12 = 'S';
136000100204            $fine  = *on ;
136100100817         endif ;
136200100115
136300091123         // verifico se ci sono delle attività aperte a fronte di questa
136400100817         // trattativa MA SOLO se è aperta altrimenti errore ed ho esitato
136500091123
136600100817         If visdch = 0 and $esitata = *on ;
136700100127            atccpo = ch1cpo ;
136800100127            atcksc = c01ksc ;
136900100127            atcnrv = c01nrv ;
137000100127            atcdco = 0 ;
137100100127              setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
137200100127              If not %equal ;
137300100817                 // errMessage  = *on;
137400100817                 // errGenerico = *on;
137500100817                 // V1Dmsg = $Msg(03);
137600100817                 Exsr Gesw11 ;
137700100914                setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
137800101005                If  not %EQUAL ;
137900100914                   leavesr;
138000100914                endif ;
138100100127              endif;
138200091123
138300100127         endif ;
138400100115
138500100115         $fine  = *on ;
138600080207
138700080207       ENDSR;
138800090810
138900100817
139000100817       //--------------------------------------------------------------
139100100817       //?Gestione videata W11 Guida alla creazione NUOVE ATTIVITA'
139200100817       //--------------------------------------------------------------
139300100817       BEGSR GesW11;
139400100817
139500100817         $fine11 = *off ;
139600100817
139700100817         dow $Fine11 =  *off ;
139800100817         // Emissione Finestra
139900100817             exfmt  TA56W11;
140000100817
140100100817         SELECT;
140200100817
140300100817         // - F10=Creazione attività
140400100817           WHEN  dsp_aid = c_F10;
140500100817                exsr F10S01 ;
140600100817                $inzs01 = *on ;
140700100817                $video = 'S1' ;
140800100817                $fine11 = *on ;
140900100817
141000100817         ENDSL;
141100100817       ENDDO;
141200100817
141300100817       ENDSR;
141400100817
141500090916       //--------------------------------------------------------------
141600091209       //?Gestione opzioni subfile S01
141700090916       //--------------------------------------------------------------
141800091209       BEGSR OpzS01;
141900090916
142000100202         numrec2= 0 ;
142100100202         Dow 1 = 1 ;
142200100202             numrec2 += 1;
142300100202             chain numrec2 ta56s01 ;
142400100202             If not %found(Tnta56d) ;
142500100202                leave ;
142600100202             endif ;
142700090916
142800100202                SflNxtChg= *off;
142900100202                WindDspF  = IndDspF;
143000100202                reset WindDspFb;
143100100202                IndDspF   = WindDspF;
143200100202                C01rcd    = S01nrr;
143300090916
143400090916           SELECT;
143500090916
143600090916             // - Nessuna opzione
143700091209             WHEN  S01opz  = *blank;
143800090916
143900090916             // - Precedente segnalazione di errore
144000100312             // WHEN  S01opz  = 'E';                 TOLTA IN QUANTO E ELIMINA
144100100312             //   clear S01opz;
144200090916
144300091209             // - 2 = Visualizzazione tariffa/ Manutenzione offerta
144400091209             WHEN  S01opz  = '2' and sh1conv <> 'S';
144500090916
144600090917                  clear ta01r2ds ;
144700090917
144800091209                  ta01ctr = s01ctr ;
144900091209                  ta01prg = s01prg ;
145000090916                  exsr Sr_ta01r2  ;
145100090916
145200100312             // - E = Elimina offerta
145300100312             WHEN  S01opz  = 'E' and sh1conv <> 'S';
145400091209
145500100113             // emetto la videata di richiesta di conferma
145600100204                  exsr GesW02 ;
145700091209
145800110112             // - A = Accetta offerta solo se il tipo trattativa lo prevede
145900110112             WHEN  S01opz  = 'A' and sh1conv <> 'S' and TTR_accettaz = *off ;
146000100312               errMessage  = *on;
146100100312               errGenerico = *on;
146200100312               PosCurOpz   = *on;
146300100312               V1Dmsg = $Msg(21);
146400100312
146500091209             // - A = Accetta offerta
146600091209             WHEN  S01opz  = 'A' and sh1conv <> 'S';
146700091209
146800100128             // verifico se ci sono altre versioni della stessa offerta
146900100128             // che devono essere annullate o congelate prima di accettare
147000100128             // una versione
147100100128                  exsr Sr_ctrAcc ;
147200100114
147300100128                  If errmessage = *off;
147400100114             // emetto la videata di richiesta di conferma
147500100204                     exsr Gesw04 ;
147600100128                  endif ;
147700100114
147800091209             // - C = Convalida Offerta
147900091209             WHEN  S01opz  = 'C' and sh1conv <> 'S';
148000100312
148100160219               // -?SE si sta convalidando un'offerta per una trattativa?
148200160219               //  ?diversa da "N" = Nuova ...?
148300160219               //*If  VIStpv <> 'N';
148400160219               If  C01tpv <> 'N';
148500160219
148600160219                 // -?Verifica se il cliente ha già tariffe con lo?
148700160219                 //  ?stesso codice MA con Tipo e/o Network diverso?
148800160219                 clear  TNTAT1ds;
148900160219                 iTAT1ksc  = C01ksc;
149000160219                 iTAT1tipo = *blank;
149100160219                 iTAT1ctr  = S01ctr;
149200160219                 select;
149300160219                   when  S01tpt = 'D';
149400160219                     iTAT1ntw  = 'DPD';
149500160222                   when  S01tpt = 'E';
149600160219                     iTAT1ntw  = 'EEX';
149700160219                   when  S01tpt = 'F';
149800160219                     iTAT1ntw  = 'FED';
149900160219                   //other;
150000160219                   //  clear  iTAT1ntw;
150100160219                 endsl;
150200160224                 iTAT1pgm  = SDSpgm;
150300160224                 KPJBU     = TNTAT1ds;
150400160219                 tntaT1r ( kpjba );
150500160224                 TNTAT1ds  = KPJBU;
150600160219                 if  oTAT1err = *on;
150700160219                   ErrGenerico = *on;
150800160219                   ErrMessage  = *on;
150900160219                   PosCurOpz   = *on;
151000160224                   V1Dmsg  = oTAT1msg;
151100160219                 endif;
151200160219
151300160219               EndIf;
151400160219
151500100312             // se si tratta di tipo trattativa di tipo A prima controllo
151600100312             // come se fossi in accettazione
151700110114             // se il tipo trattativa non prevede accettazione
151800110114             // faccio i controlli come se la facessero
151900160222                  if TTR_Accettaz = *off  and  ErrMessage   = *off;
152000100312                     exsr Sr_CtrAcc ;
152100100312                  endif ;
152200100312
152300100312                  If errmessage = *off;
152400100118             // Verifico che non ci siano degli errori e verifico se ci sono
152500100118             // altre offerte da convalidare
152600100312                     exsr Sr_ConvOff  ;
152700100312                  endif ;
152800091209
152900091209             // - H = Congela Offerta
153000100422             WHEN  S01opz  = 'H' and sh1conv <> 'S' and sh1eso <> 'C' ;
153100091209
153200091209                  exsr Sr_CongeOff  ;
153300091209
153400090916             // - ? = Opzione NON valida
153500090916             OTHER;
153600090916               errMessage  = *on;
153700090916               errGenerico = *on;
153800091209               PosCurOpz   = *on;
153900091215               V1Dmsg = $Msg(02);
154000090916
154100090916           ENDSL;
154200090916
154300090916           // Aggiornamento sfl
154400090916           select;
154500090916             when  errMessage  = *on;
154600091209               SflNxtChg  = *on;
154700091209               C01csr    = C01rcd;
154800090916             when  errGenerico = *on;
154900091209               C01csr    = C01rcd;
155000100118               clear  S01opz;
155100090916             other;
155200091209               C01csr    = C01rcd;
155300091209               clear  S01opz;
155400090916           endsl;
155500090916
155600100201           If  sh1n12 = '1' ;
155700100201               note10_tar = *on ;
155800100204           else ;
155900100204               note10_tar = *off ;
156000100201           endif ;
156100100201
156200100204           ///////////update TA56S01;
156300090916
156400090916           if  errMessage = *on  or  errGenerico = *on;
156500090916             leave;
156600090916           endif;
156700090916
156800090916         ENDDO;
156900100204
157000100204           if  errMessage = *off and errGenerico = *off ;
157100100204             $inzs01 = *on ;
157200100204           endif;
157300090916
157400090916       ENDSR;
157500090916
157600090814       //--------------------------------------------------------------
157700090814       //?Richiamo del TNTA01R2
157800090814       //--------------------------------------------------------------
157900090814       BEGSR sr_ta01r2 ;
158000090814
158100090814                   ta01nrv = c01nrv ;
158200090814                   ta01flg = '1' ;
158300090814                   ta01ccc = dutkci ;
158400090814                   ta01cli = c01ksc ;
158500090814                   ta01cpo = ch1cpo ;
158600090916                   ta01rgs = c01rag ;
158700090917                   ta01trat = 'T' ;
158800090814                   clear kpjbu ;
158900090814                   kpjbu = ta01r2ds ;
159000090923
159100091209                       Tnta01r2 (kpjba ) ;
159200090923
159300090814       ENDSR ;
159400091209
159500091209       //--------------------------------------------------------------
159600091209       //?Annullamento Offerta
159700091209       //--------------------------------------------------------------
159800091209       BEGSR sr_AnnOff ;
159900091209
160000091215
160100091215       // Richiamo la routine che deleta l'offerta
160200091215             exsr Sr_DelOfm ;
160300100108
160400100108       // Richiamo la routine che deleta le note 10 offerta  se le ha
160500100108       // a livello di offerta
160600100112             If   s01not = 'SI' and sh1n12 = '1' ;
160700100425                  KeyNrv = c01nrv ;
160800100425                  KeyCtr = s01ctr ;
160900100108                  exsr Sr_DelNote10;
161000100108             endif ;
161100091215
161200100107       // Cancello attività e note attività legate all'offerta
161300100111             $offerta = *on ;
161400100107             exsr Sr_Delatc ;
161500100111             $offerta = *off;
161600091215
161700100422       // Cancello record su Tivof se data presentazione offerta maggiore
161800100422       // dell'ultima statistica altrimenti annullo logicamente
161900100712       // non faccio + il controllo della data ma tengo tutte le offerte
162000110112            chain (tamksc:tamctr:tamprg) Tivof11l ;
162100110112            If %found(Tivof11l) ;
162200100712       //      If %dec(%subst(%EDITC(vofdpo:'X'):1:6):6:0) <= §sdfstcu ;
162300100422                  vofeso = '*' ;
162400100422                  update tivof000 ;
162500100712        //     else ;
162600100712        //        delete tivof000;
162700100712        //     endif ;
162800100115               $Esitata = *on ;
162900100107            endif;
163000100107
163100091216       // Richiamo la routine che verifica ed esita la trattativa
163200091215             exsr Sr_EsiTrat ;
163300100107
163400100112       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
163500100126             If N_off_daesi= 0 ;
163600100113       // emetto la videata di richiesta di conferma
163700100113                $video = 'W3' ;
163800100113                $inzw03 = *on  ;
163900100113                leavesr ;
164000100107
164100100107             Endif ;
164200100107
164300091215
164400100111          $inzs01 = *on  ;
164500100115          $video  = 'S1' ;
164600100111
164700091209       ENDSR ;
164800091215
164900091215       //--------------------------------------------------------------
165000091215       //?Conferma Annullamento offerta
165100091215       //--------------------------------------------------------------
165200091215       BEGSR GesW02;
165300100312
165400100312       // imposto i campi della videata
165500100312         w02ctr = s01ctr ;
165600100312         w02prg = s01prg ;
165700100312         w02tpt = s01tpt ;
165800100312         w02dtr = s01dtr ;
165900100312         w02dcv = s01dcv ;
166000100426         clear  w02conf ;
166100100312
166200100204         Dow $fine2 = *off ;
166300100112
166400091215         // Emissione Finestra di conferma
166500091215         exfmt  TA56W02;
166600091215         reset errMessage;
166700091215         reset errGenerico;
166800091215         clear V1Dmsg;
166900091215
167000091215         SELECT;
167100091215
167200091215         // - F12=Ritorno
167300091215           WHEN  dsp_aid = c_F12;
167400100126
167500100126                clear s01opz ;
167600100201
167700100201                If  sh1n12 = '1' ;
167800100201                    note10_tar = *on ;
167900100204                else ;
168000100204                    note10_tar = *off ;
168100100201                endif ;
168200100201
168300100126                update ta56s01 ;
168400100204                leave ;
168500091215
168600091215         // - F06=Conferma
168700091215           WHEN  dsp_aid = c_F06  ;
168800091215
168900091215         // Verifico se confermo annullamento
169000100503                If W02Conf = 'S' ;
169100100113                   exsr Sr_AnnOff ;
169200091215                endif ;
169300100126
169400100126                clear s01opz ;
169500100201
169600100204                If  sh1n12 = '1' ;
169700100204                    note10_tar = *on ;
169800100204                else ;
169900100204                    note10_tar = *off ;
170000100204                endif ;
170100100201
170200100126                update ta56s01 ;
170300100204                leave ;
170400100126
170500091215         ENDSL;
170600100112
170700100204         ENDDO;
170800091215
170900091215       ENDSR;
171000091215
171100100112       //--------------------------------------------------------------
171200100112       //?Richiesta chiusura trattativa
171300100112       //--------------------------------------------------------------
171400100112       BEGSR GesW03;
171500100112
171600120125         w_ChiudiT = *off;
171700100112
171800100112         // Emissione Finestra di conferma
171900100112         exfmt  TA56W03;
172000100112         reset errMessage;
172100100112         reset errGenerico;
172200100113         reset $Inzw03 ;
172300100112         clear V1Dmsg;
172400100112
172500100112         SELECT;
172600100112
172700100112
172800100112         // - F06=Conferma
172900100112           WHEN  dsp_aid = c_F06  ;
173000120125
173100120125           w_ChiudiT = *on;
173200100112
173300100112         // Verifico se confermo chiusura trattativa
173400100503             If W03Conf = 'S' ;
173500100115                If visesi = '  ' ;
173600100114         // verifico se ce nè almeno una convalidata è positiva altrimenti
173700100114         // è negativa
173800100115                   If N_Off_conv > 0 ;
173900100114                         visesi = 'P ' ;
174000100115                   else ;
174100100319                         visesi = '27' ;
174200100115                   endif ;
174300100115                endif ;
174400100113                   data_iso = (%date()) ;
174500100113                   visdch = %dec(data_iso) ;
174600100113                   update tivis000 ;
174700100114       // propongo una nuova attività di richiamo con causale 27 se esito
174800100114       // negativo
174900100319                If visesi = '27' ;
175000100115                   causale = '27' ;
175100100113       // Richiamo la routine che crea nuove attività
175200100115                   exsr Sr_CreaAtt ;
175300100112                endif ;
175400100924
175500100924         // richiamo i  programmi info trattativa e commerciali
175600110112         // le info le richiedo se la trattaiva le prevede
175700110112                If TTR_coninfot = *ON ;
175800110112                   exsr f15s01 ;
175900110112                Endif ;
176000110112
176100100924                exsr f14s01 ;
176200100115
176300100520                $Fine = *on;
176400100520             endif ;
176500100520
176600100520         // Verifico se confermo chiusura trattativa
176700100520             If W03Conf = 'N' ;
176800100520                $Fine = *on;
176900100520             endif ;
177000100112
177100100112         ENDSL;
177200100112
177300100112       ENDSR;
177400100112
177500091215       //--------------------------------------------------------------
177600100209       //?Deleto  Offerta e i suoi dati tipo
177700091215       //--------------------------------------------------------------
177800091215       BEGSR sr_DelOfm ;
177900091215
178000100209
178100100209         Tamksc = c01nrv ;
178200100209         Tamctr = s01ctr ;
178300100209         Tamprg = s01prg ;
178400100209
178500100209
178600100209         // annullo fatio tipo legati all'offerta
178700100209         clear tnta47ds ;
178800100209         d47tup = '2' ;
178900100209         d47cto = 'O' ;
179000100209         d47dsf = 'S' ;
179100100209         d47ksc = tamksc ;
179200100209         d47ctr = tamctr ;
179300100209         d47prg = tamprg ;
179400100209         kpjbu = tnta47ds ;
179500100209         tnta47r (kpjba) ;
179600100209
179700100209         chain (tamksc:tamctr:tamprg) Tiogc01l ;
179800100209         If %found(Tiogc01l) ;
179900100209            delete tiogc000;
180000100209         endif;
180100100209
180200100209         setll (tamksc:tamctr:tamprg) Tiopd01l ;
180300100209         reade (tamksc:tamctr:tamprg) Tiopd01l ;
180400100209         Dow not %eof ;
180500100209             delete  tiopd000 ;
180600100209             reade (tamksc:tamctr:tamprg) Tiopd01l  ;
180700100209         enddo ;
180800100209
180900100209
181000100209         setll (tamksc:tamctr:tamprg) Tiopt01l ;
181100100209         reade (tamksc:tamctr:tamprg) Tiopt01l ;
181200100209         Dow not %eof ;
181300100209             delete  tiopt000 ;
181400100209             reade (tamksc:tamctr:tamprg) Tiopt01l  ;
181500100209         enddo ;
181600100209
181700100209
181800100209         setll (tamksc:tamctr:tamprg) Tiofd01l ;
181900100209         reade (tamksc:tamctr:tamprg) Tiofd01l ;
182000100209         Dow not %eof ;
182100100209             delete  tiofd000 ;
182200100209             reade (tamksc:tamctr:tamprg) Tiofd01l ;
182300100209         enddo ;
182400100209
182500100209         chain (tamksc:tamctr:tamprg) tnofm01l ;
182600100209         If %found(tnofm01l) ;
182700100209            delete tnofm000;
182800100209         endif;
182900091215
183000091215       ENDSR ;
183100100114
183200100114       //--------------------------------------------------------------
183300100114       //?Richiesta Accettazione offerta
183400100114       //--------------------------------------------------------------
183500100114       BEGSR GesW04;
183600100114
183700100115         // Inizializzazione videata
183800100115            exsr Inzw04;
183900100115
184000100204         // Gestione video
184100100204         DOW $Fine4 = *off;
184200100114         // Emissione Finestra di accettazione offerta
184300100204             exfmt  TA56W04;
184400100204             reset errMessage;
184500100204             reset errGenerico;
184600100204             clear w04msg;
184700100114
184800100114         SELECT;
184900100114
185000100114         // - F12=Ritorno
185100100114           WHEN  dsp_aid = c_F12;
185200100204             $Fine4 = *on ;
185300100114
185400100114         // - F06=Accetta
185500100114           WHEN  dsp_aid = c_F06  ;
185600100115             Exsr SR_ctrw04 ;
185700100115             if  errGenerico = *on;
185800100204                 iter ;
185900100115             endif;
186000100115
186100100115         // aggiorno i record per l'accettazione
186200100115             exsr Sr_AccOff ;
186300100126
186400100126                clear s01opz ;
186500100204                If  sh1n12 = '1' ;
186600100204                    note10_tar = *on ;
186700100204                else ;
186800100204                    note10_tar = *off ;
186900100204                endif ;
187000100126                update ta56s01 ;
187100100126
187200100115
187300100115         // riemetto la  videata
187400100204             $inzs01 = *on  ;
187500100204             $Fine4 = *on ;
187600100115             $video  = 'S1' ;
187700100115
187800100115           OTHER ;
187900100115
188000100115             Exsr SR_ctrw04 ;
188100100115             if  errGenerico = *on;
188200100204                 iter   ;
188300100115             endif;
188400100114
188500100114         ENDSL;
188600100114
188700100204         ENDDO;
188800100204
188900100114       ENDSR;
189000100115
189100100115       //--------------------------------------------------------------
189200100115       //?Carico videata Accettazione Offerta
189300100115       //--------------------------------------------------------------
189400100115       BEGSR Inzw04 ;
189500100115
189600100115          clear Ta56w04 ;
189700100115          w04ctr = s01ctr ;
189800100115          w04prg = s01prg ;
189900100115          w04tpt = s01tpt ;
190000100115          w04dtr = s01dtr ;
190100100115          w04dcv = s01dcv ;
190200100312          w04dpa = sh1dpa ;
190300100503          w04sis = ' ' ;
190400100204          $fine4 = *off ;
190500100114
190600100115       ENDSR;
190700100115
190800100115       //--------------------------------------------------------------
190900100115       //?Controllo videata Accettazione Offerta
191000100115       //--------------------------------------------------------------
191100100115       BEGSR Sr_CtrW04 ;
191200100115
191300100115            WindDspF = IndDspF;
191400100115            reset WindDspFb;
191500100115            IndDspF  = WindDspF;
191600100115
191700100115        // verifico se inserito numero spedizione affidateci
191800100519        // campo non più obbligatorio NICOLA 19-05-10
191900100519         // If    W04Nsa = 0  ;
192000100519         //   errMessage  = *on;
192100100519         //   errGenerico = *on;
192200100519         //   PosCurNsa   = *on;
192300100519         //   w04msg = $Msg(05);
192400100519         //   leavesr;
192500100519         // endif;
192600100316
192700100316        // Controllo data presunto affidamento
192800100316           If    W04Dpa <> 0;
192900100316             clear wlbdat;
193000100316             g02dat = W04Dpa;
193100100316             xsrda8(wlbdat);
193200100316             if g02err = '1';
193300100316               errMessage  = *on;
193400100316               errGenerico = *on;
193500100316               PosCurDpa   = *on;
193600100316               w04msg = $Msg(04);
193700100316               leavesr;
193800100316             endif;
193900100316
194000100316        // Verfiico che non sia inferiore all'anno 2000 e superiore al 2039
194100100316           If    G02Inv < 20000101 or G02Inv > 20391231 ;
194200100316               errMessage  = *on;
194300100316               errGenerico = *on;
194400100316               PosCurDpa   = *on;
194500100316               w04msg = $Msg(04);
194600100316               leavesr;
194700100316             endif;
194800100316
194900100316             w04dpa = g02dat;
195000100316             wrkdpa = g02inv;
195100100316           else ;
195200100316             errMessage  = *on;
195300100316             errGenerico = *on;
195400100316             PosCurDpa   = *on;
195500100316             w04msg = $Msg(04);
195600100316             leavesr;
195700100316           endif;
195800100323
195900100323        // Verifico che non sia inferiore ad oggi
196000100323           If    G02Inv < datacor8 ;
196100100323               errMessage  = *on;
196200100323               errGenerico = *on;
196300100323               PosCurDpa   = *on;
196400100323               w04msg = $Msg(24);
196500100323               leavesr;
196600100323             endif;
196900100115
197000100115       ENDSR;
197100100115
197200100624       //--------------------------------------------------------------
197300100624       //?Segnalazione errore incongruenza date deorrenza e scadenza offerta
197400100624       //--------------------------------------------------------------
197500100624       BEGSR GesW07;
197600100624
197700100624         // Inizializzazione videata
197800100624            exsr Inzw07;
197900130321
198000130321       //?Imposto già l'errore se emetto la window per data decorrenza
198100130321       //?offerta minore data ultima spedizione fatturata
198200130321            IF  sh5ddt < wDataSp;
198300130321              ErrMessage  = *on;
198400130321              ErrGenerico = *on;
198500130321              PosCurDdto  = *on;
198600130321              W07msg = 'Decorrenza Offerta <= Ultima Sped. Fatt. ' +
198700130321                        %subst(%editc(wDataSP:'X'):7:2) + '/' +
198800130321                        %subst(%editc(wDataSP:'X'):5:2) + '/' +
198900130321                        %subst(%editc(wDataSP:'X'):1:4);
199000130321            ENDIF;
199100100624
199200100624         // Gestione video
199300100624         DOW $Fine7 = *off;
199400100625         // Emissione Finestra di incongruenza date offerta tariffa
199500100624             exfmt  TA56W07;
199600100624             reset errMessage;
199700100624             reset errGenerico;
199800100624             clear w07msg;
199900100624
200000100624         SELECT;
200100100624
200200100624         // - F12=Ritorno
200300100624           WHEN  dsp_aid = c_F12;
200400100713             errGenerico = *on;
200500100624             $Fine7 = *on ;
200600100624
200700100624         // - F06=Modifica tariffa
200800100624           WHEN  dsp_aid = c_F06  ;
200900100707                 exsr Sr_Ctrw07 ;
201000100707             if  errGenerico = *on;
201100100707                 iter   ;
201200100707             endif;
201300100707
201400100707         // aggiorno le date decorrenza
201500100707             exsr Sr_Aggdate ;
201600100707             if  errGenerico = *on;
201700100707                 iter   ;
201800100707             endif;
201900100708         // se non ci sono errori aggiorno la date del subfile
202000100708                // mi salvo la data decorrenza e scadenza tariffa
202100100708                   sh5ddt = WRKddto;
202200100708                   sh5dst = WRKdsto;
202300100708                   data_eur= %date(sh5ddt:*iso) ;
202400100708                   s05ddt = %dec(data_eur) ;
202500100708                   update ta56s05 ;
202600100707
202700100624                  $Fine7 = *on ;
202800100707
202900100707           OTHER ;
203000100707
203100100707             Exsr SR_ctrw07 ;
203200100707             if  errGenerico = *on;
203300100707                 iter   ;
203400100707             endif;
203500100624
203600100624         ENDSL;
203700100624
203800100624         ENDDO;
203900100624
204000100624       ENDSR;
204100100624
204200100624       //--------------------------------------------------------------
204300100624       //?Carico videata Segnalazione errore date decor/scad Offerta
204400100624       //--------------------------------------------------------------
204500100624       BEGSR Inzw07 ;
204600100624
204700100624          clear Ta56w07 ;
204800100624          w07ctr = s05ctr ;
204900100624          w07prg = sh5prg ;
205000100624          w07tpt = s05tpt ;
205100100624          w07dcv = s05dcv ;
205200100624
205300100624        // Imposto le date
205400100624          data_eur = %date(sh5ddt :*iso);
205500100624          w07ddto = %dec(data_eur)  ;
205600100624
205700100707          data_eur = %date(sh5dst :*iso);
205800100707          w07dsto = %dec(data_eur)  ;
205900100707
206000100624          data_eur = %date(tamdst :*iso);
206100100624          w07dstt = %dec(data_eur)  ;
206200100624
206300100707          data_eur = %date(tamddt :*iso);
206400100707          w07ddtt = %dec(data_eur)  ;
206500100707          wrkddtt = tamddt          ;
206600100707
206700100624          $fine7 = *off ;
206800100624
206900100624       ENDSR;
207000100624
207100100707       //--------------------------------------------------------------
207200100707       //?Controllo videata Incongruenza data decorrenza scadenza
207300100707       //--------------------------------------------------------------
207400100707       BEGSR Sr_CtrW07 ;
207500100707
207600100707            WindDspF = IndDspF;
207700100708            reset WindDspFc;
207800100707            IndDspF  = WindDspF;
208000100707
208100100707        // Controllo data scadenza tariffa
208200100707           If    W07Dstt <> 0;
208300100707             clear wlbdat;
208400100707             g02dat = W07Dstt;
208500100707             xsrda8(wlbdat);
208600100707             if g02err = '1';
208700100707               errMessage  = *on;
208800100707               errGenerico = *on;
208900100707               PosCurDstt  = *on;
209000100707               w07msg = $Msg(04);
209100100707               leavesr;
209200100707             endif;
209201171221
209202171221        // non posso avere date in negativo, caso di allineamento con '-'
209203171221           IF  G02dat < 0 or G02inv < 0;
209204171221             errMessage  = *on;
209205171221             errGenerico = *on;
209206171221             PosCurDstt  = *on;
209207171221             w07msg = $Msg(04);
209208171221             leavesr;
209210171221           ENDIF;
209300100707
209400100707             w07dstt = g02dat;
209500100707             wrkdstt = g02inv;
209600130319
209700130319       //?se <= data ultima spedizione fatturata
209800130319       //?non posso metterla più bassa di quella che c'è
209900130319       //?già sul file
210000130319         IF  TAMdst  <= wDataSP and wrkdstt <> TAMdst and
210100130319             wrkdstt <  TAMdst;
210200130319           ErrMessage  = *on;
210300130319           ErrGenerico = *on;
210400130319           PosCurDstt  = *on;
210500130320           W07msg = 'Scadenza Tariffa <= '+
210600130319                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
210700130319                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
210800130319                    %subst(%editc(wDataSP:'X'):1:4) + ' ' +
210900130319                    'non si può anticipare.';
211000130319           leavesr;
211100130319         ENDIF;
211200130319       //?se > data ultima spedizione fatturata
211300130319       //?posso impostarla solo >= alla data ultima sp.fatturata
211400130319         IF  TAMdst  > wDataSP and wrkdstt <> TAMdst and
211500130319             wrkdstt < wDataSP;
211600130319           ErrMessage  = *on;
211700130319           ErrGenerico = *on;
211800130319           PosCurDstt  = *on;
211900130320           W07msg = 'Scadenza Tariffa < Ultima Sped. Fatt. ' +
212000130319                    %subst(%editc(wDataSP:'X'):7:2) + '/' +
212100130319                    %subst(%editc(wDataSP:'X'):5:2) + '/' +
212200130320                    %subst(%editc(wDataSP:'X'):1:4);
212300130319           leavesr;
212400130319         ENDIF;
212500130319
212600100707           else ;
212700100707             errMessage  = *on;
212800100707             errGenerico = *on;
212900100707             PosCurDstt  = *on;
213000100707             w07msg = $Msg(04);
213100100707             leavesr;
213200100707           endif;
213300100707
213400100707
213500100707        // Controllo data decorrenza offerta
213600100707           If    W07Ddto <> 0;
213700100707             clear wlbdat;
213800100707             g02dat = W07Ddto;
213900100707             xsrda8(wlbdat);
214000100707             if g02err = '1';
214100100707               errMessage  = *on;
214200100707               errGenerico = *on;
214300100707               PosCurDdto  = *on;
214400100707               w07msg = $Msg(04);
214500100707               leavesr;
214600100707             endif;
214601171221
214602171221        // non posso avere date in negativo, caso di allineamento con '-'
214603171221           IF  G02dat < 0 or G02inv < 0;
214604171221             errMessage  = *on;
214605171221             errGenerico = *on;
214606171221             PosCurDdto  = *on;
214607171221             w07msg = $Msg(04);
214608171221             leavesr;
214609171221           ENDIF;
214700100707
214800100707             w07ddto = g02dat;
214900100707             wrkddto = g02inv;
215000130320       //?deve essere maggiore della data ultima sp. fatturata
215100130320           IF  wrkddto <= wDataSP;
215200130320             ErrMessage  = *on;
215300130320             ErrGenerico = *on;
215400130320             PosCurDdto  = *on;
215500130320             W07msg = 'Decorrenza Offerta <= Ultima Sped. Fatt. ' +
215600130320                       %subst(%editc(wDataSP:'X'):7:2) + '/' +
215700130320                       %subst(%editc(wDataSP:'X'):5:2) + '/' +
215800130320                       %subst(%editc(wDataSP:'X'):1:4);
215900130320             leavesr;
216000130320           ENDIF;
216100100707           else ;
216200100707             errMessage  = *on;
216300100707             errGenerico = *on;
216400100707             PosCurDdto  = *on;
216500100707             w07msg = $Msg(04);
216600100707             leavesr;
216700100707           endif;
216800100707
216900100707        // Controllo data scadenza offerta
217000100707           If    W07Dsto <> 0;
217100100707             clear wlbdat;
217200100707             g02dat = W07Dsto;
217300100707             xsrda8(wlbdat);
217400100707             if g02err = '1';
217500100707               errMessage  = *on;
217600100707               errGenerico = *on;
217700100707               PosCurDsto  = *on;
217800100707               w07msg = $Msg(04);
217900100707               leavesr;
218000100707             endif;
218001171221
218002171221        // non posso avere date in negativo, caso di allineamento con '-'
218003171221           IF  G02dat < 0 or G02inv < 0;
218004171221             errMessage  = *on;
218005171221             errGenerico = *on;
218006171221             PosCurDsto  = *on;
218007171221             w07msg = $Msg(04);
218008171221             leavesr;
218009171221           ENDIF;
218100100707
218200100707             w07dsto = g02dat;
218300100707             wrkdsto = g02inv;
218400100707           else ;
218500100707             errMessage  = *on;
218600100707             errGenerico = *on;
218700100707             PosCurDsto  = *on;
218800100707             w07msg = $Msg(04);
218900100707             leavesr;
219000100707           endif;
219100100707
219200100707              data_iso = %date(wrkdstt:*iso) ;
219300100707              data_iso = data_iso + %days(1);
219400100707              wdata8=%dec(data_iso);
219500100707         // verifico le date decorrenza e scadenza se corrette
219600100707              if wrkdstt > wrkddto or
219700100707         // data scadenza tariffa   + 1 diversa da data decorrenza
219800100707                 wdata8 <> wrkddto ;
219900100707
220000100707                 errMessage  = *on;
220100100707                 errGenerico = *on;
220200100707                 PosCurDstt  = *on;
220300100707                 w07msg = $Msg(16);
220400100707                 leavesr;
220500100707              endif;
220600100707
220700100707         // verifico le date decorrenza e scadenza tariffa se corrette
220800100707              if wrkdstt < wrkddtt ;
220900100707
221000100707                 errMessage  = *on;
221100100707                 errGenerico = *on;
221200100707                 PosCurDstt  = *on;
221300100707                 w07msg = $Msg(27);
221400100707                 leavesr;
221500100707              endif;
221600100707
221700100707         // verifico le date decorrenza e scadenza offerta se corrette
221800100707              if wrkdsto < wrkddto ;
221900100707
222000100707                 errMessage  = *on;
222100100707                 errGenerico = *on;
222200100707                 PosCurDsto  = *on;
222300100707                 w07msg = $Msg(27);
222400100707                 leavesr;
222500100707              endif;
222600100707
222700100707       ENDSR;
222800100707
222900100707       //--------------------------------------------------------------
223000100707       //?Aggiorno date decorrenza scadenza tariffa / offerta
223100100707       //--------------------------------------------------------------
223200100707       BEGSR Sr_Aggdate;
223300100707
223400100707       // se modificate le date decorrenza e scadenza tariffa aggiorno TNTAM
223500100707          If Tamddt <> wrkddtt or tamdst <> wrkdstt ;
223600100707             chain(e) (c05ksc:s05ctr:tamprg) Tntam01l ;
223700100707             If %error ;
223800100707                 errMessage  = *on;
223900100707                 errGenerico = *on;
224000100707                 PosCurDstt  = *on;
224100100707                 w07msg = $Msg(28);
224200100707                 leavesr;
224300100707             endif;
224400100707             If %found(tntam01l) ;
224500100707                tamddt = wrkddtt ;
224600100707                tamdst = wrkdstt ;
224700100707                tamdtr = datacor8 ;
224800100707                tamftr = ' ' ;
224900100707                tamduv = datacor8 ;
225000100707                update tntam000 ;
225100100707             endif ;
225200100707          endif ;
225300100707
225400100707       // se modificate le date decorrenza e scadenza offerta aggiorno TNOFM
225500100707          If s05ddt <> w07ddto or sh5dst <> w07dsto ;
225600100707             chain(e) (c01nrv:s05ctr:sh5prg) tnofm01l ;
225700100707             If %error ;
225800100707                 errMessage  = *on;
225900100707                 errGenerico = *on;
226000100707                 PosCurDdto  = *on;
226100100707                 w07msg = $Msg(29);
226200100707                 leavesr;
226300100707             endif;
226400100707             If %found(tnofm01l) ;
226500100707                tamddt = wrkddto ;
226600100707                tamdst = wrkdsto ;
226700100707                tamdtr = datacor8 ;
226800100707                tamftr = ' ' ;
226900100707                tamduv = datacor8 ;
227000100707                update tnofm000 ;
227100100707             endif ;
227200100707          endif ;
227300100707
227400100707       ENDSR;
227500100707
227600100625       //--------------------------------------------------------------
227700100625       //?Segnalazione di attività 30 da annullare
227800100625       //--------------------------------------------------------------
227900100625       BEGSR GesW08;
228000100625
228100100625         // Inizializzazione videata
228200100625            exsr Inzw08;
228300100625
228400100625         // Gestione video
228500100625         DOW $Fine8 = *off;
228600100625         // Emissione Finestra di attività 30 da annullare
228700100625             exfmt  TA56W08;
228800100625             reset errMessage;
228900100625             reset errGenerico;
229000100625
229100100625         SELECT;
229200100625
229300100625         // - F06=Conferma annullamento attività
229400100628           WHEN  dsp_aid = c_F06  ;
229500100628                 if w08conf = 'S' ;
229600100625          // richiamo la routine che deleta le attività 30 e le sue note
229700100628                    Exsr Sr_Del_30 ;
229800100625                  $Fine8 = *on ;
229900100628                 endif ;
230000100628
230100100628                 if w08conf = 'N' ;
230200100628          // richiamo la routine che deleta le attività 30 e le sue note
230300100628                  $Fine8 = *on ;
230400100628                 endif ;
230500100625
230600100625         ENDSL;
230700100625
230800100625         ENDDO;
230900100625
231000100625       ENDSR;
231100100625
231200100625       //--------------------------------------------------------------
231300100625       //?Carico videata Segnalazione attività 30 da annullare
231400100625       //--------------------------------------------------------------
231500100625       BEGSR Inzw08 ;
231600100625
231700100625         clear Ta56w08 ;
231800100625         clear TIBS02ds;
231900100625         clear dCCO;
232000100625         T02mod = 'C';
232100100625         T02cod = 'CCO';
232200100625         T02ke1 = '30';
232300100625         T02sif = KNSIF;
232400100625         TNTBE_RicercaControllo (kpjba : tibs02ds);
232500100625         IF  T02err = *blanks;
232600100625           dCCO = T02uni;
232700100625         ENDIF;
232800100625         W08att = '30 - ' + §CCOdes;
232900100625         W08att = %trim(W08att) + ' aperte ?';
233000100625         W08conf = ' ';
233100100625
233200100625          $fine8 = *off ;
233300100625
233400100625       ENDSR;
233500091216       //--------------------------------------------------------------
233600091216       //?Verifica situazione Trattativa ed esitazione trattativa
233700091216       //--------------------------------------------------------------
233800091216       BEGSR sr_EsiTrat ;
233900091216
234000100107         clear N_off_trat;          // numero totale offerte
234100100107         clear N_off_hold;          // numero offerte congelate
234200100107         clear N_off_conv;          // numero offerte convalidate
234300100625         clear N_off_acce;          // numero offerte accettate
234400100126         clear N_off_daesi;         // numero offerte da esitare
234500100107
234600100107       // verifico  le offerte legate alla trattativa
234700091216         vofnrv = c01nrv ;
234800110112         setll vofnrv Tivof11l ;
234900110112         reade(n) vofnrv Tivof11l ;
235000110112         dow not %eof(tivof11l) ;
235100100107
235200100422       // se non annullata sommo il numero delle offerte per trattativa
235300100422             If vofeso <> '*' ;
235400100422                N_off_trat = N_off_trat + 1 ;
235500100422             endif ;
235600100111       // congelate
235700100107             If vofeso = 'H' ;
235800100107                N_off_hold = N_off_hold + 1 ;
235900100107             endif ;
236000100111       // convalidate
236100100107             If vofeso = 'C' ;
236200100107                N_off_conv = N_off_conv + 1 ;
236300100107             endif ;
236400100625       // accettate
236500100625             If vofeso = 'A' ;
236600100625                N_off_acce = N_off_acce + 1 ;
236700100625             endif ;
236800100126        // accettate oppure ancora da esitare
236900100126             If vofeso = 'A' or vofeso = ' ' ;
237000100126                N_off_Daesi = N_off_daesi + 1 ;
237100100107             endif ;
237200091216
237300110112             reade(n) vofnrv Tivof11l ;
237400100107         enddo ;
237500091216       ENDSR ;
237600091216
237700100107
237800100107       //--------------------------------------------------------------
237900100108       //?Deleto  le attività legate alla singola trattativa o offerta
238000100107       //--------------------------------------------------------------
238100100107       BEGSR sr_DelAtc ;
238200100108
238300100108       // verifico se devo cancellare le sole attività legate all'offerta
238400100108       // oppure alla trattativa in genere
238500100108
238600100108         atccpo = ch1cpo ;
238700100108         atcksc = c01ksc ;
238800100108         atcnrv = c01nrv ;
238900100108         atcdco = 0 ;
239000100108         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
239100100127         reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
239200100108         dow not %eof(tiatc04l);
239300100108
239400100108             SELECT ;
239500100111       // solo attività offerta ?
239600100111             WHEN $offerta = *on ;
239700100111                  If atcctr <> *blank and atcprg <> *blank  and
239800100111                     s01ctr = %dec(atcctr:3:0) and s01prg = %dec(atcprg:3:0) ;
239900100127                     delete (atctat:atcatn:atcatnp) tiatc001 ;
240000100203       // cancello eventuali legami attività
240100100127                     delete (atcatn:atcatnp) tiatl01l ;
240200100111       // se l'attività era un progressivo uno vado a cancellare le note
240300100111       // con progressivo 0
240400100108                     If atcatnp = 1 ;
240500100127                        exsr Sr_DelNote ;
240600100108                     endif ;
240700100108                  endif ;
240800100111       // tutte attività trattativa  ?
240900100108             OTHER ;
241000100127                  delete (atctat:atcatn:atcatnp) tiatc001 ;
241100100125       // cancello eventuali legami attività
241200100127                  delete (atcatn:atcatnp) tiatl01l ;
241300100111       // se l'attività era un progressivo 1   vado a cancellare le note
241400100111       // con progressivo 0
241500100108                  If atcatnp = 1 ;
241600100127                     exsr Sr_DelNote ;
241700100108                  endif ;
241800100108
241900100108             endsl ;
242000100108
242100100127             reade   (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
242200100108         enddo ;
242300100108
242400100107       ENDSR ;
242500100108
242600100108       //--------------------------------------------------------------
242700100108       //?Deleto  le note legate alle attività ancora da eseguire !!!!!
242800100108       //--------------------------------------------------------------
242900100108       BEGSR sr_DelNote;
243000100108
243100100108       // Deleto le sole note relative ad una attività da fare con progressivo
243200100108       // 1
243300100108
243400100108         cpncpo = atccpo ;
243500100108         cpntat = atctat ;
243600100108         cpnatn = atcatn ;
243700100108         cpnatnp= 0 ;
243800100108         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
243900100108         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
244000100108         dow not %eof(ticpn03l);
244100100108             delete ticpn000;
244200100108             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
244300100108         enddo ;
244400100108
244500100108       ENDSR ;
244600100108
244700100108       //--------------------------------------------------------------
244800100108       //?Deleto  le note 10
244900100108       //--------------------------------------------------------------
245000100108       BEGSR sr_DelNote10;
245100100108
245200100108       // Deleto le sole note 10 legate all'offerta se non esistono altri
245300100108       // progressivi
245400100108
245500100108       // prima verifico se ho altri progressivi
245600100108
245700100425         setll (Keynrv:Keyctr) Tnofm01l ;
245800100108         If not %equal ;
245900100108
246000100108       // preparo la chiave
246100100108            ntcapl = 'T' ;
246200100425            ntcnk1 = %editc(dutkci: 'X') + %editc(Keynrv:'X');
246300100425            ntcnk2 = %editc(Keyctr: 'X') ;
246400100108            ntctnt = '10' ;
246500100108            setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
246600100108            reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
246700100108            dow not %eof(tfntc01l) ;
246800100111                delete tfntc;
246900100108                reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
247000100108            enddo ;
247100100108
247200100108         endif ;
247300100108
247400100108       ENDSR ;
247500100108
247600100628
247700100628       //--------------------------------------------------------------
247800100628       //?Deleto  le attività 30 legata alla trattativa
247900100628       //--------------------------------------------------------------
248000100628       BEGSR sr_Del_30 ;
248100100628
248200100628
248300100628         atccpo = ch1cpo ;
248400100628         atcksc = c01ksc ;
248500100628         atcnrv = c01nrv ;
248600100628         atcdco = 0 ;
248700100628         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
248800100628         reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
248900100628         dow not %eof(tiatc04l);
249000100628
249100100628             SELECT ;
249200100628       // se si tratta della stessa attività che mi ha portato qui non deleto
249300100628             When atctat=ita56tat and atcatn=ita56atn and atcatnp=ita56atnp;
249400100628
249500100628             When atccad = '30' ;
249600100628                  delete (atctat:atcatn:atcatnp) tiatc001 ;
249700100628       // cancello eventuali legami attività
249800100628                  delete (atcatn:atcatnp) tiatl01l ;
249900100628       // se l'attività era un progressivo 1   vado a cancellare le note
250000100628       // con progressivo 0
250100100628                  If atcatnp = 1 ;
250200100628                     exsr Sr_DelNote ;
250300100628                  endif ;
250400100628
250500100628             endsl ;
250600100628
250700100628             reade   (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
250800100628         enddo ;
250900100628
251000100628       ENDSR ;
251100100628
251200091209       //--------------------------------------------------------------
251300091209       //?Accettazione Offerta
251400091209       //--------------------------------------------------------------
251500091209       BEGSR sr_AccOff ;
251600091209
251700100115       // aggiorno l'esito in tivof ea anche i dati di presunto
251800100115       // affidamento
251900100115
252000100115         Tamksc = c01nrv ;
252100100115         Tamctr = s01ctr ;
252200100115         Tamprg = s01prg ;
252300110112         chain (tamksc:tamctr:tamprg) tivof11l ;
252400110112         If %found(tivof11l) ;
252500100115            vofeso = 'A ' ;
252600100115            vofdpa = wrkdpa  ;
252700100115            vofnsa = w04nsa  ;
252800100115            update tivof000 ;
252900100115            $Esitata = *on ;
253000100115         endif ;
253100100126
253200100126       // aggiorno copia firmata su tnofm
253300100126         chain (tamksc:tamctr:tamprg) tnofm01l ;
253400100126         If %found(tnofm01l) ;
253500100126            tamsis = w04sis ;
253600100126            update tnofm000;
253700100126         endif ;
253800100126
253900100115
254000100115       // prima di scrivere una nuova attività legata alla offerta in questione
254100100115       // devo annullare eventuali attività già legate all'offerta
254200100115         $offerta = *on ;
254300100115         exsr Sr_DelAtc ;
254400100115         $offerta = *off;
254500100115
254600100115       // propongo una nuova attività di richiamo con causale 31 cliente
254700100115       // caldo
254800100115         causale = '31' ;
254900100115
255000100426       // Richiamo la routine che crea nuove attività
255100100426               exsr Sr_CreaAtt ;
255200100426
255300100115
255400091209       ENDSR ;
255500100118
255600091209       //--------------------------------------------------------------
255700091209       //?Congelamento Offerta
255800091209       //--------------------------------------------------------------
255900091209       BEGSR sr_CongeOff ;
256000091209
256100100113         // Aggiorno il record tivof
256200100113
256300100113         Tamksc = c01nrv ;
256400100113         Tamctr = s01ctr ;
256500100113         Tamprg = s01prg ;
256600110112         chain (tamksc:tamctr:tamprg) tivof11l ;
256700110112         If %found(tivof11l) ;
256800100422            if vofeso <> '*' and vofeso <> 'C';
256900100422               vofeso = 'H' ;
257000100114               data_iso = (%date()) ;
257100100114               vofdch = %dec(data_iso) ;
257200100114               update tivof000 ;
257300100115               $Esitata = *on ;
257400100114            endif ;
257500100114         endif ;
257600100114
257700100128       // Cancello attività e note attività legate all'offerta
257800100128             $offerta = *on ;
257900100128             exsr Sr_Delatc ;
258000100128             $offerta = *off;
258100100128
258200100114         // verifico in che situazione si trova la trattativa
258300100114             exsr Sr_EsiTrat ;
258400100114
258500100114       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
258600100126             If N_off_daesi= 0 ;
258700100114       // emetto la videata di richiesta di conferma
258800100114                $video = 'W3' ;
258900100114                $inzw03 = *on  ;
259000100114                leavesr ;
259100100114
259200100114             Endif ;
259300100114
259400100114
259500100114          $inzs01 = *on  ;
259600100115          $video  = 'S1' ;
259700091209       ENDSR ;
259800091209
259900100108       //--------------------------------------------------------------
260000100312       //?Propongo la creazione di una nuova attività  solo se sono
260100100312       //?su trattativa senza codice cliente
260200100108       //--------------------------------------------------------------
260300100111       BEGSR Sr_CreaAtt ;
260400100312
260500100425         // verifico se trattativa solo su potenziale e mai codificato
260600110117         // oppure devo scrivere l'attività 31
260700110117        If (c01ksc = 0  and mai_codific = *on) or
260800110117           (causale = '31') ;
260900100108
261000100108         // Richiamo il pgm trmk17c
261100100108             clear trmk17ds ;
261200100108             imk17fcmt = '1'  ;
261300100108             imk17cmt = 'S'  ;
261400100108         // devo recuperare i dati da registrare nell'attività futura
261500100108             imk17cpo = ch1cpo;
261600100108             imk17ksc = c01ksc ;
261700100108             imk17nrv = c01nrv ;
261800100121             imk17cmm = c01cmm ;
261900100303             imk17co3 = ita56com ;
262000100111
262100100115         // Se la causale è valorizzata propongo i campi nella ds
262200100111            If causale <> '  ' ;
262300100111
262400100111               clear  dCCO;
262500100111               clear  TIBS02ds;
262600100111               T02mod = 'C';
262700100111               T02cod = 'CCO';
262800100111               T02ke1 = causale ;
262900100111               T02sif = KNSIF;
263000100111               TNTBE_RicercaControllo  (kpjba : tibs02ds);
263100100111               if  T02err = *blanks;
263200100111                   dcco = t02uni ;
263300100111               endif;
263400100111             // se è valorizzato il tipo attività associata la propongo
263500100111             // proteggendola
263600100111               If  §ccoatt <> ' ' ;
263700100111                   imk17att = §ccoatt ;
263800100111                   imK17patt = 'S' ;
263900100111               endif ;
264000100108
264100100111             // valorizzo la causale e la proteggo
264200100111               imk17cau = causale ;
264300100111               imk17pcau = 'S' ;
264400100111
264500100115               If causale = '27' ;
264600100115             // se la causale 27
264700100111             // se ci sono giorni di scadenza futuri li propongo nel calcolo
264800100111             // data scadenza
264900100924                  If §ccoupm <> ' ' and
265000100924                     §ccogio > 0;
265100100420                     clear XGIOLAVDS;
265200100420                     IXGLdata = %dec(%date());
265300100924                     If §ccoupm = '+' ;
265400100924                        IXGLadd  = 'S';
265500100924                     else ;
265600100924                        IXGLsub  = 'S';
265700100924                     endif;
265800100420                     IXGLgg   = §CCOgio;
265900100420                     IXGLpa   = 'P';
266000100420                     If  c01cmm > *zeros;
266100100420                         IXGLfil = %dec(%subst(%editc(c01cmm:'X'):1:3):3:0);
266200100420                     else ;
266300100420                         IXGLfil = DUTpou;
266400100420                     Endif;
266500100420                     XGIOLAV (xgiolavds);
266600100420                     If  OXGLerr = *blanks;
266700100420                         imk17dad = OXGLdata;
266800100420                     Endif;
266900100115                  endif;
267000100924                  If §ccoupm = '=' ;
267100100924                    imk17dad = %dec(%date());
267200100924                  Endif ;
267300100127                  imk17nrv = 0 ;
267400100224             // inibisco F12
267500100224                  imk17f12 = 'S' ;
267600100115               endif;
267700100115
267800100115               If causale = '31' ;
267900100115             // se la causale 31
268000100115             // la data futura viene calcolata sommando 15 alla data di presunta
268100100115             // affidamento
268200100924                  If §ccoupm <> ' ' and
268300100924                     §ccogio > 0;
268400100420                     clear XGIOLAVDS;
268500100420                     IXGLdata = vofdpa ;
268600100924                     If §ccoupm = '+' ;
268700100924                        IXGLadd  = 'S';
268800100924                     else ;
268900100924                        IXGLsub  = 'S';
269000100924                     endif;
269100100420                     IXGLgg   = §CCOgio;
269200100420                     IXGLpa   = 'P';
269300100420                     If  c01cmm > *zeros;
269400100420                         IXGLfil = %dec(%subst(%editc(c01cmm:'X'):1:3):3:0);
269500100420                     else ;
269600100420                         IXGLfil = DUTpou;
269700100420                     Endif;
269800100420                     XGIOLAV (xgiolavds);
269900100420                     If  OXGLerr = *blanks;
270000100420                         imk17dad = OXGLdata;
270100100420                     Endif;
270200100924                  Endif ;
270300100924                  If §ccoupm = '=' ;
270400100924                    imk17dad = vofdpa;
270500100924                  Endif ;
270600100115               endif;
270700100126
270800100126               If causale = '32' ;
270900100126             // se la causale 32
271000100126             // pulisco il campo della trattativa
271100100126                  imk17nrv = 0 ;
271200100126                  imk17ksc = tamksc ;
271300100127             // la data futura viene calcolata sommando 15 alla data di reale
271400100126             // affidamento
271500100924                If §ccoupm <> ' ' and
271600100924                  §ccogio > 0;
271700100420                  clear XGIOLAVDS;
271800100420                  IXGLdata = vofdra ;
271900100924                  If §ccoupm = '+' ;
272000100924                     IXGLadd  = 'S';
272100100924                  else ;
272200100924                     IXGLsub  = 'S';
272300100924                  endif;
272400100420                  IXGLgg   = §CCOgio;
272500100420                  IXGLpa   = 'P';
272600100420                  If  c01cmm > *zeros;
272700100420                      IXGLfil = %dec(%subst(%editc(c01cmm:'X'):1:3):3:0);
272800100420                  else ;
272900100420                      IXGLfil = DUTpou;
273000100420                  Endif;
273100100420                  XGIOLAV (xgiolavds);
273200100420                  If  OXGLerr = *blanks;
273300100420                      imk17dad = OXGLdata;
273400100420                  Endif;
273500100924                Endif ;
273600100924                If §ccoupm = '=' ;
273700100924                  imk17dad = vofdra;
273800100924                Endif ;
273900100420               endif;
274000100115             endif;
274100100115
274200100108             trmk17c (kpjba : trmk17ds) ;
274300100108
274400100426             endif ;
274500100108       ENDSR;
274600100114
274700100203
274800100128       //--------------------------------------------------------------
274900100128       //?Controllo in fase di accettazione se esistono altre versioni
275000100128       //?da annullare o da congelare
275100100128       //--------------------------------------------------------------
275200100128       BEGSR Sr_CtrAcc  ;
275300100128
275400100128         // rilascio il record precedentemente letto
275500100204           If  sh1n12 = '1' ;
275600100204               note10_tar = *on ;
275700100204           else ;
275800100204               note10_tar = *off ;
275900100204           endif ;
276000100202           update ta56s01 ;
276100100202
276200100204         // mi salvo il codice offerta e versione che si vuole accettare
276300100128         sav_ctr = s01ctr ;
276400100128         sav_ver = s01prg ;
276500100128
276600100128
276700100128         // verifico leggendo tutti i records del subfile se esistono
276800100128         // più versioni della stessa offerta
276900100128         numrec= 0 ;
277000100128         Dow 1 = 1 ;
277100100128             numrec += 1;
277200100128             chain numrec ta56s01 ;
277300100128             If not %found(Tnta56d) ;
277400100128                leave ;
277500100128             endif ;
277600100128
277700100312          //   if s01opz = 'E' ;
277800100312          //      clear s01opz ;
277900100312          //   endif ;
278000100128         // verifico se si tratta della stessa offerta / versione
278100100128             if s01ctr = sav_ctr and s01prg = sav_ver ;
278200100128                iter ;
278300100128             endif ;
278400100128
278500100128         // verifico se si tratta di un'altra offerta
278600100128             if s01ctr <> sav_ctr ;
278700100128                iter ;
278800100128             endif ;
278900100128
279000100128         // verifico se si tratta della stessa offerta ma è già stata Congelata
279100100312             if s01ctr = sav_ctr and sh1eso = 'H' ;
279200100128                iter ;
279300100128             endif ;
279400100128
279500100128         // se si tratta della stessa offerta ed è già stata selezionata
279600100128         // un'opzione di accettazione do errore oppure è già stata accettata
279700100128         // in passato
279800111214         // o un'opzione di convalida o è già stata convalidata in passato
279900111214             if s01ctr = sav_ctr and ((s01opz = 'A' or sh1eso = 'A') or
280000111214                                     (s01opz = 'C' or sh1eso = 'C'));
280100100128                errMessage  = *on;
280200100128                errGenerico = *on;
280300100128                PosCurOpz   = *on;
280400100312         // se si tratta di convalida  metto un'altro msg
280500100312                If s01opz = 'C';
280600111214                   V1Dmsg = $Msg(22);
280700100312                else ;
280800100312                   V1Dmsg = $Msg(19);
280900100312                endif ;
281000100204                If  sh1n12 = '1' ;
281100100204                    note10_tar = *on ;
281200100204                else ;
281300100204                    note10_tar = *off ;
281400100204                endif ;
281500100128                update ta56s01 ;
281600100128                leave ;
281700100128             endif ;
281800100128
281900100128         // se si tratta della stessa offerta ed è già stata selezionata
282000100128         // un'opzione di annullamento o congelamento tutto ok
282100100128             if s01ctr = sav_ctr and (s01opz = '4' or s01opz = 'H');
282200100128                iter ;
282300100128             endif ;
282400100128
282500100128         // se si tratta della stessa offerta ed è già stata selezionata
282600100128         // un'opzione di annullamento o congelamento tutto ok
282700100128             if s01ctr = sav_ctr and s01opz = ' ' ;
282800100128                errMessage  = *on;
282900100128                errGenerico = *on;
283000100128                PosCurOpz   = *on;
283100100312         // se si tratta di convalida  metto un'altro msg
283200111214                if TTR_Accettaz = *off ;
283300111214                   V1Dmsg = $Msg(23);
283400100312                else ;
283500100312                   V1Dmsg = $Msg(20);
283600100312                endif ;
283700100204                If  sh1n12 = '1' ;
283800100204                    note10_tar = *on ;
283900100204                else ;
284000100204                    note10_tar = *off ;
284100100204                endif ;
284200100128                update ta56s01 ;
284300100128                leave ;
284400100128             endif ;
284500100128
284600100128         Enddo ;
284700100128
284800100128         // riaggancio il record letto in precedenza
284900100128         chain c01rcd ta56s01;
285000140221
285100140221       // Se non ho avuto un errore precedente controllo la tariffa
285200140221       // particolare Fuel Surcharge
285300140221         IF  not errMessage;
285400140221           exsr ctrFuel;
285500140221         ENDIF;
285600100128
285700100128       ENDSR ;
285800100128
285900140221       //--------------------------------------------------------------
286000140221       //?Controllo la tariffa particolare Fuel
286100140221       //--------------------------------------------------------------
286200140221       BEGSR ctrFuel;
286300140221
286400140221       // Se trovo tariffa particolare Fuel e la data del prezzo base
286500140221       // di riferimento è maggiore a oggi devo dare errore
286600140221
286700140221         TPTftc = 'f ';
286800140221         chain(n) (C01nrv:S01ctr:S01prg:TPTftc) TIOPT01L;
286900140221    3    IF  %found and TPTatb = *blanks and TPTdpb > datacor8;
287000140221           errMessage  = *on;
287100140221           errGenerico = *on;
287200140221           PosCurOpz   = *on;
287300140221           V1Dmsg = $msg(33);
287400140221           IF  sh1n12 = '1';
287500140221             note10_tar = *on;
287600140221           ELSE;
287700140221             note10_tar = *off;
287800140221           ENDIF;
287900140221           update ta56s01;
288000140221         ENDIF;
288100140221
288200140221       ENDSR;
288300100115
288400100118       //--------------------------------------------------------------
288500100118       //?Convalida Offerte
288600100118       //--------------------------------------------------------------
288700100118       BEGSR Sr_ConvOff ;
288800100118
288900100118         // rilascio il record precedentemente letto
289000100204         If  sh1n12 = '1' ;
289100100204             note10_tar = *on ;
289200100204         else ;
289300100204             note10_tar = *off ;
289400100204         endif ;
289500100118         update ta56s01 ;
289600100118
289700100118         // pulisco il subfile della convalida
289800100118         exsr pulsfl5 ;
289900100118         clear s05nrr ;
290000100118         SflNxtChg5= *off;
290100100505
290200100505         // pulisco i campi di comodo
290300100805         clear newksc ;
290400100518         wforzacon = *off ;
290500100118
290600100118         // verifico leggendo tutti i records del subfile se esistono
290700100120         // più convalide e verifico che non ci siano degli errori
290800100118         numrec= 1 ;
290900100118         Dow 1 = 1 ;
291000100118             chain numrec ta56s01 ;
291100100121             If not %found(Tnta56d) ;
291200100118                leave ;
291300100118             endif ;
291400100118
291500100312         // se opzione = C  convalida verifico se ci sono degli errori se
291600100312         // non esito una trattativa di tipo Aumento
291700110114             If s01opz = 'C' and  sh1eso <> 'A' and TTR_accettaz = *on ;
291800100118                errMessage  = *on;
291900100118                errGenerico = *on;
292000100118                PosCurOpz   = *on;
292100100118                V1Dmsg = $Msg(06);
292200100126                s01opz = 'C' ;
292300100204                If  sh1n12 = '1' ;
292400100204                    note10_tar = *on ;
292500100204                else ;
292600100204                    note10_tar = *off ;
292700100204                endif ;
292800100121                update ta56s01 ;
292900100118                leave ;
293000100121             endif ;
293100100121             If s01opz = 'C'  ;
293200100118                if s05nrr < *hival ;
293300100118                   S05nrr += 1;
293400100118                   s05ctr = s01ctr ;
293500100121                   sh5prg = s01prg ;
293600100118                   s05tpt = s01tpt ;
293700100118                   s05dtr = s01dtr ;
293800100118                   s05dcv = s01dcv ;
293900100118
294000100118                   tamksc = c01nrv ;
294100100118                   tamctr = s01ctr ;
294200100118                   tamprg = s01prg ;
294300100118                   chain(n) (tamksc:tamctr:tamprg) tnofm01l ;
294400100118                   If %found(tnofm01l) ;
294500100118                      s05sis = tamsis  ;
294600100203                      data_eur= %date(tamddt:*iso) ;
294700100203                      s05ddt = %dec(data_eur) ;
294800100118                   endif;
294900100118
295000100118                // mi salvo il numero record del primo subfile
295100100118                   sh5nrr = s01nrr ;
295200100118
295300100120                // mi salvo la data decorrenza e scadenza tariffa
295400100120                   sh5ddt = tamddt ;
295500100120                   sh5dst = tamdst ;
295600100120
295700100118                   write TA56S05;
295800100118                endif ;
295900100118
296000100204                If  sh1n12 = '1' ;
296100100204                    note10_tar = *on ;
296200100204                else ;
296300100204                    note10_tar = *off ;
296400100204                endif ;
296500100507
296600100507                clear s01opz ;
296700100118             update ta56s01 ;
296800100121             endif ;
296900100118
297000100118             numrec += 1;
297100100118
297200100118         Enddo ;
297300100118
297400100121         // riaggancio il record letto in precedenza
297500100121         chain c01rcd ta56s01;
297600100729
297700100729
297800100729         // se non ci sono errori emetto la videata altrimenti torno al sfl
297900100729         // precedente
298000100118         If ErrGenerico = *off ;
298100100204            exsr Gess05 ;
298200100118         endif;
298300100118
298400100118       ENDSR ;
298500100729
298600100729       //--------------------------------------------------------------
298700100729       //?Gestione scelta codice cliente per la convalida
298800100729       //--------------------------------------------------------------
298900100729       BEGSR GesS10;
299000100729
299100100729         // Inizializzazione videata
299200100729            exsr Inzs10;
299300100729
299400100804         Dow $fine10 = *off ;
299500100729         // Emissione Testata e Piede con tasti funzionali abilitati
299600100804           write  TA56W10;
299700100729
299800100729
299900100729         // Emissione videata
300000100804         exfmt  TA56WC10;
300100100729         reset errMessage;
300200100729         reset errGenerico;
300300100729
300400100729         SELECT;
300500100729
300600100729         // - F12=Ritorno
300700100729           WHEN  dsp_aid = c_F12;
300800100804             $video = 'S5';
300900100729             leave ;
301000100729
301100100729         // - F06=Convalida
301200100729           WHEN  dsp_aid = c_F06  ;
301300100804             Exsr SR_ctrS10 ;
301400100729             if  errGenerico = *on;
301500100729                 iter   ;
301600100729             endif;
301700100729
301800100804         // Elaboro la scelta del codice cliente per la convalida
301900100805             exsr F06W10;
302000100817        // se ritorna l'errore generico acceso vuol dire che è stato dato
302100100817        // f12 e devo riemettere la videata
302200100817             If errGenerico = *on ;
302300100817                iter    ;
302400100817             Endif ;
302500100729
302600100804             $fine10= *on ;
302700100729
302800100729           OTHER ;
302900100729
303000100804             Exsr SR_ctrs10 ;
303100100729             if  errGenerico = *on;
303200100729                 iter   ;
303300100729             endif;
303400100729
303500100729         ENDSL;
303600100729
303700100729         ENDDO;
303800100729
303900100729       ENDSR;
304000100729
304100100729       //--------------------------------------------------------------
304200100729       //?Carico subfile con le scelte dei codici clienti da convalidare
304300100729       //--------------------------------------------------------------
304400100729       BEGSR Inzs10 ;
304500100729
304600100729            $fine10 = *off ;
304700100730
304800100730       // Pulizia subfile
304900100730         SflDsp10_N   = *on;
305000100730         SflDspCtl10_N= *on;
305100100730         write  TA56WC10;
305200100730         SflDspCtl10_N= *off;
305300100730         SflEnd10     = *off;
305400100730
305500100730
305600100730         clear W_SflPag10;
305700100730         clear C10rcd;
305800100730         clear C10csr;
305900100730         clear S10nrr;
306000100816         clear w10opz;
306100100730         SflNxtChg10= *off;
306200100730         errMessage  = *off;
306300100730         errGenerico = *off;
306400100730
306500100729
306600100729         // proporre le scelte di codifica in base al tipo visita
306700100729         // ed alla presenza o meno di altri codici a cui appoggiarsi
306800100729
306900100730         // se si tratta di una trattativa di tipo nuovo :
307000100729         // 1) propongo un nuovo codice con il commerciale della trattativa
307100100729         // 2) propongo un nuovo codice con commerciale a scelta
307200100729         // 3) se già convalidata propongo l'elenco dei codici presenti in tivof
307300100729
307400100729            If vistpv = 'N'  ;
307500100730                   S10nrr = 1;
307600100730                   w10des = 'Nuovo cliente legato al comm.le ' ;
307700100730                   w10des = %trim(w10des) + ' ' + %editc(c01cmm: 'X');
307800100730                   w10hcmm = c01cmm ;
307900100730                   clear w10hksc ;
308000121128                   clear W10Hpre;
308100100730                   write TA56WS10;
308200100730
308300100730         // se nuovo aggiungo anche l'opzione di scelta nuovo codice con
308400100730         // commerciale diverso
308500100730                   S10nrr += 1;
308600100924                   w10des = 'Nuovo cliente legato ad altro comm.le ' ;
308700100730                   clear w10hcmm ;
308800100730                   clear w10hksc ;
308900121128                   clear W10Hpre;
309000100730                   write TA56WS10;
309100100730
309200100730             endif ;
309300100730
309400100730         // se si tratta di una trattativa di tipo non  nuovo :
309500100730         // 1) propongo codice cliente della trattativa
309600100730         // 2) propongo un nuovo codice con il commerciale della trattativa
309700100730         // 3) propongo un nuovo codice con commerciale a scelta
309800100730         // 4) se già convalidata propongo l'elenco dei codici presenti in tivof
309900100730
310000100730            If vistpv <> 'N'  ;
310100100730                   S10nrr = 1;
310200100730                   w10des = 'Aggiungi al cliente della Trattativa' ;
310300100730                   w10des = %trim(w10des) + ' ' + %editc(c01ksc: 'X');
310400100730                   w10hcmm = c01cmm ;
310500100730                   w10hksc = c01ksc ;
310600121128                   clear W10Hpre;
310700100730                   write TA56WS10;
310800100730
310900100730         // propongo un nuovo codice con il commerciale della trattativa
311000100730                   S10nrr += 1;
311100100730                   w10des = 'Nuovo cliente legato al comm.le ' ;
311200100730                   w10des = %trim(w10des) + ' ' + %editc(c01cmm: 'X');
311300100730                   w10hcmm = c01cmm ;
311400100730                   clear w10hksc ;
311500121128                   clear W10Hpre;
311600100730                   write TA56WS10;
311700100730
311800100730         // propongo un nuovo codice con il commerciale della trattativa
311900100730                   S10nrr += 1;
312000100730                   w10des = 'Nuovo cliente legato a nuovo comm.le ' ;
312100100730                   clear w10hcmm ;
312200100730                   clear w10hksc ;
312300121128                   clear W10Hpre;
312400100730                   write TA56WS10;
312500100730
312600100730             endif ;
312700121128
312800121128         //?Per qualsiasi tipo trattativa
312900121129         //?proposta di nuovo codice già prenotato
313000121128                   S10nrr += 1;
313100121129                   w10des = 'Nuovo codice già prenotato';
313200121128                   clear w10hcmm ;
313300121128                   clear w10hksc ;
313400121128                   W10Hpre = 'S';
313500121128                   write TA56WS10;
313600100730
313700100730         // sia per trattativa di tipo nuovo che no propongo anche i codici
313800100730         // clienti delle offerte gia convalidate se ce ne sono
313900100730            If visesi =  'P'  ;
314000100730
314100100805               exec sql  declare C1  cursor for
314200100730                   select distinct vofksc
314300100730                   from tivof00f where vofnrv = :visnrv and vofeso = 'C' and
314400110112                   vofdco > 0 and vofctr <> 999 ;
314500100730
314600100805               exec sql open C1;
314700100730               dow  sqlcod = 0;
314800100730                    exec sql fetch next from C1  into :vofksc;
314900100730                    IF sqlcod = 100 or sqlcod < 0;
315000100805                       leave  ;
315100100730                    endif;
315200100730            // se cliente diverso da quello della trattativa lo propongo
315300100730                    If vofksc <> c01ksc ;
315400100730                       S10nrr += 1;
315500100730                       w10des = 'Aggiungi la tariffa al cliente' ;
315600100730                       w10des = %trim(w10des) + ' ' + %editc(vofksc: 'X');
315700100730                       clear w10hcmm ;
315800121128                       clear W10Hpre;
315900100730                       w10hksc = vofksc ;
316000100730                       write TA56WS10;
316100100730                    endif ;
316200100730               Enddo ;
316300100730
316400100805               exec sql close C1 ;
316500100730
316600100730            Endif ;
316700100730
316800100729
316900100729         // Visualizzazione del SFL (se ci sono dati)
317000100805            SflDsp10_N = (S10nrr <= *zeros);
317100100729
317200100805         // Attivazione (eventuale) del SFLEND
317300100805            SflEnd10=  *on;
317400100729
317500100729         // Posizionamento cursore al 1° rcd della pagina
317600100805            if  S10nrr > *zero;
317700100805                c10rcd = 1;
317800100805            else;
317900100805                clear  C10rcd;
318000100805            endif;
318100100729
318200100729       ENDSR;
318300100729
318400100118
318500100805       //--------------------------------------------------------------
318600100805       //?Controllo videata scelta Codice cliente
318700100805       //--------------------------------------------------------------
318800100805       BEGSR Sr_Ctrs10 ;
318900100805
319000100805           reset errMessage ;
319100100805           reset errGenerico;
319200100805           reset Poscuropz10 ;
319300100805           clear numrec10 ;
319400100805           clear scelta_rcd ;
319500100805           clear v10msg  ;
319600100805
319700100805        // Eseguo i controlli sul codice cliente da utilizzare per la convalida
319800100805
319900100805       numrec10 = 1 ;
320000100805       chain numrec10 Ta56WS10 ;
320100100805
320200100805         DOW %found(TNTA56D);
320300100805
320400100805           C10rcd = S10nrr;
320500100805
320600100805           SELECT;
320700100805
320800100805         // verifico se fatta + di una scelta
320900100805          When w10opz <> ' ' and w10opz <> '1' ;
321000100805               errMessage  = *on;
321100100805               errGenerico = *on;
321200100805               PosCurOpz10 = *on;
321300100805               V10msg = $Msg(02);
321400100805
321500100805         // se ho fatto una scelta verifico se ne ho fatta più di una
321600100805          When w10opz = '1' and scelta_rcd  > 0 ;
321700100805               errMessage  = *on;
321800100805               errGenerico = *on;
321900100805               PosCurOpz10 = *on;
322000100805               V10msg = $Msg(29);
322100100805
322200100805         // se ho fatto una scelta la memorizzo
322300100805          When w10opz = '1' and scelta_rcd  = 0 ;
322400100805               eval scelta_rcd = numrec10 ;
322500100805
322600100805          ENDSL ;
322700100805
322800100805           if  errMessage = *on  or  errGenerico = *on;
322900100805             leavesr ;
323000100805           endif;
323100100805
323200100805           numrec10 += 1;
323300100805           chain numrec10 Ta56WS10 ;
323400100805          enddo;
323500100805
323600100805       // se non ho scelto nulla
323700100805          If scelta_rcd = 0 ;
323800100805             errMessage  = *on;
323900100805             errGenerico = *on;
324000100805             PosCurOpz10 = *on;
324100100805             V10msg = $Msg(30);
324200100805          endif ;
324300100805
324400100805       ENDSR;
324500100805
324600100805       //--------------------------------------------------------------
324700100805       //?Effettuata scelta codice per la convalida F6
324800100805       //--------------------------------------------------------------
324900100805       BEGSR F06W10  ;
325000100805
325100100805          clear fil_cmm ;
325101171220          NuovoCliente = *off;
325200100805
325300100805       // aggancio il record del subfile 10 scelto
325400100805          chain scelta_rcd ta56ws10 ;
325500100805
325600100805          If not %found(TNTA56D) ;
325700100805             $video = 'S5';
325800100805             leavesr ;
325900100805          Endif ;
326000100805
326100100805       // verifico che codice cliente devo attribuire
326200100805
326300100805           SELECT ;
326400121128
326500121129       //?Scelta di nuovo codice prenotato
326600121128           WHEN W10Hpre = 'S';
326700121128             wInzW12 = *on;
326800121128             exsr gesW12;
326900121128         //?se torna errore generico vuol dire che è stato dato
327000121128         //?F12 e quindi devo tornare alla videata di selezione codici
327100121128             IF  ErrGenerico;
327200121128               leavesr;
327300121128             ENDIF;
327400121128         //?se tutto ok vado avanti con la creazione del nuovo codice
327500121128             c05ksc = W12ksc;
327600121129             IF  Vis_comm;
327700121129               comcon = %dec(W12cmm:7:0);
327800121129             ELSE;
327900121129               comcon = C01cmm;
328000121129             ENDIF;
328001171220             NuovoCliente = *on;
328100100805
328200100805       // se codice commerciale diverso da 0 e codice cliente =  a 0
328300100805       // ricerco un nuovo codice con commerciale passato (trattativa)
328400100805           When w10hcmm > 0 and w10hksc = 0 ;
328500100805                fil_cmm = %dec(%subst(%EDITC(W10hcmm:'X'):1:3):3:0);
328600100805                exsr Sr_Newcli ;
328700100805                c05ksc = newksc ;
328800100818                comcon = c01cmm ;
328900100805
329000100805
329100100805       // se codice commerciale uguale a 0 e codice cliente =  a 0
329200100805       // ricerco un nuovo codice con commerciale nuovo da richiedere
329300100805           When w10hcmm = 0 and w10hksc = 0 ;
329400100816                  exsr Gesw09 ;
329500100818        // se ritorna l'errore generico acceso vuol dire che è stato dato
329600100817        // f12 e devo tornare alla video di selezione codici
329700100817                  If errGenerico = *on ;
329800100817                     leavesr ;
329900100817                  Endif ;
330000100817
330100100817        // se tutto ok recupero la filiale del commerciale inserito per
330200100817        // recuperare nuovo codice cliente
330300100817                  If w09cmm <> *blanks and w09cmm <> *zeros;
330400100817                     fil_cmm = %dec(%subst(W09cmm:1:3):3:0);
330500100816                     exsr Sr_Newcli ;
330600100816                     c05ksc = newksc ;
330700100818                     comcon = %dec(w09cmm:7:0);
330800100816                  endif ;
330900100805
331000100805       // se codice commerciale = a 0 e codice cliente diverso da 0
331100100805       // appoggio le offerte da convalidare su di una anagrafica già esistente
331200100816           When w10hcmm = 0 and w10hksc > 0 ;
331300100805                c05ksc = W10hksc ;
331400100818                comcon = c01cmm ;
331401171221
331402171221       // se nessuna delle opzioni precedenti reimposto x precauzione i dati
331403171221       // presenti sulla trattativa
331404171221           OTHER;
331405171221             C05ksc = C01ksc ;
331406171221             COMcon = C01cmm ;
331500100805
331600100805           Endsl ;
331700100805
331800100805         // Aggancio anagrafiche provvisorie e non
331900100805            exsr Sr_AggAna ;
332000100805         // verifico il codice pagamento
332100100805            Exsr Sr_CodPag ;
332200100805
332300100805             $video = 'S5';
332400100805             $inzs05 = *on ;
332500100805
332600100805       ENDSR;
332700100805
332800100118       //--------------------------------------------------------------
332900100118       //?Gestione Videata Convalida Offerta
333000100118       //--------------------------------------------------------------
333100100118       BEGSR GesS05;
333200100118
333300100118         // Inizializzazione videata
333400100118            exsr Inzs05;
333500100204
333600100204         Dow $fine5 = *off ;
333700100120         // Emissione Testata e Piede con tasti funzionali abilitati
333800100204           write  TA56T01;
333900100204           write  TA56P05;
334000100120
334100100120
334200100120         // Emissione videata
334300100120         exfmt  TA56C05;
334400100120         reset errMessage;
334500100120         reset errGenerico;
334600100120         clear V1Dmsg;
334700100120
334800100120         SELECT;
334900100118
335000100118         // - F12=Ritorno
335100100118           WHEN  dsp_aid = c_F12;
335200100118             $video = 'S1';
335300100204             leave ;
335400100424
335500100424         // - F11=Attribuisco un nuovo codice
335600100424           WHEN  dsp_aid = c_F11;
335700100805             exsr Gess10 ;
335701171221               wforzacon = *off;
335800100805                 iter   ;
335900100118
336000100120         // - F06=Convalida
336100100118           WHEN  dsp_aid = c_F06  ;
336200100120             Exsr SR_ctrS05 ;
336300100118             if  errGenerico = *on;
336400100204                 iter   ;
336500100118             endif;
336600100118
336700100203         // aggiorno i record per la convalida
336800100121             exsr Sr_F6ConvOff;
336900100312
337000100224             $fine5 = *on ;
337100100118
337200100118           OTHER ;
337300100118
337400100120             Exsr SR_ctrs05 ;
337500100118             if  errGenerico = *on;
337600100204                 iter   ;
337700100118             endif;
337800100118
337900100118         ENDSL;
338000100118
338100100204         ENDDO;
338200100204
338300100118       ENDSR;
338400100118
338500100118       //--------------------------------------------------------------
338600100118       //?Carico videata Convalida Offerte  PARTE ALTA
338700100118       //--------------------------------------------------------------
338800100118       BEGSR Inzs05 ;
338900100118
339000100224            $fine5 = *off ;
339100100518            wforzacon = *off ;
339101171214            reset ClienteSofferenza;
339200100119         // inizializzo alcuni campi chiave
339300100119            acokut = 1 ;
339400100119            acokcc = dutkci ;
339500100424            clear coldksc ;
339600100424            clear c05ksc ;
339700100424            disatt_f11 = *off ;
339800100804            protez_ksc = *on ;
339900100119
340000100118         // proporre il codice cliente  in base al tipo visita
340100100118         // ed alla presenza o meno di altri codici a cui appoggiarsi
340200100119
340300100119         // se si tratta di una visita di tipo nuovo e non è mai stata
340400100119         // convalidata una offerta prima ad ora propongo un nuovo codice
340500100119         // e lo proteggo
340600100119
340700100424            If vistpv = 'N'  and visesi = ' ' ;
340800100805               fil_cmm = %dec(%subst(%EDITC(c01cmm:'X'):1:3):3:0);
340900100119               exsr Sr_NewCli ;
341000100424               c05ksc = newksc ;
341100100818               comcon = c01cmm ;
341200100510               PoscurKsc = *off ;
341300100518               wforzacon = *off ;
341400100424            endif ;
341500100805
341600100805         // se si tratta di una visita di tipo nuovo ed è  stata
341700100805         // convalidata una offerta  propongo l'ultimo codice
341800100805         // convalidato
341900100805
342000100805            If vistpv = 'N'  and visesi = 'P' ;
342100100805               exsr sr_reccli ;
342200100805               c05ksc = oldksc ;
342300100818               comcon = c01cmm ;
342400100805               PoscurKsc = *off ;
342500100805               wforzacon = *off ;
342600100805            endif ;
342700100119
342800100424         // se visita non nuova imposto il codice cliente della trattativa
342900100119         // per adesso protetto
343000100119            If vistpv <> 'N' ;
343100100119               c05ksc = c01ksc ;
343200100818               comcon = c01cmm ;
343300100119            endif ;
343400100518
343500100119         // Come data reale affidamento propongo la data di presunto affidamento
343600100119         // o la data del giorno ?
343700100119            c05dra = *date  ;
343800100118
343900100424         // Aggancio anagrafiche provvisorie e non
344000100424            exsr Sr_AggAna ;
344100100119         // verifico il codice pagamento
344200100119            Exsr Sr_CodPag ;
344300100120
344400100121         // Visualizzazione del SFL (se ci sono dati)
344500100121         SflDsp5_N = (S05nrr <= *zeros);
344600100121
344700100121         // Attivazione (eventuale) del SFLEND
344800100121         SflEnd5 =  *on;
344900100121
345000100121         // Posizionamento cursore al 1° rcd della pagina
345100100121         if  S05nrr > *zero;
345200100121             c05rcd = 1;
345300100121         else;
345400100121           clear  C05rcd;
345500100121         endif;
345600100121
345700100118       ENDSR;
345800100805
345900100805       //--------------------------------------------------------------
346000100805       //?Recupero l'ultimo codice convalidato
346100100805       //--------------------------------------------------------------
346200100805       BEGSR Sr_RecCli ;
346300100805
346400100805         exec sql
346500100805          declare C2  cursor for
346600100805          select vofksc
346700100805          from tivof00f where vofnrv = :visnrv and vofeso = 'C' and
346800110112          vofdco > 0  and vofctr <> 999 order by vofdco desc ;
346900100805
347000100805         exec sql
347100100805           open C2;
347200100805           IF sqlcod = 0;
347300100805           exec sql
347400100805             fetch next from C2  into :vofksc;
347500100805             IF sqlcod = 100 or sqlcod < 0;
347600100805               leavesr ;
347700100805             endif;
347800100805            Oldksc = vofksc ;
347900100805           ENDIF;
348000100805
348100100805         exec sql close C2 ;
348200100805
348300100805       ENDSR;
348400100805
348500100424       //--------------------------------------------------------------
348600100424       //?Aggancio Anagrafiche provvisorie e non
348700100424       //--------------------------------------------------------------
348800100424       BEGSR Sr_AggAna ;
348900100424
349000100424         // Aggancio anagrafiche provvisorie e non
349100100424         select ;
349200100424
349300100424         // se tipo visita N e mai esistata
349400100424         When vistpv = 'N' and visesi = ' ' ;
349500100804            chain    (acokut:acokcc:c01nrv) tfaco00f ;
349600100424            If %found (tfaco00f) ;
349700100804               chain    (acokut:acokcc:c01nrv) tfind00f ;
349800100804               chain    (acokut:acokcc:c01nrv) tfclp00f ;
349900100424            endif ;
350000100424
350100100424         // se tipo visita N ed esitata visualizzo anagrafica cod proposto
350200100424         When vistpv = 'N' and visesi = 'P' ;
350300100804            chain    (acokut:acokcc:c05ksc) cnaco00f ;
350400100506            If %found (cnaco00f) ;
350500100804               chain    (acokut:acokcc:c05ksc) cnind00f ;
350600100804               chain    (acokut:acokcc:c05ksc) cnclp00f ;
350700100506            else ;
350800100804               chain    (acokut:acokcc:c01nrv) tfaco00f ;
350900100506               If %found (tfaco00f) ;
351000100804                  chain    (acokut:acokcc:c01nrv) tfind00f ;
351100100804                  chain    (acokut:acokcc:c01nrv) tfclp00f ;
351200100506               endif ;
351300100424            endif ;
351400100424
351500100424         // se tipo visita diversa da N ed esito P anagrafica cod proposto
351600110113         When vistpv <> 'N' and visesi = 'P' ;
351700100804               chain    (acokut:acokcc:c05ksc) cnaco00f ;
351800100424               If %found (cnaco00f) ;
351900100804                  chain    (acokut:acokcc:c05ksc) cnind00f ;
352000100804                  chain    (acokut:acokcc:c05ksc) cnclp00f ;
352100100506               else ;
352200100804                  chain    (acokut:acokcc:c01nrv) tfaco00f ;
352300100506                  If %found (tfaco00f) ;
352400100804                     chain    (acokut:acokcc:c01nrv) tfind00f ;
352500100804                     chain    (acokut:acokcc:c01nrv) tfclp00f ;
352600100506                  else  ;
352700100804                     chain    (acokut:acokcc:c01ksc) cnaco00f ;
352800100506                     If %found (cnaco00f) ;
352900100804                        chain    (acokut:acokcc:c01ksc) cnind00f ;
353000100804                        chain    (acokut:acokcc:c01ksc) cnclp00f ;
353100100506                     endif;
353200100506                 endif ;
353300100424               endif;
353400100424
353500100424         // se tipo visita diversa da N e no esito prima tfaco poi cnaco
353600110113         When vistpv <> 'N' and visesi = ' ' ;
353700100804            chain    (acokut:acokcc:c01nrv) tfaco00f ;
353800100424            If %found (tfaco00f) ;
353900100804               chain    (acokut:acokcc:c01nrv) tfind00f ;
354000100804               chain    (acokut:acokcc:c01nrv) tfclp00f ;
354100100424            else  ;
354200100804               chain    (acokut:acokcc:c01ksc) cnaco00f ;
354300100424               If %found (cnaco00f) ;
354400100804                  chain    (acokut:acokcc:c01ksc) cnind00f ;
354500100804                  chain    (acokut:acokcc:c01ksc) cnclp00f ;
354600100424               endif;
354700100424            endif ;
354800100424
354900100424        Endsl ;
355000100424
355100100424       ENDSR;
355200100424
355300100119       //--------------------------------------------------------------
355400100119       //?Controllo condizioni pagamento
355500100119       //--------------------------------------------------------------
355600100119       BEGSR Sr_CodPag ;
355700100119
355800100122            Vis_pagamento = *off;
355900170410            Pro_pagamento = *off;
356000170410
356100170410       // se cliente in contenzioso proteggo il codice di pagamento
356200170410       // sia anagrafica provvisoria che non
356300170410         IF  CLPcon <> *blanks;
356400170410           clear ds4W;
356500170410           TBLkut = 1;
356600170410           TBLcod = '4W';
356700170410           TBLkey = CLPcon ;
356800170410           chain (TBLkut:TBLcod:TBLkey) TABEL00F;
356900170410           IF  %found(TABEL00F) and TBLflg = *blanks;
357000170410             ds4W = TBLuni;
357100170410           ENDIF;
357200170410           IF  §4Wtip <> *blanks;
357300170410             Pro_pagamento = *on;
357400170410           ENDIF;
357500170410         ENDIF;
357600100122
357700100119            clear dsfa ;
357800100119            Tblkut = 1 ;
357900100119            tblcod = 'FA';
358000100316            tblkey = %subst(indcdp:4:3) + '1' ;
358100100119            chain (Tblkut:tblcod:tblkey) tabel00f ;
358200100119
358300100119            If %found(tabel00f) ;
358400100119               dsfa = tbluni ;
358500100122            else ;
358600100122               Vis_pagamento = *on ;
358700100119            endif ;
358800100119
358900100119         // Verifico se pagamento con ricevuta bancaria
359000100119         // obbligatorio codice ABI CAB e descrizione bancaria
359100100316            If §fatpg = '1' and (indabi = 0 and indcab = 0)
359200100316               or (indbna = *blank and (indabi<>0 or indcab<>0));
359300100119               Vis_pagamento = *on ;
359400100316               c05cdp = %subst(indcdp:4:3);
359500100119               c05abi = indabi ;
359600100119               c05cab = indcab ;
359700150417               savcdp = C05cdp;
359800100120         // aggancio anagrafico banche per decodifica banca
359900100119               chain (indabi:indcab) cnabi00f ;
360000100119               If %found(cnabi00f) and abiann <> '*' ;
360100100119         // richiamo la routine di decodifica banca d'appoggio
360200100119                  exsr sr_Crebna ;
360300100119               endif ;
360400100119            endif;
360500100119
360600100119         // Imposto la Partita IVA
360700100121            If %subst(indiva:1:2) >= '00' ;
360800100119               c05ivai =  indiva ;
360900100119            Else ;
361000100312               c05ivai = %subst(indiva:3:14) ;
361100100312               c05ivae = %subst(indiva:1:2) ;
361200100119            endif ;
361300100119
361400100119            wiva = indiva ;
361500100120            saviva = indiva ;
361600100119
361700100119         // Imposto il Codice Fiscale
361800100119
361900100119            c05cdf = indcdf ;
362000100119            Wcdf = indcdf ;
362100100120            Savcdf = indcdf ;
362200100119
362300100119         // se il sottoconto intestazione fattura è diverso dal codice cliente
362400100119         // devo reperire la nazione del sottoconto fatturazione
362500100119
362600100119            if clpscf>0 and clpscf<>visksc ;
362700100119
362800100119               clear  wsta  ;
362900100119               clear  tibs69ds;
363000100119               I69kin = clpscf;
363100100119               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
363200100119               wtibs69 = *On  ;
363300100119               if  o69err=*blanks and w_indflg=*blanks ;
363400100119                   wsta=w_indsta;
363500100119               endif ;
363600100119            else  ;
363700100119               wsta=indsta ;
363800100119            endif ;
363900100119
364000100119          // se cliente estero non visualizzo il codice fiscale
364100100119            Vis_CodFisc=(indsta = *blanks and wsta = *blanks);
364200100119
364300100119       ENDSR ;
364400100119
364500100119       //--------------------------------------------------------------
364600100119       //?Decodifica Banca d'appoggio
364700100119       //--------------------------------------------------------------
364800100119       BEGSR Sr_Crebna ;
364900100119
365000100119          If  abiabi <> 99999 or abicab <> 99999 ;
365100100119                Clear wagenzia ;
365200100119                Clear wbanca ;
365300100119                wbanca = abiist ;
365400100119         // Compongo il nome della banca con località o comune
365500100119            If  abiage = *Blanks ;
365600100119                Select ;
365700100119                When  abiloc <> *Blanks ;
365800100119                      wagenzia = abiloc ;
365900100119                When  abicom <> *Blanks ;
366000100119                      wagenzia = abicom ;
366100100119                EndSl ;
366200100119         // Compongo il nome della banca con agenzia + località o comune
366300100119            Else ;
366400100119         // Cerco il primo byte vuoto dell'agenzia
366500100119                wpos = %checkr(' ':abiage);
366600100119
366700100119                wagenzia = %subst(abiage:1:wpos);
366800100119                wpos1 = wpos + 2 ;
366900100119                wpos2 = 16 - wpos1 ;
367000100119         // Se wpos2 inferiore a zero lo imposto a 0
367100100119                If wpos2 < *Zeros ;
367200100119                   Clear wpos2 ;
367300100119                EndIf ;
367400100119         // Se posso aggiungere anche solo 1 carattere procedo
367500100119                If wpos > 1 and wpos < 16 ;
367600100119         // Cerco di riempire quello che resta con la località o il comune
367700100119                   Select ;
367800100119                   When  abiloc <> *Blanks ;
367900100119                         wagenzia = %trim(wagenzia) + ' ' +
368000100119                                    %subst(abiloc:1:wpos2);
368100100119                   When  abicom <> *Blanks ;
368200100119                         wagenzia = %trim(wagenzia) + ' ' +
368300100119                                    %subst(abicom:1:wpos2);
368400100119                   EndSl ;
368500100119                EndIf ;
368600100119
368700100119            EndIf ;
368800100120
368900100120         // imposto il campo a video
369000100120            c05bna = wbanca + wagenzia ;
369100100120
369200100119          EndIf;
369300100119
369400100119       ENDSR ;
369500100119
369600100816       //--------------------------------------------------------------
369700100816       //?Routine di richiesta nuovo commerciale
369800100816       //--------------------------------------------------------------
369900100816       BEGSR GesW09;
370000100816
370100100816         // Inizializzazione videata
370200100816            exsr Inzw09;
370300100816
370400100816         // Gestione video
370500100816         DOW $Fine9 = *off;
370600100816         // Emissione Finestra di accettazione offerta
370700100816             exfmt  TA56W09;
370800100816             reset errMessage;
370900100816             reset errGenerico;
371000100816             clear w09msg;
371100100816
371200100816         SELECT;
371300100816
371400100816         // - F12=Ritorno
371500100816           WHEN  dsp_aid = c_F12;
371600100816             $Fine9 = *on ;
371700100817             errGenerico = *on;
371800100816
371900100816         // - F06=Accetta
372000100816           WHEN  dsp_aid = c_F06  ;
372100100816             Exsr SR_ctrw09 ;
372200100816             if  errGenerico = *on;
372300100816                 iter ;
372400100816             endif;
372500100816
372600100816
372700100816         // riemetto la  videata
372800100816             $Fine9 = *on ;
372900100816             $video  = 'S5' ;
373000100816
373100100816           OTHER ;
373200100816
373300100816             Exsr SR_ctrw09 ;
373400100816             if  errGenerico = *on;
373500100816                 iter   ;
373600100816             endif;
373700100816
373800100816         ENDSL;
373900100816
374000100816         ENDDO;
374100100816
374200100816       ENDSR;
374300100816
374400100816       //--------------------------------------------------------------
374500100816       //?Carico videata Richiesta Commerciale
374600100816       //--------------------------------------------------------------
374700100816       BEGSR Inzw09 ;
374800100816
374900100817          clear w09cmm ;
375000100817          clear w09cmmd ;
375100100816          $fine9 = *off ;
375200100816
375300100816       ENDSR;
375400100816
375500100816       //--------------------------------------------------------------
375600100816       //?Controllo videata Nuovo commerciale
375700100816       //--------------------------------------------------------------
375800100816       BEGSR Sr_CtrW09 ;
375900100816
376000100817         // Controllo codice commerciale (? Ricerca)
376100100817           If    %scan('?' : W09cmm) > 0  ;
376200100817             errGenerico = *on;
376300100917          // se esiste  il commerciale esterno ricerco con la filiale
376400100917          // di quest'ultimo
376500130806             clear  TRMK43ds;
376600100917               If  c01cmm > *zeros;
376700130806                 iMK43fil = %int(%subst(%editc(c01cmm:'X'):1:3));
376800100917               endif ;
376900130806             TRMK43R (kpjba : TRMK43ds);
377000130806             if  oMK43err = *off  and  oMK43fxx = *blank;
377100130806               w09cmm  = %editc( oMK43cmm : 'X' );
377200130806               w09cmmd = oMK43des;
377300130806             endif;
377400100817             leavesr;
377500100817           endif;
377600100817
377700100817         // Controllo codice commerciale esterno
377800100817           If    w09cmm = *blanks or w09cmm = *zeros;
377900100817             errMessage  = *on;
378000100817             errGenerico = *on;
378100100817             W09msg = $Msg(25);
378200100817             leavesr;
378300100817           endif;
378400100817
378500100817           IF  %check(digits:w09cmm) > 0;
378600100817             errMessage  = *on;
378700100817             errGenerico = *on;
378800100914             w09msg = $Msg(31);
378900100817             leavesr;
379000100817           ENDIF;
379100100817
379200130806             k_CMMcod = %int(w09cmm);
379300130806             chain  %kds(k_azcmm01)  AZCMM000;
379400130806             if  NOT  %found(AZCMM01L);
379500100817               errMessage  = *on;
379600100817               errGenerico = *on;
379700100914               w09msg = $Msg(31);
379800100817               leavesr;
379900100817             else;
380000130806               w09cmmd = CMMdes;
380100100817             endif;
380200111129
380300111129         //?commerciale valido no vari o clienti inattivi
380400130806           IF  CMMpar <> *blanks;
380500111129             ErrMessage  = *on;
380600111129             ErrGenerico = *on;
380700111129             w09msg = $Msg(31);
380800111129             leavesr;
380900111129           ENDIF;
381000111129
381100111129         //?commerciale valido data inizio e fine attività
381200130806           IF  CMMdtIni > datacor8  or
381300130806               CMMdtFin < datacor8;
381400111129             ErrMessage  = *on;
381500111129             ErrGenerico = *on;
381600111129             w09msg = $Msg(31);
381700111129             leavesr;
381800111129           ENDIF;
381900100817
382000100817
382100100817           // verifico se ha lo stesso commerciale unificante del commerciale
382200100817           // esterno della trattativa
382300130806             If sav_rgf <> CMMuni ;
382400100817               ErrMessage  = *on;
382500100817               ErrGenerico = *on;
382600100817               w09msg      = $Msg(26);
382700100817               leavesr;
382800100817             ENDIF;
382900100816
383000100816       ENDSR;
383100121128
383200121128       //--------------------------------------------------------------
383300121128       //?Routine di richiesta nuovo codice già prenotato.
383400121128       //--------------------------------------------------------------
383500121128       BEGSR GesW12;
383600121128
383700121128       //?Inizializzazione videata
383800121128         IF  wInzW12;
383900121128           exsr InzW12;
384000121128           wInzW12 = *off;
384100121128         ENDIF;
384200121128
384300121128       //?Gestione video
384400121128         DOW  not wFine12;
384500121128         //?Emissione Finestra di richiesta codice prenotato
384600121128           exfmt TA56W12;
384700121128           reset errMessage;
384800121128           reset errGenerico;
384900121128           clear W12msg;
385000121128
385100121128           SELECT;
385200121128
385300121128           //?- F12=Ritorno
385400121128             WHEN  dsp_aid = c_F12;
385500121128               wFine12 = *on ;
385600121128               ErrGenerico = *on;
385700121128
385800121128           //?- F06=Conferma
385900121128             WHEN  dsp_aid = c_F06  ;
386000121128               exsr CtrW12;
386100121128               IF  ErrGenerico;
386200121128                 iter;
386300121128               ENDIF;
386400121128             //?Riemetto la  videata
386500121128               wFine12 = *on;
386600121128               $video  = 'S5';
386700121128
386800121128             OTHER;
386900121128
387000121128           //?Controllo dati videata
387100121128               exsr CtrW12;
387200121128               IF  ErrGenerico;
387300121128                 iter;
387400121128               ENDIF;
387500121128
387600121128           ENDSL;
387700121128         ENDDO;
387800121128
387900121128       ENDSR;
388000121128
388100121128       //--------------------------------------------------------------
388200121128       //?Imposto i dati videata Nuovo codice già prenotato.
388300121128       //--------------------------------------------------------------
388400121128       BEGSR InzW12;
388500121128
388600121128          clear W12ksc;
388700121128          clear W12cmm;
388800121128          clear W12cmmd;
388900121128          wFine12 = *off;
389000121128          Vis_comm = *off;
389100121128
389200121128       ENDSR;
389300121128
389400121128       //--------------------------------------------------------------
389500121128       //?Controllo videata Nuovo codice già prenotato.
389600121128       //--------------------------------------------------------------
389700121128       BEGSR CtrW12;
389800121128
389900121128       //?Controllo codice cliente
390000121128         IF  W12ksc = 0;
390100121128           ErrGenerico = *on;
390200121128           ErrMessage  = *on;
390300121129           PosCurKSC12 = *on;
390400121128           W12msg = 'Immettere il codice cliente';
390500121128           leavesr;
390600121128         ENDIF;
390700121128
390800130806         chain  (1:DUTkci:W12ksc) CNACO00F;
390900121128         IF  %found(CNACO00F);
391000121128           ErrGenerico = *on;
391100121128           ErrMessage  = *on;
391200121129           PosCurKSC12 = *on;
391300121128           W12msg = 'Codice già presente in anagrafica';
391400121128           leavesr;
391500121128         ENDIF;
391600121128
391700121128       //?Se ho visualizzato il commerciale lo controllo
391800121128         IF  Vis_comm;
391900121128           exsr CtrW12cmm;
392000121128           leavesr;
392100121128         ENDIF;
392200121128
392300121128       //?Se cliente è ok controllo che il commerciale della trattativa
392400121128       //?abbia il cod. filiale = alla fil. cod. cliente
392500121128       //?in caso contrario devo richiedere il cod. commerciale
392600121128         IF  %subst(%editc(W12ksc:'X'):1:3) <>
392700121128             %subst(%editc(C01cmm:'X'):1:3);
392800121128           ErrGenerico = *on;
392900121129           PosCurCMM12 = *on;
393000121128           Vis_comm = *on;
393100121128           leavesr;
393200121128         ENDIF;
393300121128
393400121128       ENDSR;
393500121128
393600121128       //--------------------------------------------------------------
393700121128       //?Controllo commerciale videata nuovo codice già prenotato.
393800121128       //--------------------------------------------------------------
393900121128       BEGSR CtrW12cmm;
394000121128
394100121128       //?Ricerca codice commeriale
394200121128         IF  %scan('?' : W12cmm) > 0;
394300121128           ErrGenerico = *on;
394400130806           clear  TRMK43ds;
394500121128         //?ricerco con filiale cliente immesso a video
394600130806           iMK43fil = %int(%subst(%editc(W12ksc:'X'):1:3));
394700130806           TRMK43R (kpjba : TRMK43ds);
394800130806           if  oMK43err = *off  and  oMK43fxx = *blank;
394900130806             w12cmm  = %editc( oMK43cmm : 'X' );
395000130806             w12cmmd = oMK43des;
395100130806           endif;
395200121128           leavesr;
395300121128         ENDIF;
395400121128
395500121129       //?Campo obbligatorio
395600121128         IF  W12cmm = *blanks or W12cmm = *zeros;
395700121128           ErrMessage  = *on;
395800121128           ErrGenerico = *on;
395900121129           PosCurCMM12 = *on;
396000121128           W12msg = $Msg(25);
396100121128           leavesr;
396200121128         ENDIF;
396300121128
396400121129       //?Accetto solo numeri
396500121128         IF  %check(digits:W12cmm) > 0;
396600121128           ErrMessage  = *on;
396700121128           ErrGenerico = *on;
396800121129           PosCurCMM12 = *on;
396900121128           W12msg = $Msg(31);
397000121128           leavesr;
397100121128         ENDIF;
397200121129
397300121129       //?Deve essere della stessa filiale del cliente
397400121129         IF  %subst(%editc(W12ksc:'X'):1:3) <>
397500121129             %subst(W12cmm:1:3);
397600121129           ErrMessage  = *on;
397700121129           ErrGenerico = *on;
397800121129           PosCurCMM12 = *on;
397900121129           W12msg = $Msg(31);
398000121129           leavesr;
398100121129         ENDIF;
398200121128
398300121129       //?Controllo validità codice
398400130806         k_CMMcod = %int(w12cmm);
398500130806         chain  %kds(k_azcmm01)  AZCMM000;
398600130806         IF  NOT  %found(AZCMM01L);
398700121128           ErrMessage  = *on;
398800121128           ErrGenerico = *on;
398900121129           PosCurCMM12 = *on;
399000121128           W12msg = $Msg(31);
399100121128           leavesr;
399200121128         ENDIF;
399300130806         W12cmmd = CMMdes;
399400121128
399500121128       //?Commerciale valido no vari o clienti inattivi
399600130806         IF  CMMpar <> *blanks;
399700121128           ErrMessage  = *on;
399800121128           ErrGenerico = *on;
399900121129           PosCurCMM12 = *on;
400000121128           W12msg = $Msg(31);
400100121128           leavesr;
400200121128         ENDIF;
400300121128
400400121128       //?commerciale valido data inizio e fine attività
400500130806         IF  CMMdtIni > datacor8  or
400600130806             CMMdtFin < datacor8;
400700121128           ErrMessage  = *on;
400800121128           ErrGenerico = *on;
400900121129           PosCurCMM12 = *on;
401000121128           W12msg = $Msg(31);
401100121128           leavesr;
401200121128         ENDIF;
401300121128
401400121128       //?verifico se ha lo stesso unificante del commerciale
401500121128       //?della trattativa
401600130806         IF  sav_rgf <> CMMuni;
401700121128           ErrMessage  = *on;
401800121128           ErrGenerico = *on;
401900121129           PosCurCMM12 = *on;
402000121128           W12msg = $Msg(26);
402100121128           leavesr;
402200121128         ENDIF;
402300121128
402400121128       ENDSR;
402500100816
402600100118       //--------------------------------------------------------------
402700100118       //?Controllo videata Convalida  Offerte
402800100118       //--------------------------------------------------------------
402900100118       BEGSR Sr_Ctrs05 ;
403000100118
403100100121           reset errMessage ;
403200100121           reset errGenerico;
403300100121           reset PosCurDra  ;
403400100121           reset PosCurIva  ;
403500100121           reset PosCurCdf  ;
403600100121           reset PosCurCdp  ;
403700100121           reset PosCurAbi  ;
403800100121           reset PosCurCab  ;
403900100424           reset PosCurKsc  ;
404000100121           clear v1dmsg ;
404100100424
404200100120        // Controllo data reale affidamento
404300100120           If    C05Dra <> 0;
404400100118             clear wlbdat;
404500100120             g02dat = C05Dra;
404600100118             xsrda8(wlbdat);
404700100118             if g02err = '1';
404800100118               errMessage  = *on;
404900100118               errGenerico = *on;
405000100120               PosCurDra   = *on;
405100100120               v1dmsg = $Msg(04);
405200100118               leavesr;
405300100118             endif;
405400100118
405500100118        // Verfiico che non sia inferiore all'anno 2000 e superiore al 2039
405600100118           If    G02Inv < 20000101 or G02Inv > 20391231 ;
405700100118               errMessage  = *on;
405800100118               errGenerico = *on;
405900100120               PosCurDra   = *on;
406000100120               v1dmsg = $Msg(04);
406100100118               leavesr;
406200100120            endif;
406300100118
406400100120             c05dra = g02dat;
406500100120             wrkdra = g02inv;
406600100118           else ;
406700100118             errMessage  = *on;
406800100118             errGenerico = *on;
406900100120             PosCurDra   = *on;
407000100120             V1dmsg = $Msg(04);
407100100118             leavesr;
407200100118           endif;
407300100118
407400100120        // Partita IVA obbligatoria
407500100120        // Non è obbligatoria se nazione Italia e sottoconto intestazione fat-
407600100120        // tura ha nazione estera
407700151015        // Controllo la Partita IVA
407800151015           wmod = 'P' ;
407900151015           wiva = c05ivae + c05ivai ;
408000151015           ExSr Sr_ctriva ;
408100151015           If errMessage  = *on;
408200151015              leavesr;
408300151015           Endif ;
408400151015
408500100120         If c05ivai = *blanks and c05ivae = *blanks ;
408600151015
408700151015              If  (indsta<>*blanks or wsta=*blanks)
408800151015                  and xcfivaflg<>9 ;
408900100120                  errMessage  = *on;
409000100120                  errGenerico = *on;
409100100120                  PosCurIva = *on ;
409200100120                  V1dmsg = $Msg(07);
409300100120                  leavesr;
409400100120              endif;
409500100120           Else;
409600100120        // se non impostato devo riportare il codice iso della partita iva che
409700100120        // mi viene passato dal pgm di controllo
409800100121           if c05ivae <> %subst(xcfivapar: 1: 2)  ;
409900100121              c05ivae = %subst(xcfivapar: 1: 2) ;
410000100120              Blink_ivae = *on  ;
410100100120              leavesr ;
410200100120           endif ;
410300100120        // Errore forzabile se p.iva già presente su altro potenziale
410400100120        // se sul potenziale manca la p.iva
410500100120           IF cpocpo>0 and cpopiv=*blanks
410600100120                       and wiva<>saviva
410700100120                       and Wiva<>cod_forziva
410800100120                       and c05ivae<>'$$';
410900100120              clear savrag ;
411000100120              if c05ivae<>*blanks ;
411100100120                 codice=Wiva;
411200100120              else;
411300100120                 codice=c05ivai ;
411400100120              endif;
411500100120              exsr sr_PivAltroP ;
411600100120              exsr sr_msgiva ;
411700100120              If errMessage  = *on;
411800100120                 leavesr;
411900100120              Endif ;
412000100120           Endif ;
412100100120
412200100429        // Controllo se partita Iva presente su potenziale in contenzioso
412300100518           Exsr Sr_ctrstc ;
412301171220         // se si sta convalidando su un nuovo codice e ce n'è già uno
412302171220         // con stessa pIVA ma è in sofferenza non vado avanti
412304171220           IF  ClienteSofferenza and NuovoCliente;
412306171220             wforzacon = *off;
412308171220             ErrGenerico = *on;
412309171220             leavesr;
412311171214           ENDIF;
412400151015
412500151015         Endif ;
412600100429
412700100120        // Controllo il codice fiscale
412800100120         wmod = 'C' ;
412900100120         exsr sr_ctriva ;
413000100120         If errMessage  = *on;
413100100120            leavesr;
413200100120         Endif ;
413300100120
413400100120         If C05cdf = *blanks ;
413500100120
413600100120            if xcfivaflg<>9 and vis_codfisc = *on ;
413700100120               errMessage  = *on;
413800100120               errGenerico = *on;
413900100120               PosCurCdf   = *on;
414000100120               V1dmsg = $Msg(08);
414100100120               leavesr;
414200100120            endif;
414300100120
414400100120         else ;
414500100120
414600100120        // Errore forzabile se cod.fisc. già presente su altro potenziale
414700100120        // se sul potenziale manca la p.iva
414800100120            If ch1cpo>0 and cpocdf=*blanks
414900100120                        and C05cdf<>savcdf
415000100120                        and C05cdf<>cod_forzcdf ;
415100100120               clear savrag ;
415200100120               codice=c05cdf;
415300100120               exsr Sr_CdfAltroP ;
415400100120               exsr Sr_msgcdf  ;
415500100120               If errMessage  = *on;
415600100120                  leavesr;
415700100120               Endif ;
415800100120            Endif ;
415900100120         Endif;
416000100120
416100100120        // Se visualizzo le condizioni di pagamento eseguo anche i controlli
416200100120         If Vis_pagamento = *on ;
416300100120            exsr Sr_ctrPag ;
416400100120            If errMessage  = *on;
416500100120               leavesr;
416600100120            Endif ;
416700100120         Endif ;
416800120710
416900120710        //?Controllo se anagrafica provvisoria e non è allocata da altro
417000120710        //?utente, se così blocco la convalida, in questo modo evitiamo
417100120710        //?di avere degli aggiornamenti zoppi.
417200120710          exsr CtrAllocAna;
417300120710          IF  ErrMessage;
417400120710            leavesr;
417500120710          ENDIF;
417600100120
417700100120        // Eseguo i controlli sulle offerte da convalidare
417800100120         Exsr ctrsfl5 ;
417900100120         If errMessage  = *on;
418000100120            leavesr;
418100100120         Endif ;
418200100118
418300100118       ENDSR;
418400100118
418500100118       //--------------------------------------------------------------
418600100118       //?Pulizia SFL05
418700100118       //--------------------------------------------------------------
418800100118       BEGSR Pulsfl5;
418900100118
419000100118       // Pulizia subfile
419100100121         SflDsp5_N   = *on;
419200100121         SflDspCtl5_N= *on;
419300100118         write  TA56C05;
419400100121         SflDspCtl5_N= *off;
419500100118         SflEnd5     = *off;
419600100118
419700100118
419800100118         clear W_SflPag5;
419900100118         clear C05rcd;
420000100118         clear C05csr;
420100100118         clear S05nrr;
420200100118         clear V1Dmsg;
420300100118         errMessage  = *off;
420400100118         errGenerico = *off;
420500100118
420600100118       ENDSR ;
420700100118
420800100120       //--------------------------------------------------------------
420900100120       //?Controllo SFL05
421000100120       //--------------------------------------------------------------
421100100120       BEGSR Ctrsfl5;
421200100120
421300100120       // leggo il subfile e per ogni offerta verifico se su cliente
421400100120       // esistente la decorrenza e scadenza e se ci sono tariffe
421500100120       // particolari obsolete
421600100120
421700100128       numrec= 1 ;
421800100128       chain numrec Ta56S05 ;
421900100120
422000100128         DOW %found(TNTA56D);
422100100120
422200100120           C05rcd = S05nrr;
422300100120
422400100120         // verifico se con il coidce cliente proposto e codice offerta
422500100120         // ho già una offerta ed anche le sue date decorrenza e scadenza
422600100120           Chain (c05ksc:s05ctr) Tntam04l ;
422700100120           If %found(tntam04l) ;
422800100624              data_iso = %date(tamdst:*iso) ;
422900100624              data_iso = data_iso + %days(1);
423000100624              wdata8=%dec(data_iso);
423100130320
423200130320         //?Cerco la data ultima spedizione fatturata con la tariffa in
423300130320         //?modifica
423400130320            clear wDataSP;
423500130320            clear TASaas;
423600130320            clear TASmgs;
423700130320            setgt  (TAMKSC:TAMCTR) TITAS41C;
423800130320            readpe (TAMKSC:TAMCTR) TITAS41C;
423900130320            wDataSP = TASaas * 10000 + TASmgs;
424000130320
424100100120         // verifico le date decorrenza e scadenza se corrette
424200130320              if tamdst > sh5ddt or wdata8 <> sh5ddt
424300130320                 or sh5ddt < wDataSp;
424400100624         // emetto la finestra di richiesta se si desidera modificare
424500100624         // le date di scadenza della tariffa precedente
424600100624                 exsr  GesW07 ;
424700100708                 If errGenerico = *on;
424800100708                    leavesr ;
424900100708                 Endif ;
425000100624              endif ;
425100100120           endif ;
425200100120
425300100120        // verifico se l'offerta si porta dietro delle tariffe particolari
425400100120        // che non si utilizzano più
425500100120          xx = 1 ;
425600100120          dow  TParEli(XX)<>'  ' ;
425700100121               chain (c01nrv:s05ctr:sh5prg:tpareli(xx)) tiopt01l ;
425800100120               if  %found(tiopt01l) and tptatb=' ' ;
425900100120        // verifico la data di scadenza della tariffa che deve essere
426000100120        // <= alla data di eliminaziuone della tariffa particolare
426100100120                   if sh5dst>TPardat(XX) ;
426200100120        // Errore
426300100121                       V1dmsg=$msg(17) ;
426400100121                       %subst(v1Dmsg:9:3)= %editc(s05ctr:'X') ;
426500100121                       %subst(v1Dmsg:49:2)=tpareli(XX)  ;
426600100120
426700100120                       data_dmy= %date(tpardat(xx):*iso) ;
426800100121                       wdata  = %dec(data_dmy)  ;
426900100121                       %subst(v1dmsg:71:8) =(%editw(wdata:'  /  /  '));
427000100120                       data_dmy= %date(sh5dst:*iso) ;
427100100121                       wdata  = %dec(data_dmy)  ;
427200100121                       %subst(v1dmsg:22:8) =(%editw(wdata:'  /  /  '));
427300100120                       errMessage  = *on;
427400100120                       errGenerico = *on;
427500100120                       leavesr ;
427600100120                   endif;
427700100120               endif ;
427800100120
427900100120               xx += 1;
428000100120          enddo;
428100100121
428200100121           if  errMessage = *on  or  errGenerico = *on;
428300100121             leave;
428400100121           endif;
428500100121
428600100128           numrec += 1;
428700100128           chain numrec Ta56S05 ;
428800100120          enddo;
428900100120
429000100120       ENDSR;
429100100120
429200100120
429300100119       //--------------------------------------------------------------
429400100119       //?Recupero il primo codice cliente libero
429500100119       //--------------------------------------------------------------
429600100119       BEGSR Sr_NewCli ;
429700100119
429800100817        // pulisco il newksc
429900100817          clear newksc ;
429901171220          NuovoCliente = *off;
430000100119        // preparo la chiave
430100100119          nufaaa = 0 ;
430200100805          nuffil = fil_cmm ;
430300100119          nufcnu = dutkci ;
430400100119          chain (nufaaa:nufcnu:nuffil) flnuf01l ;
430500100119          if not %found(flnuf01l);
430600100119             clear flnuf    ;
430700100119             nufaaa = 0 ;
430800100805             nuffil = fil_cmm ;
430900100119             nufcnu = dutkci ;
431000100119             write flnuf   ;
431100100119             chain (nufaaa:nufcnu:nuffil) flnuf01l ;
431200100119          endif ;
431300100424          Dow newksc = 0 ;
431400100119              nufnum +=1 ;
431500110303              if nufnum = 10000;
431600100119                 nufnum = 1 ;
431700100119              endif ;
431800100119
431900100119          // verifico se esiste già un cliente con questo codice
432000100119              acoksc = (nuffil*10000) + nufnum ;
432100100119              setll (acokut:acokcc:acoksc) cnaco00f ;
432200100119              if  not %equal(cnaco00f) ;
432300100424                  newksc = acoksc ;
432400100119                  update flnuf    ;
432401171220                NuovoCliente = *on;
432500100119              endif ;
432600100119          enddo ;
432700100119
432800100119       ENDSR ;
432900100429
433000100429       //--------------------------------------------------------------
433100100429       //?Controllo stato del credito sullo stessa P.IVA
433200100429       //--------------------------------------------------------------
433300100429       BEGSR Sr_Ctrstc ;
433301171214
433302171214       reset ClienteSofferenza;
433400100429
433500100429       // - controllo se c'è già un cliente con la stessa p.IVA
433600100429       //
433700100518if  1   If  C05Ivae =  '$$' ;
433800100429            leavesr ;
433900100429        endif ;
434000100429
434100100429        If  C05Ivae = *Blanks ;
434200100429            codice = c05ivai ;
434300100429        else ;
434400100429            codice = c05ivae + c05ivai ;
434500100429        endif ;
434600100518        If codice <> sav_codice ;
434700100518           wforzacon = *off ;
434800100518           sav_codice = codice ;
434900100518        endif ;
435000100518
435100100429        setll (acokut:acokcc:codice) cnind02l ;
435200100429
435300100429        //  non trovo nessun altra partita IVA esco
435400100429
435500100429        If  Not %Equal ;
435600100429            Leavesr ;
435700100429        EndIf ;
435800100429        reade (acokut:acokcc:codice) cnind02l ;
435900100429do  2   Dow not %eof(cnind02l);
436000100429        //  deve avere codice cliente diverso
436100100429            If  ww_indksc <> c05ksc ;
436200100429               clear  tibs69ds;
436300100429               I69kcp = WW_indksc ;
436400100429               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
436500100429               wtibs69 = *On  ;
436600100429        // errore forzabile per stato del credito trovato
436700100429        // emesso con window e f6 di conferma
436800100429                 If w_clpcon <> *Blanks and wforzacon = *Off ;
436900100429        // decodifico stato del credito
437000100429                    clear ds4w ;
437100100429                    Tblkut = 1 ;
437200100429                    tblcod = '4W';
437300100429                    tblkey = W_clpcon ;
437400100429                    w6ccon = %subst(w_clpcon:2:2) ;
437500100429                    chain (Tblkut:tblcod:tblkey) tabel00f ;
437600100429
437700100429                    if %found(tabel00f) and tblflg = *blanks ;
437800100429                       ds4w = tbluni ;
437900100429                    endIf ;
438000100429                    w6dcon = §4wdes ;
438100100429                    dow 1=1 ;
438200100429                        exfmt  ta56w06  ;
438300100518                        If dsp_aid = c_F06 ;
438400100429                           leave ;
438500100429                        endif ;
438600100429                    enddo ;
438700100429                    wforzacon = *On ;
438702171214                    IF  §4Wtip <> *blanks;
438703171214                      ClienteSofferenza = *on;
438704171214                    ENDIF;
438800100429                    leave ;
438900100429                 EndIf;
439000100429             EndIf;
439100100518          reade (acokut:acokcc:codice) cnind02l ;
439200100429         EndDo;
439300100429       ENDSR;
439400120710
439500120710       //--------------------------------------------------------------
439600120710       //?Controllo se anagrafica allocata
439700120710       //--------------------------------------------------------------
439800120710       BEGSR CtrAllocAna;
439900120712
440000120712       //?Con N. Trattativa su anagrafica provvisoria
440100120712         IF  C01nrv > 0;
440200120712         //?TFACO
440300120712           exec sql
440400120712             select rrn(TFACO00F) into :tfaco_rrn
440500120712             from   TFACO00F
440600120712             where  ACOkut = 1 and ACOkcc = 151 and
440700120712                    ACOksc = :C01nrv;
440800120712           IF  tfaco_rrn > 0;
440900120712             wRrn = tfaco_rrn;
441000120712             wFil = 'TFACO00F';
441100120712             exsr CallTrul82;
441200120712             IF  UL82§sts = 'A';
441300120712               leavesr;
441400120712             ENDIF;
441500120712           ENDIF;
441600120712         //?TFIND
441700120712           exec sql
441800120712             select rrn(TFIND00F) into :tfind_rrn
441900120712             from   TFIND00F
442000120712             where  INDkut = 1 and INDkcc = 151 and
442100120712                    INDksc = :C01nrv;
442200120712           IF  tfind_rrn > 0;
442300120712             wRrn = tfind_rrn;
442400120712             wFil = 'TFIND00F';
442500120712             exsr CallTrul82;
442600120712             IF  UL82§sts = 'A';
442700120712               leavesr;
442800120712             ENDIF;
442900120712           ENDIF;
443000120712         //?TFCLP
443100120712           exec sql
443200120712             select rrn(TFCLP00F) into :tfclp_rrn
443300120712             from   TFCLP00F
443400120712             where  CLPkut = 1 and CLPkcc = 151 and
443500120712                    CLPksc = :C01nrv;
443600120712           IF  tfclp_rrn > 0;
443700120712             wRrn = tfclp_rrn;
443800120712             wFil = 'TFCLP00F';
443900120712             exsr CallTrul82;
444000120712             IF  UL82§sts = 'A';
444100120712               leavesr;
444200120712             ENDIF;
444300120712           ENDIF;
444400120712         //?TFCLS
444500120712           exec sql
444600120712             select rrn(TFCLS00F) into :tfcls_rrn
444700120712             from   TFCLS00F
444800120712             where  CLSksc = :C01nrv;
444900120712           IF  tfcls_rrn > 0;
445000120712             wRrn = tfcls_rrn;
445100120712             wFil = 'TFCLS00F';
445200120712             exsr CallTrul82;
445300120712             IF  UL82§sts = 'A';
445400120712               leavesr;
445500120712             ENDIF;
445600120712           ENDIF;
445700120712         ENDIF;
445800120712
445900120712       //?Con Codice cliente proposto su anagrafica
446000120712         IF  C05ksc > 0;
446100120712         //?CNACO
446200120712           exec sql
446300120712             select rrn(CNACO00F) into :cnaco_rrn
446400120712             from   CNACO00F
446500120712             where  ACOkut = 1 and ACOkcc = 151 and
446600120712                    ACOksc = :C05ksc;
446700120712           IF  cnaco_rrn > 0;
446800120712             wRrn = cnaco_rrn;
446900120712             wFil = 'CNACO00F';
447000120712             exsr CallTrul82;
447100120712             IF  UL82§sts = 'A';
447200120712               leavesr;
447300120712             ENDIF;
447400120712           ENDIF;
447500120712         //?CNIND
447600120712           exec sql
447700120712             select rrn(CNIND00F) into :cnind_rrn
447800120712             from   CNIND00F
447900120712             where  INDkut = 1 and INDkcc = 151 and
448000120712                    INDksc = :C05ksc;
448100120712           IF  cnind_rrn > 0;
448200120712             wRrn = cnind_rrn;
448300120712             wFil = 'CNIND00F';
448400120712             exsr CallTrul82;
448500120712             IF  UL82§sts = 'A';
448600120712               leavesr;
448700120712             ENDIF;
448800120712           ENDIF;
448900120712         //?CNCLP
449000120712           exec sql
449100120712             select rrn(CNCLP00F) into :cnclp_rrn
449200120712             from   CNCLP00F
449300120712             where  CLPkut = 1 and CLPkcc = 151 and
449400120712                    CLPksc = :C05ksc;
449500120712           IF  cnclp_rrn > 0;
449600120712             wRrn = cnclp_rrn;
449700120712             wFil = 'CNCLP00F';
449800120712             exsr CallTrul82;
449900120712             IF  UL82§sts = 'A';
450000120712               leavesr;
450100120712             ENDIF;
450200120712           ENDIF;
450300120712         //?FNCLS
450400120712           exec sql
450500120712             select rrn(FNCLS00F) into :fncls_rrn
450600120712             from   FNCLS00F
450700120712             where  CLSksc = :C05ksc;
450800120712           IF  fncls_rrn > 0;
450900120712             wRrn = fncls_rrn;
451000120712             wFil = 'FNCLS00F';
451100120712             exsr CallTrul82;
451200120712             IF  UL82§sts = 'A';
451300120712               leavesr;
451400120712             ENDIF;
451500120712           ENDIF;
451600120712         ENDIF;
451700120712
451800120712       //?Con Codice cliente trattativa su anagrafica
451900120712         IF  C01ksc > 0;
452000120712         //?CNACO
452100120712           exec sql
452200120712             select rrn(CNACO00F) into :cnaco_rrn
452300120712             from   CNACO00F
452400120712             where  ACOkut = 1 and ACOkcc = 151 and
452500120712                    ACOksc = :C01ksc;
452600120712           IF  cnaco_rrn > 0;
452700120712             wRrn = cnaco_rrn;
452800120712             wFil = 'CNACO00F';
452900120712             exsr CallTrul82;
453000120712             IF  UL82§sts = 'A';
453100120712               leavesr;
453200120712             ENDIF;
453300120712           ENDIF;
453400120712         //?CNIND
453500120712           exec sql
453600120712             select rrn(CNIND00F) into :cnind_rrn
453700120712             from   CNIND00F
453800120712             where  INDkut = 1 and INDkcc = 151 and
453900120712                    INDksc = :C01ksc;
454000120712           IF  cnind_rrn > 0;
454100120712             wRrn = cnind_rrn;
454200120712             wFil = 'CNIND00F';
454300120712             exsr CallTrul82;
454400120712             IF  UL82§sts = 'A';
454500120712               leavesr;
454600120712             ENDIF;
454700120712           ENDIF;
454800120712         //?CNCLP
454900120712           exec sql
455000120712             select rrn(CNCLP00F) into :cnclp_rrn
455100120712             from   CNCLP00F
455200120712             where  CLPkut = 1 and CLPkcc = 151 and
455300120712                    CLPksc = :C01ksc;
455400120712           IF  cnclp_rrn > 0;
455500120712             wRrn = cnclp_rrn;
455600120712             wFil = 'CNCLP00F';
455700120712             exsr CallTrul82;
455800120712             IF  UL82§sts = 'A';
455900120712               leavesr;
456000120712             ENDIF;
456100120712           ENDIF;
456200120712         //?FNCLS
456300120712           exec sql
456400120712             select rrn(FNCLS00F) into :fncls_rrn
456500120712             from   FNCLS00F
456600120712             where  CLSksc = :C01ksc;
456700120712           IF  fncls_rrn > 0;
456800120712             wRrn = fncls_rrn;
456900120712             wFil = 'FNCLS00F';
457000120712             exsr CallTrul82;
457100120712             IF  UL82§sts = 'A';
457200120712               leavesr;
457300120712             ENDIF;
457400120712           ENDIF;
457500120712         ENDIF;
457600120712
457700120712       ENDSR;
457800120712
457900120712       //--------------------------------------------------------------
458000120712       //?Richiamo TRUL82R.
458100120712       //--------------------------------------------------------------
458200120712       BEGSR CallTRUL82;
458300120712
458400120712         clear TRUL82DS;
458500120712         UL82§rrn = wRrn;
458600120712         UL82§fil = wFil;
458700120712         UL82§win = wWin;
458800120712         UL82§f7  = wF7;
458900120712         UL82§mss = wMss;
459000120712         UL82§msw = wMsw;
459100120712         callp(e) TRUL82R (trul82ds);
459200120712         IF  UL82§sts = 'A';
459300120712           ErrMessage  = *on;
459400120712           ErrGenerico = *on;
459500120712           V1Dmsg      = UL82§msw;
459600120712         ENDIF;
459700120710
459800120710       ENDSR;
459900100429
460000100121       //--------------------------------------------------------------
460100100121       //?Conferma  SFL05
460200100121       //--------------------------------------------------------------
460300100122       BEGSR Sr_F6ConvOff ;
460400100121
460500100121       // leggo il subfile e per ogni tariffa convalido i dati
460600100121       s05nrr = 1 ;
460700100121       chain s05nrr ta56s05 ;
460800100121       Dow %found(tnta56d) ;
460900100121
461000100121       // aggancio il file testate offerta per aggiornare i dati
461100100121       chain (c01nrv:s05ctr:sh5prg) Tnofm01l ;
461200100121       If %found(tnofm01l) ;
461300100121          tamsis = s05sis ;
461400100121          update tnofm000 ;
461500100121       endif;
461600100121
461700100122       // gestione anagrafica solo se devo creare un nuovo cliente
461800100424       If s05nrr = 1 ;
461900100122          exsr sr_Anagra ;
462000100203       // esito la  trattativa in positivo
462100100203          visesi = 'P' ;
462200100203          update tivis000 ;
462300100203       // rialloco il record
462400100203          chain  ita56nrv tivis05l ;
462500100122       endif ;
462600100122
462700100122       // gestione tariffe
462800100122       exsr Sr_tariffe ;
462900100122
463000100424       // gestione note
463100100424       exsr sr_Note ;
463200100424
463300100122       // Cancello attività e note attività legate all'offerta
463400100122             $offerta = *on ;
463500100122             exsr Sr_Delatc ;
463600100122             $offerta = *off;
463700100122
463800100122       // aggiorno i dati su TIVOF
463900110112       chain (c01nrv:s05ctr:sh5prg) tivof11l;
464000100122       vofksc = tamksc ;
464100101026       vofprt = wprg   ;
464200100122       vofeso = 'C' ;
464300100122       vofdra = wrkdra ;
464400100122       vofdco = datacor8 ;
464500100122       update tivof000 ;
464600100126       $Esitata = *on ;
464700100126
464800100126       // aggancio il record del subfile precedente e lo aggiorno
464900100126       chain sh5nrr ta56s01 ;
465000100126       If %found(tnta56d) ;
465100100126          clear s01opz ;
465200100312          sh1dpa = c05dra ;
465300100312          data_dmy = %date(c05dra :*eur);
465400100312          s01dpa  = %dec(data_dmy)  ;
465500100126          sh1conv = *on ;
465600100312          sh1eso = 'C' ;
465700100204          If  sh1n12 = '1' ;
465800100204              note10_tar = *on ;
465900100204          else ;
466000100204              note10_tar = *off ;
466100100204          endif ;
466200100126          update ta56s01 ;
466300100126       endif ;
466400100126
466500100126       s05nrr +=1 ;
466600100126       chain s05nrr ta56s05 ;
466700100121
466800100122       enddo ;
466900100122
467000110118       // se si tratta di codifica di un nuovo cliente
467100100122       // fine lettura e convalida offerte emetto videata per stampa lettera
467200110118       If Nuovo_cli = *on  ;
467300110118          parcli = c05ksc ;
467400110118          parrgs = c01rag ;
467500110118          kpjbu = param ;
467600110118          fnlv43r (kpjba) ;
467700110118       Endif ;
467800100122
467900100122       // richiamo il pgm gestione anagrafica clienti
468000100122       Clear     tnta60ds ;
468100100122       ta60flg = 'C'   ;
468200100122       ta60nrv = c05ksc;
468300100122       TNTA60R (Kpjba:tnta60ds);
468400100122       //  devo abilitare il cliente ai servizi ON LINE
468500100122       //  se cliente nuovo
468600100122       //  se cliente non nuovo ma lui o suo papà (WW) non sono ancora abilitati
468700100122       ExSr   Sr_Psw  ;
468800100122
468900100122       // propongo una nuova attività di richiamo con causale 32 cliente
469000100122       // acuisito
469100100126         causale = '32' ;
469200100122
469300100122       // Richiamo la routine che crea nuove attività
469400100122         exsr Sr_CreaAtt ;
469500100122
469600100122
469700100122       // Richiamo la routine che verifica ed esita la trattativa
469800100122             exsr Sr_EsiTrat ;
469900100122
470000100122       // Se non ho + OFFERTE chiedo se è il caso di chiudere la trattativa
470100100126             If N_off_daesi= 0 ;
470200100122       // emetto la videata di richiesta di conferma
470300100122                $video = 'W3' ;
470400100122                $inzw03 = *on  ;
470500100122                leavesr ;
470600100122
470700100122             Endif ;
470800100122
470900100122
471000100122          $inzs01 = *on  ;
471100100122          $video  = 'S1' ;
471200100122
471300100122
471400100122
471500100121       ENDSR ;
471600100121
471700100121       //--------------------------------------------------------------
471800100121       //?Gestione Anagrafiche
471900100121       //--------------------------------------------------------------
472000100121       BEGSR Sr_anagra ;
472100100121
472200100504       clear tnta55ds ;
472300100505       ita55nrv = c01nrv ;
472400100505       ita55tpv = c01tpv ;
472500100505       ita55ksc = c01ksc ;
472600100505       ita55nwksc = c05ksc ;
472700100917       If c01tpv = 'N' ;
472800100505       If c01esi = ' ';
472900100505          ita55prima = 'SI' ;
473000100505       endif ;
473100100917       endif;
473200100917       // se tipo visita di aumento verifico se ho già confermato
473300101007       // sul cliente che devo confermare
473400101007       //      If c01tpv = 'A' ;
473500100917
473600100917         exec sql
473700100917          declare t2  cursor for
473800100917          select vofksc
473900100917          from tivof00f where vofnrv = :visnrv and vofeso = 'C' and
474000110112          vofdco > 0   and vofksc = :c05ksc and vofctr <> 999 ;
474100100917
474200100917         exec sql
474300100917           open t2;
474400100917           IF sqlcod = 0;
474500100917           exec sql
474600100917             fetch next from t2  into :vofksc;
474700100917             IF sqlcod = 100 or sqlcod < 0;
474800100917               ita55prima = 'SI' ;
474900100917             else ;
475000100917               ita55prima = '  ' ;
475100100917             endif ;
475200100917           Endif;
475300100917
475400100917         exec sql close t2 ;
475500101007       //   endif ;
475600100917
475700100505       If Vis_pagamento  = *on ;
475800100505          ita55pag = '1' ;
475900100505          ita55abi = c05abi ;
476000100505          ita55cab = c05cab ;
476100100520          ita55cdp = '000' + c05cdp ;
476200100505       endif ;
476300100504
476400100505           If c05ivae = *Blanks  ;
476500100505              ita55iva = c05ivai ;
476600100505           else ;
476700100505              ita55iva = c05ivae + c05ivai ;
476800100505           EndIf;
476900100505
477000100505       ita55cdf = c05cdf ;
477100100818       ita55cmm = comcon ;
477200100505       ita55cpo = ch1cpo ;
477300100505       Tnta55r (kpjba:tnta55ds) ;
477400100505
477500100505        // verifico se ho creato nuovo cliente
477600100505       If ota55new = '1' ;
477700100505          Nuovo_cli = *on  ;
477800100505       else ;
477900100505          Nuovo_cli = *off ;
478000100505       endif ;
478100100505
478200100122       // aggiorno anagrafica clienti potenziali
478300100122
478400100122       If ch1cpo > 0 ;
478500100122          chain ch1cpo tncpo01l ;
478600100122          If %found(tncpo01l) ;
478700160803           //?Salvo l'immagine precedente dell'anagrafica potenziale
478800160803            exsr ImmaginePrima;
478900100122        // partita IVA
479000100122             If cpopiv = *blanks and indiva <> *blanks;
479100160831                //cpopiv = indiva ;
479200160831                cpopiv = ita55iva;
479300100316                cpodtr = datacor8 ;
479400100122             endif ;
479500100122        // codice fiscale
479600100122             If cpocdf = *blanks and indcdf <> *blanks;
479700160831                //cpocdf = indcdf ;
479800160831                cpocdf = ita55cdf;
479900100316                cpodtr = datacor8 ;
480000100122             endif ;
480100100122
480200100122             update tncpo000 ;
480300160803
480400160803           //?Scrivo la Variazione fatta sull'anagrafica potenziale
480500160803            exsr ScriviVar;
480600100122          endif ;
480700100122        endif ;
480800100122
480900100121       ENDSR ;
481000100122
481100100122       //--------------------------------------------------------------
481200100122       //?Gestione TARIFFE
481300100122       //--------------------------------------------------------------
481400100122       BEGSR Sr_Tariffe ;
481500100122
481600100122       // verifico se esiste già una tariffa annullata e ci vado sopra
481700100122       chain (c05ksc:s05ctr) Tntam04l ;
481800100122       // non trovato nessun progressivo per quella tariffa imposto
481900100122       // il nuovo progressivo a zero
482000100316       If not %found(tntam04l);
482100100122          wprg   = 0 ;
482200100122       else ;
482300100122          wprg   = tamprg + 1 ;
482400100122       endif ;
482500100122       chain (c05ksc:s05ctr:wprg) Tntam01l ;
482600100122
482700100122       // aggancio il file testate offerta per aggiornare i dati
482800100122       chain (c01nrv:s05ctr:sh5prg) Tnofm01l ;
482900100122
483000100122       tamprg = wprg ;
483100100122       tamatb = ' ' ;
483200100122       tamftr = ' ' ;
483300100316       tamduv = datacor8 ;
483400100316       tamdtr = datacor8 ;
483500100122       tamksc = c05ksc ;
483600100122       if not %found(tntam01l) ;
483700100122          write tntam000 ;
483800100122       else ;
483900100122          update tntam000 ;
484000100122       endif ;
484100100122       delete tnofm000 ;
484200100122
484300100122       // scrivo lo storico
484400100122
484500100122       Clear Titav000     ;
484600100122       TavKsc = TamKsc    ;
484700100122       TavCtr = TamCtr    ;
484800100122       TavPrg = TamPrg    ;
484900100122       TavTrd = 'TES'     ;
485000100122       TavCta = 'O'       ;
485100100316       TavDav = datacor8  ;
485200100122       TavOrv = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0) ;
485300110325       TavNoj = sdsjob    ;
485400100122       TavPru = sdsuse    ;
485500100316       TavDtr = datacor8  ;
485600100122       Write     Titav000 ;
485700100122
485800100122       // scrivo dettaglio tariffe
485900100122       // prima verifico se ci sono dei record da cancellare
486000100122       // con il nuovo progressivo
486100100122       setll (c05ksc:s05ctr:wprg) Titad04l ;
486200100122       reade (c05ksc:s05ctr:wprg) Titad04l ;
486300100122
486400100122       dow not %eof(titad04l) ;
486500100122           delete titad000 ;
486600100122           reade (c05ksc:s05ctr:wprg) Titad04l ;
486700100122       enddo ;
486800100122
486900100122       // scrivo il dettaglio
487000100122       setll (c01nrv:s05ctr:sh5prg) Tiofd01l ;
487100100122       reade (c01nrv:s05ctr:sh5prg) Tiofd01l ;
487200100122
487300100122       dow not %eof(tiofd01l) ;
487400100122           tadftr = ' ';
487500100316           taddtr = datacor8 ;
487600100122           tadksc = c05ksc ;
487700100122           tadprg = wprg ;
487800100122           write titad000 ;
487900100122           delete tiofd000 ;
488000100122           reade (c01nrv:s05ctr:sh5prg) Tiofd01l ;
488100100122       enddo ;
488200100122
488300100122       // scrivo testata   tariffe particolari
488400100122       // prima verifico se ci sono dei record da cancellare
488500100122       // con il nuovo progressivo
488600100122       setll (c05ksc:s05ctr:wprg) Titpt01l ;
488700100122       reade (c05ksc:s05ctr:wprg) Titpt01l ;
488800100122
488900100122       dow not %eof(titpt01l) ;
489000100122           delete titpt000 ;
489100100122           reade (c05ksc:s05ctr:wprg) Titpt01l ;
489200100122       enddo ;
489300100122
489400100122       // scrivo il dettaglio
489500100122       setll (c01nrv:s05ctr:sh5prg) Tiopt01l ;
489600100122       reade (c01nrv:s05ctr:sh5prg) Tiopt01l ;
489700100122
489800100122       dow not %eof(tiopt01l) ;
489900100122           tptftr = ' ';
490000100316           tptdtr = datacor8 ;
490100100316           tptduv = datacor8 ;
490200100122           tptksc = c05ksc ;
490300100122           tptprg = wprg ;
490400100122           write titpt000 ;
490500100122           delete tiopt000 ;
490600100122           reade (c01nrv:s05ctr:sh5prg) Tiopt01l ;
490700100122       enddo ;
490800100122
490900100122       // scrivo dettaglio tariffe particolari
491000100122       // prima verifico se ci sono dei record da cancellare
491100100122       // con il nuovo progressivo
491200100122       setll (c05ksc:s05ctr:wprg) Titpd01l ;
491300100122       reade (c05ksc:s05ctr:wprg) Titpd01l ;
491400100122
491500100122       dow not %eof(titpd01l) ;
491600100122           delete titpd000 ;
491700100122           reade (c05ksc:s05ctr:wprg) Titpd01l ;
491800100122       enddo ;
491900100122
492000100122       // scrivo il dettaglio
492100100122       setll (c01nrv:s05ctr:sh5prg) Tiopd01l ;
492200100122       reade (c01nrv:s05ctr:sh5prg) Tiopd01l ;
492300100122
492400100122       dow not %eof(tiopd01l) ;
492500100122           tpdftr = ' ';
492600100316           tpddtr = datacor8 ;
492700100122           tpdksc = c05ksc ;
492800100122           tpdprg = wprg ;
492900100122           write titpd000 ;
493000100122           delete tiopd000 ;
493100100122           reade (c01nrv:s05ctr:sh5prg) Tiopd01l ;
493200100122       enddo ;
493300100122
493400100122       // scrivo tariffe di giacenza
493500100122       // prima verifico se ci sono dei record da cancellare
493600100122       // con il nuovo progressivo
493700100122       chain (c05ksc:s05ctr:wprg) Titgc01l ;
493800100122
493900100122       If %found(titgc01l) ;
494000100122           delete titgc000 ;
494100100122           reade (c05ksc:s05ctr:wprg) Titgc01l ;
494200100122       endif ;
494300100122
494400100122       // scrivo il dettaglio
494500100122       chain (c01nrv:s05ctr:sh5prg) Tiogc01l ;
494600100122
494700100122       If %found(tiogc01l) ;
494800100122           tgcftr = ' ';
494900100316           tgcdtr = datacor8 ;
495000100316           tgcduv = datacor8 ;
495100100122           tgcksc = c05ksc ;
495200100316           tgcprg = wprg ;
495300100122           write titgc000 ;
495400100126           delete tiogc000 ;
495500100122       endif ;
495600150223
495700150223       // richiamo il programma di inglobamento per verificare
495800150223       // la presenza o meno di tutte le tariffe particolari
495900150223       // obbligatorie e il fuel se corretto in  base alla
496000150223       // tabella VMA negazione VMA
496100150223       clear Tnta25ds ;
496200150223       clear Tnta25tad;
496300150223       clear Tnta25tgc;
496400150223       clear Tnta25tpd;
496500150223       clear Tnta25tpt;
496600150223       ta25tipo = 'T' ;
496700150223       ta25ksco = tamksc ;
496800150223       ta25ctro = tamctr ;
496900150223       ta25prgo = tamprg ;
497000150223       ta25tipn = 'T' ;
497100150223       ta25kscn = tamksc ;
497200150223       ta25ctrn = tamctr ;
497300150302       ta25prgn = %editc(tamprg:'X');
497400150223       ta25tam = 'N' ;
497500150223       ta25fie = tamfie ;
497600150223       ta25tam = 'N' ;
497700150223       ta25tad = 'N' ;
497800150223       ta25tpt = 'N' ;
497900150223       ta25tpd = 'N' ;
498000150223       ta25tgc = 'N' ;
498100150223       ta25cat = 'N' ;
498200150223       ta25inglo = 'S';
498300150223       Tnta25r (Kpjba:Tnta25ds:Tnta25tad:Tnta25tgc:Tnta25tpd:Tnta25tpt);
498400100122
498500100122       // creao i dati per il conto economico
498600100122       clear tnta33ds ;
498700100122       wta33 = *on ;
498800100122       d33nrv = c01nrv ;
498900100122       d33ksc = c05ksc;
499000100122       d33ctr = tamctr ;
499100100122       d33prg = tamprg ;
499200140919       D33pr2 = SH5prg;
499300100122       kpjbu = tnta33ds ;
499400100122       tnta33r (Kpjba) ;
499500100122
499600100122       ENDSR ;
499700100122
499800100122       //--------------------------------------------------------------
499900100122       //?Gestione Note
500000100122       //--------------------------------------------------------------
500100100122       BEGSR Sr_Note    ;
500200100424
500300100424         xx=%lookup('10' : tno) ;
500400100424       // cerco le note 10 relative al codice tariffa
500500100424         ntcapl = 'T' ;
500600100425         ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
500700100424         ntcnk2 = %editc(s05ctr: 'X') ;
500800100424         ntctnt = '10' ;
500900100424         setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
501000100424         If %equal(tfntc01l) ;
501100100424       // verifico se sono da duplicare nell'eventualità che ci
501200100424       // siano altre note già legate a cliente e tariffa
501300100424           if  tdup(xx) = 'S' and ttdp(xx) = 'R' ;
501400100424          // in caso di replace sulle note già esistenti
501500100424          // deleto le note 10 codice tariffa del cliente in convalida
501600100424                 ntcapl = 'C' ;
501700100424                 ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
501800100424                 ntcnk2 = %editc(tamctr: 'X') ;
501900100424                 ntctnt = '10' ;
502000100424                 setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
502100100424                 reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
502200100424                 dow not %eof(tfntc01l) ;
502300100424                     delete tfntc ;
502400100424                     reade  (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
502500100424                 enddo ;
502600100424           endif ;
502700100424           if  tdup(xx) = 'S'  ;
502800100424        // ora leggo le note 10 trattativa e le scrivo con il ksc tariffa
502900100424               ntcapl = 'T' ;
503000100425               ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
503100100424               ntcnk2 = %editc(s05ctr: 'X') ;
503200100424               ntctnt = '10' ;
503300100424               setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
503400100424               reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
503500100424               dow not %eof(tfntc01l) ;
503600100424                   ntcapl = 'C' ;
503700100424                   ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
503800100424                   ntcntr = datacor6;
503900100424                   write tfntc ;
504000100424                   reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
504100100424               enddo ;
504200100424            endif ;
504300100424
504400100424        // verifico e deleto note 10 legate all'offerta se non sono
504500100424        // presenti altri progressivi
504600100425            keynrv = c01nrv ;
504700100425            Keyctr = s05ctr ;
504800100424            exsr Sr_Delnote10 ;
504900100424
505000100424          else  ;
505100100424        // se non trovate note 10 legate alla tariffa cerco quelle
505200100424        // legate al cliente (tariffa a blank)
505300100424
505400100424         ntcapl = 'T' ;
505500100425         ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
505600100424         clear ntcnk2 ;
505700100424         ntctnt = '10' ;
505800100424         setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
505900100424         If %equal(tfntc01l) ;
506000100424        // verifico se sono da duplicare nell'eventualità che ci
506100100424        // siano altre note già legate a cliente e tariffa
506200100424           if  tdup(xx) = 'S' and ttdp(xx) = 'R' ;
506300100424          // in caso di replace sulle note già esistenti
506400100424          // deleto le note 10 codice tariffa del cliente in convalida
506500100424                 ntcapl = 'C' ;
506600100424                 ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
506700100424                 ntcnk2 = %editc(tamctr: 'X') ;
506800100424                 ntctnt = '10' ;
506900100424                 setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
507000100424                 reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
507100100424                 dow not %eof(tfntc01l) ;
507200100424                     delete tfntc ;
507300100424                     reade  (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
507400100424                 enddo ;
507500100424           endif ;
507600100424           if  tdup(xx) = 'S'  ;
507700100424         // ora leggo le note 10 trattativa e le scrivo con il ksc tariffa
507800100424               ntcapl = 'T' ;
507900100425               ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
508000100424               clear ntcnk2 ;
508100100424               ntctnt = '10' ;
508200100424               setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
508300100424               reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
508400100424               dow not %eof(tfntc01l) ;
508500100424                   ntcapl = 'C' ;
508600100424                   ntcnk1 = %editc(dutkci: 'X') + %editc(tamksc:'X');
508700100424                   ntcnk2 = %editc(tamctr: 'X') ;
508800100424                   ntcntr = datacor6;
508900100424                   write tfntc ;
509000100424                   reade    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
509100100424               enddo ;
509200100424            endif ;
509300100424          endif ;
509400100424         endif ;
509500100122
509600100122       ENDSR ;
509700160803
509800160803       //--------------------------------------------------------------
509900160803       //?Scrivo l'immagine Precedente.
510000160803       //--------------------------------------------------------------
510100160803       BEGSR ImmaginePrima;
510200160803
510300160803         clear TRMK30DS;
510400160803         IMK30immag = 'P';
510500160803         IMK30tvp = 'B';
510600160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
510700160803
510800160803       ENDSR;
510900160803
511000160803       //--------------------------------------------------------------
511100160803       //?Scrivo Storico Variazioni.
511200160803       //--------------------------------------------------------------
511300160803       BEGSR ScriviVar;
511400160803
511500160803         clear TRMK30DS;
511600160803         IMK30pru = knmus;
511700160803         IMK30noj = knmeb;
511800160803         IMK30pgm = 'TNTA56R';
511900160803         IMK30immag = 'D';
512000160803         IMK30cta = 'M';
512100160803         IMK30tvp = 'B';
512200160803
512300160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
512400160803
512500160803       ENDSR;
512600100424
512700100115       //--------------------------------------------------------------
512800100115       //?Operazioni iniziali.
512900100115       //--------------------------------------------------------------
513000100115       BEGSR RoutInz;
513100100115
513200100115         // Impostazione campi "fissi"
513300100115         T01pgm = SDSpgm;
513400100115         TBLkut = 1;
513500100115
513600100115         // Reperimento dati job
513700100115         exsr DatiJob;
513800100115
513900100115         // Verifica errori e autorità profilo
514000100115         clear  wAbi;
514100100115         clear  dLAT;
514200100115         select;
514300100115         // - Se ho errori nei dati utente esco dal pgm
514400100115           when  DUTerr = 'E';
514500100115             $Fine = *on;
514600100115         // - Se non c'è l'abilitazione:
514700100115         //   - se 1° livello, abilitazioni al terminal
514800100115         //   - se 2° livello, abilitazioni al punto operativo
514900100115         //   - se sede è impossibile ma se capita mando a fine il pgm
515000100115           when  UTEaut = *blank;
515100100115             select;
515200100115               when  DUTlpo = '1';
515300100115                 wAbi  = 'TP';
515400100115               when  DUTlpo = '2';
515500100115                 wAbi  = 'PO';
515600100115               when  DUTlpo = 'S';
515700100115                 $Fine = *on;
515800100115             endsl;
515900100115         // - Altrimenti si caricano le abilitazioni del profilo
516000100115           other;
516100100115             dUTE01 = UTEfaf;
516200100115             if  §UTEpot <> *blank;
516300100115               wAbi = §UTEpot;
516400100115             else;
516500100115               wAbi = UTEaut;
516600100115             endif;
516700100115         ENDSL;
516800100115
516900100115         // Controllo se ok l'abilitazione dell'utente
517000100115         reset TIBS02ds;
517100100115         T02sif = knsif;
517200100115         T02cod = 'LAT';
517300100115         T02ke1 = wAbi;
517400100115         TNTBE_RicercaControllo  (kpjba : tibs02ds);
517500100115
517600100115         if  T02err  = *blank;
517700100115           dLAT = T02uni;
517800100115         endif;
517900100115
518000100115         // Errore o utente non abilitato
518100100115         if  T02err <> *blanks   or
518200100115             §LATabi = 'S';
518300100115           $ErrGrave   = *on;
518400100115           errMessage  = *on;
518500100115           errGenerico = *on;
518600100115           V1Dmsg = $Msg(01);
518700100115           leavesr;
518800100115         endif;
518900100115
519000100115         clear trul31ds  ;
519100100115         i31abi=wabi   ;
519200100115         I31cdi = DUTdis   ;
519300100115         I31car = DUTare   ;
519400100115         I31cpo = DUTpou   ;
519500100115         callp TRUL31R (kpjba:trul31ds)   ;
519600100115
519700100115         if o31pog<=*zeros  ;
519800100115           $ErrGrave   = *on;
519900100115           errMessage  = *on;
520000100115           errGenerico = *on;
520100100115           V1Dmsg = $Msg(01);
520200100115           leavesr;
520300100115         endif;
520400100115
520500100115
520600100115         // Intabellamento filiali e relative aree
520700100115         setll *loval AZORG;
520800100115         read  AZORG;
520900100115         dow  NOT  %eof(AZORG01L);
521000100115           if  ORGfva = *blank;
521100100115             xx += 1;
521200100115             $Fil(xx) = ORGfil;
521300100115             $Car(xx) = ORGcar;
521400100115           endif;
521500100115           read  AZORG;
521600100115         enddo;
521700100115
521800100115       // Reperisco data corrente
521900100115             data_iso = (%date());
522000100122             datacor8 = %dec(data_iso);
522100100125             data_ymd = (%date());
522200100121             datacor6 = %dec(data_ymd) ;
522300100115
522400100115        //  Carico schiera con tabella TR (tipo tariffa))
522500100115             Clear   xx;
522600100115             TBLcod = 'TR';
522700100115             setll (TBLkut : TBLcod) TABEL;
522800100115             reade (TBLkut : TBLcod) TABEL;
522900100115             dow not %eof(TABEL00F);
523000100115                 If tblflg = ' ' and tblkey <> *blanks ;
523100100115                    xx = xx + 1 ;
523200100115                    ctr(xx) = tblkey ;
523300100115                    dtr(xx) = tbluni;
523400100115                 endif ;
523500100115                 reade (TBLkut : TBLcod) TABEL;
523600100115             EndDo ;
523700100122
523800100122        //  Carico schiera con tabella 1T (tipo nota)
523900100122             Clear   xx;
524000100122             TBLcod = '1T';
524100100122             setll (TBLkut : TBLcod) TABEL;
524200100122             reade (TBLkut : TBLcod) TABEL;
524300100122             dow not %eof(TABEL00F);
524400100122                 If tblflg = ' ' and tblkey <> *blanks ;
524500100122                    xx = xx + 1 ;
524600100122                    ds1t = tbluni ;
524700100122                    tno(xx) = tblkey ;
524800100122                    tdup(xx) = §1tdup;
524900100122                    ttdp(xx) = §1ttdp;
525000100122                 endif ;
525100100122                 reade (TBLkut : TBLcod) TABEL;
525200100122             EndDo ;
525300100115
525400100115       // verifico i dati passati
525500100115
525600100115       // se impostato il numero visita aggancio il file direttamente
525700100115         If ita56nrv > 0 ;
525800100115            chain    ita56nrv tivis05l ;
525900100115
526000100115       // se trattativa non trovata vado a fine
526100100115            If not %found(tivis05l) ;
526200100115               $fine = *on ;
526300100115               ota56err = 'E' ;
526400100115               ota56msg = 'Trattativa inesistente' ;
526500100115               leavesr ;
526600100115            endif;
526700100119
526800100119       // se trovata verifico se chiusa
526900100119            If visdch > 0 ;
527000100119               $fine = *on ;
527100100119               ota56err = 'E' ;
527200100119               ota56msg = 'Trattativa Chiusa' ;
527300100119               leavesr ;
527400100119            endif ;
527500100119
527600100119       // se trovata verifico se fittizia
527700100413            If visffz = 'S' ;
527800100119               $fine = *on ;
527900100119               ota56err = 'E' ;
528000100119               ota56msg = 'Trattativa Fittizia';
528100100119               leavesr ;
528200100119            endif ;
528300100119
528400100119       // se senza offerte ancora da esitare
528500100316       //   setll (visnrv) tnofm01l ;
528600100316       //   If not %equal(tnofm01l) ;
528700100316       //      $fine = *on ;
528800100316       //      ota56err = 'E' ;
528900100316       //      ota56msg = 'Trattativa senza Offerte' ;
529000100316       //      leavesr ;
529100100316       //   endif ;
529200100115
529300100115         endif;
529400100119
529500100119       // verifico se utente abilitato alla gestione della trattativa
529600100119         clear tntaa1ds;
529700100119         itaa1nrv =visnrv ;
529800100119         itaa1caut='§UTEGTC';
529900100119         clear kpjbu;
530000100119         kpjbu = tntaa1ds ;
530100100119         Tntaa1c (kpjba) ;
530200100119         tntaa1ds = kpjbu ;
530300100119
530400100119         SELECT ;
530500100119        //   Errore --> chiamare il ced di sede
530600100119         When  otaa1err='S';
530700100119               $fine = *on ;
530800100119               ota56err = 'E' ;
530900100121               ota56msg = $msg(18)  ;
531000100119               leavesr ;
531100100119
531200100119        // Non abilitato
531300100119           when otaa1fabi='N' ;
531400100119               $fine = *on ;
531500100119               ota56err = 'E' ;
531600100119               ota56msg = 'Visita non in gestione all''utente';
531700100119               leavesr ;
531800100119
531900100119           ENDSL;
532000100422
532100100422       // recupero date statistiche
532200100422         clear dsdf ;
532300100422         reset TIBS02ds;
532400100422         T02sif = knsif;
532500100422         T02cod = 'SDF';
532600100422         T02ke1 = '046    ' ;
532700100422         TNTBE_RicercaControllo  (kpjba : tibs02ds);
532800100422
532900100422         if  T02err  = *blank;
533000100422           dSDF = T02uni;
533100100422         endif;
533200130319
533300130319       //?Imposto la libreria per il TITAS
533400130319         IF  %subst(knsif : 7 : 1) = 'P';
533500130319           wLib = 'GAITRAGRPS';
533600130319         ELSE;
533700130319           wLib = 'GAITRAGRU';
533800130319         ENDIF;
533900130319       //?Apro il file
534000130319         wFLib = %trim(wLib) + '/TITAS41C';
534100130319         open  TITAS41C;
534200100422
534300100115       ENDSR;
534400100115
534500100120       //--------------------------------------------------------------
534600100120       //?Routine di controllo PARTITA IVA
534700100120       //--------------------------------------------------------------
534800100120       BEGSR  Sr_ctriva ;
534900100120
535000100120        // controllo il codice fiscale e la partita iva
535100151015         clear xcfiva1ds;
535200100120        // controllo la partita IVA
535300100120         if  Wmod = 'P' ;
535400100120             xcfivamod = wmod ;
535500100121             xcfivapar = c05ivae + c05ivai ;
535600100120         endif ;
535700151015
535800100120        // controllo il codice fiscale
535900151015        //  passando anche la p.iva  - serve per san marino -
536000100120         if  wmod = 'C';
536100100120             clear xcfivamod ;
536200100120             xcfivapar = c05cdf ;
536300151015             xcfivapiva=wiva    ;
536400100120         endif ;
536500151015
536600100120        // dati comuni ai due tipi di controllo
536700100120         xcfivanar = indsta ;
536800100120         xcfivaprv = indprv ;
536900151015         xcfivacap = indcae ;
537000151015         xcfivaloc = indcit ;
537100151015
537200100120        // richiamo il pgm
537300151015         XCFIVAR1 (xcfiva1ds);
537400100120
537500100120         if xcfivaflg < *zeros and wmod = 'C' ;
537600100120            errMessage  = *on;
537700100120            errGenerico = *on;
537800100120            PosCurCdf = *on ;
537900100120            V1dmsg = xcfivamsg;
538000100120            leavesr;
538100100120         endif ;
538200100120        // -> errore forzabile
538300100120         if wmod = 'P' ;
538400100120            if xcfivaflg = -9 and wforzaiva = *off ;
538500100120               errMessage  = *on;
538600100120               errGenerico = *on;
538700100120               PosCurIva = *on ;
538800100120               V1dmsg = xcfivamsg;
538900100120               V1dmsg = %trim(V1dmsg) + ' Enter x forzare';
539000100120               wforzaiva = *on ;
539100100120               leavesr;
539200100120            endif ;
539300100120        // -> errore bloccante
539400100120            if xcfivaflg < *zeros and xcfivaflg <> -9;
539500100120               errMessage  = *on;
539600100120               errGenerico = *on;
539700100120               PosCurIva = *on ;
539800100120               V1dmsg = xcfivamsg;
539900100120               leavesr;
540000100120            endif ;
540100100120         EndIf;
540200100120
540300100120       ENDSR ;
540400100120
540500100120       //--------------------------------------------------------------
540600100120       //?Routine di controllo presenza codice su altro potenziale
540700100120       //?con lo stesso codice fiscale
540800100120       //--------------------------------------------------------------
540900100120       BEGSR  Sr_CdfAltroP ;
541000100120
541100100120             setll Codice tncpo05l ;
541200100120             reade Codice tncpo05l ;
541300100120             dow not %eof(tncpo05l);
541400100120       // se trovato un record "filiale memorizzo ragione sociale del potenz.
541500100120                   savrag=c_cporag ;
541600100120                   reade Codice tncpo05l ;
541700100120             enddo;
541800100120             if not %eof (tncpo05l) ;
541900100120                savrag=c_cporag;
542000100120             endif ;
542100100120
542200100120       ENDSR ;
542300100120
542400100120       //--------------------------------------------------------------
542500100120       //?Routine di controllo presenza codice su altro potenziale
542600100120       //?con la stessa partita IVA
542700100120       //--------------------------------------------------------------
542800100121       BEGSR  Sr_PivAltroP ;
542900100120
543000100120             setll Codice tncpo06l ;
543100100120             reade Codice tncpo06l ;
543200100120             dow not %eof(tncpo06l);
543300100120       // se trovato un record "filiale memorizzo ragione sociale del potenz.
543400100120                   savrag=c_cporag ;
543500100120                   reade Codice tncpo06l ;
543600100120             enddo;
543700100120             if not %eof (tncpo06l) ;
543800100120                savrag=c_cporag;
543900100120             endif ;
544000100120
544100100120       ENDSR ;
544200100120
544300100120       //--------------------------------------------------------------
544400100120       //?Messaggio di errore codice fiscale già presente per altro
544500100120       //?potenziale
544600100120       //--------------------------------------------------------------
544700100120       BEGSR  Sr_Msgcdf ;
544800100120
544900100120        // Errore forzabile se trovato un altro potenziale con lo stesso
545000100120        // codice fiscale
545100100120           if savrag<>*blanks ;
545200100120              cod_forzcdf=codice  ;
545300100120              errMessage  = *on;
545400100120              errGenerico = *on;
545500100120              PosCurCdf = *on ;
545600100120              V1dmsg = $msg(10);
545700100120              leavesr;
545800100120           endif;
545900100120
546000100120       ENDSR ;
546100100120
546200100120       //--------------------------------------------------------------
546300100120       //?Messaggio di errore Partita Iva  già presente per altro
546400100120       //?potenziale
546500100120       //--------------------------------------------------------------
546600100120       BEGSR  Sr_Msgiva ;
546700100120
546800100120        // Errore forzabile se trovato un altro potenziale con lo stesso
546900100120        // codice fiscale
547000100120           if savrag<>*blanks ;
547100100120              cod_forziva=wiva  ;
547200100120              errMessage  = *on;
547300100120              errGenerico = *on;
547400100120              PosCurIva = *on ;
547500100120              V1dmsg = $msg(09);
547600100120              leavesr;
547700100120           endif;
547800100120
547900100120       ENDSR ;
548000100120
548100100120       //--------------------------------------------------------------
548200100120       //?Controllo le condizioni di pagamento
548300100120       //--------------------------------------------------------------
548400100120       BEGSR  Sr_CtrPag ;
548500100120
548600100120            clear dsfa ;
548700100120            Tblkut = 1 ;
548800100120            tblcod = 'FA';
548900100120            tblkey = c05cdp + '1' ;
549000100120            chain (Tblkut:tblcod:tblkey) tabel00f ;
549100100120
549200100120            If %found(tabel00f) ;
549300100120               dsfa = tbluni ;
549400100120            else ;
549500100120              errMessage  = *on;
549600100120              errGenerico = *on;
549700100120              PosCurCdp = *on ;
549800100120              V1dmsg = $msg(11);
549900100120              leavesr;
550000100120            endif ;
550100150416
550200150416       //?Se tipo pagamento gestibile solo da sede errore
550300150417           IF  C05cdp <> savcdp and §fauti = 'S';
550400150416             ErrMessage  = *on;
550500150416             ErrGenerico = *on;
550600150416             PosCurCdp = *on ;
550700150416             V1dmsg = $msg(34);
550800150416             leavesr;
550900150416           ENDIF;
551000100120
551100100121        // verifico i codici ABI e CAB
551200100120            If §fatpg = '1' and c05abi = 0 and c05cab = 0 ;
551300100120               errMessage  = *on;
551400100120               errGenerico = *on;
551500100120               PosCurAbi = *on ;
551600100120               V1dmsg = $msg(12);
551700100120               leavesr;
551800100120            endif ;
551900100120
552000100121        // controllo ABI CAB
552100100120            exsr Sr_ctrabicab ;
552200100120            If errGenerico = *on ;
552300100120               leavesr ;
552400100120            endif ;
552500100120
552600100120       ENDSR ;
552700100120
552800100120       //--------------------------------------------------------------
552900100120       //?Controllo i codici ABI e CAB
553000100120       //--------------------------------------------------------------
553100100120       BEGSR  Sr_CtrAbiCab ;
553200100120
553300100121        // se già controllati non torno più a controllare
553400100121            if c05abi = savabi and c05cab = savcab ;
553500100120               leavesr ;
553600100120            endif ;
553700100120
553800100121        // verifico se entrambi i codici ABI e CAB sono stati inseriti
553900100120            If c05abi = 0 and c05cab <> 0 ;
554000100120               errMessage  = *on;
554100100120               errGenerico = *on;
554200100120               PosCurAbi = *on ;
554300100120               V1dmsg = $msg(13);
554400100120               leavesr;
554500100120            endif ;
554600100120
554700100120            If c05abi <> 0 and c05cab = 0 ;
554800100120               errMessage  = *on;
554900100120               errGenerico = *on;
555000100120               PosCurCab = *on ;
555100100120               V1dmsg = $msg(15);
555200100120               leavesr;
555300100120            endif ;
555400100120         // aggancio anagrafico banche per verifica esistenza codici
555500100120            savabi = c05abi ;
555600100120            savcab = c05cab ;
555700100120
555800100120            chain (c05abi:c05cab) cnabi00f ;
555900100120            If not %found(cnabi00f) or abiann = '*' ;
556000100120                errMessage  = *on;
556100100120                errGenerico = *on;
556200100120                PosCurCab = *on ;
556300100120                V1dmsg = $msg(14);
556400100120                leavesr;
556500100120            endif ;
556600100120
556700100120            If %found(cnabi00f) ;
556800100120               exsr Sr_Crebna ;
556900100120            Endif ;
557000100120
557100100120       ENDSR ;
557200100120
557300100122       //--------------------------------------------------------------
557400100122       //?Stampa servizi ON LINE se offerta
557500100122       //--------------------------------------------------------------
557600100122       BEGSR  Sr_psw  ;
557700100122
557800100122        wpsw = *Off ;
557900100122
558000100122       // Se cliente nuovo
558100100122        If Nuovo_cli = *on ;
558200100122       //   richiamo sempre il pgm x abilitare il cliente
558300100122           wpsw = *on ;
558400100122        else ;
558500100122       //   Se cliente non nuovo
558600100122       //   stampo se non abilitato o se non è figlio di papà abilitato
558700100607        vssksu = '0' + %editc(c05ksc:'X')  ;
558800100122        vssisv = 'TT' ;
558900100122        Setll (vssksu:vssisv) Tivss02l ;
559000100122        If  not %Equal(Tivss02l) ;
559100100122            Clear Tibs10ds ;
559200100122            d10tle = 'WW';
559300100122            d10paf = 'P' ;
559400100122            d10cod = c05ksc ;
559500100608            d10drf = datacor8 ;
559600100122            TIBS10R (tibs10ds);
559700100122            IF  d10err = *blanks and d10cop > 0;
559800100607                vssksu = %subst(%editc(d10cop:'X'):4:8);
559900100122                vssisv = 'TT' ;
560000100122                Setll (vssksu:vssisv) Tivss02l ;
560100100122                If not %Equal(Tivss02l) ;
560200100122                   wpsw = *On ;
560300100122                EndIf ;
560400100122            EndIf ;
560500100122            If  d10err <> *Blanks ;
560600100122                wpsw = *On ;
560700100122            EndIf;
560800100122        EndIf;
560900100122        EndIf;
561000100122
561100100122       If  wpsw = *On ;
561200100122           parric = 'S' ;
561300100122           parksc = %editc(c05ksc:'X');
561400100122           Kpjbu = Parm82 ;
561500100122           Tnta82r (Kpjba);
561600100122       EndIf ;
561700100122
561800100122       ENDSR ;
561900100426
562000100115       //--------------------------------------------------------------
562100100115       //?Reperimento Dati del job (Utente/Operativi).
562200100115       //--------------------------------------------------------------
562300100115       BEGSR DatiJob;
562400100115
562500100115         in(E) §AzUte;
562600100115         if NOT %error;
562700100115           in(E) §DatiUte;
562800100115         endif;
562900100115         if %error or RSut = *blanks;
563000100115           clear TIBS34ds;
563100100115           tibs34r(tibs34ds);
563200100115           in §AzUte;
563300100115           in §DatiUte;
563400100115         endif;
563500100115
563600100115       ENDSR;
563700100108
563800080206       //--------------------------------------------------------------
563900080206       //?Operazioni finali.
564000080206       //--------------------------------------------------------------
564100080206       BEGSR RoutEnd;
564200160803
564300160803       //?Chiudo file storicizzazione variazioni potenziale
564400160803         clear TRMK30DS;
564500160803         clear tncpo_30;
564600160803         clear tncpo1_30;
564700160803         clear ticpi_30;
564800160803         IMK30tla = 'C';
564900160803         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
565000090806
565100080206         *inLR = *on;
565200080206         return;
565300080206
565400080206       ENDSR;
565500080206
565600080206      /end-free
565700080206       //--------------------------------------------------------------
565800080206       //?Schiere a tempo di compilazione.
565900080206       //--------------------------------------------------------------
566000080206
566100080206** - $MSG -------------------------------------------------------------------*
566200090724Utente non abilitato alla gestione del potenziale                              1
566300091215Opzione non valida                                                             2
566400091215Non esistono attività aperte legate alla trattativa                            3
566500100115Data Errata                                                                    4
566600100115Inserire il numero spedizioni presunto affidamento                             5
566700100118Non si può convalidare un'offerta NON ACCETTATA                                6
566800100120Immettere la Partita IVA!                                                      7
566900100120Immettere il codice fiscale!                                                   8
567000100120Partita Iva già presente su altri potenziali. Enter per forzare                9
567100100120Cod.fiscale già presente su altri potenziali. Enter per forzare               10
567200100120Codice di Pagamento non codificato                                            11
567300100120ABI + CAB Obbligatori per pagamento con Ricevuta Bancaria                     12
567400100120Codice ABI Obbligatorio se immesso codice CAB                                 13
567500100120ABI + CAB Inesistenti: Premere ENTER per Forzare                              14
567600100120Codice CAB Obbligatorio se immesso codice ABI                                 15
567700100707Incongruenza data decorrenza con data scadenza preced.                        16
567800100120Offerta xxx scadenza xx/xx/xx ha TariffaPartic."XX"da eliminare entro xx/xx/xx 7
567900100121Errori nella ricerca abilitazioni utente: telefonare al CED di sede!!         18
568000100128Non si possono accettare più versioni della stessa offerta                    19
568100100128Prima di accettare un'offerta annullare o congelare le altre versioni         20
568200110112Per questo tipo trattativa non si accetta l'offerta ma si convalida           21
568300100312Non si possono convalidare più versioni della stessa offerta                  22
568400100312Prima di convalidare un'offerta annullare o congelare le altre versioni       23
568500100323Data errata minore della data odierna                                         24
568600100817Codice commerciale errato                                                     25
568700100817Inserire comm.le con lo stesso unificante del comm.le trattativa              26
568800100707Data Scadenza Incongruente con Data Decorrenza                                27
568900100707Tariffa in modifica da altro utente riprovare più tardi                       28
569000100805Effettuare una sola selezione  !!                                             29
569100100805Effettuare almeno una selezione !!                                            30
569200100914Commerciale errato o non in gestione all'utente                               31
569300110112Per questa trattativa non si possono inserire Info con F15                    32
569400140221ATTENZIONE!!! Prezzo base Fuel mai rilevato settimanalmente dal Ministero.    33
569500150416Codice pagamento utilizzabile SOLO dalla SEDE                                 34
