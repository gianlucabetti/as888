000100080206      //--------------------------------------------------------------
000200090721      //?TNTA88R - GESTIONE TRATTATIVE COMMERCIALI
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600100115     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200090721      // - Archivio Trattative Commerciali
001300090721     fTIVIS05L  uf a e           k disk
001400110112      // - Archivio offerte legate a trattativa
001500110104     fTIVOF11L  If   e           k disk
001600110112      // - Archivio offerte legate a trattativa comprese le 999
001700110112     fTIVOF01L  uf   e           k disk    rename(tivof000:tivof001) prefix(I_)
001800090723      // - Archivio Clienti Potenziali
001900090723     fTNCPO01L  uf a e           k disk
002000090724      // - Archivio Rubrica Clienti/Potenziali/trattative
002100090724     fTFNTC01L  uf a e           k disk
002200090810      // - Anagrafica Clienti
002300090810     fCNACO00F  uf a e           k disk
002400090806      // - Anagrafica Trattative
002500090810     fTFACO00F  uf a e           k disk    rename(cnaco000:tfaco000)
002600110805     f                                     extfile(wlib)
002700110805     f                                     usropn
002800090810      // - Anagrafica Trattative
002900090810     fTFIND00F  uf a e           k disk    rename(cnind000:tfind000)
003000110805     f                                     extfile(wlib)
003100110805     f                                     usropn
003200090810      // - Anagrafica Trattative
003300090810     fTFCLP00F  uf a e           k disk    rename(cnclp000:tfclp000)
003400110805     f                                     extfile(wlib)
003500110805     f                                     usropn
003600100914      // - Anagrafica Trattative
003700100914     fTFCBA01L  uf a e           k disk    rename(fncba000:tfcba000)
003800110805     f                                     extfile(wlib)
003900110805     f                                     usropn
004000100914      // - Anagrafica Clienti
004100100914     fFNCBA01L  if   e           k disk
004200090812      // - Testata Offerte
004300090812     fTNOFM01L  if   e           k disk
004400110805     f                                     extfile(wlib)
004500110805     f                                     usropn
004600090812      // - Anagrafica Trattative
004700091123     fTFCLS01l  uf a e           k disk    rename(fncls000:tfcls000)
004800110805     f                                     extfile(wlib)
004900110805     f                                     usropn
005000080208      // - Organigramma filiale/aziendale
005100080206     fAZORG01L  if   e           k disk
005200130807      // -?Anagrafica Commerciali?
005300130807     fAZCMM01L  if   e           k disk
005400080208      // - Tabelle
005500080206     fTABEL00F  if   e           k disk
005600101223      // - Tabelle
005700101223     fTNTBE01L  if   e           k disk
005800091123      // - Attività
005900100304     fTIATC04L  uf a e           k disk
006000100722      // - Attività
006100100722     fTIATC01L  if   e           k disk    rename(Tiatc000:Tiatc01)
006200100304      // - Attività legami
006300100304     fTIATL01L  uf   e           k disk
006400100304      // - Info trattativa
006500100304     fTIVII01l  uf   e           k disk
006600100304      // - Note
006700100304     fTICPN03l  uf   e           k disk
006800100304     fTICPN04l  uf   e           k disk    rename(Ticpn000:Ticpn4)
006900150430      // -?Campagna
007000150430     fTICMC01L  if   e           k disk
007100150430     fTICMF01L  uf   e           k disk
007200151113      // -?V.L. per ricerca clienti per Partita Iva:?
007300151113      // ·?Anagrafica Indirizzi e Condizioni di Pagamento?
007400151113     fCNIND02L  if   e           k disk    rename( CNIND000 : CNIND002 )
007500151113     f                                     prefix( w_ )
007600080206
007700090721      // - Video Gestione trattative commerciali
007800090721     fTNTA88D   cf   e             workstn
007900090721     f                                     sfile(TA88S01 : S01nrr)
008000090721     f                                     sfile(TA88S02 : S02nrr)
008100080208     f                                     indds(IndDspF)
008200080206     f                                     infds(InfDspF)
008300080206
008400080206      //---------------------------------------------------------------
008500090406      //?Definizione costanti.
008600080206      //---------------------------------------------------------------
008700050519
008800080207      // - Numero di record in ciascuna videata di subfile
008900090804     d c_SflPag1       c                   const(07)
009000090721     d c_SflPag2       c                   const(11)
009100080207
009200080207      // - Tasti funzionali a video
009300080207     d c_F01           c                   const(x'31')
009400080207     d c_F02           c                   const(x'32')
009500080207     d c_F03           c                   const(x'33')
009600090406     d c_F04           c                   const(x'34')
009700080207     d c_F05           c                   const(x'35')
009800080207     d c_F06           c                   const(x'36')
009900080207     d c_F07           c                   const(x'37')
010000080207     d c_F08           c                   const(x'38')
010100080207     d c_F09           c                   const(x'39')
010200080207     d c_F10           c                   const(x'3A')
010300090303     d c_F11           c                   const(x'3B')
010400080207     d c_F12           c                   const(x'3C')
010500080207     d c_F13           c                   const(x'B1')
010600080207     d c_F14           c                   const(x'B2')
010700080207     d c_F15           c                   const(x'B3')
010800080207     d c_F16           c                   const(x'B4')
010900080207     d c_F17           c                   const(x'B5')
011000080207     d c_F18           c                   const(x'B6')
011100080207     d c_F19           c                   const(x'B7')
011200080207     d c_F20           c                   const(x'B8')
011300080207     d c_F21           c                   const(x'B9')
011400080207     d c_F22           c                   const(x'BA')
011500080207     d c_F23           c                   const(x'BB')
011600080207     d c_F24           c                   const(x'BC')
011700080207     d c_Enter         c                   const(x'F1')
011800080207     d c_RollDown      c                   const(x'F4')
011900080207     d c_RollUp        c                   const(x'F5')
012000080214
012100080214      // - Attributi di visualizzazione
012200080215      //  -  DspAtr() - Normale
012300080214     d C_dspatr_Norm   c                   const(x'20')
012400080215      //  -  DspAtr(RI)
012500080214     d C_dspatr_RI     c                   const(x'21')
012600081021     d C_dspatr_HI     c                   const(x'22')
012700080215      //  -  DspAtr(ND)
012800080214     d C_dspatr_ND     c                   const(x'27')
012900080215      //  -  DspAtr(BL) / Color(Red)
013000080214     d C_dspatr_BL     c                   const(x'28')
013100090722
013200090722     d Tit_visua       c                   const('     VISUALIZZAZIONE   ')
013300090722
013400090722     d Tit_aggio       c                   const('      AGGIORNAMENTO    ')
013500090722
013600090722     d Tit_inser       c                   const('       INSERIMENTO     ')
013700100111
013800100111     d Digits          c                   const('0123456789')
013900090722
014000080206      //---------------------------------------------------------------
014100080206      //?Definizione schiere.
014200080206      //---------------------------------------------------------------
014300080206
014400080206      // - Messaggi di errore
014500130807     d $Msg            s             78    dim(41) ctdata perrcd(1)
014600080207      // - Filiali
014700080207     d C_Max_Fil       c                   const(350)
014800090723     d $Fil            s                   like(ORGfil)
014900080208     d                                     dim(C_Max_Fil)  inz
015000080207     d $Car            s                   like(ORGcar)
015100080207     d                                     dim(C_Max_Fil)  inz
015200130807
015300090813      // Schiera tipo Note / relativa flag copia da potenziale a visita
015400090811     d d1t             s              2    dim(50)
015500090811     d dpot            s              1    dim(50)
015600081020      *
015700090813      // Schiera tipo Tariffa e relativa descrizione
015800090813     d ctr             s              1    dim(30)
015900090813     d dtr             s             10    dim(30)
016000101223      *
016100101223      // Schiera tipo Trattativa con info trattativa
016200101223     d infttr          s              1    dim(30)
016300080206
016400080206      //---------------------------------------------------------------
016500080206      //?Definizione aree dati.
016600080206      //---------------------------------------------------------------
016700080206
016800080206      // - Dati utente
016900080206     d §AzUte        e ds                  extname(AZUTE00F)
017000080206     d                                     dtaara
017100080206     d §DatiUte      e ds                  extname(dDatiUte)
017200080206     d                                     dtaara
017300090515     d tiatcDS       e ds                  ExtName(tiatc00f)
017400090812     d tivofDS       e ds                  ExtName(tivof00f)
017500150430     d ticmfDS       e ds                  extname(TICMF00F)
017600080206
017700080206      //---------------------------------------------------------------
017800080206      //?Definizione strutture dati.
017900080206      //---------------------------------------------------------------
018000080206
018100080206      // - Status
018200080206     d Psds           sds
018300080206     d   SDSpgm          *proc
018400080206
018500080206      // - InfDS
018600080206     d InfDspF         ds
018700080207     d  dsp_aid              369    369a
018800080206
018900080206      // - Indicatori su DspF
019000080208     d IndDspF         ds
019100090721        // - Indicatori di gestione del subfile 1
019200090721     d  SflDsp_N1                     1n   overlay(IndDspF : 30)
019300090721     d  SflDspCtl_N1                  1n   overlay(IndDspF : 31)
019400090721     d  SflNxtChg1                    1n   overlay(IndDspF : 32)
019500090721     d  SflEnd1                       1n   overlay(IndDspF : 33)
019600090721        // - Indicatori di gestione del subfile 2
019700090721     d  SflDsp_N2                     1n   overlay(IndDspF : 20)
019800090721     d  SflDspCtl_N2                  1n   overlay(IndDspF : 21)
019900090721     d  SflNxtChg2                    1n   overlay(IndDspF : 22)
020000090721     d  SflEnd2                       1n   overlay(IndDspF : 23)
020100090422        // - Indicatori di errore 1° videata
020200080206     d  errMessage                    1n   overlay(IndDspF : 28)
020300090721     d  PosCurDta                     1n   overlay(IndDspF : 51)
020400090721     d  PosCurCmmi                    1n   overlay(IndDspF : 52)
020500090721     d  PosCurTpv                     1n   overlay(IndDspF : 53)
020600090721     d  PosCurCmm                     1n   overlay(IndDspF : 54)
020700090806     d  PosCurMrt                     1n   overlay(IndDspF : 55)
020800100419     d  PosCurFfz                     1n   overlay(IndDspF : 60)
020900090529        // - Indicatori di errore subfile
021000090721     d  PosCurOpz1                    1n   overlay(IndDspF : 50)
021100090721     d  PosCurOpz2                    1n   overlay(IndDspF : 50)
021200090814        // - Indicatori di errore 2° videata
021300090814     d  PosCurCtr                     1n   overlay(IndDspF : 56)
021400090814     d  PosCurTto                     1n   overlay(IndDspF : 57)
021500090814     d  PosCurCop                     1n   overlay(IndDspF : 58)
021600090422        // - Indicatori di errore generico
021700080206     d  errGenerico                   1n   overlay(IndDspF : 99)
021800090721        // - Indicatori di visualizzazione
021900100819     d  ProtDta                       1n   overlay(IndDspF : 06)
022000090910     d  visualizza                    1n   overlay(IndDspF : 07)
022100091124     d  manutenzio                    1n   overlay(IndDspF : 08)
022200100111     d  ProtCmmi                      1n   overlay(IndDspF : 09)
022300090721     d  convalidato                   1n   overlay(IndDspF : 10)
022400090723     d  offerte                       1n   overlay(IndDspF : 11)
022500100316     d  Note10_tar                    1n   overlay(IndDspF : 12)
022600100205     d  esito                         1n   overlay(IndDspF : 13)
022700110112     d  caldo                         1n   overlay(IndDspF : 14)
022800100316     d  Da_Annullare                  1n   overlay(IndDspF : 15)
022900110112     d  Prot_fittizia                 1n   overlay(IndDspF : 16)
023000100818     d  Protopzs2                     1n   overlay(IndDspF : 17)
023100110112     d  Data_caldo                    1n   overlay(IndDspF : 18)
023200090722     d  Coldafare                     1n   overlay(IndDspF : 19)
023300090805     d  Nuova_potenz                  1n   overlay(IndDspF : 25)
023400090805     d  Nuova_client                  1n   overlay(IndDspF : 26)
023500091022     d  Da_attivita                   1n   overlay(IndDspF : 27)
023600091029     d  Da_potenziale                 1n   overlay(IndDspF : 29)
023700100114     d  New_Attivita                  1n   overlay(IndDspF : 35)
023800100115     d  AttAperte                     1n   overlay(IndDspF : 36)
023900150831     d  F04Attivo                     1n   overlay(IndDspF : 37)
024000100311     d  Convalidata                   1n   overlay(IndDspF : 40)
024100080206
024200090407
024300090407     d WindDspF        ds                  inz
024400090407     d   WindDspFa             1     49    inz(*zero)
024500090407     d   WindDspFb            50     99    inz(*zero)
024600090319
024700090319     d wlbdat          ds                  inz
024800090319     d  g02dat                 1      8  0
024900090319     d  g02inv                 9     16  0
025000090319     d  g02err                17     17
025100090319     d  g02tgi                18     22  0
025200090319
025300090814     d wiva            ds
025400090814     d  euriva                 1      2
025500090814     d  itaiva                 3     16
025600080206
025700090814      // - Parametri TNTA01R2
025800090814     d Ta01r2ds        ds                  inz
025900090814     d Ta01nrv                 2      8  0
026000090814     d ta01prg                 9     11  0
026100090814     d ta01ccc                12     15  0
026200090814     d ta01rgs                16     63
026300090814     d ta01ctr                64     66  0
026400090814     d ta01cli                67     73  0
026500090814     d ta01flg                74     74
026600090814      * ta01all = 'A' ---> rcd allocato
026700090814     d ta01all                95     95
026800090814     d ta01cpo               156    161P 0
026900090814     d* FLAG DI RITORNO
027000090814     d ta01rit               162    162
027100090814     d* PARIST = 'E' ---> SCATTO ISTAT INESISTENTE ERRORE
027200090814     d ta01ist               178    178
027300090917     d* PARTRAT = 'T' ---> Richiamo come trattativa
027400090917     d ta01trat              179    179
027500090814
027600080206      // - Parametri ricevuti
027700080206     d KPJBA         e ds
027800090721     d TNTA88DS      e ds
027900120622
028000120622      // - Ds esterna usata in TNTA01R2 e TNTA02R2 ma non in questo pgm
028100120622      // - devo definirla se non va in errore per via dei prototipi
028200120622     d TNSB48ds      e ds
028300080206
028400080206      // - Reperimento dati utente
028500080206     d TIBS34ds      e ds
028600080206     d dUte01        e ds
028700081020     d TRUL31DS      e ds
028800080206
028900080206      // - Ricerca/Controllo tabelle
029000080206     d TIBS02ds      e ds                  inz
029100080206     d   T02Mod      e                     inz('C')
029200090504
029300090806      // - Ricerca ultimo numero trattativa
029400100413     d trul48ds      e ds                  inz
029500090521
029600100413      // - Ricerca ultimo numero attività
029700100413     d trul33ds      e ds                  inz
029800100413
029900100312      // - controllo abilitazioni
030000100312     d TNTAA1DS      e ds                  inz
030100090812
030200090811      // - Tabella 1T  = Tipo nota
030300090811     d ds1t          e ds                  inz
030400090812
030500090402      // - Tabella CCO = Causale contatto
030600090402     d dcco          e ds                  inz
030700090422
030800090402      // - Tabella LAT = Autorizzazione
030900090402     d dlat          e ds                  inz
031000090722
031100100311      // - Tabella TTR = Tipi trattative
031200100311     d dttr          e ds                  inz
031300090812
031400110211      // - FLO del file ATTIVITA'
031500110211     d datc01        e ds                  inz
031600110211
031700101015      // - FLO del file TRATTATIVA
031800101015     d dvis01        e ds                  inz
031900101015
032000090812      // - DS Anagrafico Potenziali CPORST
032100090812     d dcpo01        e ds                  inz
032200090812     d s_dcpo01      e ds                  extname(dcpo01) prefix(S_)
032300090423
032400090724      // - Ds date statistiche
032500090724     d dSdf          e ds
032600151111
032700151111      // -?Ds Stato del Credito Clienti?
032800151111     d ds4W          e ds                  inz
032900100119
033000100119      // - Controllo se ci sono trattative aperte per lo stesso potenziale
033100100119     d Tnta43ds      e ds                  inz
033200100119      // - Controllo se commerciale diverso da commerciale trattativa
033300100119     d Tnta45ds      e ds                  inz
033400110112
033500110112      // - Gestione Data caldo senza offerte
033600110112     d TNTA63DS      e ds                  inz
033700090724
033800100303      // - Info Trattativa
033900100303     d Tnta41ds      e ds                  inz
034000100303
034100090810      // - Gestione anagrafica clienti
034200090810     d tnta60ds      e ds                  inz
034300090810
034400090806      // - Gestione attività
034500090609     d TRMK19ds      e ds                  inz
034600090317
034700090806      // - Gestione nuove attività
034800090806     d TRMK17ds      e ds                  inz
034900090806
035000090420      // - Interrogazione anagrafica clienti potenziali
035100091104     d TRMK01ds      e ds                  inz
035200080206
035300090629      // -  Gestione Note clienti/potenziali
035400090629     d TRMK20ds      e ds                  inz
035500090629
035600091124      // -  Interrogazione agenda
035700091124     d TRMK82ds      e ds                  inz
035800091124
035900090810      // - Interrogazione info commerciali
036000090810     d TRMK50ds      e ds                  inz
036100100115
036200100115      // -  Controllo se ci sono altre attività aperte
036300100115     d TRMK85ds      e ds                  inz
036400090810
036500090721      // -  Gestione Contatti clienti/potenziali
036600090721     d TNTA12DS      e ds                  inz
036700090722
036800090814      // -  Gestione Copia tariffe offerte
036900090814     d TNTA24DS      e ds                  inz
037000090814
037100090813      // -  Gestione Note tariffe/offerte (10)
037200090813     d TNTA28DS      e ds                  inz
037300090814
037400090814      // -  Controllo partita IVA
037500151021     d XCFIVA1DS     e ds                  inz
037600090813
037700090722       // - Reperimento dati anagrafici
037800090722     d TIBS69ds      e ds
037900090722     d DS_cnaco      e ds                  inz extname(CNACO00F)
038000090722     d DS_cnind      e ds                  inz extname(CNIND00F)
038100090722     d DS_cnclp      e ds                  inz extname(CNCLP00F)
038200090722     d DS_fncls      e ds                  inz extname(FNCLS00F)
038300090814      // - ds d comodo per il controllo dell'iva
038400090814     d DS_cnind_W    e ds                  inz extname(CNIND00F) prefix(W_)
038500090806
038600090806      // - Controllo correttezza mail
038700090806     d dsemail       e ds                  inz
038800160804
038900160804       // -?Storicizzazione variazioni
039000160804     d TRMK30DS      e ds
039100160804     d TNCPO_30      e ds                  extname(TNCPO00F) inz
039200160804     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
039300160804     d TICPI_30      e ds                  extname(TICPI00F) inz
039400090811
039500080206      //---------------------------------------------------------------
039600080206      //?Definizione variabili globali.
039700080206      //---------------------------------------------------------------
039800080206
039900080206      // - Flags booleani
040000080208     d $Fine           s               n   inz(*off)
040100100714     d $Fine3          s               n   inz(*off)
040200100723     d $Fine4          s               n   inz(*off)
040300110104     d $Fine5          s               n   inz(*off)
040400080208     d $InzS01         s               n   inz(*on)
040500090722     d $InzS02         s               n   inz(*on)
040600080206     d $ErrGrave       s               n   inz(*off)
040700080207     d $EoF            s               n   inz(*off)
040800080208     d $RecOK          s               n   inz(*off)
040900090810     d $ModTa60        s               n   inz(*off)
041000090917     d $Datnta12       s               n   inz(*off)
041100090917     d $Scritta        s               n   inz(*off)
041200100304     d $InfoTrat       s               n   inz(*off)
041300100311     d $ForzaComm      s               n   inz(*off)
041400100311     d $Convalidata    s               n   inz(*off)
041500100420     d $Primo_atc      s               n   inz(*off)
041600100714     d $Ric_annull     s               n   inz(*off)
041700100819     d $vengo_da_Gest  s               n   inz(*off)
041800110114     d $Pres_offerte   s               n   inz(*off)
041900110314     d $W05            s               n   inz(*off)
042000110414     d $F15_on         s               n   inz(*off)
042100110617     d $GES_chiusa     s               n   inz(*off)
042200151113     d $CtrlContenz    s               n   inz
042300151111     d $Contenzioso    s               n   inz
042400120622     d wDataPrimoCon   s               n   inz(*off)
042500121211     d Info_Obb        s               n   inz(*off)
042600150430     d wEoF_CMF        s               n   inz(*off)
042700150430     d wDelTRffz       s               n   inz(*off)
042800080206
042900080206      // - Indici di schiera
043000080208     d xx              s              3  0 inz
043100080206
043200080207      // - Campi associati al video
043300090722     d $Video          s              2    inz('S1')
043400080208     d S01nrr          s              4  0 inz
043500090722     d S02nrr          s              4  0 inz
043600090722     d W_SflPag1       s              3  0 inz
043700090722     d W_SflPag2       s              3  0 inz
043800080206
043900090401      // - Campi di comodo data
044000090401     d  data_eur       s               d   Datfmt(*eur)
044100090401     d  data_iso       s               d   Datfmt(*iso)
044200090401     d  data_dmy       s               d   Datfmt(*dmy)
044300090811     d  data_ymd       s               d   datfmt(*ymd)
044400090401
044500080206      // - Campi di comodo
044600090406     d wAbi            s                   like(UTEaut)  inz
044700090714     d  datacor        s              8  0
044800090814     d ForzaIta        s              1
044900110805     d wLib            s             21    inz
045000090814     d W_scfsta        s                   like(indsta)
045100091022     d Newnrv          s                   like(visnrv)
045200091103     d Key_ntcapl      s                   like(ntcapl)
045300091103     d Key_ntcnk1      s                   like(ntcnk1)
045400100304     d Sav_ifotot      s                   like(visinfot)
045500100311     d sav_c01cmm      s                   like(c01cmm)
045600151113     d wSaveCON        s                   like(CLPcon)   inz
045700111129     d wOggi           s              8s 0
045800141111
045900090722       // - Stringa SQL da eseguire
046000090722     d stringasql      s           2048    Varying        inz
046100090722
046200090724      // campi di comodo date
046300090813     d w01dta          s              8  0
046400090813     d dta_ult_var     s                   like(tamduv)
046500090724
046600090722      // - Campi di comodo decodifica commerciale
046700090722     D  comcmm         s                   like(c01cmm)
046800130807     D  comcmmd        s                   like(CMMdes)
046900080208
047000090508      //---------------------------------------------------------------
047100090508      //?Definizione procedure usate.
047200090508      //---------------------------------------------------------------
047300100119
047400100119     D Tnta43R         pr                  extpgm('TNTA43R')
047500100119     D  tnta43ds                           likeds(tnta43ds)
047600100119
047700100119     d TNTA45R         pr                  extpgm('TNTA45R')
047800100119     d  tnta45ds                           likeds(tnta45ds)
047900110112
048000110112      // - Gestione data caldo
048100110112     d Tnta63r         pr                  extpgm('TNTA63R')
048200110112     d  kpjba                              likeds(kpjba)
048300110112     d  tnta63ds                           likeds(tnta63ds)
048400100412
048500100412     d TRMK02C         pr                  extpgm('TRMK02C')
048600100412     d  kpjba                              likeds(kpjba)
048700100412     d  trmk01ds                           likeds(trmk01ds)
048800100125
048900091124     D TRMK82R         pr                  extpgm('TRMK82R')
049000091124     D  kpjba                              likeds(kpjba)
049100100115
049200100115     d TRMK85R         pr                  extpgm('TRMK85R')
049300100115     d  trmk85ds                           likeds(trmk85ds)
049400100303
049500100303     d TNTA41R         pr                  extpgm('TNTA41R')
049600100303     d  kpjba                              likeds(kpjba)
049700100303     d  tnta41ds                           likeds(tnta41ds)
049800100413
049900100413     d TRUL48R         pr                  extpgm('TRUL48R')
050000100413     d  kpjba                              likeds(kpjba)
050100100413     d  trul48ds                           likeds(trul48ds)
050200150806
050300150806       // -?Interrogazione Campagne per cliente
050400150806     d TRKC09DS      e ds                  inz
050500150806     d TRKC09R         pr                  extpgm('TRKC09R')
050600150806     d   kpjba                             likeds(KPJBA)
050700150806     d   trkc09ds                          likeds(TRKC09DS)
050800150430
050900150430       // -?Scrittura note su Campagna
051000150430     d TRKC10DS      e ds                  inz
051100150430     d TRKC10R         pr                  extpgm('TRKC10R')
051200150430     d   kpjba                             likeds(KPJBA)
051300150504     d   trkc10ds                          likeds(TRKC10DS)
051400160804
051500160804       // -?Storicizzazione Variazioni Potenziale
051600160804     d TRMK30R         pr                  extpgm('TRMK30R')
051700160804     d  trmk30ds                           likeds(trmk30ds)
051800160804     d  tncpo_30                           likeds(tncpo_30)
051900160804     d  tncpo1_30                          likeds(tncpo1_30)
052000160804     d  ticpi_30                           likeds(ticpi_30)
052100141104
052200091124      //---------------------------------------------------------------
052300091124      //?prototipi
052400091124      //---------------------------------------------------------------
052500091124
052600080626      /copy gaitrasrc/srcprotopr,tibs02r
052700080626      /copy gaitrasrc/srcprotopr,tibs34r
052800090722      /copy gaitrasrc/srcprotopr,tibs69r
052900100312      /copy gaitrasrc/srcprotopr,tntaa1c
053000090814      /copy gaitrasrc/srcprotopr,tnta01r2
053100090813      /copy gaitrasrc/srcprotopr,tnta12r
053200090814      /copy gaitrasrc/srcprotopr,tnta24r
053300090813      /copy gaitrasrc/srcprotopr,tnta28r
053400090810      /copy gaitrasrc/srcprotopr,tnta60r
053500091022      /copy gaitrasrc/srcprotopr,trmk17c
053600100126      /copy gaitrasrc/srcprotopr,trmk19r
053700090629      /copy gaitrasrc/srcprotopr,trmk20r
053800130807      /copy gaitrasrc/srcprotoPI,TRMK43R_1
053900130807      /copy gaitrasrc/srcprotoPR,TRMK43R
054000090810      /copy gaitrasrc/srcprotopr,trmk50r
054100081020      /copy gaitrasrc/srcprotopr,trul31r
054200100413      /copy gaitrasrc/srcprotopr,trul33r
054300151021      /copy gaitrasrc/srcprotopr,xcfivar1
054400090406      /copy gaitrasrc/srcprotopr,xsrda8
054500090806      /copy gaitrasrc/srcprotopr,xemail
054600090508
054700080206      //---------------------------------------------------------------
054800080206      //?Definizione key-list.
054900080206      //---------------------------------------------------------------
055000080206
055100080206      //---------------------------------------------------------------
055200080206      //?Riepilogo indicatori.
055300080206      //---------------------------------------------------------------
055400090421      // - visualizzazioni particolari
055500090910      // 07    : Visualizzazione
055600090722      // 10    : Convalidato
055700100818      // 17    : ozpione sbfile offerte protetto
055800090910      // 25    : Visita nuova da Potenziale
055900090910      // 26    : Visita nuova da Cliente
056000100909      // 16    : Campo fittizia protetto
056100090518      // - Comuni
056200080207      // 28    : Emissione messaggio di errore a video
056300080207      // - C01/S01
056400080207      // 30    : Condiziona SFLDSP    (*not)
056500080207      // 31    : Condiziona SFLDSPCTL (*not)
056600080207      // 30+31 : Condiziona SFLCLR
056700080207      // 32    : Condiziona SFLNXTCHG in S01
056800080207      // 33    : Condiziona SFLEND
056900090518      // 50    : Errore: Posizione cursore
057000090722      // - C02/S02
057100090722      // 20    : Condiziona SFLDSP    (*not)
057200090722      // 21    : Condiziona SFLDSPCTL (*not)
057300090722      // 20+21 : Condiziona SFLCLR
057400090722      // 22    : Condiziona SFLNXTCHG in S02
057500090722      // 23    : Condiziona SFLEND
057600090722      // 50    : Errore: Posizione cursore
057700080207      // - D01
057800090722      // 51    : Errore: Data apertura errata
057900090722      // 52    : Errore: Commerciale interno errato
058000090722      // 53    : Errore: Tipo trattativa errata
058100090722      // 54    : Errore: Commerciale esterno errato
058200100419      // 60    : Errore: Tipo fittizia
058300080207      // - Comuni
058400080207      // 99    : Generico di Errore
058500080206      //---------------------------------------------------------------
058600080206
058700080206      //---------------------------------------------------------------
058800080206      //?M A I N - L I N E
058900080206      //---------------------------------------------------------------
059000080206
059100080206     c     *Entry        plist
059200080206     c                   parm                    KPJBA
059300090722     c                   parm                    tnta88ds
059400080206
059500080206      /free
059600080206
059700080206       // Operazioni iniziali
059800080206       exsr RoutInz;
059900090601
060000080206       // Gestione video
060100080206       DOW $Fine = *off;
060200080206         select;
060300080206           when $Video = 'S1';
060400080206             exsr GesS01;
060500090722           when $Video = 'S2';
060600090812             exsr GesS02;
060700090428           other;
060800080206             $Fine = *on;
060900080206         endsl;
061000080206       ENDDO;
061100080206
061200080206       // Operazioni finali
061300080206       exsr RoutEnd;
061400080206
061500080206       //--------------------------------------------------------------
061600080206       //?Operazioni iniziali.
061700080206       //--------------------------------------------------------------
061800080206       BEGSR RoutInz;
061900080206
062000080206         // Impostazione campi "fissi"
062100080208         T01pgm = SDSpgm;
062200080208         TBLkut = 1;
062300080206
062400080206         // Reperimento dati job
062500080206         exsr DatiJob;
062600150114
062700150114         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
062800111129
062900111129         //?Data del giorno
063000111129         wOggi = %dec(%date());
063100080206
063200110805         //?Se S.I. di prova imposto FILTRAGRPF
063300110805         IF  %subst(knsif:7:1) = 'P';
063400110805           wLib = 'FILTRAGRPF/';
063500110805         //?Apro i file
063600110805           %subst(wLib:12:8) = 'TFCLS01L';
063700110805           open TFCLS01L ;
063800110805           %subst(wLib:12:8) = 'TNOFM01L';
063900110805           open TNOFM01L ;
064000110805           %subst(wLib:12:8) = 'TFCBA01L';
064100110805           open TFCBA01L ;
064200110805           %subst(wLib:12:8) = 'TFCLP00F';
064300110805           open TFCLP00F ;
064400110805           %subst(wLib:12:8) = 'TFIND00F';
064500110805           open TFIND00F ;
064600110805           %subst(wLib:12:8) = 'TFACO00F';
064700110805           open TFACO00F ;
064800110805         ENDIF;
064900110805         //?Se S.I. di produzione imposto FILTRAGRU
065000110805         IF  %subst(knsif:7:1) <> 'P';
065100110805           wLib = 'FILTRAGRU/';
065200110805         //?Apro i file
065300110927           %subst(wLib:12:8) = 'TFCLS01L';
065400110927           open TFCLS01L ;
065500110805           %subst(wLib:12:8) = 'TNOFM01L';
065600110805           open TNOFM01L ;
065700110805           %subst(wLib:12:8) = 'TFCBA01L';
065800110805           open TFCBA01L ;
065900110805           %subst(wLib:12:8) = 'TFCLP00F';
066000110805           open TFCLP00F ;
066100110805           %subst(wLib:12:8) = 'TFIND00F';
066200110805           open TFIND00F ;
066300110805           %subst(wLib:12:8) = 'TFACO00F';
066400110805           open TFACO00F ;
066500110805         ENDIF;
066600110805
066700080206         // Verifica errori e autorità profilo
066800080206         clear  wAbi;
066900080206         clear  dLAT;
067000080206         select;
067100080206         // - Se ho errori nei dati utente esco dal pgm
067200080208           when  DUTerr = 'E';
067300080206             $Fine = *on;
067400080206         // - Se non c'è l'abilitazione:
067500080206         //   - se 1° livello, abilitazioni al terminal
067600080206         //   - se 2° livello, abilitazioni al punto operativo
067700080206         //   - se sede è impossibile ma se capita mando a fine il pgm
067800080208           when  UTEaut = *blank;
067900080206             select;
068000080208               when  DUTlpo = '1';
068100080206                 wAbi  = 'TP';
068200080208               when  DUTlpo = '2';
068300080206                 wAbi  = 'PO';
068400080208               when  DUTlpo = 'S';
068500080206                 $Fine = *on;
068600080206             endsl;
068700080206         // - Altrimenti si caricano le abilitazioni del profilo
068800080206           other;
068900080208             dUTE01 = UTEfaf;
069000080208             if  §UTEpot <> *blank;
069100080208               wAbi = §UTEpot;
069200080206             else;
069300080208               wAbi = UTEaut;
069400080206             endif;
069500080206         ENDSL;
069600080206
069700080206         // Controllo se ok l'abilitazione dell'utente
069800080206         reset TIBS02ds;
069900080208         T02sif = knsif;
070000080208         T02cod = 'LAT';
070100080208         T02ke1 = wAbi;
070200080626         TNTBE_RicercaControllo  (kpjba : tibs02ds);
070300080206
070400080208         if  T02err  = *blank;
070500080208           dLAT = T02uni;
070600080206         endif;
070700080206
070800080206         // Errore o utente non abilitato
070900080208         if  T02err <> *blanks   or
071000080208             §LATabi = 'S';
071100080206           $ErrGrave   = *on;
071200080206           errMessage  = *on;
071300080206           errGenerico = *on;
071400080206           V1Dmsg = $Msg(01);
071500080206           leavesr;
071600080206         endif;
071700090724
071800081020         clear trul31ds  ;
071900081020         i31abi=wabi   ;
072000081020         I31cdi = DUTdis   ;
072100081020         I31car = DUTare   ;
072200081020         I31cpo = DUTpou   ;
072300081020         callp TRUL31R (kpjba:trul31ds)   ;
072400090724
072500081020         if o31pog<=*zeros  ;
072600081020           $ErrGrave   = *on;
072700081020           errMessage  = *on;
072800081020           errGenerico = *on;
072900081020           V1Dmsg = $Msg(01);
073000081020           leavesr;
073100081020         endif;
073200081020
073300080207
073400080207         // Intabellamento filiali e relative aree
073500080207         setll *loval AZORG;
073600080207         read  AZORG;
073700080208         dow  NOT  %eof(AZORG01L);
073800080208           if  ORGfva = *blank;
073900080207             xx += 1;
074000080208             $Fil(xx) = ORGfil;
074100080208             $Car(xx) = ORGcar;
074200080207           endif;
074300080207           read  AZORG;
074400080207         enddo;
074500080206
074600090722       // Reperisco data corrente
074700090714             data_iso = (%date());
074800090714             datacor = %dec(data_iso);
074900090812
075000090811        //  Carico schiera con tabella 1T (tipi nota)
075100090811             Clear   xx;
075200100311             clear  ds1t;
075300090811             TBLcod = '1T';
075400090811             setll (TBLkut : TBLcod) TABEL;
075500090811             reade (TBLkut : TBLcod) TABEL;
075600090811             dow not %eof(TABEL00F);
075700090811                 If tblflg = ' ' and tblkey <> *blanks ;
075800090811                    xx = xx + 1 ;
075900090811                    d1t(xx) = tblkey ;
076000090811                    ds1t    = TBLuni;
076100090811                    dpot(xx)= §1tPot;
076200090811                 endif ;
076300090811                 reade (TBLkut : TBLcod) TABEL;
076400090811             EndDo ;
076500090714
076600090813        //  Carico schiera con tabella TR (tipo tariffa))
076700090813             Clear   xx;
076800090813             TBLcod = 'TR';
076900090813             setll (TBLkut : TBLcod) TABEL;
077000090813             reade (TBLkut : TBLcod) TABEL;
077100090813             dow not %eof(TABEL00F);
077200090813                 If tblflg = ' ' and tblkey <> *blanks ;
077300090813                    xx = xx + 1 ;
077400090813                    ctr(xx) = tblkey ;
077500090813                    dtr(xx) = tbluni;
077600090813                 endif ;
077700090813                 reade (TBLkut : TBLcod) TABEL;
077800090813             EndDo ;
077900101223
078000101223        //  Carico schiera con tipo trattativa con info
078100101223             Clear   xx;
078200101223             setll ('TTR') tntbe01l   ;
078300101223             READE ('TTR') tntbe01l   ;
078400101223
078500101223 2           dow not %eof(tntbe01l)   ;
078600101223 3             if tbeatb=' '    ;
078700101223                  dttr=tbeuni    ;
078800101223                  If §ttrinfo = 'S' ;
078900101223                     xx=xx+1        ;
079000101223                     infttr(xx)=tbeke1 ;
079100101223                  endif ;
079200101223 3             endif ;
079300101223
079400101223               READE ('TTR') tntbe01l   ;
079500101223 2           enddo    ;
079600101223
079700101223             TBLcod = 'TR';
079800101223             setll (TBLkut : TBLcod) TABEL;
079900101223             reade (TBLkut : TBLcod) TABEL;
080000101223             dow not %eof(TABEL00F);
080100101223                 If tblflg = ' ' and tblkey <> *blanks ;
080200101223                    xx = xx + 1 ;
080300101223                    ctr(xx) = tblkey ;
080400101223                    dtr(xx) = tbluni;
080500101223                 endif ;
080600101223                 reade (TBLkut : TBLcod) TABEL;
080700101223             EndDo ;
080800090813
080900090722       // verifico i dati passati
081000090722
081100090722       // se impostato il numero visita aggancio il file direttamente
081200091113         If ita88nrv > 0 and (ita88fle <> 'P' and ita88fle <> 'C');
081300100315            chain(n) ita88nrv tivis05l ;
081400090722
081500090722       // se trattativa non trovata vado a fine
081600090722            If not %found(tivis05l) ;
081700090722               $fine = *on ;
081800090722               ota88err = 'E' ;
081900090722               ota88msg = 'Trattativa inesistente' ;
082000090722               leavesr ;
082100091124            else ;
082200110926               If dutpou = 046 ;
082300110926                  visualizza = *on ;
082400110926               else ;
082500110926                  manutenzio = *on ;
082600110926               endif ;
082700110617               dvis01 = visflo ;
082800110617        // verifico se trattativa chiusa e sono entrata in gestione lo segnalo
082900110617        // per non dare la possibilità di entrare in gestione delle INFO
083000110617        // Trattativa
083100110617               If  Visdch > 0 and ITA88fle = 'G' ;
083200110617                   $Ges_chiusa = *on ;
083300110617               Endif ;
083400090722            endif;
083500090722
083600090722         endif;
083700090724
083800090724       // recupero date statistiche
083900090724         clear dsdf ;
084000090724         reset TIBS02ds;
084100090724         T02sif = knsif;
084200090724         T02cod = 'SDF';
084300100421         T02ke1 = '046    ' ;
084400090724         TNTBE_RicercaControllo  (kpjba : tibs02ds);
084500090724
084600090724         if  T02err  = *blank;
084700090724           dSDF = T02uni;
084800090724         endif;
084900090724
085000091022       // Recupero numero trattativa
085100091112         if  Ita88nrv = 0 and (ita88fle = 'P' or ita88fle = 'C') ;
085200091022            exsr Sr_Recnum ;
085300091022            if  $ErrGrave = *on;
085400091022                $fine = *on ;
085500091022                leavesr;
085600091022            endif;
085700091022         endif;
085800080206       ENDSR;
085900080206
086000080206       //--------------------------------------------------------------
086100080206       //?Reperimento Dati del job (Utente/Operativi).
086200080206       //--------------------------------------------------------------
086300080206       BEGSR DatiJob;
086400080206
086500080206         in(E) §AzUte;
086600080206         if NOT %error;
086700080206           in(E) §DatiUte;
086800080206         endif;
086900080206         if %error or RSut = *blanks;
087000080206           clear TIBS34ds;
087100080206           tibs34r(tibs34ds);
087200080206           in §AzUte;
087300080206           in §DatiUte;
087400080206         endif;
087500080206
087600080206       ENDSR;
087700080206
087800090722       //--------------------------------------------------------------
087900090722       //?Gestione videata S01
088000090722       //--------------------------------------------------------------
088100090722       BEGSR GesS01;
088200090722
088300090722         // Inizializzazione videata
088400090722         if  $InzS01 = *on;
088500090722            exsr InzS01;
088600090722            $InzS01  = *off;
088700090722         endif;
088800090722
088900090722         // Posizionamento cursore
089000090722         if  C01csr  > *zero;
089100090722           C01rcd = C01csr;
089200090722         endif;
089300090722
089400090722         // Emissione Testata e Piede con tasti funzionali abilitati
089500090722           write  TA88T01;
089600090722           write  TA88P01;
089700090722
089800100714          // se si tratta di trattativa chiusa ed ho ancora delle attività
089900100714          // aperte emetto una fimnestra di richiesta annullamento attività
090000100714         IF  convalidato and ita88fpr <> 'A' and $ric_annull = *off;
090100100714          atccpo = ch1cpo ;
090200100714          atcksc = c01ksc ;
090300100714          atcnrv = c01nrv ;
090400100714          atcdco = 0 ;
090500100714          setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
090600100714          If  %equal ;
090700100714              write TA88C01 ;
090800100714              exsr gesw03 ;
090900100714              $ric_annull = *on ;
091000100714              leavesr;
091100100714          endif ;
091200100714         endif ;
091300151111
091400151113         // -?Se si stratta di una trattativa su un cliente o un?
091500151113         //  ?potenziale con stessa Partiva IVA di uno in CONTENZIOSO?
091600151113         //  ?si emette una finestra con F6 x FORZARE?
091700151111         If  $Contenzioso;
091800151111           write  TA88C01;
091900151111           exsr  sr_GesW06;
092000151111           clear  $Contenzioso;
092100151111           //leavesr;
092200151111         EndIf ;
092300100714
092400090722
092500090722         // Emissione videata
092600090722         exfmt  TA88C01;
092700090722         reset errMessage;
092800090722         reset errGenerico;
092900090722         clear V1Dmsg;
093000090722         SELECT;
093100090804
093200100311         // - F10=Inserisce nuove attività solo se non sono in inserimento
093300100315           WHEN  dsp_aid = c_F10 ;
093400100111             exsr CtrC01 ;
093500100111             if  errGenerico = *on;
093600100111               leavesr;
093700100111             endif;
093800090804             exsr F10S01;
093900090722
094000091124         // - F11=Agenda
094100091124           WHEN  dsp_aid = c_F11;
094200091124             exsr F11S01 ;
094300100111
094400100111         // - F03=Fine
094500100111           WHEN  dsp_aid = c_F03;
094600110905           If DutPou <> 046 ;
094700100518           If c01ffz <> visffz ;
094800100518              exsr ctrc01 ;
094900100518             if  errGenerico = *on;
095000100518               leavesr;
095100100518             else ;
095200100518             chain c01nrv tivis05l ;
095300100518             If %found(tivis05l) ;
095400100518                 visffz = c01ffz ;
095500100518                 update tivis000 ;
095600100518             endif ;
095700100518             endif;
095800150211           //?Se cliente in campagna e trattativa ora è fittizzia devo cancellar
095900150211           //?già scritte per questa trattativa
096000150211             IF  VISffz = 'S';
096100150430               wDelTRffz = *on;
096200150430               exsr FaseCampagna;
096300150211             ENDIF;
096400100518           endif;
096500100111             exsr F03S01;
096600110905           Else ;
096700110905         // Ritorno al programma chiamante
096800110905             $fine  = *on ;
096900110905           Endif;
097000091124
097100090722         // - F12=Ritorno
097200090722           WHEN  dsp_aid = c_F12;
097300090722             exsr F12S01;
097400091123             if  errGenerico = *on;
097500091123               leavesr;
097600091123             endif;
097700150126
097800150126           // -?F4=Campagna?
097900150126           WHEN  dsp_aid = c_F04;
098000150126             exsr  F04S01;
098100110112
098200110112         // - F13=Gestione Data caldo
098300110112           WHEN  dsp_aid = c_F13;
098400110112             exsr F13S01 ;
098500090722
098600090722         // - F21=Potenziale
098700090722           WHEN  dsp_aid = c_F21;
098800090810             exsr F21S01 ;
098900090722
099000100311         // - F14=Info Commerciale
099100100311           WHEN  dsp_aid = c_F14;
099200100311             exsr F14S01 ;
099300100311
099400110805         // - F15=Info Trattativa  non da sede
099500110805           WHEN  dsp_aid = c_F15 and dutpou <> 046 ;
099600110414             $f15_on = *on ;
099700110111             exsr CtrC01 ;
099800110111             if  errGenerico = *on;
099900110111               leavesr;
100000110111             endif;
100100101223             If  %lookup(c01tpv:infttr) = 0 ;
100200101223                 errMessage  = *on;
100300101223                 errGenerico = *on;
100400101223                 V1Dmsg = $Msg(40);
100500101223                 leavesr;
100600101223             endif;
100700100303             exsr F15S01 ;
100800110414             $f15_on = *off ;
100900090722
101000110805         // - F15=Info Trattativa   da sede
101100110805           WHEN  dsp_aid = c_F15 and dutpou = 046 ;
101200110805             exsr F15S01 ;
101300110805
101400100303         // - F18=Note
101500100303           WHEN  dsp_aid = c_F18;
101600100303             exsr F18S01 ;
101700100303
101800090722         // - F02=Rubrica
101900090722           WHEN  dsp_aid = c_F02;
102000110906            If dutpou <> 046;
102100100304             exsr CtrC01 ;
102200100304             if  errGenerico = *on;
102300100304               leavesr;
102400100304             endif;
102500110906            endif;
102600090810             exsr F02S01 ;
102700090722
102800090722         // - Roll-Up
102900090722           WHEN  dsp_aid = c_RollUp;
103000090722
103100090722         // Invio
103200090722           OTHER;
103300090813           If ita88fle <> '5' ;
103400110111         // verifico i campi immessi  se non sono in visualizzazione
103500090722             exsr CtrC01 ;
103600090722             if  errGenerico = *on;
103700090722               leavesr;
103800090722             endif;
103900090813           endif ;
104000091014         // - F06=Conferma oppure sono in manutenzione e ho dato enter
104100110314             If dsp_aid = c_F06 or (ita88fle = 'G' and dsp_aid = c_Enter) ;
104200090811                exsr F06S01;
104300110104                if  errGenerico = *on;
104400110104                    leavesr;
104500110104                endif;
104600090811             endif;
104700090813
104800090916         // - F07=Anagrafica
104900090916             If  dsp_aid = c_F07;
105000090916                 exsr F07S01 ;
105100091111                 leavesr ;
105200090916             endif;
105300100111
105400100111         // leggo il subfile solo se ho dei records
105500100111             If s01nrr > *zeros ;
105600100111                exsr OpzS01;
105700100111                if  errGenerico = *on or $inzs01= *on;
105800100111                    leavesr;
105900100111                endif;
106000100111             endif;
106100090916
106200090813       // passo alla videata successiva se dato f6 oppure se sono in
106300100311       // visualizzazione o in mnautenzione e il tipo trattativa prevede
106400100311       // offerte
106500110314       // solo però se non è stato fatto un cambio tipo trattativa
106600100311             if §ttroff = 'S' and
106700110314                not $w05 and
106800100311               ( dsp_aid = c_F06 or ita88fle = '5'or ita88fle = 'G');
106900090813                $video = 'S2' ;
107000090813                $inzs02 = *on ;
107100090813             endif ;
107200100504       // no offerte
107300100504             if §ttroff <> 'S' and dsp_aid = c_F06 ;
107400100504               $video = 'S1';
107500100504               $inzs01 = *on ;
107600100504             endif ;
107700100120
107800100120       //?Se F6 e provengo da attività controllo se ho inserito delle
107900100120       //?attività, nel caso abilito già F10=Attività
108000100120         IF  dsp_aid = c_F06 and ita88fle = 'G' and ita88fpr = 'A';
108100100120           atccpo = ch1cpo ;
108200100120           atcksc = c01ksc ;
108300100120           atcnrv = c01nrv ;
108400100120           atcdco = 0 ;
108500100120           setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
108600100120           IF  not %equal ;
108700100120             New_Attivita = *on;
108800100120           ENDIF;
108900100120         ENDIF;
109000090811
109100090722         ENDSL;
109200090722
109300090722       ENDSR;
109400090722
109500090722       //--------------------------------------------------------------
109600090722       //?Inizializzazione videata S01
109700090722       //--------------------------------------------------------------
109800090722       BEGSR InzS01;
109900090722
110000090722       // Pulizia subfile
110100090722         exsr pulsfl1;
110200090722
110300100311         clear sav_c01cmm ;
110400100316         Da_annullare = *off ;
110500100311         $forzacomm = *off ;
110600100304            ProtDta  = *on ;
110700151111         $Contenzioso = *off;
110800151113         clear  wSaveCON;
110900091022
111000091022       // verifico se provengo da attività non abilito F10
111100091022         Da_attivita = (Ita88fpr = 'A') ;
111200091029       // verifico se provengo da potenziale non abilito F10/F21
111300091029         Da_potenziale = (Ita88fpr = 'P') ;
111400100419       // verifico se provengo da attività proteggo il fittizio
111500100419         Prot_Fittizia = (Ita88fpr = 'A') ;
111600091022
111700090722       // Aggiornamento
111800090722         if ita88fle = 'G' ;
111900090722       // trattativa convalidata visualizzazione
112000090722               if visdch > 0 ;
112100090722                  v01tit = Tit_visua ;
112200090723                  convalidato = *on ;
112300100115                  visualizza = *on ;
112400100201                  ITA88fle = '5';
112500100819                  $vengo_da_gest = *on ;
112600090722               else ;
112700090722       // trattativa non convalidata aggiornamento
112800090722                  v01tit = Tit_aggio ;
112900090723                  convalidato = *off ;
113000090722               endif ;
113100150831
113200150831           //?Attivo F4 se trattativa su cliente
113300150831           F04Attivo = (VISksc <> *zero );
113400141111
113500090722         endif;
113600090722
113700090722       // Visualizzazione
113800090722         if ita88fle = '5' ;
113900090722            v01tit = Tit_visua ;
114000090910            visualizza = *on ;
114100100316       // se sono in visualizzazione e trattativa chiusa e attività ancora
114200100316       // aperte sulla trattativa do la possibilità di annullarle
114300100316            If visdch > 0 ;
114400110617               atccpo = viscpo ;
114500110617               atcksc = visksc ;
114600110617               atcnrv = visnrv ;
114700100316               atcdco = 0 ;
114800100316               setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
114900100316               IF  %equal ;
115000100316                   Da_annullare = *on;
115100100316               Endif;
115200100316            endif ;
115300141111
115400150831           //?Attivo F4 se trattativa su cliente
115500150831           F04Attivo = (VISksc <> *zero );
115600141111
115700090722         endif ;
115800090722
115900090722       // Immissione
116000090722         if ita88fle = 'C'or ita88fle = 'P' ;
116100090722            v01tit = Tit_inser ;
116200091112        // valorizzo il numero trattativa se passato da ds oppure no
116300091112            if ita88nrv > 0 ;
116400091112               c01nrv = ita88nrv ;
116500091112            else ;
116600091112               c01nrv = newnrv ;
116700091112            endif ;
116800091022       // Propongo tipo trattativa nuovo se "P"
116900100503       // Imposto ragione sociale del nuovo cliente / potenziale
117000151117       // -?Reperisco anche la P.Iva da CNIND?
117100100503            if ita88fle = 'C' ;
117200100503               clear  tibs69ds;
117300100503               I69kac = Ita88ksc;
117400151117               I69kin = Ita88ksc;
117500100503               I69kcp = Ita88ksc;
117600100503               TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
117700100503               c01rag = ACOrag;
117800100503               c01ksc = Ita88ksc ;
117900100503               ch1cpo = acolib  ;
118000100503               c01cmm = %editc(clpage:'X')  ;
118100100503               nuova_client = *on  ;
118200100503               nuova_potenz = *off ;
118300100503            else ;
118400100503               chain(n) ita88cpo tncpo01l ;
118500100503               if %found(tncpo01l) ;
118600100503                  c01rag = cporag ;
118700100503                  clear c01ksc ;
118800100503                  ch1cpo = cpocpo ;
118900100503                  c01cmm = %editc(cpocmm:'X') ;
119000100503               else ;
119100100503                  c01rag = '??????????????' ;
119200100503               endif ;
119300100503               nuova_client = *off ;
119400100503               nuova_potenz = *on  ;
119500100503            endif ;
119600091103       // Recupero le note del potenziale
119700091022            If ita88fle = 'P' ;
119800091022               c01tpv = 'N' ;
119900091109
120000091109
120100091103               exsr Sr_noteda ;
120200091022            endif ;
120300090722       // Imposto la data corrente in data apertura
120400090724            c01dta = *date   ;
120500090722
120600090722        // se passato il commerciale interno lo valorizzo a video
120700100111            ProtCmmi = *off;
120800100114            PosCurCmmi = *off;
120900090722            If ita88cmmi > 0  ;
121000090723               c01cmmi = %editc(ita88cmmi: 'X');
121100090722               comcmm = c01cmmi ;
121200090722               exsr Dec_com ;
121300130807               c01cmmid = ComCmmD;
121400100111               ProtCmmi = *on;
121500100114             ELSE;
121600100304         // verifico se passato il chi sono
121700100304               If ita88com  > 0  ;
121800100304                  c01cmmi = %editc(ita88com : 'X');
121900100304                  comcmm = c01cmmi ;
122000100304                  exsr Dec_com ;
122100130807                  c01cmmid = ComCmmD;
122200100304                  ProtCmmi = *on;
122300100304               ELSE;
122400100304                  PosCurCmmi = *on;
122500100304               endif;
122600090722            endif;
122700090722
122800090722        // se passato il commerciale esterno lo valorizzo a video
122900090722            If ita88cmm > 0  ;
123000090722               c01cmm = %editc(ita88cmm : 'X');
123100090722               comcmm = c01cmm ;
123200090722               exsr Dec_com ;
123300130807               c01cmmd = ComCmmD;
123400090722            endif;
123500090722
123600091027        // recupero i responsabili trasporti
123700091027            exsr Rec_responsa ;
123800091027
123900091111        // devo recuperare anche eventuali attività legate
124000091111        // anche se sono in immissione
124100091111
124200091111           exsr Rec_Attivita ;
124300151113
124400151113           // -?Verifica SE cliente o potenziale in Contenzioso?
124500151113           //  ?(se NON già fatto)?
124600151113           if  $CtrlContenz = *off;
124700151113             exsr  sr_CtrSTC;
124800151113           endif;
124900091111
125000150831           //?Attivo F4 se trattativa su cliente
125100150831           F04Attivo = (VISksc <> *zero );
125200100114
125300100114        // se non mi sono posizionata sul commerciale interno mi
125400100114        // posiziono sul tipo
125500100114        IF  not PosCurCmmi;
125600091216           PosCurTpv = *on ;
125700100114        ENDIF;
125800091216
125900091111           leavesr ;
126000090722
126100090722         endif ;
126200090722
126300090722        // Se non è immissione recupero i campi del file e decodifico
126400090722
126500100111         ProtCmmi = *off;
126600091111
126700090722       // valorizzo i campi del video
126800101015            dvis01= visflo ;
126900090722            c01nrv = visnrv ;
127000090722            c01rag = visrag ;
127100090807            c01ksc = visksc ;
127200090722            data_eur = %date(visdat:*iso);
127300090722            c01dta = %dec(data_eur);
127400100111         // proteggo commerciale interno
127500100111            ProtCmmi = *on;
127600090722            c01cmmi = %editc(viscmmi : 'X');
127700090722            comcmm = c01cmmi ;
127800090722            exsr Dec_com ;
127900130807            c01cmmid = ComCmmD;
128000090722            c01cmm = %editc(viscmm : 'X');
128100090722            comcmm = c01cmm  ;
128200090722            exsr Dec_com ;
128300130807            c01cmmd = ComCmmD;
128400090722            c01tpv = vistpv;
128500090722            c01ffz = visffz;
128600100419          // se fittizia proteggo il campo
128700100419            Prot_fittizia = (c01ffz <> ' ' ) ;
128800090722            c01esi = visesi;
128900101015            c01esi2 = §visesito;
129000100205            esito = visesi <> '  ' ;
129100090723            if visdch > 0 ;
129200090723               data_eur = %date(visdch:*iso);
129300090723               c01dch = %dec(data_eur);
129400100205               convalidato = *on ;
129500090723            endif ;
129600090810         // recupero i codice del potenziale da tfaco se esiste il record
129700090810         // altrimenti da cnaco con il codice cliente
129800090810             acokut = 1 ;
129900090810             acokcc = dutkci ;
130000090810             acoksc = c01nrv ;
130100090810             chain(n) (acokut:acokcc:acoksc) Tfaco00f ;
130200090810             If not %found(tfaco00f)  ;
130300090810                acoksc = c01ksc ;
130400090810                chain(n) (acokut:acokcc:acoksc) Cnaco00f ;
130500090810                if not %found(cnaco00f) ;
130600090810                   clear acolib;
130700090810                endif ;
130800090810             endif ;
130900090810             ch1cpo = acolib ;
131000151113
131100151113           // -?Se Gestione:?
131200151113           //  ?Verifica SE cliente o potenziale in Contenzioso?
131300151113           //  ?(se NON già fatto)?
131400151113           if  $CtrlContenz = *off
131500151113               and iTA88fle = 'G';
131600151113             exsr  sr_CtrSTC;
131700151113           endif;
131800090723
131900110112         // verifico se ci sono offerte  e se è caldo
132000110114            $Pres_offerte = *off ;
132100100421            offerte = *off ;
132200110112            caldo   = *off ;
132300110112            Data_caldo  = *off ;
132400110104            setll    visnrv tivof11l ;
132500110104            reade    visnrv tivof11l ;
132600110104                dow not %eof(tivof11l) ;
132700100421                    if vofeso <> '*' ;
132800100421                       offerte = *on ;
132900110114                       $Pres_offerte = *on  ;
133000110112                       If vofeso = 'A' ;
133100110112                          caldo = *on ;
133200110112                          leave ;
133300110112                       endif ;
133400100421                    endif ;
133500110104                    reade    visnrv tivof11l ;
133600100421                enddo ;
133700110112         // verifico se trattativa di incremento calda
133800110112           If vistpv = 'I' ;
133900110112              chain(n) (visnrv:999:000) tivof01l ;
134000110112              If %found(tivof01l) and I_vofeso = 'A' and I_vofdpa > 0 ;
134100110112                 caldo = *on ;
134200110112                 Data_caldo = *on ;
134300110112              endif ;
134400110112           endif ;
134500100205         // se trattativa chiusa però spengo l'indicatore offerte
134600100211               If offerte = *on ;
134700110112                  offerte     = visdch = 0 ;
134800110113               endif ;
134900110113               If caldo   = *on ;
135000110112                  caldo       = visdch = 0 ;
135100110113               endif ;
135200110113               If Data_caldo   = *on ;
135300110112                  Data_caldo  = visdch = 0 ;
135400100211               endif ;
135500090723         // recupero i dati anagrafici del potenziale o del cliente
135600090723               exsr Rec_Responsa ;
135700090722
135800100311        // decodifico il tipo trattativa
135900100311             clear dTTR;
136000100311             clear TIBS02ds;
136100100311             T02mod = 'C';
136200100311             T02cod = 'TTR';
136300100311             T02ke1 = vistpv   ;
136400100311             T02sif = KNSIF;
136500100311             TNTBE_RicercaControllo  (kpjba : tibs02ds);
136600100311             IF  T02err = *blanks;
136700100311                 dTTR = t02uni ;
136800100311                 c01tpvd = §ttrdes;
136900100311             endif ;
137000090722
137100091111        // devo recuperare anche eventuali attività legate
137200091111
137300091111           exsr Rec_Attivita ;
137400091111
137500090722
137600090722       ENDSR;
137700151111
137800151111       //--------------------------------------------------------------
137900151111       //?Controllo dello Stato del Credito.
138000151111       //--------------------------------------------------------------
138100151111       BEGSR  sr_CtrSTC;
138200151111
138300151113         $CtrlContenz = *on;
138400151111         $Contenzioso = *off;
138500151113         clear  wSaveCON;
138600151113
138700151113         w_INDkut = 1;
138800151113         w_INDkcc = DUTkci;
138900151113         clear  w_INDiva;
139000151111
139100151113         Select;
139200151113
139300151113           // -?Nuova Trattativa con Cliente:?
139400151113           //  ?Partita Iva già reperita da CNIND?
139500151113           When  iTA88fle = 'C';
139600151113             w_INDiva = INDiva;
139700151113
139800151113           // -?Nuova Trattativa con Potenziale:?
139900151113           //  ?Partita Iva già reperita da TNCPO?
140000151113           When  iTA88fle = 'P';
140100151113             w_INDiva = CPOpiv;
140200151113
140300151113           // -?Gestione Trattativa:?
140400151113           //  ?Partita Iva NON ancora reperita?
140500151113           //  ?(ma già agganciato TFACO/CNACO)?
140600151113           When  iTA88fle = 'G';
140700151113             if  %found(TFACO00F);
140800160224               clear  INDiva;
140900151113               chain(N)  ( ACOkut : ACOkcc : ACOksc )  TFIND000;
141000151113             else;
141100151113               clear  TIBS69ds;
141200151113               I69kin = ACOksc;
141300151113               tibs69r ( tibs69ds : ds_CNACO
141400151113                                  : ds_CNIND
141500151113                                  : ds_CNCLP
141600151113                                  : ds_FNCLS );
141700151113             endif;
141800151113             w_INDiva = INDiva;
141900151113
142000151113           Other;
142100151113             leavesr;
142200151113
142300151113         EndSl;
142400160315
142500160315         // -?Cliente Privato (senza Partita Iva) => uscita?
142600160315         if  %scan( '$$' : w_INDiva ) > *zero;
142700160315           leavesr;
142800160315         endif;
142900151111
143000151111         // -?Ricerca di altri clienti con stessa Partita Iva?
143100151113         setll  ( w_INDkut : w_INDkcc : w_INDiva )  CNIND002;
143200151113
143300160315         // -?Non trovato nessun cliente con stessa Partita IVA?
143400151113         //  ?=> uscita?
143500151113         if  Not %equal(CNIND02L);
143600151113           leavesr;
143700151113         endif;
143800151111
143900151113         // -?Ciclo di lettura CNIND per Partita Iva?
144000151113         reade  ( w_INDkut : w_INDkcc : w_INDiva )  CNIND002;
144100151113         DoW  Not %eof(CNIND02L);
144200151111
144300151111           // -?Controllo dello stato del credito?
144400151111           clear  TIBS69ds;
144500151113           I69kcp = w_INDksc;
144600151111           tibs69r ( TIBS69ds : ds_CNACO
144700151111                              : ds_CNIND
144800151111                              : ds_CNCLP
144900151111                              : ds_FNCLS );
145000151113
145100151113           // -?Memorizzazione Cliente in Contenzioso?
145200151111           if  CLPcon <> *blank;
145300151111             $Contenzioso = *on;
145400151113             wSaveCON     = CLPcon;
145500151111             leave;
145600151111           endif;
145700151111
145800151113           reade  ( w_INDkut : w_INDkcc : w_INDiva )  CNIND002;
145900151111
146000151111         EndDo;
146100151111
146200151111       ENDSR;
146300151111
146400091111       //--------------------------------------------------------------
146500091111       //?Recupero attività legate alla trattativa
146600091111       //--------------------------------------------------------------
146700091111       BEGSR Rec_Attivita ;
146800100115
146900100115         AttAperte = *off;
147000100115       //?per prima cosa controllo se ci sono attività non legate alla
147100100115       //?trattativa
147200100115         clear TRMK85ds;
147300100115         IMK85cpo = CH1cpo;
147400100115         IMK85ksc = C01ksc;
147500100115         IMK85nrv = C01nrv;
147600100115         trmk85r (trmk85ds);
147700100115         //?se torna errore <> "9" ho trovato altre attività aperte
147800100115         IF  OMK85err <> *blanks and OMK85err <> '9';
147900100115           AttAperte = *on;
148000100115         ENDIF;
148100091111
148200090722
148300091111       // Caricamento dei dati da presentare nel sfl
148400091111         $EoF = *off;
148500091111         //  Preparo la stringa sql
148600091111         stringasql  = 'select TIATC00F.*, case +
148700091111                           when (ATCdco*1000000)+ATChco > 0 +
148800091111                           then (ATCdco*1000000)+ATChco +
148900091111                           else (ATCdad*1000000)+ATChda +
149000091111                           end  as WWWord +
149100091111                           from TIATC00F +
149200091111                           where atcnrv = ' + %editc(c01nrv:'X') ;
149300091111
149400091111         stringasql = %trim(stringasql) + ' order by WWWord desc ' ;
149500091111
149600091111
149700091111         stringasql = %trim(stringaSql) + ' for fetch only';
149800091111
149900091111
150000091111         exec sql prepare S1 from :StringaSql;
150100091111
150200091111         exec sql declare C1 cursor for S1;
150300091111
150400091111         exec sql open C1 ;
150500091111
150600091111         exsr sr_ReadRec;
150700091111
150800091111         dow  $EoF   = *off ;
150900091111           exsr RollUpS01;
151000091111         enddo;
151100091111
151200091111         exec sql close C1 ;
151300091111
151400091111       // Impaginazionesubfile
151500091111       // -> forza l'impaginazione sul 1° rec. del subfile
151600091111         if S01nrr > *zero;
151700091111           C01rcd  = 1;
151800091111           C01csr  = 1;
151900091111         else;
152000091111           clear C01rcd;
152100091111         endif;
152200091111
152300091111       ENDSR ;
152400090723       //--------------------------------------------------------------
152500090723       //?Recupero responsabile trasporti e mail
152600090723       //--------------------------------------------------------------
152700090724       BEGSR Rec_responsa ;
152800090723
152900090724       // devo recuperare le note 05/06 in base al momento in cui sono
153000090724       // In Immissione da potenziale :
153100090724       //                  recupero le note con in key cod.potenziale
153200090724       // In Immissione da cliente    :
153300090724       //                  recupero le note con in key cod.cliente
153400090810       // In Aggiornamento trattativa :
153500090810       //                  recupero le note con in key numero trattativa
153600090810       // In fase di manutenzione trattativa :
153700090810       //                  recupero le note con in key numero trattativa
153800090724
153900090724       // preparo la chiave
154000090810       // se sono in manutenzione di una trattativa e devo recuperare le
154100090810       // note dopo il richiamo del pgm TNTA60 il flag  $modta60 è = *on
154200090917       // o dopo il richiamo del pgm TNTA12 il flag  $Datnta12 è = *on
154300091103       // quindi vado direttamente con la chiave della trattativa
154400090724
154500091109          If ($modta60 = *off and $Datnta12 = *off) or
154600091109              c01ksc <> 0 ;
154700090810
154800090724       // Potenziale
154900090724             If  Ita88fle = 'P' ;
155000090724                 ntcapl = 'P' ;
155100090810                 ntcnk1 = %editc(ch1cpo:'X');
155200090724             endif ;
155300090724
155400090724       // Cliente
155500091109             If  Ita88fle = 'C' or c01ksc <> 0;
155600090724                 ntcapl = 'C' ;
155700090810                 ntcnk1 = %editc(dutkci: 'X') + %editc(c01ksc:'X');
155800090724             endif ;
155900090724
156000090724       // Trattativa
156100091109             If  Ita88nrv > 0 and c01ksc = 0 ;
156200091112                 ntcapl = 'T' ;
156300090724                 ntcnk1 = %editc(dutkci: 'X') + %editc(ita88nrv:'X');
156400090724             endif ;
156500090810
156600091109             endif ;
156700091103       // modifica da tnta12/ta60
156800091109
156900091109          If ($modta60 <> *off or  $Datnta12 <>*off) and
157000091109              c01ksc =  0 ;
157100091112              ntcapl = 'T' ;
157200091109              ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
157300091109          endif ;
157400090810
157500090723       // carico responsabile trasporti
157600090723             ntctnt = '05';
157700090723             clear  ntcnk2 ;
157800090724          setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
157900090922          READE(n) (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
158000090723          dow not %eof(tfntc01l)  ;
158100090724             c01rt1 = ntcrnt ;
158200090922             READE(n) (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l     ;
158300090723             If  not %eof(tfntc01l) ;
158400090724               c01rt2 = ntcrnt ;
158500090723               leave ;
158600090723             else;
158700090723               leave ;
158800090723             endif ;
158900090723          enddo ;
159000090723
159100090723       // carico mail responsabile trasporti
159200090723             ntctnt = '06';
159300090723             clear  ntcnk2 ;
159400090922          chain(n) (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l             ;
159500090723          if  %found(tfntc01l)  ;
159600090724             c01mrt = ntcrnt ;
159700090723          endif ;
159800090723
159900090723       ENDSR;
160000090722       //--------------------------------------------------------------
160100090722       //?Decodifica Commerciale
160200090722       //--------------------------------------------------------------
160300090722       BEGSR Dec_com ;
160400130807
160500130807         clear  ComCmmD;
160600090722
160700130807         if  %check(digits:ComCmm) > 0;
160800130807           leavesr;
160900130807         endif;
161000130807
161100090722        // decodifico il commerciale
161200130807         CMMcod = %int(ComCmm);
161300130807         chain  (CMMcod)  AZCMM000;
161400130807         if  %found(AZCMM01L);
161500130807           ComCmmD = CMMdes;
161600130807         endif;
161700090722
161800090722       ENDSR ;
161900090722
162000090722       //--------------------------------------------------------------
162100090722       //?Pulizia SFL01
162200090722       //--------------------------------------------------------------
162300090722       BEGSR Pulsfl1;
162400090722
162500090722       // Pulizia subfile
162600090722         SflDsp_N1   = *on;
162700090722         SflDspCtl_N1= *on;
162800090722         write  TA88C01;
162900090722         SflDspCtl_N1= *off;
163000090722         SflEnd1     = *off;
163100090722
163200090722
163300090722         clear W_SflPag1;
163400090722         clear C01rcd;
163500090722         clear C01csr;
163600090722         clear S01nrr;
163700090722         clear V1Dmsg;
163800090722         errMessage  = *off;
163900090722         errGenerico = *off;
164000090722         WindDspF    = IndDspF;
164100090722         reset WindDspFb;
164200090722         IndDspF     = WindDspF;
164300090722
164400090722       ENDSR ;
164500090722
164600090722       //--------------------------------------------------------------
164700090722       //?Caricamento di tutto il sfl
164800090722       //--------------------------------------------------------------
164900090722       BEGSR RollUpS01;
165000090722
165100090722         clear  s01nrr;
165200090723         SflNxtChg1= *off;
165300090722
165400090722         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
165500090722         DOW  $EoF   = *off  and
165600090722              S01nrr < *hival;
165700090722
165800090722         // - Caricamento dati nel record del subfile
165900090722           exsr  RieS01;
166000090722           If $recok = *on ;
166100090722              S01nrr += 1;
166200090722              write TA88S01;
166300090722           endif ;
166400090722
166500090722         // - Lettura record successivo nell'archivio
166600090722           exsr sr_ReadRec;
166700090722
166800090722         ENDDO;
166900090722
167000090722         // Visualizzazione del SFL (se ci sono dati)
167100090723         SflDsp_N1= (S01nrr <= *zeros);
167200090722
167300090722         // Attivazione (eventuale) del SFLEND
167400090723         SflEnd1 =  *on;
167500090722
167600090722         // Posizionamento cursore al 1° rcd della pagina
167700090722         if  S01nrr > *zero;
167800090813         //  wPag1  = S01nrr / C_SflPag1;
167900090813         //  wRig1  = S01nrr - (C_SflPag1 * wPag1);
168000090813         //  C01rcd = wPag1 * C_SflPag1;
168100090813         //  if  wRig1 > *zeros;
168200090722         //    C01rcd = C01rcd + 1;
168300090722         //  else;
168400090813         //    C01rcd = C01rcd - C_SflPag1 + 1;
168500090722         //  endif;
168600090722             c01rcd = 1;
168700100115             New_Attivita = *off;
168800090722         else;
168900090722           clear  C01rcd;
169000090722         endif;
169100090722
169200090722         C01csr = C01rcd;
169300090722
169400090722       ENDSR;
169500090722
169600090722       //--------------------------------------------------------------
169700090722       //?Lettura record successivo
169800090722       //--------------------------------------------------------------
169900090722       BEGSR sr_ReadRec;
170000090722
170100090722         $RecOK  = *off;
170200090722
170300090722         exec sql Fetch C1 into :tiatcds;
170400090722
170500090804         if  Sqlcod = 100 or Sqlcod < 0 ;
170600090722              $Eof  = *on;
170700090804         else ;
170800090722             $RecOK  = *on;
170900090722         endif;
171000090722       ENDSR;
171100090722
171200090722
171300090722       //--------------------------------------------------------------
171400090722       //?Caricamento singolo record nel SubFile S01
171500090722       //--------------------------------------------------------------
171600090722       BEGSR RieS01;
171700090722
171800090722         clear  ta88S01;
171900090722
172000090722         // Attività già eseguita
172100090722
172200090722         if  atcdco > 0;
172300090722         // commerciale
172400090722             s01com = atccco;
172500090722         // data e ora contatto e causale eseguita
172600090722             Coldafare = *off;
172700090722             data_dmy = %date(atcdco:*iso) ;
172800090722             s01dat = %dec(data_dmy) ;
172900090722             s01oat = %dec(%subst(%EDITC(atchco:'X'):1:4):4:0);
173000090722             s01cat = atccac ;
173100090722             sh1fatto = 'S' ;
173200090722         endif ;
173300090722
173400090722         // Attività da eseguire
173500090722
173600090722         if  atcdco = 0;
173700090722         // commerciale
173800090722             s01com = atccmm;
173900090722         // data e ora contatto e causale da eseguire
174000090722             Coldafare = *on ;
174100090722             data_dmy = %date(atcdad:*iso) ;
174200090722             s01dat = %dec(data_dmy) ;
174300090722             s01oat = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
174400090722             s01cat = atccad ;
174500090722             sh1fatto = ' ' ;
174600090722         endif ;
174700090722
174800090723            comcmm = %editc(s01com: 'X');
174900090722            exsr Dec_com ;
175000130807            s01comd  = ComCmmD;
175100090722
175200090722         // decodifica della causale
175300090722             clear  dCCO;
175400090722             clear  TIBS02ds;
175500090722             T02mod = 'C';
175600090722             T02cod = 'CCO';
175700090722             T02ke1 = s01cat;
175800090722             T02sif = KNSIF;
175900090722             TNTBE_RicercaControllo  (kpjba : tibs02ds);
176000090722             if  T02err = *blanks;
176100090722               dcco = t02uni ;
176200090722               s01catd = §ccodes ;
176300090722             endif ;
176400100115         // valorizzo il campo delle opzioni inibite
176500100115             sh1opz = §ccoopz ;
176600090722
176700090722             s01tat = atctat ;
176800090804             sh1cpo = atccpo ;
176900090804             sh1atn = atcatn ;
177000090804             sh1atnp = atcatnp ;
177100090722
177200090722       ENDSR;
177300090722
177400090722       //--------------------------------------------------------------
177500090722       //?Controllo i dati della parzializzazione C01
177600090722       //--------------------------------------------------------------
177700090722       BEGSR CtrC01;
177800090722
177900090724            WindDspF = IndDspF;
178000090724            reset WindDspFb;
178100090724            IndDspF  = WindDspF;
178200110314
178300110314            $w05 = *off;
178400090724
178500090724        // Controllo data apertura trattativa
178600090724           If    c01dta <> 0;
178700090724             clear wlbdat;
178800090724             g02dat = c01dta;
178900090724             xsrda8(wlbdat);
179000090724             if g02err = '1';
179100090724               errMessage  = *on;
179200090724               errGenerico = *on;
179300090724               PosCurDta   = *on;
179400090724               V1Dmsg = $Msg(02);
179500090724               leavesr;
179600090724             endif;
179700090724
179800090724             c01dta = g02dat;
179900090724             w01dta = g02inv;
180000090724           else ;
180100090724             errMessage  = *on;
180200090724             errGenerico = *on;
180300090724             PosCurDta   = *on;
180400090724             V1Dmsg = $Msg(02);
180500090724             leavesr;
180600090724           endif;
180700100119
180800100119       //?controllo che la data non sia inferiore al 2000 o superiore al 2039
180900100119         IF  w01dta < 20000101 or w01dta > 20391231;
181000100119           ErrMessage  = *on;
181100100119           ErrGenerico = *on;
181200100119           PosCurDta   = *on;
181300100119           V1Dmsg      = $Msg(02);
181400100119           leavesr;
181500100119         ENDIF;
181600090724
181700090724        // verifico se Anno/mese antecedente Anno/mese ultima statistica
181800090724        // e sono in fase di modifica
181900090724           If ita88fle = 'G' and w01dta <> visdat and
182000090916              %dec(%subst(%EDITC(w01dta:'X'):1:6):6:0) <= §sdfstcu ;
182100090724             errMessage  = *on;
182200090724             errGenerico = *on;
182300090724             PosCurDta   = *on;
182400090724             V1Dmsg = $Msg(03);
182500090724             leavesr;
182600090724           endif;
182700090724
182800100422        // controllo il commerciale interno solo se non è protetto
182900100422          If ProtCmmi = *off ;
183000090724         // Controllo codice commerciale Interno (? Ricerca)
183100090724           If    %scan('?' : c01cmmi) > 0  ;
183200090724             errGenerico = *on;
183300090724             PosCurCmmi  = *on;
183400100315        // se esiste  il commerciale esterno ricerco con la filiale
183500100315        // di quest'ultimo
183600130807             clear  TRMK43ds;
183700100315             If %subst(c01cmm :1:3) <> '   '  ;
183800130807               iMK43fil = %int( %subst( C01cmm : 1 : 3 ) );
183900100315             endif ;
184000130807             TRMK43R (kpjba : TRMK43ds);
184100130807             if  oMK43err = *off  and  oMK43fxx = *blank;
184200130807               C01cmmi  = %editc( oMK43cmm : 'X' );
184300130807               C01cmmid = oMK43des;
184400130807             endif;
184500090724             leavesr;
184600090724           endif;
184700090724
184800090724         // Controllo codice commerciale interno
184900100111           If    c01cmmi = *blanks or c01cmmi = *zeros;
185000100111             errMessage  = *on;
185100100111             errGenerico = *on;
185200100111             PosCurCmmi  = *on;
185300100111             V1Dmsg = $Msg(04);
185400100111             leavesr;
185500100111           endif;
185600100111
185700100111         IF  %check(digits:c01cmmi) > 0;
185800100111             errMessage  = *on;
185900100111             errGenerico = *on;
186000100111             PosCurCmmi  = *on;
186100100111             V1Dmsg = $Msg(04);
186200100111             leavesr;
186300100111         ENDIF;
186400100111
186500130807             CMMcod = %int(C01cmmI);
186600130807             chain  (CMMcod)  AZCMM000;
186700130807             if  NOT  %found(AZCMM01L);
186800090724               errMessage  = *on;
186900090724               errGenerico = *on;
187000090724               PosCurCmmi  = *on;
187100090724               V1Dmsg = $Msg(04);
187200090724               leavesr;
187300090724             else;
187400130807               C01cmmId = CMMdes;
187500090724             endif;
187600090806
187700090807         // Controllo se codice commerciale VARI
187800130807           If    CMMpar <> *blanks;
187900090807             errMessage  = *on;
188000090807             errGenerico = *on;
188100090807             PosCurCmmi  = *on;
188200090807             V1Dmsg = $Msg(12);
188300090807             leavesr;
188400090807           endif;
188500111129
188600111129         // Il commerciale deve essere ancora attivo
188700130807           IF  CMMdtIni > wOggi  or
188800130807               CMMdtFin < wOggi;
188900111129             errMessage  = *on;
189000111129             errGenerico = *on;
189100111129             PosCurCmmi  = *on;
189200111129             V1Dmsg = $Msg(12);
189300111129             leavesr;
189400111129           endif;
189500100420
189600100420           // utente abilitato al commerciale
189700100420             clear TNTAA1DS;
189800120810             ITAA1tipo = 'M';
189900120810             ITAA1caut = '§utegtc';
190000100420             ITAA1cmm  = %dec(C01cmmi:7:0);
190100100521             ITAA1abi  = 'RA';
190200100420             kpjbu     = tntaa1ds;
190300100420             tntaa1c (kpjba);
190400100420             TNTAA1DS = kpjbu;
190500100420             IF  OTAA1fabi = 'N';
190600100420               ErrMessage  = *on;
190700100420               ErrGenerico = *on;
190800100420               PosCurCmmi  = *on;
190900100420               V1Dmsg      = $Msg(04);
191000100420               leavesr;
191100100420             ENDIF;
191200100420
191300100422             Endif ;
191400090803         // Controllo codice tipo trattativa (? Ricerca)
191500090803           If    %scan('?' : c01tpv) > 0  ;
191600090803             errGenerico = *on;
191700090803             PosCurTpv   = *on;
191800100311             clear dTTR;
191900100311             clear TIBS02ds;
192000100311             T02mod = 'R';
192100100311             T02cod = 'TTR';
192200100311             T02sif = KNSIF;
192300100311             TNTBE_RicercaControllo  (kpjba : tibs02ds);
192400100311             IF  T02err = *blanks;
192500100311                 dTTR = t02uni ;
192600100311                 c01tpv = t02ke1 ;
192700100311                 c01tpvd = §ttrdes;
192800100311             else ;
192900100311                  errMessage  = *on;
193000100311                  errGenerico = *on;
193100100311                  PosCurTpv   = *on;
193200100311                  V1Dmsg = $Msg(05);
193300100311                  leavesr;
193400100311             endif;
193500090803             leavesr;
193600090803           endif;
193700090803
193800090803         // Controllo tipologia trattativa
193900090803           If    c01tpv  =  *blanks ;
194000090803             errMessage  = *on;
194100090803             errGenerico = *on;
194200090804             PosCurTpv   = *on;
194300090804             V1Dmsg = $Msg(05);
194400090803             leavesr;
194500090804
194600090804        // Verifico se corretto
194700090804           else ;
194800100311             clear dTTR;
194900100311             clear TIBS02ds;
195000100311             T02mod = 'C';
195100100311             T02cod = 'TTR';
195200100311             T02ke1 = c01tpv   ;
195300100311             T02sif = KNSIF;
195400100311             TNTBE_RicercaControllo  (kpjba : tibs02ds);
195500100311             IF  T02err = *blanks;
195600100311                 dTTR = t02uni ;
195700100311                 c01tpvd = §ttrdes;
195800090804             else ;
195900090804                  errMessage  = *on;
196000090804                  errGenerico = *on;
196100090804                  PosCurTpv   = *on;
196200090804                  V1Dmsg = $Msg(05);
196300090804                  leavesr;
196400090804             endif;
196500090804           endif;
196600090804
196700090804        // Verifico se tipo trattativa congruente con il cliente
196800100311          If c01ksc = 0 and §ttrVcn <> 'N' ;
196900090804             errMessage  = *on;
197000090804             errGenerico = *on;
197100090804             PosCurTpv   = *on;
197200090804             V1Dmsg = $Msg(06);
197300090804             leavesr;
197400090803           endif ;
197500090722
197600100311          If c01ksc > 0 and §ttrVcn = 'N' ;
197700090804             errMessage  = *on;
197800090804             errGenerico = *on;
197900090804             PosCurTpv   = *on;
198000090804             V1Dmsg = $Msg(07);
198100090804             leavesr;
198200090804           endif ;
198300090916
198400100311        // Verifico se tipo trattativa congruente con le Offerte
198500100311          If offerte = *on  and §ttrOff = 'N' ;
198600100311             errMessage  = *on;
198700100311             errGenerico = *on;
198800100311             PosCurTpv   = *on;
198900100311             V1Dmsg = $Msg(31);
199000100311             leavesr;
199100100311           endif ;
199200110104
199300100419        // se sono in modifica non posso variare il flag da vero a fittizio
199400100507        // se esistono delle attività legate alla trattative (anche chiuse)
199500100507        // e se esistono delle offerte esitate
199600100419           If ita88fle = 'G' and c01ffz <> visffz ;
199700100419              atccpo = ch1cpo ;
199800100419              atcksc = c01ksc ;
199900100419              atcnrv = c01nrv ;
200000100507              setll (atccpo:atcksc:atcnrv) tiatc04l ;
200100100419              If %equal ;
200200100419                 errMessage  = *on;
200300100419                 errGenerico = *on;
200400100419                 PosCurFfz   = *on;
200500100419                 V1Dmsg = $Msg(36);
200600100419                 leavesr;
200700100419             endif;
200800110104             setll    c01nrv tivof11l ;
200900110104             reade    c01nrv tivof11l ;
201000110104                dow not %eof(tivof11l) ;
201100100507                    if vofeso <> '*' and vofeso <> 'H' and
201200100507                       vofeso <> ' ' ;
201300100507                       errMessage  = *on;
201400100507                       errGenerico = *on;
201500100507                       PosCurFfz   = *on;
201600100507                       V1Dmsg = $Msg(38);
201700100507                       leavesr;
201800100507                    endif ;
201900110104                    reade    c01nrv tivof11l ;
202000100507                enddo ;
202100100419           ENDIF;
202200090804
202300090806         // Controllo codice commerciale Esterno (? Ricerca)
202400090806           If    %scan('?' : c01cmm) > 0  ;
202500090806             errGenerico = *on;
202600090806             PosCurCmm  = *on;
202700100315        // se esiste  il commerciale esterno ricerco con la filiale
202800100315        // di quest'ultimo
202900130807             clear  TRMK43ds;
203000100315             If %subst(c01cmmi :1:3) <> '   '  ;
203100130807               iMK43fil = %int( %subst( C01cmmi : 1 : 3 ) );
203200100315             endif ;
203300130807             TRMK43R (kpjba : TRMK43ds);
203400130807             if  oMK43err = *off  and  oMK43fxx = *blank;
203500130807               C01cmm  = %editc( oMK43cmm : 'X' );
203600130807               C01cmmd = oMK43des;
203700130807             endif;
203800090806             leavesr;
203900090806           endif;
204000090806
204100090806         // Controllo codice commerciale esterno
204200100111           If    c01cmm = *blanks or c01cmm = *zeros;
204300100111             errMessage  = *on;
204400100111             errGenerico = *on;
204500100111             PosCurCmm  = *on;
204600100111             V1Dmsg = $Msg(11);
204700100111             leavesr;
204800100111           endif;
204900100111
205000100111         IF  %check(digits:c01cmm) > 0;
205100100111             errMessage  = *on;
205200100111             errGenerico = *on;
205300100111             PosCurCmm   = *on;
205400100111             V1Dmsg = $Msg(11);
205500100111             leavesr;
205600100111         ENDIF;
205700100111
205800130807             CMMcod = %int(C01cmm);
205900130807             chain  (CMMcod)  AZCMM000;
206000130807             if  NOT  %found(AZCMM01L);
206100090806               errMessage  = *on;
206200090806               errGenerico = *on;
206300090806               PosCurCmm  = *on;
206400090806               V1Dmsg = $Msg(11);
206500090806               leavesr;
206600090806             else;
206700130807               C01cmmD = CMMdes;
206800090806             endif;
206900100125
207000100420
207100100420           // utente abilitato al commerciale
207200100420             clear TNTAA1DS;
207300120810             ITAA1tipo = 'M';
207400120810             ITAA1caut = '§utegtc';
207500100420             ITAA1cmm  = %dec(C01cmm:7:0);
207600100521             ITAA1abi  = 'RA';
207700100420             kpjbu     = tntaa1ds;
207800100420             tntaa1c (kpjba);
207900100420             TNTAA1DS = kpjbu;
208000100420             IF  OTAA1fabi = 'N';
208100100420               ErrMessage  = *on;
208200100420               ErrGenerico = *on;
208300100420               PosCurCmm  = *on;
208400100420               V1Dmsg      = $Msg(04);
208500100420               leavesr;
208600100420             ENDIF;
208700100420
208800100125          // se trattativa aperta su cliente codificato il
208900100125          // commericale esterno deve essere un commerciale della
209000100125          // filiale cliente
209100100125           IF  C01ksc > 0 and %subst(%editc(c01ksc:'X'):1:3) <>
209200100125               %subst(C01cmm:1:3);
209300100125             errMessage  = *on;
209400100125             errGenerico = *on;
209500100125             PosCurCmm  = *on;
209600100125             V1Dmsg = $Msg(28);
209700100125             leavesr;
209800100125           ENDIF;
209900090806
210000090806         // Controllo se codice commerciale VARI
210100130807           If    CMMpar <> *blanks;
210200090806             errMessage  = *on;
210300090806             errGenerico = *on;
210400090806             PosCurCmm  = *on;
210500090806             V1Dmsg = $Msg(12);
210600090806             leavesr;
210700090806           endif;
210800111129
210900111129         // Il commerciale deve essere ancora attivo
211000130807           IF  CMMdtIni > wOggi  or
211100130807               CMMdtFin < wOggi;
211200111129             errMessage  = *on;
211300111129             errGenerico = *on;
211400111129             PosCurCmm  = *on;
211500111129             V1Dmsg = $Msg(12);
211600111129             leavesr;
211700111129           endif;
211800100119
211900100419       // se digitato f10 e trattativa fittizia errore
212000100419         If  dsp_aid = c_F10 and c01ffz <> ' ' ;
212100100419           errMessage  = *on;
212200100419           errGenerico = *on;
212300100419           PosCurFfz  = *on;
212400100419           V1Dmsg = $msg(35);
212500100419           leavesr;
212600100419         endif ;
212700100119
212800100119       //?Controllo se esistono altre trattative aperte non fittizie
212900100413       //?per stesso potenziale/commerciale/cliente
213000100414       // solo se la trattativa non è fittizia
213100100414         If c01ffz = ' ' ;
213200100119         clear TNTA43ds;
213300100119         ITA43cpo = ch1cpo;
213400100119         ITA43nrv = c01nrv;
213500100413         ITA43ksc = c01ksc;
213600100119         ITA43cmm = %dec(c01cmm:7:0);
213700100119         tnta43r (tnta43ds);
213800100125         IF  OTA43err <> *blanks;
213900100119           errMessage  = *on;
214000100119           errGenerico = *on;
214100100119           PosCurCmm  = *on;
214200100119           V1Dmsg = OTA43msg;
214300100119           leavesr;
214400100119         ENDIF;
214500100414         Endif ;
214600100119
214700100119       //?Controllo se esistono altri appuntamenti sulla stessa trattativa
214800100119       //?per commerciale diverso
214900100311         if c01cmm <> sav_c01cmm ;
215000100311             $forzacomm = *off ;
215100100311         endif ;
215200100119         clear TNTA45ds;
215300100119         ITA45tip = 'A';
215400100119         ITA45nrv = c01nrv;
215500100125         ITA45cmm = %dec(c01cmm:7:0);
215600100119         tnta45r (tnta45ds);
215700100311         IF  OTA45err <> *blanks and $forzacomm = *off ;
215800100125           errMessage  = *on;
215900100125           errGenerico = *on;
216000100125           PosCurCmm  = *on;
216100100125           V1Dmsg = OTA45msg;
216200100311           $forzacomm = *on ;
216300100311           sav_c01cmm = c01cmm ;
216400100125           leavesr;
216500100119         ENDIF;
216600100119
216700090806
216800090806        // Controllo la mail del responsabile trasporti
216900090806          If c01mrt <> *blanks ;
217000090806             clear dsemail ;
217100090806             §emlindi = c01mrt ;
217200090806             xemail (dsemail) ;
217300090806        // Errore
217400090806             If   §emlerro <> '0' ;
217500090806                  errMessage  = *on;
217600090806                  errGenerico = *on;
217700090806                  PosCurMrt  = *on;
217800090806                  V1Dmsg = §emlmsgo;
217900090806                  leavesr;
218000090806             else ;
218100090806                  c01mrt = §emlindo;
218200090806             endif;
218300090806          endif;
218400090812
218500090812        // verifico se ci sono le info commerciali
218600121211          Info_Obb = *off;
218700090812          exsr SR_Infocom;
218800090812          If errGenerico = *on ;
218900090812             leavesr ;
219000090812          endif ;
219100090807
219200090807        // se non ci sono errori e la visita è legata ad un cliente
219300090807        // nuovo creo anagrafica
219400100311          If §TTRVcn = 'N' ;
219500090807             exsr sr_CreaAna ;
219600090807          Endif ;
219700100315        // se non ci sono errori e la trattativa è nuova ed è stato dato
219800110111        // f10 mi comporto come se avesse dato F06
219900100315          If (ita88fle = 'P' or ita88fle = 'C') and dsp_aid = c_F10 ;
220000100315        // primo confermo la trattativa
220100100315             exsr F06S01 ;
220200110104             if  errGenerico = *on;
220300110104                 leavesr;
220400110104             endif;
220500100315          endif ;
220600110112
220700110112        // se sono in variazione e modifico il tipo trattativa chiedo conferma
220800110112        // della modifica
220900110112           If ita88fle = 'G' and vistpv <> c01tpv ;
221000110112              exsr gesw05 ;
221100110314              $w05 = (W05sino = 'S');
221200110112       // se non è stato confermato il cambio trattativa do errore
221300110112       // alla prima videata
221400110112              If w05sino   = 'N';
221500110112                 errMessage  = *on;
221600110112                 errGenerico = *on;
221700110112                 c01tpv = vistpv  ;
221800110112        // decodifico il tipo trattativa
221900110112                 clear dTTR;
222000110112                 clear TIBS02ds;
222100110112                 T02mod = 'C';
222200110112                 T02cod = 'TTR';
222300110112                 T02ke1 = vistpv   ;
222400110112                 T02sif = KNSIF;
222500110112                 TNTBE_RicercaControllo  (kpjba : tibs02ds);
222600110112                 IF  T02err = *blanks;
222700110112                     dTTR = t02uni ;
222800110112                     c01tpvd = §ttrdes;
222900110112                 Endif;
223000110112                 Poscurtpv = *on ;
223100110112                 V1Dmsg = $Msg(41);
223200110112                 leavesr;
223300110112              endif;
223400150310           endif ;
223500110112        // se è stato richiesto f15 confermo i dati  sul file
223600150310           If $f15_on = *on   ;
223700110112        // primo confermo la trattativa
223800110112                 exsr F06S01 ;
223900110112                 if  errGenerico = *on;
224000110112                     leavesr;
224100110112                 endif;
224200150310           endif ;
224300090806       ENDSR ;
224400090807
224500090807       //--------------------------------------------------------------
224600090807       //?Gestione e creazione anagrafiche provvisorie
224700090807       //?
224800090807       //--------------------------------------------------------------
224900090807       BEGSR SR_CREAANA ;
225000090807
225100090807       // verifico se esiste già l'anagrafica
225200090807             acokut = 1 ;
225300090807             acokcc = dutkci ;
225400090807             acoksc = c01nrv ;
225500090922             chain(n) (acokut:acokcc:acoksc) Tfaco00f ;
225600090807             If not %found(tfaco00f)  ;
225700090807       // se non trovato creo anagrafiche
225800090807       // da cliente
225900091123                if nuova_client = *on  or c01ksc > 0 ;
226000090807                   exsr Sr_DaCliente ;
226100090807                endif ;
226200090807       // da potenziale
226300090807                if nuova_potenz = *on ;
226400090807                   exsr Sr_DaPotenz  ;
226500090807                endif ;
226600090807
226700090807             endif ;
226800090807       ENDSR;
226900090807
227000090807       //--------------------------------------------------------------
227100090807       //?Creazione anagrafiche provvisorie da Cliente
227200090807       //?
227300090807       //--------------------------------------------------------------
227400090807       BEGSR Sr_DaCliente ;
227500090807
227600090807             clear  tibs69ds;
227700090807             I69kac = C01ksc  ;
227800090807             I69kin = C01ksc  ;
227900090807             I69kcp = C01ksc  ;
228000090807             I69kcs = C01ksc  ;
228100090807             TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
228200090807
228300090807             If acoksc = c01ksc ;
228400090807                acoksc = c01nrv ;
228500090810                write Tfaco000 ;
228600090807             endif ;
228700090807
228800090807             If indksc = c01ksc ;
228900090807                indksc = c01nrv ;
229000090810                write Tfind000 ;
229100090807             endif ;
229200090807
229300090807             If clpksc = c01ksc ;
229400090807                clpksc = c01nrv ;
229500090810                write Tfclp000 ;
229600090807             endif ;
229700090807
229800090807             If clsksc = c01ksc ;
229900090807                clsksc = c01nrv ;
230000090810                write Tfcls000 ;
230100090807             endif ;
230200100914
230300110118             setll c01ksc Fncba01l ;
230400110118             reade c01ksc Fncba01l ;
230500100914             dow not %eof(Fncba01l);
230600100914                 If cbaksc = c01ksc ;
230700100914                    cbaksc = c01nrv ;
230800100914                    write tfcba000 ;
230900100914                 Endif ;
231000110118                 reade c01ksc Fncba01l ;
231100100914             enddo ;
231200100914
231300091123
231400091123         // copio la nota  DC del cliente
231500091123                 ntcapl = 'C' ;
231600091123                 ntcnk1 = %editc(dutkci: 'X') + %editc(c01ksc:'X');
231700091123                 ntctnt = 'DC';
231800091123                 clear  ntcnk2 ;
231900091123                 Chain(n) (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
232000091123                 if %found(tfntc01l) ;
232100091123                       NtcApl = 'T'  ;
232200091123                       ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
232300091123                       write tfntc;
232400091123                 endif ;
232500090807
232600090807       ENDSR;
232700090807
232800090807       //--------------------------------------------------------------
232900090807       //?Creazione anagrafiche provvisorie da Potenziale
233000090807       //?
233100090807       //--------------------------------------------------------------
233200090807       BEGSR Sr_DaPotenz  ;
233300090807
233400090807       // per creare la nuova anagrafica da potenziale richiamo
233500090807       // tnta60
233600090807       // XXXXXX dovrei fare il richiamo con "T" trattativa e modificare
233700090807       // il TNTA60R
233800090807
233900090807          exsr Sr_Anagra ;
234000090807
234100090807       ENDSR;
234200090807
234300090807       //--------------------------------------------------------------
234400090807       //?Gestione  anagrafiche provvisorie
234500090807       //?
234600090807       //--------------------------------------------------------------
234700090807       BEGSR Sr_Anagra  ;
234800090807
234900090810          $modta60 = *off;
235000090810
235100090807          clear Tnta60ds ;
235200090917          ta60flg = 'T';
235300090807          ta60nrv = c01nrv ;
235400090810          ta60cpo = ch1cpo ;
235500090810          ta60cmm = %dec(c01cmm:7:0) ;
235600090810          ta60cli = c01ksc ;
235700090810       // anagrafica nuova
235800091123          if ita88fle = 'C'  ;
235900090810             ta60icli = '1' ;
236000090810          endif ;
236100090810
236200090810       // gestire anche il flag ta60pot nel caso in cui vengo da potenziale
236300090810       // XXXXXXX
236400090810
236500090810          tnta60r (kpjba:tnta60ds) ;
236600090810
236700090810       // gestisco i dati di ritorno
236800090810
236900090810          if ta60err <> *blanks ;
237000090810             errMessage  = *on;
237100090810             errGenerico = *on;
237200090810             V1Dmsg = ta60msg;
237300090810             leavesr;
237400090810          EndIf ;
237500090810
237600090810       // se non ci sono stati errori
237700090810
237800090810          If ta60f03 = *blanks ;
237900090810
238000090810       // verifico se modificato il codice commerciale
238100090810
238200090810          if %editc(ta60cmm: 'X') <> c01cmm ;
238300090810             $modta60 = *on ;
238400090810             c01cmm = %editc(ta60cmm : 'X') ;
238500090810
238600090810       // decodifica
238700090810
238800130807             CMMcod = %int(C01cmm);
238900130807             chain  (CMMcod)  AZCMM000;
239000130807             if   %found(AZCMM01L) ;
239100130807               C01cmmD = CMMdes;
239200090810             endif;
239300090810          endif;
239400090810
239500091023         endif ;
239600090810       // aggancio TFACO per recuperare una eventuale anagrafica modificata
239700090810
239800090810             acokut = 1 ;
239900090810             acokcc = dutkci ;
240000090810             acoksc = c01nrv ;
240100090810             chain(n) (acokut:acokcc:acoksc) Tfaco00f ;
240200090810             If %found(tfaco00f) and acorag <> c01rag ;
240300090810                $modta60 = *on ;
240400090810                c01rag = acorag ;
240500090810             endif ;
240600090810
240700091109        // se non c'è anagrafica segnalo un errore  se cliente = a 0
240800091109             if  not %found (tfaco00f) and c01ksc = 0;
240900090810                 errMessage  = *on;
241000090810                 errGenerico = *on;
241100090810                 V1Dmsg = $msg(13);
241200090810                 leavesr;
241300090810             EndIf ;
241400090810
241500090810        // recupero le note del responsabile trasporti
241600090810             exsr Rec_Responsa ;
241700090807
241800090810
241900090807       ENDSR;
242000091124
242100091124       //--------------------------------------------------------------
242200091124       //?Gestione agenda commerciale che ha in gestione la trattativa
242300091124       //?F11=Agenda
242400091124       //--------------------------------------------------------------
242500091124       BEGSR F11S01;
242600091124
242700091124         // Richiamo il pgm visualizzazione agenda
242800091124
242900091124            clear trmk82ds ;
243000100104            imk82rich = '1' ;
243100091124         // definisco il commerciale da passare
243200091124
243300091124         // vince uno dei due commerciali valorizzati nella trattativa
243400091124         // il commerciale che ha in gestione la trattativa
243500091124         // il commerciale che ha aperto la trattativa
243600091124
243700091124            select ;
243800130807            when c01cmm <> *blanks  and
243900130807                 %check(digits:C01cmm) = 0;
244000091124                imk82cage = %dec(c01cmm:7:0) ;
244100130807            when c01cmmi <> *blanks  and
244200130807                 %check(digits:C01cmmI) = 0;
244300091124                imk82cage = %dec(c01cmmi:7:0) ;
244400091124            endsl ;
244500091124
244600091124            kpjbu = trmk82ds ;
244700091124
244800091124            Trmk82r (kpjba);
244900091124
245000091124       ENDSR;
245100091124
245200080206       //--------------------------------------------------------------
245300090810       //?Gestione tasto funzionale F07 da videata S01
245400090810       //?F07=GESTIONE ANAGRAFICA
245500080206       //--------------------------------------------------------------
245600090810       BEGSR F07S01;
245700090609
245800091123         // Richiamo il pgm creazione ANAGRAFICA prima di fare la creazione
245900091123         exsr Sr_CreaAna;
246000091120         // Richiamo il pgm gestione ANAGRAFICA
246100090810         exsr Sr_anagra ;
246200091120         // se ritorno da gestione anagrafica ed è stato dato f7 cancello
246300091120         // la nota DC se non c'è anagrafico
246400091123         If ta60f03 = '1' ;
246500091120             acokut = 1 ;
246600091120             acokcc = dutkci ;
246700091120             acoksc = c01nrv ;
246800091120             setll (acokut:acokcc:acoksc) Tfaco00f ;
246900091120             If not %equal(tfaco00f)  ;
247000091120         // deleto la nota  DC del cliente
247100091120                 ntcapl = 'T' ;
247200091120                 ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
247300091120                 ntctnt = 'DC';
247400091120                 clear  ntcnk2 ;
247500091120                 Chain    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
247600091120                 if %found(tfntc01l) ;
247700091120                       delete tfntc;
247800091120                 endif ;
247900091120             endif ;
248000091120         endif ;
248100090407
248200080206       ENDSR;
248300090804
248400090804       //--------------------------------------------------------------
248500090804       //?Gestione tasto funzionale F10 da videata S01
248600090804       //?F10=Inserisci nuove attività
248700090804       //--------------------------------------------------------------
248800090804       BEGSR F10S01;
248900090804
249000091022         // Richiamo il pgm trmk17c
249100091022             clear trmk17ds ;
249200100107             imk17fcmt = '1'  ;
249300100107             imk17cmt = 'S'  ;
249400090804         // devo recuperare i dati da registrare nell'attività futura
249500100107             imk17cpo = ch1cpo;
249600090804             imk17ksc = c01ksc ;
249700090804             imk17nrv = c01nrv ;
249800100107             imk17cmm = %dec(c01cmm:7:0) ;
249900100303             imk17co3 = ita88com ;
250000100303
250100100303             imk17dat = c01dta ;
250200100303             imk17cgt = %dec(c01cmm:7:0) ;
250300090804
250400091022             trmk17c (kpjba : trmk17ds) ;
250500090806             $inzs01 = *on ;
250600090804
250700090804       ENDSR;
250800100111
250900100111       //--------------------------------------------------------------
251000100111       //?Gestione tasto funzionale F3 da videata S01
251100100111       //?F3=Fine
251200100111       //--------------------------------------------------------------
251300100111       BEGSR F03S01;
251400100111
251500100111         // Se non ci sono attività aperte a fronte di questa trattativa
251600100114         // messaggio di errore
251700100518
251800100518         // non controllo il campo del video ma quello del file
251900100114
252000100518         IF  not visualizza and visffz = ' ';
252100100201          atccpo = ch1cpo ;
252200100201          atcksc = c01ksc ;
252300100201          atcnrv = c01nrv ;
252400100201          atcdco = 0 ;
252500100201          setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
252600100201          If not %equal ;
252700100723           // errMessage  = *on;
252800100723           // errGenerico = *on;
252900100723           // PosCurDta   = *on;
253000100723           // V1Dmsg = $Msg(26);
253100100723              New_Attivita = *on;
253200100723              exsr Gesw04 ;
253300100201              leavesr;
253400100201           endif;
253500100201         ENDIF;
253600100114
253700100315         // Se ci sono appuntamenti a fronte di questa trattativa
253800100315         // con il commerciale diverso da quello della trattativa non si esce
253900100429         IF  %check(digits:c01cmm) > 0 OR C01CMM = *BLANKS OR
254000100429            C01CMM = *ZEROS ;
254100100429            C01CMM = %EDITC(VISCMM: 'X');
254200100429         ENDIF ;
254300100429
254400100315         clear TNTA45ds;
254500100315         ITA45tip = 'A';
254600100315         ITA45nrv = c01nrv;
254700100315         ITA45cmm = %dec(c01cmm:7:0);
254800100315         tnta45r (tnta45ds);
254900100315         IF  OTA45err <> *blanks  ;
255000100315           errMessage  = *on;
255100100315           errGenerico = *on;
255200100315           PosCurCmm  = *on;
255300100315           V1Dmsg = OTA45msg;
255400100315           leavesr;
255500100315         ENDIF;
255600100315
255700100316         // Se ci sono attività aperte a fronte di questa trattativa chiusa
255800100316         // messaggio di errore
255900100316
256000100316         IF  convalidato and ita88fpr <> 'A' ;
256100100316          atccpo = ch1cpo ;
256200100316          atcksc = c01ksc ;
256300100316          atcnrv = c01nrv ;
256400100316          atcdco = 0 ;
256500100316          setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
256600100316          If  %equal ;
256700100723              exsr gesw03 ;
256800100723              leavesr;
256900100316           endif;
257000100316         ENDIF;
257100100316
257200110104
257300110104         // Se tipo trattativa con offerte e senza info ma obbligatorie
257400110104         // richiamo il TNTA41R
257500110311         // Oppure se ho gestito un'attività ed ho chiuso la trattativa
257600110104
257700110114         IF  %lookup(vistpv:infttr) > 0 and visinfot = ' ' and
257800150302             visffz = ' ' and ($pres_offerte = *on or
257900150302             VISTPV = 'A') and
258000110311             (ita88fle <> '5' or $vengo_da_gest);
258100110104             exsr f15s01 ;
258200110111             If visinfot <> 'S' ;
258300110111                leavesr ;
258400110111             Endif ;
258500110104         endif ;
258600110314
258700110314        //?Se trattativa senza offerte, CALDA e senza INFO, ma le INFO
258800110314        //?sono previste, obbligo l'immissione delle INFO
258900110314         IF  %lookup(VIStpv:infttr) > 0 and not $pres_offerte and
259000110314             caldo and ITA88fle <> '5' and VISffz = *blanks and
259100110314             VISinfot = *blanks;
259200110314             exsr F15s01;
259300110314             IF  VISinfot <> 'S';
259400110314               leavesr;
259500110314             ENDIF;
259600110314         ENDIF;
259700110314
259800100114         // Ritorno al programma chiamante
259900100114         $fine  = *on ;
260000100111
260100100111       ENDSR;
260200080206
260300080207       //--------------------------------------------------------------
260400080207       //?Gestione tasto funzionale F12 da videata S01
260500080207       //?F12=Ritorno
260600080207       //--------------------------------------------------------------
260700080207       BEGSR F12S01;
260800080207
260900091123         // verificare che cosa si è creato fino ad ora ed eventualmente
261000091123         // deletarli
261100091123
261200091123         If $scritta = *off and (ita88fle =  'P'  or ita88fle = 'C') ;
261300100416            exsr SR_Deltutto ;
261400091123         // Ritorno al programma chiamante
261500091127         ota88nrv = 0 ;
261600091127         ota88f12 = 'S';
261700091123         $fine  = *on ;
261800091123         leavesr ;
261900091123         endif ;
262000091123
262100091016         // se il flag $scritta = * on vuol dire che ho già scritto
262200091016         // quindi non valorizzo il flag  del f12 e metto il numero visita
262300091016
262400091109         // solo in caso di immissione se dato f12 deleto la rubrica
262500091109
262600091109         If ita88fle =  'P'  or ita88fle = 'C' ;
262700091109
262800091016         If $scritta = *on ;
262900091016            ota88nrv = C01nrv ;
263000091016            ota88f12 = ' ' ;
263100091016         else ;
263200091016            ota88nrv = 0 ;
263300091016            ota88f12 = 'S';
263400091016         endif ;
263500090807
263600091109         endif ;
263700091109
263800091123         // verifico se ci sono delle attività aperte a fronte di questa
263900091123         // trattativa altrimenti errore
264000091123
264100100419         IF  not visualizza and c01ffz = ' ' ;
264200100201           atccpo = ch1cpo ;
264300100201           atcksc = c01ksc ;
264400100201           atcnrv = c01nrv ;
264500100201           atcdco = 0 ;
264600100201           setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
264700100201           If not %equal ;
264800100723            // errMessage  = *on;
264900100723            // errGenerico = *on;
265000100723            // PosCurDta   = *on;
265100100723            // V1Dmsg = $Msg(26);
265200100114               New_Attivita = *on;
265300100723               exsr Gesw04 ;
265400091123               leavesr;
265500100201           endif;
265600100201         ENDIF;
265700091123
265800091016         // Ritorno al programma chiamante
265900090723         $fine  = *on ;
266000080207
266100080207       ENDSR;
266200090810
266300091123       //--------------------------------------------------------------
266400091123       //?Gestione pulizia degli archivi legati alla visita
266500091123       //?
266600091123       //--------------------------------------------------------------
266700091123       BEGSR Sr_DelTutto ;
266800091123
266900091123       // verifico se esiste la trattativa all'interno del file TIVIS
267000091123       chain(n) c01nrv tivis05l ;
267100091123       if not %found(tivis05l) ;
267200091123         // Devo deletare le note
267300091123            NtcApl = 'T'  ;
267400091123            ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
267500091123            setll    (ntcapl:ntcnk1:ntcnk2) tfntc01l;
267600091123            reade    (ntcapl:ntcnk1:ntcnk2) tfntc01l;
267700091123            dow not %eof(tfntc01l) ;
267800091123                delete tfntc ;
267900091123                reade    (ntcapl:ntcnk1:ntcnk2) tfntc01l;
268000091123            Enddo ;
268100091123         // Devo deletare le anagrafiche provvisorie
268200091123             acokut = 1 ;
268300091123             acokcc = dutkci ;
268400091123             acoksc = c01nrv ;
268500091123             delete   (acokut:acokcc:acoksc) Tfaco00f ;
268600091123             delete   (acokut:acokcc:acoksc) Tfind00f ;
268700091123             delete   (acokut:acokcc:acoksc) Tfclp00f ;
268800091123             delete   acoksc Tfcls01l ;
268900100914             setll acoksc Tfcba01l ;
269000100914             reade acoksc Tfcba01l ;
269100100914             dow not %eof(tfcba01l);
269200100914                 delete tfcba01l ;
269300100914                 reade acoksc Tfcba01l ;
269400100914             enddo ;
269500100914
269600091123       endif ;
269700100304
269800100304         // Devo deletare le info trattativa a fronte di questa visita
269900110111         exsr Del_info ;
270000091123
270100100304         // Devo deletare le note legate alla trattativa
270200100304
270300100304         cpncpo = ch1cpo ;
270400100304         cpnksc = c01ksc ;
270500100304         cpnnrv = c01nrv ;
270600100304         data_iso = (%date()) ;
270700100304         cpndim = %dec(data_iso) ;
270800100304         setll (cpncpo:cpnksc:cpnnrv:cpndim) ticpn04l ;
270900100304         reade (cpncpo:cpnksc:cpnnrv:cpndim) ticpn04l ;
271000100304         dow not %eof(ticpn04l);
271100100304             delete ticpn4;
271200100304             reade (cpncpo:cpnksc:cpnnrv:cpndim) ticpn04l ;
271300100304         enddo ;
271400100304
271500100304
271600091123       ENDSR;
271700091123
271800100714       //--------------------------------------------------------------
271900100714       //?Deleto  le attività legata alla trattativa
272000100714       //--------------------------------------------------------------
272100100714       BEGSR sr_DelAtc   ;
272200100714
272300100714
272400100714         atccpo = ch1cpo ;
272500100714         atcksc = c01ksc ;
272600100714         atcnrv = c01nrv ;
272700100714         atcdco = 0 ;
272800100714         setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
272900100714         reade (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
273000100714         dow not %eof(tiatc04l);
273100100714
273200100714                  delete  tiatc000 ;
273300100714       // cancello eventuali legami attività
273400100714                  delete (atcatn:atcatnp) tiatl01l ;
273500100714       // se l'attività era un progressivo 1   vado a cancellare le note
273600100714       // con progressivo 0
273700100714                  If atcatnp = 1 ;
273800100714                     exsr Sr_DelNote ;
273900100714                  endif ;
274000100714             reade   (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
274100100714         enddo ;
274200100714
274300100714       ENDSR ;
274400100304       //--------------------------------------------------------------
274500100304       //?Deleto  le note legate alle attività ancora da eseguire !!!!!
274600100304       //--------------------------------------------------------------
274700100304       BEGSR sr_DelNote;
274800100304
274900100304       // Deleto le sole note relative ad una attività da fare con progressivo
275000100304       // 1
275100100304
275200100304         cpncpo = atccpo ;
275300100304         cpntat = atctat ;
275400100304         cpnatn = atcatn ;
275500100304         cpnatnp= 0 ;
275600100304         setll (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
275700100304         reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
275800100304         dow not %eof(ticpn03l);
275900100304             delete ticpn000;
276000100304             reade (cpncpo:cpntat:cpnatn:cpnatnp) ticpn03l ;
276100100304         enddo ;
276200100304
276300100304       ENDSR ;
276400100304
276500090813       //--------------------------------------------------------------
276600090813       //?Gestione tasto funzionale F12 da videata S02
276700090813       //?F12=Ritorno
276800090813       //--------------------------------------------------------------
276900090813       BEGSR F12S02;
277000090813
277100091027         // Ritorno alla videata s01  e ricarico il subfile
277200090813         $video = 'S1';
277300091027         $inzs01 = *on ;
277400090917         // riaggancio il file tivis dopo F06
277500100315         // chain    c01nrv   tivis05l ;
277600090813
277700090813       ENDSR;
277800150126
277900150126       //--------------------------------------------------------------
278000150126       //?Gestione tasto funzionale F04 da videata S01                 ?
278100150126       //?F4=Campagna                                                  ?
278200150126       //--------------------------------------------------------------
278300150126       BEGSR F04S01;
278400150806
278500150807         clear TRKC09DS;
278600150807         IKC09ksu = C01ksc;
278700150807         trkc09r (kpjba:TRKC09DS);
278800150126
278900150126       ENDSR;
279000110112
279100110112       //--------------------------------------------------------------
279200110112       //?Gestione tasto funzionale F13 da videata S01
279300110112       //?F13=Gestione Data Caldo
279400110112       //--------------------------------------------------------------
279500110112       BEGSR F13S01;
279600110112
279700110112         clear tnta63ds   ;
279800110112         ITA63fmt  = '3';
279900110118         ITA63fle  = 'M';
280000110112         ITA63nrv  = c01nrv;
280100110112
280200110112         tnta63r (kpjba : TNTA63ds);
280300110118         $inzs01 = *on ;
280400110112
280500110112       ENDSR ;
280600110112
280700090813
280800090810       //--------------------------------------------------------------
280900090810       //?Gestione tasto funzionale F21 da videata S01
281000090810       //?F21=Anagrafica potenziale
281100090810       //--------------------------------------------------------------
281200090810       BEGSR F21S01;
281300080207
281400091104         clear trmk01ds   ;
281500100310       If Ita88fle <> '5' ;
281600091104         mk01k10 ='V'   ;
281700091104         mk01cpo =ch1cpo ;
281800100412         TRMK02c  (kpjba:Trmk01ds);
281900100310       endif ;
282000090810         // Se al ritorno dal pgm c'è errore richiamo il pgm dei potenziali in i
282100100310         If  mk01err <> ' ' or ita88fle = '5';
282200091104             clear trmk01ds   ;
282300091104             mk01k10 ='N'   ;
282400091104             mk01cpo =ch1cpo ;
282500100412             TRMK02c  (kpjba:Trmk01ds);
282600090810         endif ;
282700090810
282800090810       ENDSR ;
282900090810
283000090810       //--------------------------------------------------------------
283100090813       //?Gestione tasto funzionale F02 da videata S01
283200090810       //?F02=Rubrica
283300090810       //--------------------------------------------------------------
283400090810       BEGSR F02S01;
283500090810
283600100304        // verifico se già inserite responsabile trasporti e mail trasporti
283700100304        // se inserite scrivo sia su tipo nota trattativa che cliente
283800100304       // registrazione note
283900100304       // prima le deleto se esistono e poi le riscrivo
284000100304
284100100304         if c01ksc = 0 ;
284200100304           ntcapl = 'T' ;
284300100304           ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
284400100304         else ;
284500100304           ntcapl = 'C' ;
284600100304           ntcnk1 = %editc(dutkci: 'X') + %editc(c01ksc:'X');
284700100304         endif ;
284800100304
284900100304           clear  ntcnk2 ;
285000100304
285100100304       // responsabile trasporti
285200100304           ntctnt = '05';
285300100304
285400100304           setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
285500100304           READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l     ;
285600100304           dow  not %eof(tfntc01l) ;
285700100304               delete tfntc ;
285800100304               READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l     ;
285900100304           enddo ;
286000100304
286100100304           If c01rt1 <> *blanks ;
286200100304              ntcrnt = c01rt1 ;
286300100304              exsr Sr_writenot ;
286400100304           endif ;
286500100304
286600100304           If c01rt2 <> *blanks ;
286700100304              ntcrnt = c01rt2 ;
286800100304              exsr Sr_writenot ;
286900100304           endif ;
287000100304       // mail responsabile trasporti
287100100304           ntctnt = '06';
287200100304           Chain    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l     ;
287300100304           If  %found(tfntc01l) ;
287400100304               delete tfntc ;
287500100304           endif ;
287600100304
287700100304           If c01mrt <> *blanks ;
287800100304              ntcrnt = c01mrt ;
287900100304              exsr Sr_writenot ;
288000100304           endif ;
288100090810         // Richiamo il pgm gestione  contatti TNTA12R
288200091023         // verifico se ci sono delle note già legate alla visita
288300091023         // altriemnti le recupero prima di chiamare il pgm
288400091023
288500091109         if c01ksc = 0 ;
288600091112           ntcapl = 'T' ;
288700091023           ntcnk1 = %editc(dutkci: 'X') + %editc(c01nrv:'X');
288800091023           clear  ntcnk2 ;
288900091023           setll    (ntcapl:ntcnk1:ntcnk2) tfntc01l;
289000091109           READE(n) (ntcapl:ntcnk1:ntcnk2) tfntc01l     ;
289100091023           If  %eof(tfntc01l) ;
289200091023          // se non esistono rubriche legate alla trattative le recupero
289300091023          // prima di richiamare il TNTA12R
289400091023               exsr sr_noteda ;
289500091023           endif ;
289600091023
289700090810         clear tnta12ds   ;
289800091109         ta12apl='T'   ;
289900090810         ta12nrv=c01nrv ;
290000091109         ta12rag=c01rag ;
290100091109         if ita88fle = '5' ;
290200091109            ta12ric = 'V' ;
290300091109         endif ;
290400090810         TNTA12R  (kpjba:TNTA12ds);
290500090810         If ta12err <> ' ';
290600090810            errMessage  = *on;
290700090810            errGenerico = *on;
290800090810            V1Dmsg = ta12msg ;
290900090810            leavesr;
291000090810         endif;
291100090810
291200091109         else ;
291300091109
291400091109         clear tnta12ds   ;
291500091109         if ita88fle = '5' ;
291600091109            ta12ric = 'V' ;
291700091109         endif ;
291800091109         ta12apl='C'   ;
291900091109         ta12ksc=c01ksc ;
292000091109         TNTA12R  (kpjba:TNTA12ds);
292100091109         If ta12err <> ' ';
292200091109            errMessage  = *on;
292300091109            errGenerico = *on;
292400091109            V1Dmsg = ta12msg ;
292500091109            leavesr;
292600091109         endif;
292700091109
292800091109         endif;
292900091109
293000090917         // recupero  responsabili trasporti con numero visita
293100090917         $datnta12 = *on ;
293200090810         exsr Rec_Responsa ;
293300090917         $datnta12 = *off;
293400090810
293500090810       ENDSR;
293600090810
293700090813       //--------------------------------------------------------------
293800090923       //?Gestione tasto funzionale F03 da videata S02
293900090923       //?F03=Fine
294000090813       //--------------------------------------------------------------
294100090923       BEGSR F03S02;
294200090923
294300091124         // verifico se ci sono delle attività aperte a fronte di questa
294400091124         // trattativa altrimenti errore
294500091124
294600100419         IF  not visualizza and c01ffz = ' ' ;
294700100201           atccpo = ch1cpo ;
294800100201           atcksc = c01ksc ;
294900100201           atcnrv = c01nrv ;
295000100201           atcdco = 0 ;
295100100201           setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
295200100201           If not %equal ;
295300100723           //  errMessage  = *on;
295400100723           //  errGenerico = *on;
295500100723           //  V1Dmsg = $Msg(26);
295600100114               New_Attivita = *on;
295700100723               exsr Gesw04 ;
295800091124               leavesr;
295900100201           endif;
296000100201         ENDIF;
296100100316
296200100316         // Se ci sono appuntamenti a fronte di questa trattativa
296300100316         // con il commerciale diverso da quello della trattativa non si esce
296400100316         clear TNTA45ds;
296500100316         ITA45tip = 'A';
296600100316         ITA45nrv = c01nrv;
296700100316         ITA45cmm = %dec(c01cmm:7:0);
296800100316         tnta45r (tnta45ds);
296900100316         IF  OTA45err <> *blanks  ;
297000100316           errMessage  = *on;
297100100316           errGenerico = *on;
297200100316           PosCurCmm  = *on;
297300100316           V1Dmsg = OTA45msg;
297400100316           leavesr;
297500100316         ENDIF;
297600100316
297700100316         // Se ci sono attività aperte a fronte di questa trattativa chiusa
297800100316         // messaggio di errore
297900100316
298000100316         IF  convalidato and ita88fpr <> 'A' ;
298100100316          atccpo = ch1cpo ;
298200100316          atcksc = c01ksc ;
298300100316          atcnrv = c01nrv ;
298400100316          atcdco = 0 ;
298500100316          setll (atccpo:atcksc:atcnrv:atcdco) tiatc04l ;
298600100316          If  %equal ;
298700100723              exsr Gesw03 ;
298800100316              leavesr;
298900100316           endif;
299000100316         ENDIF;
299100091123
299200091016         // se il flag $scritta = * on vuol dire che ho già scritto
299300091016         // quindi non valorizzo il flag  del f12 e metto il numero visita
299400091016
299500091016         If $scritta = *on ;
299600091016            ota88nrv = VISnrv ;
299700091016            ota88f12 = ' ' ;
299800091016         else ;
299900091016            ota88nrv = 0 ;
300000091016            ota88f12 = 'S';
300100091016         endif ;
300200110104
300300110104         // Se tipo trattativa con offerte e senza info ma obbligatorie
300400110104         // richiamo il TNTA41R
300500110104
300600110329         IF  %lookup(vistpv:infttr) > 0 and visinfot = ' ' and
300700110329             visffz = ' ' and $PRES_offerte = *on and
300800110329             (ita88fle <> '5' or $vengo_da_gest);
300900110104             exsr f15s01 ;
301000110111             If visinfot <> 'S' ;
301100110111                leavesr ;
301200110111             Endif ;
301300110104         endif ;
301400110314
301500110314        //?Se trattativa senza offerte, CALDA e senza INFO, ma le INFO
301600110314        //?sono previste, obbligo l'immissione delle INFO
301700110314         IF  %lookup(VIStpv:infttr) > 0 and not offerte and
301800110314             caldo and VISinfot = *blanks and VISffz = *blanks;
301900110314             exsr F15s01;
302000110314             IF  VISinfot <> 'S';
302100110314               leavesr;
302200110314             ENDIF;
302300110314         ENDIF;
302400110104
302500090923         // Chiusura del programma
302600090923         $Fine = *on;
302700090813
302800090813       ENDSR;
302900090813
303000090813       //--------------------------------------------------------------
303100090813       //?Gestione tasto funzionale F08 da videata S02
303200090813       //?F08=Note Tariffe
303300090813       //--------------------------------------------------------------
303400090813       BEGSR F08S02;
303500090813
303600090813         // Richiamo il pgm gestione note tariffe/offerte
303700090813         clear tnta28ds ;
303800090813
303900090915         If ita88fle = '5' ;
304000090915            ita28ela = 'I' ;
304100090915         else ;
304200090915            ita28ela = 'M' ;
304300090915         endif;
304400090813         ita28tip = 'T' ;
304500090813         ita28cod = c01nrv ;
304600090813         ita28rsc = c01rag ;
304700090813         clear kpjbu ;
304800090813         kpjbu = tnta28ds ;
304900090813         tnta28r (kpjba) ;
305000090813
305100090813       ENDSR;
305200090810       //--------------------------------------------------------------
305300090810       //?Gestione tasto funzionale F18 da videata S01
305400090810       //?F18=Note
305500090810       //--------------------------------------------------------------
305600090810       BEGSR F18S01;
305700090810
305800090810         // Richiamo il pgm gestione note TRMK20R
305900090810         clear trmk20ds ;
306000110511         imk20tla = 'L' ;
306100090810         if Ita88fle = '5' ;
306200090810            imk20flm = 'I';
306300090810         else ;
306400100115            imk20flm = 'V';
306500090810         endif ;
306600090810         imk20cpo = ch1cpo ;
306700090810         imk20ksc = c01ksc ;
306800090810         imk20nrv = c01nrv ;
306900090810         imk20rsc = c01rag ;
307000090810         Trmk20r ( kpjba : trmk20ds );
307100090810
307200090810       ENDSR;
307300090810
307400090813       //--------------------------------------------------------------
307500090813       //?Gestione tasto funzionale F18 da videata S02
307600090813       //?F18=Note
307700090813       //--------------------------------------------------------------
307800090813       BEGSR F18S02;
307900090813
308000090813         // Richiamo la routine F18S01 della prima videata
308100090813         exsr F18S01 ;
308200090813
308300090813       ENDSR;
308400090813
308500090810       //--------------------------------------------------------------
308600090810       //?Gestione tasto funzionale F14 da videata S01
308700090810       //?F14=Informazioni Commerciali
308800090810       //--------------------------------------------------------------
308900090810       BEGSR F14S01;
309000090810
309100090810         // Richiamo il pgm informazioni Commerciali
309200090810         clear     trmk50ds   ;
309300090810         i50cpo=ch1cpo ;
309400100310         If ita88fle <> '5' ;
309500100310            i50mod='G'   ;
309600100310         else ;
309700100310            i50mod='I'   ;
309800100310         endif;
309900090810         i50rag=c01rag   ;
310000090812         i50ifotot = §cpoifotot ;
310100090810         i50pgm='TNTA88R' ;
310200100924         i50obl = 'S' ;
310300090812
310400090810         TRMK50R  (kpjba:trmk50ds) ;
310500090810
310600090810       ENDSR;
310700090810
310800100303       //--------------------------------------------------------------
310900100303       //?Gestione tasto funzionale F15 da videata S01
311000100303       //?F15=Informazioni trattativa
311100100303       //--------------------------------------------------------------
311200100303       BEGSR F15S01;
311300100303
311400100303         // Richiamo il pgm informazioni Trattativa
311500100303         clear     tnta41ds   ;
311600100303         i41nrv=c01nrv ;
311700100303         i41rag=c01rag   ;
311800110617         If ita88fle <> '5' or ($vengo_da_Gest and $Ges_chiusa = *off);
311900100310            i41mod='G'      ;
312000100310         else ;
312100100310            i41mod='I'   ;
312200100310         endif;
312300100303         i41ifotot = visinfot   ;
312400101221         I41tpv = c01tpv;
312500100303
312600100303         Tnta41R  (kpjba:tnta41ds) ;
312700100304         // mi salvo il campo delle infotrattative
312800100304         If o41f06 = 'S' ;
312900100304            $InfoTrat = *on ;
313000100304            sav_ifotot = o41ifotot ;
313100100304         endif ;
313200100709       // Devo aggiornare il flag delle info all'interno della visita
313300100709        If $infotrat = *on ;
313400100709           chain c01nrv tivis05l ;
313500100709           If %found(tivis05l) ;
313600100709              visinfot = sav_ifotot ;
313700100709              update tivis000 ;
313800100709           endif ;
313900100709        endif ;
314000100304
314100100303       ENDSR;
314200090813       //--------------------------------------------------------------
314300090813       //?Gestione tasto funzionale F14 da videata S02
314400090813       //?F14=Informazioni Commerciali
314500090813       //--------------------------------------------------------------
314600090813       BEGSR F14S02;
314700090813
314800090813       // richiamo la routine F14S01 della videata S01
314900090813        exsr F14S01 ;
315000090813
315100090813       ENDSR ;
315200090810
315300100304       //--------------------------------------------------------------
315400100304       //?Gestione tasto funzionale F15 da videata S02
315500100304       //?F15=Informazioni Trattativa
315600100304       //--------------------------------------------------------------
315700100304       BEGSR F15S02;
315800100304
315900100305        $infotrat = *off;
316000100304       // richiamo la routine F15S01 della videata S01
316100101223        If  %lookup(c01tpv:infttr) = 0 ;
316200101223            errMessage  = *on;
316300101223            errGenerico = *on;
316400101223            V1Dmsg = $Msg(40);
316500101223            leavesr;
316600101223        endif;
316700100304        exsr F15S01 ;
316800100304
316900100304       ENDSR ;
317000100304
317100080207       //--------------------------------------------------------------
317200080207       //?Gestione opzioni subfile
317300080207       //--------------------------------------------------------------
317400080207       BEGSR OpzS01;
317500080207
317600090723         readc ta88S01;
317700080207
317800090723         DOW  NOT  %eof(TNTA88D);
317900080207
318000090723           SflNxtChg1= *off;
318100080207           WindDspF  = IndDspF;
318200080207           reset WindDspFb;
318300080207           IndDspF   = WindDspF;
318400090723           C01rcd    = S01nrr;
318500100721
318600100721             // verifico se nel frattempo è stata eseguita da altro utente
318700100721                If   Sh1fatto = ' ';
318800100722                   chain (s01tat:sh1atn:sh1atnp) tiatc01l ;
318900100721                   If %found(tiatc01l) and atcdco > 0 ;
319000100721                      sh1fatto = 'S' ;
319100100721                   endif ;
319200100721                endif ;
319300080207
319400080207           SELECT;
319500080207
319600080207             // - Nessuna opzione
319700080208             WHEN  S01opz  = *blank;
319800100115
319900100115             // - Controllo se inibita
320000100115             WHEN  S01opz <> *blank and sh1opz <> *blanks and
320100100115               %scan(s01opz:sh1opz) > 0 ;
320200100115               errMessage  = *on;
320300100115               errGenerico = *on;
320400100115               PosCurOpz1  = *on;
320500100115               V1Dmsg = $Msg(27);
320600080208
320700080208             // - Precedente segnalazione di errore
320800080208             WHEN  S01opz  = 'E';
320900080208               clear S01opz;
321000090514
321100090514             // - 2 = Manutenzione
321200090514             WHEN  S01opz  = '2';
321300090514
321400100205             // verfiico se provengo da pgm attività do errore oppure
321500100205             // in visualizzazione
321600100317             If Da_Attivita = *on or Da_potenziale or (visualizza  and
321700100317                c01dch = 0) ;
321800091022                errMessage  = *on;
321900091022                errGenerico = *on;
322000091022                PosCurOpz1  = *on;
322100091022                V1Dmsg = $Msg(10);
322200091022             else ;
322300090514             // verifico se imposto 2 per un contatto già eseguito
322400090514                If   Sh1fatto = 'S';
322500090514                     errMessage  = *on;
322600090514                     errGenerico = *on;
322700090723                     PosCurOpz1  = *on;
322800090609                     V1Dmsg = $Msg(09);
322900090514                else ;
323000090611                     exsr Calltrmk19  ;
323100100722                     If errMessage = *off;
323200100722                        $inzs01 = *on ;
323300100722                     endif ;
323400090514                endif;
323500091022             endif;
323600090514
323700091124             // - G = Gestione
323800091124             WHEN  S01opz  = 'G';
323900091124
324000100909             // verifico se trattativa fittizia posso solo annullare
324100100909                If   c01ffz   = 'S';
324200100909                     errMessage  = *on;
324300100909                     errGenerico = *on;
324400100909                     PosCurOpz1  = *on;
324500100909                     V1Dmsg = $Msg(39);
324600100909                else ;
324700100205             // verifico se provengo da pgm attività oppure
324800100205             // visualizzazione do errore
324900100909                If Da_Attivita  or Da_potenziale or visualizza  ;
325000100909                   errMessage  = *on;
325100100909                   errGenerico = *on;
325200100909                   PosCurOpz1  = *on;
325300100909                   V1Dmsg = $Msg(10);
325400100909                else ;
325500091124             // verifico se imposto G per un contatto già eseguito
325600100909                   If   Sh1fatto = 'S';
325700100909                        errMessage  = *on;
325800100909                        errGenerico = *on;
325900100909                        PosCurOpz1  = *on;
326000100909                        V1Dmsg = $Msg(09);
326100100909                   else ;
326200100909                        exsr Calltrmk19  ;
326300100909                        If errMessage = *off;
326400100909                           $inzs01 = *on ;
326500100909                        endif;
326600100909                   endif;
326700100909                endif;
326800100909        endif;
326900091124
327000090514             // - 5 = Visualizzazione
327100090514             WHEN  S01opz  = '5';
327200091026             // verfiico se provengo da pgm attività do errore
327300091029             If Da_Attivita = *on or da_potenziale = *on ;
327400091026                errMessage  = *on;
327500091026                errGenerico = *on;
327600091026                PosCurOpz1  = *on;
327700091026                V1Dmsg = $Msg(10);
327800091026             else ;
327900090611                exsr Calltrmk19  ;
328000091014                $inzs01 = *on ;
328100091026             endif;
328200090514
328300080207             // - ? = Opzione NON valida
328400080207             OTHER;
328500080207               errMessage  = *on;
328600080207               errGenerico = *on;
328700090723               PosCurOpz1  = *on;
328800090609               V1Dmsg = $Msg(10);
328900080207
329000080207           ENDSL;
329100080207
329200080207           // Aggiornamento sfl
329300080207           select;
329400080207             when  errMessage  = *on;
329500090723               SflNxtChg1= *on;
329600080207               C01csr    = C01rcd;
329700080207             when  errGenerico = *on;
329800080208               C01csr    = C01rcd;
329900080208               clear  S01opz;
330000080207             other;
330100080207               C01csr    = C01rcd;
330200080208               clear  S01opz;
330300080207           endsl;
330400080207
330500090722           coldafare = (sh1fatto= ' ' );
330600090723           update TA88S01;
330700080207
330800080208           if  errMessage = *on  or  errGenerico = *on;
330900080207             leave;
331000080207           endif;
331100080207
331200090722           readc TA88S01;
331300080207
331400080207         ENDDO;
331500080207
331600080207       ENDSR;
331700080207
331800090611       //--------------------------------------------------------------
331900090611       //?Richiamo trmk19r Gestione dettaglio contatti
332000090611       //--------------------------------------------------------------
332100090611       BEGSR Calltrmk19 ;
332200090611
332300090804                     clear trmk19ds ;
332400090804                     imk19fle = s01opz ;
332500091105                     imk19fpr = 'T' ;
332600090804                     imk19cmt = 'S' ;
332700090804                     imk19fatt  = sh1fatto ;
332800090804                     imk19tco = s01tat ;
332900090804                     imk19cpo = sh1cpo ;
333000090804                     imk19tat = s01tat ;
333100090804                     imk19atn = sh1atn ;
333200090804                     imk19atnp= sh1atnp;
333300091124        // propongo commerciale che ha in gestione la trattativa
333400100607        // se non ho il chi sono
333500100607                     If ita88com = 0 ;
333600100607                        imk19com = %dec(c01cmm:7:0);
333700100607                        imk19comd= c01cmmd ;
333800100607                     else ;
333900100607                        imk19com = ita88com ;
334000100607                        imk19comd= ita88comd ;
334100100607                     endif ;
334200090804
334300100126                     trmk19r (kpjba : trmk19ds) ;
334400090611
334500100722              // se ritorna l'errore lo emetto a video
334600100722                     If omk19err =  'E' ;
334700100722                        errMessage  = *on;
334800100722                        errGenerico = *on;
334900100722                        PosCurOpz1  = *on;
335000100722                        V1Dmsg = omk19msg;
335100100722                     endif;
335200100909                     chain(n) c01nrv tivis05l ;
335300090611       endsr;
335400090811
335500090811       //--------------------------------------------------------------
335600090811       //?Gestione tasto funzionale F06 da videata S01
335700090811       //?F06=Conferma visita
335800090811       //--------------------------------------------------------------
335900090811       BEGSR F06S01;
336000090811
336100100315       // aggancio il file tivis
336200100315            chain    c01nrv   tivis05l ;
336300110112       // verifico se nuovo tipo trattativa non necessita di info le cancello
336400110104              if %lookup(c01tpv:infttr) = 0 and
336500110104                 visinfot = 'S' ;
336600110112                 exsr Del_info ;
336700110104              endif;
336800110111
336900110111       // Memorizzo il vecchio tipo trattativa se la prima volta che lo cambio
337000110104              dvis01 = visflo ;
337100110328              If §visextpv = ' ' and vistpv <> c01tpv ;
337200110104                 §visextpv = vistpv ;
337300110104                 visflo = dvis01 ;
337400110104              endif;
337500110112
337600090811       // confermo i dati nel file visita
337700090811         viscmmi = %dec(c01cmmi:7:0) ;
337800090811         viscmm  = %dec(c01cmm:7:0)  ;
337900090811         visdat  = w01dta  ;
338000090811         vistpv  = c01tpv  ;
338100100319         if ch1cpo > 0 ;
338200100319            viscpo  = ch1cpo  ;
338300100319         endif ;
338400090811         visksc  = c01ksc  ;
338500090811         If ita88fle = 'P' ;
338600090811            viscnw = 'S' ;
338700100414         else ;
338800100414            viscnw = 'N' ;
338900090811         endif ;
339000090811         visfil = %dec(%subst(%editc(viscmm: 'X'):1:3):3:0) ;
339100121017        // se visita da vera a fittizia e l'ho appena creata
339200100419         If ita88fle = 'G' and $scritta = *on and visffz <> c01ffz and
339300100419            c01ffz <> ' ' ;
339400100420        // se scritto primocontatto lo cancello
339500100420            If $Primo_atc = *on ;
339600100420               exec sql delete from tiatc00f where atccpo =:viscpo
339700100420               and atcnra =:visnrv and atccac = 'AT' ;
339800100420            endif ;
339900120622         //?Se scritta la data primo contatto la tolgo
340000120622            IF  wDataPrimoCon;
340100120622              clear dCPO01;
340200120622              chain(e) CPOcpo TNCPO01L;
340300120622              IF  not %error and %found(TNCPO01L);
340400120622                dCPO01 = CPOrst;
340500120622                clear §CPOdtpcon;
340600120622                CPOrst = dCPO01;
340700120622                update TNCPO000;
340800120622              ENDIF;
340900120622            ENDIF;
341000120622
341100100419         endif ;
341200100419
341300090811         visffz = c01ffz ;
341400090811         visrag = c01rag ;
341500100304
341600100309        If $infotrat = *on ;
341700100309           visinfot = sav_ifotot ;
341800100309        endif ;
341900090811
342000090811        // Aggiornamento
342100090811
342200100315        If ita88fle = 'G' and %found(tivis05l) ;
342300090811            update tivis000 ;
342400090811         endif;
342500090811
342600090811        // Scrittura
342700090811
342800100303         If ita88fle = 'C' or ita88fle = 'P' ;
342900090917        // se ho già scritto faccio update altrimenti faccio write
343000100303            If $scritta = *off ;
343100120622               clear dCPO01;
343200100315               visnrv = c01nrv ;
343300110211               dvis01 = visflo ;
343400110211               chain(n) viscpo tncpo01l ;
343500110211               If %found(tncpo01l) ;
343600110211                  §viscatpo = cpofls ;
343700110401                  visflo = dvis01 ;
343800120622                  dCPO01 = CPOrst;
343900110211               Endif ;
344000090917               write tivis000 ;
344100100325       // se non provengo dai pgm attività e se sto scrivendo una
344200100325       // nuova trattativa scrivo un'attività di apertura trattativa che mi
344300100420       // servirà per i conteggi in statistica se trattativa non fittizia
344400100420               if ita88fpr <> 'A' and visffz = ' ' ;
344500100329                  exsr PrimoContatto ;
344600100329               endif ;
344700100720       // se  provengo dai pgm attività e se sto scrivendo una
344800100720       // nuova trattativa committo i record scritti nel programma precedente
344900100720               If Ita88cmt = 'S' and ita88fpr = 'A' ;
345000100720                  commit ;
345100100720               Endif ;
345200100720
345300090917               $scritta = *on ;
345400091113
345500091113
345600121017        // Imposto la data primo contatto se è mai codificato
345700110502               IF  cpofls = 'M' ;
345800120622
345900120622         //?Imposto data primo contatto sul Potenziale
346000120622                 IF  §CCOdpc <> 'N' and
346100120622                     (§CPOdtpcon = *blanks or §CPOdtpcon = *zeros);
346200120622                   clear dCPO01;
346300120622                   chain(e) CPOcpo TNCPO01L;
346400120622                   IF  not %error and %found(TNCPO01L);
346500120622                     dCPO01 = CPOrst;
346600120622                     §CPOdtpcon = %editc(datacor:'X');
346700120622                     CPOrst = dCPO01;
346800120622                     update TNCPO000;
346900120622                     wDataPrimoCon = *on;
347000120622                   ENDIF;
347100120622                 ENDIF;
347200120622
347300110502               ENDIF;
347400091113
347500100303            else ;
347600090917               update tivis000 ;
347700100303            endif ;
347800100303         endif;
347900150211
348000150211       //?Se cliente in campagna e trattativa ora è fittizzia devo cancellare le
348100150211       //?già scritte per questa trattativa
348200150211         IF  VISffz = 'S';
348300150430           wDelTRffz = *on;
348400150430           exsr FaseCampagna;
348500150211         ENDIF;
348600100115
348700100115         //?Devo aggiornare il commerciale sul potenziale
348800100115         chain  Ch1cpo  TNCPO01L;
348900100115         IF  %found(TNCPO01L)
349000100115             and CpoCmm > 0;
349100100115           unlock TNCPO01L;
349200100115         ENDIF;
349300100115         IF  %found(TNCPO01L)
349400100115             and CpoCmm = 0;
349500160804         //?Salvo l'immagine precedente del potenziale
349600160804           exsr ImmaginePrima;
349700100115           Cpocmm = %dec(C01cmm:7:0);
349800100115           Cpoflt = %dec(%subst(%editc(cpocmm:'X'):1:3):3:0);
349900100115           update TNCPO000;
350000160804         //?Scrivo la variazione fatta sul potenziale
350100160804           exsr ScriviVariazione;
350200100115         ENDIF;
350300090811
350400091109       // registrazione note
350500090811       // prima le deleto se esistono e poi le riscrivo
350600090811
350700091109         if c01ksc = 0 ;
350800091112           ntcapl = 'T' ;
350900090811           ntcnk1 = %editc(dutkci: 'X') + %editc(visnrv:'X');
351000091109         else ;
351100091109           ntcapl = 'C' ;
351200091109           ntcnk1 = %editc(dutkci: 'X') + %editc(c01ksc:'X');
351300091109         endif ;
351400091109
351500090811           clear  ntcnk2 ;
351600090811
351700090811       // responsabile trasporti
351800090811           ntctnt = '05';
351900090811
352000090811           setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
352100090811           READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l     ;
352200090811           dow  not %eof(tfntc01l) ;
352300090811               delete tfntc ;
352400090811               READE    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l     ;
352500090811           enddo ;
352600090811
352700090917           If c01rt1 <> *blanks ;
352800090917              ntcrnt = c01rt1 ;
352900090917              exsr Sr_writenot ;
353000090917           endif ;
353100090811
353200090917           If c01rt2 <> *blanks ;
353300090917              ntcrnt = c01rt2 ;
353400090917              exsr Sr_writenot ;
353500090917           endif ;
353600090811       // mail responsabile trasporti
353700090811           ntctnt = '06';
353800090811           Chain    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l     ;
353900090811           If  %found(tfntc01l) ;
354000090811               delete tfntc ;
354100090811           endif ;
354200090811
354300090917           If c01mrt <> *blanks ;
354400090917              ntcrnt = c01mrt ;
354500090917              exsr Sr_writenot ;
354600090917           endif ;
354700091216         // valorizzo i flag che mi pilotano l'immissione e la
354800091216         // manutenzione
354900150707         // solo nel caso in cui non entro in visualizzazione della trattativa
355000150707         // altrimenti entra in gestione anche di info trattative già chiuse
355100150707          IF  ita88fle <> '5'  ;
355200091216            ita88fle = 'G' ;
355300100111            Nuova_Potenz = *off;
355400100111            Nuova_Client = *off;
355500100111            Manutenzio   = *on;
355600150707          Endif ;
355700150114
355800150114         //?Devo richiedere sempre le info trattative se trattativa
355900150114         //?di tipo Aumento
356000150310          IF  C01tpv = 'A' and not $f15_on ;
356100150114            exsr sr_InfoTra;
356200150114            IF  ErrGenerico;
356300150114              leavesr;
356400150114            ENDIF;
356500150114          ENDIF;
356600090813
356700090811       ENDSR ;
356800090806
356900100304       //--------------------------------------------------------------
357000100304       //?Scrive primo contatto
357100100304       //--------------------------------------------------------------
357200100304       BEGSR PrimoContatto ;
357300100304
357400100304               clear tiatc000 ;
357500100304
357600110211               datc01 = atcflo ;
357700110211
357800100304               RESET trul33ds;
357900100304               i33tla = 'L'  ;
358000100304               i33ope = 0    ;
358100100304               i33Cnu = 070  ;
358200100304               i33num = 1    ;
358300100304               i33Aaa = *YEAR;
358400100304               kpjbu = trul33ds;
358500100304               trul33r(kpjba);
358600100304               trul33ds = kpjbu;
358700100304               If o33Err = 0;
358800100304                  atcatn  = o33Nrf;
358900100304               Endif;
359000100304
359100100304       // imposto a zero il progressivo telefonata
359200100304            atcatnp  = 1 ;
359300100304       // imposto i campi che vengono inseriti solo nel primo contatto
359400100304            atctat = 'T' ;
359500100304            atccpo = ch1cpo ;
359600100304            atcksc = c01ksc ;
359700100304
359800100304            atcest = 'S'    ;
359900100304
360000100304            data_iso = (%date()) ;
360100100304            atcdco = %dec(data_iso) ;
360200100304            atchco = %dec(%subst(%EDITC(%dec(%time()):'X'):1:6):6:0);
360300100304            atccco = %dec(C01cmmi:7:0);
360400100304            atcpru = knmus  ;
360500100304            atccac = 'AT'   ;
360600100304
360700100304            atcnra = newnrv ;
360800100304            atcdim = atcdco  ;
360900100304            atchim = atchco ;
361000100304            atcpri = knmus ;
361100100304            exsr SR_valcnw ;
361200110211            atcflo = datc01 ;
361300100304
361400100304            write tiatc000  ;
361500100420            $Primo_atc = *on ;
361600100304
361700100304        ENDSR ;
361800100304
361900100304       //--------------------------------------------------------------
362000100304       //?Valorizzo il campo del cliente potenziale o codificato
362100100304       //--------------------------------------------------------------
362200100304       BEGSR Sr_ValCnw;
362300100304
362400110502       // Registro l'attività che sto creando in categoria potenziale
362500110502       //
362600110502         chain(n) atccpo tncpo01l ;
362700110502         If %found(tncpo01l) ;
362800110502            §atccapo1 = cpofls ;
362900110502         Endif ;
363000110502
363100110211       // Verifico se l'attività creata è su  potenziale o su cliente codificato
363200100304
363300110502         IF  cpofls = 'C' ;
363400100304             atccnw = 'C';
363500110502         ELSE;
363600100304             atccnw = 'P';
363700110502         ENDIF;
363800110211
363900100304       ENDSR  ;
364000090811       //--------------------------------------------------------------
364100090811       //?Scrivo le note
364200090811       //--------------------------------------------------------------
364300090811       BEGSR sr_Writenot ;
364400090811
364500090811            ntcsns = 'S' ;
364600090811            ntcflt = ' ' ;
364700090811            data_eur =%date(*date:*eur)  ;
364800090811            data_ymd =data_eur;
364900090811            ntcntr=%dec(data_ymd);
365000090811
365100090811            write tfntc;
365200090811
365300090811       ENDSR ;
365400091023
365500091023       //--------------------------------------------------------------
365600091023       //?Recupero altre note dal potenziale / cliente
365700091023       //--------------------------------------------------------------
365800091023       BEGSR sr_noteda  ;
365900091023
366000091023       // Potenziale
366100091023             If  Ita88fle = 'P' ;
366200091103                 Key_ntcapl = 'P' ;
366300091103                 Key_ntcnk1 = %editc(ch1cpo: 'X') ;
366400091023             endif ;
366500091023
366600091109       // Cliente  NO non mi serve
366700091109        //   If  Ita88fle = 'C' ;
366800091109        //       Key_ntcapl = 'C' ;
366900091109        //       Key_ntcnk1 = %editc(dutkci: 'X') +
367000091109        //                              %editc(c01ksc:'X');
367100091109        //   endif ;
367200091023
367300091109             setll  (Key_ntcapl:Key_ntcnk1) tfntc01l;
367400091109             reade(n) (key_ntcapl:Key_ntcnk1) tfntc01l;
367500091023
367600091103             dow not %eof(tfntc01l) ;
367700091023
367800091103             If Ntcflt <> 'A' ;
367900130807                xx = %lookup(ntctnt:d1t);
368000091103                If xx > 0  and dpot(xx) = 'S';
368100091112                       NtcApl = 'T'  ;
368200091023                       ntcnk1 = %editc(dutkci: 'X') + %editc(C01nrv:'X');
368300091103                       exsr sr_writenot ;
368400091103                EndIf ;
368500091103             EndIf ;
368600091023
368700091103             reade(n) (key_ntcapl:Key_ntcnk1) tfntc01l;
368800091023
368900091103             enddo;
369000091023
369100091023       ENDSR ;
369200090811
369300090812       //--------------------------------------------------------------
369400121211       //?Verifico se esistono le info commerciali
369500090812       //--------------------------------------------------------------
369600090812       BEGSR sr_InfoCom  ;
369700090812
369800090812        // aggancio potenziale
369900090812
370000090812               chain(n) ch1cpo tncpo01l ;
370100090812               dcpo01=cporst ;
370200090812
370300090812               clear s_dcpo01 ;
370400090812
370500090812
370600090812        // se non è stato digitato f14
370700090812        // - F14=Informazioni commerciali
370800090812           if    dsp_aid = c_F14;
370900090812                 exsr F14S01 ;
371000090812
371100090812           else ;
371200121211        // E' obbligatorio inserire info commerciali
371300121211        // solo se non solo già complete
371400121211               if Info_Obb and §cpoifotot<>'S';
371500090812                  errMessage  = *on;
371600090812                  errGenerico = *on;
371700090812                  V1Dmsg = $Msg(16);
371800090812                  leavesr ;
371900090812               endif;
372000090812           endif;
372100090812
372200090812       ENDSR ;
372300090812
372400100305       //--------------------------------------------------------------
372500100305       //?Verifico se esistono le info trattativa
372600100305       //--------------------------------------------------------------
372700100305       BEGSR sr_InfoTra  ;
372800100305
372900100305        // aggancio visita
373000101223
373100101223        // se tipo visita prevede le info e non ci sono prevedo errore
373200100305
373300100305               chain(n) c01nrv tivis05l ;
373400100305               If %found(tivis05l) and visffz = ' ' and
373500100305                  visinfot <> 'S';
373600101223        // verifico il tipo visita
373700101223                  if %lookup(vistpv:infttr) >0 ;
373800101223                     errMessage  = *on;
373900101223                     errGenerico = *on;
374000101223                     V1Dmsg = $Msg(30);
374100101223                  Endif ;
374200100305               endif ;
374300100305
374400100305       ENDSR ;
374500090812
374600090812       //--------------------------------------------------------------
374700090812       //?Gestione videata S02
374800090812       //--------------------------------------------------------------
374900090812       BEGSR GesS02;
375000090812
375100090812         // Inizializzazione videata
375200090812         if  $InzS02 = *on;
375300090812            exsr InzS02;
375400090812            $InzS02  = *off;
375500090812         endif;
375600090812
375700090812         // Posizionamento cursore
375800090812         if  C02csr  > *zero;
375900090812           C02rcd = C02csr;
376000090812         endif;
376100090812
376200090812         // Emissione Testata e Piede con tasti funzionali abilitati
376300090812           write  TA88T01;
376400090812           write  TA88P02;
376500090812
376600090812
376700090812         // Emissione videata
376800090812         exfmt  TA88C02;
376900090812         reset errMessage;
377000090812         reset errGenerico;
377100090812         clear V1Dmsg;
377200090812
377300090812         SELECT;
377400100723
377500090812         // - F12=Ritorno
377600090812           WHEN  dsp_aid = c_F12;
377700090812             exsr F12S02;
377800090812
377900100304         // - F14=Info Commerciali
378000100304           WHEN  dsp_aid = c_F14;
378100100304             exsr F14S02;
378200100304
378300100304         // - F15=INfo trattativa
378400100304           WHEN  dsp_aid = c_F15;
378500101223             if  errGenerico = *on;
378600101223               leavesr;
378700101223             endif;
378800100304             exsr F15S02;
378900100304
379000090812         // - F18=Note
379100090812           WHEN  dsp_aid = c_F18;
379200090812             exsr F18S02 ;
379300090812
379400090923         // - F03=Fine Pgm
379500090923           WHEN  dsp_aid = c_F03;
379600090923             exsr F03S02 ;
379700100115
379800100115         // - F10=Inserisce nuove attività
379900100115           WHEN  dsp_aid = c_F10;
380000100115             exsr F10S01;
380100090813
380200090813         // - F08=Note tariffe
380300090813           WHEN  dsp_aid = c_F08;
380400090813             exsr F08S02 ;
380500090910             $InzS02  = *oN;
380600090812
380700090812         // - Roll-Up
380800090812           WHEN  dsp_aid = c_RollUp;
380900090812
381000090812         // Invio
381100090812           OTHER;
381200090813           If ita88fle <> '5' ;
381300090813         // verifico i campi immessi  se non sono in visualizzazione
381400090813             exsr CtrC02 ;
381500090813             if  errGenerico = *on;
381600090813               leavesr;
381700090813             endif;
381800090813           endif ;
381900090812         // leggo il subfile solo se ho dei records
382000090812             If s02nrr > *zeros ;
382100090916                exsr OpzS02;
382200090812                if  errGenerico = *on;
382300090812                    leavesr;
382400090812                endif;
382500090812             endif;
382600090916             $InzS02  = *oN;
382700090812
382800090812         ENDSL;
382900090812
383000090812       ENDSR;
383100090812
383200090812       //--------------------------------------------------------------
383300090812       //?Inizializzazione videata S02
383400090812       //--------------------------------------------------------------
383500090812       BEGSR InzS02;
383600090917
383700090917       // Pulizia control
383800090917         clear  c02ctr ;
383900090917         clear  c02prg ;
384000090917         clear  c02cop ;
384100090917         clear  c02tto ;
384200090812
384300090812       // Pulizia subfile
384400090812         exsr pulsfl2;
384500090812
384600090812       // Caricamento dei dati da presentare nel sfl
384700090812         $EoF = *off;
384800090812         //  Preparo la stringa sql
384900090812         stringasql  = 'select * from TIVOF00F +
385000100421                           where vofnrv = ' + %editc(c01nrv:'X')
385100100421                            + ' and vofeso <> '
385200100421                            + ''''
385300100421                            + '*'
385400110112                            + '''' + ' and vofctr < 999' ;
385500090812
385600090812         stringasql = %trim(stringasql) + ' order by vofctr, vofprg ';
385700090812
385800090812
385900090812         stringasql = %trim(stringaSql) + ' for fetch only';
386000090812
386100090812
386200090812         exec sql prepare S3 from :StringaSql;
386300090812
386400090812         exec sql declare C3 cursor for S3;
386500090812
386600090812         exec sql open C3 ;
386700090812
386800090812         exsr sr_ReadRec2;
386900090812
387000090813         dow  $EoF   = *off ;
387100090812           exsr RollUpS02;
387200090812         enddo;
387300090812
387400090812         exec sql close C3 ;
387500090812
387600090812       // Impaginazione subfile
387700090812       // -> forza l'impaginazione sul 1° rec. del subfile
387800090812         if S02nrr > *zero;
387900090812           C02rcd  = 1;
388000090812           C02csr  = 1;
388100090812         else;
388200090812           clear C02rcd;
388300090812         endif;
388400110113
388500110113       // in presenta di una trattativa di tipo "I" ed in presenza di offerte
388600110113       // 999 ed attività 44 cancello tutto
388700110113         If c01tpv = 'I' and s02nrr > *zero ;
388800110113            exsr Sr_DelCaldo    ;
388900110113         Endif ;
389000090812       ENDSR;
389100090812
389200090812       //--------------------------------------------------------------
389300090812       //?Pulizia SFL02
389400090812       //--------------------------------------------------------------
389500090812       BEGSR Pulsfl2;
389600090812
389700090812       // Pulizia subfile
389800090812         SflDsp_N2   = *on;
389900090812         SflDspCtl_N2= *on;
390000090812         write  TA88C02;
390100090812         SflDspCtl_N2= *off;
390200090812         SflEnd2     = *off;
390300090812
390400090812
390500090812         clear W_SflPag2;
390600090812         clear C02rcd;
390700090812         clear C02csr;
390800090812         clear S02nrr;
390900090812         clear V1Dmsg;
391000090812         errMessage  = *off;
391100090812         errGenerico = *off;
391200090812         WindDspF    = IndDspF;
391300090812         reset WindDspFb;
391400090812         IndDspF     = WindDspF;
391500090812
391600090812       ENDSR ;
391700090812
391800090812       //--------------------------------------------------------------
391900090812       //?Lettura record successivo  sf2
392000090812       //--------------------------------------------------------------
392100090812       BEGSR sr_ReadRec2;
392200090812
392300090812         $RecOK  = *off;
392400090812
392500090812         exec sql Fetch C3 into :tivofds;
392600090812
392700090812         if  Sqlcod = 100 or Sqlcod < 0 ;
392800090812              $Eof  = *on;
392900090812         else ;
393000090813              $RecOK  = *on;
393100090812         endif;
393200090812       ENDSR;
393300090813
393400090813       //--------------------------------------------------------------
393500090813       //?Caricamento di tutto il sfl2
393600090813       //--------------------------------------------------------------
393700090813       BEGSR RollUpS02;
393800090813
393900090813         clear  s02nrr;
394000090813         SflNxtChg2= *off;
394100090813
394200090813         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
394300090813         DOW  $EoF   = *off  and
394400090813              S02nrr < *hival;
394500090813
394600090813         // - Caricamento dati nel record del subfile
394700090813           exsr  RieS02;
394800090813           If $recok = *on ;
394900090813              S02nrr += 1;
395000090813              write TA88S02;
395100090813           endif ;
395200090813
395300090813         // - Lettura record successivo nell'archivio
395400090813           exsr sr_ReadRec2;
395500090813
395600090813         ENDDO;
395700090813
395800090813         // Visualizzazione del SFL (se ci sono dati)
395900090813         SflDsp_N2= (S02nrr <= *zeros);
396000090813
396100090813         // Attivazione (eventuale) del SFLEND
396200090813         SflEnd2 =  *on;
396300090813
396400090813         // Posizionamento cursore al 1° rcd della pagina
396500090813         if  S02nrr > *zero;
396600090813         //  wPag2  = S02nrr / C_SflPag2;
396700090813         //  wRig2   = S02nrr - (C_SflPag2 * wPag2);
396800090813         //  C02rcd = wPag2 * C_SflPag2;
396900090813         //  if  wRig2 > *zeros;
397000090813         //    C02rcd = C02rcd + 1;
397100090813         //  else;
397200090813         //    C02rcd = C02rcd - C_SflPag2 + 1;
397300090813         //  endif;
397400090813             c02rcd = 1;
397500090813         else;
397600090813           clear  C02rcd;
397700090813         endif;
397800090813
397900090813         C02csr = C02rcd;
398000090813
398100090813       ENDSR;
398200090813
398300090812       //--------------------------------------------------------------
398400090812       //?Caricamento singolo record nel SubFile S02
398500090812       //--------------------------------------------------------------
398600090812       BEGSR RieS02;
398700090812
398800090812         clear  ta88S02;
398900100311         $convalidata = *off ;
399000100311         convalidata = *off ;
399100090812
399200090812         s02ctr = vofctr ;
399300090813         s02prg = vofprg;
399400090812         s02tpt = voftpt ;
399500090812
399600090813         // verifico se esiste veramente il record tariffa relativo
399700100311         // altrimenti metto l'esito
399800090813
399900090813             tamksc = vofnrv ;
400000090813             tamctr = vofctr ;
400100090813             tamprg = vofprg ;
400200090813             chain (tamksc:tamctr:tamprg) tnofm01l ;
400300090813             If not %found(tnofm01l) ;
400400100311         // valorizzo i dati relativi l'esito  se convalidata
400500100311                 $convalidata = *on;
400600100427             else  ;
400700090813
400800100311         // valorizzo  descrizione / note
400900100427             s02dcv = tamdcv ;
401000100427             endif ;
401100090813
401200090813         // valorizzo descrizione tipo tariffa
401300090813            xx = %lookup ((%subst(%editc(tamctr:'X'):1:1)):ctr) ;
401400090813            if xx > 0 ;
401500090813               s02dtr = dtr(xx) ;
401600090813            endif ;
401700090813
401800100311         // cerco l'ultima data variazione solo se presente in tnofm
401900090813
402000100311         If $convalidata = *off ;
402100110922          IF  %subst(knsif:7:1) = 'P' and dutpou = 046 ;
402200110922            exec sql declare c4 cursor for
402300110922            select distinct max(TAMduv, TPTduv, TGCduv) as ULTIMA
402400110922            from   FILTRAGRPF/TNOFM00F  inner join  FILTRAGRPF/TIOPT00F
402500110922            on     TAMksc = TPTksc
402600110922               and TAMctr = TPTctr
402700110922               and TAMprg = TPTprg
402800110922                          inner join  FILTRAGRPF/TIOGC00F
402900110922            on     TAMksc = TGCksc
403000110922               and TAMctr = TGCctr
403100110922               and TAMprg = TGCprg
403200110922            where  TAMksc = :VOFnrv
403300110922               and TAMctr = :VOFctr
403400110922               and TAMprg = :VOFprg
403500110922            order  by ULTIMA desc  ;
403600110922            exec sql open c4 ;
403700110922
403800110922            exec sql fetch c4 into :dta_ult_var ;
403900110922
404000110922          Endif ;
404100110922
404200110922          IF  %subst(knsif:7:1) <> 'P' and dutpou = 046 ;
404300110922            exec sql declare c5 cursor for
404400110922            select distinct max(TAMduv, TPTduv, TGCduv) as ULTIMA
404500110922            from   FILTRAGRU/TNOFM00F  inner join  FILTRAGRU/TIOPT00F
404600110922            on     TAMksc = TPTksc
404700110922               and TAMctr = TPTctr
404800110922               and TAMprg = TPTprg
404900110922                          inner join  FILTRAGRU/TIOGC00F
405000110922            on     TAMksc = TGCksc
405100110922               and TAMctr = TGCctr
405200110922               and TAMprg = TGCprg
405300110922            where  TAMksc = :VOFnrv
405400110922               and TAMctr = :VOFctr
405500110922               and TAMprg = :VOFprg
405600110922            order  by ULTIMA desc  ;
405700110922            exec sql open c5 ;
405800110922
405900110922            exec sql fetch c5 into :dta_ult_var ;
406000110922
406100110922          Endif ;
406200110922
406300110922          If dutpou <> 046 ;
406400110922            exec sql declare c6 cursor for
406500110922            select distinct max(TAMduv, TPTduv, TGCduv) as ULTIMA
406600110922            from   TNOFM00F  inner join  TIOPT00F
406700110922            on     TAMksc = TPTksc
406800110922               and TAMctr = TPTctr
406900110922               and TAMprg = TPTprg
407000110922                          inner join  TIOGC00F
407100110922            on     TAMksc = TGCksc
407200110922               and TAMctr = TGCctr
407300110922               and TAMprg = TGCprg
407400110922            where  TAMksc = :VOFnrv
407500110922               and TAMctr = :VOFctr
407600110922               and TAMprg = :VOFprg
407700110922            order  by ULTIMA desc  ;
407800110922            exec sql open c6 ;
407900110922
408000110922            exec sql fetch c6 into :dta_ult_var ;
408100110922          Endif ;
408200090813
408300100311            If Sqlcod = 0 ;
408400100311               data_eur = %date(dta_ult_var:*iso) ;
408500100311               s02duv = %dec(data_eur) ;
408600100311            else ;
408700100311               data_eur = %date(tamduv:*iso) ;
408800100311               s02duv = %dec(data_eur) ;
408900100311            endif ;
409000090813
409100100311            exec sql close c4 ;
409200090813
409300090813         // cerco se ci sono delle note legate  all'offerta
409400090813
409500091112                 ntcapl = 'T' ;
409600090813                 ntcnk1 = %editc(dutkci: 'X') + %editc(vofnrv:'X');
409700090813                 ntcnk2 = %editc(vofctr: 'X') ;
409800090813                 ntctnt = '10' ;
409900090813                 setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
410000090813                 If %equal(tfntc01l) ;
410100090813                    s02not = 'SI' ;
410200091120                    note10_tar = *on ;
410300090813                 else ;
410400091120                    note10_tar = *off ;
410500091120         // provo con chiave generica con solo la trattativa
410600091120                    clear ntcnk2 ;
410700091120                    setll    (ntcapl:ntcnk1:ntcnk2:ntctnt) tfntc01l;
410800091120                    If %equal(tfntc01l) ;
410900091120                       s02not = 'SI' ;
411000091120                    else ;
411100091120                       s02not = '  ' ;
411200091120                    endif ;
411300090813    2            EndIf ;
411400100311    2     EndIf ;
411500100311
411600100311          SELECT ;
411700100311        // verifico l'Esito
411800100311          when vofeso = ' ' ;
411900090812
412000100311        // congelata
412100100311          When vofeso = 'H' ;
412200100311            s02esi = 'Congelata  ';
412300100311        // accettata
412400100311          When vofeso = 'A' ;
412500100311            s02esi = 'Accettata  ';
412600100311        // convalidata
412700100311          When vofeso = 'C' ;
412800100311            s02esi = 'Convalidata';
412900100311            data_eur = %date(vofdco:*iso) ;
413000100311            s02duv = %dec(data_eur) ;
413100100311            convalidata = *on ;
413200100311        // congelata
413300100311          other ;
413400100311            clear s02esi ;
413500100311
413600100311          ENDSL ;
413700100506
413800100506          sh2eso = vofeso ;
413900100818
414000101007            protopzs2 = *off;
414100101007            sh2in17 = '0' ;
414200100818         // verifico se abilitare l'opzione di modifica offerta
414300100818         // se trattativa chiusa , offerta congelata e sono in gestione
414400100819         If visdch > 0 and vofeso = 'H' and $Vengo_da_Gest ;
414500100818            protopzs2 = *off;
414600100818            sh2in17 = '0' ;
414700100818         endif ;
414800100818         // visualizzazione proteggo
414900100819         If ita88fle = '5' and $Vengo_da_gest = *off ;
415000100818            protopzs2 = *on;
415100100818            sh2in17 = '1' ;
415200100818         endif ;
415300100818         // offerta convalidata proteggo
415400100818         If vofeso = 'C' ;
415500100818            protopzs2 = *on;
415600100818            sh2in17 = '1' ;
415700100818         endif ;
415800100311
415900090812       ENDSR;
416000090812
416100090813       //--------------------------------------------------------------
416200090813       //?Controllo i dati relativi al formato C02
416300090813       //--------------------------------------------------------------
416400090813       BEGSR CtrC02;
416500090813
416600090813            WindDspF = IndDspF;
416700090813            reset WindDspFb;
416800090813            IndDspF  = WindDspF;
416900100419
417000100419       // se digitato f10 e trattativa fittizia errore
417100100419         If  dsp_aid = c_F10 and c01ffz <> ' ' ;
417200100419           errMessage  = *on;
417300100419           errGenerico = *on;
417400100419           PosCurFfz  = *on;
417500100419           V1Dmsg = $msg(35);
417600100419           leavesr;
417700100419         endif ;
417800100312
417900090813        // verifico la partita IVA in caso di immissione di nuove tariffe
418000090814        // se non è valida non vado avanti
418100090814            If c02ctr <> *blanks and s02nrr = 0 ;
418200090813               exsr sr_CtrIva ;
418300090814               If errgenerico= *on ;
418400090814                  leavesr ;
418500090814               EndIf ;
418600090813            endif ;
418700090813
418800090814        // verifico se ci sono le info commerciali
418900121211            Info_Obb = *on;
419000090814            exsr SR_Infocom;
419100090814             If errGenerico = *on ;
419200090814                leavesr ;
419300090814             endif ;
419400100305
419500100305        // verifico se ci sono le info trattativa
419600100305            exsr SR_Infotra;
419700100305             If errGenerico = *on ;
419800100305                leavesr ;
419900100305             endif ;
420000090814
420100090814        // verifico il codice tariffa da creare o da copiare
420200090814            If c02ctr <> *blanks ;
420300100316
420400100316        // verifico che non esista per la stessa tariffa già delle
420500100316        // accettate o convalidate
420600100316               vofnrv = c01nrv ;
420700100316               vofctr = %dec(c02ctr:3:0) ;
420800110104               setll (vofnrv:vofctr) tivof11l ;
420900110104               reade (vofnrv:vofctr) tivof11l ;
421000110104               dow not %eof(tivof11l);
421100100316                   if vofeso = 'A' or vofeso = 'C' ;
421200100316                      errMessage  = *on;
421300100316                      errGenerico = *on;
421400100316                      PosCurCtr   = *on;
421500100316                      V1Dmsg = $msg(33) ;
421600100316                      leavesr;
421700100316                   endif ;
421800110104               reade (vofnrv:vofctr) tivof11l ;
421900100316               enddo ;
422000090814
422100090814        // controllo se il tipo tariffa è valido nella tabella TR
422200090814               If %lookup ((%subst(c02ctr:1:1)):ctr) = 0 ;
422300090814                  errMessage  = *on;
422400090814                  errGenerico = *on;
422500090814                  PosCurCtr   = *on;
422600090814                  V1Dmsg = $msg(19) ;
422700090814                  leavesr;
422800090814               endif;
422900090814
423000090814        // controllo se tariffa già esistente se non inserito il progressivo
423100090917        // vedo se esiste il progressivo 9 e do errore
423200090814               if s02nrr > 0 and c02prg = *blank ;
423300090814                  tamksc = c01nrv ;
423400090814                  tamctr = %dec(c02ctr:3:0) ;
423500090917                  tamprg = 9 ;
423600090917                  setll (tamksc:tamctr:tamprg) tnofm01l ;
423700090814                  If %equal (tnofm01l) ;
423800090814        // emetto il msg di errore ....
423900090814                     errMessage  = *on;
424000090814                     errGenerico = *on;
424100090814                     PosCurCtr   = *on;
424200090814                     V1Dmsg = $msg(20) ;
424300090814                     leavesr;
424400090814                  endif;
424500090814               endif;
424600090814
424700090917        // controllo se tariffa no esistente non si deve inserire il progressivo
424800090917               if c02ctr <> *blanks and c02prg <> *blank ;
424900100126
425000100126        // controllo se numerico il campo della versione
425100100126         IF  %check(digits:c02prg) > 0;
425200100126             errMessage  = *on;
425300100126             errGenerico = *on;
425400100126             PosCurCtr   = *on;
425500100126             V1Dmsg = $Msg(29);
425600100126             leavesr;
425700100126         ENDIF;
425800100126
425900090917                  tamksc = c01nrv ;
426000090917                  tamctr = %dec(c02ctr:3:0) ;
426100090917                  tamprg = %dec(c02prg:3:0) ;
426200090917                  setll (tamksc:tamctr:tamprg) tnofm01l ;
426300090917                  If not %equal (tnofm01l) ;
426400090917        // emetto il msg di errore ....
426500090917                     errMessage  = *on;
426600090917                     errGenerico = *on;
426700090917                     PosCurCtr   = *on;
426800090917                     V1Dmsg = $msg(25) ;
426900090917                     leavesr;
427000090917                  endif;
427100090917               endif;
427200090814        // controllo se tariffa già esistente se inserito il progressivo
427300090814               if s02nrr > 0 and c02prg <> *blank ;
427400090814                  tamksc = c01nrv ;
427500090814                  tamctr = %dec(c02ctr:3:0) ;
427600090814                  tamprg = %dec(c02prg:3:0) ;
427700090814                  setll (tamksc:tamctr:tamprg) tnofm01l ;
427800090911                  If %equal (tnofm01l) and c02cop = ' ' ;
427900090814        // emetto il msg di errore ....
428000090814                     errMessage  = *on;
428100090814                     errGenerico = *on;
428200090814                     PosCurCtr   = *on;
428300090814                     V1Dmsg = $msg(21) ;
428400090814                     leavesr;
428500090814                  endif;
428600090814               endif;
428700090814            endif;
428800090814
428900090814        // controllo  copia tariffa offerta
429000090814               if c02tto <> ' ' and c02cop = ' ' ;
429100090814                  errMessage  = *on;
429200090814                  errGenerico = *on;
429300090814                  PosCurCop   = *on;
429400090814                  V1Dmsg = $msg(22) ;
429500090814                  leavesr;
429600090814               endif;
429700090814
429800090814         // e il contrario
429900090814               if c02cop <> ' ' and c02tto = ' ' ;
430000090814                  errMessage  = *on;
430100090814                  errGenerico = *on;
430200090814                  PosCurTto   = *on;
430300090814                  V1Dmsg = $msg(23) ;
430400090814                  leavesr;
430500090814               endif;
430600090814
430700090814         // se immessa copia verificare se inserita offerta
430800090814               If c02cop = 'S' and c02ctr = *blanks;
430900090814                  errMessage  = *on;
431000090814                  errGenerico = *on;
431100090814                  PosCurCtr   = *on;
431200090814                  V1Dmsg = $msg(24) ;
431300090814                  leavesr;
431400090814               endif;
431500090814
431600090814        // se passati tutti i controlli richiamo i pgm gestione o copia
431700090814        // tariffa/offerta
431800090814               if c02ctr <> *blanks ;
431900090814               If c02cop = 'S' ;
432000090917        // se progressivo a zero copia altrimenti integra
432100090814                  clear tnta24ds ;
432200090814                  ta24tip = c02tto ;
432300090814                  ta24nrv = c01nrv ;
432400090814                  ta24ctr = %dec(c02ctr:3:0) ;
432500090917                  If c02prg = *blanks ;
432600090917                     clear ta24prg ;
432700090917                  else;
432800090917                     ta24prg = '00' + c02prg ;
432900090917                  endif;
433000090916                  clear kpjbu ;
433100090916                  kpjbu = 'T';
433200090814                  TNTA24R (kpjba : tnta24ds) ;
433300090814                  If ta24err <> *blanks ;
433400090814                     errMessage  = *on;
433500090814                     errGenerico = *on;
433600090814                     PosCurCtr   = *on;
433700090814                     V1Dmsg = ta24msg  ;
433800090814                     leavesr;
433900090814                  endif ;
434000090814
434100090814         // se invece creazione di una nuova tariffa richiamo il pgm tnta01r2
434200090814
434300090814               else ;
434400090917
434500090917                  clear ta01r2ds ;
434600090814
434700090814                  ta01ctr = %dec(c02ctr:3:0) ;
434800090907
434900090917                  tamksc = c01nrv ;
435000090917                  tamctr = %dec(c02ctr:3:0) ;
435100110104                  setgt (tamksc:tamctr) tivof11l ;
435200110104                  readpe (tamksc:tamctr) tivof11l ;
435300110104                  if %eof(Tivof11l) ;
435400090917                     ta01prg = 0 ;
435500090907                  else ;
435600100127                     ta01prg = vofprg +1 ;
435700090907                  endif ;
435800090907
435900090814                  exsr sr_ta01r2 ;
436000090814
436100090814               Endif ;
436200090814               Endif ;
436300090814
436400090813       ENDSR ;
436500090814
436600090916
436700090916       //--------------------------------------------------------------
436800090916       //?Gestione opzioni subfile S02
436900090916       //--------------------------------------------------------------
437000090916       BEGSR OpzS02;
437100090916
437200090916         readc TA88S02;
437300090916
437400090916         DOW  NOT  %eof(TNTA88D);
437500090916
437600090916           SflNxtChg2= *off;
437700090916           WindDspF  = IndDspF;
437800090916           reset WindDspFb;
437900090916           IndDspF   = WindDspF;
438000090916           C02rcd    = S02nrr;
438100090916
438200090916           SELECT;
438300090916
438400090916             // - Nessuna opzione
438500090916             WHEN  S02opz  = *blank;
438600090916
438700090916             // - Precedente segnalazione di errore
438800090916             WHEN  S02opz  = 'E';
438900090916               clear S02opz;
439000090916
439100100506             // - 1 = Gestione  ma tariffa convalidata errore
439200100506             WHEN  S02opz  = '1'and sh2eso = 'C';
439300100506               errMessage  = *on;
439400100506               errGenerico = *on;
439500100506               PosCurOpz2  = *on;
439600100506               V1Dmsg = $Msg(37);
439700100506
439800100506
439900100506             // - 1 = Gestione
440000090916             WHEN  S02opz  = '1';
440100090916
440200090917                  clear ta01r2ds ;
440300090917
440400090916                  ta01ctr = s02ctr ;
440500090916                  ta01prg = s02prg ;
440600090916                  exsr Sr_ta01r2  ;
440700090916
440800090916             // - ? = Opzione NON valida
440900090916             OTHER;
441000090916               errMessage  = *on;
441100090916               errGenerico = *on;
441200090916               PosCurOpz2  = *on;
441300100209               V1Dmsg = $Msg(10);
441400090916
441500090916           ENDSL;
441600090916
441700090916           // Aggiornamento sfl
441800090916           select;
441900090916             when  errMessage  = *on;
442000090916               SflNxtChg2 = *on;
442100090916               C02csr    = C02rcd;
442200090916             when  errGenerico = *on;
442300090916               C02csr    = C02rcd;
442400090916               clear  S02opz;
442500090916             other;
442600090916               C02csr    = C02rcd;
442700090916               clear  S02opz;
442800090916           endsl;
442900100818
443000100818           // verifico la protezione
443100100818           if sh2in17 = '1' ;
443200100818              protopzs2 = *on ;
443300100818           else ;
443400100818              protopzs2 = *off ;
443500100818           endif ;
443600100818
443700090916           update TA88S02;
443800090916
443900090916           if  errMessage = *on  or  errGenerico = *on;
444000090916             leave;
444100090916           endif;
444200090916
444300090916           readc TA88S02;
444400090916
444500090916         ENDDO;
444600090916
444700090916       ENDSR;
444800090916
444900090814       //--------------------------------------------------------------
445000090814       //?Richiamo del TNTA01R2
445100090814       //--------------------------------------------------------------
445200090814       BEGSR sr_ta01r2 ;
445300090814
445400090814                   ta01nrv = c01nrv ;
445500090814                   ta01flg = '1' ;
445600090814                   ta01ccc = dutkci ;
445700090814                   ta01cli = c01ksc ;
445800090814                   ta01cpo = ch1cpo ;
445900090916                   ta01rgs = c01rag ;
446000090917                   ta01trat = 'T' ;
446100090814                   clear kpjbu ;
446200090814                   kpjbu = ta01r2ds ;
446300100818                   Tnta01r2 (kpjba ) ;
446400090923
446500090814       ENDSR ;
446600090813
446700090813       //--------------------------------------------------------------
446800090813       //?Controllo partita IVA cliente
446900090813       //--------------------------------------------------------------
447000090813       BEGSR sr_CtrIva;
447100090813
447200090814        //  Aggancio l'anagrafico per recuperare la partita IVA
447300090814        //  e il codice fiscale
447400090814             acokut = 1 ;
447500090814             acokcc = dutkci ;
447600090814             acoksc = c01nrv ;
447700090814             chain(n) (acokut:acokcc:acoksc) Tfind00f ;
447800090814             chain(n) (acokut:acokcc:acoksc) Tfclp00f ;
447900090814             If not %found(tfind00f)  ;
448000090814                clear  tibs69ds;
448100090814                I69kin = c01ksc  ;
448200090814                I69kcp = c01ksc  ;
448300090814                TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
448400090814             endif;
448500090813
448600090814             if clpscf>0 and clpscf<>c01ksc ;
448700090814
448800090814        // devo reperire la nazione del sottoconto fatturazione
448900090814        // che mi serve per i controlli di obbligatorietà di cod.fiscale e PIva
449000090814
449100090814                clear  w_scfsta;
449200090814                clear  Tibs69ds;
449300090814                I69kin = clpscf;
449400090814                TIBS69R (tibs69ds:ds_cnaco:ds_cnind_w:ds_cnclp:ds_fncls);
449500090814                if o69err=*blanks and w_indflg=*blanks ;
449600090814                   w_scfsta=w_indsta;
449700090814                endif;
449800090814             else;
449900090814                w_scfsta=indsta ;
450000090814             endif ;
450100090813
450200090814        // Controllo il Codice Fiscale
450300090814
450400151021             clear xcfiva1ds ;
450500090814             clear xcfivamod ;
450600090814             xcfivapar = indcdf ;
450700090814             xcfivanar = indsta ;
450800090814             xcfivaprv = indprv ;
450900151021             xcfivaloc = indcit ;
451000151021             xcfivacap = indcae ;
451100151021             XCFIVAFORI=' ' ;
451200151021             XCFIVApiva=indiva ;
451300151021             XCFIVAR1(xcfiva1ds);
451400090814
451500090814             If xcfivaflg < *zeros ;
451600090814                errMessage  = *on;
451700090814                errGenerico = *on;
451800090814                PosCurCtr   = *on;
451900090814                V1Dmsg = xcfivamsg ;
452000090814                leavesr;
452100090814             endif;
452200090814
452300090814        // Se non c'e' codice fiscale cli italia --> errore
452400090814             if  xcfivaflg<>9 and
452500090814                 (indcdf = *blanks and
452600090814                 indsta = *blanks and w_scfsta=*blanks);
452700090814                 errMessage  = *on;
452800090814                 errGenerico = *on;
452900090814                 PosCurCtr   = *on;
453000090814                 V1Dmsg = $msg(17) ;
453100090814                 leavesr;
453200090814             endif;
453300090813
453400090814        // Imposto la partita IVA
453500090814             if  indiva<>*blanks ;
453600090814                 if  %subst(indiva:1:2) >= '00';
453700090814                     itaiva = indiva  ;
453800090814                 Else ;
453900090814                     wiva =  indiva ;
454000090814                 EndIf;
454100090814        // Controllo la partita IVA recuperata
454200090814        // devo richiamare il nuovo driver fatto per controllare il codice
454300090814        // fiscale e la partita iva
454400151021                 clear xcfiva1ds;
454500090814                 xcfivamod = 'P';
454600090814                 xcfivapar = wiva ;
454700090814                 xcfivanar = indsta ;
454800090814                 xcfivaprv = indprv ;
454900151021                 xcfivaloc = indcit ;
455000151021                 xcfivacap = indcae ;
455100151021                 XCFIVAR1 (xcfiva1ds);
455200090814                 if  xcfivaflg < *zeros  and
455300090814                     xcfivaflg > -9  ;
455400090814                    errMessage  = *on;
455500090814                    errGenerico = *on;
455600090814                    PosCurCtr   = *on;
455700090814                    V1Dmsg = xcfivamsg ;
455800090814                    leavesr;
455900090814                 endif;
456000151021        // Non c'è la partita IVA errore
456100151021             if xcfivaflg<>9 and
456200151021                (indiva = *Blanks and (indsta<>*blanks or
456300151021                 w_scfsta=*blanks));
456400151021                 errMessage  = *on;
456500151021                 errGenerico = *on;
456600151021                 PosCurCtr   = *on;
456700151021                 V1Dmsg = $msg(18) ;
456800151021                 leavesr;
456900151021             endif;
457000090814             EndIf;
457100090813
457200090813       ENDSR ;
457300090814
457400090806       //--------------------------------------------------------------
457500090806       //?Recupero numero trattativa nuova
457600090806       //--------------------------------------------------------------
457700090806       BEGSR sr_Recnum;
457800090806
457900100413                RESET trul48ds;
458000100413                trul48r(kpjba:trul48ds);
458100090806
458200100413                If o48Err = 0;
458300100413                   newnrv  = o48nrv;
458400090807                else ;
458500090807                   $ErrGrave   = *on;
458600090807                   errMessage  = *on;
458700090807                   errGenerico = *on;
458800100413                   V1Dmsg = o48msg ;
458900090807                   leavesr;
459000090806                Endif;
459100090806
459200090806       endsr;
459300090814
459400100713       //--------------------------------------------------------------
459500100723       //?Gestione videata W03 Richiesta annullamento ATTIVITA'
459600100713       //--------------------------------------------------------------
459700100713       BEGSR GesW03;
459800100713
459900100713         clear w03sino ;
460000100714         $fine3 = *off ;
460100100713
460200100714         dow $Fine3 =  *off ;
460300100713         // Emissione Finestra
460400100713             exfmt  TA88W03;
460500100713
460600100713         SELECT;
460700100713
460800100713         // - F06=Conferma
460900100713           WHEN  dsp_aid = c_F06;
461000100713             If W03sino = 'S' ;
461100100714                exsr Sr_DelAtc ;
461200100714                $inzs01 = *on ;
461300100723                $video = 'S1' ;
461400100714                $fine3 = *on ;
461500100713             endif ;
461600100713             If W03sino = 'N' ;
461700100714                $fine3 = *on ;
461800100713             endif ;
461900100713
462000100713         ENDSL;
462100100713       ENDDO;
462200100713
462300100713       ENDSR;
462400100713
462500100723
462600100723       //--------------------------------------------------------------
462700100723       //?Gestione videata W04 Guida alla creazione NUOVE ATTIVITA'
462800100723       //--------------------------------------------------------------
462900100723       BEGSR GesW04;
463000100723
463100100723         $fine4 = *off ;
463200100723
463300100723         dow $Fine4 =  *off ;
463400100723         // Emissione Finestra
463500100723             exfmt  TA88W04;
463600100723
463700100723         SELECT;
463800100723
463900100723         // - F10=Creazione attività
464000100723           WHEN  dsp_aid = c_F10;
464100100723                exsr F10S01 ;
464200100723                $inzs01 = *on ;
464300100723                $video = 'S1' ;
464400100723                $fine4 = *on ;
464500100723
464600100723         ENDSL;
464700100723       ENDDO;
464800100723
464900100723       ENDSR;
465000100723
465100110104
465200110104       //--------------------------------------------------------------
465300110104       //?Gestione videata W05 Richiesta annullamento INFO TRATTATIVA
465400110104       //--------------------------------------------------------------
465500110104       BEGSR GesW05;
465600110104
465700110104         clear w05sino ;
465800110104         $fine5 = *off ;
465900110104
466000110104         dow $Fine5 =  *off ;
466100110104         // Emissione Finestra
466200110104             exfmt  TA88W05;
466300110104
466400110104         SELECT;
466500110104
466600110104         // - F06=Conferma
466700110104           WHEN  dsp_aid = c_F06;
466800110104             If W05sino = 'S' ;
466900110104         // Devo deletare le info trattativa a fronte di questa visita
467000110111                exsr Del_info ;
467100110112         // Devo verificare se si tratta di modifica di trattativa di tipo
467200110112         // I=Incremento con caldo
467300110112                If vistpv = 'I';
467400110113                   exsr Sr_Delcaldo   ;
467500110112                Endif ;
467600110112
467700110104                $fine5 = *on ;
467800110104             endif ;
467900110104             If W05sino = 'N' ;
468000110104                $fine5 = *on ;
468100110104             endif ;
468200110104
468300110104         ENDSL;
468400110104       ENDDO;
468500110104
468600110104       ENDSR;
468700151111
468800151111       //--------------------------------------------------------------
468900151111       //?Gestione videata W06 Cliente in Contenzioso.
469000151111       //--------------------------------------------------------------
469100151111       BEGSR  sr_GesW06;
469200151111
469300151111         // -?Decodifica Stato del Credito?
469400151111         clear  ds4W;
469500151113         chain  ( 1 : '4W' : wSaveCON )  TABEL;
469600151111         if  %found(TABEL00F)  and  TBLflg = *blanks;
469700151111           ds4W = TBLuni;
469800151111         endif;
469900151111
470000151111         // -?Emissione di una window con messaggio di errore?
470100151111         //  ?forzabile per stato del credito trovato?
470200151111         //  ?(emesso in window con F6 di conferma)?
470300151113         W6Ccon = %subst( wSaveCON : 2 : 2 );
470400151111         W6Dcon = §4Wdes;
470500151111
470600151113         DoU  dsp_aid = c_F06;
470700151111           exfmt  TA88W06;
470800151111         EndDo;
470900151111
471000151111         $Contenzioso = *off;
471100151111
471200151111       ENDSR;
471300110111
471400110111       //--------------------------------------------------------------
471500110111       //?Pulizia delle info trattative
471600110111       //--------------------------------------------------------------
471700110111       BEGSR Del_info ;
471800110104
471900110111       // pulizia di tutte le info legate alla trattativa
472000110111
472100110111                setll (c01nrv) tivii01l ;
472200110111                reade (c01nrv) tivii01l ;
472300110111                dow not %eof(tivii01l);
472400110111                    delete (c01nrv) tivii01l ;
472500110111                    reade (c01nrv) tivii01l ;
472600110111                enddo ;
472700150309
472800150309       //?Se cliente in campagna e il tipo trattativa cambiato o cancello
472900150309       //?tutta la trattativa cancello la fase TR per questa trattativa
473000150430           wDelTRffz = *off;
473100150430           exsr FaseCampagna;
473200110112
473300110112                $infotrat = *on ;
473400110112                sav_ifotot = ' ' ;
473500110111
473600110111       ENDSR;
473700110111
473800110112
473900110112       //--------------------------------------------------------------
474000110112       //?Pulizia dei dati relativi alla trattativa di tipo incremento
474100110112       //--------------------------------------------------------------
474200110113       BEGSR Sr_DelCaldo   ;
474300110112
474400110112       // pulizia del TIVOF  999
474500110112
474600110112                setll (c01nrv:999) tivof01l ;
474700110112                reade (c01nrv:999) tivof01l ;
474800110112                dow not %eof(tivof01l);
474900110112                    delete tivof01l ;
475000110112                    reade (c01nrv:999) tivof01l ;
475100110112                enddo ;
475200110112
475300110211       // se esiste l'attività 44 caldo ancora aperta la deleto
475400110112               exec sql delete from tiatc00f where atcdco =0
475500110113               and atcnrv =:c01nrv and atccad = '44' ;
475600110112
475700110112       ENDSR;
475800150430
475900150430       //--------------------------------------------------------------
476000150430       //?Cancello Fase TR dalla Campagna.
476100150430       //--------------------------------------------------------------
476200150430       BEGSR FaseCampagna;
476300150430
476400150430         wEoF_CMF = *off;
476500150430
476600150430         exec sql
476700150430         DECLARE CMF cursor for
476800150430         SELECT * from TICMF00F
476900150430         WHERE CMFnrv = :C01nrv and CMFacm = ' TR';
477000150430
477100150430         exec sql OPEN CMF;
477200150430
477300150430         IF  sqlcode < 0;
477400150430           wEoF_CMF = *on;
477500150430           exec sql CLOSE CMF;
477600150430           leavesr;
477700150430         ENDIF;
477800150430
477900150430         DOW  not wEoF_CMF;
478000150430           exec sql
478100150430           fetch next from CMF into :ticmfDS;
478200150430
478300150430           IF  sqlcod = 100 or sqlcod < 0;
478400150430             wEoF_CMF = *on;
478500150430             leave;
478600150430           ENDIF;
478700150430
478800150430           //?Cancello fase TR
478900150430           chain (CMFncm:CMFksu:CMFksc:CMFcpo:CMFacm:CMFdfa:CMFhfc)
479000150430                 TICMF01L;
479100150430           IF  %found(TICMF01L);
479200150430             delete  TICMF000;
479300150430             chain (CMFncm:CMFksu:CMFksc:CMFcpo) TICMC01L;
479400150430           //?Scrivo nota sulla campagna
479500150430             clear TRKC10DS;
479600150430             IKC10tla = 'L';
479700150430             IKC10flm = 'C';
479800150430             IKC10fno = 'S';
479900150430             IKC10ncm = CMFncm;
480000150430             IKC10ksu = CMFksu;
480100150430             IKC10ksc = CMFksc;
480200150430             IKC10cpo = CMFcpo;
480300150430             IKC10acm = CMCufe;
480400150430             EKC10no1 = 'Cancellata fase TR su Trattativa ' +
480500150430                        %editc(C01nrv:'X');
480600150430             IF  wDelTRffz;
480700150430               EKC10no1 = %trim(EKC10no1) +
480800150430                          '. Resa Fittizia';
480900150430             ENDIF;
481000150430             IF  not wDelTRffz and C01tpv <> 'A';
481100150430               EKC10no1 = %trim(EKC10no1) +
481200150430                          '. Variato tipo trattativa';
481300150430             ENDIF;
481400150430             IF  not wDelTRffz and C01tpv = 'A';
481500150430               EKC10no1 = %trim(EKC10no1) +
481600150430                          '. Annullata';
481700150430             ENDIF;
481800150430             EKC10dim = %dec(%date());
481900150430             EKC10him = %dec(%time());
482000150430             trkc10r (kpjba:TRKC10DS);
482100150430           ENDIF;
482200150430         ENDDO;
482300150430
482400150430         exec sql CLOSE CMF;
482500150430
482600150430       ENDSR;
482700160804
482800160804       //--------------------------------------------------------------
482900160804       //?Scrivo l'immagine Precedente anagrafica Potenziale
483000160804       //--------------------------------------------------------------
483100160804       BEGSR ImmaginePrima;
483200160804
483300160804         clear TRMK30DS;
483400160804         IMK30immag = 'P';
483500160804         IMK30tvp = 'E';
483600160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
483700160804
483800160804       ENDSR;
483900160804
484000160804       //--------------------------------------------------------------
484100160804       //?Scrivo Storico Variazioni.
484200160804       //--------------------------------------------------------------
484300160804       BEGSR ScriviVariazione;
484400160804
484500160804         clear TRMK30DS;
484600160804         IMK30pru = knmus;
484700160804         IMK30noj = knmeb;
484800160804         IMK30pgm = 'TNTA88R';
484900160804         IMK30immag = 'D';
485000160804         IMK30cta = 'M';
485100160804         IMK30tvp = 'E';
485200160804
485300160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
485400160804
485500160804       ENDSR;
485600110112
485700080206       //--------------------------------------------------------------
485800080206       //?Operazioni finali.
485900080206       //--------------------------------------------------------------
486000080206       BEGSR RoutEnd;
486100080206
486200090806         reset trul33ds;
486300090806         i33tla = 'C'  ;
486400090806         kpjbu = trul33ds;
486500090806         trul33r(kpjba);
486600130409
486700130409         clear tntaa1ds;
486800130409         itaa1tla = 'C'  ;
486900130409         kpjbu = tntaa1ds;
487000130409         tntaa1c(kpjba);
487100160804
487200160804       //?Chiudo file storicizzazione variazioni potenziale
487300160804         clear TRMK30DS;
487400160804         clear tncpo_30;
487500160804         clear tncpo1_30;
487600160804         clear ticpi_30;
487700160804         IMK30tla = 'C';
487800160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
487900090806
488000080206         *inLR = *on;
488100080206         return;
488200080206
488300080206       ENDSR;
488400080206
488500080206      /end-free
488600080206       //--------------------------------------------------------------
488700080206       //?Schiere a tempo di compilazione.
488800080206       //--------------------------------------------------------------
488900080206
489000080206** - $MSG -------------------------------------------------------------------*
489100090724Utente non abilitato alla gestione del potenziale                              1
489200090724Data errata                                                                    2
489300090724Trattativa non retrodatabile : già elaborata la STATISTICA COMMERCIALE         3
489400100420Commerciale apertura trattativa errato o non in gestione all'utente            4
489500090804Inserire Tipo Trattativa                                                       5
489600090804Tipo Trattativa errata per cliente nuovo                                       6
489700090804Tipo Trattativa errata per cliente non nuovo                                   7
489800090804Tipo esito non codificato                                                      8
489900100506Opzione non valida per attività già eseguita                                   9
490000090804Opzione errata                                                                10
490100100420Commerciale errato o non in gestione all'utente                               11
490200100315Codice Commerciale NON UTILIZZABILE                                           12
490300090810E' OBBLIGATORIO IMMETTERE L'ANGRAFICA PER UN CLIENTE NUOVO                    13
490400120313Libero ..............                                                         14
490500120313Libero ..............                                                         15
490600090812Obbligatorio inserire TUTTE le informazioni commerciali: premere F14 !!       16
490700090814P.IVA e/o Cod.Fiscale errati/mancanti.                                        17
490800090814P.IVA errata/mancante.                                                        18
490900090814Codice Tariffa non codificato                                                 19
491000091130Tariffa con versione 9 esistente impossibile aggiungere nuova versione        20
491100090814Codice tariffa già esistente.Per integrare selezione "Copia"                  21
491200090814Per copiare da tariffa/offerta impostare "S"                                  22
491300090814Richiesta la copia, immettere se da tariffa o da offerta                      23
491400090917Indicare il codice tariffa  da creare/integrare                               24
491500091130In caso di creazione di una nuova tariffa non indicarne la versione           25
491600100114Non esistono attività aperte legate alla trattativa. F10 per inserire!        26
491700100115Opzione non valida per questa attività                                        27
491800100125Fil. del codice commerciale incongruente con la fil. del cliente              28
491900100126Codice versione errato.                                                       29
492000100305Obbligatorio inserire le informazioni trattativa : premere F15 !!             30
492100100311Tipo Trattativa errata in presenza di offerte                                 31
492200100312Utente non abilitato alla gestione delle tariffe                              32
492300100316Tariffa già esistente e già esitata impossibile aggiungere nuova versione     33
492400100322TRATTATIVA CHIUSA. Annullare tutte le attività ancora aperte                  34
492500100419In caso di trattativa fittizia non si possono inserire Attività con F10       35
492600100419Non si può trasformare in FITTIZIA perchè presenti attività                   36
492700100506Opzione non valida per offerta convalidata                                    37
492800100507Non si può trasformare in FITTIZIA perchè presenti offerte esitate            38
492900100909Opzione non valida per trattativa fittizia                                    39
493000110118Per questa trattativa non sono previste Info Trattative                       40
493100110112Tipo trattativa non modificata                                                41
