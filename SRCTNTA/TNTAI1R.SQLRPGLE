000100141201     /*PRM  dbgview(*source)
000200141219     /*END
000300080206      //--------------------------------------------------------------
000400110607      //?INTERROGAZIONE ANAGRAFICA CLIENTI
000500080206      //--------------------------------------------------------------
000600080206
000700080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000800100409     h dftactgrp(*no) actgrp(*caller)
000900080206
001000080206      //---------------------------------------------------------------
001100080206      //?Dichiarazione file.
001200080206      //---------------------------------------------------------------
001300110607
001400110607      // - Anagrafica cliente
001500110607     fCNACO00F  if   e           k disk
001600110607     fCNACO04L  if   e           k disk    rename(CNACO000:CNACO04)
001700110607     fCNCLP00F  if   e           k disk
001800110609     fCNIND00F  if   e           k disk
001900110607
002000110607      // - Tabelle
002100110607     fTABEL00F  if   e           k disk
002200130801
002300130801       // -?Anagrafica commerciali
002400130801     fAZCMM01L  if   e           k disk
002500110607
002600110607      // - Note DC
002700110607     fTFNTC01L  if   e           k disk
002800110607
002900110607      // - Potenziale
003000110607     fTNCPO01L  if   e           k disk
003100110607
003200110309      // - File attività
003300110309     fTIATC04L  if   e           k disk
003400110310
003500110310      // - File Info Commerciali
003600110310     fTICPI01L  if   e           k disk
003700110601
003800110601      // - Trattative
003900110803     fTIVIS01L  if   e           k disk
004000110607
004100080208      // - Organigramma filiale/aziendale
004200080206     fAZORG01L  if   e           k disk
004300110308
004400080208      // - Tabelle
004500110308     fTNTBE01L  if   e           k disk
004600110308
004700110607      // - Video
004800110615     fTNTAI1D   cf   e             workstn
004900110615     f                                     sfile(TAI1S01 : S01nrr)
005000080208     f                                     indds(IndDspF)
005100080206     f                                     infds(InfDspF)
005200080206
005300080206      //---------------------------------------------------------------
005400080206      //?Definizione costanti.
005500080206      //---------------------------------------------------------------
005600050519
005700080207      // - Numero di record in ciascuna videata di subfile
005800080207     d c_SflPag        c                   const(18)
005900080207
006000080207      // - Tasti funzionali a video
006100080207     d c_F01           c                   const(x'31')
006200080207     d c_F02           c                   const(x'32')
006300080207     d c_F03           c                   const(x'33')
006400091026     d c_F04           c                   const(x'34')
006500080207     d c_F05           c                   const(x'35')
006600080207     d c_F06           c                   const(x'36')
006700080207     d c_F07           c                   const(x'37')
006800080207     d c_F08           c                   const(x'38')
006900080207     d c_F09           c                   const(x'39')
007000080207     d c_F10           c                   const(x'3A')
007100091026     d c_F11           c                   const(x'3B')
007200080207     d c_F12           c                   const(x'3C')
007300080207     d c_F13           c                   const(x'B1')
007400080207     d c_F14           c                   const(x'B2')
007500080207     d c_F15           c                   const(x'B3')
007600080207     d c_F16           c                   const(x'B4')
007700080207     d c_F17           c                   const(x'B5')
007800080207     d c_F18           c                   const(x'B6')
007900080207     d c_F19           c                   const(x'B7')
008000080207     d c_F20           c                   const(x'B8')
008100080207     d c_F21           c                   const(x'B9')
008200080207     d c_F22           c                   const(x'BA')
008300080207     d c_F23           c                   const(x'BB')
008400080207     d c_F24           c                   const(x'BC')
008500080207     d c_Enter         c                   const(x'F1')
008600080207     d c_RollDown      c                   const(x'F4')
008700080207     d c_RollUp        c                   const(x'F5')
008800080214
008900080214      // - Attributi di visualizzazione
009000080215      //  -  DspAtr() - Normale
009100080214     d C_dspatr_Norm   c                   const(x'20')
009200080215      //  -  DspAtr(RI)
009300080214     d C_dspatr_RI     c                   const(x'21')
009400081021     d C_dspatr_HI     c                   const(x'22')
009500080215      //  -  DspAtr(ND)
009600080214     d C_dspatr_ND     c                   const(x'27')
009700080215      //  -  DspAtr(BL) / Color(Red)
009800080214     d C_dspatr_BL     c                   const(x'28')
009900110607
010000110607     d Digits          c                   const('0123456789')
010100110607
010200110607     d maiu            c                   const('CPME')
010300110607     d minu            c                   const('cpme')
010400080206
010500110610       // -?Costante per controllo "caratteri solo numerici"?
010600110610     d c_Digits        c                   const('0123456789')
010700110610
010800080206      //---------------------------------------------------------------
010900080206      //?Definizione schiere.
011000080206      //---------------------------------------------------------------
011100080206
011200080206      // - Messaggi di errore
011300140915     d $Msg            s             78    dim(40) ctdata perrcd(1)
011400110309
011500110607      // - Filiali
011600110607     d C_Max_Fil       c                   const(350)
011700110607     d $Fil            s                   like(ORGfil)
011800110607     d                                     dim(C_Max_Fil)  inz
011900110607     d $Car            s                   like(ORGcar)
012000110607     d                                     dim(C_Max_Fil)  inz
012100110607
012200110309      // - Sk appoggio per filiali di un'area richiesta
012300110607     d $Fil1           s                   like(ORGfil)
012400110309     d                                     dim(C_Max_Fil)  inz
012500110308
012600110308      // - Categoria merceologica da tabella 1L
012700110308     d $1l             s              5    dim(50)
012800110308
012900110308      // - Importanza clienti da tabella IC
013000110308     d $Ic             s              1    dim(10)
013100110308
013200110407      // - Categoria potenziale da tabella CPO
013300110407     d $Cpo            s              1    dim(10)
013400110407
013500110308      // - Concorrenti possibili da tabella IFS
013600110308     d $Ifs            s              4    dim(30)
013700140915
013800140915      // - Concorrenti possibili da tabella IFS logistica
013900140915     d $IfsLOG         s              4    dim(30)
014000140915
014100140915      // - Paesi Estero possibili da tabella IFS
014200140915     d $IfsPAESI       s              4    dim(30)
014300110322
014400110407      // - Sk x importanza clienti richiesti nella selezione
014500110322     d $cic            s              1    dim(5)
014600110407
014700110609      // - Agenti e Unificanti
014800141219     d c_Max_Cmm       c                   const(9000)
014900141219     d $Age            s              7  0 dim(c_Max_Cmm)
015000141219     d $AgeU           s              7  0 dim(c_Max_Cmm)
015100110614
015200110614      // - Sk appoggio per comm.li legati all'unificante
015300150109     d $Age1           s              7  0 dim(c_Max_Fil)
015400110607
015500080206
015600080206      //---------------------------------------------------------------
015700080206      //?Definizione aree dati.
015800080206      //---------------------------------------------------------------
015900080206
016000080206      // - Dati utente
016100080206     d §AzUte        e ds                  extname(AZUTE00F)
016200080206     d                                     dtaara
016300080206     d §DatiUte      e ds                  extname(dDatiUte)
016400080206     d                                     dtaara
016500080206
016600080206      //---------------------------------------------------------------
016700080206      //?Definizione strutture dati.
016800080206      //---------------------------------------------------------------
016900080206
017000080206      // - Status
017100080206     d Psds           sds
017200080206     d   SDSpgm          *proc
017300080206
017400080206      // - InfDS
017500080206     d InfDspF         ds
017600080207     d  dsp_aid              369    369a
017700080206
017800080206      // - Indicatori su DspF
017900080208     d IndDspF         ds
018000140915        // - Reverse immage  per comando altre parzializzazioni
018100140915     d  AltreParz                     1n   overlay(IndDspF : 11)
018200080206        // - Indicatori di gestione del subfile
018300080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
018400080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
018500080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
018600080206     d  SflEnd                        1n   overlay(IndDspF : 33)
018700080206        // - Indicatori di errore
018800080206     d  errMessage                    1n   overlay(IndDspF : 28)
018900080206     d  PosCurOpz                     1n   overlay(IndDspF : 50)
019000110607     d  PosCurKSC                     1n   overlay(IndDspF : 51)
019100080215     d  PosCurOSF                     1n   overlay(IndDspF : 52)
019200110308     d  PosCurCic1                    1n   overlay(IndDspF : 53)
019300140915     d  PosCurBrtda                   1n   overlay(IndDspF : 54)
019400110308     d  PosCurCic2                    1n   overlay(IndDspF : 55)
019500140915     d  PosCurPRV                     1n   overlay(IndDspF : 56)
019600140915     d  PosCurCAP                     1n   overlay(IndDspF : 57)
019700110607     d  PosCurCic3                    1n   overlay(IndDspF : 58)
019800110607     d  PosCurCic4                    1n   overlay(IndDspF : 59)
019900110607     d  PosCurCic5                    1n   overlay(IndDspF : 60)
020000110308     d  PosCurFlt                     1n   overlay(IndDspF : 61)
020100110308     d  PosCurSCT1                    1n   overlay(IndDspF : 62)
020200080215     d  PosCurCAR                     1n   overlay(IndDspF : 63)
020300140915     d  PosCurBrta                    1n   overlay(IndDspF : 64)
020400110308     d  PosCurSct2                    1n   overlay(IndDspF : 65)
020500110607     d  PosCurRGF                     1n   overlay(IndDspF : 66)
020600110310     d  PosCurSct3                    1n   overlay(IndDspF : 67)
020700140915     d  PosCurNrfda                   1n   overlay(IndDspF : 68)
020800140915     d  PosCurNrfa                    1n   overlay(IndDspF : 69)
020900110310     d  PosCurPkgda                   1n   overlay(IndDspF : 70)
021000110310     d  PosCurPkga                    1n   overlay(IndDspF : 71)
021100110310     d  PosCurConc1                   1n   overlay(IndDspF : 72)
021200110310     d  PosCurConc2                   1n   overlay(IndDspF : 73)
021300110310     d  PosCurConc3                   1n   overlay(IndDspF : 74)
021400140915     d  PosCurNroda                   1n   overlay(IndDspF : 75)
021500140915     d  PosCurNroa                    1n   overlay(IndDspF : 76)
021600140915     d  PosCurNerda                   1n   overlay(IndDspF : 77)
021700140915     d  PosCurNera                    1n   overlay(IndDspF : 78)
021800140915     d  PosCurNaeda                   1n   overlay(IndDspF : 79)
021900140915     d  PosCurNaea                    1n   overlay(IndDspF : 80)
022000140915     d  PosCurNtrda                   1n   overlay(IndDspF : 81)
022100140915     d  PosCurNtra                    1n   overlay(IndDspF : 82)
022200140915     d  PosCurPaesi1                  1n   overlay(IndDspF : 83)
022300140915     d  PosCurPaesi2                  1n   overlay(IndDspF : 84)
022400140915     d  PosCurPaesi3                  1n   overlay(IndDspF : 85)
022500140915     d  PosCurInt                     1n   overlay(IndDspF : 86)
022600140915     d  PosCurDil1                    1n   overlay(IndDspF : 87)
022700140915     d  PosCurDil2                    1n   overlay(IndDspF : 88)
022800140915     d  PosCurLos                     1n   overlay(IndDspF : 89)
022900140915     d  PosCurConcl1                  1n   overlay(IndDspF : 90)
023000140915     d  PosCurConcl2                  1n   overlay(IndDspF : 91)
023100140915     d  PosCurConcl3                  1n   overlay(IndDspF : 92)
023200080206     d  errGenerico                   1n   overlay(IndDspF : 99)
023300080206
023400080206     d WindDspF        ds                  inz
023500080207     d   WindDspFa             1     49    inz(*zero)
023600080207     d   WindDspFb            50     99    inz(*zero)
023700080206
023800140916     d wlbdat          ds                  inz
023900140916     d  g02dat                 1      8  0
024000140916     d  g02inv                 9     16  0
024100140916     d  g02err                17     17
024200140916     d  g02tgi                18     22  0
024300110308
024400110308      // Campo unico per categoria merceologica
024500110308     d V01sct          ds                  inz
024600110308     d   V01sct1                           inz
024700110308     d   V01sct2                           inz
024800110308     d   V01sct3                           inz
024900110308
025000110308      // Campo unico per Importanza clienti
025100110308     d V01cic          ds                  inz
025200110308     d   V01cic1                           inz
025300110308     d   V01cic2                           inz
025400110308     d   V01cic3                           inz
025500110308     d   V01cic4                           inz
025600110308     d   V01cic5                           inz
025700140915
025800140915      // Campo unico per Paesi Estero
025900140915     d V02paesi        ds                  inz
026000140915     d   V02paesi1                         inz
026100140915     d   V02paesi2                         inz
026200140915     d   V02paesi3                         inz
026300110308
026400110308      // Campo unico per Concorrenti
026500140915     d V02conc         ds                  inz
026600140915     d   V02conc1                          inz
026700140915     d   V02conc2                          inz
026800140915     d   V02conc3                          inz
026900140915
027000140915      // Campo unico per Concorrenti Logistica
027100140915     d V02conl         ds                  inz
027200140915     d   V02concl1                         inz
027300140915     d   V02concl2                         inz
027400140915     d   V02concl3                         inz
027500110614
027600110614      // campi sql
027700110614     d cnacods       e ds                  extname(cnaco00f) prefix(a_)
027800110615     d                                     based(nullPtr)
027900110614     d cnindds       e ds                  extname(cnind00f) prefix(a_)
028000110615     d                                     based(nullPtr)
028100110614     d cnclpds       e ds                  extname(cnclp00f) prefix(a_)
028200110615     d                                     based(nullPtr)
028300110614     d CLI_SQL_ds      ds
028400110614     d  clpkut                             like(a_clpkut)
028500110614     d  clpkcc                             like(a_clpkcc)
028600110614     d  clpksc                             like(a_clpksc)
028700110614     d  clpclv                             like(a_clpclv)
028800110614     d  clpage                             like(a_clpage)
028900110614     d  acoflg                             like(a_acoflg)
029000110614     d  acoabl                             like(a_acoabl)
029100110614     d  acokut                             like(a_acokut)
029200110614     d  acokcc                             like(a_acokcc)
029300110614     d  acoksc                             like(a_acoksc)
029400110614     d  acorag                             like(a_acorag)
029500110614     d  acolib                             like(a_acolib)
029600110614     d  acoitc                             like(a_acoitc)
029700110614     d  indkut                             like(a_indkut)
029800110614     d  indkcc                             like(a_indkcc)
029900110614     d  indksc                             like(a_indksc)
030000110614     d  indvia                             like(a_indvia)
030100110614     d  indcit                             like(a_indcit)
030200110615     d  indcae                             like(a_indcae)
030300110614     d  indprv                             like(a_indprv)
030400110614     d  indcdf                             like(a_indcdf)
030500110614     d  indiva                             like(a_indiva)
030600110614
030700080206      // - Parametri ricevuti
030800080206     d KPJBA         e ds
030900110610     d TNTAI1ds      e ds
031000150112     d TRKC20ds      e ds
031100080206
031200080206      // - Reperimento dati utente
031300080206     d TIBS34ds      e ds
031400081020     d TRUL31DS      e ds
031500081020     d POG                    10    759    DIM(250)
031600110610
031700110610      // - Dettaglio interrogazione clienti
031800110610     d TNTAI2ds      e ds
031900080206
032000080206      // - Ricerca/Controllo tabelle
032100080206     d TIBS02ds      e ds                  inz
032200110309
032300110610      // - Ricerca unificante padre
032400110610     d TIBS10ds      e ds                  inz
032500110610     d  skc                   21   5520    dim(500)
032600110610
032700090701      // - controllo abilitazioni
032800090701     d TNTAA1DS      e ds                  inz
032900110509
033000110509      // - Ricerca tabella CPO
033100110509     d TNTB87ds      e ds                  inz
033200120202
033300120202       // -?DS Azorg per recupero NTW
033400120202     d OG143         e ds
033500080206
033600080206      // - Tabella 1L = Categorie Merceologiche
033700080207     d ds1L          e ds                  inz
033800110610
033900080208      // - Tabella 05 = Codici Area
034000080208     d ds05          e ds                  inz
034100110407
034200110407      // - Tabella CPO = Categoria potenziale
034300110407     d dCPO          e ds                  inz
034400110407
034500080206      // - Controllo congruenza CAP e provincia
034600080206     d FNLV13ds      e ds
034700080206     d   I13af0      e                     inz('S')
034800080206     d   I13cng      e                     inz('S')
034900080206     d TISI95ds      e ds                  inz
035000080206     d   I95tcn      e                     inz('3')
035100110610
035200080206      //---------------------------------------------------------------
035300080206      //?Definizione variabili globali.
035400080206      //---------------------------------------------------------------
035500080206
035600080206      // - Flags booleani
035700110310     d $Conc           s               n   inz(*off)
035800140915     d $Conclog        s               n   inz(*off)
035900110309     d $EndAtt         s               n   inz(*off)
036000110309     d $EndTtr         s               n   inz(*off)
036100110615     d $EndApp         s               n   inz(*off)
036200080208     d $Fine           s               n   inz(*off)
036300080208     d $InzD01         s               n   inz(*on)
036400080208     d $InzS01         s               n   inz(*on)
036500140915     d $InzD02         s               n   inz(*on)
036600080206     d $ErrGrave       s               n   inz(*off)
036700080207     d $EoF            s               n   inz(*off)
036800080208     d $RecOK          s               n   inz(*off)
036900140915     d $Paesi          s               n   inz(*off)
037000080206
037100080206      // - Indici di schiera
037200110610     d xx              s              4  0 inz
037300110610     d yy              s              4  0 inz
037400110613     d ww              s              4  0 inz
037500110613     d zz              s              4  0 inz
037600110610     d xpos            s              4  0 inz
037700080206
037800080207      // - Campi associati al video
037900080207     d $Video          s              2    inz('D1')
038000080208     d S01nrr          s              4  0 inz
038100080207     d W_SflPag        s              3  0 inz
038200080207     d wPag            s              4  0 inz
038300080207     d wRig            s              3  0 inz
038400110309
038500110309       // - Stringa SQL da eseguire
038600110309     d wSQL            s           2048    Varying        inz
038700080206
038800080206      // - Campi di comodo
038900080207     d w030a           s             30    inz
039000140915     d wdil1           s              8  0 inz
039100140915     d wdil2           s              8  0 inz
039200080207     d $Ord            s              3  0 inz
039300080626     d savkpjbu        s                   like(kpjbu)
039400100202     d w_damenu        s              1
039500100408     d w_daatti        s              1
039600100423     d DataSYS         s               d   inz(*sys)
039700100423     d Dataiso         s               d   datfmt(*iso)
039800100423     d Alfaoggi        s              8
039900100423     d W0080           s              8  0
040000110308     d C_tab           s                   like(TBEcod)
040100110309     d C_ke1           s                   like(TBEke1)
040200110310     d wval            s                   like(CPIval)
040300110310     d wpft            s                   like(CPIpft)
040400110609     d wCodUni         s              7  0 inz
040500110613     d Fltda           s              7  0 inz
040600110613     d Flta            s              7  0 inz
040700110615     d Fil_ksc         s              3  0 inz
040800110615     d w05_itc         s              5  0 inz
040900120202     d w0030           s              3  0 inz
041000110615
041100110615      // - Campi di comodo data
041200110615     d  data_iso       s               d   Datfmt(*iso)
041300110615     d  data_dmy       s               d   Datfmt(*dmy)
041400140916     d  data_eur       s               d   Datfmt(*eur)
041500110308
041600110308     d idTabella       s              2
041700110308     d Ordinamento     s              1
041800110308     d idElemento      s              8
041900110308     d TastoUscita     s              1
042000110601
042100110601     d wLib            s             21    inz
042200110601     d wTtr            s              4  0 inz
042300080206
042400080206      //---------------------------------------------------------------
042500080206      //?Definizione procedure usate.
042600080206      //---------------------------------------------------------------
042700080208
042800080208      // - Controllo cap/località
042900080208     d fnlv13r         pr                  extpgm('FNLV13R')
043000080208     d  kpjba                              likeds(kpjba)    const
043100080208     d  fnlv13ds                           likeds(FNLV13ds)
043200080208     d  tisi95ds                           likeds(TISI95ds)
043300110509
043400110509      // - interrogazione tabella CPO
043500110509     d TNTB87R         pr                  extpgm('TNTB87R')
043600110509     d  tntb87ds                           likeds(TNTB87ds)
043700110609
043800110610      // - Interrogazione dettaglio cliente
043900110610     d tntai2r         pr                  extpgm('TNTAI2R')
044000110610     d  kpjba                              likeds(KPJBA)
044100110610     d  tntai2ds                           likeds(tntai2ds)
044200110610
044300110609      // - File clienti
044400080626      //---------------------------------------------------------------
044500080626      //?prototipi
044600080626      //---------------------------------------------------------------
044700080626      /copy gaitrasrc/srcprotopr,tibs02r
044800110610      /copy gaitrasrc/srcprotopr,tibs10r
044900080626      /copy gaitrasrc/srcprotopr,tibs34r
045000080626      /copy gaitrasrc/srcprotopr,trmk07r
045100081022      /copy gaitrasrc/srcprotopr,trmk51r
045200130801      /copy gaitrasrc/srcprotoPI,TRMK43R_1
045300130801      /copy gaitrasrc/srcprotoPR,TRMK43R
045400110308      /copy gaitrasrc/srcprotopr,trul19r
045500081020      /copy gaitrasrc/srcprotopr,trul31r
045600080626      /copy gaitrasrc/srcprotopr,x§taber
045700081017      /copy gaitrasrc/srcprotopr,xalfa3br
045800140916      /copy gaitrasrc/srcprotopr,xsrda8
045900090703      /copy gaitrasrc/srcprotopr,TNTAA1C
046000080206      //---------------------------------------------------------------
046100080206      //?Definizione key-list.
046200080206      //---------------------------------------------------------------
046300110607
046400110607       // - File CNACO00F
046500110607     d k03acocod     e ds                  extname(CNACO00F:*key)
046600110607     d                                     prefix(k00_)
046700110607
046800110607       // - File CNACO04L
046900110607     d k03acorag     e ds                  extname(CNACO04L:*key)
047000110607     d                                     prefix(k04_)
047100110607
047200110607       // - File CNCLP00F
047300110607     d k03cnclp      e ds                  extname(CNCLP00F:*key)
047400110607     d                                     prefix(k_)
047500110609       // - File CNIND00F
047600110609     d k03cnind      e ds                  extname(CNIND00F:*key)
047700110609     d                                     prefix(k_)
047800110607
047900110607       // - File TABEL00F
048000110607     d k03tabel      e ds                  extname(TABEL00F:*key)
048100110607     d                                     prefix(k_)
048200130801
048300130801       // -?File AZCMM01L
048400130801     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
048500130801     d                                     prefix(k_)
048600080206
048700080206      //---------------------------------------------------------------
048800080206      //?M A I N - L I N E
048900080206      //---------------------------------------------------------------
049000080206
049100080206     c     *Entry        plist
049200080206     c                   parm                    KPJBA
049300110607     c                   parm                    tntai1ds
049400150112     c                   parm                    TRKC20ds
049500080206
049600080206      /free
049700080206
049800080206       // Operazioni iniziali
049900080206       exsr RoutInz;
050000110309
050100110309       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
050200080206
050300080206       // Gestione video
050400080206       DOW $Fine = *off;
050500080206         select;
050600080206           when $Video = 'D1';
050700080206             exsr GesD01;
050800140915           when $Video = 'D2';
050900140915             exsr GesD02;
051000080206           when $Video = 'S1';
051100080206             exsr GesS01;
051200080206           other;
051300080206             $Fine = *on;
051400080206         endsl;
051500080206       ENDDO;
051600080206
051700080206       // Operazioni finali
051800080206       exsr RoutEnd;
051900080206
052000080206       //--------------------------------------------------------------
052100080206       //?Operazioni iniziali.
052200080206       //--------------------------------------------------------------
052300080206       BEGSR RoutInz;
052400080206
052500110607       //?Pulisco campi output
052600110607         clear OTAI1ksc;
052700110607         clear OTAI1f03;
052800110607         clear OTAI1f12;
052900110607         clear OTAI1err;
053000110607         clear OTAI1msg;
053100110607
053200110607       //?Impostazione campi "fissi"
053300110607         T01pgm = SDSpgm;
053400110607         TBLkut = 1;
053500110607
053600110607       //?Reperimento dati job
053700110607         exsr DatiJob;
053800110607
053900110607       //?Carico filiali abilitate al profilo con AUT
054000110607       //?gestione tariffe clienti (arrivata da pgm chiamante)
054100110607         clear TRUL31DS;
054200110607         I31abi = ITAI1abi;
054300110607         I31cdi = DUTdis;
054400110607         I31car = DUTare;
054500110607         I31cpo = DUTpou;
054600110607         callp trul31r (kpjba : trul31ds);
054700110607         IF  O31pog <= *zeros;
054800110607           $Fine    = *on;
054900110607           OTAI1err = '1';
055000110607           OTAI1msg = $Msg(01);
055100110607           leavesr;
055200110607         ENDIF;
055300110607
055400130801       // -?Preparazione (da AZCMM) dell'elenco dei commerciali:?
055500110607       //  ?1) di filiale abilitata?
055600110607       //  ?2) con comm.le unificante di filiale abilitata?
055700110607       //  ?(SE ricevuta apposita richiesta dal chiamante)?
055800110607         clear $Age;
055900110607         clear xx;
056000110607         clear yy;
056100110607
056200110607         IF  ITAI1aut <> *blanks;
056300110607
056400130801           setll  (*loval)  AZCMM000;
056500130801           read   AZCMM000;
056600130801           DOW  NOT %eof(AZCMM01L);
056700130801             if  %lookup(%subst(%editc(CMMcod:'X'):1:3):POG) > *zero  or
056800130801                 %lookup(%subst(%editc(CMMuni:'X'):1:3):POG) > *zero;
056900110607               xx += 1;
057000130801               $Age(xx) = CMMcod;
057100110607               yy += 1;
057200130801               $AgeU(yy) = CMMuni;
057300110621             endif;
057400130801             read   AZCMM000;
057500110607           ENDDO;
057600110607
057700110607         ENDIF;
057800080207
057900080207         // Intabellamento filiali e relative aree
058000110610         clear xx;
058100080207         setll *loval AZORG;
058200080207         read  AZORG;
058300080208         dow  NOT  %eof(AZORG01L);
058400080208           if  ORGfva = *blank;
058500080207             xx += 1;
058600080208             $Fil(xx) = ORGfil;
058700080208             $Car(xx) = ORGcar;
058800080207           endif;
058900080207           read  AZORG;
059000080207         enddo;
059100110308
059200110308         //?Carico sk Categoria merceologica
059300110308         clear xx;
059400110308         TBLcod = '1L';
059500110308         setll (TBLkut:TBLcod) TABEL00F;
059600110308         reade (TBLkut:TBLcod) TABEL00F;
059700110308
059800110308         DOW not %eof(TABEL00F);
059900110308           IF  TBLflg = *blanks;
060000110308             xx += 1;
060100110309             $1l(xx) = %subst(TBLkey:1:5);
060200110308           ENDIF;
060300110308           reade (TBLkut:TBLcod) TABEL00F;
060400110308         ENDDO;
060500110308
060600110308         //?Carico sk Importanza Clienti
060700110308         clear xx;
060800110309         TBLcod = 'IC';
060900110308         setll (TBLkut:TBLcod) TABEL00F;
061000110308         reade (TBLkut:TBLcod) TABEL00F;
061100110308
061200110308         DOW not %eof(TABEL00F);
061300110308           IF  TBLflg = *blanks;
061400110308             xx += 1;
061500110309             $ic(xx) = %subst(TBLkey:1:1);
061600110308           ENDIF;
061700110308           reade (TBLkut:TBLcod) TABEL00F;
061800110308         ENDDO;
061900110308
062000110308         //?Carico sk Concorrenti
062100110308         clear xx;
062200110308         C_Tab = 'IFS';
062300110309         C_ke1 = '10';
062400110309         setll (C_tab:C_ke1) TNTBE01L;
062500110309         reade (C_tab:C_ke1) TNTBE01L;
062600110308
062700110308         DOW not %eof(TNTBE01L);
062800110308           IF  TBEatb = *blanks;
062900110308             xx += 1;
063000110308             $Ifs(xx) = %subst(TBEke2:1:4);
063100110308           ENDIF;
063200110309           reade (C_tab:C_ke1) TNTBE01L;
063300110308         ENDDO;
063400140915
063500140915         //?Carico sk Concorrenti LOGISTICA
063600140915         clear xx;
063700140915         C_Tab = 'IFS';
063800140915         C_ke1 = '40';
063900140915         setll (C_tab:C_ke1) TNTBE01L;
064000140915         reade (C_tab:C_ke1) TNTBE01L;
064100140915
064200140915         DOW not %eof(TNTBE01L);
064300140915           IF  TBEatb = *blanks;
064400140915             xx += 1;
064500140915             $IfsLOG(xx) = %subst(TBEke2:1:4);
064600140915           ENDIF;
064700140915           reade (C_tab:C_ke1) TNTBE01L;
064800140915         ENDDO;
064900140915
065000140915         //?Carico sk Paesi Estero
065100140915         clear xx;
065200140915         C_Tab = 'IFS';
065300140915         C_ke1 = '50';
065400140915         setll (C_tab:C_ke1) TNTBE01L;
065500140915         reade (C_tab:C_ke1) TNTBE01L;
065600140915
065700140915         DOW not %eof(TNTBE01L);
065800140915           IF  TBEatb = *blanks;
065900140915             xx += 1;
066000140915             $IfsPAESI(xx) = %subst(TBEke2:1:4);
066100140915           ENDIF;
066200140915           reade (C_tab:C_ke1) TNTBE01L;
066300140915         ENDDO;
066400100423
066500100423          dataiso=datasys         ;
066600100423          w0080=%dec(dataiso)     ;
066700100423          Alfaoggi=%editc(w0080:'X')  ;
066800141201
066900141201         // -?Richiamato con parametri aggiuntivi (parzializzazioni)?
067000150109         If  %parms() > 2;
067100150109
067200141201           exsr  sr_ParmsCaller;
067300150109           $InzD01 = *off;
067400150109           $InzD02 = *off;
067500150109
067600141201           exsr  CtrD01;
067700150109           if  errMessage;
067800150109             $Fine    = *on;
067900150109             oTAI1err = *on;
068000150109             oTAI1msg = V1Dmsg;
068100141201             leavesr;
068200141201           endif;
068300141202           exsr  CtrD02;
068400150109           if  errMessage;
068500150109             $Fine    = *on;
068600150109             oTAI1err = *on;
068700150109             oTAI1msg = V1Dmsg;
068800141202             leavesr;
068900141202           endif;
069000150109           exsr  InzS01;
069100150109           if  S01nrr = *zero;
069200150109             $Fine    = *on;
069300150109             oTAI1err = *on;
069400150109             oTAI1msg = V1Dmsg;
069500150109             leavesr;
069600150109           endif;
069700150109
069800141201           $Video  = 'S1';
069900150109           $InzS01 = *off;
070000150109
070100150109         EndIf;
070200080206
070300080206       ENDSR;
070400080206
070500080206       //--------------------------------------------------------------
070600080206       //?Reperimento Dati del job (Utente/Operativi).
070700080206       //--------------------------------------------------------------
070800080206       BEGSR DatiJob;
070900080206
071000080206         in(E) §AzUte;
071100080206         if NOT %error;
071200080206           in(E) §DatiUte;
071300080206         endif;
071400080206         if %error or RSut = *blanks;
071500080206           clear TIBS34ds;
071600080206           tibs34r(tibs34ds);
071700080206           in §AzUte;
071800080206           in §DatiUte;
071900080206         endif;
072000080206
072100080206       ENDSR;
072200141201
072300141201       //--------------------------------------------------------------
072400141201       //?Impostazione a video parametri aggiuntivi ricevuti.          ?
072500141201       //--------------------------------------------------------------
072600141201       BEGSR  sr_ParmsCaller;
072700141201
072800141201         clear  TAI1D01;
072900141202         clear  TAI1D02;
073000141202
073100150112         V01ksc  = KC20ksc;
073200150112         V01osf  = KC20osf;
073300150112         V01rag  = KC20rag;
073400150112         V01cic1 = KC20cic1;
073500150112         V01cic2 = KC20cic2;
073600150112         V01cic3 = KC20cic3;
073700150112         V01cic4 = KC20cic4;
073800150112         V01cic5 = KC20cic5;
073900150112         V01loc  = KC20loc;
074000150112         V01prv  = KC20prv;
074100150112         V01cap  = KC20cap;
074200150112         V01flt  = KC20flt;
074300150112         if  KC20car  <> *zero;
074400150112           V01car  = %editc( KC20car : 'X' );
074500141201         endif;
074600150112         V01piv  = KC20piv;
074700150112         V01cdf  = KC20cdf;
074800150112         if  KC20sct1 <> *zero;
074900150112           V01sct1 = %editc( KC20sct1 : 'X' );
075000141201         endif;
075100150112         if  KC20sct2 <> *zero;
075200150112           V01sct2 = %editc( KC20sct2 : 'X' );
075300141201         endif;
075400150112         if  KC20sct3 <> *zero;
075500150112           V01sct3 = %editc( KC20sct3 : 'X' );
075600141201         endif;
075700150112         if  KC20cmu  <> *zero;
075800150112           V01rgf  = %editc( KC20cmu : 'X' );
075900141201         endif;
076000150112         if  KC20cmu <> *zero;
076100150112           V01rgf  = %editc( KC20cmu : 'X' );
076200141202         endif;
076300150112         V01uni  = KC20uni;
076400150112         V01blc  = KC20blc;
076500150112         V01ttr  = KC20ttr;
076600141202
076700150112         V02brtDa  = KC20brtI;
076800150112         V02brtA   = KC20brtF;
076900150112         V02nrfDa  = KC20nrfI;
077000150112         V02nrfA   = KC20nrfF;
077100150112         V02nroDa  = KC20nroI;
077200150112         V02nroA   = KC20nroF;
077300150112         V02nerDa  = KC20nerI;
077400150112         V02nerA   = KC20nerF;
077500150112         V02naeDa  = KC20naeI;
077600150112         V02naeA   = KC20naeF;
077700150112         V02ntrDa  = KC20ntrI;
077800150112         V02ntrA   = KC20ntrF;
077900150112         V02paesi1 = KC20est1;
078000150112         V02paesi2 = KC20est2;
078100150112         V02paesi3 = KC20est3;
078200150112         V02pkgDa  = KC20pkgI;
078300150112         V02pkgA   = KC20pkgF;
078400150112         V02she    = KC20shp;
078500150112         V02secm   = KC20secm;
078600150112         V02sbx    = KC20sbx;
078700150112         V02conc1  = KC20conc1;
078800150112         V02conc2  = KC20conc2;
078900150112         V02conc3  = KC20conc3;
079000150112         V02int    = KC20int;
079100150112         V02dil1   = KC20dil1;
079200150112         V02dil2   = KC20dil2;
079300150112         V02los    = KC20los;
079400150112         V02concL1 = KC20conL1;
079500150112         V02concL2 = KC20conL2;
079600150112         V02concL3 = KC20conL3;
079700141201
079800141201       ENDSR;
079900080206
080000080206       //--------------------------------------------------------------
080100080206       //?Gestione videata D01
080200080206       //--------------------------------------------------------------
080300080206       BEGSR GesD01;
080400080206
080500080206         // Inizializzazione videata
080600080206         if $InzD01 = *on;
080700080206           exsr InzD01;
080800080206           $InzD01 = *off;
080900080214         else;
081000080225           if  errGenerico = *off;
081100110608             clear V01ksc;
081200080225           endif;
081300080206         endif;
081400080206
081500080206         // Emissione testata
081600110615           write TAI1T01;
081700090702
081800090702         // Se errore grave, ovvero utente non abilitato, emetto msg ed esco dal
081900090702         if $ErrGrave=*on    ;
082000090702           errMessage  = *on;
082100090702           errGenerico = *on;
082200090702           V1Dmsg = $Msg(01);
082300090702         endif   ;
082400080206
082500080206         // Emissione videata
082600110615         exfmt TAI1D01;
082700080206         errMessage   = *off;
082800080206         errGenerico  = *off;
082900080206         clear V1Dmsg;
083000080207
083100080206         select;
083200080206         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
083300080206           when  $ErrGrave = *on;
083400110608             Otai1f03 = *on;
083500080206             $Fine  = *on;
083600110308
083700080206         // F3=Fine
083800080208           when dsp_aid = c_F03;
083900080206             exsr F03D01;
084000140915
084100140915         // F5=Altre Prazializzazioni
084200140915           when dsp_aid = c_F05;
084300140915             exsr CtrD01;
084400140915             if errGenerico;
084500140915               leavesr;
084600140915             endif;
084700140915             exsr F05D01;
084800140915
084900140915         // F11=Pulizia
085000140915           when dsp_aid = c_F11;
085100140915             exsr F11D01;
085200081021
085300080206         // Enter
085400080206           other;
085500080206             exsr CtrD01;
085600080206             if errGenerico;
085700080206               leavesr;
085800080206             endif;
085900110308
086000080208               $Video = 'S1';
086100110308
086200080206             $InzS01 = *on;
086300080206         endsl;
086400080206
086500080206       ENDSR;
086600080206
086700080206       //--------------------------------------------------------------
086800080206       //?Inizializzazione videata D01
086900080206       //--------------------------------------------------------------
087000080206       BEGSR InzD01;
087100080206
087200110615         clear TAI1D01;
087300110608
087400110608         V01osf  = 'I';
087500110608         V01uni  = 'S';
087600110608         V01blc  = 'N';
087700080206
087800080206       ENDSR;
087900140915
088000140915       //--------------------------------------------------------------
088100140915       //?Gestione videata D02
088200140915       //--------------------------------------------------------------
088300140915       BEGSR GesD02;
088400140915
088500140915         // Inizializzazione videata
088600140915         if $InzD02 = *on;
088700140915           exsr InzD02;
088800140915           $InzD02 = *off;
088900140915         endif;
089000140915
089100140915         // Emissione testata
089200140915           write TAI1T01;
089300140915
089400140915         // Emissione videata
089500140915         exfmt TAI1D02;
089600140915         errMessage   = *off;
089700140915         errGenerico  = *off;
089800140915         clear V2Dmsg;
089900140915
090000140915         select;
090100140915         // Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
090200140915           when  $ErrGrave = *on;
090300140915             OTAI1f03 = *on;
090400140915             $Fine  = *on;
090500140915
090600140915         // Enter
090700140915           other;
090800140915             exsr CtrD02;
090900140915             if errGenerico;
091000140915               leavesr;
091100140915             endif;
091200140915
091300140915             $Video = 'D1';
091400140915             $InzS01 = *on;
091500140915
091600140915         endsl;
091700140915
091800140915       ENDSR;
091900140915
092000140915       //--------------------------------------------------------------
092100140915       //?Inizializzazione videata D02
092200140915       //--------------------------------------------------------------
092300140915       BEGSR InzD02;
092400140915
092500140916         clear TAI1D02;
092600140915         clear wdil1;
092700140915         clear wdil2;
092800140915
092900140915       ENDSR;
093000080206
093100080206       //--------------------------------------------------------------
093200080206       //?Gestione tasto funzionale F3 da videata D01
093300080206       //?F3=Fine
093400080206       //--------------------------------------------------------------
093500080206       BEGSR F03D01;
093600080206
093700080207         // Chiusura del programma
093800080206         $Fine = *on;
093900080206
094000080206         // Restituzione indicazione del tasto funzionale premuto
094100110608         Otai1f03 = *on;
094200080206
094300080206       ENDSR;
094400140915
094500140915       //--------------------------------------------------------------
094600140915       //?Gestione tasto funzionale F5 da videata D01
094700140915       //?F5=Altre parzializzazioni
094800140915       //--------------------------------------------------------------
094900140915       BEGSR F05D01;
095000140915
095100140915       //?Videata sucessiva
095200140915       //?Se l'infdicatore di altre parzializzazioni è spento pulisco il viedo a
095300140915         IF  not AltreParz;
095400140915           $InzD02 = *on;
095500140915         ENDIF ;
095600140915
095700140915         $Video = 'D2';
095800140915
095900140915       ENDSR;
096000140915
096100140915       //--------------------------------------------------------------
096200140915       //?Gestione tasto funzionale F11 da videata D01
096300140915       //?F11=Pulizia
096400140915       //--------------------------------------------------------------
096500140915       BEGSR F11D01;
096600140915
096700140915       //?Pulizia delle videate 1 e 2
096800140915         $Video = 'D1';
096900140915         exsr Inzd01;
097000140915         exsr InzD02;
097100140915
097200140915         AltreParz = *off;
097300140915
097400140915       ENDSR;
097500080206
097600080206       //--------------------------------------------------------------
097700080206       //?Controllo videata D01
097800080206       //--------------------------------------------------------------
097900080206       BEGSR CtrD01;
098000080206
098100080206         WindDspF = IndDspF;
098200080206         reset WindDspFb;
098300080206         IndDspF  = WindDspF;
098400110309
098500110608       //?Se richiesto codice potenziale
098600110608       //?finisco qua i controlli
098700110608         IF  V01ksc > *zeros;
098800110613           $Ord   = 1;
098900110608           leavesr;
099000110608         ENDIF;
099100110608
099200110309       //?Ordinamento Elenco
099300110309         IF  V01osf <> 'R'   and
099400110309             V01osf <> 'I';
099500110309           ErrMessage  = *on;
099600110309           ErrGenerico = *on;
099700110309           PosCurOSF   = *on;
099800110309           V1Dmsg = $Msg(07);
099900110309           leavesr;
100000110309         ENDIF;
100100080206
100200080206         SELECT;
100300110309
100400110309         //?Se inserita ragione sociale no ordinamento per importanza cliente
100500110601         //?per ora lo tolgo
100600110309           WHEN  V01rag <> *blanks and V01osf <> 'R';
100700110308
100800110308         //?Se inseriti dati info commerciali obbligo 1 dato delle parzializzazi
100900140916          // WHEN   (V01treda > *zeros or V01trea > *zeros or
101000140916             //    V01brtda > *zeros or V01brta > *zeros or
101100140916             //    V01pkgda > *zeros or V01pkga > *zeros or
101200140916             //    V01she <> *blanks or V01conc <> *blanks or
101300140916             //    V01sbx <> *blanks)  and
101400140915             //    V01rag = *blanks and V01loc = *blanks and
101500140915             //    V01prv = *blanks and V01cap = *blanks and
101600140915             //    V01flt = *zeros  and V01car = *blanks and
101700140915             //    V01piv = *blanks and V01cdf = *blanks and
101800140915             //    V01sct = *blanks and V01cic = *blanks;
101900140915          // ErrMessage  = *on;
102000140915          // ErrGenerico = *on;
102100140915          // PosCurOSF   = *on;
102200140915          // V1Dmsg      = $Msg(04);
102300140915          // leavesr;
102400110601
102500110601         //?Se inserita selezione per trattativa obbligo 1 dato delle parzializz
102600110601           WHEN   V01ttr <> *blanks and
102700110601                   V01rag = *blanks and V01loc = *blanks and
102800110601                   V01prv = *blanks and V01cap = *blanks and
102900110601                   V01flt = *zeros  and V01car = *blanks and
103000110601                   V01piv = *blanks and V01cdf = *blanks and
103100110601                   V01sct = *blanks and V01cic = *blanks;
103200110601             ErrMessage  = *on;
103300110601             ErrGenerico = *on;
103400110616             PosCurOSF   = *on;
103500110616             V1Dmsg      = $Msg(12);
103600110601             leavesr;
103700110601         ENDSL;
103800110308
103900110308       //?Importanza Cliente
104000110308         IF  V01cic1 <> *blanks;
104100110308           IF    %scan('?' : V01cic1) > 0;
104200110308             ErrGenerico = *on;
104300110308             PosCurCic1  = *on;
104400110308             idTabella = 'IC';
104500110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
104600110308                            tastoUscita);
104700110309             V01cic1 = idElemento;
104800110308           ENDIF;
104900110308           IF  %lookup(V01cic1 : $Ic) = 0;
105000110308             ErrMessage  = *on;
105100110308             ErrGenerico = *on;
105200110308             PosCurCic1  = *on;
105300110309             V1Dmsg = $Msg(17);
105400110308             leavesr;
105500110308           ENDIF;
105600110308         ENDIF;
105700110308         IF  V01cic2 <> *blanks;
105800110308           IF    %scan('?' : V01cic2) > 0;
105900110308             ErrGenerico = *on;
106000110308             PosCurCic2  = *on;
106100110308             idTabella = 'IC';
106200110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
106300110308                            tastoUscita);
106400110308             V01cic2 = idElemento;
106500110308           ENDIF;
106600110308           IF  %lookup(V01cic2 : $Ic) = 0;
106700110308             ErrMessage  = *on;
106800110308             ErrGenerico = *on;
106900110308             PosCurCic2  = *on;
107000110309             V1Dmsg = $Msg(17);
107100110308             leavesr;
107200110308           ENDIF;
107300110308         ENDIF;
107400110308         IF  V01cic3 <> *blanks;
107500110308           IF    %scan('?' : V01cic3) > 0;
107600110308             ErrGenerico = *on;
107700110308             PosCurCic3  = *on;
107800110308             idTabella = 'IC';
107900110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
108000110308                            tastoUscita);
108100110308             V01cic3 = idElemento;
108200110308           ENDIF;
108300110308           IF  %lookup(V01cic3 : $Ic) = 0;
108400110308             ErrMessage  = *on;
108500110308             ErrGenerico = *on;
108600110308             PosCurCic3  = *on;
108700110309             V1Dmsg = $Msg(17);
108800110308             leavesr;
108900110308           ENDIF;
109000110308         ENDIF;
109100110308         IF  V01cic4 <> *blanks;
109200110308           IF    %scan('?' : V01cic4) > 0;
109300110308             ErrGenerico = *on;
109400110308             PosCurCic4  = *on;
109500110308             idTabella = 'IC';
109600110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
109700110308                            tastoUscita);
109800110308             V01cic4 = idElemento;
109900110308           ENDIF;
110000110308           IF  %lookup(V01cic4 : $Ic) = 0;
110100110308             ErrMessage  = *on;
110200110308             ErrGenerico = *on;
110300110308             PosCurCic4  = *on;
110400110309             V1Dmsg = $Msg(17);
110500110308             leavesr;
110600110308           ENDIF;
110700110308         ENDIF;
110800110308         IF  V01cic5 <> *blanks;
110900110308           IF    %scan('?' : V01cic5) > 0;
111000110308             ErrGenerico = *on;
111100110308             PosCurCic5  = *on;
111200110308             idTabella = 'IC';
111300110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
111400110308                            tastoUscita);
111500110308             V01cic5 = idElemento;
111600110308           ENDIF;
111700110308           IF  %lookup(V01cic5 : $Ic) = 0;
111800110308             ErrMessage  = *on;
111900110308             ErrGenerico = *on;
112000110308             PosCurCic5  = *on;
112100110309             V1Dmsg = $Msg(17);
112200110308             leavesr;
112300110308           ENDIF;
112400110308         ENDIF;
112500110322         IF  V01cic <> *blanks;
112600110322           clear $cic;
112700110322           $cic(01) = V01cic1;
112800110322           $cic(02) = V01cic2;
112900110322           $cic(03) = V01cic3;
113000110322           $cic(04) = V01cic4;
113100110322           $cic(05) = V01cic5;
113200110322         ENDIF;
113300110308
113400110308         // Se inserito il cap o la località è obbligatoria la provincia
113500110308           IF    (V01cap <> *blank  or  V01loc <> *blanks)  and
113600110308                  V01prv  = *blank;
113700110308             errMessage  = *on;
113800110308             errGenerico = *on;
113900110308             PosCurPRV   = *on;
114000110308             V1Dmsg      = $Msg(03);
114100110308             leavesr;
114200110308           ENDIF;
114300080206
114400080206         // Se inserito cap e provincia controllo se sono congruenti
114500110308           if  V01cap <> *blank;
114600080206             reset  fnlv13ds;
114700080206             reset  tisi95ds;
114800110309             I95cap = V01cap;
114900110309             I95prv = V01prv;
115000080206             fnlv13r(kpjba : fnlv13ds : tisi95ds);
115100080206             if  O13err <> *blank;
115200080206               errMessage  = *on;
115300080206               errGenerico = *on;
115400080206               select;
115500080206                 when  O13err = '4';
115600080206                   PosCurPRV   = *on;
115700080206                 when  O13err = '5';
115800080206                   PosCurCAP   = *on;
115900080206               endsl;
116000080206               V1Dmsg = O13msg;
116100080206               leavesr;
116200080206             endif;
116300080206           endif;
116400110308
116500110308         //?Filiale appartenenza
116600110308         IF  V01flt > 0;
116700110308           chain V01flt AZORG01L;
116800110308           IF  not %found(AZORG01L) or ORGfva <> *blanks;
116900110308             ErrMessage  = *on;
117000110308             ErrGenerico = *on;
117100110308             PosCurFlt   = *on;
117200110308             V1Dmsg      = $Msg(16);
117300110308             leavesr;
117400110308           ENDIF;
117500110308         ENDIF;
117600110308
117700110308         // Ricerca/Controllo/Decodifica codice area
117800110308         clear  V01carD;
117900110308         TBLcod = '05';
118000110308         TBLkey = V01car;
118100110308         if  V01car = *zero;
118200110308           clear  V01car;
118300110308         endif;
118400110308         SELECT;
118500110308         // - Ricerca
118600110308           WHEN  %scan('?' : V01car) > *zero;
118700110308             errGenerico = *on;
118800110308             PosCurCAR  = *on;
118900110308             clear w030a;
119000110308             x§taber(TBLkut : TBLcod : TBLkey : w030a);
119100110308         // - - F3 invece di scelta codice
119200110308             if  TBLkey = *blank;
119300110308               clear  V01car;
119400110308         // - - effettuata scelta
119500110308             else;
119600110308               V01car   = TBLkey;
119700110309               V01carD  = %subst(w030a : 1 : %len(V01carD));
119800110308             endif;
119900110308             leavesr;
120000110308         // - Controllo validità del codice area
120100110308           WHEN  V01car <> *blank;
120200110308             clear  ds05;
120300110308             chain (TBLkut : TBLcod : TBLkey) TABEL;
120400110308             if  NOT  %found(TABEL00F)
120500110308                 or   TBLflg <> *blank;
120600110308               errMessage  = *on;
120700110308               errGenerico = *on;
120800110308               PosCurCAR   = *on;
120900110308               V1Dmsg = $Msg(06);
121000110308               leavesr;
121100110308             else;
121200110308               ds05    = TBLuni;
121300110308               V01carD = §05des;
121400110308             endif;
121500110308         ENDSL;
121600050519
121700110308       //?Categoria merceologica
121800110615         IF  V01sct1 <> *blanks and V01sct1 <> *zeros ;
121900110308           IF    %scan('?' : V01sct1) > 0;
122000110308             ErrGenerico = *on;
122100110308             PosCurSct1  = *on;
122200110309             idTabella = '1L';
122300110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
122400110308                            tastoUscita);
122500110308             V01sct1 = idElemento;
122600110308           ENDIF;
122700110308           IF  %lookup(V01sct1 : $1l) = 0;
122800110308             ErrMessage  = *on;
122900110308             ErrGenerico = *on;
123000110308             PosCurSct1  = *on;
123100110309             V1Dmsg = $Msg(05);
123200110308             leavesr;
123300110308           ENDIF;
123400110308         ENDIF;
123500110615         IF  V01sct2 <> *blanks and V01sct2 <> *zeros ;
123600110308           IF    %scan('?' : V01sct2) > 0;
123700110308             ErrGenerico = *on;
123800110308             PosCurSct2  = *on;
123900110309             idTabella = '1L';
124000110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
124100110308                            tastoUscita);
124200110308             V01sct2 = idElemento;
124300110308           ENDIF;
124400110308           IF  %lookup(V01sct2 : $1l) = 0;
124500110308             ErrMessage  = *on;
124600110308             ErrGenerico = *on;
124700110308             PosCurSct2  = *on;
124800110309             V1Dmsg = $Msg(05);
124900110308             leavesr;
125000110308           ENDIF;
125100110308         ENDIF;
125200110615         IF  V01sct3 <> *blanks and V01sct3 <> *zeros ;
125300110308           IF    %scan('?' : V01sct3) > 0;
125400110308             ErrGenerico = *on;
125500110308             PosCurSct3  = *on;
125600110309             idTabella = '1L';
125700110308             Tabel_Ricerca (idTabella : Ordinamento : idElemento :
125800110308                            tastoUscita);
125900110308             V01sct3 = idElemento;
126000110308           ENDIF;
126100110308           IF  %lookup(V01sct3 : $1l) = 0;
126200110308             ErrMessage  = *on;
126300110308             ErrGenerico = *on;
126400110308             PosCurSct3  = *on;
126500110309             V1Dmsg = $Msg(05);
126600110308             leavesr;
126700110308           ENDIF;
126800110308         ENDIF;
126900080213
127000110609         // Comm.le Unificante
127100110609         select ;
127200110609           When v01rgf = *blanks or v01rgf = *zeros ;
127300110609
127400130801           // -?Commerciale unificante?
127500110609           When  %scan( '?' : V01rgf ) > *zero;
127600130801             clear  TRMK43ds;
127700130801             iMK43solU = 'S';
127800130801             iMK43fil  = DUTpou;
127900130801             TRMK43R (kpjba : TRMK43ds);
128000130801             if  oMK43err = *off  and  oMK43fxx = *blank;
128100130801               V01rgf  = %editc( oMK43cmm : 'X' );
128200130801               V01rgfd = oMK43des;
128300130801             endif;
128400110609             ErrGenerico = *on;
128500110609             PosCurRgf   = *on;
128600110609             leavesr;
128700110609
128800110609         // controllo numericità
128900110610           When  %check( c_Digits : V01rgf ) > *zero;
129000110609             ErrMessage  = *on;
129100110609             ErrGenerico = *on;
129200110609             PosCurRgf   = *on;
129300110609             V1Dmsg = $Msg(24);
129400110609             leavesr;
129500110609           // -?Controllo validità?
129600110609           Other;
129700130801             k_CMMcod = %int(V01rgf);
129800130801             chain  %kds(k_azcmm01)  AZCMM000;
129900130801             if  NOT %found(AZCMM01L);
130000110609               ErrMessage  = *on;
130100110609               ErrGenerico = *on;
130200110609               PosCurRgf   = *on;
130300110609               V1Dmsg = $Msg(24);
130400110609               leavesr;
130500110609             endif;
130600110609           // verifico se codice attivo
130700130801             IF  CMMPAR <> ' ' ;
130800110609               ErrMessage  = *on;
130900110609               ErrGenerico = *on;
131000110609               PosCurRgf   = *on;
131100110609               V1Dmsg      = $Msg(02);
131200110609              leavesr;
131300110609             ENDIF;
131400110609
131500130801             v01rgfd = CMMdes ;
131600110609         EndSl;
131700110609
131800110609         IF  V01rgf > *zero;
131900110609
132000110609           // -?Abilitazione utente al commerciale?
132100110609           reset  TNTAA1ds;
132200110609           ITAA1caut = '§utegtc';
132300110609           ITAA1cmm  = %int( V01rgf );
132400110609           kpjbu     = TNTAA1ds;
132500110609           tntaa1c (kpjba);
132600110609           TNTAA1ds = kpjbu;
132700110609           if  oTAA1fabi = 'N';
132800110609             ErrMessage  = *on;
132900110609             ErrGenerico = *on;
133000110609             PosCurRgf   = *on;
133100110609             V1Dmsg = $Msg(24);
133200110609             leavesr;
133300110609           endif;
133400110609
133500110609           // -?Selezionabili SOLO unificanti?
133600130801           if  %int( V01rgf ) <> CMMuni;
133700110609             ErrMessage  = *on;
133800110609             ErrGenerico = *on;
133900110609             PosCurRgf   = *on;
134000110609             V1Dmsg = $Msg(25);
134100110609             leavesr;
134200110609           endif;
134300110609
134400110609         ENDIF;
134500110609
134600080206         // Imposta il tipo selezioni da eseguire:
134700110310
134800080207         clear $Ord;
134900080207         select;
135000110609           // 1 = Singolo codice cliente
135100110608           when  V01ksc <> *zero;
135200110520
135300110520           // 2 = per Ragione Sociale se richiesto ordinamento x Ragione sociale
135400110520           when  V01osf = 'R';
135500110520             $Ord   = 2;
135600110520
135700080206         endsl;
135800140915
135900140915       ENDSR;
136000140915
136100140915       //--------------------------------------------------------------
136200140915       //?Controllo videata D02
136300140915       //--------------------------------------------------------------
136400140915       BEGSR CtrD02;
136500110310
136600110310       //?Controllo parzializzazioni INFO
136700110608       //?Affidato a BRT
136800140915         IF  V02brtda <> 0 or V02brta <> 0;
136900110608         //?Obbligatori tutti e 2 se immesso uno dei due
137000140915           IF  V02brtda <> 0  and  V02brta = 0;
137100110608             ErrMessage  = *on;
137200110608             ErrGenerico = *on;
137300110608             PosCurBrta  = *on;
137400140915             V2Dmsg      = $Msg(13);
137500110608             leavesr;
137600110608           ENDIF;
137700140915           IF  V02brtda = 0  and  V02brta <> 0;
137800110608             ErrMessage  = *on;
137900110608             ErrGenerico = *on;
138000110608             PosCurBrtda = *on;
138100140915             V2Dmsg      = $Msg(13);
138200110608             leavesr;
138300110608           ENDIF;
138400110608         //?Controllo congruenza tra "DA" e "A"
138500140915           IF  V02brtda > V02brta;
138600110608             ErrMessage  = *on;
138700110608             ErrGenerico = *on;
138800110608             PosCurBrtda = *on;
138900140915             V2Dmsg      = $Msg(14);
139000110608             leavesr;
139100110608           ENDIF;
139200110608         ENDIF;
139300110608
139400140915       //?No affido a BRT Italia Parcel
139500140915         IF  V02nrfda <> 0 or V02nrfa <> 0;
139600140915         //?Obbligatori tutti e 2 se immesso uno dei due
139700140915           IF  V02nrfda <> 0  and  V02nrfa = 0;
139800140915             ErrMessage  = *on;
139900140915             ErrGenerico = *on;
140000140915             PosCurNrfa  = *on;
140100140915             V2Dmsg      = $Msg(28);
140200140915             leavesr;
140300140915           ENDIF;
140400140915           IF  V02nrfda = 0  and  V02nrfa <> 0;
140500140915             ErrMessage  = *on;
140600140915             ErrGenerico = *on;
140700140915             PosCurNrfda = *on;
140800140915             V2Dmsg      = $Msg(28);
140900140915             leavesr;
141000140915           ENDIF;
141100140915         //?Controllo congruenza tra "DA" e "A"
141200140915           IF  V02nrfda > V02nrfa;
141300140915             ErrMessage  = *on;
141400140915             ErrGenerico = *on;
141500140915             PosCurNrfda = *on;
141600140915             V2Dmsg      = $Msg(29);
141700140915             leavesr;
141800140915           ENDIF;
141900140915         ENDIF;
142000140915
142100140915       //?No affido a BRT Italia Messaggeria
142200140915         IF  V02nroda <> 0 or V02nroa <> 0;
142300140915         //?Obbligatori tutti e 2 se immesso uno dei due
142400140915           IF  V02nroda <> 0  and  V02nroa = 0;
142500140915             ErrMessage  = *on;
142600140915             ErrGenerico = *on;
142700140915             PosCurNroa  = *on;
142800140915             V2Dmsg      = $Msg(28);
142900140915             leavesr;
143000140915           ENDIF;
143100140915           IF  V02nroda = 0  and  V02nroa <> 0;
143200140915             ErrMessage  = *on;
143300140915             ErrGenerico = *on;
143400140915             PosCurNroda = *on;
143500140915             V2Dmsg      = $Msg(28);
143600140915             leavesr;
143700140915           ENDIF;
143800140915         //?Controllo congruenza tra "DA" e "A"
143900140915           IF  V02nroda > V02nroa;
144000140915             ErrMessage  = *on;
144100140915             ErrGenerico = *on;
144200140915             PosCurNroda = *on;
144300140915             V2Dmsg      = $Msg(29);
144400140915             leavesr;
144500140915           ENDIF;
144600140915         ENDIF;
144700140915
144800140915       //?No affido a BRT Estero Via Aerea
144900140915         IF  V02nerda <> 0 or V02nera <> 0;
145000140915         //?Obbligatori tutti e 2 se immesso uno dei due
145100140915           IF  V02nerda <> 0  and  V02nera = 0;
145200140915             ErrMessage  = *on;
145300140915             ErrGenerico = *on;
145400140915             PosCurNera  = *on;
145500140915             V2Dmsg      = $Msg(28);
145600140915             leavesr;
145700140915           ENDIF;
145800140915           IF  V02nerda = 0  and  V02nera <> 0;
145900140915             ErrMessage  = *on;
146000140915             ErrGenerico = *on;
146100140915             PosCurNerda = *on;
146200140915             V2Dmsg      = $Msg(28);
146300140915             leavesr;
146400140915           ENDIF;
146500140915         //?Controllo congruenza tra "DA" e "A"
146600140915           IF  V02nerda > V02nera;
146700140915             ErrMessage  = *on;
146800140915             ErrGenerico = *on;
146900140915             PosCurNerda = *on;
147000140915             V2Dmsg      = $Msg(29);
147100140915             leavesr;
147200140915           ENDIF;
147300140915         ENDIF;
147400140915
147500140915       //?No affido a BRT Estero Serv. Camionistico
147600140915         IF  V02naeda <> 0 or V02naea <> 0;
147700140915         //?Obbligatori tutti e 2 se immesso uno dei due
147800140915           IF  V02naeda <> 0  and  V02naea = 0;
147900140915             ErrMessage  = *on;
148000140915             ErrGenerico = *on;
148100140915             PosCurNaea  = *on;
148200140915             V2Dmsg      = $Msg(28);
148300140915             leavesr;
148400140915           ENDIF;
148500140915           IF  V02naeda = 0  and  V02naea <> 0;
148600140915             ErrMessage  = *on;
148700140915             ErrGenerico = *on;
148800140915             PosCurNaeda = *on;
148900140915             V2Dmsg      = $Msg(28);
149000140915             leavesr;
149100140915           ENDIF;
149200140915         //?Controllo congruenza tra "DA" e "A"
149300140915           IF  V02naeda > V02naea;
149400140915             ErrMessage  = *on;
149500140915             ErrGenerico = *on;
149600140915             PosCurNaeda = *on;
149700140915             V2Dmsg      = $Msg(29);
149800140915             leavesr;
149900140915           ENDIF;
150000140915         ENDIF;
150100140915
150200140915       //?No affido a BRT Altro (es. Completi, ADR)
150300140915         IF  V02ntrda <> 0 or V02ntra <> 0;
150400140915         //?Obbligatori tutti e 2 se immesso uno dei due
150500140915           IF  V02ntrda <> 0  and  V02ntra = 0;
150600140915             ErrMessage  = *on;
150700140915             ErrGenerico = *on;
150800140915             PosCurNtra  = *on;
150900140915             V2Dmsg      = $Msg(28);
151000140915             leavesr;
151100140915           ENDIF;
151200140915           IF  V02ntrda = 0  and  V02ntra <> 0;
151300140915             ErrMessage  = *on;
151400140915             ErrGenerico = *on;
151500140915             PosCurNtrda = *on;
151600140915             V2Dmsg      = $Msg(28);
151700140915             leavesr;
151800140915           ENDIF;
151900140915         //?Controllo congruenza tra "DA" e "A"
152000140915           IF  V02ntrda > V02ntra;
152100140915             ErrMessage  = *on;
152200140915             ErrGenerico = *on;
152300140915             PosCurNtrda = *on;
152400140915             V2Dmsg      = $Msg(29);
152500140915             leavesr;
152600140915           ENDIF;
152700140915         ENDIF;
152800140915
152900140915         //?Paesi Estero 1
153000140915         IF  V02paesi1 <> *blanks;
153100140915           IF    %scan('?' : V02paesi1) > 0;
153200140915             ErrGenerico  = *on;
153300140915             PosCurPaesi1 = *on;
153400140915             clear TIBS02ds;
153500140915             T02mod = 'R';
153600140915             T02cod = 'IFS';
153700140915             T02ke1 = '50';
153800140915             T02sif = KNSIF;
153900140915             TNTBE_RicercaControllo  (kpjba : tibs02ds);
154000140915             IF  T02err  = *blanks;
154100140915               V02paesi1 = t02ke2;
154200140915             ENDIF;
154300140915           ENDIF;
154400140915           IF  %lookup(V02paesi1 : $IfsPAESI) = 0;
154500140915             ErrMessage   = *on;
154600140915             ErrGenerico  = *on;
154700140915             PosCurPaesi1 = *on;
154800140915             V2Dmsg = $Msg(30);
154900140915             leavesr;
155000140915           ENDIF;
155100140915         ENDIF;
155200140915         //?Paesi Estero 2
155300140915         IF  V02paesi2 <> *blanks;
155400140915           IF    %scan('?' : V02paesi2) > 0;
155500140915             ErrGenerico  = *on;
155600140915             PosCurPaesi2 = *on;
155700140915             clear TIBS02ds;
155800140915             T02mod = 'R';
155900140915             T02cod = 'IFS';
156000140915             T02ke1 = '50';
156100140915             T02sif = KNSIF;
156200140915             TNTBE_RicercaControllo  (kpjba : tibs02ds);
156300140915             IF  T02err  = *blanks;
156400140915               V02paesi2 = t02ke2;
156500140915             ENDIF;
156600140915           ENDIF;
156700140915           IF  %lookup(V02paesi2 : $IfsPAESI) = 0;
156800140915             ErrMessage   = *on;
156900140915             ErrGenerico  = *on;
157000140915             PosCurPaesi2 = *on;
157100140915             V2Dmsg = $Msg(30);
157200140915             leavesr;
157300140915           ENDIF;
157400140915         ENDIF;
157500140915         //?Paesi Estero 3
157600140915         IF  V02paesi3 <> *blanks;
157700140915           IF    %scan('?' : V02paesi3) > 0;
157800140915             ErrGenerico  = *on;
157900140915             PosCurPaesi3 = *on;
158000140915             clear TIBS02ds;
158100140915             T02mod = 'R';
158200140915             T02cod = 'IFS';
158300140915             T02ke1 = '50';
158400140915             T02sif = KNSIF;
158500140915             TNTBE_RicercaControllo  (kpjba : tibs02ds);
158600140915             IF  T02err = *blanks;
158700140915               V02paesi3 = t02ke2;
158800140915             ENDIF;
158900140915           ENDIF;
159000140915           IF  %lookup(V02paesi3 : $IfsPAESI) = 0;
159100140915             ErrMessage   = *on;
159200140915             ErrGenerico  = *on;
159300140915             PosCurPaesi3 = *on;
159400140915             V2Dmsg = $Msg(30);
159500140915             leavesr;
159600140915           ENDIF;
159700140915         ENDIF;
159800110310
159900110310       //?Peso medio a Spedizione
160000140915         IF  V02pkgda <> 0 or V02pkga <> 0;
160100110310         //?Obbligatori tutti e 2 se immesso uno dei due
160200140915           IF  V02pkgda <> 0  and  V02pkga = 0;
160300110310             ErrMessage  = *on;
160400110310             ErrGenerico = *on;
160500110310             PosCurPkga  = *on;
160600140915             V2Dmsg      = $Msg(21);
160700110310             leavesr;
160800110310           ENDIF;
160900140915           IF  V02pkgda = 0  and  V02pkga <> 0;
161000110310             ErrMessage  = *on;
161100110310             ErrGenerico = *on;
161200110310             PosCurPkgda = *on;
161300140915             V2Dmsg      = $Msg(21);
161400110310             leavesr;
161500110310           ENDIF;
161600110310         //?Controllo congruenza tra "DA" e "A"
161700140915           IF  V02pkgda > V02pkga;
161800110310             ErrMessage  = *on;
161900110310             ErrGenerico = *on;
162000110310             PosCurPkgda = *on;
162100140915             V2Dmsg      = $Msg(22);
162200110310             leavesr;
162300110310           ENDIF;
162400110310         ENDIF;
162500110310
162600110310         //?Concorrente 1
162700140915         IF  V02conc1 <> *blanks;
162800140915           IF    %scan('?' : V02conc1) > 0;
162900110310             ErrGenerico = *on;
163000110310             PosCurConc1  = *on;
163100110310             clear TIBS02ds;
163200110310             T02mod = 'R';
163300110310             T02cod = 'IFS';
163400110310             T02ke1 = '10';
163500110310             T02sif = KNSIF;
163600110310             TNTBE_RicercaControllo  (kpjba : tibs02ds);
163700110310             IF  T02err = *blanks;
163800140915               V02conc1 = t02ke2;
163900110310             ENDIF;
164000110310           ENDIF;
164100140915           IF  %lookup(V02conc1 : $Ifs) = 0;
164200110310             ErrMessage  = *on;
164300110310             ErrGenerico = *on;
164400110310             PosCurConc1 = *on;
164500140915             V2Dmsg = $Msg(23);
164600110310             leavesr;
164700110310           ENDIF;
164800110310         ENDIF;
164900110310         //?Concorrente 2
165000140915         IF  V02conc2 <> *blanks;
165100140915           IF    %scan('?' : V02conc2) > 0;
165200110310             ErrGenerico = *on;
165300110310             PosCurConc2  = *on;
165400110310             clear TIBS02ds;
165500110310             T02mod = 'R';
165600110310             T02cod = 'IFS';
165700110310             T02ke1 = '10';
165800110310             T02sif = KNSIF;
165900110310             TNTBE_RicercaControllo  (kpjba : tibs02ds);
166000110310             IF  T02err = *blanks;
166100140915               V02conc2 = t02ke2;
166200110310             ENDIF;
166300110310           ENDIF;
166400140915           IF  %lookup(V02conc2 : $Ifs) = 0;
166500110310             ErrMessage  = *on;
166600110310             ErrGenerico = *on;
166700110310             PosCurConc2 = *on;
166800140915             V2Dmsg = $Msg(23);
166900110310             leavesr;
167000110310           ENDIF;
167100110310         ENDIF;
167200110310         //?Concorrente 3
167300140915         IF  V02conc3 <> *blanks;
167400140915           IF    %scan('?' : V02conc3) > 0;
167500110310             ErrGenerico = *on;
167600110310             PosCurConc3  = *on;
167700110310             clear TIBS02ds;
167800110310             T02mod = 'R';
167900110310             T02cod = 'IFS';
168000110310             T02ke1 = '10';
168100110310             T02sif = KNSIF;
168200110310             TNTBE_RicercaControllo  (kpjba : tibs02ds);
168300110310             IF  T02err = *blanks;
168400140915               V02conc3 = t02ke2;
168500110310             ENDIF;
168600110310           ENDIF;
168700140915           IF  %lookup(V02conc3 : $Ifs) = 0;
168800110310             ErrMessage  = *on;
168900110310             ErrGenerico = *on;
169000110310             PosCurConc3 = *on;
169100140915             V2Dmsg = $Msg(23);
169200110310             leavesr;
169300110310           ENDIF;
169400110310         ENDIF;
169500140915
169600140915       //?Interessati alla Logistica
169700140915       //?Se inserite le data di immissione interessamento alla logistica inseri
169800140915         IF  V02int = ' ' and
169900140915             (V02dil1 > 0 or V02dil2 > 0);
170000140915           ErrMessage  = *on;
170100140915           ErrGenerico = *on;
170200140915           PosCurINT   = *on;
170300140915           V2Dmsg = $Msg(31);
170400140915           leavesr;
170500140915         ENDIF;
170600140915
170700140915       //?Controllo la correttezza delle date immissione interesse logistica
170800140915           IF v02dil1 > 0 ;
170900140915             clear wlbdat;
171000140915             G02dat = V02dil1;
171100140915             xsrda8(wlbdat);
171200140915             IF  G02err = '1';
171300140915               ErrMessage  = *on;
171400140915               ErrGenerico = *on;
171500140915               PosCurDil1  = *on;
171600140915               V2Dmsg      = $Msg(32);
171700140915               leavesr;
171800140915             ENDIF;
171900140915
172000140915             V02dil1  = G02dat;
172100140915             wdil1    = G02inv;
172200140915
172300140915           ENDIF;
172400140915
172500140915           IF v02dil2 > 0 ;
172600140915             clear wlbdat;
172700140915             G02dat = V02dil2;
172800140915             xsrda8(wlbdat);
172900140915             IF  G02err = '1';
173000140915               ErrMessage  = *on;
173100140915               ErrGenerico = *on;
173200140915               PosCurDil2  = *on;
173300140915               V2Dmsg      = $Msg(32);
173400140915               leavesr;
173500140915             ENDIF;
173600140915
173700140915             V02dil2  = G02dat;
173800140915             wdil2    = G02inv;
173900140915
174000140915           ENDIF;
174100140915
174200140915         //?Se impostata data "dal" e non "al"
174300140915         //?imposto "al" = data del giorno
174400140915           IF  V02dil1  > 0 and V02dil2  = 0;
174500140915             wdil2     = %dec(%date());
174600140915             data_eur  = %date(wdil2:*iso);
174700140915             V02dil2   = %dec(data_eur);
174800140915           ENDIF;
174900140915         //?"DAL" congruente con "AL"
175000140915           IF  wdil1 > 0  and  wdil2 = 0;
175100140915             ErrMessage  = *on;
175200140915             ErrGenerico = *on;
175300140915             PosCurDil1  = *on;
175400140915             V2Dmsg      = $Msg(33);
175500140915             leavesr;
175600140915           ENDIF;
175700140915           IF  wdil1  > 0  and  wdil2  > 0  and  wdil1  > wdil2 ;
175800140915             ErrMessage  = *on;
175900140915             ErrGenerico = *on;
176000140915             PosCurDil2  = *on;
176100140915             V2Dmsg      = $Msg(33);
176200140915             leavesr;
176300140915           ENDIF;
176400140915
176500140915       //?Se inserite Concorrenti ci deve essere  anche se logistica in outsourc
176600140915         IF  V02conl <> *blanks  and
176700140915              V02LOS <> 'S' ;
176800140915           ErrMessage  = *on;
176900140915           ErrGenerico = *on;
177000140915           PosCurLOS   = *on;
177100140915           V2Dmsg = $Msg(34);
177200140915           leavesr;
177300140915         ENDIF;
177400140915
177500140915         //?Concorrente 1
177600140915         IF  V02concl1 <> *blanks;
177700140915           IF    %scan('?' : V02concl1) > 0;
177800140915             ErrGenerico = *on;
177900140915             PosCurConcl1  = *on;
178000140915             clear TIBS02ds;
178100140915             T02mod = 'R';
178200140915             T02cod = 'IFS';
178300140915             T02ke1 = '40';
178400140915             T02sif = KNSIF;
178500140915             TNTBE_RicercaControllo  (kpjba : tibs02ds);
178600140915             IF  T02err = *blanks;
178700140915               V02concl1 = t02ke2;
178800140915             ENDIF;
178900140915           ENDIF;
179000140915           IF  %lookup(V02concl1 : $IfsLOG) = 0;
179100140915             ErrMessage  = *on;
179200140915             ErrGenerico = *on;
179300140915             PosCurConcl1 = *on;
179400140915             V2Dmsg = $Msg(35);
179500140915             leavesr;
179600140915           ENDIF;
179700140915         ENDIF;
179800140915         //?Concorrente 2
179900140915         IF  V02concl2 <> *blanks;
180000140915           IF    %scan('?' : V02concl2) > 0;
180100140915             ErrGenerico = *on;
180200140915             PosCurConcl2  = *on;
180300140915             clear TIBS02ds;
180400140915             T02mod = 'R';
180500140915             T02cod = 'IFS';
180600140915             T02ke1 = '40';
180700140915             T02sif = KNSIF;
180800140915             TNTBE_RicercaControllo  (kpjba : tibs02ds);
180900140915             IF  T02err = *blanks;
181000140915               V02concl2 = t02ke2;
181100140915             ENDIF;
181200140915           ENDIF;
181300140915           IF  %lookup(V02concl2 : $IfsLOG) = 0;
181400140915             ErrMessage  = *on;
181500140915             ErrGenerico = *on;
181600140915             PosCurConcl2 = *on;
181700140915             V2Dmsg = $Msg(35);
181800140915             leavesr;
181900140915           ENDIF;
182000140915         ENDIF;
182100140915         //?Concorrente 3
182200140915         IF  V02concl3 <> *blanks;
182300140915           IF    %scan('?' : V02concl3) > 0;
182400140915             ErrGenerico = *on;
182500140915             PosCurConcl3  = *on;
182600140915             clear TIBS02ds;
182700140915             T02mod = 'R';
182800140915             T02cod = 'IFS';
182900140915             T02ke1 = '40';
183000140915             T02sif = KNSIF;
183100140915             TNTBE_RicercaControllo  (kpjba : tibs02ds);
183200140915             IF  T02err = *blanks;
183300140915               V02concl3 = t02ke2;
183400140915             ENDIF;
183500140915           ENDIF;
183600140915           IF  %lookup(V02concl3 : $IfsLOG) = 0;
183700140915             ErrMessage  = *on;
183800140915             ErrGenerico = *on;
183900140915             PosCurConcl3 = *on;
184000140915             V2Dmsg = $Msg(35);
184100140915             leavesr;
184200140915           ENDIF;
184300140915         ENDIF;
184400140915
184500140915         //?Se inserite delle selezioni metto in reverse immage F05
184600140915            altreparz = *off;
184700140915         IF V02INT <> ' 'or V02LOS <> ' ' or V02Conl <> *blanks  or
184800140915            V02nrfda <> *zeros or V02nroda <> *zeros or
184900140915            V02nerda <> *zeros or V02naeda <> *zeros or
185000140915            V02ntrda <> *zeros or V02paesi <> *blanks or
185100140915            V02conc <> *blanks or V02pkgda <> *zeros or
185200140915            V02she <> ' ' or V02secm <> ' ' or V02sbx <> ' ' or
185300140915            V02brtda <> *zeros or V02brta <> *zeros;
185400140915            AltreParz = *on;
185500140915         ENDIF;
185600080206
185700080206       ENDSR;
185800110608
185900080206       //--------------------------------------------------------------
186000080206       //?Gestione videata S01
186100080206       //--------------------------------------------------------------
186200080206       BEGSR GesS01;
186300080207
186400080207         // Inizializzazione videata
186500080207         if  $InzS01 = *on;
186600080207            exsr InzS01;
186700080207            $InzS01  = *off;
186800080207         endif;
186900080207
187000080207         // Posizionamento cursore
187100080207         if  C01csr  > *zero;
187200080207           C01rcd = C01csr;
187300080207         else;
187400080207           C01rcd = 1;
187500150109           // Se non è stato caricato nulla si riemette la videata D01
187600080207           $Video = 'D1';
187700080207           leavesr;
187800080207         endif;
187900080207
188000080207         // Emissione Testata e Piede con tasti funzionali abilitati
188100110615           write  TAI1T01;
188200110615           write  TAI1P01;
188300080207
188400080207         // Emissione videata
188500110615         exfmt  TAI1C01;
188600080207         reset errMessage;
188700080207         reset errGenerico;
188800080207         clear V1Dmsg;
188900080207
189000080207         SELECT;
189100080207
189200080207         // - F3=Fine
189300080207           WHEN  dsp_aid = c_F03;
189400150109             // Chiudo il cursore
189500150109             exec sql
189600150109               close CLI;
189700080207             exsr F03D01;
189800080207
189900080207         // - F12=Ritorno
190000080207           WHEN  dsp_aid = c_F12;
190100080207             exsr F12S01;
190200080207
190300080207         // - Roll-Up
190400080207           WHEN  dsp_aid = c_RollUp;
190500080208           // - Verifica se raggiunto il limite di records registrabili
190600080208           //   nel subfile (9999)
190700080208             if  S01nrr = *hival;
190800080208               errMessage  = *on;
190900080208               errGenerico = *on;
191000080208               V1Dmsg = $Msg(10);
191100080208             else;
191200080208               exsr RollUpS01;
191300080208             endif;
191400080207
191500080207         // Invio
191600080207           OTHER;
191700080207             exsr OpzS01;
191800080207             if  errGenerico = *on;
191900080207               leavesr;
192000080207             endif;
192100080207
192200080207         ENDSL;
192300080207
192400080207       ENDSR;
192500080207
192600080207       //--------------------------------------------------------------
192700080207       //?Inizializzazione videata S01
192800080207       //--------------------------------------------------------------
192900080207       BEGSR InzS01;
193000080207
193100080207       // Pulizia subfile
193200080207         SflDsp_N    = *on;
193300080207         SflDspCtl_N = *on;
193400110615         write  TAI1C01;
193500080207         SflDspCtl_N = *off;
193600080207         SflEnd      = *off;
193700080207
193800080207         clear W_SflPag;
193900080207         clear C01rcd;
194000080207         clear C01csr;
194100080207         clear S01nrr;
194200080207         clear V1Dmsg;
194300080207         errMessage  = *off;
194400080207         errGenerico = *off;
194500080207         WindDspF    = IndDspF;
194600080207         reset WindDspFb;
194700080207         IndDspF     = WindDspF;
194800080207
194900081017
195000080207       // Caricamento dei dati da presentare nel sfl
195100080207         $EoF = *off;
195200080213         select;
195300110608         //?Richiesta selezione per codice
195400110608           when V01ksc > 0;
195500110613                k00_ACOkut = 1;
195600110613                k00_ACOkcc = DUTkci;
195700110608                k00_ACOksc = V01ksc;
195800110615                k_INDkut = 1;
195900110615                k_INDkcc = DUTkci;
196000110615                k_INDksc = V01ksc;
196100110615                k_CLPkut = 1;
196200110615                k_CLPkcc = DUTkci;
196300110615                k_CLPksc = V01ksc;
196400110608                setll  %kds(k03acocod) CNACO00F;
196500110608
196600110309
196700110309         // - per tutto il resto
196800110309           other;
196900110309             exsr PrepSql;
197000110309
197100080213         endsl;
197200080213
197300080213         exsr sr_ReadRec;
197400080207
197500080207         dow  $EoF   = *off     and
197600080207              S01nrr < C_SflPag;
197700080207           exsr RollUpS01;
197800080207         enddo;
197900080207
198000080207       // Impaginazione subfile
198100080207       // -> forza l'impaginazione sul 1° rec. del subfile
198200080207         if S01nrr > *zero;
198300080207           C01rcd  = 1;
198400080207           C01csr  = 1;
198500080207         else;
198600080207           clear C01rcd;
198700080207         endif;
198800150109
198900150109         // -?NON trovati dati?
199000150109         If  S01nrr = *zero;
199100150109
199200150109           errMessage  = *on;
199300150109           errGenerico = *on;
199400150109           V1Dmsg = $Msg(09);
199500150109
199600150109           // -?Chiusura cursore?
199700150109           if  V01ksc <= *zero;
199800150109             exec sql  close CLI;
199900150109           endif;
200000150109
200100150109         EndIf;
200200080207
200300080207       ENDSR;
200400110309
200500110309       //--------------------------------------------------------------
200600110309       //?Preparo Stringa SQL.
200700110309       //--------------------------------------------------------------
200800110309       BEGSR PrepSql;
200900110309
201000110309       //?Imposto la stringa per SQL
201100110601         IF  V01osf = 'R';
201200110614           wsql = 'With J1 as -
201300131031           (select clpkut, clpkcc, clpksc, clpclv, clpage, +
201400131031            acoflg, acoabl, acokut, acokcc, acoksc, acorag, +
201500131031            acolib, acoitc from cnclp00f left join +
201600131031            cnaco00f on clpkut=acokut and clpkcc=acokcc and +
201700131031            clpksc=acoksc), J2 as +
201800131031           (select J1.*, indkut, indkcc, indksc, indvia, +
201900131031            indcit, indcae, indprv, indcdf, indiva from J1 +
202000110614            left join cnind00f on acokut=indkut and acokcc=indkcc -
202100131031            and acoksc=indksc) select * from J2 +
202200110615            where acoflg = '' ''' ;
202300110601         ENDIF;
202400110614
202500110601         IF  V01osf = 'I';
202600131031           wSql = 'With f1 as +
202700131031                   (select clpkut, clpkcc, clpksc, clpclv, clpage, +
202800131031                   case clpclv when '' '' then ''0'' +
202900131031                   else ORD end as CLPord +
203000131031                   from cnclp00f left join +
203100131031                  (select tblkey, substr(tbluni, 31, 1) +
203200131031                   as ORD from tabel00f +
203300161128                   where TBLkut = 1 and tblcod = ''IC'') as TAB +
203400131031                   on clpclv = tblkey ), +
203500110614
203600131031                   f2 as (select acoflg, acoabl, acokut, acokcc, acoksc, +
203700131031                   acorag, acolib, acoitc, indkut, indkcc, indksc, +
203800131031                   indvia, indcit, indcae, indprv, indcdf, indiva +
203900131031                   from cnaco00f left join cnind00f on +
204000131031                   acokut=indkut and acokcc=indkcc and acoksc=indksc)  +
204100110614
204200131031                   select f1.clpkut, f1.clpkcc, f1.clpksc, f1.clpclv, +
204300131031                   f1.clpage , f2.* from f1 left join f2 on +
204400131031                   acokut=clpkut and acokcc=clpkcc and acoksc=clpksc +
204500110614                   where acoflg = '' ''' ;
204600110614
204700110601         ENDIF;
204800110613         // in base alle selezioni fatte a video parazializzo i record in
204900110613         // lettura
205000110614         // bloccati o non bloccati
205100110614         If v01blc = 'N' ;
205200110614            Wsql += ' and acoabl =  '' ''' ;
205300110614         endif ;
205400110614         If v01blc = 'S' ;
205500110614            Wsql += ' and acoabl <> '' ''' ;
205600110614         endif ;
205700110613         // ragione sociale
205800110613         If v01rag <> *blanks ;
205900110613            xx = %checkr(' ' : V01rag) ;
206000110613            WSQL += ' and substr(' + ''''+ v01rag + ''''  +
206100110613            ', 1, ' + %editc(xx:'X') +
206200110613             ') = substr(acorag, 1, ' + %editc(xx:'X') +')' ;
206300110613         Endif ;
206400110613         // in base al codice importanza cliente
206500110613         If v01cic <> *blank ;
206600110613            Wsql += ' and clpclv <> '' '' and clpclv in (' ;
206700110613            WW = 1 ;
206800110613            zz = 0 ;
206900110613            FOR ww by 1 to %elem($CIC);
207000110613             IF $CIC(ww) <> *blanks;
207100110613               IF zz > 0;
207200110613                 WSql += ', ';
207300110613               ELSE;
207400110613                 zz = 1;
207500110613               ENDIF;
207600110613               WSql += '''';
207700110613               WSql += $CIC(ww);
207800110613               WSql += '''';
207900110613             ENDIF;
208000110613           ENDFOR;
208100110613           Wsql += ')';
208200110613         ENDIF;
208300110613         // Località
208400110613         If v01loc <> *blanks ;
208500110613            xx = %checkr(' ' : V01loc) ;
208600110613            WSQL += ' and substr(' + '''' + v01loc + '''' +
208700110613            ', 1, ' + %editc(xx:'X') +
208800110613             ' ) = substr(indcit, 1, ' +
208900110613            %editc(xx:'X') + ' ) ' ;
209000110613         Endif ;
209100110613         // Provincia
209200110613         If v01Prv <> *blanks ;
209300110613            WSQL += ' and indprv = ' + '''' + V01prv + '''';
209400110613         Endif ;
209500110613         // Cap
209600110613         If v01cap <> *blanks ;
209700110613            WSQL += ' and indcae = ' + '''' + v01cap + '''' ;
209800110613         Endif ;
209900110613         // in base alla filiale di appartenenza
210000110613         If v01flt <> *zeros ;
210100110613            Wsql += ' and decimal(substr(digits(acoksc), 1, 3)) = ' +
210200110613            %editc(v01flt:'X') ;
210300110613         ENDIF;
210400110613         // area di appartenenza
210500110613         If v01car <> *blanks ;
210600110614            clear $Fil1;
210700110614            yy = 0;
210800110614            xx = 1;
210900110614            FOR xx by 1 to %elem($Car);
211000110614              IF %editc($Car(xx):'X') = V01car;
211100110614                  yy += 1;
211200110614                  $Fil1(yy) = $Fil(xx);
211300110614              ENDIF;
211400110614            ENDFOR;
211500110614            yy = 0;
211600110614            xx = 1;
211700110614            wsql += ' and decimal(substr(digits(acoksc), 1, 3)) in(';
211800110614            FOR xx by 1 to %elem($Fil1);
211900110614                IF $Fil1(xx) <> *zeros;
212000110614                   IF yy > 0;
212100110614                      wSQL += ', ';
212200110614                   ELSE;
212300110614                      yy = 1;
212400110614                   ENDIF;
212500110614                   wSQL += %editc($Fil1(xx):'X');
212600110614                ENDIF;
212700110614            ENDFOR;
212800110614            wSQL += ')';
212900110613         ENDIF;
213000110614         // Partita iva
213100110614         If v01Piv <> *blanks ;
213200131031            WSQL += ' and indiva= ' + '''' + V01piv + '''';
213300110614         Endif ;
213400110614         // Codice Fiscali
213500110614         If v01Cdf <> *blanks ;
213600131031            WSQL += ' and indcdf= ' + '''' + V01cdf + '''';
213700110614         Endif ;
213800110614         // Categoria merceologica
213900110615         If v01sct <> *blanks and v01sct <> *zeros ;
214000110614             yy = 0;
214100110614             wSQL += ' and AcoItc in (';
214200110614
214300110614             IF V01sct1 <> *blanks;
214400110614               IF yy > 0;
214500110614                 wSQL += ', ';
214600110614               ELSE;
214700110614                 yy = 1;
214800110614               ENDIF;
214900110614               wSQL += V01sct1;
215000110614             ENDIF;
215100110614             IF V01sct2 <> *blanks;
215200110614               IF yy > 0;
215300110614                 wSQL += ', ';
215400110614               ELSE;
215500110614                 yy = 1;
215600110614               ENDIF;
215700110614               wSQL += V01sct2;
215800110614             ENDIF;
215900110614             IF V01sct3 <> *blanks;
216000110614               IF yy > 0;
216100110614                 wSQL += ', ';
216200110614               ELSE;
216300110614                 yy = 1;
216400110614               ENDIF;
216500110614               wSQL += V01sct3;
216600110614             ENDIF;
216700110614
216800110614           wSQL += ')';
216900110614         ENDIF;
217000110614         // agente unificante
217100110621         If v01rgf <> *blanks and v01rgf <> *zeros ;
217200110614            clear $Age1;
217300110614            yy = 0;
217400110614            xx = 1;
217500110614            FOR xx by 1 to %elem($Ageu);
217600110614              IF %editc($AGEU(xx):'X') = V01rgf;
217700110614                  yy += 1;
217800110614                  $AGE1(yy) = $Age(xx);
217900110614              ENDIF;
218000110614            ENDFOR;
218100110614            yy = 0;
218200110614            xx = 1;
218300110614            wsql += ' and clpage in(';
218400110614            FOR xx by 1 to %elem($Age1);
218500110614                IF $Age1(xx) <> *zeros;
218600110614                   IF yy > 0;
218700110614                      wSQL += ', ';
218800110614                   ELSE;
218900110614                      yy = 1;
219000110614                   ENDIF;
219100110614                   wSQL += %editc($AGE1(xx):'X');
219200110614                ENDIF;
219300110614            ENDFOR;
219400110614            wSQL += ')';
219500110614         ENDIF;
219600110613
219700110613         IF  V01osf = 'R';
219800110613           wSQL += ' order by ACOrag';
219900110613         ENDIF;
220000110613         IF  V01osf = 'I';
220100110718           wSQL += ' order by CLPord desc, CLPksc';
220200110613         ENDIF;
220300110613
220400110309         wSQL += ' for fetch only';
220500110309
220600110309         //?Dichiarazione cursore
220700110309         exec sql
220800110309           prepare S1 from :wSQL;
220900110309         exec sql
221000110608           declare CLI cursor for S1;
221100110309
221200110309         //?Apertura del cursore
221300110309         exec sql
221400110608           open CLI;
221500110309
221600110309         IF sqlcode < 0;
221700110309           $Eof = *on;
221800110309         ENDIF;
221900110309
222000110309       ENDSR;
222100080207
222200080207       //--------------------------------------------------------------
222300080207       //?Caricamento singola pagina S01
222400080207       //--------------------------------------------------------------
222500080207       BEGSR RollUpS01;
222600080207
222700080207         S01nrr    = (W_SflPag * C_SflPag);
222800080207         W_SflPag += 1;
222900080207         SflNxtChg = *off;
223000080207
223100080207         // Ciclo di caricamento dati nel sfl / lettura rec. successivo
223200080208         DOW  $EoF   = *off                   and
223300080208              S01nrr < (W_SflPag * C_SflPag)  and
223400080208              S01nrr < *hival;
223500080207
223600080207         // - Caricamento dati nel record del subfile
223700110613              If $recok = *on ;
223800110613                 exsr  RieS01;
223900110613                 S01nrr += 1;
224000110615                 write TAI1S01;
224100110613              Endif;
224200110613
224300080207         // - Lettura record successivo nell'archivio
224400080213           exsr sr_ReadRec;
224500080207
224600080207         ENDDO;
224700080207
224800080207         // Visualizzazione del SFL (se ci sono dati)
224900080207         SflDsp_N = (S01nrr <= *zeros);
225000080207
225100080207         // Attivazione (eventuale) del SFLEND
225200080207         SflEnd   = ($EoF = *on);
225300080207
225400080207         // Posizionamento cursore al 1° rcd della pagina
225500080207         if  S01nrr > *zero;
225600080207           wPag   = S01nrr / C_SflPag;
225700080207           wRig   = S01nrr - (C_SflPag * wPag);
225800080207           C01rcd = wPag * C_SflPag;
225900080207           if  wRig > *zeros;
226000080207             C01rcd = C01rcd + 1;
226100080207           else;
226200080207             C01rcd = C01rcd - C_SflPag + 1;
226300080207           endif;
226400080207         else;
226500080207           clear  C01rcd;
226600080207         endif;
226700080207
226800080207         C01csr = C01rcd;
226900080207
227000080207       ENDSR;
227100080213
227200080213       //--------------------------------------------------------------
227300080213       //?Lettura record successivo in base all'ordinamento
227400080213       //--------------------------------------------------------------
227500080213       BEGSR sr_ReadRec;
227600080213
227700080213         $RecOK  = *off;
227800080213
227900080213         DOW  $EoF   = *off   and
228000080213              $RecOK = *off;
228100080213
228200080213           select;
228300080213
228400080213           // (se non rilevato stop record elaborabili in selezione)
228500080213             when  $EoF = *on;
228600080213
228700110613           // - per Codice Cliente
228800110613             when  $Ord = 1 ;
228900110613                reade  %kds(k03acocod) CNACO00F;
229000110613                $EoF  = (%eof(CNACO00F));
229100110615                if not $Eof ;
229200110615                   chain %kds(K03cnind) CNIND00F ;
229300110615                   chain %kds(K03cnclp) CNCLP00F ;
229400110615                Endif ;
229500110309
229600110309           // - per tutto il resto
229700110309             other;
229800110309               exec sql
229900110614               fetch next from CLI into :CLI_SQL_DS ;
230000110614
230100110309               IF sqlcod = 100 or sqlcod < 0;
230200110309                 $Eof = *on;
230300110309               ENDIF;
230400080213
230500080213           endsl;
230600080213
230700080213           // Selezione record in base alle parzializzazioni
230800080213           if  $EoF  = *off;
230900110609           // Prima verifico se Autorizzato al cliente
231000110609             exsr CtrRcdOk ;
231100110609           // se record passato al primo controllo proseguo con i successivi
231200110610             If $RecOK = *on ;
231300110609                 exsr sr_SelRec;
231400110609             Endif ;
231500080213           endif;
231600080213
231700080213         ENDDO;
231800080213
231900080213       ENDSR;
232000110609       //--------------------------------------------------------------
232100110609       //?Controllo se il rcd letto è OK.
232200110609       //--------------------------------------------------------------
232300110609       BEGSR CtrRcdOK;
232400110609
232500110609         $RecOK = *on;
232600110609
232700110609       //?Escludo se capoconto NON clienti
232800110609         IF  ACOkcc <> DUTkci;
232900110609           $RecOK = *off;
233000110609           leavesr;
233100110609         ENDIF;
233200110609
233300110609       //?Escludo se codice cartello 888
233400110609         IF  %subst(%editc(ACOksc:'X'):1:3) = '888';
233500110609           $RecOK = *off;
233600110609           leavesr;
233700110609         ENDIF;
233800110609
233900110609       //?Escludo i codici fil+0000
234000110609         IF  %subst(%editc(ACOksc:'X'):4:4) = '0000';
234100110609           $RecOK = *off;
234200110609           leavesr;
234300110609         ENDIF;
234400120202
234500120202       //?Escludo i clienti con ntw 'LOG' e 'XXX'
234600120202         clear OG143;
234700120202         w0030 = %dec(%subst(%editc(ACOksc:'X'):1:3):3:0);
234800120202         chain w0030 AZORG01L;
234900120202         IF  %found(AZORG01L);
235000120202           OG143 = ORGde3;
235100120202         ENDIF;
235200120202         IF  §OGntw = 'LOG' or §OGntw = 'XXX';
235300120202           $RecOK = *off;
235400120202           leavesr;
235500120202         ENDIF;
235600110609
235700110609       //?Escludo i clienti non in gestione
235800110609         clear TNTAA1DS;
235900110609         ITAA1ksc  = ACOksc;
236000110609         ITAA1caut = '§utecli';
236100110609         kpjbu = tntaa1ds ;
236200110609         tntaa1c (kpjba) ;
236300110609         TNTAA1DS = kpjbu ;
236400110610       //?Se cliente non in gestione
236500110610         IF  OTAA1fabi = 'N';
236600110610       //?Se richiesto controllo verifico con il commerciale
236700110610           IF  ITAI1aut <> *blanks;
236800110609               xx =  %lookup(CLPage : $Age) ;
236900110609               IF  xx = *zero;
237000110609                 $RecOK = *off;
237100110609                 leavesr;
237200110615               ENDIF ;
237300110610           ELSE;
237400110610       //?Se non richiesto escludo il cliente
237500110610             $RecOK = *off;
237600110610             leavesr;
237700110610           ENDIF;
237800110610         ENDIF;
237900110609
238000110609
238100110609       ENDSR;
238200080207
238300080207       //--------------------------------------------------------------
238400080207       //?Selezione record in base alle parzializzazioni
238500080207       //--------------------------------------------------------------
238600080207       BEGSR sr_SelRec;
238700080207
238800110621        // SE RICHIESTO SOLO IL CLIENTE PASSO OLTRE
238900110621         If v01ksc > 0 ;
239000110621            $RecOK  = *on;
239100110621            leavesr ;
239200110621         Endif ;
239300110621
239400080213         $RecOK  = *off;
239500110601
239600110601       //?Controllo trattative in corso
239700110601         $EndTtr = *off;
239800110601         IF  V01ttr <> *blanks;
239900110601           wTtr = 0;
240000110609           setll (ACOlib) TIVIS01L;
240100110601           DOW  not $EndTtr;
240200110609             reade (ACOlib) TIVIS01L;
240300110601             IF  %eof(TIVIS01L);
240400110601               $EndTtr = *on;
240500110601               leave;
240600110601             ENDIF;
240700110609           // verifico se è del cliente
240800110609             If  visksc <> acoksc ;
240900110609                 iter ;
241000110609             endif ;
241100110606             IF  VISffz = *blanks and VISdch = 0;
241200110606               wTtr += 1;
241300110606               $EndTtr = *on;
241400110606               leave;
241500110606             ENDIF;
241600110601           ENDDO;
241700110606           IF (V01ttr = 'S' and wTtr = 0) or
241800110606              (V01ttr = 'N' and wTtr > 0);
241900110601             leavesr;
242000110601           ENDIF;
242100110601         ENDIF;
242200110614
242300140916
242400140916       //?Parzializzazioni INFO commerciali
242500140916       //?Affidato a BRT
242600140916         If  V02brtda > 0 ;
242700140916             chain (acolib:'10 ':'_BAR') TICPI01L ;
242800140916             If not %found(TICPI01L) ;
242900140916                leavesr ;
243000140916             endif ;
243100140916             If %found(TICPI01L) and (cpival < v02brtda or
243200140916                cpival > v02brta) ;
243300140916                leavesr ;
243400140916             Endif ;
243500140916         Endif ;
243600140916
243700140916       //?Dettaglio NON affidato a BRT
243800140916       //?Non affidato a BRT Italia Parcel
243900140916         IF  V02nrfda > 0;
244000140916           chain (ACOlib:'NRF') TICPI01L;
244100140916           IF  not %found(TICPI01L);
244200140916             leavesr;
244300140916           ENDIF;
244400140916           IF  CPIpft < V02nrfda or CPIpft > V02nrfa;
244500140916             leavesr;
244600140916           ENDIF;
244700140916         ENDIF;
244800140916
244900140916       //?Non affidato a BRT Italia Messaggeria
245000140916         IF  V02nroda > 0;
245100140916           chain (ACOlib:'NRO') TICPI01L;
245200140916         //?Non trovo dati relativi al peso escludo il potenziale
245300140916           IF  not %found(TICPI01L);
245400140916             leavesr;
245500140916           ENDIF;
245600140916           IF  CPIpft < V02nroda or CPIpft > V02nroa;
245700140916             leavesr;
245800140916           ENDIF;
245900140916         ENDIF;
246000140916
246100140916       //?Non affidato a BRT Estero Via Aerea
246200140916         IF  V02nerda > 0;
246300140916           chain (ACOlib:'NER') TICPI01L;
246400140916         //?Non trovo dati relativi al peso escludo il potenziale
246500140916           IF  not %found(TICPI01L);
246600140916             leavesr;
246700140916           ENDIF;
246800140916           IF  CPIpft < V02nerda or CPIpft > V02nera;
246900140916             leavesr;
247000140916           ENDIF;
247100140916         ENDIF;
247200140916
247300140916       //?Non affidato a BRT Estero Serv. Camionistico
247400140916         IF  V02naeda > 0;
247500140916           chain (ACOlib:'NAE') TICPI01L;
247600140916         //?Non trovo dati relativi al peso escludo il potenziale
247700140916           IF  not %found(TICPI01L);
247800140916             leavesr;
247900140916           ENDIF;
248000140916           IF  CPIpft < V02naeda or CPIpft > V02naea;
248100140916             leavesr;
248200140916           ENDIF;
248300140916         ENDIF;
248400140916
248500140916       //?Non affidato a BRT Altro (es. Completi, ADR)
248600140916         IF  V02ntrda > 0;
248700140916           chain (ACOlib:'NTR') TICPI01L;
248800140916         //?Non trovo dati relativi al peso escludo il potenziale
248900140916           IF  not %found(TICPI01L);
249000140916             leavesr;
249100140916           ENDIF;
249200140916           IF  CPIpft < V02ntrda or CPIpft > V02ntra;
249300140916             leavesr;
249400140916           ENDIF;
249500140916         ENDIF;
249600140916
249700140916       //?Paesi Estero
249800140916         IF  V02paesi <> *blanks;
249900140916           $Paesi = *off;
250000140916           IF  V02paesi1 <> *blanks;
250100140916             chain (ACOlib:'50':V02paesi1) TICPI01L;
250200140916             IF  %found(TICPI01L) and CPIval > 0;
250300140916               $Paesi = *on;
250400140916             ENDIF;
250500140916           ENDIF;
250600140916           IF  V02paesi2 <> *blanks;
250700140916             chain (ACOlib:'50':V02paesi2) TICPI01L;
250800140916             IF  %found(TICPI01L) and CPIval > 0;
250900140916               $Paesi = *on;
251000140916             ENDIF;
251100140916           ENDIF;
251200140916           IF  V02paesi3 <> *blanks;
251300140916             chain (ACOlib:'50':V02paesi3) TICPI01L;
251400140916             IF  %found(TICPI01L) and CPIval > 0;
251500140916               $Paesi = *on;
251600140916             ENDIF;
251700140916           ENDIF;
251800140916           IF  not $Paesi;
251900140916             leavesr;
252000140916           ENDIF;
252100140916         ENDIF;
252200140916
252300140916       //?Peso medio a spedizione
252400140916         IF  V02pkgda > 0;
252500140916           chain (ACOlib:'KMS') TICPI01L;
252600140916         //?Non trovo dati relativi al peso escludo il potenziale
252700140916           IF  not %found(TICPI01L);
252800140916             leavesr;
252900140916           ENDIF;
253000140916           IF  CPIval < V02pkgda or CPIval > V02pkga;
253100140916             leavesr;
253200140916           ENDIF;
253300140916         ENDIF;
253400140916
253500140916       //?Spedizioni 10:30-PRIORITY
253600140916         IF  V02she <> *blanks;
253700140916           chain (ACOLIB:'HPR') TICPI01L;
253800140916         //?Non trovo dati relativi al tipo spedizione escludo il potenziale
253900140916           IF  not %found(TICPI01L);
254000140916             leavesr;
254100140916           ENDIF;
254200140916           IF  CPIval = 0;
254300140916             leavesr;
254400140916           ENDIF;
254500140916         ENDIF;
254600140916
254700140916       //?Spedizioni E-COMMERCE
254800140916         IF  V02secm <> *blanks;
254900140916           chain (ACOlib:'ECM') TICPI01L;
255000140916         //?Non trovo dati escludo il potenziale
255100140916           IF  not %found(TICPI01L);
255200140916             leavesr;
255300140916           ENDIF;
255400140916           IF  CPIval = 0;
255500140916             leavesr;
255600140916           ENDIF;
255700140916         ENDIF;
255800140916
255900140916       //?Spedizioni SECURE BOX
256000140916         IF  V02sbx <> *blanks;
256100140916           chain (ACOLIB:'SBX') TICPI01L;
256200140916         //?Non trovo dati relativi al tipo spedizione escludo il potenziale
256300140916           IF  not %found(TICPI01L);
256400140916             leavesr;
256500140916           ENDIF;
256600140916           IF  CPIfsn <> 'S';
256700140916             leavesr;
256800140916           ENDIF;
256900140916         ENDIF;
257000140916
257100140916       //?Concorrenti
257200140916         IF  V02conc <> *blanks;
257300140916           $Conc = *off;
257400140916           IF  V02conc1 <> *blanks;
257500140916             chain (ACOLIB:'10':V02conc1) TICPI01L;
257600140916             IF  %found(TICPI01L) and CPIval > 0;
257700140916               $Conc = *on;
257800140916             ENDIF;
257900140916           ENDIF;
258000140916           IF  V02conc2 <> *blanks;
258100140916             chain (ACOLIB:'10':V02conc2) TICPI01L;
258200140916             IF  %found(TICPI01L) and CPIval > 0;
258300140916               $Conc = *on;
258400140916             ENDIF;
258500140916           ENDIF;
258600140916           IF  V02conc3 <> *blanks;
258700140916             chain (ACOLIB:'10':V02conc3) TICPI01L;
258800140916             IF  %found(TICPI01L) and CPIval > 0;
258900140916               $Conc = *on;
259000140916             ENDIF;
259100140916           ENDIF;
259200140916           IF  not $Conc;
259300140916             leavesr;
259400140916           ENDIF;
259500140916         ENDIF;
259600140916
259700140916       //?Logistica  INTERESSATI S/N
259800140916         IF  V02int <> *blank ;
259900140916
260000140916           chain (ACOlib:'LOG') TICPI01L;
260100140916           IF  not %found(TICPI01L);
260200140916                  leavesr ;
260300140916           ENDIF ;
260400140916           IF  %found(TICPI01L);
260500140916               If CPIfsn <> V02int ;
260600140916                  leavesr ;
260700140916               ENDIF ;
260800140916       //?verifico le data di inserimento delle INFO
260900140916               IF (Wdil1 > 0 or Wdil2 > 0) and
261000140916                  (CPIdim < Wdil1 or Cpidim > Wdil2) ;
261100140916                  leavesr ;
261200140916               ENDIF ;
261300140916           ENDIF;
261400140916
261500140916         ENDIF;
261600140916
261700140916       //?Logistica  OUTSOURCING INTERESSATI S/N
261800140916         IF  V02los <> *blank ;
261900140916
262000140916           chain (ACOlib:'OUT') TICPI01L;
262100140916           IF  not %found(TICPI01L);
262200140916                  leavesr ;
262300140916           ENDIF ;
262400140916           IF  %found(TICPI01L);
262500140916               If CPIfsn <> V02los ;
262600140916                  leavesr ;
262700140916               ENDIF ;
262800140916           ENDIF;
262900140916
263000140916         ENDIF;
263100140916
263200140916       //?Concorrenti  LOGISTICA
263300140916         IF  V02conl <> *blanks;
263400140916           $Conclog = *off;
263500140916           IF  V02concl1 <> *blanks;
263600140916             chain (ACOlib:'40':V02concl1) TICPI01L;
263700140916             IF  %found(TICPI01L) ;
263800140916               $Conclog = *on;
263900140916             ENDIF;
264000140916           ENDIF;
264100140916           IF  V02concl2 <> *blanks;
264200140916             chain (ACOlib:'40':V02concl2) TICPI01L;
264300140916             IF  %found(TICPI01L) ;
264400140916               $Conclog = *on;
264500140916             ENDIF;
264600140916           ENDIF;
264700140916           IF  V02concl3 <> *blanks;
264800140916             chain (ACOlib:'40':V02concl3) TICPI01L;
264900140916             IF  %found(TICPI01L) and CPIval > 0;
265000140916               $Conclog = *on;
265100140916             ENDIF;
265200140916           ENDIF;
265300140916           IF  not $Conclog ;
265400140916             leavesr;
265500140916           ENDIF;
265600140916         ENDIF;
265700110614
265800110614           // Parzializzazione dei soli codici unificanti
265900110614           If    v01uni <> *blanks ;
266000110614
266100110614              clear TIBS10DS;
266200110614              D10tle = 'ST';
266300110614              D10paf = 'P';
266400110614              D10cod = ACOksc;
266500110614              tibs10r (TIBS10DS);
266600110614              IF  D10err = *blanks and D10cop > 0;
266700110614                  wCodUni = D10cop;
266800110614              ELSE;
266900110614                  wCodUni = ACOksc;
267000110614             ENDIF;
267100110614             If v01uni = 'S' and wcoduni <> acoksc ;
267200110614                leavesr ;
267300110614             Endif ;
267400110614             If v01uni = 'N' and wcoduni  = acoksc ;
267500110614                leavesr ;
267600110614             Endif ;
267700110614           Endif ;
267800110614
267900080207
268000080207         // => record OK!
268100080208         $RecOK  = *on;
268200080207
268300080207       ENDSR;
268400110601
268500080207       //--------------------------------------------------------------
268600080207       //?Caricamento singolo record nel SubFile S01
268700080207       //--------------------------------------------------------------
268800080208       BEGSR RieS01;
268900080207
269000110615         clear  TAI1S01;
269100080211
269200110609         s01ksc = acoksc ;
269300110609
269400080211         // Decodifica categoria merceologica
269500110615         W05_Itc = Acoitc ;
269600080211         clear  ds1L;
269700110609         if  ACOitc <> *zero;
269800080211           TBLcod = '1L';
269900110615           TBLkey = %editc(W05_itc : 'X');
270000080211           chain (TBLkut : TBLcod : TBLkey) TABEL;
270100080211           if  %found(TABEL00F)   and
270200080211               TBLflg = *blank;
270300080211             ds1L    = TBLuni;
270400080211           endif;
270500080211         endif;
270600080207
270700080214         // Parametri di visualizzazione (default)
270800080214         P01da1 = C_dspatr_Norm;
270900080214
271000080207         // Campi hidden
271100110609         H01sct  = ACOitc;
271200110609
271300110616         // unificante cerco solo se non l'ho cercato prima
271400110616         If    v01uni = *blanks ;
271500110616               clear TIBS10DS;
271600110616               D10tle = 'ST';
271700110616               D10paf = 'P';
271800110616               D10cod = ACOksc;
271900110616               tibs10r (TIBS10DS);
272000110616               IF  D10err = *blanks and D10cop > 0;
272100110616                   wCodUni = D10cop;
272200110616               ELSE;
272300110616                   wCodUni = ACOksc;
272400110616               ENDIF;
272500110616         ENDIF;
272600110609
272700110610         IF  wCodUni = s01ksc;
272800110609           S01uni = 'U';
272900110609         ENDIF;
273000110609
273100110609         S01abl = ACOabl;
273200130321
273300130321       //?Se blocco cliente "7" visualizzo "8"
273400130321         IF  S01abl = '7';
273500130321           S01abl = '8';
273600130321         ENDIF;
273700110609
273800110609       //?Cerco categoria del potenziale
273900110609         chain  ACOlib  TNCPO01L;
274000110609         IF  %found(TNCPO01L);
274100110610           s01fls = %xlate(maiu:minu:CPOfls);
274200110610           sh1fls = cpofls ;
274300110609         ENDIF;
274400080208
274500081017
274600110609         S01rag  = %subst(ACOrag : 1 : %len(S01rag));
274700081021         P01da1 = C_dspatr_HI  ;
274800110609         S01loc  = %subst(INDcit : 1 : %len(S01loc));
274900110609         S01prv  = indprv;
275000080211
275100110610         S01cap  = indcae;
275200080211         S01sctD = %subst(§1Ldes : 1 : %len(S01sctD));
275300110610         if  acoflg <> *blank;
275400080214           P01da1 = C_dspatr_BL;
275500080211         endif;
275600110309
275700110309        //?memorizzo importanza cliente
275800110609         S01cic = CLPclv;
275900110309
276000110309        //?Cerco prox attività da fare
276100110523         $EndAtt = *off;
276200110523         exec sql
276300110523         declare ATT cursor for select ATCcad from TIATC00F
276400110609         where ATCcpo = :ACOlib and ATCKSC = :ACOksc and atcdco = 0
276500110523         order by ATCdad, ATChda;
276600110523         exec sql
276700110523         open ATT;
276800110523         IF sqlcode < 0;
276900110523           $EndAtt = *on;
277000110523         ENDIF;
277100110523         DOW not $EndAtt;
277200110523           exec sql
277300110523           fetch next from ATT into :ATCcad;
277400110523           IF sqlcod = 100 or sqlcod < 0;
277500110523             $EndAtt = *on;
277600110523             leave;
277700110523           ENDIF;
277800110523           S01cad = ATCcad;
277900110523           leave;
278000110523         ENDDO;
278100110523         exec sql close ATT;
278200110309
278300110309        //?Cerco se ci sono trattative aperte
278400110309         $EndTtr = *off;
278500110601         setll (CPOcpo:0) TIVIS01L;
278600110601         DOW  not $EndTtr;
278700110601           reade (CPOcpo:0) TIVIS01L;
278800110601           IF  %eof(TIVIS01L);
278900110601             $EndTtr = *on;
279000110601             leave;
279100110601           ENDIF;
279200110609           IF  VISKSC <> S01KSC ;
279300110609               iter ;
279400110609           ENDIF ;
279500110601           IF  VISffz <> *blanks;
279600110601             iter;
279700110601           ENDIF;
279800110601           S01ttr = 'S';
279900110601           $EndTtr = *on;
280000110601         ENDDO;
280100110615
280200110615        //?Cerco ultimo appuntamento effettuato
280300110615         $EndApp = *off;
280400110615         exec sql
280500110615         declare APP cursor for select ATCdco from TIATC00F
280600110615         where ATCcpo = :ACOlib and ATCKSC = :ACOksc and ATCtat = 'A'
280700110615         and ATCest = 'S' and atccad = '91'
280800110615         order by ATCdco desc, ATChco desc;
280900110615         exec sql
281000110615         open APP;
281100110615         IF sqlcode < 0;
281200110615           $EndApp = *on;
281300110615         ENDIF;
281400110615         DOW not $EndApp;
281500110615           exec sql
281600110615           fetch next from App into :ATCdco;
281700110615           IF sqlcod = 100 or sqlcod < 0;
281800110615             $EndApp = *on;
281900110615             leave;
282000110615           ENDIF;
282100110615           data_dmy = %date(atcdco:*iso) ;
282200110615           s01uap = %dec(data_dmy) ;
282300110615           leave;
282400110615         ENDDO;
282500110615         exec sql close APP;
282600080207
282700080207       ENDSR;
282800080207
282900080207       //--------------------------------------------------------------
283000080207       //?Gestione tasto funzionale F12 da videata S01
283100080207       //?F12=Ritorno
283200080207       //--------------------------------------------------------------
283300080207       BEGSR F12S01;
283400110309
283500110309         // Chiudo il cursore
283600110309         exec sql
283700110609           close CLI;
283800141201
283900141201         If  %parms() = 2;
284000141201
284100141201           // Ritorno alla videata D01
284200141201           $Video = 'D1';
284300141201
284400141201         Else;
284500141201
284600141201           // Chiusura del programma
284700141201           $Fine = *on;
284800141201
284900141201           // Restituzione indicazione del tasto funzionale premuto
285000141201           oTAI1f12 = *on;
285100141201
285200141201         EndIf;
285300110309
285400080207       ENDSR;
285500080207
285600080207       //--------------------------------------------------------------
285700080207       //?Gestione opzioni subfile
285800080207       //--------------------------------------------------------------
285900080207       BEGSR OpzS01;
286000080207
286100110615         readc TAI1S01;
286200080207
286300110615         DOW  NOT  %eof(TNTAI1D);
286400080207
286500080207           SflNxtChg = *off;
286600080207           WindDspF  = IndDspF;
286700080207           reset WindDspFb;
286800080207           IndDspF   = WindDspF;
286900080207           C01rcd    = S01nrr;
287000110609
287100110609           PosCurOpz = *off;
287200110609
287300110609           SELECT;
287400110609
287500110609             //?- Nessuna opzione
287600110610             WHEN  s01opz  = *blank;
287700110609
287800110609             //?- 1 = Scelta
287900110610             WHEN  s01opz  = '1';
288000110609               //?Se categoria potenziale PERSO non posso andare avanti
288100110609               //?con l'attività/trattativa
288200110610               IF  SH1fls = 'P';
288300110609                 ErrMessage  = *on;
288400110609                 ErrGenerico = *on;
288500110609                 PosCurOpz   = *on;
288600110609                 V1Dmsg = $Msg(26);
288700110609               ENDIF;
288800110609               //?Se manca la categoria potenziale non posso andare avanti
288900110609               //?con l'attività/trattativa
289000110610               IF  SH1fls = ' ';
289100110609                 ErrMessage  = *on;
289200110609                 ErrGenerico = *on;
289300110609                 PosCurOpz   = *on;
289400110609                 V1Dmsg = $Msg(27);
289500110609               ENDIF;
289600110609               //?richiamato per pura selezione del codice lo passo al
289700110609               //?chiamante ed esco dal pgm
289800120201               IF  not ErrMessage;
289900120201                 IF  ITAI1ric = 'S';
290000120201                   OTAI1ksc = S01ksc;
290100120201                   $Fine = *on;
290200120201                   leavesr;
290300120201                 ENDIF;
290400120201               //?se non richiamato per pura selezione vado al dettaglio
290500110609                 exsr Call_TNTAI2;
290600110609              //?Se F03 esco dal pgm
290700110609                 IF  OTAI2f03 <> *blanks;
290800110609                   OTAI1F03 = '1';
290900110609                   $Fine = *on;
291000110609                   leavesr;
291100110609                 ENDIF;
291200110609               ENDIF;
291300110609
291400110609             //?- ? = Opzione NON valida
291500110609             OTHER;
291600110609               ErrMessage  = *on;
291700110609               ErrGenerico = *on;
291800110609               PosCurOpz   = *on;
291900110609               V1Dmsg = $Msg(11);
292000110609
292100110609           ENDSL;
292200110609
292300110609           //?Aggiornamento sfl
292400110609           SELECT;
292500110609             WHEN  ErrMessage;
292600110609               SflNxtChg = *on;
292700110610               C01csr    = C01rcd;
292800110609             WHEN  ErrGenerico;
292900110610               C01csr    = C01rcd;
293000110610               clear  s01opz;
293100110609             OTHER;
293200110610               C01csr    = C01rcd;
293300110610               clear  s01opz;
293400110609           ENDSL;
293500110609
293600110615           update TAI1S01;
293700110609
293800110609           IF  ErrMessage or ErrGenerico;
293900110609             leave;
294000110609           ENDIF;
294100110609
294200110615           readc TAI1S01;
294300110609
294400110609         ENDDO;
294500080207
294600080207       ENDSR;
294700110609
294800110609       //--------------------------------------------------------------
294900110609       //?Richiamo TNTAI2R visualizza anagrafica cliente.
295000110609       //--------------------------------------------------------------
295100110609       BEGSR Call_TNTAI2;
295200110609
295300110609         clear TNTAI2DS;
295400110609         ITAI2ric  = ITAI1ric;
295500110610         ITAI2ksc  = s01KSC;
295600110609         ITAI2com  = ITAI1com;
295700110609         ITAI2comd = ITAI1comd;
295800110609
295900110609         tntai2r (kpjba : TNTAI2DS);
296000110609
296100110609       ENDSR;
296200110609
296300080206       //--------------------------------------------------------------
296400080206       //?Operazioni finali.
296500080206       //--------------------------------------------------------------
296600080206       BEGSR RoutEnd;
296700090702
296800090702         //
296900090702         clear tntaa1ds;
297000090702         itaa1TLA='C' ;
297100090702         clear kpjbu   ;
297200090702         kpjbu=tntaa1ds ;
297300090703         TNTAA1C     (kpjba);
297400080206
297500080206         *inLR = *on;
297600080206         return;
297700080206
297800080206       ENDSR;
297900080206
298000080206      /end-free
298100080206
298200080206       //--------------------------------------------------------------
298300080206       //?Schiere a tempo di compilazione.
298400080206       //--------------------------------------------------------------
298500080206
298600080206** - $MSG -------------------------------------------------------------------*
298700110308Utente non abilitato alla Funzione.                                           01
298800110518Inserire o Cod.Potenziale o Cliente Unificante no tutti e 2 assieme.          02
298900110308Se parzializzazione per cap o località  immettere la provincia                03
299000110308Se parzializzazione per Info Commerciale immettere almeno 1 dato anagrafico.  04
299100110308Categoria merceologica errata.                                                05
299200110308Codice area errato                                                            06
299300110308Ordinamento errato                                                            07
299400110608Cliente inesistente                                                           08
299500110608Non è stato trovato nessun cliente                                            09
299600110308Raggiunta l'ampiezza massima del subfile                                      10
299700110308Opzione errata                                                                11
299800110601Se richiesto 'Trattative in corso' immettere almeno 1 dato anagrafico.        12
299900110608Immettere la percentuale traffico affidato a BRT                              13
300000110608Traffico Affidato a BRT "DA" incongruente con Trafiico Affidato a BRT "A"     14
300100110608Utente non abilitato al cliente                                               15
300200110308Filiale di appartenenza errata.                                               16
300300110309Importanza cliente errata.                                                    17
300400110309Ordinamento non valido se immessa ragione sociale.                            18
300500110310Immettere la Spesa in trasporti ESTERO                                        19
300600110310Spesa Trasporti Estero "DA" incongruente con Spesa Trasporti Estero "A"       20
300700110310Immettere Peso medio a Spedizione                                             21
300800110310Peso Medio Spedizione "DA" incongruente con Peso Medio Spedizione "A"         22
300900110310Concorrente errato                                                            23
301000110609Codice commerciale errato o non in gestione all'utente                        24
301100110609Richiedere solo commerciali UNIFICANTI!                                       25
301200110609Scelta non possibile potenziale PERSO                                         26
301300110609Scelta non possibile, manca il codice potenziale in anagrafica. Inserirlo.    27
301400140915Immettere Valore                                                              28
301500140915Valore "DA" incongruente con Valore "A"                                       29
301600140915Paese estero errato                                                           30
301700140915Se non richiesti CLIENTI interessati a LOGISTICA non immettere le date        31
301800140915Data errata                                                                   32
301900140915Data "DAL" incongruente con data "AL"                                         33
302000140915Immettere i Concorrenti SOLO se selezionata Logistica in outsourcing          34
302100140915Concorrente Logistica errato                                                  35
