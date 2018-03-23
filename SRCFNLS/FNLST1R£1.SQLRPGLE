000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300130315      //?FNLST1R - Gestione estensione traini
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200080208      // - Anagrafica clienti potenziali
001300130220     fFNfgt00F  uf a e             disk    rename(fnfgt000:Fnfgtfis)
001400130220     fFNfgt01L  if   e           k disk
001500130220     F                                     INFDS(FGTD1)
001600130218     ftntlt01L  if   e           k disk
001700130218     ftntlr01L  if   e           k disk
001800130218     ftitll01L  if   e           k disk
001900130502     ftitll02L  if   e           k disk    rename(titll000:titll002)
002000130412     ffnfv401l  if   e           k disk
002100130412     ffnfgv03l  if   e           k disk
002200130422      // - anagrafiche trazionisti/conducenti/targhe
002300130218     FTNTLZ01L  IF   E           K DISK
002400130422     Faiats01l  IF   E           K DISK
002500130422     Faiata01l  IF   E           K DISK
002600130422     Faiatm01l  IF   E           K DISK
002700080208      // - Organigramma filiale/aziendale
002800080206     fAZORG01L  if   e           k disk
002900130123     fAZCLN01L  if   e           k disk
003000080208      // - Tabelle
003100080206     fTABEL00F  if   e           k disk
003200080604     fTNTBE01l  if   e           k disk
003300080604
003400080208      // - Video Interrogazione anagrafica clienti potenziali
003500130218     ffnlst1D   cf   e             workstn
003600080208     f                                     indds(IndDspF)
003700080206     f                                     infds(InfDspF)
003800130218     f                                     sfile(lst1S02 : S02nrr)
003900080206
004000080206      //---------------------------------------------------------------
004100080206      //?Definizione costanti.
004200080206      //---------------------------------------------------------------
004300080411      // - Numero di record in ciascuna videata di subfile
004400130122     d c_SflPag        c                   const(16)
004500080411     d W_SflPag        s              3  0 inz
004600080530     d totrig          s              5  0 inz
004700130122     d ricInt          s              1
004800080207
004900080207      // - Tasti funzionali a video
005000080207     d c_F01           c                   const(x'31')
005100080207     d c_F02           c                   const(x'32')
005200080207     d c_F03           c                   const(x'33')
005300080207     d c_F05           c                   const(x'35')
005400080207     d c_F06           c                   const(x'36')
005500080207     d c_F07           c                   const(x'37')
005600080207     d c_F08           c                   const(x'38')
005700080207     d c_F09           c                   const(x'39')
005800080207     d c_F10           c                   const(x'3A')
005900080207     d c_F12           c                   const(x'3C')
006000080207     d c_F13           c                   const(x'B1')
006100080207     d c_F14           c                   const(x'B2')
006200080207     d c_F15           c                   const(x'B3')
006300080207     d c_F16           c                   const(x'B4')
006400080207     d c_F17           c                   const(x'B5')
006500080207     d c_F18           c                   const(x'B6')
006600080207     d c_F19           c                   const(x'B7')
006700080207     d c_F20           c                   const(x'B8')
006800080207     d c_F21           c                   const(x'B9')
006900080207     d c_F22           c                   const(x'BA')
007000080207     d c_F23           c                   const(x'BB')
007100080207     d c_F24           c                   const(x'BC')
007200080207     d c_Enter         c                   const(x'F1')
007300080207     d c_RollDown      c                   const(x'F4')
007400080207     d c_RollUp        c                   const(x'F5')
007500080214
007600080214      // - Attributi di visualizzazione
007700080215      //  -  DspAtr() - Normale
007800080214     d C_dspatr_Norm   c                   const(x'20')
007900080215      //  -  DspAtr(RI)
008000080214     d C_dspatr_RI     c                   const(x'21')
008100080215      //  -  DspAtr(ND)
008200080214     d C_dspatr_ND     c                   const(x'27')
008300080215      //  -  DspAtr(BL) / Color(Red)
008400080214     d C_dspatr_BL     c                   const(x'28')
008500080206
008600080206      //---------------------------------------------------------------
008700080206      //?Definizione schiere.
008800080206      //---------------------------------------------------------------
008900080206
009000080206      // - Messaggi di errore
009100130503     d MSG             s             78    dim(19) ctdata perrcd(1)
009200080609
009300080206      //---------------------------------------------------------------
009400080206      //?Definizione aree dati.
009500080206      //---------------------------------------------------------------
009600080206
009700080206      // - Dati utente
009800080206     d §AzUte        e ds                  extname(AZUTE00F)
009900080206     d                                     dtaara
010000080206     d §DatiUte      e ds                  extname(dDatiUte)
010100080206     d                                     dtaara
010200080206
010300080206      //---------------------------------------------------------------
010400080206      //?Definizione strutture dati.
010500080206      //---------------------------------------------------------------
010600080206
010700080206      // - Status
010800080206     d Psds           sds
010900080206     d   SDSpgm          *proc
011000080206
011100080206      // - InfDS
011200080206     d InfDspF         ds
011300080207     d  dsp_aid              369    369a
011400080207     d  sfl_rrn              376    377i 0
011500080207     d  min_nrr              378    379i 0
011600080207     d  num_rcds             380    381i 0
011700080206
011800080206      // - Indicatori su DspF
011900080208     d IndDspF         ds
012000080206        // - Indicatori di gestione del subfile
012100130220     d  NoTRN                         1n   overlay(IndDspF : 02)
012200130220     d  ProtD05                       1n   overlay(IndDspF : 03)
012300130130     d  InterD05                      1n   overlay(IndDspF : 04)
012400130313     d  CopiaD05                      1n   overlay(IndDspF : 05)
012500130220     d  spostad05                     1n   overlay(IndDspF : 06)
012600130220     d  ManutD05                      1n   overlay(IndDspF : 07)
012700130220     d  Imm_d05                       1n   overlay(IndDspF : 08)
012800130221     d  Sott_fgt                      1n   overlay(IndDspF : 09)
012900130314     d  Interrogaz                    1n   overlay(IndDspF : 10)
013000130314     d  Ricerca_PRP                   1n   overlay(IndDspF : 11)
013100130412     d  S02aper_REV                   1n   overlay(IndDspF : 12)
013200130417     d  S02sce_prot                   1n   overlay(IndDspF : 13)
013300130418     d  CTL02_prot                    1n   overlay(IndDspF : 14)
013400130314     d
013500080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
013600080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
013700080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
013800080206     d  SflEnd                        1n   overlay(IndDspF : 33)
013900130218     d  SflEnd3                       1n   overlay(IndDspF : 34)
014000080206        // - Indicatori di errore
014100080206     d  errMessage                    1n   overlay(IndDspF : 28)
014200130313     d  Errgg1                        1n   overlay(IndDspF : 41)
014300130313     d  Errgg2                        1n   overlay(IndDspF : 42)
014400130313     d  Errgg3                        1n   overlay(IndDspF : 43)
014500130313     d  Errgg4                        1n   overlay(IndDspF : 44)
014600130313     d  Errgg5                        1n   overlay(IndDspF : 45)
014700130313     d  Errgg6                        1n   overlay(IndDspF : 46)
014800130313     d  Errgg7                        1n   overlay(IndDspF : 47)
014900130313     d  ErrScelta                     1n   overlay(IndDspF : 48)
015000130315     d  ErrDTN                        1n   overlay(IndDspF : 50)
015100130412     d  ErrTRN                        1n   overlay(IndDspF : 51)
015200130419     d  ErrTRN_imm                    1n   overlay(IndDspF : 52)
015300130422     d  ErrTRN_NW                     1n   overlay(IndDspF : 53)
015400130422     d  Errpdr                        1n   overlay(IndDspF : 54)
015500130422     d  Errdcn                        1n   overlay(IndDspF : 55)
015600130422     d  Errtrm                        1n   overlay(IndDspF : 56)
015700080606     d  errGenerico                   1n   overlay(IndDspF : 99)
015800130220
015900130220     D FGTD1           DS
016000130220     D  FGTNRR               397    400B 0
016100080206
016200080206      // - Parametri ricevuti
016300080206     d KPJBA         e ds
016400080206
016500080206      // - Reperimento dati utente
016600080206     d TIBS34ds      e ds
016700080206     d dUte01        e ds
016800080206
016900080206      // - Ricerca/Controllo tabelle
017000130313     d tntl00ds      e ds                  inz
017100130313     d TIBS02ds      e ds                  inz
017200130123     d trul33ds      e ds                  inz
017300130422     d ds3I          e ds                  inz
017400130430     d dvpodecofi    e ds                  inz
017500080206
017600080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
017700080206     d dLAT          e ds                  inz
017800130122     D TRUL31DS      E DS
017900130122     d POG                            3  0 DIM(250) overlay(o31pog)
018000130122     d
018100130122     d dstu          e ds                  inz
018200130314     d
018300130121     D WLBDAT          DS
018400130121     D  G02DAT                 1      8  0
018500130121     D  G02INV                 9     16  0
018600130121     D  G02ERR                17     17
018700130121     D  G02TGI                18     22  0
018800130313     d
018900130123     D                 DS
019000130123     D  kaaa                   1      4  0
019100130123     D  kmmm                   5      6  0
019200130123     D  kgg                    7      8  0
019300130123     D  kDATA                  1      8  0
019400130123     d
019500130123     D clnmat          DS
019600130123     D  MAT                    1     31    dim(31)
019700130123     D clnpom          DS
019800130123     D  pom                    1     31    dim(31)
019900130123     D                 DS
020000130314     d
020100130314     D FNLST1DS        DS
020200130314     d* ' ' --> gestione proposte
020300130315     d* 'R' --> ricerca  proposte
020400130315     d* 'A' --> ricerca proposte per apertura FGV
020500130314     d* 'I' --> Interrogazione proposte
020600130314     D  Ilst1GES               1      1
020700130314     D  Ilst1DAT               2      9
020800130314     d
020900130314     D* 'G' --> caricare solo le proposte traini del giorno della settimana
021000130314     D*         corrispondente alla data;
021100130314     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
021200130314     D  Ilst1TUT              10     10
021300130314     D  Ilst1fgs              11     13
021400130417     D  Ilst1aut              14     14
021500130315     d
021600130315     d* Campi di output
021700130315     d*  S=apertura dei traini non ancora aperti
021800130315     d*  1=opzione di scelta '1' da cercare in tutte le proposte a video
021900130315     d  Olst1OPZIO            21     21
022000130315     d* per opzione '1'
022100130315     d  Olst1TRN              22     28
022200130315     d* Num relativo record della proposta FNFGT
022300130315     d*  se = *blanks  non c'e'  proposta
022400130315     d  Olst1NRRP             21     33
022500130417     d* E=errore nessun trino caricato
022600130417     d  Olst1ERR              34     34
022700130315     d
022800130315     d
022900130318     D fnlst4ds        DS
023000130318     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
023100130318     D*          B - DA BORDERO'
023200130318     D*          A - apertura foglio
023300130318     D*          C - DA CHIUSURA FV SINGOLA
023400130318     D*          I - DA CHIUSURA DA IMP
023500130318     D*          N - DA INTERROGAZIONE  FV
023600130318     D*          A - apertura fogli viaggio
023700130319     D  ilst4DFV               3     10  0
023800130318     D  ilst4nfv              11     15
023900130318     D  IlsT4GES              16     16
024000130318     D* Se PARMSG pieno --> c'è stato ERRORE
024100130318     D  olst4msg              17     94
024200130429     D  iolsT4FOR             95     97
024300130429     D  IlsT4OLD              98     98
024400130318     D* PARF12 = 'S'    --> premuto F12
024500130318     D  olst4F12             102    102
024600130318     D  ilst4fgs             103    105
024700130318     D  ilst4TRN             106    112
024800130318     D  ilst4NRRP            113    125
024900130417     D  ilst4PDR             126    132
025000130422
025100130422     d* ricerca autisti di un trazionista
025200130422     D tntl83ds        DS
025300130422     d  i83soc                        3s 0
025400130422     d  i83ksc                        7s 0
025500130422     d* i83mod = 1 --> modalità scelta conducente
025600130423     d  i83mod                        1
025700130423     d* data di riferimento alla quale caricare per ora non usata
025800130423     d  i83Drf                        8
025900130422     d  o83_AUT                      35
026000130424     d  o83_TEL                      12
026100130423     d
026200130423     d* ricerca TARGHE  di un trazionista
026300130423     D tntl84ds        DS
026400130423     d  i84soc                        3s 0
026500130423     d  i84ksc                        7s 0
026600130423     d* i84mod = 1 --> modalità scelta conducente
026700130423     d  i84mod                        1
026800130423     d* data di riferimento alla quale caricare per ora non usata
026900130423     d  i84Drf                        8
027000130423     d  o84_TRM                      10
027100080206      //---------------------------------------------------------------
027200080206      //?Definizione variabili globali.
027300080206      //---------------------------------------------------------------
027400080206
027500080206      // - Flags booleani
027600111115     d $Contattato1    s               n   inz(*off)
027700111115     d $Contattato2    s               n   inz(*off)
027800111115     d $EndAtt         s               n   inz(*off)
027900080208     d $Fine           s               n   inz(*off)
028000080208     d $InzD01         s               n   inz(*on)
028100080208     d $InzS01         s               n   inz(*on)
028200080414     d $InzS02         s               n   inz(*on)
028300080603     d $InzS03         s               n   inz(*on)
028400130129     d $InzD05         s               n   inz(*on)
028500080206     d $ErrGrave       s               n   inz(*off)
028600080207     d $EoF            s               n   inz(*off)
028700080208     d $RecOK          s               n   inz(*off)
028800080206
028900080207      // - Campi associati al video
029000130130     d Savvideo        s              2    inz
029100130218     d $Video          s              2    inz('S2')
029200130123     d S03nrr          s              4  0 inz
029300080414     d S02nrr          s              4  0 inz
029400080414     d wPag            s              4  0 inz
029500080414     d wRig            s              3  0 inz
029600080414
029700080604     d wAbi            s                   like(UTEaut)  inz
029800130121     d Indx            s              3  0 inz
029900080619     d Indx2           s              3  0 inz
030000080609     d xx              s              3  0 inz
030100080929     d yy              s              3  0 inz
030200080415     d ss              s              3  0 inz
030300080415     d ff              s              3  0 inz
030400130123     d Ktfp            s              3  0 inz
030500130123     d Ktfa            s              3  0 inz
030600130123     d Wfest           s              1
030700130315     d Almenouno       s              1
030800130315     d Wscelta         s              1
030900130417     d Wglobale        s              1
031000130418     d Unascelta       s              1
031100130430     d alfaFGS         s              3
031200130128     d x§tcod          s                   like(tblcod)
031300130128     d x§tkey          s                   like(tblkey)
031400130128     d x§tdes          s             30
031500130122     d
031600130314     d GIOggmmaa       s              6
031700130314     d GIOset          s              1
031800130315     d W02gioset       s              1
031900130314     d Datcor          s              8  0
032000130220     d w02DTN          s              8  0
032100130315     d savDTN          s              8  0
032200130412     d savTRN          s              7  0
032300130424     d sql_FGTTRN      s              7  0
032400130424     d Ktrn2           s              7  0
032500130422     d alfaTRN         s              7
032600130218     d carTRN          s              1
032700130424     d sql_FGTGSE      s              7
032800130412     d Wok             s              1
032900130412     d Carsfl          s              1
033000130412     d Savaper         s              1
033100130424     d Wforzgg1        s              1
033200130424     d Wforzgg2        s              1
033300130424     d Wforzgg3        s              1
033400130424     d Wforzgg4        s              1
033500130424     d Wforzgg5        s              1
033600130424     d Wforzgg6        s              1
033700130424     d Wforzgg7        s              1
033800130424     d Wfor2gg1        s              1
033900130424     d Wfor2gg2        s              1
034000130424     d Wfor2gg3        s              1
034100130424     d Wfor2gg4        s              1
034200130424     d Wfor2gg5        s              1
034300130424     d Wfor2gg6        s              1
034400130424     d Wfor2gg7        s              1
034500130426     d WforMgg1        s              1
034600130426     d WforMgg2        s              1
034700130426     d WforMgg3        s              1
034800130426     d WforMgg4        s              1
034900130426     d WforMgg5        s              1
035000130426     d WforMgg6        s              1
035100130426     d WforMgg7        s              1
035200130426     d Wtipo           s              1
035300130426     d Wmsg            s                   like(v1cmsg)
035400130422     d AlfaSOC         s              3
035500130422     d SocTraini       s              3  0
035600130422     d Kpdr            s              7  0
035700130502     D savnrrp5        S                   LIKE(h02nrrp)
035800130502     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
035900130424     d wrkgetlista     s           4096    varying
036000130218     d
036100080605     d Dataiso         s               d   datfmt(*iso)
036200080605     d Dataymd         s               d   datfmt(*ymd)
036300080605     d Datadmy         s               d   datfmt(*dmy)
036400130122     d Dataeur         s               d   datfmt(*eur)
036500081010     d DataSYS         s               d   inz(*sys)
036600130123     d Orasys          s               t   timfmt(*hms) inz(*sys)
036700081112     D
036800080206      //---------------------------------------------------------------
036900080206      //?Definizione procedure usate.
037000080206      //---------------------------------------------------------------
037100080414      /copy gaitrasrc/srcprotopr,tibs02r
037200080414      /copy gaitrasrc/srcprotopr,tibs34r
037300130122      /copy gaitrasrc/srcprotopr,trul31r
037400130123      /copy gaitrasrc/srcprotopr,trul33r
037500130122      /copy gaitrasrc/srcprotopr,x§taber
037600130122      /copy gaitrasrc/srcprotopr,xsrda8
037700130314      /copy gaitrasrc/srcprotopr,xgiose1
037800130313      /copy gaitrasrc/srcprotopr,tntl17r
037900130318      /copy gaitrasrc/srcprotopr,fnlst4r
038000130422      /copy gaitrasrc/srcprotopr,tntl83r
038100130423      /copy gaitrasrc/srcprotopr,tntl84r
038200130122
038300080206      //---------------------------------------------------------------
038400080206      //?Riepilogo indicatori.
038500080206      //---------------------------------------------------------------
038600080207      // - Comuni
038700080207      // 28    : Emissione messaggio di errore a video
038800080207      // - C01/S01
038900080207      // 30    : Condiziona SFLDSP    (*not)
039000080207      // 31    : Condiziona SFLDSPCTL (*not)
039100080207      // 30+31 : Condiziona SFLCLR
039200080207      // 32    : Condiziona SFLNXTCHG in S01
039300080207      // 50    : Errore: Opzione errata
039400080207      // - D01
039500080207      // - Comuni
039600080207      // 99    : Generico di Errore
039700080206      //---------------------------------------------------------------
039800080206
039900080206      //---------------------------------------------------------------
040000080206      //?M A I N - L I N E
040100080206      //---------------------------------------------------------------
040200080206
040300080206     c     *Entry        plist
040400080206     c                   parm                    KPJBA
040500130314     c                   movel     kpjbu         fnlst1ds
040600080206
040700080206      /free
040800080206
040900080206       // Operazioni iniziali
041000080206       exsr RoutInz;
041100080206
041200080206       // Gestione video
041300080206       DOW $Fine = *off;
041400080206         select;
041500080530
041600080530         // SFL di scelta
041700080411           when $Video = 'S2';
041800080411             exsr GesS02;
041900130122
042000130417         // Sfl nascosto per scelta totale dei non apertu
042100130417           when $Video = 'A2';
042200130417            D02apfgv='S'   ;
042300130417            exsr InzS02;
042400130417
042500130417          // Posizionamento cursore
042600130417          if  Carsfl  <>'N'  ;
042700130417              $Video='CA' ;
042800130417           else  ;
042900130417              $Fine = *on;
043000130417              olst1err='E'   ;
043100130417              kpjbu=fnlst1ds  ;
043200130417              leave    ;
043300130417          endif  ;
043400130417
043500130417           // Elaborazione dei record scelti per apertura fogli
043600130417           when $Video = 'CA';
043700130417           exsr    ElaboraSFL   ;
043800130417
043900130319             if ilst1ges='A' and v1cmsg<>*blanks  ;
044000130319               $Video='S2'  ;
044100130319               else  ;
044200130319               $Fine = *on;
044300130319             endif   ;
044400130315
044500080206           other;
044600080206             $Fine = *on;
044700080206         endsl;
044800080206       ENDDO;
044900080206
045000080206       // Operazioni finali
045100130315         exsr RoutEnd;
045200080206
045300130122       //--------------------------------------------------------------
045400130122       //?Reperimento Dati del job (Utente/Operativi).
045500130122       //--------------------------------------------------------------
045600130122       BEGSR DatiJob;
045700130122
045800130122         in(E) §AzUte;
045900130122         if NOT %error;
046000130122           in(E) §DatiUte;
046100130122         endif;
046200130122         if %error or RSut = *blanks;
046300130122           clear TIBS34ds;
046400130122           tibs34r(tibs34ds);
046500130122           in §AzUte;
046600130122           in §DatiUte;
046700130122         endif;
046800130122
046900130122       ENDSR;
047000130122
047100080206       //--------------------------------------------------------------
047200080206       //?Operazioni iniziali.
047300080206       //--------------------------------------------------------------
047400130417       BEGSR RoutInz;
047500080206
047600130314       clear  Ricerca_PRP  ;
047700130502       $Fine =*off  ;
047800130314
047900130315 0     if olst1OPZIO= ' '  ;
048000130315
048100080206         // Impostazione campi "fissi"
048200080208         TBLkut = 1;
048300080208         TBLcod = '1L';
048400080206
048500080206         // Reperimento dati job
048600080206         exsr DatiJob;
048700080206
048800080206         // Verifica errori e autorità profilo
048900080206         clear  wAbi;
049000080206         clear  dLAT;
049100080206         select;
049200080206         // - Se ho errori nei dati utente esco dal pgm
049300080208           when  DUTerr = 'E';
049400080206             $Fine = *on;
049500080206         // - Se non c'è l'abilitazione:
049600080206         //   - se 1° livello, abilitazioni al terminal
049700080206         //   - se 2° livello, abilitazioni al punto operativo
049800080206         //   - se sede è impossibile ma se capita mando a fine il pgm
049900080208           when  UTEaut = *blank;
050000080206             select;
050100080208               when  DUTlpo = '1';
050200080206                 wAbi  = 'TP';
050300080208               when  DUTlpo = '2';
050400080206                 wAbi  = 'PO';
050500080208               when  DUTlpo = 'S';
050600080206                 $Fine = *on;
050700080206             endsl;
050800080206         // - Altrimenti si caricano le abilitazioni del profilo
050900080206           other;
051000080208             dUTE01 = UTEfaf;
051100080208             if  §UTEpot <> *blank;
051200080208               wAbi = §UTEpot;
051300080206             else;
051400080208               wAbi = UTEaut;
051500080206             endif;
051600080206         ENDSL;
051700080206
051800080206         // Controllo se ok l'abilitazione dell'utente
051900080206         reset TIBS02ds;
052000111115         T02Mod = 'C';
052100080208         T02sif = knsif;
052200080208         T02cod = 'LAT';
052300080208         T02ke1 = wAbi;
052400080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
052500080206
052600080208         if  T02err  = *blank;
052700080208           dLAT = T02uni;
052800080206         endif;
052900080206
053000080206         // Errore o utente non abilitato
053100080208         if  T02err <> *blanks   or
053200080208             §LATabi = 'S';
053300080206           $ErrGrave   = *on;
053400080206           errMessage  = *on;
053500080206           errGenerico = *on;
053600130220           V1cmsg = Msg(02);
053700080206           leavesr;
053800080206         endif;
053900080617
054000080617         clear trul31ds  ;
054100080617         I31abi = wabi    ;
054200080617         I31cdi = DUTdis  ;
054300080617         I31car = DUTare  ;
054400080617         I31cpo = DUTpou  ;
054500080617         TRUL31R   (KPJBA:trul31ds)   ;
054600080617
054700080617         if o31pog=*zeros   ;
054800080617           $ErrGrave   = *on;
054900080617           errMessage  = *on;
055000080617           errGenerico = *on;
055100130315          V1cmsg = Msg(07);
055200080617           leavesr;
055300080617         endif         ;
055400130218
055500130430        // Carico filiali di test abilitate
055600130430         reset TIBS02ds;
055700130430         T02Mod = 'C';
055800130430         T02sif = knsif;
055900130430         T02cod = 'VPO';
056000130430         T02ke1 ='DECOFI'   ;
056100130430         TNTBE_RicercaControllo  (kpjba : tibs02ds);
056200130430         if  T02err  = *blank;
056300130430           dvpodecofi = T02uni;
056400130430         endif;
056500130430
056600130218       $inzs02=*on;
056700130218
056800130218       // Imposto come terminal in gestione simfel
056900130314       if ilst1fgs<=*zeros  ;
057000130314         v01fgs=simfel  ;
057100130314       else  ;
057200130314         v01fgs=%int(ilst1fgs)  ;
057300130314       endif ;
057400130430
057500130430       if  §vpofgvf1<>'999'  ;
057600130430           eval alfafgs=%editc(v01fgs:'X')  ;
057700130430       if  alfafgs<>§vpofgvf1 and alfafgs<>§vpofgvf2 and
057800130430           alfafgs<>§vpofgvf3 and alfafgs<>§vpofgvf4 and
057900130430           alfafgs<>§vpofgvf5 ;
058000130430           $ErrGrave   = *on;
058100130430           errMessage  = *on;
058200130430           errGenerico = *on;
058300130430          V1cmsg = Msg(17);
058400130430           leavesr;
058500130430         endif         ;
058600130430         endif         ;
058700130314
058800130430       chain   (v01fgs)  azorg01l   ;
058900130218       if %found(azorg01l)   ;
059000130218        v01dfgs=orgdes  ;
059100130218       endif  ;
059200130218
059300130218       // Propongo data del giorno con selezione traini
059400130218       dataeur=datasys  ;
059500130218       d02dtn=%dec(dataeur)  ;
059600130218       w02dtn=%dec(datasys)  ;
059700130220       Datcor=%dec(datasys)  ;
059800130412       clear d02trn  ;
059900130412       clear savtrn  ;
060000130412       clear savaper ;
060100100520
060200130417       // Tipo gestione
060300130417
060400130314 1     if ilst1ges='I'     ;
060500130314       Interrogaz =*on  ;
060600130314       V01dric='INTERROGAZIONE'  ;
060700130314       endif  ;
060800130314
060900130315 1     if ilst1ges='R'   or ilst1ges='A' ;
061000130315 1     if ilst1ges='R'   ;
061100130315        V01dric=' S C E L T A  '  ;
061200130315       endif  ;
061300130315 1     if ilst1ges='A'   ;
061400130315        V01dric='APERTURA FOGLI'  ;
061500130315       endif  ;
061600130315       Ricerca_prp=*on  ;
061700130315       Interrogaz =*on  ;
061800130314
061900130314 2       if ilst1dat>*zeros  ;
062000130314          dataiso=%date(%int(ilst1dat)) ;
062100130314         else  ;
062200130314          dataiso=%date(datcor)         ;
062300130314 2       endif  ;
062400130314          dataeur=dataiso  ;
062500130314          datadmy=dataiso  ;
062600130314          d02dtn=%dec(dataeur) ;
062700130314          w02dtn=%dec(dataiso)  ;
062800130314          GIOggmmaa=%editc(%dec(datadmy) :'X') ;
062900130314
063000130315          exsr  CercaGIORNO  ;
063100130315
063200130314 1      endif  ;
063300130417
063400130417        // Se proposta automatica senza far vedere videata,
063500130417        //  carico SFL ed elaboro in automatico
063600130417        if ilst1ges='A' and ilst1aut='S'   ;
063700130417        $Video='A2'  ;
063800130417        endif   ;
063900130422
064000130422        // Carico società trazinisti
064100130422        tblkey='1       ';
064200130422        clear alfasoc ;
064300130422        clear soctraini  ;
064400130422        chain  (1:'3I':tblkey ) tabel00f ;
064500130422        if %found(tabel00f)  ;
064600130422        ds3i=tbluni  ;
064700130422        alfasoc=%subst(§3ibst:7:3)  ;
064800130422        if alfasoc>*zeros  ;
064900130422        soctraini=%int(alfasoc)  ;
065000130422        endif  ;
065100130422        endif  ;
065200130315
065300130315 x0     else  ;
065400130315
065500130315        $Video='CA'  ;
065600130315        clear olst1TRN  ;
065700130315        clear olst1NRRP  ;
065800130315 0      endif  ;
065900130314
066000130122        ENDSR  ;
066100130315       //--------------------------------------------------------------
066200130315       //?Cerca giorno della settimana
066300130315       //--------------------------------------------------------------
066400130315          BEGSR  cercaGIORNO ;
066500130315          // cerco girno della settimana se non devo caricare tutti
066600130315 2        if ilst1tut='G'  ;
066700130315          xgiose1 (Gioggmmaa:GIOset)  ;
066800130315          // Salvo il giorno della settimana per cui devo caricare
066900130315          //  i traini
067000130315 3          if gioset>*zeros   ;
067100130315             w02gioset=GIOSET   ;
067200130315            else  ;
067300130315             clear w02gioset  ;
067400130315 3          endif  ;
067500130315 2        endif  ;
067600130315        ENDSR  ;
067700080411       //--------------------------------------------------------------
067800130218       //?Gestione videata S02   - sfl traini con o senza estensione
067900080411       //--------------------------------------------------------------
068000080411       BEGSR GesS02;
068100080411
068200130430         // se errore in prima videata lo emetto e poi esco
068300130430         if $errGrave=*on ;
068400130430         Carsfl='N'  ;
068500130430         ricint='S'  ;
068600130430         else    ;
068700080411         // Inizializzazione videata
068800080411         if  $InzS02 = *on;
068900080411            exsr InzS02;
069000080411             $InzS02  = *off;
069100080411         endif;
069200130430         endif;
069300080411
069400080411         // Posizionamento cursore
069500130412         if  Carsfl  <>'N'  ;
069600130412
069700130412         // Se non ci sono errori e ho indicato posizionamento
069800130412         if  errGenerico = *off and ricInt =' '
069900130412              and  savtrn<>d02trn  ;
070000130412          exsr posizTRN  ;
070100130412         endif  ;
070200130412
070300130123           sfldsp_n=*off;
070400080411           C02rcd = C02csr;
070500130412           if C02rcd=0  ;
070600130412            c02rcd=1  ;
070700130412           endif  ;
070800130412
070900080411         else;
071000080411
071100130123         // Se non è stato caricato nulla emetto sfl vuoto
071200130123           sfldsp_n=*on ;
071300080411         endif;
071400080411
071500080411         // Emissione Testata e Piede con tasti funzionali abilitati
071600080604         if  errGenerico = *off or ricInt ='S'  ;
071700130218           write  lst1T01;
071800130218           write  lst1P02;
071900130123           if sfldsp_n=*on  ;
072000130218           write  lst1d04;
072100130123           endif  ;
072200130123
072300080604           clear  ricInt   ;
072400080411         endif;
072500080411
072600080411         // Emissione videata
072700130218         exfmt  lst1C02;
072800080411
072900080411         reset errMessage;
073000080411         reset errGenerico;
073100130122         clear V1cmsg;
073200080523         clear errScelta ;
073300130315         clear errDTN    ;
073400130412         clear errTRN    ;
073500080411
073600080411         SELECT;
073700130430           WHEN  $ErrGrave=*on  ;
073800130430            $Fine = *on;
073900080411
074000130430         // - F3=Fine  12"=ritorno
074100130412           WHEN  dsp_aid = c_F03
074200130412              or dsp_aid = c_F12 ;
074300080411            $Fine = *on;
074400080411
074500130220         // - F10=Immissione (senza impostazione num traino)
074600130220           WHEN  dsp_aid = c_F10;
074700130130
074800130130           savVideo=$Video  ;
074900130220           $Video = 'D5';
075000130220           $inzD05=*on  ;
075100130314
075200130314           dow $Video='D5'  ;
075300130314            exsr Gesd05     ;
075400130314           enddo ;
075500130314           InterD05=*off ;
075600130314           Protd05 =*off ;
075700130220
075800080411         // Invio
075900080411           OTHER;
076000080523             exsr ContrS02  ;
076100080411             if  errGenerico = *on;
076200080411               leavesr;
076300080411             endif;
076400130315
076500130429         // Se premuto F15 - elaboro  tutto il sfl
076600130429           if    dsp_aid = c_F15;
076700130417           Wglobale='S'  ;
076800130417            $Video='CA'  ;
076900130315           else  ;
077000130315            if wscelta='1'  ;
077100130315             $Video='CA'  ;
077200130315            endif  ;
077300130315           endif  ;
077400080411
077500080411         ENDSL;
077600080411
077700080411       ENDSR;
077800080411       //--------------------------------------------------------------
077900080414       //?Inizializzazione sfl 2
078000080411       //--------------------------------------------------------------
078100080411       BEGSR InzS02;
078200080411
078300130418       exsr Pulizs02 ;
078400080411
078500130218       setll *loval   tntlt01l  ;
078600130218       read   tntlt01l  ;
078700130218
078800130418 1     dow not %eof(tntlt01l)  ;
078900130218
079000130218       // Escludo se annullato  o non decorrente alla data richiesta
079100130418 2     if tltatb=' '  ;
079200130418 3     if w02dtn>=tltdde  and  w02dtn<=tltdsc  ;
079300130218         exsr contr_ferm ;
079400130218
079500130418 4       if carTRN='S'  ;
079600130218          exsr EmisTRN  ;
079700130417
079800130417          // se devo fare una conferma automatica senza far vedere il slf
079900130417          //  se ho carico solo un record --> creo foglio viaggio
080000130417          //  se ne ho caricati più di 1  --> emetto sfl per scelta
080100130418 5       if ilst1ges='A' and ilst1aut='S' and s02nrr>0  ;
080200130418          exsr CreaAUT  ;
080300130418 5       endif  ;
080400130418 4     endif  ;
080500130418 3     endif  ;
080600130418 2     endif  ;
080700130218
080800130218       read   tntlt01l  ;
080900130418 1     enddo  ;
081000130130
081100130418 1       if S02nrr > *zero;
081200080411           C02rcd  = 1;
081300080411           C02csr  = 1;
081400080530
081500080411         else;
081600080411           clear C02rcd;
081700110831           clear C02csr;
081800130412           carSFL='N'  ;
081900130418 1       endif;
082000080411
082100130315       savdtn=w02dtn  ;
082200130412       savaper= d02apFGV;
082300080411       ENDSR;
082400130418       //--------------------------------------------------------------
082500130418       //?Pulizia sfl e campi sfl 2
082600130418       //--------------------------------------------------------------
082700130418       BEGSR Pulizs02  ;
082800130418
082900130418       // Pulizia subfile
083000130418         SflDsp_N    = *on;
083100130418         SflDspCtl_N = *on;
083200130418         write  lst1C02;
083300130418         SflDspCtl_N = *off;
083400130418         SflEnd      = *off;
083500130418
083600130418         clear W_SflPag;
083700130418         $EoF = *off;
083800130418         clear C02rcd;
083900130418         clear S02nrr;
084000130418         clear V1cmsg;
084100130418         clear carsfl  ;
084200130418 1       if ilst1ges=' '  ;
084300130418          clear V01dric  ;
084400130418 1       endif  ;
084500130418         errMessage  = *off;
084600130418         errGenerico = *off;
084700130418         errscelta=*off  ;
084800130418         NoTRN=*off  ;
084900130418         ENDSR  ;
085000130218       //--------------------------------------------------------------
085100130219       //?controllo se previsto il terminal come fermata
085200130218       //--------------------------------------------------------------
085300130218       BEGSR Contr_ferm  ;
085400130418
085500130218       clear cartrn  ;
085600130220       NoTRN=*off  ;
085700130218       setll  (tlttrn:tltdde)  tntlr01l  ;
085800130218       reade  (tlttrn:tltdde)  tntlr01l  ;
085900130218
086000130218       dow not %eof(tntlr01l)  ;
086100130218       if tlratb=' '  and tlrcar='S' and tlrfil=v01fgs  ;
086200130218        cartrn='S'  ;
086300130218       leave  ;
086400130218       endif  ;
086500130218
086600130218       reade  (tlttrn:tltdde)  tntlr01l  ;
086700130218       enddo  ;
086800130218       ENDSR;
086900080411       //--------------------------------------------------------------
087000080411       //?Caricamento singola pagina S02
087100080411       //--------------------------------------------------------------
087200130218       BEGSR EmisTRN  ;
087300080411
087400080411         // - Caricamento dati nel record del subfile
087500080414         clear s02sce  ;
087600130412         clear s02aper ;
087700130412         s02aper_rev=*off  ;
087800130417         s02sce_prot=*off  ;
087900130412
088000130218         s02trn=tlttrn  ;
088100130417         h02dde=tltdde  ;
088200130218         s02tfp=tlttfp  ;
088300130218         s02tfa=tlttfa  ;
088400130218          chain tlttfa azorg01l  ;
088500130219  1       if %found(azorg01l)  ;
088600130122           s02dtfa=orgdes  ;
088700130219  1       endif  ;
088800130412
088900130412         // Se sono in apertura FGV visualizzo se già aperto
089000130412 1     if ilst1ges='A'   ;
089100130412        setll  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
089200130412        reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
089300130412 2      dow not %eof(fnfgv03l)  ;
089400130412 3       if   fgvatb=' '  ;
089500130412          s02aper='Ap'   ;
089600130429          //  s02aper_rev=*on  ;
089700130412          leave  ;
089800130412 3       endif  ;
089900130412       reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
090000130412 2     enddo  ;
090100130412
090200130412       // Se non è aperto verifico se si è SBINATO   ;
090300130412 2       if s02aper=*blanks  ;
090400130412           setll  (v01fgs:w02dtn:0000000) fnfgv03l  ;
090500130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
090600130412 3         dow not %eof(fnfgv03l)  ;
090700130412
090800130412 4          if   fgvatb=' '  ;
090900130412            // chain fnfv4 record "D"
091000130412            chain (FGVlnp:fgvnfv:'P':000:'D') fnfv401l ;
091100130412
091200130412 5          if %found(fnfv401l) and fv4atb=' '  ;
091300130422            alfaTRN=%subst(fv4not:1:7)  ;
091400130422 6          if alfaTRN>*zeros and alfaTRN=%editc(tlttrn:'X')  ;
091500130412             s02aper='Sb'   ;
091600130412             s02aper_rev=*on  ;
091700130412             leave  ;
091800130412 6          endif  ;
091900130412 5          endif  ;
092000130412 4          endif  ;
092100130412
092200130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
092300130412 3     enddo  ;
092400130412
092500130412 2       endif  ;
092600130412 1       endif  ;
092700130315
092800130315         clear Almenouno  ;
092900130412
093000130412         // non scrivo se devo escludere i già aperti
093100130412 0       if (d02apFGV='S' and s02aper=*blanks) or
093200130412             d02apfgv=' ' ;
093300130218
093400130219         // Lettura ESTENSIONI : mi memorizzo i giorni della sett
093500130219         //  se inseriti tutti non faccio nessuna proposta da listino traini
093600130219         //  per il trazionista, altrimenti si
093700130219         setll tlttrn     fnfgt01l       ;
093800130219         reade tlttrn     fnfgt01l       ;
093900130219
094000130219  1      dow not %eof(fnfgt01l)  ;
094100130315
094200130412         // se devo caricare solo il giorno previsto della sett --> controllo
094300130315         clear Indx ;
094400130315         if ilst1TUT='G'  and fgtgse<>*blanks  ;
094500130315         Indx=%scan(w02gioset:fgtgse)  ;
094600130315         endif  ;
094700130315
094800130315  2a     if ilst1tut=' ' or fgtgse= *blanks or Indx>0   ;
094900130315
095000130219         s02pdr=fgtpdr  ;
095100130219         chain (ktltip:s02pdr)  tntlz01l  ;
095200130219  2      if %found(tntlz01l ) ;
095300130219           s02dpdr=tlzrsc  ;
095400130219  2      endif  ;
095500130219
095600130319         clear   s02set  ;
095700130313         if %subst(fgtgse:1:1)='1'  ;
095800130313          %subst(s02set:1:1)='L'  ;
095900130313  2      endif  ;
096000130313         if %subst(fgtgse:2:1)='2'  ;
096100130313          %subst(s02set:2:1)='M'  ;
096200130313  2      endif  ;
096300130313         if %subst(fgtgse:3:1)='3'  ;
096400130313          %subst(s02set:3:1)='M'  ;
096500130313  2      endif  ;
096600130313         if %subst(fgtgse:4:1)='4'  ;
096700130313          %subst(s02set:4:1)='G'  ;
096800130313  2      endif  ;
096900130313         if %subst(fgtgse:5:1)='5'  ;
097000130313          %subst(s02set:5:1)='V'  ;
097100130313  2      endif  ;
097200130313         if %subst(fgtgse:6:1)='6'  ;
097300130313          %subst(s02set:6:1)='S'  ;
097400130313  2      endif  ;
097500130313         if %subst(fgtgse:7:1)='7'  ;
097600130313          %subst(s02set:7:1)='L'  ;
097700130313  2      endif  ;
097800130313
097900130219         s02dcn=fgtdcn  ;
098000130219         s02trm=fgttrm ;
098100130219         s02trr=fgttrr ;
098200130219         clear h02imm ;
098300130315         h02nrrp=FGTnrr  ;
098400130219
098500130221         Sott_FGT=*on  ;
098600130417          H02notrn=notrn  ;
098700130417
098800130219           S02nrr += 1;
098900130219           write lst1S02;
099000130417
099100130220           NoTRN=*on  ;
099200130315           Almenouno='S'  ;
099300130219
099400130315  2a     endif  ;
099500130315
099600130219         reade tlttrn     fnfgt01l       ;
099700130219  1     enddo  ;
099800130412  0     endif  ;
099900130219
100000130218         // Lettura listino per proporre il trazionista
100100130315         // Per scelta solo se non ho scritto per nulla il traino
100200130412         // e se non ho ancora perto un foglio su quel traino
100300130412
100400130417  1    if Ricerca_prp=*off  or  Almenouno=*blanks
100500130417 0       and (d02apFGV=' ' or  s02aper=*blanks)  ;
100600130315
100700130218       setll  (tlttrn:tltdde)  titll01l  ;
100800130218       reade  (tlttrn:tltdde)  titll01l  ;
100900130218
101000130315  1a   dow not %eof(titll01l)  ;
101100130219  2    if tllatb=' '   ;
101200130219       // Se non trovata estensione del trazionista --> propongo
101300130219         chain (tlttrn:tllpdr)  fnfgt01l       ;
101400130219  3      if not %found(fnfgt01l)  ;
101500130219
101600130218         s02pdr=tllpdr  ;
101700130218         chain (ktltip:s02pdr)  tntlz01l  ;
101800130219  4      if %found(tntlz01l ) ;
101900130218           s02dpdr=tlzrsc  ;
102000130502           else  ;
102100130502           s02dpdr='?????????????'   ;
102200130219  4      endif  ;
102300130219
102400130218         clear s02dcn  ;
102500130218         clear s02set  ;
102600130219         clear s02trm  ;
102700130219         clear s02trr  ;
102800130417         clear h02nrrp ;
102900130218
103000130218          h02imm='S'   ;
103100080411
103200130417          H02notrn=notrn  ;
103300130221          Sott_FGT=*off ;
103400080411           S02nrr += 1;
103500130218           write lst1S02;
103600130220           NoTRN=*on  ;
103700130418           Almenouno='S'  ;
103800130218
103900130219  3     endif                           ;
104000130219  2     endif                           ;
104100130219
104200130218       reade  (tlttrn:tltdde)  titll01l  ;
104300130315  1a    enddo  ;
104400130315  1     endif                           ;
104500130418
104600130418       // se non ne ho caricato nemmeno uno --> emetto traino SENZA
104700130418       //  trazionista
104800130429       if Almenouno=' '
104900130429         and (d02apFGV=' ' or  s02aper=*blanks)  ;
105000130418       clear s02pdr  ;
105100130418       s02dpdr='???????'  ;
105200130418
105300130418         clear s02dcn  ;
105400130418         clear s02set  ;
105500130418         clear s02trm  ;
105600130418         clear s02trr  ;
105700130418         clear h02nrrp ;
105800130418
105900130418          h02imm='S'   ;
106000130418
106100130418          H02notrn=notrn  ;
106200130418          Sott_FGT=*off ;
106300130418           S02nrr += 1;
106400130418           write lst1S02;
106500130418           NoTRN=*on  ;
106600130418       endif  ;
106700130218
106800130221       Sott_FGT=*off ;
106900080411       ENDSR;
107000080414       //--------------------------------------------------------------
107100080414       //?controllo scelta SFL 2
107200080414       //--------------------------------------------------------------
107300080523       BEGSR contrS02 ;
107400080414
107500130130       ErrScelta=*off ;
107600130315       clear Wscelta  ;
107700080415
107800130315       // controllo data traino
107900130315       clear   wlbdat  ;
108000130315       eval g02dat=d02dtn  ;
108100130315       xsrda8 (wlbdat)   ;
108200130315       if g02err='1'    ;
108300130315           errDTN      = *on;
108400130315           errMessage  = *on;
108500130315           errGenerico = *on;
108600130315           V1cmsg = Msg(01);
108700130315       leavesr    ;
108800130315       endif  ;
108900130315
109000130315       w02dtn=g02inv  ;
109100130315       d02dtn=g02dat  ;
109200130412
109300130412       // controllo se richiesto traino
109400130412       clear wok  ;
109500130412 1     if d02trn>0    ;
109600130424       ktrn2=d02trn  ;
109700130424       exsr decorrTRN  ;
109800130412
109900130412 2     if wok=' '  ;
110000130412           errTRN      = *on;
110100130412           errMessage  = *on;
110200130412           errGenerico = *on;
110300130412           V1cmsg = Msg(08);
110400130412       leavesr    ;
110500130412 2     endif  ;
110600130412 1     endif  ;
110700130412
110800130315
110900130315       // se cambiata data devo ricaricare
111000130315       if savdtn<>w02dtn  ;
111100130412        $inzs02=*on  ;
111200130412        dataiso=%date(%int(w02dtn)) ;
111300130412        datadmy=dataiso  ;
111400130412        GIOggmmaa=%editc(%dec(datadmy) :'X') ;
111500130412        exsr CercaGIORNO  ;
111600130315       endif ;
111700130412
111800130412       // Se cambiato flag solo non aperti ricarico SFL
111900130412       if savaper<>D02apFGV ;
112000130412        $inzs02=*on  ;
112100130412       endif  ;
112200130315
112300130218       readc lst1s02;
112400130218 1     DOW  NOT  %eof(fnlst1D);
112500130129
112600130315       if s02sce<>' '  ;
112700130315
112800130412
112900130314       // In GESTIONE opzione '1' non ammmessa
113000130314       if Interrogaz=*off and s02sce='1'  ;
113100130314           errScelta   = *on;
113200130314           errMessage  = *on;
113300130314           errGenerico = *on;
113400130314           V1cmsg = Msg(05);
113500130314          exsr errSFL2  ;
113600130314          leavesr   ;
113700130314       endif  ;
113800130314
113900130314       // INTERROGAZIONE E RICERCA
114000130314       // Ammessa solo scelta '1' per ricerca
114100130314
114200130412       if Interrogaz=*on  and s02sce<>'5' and s02sce<>'T'  ;
114300130315       if ilst1ges='I' or (Ricerca_prp=*on  and s02sce<>'1')  ;
114400130314           errScelta   = *on;
114500130314           errMessage  = *on;
114600130314           errGenerico = *on;
114700130314           V1cmsg = Msg(05);
114800130314          exsr errSFL2  ;
114900130314          leavesr   ;
115000130314       endif  ;
115100130314       endif  ;
115200130412
115300130412       // Interrogazione TRAINO
115400130412       if s02sce='T'   ;
115500130412      /end-free
115600130412     C                   MOVEL     'T05'         D00OP0
115700130412     C                   MOVEL     'O05'         D00OP1
115800130412     C                   MOVEL     '0'           D00F03
115900130412     C                   MOVEL     '0'           D00F12
116000130412     C                   MOVEL     '0'           D00ERR
116100130412     C                   MOVEL     *BLANKS       D00MSG
116200130412     C                   Z-ADD     s02trn        D00TRN
116300130417     C                   Z-ADD     h02dde        D00DDE
116400130412     C                   MOVEL     tntl00ds      KPJBU
116500130412     C                   CALL      'TNTL01C'
116600130412     C                   PARM                    KPJBA
116700130412      /free
116800130412
116900130412          clear s02sce    ;
117000130412          exsr errSFL2  ;
117100130412        else    ;
117200130314
117300130220       // non posso fare visualizzazione su un record in inserimento
117400130319       //  non do errore in apertura FGV
117500130319       if ilst1ges<>'A'   ;
117600130313 2     if h02imm='S' and  s02sce<>'I'   and s02sce<>' '  ;
117700130220           errScelta   = *on;
117800130220           errMessage  = *on;
117900130220           errGenerico = *on;
118000130313           V1cmsg = Msg(02);
118100130220          exsr errSFL2  ;
118200130313          leavesr   ;
118300130220       endif  ;
118400130319       endif  ;
118500130503
118600130503 2     if h02imm='S' and  s02sce= '5'   ;
118700130503          clear s02sce    ;
118800130503          exsr errSFL2  ;
118900130503          leavesr   ;
119000130503       endif  ;
119100130319
119200130315
119300130429       // se premeto F15--> non accetto scelta con '1'
119400130429           if   dsp_aid = c_F15 and s02sce='1'  ;
119500130315           errScelta   = *on;
119600130315           errMessage  = *on;
119700130315           errGenerico = *on;
119800130315           V1cmsg = Msg(06);
119900130315          exsr errSFL2  ;
120000130315          leavesr   ;
120100130315           endif  ;
120200130220
120300130318       // Per scelta aggiorno con SFLNEXG per rileggerli tutti alla fine
120400130318       if s02sce='1'     ;
120500130318          wscelta='1'  ;
120600130318          exsr errSFL2  ;
120700130318       else   ;
120800130129         $Video = 'D5';
120900130129          $inzd05=*on;
121000130129
121100130129         dow $Video='D5'  ;
121200130129          exsr Gesd05     ;
121300130129         enddo ;
121400130130          InterD05=*off ;
121500130130          Protd05 =*off ;
121600130129          clear s02sce    ;
121700130130
121800130130          exsr errSFL2  ;
121900130313          endif  ;
122000130318          endif  ;
122100130319          endif  ;
122200080414
122300130315
122400130218       readc lst1s02;
122500130122       enddo  ;
122600130122
122700130122       ENDSR   ;
122800080603       //--------------------------------------------------------------
122900080603       //?Aggiornamento sfl per errore
123000080603       //--------------------------------------------------------------
123100080603       BEGSR ErrSFL2 ;
123200130417
123300130313        if h02imm='S'  ;
123400130313        Sott_fgt=*off  ;
123500130313        else  ;
123600130313        Sott_fgt=*on   ;
123700130313        endif  ;
123800130313
123900130417         NoTRN=H02noTRN  ;
124000130418         if  s02sce_prot=*off  ;
124100130417          SflNxtChg = *on ;
124200130417         else  ;
124300130417          SflNxtChg = *off ;
124400130417         endif  ;
124500130429
124600130429         if s02aper='Sb'      ;
124700130429          s02aper_rev=*on  ;
124800130429         endif  ;
124900130429
125000130218         update lst1s02;
125100130412         c02csr=s02nrr ;
125200080603
125300130429         s02aper_rev=*off ;
125400080603         errscelta=*off  ;
125500080603         SflNxtChg = *off ;
125600080603       ENDSR;
125700080411
125800130412       //--------------------------------------------------------------
125900130412       //?Posizinamento SFL2 per numero traino
126000130412       //--------------------------------------------------------------
126100130412       BEGSR PosizTRN  ;
126200130412
126300130412       if d02trn=0  ;
126400130412        c02csr=1  ;
126500130412       else  ;
126600130412
126700130412       eval s02nrr=1  ;
126800130412       chain s02nrr   lst1s02;
126900130412
127000130412       dow %found     ;
127100130412       if s02trn=d02trn  ;
127200130412         c02csr=s02nrr ;
127300130412         leave  ;
127400130412       endif  ;
127500130412
127600130412       s02nrr=s02nrr+1  ;
127700130412
127800130412       chain s02nrr   lst1s02;
127900130412       enddo   ;
128000130412       endif  ;
128100130412       savtrn=d02trn  ;
128200130412
128300130412       ENDSR;
128400130129       //--------------------------------------------------------------
128500130129       //?Gestione videata D05
128600130129       //--------------------------------------------------------------
128700130129         BEGSR GesD05;
128800130129
128900130129         // Inizializzazione videata
129000130129
129100130129         if  $inzd05 = *on;
129200130129         // Verifico che data proporre per le richieste
129300130129           exsr Inzd05    ;
129400130129          $Inzd05=*off      ;
129500130129         endif ;
129600130129
129700130129         // Emissione testata
129800130129         if  errGenerico = *off or ricInt='S';
129900130218           write lst1T01;
130000130129           clear ricint   ;
130100130129         endif;
130200130129
130300130129         // Emissione videata
130400130218         exfmt lst1D05;
130500130129
130600130129         clear V1cmsg;
130700130129         clear errGenerico;
130800130422         ErrMessage =*off  ;
130900130313         clear errgg1  ;
131000130313         clear errgg2  ;
131100130313         clear errgg3  ;
131200130313         clear errgg4  ;
131300130313         clear errgg5  ;
131400130313         clear errgg6  ;
131500130313         clear errgg7  ;
131600130419         clear ERRTRN_IMM      ;
131700130419         clear ERRTRN_nw       ;
131800130422         clear ERRpdr          ;
131900130422         clear ERRdcn          ;
132000130422         clear ERRTRM          ;
132100130129
132200130129    1    select;
132300130129
132400130129         // F12=ritorno
132500130129           when dsp_aid = c_F12;
132600130129               $Video = 'S2';
132700130130               Errgenerico=*off  ;
132800130130               ErrMessage =*off  ;
132900130129
133000130129           other  ;
133100130129             exsr Contrd05 ;
133200130129
133300130130             if InterD05=*on  ;
133400130130             ErrMessage=*off  ;
133500130130             ErrGenerico=*off  ;
133600130130             endif    ;
133700130130
133800130129    2        if  errGenerico = *on;
133900130129               leavesr;
134000130129    2        endif;
134100130129
134200130130    2      if   InterD05=*on    ;
134300130129               $Video = 'S2';
134400130129    2      endif  ;
134500130129
134600130129 2         if    dsp_aid = c_F16;
134700130129               $Inzs02=*on  ;
134800130129               $Video = 'S2';
134900130129 2           endif;
135000130129
135100130129 2         if    dsp_aid = c_F06;
135200130129            exsr  AggiorD05  ;
135300130129
135400130129 3           if  errGenerico = *on;
135500130129               leavesr;
135600130129             else  ;
135700130129               $Video = 'S2';
135800130129 3           endif;
135900130313 2         endif;
136000130313
136100130313 2         if    dsp_aid = c_F16;
136200130313            delete fnfgtfis ;
136300130313               $Video = 'S2';
136400130313 2         endif;
136500130129
136600130129 1           endsl  ;
136700130129
136800130502          if   $Video = 'S2';
136900130502          h02nrrp=savnrrp5  ;
137000130502          endif  ;
137100130502
137200130129          ENDSR;
137300130129       //--------------------------------------------------------------
137400130129       //?Inizializzazione VIDEATA 5
137500130129       //--------------------------------------------------------------
137600130129       BEGSR Inzd05;
137700130220       Interd05=*off  ;
137800130220       Protd05 =*off  ;
137900130220       CopiaD05=*off  ;
138000130220       Spostad05=*off  ;
138100130220       Manutd05=*off  ;
138200130220       Imm_d05 =*off  ;
138300130502       savnrrp5=h02nrrp  ;
138400130129
1385001302201      if s02sce='2'  ;
138600130129       v01dric= ' MANUTENZIONE '  ;
138700130220       ManutD05=*ON  ;
1388001302201      ENDIF  ;
1389001302201      if s02sce='3'  ;
139000130220       v01dric= 'C  O  P  I  A '  ;
139100130220       PROTD05=*ON  ;
139200130220       copiaD05=*ON  ;
1393001302201      ENDIF  ;
1394001303131      if s02sce='8'  ;
139500130220       v01dric= ' S P O S T A  ' ;
139600130220       PROTD05=*ON  ;
139700130220       SpostaD05=*ON  ;
1398001302201      ENDIF  ;
1399001302201      if s02sce='5'  ;
140000130129       v01dric= 'INTERROGAZIONE'  ;
140100130130       INTERD05=*ON  ;
140200130129       PROTD05=*ON  ;
1403001302201      ENDIF  ;
140400130220
1405001302201      if s02sce='I' or S02sce=' '  ;
140600130220       v01dric= ' INSERIMENTO  '  ;
140700130220       Imm_d05=*on  ;
140800130424       clear h02nrrp  ;
1409001302201      ENDIF  ;
141000130220
141100130220       // se si tratta di record di nuova immissione
141200130220       //   pulisco i campi
141300130220       clear d05pru  ;
141400130220       clear d05dim  ;
141500130220       clear d05trn  ;
141600130419       clear d05trn_n;
141700130220       clear d05tfp  ;
141800130220       clear d05dtfp  ;
141900130220       clear d05tfa  ;
142000130220       clear d05dtfa ;
142100130220       clear d05pdr  ;
142200130220       clear d05dpdr ;
142300130220       clear d05dcn  ;
142400130220       clear d05trm  ;
142500130220       clear d05trr  ;
142600130220       clear d05not  ;
142700130220       clear d05salt  ;
142800130220       clear d05gg1   ;
142900130220       clear d05gg2   ;
143000130220       clear d05gg3   ;
143100130220       clear d05gg4   ;
143200130220       clear d05gg5   ;
143300130220       clear d05gg6   ;
143400130220       clear d05gg7   ;
143500130313
143600130424       clear Wforzgg1 ;
143700130424       clear Wforzgg2 ;
143800130424       clear Wforzgg3 ;
143900130424       clear Wforzgg4 ;
144000130424       clear Wforzgg5 ;
144100130424       clear Wforzgg6 ;
144200130424       clear Wforzgg7 ;
144300130424       clear Wfor2gg1 ;
144400130424       clear Wfor2gg2 ;
144500130424       clear Wfor2gg3 ;
144600130424       clear Wfor2gg4 ;
144700130424       clear Wfor2gg5 ;
144800130424       clear Wfor2gg6 ;
144900130424       clear Wfor2gg7 ;
145000130426       clear WforMgg1 ;
145100130426       clear WforMgg2 ;
145200130426       clear WforMgg3 ;
145300130426       clear WforMgg4 ;
145400130426       clear WforMgg5 ;
145500130426       clear WforMgg6 ;
145600130426       clear WforMgg7 ;
145700130424
145800130313       clear d05trn_nw ;
145900130313       clear d05tfpnw;
146000130313       clear d05dtfpnw ;
146100130313       clear d05tfanw;
146200130313       clear d05dtfanw ;
146300130220
146400130220       // Se ho effettuato la scelta --> propongo i dati della riga
146500130220 1     if s02sce<>' '  ;
146600130220
146700130220       // IMMISSIONE
146800130220 2     select ;
146900130220       when s02sce ='I'     ;
147000130313       d05trn=%editc(s02trn:'X')  ;
147100130220       d05tfp=s02tfp  ;
147200130220       d05tfa=s02tfa  ;
147300130313       d05pdr=%editc(s02pdr:'X')  ;
147400130423       // mi posiziono sul padroncino se vuoto, altrimenti sul conducente
147500130423       if d05trn>*zeros and d05pdr>*zeros  ;
147600130423       errdcn=*on   ;
147700130423       endif  ;
147800130423       if d05trn>*zeros and d05pdr<=*zeros  ;
147900130423       d05pdr='?'  ;
148000130423       errpdr=*on   ;
148100130423       endif  ;
148200130423
148300130423       d05dcn='?'  ;
148400130423       d05trm='?'  ;
148500130220
148600130313       // altre opzioni
148700130313 2     when s02sce  <>' ' and s02sce<>'I'  ;
148800130315       chain  h02nrrp  fnfgt00f  ;
148900130220 3     if %found(fnfgt00f) ;
149000130313       d05trn=%editc(s02trn:'X')  ;
149100130419       d05trn_n= s02trn  ;
149200130220       d05tfp=s02tfp  ;
149300130220       d05tfa=s02tfa  ;
149400130313       d05pdr=%editc(fgtpdr:'X')  ;
149500130220       d05dcn=fgtdcn  ;
149600130220       d05trm=fgttrm  ;
149700130220       d05trr=fgttrr  ;
149800130220       d05not=fgtnot  ;
149900130313       d05pru=fgtpru  ;
150000130313       dataiso=%date(fgtdim) ;
150100130313       dataeur=dataiso  ;
150200130313       d05dim=%dec(dataeur)   ;
150300130220
150400130220  4    if fgtgse=*blanks  ;
150500130417        //  d05salt ='SI'   ;
150600130220       else  ;
150700130313         d05gg1= %subst(fgtgse:1:1)     ;
150800130313         d05gg2= %subst(fgtgse:2:1)     ;
150900130313         d05gg3= %subst(fgtgse:3:1)     ;
151000130313         d05gg4= %subst(fgtgse:4:1)     ;
151100130313         d05gg5= %subst(fgtgse:5:1)     ;
151200130313         d05gg6= %subst(fgtgse:6:1)     ;
151300130313         d05gg7= %subst(fgtgse:7:1)     ;
151400130220 4       endif  ;
151500130220
151600130220  3    endif  ;
151700130220  2    endsl  ;
151800130221  1    endif  ;
151900130220
152000130220       if d05pdr > *zeros   ;
152100130220       kpdr =%int(d05pdr)  ;
152200130220         chain (ktltip:kpdr)  tntlz01l  ;
152300130221         if %found(tntlz01l ) ;
152400130220           D05dpdr=tlzrsc  ;
152500130221         endif  ;
152600130221       endif  ;
152700130220
152800130220       if d05tfp>0  ;
152900130220       chain d05tfp azorg01l  ;
153000130220  2      if %found(azorg01l ) ;
153100130220           D05dtfp =orgdes ;
153200130220  2      endif  ;
153300130220       endif  ;
153400130220       if d05tfa>0  ;
153500130220       chain d05tfa azorg01l  ;
153600130220  2      if %found(azorg01l ) ;
153700130220           D05dtfa =orgdes ;
153800130220  2      endif  ;
153900130220  1    endif  ;
154000130129
154100130129       ENDSR  ;
154200130129       //--------------------------------------------------------------
154300130129       //?controllo dati   video 5
154400130129       //--------------------------------------------------------------
154500130129       BEGSR contrd05 ;
154600130313
154700130313       // Per immissione controllo il traino
154800130313 1     if imm_d05=*on   ;
154900130313       Indx=%scan('?':d05trn)  ;
155000130313 2     if indx>0   ;
155100130313        exsr ricercaTraino  ;
155200130313
155300130313 3     if d00trn>0  ;
155400130313       d05trn = %editc(d00trn:'X')  ;
155500130422
155600130422        chain  (d00trn:d00dde) tntlt01l  ;
155700130422 4      if %found(tntlt01l)  ;
155800130422        d05tfp    = tlttfp              ;
155900130422        d05tfa    = tlttfa              ;
156000130422        exsr DecoTRNimm ;
156100130422 x4    else  ;
156200130422       clear d05trn  ;
156300130422  4     endif  ;
156400130313 x3    else  ;
156500130313       clear d05trn  ;
156600130313  3    endif  ;
156700130313
156800130313       ricint=*on  ;
156900130314       leavesr     ;
157000130313  2    endif    ;
157100130313  1    endif   ;
157200130313
157300130313       // Per copia o spostamento  vedo se attivata la ricerca traino
157400130313       Indx=%scan('?':d05trn_nw)  ;
157500130419  0    if indx>0   ;
157600130313        exsr ricercaTraino  ;
157700130313
157800130419 1     if d00trn>0  ;
157900130313        d05trn_nw = %editc(d00trn:'X')  ;
158000130419
158100130419        chain  (d00trn:d00dde) tntlt01l  ;
158200130419 2      if %found(tntlt01l)  ;
158300130422        d05tfpNW  = tlttfp              ;
158400130422        d05tfaNW  = tlttfa              ;
158500130422        exsr DecoTRN_nw ;
158600130422 x2    else  ;
158700130422        clear d05trn_nw  ;
158800130422 2      endif  ;
158900130313
159000130419 x1    else  ;
159100130313        clear d05trn_nw  ;
159200130419 1     endif  ;
159300130313
159400130313       ricint=*on  ;
159500130419       leavesr  ;
159600130419 0     endif    ;
159700130419
159800130419       // Immissione     Traino --> controllo
159900130502 1     // if imm_d05=*on   ;
160000130419       alfaTRN=d05trn  ;
160100130419       exsr  CtrTRN  ;
160200130419 2      if Errgenerico=*on   ;
160300130419         Errtrn_imm=*on   ;
160400130419         leavesr  ;
160500130422 x2      else  ;
160600130422        d05tfp    = tlttfp              ;
160700130422        d05tfa    = tlttfa              ;
160800130422        exsr DecoTRNimm ;
160900130419 2      endif    ;
161000130502 1     // endif    ;
161100130419
161200130419       // Copia o sposta traino --> controllo
161300130419 1     if Copiad05=*on or spostad05=*on    ;
161400130419       alfatrn=d05trn_nw  ;
161500130419       exsr  CtrTRN  ;
161600130419 2      if Errgenerico=*on   ;
161700130419         Errtrn_nw =*on   ;
161800130419         leavesr  ;
161900130422 x2      else  ;
162000130422        d05tfpNW  = tlttfp              ;
162100130422        d05tfaNW  = tlttfa              ;
162200130422        exsr DecoTRN_nw ;
162300130419 2      endif    ;
162400130419 1     endif    ;
162500130422
162600130422       // trazionista --> interrogo quelli del traino
162700130422       if d05pdr  = *zeros    or d05pdr=*blanks  ;
162800130422           errpdr      = *on;
162900130422           errMessage  = *on;
163000130422           errGenerico = *on;
163100130422           V1cmsg = Msg(10);
163200130422           leavesr  ;
163300130422           endif  ;
163400130422
163500130422       if  d05pdr<= *ZEROS   ;
163600130422           errpdr      = *on;
163700130422           errMessage  = *on;
163800130422           errGenerico = *on;
163900130422           V1cmsg = Msg(10);
164000130422           leavesr  ;
164100130422       ELSE  ;
164200130422
164300130422       kpdr =%int(d05pdr)  ;
164400130422         chain (ktltip:kpdr)  tntlz01l  ;
164500130422         if %found(tntlz01l ) ;
164600130422           D05dpdr=tlzrsc  ;
164700130422         else  ;
164800130422           errpdr      = *on;
164900130422           errMessage  = *on;
165000130422           errGenerico = *on;
165100130422           V1cmsg = Msg(10);
165200130422           leavesr  ;
165300130422         endif  ;
165400130422       endif  ;
165500130502
165600130502       // Il trazionista deve essere previsto dal traino
165700130502       setll (kpdr:tlttrn:tltdde) titll02l   ;
165800130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
165900130502       dow not %eof(titll02l)  ;
166000130502       if tllatb=' '  ;
166100130502       leave    ;
166200130502       else  ;
166300130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
166400130502       endif  ;
166500130502       enddo  ;
166600130502
166700130502       if %eof(titll02l)  ;
166800130502           errpdr      = *on;
166900130502           errMessage  = *on;
167000130502           errGenerico = *on;
167100130502           V1cmsg = Msg(18);
167200130502           leavesr  ;
167300130502       endif  ;
167400130422
167500130422       // Conducente --> vedo se accreditato
167600130422 1     if d05dcn<>*blanks   ;
167700130422
167800130422       // controllo se c'e' ricerca
167900130422       Indx=%scan('?':d05dcn)  ;
168000130422 2     if indx>0   ;
168100130422         clear tntl83ds  ;
168200130422         i83soc=soctraini ;
168300130422         i83ksc=kpdr      ;
168400130423         i83mod='1'  ;
168500130423         i83drf=%editc(w02dtn:'X')  ;
168600130422         kpjbu=tntl83ds  ;
168700130422         callp TNTL83R  (kpjba)  ;
168800130422         tntl83ds =kpjbu  ;
168900130422 3       if o83_aut<>*blanks  ;
169000130422          d05dcn= o83_aut  ;
169100130423          else  ;
169200130423          clear d05dcn ;
169300130422 3       endif   ;
169400130423
169500130422         ricint=*on  ;
169600130423         errdcn=*on  ;
169700130422       leavesr  ;
169800130422 2     endif   ;
169900130422
170000130422       chain (soctraini:kpdr:d05dcn)  aiata01l  ;
170100130422       if not %found(aiata01l) or ataatb<>' '  ;
170200130422           errdcn      = *on;
170300130422           errMessage  = *on;
170400130422           errGenerico = *on;
170500130422           V1cmsg = Msg(11);
170600130422           leavesr  ;
170700130422       endif  ;
170800130422
170900130422       // se alla data decorrenza è staduto il rapporto  --> errore
171000130422       if atadtfir>0 and w02dtn > atadtfir  ;
171100130422           errdcn      = *on;
171200130422           errMessage  = *on;
171300130422           errGenerico = *on;
171400130422           V1cmsg = Msg(12);
171500130422           leavesr  ;
171600130422       endif  ;
171700130422       endif  ;
171800130422
171900130422       // Targa Motrice
172000130422       if d05trm<>*blanks  ;
172100130423
172200130423       // controllo se c'e' ricerca
172300130423       Indx=%scan('?':d05trm)  ;
172400130423 2     if indx>0   ;
172500130423         clear tntl84ds  ;
172600130423         i84soc=soctraini ;
172700130423         i84ksc=kpdr      ;
172800130423         i84mod='1'  ;
172900130423         i84drf=%editc(w02dtn:'X')  ;
173000130423         kpjbu=tntl84ds  ;
173100130423         callp TNTL84R  (kpjba)  ;
173200130423         tntl84ds =kpjbu  ;
173300130423 3       if o84_TRM<>*blanks  ;
173400130423          d05trm= o84_TRM  ;
173500130423          else  ;
173600130423          clear d05trm ;
173700130423 3       endif   ;
173800130423
173900130423         ricint=*on  ;
174000130423         errtrm=*on  ;
174100130423       leavesr  ;
174200130423 2     endif   ;
174300130423
174400130422       chain (soctraini:kpdr:d05trm)  aiatm01l  ;
174500130422       if not %found(aiatm01l) or atmatb<>' '  ;
174600130422           errtrm      = *on;
174700130422           errMessage  = *on;
174800130422           errGenerico = *on;
174900130422           V1cmsg = Msg(13);
175000130422           leavesr  ;
175100130422       endif  ;
175200130422
175300130422       // se alla data decorrenza è scaduto il rapporto  --> errore
175400130422       if atmdtfir>0 and w02dtn > atmdtfir  ;
175500130422           errtrm      = *on;
175600130422           errMessage  = *on;
175700130422           errGenerico = *on;
175800130422           V1cmsg = Msg(13);
175900130422           leavesr  ;
176000130422       endif  ;
176100130422       endif  ;
176200130422
176300130422       // Giorni partenza
176400130313       if d05gg1<>' '  and d05gg1<>'1'  ;
176500130422           errgg1      = *on;
176600130313           errMessage  = *on;
176700130313           errGenerico = *on;
176800130313           V1cmsg = Msg(03);
176900130313           %subst(v1cmsg:22:1)='1'  ;
177000130313           %subst(v1cmsg:59:10)='LUNEDÌ    ' ;
177100130313           leavesr;
177200130313       endif  ;
177300130503       // controllo se giorno previsto dal traino
177400130503       if d05gg1='1' and %subst(tltgse:1:1)=' '  ;
177500130503           errgg1      = *on;
177600130503           errMessage  = *on;
177700130503           errGenerico = *on;
177800130503           V1cmsg = Msg(19);
177900130503       endif  ;
178000130503
178100130313       if d05gg2<>' '  and d05gg2<>'2'  ;
178200130313           errgg2      = *on;
178300130313           errMessage  = *on;
178400130313           errGenerico = *on;
178500130313           V1cmsg = Msg(03);
178600130313           %subst(v1cmsg:22:1)='2'  ;
178700130313           %subst(v1cmsg:59:10)='MARTEDÌ   ' ;
178800130313           leavesr;
178900130313       endif  ;
179000130503       // controllo se giorno previsto dal traino
179100130503       if d05gg2='2' and %subst(tltgse:2:1)=' '  ;
179200130503           errgg2      = *on;
179300130503           errMessage  = *on;
179400130503           errGenerico = *on;
179500130503           V1cmsg = Msg(19);
179600130503       endif  ;
179700130313       if d05gg3<>' '  and d05gg3<>'3'  ;
179800130313           errgg3      = *on;
179900130313           errMessage  = *on;
180000130313           errGenerico = *on;
180100130313           V1cmsg = Msg(03);
180200130313           %subst(v1cmsg:22:1)='3'  ;
180300130313           %subst(v1cmsg:59:10)='MERCOLEDÌ ' ;
180400130313           leavesr;
180500130313       endif  ;
180600130503       // controllo se giorno previsto dal traino
180700130503       if d05gg3='3' and %subst(tltgse:3:1)=' '  ;
180800130503           errgg3      = *on;
180900130503           errMessage  = *on;
181000130503           errGenerico = *on;
181100130503           V1cmsg = Msg(19);
181200130503       endif  ;
181300130313       if d05gg4<>' '  and d05gg4<>'4'  ;
181400130313           errgg4      = *on;
181500130313           errMessage  = *on;
181600130313           errGenerico = *on;
181700130313           V1cmsg = Msg(03);
181800130313           %subst(v1cmsg:22:1)='4'  ;
181900130313           %subst(v1cmsg:59:10)='GIOVEDÌ   ' ;
182000130313           leavesr;
182100130313       endif  ;
182200130503       // controllo se giorno previsto dal traino
182300130503       if d05gg4='4' and %subst(tltgse:4:1)=' '  ;
182400130503           errgg4      = *on;
182500130503           errMessage  = *on;
182600130503           errGenerico = *on;
182700130503           V1cmsg = Msg(19);
182800130503       endif  ;
182900130313       if d05gg5<>' '  and d05gg5<>'5'  ;
183000130313           errgg5      = *on;
183100130313           errMessage  = *on;
183200130313           errGenerico = *on;
183300130313           V1cmsg = Msg(03);
183400130313           %subst(v1cmsg:22:1)='5'  ;
183500130313           %subst(v1cmsg:59:10)='VENERDÌ   ' ;
183600130313           leavesr;
183700130313       endif  ;
183800130503       if d05gg5='5' and %subst(tltgse:5:1)=' '  ;
183900130503           errgg5      = *on;
184000130503           errMessage  = *on;
184100130503           errGenerico = *on;
184200130503           V1cmsg = Msg(19);
184300130503       endif  ;
184400130313       if d05gg6<>' '  and d05gg6<>'6'  ;
184500130313           errgg6      = *on;
184600130313           errMessage  = *on;
184700130313           errGenerico = *on;
184800130313           V1cmsg = Msg(03);
184900130313           %subst(v1cmsg:22:1)='6'  ;
185000130313           %subst(v1cmsg:59:10)='SABATO    ' ;
185100130313           leavesr;
185200130313       endif  ;
185300130503       if d05gg6='6' and %subst(tltgse:6:1)=' '  ;
185400130503           errgg6      = *on;
185500130503           errMessage  = *on;
185600130503           errGenerico = *on;
185700130503           V1cmsg = Msg(19);
185800130503       endif  ;
185900130313       if d05gg7<>' '  and d05gg7<>'7'  ;
186000130313           errgg7      = *on;
186100130313           errMessage  = *on;
186200130313           errGenerico = *on;
186300130313           V1cmsg = Msg(03);
186400130313           %subst(v1cmsg:22:1)='7'  ;
186500130313           %subst(v1cmsg:59:10)='DOMENICA  ' ;
186600130313           leavesr;
186700130313       endif  ;
186800130503       if d05gg7='7' and %subst(tltgse:7:1)=' '  ;
186900130503           errgg7      = *on;
187000130503           errMessage  = *on;
187100130503           errGenerico = *on;
187200130503           V1cmsg = Msg(19);
187300130503       endif  ;
187400130129
187500130313       IF  D05GG1= ' ' and D05GG2= ' '   and D05GG3= ' '
187600130313          and D05GG4= ' '  and D05GG5= ' ' and D05GG6= ' '
187700130422          and D05GG7= ' '   ;
187800130423           errgg1      = *on;
187900130313           errMessage  = *on;
188000130313           errGenerico = *on;
188100130313           V1cmsg = Msg(04);
188200130313           leavesr;
188300130313       endif  ;
188400130424
188500130502       // controllo da fare solo se non annullo
188600130502       if   dsp_aid <> c_F16;
188700130502
188800130424       // Per ogni giorno inserito verifico se c'e' un'altra proposta
188900130424       // se sposto o copio uso il traino di destino
189000130424       if d05trn_nw>*zeros   ;
189100130424        ktrn=%int(d05trn_nw)  ;
189200130424       else  ;
189300130424        ktrn=%int(d05trn)  ;
189400130424       endif  ;
189500130424
189600130424       setll   ktrn     fnfgt01l  ;
189700130424       reade   ktrn     fnfgt01l  ;
189800130424
189900130424       dow  not %eof(fnfgt01l)  ;
190000130424       if fgtnrr <>  h02nrrp ;
190100130424       if d05gg1<>' '  and d05gg1=%subst(fgtgse:1:1)
190200130424        and wforzgg1=' '  ;
190300130424           errgg1      = *on;
190400130424           errMessage  = *on;
190500130424           errGenerico = *on;
190600130424           V1cmsg = Msg(14);
190700130424           %subst(v1cmsg:39:10)='  LUNEDÌ  ' ;
190800130424           wforzgg1='S'  ;
190900130424           leavesr;
191000130424       endif  ;
191100130424       if d05gg2<>' '  and d05gg2=%subst(fgtgse:2:1)
191200130424        and wforzgg2=' '  ;
191300130424           errgg2      = *on;
191400130424           errMessage  = *on;
191500130424           errGenerico = *on;
191600130424           V1cmsg = Msg(14);
191700130424           %subst(v1cmsg:39:10)='  MARTEDÌ ' ;
191800130424           wforzgg2='S'  ;
191900130424           leavesr;
192000130424       endif  ;
192100130424       if d05gg3<>' '  and d05gg3=%subst(fgtgse:3:1)
192200130424        and wforzgg3=' '  ;
192300130424           errgg3      = *on;
192400130424           errMessage  = *on;
192500130424           errGenerico = *on;
192600130424           V1cmsg = Msg(14);
192700130424           %subst(v1cmsg:39:10)='MERCOLEDÌ ';
192800130424           wforzgg3='S'  ;
192900130424           leavesr;
193000130424       endif  ;
193100130424       if d05gg4<>' '  and d05gg4=%subst(fgtgse:4:1)
193200130424        and wforzgg4=' '  ;
193300130424           errgg4      = *on;
193400130424           errMessage  = *on;
193500130424           errGenerico = *on;
193600130424           V1cmsg = Msg(14);
193700130424           %subst(v1cmsg:39:10)=' GIOVEDÌ  ' ;
193800130424           wforzgg4='S'  ;
193900130424           leavesr;
194000130424       endif  ;
194100130424       if d05gg5<>' '  and d05gg5=%subst(fgtgse:5:1)
194200130424        and wforzgg5=' '  ;
194300130424           errgg5      = *on;
194400130424           errMessage  = *on;
194500130424           errGenerico = *on;
194600130424           V1cmsg = Msg(14);
194700130424           %subst(v1cmsg:39:10)=' VENERDÌ  ' ;
194800130424           wforzgg5='S'  ;
194900130424           leavesr;
195000130424       endif  ;
195100130424       if d05gg6<>' '  and d05gg6=%subst(fgtgse:6:1)
195200130424        and wforzgg6=' '  ;
195300130424           errgg6      = *on;
195400130424           errMessage  = *on;
195500130424           errGenerico = *on;
195600130424           V1cmsg = Msg(14);
195700130424           %subst(v1cmsg:39:10)='  SABATO  ' ;
195800130424           wforzgg6='S'  ;
195900130424           leavesr;
196000130424       endif  ;
196100130424       if d05gg7<>' '  and d05gg7=%subst(fgtgse:7:1)
196200130424        and wforzgg7=' '  ;
196300130424           errgg7      = *on;
196400130424           errMessage  = *on;
196500130424           errGenerico = *on;
196600130424           V1cmsg = Msg(14);
196700130424           %subst(v1cmsg:39:10)=' DOMENICA ' ;
196800130424           wforzgg7='S'  ;
196900130424           leavesr;
197000130424       endif  ;
197100130424       endif  ;
197200130424
197300130424       reade   ktrn     fnfgt01l  ;
197400130424       enddo  ;
197500130424
197600130426       // Controllo se il CONDUCENTE è già presente in un traino decorrente
197700130424       //  nello stesso giorno
197800130424       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtdcn='''+
197900130424        d05dcn + ''''                                    ;
198000130424       EXEC sql  prepare s1 from : wrkgetlista ;
198100130424       EXEC sql  declare a1 cursor for s1      ;
198200130424       EXEC sql  open    a1                    ;
198300130424
198400130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
198500130424  1    dow sqlcod=0    ;
198600130426       wtipo='C'  ;
198700130426       Exsr ErrDOPP  ;
198800130426       if v1cmsg<>*blanks  ;
198900130426        leave  ;
199000130426       endif  ;
199100130426
199200130424
199300130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
199400130424       enddo           ;
199500130424
199600130424       EXEC sql  CLOSE   a1                    ;
199700130426
199800130426       if v1cmsg<>*blanks  ;
199900130426        leavesr;
200000130426       endif  ;
200100130424
200200130426       // Controllo se la TARGA MOTRICE è già presente in un traino decorrente
200300130426       //  nello stesso giorno
200400130426       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtTRM='''+
200500130426        d05trm + ''''                                    ;
200600130426       EXEC sql  prepare s2 from : wrkgetlista ;
200700130426       EXEC sql  declare a2 cursor for s2      ;
200800130426       EXEC sql  open    a2                    ;
200900130426
201000130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
201100130426  1    dow sqlcod=0    ;
201200130426       wtipo='M'  ;
201300130426       Exsr ErrDOPP  ;
201400130426       if v1cmsg<>*blanks  ;
201500130426        leave  ;
201600130426       endif  ;
201700130426
201800130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
201900130426       enddo           ;
202000130426
202100130426       EXEC sql  CLOSE   a2                    ;
202200130426
202300130426       if v1cmsg<>*blanks  ;
202400130426        leavesr;
202500130426       endif  ;
202600130502
202700130502       endif  ;
202800130424
202900130129       ENDSR   ;
203000130426       //--------------------------------------------------------------
203100130426       //?Errore per conducente/taga già presente nel giorno
203200130426       //--------------------------------------------------------------
203300130426       Begsr ErrDOPP  ;
203400130426
203500130426           if wtipo='C'  ;
203600130426            wmsg = Msg(15);
203700130426           else  ;
203800130426            wmsg = Msg(16);
203900130426           endif ;
204000130426
204100130426 2     if sql_fgttrn<> ktrn  and sql_fgttrn<>%int(d05trn)  ;
204200130426
204300130426 3     if d05gg1<>' '  and d05gg1=%subst(sql_fgtgse:1:1)  ;
204400130426 3a     if  (wfor2gg1=' ' and wtipo='C') or (wforMgg1=' ' and wtipo='M') ;
204500130426        // Verifico che il traino sia decorrente
204600130426        ktrn2=sql_fgttrn  ;
204700130426        exsr decorrTRN  ;
204800130426 4      if wok='S'      ;
204900130426           errgg1      = *on;
205000130426           errMessage  = *on;
205100130426           errGenerico = *on;
205200130426           V1cmsg = wmsg   ;
205300130426           %subst(v1cmsg:36:10)='  LUNEDÌ  ' ;
205400130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
205500130426           if wtipo='C'  ;
205600130426            wfor2gg1='S'  ;
205700130426           else  ;
205800130426            wforMgg1='S'  ;
205900130426           endif  ;
206000130426           leavesr;
206100130426 4     endif  ;
206200130426 3a    endif  ;
206300130426 3     endif  ;
206400130426 3     if d05gg2<>' '  and d05gg2=%subst(sql_fgtgse:2:1)  ;
206500130426 3a     if  (wfor2gg2=' ' and wtipo='C') or (wforMgg2=' ' and wtipo='M') ;
206600130426        // Verifico che il traino sia decorrente
206700130426        ktrn2=sql_fgttrn  ;
206800130426        exsr decorrTRN  ;
206900130426 4      if wok='S'      ;
207000130426           errgg2      = *on;
207100130426           errMessage  = *on;
207200130426           errGenerico = *on;
207300130426           V1cmsg = wmsg   ;
207400130426           %subst(v1cmsg:36:10)=' MARTEDÌ  ' ;
207500130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
207600130426           if wtipo='C'  ;
207700130426            wfor2gg2='S'  ;
207800130426           else  ;
207900130426            wforMgg2='S'  ;
208000130426           endif  ;
208100130426           leavesr;
208200130426 4     endif  ;
208300130426 3a    endif  ;
208400130426 3     endif  ;
208500130426 3     if d05gg3<>' '  and d05gg3=%subst(sql_fgtgse:3:1) ;
208600130426 3a     if  (wfor2gg3=' ' and wtipo='C') or (wforMgg3=' ' and wtipo='M') ;
208700130426        // Verifico che il traino sia decorrente
208800130426        ktrn2=sql_fgttrn  ;
208900130426        exsr decorrTRN  ;
209000130426 4      if wok='S'      ;
209100130426           errgg3      = *on;
209200130426           errMessage  = *on;
209300130426           errGenerico = *on;
209400130426           V1cmsg = wmsg   ;
209500130426           %subst(v1cmsg:36:10)=' MERCOLEDÌ' ;
209600130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
209700130426           if wtipo='C'  ;
209800130426            wfor2gg3='S'  ;
209900130426           else  ;
210000130426            wforMgg3='S'  ;
210100130426           endif  ;
210200130426           leavesr;
210300130426 4     endif  ;
210400130426 3a    endif  ;
210500130426 3     endif  ;
210600130426 3     if d05gg4<>' '  and d05gg4=%subst(sql_fgtgse:4:1)  ;
210700130426 3a     if  (wfor2gg4=' ' and wtipo='C') or (wforMgg4=' ' and wtipo='M') ;
210800130426        // Verifico che il traino sia decorrente
210900130426        ktrn2=sql_fgttrn  ;
211000130426        exsr decorrTRN  ;
211100130426 4      if wok='S'      ;
211200130426           errgg4      = *on;
211300130426           errMessage  = *on;
211400130426           errGenerico = *on;
211500130426           V1cmsg = wmsg   ;
211600130426           %subst(v1cmsg:36:10)=' GIOVEDÌ  ' ;
211700130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
211800130426           if wtipo='C'  ;
211900130426            wfor2gg4='S'  ;
212000130426           else  ;
212100130426            wforMgg4='S'  ;
212200130426           endif  ;
212300130426           leavesr;
212400130426 4     endif  ;
212500130426 3a    endif  ;
212600130426 3     endif  ;
212700130426 3     if d05gg5<>' '  and d05gg5=%subst(sql_fgtgse:5:1)  ;
212800130426 3a     if  (wfor2gg5=' ' and wtipo='C') or (wforMgg5=' ' and wtipo='M') ;
212900130426        // Verifico che il traino sia decorrente
213000130426        ktrn2=sql_fgttrn  ;
213100130426        exsr decorrTRN  ;
213200130426 4      if wok='S'      ;
213300130426           errgg5      = *on;
213400130426           errMessage  = *on;
213500130426           errGenerico = *on;
213600130426           V1cmsg = wmsg   ;
213700130426           %subst(v1cmsg:36:10)=' VENERDÌ  ' ;
213800130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
213900130426           if wtipo='C'  ;
214000130426            wfor2gg5='S'  ;
214100130426           else  ;
214200130426            wforMgg5='S'  ;
214300130426           endif  ;
214400130426           leavesr;
214500130426 4     endif  ;
214600130426 3a    endif  ;
214700130426 3     endif  ;
214800130426 3     if d05gg6<>' '  and d05gg6=%subst(sql_fgtgse:6:1)  ;
214900130426 3a     if  (wfor2gg6=' ' and wtipo='C') or (wforMgg6=' ' and wtipo='M') ;
215000130426        // Verifico che il traino sia decorrente
215100130426        ktrn2=sql_fgttrn  ;
215200130426        exsr decorrTRN  ;
215300130426 4      if wok='S'      ;
215400130426           errgg6      = *on;
215500130426           errMessage  = *on;
215600130426           errGenerico = *on;
215700130426           V1cmsg = wmsg   ;
215800130426           %subst(v1cmsg:36:10)=' SABATO   ' ;
215900130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
216000130426           if wtipo='C'  ;
216100130426            wfor2gg6='S'  ;
216200130426           else  ;
216300130426            wforMgg6='S'  ;
216400130426           endif  ;
216500130426           leavesr;
216600130426 4     endif  ;
216700130426 3a    endif  ;
216800130426 3     endif  ;
216900130426 3     if d05gg7<>' '  and d05gg7=%subst(sql_fgtgse:7:1) ;
217000130426 3a     if  (wfor2gg7=' ' and wtipo='C') or (wforMgg7=' ' and wtipo='M') ;
217100130426        // Verifico che il traino sia decorrente
217200130426        ktrn2=sql_fgttrn  ;
217300130426        exsr decorrTRN  ;
217400130426 4      if wok='S'      ;
217500130426           errgg7      = *on;
217600130426           errMessage  = *on;
217700130426           errGenerico = *on;
217800130426           V1cmsg = wmsg   ;
217900130426           %subst(v1cmsg:36:10)=' DOMENICA ' ;
218000130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
218100130426           if wtipo='C'  ;
218200130426            wfor2gg7='S'  ;
218300130426           else  ;
218400130426            wforMgg7='S'  ;
218500130426           endif  ;
218600130426           leavesr;
218700130426 4     endif  ;
218800130426 3a    endif  ;
218900130426 3     endif  ;
219000130426
219100130426 2     endif  ;
219200130426
219300130426       ENDSR  ;
219400130424       //--------------------------------------------------------------
219500130424       //?controlla decorrenza di un traino
219600130424       //--------------------------------------------------------------
219700130424       BEGSR DecorrTRN            ;
219800130424       clear wok                  ;
219900130424       setgt  ktrn2   tntlt01l    ;
220000130424       readpe ktrn2   tntlt01l    ;
220100130424 2     dow not %eof(tntlt01l)  ;
220200130424 3     if tltatb=' '  and w02dtn>=tltdde and w02dtn<=tltdsc ;
220300130424        wok='S'  ;
220400130424        leave  ;
220500130424 3     endif  ;
220600130424
220700130424       readpe ktrn2   tntlt01l    ;
220800130424 2     enddo   ;
220900130424       ENDSR   ;
221000130424
221100130422       //--------------------------------------------------------------
221200130422       //?Decodifica fermate iniziale e finale traino
221300130422       //--------------------------------------------------------------
221400130422        BEGSR DEcoTRNimm ;
221500130422
221600130422 4      if d05tfp  >0   ;
221700130422         chain d05tfp   azorg01l  ;
221800130422 5       if %found(azorg01l)  ;
221900130422         d05dtfp  =orgdes  ;
222000130422         else  ;
222100130422         d05dtfp  =*all'?'  ;
222200130422 5       endif  ;
222300130422 4      endif  ;
222400130422
222500130422 4      if d05tfa  >0   ;
222600130422         chain d05tfa   azorg01l  ;
222700130422 5       if %found(azorg01l)  ;
222800130422         d05dtfa  =orgdes  ;
222900130422         else  ;
223000130422         d05dtfa  =*all'?'  ;
223100130422 5       endif  ;
223200130422 4      endif  ;
223300130422       //--------------------------------------------------------------
223400130422       ENDSR  ;
223500130422        BEgsr DecoTRN_nw  ;
223600130422         chain d05tfpNW azorg01l  ;
223700130422 3       if %found(azorg01l)  ;
223800130422         d05dtfpnw=orgdes  ;
223900130422         else  ;
224000130422         d05dtfpnw=*all'?'  ;
224100130422 3       endif  ;
224200130422
224300130422         chain d05tfanw azorg01l  ;
224400130422 3       if %found(azorg01l)  ;
224500130422         d05dtfanw=orgdes  ;
224600130422         else  ;
224700130422         d05dtfanw=*all'?'  ;
224800130422 3       endif  ;
224900130422        ENDSR   ;
225000130313       //--------------------------------------------------------------
225100130313       //?Richiama pgm di ricerca traino per il terminal
225200130313       //--------------------------------------------------------------
225300130313       BEGSR ricercaTraino  ;
225400130313       clear tntl00ds  ;
225500130313       d00sca='C'   ;
225600130319       d00dde=w02dtn  ;
225700130313       d00fil= v01fgs  ;
225800130313
225900130313       kpjbu=tntl00ds  ;
226000130313       tntl17r (kpjba)  ;
226100130313       tntl00ds=kpjbu ;
226200130313       ENDSR  ;
226300130129       //--------------------------------------------------------------
226400130430       //?Aggiornoa richiesta singola    D05
226500130129       //--------------------------------------------------------------
226600130129       BEGSR AggiorD05 ;
226700130220
226800130313       // IMMISSIONE
226900130313       if Imm_d05=*on    ;
227000130430        clear fnfgtfis   ;
227100130313        fgtpru=knmus  ;
227200130313        fgtdim=datcor  ;
227300130313        fgttrn=%int(d05trn)  ;
227400130313       endif  ;
227500130313
227600130313       // COPIA
227700130313       if copiad05=*on  ;
227800130430        clear fnfgtfis   ;
227900130313        fgtpru=knmus  ;
228000130313        fgtdim=datcor  ;
228100130313        fgttrn=%int(d05trn_nw)  ;
228200130313       endif  ;
228300130313
228400130220       fgtpdr=%int(d05pdr) ;
228500130220       fgtdcn=d05dcn  ;
228600130220       fgttrm=d05trm;
228700130220       fgttrr=d05trr;
228800130220       fgtnot=d05not ;
228900130313       fgtgse=d05gg1+d05gg2+d05gg3+d05gg4+d05gg5+d05gg6+d05gg7  ;
229000130220
229100130313       if Imm_d05=*on  or Copiad05=*on ;
229200130220       write fnfgtfis  ;
229300130220       endif  ;
229400130220
229500130220       if   ManutD05=*on  ;
229600130220       update fnfgtfis  ;
229700130220       endif   ;
229800130313
229900130313       if   spostad05=*on  ;
230000130313        fgttrn=%int(d05trn_nw)  ;
230100130313       update fnfgtfis  ;
230200130313       endif   ;
230300130129
230400130129           $Inzs02=*on  ;
230500130129
230600130129       ENDSR  ;
230700130417
230800130417       //--------------------------------------------------------------
230900130429       //?elabora SFL per scelta proposte
231000130417       //--------------------------------------------------------------
231100130417           BEGSR ElaboraSFL   ;
231200130417
231300130417           clear s02sce  ;
231400130417           clear s02sce_prot ;
231500130418           clear unascelta ;
231600130417
231700130417 0         if wglobale=' '  ;
231800130417
231900130417           readc lst1s02;
232000130417 1         DOW  NOT  %eof(fnlst1D) and $Fine=*off ;
232100130417
232200130417 2         if    s02sce= '1'  ;
232300130417 3           if ilst1ges='R'  ;
232400130417              olst1TRN =%editc(s02trn :'X')   ;
232500130417              olst1nrrp=%editc(h02nrrp:'X')  ;
232600130417              olst1OPZIO='1'  ;
232700130417              $Fine = *on;
232800130417              kpjbu=fnlst1ds  ;
232900130417              leave    ;
233000130417
233100130417 x3          else  ;
233200130417
233300130417             exsr AperturaFGV  ;
233400130418             unascelta='1'  ;
233500130417
233600130418  4          if olst4msg<>*blanks  ;
233700130417              errScelta   = *on;
233800130417              errMessage  = *on;
233900130417              errGenerico = *on;
234000130417              V1cmsg = olst4msg  ;
234100130418  4          endif  ;
234200130418  3          endif  ;
234300130417              exsr errSFL2  ;
234400130417 2        endif  ;
234500130417
234600130417            readc  lst1s02   ;
234700130417 1     enddo  ;
234800130418
234900130418       // se non ho fatto nessuna scelta e sono nella apertura
235000130418       //  totale --> apro senza trazionista/proposta
235100130418 1        if Unascelta=' ' and ilst1aut='S'  ;
235200130418            clear h02nrrp  ;
235300130418            clear s02pdr  ;
235400130418             exsr AperturaFGV  ;
235500130418 1        endif  ;
235600130417
235700130417 0     else  ;
235800130417
235900130417       // Scelta globale --> leggo tutto il slf
236000130417       //  non ancora aperti
236100130417
236200130417       eval   s02nrr=1  ;
236300130417       chain   s02nrr lst1s02;
236400130418 1     dow  %found           ;
236500130417
236600130417 3           if ilst1ges='R'  ;
236700130417              olst1TRN =%editc(s02trn :'X')   ;
236800130417              olst1nrrp=%editc(h02nrrp:'X')  ;
236900130417              olst1OPZIO='1'  ;
237000130417              $Fine = *on;
237100130417              kpjbu=fnlst1ds  ;
237200130417              leave    ;
237300130417
237400130417 x3          else  ;
237500130417
237600130417             exsr AperturaFGV  ;
237700130417
237800130418  4          if olst4msg<>*blanks  ;
237900130417              errScelta   = *on;
238000130417              errMessage  = *on;
238100130417              errGenerico = *on;
238200130417              V1cmsg = olst4msg  ;
238300130418  4          endif  ;
238400130418  3          endif  ;
238500130417              exsr errSFL2  ;
238600130429
238700130417         s02nrr=s02nrr+1  ;
238800130417         chain   s02nrr lst1s02;
238900130417  1    enddo  ;
239000130417
239100130417 0     endif  ;
239200130417
239300130417       ENDSR  ;
239400130417
239500130417       //--------------------------------------------------------------
239600130417       //?Richiamo pgm per apertua fogli viaggio
239700130417       //--------------------------------------------------------------
239800130417       BEGSR AperturaFGV  ;
239900130417
240000130417              clear fnlst4ds  ;
240100130417              ilst4ges='A'  ;
240200130417              ilst4fgs =%editc(v01fgs:'X')    ;
240300130417              ilst4TRN =%editc(s02trn :'X')   ;
240400130418 1            if h02nrrp>0  ;
240500130417                ilst4nrrp=%editc(h02nrrp:'X')  ;
240600130418 x1           else  ;
240700130418                ilst4nrrp=*all'0'  ;
240800130417
240900130417                // Se non si tratta di richiamo automatico ma scelta
241000130429                //  a video la passo altrimenti vuoto per far scegliere
241100130417                //  dopo
241200130417                ilst4pdr=%editc(s02pdr:'X')   ;
241300130502              //   ilst4old='S'  ;
241400130418  1           endif  ;
241500130417              ilst4dfv=w02dtn  ;
241600130429
241700130429              // Se premuto F6 passo al chiamante
241800130429              if dsp_aid = c_F06;
241900130429               iolst4for='F6 '  ;
242000130429              endif  ;
242100130429
242200130417              kpjbu=fnlst4ds    ;
242300130417              callp  FNLST4R (kpjba)   ;
242400130417              fnlst4ds=kpjbu   ;
242500130417
242600130429             if iolst4for= *blanks ;
242700130429              clear s02sce   ;
242800130429             endif  ;
242900130417       ENDSR  ;
243000130417       //--------------------------------------------------------------
243100130417       //?Apertura automatica dei fogli viaggio traino oper traino
243200130417       //--------------------------------------------------------------
243300130417       BEGSR creaAUT      ;
243400130417       if s02nrr=1  ;
243500130418        exsr AperturaFGV ;
243600130417       else   ;
243700130417       // emetto SFL per la scelta
243800130418           C02rcd  = 1;
243900130418           C02csr  = 1;
244000130418           $InzS02  = *off;
244100130418           ctl02_prot=*on   ;
244200130418
244300130418           exsr Gess02  ;
244400130418
244500130418           // controllo cosa scelto: se nulla --> apro senza trazionista
244600130418           if   $Video = 'CA';
244700130418             exsr    ElaboraSFL   ;
244800130503           else  ;
244900130503            clear h02nrrp  ;
245000130503            clear s02pdr   ;
245100130503            exsr AperturaFGV ;
245200130418           endif  ;
245300130418       endif  ;
245400130418
245500130418           ctl02_prot=*off  ;
245600130418           exsr Pulizs02  ;
245700130417
245800130417       ENDSR  ;
245900130417       //--------------------------------------------------------------
246000130417       //?Scelta su singolo traino: proposta o trazionista
246100130417       //--------------------------------------------------------------
246200130417       BEGSR SceltasingTRN;
246300130417       clear s02sce   ;
246400130417       exsr errsfl2  ;
246500130417       ENDSR  ;
246600080206       //--------------------------------------------------------------
246700080206       //?Operazioni finali.
246800080206       //--------------------------------------------------------------
246900080206       BEGSR RoutEnd;
247000080410
247100130502       if $Video='CA' and s02sce='1' and ilst1ges='R' ;
247200130315         *inrt=*on  ;
247300130315         else  ;
247400080206         *inLR = *on;
247500130315         endif  ;
247600130315
247700080206         return;
247800080206
247900080206       ENDSR;
248000080206
248100080206      /end-free
248200130419     C*-----------------------------------------------------***********
248300130419     C* CONTROLLO NUMERO TRAINO                             * CTRTRN  *
248400130419     C*-----------------------------------------------------***********
248500130419     C     CTRTRN        BEGSR
248600130419     C*
248700130419     c* controllo se inserito un numero
248800130419     c                   testn                   alfaTRN              30
248900130419     c                   movel     alfaTRN       alfaUNO           1
249000130419    2c                   if        not *in30 or alfaUNO<'0'
249100130419     C                   MOVEL     MSG(8)        V1cMSG
249200130422     c                   eval      errMessage  = *on
249300130422     c                   eval      errGenerico = *on
249400130419     C                   GOTO      ENDTRN
249500130419    2C                   ENDIF
249600130419     c
249700130419     c                   movel     alfaTRN       ktrn              7 0
249800130419     c
249900130419     C     ktrn          SETLL     TNTLT01L
250000130422     C     ktrn          READE     TNTLT01L
250100130419     C*
250200130419    2c                   dow       not %eof(tntlt01l)
250300130422     c                   if        tltatb=' ' and
250400130422     c                             w02dtn>=tltdde and w02dtn<=tltdsc
250500130422     c                   leave
250600130422     c                   else
250700130422     C     ktrn          READE     TNTLT01L
250800130422     c                   endif
250900130422     c                   enddo
251000130422     c
251100130422     c* Errore : traino non trovat o
251200130422     c                   if        %eof(tntlt01l)
251300130422     C                   MOVEL     MSG(8)        V1cMSG
251400130422     c                   eval      errMessage  = *on
251500130422     c                   eval      errGenerico = *on
251600130422     C                   GOTO      ENDTRN
251700130422     c                   endif
251800130422     c
251900130422     C*   Errore: traino  "A-AFFLUENZA"
252000130419    4C     TLTTTR        IFEQ      'A'
252100130419     C                   MOVEL     MSG(8)        V1cMSG
252200130422     c                   eval      errMessage  = *on
252300130422     c                   eval      errGenerico = *on
252400130419     C                   GOTO      ENDTRN
252500130422     c                   endif
252600130422     c
252700130422     c* controllo se la filiale gestione è una fermata di carico
252800130422     c                   exsr      contr_ferm
252900130422     c
253000130422     c                   if        cartrn=' '
253100130422     C                   MOVEL     MSG(9)        V1cMSG
253200130422     c                   eval      errMessage  = *on
253300130422     c                   eval      errGenerico = *on
253400130422     C                   GOTO      ENDTRN
253500130422     c                   endif
253600130419     C     ENDTRN        ENDSR
253700080206
253800080206       //--------------------------------------------------------------
253900080206       //?Schiere a tempo di compilazione.
254000080206       //--------------------------------------------------------------
254100080206
254200080410** - MSG ------------------------------------------------------------------ -*
254300130121Data errata
254400130220Proposta ancora da inserire: possibile solo opzione "I-inserimento"
254500130424Immettere il numero 'x' per indicare che la partenza è di xxxxxxxxxx
254600130423Immettere  i giorni di partenza
254700130314Opzione non ammmessa
254800130315Se premuto F7 non effettuare scelte con l'opzione "1"                         6
254900130315Utente non abilitato: chiamare CED di sede !!                                 7
255000130412Numero traino inesistente o non decorrente alla data specificata              8
255100130422La filiale gestione non è una fermata di carico. Traino errato                9
255200130424Codice trazionista errato                                                     10
255300130424Conducente non accreditato per il trazionista indicato                        11
255400130424Il conducente  ha terminato il rapporto col trazionista                       12
255500130424Targa MOTRICE non accredita per il trazionista indicato                       13
255600130424Esiste già una proposta nel giorno di xxxxxxxxxx x questo traino:ENTER forza  14
255700130424Conducente già presente nel giorno xxxxxxxxxx per traino xxxxxxx:ENTER forza  15
255800130426TargaMOTRICE già presente nel gior.xxxxxxxxxx per traino xxxxxxx:ENTER forza  16
255900130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare17
256000130502Il trazionista non è previsto nel traino indicato                             18
256100130503Il giorno non è previsto dal traino                                           19
