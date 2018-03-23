000100080530      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF) DATEDIT(*yMd.)
000200080206      //--------------------------------------------------------------
000300130508      //?FNLST1R - Gestione anagrafica traini per fogli viaggio
000400080206      //--------------------------------------------------------------
000500080206
000600080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100050704
001200130220     fFNfgt00F  uf a e             disk    rename(fnfgt000:Fnfgtfis)
001300130220     fFNfgt01L  if   e           k disk
001400130220     F                                     INFDS(FGTD1)
001500130218     ftntlt01L  if   e           k disk
001501130614     ftntlo01L  if   e           k disk
001600130218     ftntlr01L  if   e           k disk
001700130218     ftitll01L  if   e           k disk
001800130502     ftitll02L  if   e           k disk    rename(titll000:titll002)
001900130412     ffnfv401l  if   e           k disk
002000130412     ffnfgv03l  if   e           k disk
002100130422      // - anagrafiche trazionisti/conducenti/targhe
002200130218     FTNTLZ01L  IF   E           K DISK
002300130422     Faiats01l  IF   E           K DISK
002400130422     Faiata01l  IF   E           K DISK
002500130422     Faiatm01l  IF   E           K DISK
002600080208      // - Organigramma filiale/aziendale
002700080206     fAZORG01L  if   e           k disk
002800130123     fAZCLN01L  if   e           k disk
002900080208      // - Tabelle
003000080206     fTABEL00F  if   e           k disk
003100080604     fTNTBE01l  if   e           k disk
003200080604
003300080208      // - Video Interrogazione anagrafica clienti potenziali
003400130218     ffnlst1D   cf   e             workstn
003500080208     f                                     indds(IndDspF)
003600080206     f                                     infds(InfDspF)
003700130218     f                                     sfile(lst1S02 : S02nrr)
003800080206
003900080206      //---------------------------------------------------------------
004000080206      //?Definizione costanti.
004100080206      //---------------------------------------------------------------
004101130618     d ord_traino      c                   'F11=Ord per n.Traino '
004102130618     d ord_dtaorp      c                   'F11=Ord per data/ora part'
004103130617     d w_ntraino       C                   1
004104130617     d w_dtaorp        C                   2
004105130617     d W_ord           S              1  0
004106130617      *---------------------------------------------------------------------
004107130617      * specifiche per ordinamento forzato sfl
004108130617     d NotUsed         S             16A
004109130617     d ReturnSize      S              9B 0
004110130617     d SizeList        S              9B 0
004111130617     D RrnLast         S              5I 0
004112130617     d*---
004113130617     d MaxKey          C                   2
004114130617     d Ascendente      C                   1
004115130617     d Discendente     C                   2
004116130617     d Carattere       C                   6
004117130617     d Put             C                   1
004118130617     d EndPut          C                   2
004119130617     d Get             C                   3
004120130617     d Numerico        C                   8
004200080411      // - Numero di record in ciascuna videata di subfile
004300130122     d c_SflPag        c                   const(16)
004400080411     d W_SflPag        s              3  0 inz
004500080530     d totrig          s              5  0 inz
004600130122     d ricInt          s              1
004700080207
004800080207      // - Tasti funzionali a video
004900080207     d c_F01           c                   const(x'31')
005000080207     d c_F02           c                   const(x'32')
005100080207     d c_F03           c                   const(x'33')
005200080207     d c_F05           c                   const(x'35')
005300080207     d c_F06           c                   const(x'36')
005400080207     d c_F07           c                   const(x'37')
005500080207     d c_F08           c                   const(x'38')
005600080207     d c_F09           c                   const(x'39')
005700080207     d c_F10           c                   const(x'3A')
005701130617     d c_F11           c                   const(x'3B')
005800080207     d c_F12           c                   const(x'3C')
005900080207     d c_F13           c                   const(x'B1')
006000080207     d c_F14           c                   const(x'B2')
006100080207     d c_F15           c                   const(x'B3')
006200080207     d c_F16           c                   const(x'B4')
006300080207     d c_F17           c                   const(x'B5')
006400080207     d c_F18           c                   const(x'B6')
006500080207     d c_F19           c                   const(x'B7')
006600080207     d c_F20           c                   const(x'B8')
006700080207     d c_F21           c                   const(x'B9')
006800080207     d c_F22           c                   const(x'BA')
006900080207     d c_F23           c                   const(x'BB')
007000080207     d c_F24           c                   const(x'BC')
007100080207     d c_Enter         c                   const(x'F1')
007200080207     d c_RollDown      c                   const(x'F4')
007300080207     d c_RollUp        c                   const(x'F5')
007400080214
007500080214      // - Attributi di visualizzazione
007600080215      //  -  DspAtr() - Normale
007700080214     d C_dspatr_Norm   c                   const(x'20')
007800080215      //  -  DspAtr(RI)
007900080214     d C_dspatr_RI     c                   const(x'21')
008000080215      //  -  DspAtr(ND)
008100080214     d C_dspatr_ND     c                   const(x'27')
008200080215      //  -  DspAtr(BL) / Color(Red)
008300080214     d C_dspatr_BL     c                   const(x'28')
008400080206
008500080206      //---------------------------------------------------------------
008600080206      //?Definizione schiere.
008700080206      //---------------------------------------------------------------
008800080206
008900080206      // - Messaggi di errore
009000130503     d MSG             s             78    dim(19) ctdata perrcd(1)
009100080609
009200080206      //---------------------------------------------------------------
009300080206      //?Definizione aree dati.
009400080206      //---------------------------------------------------------------
009500080206
009600080206      // - Dati utente
009700080206     d §AzUte        e ds                  extname(AZUTE00F)
009800080206     d                                     dtaara
009900080206     d §DatiUte      e ds                  extname(dDatiUte)
010000080206     d                                     dtaara
010100080206
010200080206      //---------------------------------------------------------------
010300080206      //?Definizione strutture dati.
010400080206      //---------------------------------------------------------------
010500080206
010600080206      // - Status
010700080206     d Psds           sds
010800080206     d   SDSpgm          *proc
010900080206
011000080206      // - InfDS
011100080206     d InfDspF         ds
011200080207     d  dsp_aid              369    369a
011300080207     d  sfl_rrn              376    377i 0
011400080207     d  min_nrr              378    379i 0
011500080207     d  num_rcds             380    381i 0
011600080206
011700080206      // - Indicatori su DspF
011800080208     d IndDspF         ds
011900080206        // - Indicatori di gestione del subfile
012000130220     d  NoTRN                         1n   overlay(IndDspF : 02)
012100130220     d  ProtD05                       1n   overlay(IndDspF : 03)
012200130130     d  InterD05                      1n   overlay(IndDspF : 04)
012300130313     d  CopiaD05                      1n   overlay(IndDspF : 05)
012400130220     d  spostad05                     1n   overlay(IndDspF : 06)
012500130220     d  ManutD05                      1n   overlay(IndDspF : 07)
012600130220     d  Imm_d05                       1n   overlay(IndDspF : 08)
012700130221     d  Sott_fgt                      1n   overlay(IndDspF : 09)
012800130314     d  Interrogaz                    1n   overlay(IndDspF : 10)
012900130314     d  Ricerca_PRP                   1n   overlay(IndDspF : 11)
013000130412     d  S02aper_REV                   1n   overlay(IndDspF : 12)
013100130417     d  S02sce_prot                   1n   overlay(IndDspF : 13)
013200130418     d  CTL02_prot                    1n   overlay(IndDspF : 14)
013300130314     d
013400080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
013500080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
013600080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
013700080206     d  SflEnd                        1n   overlay(IndDspF : 33)
013800130218     d  SflEnd3                       1n   overlay(IndDspF : 34)
013900080206        // - Indicatori di errore
014000080206     d  errMessage                    1n   overlay(IndDspF : 28)
014100130313     d  Errgg1                        1n   overlay(IndDspF : 41)
014200130313     d  Errgg2                        1n   overlay(IndDspF : 42)
014300130313     d  Errgg3                        1n   overlay(IndDspF : 43)
014400130313     d  Errgg4                        1n   overlay(IndDspF : 44)
014500130313     d  Errgg5                        1n   overlay(IndDspF : 45)
014600130313     d  Errgg6                        1n   overlay(IndDspF : 46)
014700130313     d  Errgg7                        1n   overlay(IndDspF : 47)
014800130313     d  ErrScelta                     1n   overlay(IndDspF : 48)
014900130315     d  ErrDTN                        1n   overlay(IndDspF : 50)
015000130412     d  ErrTRN                        1n   overlay(IndDspF : 51)
015100130419     d  ErrTRN_imm                    1n   overlay(IndDspF : 52)
015200130422     d  ErrTRN_NW                     1n   overlay(IndDspF : 53)
015300130422     d  Errpdr                        1n   overlay(IndDspF : 54)
015400130422     d  Errdcn                        1n   overlay(IndDspF : 55)
015500130422     d  Errtrm                        1n   overlay(IndDspF : 56)
015600080606     d  errGenerico                   1n   overlay(IndDspF : 99)
015700130220
015701130617     d SflRcd          DS
015702130617     d  s02trn
015703130617     d  s02ggp
015704130617     d  s02hmp
015705130617     d  h02imm
015706130617     d  h02dde
015707130617     d  h02notrn
015709130617     d  H02NRRP
015711130617     d  h02ttr
015712130617     d  s02tfp
015713130617     d  s02tfa
015714130617     d  s02dtfa
015715130617     d  s02aper
015716130617     d  s02dpdr
015717130617     d  s02dcn
015718130617     d  s02set
015719130617     d  s02pdr
015720130617     d  s02trm
015721130617     d  s02trr
015723130617     d  Selected                      1A
015800130220     D FGTD1           DS
015900130220     D  FGTNRR               397    400B 0
016000080206
016100080206      // - Parametri ricevuti
016200080206     d KPJBA         e ds
016300080206
016400080206      // - Reperimento dati utente
016500080206     d TIBS34ds      e ds
016600080206     d dUte01        e ds
016700080206
016800080206      // - Ricerca/Controllo tabelle
016900130313     d tntl00ds      e ds                  inz
017000130313     d TIBS02ds      e ds                  inz
017100130123     d trul33ds      e ds                  inz
017200130422     d ds3I          e ds                  inz
017300080206
017400080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
017500080206     d dLAT          e ds                  inz
017600130122     D TRUL31DS      E DS
017700130122     d POG                            3  0 DIM(250) overlay(o31pog)
017800130122     d
017900130122     d dstu          e ds                  inz
017901130618     d dppa          e ds                  inz
018000130314     d
018100130121     D WLBDAT          DS
018200130121     D  G02DAT                 1      8  0
018300130121     D  G02INV                 9     16  0
018400130121     D  G02ERR                17     17
018500130121     D  G02TGI                18     22  0
018600130313     d
018700130123     D                 DS
018800130123     D  kaaa                   1      4  0
018900130123     D  kmmm                   5      6  0
019000130123     D  kgg                    7      8  0
019100130123     D  kDATA                  1      8  0
019200130123     d
019300130123     D clnmat          DS
019400130123     D  MAT                    1     31    dim(31)
019500130123     D clnpom          DS
019600130123     D  pom                    1     31    dim(31)
019700130123     D                 DS
019800130314     d
019900130314     D FNLST1DS        DS
020000130314     d* ' ' --> gestione proposte
020100130315     d* 'R' --> ricerca  proposte
020200130315     d* 'A' --> ricerca proposte per apertura FGV
020300130314     d* 'I' --> Interrogazione proposte
020400130314     D  Ilst1GES               1      1
020500130314     D  Ilst1DAT               2      9
020600130314     d
020700130314     D* 'G' --> caricare solo le proposte traini del giorno della settimana
020800130314     D*         corrispondente alla data;
020900130314     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
021000130314     D  Ilst1TUT              10     10
021100130314     D  Ilst1fgs              11     13
021200130417     D  Ilst1aut              14     14
021300130315     d
021400130315     d* Campi di output
021500130315     d*  S=apertura dei traini non ancora aperti
021600130315     d*  1=opzione di scelta '1' da cercare in tutte le proposte a video
021700130315     d  Olst1OPZIO            21     21
021800130315     d* per opzione '1'
021900130315     d  Olst1TRN              22     28
022000130315     d* Num relativo record della proposta FNFGT
022100130315     d*  se = *blanks  non c'e'  proposta
022200130315     d  Olst1NRRP             21     33
022300130417     d* E=errore nessun trino caricato
022400130417     d  Olst1ERR              34     34
022500130315     d
022600130315     d
022700130318     D fnlst4ds        DS
022800130318     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
022900130318     D*          B - DA BORDERO'
023000130318     D*          A - apertura foglio
023100130318     D*          C - DA CHIUSURA FV SINGOLA
023200130318     D*          I - DA CHIUSURA DA IMP
023300130318     D*          N - DA INTERROGAZIONE  FV
023400130318     D*          A - apertura fogli viaggio
023500130319     D  ilst4DFV               3     10  0
023600130318     D  ilst4nfv              11     15
023700130318     D  IlsT4GES              16     16
023800130318     D* Se PARMSG pieno --> c'è stato ERRORE
023900130318     D  olst4msg              17     94
024000130429     D  iolsT4FOR             95     97
024100130429     D  IlsT4OLD              98     98
024200130318     D* PARF12 = 'S'    --> premuto F12
024300130318     D  olst4F12             102    102
024400130318     D  ilst4fgs             103    105
024500130318     D  ilst4TRN             106    112
024600130318     D  ilst4NRRP            113    125
024700130417     D  ilst4PDR             126    132
024800130422
024900130422     d* ricerca autisti di un trazionista
025000130422     D tntl83ds        DS
025100130422     d  i83soc                        3s 0
025200130422     d  i83ksc                        7s 0
025300130422     d* i83mod = 1 --> modalità scelta conducente
025400130423     d  i83mod                        1
025500130423     d* data di riferimento alla quale caricare per ora non usata
025600130423     d  i83Drf                        8
025700130422     d  o83_AUT                      35
025800130424     d  o83_TEL                      12
025900130423     d
026000130423     d* ricerca TARGHE  di un trazionista
026100130423     D tntl84ds        DS
026200130423     d  i84soc                        3s 0
026300130423     d  i84ksc                        7s 0
026400130423     d* i84mod = 1 --> modalità scelta conducente
026500130423     d  i84mod                        1
026600130423     d* data di riferimento alla quale caricare per ora non usata
026700130423     d  i84Drf                        8
026800130423     d  o84_TRM                      10
026900130509     d
027000130509      * Parametri per la ricerca trazionisti     - TRUL16R -
027100130509     D TRUL16DS        DS
027200130509     D* Tipologia T_=TRAZIONISTA
027300130509     D  PA3TIP                 1      2
027400130509     D* Ragione sociale trazionista
027500130509     D  PA3RSC                 3     37
027600130509     D* Sistema inform. trazionista
027700130509     D  PA3CSF                38     40
027800130509     D* Codice trazionista
027900130509     D  PA3PDR                41     47  0
028000130509     D* PA3FLG = "3" --> restituisce al pgm chiamante il messaggio che
028100130509     D*   non esistono records per la chiave alfabetica richiesta
028200130509     D  PA3FLG                48     48
028300130509     d
028400080206      //---------------------------------------------------------------
028500080206      //?Definizione variabili globali.
028600080206      //---------------------------------------------------------------
028700080206
028800080206      // - Flags booleani
028900111115     d $Contattato1    s               n   inz(*off)
029000111115     d $Contattato2    s               n   inz(*off)
029100111115     d $EndAtt         s               n   inz(*off)
029200080208     d $Fine           s               n   inz(*off)
029300080208     d $InzD01         s               n   inz(*on)
029400080208     d $InzS01         s               n   inz(*on)
029500080414     d $InzS02         s               n   inz(*on)
029600080603     d $InzS03         s               n   inz(*on)
029700130129     d $InzD05         s               n   inz(*on)
029800080206     d $ErrGrave       s               n   inz(*off)
029900080207     d $EoF            s               n   inz(*off)
030000080208     d $RecOK          s               n   inz(*off)
030100080206
030200080207      // - Campi associati al video
030300130130     d Savvideo        s              2    inz
030400130218     d $Video          s              2    inz('S2')
030500130123     d S03nrr          s              4  0 inz
030600080414     d S02nrr          s              4  0 inz
030700080414     d wPag            s              4  0 inz
030800080414     d wRig            s              3  0 inz
030900080414
031000080604     d wAbi            s                   like(UTEaut)  inz
031100130121     d Indx            s              3  0 inz
031200080619     d Indx2           s              3  0 inz
031300130514     d xi              s              1  0 inz
031400130514     d xx              s              3  0 inz
031500080929     d yy              s              3  0 inz
031600080415     d ss              s              3  0 inz
031700080415     d ff              s              3  0 inz
031800130123     d Ktfp            s              3  0 inz
031900130123     d Ktfa            s              3  0 inz
032000130123     d Wfest           s              1
032100130315     d Almenouno       s              1
032200130315     d Wscelta         s              1
032300130417     d Wglobale        s              1
032400130418     d Unascelta       s              1
032500130430     d alfaFGS         s              3
032600130128     d x§tcod          s                   like(tblcod)
032700130128     d x§tkey          s                   like(tblkey)
032800130128     d x§tdes          s             30
032900130122     d
033000130314     d GIOggmmaa       s              6
033100130314     d GIOset          s              1
033200130315     d W02gioset       s              1
033300130314     d Datcor          s              8  0
033400130220     d w02DTN          s              8  0
033500130315     d savDTN          s              8  0
033600130412     d savTRN          s              7  0
033700130424     d sql_FGTTRN      s              7  0
033800130424     d Ktrn2           s              7  0
033900130422     d alfaTRN         s              7
034000130218     d carTRN          s              1
034100130424     d sql_FGTGSE      s              7
034200130412     d Wok             s              1
034300130412     d Carsfl          s              1
034400130412     d Savaper         s              1
034500130424     d Wforzgg1        s              1
034600130424     d Wforzgg2        s              1
034700130424     d Wforzgg3        s              1
034800130424     d Wforzgg4        s              1
034900130424     d Wforzgg5        s              1
035000130424     d Wforzgg6        s              1
035100130424     d Wforzgg7        s              1
035200130424     d Wfor2gg1        s              1
035300130424     d Wfor2gg2        s              1
035400130424     d Wfor2gg3        s              1
035500130424     d Wfor2gg4        s              1
035600130424     d Wfor2gg5        s              1
035700130424     d Wfor2gg6        s              1
035800130424     d Wfor2gg7        s              1
035900130426     d WforMgg1        s              1
036000130426     d WforMgg2        s              1
036100130426     d WforMgg3        s              1
036200130426     d WforMgg4        s              1
036300130426     d WforMgg5        s              1
036400130426     d WforMgg6        s              1
036500130426     d WforMgg7        s              1
036600130426     d Wtipo           s              1
036700130426     d Wmsg            s                   like(v1cmsg)
036800130422     d AlfaSOC         s              3
036900130422     d SocTraini       s              3  0
037000130422     d Kpdr            s              7  0
037100130502     D savnrrp5        S                   LIKE(h02nrrp)
037200130502     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
037300130424     d wrkgetlista     s           4096    varying
037400130218     d
037500080605     d Dataiso         s               d   datfmt(*iso)
037600080605     d Dataymd         s               d   datfmt(*ymd)
037700080605     d Datadmy         s               d   datfmt(*dmy)
037800130122     d Dataeur         s               d   datfmt(*eur)
037900081010     d DataSYS         s               d   inz(*sys)
038000130123     d Orasys          s               t   timfmt(*hms) inz(*sys)
038001130614     d wnfm            s                   like(tlrnfm)
038100081112     D
038200080206      //---------------------------------------------------------------
038300080206      //?Definizione procedure usate.
038400080206      //---------------------------------------------------------------
038401130617      /COPY QSYSINC/QRPGLESRC,QLGSORT
038402130617     D QLGKL                         16    DIM(MaxKey)
038403130617     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
038404130617     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
038405130617     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
038406130617     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
038407130617
038408130617      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
038409130617      /COPY QSYSINC/QRPGLESRC,QUSEC
038500080414      /copy gaitrasrc/srcprotopr,tibs02r
038600080414      /copy gaitrasrc/srcprotopr,tibs34r
038700130122      /copy gaitrasrc/srcprotopr,trul31r
038800130123      /copy gaitrasrc/srcprotopr,trul33r
038900130122      /copy gaitrasrc/srcprotopr,x§taber
039000130122      /copy gaitrasrc/srcprotopr,xsrda8
039100130314      /copy gaitrasrc/srcprotopr,xgiose1
039200130313      /copy gaitrasrc/srcprotopr,tntl17r
039300130509      /copy gaitrasrc/srcprotopr,trul16r
039400130318      /copy gaitrasrc/srcprotopr,fnlst4r
039500130422      /copy gaitrasrc/srcprotopr,tntl83r
039600130423      /copy gaitrasrc/srcprotopr,tntl84r
039700130122
039800080206      //---------------------------------------------------------------
039900080206      //?Riepilogo indicatori.
040000080206      //---------------------------------------------------------------
040100080207      // - Comuni
040200080207      // 28    : Emissione messaggio di errore a video
040300080207      // - C01/S01
040400080207      // 30    : Condiziona SFLDSP    (*not)
040500080207      // 31    : Condiziona SFLDSPCTL (*not)
040600080207      // 30+31 : Condiziona SFLCLR
040700080207      // 32    : Condiziona SFLNXTCHG in S01
040800080207      // 50    : Errore: Opzione errata
040900080207      // - D01
041000080207      // - Comuni
041100080207      // 99    : Generico di Errore
041200080206      //---------------------------------------------------------------
041300080206
041400080206      //---------------------------------------------------------------
041500080206      //?M A I N - L I N E
041600080206      //---------------------------------------------------------------
041700080206
041800080206     c     *Entry        plist
041900080206     c                   parm                    KPJBA
042000130314     c                   movel     kpjbu         fnlst1ds
042100080206
042200080206      /free
042300080206
042400080206       // Operazioni iniziali
042500080206       exsr RoutInz;
042600080206
042700080206       // Gestione video
042800080206       DOW $Fine = *off;
042900080206         select;
043000080530
043100080530         // SFL di scelta
043200080411           when $Video = 'S2';
043300080411             exsr GesS02;
043400130122
043500130417         // Sfl nascosto per scelta totale dei non apertu
043600130417           when $Video = 'A2';
043700130417            D02apfgv='S'   ;
043800130417            exsr InzS02;
043900130417
044000130417          // Posizionamento cursore
044100130417          if  Carsfl  <>'N'  ;
044200130417              $Video='CA' ;
044300130417           else  ;
044400130417              $Fine = *on;
044500130417              olst1err='E'   ;
044600130417              kpjbu=fnlst1ds  ;
044700130417              leave    ;
044800130417          endif  ;
044900130417
045000130417           // Elaborazione dei record scelti per apertura fogli
045100130417           when $Video = 'CA';
045200130417           exsr    ElaboraSFL   ;
045300130417
045400130319             if ilst1ges='A' and v1cmsg<>*blanks  ;
045500130319               $Video='S2'  ;
045600130319               else  ;
045700130319               $Fine = *on;
045800130319             endif   ;
045900130315
046000080206           other;
046100080206             $Fine = *on;
046200080206         endsl;
046300080206       ENDDO;
046400080206
046500080206       // Operazioni finali
046600130315         exsr RoutEnd;
046700080206
046800130122       //--------------------------------------------------------------
046900130122       //?Reperimento Dati del job (Utente/Operativi).
047000130122       //--------------------------------------------------------------
047100130122       BEGSR DatiJob;
047200130122
047300130122         in(E) §AzUte;
047400130122         if NOT %error;
047500130122           in(E) §DatiUte;
047600130122         endif;
047700130122         if %error or RSut = *blanks;
047800130122           clear TIBS34ds;
047900130122           tibs34r(tibs34ds);
048000130122           in §AzUte;
048100130122           in §DatiUte;
048200130122         endif;
048300130122
048400130122       ENDSR;
048500130122
048600080206       //--------------------------------------------------------------
048700080206       //?Operazioni iniziali.
048800080206       //--------------------------------------------------------------
048900130417       BEGSR RoutInz;
049000080206
049001130619       // Ordinamento sfl di default: per data/ora partenza
049002130619       w_ord=w_dtaorp ;
049003130619       v1pf11=ord_traino;
049021130617
049100130314       clear  Ricerca_PRP  ;
049200130502       $Fine =*off  ;
049300130314
049400130315 0     if olst1OPZIO= ' '  ;
049500130315
049600080206         // Impostazione campi "fissi"
049700080208         TBLkut = 1;
049800080208         TBLcod = '1L';
049900080206
050000080206         // Reperimento dati job
050100080206         exsr DatiJob;
050200080206
050300080206         // Verifica errori e autorità profilo
050400080206         clear  wAbi;
050500080206         clear  dLAT;
050600080206         select;
050700080206         // - Se ho errori nei dati utente esco dal pgm
050800080208           when  DUTerr = 'E';
050900080206             $Fine = *on;
051000080206         // - Se non c'è l'abilitazione:
051100080206         //   - se 1° livello, abilitazioni al terminal
051200080206         //   - se 2° livello, abilitazioni al punto operativo
051300080206         //   - se sede è impossibile ma se capita mando a fine il pgm
051400080208           when  UTEaut = *blank;
051500080206             select;
051600080208               when  DUTlpo = '1';
051700080206                 wAbi  = 'TP';
051800080208               when  DUTlpo = '2';
051900080206                 wAbi  = 'PO';
052000080208               when  DUTlpo = 'S';
052100080206                 $Fine = *on;
052200080206             endsl;
052300080206         // - Altrimenti si caricano le abilitazioni del profilo
052400080206           other;
052500080208             dUTE01 = UTEfaf;
052600080208             if  §UTEpot <> *blank;
052700080208               wAbi = §UTEpot;
052800080206             else;
052900080208               wAbi = UTEaut;
053000080206             endif;
053100080206         ENDSL;
053200080206
053300080206         // Controllo se ok l'abilitazione dell'utente
053400080206         reset TIBS02ds;
053500111115         T02Mod = 'C';
053600080208         T02sif = knsif;
053700080208         T02cod = 'LAT';
053800080208         T02ke1 = wAbi;
053900080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
054000080206
054100080208         if  T02err  = *blank;
054200080208           dLAT = T02uni;
054300080206         endif;
054400080206
054500080206         // Errore o utente non abilitato
054600080208         if  T02err <> *blanks   or
054700080208             §LATabi = 'S';
054800080206           $ErrGrave   = *on;
054900080206           errMessage  = *on;
055000080206           errGenerico = *on;
055100130220           V1cmsg = Msg(02);
055200080206           leavesr;
055300080206         endif;
055400080617
055500080617         clear trul31ds  ;
055600080617         I31abi = wabi    ;
055700080617         I31cdi = DUTdis  ;
055800080617         I31car = DUTare  ;
055900080617         I31cpo = DUTpou  ;
056000080617         TRUL31R   (KPJBA:trul31ds)   ;
056100080617
056200080617         if o31pog=*zeros   ;
056300080617           $ErrGrave   = *on;
056400080617           errMessage  = *on;
056500080617           errGenerico = *on;
056600130315          V1cmsg = Msg(07);
056700080617           leavesr;
056800080617         endif         ;
056900130430
057000130218       $inzs02=*on;
057100130218
057200130218       // Imposto come terminal in gestione simfel
057300130314       if ilst1fgs<=*zeros  ;
057400130314         v01fgs=simfel  ;
057500130314       else  ;
057600130314         v01fgs=%int(ilst1fgs)  ;
057700130314       endif ;
057800130314
057900130430       chain   (v01fgs)  azorg01l   ;
058000130218       if %found(azorg01l)   ;
058100130218        v01dfgs=orgdes  ;
058200130218       endif  ;
058300130218
058400130218       // Propongo data del giorno con selezione traini
058500130218       dataeur=datasys  ;
058600130218       d02dtn=%dec(dataeur)  ;
058700130218       w02dtn=%dec(datasys)  ;
058800130220       Datcor=%dec(datasys)  ;
058900130412       clear d02trn  ;
059000130412       clear savtrn  ;
059100130412       clear savaper ;
059200100520
059300130417       // Tipo gestione
059400130417
059500130314 1     if ilst1ges='I'     ;
059600130314       Interrogaz =*on  ;
059700130314       V01dric='INTERROGAZIONE'  ;
059800130314       endif  ;
059900130314
060000130315 1     if ilst1ges='R'   or ilst1ges='A' ;
060100130315 1     if ilst1ges='R'   ;
060200130315        V01dric=' S C E L T A  '  ;
060300130315       endif  ;
060400130315 1     if ilst1ges='A'   ;
060500130315        V01dric='APERTURA FOGLI'  ;
060600130315       endif  ;
060601130618       // In apertura f.v.p determina l'ordinamento su tab PPA
060602130618 1     if ilst1ges='A'   ;
060603130618         reset TIBS02ds;
060604130618         T02Mod = 'C';
060605130618         T02sif = knsif;
060606130618         T02cod = 'PPA';
060607130618         T02ke1 = %editc(simfel:'X');
060608130618         TNTBE_RicercaControllo  (kpjba : tibs02ds);
060609130618
060610130618         if  T02err  = *blank;
060611130618           dppa = T02uni;
060612130619           if §ppaoaf='T';
060613130619              w_ord=w_ntraino;
060614130619             v1pf11=ord_dtaorp;
060615130618           endif;
060616130618         endif;
060617130618       endif;
060618130618
060700130315       Ricerca_prp=*on  ;
060800130315       Interrogaz =*on  ;
060900130314
061000130314 2       if ilst1dat>*zeros  ;
061100130314          dataiso=%date(%int(ilst1dat)) ;
061200130314         else  ;
061300130314          dataiso=%date(datcor)         ;
061400130314 2       endif  ;
061500130314          dataeur=dataiso  ;
061600130314          datadmy=dataiso  ;
061700130314          d02dtn=%dec(dataeur) ;
061800130314          w02dtn=%dec(dataiso)  ;
061900130314          GIOggmmaa=%editc(%dec(datadmy) :'X') ;
062000130314
062100130315          exsr  CercaGIORNO  ;
062200130315
062300130314 1      endif  ;
062400130417
062500130417        // Se proposta automatica senza far vedere videata,
062600130417        //  carico SFL ed elaboro in automatico
062700130417        if ilst1ges='A' and ilst1aut='S'   ;
062800130417        $Video='A2'  ;
062900130417        endif   ;
063000130422
063100130422        // Carico società trazinisti
063200130422        tblkey='1       ';
063300130422        clear alfasoc ;
063400130422        clear soctraini  ;
063500130422        chain  (1:'3I':tblkey ) tabel00f ;
063600130422        if %found(tabel00f)  ;
063700130422        ds3i=tbluni  ;
063800130422        alfasoc=%subst(§3ibst:7:3)  ;
063900130422        if alfasoc>*zeros  ;
064000130422        soctraini=%int(alfasoc)  ;
064100130422        endif  ;
064200130422        endif  ;
064300130315
064400130315 x0     else  ;
064500130315
064600130315        $Video='CA'  ;
064700130315        clear olst1TRN  ;
064800130315        clear olst1NRRP  ;
064900130315 0      endif  ;
065000130314
065100130122        ENDSR  ;
065200130315       //--------------------------------------------------------------
065300130315       //?Cerca giorno della settimana
065400130315       //--------------------------------------------------------------
065500130315          BEGSR  cercaGIORNO ;
065600130315          // cerco girno della settimana se non devo caricare tutti
065700130315 2        if ilst1tut='G'  ;
065800130315          xgiose1 (Gioggmmaa:GIOset)  ;
065900130315          // Salvo il giorno della settimana per cui devo caricare
066000130315          //  i traini
066100130315 3          if gioset>*zeros   ;
066200130315             w02gioset=GIOSET   ;
066300130315            else  ;
066400130315             clear w02gioset  ;
066500130315 3          endif  ;
066600130315 2        endif  ;
066700130315        ENDSR  ;
066800080411       //--------------------------------------------------------------
066900130218       //?Gestione videata S02   - sfl traini con o senza estensione
067000080411       //--------------------------------------------------------------
067100080411       BEGSR GesS02;
067200080411
067300130430         // se errore in prima videata lo emetto e poi esco
067400130430         if $errGrave=*on ;
067500130430         Carsfl='N'  ;
067600130430         ricint='S'  ;
067700130430         else    ;
067800080411         // Inizializzazione videata
067900080411         if  $InzS02 = *on;
068000080411            exsr InzS02;
068100080411             $InzS02  = *off;
068200080411         endif;
068300130430         endif;
068400080411
068500080411         // Posizionamento cursore
068600130412         if  Carsfl  <>'N'  ;
068700130412
068800130412         // Se non ci sono errori e ho indicato posizionamento
068900130412         if  errGenerico = *off and ricInt =' '
069000130412              and  savtrn<>d02trn  ;
069100130412          exsr posizTRN  ;
069200130412         endif  ;
069300130412
069400130123           sfldsp_n=*off;
069500080411           C02rcd = C02csr;
069600130412           if C02rcd=0  ;
069700130412            c02rcd=1  ;
069800130412           endif  ;
069900130412
070000080411         else;
070100080411
070200130123         // Se non è stato caricato nulla emetto sfl vuoto
070300130123           sfldsp_n=*on ;
070400080411         endif;
070500080411
070600080411         // Emissione Testata e Piede con tasti funzionali abilitati
070700130514         if  errGenerico = *off or ricInt <>' '  ;
070800130218           write  lst1T01;
070900130218           write  lst1P02;
071000130123           if sfldsp_n=*on  ;
071100130218           write  lst1d04;
071200130123           endif  ;
071300130123
071400080604           clear  ricInt   ;
071500080411         endif;
071600080411
071700080411         // Emissione videata
071800130218         exfmt  lst1C02;
071900080411
072000080411         reset errMessage;
072100080411         reset errGenerico;
072200130122         clear V1cmsg;
072300080523         clear errScelta ;
072400130315         clear errDTN    ;
072500130412         clear errTRN    ;
072600080411
072700080411         SELECT;
072800130430           WHEN  $ErrGrave=*on  ;
072900130430            $Fine = *on;
073000080411
073100130430         // - F3=Fine  12"=ritorno
073200130412           WHEN  dsp_aid = c_F03
073300130412              or dsp_aid = c_F12 ;
073400080411            $Fine = *on;
073401130617
073402130617         // - F11=Ordinamento
073403130617           WHEN  dsp_aid = c_F11;
073404130617           select;
073405130617           when w_ord=w_ntraino;
073406130617           w_ord=w_dtaorp;
073407130618           v1pf11=ord_traino;
073408130617           when w_ord=w_dtaorp;
073409130617           w_ord=w_ntraino;
073410130618           v1pf11=ord_dtaorp;
073411130617           endsl;
073412130617           exsr sr_ordina;
073500080411
073600130220         // - F10=Immissione (senza impostazione num traino)
073700130220           WHEN  dsp_aid = c_F10;
073800130130
073900130130           savVideo=$Video  ;
074000130220           $Video = 'D5';
074100130220           $inzD05=*on  ;
074200130314
074300130314           dow $Video='D5'  ;
074400130314            exsr Gesd05     ;
074500130314           enddo ;
074600130314           InterD05=*off ;
074700130314           Protd05 =*off ;
074800130220
074900080411         // Invio
075000080411           OTHER;
075100080523             exsr ContrS02  ;
075200080411             if  errGenerico = *on;
075300080411               leavesr;
075400080411             endif;
075500130315
075600130429         // Se premuto F15 - elaboro  tutto il sfl
075700130429           if    dsp_aid = c_F15;
075800130417           Wglobale='S'  ;
075900130417            $Video='CA'  ;
076000130315           else  ;
076100130315            if wscelta='1'  ;
076200130315             $Video='CA'  ;
076300130315            endif  ;
076400130315           endif  ;
076500080411
076600080411         ENDSL;
076700080411
076800080411       ENDSR;
076900080411       //--------------------------------------------------------------
077000080414       //?Inizializzazione sfl 2
077100080411       //--------------------------------------------------------------
077200080411       BEGSR InzS02;
077300080411
077400130418       exsr Pulizs02 ;
077500080411
077600130218       setll *loval   tntlt01l  ;
077700130218       read   tntlt01l  ;
077800130218
077900130418 1     dow not %eof(tntlt01l)  ;
078000130218
078100130218       // Escludo se annullato  o non decorrente alla data richiesta
078200130418 2     if tltatb=' '  ;
078300130418 3     if w02dtn>=tltdde  and  w02dtn<=tltdsc  ;
078400130218         exsr contr_ferm ;
078500130218
078600130418 4       if carTRN='S'  ;
078700130218          exsr EmisTRN  ;
078800130417
078900130417          // se devo fare una conferma automatica senza far vedere il slf
079000130417          //  se ho carico solo un record --> creo foglio viaggio
079100130417          //  se ne ho caricati più di 1  --> emetto sfl per scelta
079200130508 5       if ilst1ges='A' and ilst1aut='S' and s02nrr>0
079300130508                         and h02ttr<>'H'  ;
079400130418          exsr CreaAUT  ;
079500130418 5       endif  ;
079600130418 4     endif  ;
079700130418 3     endif  ;
079800130418 2     endif  ;
079900130218
080000130218       read   tntlt01l  ;
080100130418 1     enddo  ;
080200130130
080201130617       clear rrnlast;
080202130617
080300130418 1       if S02nrr > *zero;
080400080411           C02rcd  = 1;
080500080411           C02csr  = 1;
080501130617           RrnLast = s02nrr;
080600080530
080700080411         else;
080800080411           clear C02rcd;
080900110831           clear C02csr;
081000130412           carSFL='N'  ;
081100130418 1       endif;
081200080411
081300130315       savdtn=w02dtn  ;
081400130412       savaper= d02apFGV;
081401130617       // vedo se ordinare il sfl
081402130617       if w_ord<>w_ntraino or v1pf11=ord_traino ;
081403130617          exsr sr_ordina;
081408130617       endif;
081500080411       ENDSR;
081600130418       //--------------------------------------------------------------
081700130418       //?Pulizia sfl e campi sfl 2
081800130418       //--------------------------------------------------------------
081900130418       BEGSR Pulizs02  ;
082000130418
082100130418       // Pulizia subfile
082200130418         SflDsp_N    = *on;
082300130418         SflDspCtl_N = *on;
082400130418         write  lst1C02;
082500130418         SflDspCtl_N = *off;
082600130418         SflEnd      = *off;
082700130418
082800130418         clear W_SflPag;
082900130418         $EoF = *off;
083000130418         clear C02rcd;
083100130418         clear S02nrr;
083200130418         clear V1cmsg;
083300130418         clear carsfl  ;
083400130418 1       if ilst1ges=' '  ;
083500130418          clear V01dric  ;
083600130418 1       endif  ;
083700130418         errMessage  = *off;
083800130418         errGenerico = *off;
083900130418         errscelta=*off  ;
084000130418         NoTRN=*off  ;
084100130418         ENDSR  ;
084200130218       //--------------------------------------------------------------
084300130219       //?controllo se previsto il terminal come fermata
084400130218       //--------------------------------------------------------------
084500130218       BEGSR Contr_ferm  ;
084600130418
084700130218       clear cartrn  ;
084701130614       clear wnfm    ;
084800130220       NoTRN=*off  ;
084900130218       setll  (tlttrn:tltdde)  tntlr01l  ;
085000130218       reade  (tlttrn:tltdde)  tntlr01l  ;
085100130218
085200130218       dow not %eof(tntlr01l)  ;
085300130218       if tlratb=' '  and tlrcar='S' and tlrfil=v01fgs  ;
085400130218        cartrn='S'  ;
085401130614        wnfm=tlrnfm;
085500130218       leave  ;
085600130218       endif  ;
085700130218
085800130218       reade  (tlttrn:tltdde)  tntlr01l  ;
085900130218       enddo  ;
086000130218       ENDSR;
086100080411       //--------------------------------------------------------------
086200080411       //?Caricamento singola pagina S02
086300080411       //--------------------------------------------------------------
086400130218       BEGSR EmisTRN  ;
086500080411
086600080411         // - Caricamento dati nel record del subfile
086700080414         clear s02sce  ;
086800130412         clear s02aper ;
086900130412         s02aper_rev=*off  ;
087000130417         s02sce_prot=*off  ;
087100130412
087200130218         s02trn=tlttrn  ;
087300130508         h02ttr=tltttr  ;
087400130417         h02dde=tltdde  ;
087500130218         s02tfp=tlttfp  ;
087600130218         s02tfa=tlttfa  ;
087700130218          chain tlttfa azorg01l  ;
087800130219  1       if %found(azorg01l)  ;
087900130122           s02dtfa=orgdes  ;
088000130219  1       endif  ;
088001130614
088002130614         // giorno e ora di partenza da tntlo
088003130614
088004130614            clear s02ggp;
088005130614            clear s02hmp;
088006130614         if ilst1TUT='G';
088007130614         // se devo considerare solo il giorno della settimana
088008130614         // cerco col giorno e se non lo trovo cerco con " "
088009130614           chain (tlttrn:tltdde:wnfm:w02gioset) tntlo01l;
088010130614           if not %found(tntlo01l);
088011130614              chain (tlttrn:tltdde:wnfm:' ') tntlo01l;
088012130614           endif;
088013130614         else;
088014130614           chain (tlttrn:tltdde:wnfm) tntlo01l;
088015130614         endif;
088016130614         if %found (tntlo01l);
088017130614            s02ggp=tloggp;
088018130614            s02hmp=%editw(tlohhp:'0 :  ');
088019130614         endif;
088100130412
088200130412         // Se sono in apertura FGV visualizzo se già aperto
088300130412 1     if ilst1ges='A'   ;
088400130412        setll  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
088500130412        reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
088600130412 2      dow not %eof(fnfgv03l)  ;
088700130412 3       if   fgvatb=' '  ;
088800130412          s02aper='Ap'   ;
088900130429          //  s02aper_rev=*on  ;
089000130412          leave  ;
089100130412 3       endif  ;
089200130412       reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
089300130412 2     enddo  ;
089400130412
089500130412       // Se non è aperto verifico se si è SBINATO   ;
089600130412 2       if s02aper=*blanks  ;
089700130412           setll  (v01fgs:w02dtn:0000000) fnfgv03l  ;
089800130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
089900130412 3         dow not %eof(fnfgv03l)  ;
090000130412
090100130412 4          if   fgvatb=' '  ;
090200130412            // chain fnfv4 record "D"
090300130412            chain (FGVlnp:fgvnfv:'P':000:'D') fnfv401l ;
090400130412
090500130412 5          if %found(fnfv401l) and fv4atb=' '  ;
090600130422            alfaTRN=%subst(fv4not:1:7)  ;
090700130422 6          if alfaTRN>*zeros and alfaTRN=%editc(tlttrn:'X')  ;
090800130412             s02aper='Sb'   ;
090900130412             s02aper_rev=*on  ;
091000130412             leave  ;
091100130412 6          endif  ;
091200130412 5          endif  ;
091300130412 4          endif  ;
091400130412
091500130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
091600130412 3     enddo  ;
091700130412
091800130412 2       endif  ;
091900130412 1       endif  ;
092000130315
092100130315         clear Almenouno  ;
092200130412
092300130412         // non scrivo se devo escludere i già aperti
092400130412 0       if (d02apFGV='S' and s02aper=*blanks) or
092500130412             d02apfgv=' ' ;
092600130218
092700130219         // Lettura ESTENSIONI : mi memorizzo i giorni della sett
092800130219         //  se inseriti tutti non faccio nessuna proposta da listino traini
092900130219         //  per il trazionista, altrimenti si
093000130509         setll (v01fgs:tlttrn)    fnfgt01l       ;
093100130509         reade (v01fgs:tlttrn)    fnfgt01l       ;
093200130219
093300130219  1      dow not %eof(fnfgt01l)  ;
093400130315
093500130412         // se devo caricare solo il giorno previsto della sett --> controllo
093600130315         clear Indx ;
093700130315         if ilst1TUT='G'  and fgtgse<>*blanks  ;
093800130315         Indx=%scan(w02gioset:fgtgse)  ;
093900130315         endif  ;
094000130315
094100130315  2a     if ilst1tut=' ' or fgtgse= *blanks or Indx>0   ;
094200130315
094300130219         s02pdr=fgtpdr  ;
094400130219         chain (ktltip:s02pdr)  tntlz01l  ;
094500130219  2      if %found(tntlz01l ) ;
094600130219           s02dpdr=tlzrsc  ;
094700130219  2      endif  ;
094800130219
094900130319         clear   s02set  ;
095000130313         if %subst(fgtgse:1:1)='1'  ;
095100130313          %subst(s02set:1:1)='L'  ;
095200130313  2      endif  ;
095300130313         if %subst(fgtgse:2:1)='2'  ;
095400130313          %subst(s02set:2:1)='M'  ;
095500130313  2      endif  ;
095600130313         if %subst(fgtgse:3:1)='3'  ;
095700130313          %subst(s02set:3:1)='M'  ;
095800130313  2      endif  ;
095900130313         if %subst(fgtgse:4:1)='4'  ;
096000130313          %subst(s02set:4:1)='G'  ;
096100130313  2      endif  ;
096200130313         if %subst(fgtgse:5:1)='5'  ;
096300130313          %subst(s02set:5:1)='V'  ;
096400130313  2      endif  ;
096500130313         if %subst(fgtgse:6:1)='6'  ;
096600130313          %subst(s02set:6:1)='S'  ;
096700130313  2      endif  ;
096800130313         if %subst(fgtgse:7:1)='7'  ;
096900130313          %subst(s02set:7:1)='L'  ;
097000130313  2      endif  ;
097100130313
097200130219         s02dcn=fgtdcn  ;
097300130219         s02trm=fgttrm ;
097400130219         s02trr=fgttrr ;
097500130219         clear h02imm ;
097600130315         h02nrrp=FGTnrr  ;
097700130219
097800130221         Sott_FGT=*on  ;
097900130417          H02notrn=notrn  ;
098000130417
098100130219           S02nrr += 1;
098200130219           write lst1S02;
098300130417
098400130220           NoTRN=*on  ;
098500130315           Almenouno='S'  ;
098600130219
098700130315  2a     endif  ;
098800130315
098900130509         reade (v01fgs:tlttrn)    fnfgt01l       ;
099000130219  1     enddo  ;
099100130412  0     endif  ;
099200130219
099300130218         // Lettura listino per proporre il trazionista
099400130315         // Per scelta solo se non ho scritto per nulla il traino
099500130412         // e se non ho ancora perto un foglio su quel traino
099600130412
099700130417  1    if Ricerca_prp=*off  or  Almenouno=*blanks
099800130417 0       and (d02apFGV=' ' or  s02aper=*blanks)  ;
099900130315
100000130218       setll  (tlttrn:tltdde)  titll01l  ;
100100130218       reade  (tlttrn:tltdde)  titll01l  ;
100200130218
100300130315  1a   dow not %eof(titll01l)  ;
100400130219  2    if tllatb=' '   ;
100500130219       // Se non trovata estensione del trazionista --> propongo
100600130509         chain (v01fgs:tlttrn:tllpdr)  fnfgt01l       ;
100700130219  3      if not %found(fnfgt01l)  ;
100800130219
100900130218         s02pdr=tllpdr  ;
101000130218         chain (ktltip:s02pdr)  tntlz01l  ;
101100130219  4      if %found(tntlz01l ) ;
101200130218           s02dpdr=tlzrsc  ;
101300130502           else  ;
101400130510           clear s02pdr  ;
101500130502           s02dpdr='?????????????'   ;
101600130219  4      endif  ;
101700130219
101800130218         clear s02dcn  ;
101900130218         clear s02set  ;
102000130219         clear s02trm  ;
102100130219         clear s02trr  ;
102200130417         clear h02nrrp ;
102300130218
102400130218          h02imm='S'   ;
102500080411
102600130417          H02notrn=notrn  ;
102700130221          Sott_FGT=*off ;
102800080411           S02nrr += 1;
102900130218           write lst1S02;
103000130220           NoTRN=*on  ;
103100130418           Almenouno='S'  ;
103200130218
103300130219  3     endif                           ;
103400130219  2     endif                           ;
103500130219
103600130218       reade  (tlttrn:tltdde)  titll01l  ;
103700130315  1a    enddo  ;
103800130315  1     endif                           ;
103900130418
104000130418       // se non ne ho caricato nemmeno uno --> emetto traino SENZA
104100130418       //  trazionista
104200130429       if Almenouno=' '
104300130429         and (d02apFGV=' ' or  s02aper=*blanks)  ;
104400130418       clear s02pdr  ;
104500130418       s02dpdr='???????'  ;
104600130418
104700130418         clear s02dcn  ;
104800130418         clear s02set  ;
104900130418         clear s02trm  ;
105000130418         clear s02trr  ;
105100130418         clear h02nrrp ;
105200130418
105300130418          h02imm='S'   ;
105400130418
105500130418          H02notrn=notrn  ;
105600130418          Sott_FGT=*off ;
105700130418           S02nrr += 1;
105800130418           write lst1S02;
105900130418           NoTRN=*on  ;
106000130418       endif  ;
106100130218
106200130221       Sott_FGT=*off ;
106300080411       ENDSR;
106400080414       //--------------------------------------------------------------
106500080414       //?controllo scelta SFL 2
106600080414       //--------------------------------------------------------------
106700080523       BEGSR contrS02 ;
106800080414
106900130130       ErrScelta=*off ;
107000130315       clear Wscelta  ;
107100080415
107200130315       // controllo data traino
107300130315       clear   wlbdat  ;
107400130315       eval g02dat=d02dtn  ;
107500130315       xsrda8 (wlbdat)   ;
107600130315       if g02err='1'    ;
107700130315           errDTN      = *on;
107800130315           errMessage  = *on;
107900130315           errGenerico = *on;
108000130315           V1cmsg = Msg(01);
108100130315       leavesr    ;
108200130315       endif  ;
108300130315
108400130315       w02dtn=g02inv  ;
108500130315       d02dtn=g02dat  ;
108600130412
108700130412       // controllo se richiesto traino
108800130412       clear wok  ;
108900130412 1     if d02trn>0    ;
109000130424       ktrn2=d02trn  ;
109100130424       exsr decorrTRN  ;
109200130412
109300130412 2     if wok=' '  ;
109400130412           errTRN      = *on;
109500130412           errMessage  = *on;
109600130412           errGenerico = *on;
109700130412           V1cmsg = Msg(08);
109800130412       leavesr    ;
109900130412 2     endif  ;
110000130412 1     endif  ;
110100130412
110200130315
110300130315       // se cambiata data devo ricaricare
110400130315       if savdtn<>w02dtn  ;
110500130412        $inzs02=*on  ;
110600130412        dataiso=%date(%int(w02dtn)) ;
110700130412        datadmy=dataiso  ;
110800130412        GIOggmmaa=%editc(%dec(datadmy) :'X') ;
110900130412        exsr CercaGIORNO  ;
111000130315       endif ;
111100130412
111200130412       // Se cambiato flag solo non aperti ricarico SFL
111300130412       if savaper<>D02apFGV ;
111400130412        $inzs02=*on  ;
111500130412       endif  ;
111600130315
111700130218       readc lst1s02;
111800130218 1     DOW  NOT  %eof(fnlst1D);
111900130129
112000130315       if s02sce<>' '  ;
112100130315
112200130412
112300130314       // In GESTIONE opzione '1' non ammmessa
112400130314       if Interrogaz=*off and s02sce='1'  ;
112500130314           errScelta   = *on;
112600130314           errMessage  = *on;
112700130314           errGenerico = *on;
112800130314           V1cmsg = Msg(05);
112900130314          exsr errSFL2  ;
113000130314          leavesr   ;
113100130314       endif  ;
113200130314
113300130314       // INTERROGAZIONE E RICERCA
113400130314       // Ammessa solo scelta '1' per ricerca
113500130314
113600130412       if Interrogaz=*on  and s02sce<>'5' and s02sce<>'T'  ;
113700130315       if ilst1ges='I' or (Ricerca_prp=*on  and s02sce<>'1')  ;
113800130314           errScelta   = *on;
113900130314           errMessage  = *on;
114000130314           errGenerico = *on;
114100130314           V1cmsg = Msg(05);
114200130314          exsr errSFL2  ;
114300130314          leavesr   ;
114400130314       endif  ;
114500130314       endif  ;
114600130412
114700130412       // Interrogazione TRAINO
114800130412       if s02sce='T'   ;
114900130412      /end-free
115000130412     C                   MOVEL     'T05'         D00OP0
115100130412     C                   MOVEL     'O05'         D00OP1
115200130412     C                   MOVEL     '0'           D00F03
115300130412     C                   MOVEL     '0'           D00F12
115400130412     C                   MOVEL     '0'           D00ERR
115500130412     C                   MOVEL     *BLANKS       D00MSG
115600130412     C                   Z-ADD     s02trn        D00TRN
115700130417     C                   Z-ADD     h02dde        D00DDE
115800130412     C                   MOVEL     tntl00ds      KPJBU
115900130412     C                   CALL      'TNTL01C'
116000130412     C                   PARM                    KPJBA
116100130412      /free
116200130412
116300130412          clear s02sce    ;
116400130412          exsr errSFL2  ;
116500130412        else    ;
116600130314
116700130220       // non posso fare visualizzazione su un record in inserimento
116800130319       //  non do errore in apertura FGV
116900130319       if ilst1ges<>'A'   ;
117000130313 2     if h02imm='S' and  s02sce<>'I'   and s02sce<>' '  ;
117100130220           errScelta   = *on;
117200130220           errMessage  = *on;
117300130220           errGenerico = *on;
117400130313           V1cmsg = Msg(02);
117500130220          exsr errSFL2  ;
117600130313          leavesr   ;
117700130220       endif  ;
117800130319       endif  ;
117900130503
118000130503 2     if h02imm='S' and  s02sce= '5'   ;
118100130503          clear s02sce    ;
118200130503          exsr errSFL2  ;
118300130503          leavesr   ;
118400130503       endif  ;
118500130319
118600130315
118700130429       // se premeto F15--> non accetto scelta con '1'
118800130429           if   dsp_aid = c_F15 and s02sce='1'  ;
118900130315           errScelta   = *on;
119000130315           errMessage  = *on;
119100130315           errGenerico = *on;
119200130315           V1cmsg = Msg(06);
119300130315          exsr errSFL2  ;
119400130315          leavesr   ;
119500130315           endif  ;
119600130220
119700130318       // Per scelta aggiorno con SFLNEXG per rileggerli tutti alla fine
119800130318       if s02sce='1'     ;
119900130318          wscelta='1'  ;
120000130318          exsr errSFL2  ;
120100130318       else   ;
120200130129         $Video = 'D5';
120300130129          $inzd05=*on;
120400130129
120500130129         dow $Video='D5'  ;
120600130129          exsr Gesd05     ;
120700130129         enddo ;
120800130130          InterD05=*off ;
120900130130          Protd05 =*off ;
121000130129          clear s02sce    ;
121100130130
121200130130          exsr errSFL2  ;
121300130313          endif  ;
121400130318          endif  ;
121500130319          endif  ;
121600080414
121700130315
121800130218       readc lst1s02;
121900130122       enddo  ;
122000130122
122100130122       ENDSR   ;
122200080603       //--------------------------------------------------------------
122300080603       //?Aggiornamento sfl per errore
122400080603       //--------------------------------------------------------------
122500080603       BEGSR ErrSFL2 ;
122600130417
122700130313        if h02imm='S'  ;
122800130313        Sott_fgt=*off  ;
122900130313        else  ;
123000130313        Sott_fgt=*on   ;
123100130313        endif  ;
123200130313
123300130417         NoTRN=H02noTRN  ;
123400130418         if  s02sce_prot=*off  ;
123500130417          SflNxtChg = *on ;
123600130417         else  ;
123700130417          SflNxtChg = *off ;
123800130417         endif  ;
123900130429
124000130429         if s02aper='Sb'      ;
124100130429          s02aper_rev=*on  ;
124200130429         endif  ;
124300130429
124400130218         update lst1s02;
124500130412         c02csr=s02nrr ;
124600080603
124700130429         s02aper_rev=*off ;
124800080603         errscelta=*off  ;
124900080603         SflNxtChg = *off ;
125000080603       ENDSR;
125100080411
125200130412       //--------------------------------------------------------------
125300130412       //?Posizinamento SFL2 per numero traino
125400130412       //--------------------------------------------------------------
125500130412       BEGSR PosizTRN  ;
125600130412
125700130412       if d02trn=0  ;
125800130412        c02csr=1  ;
125900130412       else  ;
126000130412
126100130412       eval s02nrr=1  ;
126200130412       chain s02nrr   lst1s02;
126300130412
126400130412       dow %found     ;
126500130412       if s02trn=d02trn  ;
126600130412         c02csr=s02nrr ;
126700130412         leave  ;
126800130412       endif  ;
126900130412
127000130412       s02nrr=s02nrr+1  ;
127100130412
127200130412       chain s02nrr   lst1s02;
127300130412       enddo   ;
127400130412       endif  ;
127500130412       savtrn=d02trn  ;
127600130412
127700130412       ENDSR;
127800130129       //--------------------------------------------------------------
127900130129       //?Gestione videata D05
128000130129       //--------------------------------------------------------------
128100130129         BEGSR GesD05;
128200130129
128300130129         // Inizializzazione videata
128400130129
128500130129         if  $inzd05 = *on;
128600130129         // Verifico che data proporre per le richieste
128700130129           exsr Inzd05    ;
128800130129          $Inzd05=*off      ;
128900130129         endif ;
129000130129
129100130129         // Emissione testata
129200130514         if  errGenerico = *off or ricInt<>' ';
129300130218           write lst1T01;
129400130129           clear ricint   ;
129500130129         endif;
129600130129
129700130129         // Emissione videata
129800130218         exfmt lst1D05;
129900130129
130000130129         clear V1cmsg;
130100130129         clear errGenerico;
130200130422         ErrMessage =*off  ;
130300130313         clear errgg1  ;
130400130313         clear errgg2  ;
130500130313         clear errgg3  ;
130600130313         clear errgg4  ;
130700130313         clear errgg5  ;
130800130313         clear errgg6  ;
130900130313         clear errgg7  ;
131000130419         clear ERRTRN_IMM      ;
131100130419         clear ERRTRN_nw       ;
131200130422         clear ERRpdr          ;
131300130422         clear ERRdcn          ;
131400130422         clear ERRTRM          ;
131500130129
131600130129    1    select;
131700130129
131800130129         // F12=ritorno
131900130129           when dsp_aid = c_F12;
132000130129               $Video = 'S2';
132100130130               Errgenerico=*off  ;
132200130130               ErrMessage =*off  ;
132300130129
132400130129           other  ;
132500130129             exsr Contrd05 ;
132600130129
132700130130             if InterD05=*on  ;
132800130130             ErrMessage=*off  ;
132900130130             ErrGenerico=*off  ;
133000130130             endif    ;
133100130130
133200130514    2        if  errGenerico = *on or ricint<>' '  ;
133300130129               leavesr;
133400130129    2        endif;
133500130129
133600130130    2      if   InterD05=*on    ;
133700130129               $Video = 'S2';
133800130129    2      endif  ;
133900130129
134000130129 2         if    dsp_aid = c_F16;
134100130129               $Inzs02=*on  ;
134200130129               $Video = 'S2';
134300130129 2           endif;
134400130129
134500130129 2         if    dsp_aid = c_F06;
134600130129            exsr  AggiorD05  ;
134700130129
134800130129 3           if  errGenerico = *on;
134900130129               leavesr;
135000130129             else  ;
135100130129               $Video = 'S2';
135200130129 3           endif;
135300130313 2         endif;
135400130313
135500130313 2         if    dsp_aid = c_F16;
135600130313            delete fnfgtfis ;
135700130313               $Video = 'S2';
135800130313 2         endif;
135900130129
136000130129 1           endsl  ;
136100130129
136200130502          if   $Video = 'S2';
136300130502          h02nrrp=savnrrp5  ;
136400130502          endif  ;
136500130502
136600130129          ENDSR;
136700130129       //--------------------------------------------------------------
136800130129       //?Inizializzazione VIDEATA 5
136900130129       //--------------------------------------------------------------
137000130129       BEGSR Inzd05;
137100130220       Interd05=*off  ;
137200130220       Protd05 =*off  ;
137300130220       CopiaD05=*off  ;
137400130220       Spostad05=*off  ;
137500130220       Manutd05=*off  ;
137600130220       Imm_d05 =*off  ;
137700130502       savnrrp5=h02nrrp  ;
137800130129
1379001302201      if s02sce='2'  ;
138000130129       v01dric= ' MANUTENZIONE '  ;
138100130220       ManutD05=*ON  ;
1382001302201      ENDIF  ;
1383001302201      if s02sce='3'  ;
138400130220       v01dric= 'C  O  P  I  A '  ;
138500130220       PROTD05=*ON  ;
138600130220       copiaD05=*ON  ;
1387001302201      ENDIF  ;
1388001303131      if s02sce='8'  ;
138900130220       v01dric= ' S P O S T A  ' ;
139000130220       PROTD05=*ON  ;
139100130220       SpostaD05=*ON  ;
1392001302201      ENDIF  ;
1393001302201      if s02sce='5'  ;
139400130129       v01dric= 'INTERROGAZIONE'  ;
139500130130       INTERD05=*ON  ;
139600130129       PROTD05=*ON  ;
1397001302201      ENDIF  ;
139800130220
1399001302201      if s02sce='I' or S02sce=' '  ;
140000130220       v01dric= ' INSERIMENTO  '  ;
140100130220       Imm_d05=*on  ;
140200130424       clear h02nrrp  ;
1403001302201      ENDIF  ;
140400130220
140500130220       // se si tratta di record di nuova immissione
140600130220       //   pulisco i campi
140700130220       clear d05pru  ;
140800130220       clear d05dim  ;
140900130220       clear d05trn  ;
141000130419       clear d05trn_n;
141100130220       clear d05tfp  ;
141200130220       clear d05dtfp  ;
141300130220       clear d05tfa  ;
141400130220       clear d05dtfa ;
141500130220       clear d05pdr  ;
141600130220       clear d05dpdr ;
141700130220       clear d05dcn  ;
141800130220       clear d05trm  ;
141900130220       clear d05trr  ;
142000130220       clear d05not  ;
142100130220       clear d05salt  ;
142200130220       clear d05gg1   ;
142300130220       clear d05gg2   ;
142400130220       clear d05gg3   ;
142500130220       clear d05gg4   ;
142600130220       clear d05gg5   ;
142700130220       clear d05gg6   ;
142800130220       clear d05gg7   ;
142900130313
143000130424       clear Wforzgg1 ;
143100130424       clear Wforzgg2 ;
143200130424       clear Wforzgg3 ;
143300130424       clear Wforzgg4 ;
143400130424       clear Wforzgg5 ;
143500130424       clear Wforzgg6 ;
143600130424       clear Wforzgg7 ;
143700130424       clear Wfor2gg1 ;
143800130424       clear Wfor2gg2 ;
143900130424       clear Wfor2gg3 ;
144000130424       clear Wfor2gg4 ;
144100130424       clear Wfor2gg5 ;
144200130424       clear Wfor2gg6 ;
144300130424       clear Wfor2gg7 ;
144400130426       clear WforMgg1 ;
144500130426       clear WforMgg2 ;
144600130426       clear WforMgg3 ;
144700130426       clear WforMgg4 ;
144800130426       clear WforMgg5 ;
144900130426       clear WforMgg6 ;
145000130426       clear WforMgg7 ;
145100130424
145200130313       clear d05trn_nw ;
145300130313       clear d05tfpnw;
145400130313       clear d05dtfpnw ;
145500130313       clear d05tfanw;
145600130313       clear d05dtfanw ;
145700130220
145800130220       // Se ho effettuato la scelta --> propongo i dati della riga
145900130220 1     if s02sce<>' '  ;
146000130220
146100130220       // IMMISSIONE
146200130220 2     select ;
146300130220       when s02sce ='I'     ;
146400130313       d05trn=%editc(s02trn:'X')  ;
146500130220       d05tfp=s02tfp  ;
146600130220       d05tfa=s02tfa  ;
146700130313       d05pdr=%editc(s02pdr:'X')  ;
146800130423       // mi posiziono sul padroncino se vuoto, altrimenti sul conducente
146900130423       if d05trn>*zeros and d05pdr>*zeros  ;
147000130423       errdcn=*on   ;
147100130423       endif  ;
147200130423       if d05trn>*zeros and d05pdr<=*zeros  ;
147300130423       d05pdr='?'  ;
147400130423       errpdr=*on   ;
147500130423       endif  ;
147600130423
147700130423       d05dcn='?'  ;
147800130423       d05trm='?'  ;
147900130220
148000130313       // altre opzioni
148100130313 2     when s02sce  <>' ' and s02sce<>'I'  ;
148200130315       chain  h02nrrp  fnfgt00f  ;
148300130220 3     if %found(fnfgt00f) ;
148400130313       d05trn=%editc(s02trn:'X')  ;
148500130419       d05trn_n= s02trn  ;
148600130220       d05tfp=s02tfp  ;
148700130220       d05tfa=s02tfa  ;
148800130313       d05pdr=%editc(fgtpdr:'X')  ;
148900130220       d05dcn=fgtdcn  ;
149000130220       d05trm=fgttrm  ;
149100130220       d05trr=fgttrr  ;
149200130220       d05not=fgtnot  ;
149300130313       d05pru=fgtpru  ;
149400130313       dataiso=%date(fgtdim) ;
149500130313       dataeur=dataiso  ;
149600130313       d05dim=%dec(dataeur)   ;
149700130220
149800130514  4    //if fgtgse=*blanks  ;
149900130417        //  d05salt ='SI'   ;
150000130514       //else  ;
150100130514         if %subst(fgtgse:1:1)<>' '     ;
150200130514         d05gg1='S'  ;
150300130514         endif  ;
150400130514         if %subst(fgtgse:2:1)<>' '     ;
150500130514         d05gg2='S'  ;
150600130514         endif  ;
150700130514         if %subst(fgtgse:3:1)<>' '     ;
150800130514         d05gg3='S'  ;
150900130514         endif  ;
151000130514         if %subst(fgtgse:4:1)<>' '     ;
151100130514         d05gg4='S'  ;
151200130514         endif  ;
151300130514         if %subst(fgtgse:5:1)<>' '     ;
151400130514         d05gg5='S'  ;
151500130514         endif  ;
151600130514         if %subst(fgtgse:6:1)<>' '     ;
151700130514         d05gg6='S'  ;
151800130514         endif  ;
151900130514         if %subst(fgtgse:7:1)<>' '     ;
152000130514         d05gg7='S'  ;
152100130514         endif  ;
152200130514         //d05gg2= %subst(fgtgse:2:1)     ;
152300130514         //d05gg3= %subst(fgtgse:3:1)     ;
152400130514         //d05gg4= %subst(fgtgse:4:1)     ;
152500130514         //d05gg5= %subst(fgtgse:5:1)     ;
152600130514         //d05gg6= %subst(fgtgse:6:1)     ;
152700130514         //d05gg7= %subst(fgtgse:7:1)     ;
152800130514 4      // endif  ;
152900130220
153000130220  3    endif  ;
153100130220  2    endsl  ;
153200130510       else  ;
153300130510       d05pdr='?'  ;
153400130510       d05dcn='?'  ;
153500130510       d05trm='?'  ;
153600130510       d05trn='?'  ;
153700130221  1    endif  ;
153800130220
153900130220       if d05pdr > *zeros   ;
154000130220       kpdr =%int(d05pdr)  ;
154100130220         chain (ktltip:kpdr)  tntlz01l  ;
154200130221         if %found(tntlz01l ) ;
154300130220           D05dpdr=tlzrsc  ;
154400130221         endif  ;
154500130221       endif  ;
154600130220
154700130220       if d05tfp>0  ;
154800130220       chain d05tfp azorg01l  ;
154900130220  2      if %found(azorg01l ) ;
155000130220           D05dtfp =orgdes ;
155100130220  2      endif  ;
155200130220       endif  ;
155300130220       if d05tfa>0  ;
155400130220       chain d05tfa azorg01l  ;
155500130220  2      if %found(azorg01l ) ;
155600130220           D05dtfa =orgdes ;
155700130220  2      endif  ;
155800130220  1    endif  ;
155900130129
156000130129       ENDSR  ;
156100130129       //--------------------------------------------------------------
156200130129       //?controllo dati   video 5
156300130129       //--------------------------------------------------------------
156400130129       BEGSR contrd05 ;
156500130313
156600130313       // Per immissione controllo il traino
156700130313 1     if imm_d05=*on   ;
156800130313       Indx=%scan('?':d05trn)  ;
156900130313 2     if indx>0   ;
157000130313        exsr ricercaTraino  ;
157100130313
157200130313 3     if d00trn>0  ;
157300130313       d05trn = %editc(d00trn:'X')  ;
157400130422
157500130422        chain  (d00trn:d00dde) tntlt01l  ;
157600130422 4      if %found(tntlt01l)  ;
157700130422        d05tfp    = tlttfp              ;
157800130422        d05tfa    = tlttfa              ;
157900130422        exsr DecoTRNimm ;
158000130422 x4    else  ;
158100130422       clear d05trn  ;
158200130422  4     endif  ;
158300130313 x3    else  ;
158400130313       clear d05trn  ;
158500130313  3    endif  ;
158600130313
158700130514       ricint='S'  ;
158800130314       leavesr     ;
158900130313  2    endif    ;
159000130313  1    endif   ;
159100130313
159200130313       // Per copia o spostamento  vedo se attivata la ricerca traino
159300130313       Indx=%scan('?':d05trn_nw)  ;
159400130419  0    if indx>0   ;
159500130313        exsr ricercaTraino  ;
159600130313
159700130419 1     if d00trn>0  ;
159800130313        d05trn_nw = %editc(d00trn:'X')  ;
159900130419
160000130419        chain  (d00trn:d00dde) tntlt01l  ;
160100130419 2      if %found(tntlt01l)  ;
160200130422        d05tfpNW  = tlttfp              ;
160300130422        d05tfaNW  = tlttfa              ;
160400130422        exsr DecoTRN_nw ;
160500130422 x2    else  ;
160600130422        clear d05trn_nw  ;
160700130422 2      endif  ;
160800130313
160900130419 x1    else  ;
161000130313        clear d05trn_nw  ;
161100130419 1     endif  ;
161200130313
161300130514       ricint='S'  ;
161400130419       leavesr  ;
161500130419 0     endif    ;
161600130419
161700130419       // Immissione     Traino --> controllo
161800130502 1     // if imm_d05=*on   ;
161900130419       alfaTRN=d05trn  ;
162000130419       exsr  CtrTRN  ;
162100130419 2      if Errgenerico=*on   ;
162200130419         Errtrn_imm=*on   ;
162300130419         leavesr  ;
162400130422 x2      else  ;
162500130422        d05tfp    = tlttfp              ;
162600130422        d05tfa    = tlttfa              ;
162700130422        exsr DecoTRNimm ;
162800130419 2      endif    ;
162900130502 1     // endif    ;
163000130419
163100130419       // Copia o sposta traino --> controllo
163200130419 1     if Copiad05=*on or spostad05=*on    ;
163300130419       alfatrn=d05trn_nw  ;
163400130419       exsr  CtrTRN  ;
163500130419 2      if Errgenerico=*on   ;
163600130419         Errtrn_nw =*on   ;
163700130419         leavesr  ;
163800130422 x2      else  ;
163900130422        d05tfpNW  = tlttfp              ;
164000130422        d05tfaNW  = tlttfa              ;
164100130422        exsr DecoTRN_nw ;
164200130419 2      endif    ;
164300130419 1     endif    ;
164400130422
164500130509       // Trazionista
164600130509       //  Ricerca
164700130509       Indx=%scan('?':d05pdr)  ;
164800130509 2     if indx>0   ;
164900130509        clear trul16ds  ;
165000130509        pa3tip='T ' ;
165100130509        pa3rsc=d05dpdr  ;
165200130509        kpjbu= trul16ds  ;
165300130509        callp trul16r   (kpjba)  ;
165400130509        trul16ds=kpjbu  ;
165500130509        d05pdr=%editc(pa3pdr:'X')   ;
165600130509        d05dpdr=pa3rsc  ;
165700130509
165800130510           ricint=*on  ;
165900130509           errpdr      = *on;
166000130509           leavesr  ;
166100130509
166200130509       endif  ;
166300130509
166400130422       if d05pdr  = *zeros    or d05pdr=*blanks  ;
166500130422           errpdr      = *on;
166600130422           errMessage  = *on;
166700130422           errGenerico = *on;
166800130422           V1cmsg = Msg(10);
166900130422           leavesr  ;
167000130422           endif  ;
167100130422
167200130422       if  d05pdr<= *ZEROS   ;
167300130422           errpdr      = *on;
167400130422           errMessage  = *on;
167500130422           errGenerico = *on;
167600130422           V1cmsg = Msg(10);
167700130422           leavesr  ;
167800130422       ELSE  ;
167900130422
168000130422       kpdr =%int(d05pdr)  ;
168100130422         chain (ktltip:kpdr)  tntlz01l  ;
168200130422         if %found(tntlz01l ) ;
168300130422           D05dpdr=tlzrsc  ;
168400130422         else  ;
168500130422           errpdr      = *on;
168600130422           errMessage  = *on;
168700130422           errGenerico = *on;
168800130422           V1cmsg = Msg(10);
168900130422           leavesr  ;
169000130422         endif  ;
169100130422       endif  ;
169200130502
169300130502       // Il trazionista deve essere previsto dal traino
169400130502       setll (kpdr:tlttrn:tltdde) titll02l   ;
169500130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
169600130502       dow not %eof(titll02l)  ;
169700130502       if tllatb=' '  ;
169800130502       leave    ;
169900130502       else  ;
170000130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
170100130502       endif  ;
170200130502       enddo  ;
170300130502
170400130502       if %eof(titll02l)  ;
170500130510       // non segnalo errore per traino locale
170600130510       if h02ttr<>'L'  ;
170700130502           errpdr      = *on;
170800130502           errMessage  = *on;
170900130502           errGenerico = *on;
171000130502           V1cmsg = Msg(18);
171100130502           leavesr  ;
171200130502       endif  ;
171300130510       endif  ;
171400130422
171500130508       // Conducente --> vedo se accreditato escludo H L D
171600130422 1     if d05dcn<>*blanks   ;
171700130422
171800130422       // controllo se c'e' ricerca
171900130422       Indx=%scan('?':d05dcn)  ;
172000130422 2     if indx>0   ;
172100130422         clear tntl83ds  ;
172200130422         i83soc=soctraini ;
172300130422         i83ksc=kpdr      ;
172400130423         i83mod='1'  ;
172500130423         i83drf=%editc(w02dtn:'X')  ;
172600130422         kpjbu=tntl83ds  ;
172700130422         callp TNTL83R  (kpjba)  ;
172800130422         tntl83ds =kpjbu  ;
172900130422 3       if o83_aut<>*blanks  ;
173000130422          d05dcn= o83_aut  ;
173100130423          else  ;
173200130423          clear d05dcn ;
173300130422 3       endif   ;
173400130423
173500130422         ricint=*on  ;
173600130423         errdcn=*on  ;
173700130422       leavesr  ;
173800130422 2     endif   ;
173900130422
174000130508       if tltttr <>'H' and tltttr<>'L' and tltttr<>'D'  ;
174100130508
174200130422       chain (soctraini:kpdr:d05dcn)  aiata01l  ;
174300130422       if not %found(aiata01l) or ataatb<>' '  ;
174400130422           errdcn      = *on;
174500130422           errMessage  = *on;
174600130422           errGenerico = *on;
174700130422           V1cmsg = Msg(11);
174800130422           leavesr  ;
174900130422       endif  ;
175000130422
175100130508       // se alla data decorrenza è scaduto il rapporto  --> errore
175200130422       if atadtfir>0 and w02dtn > atadtfir  ;
175300130422           errdcn      = *on;
175400130422           errMessage  = *on;
175500130422           errGenerico = *on;
175600130422           V1cmsg = Msg(12);
175700130422           leavesr  ;
175800130422       endif  ;
175900130508       endif  ;
176000130508
176100130422       endif  ;
176200130422
176300130422       // Targa Motrice
176400130422       if d05trm<>*blanks  ;
176500130423
176600130423       // controllo se c'e' ricerca
176700130423       Indx=%scan('?':d05trm)  ;
176800130423 2     if indx>0   ;
176900130423         clear tntl84ds  ;
177000130423         i84soc=soctraini ;
177100130423         i84ksc=kpdr      ;
177200130423         i84mod='1'  ;
177300130423         i84drf=%editc(w02dtn:'X')  ;
177400130423         kpjbu=tntl84ds  ;
177500130423         callp TNTL84R  (kpjba)  ;
177600130423         tntl84ds =kpjbu  ;
177700130423 3       if o84_TRM<>*blanks  ;
177800130423          d05trm= o84_TRM  ;
177900130423          else  ;
178000130423          clear d05trm ;
178100130423 3       endif   ;
178200130423
178300130423         ricint=*on  ;
178400130423         errtrm=*on  ;
178500130423       leavesr  ;
178600130423 2     endif   ;
178700130423
178800130508       if tltttr <>'H' and tltttr<>'L' and tltttr<>'D'  ;
178900130508
179000130422       chain (soctraini:kpdr:d05trm)  aiatm01l  ;
179100130422       if not %found(aiatm01l) or atmatb<>' '  ;
179200130422           errtrm      = *on;
179300130422           errMessage  = *on;
179400130422           errGenerico = *on;
179500130422           V1cmsg = Msg(13);
179600130422           leavesr  ;
179700130422       endif  ;
179800130422
179900130422       // se alla data decorrenza è scaduto il rapporto  --> errore
180000130422       if atmdtfir>0 and w02dtn > atmdtfir  ;
180100130422           errtrm      = *on;
180200130422           errMessage  = *on;
180300130422           errGenerico = *on;
180400130422           V1cmsg = Msg(13);
180500130422           leavesr  ;
180600130422       endif  ;
180700130508       endif  ;
180800130508
180900130422       endif  ;
181000130422
181100130422       // Giorni partenza
181200130514       if d05gg1<>' '  and d05gg1<>'S'  ;
181300130422           errgg1      = *on;
181400130313           errMessage  = *on;
181500130313           errGenerico = *on;
181600130313           V1cmsg = Msg(03);
181700130514           %subst(v1cmsg:53:10)='LUNEDÌ    ' ;
181800130313           leavesr;
181900130313       endif  ;
182000130503       // controllo se giorno previsto dal traino
182100130514       if d05gg1='S' and %subst(tltgse:1:1)=' '  ;
182200130503           errgg1      = *on;
182300130503           errMessage  = *on;
182400130503           errGenerico = *on;
182500130503           V1cmsg = Msg(19);
182600130503       endif  ;
182700130503
182800130514       if d05gg2<>' '  and d05gg2<>'S'  ;
182900130313           errgg2      = *on;
183000130313           errMessage  = *on;
183100130313           errGenerico = *on;
183200130313           V1cmsg = Msg(03);
183300130514           %subst(v1cmsg:53:10)='MARTEDÌ   ' ;
183400130313           leavesr;
183500130313       endif  ;
183600130503       // controllo se giorno previsto dal traino
183700130514       if d05gg2='S' and %subst(tltgse:2:1)=' '  ;
183800130503           errgg2      = *on;
183900130503           errMessage  = *on;
184000130503           errGenerico = *on;
184100130503           V1cmsg = Msg(19);
184200130503       endif  ;
184300130514       if d05gg3<>' '  and d05gg3<>'S'  ;
184400130313           errgg3      = *on;
184500130313           errMessage  = *on;
184600130313           errGenerico = *on;
184700130313           V1cmsg = Msg(03);
184800130514           %subst(v1cmsg:53:10)='MERCOLEDÌ ' ;
184900130313           leavesr;
185000130313       endif  ;
185100130503       // controllo se giorno previsto dal traino
185200130514       if d05gg3='S' and %subst(tltgse:3:1)=' '  ;
185300130503           errgg3      = *on;
185400130503           errMessage  = *on;
185500130503           errGenerico = *on;
185600130503           V1cmsg = Msg(19);
185700130503       endif  ;
185800130514       if d05gg4<>' '  and d05gg4<>'S'  ;
185900130313           errgg4      = *on;
186000130313           errMessage  = *on;
186100130313           errGenerico = *on;
186200130313           V1cmsg = Msg(03);
186300130514           %subst(v1cmsg:53:10)='GIOVEDÌ   ' ;
186400130313           leavesr;
186500130313       endif  ;
186600130503       // controllo se giorno previsto dal traino
186700130514       if d05gg4='S' and %subst(tltgse:4:1)=' '  ;
186800130503           errgg4      = *on;
186900130503           errMessage  = *on;
187000130503           errGenerico = *on;
187100130503           V1cmsg = Msg(19);
187200130503       endif  ;
187300130514       if d05gg5<>' '  and d05gg5<>'S'  ;
187400130313           errgg5      = *on;
187500130313           errMessage  = *on;
187600130313           errGenerico = *on;
187700130313           V1cmsg = Msg(03);
187800130514           %subst(v1cmsg:53:10)='VENERDÌ   ' ;
187900130313           leavesr;
188000130313       endif  ;
188100130514       if d05gg5='S' and %subst(tltgse:5:1)=' '  ;
188200130503           errgg5      = *on;
188300130503           errMessage  = *on;
188400130503           errGenerico = *on;
188500130503           V1cmsg = Msg(19);
188600130503       endif  ;
188700130514       if d05gg6<>' '  and d05gg6<>'S'  ;
188800130313           errgg6      = *on;
188900130313           errMessage  = *on;
189000130313           errGenerico = *on;
189100130313           V1cmsg = Msg(03);
189200130514           %subst(v1cmsg:53:10)='SABATO    ' ;
189300130313           leavesr;
189400130313       endif  ;
189500130514       if d05gg6='S' and %subst(tltgse:6:1)=' '  ;
189600130503           errgg6      = *on;
189700130503           errMessage  = *on;
189800130503           errGenerico = *on;
189900130503           V1cmsg = Msg(19);
190000130503       endif  ;
190100130514       if d05gg7<>' '  and d05gg7<>'S'  ;
190200130313           errgg7      = *on;
190300130313           errMessage  = *on;
190400130313           errGenerico = *on;
190500130313           V1cmsg = Msg(03);
190600130514           %subst(v1cmsg:53:10)='DOMENICA  ' ;
190700130313           leavesr;
190800130313       endif  ;
190900130514       if d05gg7='S' and %subst(tltgse:7:1)=' '  ;
191000130503           errgg7      = *on;
191100130503           errMessage  = *on;
191200130503           errGenerico = *on;
191300130503           V1cmsg = Msg(19);
191400130503       endif  ;
191500130129
191600130313       IF  D05GG1= ' ' and D05GG2= ' '   and D05GG3= ' '
191700130313          and D05GG4= ' '  and D05GG5= ' ' and D05GG6= ' '
191800130422          and D05GG7= ' '   ;
191900130423           errgg1      = *on;
192000130313           errMessage  = *on;
192100130313           errGenerico = *on;
192200130313           V1cmsg = Msg(04);
192300130313           leavesr;
192400130313       endif  ;
192500130424
192600130502       // controllo da fare solo se non annullo
192700130502       if   dsp_aid <> c_F16;
192800130502
192900130424       // Per ogni giorno inserito verifico se c'e' un'altra proposta
193000130424       // se sposto o copio uso il traino di destino
193100130424       if d05trn_nw>*zeros   ;
193200130424        ktrn=%int(d05trn_nw)  ;
193300130424       else  ;
193400130424        ktrn=%int(d05trn)  ;
193500130424       endif  ;
193600130424
193700130509       setll   (v01fgs:ktrn)     fnfgt01l  ;
193800130509       reade   (v01fgs:ktrn)     fnfgt01l  ;
193900130424
194000130424       dow  not %eof(fnfgt01l)  ;
194100130424       if fgtnrr <>  h02nrrp ;
194200130514       if d05gg1<>' '  and %subst(fgtgse:1:1)<>' '
194300130424        and wforzgg1=' '  ;
194400130424           errgg1      = *on;
194500130424           errMessage  = *on;
194600130424           errGenerico = *on;
194700130424           V1cmsg = Msg(14);
194800130424           %subst(v1cmsg:39:10)='  LUNEDÌ  ' ;
194900130424           wforzgg1='S'  ;
195000130424           leavesr;
195100130424       endif  ;
195200130514       if d05gg2<>' '  and %subst(fgtgse:2:1)<>' '
195300130424        and wforzgg2=' '  ;
195400130424           errgg2      = *on;
195500130424           errMessage  = *on;
195600130424           errGenerico = *on;
195700130424           V1cmsg = Msg(14);
195800130424           %subst(v1cmsg:39:10)='  MARTEDÌ ' ;
195900130424           wforzgg2='S'  ;
196000130424           leavesr;
196100130424       endif  ;
196200130514       if d05gg3<>' '  and  %subst(fgtgse:3:1)<>' '
196300130424        and wforzgg3=' '  ;
196400130424           errgg3      = *on;
196500130424           errMessage  = *on;
196600130424           errGenerico = *on;
196700130424           V1cmsg = Msg(14);
196800130424           %subst(v1cmsg:39:10)='MERCOLEDÌ ';
196900130424           wforzgg3='S'  ;
197000130424           leavesr;
197100130424       endif  ;
197200130514       if d05gg4<>' '  and %subst(fgtgse:4:1)<>' '
197300130424        and wforzgg4=' '  ;
197400130424           errgg4      = *on;
197500130424           errMessage  = *on;
197600130424           errGenerico = *on;
197700130424           V1cmsg = Msg(14);
197800130424           %subst(v1cmsg:39:10)=' GIOVEDÌ  ' ;
197900130424           wforzgg4='S'  ;
198000130424           leavesr;
198100130424       endif  ;
198200130514       if d05gg5<>' '  and %subst(fgtgse:5:1)<>' '
198300130424        and wforzgg5=' '  ;
198400130424           errgg5      = *on;
198500130424           errMessage  = *on;
198600130424           errGenerico = *on;
198700130424           V1cmsg = Msg(14);
198800130424           %subst(v1cmsg:39:10)=' VENERDÌ  ' ;
198900130424           wforzgg5='S'  ;
199000130424           leavesr;
199100130424       endif  ;
199200130514       if d05gg6<>' '  and %subst(fgtgse:6:1)<>' '
199300130424        and wforzgg6=' '  ;
199400130424           errgg6      = *on;
199500130424           errMessage  = *on;
199600130424           errGenerico = *on;
199700130424           V1cmsg = Msg(14);
199800130424           %subst(v1cmsg:39:10)='  SABATO  ' ;
199900130424           wforzgg6='S'  ;
200000130424           leavesr;
200100130424       endif  ;
200200130514       if d05gg7<>' '  and %subst(fgtgse:7:1)<>' '
200300130424        and wforzgg7=' '  ;
200400130424           errgg7      = *on;
200500130424           errMessage  = *on;
200600130424           errGenerico = *on;
200700130424           V1cmsg = Msg(14);
200800130424           %subst(v1cmsg:39:10)=' DOMENICA ' ;
200900130424           wforzgg7='S'  ;
201000130424           leavesr;
201100130424       endif  ;
201200130424       endif  ;
201300130424
201400130509       reade   (v01fgs:ktrn)     fnfgt01l  ;
201500130424       enddo  ;
201600130424
201700130426       // Controllo se il CONDUCENTE è già presente in un traino decorrente
201800130424       //  nello stesso giorno
201900130508
202000130508       if d05dcn<>*blanks  ;
202100130508
202200130424       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtdcn='''+
202300130424        d05dcn + ''''                                    ;
202400130424       EXEC sql  prepare s1 from : wrkgetlista ;
202500130424       EXEC sql  declare a1 cursor for s1      ;
202600130424       EXEC sql  open    a1                    ;
202700130424
202800130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
202900130424  1    dow sqlcod=0    ;
203000130426       wtipo='C'  ;
203100130426       Exsr ErrDOPP  ;
203200130426       if v1cmsg<>*blanks  ;
203300130426        leave  ;
203400130426       endif  ;
203500130426
203600130424
203700130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
203800130424       enddo           ;
203900130424
204000130424       EXEC sql  CLOSE   a1                    ;
204100130426
204200130426       if v1cmsg<>*blanks  ;
204300130426        leavesr;
204400130426       endif  ;
204500130508
204600130508       endif  ;
204700130424
204800130426       // Controllo se la TARGA MOTRICE è già presente in un traino decorrente
204900130426       //  nello stesso giorno
205000130508       if d05trm<>*blanks  ;
205100130508
205200130426       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtTRM='''+
205300130426        d05trm + ''''                                    ;
205400130426       EXEC sql  prepare s2 from : wrkgetlista ;
205500130426       EXEC sql  declare a2 cursor for s2      ;
205600130426       EXEC sql  open    a2                    ;
205700130426
205800130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
205900130426  1    dow sqlcod=0    ;
206000130426       wtipo='M'  ;
206100130426       Exsr ErrDOPP  ;
206200130426       if v1cmsg<>*blanks  ;
206300130426        leave  ;
206400130426       endif  ;
206500130426
206600130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
206700130426       enddo           ;
206800130426
206900130426       EXEC sql  CLOSE   a2                    ;
207000130426
207100130426       if v1cmsg<>*blanks  ;
207200130426        leavesr;
207300130426       endif  ;
207400130502
207500130502       endif  ;
207600130508       endif  ;
207700130424
207800130129       ENDSR   ;
207900130426       //--------------------------------------------------------------
208000130508       //?Errore per conducente/targa già presente nel giorno
208100130426       //--------------------------------------------------------------
208200130426       Begsr ErrDOPP  ;
208300130426
208400130426           if wtipo='C'  ;
208500130426            wmsg = Msg(15);
208600130426           else  ;
208700130426            wmsg = Msg(16);
208800130426           endif ;
208900130426
209000130426 2     if sql_fgttrn<> ktrn  and sql_fgttrn<>%int(d05trn)  ;
209100130426
209200130514 3     if d05gg1<>' '  and %subst(sql_fgtgse:1:1)<>' ' ;
209300130426 3a     if  (wfor2gg1=' ' and wtipo='C') or (wforMgg1=' ' and wtipo='M') ;
209400130426        // Verifico che il traino sia decorrente
209500130426        ktrn2=sql_fgttrn  ;
209600130426        exsr decorrTRN  ;
209700130426 4      if wok='S'      ;
209800130426           errgg1      = *on;
209900130426           errMessage  = *on;
210000130426           errGenerico = *on;
210100130426           V1cmsg = wmsg   ;
210200130426           %subst(v1cmsg:36:10)='  LUNEDÌ  ' ;
210300130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
210400130426           if wtipo='C'  ;
210500130426            wfor2gg1='S'  ;
210600130426           else  ;
210700130426            wforMgg1='S'  ;
210800130426           endif  ;
210900130426           leavesr;
211000130426 4     endif  ;
211100130426 3a    endif  ;
211200130426 3     endif  ;
211300130514 3     if d05gg2<>' '  and %subst(sql_fgtgse:2:1)<>' ' ;
211400130426 3a     if  (wfor2gg2=' ' and wtipo='C') or (wforMgg2=' ' and wtipo='M') ;
211500130426        // Verifico che il traino sia decorrente
211600130426        ktrn2=sql_fgttrn  ;
211700130426        exsr decorrTRN  ;
211800130426 4      if wok='S'      ;
211900130426           errgg2      = *on;
212000130426           errMessage  = *on;
212100130426           errGenerico = *on;
212200130426           V1cmsg = wmsg   ;
212300130426           %subst(v1cmsg:36:10)=' MARTEDÌ  ' ;
212400130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
212500130426           if wtipo='C'  ;
212600130426            wfor2gg2='S'  ;
212700130426           else  ;
212800130426            wforMgg2='S'  ;
212900130426           endif  ;
213000130426           leavesr;
213100130426 4     endif  ;
213200130426 3a    endif  ;
213300130426 3     endif  ;
213400130514 3     if d05gg3<>' '  and %subst(sql_fgtgse:3:1)<>' ' ;
213500130426 3a     if  (wfor2gg3=' ' and wtipo='C') or (wforMgg3=' ' and wtipo='M') ;
213600130426        // Verifico che il traino sia decorrente
213700130426        ktrn2=sql_fgttrn  ;
213800130426        exsr decorrTRN  ;
213900130426 4      if wok='S'      ;
214000130426           errgg3      = *on;
214100130426           errMessage  = *on;
214200130426           errGenerico = *on;
214300130426           V1cmsg = wmsg   ;
214400130426           %subst(v1cmsg:36:10)=' MERCOLEDÌ' ;
214500130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
214600130426           if wtipo='C'  ;
214700130426            wfor2gg3='S'  ;
214800130426           else  ;
214900130426            wforMgg3='S'  ;
215000130426           endif  ;
215100130426           leavesr;
215200130426 4     endif  ;
215300130426 3a    endif  ;
215400130426 3     endif  ;
215500130514 3     if d05gg4<>' '  and %subst(sql_fgtgse:4:1)<>' '  ;
215600130426 3a     if  (wfor2gg4=' ' and wtipo='C') or (wforMgg4=' ' and wtipo='M') ;
215700130426        // Verifico che il traino sia decorrente
215800130426        ktrn2=sql_fgttrn  ;
215900130426        exsr decorrTRN  ;
216000130426 4      if wok='S'      ;
216100130426           errgg4      = *on;
216200130426           errMessage  = *on;
216300130426           errGenerico = *on;
216400130426           V1cmsg = wmsg   ;
216500130426           %subst(v1cmsg:36:10)=' GIOVEDÌ  ' ;
216600130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
216700130426           if wtipo='C'  ;
216800130426            wfor2gg4='S'  ;
216900130426           else  ;
217000130426            wforMgg4='S'  ;
217100130426           endif  ;
217200130426           leavesr;
217300130426 4     endif  ;
217400130426 3a    endif  ;
217500130426 3     endif  ;
217600130514 3     if d05gg5<>' '  and %subst(sql_fgtgse:5:1)<>' '  ;
217700130426 3a     if  (wfor2gg5=' ' and wtipo='C') or (wforMgg5=' ' and wtipo='M') ;
217800130426        // Verifico che il traino sia decorrente
217900130426        ktrn2=sql_fgttrn  ;
218000130426        exsr decorrTRN  ;
218100130426 4      if wok='S'      ;
218200130426           errgg5      = *on;
218300130426           errMessage  = *on;
218400130426           errGenerico = *on;
218500130426           V1cmsg = wmsg   ;
218600130426           %subst(v1cmsg:36:10)=' VENERDÌ  ' ;
218700130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
218800130426           if wtipo='C'  ;
218900130426            wfor2gg5='S'  ;
219000130426           else  ;
219100130426            wforMgg5='S'  ;
219200130426           endif  ;
219300130426           leavesr;
219400130426 4     endif  ;
219500130426 3a    endif  ;
219600130426 3     endif  ;
219700130514 3     if d05gg6<>' '  and %subst(sql_fgtgse:6:1)<>' '  ;
219800130426 3a     if  (wfor2gg6=' ' and wtipo='C') or (wforMgg6=' ' and wtipo='M') ;
219900130426        // Verifico che il traino sia decorrente
220000130426        ktrn2=sql_fgttrn  ;
220100130426        exsr decorrTRN  ;
220200130426 4      if wok='S'      ;
220300130426           errgg6      = *on;
220400130426           errMessage  = *on;
220500130426           errGenerico = *on;
220600130426           V1cmsg = wmsg   ;
220700130426           %subst(v1cmsg:36:10)=' SABATO   ' ;
220800130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
220900130426           if wtipo='C'  ;
221000130426            wfor2gg6='S'  ;
221100130426           else  ;
221200130426            wforMgg6='S'  ;
221300130426           endif  ;
221400130426           leavesr;
221500130426 4     endif  ;
221600130426 3a    endif  ;
221700130426 3     endif  ;
221800130514 3     if d05gg7<>' '  and %subst(sql_fgtgse:7:1)<>' ' ;
221900130426 3a     if  (wfor2gg7=' ' and wtipo='C') or (wforMgg7=' ' and wtipo='M') ;
222000130426        // Verifico che il traino sia decorrente
222100130426        ktrn2=sql_fgttrn  ;
222200130426        exsr decorrTRN  ;
222300130426 4      if wok='S'      ;
222400130426           errgg7      = *on;
222500130426           errMessage  = *on;
222600130426           errGenerico = *on;
222700130426           V1cmsg = wmsg   ;
222800130426           %subst(v1cmsg:36:10)=' DOMENICA ' ;
222900130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
223000130426           if wtipo='C'  ;
223100130426            wfor2gg7='S'  ;
223200130426           else  ;
223300130426            wforMgg7='S'  ;
223400130426           endif  ;
223500130426           leavesr;
223600130426 4     endif  ;
223700130426 3a    endif  ;
223800130426 3     endif  ;
223900130426
224000130426 2     endif  ;
224100130426
224200130426       ENDSR  ;
224300130424       //--------------------------------------------------------------
224400130424       //?controlla decorrenza di un traino
224500130424       //--------------------------------------------------------------
224600130424       BEGSR DecorrTRN            ;
224700130424       clear wok                  ;
224800130424       setgt  ktrn2   tntlt01l    ;
224900130424       readpe ktrn2   tntlt01l    ;
225000130424 2     dow not %eof(tntlt01l)  ;
225100130424 3     if tltatb=' '  and w02dtn>=tltdde and w02dtn<=tltdsc ;
225200130424        wok='S'  ;
225300130424        leave  ;
225400130424 3     endif  ;
225500130424
225600130424       readpe ktrn2   tntlt01l    ;
225700130424 2     enddo   ;
225800130424       ENDSR   ;
225900130424
226000130422       //--------------------------------------------------------------
226100130422       //?Decodifica fermate iniziale e finale traino
226200130422       //--------------------------------------------------------------
226300130422        BEGSR DEcoTRNimm ;
226400130422
226500130422 4      if d05tfp  >0   ;
226600130422         chain d05tfp   azorg01l  ;
226700130422 5       if %found(azorg01l)  ;
226800130422         d05dtfp  =orgdes  ;
226900130422         else  ;
227000130422         d05dtfp  =*all'?'  ;
227100130422 5       endif  ;
227200130422 4      endif  ;
227300130422
227400130422 4      if d05tfa  >0   ;
227500130422         chain d05tfa   azorg01l  ;
227600130422 5       if %found(azorg01l)  ;
227700130422         d05dtfa  =orgdes  ;
227800130422         else  ;
227900130422         d05dtfa  =*all'?'  ;
228000130422 5       endif  ;
228100130422 4      endif  ;
228200130422       //--------------------------------------------------------------
228300130422       ENDSR  ;
228400130422        BEgsr DecoTRN_nw  ;
228500130422         chain d05tfpNW azorg01l  ;
228600130422 3       if %found(azorg01l)  ;
228700130422         d05dtfpnw=orgdes  ;
228800130422         else  ;
228900130422         d05dtfpnw=*all'?'  ;
229000130422 3       endif  ;
229100130422
229200130422         chain d05tfanw azorg01l  ;
229300130422 3       if %found(azorg01l)  ;
229400130422         d05dtfanw=orgdes  ;
229500130422         else  ;
229600130422         d05dtfanw=*all'?'  ;
229700130422 3       endif  ;
229800130422        ENDSR   ;
229900130313       //--------------------------------------------------------------
230000130313       //?Richiama pgm di ricerca traino per il terminal
230100130313       //--------------------------------------------------------------
230200130313       BEGSR ricercaTraino  ;
230300130313       clear tntl00ds  ;
230400130313       d00sca='C'   ;
230500130319       d00dde=w02dtn  ;
230600130313       d00fil= v01fgs  ;
230700130313
230800130313       kpjbu=tntl00ds  ;
230900130313       tntl17r (kpjba)  ;
231000130313       tntl00ds=kpjbu ;
231100130313       ENDSR  ;
231200130129       //--------------------------------------------------------------
231300130430       //?Aggiornoa richiesta singola    D05
231400130129       //--------------------------------------------------------------
231500130129       BEGSR AggiorD05 ;
231600130220
231700130313       // IMMISSIONE
231800130313       if Imm_d05=*on    ;
231900130430        clear fnfgtfis   ;
232000130313        fgtpru=knmus  ;
232100130313        fgtdim=datcor  ;
232200130313        fgttrn=%int(d05trn)  ;
232300130313       endif  ;
232400130313
232500130313       // COPIA
232600130313       if copiad05=*on  ;
232700130430        clear fnfgtfis   ;
232800130313        fgtpru=knmus  ;
232900130313        fgtdim=datcor  ;
233000130313        fgttrn=%int(d05trn_nw)  ;
233100130313       endif  ;
233200130313
233300130509       fgttfp=v01fgs ;
233400130220       fgtpdr=%int(d05pdr) ;
233500130220       fgtdcn=d05dcn  ;
233600130220       fgttrm=d05trm;
233700130220       fgttrr=d05trr;
233800130220       fgtnot=d05not ;
233900130313       fgtgse=d05gg1+d05gg2+d05gg3+d05gg4+d05gg5+d05gg6+d05gg7  ;
234000130514             xi=1  ;
234100130514               for  xi = 1  to  %len( %trimr( fgtgse ) );
234200130514                 if  %subst( fgtgse : xi : 1 ) <>' ';
234300130514                   %subst(fgtgse:xi:1)=%editc(xi:'X') ;
234400130514                 endif;
234500130514               endfor;
234600130220
234700130313       if Imm_d05=*on  or Copiad05=*on ;
234800130220       write fnfgtfis  ;
234900130220       endif  ;
235000130220
235100130220       if   ManutD05=*on  ;
235200130220       update fnfgtfis  ;
235300130220       endif   ;
235400130313
235500130313       if   spostad05=*on  ;
235600130313        fgttrn=%int(d05trn_nw)  ;
235700130313       update fnfgtfis  ;
235800130313       endif   ;
235900130129
236000130129           $Inzs02=*on  ;
236100130129
236200130129       ENDSR  ;
236300130417
236400130417       //--------------------------------------------------------------
236500130429       //?elabora SFL per scelta proposte
236600130417       //--------------------------------------------------------------
236700130417           BEGSR ElaboraSFL   ;
236800130417
236900130417           clear s02sce  ;
237000130417           clear s02sce_prot ;
237100130418           clear unascelta ;
237200130417
237300130417 0         if wglobale=' '  ;
237400130417
237500130417           readc lst1s02;
237600130417 1         DOW  NOT  %eof(fnlst1D) and $Fine=*off ;
237700130417
237800130417 2         if    s02sce= '1'  ;
237900130417 3           if ilst1ges='R'  ;
238000130417              olst1TRN =%editc(s02trn :'X')   ;
238100130417              olst1nrrp=%editc(h02nrrp:'X')  ;
238200130417              olst1OPZIO='1'  ;
238300130417              $Fine = *on;
238400130417              kpjbu=fnlst1ds  ;
238500130417              leave    ;
238600130417
238700130417 x3          else  ;
238800130417
238900130417             exsr AperturaFGV  ;
239000130418             unascelta='1'  ;
239100130417
239200130418  4          if olst4msg<>*blanks  ;
239300130417              errScelta   = *on;
239400130417              errMessage  = *on;
239500130417              errGenerico = *on;
239600130417              V1cmsg = olst4msg  ;
239700130418  4          endif  ;
239800130418  3          endif  ;
239900130417              exsr errSFL2  ;
240000130417 2        endif  ;
240100130417
240200130417            readc  lst1s02   ;
240300130417 1     enddo  ;
240400130418
240500130418       // se non ho fatto nessuna scelta e sono nella apertura
240600130418       //  totale --> apro senza trazionista/proposta
240700130418 1        if Unascelta=' ' and ilst1aut='S'  ;
240800130418            clear h02nrrp  ;
240900130418            clear s02pdr  ;
241000130418             exsr AperturaFGV  ;
241100130418 1        endif  ;
241200130417
241300130417 0     else  ;
241400130417
241500130417       // Scelta globale --> leggo tutto il slf
241600130417       //  non ancora aperti
241700130417
241800130417       eval   s02nrr=1  ;
241900130417       chain   s02nrr lst1s02;
242000130418 1     dow  %found           ;
242100130417
242200130417 3           if ilst1ges='R'  ;
242300130417              olst1TRN =%editc(s02trn :'X')   ;
242400130417              olst1nrrp=%editc(h02nrrp:'X')  ;
242500130417              olst1OPZIO='1'  ;
242600130417              $Fine = *on;
242700130417              kpjbu=fnlst1ds  ;
242800130417              leave    ;
242900130417
243000130417 x3          else  ;
243100130417
243200130417             exsr AperturaFGV  ;
243300130417
243400130418  4          if olst4msg<>*blanks  ;
243500130417              errScelta   = *on;
243600130417              errMessage  = *on;
243700130417              errGenerico = *on;
243800130417              V1cmsg = olst4msg  ;
243900130418  4          endif  ;
244000130418  3          endif  ;
244100130417              exsr errSFL2  ;
244200130429
244300130417         s02nrr=s02nrr+1  ;
244400130417         chain   s02nrr lst1s02;
244500130417  1    enddo  ;
244600130417
244700130417 0     endif  ;
244800130417
244900130417       ENDSR  ;
245000130417
245100130417       //--------------------------------------------------------------
245200130417       //?Richiamo pgm per apertua fogli viaggio
245300130417       //--------------------------------------------------------------
245400130417       BEGSR AperturaFGV  ;
245500130417
245600130417              clear fnlst4ds  ;
245700130417              ilst4ges='A'  ;
245800130417              ilst4fgs =%editc(v01fgs:'X')    ;
245900130417              ilst4TRN =%editc(s02trn :'X')   ;
246000130418 1            if h02nrrp>0  ;
246100130417                ilst4nrrp=%editc(h02nrrp:'X')  ;
246200130418 x1           else  ;
246300130418                ilst4nrrp=*all'0'  ;
246400130417
246500130417                // Se non si tratta di richiamo automatico ma scelta
246600130429                //  a video la passo altrimenti vuoto per far scegliere
246700130417                //  dopo
246800130417                ilst4pdr=%editc(s02pdr:'X')   ;
246900130502              //   ilst4old='S'  ;
247000130418  1           endif  ;
247100130417              ilst4dfv=w02dtn  ;
247200130429
247300130429              // Se premuto F6 passo al chiamante
247400130429              if dsp_aid = c_F06;
247500130429               iolst4for='F6 '  ;
247600130429              endif  ;
247700130429
247800130417              kpjbu=fnlst4ds    ;
247900130417              callp  FNLST4R (kpjba)   ;
248000130417              fnlst4ds=kpjbu   ;
248100130417
248200130429             if iolst4for= *blanks ;
248300130429              clear s02sce   ;
248400130429             endif  ;
248500130417       ENDSR  ;
248600130417       //--------------------------------------------------------------
248700130417       //?Apertura automatica dei fogli viaggio traino oper traino
248800130417       //--------------------------------------------------------------
248900130417       BEGSR creaAUT      ;
249000130417       if s02nrr=1  ;
249100130418        exsr AperturaFGV ;
249200130417       else   ;
249300130417       // emetto SFL per la scelta
249400130418           C02rcd  = 1;
249500130418           C02csr  = 1;
249600130418           $InzS02  = *off;
249700130418           ctl02_prot=*on   ;
249800130418
249900130418           exsr Gess02  ;
250000130418
250100130418           // controllo cosa scelto: se nulla --> apro senza trazionista
250200130418           if   $Video = 'CA';
250300130418             exsr    ElaboraSFL   ;
250400130503           else  ;
250500130503            clear h02nrrp  ;
250600130503            clear s02pdr   ;
250700130503            exsr AperturaFGV ;
250800130418           endif  ;
250900130418       endif  ;
251000130418
251100130418           ctl02_prot=*off  ;
251200130418           exsr Pulizs02  ;
251300130417
251400130417       ENDSR  ;
251500130417       //--------------------------------------------------------------
251600130417       //?Scelta su singolo traino: proposta o trazionista
251700130417       //--------------------------------------------------------------
251800130417       BEGSR SceltasingTRN;
251900130417       clear s02sce   ;
252000130417       exsr errsfl2  ;
252100130417       ENDSR  ;
252200080206       //--------------------------------------------------------------
252300080206       //?Operazioni finali.
252400080206       //--------------------------------------------------------------
252500080206       BEGSR RoutEnd;
252600080410
252700130502       if $Video='CA' and s02sce='1' and ilst1ges='R' ;
252800130315         *inrt=*on  ;
252900130315         else  ;
253000080206         *inLR = *on;
253100130315         endif  ;
253200130315
253300080206         return;
253400080206
253500080206       ENDSR;
253600080206
253700080206      /end-free
253800130419     C*-----------------------------------------------------***********
253900130419     C* CONTROLLO NUMERO TRAINO                             * CTRTRN  *
254000130419     C*-----------------------------------------------------***********
254100130419     C     CTRTRN        BEGSR
254200130419     C*
254300130419     c* controllo se inserito un numero
254400130419     c                   testn                   alfaTRN              30
254500130419     c                   movel     alfaTRN       alfaUNO           1
254600130419    2c                   if        not *in30 or alfaUNO<'0'
254700130419     C                   MOVEL     MSG(8)        V1cMSG
254800130422     c                   eval      errMessage  = *on
254900130422     c                   eval      errGenerico = *on
255000130419     C                   GOTO      ENDTRN
255100130419    2C                   ENDIF
255200130419     c
255300130419     c                   movel     alfaTRN       ktrn              7 0
255400130419     c
255500130419     C     ktrn          SETLL     TNTLT01L
255600130422     C     ktrn          READE     TNTLT01L
255700130419     C*
255800130419    2c                   dow       not %eof(tntlt01l)
255900130422     c                   if        tltatb=' ' and
256000130422     c                             w02dtn>=tltdde and w02dtn<=tltdsc
256100130422     c                   leave
256200130422     c                   else
256300130422     C     ktrn          READE     TNTLT01L
256400130422     c                   endif
256500130422     c                   enddo
256600130422     c
256700130422     c* Errore : traino non trovat o
256800130422     c                   if        %eof(tntlt01l)
256900130422     C                   MOVEL     MSG(8)        V1cMSG
257000130422     c                   eval      errMessage  = *on
257100130422     c                   eval      errGenerico = *on
257200130422     C                   GOTO      ENDTRN
257300130422     c                   endif
257400130422     c
257500130422     C*   Errore: traino  "A-AFFLUENZA"
257600130419    4C     TLTTTR        IFEQ      'A'
257700130419     C                   MOVEL     MSG(8)        V1cMSG
257800130422     c                   eval      errMessage  = *on
257900130422     c                   eval      errGenerico = *on
258000130419     C                   GOTO      ENDTRN
258100130422     c                   endif
258200130422     c
258300130422     c* controllo se la filiale gestione è una fermata di carico
258400130422     c                   exsr      contr_ferm
258500130422     c
258600130422     c                   if        cartrn=' '
258700130422     C                   MOVEL     MSG(9)        V1cMSG
258800130422     c                   eval      errMessage  = *on
258900130422     c                   eval      errGenerico = *on
259000130422     C                   GOTO      ENDTRN
259100130422     c                   endif
259200130419     C     ENDTRN        ENDSR
259201130617      *-------------------------------------------------------------------------
259202130617      * ordinamento sfl in base alle richieste
259203130617      *-------------------------------------------------------------------------
259204130617     C     sr_ordina     BEGSR
259206130617      * IMPOSTO ENTRATA AD 'A'
259207130617     C                   EVAL      s02nrr = 0
259208130617      *
259209130617     C                   CLEAR                   QLGSCB
259210130617     C                   CLEAR                   QLGSCB00
259211130617
259214130617     c                   select
259215130617     c* ordinamento per n.traino
259216130617     c                   when      w_ord=1
259217130617     ** 1 campo chiave.
259218130617     C                   EVAL      QLGNBRK    = 1
259220130617     C                   EVAL      QLGSP      = 1
259221130617     C                   EVAL      QLGSS      = %SIZE(s02trn)
259222130617     C                   EVAL      QLGDT      = carattere
259223130617     C                   EVAL      QLGSO      = Ascendente
259224130617     C                   EVAL      QLGKL(1)   = QLGSKL
259231130617     c* ordinamento per data/ora partenza
259232130617     c                   when      w_ord=2
259233130617     ** 2 campi chiave.
259234130617     C                   EVAL      QLGNBRK    = 2
259235130617     C                   EVAL      QLGSP      = 1 + %size(s02trn)
259236130617     C                   EVAL      QLGSS      = %SIZE(s02ggp)
259237130617     C                   EVAL      QLGDT      = carattere
259238130617     C                   EVAL      QLGSO      = Ascendente
259239130617     C                   EVAL      QLGKL(1)   = QLGSKL
259240130617     ** ragione sociale a posizione 1, carattere, 24 byte,
259241130617     C                   EVAL      QLGSP      = 1 + %size(s02trn) +
259242130617     c                                          %size(s02ggp)
259243130617     C                   EVAL      QLGSS      = %SIZE(s02hmp)
259244130617     C                   EVAL      QLGDT      = carattere
259245130617     C                   EVAL      QLGSO      = Ascendente
259246130617     C                   EVAL      QLGKL(2)   = QLGSKL
259247130617     c                   endsl
259248130617
259249130617      * Load other sort parameters.
259250130617     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
259251130617     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
259252130617     C                   EVAL      QLGRT     = 8
259253130617     C                   EVAL      QLGOKL    = 80
259254130617     C                   EVAL      QLGLKE    = 16
259255130617     C                   EVAL      QLGLSS    = 290
259256130617
259257130617      * Initialize Sort I/O API fields.
259258130617     C                   EVAL      QLGRL00  = QLGRL
259259130617     C                   EVAL      QLGRC00  = 1
259260130617     C                   CLEAR                   QUSEI
259261130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
259262130617
259263130617      * First step - Initialize the sort routine.
259264130617     C                   CALL      'QLGSORT'
259265130617     C                   PARM                    QLGSCB
259266130617     C                   PARM                    NotUsed
259267130617     C                   PARM                    NotUsed
259268130617     C                   PARM                    SizeList
259269130617     C                   PARM                    ReturnSize
259270130617     C                   PARM                    QUSEC
259271130617
259272130617      * Next step - Write records to I/O routine.
259273130617     C                   EVAL      QLGRT00 = Put
259274130617
259275130617     C                   DO        RrnLast       s02nrr
259276130617
259277130617     C     s02nrr        CHAIN     lst1s02
259278130617     c                   if        not %found
259279130617     c                   leave
259280130617     c                   endif
259281130617     ** Solo le righe con Selected = 'Y' sono riordinate,
259282130617     ** quindi per fare un ordinamento di tutte le righe
259283130617     ** metto 'Y' sempre.
259284130617     C                   EVAL      Selected = 'Y'
259285130617
259286130617     C                   CLEAR                   QUSEI
259287130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
259288130617     C                   CALL      'QLGSRTIO'
259289130617     C                   PARM                    QLGSCB00
259290130617     C                   PARM                    SflRcd
259291130617     C                   PARM                    NotUsed
259292130617     C                   PARM                    SizeList
259293130617     C                   PARM                    NotUsed
259294130617     C                   PARM                    QUSEC
259295130617
259296130617     C                   ENDDO
259297130617
259298130617      * Next step - Signal end of input, clear subfile for reload.
259299130617     C                   EVAL      QLGRT00 = EndPut
259300130617     C                   CLEAR                   QUSEI
259301130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
259302130617
259303130617     C                   CALL      'QLGSRTIO'
259304130617     C                   PARM                    QLGSCB00
259305130617     C                   PARM                    SflRcd
259306130617     C                   PARM                    NotUsed
259307130617     C                   PARM                    SizeList
259308130617     C                   PARM                    NotUsed
259309130617     C                   PARM                    QUSEC
259310130617
259312130617     c                   eval      SflDsp_N= *on
259313130617     c                   eval      SflDspCtl_N = *on
259314130617     C                   WRITE     lst1c02
259315130617     c                   eval      SflDspCtl_N = *off
259316130617     c                   eval      SflEnd      = *off
259317130617     c                   write     lst1p02
259319130617
259320130617      * Final step - Write the records back to the subfile.
259321130617     C                   EVAL      QLGRT00 = Get
259322130617     C                   DO        RrnLast       s02nrr
259323130617     C                   CLEAR                   QUSEI
259324130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
259325130617     C                   CALL      'QLGSRTIO'
259326130617     C                   PARM                    QLGSCB00
259327130617     C                   PARM                    NotUsed
259328130617     C                   PARM                    SflRcd
259329130617     C                   PARM                    QLGRL00
259330130617     C                   PARM                    NotUsed
259331130617     C                   PARM                    QUSEC
259332130617      *
259333130617     c                   eval      notrn=h02notrn
259334130617     c                   if        h02imm='S'
259335130617     c                   eval      Sott_fgt=*off
259336130617     c                   else
259337130617     c                   eval      Sott_fgt=*on
259338130617     c                   endif
259339130617     c                   if        s02aper='Sb'
259340130617     c                   eval      s02aper_rev=*on
259341130617     c                   endif
259342130617     C                   WRITE     lst1s02
259343130617     C                   ENDDO
259344130617
259345130617      *
259346130617     C                   ENDSR
259347080206
259400080206       //--------------------------------------------------------------
259500080206       //?Schiere a tempo di compilazione.
259600080206       //--------------------------------------------------------------
259700080206
259800080410** - MSG ------------------------------------------------------------------ -*
259900130121Data errata
260000130220Proposta ancora da inserire: possibile solo opzione "I-inserimento"
260100130514Immettere una "S" per indicare che la partenza è di xxxxxxxxxx
260200130423Immettere  i giorni di partenza
260300130314Opzione non ammmessa
260400130315Se premuto F7 non effettuare scelte con l'opzione "1"                         6
260500130315Utente non abilitato: chiamare CED di sede !!                                 7
260600130412Numero traino inesistente o non decorrente alla data specificata              8
260700130422La filiale gestione non è una fermata di carico. Traino errato                9
260800130424Codice trazionista errato                                                     10
260900130424Conducente non accreditato per il trazionista indicato                        11
261000130424Il conducente  ha terminato il rapporto col trazionista                       12
261100130424Targa MOTRICE non accredita per il trazionista indicato                       13
261200130424Esiste già una proposta nel giorno di xxxxxxxxxx x questo traino:ENTER forza  14
261300130424Conducente già presente nel giorno xxxxxxxxxx per traino xxxxxxx:ENTER forza  15
261400130426TargaMOTRICE già presente nel gior.xxxxxxxxxx per traino xxxxxxx:ENTER forza  16
261500130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare17
261600130502Il trazionista non è previsto nel traino indicato                             18
261700130503Il giorno non è previsto dal traino                                           19
