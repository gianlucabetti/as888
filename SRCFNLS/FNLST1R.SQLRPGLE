000100080206      //--------------------------------------------------------------
000200130508      //?FNLST1R - Gestione anagrafica traini per fogli viaggio
000300080206      //--------------------------------------------------------------
000400080206
000500080206     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000050704
001100130220     fFNfgt00F  uf a e             disk    rename(fnfgt000:Fnfgtfis)
001200130220     fFNfgt01L  if   e           k disk
001300130220     F                                     INFDS(FGTD1)
001400130218     ftntlt01L  if   e           k disk
001500130614     ftntlo01L  if   e           k disk
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
003800130620     f                                     sfile(lst1S0W : S02nrw)
003900080206
004000080206      //---------------------------------------------------------------
004100080206      //?Definizione costanti.
004200080206      //---------------------------------------------------------------
004300130618     d ord_traino      c                   'F11=Ord per n.Traino '
004400130618     d ord_dtaorp      c                   'F11=Ord per data/ora part'
004500130617     d w_ntraino       C                   1
004600130617     d w_dtaorp        C                   2
004700130617     d W_ord           S              1  0
004800130617      *---------------------------------------------------------------------
004900130617      * specifiche per ordinamento forzato sfl
005000130617     d NotUsed         S             16A
005100130617     d ReturnSize      S              9B 0
005200130617     d SizeList        S              9B 0
005300130617     D RrnLast         S              5I 0
005400130617     d*---
005500130617     d MaxKey          C                   2
005600130617     d Ascendente      C                   1
005700130617     d Discendente     C                   2
005800130617     d Carattere       C                   6
005900130617     d Put             C                   1
006000130617     d EndPut          C                   2
006100130617     d Get             C                   3
006200130617     d Numerico        C                   8
006300080411      // - Numero di record in ciascuna videata di subfile
006400130122     d c_SflPag        c                   const(16)
006500080411     d W_SflPag        s              3  0 inz
006600080530     d totrig          s              5  0 inz
006700130122     d ricInt          s              1
006800080207
006900080207      // - Tasti funzionali a video
007000080207     d c_F01           c                   const(x'31')
007100080207     d c_F02           c                   const(x'32')
007200080207     d c_F03           c                   const(x'33')
007300080207     d c_F05           c                   const(x'35')
007400080207     d c_F06           c                   const(x'36')
007500080207     d c_F07           c                   const(x'37')
007600080207     d c_F08           c                   const(x'38')
007700080207     d c_F09           c                   const(x'39')
007800080207     d c_F10           c                   const(x'3A')
007900130617     d c_F11           c                   const(x'3B')
008000080207     d c_F12           c                   const(x'3C')
008100080207     d c_F13           c                   const(x'B1')
008200080207     d c_F14           c                   const(x'B2')
008300080207     d c_F15           c                   const(x'B3')
008400080207     d c_F16           c                   const(x'B4')
008500080207     d c_F17           c                   const(x'B5')
008600080207     d c_F18           c                   const(x'B6')
008700080207     d c_F19           c                   const(x'B7')
008800080207     d c_F20           c                   const(x'B8')
008900080207     d c_F21           c                   const(x'B9')
009000080207     d c_F22           c                   const(x'BA')
009100080207     d c_F23           c                   const(x'BB')
009200080207     d c_F24           c                   const(x'BC')
009300080207     d c_Enter         c                   const(x'F1')
009400080207     d c_RollDown      c                   const(x'F4')
009500080207     d c_RollUp        c                   const(x'F5')
009600080214
009700080214      // - Attributi di visualizzazione
009800080215      //  -  DspAtr() - Normale
009900080214     d C_dspatr_Norm   c                   const(x'20')
010000080215      //  -  DspAtr(RI)
010100080214     d C_dspatr_RI     c                   const(x'21')
010200080215      //  -  DspAtr(ND)
010300080214     d C_dspatr_ND     c                   const(x'27')
010400080215      //  -  DspAtr(BL) / Color(Red)
010500080214     d C_dspatr_BL     c                   const(x'28')
010600080206
010700080206      //---------------------------------------------------------------
010800080206      //?Definizione schiere.
010900080206      //---------------------------------------------------------------
011000080206
011100080206      // - Messaggi di errore
011200130503     d MSG             s             78    dim(19) ctdata perrcd(1)
011300080609
011400080206      //---------------------------------------------------------------
011500080206      //?Definizione aree dati.
011600080206      //---------------------------------------------------------------
011700080206
011800080206      // - Dati utente
011900080206     d §AzUte        e ds                  extname(AZUTE00F)
012000080206     d                                     dtaara
012100080206     d §DatiUte      e ds                  extname(dDatiUte)
012200080206     d                                     dtaara
012300080206
012400080206      //---------------------------------------------------------------
012500080206      //?Definizione strutture dati.
012600080206      //---------------------------------------------------------------
012700080206
012800080206      // - Status
012900080206     d Psds           sds
013000080206     d   SDSpgm          *proc
013100080206
013200080206      // - InfDS
013300080206     d InfDspF         ds
013400080207     d  dsp_aid              369    369a
013500080207     d  sfl_rrn              376    377i 0
013600080207     d  min_nrr              378    379i 0
013700080207     d  num_rcds             380    381i 0
013800080206
013900080206      // - Indicatori su DspF
014000080208     d IndDspF         ds
014100080206        // - Indicatori di gestione del subfile
014200130220     d  NoTRN                         1n   overlay(IndDspF : 02)
014300130220     d  ProtD05                       1n   overlay(IndDspF : 03)
014400130130     d  InterD05                      1n   overlay(IndDspF : 04)
014500130313     d  CopiaD05                      1n   overlay(IndDspF : 05)
014600130220     d  spostad05                     1n   overlay(IndDspF : 06)
014700130220     d  ManutD05                      1n   overlay(IndDspF : 07)
014800130220     d  Imm_d05                       1n   overlay(IndDspF : 08)
014900130221     d  Sott_fgt                      1n   overlay(IndDspF : 09)
015000130314     d  Interrogaz                    1n   overlay(IndDspF : 10)
015100130314     d  Ricerca_PRP                   1n   overlay(IndDspF : 11)
015200130412     d  S02aper_REV                   1n   overlay(IndDspF : 12)
015300130417     d  S02sce_prot                   1n   overlay(IndDspF : 13)
015400130418     d  CTL02_prot                    1n   overlay(IndDspF : 14)
015500130314     d
015600080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
015700080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
015800080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
015900080206     d  SflEnd                        1n   overlay(IndDspF : 33)
016000130218     d  SflEnd3                       1n   overlay(IndDspF : 34)
016100130620        // sfl di work per gestione F15
016200130620     d  SflDspw_N                     1n   overlay(IndDspF : 38)
016300130620     d  SflDspCtlw_N                  1n   overlay(IndDspF : 39)
016400080206        // - Indicatori di errore
016500080206     d  errMessage                    1n   overlay(IndDspF : 28)
016600130313     d  Errgg1                        1n   overlay(IndDspF : 41)
016700130313     d  Errgg2                        1n   overlay(IndDspF : 42)
016800130313     d  Errgg3                        1n   overlay(IndDspF : 43)
016900130313     d  Errgg4                        1n   overlay(IndDspF : 44)
017000130313     d  Errgg5                        1n   overlay(IndDspF : 45)
017100130313     d  Errgg6                        1n   overlay(IndDspF : 46)
017200130313     d  Errgg7                        1n   overlay(IndDspF : 47)
017300130313     d  ErrScelta                     1n   overlay(IndDspF : 48)
017400130315     d  ErrDTN                        1n   overlay(IndDspF : 50)
017500130412     d  ErrTRN                        1n   overlay(IndDspF : 51)
017600130419     d  ErrTRN_imm                    1n   overlay(IndDspF : 52)
017700130422     d  ErrTRN_NW                     1n   overlay(IndDspF : 53)
017800130422     d  Errpdr                        1n   overlay(IndDspF : 54)
017900130422     d  Errdcn                        1n   overlay(IndDspF : 55)
018000130422     d  Errtrm                        1n   overlay(IndDspF : 56)
018100080606     d  errGenerico                   1n   overlay(IndDspF : 99)
018200130220
018300130617     d SflRcd          DS
018400130617     d  s02trn
018500130617     d  s02ggp
018600130617     d  s02hmp
018700130617     d  h02imm
018800130617     d  h02dde
018900130617     d  h02notrn
019000130617     d  H02NRRP
019100130617     d  h02ttr
019200130617     d  s02tfp
019300130617     d  s02tfa
019400130617     d  s02dtfa
019500130617     d  s02aper
019600130617     d  s02dpdr
019700130617     d  s02dcn
019800130617     d  s02set
019900130617     d  s02pdr
020000130617     d  s02trm
020100130617     d  s02trr
020200130617     d  Selected                      1A
020300130220     D FGTD1           DS
020400130220     D  FGTNRR               397    400B 0
020500080206
020600080206      // - Parametri ricevuti
020700080206     d KPJBA         e ds
020800080206
020900080206      // - Reperimento dati utente
021000080206     d TIBS34ds      e ds
021100080206     d dUte01        e ds
021200080206
021300080206      // - Ricerca/Controllo tabelle
021400130313     d tntl00ds      e ds                  inz
021500130313     d TIBS02ds      e ds                  inz
021600130123     d trul33ds      e ds                  inz
021700130422     d ds3I          e ds                  inz
021800080206
021900080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
022000080206     d dLAT          e ds                  inz
022100130122     D TRUL31DS      E DS
022200130122     d POG                            3  0 DIM(250) overlay(o31pog)
022300130122     d
022400130122     d dstu          e ds                  inz
022500130618     d dppa          e ds                  inz
022600130314     d
022700130121     D WLBDAT          DS
022800130121     D  G02DAT                 1      8  0
022900130121     D  G02INV                 9     16  0
023000130121     D  G02ERR                17     17
023100130121     D  G02TGI                18     22  0
023200130313     d
023300130123     D                 DS
023400130123     D  kaaa                   1      4  0
023500130123     D  kmmm                   5      6  0
023600130123     D  kgg                    7      8  0
023700130123     D  kDATA                  1      8  0
023800130123     d
023900130123     D clnmat          DS
024000130123     D  MAT                    1     31    dim(31)
024100130123     D clnpom          DS
024200130123     D  pom                    1     31    dim(31)
024300130123     D                 DS
024400130314     d
024500130314     D FNLST1DS        DS
024600130314     d* ' ' --> gestione proposte
024700130315     d* 'R' --> ricerca  proposte
024800130315     d* 'A' --> ricerca proposte per apertura FGV
024900130314     d* 'I' --> Interrogazione proposte
025000130314     D  Ilst1GES               1      1
025100130314     D  Ilst1DAT               2      9
025200130314     d
025300130314     D* 'G' --> caricare solo le proposte traini del giorno della settimana
025400130314     D*         corrispondente alla data;
025500130314     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
025600130314     D  Ilst1TUT              10     10
025700130314     D  Ilst1fgs              11     13
025800130417     D  Ilst1aut              14     14
025900130315     d
026000130315     d* Campi di output
026100130315     d*  S=apertura dei traini non ancora aperti
026200130315     d*  1=opzione di scelta '1' da cercare in tutte le proposte a video
026300130315     d  Olst1OPZIO            21     21
026400130315     d* per opzione '1'
026500130315     d  Olst1TRN              22     28
026600130315     d* Num relativo record della proposta FNFGT
026700130315     d*  se = *blanks  non c'e'  proposta
026800130315     d  Olst1NRRP             21     33
026900130417     d* E=errore nessun trino caricato
027000130417     d  Olst1ERR              34     34
027100130315     d
027200130315     d
027300130318     D fnlst4ds        DS
027400130318     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
027500130318     D*          B - DA BORDERO'
027600130318     D*          A - apertura foglio
027700130318     D*          C - DA CHIUSURA FV SINGOLA
027800130318     D*          I - DA CHIUSURA DA IMP
027900130318     D*          N - DA INTERROGAZIONE  FV
028000130318     D*          A - apertura fogli viaggio
028100130319     D  ilst4DFV               3     10  0
028200130318     D  ilst4nfv              11     15
028300130318     D  IlsT4GES              16     16
028400130318     D* Se PARMSG pieno --> c'è stato ERRORE
028500130318     D  olst4msg              17     94
028600130429     D  iolsT4FOR             95     97
028700130429     D  IlsT4OLD              98     98
028800130318     D* PARF12 = 'S'    --> premuto F12
028900130318     D  olst4F12             102    102
029000130318     D  ilst4fgs             103    105
029100130318     D  ilst4TRN             106    112
029200130318     D  ilst4NRRP            113    125
029300130417     D  ilst4PDR             126    132
029400130422
029500130422     d* ricerca autisti di un trazionista
029600130422     D tntl83ds        DS
029700130422     d  i83soc                        3s 0
029800130422     d  i83ksc                        7s 0
029900130422     d* i83mod = 1 --> modalità scelta conducente
030000130423     d  i83mod                        1
030100130423     d* data di riferimento alla quale caricare per ora non usata
030200130423     d  i83Drf                        8
030300130422     d  o83_AUT                      35
030400130424     d  o83_TEL                      12
030500130423     d
030600130423     d* ricerca TARGHE  di un trazionista
030700130423     D tntl84ds        DS
030800130423     d  i84soc                        3s 0
030900130423     d  i84ksc                        7s 0
031000130423     d* i84mod = 1 --> modalità scelta conducente
031100130423     d  i84mod                        1
031200130423     d* data di riferimento alla quale caricare per ora non usata
031300130423     d  i84Drf                        8
031400130423     d  o84_TRM                      10
031500130509     d
031600130509      * Parametri per la ricerca trazionisti     - TRUL16R -
031700130509     D TRUL16DS        DS
031800130509     D* Tipologia T_=TRAZIONISTA
031900130509     D  PA3TIP                 1      2
032000130509     D* Ragione sociale trazionista
032100130509     D  PA3RSC                 3     37
032200130509     D* Sistema inform. trazionista
032300130509     D  PA3CSF                38     40
032400130509     D* Codice trazionista
032500130509     D  PA3PDR                41     47  0
032600130509     D* PA3FLG = "3" --> restituisce al pgm chiamante il messaggio che
032700130509     D*   non esistono records per la chiave alfabetica richiesta
032800130509     D  PA3FLG                48     48
032900130509     d
033000080206      //---------------------------------------------------------------
033100080206      //?Definizione variabili globali.
033200080206      //---------------------------------------------------------------
033300080206
033400080206      // - Flags booleani
033500111115     d $Contattato1    s               n   inz(*off)
033600111115     d $Contattato2    s               n   inz(*off)
033700111115     d $EndAtt         s               n   inz(*off)
033800080208     d $Fine           s               n   inz(*off)
033900080208     d $InzD01         s               n   inz(*on)
034000080208     d $InzS01         s               n   inz(*on)
034100080414     d $InzS02         s               n   inz(*on)
034200080603     d $InzS03         s               n   inz(*on)
034300130129     d $InzD05         s               n   inz(*on)
034400080206     d $ErrGrave       s               n   inz(*off)
034500080207     d $EoF            s               n   inz(*off)
034600080208     d $RecOK          s               n   inz(*off)
034700080206
034800080207      // - Campi associati al video
034900130130     d Savvideo        s              2    inz
035000130218     d $Video          s              2    inz('S2')
035100130123     d S03nrr          s              4  0 inz
035200080414     d S02nrr          s              4  0 inz
035300130620     d S02nrw          s              4  0 inz
035400080414     d wPag            s              4  0 inz
035500080414     d wRig            s              3  0 inz
035600080414
035700080604     d wAbi            s                   like(UTEaut)  inz
035800130121     d Indx            s              3  0 inz
035900080619     d Indx2           s              3  0 inz
036000130514     d xi              s              1  0 inz
036100130514     d xx              s              3  0 inz
036200080929     d yy              s              3  0 inz
036300080415     d ss              s              3  0 inz
036400080415     d ff              s              3  0 inz
036500130123     d Ktfp            s              3  0 inz
036600130123     d Ktfa            s              3  0 inz
036700130123     d Wfest           s              1
036800130315     d Almenouno       s              1
036900130315     d Wscelta         s              1
037000130417     d Wglobale        s              1
037100130418     d Unascelta       s              1
037200130430     d alfaFGS         s              3
037300130128     d x§tcod          s                   like(tblcod)
037400130128     d x§tkey          s                   like(tblkey)
037500130128     d x§tdes          s             30
037600130122     d
037700130314     d GIOggmmaa       s              6
037800130314     d GIOset          s              1
037900130315     d W02gioset       s              1
038000130314     d Datcor          s              8  0
038100130220     d w02DTN          s              8  0
038200130315     d savDTN          s              8  0
038300130412     d savTRN          s              7  0
038400130620     d savTRNw         s              7  0
038500130424     d sql_FGTTRN      s              7  0
038600130424     d Ktrn2           s              7  0
038700130422     d alfaTRN         s              7
038800130218     d carTRN          s              1
038900130424     d sql_FGTGSE      s              7
039000130412     d Wok             s              1
039100130412     d Carsfl          s              1
039200130412     d Savaper         s              1
039300130424     d Wforzgg1        s              1
039400130424     d Wforzgg2        s              1
039500130424     d Wforzgg3        s              1
039600130424     d Wforzgg4        s              1
039700130424     d Wforzgg5        s              1
039800130424     d Wforzgg6        s              1
039900130424     d Wforzgg7        s              1
040000130424     d Wfor2gg1        s              1
040100130424     d Wfor2gg2        s              1
040200130424     d Wfor2gg3        s              1
040300130424     d Wfor2gg4        s              1
040400130424     d Wfor2gg5        s              1
040500130424     d Wfor2gg6        s              1
040600130424     d Wfor2gg7        s              1
040700130426     d WforMgg1        s              1
040800130426     d WforMgg2        s              1
040900130426     d WforMgg3        s              1
041000130426     d WforMgg4        s              1
041100130426     d WforMgg5        s              1
041200130426     d WforMgg6        s              1
041300130426     d WforMgg7        s              1
041400130426     d Wtipo           s              1
041500130426     d Wmsg            s                   like(v1cmsg)
041600130422     d AlfaSOC         s              3
041700130422     d SocTraini       s              3  0
041800130422     d Kpdr            s              7  0
041900130502     D savnrrp5        S                   LIKE(h02nrrp)
042000130502     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
042100130424     d wrkgetlista     s           4096    varying
042200130218     d
042300080605     d Dataiso         s               d   datfmt(*iso)
042400080605     d Dataymd         s               d   datfmt(*ymd)
042500080605     d Datadmy         s               d   datfmt(*dmy)
042600130122     d Dataeur         s               d   datfmt(*eur)
042700081010     d DataSYS         s               d   inz(*sys)
042800130123     d Orasys          s               t   timfmt(*hms) inz(*sys)
042900130614     d wnfm            s                   like(tlrnfm)
043000081112     D
043100080206      //---------------------------------------------------------------
043200080206      //?Definizione procedure usate.
043300080206      //---------------------------------------------------------------
043400130617      /COPY QSYSINC/QRPGLESRC,QLGSORT
043500130617     D QLGKL                         16    DIM(MaxKey)
043600130617     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
043700130617     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
043800130617     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
043900130617     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
044000130617
044100130617      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
044200130617      /COPY QSYSINC/QRPGLESRC,QUSEC
044300080414      /copy gaitrasrc/srcprotopr,tibs02r
044400080414      /copy gaitrasrc/srcprotopr,tibs34r
044500130122      /copy gaitrasrc/srcprotopr,trul31r
044600130123      /copy gaitrasrc/srcprotopr,trul33r
044700130122      /copy gaitrasrc/srcprotopr,x§taber
044800130122      /copy gaitrasrc/srcprotopr,xsrda8
044900130314      /copy gaitrasrc/srcprotopr,xgiose1
045000130313      /copy gaitrasrc/srcprotopr,tntl17r
045100130509      /copy gaitrasrc/srcprotopr,trul16r
045200130318      /copy gaitrasrc/srcprotopr,fnlst4r
045300130422      /copy gaitrasrc/srcprotopr,tntl83r
045400130423      /copy gaitrasrc/srcprotopr,tntl84r
045500130122
045600080206      //---------------------------------------------------------------
045700080206      //?Riepilogo indicatori.
045800080206      //---------------------------------------------------------------
045900080207      // - Comuni
046000080207      // 28    : Emissione messaggio di errore a video
046100080207      // - C01/S01
046200080207      // 30    : Condiziona SFLDSP    (*not)
046300080207      // 31    : Condiziona SFLDSPCTL (*not)
046400080207      // 30+31 : Condiziona SFLCLR
046500080207      // 32    : Condiziona SFLNXTCHG in S01
046600080207      // 50    : Errore: Opzione errata
046700080207      // - D01
046800080207      // - Comuni
046900080207      // 99    : Generico di Errore
047000080206      //---------------------------------------------------------------
047100080206
047200080206      //---------------------------------------------------------------
047300080206      //?M A I N - L I N E
047400080206      //---------------------------------------------------------------
047500080206
047600080206     c     *Entry        plist
047700080206     c                   parm                    KPJBA
047800130314     c                   movel     kpjbu         fnlst1ds
047900080206
048000080206      /free
048100080206
048200080206       // Operazioni iniziali
048300080206       exsr RoutInz;
048400080206
048500080206       // Gestione video
048600080206       DOW $Fine = *off;
048700080206         select;
048800080530
048900080530         // SFL di scelta
049000080411           when $Video = 'S2';
049100080411             exsr GesS02;
049200130122
049300130417         // Sfl nascosto per scelta totale dei non apertu
049400130417           when $Video = 'A2';
049500130417            D02apfgv='S'   ;
049600130417            exsr InzS02;
049700130417
049800130417          // Posizionamento cursore
049900130417          if  Carsfl  <>'N'  ;
050000130417              $Video='CA' ;
050100130417           else  ;
050200130417              $Fine = *on;
050300130417              olst1err='E'   ;
050400130417              kpjbu=fnlst1ds  ;
050500130417              leave    ;
050600130417          endif  ;
050700130417
050800130417           // Elaborazione dei record scelti per apertura fogli
050900130417           when $Video = 'CA';
051000130417           exsr    ElaboraSFL   ;
051100130417
051200130319             if ilst1ges='A' and v1cmsg<>*blanks  ;
051300130319               $Video='S2'  ;
051400130319               else  ;
051500130319               $Fine = *on;
051600130319             endif   ;
051700130315
051800080206           other;
051900080206             $Fine = *on;
052000080206         endsl;
052100080206       ENDDO;
052200080206
052300080206       // Operazioni finali
052400130315         exsr RoutEnd;
052500080206
052600130122       //--------------------------------------------------------------
052700130122       //?Reperimento Dati del job (Utente/Operativi).
052800130122       //--------------------------------------------------------------
052900130122       BEGSR DatiJob;
053000130122
053100130122         in(E) §AzUte;
053200130122         if NOT %error;
053300130122           in(E) §DatiUte;
053400130122         endif;
053500130122         if %error or RSut = *blanks;
053600130122           clear TIBS34ds;
053700130122           tibs34r(tibs34ds);
053800130122           in §AzUte;
053900130122           in §DatiUte;
054000130122         endif;
054100130122
054200130122       ENDSR;
054300130122
054400080206       //--------------------------------------------------------------
054500080206       //?Operazioni iniziali.
054600080206       //--------------------------------------------------------------
054700130417       BEGSR RoutInz;
054800080206
054900130628       // Ordinamento sfl di default: per numero traino se non richiamato
055000130628       w_ord=w_ntraino;
055100130628       v1pf11=ord_dtaorp;
055200130617
055300130314       clear  Ricerca_PRP  ;
055400130502       $Fine =*off  ;
055500130314
055600130315 0     if olst1OPZIO= ' '  ;
055700130315
055800080206         // Impostazione campi "fissi"
055900080208         TBLkut = 1;
056000080208         TBLcod = '1L';
056100080206
056200080206         // Reperimento dati job
056300080206         exsr DatiJob;
056400080206
056500080206         // Verifica errori e autorità profilo
056600080206         clear  wAbi;
056700080206         clear  dLAT;
056800080206         select;
056900080206         // - Se ho errori nei dati utente esco dal pgm
057000080208           when  DUTerr = 'E';
057100080206             $Fine = *on;
057200080206         // - Se non c'è l'abilitazione:
057300080206         //   - se 1° livello, abilitazioni al terminal
057400080206         //   - se 2° livello, abilitazioni al punto operativo
057500080206         //   - se sede è impossibile ma se capita mando a fine il pgm
057600080208           when  UTEaut = *blank;
057700080206             select;
057800080208               when  DUTlpo = '1';
057900080206                 wAbi  = 'TP';
058000080208               when  DUTlpo = '2';
058100080206                 wAbi  = 'PO';
058200080208               when  DUTlpo = 'S';
058300080206                 $Fine = *on;
058400080206             endsl;
058500080206         // - Altrimenti si caricano le abilitazioni del profilo
058600080206           other;
058700080208             dUTE01 = UTEfaf;
058800080208             if  §UTEpot <> *blank;
058900080208               wAbi = §UTEpot;
059000080206             else;
059100080208               wAbi = UTEaut;
059200080206             endif;
059300080206         ENDSL;
059400080206
059500080206         // Controllo se ok l'abilitazione dell'utente
059600080206         reset TIBS02ds;
059700111115         T02Mod = 'C';
059800080208         T02sif = knsif;
059900080208         T02cod = 'LAT';
060000080208         T02ke1 = wAbi;
060100080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
060200080206
060300080208         if  T02err  = *blank;
060400080208           dLAT = T02uni;
060500080206         endif;
060600080206
060700080206         // Errore o utente non abilitato
060800080208         if  T02err <> *blanks   or
060900080208             §LATabi = 'S';
061000080206           $ErrGrave   = *on;
061100080206           errMessage  = *on;
061200080206           errGenerico = *on;
061300130220           V1cmsg = Msg(02);
061400080206           leavesr;
061500080206         endif;
061600080617
061700080617         clear trul31ds  ;
061800080617         I31abi = wabi    ;
061900080617         I31cdi = DUTdis  ;
062000080617         I31car = DUTare  ;
062100080617         I31cpo = DUTpou  ;
062200080617         TRUL31R   (KPJBA:trul31ds)   ;
062300080617
062400080617         if o31pog=*zeros   ;
062500080617           $ErrGrave   = *on;
062600080617           errMessage  = *on;
062700080617           errGenerico = *on;
062800130315          V1cmsg = Msg(07);
062900080617           leavesr;
063000080617         endif         ;
063100130430
063200130218       $inzs02=*on;
063300130218
063400130218       // Imposto come terminal in gestione simfel
063500130314       if ilst1fgs<=*zeros  ;
063600130314         v01fgs=simfel  ;
063700130314       else  ;
063800130314         v01fgs=%int(ilst1fgs)  ;
063900130314       endif ;
064000130314
064100130430       chain   (v01fgs)  azorg01l   ;
064200130218       if %found(azorg01l)   ;
064300130218        v01dfgs=orgdes  ;
064400130218       endif  ;
064500130218
064600130218       // Propongo data del giorno con selezione traini
064700130218       dataeur=datasys  ;
064800130218       d02dtn=%dec(dataeur)  ;
064900130218       w02dtn=%dec(datasys)  ;
065000130220       Datcor=%dec(datasys)  ;
065100130412       clear d02trn  ;
065200130412       clear savtrn  ;
065300130412       clear savaper ;
065400100520
065500130417       // Tipo gestione
065600130417
065700130314 1     if ilst1ges='I'     ;
065800130314       Interrogaz =*on  ;
065900130314       V01dric='INTERROGAZIONE'  ;
066000130314       endif  ;
066100130314
066200130315 1     if ilst1ges='R'   or ilst1ges='A' ;
066300130628
066400130628       // Ordinamento sfl di default: per data/ora partenza
066500130628       w_ord=w_dtaorp ;
066600130628       v1pf11=ord_traino;
066700130628
066800130628 2     if ilst1ges='R'   ;
066900130315        V01dric=' S C E L T A  '  ;
067000130628 2     endif  ;
067100130628 2     if ilst1ges='A'   ;
067200130315        V01dric='APERTURA FOGLI'  ;
067300130628 2     endif  ;
067400130618       // In apertura f.v.p determina l'ordinamento su tab PPA
067500130628 2     if ilst1ges='A'   ;
067600130618         reset TIBS02ds;
067700130618         T02Mod = 'C';
067800130618         T02sif = knsif;
067900130618         T02cod = 'PPA';
068000130618         T02ke1 = %editc(simfel:'X');
068100130618         TNTBE_RicercaControllo  (kpjba : tibs02ds);
068200130618
068300130618         if  T02err  = *blank;
068400130618           dppa = T02uni;
068500130619           if §ppaoaf='T';
068600130619              w_ord=w_ntraino;
068700130619             v1pf11=ord_dtaorp;
068800130618           endif;
068900130618         endif;
069000130628 2     endif;
069100130618
069200130315       Ricerca_prp=*on  ;
069300130315       Interrogaz =*on  ;
069400130314
069500130314 2       if ilst1dat>*zeros  ;
069600130314          dataiso=%date(%int(ilst1dat)) ;
069700130314         else  ;
069800130314          dataiso=%date(datcor)         ;
069900130314 2       endif  ;
070000130314          dataeur=dataiso  ;
070100130314          datadmy=dataiso  ;
070200130314          d02dtn=%dec(dataeur) ;
070300130314          w02dtn=%dec(dataiso)  ;
070400130314          GIOggmmaa=%editc(%dec(datadmy) :'X') ;
070500130314
070600130315          exsr  CercaGIORNO  ;
070700130315
070800130314 1      endif  ;
070900130417
071000130417        // Se proposta automatica senza far vedere videata,
071100130417        //  carico SFL ed elaboro in automatico
071200130417        if ilst1ges='A' and ilst1aut='S'   ;
071300130417        $Video='A2'  ;
071400130417        endif   ;
071500130422
071600130422        // Carico società trazinisti
071700130422        tblkey='1       ';
071800130422        clear alfasoc ;
071900130422        clear soctraini  ;
072000130422        chain  (1:'3I':tblkey ) tabel00f ;
072100130422        if %found(tabel00f)  ;
072200130422        ds3i=tbluni  ;
072300130422        alfasoc=%subst(§3ibst:7:3)  ;
072400130422        if alfasoc>*zeros  ;
072500130422        soctraini=%int(alfasoc)  ;
072600130422        endif  ;
072700130422        endif  ;
072800130315
072900130315 x0     else  ;
073000130315
073100130315        $Video='CA'  ;
073200130315        clear olst1TRN  ;
073300130315        clear olst1NRRP  ;
073400130315 0      endif  ;
073500130314
073600130122        ENDSR  ;
073700130315       //--------------------------------------------------------------
073800130315       //?Cerca giorno della settimana
073900130315       //--------------------------------------------------------------
074000130315          BEGSR  cercaGIORNO ;
074100130315          // cerco girno della settimana se non devo caricare tutti
074200130315 2        if ilst1tut='G'  ;
074300130315          xgiose1 (Gioggmmaa:GIOset)  ;
074400130315          // Salvo il giorno della settimana per cui devo caricare
074500130315          //  i traini
074600130315 3          if gioset>*zeros   ;
074700130315             w02gioset=GIOSET   ;
074800130315            else  ;
074900130315             clear w02gioset  ;
075000130315 3          endif  ;
075100130315 2        endif  ;
075200130315        ENDSR  ;
075300080411       //--------------------------------------------------------------
075400130218       //?Gestione videata S02   - sfl traini con o senza estensione
075500080411       //--------------------------------------------------------------
075600080411       BEGSR GesS02;
075700080411
075800130430         // se errore in prima videata lo emetto e poi esco
075900130430         if $errGrave=*on ;
076000130430         Carsfl='N'  ;
076100130430         ricint='S'  ;
076200130430         else    ;
076300080411         // Inizializzazione videata
076400080411         if  $InzS02 = *on;
076500080411            exsr InzS02;
076600080411             $InzS02  = *off;
076700080411         endif;
076800130430         endif;
076900080411
077000080411         // Posizionamento cursore
077100130412         if  Carsfl  <>'N'  ;
077200130412
077300130412         // Se non ci sono errori e ho indicato posizionamento
077400130412         if  errGenerico = *off and ricInt =' '
077500130412              and  savtrn<>d02trn  ;
077600130412          exsr posizTRN  ;
077700130412         endif  ;
077800130412
077900130123           sfldsp_n=*off;
078000080411           C02rcd = C02csr;
078100130412           if C02rcd=0  ;
078200130412            c02rcd=1  ;
078300130412           endif  ;
078400130412
078500080411         else;
078600080411
078700130123         // Se non è stato caricato nulla emetto sfl vuoto
078800130123           sfldsp_n=*on ;
078900080411         endif;
079000080411
079100080411         // Emissione Testata e Piede con tasti funzionali abilitati
079200130514         if  errGenerico = *off or ricInt <>' '  ;
079300130218           write  lst1T01;
079400130218           write  lst1P02;
079500130123           if sfldsp_n=*on  ;
079600130218           write  lst1d04;
079700130123           endif  ;
079800130123
079900080604           clear  ricInt   ;
080000080411         endif;
080100080411
080200080411         // Emissione videata
080300130218         exfmt  lst1C02;
080400080411
080500080411         reset errMessage;
080600080411         reset errGenerico;
080700130122         clear V1cmsg;
080800080523         clear errScelta ;
080900130315         clear errDTN    ;
081000130412         clear errTRN    ;
081100080411
081200080411         SELECT;
081300130430           WHEN  $ErrGrave=*on  ;
081400130430            $Fine = *on;
081500080411
081600130430         // - F3=Fine  12"=ritorno
081700130412           WHEN  dsp_aid = c_F03
081800130412              or dsp_aid = c_F12 ;
081900080411            $Fine = *on;
082000130617
082100130617         // - F11=Ordinamento
082200130617           WHEN  dsp_aid = c_F11;
082300130617           select;
082400130617           when w_ord=w_ntraino;
082500130617           w_ord=w_dtaorp;
082600130618           v1pf11=ord_traino;
082700130617           when w_ord=w_dtaorp;
082800130617           w_ord=w_ntraino;
082900130618           v1pf11=ord_dtaorp;
083000130617           endsl;
083100130617           exsr sr_ordina;
083200080411
083300130220         // - F10=Immissione (senza impostazione num traino)
083400130220           WHEN  dsp_aid = c_F10;
083500130130
083600130130           savVideo=$Video  ;
083700130220           $Video = 'D5';
083800130220           $inzD05=*on  ;
083900130314
084000130314           dow $Video='D5'  ;
084100130314            exsr Gesd05     ;
084200130314           enddo ;
084300130314           InterD05=*off ;
084400130314           Protd05 =*off ;
084500130220
084600080411         // Invio
084700080411           OTHER;
084800080523             exsr ContrS02  ;
084900080411             if  errGenerico = *on;
085000080411               leavesr;
085100080411             endif;
085200130315
085300130429         // Se premuto F15 - elaboro  tutto il sfl
085400130429           if    dsp_aid = c_F15;
085500130417           Wglobale='S'  ;
085600130417            $Video='CA'  ;
085700130315           else  ;
085800130315            if wscelta='1'  ;
085900130315             $Video='CA'  ;
086000130315            endif  ;
086100130315           endif  ;
086200080411
086300080411         ENDSL;
086400080411
086500080411       ENDSR;
086600080411       //--------------------------------------------------------------
086700080414       //?Inizializzazione sfl 2
086800080411       //--------------------------------------------------------------
086900080411       BEGSR InzS02;
087000080411
087100130418       exsr Pulizs02 ;
087200130620       exsr Pulizs0w ;
087300080411
087400130218       setll *loval   tntlt01l  ;
087500130218       read   tntlt01l  ;
087600130218
087700130418 1     dow not %eof(tntlt01l)  ;
087800130218
087900130218       // Escludo se annullato  o non decorrente alla data richiesta
088000130418 2     if tltatb=' '  ;
088100130418 3     if w02dtn>=tltdde  and  w02dtn<=tltdsc  ;
088200130218         exsr contr_ferm ;
088300130218
088400130418 4       if carTRN='S'  ;
088500130218          exsr EmisTRN  ;
088600130417
088700130417          // se devo fare una conferma automatica senza far vedere il slf
088800130417          //  se ho carico solo un record --> creo foglio viaggio
088900130417          //  se ne ho caricati più di 1  --> emetto sfl per scelta
089000130620 5         if ilst1ges='A' and ilst1aut='S' and s02nrr>0
089100130620                           and h02ttr<>'H'  and w_ord=w_ntraino;
089200130620            exsr CreaAUT  ;
089300130620 5         endif  ;
089400130418 4     endif  ;
089500130418 3     endif  ;
089600130418 2     endif  ;
089700130218
089800130218       read   tntlt01l  ;
089900130418 1     enddo  ;
090000130130
090100130617       clear rrnlast;
090200130617
090300130418 1       if S02nrr > *zero;
090400080411           C02rcd  = 1;
090500080411           C02csr  = 1;
090600130617           RrnLast = s02nrr;
090700080530
090800080411         else;
090900080411           clear C02rcd;
091000110831           clear C02csr;
091100130412           carSFL='N'  ;
091200130624           RrnLast = s02nrw;
091300130418 1       endif;
091400080411
091500130315       savdtn=w02dtn  ;
091600130412       savaper= d02apFGV;
091700130617       // vedo se ordinare il sfl
091800130620       if rrnlast>0;
091900130617       if w_ord<>w_ntraino or v1pf11=ord_traino ;
092000130620          if ilst1ges='A' and ilst1aut='S';
092100130620             exsr sr_ordinaw;
092200130620             exsr sr_elabs0w;
092300130620          else;
092400130620             exsr sr_ordina;
092500130620          endif;
092600130617       endif;
092700130620       endif;
092800080411       ENDSR;
092900130418       //--------------------------------------------------------------
093000130418       //?Pulizia sfl e campi sfl 2
093100130418       //--------------------------------------------------------------
093200130418       BEGSR Pulizs02  ;
093300130418
093400130418       // Pulizia subfile
093500130418         SflDsp_N    = *on;
093600130418         SflDspCtl_N = *on;
093700130418         write  lst1C02;
093800130418         SflDspCtl_N = *off;
093900130418         SflEnd      = *off;
094000130418
094100130418         clear W_SflPag;
094200130418         $EoF = *off;
094300130418         clear C02rcd;
094400130418         clear S02nrr;
094500130418         clear V1cmsg;
094600130418         clear carsfl  ;
094700130418 1       if ilst1ges=' '  ;
094800130418          clear V01dric  ;
094900130418 1       endif  ;
095000130418         errMessage  = *off;
095100130418         errGenerico = *off;
095200130418         errscelta=*off  ;
095300130418         NoTRN=*off  ;
095400130418         ENDSR  ;
095500130620       //--------------------------------------------------------------
095600130620       BEGSR Pulizs0w  ;
095700130620
095800130620       // Pulizia subfile
095900130620         SflDspw_N    = *on;
096000130620         SflDspCtlw_N = *on;
096100130620         write  lst1C0w;
096200130620         SflDspCtlw_N = *off;
096300130620
096400130620         clear S02nrw;
096500130620
096600130620         ENDSR  ;
096700130218       //--------------------------------------------------------------
096800130219       //?controllo se previsto il terminal come fermata
096900130218       //--------------------------------------------------------------
097000130218       BEGSR Contr_ferm  ;
097100130418
097200130218       clear cartrn  ;
097300130614       clear wnfm    ;
097400130220       NoTRN=*off  ;
097500130218       setll  (tlttrn:tltdde)  tntlr01l  ;
097600130218       reade  (tlttrn:tltdde)  tntlr01l  ;
097700130218
097800130218       dow not %eof(tntlr01l)  ;
097900130218       if tlratb=' '  and tlrcar='S' and tlrfil=v01fgs  ;
098000130218        cartrn='S'  ;
098100130614        wnfm=tlrnfm;
098200130218       leave  ;
098300130218       endif  ;
098400130218
098500130218       reade  (tlttrn:tltdde)  tntlr01l  ;
098600130218       enddo  ;
098700130218       ENDSR;
098800080411       //--------------------------------------------------------------
098900080411       //?Caricamento singola pagina S02
099000080411       //--------------------------------------------------------------
099100130218       BEGSR EmisTRN  ;
099200080411
099300080411         // - Caricamento dati nel record del subfile
099400080414         clear s02sce  ;
099500130412         clear s02aper ;
099600130412         s02aper_rev=*off  ;
099700130417         s02sce_prot=*off  ;
099800130412
099900130218         s02trn=tlttrn  ;
100000130508         h02ttr=tltttr  ;
100100130417         h02dde=tltdde  ;
100200130218         s02tfp=tlttfp  ;
100300130218         s02tfa=tlttfa  ;
100400130218          chain tlttfa azorg01l  ;
100500130219  1       if %found(azorg01l)  ;
100600130122           s02dtfa=orgdes  ;
100700130219  1       endif  ;
100800130614
100900130614         // giorno e ora di partenza da tntlo
101000130614
101100130614            clear s02ggp;
101200130614            clear s02hmp;
101300130614         if ilst1TUT='G';
101400130614         // se devo considerare solo il giorno della settimana
101500130614         // cerco col giorno e se non lo trovo cerco con " "
101600130614           chain (tlttrn:tltdde:wnfm:w02gioset) tntlo01l;
101700130614           if not %found(tntlo01l);
101800130614              chain (tlttrn:tltdde:wnfm:' ') tntlo01l;
101900130614           endif;
102000130614         else;
102100130614           chain (tlttrn:tltdde:wnfm) tntlo01l;
102200130614         endif;
102300130614         if %found (tntlo01l);
102400130614            s02ggp=tloggp;
102500130614            s02hmp=%editw(tlohhp:'0 :  ');
102600130614         endif;
102700130412
102800130412         // Se sono in apertura FGV visualizzo se già aperto
102900130412 1     if ilst1ges='A'   ;
103000130412        setll  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
103100130412        reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
103200130412 2      dow not %eof(fnfgv03l)  ;
103300130412 3       if   fgvatb=' '  ;
103400130412          s02aper='Ap'   ;
103500130429          //  s02aper_rev=*on  ;
103600130412          leave  ;
103700130412 3       endif  ;
103800130412       reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
103900130412 2     enddo  ;
104000130412
104100130412       // Se non è aperto verifico se si è SBINATO   ;
104200130412 2       if s02aper=*blanks  ;
104300130412           setll  (v01fgs:w02dtn:0000000) fnfgv03l  ;
104400130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
104500130412 3         dow not %eof(fnfgv03l)  ;
104600130412
104700130412 4          if   fgvatb=' '  ;
104800130412            // chain fnfv4 record "D"
104900130412            chain (FGVlnp:fgvnfv:'P':000:'D') fnfv401l ;
105000130412
105100130412 5          if %found(fnfv401l) and fv4atb=' '  ;
105200130422            alfaTRN=%subst(fv4not:1:7)  ;
105300130422 6          if alfaTRN>*zeros and alfaTRN=%editc(tlttrn:'X')  ;
105400130412             s02aper='Sb'   ;
105500130412             s02aper_rev=*on  ;
105600130412             leave  ;
105700130412 6          endif  ;
105800130412 5          endif  ;
105900130412 4          endif  ;
106000130412
106100130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
106200130412 3     enddo  ;
106300130412
106400130412 2       endif  ;
106500130412 1       endif  ;
106600130315
106700130315         clear Almenouno  ;
106800130412
106900130412         // non scrivo se devo escludere i già aperti
107000130412 0       if (d02apFGV='S' and s02aper=*blanks) or
107100130412             d02apfgv=' ' ;
107200130218
107300130219         // Lettura ESTENSIONI : mi memorizzo i giorni della sett
107400130219         //  se inseriti tutti non faccio nessuna proposta da listino traini
107500130219         //  per il trazionista, altrimenti si
107600130509         setll (v01fgs:tlttrn)    fnfgt01l       ;
107700130509         reade (v01fgs:tlttrn)    fnfgt01l       ;
107800130219
107900130219  1      dow not %eof(fnfgt01l)  ;
108000130315
108100130412         // se devo caricare solo il giorno previsto della sett --> controllo
108200130315         clear Indx ;
108300130315         if ilst1TUT='G'  and fgtgse<>*blanks  ;
108400130315         Indx=%scan(w02gioset:fgtgse)  ;
108500130315         endif  ;
108600130315
108700130315  2a     if ilst1tut=' ' or fgtgse= *blanks or Indx>0   ;
108800130315
108900130219         s02pdr=fgtpdr  ;
109000130219         chain (ktltip:s02pdr)  tntlz01l  ;
109100130219  2      if %found(tntlz01l ) ;
109200130219           s02dpdr=tlzrsc  ;
109300130219  2      endif  ;
109400130219
109500130319         clear   s02set  ;
109600130313         if %subst(fgtgse:1:1)='1'  ;
109700130313          %subst(s02set:1:1)='L'  ;
109800130313  2      endif  ;
109900130313         if %subst(fgtgse:2:1)='2'  ;
110000130313          %subst(s02set:2:1)='M'  ;
110100130313  2      endif  ;
110200130313         if %subst(fgtgse:3:1)='3'  ;
110300130313          %subst(s02set:3:1)='M'  ;
110400130313  2      endif  ;
110500130313         if %subst(fgtgse:4:1)='4'  ;
110600130313          %subst(s02set:4:1)='G'  ;
110700130313  2      endif  ;
110800130313         if %subst(fgtgse:5:1)='5'  ;
110900130313          %subst(s02set:5:1)='V'  ;
111000130313  2      endif  ;
111100130313         if %subst(fgtgse:6:1)='6'  ;
111200130313          %subst(s02set:6:1)='S'  ;
111300130313  2      endif  ;
111400130313         if %subst(fgtgse:7:1)='7'  ;
111500130313          %subst(s02set:7:1)='L'  ;
111600130313  2      endif  ;
111700130313
111800130219         s02dcn=fgtdcn  ;
111900130219         s02trm=fgttrm ;
112000130219         s02trr=fgttrr ;
112100130219         clear h02imm ;
112200130315         h02nrrp=FGTnrr  ;
112300130219
112400130221         Sott_FGT=*on  ;
112500130417          H02notrn=notrn  ;
112600130417
112700130620           if ilst1ges='A' and ilst1aut='S'
112800130620                           and w_ord<>w_ntraino;
112900130620              S02nrw += 1;
113000130620              write lst1S0w;
113100130620           else;
113200130620              S02nrr += 1;
113300130620              write lst1S02;
113400130620           endif;
113500130417
113600130220           NoTRN=*on  ;
113700130315           Almenouno='S'  ;
113800130219
113900130315  2a     endif  ;
114000130315
114100130509         reade (v01fgs:tlttrn)    fnfgt01l       ;
114200130219  1     enddo  ;
114300130412  0     endif  ;
114400130219
114500130218         // Lettura listino per proporre il trazionista
114600130315         // Per scelta solo se non ho scritto per nulla il traino
114700130412         // e se non ho ancora perto un foglio su quel traino
114800130412
114900130417  1    if Ricerca_prp=*off  or  Almenouno=*blanks
115000130417 0       and (d02apFGV=' ' or  s02aper=*blanks)  ;
115100130315
115200130218       setll  (tlttrn:tltdde)  titll01l  ;
115300130218       reade  (tlttrn:tltdde)  titll01l  ;
115400130218
115500130315  1a   dow not %eof(titll01l)  ;
115600130219  2    if tllatb=' '   ;
115700130219       // Se non trovata estensione del trazionista --> propongo
115800130509         chain (v01fgs:tlttrn:tllpdr)  fnfgt01l       ;
115900130219  3      if not %found(fnfgt01l)  ;
116000130219
116100130218         s02pdr=tllpdr  ;
116200130218         chain (ktltip:s02pdr)  tntlz01l  ;
116300130219  4      if %found(tntlz01l ) ;
116400130218           s02dpdr=tlzrsc  ;
116500130502           else  ;
116600130510           clear s02pdr  ;
116700130502           s02dpdr='?????????????'   ;
116800130219  4      endif  ;
116900130219
117000130218         clear s02dcn  ;
117100130218         clear s02set  ;
117200130219         clear s02trm  ;
117300130219         clear s02trr  ;
117400130417         clear h02nrrp ;
117500130218
117600130218          h02imm='S'   ;
117700080411
117800130417          H02notrn=notrn  ;
117900130221          Sott_FGT=*off ;
118000130620           if ilst1ges='A' and ilst1aut='S'
118100130620                           and w_ord<>w_ntraino;
118200130620              S02nrw += 1;
118300130620              write lst1S0w;
118400130620           else;
118500130620              S02nrr += 1;
118600130620              write lst1S02;
118700130620           endif;
118800130220           NoTRN=*on  ;
118900130418           Almenouno='S'  ;
119000130218
119100130219  3     endif                           ;
119200130219  2     endif                           ;
119300130219
119400130218       reade  (tlttrn:tltdde)  titll01l  ;
119500130315  1a    enddo  ;
119600130315  1     endif                           ;
119700130418
119800130418       // se non ne ho caricato nemmeno uno --> emetto traino SENZA
119900130418       //  trazionista
120000130429       if Almenouno=' '
120100130429         and (d02apFGV=' ' or  s02aper=*blanks)  ;
120200130418       clear s02pdr  ;
120300130418       s02dpdr='???????'  ;
120400130418
120500130418         clear s02dcn  ;
120600130418         clear s02set  ;
120700130418         clear s02trm  ;
120800130418         clear s02trr  ;
120900130418         clear h02nrrp ;
121000130418
121100130418          h02imm='S'   ;
121200130418
121300130418          H02notrn=notrn  ;
121400130418          Sott_FGT=*off ;
121500130620           if ilst1ges='A' and ilst1aut='S'
121600130620                           and w_ord<>w_ntraino;
121700130620              S02nrw += 1;
121800130620              write lst1S0w;
121900130620           else;
122000130620              S02nrr += 1;
122100130620              write lst1S02;
122200130620           endif;
122300130418           NoTRN=*on  ;
122400130418       endif  ;
122500130218
122600130221       Sott_FGT=*off ;
122700080411       ENDSR;
122800080414       //--------------------------------------------------------------
122900080414       //?controllo scelta SFL 2
123000080414       //--------------------------------------------------------------
123100080523       BEGSR contrS02 ;
123200080414
123300130130       ErrScelta=*off ;
123400130315       clear Wscelta  ;
123500080415
123600130315       // controllo data traino
123700130315       clear   wlbdat  ;
123800130315       eval g02dat=d02dtn  ;
123900130315       xsrda8 (wlbdat)   ;
124000130315       if g02err='1'    ;
124100130315           errDTN      = *on;
124200130315           errMessage  = *on;
124300130315           errGenerico = *on;
124400130315           V1cmsg = Msg(01);
124500130315       leavesr    ;
124600130315       endif  ;
124700130315
124800130315       w02dtn=g02inv  ;
124900130315       d02dtn=g02dat  ;
125000130412
125100130412       // controllo se richiesto traino
125200130412       clear wok  ;
125300130412 1     if d02trn>0    ;
125400130424       ktrn2=d02trn  ;
125500130424       exsr decorrTRN  ;
125600130412
125700130412 2     if wok=' '  ;
125800130412           errTRN      = *on;
125900130412           errMessage  = *on;
126000130412           errGenerico = *on;
126100130412           V1cmsg = Msg(08);
126200130412       leavesr    ;
126300130412 2     endif  ;
126400130412 1     endif  ;
126500130412
126600130315
126700130315       // se cambiata data devo ricaricare
126800130315       if savdtn<>w02dtn  ;
126900130412        $inzs02=*on  ;
127000130412        dataiso=%date(%int(w02dtn)) ;
127100130412        datadmy=dataiso  ;
127200130412        GIOggmmaa=%editc(%dec(datadmy) :'X') ;
127300130412        exsr CercaGIORNO  ;
127400130315       endif ;
127500130412
127600130412       // Se cambiato flag solo non aperti ricarico SFL
127700130412       if savaper<>D02apFGV ;
127800130412        $inzs02=*on  ;
127900130412       endif  ;
128000130315
128100130218       readc lst1s02;
128200130218 1     DOW  NOT  %eof(fnlst1D);
128300130129
128400130315       if s02sce<>' '  ;
128500130315
128600130412
128700130314       // In GESTIONE opzione '1' non ammmessa
128800130314       if Interrogaz=*off and s02sce='1'  ;
128900130314           errScelta   = *on;
129000130314           errMessage  = *on;
129100130314           errGenerico = *on;
129200130314           V1cmsg = Msg(05);
129300130314          exsr errSFL2  ;
129400130314          leavesr   ;
129500130314       endif  ;
129600130314
129700130314       // INTERROGAZIONE E RICERCA
129800130314       // Ammessa solo scelta '1' per ricerca
129900130314
130000130412       if Interrogaz=*on  and s02sce<>'5' and s02sce<>'T'  ;
130100130315       if ilst1ges='I' or (Ricerca_prp=*on  and s02sce<>'1')  ;
130200130314           errScelta   = *on;
130300130314           errMessage  = *on;
130400130314           errGenerico = *on;
130500130314           V1cmsg = Msg(05);
130600130314          exsr errSFL2  ;
130700130314          leavesr   ;
130800130314       endif  ;
130900130314       endif  ;
131000130412
131100130412       // Interrogazione TRAINO
131200130412       if s02sce='T'   ;
131300130412      /end-free
131400130412     C                   MOVEL     'T05'         D00OP0
131500130412     C                   MOVEL     'O05'         D00OP1
131600130412     C                   MOVEL     '0'           D00F03
131700130412     C                   MOVEL     '0'           D00F12
131800130412     C                   MOVEL     '0'           D00ERR
131900130412     C                   MOVEL     *BLANKS       D00MSG
132000130412     C                   Z-ADD     s02trn        D00TRN
132100130417     C                   Z-ADD     h02dde        D00DDE
132200130412     C                   MOVEL     tntl00ds      KPJBU
132300130412     C                   CALL      'TNTL01C'
132400130412     C                   PARM                    KPJBA
132500130412      /free
132600130412
132700130412          clear s02sce    ;
132800130412          exsr errSFL2  ;
132900130412        else    ;
133000130314
133100130220       // non posso fare visualizzazione su un record in inserimento
133200130319       //  non do errore in apertura FGV
133300130319       if ilst1ges<>'A'   ;
133400130313 2     if h02imm='S' and  s02sce<>'I'   and s02sce<>' '  ;
133500130220           errScelta   = *on;
133600130220           errMessage  = *on;
133700130220           errGenerico = *on;
133800130313           V1cmsg = Msg(02);
133900130220          exsr errSFL2  ;
134000130313          leavesr   ;
134100130220       endif  ;
134200130319       endif  ;
134300130503
134400130503 2     if h02imm='S' and  s02sce= '5'   ;
134500130503          clear s02sce    ;
134600130503          exsr errSFL2  ;
134700130503          leavesr   ;
134800130503       endif  ;
134900130319
135000130315
135100130429       // se premeto F15--> non accetto scelta con '1'
135200130429           if   dsp_aid = c_F15 and s02sce='1'  ;
135300130315           errScelta   = *on;
135400130315           errMessage  = *on;
135500130315           errGenerico = *on;
135600130315           V1cmsg = Msg(06);
135700130315          exsr errSFL2  ;
135800130315          leavesr   ;
135900130315           endif  ;
136000130220
136100130318       // Per scelta aggiorno con SFLNEXG per rileggerli tutti alla fine
136200130318       if s02sce='1'     ;
136300130318          wscelta='1'  ;
136400130318          exsr errSFL2  ;
136500130318       else   ;
136600130129         $Video = 'D5';
136700130129          $inzd05=*on;
136800130129
136900130129         dow $Video='D5'  ;
137000130129          exsr Gesd05     ;
137100130129         enddo ;
137200130130          InterD05=*off ;
137300130130          Protd05 =*off ;
137400130129          clear s02sce    ;
137500130130
137600130130          exsr errSFL2  ;
137700130313          endif  ;
137800130318          endif  ;
137900130319          endif  ;
138000080414
138100130315
138200130218       readc lst1s02;
138300130122       enddo  ;
138400130122
138500130122       ENDSR   ;
138600080603       //--------------------------------------------------------------
138700080603       //?Aggiornamento sfl per errore
138800080603       //--------------------------------------------------------------
138900080603       BEGSR ErrSFL2 ;
139000130417
139100130313        if h02imm='S'  ;
139200130313        Sott_fgt=*off  ;
139300130313        else  ;
139400130313        Sott_fgt=*on   ;
139500130313        endif  ;
139600130313
139700130417         NoTRN=H02noTRN  ;
139800130418         if  s02sce_prot=*off  ;
139900130417          SflNxtChg = *on ;
140000130417         else  ;
140100130417          SflNxtChg = *off ;
140200130417         endif  ;
140300130429
140400130429         if s02aper='Sb'      ;
140500130429          s02aper_rev=*on  ;
140600130429         endif  ;
140700130429
140800130218         update lst1s02;
140900130412         c02csr=s02nrr ;
141000080603
141100130429         s02aper_rev=*off ;
141200080603         errscelta=*off  ;
141300080603         SflNxtChg = *off ;
141400080603       ENDSR;
141500080411
141600130412       //--------------------------------------------------------------
141700130412       //?Posizinamento SFL2 per numero traino
141800130412       //--------------------------------------------------------------
141900130412       BEGSR PosizTRN  ;
142000130412
142100130412       if d02trn=0  ;
142200130412        c02csr=1  ;
142300130412       else  ;
142400130412
142500130412       eval s02nrr=1  ;
142600130412       chain s02nrr   lst1s02;
142700130412
142800130412       dow %found     ;
142900130412       if s02trn=d02trn  ;
143000130412         c02csr=s02nrr ;
143100130412         leave  ;
143200130412       endif  ;
143300130412
143400130412       s02nrr=s02nrr+1  ;
143500130412
143600130412       chain s02nrr   lst1s02;
143700130412       enddo   ;
143800130412       endif  ;
143900130412       savtrn=d02trn  ;
144000130412
144100130412       ENDSR;
144200130129       //--------------------------------------------------------------
144300130129       //?Gestione videata D05
144400130129       //--------------------------------------------------------------
144500130129         BEGSR GesD05;
144600130129
144700130129         // Inizializzazione videata
144800130129
144900130129         if  $inzd05 = *on;
145000130129         // Verifico che data proporre per le richieste
145100130129           exsr Inzd05    ;
145200130129          $Inzd05=*off      ;
145300130129         endif ;
145400130129
145500130129         // Emissione testata
145600130514         if  errGenerico = *off or ricInt<>' ';
145700130218           write lst1T01;
145800130129           clear ricint   ;
145900130129         endif;
146000130129
146100130129         // Emissione videata
146200130218         exfmt lst1D05;
146300130129
146400130129         clear V1cmsg;
146500130129         clear errGenerico;
146600130422         ErrMessage =*off  ;
146700130313         clear errgg1  ;
146800130313         clear errgg2  ;
146900130313         clear errgg3  ;
147000130313         clear errgg4  ;
147100130313         clear errgg5  ;
147200130313         clear errgg6  ;
147300130313         clear errgg7  ;
147400130419         clear ERRTRN_IMM      ;
147500130419         clear ERRTRN_nw       ;
147600130422         clear ERRpdr          ;
147700130422         clear ERRdcn          ;
147800130422         clear ERRTRM          ;
147900130129
148000130129    1    select;
148100130129
148200130129         // F12=ritorno
148300130129           when dsp_aid = c_F12;
148400130621               unlock fnfgt00f;
148500130129               $Video = 'S2';
148600130130               Errgenerico=*off  ;
148700130130               ErrMessage =*off  ;
148800130129
148900130129           other  ;
149000130129             exsr Contrd05 ;
149100130129
149200130130             if InterD05=*on  ;
149300130130             ErrMessage=*off  ;
149400130130             ErrGenerico=*off  ;
149500130130             endif    ;
149600130130
149700130514    2        if  errGenerico = *on or ricint<>' '  ;
149800130129               leavesr;
149900130129    2        endif;
150000130129
150100130130    2      if   InterD05=*on    ;
150200130129               $Video = 'S2';
150300130129    2      endif  ;
150400130129
150500130129 2         if    dsp_aid = c_F16;
150600130129               $Inzs02=*on  ;
150700130129               $Video = 'S2';
150800130129 2           endif;
150900130129
151000130129 2         if    dsp_aid = c_F06;
151100130129            exsr  AggiorD05  ;
151200130129
151300130129 3           if  errGenerico = *on;
151400130129               leavesr;
151500130129             else  ;
151600130129               $Video = 'S2';
151700130129 3           endif;
151800130313 2         endif;
151900130313
152000130313 2         if    dsp_aid = c_F16;
152100130313            delete fnfgtfis ;
152200130313               $Video = 'S2';
152300130313 2         endif;
152400130129
152500130129 1           endsl  ;
152600130129
152700130502          if   $Video = 'S2';
152800130502          h02nrrp=savnrrp5  ;
152900130502          endif  ;
153000130502
153100130129          ENDSR;
153200130129       //--------------------------------------------------------------
153300130129       //?Inizializzazione VIDEATA 5
153400130129       //--------------------------------------------------------------
153500130129       BEGSR Inzd05;
153600130220       Interd05=*off  ;
153700130220       Protd05 =*off  ;
153800130220       CopiaD05=*off  ;
153900130220       Spostad05=*off  ;
154000130220       Manutd05=*off  ;
154100130220       Imm_d05 =*off  ;
154200130502       savnrrp5=h02nrrp  ;
154300130129
1544001302201      if s02sce='2'  ;
154500130129       v01dric= ' MANUTENZIONE '  ;
154600130220       ManutD05=*ON  ;
1547001302201      ENDIF  ;
1548001302201      if s02sce='3'  ;
154900130220       v01dric= 'C  O  P  I  A '  ;
155000130220       PROTD05=*ON  ;
155100130220       copiaD05=*ON  ;
1552001302201      ENDIF  ;
1553001303131      if s02sce='8'  ;
155400130220       v01dric= ' S P O S T A  ' ;
155500130220       PROTD05=*ON  ;
155600130220       SpostaD05=*ON  ;
1557001302201      ENDIF  ;
1558001302201      if s02sce='5'  ;
155900130129       v01dric= 'INTERROGAZIONE'  ;
156000130130       INTERD05=*ON  ;
156100130129       PROTD05=*ON  ;
1562001302201      ENDIF  ;
156300130220
1564001302201      if s02sce='I' or S02sce=' '  ;
156500130220       v01dric= ' INSERIMENTO  '  ;
156600130220       Imm_d05=*on  ;
156700130424       clear h02nrrp  ;
1568001302201      ENDIF  ;
156900130220
157000130220       // se si tratta di record di nuova immissione
157100130220       //   pulisco i campi
157200130220       clear d05pru  ;
157300130220       clear d05dim  ;
157400130220       clear d05trn  ;
157500130419       clear d05trn_n;
157600130220       clear d05tfp  ;
157700130220       clear d05dtfp  ;
157800130220       clear d05tfa  ;
157900130220       clear d05dtfa ;
158000130220       clear d05pdr  ;
158100130220       clear d05dpdr ;
158200130220       clear d05dcn  ;
158300130220       clear d05trm  ;
158400130220       clear d05trr  ;
158500130220       clear d05not  ;
158600130628       clear d05nt2  ;
158700130628       clear d05an1  ;
158800130628       clear d05an2  ;
158900130220       clear d05salt  ;
159000130220       clear d05gg1   ;
159100130220       clear d05gg2   ;
159200130220       clear d05gg3   ;
159300130220       clear d05gg4   ;
159400130220       clear d05gg5   ;
159500130220       clear d05gg6   ;
159600130220       clear d05gg7   ;
159700130313
159800130424       clear Wforzgg1 ;
159900130424       clear Wforzgg2 ;
160000130424       clear Wforzgg3 ;
160100130424       clear Wforzgg4 ;
160200130424       clear Wforzgg5 ;
160300130424       clear Wforzgg6 ;
160400130424       clear Wforzgg7 ;
160500130424       clear Wfor2gg1 ;
160600130424       clear Wfor2gg2 ;
160700130424       clear Wfor2gg3 ;
160800130424       clear Wfor2gg4 ;
160900130424       clear Wfor2gg5 ;
161000130424       clear Wfor2gg6 ;
161100130424       clear Wfor2gg7 ;
161200130426       clear WforMgg1 ;
161300130426       clear WforMgg2 ;
161400130426       clear WforMgg3 ;
161500130426       clear WforMgg4 ;
161600130426       clear WforMgg5 ;
161700130426       clear WforMgg6 ;
161800130426       clear WforMgg7 ;
161900130424
162000130313       clear d05trn_nw ;
162100130313       clear d05tfpnw;
162200130313       clear d05dtfpnw ;
162300130313       clear d05tfanw;
162400130313       clear d05dtfanw ;
162500130220
162600130220       // Se ho effettuato la scelta --> propongo i dati della riga
162700130220 1     if s02sce<>' '  ;
162800130220
162900130220       // IMMISSIONE
163000130220 2     select ;
163100130220       when s02sce ='I'     ;
163200130313       d05trn=%editc(s02trn:'X')  ;
163300130220       d05tfp=s02tfp  ;
163400130220       d05tfa=s02tfa  ;
163500130313       d05pdr=%editc(s02pdr:'X')  ;
163600130423       // mi posiziono sul padroncino se vuoto, altrimenti sul conducente
163700130423       if d05trn>*zeros and d05pdr>*zeros  ;
163800130423       errdcn=*on   ;
163900130423       endif  ;
164000130423       if d05trn>*zeros and d05pdr<=*zeros  ;
164100130423       d05pdr='?'  ;
164200130423       errpdr=*on   ;
164300130423       endif  ;
164400130423
164500130423       d05dcn='?'  ;
164600130423       d05trm='?'  ;
164700130220
164800130313       // altre opzioni
164900130313 2     when s02sce  <>' ' and s02sce<>'I'  ;
165000130621       if s02sce='5';
165100130621       chain(n) h02nrrp  fnfgt00f  ;
165200130621       else;
165300130315       chain  h02nrrp  fnfgt00f  ;
165400130621       endif;
165500130220 3     if %found(fnfgt00f) ;
165600130313       d05trn=%editc(s02trn:'X')  ;
165700130419       d05trn_n= s02trn  ;
165800130220       d05tfp=s02tfp  ;
165900130220       d05tfa=s02tfa  ;
166000130313       d05pdr=%editc(fgtpdr:'X')  ;
166100130220       d05dcn=fgtdcn  ;
166200130220       d05trm=fgttrm  ;
166300130220       d05trr=fgttrr  ;
166400130220       d05not=fgtnot  ;
166500130628       d05nt2=fgtnt2  ;
166600130628       d05an1=fgtan1  ;
166700130628       d05an2=fgtan2  ;
166800130313       d05pru=fgtpru  ;
166900130313       dataiso=%date(fgtdim) ;
167000130313       dataeur=dataiso  ;
167100130313       d05dim=%dec(dataeur)   ;
167200130220
167300130514  4    //if fgtgse=*blanks  ;
167400130417        //  d05salt ='SI'   ;
167500130514       //else  ;
167600130514         if %subst(fgtgse:1:1)<>' '     ;
167700130514         d05gg1='S'  ;
167800130514         endif  ;
167900130514         if %subst(fgtgse:2:1)<>' '     ;
168000130514         d05gg2='S'  ;
168100130514         endif  ;
168200130514         if %subst(fgtgse:3:1)<>' '     ;
168300130514         d05gg3='S'  ;
168400130514         endif  ;
168500130514         if %subst(fgtgse:4:1)<>' '     ;
168600130514         d05gg4='S'  ;
168700130514         endif  ;
168800130514         if %subst(fgtgse:5:1)<>' '     ;
168900130514         d05gg5='S'  ;
169000130514         endif  ;
169100130514         if %subst(fgtgse:6:1)<>' '     ;
169200130514         d05gg6='S'  ;
169300130514         endif  ;
169400130514         if %subst(fgtgse:7:1)<>' '     ;
169500130514         d05gg7='S'  ;
169600130514         endif  ;
169700130514         //d05gg2= %subst(fgtgse:2:1)     ;
169800130514         //d05gg3= %subst(fgtgse:3:1)     ;
169900130514         //d05gg4= %subst(fgtgse:4:1)     ;
170000130514         //d05gg5= %subst(fgtgse:5:1)     ;
170100130514         //d05gg6= %subst(fgtgse:6:1)     ;
170200130514         //d05gg7= %subst(fgtgse:7:1)     ;
170300130514 4      // endif  ;
170400130220
170500130220  3    endif  ;
170600130220  2    endsl  ;
170700130510       else  ;
170800130510       d05pdr='?'  ;
170900130510       d05dcn='?'  ;
171000130510       d05trm='?'  ;
171100130510       d05trn='?'  ;
171200130221  1    endif  ;
171300130220
171400130220       if d05pdr > *zeros   ;
171500130220       kpdr =%int(d05pdr)  ;
171600130220         chain (ktltip:kpdr)  tntlz01l  ;
171700130221         if %found(tntlz01l ) ;
171800130220           D05dpdr=tlzrsc  ;
171900130221         endif  ;
172000130221       endif  ;
172100130220
172200130220       if d05tfp>0  ;
172300130220       chain d05tfp azorg01l  ;
172400130220  2      if %found(azorg01l ) ;
172500130220           D05dtfp =orgdes ;
172600130220  2      endif  ;
172700130220       endif  ;
172800130220       if d05tfa>0  ;
172900130220       chain d05tfa azorg01l  ;
173000130220  2      if %found(azorg01l ) ;
173100130220           D05dtfa =orgdes ;
173200130220  2      endif  ;
173300130220  1    endif  ;
173400130129
173500130129       ENDSR  ;
173600130129       //--------------------------------------------------------------
173700130129       //?controllo dati   video 5
173800130129       //--------------------------------------------------------------
173900130129       BEGSR contrd05 ;
174000130313
174100130313       // Per immissione controllo il traino
174200130313 1     if imm_d05=*on   ;
174300130313       Indx=%scan('?':d05trn)  ;
174400130313 2     if indx>0   ;
174500130313        exsr ricercaTraino  ;
174600130313
174700130313 3     if d00trn>0  ;
174800130313       d05trn = %editc(d00trn:'X')  ;
174900130422
175000130422        chain  (d00trn:d00dde) tntlt01l  ;
175100130422 4      if %found(tntlt01l)  ;
175200130422        d05tfp    = tlttfp              ;
175300130422        d05tfa    = tlttfa              ;
175400130422        exsr DecoTRNimm ;
175500130422 x4    else  ;
175600130422       clear d05trn  ;
175700130422  4     endif  ;
175800130313 x3    else  ;
175900130313       clear d05trn  ;
176000130313  3    endif  ;
176100130313
176200130514       ricint='S'  ;
176300130314       leavesr     ;
176400130313  2    endif    ;
176500130313  1    endif   ;
176600130313
176700130313       // Per copia o spostamento  vedo se attivata la ricerca traino
176800130313       Indx=%scan('?':d05trn_nw)  ;
176900130419  0    if indx>0   ;
177000130313        exsr ricercaTraino  ;
177100130313
177200130419 1     if d00trn>0  ;
177300130313        d05trn_nw = %editc(d00trn:'X')  ;
177400130419
177500130419        chain  (d00trn:d00dde) tntlt01l  ;
177600130419 2      if %found(tntlt01l)  ;
177700130422        d05tfpNW  = tlttfp              ;
177800130422        d05tfaNW  = tlttfa              ;
177900130422        exsr DecoTRN_nw ;
178000130422 x2    else  ;
178100130422        clear d05trn_nw  ;
178200130422 2      endif  ;
178300130313
178400130419 x1    else  ;
178500130313        clear d05trn_nw  ;
178600130419 1     endif  ;
178700130313
178800130514       ricint='S'  ;
178900130419       leavesr  ;
179000130419 0     endif    ;
179100130419
179200130419       // Immissione     Traino --> controllo
179300130502 1     // if imm_d05=*on   ;
179400130419       alfaTRN=d05trn  ;
179500130419       exsr  CtrTRN  ;
179600130419 2      if Errgenerico=*on   ;
179700130419         Errtrn_imm=*on   ;
179800130419         leavesr  ;
179900130422 x2      else  ;
180000130422        d05tfp    = tlttfp              ;
180100130422        d05tfa    = tlttfa              ;
180200130422        exsr DecoTRNimm ;
180300130419 2      endif    ;
180400130502 1     // endif    ;
180500130419
180600130419       // Copia o sposta traino --> controllo
180700130419 1     if Copiad05=*on or spostad05=*on    ;
180800130419       alfatrn=d05trn_nw  ;
180900130419       exsr  CtrTRN  ;
181000130419 2      if Errgenerico=*on   ;
181100130419         Errtrn_nw =*on   ;
181200130419         leavesr  ;
181300130422 x2      else  ;
181400130422        d05tfpNW  = tlttfp              ;
181500130422        d05tfaNW  = tlttfa              ;
181600130422        exsr DecoTRN_nw ;
181700130419 2      endif    ;
181800130419 1     endif    ;
181900130422
182000130509       // Trazionista
182100130509       //  Ricerca
182200130509       Indx=%scan('?':d05pdr)  ;
182300130509 2     if indx>0   ;
182400130509        clear trul16ds  ;
182500130509        pa3tip='T ' ;
182600130509        pa3rsc=d05dpdr  ;
182700130509        kpjbu= trul16ds  ;
182800130509        callp trul16r   (kpjba)  ;
182900130509        trul16ds=kpjbu  ;
183000130509        d05pdr=%editc(pa3pdr:'X')   ;
183100130509        d05dpdr=pa3rsc  ;
183200130509
183300130510           ricint=*on  ;
183400130509           errpdr      = *on;
183500130509           leavesr  ;
183600130509
183700130509       endif  ;
183800130509
183900130422       if d05pdr  = *zeros    or d05pdr=*blanks  ;
184000130422           errpdr      = *on;
184100130422           errMessage  = *on;
184200130422           errGenerico = *on;
184300130422           V1cmsg = Msg(10);
184400130422           leavesr  ;
184500130422           endif  ;
184600130422
184700130422       if  d05pdr<= *ZEROS   ;
184800130422           errpdr      = *on;
184900130422           errMessage  = *on;
185000130422           errGenerico = *on;
185100130422           V1cmsg = Msg(10);
185200130422           leavesr  ;
185300130422       ELSE  ;
185400130422
185500130422       kpdr =%int(d05pdr)  ;
185600130422         chain (ktltip:kpdr)  tntlz01l  ;
185700130422         if %found(tntlz01l ) ;
185800130422           D05dpdr=tlzrsc  ;
185900130422         else  ;
186000130422           errpdr      = *on;
186100130422           errMessage  = *on;
186200130422           errGenerico = *on;
186300130422           V1cmsg = Msg(10);
186400130422           leavesr  ;
186500130422         endif  ;
186600130422       endif  ;
186700130502
186800130502       // Il trazionista deve essere previsto dal traino
186900130502       setll (kpdr:tlttrn:tltdde) titll02l   ;
187000130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
187100130502       dow not %eof(titll02l)  ;
187200130502       if tllatb=' '  ;
187300130502       leave    ;
187400130502       else  ;
187500130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
187600130502       endif  ;
187700130502       enddo  ;
187800130502
187900130502       if %eof(titll02l)  ;
188000130510       // non segnalo errore per traino locale
188100130510       if h02ttr<>'L'  ;
188200130502           errpdr      = *on;
188300130502           errMessage  = *on;
188400130502           errGenerico = *on;
188500130502           V1cmsg = Msg(18);
188600130502           leavesr  ;
188700130502       endif  ;
188800130510       endif  ;
188900130422
189000130508       // Conducente --> vedo se accreditato escludo H L D
189100130422 1     if d05dcn<>*blanks   ;
189200130422
189300130422       // controllo se c'e' ricerca
189400130422       Indx=%scan('?':d05dcn)  ;
189500130422 2     if indx>0   ;
189600130422         clear tntl83ds  ;
189700130422         i83soc=soctraini ;
189800130422         i83ksc=kpdr      ;
189900130423         i83mod='1'  ;
190000130423         i83drf=%editc(w02dtn:'X')  ;
190100130422         kpjbu=tntl83ds  ;
190200130422         callp TNTL83R  (kpjba)  ;
190300130422         tntl83ds =kpjbu  ;
190400130422 3       if o83_aut<>*blanks  ;
190500130422          d05dcn= o83_aut  ;
190600130423          else  ;
190700130423          clear d05dcn ;
190800130422 3       endif   ;
190900130423
191000130422         ricint=*on  ;
191100130423         errdcn=*on  ;
191200130422       leavesr  ;
191300130422 2     endif   ;
191400130422
191500130508       if tltttr <>'H' and tltttr<>'L' and tltttr<>'D'  ;
191600130508
191700130422       chain (soctraini:kpdr:d05dcn)  aiata01l  ;
191800130422       if not %found(aiata01l) or ataatb<>' '  ;
191900130422           errdcn      = *on;
192000130422           errMessage  = *on;
192100130422           errGenerico = *on;
192200130422           V1cmsg = Msg(11);
192300130422           leavesr  ;
192400130422       endif  ;
192500130422
192600130508       // se alla data decorrenza è scaduto il rapporto  --> errore
192700130422       if atadtfir>0 and w02dtn > atadtfir  ;
192800130422           errdcn      = *on;
192900130422           errMessage  = *on;
193000130422           errGenerico = *on;
193100130422           V1cmsg = Msg(12);
193200130422           leavesr  ;
193300130422       endif  ;
193400130508       endif  ;
193500130508
193600130422       endif  ;
193700130422
193800130422       // Targa Motrice
193900130422       if d05trm<>*blanks  ;
194000130423
194100130423       // controllo se c'e' ricerca
194200130423       Indx=%scan('?':d05trm)  ;
194300130423 2     if indx>0   ;
194400130423         clear tntl84ds  ;
194500130423         i84soc=soctraini ;
194600130423         i84ksc=kpdr      ;
194700130423         i84mod='1'  ;
194800130423         i84drf=%editc(w02dtn:'X')  ;
194900130423         kpjbu=tntl84ds  ;
195000130423         callp TNTL84R  (kpjba)  ;
195100130423         tntl84ds =kpjbu  ;
195200130423 3       if o84_TRM<>*blanks  ;
195300130423          d05trm= o84_TRM  ;
195400130423          else  ;
195500130423          clear d05trm ;
195600130423 3       endif   ;
195700130423
195800130423         ricint=*on  ;
195900130423         errtrm=*on  ;
196000130423       leavesr  ;
196100130423 2     endif   ;
196200130423
196300130508       if tltttr <>'H' and tltttr<>'L' and tltttr<>'D'  ;
196400130508
196500130422       chain (soctraini:kpdr:d05trm)  aiatm01l  ;
196600130422       if not %found(aiatm01l) or atmatb<>' '  ;
196700130422           errtrm      = *on;
196800130422           errMessage  = *on;
196900130422           errGenerico = *on;
197000130422           V1cmsg = Msg(13);
197100130422           leavesr  ;
197200130422       endif  ;
197300130422
197400130422       // se alla data decorrenza è scaduto il rapporto  --> errore
197500130422       if atmdtfir>0 and w02dtn > atmdtfir  ;
197600130422           errtrm      = *on;
197700130422           errMessage  = *on;
197800130422           errGenerico = *on;
197900130422           V1cmsg = Msg(13);
198000130422           leavesr  ;
198100130422       endif  ;
198200130508       endif  ;
198300130508
198400130422       endif  ;
198500130422
198600130422       // Giorni partenza
198700130514       if d05gg1<>' '  and d05gg1<>'S'  ;
198800130422           errgg1      = *on;
198900130313           errMessage  = *on;
199000130313           errGenerico = *on;
199100130313           V1cmsg = Msg(03);
199200130514           %subst(v1cmsg:53:10)='LUNEDÌ    ' ;
199300130313           leavesr;
199400130313       endif  ;
199500130503       // controllo se giorno previsto dal traino
199600130514       if d05gg1='S' and %subst(tltgse:1:1)=' '  ;
199700130503           errgg1      = *on;
199800130503           errMessage  = *on;
199900130503           errGenerico = *on;
200000130503           V1cmsg = Msg(19);
200100130503       endif  ;
200200130503
200300130514       if d05gg2<>' '  and d05gg2<>'S'  ;
200400130313           errgg2      = *on;
200500130313           errMessage  = *on;
200600130313           errGenerico = *on;
200700130313           V1cmsg = Msg(03);
200800130514           %subst(v1cmsg:53:10)='MARTEDÌ   ' ;
200900130313           leavesr;
201000130313       endif  ;
201100130503       // controllo se giorno previsto dal traino
201200130514       if d05gg2='S' and %subst(tltgse:2:1)=' '  ;
201300130503           errgg2      = *on;
201400130503           errMessage  = *on;
201500130503           errGenerico = *on;
201600130503           V1cmsg = Msg(19);
201700130503       endif  ;
201800130514       if d05gg3<>' '  and d05gg3<>'S'  ;
201900130313           errgg3      = *on;
202000130313           errMessage  = *on;
202100130313           errGenerico = *on;
202200130313           V1cmsg = Msg(03);
202300130514           %subst(v1cmsg:53:10)='MERCOLEDÌ ' ;
202400130313           leavesr;
202500130313       endif  ;
202600130503       // controllo se giorno previsto dal traino
202700130514       if d05gg3='S' and %subst(tltgse:3:1)=' '  ;
202800130503           errgg3      = *on;
202900130503           errMessage  = *on;
203000130503           errGenerico = *on;
203100130503           V1cmsg = Msg(19);
203200130503       endif  ;
203300130514       if d05gg4<>' '  and d05gg4<>'S'  ;
203400130313           errgg4      = *on;
203500130313           errMessage  = *on;
203600130313           errGenerico = *on;
203700130313           V1cmsg = Msg(03);
203800130514           %subst(v1cmsg:53:10)='GIOVEDÌ   ' ;
203900130313           leavesr;
204000130313       endif  ;
204100130503       // controllo se giorno previsto dal traino
204200130514       if d05gg4='S' and %subst(tltgse:4:1)=' '  ;
204300130503           errgg4      = *on;
204400130503           errMessage  = *on;
204500130503           errGenerico = *on;
204600130503           V1cmsg = Msg(19);
204700130503       endif  ;
204800130514       if d05gg5<>' '  and d05gg5<>'S'  ;
204900130313           errgg5      = *on;
205000130313           errMessage  = *on;
205100130313           errGenerico = *on;
205200130313           V1cmsg = Msg(03);
205300130514           %subst(v1cmsg:53:10)='VENERDÌ   ' ;
205400130313           leavesr;
205500130313       endif  ;
205600130514       if d05gg5='S' and %subst(tltgse:5:1)=' '  ;
205700130503           errgg5      = *on;
205800130503           errMessage  = *on;
205900130503           errGenerico = *on;
206000130503           V1cmsg = Msg(19);
206100130503       endif  ;
206200130514       if d05gg6<>' '  and d05gg6<>'S'  ;
206300130313           errgg6      = *on;
206400130313           errMessage  = *on;
206500130313           errGenerico = *on;
206600130313           V1cmsg = Msg(03);
206700130514           %subst(v1cmsg:53:10)='SABATO    ' ;
206800130313           leavesr;
206900130313       endif  ;
207000130514       if d05gg6='S' and %subst(tltgse:6:1)=' '  ;
207100130503           errgg6      = *on;
207200130503           errMessage  = *on;
207300130503           errGenerico = *on;
207400130503           V1cmsg = Msg(19);
207500130503       endif  ;
207600130514       if d05gg7<>' '  and d05gg7<>'S'  ;
207700130313           errgg7      = *on;
207800130313           errMessage  = *on;
207900130313           errGenerico = *on;
208000130313           V1cmsg = Msg(03);
208100130514           %subst(v1cmsg:53:10)='DOMENICA  ' ;
208200130313           leavesr;
208300130313       endif  ;
208400130514       if d05gg7='S' and %subst(tltgse:7:1)=' '  ;
208500130503           errgg7      = *on;
208600130503           errMessage  = *on;
208700130503           errGenerico = *on;
208800130503           V1cmsg = Msg(19);
208900130503       endif  ;
209000130129
209100130313       IF  D05GG1= ' ' and D05GG2= ' '   and D05GG3= ' '
209200130313          and D05GG4= ' '  and D05GG5= ' ' and D05GG6= ' '
209300130422          and D05GG7= ' '   ;
209400130423           errgg1      = *on;
209500130313           errMessage  = *on;
209600130313           errGenerico = *on;
209700130313           V1cmsg = Msg(04);
209800130313           leavesr;
209900130313       endif  ;
210000130424
210100130502       // controllo da fare solo se non annullo
210200130502       if   dsp_aid <> c_F16;
210300130502
210400130424       // Per ogni giorno inserito verifico se c'e' un'altra proposta
210500130424       // se sposto o copio uso il traino di destino
210600130424       if d05trn_nw>*zeros   ;
210700130424        ktrn=%int(d05trn_nw)  ;
210800130424       else  ;
210900130424        ktrn=%int(d05trn)  ;
211000130424       endif  ;
211100130424
211200130509       setll   (v01fgs:ktrn)     fnfgt01l  ;
211300130509       reade   (v01fgs:ktrn)     fnfgt01l  ;
211400130424
211500130424       dow  not %eof(fnfgt01l)  ;
211600130424       if fgtnrr <>  h02nrrp ;
211700130514       if d05gg1<>' '  and %subst(fgtgse:1:1)<>' '
211800130424        and wforzgg1=' '  ;
211900130424           errgg1      = *on;
212000130424           errMessage  = *on;
212100130424           errGenerico = *on;
212200130424           V1cmsg = Msg(14);
212300130424           %subst(v1cmsg:39:10)='  LUNEDÌ  ' ;
212400130424           wforzgg1='S'  ;
212500130424           leavesr;
212600130424       endif  ;
212700130514       if d05gg2<>' '  and %subst(fgtgse:2:1)<>' '
212800130424        and wforzgg2=' '  ;
212900130424           errgg2      = *on;
213000130424           errMessage  = *on;
213100130424           errGenerico = *on;
213200130424           V1cmsg = Msg(14);
213300130424           %subst(v1cmsg:39:10)='  MARTEDÌ ' ;
213400130424           wforzgg2='S'  ;
213500130424           leavesr;
213600130424       endif  ;
213700130514       if d05gg3<>' '  and  %subst(fgtgse:3:1)<>' '
213800130424        and wforzgg3=' '  ;
213900130424           errgg3      = *on;
214000130424           errMessage  = *on;
214100130424           errGenerico = *on;
214200130424           V1cmsg = Msg(14);
214300130424           %subst(v1cmsg:39:10)='MERCOLEDÌ ';
214400130424           wforzgg3='S'  ;
214500130424           leavesr;
214600130424       endif  ;
214700130514       if d05gg4<>' '  and %subst(fgtgse:4:1)<>' '
214800130424        and wforzgg4=' '  ;
214900130424           errgg4      = *on;
215000130424           errMessage  = *on;
215100130424           errGenerico = *on;
215200130424           V1cmsg = Msg(14);
215300130424           %subst(v1cmsg:39:10)=' GIOVEDÌ  ' ;
215400130424           wforzgg4='S'  ;
215500130424           leavesr;
215600130424       endif  ;
215700130514       if d05gg5<>' '  and %subst(fgtgse:5:1)<>' '
215800130424        and wforzgg5=' '  ;
215900130424           errgg5      = *on;
216000130424           errMessage  = *on;
216100130424           errGenerico = *on;
216200130424           V1cmsg = Msg(14);
216300130424           %subst(v1cmsg:39:10)=' VENERDÌ  ' ;
216400130424           wforzgg5='S'  ;
216500130424           leavesr;
216600130424       endif  ;
216700130514       if d05gg6<>' '  and %subst(fgtgse:6:1)<>' '
216800130424        and wforzgg6=' '  ;
216900130424           errgg6      = *on;
217000130424           errMessage  = *on;
217100130424           errGenerico = *on;
217200130424           V1cmsg = Msg(14);
217300130424           %subst(v1cmsg:39:10)='  SABATO  ' ;
217400130424           wforzgg6='S'  ;
217500130424           leavesr;
217600130424       endif  ;
217700130514       if d05gg7<>' '  and %subst(fgtgse:7:1)<>' '
217800130424        and wforzgg7=' '  ;
217900130424           errgg7      = *on;
218000130424           errMessage  = *on;
218100130424           errGenerico = *on;
218200130424           V1cmsg = Msg(14);
218300130424           %subst(v1cmsg:39:10)=' DOMENICA ' ;
218400130424           wforzgg7='S'  ;
218500130424           leavesr;
218600130424       endif  ;
218700130424       endif  ;
218800130424
218900130509       reade   (v01fgs:ktrn)     fnfgt01l  ;
219000130424       enddo  ;
219100130424
219200130426       // Controllo se il CONDUCENTE è già presente in un traino decorrente
219300130424       //  nello stesso giorno
219400130508
219500130508       if d05dcn<>*blanks  ;
219600130508
219700130424       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtdcn='''+
219800130424        d05dcn + ''''                                    ;
219900130424       EXEC sql  prepare s1 from : wrkgetlista ;
220000130424       EXEC sql  declare a1 cursor for s1      ;
220100130424       EXEC sql  open    a1                    ;
220200130424
220300130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
220400130424  1    dow sqlcod=0    ;
220500130426       wtipo='C'  ;
220600130426       Exsr ErrDOPP  ;
220700130426       if v1cmsg<>*blanks  ;
220800130426        leave  ;
220900130426       endif  ;
221000130426
221100130424
221200130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
221300130424       enddo           ;
221400130424
221500130424       EXEC sql  CLOSE   a1                    ;
221600130426
221700130426       if v1cmsg<>*blanks  ;
221800130426        leavesr;
221900130426       endif  ;
222000130508
222100130508       endif  ;
222200130424
222300130426       // Controllo se la TARGA MOTRICE è già presente in un traino decorrente
222400130426       //  nello stesso giorno
222500130508       if d05trm<>*blanks  ;
222600130508
222700130426       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtTRM='''+
222800130426        d05trm + ''''                                    ;
222900130426       EXEC sql  prepare s2 from : wrkgetlista ;
223000130426       EXEC sql  declare a2 cursor for s2      ;
223100130426       EXEC sql  open    a2                    ;
223200130426
223300130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
223400130426  1    dow sqlcod=0    ;
223500130426       wtipo='M'  ;
223600130426       Exsr ErrDOPP  ;
223700130426       if v1cmsg<>*blanks  ;
223800130426        leave  ;
223900130426       endif  ;
224000130426
224100130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
224200130426       enddo           ;
224300130426
224400130426       EXEC sql  CLOSE   a2                    ;
224500130426
224600130426       if v1cmsg<>*blanks  ;
224700130426        leavesr;
224800130426       endif  ;
224900130502
225000130502       endif  ;
225100130508       endif  ;
225200130424
225300130129       ENDSR   ;
225400130426       //--------------------------------------------------------------
225500130508       //?Errore per conducente/targa già presente nel giorno
225600130426       //--------------------------------------------------------------
225700130426       Begsr ErrDOPP  ;
225800130426
225900130426           if wtipo='C'  ;
226000130426            wmsg = Msg(15);
226100130426           else  ;
226200130426            wmsg = Msg(16);
226300130426           endif ;
226400130426
226500130426 2     if sql_fgttrn<> ktrn  and sql_fgttrn<>%int(d05trn)  ;
226600130426
226700130514 3     if d05gg1<>' '  and %subst(sql_fgtgse:1:1)<>' ' ;
226800130426 3a     if  (wfor2gg1=' ' and wtipo='C') or (wforMgg1=' ' and wtipo='M') ;
226900130426        // Verifico che il traino sia decorrente
227000130426        ktrn2=sql_fgttrn  ;
227100130426        exsr decorrTRN  ;
227200130426 4      if wok='S'      ;
227300130426           errgg1      = *on;
227400130426           errMessage  = *on;
227500130426           errGenerico = *on;
227600130426           V1cmsg = wmsg   ;
227700130426           %subst(v1cmsg:36:10)='  LUNEDÌ  ' ;
227800130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
227900130426           if wtipo='C'  ;
228000130426            wfor2gg1='S'  ;
228100130426           else  ;
228200130426            wforMgg1='S'  ;
228300130426           endif  ;
228400130426           leavesr;
228500130426 4     endif  ;
228600130426 3a    endif  ;
228700130426 3     endif  ;
228800130514 3     if d05gg2<>' '  and %subst(sql_fgtgse:2:1)<>' ' ;
228900130426 3a     if  (wfor2gg2=' ' and wtipo='C') or (wforMgg2=' ' and wtipo='M') ;
229000130426        // Verifico che il traino sia decorrente
229100130426        ktrn2=sql_fgttrn  ;
229200130426        exsr decorrTRN  ;
229300130426 4      if wok='S'      ;
229400130426           errgg2      = *on;
229500130426           errMessage  = *on;
229600130426           errGenerico = *on;
229700130426           V1cmsg = wmsg   ;
229800130426           %subst(v1cmsg:36:10)=' MARTEDÌ  ' ;
229900130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
230000130426           if wtipo='C'  ;
230100130426            wfor2gg2='S'  ;
230200130426           else  ;
230300130426            wforMgg2='S'  ;
230400130426           endif  ;
230500130426           leavesr;
230600130426 4     endif  ;
230700130426 3a    endif  ;
230800130426 3     endif  ;
230900130514 3     if d05gg3<>' '  and %subst(sql_fgtgse:3:1)<>' ' ;
231000130426 3a     if  (wfor2gg3=' ' and wtipo='C') or (wforMgg3=' ' and wtipo='M') ;
231100130426        // Verifico che il traino sia decorrente
231200130426        ktrn2=sql_fgttrn  ;
231300130426        exsr decorrTRN  ;
231400130426 4      if wok='S'      ;
231500130426           errgg3      = *on;
231600130426           errMessage  = *on;
231700130426           errGenerico = *on;
231800130426           V1cmsg = wmsg   ;
231900130426           %subst(v1cmsg:36:10)=' MERCOLEDÌ' ;
232000130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
232100130426           if wtipo='C'  ;
232200130426            wfor2gg3='S'  ;
232300130426           else  ;
232400130426            wforMgg3='S'  ;
232500130426           endif  ;
232600130426           leavesr;
232700130426 4     endif  ;
232800130426 3a    endif  ;
232900130426 3     endif  ;
233000130514 3     if d05gg4<>' '  and %subst(sql_fgtgse:4:1)<>' '  ;
233100130426 3a     if  (wfor2gg4=' ' and wtipo='C') or (wforMgg4=' ' and wtipo='M') ;
233200130426        // Verifico che il traino sia decorrente
233300130426        ktrn2=sql_fgttrn  ;
233400130426        exsr decorrTRN  ;
233500130426 4      if wok='S'      ;
233600130426           errgg4      = *on;
233700130426           errMessage  = *on;
233800130426           errGenerico = *on;
233900130426           V1cmsg = wmsg   ;
234000130426           %subst(v1cmsg:36:10)=' GIOVEDÌ  ' ;
234100130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
234200130426           if wtipo='C'  ;
234300130426            wfor2gg4='S'  ;
234400130426           else  ;
234500130426            wforMgg4='S'  ;
234600130426           endif  ;
234700130426           leavesr;
234800130426 4     endif  ;
234900130426 3a    endif  ;
235000130426 3     endif  ;
235100130514 3     if d05gg5<>' '  and %subst(sql_fgtgse:5:1)<>' '  ;
235200130426 3a     if  (wfor2gg5=' ' and wtipo='C') or (wforMgg5=' ' and wtipo='M') ;
235300130426        // Verifico che il traino sia decorrente
235400130426        ktrn2=sql_fgttrn  ;
235500130426        exsr decorrTRN  ;
235600130426 4      if wok='S'      ;
235700130426           errgg5      = *on;
235800130426           errMessage  = *on;
235900130426           errGenerico = *on;
236000130426           V1cmsg = wmsg   ;
236100130426           %subst(v1cmsg:36:10)=' VENERDÌ  ' ;
236200130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
236300130426           if wtipo='C'  ;
236400130426            wfor2gg5='S'  ;
236500130426           else  ;
236600130426            wforMgg5='S'  ;
236700130426           endif  ;
236800130426           leavesr;
236900130426 4     endif  ;
237000130426 3a    endif  ;
237100130426 3     endif  ;
237200130514 3     if d05gg6<>' '  and %subst(sql_fgtgse:6:1)<>' '  ;
237300130426 3a     if  (wfor2gg6=' ' and wtipo='C') or (wforMgg6=' ' and wtipo='M') ;
237400130426        // Verifico che il traino sia decorrente
237500130426        ktrn2=sql_fgttrn  ;
237600130426        exsr decorrTRN  ;
237700130426 4      if wok='S'      ;
237800130426           errgg6      = *on;
237900130426           errMessage  = *on;
238000130426           errGenerico = *on;
238100130426           V1cmsg = wmsg   ;
238200130426           %subst(v1cmsg:36:10)=' SABATO   ' ;
238300130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
238400130426           if wtipo='C'  ;
238500130426            wfor2gg6='S'  ;
238600130426           else  ;
238700130426            wforMgg6='S'  ;
238800130426           endif  ;
238900130426           leavesr;
239000130426 4     endif  ;
239100130426 3a    endif  ;
239200130426 3     endif  ;
239300130514 3     if d05gg7<>' '  and %subst(sql_fgtgse:7:1)<>' ' ;
239400130426 3a     if  (wfor2gg7=' ' and wtipo='C') or (wforMgg7=' ' and wtipo='M') ;
239500130426        // Verifico che il traino sia decorrente
239600130426        ktrn2=sql_fgttrn  ;
239700130426        exsr decorrTRN  ;
239800130426 4      if wok='S'      ;
239900130426           errgg7      = *on;
240000130426           errMessage  = *on;
240100130426           errGenerico = *on;
240200130426           V1cmsg = wmsg   ;
240300130426           %subst(v1cmsg:36:10)=' DOMENICA ' ;
240400130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
240500130426           if wtipo='C'  ;
240600130426            wfor2gg7='S'  ;
240700130426           else  ;
240800130426            wforMgg7='S'  ;
240900130426           endif  ;
241000130426           leavesr;
241100130426 4     endif  ;
241200130426 3a    endif  ;
241300130426 3     endif  ;
241400130426
241500130426 2     endif  ;
241600130426
241700130426       ENDSR  ;
241800130424       //--------------------------------------------------------------
241900130424       //?controlla decorrenza di un traino
242000130424       //--------------------------------------------------------------
242100130424       BEGSR DecorrTRN            ;
242200130424       clear wok                  ;
242300130424       setgt  ktrn2   tntlt01l    ;
242400130424       readpe ktrn2   tntlt01l    ;
242500130424 2     dow not %eof(tntlt01l)  ;
242600130424 3     if tltatb=' '  and w02dtn>=tltdde and w02dtn<=tltdsc ;
242700130424        wok='S'  ;
242800130424        leave  ;
242900130424 3     endif  ;
243000130424
243100130424       readpe ktrn2   tntlt01l    ;
243200130424 2     enddo   ;
243300130424       ENDSR   ;
243400130424
243500130422       //--------------------------------------------------------------
243600130422       //?Decodifica fermate iniziale e finale traino
243700130422       //--------------------------------------------------------------
243800130422        BEGSR DEcoTRNimm ;
243900130422
244000130422 4      if d05tfp  >0   ;
244100130422         chain d05tfp   azorg01l  ;
244200130422 5       if %found(azorg01l)  ;
244300130422         d05dtfp  =orgdes  ;
244400130422         else  ;
244500130422         d05dtfp  =*all'?'  ;
244600130422 5       endif  ;
244700130422 4      endif  ;
244800130422
244900130422 4      if d05tfa  >0   ;
245000130422         chain d05tfa   azorg01l  ;
245100130422 5       if %found(azorg01l)  ;
245200130422         d05dtfa  =orgdes  ;
245300130422         else  ;
245400130422         d05dtfa  =*all'?'  ;
245500130422 5       endif  ;
245600130422 4      endif  ;
245700130422       //--------------------------------------------------------------
245800130422       ENDSR  ;
245900130422        BEgsr DecoTRN_nw  ;
246000130422         chain d05tfpNW azorg01l  ;
246100130422 3       if %found(azorg01l)  ;
246200130422         d05dtfpnw=orgdes  ;
246300130422         else  ;
246400130422         d05dtfpnw=*all'?'  ;
246500130422 3       endif  ;
246600130422
246700130422         chain d05tfanw azorg01l  ;
246800130422 3       if %found(azorg01l)  ;
246900130422         d05dtfanw=orgdes  ;
247000130422         else  ;
247100130422         d05dtfanw=*all'?'  ;
247200130422 3       endif  ;
247300130422        ENDSR   ;
247400130313       //--------------------------------------------------------------
247500130313       //?Richiama pgm di ricerca traino per il terminal
247600130313       //--------------------------------------------------------------
247700130313       BEGSR ricercaTraino  ;
247800130313       clear tntl00ds  ;
247900130313       d00sca='C'   ;
248000130319       d00dde=w02dtn  ;
248100130313       d00fil= v01fgs  ;
248200130313
248300130313       kpjbu=tntl00ds  ;
248400130313       tntl17r (kpjba)  ;
248500130313       tntl00ds=kpjbu ;
248600130313       ENDSR  ;
248700130129       //--------------------------------------------------------------
248800130430       //?Aggiornoa richiesta singola    D05
248900130129       //--------------------------------------------------------------
249000130129       BEGSR AggiorD05 ;
249100130220
249200130313       // IMMISSIONE
249300130313       if Imm_d05=*on    ;
249400130430        clear fnfgtfis   ;
249500130313        fgtpru=knmus  ;
249600130313        fgtdim=datcor  ;
249700130313        fgttrn=%int(d05trn)  ;
249800130313       endif  ;
249900130313
250000130313       // COPIA
250100130313       if copiad05=*on  ;
250200130430        clear fnfgtfis   ;
250300130313        fgtpru=knmus  ;
250400130313        fgtdim=datcor  ;
250500130313        fgttrn=%int(d05trn_nw)  ;
250600130313       endif  ;
250700130313
250800130509       fgttfp=v01fgs ;
250900130220       fgtpdr=%int(d05pdr) ;
251000130220       fgtdcn=d05dcn  ;
251100130220       fgttrm=d05trm;
251200130220       fgttrr=d05trr;
251300130220       fgtnot=d05not ;
251400130628       fgtnt2=d05nt2 ;
251500130628       fgtan1=d05an1 ;
251600130628       fgtan2=d05an2 ;
251700130313       fgtgse=d05gg1+d05gg2+d05gg3+d05gg4+d05gg5+d05gg6+d05gg7  ;
251800130514             xi=1  ;
251900130514               for  xi = 1  to  %len( %trimr( fgtgse ) );
252000130514                 if  %subst( fgtgse : xi : 1 ) <>' ';
252100130514                   %subst(fgtgse:xi:1)=%editc(xi:'X') ;
252200130514                 endif;
252300130514               endfor;
252400130220
252500130313       if Imm_d05=*on  or Copiad05=*on ;
252600130220       write fnfgtfis  ;
252700130220       endif  ;
252800130220
252900130220       if   ManutD05=*on  ;
253000130220       update fnfgtfis  ;
253100130220       endif   ;
253200130313
253300130313       if   spostad05=*on  ;
253400130313        fgttrn=%int(d05trn_nw)  ;
253500130313       update fnfgtfis  ;
253600130313       endif   ;
253700130129
253800130129           $Inzs02=*on  ;
253900130129
254000130129       ENDSR  ;
254100130417
254200130417       //--------------------------------------------------------------
254300130429       //?elabora SFL per scelta proposte
254400130417       //--------------------------------------------------------------
254500130417           BEGSR ElaboraSFL   ;
254600130417
254700130417           clear s02sce  ;
254800130417           clear s02sce_prot ;
254900130418           clear unascelta ;
255000130417
255100130417 0         if wglobale=' '  ;
255200130417
255300130417           readc lst1s02;
255400130417 1         DOW  NOT  %eof(fnlst1D) and $Fine=*off ;
255500130417
255600130417 2         if    s02sce= '1'  ;
255700130417 3           if ilst1ges='R'  ;
255800130417              olst1TRN =%editc(s02trn :'X')   ;
255900130417              olst1nrrp=%editc(h02nrrp:'X')  ;
256000130417              olst1OPZIO='1'  ;
256100130417              $Fine = *on;
256200130417              kpjbu=fnlst1ds  ;
256300130417              leave    ;
256400130417
256500130417 x3          else  ;
256600130417
256700130417             exsr AperturaFGV  ;
256800130418             unascelta='1'  ;
256900130417
257000130418  4          if olst4msg<>*blanks  ;
257100130417              errScelta   = *on;
257200130417              errMessage  = *on;
257300130417              errGenerico = *on;
257400130417              V1cmsg = olst4msg  ;
257500130418  4          endif  ;
257600130418  3          endif  ;
257700130417              exsr errSFL2  ;
257800130417 2        endif  ;
257900130417
258000130417            readc  lst1s02   ;
258100130417 1     enddo  ;
258200130418
258300130418       // se non ho fatto nessuna scelta e sono nella apertura
258400130418       //  totale --> apro senza trazionista/proposta
258500130418 1        if Unascelta=' ' and ilst1aut='S'  ;
258600130418            clear h02nrrp  ;
258700130418            clear s02pdr  ;
258800130418             exsr AperturaFGV  ;
258900130418 1        endif  ;
259000130417
259100130417 0     else  ;
259200130417
259300130417       // Scelta globale --> leggo tutto il slf
259400130417       //  non ancora aperti
259500130417
259600130417       eval   s02nrr=1  ;
259700130417       chain   s02nrr lst1s02;
259800130418 1     dow  %found           ;
259900130417
260000130417 3           if ilst1ges='R'  ;
260100130417              olst1TRN =%editc(s02trn :'X')   ;
260200130417              olst1nrrp=%editc(h02nrrp:'X')  ;
260300130417              olst1OPZIO='1'  ;
260400130417              $Fine = *on;
260500130417              kpjbu=fnlst1ds  ;
260600130417              leave    ;
260700130417
260800130417 x3          else  ;
260900130417
261000130417             exsr AperturaFGV  ;
261100130417
261200130418  4          if olst4msg<>*blanks  ;
261300130417              errScelta   = *on;
261400130417              errMessage  = *on;
261500130417              errGenerico = *on;
261600130417              V1cmsg = olst4msg  ;
261700130418  4          endif  ;
261800130418  3          endif  ;
261900130417              exsr errSFL2  ;
262000130429
262100130417         s02nrr=s02nrr+1  ;
262200130417         chain   s02nrr lst1s02;
262300130417  1    enddo  ;
262400130417
262500130417 0     endif  ;
262600130417
262700130417       ENDSR  ;
262800130417
262900130417       //--------------------------------------------------------------
263000130417       //?Richiamo pgm per apertua fogli viaggio
263100130417       //--------------------------------------------------------------
263200130417       BEGSR AperturaFGV  ;
263300130417
263400130417              clear fnlst4ds  ;
263500130417              ilst4ges='A'  ;
263600130417              ilst4fgs =%editc(v01fgs:'X')    ;
263700130417              ilst4TRN =%editc(s02trn :'X')   ;
263800130418 1            if h02nrrp>0  ;
263900130417                ilst4nrrp=%editc(h02nrrp:'X')  ;
264000130418 x1           else  ;
264100130418                ilst4nrrp=*all'0'  ;
264200130417
264300130417                // Se non si tratta di richiamo automatico ma scelta
264400130429                //  a video la passo altrimenti vuoto per far scegliere
264500130417                //  dopo
264600130417                ilst4pdr=%editc(s02pdr:'X')   ;
264700130502              //   ilst4old='S'  ;
264800130418  1           endif  ;
264900130417              ilst4dfv=w02dtn  ;
265000130429
265100130429              // Se premuto F6 passo al chiamante
265200130429              if dsp_aid = c_F06;
265300130429               iolst4for='F6 '  ;
265400130429              endif  ;
265500130429
265600130417              kpjbu=fnlst4ds    ;
265700130417              callp  FNLST4R (kpjba)   ;
265800130417              fnlst4ds=kpjbu   ;
265900130417
266000130429             if iolst4for= *blanks ;
266100130429              clear s02sce   ;
266200130429             endif  ;
266300130417       ENDSR  ;
266400130417       //--------------------------------------------------------------
266500130417       //?Apertura automatica dei fogli viaggio traino oper traino
266600130417       //--------------------------------------------------------------
266700130417       BEGSR creaAUT      ;
266800130417       if s02nrr=1  ;
266900130418        exsr AperturaFGV ;
267000130417       else   ;
267100130417       // emetto SFL per la scelta
267200130418           C02rcd  = 1;
267300130418           C02csr  = 1;
267400130418           $InzS02  = *off;
267500130418           ctl02_prot=*on   ;
267600130418
267700130418           exsr Gess02  ;
267800130418
267900130418           // controllo cosa scelto: se nulla --> apro senza trazionista
268000130418           if   $Video = 'CA';
268100130418             exsr    ElaboraSFL   ;
268200130503           else  ;
268300130503            clear h02nrrp  ;
268400130503            clear s02pdr   ;
268500130503            exsr AperturaFGV ;
268600130418           endif  ;
268700130418       endif  ;
268800130418
268900130418           ctl02_prot=*off  ;
269000130418           exsr Pulizs02  ;
269100130417
269200130417       ENDSR  ;
269300130417       //--------------------------------------------------------------
269400130417       //?Scelta su singolo traino: proposta o trazionista
269500130417       //--------------------------------------------------------------
269600130417       BEGSR SceltasingTRN;
269700130417       clear s02sce   ;
269800130417       exsr errsfl2  ;
269900130417       ENDSR  ;
270000080206       //--------------------------------------------------------------
270100080206       //?Operazioni finali.
270200080206       //--------------------------------------------------------------
270300080206       BEGSR RoutEnd;
270400080410
270500130502       if $Video='CA' and s02sce='1' and ilst1ges='R' ;
270600130315         *inrt=*on  ;
270700130315         else  ;
270800080206         *inLR = *on;
270900130315         endif  ;
271000130315
271100080206         return;
271200080206
271300080206       ENDSR;
271400130620       //--------------------------------------------------------------
271500130620       //?Elaborazione SFL di work per la gestione dell F15=Immissione tot
271600130620       //--------------------------------------------------------------
271700130620       BEGSR sr_elabs0w;
271800130620       s02nrw=1;
271900130620       clear savtrnw;
272000130620       chain s02nrw lst1s0W;
272100130620       dow %found;
272200130620        // A cambio di traino richiamo la creazione del foglio viaggio
272300130620          if s02trn<>savtrnw and savtrnw>0;
272400130620             s02nrw-=1;
272500130620             chain s02nrw lst1s0W;
272600130620             exsr CreaAUT;
272700130620             s02nrw+=1;
272800130620             chain s02nrw lst1s0W;
272900130620          endif;
273000130620          savtrnw=s02trn;
273100130620          S02nrr += 1;
273200130620          write lst1S02;
273300130620          s02nrw+=1;
273400130620          chain s02nrw lst1s0W;
273500130620       enddo;
273600130620          s02nrw-=1;
273700130620          chain s02nrw lst1s0W;
273800130620          exsr CreaAUT;
273900130620       endsr;
274000080206
274100080206      /end-free
274200130419     C*-----------------------------------------------------***********
274300130419     C* CONTROLLO NUMERO TRAINO                             * CTRTRN  *
274400130419     C*-----------------------------------------------------***********
274500130419     C     CTRTRN        BEGSR
274600130419     C*
274700130419     c* controllo se inserito un numero
274800130419     c                   testn                   alfaTRN              30
274900130419     c                   movel     alfaTRN       alfaUNO           1
275000130419    2c                   if        not *in30 or alfaUNO<'0'
275100130419     C                   MOVEL     MSG(8)        V1cMSG
275200130422     c                   eval      errMessage  = *on
275300130422     c                   eval      errGenerico = *on
275400130419     C                   GOTO      ENDTRN
275500130419    2C                   ENDIF
275600130419     c
275700130419     c                   movel     alfaTRN       ktrn              7 0
275800130419     c
275900130419     C     ktrn          SETLL     TNTLT01L
276000130422     C     ktrn          READE     TNTLT01L
276100130419     C*
276200130419    2c                   dow       not %eof(tntlt01l)
276300130422     c                   if        tltatb=' ' and
276400130422     c                             w02dtn>=tltdde and w02dtn<=tltdsc
276500130422     c                   leave
276600130422     c                   else
276700130422     C     ktrn          READE     TNTLT01L
276800130422     c                   endif
276900130422     c                   enddo
277000130422     c
277100130422     c* Errore : traino non trovat o
277200130422     c                   if        %eof(tntlt01l)
277300130422     C                   MOVEL     MSG(8)        V1cMSG
277400130422     c                   eval      errMessage  = *on
277500130422     c                   eval      errGenerico = *on
277600130422     C                   GOTO      ENDTRN
277700130422     c                   endif
277800130422     c
277900130422     C*   Errore: traino  "A-AFFLUENZA"
278000130419    4C     TLTTTR        IFEQ      'A'
278100130419     C                   MOVEL     MSG(8)        V1cMSG
278200130422     c                   eval      errMessage  = *on
278300130422     c                   eval      errGenerico = *on
278400130419     C                   GOTO      ENDTRN
278500130422     c                   endif
278600130422     c
278700130422     c* controllo se la filiale gestione è una fermata di carico
278800130422     c                   exsr      contr_ferm
278900130422     c
279000130422     c                   if        cartrn=' '
279100130422     C                   MOVEL     MSG(9)        V1cMSG
279200130422     c                   eval      errMessage  = *on
279300130422     c                   eval      errGenerico = *on
279400130422     C                   GOTO      ENDTRN
279500130422     c                   endif
279600130419     C     ENDTRN        ENDSR
279700130617      *-------------------------------------------------------------------------
279800130617      * ordinamento sfl in base alle richieste
279900130617      *-------------------------------------------------------------------------
280000130617     C     sr_ordina     BEGSR
280100130617      * IMPOSTO ENTRATA AD 'A'
280200130624     c                   eval      C02rcd  = 1
280300130624     c                   eval      C02csr  = 1
280400130624     c                   clear                   d02trn
280500130617     C                   EVAL      s02nrr = 0
280600130617      *
280700130617     C                   CLEAR                   QLGSCB
280800130617     C                   CLEAR                   QLGSCB00
280900130617
281000130617     c                   select
281100130617     c* ordinamento per n.traino
281200130617     c                   when      w_ord=1
281300130617     ** 1 campo chiave.
281400130617     C                   EVAL      QLGNBRK    = 1
281500130617     C                   EVAL      QLGSP      = 1
281600130617     C                   EVAL      QLGSS      = %SIZE(s02trn)
281700130617     C                   EVAL      QLGDT      = carattere
281800130617     C                   EVAL      QLGSO      = Ascendente
281900130617     C                   EVAL      QLGKL(1)   = QLGSKL
282000130617     c* ordinamento per data/ora partenza
282100130617     c                   when      w_ord=2
282200130617     ** 2 campi chiave.
282300130617     C                   EVAL      QLGNBRK    = 2
282400130617     C                   EVAL      QLGSP      = 1 + %size(s02trn)
282500130617     C                   EVAL      QLGSS      = %SIZE(s02ggp)
282600130617     C                   EVAL      QLGDT      = carattere
282700130617     C                   EVAL      QLGSO      = Ascendente
282800130617     C                   EVAL      QLGKL(1)   = QLGSKL
282900130617     ** ragione sociale a posizione 1, carattere, 24 byte,
283000130617     C                   EVAL      QLGSP      = 1 + %size(s02trn) +
283100130617     c                                          %size(s02ggp)
283200130617     C                   EVAL      QLGSS      = %SIZE(s02hmp)
283300130617     C                   EVAL      QLGDT      = carattere
283400130617     C                   EVAL      QLGSO      = Ascendente
283500130617     C                   EVAL      QLGKL(2)   = QLGSKL
283600130617     c                   endsl
283700130617
283800130617      * Load other sort parameters.
283900130617     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
284000130617     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
284100130617     C                   EVAL      QLGRT     = 8
284200130617     C                   EVAL      QLGOKL    = 80
284300130617     C                   EVAL      QLGLKE    = 16
284400130617     C                   EVAL      QLGLSS    = 290
284500130617
284600130617      * Initialize Sort I/O API fields.
284700130617     C                   EVAL      QLGRL00  = QLGRL
284800130617     C                   EVAL      QLGRC00  = 1
284900130617     C                   CLEAR                   QUSEI
285000130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
285100130617
285200130617      * First step - Initialize the sort routine.
285300130617     C                   CALL      'QLGSORT'
285400130617     C                   PARM                    QLGSCB
285500130617     C                   PARM                    NotUsed
285600130617     C                   PARM                    NotUsed
285700130617     C                   PARM                    SizeList
285800130617     C                   PARM                    ReturnSize
285900130617     C                   PARM                    QUSEC
286000130617
286100130617      * Next step - Write records to I/O routine.
286200130617     C                   EVAL      QLGRT00 = Put
286300130617
286400130617     C                   DO        RrnLast       s02nrr
286500130617
286600130617     C     s02nrr        CHAIN     lst1s02
286700130617     c                   if        not %found
286800130617     c                   leave
286900130617     c                   endif
287000130617     ** Solo le righe con Selected = 'Y' sono riordinate,
287100130617     ** quindi per fare un ordinamento di tutte le righe
287200130617     ** metto 'Y' sempre.
287300130617     C                   EVAL      Selected = 'Y'
287400130617
287500130617     C                   CLEAR                   QUSEI
287600130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
287700130617     C                   CALL      'QLGSRTIO'
287800130617     C                   PARM                    QLGSCB00
287900130617     C                   PARM                    SflRcd
288000130617     C                   PARM                    NotUsed
288100130617     C                   PARM                    SizeList
288200130617     C                   PARM                    NotUsed
288300130617     C                   PARM                    QUSEC
288400130617
288500130617     C                   ENDDO
288600130617
288700130617      * Next step - Signal end of input, clear subfile for reload.
288800130617     C                   EVAL      QLGRT00 = EndPut
288900130617     C                   CLEAR                   QUSEI
289000130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
289100130617
289200130617     C                   CALL      'QLGSRTIO'
289300130617     C                   PARM                    QLGSCB00
289400130617     C                   PARM                    SflRcd
289500130617     C                   PARM                    NotUsed
289600130617     C                   PARM                    SizeList
289700130617     C                   PARM                    NotUsed
289800130617     C                   PARM                    QUSEC
289900130617
290000130617     c                   eval      SflDsp_N= *on
290100130617     c                   eval      SflDspCtl_N = *on
290200130617     C                   WRITE     lst1c02
290300130617     c                   eval      SflDspCtl_N = *off
290400130617     c                   eval      SflEnd      = *off
290500130617     c                   write     lst1p02
290600130617
290700130617      * Final step - Write the records back to the subfile.
290800130617     C                   EVAL      QLGRT00 = Get
290900130617     C                   DO        RrnLast       s02nrr
291000130617     C                   CLEAR                   QUSEI
291100130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
291200130617     C                   CALL      'QLGSRTIO'
291300130617     C                   PARM                    QLGSCB00
291400130617     C                   PARM                    NotUsed
291500130617     C                   PARM                    SflRcd
291600130617     C                   PARM                    QLGRL00
291700130617     C                   PARM                    NotUsed
291800130617     C                   PARM                    QUSEC
291900130617      *
292000130617     c                   eval      notrn=h02notrn
292100130617     c                   if        h02imm='S'
292200130617     c                   eval      Sott_fgt=*off
292300130617     c                   else
292400130617     c                   eval      Sott_fgt=*on
292500130617     c                   endif
292600130617     c                   if        s02aper='Sb'
292700130617     c                   eval      s02aper_rev=*on
292800131009     c                   else
292900131009     c                   eval      s02aper_rev=*off
293000130617     c                   endif
293100130617     C                   WRITE     lst1s02
293200130617     C                   ENDDO
293300130617
293400130617      *
293500130617     C                   ENDSR
293600130620      *-------------------------------------------------------------------------
293700130620      * ordinamento sfl di work in base alle richieste
293800130620      *-------------------------------------------------------------------------
293900130620     C     sr_ordinaW    BEGSR
294000130620      * IMPOSTO ENTRATA AD 'A'
294100130620     C                   EVAL      s02nrw = 0
294200130624     c                   clear                   d02trn
294300130620      *
294400130620     C                   CLEAR                   QLGSCB
294500130620     C                   CLEAR                   QLGSCB00
294600130620
294700130620     c                   select
294800130620     c* ordinamento per n.traino
294900130620     c                   when      w_ord=1
295000130620     ** 1 campo chiave.
295100130620     C                   EVAL      QLGNBRK    = 1
295200130620     C                   EVAL      QLGSP      = 1
295300130620     C                   EVAL      QLGSS      = %SIZE(s02trn)
295400130620     C                   EVAL      QLGDT      = carattere
295500130620     C                   EVAL      QLGSO      = Ascendente
295600130620     C                   EVAL      QLGKL(1)   = QLGSKL
295700130620     c* ordinamento per data/ora partenza
295800130620     c                   when      w_ord=2
295900130620     ** 2 campi chiave.
296000130620     C                   EVAL      QLGNBRK    = 2
296100130620     C                   EVAL      QLGSP      = 1 + %size(s02trn)
296200130620     C                   EVAL      QLGSS      = %SIZE(s02ggp)
296300130620     C                   EVAL      QLGDT      = carattere
296400130620     C                   EVAL      QLGSO      = Ascendente
296500130620     C                   EVAL      QLGKL(1)   = QLGSKL
296600130620     ** ragione sociale a posizione 1, carattere, 24 byte,
296700130620     C                   EVAL      QLGSP      = 1 + %size(s02trn) +
296800130620     c                                          %size(s02ggp)
296900130620     C                   EVAL      QLGSS      = %SIZE(s02hmp)
297000130620     C                   EVAL      QLGDT      = carattere
297100130620     C                   EVAL      QLGSO      = Ascendente
297200130620     C                   EVAL      QLGKL(2)   = QLGSKL
297300130620     c                   endsl
297400130620
297500130620      * Load other sort parameters.
297600130620     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
297700130620     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
297800130620     C                   EVAL      QLGRT     = 8
297900130620     C                   EVAL      QLGOKL    = 80
298000130620     C                   EVAL      QLGLKE    = 16
298100130620     C                   EVAL      QLGLSS    = 290
298200130620
298300130620      * Initialize Sort I/O API fields.
298400130620     C                   EVAL      QLGRL00  = QLGRL
298500130620     C                   EVAL      QLGRC00  = 1
298600130620     C                   CLEAR                   QUSEI
298700130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
298800130620
298900130620      * First step - Initialize the sort routine.
299000130620     C                   CALL      'QLGSORT'
299100130620     C                   PARM                    QLGSCB
299200130620     C                   PARM                    NotUsed
299300130620     C                   PARM                    NotUsed
299400130620     C                   PARM                    SizeList
299500130620     C                   PARM                    ReturnSize
299600130620     C                   PARM                    QUSEC
299700130620
299800130620      * Next step - Write records to I/O routine.
299900130620     C                   EVAL      QLGRT00 = Put
300000130620
300100130620     C                   DO        RrnLast       s02nrw
300200130620
300300130620     C     s02nrw        CHAIN     lst1s0w
300400130620     c                   if        not %found
300500130620     c                   leave
300600130620     c                   endif
300700130620     ** Solo le righe con Selected = 'Y' sono riordinate,
300800130620     ** quindi per fare un ordinamento di tutte le righe
300900130620     ** metto 'Y' sempre.
301000130620     C                   EVAL      Selected = 'Y'
301100130620
301200130620     C                   CLEAR                   QUSEI
301300130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
301400130620     C                   CALL      'QLGSRTIO'
301500130620     C                   PARM                    QLGSCB00
301600130620     C                   PARM                    SflRcd
301700130620     C                   PARM                    NotUsed
301800130620     C                   PARM                    SizeList
301900130620     C                   PARM                    NotUsed
302000130620     C                   PARM                    QUSEC
302100130620
302200130620     C                   ENDDO
302300130620
302400130620      * Next step - Signal end of input, clear subfile for reload.
302500130620     C                   EVAL      QLGRT00 = EndPut
302600130620     C                   CLEAR                   QUSEI
302700130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
302800130620
302900130620     C                   CALL      'QLGSRTIO'
303000130620     C                   PARM                    QLGSCB00
303100130620     C                   PARM                    SflRcd
303200130620     C                   PARM                    NotUsed
303300130620     C                   PARM                    SizeList
303400130620     C                   PARM                    NotUsed
303500130620     C                   PARM                    QUSEC
303600130620
303700130620     c                   eval      SflDspw_N= *on
303800130620     c                   eval      SflDspCtlw_N = *on
303900130620     C                   WRITE     lst1c0w
304000130620     c                   eval      SflDspCtlw_N = *off
304100130620
304200130620      * Final step - Write the records back to the subfile.
304300130620     C                   EVAL      QLGRT00 = Get
304400130620     C                   DO        RrnLast       s02nrw
304500130620     C                   CLEAR                   QUSEI
304600130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
304700130620     C                   CALL      'QLGSRTIO'
304800130620     C                   PARM                    QLGSCB00
304900130620     C                   PARM                    NotUsed
305000130620     C                   PARM                    SflRcd
305100130620     C                   PARM                    QLGRL00
305200130620     C                   PARM                    NotUsed
305300130620     C                   PARM                    QUSEC
305400130620      *
305500130620     c                   eval      notrn=h02notrn
305600130620     c                   if        h02imm='S'
305700130620     c                   eval      Sott_fgt=*off
305800130620     c                   else
305900130620     c                   eval      Sott_fgt=*on
306000130620     c                   endif
306100130620     c                   if        s02aper='Sb'
306200130620     c                   eval      s02aper_rev=*on
306300130620     c                   endif
306400130620     C                   WRITE     lst1s0w
306500130620     C                   ENDDO
306600130620
306700130620      *
306800130620     C                   ENDSR
306900080206
307000080206       //--------------------------------------------------------------
307100080206       //?Schiere a tempo di compilazione.
307200080206       //--------------------------------------------------------------
307300080206
307400080410** - MSG ------------------------------------------------------------------ -*
307500130121Data errata
307600130220Proposta ancora da inserire: possibile solo opzione "I-inserimento"
307700130514Immettere una "S" per indicare che la partenza è di xxxxxxxxxx
307800130423Immettere  i giorni di partenza
307900130314Opzione non ammmessa
308000130315Se premuto F7 non effettuare scelte con l'opzione "1"                         6
308100130315Utente non abilitato: chiamare CED di sede !!                                 7
308200130412Numero traino inesistente o non decorrente alla data specificata              8
308300130422La filiale gestione non è una fermata di carico. Traino errato                9
308400130424Codice trazionista errato                                                     10
308500130424Conducente non accreditato per il trazionista indicato                        11
308600130424Il conducente  ha terminato il rapporto col trazionista                       12
308700130424Targa MOTRICE non accredita per il trazionista indicato                       13
308800130424Esiste già una proposta nel giorno di xxxxxxxxxx x questo traino:ENTER forza  14
308900130424Conducente già presente nel giorno xxxxxxxxxx per traino xxxxxxx:ENTER forza  15
309000130426TargaMOTRICE già presente nel gior.xxxxxxxxxx per traino xxxxxxx:ENTER forza  16
309100130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare17
309200130502Il trazionista non è previsto nel traino indicato                             18
309300130503Il giorno non è previsto dal traino                                           19
