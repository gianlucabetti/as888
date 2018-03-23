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
001600130614     ftntlo01L  if   e           k disk
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
003900130620     f                                     sfile(lst1S0W : S02nrw)
004000080206
004100080206      //---------------------------------------------------------------
004200080206      //?Definizione costanti.
004300080206      //---------------------------------------------------------------
004400130618     d ord_traino      c                   'F11=Ord per n.Traino '
004500130618     d ord_dtaorp      c                   'F11=Ord per data/ora part'
004600130617     d w_ntraino       C                   1
004700130617     d w_dtaorp        C                   2
004800130617     d W_ord           S              1  0
004900130617      *---------------------------------------------------------------------
005000130617      * specifiche per ordinamento forzato sfl
005100130617     d NotUsed         S             16A
005200130617     d ReturnSize      S              9B 0
005300130617     d SizeList        S              9B 0
005400130617     D RrnLast         S              5I 0
005500130617     d*---
005600130617     d MaxKey          C                   2
005700130617     d Ascendente      C                   1
005800130617     d Discendente     C                   2
005900130617     d Carattere       C                   6
006000130617     d Put             C                   1
006100130617     d EndPut          C                   2
006200130617     d Get             C                   3
006300130617     d Numerico        C                   8
006400080411      // - Numero di record in ciascuna videata di subfile
006500130122     d c_SflPag        c                   const(16)
006600080411     d W_SflPag        s              3  0 inz
006700080530     d totrig          s              5  0 inz
006800130122     d ricInt          s              1
006900080207
007000080207      // - Tasti funzionali a video
007100080207     d c_F01           c                   const(x'31')
007200080207     d c_F02           c                   const(x'32')
007300080207     d c_F03           c                   const(x'33')
007400080207     d c_F05           c                   const(x'35')
007500080207     d c_F06           c                   const(x'36')
007600080207     d c_F07           c                   const(x'37')
007700080207     d c_F08           c                   const(x'38')
007800080207     d c_F09           c                   const(x'39')
007900080207     d c_F10           c                   const(x'3A')
008000130617     d c_F11           c                   const(x'3B')
008100080207     d c_F12           c                   const(x'3C')
008200080207     d c_F13           c                   const(x'B1')
008300080207     d c_F14           c                   const(x'B2')
008400080207     d c_F15           c                   const(x'B3')
008500080207     d c_F16           c                   const(x'B4')
008600080207     d c_F17           c                   const(x'B5')
008700080207     d c_F18           c                   const(x'B6')
008800080207     d c_F19           c                   const(x'B7')
008900080207     d c_F20           c                   const(x'B8')
009000080207     d c_F21           c                   const(x'B9')
009100080207     d c_F22           c                   const(x'BA')
009200080207     d c_F23           c                   const(x'BB')
009300080207     d c_F24           c                   const(x'BC')
009400080207     d c_Enter         c                   const(x'F1')
009500080207     d c_RollDown      c                   const(x'F4')
009600080207     d c_RollUp        c                   const(x'F5')
009700080214
009800080214      // - Attributi di visualizzazione
009900080215      //  -  DspAtr() - Normale
010000080214     d C_dspatr_Norm   c                   const(x'20')
010100080215      //  -  DspAtr(RI)
010200080214     d C_dspatr_RI     c                   const(x'21')
010300080215      //  -  DspAtr(ND)
010400080214     d C_dspatr_ND     c                   const(x'27')
010500080215      //  -  DspAtr(BL) / Color(Red)
010600080214     d C_dspatr_BL     c                   const(x'28')
010700080206
010800080206      //---------------------------------------------------------------
010900080206      //?Definizione schiere.
011000080206      //---------------------------------------------------------------
011100080206
011200080206      // - Messaggi di errore
011300130503     d MSG             s             78    dim(19) ctdata perrcd(1)
011400080609
011500080206      //---------------------------------------------------------------
011600080206      //?Definizione aree dati.
011700080206      //---------------------------------------------------------------
011800080206
011900080206      // - Dati utente
012000080206     d §AzUte        e ds                  extname(AZUTE00F)
012100080206     d                                     dtaara
012200080206     d §DatiUte      e ds                  extname(dDatiUte)
012300080206     d                                     dtaara
012400080206
012500080206      //---------------------------------------------------------------
012600080206      //?Definizione strutture dati.
012700080206      //---------------------------------------------------------------
012800080206
012900080206      // - Status
013000080206     d Psds           sds
013100080206     d   SDSpgm          *proc
013200080206
013300080206      // - InfDS
013400080206     d InfDspF         ds
013500080207     d  dsp_aid              369    369a
013600080207     d  sfl_rrn              376    377i 0
013700080207     d  min_nrr              378    379i 0
013800080207     d  num_rcds             380    381i 0
013900080206
014000080206      // - Indicatori su DspF
014100080208     d IndDspF         ds
014200080206        // - Indicatori di gestione del subfile
014300130220     d  NoTRN                         1n   overlay(IndDspF : 02)
014400130220     d  ProtD05                       1n   overlay(IndDspF : 03)
014500130130     d  InterD05                      1n   overlay(IndDspF : 04)
014600130313     d  CopiaD05                      1n   overlay(IndDspF : 05)
014700130220     d  spostad05                     1n   overlay(IndDspF : 06)
014800130220     d  ManutD05                      1n   overlay(IndDspF : 07)
014900130220     d  Imm_d05                       1n   overlay(IndDspF : 08)
015000130221     d  Sott_fgt                      1n   overlay(IndDspF : 09)
015100130314     d  Interrogaz                    1n   overlay(IndDspF : 10)
015200130314     d  Ricerca_PRP                   1n   overlay(IndDspF : 11)
015300130412     d  S02aper_REV                   1n   overlay(IndDspF : 12)
015400130417     d  S02sce_prot                   1n   overlay(IndDspF : 13)
015500130418     d  CTL02_prot                    1n   overlay(IndDspF : 14)
015600130314     d
015700080626     d  SflDsp_N                      1n   overlay(IndDspF : 30)
015800080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
015900080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
016000080206     d  SflEnd                        1n   overlay(IndDspF : 33)
016100130218     d  SflEnd3                       1n   overlay(IndDspF : 34)
016200130620        // sfl di work per gestione F15
016300130620     d  SflDspw_N                     1n   overlay(IndDspF : 38)
016400130620     d  SflDspCtlw_N                  1n   overlay(IndDspF : 39)
016500080206        // - Indicatori di errore
016600080206     d  errMessage                    1n   overlay(IndDspF : 28)
016700130313     d  Errgg1                        1n   overlay(IndDspF : 41)
016800130313     d  Errgg2                        1n   overlay(IndDspF : 42)
016900130313     d  Errgg3                        1n   overlay(IndDspF : 43)
017000130313     d  Errgg4                        1n   overlay(IndDspF : 44)
017100130313     d  Errgg5                        1n   overlay(IndDspF : 45)
017200130313     d  Errgg6                        1n   overlay(IndDspF : 46)
017300130313     d  Errgg7                        1n   overlay(IndDspF : 47)
017400130313     d  ErrScelta                     1n   overlay(IndDspF : 48)
017500130315     d  ErrDTN                        1n   overlay(IndDspF : 50)
017600130412     d  ErrTRN                        1n   overlay(IndDspF : 51)
017700130419     d  ErrTRN_imm                    1n   overlay(IndDspF : 52)
017800130422     d  ErrTRN_NW                     1n   overlay(IndDspF : 53)
017900130422     d  Errpdr                        1n   overlay(IndDspF : 54)
018000130422     d  Errdcn                        1n   overlay(IndDspF : 55)
018100130422     d  Errtrm                        1n   overlay(IndDspF : 56)
018200080606     d  errGenerico                   1n   overlay(IndDspF : 99)
018300130220
018400130617     d SflRcd          DS
018500130617     d  s02trn
018600130617     d  s02ggp
018700130617     d  s02hmp
018800130617     d  h02imm
018900130617     d  h02dde
019000130617     d  h02notrn
019100130617     d  H02NRRP
019200130617     d  h02ttr
019300130617     d  s02tfp
019400130617     d  s02tfa
019500130617     d  s02dtfa
019600130617     d  s02aper
019700130617     d  s02dpdr
019800130617     d  s02dcn
019900130617     d  s02set
020000130617     d  s02pdr
020100130617     d  s02trm
020200130617     d  s02trr
020300130617     d  Selected                      1A
020400130220     D FGTD1           DS
020500130220     D  FGTNRR               397    400B 0
020600080206
020700080206      // - Parametri ricevuti
020800080206     d KPJBA         e ds
020900080206
021000080206      // - Reperimento dati utente
021100080206     d TIBS34ds      e ds
021200080206     d dUte01        e ds
021300080206
021400080206      // - Ricerca/Controllo tabelle
021500130313     d tntl00ds      e ds                  inz
021600130313     d TIBS02ds      e ds                  inz
021700130123     d trul33ds      e ds                  inz
021800130422     d ds3I          e ds                  inz
021900080206
022000080206      // - Tabella LAT = Livello autorità x singola funzione aziendale
022100080206     d dLAT          e ds                  inz
022200130122     D TRUL31DS      E DS
022300130122     d POG                            3  0 DIM(250) overlay(o31pog)
022400130122     d
022500130122     d dstu          e ds                  inz
022600130618     d dppa          e ds                  inz
022700130314     d
022800130121     D WLBDAT          DS
022900130121     D  G02DAT                 1      8  0
023000130121     D  G02INV                 9     16  0
023100130121     D  G02ERR                17     17
023200130121     D  G02TGI                18     22  0
023300130313     d
023400130123     D                 DS
023500130123     D  kaaa                   1      4  0
023600130123     D  kmmm                   5      6  0
023700130123     D  kgg                    7      8  0
023800130123     D  kDATA                  1      8  0
023900130123     d
024000130123     D clnmat          DS
024100130123     D  MAT                    1     31    dim(31)
024200130123     D clnpom          DS
024300130123     D  pom                    1     31    dim(31)
024400130123     D                 DS
024500130314     d
024600130314     D FNLST1DS        DS
024700130314     d* ' ' --> gestione proposte
024800130315     d* 'R' --> ricerca  proposte
024900130315     d* 'A' --> ricerca proposte per apertura FGV
025000130314     d* 'I' --> Interrogazione proposte
025100130314     D  Ilst1GES               1      1
025200130314     D  Ilst1DAT               2      9
025300130314     d
025400130314     D* 'G' --> caricare solo le proposte traini del giorno della settimana
025500130314     D*         corrispondente alla data;
025600130314     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
025700130314     D  Ilst1TUT              10     10
025800130314     D  Ilst1fgs              11     13
025900130417     D  Ilst1aut              14     14
026000130315     d
026100130315     d* Campi di output
026200130315     d*  S=apertura dei traini non ancora aperti
026300130315     d*  1=opzione di scelta '1' da cercare in tutte le proposte a video
026400130315     d  Olst1OPZIO            21     21
026500130315     d* per opzione '1'
026600130315     d  Olst1TRN              22     28
026700130315     d* Num relativo record della proposta FNFGT
026800130315     d*  se = *blanks  non c'e'  proposta
026900130315     d  Olst1NRRP             21     33
027000130417     d* E=errore nessun trino caricato
027100130417     d  Olst1ERR              34     34
027200130315     d
027300130315     d
027400130318     D fnlst4ds        DS
027500130318     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
027600130318     D*          B - DA BORDERO'
027700130318     D*          A - apertura foglio
027800130318     D*          C - DA CHIUSURA FV SINGOLA
027900130318     D*          I - DA CHIUSURA DA IMP
028000130318     D*          N - DA INTERROGAZIONE  FV
028100130318     D*          A - apertura fogli viaggio
028200130319     D  ilst4DFV               3     10  0
028300130318     D  ilst4nfv              11     15
028400130318     D  IlsT4GES              16     16
028500130318     D* Se PARMSG pieno --> c'è stato ERRORE
028600130318     D  olst4msg              17     94
028700130429     D  iolsT4FOR             95     97
028800130429     D  IlsT4OLD              98     98
028900130318     D* PARF12 = 'S'    --> premuto F12
029000130318     D  olst4F12             102    102
029100130318     D  ilst4fgs             103    105
029200130318     D  ilst4TRN             106    112
029300130318     D  ilst4NRRP            113    125
029400130417     D  ilst4PDR             126    132
029500130422
029600130422     d* ricerca autisti di un trazionista
029700130422     D tntl83ds        DS
029800130422     d  i83soc                        3s 0
029900130422     d  i83ksc                        7s 0
030000130422     d* i83mod = 1 --> modalità scelta conducente
030100130423     d  i83mod                        1
030200130423     d* data di riferimento alla quale caricare per ora non usata
030300130423     d  i83Drf                        8
030400130422     d  o83_AUT                      35
030500130424     d  o83_TEL                      12
030600130423     d
030700130423     d* ricerca TARGHE  di un trazionista
030800130423     D tntl84ds        DS
030900130423     d  i84soc                        3s 0
031000130423     d  i84ksc                        7s 0
031100130423     d* i84mod = 1 --> modalità scelta conducente
031200130423     d  i84mod                        1
031300130423     d* data di riferimento alla quale caricare per ora non usata
031400130423     d  i84Drf                        8
031500130423     d  o84_TRM                      10
031600130509     d
031700130509      * Parametri per la ricerca trazionisti     - TRUL16R -
031800130509     D TRUL16DS        DS
031900130509     D* Tipologia T_=TRAZIONISTA
032000130509     D  PA3TIP                 1      2
032100130509     D* Ragione sociale trazionista
032200130509     D  PA3RSC                 3     37
032300130509     D* Sistema inform. trazionista
032400130509     D  PA3CSF                38     40
032500130509     D* Codice trazionista
032600130509     D  PA3PDR                41     47  0
032700130509     D* PA3FLG = "3" --> restituisce al pgm chiamante il messaggio che
032800130509     D*   non esistono records per la chiave alfabetica richiesta
032900130509     D  PA3FLG                48     48
033000130509     d
033100080206      //---------------------------------------------------------------
033200080206      //?Definizione variabili globali.
033300080206      //---------------------------------------------------------------
033400080206
033500080206      // - Flags booleani
033600111115     d $Contattato1    s               n   inz(*off)
033700111115     d $Contattato2    s               n   inz(*off)
033800111115     d $EndAtt         s               n   inz(*off)
033900080208     d $Fine           s               n   inz(*off)
034000080208     d $InzD01         s               n   inz(*on)
034100080208     d $InzS01         s               n   inz(*on)
034200080414     d $InzS02         s               n   inz(*on)
034300080603     d $InzS03         s               n   inz(*on)
034400130129     d $InzD05         s               n   inz(*on)
034500080206     d $ErrGrave       s               n   inz(*off)
034600080207     d $EoF            s               n   inz(*off)
034700080208     d $RecOK          s               n   inz(*off)
034800080206
034900080207      // - Campi associati al video
035000130130     d Savvideo        s              2    inz
035100130218     d $Video          s              2    inz('S2')
035200130123     d S03nrr          s              4  0 inz
035300080414     d S02nrr          s              4  0 inz
035400130620     d S02nrw          s              4  0 inz
035500080414     d wPag            s              4  0 inz
035600080414     d wRig            s              3  0 inz
035700080414
035800080604     d wAbi            s                   like(UTEaut)  inz
035900130121     d Indx            s              3  0 inz
036000080619     d Indx2           s              3  0 inz
036100130514     d xi              s              1  0 inz
036200130514     d xx              s              3  0 inz
036300080929     d yy              s              3  0 inz
036400080415     d ss              s              3  0 inz
036500080415     d ff              s              3  0 inz
036600130123     d Ktfp            s              3  0 inz
036700130123     d Ktfa            s              3  0 inz
036800130123     d Wfest           s              1
036900130315     d Almenouno       s              1
037000130315     d Wscelta         s              1
037100130417     d Wglobale        s              1
037200130418     d Unascelta       s              1
037300130430     d alfaFGS         s              3
037400130128     d x§tcod          s                   like(tblcod)
037500130128     d x§tkey          s                   like(tblkey)
037600130128     d x§tdes          s             30
037700130122     d
037800130314     d GIOggmmaa       s              6
037900130314     d GIOset          s              1
038000130315     d W02gioset       s              1
038100130314     d Datcor          s              8  0
038200130220     d w02DTN          s              8  0
038300130315     d savDTN          s              8  0
038400130412     d savTRN          s              7  0
038500130620     d savTRNw         s              7  0
038600130424     d sql_FGTTRN      s              7  0
038700130424     d Ktrn2           s              7  0
038800130422     d alfaTRN         s              7
038900130218     d carTRN          s              1
039000130424     d sql_FGTGSE      s              7
039100130412     d Wok             s              1
039200130412     d Carsfl          s              1
039300130412     d Savaper         s              1
039400130424     d Wforzgg1        s              1
039500130424     d Wforzgg2        s              1
039600130424     d Wforzgg3        s              1
039700130424     d Wforzgg4        s              1
039800130424     d Wforzgg5        s              1
039900130424     d Wforzgg6        s              1
040000130424     d Wforzgg7        s              1
040100130424     d Wfor2gg1        s              1
040200130424     d Wfor2gg2        s              1
040300130424     d Wfor2gg3        s              1
040400130424     d Wfor2gg4        s              1
040500130424     d Wfor2gg5        s              1
040600130424     d Wfor2gg6        s              1
040700130424     d Wfor2gg7        s              1
040800130426     d WforMgg1        s              1
040900130426     d WforMgg2        s              1
041000130426     d WforMgg3        s              1
041100130426     d WforMgg4        s              1
041200130426     d WforMgg5        s              1
041300130426     d WforMgg6        s              1
041400130426     d WforMgg7        s              1
041500130426     d Wtipo           s              1
041600130426     d Wmsg            s                   like(v1cmsg)
041700130422     d AlfaSOC         s              3
041800130422     d SocTraini       s              3  0
041900130422     d Kpdr            s              7  0
042000130502     D savnrrp5        S                   LIKE(h02nrrp)
042100130502     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
042200130424     d wrkgetlista     s           4096    varying
042300130218     d
042400080605     d Dataiso         s               d   datfmt(*iso)
042500080605     d Dataymd         s               d   datfmt(*ymd)
042600080605     d Datadmy         s               d   datfmt(*dmy)
042700130122     d Dataeur         s               d   datfmt(*eur)
042800081010     d DataSYS         s               d   inz(*sys)
042900130123     d Orasys          s               t   timfmt(*hms) inz(*sys)
043000130614     d wnfm            s                   like(tlrnfm)
043100081112     D
043200080206      //---------------------------------------------------------------
043300080206      //?Definizione procedure usate.
043400080206      //---------------------------------------------------------------
043500130617      /COPY QSYSINC/QRPGLESRC,QLGSORT
043600130617     D QLGKL                         16    DIM(MaxKey)
043700130617     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
043800130617     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
043900130617     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
044000130617     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
044100130617
044200130617      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
044300130617      /COPY QSYSINC/QRPGLESRC,QUSEC
044400080414      /copy gaitrasrc/srcprotopr,tibs02r
044500080414      /copy gaitrasrc/srcprotopr,tibs34r
044600130122      /copy gaitrasrc/srcprotopr,trul31r
044700130123      /copy gaitrasrc/srcprotopr,trul33r
044800130122      /copy gaitrasrc/srcprotopr,x§taber
044900130122      /copy gaitrasrc/srcprotopr,xsrda8
045000130314      /copy gaitrasrc/srcprotopr,xgiose1
045100130313      /copy gaitrasrc/srcprotopr,tntl17r
045200130509      /copy gaitrasrc/srcprotopr,trul16r
045300130318      /copy gaitrasrc/srcprotopr,fnlst4r
045400130422      /copy gaitrasrc/srcprotopr,tntl83r
045500130423      /copy gaitrasrc/srcprotopr,tntl84r
045600130122
045700080206      //---------------------------------------------------------------
045800080206      //?Riepilogo indicatori.
045900080206      //---------------------------------------------------------------
046000080207      // - Comuni
046100080207      // 28    : Emissione messaggio di errore a video
046200080207      // - C01/S01
046300080207      // 30    : Condiziona SFLDSP    (*not)
046400080207      // 31    : Condiziona SFLDSPCTL (*not)
046500080207      // 30+31 : Condiziona SFLCLR
046600080207      // 32    : Condiziona SFLNXTCHG in S01
046700080207      // 50    : Errore: Opzione errata
046800080207      // - D01
046900080207      // - Comuni
047000080207      // 99    : Generico di Errore
047100080206      //---------------------------------------------------------------
047200080206
047300080206      //---------------------------------------------------------------
047400080206      //?M A I N - L I N E
047500080206      //---------------------------------------------------------------
047600080206
047700080206     c     *Entry        plist
047800080206     c                   parm                    KPJBA
047900130314     c                   movel     kpjbu         fnlst1ds
048000080206
048100080206      /free
048200080206
048300080206       // Operazioni iniziali
048400080206       exsr RoutInz;
048500080206
048600080206       // Gestione video
048700080206       DOW $Fine = *off;
048800080206         select;
048900080530
049000080530         // SFL di scelta
049100080411           when $Video = 'S2';
049200080411             exsr GesS02;
049300130122
049400130417         // Sfl nascosto per scelta totale dei non apertu
049500130417           when $Video = 'A2';
049600130417            D02apfgv='S'   ;
049700130417            exsr InzS02;
049800130417
049900130417          // Posizionamento cursore
050000130417          if  Carsfl  <>'N'  ;
050100130417              $Video='CA' ;
050200130417           else  ;
050300130417              $Fine = *on;
050400130417              olst1err='E'   ;
050500130417              kpjbu=fnlst1ds  ;
050600130417              leave    ;
050700130417          endif  ;
050800130417
050900130417           // Elaborazione dei record scelti per apertura fogli
051000130417           when $Video = 'CA';
051100130417           exsr    ElaboraSFL   ;
051200130417
051300130319             if ilst1ges='A' and v1cmsg<>*blanks  ;
051400130319               $Video='S2'  ;
051500130319               else  ;
051600130319               $Fine = *on;
051700130319             endif   ;
051800130315
051900080206           other;
052000080206             $Fine = *on;
052100080206         endsl;
052200080206       ENDDO;
052300080206
052400080206       // Operazioni finali
052500130315         exsr RoutEnd;
052600080206
052700130122       //--------------------------------------------------------------
052800130122       //?Reperimento Dati del job (Utente/Operativi).
052900130122       //--------------------------------------------------------------
053000130122       BEGSR DatiJob;
053100130122
053200130122         in(E) §AzUte;
053300130122         if NOT %error;
053400130122           in(E) §DatiUte;
053500130122         endif;
053600130122         if %error or RSut = *blanks;
053700130122           clear TIBS34ds;
053800130122           tibs34r(tibs34ds);
053900130122           in §AzUte;
054000130122           in §DatiUte;
054100130122         endif;
054200130122
054300130122       ENDSR;
054400130122
054500080206       //--------------------------------------------------------------
054600080206       //?Operazioni iniziali.
054700080206       //--------------------------------------------------------------
054800130417       BEGSR RoutInz;
054900080206
055000130628       // Ordinamento sfl di default: per numero traino se non richiamato
055100130628       w_ord=w_ntraino;
055200130628       v1pf11=ord_dtaorp;
055300130617
055400130314       clear  Ricerca_PRP  ;
055500130502       $Fine =*off  ;
055600130314
055700130315 0     if olst1OPZIO= ' '  ;
055800130315
055900080206         // Impostazione campi "fissi"
056000080208         TBLkut = 1;
056100080208         TBLcod = '1L';
056200080206
056300080206         // Reperimento dati job
056400080206         exsr DatiJob;
056500080206
056600080206         // Verifica errori e autorità profilo
056700080206         clear  wAbi;
056800080206         clear  dLAT;
056900080206         select;
057000080206         // - Se ho errori nei dati utente esco dal pgm
057100080208           when  DUTerr = 'E';
057200080206             $Fine = *on;
057300080206         // - Se non c'è l'abilitazione:
057400080206         //   - se 1° livello, abilitazioni al terminal
057500080206         //   - se 2° livello, abilitazioni al punto operativo
057600080206         //   - se sede è impossibile ma se capita mando a fine il pgm
057700080208           when  UTEaut = *blank;
057800080206             select;
057900080208               when  DUTlpo = '1';
058000080206                 wAbi  = 'TP';
058100080208               when  DUTlpo = '2';
058200080206                 wAbi  = 'PO';
058300080208               when  DUTlpo = 'S';
058400080206                 $Fine = *on;
058500080206             endsl;
058600080206         // - Altrimenti si caricano le abilitazioni del profilo
058700080206           other;
058800080208             dUTE01 = UTEfaf;
058900080208             if  §UTEpot <> *blank;
059000080208               wAbi = §UTEpot;
059100080206             else;
059200080208               wAbi = UTEaut;
059300080206             endif;
059400080206         ENDSL;
059500080206
059600080206         // Controllo se ok l'abilitazione dell'utente
059700080206         reset TIBS02ds;
059800111115         T02Mod = 'C';
059900080208         T02sif = knsif;
060000080208         T02cod = 'LAT';
060100080208         T02ke1 = wAbi;
060200080414         TNTBE_RicercaControllo  (kpjba : tibs02ds);
060300080206
060400080208         if  T02err  = *blank;
060500080208           dLAT = T02uni;
060600080206         endif;
060700080206
060800080206         // Errore o utente non abilitato
060900080208         if  T02err <> *blanks   or
061000080208             §LATabi = 'S';
061100080206           $ErrGrave   = *on;
061200080206           errMessage  = *on;
061300080206           errGenerico = *on;
061400130220           V1cmsg = Msg(02);
061500080206           leavesr;
061600080206         endif;
061700080617
061800080617         clear trul31ds  ;
061900080617         I31abi = wabi    ;
062000080617         I31cdi = DUTdis  ;
062100080617         I31car = DUTare  ;
062200080617         I31cpo = DUTpou  ;
062300080617         TRUL31R   (KPJBA:trul31ds)   ;
062400080617
062500080617         if o31pog=*zeros   ;
062600080617           $ErrGrave   = *on;
062700080617           errMessage  = *on;
062800080617           errGenerico = *on;
062900130315          V1cmsg = Msg(07);
063000080617           leavesr;
063100080617         endif         ;
063200130430
063300130218       $inzs02=*on;
063400130218
063500130218       // Imposto come terminal in gestione simfel
063600130314       if ilst1fgs<=*zeros  ;
063700130314         v01fgs=simfel  ;
063800130314       else  ;
063900130314         v01fgs=%int(ilst1fgs)  ;
064000130314       endif ;
064100130314
064200130430       chain   (v01fgs)  azorg01l   ;
064300130218       if %found(azorg01l)   ;
064400130218        v01dfgs=orgdes  ;
064500130218       endif  ;
064600130218
064700130218       // Propongo data del giorno con selezione traini
064800130218       dataeur=datasys  ;
064900130218       d02dtn=%dec(dataeur)  ;
065000130218       w02dtn=%dec(datasys)  ;
065100130220       Datcor=%dec(datasys)  ;
065200130412       clear d02trn  ;
065300130412       clear savtrn  ;
065400130412       clear savaper ;
065500100520
065600130417       // Tipo gestione
065700130417
065800130314 1     if ilst1ges='I'     ;
065900130314       Interrogaz =*on  ;
066000130314       V01dric='INTERROGAZIONE'  ;
066100130314       endif  ;
066200130314
066300130315 1     if ilst1ges='R'   or ilst1ges='A' ;
066400130628
066500130628       // Ordinamento sfl di default: per data/ora partenza
066600130628       w_ord=w_dtaorp ;
066700130628       v1pf11=ord_traino;
066800130628
066900130628 2     if ilst1ges='R'   ;
067000130315        V01dric=' S C E L T A  '  ;
067100130628 2     endif  ;
067200130628 2     if ilst1ges='A'   ;
067300130315        V01dric='APERTURA FOGLI'  ;
067400130628 2     endif  ;
067500130618       // In apertura f.v.p determina l'ordinamento su tab PPA
067600130628 2     if ilst1ges='A'   ;
067700130618         reset TIBS02ds;
067800130618         T02Mod = 'C';
067900130618         T02sif = knsif;
068000130618         T02cod = 'PPA';
068100130618         T02ke1 = %editc(simfel:'X');
068200130618         TNTBE_RicercaControllo  (kpjba : tibs02ds);
068300130618
068400130618         if  T02err  = *blank;
068500130618           dppa = T02uni;
068600130619           if §ppaoaf='T';
068700130619              w_ord=w_ntraino;
068800130619             v1pf11=ord_dtaorp;
068900130618           endif;
069000130618         endif;
069100130628 2     endif;
069200130618
069300130315       Ricerca_prp=*on  ;
069400130315       Interrogaz =*on  ;
069500130314
069600130314 2       if ilst1dat>*zeros  ;
069700130314          dataiso=%date(%int(ilst1dat)) ;
069800130314         else  ;
069900130314          dataiso=%date(datcor)         ;
070000130314 2       endif  ;
070100130314          dataeur=dataiso  ;
070200130314          datadmy=dataiso  ;
070300130314          d02dtn=%dec(dataeur) ;
070400130314          w02dtn=%dec(dataiso)  ;
070500130314          GIOggmmaa=%editc(%dec(datadmy) :'X') ;
070600130314
070700130315          exsr  CercaGIORNO  ;
070800130315
070900130314 1      endif  ;
071000130417
071100130417        // Se proposta automatica senza far vedere videata,
071200130417        //  carico SFL ed elaboro in automatico
071300130417        if ilst1ges='A' and ilst1aut='S'   ;
071400130417        $Video='A2'  ;
071500130417        endif   ;
071600130422
071700130422        // Carico società trazinisti
071800130422        tblkey='1       ';
071900130422        clear alfasoc ;
072000130422        clear soctraini  ;
072100130422        chain  (1:'3I':tblkey ) tabel00f ;
072200130422        if %found(tabel00f)  ;
072300130422        ds3i=tbluni  ;
072400130422        alfasoc=%subst(§3ibst:7:3)  ;
072500130422        if alfasoc>*zeros  ;
072600130422        soctraini=%int(alfasoc)  ;
072700130422        endif  ;
072800130422        endif  ;
072900130315
073000130315 x0     else  ;
073100130315
073200130315        $Video='CA'  ;
073300130315        clear olst1TRN  ;
073400130315        clear olst1NRRP  ;
073500130315 0      endif  ;
073600130314
073700130122        ENDSR  ;
073800130315       //--------------------------------------------------------------
073900130315       //?Cerca giorno della settimana
074000130315       //--------------------------------------------------------------
074100130315          BEGSR  cercaGIORNO ;
074200130315          // cerco girno della settimana se non devo caricare tutti
074300130315 2        if ilst1tut='G'  ;
074400130315          xgiose1 (Gioggmmaa:GIOset)  ;
074500130315          // Salvo il giorno della settimana per cui devo caricare
074600130315          //  i traini
074700130315 3          if gioset>*zeros   ;
074800130315             w02gioset=GIOSET   ;
074900130315            else  ;
075000130315             clear w02gioset  ;
075100130315 3          endif  ;
075200130315 2        endif  ;
075300130315        ENDSR  ;
075400080411       //--------------------------------------------------------------
075500130218       //?Gestione videata S02   - sfl traini con o senza estensione
075600080411       //--------------------------------------------------------------
075700080411       BEGSR GesS02;
075800080411
075900130430         // se errore in prima videata lo emetto e poi esco
076000130430         if $errGrave=*on ;
076100130430         Carsfl='N'  ;
076200130430         ricint='S'  ;
076300130430         else    ;
076400080411         // Inizializzazione videata
076500080411         if  $InzS02 = *on;
076600080411            exsr InzS02;
076700080411             $InzS02  = *off;
076800080411         endif;
076900130430         endif;
077000080411
077100080411         // Posizionamento cursore
077200130412         if  Carsfl  <>'N'  ;
077300130412
077400130412         // Se non ci sono errori e ho indicato posizionamento
077500130412         if  errGenerico = *off and ricInt =' '
077600130412              and  savtrn<>d02trn  ;
077700130412          exsr posizTRN  ;
077800130412         endif  ;
077900130412
078000130123           sfldsp_n=*off;
078100080411           C02rcd = C02csr;
078200130412           if C02rcd=0  ;
078300130412            c02rcd=1  ;
078400130412           endif  ;
078500130412
078600080411         else;
078700080411
078800130123         // Se non è stato caricato nulla emetto sfl vuoto
078900130123           sfldsp_n=*on ;
079000080411         endif;
079100080411
079200080411         // Emissione Testata e Piede con tasti funzionali abilitati
079300130514         if  errGenerico = *off or ricInt <>' '  ;
079400130218           write  lst1T01;
079500130218           write  lst1P02;
079600130123           if sfldsp_n=*on  ;
079700130218           write  lst1d04;
079800130123           endif  ;
079900130123
080000080604           clear  ricInt   ;
080100080411         endif;
080200080411
080300080411         // Emissione videata
080400130218         exfmt  lst1C02;
080500080411
080600080411         reset errMessage;
080700080411         reset errGenerico;
080800130122         clear V1cmsg;
080900080523         clear errScelta ;
081000130315         clear errDTN    ;
081100130412         clear errTRN    ;
081200080411
081300080411         SELECT;
081400130430           WHEN  $ErrGrave=*on  ;
081500130430            $Fine = *on;
081600080411
081700130430         // - F3=Fine  12"=ritorno
081800130412           WHEN  dsp_aid = c_F03
081900130412              or dsp_aid = c_F12 ;
082000080411            $Fine = *on;
082100130617
082200130617         // - F11=Ordinamento
082300130617           WHEN  dsp_aid = c_F11;
082400130617           select;
082500130617           when w_ord=w_ntraino;
082600130617           w_ord=w_dtaorp;
082700130618           v1pf11=ord_traino;
082800130617           when w_ord=w_dtaorp;
082900130617           w_ord=w_ntraino;
083000130618           v1pf11=ord_dtaorp;
083100130617           endsl;
083200130617           exsr sr_ordina;
083300080411
083400130220         // - F10=Immissione (senza impostazione num traino)
083500130220           WHEN  dsp_aid = c_F10;
083600130130
083700130130           savVideo=$Video  ;
083800130220           $Video = 'D5';
083900130220           $inzD05=*on  ;
084000130314
084100130314           dow $Video='D5'  ;
084200130314            exsr Gesd05     ;
084300130314           enddo ;
084400130314           InterD05=*off ;
084500130314           Protd05 =*off ;
084600130220
084700080411         // Invio
084800080411           OTHER;
084900080523             exsr ContrS02  ;
085000080411             if  errGenerico = *on;
085100080411               leavesr;
085200080411             endif;
085300130315
085400130429         // Se premuto F15 - elaboro  tutto il sfl
085500130429           if    dsp_aid = c_F15;
085600130417           Wglobale='S'  ;
085700130417            $Video='CA'  ;
085800130315           else  ;
085900130315            if wscelta='1'  ;
086000130315             $Video='CA'  ;
086100130315            endif  ;
086200130315           endif  ;
086300080411
086400080411         ENDSL;
086500080411
086600080411       ENDSR;
086700080411       //--------------------------------------------------------------
086800080414       //?Inizializzazione sfl 2
086900080411       //--------------------------------------------------------------
087000080411       BEGSR InzS02;
087100080411
087200130418       exsr Pulizs02 ;
087300130620       exsr Pulizs0w ;
087400080411
087500130218       setll *loval   tntlt01l  ;
087600130218       read   tntlt01l  ;
087700130218
087800130418 1     dow not %eof(tntlt01l)  ;
087900130218
088000130218       // Escludo se annullato  o non decorrente alla data richiesta
088100130418 2     if tltatb=' '  ;
088200130418 3     if w02dtn>=tltdde  and  w02dtn<=tltdsc  ;
088300130218         exsr contr_ferm ;
088400130218
088500130418 4       if carTRN='S'  ;
088600130218          exsr EmisTRN  ;
088700130417
088800130417          // se devo fare una conferma automatica senza far vedere il slf
088900130417          //  se ho carico solo un record --> creo foglio viaggio
089000130417          //  se ne ho caricati più di 1  --> emetto sfl per scelta
089100130620 5         if ilst1ges='A' and ilst1aut='S' and s02nrr>0
089200130620                           and h02ttr<>'H'  and w_ord=w_ntraino;
089300130620            exsr CreaAUT  ;
089400130620 5         endif  ;
089500130418 4     endif  ;
089600130418 3     endif  ;
089700130418 2     endif  ;
089800130218
089900130218       read   tntlt01l  ;
090000130418 1     enddo  ;
090100130130
090200130617       clear rrnlast;
090300130617
090400130418 1       if S02nrr > *zero;
090500080411           C02rcd  = 1;
090600080411           C02csr  = 1;
090700130617           RrnLast = s02nrr;
090800080530
090900080411         else;
091000080411           clear C02rcd;
091100110831           clear C02csr;
091200130412           carSFL='N'  ;
091300130624           RrnLast = s02nrw;
091400130418 1       endif;
091500080411
091600130315       savdtn=w02dtn  ;
091700130412       savaper= d02apFGV;
091800130617       // vedo se ordinare il sfl
091900130620       if rrnlast>0;
092000130617       if w_ord<>w_ntraino or v1pf11=ord_traino ;
092100130620          if ilst1ges='A' and ilst1aut='S';
092200130620             exsr sr_ordinaw;
092300130620             exsr sr_elabs0w;
092400130620          else;
092500130620             exsr sr_ordina;
092600130620          endif;
092700130617       endif;
092800130620       endif;
092900080411       ENDSR;
093000130418       //--------------------------------------------------------------
093100130418       //?Pulizia sfl e campi sfl 2
093200130418       //--------------------------------------------------------------
093300130418       BEGSR Pulizs02  ;
093400130418
093500130418       // Pulizia subfile
093600130418         SflDsp_N    = *on;
093700130418         SflDspCtl_N = *on;
093800130418         write  lst1C02;
093900130418         SflDspCtl_N = *off;
094000130418         SflEnd      = *off;
094100130418
094200130418         clear W_SflPag;
094300130418         $EoF = *off;
094400130418         clear C02rcd;
094500130418         clear S02nrr;
094600130418         clear V1cmsg;
094700130418         clear carsfl  ;
094800130418 1       if ilst1ges=' '  ;
094900130418          clear V01dric  ;
095000130418 1       endif  ;
095100130418         errMessage  = *off;
095200130418         errGenerico = *off;
095300130418         errscelta=*off  ;
095400130418         NoTRN=*off  ;
095500130418         ENDSR  ;
095600130620       //--------------------------------------------------------------
095700130620       BEGSR Pulizs0w  ;
095800130620
095900130620       // Pulizia subfile
096000130620         SflDspw_N    = *on;
096100130620         SflDspCtlw_N = *on;
096200130620         write  lst1C0w;
096300130620         SflDspCtlw_N = *off;
096400130620
096500130620         clear S02nrw;
096600130620
096700130620         ENDSR  ;
096800130218       //--------------------------------------------------------------
096900130219       //?controllo se previsto il terminal come fermata
097000130218       //--------------------------------------------------------------
097100130218       BEGSR Contr_ferm  ;
097200130418
097300130218       clear cartrn  ;
097400130614       clear wnfm    ;
097500130220       NoTRN=*off  ;
097600130218       setll  (tlttrn:tltdde)  tntlr01l  ;
097700130218       reade  (tlttrn:tltdde)  tntlr01l  ;
097800130218
097900130218       dow not %eof(tntlr01l)  ;
098000130218       if tlratb=' '  and tlrcar='S' and tlrfil=v01fgs  ;
098100130218        cartrn='S'  ;
098200130614        wnfm=tlrnfm;
098300130218       leave  ;
098400130218       endif  ;
098500130218
098600130218       reade  (tlttrn:tltdde)  tntlr01l  ;
098700130218       enddo  ;
098800130218       ENDSR;
098900080411       //--------------------------------------------------------------
099000080411       //?Caricamento singola pagina S02
099100080411       //--------------------------------------------------------------
099200130218       BEGSR EmisTRN  ;
099300080411
099400080411         // - Caricamento dati nel record del subfile
099500080414         clear s02sce  ;
099600130412         clear s02aper ;
099700130412         s02aper_rev=*off  ;
099800130417         s02sce_prot=*off  ;
099900130412
100000130218         s02trn=tlttrn  ;
100100130508         h02ttr=tltttr  ;
100200130417         h02dde=tltdde  ;
100300130218         s02tfp=tlttfp  ;
100400130218         s02tfa=tlttfa  ;
100500130218          chain tlttfa azorg01l  ;
100600130219  1       if %found(azorg01l)  ;
100700130122           s02dtfa=orgdes  ;
100800130219  1       endif  ;
100900130614
101000130614         // giorno e ora di partenza da tntlo
101100130614
101200130614            clear s02ggp;
101300130614            clear s02hmp;
101400130614         if ilst1TUT='G';
101500130614         // se devo considerare solo il giorno della settimana
101600130614         // cerco col giorno e se non lo trovo cerco con " "
101700130614           chain (tlttrn:tltdde:wnfm:w02gioset) tntlo01l;
101800130614           if not %found(tntlo01l);
101900130614              chain (tlttrn:tltdde:wnfm:' ') tntlo01l;
102000130614           endif;
102100130614         else;
102200130614           chain (tlttrn:tltdde:wnfm) tntlo01l;
102300130614         endif;
102400130614         if %found (tntlo01l);
102500130614            s02ggp=tloggp;
102600130614            s02hmp=%editw(tlohhp:'0 :  ');
102700130614         endif;
102800130412
102900130412         // Se sono in apertura FGV visualizzo se già aperto
103000130412 1     if ilst1ges='A'   ;
103100130412        setll  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
103200130412        reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
103300130412 2      dow not %eof(fnfgv03l)  ;
103400130412 3       if   fgvatb=' '  ;
103500130412          s02aper='Ap'   ;
103600130429          //  s02aper_rev=*on  ;
103700130412          leave  ;
103800130412 3       endif  ;
103900130412       reade  (v01fgs:w02dtn:tlttrn) fnfgv03l  ;
104000130412 2     enddo  ;
104100130412
104200130412       // Se non è aperto verifico se si è SBINATO   ;
104300130412 2       if s02aper=*blanks  ;
104400130412           setll  (v01fgs:w02dtn:0000000) fnfgv03l  ;
104500130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
104600130412 3         dow not %eof(fnfgv03l)  ;
104700130412
104800130412 4          if   fgvatb=' '  ;
104900130412            // chain fnfv4 record "D"
105000130412            chain (FGVlnp:fgvnfv:'P':000:'D') fnfv401l ;
105100130412
105200130412 5          if %found(fnfv401l) and fv4atb=' '  ;
105300130422            alfaTRN=%subst(fv4not:1:7)  ;
105400130422 6          if alfaTRN>*zeros and alfaTRN=%editc(tlttrn:'X')  ;
105500130412             s02aper='Sb'   ;
105600130412             s02aper_rev=*on  ;
105700130412             leave  ;
105800130412 6          endif  ;
105900130412 5          endif  ;
106000130412 4          endif  ;
106100130412
106200130412           reade  (v01fgs:w02dtn:0000000) fnfgv03l  ;
106300130412 3     enddo  ;
106400130412
106500130412 2       endif  ;
106600130412 1       endif  ;
106700130315
106800130315         clear Almenouno  ;
106900130412
107000130412         // non scrivo se devo escludere i già aperti
107100130412 0       if (d02apFGV='S' and s02aper=*blanks) or
107200130412             d02apfgv=' ' ;
107300130218
107400130219         // Lettura ESTENSIONI : mi memorizzo i giorni della sett
107500130219         //  se inseriti tutti non faccio nessuna proposta da listino traini
107600130219         //  per il trazionista, altrimenti si
107700130509         setll (v01fgs:tlttrn)    fnfgt01l       ;
107800130509         reade (v01fgs:tlttrn)    fnfgt01l       ;
107900130219
108000130219  1      dow not %eof(fnfgt01l)  ;
108100130315
108200130412         // se devo caricare solo il giorno previsto della sett --> controllo
108300130315         clear Indx ;
108400130315         if ilst1TUT='G'  and fgtgse<>*blanks  ;
108500130315         Indx=%scan(w02gioset:fgtgse)  ;
108600130315         endif  ;
108700130315
108800130315  2a     if ilst1tut=' ' or fgtgse= *blanks or Indx>0   ;
108900130315
109000130219         s02pdr=fgtpdr  ;
109100130219         chain (ktltip:s02pdr)  tntlz01l  ;
109200130219  2      if %found(tntlz01l ) ;
109300130219           s02dpdr=tlzrsc  ;
109400130219  2      endif  ;
109500130219
109600130319         clear   s02set  ;
109700130313         if %subst(fgtgse:1:1)='1'  ;
109800130313          %subst(s02set:1:1)='L'  ;
109900130313  2      endif  ;
110000130313         if %subst(fgtgse:2:1)='2'  ;
110100130313          %subst(s02set:2:1)='M'  ;
110200130313  2      endif  ;
110300130313         if %subst(fgtgse:3:1)='3'  ;
110400130313          %subst(s02set:3:1)='M'  ;
110500130313  2      endif  ;
110600130313         if %subst(fgtgse:4:1)='4'  ;
110700130313          %subst(s02set:4:1)='G'  ;
110800130313  2      endif  ;
110900130313         if %subst(fgtgse:5:1)='5'  ;
111000130313          %subst(s02set:5:1)='V'  ;
111100130313  2      endif  ;
111200130313         if %subst(fgtgse:6:1)='6'  ;
111300130313          %subst(s02set:6:1)='S'  ;
111400130313  2      endif  ;
111500130313         if %subst(fgtgse:7:1)='7'  ;
111600130313          %subst(s02set:7:1)='L'  ;
111700130313  2      endif  ;
111800130313
111900130219         s02dcn=fgtdcn  ;
112000130219         s02trm=fgttrm ;
112100130219         s02trr=fgttrr ;
112200130219         clear h02imm ;
112300130315         h02nrrp=FGTnrr  ;
112400130219
112500130221         Sott_FGT=*on  ;
112600130417          H02notrn=notrn  ;
112700130417
112800130620           if ilst1ges='A' and ilst1aut='S'
112900130620                           and w_ord<>w_ntraino;
113000130620              S02nrw += 1;
113100130620              write lst1S0w;
113200130620           else;
113300130620              S02nrr += 1;
113400130620              write lst1S02;
113500130620           endif;
113600130417
113700130220           NoTRN=*on  ;
113800130315           Almenouno='S'  ;
113900130219
114000130315  2a     endif  ;
114100130315
114200130509         reade (v01fgs:tlttrn)    fnfgt01l       ;
114300130219  1     enddo  ;
114400130412  0     endif  ;
114500130219
114600130218         // Lettura listino per proporre il trazionista
114700130315         // Per scelta solo se non ho scritto per nulla il traino
114800130412         // e se non ho ancora perto un foglio su quel traino
114900130412
115000130417  1    if Ricerca_prp=*off  or  Almenouno=*blanks
115100130417 0       and (d02apFGV=' ' or  s02aper=*blanks)  ;
115200130315
115300130218       setll  (tlttrn:tltdde)  titll01l  ;
115400130218       reade  (tlttrn:tltdde)  titll01l  ;
115500130218
115600130315  1a   dow not %eof(titll01l)  ;
115700130219  2    if tllatb=' '   ;
115800130219       // Se non trovata estensione del trazionista --> propongo
115900130509         chain (v01fgs:tlttrn:tllpdr)  fnfgt01l       ;
116000130219  3      if not %found(fnfgt01l)  ;
116100130219
116200130218         s02pdr=tllpdr  ;
116300130218         chain (ktltip:s02pdr)  tntlz01l  ;
116400130219  4      if %found(tntlz01l ) ;
116500130218           s02dpdr=tlzrsc  ;
116600130502           else  ;
116700130510           clear s02pdr  ;
116800130502           s02dpdr='?????????????'   ;
116900130219  4      endif  ;
117000130219
117100130218         clear s02dcn  ;
117200130218         clear s02set  ;
117300130219         clear s02trm  ;
117400130219         clear s02trr  ;
117500130417         clear h02nrrp ;
117600130218
117700130218          h02imm='S'   ;
117800080411
117900130417          H02notrn=notrn  ;
118000130221          Sott_FGT=*off ;
118100130620           if ilst1ges='A' and ilst1aut='S'
118200130620                           and w_ord<>w_ntraino;
118300130620              S02nrw += 1;
118400130620              write lst1S0w;
118500130620           else;
118600130620              S02nrr += 1;
118700130620              write lst1S02;
118800130620           endif;
118900130220           NoTRN=*on  ;
119000130418           Almenouno='S'  ;
119100130218
119200130219  3     endif                           ;
119300130219  2     endif                           ;
119400130219
119500130218       reade  (tlttrn:tltdde)  titll01l  ;
119600130315  1a    enddo  ;
119700130315  1     endif                           ;
119800130418
119900130418       // se non ne ho caricato nemmeno uno --> emetto traino SENZA
120000130418       //  trazionista
120100130429       if Almenouno=' '
120200130429         and (d02apFGV=' ' or  s02aper=*blanks)  ;
120300130418       clear s02pdr  ;
120400130418       s02dpdr='???????'  ;
120500130418
120600130418         clear s02dcn  ;
120700130418         clear s02set  ;
120800130418         clear s02trm  ;
120900130418         clear s02trr  ;
121000130418         clear h02nrrp ;
121100130418
121200130418          h02imm='S'   ;
121300130418
121400130418          H02notrn=notrn  ;
121500130418          Sott_FGT=*off ;
121600130620           if ilst1ges='A' and ilst1aut='S'
121700130620                           and w_ord<>w_ntraino;
121800130620              S02nrw += 1;
121900130620              write lst1S0w;
122000130620           else;
122100130620              S02nrr += 1;
122200130620              write lst1S02;
122300130620           endif;
122400130418           NoTRN=*on  ;
122500130418       endif  ;
122600130218
122700130221       Sott_FGT=*off ;
122800080411       ENDSR;
122900080414       //--------------------------------------------------------------
123000080414       //?controllo scelta SFL 2
123100080414       //--------------------------------------------------------------
123200080523       BEGSR contrS02 ;
123300080414
123400130130       ErrScelta=*off ;
123500130315       clear Wscelta  ;
123600080415
123700130315       // controllo data traino
123800130315       clear   wlbdat  ;
123900130315       eval g02dat=d02dtn  ;
124000130315       xsrda8 (wlbdat)   ;
124100130315       if g02err='1'    ;
124200130315           errDTN      = *on;
124300130315           errMessage  = *on;
124400130315           errGenerico = *on;
124500130315           V1cmsg = Msg(01);
124600130315       leavesr    ;
124700130315       endif  ;
124800130315
124900130315       w02dtn=g02inv  ;
125000130315       d02dtn=g02dat  ;
125100130412
125200130412       // controllo se richiesto traino
125300130412       clear wok  ;
125400130412 1     if d02trn>0    ;
125500130424       ktrn2=d02trn  ;
125600130424       exsr decorrTRN  ;
125700130412
125800130412 2     if wok=' '  ;
125900130412           errTRN      = *on;
126000130412           errMessage  = *on;
126100130412           errGenerico = *on;
126200130412           V1cmsg = Msg(08);
126300130412       leavesr    ;
126400130412 2     endif  ;
126500130412 1     endif  ;
126600130412
126700130315
126800130315       // se cambiata data devo ricaricare
126900130315       if savdtn<>w02dtn  ;
127000130412        $inzs02=*on  ;
127100130412        dataiso=%date(%int(w02dtn)) ;
127200130412        datadmy=dataiso  ;
127300130412        GIOggmmaa=%editc(%dec(datadmy) :'X') ;
127400130412        exsr CercaGIORNO  ;
127500130315       endif ;
127600130412
127700130412       // Se cambiato flag solo non aperti ricarico SFL
127800130412       if savaper<>D02apFGV ;
127900130412        $inzs02=*on  ;
128000130412       endif  ;
128100130315
128200130218       readc lst1s02;
128300130218 1     DOW  NOT  %eof(fnlst1D);
128400130129
128500130315       if s02sce<>' '  ;
128600130315
128700130412
128800130314       // In GESTIONE opzione '1' non ammmessa
128900130314       if Interrogaz=*off and s02sce='1'  ;
129000130314           errScelta   = *on;
129100130314           errMessage  = *on;
129200130314           errGenerico = *on;
129300130314           V1cmsg = Msg(05);
129400130314          exsr errSFL2  ;
129500130314          leavesr   ;
129600130314       endif  ;
129700130314
129800130314       // INTERROGAZIONE E RICERCA
129900130314       // Ammessa solo scelta '1' per ricerca
130000130314
130100130412       if Interrogaz=*on  and s02sce<>'5' and s02sce<>'T'  ;
130200130315       if ilst1ges='I' or (Ricerca_prp=*on  and s02sce<>'1')  ;
130300130314           errScelta   = *on;
130400130314           errMessage  = *on;
130500130314           errGenerico = *on;
130600130314           V1cmsg = Msg(05);
130700130314          exsr errSFL2  ;
130800130314          leavesr   ;
130900130314       endif  ;
131000130314       endif  ;
131100130412
131200130412       // Interrogazione TRAINO
131300130412       if s02sce='T'   ;
131400130412      /end-free
131500130412     C                   MOVEL     'T05'         D00OP0
131600130412     C                   MOVEL     'O05'         D00OP1
131700130412     C                   MOVEL     '0'           D00F03
131800130412     C                   MOVEL     '0'           D00F12
131900130412     C                   MOVEL     '0'           D00ERR
132000130412     C                   MOVEL     *BLANKS       D00MSG
132100130412     C                   Z-ADD     s02trn        D00TRN
132200130417     C                   Z-ADD     h02dde        D00DDE
132300130412     C                   MOVEL     tntl00ds      KPJBU
132400130412     C                   CALL      'TNTL01C'
132500130412     C                   PARM                    KPJBA
132600130412      /free
132700130412
132800130412          clear s02sce    ;
132900130412          exsr errSFL2  ;
133000130412        else    ;
133100130314
133200130220       // non posso fare visualizzazione su un record in inserimento
133300130319       //  non do errore in apertura FGV
133400130319       if ilst1ges<>'A'   ;
133500130313 2     if h02imm='S' and  s02sce<>'I'   and s02sce<>' '  ;
133600130220           errScelta   = *on;
133700130220           errMessage  = *on;
133800130220           errGenerico = *on;
133900130313           V1cmsg = Msg(02);
134000130220          exsr errSFL2  ;
134100130313          leavesr   ;
134200130220       endif  ;
134300130319       endif  ;
134400130503
134500130503 2     if h02imm='S' and  s02sce= '5'   ;
134600130503          clear s02sce    ;
134700130503          exsr errSFL2  ;
134800130503          leavesr   ;
134900130503       endif  ;
135000130319
135100130315
135200130429       // se premeto F15--> non accetto scelta con '1'
135300130429           if   dsp_aid = c_F15 and s02sce='1'  ;
135400130315           errScelta   = *on;
135500130315           errMessage  = *on;
135600130315           errGenerico = *on;
135700130315           V1cmsg = Msg(06);
135800130315          exsr errSFL2  ;
135900130315          leavesr   ;
136000130315           endif  ;
136100130220
136200130318       // Per scelta aggiorno con SFLNEXG per rileggerli tutti alla fine
136300130318       if s02sce='1'     ;
136400130318          wscelta='1'  ;
136500130318          exsr errSFL2  ;
136600130318       else   ;
136700130129         $Video = 'D5';
136800130129          $inzd05=*on;
136900130129
137000130129         dow $Video='D5'  ;
137100130129          exsr Gesd05     ;
137200130129         enddo ;
137300130130          InterD05=*off ;
137400130130          Protd05 =*off ;
137500130129          clear s02sce    ;
137600130130
137700130130          exsr errSFL2  ;
137800130313          endif  ;
137900130318          endif  ;
138000130319          endif  ;
138100080414
138200130315
138300130218       readc lst1s02;
138400130122       enddo  ;
138500130122
138600130122       ENDSR   ;
138700080603       //--------------------------------------------------------------
138800080603       //?Aggiornamento sfl per errore
138900080603       //--------------------------------------------------------------
139000080603       BEGSR ErrSFL2 ;
139100130417
139200130313        if h02imm='S'  ;
139300130313        Sott_fgt=*off  ;
139400130313        else  ;
139500130313        Sott_fgt=*on   ;
139600130313        endif  ;
139700130313
139800130417         NoTRN=H02noTRN  ;
139900130418         if  s02sce_prot=*off  ;
140000130417          SflNxtChg = *on ;
140100130417         else  ;
140200130417          SflNxtChg = *off ;
140300130417         endif  ;
140400130429
140500130429         if s02aper='Sb'      ;
140600130429          s02aper_rev=*on  ;
140700130429         endif  ;
140800130429
140900130218         update lst1s02;
141000130412         c02csr=s02nrr ;
141100080603
141200130429         s02aper_rev=*off ;
141300080603         errscelta=*off  ;
141400080603         SflNxtChg = *off ;
141500080603       ENDSR;
141600080411
141700130412       //--------------------------------------------------------------
141800130412       //?Posizinamento SFL2 per numero traino
141900130412       //--------------------------------------------------------------
142000130412       BEGSR PosizTRN  ;
142100130412
142200130412       if d02trn=0  ;
142300130412        c02csr=1  ;
142400130412       else  ;
142500130412
142600130412       eval s02nrr=1  ;
142700130412       chain s02nrr   lst1s02;
142800130412
142900130412       dow %found     ;
143000130412       if s02trn=d02trn  ;
143100130412         c02csr=s02nrr ;
143200130412         leave  ;
143300130412       endif  ;
143400130412
143500130412       s02nrr=s02nrr+1  ;
143600130412
143700130412       chain s02nrr   lst1s02;
143800130412       enddo   ;
143900130412       endif  ;
144000130412       savtrn=d02trn  ;
144100130412
144200130412       ENDSR;
144300130129       //--------------------------------------------------------------
144400130129       //?Gestione videata D05
144500130129       //--------------------------------------------------------------
144600130129         BEGSR GesD05;
144700130129
144800130129         // Inizializzazione videata
144900130129
145000130129         if  $inzd05 = *on;
145100130129         // Verifico che data proporre per le richieste
145200130129           exsr Inzd05    ;
145300130129          $Inzd05=*off      ;
145400130129         endif ;
145500130129
145600130129         // Emissione testata
145700130514         if  errGenerico = *off or ricInt<>' ';
145800130218           write lst1T01;
145900130129           clear ricint   ;
146000130129         endif;
146100130129
146200130129         // Emissione videata
146300130218         exfmt lst1D05;
146400130129
146500130129         clear V1cmsg;
146600130129         clear errGenerico;
146700130422         ErrMessage =*off  ;
146800130313         clear errgg1  ;
146900130313         clear errgg2  ;
147000130313         clear errgg3  ;
147100130313         clear errgg4  ;
147200130313         clear errgg5  ;
147300130313         clear errgg6  ;
147400130313         clear errgg7  ;
147500130419         clear ERRTRN_IMM      ;
147600130419         clear ERRTRN_nw       ;
147700130422         clear ERRpdr          ;
147800130422         clear ERRdcn          ;
147900130422         clear ERRTRM          ;
148000130129
148100130129    1    select;
148200130129
148300130129         // F12=ritorno
148400130129           when dsp_aid = c_F12;
148500130621               unlock fnfgt00f;
148600130129               $Video = 'S2';
148700130130               Errgenerico=*off  ;
148800130130               ErrMessage =*off  ;
148900130129
149000130129           other  ;
149100130129             exsr Contrd05 ;
149200130129
149300130130             if InterD05=*on  ;
149400130130             ErrMessage=*off  ;
149500130130             ErrGenerico=*off  ;
149600130130             endif    ;
149700130130
149800130514    2        if  errGenerico = *on or ricint<>' '  ;
149900130129               leavesr;
150000130129    2        endif;
150100130129
150200130130    2      if   InterD05=*on    ;
150300130129               $Video = 'S2';
150400130129    2      endif  ;
150500130129
150600130129 2         if    dsp_aid = c_F16;
150700130129               $Inzs02=*on  ;
150800130129               $Video = 'S2';
150900130129 2           endif;
151000130129
151100130129 2         if    dsp_aid = c_F06;
151200130129            exsr  AggiorD05  ;
151300130129
151400130129 3           if  errGenerico = *on;
151500130129               leavesr;
151600130129             else  ;
151700130129               $Video = 'S2';
151800130129 3           endif;
151900130313 2         endif;
152000130313
152100130313 2         if    dsp_aid = c_F16;
152200130313            delete fnfgtfis ;
152300130313               $Video = 'S2';
152400130313 2         endif;
152500130129
152600130129 1           endsl  ;
152700130129
152800130502          if   $Video = 'S2';
152900130502          h02nrrp=savnrrp5  ;
153000130502          endif  ;
153100130502
153200130129          ENDSR;
153300130129       //--------------------------------------------------------------
153400130129       //?Inizializzazione VIDEATA 5
153500130129       //--------------------------------------------------------------
153600130129       BEGSR Inzd05;
153700130220       Interd05=*off  ;
153800130220       Protd05 =*off  ;
153900130220       CopiaD05=*off  ;
154000130220       Spostad05=*off  ;
154100130220       Manutd05=*off  ;
154200130220       Imm_d05 =*off  ;
154300130502       savnrrp5=h02nrrp  ;
154400130129
1545001302201      if s02sce='2'  ;
154600130129       v01dric= ' MANUTENZIONE '  ;
154700130220       ManutD05=*ON  ;
1548001302201      ENDIF  ;
1549001302201      if s02sce='3'  ;
155000130220       v01dric= 'C  O  P  I  A '  ;
155100130220       PROTD05=*ON  ;
155200130220       copiaD05=*ON  ;
1553001302201      ENDIF  ;
1554001303131      if s02sce='8'  ;
155500130220       v01dric= ' S P O S T A  ' ;
155600130220       PROTD05=*ON  ;
155700130220       SpostaD05=*ON  ;
1558001302201      ENDIF  ;
1559001302201      if s02sce='5'  ;
156000130129       v01dric= 'INTERROGAZIONE'  ;
156100130130       INTERD05=*ON  ;
156200130129       PROTD05=*ON  ;
1563001302201      ENDIF  ;
156400130220
1565001302201      if s02sce='I' or S02sce=' '  ;
156600130220       v01dric= ' INSERIMENTO  '  ;
156700130220       Imm_d05=*on  ;
156800130424       clear h02nrrp  ;
1569001302201      ENDIF  ;
157000130220
157100130220       // se si tratta di record di nuova immissione
157200130220       //   pulisco i campi
157300130220       clear d05pru  ;
157400130220       clear d05dim  ;
157500130220       clear d05trn  ;
157600130419       clear d05trn_n;
157700130220       clear d05tfp  ;
157800130220       clear d05dtfp  ;
157900130220       clear d05tfa  ;
158000130220       clear d05dtfa ;
158100130220       clear d05pdr  ;
158200130220       clear d05dpdr ;
158300130220       clear d05dcn  ;
158400130220       clear d05trm  ;
158500130220       clear d05trr  ;
158600130220       clear d05not  ;
158700130628       clear d05nt2  ;
158800130628       clear d05an1  ;
158900130628       clear d05an2  ;
159000130220       clear d05salt  ;
159100130220       clear d05gg1   ;
159200130220       clear d05gg2   ;
159300130220       clear d05gg3   ;
159400130220       clear d05gg4   ;
159500130220       clear d05gg5   ;
159600130220       clear d05gg6   ;
159700130220       clear d05gg7   ;
159800130313
159900130424       clear Wforzgg1 ;
160000130424       clear Wforzgg2 ;
160100130424       clear Wforzgg3 ;
160200130424       clear Wforzgg4 ;
160300130424       clear Wforzgg5 ;
160400130424       clear Wforzgg6 ;
160500130424       clear Wforzgg7 ;
160600130424       clear Wfor2gg1 ;
160700130424       clear Wfor2gg2 ;
160800130424       clear Wfor2gg3 ;
160900130424       clear Wfor2gg4 ;
161000130424       clear Wfor2gg5 ;
161100130424       clear Wfor2gg6 ;
161200130424       clear Wfor2gg7 ;
161300130426       clear WforMgg1 ;
161400130426       clear WforMgg2 ;
161500130426       clear WforMgg3 ;
161600130426       clear WforMgg4 ;
161700130426       clear WforMgg5 ;
161800130426       clear WforMgg6 ;
161900130426       clear WforMgg7 ;
162000130424
162100130313       clear d05trn_nw ;
162200130313       clear d05tfpnw;
162300130313       clear d05dtfpnw ;
162400130313       clear d05tfanw;
162500130313       clear d05dtfanw ;
162600130220
162700130220       // Se ho effettuato la scelta --> propongo i dati della riga
162800130220 1     if s02sce<>' '  ;
162900130220
163000130220       // IMMISSIONE
163100130220 2     select ;
163200130220       when s02sce ='I'     ;
163300130313       d05trn=%editc(s02trn:'X')  ;
163400130220       d05tfp=s02tfp  ;
163500130220       d05tfa=s02tfa  ;
163600130313       d05pdr=%editc(s02pdr:'X')  ;
163700130423       // mi posiziono sul padroncino se vuoto, altrimenti sul conducente
163800130423       if d05trn>*zeros and d05pdr>*zeros  ;
163900130423       errdcn=*on   ;
164000130423       endif  ;
164100130423       if d05trn>*zeros and d05pdr<=*zeros  ;
164200130423       d05pdr='?'  ;
164300130423       errpdr=*on   ;
164400130423       endif  ;
164500130423
164600130423       d05dcn='?'  ;
164700130423       d05trm='?'  ;
164800130220
164900130313       // altre opzioni
165000130313 2     when s02sce  <>' ' and s02sce<>'I'  ;
165100130621       if s02sce='5';
165200130621       chain(n) h02nrrp  fnfgt00f  ;
165300130621       else;
165400130315       chain  h02nrrp  fnfgt00f  ;
165500130621       endif;
165600130220 3     if %found(fnfgt00f) ;
165700130313       d05trn=%editc(s02trn:'X')  ;
165800130419       d05trn_n= s02trn  ;
165900130220       d05tfp=s02tfp  ;
166000130220       d05tfa=s02tfa  ;
166100130313       d05pdr=%editc(fgtpdr:'X')  ;
166200130220       d05dcn=fgtdcn  ;
166300130220       d05trm=fgttrm  ;
166400130220       d05trr=fgttrr  ;
166500130220       d05not=fgtnot  ;
166600130628       d05nt2=fgtnt2  ;
166700130628       d05an1=fgtan1  ;
166800130628       d05an2=fgtan2  ;
166900130313       d05pru=fgtpru  ;
167000130313       dataiso=%date(fgtdim) ;
167100130313       dataeur=dataiso  ;
167200130313       d05dim=%dec(dataeur)   ;
167300130220
167400130514  4    //if fgtgse=*blanks  ;
167500130417        //  d05salt ='SI'   ;
167600130514       //else  ;
167700130514         if %subst(fgtgse:1:1)<>' '     ;
167800130514         d05gg1='S'  ;
167900130514         endif  ;
168000130514         if %subst(fgtgse:2:1)<>' '     ;
168100130514         d05gg2='S'  ;
168200130514         endif  ;
168300130514         if %subst(fgtgse:3:1)<>' '     ;
168400130514         d05gg3='S'  ;
168500130514         endif  ;
168600130514         if %subst(fgtgse:4:1)<>' '     ;
168700130514         d05gg4='S'  ;
168800130514         endif  ;
168900130514         if %subst(fgtgse:5:1)<>' '     ;
169000130514         d05gg5='S'  ;
169100130514         endif  ;
169200130514         if %subst(fgtgse:6:1)<>' '     ;
169300130514         d05gg6='S'  ;
169400130514         endif  ;
169500130514         if %subst(fgtgse:7:1)<>' '     ;
169600130514         d05gg7='S'  ;
169700130514         endif  ;
169800130514         //d05gg2= %subst(fgtgse:2:1)     ;
169900130514         //d05gg3= %subst(fgtgse:3:1)     ;
170000130514         //d05gg4= %subst(fgtgse:4:1)     ;
170100130514         //d05gg5= %subst(fgtgse:5:1)     ;
170200130514         //d05gg6= %subst(fgtgse:6:1)     ;
170300130514         //d05gg7= %subst(fgtgse:7:1)     ;
170400130514 4      // endif  ;
170500130220
170600130220  3    endif  ;
170700130220  2    endsl  ;
170800130510       else  ;
170900130510       d05pdr='?'  ;
171000130510       d05dcn='?'  ;
171100130510       d05trm='?'  ;
171200130510       d05trn='?'  ;
171300130221  1    endif  ;
171400130220
171500130220       if d05pdr > *zeros   ;
171600130220       kpdr =%int(d05pdr)  ;
171700130220         chain (ktltip:kpdr)  tntlz01l  ;
171800130221         if %found(tntlz01l ) ;
171900130220           D05dpdr=tlzrsc  ;
172000130221         endif  ;
172100130221       endif  ;
172200130220
172300130220       if d05tfp>0  ;
172400130220       chain d05tfp azorg01l  ;
172500130220  2      if %found(azorg01l ) ;
172600130220           D05dtfp =orgdes ;
172700130220  2      endif  ;
172800130220       endif  ;
172900130220       if d05tfa>0  ;
173000130220       chain d05tfa azorg01l  ;
173100130220  2      if %found(azorg01l ) ;
173200130220           D05dtfa =orgdes ;
173300130220  2      endif  ;
173400130220  1    endif  ;
173500130129
173600130129       ENDSR  ;
173700130129       //--------------------------------------------------------------
173800130129       //?controllo dati   video 5
173900130129       //--------------------------------------------------------------
174000130129       BEGSR contrd05 ;
174100130313
174200130313       // Per immissione controllo il traino
174300130313 1     if imm_d05=*on   ;
174400130313       Indx=%scan('?':d05trn)  ;
174500130313 2     if indx>0   ;
174600130313        exsr ricercaTraino  ;
174700130313
174800130313 3     if d00trn>0  ;
174900130313       d05trn = %editc(d00trn:'X')  ;
175000130422
175100130422        chain  (d00trn:d00dde) tntlt01l  ;
175200130422 4      if %found(tntlt01l)  ;
175300130422        d05tfp    = tlttfp              ;
175400130422        d05tfa    = tlttfa              ;
175500130422        exsr DecoTRNimm ;
175600130422 x4    else  ;
175700130422       clear d05trn  ;
175800130422  4     endif  ;
175900130313 x3    else  ;
176000130313       clear d05trn  ;
176100130313  3    endif  ;
176200130313
176300130514       ricint='S'  ;
176400130314       leavesr     ;
176500130313  2    endif    ;
176600130313  1    endif   ;
176700130313
176800130313       // Per copia o spostamento  vedo se attivata la ricerca traino
176900130313       Indx=%scan('?':d05trn_nw)  ;
177000130419  0    if indx>0   ;
177100130313        exsr ricercaTraino  ;
177200130313
177300130419 1     if d00trn>0  ;
177400130313        d05trn_nw = %editc(d00trn:'X')  ;
177500130419
177600130419        chain  (d00trn:d00dde) tntlt01l  ;
177700130419 2      if %found(tntlt01l)  ;
177800130422        d05tfpNW  = tlttfp              ;
177900130422        d05tfaNW  = tlttfa              ;
178000130422        exsr DecoTRN_nw ;
178100130422 x2    else  ;
178200130422        clear d05trn_nw  ;
178300130422 2      endif  ;
178400130313
178500130419 x1    else  ;
178600130313        clear d05trn_nw  ;
178700130419 1     endif  ;
178800130313
178900130514       ricint='S'  ;
179000130419       leavesr  ;
179100130419 0     endif    ;
179200130419
179300130419       // Immissione     Traino --> controllo
179400130502 1     // if imm_d05=*on   ;
179500130419       alfaTRN=d05trn  ;
179600130419       exsr  CtrTRN  ;
179700130419 2      if Errgenerico=*on   ;
179800130419         Errtrn_imm=*on   ;
179900130419         leavesr  ;
180000130422 x2      else  ;
180100130422        d05tfp    = tlttfp              ;
180200130422        d05tfa    = tlttfa              ;
180300130422        exsr DecoTRNimm ;
180400130419 2      endif    ;
180500130502 1     // endif    ;
180600130419
180700130419       // Copia o sposta traino --> controllo
180800130419 1     if Copiad05=*on or spostad05=*on    ;
180900130419       alfatrn=d05trn_nw  ;
181000130419       exsr  CtrTRN  ;
181100130419 2      if Errgenerico=*on   ;
181200130419         Errtrn_nw =*on   ;
181300130419         leavesr  ;
181400130422 x2      else  ;
181500130422        d05tfpNW  = tlttfp              ;
181600130422        d05tfaNW  = tlttfa              ;
181700130422        exsr DecoTRN_nw ;
181800130419 2      endif    ;
181900130419 1     endif    ;
182000130422
182100130509       // Trazionista
182200130509       //  Ricerca
182300130509       Indx=%scan('?':d05pdr)  ;
182400130509 2     if indx>0   ;
182500130509        clear trul16ds  ;
182600130509        pa3tip='T ' ;
182700130509        pa3rsc=d05dpdr  ;
182800130509        kpjbu= trul16ds  ;
182900130509        callp trul16r   (kpjba)  ;
183000130509        trul16ds=kpjbu  ;
183100130509        d05pdr=%editc(pa3pdr:'X')   ;
183200130509        d05dpdr=pa3rsc  ;
183300130509
183400130510           ricint=*on  ;
183500130509           errpdr      = *on;
183600130509           leavesr  ;
183700130509
183800130509       endif  ;
183900130509
184000130422       if d05pdr  = *zeros    or d05pdr=*blanks  ;
184100130422           errpdr      = *on;
184200130422           errMessage  = *on;
184300130422           errGenerico = *on;
184400130422           V1cmsg = Msg(10);
184500130422           leavesr  ;
184600130422           endif  ;
184700130422
184800130422       if  d05pdr<= *ZEROS   ;
184900130422           errpdr      = *on;
185000130422           errMessage  = *on;
185100130422           errGenerico = *on;
185200130422           V1cmsg = Msg(10);
185300130422           leavesr  ;
185400130422       ELSE  ;
185500130422
185600130422       kpdr =%int(d05pdr)  ;
185700130422         chain (ktltip:kpdr)  tntlz01l  ;
185800130422         if %found(tntlz01l ) ;
185900130422           D05dpdr=tlzrsc  ;
186000130422         else  ;
186100130422           errpdr      = *on;
186200130422           errMessage  = *on;
186300130422           errGenerico = *on;
186400130422           V1cmsg = Msg(10);
186500130422           leavesr  ;
186600130422         endif  ;
186700130422       endif  ;
186800130502
186900130502       // Il trazionista deve essere previsto dal traino
187000130502       setll (kpdr:tlttrn:tltdde) titll02l   ;
187100130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
187200130502       dow not %eof(titll02l)  ;
187300130502       if tllatb=' '  ;
187400130502       leave    ;
187500130502       else  ;
187600130502       reade (kpdr:tlttrn:tltdde) titll02l   ;
187700130502       endif  ;
187800130502       enddo  ;
187900130502
188000130502       if %eof(titll02l)  ;
188100130510       // non segnalo errore per traino locale
188200130510       if h02ttr<>'L'  ;
188300130502           errpdr      = *on;
188400130502           errMessage  = *on;
188500130502           errGenerico = *on;
188600130502           V1cmsg = Msg(18);
188700130502           leavesr  ;
188800130502       endif  ;
188900130510       endif  ;
189000130422
189100130508       // Conducente --> vedo se accreditato escludo H L D
189200130422 1     if d05dcn<>*blanks   ;
189300130422
189400130422       // controllo se c'e' ricerca
189500130422       Indx=%scan('?':d05dcn)  ;
189600130422 2     if indx>0   ;
189700130422         clear tntl83ds  ;
189800130422         i83soc=soctraini ;
189900130422         i83ksc=kpdr      ;
190000130423         i83mod='1'  ;
190100130423         i83drf=%editc(w02dtn:'X')  ;
190200130422         kpjbu=tntl83ds  ;
190300130422         callp TNTL83R  (kpjba)  ;
190400130422         tntl83ds =kpjbu  ;
190500130422 3       if o83_aut<>*blanks  ;
190600130422          d05dcn= o83_aut  ;
190700130423          else  ;
190800130423          clear d05dcn ;
190900130422 3       endif   ;
191000130423
191100130422         ricint=*on  ;
191200130423         errdcn=*on  ;
191300130422       leavesr  ;
191400130422 2     endif   ;
191500130422
191600130508       if tltttr <>'H' and tltttr<>'L' and tltttr<>'D'  ;
191700130508
191800130422       chain (soctraini:kpdr:d05dcn)  aiata01l  ;
191900130422       if not %found(aiata01l) or ataatb<>' '  ;
192000130422           errdcn      = *on;
192100130422           errMessage  = *on;
192200130422           errGenerico = *on;
192300130422           V1cmsg = Msg(11);
192400130422           leavesr  ;
192500130422       endif  ;
192600130422
192700130508       // se alla data decorrenza è scaduto il rapporto  --> errore
192800130422       if atadtfir>0 and w02dtn > atadtfir  ;
192900130422           errdcn      = *on;
193000130422           errMessage  = *on;
193100130422           errGenerico = *on;
193200130422           V1cmsg = Msg(12);
193300130422           leavesr  ;
193400130422       endif  ;
193500130508       endif  ;
193600130508
193700130422       endif  ;
193800130422
193900130422       // Targa Motrice
194000130422       if d05trm<>*blanks  ;
194100130423
194200130423       // controllo se c'e' ricerca
194300130423       Indx=%scan('?':d05trm)  ;
194400130423 2     if indx>0   ;
194500130423         clear tntl84ds  ;
194600130423         i84soc=soctraini ;
194700130423         i84ksc=kpdr      ;
194800130423         i84mod='1'  ;
194900130423         i84drf=%editc(w02dtn:'X')  ;
195000130423         kpjbu=tntl84ds  ;
195100130423         callp TNTL84R  (kpjba)  ;
195200130423         tntl84ds =kpjbu  ;
195300130423 3       if o84_TRM<>*blanks  ;
195400130423          d05trm= o84_TRM  ;
195500130423          else  ;
195600130423          clear d05trm ;
195700130423 3       endif   ;
195800130423
195900130423         ricint=*on  ;
196000130423         errtrm=*on  ;
196100130423       leavesr  ;
196200130423 2     endif   ;
196300130423
196400130508       if tltttr <>'H' and tltttr<>'L' and tltttr<>'D'  ;
196500130508
196600130422       chain (soctraini:kpdr:d05trm)  aiatm01l  ;
196700130422       if not %found(aiatm01l) or atmatb<>' '  ;
196800130422           errtrm      = *on;
196900130422           errMessage  = *on;
197000130422           errGenerico = *on;
197100130422           V1cmsg = Msg(13);
197200130422           leavesr  ;
197300130422       endif  ;
197400130422
197500130422       // se alla data decorrenza è scaduto il rapporto  --> errore
197600130422       if atmdtfir>0 and w02dtn > atmdtfir  ;
197700130422           errtrm      = *on;
197800130422           errMessage  = *on;
197900130422           errGenerico = *on;
198000130422           V1cmsg = Msg(13);
198100130422           leavesr  ;
198200130422       endif  ;
198300130508       endif  ;
198400130508
198500130422       endif  ;
198600130422
198700130422       // Giorni partenza
198800130514       if d05gg1<>' '  and d05gg1<>'S'  ;
198900130422           errgg1      = *on;
199000130313           errMessage  = *on;
199100130313           errGenerico = *on;
199200130313           V1cmsg = Msg(03);
199300130514           %subst(v1cmsg:53:10)='LUNEDÌ    ' ;
199400130313           leavesr;
199500130313       endif  ;
199600130503       // controllo se giorno previsto dal traino
199700130514       if d05gg1='S' and %subst(tltgse:1:1)=' '  ;
199800130503           errgg1      = *on;
199900130503           errMessage  = *on;
200000130503           errGenerico = *on;
200100130503           V1cmsg = Msg(19);
200200130503       endif  ;
200300130503
200400130514       if d05gg2<>' '  and d05gg2<>'S'  ;
200500130313           errgg2      = *on;
200600130313           errMessage  = *on;
200700130313           errGenerico = *on;
200800130313           V1cmsg = Msg(03);
200900130514           %subst(v1cmsg:53:10)='MARTEDÌ   ' ;
201000130313           leavesr;
201100130313       endif  ;
201200130503       // controllo se giorno previsto dal traino
201300130514       if d05gg2='S' and %subst(tltgse:2:1)=' '  ;
201400130503           errgg2      = *on;
201500130503           errMessage  = *on;
201600130503           errGenerico = *on;
201700130503           V1cmsg = Msg(19);
201800130503       endif  ;
201900130514       if d05gg3<>' '  and d05gg3<>'S'  ;
202000130313           errgg3      = *on;
202100130313           errMessage  = *on;
202200130313           errGenerico = *on;
202300130313           V1cmsg = Msg(03);
202400130514           %subst(v1cmsg:53:10)='MERCOLEDÌ ' ;
202500130313           leavesr;
202600130313       endif  ;
202700130503       // controllo se giorno previsto dal traino
202800130514       if d05gg3='S' and %subst(tltgse:3:1)=' '  ;
202900130503           errgg3      = *on;
203000130503           errMessage  = *on;
203100130503           errGenerico = *on;
203200130503           V1cmsg = Msg(19);
203300130503       endif  ;
203400130514       if d05gg4<>' '  and d05gg4<>'S'  ;
203500130313           errgg4      = *on;
203600130313           errMessage  = *on;
203700130313           errGenerico = *on;
203800130313           V1cmsg = Msg(03);
203900130514           %subst(v1cmsg:53:10)='GIOVEDÌ   ' ;
204000130313           leavesr;
204100130313       endif  ;
204200130503       // controllo se giorno previsto dal traino
204300130514       if d05gg4='S' and %subst(tltgse:4:1)=' '  ;
204400130503           errgg4      = *on;
204500130503           errMessage  = *on;
204600130503           errGenerico = *on;
204700130503           V1cmsg = Msg(19);
204800130503       endif  ;
204900130514       if d05gg5<>' '  and d05gg5<>'S'  ;
205000130313           errgg5      = *on;
205100130313           errMessage  = *on;
205200130313           errGenerico = *on;
205300130313           V1cmsg = Msg(03);
205400130514           %subst(v1cmsg:53:10)='VENERDÌ   ' ;
205500130313           leavesr;
205600130313       endif  ;
205700130514       if d05gg5='S' and %subst(tltgse:5:1)=' '  ;
205800130503           errgg5      = *on;
205900130503           errMessage  = *on;
206000130503           errGenerico = *on;
206100130503           V1cmsg = Msg(19);
206200130503       endif  ;
206300130514       if d05gg6<>' '  and d05gg6<>'S'  ;
206400130313           errgg6      = *on;
206500130313           errMessage  = *on;
206600130313           errGenerico = *on;
206700130313           V1cmsg = Msg(03);
206800130514           %subst(v1cmsg:53:10)='SABATO    ' ;
206900130313           leavesr;
207000130313       endif  ;
207100130514       if d05gg6='S' and %subst(tltgse:6:1)=' '  ;
207200130503           errgg6      = *on;
207300130503           errMessage  = *on;
207400130503           errGenerico = *on;
207500130503           V1cmsg = Msg(19);
207600130503       endif  ;
207700130514       if d05gg7<>' '  and d05gg7<>'S'  ;
207800130313           errgg7      = *on;
207900130313           errMessage  = *on;
208000130313           errGenerico = *on;
208100130313           V1cmsg = Msg(03);
208200130514           %subst(v1cmsg:53:10)='DOMENICA  ' ;
208300130313           leavesr;
208400130313       endif  ;
208500130514       if d05gg7='S' and %subst(tltgse:7:1)=' '  ;
208600130503           errgg7      = *on;
208700130503           errMessage  = *on;
208800130503           errGenerico = *on;
208900130503           V1cmsg = Msg(19);
209000130503       endif  ;
209100130129
209200130313       IF  D05GG1= ' ' and D05GG2= ' '   and D05GG3= ' '
209300130313          and D05GG4= ' '  and D05GG5= ' ' and D05GG6= ' '
209400130422          and D05GG7= ' '   ;
209500130423           errgg1      = *on;
209600130313           errMessage  = *on;
209700130313           errGenerico = *on;
209800130313           V1cmsg = Msg(04);
209900130313           leavesr;
210000130313       endif  ;
210100130424
210200130502       // controllo da fare solo se non annullo
210300130502       if   dsp_aid <> c_F16;
210400130502
210500130424       // Per ogni giorno inserito verifico se c'e' un'altra proposta
210600130424       // se sposto o copio uso il traino di destino
210700130424       if d05trn_nw>*zeros   ;
210800130424        ktrn=%int(d05trn_nw)  ;
210900130424       else  ;
211000130424        ktrn=%int(d05trn)  ;
211100130424       endif  ;
211200130424
211300130509       setll   (v01fgs:ktrn)     fnfgt01l  ;
211400130509       reade   (v01fgs:ktrn)     fnfgt01l  ;
211500130424
211600130424       dow  not %eof(fnfgt01l)  ;
211700130424       if fgtnrr <>  h02nrrp ;
211800130514       if d05gg1<>' '  and %subst(fgtgse:1:1)<>' '
211900130424        and wforzgg1=' '  ;
212000130424           errgg1      = *on;
212100130424           errMessage  = *on;
212200130424           errGenerico = *on;
212300130424           V1cmsg = Msg(14);
212400130424           %subst(v1cmsg:39:10)='  LUNEDÌ  ' ;
212500130424           wforzgg1='S'  ;
212600130424           leavesr;
212700130424       endif  ;
212800130514       if d05gg2<>' '  and %subst(fgtgse:2:1)<>' '
212900130424        and wforzgg2=' '  ;
213000130424           errgg2      = *on;
213100130424           errMessage  = *on;
213200130424           errGenerico = *on;
213300130424           V1cmsg = Msg(14);
213400130424           %subst(v1cmsg:39:10)='  MARTEDÌ ' ;
213500130424           wforzgg2='S'  ;
213600130424           leavesr;
213700130424       endif  ;
213800130514       if d05gg3<>' '  and  %subst(fgtgse:3:1)<>' '
213900130424        and wforzgg3=' '  ;
214000130424           errgg3      = *on;
214100130424           errMessage  = *on;
214200130424           errGenerico = *on;
214300130424           V1cmsg = Msg(14);
214400130424           %subst(v1cmsg:39:10)='MERCOLEDÌ ';
214500130424           wforzgg3='S'  ;
214600130424           leavesr;
214700130424       endif  ;
214800130514       if d05gg4<>' '  and %subst(fgtgse:4:1)<>' '
214900130424        and wforzgg4=' '  ;
215000130424           errgg4      = *on;
215100130424           errMessage  = *on;
215200130424           errGenerico = *on;
215300130424           V1cmsg = Msg(14);
215400130424           %subst(v1cmsg:39:10)=' GIOVEDÌ  ' ;
215500130424           wforzgg4='S'  ;
215600130424           leavesr;
215700130424       endif  ;
215800130514       if d05gg5<>' '  and %subst(fgtgse:5:1)<>' '
215900130424        and wforzgg5=' '  ;
216000130424           errgg5      = *on;
216100130424           errMessage  = *on;
216200130424           errGenerico = *on;
216300130424           V1cmsg = Msg(14);
216400130424           %subst(v1cmsg:39:10)=' VENERDÌ  ' ;
216500130424           wforzgg5='S'  ;
216600130424           leavesr;
216700130424       endif  ;
216800130514       if d05gg6<>' '  and %subst(fgtgse:6:1)<>' '
216900130424        and wforzgg6=' '  ;
217000130424           errgg6      = *on;
217100130424           errMessage  = *on;
217200130424           errGenerico = *on;
217300130424           V1cmsg = Msg(14);
217400130424           %subst(v1cmsg:39:10)='  SABATO  ' ;
217500130424           wforzgg6='S'  ;
217600130424           leavesr;
217700130424       endif  ;
217800130514       if d05gg7<>' '  and %subst(fgtgse:7:1)<>' '
217900130424        and wforzgg7=' '  ;
218000130424           errgg7      = *on;
218100130424           errMessage  = *on;
218200130424           errGenerico = *on;
218300130424           V1cmsg = Msg(14);
218400130424           %subst(v1cmsg:39:10)=' DOMENICA ' ;
218500130424           wforzgg7='S'  ;
218600130424           leavesr;
218700130424       endif  ;
218800130424       endif  ;
218900130424
219000130509       reade   (v01fgs:ktrn)     fnfgt01l  ;
219100130424       enddo  ;
219200130424
219300130426       // Controllo se il CONDUCENTE è già presente in un traino decorrente
219400130424       //  nello stesso giorno
219500130508
219600130508       if d05dcn<>*blanks  ;
219700130508
219800130424       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtdcn='''+
219900130424        d05dcn + ''''                                    ;
220000130424       EXEC sql  prepare s1 from : wrkgetlista ;
220100130424       EXEC sql  declare a1 cursor for s1      ;
220200130424       EXEC sql  open    a1                    ;
220300130424
220400130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
220500130424  1    dow sqlcod=0    ;
220600130426       wtipo='C'  ;
220700130426       Exsr ErrDOPP  ;
220800130426       if v1cmsg<>*blanks  ;
220900130426        leave  ;
221000130426       endif  ;
221100130426
221200130424
221300130424       EXEC sql fetch next from a1 into :sql_fgttrn, :sql_fgtgse ;
221400130424       enddo           ;
221500130424
221600130424       EXEC sql  CLOSE   a1                    ;
221700130426
221800130426       if v1cmsg<>*blanks  ;
221900130426        leavesr;
222000130426       endif  ;
222100130508
222200130508       endif  ;
222300130424
222400130426       // Controllo se la TARGA MOTRICE è già presente in un traino decorrente
222500130426       //  nello stesso giorno
222600130508       if d05trm<>*blanks  ;
222700130508
222800130426       wrkgetlista='select fgttrn, fgtgse from fnfgt00f where fgtTRM='''+
222900130426        d05trm + ''''                                    ;
223000130426       EXEC sql  prepare s2 from : wrkgetlista ;
223100130426       EXEC sql  declare a2 cursor for s2      ;
223200130426       EXEC sql  open    a2                    ;
223300130426
223400130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
223500130426  1    dow sqlcod=0    ;
223600130426       wtipo='M'  ;
223700130426       Exsr ErrDOPP  ;
223800130426       if v1cmsg<>*blanks  ;
223900130426        leave  ;
224000130426       endif  ;
224100130426
224200130426       EXEC sql fetch next from a2 into :sql_fgttrn, :sql_fgtgse ;
224300130426       enddo           ;
224400130426
224500130426       EXEC sql  CLOSE   a2                    ;
224600130426
224700130426       if v1cmsg<>*blanks  ;
224800130426        leavesr;
224900130426       endif  ;
225000130502
225100130502       endif  ;
225200130508       endif  ;
225300130424
225400130129       ENDSR   ;
225500130426       //--------------------------------------------------------------
225600130508       //?Errore per conducente/targa già presente nel giorno
225700130426       //--------------------------------------------------------------
225800130426       Begsr ErrDOPP  ;
225900130426
226000130426           if wtipo='C'  ;
226100130426            wmsg = Msg(15);
226200130426           else  ;
226300130426            wmsg = Msg(16);
226400130426           endif ;
226500130426
226600130426 2     if sql_fgttrn<> ktrn  and sql_fgttrn<>%int(d05trn)  ;
226700130426
226800130514 3     if d05gg1<>' '  and %subst(sql_fgtgse:1:1)<>' ' ;
226900130426 3a     if  (wfor2gg1=' ' and wtipo='C') or (wforMgg1=' ' and wtipo='M') ;
227000130426        // Verifico che il traino sia decorrente
227100130426        ktrn2=sql_fgttrn  ;
227200130426        exsr decorrTRN  ;
227300130426 4      if wok='S'      ;
227400130426           errgg1      = *on;
227500130426           errMessage  = *on;
227600130426           errGenerico = *on;
227700130426           V1cmsg = wmsg   ;
227800130426           %subst(v1cmsg:36:10)='  LUNEDÌ  ' ;
227900130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
228000130426           if wtipo='C'  ;
228100130426            wfor2gg1='S'  ;
228200130426           else  ;
228300130426            wforMgg1='S'  ;
228400130426           endif  ;
228500130426           leavesr;
228600130426 4     endif  ;
228700130426 3a    endif  ;
228800130426 3     endif  ;
228900130514 3     if d05gg2<>' '  and %subst(sql_fgtgse:2:1)<>' ' ;
229000130426 3a     if  (wfor2gg2=' ' and wtipo='C') or (wforMgg2=' ' and wtipo='M') ;
229100130426        // Verifico che il traino sia decorrente
229200130426        ktrn2=sql_fgttrn  ;
229300130426        exsr decorrTRN  ;
229400130426 4      if wok='S'      ;
229500130426           errgg2      = *on;
229600130426           errMessage  = *on;
229700130426           errGenerico = *on;
229800130426           V1cmsg = wmsg   ;
229900130426           %subst(v1cmsg:36:10)=' MARTEDÌ  ' ;
230000130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
230100130426           if wtipo='C'  ;
230200130426            wfor2gg2='S'  ;
230300130426           else  ;
230400130426            wforMgg2='S'  ;
230500130426           endif  ;
230600130426           leavesr;
230700130426 4     endif  ;
230800130426 3a    endif  ;
230900130426 3     endif  ;
231000130514 3     if d05gg3<>' '  and %subst(sql_fgtgse:3:1)<>' ' ;
231100130426 3a     if  (wfor2gg3=' ' and wtipo='C') or (wforMgg3=' ' and wtipo='M') ;
231200130426        // Verifico che il traino sia decorrente
231300130426        ktrn2=sql_fgttrn  ;
231400130426        exsr decorrTRN  ;
231500130426 4      if wok='S'      ;
231600130426           errgg3      = *on;
231700130426           errMessage  = *on;
231800130426           errGenerico = *on;
231900130426           V1cmsg = wmsg   ;
232000130426           %subst(v1cmsg:36:10)=' MERCOLEDÌ' ;
232100130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
232200130426           if wtipo='C'  ;
232300130426            wfor2gg3='S'  ;
232400130426           else  ;
232500130426            wforMgg3='S'  ;
232600130426           endif  ;
232700130426           leavesr;
232800130426 4     endif  ;
232900130426 3a    endif  ;
233000130426 3     endif  ;
233100130514 3     if d05gg4<>' '  and %subst(sql_fgtgse:4:1)<>' '  ;
233200130426 3a     if  (wfor2gg4=' ' and wtipo='C') or (wforMgg4=' ' and wtipo='M') ;
233300130426        // Verifico che il traino sia decorrente
233400130426        ktrn2=sql_fgttrn  ;
233500130426        exsr decorrTRN  ;
233600130426 4      if wok='S'      ;
233700130426           errgg4      = *on;
233800130426           errMessage  = *on;
233900130426           errGenerico = *on;
234000130426           V1cmsg = wmsg   ;
234100130426           %subst(v1cmsg:36:10)=' GIOVEDÌ  ' ;
234200130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
234300130426           if wtipo='C'  ;
234400130426            wfor2gg4='S'  ;
234500130426           else  ;
234600130426            wforMgg4='S'  ;
234700130426           endif  ;
234800130426           leavesr;
234900130426 4     endif  ;
235000130426 3a    endif  ;
235100130426 3     endif  ;
235200130514 3     if d05gg5<>' '  and %subst(sql_fgtgse:5:1)<>' '  ;
235300130426 3a     if  (wfor2gg5=' ' and wtipo='C') or (wforMgg5=' ' and wtipo='M') ;
235400130426        // Verifico che il traino sia decorrente
235500130426        ktrn2=sql_fgttrn  ;
235600130426        exsr decorrTRN  ;
235700130426 4      if wok='S'      ;
235800130426           errgg5      = *on;
235900130426           errMessage  = *on;
236000130426           errGenerico = *on;
236100130426           V1cmsg = wmsg   ;
236200130426           %subst(v1cmsg:36:10)=' VENERDÌ  ' ;
236300130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
236400130426           if wtipo='C'  ;
236500130426            wfor2gg5='S'  ;
236600130426           else  ;
236700130426            wforMgg5='S'  ;
236800130426           endif  ;
236900130426           leavesr;
237000130426 4     endif  ;
237100130426 3a    endif  ;
237200130426 3     endif  ;
237300130514 3     if d05gg6<>' '  and %subst(sql_fgtgse:6:1)<>' '  ;
237400130426 3a     if  (wfor2gg6=' ' and wtipo='C') or (wforMgg6=' ' and wtipo='M') ;
237500130426        // Verifico che il traino sia decorrente
237600130426        ktrn2=sql_fgttrn  ;
237700130426        exsr decorrTRN  ;
237800130426 4      if wok='S'      ;
237900130426           errgg6      = *on;
238000130426           errMessage  = *on;
238100130426           errGenerico = *on;
238200130426           V1cmsg = wmsg   ;
238300130426           %subst(v1cmsg:36:10)=' SABATO   ' ;
238400130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
238500130426           if wtipo='C'  ;
238600130426            wfor2gg6='S'  ;
238700130426           else  ;
238800130426            wforMgg6='S'  ;
238900130426           endif  ;
239000130426           leavesr;
239100130426 4     endif  ;
239200130426 3a    endif  ;
239300130426 3     endif  ;
239400130514 3     if d05gg7<>' '  and %subst(sql_fgtgse:7:1)<>' ' ;
239500130426 3a     if  (wfor2gg7=' ' and wtipo='C') or (wforMgg7=' ' and wtipo='M') ;
239600130426        // Verifico che il traino sia decorrente
239700130426        ktrn2=sql_fgttrn  ;
239800130426        exsr decorrTRN  ;
239900130426 4      if wok='S'      ;
240000130426           errgg7      = *on;
240100130426           errMessage  = *on;
240200130426           errGenerico = *on;
240300130426           V1cmsg = wmsg   ;
240400130426           %subst(v1cmsg:36:10)=' DOMENICA ' ;
240500130426           %subst(v1cmsg:58:7)=%editc(sql_fgttrn:'Z')  ;
240600130426           if wtipo='C'  ;
240700130426            wfor2gg7='S'  ;
240800130426           else  ;
240900130426            wforMgg7='S'  ;
241000130426           endif  ;
241100130426           leavesr;
241200130426 4     endif  ;
241300130426 3a    endif  ;
241400130426 3     endif  ;
241500130426
241600130426 2     endif  ;
241700130426
241800130426       ENDSR  ;
241900130424       //--------------------------------------------------------------
242000130424       //?controlla decorrenza di un traino
242100130424       //--------------------------------------------------------------
242200130424       BEGSR DecorrTRN            ;
242300130424       clear wok                  ;
242400130424       setgt  ktrn2   tntlt01l    ;
242500130424       readpe ktrn2   tntlt01l    ;
242600130424 2     dow not %eof(tntlt01l)  ;
242700130424 3     if tltatb=' '  and w02dtn>=tltdde and w02dtn<=tltdsc ;
242800130424        wok='S'  ;
242900130424        leave  ;
243000130424 3     endif  ;
243100130424
243200130424       readpe ktrn2   tntlt01l    ;
243300130424 2     enddo   ;
243400130424       ENDSR   ;
243500130424
243600130422       //--------------------------------------------------------------
243700130422       //?Decodifica fermate iniziale e finale traino
243800130422       //--------------------------------------------------------------
243900130422        BEGSR DEcoTRNimm ;
244000130422
244100130422 4      if d05tfp  >0   ;
244200130422         chain d05tfp   azorg01l  ;
244300130422 5       if %found(azorg01l)  ;
244400130422         d05dtfp  =orgdes  ;
244500130422         else  ;
244600130422         d05dtfp  =*all'?'  ;
244700130422 5       endif  ;
244800130422 4      endif  ;
244900130422
245000130422 4      if d05tfa  >0   ;
245100130422         chain d05tfa   azorg01l  ;
245200130422 5       if %found(azorg01l)  ;
245300130422         d05dtfa  =orgdes  ;
245400130422         else  ;
245500130422         d05dtfa  =*all'?'  ;
245600130422 5       endif  ;
245700130422 4      endif  ;
245800130422       //--------------------------------------------------------------
245900130422       ENDSR  ;
246000130422        BEgsr DecoTRN_nw  ;
246100130422         chain d05tfpNW azorg01l  ;
246200130422 3       if %found(azorg01l)  ;
246300130422         d05dtfpnw=orgdes  ;
246400130422         else  ;
246500130422         d05dtfpnw=*all'?'  ;
246600130422 3       endif  ;
246700130422
246800130422         chain d05tfanw azorg01l  ;
246900130422 3       if %found(azorg01l)  ;
247000130422         d05dtfanw=orgdes  ;
247100130422         else  ;
247200130422         d05dtfanw=*all'?'  ;
247300130422 3       endif  ;
247400130422        ENDSR   ;
247500130313       //--------------------------------------------------------------
247600130313       //?Richiama pgm di ricerca traino per il terminal
247700130313       //--------------------------------------------------------------
247800130313       BEGSR ricercaTraino  ;
247900130313       clear tntl00ds  ;
248000130313       d00sca='C'   ;
248100130319       d00dde=w02dtn  ;
248200130313       d00fil= v01fgs  ;
248300130313
248400130313       kpjbu=tntl00ds  ;
248500130313       tntl17r (kpjba)  ;
248600130313       tntl00ds=kpjbu ;
248700130313       ENDSR  ;
248800130129       //--------------------------------------------------------------
248900130430       //?Aggiornoa richiesta singola    D05
249000130129       //--------------------------------------------------------------
249100130129       BEGSR AggiorD05 ;
249200130220
249300130313       // IMMISSIONE
249400130313       if Imm_d05=*on    ;
249500130430        clear fnfgtfis   ;
249600130313        fgtpru=knmus  ;
249700130313        fgtdim=datcor  ;
249800130313        fgttrn=%int(d05trn)  ;
249900130313       endif  ;
250000130313
250100130313       // COPIA
250200130313       if copiad05=*on  ;
250300130430        clear fnfgtfis   ;
250400130313        fgtpru=knmus  ;
250500130313        fgtdim=datcor  ;
250600130313        fgttrn=%int(d05trn_nw)  ;
250700130313       endif  ;
250800130313
250900130509       fgttfp=v01fgs ;
251000130220       fgtpdr=%int(d05pdr) ;
251100130220       fgtdcn=d05dcn  ;
251200130220       fgttrm=d05trm;
251300130220       fgttrr=d05trr;
251400130220       fgtnot=d05not ;
251500130628       fgtnt2=d05nt2 ;
251600130628       fgtan1=d05an1 ;
251700130628       fgtan2=d05an2 ;
251800130313       fgtgse=d05gg1+d05gg2+d05gg3+d05gg4+d05gg5+d05gg6+d05gg7  ;
251900130514             xi=1  ;
252000130514               for  xi = 1  to  %len( %trimr( fgtgse ) );
252100130514                 if  %subst( fgtgse : xi : 1 ) <>' ';
252200130514                   %subst(fgtgse:xi:1)=%editc(xi:'X') ;
252300130514                 endif;
252400130514               endfor;
252500130220
252600130313       if Imm_d05=*on  or Copiad05=*on ;
252700130220       write fnfgtfis  ;
252800130220       endif  ;
252900130220
253000130220       if   ManutD05=*on  ;
253100130220       update fnfgtfis  ;
253200130220       endif   ;
253300130313
253400130313       if   spostad05=*on  ;
253500130313        fgttrn=%int(d05trn_nw)  ;
253600130313       update fnfgtfis  ;
253700130313       endif   ;
253800130129
253900130129           $Inzs02=*on  ;
254000130129
254100130129       ENDSR  ;
254200130417
254300130417       //--------------------------------------------------------------
254400130429       //?elabora SFL per scelta proposte
254500130417       //--------------------------------------------------------------
254600130417           BEGSR ElaboraSFL   ;
254700130417
254800130417           clear s02sce  ;
254900130417           clear s02sce_prot ;
255000130418           clear unascelta ;
255100130417
255200130417 0         if wglobale=' '  ;
255300130417
255400130417           readc lst1s02;
255500130417 1         DOW  NOT  %eof(fnlst1D) and $Fine=*off ;
255600130417
255700130417 2         if    s02sce= '1'  ;
255800130417 3           if ilst1ges='R'  ;
255900130417              olst1TRN =%editc(s02trn :'X')   ;
256000130417              olst1nrrp=%editc(h02nrrp:'X')  ;
256100130417              olst1OPZIO='1'  ;
256200130417              $Fine = *on;
256300130417              kpjbu=fnlst1ds  ;
256400130417              leave    ;
256500130417
256600130417 x3          else  ;
256700130417
256800130417             exsr AperturaFGV  ;
256900130418             unascelta='1'  ;
257000130417
257100130418  4          if olst4msg<>*blanks  ;
257200130417              errScelta   = *on;
257300130417              errMessage  = *on;
257400130417              errGenerico = *on;
257500130417              V1cmsg = olst4msg  ;
257600130418  4          endif  ;
257700130418  3          endif  ;
257800130417              exsr errSFL2  ;
257900130417 2        endif  ;
258000130417
258100130417            readc  lst1s02   ;
258200130417 1     enddo  ;
258300130418
258400130418       // se non ho fatto nessuna scelta e sono nella apertura
258500130418       //  totale --> apro senza trazionista/proposta
258600130418 1        if Unascelta=' ' and ilst1aut='S'  ;
258700130418            clear h02nrrp  ;
258800130418            clear s02pdr  ;
258900130418             exsr AperturaFGV  ;
259000130418 1        endif  ;
259100130417
259200130417 0     else  ;
259300130417
259400130417       // Scelta globale --> leggo tutto il slf
259500130417       //  non ancora aperti
259600130417
259700130417       eval   s02nrr=1  ;
259800130417       chain   s02nrr lst1s02;
259900130418 1     dow  %found           ;
260000130417
260100130417 3           if ilst1ges='R'  ;
260200130417              olst1TRN =%editc(s02trn :'X')   ;
260300130417              olst1nrrp=%editc(h02nrrp:'X')  ;
260400130417              olst1OPZIO='1'  ;
260500130417              $Fine = *on;
260600130417              kpjbu=fnlst1ds  ;
260700130417              leave    ;
260800130417
260900130417 x3          else  ;
261000130417
261100130417             exsr AperturaFGV  ;
261200130417
261300130418  4          if olst4msg<>*blanks  ;
261400130417              errScelta   = *on;
261500130417              errMessage  = *on;
261600130417              errGenerico = *on;
261700130417              V1cmsg = olst4msg  ;
261800130418  4          endif  ;
261900130418  3          endif  ;
262000130417              exsr errSFL2  ;
262100130429
262200130417         s02nrr=s02nrr+1  ;
262300130417         chain   s02nrr lst1s02;
262400130417  1    enddo  ;
262500130417
262600130417 0     endif  ;
262700130417
262800130417       ENDSR  ;
262900130417
263000130417       //--------------------------------------------------------------
263100130417       //?Richiamo pgm per apertua fogli viaggio
263200130417       //--------------------------------------------------------------
263300130417       BEGSR AperturaFGV  ;
263400130417
263500130417              clear fnlst4ds  ;
263600130417              ilst4ges='A'  ;
263700130417              ilst4fgs =%editc(v01fgs:'X')    ;
263800130417              ilst4TRN =%editc(s02trn :'X')   ;
263900130418 1            if h02nrrp>0  ;
264000130417                ilst4nrrp=%editc(h02nrrp:'X')  ;
264100130418 x1           else  ;
264200130418                ilst4nrrp=*all'0'  ;
264300130417
264400130417                // Se non si tratta di richiamo automatico ma scelta
264500130429                //  a video la passo altrimenti vuoto per far scegliere
264600130417                //  dopo
264700130417                ilst4pdr=%editc(s02pdr:'X')   ;
264800130502              //   ilst4old='S'  ;
264900130418  1           endif  ;
265000130417              ilst4dfv=w02dtn  ;
265100130429
265200130429              // Se premuto F6 passo al chiamante
265300130429              if dsp_aid = c_F06;
265400130429               iolst4for='F6 '  ;
265500130429              endif  ;
265600130429
265700130417              kpjbu=fnlst4ds    ;
265800130417              callp  FNLST4R (kpjba)   ;
265900130417              fnlst4ds=kpjbu   ;
266000130417
266100130429             if iolst4for= *blanks ;
266200130429              clear s02sce   ;
266300130429             endif  ;
266400130417       ENDSR  ;
266500130417       //--------------------------------------------------------------
266600130417       //?Apertura automatica dei fogli viaggio traino oper traino
266700130417       //--------------------------------------------------------------
266800130417       BEGSR creaAUT      ;
266900130417       if s02nrr=1  ;
267000130418        exsr AperturaFGV ;
267100130417       else   ;
267200130417       // emetto SFL per la scelta
267300130418           C02rcd  = 1;
267400130418           C02csr  = 1;
267500130418           $InzS02  = *off;
267600130418           ctl02_prot=*on   ;
267700130418
267800130418           exsr Gess02  ;
267900130418
268000130418           // controllo cosa scelto: se nulla --> apro senza trazionista
268100130418           if   $Video = 'CA';
268200130418             exsr    ElaboraSFL   ;
268300130503           else  ;
268400130503            clear h02nrrp  ;
268500130503            clear s02pdr   ;
268600130503            exsr AperturaFGV ;
268700130418           endif  ;
268800130418       endif  ;
268900130418
269000130418           ctl02_prot=*off  ;
269100130418           exsr Pulizs02  ;
269200130417
269300130417       ENDSR  ;
269400130417       //--------------------------------------------------------------
269500130417       //?Scelta su singolo traino: proposta o trazionista
269600130417       //--------------------------------------------------------------
269700130417       BEGSR SceltasingTRN;
269800130417       clear s02sce   ;
269900130417       exsr errsfl2  ;
270000130417       ENDSR  ;
270100080206       //--------------------------------------------------------------
270200080206       //?Operazioni finali.
270300080206       //--------------------------------------------------------------
270400080206       BEGSR RoutEnd;
270500080410
270600130502       if $Video='CA' and s02sce='1' and ilst1ges='R' ;
270700130315         *inrt=*on  ;
270800130315         else  ;
270900080206         *inLR = *on;
271000130315         endif  ;
271100130315
271200080206         return;
271300080206
271400080206       ENDSR;
271500130620       //--------------------------------------------------------------
271600130620       //?Elaborazione SFL di work per la gestione dell F15=Immissione tot
271700130620       //--------------------------------------------------------------
271800130620       BEGSR sr_elabs0w;
271900130620       s02nrw=1;
272000130620       clear savtrnw;
272100130620       chain s02nrw lst1s0W;
272200130620       dow %found;
272300130620        // A cambio di traino richiamo la creazione del foglio viaggio
272400130620          if s02trn<>savtrnw and savtrnw>0;
272500130620             s02nrw-=1;
272600130620             chain s02nrw lst1s0W;
272700130620             exsr CreaAUT;
272800130620             s02nrw+=1;
272900130620             chain s02nrw lst1s0W;
273000130620          endif;
273100130620          savtrnw=s02trn;
273200130620          S02nrr += 1;
273300130620          write lst1S02;
273400130620          s02nrw+=1;
273500130620          chain s02nrw lst1s0W;
273600130620       enddo;
273700130620          s02nrw-=1;
273800130620          chain s02nrw lst1s0W;
273900130620          exsr CreaAUT;
274000130620       endsr;
274100080206
274200080206      /end-free
274300130419     C*-----------------------------------------------------***********
274400130419     C* CONTROLLO NUMERO TRAINO                             * CTRTRN  *
274500130419     C*-----------------------------------------------------***********
274600130419     C     CTRTRN        BEGSR
274700130419     C*
274800130419     c* controllo se inserito un numero
274900130419     c                   testn                   alfaTRN              30
275000130419     c                   movel     alfaTRN       alfaUNO           1
275100130419    2c                   if        not *in30 or alfaUNO<'0'
275200130419     C                   MOVEL     MSG(8)        V1cMSG
275300130422     c                   eval      errMessage  = *on
275400130422     c                   eval      errGenerico = *on
275500130419     C                   GOTO      ENDTRN
275600130419    2C                   ENDIF
275700130419     c
275800130419     c                   movel     alfaTRN       ktrn              7 0
275900130419     c
276000130419     C     ktrn          SETLL     TNTLT01L
276100130422     C     ktrn          READE     TNTLT01L
276200130419     C*
276300130419    2c                   dow       not %eof(tntlt01l)
276400130422     c                   if        tltatb=' ' and
276500130422     c                             w02dtn>=tltdde and w02dtn<=tltdsc
276600130422     c                   leave
276700130422     c                   else
276800130422     C     ktrn          READE     TNTLT01L
276900130422     c                   endif
277000130422     c                   enddo
277100130422     c
277200130422     c* Errore : traino non trovat o
277300130422     c                   if        %eof(tntlt01l)
277400130422     C                   MOVEL     MSG(8)        V1cMSG
277500130422     c                   eval      errMessage  = *on
277600130422     c                   eval      errGenerico = *on
277700130422     C                   GOTO      ENDTRN
277800130422     c                   endif
277900130422     c
278000130422     C*   Errore: traino  "A-AFFLUENZA"
278100130419    4C     TLTTTR        IFEQ      'A'
278200130419     C                   MOVEL     MSG(8)        V1cMSG
278300130422     c                   eval      errMessage  = *on
278400130422     c                   eval      errGenerico = *on
278500130419     C                   GOTO      ENDTRN
278600130422     c                   endif
278700130422     c
278800130422     c* controllo se la filiale gestione è una fermata di carico
278900130422     c                   exsr      contr_ferm
279000130422     c
279100130422     c                   if        cartrn=' '
279200130422     C                   MOVEL     MSG(9)        V1cMSG
279300130422     c                   eval      errMessage  = *on
279400130422     c                   eval      errGenerico = *on
279500130422     C                   GOTO      ENDTRN
279600130422     c                   endif
279700130419     C     ENDTRN        ENDSR
279800130617      *-------------------------------------------------------------------------
279900130617      * ordinamento sfl in base alle richieste
280000130617      *-------------------------------------------------------------------------
280100130617     C     sr_ordina     BEGSR
280200130617      * IMPOSTO ENTRATA AD 'A'
280300130624     c                   eval      C02rcd  = 1
280400130624     c                   eval      C02csr  = 1
280500130624     c                   clear                   d02trn
280600130617     C                   EVAL      s02nrr = 0
280700130617      *
280800130617     C                   CLEAR                   QLGSCB
280900130617     C                   CLEAR                   QLGSCB00
281000130617
281100130617     c                   select
281200130617     c* ordinamento per n.traino
281300130617     c                   when      w_ord=1
281400130617     ** 1 campo chiave.
281500130617     C                   EVAL      QLGNBRK    = 1
281600130617     C                   EVAL      QLGSP      = 1
281700130617     C                   EVAL      QLGSS      = %SIZE(s02trn)
281800130617     C                   EVAL      QLGDT      = carattere
281900130617     C                   EVAL      QLGSO      = Ascendente
282000130617     C                   EVAL      QLGKL(1)   = QLGSKL
282100130617     c* ordinamento per data/ora partenza
282200130617     c                   when      w_ord=2
282300130617     ** 2 campi chiave.
282400130617     C                   EVAL      QLGNBRK    = 2
282500130617     C                   EVAL      QLGSP      = 1 + %size(s02trn)
282600130617     C                   EVAL      QLGSS      = %SIZE(s02ggp)
282700130617     C                   EVAL      QLGDT      = carattere
282800130617     C                   EVAL      QLGSO      = Ascendente
282900130617     C                   EVAL      QLGKL(1)   = QLGSKL
283000130617     ** ragione sociale a posizione 1, carattere, 24 byte,
283100130617     C                   EVAL      QLGSP      = 1 + %size(s02trn) +
283200130617     c                                          %size(s02ggp)
283300130617     C                   EVAL      QLGSS      = %SIZE(s02hmp)
283400130617     C                   EVAL      QLGDT      = carattere
283500130617     C                   EVAL      QLGSO      = Ascendente
283600130617     C                   EVAL      QLGKL(2)   = QLGSKL
283700130617     c                   endsl
283800130617
283900130617      * Load other sort parameters.
284000130617     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
284100130617     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
284200130617     C                   EVAL      QLGRT     = 8
284300130617     C                   EVAL      QLGOKL    = 80
284400130617     C                   EVAL      QLGLKE    = 16
284500130617     C                   EVAL      QLGLSS    = 290
284600130617
284700130617      * Initialize Sort I/O API fields.
284800130617     C                   EVAL      QLGRL00  = QLGRL
284900130617     C                   EVAL      QLGRC00  = 1
285000130617     C                   CLEAR                   QUSEI
285100130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
285200130617
285300130617      * First step - Initialize the sort routine.
285400130617     C                   CALL      'QLGSORT'
285500130617     C                   PARM                    QLGSCB
285600130617     C                   PARM                    NotUsed
285700130617     C                   PARM                    NotUsed
285800130617     C                   PARM                    SizeList
285900130617     C                   PARM                    ReturnSize
286000130617     C                   PARM                    QUSEC
286100130617
286200130617      * Next step - Write records to I/O routine.
286300130617     C                   EVAL      QLGRT00 = Put
286400130617
286500130617     C                   DO        RrnLast       s02nrr
286600130617
286700130617     C     s02nrr        CHAIN     lst1s02
286800130617     c                   if        not %found
286900130617     c                   leave
287000130617     c                   endif
287100130617     ** Solo le righe con Selected = 'Y' sono riordinate,
287200130617     ** quindi per fare un ordinamento di tutte le righe
287300130617     ** metto 'Y' sempre.
287400130617     C                   EVAL      Selected = 'Y'
287500130617
287600130617     C                   CLEAR                   QUSEI
287700130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
287800130617     C                   CALL      'QLGSRTIO'
287900130617     C                   PARM                    QLGSCB00
288000130617     C                   PARM                    SflRcd
288100130617     C                   PARM                    NotUsed
288200130617     C                   PARM                    SizeList
288300130617     C                   PARM                    NotUsed
288400130617     C                   PARM                    QUSEC
288500130617
288600130617     C                   ENDDO
288700130617
288800130617      * Next step - Signal end of input, clear subfile for reload.
288900130617     C                   EVAL      QLGRT00 = EndPut
289000130617     C                   CLEAR                   QUSEI
289100130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
289200130617
289300130617     C                   CALL      'QLGSRTIO'
289400130617     C                   PARM                    QLGSCB00
289500130617     C                   PARM                    SflRcd
289600130617     C                   PARM                    NotUsed
289700130617     C                   PARM                    SizeList
289800130617     C                   PARM                    NotUsed
289900130617     C                   PARM                    QUSEC
290000130617
290100130617     c                   eval      SflDsp_N= *on
290200130617     c                   eval      SflDspCtl_N = *on
290300130617     C                   WRITE     lst1c02
290400130617     c                   eval      SflDspCtl_N = *off
290500130617     c                   eval      SflEnd      = *off
290600130617     c                   write     lst1p02
290700130617
290800130617      * Final step - Write the records back to the subfile.
290900130617     C                   EVAL      QLGRT00 = Get
291000130617     C                   DO        RrnLast       s02nrr
291100130617     C                   CLEAR                   QUSEI
291200130617     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
291300130617     C                   CALL      'QLGSRTIO'
291400130617     C                   PARM                    QLGSCB00
291500130617     C                   PARM                    NotUsed
291600130617     C                   PARM                    SflRcd
291700130617     C                   PARM                    QLGRL00
291800130617     C                   PARM                    NotUsed
291900130617     C                   PARM                    QUSEC
292000130617      *
292100130617     c                   eval      notrn=h02notrn
292200130617     c                   if        h02imm='S'
292300130617     c                   eval      Sott_fgt=*off
292400130617     c                   else
292500130617     c                   eval      Sott_fgt=*on
292600130617     c                   endif
292700130617     c                   if        s02aper='Sb'
292800130617     c                   eval      s02aper_rev=*on
292900131009     c                   else
293000131009     c                   eval      s02aper_rev=*off
293100130617     c                   endif
293200130617     C                   WRITE     lst1s02
293300130617     C                   ENDDO
293400130617
293500130617      *
293600130617     C                   ENDSR
293700130620      *-------------------------------------------------------------------------
293800130620      * ordinamento sfl di work in base alle richieste
293900130620      *-------------------------------------------------------------------------
294000130620     C     sr_ordinaW    BEGSR
294100130620      * IMPOSTO ENTRATA AD 'A'
294200130620     C                   EVAL      s02nrw = 0
294300130624     c                   clear                   d02trn
294400130620      *
294500130620     C                   CLEAR                   QLGSCB
294600130620     C                   CLEAR                   QLGSCB00
294700130620
294800130620     c                   select
294900130620     c* ordinamento per n.traino
295000130620     c                   when      w_ord=1
295100130620     ** 1 campo chiave.
295200130620     C                   EVAL      QLGNBRK    = 1
295300130620     C                   EVAL      QLGSP      = 1
295400130620     C                   EVAL      QLGSS      = %SIZE(s02trn)
295500130620     C                   EVAL      QLGDT      = carattere
295600130620     C                   EVAL      QLGSO      = Ascendente
295700130620     C                   EVAL      QLGKL(1)   = QLGSKL
295800130620     c* ordinamento per data/ora partenza
295900130620     c                   when      w_ord=2
296000130620     ** 2 campi chiave.
296100130620     C                   EVAL      QLGNBRK    = 2
296200130620     C                   EVAL      QLGSP      = 1 + %size(s02trn)
296300130620     C                   EVAL      QLGSS      = %SIZE(s02ggp)
296400130620     C                   EVAL      QLGDT      = carattere
296500130620     C                   EVAL      QLGSO      = Ascendente
296600130620     C                   EVAL      QLGKL(1)   = QLGSKL
296700130620     ** ragione sociale a posizione 1, carattere, 24 byte,
296800130620     C                   EVAL      QLGSP      = 1 + %size(s02trn) +
296900130620     c                                          %size(s02ggp)
297000130620     C                   EVAL      QLGSS      = %SIZE(s02hmp)
297100130620     C                   EVAL      QLGDT      = carattere
297200130620     C                   EVAL      QLGSO      = Ascendente
297300130620     C                   EVAL      QLGKL(2)   = QLGSKL
297400130620     c                   endsl
297500130620
297600130620      * Load other sort parameters.
297700130620     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
297800130620     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
297900130620     C                   EVAL      QLGRT     = 8
298000130620     C                   EVAL      QLGOKL    = 80
298100130620     C                   EVAL      QLGLKE    = 16
298200130620     C                   EVAL      QLGLSS    = 290
298300130620
298400130620      * Initialize Sort I/O API fields.
298500130620     C                   EVAL      QLGRL00  = QLGRL
298600130620     C                   EVAL      QLGRC00  = 1
298700130620     C                   CLEAR                   QUSEI
298800130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
298900130620
299000130620      * First step - Initialize the sort routine.
299100130620     C                   CALL      'QLGSORT'
299200130620     C                   PARM                    QLGSCB
299300130620     C                   PARM                    NotUsed
299400130620     C                   PARM                    NotUsed
299500130620     C                   PARM                    SizeList
299600130620     C                   PARM                    ReturnSize
299700130620     C                   PARM                    QUSEC
299800130620
299900130620      * Next step - Write records to I/O routine.
300000130620     C                   EVAL      QLGRT00 = Put
300100130620
300200130620     C                   DO        RrnLast       s02nrw
300300130620
300400130620     C     s02nrw        CHAIN     lst1s0w
300500130620     c                   if        not %found
300600130620     c                   leave
300700130620     c                   endif
300800130620     ** Solo le righe con Selected = 'Y' sono riordinate,
300900130620     ** quindi per fare un ordinamento di tutte le righe
301000130620     ** metto 'Y' sempre.
301100130620     C                   EVAL      Selected = 'Y'
301200130620
301300130620     C                   CLEAR                   QUSEI
301400130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
301500130620     C                   CALL      'QLGSRTIO'
301600130620     C                   PARM                    QLGSCB00
301700130620     C                   PARM                    SflRcd
301800130620     C                   PARM                    NotUsed
301900130620     C                   PARM                    SizeList
302000130620     C                   PARM                    NotUsed
302100130620     C                   PARM                    QUSEC
302200130620
302300130620     C                   ENDDO
302400130620
302500130620      * Next step - Signal end of input, clear subfile for reload.
302600130620     C                   EVAL      QLGRT00 = EndPut
302700130620     C                   CLEAR                   QUSEI
302800130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
302900130620
303000130620     C                   CALL      'QLGSRTIO'
303100130620     C                   PARM                    QLGSCB00
303200130620     C                   PARM                    SflRcd
303300130620     C                   PARM                    NotUsed
303400130620     C                   PARM                    SizeList
303500130620     C                   PARM                    NotUsed
303600130620     C                   PARM                    QUSEC
303700130620
303800130620     c                   eval      SflDspw_N= *on
303900130620     c                   eval      SflDspCtlw_N = *on
304000130620     C                   WRITE     lst1c0w
304100130620     c                   eval      SflDspCtlw_N = *off
304200130620
304300130620      * Final step - Write the records back to the subfile.
304400130620     C                   EVAL      QLGRT00 = Get
304500130620     C                   DO        RrnLast       s02nrw
304600130620     C                   CLEAR                   QUSEI
304700130620     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
304800130620     C                   CALL      'QLGSRTIO'
304900130620     C                   PARM                    QLGSCB00
305000130620     C                   PARM                    NotUsed
305100130620     C                   PARM                    SflRcd
305200130620     C                   PARM                    QLGRL00
305300130620     C                   PARM                    NotUsed
305400130620     C                   PARM                    QUSEC
305500130620      *
305600130620     c                   eval      notrn=h02notrn
305700130620     c                   if        h02imm='S'
305800130620     c                   eval      Sott_fgt=*off
305900130620     c                   else
306000130620     c                   eval      Sott_fgt=*on
306100130620     c                   endif
306200130620     c                   if        s02aper='Sb'
306300130620     c                   eval      s02aper_rev=*on
306400130620     c                   endif
306500130620     C                   WRITE     lst1s0w
306600130620     C                   ENDDO
306700130620
306800130620      *
306900130620     C                   ENDSR
307000080206
307100080206       //--------------------------------------------------------------
307200080206       //?Schiere a tempo di compilazione.
307300080206       //--------------------------------------------------------------
307400080206
307500080410** - MSG ------------------------------------------------------------------ -*
307600130121Data errata
307700130220Proposta ancora da inserire: possibile solo opzione "I-inserimento"
307800130514Immettere una "S" per indicare che la partenza è di xxxxxxxxxx
307900130423Immettere  i giorni di partenza
308000130314Opzione non ammmessa
308100130315Se premuto F7 non effettuare scelte con l'opzione "1"                         6
308200130315Utente non abilitato: chiamare CED di sede !!                                 7
308300130412Numero traino inesistente o non decorrente alla data specificata              8
308400130422La filiale gestione non è una fermata di carico. Traino errato                9
308500130424Codice trazionista errato                                                     10
308600130424Conducente non accreditato per il trazionista indicato                        11
308700130424Il conducente  ha terminato il rapporto col trazionista                       12
308800130424Targa MOTRICE non accredita per il trazionista indicato                       13
308900130424Esiste già una proposta nel giorno di xxxxxxxxxx x questo traino:ENTER forza  14
309000130424Conducente già presente nel giorno xxxxxxxxxx per traino xxxxxxx:ENTER forza  15
309100130426TargaMOTRICE già presente nel gior.xxxxxxxxxx per traino xxxxxxx:ENTER forza  16
309200130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare17
309300130502Il trazionista non è previsto nel traino indicato                             18
309400130503Il giorno non è previsto dal traino                                           19
