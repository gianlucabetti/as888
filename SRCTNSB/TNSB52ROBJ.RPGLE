000100991104      *------------------------------------------------------------------------*
000200990730      *  GESTIONE BOLLE DI SEDE - MANUTENZIONE BOLLA
000300120417      *
000400120417      * PER COMPILARE E' NECESSARIO AVERE IN LISTA LA FILTRAPRD (per FNARB)
000500990730      *------------------------------------------------------------------------*
000600990730     H DECEDIT('0,') DATEDIT(*DMY.)
000700151020     h option(*nodebugio)
000800990730      *------------------------------------------------------------------------*
000900990730     FTNSB52D   CF   E             WORKSTN INFDS(DEVDS1)
001000120220     F                                     maxdev(*file)
001100990730     F                                     SFILE(SB52DVM:MSGULT)
001200990730     F                                     SFILE(SB52S02:SFLULT)
001300990730     FAZORG01L  IF   E           K DISK
001400120221     FTNCSB03L  UF   E           K DISK    extfile(wlibsede)usropn
001500990730     FTNTAM01L  IF   E           K DISK
001600120221     FTITAS30C  UF A E           K DISK    extfile(wlibsede)usropn
001700120221     FTITASV1L  UF A E           K DISK    extfile(wlibsede)usropn
001800120221     FTITA7V0F  O    E             DISK    extfile(wlibsede)usropn
001900120221     FTITA730C  UF A E           K DISK    extfile(wlibsede)usropn
002000120221     FTITAA30C  UF A E           K DISK    extfile(wlibsede)usropn
002100120221     FTITA430C  uF a E           K DISK    extfile(wlibsede)usropn
002200120221     FTABEL00F  IF   E           K DISK    extfile(wlibsede)usropn
002300120221     FECCET30C  IF   E           K DISK    extfile(wlibsede)usropn
002400150929     fFiar531c  uf a e           k Disk    extfile(wlibsede)usropn
002500120416     fFnarb01l  if   e           k Disk    extfile(wlibfil)usropn
002600051010     Ffnevb04l  if   e           k disk
002700061114     fndppa01l  if   e           k disk
002800061114     fndcpa01l  uf   e           k disk
002801160921     FFIARBT0F  O  A E             DISK
002802160922     FFIARBU0F  O  A E             DISK
002803160922     FFNARBM0F  O  A E             DISK
002804160922     FFNARBG0F  O  A E             DISK
002805160923     FFNARBD0F  O  A E             DISK
002806160929     FFNARBK0F  O  A E             DISK
002807170628     ffnfta01l  if a e           k disk
002900990730      *------------------------------------------------------------------------*
003000990730      * SCHIERE
003100990730      *------------------------------------------------------------------------*
003200150414     D MSG             S             78    DIM(87) CTDATA PERRCD(1)             *MESSAGGI
003300990730     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
003400990730      *-------------------
003500990730      * DECODIFICA OPZIONE RICHIAMATA
003600990730      *-------------------
003700990730      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003800990730     D FFU             S              6    DIM(31)                              *FUNZIONE
003900990730     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
004000990730     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
004100990730      * TABELLA DESCRIZIONI A VIDEO
004200990730     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004300990730      * TABELLA TASTI FUNZIONALI PREMUTI
004400990730     D KEYS            S              1    DIM(40)
004500990730      * TABELLA CAPICONTO
004600990730      * FILIALI DI UNA STESSA AREA
004700990730     D L1              S              3    DIM(29)                              *FILIALI DELL'AREA
004800990730      *
004900990730      * DI COMODO
005000990730     D $CM             S              1    DIM(78)                              *DI COMODO
005100011023      *
005200990730     D K78             S              1    DIM(78)                              PER VISUAL MSG
005300020516      *
005400020516      * P.O. gestibili dall'utente
005500120328     D*$POG            s              3    dim(250) inz(*zeros)
005600150414      * filiali abilitate a cons.part "Z"=Expo
005700151120     d*fil_expo        s              3    dim(85)
005800141217
005900141217       //?Sk part.consegna con abilitazione PIN Code
006000141217     d skGMA           s              2a   dim(10)
006100011129
006200080922     d savnzm          s                   like(tasnzm)
006300080922     d savcam          s                   like(tascam)
006400080922     d savmct          s                   like(tasmct)
006500151008     d savimv          s                   Like(tasimv)
006600151019     d sav_tasimv      s                   Like(tasimv)
006700080922     d decsav          s                   like(§cvdec)
006800011129     d w0173           s             17  3
006900011129     d tot_importi     s             17  3
007000011129     d forza           s              1
007100011129     d sav_vidias      s                   like(w0173)
007200130124     d sav_tasias      s                   like(tasias)
007300130124     d sav_tasvas      s                   like(tasvas)
007400030703     d kAr5Trd         s                   Like(Ar5Trd)
007500030331     d wforza          s              1    Inz('0')
007600030331     d wnofatt         s              1    Inz('0')
007700030331     d wquatt9         s              4  0
007800120802     d savQUATT        s              4  0
007900040128     d wvar3ai         s              1
008000051010     D Wcev            s                   like(evbcev) inz('RIC')
008100061107     d wforzacdfm      s              1    inz(*off)
008200120508     d wforzacdfm1     s              1    inz(*off)
008300061107     d wforzacdfd      s              1    inz(*off)
008400120508     d wforzacdfd1     s              1    inz(*off)
008500071026     d forzapid2       s              1
008600071026     d sav1cpid        s                   Like(v1cpid)
008700071029     d forzapim2       s              1
008800071029     d sav1cpim        s                   Like(v1cpim)
008900071106     d s_tascpd        s                   Like(tascpd)
009000120323     d w0040           s              4  0
009100120323     d w003a           s              3
009200120326     d f15ksc          s                   like(w01ksc)
009300120326     d f15ctr          s                   like(w01ctr)
009400120326     d f15sco          s                   like(w01sco)
009500120326     d wper            s                   like(w01sco)
009600120327     d wf15            s              1
009700120323     d $finew          s              1
009800120323     d codut           s              1  0 inz(1)
009900120323     d paxccc          s                   like(acokcc)
010000120323     d paxdmt          s                   like(acorag)
010001160926     d savdfd          s                   like(v1cdfd)
010002160926     d savdfm          s                   like(v1cdfm)
010003160926     d savpid          s                   like(v1cpid)
010004160926     d savpim          s                   like(v1cpim)
010100120221     D Wlibsede        S             21
010200120416     D Wlibfil         S             21
010300120403     D Inx             S              2  0
010301170628     d w_ali           s                   like(d04ali)
010400011129
010500990730      *------------------------------------------------------------------------*
010600990730      * INPUT
010700990730      *------------------------------------------------------------------------*
010800990730      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
010900990730      *-------------------
011000990730     D DSFUN         E DS
011100990730     D  FUNRIG                 1     40
011200990730      *-------------------
011300990730      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
011400990730      *-------------------
011500990730     D DSMSG         E DS
011600020415      *-------------------
011700020415      * ds codice bolla da filiale
011800020415     D DS3a          E DS
011900110624      *-------------------
012000110624      * ds tipi inoltro
012100110624     D DS9A          E DS
012200990730      *-------------------
012300990730      * DS PER GESTIONE TASTI FUNZIONALI
012400990730      *-------------------
012500990730     D DSKEYS        E DS
012600990730     D  KEYRIG                 1     40
012700990730      *-------------------
012800990730      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
012900990730      *-------------------
013000990730     D                SDS
013100990730     D  VTCPGM                 1     10
013200990730     D  USER                 254    263                                         UTENTE
013300990730      *-------------------
013400990730      * RIDEFINIZIONE INFDS DEL FILE VIDEO
013500990730      *-------------------
013600990730     D DEVDS1          DS
013700990730      * CHIAVE FUNZIONALE BATTUTA
013800990730     D  KEY                  369    369
013900990730      * POSIZIONE CURSORE (IN HEX)
014000990730     D  CURSOR               370    371
014100990730      * RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
014200990730     D  FRTSFL               378    379B 0
014300990730      *------------------
014400990730     D KPJBA         E DS
014500061114     d  wsoc                  98    100
014600990730     D TNSB01DS      E DS
014700991102     D TIBS02DS      E DS
014800991102     D TIBS69DS      E DS
014900990730     d DS_cnaco      E DS                  extname(CNACO00F)
015000990730     d DS_cnind      E DS                  extname(CNIND00F)
015100990730     d DS_cnclp      E DS                  extname(CNCLP00F)
015200990730     d DS_fncls      E DS                  extname(FNCLS00F)
015300170727     D*DSQC2         E DS
015400990730     D DSCV          E DS
015500991102     D Dged          E DS
015600991102     D Dgei          E DS
015700990730     D DSTB          E DS
015800120706     D DS1B          E DS
015900990730     D DS15          E DS
016000990730     D DS1P          E DS
016100990730     D DS$2          E DS
016200990730     D DSCC          E DS
016300020327     D DSCB          E DS
016400000126     D OG143         E DS
016500990730     D DS7T          E DS                  inz
016600021125     d Ddfe          e ds
016700991102      *
016800110630     d dtasflo       e ds
016900991102     D DTAS          E DS
017000030729     d  §tasncl      e                     extfld(tasncl)
017100030204     d  §tasflo      e                     extfld(tasflo)
017200060922     D DTAS01        E DS
017300991102     D DTASV         E DS
017400990730     D  TSV                    1     20    DIM(20)                              SIGLA "VARIA"
017500990730     D  TVA                   21    140P 3 DIM(20)                              IMPORTO "VARIA"
017600990730     D  TEV                  141    160    DIM(20)                              ESENZIONE "VARIA"
017700011106
017800011106     d sav_DTASV     E DS                  extname(dtasv)
017900011106     d                                     prefix(s_)
018000150929     D DTASpes       E DS
018100991102      *
018200991103     D YEURCODS      E DS
018300991103     D FNLV04DS      E DS
018400990730     D FNLV16DS      E DS
018500040127     D DLV1601       E DS
018600990730     D FNLV18DS      E DS
018700990730     D FNLV20DS      E DS
018800160229     D FNLV59DS      E DS                  INZ
018900990730     D DS7PQRS       E DS
019000990730     D TNSC40DS      E DS
019100990730      * DS PER TISI30R - INTERROGAZIONE LOCALITA'
019200990730     D TISI00DS      E DS
019300990730      * DS PER FNLV13R - CONTROLLO DI UN CAP
019400990730     D FNLV13DS      E DS
019500990730      * DS PER FNLV14R - CONTROLLO DI INDIRIZZO COMPLETO  SENZA CAP
019600990730     D FNLV14DS      E DS
019700990730      * DS PER TISI95R - CONTROLLO DI CAP
019800990730     D TISI95DS      E DS
019900011129
020000011129     d trul22ds      e ds
020100160229     d trul27ds1     e ds
020200160229     d trulc7ds      e ds
020300030401     d Tnsb51ds      e ds
020400020516
020500120328     D*TRUL31DS      e ds                  inz
020600020516     D TIBS34DS      e ds                  inz
020700020516     D DDATIUTE      e ds                  inz
020800020516     D AZUTEDS       e ds                  inz extname(AZUTE00F)
020900020516     D DLAT          e ds                  inz
021000020516     D DUTE01        e ds                  inz
021100071026     D dvpodeco      e ds
021200120328     d* Controllo abilitazioni utente
021300120328     d tntaa1ds      e ds
021400120403     d fnlv55ds      e ds
021500120403     d fnlr48ds      e ds
021600120403     d dsarbd        e ds
021700120403     d dsarbk        e ds
021800120403     d dsarbg        e ds
021900120403     d darbt         e ds
022000120403     D  VSV                   44     63
022100120403     D                                     DIM(20)
022200120403     D  VVA                   64    183P 3
022300120403     D                                     DIM(20)
022400130124     D fnlvv1ds      E DS
022500030204
022600030204     d dAr5Ban       e ds
022700030703     d dAr5Bnb       e ds
022800150929     D** ds per memorizzare peso usato in tassazione FIAR5
022900150929     D Dar5Fat       E DS
023000160104     D** ds per recupero stampa packing List  FIAR5 record GEN
023100160104     d dAR5gen       e ds                  inz
023200160104
023300151002
023400030411     d ds5a          e ds
023500151104     d xcfiva1ds     e ds
023600071026     d  ivaeu                  3      4
023700061108     d dta4q         e ds
023800061108     d wdta4q        e ds                  extname(dta4q) prefix(w)
023900141217
024000141217       //?Ds tabella 7R - part.consegna
024100141217     d ds7R          e ds
024200011129
024300990730      *------------------
024400990730      * PARAMETRI CHIAMATA 'TNSB07R'
024500990730      *------------------
024600990730     D PARA07          DS
024700990730     D  P07AAS                 1      4  0
024800990730     D  P07MGS                 5      8  0
024900990730     D  P07LNP                 9     11  0
025000990730     D  P07NRS                12     13  0
025100990730     D  P07NSP                14     20  0
025200990730     D  P07TBL                21     22
025300990730     D  P07DTB                23     42
025400990730     D  P07LNA                43     45  0
025500990730     D  P07DLA                46     64
025600990730     D  P07RMN                65     79  0
025700990730     D  P07F03                80     80
025800990730     D  P07RSU                81    100
025900990730      *------------------
026000990730      * DATA DI SPEDIZIONE BOLLA
026100990730      *------------------
026200990730     D                 DS
026300990730     D  TA$DSP                 1      8  0
026400990730     D  TASAAS                 1      4  0
026500990730     D  TASMGS                 5      8  0
026600071025      *------------------
026700071025      * PARTITA I.V.A.
026800071025      *------------------
026900071025     D                 DS
027000071025     D  V1CPIM                 1     16
027100071025     D  V1CP2M                 1      2
027200071025     D  V1CP1M                 3     16
027300071025     D                 DS
027400071025     D  V1CPID                 1     16
027500071025     D  V1CP2D                 1      2
027600071025     D  V1CP1D                 3     16
027700990730      *
027800990730     D CPDC            C                   CONST('su Piano dei Conti')
027900990730      *------------------------------------------------------------------------*
028000990730      * DEFINIZIONE CAMPI
028100990730      *------------------------------------------------------------------------*
028200990730     D  XX             s              3  0
028300990730     D  WA             s              3  0
028400990730     D  WB             s              3  0
028500990826     D  WC             s              3  0
028600990805     D  w0020          s              2  0
028700990805     D  w0030          s              3  0
028800990805     D  w0060          s              6  0
028900990730     D  w0133          s             13  3
029000990730     D  Wccm4          s              4  0
029100990730     D  Wquatt         s              4  0
029200990730     D  DATEU          s              8  0
029300990730     D  Wtime          s             14  0
029400990730     D  DATAA          s              4  0
029500990730     d  SFLULT         s              4  0
029600021122     d  DueCtr         s              2  0
029700030411     d  dtpularr       s              8  0
029800990730
029900991103     D  Wconve         s              1
030000991103     D  WTEVdif        s              1
030100990730     D  WTEVinl        s              1
030200990812     D  wprepagato     s              1
030300990812     D  w001A          s              1
030400990730     D  wmodif         s              1
030500990730     D  wtitaa         s              1
030600061107     d  wtita4q        s              1    inz(*off)
030700990730     D  win62          s              1
030800990730     D  win63          s              1
030900990730     D  win79          s              1
031000990730     D  wcts           s              1
031100990730     D  wcli           s              1
031200990730     D  wpgm           s             10
031300990730     D  wsf20          s              1
031400150929     D  wsf22          s              1
031500990730     D  wlv17          s              1
031600990730     D  wlv51          s              1
031700990730     D  wlv03          s              1
031800990730     D  wbs69          s              1
031900990730     D  wwfim          s              1
032000030626     D  wtassa         s              1
032100030626     D  flgvaa         s              1
032200011023
032300011023     d  wflgdiv        s              1    inz('0')
032400011023     d  wmitie         s              1
032500011023     d  flgvrn         s              1
032600021122     d  wpeso          s              1
032700080922     d  wmitor         s              1
032800020516
032900020516     d wKSCa           S              7    inz
033000120328     d*wPOa            s              3    inz
033100120328     d wABI            s                   like(uteaut) inz
033200020516     D WsavMIT         S                   LIKE(V1Cmit) inz
033300020516     D WsavDES         S                   LIKE(V1Cdes) inz
033400030331     D WsavDES9        S                   LIKE(V1Cdes) inz
033500990730
033600991103     D wdiv            S                   LIKE(tasdiv)
033700991103     D mempor          S                   LIKE(taspor)
033800991103     D savfin          S                   LIKE(v1cfin)
033900990730     D KSBaas          S                   LIKE(CSBaas)
034000990730     D KSBlnp          S                   LIKE(CSBlnp)
034100990730     D KSBnrs          S                   LIKE(CSBnrs)
034200990730     D KSBnsp          S                   LIKE(CSBnsp)
034300990730     D KCTaas          S                   LIKE(CETaas)
034400990730     D KCTlnp          S                   LIKE(CETlnp)
034500990730     D KCTnrs          S                   LIKE(CETnrs)
034600990730     D KCTnsp          S                   LIKE(CETnsp)
034700990730     D KCTtbl          S                   LIKE(CETtbl)
034800990730     D KAMksc          S                   LIKE(TAMksc)
034900020516     D KTBkut          S                   LIKE(TBLkut) inz(1)
035000990730     D KTBcod          S                   LIKE(TBLcod)
035100990730     D KTBkey          S                   LIKE(TBLkey)
035200990730     D KASaas          S                   LIKE(TASaas)
035300990730     D KASlnp          S                   LIKE(TASlnp)
035400990730     D KASnrs          S                   LIKE(TASnrs)
035500990730     D KASnsp          S                   LIKE(TASnsp)
035600990730     D KAStbl          S                   LIKE(TAStbl)
035700990730     D $kcc            S                   LIKE(ACOkcc)
035800990730     D WOP0            S                   LIKE(D01OP0)
035900990730     D WOP1            S                   LIKE(D01OP1)
036000990730     D Waas            S                   LIKE(D01aas)
036100990730     D Wlnp            S                   LIKE(D01lnp)
036200020328     D Wlna            S                   LIKE(D01lna)
036300990730     D Wnrs            S                   LIKE(D01nrs)
036400990730     D Wnsp            S                   LIKE(D01nsp)
036500990730     D Wtbl            S                   LIKE(D01tbl)
036600990730     D Wdsp            S                   LIKE(D01dsp)
036700990730     D Wdtv            S                   LIKE(D01dtv)
036800990730     D Worv            S                   LIKE(D01orv)
036900990730     D Wpru            S                   LIKE(D01pru)
037000990730     D Wtastbl         S                   LIKE(TAStbl)
037100990730     D Wtasvlf         S                   LIKE(TASvlf)
037200990730     D Wtasfiv         S                   LIKE(TASfiv)
037300990730     D $ksc            S                   LIKE(ACOksc)
037400130404     D $ksctas         S                   LIKE(ACOksc)
037500990730     D $kscd           S                   LIKE(ACOrag)
037600160229     D $ctd            S                   LIKE(olv59dfs)
037700160229     D $tks            S                   LIKE(olv59tks)
037800160301     D $prg            S                   LIKE(olv59prg)
037900160229     D $div            S                   LIKE(olv59div)
038000990730     D Wvlt            S                   LIKE(§15vlt)
038100990730     D WLOM            S                   LIKE(V1Clom)
038200990730     D WCAM            S                   LIKE(V1Ccam)
038300990730     D WTASfic         S                   LIKE(TASfic)
038400990730     D Wv1cmit         S                   LIKE(V1Cmit)
038500990730     D KASTRC          S                   LIKE(TAAtrc)
038600990730     D W§TBRBL         S                   LIKE(§TBRBL)
038700120706     D W§TBFCB         S                   LIKE(§TBfcb)
038800990730     D W§TBTPO         S                   LIKE(§TBTPO)
038900120705     D W§Tbmod1        S                   LIKE(§TBmod1)
039000120705     D W§Tbmod2        S                   LIKE(§TBmod2)
039100120705     D W§Tbmod3        S                   LIKE(§TBmod3)
039200120706     D W§Tbmod4        S                   LIKE(§TBmod4)
039300990730     D VARcam          S                   LIKE(E14cap)
039400990730     D VARlom          S                   LIKE(E14loc)
039500990730     D VARprm          S                   LIKE(E14prv)
039600990730     D VARnam          S                   LIKE(E14nar)
039700990730     D VARcad          S                   LIKE(E14cap)
039800990730     D VA2Cad          S                   LIKE(E14cap)
039900990730     D VARlod          S                   LIKE(E14loc)
040000990730     D VA2lod          S                   LIKE(E14loc)
040100990730     D VARprd          S                   LIKE(E14prv)
040200990730     D VARnad          S                   LIKE(E14nar)
040300021122     d Wfie            S                   Like(O27Fie)
040400021122     d LnaNtw          s                   Like(§OgNtw)
040500061114
040600061114     d wctb            s                   like(ppactb) inz('CG')
040700061114     d wkcc            s                   like(ppakcc) inz('000151')
040800061114     d wksc            s                   like(ppaksc) inz('00000000')
040900061114     d wdft            s                   like(ppadtpar)
041000061114     d wnft            s                   like(ppanrpar)
041100061114     d wfiv            s                   like(ppaserpar) inz
041200990730
041300990730     D DATA_eur        S               D   DATFMT(*eur)
041400130206     d parvar          s            224
041500141217
041600141217     d kk              s              3  0 inz
041700141217
041800990730      *------------------------------------------------------------------------*
041900990730      * INPUT DEI FILES
042000990730      *------------------------------------------------------------------------*
042100990730     ITITAS000      81
042200990730     ITITAS010      82
042300990730     ITITASP00      83
042400990730      *------------------------------------------------------------------------*
042500990730      * MAIN LINE
042600990730      *------------------------------------------------------------------------*
042700990730      * INDICATORI UTILIZZATI:
042800020516      * 01   Importo da Assicurare inserito dalla Fatturazione
042900030709      * 02   Proteggo importo da assicurare e divisa
043000030729      * 03   Visualizzo Colli Originali
043100990730      * 20...44   UTILI ALLO SCHEMA LOGICO DEL PROGRAMMA
043200020516      *   22 on > Protezione di campi a video per utente con
043300020516      *           abilitazione valida e diversa da "AZ".
043400990730      * 47   ON   C'E' PARTICOLARITA' GIACENZA
043500990730      * 48   ON   C'E' PARTICOLARITA' CONSEGNA
043600990730      * 49   ON   C'E' PARTICOLARITA' VARIE
043700990730      * 50...80   ERRORI SUI FORMATI VIDEO
043800990730      * 62   MODIFICATO IMP. DA ASSICURARE IN SB52C02
043900990730      * 63   MODIFICATA DIVISA IMP. DA ASSICURARE IN SB52C02
044000991103      * 74   ERRORE DIVISA
044100991103      * 78   ERRORE PORTO
044200990730      * 79   MODIFICATO VOLUME DA FATTURARE IN SB52D01
044300990730      * 80   MODIFICATO FORMATO RCD SB52D01 o SB52C02
044400990730      * 81   ON   LETTURA RECORD DA TITAS00F
044500990730      * 82   ON   LETTURA RECORD DA TITAS10F
044600990730      * 83   ON   LETTURA RECORD DA TITASP0F
044700990730      * 84   MODIFICATO FORMATO RCD SB52S02
044800061114      * 85   se fattura di filiale proteggo sempre i dati del cliente mittente e del destinatario
044900020516      * 87   ON   MITTENTE CODIFICATO
045000020516      * 88   ???
045100990730      * 89   ON   SPEDIZIONE IN FRANCO
045200990805      *      OFF  SPEDIZIONE IN ASSEGNATO
045300061107      * 90   ON   PROTEGGO COD.FISCALE MITTENTE
045400120706      * 91   ON   VISUALIZZAZIONE per bolla già contabilizzata
045500990730      * 92   ON   NON C'E' INDIRIZZO/LOCALITA' MITTENTE
045600061107      * 93   ON   PROTEGGO COD.FISCALE DESTINATARIO
045700990730      * 94   ON   PROTETTO COD.DESTINATARIO (NON C'E')
045800990730      * 95   ON   PROTETTO VOLUME DA FATTURARE
045900990730      * 96   ON   NON VISUALIZZATO TOTALE FATTURA
046000990805      * 97   ON   PROTEGGO VARIE NEL SUBFILE
046100990730      * 98        GENERICO, UTILIZZABILE A PIACERE
046200990730      * 99        GENERICO, UTILIZZABILE A PIACERE
046300990730      *------------------------------------------------------------------------*
046400990730      *
046500990730      * SOLO LA PRIMA VOLTA ENTRO NELLA ROUTINE DI INIZIALIZZAZIONE.
046600990730      * NON L'HO CHIAMATA *INZSR POICHE' IN USCITA DA *INZSR VENGONO
046700990730      * AZZERATI GLI INDICATORI DI TIPO RECORD (SUI FILE).
046800990730     C                   EXSR      INIZIA
046900990730      *
047000990730      * SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO DAL PGM CON MSG DI ERRORE AL CHIAMANTE
047100990730     C     $FINE         IFEQ      '1'
047200990730     C                   MOVEL     '1'           D01ERR
047300990730      *
047400990730      * SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
047500990730     C                   ELSE
047600990730      *
047700990730      * CICLO FINO A CHE:
047800990730     c                   DOW       $FINE = '0'
047900990730     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
048000990730      *
048100990730      * CONTROLLA GESTIONE VIDEO
048200990730     c                   SELECT
048300990730     C                   WHEN      $GES = 'V1'
048400990730     C                   exsr      GESV1
048500990730     C                   WHEN      $GES = 'V2'  and  $FINE = '0'
048600990730     C                   exsr      GESV2
048700990730     C                   ENDSL
048800990730      *
048900990730     C                   ENDDO
049000990730     C                   ENDIF
049100990730      *
049200990730      * SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
049300990730     c                   SELECT
049400990730     C                   WHEN      $F03 = '1'
049500990730     C                   movel     '1'           D01F03
049600990730     C                   WHEN      $F12 = '1'
049700990730     C                   movel     '1'           D01F12
049800990730     C                   ENDSL
049900990730      * FINE PGM
050000990730     C                   EXSR      CHIUDI
050100990730     C                   MOVEL     $ERR          D01ERR
050200990730     C                   MOVEL     TNSB01DS      KPJBU
050300990730      *
050400990730     C                   eval      *inlr = *on
050500990730      *------------------------------------------------------------------------*
050600990730      * CHIUDI  - CHIUDE EVENTUALI PGM CON USCITA RETURN
050700990730      *------------------------------------------------------------------------*
050800990730     C     CHIUDI        BEGSR
050900990730      *
051000990730     C     WLV03         IFNE      *BLANKS
051100990730     C                   MOVEL     'C'           D04TLA                         'C'=CHIUDI
051200990730     C                   CALL      'FNLV04R'                            99
051300990730     C                   PARM                    FNLV04DS
051400990730     C                   PARM                    DTASV
051500990730     C                   ENDIF
051600990730      *
051700990730     C     WLV17         IFNE      *BLANKS
051800990730     C                   MOVEL     'C'           D17TLA                         'C'=CHIUDI
051900990730     C                   CALL      'FNLV16R'                            99
052000990730     C                   PARM                    FNLV16DS
052100990730     C                   PARM                    DTASV
052200990730     C                   ENDIF
052300990730      *
052400990730     C     WLV18         IFNE      *BLANKS
052500990730     C                   MOVEL     'C'           D18TLA                         'C'=CHIUDI
052600990730     C                   CALL      'FNLV18R'                            99
052700990730     C                   PARM                    FNLV18DS
052800990730     C                   ENDIF
052900990730      *
053000990730     C     WLV20         IFNE      *BLANKS
053100990730     C                   MOVEL     'C'           D20TLA                         'C'=CHIUDI
053200160229     c                   movel(p)  fnlv20ds      kpjbu
053300990730     C                   CALL      'FNLV20R'                            99
053400160229     C                   PARM                    kpjba
053500990730     C                   ENDIF
053600990730      *
053700990730     C     WLV51         IFNE      *BLANKS
053800160229     C                   MOVEL     'C'           ilv59tla                       'C'=CHIUDI
053900160229     C                   CALL      'FNLV59R'                            99
054000160229     C                   PARM                    FNLV59DS
054100990730     C                   ENDIF
054200990730      *
054300990730     C     WSF20         IFNE      *BLANKS
054400991102     C                   MOVEL     'C'           TASTLA                         'C'=CHIUDI
054500990730     C                   CALL      'TNSF20R'                            99
054600990730     C                   PARM                    KPJBA
054700991102     C                   PARM                    DTAS
054800991102     C                   PARM                    DTASV
054900990730     C                   ENDIF
055000150929      *
055100150929     C     WSF22         IFNE      *BLANKS
055200150929     C                   MOVEL     'C'           TASPTLA                         'C'=CHIUDI
055300150929     C                   CALL      'TNSF22R'                            99
055400150929     C                   PARM                    KPJBA
055500150929     C                   PARM                    DTAS
055600150929     C                   PARM                    DTASV
055700150929     C                   PARM                    DTASpes
055800150929     C                   ENDIF
055900990730      *
056000990730     C     WLV13         IFNE      *BLANKS
056100990730     C                   MOVEL     'C'           I13TLA                         'C'=CHIUDI
056200990730     C                   CALL      'FNLV13R'                            99
056300990730     C                   PARM                    KPJBA
056400990730     C                   PARM                    FNLV13DS
056500990730     C                   PARM                    TISI95DS
056600990730     C                   ENDIF
056700990730      *
056800990730     C     WLV14         IFNE      *BLANKS
056900990730     C                   MOVEL     'C'           I14TLA
057000990730     C                   CALL      'FNLV14R'
057100990730     C                   PARM                    KPJBA
057200990730     C                   PARM                    FNLV14DS
057300990730     C                   ENDIF
057400990730      *
057500990730     C     WBS69         IFNE      *BLANKS
057600990730     C                   MOVEL     'C'           I69TLA
057700990730     C                   CALL      'TIBS69R'
057800990730     C                   PARM                    TIBS69DS
057900990730     C                   ENDIF
058000990730      *
058100991102     c                   clear                   tibs02ds
058200991102     c                   movel     'C'           t02tla
058300991102     c                   call      'TIBS02R'
058400991102     c                   parm                    kpjba
058500991102     c                   parm                    tibs02ds
058600021122
058700160229     c                   Clear                   Trul27Ds1
058800021122     c                   Eval      I27Tla = 'C'
058900160229     c                   Call      'TRUL27R1'
059000160229     c                   Parm                    Trul27Ds1
059100160229     c
059200160229     c                   Clear                   Trulc7Ds
059300160229     c                   Eval      ic7Tla = 'C'
059400160229     c                   Call      'TRULC7R'
059500160229     c                   Parm                    TrulC7Ds
059600120328
059700120328     C                   clear                   tntaa1ds
059800120328     C                   movel     'C'           Itaa1Tla
059900120328     C                   movel(p)  tntaa1ds      kpjbu
060000120328     C                   CALL      'TNTAA1C'
060100120328     C                   PARM                    KPJBA
060200130124
060300130124     C                   MOVEL     'C'           iv1tla
060400130124     c                   movel     fnlvv1ds      kpjbu
060500130124     C                   CALL      'FNLVV1R'
060600130124     C                   PARM                    kpjba
060700991102      *
060800990730     C                   ENDSR
060900990730      *------------------------------------------------------------------------*
061000990730      * CARV1  - CARICA GESTIONE PRIMO VIDEO
061100990730      *------------------------------------------------------------------------*
061200990730     C     CARV1         BEGSR
061300990730      *
061400990730     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
061500990730      * TOLGO FUNZIONE F06=AGGIORNAMENTO
061600990730     C                   MOVEL     FUN(6)        FUNRIG                         *AGGIORNAMENTO
061700990730     C                   MOVEL     'N'           FUNASG
061800990730     C                   MOVEL     'N'           FUNASI
061900990730     C                   MOVEL     'N'           FUNAFG
062000990730     C                   MOVEL     'N'           FUNAFG
062100990730     C                   MOVEL     FUNRIG        FUN(6)
062200990730      * TOLGO FUNZIONE F14=TASSAZIONE
062300990730     C                   MOVEL     FUN(14)       FUNRIG                         *TASSAZIONE
062400990730     C                   MOVEL     'N'           FUNASG
062500990730     C                   MOVEL     'N'           FUNASI
062600990730     C                   MOVEL     'N'           FUNAFG
062700990730     C                   MOVEL     'N'           FUNAFG
062800990730     C                   MOVEL     FUNRIG        FUN(14)
062900120327      * TOLGO FUNZIONE F15=TASSAZIONE con cliente/tariffa
063000120327     C                   MOVEL     FUN(15)       FUNRIG                         *TASSAZIONE
063100120327     C                   MOVEL     'N'           FUNASG
063200120327     C                   MOVEL     'N'           FUNASI
063300120327     C                   MOVEL     'N'           FUNAFG
063400120327     C                   MOVEL     'N'           FUNAFG
063500120327     C                   MOVEL     FUNRIG        FUN(15)
063600990730      *
063700990730     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
063800990730     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
063900990730      *
064000990730     C                   WRITE     SB52DVT                                      *TESTATA
064100990730     C                   WRITE     SB52DVF                                      *FUNZIONI
064200990730      *
064300990730     C                   ENDSR
064400990730      *------------------------------------------------------------------------*
064500990730      * GESV1  - GESTIONE PRIMO VIDEO
064600990730      *------------------------------------------------------------------------*
064700990730     C     GESV1         BEGSR
064800990730      *
064900990730     C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
065000990730     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
065100990730     C                   ENDIF
065200990730      *
065300990730     C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
065400990730     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
065500990730     C                   ENDIF
065600990730      *
065700120221     C****               EXFMT     SB52D01
065800120221     C                   write     SB52D01
065900120221     c                   read(E)   tnsb52d
066000990730      *
066100990730     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
066200120221
066300120221     c                   if        %error
066400120221     c                   eval      key=F12
066500120221     c                   endif
066600990730      *
066700990730     C                   SELECT
066800990730     C                   WHEN      $ERR = '1'
066900990730     c                   WHEN      KEY = ENTER
067000990730     C                   exsr      ENTV1                                        *GEST ENTER VIDEO V1
067100030401     c                   WHEN      KEY = F7
067200030401     C                   exsr      F07GES
067300990730     c                   WHEN      KEY = F10
067400990730     C                   exsr      F10GES
067500990730     c                   WHEN      KEY = F16
067600990730     C                   exsr      F16GES
067700990730     c                   WHEN      KEY = F18
067800990730     C                   exsr      F18GES
067900990730     c                   WHEN      KEY = F22
068000990730     C                   exsr      F22GES
068100990730     C                   OTHER
068200990730     C                   exsr      FUNSTD                                       *GEST.TASTI STANDARD
068300990730     C                   ENDSL
068400990730      *
068500990730     C                   ENDSR
068600990730      *------------------------------------------------------------------------*
068700990730      * CARV2  - CARICA GESTIONE SECONDO VIDEO
068800990730      *------------------------------------------------------------------------*
068900990730     C     CARV2         BEGSR
069000990730      *
069100990730     C                   MOVEL     'V2'          $GES                           *GESTIONE ATTUALE
069200990730      *
069300990730      * ABILITO FUNZIONE F06=AGGIORNAMENTO
069400020730      * anche per i P.O.
069500990730     C                   MOVEL     FUN(6)        FUNRIG                         *AGGIORNAMENTO
069600990730     C                   MOVEL     'S'           FUNASG
069700990730     C                   MOVEL     'S'           FUNASI
069800020730     C                   MOVEL     'S'           FUNAFG
069900020730     C                   MOVEL     'S'           FUNAFG
070000990730     C                   MOVEL     FUNRIG        FUN(6)
070100990730      *
070200061107      * ABILITO F14=TASSAZIONE
070300020717     C                   IF        *in91 = *off
070400990730     C                   MOVEL     FUN(14)       FUNRIG                         *TASSAZIONE
070500990730     C                   MOVEL     'S'           FUNASG
070600990730     C                   MOVEL     'S'           FUNASI
070700020730     C                   MOVEL     'S'           FUNAFG
070800020730     C                   MOVEL     'S'           FUNAFG
070900990730     C                   MOVEL     FUNRIG        FUN(14)
071000990730     C                   ENDIF
071100120323      * ABILITO F15=Cliente/Tariffa Se proveniente da Gestione manca tariffa
071200120327     C                   IF        *in91 = *off and %subst(d01op0:1:1)='T'
071300120323     C                   MOVEL     FUN(15)       FUNRIG                         *TASSAZIONE
071400120323     C                   MOVEL     'N'           FUNASG
071500120323     C                   MOVEL     'N'           FUNASI
071600120323     C                   MOVEL     'S'           FUNAFG
071700120323     C                   MOVEL     'S'           FUNAFG
071800120323     C                   MOVEL     FUNRIG        FUN(15)
071900120323     C                   ENDIF
072000030331      * emetto msg info a video
072100030331     c                   If        wforza = *Off and wNofatt = *On
072200030331     c                   Eval      wforza = *On
072300030331     c                   Eval      $msg = Msg(65)
072400030331     c                   Eval      $err = '1'
072500030331     c                   ExSr      CARMSG
072600030331     c                   EndIf
072700990730      *
072800990730     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
072900990730     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
073000990730      *
073100990730     C                   WRITE     SB52DVT                                      *TESTATA
073200990730     C                   WRITE     SB52DVF                                      *FUNZIONI
073300990730      *
073400990730     C                   ENDSR
073500990730      *------------------------------------------------------------------------*
073600990730      * GESV2  - GESTIONE SECONDO VIDEO
073700990730      *------------------------------------------------------------------------*
073800990730     c     GESV2         BEGSR
073900990730      *
074000990730     C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
074100990730     C                   EXSR      CARV2                                        *CARICA VIDEO MOVIM
074200990730     C                   ENDIF
074300990730      *
074400990730     C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
074500990730     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
074600990730     C                   ENDIF
074700990730      *
074800990826     c                   z-add     1             sflult
074900120221     C*****              EXFMT     SB52C02
075000120221     C                   write     SB52C02
075100120221     c                   read(E)   tnsb52d
075200990730      *
075300990730     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
075400120221
075500120221     c                   if        %error
075600120221     c                   eval      KEY=F3
075700120221     c                   endif
075800990730      *
075900990730     C                   SELECT
076000990730     C                   WHEN      $ERR <> '0'
076100990730     C                   WHEN      KEY = ENTER
076200990730     C                   exsr      ENTV2                                        *GEST ENTER VIDEO V2
076300990730     C                   WHEN      KEY = F6
076400990730     C                   exsr      F06GE2
076500030401     C                   WHEN      KEY = F7
076600030401     C                   exsr      F07GES
076700990730     C                   WHEN      KEY = F10
076800990730     C                   exsr      F10GES
076900990730     C                   WHEN      KEY = F12
077000990730     C                   exsr      F12GE2                                       *GEST F12 PRECEDENTE
077100990730     C                   WHEN      KEY = F14
077200990730     C                   exsr      F14GES
077300120323     C                   WHEN      KEY = F15
077400120323     C                   exsr      F15GES
077500990730     C                   WHEN      KEY = F16
077600990730     C                   exsr      F16GES
077700990730     C                   WHEN      KEY = F18
077800990730     C                   exsr      F18GES
077900990730     C                   WHEN      KEY = F22
078000990730     C                   exsr      F22GES
078100990730     C                   WHEN      KEY = ROLLUP
078200990730     C                   exsr      RUPV2
078300990730     C                   WHEN      KEY = ROLLDN
078400990730     C                   exsr      RDNV2
078500990730     C                   OTHER
078600990730     C                   exsr      FUNSTD                                       *GEST.TASTI STANDARD
078700990730     C                   ENDSL
078800990730      *
078900990730     C                   ENDSR
079000990730      *------------------------------------------------------------------------*
079100990730      * ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
079200990730      *------------------------------------------------------------------------*
079300990730     C     ENTV1         BEGSR
079400990730      *
079500990730      * CONTROLLA I CAMPI IMMESSI
079600071106     c                   clear                   winizia           1
079700990730     C                   EXSR      DECOD1
079800990730      *
079900990730     C     $ERR          IFEQ      '0'                                          *NO ERRORE
080000990730     C                   MOVEL     'V2'          $GES                           *VIDEO 2
080100990730     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
080200011023      * Prima richiamo la routine per impostare dei dati di dft
080300011023      * ma solo al momento dell'emissione della seconda videata
080400011023     C                   exsr      decod
080500990826     C                   EXSR      DECOD2
080600990730     C                   ENDIF
080700990730      *
080800990730     C                   ENDSR
080900990730      *------------------------------------------------------------------------*
081000990730      * ENTV2  - GESTIONE TASTO ENTER SECONDO VIDEO
081100990730      *------------------------------------------------------------------------*
081200990730     C     ENTV2         BEGSR
081300990730      *
081400990730      * CONTROLLA I CAMPI IMMESSI
081500990730     C                   EXSR      DECOD2
081600990730      *
081700990730     C                   ENDSR
081800990730      *------------------------------------------------------------------------*
081900990730      * F06GE2 -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
082000990730      *------------------------------------------------------------------------*
082100990730     C     F06GE2        BEGSR
082200990730      *
082300990730      * CONTROLLA I CAMPI IMMESSI
082400071106     c                   clear                   winizia
082500990730     C                   EXSR      DECOD1
082600990730      *
082700990730     C                   if        $ERR = '0'
082800990730     C                   EXSR      DECOD2
082900130124     C                   endif
083000990730      *
083100990730     C     $ERR          IFEQ      '0'
083200990730      *
083300990730     C                   Z-ADD     V1CAAS        KASAAS                         *ANNO
083400990730     C                   Z-ADD     V1CLNP        KASLNP                         *LNP
083500990730     C                   Z-ADD     V1CNRS        KASNRS                         *SERIE
083600990730     C                   Z-ADD     V1CNSP        KASNSP                         *SPEDIZIONE
083700990730     C                   MOVEL     Wtastbl       KASTBL                         *TIPO BOLLA ORIGINAL
083800990730      *
083900990730     C                   eval      *in81 = *off
084000990730     C                   eval      *in82 = *off
084100990730     C                   eval      *in83 = *off
084200071106     c                   clear                   wcpd2bo           1
084300990730      *
084400990730     C     KEYTAS        CHAIN     TITAS30C
084500990730     C                   IF        %FOUND
084600990730      *
084700990730      * AGGIORNO LA BOLLA SOLO SE VARIATE LE VIDEATE
084800990730     C     WMODIF        IFEQ      '1'                                          *SOLO SE MODIFICATA
084900161024     C****               EXSR      WRIVAR                                       *SCRIVI VARIAZIONI
084901160922     C                   EXSR      WRIVARU                                      *storico var unico
085000990730     C                   EXSR      MOVVF                                        *MUOVI VIDEO A FILE
085100151020
085200151009      * memorizzo il valore del peso usato in tassazione in FIAR5 record FAT
085300151009      * solo se imponibile diverso dal precedente  e non 2° bolla
085400151019     c                   If        tasimv <> sav_tasimv and (
085500151019     c                             §3atb2 = '  ' OR (§3atb2 <> '  '  and
085600151019     c                              %subst(tastbl:1:1)<>%subst(§3atb2:1:1)))
085700151020     C                   MOVEL     V1CFPF        TASFPF
085800151009     c                   exsr      Sr_RecFat
085900151009     c                   Endif
086000151020     c
086100151020     c                   If        tasimv <> sav_tasimv and
086200151020     c                              §3atb2 <> '  '  and
086300151020     c                              %subst(tastbl:1:1)= %subst(§3atb2:1:1)
086400151020     C                   MOVEL     V1CFPF        TASFPF
086500151020     C                   IF        TASIMV = 0 or tasimv<>savimv or
086600151020     c                             wf15='1'
086700151020     c                   eval      tasfpf = 'R'
086800151020     c                   endif
086900151020     c                   endif
087000151020     c
087100151020     c
087200130124     c* prima di aggiornare richiamo pgm per gestione variazioni post-fattur
087300130124     c                   if        tasfic<>' '
087400130124     c                             and (tasias<>sav_tasias  or
087500130124     c                                  tasvas<>sav_tasvas)
087600130124     c                   exsr      sr_tivrb
087700130124     c                   endif
087800990730     C   81              UPDATE    TITAS000
087900990730     C   82              UPDATE    TITAS010
088000070601     C   83              UPDATE    TITASp00
088100990730     C                   EXSR      AGGCON                                       AGGIORNA CONTRASSEGN
088200120705     c* reimposto il tipo bolla ricevuto se è stato cammbaito
088300120705     c                   movel     tastbl        d01tbl
088400151002
088500990730     C                   ENDIF
088600071106     c* Se ho aggiornato la partita iva del destinatario vedo se c'è
088700071106     c* seconda bolla per aggiornare anche quella
088800071106     c                   if        wcpd2bo='1' and §3atb2<>*blanks
088900071106     c                   eval      kastbl=§3atb1
089000071106     C                   eval      *in81 = *off
089100071106     C                   eval      *in82 = *off
089200071106     C                   eval      *in83 = *off
089300071106     C     KEYTAS        CHAIN     TITAS30C
089400071106     C                   IF        %FOUND
089500071106     c                   eval      tascpd=s_tascpd
089600071106     C   81              UPDATE    TITAS000
089700071106     C   82              UPDATE    TITAS010
089800071106     C   83              UPDATE    TITASp00
089900071106     c                   endif
090000071106     c                   endif
090100120403     c* Se aggiornato la bolla richiamo l'aggiornamento delle bolle di filiale
090200120403     C     WMODIF        IFEQ      '1'                                          *SOLO SE MODIFICATA
090300120404     c* se ho aggiornato la partita iva del destinatario sull'altra bolla
090400120404     c* richaino la prima bolla
090500120404     c                   if        wcpd2bo='1' and §3atb2<>*blanks
090600120404     c                   eval      kastbl=wtastbl
090700120404     C     KEYTAS        CHAIN(N)  TITAS30C
090800120404     c                   endif
090900120403     c                   exsr      aggfil                                       Aggiorna bolle di fi
091000120404     c                   ENDIF
091100990730      *
091200990730      * IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
091300990730     C                   eval      $FINE = '1'
091400990730      *
091500990730     C                   ENDIF
091600990730     C                   ENDIF
091700990730      *
091800990730     C                   ENDSR
091900151002      *------------------------------------------------------------------------*
092000151002      * Sr_RecFat registro dati della tassazione in FIAR5  record FAT
092100151002      *------------------------------------------------------------------------*
092200151002     C     Sr_RecFat     BEGSR
092300151002
092400151002     c                   eval      kAr5Trd  = 'FAT'
092500151002     c     kFiar5        Chain     Fiar531c
092600151002If  1c                   If        %Found(Fiar531c)
092700151002     c                   delete    fiar5000
092800151008     c                   endif
092900151008
093000151019      * imposto sempre il flag del peso fatturato con R nel caso in cui non venga modificato
093100151019      * dalla tassazione in quanto viene azzerato l'imponibile e vado a fine
093200151019     C                   IF        TASIMV = 0
093300151019     c                   eval      tasfpf = 'R'
093400151019     c                   leavesr
093500151019     C                   ENDIF
093600151019      *
093700151002     c                   clear                   dAr5Fat
093800151002     c                   clear                   FIAR5000
093900151019      * se imponibile maggiore di zero
094000151019     c                   If        tasimv > 0
094100151002     c* Volume
094101160422      * Verifico se è stato usato il volume desunto valorizzo tutti i campi della ds relativo al
094102160422      * VOLUME e statistiche
094103160531      * Prima verifico se mi è tornato dalla DTAS
094104160531     c                   If        Tasfvt <> ' '
094105160531     c                   z-add     tasvlt        §ar5vltas
094106160531     c                   movel     tasfvt        §ar5fvtas
094107160531     c                   z-add     tasvlf        §ar5vlf
094108160531     c                   movel     tasfvf        §ar5fvf
094109160617     c                   ENDIF
094110160422     c                   If        tasvdes = 'S'
094111160422     c                   z-add     tasvvld       §ar5vltas
094112160422     c                   movel     'D'           §ar5fvtas
094113160422     c                   eval      §ar5SVAAA  = TASVANNO
094114160422     c                   eval      §ar5SVAMM  = TASVMESE
094115170420     c****               z-add     tasvlf        §ar5vlf
094116170420     c****               movel     tasfvf        §ar5fvf
094117170420     c****               else
094200170420     c****               z-add     tasvlf        §ar5vltas
094300170420     c****               movel     tasfvf        §ar5fvtas
094301160422     c                   endif
094400151019     c* Peso se non utilizzato ne il desunto e neppure vdl metto R reale
094500151019     c                   if        tasspc = ' '
094600151019     c                   movel     'R'           §ar5fptas
094700151019     c                   z-add     taspkf        §ar5pktas
094800151019     c                   endif
094900151019     c* Peso se utilizzato il desunto metto D desunto
095000151019     c                   if        tasspc = 'D'
095100151019     c                   movel     'D'           §ar5fptas
095200151019     c                   z-add     taspkcn       §ar5pktas
095300151019     c                   endif
095400151019     c
095500151019     c* Peso se utilizzato quello VDL
095600151019     c                   if        tasspc = 'S'
095700151019     c                   z-add     taspkcn       §ar5pktas
095800151019    3c                   if        tasncp=tasncl
095900151019     c                   movel     'T'           §ar5fptas
096000151019     c                   else
096100151019     c                   movel     'Z'           §ar5fptas
096200151019    3c                   endif
096300151019     c
096400151019    2c                   endif
096500151008      * se imponibile reperito dal richiamo dalla tassazione risulta diverso da quello
096600151008      * del video o ho tassato con altra tariffa imposto peso = TASPKF e flag 'R'
096700151008     c                   if        tasimv <> savimv or WF15 = '1'
096800151019      * solo in caso di desunto
096900151009     c                   movel     'R'           §ar5fptas
097000151009     c                   z-add     taspkf        §ar5pktas
097100151009      * valorizzo anche il campo di titas già in precedenza valorizzato
097200151009     c                   eval      tasfpf = 'R'
097201160503      * volume originale della bolla
097202160503     c                   z-add     tasvlf        §ar5vltas
097203160503     c                   movel     tasfvf        §ar5fvtas
097204160503      * pulisco i campi della statistica
097205160503     c                   clear                   §ar5SVAAA
097206160503     c                   clear                   §ar5SVAMM
097207160503     c                   clear                   §ar5vlf
097208160503     c                   clear                   §ar5fvf
097300151008     c                   endif
097400151002      * data e ora tassazione pc e utente
097500151002     c                   eval      §ar5HMtas = %dec(%time())
097600151002     c                   eval      §ar5DAtas = Dateu
097700151002     c                   eval      §ar5PRtas = Knmus
097800151002     c                   eval      §ar5NJtas = Knmtd
097900151002      * se peso desunto imposto anno e mese file statistica pesi SIPES
098000151020     c                   If        tasfpf = 'D'
098100151002     c                   eval      §ar5STAAA  = TASPANNO
098200151002     c                   eval      §ar5STAMM  = TASPMESE
098300151002     c                   Else
098400151002     c                   clear                   §ar5STAAA
098500151002     c                   clear                   §ar5STAMM
098600151002     c                   Endif
098700151002     c                   Movel     dAr5Fat       Ar5uni
098800151002     C                   eval      ar5aas=tasaas
098900151002     C                   eval      ar5lnp=taslnp
099000151002     C                   eval      ar5nrs=tasnrs
099100151002     C                   eval      ar5nsp=tasnsp
099200151002     c*
099300151002     C                   eval      ar5trd='FAT'
099400151002     c                   eval      ar5dac=DATEU
099500151002     c                   time                    ar5orc
099600151013      * imposto i campi di trasmissione
099700151013     c                   eval      ar5ft1= 'T'
099800151013     c                   eval      ar5dt1 = *all'9'
099900151013     c                   eval      ar5ft2= 'T'
100000151013     c                   eval      ar5dt2 = *all'9'
100100151013     c                   eval      ar5ft3= 'T'
100200151013     c                   eval      ar5dt3 = *all'9'
100300151013
100400151002     c                   write     fiar5000
100500151008     c                   Endif
100600151002
100700151002     c                   Endsr
100800151002
100900030401      *------------------------------------------------------------------------*
101000030401      * F07GES -GEST.TASTO FUNZ. F07 ===> INTERROGAZIONE BOLLA
101100030401      *------------------------------------------------------------------------*
101200030401     C     F07GES        BEGSR
101300030401
101400030401     c                   Clear                   Tnsb51Ds
101500030401     C                   MOVEL     WOP0          i51OP0                         *LIVELLO PROCEDURA
101600030401     C                   MOVEL     WOP1          i51OP1                         *LIVELLO PROGRAMMA
101700080118     c                   movel     'M02'         i51op0
101800080118     c                   movel     'M02'         i51op1
101900030401
102000030401      *
102100030401     C                   Z-ADD     V1CAAS        i51AAS                         *ANNO
102200030401     C                   Z-ADD     V1CLNP        i51LNP                         *LNP
102300030401     C                   Z-ADD     V1CNRS        i51NRS                         *SERIE
102400030401     C                   Z-ADD     V1CNSP        i51NSP                         *SPEDIZIONE
102500030401     C                   Move      TasTbl        i51TBL
102600030401      *
102700030401     C                   movel     TNSB51DS      Kpjbu
102800030401     c                   call      'TNSB51R'
102900030401     c                   parm                    KPJBA
103000030401     C                   movel     KPJBU         TNSB51DS
103100030401      *
103200030401     C                   ENDSR
103300990730      *------------------------------------------------------------------------*
103400990730      * F10GES -GEST.TASTO FUNZ. F10 ===> VARIAZIONI BOLLA
103500990730      *------------------------------------------------------------------------*
103600990730     C     F10GES        BEGSR
103700990730      *
103800990730     C                   Z-ADD     V1CAAS        WAAS                           *ANNO
103900990730     C                   Z-ADD     V1CLNP        WLNP                           *LNP
104000020328     C                   Z-ADD     V1CLNA        WLNA                           *LNP
104100990730     C                   Z-ADD     V1CNRS        WNRS                           *SERIE
104200990730     C                   Z-ADD     V1CNSP        WNSP                           *SPEDIZIONE
104300990730     C                   Z-ADD     V1CDSP        WDSP                           * GG/MM/AAAA
104400990730     C                   clear                   WTBL
104500990730     C                   clear                   WDTV
104600990730     C                   clear                   WORV
104700990730     C                   clear                   WPRU
104800990730      *
104900990730     C                   MOVEL     D01OP0        WOP0                           *LIVELLO PROCEDURA
105000990730     C                   MOVEL     'F10'         WOP1                           *LIVELLO PROGRAMMA
105100990802     C                   MOVEL(P)  'TNSB53R'     WPGM                           *ROUTINE MODIFICA
105200990730     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
105300990730      *
105400990730     C                   ENDSR
105500990730      *------------------------------------------------------------------------*
105600990730      * F12GE2 -GEST.TASTO FUNZ. F12 ===> PRECEDENTE DA VIDEO 2
105700990730      *------------------------------------------------------------------------*
105800990730     C     F12GE2        BEGSR
105900990730      *
106000990730     C                   MOVEL     'V1'          $GES                           *VIDEO 1
106100071106     c                   clear                   winizia
106200990730     C                   EXSR      DECOD1
106300990730      *
106400990730     C                   ENDSR
106500990730      *------------------------------------------------------------------------*
106600990730      * F14GES -GEST.TASTO FUNZ. F14 ===> TASSAZIONE
106700990730      *------------------------------------------------------------------------*
106800990730     C     F14GES        BEGSR
106900030626      *
107000030626     c                   clear                   flgvaa
107100990730      *
107200990730      * IL COMANDO E' VALIDO SOLO SE IL CAMPO IMPONIBILE E' VUOTO
107300990730     C                   IF        V2CIMV > *zeros
107400990730     C                   eval      $ERR = '1'
107500990730     C                   movel     MSG(37)       $MSG
107600990730     C                   EXSR      CARMSG
107700990730     C                   eval      *in69 = *on
107800990730     C                   ELSE
107900151020     c
108000151020      * pulisco il campo dell'imponibile che sarà valorizzato con quello che riceviamo
108100151020      * dai programmi di tassazione per poi confrontarlo con quello a video
108200151020      * al momento della conferma.
108300151020     c                   clear                   savimv
108400990730      *
108500990730      * RIEMPO LE DS GENERICHE CON LE 20 "VARIE"
108600991102     C                   clear                   DTASV
108700020415     C                   clear                   wtassa            1
108800990730     C                   DO        20            WA
108900990730     C     WA            CHAIN     SB52S02
109000990730     c                   if        %FOUND
109100011023      * Se è una bolla di reso con varia 'N' 88888888
109200011023      * tasso la varia 'N'
109300011023     c                   if        flgvrn = '1'
109400011023     c                             and v2csv1 = 'N' and v2cva1 = 88888888
109500011106     c                   eval      sav_dtasv = dtasv
109600120327     c* Se presente codice cliente/tariffa con cui tassare  (Da F15) uso quelli
109700120327     c                   if        wf15='1'
109800120327     c                   z-add     f15ksc        tasksc
109900120327     c                   move      f15ctr        tasctr
110000120327     c                   endif
110100011023     c                   exsr      tassan
110200011106     c                   eval      dtasv = sav_dtasv
110300011023     c                   endif
110400020415     c* se tipo bolla che prevede la varia, e c'e' gia' il bolla non tasso
110500040128     c                   if        v2csv1=§3asva and §3asva<>*blanks and
110600040128     c                             V2cImv > *Zeros
110700020415     c                   eval      wtassa='N'
110800020415     c                   endif
110900011023      *
111000030626      * SE ESISTE LA VARIA "&" CON TUTTI 8888888 NON LA PASSO AL TNSF20R
111100030626      * MA PASSO UN FLAG  DI RICHIESTA CALCOLO VARIA "&"
111200030626     C                   IF        V2CSV1 = '&' AND V2CVA1 = 88888888
111300030626     C                   EVAL      flgvaa =  'S'
111400030626     C                   ELSE
111500991102     C                   MOVEL     V2CSV1        TSV(WA)                        SIGLA "VARIA"
111600991102     C                   Z-ADD     V2CVA1        TVA(WA)                        IMPORTO "VARIA"
111700991102     C                   MOVEL     V2HTEV        TEV(WA)                        ESENZIONE "VARIA"
111800990730     C                   endif
111900030626     C                   endif
112000990730     C                   ENDDO
112100990730      *
112200990730      * C A L C O L O    L A    T A S S A Z I O N E
112300990730      *
112400991102     C                   CLEAR                   DTAS
112500030626      * varia &
112600030626     c                   eval      tasfcaa = flgvaa
112700990730      * COD.CLIENTE: 1) MITTENTE SE E' FRANCO
112800990730      *              2) MITTENTE SE IL FLAG VHCFTM='S'
112900990730      *              3) ALTRIMENTI ASSEGNATO
113000990730     c                   If        §TBTPO = 'F'  or  VHCftm = 'S'
113100991102     C                   Z-ADD     V1CMIT        TASKSC                          MITTENTE
113200990730     C                   Else
113300991102     C                   Z-ADD     V1CDES        TASKSC                          DESTINATARIO
113400990730     C                   Endif
113500120327     c* Se provengo da tassazione con cliente/traiffa (Da F15) uso codici richiesti
113600120327     c                   if        wf15='1'
113700120326     c                   z-add     f15ksc        tasksc
113800120326     c                   endif
113900991102     C                   MOVE      TASKSC        WQUATT
114000990730      *
114100991102     C                   Z-ADD     V1CPKF        TASPKF
114200030915     C                   Z-ADD     V1CPKc        TASPKc
114300991102     C                   Z-ADD     V1CVLF        TASVLF
114301160531     C                   Z-ADD     V1Cpkb        TASpkb
114302160531     C                   Z-ADD     V1Cvlc        TASvlc
114303160531     C                   Z-ADD     V1Cncr        TASncr
114304160531     C                   move      V1Cfvf        TASfvf
114400030729     C                   Z-ADD     V1CNCL        §TASNCL
114401160531     C                   Z-ADD     V1CNCL        TASNCLB
114500030915     C                   Z-ADD     V1CNCp        TASNCp
114600991102     C                   MOVEL     V1CCTS        TASCTS
114700991102     C                   MOVEL     V1CCTR        TASCTR
114800120327     c                   if        wf15='1'
114900120326     C                   MOVEL     f15ctr        TASCTR
115000120326     c                   endif
115100991102     C                   Z-ADD     TA$DSP        TASDSP                          AAAA/MM/GG
115200991102     C                   MOVEL     V1CCAD        TASCAD
115300991102     C                   MOVEL     V1CFIN        TASFIN
115400991102     C                   MOVEL     V1CCAM        TASCAM
115500991102     C                   MOVEL     V1CFAP        TASFAP
115600991102     C                   MOVEL     V1CTSP        TASTSP
115700991102     C                   MOVEL     V1CFTC        TASTC1
115800991102     C                   MOVEL     V1CTC2        TASTC2
115900991102     C                   MOVEL     V2CVAS        TASVAS
116000991102     C                   Z-ADD     V1CLNP        TASLNP
116100991102     C                   Z-ADD     V1CLNA        TASLNA
116200991102     C                   MOVEL     V1CTBL        TASTBL
116300991102     C                   Z-ADD     V2CIAS        TASIAS                          IMPORTO ASSICURARE
116400991102     C                   MOVEL     V1CFDN        TASFDN                          MERCE PORT.MAG.
116500991102     C                   MOVEL     V1Cnzm        TASnzm                          MERCE PORT.MAG.
116600991102     C                   MOVEL     V1Cnzd        TASnzd                          MERCE PORT.MAG.
116700991102     C                   MOVEL     V2Cdiv        TASdiv                          MERCE PORT.MAG.
116800991102     C                   MOVEL     V1CMCT        TASMCT                          COD.TASSAZ.MITTENTE
116900991102     C                   Z-ADD     V1CQFT        TASQFT                          QTA' FATTURABILE
117000991102     C                   Z-ADD     V2CPOR        TASPOR                          TRASPORTO
117100991102     C                   z-add     TA$DSP        TASDCT                          AAAA/MM/GG
117200990730      *
117300990730      * AGGANCIO IL CONTRASSEGNO.
117400990730      * se trovato, non annullato, verifico che il 1o carattere del tipo bolla corrisponda
117500990730     C                   if        V1CFCA <> *BLANKS
117600990730     C                   z-add     V1CAAS        KSBAAS                         ANNO
117700990730     C                   z-add     V1CLNP        KSBLNP                         LNP
117800990730     C                   z-add     V1CNRS        KSBNRS                         SERIE
117900990730     C                   z-add     V1CNSP        KSBNSP                         SPEDIZIONE
118000990826     C     KEYCSB        chain(N)  TNCSB03L
118100020415     c* adesso passo il C/A anche se annullato: solo in sede
118200020415     c                   if        %found  and
118300990730     c                             %subst(v1ctbl:1:1) = %subst(csbtbl:1:1)
118400991102     C                   z-add     CSBCAS        TASCAS                          CONTRASSEGNO
118500030204     C                   movel     CSBsta        Tassta                          CONTRASSEGNO
118600991102     C                   movel     CSBVCA        TASVCA                          VALUTA
118700991102     C                   z-add     CSBCMB        TASCMB                          CAMBIO DIVISA
118800061016     C****               movel     CSBTPA        TASTIC                          TIPO INCASSO
118900061013     C****               move      CSBTPI        TASTIC                          TIPO INCASSO
119000061013     c                   if        csbfus <> *blanks
119100061013     c                   move      csbfus        tastic
119200061013     c                   else
119300061013     c                   move      csbtpi        tastic
119400061013     c                   end
119500990730     C                   endif
119600990730     C                   ENDIF
119700990730      *
119800990730      * FALG SI NO DDT
119900990730     C                   if        %SUBST(V1Dddt:1:1) = 'S'
120000991102     C                   MOVEL     'S'           TASDDT
120100990730     C                   endif
120200060922     c***                movel     tasfbr        §tasflo
120300110630     c                   eval      dtasflo=tasflo
120400060922     c                   clear                   dtas01
120500060922     c                   movel     tasfbr        §asfbr
120600060922     c                   movel     tascca        §ascca
120700110630     c                   movel     §floemd       §asemd
120800141217      /free
120900141217       //?Imposto se part.consegna con PinCode
121000141217         IF  TASgma <> *blanks and %lookup (TASgma:skGMA) > 0;
121100141217           §ASpin = 'S';
121200141217         ENDIF;
121300141217      /end-free
121400160104
121500160104      * valorizzo addebito di diritto di chiamata e packing list solo se non 2° bolla
121600160104     c                   If        §3atb2 = '  ' OR (§3atb2 <> '  '  and
121700160104     c                              %subst(tastbl:1:1)<>%subst(§3atb2:1:1))
121800160104      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
121900160104     c                   move      §floado       tasprt
122000160104      * valorizzo il campo per l'addebito stampa PACKING LIST
122100160104     c                   eval      kAr5Trd  = 'GEN'
122200160104     c                   clear                   DAr5gen
122300160104     c     kFiar5        Chain(n)  Fiar531c
122400160104If  1c                   If        %Found(Fiar531c)
122500160104     c                   Movel     Ar5Uni        dAr5Gen
122600160104    1c                   EndIf
122700160104      *
122800160104     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
122900160104     c                   Eval      TasSpl = 'S'
123000160104     c                   endif
123100160104
123200160104     c                   endif
123300160104
123400060922     c                   eval      §tasflo = dtas01
123500990730      *
123600990730     C                   CLEAR                   KPJBU
123700990730      *
123800020415     c                   if        wtassa=' '
123900020415     c* se devo tassare una varia solo la imposto
124000020415     c                   if        §3asva<>*blanks
124100020415     c                   eval      tassva=§3asva
124200020415     c                   endif
124300030204      * Bancali
124400030205     c                   If        %Subst(TasGva:1:1) = 'B'
124500030204     c                   Z-Add     V1cBan        TasBan
124600030205     c                   EndIf
124700030729      * Colli Originali
124800030729     c                   If        %Subst(TasGva:1:1) = 'O'
124900030729     c                   Z-Add     V1cCor        §TasNcl
125000030729     c                   EndIf
125100040128      * nel caso di seconda bolla passo info al TNSF20
125200040128     c                   If        (V2cCbo = '5 ' or V2cCbo = 'L ') and
125300040128     c                              V1cTbl = 'A3'
125400040128     c                   Eval      TasSva = 'G'
125500040128     c                   Eval      TascVag = 'S'
125600040128     c                   EndIf
125700051010      * verifico se devo calcolare l'addebito di lasciato avviso
125800051010      *
125900051010     C                   MOVEL     TASKSC        KSCFIL            3 0
126000051010      *
126100051010      *
126200080618      * verifico se c'è evento per addebito lasciato avviso
126300051010     c     Kevb4         chain     Fnevb04l
126400051010
126500051010     c                   If        %found(Fnevb04l)
126600051010     c                   eval      tasric = 'S'
126700051010     c                   endif
126800150929
126900150929      * valorizzo la ds aggiuntiva
127000150929     c                   clear                   dtaspes
127100150929     c                   eval       tasppkb = taspkb
127101160422     c                   eval       taspfvf = tasfvf
127102160422     c                   eval       taspncr = tasncr
127200150929
127300150929     c***
127400151008      * se sto tassando con un altro cliente e tariffa (F15 attivo) richiamo il TNSF20
127500151008      * in quanto non devo xcalcolare il peso desunto
127600151008     c                   if         wf15 = ' '
127700150929     C                   CALL      'TNSF22R'
127800150929     C                   PARM                    KPJBA
127900150929     C                   PARM                    DTAS
128000150929     C                   PARM                    DTASV
128100150929     C                   PARM                    DTASpes
128200151008     c                   else
128300151012     C                   CALL      'TNSF20R'
128400151008     C                   PARM                    KPJBA
128500151008     C                   PARM                    DTAS
128600151008     C                   PARM                    DTASV
128700151008     c                   Endif
128800150929      *
128900150929     C                   MOVEL     '1'           WSF22
129000150929     C                   MOVEL     '1'           WSF20
129100051010
129200020415     c***
129300990730      *
129400990730      *  Ho ricevuto un errore dal pgm di tassazione
129500991102     c                   IF        TASerr <> *blanks  or  TASmsg <> *blanks
129600120327     c                   if        wf15=' '
129700120327     C                   eval      *in69 = *on
129800990730     C                   eval      $err = '1'
129900991102     C                   If        TASMSG <> *BLANKS
130000991102     C                   movel     TASMSG        $MSG
130100990730     C                   Else
130200990730     C                   movel     MSG(38)       $MSG
130300990730     C                   Endif
130400990730     C                   EXSR      CARMSG
130500120327     c                   else
130600120327     C                   If        TASMSG <> *BLANKS
130700120327     C                   movel     TASMSG        w01msg
130800120327     C                   Else
130900120327     C                   movel     MSG(38)       w01msg
131000120327     C                   Endif
131100120327     c                   endif
131200990730     c                   ELSE
131300990730      *
131400120326     C* APPLICA LA % DI SCONTO, SE INSERITA, SU TUTTI GLI IMPORTI
131500120327IF  4C                   IF        wf15='1' and f15sco>0
131600120326     C                   EXSR      APPSCO
131700120326E   4C                   ENDIF
131800990730     C                   movel     '1'           WMODIF                         *VARIATA BOLLA
131900991102     C                   z-add     TASPOR        V2CPOR
132000991103     C                   z-add     TASPOR        memPOR
132100991102     C                   movel     TASPVL        V1CPVL
132200991102     C                   z-add     TASQFT        V1CQFT                         QTA' FATTURARE
132300991103     c                   movel     TASdiv        V2Cdiv
132400991103     c                   movel     TASdiv        Wdiv
132500991104     c                   z-add     TASimv        V2Cimv
132600151008     c                   z-add     TASimv        savimv
132700020829     c*
132800151019
132900030204     c                   if        Tasspc='S'
133000151019     c                   eval      v1cfpf='V'
133100020829     c                   endif
133200151006     c*
133300151019     c                   if        Tasspc=' '
133400151019     c                   eval      v1cfpf='R'
133500151019     c                   endif
133600151019     c*
133700151006     c                   if        Tasspc='D'
133800151016     c                   eval      v1cfpf='D'
133900151006     c                   endif
134000991103      *
134100990730      * PULISCO IL SFL
134200990730     c                   eval      *in20 = *off
134300990730     c                   eval      *in21 = *off
134400990730     C                   WRITE     SB52C02
134500990730     c                   eval      *in20 = *on
134600990730     c                   eval      *in21 = *on
134700990730      *
134800991104      * TRASFERISCO LE 20 VARIE DALLA DS AL SFL
134900990730     C                   clear                   SFLULT
135000990730     C                   DO        20            WA
135100991102     c                   if        tsv(WA)<>' '
135200991102     C                   movel     TSV(WA)       V2CSV1                         SIGLA "VARIA"
135300991102     C                   z-add     TVA(WA)       w0133                          IMPORTO "VARIA"
135400990826     c                   EXSR      aggiosbf
135500990826     c                   endif
135600990730     C                   ENDDO
135700990827     c
135800990827      * FINISCO DI RIEMPIRE IL SFL
135900990827     C                   clear                   V2CSV1
136000990827     C                   clear                   V2DSV1
136100991104     C                   clear                   V2CVA1
136200991104     C                   clear                   V2HVA1
136300990827     C                   clear                   V2HTEV
136400990827     C                   clear                   V2HI97
136500990827     C                   eval      WA =  SFLULT + 1
136600990827     C     WA            DO        20            SFLULT
136700990827     C                   WRITE     SB52S02                                      SFL
136800990827     C                   ENDDO
136900990827     c
137000990827     c                   z-add     1             sflrcd
137100990730      *
137200991104     C                   ENDIF
137300020415     C                   ENDIF
137400990730     C                   ENDIF
137500990730      *
137600990730     C                   ENDSR
137700120323      *------------------------------------------------------------------------*
137800120323      * F15GES -GEST.TASTO FUNZ. F15 ===> Cliente/tariffa
137900120323      *------------------------------------------------------------------------*
138000120323     C     F15GES        BEGSR
138100120327     c                   move      '1'           wf15
138200120327      * IL COMANDO E' VALIDO SOLO SE IL CAMPO IMPONIBILE E' VUOTO
138300120327     C                   IF        V2CIMV > *zeros
138400120327     C                   eval      $ERR = '1'
138500120327     C                   movel     MSG(37)       $MSG
138600120327     C                   EXSR      CARMSG
138700120327     C                   eval      *in69 = *on
138800120327     C                   ELSE
138900120323      * COD.CLIENTE: 1) MITTENTE SE E' FRANCO
139000120323      *              2) MITTENTE SE IL FLAG VHCFTM='S'
139100120323      *              3) ALTRIMENTI ASSEGNATO
139200120327    1c                   If        §TBTPO = 'F'  or  VHCftm = 'S'
139300120323     C                   Z-ADD     V1CMIT        w01kscb                         MITTENTE
139400120327   x1C                   Else
139500120323     C                   Z-ADD     V1CDES        w01kscb                         DESTINATARIO
139600120327    1C                   Endif
139700120323     C                   CLEAR                   TIBS69DS
139800120323     C                   Z-ADD     w01kscb       I69KAC
139900120323     C                   MOVEL     KNSIF         I69SIF
140000120323     C                   exsr      CTR_ANAGRA
140100120323     c                   clear                   w01dkscb
140200120323     C                   MOVEL     ACORAG        w01dkscb
140300120323     c* tariffa della bolla
140400120323     c                   eval      w01ctrb=v1cctr
140500120323     c                   eval      w01dctrb=v1dctr
140600120323     c* CLIENTE/TAriffa per TASSAZIONE  e % di sconto
140700120327    1c                   if        f15ksc>0
140800120326     c                   z-add     f15ksc        w01ksc
140900120326     c                   move      f15ctr        w01ctr
141000120326     c                   z-add     f15sco        w01sco
141100120327   x1c                   else
141200120323     c                   clear                   w01ksc
141300120323     c                   clear                   w01dksc
141400120323     c                   clear                   w01ctr
141500120323     c                   clear                   w01sco
141600120327    1c                   endif
141700120323     c                   setoff                                       28
141800120323     c                   eval      $finew='0'
141900120327    1c                   dow       $finew='0'
142000120327     c                   write     sb52w01
142100120327     c                   read(E)   tnsb52d
142200120323     c                   setoff                                       28
142300120323     c                   clear                   w01msg
142400120327    2c                   select
142500120323     c* f12=Ritorno
142600120327     c                   when      *inkl='1'  or %error
142700120323     c                   eval      $finew='1'
142800120323     c                   other
142900120323     c* f6=conferma o enter
143000120323     c                   exsr      ctrwdwcli
143100120327    3c                   select
143200120327   whc                   when      *inkl='1'
143300120327     c                   eval      $finew='1'
143400120327   whc                   when      *inkn and not *in28
143500120326     c                   eval      f15ksc=w01ksc
143600120326     c                   eval      f15ctr=w01ctr
143700120326     c                   eval      f15sco=w01sco
143800120327    4c                   if        f15ksc>0
143900120326     c                   exsr      f14ges
144000120327    5c                   if        w01msg<>*blanks
144100120327     c                   seton                                        1128
144200120327    5c                   endif
144300120327    4c                   endif
144400120327     c  N28              eval      $finew='1'
144500120327    3c                   endsl
144600120327    2c                   endsl
144700120327    1c                   enddo
144800120327     c                   endif
144900120327     c                   clear                   wf15
145000120323     c                   endsr
145100990730      *------------------------------------------------------------------------*
145200990730      * F16GES -GEST.TASTO FUNZ. F16 ===> COMPETENZE DI CONTO ECONOMICO
145300990730      *------------------------------------------------------------------------*
145400990730     C     F16GES        BEGSR
145500990730      *
145600990730     C                   Z-ADD     V1CAAS        WAAS                           *ANNO
145700990730     C                   Z-ADD     V1CLNP        WLNP                           *LNP
145800020328     C                   Z-ADD     V1CLNA        WLNA                           *LNP
145900990730     C                   Z-ADD     V1CNRS        WNRS                           *SERIE
146000990730     C                   Z-ADD     V1CNSP        WNSP                           *SPEDIZIONE
146100990730     C                   MOVEL     V1CTBL        WTBL                           *TIPO BOLLA
146200990730     C                   Z-ADD     V1CDSP        WDSP                           * GG/MM/AAAA
146300990730     C                   Z-ADD     *ZERO         WDTV
146400990730     C                   Z-ADD     *ZERO         WORV
146500990730     C                   clear                   WPRU
146600990730      *
146700990730     C                   MOVEL     D01OP0        WOP0                           *LIVELLO PROCEDURA
146800990730     C                   MOVEL     'F16'         WOP1                           *LIVELLO PROGRAMMA
146900990730     C                   MOVEL(P)  'TNSB08R'     WPGM                           *ROUTINE SUBFILE
147000990730     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
147100990730      *
147200990730     C                   ENDSR
147300990730      *------------------------------------------------------------------------*
147400990730      * F18GES -GEST.TASTO FUNZ. F18 ===> LEGAMI BOLLA
147500990730      *------------------------------------------------------------------------*
147600990730     C     F18GES        BEGSR
147700990730      *
147800990730     C                   CLEAR                   PARA07
147900990730     C                   Z-ADD     V1CAAS        P07AAS
148000990730     C                   Z-ADD     V1CLNP        P07LNP
148100990730     C                   Z-ADD     V1CNRS        P07NRS
148200990730     C                   Z-ADD     V1CNSP        P07NSP
148300990730     C                   Z-ADD     V1CMGS        P07MGS
148400990730     C                   MOVEL     V1CTBL        P07TBL
148500990730     C                   MOVEL     V1DTBL        P07DTB
148600990730     C                   Z-ADD     V1CLNA        P07LNA
148700990730     C                   MOVEL     V1DLNA        P07DLA
148800990730     C                   Z-ADD     V1CRMN        P07RMN
148900990730     C                   MOVEL     VTDRSU        P07RSU
149000990730      *
149100990730     C                   MOVEL     PARA07        KPJBU
149200990730     C                   CALL      'TNSB07R'
149300990730     C                   PARM                    KPJBA
149400990730      *
149500990730     C                   MOVEL     KPJBU         PARA07
149600990730     C     P07F03        IFEQ      '1'
149700990730     C                   MOVEL     '1'           $F03
149800990730     C                   MOVEL     '1'           $FINE
149900990730     C                   ENDIF
150000990730      *
150100990730     C                   ENDSR
150200990730      *------------------------------------------------------------------------*
150300990730      * F22GES -GEST.TASTO FUNZ. F22 ===> CONTRASSEGNO
150400990730      *------------------------------------------------------------------------*
150500990730     C     F22GES        BEGSR
150600990730      *
150700990730     C                   CLEAR                   TNSC40DS
150800990730     C                   Z-ADD     1             D40INT
150900990730     C                   Z-ADD     V1CAAS        D40AAS
151000990730     C                   Z-ADD     V1CLNP        D40LNP
151100990730     C                   Z-ADD     V1CNRS        D40NRS
151200990730     C                   Z-ADD     V1CNSP        D40NSP
151300990730      *
151400990730     C                   MOVEL     TNSC40DS      KPJBU
151500990730     C                   CALL      'TNSC43R'
151600990730     C                   PARM                    KPJBA
151700990730      *
151800990730     C                   ENDSR
151900990730      *------------------------------------------------------------------------*
152000990730      * RUPV2  -GEST.TASTO FUNZ. ROLLUP VIDEO 2  ===> PAGINA SUCCESSIVA
152100990730      *------------------------------------------------------------------------*
152200990730     C     RUPV2         BEGSR
152300990730      *
152400990730      * SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
152500990730      * SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
152600990730      *
152700990730     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
152800990730     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
152900990730     C                   ELSE
153000990730      * INVIO MESSAGGIO DI FINE ELENCO
153100990730     C                   MOVEL     '1'           $ERR
153200990730     C                   MOVEL     MSG(6)        $MSG
153300990730     C                   EXSR      CARMSG
153400990730     C                   Z-ADD     SFLULT        SFLRCD
153500990730     C                   ENDIF
153600990730      *
153700990730     C                   ENDSR
153800990730      *------------------------------------------------------------------------*
153900990730      * RDNV2  -GEST.TASTO FUNZ. ROLLDOWN VIDEO 2  ===> PAGINA PRECEDENTE
154000990730      *------------------------------------------------------------------------*
154100990730     C     RDNV2         BEGSR
154200990730      *
154300990730      * SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
154400990730      * SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
154500990730      *
154600990730     C     CURSOR        IFGE      POSMIN
154700990730     C     CURSOR        ANDLE     POSMAX
154800990730     C                   ELSE
154900990730      * INVIO MESSAGGIO DI INIZIO ELENCO
155000990730     C                   MOVEL     '1'           $ERR
155100990730     C                   MOVEL     MSG(5)        $MSG
155200990730     C                   EXSR      CARMSG
155300990730     C                   Z-ADD     1             SFLRCD
155400990730     C                   ENDIF
155500990730      *
155600990730     C                   ENDSR
155700990730      *------------------------------------------------------------------------*
155800990730      *  DECOD1 - CONTROLLA CAMPI E DECODIFICA 1' FORMATO
155900990730      *------------------------------------------------------------------------*
156000990730     C     DECOD1        BEGSR
156100990730      *
156200990730      * REIMPOSTO INDICATORI DI ERRORE
156300990730      *
156400990730     C                   SETOFF                                       505152
156500990730     C                   SETOFF                                       535455
156600021125     c                   Setoff                                       5657
156700021125     C                   SETOFF                                       6061
156800990730     C                   SETOFF                                       626364
156900990730     C                   SETOFF                                       656667
157000990730     C                   SETOFF                                       686970
157100990730     C                   SETOFF                                       717273
157200990730     C                   SETOFF                                       747576
157300071029     c                   setoff                                       585942
157400071029     c                   setoff                                       43
157500990730      *
157600990730     C                   clear                   $PUNTO                         *PUNTO INTERROGATIVO
157700990730     C                   clear                   WVLT                           *DIVISA DELLA NAZION
157800990730     C   79              eval      WIN79 = '1'                                  *MEMORIA *IN79
157900990730     C   80              eval      WMODIF = '1'                                 *VARIATA BOLLA
158000990730      *--------------------
158100990730      * T I P O     B O L L A
158200990730      *--------------------
158300990730     C                   clear                   V1DTBL                         *DESCRIZIONE
158400990730     C                   MOVEL     'TB'          §COD                           *CODICE TABELLA
158500990730     C                   MOVEL(P)  V1CTBL        §KEY                           *CODICE ELEMENTO
158600990730     C                   EXSR      CHKTAB
158700990730     C                   MOVEL     §KEY          V1CTBL                         *TIPO BOLLA
158800990730     C                   MOVEL     §DES          V1DTBL                         *DESCRIZIONE
158900990730     C                   If        $ERR = '0'
159000990730     C                   MOVEL     TBLUNI        DSTB
159100990730     C                   Else
159200990730     C                   CLEAR                   DSTB
159300990730     c                   if        *in91 = *off
159400990730     C                   MOVEL     MSG(7)        $MSG
159500990730     C                   EXSR      CARMSG
159600990730     C                   EVAL      *in60 = *on
159700990730     C                   endif
159800990730     C                   Endif
159900990730      *
160000990730      * NON SI PUO' TRASFORMARE TIPO BOLLA IN RECUPERO (SE NON LO ERA PRIMA)
160100120706      *  se si tratta di tipi bolla "paganti"
160200120706     c                   IF        $ERR = '0'  and  not *in91
160300120706     c                   if        §tbfcb='1' and w§tbfcb='1'
160400120706     C                   if        (§TBRBL = 'R' and  W§tbrbl <> 'R')  or
160500120706     c                             (§TBRBL <> 'R' and  W§tbrbl = 'R')
160600990730     C                   eval      $ERR = '1'
160700990730     C                   eval      *in60 = *on
160800990730     C                   movel     MSG(8)        $MSG
160900990730     C                   EXSR      CARMSG
161000990730     C                   ENDIF
161100120706     C                   ENDIF
161200120706     C                   ENDIF
161300990730      *
161400990730      * NON SI PUO TRASFORMARE DA FRANCO AD ASSEGNATO E VICEVERSA
161500990730     c                   IF        $ERR = '0'  and
161600990730     C                             *IN91 = *OFF  and  §TBTPO <> W§tbtpo
161700990730     C                   eval      $ERR = '1'
161800990730     C                   eval      *in60 = *on
161900990730     C                   movel     MSG(9)        $MSG
162000990730     C                   EXSR      CARMSG
162100990730     C                   ENDIF
162200060706     c
162300060706     c* non posso modificare tipo bolla per franchi prepagati
162400060706     c                   if        §tbtpo='F'  and
162500060706     c                             TasFic = *Blanks and TasDft > 0
162600060706     c                             and v1ctbl<>tastbl
162700060706     c                   Eval      $Err = '1'
162800060706     c                   Eval      *In60 = *On
162900060706     c                   Eval      $Msg = Msg(69)
163000060706     c                   ExSr      CarMsg
163100060706     c                   EndIf
163200030218
163300030218      * Non si può modificare il tipo bolla se §3aIm0 è "S"  (Bolle F* - FO - FY)
163400030218     c                   If        $Err = '0' and Not *In91 and §3aIm0 = 'S' and
163500030626     c                             TasTbl <> V1cTbl  and Tastbl = 'A3'
163600030218     c                   Eval      $Err = '1'
163700030218     c                   Eval      *In60 = *On
163800060706     c                   Eval      $Msg = Msg(64)
163900030218     c                   ExSr      CarMsg
164000030218     c                   EndIf
164100120705     c*
164200120705     c* Verifico se il tipo bolla della sped. è modificabile in quello indicato
164300120705    1c                   if        $Err = '0'and v1ctbl<>tastbl
164400120705
164500120705    2c                   if        w§tbmod1=*blanks and w§tbmod2=*blanks and
164600120706     c                             w§tbmod3=*blanks and w§tbmod4=*blanks
164700120705     c                   Eval      $Err = '1'
164800120705     c                   Eval      *In60 = *On
164900120705     c                   Eval      $Msg = Msg(79)
165000120705     c                   ExSr      CarMsg
165100120705   x2c                   Else
165200120705    3c                   if        v1ctbl<>w§tbmod1 and v1ctbl<>w§tbmod2 and
165300120706     c                              v1ctbl<>w§tbmod3 and v1ctbl<>w§tbmod4
165400120705     c                   Eval      $Err = '1'
165500120705     c                   Eval      *In60 = *On
165600120705     c                   Eval      $Msg = Msg(78)
165700120705     c                   eval      %subst($msg:16:2)=tastbl
165800120705     c                   if        w§tbmod1<>*blanks
165900120705     c                   eval      $Msg=%trim($msg)+' ' +w§tbmod1
166000120705     c                   endif
166100120705     c                   if        w§tbmod2<>*blanks
166200120705     c                   eval      $Msg=%trim($msg)+' ' +w§tbmod2
166300120705     c                   endif
166400120705     c                   if        w§tbmod3<>*blanks
166500120705     c                   eval      $Msg=%trim($msg)+' ' +w§tbmod3
166600120705     c                   endif
166700120706     c                   if        w§tbmod4<>*blanks
166800120706     c                   eval      $Msg=%trim($msg)+' ' +w§tbmod4
166900120706     c                   endif
167000120705     c                   eval      $Msg=%trim($msg)+' "'
167100120705     c
167200120705     c                   ExSr      CarMsg
167300120705    3c                   endif
167400120705    2c                   endif
167500120706     c
167600120706     c* controllo congruenza col cod.trattamento merce  se tipo bolla "pagante"
167700120706    2c                   if        $ERR = '0'  and  §tbfcb='1'
167800120706     c* non accetto tipo bolla di recupero se il cod trattamento merce prevede segnacolli
167900120706    3c                   if        §tbrbl='R' and §1bfg8='S'
168000120706     c                   Eval      $Err = '1'
168100120706     c                   Eval      *In60 = *On
168200120706     c                   Eval      $Msg = Msg(80)
168300120706     c                   ExSr      CarMsg
168400120706    3c                   endif
168500120706     c* viceversa
168600120706    3c                   if        §tbrbl='N' and §1bfg8='N'
168700120706     c                   Eval      $Err = '1'
168800120706     c                   Eval      *In60 = *On
168900120706     c                   Eval      $Msg = Msg(81)
169000120706     c                   ExSr      CarMsg
169100120706    3c                   endif
169200120706    2c                   endif
169300120706    1c                   endif
169400120705     c
169500990730      *
169600990730      * T I P O    S E R V I Z I O
169700990730     c                   IF        $ERR = '0'
169800990730     C                   MOVE      *BLANKS       V1DTSP                         *DESCRIZIONE
169900990730     C                   MOVEL     '5E'          §COD                           *CODICE TABELLA
170000990730     C                   MOVEL(P)  V1CTSP        §KEY                           *CODICE ELEMENTO
170100990730     C                   EXSR      CHKTAB
170200990730     C                   MOVEL     §KEY          V1CTSP                         *DESCRIZIONE
170300990730     C                   MOVEL     §DES          V1DTSP                         *DESCRIZIONE
170400990730     c                   If        $ERR = '1'  and  *IN91 = *OFF
170500990730     C                   MOVEL     MSG(25)       $MSG
170600990730     C                   EXSR      CARMSG
170700990730     C                   eval      *in69 = *on
170800990730     C                   Endif
170900990730     C                   ENDIF                                                  $ERR = '0'
171000990730      *-----------------------
171100990730      * M I T T E N T E
171200990730      *-----------------------
171300020516      *
171400020516      * se modificato il mittente simulo una interrogazione per
171500020516      *  riemettere la videata
171600020516     C                   if        WsavMIT <> V1Cmit
171700120802     c* memorizzo se era un codificato
171800120802     C                   move      wsavMIT       savQUATT
171900020516     C                   eval      WsavMIT =  V1Cmit
172000020516     C                   eval      $PUNTO  =  *on
172100020516     C                   endif
172200020516      *
172300990730     C                   move      V1CMIT        WQUATT                         ULTIME QUATTRO DX
172400990730     C                   clear                   WCTS
172500990730      *
172600990730     c                   SELECT
172700990730     c                   WHEN      $ERR = '1'
172800990730      *
172900990806      * NON può mai essere 9999
173000990806     c                   WHEN       Wquatt = 9999
173100990730     C                   eval      $err = '1'
173200990730     c                   eval      *in63 = *on
173300990730     C                   movel     MSG(11)       $MSG
173400990730     C                   exsr      CARMSG
173500990730      *
173600990730      * Mittente non codificato
173700990730     C                   WHEN      WQUATT = 8888
173800990730     C                   eval      *in87 = *off
173900990730      *  Il P.O. cliente deve essere uguale a LNP
174000040930     C  n91              movel     V1CMIT        W0030
174100041109     C  n91              If        W0030  <>  V1CLNP
174200990730     C                   eval      $err = '1'
174300990730     c                   eval      *in63 = *on
174400990730     C                   movel     MSG(11)       $MSG
174500990730     C                   exsr      CARMSG
174600990730     C                   Endif
174700120802     c
174800120802     c* se prima era stato scritto un codificato non posso rimettere 8888
174900120802     c                   if        savQUATT <> 8888
175000120802     C                   eval      $err = '1'
175100120802     c                   eval      *in63 = *on
175200120802     C                   movel     MSG(82)       $MSG
175300120802     C                   exsr      CARMSG
175400120802     C                   Endif
175500040930      *  Controllo solo se campi mittente non protetti
175600040930     C                   IF        *IN92 = *off  and  *IN91 = *off
175700040930      * NON può essere 8888 in modifica spedizioni di sede da parte di P.O.
175800120802     c**                 if        *in22 and *in89
175900120802     c**                                 and $err = '0'
176000120802     c**                 eval      $err = '1'
176100120802     c**                 eval      *in63 = *on
176200120802     c**                 movel     MSG(11)       $MSG
176300120802     c**                 exsr      CARMSG
176400120802     c**                 endif
176500990730      *  Verifico l'indirizzo con PGM esterno
176600990730     C                   clear                   I14TLA                         TIPO LANCIO
176700990730     C                   movel     V1CRSM        I14RSC                         RAGIONE SOCIALE
176800990730     C                   clear                   I14RS2                         2' RAGIONE SOCIALE
176900990730     C                   movel     V1CINM        I14IND                         INDIRIZZO
177000020313     c                   movel     'S'           i14cnz
177100990730     C                   movel     V1CLOM        E14LOC                         LOCALITA'
177200071024     c* partita iva solo se sprotetta
177300071024     C  n90              move      V1CPIM        E14PIP                         PARTITA IVA (14 CAR)
177400990730     c                   movel     V1CPRM        E14PRV                         PROVINCIA
177500990730     C                   movel     V1CNZM        E14NAR                         NAZIONE
177600990730     C                   movel     V1CCAM        E14CAP                         C.A.P.
177700990730     C                   movel     V1CFAP        I14ISO                         INOLTRO
177800020313     c
177900990730     C                   eval      WCLI = 'M'
178000990730     C                   EXSR      CTRIND
178100990730      * CAMPI CHE POSSO AVER IMPOSTATO
178200990730     C                   movel     E14LOC        V1CLOM
178300990730     C                   movel     E14PRV        V1CPRM
178400990730     C                   movel     E14NAR        V1CNZM
178500990730     C                   movel     E14CAP        V1CCAM
178600071024     C  n90              movel     E14PIP        V1CPIM
178700990730     C                   eval      WCTS = 'S'
178800990730     C                   ENDIF
178900990730      *
179000990730      * MITTENTE CODIFICATO
179100990730     C                   WHEN      WQUATT <> 8888
179200990730     C                   eval      *in87 = *on
179300990730      *
179400990730     C                   IF        §TBTPO = 'F'  or
179500990730     C                              (§TBTPO = 'A'  and V1Cmit > *zeros)
179600020516     C                   movel     DUTkci        $KCC                           CONTO
179700990730     C                   movel     V1CMIT        $KSC                           SOTTOCONTO
179800130404     C                   movel     tasccm        $KSCtas                        SOTTOCONTO
179900990730     C                   movel     V1CRSM        $KSCD                          DESCRIZIONE
180000990730     C                   movel     'S'           WCTS              1
180100990730     C                   exsr      CHKCLI
180200990730      *
180300990730     c                   IF        $ERR = '1'
180400990730     c                   if        *in91 = *off
180500990730     C                   EXSR      CARMSG
180600990730     C                   eval      *in63 = *on
180700990730     c                   endif
180800990730     C                   ELSE
180900990730     C                   movel     $KSCD         V1CRSM
181000990730     C                   if        V1CMIT <> WV1Cmit or  V1Cmit = *zeros
181100990730     C                   movel     $KSC          V1CMIT
181200990730     C                   movel     V1CMIT        WV1Cmit
181300990730     C                   movel     INDVIA        V1CINM
181400990730     C                   movel     INDCIT        V1CLOM
181500990730     C                   movel     INDPRV        V1CPRM
181600061107
181700061107      * se non ho caricato prima il codice fiscale lo carico ora
181800071106     c***                if        v1cdfm = *blanks
181900061107     c                   eval      v1cdfm = indcdf
182000071106     c***                endif
182100071024      * partita IVA
182200071106     c*****              if        v1cpim=*blanks
182300071024     c                   IF        %subst(INDiva:1:2) < *zeros
182400071024     C                   eval      V1Cp2m = %subst(INDiva:1:2)
182500071024     C                   eval      V1Cp1m = %subst(INDiva:3)
182600071024     c                   ELSE
182700071024     c                   movel     INDiva        V1Cp1m
182800071024     c                   ENDIF
182900071106     c*****              ENDIF
183000080922     c* solo se nn c'e' il mittente originale, carico cap e nazione
183100080922     c                   if        wmitor<>'S'
183200990730     C                   movel     INDCAE        V1CCAM                         C.A.P. EUROPEO
183300990730     C                   movel     INDSTA        V1CNZM
183400990730     c                   endif
183500080922     c
183600080922     c                   endif
183700990730     C                   ENDIF
183800990730      *
183900990730     C                   ENDIF
184000040930     c
184100040930     c* Il cliente codificato non deve avere un ntw LOG o XXX
184200040930     c                   if        $err='0'
184300040930     c                   movel     v1cmit        w0030
184400040930     c     w0030         chain     azorg01l
184500040930     c                   if        %found(azorg01l)
184600040930     c                   movel     orgde3        og143
184700040930     c                   else
184800040930     c                   movel     'XXX'         §ogntw
184900040930     c                   endif
185000040930     c                   if        §ogntw='LOG' or §ogntw='XXX'
185100040930     C                   eval      $err = '1'
185200040930     c                   eval      *in63 = *on
185300040930     C                   movel     MSG(68)       $MSG
185400040930     C                   exsr      CARMSG
185500040930     C                   ENDIF
185600040930     C                   ENDIF
185700990730     C                   ENDSL
185800990806      * Il codice mittente non può essere zero se non ho errori
185900990806     c                   IF        V1Cmit = *zeros  and  $ERR = '0'
186000990806     C                   eval      $err = '1'
186100990806     c                   eval      *in63 = *on
186200990806     C                   movel     MSG(11)       $MSG
186300990806     C                   exsr      CARMSG
186400990806     C                   ENDIF
186500990730      *
186600990730      * CODICE TASSAZIONE MITTENTE
186700990730     C                   IF        WCTS = 'S'  and  $ERR = '0'
186800020516      *
186900080922     C                   if        O95CTS <> V1CMCT   and wmitor<>'S'
187000990730     C                   eval      *in54 = *on
187100990730     C                   MOVEL     O95CTS        V1CMCT                         COD.TASSAZIONE
187200990730     C                   MOVEL     '1'           $PUNTO                         SIMULO '?'
187300990730     C                   endif
187400990730      *
187500990730      *   Il flag anteporto lo imposto dal CAP solo se quest'ultimo è stato variato
187600990730     C                   IF        O95ISO <> V1CFAP
187700990730      *
187800990730    6C                   if            V1CCAM <> Wcam
187900990730    6C                             or (V1CLOM <> Wlom  and  Wlom <> *blanks)
188000990730     C                             or  V1Cfap = *blanks
188100990730      *   Se è consegna a magazzino metto sempre 'C'
188200990730     c                   SELECT
188300990730     C                   WHEN      V1CFDN <> 'SI'
188400990730     C                   movel     O95ISO        V1CFAP                         ANTEPORTO
188500990730     C                   eval      *in53 = *on
188600990730     C                   eval      $PUNTO = '1'                                 SIMULO '?'
188700990730    7C                   WHEN      V1CFAP <> 'C'
188800990730     C                   movel     'C'           V1CFAP
188900990730     C                   eval      *in53 = *on
189000990730     C                   eval      $PUNTO = '1'                                 SIMULO '?'
189100990730     C                   ENDSL
189200990730      *
189300990730     C                   ENDIF
189400990730     C                   ENDIF
189500990730     C                   ENDIF
189600990730      *
189700990730      * Memorizzo Località e CAP MITTENTE
189800990730     C                   IF        $ERR = '0'
189900990730     C                   movel     V1CCAM        Wcam
190000990730     C                   movel     V1CLOM        Wlom
190100990730     C                   ENDIF
190200990730      *
190300990730      * CONTROLLO FLAG ANTEPORTO SE E' GIUSTO
190400110624    1c                   IF        $ERR = '0'  and
190500990730     c                             *in91 = *off
190600990730     C                   movel     '9A'          §COD                           *CODICE TABELLA
190700990730     C                   movel(P)  V1CFAP        §KEY                           *CODICE ELEMENTO
190800990730     C                   EXSR      CHKTAB
190900990730     C                   movel     §KEY          V1CFAP
191000110624    2c                   if        $ERR = '1'  and
191100990730     c                             *in91 = *off
191200990730     C                   movel     MSG(41)       $MSG
191300990730     C                   EXSR      CARMSG
191400990730     C                   eval      *in55 = *on
191500110624   x2c                   else
191600110624     c
191700110624     c                   movel     tbluni        ds9a
191800110624    3c                   if        §9autiop='N'
191900110624     C                   eval      $ERR = '1'
192000110624     C                   movel     MSG(41)       $MSG
192100110624     C                   EXSR      CARMSG
192200110624     C                   eval      *in55 = *on
192300110624    3C                   endif
192400110624    2C                   endif
192500110624     c
192600110624    1C                   ENDIF
192700990730      *--------------------
192800990730      * R A G I O N E    S O C I A L E    M I T T E N T E
192900990730      *--------------------
193000990730      *  SEMPRE OBBLIGATORIA
193100990730     c                   IF        $ERR = '0'  and
193200990730     C                             *in81 = *on  and  V1Crsm = *blanks
193300990730     C                   eval      $ERR = '1'
193400990730     C                   movel     MSG(12)       $MSG
193500990730     C                   EXSR      CARMSG
193600990730     C                   eval      *in64 = *on
193700990730     C                   ENDIF
193800990730      *--------------------
193900990730      * N A Z I O N E    M I T T E N T E
194000990730      *--------------------
194100990730     c                   IF        $ERR = '0'
194200020327     c                   eval      codtab = '15'
194300020327      * ricerca con nuovo pgm ordinato per descrizione
194400020327     c     '?'           scan      v1cnzm                                 99
194500020327     c                   if        *in99
194600020327     c                   call      'TRUL19R'
194700020327     c                   parm                    codtab            2
194800020327     c                   parm      '1'           ordina            1
194900020327     c                   parm                    ktab              8
195000020327     c                   parm                    cmd               1
195100020327     c                   movel     ktab          v1cnzm
195200020327     c                   endif
195300020327      * controllo
195400020327     C                   movel     '15'          §COD                           *CODICE TABELLA
195500020327     C                   movel(P)  V1CNZM        §KEY                           *CODICE ELEMENTO
195600020327     C                   EXSR      CHKTAB
195700990730      *
195800990730     c                   IF        $ERR = '1'
195900990730     C                   If        *IN91 = *off
196000990730     C                   clear                   DS15
196100990730     C                   movel     MSG(30)       $MSG
196200990730     C                   EXSR      CARMSG
196300990730     C                   eval      *in73 = *on
196400990730     C                   Endif
196500990730      *
196600990730     C                   ELSE
196700990730     C                   movel     TBLUNI        DS15
196800990730      * SE NAZIONE ITALIA METTO BLANKS
196900990730     C                   IF        §15ITA = 'I'  and  V1Cnzm <> *blanks
197000990730     C                   clear                   V1CNZM
197100990730     C                   ENDIF
197200990730      * SALVO IN CAMPO DI COMODO LA VALUTA DELLA NAZIONE MITTENTE
197300990730     C                   movel     §15VLT        WVLT
197400011023      * salvo se mittente italia o estero
197500011023     c                   if        §15ita = 'I'
197600011023     c                   eval      wmitie = 'I'
197700011023     c                   else
197800011023     c                   eval      wmitie = 'E'
197900011023     c                   endif
198000990730     C                   ENDIF
198100990730     C                   ENDIF
198200021122      *--------------------
198300021122      * P E S O
198400021122      *--------------------
198500021126      * Per bolla FedEx msg se peso supera il limite imposto da tabella DFE
198600021126      * Per tariffa documenti
198700050627      * solo se non c'è "?" in codice tariffa
198800050627
198900021125     c                   If        $err = '0' and LnaNtw = 'FED'
199000050627     c                             and %scan('?':V1cCtr)=0
199100021122     c                   Move      V1cCtr        DueCtr
199200021122     c                   If        V1cPkf > §DfePkgD and DueCtr >= §DfeDalD
199300021125     c                             and DueCtr <= §DfeAlD
199400021122     C                   eval      $err = '1'
199500021125     C                   movel     MSG(62)       $MSG
199600021125     c                   Eval      %Subst($msg:68:8) = (%editc(§DfePkgd: '4'))
199700021122     C                   EXSR      CARMSG
199800021125     c                   Eval      *In57= *On
199900021122     c                   EndIf
200000021122     c                   EndIf
200100990730      *--------------------
200200990730      * C O D.    T A S S A Z I O N E    M I T T E N T E
200300990730      *--------------------
200400990730     c                   IF        $ERR = '0'  and
200500990730     C                             V1CMCT <> *blanks
200600990730     C                   move      *BLANKS       V1DMCT                         *DESCRIZIONE
200700990730     C                   movel     'CT'          §COD                           *CODICE TABELLA
200800990730     C                   movel     *BLANKS       §KEY
200900990730     C                   movel     V1CMCT        §KEY                           *CODICE ELEMENTO
201000990730     C                   EXSR      CHKTAB
201100990730     C                   movel     §DES          V1DMCT                         *DESCRIZIONE
201200990730     C                   ENDIF
201300071025      *----------------------------------------------------------------
201400071025      * C O D I C E   F I S C A L E   M I T T E N T E   E PARTITA IVA
201500071025      *----------------------------------------------------------------
201600061107      * controllo il codice fiscale del mittente se non è protetto
201700071025      * controllo anche la partita iva
201800151104     c                   clear                   pivaflg           1 0
201900151104     c                   move      v1cmit        wquatt
202000151104     c
202100080423    1c                   if        not *in90 and $err='0'
202200071105     c* partita IVA se mittente protetto
202300151104   1aC****               IF        *IN92 = *on or  *IN91 = *on
202400071105     c* Controllo correttezza partita iva se inserita
202500151104   1bc****               if        v1cpim<>*blanks and $err='0'
202600071105     c* Se cambiata pulisco la forzatura
202700071106    2c                   if        v1cpim<>sav1cpim and winizia=*blanks
202800071105     c                   eval      forzapim2=*off
202900071105     c                   movel     v1cpim        sav1cpim
203000071105    2c                   endif
203100151104     c                   clear                   xcfiva1ds
203200071105     c                   eval      xcfivamod = 'P'
203300071105     c                   eval      xcfivapar = v1cpim
203400071105     c                   eval      xcfivanar = v1cnzm
203500071105     c                   eval      xcfivaprv = v1cprm
203600151104     c                   eval      xcfivacap = v1ccam
203700151104     c                   eval      xcfivaloc = v1clom
203800151104     c                   if        wquatt=8888
203900151109     c                   eval      xcfivap88='8'
204000151104     c                   endif
204100151104     c
204200151104     c                   call      'XCFIVAR1'
204300151104     c                   parm                    xcfiva1ds
204400071105      * --> errata forzabile
204500071105    2c                   If        xcfivaflg = -9 and forzaPIM2 = *off
204600071106     c                             and winizia=*blanks
204700071105     c                   eval      *in42=*on
204800071105     c                   Eval      $msg = xcfivamsg
204900071105     c                   Eval      $msg = %trim($msg) + ' Enter x forzare'
205000071105     c                   eval      forzaPIM2= *on
205100071105     c                   eval      $err = '1'
205200071105     C                   exsr      carmsg
205300071105    2c                   EndIf
205400071105      * --> errore
205500071105    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
205600071105     c                   eval      *in42=*on
205700071105     c                   eval      $msg = xcfivamsg
205800071105     c                   eval      $err = '1'
205900071105     C                   exsr      carmsg
206000071105    2c                   EndIf
206100151104     c                   eval      pivaflg=xcfivaflg
206200071105     c
206300071105      * se non impostato devo riportare il codice iso della partita iva che
206400071105      * mi viene passato dal pgm di controllo
206500071105    2c                   if        v1cp2m <> ivaeu
206600071105     c                   eval      v1cp2m = ivaeu
206700071105     c                   eval      *in42=*on
206800071105     c                   eval      $err = '1'
206900071105    2c                   endif
207000151104   1bc******             endif
207100151104   1ac******             endif
207200071025      * Solo per codice fiscale:
207300061107      * se inserito controllo se corretto
207400080423    2c********           if        v1cdfm <> *blanks and $err='0'
207500080423    2c                   if        $err='0'
207600151104     c                   clear                   xcfiva1ds
207700061107     c                   eval      xcfivapar = v1cdfm
207800061107     c                   eval      xcfivanar = v1cnzm
207900061107     c                   eval      xcfivaprv = v1cprm
208000151104     c                   eval      xcfivacap = v1ccam
208100151104     c                   eval      xcfivaloc = v1clom
208200151104     c                   eval      xcfivapiva= v1cpim
208300151104     c                   if        wquatt= 8888
208400151104     c                   eval      xcfivap88='8'
208500151104     c                   endif
208600151104     c                   call      'XCFIVAR1'
208700151104     c                   parm                    xcfiva1ds
208800061107      * errore
208900080423    3c                   if        xcfivaflg < *zeros
209000061107     c                   eval      $err = '1'
209100061107     c                   eval      *in58 = *on
209200080423     c                   eval      $msg = xcfivamsg
209300061107     c                   exsr      carmsg
209400080423    3c                   endif
209500080423    2c                   endif
209600080423     c
209700120508     c* Se presente partita IVA '$$' su fattura immediata errore bloccante
209800120508     c                   if        $err='0'
209900120508     c                             and %scan('$$':v1cpim)>0 and wquatt=8888
210000120508     c                   eval      $err = '1'
210100120508     c                   eval      *in42 = *on
210200120508     c                   eval      $msg = msg(77)
210300120508     c                   exsr      carmsg
210400120508     c                   endif
210500080423      * se non c'è il codice fiscale errore forzabile
210600080423    2c                   if        $err='0'   and
210700080423    2c                             wforzacdfm = *off  and winizia=*blanks and
210800120508     c                             v1cdfm=*blanks and v1cpim=*blanks
210900151104
211000120508     c                   if        xcfivaflg<>9
211100120508     c                   eval      $msg = msg(75)
211200151104     c                   eval      $err = '1'
211300151104     c                   eval      *in42 = *on
211400151104     c                   eval      wforzacdfm = *on
211500120508     c                   else
211600151104     c                   if        pivaflg<>9
211700120508     c                   eval      $msg = msg(76)
211800151104     c                   eval      $err = '1'
211900151104     c                   eval      *in42 = *on
212000151104     c                   eval      wforzacdfm = *on
212100120508     c                   endif
212200151104     c                   endif
212300151104     c
212400080423     c                   exsr      carmsg
212500080423    2c                   endif
212600120508     c* errore forzabile: se inserito codice fiscale lungo 11 inserire anche la
212700120508     c*                   P.IVA  per 8888
212800120508    2c                   if        $err='0'   and
212900120508    2c                             wforzacdfm1 = *off  and winizia=*blanks and
213000120508     c                             v1cdfm<>*blanks and v1cpim=*blanks
213100120508     c                             and %subst(v1cdfm:12:1)=' '  and wquatt=8888
213200120508     c****                         ((v1cdfm = *blanks and xcfivaflg<>9)
213300120508     c***                            or v1cpim=*blanks)
213400120508     c                   eval      $err = '1'
213500120508     c                   eval      *in42 = *on
213600120508     c                   eval      wforzacdfm1 = *on
213700120508     c                   eval      $msg = msg(76)
213800120508     c                   exsr      carmsg
213900120508    2c                   endif
214000080423    1c                   endif
214100990730      *--------------------
214200990730      * D E S T I N A T A R I O
214300990730      *--------------------
214400020516      *
214500020516      * se modificato il destinatario simulo una interrogazione per
214600020516      *  riemettere la videata
214700020516     C                   if        WsavDES <> V1Cdes
214800020516     C                   eval      WsavDES =  V1Cdes
214900020516     C                   eval      $PUNTO  =  *on
215000020516     C                   endif
215100020516      *
215200990730     C                   move      V1CDES        WQUATT                         ULTIME QUATTRO DX
215300990730      *
215400990730      * NON PUO' ESSERE 8888
215500990730     c                   IF        $ERR = '0'  and  WQUATT = 8888
215600990730     C                   eval      *in66 = *on
215700990730     C                   eval      $err = '1'
215800990730     C                   movel     MSG(11)       $MSG
215900990730     C                   EXSR      CARMSG
216000990730     C                   ENDIF
216100020516      * NON può essere 9999 in modifica spedizioni di sede da parte di P.O.
216200020516     c                   IF        $ERR = '0'  and  WQUATT = 9999
216300020516     c                             and (*in22 and not *in89)
216400020516     c                             and  not *in91
216500020516     c                             and  not *in94
216600020516     c                   eval      $err = '1'
216700020516     c                   eval      *in66 = *on
216800020516     c                   movel     MSG(11)       $MSG
216900020516     c                   exsr      CARMSG
217000020516     c                   endif
217100990730      *
217200990730      * SE DESTINATARIO CODIFICATO VERIFICO L'ESISTENZA DEL CODICE
217300990806      *?*  SE HA TUTTO L'INDIRIZZO E' VUOTO, SOSTITUISCO CON QUELLO TROVATO IN ANAGRAFICA
217400990730     c                   IF        $ERR = '0'
217500990730     C                   if        WQUATT <> 9999
217600990730      *
217700990730      * ESCLUDO SE 94 ON
217800990730     c                   if        *IN94 = *off
217900020516     C                   movel     DUTkci        $KCC                           CONTO
218000990730     C                   movel     V1CDES        $KSC                           SOTTOCONTO
218100130404     C                   movel     tasksc        $KSCtas                        SOTTOCONTO
218200990730     C                   clear                   $KSCD                          DESCRIZIONE
218300990806     C                   movel     V1Dksc        $KSCD                          DESCRIZIONE
218400990730     C                   EXSR      CHKCLI
218500990806     c                   IF        $ERR = '1'
218600990730     c                   if        *in91 = *off
218700990730     C                   EXSR      CARMSG
218800990730     C                   eval      *in66 = *on
218900130228     c                   endif
219000040930     c
219100990730     C                   ELSE
219200990730      *
219300990730      * RICERCO L'INDIRIZZO SOLO SE TUTTI I CAMPI VIDEO SONO VUOTI
219400990730     C                   if        V1CDES = *zeros
219500990730     C                   movel     $KSC          V1CDES
219600990730     C                   ENDIF
219700130228     c
219800990806      * Decodifico intestatario fattura
219900990806     C                   movel     $KSCD         V1DKSC
220000120327     c* controllo in ntw del cliente no LOG e XXX
220100120327     c                   IF        $ERR = '0'
220200120327     c                   movel     v1cdes        w0030
220300120327     c     w0030         chain     azorg01l
220400120327     c                   if        %found(azorg01l)
220500120327     c                   movel     orgde3        og143
220600120327     c                   else
220700120327     c                   movel     'XXX'         §ogntw
220800120327     c                   endif
220900120327     c                   if        §ogntw='LOG' or §ogntw='XXX'
221000120327     C                   eval      $err = '1'
221100120327     c                   eval      *in66 = *on
221200120327     C                   movel     MSG(68)       $MSG
221300120327     C                   exsr      CARMSG
221400120327     C                   ENDIF
221500120327     C                   ENDIF
221600990730      *
221700990730     C                   ENDIF
221800990730      *
221900990730     C                   ENDIF
222000030331
222100030331      * In caso di modifica cliente destinatario da 9999 ad altro codice e
222200030416      * solo se sono un utente di sede e
222300030331      * in presenza di fattura, per le bolle dove è accettato l'imponibile a 0,
222400030331      * devo cancellare i dati di fatturazione
222500030331     c                   If        WsavDES9 <> V1Cdes
222600030416     c                             and DutPou = 046
222700030331     c                   Move      WsavDes9      WQuatt9
222800030331      * il cliente destinatario, prima della modifica, è un 9999
222900030331     c                   If        Wquatt9 = 9999 and
223000030331      * è un tipo bolla che accetta l'imponibile a 0
223100030331     c                             §3aIm0 = 'S' and
223200030411      * la bolla è già fatturata ma non contabilizzata
223300030411     c                             TasFic = *Blanks and TasDft > 0
223400030331      * la bolla è stata consegnata
223500030416     c                   If        TasDcm > *Zeros
223600030331      * devo eliminare i dati della fatturazione ed emettere msg info a video
223700030331     c                   Eval      wNoFatt = *On
223800030331     c                   Clear                   v2cbol
223900030331     c                   Clear                   v2civa
224000030331     c                   Clear                   v2cift
224100030331     c                   Clear                   v2cfiv
224200030331     c                   Clear                   v2cnft
224300030331     c                   Clear                   v2cdft
224400030331     c                   Clear                   v2cfei
224500030416     c                   Eval      WsavDES9 = V1Cdes
224501170628     c                   clear                   w_ali
224600030411     c                   Else
224700030416      * se la bolla non è consegnata
224800030416      * o non sono ancora trascorsi i gg. di pulizia
224900030411      * emetto un msg a video
225000030411     c                   Eval      $Err = '1'
225100030411     c                   Eval      *In66 = *On
225200030411     c                   Eval      $Msg = Msg(66)
225300030411     c                   ExSr      CarMsg
225400030411     c                   EndIf
225500030331     c                   EndIf
225600030331     c                   EndIf
225700120416     c* se presente bolla in arb e assegnato non abilitato emetto msg a video
225800120417     c                   if        $err='0' and *in94=*off
225900120416     c     keyta1        chain     fnarb01l
226000120416     c                   if        %found(fnarb01l) and arbaca<>'S'
226100120416     c                   Eval      $Err = '1'
226200120416     c                   Eval      *In66 = *On
226300120416     c                   Eval      $Msg = Msg(66)
226400120416     c                   ExSr      CarMsg
226500120416     c                   endif
226600120416     c                   endif
226700030331
226800990730     c                   ENDIF
226900990730      *
227000990730     C                   ENDIF                                                  $ERR = '0'
227100990730      *
227200990826      *--------------------
227300990826      * F L A G   I N O L T R O
227400990826      *--------------------
227500990826     c                   if        v1cfin<>savfin
227600990826     c                   clear                   wuno
227700990826     c                   movel     v1cfin        savfin
227800990826     c                   endif
227900990826     c
228000990826     C     V1CFIN        IFEQ      *BLANKS
228100990826      * SE FERMO DEPOSITO IMPOSTO 'C'
228200990826     C* AL MOMENTO NON LO calcoliamo piu' per cui se viene tolto deve
228300990826     c*  reimpostato a mano
228400990826     C     V1CFFD        IFEQ      'S'
228500110624     C                   movel     'C'           V1CFIN
228600990826     C                   movel     '1'           $PUNTO                         SIMULO '?'
228700990826     C                   ENDIF
228800990826     C                   ENDIF
228900990826     c
229000990826     c                   if        v1cffd='S'  AND v1cfin<>'C'  AND
229100990826     c                             wuno=' '
229200990826     C                   movel     MSG(45)       $MSG
229300990826     C                   EXSR      CARMSG
229400990826     C                   SETON                                        56
229500990826     c                   movel     ' '           wuno              1
229600990826     c                   endif
229700990826
229800110624    1c                   IF        $ERR = '0'  and  *in91 = *off
229900990730     C                   movel     '9A'          §COD                           *CODICE TABELLA
230000990730     C                   movel(P)  V1CFIN        §KEY                           *CODICE ELEMENTO
230100990730     C                   EXSR      CHKTAB
230200990730     C                   movel     §KEY          V1CFIN
230300110624    2c                   IF        $ERR = '1'
230400990730     C                   movel     MSG(42)       $MSG
230500110624     C                   eval      *in56 = *on
230600990730     C                   EXSR      CARMSG
230700110624   x2c                   else
230800110624     c
230900110624     c                   movel     tbluni        ds9a
231000110624    3c                   if        §9autiop='N'
231100110624     C                   eval      $ERR = '1'
231200110624     C                   movel     MSG(42)       $MSG
231300110624     C                   EXSR      CARMSG
231400110624     C                   eval      *in56 = *on
231500110624    3C                   endif
231600110624    2C                   ENDIF
231700110624    1C                   ENDIF
231800990730      *--------------------
231900990730      * C O D.    T A S S A Z I O N E    D E S T I N A T A R I O
232000990730      *--------------------
232100990730     c                   IF        $ERR = '0'  and  V1CCTS <> *BLANKS
232200990730     C                   clear                   V1DCTS                         *DESCRIZIONE
232300990730     C                   movel     'CT'          §COD                           *CODICE TABELLA
232400990730     C                   movel(p)  V1CCTS        §KEY                           *CODICE ELEMENTO
232500990730     C                   EXSR      CHKTAB
232600990730     C                   movel     §DES          V1DCTS                         *DESCRIZIONE
232700990730     C                   ENDIF                                                  $ERR = '0'
232800071026      *---------------------------------------------------------------------
232900071026      * C O D I C E   F I S C A L E   D E S T I N A T A R I O  E Partita IVA
233000071026      *---------------------------------------------------------------------
233100151104     c                   clear                   pivaflg
233200061107      * controllo il codice fiscale del destinatario se non è protetto
233300080423    1c                   if        not *in93 and $err='0'
233400071105     c* Controllo correttezza partita iva se inserita
233500151104   1ac***                if        v1cpid<>*blanks and $err='0'
233600071105     c* Se cambiata pulisco la forzatura
233700071106    2c                   if        v1cpid<>sav1cpid and winizia=*blanks
233800071105     c                   eval      forzapid2=*off
233900071105     c                   movel     v1cpid        sav1cpid
234000071105    2c                   endif
234100151104     c                   clear                   xcfiva1ds
234200071105     c                   eval      xcfivamod = 'P'
234300071105     c                   eval      xcfivapar = v1cpid
234400071105     c                   eval      xcfivanar = v1cnzd
234500071105     c                   eval      xcfivaprv = v1cprd
234600151104     c                   eval      xcfivacap = v1ccad
234700151104     c                   eval      xcfivaloc = v1clod
234800151104     c                   if        wquatt= 9999
234900151104     c                   eval      xcfivap88='8'
235000151104     c                   endif
235100151104     c                   call      'XCFIVAR1'
235200151104     c                   parm                    xcfiva1ds
235300071105      * --> errata forzabile
235400071105    2c                   If        xcfivaflg = -9 and forzaPID2 = *off
235500071106     c                             and winizia=*blanks
235600071105     c                   eval      *in43=*on
235700071105     c                   Eval      $msg = xcfivamsg
235800071105     c                   Eval      $msg = %trim($msg) + ' Enter x forzare'
235900071105     c                   eval      forzaPID2= *on
236000071105     c                   eval      $err = '1'
236100071105     C                   exsr      carmsg
236200071105    2c                   EndIf
236300071105      * --> errore
236400071105    2c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
236500071105     c                   eval      *in43=*on
236600071105     c                   eval      $msg = xcfivamsg
236700071105     c                   eval      $err = '1'
236800071105     C                   exsr      carmsg
236900071105    2c                   EndIf
237000071105     c
237100151104     c                   eval      pivaflg=xcfivaflg
237200151104     c
237300071105      * se non impostato devo riportare il codice iso della partita iva che
237400071105      * mi viene passato dal pgm di controllo
237500071105    2c                   if        v1cp2d <> ivaeu
237600071105     c                   eval      v1cp2d = ivaeu
237700071105     c                   eval      *in43=*on
237800071105     c                   eval      $err = '1'
237900071105    2c                   endif
238000151104   1ac****               endif
238100061107      * se inserito controllo se corretto
238200080423    2c*****              if        v1cdfd <> *blanks and $err='0'
238300080423    2c                   if        $err='0'
238400151104     c                   clear                   xcfiva1ds
238500061107     c                   eval      xcfivapar = v1cdfd
238600061107     c                   eval      xcfivanar = v1cnzd
238700061107     c                   eval      xcfivaprv = v1cprd
238800151104     c                   eval      xcfivacap = v1ccad
238900151104     c                   eval      xcfivaloc = v1clod
239000151104     c                   eval      xcfivapiva= v1cpid
239100151104     c                   if        wquatt= 9999
239200151104     c                   eval      xcfivap88='8'
239300151104     c                   endif
239400151104     c                   call      'XCFIVAR1'
239500151104     c                   parm                    xcfiva1ds
239600061107      * errore
239700080423    3c                   if        xcfivaflg < *zeros
239800061107     c                   eval      $err = '1'
239900061107     c                   eval      *in59 = *on
240000080423     c                   eval      $msg = xcfivamsg
240100061107     c                   exsr      carmsg
240200080423    3c                   endif
240300080423    2c                   endif
240400120508     c*
240500120508     c                   move      v1cdes        wquatt
240600120508     c* Se presente partita IVA '$$' su fattura immediata errore bloccante
240700120508     c                   if        $err='0'
240800120508     c                             and %scan('$$':v1cpid)>0 and wquatt=9999
240900120508     c                   eval      $err = '1'
241000120508     c                   eval      *in43 = *on
241100120508     c                   eval      $msg = msg(77)
241200120508     c                   exsr      carmsg
241300120508     c                   endif
241400080423     c
241500120508      * se non c'è nè il codice fiscale nè partita iva errore forzabile
241600080423    2c                   if        $err='0' and
241700080423    2c                             wforzacdfd = *off and winizia=*blanks and
241800120508     c                             v1cdfd=*blanks and v1cpid=*blanks
241900120508     c                   if        xcfivaflg<>9
242000120508     c                   eval      $msg = msg(75)
242100151104     c                   eval      $err = '1'
242200151104     c                   eval      *in43 = *on
242300151104     c                   eval      wforzacdfd = *on
242400120508     c                   else
242500151104     c                   if        pivaflg <> 9
242600120508     c                   eval      $msg = msg(76)
242700151104     c                   eval      $err = '1'
242800151104     c                   eval      *in43 = *on
242900151104     c                   eval      wforzacdfd = *on
243000120508     c                   endif
243100151104     c                   endif
243200080423     c                   exsr      carmsg
243300080423    2c                   endif
243400120508     c* errore forzabile: se inserito codice fiscale lungo 11 inserire anche la
243500120508     c*                   P.IVA  per 9999
243600120508    2c                   if        $err='0' and
243700120508    2c                             wforzacdfd1 = *off and winizia=*blanks and
243800120508     c                             v1cdfd<>*blanks and %subst(v1cdfd:12:1)=' '
243900120508     c                             and v1cpid=*blanks and wquatt=9999
244000120508     c                   eval      $err = '1'
244100120508     c                   eval      *in43 = *on
244200120508     c                   eval      wforzacdfd1 = *on
244300120508     c                   eval      $msg = msg(76)
244400120508     c                   exsr      carmsg
244500120508    2c                   endif
244600120508     c
244700080423    1c                   endif
244800990730      *--------------------
244900990730      * T A R I F F A
245000990730      * FARE DOPO IL CLIENTE
245100990730      *--------------------
245200990730     c                   IF        $ERR = '0'
245300990730      *
245400990730     C     §TBTPO        IFEQ      'F'                                          'F'=FRANCO
245500990730     C                   movel     V1CMIT        $KSC                           MITTENTE
245600990730     C                   ELSE
245700990730     C                   movel     V1CDES        $KSC                           DESTINATARIO
245800990730     C                   ENDIF
245900990730     C                   move      $KSC          WQUATT                         ULTIMI 4 A DX
246000990730      *
246100990730     C                   if        WQUATT <> 8888  and  WQUATT <> 9999
246200990730      *
246300990730     C                   Z-ADD     TA$DSP        $DSP              8 0          DATA SPED. (A/M/G)
246400990730     C                   movel     V1CCTR        $CTR              3
246500990730     C                   EXSR      CHKCTR
246600990730     c                   eval      *in67 = ($ERR <> '0' and  *in91 = *off)
246700990730     C                   movel     $CTR          V1CCTR
246800990730     C                   movel     $CTD          V1DCTR
246900021122
247000021122      * Se il cliente non ha tariffe
247100021122If   c                   If        $tks <> 'S' and Not *In91
247200021129     c                             and LnaNtw = 'FED'
247300021122      *      non posso accettare una tariffa documenti se il peso della
247400021122      *      bolla superare il limite previsto in tabella "DFE"
247500021122     c                   Move      V1cCtr        DueCtr
247600021122If   c                   If        V1cPkf > §DfePkgD and
247700021122     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
247800021122     C                   eval      $err = '1'
247900021122     C                   movel     MSG(62)       $MSG
248000021122     c                   Eval      %Subst($msg:68:8) = (%editc(§DfePkgd: '4'))
248100021122     C                   EXSR      CARMSG
248200021122     c                   Eval      *In67 = *On
248300021122e    c                   EndIf
248400021122e    c                   EndIf
248500990730      *
248600990730     C                   ELSE
248700020213
248800160229      * Richiamo il trul27r1 per avere il FIE per poi caricare la cartello
248900160229     c                   clear                   trul27ds1
249000020312     c                   eval      i27tsp = v1ctsp
249100020213     c                   eval      i27lna = v1clna
249200020213     c                   eval      i27lnp = v1clnp
249300020312     c                   z-add     $ksc          i27cli
249400021122     c                   Eval      I27Pkg = V1cPkf
249500160229     c                   call      'TRUL27R1'
249600160229     c                   parm                    trul27ds1
249700160229     c
249800021122     c                   Movel     O27Fie        Wfie
249900990730      *
250000990730      * PER CLIENTE NON CODIFICATO --> SOLO TARIFFA DI CARTELLO
250100160229     c****               if        *in91 = *off  and  V1Cctr <> TARctr
250200021122      * per bolla non FedEx
250300120607     c****               If        O27Ntw <> 'FED'
250400120607     c* EDPES/RM 07/06/12
250500120607      * visto che la tariffa di cartello viene caricata dal pgm di tassazione  TNSF20R per 8888/9999
250600120607     c*  senza usare il cod tariffa ricevuto, il controllo sul cod tariffa=tariffa di cartello
250700120607     c*  non lo eseguo più ma solo per fedex perchè devo differenziare tar.documenti da merci
250800120607     c*  Anche in immissione bolle fa così.
250900120607
251000120607      *  Per bolla FedEx con O27Fie = 'M'
251100120607     c****                         or (O27Ntw = 'FED' and WFie = 'M')
251200120607     c                   if           (O27Ntw = 'FED' and WFie = 'M')
251300160229     c* il cod tariffa può essere solo MERCI
251400160229     c                   clear                   trulc7ds
251500160229     c                   eval      ic7tfie=wfie
251600160229     c                   call      'TRULC7R'
251700160229     c                   parm                    trulc7ds
251800160229
251900160229     c                   if        v1cctr<>%editc(oc7ctrc:'X')
252000990730     C                   eval      $err = '1'
252100990730     C                   movel     MSG(26)       $MSG
252200160229     c                   eval      %subst($msg:74:3)=%editc(oc7ctrc:'X')
252300990730     C                   EXSR      CARMSG
252400990730     C                   eval      *in67 = *on
252500021122     C                   ENDIF
252600990730     C                   ENDIF
252700021122      * Per cliente non codificato 8888 e O27Fie = <> 'M' e bolla FedEx
252800021122     c                   If        O27Ntw = 'FED' and WFie <> 'M'
252900021122     c                             and Wquatt = 8888 and Not *In91
253000160229      * richiamo il trulc7r xchè la tariffa in bolla deve essere una delle
253100021122      * 2 cartello FedEx
253200160229     c                   clear                   trulc7ds
253300160301     c                   Eval      Ic7Ctb = V1cCtr
253400160301     c                   Eval      Ic7Pkg = V1cPkf
253500160229     c                   eval      ic7tfie=wfie
253600160229     c                   call      'TRULC7R'
253700160229     c                   parm                    trulC7ds
253800160229     c                   If        V1Cctr <> %editc(oc7ctrc:'X')
253900021122     C                   eval      $err = '1'
254000021122     C                   movel     MSG(26)       $MSG
254100160229     c                   eval      %subst($msg:74:3)=%editc(oc7ctrc:'X')
254200021122     C                   EXSR      CARMSG
254300021122     C                   eval      *in67 = *on
254400021122     c                   EndIf
254500021122     c                   EndIf
254600021122
254700990730      *
254800990730      * DECODIFICO IL TIPO TARIFFA (1° CARATTERE SX)
254900990730     C                   movel     'TR'          §COD                           *CODICE TABELLA
255000990730     C                   clear                   §KEY
255100990730     C                   movel     V1CCTR        W001a
255200990730     C                   movel     W001a         §KEY                           *CODICE ELEMENTO
255300990730     C                   EXSR      CHKTAB
255400990730     C                   movel     §DES          V1DCTR                         *DESCRIZIONE
255500990730      *
255600990730     C                   ENDIF
255700990730     C                   ENDIF
255800990730      *--------------------
255900990730      * V O L U M E    F A T T U R A R E
256000990730      *--------------------
256100071106     c                   IF        $ERR = '0'
256200990730      * SE DIVERSO DA QUELLO ORIGINALE, IMPOSTO IL FLAG
256300990730      * MA SOLO SE MODIFICATO DALL'UTENTE
256400990730     c                   IF        *in91 = *off  and  V1Cvlf <> Wtasvlf  and
256500990730     c                             WIN79 = '1'
256600990730      *
256700990730     C                   if        V1CVLF <> *zeros
256800990730     C                   movel     'R'           V1CFVF                         'R'=REALE
256900990730     C                   ELSE
257000990730     C                   clear                   V1CFVF
257100990730     C                   ENDIF
257200990730      *
257300990730     C                   movel     '1'           $PUNTO                         SIMULO '?'
257400990730     C                   ENDIF
257500990730      * ORA POSSO SPEGNERE L'INDICATORE DI COMODO
257600990730     C                   clear                   WIN79
257700071106     C                   ENDIF                                                  $ERR = '0'
257800990730      *--------------------
257900990730      * F L A G    V O L U M E    F A T T U R A R E
258000990730      * DA FARE DOPO IL CLIENTE E DOPO LA TARIFFA
258100990730      *--------------------
258200990730     c                   IF        $ERR = '0'
258300990730      *
258400990730     C     V1CFVF        IFNE      *BLANKS
258500990730     C                   move      *BLANKS       V1DFVF                         *DESCRIZIONE
258600990730     C                   movel     '7T'          §COD                           *CODICE TABELLA
258700990730     C                   movel(P)  V1CFVF        §KEY                           *CODICE ELEMENTO
258800990730     C                   EXSR      CHKTAB
258900990730     C                   movel     §DES          V1DFVF                         *DESCRIZIONE
259000990730     C                   ENDIF
259100990730      *
259200990730      * SE IL FLAG DICE VOLUME AUTOMATICO, LO RICALCOLO
259300990730     c                   if        *in91 = *off  and
259400990730     c                              (V1Cfvf = 'C'  or  V1Cfvf = 'K'
259500990730     c                                             or  V1Cfvf = 'P')
259600990730     C                   CLEAR                   FNLV18DS
259700990730     C                   Z-ADD     V1CMIT        D18KSC                         MITTENTE
259800990730     C                   movel     V1CTBL        D18TBL
259900990730     C                   Z-ADD     V1CAAS        D18AAS
260000990730     C                   Z-ADD     V1CMGS        D18MGS
260100990730     C                   Z-ADD     V1CNCL        D18NCL
260200990730     C                   Z-ADD     V1CPKF        D18PKF
260300990730     C                   movel     V1CCTR        D18CTR
260400160301     c                   if        $tks='S'
260500160301     c                   movel     $prg          d18prg
260600160301     c                   endif
260700020311     c                   eval      d18tsp = v1ctsp
260800020311     c                   eval      d18lna = v1clna
260900020311     c                   eval      d18lnp = v1clnp
261000020311     C                   CALL      'FNLV18R'
261100990730     C                   PARM                    FNLV18DS
261200990730     C                   movel     'S'           WLV18             1            DI COMODO
261300990730     C     D18FVF        IFNE      V1CFVF
261400990730     C     D18VLF        ORNE      V1CVLF
261500990730     C                   movel     D18FVF        V1CFVF
261600990730     C                   Z-ADD     D18VLF        V1CVLF
261700990730     C                   movel     '1'           $PUNTO
261800990730     C                   movel     '1'           WMODIF                         *VARIATA BOLLA
261900990730     C                   ENDIF
262000990730     C                   ENDIF
262100990730     C                   ENDIF                                                  $ERR = '0'
262200990730      *
262300990730      * DEVO DECIDERE SE SOSTITUIRE IL VOLUME BOLLETTATO O NO
262400990730     c                   IF        $ERR = '0'
262500990730     C                   CLEAR                   FNLV20DS
262600990730     C                   movel     'F'           D20TBO                         'F'=FATTURABILE
262700990730     C                   movel     V1CFVF        D20FVL
262800990730     C                   movel     V1CVLF        D20VLU
262900990730     C                   movel     V1CFVB        D20FVB
263000990730     C                   Z-ADD     V1CVLB        D20VLB
263100990730     C                   movel     V1CFVF        D20FVF
263200990730     C                   Z-ADD     V1CVLF        D20VLF
263300160229     c                   movel(p)  fnlv20ds      kpjbu
263400990730     C                   CALL      'FNLV20R'
263500160229     C                   PARM                    kpjba
263600990730     C                   movel     'S'           WLV20             1            DI COMODO
263700160229     c                   movel     kpjbu         fnlv20ds
263800160229     c
263900990730     C                   movel     D20FVB        V1CFVB
264000990730     C                   Z-ADD     D20VLB        V1CVLB
264100990730     C                   ENDIF                                                  $ERR = '0'
264200990730      *--------------------
264300990730      * F L A G    V O L U M E    B O L L E T T A T O
264400990730      * FARE DOPO VOLUME FATTURARE
264500990730      *--------------------
264600990730     c                   IF        $ERR = '0'  and
264700990730     C                             V1CFVB <> *blanks
264800990730     C                   movel     *BLANKS       V1DFVB                         *DESCRIZIONE
264900990730     C                   movel     '7T'          §COD                           *CODICE TABELLA
265000990730     C                   movel     *BLANKS       §KEY
265100990730     C                   movel     V1CFVB        §KEY                           *CODICE ELEMENTO
265200990730     C                   EXSR      CHKTAB
265300990730     C                   movel     §DES          V1DFVB                         *DESCRIZIONE
265400990730     C                   ENDIF
265500990730      *--------------------
265600990730      * C O N S E G N A    P A R T I C O L A R E    (1)
265700990730      *--------------------
265800990730     c                   IF        $ERR = '0'
265900990730     C                   movel     *BLANKS       V1DFTC
266000990730     C     V1CFTC        IFNE      *BLANKS
266100990730     C                   movel     '1P'          §COD                           *CODICE TABELLA
266200990730     C                   movel     *BLANKS       §KEY
266300990730     C                   movel     V1CFTC        §KEY                           *CODICE ELEMENTO
266400990730     C                   EXSR      CHKTAB
266500990730     C                   movel     TBLUNI        DS1P
266600990730     C                   movel     §KEY          V1CFTC
266700990730     C                   movel     §DES          V1DFTC                         *DESCRIZIONE
266800990730      *
266900990730     C     $ERR          IFEQ      '1'
267000990730     C     §1PUAR        ORNE      'S'
267100990730     C     §1PUPR        ANDNE     'S'
267200990730      *
267300990730     C                   if        *IN91 = *off
267400990730     C                   eval      $err = '1'
267500990730     C                   movel     MSG(14)       $MSG
267600990730     C                   EXSR      CARMSG
267700990730     C                   eval      *in75 = *on
267800990730     C                   ENDIF
267900990730      *
268000990730     C                   ENDIF
268100150414      * Controllo se consegna particolare "Z"-EXPO
268200151120    1c**                 if        $err = '0'  and v1cftc='Z'
268300151120     c**                 exsr      sr_chkexpo
268400151120     C**                 if        $err = '1'
268500151120     C**                 eval      *in75 = *on
268600151120     c**                 endif
268700151120    1c**                 endif
268800990730     C                   ENDIF
268900990730     C                   ENDIF                                                  $ERR = '0'
269000990730      *--------------------
269100990730      * C O N S E G N A    P A R T I C O L A R E    (2)
269200990730      *--------------------
269300990730     c                   IF        $ERR = '0'
269400990730     C                   movel     *BLANKS       V1DTC2
269500990730     C     V1CTC2        IFNE      *BLANKS
269600990730     C                   movel     '1P'          §COD                           *CODICE TABELLA
269700990730     C                   movel     *BLANKS       §KEY
269800990730     C                   movel     V1CTC2        §KEY                           *CODICE ELEMENTO
269900990730     C                   EXSR      CHKTAB
270000990730     C                   movel     TBLUNI        DS1P
270100990730     C                   movel     §KEY          V1CTC2
270200990730     C                   movel     §DES          V1DTC2                         *DESCRIZIONE
270300990730     C     $ERR          IFEQ      '1'
270400990730     C     §1PUAR        ORNE      'S'
270500990730     C     §1PUPR        ANDNE     'S'
270600990730      *
270700990730     C                   if        *IN91 = *off
270800990730     C                   movel     '1'           $ERR
270900990730     C                   movel     MSG(14)       $MSG
271000990730     C                   EXSR      CARMSG
271100990730     C                   eval      *in76 = *on
271200990730     C                   ENDIF
271300990730      *
271400990730     C                   ENDIF
271500150413      * Controllo se consegna particolare "Z"-EXPO e spedizione con C/A
271600151120     c***                if        $err = '0'  and  v1ctc2='Z'
271700151120     c***                exsr      sr_chkexpo
271800151120     C***                if        $err = '1'
271900151120     C***                eval      *in76 = *on
272000151120     c***                endif
272100151120     c***                endif
272200990730     C                   ENDIF
272300990730     C                   ENDIF                                                  $ERR = '0'
272400990730      * LE DUE CONSEGNE DEVONO ESSERE DIVERSE
272500990730     C     $ERR          IFEQ      '0'
272600990730     C     V1CTC2        IFNE      *BLANKS
272700990730     C     V1CTC2        ANDEQ     V1CFTC
272800990730     C                   movel     '1'           $ERR
272900990730     C                   movel     MSG(31)       $MSG
273000990730     C                   EXSR      CARMSG
273100990730     C                   eval      *in75 = *on
273200990730     C                   eval      *in76 = *on
273300990730     C                   ENDIF
273400990730     C                   ENDIF                                                  $ERR = '0'
273500020731     C* Non è possibile indicare contemporaneamente consegne particolari
273600020731     C*  'S' ed 'X'
273700020731     C                   IF            $ERR = *OFF
273800020731     C                             and (V1CFTC = 'X'  and  V1CTC2 = 'S')
273900020731     C                             or  (V1CFTC = 'S'  and  V1CTC2 = 'X')
274000020731     C                   movel     '1'           $ERR
274100020731     C                   movel     MSG(61)       $MSG
274200020731     C                   EXSR      CARMSG
274300020731     C                   eval      *in75 = *on
274400020731     C                   eval      *in76 = *on
274500020731     C                   ENDIF
274600990730      *--------------------
274700990730      * SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
274800990730      *--------------------
274900990730     C     $PUNTO        IFEQ      '1'
275000990730     C                   movel     '1'           $ERR
275100990730     C                   move      '1'           $TESVD                         *RIEMETTI TESTATA
275200990730     C                   ENDIF
275300990730      *
275400120703     c                   if        $err='1'
275500120703     c                   setoff                                       45
275600120703     c                   else
275700120703     c* pos.curs. su codice tariffa se utente con autorizzazione <> "AZ"
275800120703     c                   if        *in22 and not *in85
275900120703     c                   eval      *in45='1'
276000120703     c                   endif
276100120703     c                   endif
276200120703     c*
276300990730     C                   ENDSR
276400011023      *------------------------------------------------------------------------*
276500011023      *  DECOD  - Imposta dei dati di dft      2' FORMATO
276600011023      *------------------------------------------------------------------------*
276700011023     C     DECOD         BEGSR
276800011023      *
276900011023      *--------------------
277000011023      * D I V I S A   I M P O R T O    D A    A S S I C U R A R E
277100011023      *--------------------
277200011023      * Se l'importo da assicurare è a 0 e non è stata impostata la divisa
277300011023     c                   if        v2cias = *zeros and v2cvas = *blanks
277400011023      * prima provo ad impostare la divisa della nazione del mittente
277500011023      * se è estero
277600011023     c                   if        wmitie = 'E' and wvlt <> *blanks
277700011023     c                   eval      v2cvas = wvlt
277800011023     c                   else
277900011023      * altrimenti imposto la §GEDCN
278000011023     c                   eval      v2cvas = §gedcn
278100011023     c                   endif
278200011023     c                   endif
278300011023      * se l'importo da assicurare non è a 0 e la divisa non è impostata
278400011023      * devo mettere fisso ITL xchè si tratta di record molto vecchi
278500011023     c                   if        v2cias <> *zeros and v2cvas = *blanks
278600011023     c                   eval      v2cvas = 'ITL'
278700011023     c                   endif
278800011023      *--------------------
278900011023      * D I V I S A   T A S S A Z I O N E
279000011023      *--------------------
279100011023      * se ho trovato un record su titas con porto o imponibile impostati ma
279200011023      * senza la divisa devo emettere un errore
279300011024     c                   if        wflgdiv = '1' and *in91 = *off
279400011023     c                   eval      $err = '1'
279500011023     c                   movel     msg(49)       $msg
279600011023     c                   eval      *in74 = *on
279700011023     c                   exsr      carmsg
279800011023     c                   endif
279900011023      *
280000011023     C                   ENDSR
280100990730      *------------------------------------------------------------------------*
280200990730      *  DECOD2 - CONTROLLA CAMPI E DECODIFICA 2' FORMATO
280300990730      *------------------------------------------------------------------------*
280400990730     C     DECOD2        BEGSR
280500990730      *
280600990730      * REIMPOSTO INDICATORI DI ERRORE
280700991102     C                   SETOFF                                       606465
280800991102     C                   SETOFF                                       666768
280900991102     C                   SETOFF                                       697071
281000991102     C                   SETOFF                                       727374
281100011129     c                   setoff                                       78
281200990730     C                   move      *BLANKS       $PUNTO                         *PUNTO INTERROGATIVO
281300990730     C   62              movel     '1'           WIN62                          *MEMORIA *IN62
281400990730     C   63              movel     '1'           WIN63                          *MEMORIA *IN63
281500990730     C   80              movel     '1'           WMODIF                         *VARIATA BOLLA
281600030626      *
281700030626     c                   clear                   wvar3a
281800040128     c                   clear                   wvar3ai
281900990730      *--------------------
282000990730      * I M P O R T O    D A    A S S I C U R A R E
282100990730      *--------------------
282200990730      * SE VALUTA MITTENTE ESTERA, E IMPORTO > 0, E DIVISA = *BLANKS VISUALIZZO DIVISA DEL MITTENTE
282300990730      * (RICORDO CHE: SE MITTENTE ESTERO NON POSSO LASCIARE  VUOTA LA DIVISA)
282400011129     c                   IF        $ERR = '0'  and
282500990730     c                             Wvlt <> *blanks  and  V2Cias > *zeros
282600990730     c                                              and  V2Cvas = *blanks
282700990730     C                   MOVEL     WVLT          V2CVAS                         *DALLA NAZIONE
282800990730     C                   MOVEL     '1'           $PUNTO                         *SIMULO '?'
282900990730     C                   ENDIF
283000990730      *
283100990730      * VERIFICO L'ESISTENZA DEL CODICE VALUTA
283200990730     c                   IF        $ERR = '0'
283300990730     C                   MOVEL     'CV'          §COD                           *CODICE TABELLA
283400990730     C                   MOVEL(P)  V2CVAS        §KEY                           *CODICE ELEMENTO
283500990730     C                   EXSR      CHKTAB
283600990730     C                   MOVEL     §KEY          V2CVAS
283700990730     c                   if        $ERR = '0'
283800990730     C                   MOVEL     TBLUNI        DSCV
283900990730     c                   else
284000990730     C                   CLEAR                   DSCV
284100990730     C                   MOVEL     MSG(28)       $MSG
284200990730     C                   EXSR      CARMSG
284300990730     C                   eval      *in65 = *on
284400990730     C                   ENDIF
284500990730     C                   ENDIF                                                  $ERR = '0'
284600990730      *
284700991103      * SE HO IMPORTO, VERIFICO SE I DECIMALI INSERITI SONO QUELLLI AMMESSI
284800991103     c                   IF        $ERR = '0'  and  V2Cias > *zeros
284900991103      *
285000991103     C                   SELECT
285100991103     C                   WHEN      §CVDEC = 3
285200991103      *
285300991103     C                   WHEN      §CVDEC = 2
285400010810     C                   move      v2cias        W0010             1 0
285500991103     C                   if         W0010 <> *ZEROS
285600991103     C                   eval         $ERR  = '1'
285700991103     C                   endif
285800991103      *
285900991103     C                   WHEN      §CVDEC = 1
286000010810     C                   move      v2cias        W0020             2 0
286100991103     C                   if         W0020 <> *ZEROS
286200991103     C                   eval         $ERR  = '1'
286300991103     C                   endif
286400991103      *
286500991103     C                   WHEN      §CVDEC = 0
286600010810     C                   move      v2cias        W0030             3 0
286700991103     C                   if         W0030 <> *ZEROS
286800991103     C                   eval         $ERR  = '1'
286900991103     C                   endif
287000991103     C                   ENDSL
287100991103      *
287200991103     c                   IF        $ERR = '1'
287300991103     C                   MOVEL     MSG(40)       $MSG
287400991103     C                   EXSR      CARMSG
287500991103     C                   eval      *in64 = *on
287600011129     c                   leavesr
287700991103     C                   ENDIF
287800011106
287900011106      * controllo che la divisa sia inserita
288000011129     c                   if        v2cvas = *blanks
288100011106     c                   eval      $ERR  = '1'
288200011106     c                   MOVEL     MSG(51)       $MSG
288300011106     c                   EXSR      CARMSG
288400011106     c                   eval      *in65 = *on
288500011129     c                   leavesr
288600011106     c                   endif
288700011106      * controllo che la divisa non sia scaduta
288800011129     c                   if        ta$dsp > §cvdli
288900011106     c                   eval      $ERR  = '1'
289000011106     c                   MOVEL     MSG(52)       $MSG
289100011106     c                   EXSR      CARMSG
289200011106     c                   eval      *in65 = *on
289300011129     c                   leavesr
289400011106     c                   endif
289500991103      *
289600991103     C                   ENDIF
289700991103      *
289800990730      * SE MODIFICO LA VALUTA DEVO MODIFICARE ANCHE L'IMPORTO (NON VICEVERSA)
289900990730      * TESTO NON L'INDICATORE "CHANGE" MA IL FLAG MEMORIA DELL'INDICATORE
290000990730      * IN QUANTO POTREI AVER RIEMESSO LA VIDEATA E PERSO I "CHANGE"
290100990730     C     $ERR          IFEQ      '0'
290200011129     c                   IF        WIN63 = '1' and WIN62 <> '1'
290300990730     C                   MOVEL     '1'           $ERR
290400990730     C                   MOVEL     MSG(27)       $MSG
290500990730     C                   EXSR      CARMSG
290600990730     C                   eval      *in64 = *on
290700990730     C                   eval      *in65 = *on
290800990730      *
290900990730     C                   ELSE
291000990730     C                   CLEAR                   WIN62                          MEMORIA *IN62
291100990730     C                   CLEAR                   WIN63                          MEMORIA *IN63
291200990730     C                   ENDIF
291300990730     C                   ENDIF                                                  $ERR = '0'
291400011129
291500011129     c                   if        $err = '1'
291600011129     c                   leavesr
291700011129     c                   endif
291800011129
291900011129      * pulisco il flag x la forzatura importo da assicurare forzabile
292000011203     c                   if        v2cias <> sav_vidias
292100011129     c                   clear                   forza
292200011129     c                   endif
292300011129
292400011203  1b c                   if        v2cias > 0
292500011203     c                   eval      sav_vidias = v2cias
292600011129      * Controllo l'importo da assicurare tramite il trul22r
292700011129     c                   clear                   trul22ds
292800011129     c                   eval      i22tla = 'L'
292900011129     c                   eval      i22cbo = tascbo
293000011129     c                   eval      i22tsp = v1ctsp
293100011129     c                   eval      i22lnp = taslnp
293200011129     c                   eval      i22nzm = v1cnzm
293300011129     c                   eval      i22lna = taslna
293400011129     c                   eval      i22nzd = tasnzd
293500011205     c                   eval      i22ksc = tasksc
293600011205     c                   movel     tasctr        i22ctr
293700011203     c                   eval      i22imp = v2cias
293800011203     c                   eval      i22div = v2cvas
293900011129     c                   eval      i22ute = knmus
294000011129     c                   eval      i22pgm = vtcpgm
294100011129     c                   call      'TRUL22R'
294200011129     c                   parm                    trul22ds
294300011129      * se ho tutti i flag impostati non posso immettere l'importo
294400011129      * da assicurare
294500011129  2b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
294600011129     c                             and o22fx2 <> *blanks
294700011129     c                   eval      $err = '1'
294800011129     c                   eval      $msg = msg(53)
294900011129     c                   exsr      carmsg
295000011129     c                   eval      *in64 = *on
295100011129     c                   leavesr
295200011129  2e c                   endif
295300011203      * controllo che non sia minore del limite previsto
295400011129  2b c                   if        o22fmn <> *blanks
295500011129     c                   eval      $err = '1'
295600011129     c                   eval      $msg = msg(54)
295700011129     c                   exsr      carmsg
295800011129     c                   eval      *in64 = *on
295900011129     c                   leavesr
296000011129  2e c                   endif
296100011129      * controllo che non sia maggiore del limite previsto
296200011129      * non forzabile
296300011129  2b c                   if        o22fx2 <> *blanks
296400011129     c                   eval      $err = '1'
296500011204     c                   eval      $msg = msg(57)
296600011210     c                   eval      %subst ($msg:46:14) = (%editc(o22lx2: '4'))
296700011129     c                   exsr      carmsg
296800011129     c                   eval      *in64 = *on
296900011129     c                   leavesr
297000011129  2e c                   endif
297100011129      * controllo che non sia maggiore del limite previsto
297200011129      * forzabile
297300011129  2b c                   if        o22fx1 <> *blanks and forza = *blanks
297400011129     c                   eval      $err = '1'
297500011129     c                   eval      forza = '1'
297600011204     c                   eval      $msg = msg(56)
297700011210     c                   eval      %subst ($msg:31:14) = (%editc(o22lx1: '4'))
297800011129     c                   exsr      carmsg
297900011129     c                   eval      *in64 = *on
298000011129     c                   leavesr
298100011129  2e c                   endif
298200011129  1e c                   endif
298300990730      *--------------------
298400990730      * P A R T I C O L A R I T A'    G I A C E N Z A
298500990730      *--------------------
298600990730     C     V2IGGA        IFEQ      '?'
298700990730     C                   CLEAR                   DS7PQRS
298800990730     C                   MOVEL     'S'           DS7RIC                         'S'=RICHIAMATO
298900990730     C                   MOVEL     'V'           DS7GES                         'V'=VISUALIZZA
299000990730     C                   MOVEL     V2CGGA        DS7COD                         CODICE
299100990730     C                   MOVEL     DS7PQRS       KPJBU
299200990730     C                   CALL      'TRTB69R'                            99
299300990730     C                   PARM                    KPJBA
299400990730     C                   MOVEL     '1'           $PUNTO
299500990730     C                   clear                   V2IGGA
299600990730     C                   ENDIF
299700990730      *--------------------
299800990730      * P A R T I C O L A R I T A'    C O N S E G N A
299900990730      *--------------------
300000990730     C     V2IGMA        IFEQ      '?'
300100990730     C                   CLEAR                   DS7PQRS
300200990730     C                   MOVEL     'S'           DS7RIC                         'S'=RICHIAMATO
300300990730     C                   MOVEL     'V'           DS7GES                         'V'=VISUALIZZA
300400990730     C                   MOVEL     V2CGMA        DS7COD                         CODICE
300500990730     C                   MOVEL     DS7PQRS       KPJBU
300600990730     C                   CALL      'TRTB70R'                            99
300700990730     C                   PARM                    KPJBA
300800990730     C                   MOVEL     '1'           $PUNTO
300900990730     C                   clear                   V2IGMA
301000990730     C                   ENDIF
301100990730     C*--------------------
301200990730     C* P A R T I C O L A R I T A'    V A R I E
301300990730     C*--------------------
301400990730     C     V2IGVA        IFEQ      '?'
301500990730     C                   CLEAR                   DS7PQRS
301600990730     C                   MOVEL     'S'           DS7RIC                         'S'=RICHIAMATO
301700990730     C                   MOVEL     'V'           DS7GES                         'V'=VISUALIZZA
301800990730     C                   MOVEL     V2CGVA        DS7COD                         CODICE
301900990730     C                   MOVEL     DS7PQRS       KPJBU
302000990730     C                   CALL      'TRTB73R'                            99
302100990730     C                   PARM                    KPJBA
302200990730     C                   MOVEL     '1'           $PUNTO
302300990730     C                   clear                   V2IGVA
302400990730     C                   ENDIF
302500990730      *--------------------
302600990730      * D S    G E N E R I C A    C O N    2 0    V A R I E
302700990730      *--------------------
302800990730      * RIEMPO LA DS GENERICA CON LE 20 "VARIE" SERVIRA' IN DIVERSI PUNTI
302900990805     c                   IF        $ERR = '0'
303000990826     C                   CLEAR                   WC
303100990826     C                   CLEAR                   DTASV
303200990826     c
303300990826     c                   DO        20            WA
303400990730     C     WA            CHAIN     SB52S02
303500990826     c                   if        %found
303600990831     c
303700990831     c                   if        *in84
303800990831     c                   movel     '1'           wmodif
303900990831     c                   endif
304000990831     c
304100990826     c                   add       1             WC
304200990826     C                   MOVEL     V2CSV1        TSV(WC)                        SIGLA "VARIA"
304300990826     C                   Z-ADD     V2CVA1        TVA(WC)                        IMPORTO "VARIA"
304400990826     C                   MOVEL     V2HTEV        TEV(WC)                        ESENZIONE "VARIA"
304500030626     C* MEMORIZZO SE C'E' LA VARIA DELLA BOLLA NEL CASO DI BOLLE CON SINGOLA VAR
304600030626     C                   IF        §3ASVA <> *BLANKS AND V2CSV1 = §3ASVA
304700030626     C                   MOVEL     'S'           WVAR3A            1
304800040128      * memorizzo anche se valorizzata
304900040128     c                   If        v2cva1 > 0
305000040128     c                   Eval      wvar3ai = 'S'
305100040128     c                   EndIf
305200030626     C                   ENDIF
305300990826     c                   endif
305400990826     c                   ENDDO
305500990826      *
305600990805     c                   ENDIF
305700990730      *--------------------
305800990730      * C O N T R O L L O    L A    T A S S A Z I O N E
305900990730      *--------------------
306000991102      *
306100991103     c                   EXSR      PREPAGATO
306200991103      *
306300991103      * Per Prepagati: se Mittente non codificato o senza tariffa utilizzo
306400991103      *                  la divisa di conto.
306500991103      *                se Mittente codificato utilizzo la divisa della tariffa
306600991103      *
306700991103     C                   SELECT
306800991104     C                   WHEN      WPREPAGATO <> 'S'
306900991104     C                   WHEN      V2CDIV = *blanks
307000991103     c                   WHEN      $TKS <> 'S'  and  V2Cdiv <> §GEDCN
307100991103     C                   eval      *in74 = *on
307200991103     C                   eval      $err = '1'
307300991103     c                   eval      $MSG = msg(46)
307400991104     c                   eval      %subst($MSG:74) = §GEDCN
307500991103     c                   exsr      carmsg
307600991103     c                   WHEN      $TKS = 'S'  and  V2Cdiv <> $DIV
307700991103     C                   eval      *in74 = *on
307800991103     C                   eval      $err = '1'
307900991103     c                   eval      $MSG = msg(47)
308000991104     c                   eval      %subst($MSG:68) = $DIV
308100991103     c                   exsr      carmsg
308200991103     C                   ENDSL
308300991103      *
308400991102      * SE VARIATA LA DIVISA: LA CONTROLLO, CONVERTO IMPORTI MODIFICATI
308500991102      *                       E RIEMETTO LA VIDEATA
308600991104     C                   IF        $ERR = '0'  and  V2CDIV <> WDIV  and
308700991102     C                             V2CDIV <> *blanks  and  WDIV <> *blanks
308800991102     C                   EXSR      VARDIV
308900991102     C                   ENDIF
309000991102      *
309100991102      *
309200990730     C                   CLEAR                   FNLV16DS
309300990730      *
309400990805     c                   IF        WWFIM <> *blanks  and  $ERR = '0'
309500990730      *
309600990812     c                   if        Wprepagato = 'S'
309700990825     C                   MOVEL     'S'           D17PRP
309800990812     C                   endif
309900990825     C                   MOVEL     '0'           D17TBO
310000990730      *
310100990730      * TASFIM CONTIENE:  'C' SE DEVO SOLO CONTROLLARE
310200990730      *                   'T' SE DEVO CALCOLARE L'IMPONIBILE
310300990730     C                   MOVEL     WWFIM         D17FIM
310400990730      *
310500990730     c                   Z-ADD     v2cpor        D17por                         porto
310600990730      *
310700990730     C                   if        D17FIM = 'C'
310800990730     c                   Z-ADD     v2cimv        D17imv                         imponibile
310900990730     C                   endif
311000990730      *
311100990730      * COD.CLIENTE: 1) MITTENTE SE E' FRANCO
311200990730      *              2) MITTENTE SE IL FLAG VHCFTM='S'
311300990730      *              3) ALTRIMENTI ASSEGNATO
311400990730     c                   IF        §TBTPO = 'F'  or  VHCftm = 'S'
311500990730     C                   Z-ADD     V1CMIT        D17KSC                         MITTENTE
311600990730     C                   ELSE
311700990730     C                   Z-ADD     V1CDES        D17KSC                         DESTINATARIO
311800990730     C                   ENDIF
311900990730      *
312000990730     C                   Z-ADD     V1CLNP        D17LNP
312100990730     C                   Z-ADD     V1CLNA        D17LNA
312200990730     C                   MOVEL     V1CTBL        D17TBL
312300990730     C                   MOVEL     §TBRBL        D17RBL
312400040129     c                   Movel     TasFca        D17Fca
312500040430      * Controllo se veramente esiste il c/a (caso di c/a su bolla mamma, impostato flag su bolla
312600040430      * figlia ma la figlia è senza c/a, se lascio il flag impostato ho poi errore in fase di
312700040430      * controllo delle varie xchè richiede obbligatoria la vaira 'G')
312800040430     c                   If        TasFca <> *Blanks
312900040430     c                   Eval      ksbaas = v1caas
313000040430     c                   Eval      ksblnp = v1clnp
313100040430     c                   Eval      ksbnrs = v1cnrs
313200040430     c                   Eval      ksbnsp = v1cnsp
313300040430     c     keyCsb        Setll     Tncsb03l
313400040430     c                   If        Not %Equal(Tncsb03l)
313500040430     c                   Clear                   D17Fca
313600040430     c                   EndIf
313700040430     c                   EndIf
313800040430
313900990730     C                   Z-ADD     V2CIAS        D17IAS                         IMPORTO ASSICURARE
314000990730     C                   Z-ADD     V1CQFT        D17QFT                         QTA' FATTURABILE
314100990922     C                   movel     V2CDIV        D17DVP                         DIVISA TASSAZIONE
314200990922     C                   movel     V2CDIV        D17DIV                         DIVISA TASSAZIONE
314300990730     C                   MOVEL     V2CFEI        D17CEI                         ESENZIONE IVA
314400990730     C                   Z-ADD     V2CFIV        D17FIV                         FILIALE IVA
314500990730     C                   Z-ADD     V2CNFT        D17NFT                         NR.FATTURA
314600990730     C                   Z-ADD     V2CDFT        D17DFG                         DATA FATT.(G/M/A)
314700990730     C                   MOVEL     VHCFTM        D17FTM                         TASSAZ.TARIFFA MITT.
314800990922     C                   z-add     ta$dsp        D17dsp                         TASSAZ.TARIFFA MITT.
314900991102     C                   MOVEL     V2CDIV        D17DIV                         TASSAZ.TARIFFA MITT.
315000991102     C                   MOVEL     WDIV          D17DVP                         TASSAZ.TARIFFA MITT.
315100990730      * SE FIL.IVA DIVERSA DA QUELLA ORIGINALE
315200990730     C                   if        V2CFIV <>  Wtasfiv
315300990730     C                   MOVEL     'S'           D17FFI
315400990730     C                   endif
315500020415     c* passo se prevista na sola varia dalcodice bolla
315600020415     c                   if        §3asva<>*blanks
315700040127     c                   movel     §3asva        §d17Sva
315800020415     c                   endif
315900040127      * passo che devo controllare la seconda bolla
316000040129      * ma solo se ho variato il video
316100040127     c                   If        (V2cCbo = '5 ' or V2cCbo = 'L ') and
316200040129     c                              V1cTbl = 'A3' and wmodif = '1'
316300040127     c                   Eval      §d17Tb2 = 'S'
316400040127     c                   EndIf
316500100121     c* controllo esenzione se c'e' fattura
316600100121     c                   if        v2cnft>0
316700100121     c                   eval      §d17ctriso='S'
316800100121     c
316900100121     c                   if        §tbtpo='F'
317000100121     c                   eval      §d17iso=v1cp2m
317100100121     c                   if        §d17iso='  ' and %subst(v1cp1m:1:11)>=*zeros
317200100121     c                   eval      §d17iso='IT'
317300100121     c                   endif
317400100121     c                   endif
317500100121     c                   if        §tbtpo='A'
317600100121     c                   eval      §d17iso=v1cp2d
317700100121     c                   if        §d17iso='  ' and %subst(v1cp1d:1:11)>=*zeros
317800100121     c                   eval      §d17iso='IT'
317900100121     c                   endif
318000100121     c                   endif
318100100121
318200100121     c                   endif
318300040127
318400040127     c                   Movel     Dlv1601       D17Flo
318500990730      *
318600990730     C                   CALL      'FNLV16R'
318700990730     C                   PARM                    FNLV16DS
318800990730     C                   PARM                    DTASV
318900990730      *
319000990730     C                   MOVEL     'S'           WLV17
319100991103      *
319200991103     C                   MOVEL     d17div        V2CDIV
319300991103     C                   MOVEL     d17cei        V2CfEI
319400991103     C                   MOVEL     o17DEI        V2DfEI
319500990902     C                   z-add     d17dfg        V2CDFT
319600990902     c
319700990902     c                   z-add     1             sflrcd
319800990826     c                   eval      *in20 = *off
319900990826     c                   eval      *in21 = *off
320000990826     C                   WRITE     SB52C02
320100990826     c                   eval      *in20 = *on
320200990826     c                   eval      *in21 = *on
320300990826     C                   clear                   SFLULT
320400990826     C                   DO        20            WA
320500990826     C                   movel     TSV(WA)       V2CSV1                         SIGLA "VARIA"
320600990826     C                   z-add     TVA(WA)       w0133                          IMPORTO "VARIA"
320700990826     c                   EXSR      AGGIOSBF
320800990826     c                   enddo
320900990826      *
321000990826     c
321100990902      * GESTIONE ERRORI
321200080123     c* Se 91 acceso gli errori di tassazione non devono bloccare per cui
321300080123     c*  aggiungo in fondo al msg F6 FORZA
321400080123     c                   if        o17msg<>*blanks and *in91
321500080123     c                   eval      %subst(o17msg:69:8)=':F6forza'
321600080123     c                   endif
321700080123     c
321800990730     c                   SELECT
321900990730      *  tutto OK
322000990730     C                   WHEN      O17err = *blanks                             POSSIBILE ERRORE
322100990730      *  Se messaggio pieno e c'è errore considero solo alcuni errori
322200990730     C                   WHEN      O17msg = *blanks
322300990730      *
322400990730     C                   WHEN      O17err = '1'                                 ERRORE ESENZIONE
322500990730     c                   If        *IN91 = *OFF
322600990730     C                   eval        *in70 = *on
322700990730     C                   eval        $ERR = '1'
322800990730     c                   Endif
322900080123     c
323000990730     C                   movel     O17msg        $MSG
323100990730     C                   exsr      CARMSG
323200990730      *
323300990730     C                   WHEN      O17err = '2'                                 ERRORE NR.FATTURA
323400990730     c                   If        *IN91 = *OFF
323500990730     C                   eval        *in72 = *on
323600990730     C                   eval        $ERR = '1'
323700990730     c                   Endif
323800990730     C                   movel     O17msg        $MSG
323900990730     C                   exsr      CARMSG
324000990730      *
324100990730     C                   WHEN      O17err = '3'                                 ERRORE FIL.IVA
324200990730     c                   If        *IN91 = *OFF
324300990730     C                   eval        *in71 = *on
324400990730     C                   eval        $ERR = '1'
324500990730     c                   Endif
324600990730     C                   movel     O17msg        $MSG
324700990730     C                   exsr      CARMSG
324800990730      *
324900990730     C                   WHEN      O17err = '4'                                 ERRORE DATA FATTURA
325000990730      *   SE ERRORE FORZABILE NON BLOCCO MA SEGNALO ERRORE
325100990730     C                   if        O17erf <> 'S'  and  *in91 = *off             'S'=ERRORE FORZABILE
325200990730     C                   eval        *in73 = *on
325300990730     C                   eval        $ERR = '1'
325400990730     c                   Endif
325500990730     C                   movel     O17msg        $MSG
325600990730     C                   exsr      CARMSG
325700990730      *
325800990730     C                   WHEN      O17err = '5'                                 ERRORE IMPONIBILE
325900990730     c                   If        *IN91 = *OFF
326000990730     C                   eval        *in69 = *on
326100990730     C                   eval        $ERR = '1'
326200990730     c                   Endif
326300990730     C                   movel     O17msg        $MSG
326400990730     C                   exsr      CARMSG
326500990730      *
326600990730     C                   WHEN      O17err = '6'                                 ERRORE "VARIE"
326700990730     c                   If        *IN91 = *OFF
326800990730     C     O17erv        CHAIN     SB52S02
326900990826     c                   z-add     o17erv        sflrcd
327000990730     C                   eval        *in67 = *on
327100990730     C                   eval        *in97 = (V2HI97 = '1')
327200990730     C                   UPDATE    SB52S02
327300990730     C                   eval        $ERR = '1'
327400990730     c                   Endif
327500990730     C                   movel     O17msg        $MSG
327600990730     C                   exsr      CARMSG
327700990730      *
327800991103     C                   WHEN      O17err = '7'
327900991103     c                   If        *IN91 = *OFF
328000991103     C                   eval        *in74 = *on
328100991103     C                   eval        $ERR = '1'
328200991103     c                   Endif
328300991103     C                   movel     O17msg        $MSG
328400991103     C                   exsr      CARMSG
328500991103      *
328600991103     C                   WHEN      O17err = '8'
328700991103     c                   If        *IN91 = *OFF
328800991103     C                   eval        *in78 = *on
328900991103     C                   eval        $ERR = '1'
329000991103     c                   Endif
329100991103     C                   movel     O17msg        $MSG
329200991103     C                   exsr      CARMSG
329300991103      *
329400990730     c                   ENDSL
329500991104      *
329600991104      * MEMORIZZO GLI IMPORTI DI TASSAZIONE IN CAMPI DI COMODO PER CONFRONTI
329700991104     C                   IF          $ERR = '0'
329800991104     C                   eval      MEMPOR = V2CPOR
329900991104     C                   eval      WDIV = V2CDIV
330000991104    2C                   DO        20            WA
330100991104     C     WA            CHAIN     SB52S02
330200991104     c                   if        %found
330300991104     C                   eval       V2HVA1 = V2CVA1
330400991104     C                   UPDATE    SB52S02
330500991104     c                   endif
330600991104    2C                   ENDDO
330700991104    2C                   ENDIF
330800990730      *
330900990730      * SE ERA DA CALCOLARE, IMPOSTO IL TOT.IMPONIBILE
331000040129      * o se è una seconda bolla ed ho l'imponibile a 0
331100040129     C                   if        WWFIM = 'T'  or
331200040129     c                             ((V2cCbo = '5 ' or V2cCbo = 'L ') and
331300040129     c                               V1cTbl = 'A3' and v2cimv = 0)
331400990730     C                   Z-ADD     O17IMV        V2CIMV                         IMPONIBILE
331500990730     C                   endif
331600030626      *
331700030626      * se non ci sono errori  e bolla con singola varia valorizzata calcolo
331800030626      * il totale imponibile
331900030626      *
332000030626      * se bolla con singola varia e la varia non  c'è azzero l'imponibile
332100030626     C     §3ASVA        IFNE      *BLANKS
332200030626     C     WVAR3A        ANDNE     'S'
332300030626     C     D17FIM        ANDEQ     'C'
332400030626     C                   Z-ADD     *ZEROS        V2CIMV
332500030626     C                   ENDIF
332600990730      * LA PROSSIMA VOLTA L'IMPONIBILE E' SOLO DA CONTROLLARE ('C')
332700990730     C                   MOVEL     'C'           WWFIM
332800990730      *
332900991104      * Se prepagato devo tassare all'imponibile
333000991104     C                   IF        Wprepagato = 'S'  and  v2cdft > *zeros
333100991104     C                                               and  v2cimv = *zeros
333200991104     c                   If        *IN91 = *OFF
333300991104     C                   eval        *in69 = *on
333400991104     C                   eval        $ERR = '1'
333500991104     c                   Endif
333600991104     C                   movel     MSG(48)       $MSG
333700991104     C                   exsr      CARMSG
333800991104     c                   ENDIF
333900991104      *
334000990730     c                   ENDIF
334100990730      *--------------------
334200990730      * C A L C O L O    T O T A L E    F A T T U R A
334300990730      *--------------------
334400990730      * 1) SOLO SE SONO PASSATO DAL CONTROLLO TASSAZIONE
334500990730      * 2) SOLO SE CI SONO NUMERO E DATA FATTURA
334600990730      *
334700990730     c                   SELECT
334800990805      *
334900990805     c                   WHEN      $ERR = '1'
335000990730      *
335100990826      *    se non ho data fattura pulisco tot.fattura IVA e Bollo
335200990730     c                   WHEN      V2Cdft = *zeros
335300990826     c                   eval      *in96=*on
335400990730     C                   clear                   V2CIFT                         TOTALE FATTURA
335500990826     C                   clear                   V2Cbol                         TOTALE FATTURA
335600990826     C                   clear                   V2CIva                         TOTALE FATTURA
335601170628     C                   clear                   w_ali                          TOTALE FATTURA
335700990730     c                   IF        *in91 = *off
335800990730     c                   z-add     1             XX
335900990730     c     §$2IVA        LOOKUP    TSV(xx)                                99
336000990730     c                   If        *in99 = *on
336100990730     C                   clear                   TVA(XX)
336200990730     C                   clear                   TSV(XX)
336300990730     C                   clear                   TEV(XX)
336400990730     c                   Endif
336500990730     c                   z-add     1             XX
336600990730     c     §$2BOL        LOOKUP    TSV(xx)                                99
336700990730     c                   If        *in99 = *on
336800990730     C                   clear                   TVA(XX)
336900990730     C                   clear                   TSV(XX)
337000990730     C                   clear                   TEV(XX)
337100990730     c                   Endif
337200990730     c                   ENDIF
337300990730      *
337400990826     c                   WHEN      V2Cimv = *zeros
337500990826     c                   eval      *in96=*on
337600990826      *
337700990730     C                   OTHER
337800990826     c                   eval      *in96=*off
337900990730     C                   eval      WLV03 = 'S'
338000990730     C                   clear                   fnlv04ds
338100990730     C                   z-add     V2CIMV        D04IMV                         IMPONIBILE
338200990730     C                   movel     V2CDIV        D04DIV                         DIVISA
338300990730     C                   movel     V1CTBL        D04tbl                         TIPO BOLLA
338400990730     C                   movel     V2CFEI        D04CEI                         COD.ESENZIONE IVA
338500990805      *
338600990805     c                   IF        V2Cdft > *zeros
338700990805     C     *eur          movel     V2CDFT        DATA_EUR
338800990805     C     *iso          movel     DATA_EUR      D04DFT                         AAAA/MM/GG
338900990805     c                   ENDIF
339000990805      *
339100990812     c                   if        Wprepagato = 'S'
339200990812     C                   movel     '8'           D04SPE                         PREPAGATO
339300990812     C                   else
339400990812     C                   movel     '1'           D04SPE                         TASSAZ.1° BOLLA
339500990812     c                   endif
339600990730     C                   z-add     V2Cpor        D04POR
339700990730      *
339800990730     C                   CALL      'FNLV04R'
339900990730     C                   PARM                    FNLV04DS
340000990730     C                   PARM                    DTASV
340100990730      *
340200990730     C                   z-add     D04IFT        V2CIFT                         TOTALE FATTURA
340201170628     C                   z-add     D04ali        w_ali                          TOTALE FATTURA
340300990804      *
340400990730     c                   IF        *in91 = *off
340500030919      * Se PORTO > 0 valorizzo il porto altrimenti la prima varia che trovo
340600030919     c                   IF        V2cPor > 0
340700990730     C                   z-add     D04POR        V2CPOR                         TRASPORTO
340800991103     C                   z-add     D04POR        MEMPOR                         TRASPORTO
340900030919      *
341000030919     c                   else
341100030919      *
341200030919     c                   do        20            wa
341300030919     c     wa            chain     sb52s02
341400030919     c                   if        %found and v2cva1 > 0
341500030919     c                   add       d04por        v2cva1
341600030919     c                   update    sb52s02
341700030919     c                   leave
341800030919     c                   endif
341900030919     c                   enddo
342000030919      *
342100030919     c                   endif
342200030919
342300990826     C                   z-add     D04BOL        V2CBOL                         TRASPORTO
342400990826     C                   z-add     D04IVA        V2CIVA                         TRASPORTO
342500991103     C                   movel     D04div        V2Cdiv                         TRASPORTO
342600991103     C                   movel     D04div        wdiv                           TRASPORTO
342700990826     C                   ENDIF
342800990804      *
342900990730     C                   ENDSL
343000011129
343100011024      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
343200011024     c                   if        *in91 = *off and $err <> '1'
343300011024     c                   setoff                                       67
343400011024     c                   if        flgvrn = '1'
343500011024     c                   do        20            wa
343600011024     c     wa            chain     sb52s02
343700011024     c                   if        %found
343800011024     c                   seton                                        67
343900011024     c                   if        v2csv1 = 'N'
344000011024     c                   setoff                                       67
344100011024     c                   leave
344200011024     c                   endif
344300011024     c                   endif
344400011024     c                   enddo
344500011024     c                   if        *in67 = *on
344600011024     c     1             chain     sb52s02
344700011024     c                   if        %found
344800011024     c                   update    sb52s02
344900011024     c                   endif
345000011024     c                   eval        $err = '1'
345100011024     c                   movel     msg(50)       $msg
345200011024     c                   exsr      carmsg
345300011024     c                   endif
345400011024     c                   endif
345500011024     c                   endif
345600040128
345700040128     c                   if        $err = '1'
345800040128     c                   leavesr
345900040128     c                   endif
346000040128
346100040128      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
346200040128      * non posso avere altre varia con importo > 0
346300040128if  1c                   If        §3asva <> *Blanks
346400040128do  2c                   Do        20            wa
346500040128     c     wa            Chain     Sb52s02                            32
346600040128     c                   Eval      *In67 = *Off
346700040128     c                   Eval      $err = '0'
346800040128if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
346900040128     c                             V2cSv1 <> *Blanks and V2cVa1 > *Zeros
347000040128     c                   Eval      *In67 = *On
347100040128     c                   Eval      $err = '1'
347200040128     c                   Eval      $msg = Msg(67)
347300040128     c                   Eval      %subst($Msg:68:1) = §3asva
347400040128     c                   ExSr      CarMsg
347500040128    3c                   EndIf
347600040128if  3c                   If        wvar3a = *Blanks  and
347700040128     c                             V2cSv1 <> *Blanks and V2cVa1 > *Zeros
347800040128     c                   Eval      *In67 = *On
347900040128     c                   Eval      $err = '1'
348000040128     c                   Eval      $msg = Msg(67)
348100040128     c                   Eval      %subst($Msg:68:1) = §3asva
348200040128     c                   ExSr      CarMsg
348300040128    3c                   EndIf
348400040128     c  n32              Update    Sb52s02
348500040128    2c                   EndDo
348600130228    c
348700130228     c* Se varia non c'e' o è >0 --> il cliente non deve essere bloccato
348800130404     c***                if        (wvar3a=*blanks or wvar3ai='S') and
348900130404     C****                          kscabl<>' '
349000130404     c****               Eval      *In67 = *On
349100130404     c****               Eval      $err = '1'
349200130404     c****               Eval      $msg = Msg(83)
349300130404     c**** 1             Chain     Sb52s02                            32
349400130404     c****               ExSr      CarMsg
349500130404     c**n32              Update    Sb52s02
349600130228     c
349700130404    3c***                EndIf
349800130228
349900130404   x1c****               else
350000130228     c*
350100130228     c* se bolla normale e da pagare e cliente bloccati --> errore
350200130404    2c****               if        §tbfcb='1' and
350300130404     c****                          kscabl<>' '
350400130404     c****               Eval      *In78 = *On
350500130404     c****               Eval      $err = '1'
350600130404     c****               Eval      $msg = Msg(84)
350700130404     c****               ExSr      CarMsg
350800130404    2c****               EndIf
350900130404    1c                   EndIf
351000011129
351100011129     c                   if        $err = '1'
351200011129     c                   leavesr
351300011129     c                   endif
351400011129
351500011129      * Controllo che la somma degli importi immessi non superi il valore
351600011129      * impostato in §GEIMF
351700011129      *      prima controllo il totale imponibile
351800011129  1b c                   if        *in91 = *off
351900011129  2b c                   if        v2cimv > 0
352000011129     c                   eval      w0173 = v2cimv
352100011129      *      controllo se devo convertire il totale imponibile
352200011129  3b c                   if        v2cdiv <> §gedcn
352300011129     c                   clear                   yeurcods
352400011129     c                   eval      yecdvi = v2cdiv
352500011129     c                   eval      yecdvo = §gedcn
352600011129     c                   eval      yecimi = v2cimv
352700011129     c                   move      decsav        yecdco
352800011129     c                   call      'YEURCO'
352900011129     c                   parm                    yeurcods
353000011129  4b c                   if        yecesi = ' '
353100011129     c                   eval      w0173 = yecimo
353200011129  4e c                   endif
353300011129  3e c                   endif
353400011129      *      controllo se l'imponibile supera il limite fatturabile
353500011129  3b c                   if        w0173 > §geimf
353600011129     c                   eval      $err = '1'
353700011129     c                   eval      $msg = msg(55)
353800011205     c                   eval      %subst ($msg:59:14) = (%editc(§geimf: '4'))
353900011205     c                   eval      %subst ($msg:74:3) = §gedcn
354000011129     c                   exsr      carmsg
354100011129     c                   eval      *in69 = *on
354200011129     c                   leavesr
354300011129  3e c                   endif
354400011129  2x c                   else
354500011129      *      altrimenti controllo tutti gli altri importi
354600011129     c                   eval      tot_importi = v2cpor
354700011129      *      leggo le varie
354800011129  3b c                   do        20            wa
354900011129     c     wa            chain     sb52s02                            32
355000011129  4b c                   if        %found and v2cva1 <> 0
355100011129  5b c                   if        flgvrn = '1' and v2cva1 = 88888888
355200011129     c                             and v2csv1 = 'N'
355300011129     c                   iter
355400011129  5e c                   endif
355500030626  5b c                   if        v2cva1 = 88888888
355600030626     c                             and v2csv1 = '&'
355700030626     c                   iter
355800030626  5e c                   endif
355900011129     c                   eval      tot_importi = tot_importi + v2cva1
356000011129  4e c                   endif
356100011129  3e c                   enddo
356200011129     c                   eval      w0173 = tot_importi
356300011129      *      controllo se devo convertire il totale importi
356400011129  3b c                   if        v2cdiv <> §gedcn
356500011129     c                   clear                   yeurcods
356600011129     c                   eval      yecdvi = v2cdiv
356700011129     c                   eval      yecdvo = §gedcn
356800011129     c                   eval      yecimi = tot_importi
356900011129     c                   move      decsav        yecdco
357000011129     c                   call      'YEURCO'
357100011129     c                   parm                    yeurcods
357200011129  4b c                   if        yecesi = ' '
357300011129     c                   eval      w0173 = yecimo
357400011129  4e c                   endif
357500011129  3e c                   endif
357600011129      *      controllo se l'imponibile supera il limite fatturabile
357700011129  3b c                   if        w0173 > §geimf
357800011129     c                   eval      $err = '1'
357900011129     c                   eval      $msg = msg(55)
358000011206     c                   eval      %subst ($msg:59:14) = (%editc(§geimf: '4'))
358100011206     c                   eval      %subst ($msg:74:3) = §gedcn
358200011129     c                   exsr      carmsg
358300011129     c                   eval      *in78 = *on
358400011129     c                   leavesr
358500011129  3e c                   endif
358600011129  2e c                   endif
358700011129  1e c                   endif
358800990730      *--------------------
358900990730      * SE ESEGUITA ROUTINE DI PUNTO INTERROGATIVO SIMULO ERRORE PER RIEMETTERE
359000990730      *--------------------
359100990730     C                   IF        $PUNTO = '1'
359200990730     C                   eval      $err = '1'
359300990730     C                   eval      $tesvd = '1'
359400990730     C                   ENDIF
359500990730      *
359600990730     C                   ENDSR
359700991102      *------------------------------------------------------------------------*
359800991102      *--- CONVERTO IMPORTI VIDEATA FATTURAZIONE NON MODIFICATI ------*
359900991102      *------------------------------------------------------------------------*
360000991102     C     VARDIV        BEGSR
360100991102      *
360200991102      * CONTROLLO DIVISA
360300991102     C                   clear                   fnlv16ds
360400991102     C                   clear                   dtasv
360500991102     C                   MOVEL     'D'           D17FIM
360600991102     C                   z-add     tasdft        D17DSP
360700991102     C                   movel     v2cDIV        D17DIV
360800991102      *
360900991102     C                   CALL      'FNLV16R'
361000991102     C                   PARM                    fnLV16ds
361100991102     C                   PARM                    DTASV
361200991104      *
361300991104     C                   eval      V2CDIV = D17DIV
361400991102      *
361500991104     C                   IF        O17ERR = '7'
361600991102     C                   eval      *in74 = *on
361700991102     C                   eval      $err = '1'
361800991102     C                   if        O17MSG <> *BLANKS
361900991102     C                   movel     O17MSG        $MSG
362000991102     C                   exsr      carmsg
362100991102    2C                   endif
362200991102     C                   GOTO      ENDDIV
362300991102    2C                   ENDIF
362400991102      *
362500991102     C                   CLEAR                   WCONVE
362600991102     C                   CLEAR                   DTASV
362700991102      *
362800991102      * PORTO
362900991102     c                   if        V2Cpor > *zeros  and  V2Cpor = MEMPOR
363000991102     C                   CLEAR                   YEURCODS
363100991102     C                   MOVEL     WDIV          YECDVI
363200991102     C                   Z-ADD     V2CPOR        YECIMI
363300991102     C                   MOVEL     V2CDIV        YECDVO
363400991102     C                   if        O17FDC = 'S'
363500991102     C                   MOVEL     '3'           YECDCO
363600991102     C                   ENDIF
363700991102     C                   CALL      'YEURCO'
363800991102     C                   PARM                    YEURCODS
363900991102    4C                   if        YECESI = *blanks
364000991102     C                   Z-ADD     YECIMO        V2CPOR
364100991102     C                   MOVEL     'S'           WCONVE
364200991102    4C                   ENDIF
364300991102    3C                   ENDIF
364400991102      *
364500991102      * VARIE
364600991103     C                   DO        20            WA
364700991103     C     WA            CHAIN     sb52s02
364800991102     C                   if        V2CVA1 > *ZEROS  and  V2Cva1 = V2Hva1
364900011106      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
365000011106      * non devo convertire la varia 'N' 88888888
365100011106     c                   IF        v2cpor = *ZEROS and flgvrn = '1'
365200011106     c                             and v2csv1 = 'N' and v2cva1 = 88888888
365300011106     c                   update    sb52s02
365400011106     c                   iter
365500011106     c                   endif
365600011106
365700991102     C                   CLEAR                   yeurcods
365800991102     C                   MOVEL     WDIV          YECDVI
365900991102     C                   Z-ADD     v2CVA1        YECIMI
366000991102     C                   MOVEL     V2CDIV        YECDVO
366100991102     C                   if        O17FDC = 'S'
366200991102     C                   MOVEL     '3'           YECDCO
366300991102     C                   ENDIF
366400991102     C                   CALL      'YEURCO'
366500991102     C                   PARM                    YEURCODS
366600991102    4C                   if        YECESI = *blanks
366700991102     C                   Z-ADD     YECIMO        V2CVA1
366800991102     C                   Z-ADD     YECIMO        V2HVA1
366900991102     C                   UPDATE    sb52s02
367000991102     C                   MOVEL     'S'           WCONVE
367100991102    4C                   ENDIF
367200991102    3C                   ENDIF
367300991102     C                   z-add     WA            XX
367400991102      * CARICO SCHIERA VARIE X RICALCOLO TOT FATTURA
367500991102     C                   if        V2CIMV > *ZEROS
367600991102     C                   MOVEL     V2CSV1        TSV(XX)
367700991102     C                   Z-ADD     V2CVA1        TVA(XX)
367800991103     C                   MOVEL     V2HTEV        TEV(XX)
367900991102     C                   ENDIF
368000991102     C                   ENDDO
368100991102      *
368200991102      * SE CONVERTITI IMPORTI ED è IMPOSTATO IL TOTALE FATTURA LO RICALCOLO
368300991102     C                   if        WCONVE = 'S'
368400991102     C                   eval      $err = '1'
368500991102     C                   if         V2CIMV > *ZEROS
368600991102     C                   CLEAR                   fnlv16ds
368700991102     C                   MOVEL     'T'           D17FIM
368800991102     C                   Z-ADD     V2CPOR        D17POR
368900991102     C                   CALL      'FNLV16R'
369000991102     C                   PARM                    fnlv16ds
369100991102     C                   PARM                    DTASV
369200991102     C                   ENDIF
369300991102     C                   ENDIF
369400991102      *
369500991102     C                   if        O17ERR = *BLANKS
369600991102     C                   Z-ADD     O17IMV        V2CIMV
369700991102     C                   ELSE
369800991102     C                   CLEAR                   v2cimv
369900991102     C                   ENDIF
370000991102      *
370100991102     C                   MOVEL     V2CDIV        WDIV
370200991102     C                   Z-ADD     V2CPOR        MEMPOR
370300991102      *
370400991102     C     ENDDIV        ENDSR
370500990812      *------------------------------------------------------------------------*
370600990826      * DETERMINO SE SPEDIZIONE PREPAGATO AUTOMATICO
370700990812      *------------------------------------------------------------------------*
370800990812     C     PREPAGATO     BEGSR
370900990812      *
371000990812     c                   clear                   Wprepagato
371100990812      *
371200990812     c                   SELECT
371300990812     c                   WHEN      §TBTPO <> 'F'
371400990812     c                   WHEN      V2Cfiv = *zeros
371500170727     c***                WHEN      V2Cfiv <> §QCfiv  and  V2Cfiv <> §QCfi2  and
371600170727     c***                          V2Cfiv <> §QCfi3  and  V2Cfiv <> §QCfi4  and
371700170727     c***                          V2Cfiv <> §QCfi5  and  V2Cfiv <> §QCfi6
371701170727     c                   WHEN      V2Cfiv < 900
371800990826
371900990812     c                   eval        Wprepagato = 'S'
372000990812     c                   ENDSL
372100990812      *
372200990812     C                   ENDSR
372300990730      *------------------------------------------------------------------------*
372400990730      * CHKCLI - CONTROLLO CENTRO DI COSTO
372500990730      *          INPUT --> $KSC : CODICE CENTRO DI COSTO
372600990730      *                    $KSCD: DESCRIZIONE CENTRO DI COSTO
372700990730      *------------------------------------------------------------------------*
372800990730     C     CHKCLI        BEGSR
372900990730      *
373000990730      *
373100990730      * SE COD.CLIENTE NON GENERICO, VEDO SE '?'
373200990730     C     WQUATT        IFEQ      0
373300990730      * DESCRIZIONE AZIENDA
373400990730     C                   movel     VTDRSU        PAXDUT                         AZIENDA
373500990730     C                   Z-ADD     9             PAXSTA                         '9'=NO ANNULLATI
373600020516     C                   Z-ADD     DUTkci        $KCC                           CAPOCONTO CLIENTI
373700990730      * CODICE DA RICERCARE: SE IMMESSA LNP CERCO SOLO AL SUO INTERNO
373800990730     C                   MOVEL(P)  $KSC          ACOKSC
373900990806      *
374000990730     C                   CALL      'XALFA1'
374100990730     C                   PARM                    PAXDUT           30
374200020516     C                   PARM                    KTBkut
374300990730     C                   PARM                    $KSCD
374400990730     C                   PARM                    $KCC
374500990730     C                   PARM                    ACOKSC
374600990730     C                   PARM                    PAXSTA            1 0
374700990730     C                   PARM                    PAXFLR           90
374800990806      *
374900990806     c                   eval      $PUNTO = '1'
375000990806      *
375100990730      * NON TROVATO O PREMUTO CMD12
375200990730     C     PAXSTA        IFNE      -1
375300990730      * CODICE SELEZIONATO: IMPOSTO A VIDEO
375400990730     C                   MOVEL     ACOKSC        $KSC
375500990730     C                   ELSE
375600990730     C                   MOVEL     '1'           $ERR
375700990730     C                   MOVEL     MSG(11)       $MSG
375800990730     C                   ENDIF
375900990730     C                   ENDIF
376000990730      *
376100990730     C     $ERR          IFEQ      '0'
376200990730     C                   CLEAR                   TIBS69DS
376300990730     C                   Z-ADD     $KSC          I69KAC
376400990730     C                   Z-ADD     $KSC          I69KIN
376500990730     C                   MOVEL     KNSIF         I69SIF
376600990730     C                   exsr      CTR_ANAGRA
376700040826sel 2c                   select
376800040826w   2c                   when      O69err = '1'
376900990730     C                   MOVEL     '1'           $ERR
377000120327     C                   MOVEL     msg(11)       $MSG
377100990730     C                   MOVEL     *ALL'*'       $KSCD                          *DESCRIZIONE C.D.C.
377200130228
377300040826w   2c                   when      ACOflg = '*'
377400040826     c                   movel     '1'           $ERR
377500040826     c                   movel     Msg(11)       $MSG
377600130219     c
377700130228     c* lo controllo solo se il tipo bolla è da "pagare"
377800130228w   2c**                 when      (ACOabl = '*'  or acoabl='8')
377900130228     c**                                           and §tbfcb='1'
378000130228     c**                 movel     '1'           $ERR
378100130228     c**                 movel     Msg(11)       $MSG
378200130404w   2c                   when      ACOabl <> *blanks  and $ksc<>$ksctas
378300130404     c                   movel     '1'           $ERR
378400130404     c                   movel     Msg(11)       $MSG
378500040826x   2c                   other
378600990730     C                   MOVEL     ACORAG        $KSCD                          *DESCRIZIONE C.D.C.
378700990730      *
378800990730     C                   CLEAR                   TISI95DS
378900990730     C                   CLEAR                   FNLV13DS
379000990730     C                   MOVEL     '3'           I95TCN
379100990730     C                   MOVEL     INDCAE        I95CAP
379200990730     C                   MOVEL     INDSTA        I95NAR
379300990730     C                   MOVEL     INDPRV        I95PRV
379400990730     C                   MOVEL     INDCIT        I95LOC
379500110117     C****               MOVEL     TA$DSP        I95DAT
379600110117     C                   MOVEL     dateu         I95DAT
379700990730      * ERRORE AFFIDABILITA = 0
379800990730     C                   MOVEL     'S'           I13AF0
379900990730      * CONTROLLO CONGRUENZA CAP PROVINCIA
380000990730     C                   MOVEL     'S'           I13CNG
380100990730      * CONTROLLO NON FORZABILE CITTA' CON VIARIO
380200150217     C*****              MOVEL     'S'           I13CNV
380300990730     C                   CALL      'FNLV13R'
380400990730     C                   PARM                    KPJBA
380500990730     C                   PARM                    FNLV13DS
380600990730     C                   PARM                    TISI95DS
380700990730      *
380800990730      * SE L'ERRORE E' PER NON TROVATA VERSIONE CAPPARIO E LA SPEDIZIONE
380900990730      * E' DELL'ANNO PRECEDENTE, RICONTROLLO CON UDATE
381000990730     C     O13ERR        IFNE      *BLANKS
381100990730     C     O13MSG        ANDEQ     MSG(44)
381200990730     C     TASAAS        ANDLT     DATAA
381300990730     C                   MOVEL     DATEU         I95DAT
381400990730     C                   CLEAR                   O13ERR
381500990730     C                   CLEAR                   O13MSG
381600990730     C                   CALL      'FNLV13R'
381700990730     C                   PARM                    KPJBA
381800990730     C                   PARM                    FNLV13DS
381900990730     C                   PARM                    TISI95DS
382000990730     C                   ENDIF
382100990730      * ERRORE
382200990730     C     O13ERR        IFNE      *BLANKS
382300990730     C                   MOVEL     O13MSG        W077A            77
382400990730     C* IMPOSTO IL MESSAGGIO METTENDO VICINO LA SCRITTA:
382500990730     C*   "SUL PIANO DEI CONTI"
382600990730     C                   CLEAR                   K78
382700990730     C                   MOVEA     W077A         K78
382800990730     C     ' '           CHECKR    W077A         WP                2 0
382900990730     C                   ADD       2             WP
383000990730     C     WP            IFLT      61
383100990730     C                   MOVEA     CPDC          K78(WP)
383200990730     C                   MOVEA     K78           $MSG
383300990730     C                   MOVEL     '1'           $ERR
383400990730     C                   ELSE
383500990730     C                   MOVEL     O95MSG        $MSG
383600990730     C                   MOVE      CPDC          $MSG
383700990730     C                   MOVEL     '1'           $ERR
383800990730     C                   ENDIF
383900990730     C                   Eval      *in63 = *on
384000990730      *
384100990730     C                   ELSE
384200990730     C                   MOVEL     O95CTS        MITCTS            2
384300990730     C                   ENDIF
384400130228     c* Memorizzo il blocco
384500130228     c                   movel     acoabl        Kscabl            1
384600130228
384700040826e   2c                   endsl
384800990730     C                   ENDIF
384900990730      *
385000990730     C                   ENDSR
385100990730      *****************************************************************
385200990730      *  AGGANCIO ANAGRAFICHE
385300990730      *****************************************************************
385400990730     C     CTR_ANAGRA    BEGSR
385500990730      *
385600990730     C                   clear                   DS_cnaco
385700990730     C                   clear                   DS_cnind
385800990730     C                   clear                   DS_cnclp
385900990730     C                   clear                   DS_fncls
386000990730      *
386100990730     C                   CALL      'TIBS69R'
386200990730     C                   PARM                    tibs69DS
386300990730     C                   PARM                    DS_cnaco
386400990730     C                   PARM                    DS_cnind
386500990730     C                   PARM                    DS_cnclp
386600990730     C                   PARM                    DS_fncls
386700990730      *
386800990730     C                   MOVE      '1'           WBS69
386900990730      *
387000990730     C                   ENDSR
387100990730      *------------------------------------------------------------------------*
387200990730      * CHKCTR - CONTROLLA CODICE TARIFFA
387300990730      *          INPUT --> $CTR  : CODICE TARIFFA ALFABETICO
387400990730      *          INPUT --> $KSC  : CODICE CLIENTE
387500990730      *          INPUT --> $DSP  : DATA PER RICERCARE TARIFFA
387600990730      *          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
387700990730      *------------------------------------------------------------------------*
387800990730     C     CHKCTR        BEGSR
387900990730      *
388000991103     c                   clear                   $TKS
388100991103     c                   clear                   $DIV
388200160301     c                   clear                   $prg
388300991103      *
388400990730     C     $KSC          IFGT      *ZERO
388500160229     C                   CLEAR                   FNLV59DS
388600160229     C                   MOVEL     DUTkci        ilv59KCI
388700160229     C                   MOVEL     $KSC          ilv59KSC
388800020213      *--------------------
388900020213
389000160229      * Richiamo il trul27r per sapere cosa passare il FIE
389100160229     c                   clear                   trul27ds1
389200020312     c                   eval      i27tsp = v1ctsp
389300020213     c                   z-add     $ksc          i27cli
389400020213     c                   eval      i27lna = v1clna
389500020213     c                   eval      i27lnp = v1clnp
389600021122     c                   Eval      I27Pkg = V1cPkf
389700160229     c                   call      'TRUL27R1'
389800160229     c                   parm                    trul27ds1
389900160229     c                   eval      ilv59fie = O27Fie
390000020213      *--------------------
390100990730      *
390200990730     C                   MOVEL     '1'           WLV51
390300160229     C                   Z-ADD     $DSP          ilv59DSP
390400160229     C                   MOVEL     'S'           ilv59CON
390500160229     C                   MOVEL     $CTR          ilv59CTR
390600160229     C                   CALL      'FNLV59R'
390700160229     C                   PARM                    FNLV59DS
390800990730      *
390900160229     C     olv59ERR      IFEQ      *BLANKS
391000160229     C                   MOVEL     olv59CTR      $CTR
391100160229     C                   MOVEL     olv59DFS      $CTD
391200160229     C                   MOVEL     olv59tks      $tks
391300160301     C                   MOVEL     olv59prg      $prg
391400160229     C                   MOVEL     olv59div      $div
391500990730     C                   ELSE
391600990730      *
391700160229     C     olv59msg      IFNE      *BLANKS                                      C'E' ERRORE
391800120326     c                   if        *in91 = *off and wf15=*blanks
391900160229     C                   MOVEL     olv59msg      $MSG
392000990730     C                   EXSR      CARMSG
392100990730     c                   endif
392200990730     C                   ELSE
392300160229     C                   MOVEL     olv59CTR      $CTR
392400160229     C                   MOVEL     olv59DFS      $CTD
392500160229     C                   MOVEL     olv59tks      $tks
392600160301     C                   MOVEL     olv59prg      $prg
392700160229     C                   MOVEL     olv59div      $div
392800990730     C                   ENDIF
392900990730      *
393000990730     c                   if        *in91 = *off
393100990730     C                   MOVEL     '1'           $ERR
393200990730     c                   endif
393300990730     C                   ENDIF
393400990730     C*
393500990730     C                   ENDIF
393600990730     C*
393700990730     C                   ENDSR
393800990730      *------------------------------------------------------------------------*
393900990730      * CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
394000990730      *          INPUT --> §COD: CODICE TABELLA
394100990730      *                    $KEY: CODICE ELEMENTO
394200990730      *------------------------------------------------------------------------*
394300990730     C     CHKTAB        BEGSR
394400990730      *
394500990730      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
394600990730     C     '?'           SCAN      §KEY                                   99     ON=TROVATO '?'
394700990730     C     *IN99         IFEQ      *ON                                          --- 1 -->
394800020516     C                   MOVEL     KTBkut        §KUT                           *CODICE UTENTE
394900990730     C                   MOVE      *BLANKS       §DES
395000990730     C                   CALL      'X§TABER'
395100990730     C                   PARM                    §KUT              1 0
395200990730     C                   PARM                    §COD              2
395300990730     C                   PARM                    §KEY              8
395400990730     C                   PARM                    §DES             30
395500990730     C                   MOVEL     '1'           $PUNTO            1            *RIEMETTO VIDEO
395600990730     C                   ENDIF                                                  <-- 1 ---
395700990730      *
395800990730      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
395900990730     C                   MOVEL     §COD          KTBCOD                         *CODICE TABELLA
396000990730     C                   MOVE      *BLANKS       KTBKEY
396100990730     C                   MOVEL     §KEY          KTBKEY                         *CODICE TENTATIVO
396200990730     C     KEYTAB        CHAIN     TABEL00F                           99
396300990730     C     *IN99         IFEQ      '1'                                          --- 2 -->
396400990730     C     TBLFLG        ORNE      *BLANKS
396500990730     C                   MOVEL     '1'           $ERR
396600990730     C                   MOVEL     *ALL'*'       §DES                           *DESCRIZIONE
396700990730     C                   CLEAR                   TBLUNI
396800990730     C                   ELSE                                                   <-- 2 -->
396900990730     C                   MOVEL     TBLUNI        §DES                           *DESCRIZIONE
397000990730     C                   ENDIF                                                  <-- 2 ---
397100990730      *
397200990730     C                   ENDSR
397300990730      *------------------------------------------------------------------------*
397400990730      *  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
397500990730      *------------------------------------------------------------------------*
397600990730     C     RECVID        BEGSR
397700011023      *
397800011023     c                   clear                   flgvrn
397900061107     c                   eval      *in90 = *on
398000061107     c                   eval      *in93 = *on
398100061114     c                   eval      *in85 = *off
398200120702     c                   eval      *in45 = *off
398300990730      *
398400990730      * Segnalo presenza C/Assegno
398500990730     C                   clear                   V1CFCA
398600990730     C                   IF        TASFCA <>  *BLANKS
398700990730     C                   eval      V1Cfca = 'CONTRASSEGNO'
398800990730     C                   ENDIF
398900990730      * stato bolla
399000990730     c                   SELECT
399100990730     c                   WHEN      TASFIC <> *blanks
399200990730     c                   eval      V1Csts = 'CONTABILIZZ.'
399300990730     c                   WHEN      TASDFT > *zeros
399400990730     c                   eval      V1Csts = 'FATTURATA   '
399500990730     c                   OTHER
399600990730     c                   eval      V1Csts = 'DA FATTURARE'
399700990730     c                   ENDSL
399800990730      *
399900990730     C                   Z-ADD     TASAAS        V1CAAS
400000990730     C                   Z-ADD     TASMGS        V1CMGS
400100990730     c                   If        TA$dsp > *zeros
400200990730     C     *iso          movel     TA$DSP        DATA_EUR                       DATA SPED. (A/M/G)
400300990730     C     *eur          movel     DATA_EUR      V1CDSP                         GG/MM/AAAA
400400990730     c                   Endif
400500990730     C                   Z-ADD     D01LNP        V1CLNP                         LNP
400600990730     C                   Z-ADD     D01NRS        V1CNRS                         SERIE
400700990730     C                   Z-ADD     D01NSP        V1CNSP                         SPEDIZIONE
400800990730     C                   Z-ADD     TASLNA        V1CLNA
400900990826     C     V1CLNA        CHAIN     AZORG01L
401000990826     c                   if        %found  and  ORGfva = *blanks
401100990826     C                   MOVEL     ORGDES        V1DLNA
401200021122     c                   Movel     Orgde3        Og143
401300021122     c                   Movel     §OgNtw        LnaNtw
401400990826     c                   else
401500990826     C                   MOVEL     *ALL'*'       V1DLNA
401600021122     c                   Clear                   Og143
401700021122     c                   Clear                   LnaNtw
401800990826     C                   endif
401900990730      * tipo bolla
402000990730      * lo memorizzo per confronti successivi
402100990730     C                   MOVEL     D01TBL        V1CTBL                         TIPO BOLLA
402200990730     C                   MOVEL     TASTBL        Wtastbl                        TIPO BOLLA ORIGINALE
402300990730     C                   MOVEL     'TB'          KTBCOD                         *CODICE TABELLA
402400990730     C                   MOVEL(P)  Wtastbl       KTBKEY                         *CODICE TENTATIVO
402500990730     C     KEYTAB        CHAIN     TABEL00F
402600990730     c                   if        %found
402700990730     C                   MOVEL     TBLUNI        DSTB
402800990730     C                   MOVEL     §TBTPO        W§tbtpo                        *FRANCO/ASSEGNATO
402900990730     C                   MOVEL     §TBRBL        W§tbrbl                        *RAGGR.BOLLE
403000120706     C                   MOVEL     §TBFCB        W§tbfcb
403100120705     c* mi salvo i tipi bolla in cui è modificabile
403200120705     c                   movel     §tbmod1       W§tbmod1
403300120705     c                   movel     §tbmod2       W§tbmod2
403400120705     c                   movel     §tbmod3       W§tbmod3
403500120706     c                   movel     §tbmod4       W§tbmod4
403600990730     c                   endif
403700020415     c* Memorizzo se il tipo bolla prevede la tassaz. di una certa varia
403800020415     C                   MOVEL     '3A'          KTBCOD                         *CODICE TABELLA
403900020415     C                   MOVEL(P)  tascbo        KTBKEY                         *CODICE TENTATIVO
404000020415     C     KEYTAB        CHAIN     TABEL00F
404100020415     c                   if        %found
404200020415     C                   MOVEL     TBLUNI        DS3A
404300020415     c                   else
404400020415     c                   clear                   DS3A
404500020415     c                   endif
404600120706     c* verifico che tipo di cod trattamento merce c'e' per eventuali controlli
404700120706     C                   MOVEL     '1B'          KTBCOD                         *CODICE TABELLA
404800120706     C                   MOVEL(P)  tasctm        KTBKEY                         *CODICE TENTATIVO
404900120706     C     KEYTAB        CHAIN     TABEL00F
405000120706     c                   if        %found
405100120706     C                   MOVEL     TBLUNI        DS1b
405200120706     c                   else
405300120706     c                   clear                   DS1b
405400120706     c                   endif
405500120706     c
405600020415      * DDT SI/NO
405700990730     C                   If        TASLL1 = 'C'  or  TASLL1 = 'S'
405800990730     C                                           or  TASLL1 = *blanks
405900990730     C                   MOVEL     'SI DDT'      V1DDDT
406000990730     C                   ELSE
406100990730     C                   MOVEL     'NO DDT'      V1DDDT
406200990730     C                   ENDIF
406300990730      *
406400990730     C                   MOVEL     TASsop        V1Csop
406500990730     C                   MOVEL     TASTSP        V1CTSP
406600990730     C                   Z-ADD     TASNCL        V1CNCL
406700990730     C                   Z-ADD     TASPVL        V1CPVL
406800990730     C                   MOVEL     TASCTR        V1CCTR
406900990730     C                   Z-ADD     TASQFT        V1CQFT
407000030729
407100030703     c                   Clear                   V1cBan
407200030729     c                   Clear                   V1cCor
407300030729     c                   Eval      *In03 = *Off
407400030703      * Bancali - part.varie "B"
407500030703     c                   If        %subst(TasGva:1:1) = 'B'
407600030204     c                   Clear                   dAr5Ban
407700030703     c                   Eval      kAr5Trd = 'BAN'
407800150929     c     kFiar5        Chain(n)  Fiar531c
407900040531If  1c                   If        %Found(Fiar531c)
408000030204     c                   Movel     Ar5Uni        dAr5Ban
408100030729     c     §Ar5Nba       Add       §Ar5Nb2       V1cBan
408200030729    1c                   EndIf
408300030703     c                   EndIf
408400030729      * Bancali e colli originali - part.varie "O"
408500030703     c                   If        %subst(TasGva:1:1) = 'O'
408600030703     c                   Clear                   dAr5Bnb
408700030703     c                   Eval      kAr5Trd = 'BNB'
408800150929     c     kFiar5        Chain(n)  Fiar531c
408900040531If  1c                   If        %Found(Fiar531c)
409000030703     c                   Movel     Ar5Uni        dAr5Bnb
409100030729     c     §Ar5bNba      Add       §Ar5bNb2      V1cBan
409200030729     c                   Z-Add     §Ar5bCor      V1cCor
409300030729     c                   Eval      *In03 = *On
409400030729    1c                   EndIf
409500030703     c                   EndIf
409600990730      * Volume
409700990730     C                   MOVEL     TASFVB        V1CFVB
409800990730     C                   Z-ADD     TASVLB        V1CVLB
409900990730     C                   Z-ADD     TASVLC        V1CVLC
410000990730     C                   Z-ADD     TASNCR        V1CNCR
410100990730     C                   MOVEL     TASFVF        V1CFVF
410200990730     C                   Z-ADD     TASVLF        V1CVLF
410300990730     C                   Z-ADD     TASVLF        WtasVLF                        MEMORIZZO ORIGINALE
410400990730      *   Verifico se devo proteggere il volume da fatturare
410500990730     C                   if        TASFVF <> *BLANKS
410600990730     C                   MOVEL     '7T'          KTBCOD                         *CODICE TABELLA
410700990730     C                   MOVEL(P)  V1CFVF        KTBKEY                         *CODICE ELEMENTO
410800990730     C     KEYTAB        CHAIN     TABEL00F
410900990730     c                   if        %found
411000990730     C                   MOVEL     TBLUNI        DS7T
411100990730     c                   endif
411200091106     c***                eval      *in95 = (§7TVRP <> '3'  and  §7TVRA <> '3')
411300091106     c
411400091106     c*** Se volume non modificabile se in partenza ne in arrivo, proteggo
411500091106     c***  anche in sede
411600091106     c                   eval      *in95 = (§7TVRP =' '    and  §7TVRA =' '  )
411700990730     C                   ENDIF
411800990730      * Peso
411900990730     C                   movel     TASFPB        V1CFPB
412000990730     C                   Z-ADD     TASPKB        V1CPKB
412100990730     C                   Z-ADD     TASPKC        V1CPKC
412200020827     C                   Z-ADD     TASncp        V1Cncp
412300990730     C                   movel     TASFPF        V1CFPF
412400151006
412500990730     C                   Z-ADD     TASPKF        V1CPKF
412600070129
412700070129      * se utente non 'AZ' e bolla fatturata controllo se fattura di filiale
412800070129      * in questo caso do la possibilità di variariare solo il codice fiscale
412900070129     c                   if        *in22 and tasdft > *zeros and
413000170727     c***                          tasfiv <> §qcfiv and tasfiv <> §qcfi2
413001170727     c                             tasfiv < 900
413100070129     c                   eval      *in91 = *on
413200070129     c                   endif
413300070201
413400070201      * se utente non 'AZ' e bolla su titasp controllo se fattura di filiale
413500070201      * in questo caso do la possibilità di variariare solo il codice fiscale
413600070201     c                   if        *in22 and *in83 and
413700170727     c***                          tasfiv <> §qcfiv and tasfiv <> §qcfi2
413701170727     c                             tasfiv < 900
413800070201     c                   eval      *in91 = *on
413900070201     c                   endif
414000061107
414100061107      * controllo se fattura di filiale
414200061107     c     tasfiv        chain     azorg01l
414300061107     c                   if        %found(azorg01l) and orgfva = *blanks and
414400061107     c                             (orgfag = 'F' or orgfag = 'A')
414500061107      * se fattura di filiale sproteggo il campo del codice fiscale mittente e destinatario
414600061107     c                   eval      *in90 = *off
414700061107     c                   eval      *in93 = *off
414800061114     c                   eval      *in85 = *on
414900061114      * ma proteggo i dati del mittente e del destinatario
415000061107     c                   endif
415100061107      * recupero il codice fiscale del mittente e del destinatario
415200061107     c                   eval      kastrc = 'Q'
415300061108     c                   clear                   dta4q
415400061108     c                   clear                   wdta4q
415500061107     c     Keytrc        chain(n)  tita430c
415600061107     c                   if        %found
415700061108     c                   eval      dta4q = ta4not
415800061107     c                   eval      wtita4q = *on
415900061107     c                   endif
416000061108     c                   eval      wdta4q = dta4q
416100061108     c                   eval      v1cdfm = §ta4qcfm
416101160923     c                   eval      savdfm = v1cdfm
416200061108     c                   eval      v1cdfd = §ta4qcfd
416201160923     c                   eval      savdfd = v1cdfd
416300061107
416400990730      * Mittente
416500080922     c********           eval      deccap='              '
416600990730     c                   z-add     TASccm        V1Cmit
416700120802     c                   move      tasccm        savQUATT
416800020516     C                   eval      WsavMIT = V1Cmit
416900990730     c                   move      TASccm        Wccm4
417000990730      *    se non codificato aggancio TITAA
417100990730     c                   IF        Wccm4 = *zero  or  Wccm4 = 8888
417200990730     c                   eval      kastrc = 'M'
417300990730     c     Keytrc        CHAIN     TITAA30C
417400990730     c                   IF        %FOUND
417500990730     c                   eval      WTITAA = 'S'
417600990730     c                   movel     TAArsc        V1Crsm
417700990730     c                   movel     TAAind        V1Cinm
417800990730     c                   movel     TAAloc        V1Clom
417900990730     c                   movel     TAAprv        V1Cprm
418000071025      * partita IVA
418100071025     C                   MOVEL     taacpi        V1cpim
418200071025     C     'PRIVATO'     SCAN      v1cpim        posiz             2 0      30
418300071025     c* solo numeri
418400071025     c* "PRIVATO" senza iso
418500071025     c     *in30         ifeq      *off
418600071025     c     v1cp2m        andge     *zeros
418700071025     c     *in30         oreq      *on
418800071025     c     posiz         andlt     3
418900071025     C                   CLEAR                   V1cpim
419000071025     c                   movel     taacpi        v1cp1m
419100071025     c                   endif
419200990730     C                   ENDIF
419201160923     c                   eval      savpim=v1cpim
419300990730      *
419400990730     c                   ELSE
419500990730      *    se codificato aggancio anagrafico
419600990730     C                   clear                   TIBS69DS
419700990730     c                   z-add     TASccm        I69kac
419800990730     c                   z-add     TASccm        I69kin
419900990730     c                   z-add     TASccm        I69kcs
420000990730     c                   exsr      CTR_anagra
420100990730      *
420200990730     c                   movel     ACOrag        V1Crsm
420300990730     c                   movel     INDvia        V1Cinm
420400990730     c                   movel     INDcit        V1Clom
420500990730     c                   movel     INDprv        V1Cprm
420600080922     c
420700080922     c* Verifico se c'e' il mittente originale con nazione/cap diversi da
420800080922     c*  anagrafica
420900080922     c                   eval      kastrc = 'O'
421000080922     c     Keytrc        CHAIN     TITAA30C
421100080922     c                   IF        %FOUND and (tasnzm<>indsta or tascam
421200080922     c                                    <>indcae)
421300080922     c                   eval      wmitor='S'
421400080922     c* mi salvo nazionel cap e codice di tassazione
421500080922     c                   eval      savnzm=tasnzm
421600080922     c                   eval      savcam=tascam
421700080922     c                   eval      savmct=tasmct
421800080922     c*********          eval      deccap='MiOrigine:'
421900080922     c*******            seton                                        05
422000080922     c                   endif
422100061107      * se non ho caricato prima il codice fiscale lo carico ora
422200061107     c                   if        v1cdfm = *blanks
422300061107     c                   eval      v1cdfm = indcdf
422301160923     c                   eval      savdfm = indcdf
422400061107     c                   endif
422500071025      * partita IVA
422600071025     c                   if        %subst(indiva:1:2) < *zeros
422700071025     C                   eval      V1Cp2m = %subst(INDiva:1:2)
422800071025     C                   eval      V1Cp1m = %subst(INDiva:3)
422900071025     c                   else
423000071025     c                   movel     indiva        V1Cp1m
423100071025     c                   endif
423101160923     c                   eval      savpim=v1cpim
423200990730     c                   ENDIF
423300990730      *
423400990730      * Per il destinatario emetto i campi del file.
423500990730     c                   movel     TASrsd        V1Crsd
423600990730     c                   movel     TASind        V1Cind
423700990730     c                   movel     TASlod        V1Clod
423800990730     c                   movel     TASprd        V1Cprd
423900061107      * non carico più la partita IVA
424000071025     c                   movel     TAScpd        V1Cpid
424100071025     C     'PRIVATO'     SCAN      v1cpid        posiz                      30
424200071025     c* solo numeri
424300071025     c* "PRIVATO" senza iso
424400071025     c     *in30         ifeq      *off
424500071025     c     v1cp2d        andge     *zeros
424600071025     c     *in30         oreq      *on
424700071025     c     posiz         andlt     3
424800071025     C                   CLEAR                   V1cpid
424900071025     c                   movel     tascpd        v1cp1d
425000071025     c                   endif
425001160923     c                   eval      savpid=v1cpid
425100990730      *
425200990730      * Spedizione in Franco
425300990730     C                   IF        W§tbtpo = 'F'
425400990730     C                   eval      *in89 = *on                                   ON = FRANCO
425500990730     C                   eval      *in94 = *on                                  PROTEGGI DESTINATARI
425600990730     c                   eval      *in87 = (Wccm4 <> *zeros  and  Wccm4 <> 8888) ON = CODIFICATO
425700071025      * proteggo il codice fiscale e la p.iva del destinatario
425800061107     c                   eval      *in93 = *on
425900990730     c                   ELSE
426000990730     C                   eval      *in87 = *off                                 OFF=ASSEGNATO
426100990730     C                   eval      *in89 = *off                                 OFF=ASSEGNATO
426200990730     C                   eval      *in92 = *on                                  PROTEGGI MITTENTE
426300990730     C                   z-add     TASKSC        V1CDES
426400071025      * proteggo il codice fiscale e la p.iva del mittente
426500061107     c                   eval      *in90 = *on
426600990730     c                   ENDIF
426700020516     C                   eval      WsavDES = V1Cdes
426800030331     c                   eval      WsavDES9 = V1Cdes
426900990730      *  cap/nazione mittente e destinatario
427000990730     C                   MOVEL     TASCAM        V1CCAM
427100990730     C                   MOVEL     TASCAM        Wcam
427200990730     C                   MOVEL     TASNZM        V1CNZM
427300990730     C                   MOVEL     TASCAD        V1CCAD
427400990730     C                   MOVEL     TASCAD        VA2CAD
427500990730     C                   MOVEL     TASNZD        V1CNZD
427600990730      *
427700990730     C                   MOVEL     TASFAP        V1CFAP
427800990730     C                   MOVEL     TASFIN        V1CFIN
427900990730     C                   MOVEL     TASMCT        V1CMCT
428000990730     C                   MOVEL     TASCTS        V1CCTS
428100990730      *
428200990730     C                   IF        TASFDN = 'S'
428300990730     C                   eval      V1CFDN = 'SI'
428400990730     C                   ENDIF
428500990730      *
428600990730     C                   MOVEL     TASFFD        V1CFFD
428700990730     C                   MOVEL     TASFTC        V1CFTC
428800990730     C                   MOVEL     TASTC2        V1CTC2
428900990730     C                   Z-ADD     TASRMN        V1CRMN
429000990730      * Riferimento mittente alfanumerico
429100990730     c                   eval      kastrc = 'A'
429200061107     c     Keytrc        CHAIN(n)  TITA430C
429300990730     c                   If        %FOUND
429400990730     c                   movel     TA4not        V1Crma
429500990730     c                   ENDIF
429600990730      * ---------------------------------
429700990730      *    SECONDA VIDEATA
429800990730      * ---------------------------------
429900990730      *    Divisa
430000991103     c                   IF        TASdiv = *blanks  and
430100991103     c                              (TASpor > *zeros  or  TASimv > *zeros  or
430200000127     c                               TASva1 > *zeros   or  TASva2 > *zeros
430300000127     c                                                  or  TASva3 > *zeros)
430400011023      * Non emetto più la divisa fissa ITL ma emetto un errore
430500011023      * per poter emetter l'errore devo gestire un flag
430600011023      * al momento dell'emissione della videata se c'è il flag impostato
430700011023      * emetto l'errore
430800011023     c                   eval      wflgdiv = '1'
430900990730     c                   ELSE
431000990730     c                   movel     tasdiv        v2cdiv
431100990730     c                   ENDIF
431200991103     c                   movel     V2Cdiv        Wdiv
431300990730      *
431400990730     C                   MOVEL     TASFTM        VHCFTM
431500990730     C     TASFTM        IFEQ      'S'                                          TASSAZ.TARIF.MITT.
431600990730     C                   eval      V2DFTM = 'SI'
431700990730     C                   ENDIF
431800990730      *
431900990730     c                   IF        TASDCM > *zeros
432000990730     C     *iso          movel     TASDCM        DATA_EUR
432100990730     C     *eur          movel     DATA_EUR      V2CDCM
432200990730     c                   ENDIF
432300990730     C                   Z-ADD     TASHMC        V2CHMC
432400020625      * codice bolla
432500020625     c                   movel     tascbo        v2ccbo
432600020625     c                   clear                   v2dcbo
432700020625     c                   if        v2ccbo <> *blanks
432800020625     c                   movel     '3A'          §cod                           codice tabella
432900020625     c                   clear                   §key
433000020625     c                   movel     v2ccbo        §key                           codice elemento
433100020625     c                   exsr      chktab
433200020625     c                   movel     §des          v2dcbo                         descrizione
433300020625     c                   endif
433400990730      *
433500990730     C                   MOVEL     TASCCA        V2CCCA
433600990826     C                   MOVEL     *BLANKS       V2DCCA
433700990826     C     V2CCCA        IFNE      *BLANKS
433800990826     C                   MOVEL     '7O'          §COD                           *CODICE TABELLA
433900990826     C                   MOVEL     *BLANKS       §KEY
434000990826     C                   MOVEL     V2CCCA        §KEY                           *CODICE ELEMENTO
434100990826     C                   EXSR      CHKTAB
434200990826     C                   MOVEL     §DES          V2DCCA                         *DESCRIZIONE
434300990826     C                   ENDIF
434400990730      *
434500990730     C                   if        TASFGC = 'S'
434600990730     C                   eval      V2Cfgc = 'SI'
434700990730     C                   endif
434800990730      *
434900990730     c                   SELECT
435000990730     C                   WHEN      TASFBR = 'S'
435100990730     C                   eval         V2Cfbr = 'SI'
435200990730     C                   WHEN      TASFBR = 'F'
435300990730     C                   eval         V2Cffr = 'SI'
435400990730     C                   ENDSL
435500990730      *
435600990730     C                   if        TASFA2  = 'S'
435700990730     C                   eval      V2Cfa2 = 'SI'
435800990730     C                   endif
435900990730     C                   if        TASFA1  = 'S'
436000990730     C                   eval      V2Cfa1 = 'SI'
436100990730     C                   endif
436200990730      *
436300990730     C                   if        TASFDA  = 'S'
436400990730     C                   eval      V2CFDA = 'SI'
436500990730     C                   endif
436600990730      *
436700050118     c                   eval      v2cxco = tasxco
436800050118
436900990730     C                   Z-ADD     TASPOR        V2CPOR
437000991103     C                   Z-ADD     TASPOR        memPOR
437100990730      *  Importo da Assicurare
437200990730     C                   Z-ADD     TASIAS        V2CIAS
437300990730     C                   MOVEL     TASVAS        V2CVAS
437400130124     C                   Z-ADD     TASIAS        sav_tasias
437500130124     C                   MOVEL     TASVAS        sav_tasvas
437600990730      *      se inserito dalla Fatturazione
437700990730     c                   eval      *in01 = (TASfcm = 'F')
437800990730      *
437900990730     C                   Z-ADD     TASFIV        V2CFIV
438000990730     C                   Z-ADD     TASFIV        Wtasfiv                        MEMORIZZO ORIGINALE
438100990730     C                   Z-ADD     TASNFT        V2CNFT
438200990730      *
438300990730     c                   IF        TASDFT > *zeros
438400990730     C     *iso          movel     TASDFT        DATA_EUR
438500990730     C     *eur          movel     DATA_EUR      V2CDFT
438600990730     c                   ENDIF
438700990730     C                   movel     TASFEI        V2CFEI
438800990730      * Particolarità
438900990730     C                   IF        TASGGA <> *blanks
439000990730     c                   eval      *in47 = *on
439100990730     C                   movel     TASGGA        V2CGGA
439200990730     c                   ENDIF
439300990730     C                   IF        TASGMA <> *blanks
439400990730     c                   eval      *in48 = *on
439500990730     C                   movel     TASGMA        V2CGMA
439600990730     c                   ENDIF
439700990730     C                   IF        TASGVA <> *blanks
439800990730     c                   eval      *in49 = *on
439900990730     C                   movel     TASGVA        V2CGVA
440000990730     C                   move      ' '           V2CGVA
440100990730     c                   ENDIF
440200990730      *
440300990730     C                   Z-ADD     *ZERO         V2CIMV                         TOT.IMPONIBILE
440400990730      * CERCO ANCHE LE VARIE: LE PRIME TRE DIRETTAMENTE SU TITAS00F, EVENTUALI ALTRE SU TITA700F
440500990730     C                   eval      *in20 = *off
440600990730     C                   eval      *in21 = *off
440700990730     C                   WRITE     SB52C02                                      SFLCTL
440800990730     C                   eval      *in20 = *on
440900990730     C                   eval      *in21 = *on
441000990730     C                   eval      *in67 = *off                                 ERRORI SFL
441100990730     C                   eval      *in68 = *off                                 ERRORI SFL
441200011025      *
441300011025      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
441400011106     c                   if        tasfbr = 'S'
441500011025     c                   select
441600011025     c                   when      tassv1 = 'N' and tasva1 = 88888888
441700011025     c                   eval      flgvrn = '1'
441800011025     c                   when      tassv2 = 'N' and tasva2 = 88888888
441900011025     c                   eval      flgvrn = '1'
442000011025     c                   when      tassv3 = 'N' and tasva3 = 88888888
442100011025     c                   eval      flgvrn = '1'
442200011025     c                   endsl
442300011025     c                   endif
442400990730      *
442500990730      *    varia 1 TAS
442600990730     c                   IF        TASsv1 <> *blanks
442700990730     c                   eval      v2csv1 = tassv1
442800990730     c                   z-add     TASva1        W0133
442900990730     c                   exsr      AGGIOSBF
443000990730     c                   ENDIF
443100990730      *    varia 2 TAS
443200990730     c                   IF        TASsv2 <> *blanks
443300990730     c                   eval      v2csv1 = tassv2
443400990730     c                   z-add     TASva2        W0133
443500990730     c                   exsr      AGGIOSBF
443600990730     c                   ENDIF
443700990730      *    varia 3 TAS
443800990730     c                   IF        TASsv3 <> *blanks
443900990730     c                   eval      v2csv1 = tassv3
444000990730     c                   z-add     TASva3        W0133
444100990730     c                   exsr      AGGIOSBF
444200990730      *
444300990730      *  carico ulteriori varie
444400990730     C     keytas        setll     tita730c
444500990730     C     keytas        reade     tita730c
444600990730     C                   DOW       %EOF = *off  and  SFLult < 20                 massimo 20 varie
444700990730     c                   IF        TA7svn <> *blanks
444800011023      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
444900011023     c                   if        tasfbr = 'S' and ta7svn = 'N'
445000011025     c                             and ta7van = 88888888 and flgvrn <> '1'
445100011023     c                   eval      flgvrn = '1'
445200011023     c                   endif
445300990730     c                   eval      v2csv1 = ta7svn
445400990730     c                   z-add     TA7van        W0133
445500990730     c                   exsr      AGGIOSBF
445600990730     c                   ENDIF
445700990730     C     keytas        reade     tita730c
445800990730     c                   ENDDO
445900990730      *
446000990730     c                   ENDIF
446100990730      *
446200990730      * FINISCO DI RIEMPIRE IL SFL
446300990730     C                   eval      WA =  SFLULT + 1
446400990730     C     WA            DO        20            SFLULT
446500990730     C                   clear                   V2CSV1
446600990730     C                   clear                   V2CVA1
446700990730     C                   clear                   V2DSV1
446800990730     C                   clear                   V2HTEV
446900990730     C                   clear                   V2HI97
447000990730     C                   WRITE     SB52S02                                      SFL
447100990730     C                   ENDDO
447200990826     c
447300990826     c                   z-add     1             sflrcd
447400990730      *
447500990730      * MEMORIZZO FLAG CONTABILIZZAZIONE
447600990730     C                   MOVEL     TASFIC        Wtasfic                        INPUT.CONTABILE
447700990730      *
447800990730      * SE NON C'E' FLAG TASSAZIONE NON VISUALIZZO IL TOT FATTURA
447900990730     C                   eval      *in96 = (TASfts = *blanks)                    ON=NON VISUALIZZA
448000990730      *
448100990730     C                   z-add     TASimv        V2Cimv
448200151019     C                   z-add     TASimv        sav_tasimv
448300990730     C                   eval      WWfim = 'C'
448400990730      *
448500990730     C                   MOVEL     '0'           $ERR
448600120703     c* pos.curs. su codice tariffa se utente con autorizzazione <> "AZ"
448700120703     c                   if        *in22 and not *in85
448800120703     c                   eval      *in45='1'
448900120703     c                   endif
449000990730      *
449100990730     C                   ENDSR
449200990730      *****************************************************************
449300990730      * CARICO SUBFILE VARIE
449400990730      *****************************************************************
449500990730     C     AGGIOSBF      BEGSR
449600990826     C                   clear                   V2DSV1                         DESCRIZIONE
449700990730      *
449800990826     c* Bollo e iva li imposto a parte
449900990826     c                   select
450000990826     c                   when      v2csv1= §$2bol
450100990826     c                   z-add     w0133         v2cbol
450200990826     c
450300990826     c                   when      v2csv1= §$2iva
450400990826     c                   z-add     w0133         v2civa
450500990826     c
450600990826     c                   other
450700990826
450800991104     c                   z-add     W0133         V2Cva1
450900991104     c                   z-add     W0133         V2Hva1
451000990827     c                   eval      *in97 = *Off
451100990827     c                   clear                   V2Hi97
451200990827     c                   clear                   V2Htev
451300990730      *
451400990827     c                   if        v2csv1<>' '
451500990730     C                   eval      §COD = 'CC'
451600990730     C                   movel(P)  'VARIE'       §KEY
451700990730     C                   move      V2Csv1        §KEY
451800990730     C                   EXSR      CHKTAB
451900990730     C                   movel     TBLuni        DSCC
452000020327     C                   MOVEL     §CCCEI        V2Htev
452100020327     C     §ccdfe        ifeq      'S'
452200020327      *
452300020327      * Verifico se si tratta di una bolla Fedex
452400020327      *
452500021122     c                   if        LnaNtw = 'FED'
452600020327      * recupero decodifica da tabella CB
452700020327     C                   eval      §COD = 'CB'
452800020327     C                   movel(P)  'VARIE'       §KEY
452900020327     C                   move      V2Csv1        §KEY
453000020327     C                   EXSR      CHKTAB
453100990826     c                   endif
453200020327     c                   endif
453300020327      *
453400020327     C                   movel     §des          V2Dsv1
453500020327     c                   endif
453600990730      *
453700990730     C                   ADD       1             SFLULT
453800990730     C                   WRITE     SB52S02                                      SFL
453900990826     c                   endsl
454000990730      *
454100990730     c                   ENDSR
454200990730      *------------------------------------------------------------------------*
454300990730      *  MOVVF - MUOVE I CAMPI DEL VIDEO AL FILE
454400990730      *------------------------------------------------------------------------*
454500990730     C     MOVVF         BEGSR
454600071105     c*
454700071105     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
454800071105     C*     NELLA P.IVA
454900071105     C                   if        v1cp2m='$$' and v1cp1m=*blanks
455000071105     C                   MOVEL     'PRIVATO'     v1cp1m
455100071105     C                   endif
455200071105     C* SE CODICE ISO $$ E VUOTA LA P.IVA --> METTO LA SCRITTA PRIVATO
455300071105     C*     NELLA P.IVA
455400071105     C                   if        v1cp2d='$$' and v1cp1d=*blanks
455500071105     C                   MOVEL     'PRIVATO'     v1cp1d
455600071105     C                   endif
455700990730      *
455800990730      * MUOVO SOLO I CAMPI CHE POTREBBERO ESSERE VARIATI
455900990730     C                   MOVEL     V1CTBL        TASTBL                         TIPO BOLLA
456000990730     C                   MOVEL     V1CTSP        TASTSP
456100990730     C                   MOVEL     V1CCTR        TASCTR
456200990730     C                   Z-ADD     V1CQFT        TASQFT
456300990730     C                   MOVEL     V1CFVF        TASFVF
456400990730     C                   Z-ADD     V1CVLF        TASVLF
456500990730     C                   Z-ADD     V1CPKF        TASPKF
456600990730     C                   Z-ADD     V1CPVL        TASPVL
456700990730     C                   MOVEL     V1CFVB        TASFVB
456800990730     C                   Z-ADD     V1CVLB        TASVLB
456900061107
457000061107      * codice fiscale memorizzo solo se manutenzionabile
457100061107     c                   if        not *in90 or not *in93
457200061107     c                   eval      kastrc = 'Q'
457300061108     c                   clear                   dta4q
457400061107     c     Keytrc        chain     tita430c
457500061108     c                   eval      dta4q = ta4not
457600061108     c                   eval      §ta4qcfm = v1cdfm
457700061108     c                   eval      §ta4qcfd = v1cdfd
457800061108     c                   eval      ta4not = dta4q
457900061107     c                   if        %found(tita430c)
458000061107     c  n83              update    tita4000
458100061107     c   83              update    tita4p00
458200061107     c                   else
458300061107     c                   z-add     v1caas        ta4aas
458400061107     c                   z-add     v1clnp        ta4lnp
458500061107     c                   z-add     v1cnrs        ta4nrs
458600061107     c                   z-add     v1cnsp        ta4nsp
458700061107     c                   eval      ta4trc = 'Q'
458800061107     c  n83              write     tita4000
458900061107     c   83              write     tita4p00
459000061107     c                   endif
459100061107     c                   endif
459200990730      *
459300990730      * FRANCO
459400990730     C     §TBTPO        IFEQ      'F'                                          'F'=FRANCO
459500990730     C                   z-add     V1CMIT        TASKSC
459600990730     C                   z-add     V1CMIT        TASCCM
459700990730     C                   move      TASKSC        WQUATT                         *ULTIME QUATTRO DX
459800990730      *
459900990730     c                   SELECT
460000990730      *
460100990730     C                   WHEN      WQUATT <> 8888  and  Wtitaa = 'S'
460200990730     c                   eval      kastrc = 'M'
460300990730     c     Keytrc        delete    TITAA30C
460400990730      *
460500990730     C                   WHEN      WQUATT <> 8888
460600990730      *
460700990730     C                   OTHER
460800990730     C                   movel     V1CRSM        TAARSc
460900990730     C                   movel     V1CINM        TAAIND
461000990730     C                   movel     V1CLOM        TAALOc
461100990730     C                   movel     V1CPRM        TAAPRV
461200990730     C                   movel     V1CCAM        TAACAP
461300990730     C                   movel     V1CNZM        TAANAZ
461400071026     c* Partita iva solo se manutenzionabile
461500071026     C  n90              If        V1CP2M = *blanks
461600071026     C                   movel     V1CP1M        TAAcpi
461700071026     C                   Else
461800071026     C                   movel     V1CPIM        TAAcpi
461900071026     C                   Endif
462000130403     c  n90              if        taacpi=*blanks and %subst(v1cdfm:12:1)=' '
462100130403     c                   movel     v1cdfm        taacpi
462200130403     c                   endif
462300990730     C                   If        Wtitaa = 'S'
462400990826     c  N83              UPDATE    TITAA000
462500990826     c   83              UPDATE    TITAAP00
462600990730     C                   Else
462700990730     c                   z-add     V1Caas        TAAaas
462800990730     c                   z-add     V1Clnp        TAAlnp
462900990730     c                   z-add     V1Cnrs        TAAnrs
463000990730     c                   z-add     V1Cnsp        TAAnsp
463100990730     c                   movel     'M'           TAAtrc
463200990826     c  N83              WRITE     TITAA000
463300990826     c   83              WRITE     TITAAP00
463400990730     C                   Endif
463500990730      *
463600990730     C                   ENDSL
463700990730      *
463800990730     C                   ELSE
463900990730      *
464000990730      *  ASSEGNATO
464100990730     C                   Z-ADD     V1CDES        TASKSC
464200990730     C                   Z-ADD     V1CMIT        TASCCM
464300990730      *
464400071026     c* partita iva solo se manutenzionabile
464500071106     c                   eval      s_tascpd=tascpd
464600071026     C  n93              if        V1CP2D = *blanks
464700071026     C                   MOVEL     V1CP1D        TASCPD
464800071026     C                   else
464900071026     C                   MOVEL     V1CPID        TASCPD
465000071026     C                   endif
465100130403     c  n93              if        tascpd=*blanks and %subst(v1cdfd:12:1)=' '
465200130403     c                   movel     v1cdfd        tascpd
465300130403     c                   endif
465400071106     c                   if        s_tascpd<>tascpd
465500071106     c                   eval      wcpd2bo='1'
465600071106     c                   eval      s_tascpd=tascpd
465700071106     c                   endif
465800990730     C                   ENDIF
465900990730      *
466000990730     C                   MOVEL     V1CCAM        TASCAM
466100990730     C                   MOVEL     V1CNZM        TASNZM
466200990730     C                   MOVEL     V1CFAP        TASFAP
466300990730     C                   MOVEL     V1CFIN        TASFIN
466400990730     C                   MOVEL     V1CMCT        TASMCT
466500990730     C                   MOVEL     V1CCTS        TASCTS
466600990730     C                   MOVEL     V1CFFD        TASFFD
466700990730     C                   MOVEL     V1CFTC        TASFTC
466800990730     C                   MOVEL     V1CTC2        TASTC2
466900990730      *
467000990730      * SECONDO FORMATO VIDEO
467100990730     C                   MOVEL     V2CDIV        TASDIV
467200991103     C                   MOVEL     V2CDIV        WDIV
467300990730     C                   Z-ADD     V2CPOR        TASPOR
467400991103     C                   Z-ADD     V2CPOR        MEMPOR
467500990730     C                   Z-ADD     V2CIMV        TASIMV
467600990730      *   Importo da assicurare
467700990730     C                   If        TASias <> V2Cias  and  TASfcm <> *blanks
467800990730     c                   clear                   TASfcm
467900990730     c                   Endif
468000990730     C                   Z-ADD     V2CIAS        TASIAS
468100990730     C                   MOVEL     V2CVAS        TASVAS
468200011023      * se l'importo da assicurare è a zero pulisco la divisa sul file
468300011023     c                   if        v2cias = *zeros
468400011023     c                   clear                   tasvas
468500011023     c                   endif
468600990730      *
468700990730     C                   MOVEL     V2CFEI        TASFEI
468800990730     C                   MOVEL     V2CFIV        TASFIV
468900990730     C                   MOVEL     V2CNFT        TASNFT
469000990730     c                   If        V2Cdft > *zeros
469100990730     C     *eur          movel     V2CDFT        DATA_EUR                       DATA FATT. G/M/A
469200990730     C     *iso          movel     data_eur      TASDFT                         AAAA/MM/GG
469300990826     c                   else
469400990826     c                   clear                   tasdft
469500990730     c                   Endif
469600061114
469700061114      * se bolla contabilizzata e se tipo bolla da contab. devo aggiornare anche il file ndcpa00f
469800061114    1c                   if        §tbfcb = '1' and tasfic <> *blanks
469900061114     c                   move      tasksc        wquatt
470000061114      * solo se 8888 o 9999
470100061114    2c                   if        wquatt = 8888 or wquatt = 9999
470200061114     c                   move      tasksc        wksc
470300061114     c                   move      tasdft        wdft
470400061114     c                   move      tasnft        wnft
470500061114     c     kppa          chain     ndppa01l
470600061114    3c                   if        %found(ndppa01l)
470700061114     c     kycpa         chain     ndcpa01l
470800061114    4c                   if        %found(ndcpa01l)
470900061114     c                   if        wquatt = 8888
471000061114     c                   movel     v1cdfm        cpacdfisc
471100071026     c                   if        v1cpim<>*blanks
471200071029     c                   if        v1cp2m=*blanks
471300071029     c                   movel     v1cp1m        cpapartiva
471400071029     c                   else
471500071026     c                   movel     v1cpim        cpapartiva
471600071029     c                   endif
471700071029     c                   else
471800071029     c                   clear                   cpapartiva
471900130403     c                   if        %subst(v1cdfm:12:1)=' '
472000130403     c                   movel     v1cdfm        cpapartiva
472100130403     c                   endif
472200071026     c                   endif
472300061114     c                   endif
472400061114     c                   if        wquatt = 9999
472500061114     c                   movel     v1cdfd        cpacdfisc
472600071026     c                   if        v1cpid<>*blanks
472700071029     c                   if        v1cp2d=*blanks
472800071029     c                   movel     v1cp1d        cpapartiva
472900071029     c                   else
473000071026     c                   movel     v1cpid        cpapartiva
473100071026     c                   endif
473200071029     c                   else
473300071029     c                   clear                   cpapartiva
473400130403     c                   if        %subst(v1cdfd:12:1)=' '
473500130403     c                   movel     v1cdfd        cpapartiva
473600130403     c                   endif
473700071029     c                   endif
473800061114     c                   endif
473900061114     c                   update    ndcpa000
474000061114    4c                   endif
474100061114    3c                   endif
474200061114    2c                   endif
474300061114    1c                   endif
474400080205     c
474500080205     c* non lo faccio in titasP
474600080205     c                   if        not *in83
474700990730      *
474800990730      * Se c'è data fattura imposto il flag tassazione
474900990730     C                   IF        V2CDFT > *zeros
475000990730     C                   movel     '1'           TASFTS
475100990730     C                   ELSE
475200990730     C                   clear                   TASFTS
475300990730     C                   ENDIF
475400990730      * IMPOSTO LE VARIE
475500990730     C                   clear                   TASSV1
475600990730     C                   clear                   TASVA1
475700990730     C                   clear                   TASSV2
475800990730     C                   clear                   TASVA2
475900990730     C                   clear                   TASSV3
476000990730     C                   clear                   TASVA3
476100990730      *
476200990730      * LE PRIME TRE VARIE SONO DA METTERE IN TITAS
476300990730     C                   clear                   WB                             DI COMODO
476400990730     C                   clear                   SFLULT
476500990730      *
476600990730     C     SFLULT        DOUEQ     20                                           MAX 20 VARIE
476700990730     C     WB            OREQ      3                                            SOLO 3 IN TITAS
476800990730      *
476900990730     C                   add       1             SFLULT
477000990730     C     SFLULT        CHAIN     SB52S02
477100990730      *
477200990730     c                   IF        %found  and  V2Csv1 <> *blanks
477300990826     C                   EXSR      MOVVF7                                       MUOVE VIDEO --> FILE
477400990730     C                   clear                   V2CSV1                         PULISCO SFL
477500990730     C                   clear                   V2CVA1                         PULISCO SFL
477600990730     C                   UPDATE    SB52S02
477700990730     C                   ENDIF
477800990826      *
477900990730     C                   ENDDO
478000990730      *
478100990730      * SCRIVO LE VARIE RESTANTI  NEL FILE TITA7
478200080205      *  non lo facco se sono su titasp
478300990730     C                   z-add     V1CDSP        KASAAS                         ANNO
478400990730     C                   z-add     V1CLNP        KASLNP                         LNP
478500990730     C                   z-add     V1CNRS        KASNRS                         SERIE
478600990730     C                   z-add     V1CNSP        KASNSP                         SPEDIZIONE
478700990730     C                   movel     Wtastbl       KASTBL                         TIPO BOLLA ORIGINALE
478800990730      *
478900990730     C     KEYTAS        SETLL     TITA730C
479000070518     C     KEYTAS        READE     TITA730c                               98
479100070518     c                   dow       not %eof(tita730c)
479200070518     c                   delete    tita7000
479300070518     c
479400070518     C     KEYTAS        READE     TITA730c                               98
479500070518     c                   enddo
479600070518     c
479700990730      *
479800990730     C     4             DO        20            SFLULT
479900990730     C     SFLULT        CHAIN     SB52S02
480000990730      *
480100990730      * NEL SFL HO LA VARIA DA REGISTRARE NELL'ARCHIVIO ?
480200990730     C     V2CSV1        IFNE      *BLANKS
480300070518      * RIEMPO I CAMPI DEL FILE E SCRIVO
480400990730     C                   EXSR      MOVVF7                                       MUOVE VIDEO --> FILE
480500070518     C                   WRITE     TITA7000                                     NUOVO RECORD
480600070518     c                   endif
480700990730      *
480800990730     C                   ENDDO
480900990730      *
481000990826     c* IVA
481100990826     c                   if        v2civa>0
481200990826     c                   movel     §$2iva        v2csv1
481300990826     c                   z-add     v2civa        v2cva1
481400990826     C                   EXSR      MOVVF7                                       MUOVE VIDEO --> FILE
481500990826     c                   if        WB>3
481600990826     C                   WRITE     TITA7000                                     NUOVO RECORD
481700990826     c                   endif
481800990826     c                   endif
481900990826     c* Bollo
482000990826     c                   if        v2cbol>0
482100990826     c                   movel     §$2bol        v2csv1
482200990826     c                   z-add     v2cbol        v2cva1
482300990826     C                   EXSR      MOVVF7                                       MUOVE VIDEO --> FILE
482400990826     c                   if        WB>3
482500990826     C                   WRITE     TITA7000                                     NUOVO RECORD
482600990826     c                   endif
482700990826     c                   endif
482800990826
482900080205     c                   endif
483000070511     C*******            ENDIF
483100020214
483200990730     C                   ENDSR
483300990730     C*------------------------------------------------------------------------*
483400990730     C*  MOVVF7 - RIEMPE I CAMPI DEL FILE TITA700F DAL VIDEO
483500990730      *------------------------------------------------------------------------*
483600990730     C     MOVVF7        BEGSR
483700990826     C                   add       1             WB
483800990730      *
483900990826     c                   SELECT
484000990826      *
484100990826     C                   WHEN      WB = 1
484200990826     C                   movel     V2CSV1        TASSV1
484300990826     C                   z-add     V2CVA1        TASVA1
484400990826      *
484500990826     c                   WHEN      WB = 2
484600990826     C                   movel     V2CSV1        TASSV2
484700990826     C                   z-add     V2CVA1        TASVA2
484800990826      *
484900990826     c                   WHEN      WB = 3
485000990826     C                   movel     V2CSV1        TASSV3
485100990826     C                   z-add     V2CVA1        TASVA3
485200990826      *
485300990826     c                   other
485400990730     C                   Z-ADD     TASAAS        TA7AAS
485500990730     C                   Z-ADD     TASLNP        TA7LNP
485600990730     C                   Z-ADD     TASNRS        TA7NRS
485700990730     C                   Z-ADD     TASNSP        TA7NSP
485800990730     C                   MOVEL     V1CTBL        TA7TBL
485900990730     C                   MOVEL     V2CSV1        TA7SVN
486000990730     C                   Z-ADD     V2CVA1        TA7VAN
486100990826     C                   ENDSL
486200990730      *
486300990730     C                   ENDSR
486400990730      *------------------------------------------------------------------------*
486500990730      *  WRIVAR - SCRIVE IL RECORD ORIGINALE NEL FILE VARIAZIONI
486600990730      *------------------------------------------------------------------------*
486700990730     C     WRIVAR        BEGSR
486800990730      *
486900990730      * ho appena letto il vecchio record da TITAS e quindi ho i campi già impostati per TITASV,
487000990730      * se esiste TITAA imposto i campi relativi al mittente
487100990730     c                   IF        WTITAA = 'S'
487200990730     c                   movel     TAArsc        TASrsc
487300990730     c                   movel     TAAind        TASinm
487400990730     c                   movel     TAAloc        TASloC
487500990730     c                   movel     TAAprv        TASprV
487600990730     c                   movel     TAAnaz        TASnAZ
487700990730     c                   movel     TAAcap        TAScaP
487800061107     **c                   movel     TAACpi        TASPIV
487900990730     C                   ENDIF
488000990730      *
488100061107      * se esiste TITA4 rcd Q imposto il codice fiscale
488200061107      * solo se era possibile modificarlo
488300061107     c                   if        wtita4q = *on and (not *in90 or not *in93)
488400061108     c  n90              eval      taspiv = w§ta4qcfm
488500061108     c  n93              eval      taspiv = w§ta4qcfd
488600061107     c                   endif
488700990730      *
488800990730      * RILEVO NUOVAMENTE ORA E DATA
488900990730     C                   TIME                    WTIME                          ORA E DATA
489000990730     C                   MOVE      WTIME         DATEU                          GG/MM/AAAA
489100990730     C     *eur          movel     DATEU         DATA_EUR
489200990730     C     *iso          movel     DATA_EUR      DATEU                          AAAA/MM/GG
489300990730      *
489400990730     C                   Z-ADD     DATEU         TASDTV                         DATA VARIAZIONE
489500990730     C                   MOVEL     WTIME         TASORV                         ORA VARIAZIONE
489600990730     C                   MOVEL     KNMUS         TASPRU                         PROFILO UTENTE
489700990730     C                   WRITE     TITASV00
489800990730      *
489900990730      * LEGGO ANCHE TUTTE LE VARIE E LE TRASCRIVO
490000990730     C     KEYTAS        SETLL     TITA730C
490100990730     C     KEYTAS        READE     TITA730C
490200990730     C                   DOW       %EOF = *OFF
490300990730     C                   Z-ADD     DATEU         TA7DTV                          DATA VARIAZIONE
490400990730     C                   MOVEL     WTIME         TA7ORV                         ORA VARIAZIONE
490500990730     C                   MOVEL     KNMUS         TA7PRU                         PROFILO UTENTE
490600990730     C                   WRITE     TITA7V00
490700990730     C     KEYTAS        READE     TITA730C                               99
490800990730     C                   ENDDO
490900990730     C*
491000990730     C                   ENDSR
491001160921      *------------------------------------------------------------------------*
491002160921      *  WRIVARU - SCRIVE IL RECORD ORIGINALE NEL FILE VARIAZIONI (unico sede/fil)
491003160921      *------------------------------------------------------------------------*
491004160921     C     WRIVARU       BEGSR
491005160922     c* pulizia dei tracciati
491006160921     c                   clear                   fiarbt00
491007160922     c                   clear                   fnarbm00
491008160922     c                   clear                   fnarbg00
491009160926     c                   clear                   fnarbd00
491010160929     c                   clear                   fnarbk00
491014160921     c* Valorizzazioni campi comuni  a tutti i file di variazioni
491019160923     c                   exsr      sr_ImpoUni
491020160922     c* Codice Bolla: per man.bolle di sede metto in questo campo il primo byte del tipo
491021160922     c*               bolla
491027160922     c* FIARBT0F - variazioni di tassazione:
491029160921     c* Codice tariffa
491030160926     c                   if        %editc(tasctr:'X')<>v1cctr  or
491031160922     c* Importo da assicurare
491032161024     c**                           tasias<>v2cias or
491033161024     c**                           (tasvas<>v2cvas and tasias>0) or
491034160922     c* Importi Tassazione
491035161024     c****                         tasdiv<>v2cdiv or
491036160921     c                             taspor<>v2cpor or
491037160921     c                             tasimv<>v2cimv or
491038160922     c                             tasfei<>v2cfei or
491039160922     c                             *in84 or
491040160922     c* Quantità fatturabile
491041160922     c                             tasqft<>v1cqft or
491042160922     c* Cliente se spedizione in assegnato
491043160922     c                             not *in89 and tasksc<>v1cdes
491044161024     c* Causale Variazione Bolla
491046161024     c* Bolla Franco --> "TP"
491047161024     c                   if        *in89
491048161024     c                   eval      arbcvb='TP'
491049161024     c* Bolla in Assegnato S.F. --> "T0"
491050161024     c                   else
491051161024     c                   eval      arbcvb='T0'
491052161024     c                   endif
491056161024
491057160922     c                   exsr      sr_ImpoArbt
491083160921     c                   write     fiarbt00
491084160921     c                   endif
491085161024     c* Importo da assicurare
491088161024     c                   if        tasias<>v2cias or
491089161024     c                             (tasvas<>v2cvas and tasias>0)
491090161024     c                   eval      arbcvb='TI'
491091161024     c                   exsr      sr_ImpoArbt
491092161024     c                   write     fiarbt00
491093161024     c                   endif
491094160922     c* FNARBM0F - variazioni di mittente:
491095160922     c                   if        tasccm<>v1cmit
491096160922     c                   exsr      sr_ImpoArbm
491097160922     c                   write     fnarbm00
491098160922     c                   endif
491099160922     c* FNARBG0F - variazioni di Consegne Particolari
491100160922     c                   if        tasftc<>v1cftc or
491101160922     c                             tastc2<>v1ctc2
491102160922     c                   exsr      sr_ImpoArbg
491103160922     c                   write     fnarbg00
491104160922     c                   endif
491105160923     c* FNARBD0F - variazioni di peso/volume/fermo deposito/inoltro
491106160923     c*            Partita Iva e Codice Fiscale
491107160923     c                   if        taspkf<>v1cpkf
491108160929     c                   eval      arbcvb='IS'
491109160923     c                   exsr      sr_ImpoArbD
491110160923     c                   write     fnarbd00
491111160923     c                   endif
491112160923     c                   if        tasvlf<>v1cvlf
491113160923     c                   eval      arbcvb='I2'
491114160923     c                   exsr      sr_ImpoArbD
491115160923     c                   write     fnarbd00
491116160923     c                   endif
491117160923     c                   if        tasffd<>v1cffd   or
491118160923     c                             tasfin<>v1cfin
491119160923     c                   eval      arbcvb='I0'
491120160923     c                   exsr      sr_ImpoArbD
491121160923     c                   write     fnarbd00
491122160923     c                   endif
491123160923     c* partita iva/codice fiscale mittente
491124160923     c                   if        not*in90 and
491125160923     c                             (v1cdfm<>savdfm or v1cpim<>savpim)
491126160923     c                   clear                   fnarbd00
491127160923     c                   exsr      sr_ImpoUni
491128160923     c                   eval      arbcvb='FI'
491129160923     c                   if        %subst(savpim:1:2)='  '
491130160923     c                   clear                   arbrsd
491131160923     c                   eval      arbrsd=%subst(savpim:3:14)
491132160923     c                   else
491133160923     c                   eval      arbrsd=savpim
491134160923     c                   endif
491135160923     c                   eval      arbrd2='M'
491136160923     c                   eval      arbcpi=savdfm
491137160923     c                   write     fnarbd00
491138160923     c                   endif
491139160923     c* partita iva/codice fiscale destinatario
491140160923     c                   if        not*in93 and
491141160923     c                             (v1cdfd<>savdfd or v1cpid<>savpid)
491142160923     c                   clear                   fnarbd00
491143160923     c                   exsr      sr_ImpoUni
491144160923     c                   eval      arbcvb='FI'
491145160923     c                   if        %subst(savpid:1:2)='  '
491146160923     c                   clear                   arbrsd
491147160923     c                   eval      arbrsd=%subst(savpid:3:14)
491148160923     c                   else
491149160923     c                   eval      arbrsd=savpid
491150160923     c                   endif
491151160923     c                   eval      arbrd2='D'
491152160923     c                   eval      arbcpi=savdfd
491153160923     c                   write     fnarbd00
491154160923     c                   endif
491155160929     c* FNARBK0F - variazione di tipo Bolla:
491156160929     c                   if        tastbl<>v1ctbl
491157160929     c                   eval      arbcvb='KT'
491158160929     c                   exsr      sr_ImpoArbK
491159160929     c                   write     fnarbk00
491160170628                 if (v1ctbl = 'AP' or v1ctbl='AS') and tasnft>0;
491161170628       // se storno verifico se già memorizzato
491162170628                      setll (tasfiv:tasdft:tasnft) fnfta01l   ;
491163170628                      if not %equal(fnfta01l);
491164170628       //          memorizzo
491165170628                         exsr sr_fnfta_tas;
491166170628                      endif;
491167170628                 endif  ;
491168160929     c                   endif
491169160921     c                   endsr
491170160923      *------------------------------------------------------------------------*
491171160923      * Valorizza campi comuni a tutti i file di storico variazioni
491172160923      *------------------------------------------------------------------------*
491173160923     c     sr_ImpoUni    begsr
491174160923     c                   eval      arbaas=tasaas
491175160923     c                   eval      arblnp=taslnp
491176160923     c                   eval      arbnrs=tasnrs
491177160923     c                   eval      arbnsp=tasnsp
491178161007     c                   eval      arbcbo=%subst(tastbl:1:1)
491179161007     c                   eval      arbdtv=%dec(%date())
491180161007     c                   eval      arborv= %dec(%time())
491181161007     c                   eval      arbpru= knmus
491182161007     c                   eval      %subst(arbpru:10:1)= 'S'
491183160923     c                   endsr
491184160922      *------------------------------------------------------------------------*
491185160922      * Valorizza campi specifici storico variazioni FIARBT0F + scrittura FIARBU
491186160922      *------------------------------------------------------------------------*
491187160922     C     sr_ImpoArbt   BEGSR
491188160922
491189160922     c* ARBTRC-I o II Bolla --> in sede non c'è questa distinzione
491190160922     c*                  metto il primo byte del tipo bolla che serve
491191160922     c*                  per collegare univocamente l'estensione tassazione alla sua
491192160922     c*                  bolla
491193160922     c                   eval      arbtrc=%subst(tastbl:1:1)
491198160922
491199160922     c                   move      tasksc        arbksc
491200160922     c                   move      tasctr        arbctr
491201160922     c                   eval      ARBQFT=tasqft
491202160926     c***                eval      ARBVMD=tasvmd
491203160926     c***                eval      ARBVAD=tasvad
491204160922     c                   eval      ARBIAS=tasias
491205160922     c                   eval      ARBVAS=tasvas
491206160922     c                   eval      ARBDIV=tasdiv
491207160922     c                   eval      ARBPOR=taspor
491208160922     c                   eval      ARBSV1=tassv1
491209160922     c                   eval      ARBVA1=tasva1
491210160922     c                   eval      ARBSV2=tassv2
491211160922     c                   eval      ARBVA2=tasva2
491212160922     c                   eval      ARBSV3=tassv3
491213160922     c                   eval      ARBVA3=tasva3
491214160922     c                   eval      ARBIMV=tasIMV
491215160926     c***                eval      ARBFIM=tasFIM
491216160926     c***                eval      ARBALI=tasALI
491217160926     c                   eval      ARBCEI=tasFEI
491218160926     c****               eval      ARBIFT=tasIFT
491219160922     c                   eval      ARBDFT=tasDFT
491220160922     c                   eval      ARBNFT=tasNFT
491221160922     c                   eval      ARBFIV=tasFIV
491222160922      * LEGGO ANCHE TUTTE LE VARIE E LE TRASCRIVO
491223160922     C     KEYTAS        SETLL     TITA730C
491224160922     C     KEYTAS        READE     TITA730C
491225160922     C                   DOW       not %eof(tita730c)
491226160922     c                   eval      ARBSVN=ta7SVN
491227160922     c                   eval      ARBVAN=ta7VAN
491228160922     C                   WRITE     fiarbu00
491229160922     C     KEYTAS        READE     TITA730C
491230160922     C                   ENDDO
491231160922     c                   endsr
491232160922      *------------------------------------------------------------------------*
491233160922      * Valorizza campi specifici storico variazioni FNARBM0F
491234160922      *------------------------------------------------------------------------*
491235160922     C     sr_ImpoArbm   BEGSR
491236160922
491237160922     c* Causale Variazione Bolla
491238160922     c                   eval      arbcvb='MI'
491239160922     c                   eval      ARBCCM=tasccm
491240160922     c                   eval      ARBFAP=tasfap
491241161025     c                   eval      ARBCAM=TAscam
491242161025     c                   eval      ARBNZM=tasnzm
491243160922     c* Ragione sociale mittente: se c'è titaa la prendo da titaa, altrimenti da P.d.C.
491244160922     c                   if        wtitaa='S'
491245161025     c*                  eval      kastrc = 'M'
491246161025     c*    Keytrc        chain     TITAA30C
491247161025     c*                  if        %found(titaa30c)
491248160922     c                   eval      ARBrsm=taarsc
491249160922     c                   eval      ARBINM=TAAIND
491250161025     c                   eval      ARBCAM=TAACAP
491251160922     c                   eval      ARBLOM=TAALOC
491252160922     c                   eval      ARBPRM=TAAPRV
491253161025     c                   eval      ARBNZM=taanaz
491254160922     c                   eval      ARBcpi=taacpi
491255161025     c*                  endif
491256160922     c                   else
491257160922      *    se codificato aggancio anagrafico
491258160922     C                   clear                   TIBS69DS
491259160922     c                   z-add     TASccm        I69kac
491260160922     c                   z-add     TASccm        I69kin
491261160922     c                   z-add     TASccm        I69kcs
491262160922     c                   exsr      CTR_anagra
491263160922     c                   eval      ARBrsm=acorag
491264160922     c                   eval      ARBINM=INDVIA
491265161025     c***                eval      ARBCAM=INDCae
491266160922     c                   eval      ARBLOM=INDCit
491267160922     c                   eval      ARBPRM=INDPRV
491268161025     c***                eval      ARBNZM=INDSTA
491269160922     c                   eval      ARBcpi=indiva
491270160922     c                   endif
491271160922     c                   endsr
491272160922      *------------------------------------------------------------------------*
491273160922      * Valorizza campi specifici storico variazioni FNARBG0F
491274160922      *------------------------------------------------------------------------*
491275160922     C     sr_ImpoArbg   BEGSR
491276160922
491277160922     c* Causale Variazione Bolla
491278160922     c                   eval      arbcvb='CP'
491279160923     c                   eval      ARBTCR=tastcr
491280160923     c                   eval      ARBDCR=tasdcr
491281160923     c                   eval      ARBHCR=tashcr
491282160923     c                   eval      ARBGC1=tasgc1
491283160923     c                   eval      ARBGC2=tasgc2
491284170118     c                   eval      ARBTC1=tasftc
491285160923     c                   eval      ARBTC2=tastc2
491286160923
491287160923     c                   endsr
491288160923      *------------------------------------------------------------------------*
491289160923      * Valorizza campi specifici storico variazioni FNARBD0F
491290160923      *------------------------------------------------------------------------*
491291160923     C     sr_ImpoArbd   BEGSR
491292160923     c                   eval      ARBRSD=tasrsd
491293160923     c                   eval      ARBIND=tasind
491294160923     c                   eval      ARBCAD=tascad
491295160923     c                   eval      ARBLOD=taslod
491296160923     c                   eval      ARBPRD=tasprd
491297160923     c                   eval      ARBNZD=tasnzd
491298160923     c                   eval      ARBFIN=tasfin
491299160923     c                   eval      ARBCPI=tascpd
491300160923     c                   eval      ARBPKB=taspkb
491301160923     c                   eval      ARBPKF=taspkf
491302160923     c                   eval      ARBVLB=tasvlb
491303160923     c                   eval      ARBFVB=tasfvb
491304160923     c                   eval      ARBVLF=tasvlf
491305160923     c                   eval      ARBFVF=tasfvf
491306160923     c                   eval      ARBFFD=tasffd
491307160923     c                   endsr
491308160929      *------------------------------------------------------------------------*
491309160929      * Valorizza campi specifici storico variazioni FNARBK0F
491310160929      *------------------------------------------------------------------------*
491311160929     C     sr_ImpoArbK   BEGSR
491312160929     c                   eval      ARBCBP=tastbl
491313160929     c                   eval      ARBCBN=v1ctbl
491314160929     c                   endsr
491315170628        //-----------------------------------------------------------------------*
491316170628        // Memorizzazione dati fattura annullata/stornata
491317170628        //-----------------------------------------------------------------------*
491318170628        begsr sr_Fnfta_tas ;
491319170628        clear  fnfta000    ;
491320170628        FTAAAS = tasaas    ;
491321170628        FTALNP = taslnp    ;
491322170628        FTANRS = tasnrs    ;
491323170628        FTANSP = tasnsp    ;
491324170628        FTAMGS = tasmgs    ;
491325170628        FTADIV = tasdiv    ;
491326170628        FTAALI = W_ali     ;
491327170628        FTACEI = tasfei    ;
491328170628        ftAIFT = v2cift    ;
491329170628        FTAFIV = tasfiv    ;
491330170628        FTANFT = tasnft    ;
491331170628        FTADFT = tasdft    ;
491332170628
491333170628        write(e) fnfta000    ;
491334170628        endsr;
491335990730      *------------------------------------------------------------------------*
491336990730      * AGGCON - AGGIORNA IL CONTRASSEGNO, SE C'E'
491337990730      *------------------------------------------------------------------------*
491400990730     C     AGGCON        BEGSR
491500990730      *
491600990730     C     TASFCA        IFNE      *BLANKS
491700990730      *
491800990730     C                   Z-ADD     TASAAS        KSBAAS                         ANNO
491900990730     C                   Z-ADD     TASLNP        KSBLNP                         LNP
492000990730     C                   Z-ADD     TASNRS        KSBNRS                         SERIE
492100990730     C                   Z-ADD     TASNSP        KSBNSP                         SPEDIZIONE
492200990730     C     KEYCSB        CHAIN     TNCSB03L
492300990730     c                   IF        %found  and  CSBtbl <> TAStbl
492400990730     C                   MOVEL     TASTBL        CSBTBL
492500990730     C                   EXCEPT    AGGCSB                                       AGGIORNO RECORD
492600990730     C                   ENDIF
492700990730      *
492800990730     C                   ENDIF
492900990730      *
493000990730     C                   ENDSR
493100120402      *------------------------------------------------------------------------*
493200120402      * AGGFIL - AGGIORNA bolle di filiale mediante fnlr48r
493300120402      *------------------------------------------------------------------------*
493400120402     C     AGGfil        BEGSR
493500120404
493600120403     c                   clear                   fnlv55ds
493700120403     C                   MOVEL     'L'           D55Tla
493800120403     C                   MOVEL     'P'           D55TPT
493900120403     C                   MOVEL     taslnp        D55LIN
494000120403     C                   MOVEL     DATEU         D55DRF
494100120403     C                   CALL      'FNLV55R'
494200120403     C                   PARM                    Fnlv55DS
494300120403     c*
494400120403     c                   clear                   fnlr48ds
494500120403     C                   MOVEL     'L'           D48TLA
494600120403     C                   z-add     d55tfp        D48FGS
494700120403     C                   z-add     tasaas        D48AAS
494800120403     C                   MOVE      tasLNP        D48LNP
494900120403     C                   MOVE      tasNRS        D48NRS
495000120403     C                   MOVE      tasNSP        D48NSP
495100120404     c                   if        tasnft>0
495200120403     C                   MOVEL     'SD'          D48CVB
495300120404     c                   else
495400120404     c                   movel     'SF'          d48cvb
495500120404     c                   endif
495600120411     c* passo "D" perchè serve ma la causale "SD"/"SF" gestisce più di un tipo record
495700120403     C                   MOVEL     'D'           D48trc
495800120403     C                   MOVEL     'E'           D48FFR
495900120403     C                   MOVEL     ' '           D48ERR
496000120403     C                   MOVEL     *BLANK        D48MSG
496100120403     C                   MOVEL     fnlr48ds      KPJBU
496200120403     c* DSARBD per peso/volume
496300120403     c                   clear                   dsarbd
496400120403     c                   z-add     taspkf        §bdpkf
496500120403     c                   z-add     tasvlf        §BDVLF
496600120404     c                   movel     tasfvf        §BDFVF
496700120403     c* DARBT per campi riguardanti la tassazione
496800120403     c                   clear                   darbt
496900120411
497000120411     c                   if        wtastbl=§3atb2
497100120411     c                   eval      §bttrc='2'
497200120411     c                   else
497300120411     c                   eval      §bttrc='1'
497400120411     c                   endif
497500120403      *  Franco
497600120403     c                   if        §tbtpo='F'
497700120403     C                   z-add     V1CMIT        §btksc
497800120403     c                   else
497900120403      *  Assegnato
498000120403     C                   Z-ADD     V1CDES        §btksc
498100120403     c                   endif
498200120528     c                   z-add     tasias        §btias
498300120528     c                   movel     tasvas        §btvas
498400120403     c                   z-add     tasctr        §btctr
498500120403     c                   z-add     tasqft        §btqft
498600120403     c                   movel     tasdiv        §btdiv
498700120403     c                   z-add     taspor        §btpor
498800120403     c                   z-add     tasimv        §btimv
498900120403     c* carico le varie
499000120404     c                   z-add     tasva1        vva(1)
499100120404     c                   movel     tassv1        vsv(1)
499200120403     c                   z-add     tasva2        vva(2)
499300120404     c                   movel     tassv2        vsv(2)
499400120403     c                   z-add     tasva3        vva(3)
499500120404     c                   movel     tassv3        vsv(3)
499600120411     c                   if        tassv3<>*blanks
499700120403     c                   z-add     3             Inx
499800120403     c* le restanti le prendo da tita7
499900120403     C     keytas        setll     tita730c
500000120403     C     keytas        reade(N)  tita730c
500100120404     C                   DOW       %EOF = *off  and  Inx < 20
500200120403     c                   IF        TA7svn <> *blanks
500300120403     c                   add       1             Inx
500400120403     c                   eval      vsv(Inx)= ta7svn
500500120403     c                   z-add     TA7van        vva(Inx)
500600120403     c                   ENDIF
500700120403     C     keytas        reade(n)  tita730c
500800120403     c                   ENDDO
500900120411     c                   endif
501000120403
501100120403     c                   call      'FNLR48C'
501200120403     C                   PARM                    KPJBA
501300120403     C                   PARM                    DSARBD
501400120403     C                   PARM                    DSARBK
501500120403     C                   PARM                    DARBT
501600120403     C                   PARM                    DSARBG
501700120403     C                   MOVEL     KPJBU         fnlr48ds
501800120403     C     D48ERR        IFne      *BLANK
501900120418     c                   movel     d48msg        w02ms1
502000120418     c                   move      d48msg        w02ms2
502100120418     c                   exfmt     sb52w02
502200120403     c                   endif
502300120402     c                   endsr
502400990730     C*------------------------------------------------------------------------*
502500990730     C* LANPGM - LANCIA UN PROGRAMMA
502600990730     C*------------------------------------------------------------------------*
502700990730     C     LANPGM        BEGSR
502800990730     C*--------------------
502900990730     C* PARAMETRI INFORMATIVI
503000990730     C*--------------------
503100990730     C                   MOVEL     WOP0          D01OP0                         *LIVELLO PROCEDURA
503200990730     C                   MOVEL     WOP1          D01OP1                         *LIVELLO PROGRAMMA
503300990730     C                   MOVEL     '0'           D01F03                         *USCITA PGM
503400990730     C                   MOVEL     '0'           D01F12                         *PREMUTO F12
503500990730     C                   MOVEL     '0'           D01ERR                         *ERRORE
503600990730     C                   MOVEL     *BLANKS       D01MSG                         *MESSAGGIO ERRORE
503700990730     C*--------------------
503800990730     C* PARAMETRI
503900990730     C*--------------------
504000020328     C                   Z-ADD     WAAS          D01AAS                         *ANNO
504100990730     C                   Z-ADD     WLNP          D01LNP                         *LNP
504200020328     C                   Z-ADD     WLNA          D01LNA                         *LNP
504300990730     C                   Z-ADD     WNRS          D01NRS                         *SERIE
504400990730     C                   Z-ADD     WNSP          D01NSP                         *SPEDIZIONE
504500990730     C                   MOVEL     WTBL          D01TBL                         *TIPO BOLLA
504600990730     C                   Z-ADD     WDSP          D01DSP                         *DATA SPEDIZIONE
504700990730     C                   Z-ADD     WDTV          D01DTV                         *DATA VARIAZIONE
504800990730     C                   Z-ADD     WORV          D01ORV                         *ORA
504900990730     C                   MOVEL     WPRU          D01PRU                         *UTENTE
505000990730     C* LANCIO
505100990730     C                   MOVEL     TNSB01DS      KPJBU
505200990730     C                   CALL      WPGM                                 99
505300990730     C                   PARM                    KPJBA
505400990730     C                   MOVEL     KPJBU         TNSB01DS
505500990730     C*
505600990730     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
505700990730     C     D01F03        IFEQ      '1'
505800990730     C                   MOVEL     '1'           $F03
505900990730     C                   MOVEL     '1'           $ERR
506000990730     C                   MOVEL     '1'           $FINE
506100990730     C*
506200990730     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
506300990730     C                   ELSE
506400990730     C     $MSG          IFNE      *BLANKS
506500990730     C                   MOVEL     D01MSG        $MSG
506600990730     C                   EXSR      CARMSG
506700990730     C                   ELSE
506800990730     C                   MOVEL     *BLANKS       D01OP1
506900990730     C                   MOVEL     '0'           D01F12
507000990730     C                   ENDIF
507100990730     C                   ENDIF
507200990730     C*
507300990730     C                   ENDSR
507400990730     C*------------------------------------------------------------------------*
507500990730     C* CONTROLLO INDIRIZZO COMPLETO MITT/DESTIN ---------------------*
507600990730     C*------------------------------------------------------------------------*
507700990730     C     CTRIND        BEGSR
507800990730     C* LE DS SONO DA CLEARARE PRIMA ALTRIMENTI DANNO PROBLEMI IN CTRER
507900990730     C                   CLEAR                   FNLV13DS
508000990730     C                   CLEAR                   TISI95DS
508100990730     C*
508200990730     C                   MOVEL     TA$DSP        I14DRI
508300100121     c                   movel     'S'           i14forzait
508400151104     c
508500151104     c                   if        wquatt=8888 or wquatt=9999
508600151104     c                   eval      i14p88='8'
508700151104     c                   else
508800151104     c                   clear                   i14p88
508900151104     c                   endif
509000151104     c
509100990730     C                   CALL      'FNLV14R'
509200990730     C                   PARM                    KPJBA
509300990730     C                   PARM                    FNLV14DS
509400990730     C                   MOVEL     'S'           WLV14             1            DI COMODO
509500990730     C*
509600990730     C                   MOVEL     O14ERR        WERR              1
509700990730     C                   MOVEL     O14MSG        $MSG
509800990730     C                   EXSR      CTRERR
509900990730     C     $ERR          CABEQ     '1'           ENDIND
510000990730     C**
510100990730     C*************
510200990730     C* CONTROLLO CAP
510300000623     c*
510400000623     c     taslnp        chain     azorg01l                           30
510500000623     c  n30              movel     orgde3        og143
510600000623     c   30              clear                   og143
510700000623     c*
510800000623     c*
510900000623     C                   MOVEL     '7'           I95TCN
511000990730     C                   MOVEL     E14CAP        I95CAP
511100990730     C                   MOVEL     E14LOC        I95LOC
511200990730     C                   MOVEL     E14PRV        I95PRV
511300990730     C                   MOVEL     E14NAR        I95NAR
511400990730     C                   MOVEL     TA$DSP        I95DAT
511500000623     c* se lnp poste devo passare lnp e non tfp
511600020213     c                   if        §ogntw = 'PPT'
511700000623     C                   MOVEL     TASLNP        I95TFP
511800000623     c                   else
511900990730     C                   MOVEL     TASTFP        I95TFP
512000000623     c                   endif
512100990730     C                   MOVEL     V1CTSP        I95TSP
512200990730     C     V1CFCA        IFNE      *BLANKS
512300990730     C                   MOVEL     'S'           I95FCA
512400990730     C                   ENDIF
512500990730     C                   MOVEL     V1CTBL        I95TPO
512600990730     C                   MOVEL     'S'           I13AF0
512700990730     C                   MOVEL     'S'           I13AF1
512800990730     C                   MOVEL     'S'           I13SZ2
512900990730     C                   MOVEL     'S'           I13CNV
513000990730     C                   MOVEL     'S'           I13SZV
513100990730     C                   MOVEL     'S'           I13LA3
513200990730     C                   MOVEL     'S'           I13SZ3
513300990730     C**
513400990730     C* SE CAMBIATO CAP CONTROLLO SE HA VIARIO : ERRORE FORZABILE
513500990730     C     WCLI          IFEQ      'M'
513600990730     C**
513700990730     C     E14CAP        IFNE      VARCAM
513800990730     C                   CLEAR                   WZV
513900990730     C                   CLEAR                   WZ1
514000990730     C                   CLEAR                   WZ2
514100990730     C                   CLEAR                   WZ3
514200990730     C                   MOVEL     E14CAP        VARCAM
514300990730     C                   ENDIF
514400990730     C** MODIFICA LOCALITA'
514500990730     C     E14LOC        IFNE      VARLOM
514600990730     C                   CLEAR                   WZ1
514700990730     C                   CLEAR                   WZ2
514800990730     C                   CLEAR                   WZ3
514900990730     C                   MOVEL     E14LOC        VARLOM
515000990730     C                   ENDIF
515100990730     C*
515200990730     C     E14PRV        IFNE      VARPRM
515300990730     C     E14NAR        ORNE      VARNAM
515400990730     C                   CLEAR                   WZ3
515500990730     C                   MOVEL     E14PRV        VARPRM
515600990730     C                   MOVEL     E14NAR        VARNAM
515700990730     C                   END
515800990730     C**
515900990730     C                   MOVEL     WZV           E13FZV
516000990730     C                   MOVEL     WZ1           E13FZ1
516100990730     C                   MOVEL     WZ2           E13FZ2
516200990730     C                   MOVE      WZ3           E13FZ3
516300020313     c                   if        §ogntw = 'FED'
516400020313     c                   movel     'S'           i95fi1
516500020313     c                   endif
516600990730     C                   ELSE
516700990730     C** DESTINTARIO
516800990730     C     E14CAP        IFNE      VARCAD
516900990730     C                   CLEAR                   WZVD
517000990730     C                   CLEAR                   WZ1D
517100990730     C                   CLEAR                   WZ2D
517200990730     C                   CLEAR                   WZ3D
517300990730     C                   MOVEL     E14CAP        VARCAD
517400990730     C                   ENDIF
517500990730     C** MODIFICA LOCALITA'
517600990730     C     E14LOC        IFNE      VARLOD
517700990730     C                   CLEAR                   WZ1D
517800990730     C                   CLEAR                   WZ2D
517900990730     C                   CLEAR                   WZ3D
518000990730     C                   MOVEL     E14LOC        VARLOD
518100990730     C                   ENDIF
518200990730     C*
518300990730     C     E14PRV        IFNE      VARPRD
518400990730     C     E14NAR        ORNE      VARNAD
518500990730     C                   CLEAR                   WZ3D
518600990730     C                   MOVEL     E14PRV        VARPRD
518700990730     C                   MOVEL     E14NAR        VARNAD
518800990730     C                   END
518900990730     C**
519000990730     C                   MOVEL     WZVD          E13FZV
519100990730     C                   MOVEL     WZ1D          E13FZ1
519200990730     C                   MOVEL     WZ2D          E13FZ2
519300990730     C                   MOVE      WZ3D          E13FZ3
519400990730     C                   ENDIF
519500990730     C**
519600990730     C** CALL
519700990730     C                   CALL      'FNLV13R'
519800990730     C                   PARM                    KPJBA
519900990730     C                   PARM                    FNLV13DS
520000990730     C                   PARM                    TISI95DS
520100990730     C                   MOVEL     'S'           WLV13             1            DI COMODO
520200990730     C*
520300990730     C                   MOVEL     O13ERR        WERR              1
520400990730     C                   MOVEL     O13MSG        $MSG
520500990730     C                   EXSR      CTRERR
520600990730     C**
520700990730     C* REIMPOSTO LE FORZATURE
520800990730     C     WCLI          IFEQ      'M'
520900990730     C                   MOVEL     E13FZV        WZV
521000990730     C                   MOVEL     E13FZ1        WZ1
521100990730     C                   MOVEL     E13FZ2        WZ2
521200990730     C                   MOVEL     E13FZ3        WZ3
521300990730     C                   ELSE
521400990730     C                   MOVEL     E13FZV        WZVD
521500990730     C                   MOVEL     E13FZ1        WZ1D
521600990730     C                   MOVEL     E13FZ2        WZ2D
521700990730     C                   MOVEL     E13FZ3        WZ3D
521800990730     C                   ENDIF
521900990730     C*
522000990730     C     ENDIND        ENDSR
522100990730     C*------------------------------------------------------------------------*
522200990730     C* CONTROLLO SE C'E' ERRORE-------------------------------------*
522300990730     C*------------------------------------------------------------------------*
522400990730     C     CTRERR        BEGSR
522500990730     C* IMPOSTO CAMPI PASSATI
522600990730     C     O13RON        IFEQ      'S'
522700990730     C                   MOVEL     O95NAR        E14NAR
522800990730     C                   ENDIF
522900990730     C     O13ROC        IFEQ      'S'
523000990730     C                   MOVEL     O95CAP        E14CAP
523100990730     C                   ENDIF
523200990730     C     O13ROP        IFEQ      'S'
523300990730     C                   MOVEL     O95PRV        E14PRV
523400990730     C                   ENDIF
523500990730     C     O13ROL        IFEQ      'S'
523600990730     C                   MOVEL     O95LOC        E14LOC
523700990730     C                   ENDIF
523800990730     C**
523900990730     C* ERRORE O RICERCA CAMPO
524000990730     C     WERR          IFNE      ' '
524100990730     C*
524200990730     C                   MOVEL     '1'           $ERR              1
524300990730     C*
524400990730     C     $MSG          IFNE      *BLANKS
524500990730     C                   EXSR      CARMSG
524600990730     C                   ENDIF
524700990730      * POSIZIONAMENTO CURSORE
524800990730     C                   SELECT
524900990730      *
525000990730     C     WERR          WHENEQ    '1'                                          R.SOCIALE
525100990730     C     WERR          OREQ      '2'                                          INDIRIZZO
525200990730     C     WERR          OREQ      '3'                                          LOCALITA'
525300080423     C**     WERR          OREQ      '7'                                          P.IVA
525400990730     C     WCLI          IFEQ      'M'
525500990730     C                   SETON                                        62
525600990730     C                   ENDIF
525700990730     C     WCLI          IFEQ      'D'
525800990730     C                   SETON                                        71
525900990730     C                   ENDIF
526000990730      *
526100990730     C     WERR          WHENEQ    '4'                                          PROVINCIA
526200990730     C     WCLI          IFEQ      'M'
526300990730     C                   SETON                                        61
526400990730     C                   ENDIF
526500990730     C     WCLI          IFEQ      'D'
526600990730     C                   SETON                                        86
526700990730     C                   ENDIF
526800990730      *
526900990730     C     WERR          WHENEQ    '5'                                          (PC) SU CAP
527000990730     C     WCLI          IFEQ      'M'                                          CAP
527100990730     C                   SETON                                        65
527200990730     C                   ENDIF
527300990730     C     WCLI          IFEQ      'D'
527400990730     C                   SETON                                        72
527500990730     C                   ENDIF
527600990730      *
527700990730     C     WERR          WHENEQ    '6'                                          (PC) NAZIONE
527800990730     C     WCLI          IFEQ      'M'
527900990730     C                   SETON                                        73
528000990730     C                   ENDIF
528100990730     C     WCLI          IFEQ      'D'
528200990730     C                   SETON                                        74
528300990730     C                   ENDIF
528400080423     c
528500080423     C     WERR          WHENEQ    '7'                                          (PC) NAZIONE
528600080423     C     WCLI          IFEQ      'M'
528700080423     C                   SETON                                        42
528800080423     C                   ENDIF
528900080423     C     WCLI          IFEQ      'D'
529000080423     C                   SETON                                        43
529100080423     C                   ENDIF
529200990730      *
529300061107     **C     WERR          WHENEQ    '8'                                          CODICE ISO
529400061107     **C     WCLI          IFEQ      'M'
529500061107     **C                   SETON                                        51
529600061107     **C                   ENDIF
529700061107     **C     WCLI          IFEQ      'D'
529800061107     **C                   SETON                                        52
529900061107     **C                   ENDIF
530000020313     c     werr          wheneq    '0'
530100020313    3C     WCLI          IFEQ      'M'
530200020313     C                   SETON                                        61
530300020313    3C                   ENDIF
530400990730      *
530500990730     C                   ENDSL
530600990730     C*
530700990730     C                   ENDIF
530800990730      *
530900990730     C                   ENDSR
531000011023      *
531100011023      *------------------------------------------------------------------------*
531200011023      * TASSAZIONE VARIA 'N'                                                   *
531300011023      *------------------------------------------------------------------------*
531400011024     c     tassan        begsr
531500011023
531600011024     c                   clear                   dtasv
531700011025     c                   clear                   tassva
531800011025     c                   clear                   tasvca
531900011025     c                   clear                   tastic
532000011025     c                   clear                   tascmb
532100011025     c                   clear                   tascas
532200011025     c                   clear                   taspor
532300011025     c                   clear                   tasimv
532400011025     c                   clear                   taserr
532500011025     c                   clear                   tasmsg
532600011025     c                   clear                   task88
532700011025     c                   clear                   tasial
532800011025     c                   clear                   tasbap
532900080528     c                   clear                   tasban
533000080528     c                   If        %Subst(TasGva:1:1) = 'B'
533100080528     c                   Z-Add     V1cBan        TasBan
533200080528     c                   EndIf
533300011023      *
533400011024     c                   z-add     ta$dsp        tasdsp
533500011024     c                   movel     tasftc        tastc1
533600011024     c                   z-add     ta$dsp        tasdct
533700011023      *
533800011024      * Cerca il contrassegno
533900011023      * se trovato, non annullato, verifico che il 1o carattere del tipo bolla corrisponda
534000011024     c                   if        v1cfca <> *blanks
534100011024     c                   z-add     v1caas        ksbaas
534200011024     c                   z-add     v1clnp        ksblnp
534300011024     c                   z-add     v1cnrs        ksbnrs
534400011024     c                   z-add     v1cnsp        ksbnsp
534500011024     c     keycsb        chain(n)  tncsb03l
534600011024     c                   if        %found  and  csbsta <> 9  and
534700011023     c                             %subst(v1ctbl:1:1) = %subst(csbtbl:1:1)
534800011024     c                   z-add     csbcas        tascas
534900011024     c                   movel     csbvca        tasvca
535000011024     c                   z-add     csbcmb        tascmb
535100011024     c                   movel     csbtpa        tastic
535200061013     c****               move      csbtpi        tastic
535300061013     c                   if        csbfus <> *blanks
535400061013     c                   move      csbfus        tastic
535500061013     c                   else
535600061013     c                   move      csbtpi        tastic
535700061013     c                   end
535800011024     c                   endif
535900011024     c                   endif
536000080528     C                   Z-ADD     V1CNCL        §TASNCL
536100080528     c                   If        %Subst(TasGva:1:1) = 'O'
536200080528     c                   Z-Add     V1cCor        §TasNcl
536300080528     c                   EndIf
536400011023      *
536500011024      * flag si/no ddt
536600011024     c                   if        %subst(v1dddt:1:1) = 'S'
536700011024     c                   movel     'S'           tasddt
536800011024     c                   endif
536900110701     c                   movel     tasfbr        §tasflo
537000011023      *
537100011024     c                   clear                   kpjbu
537200011023      *
537300011024     c                   call      'TNSF20R'
537400011024     c                   parm                    kpjba
537500011024     c                   parm                    dtas
537600011024     c                   parm                    dtasv
537700011023      * Tassazione andata buon fine
537800011024     c                   if        taserr = *BLANKS
537900011023      *
538000011023      * Controllo la divisa della tassazione con la divisa del video
538100011023      * se non corrispondono devo convertire la varia 'N' nella divisa
538200011023      * del video
538300011024     c                   if        tasdiv <> v2cdiv
538400011023      * aggancio la tabella CV per sicurezza
538500011024     c                   movel     'CV'          §cod
538600011024     c                   movel(P)  v2cdiv        §key
538700011024     c                   exsr      chktab
538800011024     c                   if        $err = '0'
538900011024     c                   movel     tbluni        dscv
539000011024     c                   endif
539100011023      *
539200011024     c                   clear                   yeurcods
539300011024     c                   movel     tasdiv        yecdvi
539400011024     c                   movel     v2cdiv        yecdvo
539500011024     c                   z-add     tasimv        yecimi
539600011024     c                   if        §cvfdc = 'S'
539700011024     c                   move      '3'           yecdco
539800011024     c                   else
539900011024     c                   move      '0'           yecdco
540000011024     c                   endif
540100011024     c                   call      'YEURCO'
540200011024     c                   parm                    yeurcods
540300011024     c                   if        yecesi = ' '
540400011024     c                   z-add     yecimo        tasimv
540500011024     c                   else
540600011024     c                   z-add     88888888      tasimv
540700011024     c                   endif
540800011024     c                   endif
540900011023      *
541000011024     c                   z-add     tasimv        v2cva1
541100011024     c                   endif
541200011023      *
541300011024     c                   endsr
541400120323      *------------------------------------------------------------------------*
541500120323      *  Controllo window per cliente/tariffa per tassazione
541600120323      *------------------------------------------------------------------------*
541700120323     C     ctrwdwcli     BEGSR
541800120323     c                   setoff                                       1011
541900120323     c* RICERCA ALFABETICA SU CODICE CLIENTE
542000120323     c                   if        w01dksc<>*blanks and w01ksc=0
542100120323     c                   eval      paxdmt=w01dksc
542200120323     C* . . . . . . . . . . . . . . . . . . .Descrizione azienda
542300120323     C                   MOVEL     Vtdrsu        PAXDUT           30
542400120323     C* . . . . . . . . . . . . . . . . . . .PAXSTA=9 ESCLUDO ANNULLATI
542500120323     C                   Z-ADD     9             PAXSTA
542600120323     C* . . . . . . . . . . . . . . .  . . . CAPOCONTO CLIENTI
542700120323     C                   Z-ADD     dutkci        PAXCCC
542800120323     C* . . . . . . . . . . . . . . . . . .  codice da ricercare
542900120323     C                   clear                   PAXFLR
543000120323     C                   CLEAR                   PAXDIT
543100120323     C                   Z-ADD     1             PAXNUM
543200120323     C*
543300120323     C                   CALL      'XALFA3BR'
543400120323     C                   PARM                    PAXDUT
543500120323     C                   PARM                    CODUT
543600120323     C                   PARM                    PAXDMT
543700120323     C                   PARM                    PAXCCC
543800120323     C                   PARM                    PAXSTA            1 0
543900120323     C                   PARM                    PAXFLR           90
544000120323     C                   PARM                    PAXDIT            3
544100120323     C                   PARM                    PAXNUM            2 0
544200120323     C                   PARM                    PAXKCM           80
544300120323     C                   PARM                    PAXKSM          140
544400120323     C                   PARM                    PAXKDM           60
544500120327     c*              S = gestisco uscita da videata di ricerca dopo 50 sec
544600120327     c*                  restituisce l'errore: PARERR = 'SP'-suepro tempo
544700120327     C                   PARM      'S'           PAResci           1
544800120327     C                   PARM                    PARerr            2
544900120327     c* Passato tempo max 50 secondi di attesa su ricerca alfabetica:
545000120327     c*                      torno al sfl
545100120327     c                   if        parERR='SP' AND PAXSTA=-1
545200120327     c                   eval      *inkl='1'
545300120327     c                   goto      fctrwdw
545400120327     c                   endif
545500120323    2C     PAXSTA        IFGT      -1
545600120323     C* codice selezionato: imposto a video
545700120323     C                   MOVEL     PAXKsm        w01ksc
545800120326     C                   MOVEL     paxdmt        w01dksc
545900120323     C                   SETON                                        10
546000120323    2C                   ENDIF
546100120323     c                   goto      fctrwdw
546200120323     c                   endif
546300120323     c* CODICE CLIENTE
546400120328     c                   clear                   w01dksc
546500120323     c* errore se = 0
546600120328     c                   if        w01ksc=0
546700120328     c                   seton                                        1028
546800120328     c                   eval      w01msg = MSG(11)
546900120328     c                   goto      fctrwdw
547000120328     c                   endif
547100120323     c                   move      w01ksc        w0040
547200120323     c* errore se = 8888 o 9999
547300120323     c                   if        w0040=8888 or w0040=9999
547400120326     c                   seton                                        1028
547500120327     c                   eval      w01msg = MSG(73)
547600120323     c                   goto      fctrwdw
547700120323     c                   endif
547800120323     c* verifico esistenza
547900120323     C                   CLEAR                   TIBS69DS
548000120323     C                   Z-ADD     w01ksc        I69KAC
548100120323     C                   MOVEL     KNSIF         I69SIF
548200120323     C                   exsr      CTR_ANAGRA
548300120323sel 2c                   select
548400120323w   2c                   when      O69err = '1'
548500120326     C                   seton                                        1028
548600120327     C                   MOVEL     msg(73)       w01msg
548700120323     C                   MOVEL     *ALL'*'       w01dksc                        *DESCRIZIONE C.D.C.
548800120323     c                   goto      fctrwdw
548900120323w   2c                   when      ACOflg = '*'
549000120326     c                   seton                                        1028
549100120327     c                   movel     Msg(73)       w01msg
549200120323     c                   goto      fctrwdw
549300130220w   2c***                when      ACOabl = '*'  or acoabl='8'
549400130220     c***                seton                                        1028
549500130220     c***                movel     Msg(73)       w01msg
549600130220     c***                goto      fctrwdw
549700120323x   2c                   other
549800120323     C                   MOVEL     ACORAG        w01dksc                        *DESCRIZIONE C.D.C.
549900120323e   2c                   endsl
550000120323     c* la filiale del cliente deve esistere nella schiera dei p.o. gestiti
550100120328     c                   movel     w01ksc        w003a
550200120328    1c                   if        w003a<>'888'
550300120328     c                   clear                   tntaa1ds
550400120709     c* Dal manca tariffa controllo abilitazione tariffe, dalla manut da menù
550500120709     c*     l'abilitazione specifica
550600120709     C                   IF        %subst(d01op0:1:1)='T'
550700120709     c                   eval      itaa1caut='§utegtc'
550800120709     c                   else
550900120709     c                   eval      itaa1caut='§utembs'
551000120709     c                   endif
551100120709     c
551200120328     c                   eval      itaa1ksc=%int(w01ksc)
551300120328     c                   clear                   kpjbu
551400120328     c                   movel     tntaa1ds      kpjbu
551500120328     c                   call      'TNTAA1C'
551600120328     c                   parm                    kpjba
551700120328     c                   movel     kpjbu         tntaa1ds
551800120328    2c                   if        otaa1fabi='N'
551900120328     c                   seton                                        1028
552000120328     c                   movel     Msg(73)       w01msg
552100120328     c                   goto      fctrwdw
552200120328    2C                   endif
552300120328    1C                   endif
552400120323     c* CODICE tariffa
552500120327     c                   if        w01ctr=*blanks
552600120327     c                   seton                                        1128
552700120327     C                   MOVEL     msg(74)       w01msg
552800120327     c                   goto      fctrwdw
552900120327     c                   endif
553000120323     c                   movel     w01ksc        $ksc
553100120323     C                   Z-ADD     TA$DSP        $DSP              8 0          DATA SPED. (A/M/G)
553200120323     C                   movel     w01ctr        $CTR              3
553300120323     C                   EXSR      CHKCTR
553400120326     c                   movel     $ctr          w01ctr
553500120327     c                   if        $ERR <> '0' or $tks<>*blank
553600120326     c                   seton                                        11
553700160229     c                   if        olv59msg<>*blanks
553800120326     c                   seton                                        28
553900160229     C                   MOVEL     olv59msg      w01msg
554000120323     c                   endif
554100120326     c                   goto      fctrwdw
554200120326     c                   endif
554300120323     c
554400120323     c     fctrwdw       tag
554500120323     c                   endsr
554600120326     C*------------------------------------------------------------------------*
554700120326     C     APPSCO        BEGSR
554800120326     C*
554900120326     C* Calcola la % da applicare per ottener l'importo scontato
555000120326     C                   EVAL      WPer = 100 - f15sco
555100120326     C*---
555200120326     C* Totale imponibile
555300120326     C*---
555400120326     C                   EVAL(H)   tasimv = (tasimv * WPer) / 100
555500120326     C*---
555600120326     C* Varie
555700120326     C*---
555800120326do  1c     1             DO        20            v                 5 0
555900120326if  2c                   IF        tva(v) > *zeros
556000120326     C                   EVAL(H)   tva(v) = (tva(v) * WPer) / 100
556100120326e   2c                   ENDIF
556200120326e   1c                   ENDDO
556300120326     C*---
556400120326     C* Trasporto
556500120326     C*---
556600120326     C                   EVAL(H)   taspor = (taspor * WPer) / 100
556700120326     C                   CLEAR                   fnlv16ds
556800120326     C                   MOVEL     'T'           D17FIM
556900120326     C                   Z-ADD     TASPOR        D17POR
557000120326     C                   CALL      'FNLV16R'
557100120326     C                   PARM                    fnlv16ds
557200120326     C                   PARM                    DTASV
557300120326     c                   eval      tasimv=o17imv
557400120326     c*
557500120326     C                   ENDSR
557600150414      *------------------------------------------------------------------------*
557700150414      *  Controlli per consegna particolare "Z"=EXPO
557800150414      *------------------------------------------------------------------------*
557900151120     c*    sr_chkexpo    begsr
558000150414     c* errore se sped. con contrassegno
558100151120    2c*                  if        tasfca <> *blanks
558200151120     C*                  z-add     V1CAAS        KSBAAS                         ANNO
558300151120     C*                  z-add     V1CLNP        KSBLNP                         LNP
558400151120     C*                  z-add     V1CNRS        KSBNRS                         SERIE
558500151120     C*                  z-add     V1CNSP        KSBNSP                         SPEDIZIONE
558600151120     C*    KEYCSB        chain(N)  TNCSB03L
558700151120    3c*                  if        %found(tncsb03l) and csbsta<> 9
558800151120     C*                  eval      $err = '1'
558900151120     C*                  movel     MSG(85)       $MSG
559000151120     C*                  EXSR      CARMSG
559100151120    3c*                  endif
559200151120    2c*                  endif
559300150414     c* errore se sped non è un franco
559400151120     c*                  if        $err = '0' and %subst(tastbl:1:1)<>'F'
559500151120     C*                  eval      $err = '1'
559600151120     C*                  movel     MSG(86)       $MSG
559700151120     C*                  EXSR      CARMSG
559800151120     c*                  endif
559900150414     c* errore se lna non è in tabella
560000151120     c*                  move      v1clna        w003a
560100151120     c*                  if        $err = '0'  and %lookup(w003a:fil_expo)=0
560200151120     C*                  eval      $err = '1'
560300151120     C*                  movel     MSG(87)       $MSG
560400151120     c*                  eval      $msg=%trim($msg) + ' ' + w003a
560500151120     C*                  EXSR      CARMSG
560600151120     c*                  endif
560700151120     c*                  endsr
560800990730      *------------------------------------------------------------------------*
560900990730      *  INIZIA - ROUTINE INIZIALE
561000990730      *------------------------------------------------------------------------*
561100990730     C     INIZIA        BEGSR
561200990730      *
561300990730     C                   TIME                    WTIME                          ORA E DATA
561400990730     C                   MOVE      WTIME         DATEU                          GG/MM/AAAA
561500990730     C     *eur          movel     DATEU         DATA_EUR
561600990730     C     *iso          movel     DATA_EUR      DATEU                          AAAA/MM/GG
561700990730     C                   MOVEL     DATEU         DATAA                          AAAA
561800030411
561900990730      *
562000990730      * RICEVIMENTO PARAMETRI
562100990730     C     *ENTRY        PLIST
562200990730     C                   PARM                    KPJBA
562300150414      *
562400990730     C                   MOVEL     KPJBU         TNSB01DS
562500120221     c
562600120221     c                   exsr      openfile
562700990730      *
562800990730     C                   clear                   SFLULT
562900990730      *
563000990730     C     KEYTAB        KLIST                                                  TABEL00F
563100990730     C                   KFLD                    KTBKUT
563200990730     C                   KFLD                    KTBCOD
563300990730     C                   KFLD                    KTBKEY
563400990730      *
563500990730     C     KEYTAS        KLIST                                                  TITAS30C
563600990730     C                   KFLD                    KASAAS                         ..ANNO
563700990730     C                   KFLD                    KASLNP                         ..LNP
563800990730     C                   KFLD                    KASNRS                         ..SERIE
563900990730     C                   KFLD                    KASNSP                         ..SPEDIZIONE
564000990730     C                   KFLD                    KASTBL                         ..TIPO BOLLA
564100990730      *
564200990730     C     KEYTA1        KLIST                                                  TITASV1L
564300990730     C                   KFLD                    KASAAS                         ..ANNO
564400990730     C                   KFLD                    KASLNP                         ..LNP
564500990730     C                   KFLD                    KASNRS                         ..SERIE
564600990730     C                   KFLD                    KASNSP                         ..SPEDIZIONE
564700990730      *
564800990730     C     KEYTRC        KLIST                                                  TITAA30C
564900990730     C                   KFLD                    KASAAS                         ..ANNO
565000990730     C                   KFLD                    KASLNP                         ..LNP
565100990730     C                   KFLD                    KASNRS                         ..SERIE
565200990730     C                   KFLD                    KASNSP                         ..SPEDIZIONE
565300990730     C                   KFLD                    KASTRC                         ..TIPO RECORD
565400990730      *
565500990730     C     KEYCSB        KLIST                                                  TNCSB03L
565600990730     C                   KFLD                    KSBAAS                         ..ANNO
565700990730     C                   KFLD                    KSBLNP                         ..LNP
565800990730     C                   KFLD                    KSBNRS                         ..SERIE
565900990730     C                   KFLD                    KSBNSP                         ..SPEDIZIONE
566000990730      *
566100990730     C     KEYCET        KLIST                                                  ECCET30C
566200990730     C                   KFLD                    KCTAAS                         ..ANNO
566300990730     C                   KFLD                    KCTLNP                         ..LINEA DI PARTENZA
566400990730     C                   KFLD                    KCTNRS                         ..SERIE
566500990730     C                   KFLD                    KCTNSP                         ..SPEDIZIONE
566600990730     C                   KFLD                    KCTTBL                         ..TIPO BOLLA
566700030204
566800030204     c     kFiar5        Klist
566900030204     c                   Kfld                    KasAas
567000030204     c                   Kfld                    KasLnp
567100030204     c                   Kfld                    KasNrs
567200030204     c                   Kfld                    KasNsp
567300030204     c                   Kfld                    kAr5Trd
567400051010      * Chiave file eventi FNEVB
567500051010     c     Kevb4         Klist
567600051010     c                   Kfld                    Kasaas
567700051010     c                   Kfld                    Kaslnp
567800051010     c                   Kfld                    Kasnrs
567900051010     c                   Kfld                    Kasnsp
568000051010     c                   Kfld                    Wcev
568100061114
568200061114     c     Kppa          klist
568300061114     c                   kfld                    wsoc
568400061114     c                   kfld                    wctb
568500061114     c                   kfld                    wkcc
568600061114     c                   kfld                    wksc
568700061114     c                   kfld                    wdft
568800061114     c                   kfld                    wnft
568900061114     c                   kfld                    wfiv
569000061114     c     Kycpa         klist
569100061114     c                   kfld                    ppasys
569200061114     c                   kfld                    ppanrasint
569300990730      *
569400990730     C                   MOVEL     '0'           WZV               1 0
569500990730     C                   MOVEL     '0'           WZ1               1 0
569600990730     C                   MOVEL     '0'           WZ2               1 0
569700990730     C                   MOVEL     '0'           WZ3               1 0
569800990730     C                   MOVEL     '0'           WZVD              1 0
569900990730     C                   MOVEL     '0'           WZ1D              1 0
570000990730     C                   MOVEL     '0'           WZ2D              1 0
570100990730     C                   MOVEL     '0'           WZ3D              1 0
570200990730     C                   CLEAR                   VARCAM
570300990730     C                   CLEAR                   VARLOM
570400990730     C                   CLEAR                   VARPRM
570500990730     C                   CLEAR                   VARNAM
570600990730     C                   CLEAR                   VARCAD
570700990730     C                   CLEAR                   VARLOD
570800990730     C                   CLEAR                   VA2CAD
570900990730     C                   CLEAR                   VA2LOD
571000990730     C                   CLEAR                   VARPRD
571100990730     C                   CLEAR                   VARNAD
571200020516     C                   clear                   WsavMIT
571300120802     C                   clear                   savQUATT
571400020516     C                   clear                   WsavDES
571500030331     C                   clear                   WsavDES9
571600080922     C                   clear                   Wmitor
571700151019     C                   clear                   savimv
571800151019     C                   clear                   sav_tasimv
571900030331     c                   Eval      wnofatt = *Off
572000030331     c                   Eval      wforza = *Off
572100061107     c                   eval      wforzacdfm = *off
572200120508     c                   eval      wforzacdfm1 = *off
572300061107     c                   eval      wforzacdfd = *off
572400071026     c                   eval      wforzacdfd = *off
572500120508     c                   eval      wforzacdfd1 = *off
572600071026     c                   clear                   forzapid2
572700071026     c                   clear                   sav1cpid
572800071029     c                   clear                   forzapim2
572900071029     c                   clear                   sav1cpim
573000080922     c                   setoff                                       05
573100120326     c                   clear                   f15ksc
573200120326     c                   clear                   f15ctr
573300120326     c                   clear                   f15sco
573400120326     c                   clear                   wf15
573500990730      *
573600990730      * ESEGUE OPERAZIONI INIZIALI STANDARD
573700990730     C                   EXSR      INZSTD
573800020516      * Reperisco le autorizzazioni dell'utente
573900020516     C                   exsr      Rep_Aut_Ute
574000020516     c                   if        $Fine = *on
574100020516     c                   leavesr
574200020516     c                   endif
574300990730      * CARICO VARIE TABELLE
574400990730     C                   EXSR      CARTAB                                       *CARICA TABELLE
574500990730      *
574600990730      * RAGIONE SOCIALE UTENTE
574700020516     C                   MOVEL     RSUT          VTDRSU
574800990730      *
574900990730      * IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
575000990730      * 0=NON PROTETTO, 1=PROTETTO
575100990730     C                   EVAL      *IN95 = *OFF
575200990730      *
575300990730      * IMPOSTO KEY SPEDIZIONE
575400990730     C                   Z-ADD     D01AAS        KASAAS                         *ANNO
575500990730     C                   Z-ADD     D01LNP        KASLNP                         *LNP
575600990730     C                   Z-ADD     D01NRS        KASNRS                         *SERIE
575700990730     C                   Z-ADD     D01NSP        KASNSP                         *SPEDIZIONE
575800990730     C                   MOVEL     D01TBL        KASTBL                         *TIPO BOLLA
575900990730      *
576000990730     C                   eval      *in81 = *off
576100990730     C                   eval      *in82 = *off
576200990730     C                   eval      *in83 = *off
576300990730      *
576400990730     C     KEYTAS        CHAIN(e)  TITAS30C
576500990730      *
576600990730      * Se il record non esiste torno il controllo al chiamante con messaggio di errore
576700990730      * Se il record è di TITASP00 non si possono effettuare variazioni, ritorno il controllo al
576800990730      *   chiamante con messaggio di errore
576900990730     c                   SELECT
577000990730      *
577100990730     c                   WHEN      %ERROR                                       *ALLOCAZIONE
577200990730     C                   movel     MSG(22)       D01MSG
577300990730     C                   eval      $ERR = '1'
577400990730     C                   eval      $FINE = '1'
577500990730      *
577600990730     c                   WHEN      not %FOUND                                   *NON TROVATO
577700990730     C                   movel     MSG(21)       D01MSG
577800990730     C                   eval      $ERR = '1'
577900990730     C                   eval      $FINE = '1'
578000990730      *
578100070201      * possibilità di variare c.f. su bolle presenti su titasp
578200070201      * con le stesse regole per quelle già contabilizzate
578300070201      * se utente non 'AZ' e bolla fatturata controllo anche se fattura di sede
578400070201     c**!!!              WHEN      *IN83 = *ON                                  *TNTASP0F
578500070201     c                   WHEN          *in22  = *on
578600070201     c                             and *in83
578700170727     c***                          and (tasfiv = §qcfiv or tasfiv = §qcfi2)
578701170727     c                             and tasfiv >= 900
578800990730     C                   movel     MSG(16)       D01MSG
578900990730     C                   eval      $ERR = '1'
579000990730     C                   eval      $FINE = '1'
579100070129      * se utente non 'AZ' e bolla fatturata controllo anche se fattura di sede
579200020516      *
579300020516     c                   WHEN          *in22  = *on
579400020516     c                             and TASdft > 0
579500170727     c***                          and (tasfiv = §qcfiv or tasfiv = §qcfi2)
579501170727     c                             and tasfiv >= 900
579600020516     C                   movel     MSG(59)       D01MSG
579700020516     C                   eval      $ERR = '1'
579800020516     C                   eval      $FINE = '1'
579900990730      *
580000990730     c                   OTHER
580100020516      *  controllo se il P.O. di competenza per il cliente è compreso
580200020516      *    tra quelli gestibili dall'utente
580300020516     c                   if        *in22
580400120328     c***                movel     TASksc        wPOa
580500120328     c***  wPOa          lookup    $POG                                   99
580600120328     c***                if        not *in99
580700120328     c*                  movel     TASksc        wKSCa
580800120328     c*                  eval      D01msg = %subst(msg(60):1:45) +
580900120328     c*                                     ' '                  +
581000120328     c*                                     wKSCa                +
581100120328     c*                                     ' '                  +
581200120328     c*                                     %subst(msg(60):55:20)
581300120328     c*                  eval      $ERR = '1'
581400120328     c*                  eval      $FINE = '1'
581500120328     c*                  leavesr
581600120328     c*                  endif
581700120328     c                   clear                   tntaa1ds
581800120709     C                   IF        %subst(d01op0:1:1)='T'
581900120709     c                   eval      itaa1caut='§utegtc'
582000120709     c                   else
582100120709     c                   eval      itaa1caut='§utembs'
582200120709     c                   endif
582300120709     c
582400120328     c                   eval      itaa1ksc=%int(tasksc)
582500120328     c                   clear                   kpjbu
582600120328     c                   movel     tntaa1ds      kpjbu
582700120328     c                   call      'TNTAA1C'
582800120328     c                   parm                    kpjba
582900120328     c                   movel     kpjbu         tntaa1ds
583000120328    2c                   if        otaa1fabi='N'
583100120328     c                   movel     TASksc        wKSCa
583200120328     c                   eval      D01msg = %subst(msg(60):1:45) +
583300120328     c                                      ' '                  +
583400120328     c                                      wKSCa                +
583500120328     c                                      ' '                  +
583600120328     c                                      %subst(msg(60):55:20)
583700120328     c                   eval      $ERR = '1'
583800120328     c                   eval      $FINE = '1'
583900120328     c                   leavesr
584000120328    2C                   endif
584100020516     c                   endif
584200990730      *    Proteggo campi
584300990730     c                   eval      *IN91 = (*IN82 = *ON  or
584400990730     c                                    (*IN81 = *ON  and  TASfic <> *blanks))
584500990730     c                   eval      *IN92 = (*IN82 = *ON)
584600070201      * se bolla su titasp proteggo
584700070206     c  n91              eval      *in91 = (*in83 = *on)
584800990730      *
584900990730     C                   clear                   WV1Cmit
585000990730     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
585100990730     C                   EXSR      CARV1                                        CARICA TESTA/FUNZION
585200990730      *
585300990730      * TOLGO FUNZIONE F16 PER LE COMPETENZE DI CONTO ECONOMICO SE NON CI SONO
585400990730     C                   Z-ADD     D01AAS        KCTAAS                         *ANNO
585500990730     C                   Z-ADD     D01LNP        KCTLNP                         *LNP
585600990730     C                   Z-ADD     D01NRS        KCTNRS                         *SERIE
585700990730     C                   Z-ADD     D01NSP        KCTNSP                         *SPEDIZIONE
585800990730     C                   MOVEL     D01TBL        KCTTBL                         *TIPO BOLLA
585900990730     C     KEYCET        CHAIN     ECCET30C
586000990730     C                   IF        not %FOUND
586100990730     C                   MOVEL     FUN(16)       FUNRIG
586200990730     C                   MOVEL     'N'           FUNASG
586300990730     C                   MOVEL     'N'           FUNASI
586400990730     C                   MOVEL     'N'           FUNAFG
586500990730     C                   MOVEL     'N'           FUNAFG
586600990730     C                   MOVEL     FUNRIG        FUN(16)
586700990730     C                   ENDIF
586800990730      *
586900990730      * TOLGO FUNZIONE F18 PER I LEGAMI SE NON CI SONO
587000990730     C     TASFA1        IFEQ      *BLANKS
587100990730     C                   MOVEL     FUN(18)       FUNRIG
587200990730     C                   MOVEL     'N'           FUNASG
587300990730     C                   MOVEL     'N'           FUNASI
587400990730     C                   MOVEL     'N'           FUNAFG
587500990730     C                   MOVEL     'N'           FUNAFG
587600990730     C                   MOVEL     FUNRIG        FUN(18)
587700990730     C                   ENDIF
587800990730      *
587900990730      * TOLGO FUNZIONE F10 PER LE VARIAZIONI SE NON CI SONO
588000990730     C     KEYTA1        SETLL     TITASV1L
588100990730     C                   IF        %EQUAL = *OFF
588200990730     C                   MOVEL     FUN(10)       FUNRIG
588300990730     C                   MOVEL     'N'           FUNASG
588400990730     C                   MOVEL     'N'           FUNASI
588500990730     C                   MOVEL     'N'           FUNAFG
588600990730     C                   MOVEL     'N'           FUNAFG
588700990730     C                   MOVEL     FUNRIG        FUN(10)
588800990730     C                   ENDIF
588900990730      *
589000990730      * TOLGO FUNZIONE F22=CONTRASSEGNO SE NON C'E' CONTRASSEGNO
589100990730     C     V1CFCA        IFEQ      *BLANKS
589200990730     C                   MOVEL     FUN(22)       FUNRIG
589300990730     C                   MOVEL     'N'           FUNASG
589400990730     C                   MOVEL     'N'           FUNASI
589500990730     C                   MOVEL     'N'           FUNAFG
589600990730     C                   MOVEL     'N'           FUNAFG
589700990730     C                   MOVEL     FUNRIG        FUN(22)
589800990730     C                   ENDIF
589900990730      *
590000990730      * DECODIFICO
590100990730     c
590200990730     C                   IF        V1CAAS > *zeros  and  V1CLNP > *zeros  and
590300990730     C                             V1CNSP > *zeros  and  V1CTBL <> *blanks
590400071106     c                   eval      winizia='1'
590500990730     C                   EXSR      DECOD1
590600990730     C                   ENDIF
590700990730      *
590800990730      * UTILIZZO UN CAMPO PER SAPERE SE LA BOLLA VIENE VARIATA
590900990730     C                   clear                   WMODIF
591000990730      *
591100990730     C                   ENDSL
591200990730      *
591300020516     C                   ENDSR
591400120221      *
591500120221      *------------------------------------------------------------------------*
591600120221      * OPENFILE    - apre i file puntandoli alla libreria di sede
591700120221      *------------------------------------------------------------------------*
591800120221     c     Openfile      BEGSR
591900120221      *
592000120221     c                   if        %subst(knsif:7:1)='P'
592100120221     c                   eval      wlibsede='GAITRAGRPS/'
592200120221     c                   else
592300120221     c                   eval      wlibsede='GAITRAGRU /'
592400120221     c                   endif
592500120221     c                   eval      wlibsede=%trim(wlibsede)+ 'TNCSB03L'
592600120221     c                   open      tncsb03l
592700120221     c                   eval      %subst(wlibsede:12:8)= 'TITAS30C'
592800120221     c                   open      titas30c
592900120221     c                   eval      %subst(wlibsede:12:8)= 'TITASV1L'
593000120221     c                   open      titasv1l
593100120221     c                   eval      %subst(wlibsede:12:8)= 'TITA7V0F'
593200120221     c                   open      tita7v0f
593300120221     c                   eval      %subst(wlibsede:12:8)= 'TITA730C'
593400120221     c                   open      tita730c
593500120221     c                   eval      %subst(wlibsede:12:8)= 'TITAA30C'
593600120221     c                   open      titaa30c
593700120221     c                   eval      %subst(wlibsede:12:8)= 'TITA430C'
593800120221     c                   open      tita430c
593900120221     c                   eval      %subst(wlibsede:12:8)= 'TABEL00F'
594000120221     c                   open      tabel00f
594100120221     c                   eval      %subst(wlibsede:12:8)= 'ECCET30C'
594200120221     c                   open      eccet30c
594300120221     c                   eval      %subst(wlibsede:12:8)= 'FIAR531C'
594400120221     c                   open      fiar531c
594500120416     c* FNARB lo punto alla libreria di fililale
594600120416     c                   if        %subst(knsif:7:1)='P'
594700120416     c                   eval      wlibfil='FILTRAPRD/'
594800120416     c                   else
594900120416     c                   eval      wlibfil='FILTRA201/'
595000120416     c                   endif
595100120416     c                   eval      wlibfil=%trim(wlibfil)+ 'FNARB01L'
595200120416     c                   open      FNARB01L
595300120221     c*
595400120221     c                   endsr
595500020516      *
595600020516      *------------------------------------------------------------------------*
595700020516      * REP_AUT_UTE - REPERISCE LE AUTORIZZAZIONI ALL'UTENTE
595800020516      *------------------------------------------------------------------------*
595900020516     c     Rep_Aut_Ute   BEGSR
596000020516      *
596100020516     c                   eval      *in22  = *off
596200020516     c                   clear                   wABI
596300020516     c                   clear                   DLAT
596400020516      *
596500020516      *-- Verifica errori e autorità profilo
596600020516sel 1c                   SELECT
596700020516      *-- controllo se ho errori nei dati utente
596800020516      *--   nel qual caso NON risulta un profilo abilitato
596900020516w   1c                   WHEN      DUTERR = 'E'
597000020516     c                   movel     MSG(58)       D01MSG
597100020516     c                   eval      $ERR   = '1'
597200020516     c                   eval      $FINE  = '1'
597300020516     c                   leavesr
597400020516      *
597500020516      *-- CONTROLLO AUTORITA'
597600020516      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
597700020516      *-- se il chiamante non richiede autorità specifica verificare
597800020516      *--   quella del profilo
597900020516      *-- se il chiamante richiede autorità specifica verificarla,
598000020516      *--  se è blank verificare quella del profilo
598100020516      *
598200020516      *-- se UTEAUT = *BLANK non siamo in sede
598300020516      *--     nel qual caso saranno attuate le protezioni del caso
598400020516w   1c                   WHEN      UTEAUT = *blank
598500020516     c                   eval      *in22  = *on
598600020516      *
598700020516x   1c                   OTHER
598800020516      *--
598900020516     c                   movel     UTEFAF        DUTE01
599000120709      *-- Autorizzazione alla manutenzione delle bolle di sede o
599100120709      *--                al   manca tariffa
599200120709     c                   select
599300120709     C                   when      %subst(d01op0:1:1)='T'  and §utegtc<>'  '
599400120709     c                   eval      Wabi=§utegtc
599500120709     C                   when      %subst(d01op0:1:1)<>'T'  and §utembs<>'  '
599600020516      *--   Con AUTORITÀ SPECIFICA
599700020516     c                   eval      wabi = §UTEmbs
599800120709x   2c                   other
599900020516      *--   Senza AUTORITÀ SPECIFICA
600000020516     c                   eval      wabi = UTEAUT
600100120709e   2c                   ENDsl
600200020516      *
600300020516     c                   clear                   tibs02ds
600400020516     c                   movel     'C'           t02mod
600500020516     c                   movel     knsif         t02sif
600600020516     c                   movel     'LAT'         t02cod
600700020516     c                   movel(p)  wabi          t02ke1
600800020516     c                   call      'TIBS02R'
600900020516     c                   parm                    kpjba
601000020516     c                   parm                    tibs02ds
601100020516if  2c                   if        t02err = *blanks
601200020516     c                   movel     t02uni        DLAT
601300020516e   2c                   endif
601400020516if  2c                   if        T02err <> *blanks   or
601500020516     c                             §LATabi = 'S'
601600020516     c                   movel     MSG(58)       D01MSG
601700020516     c                   eval      $ERR   = '1'
601800020516     c                   eval      $FINE  = '1'
601900020516     c                   leavesr
602000020516e   2c                   endif
602100020516      *
602200020516e   1c                   ENDSL
602300020516      *
602400020516      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
602500020516      *
602600020516      * Utente con abilitazione valida e diversa da "AZ"
602700020516if  1c                   if        wABI  <> 'AZ'
602800020516      *
602900020516     c                   eval      *in22  = *on
603000020516      *
603100020516      * Reperimento dei P.O. gestibili dall'utente
603200120328     c*                  clear                   TRUL31DS
603300120328     c*                  eval      I31abi = wabi
603400120328     c*                  eval      I31cdi = DUTdis
603500120328     c*                  eval      I31car = DUTare
603600120328     c*                  eval      I31cpo = DUTpou
603700120328     c*                  call      'TRUL31R'
603800120328     c*                  parm                    KPJBA
603900120328     c*                  parm                    TRUL31DS
604000120328if  2c*                  if        O31pog > *zeros
604100120328     c*                  movea     O31pog        $POG
604200120328x   2c*                  else
604300120328     c*                  movel     MSG(58)       D01MSG
604400120328     c*                  eval      $ERR  = '1'
604500120328     c*                  eval      $FINE = '1'
604600120328     c*                  leavesr
604700120328e   2c*                  endif
604800020516      *
604900120328x   1c*                  else
605000120328     c*                  reset                   $POG
605100020516e   1c                   endif
605200030709
605300030709      * se utente non abilitato alla modifica dell'importo da assicurare
605400030709      * proteggo il campo
605500030709     c                   Eval      *In02 = (§uteIas <> 'S')
605600020516      *
605700020516     c     E_RepAutUte   ENDSR
605800020516      *
605900990730      *------------------------------------------------------------------------*
606000990730      * CARTAB - CARICA TABELLE VARIE
606100990730      *------------------------------------------------------------------------*
606200990730     C     CARTAB        BEGSR
606300990730      *
606400991102      * AGGANCIO TABELLA GED PER DIVISA DELLA MONETA DI CONTO
606500991102     C                   CLEAR                   tibs02ds
606600991102     C                   MOVEL     'C'           T02MOD
606700991102     C                   MOVEL     KNSIF         T02SIF
606800991102     C                   MOVEL     'GED'         T02COD
606900991102     C                   MOVEL     '1'           T02KE1
607000991102      *
607100991102     C                   CALL      'TIBS02R'
607200991102     C                   PARM                    KPJBA
607300991102     C                   PARM                    tibs02ds
607400991102      *
607500991102     C                   if        T02ERR = *BLANKS
607600991102     C                   MOVEL     T02UNI        DGED
607700991102     C                   ELSE
607800991102     C                   CLEAR                   DGED
607900991102     C                   ENDIF
608000011129      * importi DELLA MONETA DI CONTO
608100011129     c                   clear                   tibs02ds
608200011129     c                   clear                   dgei
608300011129     c                   movel     'C'           t02mod
608400011129     c                   movel     knsif         t02sif
608500011129     c                   movel     'GEI'         t02cod
608600011129     c                   movel     §gedcn        t02ke1
608700011129     c                   call      'TIBS02R'
608800011129     c                   parm                    kpjba
608900011129     c                   parm                    tibs02ds
609000011129     c                   if        t02err = *blanks
609100011129     c                   movel     t02uni        dgei
609200011129     c                   endif
609300011129
609400011129      * aggancio la tabella CV della monte di conto
609500011129     c                   movel     'CV'          §cod
609600011129     c                   movel(P)  §gedcn        §key
609700011129     c                   exsr      chktab
609800011129     c                   if        $err = '0'
609900011129     c                   movel     tbluni        dscv
610000011129     c                   movel     §cvdec        decsav
610100011129     c                   endif
610200991102      *
610300990730      * Recupero codice variE
610400990730     C                   MOVEL     '$2'          KTBCOD
610500990730     C                   movel(P)  1             KTBKEY
610600990730     C     KEYTAB        CHAIN     TABEL00F
610700990730     c                   IF        %FOUND  and  TBLflg = *blanks
610800990730     C                   movel     tbluni        DS$2
610900990730     c                   ENDIF
611000990812      *
611100990812      * Per determinare se la spedizione è un prepagato aggancio la tabella QC
611200170727     C***                MOVEL     'QC'          KTBCOD
611300170727     C***                MOVEL(P)  '2'           KTBKEY
611400170727     C***  KEYTAB        CHAIN     TABEL00F
611500170727     c***                IF        %FOUND  and  TBLflg = *blanks
611600170727     C***                MOVEL     TBLUNI        DSQC2
611700170727     c***                ELSE
611800170727     C***                CLEAR                   DSQC2
611900170727     c***                ENDIF
612000021122
612100021122      * Aggancio tabella default tariffe fedex
612200021125     c                   Clear                   Tibs02Ds
612300021122     c                   Movel     'C'           T02Mod
612400021122     c                   Movel     KNSIF         T02Sif
612500021122     c                   Movel     'DFE'         T02Cod
612600021122     c                   Movel     'FED'         T02Ke1
612700021122     c                   Call      'TIBS02R'
612800021122     c                   Parm                    Kpjba
612900021125     c                   Parm                    Tibs02Ds
613000021122     c                   If        T02Err = *Blanks
613100021122     c                   Movel     T02Uni        DDfe
613200021122     c                   Else
613300021122     c                   Clear                   DDfe
613400021122     c                   EndIf
613500030411
613600030411      * calcolo i gg. di pulizia bolle arrivi in filiale
613700030411     c                   clear                   ds5a
613800030411     c                   movel     '5A'          §cod
613900030411     c                   movel(P)  '1'           §key
614000030411     c                   exsr      chktab
614100030411     c                   if        $err = '0'
614200030411     c                   movel     tbluni        ds5a
614300030411     c                   endif
614400030411     c     *iso          movel     dateu         data_eur
614500030411     c                   subdur    §5aarb:*d     data_eur
614600030411     c     *iso          movel     data_eur      dtpularr
614700030411     c
614800071026      * Aggancio tabella "VPODECO"
614900071026     C                   CLEAR                   tibs02ds
615000071026     c                   clear                   dvpodeco
615100071026     C                   MOVEL     'C'           T02MOD
615200071026     C                   MOVEL     KNSIF         T02SIF
615300071026     C                   MOVEL     'VPO'         T02COD
615400071026     c                   movel(P)  'DECO'        t02ke1
615500071026     C                   CALL      'TIBS02R'
615600071026     C                   PARM                    KPJBA
615700071026     C                   PARM                    tibs02ds
615800071026    2C                   IF        T02ERR = *BLANKS
615900071026     C                   MOVEL     T02uni        dvpodeco
616000071026    2C                   ENDIF
616100150414      * Aggancio tabella "VPO" "EXPO"
616200151120     C*                  CLEAR                   tibs02ds
616300151120     c*                  clear                   fil_expo
616400151120     C*                  MOVEL     'C'           T02MOD
616500151120     C*                  MOVEL     KNSIF         T02SIF
616600151120     C*                  MOVEL     'VPO'         T02COD
616700151120     c*                  movel(P)  'EXPO'        t02ke1
616800151120     C*                  CALL      'TIBS02R'
616900151120     C*                  PARM                    KPJBA
617000151120     C*                  PARM                    tibs02ds
617100151120    2C*                  IF        T02ERR = *BLANKS
617200151120     C*                  MOVEa     T02uni        fil_expo
617300151120    2C*                  ENDIF
617400141217      /free
617500141217       //?Carico part.consegna con abilitazione PinCode
617600141217         KTBcod = '7R';
617700141217         clear kk;
617800141217         setll (KTBkut:KTBcod) TABEL00F;
617900141217         reade (KTBkut:KTBcod) TABEL00F;
618000141217         DOW  not %eof(TABEL00F);
618100141217           IF  TBLflg = *blanks;
618200141217             ds7R = TBLuni;
618300141217             IF  §7Rpincode = 'S';
618400141217               kk += 1;
618500141217               skGMA(kk) = TBLkey;
618600141217             ENDIF;
618700141217           ENDIF;
618800141217           reade (KTBkut:KTBcod) TABEL00F;
618900141217         ENDDO;
619000141217      /end-free
619100990730      *
619200990730     C                   ENDSR
619300130124      *------------------------------------------------------------------------*
619400130124      *
619500130124      *------------------------------------------------------------------------*
619600130124     C     SR_TIVRB      BEGSR
619700130124     c                   clear                   fnlvv1ds
619800130124     c                   eval      iv1cvb='TS'
619900130124     c                   eval      iv1pgm=vtcpgm
620000130124     c                   eval      iv1aas=tasaas
620100130124     c                   eval      iv1lnp=taslnp
620200130124     c                   eval      iv1nrs=tasnrs
620300130124     c                   eval      iv1nsp=tasnsp
620400130124     c                   eval      iv1tbl=tastbl
620500130124     c                   clear                   darbt
620600130124     c                   z-add     tasias        §btias
620700130124     c                   movel     tasvas        §btvas
620800130124     c                   eval      parvar=darbt
620900130124     c                   eval      kpjbu=fnlvv1ds
621000130124     c                   call      'FNLVV1R'
621100130124     c                   parm                    kpjba
621200130124     c                   parm                    parvar
621300130124     C                   ENDSR
621400990730     C*------------------------------------------------------------------------*
621500990730     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
621600990730     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
621700990730     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
621800990730     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
621900990730     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
622000990730     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
622100990730     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
622200990730     C*------------------------------------------------------------------------*
622300990730     C* INZSTD - ROUTINE INIZIALE STANDARD
622400990730     C*------------------------------------------------------------------------*
622500990730     C     INZSTD        BEGSR
622600990730     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
622700990730     C                   CALL      'XKEYS'
622800990730     C                   PARM                    DSKEYS
622900990730     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
623000990730     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
623100990730     C     XMSGPA        PLIST
623200990730     C                   PARM                    DSMSMS
623300990730     C                   PARM                    DSMSPG
623400990730     C                   PARM                    DSMSKE
623500990730     C                   PARM                    DSMSER
623600990730     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
623700990730     C                   MOVEL     'TNSB04R'     DSMSPG                         *NOME PGM CHIAMANTE
623800990730     C                   MOVEL     'TNSB04R'     VMCQUE                         *CODA MESSAGGI DESTI
623900990730     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
624000990730     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
624100990730     C* INIZIALIZZO FLAG CONTROLLO PGM
624200990730     C                   MOVEL     '0'           $ERR              1            *ERRORE
624300990730     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
624400990730     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
624500990730     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
624600990730     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
624700990730     C                   MOVEL     '0'           $FINE             1            *FINE PGM
624800990730     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
624900990730     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
625000990730     C                   BITOFF    '01234567'    POSMI1            1
625100990730     C                   BITOFF    '01234567'    POSMI2            1
625200990730     C                   BITOFF    '01234567'    POSMA1            1
625300990730     C                   BITOFF    '01234567'    POSMA2            1
625400990730     C                   BITON     '34'          POSMI1            1            *18
625500990730     C                   BITON     '7'           POSMI2            1            *01
625600990730     C                   BITON     '34'          POSMA1            1            *18
625700990730     C                   BITON     '0'           POSMA2            1            *80
625800990730     C                   MOVE      POSMI2        POSMIN            2
625900990730     C                   MOVEL     POSMI1        POSMIN
626000990730     C                   MOVE      POSMA2        POSMAX            2
626100990730     C                   MOVEL     POSMA1        POSMAX
626200990730     C                   ENDSR
626300990730     C*------------------------------------------------------------------------*
626400990730     C* FUNSTD - LANCIA SUBROUTINE STANDARD
626500990730     C*------------------------------------------------------------------------*
626600990730     C     FUNSTD        BEGSR
626700990730     C                   SELECT
626800990730     C     KEY           WHENEQ    F3
626900990730     C                   EXSR      F03STD
627000990730     C     KEY           WHENEQ    F12
627100990730     C                   EXSR      F12STD
627200990730     C     KEY           WHENEQ    F24
627300990730     C                   EXSR      F24STD
627400990730     C                   OTHER
627500990730     C                   MOVEL     MSG(2)        $MSG
627600990730     C                   EXSR      CARMSG
627700990730     C                   ENDSL
627800990730     C                   ENDSR
627900990730     C*------------------------------------------------------------------------*
628000990730     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
628100990730     C*------------------------------------------------------------------------*
628200990730     C     F03STD        BEGSR
628300990730     C* IMPOSTO FLAG DI PREMUTO TASTO F03
628400990730     C                   MOVEL     '1'           $F03
628500990730     C* IMPOSTO FLAG DI FINE PROGRAMMA
628600990730     C                   MOVEL     '1'           $FINE
628700990730     C                   ENDSR
628800990730     C*------------------------------------------------------------------------*
628900990730     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
629000990730     C*------------------------------------------------------------------------*
629100990730     C     F12STD        BEGSR
629200990730     C* IMPOSTO FLAG DI FIN PGM
629300990730     C                   MOVEL     '1'           $FINE
629400990730     C                   MOVEL     '1'           $F12
629500990730     C                   ENDSR
629600990730     C*------------------------------------------------------------------------*
629700990730     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
629800990730     C*------------------------------------------------------------------------*
629900990730     C     F24STD        BEGSR
630000990730     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
630100990730     C                   EXSR      OUTFUN
630200990730     C                   ENDSR
630300990730     C*------------------------------------------------------------------------*
630400990730     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
630500990730     C*          CODA MESSAGGI DEL PGM
630600990730     C*------------------------------------------------------------------------*
630700990730     C     CARMSG        BEGSR
630800990730     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
630900990730     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
631000990730     C     MSGULT        IFEQ      0
631100990730     C                   MOVEL     '1'           *IN40
631200990730     C                   WRITE     SB52DVK
631300990730     C                   MOVEL     '0'           *IN40
631400990730     C                   ENDIF
631500990730     C* INVIA MESSAGGIO ALLA CODA DEL PGM
631600990730     C                   MOVEL     $MSG          DSMSMS
631700990730     C                   CALL      'XMSG'        XMSGPA
631800990730     C     DSMSER        IFEQ      0
631900990730     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
632000990730     C                   ENDIF
632100990730     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
632200990730     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
632300990730     C                   WRITE     SB52DVM                                      *SFL MESS
632400990730     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
632500990730     C                   MOVEL     *BLANKS       $MSG
632600990730     C                   ENDSR
632700990730     C*------------------------------------------------------------------------*
632800990730     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
632900990730     C*------------------------------------------------------------------------*
633000990730     C     OUTMSG        BEGSR
633100990730     C* VISUALIZZAZIONE MESSAGGIO
633200990730     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
633300990730     C                   WRITE     SB52DVK                                      *CONTR MESS
633400990730     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
633500990730     C                   Z-ADD     *ZEROS        MSGULT
633600990730     C                   ENDSR
633700990730     C*------------------------------------------------------------------------*
633800990730     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
633900990730     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
634000990730     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
634100990730     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
634200990730     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
634300990730     C*------------------------------------------------------------------------*
634400990730     C     CARFUN        BEGSR
634500990730     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
634600990730     C     1             DO        31            I                 5 0          --- 1 -->
634700990730     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
634800990730     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
634900990730     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
635000020724     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
635100990730     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
635200990730     C                   ELSE                                                   <-- 2 --> *FILIALE
635300990730     C                   MOVEL     FUNAFG        FAT(I)
635400990730     C                   ENDIF                                                  <-- 2 ---
635500990730     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
635600990730     C                   MOVEL     USER          WEDP              3            *PROFILO
635700990730     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
635800990730     C                   MOVEL     'S'           FAT(I)                         *ABILITO
635900990730     C                   ELSE
636000990730     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
636100990730     C                   ENDIF
636200990730     C                   ENDIF
636300990730     C                   ENDDO                                                  <-- 1 ---
636400990730     C                   ENDSR
636500990730     C*------------------------------------------------------------------------*
636600990730     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
636700990730     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
636800990730     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
636900990730     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
637000990730     C*------------------------------------------------------------------------*
637100990730     C     OUTFUN        BEGSR
637200990730     C     1             DO        8             I                              *PULIZIA FUNZIONI A
637300990730     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
637400990730     C                   ENDDO
637500990730     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
637600990730     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
637700990730     C     1             DO        31            I
637800990730     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
637900990730     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
638000990730     C                   ADD       1             FUNNAT
638100990730     C                   ENDIF
638200990730     C                   ENDDO
638300990730     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
638400990730     C     FUNNAT        IFGT      8
638500990730     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
638600990730     C                   ENDIF
638700990730     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
638800990730     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
638900990730     C                   Z-ADD     8             FUNVIS            5 0
639000990730     C     FAT(23)       IFEQ      'S'
639100990730     C                   SUB       1             FUNVIS
639200990730     C                   ENDIF
639300990730     C     FAT(24)       IFEQ      'S'
639400990730     C                   SUB       1             FUNVIS
639500990730     C                   ENDIF
639600990730     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
639700990730     C                   Z-ADD     *ZEROS        W                 5 0
639800990730     C                   ADD       1             FUNULT
639900990730     C     FUNULT        IFGT      31
640000990730     C                   Z-ADD     1             FUNULT
640100990730     C                   ENDIF
640200990730     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
640300990730     C     FUNULT        DO        31            I                              --- 1 -->
640400990730     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
640500990730     C     W             IFLT      FUNVIS                                       --- 2 -->
640600990730     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
640700990730     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
640800990730     C     FDE(I)        ANDNE     *BLANKS
640900990730     C     I             IFEQ      23                                           *TOLGO F23
641000990730     C     I             OREQ      24                                           *E F24
641100990730     C                   ELSE
641200990730     C                   ADD       1             W
641300990730     C                   MOVEL     FDE(I)        FUV(W)
641400990730     C                   ENDIF
641500990730     C                   ENDIF                                                  <-- 3 ---
641600990730     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
641700990730     C                   Z-ADD     I             FUNULT
641800990730     C                   ENDIF                                                  <-- 2 ---
641900990730     C                   ENDDO                                                  <-- 1 ---
642000990730     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
642100990730     C                   MOVEL     FUV(1)        VFD001
642200990730     C                   MOVEL     FUV(2)        VFD002
642300990730     C                   MOVEL     FUV(3)        VFD003
642400990730     C                   MOVEL     FUV(4)        VFD004
642500990730     C                   MOVEL     FUV(5)        VFD005
642600990730     C                   MOVEL     FUV(6)        VFD006
642700990730     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
642800990730     C     FAT(24)       ANDEQ     'N'
642900990730     C                   MOVEL     FUV(7)        VFD007
643000990730     C                   MOVEL     FUV(8)        VFD008
643100990730     C                   ELSE                                                   <-- 1 -->
643200990730     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
643300990730     C     FAT(24)       ANDEQ     'S'
643400990730     C                   MOVEL     FDE(23)       VFD007
643500990730     C                   MOVEL     FDE(24)       VFD008
643600990730     C                   ELSE                                                   <-- 2 -->
643700990730     C                   MOVEL     FUV(7)        VFD007
643800990730     C     FAT(23)       IFEQ      'S'
643900990730     C                   MOVEL     FDE(23)       VFD008
644000990730     C                   ELSE
644100990730     C                   MOVEL     FDE(24)       VFD008
644200990730     C                   ENDIF
644300990730     C                   ENDIF                                                  <-- 2 ---
644400990730     C                   ENDIF                                                  <-- 1 ---
644500990730     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
644600990730     C                   ENDSR
644700990730     C*------------------------------------------------------------------------*
644800990730     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
644900990730     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
645000990730     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
645100990730     C*                   - ERRORE SE FUNZIONE NON ATTIVA
645200990730     C*------------------------------------------------------------------------*
645300990730     C     CONFUN        BEGSR
645400990730     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
645500990730     C     1             DO        31            I                              --- 1 -->
645600990730     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
645700990730     C* CONTROLLO SE IL TASTO E' ATTIVO
645800990730     C     FAT(I)        IFNE      'S'                                          --- 3 -->
645900990730     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
646000990730     C                   EXSR      CARMSG
646100990730     C                   MOVEL     '1'           $ERR
646200990730     C                   ENDIF                                                  <-- 3 ---
646300990730     C                   ENDIF                                                  <-- 2 ---
646400990730     C                   ENDDO                                                  <-- 1 ---
646500990730     C                   ENDSR
646600020516      *****************************************************************
646700020516      * ROUTINE INIZIALE
646800020516      *****************************************************************
646900020516     C     *INZSR        BEGSR
647000020516      *
647100020516      *-- Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
647200020516     c     *dtaara       define    §azute        azuteds
647300020516     c     *dtaara       define    §datiute      ddatiute
647400020516      *
647500020516     c                   in(E)     *dtaara
647600020516if  1c                   IF        %Error  or  RSUT = *blanks
647700020516     c                   call      'TIBS34R'
647800020516     c                   parm                    Tibs34Ds
647900020516     c                   in        *dtaara
648000020516e   1c                   ENDIF
648100020516      *
648200020516     C                   ENDSR
648300990730     C*------------------------------------------------------------------------*
648400990730      * AGGIORNO IL TIPO BOLLA SUL CONTRASSEGNO
648500990730     OTNCSB000  E            AGGCSB
648600990730     O                       CSBTBL
648700990730     C*------------------------------------------------------------------------*
648800990730** ======== SCHIERA: MSG  (MESSAGGI)                       ================
648900990730TNSB52R- Tasto funzionale non valido.                                          1
649000990730TNSB52R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
649100990730TNSB52R- Opzione non valida.                                                   3
649200990730TNSB52R- Opzione abilitata ma non gestita. Avvertire CED.                      4
649300990730TNSB52R- Inizio elenco.                                                        5
649400990730TNSB52R- Fine elenco.                                                          6
649500990730TNSB52R- Immettere tipo bolla esistente e non annullato                        7
649600120706TNSB52R- Non e' possibile trasformare la bolla in un recupero e viceversa      8
649700990730TNSB52R- Non e' possibile trasformare il tipo porto franco/assegnato           9
649800990730TNSB52R- Per i clienti non codificati occore immettere i dati necessari       10
649900130219TNSB52R- Immettere codice cliente valido e non bloccato                       11
650000990730TNSB52R- Immettere la ragione sociale                                         12
650100990730TNSB52R- Immettere il C.A.P.                                                  13
650200990730TNSB52R- Immettere consegna particolare valida                                14
650300990730TNSB52R- Immettere data formalmente valida                                    15
650400990730TNSB52R- Spedizione non manutenzionabile. Inserita nel file storico           16
650500990730TNSB52R- Inserimento effettuato.                                              17
650600990730TNSB52R- Modifica effettuata.                                                 18
650700990730TNSB52R- Cancellazione effettuata.                                            19
650800990730TNSB52R- Ripristino effettuato.                                               20
650900990826TNSB52R- Bolla non trovata: e' stato appena modificato il tipo bolla          21
651000990730TNSB52R- Registrazione momentaneamente allocata: riprovare piu' tardi         22
651100990730TNSB52R- Immettere codice tariffa valido                                      23
651200990805TNSB52R- Data fattura non valida                                              24
651300990730TNSB52R- Immettere tipo servizio valido                                       25
651400990730TNSB52R- Per cli.non codificato, solo cod.tariffa di tariffa di cartello (   )26
651500990730TNSB52R- Se cambiata la divisa, variare (o riscrivere) anche l'importo        27
651600990730TNSB52R- Immettere divisa valida                                              28
651700990730TNSB52R- Se immessa la divisa immettere anche l'importo                       29
651800990730TNSB52R- Immettere codice nazione valido                                      30
651900990730TNSB52R- Immettere consegne particolari differenti                            31
652000990730TNSB52R- Il codice mittente non puo' essere generico                          32
652100990730TNSB52R- Immettere codice esenzione I.V.A. valido                             33
652200990730TNSB52R- Non possono coesistere l'esenzione I.V.A. e l'importo I.V.A.         34
652300990730TNSB52R- Immettere codice varia valido                                        35
652400990730TNSB52R- Se immesso l'importo occorre immettere anche il codice               36
652500990730TNSB52R- Non possibile il calcolo tassazione se campo imponibile pieno        37
652600990730TNSB52R- Tassazione non possibile a causa di errori                           38
652700990730TNSB52R- C.A.P. non corrispondente all'anagrafica cliente. INVIO=Continua     39
652800991104TNSB52R- Numero decimali non corretto per la divisa indicata                  40
652900110624TNSB52R- Anteporto errato o non utilizzabile                                  41
653000110624TNSB52R- Tipo inoltro errato o non utilizzabile                               42
653100990730TNSB52R- Bolla non manutenzionabile: di competenza di                         43
653200990826TISI95R- Versione cappario non trovata.                                       44
653300990826TISI95R- Per fermi deposito inoltro dovrebbe essere C-citta'. ENTER forza     45
653400991104TNSB52R- Prepagati:cliente non codificato o senza tariffa obbligo divisa xxx  46
653500991104TNSB52R- Prepagati:utilizzare la divisa della tariffa del cliente, xxx        47
653600991104TNSB52R- Prepagati:inserire tassazione fino all'imponibile                    48
653700011023TNSB52R- Divisa non trovata sul file. AVVISARE CED!!                          49
653800011024TNSB52R- Manca la varia 'N'                                                   50
653900011106TNSB52R- Divisa obbligatoria                                                  51
654000011106TNSB52R- Divisa scaduta                                                       52
654100011129TNSB52R- Importo da assicurare non ammesso                                    53
654200060220TNSB52R- Importo da assicurare INFERIORE a quanto previsto dalla legge        54
654300011205TNSB52R- Gli imp.inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx  55
654400011210TNSB52R- Importo da Ass. > di xxxxxxxxxx,xxx EUR:VERIFICARE. ENTER X FORZARE  56
654500011210TNSB52R- Importo da Ass.> del limite imposto xxxxxxxxxx,xxx EUR.AVVISARE CED  57
654600020516TNSB52R- Profilo non abilitato alla funzione                                  58
654700020516TNSB52R- Spedizione non modificabile: gia' fatturata                          59
654800020516TNSB52R- Spedizione non modificabile: cliente xxxxxxx non di competenza       60
654900020731TNSB52R- Non è possibile indicare insieme consegne particolari 'S' e 'X'      61
655000021122TNSB52R- Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x   62
655100021122TNSB52R- Bolla con Tar.FedEx Doc.e peso > Kg. xxxxxx,x: ENTER X FORZARE       63
655200030218TNSB52R- Non è possibile modificare il tipo bolla                             64
655300030331TNSB52R- Eliminato il numero fattura, verificare la tassazione!!              65
655400030416TNSB52R- Modifica effettuabile presso il p.o. di arrivo                       66
655500040128TNSB52R- Sped."monovaria". No altre varie se non immessa la varia " "         67
655600040930TNSB52R- codice cliente inutilizzabile                                        68
655700060706TNSB52R- Impossibile trasformare tipo bolla per franchi prepagati             69
655800071106TNSB52R- Immettere il Codice Fiscale. Enter x forzare!                        70
655900061107TNSB52R- Codice fiscale formalmente errato                                    71
656000071106TNSB52R- Immettere Partita Iva e codice Fiscale. Enter x forzare!             72
656100120327TNSB52R- Codice cliente non valido o non utilizzabile dall'utente             73
656200120327TNSB52R- Immettere il codice tariffa                                          74
656300120508Immettere Partita Iva o codice Fiscale per cliente Italia. Enter x forzare!   75
656400151109TNSB52R- Immettere Partita Iva. Enter x forzare!                              76
656500120508TNSB52R- P.Iva "$$" non ammessa per fatture emesse in filiale                 77
656600120705Il tipo bolla "xx" si può modificare solo in "                                78
656700120705Non è possibile modificare il tipo bolla per questa spedizione                79
656800120706Tipo bolla non può essere RECUPERO per il cod.trattamento merce della sped.   80
656900120802Tipo bolla deve essere RECUPERO per il cod.trattamento merce della sped.      81
657000120802Non è possibile trasformare il codice cliente in 8888
657100130228Cod.cli BLOCCATO non utilizzab:immettere Varia=0 o modificare il codice cli.  83
657200130228Cod.cli BLOCCATO non utilizzab:stornare la bolla o modificare il codice cli.  84
657300150413Spedizione per EXPO: non ammesso il Contrassegno                              85
657400150414Spedizione per EXPO possibile solo per bolle Franco                           86
657500150414Spedizione per EXPO non ammessa per la linea di arrivo                        87
657600990730** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
657700990730F1    -                   -    NN     NN
657800990730F2    -                   -    NN     NN
657900990730F3    -F3=Fine            -    SS     SS
658000990730F4    -                   -    NN     NN
658100990730F5    -                   -    NN     NN
658200020724F6    -F6=Aggiornamento   -    SS     SS
658300030401F7    -F7=Int.Bolla       -    SS     SS
658400990730F8    -                   -    NN     NN
658500990730F9    -                   -    NN     NN
658600990730F10   -F10=Variazioni sede-    SS     SS
658700990730F11   -                   -    NN     NN
658800990730F12   -F12=Ritorno        -    SS     SS
658900990730F13   -                   -    NN     NN
659000020724F14   -F14=Tassazione     -    SS     SS
659100120328F15   -F15=Tass.Altra Tar -    NN     NN
659200990730F16   -F16=Competenze C/E -    SS     SS
659300990730F17   -                   -    NN     NN
659400990730F18   -F18=Legami         -    SS     SS
659500990730F19   -                   -    NN     NN
659600990730F20   -                   -    NN     NN
659700990730F21   -                   -    NN     NN
659800990730F22   -F22=Contrassegno   -    SS     SS
659900990730F23   -                   -    NN     NN
660000990730F24   -F24=Altri tasti    -    NN     NN
660100990730ENTER -                   -    SS     SS
660200990730ROLLUP-                   -    SS     SS
660300990730ROLLDN-                   -    SS     SS
660400990730HELP  -                   -    NN     NN
660500990730PRINT -                   -    NN     NN
660600990730BSPACE-                   -    NN     NN
660700990730CLEAR -                   -    NN     NN
