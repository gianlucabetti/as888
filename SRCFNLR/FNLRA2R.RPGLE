000100090428     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO) bnddir('TIBS')
000200090428     h DFTACTGRP(*NO)
000300050317     H* FNLRA2R *----------------------------------------------------*
000400050309     H* - TASSAZIONE PORTI ASSEGNATI IN ARRIVO E IN PARTENZA
000500000000     H*--------------------------------------------------------------*
000600050317     FFNLRA2D   CF   E             WORKSTN
000700900517     F                                     INFDS(CA02DS)
000800050314     F                                     maxdev(*file)
000900050317     F                                     SFILE(LRA2SS2:NRRs2)
001000050317     F                                     SFILE(LRA2SC2:NRRc2)
001100050317     F                                     SFILE(LRA2S06:NRR)
001200020911     FFNFVV01L  IF   E           K DISK
001300911028     FTABEL00F  IF   E           K DISK
001400940225     FCNNUM00F  UF   E           K DISK    USROPN
001500911028     FAZORG01L  IF   E           K DISK
001600941216     FTNTAM01L  IF   E           K DISK    USROPN
001700990817     FTITPT01L  IF   E           K DISK    USROPN
001800030506     fTfntc01l  if   e           k Disk    Usropn
001900050309     F** Tassazione P.A. in arrivo
002000941217     FFNARB01L  UF   E           K DISK
002100051115     FFiAR901L  iF   E           K DISK
002200990907     FFIAR601L  UF A E           K DISK
002300990907     FFIAR701L  UF A E           K DISK
002400061107     FFiAR401L  uF a E           K DISK
002500171030     Ffipnd01l  iF   E           K DISK
002600050110     FFNARB07L  IF   E           K DISK    RENAME(FNARB000:FNARB007)
002700050309     c* Tassazione P.A. in partenza
002800050309     FFNBLP00f  uF   E             DISK    rename(fnblp000:fnblpfis)
002900050309     f                                     prefix(ARB:3)
003000050310     FFNblp07L  iF   E           K DISK    RENAME(FNblp000:FNblp007)
003100050309     f                                     prefix(ARB:3)
003200050309     F                                     INFDS(blp07)
003300050309     f**
003400020911     fFnlbl01l  if   e           k Disk
003500040513     fFnblp01l  uf   e           k Disk
003600050309     F                                     INFDS(blp01)
003700071001     fFiar501l  if A e           k Disk
003800151013     f*Tita430c  uf A e           k disk    usropn
003900071001     fTitas30c  if   e           k disk    usropn
004000071001     f                                     prefix(S_)
004100151013     fFiar531c  uf A e           k Disk    usropn
004200151013     f                                     rename(fiar5000:fiar500S)
004300151013     f                                     rename(fiar5p00:fiar5p0S)
004400030613      **
004500990907     FFIARBT0F  O  A E           K DISK    USROPN
004600170116     FFIARBT2T  O  A E           K DISK    USROPN RENAME(FIARBT00:FIARBTTT)
004700941217     F**
004800990907     FFIARBU0F  O  A E           K DISK    USROPN
004900170116     FFIARBU2T  O  A E           K DISK    USROPN RENAME(FIARBU00:FIARBUTT)
005000030722     F**
005100030613
005200050309     D blp07           DS
005300050309     D  blp07nrr             397    400B 0
005400050309     D blp01           DS
005500050309     D  blp01nrr             397    400B 0
005600060123     D** L1              S              3  0 DIM(30)                            FIL DELL'AS
005700920319     D L6              S              3  0 DIM(30)                              FIL GESTITE
005800050523     d
005900941217     D K78             S              1    DIM(78)                              COMODO
006000941217     D* VARIE DI COMODO
006100941217     D WSV             S              1    DIM(21)                              SIGLA  VARIE
006200990907     D WVA             S             11  3 DIM(21)                              VALORE VARIE
006300050113     D*
006400150120     D SVA             S              1    DIM(100)                             VARIE IN TOT.I
006500050113     D*
006600050127     D* CODICI ASSEGNATI PREIMPOSTATI SE C'E' TASSAz MULTIPLA DI COMODO
006700050125     D TAS9KC          S             10    DIM(20)                              SIGLA  VARIE
006800050125     D TAS9K           S              7    DIM(20)                              SIGLA  VARIE
006900050125     D TAS9Nk          S              7  0 DIM(20)                              VALORE VARIE
007000050125     D TAS9N           S              7  0 DIM(20)                              VALORE VARIE
007100050112     D TAS9T           S              1    DIM(3)                               VALORE VARIE
007200050128     D* schiere cod.tariffa del cliente
007300170103     D ctrw            S              3    DIM(16)                              VARIE IN TOT.I
007400170103     D fiew            S              1    DIM(16)                              VARIE IN TOT.I
007500170103     D tspw            S              1    DIM(16)                              VARIE IN TOT.I
007600170103     D bapw            S              1    DIM(16)                              VARIE IN TOT.I
007700170103     D tfpw            S              1    DIM(16)                              VARIE IN TOT.I
007800170103     D valw            S              1    DIM(16)                              VARIE IN TOT.I
007900050113     D* schiere del sfl
008000990909     D*
008100050113     D Sksflcon        S              1    DIM(9902)                            VARIE IN TOT.I
008200050113     D Sksflsen        S              1    DIM(9902)                            VARIE IN TOT.I
008300050113     D Skiopz          S              1    DIM(9902)                            VARIE IN TOT.I
008400911218     D*
008500020805     D SOVR            S             48    DIM(1)                               DI COMODO SED
008600020805     D SALC            S             48    DIM(1)                               DI COMODO SED
008700020805     D SDLC            S             48    DIM(1)                               DI COMODO SED
008800050126     d
008900050309     D TBSTO           S              2    DIM(25)
009000050309     D CBOASS          S              2    DIM(25)
009100050309     D CBOASS2         S              2    DIM(25)
009200050309     d cborec          s              2    dim(25)                              sk cbo recupero
009300020715     d tbl             s              2    dim(20)                              sk tbl con ass.
009400020703
009500941217     D*
009600941217     D TAS             S             22    DIM(4) CTDATA PERRCD(1)              DESCRIZ TASSAZ
009700050310     D TES             S             39    DIM(3) CTDATA PERRCD(1)              TESTATE
009800941217     D*
009900170116     D*C20             S              1    DIM(48) CTDATA PERRCD(48)
010000170116     D*C21             S              1    DIM(48) CTDATA PERRCD(48)
010100170116     D*C22             S              1    DIM(48) CTDATA PERRCD(48)
010200030613      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
010300030613     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
010400030613     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
010500071001     d OVR             s             60    dim(02) ctdata perrcd(1)
010600051024     D MSG             S             78    DIM(84) CTDATA PERRCD(1)
010700011127
010800011220     d sav_geimf       s                   like(§geimf)
010900011203     d w0173           s             17  3
011000011127     d tot_importi     s             17  3
011100011128     d forza           s              1
011200011203     d sav_vidias      s                   like(vidias)
011300040513     d sv1_vidias      s                   like(vidias)
011400040513     d sv1_vidvas      s                   like(vidvas)
011500040513     d sv1_vidqft      s                   like(vidqft)
011600020222     d lnpntw          s              3
011700020222     d lnantw          s              3
011800020222     d clintw          s              3
011900050125     d wcodtar         s              3
012000050125     d wcodtar1        s                   Like(wcodtar)
012100050125     d wcodtar2        s                   Like(wcodtar)
012200050125     d w003a           s                   Like(wcodtar)
012300020805     d $mbr            s              6
012400020911     d Kfgs            s                   Like(FvvFgs)
012500021121     d DueCtr          s              2  0
012600021121     d Wctr            s                   Like(VidCtr)
012700030708     d kAr5Trd         s                   Like(Ar5Trd)
012800030506     d kNtcApl         s                   Like(NtcApl) Inz('C')
012900030506     d kNtcNk1         s                   Like(NtcNk1)
013000030506     d kNtcNk2         s                   Like(NtcNk2)
013100030506     d KntcTnt         s                   Like(NtcTnt) Inz('DC')
013200030506     d sav_vidksc      s                   Like(vidksc)
013300030605     d sav_indsin      s                   Like(indsin)
013400050126     d wtas9rec        s                   Like(rec)
013500050126     d Wtas9ntw        s                   Like(§ogntw)
013600050203     d Wtas9sva        s                   Like(§3asva)
013700050112     d wforzatas9      s              1
013800050112     d wcontabolle     s              7  0
013900050112     d wsalvaconta     s                   like(wcontabolle)
014000050112     d wsflpieno2      s              1
014100050110     d wsflpieno22     s              1
014200050110     d wtiposfl        s              1
014300050111     d wscelta         s              1
014400110629     d wemd            s              1
014500150107     d wpincode        s              1
014600040119     d va2va2          s                   Like(Ar6Va1)
014700040119     d va2va3          s                   Like(Ar6Va1)
014800040119     d va2va4          s                   Like(Ar6Va1)
014900041129     d va2va5          s                   Like(Ar6Va1)
015000041129     d va2va6          s                   Like(Ar6Va1)
015100040119     d war61           s              1
015200040119     d war71           s              1
015300050310     d wesistear6      s              1
015400050310     d wesistear7      s              1
015500050310     d wtiporecar6     s                   like(ar6trc)
015600050121     d WERR            s              2
015700050318     d errmult         s              3
015800050203     d Waggerr         s              1
015900050209     d Wusaaca         s              1
016000050314     d Wscarta         s              1
016100050128     d aa              s              3  0
016200061107     d w0040           s              4  0
016300040119     d contasv         s              2  0
016400040119     d totimv          s                   Like(Ar6Imv)
016500061107     d wcdfis          s                   Like(arbcpi)
016600071030     d wpariva         s                   Like(arbcpi)
016700040119     d wesvar1         s              1
016800040120     d wvar3ai         s              1
016900060123     d wVaria          s              1
017000071001     d wlib            s             10
017100020117
017200100120     d Posiz           s              2  0
017300100120     d c               s              3  0
017400050309     d XX              s              3  0
017500050309     d II              s              3  0
017600050309     d ZZ              s              3  0
017700050201     d Wsfact          s                   Like(vsfact)
017800070924     d Savimv          s                   Like(ar6imv)
017900070924     d PesVolpkcn      s                   Like(Taspkcn)
018000070924     d PesVolSpc       s                   Like(TasSpc)
018100070924     d PesVolIMV       s                   Like(Ar6IMV)
018200070924     d Tassatoimv      s                   Like(Ar6IMV)
018300011127
018400921130     D*
018500050309     D* PASSO A INTERROGAZIONE BOLLE ARRIVI              - FNLR36R -
018600161102     D*PARAM           DS
018700161102     D* GIA                    1     12  0
018800161102     D* RICH                  13     13
018900161102     D* VsfAAS                14     17  0
019000161102     D* VsfLNP                18     20  0
019100161102     D* VsfNRS                21     22  0
019200161102     D* VsfNSP                23     29  0
019300161102     D* PARFLG                31     31
019400050309     D* RICHIAMO PGM INTERROGAZ.BOLLE PARTENZE            - FNLS04R -
019500161102     D*PARAM3          DS
019600161102     D* PA3AAS                 1      4  0
019700161102     D* PA3LNP                 5      7  0
019800161102     D* PA3NRS                 8      9  0
019900161102     D* PA3NSP                10     16  0
020000161102     D* PA3FL2                18     18
020100161102     D* PA3RSU                23     42
020200161102     D* PA3FLG                31     31
020300161103
020400161103     d fnlru6ds      e DS
020500161103
020600161102     D* RICHIAMO PGM INTERROGAZ.BOLLE unica               - FNLRU6R -
020700161102     D PARAMu6ds       DS
020800161102     d* CAMPI DI OUTPUT:
020900161102     d* . Key spedizione
021000161102     D  pa1AAS                14     17  0
021100161102     D  pa1LNP                18     20  0
021200161102     D  pa1NRS                21     22  0
021300161102     D  pa1NSP                23     29  0
021400161102     d* . Flag '1'=Premuto F3=Fine
021500161102     D  PA1F03                30     30
021600161102     d* CAMPI DI INPUT:
021700161102     d* . Modalità di richiamo
021800161102     D  PA1FLG                31     31
021900161102     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F14=Giac.
022000161102     d*                                in interrogazione della bolla ed evitare così
022100161102     d*                                errore di chiamata ricorsiva
022200161102     D  PA1GIA               144    144
022300161102     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
022400161102     d  pa1rma               149    163
022500161102     d  pa1xco               164    164
022600161102     d* non utilizzato
022700161102     d  pa1flo               165    256
022800161102     D PARAMU6ds1      DS
022900161102     d** INPUT
023000161102     D  pa1AAS_               14     17  0
023100161102     D  pa1LNP_               18     20  0
023200161102     D  pa1NRS_               21     22  0
023300161102     D  pa1NSP_               23     29  0
023400161102     d** OUTPUT
023500161102     D  PA1F03_               30     30
023600161102     d** INPUT
023700161102     D  PA1FLG_               31     31
023800161102     D*    SE RICH DA PGM GIAC. = 1
023900161102     D  PA1GIA_              144    144
024000161102     d** OUTPUT
024100161102     d  pa1rma_              149    163                                         x F22=PerfectOrder
024200161102     d  pa1xco_              164    164                                         x F22=PerfectOrder
024300161102     d** INPUT
024400161102     d*   Tipo lancio: "C" -           CHIUSO CON LR
024500161102     d*                "L" - ELABORO E CHIUDO CON LR
024600161102     d*                " " - ELABORO E CHIUDO IN RETRN
024700161102     d  partla               165    165
024800161102     d  parflo               166    256
024900921130      *
025000941220     D                 DS
025100941220     D  EMME                   1      1    INZ('M')
025200020805     d  $Simfel                2      4  0
025300020805     D  PARMBR                 5      7  0
025400941220     D  MBRFIS                 1     10
025500941216     D***
025600941216     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
025700941216     D***
025800941216     D WLBDAT          DS                  INZ
025900941216     D  G02DAT                 1      8  0
026000941216     D  G02INV                 9     16  0
026100941216     D  G02ERR                17     17
026200941216     D  G02TGI                18     22  0
026300900516     D MMGG            DS
026400900516     D  MM                     1      2  0
026500900516     D  GG                     3      4  0
026600901116     D                 DS
026700911025     D  VIDLNP                 1      3  0
026800911025     D  VIDNRS                 4      5  0
026900911025     D  VIDNSP                 6     12  0
027000911025     D  SEL1                   1     12  0
027100911028     D                 DS
027200941216     D  ARBAAS                 1      4  0
027300941216     D  ARBMGS                 5      8  0
027400941216     D  ARBDSP                 1      8  0
027500900517     D CA02DS          DS
027600900517     D  PRIMA                378    379B 0
027700911024     D KPJBA         E DS
027800930209     D  LIB                   92    101
027900941222     D DSARBD        E DS
028000941222     D DSARBG        E DS
028100990907     D DARBT         E DS
028200941222     D DSARBK        E DS
028300941217     D DSCV          E DS
028400941217     D DSCC          E DS
028500941217     D DS15          E DS
028600930211     D DS3A          E DS
028700030217     d ds3aw         e ds                  Extname(ds3a)
028800030217     d                                     Prefix(w_)
028900020715     d dstb          e ds
029000990907     D DGEI          E DS
029100990915     D DGED          E DS
029200020527     D DSBL4A        E DS
029300151127     D DSBL4m        E DS
029400151013     D Dar5fat       E DS
029500151127     D Dar5gen       E DS
029600990923     D DS$2          E DS
029700060529     D*DSBL4F        E DS
029800171030     D*DSBL4I        E DS
029900171030     D dpnddp4       E DS
030000911211     D DS7L          E DS
030100150108     D DS7R          E DS
030200020725     D DYIV          E DS
030300000202     D OG143         E DS
030400070314     D OG148         E DS
030500021121     d Ddfe          e ds
030600050714     d DVPO          e ds
030700991028     D DSTA01        E DS
030800160927     D DTAS          E DS                  inz
030900060925      ** DS Flag operativi DS DTAS
031000060925     d Dtas01        e ds
031100060925
031200990907     D DTASV         E DS
031300030613     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
031400030613     D  VVA                   21    140P 3 DIM(20)                              VALORE VARIE
031500030613     D  VES                  141    160    DIM(20)                              ESENZ  VARIE
031600011002      *
031700011002     D DSCW          E DS
031800090604     d ds5e          e ds
031900011002      *
032000990907     D* DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
032100990907     D DSLV04        E DS                  EXTNAME(FNLV04DS)
032200941216     D*
032300990907     D* DS PER FNLV16R - CONTROLLO TASSAZIONE
032400990907     D DSLV16        E DS                  EXTNAME(FNLV16DS)
032500040119     d dlv1601       e ds
032600941216     D*
032700060217     D* DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
032800941216     D DSLV19        E DS                  EXTNAME(FNLV19DS)
032900941216     D*
033000050128     D* DS PER FNLV50R - controllo p.o. gestione
033100941217     D DSLV50        E DS                  EXTNAME(FNLV50DS)
033200941217     D*
033300050128     D* DS PER FNLV59R - nuovo pgm di CARICAMENTO E CONTROLLO CODICI TARIFFA
033400050128     D fnlv59ds      E DS
033500941216     D*
033600971117     D* DS PER FNLV53R - CARICAMENTO DATI DI UN FOGLIO
033700971117     D DSLV53        E DS                  EXTNAME(FNLV53DS)
033800971117     D*
033900030718     D* DS PER stampa LDV
034000130320     D*fnlsb5DS      E DS
034100030613      *
034200050714     D* DS controllo se bolla dirottata
034300050714     D FNLV58DS      E DS
034400060123     D wFnlv58DS     E DS                  extname(fnlv58ds) prefix(W)
034500060317     d* Ds richiesta/aggiornamento motivo variazione
034600060317     D FNLV85DS      E DS
034700050714      *
034800030613      * DS RICHIESTA STAMPANTI ricevuta in input
034900030613     D Trul90DS      E DS
035000050519      * DS per controllo riempimento schiera
035100050519     D Trul0sDS      E DS
035200030613      * DS PER TRUL90R - RICHIESTA STAMPANTI eseguita dal pgm
035300030613     D WTrul90DS     E DS                  ExtName(Trul90DS)  Prefix(W)
035400030613      *
035500030613      * DS PER TISI95R - CONTROLLO CAP
035600970710     D DSSI95        E DS                  EXTNAME(TISI95DS)
035700970710     D DSLV13        E DS                  EXTNAME(FNLV13DS)
035800941217      *
035900050804     D PARAMT          DS
036000050804     D  PARCA                256    256
036100050804      *
036200941216     D* DS PER TRUL06R - CARICAMENTO £X
036300941216     D DSUL06        E DS                  EXTNAME(TRUL06DS)
036400941216     D  LIN                    1     90  0
036500941216     D                                     DIM(30)                              SKI COMODO
036600050317     D* DS PER FNLRA2R - DATI DI ENTRATA
036700050317     D DSLRA2        E DS                  EXTNAME(FNLRA2DS)
036800941216      *
036900941216     D* DS PER FNLR48R - VARIAZIONE BOLLE
037000941216     D DSLR48        E DS                  EXTNAME(FNLR48DS)
037100941216      *
037200941216     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
037300941216     D DSLR66        E DS                  EXTNAME(FNLR66DS)
037400990914     D* DS PER YEURCO - EURO CONVERTITORE
037500990914     D DSYEUR        E DS                  EXTNAME(YEURCODS)
037600991117     D* DS PER TIBS02R - RICERCA TABELLA
037700991117     D DSBS02        E DS                  EXTNAME(TIBS02DS)
037800061107     D* DS PER YCO602R - RICERCA codice fiscale
037900061107     D YCO600DS      E DS
038000011127
038100020222     d trul22ds      e ds
038200160226     d trul27ds1     e ds
038300160226     d trulc7ds      e ds
038400030203     d dAr5Ban       e ds
038500030708     d dAr5Bnb       e ds
038600130924     d dAr5emd       e ds
038700030506
038800030506     D TIBS69DS      E DS
038900030506     D ds_cnaco      E DS                  extname(CNACO00F)
039000030506     D ds_cnind      E DS                  extname(CNIND00F)
039100030506     D ds_cnclp      E DS                  extname(CNCLP00F)
039200030506     D ds_fncls      E DS                  extname(FNCLS00F)
039300130416      * DS PER FIDG42R -
039400131119     D*Fidg42ds      E DS
039500981116      *
039600911028     D CNCR80        E DS
039700911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
039800000000     D  TCU                  398    697
039900000000     D                                     DIM(50)                              TAB FLAG CNT
040000000000     D  KCU                  698    847P 0
040100000000     D                                     DIM(50)                              TAB CAPO CONTO
040200000000     D                                     PACKEVEN
040300911028     D TCUDS           DS
040400911028     D  F1                     1      1
040500911028     D  F3                     3      3
040600911028     D  F2                     2      2
040700911028     D  F4                     4      4
040800911028     D  F56                    5      6
040900011128
041000011128     D                SDS
041100011128     D  NOMPGM                 1     10
041200090428
041300090428     d
041400090428     d  pjsocieta      s              3
041500090428     d  pjkcc          s              6
041600090428     d  pjksc          s              8
041700090428     d  pjesito        s             10I 0
041800100119     d
041900100119     D                 DS
042000100119     D  VIDISD                 1      2
042100100119     D  VIDCPD                 3     16
042200100119     D  VIDPID                 1     16
042300100119     D                 DS
042400100119     D  WISO                   1      2
042500100119     D  WCPI                   3     16
042600100119     D  WPIVA                  1     16
042700090428     d
042800090428     D chkContoIncassiAttribuire...
042900090428     D                 PR            10i 0
043000090428     D  pjsocieta                     3
043100090428     D  pjkcc                         6
043200090428     D  pjksc                         8     const
043300090428     D  pjesito                      10I 0
043400020725
043500020725      * Costanti
043600020725      * - Società chiave per tabella "YIV"
043700020725     D C_Soc           c                   const('201')
043800060117     D C_dirotta       c                   const('DIROTTATA, partita da')
043900060117     D C_cambio        c                   const('CAMBIO PORTO,parte da')
044000060117     D C_legame        c                   const('Bolla LEGATA,parte da')
044100071001     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
044200151013     I*TITA4000      92
044300151013     I*TITA4P00      93
044400151013     Ifiar500S      92
044500151013     Ifiar5p0s      93
044600071001     ITITAs000      92
044700071001     ITITAs010      92
044800071001     ITITAsP00      93
044900011128
045000900518     C****************************************************************
045100900518     C*  RIEPILOGO INDICATORI
045200900518     C***************************************************************
045300050121     C* 01    - F1  SEGUE FATTURA
045400960302     C* 02    - VISULIZZA BENE LA NATURA MERCE
045500120208     C* 03    - Abilitazione stampa DDT con F8
045600050112     C* 04    - TASSAZIONE MULTIPLA: proteggo campi nel video
045700911029     C* 05    - DOPPIA BOLLA CON ESTENSIONE
045800950421     C* 06    - PROTEGGO Q.TA' DA FATTURARE
045900990913     C* 07    - PROTEGGO DIVISA TASSAZIONE
046000030729     C* 08    - Visualizzo Colli Originali
046100941217     C* 09    - IMMESSA ALMENO UNA VARIA
046200050314     C* 10    - proteggo i campi del D03 quanndo lo emetto insieme al d04
046300911211     C* 11    - PGM RICHIAMATO
046400911211     C* 12    - PGM RICHIAMATO - RITASSAZIONE PORTI ASSEGNATI
046500050216     C* 13    - metto in HI le bolle sfl senza codice con codice "S"
046600050126     C* 14    - Visualizzo raggruppamento se c'e'
046700010703     C* 15    - VISUALIZZAZIONE LINEA ARRIVO DPD
046800920813     C* 16    - CAMBIO FILIALE
046900050314     C* 17    - p.o. 2 livello
047000930430     C* 18    - POSIZIONAMENTO CURSORE SU SUBFILE
047100930430     C* 19    - CAMPO SCELTA PROTETTO E NON VISUALIZZATO SU SUBFILE
047200050110     C* 20    - sfldsp su lsa1s02
047300050110     C* 21    - sfldsp su lsa1s22
047400050715     C* 22    - Bolla dirottata
047500120125     C* 23    -
047600971117     C* 25    - PROTEGGO I CONVENUTI SE IMMESSI DALLA PARTENZA
047700050317     C* 27    - SFLDSP E SFL CLR SU LRA2C06
047800941217     C* 28    - EMISSIONE VIDMSG
047900050114     C* 29    - CLIENTE SENZA TARIFFE
048000941220     C* 30    - LETTURA FNARB
048100941220     C* 31/33 - COMODO
048200911023     C* 34    - LETTURA SUBFILE
048300911023     C* 35    - PULIZIA SUBFILE
048400911111     C* 36    - SFLNEXCHG
048500050314     C* 38    - LETTURA FnAR600F
048600941220     C* 39    - COMODO
048700950104     C* 40-56 - POSIZIONAMENTO CURSORE PER ERRORI VIDMSG
048800950104     C* 58    - ERRORE: NON TROVATI ASSEGNATI O C/A DA TASSARE/ABILIT
048900071001     C* 70-83 - ERRORI ANCORA CON VIDMSG
049000900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
049100930430     C* 91    - NON E' POSSIBILE ESEGUIRE VARIAZ. MEMBRO ALLOCATO
049200151013     C* 92-93 - indicatori di lettura file fiar531c
049300050309     C* 94    - ON - TASSAZIONE PORTI ASSEGNATI IN PARTENZA
049400050314     C* 95    - ALLOCAZIONE record FILE
049500981008     C* 96    - OBBLIGATORIO TASSARE CON TARIFFA DI CARTELLO
049600981008     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
049700941217     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
049800960227     C* 99    - BOLLA LOCALE
049900900518     C*****************************************************************
050000000000     C     *ENTRY        PLIST
050100000000     C                   PARM                    KPJBA
050200030613     C                   PARM                    TRUL90DS
050300030613      *
050400030613      * Il numero dei parametri ricevuti è testato, in caso di modifica
050500030613      *  ricercare la stringa "%PARMS"
050600070703     C                   IF        %PARMS = 2 and d90psl<>*blanks
050700030613     C                   movel     TRUL90DS      WTRUL90DS
050800030613     c                   ENDIF
050900030613      *
051000050317     C                   MOVEL     KPJBU         DSLRA2
051100940223     C*---------------------------------------------------------------*
051200130320     C***  WFINE         IFEQ      '1'
051300130320     C***                GOTO      FINE
051400130320     C***                ENDIF
051500941216     C*
051600050317     C* TIPO LANCIO, ILRA2TLA "C" -           CHIUSO CON LR
051700050317     C* TIPO LANCIO, ILRA2TLA "L" - ELABORO E CHIUDO CON LR
051800050317     C* TIPO LANCIO, ILRA2TLA " " - ELABORO E CHIUDO IN RETRN
051900941216     C*
052000050317    1C     ILRA2TLA      IFNE      'C'
052100070319     c
052200070319     C* SE pgm richiamato e cambiata filiale in gestione, ricarico i dati
052300070319     c                   if        ilra2fgs>0 and ilra2fgs<>vidfil
052400070319     c                   z-add     ilra2fgs      vidfil
052500070319     c                   exsr      DECOfgs
052600070319     c                   endif
052700070319     c
052800941227     C* VEDO SE PASSATA LA KEY SPEDIZIONE
052900050317     C     ILRA2NSP      IFGT      0
053000941227     C                   SETON                                        11
053100050317     C                   MOVEL     ILRA2AAS      VIDAAS
053200050317     C                   MOVEL     ILRA2LNP      VIDLNP
053300050317     C                   MOVEL     ILRA2NRS      VIDNRS
053400050317     C                   MOVEL     ILRA2NSP      VIDNSP
053500070314     c
053600941227     C                   ELSE
053700941227     C                   SETOFF                                       11
053800941227     C                   ENDIF
053900050309     C** tassazione in arrivo
054000050309     C  n94              MOVE      TES(1)        VIDTES
054100050309     C** tassazione in partenza
054200050309     C   94              MOVE      TES(2)        VIDTES
054300050309     c*
054400050804     C     WAPRE         IFEQ      ' '
054500030506     c                   Open      Tfntc01l
054600990907     C                   OPEN      FIARBT0F
054700170116     C                   OPEN      FIARBT2t
054800990907     C                   OPEN      FIARBU0F
054900170116     C                   OPEN      FIARBU2t
055000941217     C                   OPEN      CNNUM00F
055100941217     C                   OPEN      TNTAM01L
055200990817     C                   OPEN      TITPT01L
055300050804     C                   MOVEL     '1'           WAPRE             1
055400050804     C                   ENDIF
055500941216     C*
055600930422     C*
055700911025     C     INIZIO        TAG
055800050216     c                   clear                   wvisualdue
055900911025     C* PULIZIA CAMPI
056000941217    2C     *IN11         IFEQ      *OFF
056100941216     C                   MOVEL     DATEU         VIDAAS
056200911025     C                   MOVEL     *ZEROS        VIDLNP
056300911025     C                   MOVEL     *ZEROS        VIDNRS
056400911025     C                   MOVEL     *ZEROS        VIDNSP
056500911028     C                   MOVEL     *ZEROS        VIDLP2
056600020703     c                   eval      vidbrc = 'N'
056700911205     C                   MOVEL     'N'           VIDBRN
056800911203     C                   MOVEL     'S'           VIDBAB
056900050110     C                   MOVEL     'S'           VIDsfl
057000050110     c                   clear                   vidrsd
057100050318     c
057200050318     C     WIDMSG        IFNE      *BLANKS
057300050318     C                   MOVEL     WIDMSG        VIDMSG
057400050318     C                   SETON                                        28
057500050318     C                   ENDIF
057600911025     C*
057700900509     C     FOR01         TAG
057800050317     C                   EXFMT     LRA2D01
057900941216     C**
058000050110     C** F3  - FINE LAVORO
058100050318     c                   if        nousa='S'
058200050318     c                   goto      fine
058300050318     c                   endif
058400050318     c
058500900509     C   KC              GOTO      FINE
058600941216     C**
058700941216     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
058800941216     C                   SETOFF                                       4928
058900941216     C**
059000050110     C** F7  - INTERROGAZIONE BOLLE ARRIVI
059100050309     c                   if        *inkg
059200050309     c                   EXSR      INTBOLLE
059300911025     C                   GOTO      FOR01
059400941217    3C                   END
059500941217    2C                   END
059600941216     C**
059700050110     C** F18- CAMBIO FILIALE
059800920813     C   KS              SETON                                        16
059900920813     C   KS              GOTO      FOR01
060000941216     C*
060100050113     C* 16 ON - CONTROLLO FILIALE IN GESTIONE
060200941216     C   16              EXSR      CTRFGS
060300941216     C   16
060400941216     CAN 90              GOTO      FOR01
060500941216     C**
060600900509     C* CONTROLLI FORMATO1
060700900509     C                   EXSR      CONTR
060800170505     C** 90              GOTO      FOR01
060900170505     c                   if        *in90
061000170505     c                   if        not *in11
061100170505     C   90              GOTO      FOR01
061200170505     c                   else
061300170505     c   90              eval      olra2msg='Spedizione allocata da un altro -
061400170505     c                             lavoro'
061500170505     c   90              eval      olra2err='1'
061600170505     c   90              goto      fine
061700170505     c                   endif
061800170505     c                   endif
061900901115     C*
062000911025     C* CARICO SOLO LA SPEDIZIONE SCELTA
062100050112     C                   Z-ADD     0             NRRS2             4 0
062200050112     C                   Z-ADD     0             NRRC2             4 0
062300911025     C*
062400941217    2C     SEL1          IFNE      0
062500940331     C*
062600940331     C* IMPOSTO LA CHIAVE PER CHAIN
062700050112     C                   MOVEL     VIDAAS        VsfAAS
062800050112     C                   MOVEL     VIDLNP        VsfLNP
062900050112     C                   MOVEL     VIDNRS        VsfNRS
063000050112     C                   MOVEL     VIDNSP        VsfNSP
063100050714     C* VERIFICO SE BOLLA dirottata
063200050714     c                   EXSR      CHIAMAlv58
063300050309     c
063400050113     c* Aggancio record a di ar4 per eventuale codice
063500050113     c                   clear                   vsfaks
063600050113     c                   clear                   vsfact
063700050309     c
063800060217     c* Aggancio fiar4 unico per prendere codice di tassazione
063900050113    3c                   if        not *in11
064000050113     C                   MOVEL     'A'           WTRC
064100060217     C     KARB4         CHAIN(N)  Fiar401L                           32
064200050113     C  N32              MOVEL     AR4NOT        DSBL4A
064300050113     C   32              CLEAR                   DSBL4A
064400050113    4C     §B4aks        IFGT      *ZEROS
064500050113     c                   movel     §b4aks        vsfaks
064600050113     c                   movel     §b4act        vsfact
064700050113    4c                   endif
064800050113    3c                   endif
064900050111     C* Tassazione singola
065000050111     c                   eval      wscelta='1'
065100050110     C                   EXSR      ELAB
065200911029     C*
065300911029     C  N11              GOTO      INIZIO
065400941217    3C   11              DO
065500050804     c                   if        %error
065600050804     c* Errore : uscita automatica
065700050804     c                   eval      olra2msg=msg(78)
065800050804     c                   eval      olra2err='1'
065900050804     c                   else
066000050317     C                   MOVEL     ARBFIV        OLRA2FIV
066100050317     C                   MOVEL     ARBNFT        OLRA2NFT
066200050317     C                   MOVEL     ARBDFT        OLRA2DFT
066300050804     c                   endif
066400911031     C                   GOTO      FINE
066500941217    3C                   END
066600911028     C**
066700941217   X2C                   ELSE
066800050216     c
066900050216     c                   EXSR      CARICabolle
067000911023     C*
067100901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
067200050112    3C     NRRS2         IFEQ      0
067300050112    3C     NRRC2         ANDEQ     0
067400911128     C                   SETON                                        58
067500911128     C                   GOTO      FOR01
067600941217    3C                   END
067700911025     C**
067800911025     C** SUBFILE
067900911025     C**
068000050110     C* IN BASE ALLA RICHIESTA DELLA 1 VIDEATA EMETTO SFL CON CODICE O SENZA
068100050125     c* se un dei 2 sfl è vuoto visualizzo quello pieno a prescindere
068200050125     c* dalla richiesta
068300050125     c                   select
068400050125     c                   when      nrrs2=0
068500050125     c                   eval      wtiposfl='C'
068600050125     c                   when      nrrc2=0
068700050125     c                   eval      wtiposfl='S'
068800050125     C                   when      VIDSFL='C'
068900050110     c                   eval      wtiposfl='C'
069000050125     c                   other
069100050110     c                   eval      wtiposfl='S'
069200050125     c                   endsl
069300050216     c* flag per visualizzare entrambi i sfl se pieni
069400050125     C
069500050125     C                   Z-ADD     1             NRRC2
069600050125     C                   Z-ADD     1             NRRS2
069700050125     C                   Z-ADD     1             REC
069800050216     c                   clear                   wvisualdue        1
069900050110     c
070000050110     c                   dow       *inkc=*off and *inkl=*off
070100050110     c                   if        wtiposfl='C'
070200050216     c
070300050110     c                   exsr      SFLCON
070400050216     c
070500050216     c* Se premuto f3 o F12  senza parzial si RSD e non ho ancora visualiz
070600050216     c*  zato entrambi i sfl pieni, emetto l'altro
070700050216     c                   if        not *in20 and not *in21 and
070800050216     c                             wvisualdue=' '  and
070900050216     c                             (*inkc or (*inkl and vidrsd=*blanks))
071000050216     c                   eval      *inkc='0'
071100050216     c                   eval      *inkl='0'
071200050216     c                   eval      wvisualdue='S'
071300050216     c                   eval      wtiposfl='S'
071400050216     c                   endif
071500050216     c
071600050110     c                   else
071700050216     c
071800050216   i c                   exsr      SFLSENZA
071900050216     c
072000050216     c* Se premuto f3 o F12  senza parzial si RSD e non ho ancora visualiz
072100050216     c*  zato entrambi i sfl pieni, emetto l'altro
072200050216     c                   if        not *in20 and not *in21 and
072300050216     c                             wvisualdue=' '  and
072400050216     c                             (*inkc or (*inkl and vidrsd=*blanks))
072500050216     c                   eval      *inkc='0'
072600050216     c                   eval      *inkl='0'
072700050216     c                   eval      wvisualdue='S'
072800050216     c                   eval      wtiposfl='C'
072900050216     c                   endif
073000050216     c
073100050110     c                   endif
073200050216     c
073300050216     c* Se non premuto F3 o F12 ricarico sfl
073400050216     c                   if        not *inkc and not *inkl and whotassato='S'
073500050216     c                   exsr      CARICAbolle
073600050216     c                   clear                   whotassato
073700050216     c                   endif
073800050216     c
073900050110     c                   enddo
074000050110     c*
074100050110     C* F3 - FINE
074200050110     C   KC              GOTO      FINE
074300050110     C* F12 - RITORNO
074400061201     c                   if        *inkl=*on
074500061201     c                   clear                   widmsg
074600061201     C                   GOTO      INIZIO
074700061201     c                   endif
074800050110     C
074900960710    2C                   END
075000960710    1C                   END
075100911023     C*
075200000000     C     FINE          TAG
075300941216     C*
075400050317     C     ILRA2TLA      IFNE      ' '
075500920113     C*
075600930423     C* CALL SOLO PER CHIUDERE I FILE
075700990907     C                   CLEAR                   DTAS
075800941216     C                   MOVEL     'C'           TASTLA
075900941216     C                   CALL      'TNSF20R'
076000930423     C                   PARM                    KPJBA
076100990907     C                   PARM                    DTAS
076200990907     C                   PARM                    DTASV
076300050314     c
076400971117     C                   CLEAR                   DSLV53
076500971117     C                   MOVEL     'C'           D53TLA
076600971117     C                   CALL      'FNLV53R'
076700971117     C                   PARM                    DSLV53
076800050201     C*
076900050201     C                   CLEAR                   fnlv59ds
077000050201     C                   MOVEL     'C'           ilv59tla
077100050201     C                   CALL      'FNLV59R'
077200050201     C                   PARM                    fnlv59ds
077300970710     C*
077400970710     C     WCAL13        IFEQ      'S'
077500970710     C                   MOVEL     'C'           I13TLA
077600970710     C                   CALL      'FNLV13R'
077700970710     C                   PARM                    KPJBA
077800970710     C                   PARM                    DSLV13
077900970710     C                   PARM                    DSSI95
078000970710     C                   ENDIF
078100971105     C**
078200971105     C                   CLEAR                   DSSI95
078300971105     C                   MOVEL     'C'           I95TLA
078400971105     C                   CALL      'TISI95R'
078500971105     C                   PARM                    DSSI95
078600930505     C*
078700990907     C                   CLEAR                   DSLV04
078800941216     C                   MOVEL     'C'           D04TLA
078900990913     C                   CALL      'FNLV04R'
079000990907     C                   PARM                    DSLV04
079100940224     C*
079200941216     C                   CLEAR                   DSUL06
079300941216     C                   MOVEL     'C'           D06TLA
079400941216     C                   MOVEL     DSUL06        KPJBU
079500940224     C                   CALL      'TRUL06R'
079600940224     C                   PARM                    KPJBA
079700020222
079800160226     c                   clear                   trul27ds1
079900020222     c                   eval      i27tla = 'C'
080000160226     c                   call      'TRUL27R1'
080100160226     c                   parm                    trul27ds1
080200050714
080300160226     c                   clear                   trulc7ds
080400160226     c                   eval      ic7tla = 'C'
080500160226     c                   call      'TRULC7R'
080600160226     c                   parm                    trulc7ds
080700160226
080800050714     c                   clear                   fnlv58ds
080900050714     c                   eval      ilv58tla = 'C'
081000050714     c                   call      'FNLV58R'
081100050714     c                   parm                    fnlv58ds
081200130416
081300131119     c*                  clear                   fidg42ds
081400131119     C*                  MOVEL     'C'           co42tla
081500131119     c*                  movel     fidg42ds      kpjbu
081600131119     c*                  call      'FIDG42R'
081700131119     c*                  parm                    kpjba
081800161102
081900161102     c                   eval      partla='C'
082000161102     c                   eval      kpjbu=paramu6ds1
082100161102     c                   call      'FNLRU6R1'
082200161102     c                   parm                    kpjba
082300020222
082400941216     C*
082500941216     C                   SETON                                        LR
082600941216     C                   ELSE
082700950120     C*
082800950120     C* PASSO IL NUMERO FATTURA PER RETRN
082900050317     C   11              MOVEL     DSLRA2        KPJBU
083000941216     C                   RETURN
083100941216     C                   ENDIF
083200950120     C*
083300950120     C* PASSO IL NUMERO FATTURA PER LR
083400050317     C   11              MOVEL     DSLRA2        KPJBU
083500911028     C*
083600911028     C*--- IMPOSTAZIONI INIZIALI -------------------------------------*
083700941216     C     *INZSR        BEGSR
083800941217     C*§ MI SALVO LA KPJBU
083900941217     C                   MOVEL     KPJBU         W256A           256
084000050110     C                   CLEAR                   WINTER            1
084100941217     C*
084200010703     C                   SETOFF                                       15
084300010703     C*
084400941216     C                   Z-ADD     1             CODUT             1 0
084500941216     C                   CALL      'X§PARUT'
084600941216     C                   PARM                    UT§DSE
084700941216     C                   MOVEL     RAGUT         VIDDSA
084800941216     C                   MOVEL     REC80         CNCR80
084900941216     C*--- RICERCA CAPOCONTI------------------------------------------*
085000941216     C                   DO        50            X                 3 0
085100941216     C                   MOVE      TCU(X)        TCUDS
085200941216     C     F56           CABNE     'CG'          END1
085300941216     C     F4            COMP      '1'                                    21
085400941216     C     F4            COMP      '2'                                    22
085500941216     C     F4            COMP      '3'                                    23
085600941216     C     F4            COMP      '6'                                    27
085700941216     C** 1 CLIENTI   21
085800941216     C** 2 FORNITORI 22
085900941216     C** 3 AGENTI    23
086000941216     C     F3            COMP      '0'                                242425
086100941216     C     F3            COMP      'I'                                    26
086200941216     C** 0 ITALIA   25
086300941216     C** 1 ESTERO   24
086400941216     ** I CAPO CONTO IVA
086500941216     C   21
086600941216     CAN 24              Z-ADD     KCU(X)        KCE               4 0
086700941216     C   21
086800941216     CAN 25              Z-ADD     KCU(X)        KCI               4 0
086900941216     C   22
087000941216     CAN 24              Z-ADD     KCU(X)        KFE               4 0
087100941216     C   22
087200941216     CAN 25              Z-ADD     KCU(X)        KFI               4 0
087300941216     C   23
087400941216     CAN 24              Z-ADD     KCU(X)        KAE               4 0
087500941216     C   23
087600941216     CAN 25              Z-ADD     KCU(X)        KAI               4 0
087700941216     C   26              Z-ADD     KCU(X)        KIVA              4 0
087800941216     C   27              Z-ADD     KCU(X)        KBNA              4 0
087900941216     C     END1          TAG
088000941216     C                   END
088100941216     C                   SETOFF                                       24
088200020805
088300020805     c                   movel     simfel        $mbr
088400020805     c                   eval      $simfel = simfel
088500941216     C**
088600941216     C* KLIST
088700941216     C**
088800970527     C     KFVV          KLIST                                                  *
088900970527     C                   KFLD                    KNPG                           *
089000970527     C                   KFLD                    W0050                          *
089100020911     C                   KFLD                    Kfgs
089200941217     C     KCAP          KLIST
089300941217     C                   KFLD                    WNAR
089400941217     C                   KFLD                    WCAP
089500941216     C     KARB          KLIST
089600941216     C                   KFLD                    VIDAAS
089700941216     C                   KFLD                    VIDLNP
089800941216     C                   KFLD                    VIDNRS
089900941216     C                   KFLD                    VIDNSP
090000941216     C     KARB2         KLIST
090100050112     C                   KFLD                    VsfAAS
090200050112     C                   KFLD                    VsfLNP
090300050112     C                   KFLD                    VsfNRS
090400050112     C                   KFLD                    VsfNSP
090500941216     C     KARB4         KLIST
090600050112     C                   KFLD                    VsfAAS
090700050112     C                   KFLD                    VsfLNP
090800050112     C                   KFLD                    VsfNRS
090900050112     C                   KFLD                    VsfNSP
091000941216     C                   KFLD                    WTRC
091100050309     C     KARB44        KLIST
091200050309     C                   KFLD                    arbAAS
091300050309     C                   KFLD                    arbLNP
091400050309     C                   KFLD                    arbNRS
091500050309     C                   KFLD                    arbNSP
091600050309     C                   KFLD                    WTRC
091700050309     C     Kblp44        KLIST
091800050309     C                   KFLD                    blpAAS
091900050309     C                   KFLD                    blpLNP
092000050309     C                   KFLD                    blpNRS
092100050309     C                   KFLD                    blpNSP
092200050309     C                   KFLD                    WTRC
092300941216     C     KTAB          KLIST
092400941216     C                   KFLD                    CODUT
092500941216     C                   KFLD                    COD               2
092600941216     C                   KFLD                    KEY               8
092700990929     C     KTAB1         KLIST
092800990929     C                   KFLD                    CODUT
092900990929     C                   KFLD                    COD               2
093000941216     C     KACO          KLIST
093100941216     C                   KFLD                    CODUT
093200941216     C                   KFLD                    KCI
093300941216     C                   KFLD                    COMKSC            7 0
093400941216     C     KTAM3         KLIST                                                  *
093500941216     C                   KFLD                    COMKSC                         *
093600941216     C                   KFLD                    COMCTR                         *
093700941216     C                   KFLD                    COMPRG                         *
093800941216     C     KTPT          KLIST                                                  *
093900941216     C                   KFLD                    COMKSC                         *
094000941216     C                   KFLD                    COMCTR                         *
094100941216     C                   KFLD                    COMPRG                         *
094200990817     C                   KFLD                    FTC               2            *
094300941216     C     KNUM          KLIST
094400941216     C                   KFLD                    DATAAS            2 0
094500941216     C                   KFLD                    RGS               1 0
094600941216     C                   KFLD                    FIV               3 0
094700020911
094800020911     c     Kblp          Klist
094900020911     c                   Kfld                    LblAao
095000020911     c                   Kfld                    LblLpo
095100020911     c                   Kfld                    LblNro
095200020911     c                   Kfld                    LblNso
095300030203
095400030203     c     kFiar5        Klist
095500050112     c                   Kfld                    VsfAas
095600050112     c                   Kfld                    VsfLnp
095700050112     c                   Kfld                    VsfNrs
095800050112     c                   Kfld                    VsfNsp
095900030203     c                   Kfld                    kAr5Trd
096000070924     c     kAR5          Klist
096100070924     c                   Kfld                    ARbAas
096200070924     c                   Kfld                    ARbLnp
096300070924     c                   Kfld                    ARbNrs
096400070924     c                   Kfld                    ARBNsp
096500070924     c                   Kfld                    kAr5Trd
096600030506
096700030506     c     kNtc          Klist
096800030506     c                   Kfld                    kNtcApl
096900030506     c                   Kfld                    kNtcNk1
097000030506     c                   Kfld                    kNtcNk2
097100030506     c                   Kfld                    kNtcTnt
097200020222
097300941216     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
097400941216     C*
097500911029     C                   Z-ADD     2             RGS
097600990817     C                   MOVEL     'C '          FTC
097700911029     C*
097800970527     C                   MOVEL     4             KNPG
097900970527     C*
098000020415     C     *LIKE         DEFINE    §CVDEC        COMDEC
098100011113     C     *LIKE         DEFINE    §CVDEC        DECSAV
098200991028     C     *LIKE         DEFINE    FVVNPG        KNPG
098300960208     C     *LIKE         DEFINE    VIDKLP        WKLP
098400960208     C     *LIKE         DEFINE    ARBQFT        VARQFT
098500950104     C     *LIKE         DEFINE    ARBIAS        VARIAS
098600941217     C     *LIKE         DEFINE    ARBVAS        WVLT
098700941217     C     *LIKE         DEFINE    ARBVAS        VARVAS
098800990930     C     *LIKE         DEFINE    ARBIAS        SAVIAS
098900990930     C     *LIKE         DEFINE    ARBVAS        SAVVAS
099000990930     C     *LIKE         DEFINE    ARBQFT        SAVQFT
099100990930     C     *LIKE         DEFINE    ARBVMD        SAVVMD
099200990930     C     *LIKE         DEFINE    ARBVAD        SAVVAD
099300990930     C     *LIKE         DEFINE    ARBKSC        KSCSAV
099400990930     C     *LIKE         DEFINE    ARBCTR        CTRSAV
099500050201     C     *LIKE         DEFINE    olv59TKS      WTKS
099600941217     C     *LIKE         DEFINE    TAMFIE        WFIE
099700941217     C     *LIKE         DEFINE    TAMPPA        WPPA
099800941217     C     *LIKE         DEFINE    TAMMPA        WMPA
099900941217 >   C     *LIKE         DEFINE    TAMCTR        COMCTR
100000941217     C     *LIKE         DEFINE    TAMPRG        COMPRG
100100941217     C     *LIKE         DEFINE    SOVR(1)       SOARBT
100200941217     C     *LIKE         DEFINE    SOVR(1)       SAARBT
100300941217     C     *LIKE         DEFINE    SOVR(1)       SDARBT
100400941216     C     *LIKE         DEFINE    AR6SV1        WSV1
100500941216     C     *LIKE         DEFINE    AR6VA1        WVA1
100600990910     C     *LIKE         DEFINE    AR6VA1        VA2VA1
100700990910     C     *LIKE         DEFINE    AR6POR        VARPOR
100800941216     C     *LIKE         DEFINE    AR4TRC        WTRC
100900941216     C     *LIKE         DEFINE    TBLKUT        §KUT
101000911028     C     *LIKE         DEFINE    TBLCOD        §COD
101100911028     C     *LIKE         DEFINE    TBLKEY        §KEY
101200941217     C     *LIKE         DEFINE    ARBNZD        WNAR
101300941217     C     *LIKE         DEFINE    ARBCAD        WCAP
101400950121     C     *LIKE         DEFINE    ARBQFT        WQFT
101500950121     C     *LIKE         DEFINE    ARBIAS        WIAS
101600950121     C     *LIKE         DEFINE    ARBVAS        WVAS
101700990923     C     *LIKE         DEFINE    AR6DIV        WDIV
101800010319     C     *LIKE         DEFINE    VIDMSG        WIDMSG
101900011107      *
102000011107     C     *LIKE         DEFINE    DSCW          WDSCW
102100011107     C                   MOVEL     *BLANKS       WESITO            1
102200011107     C                   MOVEL     *BLANKS       DATA10           10
102300011107     C                   Z-ADD     *ZEROS        DATA80            8 0
102400940223     C*
102500911028     C* GIRO UDATE
102600920108     C                   MOVE      UDATE         DATAAS            2 0
102700941216     C                   TIME                    W0140            14 0
102800941216     C* UDATE IN GGMMAAAA
102900941216     C                   MOVE      W0140         WDTGIO            8 0
103000941216     C*
103100941216     C* UDATE IN AAAAMMGG
103200941216     C                   Z-ADD     WDTGIO        G02DAT
103300941216     C                   MOVEL     *BLANK        G02ERR
103400941216     C                   CALL      'XSRDA8'
103500941216     C                   PARM                    WLBDAT
103600941216     C                   MOVEL     G02INV        DATEU             8 0
103700941216     C*
103800941216     C* VEDO SE SONO SIMFEL O REMOTO
103900941216     C*
104000050318     c                   clear                   nousa             1
104100020513     c     simtpp        ifeq      '2'
104200020513     c     simtpp        oreq      *blanks
104300020513     c                   movel     simpou        vidfil
104400941216     C                   SETON                                        17
104500941216     C                   ELSE
104600941216     C                   MOVEL     SIMFEL        VIDFIL
104700941216     C                   END
104800050318     c                   movel     knmus         w003a
104900050318     c*
105000050401     c***                if        w003a<>'EDP' and vidfil<>28 and vidfil<>6
105100050401     c***                movel     msg(69)       widmsg
105200050401     c***                eval      nousa='S'
105300050401     c***                endif
105400941217     C*
105500050317     C                   MOVEL     W256A         DSLRA2
105600050309     C* VERIFICO SE SI TRATA DI TASSAZIONE IN PARTENZA
105700050309     C                   MOVE      W256A         WTIPOTASSA        1
105800050309     C
105900050309     C                   IF        WTIPOTASSA='P'
106000050309     C                   SETON                                        94
106100050309     C                   ENDIF
106200941217     C*
106300941217     C* 11 ON  - PGM RICHIAMATO
106400050317     C     iLRA2NSP      IFGT      0
106500941217     C                   SETON                                        11
106600941217     C* SE PASSATA ANCHE FIL IN GESTIONE LA IMPOSTO
106700050317     C     ILRA2FGS      IFGT      *ZEROS
106800050317     C                   MOVEL     ILRA2FGS      VIDFIL
106900941217     C                   END
107000941217     C                   END
107100070314     c
107200070314     c                   EXSR      DECOFGS
107300070314     c
107400070314     C* CARICO tabelle
107500941217     C                   EXSR      CARTAB
107600930427     C*
107700050309     C* SE PROGRAMMA NON RICHIAMATO E NON È TASSAZIONE IN PARTENZA
107800050309     C*                             CHIEDO STAMPANTI
107900070703     C**   *IN11         IFEQ      *OFF
108000070703     C**   *IN94         ANDEQ     *OFF
108100070703     c* oppure se richiamato in arrivo ma non passata la DS stampanti
108200130320     c****               if        (*in11=*off and *in94=*off) or
108300130320     c****                         (*in94=*off and %parms=2 and d90pss=*blanks)
108400130320     c****                         or (*in94=*off and %parms<2)
108500070703     c
108600130320     C****               CLEAR                   WTrul90DS
108700941217     C*
108800130320     C****               MOVEL     'S'           wd90RSB
108900130320     C****               MOVEL     'FAT999'      wd90MDB
109000130320     C****               MOVEL     'FAT99'       wd90MDB4
109100130320     C****               MOVEL     'FAT99'       wd90MDB5
109200130320     C****               CALL      'TRUL90R'
109300130320     C****               PARM                    KPJBA
109400130320     C***                PARM                    WTrul90DS
109500941217     C**
109600941217     C* F3 - FINE
109700130320     C***  wd90F3        IFEQ      '1'
109800130320     C****               MOVEL     '1'           WFINE             1
109900130320     C****               ENDIF
110000030613      *
110100030613      * OVRPRTF BOLLE A4
110200130320     C****               MOVEA     wD90PRB4      CMA4(30)
110300130320     C****               MOVEA     wD90MDB4      CMA4(52)
110400030613     C*
110500130320     C****               Z-ADD     130           LUNG             15 5
110600130320     C****               CLEAR                   COMMAN          130
110700130320     C****               MOVEA     CMA4          COMMAN
110800130320     C****               CALL      'QCMDEXC'                            91
110900130320     C****               PARM                    COMMAN
111000130320     C****               PARM                    LUNG
111100030613      *
111200030613      * OVRPRTF BOLLE A5
111300130320     C****               MOVEA     wD90PRB5      CMA5(30)
111400130320     C****               MOVEA     wD90MDB5      CMA5(52)
111500030613     C*
111600130320     C****               Z-ADD     130           LUNG
111700130320     C****               CLEAR                   COMMAN
111800130320     C****               MOVEA     CMA5          COMMAN
111900130320     C****               CALL      'QCMDEXC'                            91
112000130320     C****               PARM                    COMMAN
112100130320     C***                PARM                    LUNG
112200941217     C*
112300130320     C****               ENDIF
112400071001      * Se ambiente prova
112500071001     c                   If        %subst(knsif:7:1) = 'P'
112600071001     c                   Eval      wlib = 'GAITRAGRPS'
112700071001     c                   Else
112800071001      * Se ambiente buono FILTRA201
112900071001     c                   Eval      wlib = 'GAITRAGRU '
113000071001     c                   EndIf
113100071001     c
113200071001     c                   Eval      lung = 80
113300170116     c                   Clear                   comman          130
113400071001     c                   Movel(p)  OVR(1)        comman
113500071001     c                   Eval      %Subst(comman:30:10) = wlib
113600071001     c                   Eval      comman =
113700151013     c                             %trim(comman) + '/FIAR531C)'
113800151013     c                   Call      'QCMDEXC'                            92
113900071001     c                   Parm                    comman
114000170116     c                   Parm                    lung             15 5
114100151013     c  n92              Open      FIAR531C
114200071001     c
114300071001     c                   Clear                   comman
114400071001     c                   Movel(p)  OVR(2)        comman
114500071001     c                   Eval      %Subst(comman:30:10) = wlib
114600071001     c                   Eval      comman =
114700071001     c                             %trim(comman) + '/TITAS30C)'
114800071001     c                   Call      'QCMDEXC'                            92
114900071001     c                   Parm                    comman
115000071001     c                   Parm                    lung
115100071001     c  n92              Open      TITAS30C
115200071001      *
115300941217     C                   MOVEL     W256A         KPJBU
115400941217     C*
115500941216     C                   ENDSR
115600070314     C*
115700070314     c* Decodifiche e caricamente della filiale in gestione ---------*
115800070314     c     DECOFGS       BEGSR
115900070314     C     VIDFIL        CHAIN     AZORG                              32
116000070314     C  N32              MOVEL     ORGDES        DESFIL
116100070314     c
116200120125     c*  permetto eventuale stampa DDT con F8
116300130320     c**n94              seton                                        03
116400070314     c
116500070314     C*
116600070314     C* CARICO LE LNA DELLA FILIALE IN GESTIONE
116700070314     C                   EXSR      CARL6
116800070314     c
116900070314     c                   ENDSR
117000920813     C*
117100920813     C* CARICO L6 ------------------------------------------***********
117200920813     C     CARL6         BEGSR
117300941216     C*
117400941216     C                   CLEAR                   DSUL06
117500941216     C                   MOVE      '£6'          D06COD
117600941217     C                   MOVEL     VIDFIL        D06KEY
117700941216     C                   MOVEL     'L'           D06TLA
117800941216     C                   MOVEL     DSUL06        KPJBU
117900941216     C                   CALL      'TRUL06R'
118000941216     C                   PARM                    KPJBA
118100941216     C                   MOVEL     KPJBU         DSUL06
118200941216     C                   MOVEA     LIN           L6
118300920813     C                   ENDSR
118400930209     C*
118500930209     C* CARICO TABELLA VARIABILI ARRIVI ------------------------------*
118600930209     C     CARTAB        BEGSR
118700941217     C                   CLEAR                   WINTER
118800941216     C*
118900990913     C                   MOVEL     '$2'          COD
119000990913     C                   MOVEL(P)  '1'           KEY
119100990913     C                   EXSR      CTRTAB
119200990913     C  N32              MOVEL     TBLUNI        DS$2
119300990913     C   32              CLEAR                   DS$2
119400990909     C**
119500990909     C* CARICO TABELLE DELLE VARIE PER CONTROLLI
119600990909     C**
119700050309     C                   Z-ADD     0             II
119800990909     C                   MOVEL     'CC'          COD
119900990929     C     KTAB1         CHAIN     TABEL                              32
120000990909    1C     *IN32         DOWEQ     *OFF
120100990909    2C     TBLFLG        IFEQ      ' '
120200990909     C                   MOVEL     TBLKEY        W005A             5
120300990909    3C     W005A         IFEQ      'VARIE'
120400990909     C                   MOVEL     TBLUNI        DSCC
120500990909     C* SIGLA VARIA
120600990909    4C     §CCFL6        IFEQ      'S'
120700990910     C                   ADD       1             II
120800990909     C                   MOVE      TBLKEY        SVA(II)
120900990909    4C                   ENDIF
121000990909    3C                   ENDIF
121100990909    2C                   ENDIF
121200990909     C**
121300990929     C     KTAB1         READE     TABEL                                  32
121400990909    1C                   ENDDO
121500991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
121600991117     C                   CLEAR                   DSBS02
121700991117     C                   CLEAR                   DGED
121800991117     C                   MOVEL     'C'           T02MOD
121900991117     C                   MOVEL     KNSIF         T02SIF
122000991117     C                   MOVEL     'GED'         T02COD
122100991117     C                   MOVEL     '1'           T02KE1
122200991117     C                   CALL      'TIBS02R'
122300991117     C                   PARM                    KPJBA
122400991117     C                   PARM                    DSBS02
122500991117     C     T02ERR        IFEQ      *BLANKS
122600991117     C                   MOVEL     T02UNI        DGED
122700991117     C                   ENDIF
122800011002     C* REPERIMENTO importi DELLA MONETA DI CONTO
122900011002     C                   CLEAR                   DSBS02
123000011002     C                   CLEAR                   DGEI
123100011002     C                   MOVEL     'C'           T02MOD
123200011002     C                   MOVEL     KNSIF         T02SIF
123300011002     C                   MOVEL     'GEI'         T02COD
123400011002     C                   MOVEL     §GEDCN        T02KE1
123500011002     C                   CALL      'TIBS02R'
123600011002     C                   PARM                    KPJBA
123700011002     C                   PARM                    DSBS02
123800011002     C     T02ERR        IFEQ      *BLANKS
123900011002     C                   MOVEL     T02UNI        DGEI
124000011220     c                   eval      sav_geimf = §geimf
124100011002     C                   ENDIF
124200011113      * Tabella CV della moneta di conto
124300011113     C                   CLEAR                   DSCV
124400011113     C                   MOVEL     'CV'          COD
124500011113     C                   MOVEL(P)  §GEDCN        KEY
124600011113     C                   EXSR      CTRTAB
124700011113     C     *IN32         IFEQ      *OFF
124800011113     C                   MOVEL     TBLUNI        DSCV
124900011113     C                   MOVEL     §CVDEC        DECSAV
125000011113     C                   ENDIF
125100020715
125200050309      * Carico sk con tipi bolla che prevedono l'assicurazione
125300020715
125400020715     c                   eval      cod = 'TB'
125500020715     c                   clear                   key
125600020715     c                   clear                   xx
125700050309     C                   clear                   II
125800020715     c     ktab1         setll     tabel00f
125900020715     c                   do        *hival
126000020715     c     ktab1         reade     tabel00f
126100020715      * fine file
126200020715     c                   if        %eof(tabel00f)
126300020715     c                   leave
126400020715     c                   endif
126500020715      * record annullato
126600020715     c                   if        tblflg <> *blanks
126700020715     c                   iter
126800020715     c                   endif
126900020715     c                   movel     tbluni        dstb
127000050309     c* Assicurazione
127100020715     c                   if        §tbfas = '1'
127200020715     c                   add       1             xx
127300020715     c                   eval      tbl(xx) = tblkey
127400020715     c                   endif
127500050309     c* Assegnati storno
127600050309     C                   if        §tbfcb = '0'
127700050309     C                   ADD       1             II
127800050309     C                   MOVEl     TBLKEY        TBSTO(II)
127900050309     C                   ENDIF
128000020715     c                   enddo
128100050523     c
128200050523     c* controllo riempimento skiere
128300050523     c                   clear                   trul0sds
128400050523     c                   eval      i0sski='TBSTO'
128500050523     c                   eval      i0sele=%elem(tbsto)
128600050523     c                   eval      i0spie=ii
128700050523     c                   eval      i0sfile='TABEL00F  '
128800050523     c                   eval      i0scod=' TB  '
128900050523     c                   eval      i0ssif=knsif
129000050523     c                   eval      i0spgm='FNLRA2R'
129100050523     c                   movel     trul0sds      kpjbu
129200050523     c                   call      'TRUL0SR'
129300050523     c                   parm                    kpjba
129400050523     c
129500050523     c                   clear                   trul0sds
129600050523     c                   eval      i0sski='TBL'
129700050523     c                   eval      i0sele=%elem(tbl)
129800050523     c                   eval      i0spie=xx
129900050523     c                   eval      i0sfile='TABEL00F  '
130000050523     c                   eval      i0scod=' TB  '
130100050523     c                   eval      i0ssif=knsif
130200050523     c                   eval      i0spgm='FNLRA2R'
130300050523     c                   movel     trul0sds      kpjbu
130400050523     c                   call      'TRUL0SR'
130500050523     c                   parm                    kpjba
130600050523     c
130700050309     C**
130800050309     C* CARICO codici bolla assegnato  e recuepro
130900050309     C**
131000050309     C                   Z-ADD     0             II
131100050309     C                   Z-ADD     0             ZZ
131200050309     C                   Z-ADD     0             XX
131300060117     C                   Z-ADD     0             yy                4 0
131400050309     C                   MOVEL     '3A'          COD
131500050309     C     KTAB1         CHAIN     TABEL                              32
131600050309    1C     *IN32         DOWEQ     *OFF
131700050309    2C     TBLFLG        IFEQ      ' '
131800050309     C                   MOVEL     TBLUNI        DS3a
131900050309     C                   MOVEL     §3ATB1        UNOTB1            1
132000050309     C                   MOVEL     §3ATB2        UNOTB2            1
132100050309    3C                   IF        UNOTB1='A' or UNOTB2='A'
132200050309     c* Controllo se si tratta di assegnato contabilizzabile
132300050309     c                   if        unotb1='A'
132400050309     c                   movel(P)  §3atb1        wtbl              2
132500050309     c                   else
132600050309     c                   movel(P)  §3atb2        wtbl
132700050309     c                   endif
132800050309     c
132900050309     C     wtbl          lookup    tbsto                                  34
133000050309    4C                   IF        not *in34
133100050309     C                   ADD       1             II
133200050309     C                   MOVEL     TBLKEY        cboass(II)
133300060117     c
133400050309     c* Se l'assegnato + 2 bolla, lo memorizzo a parte
133500050309     c                   if        §3atb2<>*blanks
133600050309     C                   ADD       1             zz
133700050309     C                   MOVEL     TBLKEY        cboass2(zz)
133800050309     c                   endif
133900050309     c
134000050309    4C                   ENDIF
134100050309    3C                   ENDIF
134200050309     c* cod bolla di recupero
134300050309     c                   if        §3arbl = 'R'
134400050309     c                   add       1             xx
134500050309     c                   eval      cborec(xx) = tblkey
134600050309     c                   endif
134700050309    2C                   ENDIF
134800050309     C**
134900050309     C     KTAB1         READE     TABEL                                  32
135000050309    1C                   ENDDO
135100050519     c* controllo riempimento skiere
135200050519     c                   clear                   trul0sds
135300050523     c                   eval      i0sski='CBOASS'
135400050523     c                   eval      i0sele=%elem(cboass)
135500050519     c                   eval      i0spie=ii
135600050519     c                   eval      i0sfile='TABEL00F  '
135700050519     c                   eval      i0scod=' 3A  '
135800050519     c                   eval      i0ssif=knsif
135900050519     c                   eval      i0spgm='FNLRA2R'
136000050519     c                   movel     trul0sds      kpjbu
136100050519     c                   call      'TRUL0SR'
136200050519     c                   parm                    kpjba
136300050523     c
136400050523     c                   clear                   trul0sds
136500050523     c                   eval      i0sski='CBOASS2'
136600050523     c                   eval      i0sele=%elem(cboass2)
136700050523     c                   eval      i0spie=zz
136800050523     c                   eval      i0sfile='TABEL00F  '
136900050523     c                   eval      i0scod=' 3A  '
137000050523     c                   eval      i0ssif=knsif
137100050523     c                   eval      i0spgm='FNLRA2R'
137200050523     c                   movel     trul0sds      kpjbu
137300050523     c                   call      'TRUL0SR'
137400050523     c                   parm                    kpjba
137500050523     c
137600050523     c                   clear                   trul0sds
137700050523     c                   eval      i0sski='CBOREC'
137800050523     c                   eval      i0sele=%elem(cborec)
137900050523     c                   eval      i0spie=xx
138000050523     c                   eval      i0sfile='TABEL00F  '
138100050523     c                   eval      i0scod=' 3A  '
138200050523     c                   eval      i0ssif=knsif
138300050523     c                   eval      i0spgm='FNLRA2R'
138400050523     c                   movel     trul0sds      kpjbu
138500050523     c                   call      'TRUL0SR'
138600050523     c                   parm                    kpjba
138700021121
138800021121      * Aggancio tabella default tariffe fedex
138900021121     c                   Clear                   Dsbs02
139000021121     c                   Movel     'C'           T02Mod
139100021121     c                   Movel     KNSIF         T02Sif
139200021121     c                   Movel     'DFE'         T02Cod
139300021121     c                   Movel     'FED'         T02Ke1
139400021121     c                   Call      'TIBS02R'
139500021121     c                   Parm                    Kpjba
139600021121     c                   Parm                    Dsbs02
139700021121     c                   If        T02Err = *Blanks
139800021121     c                   Movel     T02Uni        DDfe
139900021121     c                   Else
140000021121     c                   Clear                   DDfe
140100021121     c                   EndIf
140200050714     c
140300050714     c* Verifico se devo tenere conto degli assegnati dirottati per
140400050714     c*  tassare
140500050714     c
140600050714      * Aggancio tabella "VPO" per sapere se tenere conto dei dirottamenti
140700050714     c*
140800050714     C                   CLEAR                   dsbs02
140900050714     c                   clear                   dvpo
141000050714     C                   MOVEL     'C'           T02MOD
141100050714     C                   MOVEL     KNSIF         T02SIF
141200050714     C                   MOVEL     'VPO'         T02COD
141300050714     c                   movel(P)  'VPO'         t02ke1
141400050714     C                   CALL      'TIBS02R'
141500050714     C                   PARM                    KPJBA
141600050714     C                   PARM                    dsbs02
141700050714    2C                   IF        T02ERR = *BLANKS
141800050714     C                   MOVEL     T02uni        dvpo
141900050714    2C                   ENDIF
142000050714     c
142100930209     C                   ENDSR
142200921229     C*
142300050309     c* Richiamo pgm di interrogazine bolle -----------------------------*
142400050309     c     INTBOLLE      begsr
142500050309     c* F7 - richiamo per selezione
142600050309    1c                   if        *inkg
142700050309     c* In arrivo
142800161102    2c***                if        not *in94
142900161102     C****               CLEAR                   PARAM
143000161102     C***                MOVEL     '2'           PARFLG
143100161102     C*****              MOVEL     PARAM         KPJBU
143200161102     C********           CALL      'FNLR36R'
143300161102     C********           PARM                    KPJBA
143400161102     C********           MOVEL     KPJBU         PARAM
143500050309     C* SE PASSATA KEY LA IMPOSTO
143600161102     C***  VsfNSP        IFGT      0
143700161102     C***                MOVEL     VsfAAS        VIDAAS
143800161102     C****               MOVEL     VsfLNP        VIDLNP
143900161102     C****               MOVEL     VsfNRS        VIDNRS
144000161102     C****               MOVEL     VsfNSP        VIDNSP
144100161102     C****               END
144200161102   x2c*****              else
144300050309     c* in partenza
144400161102     C****               CLEAR                   PARAM3
144500161102     C****               MOVEL     '1'           PA3FLG
144600161102     C**********         MOVEL     PARAM3        KPJBU
144700161102     C*********          CALL      'FNLS04R'
144800161102     C**********         PARM                    KPJBA
144900161102     C***********        MOVEL     KPJBU         PARAM3
145000050309     C* SE PASSATA KEY LA IMPOSTO
145100161102     C***  PA3NSP        IFGT      0
145200161102     C********           MOVEL     PA3AAS        vidaas
145300161102     C************       MOVEL     PA3LNP        vidLNP
145400161102     C***********        MOVEL     PA3NRS        vidNRS
145500161102     C***********        MOVEL     PA3NSP        vidNSP
145600161102     C**********         END
145700161102    2c************       endif
145800161102     c                   clear                   paramu6ds
145900161103     c                   clear                   fnlru6ds
146000161102     c                   eval      pa1flg='2'
146100161102     c                   eval      kpjbu=paramu6ds
146200161102     c   94              eval      ILRU6SSP='N'
146300161103     c  n94              eval      ILRU6SSP='1'
146400161103     c                   eval      ilru6lna=vidfil
146500161102     c                   call      'FNLRU6R'
146600161102     c                   parm                    kpjba
146700161103     c                   parm                    fnlru6ds
146800161102     c                   movel     kpjbu         paramu6ds
146900161102     c                   if        pa1nsp>0
147000161102     C                   MOVEL     PA1AAS        vidaas
147100161102     C                   MOVEL     PA1LNP        vidLNP
147200161102     C                   MOVEL     PA1NRS        vidNRS
147300161102     C                   MOVEL     PA1NSP        vidNSP
147400161102     c                   endif
147500050309     c*
147600050309     c* Int.bolle singola
147700050309   x1c                   else
147800050309     c*
147900161102    2c****               if        not *in94
148000161102     C******             MOVEL     '1'           PARFLG
148100161102     C**********         MOVEL     PARAM         KPJBU
148200161102     C************       CALL      'FNLR36R'
148300161102     C***********        PARM                    KPJBA
148400161102   x2c********           else
148500161102     c********           clear                   param3
148600161102     C********           MOVEL     VsfAAS        PA3AAS
148700161102     C********           MOVEL     VsfLnp        PA3LNP
148800161102     C********           MOVEL     VsfNRS        PA3NRS
148900161102     C********           MOVEL     VsfNSP        PA3NSP
149000161102     C********           MOVEL     ' '           PA3FL2
149100161102     C********           MOVEL     Viddsa        PA3RSU
149200161102     C********           MOVEL     PARAM3        KPJBU
149300161102     C********           CALL      'FNLS05R'
149400161102     C********           PARM                    KPJBA
149500161102    2c********           endif
149600161102     c                   clear                   paramu6ds1
149700161102     c                   eval      pa1aas_=vsfaas
149800161102     c                   eval      pa1lnp_=vsflnp
149900161102     c                   eval      pa1nrs_=vsfnrs
150000161102     c                   eval      pa1nsp_=vsfnsp
150100161103     c                   eval      kpjbu=paramu6ds1
150200161102     c                   call      'FNLRU6R1'
150300161102     c                   parm                    kpjba
150400050309    1c                   endif
150500050309     c                   ENDSR
150600050309     c
150700900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
150800900509     C     CONTR         BEGSR
150900941216     C                   SETOFF                                       9030
151000941216     C* P E R   D A T A   S P E D I Z I O N E
151100941216    1C     SEL1          IFNE      0
151200050309     c* In arrivo
151300050309    2c                   if        not *in94
151400941216     C*
151500050309     C     KARB          CHAIN     FNARB000                           3034
151600050309     C* inesistente
151700911127     C   30              SETON                                        4190
151800911127     C   30              GOTO      ENDCTR
151900050309     c* Allocata da un altro lavoro
152000050309     C   34              SETON                                        4690
152100050309     C   34              GOTO      ENDCTR
152200911029     C*
152300050309     C* GIA' TASSATA
152400050309     c                   if        not *in11
152500050309     C     ARBACA        IFEQ      'S'
152600911127     C                   SETON                                        4090
152700911127     C                   GOTO      ENDCTR
152800911028     C                   END
152900911028     C*
153000050309     C* SPEDIZIONE CON LINEA ARRIVO NON gestita
153100911028     C     ARBLNA        LOOKUP    L6                                     30
153200911028     C  N30              SETON                                        4390
153300911028     C  N30              GOTO      ENDCTR
153400911217     C                   END
153500050309     c*
153600050309   x2c                   else
153700050309     c* In partenza
153800050309     C*
153900050309     C     Karb          CHAIN(n)  FNBLP000                           30
154000050309     C* inesistente
154100050309     C   30              SETON                                        4190
154200050309     C   30              GOTO      ENDCTR
154300161102     c* Bolla già partita: devo testare FT1 e non FT2 perchè la bolla scritta sia in blp che
154400161102     c*  in ARB ha FT1  flaggato ma non FT2 perchè ancora da trasmettere in sede
154500161102     c                   if        blpft1<>' '
154600050311     C                   SETON                                        4790
154700050311     C                   GOTO      ENDCTR
154800050311    3C                   ENDIF
154900050309     C*
155000050309     C* CONTROLLO SE SPEDIZIONE IN ASSEGNATO contabilizzabile
155100050309     c     blpcbo        lookup    cboass                                 34
155200050309    3c                   if        not *in34
155300050309     C                   SETON                                        4490
155400050309     C                   GOTO      ENDCTR
155500050309    3C                   ENDIF
155600050309     C
155700050309     c* chaino record di tassazione
155800050309     c     blpcbo        lookup    cboass2                                34
155900050309    3c                   if        *in34
156000050309     C                   MOVEL     '2'           WTRC
156100050309     c                   else
156200050309     C                   MOVEL     '1'           WTRC
156300050309    3c                   endif
156400050309     C     Kblp44        CHAIN(N)  FIAR6000                           38
156500050309    3c                   if        not *in38
156600050309     c* Se presenti importi di tassazione: non tassabile in partenza
156700050309    4c                   if        ar6por>0 or ar6sv1<>*blanks or
156800050309     c                             ar6cei<>*blanks
156900050309     C                   SETON                                        4590
157000050309     C                   GOTO      ENDCTR
157100050309    4c                   endif
157200050309     c
157300050309     C* GIA' TASSATA  se presete codice S.F.
157400050309     c                   move      ar6ksc        wfineksc          4
157500050309    4C                   if        wfineksc<>'9999'
157600050309     C                   SETON                                        4090
157700050309     C                   GOTO      ENDCTR
157800050309    4C                   END
157900050309    3C                   END
158000050309     C*
158100050309    3C  N11              DO
158200050309     C* SPEDIZIONE CON LINEA ARRIVO NON ABILITATA
158300050309     C     blpLNA        LOOKUP    L6                                     30
158400050309     C  N30              SETON                                        4390
158500050309     C  N30              GOTO      ENDCTR
158600050309    3C                   END
158700050309     c*
158800050309     c* Per tassazione in partenza aggancio fnblp00f in update
158900050309     c*  che ha i campi ridenominati come arb
159000050309     c     blp01nrr      chain     fnblpfis                             30
159100050309     c                   if        *in30
159200050309     c                   seton                                        9046
159300050309     c                   endif
159400050309    2c                   endif
159500941216     C*
159600941216   X1C                   ELSE
159700911028     C*
159800941216     C* T U T T E   S P E D I Z I O N I
159900911028     C     VIDLP2        IFNE      0
160000911028     C     VIDLP2        CHAIN     AZORG                              30
160100911028     C* LINEA ANNULLATA
160200911028     C  N30ORGFVA        IFNE      *BLANKS
160300911028     C                   SETON                                        30
160400911028     C                   END
160500911028     C* NON E' FILIALE O AGENZIA
160600911028     C  N30ORGFAG        IFNE      'A'
160700911127     C     ORGFAG        ANDNE     'F'
160800911028     C                   SETON                                        30
160900911028     C                   END
161000911028     C* ERRORE
161100911028     C   30              SETON                                        4290
161200911028     C   30              GOTO      ENDCTR
161300911028     C                   END
161400911028     C*
161500941216    1C                   END
161600911023     C*
161700900514     C     ENDCTR        ENDSR
161800050216     c*
161900050216     c* Operazioni per caricamento sfl ----------------------------------*
162000050216     c     CARICAbolle   begsr
162100050216     c* Pulizia sfl
162200050216     C                   EXSR      PULSF
162300050309      *----
162400050309      * controllo se immessa ragione sociale destinatario
162500050309      *----
162600050309     c                   if        vidrsd<>*blanks
162700050309     c     ' '           checkr    vidrsd        wposizrsd         2 0
162800050309     c                   endif
162900050216     c
163000050216     C                   Z-ADD     0             NRRS2             4 0
163100050216     C                   Z-ADD     0             NRRC2             4 0
163200050216     C                   Z-ADD     0             NRR               4 0
163300050216     c                   Clear                   wsflpieno2
163400050216     c                   Clear                   wsflpieno22
163500050309     c                   clear                   sksflcon
163600050309     c                   clear                   sksflsen
163700050311     C                   Z-ADD     1             savRECc2
163800050311     C                   Z-ADD     1             savRECs2
163900050309     C** CARICO SUBFILE in ARRIVO
164000050216     C                   SETOFF                                       302021
164100050216     C                   Z-ADD     L6(1)         COMFIL
164200050216     C                   Z-ADD     1             D                 3 0
164300050309     C
164400050309     C                   IF        NOT *IN94
164500050216     C     COMFIL        SETLL     FNARB007
164600050309     C                   EXSR      CARSFARR
164700050309     C                   ELSE
164800050309     C     COMFIL        SETLL     FNBLP007
164900050309     C                   EXSR      CARSFPART
165000050309     C                   ENDIF
165100050216     c
165200050216     c                   if        nrrs2=0
165300050216     c                   seton                                        20
165400050216     c                   endif
165500050216     c                   if        NRRC2=0
165600050216     c                   seton                                        21
165700050216     c                   endif
165800050216     c                   endsr
165900900515     C*
166000050309     C*--- CARICO SUBFILE IN ARRIVO ----------------------------------*
166100050309     C     CARSFARR      BEGSR
166200911028     C*
166300050110    0C                   DOW       not *in30
166400911028     C                   SETON                                            31
166500950807    1C     *IN31         DOWEQ     *ON
166600050110     C     COMFIL        READE     FNARB007                               31
166700911028     C*
166800911029     C   31              ADD       1             D
166900950807    2C   31L6(D)         IFNE      0
167000941217     C                   Z-ADD     L6(D)         COMFIL            3 0
167100050110     C     COMFIL        SETLL     FNARB007
167200950807   X2C                   ELSE
167300911028     C                   SETON                                        30
167400911028     C                   SETOFF                                       31
167500950807    2C                   ENDIF
167600950807    1C                   ENDDO
167700911024     C*
167800050110     c* solo se non ho finito di leggere tutte le bolle per la £6
167900050110     c                   if        not *in30
168000911203     C                   SETOFF                                       3233
168100911203     C                   MOVEL     ' '           ABB               1
168200050110      *----
168300050110     C* FLAG ASSEGNATO = BLANKS
168400050110      *----
168500050110    1C     ARBACA        IFNE      *BLANKS
168600911203     C                   SETON                                        32
168700950807    1C                   END
168800050110      *----
168900050110      * Ragione sociale destinatario
169000050110      *----
169100050110     c                   if        vidrsd<>*blanks
169200050110     C     wposizrsd     SUBST(P)  ARBRSD        COMRAG           15
169300050110      * Se rag.soc. letta > video riposiziono solo per data con SETGT
169400050111     C     COMRAG        IFne      VidRSD
169500050110     c                   seton                                        32
169600050110     c                   endif
169700050110     c                   endif
169800020703      *----
169900020703      * BOLLE DI RECUPERO
170000020703      *----
170100050110     c                   if        not *in32
170200020703     c                             and vidbrc <> 'N'
170300050309     c     arbcbo        lookup    cborec                                 33
170400020703      * E - SOLO NON RECUPERO
170500020703     c                   if        *in33 and vidbrc = 'E'
170600020703     c                   eval      *in32 = *on
170700020703     c                   endif
170800020703      * S - SOLO RECUPERO
170900020703     c                   if        not *in33 and vidbrc = 'S'
171000020703     c                   eval      *in32 = *on
171100020703     c                   endif
171200020703     c                   endif
171300050110     C*----
171400911203     C* BOLLE DI RIENTRO
171500050110     C*----
171600050110     C
171700050110    1C  N32VIDBRN        IFNE      'N'
171800911203     C* E - SOLO LE NON DI RIENTRO
171900950807    2C     VIDBRN        IFEQ      'E'
172000961112    3C     ARBFBR        IFEQ      'S'
172100911203     C                   SETON                                        32
172200950807    3C                   END
172300911203     C*
172400950807   X2C                   ELSE
172500911203     C* S - SOLO DI RIENTRO
172600961112    3C     ARBFBR        IFNE      'S'
172700911203     C                   SETON                                        32
172800950807    3C                   END
172900950807    2C                   END
173000950807    1C                   END
173100050110     C*----
173200911203     C* BOLLE ABBINATE
173300050110     C*----
173400050110     C
173500050110    1C  N32VIDBAB        IFNE      'N'
173600940620     C* VEDO SE BOLLA DI FV ABBINATO:
173700060123     C**** ARBLNP        LOOKUP    L1                                     99
173800060123     c
173900060123     c* Vedo se locale
174000060123     c                   if        arbtfp=arbtfa  or
174100060123     c                             arblnp=arblna
174200060123     c                   seton                                        99
174300060123     c                   else
174400060123     c                   setoff                                       99
174500060123     c                   endif
174600940620     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
174700950807    2C                   SELECT
174800950807     C     ARBAMA        WHENEQ    3
174900911205     C                   MOVEL     'A'           ABB
175000911205     C*
175100950807     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
175200950807     C     ARBAMA        WHENEQ    1
175300950807     C     ARBAMP        ANDEQ     1
175400911203     C                   MOVEL     'A'           ABB
175500950807     C*
175600950807     C* 3) SE LOCALE SEMPRE ABBINATO
175700950807     C     *IN99         WHENEQ    *ON
175800950807     C                   MOVEL     'A'           ABB
175900940620     C*
176000950807   X2C                   OTHER
176100971117     C* REPERISCO INFORMAZIONE SE FOGLIO E' ABBINATO O MENO
176200971117     C                   CLEAR                   DSLV53
176300971117     C                   MOVEL     'A'           D53TFO
176400971117     C                   MOVEL     ARBNFV        D53NFV
176500111130     C                   MOVEL     ARBtfp        D53FGS
176600971117     C                   MOVEL     1             D53NPG
176700040108     C                   MOVEL     'B'           D53ABB
176800971117     C                   MOVEL     ARBLNA        D53LNA
176900971117     C                   MOVEL     ARBTFA        D53TFA
177000971117     C                   CALL      'FNLV53R'
177100971117     C                   PARM                    DSLV53
177200971117     C     D53ERR        IFEQ      ' '
177300971117     C     D53NFA        ANDGT     0
177400971117     C                   MOVEL     'A'           ABB
177500971117     C                   ENDIF
177600921217     C*
177700950807    2C                   ENDSL
177800911203     C* S - SOLO ABBINATE
177900950807    2C     VIDBAB        IFEQ      'S'
178000950807    3C     ABB           IFNE      'A'
178100911203     C                   SETON                                        32
178200950807    3C                   END
178300911203     C*
178400950807   X2C                   ELSE
178500911203     C* E - SOLO NON ABBINATE
178600950807    3C     ABB           IFNE      ' '
178700911203     C                   SETON                                        32
178800950807    3C                   ENDIF
178900950807    2C                   ENDIF
179000950807    1C                   ENDIF
179100050110     C*----
179200911203     C* LINEA DI PARTENZA
179300050110     C*----
179400950807    1C     VIDLP2        IFNE      0
179500911203     C     VIDLP2        ANDNE     ARBLNP
179600911203     C                   SETON                                        32
179700950807    1C                   ENDIF
179800911203     C*
179900050110     C  N32              EXSR      RIEMPI                                        CARICO BOLL
180000050110     c                   endif
180100911025     C*
180200050110    0C                   ENDDO
180300900515     C                   ENDSR
180400050309     C*
180500050309     C*--- CARICO SUBFILE IN PARTENZA --------------------------------*
180600050309     C     CARSFPART     BEGSR
180700050309     C*
180800050309    0C                   DOW       not *in30
180900050309     C                   SETON                                            31
181000050309    1C     *IN31         DOWEQ     *ON
181100050309     C     COMFIL        READE     FNblp007                               31
181200050309     C*
181300050309     C   31              ADD       1             D
181400050309    2C   31L6(D)         IFNE      0
181500050309     C                   Z-ADD     L6(D)         COMFIL            3 0
181600050309     C     COMFIL        SETLL     FNblp007
181700050309   X2C                   ELSE
181800050309     C                   SETON                                        30
181900050309     C                   SETOFF                                       31
182000050309    2C                   ENDIF
182100050309    1C                   ENDDO
182200050309     C*
182300050309     c* solo se non ho finito di leggere tutte le bolle per la £6
182400050309     c                   if        not *in30
182500050314     c                   setoff                                       3233
182600050314     c                   exsr      SCARTAPAR
182700050314     c                   if        wscarta='S'
182800050314     c                   seton                                        32
182900050314     c                   endif
183000050309      *----
183100050309      * Ragione sociale destinatario
183200050309      *----
183300050309     c  n32              if        vidrsd<>*blanks
183400050309     C     wposizrsd     SUBST(P)  ARBRSD        COMRAG           15
183500050309      * Se rag.soc. letta > video riposiziono solo per data con SETGT
183600050309     C     COMRAG        IFne      VidRSD
183700050309     c                   seton                                        32
183800050309     c                   endif
183900050309     c                   endif
184000050309      *----
184100050309      * BOLLE DI RECUPERO
184200050309      *----
184300050309     c                   if        not *in32
184400050309     c                             and vidbrc <> 'N'
184500050309     c     ARBcbo        lookup    cborec                                 33
184600050309      * E - SOLO NON RECUPERO
184700050309     c                   if        *in33 and vidbrc = 'E'
184800050309     c                   eval      *in32 = *on
184900050309     c                   endif
185000050309      * S - SOLO RECUPERO
185100050309     c                   if        not *in33 and vidbrc = 'S'
185200050309     c                   eval      *in32 = *on
185300050309     c                   endif
185400050309     c                   endif
185500050309     C*----
185600050309     C* BOLLE DI RIENTRO
185700050309     C*----
185800050309     C
185900050309    1C  N32VIDBRN        IFNE      'N'
186000050309     C* E - SOLO LE NON DI RIENTRO
186100050309    2C     VIDBRN        IFEQ      'E'
186200050309    3C     ARBFBR        IFEQ      'S'
186300050309     C                   SETON                                        32
186400050309    3C                   END
186500050309     C*
186600050309   X2C                   ELSE
186700050309     C* S - SOLO DI RIENTRO
186800050309    3C     ARBFBR        IFNE      'S'
186900050309     C                   SETON                                        32
187000050309    3C                   END
187100050309    2C                   END
187200050309    1C                   END
187300050309     C*----
187400050309     C* LINEA DI PARTENZA
187500050309     C*----
187600050309    1C     VIDLP2        IFNE      0
187700050309     C     VIDLP2        ANDNE     ARBLNP
187800050309     C                   SETON                                        32
187900050309    1C                   ENDIF
188000050309     C*
188100050309     C  N32              EXSR      RIEMPI
188200050309     c                   endif
188300050309     C*
188400050309    0C                   ENDDO
188500050309     C                   ENDSR
188600050314     c*
188700050314     c* Scarto di bolle specifico per partenza ---------------------------*
188800050314     c     SCARTAPAR     BEGSR
188900050314     C                   clear                   wscarta
189000050314      *----
189100050314     C* scarto bolle franco o assegnati storno o assegnati già tassati
189200050314      *----
189300050314    1C     ARBcbo        lookup    cboass                                 34
189400050314     c                   if        not *in34
189500050314     C                   eval      wscarta='S'
189600050314    1C                   END
189700050314     c* chaino record di tassazione
189800050314     c     ARBcbo        lookup    cboass2                                34
189900050314     c                   if        *in34
190000050314     C                   MOVEL     '2'           WTRC
190100050314     c                   else
190200050314     C                   MOVEL     '1'           WTRC
190300050314     c                   endif
190400050314     C     Karb44        CHAIN(N)  FIAR6000
190500050314     c                   if        %found(fiar601l)
190600050314     c* Se presente AR6 non eseguo tassazione
190700050314     C                   eval      wscarta='S'
190800050314     C                   END
190900050321     c* Se nel frattempo è "partita", non carico
191000050321     c                   if        arbft2<>' '
191100050314     C                   eval      wscarta='S'
191200050314     C                   END
191300050314     c                   endsr
191400900515     C*
191500900515     C* EMISSIONE SUBFILE -------------------------------------------
191600911025     C     RIEMPI        BEGSR
191700050112     C                   MOVEL     *BLANKS       VsfSCE
191800050318     C                   MOVEL     *BLANKS       Vsfdop
191900050113     C                   MOVEL     *BLANKS       Vsfaks
192000050113     C                   MOVEL     *BLANKS       Vsfact
192100050113     C                   MOVEL     *BLANKS       Vs2aks
192200050124     C                   MOVEL     *BLANKS       Vs2act
192300050310     C   94              z-add     blp07nrr      Vsfnrr
192400050112     C                   MOVEL     ARBLNP        VsfLNP
192500050112     C                   MOVEL     ARBLNA        VsfLNA
192600911023     C                   MOVEL     ARBMGS        MMGG
192700050112     C                   MOVEL     GG            VsfDSP
192800050112     C                   MOVE      MM            VsfDSP
192900050112     C                   MOVE      ARBAAS        VsfAAS
193000900516     C*
193100050112     C                   MOVEL     ARBNSP        VsfNSP
193200050112     C                   MOVEL     ARBind        Vsfind
193300050112     c                   if        arbnzd<>*blanks
193400050112     C                   MOVEL     ARBnzd        Vsfprd
193500050112     c                   else
193600050112     C                   MOVEL(P)  ARBprd        Vsfprd
193700050112     c                   endif
193800050112     C                   MOVEL     ARBRSM        VsfRSM
193900050112     C                   MOVEL     ARBNRS        VsfNRS
194000050112     C                   MOVEL     ARBtsp        Vsftsp
194100050112     c* Campi di lunghezza diversa nei 2 sfl
194200050112     C                   MOVEL     ARBRSD        Vs2RSD
194300050112     C                   MOVEL     ARBlod        Vs2lod
194400050112     C                   MOVEL     ARBRSD        Vc2RSD
194500050112     C                   MOVEL     ARBlod        Vc2lod
194600050110     c
194700050110     c* controllo se con codice o con "*" o senza codice
194800050110     C                   MOVEL     'A'           WTRC
194900060217     C     KARB4         CHAIN(N)  Fiar401L                           32
195000050110     C  N32              MOVEL     AR4NOT        DSBL4A
195100050110     C   32              CLEAR                   DSBL4A
195200911113     C* COD BOLLA
195300911113     C                   MOVEL     '3A'          COD
195400911113     C                   MOVEL     *BLANKS       KEY
195500941216     C                   MOVEL     ARBCBO        KEY
195600911113     C     KTAB          CHAIN     TABEL                              34
195700911113     C  N34              MOVEL     TBLUNI        DS3A
195800911113     C                   MOVEL     §3ATB1        UNOTB1            1
195900050203     C                   MOVEL     §3Asva        vsfsva
196000051024     c
196100051024     c* Memorizzo se esiste importo da assicurare solo per tipi bolla che
196200051024     c*  lo prevedono solo per 1°bolla
196300051024     c     §3atb1        lookup    tbl                                    34
196400051024     c                   clear                   vsfesiias
196500051024     c                   if        *in34 and unotb1='A' and arbcbo<>'FW'
196600051024     c                   if        arbias>0
196700051024     c                   eval      vsfesiias='S'
196800051024     c                   else
196900051024     c                   eval      vsfesiias='N'
197000051024     c                   endif
197100051024     c                   endif
197200930430     C* TIPO BOLLA
197300911113     C     UNOTB1        IFEQ      'A'
197400050112     C                   MOVEL     §3ATB1        VsfTBL
197500930430     C*
197600911113     C                   ELSE
197700930430     C*
197800050112     C                   MOVEL     §3ATB2        VsfTBL
197900050318     C*
198000050318     c                   if        §3atb2<>*blanks
198100050318     c                   eval      vsfdop='S'
198200050318     c                   endif
198300941216     C*
198400930430     C* SE NON 'C'E 2 BOLLA E DEVO TASSARE ASSEGNATI ABILITO SUBITO
198500050309     C  N94§3ATB2        IFEQ      '  '
198600941217     C     KARB2         CHAIN     FNARB000                           34
198700930430     C  N34              MOVEL     'S'           ARBACA
198800941217     C  N34              UPDATE    FNARB000
198900930430     C                   END
199000911113     C                   END
199100941216     C* A S S E G N A T O
199200941216     C*
199300050309     C***  ARBACA        IFEQ      ' '
199400050309     C                   IF        NOT *IN94 AND ARBACA=' '  OR *IN94
199500050309     C
199600961112     C     ARBFBR        IFEQ      'S'
199700050112     C                   eval      vsffbr='Rientro'                              RIENTRO
199800961112     C                   ELSE
199900050112     C                   CLEAR                   VsfFBR
200000961112     C                   ENDIF
200100941216     C* PRENDO LE NOTE
200200941216     C                   MOVEL     '8'           WTRC
200300060217     C     KARB4         CHAIN(n)  FiAR4000                           32
200400050309     C
200500050112     C  N32              MOVEL     AR4NOT        VsfNOT
200600050112     C   32              CLEAR                   VsfNOT
200700050112     c* codice in bl4a
200800050112     c                   if        §b4aks>0
200900050112     c                   movel     §b4aks        vs2aks
201000050112     c                   movel     §b4aks        vsfaks
201100050113     c                   movel     §b4act        vsfact
201200050124     c                   movel     §b4act        vs2act
201300050112     c                   movel     arblod        w012a            12
201400050112     c                   eval      vc2lod=(w012a+' '+vs2aks)
201500050112     c                   endif
201600050714     c* Vedo se bolla dirottata
201700050714     c                   EXSR      CHIAMAlv58
201800941216     C*
201900050110     c* sfl con codice
202000050202     c                   if        §b4aks>0 and §b4af1<>'S'
202100050309     c                   If        NRRC2>= 9990
202200050110     c                   Eval      wsflpieno22= '1'
202300050309     C                   ELSE
202400050309     C                   ADD       1             NRRC2
202500050317     C                   WRITE     LRA2SC2
202600050309     c                   EndIf
202700050110     c
202800050110     c                   else
202900050110     c* sfl senza codice
203000050125     c* se con codice , metto in HI
203100050125     c                   if        vsfaks<>*blanks
203200050125     c                   seton                                        13
203300050125     c                   endif
203400050125     c
203500050309     c                   If        NRRS2>= 9990
203600050110     c                   Eval      wsflpieno2= '1'
203700050309     C                   ELSE
203800050309     C                   ADD       1             NRRS2
203900050317     C                   WRITE     LRA2SS2
204000050309     c                   EndIf
204100050125     c
204200050125     c                   setoff                                       13
204300050110     c                   endif
204400050110     c
204500941216     C                   ENDIF
204600930430     C*
204700900515     C                   ENDSR
204800050714     c
204900050714     c*
205000050714     c* Richiamo a pgmm di controllo bolla dirottata---------------------*
205100050714     c     CHIAMAlv58    BEGSR
205200050714     c* Imposto i campi come quelli della bolla,
205300050714     c                   eval      VUNICOlnp=vsflnp
205400050714     c                   eval      VUNICOtfp=arbtfp
205500050714     c                   eval      VUNICOnzm=arbnzm
205600050714     c                   eval      VUNICOcam=arbcam
205700050714     c                   eval      VUNICOfap=arbfap
205800060123     c                   eval      VUNICOfdn=arbfdn
205900050714     c* Rimane vuoto il cod tassazione mittente perchè non c'è in bolla
206000050714     c                   clear                   VUNICOmct
206100050715     c                   clear                   Vsfdir
206200050714     c**
206300050714     c**
206400050714     c*  Se devo considerare i dirottamenti e c'e' legame,
206500050714     c*  allora li imposto = alla bolla mamma
206600050715    1c                   if        §vpodir='A' or §vpodir='E'
206700050714     c
206800050714     c                   clear                   fnlv58ds
206900050714     c                   eval      ilv58tle='1'
207000050714     c                   eval      ilv58aas=arbaas
207100050714     c                   eval      ilv58lnp=arblnp
207200050714     c                   eval      ilv58nrs=arbnrs
207300050714     c                   eval      ilv58nsp=arbnsp
207400050714     c                   eval      ilv58por=vsftbl
207500050714     c                   call      'FNLV58R'
207600050714     c                   parm                    fnlv58ds
207700050714     c
207800050926    2c                   if        olv58nsp>0  and (olv58oumfi='O ' or
207900050926     c                             olv58oumfi='MP')
208000050715     c                   eval      vsfdir='S'
208100060117     c                   eval      v1dleg=C_Dirotta
208200060117     c*
208300060117   x2c                   else
208400060117     c* Verifico se cambio porto
208500060117     c                   eval      ilv58tle='9'
208600060117     c                   call      'FNLV58R'
208700060117     c                   parm                    fnlv58ds
208800060117     c
208900060117    3c                   if        olv58nsp>0  and (olv58oumfi='O ' or
209000060117     c                             olv58oumfi='MP')
209100060117     c                   eval      vsfdir='C'
209200060117     c                   eval      v1dleg=C_Cambio
209300060117     c* Se la bolla trovata non è l'originale, potrebbe essere una dirotta
209400060117     c*  ta per cui tasso dall'originale
209500060117    4c                   if        olv58nsp>0  and olv58oumfi='MP'
209600060117     c                   eval      ilv58tle=' '
209700060117     c                   call      'FNLV58R'
209800060117     c                   parm                    fnlv58ds
209900060117    4c                   endif
210000060123   x3c                   else
210100060123     c* Verifico se bolla legata da giacenza
210200060123     c                   eval      ilv58tle='G'
210300060123     c                   call      'FNLV58R'
210400060123     c                   parm                    fnlv58ds
210500060123     c
210600060123    4c                   if        olv58nsp>0  and (olv58oumfi='O ' or
210700060123     c                             olv58oumfi='MP')
210800060123     c                   eval      vsfdir='G'
210900060123     c                   eval      v1dleg=C_Legame
211000060123     c* Se la bolla trovata non è l'originale, potrebbe essere una dirotta
211100060123     c*  ta per cui cerco l'eventuale mamma dirottata
211200060123    5c                   if        olv58nsp>0  and olv58oumfi='MP'
211300060123     c                   movel     fnlv58ds      wfnlv58ds
211400060123     c                   clear                   fnlv58ds
211500060123     c                   eval      ilv58tle='1'
211600060123     c                   eval      ilv58aas=wolv58aas
211700060123     c                   eval      ilv58lnp=wolv58lnp
211800060123     c                   eval      ilv58nrs=wolv58nrs
211900060123     c                   eval      ilv58nsp=wolv58nsp
212000060123     c                   call      'FNLV58R'
212100060123     c                   parm                    fnlv58ds
212200060123    6c                   if        olv58nsp>0  and (olv58oumfi='O ' or
212300060123     c                             olv58oumfi='MP')
212400060123   x6c                   else
212500060123     c* se non trovato nulla reimposto i dati nella DS
212600060123     c                   movel     wfnlv58ds     fnlv58ds
212700060123    6c                   endif
212800060123    5c                   endif
212900060123    4c                   endif
213000060123    3c                   endif
213100060117    2c                   endif
213200060117     c
213300060117    2c                   if        vsfdir<>' '
213400050715     c                   eval      VUNICOlnp=olv58lnp
213500050714     c                   eval      VUNICOtfp=olv58tfp
213600050714     c                   eval      VUNICOmct=olv58mct
213700050714     c                   eval      VUNICOfap=olv58fap
213800050714     c                   eval      VUNICOcam=olv58cam
213900050714     c                   eval      VUNICOnzm=olv58nzm
214000060123     c                   eval      VUNICOfdn=olv58fdn
214100050715    2c                   endif
214200050715    1c                   endif
214300050714     c
214400050714     c                   ENDSR
214500900515     C*
214600900515     C* PULIZIA SUBFILE ---------------------------------------------
214700900515     C     PULSF         BEGSR
214800050110     C                   SETON                                        352021
214900050317     C                   WRITE     LRA2CC2
215000050317     C                   WRITE     LRA2CS2
215100050110     C                   SETOFF                                       352021
215200900515     C                   ENDSR
215300911025     C*
215400050110     C* GESTIONE sfl assegnati CON codice ---------------------------*
215500050110     c     SFLCON        BEGSR
215600050216     c* Flag che mi dice se ho tassato qualche cosa per ricaricare i sfl
215700050216     c                   clear                   whotassato        1
215800050110     C
215900050110      * se i record scritti nel sfl degli assegnati supera i 9900 emetto msg di errore
216000050110If  1c                   If        wsflpieno22<> *Blanks
216100050110     c                   Eval      *In18 = *On
216200050110     c                   Eval      *In28 = *On
216300050110     c                   Eval      VidMsg = Msg(59)
216400050317     c     NRRC2         Chain     LRA2SC2                            34
216500050110     c                   Eval      *In36 = *On
216600050317     c                   Update    LRA2SC2
216700050110     c                   Eval      *In36 = *Off
216800050110     c                   Eval      *In18 = *Off
216900050110    1c                   EndIf
217000050127     c
217100050127     c                   if        savrecc2>0
217200050114     c                   z-add     savrecc2      rec
217300050127     c                   else
217400050127     c                   z-add     1             rec
217500050127     c                   endif
217600050110     C
217700050112     C     FORCTC2       TAG
217800050317     C                   WRITE     LRA2ZC2
217900050317     C   21              WRITE     LRA2Dc2
218000050317     C                   EXFMT     LRA2CC2
218100050110     C**
218200050110     C                   SETOFF                                       2890
218300050110     C* F3 - FINE
218400050110     C   KC              GOTO      ENDSFLCON
218500050110     C**
218600050110     C* F12 - RITORNO
218700050110     C   KL              GOTO      ENDSFLCON
218800050111     c* F8 - Assegnati senza codice
218900050111     c                   if        *inkh
219000050114     C                   Z-ADD     PRIMA         SAVRECc2          4 0
219100050110     c                   eval      wtiposfl='S'
219200050110     c                   goto      endsflcon
219300050110     c                   endif
219400061122     c
219500061123     c* Se sfl vuoto, permetto solo F8, F12, F3
219600061123     c                   if        *in21
219700061123     c                   goto      forctc2
219800061123     c                   endif
219900050113     c* Leggo in readc il sfl per caricarmi la schiera delle opzioni
220000050317     c                   readc     LRA2sc2                                34
220100050113     c                   dow       not *in34
220200050113     c                   eval      sksflcon(nrrc2)=vsfsce
220300050126     c* se scelta a ' ' --> aggiorno sfl spegnendo indicatori di posiz e RI
220400050126     c                   if        vsfsce=' '
220500050126     c                   setoff                                       1816
220600050317     c                   update    LRA2sc2
220700050126     c                   endif
220800050317     c                   readc     LRA2sc2                                34
220900050113     c                   enddo
221000050127     c
221100050127     C* SALVO IL POSIZIONAMENTO CURSORE
221200050127     c                   if        prima=0
221300050127     c                   z-add     nrrc2         savrecc2
221400050127     c                   else
221500050127     C                   Z-ADD     PRIMA         SAVRECc2          4 0
221600050127     c                   endif
221700050113     C*
221800050113     C* SE PREMUto F1 impossibile impostare opzioni
221900050113    2c                   if        *inka
222000050113     C                   MOVEA     SKSFLCON      skiopz
222100050113     c                   sorta     skiopz
222200050113     c
222300050113     c                   if        skiopz(9902)<>*blanks
222400050113     C                   MOVEL     MSG(62)       VIDMSG
222500050113     C                   SETON                                        2890
222600050113     c                   goto      forctc2
222700050113     C                   ENDIF
222800050121     c
222900050121     c* F1 - conferma totale leggo tutto il sfl e elaboro con 'X'
223000050121     c                   exsr      CONFTOTALE
223100050121     c
223200050121     c                   exsr      SFLERROR
223300050121     c
223400050121     C  N90              Z-ADD     SAVRECc2      REC
223500050121     C                   GOTO      FORCTC2
223600050121     C                   ENDIF
223700050110     C**
223800050113     c                   movea     sksflcon      skiopz
223900050112     c
224000050112     C* LETTURA SUBFILE  : opzione 5 - interrogazione
224100050112     c                   exsr      Opzione5
224200050114     C**
224300050114     C* LETTURA SUBFILE  : opzione 1 - singola
224400050114     c                   exsr      Opzione1
224500050120     C**
224600050120     C* LETTURA SUBFILE  : opzione X - singola senza far vedere le videate
224700050120     c                   exsr      OpzioneX
224800050110     C**
224900050112     C* LETTURA SUBFILE  : opzione 9 - tassazione multipla
225000050112     c                   exsr      Opzione9
225100050113     c                   movea     skiopz        sksflcon
225200050112     c   90              goto      forctc2
225300050110     C*
225400050110     C* ERRORE
225500050120     c                   exsr      SFLERROR
225600050110     C*
225700050113     C  N90              Z-ADD     SAVRECc2      REC
225800050112     C                   GOTO      FORCTC2
225900050110     C     endsflcon     ENDSR
226000050110     C*
226100050110     C* GESTIONE sfl assegnati SENZA codice -------------------------*
226200050110     c     SFLSENZA      BEGSR
226300050216     c* Flag che mi dice se ho tassato qualche cosa per ricaricare i sfl
226400050216     c                   clear                   whotassato
226500050110     C
226600050110      * se i record scritti nel sfl degli assegnati supera i 9900 emetto msg di errore
226700050110If  1c                   If        wsflpieno2<> *Blanks
226800050110     c                   Eval      *In18 = *On
226900050110     c                   Eval      *In28 = *On
227000050110     c                   Eval      VidMsg = Msg(59)
227100050317     c     NRRS2         Chain     LRA2SS2                            34
227200050110     c                   Eval      *In36 = *On
227300050317     c                   Update    LRA2SS2
227400050110     c                   Eval      *In36 = *Off
227500050110     c                   Eval      *In18 = *Off
227600050110    1c                   EndIf
227700050127     c
227800050127     c                   if        savrecs2>0
227900050114     c                   z-add     savrecs2      rec
228000050127     c                   else
228100050127     c                   z-add     1             rec
228200050127     c                   endif
228300050110     C
228400050113     C     FORCTs2       TAG
228500050317     C                   WRITE     LRA2Zs2
228600050317     C   20              WRITE     LRA2DS2
228700050317     C                   EXFMT     LRA2CS2
228800050110     C**
228900050110     C                   SETOFF                                       2890
229000050110     C* F3 - FINE
229100050110     C   KC              GOTO      ENDSFLSENZA
229200050110     C**
229300050110     C* F12 - RITORNO
229400050110     C   KL              GOTO      ENDSFLSENZA
229500050111     c* F8 - Assegnati con   codice
229600050111     c                   if        *inkh
229700050114     C                   Z-ADD     PRIMA         savrecs2
229800050110     c                   eval      wtiposfl='C'
229900050110     c                   goto      endsflSENZA
230000050110     c                   endif
230100061123     c
230200061123     c* Se sfl vuoto, permetto solo F8, F12, F3
230300061123     c                   if        *in20
230400061123     c                   goto      forcts2
230500061123     c                   endif
230600061123     c
230700050113     c* Leggo in readc il sfl per caricarmi la schiera delle opzioni
230800050317     c                   readc     LRA2ss2                                34
230900050113     c                   dow       not *in34
231000050113     c                   eval      sksflsen(nrrs2)=vsfsce
231100050126     c                   if        vsfsce=' '
231200050127     c                   setoff                                       1816
231300050317     c                   update    LRA2ss2
231400050126     c                   endif
231500050317     c                   readc     LRA2ss2                                34
231600050113     c                   enddo
231700050110     C**
231800050110     C* SALVO IL POSIZIONAMENTO CURSORE
231900050127     c                   if        prima>0
232000050113     C                   Z-ADD     PRIMA         SAVRECs2          4 0
232100050127     c                   else
232200050127     c                   z-add     nrrs2         savrecs2
232300050127     c                   endif
232400050127     c
232500050113     c                   movea     sksflsen      skiopz
232600050113     c
232700050113     C* LETTURA SUBFILE  : opzione 5 - interrogazione
232800050113     c                   exsr      Opzione5
232900050114     c*
233000050114     C* LETTURA SUBFILE  : opzione 1 - tassazione singola
233100050114     c                   exsr      Opzione1
233200050113     C**
233300050113     C* LETTURA SUBFILE  : opzione 9 - tassazione multipla
233400050113     c                   exsr      Opzione9
233500050113     c                   movea     skiopz        sksflsen
233600050113     c   90              goto      forcts2
233700050113     c
233800050110     C* ERRORE
233900050120     c                   EXSR      SFLERROR
234000050110     C*
234100050113     C  N90              Z-ADD     SAVRECs2      REC
234200050113     C                   GOTO      FORCTs2
234300050110     C     endsflsenza   ENDSR
234400050121     c*
234500050926     c* Conferma totale tassazione tutte le bolle sflcon con la 'X' -------*
234600050121     c     CONFTOTALE    BEGSR
234700050121     c                   z-aDD     1             nrrc2
234800050317     C     nrrc2         chain     LRA2sc2                            34
234900050121     c
235000050121    1C     *IN34         DOWEQ     *Off
235100050121     c
235200050121     c* Leggo bolla per poi aggiornarla
235300050311     C  n94KARB2         CHAIN     FNARB000                           3295
235400050311     c   94vsfnrr        chain     fnblpfis                           3295
235500050121     c                   z-add     nrrc2         pp
235600050121     c                   movel     'X'           vsfsce
235700050121     c
235800050121     c                   EXSR      ELAARB
235900050121     C*
236000050121     c                   add       1             nrrc2
236100050317     C     nrrc2         chain     LRA2sc2                            34
236200050121    1c                   enddo
236300050121     c
236400050121     c                   ENDSR
236500050112     c*
236600050112     c* Opzione 5 del sfl: visualizzazione bolla ------------------------*
236700050112     c     OPZIONE5      BEGSR
236800050113     c                   z-aDD     1             PP                5 0
236900050113     C     '5'           lookup    skiopz(PP)                             34
237000050112     c
237100050113    1C     *IN34         DOWEQ     *On
237200050113     c
237300050113     c                   exsr      CHAINSFL
237400050309     c
237500050309     c                   exsr      INTBOLLE
237600050113     c*
237700050113     c
237800050926     c* imposto il flag di ignorare arbaca
237900050209     c                   eval      wusaaca='N'
238000050311     c                   clear                   waggiobol
238100050113     c                   exsr      AGGIOSFL
238200050209     c                   eval      wusaaca=' '
238300050113     c
238400050113     c                   add       1             PP
238500050113     C     '5'           lookup    skiopz(PP)                             34
238600050112    1c                   enddo
238700050113     c
238800050112     c                   ENDSR
238900050114     c*
239000050114     c* Opzione 1 del sfl: tassazione singola ---------------------------*
239100050114     c     OPZIONE1      BEGSR
239200050114     c                   z-aDD     1             PP                5 0
239300050114     C     '1'           lookup    skiopz(PP)                             34
239400050114     c
239500050114    1C     *IN34         DOWEQ     *On
239600050114     c
239700050114     c* Leggo bolla per poi aggiornarla
239800050114     c                   exsr      LEGARB
239900050121     c
240000050121     c                   exsr      ELAARB
240100050114     C*
240200050330     c* Se  c'e' stato invio automatico devo per forza riemettere il sfl
240300050330     c                   if        %error
240400050330     c                   setoff                                       34
240500050330     c                   else
240600050114     c                   add       1             PP
240700050114     C     '1'           lookup    skiopz(PP)                             34
240800050330     c                   endif
240900050114    1c                   enddo
241000050114     c
241100050114     c                   ENDSR
241200050120     c*
241300050120     c* Opzione X del sfl: tassazione singola senza videate -------------*
241400050120     c     OPZIONEX      BEGSR
241500050120     c                   z-aDD     1             PP                5 0
241600050120     C     'X'           lookup    skiopz(PP)                             34
241700050120     c
241800050120    1C     *IN34         DOWEQ     *On
241900050120     c
242000050120     c* Leggo bolla per poi aggiornarla
242100050120     c                   exsr      LEGARB
242200050121     c
242300050121     c                   exsr      ELAARB
242400050120     C*
242500050120     c                   add       1             PP
242600050120     C     'X'           lookup    skiopz(PP)                             34
242700050120    1c                   enddo
242800050120     c
242900050120     c                   ENDSR
243000050112     c*
243100050112     c* Opzione 9 del sfl: Tassazione multimpla -------------------------*
243200050112     c     OPZIONE9      BEGSR
243300050112     c* Devo fare 2 giri di lettura:
243400050112     c
243500050112     c* 1-Memorizzare codice di tassazione e vedere se uguale su tutte le
243600050112     c*   bolle. se <> dare errore forzabile. se cambia num bolle col 9
243700050112     c*   rifare controllo
243800050125     c                   clear                   tas9kc
243900050125     c                   clear                   tas9k
244000050112     c                   clear                   tas9n
244100050125     c                   clear                   tas9nk
244200050112     c                   clear                   tas9t
244300051024     c                   clear                   tas9siias         1
244400051024     c                   clear                   tas9noias         1
244500050112     c                   clear                   wcontabolle
244600050126     c                   clear                   wtas9ntw
244700050203     c                   clear                   wtas9sva
244800050318     c                   clear                   errmult
244900050113     c
245000050113     c                   z-aDD     1             PP                5 0
245100050113     C     '9'           lookup    skiopz(PP)                             34
245200050113     c
245300050113    1C     *IN34         DOWEQ     *On
245400050113     c
245500050203     c                   clear                   waggerr
245600050112     c                   add       1             wcontabolle       7 0
245700050113     c* NRR della pagina da visualizzare a video
245800050126     c                   z-add     PP            wtas9rec
245900050113     c                   exsr      CHAINSFL
246000050126     c
246100050126     c* se tipo servizio FEDEX , errore vincolante
246200160226     c* richiamo bolla x bolla trul27r1
246300160226     c                   clear                   trul27ds1
246400050126     c                   eval      i27tsp = vsftsp
246500050126     c                   eval      i27lna = vsflna
246600050714     c****               eval      i27lnp = vsflnp
246700050714     c                   eval      i27lnp=  VUNICOlnp
246800050714     c
246900050126     c                   if        vsfaks<>*blanks
247000050126     c                   movel     vsfaks        i27cli
247100050126     c                   endif
247200050126
247300160226     c                   call      'TRUL27R1'
247400160226     c                   parm                    trul27ds1
247500050126    2c                   if        o27ntw='FED'
247600050126     c                   seton                                        1890
247700050318     c                   eval      errmult='ERR'
247800050126     c                   eval      wtas9ntw='FED'
247900050126    3c                   if        errrec=0
248000050126     c                   z-add     pp            errrec
248100050318     c                   eval      errmult='NTW'
248200050126    3c                   endif
248300050203     c                   eval      waggerr='S'
248400050126   x2c                   else
248500050126     c
248600050203     c* Devono avere stesso NTW le bolle in tassazione multipla
248700050203    3c                   if        wtas9ntw='   '
248800050203     c                   eval      wtas9ntw=o27ntw
248900050126   x3c                   else
249000050126    4c                   if        o27ntw<>wtas9ntw
249100050126     c                   seton                                        1890
249200050318     c                   eval      errmult='ERR'
249300050126    5c                   if        errrec=0
249400050126     c                   z-add     pp            errrec
249500050318     c                   eval      errmult='NTW'
249600050126    5c                   endif
249700050203     c                   eval      waggerr='S'
249800050126    4c                   endif
249900050126    3c                   endif
250000050126    2c                   endif
250100050126     c
250200050203     c* Devono essere tutte monovaria o non monovaria
250300050203    2c                   if        wcontabolle=1
250400050203     c                   eval      wtas9sva=vsfsva
250500050203   x2c                   else
250600050203    3c                   if        vsfsva<>wtas9sva
250700050318     c                   eval      errmult='ERR'
250800050203     c                   seton                                        1890
250900050203    4c                   if        errrec=0
251000050203     c                   z-add     pp            errrec
251100050318     c                   eval      errmult='SVA'
251200050203    4c                   endif
251300050203     c                   eval      waggerr='S'
251400050203    3c                   endif
251500050203    2c                   endif
251600050318     c* Se bolla doppia, non posso fare tassazione multipla
251700050318    2c                   if        vsfdop='S'
251800050318     c                   eval      errmult='ERR'
251900050318     c                   seton                                        1890
252000050318    3c                   if        errrec=0
252100050318     c                   z-add     pp            errrec
252200050318     c                   eval      errmult='DOP'
252300050318    4c                   endif
252400050318     c                   eval      waggerr='S'
252500050318    2c                   endif
252600050203     c
252700050203     c                   if        waggerr='S'
252800050203     c                   exsr      aggiosfl
252900050203     c                   endif
253000050112     c
253100050112     c* Memorizzo il tipo servizio delle bolle
253200050112     c     vsftsp        lookup    tas9t                                  32
253300050113    3c                   if        not *in32
253400050112     c                   z-add     1             aa                3 0
253500050112     c     ' '           lookup    tas9t(AA)                              32
253600050112     c                   if        *in32
253700050112     c                   movel     vsftsp        tas9t(aa)
253800050112     c                   endif
253900050113    3c                   endif
254000050112     c
254100050112    3c                   if        vsfaks<>*blanks
254200050125     c* Memorizzo sia ksc+ctr che ksc da solo
254300050112     c                   z-add     1             aa                3 0
254400050113     c                   movel     vsfaks        w010a            10
254500050113     c                   move      vsfact        w010a            10
254600050125     c     w010a         lookup    tas9kc(AA)                             32
254700050112    4c                   if        *in32
254800050113     c                   add       1             tas9n(aa)
254900050112   x4c                   else
255000050112     c                   z-add     1             aa                3 0
255100050125     c     '          '  lookup    tas9kc(aa)                             32
255200050112    5c                   if        *in32
255300050125     c                   eval      tas9kc(aa)=w010a
255400050113     c                   z-add     1             tas9n(aa)
255500050112    5c                   endif
255600050112    4c                   endif
255700050125     c* solo codice
255800050125     c                   z-add     1             aa                3 0
255900050125     c     vsfaks        lookup    tas9k(AA)                              32
256000050125    4c                   if        *in32
256100050125     c                   add       1             tas9nk(aa)
256200050125   x4c                   else
256300050125     c                   z-add     1             aa                3 0
256400050125     c     '       '     lookup    tas9k(aa)                              32
256500050125    5c                   if        *in32
256600050125     c                   eval      tas9k(aa)=vsfaks
256700050125     c                   z-add     1             tas9nk(aa)
256800050125    5c                   endif
256900050125    4c                   endif
257000050112    3c                   endif
257100051024     c* Memorizzo se almeno una bolla     ha importo da assicurare
257200051024     c                   if        vsfesiias='S'
257300051024     c                   eval      tas9siias='S'
257400051024     c                   endif
257500051024     c* Memorizzo se almeno una bolla non ha importo da assicurare
257600051024     c                   if        vsfesiias='N'
257700051024     c                   eval      tas9noias='S'
257800051024     c                   endif
257900050112     c
258000050113     c                   add       1             PP
258100050113     C     '9'           lookup    skiopz(PP)                             34
258200050112    1c                   enddo
258300050126     c
258400050126     c* Se trovati errori vincolanti riemetto bideata
258500050126    0c                   if        *in90
258600050126     c                   seton                                        28
258700050203     c                   select
258800050318     c                   when      errmult='ERR'
258900050203     c                   eval      vidmsg=msg(73)
259000050203     c
259100050318     c                   when      errmult='SVA'
259200050203     c                   eval      vidmsg=msg(72)
259300050318     c
259400050318     c                   when      errmult='DOP'
259500050318     c                   eval      vidmsg=msg(77)
259600050203     c
259700050203     c                   when      wtas9ntw='FED'
259800050126     c                   eval      vidmsg=msg(70)
259900050203     c                   other
260000050126     c                   eval      vidmsg=msg(71)
260100050203     c                   endsl
260200050126     c                   z-add     errrec        rec
260300050126     c                   clear                   errrec
260400050126   x0c                   else
260500050112     c
260600050112     c* Se variato numero bolle torno a dare msg forzabile se + codici
260700050112    1c                   if        wcontabolle>0
260800050112     c
260900050112    2c                   if        wcontabolle<>wsalvaconta
261000050112     c                   clear                   wforzatas9
261100050112    2c                   endif
261200050112     c
261300050112     c                   eval      wsalvaconta=wcontabolle
261400050125     c* Errore forzabile  se scelte bolle con <> codici
261500050125    2c                   if        tas9kc(2)<>*blanks  and wforzatas9=' '
261600050125    3c                   select
261700050125     c* sicuramente errore se ci sono 3 <> cli+tar
261800050125     c                   when      tas9kc(3)<>*blanks
261900050125     c                   seton                                        2890
262000050125     c* Se c'è più di un codcliente errore
262100050125     c                   when      tas9k(2)<>*blanks
262200050112     c                   seton                                        2890
262300050125     c                   other
262400050125     c* se entrambi i 2 codici hanno il cod tariffa --> errore
262500050125     c                   move      tas9kc(1)     wcodtar1
262600050125     c                   move      tas9kc(2)     wcodtar2
262700050125    4c                   if        wcodtar1<>*blanks and wcodtar2<>*blanks
262800050125     c                   seton                                        2890
262900050125    4c                   endif
263000050125    3c                   endsl
263100050125     c
263200050125     c
263300050125    4c                   if        *in90
263400050112     c                   eval      vidmsg=msg(63)
263500050112     c                   eval      wforzatas9='1'
263600050126     c                   z-add     wtas9rec      rec
263700050125    4c                   endif
263800050125     c
263900050112    2c                   endif
264000050112     c
264100050112     c* 2-Procedo alla tassazione se non emesso msg forzabile
264200050113    2c                   if        not *in90
264300050112     c
264400050113     c                   z-aDD     1             PP                5 0
264500050113     C     '9'           lookup    skiopz(PP)                             34
264600050113     c
264700050113    3C     *IN34         ifeq      *On
264800050121     c* Leggo la prima bolla per proporre videata
264900050121     c                   exsr      CHAINSFL
265000050121     c
265100050310     C  n94KARB2         CHAIN(N)  FNARB000                           32
265200050310     C   94vsfnrr        CHAIN(N)  FNblpfis                           32
265300050112     C
265400050112     c                   eval      wscelta=vsfsce
265500050112     C  N32              EXSR      ELAB                                          ELABORO
265600050113    3C                   ENDiF
265700050110     c
265800050113    2c                   endif
265900050113    1c                   endif
266000050126    0c                   endif
266100050112     c
266200050112     c                   ENDSR
266300050113     c*
266400050113     c* Chain su2 2 sfl a seconda di dove sono -------------------------*
266500050113     c     CHAINSFL      BEGSR
266600050113    2c                   if        wtiposfl='C'
266700050113     c                   z-add     pp            nrrc2
266800050317     c     nrrc2         chain     LRA2sc2
266900050113     c                   else
267000050113     c                   z-add     pp            nrrs2
267100050317     c     nrrs2         chain     LRA2ss2
267200050113    2c                   endif
267300050113     c                   ENDSR
267400050113     c*
267500050113     c* chain su sfl e lettura fnarb000 ----------------------------------
267600050113     c     LEGARB        BEGSR
267700050113     c                   exsr      CHAINSFL
267800050113     c
267900050310     c* In Arrivo
268000050310     c                   if        not *in94
268100050113     C     KARB2         CHAIN     FNARB000                           3295
268200050310     c                   else
268300050314     c* In Partenza
268400050310     c     vsfnrr        chain     fnblpfis                           3295
268500050310     c                   endif
268600050113     c
268700050113     c                   ENDSR
268800050113     c*
268900050113     c* Aggiornamento dei 2 sfl ------------------------------------------
269000050113     c     AGGIOSFL      BEGSR
269100050310     c* In Arrivo
269200050121     C* proteggo bolla già elaborata  se non c'è errore
269300050209     C*  e se posso controllare il flag arbaca (no per opz 5)
269400050330     c  n94              if        wusaaca=' '
269500050121     C  n18ARBACA        IFNE      ' '
269600050113     C                   SETON                                        19
269700050216     c                   eval      whotassato='S'
269800050113     C                   END
269900050209     C                   ENDif
270000050310     c
270100050310     c* In partenza
270200050310     c                   if        *in94 and waggiobol='S'
270300050310     C                   SETON                                        19
270400050310     c                   eval      whotassato='S'
270500050310     C                   ENDif
270600050310     c
270700050114     c
270800050114     c* Per 18 on - errore --> imposto la E nel campo scelta del sfl
270900050114     c                   if        not *in18
271000050330     c* pulisco solo se no invio automatico
271100050330     c                   if        not %error
271200050113     C                   MOVEL     *BLANKS       VsfSCE
271300050330     c                   endif
271400050330     c
271500050114     c                   else
271600050114     C                   MOVEL     'E'           VsfSCE
271700050114     c                   endif
271800050114     c
271900050113    2c                   if        wtiposfl='C'
272000050317     C                   UPDATE    LRA2SC2
272100050113     c                   else
272200050215     c* Accendo il 13 se con codice
272300050215     c                   if        vsfaks<>*blanks
272400050215     c                   seton                                        13
272500050215     c                   endif
272600050215     c
272700050317     C                   UPDATE    LRA2Ss2
272800050113     c                   endif
272900050215     c
273000050113     C                   SETOFF                                       191816
273100050215     C                   SETOFF                                       13
273200050113     c
273300050113     c* pulisco relativa posizione dell'opzione
273400050330     c* solo se non c'e' stato inivio automatico
273500050330     c                   if        not %error
273600050113     c                   clear                   skiopz(pp)
273700050330     c                   endif
273800050113     c                   ENDSR
273900050121     c*
274000050314     c* Elaboro bolla in arb se non allocata e se trovata---------------*
274100050121     c     ELAARB        BEGSR
274200050121     C* SE IL RECORD E' ALLOCATO --> ERRORE
274300050121    1C     *IN95         IFEQ      *ON
274400050121     C                   SETON                                        18
274500050121     C                   MOVEL     '1'           WERR
274600050121     C* MEMORIZZO IL 1 RECORD IN ERRORE
274700050121     C     ERRREC        IFEQ      0
274800050121     C                   z-add     pp            ERRREC            4 0
274900050121     C                   ENDIF
275000050121     C*
275100050121   X1C                   ELSE
275200050121     C
275300050121    2c                   if        not *in32
275400050314     c* Se nel frattempo la spedizione è stata tassata, non proseguo
275500050314     c   94              exsr      SCARTAPAR
275600050314   2ac                   if        not *in94 and arbaca=' '  or
275700050314     c                             *in94 and wscarta=' '
275800050314     c                   eval      wscelta=vsfsce
275900050121     C                   EXSR      ELAB                                          ELABORO
276000050121     c
276100050121     c* 90 on - Errori in tassazione
276200050121     c*         solo per opzione X (anche se per 1 e 9 non dovrebbe mai
276300050121     c*         accadere che il 90 e il 91 qui siano accesi)
276400050121    3c                   if        (*in90 or *in91)  and  wscelta='X'
276500050310     c  n94              unlock    fnarb01l
276600050310     c   94              unlock    fnblp00f
276700050121     c
276800050121     C   90              MOVE      '2'           WERR
276900050121    4c                   if        *in91
277000050121     c                   move      werr          wuno              1
277100050121    5c                   if        wuno=' '
277200050121     C                   MOVE      '3'           WERR
277300050121    5c                   endif
277400050121    4c                   endif
277500050121     c
277600050121     C                   SETON                                        18
277700050121     C                   SETOff                                       289091
277800050121     C* MEMORIZZO IL 1 RECORD IN ERRORE
277900050121    4C     ERRREC        IFEQ      0
278000050121     C                   z-add     pp            ERRREC            4 0
278100050121    4C                   ENDIF
278200050121    3c                   endif
278300050314   2ac                   else
278400050314     c   94              eval      waggiobol='S'
278500050314     c  n94              unlock    fnarb01l
278600050314     c   94              unlock    fnblp00f
278700050314   2ac                   endif
278800050314   x2c                   else
278900050314     c   94              eval      waggiobol='S'
279000050121    2c                   endif
279100050121     c
279200050121    1c                   endif
279300050121     c
279400050310     c   94              eval      wusaaca='N'
279500050121     c                   exsr      AGGIOSFL
279600050121     c
279700050121     c                   ENDSR
279800050112     c*
279900050112     c* Opzione 9 del sfl: leggo le bolle ed aggiorno -------------------*
280000050112     c     ELABopzione9  BEGSR
280100050121     c                   z-add     1             PP
280200050113     C     '9'           lookup    skiopz(PP)                             34
280300050113    1c                   dow       *in34
280400050311     C                   CLEAR                   WAGGIOBOL
280500050113     c* Leggo bolla per poi aggiornarla
280600050113     c                   exsr      LEGARB
280700050113     C*
280800050113     C* SE IL RECORD E' ALLOCATO --> ERRORE
280900050113    2C     *IN95         IFEQ      *ON
281000050113     C                   SETON                                        18
281100050120     C                   MOVEL     '1'           WERR
281200050113     C* MEMRORIZZO IL 1 RECORD IN ERRORE
281300050113     C     ERRREC        IFEQ      0
281400050114     C                   z-add     pp            ERRREC            4 0
281500050113     C                   ENDIF
281600050113     C*
281700050113   X2C                   ELSE
281800050113     c
281900050314     c* Se nel frattempo le bolle sono state tassate --> scarto
282000050314     c  n32
282100050314     can 94              exsr      SCARTAPAR
282200050311    3c                   if        not *in32  and arbaca=' ' AND not *IN94 or
282300050314     c                             *in94 and not *in32 and wscarta=' '
282400050314     c* codice bolle per impostare in cod.Variazione bolla
282500050120    4c                   if        not *in11
282600050113     C                   MOVEL     '3A'          COD
282700050113     C                   MOVEL(P)  ARBCBO        KEY
282800050113     C     KTAB          CHAIN     TABEL                              32
282900050113     C  N32              MOVEL     TBLUNI        DS3A
283000050113     C   32              CLEAR                   DS3A
283100050113     C                   MOVEL     §3ADES        DESCBO
283200050113     C                   MOVEL     §3ATB1        TIPTB1
283300050113     c*
283400050120    5C     TIPTB1        IFEQ      'A'
283500050317     C                   MOVEL     'TB'          ILRA2CVB
283600050113     C                   ELSE
283700050317     C                   MOVEL     '2B'          ILRA2CVB
283800050120    5C                   END
283900050113     C*
284000050113     C* PRENDO TIPO CODICE VARIAZIONE BOLLA
284100050113     C                   MOVEL     '7L'          COD
284200050113     C                   MOVEL     *BLANKS       KEY
284300050317     C                   MOVEL     ILRA2CVB      KEY
284400050113     C     KTAB          CHAIN     TABEL                              32
284500050113     C  N32              MOVEL     TBLUNI        DS7L
284600050120     C   32              MOVEL     *BLANKS       DS7L
284700050120    4c                   endif
284800050113     C
284900050120     C                   EXSR      PREPA06                                       ELABORO
285000050926     c
285100060117     c* Tasso il  porto se devo considerare le bolle dirottate/cambio p
285200060117     c                   if        vsfdir<>' '
285300050926     c                   movel     'S'           Memsoloporto      1
285400050926     C                   MOVEL     '1'           WBOL              1
285500050926     C                   MOVEL     'N'           WCANCN            1
285600050926     C                   EXSR      TASSAZ
285700050926     c                   movel     ' '           Memsoloporto      1
285800050926     c                   endif
285900050120
286000050310     C  n94              EXSR      AGGIORARB                                     ELABORO
286100050310     C   94              EXSR      AGGIORblp                                     ELABORO
286200050314     c                   else
286300050314     c* Se bolla ora da scartare, nel sfl la proteggo come se fosse tassat
286400050314     c   94              eval      waggiobol='S'
286500050120    3c                   endif
286600050120    2c                   endif
286700050113     c
286800050113     c                   exsr      AGGIOSFL
286900050113     c
287000050113     c                   add       1             PP
287100050113     C     '9'           lookup    skiopz(PP)                             34
287200050113    1c                   enddo
287300050113     c
287400050112     c                   ENDSR
287500050120     c*
287600050120     c* Emissione msg di errore su sfl ------------------------------------*
287700050120     c     SFLERROR      BEGSR
287800050120    1C                   IF        werr<>*blanks
287900050120     c                   select
288000050120    1C     WERR          wheneq    '1 '
288100050120     C                   MOVEL     MSG(39)       VIDMSG
288200050120    1C     WERR          wheneq    ' 2'
288300050120     C                   MOVEL     MSG(66)       VIDMSG
288400050120    1C     WERR          wheneq    ' 3'
288500050120     C                   MOVEL     MSG(22)       VIDMSG
288600050120   x1C                   other
288700050120     C                   MOVEL     MSG(67)       VIDMSG
288800050120    1c                   endsl
288900050120     c
289000050120     C                   Z-ADD     ERRREC        REC
289100050120     C                   SETON                                        2890
289200050120     C                   CLEAR                   WERR
289300050120     C                   CLEAR                   errrec
289400050120    1C                   ENDIF
289500050120     c                   ENDSR
289600050112     c*
289700911028     C*--- ELABORAZIONE BOLLA ----------------------------------------*
289800911028     C     ELAB          BEGSR
289900011015      *
290000011015     C                   CLEAR                   FLGVRN
290100911114     C* CARICO DATI BOLLA
290200911114     C                   EXSR      CARBOL
290300911028     C**
290400941219     C* PULIZIA DEL FORMATO CHE DEVO EMETTERE
290500050317     C                   CLEAR                   LRA2D04
290600010319     C     WIDMSG        IFNE      *BLANKS
290700010319     C                   MOVEL     WIDMSG        VIDMSG
290800010319     C                   SETON                                        28
290900010319     C                   ENDIF
291000941221     C*
291100941221     C     FOR03         TAG
291200050317     c                   setoff                                       1004
291300050113     c
291400050113     c                   if        wscelta='9'
291500050113     c                   seton                                        04
291600050113     c                   endif
291700030507      * Recupero i dati del cliente
291800030507     c                   ExSr      Sr_RecDati
291900941219     C*
292000911127     C                   MOVEL     *BLANKS       DESKSC
292100911127     C                   MOVEL     COMDES        DESKSC
292200050112     c*
292300050112     c* Videate diverse per tassazione singola o tassazione multimpla
292400050112     c                   if        wscelta<>'X'
292500050112     C
292600050112     c                   if        wscelta='1'
292700050317     C                   WRITE     LRA2TS3
292800050317     C                   WRITE     LRA2Z03
292900050317     C                   write     LRA2D03
293000050317     C                   read(E)   fnLRA2d
293100050112     c                   endif
293200050317     C*
293300050112     c                   if        wscelta='9'
293400050317     C                   WRITE     LRA2Tm3
293500050317     C                   WRITE     LRA2Z03
293600050317     C                   EXFMT     LRA2D03
293700050112     c                   endif
293800050112     c
293900050317     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
294000050317     C                   EXSR      INZD03
294100050314     c*
294200050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
294300050314     c                   if        %error
294400050317     c                   eval      *inkl='1'
294500050314     C  n94              UNLOCK    FNARB01L
294600050314     C   94              UNLOCK    FNblp00f
294700050317     c                   goto      endela
294800050112     c                   endif
294900050314     c                   endif
295000941219     C* F 3 - FINE
295100050310     C* F12 - RITORNO
295200050310     c   kl
295300050310     cor kc              if        not *in94
295400050310     C                   UNLOCK    FNARB01L
295500050310     c                   else
295600050310     C                   UNLOCK    FNblp00f
295700050310     c                   endif
295800170512     C** KC              GOTO      FINE
295900170512     C** KL              GOTO      ENDELA
296000170512     c  n11
296100170512     can KC              GOTO      FINE
296200170515     c                   if        *inkl or *inkc
296300170512     c                   eval      olra2err='N'
296400170512     c                   goto      endela
296500170512     c                   endif
296600911114     C*
296700050112     C* F7  - RICERCA CODICE CLIENTE
296800911203     C   KG              DO
296900050926     C* NON AMMESSA SE CODICE DI ALTRA FILIale
297000941216     C                   MOVEL     VIDDSA        PARDUT           30
297100911114     C                   MOVEL     *BLANKS       DESCDS           48
297200911114     C                   MOVEL     DESKSC        DESCDS
297300911114     C* PARSTA=9 ESCLUDO ANNULLATI
297400911114     C                   Z-ADD     9             PARSTA
297500911115     C                   Z-ADD     KCI           CCC               4 0
297600000202     C                   CLEAR                   PARFLR
297700000202     C                   CLEAR                   PARNUM
297800000202     C                   CLEAR                   PARKCM
297900000202     C                   CLEAR                   PARKSM
298000000202     C                   CLEAR                   PARKDM
298100000202     C                   MOVEL     VIDKLP        PARFLR
298200980703     C                   CLEAR                   PARDIT
298300000202     C                   CALL      'XALFA3BR'
298400911114     C                   PARM                    PARDUT
298500911114     C                   PARM                    CODUT
298600911114     C                   PARM                    DESCDS
298700911114     C                   PARM                    CCC
298800911114     C                   PARM                    PARSTA            1 0
298900961016     C                   PARM                    PARFLR           90
299000980703     C                   PARM                    PARDIT            3
299100000202     C                   PARM                    PARNUM            2 0
299200000202     C                   PARM                    PARKCM           80
299300000202     C                   PARM                    PARKSM          140
299400000202     C                   PARM                    PARKDM           60
299500050317     C                   PARM      'S'           PAResci           1
299600050317     C                   PARM      '  '          PARERR            2
299700050317     c*
299800050317     c* Passato tempo max 50 secondi di attesa su ricerca alfabetica:
299900050317     c*                      torno al sfl
300000050317     c                   if        parERR='SP' AND PARSTA=-1
300100050317     c                   eval      *inkl='1'
300200050317     C  n94              UNLOCK    FNARB01L
300300050317     C   94              UNLOCK    FNblp00f
300400170523     c                   if        *in11
300500170523     c                   eval      olra2msg=msg(78)
300600170523     c                   eval      olra2err='1'
300700170523     c                   endif
300800050317     c                   goto      endela
300900050317     c                   endif
301000050317     c* Nessuna selezione:
301100911115     C     PARSTA        IFEQ      -1
301200911128     C*
301300911128     C     TIPTB1        IFEQ      'A'
301400911203     C                   MOVEL     ARBKSC        VIDKLP
301500911203     C                   MOVE      ARBKSC        VIDKSC
301600911128     C                   ELSE
301700960208     C                   MOVEL     ARBLNA        VIDKLP
301800911203     C                   MOVE      9999          VIDKSC
301900911128     C                   END
302000960208     C* SE IL CODICE E' 9999 --> PROPONGO IN KLP QUANTO MEMORIZZATO
302100960208     C*  NELLA ROUTINE CARBOL
302200960208     C     VIDKSC        IFEQ      9999
302300960208     C                   MOVEL     WKLP          VIDKLP
302400960208     C                   ENDIF
302500030507      * salvo il codice cliente impostato
302600030507     c                   Eval      Sav_VidKsc = VidKsc
302700911128     C*
302800911115     C                   ELSE
302900911128     C*
303000000202     C                   MOVEL     PARKSM        W0070             7 0
303100000202     C                   MOVEL     W0070         VIDKLP
303200000202     C                   MOVE      W0070         VIDKSC
303300911115     C                   END
303400911114     C                   GOTO      FOR03
303500911114     C                   END
303600911114     C* CONTROLLI CAMPI
303700911114     C                   EXSR      CONTR3
303800050120     c* Se conferma con X imposto la "E" di errore ed esco dal sfl
303900050120     c                   if        *in90
304000050120     c                   if        wscelta='X'
304100050120     c                   goto      endela
304200050120     c                   else
304300050120     C                   GOTO      FOR03
304400050120     c                   endif
304500050120     c                   endif
304600911114     C***
304700050113     C*  GESTISCO TASSAZIONE di  CLIENTE CHE NON HA TARIFFE
304800941216     C***
304900050202     C                   SETOFF                                       96
305000050112     C     WTKS          ifeq      *BLANKS
305100941216     C                   SETON                                        29
305200050202     C                   SETON                                        29
305300941216     C                   EXSR      GESKCA
305400050317     c* Se errore per supero tempo mak di attesa: torno al sfl
305500050317     c                   if        *inkl and %error
305600050317     c                   goto      endela
305700050317     c                   endif
305800050317     c
305900941216     C   KL              GOTO      FOR03
306000941216     C*
306100941216   X1C                   ELSE
306200941216     C***
306300050114     C* CLIENTE CODIFICATO  CON TARIFFE
306400941216     C***
306500050202     C                   SETOFF                                       2901
306600941216     C*
306700941216    2C     VIDKSC        IFNE      9999
306800941216     C                   MOVEL     ACORAG        DESKSC
306900010327     C     WIDMSG        IFNE      *BLANKS
307000010327     C                   MOVEL     WIDMSG        VIDMSG
307100010327     C                   SETON                                        28
307200010327     C                   ENDIF
307300920319     C*
307400911028     C* EMETTO FORMATO4
307500911028     C     FOR04         TAG
307600050113     C                   SETOFF                                       0126
307700941221     C                   CLEAR                   DESTAS
307800050113     c
307900030507      * Recupero i dati del cliente
308000030507     c                   ExSr      Sr_RecDati
308100911028     C**
308200050112     c                   if        wscelta<>'X'
308300050317     c                   seton                                        10
308400050112     c                   if        wscelta='1'
308500050330     C                   WRITE     LRA2Ts3                                      TESTATA
308600050330     C                   WRITE     LRA2D03                                      COD CLIENTE
308700050330     C                   WRITE     LRA2Z04                                      PIEDI
308800050317     C                   write     LRA2D04                                      COD TARIFFA
308900050317     C                   read(E)   fnLRA2d                                      COD TARIFFA
309000050112     c                   endif
309100050317     C
309200050112     c                   if        wscelta='9'
309300050317     C                   WRITE     LRA2TM3                                      TESTATA
309400050317     C                   WRITE     LRA2D03                                      COD CLIENTE
309500050317     C                   WRITE     LRA2Z04                                      PIEDI
309600050317     C                   EXFMT     LRA2D04                                      COD TARIFFA
309700050112     c                   endif
309800050317     c
309900050317     C* SPENGO INDICATORI DI TIPO RECORD
310000050317     C                   EXSR      INZD04
310100050317     C
310200050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
310300050314     c                   if        %error
310400050314     c                   eval      *inkl='1'
310500050314     C  n94              UNLOCK    FNARB01L
310600050314     C   94              UNLOCK    FNblp00f
310700050314     c                   goto      endela
310800050314     c                   endif
310900050112     c                   endif
311000941219     C**
311100050112     C* F12 - RITORNO
311200941221     C     *INKL         IFEQ      *ON
311300941221     C* PULIZIA DEL FORMATO CHE DEVO EMETTERE
311400050317     C                   CLEAR                   LRA2D04
311500941221     C                   GOTO      FOR03
311600941221     C                   ENDIF
311700050310     c* F15 - forzatura fattura immediata
311800050310     c*       non ammessa per tassazione in partenza
311900050310     c                   if        *in94 and *inkp
312000050310     C                   MOVEL     MSG(76)       VIDMSG
312100050310     C                   SETON                                        289055
312200050310     C                   GOTO      FOR04
312300050310     c                   endif
312400051013     c* F15 - forzatura fattura immediata
312500051013     c*       non ammessa per cliente estero
312600051013     c                   if        *inkp and (clintw='EEX' or clintw='FED'or
312700051013     c                             clintw='DPD')
312800051013     C                   MOVEL     MSG(80)       VIDMSG
312900051013     C                   SETON                                        289055
313000051013     C                   GOTO      FOR04
313100051013     c                   endif
313200911028     C* ENTER - CONTROLLI
313300911028     C                   EXSR      CONTR4
313400050120     c
313500050120     c                   if        *in90
313600050120     c                   if        wscelta='X'
313700050120     c                   goto      endela
313800050120     c                   else
313900050120     C                   GOTO      FOR04
314000050120     c                   endif
314100050120     c                   endif
314200050120     c
314300050112     c* Per tassazione multipla, enter riemetto videata , F1 confermo
314400050112     c                   if        wscelta='9' and not *inka
314500050112     c                   goto      for04
314600050112     c                   endif
314700050112     c* F1 - conferma
314800050112     c                   if        wscelta='9' and *inka
314900050120     c                   seton                                        01
315000050112     c                   exsr      ELABopzione9
315100050120     c                   goto      endela
315200050112     c                   endif
315300050112     c*
315400941217     C* VEDO SE IL CLIENTE ERA S.F.
315500941216    3C     TAMFTS        IFEQ      '1'
315600941217     C                   MOVEL     TAS(1)        DESTAS
315700941217     C                   ELSE
315800941217     C                   MOVEL     TAS(2)        DESTAS
315900941216    3C                   END
316000941216     C*
316100950203     C* F1   = SEGUE FATTURA
316200050223     C* F15  = FATTURA IMMEDIATA
316300941217     C* ENTER=QUELLO CHE E'
316400941217     C   KA              SETON                                        01
316500050223     C   KP              SETOFF                                       01
316600941220     C  NKA
316700050223     CANNKPTAMFTS        IFEQ      '1'
316800941217     C                   SETON                                        01
316900941217     C                   ENDIF
317000941216   X2C                   ELSE
317100911028     C**
317200911028     C* LASCIATO 9999 PROPONGO VIDEATA PER TASSAZIONE
317300911030     C                   MOVEL     *BLANKS       DESKSC
317400050112     C* Tassazione con tariffa di cartello obbligatoria
317500020222      * ricerco tariffa di cartello
317600160226     c                   clear                   trul27ds1
317700160226     c                   Eval      I27Pkg = ArbPkf
317800160226     c                   eval      i27tsp = arbtsp
317900160226     c                   eval      i27lna = arblna
318000160226     c                   eval      i27lnp = VUNICOLNP
318100160226     c
318200160226     c                   movel     vidklp        i27cli
318300160226     c                   move      vidksc        i27cli
318400160226
318500160226     c                   call      'TRUL27R1'
318600160226     c                   parm                    trul27ds1
318700160226     c
318800160226     c                   clear                   trulc7ds
318900160226     c                   eval      ic7tntw=o27ntw
319000160226     c                   call      'TRULC7R'
319100160226     c                   parm                    trulc7ds
319200160226     c
319300160226     c****               eval      tszksc = o27ksc
319400160226     c*****              move      o27ctr        tszctr
319500160226     c
319600160226     c                   eval      tszksc = oc7kscc
319700160226     c                   move      oc7ctrc       tszctr
319800020222
319900050112     C                   SETON                                        96
320000930430     C*
320100050120     C**   FOR05         TAG
320200030507      * Se cliente 9999 pulisco i campi a video
320300030506     c                   If        VidKsc = 9999
320400030506     c                   Clear                   VidVia
320500030506     c                   Clear                   VidLoc
320600050114     c                   Clear                   VidCae
320700030506     c                   Clear                   VidPrv
320800030506     c                   Clear                   VidNota
320900050114     c                   Clear                   V6dVia
321000050114     c                   Clear                   V6dLoc
321100050114     c                   Clear                   V6dCae
321200050114     c                   Clear                   V6dNota
321300030506     c                   Eval      *In46 = *Off
321400030507      * salvo il codice cliente impostato
321500030507     c                   Eval      Sav_VidKsc = VidKsc
321600030605      * salco le spese di incasso
321700030605     c                   eval      Sav_indsin = Indsin
321800030506     c                   EndIf
321900030506
322000050317     C**                 WRITE     LRA2T03                                      TESTATA
322100050317     C**                 WRITE     LRA2T04                                      COD CLIENTE
322200050317     C**                 WRITE     LRA2Z05                                      PIEDI
322300050317     C**N96              EXFMT     LRA2D05                                      CODICE TASSA
322400050317     C** 96              WRITE     LRA2D05                                      CODICE TASSA
322500941219     C*
322600941217     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
322700050113     C**                 SETOFF                                       452890
322800050113     C**                 SETOFF                                       70
322900941221     C* F12 - RITORNO
323000050112     C**   *INKL         IFEQ      *ON
323100941221     C* PULIZIA DEL FORMATO CHE DEVO EMETTERE
323200050317     C**                 CLEAR                   LRA2D04
323300050112     C**                 GOTO      FOR03
323400050112     C**                 ENDIF
323500911028     C*
323600911028     C* ENTER - CONTROLLI
323700050113    4C**   TSZKSC        IFNE      0
323800050113     C**                 EXSR      CONTR5
323900050113     C** 90              GOTO      FOR05
324000050113    4C**                 END
324100050113     c
324200050113    2C                   END
324300911115     C**
324400911115     C* TASSAZIONE SCELTA
324500941217     C     *IN01         IFEQ      *OFF
324600941217     C                   MOVEL     TAS(2)        SCETAS
324700941216     C                   ELSE
324800941217     C                   MOVEL     TAS(4)        SCETAS
324900941216     C                   ENDIF
325000911204     C*
325100050120     C                   EXSR      PREPA06
325200941216     C*
325300950104     C                   CLEAR                   VIDMSG
325400911028     C*
325500911028     C* A S S E G N A T I   N O R M A L I
325600941216    2C  N05              DO
325700941216     C**
325800950104     C     *IN01         IFEQ      *OFF
325900950104     C     VIDKSC        ANDNE     9999
326000950104     C     TSZKSC        ORGT      0
326100950104     C     VIDKSC        ANDEQ     9999
326200970606     C                   MOVEL     'N'           WCANCN            1
326300980512     C                   MOVEL     '1'           WBOL              1
326400011018      * Se è una bolla di reso con varia 'N' 88888888 ed è fattura immediata
326500011126      * tasso la varia 'N'
326600011018     C     FLGVRN        IFEQ      '1'
326700011010     C     *IN01         ANDEQ     *OFF
326800050120      * leggo il sfl delle varie
326900011010     C                   DO        20            NRR
327000050317     C     NRR           CHAIN     LRA2S06                            32
327100011010     C     V6CSV1        IFEQ      'N'
327200011010     C     V6CVA1        ANDEQ     88888888
327300011010     C                   EXSR      TASSAN
327400050317     C  N32              UPDATE    LRA2S06
327500011010     C                   ENDIF
327600011010     C                   ENDDO
327700011010     C                   ENDIF
327800011010      *
327900070924     c                   clear                   PesVolPkcn
328000070924     c                   clear                   PesVolSpc
328100070924     c                   clear                   PesVolImv
328200070924     c                   clear                   Tassatoimv
328300930225     C                   EXSR      TASSAZ
328400070924     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
328500070924     c                   if        vidimv>0
328600070924     c                   eval      PesVolPkcn=taspkcn
328700070924     c                   eval      PesVolSpc=tasspc
328800070924     c                   eval      PesVolIMV=vidimv
328900070924     c                   endif
329000070924     c
329100050926     c                   else
329200060117     c* Propongo il porto se devo considerare le bolle dirottate/cambio p
329300060117     c                   if        vsfdir<>' '
329400050926     c                   movel     'S'           Memsoloporto      1
329500050926     C                   MOVEL     'N'           WCANCN            1
329600050926     C                   MOVEL     '1'           WBOL              1
329700050926     C                   EXSR      TASSAZ
329800050926     c                   movel     ' '           Memsoloporto      1
329900050926     c                   endif
330000941216     C                   ENDIF
330100941216     C*
330200941216     C* VIDEATA DI TASSAZIONE
330300941216     C     FOR06         TAG
330400941221     C* WRITE TESTATA
330500050120     c                   if        wscelta<>'X'
330600050317     C                   WRITE     LRA2T06                                      TESTATA
330700941221     C     VIDKSC        IFEQ      9999
330800050317     C                   WRITE     LRA2D06                                      COD X TASSAZ
330900941221     C                   ELSE
331000050317     C                   WRITE     LRA2D66                                      COD CLIENTE
331100941221     C                   ENDIF
331200050317     C  n03              WRITE     LRA2Z06                                      PIEDI
331300050317     c   03              Write     LRA2z06p                                     Piedi x Picking
331400941223     C*
331500941221     C     VIDMSG        IFNE      *BLANKS
331600941221     C                   SETON                                        28
331700950421     C                   ENDIF
331800941221     C* 1 BOLLA
331900050317     C*                  EXFMT     LRA2C06                                      TASSAZIONE
332000050317     C                   write     LRA2C06                                      TASSAZIONE
332100050317     C                   read(E)   fnlra2d
332200050120     c                   endif
332300941217     C*
332400941217     C                   SETOFF                                       902841
332500941217     C                   SETOFF                                       985253
332600941217     C                   SETOFF                                       554249
332700941217     C                   SETOFF                                       505145
332800011127     C                   SETOFF                                       4847
332900050317     c
333000050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
333100050317     c                   if        %error
333200050317     c                   eval      *inkl='1'
333300050317     C  n94              UNLOCK    FNARB01L
333400050317     C   94              UNLOCK    FNblp00f
333500050317     C                   UNLOCK    FIAR601L
333600050317     c                   goto      endela
333700050317     c                   endif
333800050113     C* F12 - RITORNO
333900990910     C* DISALLOCO FIAR6
334000990910     C   KL              UNLOCK    FIAR601L
334100941217    3C   KLVIDKSC        IFNE      9999
334200941216     C                   GOTO      FOR04
334300941216     C                   ELSE
334400981008     C* SE 96 ON DEVO RITRONARE INDIETRO DI 2 VIDEATE
334500050317     C**   *IN96         IFEQ      *ON
334600050317     C                   CLEAR                   LRA2D04
334700981008     C                   GOTO      FOR03
334800050120     C**                 ELSE
334900050120     C**                 GOTO      FOR05
335000050317    3C**                 ENDIF
335100981008    3C                   ENDIF
335200911114     C*
335300911028     C* ENTER - CONTROLLI
335400911204     C                   EXSR      CONTR6
335500911114     C*
335600911115     C* L'IMPONIBILE E' OBBLIGATORIO PER FATTURAZIONE IMMEDIATA
335700911115     C  N01
335800911115     CANN90VIDIMV        IFEQ      0
335900941221     C                   MOVEL     MSG(32)       VIDMSG
336000950104     C                   SETON                                        4990
336100941217    3C                   END
336200050120     c
336300050120     c                   if        wscelta='X'
336400050120     c   90              goto      endela
336500050120     c                   else
336600050120     c
336700031028     c                   If        Not *In03 and
336800031028     c                             (Not *InKf or *In90)
336900031028     c                   GoTo      For06
337000031028     c                   EndIf
337100031028     c                   If        *In03 and
337200031028     c                             ((Not *InKf and Not *InKh) or *In90)
337300031028     c                   GoTo      For06
337400031028     c                   EndIf
337500050120     c                   EndIf
337600060317     c*
337700060317     c                   if        §7lmtv = 'S'
337800060317     c                   exsr      ges_mtv
337900060317     c                   if        olv85f12 = '1'
338000060317     c                   GoTo      For06
338100060317     c                   endif
338200060317     c                   endif
338300911218     C**
338400911218     C* CONFERMA TASSAZIONE AGGIORNO BOLLE
338500911218     C**
338600050310     C  n94              EXSR      AGGIORARB
338700050310     C   94              EXSR      AGGIORblp
338800050120     c*
338900050120     c                   if        *in91
339000050120     c                   if        wscelta='X'
339100050120     c                   goto      endela
339200050120     c                   else
339300050120     C                   GOTO      FOR06
339400050120     c                   endif
339500050120     c                   endif
339600050120     c
339700941217    2C                   ENDDO
339800911028     C*
339900911028     C* E S T E N S I O N E   B O L L A
340000941217    2C   05              DO
340100911115     C* PER SEGUE FATTURA O FATTURAZIONE E' COMUNQUE OBBLIGATORIO
340200990913     C*  L'IMPORTO DELLA VARIA 'G'
340300980512     C                   MOVEL     '2'           WBOL
340400980512     C                   EXSR      TASSAZ
340500930503     C*
340600911028     C*
340700911028     C     FOR07         TAG
340800941221     C* WRITE TESTATA
340900050317     C                   WRITE     LRA2T06                                      TESTATA
341000941221     C     VIDKSC        IFEQ      9999
341100050317     C                   WRITE     LRA2D06                                      COD X TASSAZ
341200941221     C                   ELSE
341300050317     C                   WRITE     LRA2D66                                      COD CLIENTE
341400941221     C                   ENDIF
341500941221     C* 2 BOLLA
341600991025     C     FOR77         TAG
341700050317     C*                  EXFMT     LRA2D07                                      TASSAZIONE
341800050317     C                   write     LRA2D07                                      TASSAZIONE
341900050317     C                   read(E)   fnlra2d
342000050317     c
342100050317     c* Spengo indicatori di posizionamento cursore
342200941219     C                   SETOFF                                       727475
342300990916     C                   SETOFF                                       902848
342400040119     c                   Setoff                                       717376
342500040119     c                   Setoff                                       777879
342600041129     c                   Setoff                                       808182
342700041129     c                   Setoff                                       83
342800050317     c
342900050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
343000050317     c                   if        %error
343100050317     c                   eval      *inkl='1'
343200050317     C  n94              UNLOCK    FNARB01L
343300050317     C   94              UNLOCK    FNblp00f
343400050317     C                   UNLOCK    FIAR601L
343500050317     c                   goto      endela
343600050317     c                   endif
343700050113     C* F12 - RITORNO
343800990910     C   KL
343900990910     CANN38              UNLOCK    FIAR601L
344000990913     C   KLVIDKSC        IFNE      9999
344100941217     C                   GOTO      FOR04
344200941217     C                   ELSE
344300981008     C     *IN96         IFEQ      *ON
344400050317     C                   CLEAR                   LRA2D04
344500981008     C                   GOTO      FOR03
344600050120     C**                 ELSE
344700050120     C**                 GOTO      FOR05
344800941217     C                   ENDIF
344900981008     C                   ENDIF
345000911114     C*
345100911028     C* ENTER - CONTROLLI
345200990913     C                   EXSR      CONTR7
345300050120     c
345400050120     c                   if        wscelta ='X'
345500050120     c   90              goto      endela
345600050120     c                   else
345700991025     C   28              GOTO      FOR77
345800950121     C  NKF
345900911028     COR 90              GOTO      FOR07
346000050120     c                   endif
346100060317     c*
346200060317     c                   if        §7lmtv = 'S'
346300060317     c                   exsr      ges_mtv
346400060317     c                   if        olv85f12 = '1'
346500060317     c                   GoTo      For07
346600060317     c                   endif
346700060317     c                   endif
346800911218     C**
346900911218     C* CONFERMA TASSAZIONE AGGIORNO BOLLE
347000050310     C  n94              EXSR      AGGIORARB
347100050310     C   94              EXSR      AGGIORblp
347200050120     c
347300050120    3c                   if        *in91
347400050120    4c                   if        wscelta='X'
347500050120     c                   goto      endela
347600050120     c                   else
347700911218     C   91              GOTO      FOR07
347800050120    4C                   ENDif
347900050120    3C                   ENDif
348000050120
348100050120    2C                   END
348200941217    1C                   END
348300911028     C*
348400911028     C     ENDELA        ENDSR
348500050317     C*
348600050317     C*--- SPENGO INDICATORI DI POSIZIONAMENTO CURSORE FORMATO D03----*
348700050317     c     INZD03        begsr
348800050317     c                   setoff                                       289055
348900050317     c                   setoff                                       404142
349000050317     c                   ENDSR
349100050317     C*
349200050317     C*--- SPENGO INDICATORI DI POSIZIONAMENTO CURSORE FORMATO D03----*
349300050317     c     INZD04        begsr
349400050317     C                   SETOFF                                       289055
349500050317     c                   ENDSR
349600911114     C*
349700911114     C*--- CARICO BOLLA ----------------------------------------------*
349800020527     C     CARBOL        BEGSR
349900050715     C                   SETOFF                                       121422
350000941220     C                   SETOFF                                       262901
350100050113     C*
350200941217     C                   CLEAR                   VARVAS
350300941217     C                   CLEAR                   WINTER
350400941217     C                   MOVEL     0             CIN               1 0
350500941217     C                   MOVEL     0             DIE               1 0
350600950104     C                   MOVEL     0             UND               1 0
350700941217     C                   Z-ADD     0             SAVKSC            7 0
350800950104     C                   CLEAR                   VARQFT
350900010327     C                   CLEAR                   WORM
351000050310     C                   CLEAR                   Waggiobol         1
351100010327     C                   CLEAR                   WIDMSG
351200030521     c                   Clear                   sav_VidKsc
351300030605     c                   Clear                   sav_indsin
351400050201     c                   Clear                   Wsfact
351500030709      * pulisco anche savtrc
351600030709     c                   Clear                   savctr
351700030729
351800030729     c                   Clear                   VidBan
351900030729     c                   Clear                   VidCor
352000030729     c                   Eval      *In08 = *Off
352100050111     c                   move      'S'           wstessogas        1
352200070924     c                   Clear                   PesVolPkcn
352300070924     c                   Clear                   PesVolSpc
352400070924     c                   Clear                   PesVolImv
352500070924     c                   Clear                   Tassatoimv
352600941219     C**
352700941219     C* PULISCO I FORMATI
352800050317     C                   CLEAR                   LRA2D03
352900050317     C                   CLEAR                   LRA2d06
353000050317     C                   CLEAR                   LRA2d66
353100050317     C                   CLEAR                   LRA2D07
353200050310     c* Reimposto l'indicatore di tassazione in partenza
353300050310     C                   IF        WTIPOTASSA='P'
353400050310     C                   SETON                                        94
353500050310     C                   ENDIF
353600050715     c**
353700050715     c* Bolla dirottata
353800050715     c**
353900060117     c                   if        Vsfdir<>' '
354000050715     c                   seton                                        22
354100050715     c                   endif
354200050111     C**
354300050111     C* VALUTA DAL DESTINATARIO
354400050111     C**
354500050111     C                   MOVEL     '15'          COD
354600050111     C                   MOVEL(P)  ARBNZD        KEY
354700050111     C                   EXSR      CTRTAB
354800050111     C  N32              MOVEL     TBLUNI        DS15
354900050111     C   32              CLEAR                   DS15
355000050111     C                   MOVEL     §15VLT        DESVLT            3
355100050111     C* BOLLA ITALIA O ESTERA
355200050111    1C     §15ITA        IFEQ      'I'
355300050111     C                   MOVEL     'I'           WDESIE            1
355400050111     C                   ELSE
355500050111     C                   MOVEL     'E'           WDESIE            1
355600050111    1C                   ENDIF
355700050111     C**
355800050111     C* Ragione sociale destinatario
355900050111     C**
356000060628     C                   MOVEL     *blanks       V1cRSD
356100050111     C* vedo se c'e' 2 parte della descrizione
356200050111     C                   MOVEL     'D'           WTRC
356300060217     C     Karb4         CHAIN(N)  Fiar401l                           32
356400050111     C* TROVATA
356500050112    1C     *IN32         IFEQ      *OFF
356600050112     C                   MOVE      ar4NOT        V1cRSD
356700050112    1C                   ENDIF
356800050112     C                   MOVEL     arbRSD        V1cRSD
356900050111     C                   MOVEL     ARBNAS        W005A             5
357000050805     C                   MOVEL     ARBlod        v1dlod
357100050805     C                   MOVEL     ARBlom        v1dlom
357200050118     c
357300050118     c                   if        arbnzd<>*blanks
357400050118     c                   movel     arbnzd        vidprd
357500050118     c                   else
357600050118     c                   movel(p)  arbprd        vidprd
357700050118     c                   endif
357800050715     c                   if        Vuniconzm<>*blanks
357900050715     c                   movel     Vuniconzm     vidprm
358000050118     c                   else
358100050118     c                   movel(p)  arbprm        vidprm
358200050118     c                   endif
358300050111     C**
358400050111     C* CODICE BOLLA
358500050111     C**
358600050111     C                   MOVEL     '3A'          COD
358700050111     C                   MOVEL(P)  ARBCBO        KEY
358800050111     C     KTAB          CHAIN     TABEL                              32
358900050111     C  N32              MOVEL     TBLUNI        DS3A
359000050111     C   32              CLEAR                   DS3A
359100050111     C                   MOVEL     §3ADES        DESCBO
359200050111     c*
359300050111     C                   MOVEL     §3ATB1        TIPTB1
359400050112    1C     TIPTB1        IFEQ      'A'
359500050311     C  n94              MOVEL     ARBKSC        VIDKLP
359600050311     C   94              MOVEL     ARBlna        VIDKLP
359700050111     C                   MOVE      ARBKSC        VIDKSC
359800050111     c*
359900050111     C* SE PGM NON RICHIAMATO VEDO SE PRESENTE CODICE DA O.R.M.
360000050112    2C     *IN11         IFEQ      *OFF
360100050317     C                   MOVEL     'TB'          ILRA2CVB
360200050111     c
360300050113     c* Non vale per tassaz. multipla
360400050113     c     wscelta       ifne      '9'
360500050113    3C     vsfaks        andgt     *ZEROS
360600050113     C                   MOVEL     vsfaks        VIDKLP
360700050113     C                   MOVE      vsfaks        VIDKSC
360800050111     C* Memorizzo che ho proposto da bollettazione
360900050111     C                   MOVEL     'S'           WORM              1
361000050112    3c                   endif
361100050111     c
361200050112    2C                   ENDIF
361300050112   x1C                   ELSE
361400050111     C                   MOVEL     ARBLNA        VIDKLP
361500050111     C                   MOVE      9999          VIDKSC
361600050317     C  N11              MOVEL     '2B'          ILRA2CVB
361700050112    1C                   END
361800050113     c
361900050113     c* Per tassazione multipla, vedo se c'e' codice
362000050125    1c                   if        wscelta='9' and tas9kc(1)<>*blanks
362100050113     c* Cerco il codice prevalente
362200050113     c                   z-add     1             aa
362300050113     c                   z-add     0             wmaggiore
362400050113     c                   clear                   wcodcli
362500050113     c                   clear                   wcodtar
362600050125     c* Propongo il cod cliente più presente, e poi di quello il cod.tar
362700050125     c* più presente se c'e'
362800050125    2c                   dow       tas9nk(aa)>0
362900050125    3c                   if        tas9nk(aa)>wmaggiore
363000050125     c                   movel     tas9k(aa)     wcodcli           7
363100050125     c                   z-add     tas9nk(aa)    wmaggiore         7 0
363200050125    3c                   endif
363300050125     c                   add       1             aa
363400050125    2c                   enddo
363500050125     c* Cerco il cod tariffa
363600050125     c                   z-add     1             aa
363700050125     c                   z-add     0             wmaggiore
363800050125    2c                   dow       tas9n(aa)>0
363900050125     c                   movel     tas9kc(aa)    w007a
364000050125     c                   move      tas9kc(aa)    w003a
364100050125    3c                   if        w007a=wcodcli  and w003a<>*blanks
364200050125    4c                   if        tas9n(aa)>wmaggiore
364300050125     c                   move      tas9kC(aa)    wcodtar
364400050113     c                   z-add     tas9n(aa)     wmaggiore         7 0
364500050125    4c                   endif
364600050125    3c                   endif
364700050113     c                   add       1             aa
364800050125    2c                   enddo
364900050113     c
365000050113     C                   MOVEL     wcodcli       VIDKLP
365100050113     C                   MOVE      wcodcli       VIDKSC
365200050113     c                   movel     'S'           worm
365300050113     c                   endif
365400050111     C*
365500050111     C* PRENDO TIPO CODICE VARIAZIONE BOLLA
365600050111     C                   MOVEL     '7L'          COD
365700050111     C                   MOVEL     *BLANKS       KEY
365800050317     C                   MOVEL     ILRA2CVB      KEY
365900050111     C     KTAB          CHAIN     TABEL                              32
366000050111     C  N32              MOVEL     TBLUNI        DS7L
366100050111     C   32              MOVEL     *BLANKS       DS7L
366200050111     C*
366300050112    1C     §7LRTA        IFEQ      'S'
366400050111     C                   SETON                                        12
366500050112    1C                   END
366600050111     c
366700050111     C                   MOVEL     ARBRSD        COMDES            8
366800050112     C*
366900050112      * Se è una bolla di rientro cerco il codice del mittente della mamma originale
367000050112     c                   Clear                   V1cccm
367100050112If  1c                   If        Arbfbr = 'S'
367200050112     c     Karb2         Chain     Fnlbl01l
367300050112If  2c                   If        %Found(Fnlbl01l)
367400050112     c     Kblp          Chain(N)  Fnblp01l
367500050112If  3c                   If        %found(Fnblp01l)
367600050112     c                   Clear                   Ds3aw
367700050112     c                   Movel     '3A'          Cod
367800050112     c                   Movel(p)  BlpCbo        Key
367900050112     c                   Exsr      CtrTab
368000050112If  4c                   If        Not *In32
368100050112     c                   Movel     TblUni        Ds3aw
368200050112    4c                   EndIf
368300050112     c                   Movel     w_§3atb1      UnoTb1
368400050112If  4c                   If        UnoTb1 = 'F' and BlpKsc <> *Zeros
368500050112     c                   Movel     BlpKsc        V1cccm
368600050112    4c                   EndIf
368700050112If  4c                   If        UnoTb1 = 'A' and BlpCcm <> *Zeros
368800050112     c                   Movel     BlpCcm        V1cccm
368900050112    4c                   EndIf
369000050112    3c                   EndIf
369100050112    2c                   EndIf
369200050112    1c                   EndIf
369300050126     c
369400050126      * ricerco tariffa di cartello + flag italia/estero
369500050126     c                   exsr      tarcar
369600050126     c                   eval      wfie = o27fie
369700050111     c*
369800050111     c* Tutti i campi sottostanti li carico solo per tassazione
369900050111     c*  bolla singola
370000050112    1c                   if        wscelta='9'
370100050112     c                   seton                                        04
370200050126   x1c                   else
370300050112     c                   setoff                                       04
370400941217     C**
370500941217     C* DIVISA MITTENTE
370600941217     C**
370700960528     C                   MOVEL     '15'          COD
370800050714     C**                 MOVEL(P)  ARBNZM        KEY
370900050714     C                   MOVEL(P)  Vuniconzm     KEY
371000941217     C                   EXSR      CTRTAB
371100941217     C  N32              MOVEL     TBLUNI        DS15
371200941217     C   32              CLEAR                   DS15
371300941217     C                   MOVEL     §15VLT        MITVLT            3
371400050112    2C     §15ITA        IFEQ      'I'
371500941217     C                   MOVEL     'I'           WMITIE            1
371600941217     C                   ELSE
371700941217     C                   MOVEL     'E'           WMITIE            1
371800050112    2C                   ENDIF
371900060223     c* Campo di comodo del cod tariffa impostato da fiar4
372000050201     c                   eval      Wsfact=vsfact
372100050111     C**
372200050714     C* LINEA DI partenza  UNICA
372300050111     C**
372400050714     C***  ARBLNP        CHAIN     AZORG01L                           32
372500050714     C     VUNICOLNP     CHAIN     AZORG01L                           32
372600941216     C*
372700020222     c                   eval      og143 = orgde3
372800020222
372900020222     C                   eval      lnpntw = §ogntw
373000980704     C                   MOVEL     ORGDIT        LNPDIT            3
373100941217     C**
373200911218     C* LINEA DI ARRIVO
373300941217     C**
373400911114     C     ARBLNA        CHAIN     AZORG                              32
373500911114     C  N32              MOVEL     ORGDES        DESLNA
373600911114     C   32              MOVEL     *BLANKS       DESLNA
373700070312     C   32              clear                   orgde3
373800070312     C   32              clear                   orgde8
373900020222
374000020222     c                   eval      og143 = orgde3
374100020222
374200020222     c                   eval      lnantw = §ogntw
374300971230     C                   MOVEL     ORGDIT        LNADIT            3
374400980704     C                   MOVEL     ORGDE8        OG148
374500050112    2C     §OGLPR        IFGT      *ZEROS
374600980704     C                   MOVEL     §OGLPR        LNALPR            3 0
374700980704     C                   ELSE
374800980704     C                   Z-ADD     ARBLNA        LNALPR
374900050112    2C                   ENDIF
375000070312     c* Memorizzo se GEO attivata, per scartare la stampa bolla
375100070312     c
375200941217     C**
375300911114     C* DATA SPEDIZIONE
375400941217     C**
375500941216     C                   MOVEL     '3'           G02ERR
375600941216     C                   Z-ADD     ARBDSP        G02INV
375700941216     C                   CALL      'XSRDA8'
375800941216     C                   PARM                    WLBDAT
375900941216     C*
376000941216     C                   Z-ADD     G02DAT        VIDDSP
376100941217     C**
376200941216     C* TIPO SERVIZIO
376300941217     C**
376400090604     c                   clear                   ds5e
376500941216     C                   MOVEL(P)  '5E'          COD
376600941216     C                   MOVEL     ARBTSP        KEY
376700941216     C     KTAB          CHAIN     TABEL                              32
376800090604     C  N32              MOVEL     TBLUNI        ds5e
376900090604     C  N32              MOVEL     §5ed08        V1DTSP
377000941216     C   32              CLEAR                   V1DTSP
377100960302     C* LA NATURA MERCE CHE INZIA CON 'BOLLA XXX COLLI' VIENE
377200960302     C*  EVIDENZIATA
377300960302     C     W005A         IFEQ      'BOLLA'
377400960302     C                   SETON                                        02
377500960302     C                   ELSE
377600960302     C                   SETOFF                                       02
377700960302     C                   ENDIF
377800941217     C**
377900941217     C* CONSEGNE PARTICOLARI
378000941217     C**
378100941219     C                   EXSR      DECCPA
378200050126     c**
378300050126     c* Se Raggruppamento c'e' lo visualizzo
378400050126     c**
378500130221     c                   if        arbxco >*zeros
378600050126     c                   seton                                        14
378700050126     c                   endif
378800050111     c*
378900911211     C*
379000911211     C   12              MOVEL     ARBLNA        VIDKLP
379100911211     C   12              MOVEL     9999          VIDKSC
379200050310     C   12              MOVE      TES(3)        VIDTES
379300050111     c
379400960208     C* SE CODICE GENERICO 99999 E 2 LINEA DISTRIBUZIONE PROPONGO
379500960208     C*  NON LA LINEA DI ARRIVO MA LA LINEA ORIGINALE
379600971230     C*  SOLO SE LNA BARTOLINI
379700050216     c
379800050216     C                   MOVEL     VIDKLP        WKLP
379900050216     C                   CLEAR                   WTISI             1
380000980704     C* 2 LINEA DISTRIBUZIONE
380100050216     C     VIDKSC        IFEQ      9999
380200050216     C     LNADIT        ANDEQ     'BAR'
380300050216     C     LNPDIT        ANDEQ     'BAR'
380400050216     C                   MOVE      ARBGVA        W001A             1
380500050216     C     W001A         IFEQ      'D'
380600050216     C                   MOVEL     'S'           WTISI             1
380700050216     C                   ENDIF
380800050216     C                   ENDIF
380900980704     C**
381000980704     C* SDI PER BAR O VICEVERSA MA NON EXPORT
381100050216     C     LNADIT        IFNE      LNPDIT
381200050216     c     lnantw        andne     'EEX'
381300050216     c     lnantw        andne     'EUP'
381400050216     c     lnantw        andne     'FED'
381500050216     c     lnantw        andne     'DPD'
381600050216     C                   MOVEL     'S'           WTISI
381700050216     C                   ENDIF
381800980704     C**
381900970710     C* CALL AL TISI95R PER VEDERE COSA PRENDERE
382000050112    1C     WTISI         IFEQ      'S'
382100970710     C                   CLEAR                   DSSI95
382200970710     C                   CLEAR                   DSLV13
382300970710     C                   MOVEL     ARBNZD        I95NAR
382400970710     C                   MOVEL     ARBLOD        I95LOC
382500970710     C                   MOVEL     ARBCAD        I95CAP
382600970710     C                   MOVEL     ARBPRD        I95PRV
382700970710     C                   MOVEL     ARBFFD        I95FFD
382800970710     C                   Z-ADD     ARBPKF        I95LKG
382900970710     C                   Z-ADD     ARBVLF        I95LMC
383000050112    2c                   if        lnpntw = 'PPT'
383100050714     C                   Z-ADD     VUNICOLNP     I95TFP
383200000623     C                   ELSE
383300050714     C                   Z-ADD     VUNICOTFP     I95TFP
383400050112    2C                   ENDIF
383500970710     C                   MOVEL     ARBTSP        I95TSP
383600970710     C                   MOVEL     '3'           I95TCN
383700971103     C                   MOVEL     ARBDSP        I95DAT
383800980602     C                   MOVEL     'S'           I95FRE
383900050112    2C     §3AFCA        IFNE      0
384000971201     C                   MOVEL     'S'           I95FCA
384100050112    2C                   ENDIF
384200971201     C                   MOVEL     §3ATB1        I95TPO
384300971201     C**
384400970710     C                   CALL      'FNLV13R'
384500970710     C                   PARM                    KPJBA
384600970710     C                   PARM                    DSLV13
384700970710     C                   PARM                    DSSI95
384800980704     C** NON C'E' ERRORE
384900050112    2c                   if        vidksc=9999
385000050112    3C     O13ERR        IFEQ      ' '
385100980704     C**
385200980704     C* PER 2 LINEA DISTRIBUZIONE
385300050112    4C     LNADIT        IFEQ      'BAR'
385400980704     C     LNPDIT        ANDEQ     'BAR'
385500970710     C                   MOVEL     O95CLA        VIDKLP
385600050112    4C                   ENDIF
385700980704     C* DA SDI PER BARTOLINI
385800050112    4C     LNPDIT        IFEQ      'SDI'
385900980704     C     LNADIT        ANDEQ     'BAR'
386000980704     C* --> UTILIZZO O50CLO SE E' LINEA SDI
386100980704     C     O95CLO        CHAIN     AZORG01L                           32
386200050112    5C     *IN32         IFEQ      *OFF
386300980704     C     ORGDIT        ANDEQ     'SDI'
386400980704     C                   MOVEL     O95CLO        VIDKLP
386500980704     C                   ELSE
386600980704     C                   Z-ADD     LNALPR        VIDKLP
386700050112    5C                   ENDIF
386800050112    4C                   ENDIF
386900980704     C* DA BARTOLINI IN SDI
387000050112    4C     LNPDIT        IFEQ      'BAR'
387100980704     C     LNADIT        ANDEQ     'SDI'
387200980704     C* --> UTILIZZO O50CLO SE E' LINEA SDI
387300980704     C     O95CLA        CHAIN     AZORG01L                           32
387400050112    5C     *IN32         IFEQ      *OFF
387500980704     C     ORGDIT        ANDEQ     'BAR'
387600980704     C                   MOVEL     O95CLA        VIDKLP
387700980704     C                   ELSE
387800980704     C                   Z-ADD     LNALPR        VIDKLP
387900050112    5C                   ENDIF
388000050112    4C                   ENDIF
388100980704     C**
388200050112   x3C                   ELSE
388300970710     C                   MOVEL     ARBLNA        VIDKLP
388400050112    3C                   ENDIF
388500050112    2C                   ENDIF
388600970710     C                   MOVEL     'S'           WCAL13            1
388700960208     C*
388800960208     C                   MOVEL     VIDKLP        WKLP
388900960208     C*
389000050112    1C                   ENDIF
389100911203     C*
389200911114     C                   MOVEL     *BLANKS       DESTAS
389300970409     C* SI NO DDT
389400970409     C                   MOVEL     'A'           WTRC
389500060217     C     KARB4         CHAIN(n)  FiAR4000                           32
389600031028     c                   If        Not *In32
389700031028     c                   Movel     Ar4Not        dsbl4a
389800031028     c                   Move      §B4Abm        wddt              1
389900031028     c                   EndIf
390000970409     C   32              MOVEL     'S'           WDDT
390100970409     C     WDDT          IFEQ      'S'
390200970409     C     WDDT          OREQ      'C'
390300031028     c     wddt          oreq      'P'
390400031028     c     wddt          oreq      'Q'
390500970409     C                   MOVEL     'DDT'         V1CDDT
390600970409     C                   ELSE
390700970409     C                   CLEAR                   V1CDDT
390800970409     C                   ENDIF
390900941216     C*
391000941216     C* PRENDO LE NOTE
391100941216     C                   MOVEL     '8'           WTRC
391200060217     C     KARB4         CHAIN(n)  FiAR4000                           32
391300941220     C  N32              MOVEL     AR4NOT        VIDNT1
391400941220     C   32              CLEAR                   VIDNT1
391500941216     C                   MOVEL     '9'           WTRC
391600060217     C     KARB4         CHAIN(n)  FiAR4000                           32
391700941220     C  N32              MOVEL     AR4NOT        VIDNT2
391800941220     C   32              CLEAR                   VIDNT2
391900941216     C*
392000950121     C                   MOVEL     ARBQFT        WQFT
392100950121     C                   MOVEL     ARBIAS        WIAS
392200950121     C                   MOVEL     ARBVAS        WVAS
392300941216     C                   CLEAR                   VIDTIC
392400941216     C                   CLEAR                   VIDCAS
392500941216     C                   CLEAR                   VIDVCA
392600941216     C* CHAIN SU C/ASSEGNO
392700941216     C     §3AFCA        IFNE      0
392800051115     C     KARB2         CHAIN     FiAR9000                           32
392900941216     C*
393000941220     C     *IN32         IFEQ      *OFF
393100941216     C                   MOVEL     AR9TIC        VIDTIC
393200941216     C                   MOVEL     AR9CAS        VIDCAS
393300941216     C                   MOVEL     AR9VCA        VIDVCA
393400941216     C                   ENDIF
393500941216     C                   ENDIF
393600911114     C*
393700010703     C                   CLEAR                   VIDRTN
393800060529     C**                 MOVE      'F'           WTRC
393900171030     C**                 MOVE      'I'           WTRC
394000171030     C**   KARB4         CHAIN(n)  FiAR401L                           33
394100171030     C**   *IN33         IFEQ      *OFF
394200171030     C***                CLEAR                   DSBL4I
394300171030     C***                MOVEL     AR4NOT        DSBL4I
394400171030     c**   §B4ida        IFNE      *BLANKS
394500171030     C**                 SETON                                        15
394600171030     C**                 MOVEL     §B4ida        VIDRTN
394700171030     C**                 ENDIF
394800171030     C***                ENDIF
394900171030         chain (VsfAAS:VsfLNP:VsfNRS:VsfNSP) fipnd01l;
395000171030         if %found(fipnd01l);
395100171030            select;
395200171030            when pnddpe='DP4';
395300171030                dpnddp4=pndetiu;
395400171030                vidrtn =§b4ida;
395500171030                *in15=*on  ;
395600171030            when pnddpe='DP5';
395700171030                vidrtn=pndida7 ;
395800171030                *in15=*on  ;
395900171030            endsl;
396000171030         endif;
396100030728      * Colli
396200030728     c                   Z-Add     ArbNcl        VidNcl
396300030203
396400030203      * Cerco il record dei bancali x passare il n. dei bancali alla tassazione
396500030708      * Record 'BAN'
396600030708     c                   If        %Subst(ArbGva:1:1) = 'B'
396700030203     c                   Clear                   dAr5Ban
396800030708     c                   Eval      kAr5Trd = 'BAN'
396900071001     c     kFiar5        Chain     Fiar501l
397000030203     c                   If        %Found(Fiar501l)
397100030203     c                   Movel     Ar5Uni        dAr5Ban
397200030728     c     §Ar5Nba       Add       §Ar5Nb2       VidBan
397300030203     c                   EndIf
397400030708     c                   EndIf
397500030729
397600030729      * Cerco il record 'BNB'
397700030708     c                   If        %Subst(ArbGva:1:1) = 'O'
397800030708     c                   Clear                   dAr5Bnb
397900030708     c                   Eval      kAr5Trd = 'BNB'
398000071001     c     kFiar5        Chain     Fiar501l
398100030708     c                   If        %Found(Fiar501l)
398200030708     c                   Movel     Ar5Uni        dAr5Bnb
398300030728     c     §Ar5bNba      Add       §Ar5bNb2      VidBan
398400030729     c                   Z-Add     §Ar5bCor      VidCor
398500030729     c                   Eval      *In08 = *On
398600030708     c                   EndIf
398700030709     c                   EndIf
398800110629      * Memorizzo se presente tipo rek. "EMD" per passarlo alla tassazione
398900130924     c                   Clear                   dAr5emd
399000110629     c                   clear                   wemd
399100110629     c                   Eval      kAr5Trd = 'EMD'
399200130924     c     kFiar5        chain     Fiar501l
399300130924     c                   if        %found(fiar501l)
399400130924     c                   Movel     Ar5Uni        dAr5emd
399500130924     c                   select
399600130924     c                   when      §ar5mmp='S' and §ar5smp=' '
399700110629     c                   eval      wemd='S'
399800130924     c                   when      §ar5mmp='S' and §ar5smp='S'
399900130924     c                   eval      wemd='E'
400000130924     c                   when      §ar5smp='S'
400100130924     c                   eval      wemd='X'
400200130924     c                   endsl
400300110629     c                   endif
400400150107     C* Verifico presenza Pin Code
400500150108     c                   clear                   ds7r
400600150108     c                   clear                   wpincode
400700150108     c                   if        arbgma<>*blanks
400800150108     C                   MOVEL     '7R'          COD
400900150108     C                   MOVEL(p)  ARBgma        KEY
401000150108     C     KTAB          CHAIN     TABEL
401100150108     c                   if        %found(tabel00f)
401200150108     C                   MOVEL     TBLUNI        DS7R
401300150108     c                   endif
401400150108     c                   endif
401500150108     c                   if        §7rpincode='S'
401600150107     c                   eval      wpincode='S'
401700150107     c                   endif
401800010702     C*
401900050112     c                   ENDIF
402000100119     C*
402100911114     C                   ENDSR
402200030507      *---------------------------------------------------------------*
402300030507      * Recupero i dati del cliente                                   *
402400030507      *---------------------------------------------------------------*
402500030507     c     Sr_RecDati    BegSr
402600100120     c                   clear                   wpiva
402700030507
402800030507      * Se non è un cliente 9999 cerco i dati anagrafici e li imposto a video
402900030507If  1c                   If        VidKsc <> 9999
403000030507      * Cliente variato
403100030507If  2c                   If        VidKsc <> Sav_VidKsc
403200030507     c                   Movel     VidKlp        ComKsc
403300030507     c                   Move      VidKsc        ComKsc
403400030507     c                   Exsr      Sr_AcoKsc
403500030507
403600030507     c                   Clear                   VidVia
403700050114     c                   Clear                   V6dVia
403800030507     c                   Clear                   VidLoc
403900050114     c                   Clear                   V6dLoc
404000050114     c                   Clear                   VidCae
404100050114     c                   Clear                   V6dCae
404200030507     c                   Clear                   VidPrv
404300030507     c                   Clear                   VidNota
404400050114     c                   Clear                   V6dNota
404500030507If  3c                   If        O69Err <> '1' and AcoFlg = *Blanks
404600030507     c                   Movel     AcoRag        ComDes
404700030507     c                   Movel     IndVia        VidVia
404800030507     c                   Movel     IndCit        VidLoc
404900050114     c                   movel     IndCae        VidCae
405000050114     c
405100050114     c                   Movel     IndVia        V6dVia
405200050114     c                   Movel     IndCit        V6dLoc
405300050114     c                   movel     IndCae        V6dCae
405400030507     c                   If        IndPrv <> *Blanks
405500030507     c                   Movel     IndPrv        VidPrv
405600030507     c                   Else
405700030507     c                   Movel     IndSta        VidPrv
405800030507     c                   EndIf
405900100120     c* Partita IVA
406000100120     c                   movel     indiva        wpiva
406100030507      * Nota
406200030507     c                   Movel     Kci           kNtcNk1
406300030507     c                   Move      ComKsc        kNtcNk1
406400030507     c     Kntc          Setll     Tfntc01l
406500030507Do  4c                   Do        *Hival
406600030507     c     Kntc          Reade     Tfntc01l
406700030507     c                   If        %Eof(Tfntc01l)
406800030507     c                   Leave
406900030507     c                   EndIf
407000030507     c                   If        NtcFlt <> 'A'
407100030507     c                   Movel     NtcRnt        VidNota
407200050114     c                   Movel     NtcRnt        V6dNota
407300030507     c                   Leave
407400030507     c                   EndIf
407500030507    4c                   EndDo
407600030507
407700030507    3c                   EndIf
407800030507      * salvo il codice cliente impostato
407900030507     c                   Eval      Sav_VidKsc = VidKsc
408000061122      * salvo le spese di incasso solo se codice impostato e senza errore
408100061122     c                   if        comksc >0  and o69err<>'1'
408200030605     c                   Eval      Sav_indsin = indsin
408300030507    2c                   EndIf
408400061122    2c                   EndIf
408500030507      * Se è un cliente 9999 pulisco i campi del video
408600030507   x1c                   Else
408700030507     c                   Clear                   VidVia
408800030507     c                   Clear                   VidLoc
408900050114     c                   Clear                   VidCae
409000030507     c                   Clear                   VidPrv
409100030507     c                   Clear                   VidNota
409200050114     c                   Clear                   V6dVia
409300050114     c                   Clear                   V6dLoc
409400050114     c                   Clear                   V6dCae
409500050114     c                   Clear                   V6dNota
409600100120     c*
409700100120     C* CARICO LA PARTITA IVA: sempre da ARBCPI
409800100120     c                   movel     arbcpi        wpiva
409900030507    1c                   EndIf
410000100120     c
410100100120     C     'PRIVATO'     SCAN      WPIVA         posiz                    30
410200100120     c                   clear                   vidpid
410300100120     c                   select
410400100120     c* solo numeri
410500100120     c* "PRIVATO senza iso
410600100120     c     *in30         wheneq    *off
410700100120     c     wiso          andge     *zeros
410800100120     c     *in30         oreq      *on
410900100120     c     posiz         andlt     3
411000100120     c                   movel     wpiva         vidcpd
411100100120     c* iso + numeri
411200100120     c* iso + "PRIVATO"
411300100120     c     *in30         wheneq    *off
411400100120     c     posiz         orgt      2
411500100120     c                   movel     wpiva         vidpid
411600100120     c                   endsl
411700100120     C*
411800030507
411900030507      * Condiziono l'uscita della costante 'Desc.'
412000050114     c                   If         Vidnota <> *Blanks
412100030507     c                   Eval      *In46 = *On
412200030507     c                   Else
412300030507     c                   Eval      *In46 = *Off
412400030507     c                   EndIf
412500030507
412600030507     c                   EndSr
412700030506      *---------------------------------------------------------------*
412800030506      * Ricerco i dati del cliente fatturazione                       *
412900030506      *---------------------------------------------------------------*
413000030506     c     Sr_AcoKsc     BegSr
413100030506
413200030506     C                   clear                   TIBS69DS
413300030506     C                   z-add     comksc        I69kac
413400030506     C                   z-add     comksc        I69kin
413500030506     C                   call      'TIBS69R'
413600030506     C                   parm                    tibs69DS
413700030506     C                   parm                    ds_cnaco
413800030506     C                   parm                    ds_cnind
413900030506     C                   parm                    ds_cnclp
414000030506     C                   parm                    ds_fncls
414100030506
414200030506     c                   EndSr
414300911114     C*
414400911114     C*--- CONTROLLO CAMPI VARIATI -----------------------------------*
414500911114     C     CONTR3        BEGSR
414600941217     C                   SETOFF                                       90
414700941216     C                   CLEAR                   WTKS
414800010327     C                   CLEAR                   WIDMSG
414900930422     C*
415000930422     C                   MLLZO     1             VIDKSC
415100941216     C*
415200941216     C                   CLEAR                   DSLR66
415300050112     C                   MOVEL     VsfNSP        D66NSP
415400050112     C                   MOVEL     VsfAAS        D66AAS
415500050112     C                   MOVEL     VsfLNP        D66LNP
415600050112     C                   MOVEL     VsfNRS        D66NRS
415700941216     C**
415800941216     C* CAUSALE VARIAZIONE BOLLE
415900941216     C**
416000941219     C                   CLEAR                   V3DCVB
416100941216    1C     VIDCVB        IFNE      *BLANKS
416200941216     C*
416300941217     C     '?'           SCAN      VIDCVB                                 90
416400941216     C* RICERCA
416500941216    2C     *IN90         IFEQ      *ON
416600941216     C                   MOVEL     'V'           D66GES
416700941216     C                   MOVEL     'A'           D66TBO
416800941216     C*
416900941216     C                   MOVEL     DSLR66        KPJBU
417000941216     C                   CALL      'FNLR66R'
417100941216     C                   PARM                    KPJBA
417200941216     C                   MOVEL     KPJBU         DSLR66
417300941216     C                   MOVEL     D66CVB        VIDCVB
417400941216     C                   MOVEL     D66DES        V3DCVB
417500941219     C                   GOTO      ENDCT3
417600941216   X2C                   ELSE
417700941216     C* CONTROLLO
417800941216     C                   CLEAR                   DSLR66
417900941216     C                   MOVEL     'C'           D66GES
418000941216     C                   MOVEL     'A'           D66TBO
418100941216     C                   MOVEL     VIDCVB        D66CVB
418200941216     C*
418300941216     C                   MOVEL     DSLR66        KPJBU
418400941216     C                   CALL      'FNLR66R'
418500941216     C                   PARM                    KPJBA
418600941216     C                   MOVEL     KPJBU         DSLR66
418700941216     C                   MOVEL     D66CVB        VIDCVB
418800941216     C                   MOVEL     D66DES        V3DCVB
418900941216     C*
419000941216     C* ERRORE
419100941216    3C     D66MSG        IFNE      *BLANKS
419200941217     C                   MOVEL     D66MSG        VIDMSG                         CAMPO MESSAGGI
419300941216     C                   SETON                                        284090
419400941216     C                   GOTO      ENDCT3
419500941216    3C                   ENDIF
419600941219    2C                   ENDIF
419700941219     C*
419800941219     C* RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
419900941219     C                   UNLOCK    FNARB01L
420000941219     C*
420100941219     C                   CLEAR                   DSLR48
420200050112     C                   MOVEL     VsfAAS        D48AAS
420300050112     C                   MOVEL     VsfLNP        D48LNP
420400050112     C                   MOVEL     VsfNRS        D48NRS
420500050112     C                   MOVEL     VsfNSP        D48NSP
420600050112     C                   MOVEL     VidFIL        D48FGS
420700941219     C                   MOVEL     ARBCBO        D48CBO
420800071031     C                   MOVEL     vidcvb        D48CVB
420900941222     C                   MOVEL     'A'           D48TBO
421000950619     C                   MOVEL     'S'           D48F12
421100950619     C                   MOVEL     'V'           D48FFR
421200941219     C                   MOVEL     DSLR48        KPJBU
421300941219     C                   CALL      'FNLR48R'
421400941219     C                   PARM                    KPJBA
421500941222     C                   PARM                    DSARBD
421600941222     C                   PARM                    DSARBK
421700990923     C                   PARM                    DARBT
421800941222     C                   PARM                    DSARBG
421900030613     C                   PARM                    TRUL90DS
422000941219     C*
422100941219     C                   MOVEL     KPJBU         DSLR48
422200950123     C*
422300950123     C     KARB2         CHAIN     FNARB000                           32
422400051115     C     KARB2         CHAIN     FiAR9000                           32
422500941219     C* ERRORE
422600941219     C     D48ERR        IFEQ      '1'
422700941219     C                   MOVEL     D48MSG        VIDMSG
422800941219     C                   SETON                                        289040
422900941219     C                   ELSE
423000941219     C                   CLEAR                   VIDCVB
423100941219     C                   CLEAR                   V3DCVB
423200941216    1C                   ENDIF
423300941219     C*
423400950221     C                   SETON                                        9042
423500941219     C                   GOTO      ENDCT3
423600941219    1C                   ENDIF
423700941216     C* CONTROLLO
423800941216    1C     VIDKLP        IFEQ      0
423900941216     C                   MOVEL     MSG(4)        VIDMSG
424000941216     C                   SETON                                        419028
424100941216     C                   GOTO      ENDCT3
424200941216    1C                   END
424300941216     C*
424400941216     C                   MOVEL     VIDKLP        COMKSC
424500941216     C                   MOVE      VIDKSC        COMKSC
424600941216     C*
424700030506     c                   Exsr      Sr_AcoKsc
424800941216     C* ERRORE
424900030506     c     O69Err        Ifeq      '1'
425000030506     c     AcoFlg        Orne      *Blanks
425100941216     C     VIDKSC        OREQ      8888
425200941216     C                   MOVEL     MSG(6)        VIDMSG
425300941216     C                   SETON                                        429028
425400941216     C                   GOTO      ENDCT3
425500941216    2C                   ENDIF
425600941216     C*
425700130320    2C***  ACOABL        IFEQ      '8'
425800130320    2C***  ACOABL        OREQ      '*'
425900130320    2C     ACOABL        ifne      ' '
426000941216     C                   MOVEL     MSG(7)        VIDMSG
426100941216     C                   SETON                                        429028
426200941216     C                   GOTO      ENDCT3
426300941216    2C                   END
426400090428     c
426500090428     c                   movel     c_soc         pjsocieta
426600090428     c                   move      *zeros        pjkcc
426700090428     c                   move      kci           pjkcc
426800090428     c                   if        chkContoIncassiAttribuire(pjSocieta:pjKcc:
426900090428     c                             '0'+%editc(comksc:'X'):pjesito) <> 0
427000090428     C                   MOVEL     MSG(6)        VIDMSG
427100090428     C                   SETON                                        429028
427200090428     C                   GOTO      ENDCT3
427300090428     c                   endif
427400941216     C*
427500941216     C* CARICO LE TARIFFE DEL CLIENTE
427600981008    2C     VIDKSC        IFNE      9999
427700941216     C                   EXSR      CARCTR
427800981008   X2C                   ELSE
427900941216     C* CODICE 9999
428000050120     c* non ammesso per tassazione multipla o tassazione singola con 'X'
428100050120     c                   if        wscelta='9' or wscelta='X'
428200050120     C                   MOVEL     MSG(65)       VIDMSG
428300050112     C                   SETON                                        429028
428400050112     C                   GOTO      ENDCT3
428500050112     c                   endif
428600050310     c* Non ammesso per tassazione in partenza
428700050310     c                   if        *in94
428800050310     C                   MOVEL     MSG(74)       VIDMSG
428900050310     C                   SETON                                        429028
429000050310     C                   GOTO      ENDCT3
429100050310     c                   endif
429200050112     c
429300941216     C                   MOVEL     '9'           WTKS
429400981008     C* SE 9999 LA LNP CODICE DEVE ESSERE = ALLA LNA DELLA BOLLA
429500981008    3C     VIDKLP        IFNE      ARBLNA
429600981008     C                   MOVEL     MSG(44)       VIDMSG
429700981008     C                   SETON                                        419028
429800981008     C                   GOTO      ENDCT3
429900981008    3C                   END
430000981008    2C                   END
430100050120     c* Per tassazione multipla o tassazione singola con 'X'
430200050120     c*                         non accetto cliente senza tariffe
430300050120     c                   if        (wscelta='9' and wtks=' ') or
430400050120     c                             (wscelta='X' and wtks=' ')
430500050120     C                   MOVEL     MSG(50)       VIDMSG
430600050120     C                   SETON                                        429028
430700050120     C                   GOTO      ENDCT3
430800050120     c                   endif
430900941216     C*
431000941216     C** SALVO ULTIMO CODICE IMPOSTATO
431100941216    3C     COMKSC        IFNE      SAVKSC
431200941216     C                   CLEAR                   CIN
431300941216     C                   CLEAR                   DIE
431400941217     C                   MOVEL     COMKSC        SAVKSC            7 0
431500000202     C* VEDO SE CLIENTE DPD
431600000202     C                   MOVEL     COMKSC        W0030             3 0
431700000202     C* CLIENTE DPD
431800000202     C     W0030         CHAIN     AZORG01L                           32
431900020222     c                   eval      og143 = orgde3
432000020222     c                   eval      clintw = §ogntw
432100941216    3C                   END
432200000911     C* SE CIENTE S.F. POSTE, NON ACCETTO IL CODICE
432300020222     c                   if        clintw = 'PPT'
432400000911     C                   MOVEL     MSG(49)       VIDMSG
432500000911     C                   SETON                                        419028
432600000911     C                   GOTO      ENDCT3
432700000911     C                   ENDIF
432800040930     c* Se cliente con ntw "LOG o "XXX" non posso utilizzare
432900040930     c                   if        clintw = 'LOG'  or clintw='XXX'
433000040930     C                   MOVEL     MSG(61)       VIDMSG
433100040930     C                   SETON                                        419028
433200040930     C                   GOTO      ENDCT3
433300040930     C                   ENDIF
433400051013     c                   if        vidksc=9999 and (clintw='FED'or
433500051013     c                             clintw='EEX' or clintw='DPD')
433600051013     C                   MOVEL     MSG(82)       VIDMSG
433700051013     C                   SETON                                        284190
433800051013     C                   GOTO      endct3
433900051013     c                   endif
434000911114     C*
434100911114     C     ENDCT3        ENDSR
434200911028     C*
434300911028     C*--- CARICO CODICI TARIFFA -------------------------------------*
434400911028     C     CARCTR        BEGSR
434500911028     C                   SETOFF                                       01
434600021121
434700021121      * Carico nuovamente la tariffa di cartello + flag italia/estero
434800021121     c                   Exsr      Tarcar
434900021121     c                   eval      wfie = o27fie
435000021121
435100911028     C                   MOVEL     *BLANKS       DE2CTR
435200050128     C                   CLEAR                   fnlv59ds
435300941216     C*
435400050128     C                   MOVEL     'A'           ilv59tbo
435500050128      * Se bolla monovaria carico tutte le tariffe ma propongo la partenza
435600040511     C                   IF        §3ASVA <> *BLANK
435700050128     C                   movel     ' '           ilv59tbo
435800050128     C                   movel     'P'           ilv59prp
435900040511     C                   ENDIF
436000040511      *
436100050128     C                   MOVEL     COMKSC        ilv59KSC
436200050128     C                   MOVEL     KCI           ilv59KCI
436300050128     C                   MOVEL     WFIE          ilv59FIE
436400160401     c                   select
436500160401     c                   when      o27ntw='DPD'
436600160401     C                   MOVEL     'D'           ilv59ta3
436700160401     c                   when      o27ntw='EEX'
436800160401     C                   MOVEL     'E'           ilv59ta3
436900160401     c                   other
437000160401     C                   MOVEL     wfie          ilv59ta3
437100160401     c                   endsl
437200160401     c
437300050128     C                   MOVEL     ARBDSP        ilv59DSP
437400050125     c                   if        wscelta<>'9'
437500050128     C                   MOVEL     ARBTSP        ilv59TSP
437600050125     c                   else
437700050128     c                   movel     tas9t(1)      ilv59tsp
437800050125     c                   endif
437900050128     C                   MOVEL     'S'           ilv59RIC
438000010327     C* SE HO CODICE TARIFFA DA O.R.M. E A VIDEO E' STATO MANTENUTO
438100010327     C* IL CODICE CLIENTE DA ORM LO PASSO AL PGM
438200050113     c                   if        worm='S'
438300050201     c                   if        wscelta<>'9' and Wsfact>=*zeros
438400050113     C                   MOVEL     VIDKLP        W007a             7
438500050113     C                   MOVE      VIDKSC        W007a
438600050113     C     vsfaks        IFEQ      W007a
438700050201     C                   MOVE      Wsfact        ilv59CTR
438800020527     C                   MOVEL     MSG(51)       WIDMSG
438900020527     C                   ENDIF
439000050113     C                   ENDif
439100050113     c*
439200050113     c                   if        wscelta='9'  and wcodtar>=*zeros
439300050113     C                   MOVEL     VIDKLP        W007a             7
439400050113     C                   MOVE      VIDKSC        W007a
439500050113     C     wcodcli       IFEQ      W007a
439600050128     C                   MOVE      wcodtar       ilv59CTR
439700050113     C                   MOVEL     MSG(51)       WIDMSG
439800050113     C                   ENDIF
439900050113     C                   ENDif
440000050113     c*
440100050113     c                   endif
440200010327     C*
440300050128     C                   CALL      'FNLV59R'
440400050128     C                   PARM                    fnlv59ds
440500050124     c
440600050124     c* Se trovato errore ed impostato il cod.tariffa lo ignoro
440700050128     c*  e faccio proporre il cod. tariffa dal pgm fnlv59r
440800050124     c                   if        wscelta='X'  and
440900050128     c                             olv59err<>*blanks  and ilv59ctr>=*zeros
441000050128     c                   clear                   ilv59CTR
441100050201     c                   clear                   Wsfact
441200050124     c                   clear                   widmsg
441300050128     C                   CALL      'FNLV59R'
441400050128     C                   PARM                    FNLV59DS
441500050124     c                   endif
441600050125     c*
441700050128     C                   MOVEL     olv59CTR      VIDCTR
441800050128     C                   MOVEL     olv59DFS      DE2CTR
441900050125     c
442000050125     c* Per tassazione multipla se sono presenti bolle con più tipi servizio
442100050125     c*  e non proposto da bolla il cod tariffa, vedo se il tipo servizio
442200050125     c*  Mi restituisce un codtariffa <> --> se si errore forzabile
442300050125    1c                   if        wscelta='9' and wcodtar=*blanks  and
442400050201     c                             tas9t(2)<>*blanks  and olv59ct3<>*blanks
442500050201     c                   eval      wcodtar1=olv59ctr
442600050201     c                   clear                   ilv59ctr
442700050201     c                   movel     tas9t(2)      ilv59tsp
442800050201     C                   CALL      'FNLV59R'
442900050201     C                   PARM                    fnlv59ds
443000050201    2c                   if        olv59ctr<>wcodtar1
443100050125     C                   MOVEL     MSG(68)       WIDMSG
443200050125   x2c                   else
443300050125    3c                   if        tas9t(3)<>*blanks
443400050201     c                   clear                   ilv59ctr
443500050201     c                   movel     tas9t(3)      ilv59tsp
443600050201     C                   CALL      'FNLV59R'
443700050201     C                   PARM                    fnlv59ds
443800050201    4c                   if        olv59ctr<>wcodtar1
443900050125     C                   MOVEL     MSG(68)       WIDMSG
444000050125    4c                   endif
444100050125    3c                   endif
444200050125    2c                   endif
444300050125    1c                   endif
444400941216     C*
444500050128     C     olv59TKS      IFNE      ' '
444600941219     C                   MOVEL     'S'           WTKS
444700941216     C                   ENDIF
444800941216     C*
444900050128     C                   MOVEL     olv59CT2      VIDCT2
445000050128     C                   MOVEL     olv59CT3      VIDCT3
445100050128     C                   MOVEL     olv59CT4      VIDCT4
445200050128     C                   MOVEL     olv59CT5      VIDCT5
445300050128     C                   MOVEL     olv59ES2      VIDES2
445400050128     C                   MOVEL     olv59ES3      VIDES3
445500050128     C                   MOVEL     olv59ES4      VIDES4
445600050128     C                   MOVEL     olv59ES5      VIDES5
445700911028     C*
445800911028     C                   ENDSR
445900020222      *---------------------------------------------------------------*
446000160226      * Cerco flag caricamento tariffe cliente
446100020222      *---------------------------------------------------------------*
446200020222     c     tarcar        begsr
446300020222
446400160226     c                   clear                   trul27ds1
446500021121     c                   Eval      I27Pkg = ArbPkf
446600020222     c                   eval      i27tsp = arbtsp
446700020222     c                   eval      i27lna = arblna
446800050714     c***                eval      i27lnp = arblnp
446900050714     c                   eval      i27lnp = VUNICOLNP
447000050714     c
447100020222     c                   movel     vidklp        i27cli
447200020222     c                   move      vidksc        i27cli
447300020222
447400160226     c                   call      'TRUL27R1'
447500160226     c                   parm                    trul27ds1
447600020222
447700020222     c                   endsr
447800020222
447900911028     C*
448000991013     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
448100991013     C     CARWAR        BEGSR
448200991013     C                   CLEAR                   WINL
448300991013     C                   CLEAR                   WSV
448400991013     C                   CLEAR                   WVA
448500991013     C                   CLEAR                   C
448600991013    2C                   DO        20            NRR
448700050317     C     NRR           CHAIN     LRA2S06                            32
448800991013     C*
448900991013    3C     *IN32         IFEQ      *OFF
449000991013     C     V6CSV1        ANDNE     *BLANKS
449100991013     C                   ADD       1             C
449200991013     C                   MOVEL     V6CSV1        WSV(C)
449300991013     C                   Z-ADD     V6CVA1        WVA(C)
449400991013    3C                   ENDIF
449500991013    2C                   ENDDO
449600991013     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
449700991013     C     WCKINL        IFEQ      '1'
449800991013     C                   Z-ADD     1             II
449900991013     C     §$2FIN        LOOKUP    WSV(II)                                32
450000991013     C   32WVA(II)       IFGT      *ZEROS
450100991013     C                   MOVE      '1'           WINL              1
450200991013     C                   ENDIF
450300991013     C                   ENDIF
450400991013     C                   ENDSR
450500040121      *---------------------------------------------------------------*
450600040121      * Carico le varie della seconda bolla in una sk di comodo
450700040121      *---------------------------------------------------------------*
450800040121     c     Carwar1       Begsr
450900040121
451000040121     c                   Clear                   wsv
451100040121     c                   Clear                   wva
451200040121     c                   Clear                   c
451300040121
451400040121     c                   If        vi2sv1 <> *Blanks
451500050401     c                   add       1             c
451600050401     c                   Movel     vi2Sv1        wsv(c)
451700050401     c                   Z-add     vi2va1        wva(c)
451800040121     c                   EndIf
451900040121     c                   If        vi2sv2 <> *Blanks
452000050401     c                   add       1             c
452100050401     c                   Movel     vi2Sv2        wsv(c)
452200050401     c                   Z-add     vi2va2        wva(c)
452300040121     c                   EndIf
452400040121     c                   If        vi2sv3 <> *Blanks
452500050401     c                   add       1             c
452600050401     c                   Movel     vi2Sv3        wsv(c)
452700050401     c                   Z-add     vi2va3        wva(c)
452800040121     c                   EndIf
452900040121     c                   If        vi2sv4 <> *Blanks
453000050401     c                   add       1             c
453100050401     c                   Movel     vi2Sv4        wsv(c)
453200050401     c                   Z-add     vi2va4        wva(c)
453300040121     c                   EndIf
453400041129     c                   If        vi2sv5 <> *Blanks
453500050401     c                   add       1             c
453600050401     c                   Movel     vi2Sv5        wsv(c)
453700050401     c                   Z-add     vi2va5        wva(c)
453800041129     c                   EndIf
453900041129     c                   If        vi2sv6 <> *Blanks
454000050401     c                   add       1             c
454100050401     c                   Movel     vi2Sv6        wsv(c)
454200050401     c                   Z-add     vi2va6        wva(c)
454300041129     c                   EndIf
454400040121
454500040121     c                   EndSr
454600050112     c
454700911028     C*--- CONTROLLO CODICE TARIFFA ----------------------------------*
454800911028     C     CONTR4        BEGSR
454900911028     C                   SETOFF                                       90
455000941216     C*
455100941216     C* CONTROLLO CODICE TARIFFA
455200050128     C                   CLEAR                   fnlv59ds
455300050128     C                   MOVEL     'A'           ilv59TBO
455400050128      * se bolla monovaria carico tutte le tariffe ma propongo
455500040511     C                   IF        §3ASVA <> *BLANK
455600050128     C                   movel     ' '           ilv59tbO
455700040511     C                   ENDIF
455800040511      *
455900050128     C                   MOVEL     ARBDSP        ilv59DSP
456000050128     c                   if        wscelta<>'9'
456100050128     C                   MOVEL     ARBTSP        ilv59TSP
456200050128     c                   else
456300050128     c                   movel     tas9t(1)      ilv59tsp
456400050128     c                   endif
456500050128     C                   MOVEL     KCI           ilv59KCI
456600050128     C                   MOVEL     VIDCTR        ilv59CTR
456700050128     C                   MOVEL     COMKSC        ilv59KSC
456800050128     C                   MOVEL     WFIE          ilv59FIE
456900050128     C                   MOVEL     'S'           ilv59CON
457000050201     C                   CALL      'FNLV59R'
457100050201     C                   PARM                    fnlv59ds
457200950121     C* VEDO SE C'E' STATA LA RICERCA
457300050128     C     olv59ERR      IFEQ      'R'
457400050128     C                   MOVEL     olv59CTR      VIDCTR
457500950121     C                   SETON                                        5590
457600950121     C                   GOTO      ENDCT4
457700950121     C                   ENDIF
457800941216     C*
457900050128     C     olv59ERR      IFNE      *BLANKS
458000050128     C                   MOVEL     olv59MSG      VIDMSG
458100941216     C                   SETON                                        289055
458200941216     C                   GOTO      ENDCT4
458300941216     C                   ENDIF
458400941216     C*
458500941216     C* DESCRIZIONE CODICE TARIFFA
458600050128     C                   MOVEL     olv59DFS      DE2CTR
458700050128     c* cod.tariffa cliente + attributi
458800050128     c                   movea     olv59ctt      ctrw
458900050128     c                   movea     olv59at1      fiew
459000050128     c                   movea     olv59at2      tspw
459100050128     c                   movea     olv59at3      bapw
459200050128     c                   movea     olv59at4      tfpw
459300050128     c                   movea     olv59at5      valw
459400941216     C*
459500941216     C* SE TARIFFA TROVATA -- CHAIN SU TNTAM E PRENDO DESCRIZIONE VAR
459600050128     C                   MOVEL     olv59CTR      COMCTR
459700050128     C                   MOVEL     olv59PRG      COMPRG
459800941216     C*
459900941220     C     KTAM3         CHAIN     TNTAM000                           32
460000941220     C     *IN32         IFEQ      *ON
460100941216     C                   MOVEL     MSG(10)       VIDMSG
460200941216     C                   SETON                                        289055
460300941216     C                   GOTO      ENDCT4
460400941216     C                   ENDIF
460500941216     C*
460600950421     C     TAMDCV        IFNE      *BLANKS
460700941216     C                   MOVEL     TAMDCV        DE2CTR
460800950421     C                   END
460900991028     C                   MOVEL     TAMFLO        DSTA01
461000050112     c
461100050114     C* Se tassazione multimpla impossibile scegliere codice con fattura
461200050112     c*  immediata impostato in tariffa
461300050124     c                   if        (tamfts='2' and wscelta='9') or
461400050124     c                             (tamfts='2' and wscelta='X')
461500050112     C                   MOVEL     MSG(64)       VIDMSG
461600050112     C                   SETON                                        289055
461700050112     C                   GOTO      endct4
461800050112     c                   endif
461900050310     C* Se tassazione in partenza impossibile scegliere codice con fattura
462000050310     c*  immediata impostato in tariffa
462100050310     c                   if        tamfts='2' and *in94
462200050310     C                   MOVEL     MSG(75)       VIDMSG
462300050310     C                   SETON                                        289055
462400050310     C                   GOTO      endct4
462500050310     c                   endif
462600051013     C* Se tassazione codice estero impossibile scegliere codice con fattura
462700051013     c*  immediata impostato in tariffa
462800051013     c                   if        tamfts='2' and (clintw='FED' or
462900051013     c                             clintw='EEX' or clintw='DPD')
463000051013     C                   MOVEL     MSG(81)       VIDMSG
463100051013     C                   SETON                                        289055
463200051013     C                   GOTO      endct4
463300051013     c                   endif
463400050128     c*
463500050128     c*
463600050128    1c                   if        wscelta='X'
463700050201     c                   if        worm='S' and Wsfact>=*zeros
463800050124     C                   MOVEL     VIDKLP        W007a             7
463900050124     C                   MOVE      VIDKSC        W007a
464000050124     c                   endif
464100050124     c
464200050128    2c                   select
464300050128     c* Se cod tariffa preimpostato --> ok
464400050201     c                   when      worm='S'   and  Wsfact>=*zeros and
464500050124     c                             vsfaks=w007a
464600050128     c* Se presente una sola tariffa --> ok
464700050128     c                   when      ctrw(2)=*blanks
464800050128   x2c                   other
464900050128     c* Per monovaria ok anche se c'e' solo una tariffa Partenza
465000050128     c                   clear                   wconta            3 0
465100050128    3c                   if        §3asva<>' '
465200050128     c                   z-add     1             aa
465300050128    4c                   dow       ctrw(aa)<>*blanks
465400050128     c                   if        bapw(aa)<>'A'
465500050128     c                   add       1             wconta
465600050128     c                   endif
465700050128     c                   add       1             aa
465800050128    4c                   enddo
465900050128    3c                   endif
466000050128     c
466100050128     c* WCONTA=0 bolla non monovaria  o
466200050128     c*          bolla     monovaria con nessuna    tariffa partenza
466300050128     c* WCONTA=1 bolla     monovaria con una sola   tariffa Partenza --> ok
466400050128     c* WCONTA>1 bolla     monovaria con più di una tariffa Partenza --> ok
466500050128    3C                   IF        wconta>1 or wconta=0
466600050128     c* Per essere ok devo avere una sola tariffa dello stesso tipo
466700050128     c*  servizio della bolla da tassare
466800050128    4C                   IF        ARBTSP=TAMTSP
466900050128     C                   Z-ADD     1             AA
467000050128     c     ARBTSP        LOOKUP    TSPW(AA)                               32
467100050128     c
467200050128    5C                   dow       *IN32 and not *in90
467300050128    6C                   if        CTrW(AA)<>VIDCTR
467400050128     c
467500050128     c* Per monovaria con più tariffe Part errore solo se tariffa "P"
467600050128    7c                   if        (wconta>1 and bapw(aa)<>'A') or
467700050128     c                              wconta=0
467800050128     C                   SETON                                        2890
467900050128    7c                   endif
468000050128    6c                   endif
468100050128     c
468200050128     c                   add       1             aa
468300050128     c
468400050128     c     ARBTSP        LOOKUP    TSPW(AA)                               32
468500050128    5c                   enddo
468600050128     C*
468700050128   x4c                   else
468800050124     C                   SETON                                        2890
468900050128    4c                   endif
469000050128    3c                   endif
469100050128    2c                   endsl
469200050128    1c                   endif
469300050128     c
469400050128     C   90              GOTO      endct4
469500051024     C*
469600051024     c* Per tassazione multipla verifico se c'e' obbligo di inserimento o
469700051024     c*  non immissione importo da assicurare: se c'e' controllo presenza
469800051024     c*  IAS sulle bolle
469900051024     c                   if        wscelta='9'
470000051024     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
470100051024     C     KTPT          CHAIN     TITPT000                           32
470200051024     C*
470300051024    3C     *IN32         IFEQ      *ON
470400051024     C     TPTATB        ORNE      ' '
470500051024     C     TPTAPL        ORNE      'P'
470600051024     c                   if        tamfis='3'  and tas9noias='S'
470700051024     C                   MOVEL     MSG(83)       VIDMSG
470800051024     C                   SETON                                        289055
470900051024     C                   GOTO      endct4
471000051024     c                   endif
471100051024     c                   if        tamfis='2'  and tas9siias='S'
471200051024     C                   MOVEL     MSG(84)       VIDMSG
471300051024     C                   SETON                                        289055
471400051024     C                   GOTO      endct4
471500051024     c                   endif
471600051024     c                   endif
471700051024     c                   endif
471800911028     C*
471900911028     C     ENDCT4        ENDSR
472000911028     C*
472100050120     C*--- Prepara CAMPI TASSAZIONE ----------------------------------*
472200050120     C     PREPA06       BEGSR
472300050120     C                   SETOFF                                       052506
472400050120     C                   SETOFF                                       07
472500050120     C                   MOVEL     *BLANKS       DESCEI
472600050120     C                   CLEAR                   WAR6
472700050120     C                   CLEAR                   WAR7
472800050120     c                   Clear                   war61
472900050120     c                   Clear                   war71
473000050120     c                   Clear                   wesvar1
473100050120     c                   Clear                   ContaSv
473200050120     c                   Clear                   Totimv
473300050926     c                   Clear                   NOeliminaporto    1
473400070924     c                   Clear                   Savimv
473500050120     C* IMPOSTO I CAMPI DELL'IMPORTO DA ASSIC E Q.TA FATTURARE
473600050120     c                   if        wscelta<>'9'
473700050120     C                   MOVEL     WQFT          VIDQFT
473800050120     C                   MOVEL     WIAS          VIDIAS
473900050120     C                   MOVEL     WIAS          VARIAS
474000050120     C                   MOVEL     WVAS          VIDVAS
474100050120     C                   MOVEL     WVAS          VARVAS
474200050120     c                   else
474300050120     C                   MOVEL     arbqft        VIDQFT
474400050120     C                   MOVEL     arbias        VIDIAS
474500050120     C                   MOVEL     arbias        VARIAS
474600050120     C                   MOVEL     arbvas        VIDVAS
474700050120     C                   MOVEL     arbvas        VARVAS
474800050120     c                   endif
474900050120     C*
475000050120      * Se l'importo da assicurare è a 0 e non è stata impostata la divisa
475100050120    1C     VIDIAS        IFEQ      *ZEROS
475200050120     C     VIDVAS        ANDEQ     *BLANKS
475300050120      * prima provo ad impostare la divisa della nazione del mittente
475400050120      * se è estero
475500050120     C     MITVLT        IFNE      *BLANKS
475600050714     C     WMITIE        ANDEQ     'E'
475700050120     C                   MOVEL     MITVLT        VIDVAS
475800050120     C                   MOVEL     MITVLT        VARVAS
475900050120    1C                   ELSE
476000050120      * altrimenti imposto la §GEDCN
476100050120     C                   MOVEL     §GEDCN        VIDVAS
476200050120     C                   MOVEL     §GEDCN        VARVAS
476300050120     C                   ENDIF
476400050120     C                   ENDIF
476500050120     c                   eval      sv1_vidias = vidias
476600050120     c                   eval      sv1_vidvas = vidvas
476700050120     c                   eval      sv1_vidqft = vidqft
476800050120      *
476900050120     C* SE FATTURA IMMEDIATA PROTEGGO LA DIVISA
477000050120     C     *IN01         IFEQ      *OFF
477100050120     C     VIDKSC        ANDNE     9999
477200050120     C     TSZKSC        ORGT      0
477300050120     C     VIDKSC        ANDEQ     9999
477400050120     C                   SETON                                        07
477500050120     C                   ENDIF
477600050120     C* SE OBBLIGO TASSAZIONE CON TARIFFA DI CARTELLO, PROTEGGO I CAMPI
477700050120     C   96              SETON                                        25
477800050120     C* PROTEGGO QUANTITA' DA FATTURARE SE FLAG Q.TA' DA FATTURARE = 3
477900050120     C* E SE CLIENTE ESISTENTE IN TARIFFA
478000050120     C     *IN29         IFEQ      *OFF
478100050120     C                   MOVEL     VIDCTR        W001A
478200050120     C     W001A         IFEQ      '4'
478300050120     C     TAMFVD        ANDEQ     '3'
478400050120     C                   SETON                                        06
478500050120     C                   END
478600050120     C                   END
478700050120     C*
478800050120     C                   CLEAR                   FIAR6000
478900050120     C                   CLEAR                   FIAR7000
479000050120     C*
479100050120     C* VEDO SE ESITONO I FILE DELLA TASSAZIONE
479200050120     C                   MOVEL     '1'           WTRC
479300050120     C     KARB4         CHAIN     FIAR6000                           32
479400050120     C  N32              MOVEL     'E'           WAR6              1
479500050120     C     KARB4         CHAIN(N)  FIAR7000                           32
479600050120     C  N32              MOVEL     'E'           WAR7              1
479700050120     C* PULIZIA SFL
479800050120     C                   SETON                                        27
479900050317     C                   WRITE     LRA2C06
480000050120     C                   SETOFF                                       27
480100050120     C*
480200050120     C                   CLEAR                   V6CSV1
480300050120     C                   CLEAR                   V6CEV1
480400050120     C                   CLEAR                   V6CVA1
480500050120     C                   CLEAR                   VARVA1
480600050120     C                   DO        20            NRR
480700050317     C                   WRITE     LRA2S06
480800050120     C                   ENDDO
480900050120     C*
481000050120     C                   MOVEL     §3ATB1        TIPTB1            1
481100050120     C* 1  B O L L A   A S S E G N A T O
481200050120    1C     TIPTB1        IFEQ      'A'
481300050120     C*
481400050120     C*  PRENDO I CAMPI DALLA BOLLA DELLA TASSAZIONE SE:
481500050120     C*
481600050120     C* 1) E' 1 TASSAZIONE
481700050120     C* 2) E' PASSAGGIO DA SF A FATT IMMEDIATA
481800050120     C* 3) SE PASSAGGIO DA FATT IMMEDIATA A SF SE CONVENUTI STABILITI
481900050120     C*    DALLA PARTENZA
482000050120     C*
482100050120    2C     *IN12         IFEQ      *OFF
482200050120     C*
482300050120    2C     *IN12         OREQ      *ON
482400050120     C     AR6DFT        ANDEQ     0
482500050120     C*
482600050120    2C     *IN12         OREQ      *ON
482700050120     C     AR6DFT        ANDGT     0
482800050120     C     AR6FIM        ANDEQ     'P'
482900050120     C                   MOVEL     AR6DIV        VIDDIV
483000050120     C                   MOVEL     AR6DIV        WDIV
483100050120     C                   Z-ADD     AR6POR        VIDPOR
483200050120     C                   Z-ADD     AR6POR        VARPOR
483300050120     C                   Z-ADD     AR6IMV        VIDIMV
483400050120     C                   MOVEL     AR6CEI        VIDCEI
483500070924     c                   eval      savimv=ar6imv
483600050120     C* SFL DELLE VARIE
483700050120     C                   Z-ADD     0             NRR
483800050120     C                   MOVEL     AR6SV1        WSV1
483900050120     C                   Z-ADD     AR6VA1        WVA1
484000050120     C                   EXSR      CARVAR
484100050120     C                   MOVEL     AR6SV2        WSV1
484200050120     C                   Z-ADD     AR6VA2        WVA1
484300050120     C                   EXSR      CARVAR
484400050120    4C     AR6SV3        IFNE      ' '
484500050120     C                   MOVEL     AR6SV3        WSV1
484600050120     C                   Z-ADD     AR6VA3        WVA1
484700050120     C                   EXSR      CARVAR
484800050120     C*
484900050120     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
485000050120     C     KARB4         SETLL     FIAR7000
485100050120     C     KARB4         READE(N)  FIAR7000                               32
485200050120    5C     *IN32         DOWEQ     *OFF
485300050120     C                   MOVEL     AR7SVN        WSV1
485400050120     C                   MOVEL     AR7VAN        WVA1
485500050120     C                   EXSR      CARVAR
485600050120     C     KARB4         READE(N)  FIAR7000                               32
485700050120    5C                   ENDDO
485800050120    4C                   ENDIF
485900050120     C*
486000050120     C* DECODIFICA ESENZIONE
486100050120    3C     VIDCEI        IFNE      ' '
486200050120     C                   MOVEL     'EI'          COD
486300050120     C                   MOVEL     *BLANKS       KEY
486400050120     C                   MOVEL     VIDCEI        KEY
486500050120     C     KTAB          CHAIN     TABEL                              32
486600050120     C  N32              MOVEL     TBLUNI        DESCEI
486700050120    3C                   END
486800050120     C*
486900050120     C* VEDO SE PROTEGGERE I CONVENUTI
487000050120    3C     *IN12         IFEQ      *OFF
487100050120    4C     AR6IMV        IFGT      0
487200050120     C                   SETON                                        25
487300050120    4C                   ENDIF
487400050120     C*
487500050120   X3C                   ELSE
487600050120     C*
487700050120     C     AR6FIM        IFEQ      'P'
487800050120     C                   SETON                                        25
487900050120    4C                   ENDIF
488000050120    3C                   ENDIF
488100050120     C*
488200050120   X2C                   ELSE
488300050120     C*
488400050120     C                   MOVE      *BLANKS       VIDDIV
488500050120     C                   MOVE      *BLANKS       WDIV
488600050120     C                   Z-ADD     0             VIDPOR
488700050120     C                   Z-ADD     0             VARPOR
488800050120     C                   Z-ADD     0             VIDIMV
488900050120     C                   MOVEL     ' '           VIDCEI
489000050120     C* PULISCO ANCHE IL SFL
489100050120     C                   DO        20            NRR
489200050317     C     NRR           CHAIN     LRA2S06                            32
489300050120     C                   CLEAR                   V6CSV1
489400050120     C                   CLEAR                   V6CVA1
489500050120     C                   CLEAR                   VARVA1
489600050120     C                   CLEAR                   V6CEV1
489700050317     C  N32              UPDATE    LRA2S06
489800050120     C                   ENDDO
489900050120    2C                   END
490000050120     C*
490100050120     C     *IN25         IFEQ      *ON
490200050120     C                   DO        20            NRR
490300050317     C     NRR           CHAIN     LRA2S06                            32
490400050317     C  N32              UPDATE    LRA2S06
490500050120     C                   ENDDO
490600050120     C                   ENDIF
490700050120     C*
490800050120   X1C                   ELSE
490900050120     C*
491000050120     C                   MOVEL     §3ATB2        TIPTB2            1
491100050120     C* 2  B O L L A   A S S E G N A T O
491200050120    2C     TIPTB2        IFEQ      'A'
491300050120     C                   SETON                                        05
491400050120
491500050120     c                   Clear                   DE2CEI
491600050120     c                   Clear                   wdiv
491700050120     c                   Clear                   vi2va1
491800050120     c                   Clear                   vi2va2
491900050120     c                   Clear                   vi2va3
492000050120     c                   Clear                   vi2va4
492100050120     c                   Clear                   vi2va5
492200050120     c                   Clear                   vi2va6
492300050120     c                   Clear                   vi2sv1
492400050120     c                   Clear                   vi2sv2
492500050120     c                   Clear                   vi2sv3
492600050120     c                   Clear                   vi2sv4
492700050120     c                   Clear                   vi2sv5
492800050120     c                   Clear                   vi2sv6
492900050120     c                   Clear                   vi2es1
493000050120     c                   Clear                   vi2es2
493100050120     c                   Clear                   vi2es3
493200050120     c                   Clear                   vi2es4
493300050120     c                   Clear                   vi2es5
493400050120     c                   Clear                   vi2es6
493500050120     C*
493600071029     C*
493700071029     C                   CLEAR                   FIAR6000
493800071029     C                   CLEAR                   FIAR7000
493900050120     C                   MOVEL     '2'           WTRC
494000050120     C     KARB4         CHAIN     FIAR6000                           38
494100050120     c  n38              Eval      war61 = 'E'
494200050120      * Verifico se ci sono altre varie
494300050120     c     Karb4         Setll     Fiar701l
494400050120     c                   If        %Equal(Fiar701l)
494500050120     c                   Eval      war71 = 'E'
494600050120     c                   EndIf
494700050120     C*
494800050120     C**!!!              MOVEL     'G'           VI2SV1
494900050120     C**!!!              MOVEL     *BLANKS       VI2CEI
495000050120     C**!!!              MOVEL     *BLANKS       DE2CEI
495100050120     C**!!!              MOVE      *BLANKS       VI2DIV
495200050120     C**!!!              MOVE      *BLANKS       WDIV
495300050120     C**!!!              Z-ADD     0             VI2VA1
495400050120     C**!!!              Z-ADD     0             VA2VA1
495500050120     C*
495600050120    3C**!!!*IN38         IFEQ      *OFF
495700050120     C*
495800050120     C* RIEMPO I CAMPI DELLA TASSAZIONE SE:
495900050120     C* 1) E' 1 TASSAZIONE
496000050120     C* 2) E' RITASSAZIONE CON CONVENUTI DALLA PARTENZA
496100050120    4C**!!!*IN12         IFEQ      *OFF
496200050120     C**!!!AR6VA1        ANDGT     0
496300050120     C*
496400050120     C**!!!*IN12         OREQ      *ON
496500050120     C**!!!AR6FIM        ANDEQ     'P'
496600050120     C**!!!AR6VA1        ANDGT     0
496700050120    3c                   If        War61 = 'E' and (Not *In12 or
496800050120     c                             (*In12 and Ar6Fim = 'P'))
496900050120
497000050120     C                   MOVEL     AR6DIV        VI2DIV
497100050120     C                   MOVEL     AR6DIV        WDIV
497200050120     C**!!!              MOVEL     AR6SV1        VI2SV1
497300050120     C**!!!              Z-ADD     AR6VA1        VI2VA1
497400050120     C**!!!              Z-ADD     AR6VA1        VA2VA1
497500050120     c                   Movel     Ar6Sv1        wsv1
497600050120     c                   Z-add     Ar6Va1        wva1
497700050120     c                   ExSr      CarVar1
497800050120     c                   Movel     Ar6Sv2        wsv1
497900050120     c                   Z-add     Ar6Va2        wva1
498000050120     c                   ExSr      CarVar1
498100050120     c                   Movel     Ar6Sv3        wsv1
498200050120     c                   Z-add     Ar6Va3        wva1
498300050120     c                   ExSr      CarVar1
498400050120      * Verifico se ci sono altre varie
498500050120     c     Karb4         Setll     Fiar701l
498600050120Do  4c                   Do        *Hival
498700050120     c     Karb4         Reade(n)  Fiar701l
498800050120     c                   If        %Eof(Fiar701l)
498900050120     c                   Leave
499000050120     c                   EndIf
499100050120     c                   Movel     Ar7Svn        wsv1
499200050120     c                   Z-add     Ar7Van        wva1
499300050120     c                   ExSr      CarVar1
499400050120    4c                   EndDo
499500050120
499600050120     C**!!!              SETON                                        25
499700050120    4C**!!!              END
499800050120     C*
499900050120    4C     AR6CEI        IFNE      ' '
500000050120    5C**!!!*IN12         IFEQ      *OFF
500100050120     C*
500200050120     C**!!!*IN12         OREQ      *ON
500300050120     C**!!!AR6FIM        ANDEQ     'P'
500400050120     C                   MOVEL     AR6CEI        VI2CEI
500500050120     C                   MOVEL     'EI'          COD
500600050120     C                   MOVEL     *BLANKS       KEY
500700050120     C                   MOVEL     VI2CEI        KEY
500800050120     C     KTAB          CHAIN     TABEL                              32
500900050120     C  N32              MOVEL     TBLUNI        DE2CEI
501000050120     C*!!32              MOVEL     *BLANKS       DE2CEI
501100050120    5C**!!!              END
501200050120    4C                   END
501300050120      * Controllo se devo proteggere i campi x i convenuti
501400050120    4c                   If        Not *In12 and TotImv > 0
501500050120     c                   Eval      *In25 = *On
501600050120    4c                   EndIf
501700050120    4c                   If        *In12 and Ar6Fim = 'P'
501800050120     c                   Eval      *In25 = *On
501900050120    4c                   EndIf
502000050120     C*
502100050120    3c                   EndIf
502200050120     C*
502300050120    2C                   END
502400050120    1C                   END
502500050120     C*
502600911028     C                   ENDSR
502700911028     C*
502800990913      *--- TASSAZIONE BOLLA ------------------------------------------*
502900911028     C     TASSAZ        BEGSR
503000990909     C                   CLEAR                   DTAS
503100990909     C                   CLEAR                   DTASV
503200950104     C                   CLEAR                   VIDMSG
503300060123     C***                CLEAR                   WFIN
503400060123     C***                CLEAR                   WVRV
503500020415     C                   CLEAR                   Wtassa            1
503600030605     C                   CLEAR                   Wtassad           1
503700030618     C                   CLEAR                   itassa           11 3
503800050926     C                   CLEAR                   NOEliminaporto
503900911028     C*
504000941217    1C     VIDKSC        IFEQ      9999
504100931109     C                   MOVE      TSZKSC        TASKSC
504200941217     C                   MOVEL     TSZCTR        TASCTR
504300931109     C*
504400931109     C                   ELSE
504500911122     C*
504600931109     C                   MOVEL     VIDKLP        TASKSC
504700931109     C                   MOVE      VIDKSC        TASKSC
504800941217     C                   MOVEL     VIDCTR        TASCTR
504900941217    2C                   ENDIF
505000941217     C*
505100941217     C                   Z-ADD     ARBPKF        TASPKF
505200160530     C                   Z-ADD     ARBPKb        TASPKb
505300050714     C***                MOVEL     ARBLNP        TASLNP
505400050714     C                   MOVEL     VUNICOLNP     TASLNP
505500941217     C                   MOVEL     ARBCTS        TASCTS
505600941217     C                   MOVEL     ARBDSP        TASDSP
505700970414     C                   MOVEL     ARBDSP        TASDCT
505800941217     C                   MOVEL     ARBVLF        TASVLF
505900160530     C                   MOVEL     ARBfvf        TASfvf
506000160530     c                   z-add     arbvlc        tasvlc
506100160530     c                   z-add     arbncr        tasncr
506200941217     C                   MOVEL     ARBTSP        TASTSP
506300980512     C     WBOL          IFEQ      '1'
506400941217     C                   MOVEL     §3ATB1        TASTBL
506500990913     C                   MOVEL     VIDDIV        TASDIV
506600990913     C                   Z-ADD     VIDIMV        TASIMV
506700980512     C                   ELSE
506800980512     C                   MOVEL     §3ATB2        TASTBL
506900990913     C                   MOVEL     VI2DIV        TASDIV
507000040119     C**!!!              Z-ADD     VI2VA1        TASIMV
507100040119     c                   Z-add     TotImv        TasImv
507200980512     C                   ENDIF
507300941217     C                   MOVEL     ARBLNA        TASLNA
507400941217     C                   MOVEL     ARBNCL        TASNCL
507500160530     C                   MOVEL     ARBNCL        TASNCLb
507600990909     C                   Z-ADD     VIDPOR        TASPOR
507700941217     C                   MOVEL     VIDQFT        TASQFT
507800980512     C     WBOL          IFEQ      '1'
507900980512     C     §3AFCA        ANDEQ     1
508000980512     C     WBOL          OREQ      '2'
508100980512     C     §3AFCA        ANDEQ     2
508200941217     C                   MOVEL     AR9CAS        TASCAS
508300941217     C                   MOVEL     AR9VCA        TASVCA
508400941217     C                   MOVEL     AR9TIC        TASTIC
508500980512     C                   ENDIF
508600941217     C                   MOVEL     VIDIAS        TASIAS
508700941217     C                   MOVEL     VIDVAS        TASVAS
508800941217     C                   MOVEL     ARBCAD        TASCAD
508900990909     C                   MOVEL     ARBNZD        TASNZD
509000941217     C                   MOVEL     ARBFIN        TASFIN
509100050715     C                   MOVEL     Vunicocam     TASCAM
509200050715     C**                 MOVEL     ARBCAM        TASCAM
509300050714     C**                 MOVEL     ARBNZM        TASNZM
509400050714     C                   MOVEL     Vuniconzm     TASNZM
509500050715     C                   MOVEL     Vunicofap     TASFAP
509600050715     C**                 MOVEL     ARBFAP        TASFAP
509700941217     C                   MOVEL     ARBTC1        TASTC1
509800941217     C                   MOVEL     ARBTC2        TASTC2
509900970409     C     WDDT          IFEQ      'C'                                          SI DDT
510000970409     C     WDDT          OREQ      'S'
510100031028     c     wddt          oreq      'P'
510200031028     c     wddt          oreq      'Q'
510300970409     C                   MOVEL     'S'           TASDDT
510400970409     C                   ENDIF
510500060424     c* flag bolla di reso in TASFLO
510600060925     c****               movel     arbfbr        tasflo
510700060925     c                   clear                   dtas01
510800060925     c                   movel     arbfbr        §asfbr
510900060925     c                   movel     arbcca        §ascca
511000110629     c* avviso al destinatario
511100110629     c                   movel     wemd          §asemd
511200150107     c* pin code
511300150107     c                   if        wpincode='S'
511400150107     c                   movel     'S'           §aspin
511500150107     c                   endif
511600060925     c                   eval      tasflo = dtas01
511700151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
511800151127     c* è solo per tassare una specifica varia
511900151127     C     WBOL          IFEQ      '1'
512000151127     c* PACKING LIST
512100151127     c                   Eval      kAr5Trd = 'GEN'
512200151127     c     kFiar5        Chain     Fiar501l
512300151127     c                   if        %found(fiar501l)
512400151127     c                   movel     ar5uni        dar5gen
512500151127     c                   if        §ar5alx='S' or §ar5als='S'
512600151127     c                   eval      tasspl='S'
512700151127     c                   endif
512800151127     c                   endif
512900151127     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
513000151127     C                   MOVEL     'M'           WTRC
513100151127     C     KARB4         CHAIN(N)  FiAR401l
513200151127     c                   if        %found(fiar401l)
513300151127     c                   movel     ar4not        dsbl4m
513400151127     c                   if        §b4ado='S'
513500151127     c                   eval      tasprt='S'
513600151127     c                   endif
513700151127     c                   endif
513800151127
513900151127     c                   endif
514000961114     C**
514100961114     C* SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
514200961114     C*                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
514300020222     c                   if        lnantw <> 'EEX' or lnantw <> 'EUP' or
514400020222     c                             lnantw <> 'FED' or lnantw <> 'DPD'
514500060123     C***                MOVEL     ARBFDN        TASFDN
514600060123     C                   MOVEL     vunicoFDN     TASFDN
514700961114     C                   ENDIF
514800961114     C**
514900970710     C* CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
515000050715     c* Se bolla dirottata, e posso utilizzare i dirottamenti
515100050715     c*  prendo quello già calcolato
515200050715     c                   if        Vunicomct<>'  ' and
515300050715     c                             (§vpodir='A' or §vpodir='E')
515400050715     c                   movel     Vunicomct     tasmct
515500050715     c                   else
515600050715     c
515700970710     C                   CLEAR                   DSSI95
515800970710     C                   MOVEL     ARBLOM        I95LOC
515900050715     C                   MOVEL     Vunicocam     I95CAP
516000050715     C**                 MOVEL     ARBCAM        I95CAP
516100050714     C**                 MOVEL     ARBNZM        I95NAR
516200050714     C                   MOVEL     Vuniconzm     I95NAR
516300970710     C                   MOVEL     '3'           I95TCN
516400971103     C                   MOVEL     ARBDSP        I95DAT
516500971201     C     §3AFCA        IFNE      0
516600971201     C                   MOVEL     'S'           I95FCA
516700971201     C                   ENDIF
516800971201     C                   MOVEL     §3ATB1        I95TPO
516900020228      * Se network FedEx passo flag apposito per dire di prendere i dati
517000020228      * dalla tabella della nazione
517100020228     c     lnpntw        ifeq      'FED'
517200020228     c                   eval      i95fi1 = 'S'
517300020228     c                   endif
517400020228
517500971105     C                   CALL      'TISI95R'
517600970710     C                   PARM                    DSSI95
517700971105     C     O95ERR        IFEQ      *BLANKS
517800970710     C                   MOVEL     O95CTS        TASMCT
517900951027     C                   ENDIF
518000050715     C                   ENDIF
518100050715     c
518200050715     C*****              MOVEL     'S'           WCAL13            1
518300941219     C* CARICO LE VARIE
518400980512     C     WBOL          IFEQ      '1'
518500941219    2C                   DO        20            NRR
518600050317     C     NRR           CHAIN     LRA2S06                            32
518700030624      * SE ESISTE LA VARIA "&" CON TUTTI 8888888 NON LA PASSO AL TNSF20R
518800030624      * MA PASSO UN FLAG  DI RICHIESTA CALCOLO VARIA "&"
518900030624     C                   Z-ADD     NRR           C
519000030624     C                   IF        V6CSV1 = '&' AND V6CVA1 = 88888888
519100030624     C                   EVAL      TASFCAA = 'S'
519200030624     C                   ELSE
519300941219     C                   MOVEL     V6CSV1        VSV(C)
519400970606     C                   MOVEL     V6CEV1        VES(C)
519500060123     C***  V6CSV1        IFEQ      §$2FIN
519600060123     C***                MOVE      '1'           WFIN              1
519700060123     C***                ELSE
519800060123     C***  V6CSV1        IFEQ      §$2VRV
519900060123     C***                MOVE      '1'           WVRV              1
520000060123     C***                ENDIF
520100060123     C***                ENDIF
520200030624     C                   ENDIF
520300020415     c* se devo tassare una specifica varia(da tabella 3A) e c'e' gia'
520400020415     c*  non richiamo la tassazione
520500040119     c**!!!              if        §3asva<>*blanks and v6csv1=§3asva
520600040119     c                   if        §3asva<>*blanks and v6csv1=§3asva and
520700040119     c                             Tasimv > 0
520800020415     c                   eval      wtassa='N'
520900030618      * mi salvo l'importo della varia
521000030618     c                   eval      itassa=v6cva1
521100020415     c                   endif
521200030605     c* se esiste già la varia "D" non la ricalcolo
521300030605     c*
521400030605     c                   if        v6csv1='D'
521500030605     c                   eval      wtassad='N'
521600030605     c                   endif
521700030925     c* se il valore del diritto di fatturazione in anagrafica è uguale a zero non lo calcolo
521800030925     c*
521900030925     c                   if        sav_indsin = 0
522000030925     c                   eval      wtassad='N'
522100030925     c                   endif
522200970606     C* SE NON LA DEVO PASSARE, LA PASSO VUOTA LASCIANDO IL FLAG IMPOST
522300970606     C*   PER NON PERDERE LA POSIZIONE CHE SI E' TENUTA MEMORIZZATA
522400970606     C     WCANCN        IFEQ      'N'
522500970606     C     V6CSV1        ORNE      'N'
522600030624     C*
522700941219     C                   Z-ADD     V6CVA1        VVA(C)
522800030624      *
522900970606     C                   ENDIF
523000030624     C                   IF        V6CSV1 = '&' AND V6CVA1 = 88888888
523100030624     C                   z-add     *zeros        vva(c)
523200030624     C                   endif
523300941219    2C                   ENDDO
523400980512     C                   ELSE
523500040121      * carico varie seconda bolla
523600980512     C                   MOVEL     'G'           TASSVA
523700040119     c                   Eval      TascVag = 'S'
523800980608     C     VI2VA1        IFGT      0
523900980608     C                   MOVEL     VI2SV1        VSV(1)
524000980608     C                   Z-ADD     VI2VA1        VVA(1)
524100040121     c                   Movel     Vi2Es1        ves(1)
524200980608     C                   ENDIF
524300040119     c                   If        Vi2Va2 > *Zeros
524400040119     c                   Movel     Vi2Sv2        Vsv(2)
524500040119     c                   Z-add     Vi2Va2        Vva(2)
524600040121     c                   Movel     Vi2Es2        ves(2)
524700040119     c                   EndIf
524800040119     c                   If        Vi2Va3 > *Zeros
524900040119     c                   Movel     Vi2Sv3        Vsv(3)
525000040119     c                   Z-add     Vi2Va3        Vva(3)
525100040121     c                   Movel     Vi2Es3        ves(3)
525200040119     c                   EndIf
525300040119     c                   If        Vi2Va4 > *Zeros
525400040119     c                   Movel     Vi2Sv4        Vsv(4)
525500040119     c                   Z-add     Vi2Va4        Vva(4)
525600040121     c                   Movel     Vi2Es4        ves(4)
525700040119     c                   EndIf
525800041129     c                   If        Vi2Va5 > *Zeros
525900041129     c                   Movel     Vi2Sv5        Vsv(5)
526000041129     c                   Z-add     Vi2Va5        Vva(5)
526100041129     c                   Movel     Vi2Es5        ves(5)
526200041129     c                   EndIf
526300041129     c                   If        Vi2Va6 > *Zeros
526400041129     c                   Movel     Vi2Sv6        Vsv(6)
526500041129     c                   Z-add     Vi2Va6        Vva(6)
526600041129     c                   Movel     Vi2Es6        ves(6)
526700041129     c                   EndIf
526800040126      * se segue fattura imposto wtassad = 'N' in questo modo il tnsf20r non mi
526900040126      * tassa la varia 'D'
527000040126     c                   If        *In01
527100040126     c                   Eval      wtassad = 'N'
527200040126     c                   EndIf
527300980512     C                   ENDIF
527400011204
527500030203     c                   z-add     arbncp        Tasncp
527600030203     c                   z-add     arbpkc        Taspkc
527700030203
527800030203      * Bancali
527900030205     c                   If        %Subst(ArbGva:1:1) = 'B'
528000030203     c                   Z-Add     VidBan        TasBan
528100030205     c                   EndIf
528200030728      * Colli originali
528300030728     c                   If        %Subst(ArbGva:1:1) = 'O'
528400030729     c                   Z-Add     VidCor        TasNcl
528500030728     c                   EndIf
528600011204
528700941217     C* ELABORO E CHIUDO CON LR IL PGM
528800011113     C                   MOVEL     ' '           TASTLA
528900941217     C*
529000020415     c                   if        wtassa=' '
529100020415     c* se devo tassare solo una varia, da tipo bolla, la imposto
529200020415     c                   if        §3asva<>*blanks
529300020415     c                   eval      tassva=§3asva
529400020415     c                   endif
529500030620     c*
529600030604     c                   endif
529700030604      * per fattura immediata passo al TNSF20R la varia  "D" e l'imorto del diritto di fatturazione
529800030926      * da sommare che prendo CNIND (INDSIN) solo se è diverso da zero
529900030604      *
530000030926     c                   if         wtassad = ' '
530100030605     c                   move      sav_indsin    tasivdf
530200030604      *
530300030604     c                   eval      tassvdf = 'D'
530400030926     c                   endif
530500030604      *
530600030605     c                   if        wtassa='N' and wtassad = ' '
530700030605     c* se ho già tassata la singola varia devo sommare il solo diritto di fatturazione se imponibil
530800030618     c* maggiore di zero oppure se imponibile a zero ho la singola varia valorizzata
530900040119     c**!!!              if        tasimv > 0  or (tasimv = 0 and itassa > 0)
531000040119     c                   if        tasimv > 0
531100030604     c                   eval      tassva='D'
531200030605     c                   else
531300030605     c                   eval      wtassad = 'N'
531400030605     c                   endif
531500030604     c                   endif
531600031124     c* in caso di tassato fino all'imponibile valorizzo la singola varia da calcolare se tassva
531700031124     c* uguale a  blank
531800031124     c                   if        wtassad = ' ' and tasimv > 0 and tassva = ' '
531900030625     c                   eval      tassva = 'D'
532000030625     c                   endif
532100020415     c**
532200030605     c                   if        wtassa = 'N' and wtassad = 'N'
532300030605     c                   goto      endtas
532400030605     c                   endif
532500050804     c
532600050804     c                   eval      parca  = ' '
532700050804     c                   movel     paramt        kpjbu
532800030605     c*
532900941217     C                   CALL      'TNSF20R'
533000941217     C                   PARM                    KPJBA
533100990909     C                   PARM                    DTAS
533200990909     C                   PARM                    DTASV
533300941217     C*
533400941217    1C     TASERR        IFNE      *BLANKS
533500980608     C     TASMSG        IFNE      *BLANKS
533600980608     C                   MOVEL(P)  TASMSG        VIDMSG
533700980608     C                   ELSE
533800980608     C                   MOVEL     MSG(13)       VIDMSG
533900980608     C                   ENDIF
534000990927     C                   SETON                                        4890
534100990915     C                   SETOFF                                       07
534200990105     C**
534300990105    2C     *IN96         IFEQ      *ON
534400990105     C     *IN25         ANDEQ     *ON
534500990105     C                   SETOFF                                       25
534600990105     C* SPROTEGGO LE VARIE
534700990105    3C                   DO        20            NRR
534800050317     C     NRR           CHAIN     LRA2S06                            32
534900050317     C  N32              UPDATE    LRA2S06
535000990105    3C                   ENDDO
535100990105    2C                   ENDIF
535200990105     C**
535300941217   X1C                   ELSE
535400950420     C*
535500980512     C     WBOL          IFEQ      '1'
535600050926     c
535700990914     C                   MOVEL     TASDIV        VIDDIV
535800991014     C                   MOVEL     TASDIV        WDIV
535900050926     c* Se dirottamento prendo solo il porto
536000060123   1ac****               if        Memsoloporto='S' and §3atb1<>'AR'
536100060123   1ac                   if        Memsoloporto='S'
536200050926     c* se calcolato me lo memorizzo
536300050926     c                   if        taspor>0
536400050926     c                   eval      NOeliminaporto='S'
536500050926     c                   endif
536600060123  x  c                   movel     taspor        vidpor
536700060123  c
536800060123     C* Vedo se nelle varie è presente Inoltro/Disagiata/Isola
536900060123     c                   movel     §$2fin        wvaria
537000060123     c                   exsr      Cervaria
537100060123     c                   movel     §$2vrk        wvaria
537200060123     c                   exsr      Cervaria
537300060123     c                   movel     §$2vrj        wvaria
537400060123     c                   exsr      Cervaria
537500050926     c
537600060123   1ac                   else
537700950420     C                   MOVEL     TASQFT        VIDQFT
537800060123     C**                 CLEAR                   W001A
537900060123     C**   VIDIMV        IFGT      0
538000060123     C**                 MOVE      '1'           W001A             1
538100060123     C**                 ENDIF
538200911028     C                   Z-ADD     0             VIDIMV
538300941217     C* CARICO LE VARIE
538400941217    2C                   DO        20            NRR
538500050317     C     NRR           CHAIN     LRA2S06                            32
538600060123     C                   MOVEL     VSV(NRR)      V6CSV1
538700060123     C                   Z-ADD     VVA(NRR)      V6CVA1
538800060123     C                   MOVEL     VES(NRR)      V6CEV1
538900991214     C* SE TIPO BOLLA "AR" E INOLTRO E ISTAT NON PRESENTI NON CARICO
539000991214     C* LA RELATIVA VARIA
539100060123     C**                 SELECT
539200060123     C**   VSV(NRR)      WHENEQ    §$2FIN
539300060123     C**   WFIN          IFEQ      '1'
539400060123     C**   §3ATB1        ORNE      'AR'
539500060123     C**                 MOVEL     VSV(NRR)      V6CSV1
539600060123     C**                 Z-ADD     VVA(NRR)      V6CVA1
539700060123     C**                 MOVEL     VES(NRR)      V6CEV1
539800060123     C**                 ELSE
539900060123     C**                 CLEAR                   V6CSV1
540000060123     C**                 CLEAR                   V6CVA1
540100060123     C**                 CLEAR                   V6CEV1
540200060123     C**                 CLEAR                   VSV(NRR)
540300060123     C**                 CLEAR                   VVA(NRR)
540400060123     C**                 CLEAR                   VES(NRR)
540500060123     C**                 ENDIF
540600060123     C**   VSV(NRR)      WHENEQ    §$2VRV
540700060123     C**   WVRV          IFEQ      '1'
540800060123     C**   §3ATB1        ORNE      'AR'
540900060123     C**                 MOVEL     VSV(NRR)      V6CSV1
541000060123     C**                 Z-ADD     VVA(NRR)      V6CVA1
541100060123     C**                 MOVEL     VES(NRR)      V6CEV1
541200060123     C**                 ELSE
541300060123     C**                 CLEAR                   V6CSV1
541400060123     C**                 CLEAR                   V6CVA1
541500060123     C**                 CLEAR                   V6CEV1
541600060123     C**                 CLEAR                   VSV(NRR)
541700060123     C**                 CLEAR                   VVA(NRR)
541800060123     C**                 CLEAR                   VES(NRR)
541900060123     C**                 ENDIF
542000060123     C**                 OTHER
542100060123     C**                 MOVEL     VSV(NRR)      V6CSV1
542200060123     C**                 Z-ADD     VVA(NRR)      V6CVA1
542300060123     C**                 MOVEL     VES(NRR)      V6CEV1
542400060123     C**                 ENDSL
542500060123     c
542600050317     C   32              WRITE     LRA2S06
542700050317     C  N32              UPDATE    LRA2S06
542800941217    2C                   ENDDO
542900941217     C*
543000991119     C* SE CODICE BOLLA "AR" NON DEVO CALCOLARE TRASPORTO
543100920213     C*  QUINDI ANCHE TOTALE IMPONIBILE: DA INSERIRE MANUALMENTE
543200060123    2C**   §3ATB1        IFNE      'AR'
543300060123     C**   §3ATB2        ANDNE     'AR'
543400060123     C**   W001A         OREQ      '1'
543500991214     C                   Z-ADD     TASIMV        VIDIMV
543600060123     C**                 ENDIF
543700991119     C* SE C'ERA GIA' LO RIPRENDO PERCHE' PUO' ESSERE STATO CONVERTITO
543800060123    2C**   §3ATB1        IFNE      'AR'
543900060123     C**   §3ATB2        ANDNE     'AR'
544000060123     C**   VIDPOR        ORGT      *ZEROS
544100990915     C                   Z-ADD     TASPOR        VIDPOR
544200991214     C* SE LA BOLLA E' "AR" RICALCOLO IL TOTALE IMPONIBILE
544300060123     C**   W001A         IFEQ      *BLANKS
544400060123     C**   §3ATB1        ANDEQ     'AR'
544500060123     C**                 CLEAR                   DSLV16
544600060123     C**                 MOVEL     'T'           D17FIM
544700060123     C**                 Z-ADD     VIDPOR        D17POR
544800060123     C**                 CALL      'FNLV16R'
544900060123     C**                 PARM                    DSLV16
545000060123     C**                 PARM                    DTASV
545100060123     C**                 Z-ADD     O17IMV        VIDIMV
545200060123     C**                 ENDIF
545300060123    2C**                 ENDIF
545400050926   1aC                   ENDIF
545500060123     c
545600060123     C* SECONDA BOLLA
545700050926   x1C                   ELSE                                                   WBOL = 2
545800040119     c                   Clear                   ContaSv
545900040121      * carico le varie seconda bolla
546000040119Do  2c                   Do        20            c
546100040119If  3c                   If        Vsv(c) <> §$2Iva and
546200040119     c                             Vsv(c) <> §$2Bol
546300040119     c                   Add       1             ContaSv
546400040119S   4c                   Select
546500040119     c                   When      ContaSv = 1
546600040119     c                   Movel     Vsv(c)        Vi2Sv1
546700040119     c                   Z-add     Vva(c)        Vi2Va1
546800040121     c                   Movel     Ves(c)        Vi2Es1
546900040119     c                   When      ContaSv = 2
547000040119     c                   Movel     Vsv(c)        Vi2Sv2
547100040119     c                   Z-add     Vva(c)        Vi2Va2
547200040121     c                   Movel     Ves(c)        Vi2Es2
547300040119     c                   When      ContaSv = 3
547400040119     c                   Movel     Vsv(c)        Vi2Sv3
547500040119     c                   Z-add     Vva(c)        Vi2Va3
547600040121     c                   Movel     Ves(c)        Vi2Es3
547700040119     c                   When      ContaSv = 4
547800040119     c                   Movel     Vsv(c)        Vi2Sv4
547900040119     c                   Z-add     Vva(c)        Vi2Va4
548000040121     c                   Movel     Ves(c)        Vi2Es4
548100041129     c                   When      ContaSv = 5
548200041129     c                   Movel     Vsv(c)        Vi2Sv5
548300041129     c                   Z-add     Vva(c)        Vi2Va5
548400041129     c                   Movel     Ves(c)        Vi2Es5
548500041129     c                   When      ContaSv = 6
548600041129     c                   Movel     Vsv(c)        Vi2Sv6
548700041129     c                   Z-add     Vva(c)        Vi2Va6
548800041129     c                   Movel     Ves(c)        Vi2Es6
548900040119    4c                   EndSl
549000040119    3c                   EndIf
549100040119    2c                   EndDo
549200040119     c                   Movel     TasDiv        Vi2Div
549300040119     c                   Movel     TasDiv        wdiv
549400040122     c                   Z-add     TasImv        TotImv
549500040119     C**!!!              Z-ADD     1             C
549600040119     C**!!!TASSVA        LOOKUP    VSV(C)                                 57
549700040119     C*!!57              Z-ADD     VVA(C)        VI2VA1
549800040119     C*!!57              MOVEL     VSV(C)        VI2SV1
549900040119     C*!!57              MOVEL     TASDIV        VI2DIV
550000040119     C*!!57              Z-ADD     VVA(1)        VA2VA1
550100040119     C*!!57              MOVEL     TASDIV        WDIV
550200040119     C*!!57VES(C)        IFNE      ' '
550300040119     C**!!!VI2CEI        ANDEQ     ' '
550400040119     C**!!!              MOVEL     VES(C)        VI2CEI
550500040119     C**!!!              ENDIF
550600980512     C                   ENDIF
550700030625      *
550800030625      * nel caso di calcolo varia "D" con imponibile > di 0 ricalcolo il totale imponibile
550900030625      *
551000030625     c                   if        tassva = 'D'
551100030625     C                   CLEAR                   DSLV16
551200030625     C                   MOVEL     'T'           D17FIM
551300030625     C                   Z-ADD     VIDPOR        D17POR
551400030625     C                   CALL      'FNLV16R'
551500030625     C                   PARM                    DSLV16
551600030625     C                   PARM                    DTASV
551700030625     C                   Z-ADD     O17IMV        VIDIMV
551800030625     c                   endif
551900980512     C*
552000030620    1C                   ENDIF
552100030605     C     endtas        ENDSR
552200911028     C*
552300060123     c* Carico le varie Inoltro/Disagiata/Isola quando tasso bolle leg
552400060123     c*  sia per Fattura Immediata che segue fattura -----------------*
552500060123
552600060123     c     CerVaria      BEGSR
552700060123     c                   z-add     1             xx
552800060123     c     Wvaria        lookup    vsv(XX)                                32
552900060123     c                   if        *in32
553000060123     c
553100060123    2C                   DO        20            NRR
553200060123     C     NRR           CHAIN     LRA2S06                            32
553300060123     c                   if        v6CSV1=' '
553400060123     C                   MOVEL     VSV(xx)       V6CSV1
553500060123     C                   Z-ADD     VVA(xx)       V6CVA1
553600060123     C                   MOVEL     VES(xx)       V6CEV1
553700060123     c                   eval      NOeliminaporto='S'
553800060123     C   32              WRITE     LRA2S06
553900060123     C  N32              UPDATE    LRA2S06
554000060123     c                   leave
554100060123     c                   endif
554200060123     c                   enddo
554300060123     c
554400060123     c                   endif
554500060123     c
554600060123     c                   ENDSR
554700911028     C*--- CONTROLLI CAMPI TASSAZIONE --------------------------------*
554800990916     C     CONTR6        BEGSR
554900911030     C                   SETOFF                                       90
555000941221     C                   CLEAR                   VIDMSG
555100930422     C*
555200930422     C                   MLLZO     1             VIDPOR
555300930422     C                   MLLZO     1             VIDIMV
555400991028     C* PRENDO I DATI DELLA DIVISA DELLA TARIFFA
555500991028     C                   MOVEL     'CV'          COD
555600991028     C                   MOVEL(P)  §TADIV        KEY
555700991028     C                   EXSR      CTRTAB
555800991028     C                   MOVEL     TBLUNI        DSCV
555900011003     C                   MOVEL     §CVDEC        COMDEC
556000950120     C*
556100011127     C*** I M P O R T O  D A  A S S I C U R A R E  ***
556200950720     C     *IN97         IFEQ      *ON
556300950720     C                   MOVEL     '1'           WIN97             1
556400950720     C                   ENDIF
556500950120     C*
556600950120     C                   MOVEL     VIDVAS        WVLT
556700950120     C                   Z-ADD     VIDIAS        W0133            13 3
556800950120     C* CONTROLLO LA DIVISA
556900950120     C                   EXSR      CTRVLT
557000950120     C                   MOVEL     WVLT          VIDVAS
557100950120     C* ERRORE
557200950120     C   90              SETON                                        41
557300950120     C   90              GOTO      ENDCT6
557400950120     C*
557500950120     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
557600950120    1C     VIDVAS        IFNE      VARVAS
557700950720     C     WIN97         ANDEQ     *BLANKS
557800950120     C*
557900011002    2C     VIDVAS        IFNE      *BLANKS
558000011002     C     VIDIAS        ORNE      0
558100950120     C                   MOVEL     MSG(14)       VIDMSG
558200950120     C                   SETON                                        4290
558300950120     C                   GOTO      ENDCT6
558400011002    2C                   ENDIF
558500950120     C*
558600950120    1C                   ENDIF
558700950120     C*
558800950120     C                   MOVEL     VIDVAS        VARVAS
558900950720     C                   CLEAR                   WIN97
559000011128
559100011128      * pulisco il flag x la forzatura importo da assicurare forzabile
559200011203     c                   if        vidias <> sav_vidias
559300011128     c                   clear                   forza
559400011128     c                   endif
559500011127
559600020222      * CONTROLLO IMPORTO DA ASSICURARE
559700011128      *---------------------------------------
559800011203  1b c                   if        vidias <> 0
559900011203     c                   eval      sav_vidias = vidias
560000011127      * Controllo l'importo da assicurare tramite il trul22r
560100011127     c                   clear                   trul22ds
560200011127     c                   eval      i22tla = 'L'
560300011127     c                   eval      i22cbo = arbcbo
560400011127     c                   eval      i22tsp = arbtsp
560500050714     c*****              eval      i22lnp = arblnp
560600050714     c*****              eval      i22NZM = arbnzm
560700050714     c                   eval      i22lnp = VUNICOLNP
560800050714     c                   eval      i22nzm = VUNICONZM
560900011127     c                   eval      i22lna = arblna
561000011127     c                   eval      i22nzd = arbnzd
561100011128     c                   movel     vidklp        i22ksc
561200011128     c                   move      vidksc        i22ksc
561300011127     c                   eval      i22ctr = vidctr
561400011203     c                   eval      i22imp = vidias
561500011203     c                   eval      i22div = vidvas
561600011128     c                   eval      i22ute = knmus
561700011128     c                   eval      i22pgm = nompgm
561800011127     c                   call      'TRUL22R'
561900011127     c                   parm                    trul22ds
562000011128      * se ho tutti i flag impostati non posso immettere l'importo
562100011127      * da assicurare
562200011128  2b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
562300011128     c                             and o22fx2 <> *blanks
562400011127     c                   eval      vidmsg = msg(43)
562500011127     c                   eval      *in42 = *on
562600011127     c                   eval      *in90 = *on
562700011128     c                   goto      endct6
562800011127  2e c                   endif
562900011203      * controllo che non sia minore del limite previsto
563000011128  2b c                   if        o22fmn <> *blanks
563100011128     c                   eval      vidmsg = msg(46)
563200011128     c                   eval      *in42 = *on
563300011128     c                   eval      *in90 = *on
563400011128     c                   goto      endct6
563500011128  2e c                   endif
563600011128      * controllo che non sia maggiore del limite previsto
563700011128      * non forzabile
563800011128  2b c                   if        o22fx2 <> *blanks
563900011203     c                   eval      vidmsg = msg(15)
564000011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
564100011128     c                   eval      *in42 = *on
564200011128     c                   eval      *in90 = *on
564300011128     c                   goto      endct6
564400011128  2e c                   endif
564500011128      * controllo che non sia maggiore del limite previsto
564600011128      * forzabile
564700011128  2b c                   if        o22fx1 <> *blanks and forza = *blanks
564800011128     c                   eval      forza = '1'
564900011203     c                   eval      vidmsg = msg(57)
565000011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
565100011128     c                   eval      *in42 = *on
565200011128     c                   eval      *in90 = *on
565300011128     c                   goto      endct6
565400011128  2e c                   endif
565500011129  1e c                   endif
565600011128
565700950120     C*
565800950120     C* SE HA TARIFFE, CONTROLLO
565900950120     C*
566000950120     C* SE CLIENTE NON E' 9999 --> CONTROLLO L'IMPORTO DA ASSICURARE
566100950120    1C     VIDKSC        IFNE      9999
566200950120     C     *IN29         ANDEQ     *OFF
566300950120     C*
566400950421     C* SE FATTURA IMMEDIATA, TARIFFA A VALORE E FLAG = '3':
566500950421     C* SE VARIATO IMPORTO DA ASSICURARE OBBLIGO AZZERAMENTO TRASPORTO
566600950421     C* E IMPONIBILE SE DIVERSI DA 0.
566700950421     C                   MOVEL     VIDCTR        W001A
566800950421     C     *IN01         IFEQ      *OFF
566900950421     C     W001A         ANDEQ     '4'
567000950421     C     TAMFVD        ANDEQ     '3'
567100950720     C     VIDIAS        ANDNE     VARIAS
567200991013     C* Carico varie del video in schiera per vedere se c'è l'inoltro
567300991013     C                   MOVEL     '1'           WCKINL            1
567400991013     C                   EXSR      CARWAR
567500950720     C*
567600950421     C     VIDPOR        IFNE      *ZEROS
567700950421     C     VIDIMV        ORNE      *ZEROS
567800991013     C     WINL          OREQ      '1'
567900950421     C                   MOVEL     MSG(37)       VIDMSG
568000950421     C                   SETON                                        4990
568100950421     C                   GOTO      ENDCT6
568200950720     C                   ENDIF
568300950720     C                   ENDIF
568400950720     C*
568500950720     C* PULISCO CAMPI D FORZATURA
568600011002    2C     W0133         IFNE      VARIAS
568700950720     C                   CLEAR                   DIE
568800950720     C                   CLEAR                   CIN
568900950720    2C                   ENDIF
569000950720     C*
569100950720     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
569200011002    2C     W0133         IFGT      0
569300950720     C     DIE           ANDNE     1
569400950720     C     TAMFIS        ORNE      '0'
569500011002     C                   Z-ADD     W0133         VARIAS
569600020715
569700020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
569800020715     c     §3atb1        Lookup    tbl                                    32
569900030728      * non deve controllare anche x le bolle 'FW'- recapito c/assegni
570000030728      * le bolle 'FW' sono tipo bolla 'A2'
570100030728     c                   If        ArbCbo = 'FW'
570200030728     c                   Eval      *In32 = *Off
570300030728     c                   EndIf
570400020715
570500020715   2Ac                   If        *in32 = *On
570600950720     C*
570700950720     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
570800990817     C     KTPT          CHAIN     TITPT000                           32
570900950720     C*
571000950720    3C     *IN32         IFEQ      *ON
571100950720     C     TPTATB        ORNE      ' '
571200950720     C     TPTAPL        ORNE      'P'
571300950120     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
571400950120     C*                                         IMPORTO DA TARIFFA
571500950720    4C     TAMFIS        IFEQ      '1'
571600950120     C     CIN           ANDNE     1
571700011002     C     W0133         ANDEQ     0
571800950120     C                   MOVEL     MSG(16)       VIDMSG
571900950120     C                   SETON                                        4290
572000950120     C                   ADD       1             CIN
572100950120     C                   GOTO      ENDCT6
572200950720    4C                   ENDIF
572300950120     C*
572400950120     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
572500950720    4C     TAMFIS        IFEQ      '3'
572600011002     C     W0133         ANDEQ     0
572700950509     C                   MOVEL     MSG(38)       VIDMSG
572800950120     C                   SETON                                        4290
572900950120     C                   GOTO      ENDCT6
573000950720    4C                   ENDIF
573100950120     C*
573200950120     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
573300950720    4C     TAMFIS        IFEQ      '2'
573400011002     C     W0133         ANDNE     0
573500950120     C                   MOVEL     MSG(17)       VIDMSG
573600950120     C                   SETON                                        4290
573700950120     C                   GOTO      ENDCT6
573800950720    4C                   ENDIF
573900950720    3C                   ENDIF
574000950720     C* SE C'E' VALORE MERCE:
574100950720    3C  N32TPTATB        IFEQ      ' '
574200950720     C     TPTAPL        ANDNE     'P'
574300011002     C     W0133         ANDGT     0
574400950720     C     DIE           ANDNE     1
574500950720     C     TPTVLM        ANDGT     0
574600950120     C*
574700950120     C* VALORE A COLLO
574800950120    4C     TPTFVM        IFEQ      '1'
574900991028     C     ARBNCL        MULT      TPTVLM        TPTVAL           13 3
575000950120   X4C                   ELSE
575100950120    5C     TPTFVM        IFEQ      '2'
575200991028     C                   Z-ADD     TPTVLM        TPTVAL
575300950120   X5C                   ELSE
575400950120     C     ARBPKF        ADD       0,9           W0060             6 0
575500991028     C     W0060         MULT      TPTVLM        TPTVAL
575600950120    5C                   END
575700950120    4C                   END
575800950120     C*
575900011204     C                   Z-ADD     VIDIAS        VIDVAL           13 3
576000991028    5C     §TADIV        IFNE      VIDVAS
576100991028     C                   CLEAR                   DSYEUR
576200991028     C                   MOVEL     VIDVAS        YECDVI
576300991028     C                   MOVEL     §TADIV        YECDVO
576400991028     C                   Z-ADD     VIDIAS        YECIMI
576500011003     C                   MOVE      COMDEC        YECDCO
576600991028     C*
576700991028     C                   CALL      'YEURCO'
576800991028     C                   PARM                    DSYEUR
576900991028     C*
577000991028    6C     YECESI        IFEQ      ' '
577100991028     C                   Z-ADD     YECIMO        VIDVAL
577200991028    6C                   ENDIF
577300991028    5C                   ENDIF
577400991028     C***
577500991028    4C     VIDVAL        IFGT      TPTVAL
577600950120     C                   ADD       1             DIE
577700011204     c                   eval      vidmsg = msg(18)
577800950120     C* PREPARO MESSAGGIO
577900011204     C                   SETON                                        4290
578000011204     C                   GOTO      ENDCT6
578100011204    4C                   ENDIF
578200950120     C*
578300950120    3C                   ENDIF
578400020715   2ac                   EndIf
578500950120    2C                   ENDIF
578600950120    1C                   ENDIF
578700950120     C***
578800011127     C*** Q U A N T I T A'  D A  F A T T U R A R E  ***
578900950120     C* CONTROLLO SOLO PER S.F.
579000950120    0C     *IN01         IFEQ      *ON
579100950120     C                   MOVEL     VIDCTR        W001A             1
579200980525     C**
579300980525     C** FLAG FORZATURA QTA DA FATTURARE
579400980525     C     VIDCTR        IFNE      SAVCTR
579500980525     C     *LIKE         DEFINE    VIDCTR        SAVCTR
579600980525     C                   CLEAR                   DUE
579700980525     C                   MOVEL     VIDCTR        SAVCTR
579800980525     C                   ENDIF
579900950120     C*
580000950120     C* SE MODIFICATA, PULISCO IL FLAG DI CONTROLLO
580100950120    1C     VIDQFT        IFNE      VARQFT
580200950120     C                   CLEAR                   UND
580300950120     C                   MOVEL     VIDQFT        VARQFT
580400950120    1C                   ENDIF
580500950120     C*
580600950120     C* CONTROLLO LA Q.TA DA FATTURARE CON TARIFFE
580700950120    1C     *IN29         IFEQ      *OFF
580800950120     C     W001A         ANDEQ     '4'
580900950120     C*
581000950120    2C     VIDQFT        IFEQ      0
581100950120     C*
581200950120     C* TAMFVD = '1' ASSUMO VALORE DA ASSICURARE SE NON IMMESSO
581300950120    3C     TAMFVD        IFEQ      '1'
581400950120     C     UND           ANDEQ     0
581500950120     C                   MOVEL     MSG(33)       VIDMSG
581600950120     C                   SETON                                        5690
581700950120     C                   MOVEL     1             UND
581800950120     C                   GOTO      ENDCT6
581900950120    3C                   ENDIF
582000961007     C* TAMFVD = '2'  O  '4' INSERIRE IN BOLLETTAZIONE
582100950120    3C     TAMFVD        IFEQ      '2'
582200961007    3C     TAMFVD        OREQ      '4'
582300950120     C                   MOVEL     MSG(34)       VIDMSG
582400950120     C                   SETON                                        5690
582500950120     C                   GOTO      ENDCT6
582600950120    3C                   ENDIF
582700950120     C*
582800950120   X2C                   ELSE
582900950120     C* TAMFVD = '3' ASSUME SEMPRE IMPORTO DA ASSICURARE: DA NON INSER
583000950120    3C     TAMFVD        IFEQ      '3'
583100950120     C                   MOVEL     MSG(35)       VIDMSG
583200950120     C                   SETON                                        5690
583300950120     C                   GOTO      ENDCT6
583400950120    3C                   ENDIF
583500950120    2C                   ENDIF
583600950120    1C                   ENDIF
583700950120     c* TARIFFA A QUANTITA'
583800950120    1C     W001A         IFEQ      '5'
583900950120     C     VIDQFT        ANDEQ     0
584000980525     C     DUE           ANDEQ     ' '
584100950120     C                   MOVEL     MSG(36)       VIDMSG
584200950120     C                   SETON                                        5690
584300980525     C                   MOVEL     '1'           DUE               1
584400950120     C                   GOTO      ENDCT6
584500950120    1C                   ENDIF
584600980525    0C                   ENDIF
584700011128      ***
584800011128      *** T A S S A Z I O N E ***
584900950120     C** PRIMA DI CONTROLLARE LA TASSAZIONE SE NON C'E' TOT
585000950120     C**  IMPONIBILE, RITASSO
585100990915     C* SE C'E' IL TOTALE IMPONIBILE RICHIAMO PER CONVERTIRE EVENTUAL-
585200990915     C* MENTE GLI IMPORTI
585300950120     C     *IN01         IFEQ      *OFF
585400950120     C     VIDKSC        ANDNE     9999
585500950120     C     TSZKSC        ORGT      0
585600950120     C     VIDKSC        ANDEQ     9999
585700990915     C* Se divisa sprotetta significa che manca tariffa: in questo caso
585800990915     C* controllo che la divisa inserita sia = alla divisa della moneta
585900990915     C*  di conto SE CLIENTE 9999
586000990915     C     *IN07         IFEQ      *OFF
586100990915     C     VIDKSC        ANDEQ     9999
586200990915     C     VIDDIV        ANDNE     §GEDCN
586300990915     C                   SETON                                        4890
586400990915     C                   MOVEA     MSG(47)       K78
586500990928     C                   MOVEA     §GEDCN        K78(64)
586600990915     C                   MOVEA     K78           VIDMSG
586700990915     C                   GOTO      ENDCT6
586800990915     C                   ENDIF
586900970606     C                   MOVEL     'N'           WCANCN            1
587000980512     C                   MOVEL     '1'           WBOL
587100950120     C                   EXSR      TASSAZ
587200950120     C*
587300950121     C   90              GOTO      ENDCT6
587400950120     C                   ENDIF
587500050926     c* Impossibile eliminare il porto se bolladirottata con porto
587600050926     c*  calcolato
587700050926     c                   if        NOeliminaporto='S' and  vidpor=0
587800050926     C                   SETON                                        4990
587900050926     C                   MOVEL     MSG(79)       VIDMSG
588000050926     C                   GOTO      ENDCT6
588100050926     c                   endif
588200941216     C**
588300991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
588400991124     C* TASSAZIONE
588500991124    1C     *IN07         IFEQ      *OFF
588600991124     C     VIDDIV        ANDNE     WDIV
588700991124     C     VIDDIV        ANDEQ     *BLANKS
588800991124    2C     VIDPOR        IFGT      *ZEROS
588900991125     C                   SETON                                        4890
589000991124     C                   MOVEL     MSG(48)       VIDMSG
589100991124     C                   GOTO      ENDCT6
589200991124   X2C                   ELSE
589300991124    3C                   DO        20            NRR
589400050317     C     NRR           CHAIN     LRA2S06                            30
589500991124    4C     V6CVA1        IFGT      *ZEROS
589600991125     C                   SETON                                        4890
589700991124     C                   MOVEL     MSG(48)       VIDMSG
589800991124     C                   GOTO      ENDCT6
589900991124    4C                   ENDIF
590000991124    3C                   ENDDO
590100991124    2C                   ENDIF
590200991124    1C                   ENDIF
590300990910     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
590400991013     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
590500991013     C* CONTROLLO DELLA DIVISA
590600991013    0C     *IN07         IFEQ      *OFF
590700991013     C     VIDDIV        ANDNE     WDIV
590800991026     C     WDIV          ANDNE     *BLANKS
590900100119     C                   CLEAR                   DLV1601
591000991013     C                   CLEAR                   DSLV16
591100991013     C                   CLEAR                   DTASV
591200991013     C                   MOVEL     'D'           D17FIM
591300991013     C                   MOVE      VIDDIV        D17DIV
591400991014     C     *IN01         IFEQ      *OFF
591500991014     C     VIDKSC        ANDNE     9999
591600991014     C     TSZKSC        ORGT      0
591700991014     C     VIDKSC        ANDEQ     9999
591800991014     C                   Z-ADD     DATEU         D17DSP
591900991014     C                   ELSE
592000991013     C                   Z-ADD     ARBDSP        D17DSP
592100991013     C                   END
592200991013     C                   CALL      'FNLV16R'
592300991013     C                   PARM                    DSLV16
592400991013     C                   PARM                    DTASV
592500991014     C                   MOVEL     D17DIV        VIDDIV
592600991013   X0C                   ELSE
592700990910     C                   CLEAR                   DSLV16
592800100119     C                   CLEAR                   DLV1601
592900990910     C                   CLEAR                   DTASV
593000941216     C                   MOVEL     'A'           D17TBO
593100950302     C                   MOVEL     VIDKLP        D17KSC
593200950302     C                   MOVE      VIDKSC        D17KSC
593300991014     C     *IN01         IFEQ      *OFF
593400991014     C     VIDKSC        ANDNE     9999
593500991014     C     TSZKSC        ORGT      0
593600991014     C     VIDKSC        ANDEQ     9999
593700991014     C                   Z-ADD     DATEU         D17DSP
593800991014     C                   ELSE
593900991014     C                   Z-ADD     ARBDSP        D17DSP
594000991014     C                   END
594100050714     c* serve solo per i franchi ...ma imposto lo stesso
594200050714     c*
594300050714     C***                MOVEL     ARBLNP        D17LNP
594400050714     C                   MOVEL     VUNICOLNP     D17LNP
594500941216     C                   MOVEL     ARBLNA        D17LNA
594600941216     C                   MOVEL     §3ATB1        D17TBL
594700941216     C                   MOVEL     §3ARBL        D17RBL
594800941216     C                   Z-ADD     VIDIAS        D17IAS
594900941216     C                   Z-ADD     VIDQFT        D17QFT
595000990910     C                   MOVEL     VIDDIV        D17DIV
595100990914     C                   MOVEL     WDIV          D17DVP
595200941216     C                   Z-ADD     VIDPOR        D17POR
595300941216     C                   Z-ADD     VIDIMV        D17IMV
595400950104    1C     §3AFCA        IFEQ      1
595500941216     C                   MOVEL     'S'           D17FCA
595600941217    1C                   ENDIF
595700990915     C*
595800941221     C     *IN01         IFEQ      *ON
595900941216     C                   MOVEL     'C'           D17FIM
596000941221     C                   ELSE
596100990915     C                   MOVEL     'E'           D17FIM
596200941221     C                   ENDIF
596300941216     C                   MOVEL     VIDCEI        D17CEI
596400100119
596500100119     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
596600100119     c                   Movel     §3asva        §d17Sva
596700100120     c
596800100119     c* Imposto il codice iso del destinatario e il flag di controllo in base all'esenz.IVA
596900100120     c                   if        not *in01
597000100119     c                   Eval      §d17ctriso='S'
597100100119     c                   Eval      §d17iso=vidisd
597200100119
597300100119     c                   if        vidpid<>*blanks and vidisd=*blanks
597400100119     c                   eval      §d17iso='IT'
597500100119     c                   endif
597600100120     c                   endif
597700100119
597800100119     c                   Movel     Dlv1601       D17Flo
597900100119     c
598000941216     C*
598100941216     C                   CLEAR                   C
598200970605     C                   CLEAR                   WVARN
598300030604     C                   CLEAR                   WVAR3A
598400040120     c                   Clear                   wvar3ai
598500941217    1C                   DO        20            NRR
598600050317     C     NRR           CHAIN     LRA2S06                            32
598700941220    2C     *IN32         IFEQ      *OFF
598800941216     C                   ADD       1             C
598900970605     C*
599000941216     C                   MOVEL     V6CSV1        VSV(C)
599100941216     C                   MOVEL     V6CVA1        VVA(C)
599200970605     C* MEMORIZZO SE C'E' LA VARIA N
599300970605     C     V6CSV1        IFEQ      'N'
599400970605     C                   Z-ADD     NRR           WVARN             5 0
599500970605     C                   ENDIF
599600030604     C* MEMORIZZO SE C'E' LA VARIA DELLA BOLLA NEL CASO DI BOLLE CON SINGOLA VARIA
599700030604     C                   IF        §3ASVA <> *BLANKS AND V6CSV1 = §3ASVA
599800030604     C                   MOVEL     'S'           WVAR3A            1
599900040120      * memorizzo anche se valorizzata
600000040120     c                   If        v6cva1 > 0
600100040120     c                   Eval      wvar3ai = 'S'
600200040120     c                   EndIf
600300030604     C                   ENDIF
600400941217    2C                   ENDIF
600500941217    1C                   ENDDO
600600941216     C*
600700990910     C                   CALL      'FNLV16R'
600800990910     C                   PARM                    DSLV16
600900990910     C                   PARM                    DTASV
601000941216     C*
601100941216     C                   MOVEL     D17CEI        VIDCEI
601200990923     C                   MOVEL     O17DEI        DESCEI
601300991011     C                   MOVEL     D17DIV        VIDDIV
601400941221     C*
601500990927     C     D17FIM        IFEQ      'E'
601600991014     C     O17ERR        ANDEQ     *BLANKS
601700030604      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
601800030604      * SOLO SE LA VARIA ESISTE
601900040120     C**!!!§3ASVA        ORNE      *BLANKS
602000040120     C**!!!WVAR3A        ANDEQ     'S'
602100040120     C**!!!O17ERR        ANDEQ     *BLANKS
602200040120     C**!!!D17FIM        ANDEQ     'C'
602300990910     C                   Z-ADD     O17IMV        VIDIMV
602400990927     C                   ENDIF
602500030604      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
602600030604     C     §3ASVA        IFNE      *BLANKS
602700030604     C     WVAR3A        ANDNE     'S'
602800030604     C     D17FIM        ANDEQ     'C'
602900030604     C                   Z-ADD     *ZEROS        VIDIMV
603000030604     C                   ENDIF
603100030604     c
603200950421     C* SE ERRORE TASSAZIONE SPROTEGGO CAMPI VIDEO PER POTERLI
603300950421     C* CORREGGERE
603400990910     C     O17ERR        IFNE      *BLANKS
603500950421     C                   SETOFF                                       25
603600950421     C                   END
603700941216     C* VARIE
603800941217    1C                   DO        20            NRR
603900050317     C     NRR           CHAIN     LRA2S06                            32
604000941216     C                   MOVEL     VSV(NRR)      V6CSV1
604100941216     C                   MOVEL     VES(NRR)      V6CEV1
604200941216     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
604300990910    2C     O17ERV        IFEQ      NRR
604400941221     C                   SETON                                        5290
604500941216     C                   ELSE
604600941216     C                   SETOFF                                       52
604700941217    2C                   ENDIF
604800050317     C  N32              UPDATE    LRA2S06
604900050317     C   32              WRITE     LRA2S06
605000941217    1C                   ENDDO
605100991014    0C                   ENDIF
605200941216     C*
605300941216     C                   SETOFF                                       52
605400941216     C*  ERRORE
605500990910    1C     O17ERR        IFNE      *BLANKS
605600941221     C                   SETON                                        90
605700941216     C*
605800990910    2C     O17MSG        IFNE      *BLANKS
605900990910     C                   MOVEL     O17MSG        VIDMSG
606000941216    2C                   ENDIF
606100941216     C**
606200941216     C* ESENZIONE IVA
606300990910    2C     O17ERR        IFEQ      '1'
606400941221     C                   SETON                                        51
606500941216    2C                   ENDIF
606600941216     C**
606700941216     C* IMPONIBILE
606800990910     C     O17ERR        IFEQ      '5'
606900941221     C                   SETON                                        50
607000941216    2C                   ENDIF
607100990916     C* DIVISA
607200990916     C     O17ERR        IFEQ      '7'
607300990916     C                   SETON                                        48
607400990916    2C                   ENDIF
607500991012     C* PORTO
607600991012     C     O17ERR        IFEQ      '8'
607700991012     C                   SETON                                        49
607800991012    2C                   ENDIF
607900941216     C**
608000941221     C                   GOTO      ENDCT6
608100941217    1C                   ENDIF
608200941221     C*
608300941221     C* PER COD 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
608400941221     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
608500990910    1C     O17FAS        IFEQ      'S'
608600050317     C     1             CHAIN     LRA2S06                            32
608700941221     C                   MOVEL     MSG(9)        VIDMSG
608800941221     C                   SETON                                        5290
608900050317     C                   UPDATE    LRA2S06
609000941221     C                   GOTO      ENDCT6
609100970605    1C                   ENDIF
609200040120      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
609300040120      * non posso avere altre varia con importo > 0
609400040120if  1c                   If        §3asva <> *Blanks
609500040120do  2c                   Do        20            nrr
609600050317     c     nrr           Chain     LRA2s06                            32
609700040120     c                   Eval      *In52 = *Off
609800040121if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
609900040121     c                             V6cSv1 <> *Blanks and V6cVa1 > *Zeros
610000040120     c                   Eval      *In52 = *On
610100040120     c                   Eval      *In90 = *On
610200040121     c                   Eval      VidMsg = Msg(60)
610300040121     c                   Eval      %subst(VidMsg:75:1) = §3asva
610400040120    3c                   EndIf
610500040121if  3c                   If        wvar3a = *Blanks  and
610600040121     c                             V6cSv1 <> *Blanks and V6cVa1 > *Zeros
610700040120     c                   Eval      *In52 = *On
610800040120     c                   Eval      *In90 = *On
610900040121     c                   Eval      VidMsg = Msg(60)
611000040121     c                   Eval      %subst(VidMsg:75:1) = §3asva
611100040120    3c                   EndIf
611200050317     c  n32              Update    LRA2s06
611300040127     c   90              Goto      Endct6
611400040120    2c                   EndDo
611500040120    1c                   EndIf
611600990914     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
611700990914    1C  N07VIDDIV        IFNE      WDIV
611800990914     C* CONVERSIONE PORTO
611900990914    2C     VIDPOR        IFEQ      VARPOR
612000990914     C     VIDPOR        ANDGT     *ZEROS
612100990914     C     WDIV          ANDNE     *BLANKS
612200990914     C                   CLEAR                   DSYEUR
612300990914     C                   MOVEL     WDIV          YECDVI
612400990914     C                   Z-ADD     VIDPOR        YECIMI
612500990914     C                   MOVEL     VIDDIV        YECDVO
612600990928     C     O17FDC        IFEQ      'S'
612700990914     C                   MOVEL     '3'           YECDCO
612800990928     C                   ELSE
612900990928     C                   CLEAR                   YECDCO
613000990928     C                   ENDIF
613100990914     C                   CALL      'YEURCO'
613200990914     C                   PARM                    DSYEUR
613300990914     C                   Z-ADD     YECIMO        VIDPOR
613400991013     C                   SETON                                        49
613500990914    2C                   END
613600990914     C* CONVERSIONE VARIE
613700990914    2C                   DO        20            NRR
613800050317     C     NRR           CHAIN     LRA2S06                            32
613900990914    3C     V6CVA1        IFEQ      VARVA1
614000990914     C     V6CVA1        ANDGT     *ZEROS
614100990928     C     WDIV          ANDNE     *BLANKS
614200011019      *
614300011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
614400011019      * non devo convertire la varia 'N' 88888888
614500011019     C     VIDIMV        IFEQ      *ZEROS
614600011019     C     FLGVRN        ANDEQ     '1'
614700011019     C     V6CSV1        ANDEQ     'N'
614800011019     C     V6CVA1        ANDEQ     88888888
614900050317     C  N32              UPDATE    LRA2S06
615000011019     C                   ITER
615100011019     C                   ENDIF
615200030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
615300030624      * non devo convertire la varia '&' 88888888
615400030624     C     VIDIMV        IFEQ      *ZEROS
615500030624     C     V6CSV1        ANDEQ     '&'
615600030624     C     V6CVA1        ANDEQ     88888888
615700050317     C  N32              UPDATE    LRA2S06
615800030624     C                   ITER
615900030624     C                   ENDIF
616000011019      *
616100990914     C                   CLEAR                   DSYEUR
616200990914     C                   MOVEL     WDIV          YECDVI
616300990928     C                   Z-ADD     V6CVA1        YECIMI
616400990914     C                   MOVEL     VIDDIV        YECDVO
616500990928     C     O17FDC        IFEQ      'S'
616600990914     C                   MOVEL     '3'           YECDCO
616700990928     C                   ELSE
616800990928     C                   CLEAR                   YECDCO
616900990928     C                   ENDIF
617000990914     C                   CALL      'YEURCO'
617100990914     C                   PARM                    DSYEUR
617200990914     C                   Z-ADD     YECIMO        V6CVA1
617300991013     C                   SETON                                        53
617400990914    3C                   ENDIF
617500990914     C                   Z-ADD     V6CVA1        VARVA1
617600050317     C  N32              UPDATE    LRA2S06
617700990914    2C                   ENDDO
617800990915     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
617900990915     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
618000990915     C     VIDIMV        IFGT      *ZEROS
618100990915     C     WDIV          ANDNE     *BLANKS
618200990915     C                   MOVEL     'T'           D17FIM
618300990915     C                   MOVEL     *ZEROS        D17IMV
618400990927     C                   Z-ADD     VIDPOR        D17POR
618500991014     C                   CLEAR                   C
618600991014    1C                   DO        20            NRR
618700050317     C     NRR           CHAIN     LRA2S06                            32
618800991014    2C     *IN32         IFEQ      *OFF
618900991014     C                   ADD       1             C
619000991014     C                   MOVEL     V6CSV1        VSV(C)
619100991014     C                   MOVEL     V6CVA1        VVA(C)
619200991014    2C                   ENDIF
619300991014    1C                   ENDDO
619400990915     C                   CALL      'FNLV16R'
619500990915     C                   PARM                    DSLV16
619600990915     C                   PARM                    DTASV
619700990915     C                   Z-ADD     O17IMV        VIDIMV
619800990915     C                   ENDIF
619900990914     C                   Z-ADD     VIDPOR        VARPOR
620000990914     C                   MOVEL     VIDDIV        WDIV
620100991013     C                   SETON                                        90
620200991013     C                   GOTO      ENDCT6
620300990929   X1C                   ELSE
620400990929     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
620500990929     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
620600990929     C                   Z-ADD     VIDPOR        VARPOR
620700990929    2C                   DO        20            NRR
620800050317     C     NRR           CHAIN     LRA2S06                            32
620900990929     C                   Z-ADD     V6CVA1        VARVA1
621000050317     C  N32              UPDATE    LRA2S06
621100990929    2C                   ENDDO
621200990914    1C                   ENDIF
621300020222
621400011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
621500011018     C                   SETOFF                                       47
621600011018    1C     FLGVRN        IFEQ      '1'
621700011008    2C                   DO        20            NRR
621800050317     C     NRR           CHAIN     LRA2S06                            32
621900011008    3C     *IN32         IFEQ      *OFF
622000011018     C                   SETON                                        47
622100011018    4C     V6CSV1        IFEQ      'N'
622200011018     C                   SETOFF                                       47
622300011008     C                   LEAVE
622400011018    4C                   ENDIF
622500011008    3C                   ENDIF
622600011008    2C                   ENDDO
622700011018    2C     *IN47         IFEQ      '1'
622800050317     C     1             CHAIN     LRA2S06                            32
622900050317     C  N32              UPDATE    LRA2S06
623000011008     C                   MOVEL     MSG(53)       VIDMSG
623100011106     C                   SETON                                        90
623200011008     C                   GOTO      ENDCT6
623300011008    2C                   ENDIF
623400011008    1C                   ENDIF
623500011127      *
623600011127      * Controllo che la somma degli importi immessi non superi il valore
623700011127      * impostato in §GEIMF
623800011127      *      prima controllo il totale imponibile
623900011127  1b c                   if        vidimv <> 0
624000011127     c                   eval      w0173 = vidimv
624100011127      *      controllo se devo convertire il totale imponibile
624200011127  2b c                   if        viddiv <> §gedcn
624300011127     c                   clear                   dsyeur
624400011127     c                   eval      yecdvi = viddiv
624500011127     c                   eval      yecdvo = §gedcn
624600011127     c                   eval      yecimi = vidimv
624700011127     c                   move      decsav        yecdco
624800011127     c                   call      'YEURCO'
624900011127     c                   parm                    dsyeur
625000011127  3b c                   if        yecesi = ' '
625100011127     c                   eval      w0173 = yecimo
625200011127  3e c                   endif
625300011127  2e c                   endif
625400011127      *      controllo se l'imponibile supera il limite fatturabile
625500011220  2b c                   if        w0173 > sav_geimf
625600011127     c                   eval      vidmsg = msg(55)
625700011220     c                   eval      %subst (vidmsg:54:14)
625800011220     c                              = (%editc(sav_geimf: '4'))
625900011203     c                   eval      %subst (vidmsg:69:3) = §gedcn
626000011127     c                   eval      *in49 = *on
626100011127     c                   eval      *in90 = *on
626200011127     c                   goto      endct6
626300011127  2e c                   endif
626400011127  1x c                   else
626500011127      *      altrimenti controllo tutti gli altri importi
626600011127     c                   eval      tot_importi = vidpor
626700011127      *      leggo le varie
626800011127  2b c                   do        20            nrr
626900050317     c     nrr           chain     LRA2s06                            32
627000011127  3b c                   if        *in32 = *off and v6cva1 <> 0
627100011127  4b c                   if        flgvrn = '1' and v6cva1 = 88888888
627200011127     c                             and v6csv1 = 'N'
627300011127     c                   iter
627400011127  4e c                   endif
627500030624  4b c                   if        v6cva1 = 88888888
627600030624     c                             and v6csv1 = '&'
627700030624     c                   iter
627800030624  4e c                   endif
627900011127     c                   eval      tot_importi = tot_importi + v6cva1
628000011127  3e c                   endif
628100011127  2e c                   enddo
628200011127     c                   eval      w0173 = tot_importi
628300011127      *      controllo se devo convertire il totale importi
628400011127  2b c                   if        viddiv <> §gedcn
628500011127     c                   clear                   dsyeur
628600011127     c                   eval      yecdvi = viddiv
628700011127     c                   eval      yecdvo = §gedcn
628800011127     c                   eval      yecimi = tot_importi
628900011127     c                   move      decsav        yecdco
629000011127     c                   call      'YEURCO'
629100011127     c                   parm                    dsyeur
629200011127  3b c                   if        yecesi = ' '
629300011127     c                   eval      w0173 = yecimo
629400011127  3e c                   endif
629500011127  2e c                   endif
629600011127      *      controllo se l'imponibile supera il limite fatturabile
629700011220  2b c                   if        w0173 > sav_geimf
629800011127     c                   eval      vidmsg = msg(55)
629900011220     c                   eval      %subst (vidmsg:54:14)
630000011220     c                             = (%editc(sav_geimf: '4'))
630100011203     c                   eval      %subst (vidmsg:69:3) = §gedcn
630200011127     c                   eval      *in49 = *on
630300011127     c                   eval      *in90 = *on
630400011127     c                   goto      endct6
630500011127  2e c                   endif
630600011127  1e c                   endif
630700970605     C*
630800941217     C     ENDCT6        ENDSR
630900911028     C*
631000911028     C*--- CONTROLLI CAMPI TASSAZIONE ESTENSIONE BOLLA ---------------*
631100911028     C     CONTR7        BEGSR
631200911030     C                   SETOFF                                       90
631300040121     C**!!!              MOVEL     ' '           CEISV3            1
631400920319     C*
631500040119     C**!!!VI2VA1        IFEQ      0
631600040119     C**!!!              MOVEL     MSG(30)       VIDMSG
631700040119     C**!!!              SETON                                        749028
631800040119     C**!!!              GOTO      ENDCT7
631900040119     C**!!!              END
632000990923     C* Se divisa sprotetta significa che manca tariffa: in questo caso
632100990923     C* controllo che la divisa inserita sia = alla divisa della moneta
632200990923     C*  di conto SE CLIENTE 9999
632300990923     C     *IN07         IFEQ      *OFF
632400990923     C     TSZKSC        ANDGT     0
632500990923     C     VIDKSC        ANDEQ     9999
632600990929     C     VI2DIV        ANDNE     §GEDCN
632700990929     C                   SETON                                        489028
632800990923     C                   MOVEA     MSG(47)       K78
632900990928     C                   MOVEA     §GEDCN        K78(64)
633000990923     C                   MOVEA     K78           VIDMSG
633100990923     C                   GOTO      ENDCT7
633200990923     C                   ENDIF
633300990915     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
633400990915     C                   CLEAR                   DSLV16
633500100119     C                   CLEAR                   DLV1601
633600990915     C                   CLEAR                   DTASV
633700990915     C                   MOVEL     'A'           D17TBO
633800990915     C                   MOVEL     VIDKLP        D17KSC
633900990915     C                   MOVE      VIDKSC        D17KSC
634000991014     C     *IN01         IFEQ      *OFF
634100991014     C     VIDKSC        ANDNE     9999
634200991014     C     TSZKSC        ORGT      0
634300991014     C     VIDKSC        ANDEQ     9999
634400991014     C                   Z-ADD     DATEU         D17DSP
634500991014     C                   ELSE
634600991014     C                   Z-ADD     ARBDSP        D17DSP
634700991014     C                   END
634800050714     C****               MOVEL     ARBLNP        D17LNP
634900050714     C                   MOVEL     VUNICOLNP     D17LNP
635000990915     C                   MOVEL     ARBLNA        D17LNA
635100990915     C                   MOVEL     §3ATB2        D17TBL
635200990915     C                   MOVEL     §3ARBL        D17RBL
635300990915     C                   MOVEL     VI2DIV        D17DIV
635400990915     C                   MOVEL     WDIV          D17DVP
635500990928    1C     §3AFCA        IFEQ      2
635600990915     C                   MOVEL     'S'           D17FCA
635700990915    1C                   ENDIF
635800990915     C                   MOVEL     'E'           D17FIM
635900040123     c                   Movel     §3asva        §d17Sva
636000040119      * passo che devo controllare la seconda bolla
636100040119     c                   Eval      §d17Tb2 = 'S'
636200100120     c
636300100119     c* Imposto il codice iso del destinatario e il flag di controllo in base all'esenz.IVA
636400100120     c                   if        not *in01
636500100119     c                   Eval      §d17ctriso='S'
636600100119     c                   Eval      §d17iso=vidisd
636700100119
636800100119     c                   if        vidpid<>*blanks and vidisd=*blanks
636900100119     c                   eval      §d17iso='IT'
637000100119     c                   endif
637100100120     c                   endif
637200100120     c
637300040119     c                   Movel     dlv1601       D17Flo
637400990915     C                   MOVEL     VI2CEI        D17CEI
637500990915     C                   MOVEL     VI2SV1        VSV(1)
637600040119     C**!!!              MOVEL     VI2VA1        VVA(1)
637700040119     c                   Z-add     Vi2Va1        vva(1)
637800040121     c                   Movel     Vi2Es1        ves(1)
637900040119     c                   Movel     Vi2Sv2        vsv(2)
638000040121     c                   Movel     Vi2Es2        ves(2)
638100040119     c                   Z-add     Vi2Va2        vva(2)
638200040119     c                   MOVEL     Vi2Sv3        vsv(3)
638300040119     c                   Z-add     Vi2Va3        vva(3)
638400040121     c                   Movel     Vi2Es3        ves(3)
638500040119     c                   MOVEL     Vi2Sv4        vsv(4)
638600040119     c                   Z-add     Vi2Va4        vva(4)
638700040121     c                   Movel     Vi2Es4        ves(4)
638800041129     c                   MOVEL     Vi2Sv5        vsv(5)
638900041129     c                   Z-add     Vi2Va5        vva(5)
639000041129     c                   Movel     Vi2Es5        ves(5)
639100041129     c                   MOVEL     Vi2Sv6        vsv(6)
639200041129     c                   Z-add     Vi2Va6        vva(6)
639300041129     c                   Movel     Vi2Es6        ves(6)
639400990915     C                   CALL      'FNLV16R'
639500990915     C                   PARM                    DSLV16
639600990915     C                   PARM                    DTASV
639700040119
639800990915     C                   MOVEL     D17CEI        VI2CEI
639900990915     C                   MOVEL     O17DEI        DE2CEI
640000991011     C                   MOVEL     D17DIV        VI2DIV
640100040119     C**!!!              MOVEL     VSV(1)        VI2SV1
640200040121     C**!!!              MOVEL     VES(1)        CEISV3
640300040119      * Ricarico le varie
640400040119     c                   Movel     Vsv(1)        Vi2Sv1
640500040119     c                   Z-add     Vva(1)        Vi2Va1
640600040121     c                   Movel     Ves(1)        Vi2Es1
640700040119     c                   Movel     Vsv(2)        Vi2Sv2
640800040119     c                   Z-add     Vva(2)        Vi2Va2
640900040121     c                   Movel     Ves(2)        Vi2Es2
641000040119     c                   Movel     Vsv(3)        Vi2Sv3
641100040119     c                   Z-add     Vva(3)        Vi2Va3
641200040121     c                   Movel     Ves(3)        Vi2Es3
641300040119     c                   Movel     Vsv(4)        Vi2Sv4
641400040119     c                   Z-add     Vva(4)        Vi2Va4
641500040121     c                   Movel     Ves(4)        Vi2Es4
641600041129     c                   Movel     Vsv(5)        Vi2Sv5
641700041129     c                   Z-add     Vva(5)        Vi2Va5
641800041129     c                   Movel     Ves(5)        Vi2Es5
641900041129     c                   Movel     Vsv(6)        Vi2Sv6
642000041129     c                   Z-add     Vva(6)        Vi2Va6
642100041129     c                   Movel     Ves(6)        Vi2Es6
642200040119      * Controllo se caricata almeno una varia
642300041129do  1c                   Do        6             c
642400040122     c                   If        vsv(c) <> *Blanks
642500040119     c                   Eval      wesvar1 = 'S'
642600040119     c                   EndIf
642700040119    1c                   EndDo
642800040119
642900990915     C     O17ERR        IFNE      *BLANKS
643000990915     C                   SETON                                        90
643100990915     C     O17MSG        IFNE      *BLANKS
643200990915     C                   MOVEL     O17MSG        VIDMSG
643300990915     C                   SETON                                        28
643400990915     C                   ENDIF
643500990915     C     O17ERR        IFEQ      '6'
643600040119     c                   Select
643700040119     c                   When      O17Erv = 1
643800990915     C                   SETON                                        72
643900040119     c                   When      O17Erv = 2
644000040119     C                   SETON                                        71
644100040119     c                   When      O17Erv = 3
644200040119     C                   SETON                                        76
644300040119     c                   When      O17Erv = 4
644400040119     C                   SETON                                        78
644500041129     c                   When      O17Erv = 5
644600041129     C                   SETON                                        80
644700041129     c                   When      O17Erv = 6
644800041129     C                   SETON                                        82
644900040119     c                   EndSl
645000990915     C                   ENDIF
645100990915     C     O17ERR        IFEQ      '1'
645200990915     C                   SETON                                        75
645300990915     C                   ENDIF
645400990916     C     O17ERR        IFEQ      '7'
645500990916     C                   SETON                                        48
645600990916     C                   ENDIF
645700990916     C   90              GOTO      ENDCT7
645800990915     C                   ENDIF
645900990916     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
646000990916    1C  N07VI2DIV        IFNE      WDIV
646100990916     C* CONVERSIONE VARIA
646200990916    3C     VI2VA1        IFEQ      VA2VA1
646300990916     C     VI2VA1        ANDGT     *ZEROS
646400990928     C     WDIV          ANDNE     *BLANKS
646500990916     C                   CLEAR                   DSYEUR
646600990916     C                   MOVEL     WDIV          YECDVI
646700990916     C                   Z-ADD     VI2VA1        YECIMI
646800990916     C                   MOVEL     VI2DIV        YECDVO
646900990928     C     O17FDC        IFEQ      'S'
647000990916     C                   MOVEL     '3'           YECDCO
647100990928     C                   ELSE
647200990928     C                   CLEAR                   YECDCO
647300990928     C                   ENDIF
647400990916     C                   CALL      'YEURCO'
647500990916     C                   PARM                    DSYEUR
647600990916     C                   Z-ADD     YECIMO        VI2VA1
647700990916     C                   SETON                                        7490
647800990916    3C                   ENDIF
647900040119     C**!!!              Z-ADD     VI2VA1        VA2VA1
648000040119     C**!!!              Z-ADD     VI2VA1        VVA(1)
648100040119     C**!!!              MOVEL     VI2DIV        WDIV
648200990916     C   90              GOTO      ENDCT7
648300040119      * Conversione Varia 2
648400040119    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
648500040119     c                             wdiv <> *Blanks
648600040119     c                   Clear                   DSYEUR
648700040119     c                   Movel     wdiv          YECDVI
648800040119     c                   Z-add     Vi2Va2        YECIMI
648900040119     c                   Movel     Vi2Div        YECDVO
649000040119     c                   If        O17Fdc = 'S'
649100040119     c                   Movel     '3'           YECDCO
649200040119     c                   EndIf
649300040119     c                   Call      'YEURCO'
649400040119     c                   Parm                    DSYEUR
649500040119     c                   Z-add     YECIMO        Vi2Va2
649600040119     c                   Eval      *In90 = *On
649700040119     c                   Eval      *In73 = *On
649800040119    2c                   EndIf
649900040119     c   90              Goto      ENDCT7
650000040119      * Conversione Varia 3
650100040119    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
650200040119     c                             wdiv <> *Blanks
650300040119     c                   Clear                   DSYEUR
650400040119     c                   Movel     wdiv          YECDVI
650500040119     c                   Z-add     Vi2Va3        YECIMI
650600040119     c                   Movel     Vi2Div        YECDVO
650700040119     c                   If        O17Fdc = 'S'
650800040119     c                   Movel     '3'           YECDCO
650900040119     c                   EndIf
651000040119     c                   Call      'YEURCO'
651100040119     c                   Parm                    DSYEUR
651200040119     c                   Z-add     YECIMO        Vi2Va3
651300040119     c                   Eval      *In90 = *On
651400040119     c                   Eval      *In77 = *On
651500040119    2c                   EndIf
651600040119     c   90              Goto      ENDCT7
651700040119      * Conversione Varia 4
651800040119    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
651900040119     c                             wdiv <> *Blanks
652000040119     c                   Clear                   DSYEUR
652100040119     c                   Movel     wdiv          YECDVI
652200040119     c                   Z-add     Vi2Va4        YECIMI
652300040119     c                   Movel     Vi2Div        YECDVO
652400040119     c                   If        O17Fdc = 'S'
652500040119     c                   Movel     '3'           YECDCO
652600040119     c                   EndIf
652700040119     c                   Call      'YEURCO'
652800040119     c                   Parm                    DSYEUR
652900040119     c                   Z-add     YECIMO        Vi2Va4
653000040119     c                   Eval      *In90 = *On
653100040119     c                   Eval      *In79 = *On
653200040119    2c                   EndIf
653300041129     c   90              Goto      ENDCT7
653400041129      * Conversione Varia 5
653500041129    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
653600041129     c                             wdiv <> *Blanks
653700041129     c                   Clear                   DSYEUR
653800041129     c                   Movel     wdiv          YECDVI
653900041129     c                   Z-add     Vi2Va5        YECIMI
654000041129     c                   Movel     Vi2Div        YECDVO
654100041129     c                   If        O17Fdc = 'S'
654200041129     c                   Movel     '3'           YECDCO
654300041129     c                   EndIf
654400041129     c                   Call      'YEURCO'
654500041129     c                   Parm                    DSYEUR
654600041129     c                   Z-add     YECIMO        Vi2Va5
654700041129     c                   Eval      *In90 = *On
654800041129     c                   Eval      *In81 = *On
654900041129    2c                   EndIf
655000041129     c   90              Goto      ENDCT7
655100041129      * Conversione Varia 6
655200041129    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
655300041129     c                             wdiv <> *Blanks
655400041129     c                   Clear                   DSYEUR
655500041129     c                   Movel     wdiv          YECDVI
655600041129     c                   Z-add     Vi2Va6        YECIMI
655700041129     c                   Movel     Vi2Div        YECDVO
655800041129     c                   If        O17Fdc = 'S'
655900041129     c                   Movel     '3'           YECDCO
656000041129     c                   EndIf
656100041129     c                   Call      'YEURCO'
656200041129     c                   Parm                    DSYEUR
656300041129     c                   Z-add     YECIMO        Vi2Va6
656400041129     c                   Eval      *In90 = *On
656500041129     c                   Eval      *In82 = *On
656600041129    2c                   EndIf
656700041129     c   90              Goto      ENDCT7
656800990916    1C                   ENDIF
656900040121
657000040121     c                   Z-add     Vi2Va1        Va2Va1
657100040121     c                   Z-add     Vi2Va2        Va2Va2
657200040121     c                   Z-add     Vi2Va3        Va2Va3
657300040121     c                   Z-add     Vi2Va4        Va2Va4
657400041129     c                   Z-add     Vi2Va5        Va2Va5
657500041129     c                   Z-add     Vi2Va6        Va2Va6
657600040121     c                   Z-add     Vi2Va1        Vva(1)
657700040121     c                   Z-add     Vi2Va2        Vva(2)
657800040121     c                   Z-add     Vi2Va3        Vva(3)
657900040121     c                   Z-add     Vi2Va4        Vva(4)
658000041129     c                   Z-add     Vi2Va5        Vva(5)
658100041129     c                   Z-add     Vi2Va6        Vva(6)
658200040119     c                   Movel     Vi2Div        Wdiv
658300040121     c   90              Goto      ENDCT7
658400040121
658500011128      * Controllo che l'importo immesso non superi il valore
658600011128      * impostato in §GEIMF
658700040119     c**!!!              eval      w0173 = vi2va1
658800040122     c                   xfoot     vva           w0173
658900011128      *      controllo se devo convertire l'importo
659000011128  2b c                   if        vi2div <> §gedcn
659100011128     c                   clear                   dsyeur
659200011128     c                   eval      yecdvi = vi2div
659300011128     c                   eval      yecdvo = §gedcn
659400040119     c**!!!              eval      yecimi = vi2va1
659500040119     c                   eval      yecimi = w0173
659600011128     c                   move      decsav        yecdco
659700011128     c                   call      'YEURCO'
659800011128     c                   parm                    dsyeur
659900011128  3b c                   if        yecesi = ' '
660000011128     c                   eval      w0173 = yecimo
660100011128  3e c                   endif
660200011128  2e c                   endif
660300011128      *      controllo se l'importo supera il limite fatturabile
660400011220  2b c                   if        w0173 > sav_geimf
660500011129     c                   eval      vidmsg = msg(56)
660600011220     c                   eval      %subst (vidmsg:45:14)
660700011220     c                              = (%editc(sav_geimf: '4'))
660800011203     c                   eval      %subst (vidmsg:60:3) = §gedcn
660900040119     c**!!!              eval      *in74 = *on
661000011128     c                   eval      *in28 = *on
661100011128     c                   eval      *in90 = *on
661200011128     c                   goto      endct7
661300011128  2e c                   endif
661400040122
661500040122     c                   Z-add     w0173         TotImv
661600940223     C*
661700941217     C     ENDCT7        ENDSR
661800911028     C*
661900911028     C*--- AGGIORNO BOLLE --------------------------------------------*
662000050310     C     AGGIORARB     BEGSR
662100941217     C                   SETOFF                                       09
662200941217     C* GIRO UDATE
662300941217     C                   MOVE      UDATE         DATAAS            2 0
662400941217     C                   TIME                    W0140            14 0
662500941217     C* UDATE IN GGMMAAAA
662600941217     C                   MOVE      W0140         WDTGIO            8 0
662700941217     C*
662800941217     C* UDATE IN AAAAMMGG
662900941217     C                   Z-ADD     WDTGIO        G02DAT
663000941217     C                   MOVEL     *BLANK        G02ERR
663100941217     C                   CALL      'XSRDA8'
663200941217     C                   PARM                    WLBDAT
663300941217     C                   MOVEL     G02INV        DATEU             8 0
663400941217     C*
663500920409     C                   Z-ADD     DATEU         ARBDTV
663600920409     C                   TIME                    ARBORV
663700941216     C                   MOVEL     KNMUS         ARBPRU
663800140120     c                   z-add     arbdtv        OLRA2DTV
663900140120     c                   z-add     arborv        OLRA2orv
664000140120     c                   movel     arbpru        OLRA2pru
664100050317     C                   MOVEL     ILRA2CVB      ARBCVB
664200990924     C                   CLEAR                   ARBFST
664300990924     C                   CLEAR                   ARBDDS
664400941217     C*
664500941217     C* E S T E N S I O N E   B O L L A
664600941217     C*
664700950203    1C   05              DO
664800990930     C* ESEGUO CLEAR CAMPI CHE PER 2A BOLLA NON DEVONO ESSERE IMPOSTATI
664900990930     C* (CAMPI DEL FILE DI STORICIZZAZIONE E TRASMISSIONE)
665000990930     C* PRIMA DI CLEARARLI LI SALVO PERCHè SONO ANCHE CAMPI DI FNARB
665100990930     C* CHE POI ANDRO' AD AGGIORNARE
665200990930     C* DEVO SALVARE ANCHE KSC E CTR PERCHE' SU ARB SONO RIFERITI ALL
665300990930     C* 1A BOLLA MENTRE SU ARBT SONO RIFERITI ALLA 2A BOLLA
665400990930     C                   Z-ADD     ARBQFT        SAVQFT
665500990930     C                   Z-ADD     ARBVMD        SAVVMD
665600990930     C                   MOVEL     ARBVAD        SAVVAD
665700990930     C                   MOVEL     ARBIAS        SAVIAS
665800990930     C                   MOVEL     ARBVAS        SAVVAS
665900990930     C                   Z-ADD     ARBKSC        KSCSAV
666000990930     C                   Z-ADD     ARBCTR        CTRSAV
666100990924     C                   CLEAR                   ARBQFT
666200990924     C                   CLEAR                   ARBVMD
666300990924     C                   CLEAR                   ARBVAD
666400990924     C                   CLEAR                   ARBIAS
666500990924     C                   CLEAR                   ARBVAS
666600941217     C* INVIO PER SEDE
666700170116     C****               MOVEL     'FIARBT0T'    SFILE             8
666800941217     C* ALLOCO OGGETTI
666900170116     C***                EXSR      ALLOC
667000170116     C***91              GOTO      ENDAGG
667100941217     C*
667200040121     C* SOLO SE FATTURA IMMEDIATA
667300040121    2C     *IN01         IFEQ      *OFF
667400040121     C                   SETON                                        32
667500040121     C* CONTROLLO SE RITASSAZIONE E SE ESISTENTE
667600040122    3C   12AR6NFT        IFNE      0
667700040121     C                   SETOFF                                       32
667800040121     c                   Z-add     Ar6Nft        wnft
667900040121     c                   Z-add     Ar6Fiv        fiv
668000040121    3C                   END
668100040121     C* LIBRO IVA
668200040121    3C   32              DO
668300040121     C                   clear                   DSBS02
668400040121     C                   clear                   DYIV
668500040121     C                   movel     'C'           T02mod
668600040121     C                   movel     KNSIF         T02sif
668700040121     C                   movel     'YIV'         T02cod
668800040121     C                   movel     C_Soc         T02ke1
668900040121     C                   movel     ARBLNA        T02ke2
669000040121     C                   call      'TIBS02R'
669100040121     C                   parm                    KPJBA
669200040121     C                   parm                    DSBS02
669300040121    4C                   IF        T02err = *blanks
669400040121     C                   movel     T02uni        DYIV
669500040126     C                   MOVEL     §IVLVF        FIV
669600040121     C                   ELSE
669700040126     C                   MOVEL     ARBLNA        FIV
669800040121    4C                   END
669900040121     C**
670000040121     C* NUMERO FATTURA
670100040121     C     KNUM          CHAIN     CNNUM000                           32
670200040121     C* ERRORE : MANCA IL NUMERATORE DELLE FATTURE
670300070222    4C     *IN32         IFEQ      *ON
670400040121     C                   MOVEL     MSG(41)       VIDMSG
670500070222    5C     §7LFFI        ifeq      'S'
670600170116     C****               EXSR      DLCSED
670700070222    5C                   ENDIF
670800040121     C**
670900040121     C                   SETON                                        9128
671000040121     C                   GOTO      ENDAGG
671100070222    4C                   ENDIF
671200040121     C*
671300040121     C                   ADD       1             NUMNUM
671400040121     C                   Z-ADD     NUMNUM        wnft
671500040121     C                   MOVE      DATEU         NUMDTP
671600040121     C                   UPDATE    CNNUM000
671700040122    3C                   ENDDO
671800040122     C* SE ERA S.F. E DIVENTA FATTURA IMMEDITATA --> SE GIA' CONSEGNATA
671900040122     C*  DIVENTA RITORNO ALL'INCASSO
672000040122    3C   12ARBDCM        IFGT      0
672100040122     C                   MOVEL     'R'           ARBICA
672200040122    3C                   ENDIF
672300040121    2C                   ENDIF
672400040121      * carico le varie in una sk di comodo
672500040122     c                   ExSr      CarWar1
672600040122     C* SE FATTURA IMMEDIATA CALCOLO IL TOTALE FATTURA:
672700040122     C* LO FACCIO ORA PERCHE' BOLLO E IVA SONO DIVENTATE VARIE
672800040122     C* E LE DEVO QUINDI CARICARE NELLA SCHIERA DI COMODO
672900040122    2C     *IN01         IFEQ      *OFF
673000040122     C                   MOVEL     VI2DIV        ARBDIV
673100040122     C                   EXSR      RIEGEI
673200040122     C                   CLEAR                   DSLV04
673300040122     C                   MOVEL     'T'           D04FIM
673400040122     C                   Z-ADD     DATEU         D04DFT
673500131001      * se c'è già la data fattura devo passarla
673600131001      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
673700131001      *  di nuovo una fattura immediata)
673800131001     c                   if        ar6dft <> 0
673900131001     c                   eval      d04dft = ar6dft
674000131001     c                   endif
674100040122     C                   MOVEL     §GEAED        D04AED
674200040122     C                   Z-ADD     §GEAVL        D04VAR
674300040122     c                   Z-add     TotImv        D04Imv
674400040122     C                   MOVEL     VI2CEI        D04CEI
674500040122     C                   MOVEL     AR9TIC        D04TIC
674600040122     C                   MOVEL     AR9VCA        D04VCA
674700040122     C                   Z-ADD     AR9CAS        D04CAS
674800040122     C                   MOVEL     '2'           D04SPE
674900040122     C                   MOVEL     §3ATB2        D04TBL
675000040122     C                   MOVEL     VI2DIV        D04DIV
675100040122     C                   MOVEL     '1'           UNO
675200040122     C*
675300040122     C                   CALL      'FNLV04R'
675400040122     C                   PARM                    DSLV04
675500040122     C                   PARM                    DTASV
675600040122     C*
675700040122     C     D04BOL        IFGT      *ZEROS
675800040122     C                   ADD       1             C
675900040122     C                   MOVEL     §$2BOL        WSV(C)
676000040122     C                   Z-ADD     D04BOL        WVA(C)
676100040122     C                   ENDIF
676200040122     C     D04IVA        IFGT      *ZEROS
676300040122     C                   ADD       1             C
676400040122     C                   MOVEL     §$2IVA        WSV(C)
676500040122     C                   Z-ADD     D04IVA        WVA(C)
676600040122     C                   ENDIF
676700040122    2C                   ENDIF
676800040122     C*
676900040122     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
677000040122    2C     C             IFGT      0
677100040122     C                   SETON                                        09
677200040122     C*
677300040122     C* INVIO PER SEDE
677400040122     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
677500170116    3C***  C             IFGT      3
677600040122     C* MI SALVO I CAMPI DELL'FIARBT
677700170116     C***                MOVEL     SOVR(1)       SOARBT
677800170116     C***                MOVEL     SALC(1)       SAARBT
677900170116     C***                MOVEL     SDLC(1)       SDARBT
678000040122     C*
678100170116     C***                MOVEL     'FIARBU0T'    SFILE
678200040122     C* ALLOCO OGGETTI
678300170116     C****               EXSR      ALLOC
678400040122     C* REIMPOSTO I CAMPI
678500170116    4C***  *IN91         IFEQ      *ON
678600170116     C***                MOVEL     SOARBT        SOVR(1)
678700170116     C***                MOVEL     SAARBT        SALC(1)
678800170116     C***                MOVEL     SDARBT        SDLC(1)
678900170116     C**   §7LFFI        ifeq      'S'
679000170116     C***                EXSR      DLCSED
679100170116    5C***                ENDIF
679200040122     C**
679300170116     C****               GOTO      ENDAGG
679400170116    4C****               ENDIF
679500170116    3C****               ENDIF
679600040122    2C                   ENDIF
679700040122     C*
679800040122     C                   MOVEL     '2'           AR6TRC
679900040122     C* SE DA STORICIZZARE ALL'ARRIVO  MEMORIZZO
680000040122     C     §7LFSA        IFEQ      'S'
680100040122     C                   EXSR      STOARB
680200040122     C                   ENDIF
680300060317     c* storicizzazione motivazione
680400060317     c     §7lmtv        ifeq      'S'
680500060317     c                   exsr      sto_mtv
680600060317     c                   endif
680700040122
680800040122     C                   MOVEL     '2'           ARBTRC
680900040122     C**
681000040122     C* INVIO VARIAZIONE --> FIARBU0T
681100040122     C**
681200040122    2C     *IN09         IFEQ      *ON
681300040122     C     C             ANDGT     3
681400040122     C                   Z-ADD     4             C
681500040122     C*
681600040122    3C     §7LFFI        IFEQ      'S'
681700040122     C                   Z-ADD     4             C
681800170116     C****               OPEN      FIARBU0T
681900040122     C*
682000040122     C* NE SCRIVO FIN TANTO CHE CE NE SONO
682100040122    4C     WSV(C)        DOWNE     ' '
682200040122     C                   MOVEL     WSV(C)        ARBSVN
682300040122     C                   Z-ADD     WVA(C)        ARBVAN
682400040122     C                   WRITE     FIARBUTT
682500040122     C                   ADD       1             C
682600040122    4C                   ENDDO
682700040122     C*
682800170116     C****               CLOSE     FIARBU0T
682900040122     C* DISALLOCO OGGETTO MEMBRO PER SEDE
683000170116     C*****              EXSR      DLCSED                                       FIARBU0T
683100040122    3C                   ENDIF
683200040122     C* REIMPOSTO I CAMPI
683300170116     C***                MOVEL     SOARBT        SOVR(1)
683400170116     C***                MOVEL     SAARBT        SALC(1)
683500170116     C***                MOVEL     SDARBT        SDLC(1)
683600040122    2C                   ENDIF
683700040122
683800040122     C                   MOVEL     'S'           ARBACA
683900040122     C                   MOVEL     VIDKLP        ARBKSC
684000040122     C                   MOVE      VIDKSC        ARBKSC
684100040122    2C     VIDKSC        IFNE      9999
684200040122     C                   MOVEL     VIDCTR        ARBCTR
684300040122     C                   ELSE
684400040122     C                   CLEAR                   ARBCTR
684500040122    2C                   ENDIF
684600040122     C                   CLEAR                   ARBPOR
684700040122     C                   Z-ADD     0             ARBALI
684800040122     C                   Z-ADD     0             ARBIFT
684900040122     C                   MOVEL     VI2DIV        ARBDIV
685000040122     C                   MOVEL     WSV(1)        ARBSV1
685100040122     C                   Z-ADD     WVA(1)        ARBVA1
685200040122     C                   MOVEL     WSV(2)        ARBSV2
685300040122     C                   Z-ADD     WVA(2)        ARBVA2
685400040122     C                   MOVEL     WSV(3)        ARBSV3
685500040122     C                   Z-ADD     WVA(3)        ARBVA3
685600040122
685700040122     C                   Z-ADD     TotImv        ARBIMV
685800040122     C                   MOVEL     VI2CEI        ARBCEI
685900040122     C                   MOVEL     AR6FIM        ARBFIM
686000040122
686100040122     C* S E G U E   F A T T U R A
686200040122    2C     *IN01         IFEQ      *ON
686300040122     C                   Z-ADD     0             ARBNFT
686400040122     C                   Z-ADD     0             ARBFIV
686500040122     C                   Z-ADD     0             ARBDFT
686600040122     C* SE SI TRATTA DI UN RITORNO ALL'INCASSO --> CHIUDO L'INCASSO
686700040122     C*   E LA BOLLA
686800040122    3C     ARBDCM        IFGT      0
686900040122     C     ARBICA        ANDEQ     'R'
687000040122     C*
687100040122     C* SE IL C/A E' INCASSATO E MANCA IL NUMERO MANDATO INTROITO
687200040122     C*  LO IMPOSTO
687300040122     C                   MOVEL     'S'           WAGG              1
687400040122     C*
687500040122     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
687600040122     C*  UN NUMERO DISTINTA ED E' CHIUSA
687700040122    4C     ARBNDC        IFGT      0
687800040122     c                   Z-Add     ArbIfp        Kfgs
687900040122     C                   Z-ADD     ARBNDC        W0050             5 0
688000040122     C     KFVV          CHAIN     FNFVV01L                           39
688100040122     C     *IN39         IFEQ      *OFF
688200040122     C     FVVFCF        ANDEQ     'S'
688300040122     C                   CLEAR                   WAGG
688400040122     C                   ENDIF
688500040122    4C                   ENDIF
688600040122     C*
688700040122    4C     WAGG          IFEQ      'S'
688800040122     C                   MOVEL     'Y'           ARBICA
688900040122     C*
689000040122    5C     ARBNDC        IFEQ      0
689100040122     C                   Z-ADD     8888888       ARBNDC
689200040122     C                   Z-ADD     ARBDCM        ARBDDC
689300080516    5C     ARBNMI        IFEQ      0
689400080516     C                   Z-ADD     8888888       ARBNMI
689500080516    5C                   ENDIF
689600080516     c                   else
689700080516     c* Se c'e' numero distinta aperto ma non ci sono incassi di C/AAA
689800080516     c*  imposto 8888888 per non creare problemi in chiusura distinta
689900080516     c                   if        arbicc<>'R'  and arbicc<>' '
690000080516     C                   Z-ADD     8888888       ARBNDC
690100080516    5C     ARBNMI        IFEQ      0
690200080516     C                   Z-ADD     8888888       ARBNMI
690300080516    5C                   ENDIF
690400080516     c                   endif
690500080516     c
690600040122    5C                   ENDIF
690700040122   X4C                   ELSE
690800040122     C                   MOVEL     'S'           ARBICA
690900040122    4C                   ENDIF
691000040122     C*
691100040122    3C                   ENDIF
691200040122
691300040122     C* F A T T U R A   I M M E D I A T A  --> TOTALE FATTURA
691400040122   x2C                   ELSE
691500040122     C* DATA FATTURA
691600040122     C                   MOVEL     FIV           ARBFIV
691700040122      * se c'è già la data fattura devo passarla
691800040122      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
691900040122      *  di nuovo una fattura immediata)
692000040122     c                   if        ar6dft <> 0
692100040122     c                   eval      arbdft = ar6dft
692200040122     c                   else
692300040122     C                   MOVEL     DATEU         ARBDFT                          DATA FATTUR
692400040122     c                   endif
692500040122     C                   Z-ADD     WNFT          ARBNFT
692600040122     C                   Z-ADD     D04ALI        ARBALI
692700040122     C                   Z-ADD     D04IFT        ARBIFT
692800040122     C                   Z-ADD     D04IMV        ARBIMV
692900040122    3C                   SELECT
693000040122     C     ARBVA1        WHENGT    0
693100040122     C     ARBSV1        ANDNE     §$2BOL
693200040122     C     ARBSV1        ANDNE     §$2IVA
693300040122     C                   ADD       D04POR        ARBVA1
693400040122     C     ARBVA2        WHENGT    0
693500040122     C     ARBSV2        ANDNE     §$2BOL
693600040122     C     ARBSV2        ANDNE     §$2IVA
693700040122     C                   ADD       D04POR        ARBVA2
693800040122     C     ARBVA3        WHENGT    0
693900040122     C                   ADD       D04POR        ARBVA3
694000040122    3C                   ENDSL
694100040122    2C                   ENDIF
694200040122     C*
694300050114     C     §7LFFI        ifeq      'S'
694400170116     C****               OPEN      FIARBT0T
694500040122     C                   WRITE     FIARBTTT
694600170116     C****               CLOSE     FIARBT0T
694700040122     C* DISALLOCO OGGETTO
694800170116     C****               EXSR      DLCSED                                       FIARBT0T
694900040122    2C                   END
695000050114     C* A G G I O R N O
695100990924     C                   Z-ADD     ARBKSC        AR6KSC
695200990924     C                   Z-ADD     ARBCTR        AR6CTR
695300990924     C                   MOVEL     ARBDIV        AR6DIV
695400990924     C                   Z-ADD     ARBPOR        AR6POR
695500990924     C                   MOVEL     ARBSV1        AR6SV1
695600990924     C                   MOVEL     ARBSV2        AR6SV2
695700990924     C                   MOVEL     ARBSV3        AR6SV3
695800990924     C                   Z-ADD     ARBVA1        AR6VA1
695900990924     C                   Z-ADD     ARBVA2        AR6VA2
696000990924     C                   Z-ADD     ARBVA3        AR6VA3
696100990924     C                   Z-ADD     ARBIMV        AR6IMV
696200990924     C                   Z-ADD     ARBALI        AR6ALI
696300990924     C                   MOVEL     ARBCEI        AR6CEI
696400990924     C                   Z-ADD     ARBIFT        AR6IFT
696500990924     C                   Z-ADD     ARBFIV        AR6FIV
696600990924     C                   Z-ADD     ARBNFT        AR6NFT
696700990924     C                   Z-ADD     ARBDFT        AR6DFT
696800050314     c*
696900050314     c* update o write di ar6/ar7  : imposto campi di comodo
697000050314     c                   eval      wesistear6=war61
697100050314     c                   eval      wesistear7=war71
697200050314     c                   eval      wtiporecar6='2'
697300050314     c                   exsr      SCRIVIar6ar7
697400950403     C*
697500990930     C* PRIMA DI AGGIORNARE FNARB RISPOSTO I CAMPI SALVATI NEI CAMPI
697600990930     C* DEL FILE
697700990930     C                   Z-ADD     SAVQFT        ARBQFT
697800990930     C                   Z-ADD     SAVVMD        ARBVMD
697900990930     C                   MOVEL     SAVVAD        ARBVAD
698000990930     C                   Z-ADD     SAVIAS        ARBIAS
698100990930     C                   MOVEL     SAVVAS        ARBVAS
698200990930     C                   Z-ADD     KSCSAV        ARBKSC
698300990930     C                   Z-ADD     CTRSAV        ARBCTR
698400990930     C                   UPDATE    FNARB000
698500131119     c***                if        ar6nft>0
698600131119     c***                exsr      sr_pda
698700131119     c***                endif
698800990428    1C                   END
698900920409     C**
699000920409     C** A S S E G N A T I   1   B O L L A
699100940223    1C  N05              DO
699200941217     C*
699300941217     C* INVIO PER SEDE
699400170116     C****               MOVEL     'FIARBT0T'    SFILE
699500941217     C* ALLOCO OGGETTI
699600170116     C****               EXSR      ALLOC
699700170116     C** 91              GOTO      ENDAGG
699800990108     C**
699900990302     C** FATTURA IMMEDIATA
700000990108     C* VEDO SE RIASSEGNARE NUMERO FATTURA
700100990108    2C     *IN01         IFEQ      *OFF
700200990108     C                   SETON                                        32
700300990302    3C   12AR6NFT        IFNE      0
700400990108     C                   SETOFF                                       32
700500990302     C                   Z-ADD     AR6NFT        WNFT
700600990302     C                   Z-ADD     AR6FIV        FIV
700700990108    3C                   END
700800990108     C*
700900990108    3C   32              DO
701000990108     C* LIBRO IVA
701100020725     C                   clear                   DSBS02
701200020725     C                   clear                   DYIV
701300020725     C                   movel     'C'           T02mod
701400020725     C                   movel     KNSIF         T02sif
701500020725     C                   movel     'YIV'         T02cod
701600020725     C                   movel     C_Soc         T02ke1
701700020725     C                   movel     ARBLNA        T02ke2
701800020725     C                   call      'TIBS02R'
701900020725     C                   parm                    KPJBA
702000020725     C                   parm                    DSBS02
702100020725    4C                   IF        T02err = *blanks
702200020725     C                   movel     T02uni        DYIV
702300990302     C                   MOVEL     §IVLVF        FIV
702400990108     C                   ELSE
702500040121     C                   MOVEL     ARBLNA        FIV
702600990108    4C                   END
702700990108     C* NUMERO FATTURA
702800990108     C     KNUM          CHAIN     CNNUM000                           32
702900990108     C* ERRORE : MANCA IL NUMERATORE DELLE FATTURE
703000070222    4C     *IN32         IFEQ      *ON
703100990108     C                   MOVEL     MSG(41)       VIDMSG
703200170116    5C***  §7LFFI        ifeq      'S'
703300170116     C***                EXSR      DLCSED
703400170116    5C***                ENDIF
703500990108     C*
703600990108     C                   SETON                                        9128
703700990108     C                   GOTO      ENDAGG
703800070222    4C                   ENDIF
703900990108     C*
704000990114     C     *LIKE         DEFINE    ARBNFV        WNFT
704100990108     C                   ADD       1             NUMNUM
704200990114     C                   Z-ADD     NUMNUM        WNFT
704300990108     C                   MOVE      DATEU         NUMDTP
704400990108     C                   UPDATE    CNNUM000
704500990108    3C                   END
704600990108    C**
704700990108     C* SE ERA S.F. E DIVENTA FATTURA IMMEDITATA --> SE GIA' CONSEGNATA
704800990108     C*  DIVENTA RITORNO ALL'INCASSO
704900990108    3C   12ARBDCM        IFGT      0
705000990108     C                   MOVEL     'R'           ARBICA
705100990108    3C                   END
705200990108    2C                   END
705300941217     C*
705400941217     C* CARICO LE VARIE IMPOSTATE IN UNA SCHIERA DI COMODO
705500991013     C                   MOVEL     *BLANKS       WCKINL            1
705600991013     C                   EXSR      CARWAR
705700990924     C* SE FATTURA IMMEDIATA CALCOLO IL TOTALE FATTURA:
705800990924     C* LO FACCIO ORA PERCHE' BOLLO E IVA SONO DIVENTATE VARIE
705900990924     C* E LE DEVO QUINDI CARICARE NELLA SCHIERA DI COMODO
706000990924    2C     *IN01         IFEQ      *OFF
706100990924     C                   MOVEL     VIDDIV        ARBDIV
706200990924     C                   EXSR      RIEGEI
706300990924     C                   CLEAR                   DSLV04
706400990924     C                   MOVEL     'T'           D04FIM
706500990924     C                   Z-ADD     DATEU         D04DFT
706600131001      * se c'è già la data fattura devo passarla
706700131001      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
706800131001      *  di nuovo una fattura immediata)
706900131001     c                   if        ar6dft <> 0
707000131001     c                   eval      d04dft = ar6dft
707100131001     c                   endif
707200990924     C                   MOVEL     §GEAED        D04AED
707300990924     C                   Z-ADD     §GEAVL        D04VAR
707400990924     C                   Z-ADD     VIDIMV        D04IMV
707500990924     C                   MOVEL     VIDCEI        D04CEI
707600990924     C                   MOVEL     AR9TIC        D04TIC
707700990924     C                   MOVEL     AR9VCA        D04VCA
707800990924     C                   Z-ADD     AR9CAS        D04CAS
707900990924     C                   MOVEL     '1'           D04SPE
708000990924     C                   MOVEL     §3ATB1        D04TBL
708100990924     C                   MOVEL     VIDDIV        D04DIV
708200990924     C                   Z-ADD     VIDPOR        D04POR
708300990924     C                   MOVEL     '1'           UNO               1
708400990924     C*
708500990924     C                   CALL      'FNLV04R'
708600990924     C                   PARM                    DSLV04
708700990924     C                   PARM                    DTASV
708800990924     C*
708900990924     C     D04BOL        IFGT      *ZEROS
709000990924     C                   ADD       1             C
709100990924     C                   MOVEL     §$2BOL        WSV(C)
709200990924     C                   Z-ADD     D04BOL        WVA(C)
709300991001     C                   ENDIF
709400990924     C     D04IVA        IFGT      *ZEROS
709500990924     C                   ADD       1             C
709600990924     C                   MOVEL     §$2IVA        WSV(C)
709700990924     C                   Z-ADD     D04IVA        WVA(C)
709800990924     C                   ENDIF
709900990924    2C                   ENDIF
710000941217     C*
710100941217     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
710200990108    2C     C             IFGT      0
710300941217     C                   SETON                                        09
710400941217     C*
710500941217     C* INVIO PER SEDE
710600941217     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
710700170116    3C**   C             IFGT      3
710800990913     C* MI SALVO I CAMPI DELL'FIARBT
710900170116     C***                MOVEL     SOVR(1)       SOARBT
711000170116     C***                MOVEL     SALC(1)       SAARBT
711100170116     C***                MOVEL     SDLC(1)       SDARBT
711200941217     C*
711300170116     C***                MOVEL     'FIARBU0T'    SFILE
711400941217     C* ALLOCO OGGETTI
711500170116     C***                EXSR      ALLOC
711600990108     C* REIMPOSTO I CAMPI
711700170116    4C**   *IN91         IFEQ      *ON
711800170116     C**                 MOVEL     SOARBT        SOVR(1)
711900170116     C**                 MOVEL     SAARBT        SALC(1)
712000170116     C**                 MOVEL     SDARBT        SDLC(1)
712100170116     C**   §7LFFI        ifeq      'S'
712200170116     C**                 EXSR      DLCSED
712300170116    5C**                 ENDIF
712400990108     C**
712500170116     C**                 GOTO      ENDAGG
712600170116    4C**                 ENDIF
712700170116    3C**                 ENDIF
712800990108    2C                   ENDIF
712900941217     C*
713000990924     C                   MOVEL     '1'           AR6TRC
713100941217     C* SE DA STORICIZZARE ALL'ARRIVO  MEMORIZZO
713200990108    2C     §7LFSA        IFEQ      'S'
713300941217     C                   EXSR      STOARB
713400990108    2C                   ENDIF
713500060317     c*
713600060317     c* storicizzazione motivazione
713700060317     c     §7lmtv        ifeq      'S'
713800060317     c                   exsr      sto_mtv
713900060317     c                   endif
714000060317     c*
714100990924     C                   MOVEL     '1'           ARBTRC
714200990924     C*
714300990108     C**
714400990913     C* INVIO VARIAZIONE --> FIARBU0T
714500941217     C**
714600990108    2C     *IN09         IFEQ      *ON
714700941217     C     C             ANDGT     3
714800941217     C                   Z-ADD     4             C
714900941217     C*
715000990108    3C     §7LFFI        IFEQ      'S'
715100941217     C                   Z-ADD     4             C
715200170116     C***                OPEN      FIARBU0T
715300941217     C*
715400941217     C* NE SCRIVO FIN TANTO CHE CE NE SONO
715500990108    4C     WSV(C)        DOWNE     ' '
715600941217     C                   MOVEL     WSV(C)        ARBSVN
715700941217     C                   Z-ADD     WVA(C)        ARBVAN
715800990913     C                   WRITE     FIARBUTT
715900941217     C                   ADD       1             C
716000990108    4C                   ENDDO
716100941217     C*
716200170116     C***                CLOSE     FIARBU0T
716300941217     C* DISALLOCO OGGETTO MEMBRO PER SEDE
716400170116     C***                EXSR      DLCSED                                       FIARBU0T
716500990108    3C                   ENDIF
716600941217     C* REIMPOSTO I CAMPI
716700170116     C***                MOVEL     SOARBT        SOVR(1)
716800170116     C***                MOVEL     SAARBT        SALC(1)
716900170116     C***                MOVEL     SDARBT        SDLC(1)
717000990108    2C                   ENDIF
717100941217     C*
717200941217     C                   MOVEL     'S'           ARBACA
717300941217     C                   Z-ADD     VIDKSC        ARBKSC
717400941217     C                   MOVEL     VIDKLP        ARBKSC
717500011113     C                   Z-ADD     VIDIAS        ARBIAS
717600991025     C                   MOVEL     VIDVAS        ARBVAS
717700011015      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
717800011015     C     VIDIAS        IFEQ      *ZEROS
717900011015     C                   CLEAR                   ARBVAS
718000011015     C                   ENDIF
718100011015      *
718200941217     C                   MOVEL     VIDQFT        ARBQFT
718300941217     C     VIDKSC        IFNE      9999
718400941217     C                   MOVEL     VIDCTR        ARBCTR
718500941217     C                   ELSE
718600941217     C                   CLEAR                   ARBCTR
718700941217     C                   ENDIF
718800020930     c* se ho tassatocon il peso vld imposto iL flaG tasfpf
718900990913     C                   MOVEL     VIDDIV        ARBDIV
719000990924     C                   Z-ADD     VIDPOR        ARBPOR
719100941217     C                   MOVEL     WSV(1)        ARBSV1
719200990924     C                   Z-ADD     WVA(1)        ARBVA1
719300941217     C                   MOVEL     WSV(2)        ARBSV2
719400990924     C                   Z-ADD     WVA(2)        ARBVA2
719500941217     C                   MOVEL     WSV(3)        ARBSV3
719600990924     C                   Z-ADD     WVA(3)        ARBVA3
719700941217     C*
719800070924     c* Memorizzo il totale imponibile per la scrittura del fiar5
719900070924     c                   z-add     vidimv        Tassatoimv
720000990924     C                   Z-ADD     VIDIMV        ARBIMV
720100941217     C                   MOVEL     VIDCEI        ARBCEI
720200991014     C                   MOVEL     AR6FIM        ARBFIM
720300941217     C                   CLEAR                   ARBALI
720400941217     C                   CLEAR                   ARBIFT
720500941217     C*
720600941217     C* S E G U E   F A T T U R A
720700941217    2C     *IN01         IFEQ      *ON
720800941217     C                   Z-ADD     0             ARBFIV
720900941217     C                   Z-ADD     0             ARBDFT
721000941217     C                   Z-ADD     0             ARBNFT
721100941217     C*
721200941217     C* GLI A3 S.F. LI CONSIDERO SUBITO COME CONSEGNATI
721300941217     C*
721400941217    3C     §3ATB1        IFEQ      'A3'
721500941217     C* IMPOSTO I CAMPI DELLA CHIUSURA
721600941217     C                   MOVEL     'Y'           ARBICC
721700960320     C                   MOVEL     'S'           ARBICA
721800941217     C                   Z-ADD     DATEU         ARBDCM
721900941217     C                   MOVEL     ARBORV        ARBHMC
722000941217     C                   CLEAR                   ARBFT1
722100941217     C                   CLEAR                   ARBDT1
722200941217     C                   MOVEL     *ALL'9'       ARBNDC
722300941217     C                   Z-ADD     DATEU         ARBDDC
722400941217     C                   MOVEL     'S'           ARBFDC
722500941217     C                   MOVEL     *ALL'9'       ARBNMI
722600941217     C                   Z-ADD     DATEU         ARBDMI
722700941217     C                   MOVEL     *ALL'9'       ARBCUC
722800941217     C                   CLEAR                   ARBFBC
722900941217    3C                   ENDIF
723000950120     C*
723100950120     C* SE SI TRATTA DI UN RITORNO ALL'INCASSO --> CHIUDO L'INCASSO
723200950120     C*   E LA BOLLA
723300990428    3C   12ARBDCM        IFGT      0
723400960704     C* SE IL C/A E' INCASSATO E MANCA IL NUMERO MANDATO INTROITO
723500960704     C*  LO IMPOSTO
723600960909     C                   MOVEL     'S'           WAGG              1
723700970528     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
723800970528     C*  UN NUMERO DISTINTA ED E' CHIUSA
723900990428    4C     ARBNDC        IFGT      0
724000020911     c                   Z-Add     ArbIfp        Kfgs
724100970528     C                   Z-ADD     ARBNDC        W0050             5 0
724200020911     C     KFVV          CHAIN     FNFVV01L                           39
724300990428    5C     *IN39         IFEQ      *OFF
724400970528     C     FVVFCF        ANDEQ     'S'
724500970528     C                   CLEAR                   WAGG
724600990428    5C                   ENDIF
724700990428    4C                   ENDIF
724800960909     C*
724900990428    4C     WAGG          IFEQ      'S'
725000971023     C                   MOVEL     'Y'           ARBICA
725100990428     C     ARBNDC        IFEQ      0
725200960704     C                   Z-ADD     8888888       ARBNDC
725300080516     C     ARBNMI        IFEQ      0
725400080516     C                   Z-ADD     8888888       ARBNMI
725500080516     C                   ENDIF
725600080516     c                   else
725700080516     c* Se c'e' numero distinta aperto ma non ci sono incassi di C/Ass
725800080516     c*  imposto 8888888 per non creare problemi in chiusura distinta
725900080516     c                   if        arbicc<>'R'  and arbicc<>' '
726000080516     C                   Z-ADD     8888888       ARBNDC
726100080516     C     ARBNMI        IFEQ      0
726200080516     C                   Z-ADD     8888888       ARBNMI
726300080516     C                   ENDIF
726400080516     c                   endif
726500990428     C                   ENDIF
726600080516     c
726700990428     C     ARBDDC        IFEQ      0
726800960704     C                   Z-ADD     ARBDCM        ARBDDC
726900990428     C                   ENDIF
727000990428   X4C                   ELSE
727100971023     C* SE DISTINTA CHIUSA MA SENZA MANDATO DI INTROITO METTO S
727200971023     C                   MOVEL     'S'           ARBICA
727300990428    4C                   ENDIF
727400990428    3C                   ENDIF
727500941217     C*
727600990428   X2C                   ELSE
727700941217     C* F A T T U R A   I M M E D I A T A  --> TOTALE FATTURA
727800990924     C* DATA FATTURA
727900990924     C                   MOVEL     FIV           ARBFIV
728000011204      * se c'è già la data fattura devo passarla
728100011204      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
728200011204      *  di nuovo una fattura immediata)
728300011204     c                   if        ar6dft <> 0
728400011204     c                   eval      arbdft = ar6dft
728500011204     c                   else
728600031118      * se la data del giorno è il 31/12/2003 devo impostare 30/12/2003
728700040116     c**!!!              If        dateu = 20031231
728800040116     c**!!!              Movel     20031230      Arbdft
728900040116     c**!!!              Else
729000990924     C                   MOVEL     DATEU         ARBDFT                          DATA FATTUR
729100040116     c**!!!              EndIf
729200011204     c                   endif
729300990924     C                   Z-ADD     WNFT          ARBNFT
729400990924     C                   Z-ADD     D04ALI        ARBALI
729500990924     C                   Z-ADD     D04IFT        ARBIFT
729600990924     C                   Z-ADD     D04IMV        ARBIMV
729700990924     C*
729800990924    3C                   SELECT
729900990924     C     ARBPOR        WHENGT    0
730000990924     C                   Z-ADD     D04POR        ARBPOR
730100990924     C     ARBVA1        WHENGT    0
730200990924     C     ARBSV1        ANDNE     §$2BOL
730300990924     C     ARBSV1        ANDNE     §$2IVA
730400990924     C                   ADD       D04POR        ARBVA1
730500990924     C     ARBVA2        WHENGT    0
730600990924     C     ARBSV2        ANDNE     §$2BOL
730700990924     C     ARBSV2        ANDNE     §$2IVA
730800990924     C                   ADD       D04POR        ARBVA2
730900990924     C     ARBVA3        WHENGT    0
731000990924     C                   ADD       D04POR        ARBVA3
731100990924    3C                   ENDSL
731200990108    2C                   ENDIF
731300941217     C*
731400050114      * Aggiorno direttamente il BLP ma solo se sono stati modificati
731500040513      * l'imp.da assicurare o la qtà da fatturare
731600040513  2b c                   if        sv1_vidias <> vidias or sv1_vidvas <> vidvas
731700040513     c                             or
731800040513     c                             sv1_vidqft <> vidqft
731900040513     c                   exsr      Agg_BLP
732000040513  2e c                   endif
732100050114     C* Invio variazione a SEDE
732200990428    2C     §7LFFI        IFEQ      'S'
732300170116     C***                OPEN      FIARBT0T
732400990913     C                   WRITE     FIARBTTT
732500170116     C***                CLOSE     FIARBT0T
732600941217     C* DISALLOCO OGGETTO MEMBRO PER SEDE
732700170116     C***                EXSR      DLCSED
732800990428    2C                   END
732900941217     C**
733000941217     C* A G G I O R N O
733100941217     C**
733200990924     C                   Z-ADD     ARBKSC        AR6KSC
733300990924     C                   Z-ADD     ARBCTR        AR6CTR
733400941217     C                   MOVE      ARBALI        AR6ALI
733500941217     C                   MOVE      ARBIFT        AR6IFT
733600941217     C                   MOVE      ARBIMV        AR6IMV
733700990913     C                   MOVEL     ARBDIV        AR6DIV
733800941217     C                   MOVE      ARBPOR        AR6POR
733900941217     C                   MOVE      ARBSV1        AR6SV1
734000941217     C                   MOVE      ARBVA1        AR6VA1
734100941217     C                   MOVE      ARBSV2        AR6SV2
734200941217     C                   MOVE      ARBVA2        AR6VA2
734300941217     C                   MOVE      ARBSV3        AR6SV3
734400941217     C                   MOVE      ARBVA3        AR6VA3
734500941217     C                   MOVEL     ARBCEI        AR6CEI
734600941217     C                   MOVEL     ARBDFT        AR6DFT
734700941221     C                   MOVEL     ARBNFT        AR6NFT
734800941217     C                   MOVEL     ARBFIV        AR6FIV
734900050314     c* update o write di ar6/ar7  : imposto campi di comodo
735000050314     c                   eval      wesistear6=war6
735100050314     c                   eval      wesistear7=war7
735200050314     c                   eval      wtiporecar6='1'
735300050314     c                   exsr      SCRIVIar6ar7
735400070924     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
735500070924     c*  per la fatturazione se inserito o variato totale imponibile
735600070925     c                   if        (ar6imv>0 and ar6imv<>savimv) or
735700070925     c                             ar6imv=0
735800151118     c                   exsr      Regpesivol
735900070924     c                   endif
736000941217     C*
736100941217     C                   UPDATE    FNARB000
736200131119     c***                if        ar6nft>0
736300131119     c***                exsr      sr_pda
736400131119     c***                endif
736500940224    1C                   ENDDO
736600941217     C**
736700941217     C* AGGIORNO LE NOTE
736800941217     C**
736900090507     c                   if        wscelta<>'9'
737000941217     C                   EXSR      NOTE
737100090507     c                   endif
737200911029     C*
737300120508     c
737400120508     c                   clear                   wcdfis
737500120508     c                   clear                   wpariva
737600120508     c* Se fattura immediata reperisco codice fiscale se cli italia
737700120508    1c                   if        not *in01  and arbnzd=*blanks
737800120508     c                   move      ar6ksc        w0040
737900120508     c* Non Codificato --> cerco col pgm YCO601R
738000120508    2c                   if        w0040=9999
738100180119
738200180119     c* non lo faccio se si tratta di bolla import DPD con RSD= "vedi pacco"
738300180119   2ac                   if        lnpntw='DPD'  and
738400180119     c                             %scan('VEDI PACCO':arbrsd)>0
738500180119  x2ac                   else
738600180119     c
738700120508     c                   clear                   yco600ds
738800120508     c
738900120508     c                   eval      i600ela='R'
739000120508     c                   eval      i600aCF='SI'
739100120508     c                   eval      i600aPI='SI'
739200120508     c     *iso          movel     20000101      i600dti
739300120508     c     *iso          movel     29991231      i600dtf
739400120508     c                   eval      i600rsc=arbrsd
739500120508     c                   eval      i600ind=arbind
739600120508     c                   eval      i600cap=arbcad
739700120508     c                   eval      i600loc=arblod
739800120508     c                   eval      i600prv=arbprd
739900120508     c                   movel     yco600ds      kpjbu
740000120508     c                   call      'YCO601R'
740100120508     c                   parm                    kpjba
740200120508     c
740300120508     c                   movel     kpjbu         yco600ds
740400120508    3c                   if        o600err=*blanks
740500120508     c                   movel     o600cdfi      wcdfis
740600120508     c* non prendo la partita iva se inizia per $$
740700120508     c                   if        %subst(o600piva:1:2)<>'$$'
740800120508     c                   movel     o600piva      wpariva
740900120508    3c                   endif
741000120508    3c                   endif
741100180119     c
741200180119   2ac                   endif
741300120508     c
741400120508   x2c                   else
741500120508     c* Se codificato reperisco dall'anagrafica se c'e'
741600120508     c                   eval      comksc=ar6ksc
741700120508     c                   exsr      SR_Acoksc
741800120508    3c                   if        indcdf<>*blanks
741900120508     c                   movel     indcdf        wcdfis
742000120508     c                   endif
742100120508     c                   if        indiva<>*blanks and %subst(indiva:1:2)<>'$$'
742200120508     c                   movel     indiva        wpariva
742300120508    3c                   endif
742400120508     c
742500120508    3c                   endif
742600120508    2c                   endif
742700120508     c
742800120508     c* Chaino record Q del codice fiscale
742900120508    2c                   if        wcdfis<>*blanks
743000120508     c* chain sul record per vedere se c'e' già CDFISC del mittente
743100120508     C                   MOVEL     'Q'           WTRC
743200120508     C     KARB4         CHAIN(N)  Fiar401L                           32
743300120508     c
743400120508     c* richiamo fnlv19r per scrittura
743500120508     c                   clear                   dslv19
743600120508     c                   movel     'E'           d19agg
743700120508     C                   MOVEL     'T'           D19ftr
743800120508     C                   MOVEL     ARBAAS        D19AAS
743900120508     C                   MOVEL     ARBLNP        D19LNP
744000120508     C                   MOVEL     ARBNRS        D19NRS
744100120508     C                   MOVEL     ARBNSP        D19NSP
744200120508     C  n32              MOVEL     ar4not        D19NT1
744300120508     c                   move      wcdfis        d19nt1
744400120508     C*
744500120508     C                   MOVEL     'Q'           D19TRC
744600120508     C                   CALL      'FNLV19R'
744700120508     C                   PARM                    DSLV19
744800120508    2c                   endif
744900120508     c
745000120508     c* Chaino record P e fnarb per la partita iva
745100120508    2c                   if        wpariva<>*blanks
745200120508     C
745300120508     c* Se non ho trovato il cod fiscale verifico se c'e' già in bolla
745400120508     c                   if        wcdfis=*blANKS
745500120508     C                   MOVEL     'Q'           WTRC
745600120508     C     KARB4         CHAIN(N)  Fiar401L                           32
745700120508     c  n32              move      ar4not        wcdfis
745800120508     C                   ENDIF
745900120508     c
746000120508     c* Preparo la trasmissione a sede chiamando in modo nascosto
746100120508     c*  FNLR48R --> non lo faccio se ritassazione perchè diventa chiamata ricorsiva
746200120508     c                   if        not *in12
746300120508     C                   CLEAR                   DSLR48
746400120508     C                   CLEAR                   DSarbd
746500120508     C                   CLEAR                   DSarbk
746600120508     C                   CLEAR                   DarbT
746700120508     C                   CLEAR                   DSarbG
746800120508     c* Imposto codice fiscale e partita iva
746900120508     c                   movel     wcdfis        §bdcpi
747000120508     c                   movel     wpariva       §bdrsd
747100120508     c
747200120508     C                   MOVEL     VsfAAS        D48AAS
747300120508     C                   MOVEL     VsfLNP        D48LNP
747400120508     C                   MOVEL     VsfNRS        D48NRS
747500120508     C                   MOVEL     VsfNSP        D48NSP
747600120508     C                   MOVEL     VidFIL        D48FGS
747700120508     C                   MOVEL     ARBCBO        D48CBO
747800120508     C                   MOVEL     'FI'          D48CVB
747900120508     C                   MOVEL     'A'           D48TBO
748000120508     C                   MOVEL     'S'           D48F12
748100120508     C                   MOVEL     'E'           D48FFR
748200120508     C                   MOVEL     'D'           D48trc
748300120508     C                   MOVEL     DSLR48        KPJBU
748400120508     C                   CALL      'FNLR48R'
748500120508     C                   PARM                    KPJBA
748600120508     C                   PARM                    DSARBD
748700120508     C                   PARM                    DSARBK
748800120508     C                   PARM                    DARBT
748900120508     C                   PARM                    DSARBG
749000120508     C                   PARM                    TRUL90DS
749100120508     c                   endif
749200120508     C*
749300120508     c
749400120508     c* chain sul record per vedere se c'e' già CDFISC del mittente
749500120508     C                   MOVEL     'P'           WTRC
749600120508     C     KARB4         CHAIN     Fiar401L                           32
749700120508    3c                   if        %found(fiar401l)
749800120508     c
749900120508     c* richiamo fnlv19r per scrittura
750000120508     c                   clear                   dslv19
750100120508     c* solo filiale, in sede non  c'e' il record
750200120508     c                   movel     ' '           d19agg
750300120508     C                   MOVEL     'T'           D19ftr
750400120508     C                   MOVEL     ARBAAS        D19AAS
750500120508     C                   MOVEL     ARBLNP        D19LNP
750600120508     C                   MOVEL     ARBNRS        D19NRS
750700120508     C                   MOVEL     ARBNSP        D19NSP
750800120508     C  n32              MOVEL     ar4not        D19NT1
750900120508     c                   move      wpariva       d19nt1
751000120508     C*
751100120508     C                   MOVEL     'P'           D19TRC
751200120508     C                   CALL      'FNLV19R'
751300120508     C                   PARM                    DSLV19
751400120508    3c                   endif
751500120508     c
751600120508     c     karb2         chain     fnarb01l
751700120508     c                   if        %found(fnarb01l)
751800120508     c                   movel     wpariva       arbcpi
751900120508     c                   update    fnarb000
752000120508    2c                   endif
752100120508     c
752200120508    1c                   endif
752300940223     C*
752400990913     C     AR6NFT        IFNE      0
752500000322     C*
752600120127      * stampo se F8
752700120127i2  2c                   if        *InKh
752800130320     C****               CLEAR                   fnlsb5DS
752900941217     C*
753000941217     C* CHIAMO PGM PER STAMPA FATTURA ASSEGNATO
753100000322     C* (AD ESCLUSIONE DEI "DDT NO" NON ANCORA STAMPATI)
753200130320     C** 12              MOVEL     'V'           DB0RIS
753300130320     C**N12              MOVEL     ' '           DB0RIS
753400130320     C***                MOVEL     'A'           DB0TBO
753500130320     C***                MOVEL     ARBAAS        DB0AAS
753600130320     C***                MOVEL     ARBLNP        DB0LNP
753700130320     C***                MOVEL     ARBNRS        DB0NRS
753800130320     C***                MOVEL     ARBNSP        DB0NSP
753900130320     C****               CALL      WD90PSL
754000130320     C****               PARM                    fnlsb5DS
754100120127      * flaggo FiAR400F come stampato
754200130320     c**                 Clear                   dsBl4a
754300130320     c**                 Eval      Wtrc = 'A'
754400130320     c**   kArb4         Chain     Fiar401l
754500130320     c**                 If        %Found(Fiar401l)
754600130320     c**                 Movel     Ar4Not        dsBl4a
754700130320     c**                 Select
754800130320     c**                 When      §b4Abm = 'N' and §b4Abm = 'Y'
754900130320     c**                 Eval      §B4Abm = 'J'
755000130320     c**                 When      §b4Abm = 'C'
755100130320     c***                Eval      §B4Abm = 'Q'
755200130320     c***                When      §b4Abm = 'S'
755300130320     c***                Eval      §B4Abm = 'P'
755400130320     c***                When      §b4Abm = 'K'
755500130320     c***                Eval      §B4Abm = 'W'
755600130320     c**                 Endsl
755700130320     c***                Movel     dsbl4a        Ar4not
755800130320     c***                Update    Fiar4000
755900130320     c**                 EndIf
756000031111    2c                   EndIf
756100070312     c
756200031111
756300990428    1C                   END
756400911211     C*
756500911218     C     ENDAGG        ENDSR
756600070924     c*
756700071001     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
756800071001     C     REGPesivol    BEGSR
756900151013     c                   clear                   dar5fat
757000151013     c                   Eval      kar5trd='FAT'
757100151013     c                   setoff                                       9293
757200151013     c     kfiar5        Chain     Fiar531c
757300151013     c                   If        %Found(Fiar531c)
757400151013     c                   Movel     ar5uni        dar5fat
757500070928     c                   endif
757600070928     c* Se tassata
757700070928    1c                   if        ar6imv>0
757800070928     c* Volume
757900170420     c                   movel     arbvlf        §ar5vltas
758000170420     c                   movel     arbfvf        §ar5fvtas
758100160530     c                   if        tasvlt>0
758200170420     c                             and PesVolIMV=Tassatoimv
758300160530     c                   z-add     tasvlt        §ar5vltas
758400160530     c                   movel     tasfvt        §ar5fvtas
758500170420     c***                else
758600170420     c***                movel     arbvlf        §ar5vltas
758700170420     c***                movel     arbfvf        §ar5fvtas
758800160530     c                   endif
758900070928     c* Peso
759000151013     c                   movel     arbpkf        §ar5pktas
759100151013     c                   movel     'R'           §ar5fptas
759200070928     c
759300070928     c* Se il totale imponibile aggiornato senza arrotondamento
759400070928     c*  è = a quello memorizzato al momento del richiamo al
759500070928     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
759600070928    2c                   if        PesVolIMV=Tassatoimv  and
759700070928     c                             PesVolspc='S'
759800151013     c                   movel     PesVolPkcn    §ar5pktas
759900070928    3c                   if        arbncp=arbncl
760000151013     c                   movel     'T'           §ar5fptas
760100070928     c                   else
760200151013     c                   movel     'Z'           §ar5fptas
760300070928    3c                   endif
760400070928     c
760500070928    2c                   endif
760600151013     c                   eval      §AR5DATAS=%dec(%date())
760700151013     c                   eval      §AR5HMTAS=%dec(%time())
760800151013     c                   eval      §AR5PRTAS=knmus
760900151013     c                   eval      §AR5NJTAS=knmtd
761000070928     c*
761100070928    1c                   endif
761200070928     c
761300151013      * aggiorno Fiar5 record "FAT"
761400151013if  1c                   If        %Found(Fiar531c)
761500151013     c* Se ho eliminato la tassazione  deleto
761600151013     c                   if        ar6imv=0
761700151013     c   92              delete    fiar500s
761800151013     c   93              delete    fiar5p0s
761900151013     c                   else
762000151013     c                   Movel(p)  dar5fat       Ar5uni
762100151013     c                   eval      ar5dac=%dec(%date())
762200151013     c                   eval      ar5orc=%dec(%time())
762300151013     c   92              Update    Fiar500s
762400151013     c   93              Update    Fiar5p0s
762500151013     c                   endif
762600151013      * scrivo   Fiar5 record "FAT"
762700070928   x1c                   Else
762800151013    2c                   if        ar6imv>0
762900151013     c                   Clear                   Fiar500s
763000151013     c                   Clear                   Fiar5p0s
763100151013     c                   Eval      Ar5Aas = ArbAas
763200151013     c                   Eval      Ar5Lnp = ArbLnp
763300151013     c                   Eval      Ar5Nrs = ArbNrs
763400151013     c                   Eval      Ar5Nsp = ArbNsp
763500151013     c                   Eval      Ar5Trd = 'FAT'
763600151013     c                   Movel(p)  dar5fat       Ar5uni
763700151013     c                   eval      ar5dac=%dec(%date())
763800151013     c                   eval      ar5orc=%dec(%time())
763900151013     c                   Eval      AR5FT1='T'
764000151013     c                   Eval      AR5DT1=99999999
764100151013     c                   Eval      AR5FT2='T'
764200151013     c                   Eval      AR5DT2=99999999
764300151013     c                   Eval      AR5FT3='T'
764400151013     c                   Eval      AR5DT3=99999999
764500151013     c* Verifico dove di trova TITAS per la write di fiar5
764600151013     c                   setoff                                       9293
764700151013     c     karb2         chain     titaS30c
764800160104     c                   if        %found(titas30c)
764900151013     c   92              Write     Fiar500s
765000151013     c   93              Write     Fiar5p0s
765100160104     c                   else
765200160104     c                   Write     Fiar500s
765300160104     c                   endif
765400070924    2c                   EndIf
765500070928    1c                   EndIf
765600070924     c
765700070924     c                   ENDSR
765800070924     c*
765900050310     C*--- AGGIORNO fnblp00f -----------------------------------------*
766000050310     C     AGGIORblp     BEGSR
766100050310     C*
766200050310     C* E S T E N S I O N E   B O L L A
766300050310     C*
766400050310    1C                   if        *in05
766500050310      * Carico le varie in una sk di comodo
766600050310     c                   ExSr      CarWar1
766700050310     C* A G G I O R N O
766800050310     C                   MOVEL     VIDKLP        AR6KSC
766900050310     C                   MOVE      VIDKSC        AR6KSC
767000050310     C                   MOVEL     VIDCTR        AR6CTR
767100050310     C                   CLEAR                   AR6POR
767200050310     C                   MOVEL     VI2DIV        AR6DIV
767300050310     C                   MOVEL     WSV(1)        AR6SV1
767400050310     C                   Z-ADD     WVA(1)        AR6VA1
767500050310     C                   MOVEL     WSV(2)        AR6SV2
767600050310     C                   Z-ADD     WVA(2)        AR6VA2
767700050310     C                   MOVEL     WSV(3)        AR6SV3
767800050310     C                   Z-ADD     WVA(3)        AR6VA3
767900050310
768000050310     C                   Z-ADD     TotImv        AR6IMV
768100050310     C                   MOVEL     VI2CEI        AR6CEI
768200050310     C                   Z-ADD     0             AR6NFT
768300050310     C                   Z-ADD     0             AR6FIV
768400050310     C                   Z-ADD     0             AR6DFT
768500050310     C                   Z-ADD     0             AR6ALI
768600050310     C                   Z-ADD     0             AR6IFT
768700050310     c*
768800050310     c* update o write di ar6/ar7  : imposto campi di comodo
768900050310     c                   eval      wesistear6=war61
769000050310     c                   eval      wesistear7=war71
769100050310     c                   eval      wtiporecar6='2'
769200050310     c                   exsr      SCRIVIar6ar7
769300050310     c
769400050310     c                   unlock    fnblp00f
769500050310    1C                   ENDif
769600050310     C**
769700050310     C** A S S E G N A T I   1   B O L L A
769800050310    1C                   if        not *in05
769900050310     C*
770000050310     C* CARICO LE VARIE IMPOSTATE IN UNA SCHIERA DI COMODO
770100050310     C                   MOVEL     *BLANKS       WCKINL            1
770200050310     C                   EXSR      CARWAR
770300050310     C**
770400050310     C* A G G I O R N O
770500050310     C**
770600050310     C                   Z-ADD     VIDKSC        AR6KSC
770700050310     C                   MOVEL     VIDKLP        AR6KSC
770800050310     C                   Z-ADD     VIDIAS        ARBIAS
770900050310     C                   MOVEL     VIDVAS        ARBVAS
771000050310      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
771100050310     C     VIDIAS        IFEQ      *ZEROS
771200050310     C                   CLEAR                   ARBVAS
771300050310     C                   ENDIF
771400050310      *
771500050310     C                   MOVEL     VIDQFT        ARBQFT
771600050310     C                   MOVEL     VIDCTR        AR6CTR
771700050310     c* se ho tassato con il peso vld imposto iL flaG tasfpf
771800050310     C                   MOVEL     VIDDIV        AR6DIV
771900050310     C                   Z-ADD     VIDPOR        AR6POR
772000050310     C                   MOVEL     WSV(1)        AR6SV1
772100050310     C                   Z-ADD     WVA(1)        AR6VA1
772200050310     C                   MOVEL     WSV(2)        AR6SV2
772300050310     C                   Z-ADD     WVA(2)        AR6VA2
772400050310     C                   MOVEL     WSV(3)        AR6SV3
772500050318     C                   Z-ADD     WVA(3)        AR6VA3
772600050310     C*
772700050310     C                   Z-ADD     VIDIMV        AR6IMV
772800050310     C                   MOVEL     VIDCEI        AR6CEI
772900050310     C                   CLEAR                   AR6ALI
773000050310     C                   CLEAR                   AR6IFT
773100050310     C                   Z-ADD     0             AR6FIV
773200050310     C                   Z-ADD     0             AR6DFT
773300050310     C                   Z-ADD     0             AR6NFT
773400050310     c* update o write di ar6/ar7  : imposto campi di comodo
773500050310     c                   eval      wesistear6=war6
773600050310     c                   eval      wesistear7=war7
773700050310     c                   eval      wtiporecar6='1'
773800050310     c                   exsr      SCRIVIar6ar7
773900151013     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
774000151013     c*  per la fatturazione se inserito o variato totale imponibile
774100151013     c                   if        (ar6imv>0 and ar6imv<>savimv) or
774200151013     c                             ar6imv=0
774300151118     c                   exsr      Regpesivol
774400151013     c                   endif
774500050310     c
774600050310     c                   except    aggblpfis
774700050310    1C                   ENDIF
774800050311     C
774900050311     C                   EVAL      WAGGIOBOL='S'
775000050310     C**
775100050310     C* AGGIORNO LE NOTE
775200050310     C**
775300090507     c                   if        wscelta<>'9'
775400050310     C                   EXSR      NOTE
775500090507     c                   endif
775600050310     C*
775700050310     C                   ENDSR
775800050310     c*
775900050310     c* Scrittura comuni fira6 fiar7 per fnarb e fnblp --------------------*
776000050310     c     SCRIVIar6ar7  begsr
776100050310     C* CANCELLO LE VARIE NELL'FIAR7
776200050310    2C     Wesistear7    IFEQ      'E'
776300050310     C                   MOVEL     wtiporecar6   WTRC
776400050310     C     KARB4         SETLL     FIAR7000
776500050310     C     KARB4         READE     FIAR7000                               32
776600050310     C*
776700050310    3C     *IN32         DOWEQ     *OFF
776800050310     C                   DELETE    FIAR7000
776900050310     C     KARB4         READE     FIAR7000                               32
777000050310    3C                   ENDDO
777100050310    2C                   ENDIF
777200050310     C*
777300050310     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
777400050310    2C     WSV(4)        IFNE      ' '
777500050310     C                   Z-ADD     4             C
777600050310     C                   CLEAR                   FIAR7000
777700050310     C                   MOVEL     ARBAAS        AR7AAS
777800050310     C                   MOVEL     ARBLNP        AR7LNP
777900050310     C                   MOVEL     ARBNRS        AR7NRS
778000050310     C                   MOVEL     ARBNSP        AR7NSP
778100050310     C                   MOVEL     wtiporecar6   AR7TRC
778200050310    3C     WSV(C)        DOWNE     ' '
778300050310     C                   MOVEL     WSV(C)        AR7SVN
778400050310     C                   MOVEL     WVA(C)        AR7VAN
778500050310     C                   WRITE     FIAR7000
778600050310     C                   ADD       1             C
778700050310    3C                   ENDDO
778800050310    2C                   ENDIF
778900050310
779000050310    2C     wesistear6    IFEQ      'E'
779100050310     C                   UPDATE    FIAR6000
779200050310     C                   ELSE
779300050310     C                   CLEAR                   AR6ATB
779400050310     C                   Z-ADD     ARBAAS        AR6AAS
779500050310     C                   Z-ADD     ARBLNP        AR6LNP
779600050310     C                   Z-ADD     ARBNRS        AR6NRS
779700050310     C                   Z-ADD     ARBNSP        AR6NSP
779800050310     C                   MOVEL     wtiporecar6   AR6TRC
779900050310     c                   Clear                   Ar6Fim
780000050310     C                   WRITE     FIAR6000
780100050310    2C                   ENDIF
780200050310     c                   ENDSR
780300940223     C*
780400940223     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
780500940223     C     CTRTAB        BEGSR
780600940223     C* 90 ON - RICERCA
780700940223     C     WINTER        IFEQ      '?'
780800940223     C                   MOVEL     CODUT         §KUT
780900940223     C                   MOVEL     COD           §COD
781000991011     C                   CLEAR                   §KEY
781100940223     C                   CALL      'X§TABER'
781200940223     C                   PARM                    §KUT
781300940223     C                   PARM                    §COD
781400940223     C                   PARM                    §KEY
781500940223     C                   PARM                    §DES             30
781600940223     C                   SETON                                        90
781700940223     C*
781800940223     C                   ELSE
781900940223     C*
782000940223     C     KTAB          CHAIN     TABEL                              32
782100940223     C  N32TBLFLG        IFNE      ' '
782200940223     C                   SETON                                        32
782300940223     C                   END
782400940223     C                   END
782500940223     C                   ENDSR
782600941216      *-----------------------------------------------------***********
782700941216      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
782800941216      *-----------------------------------------------------***********
782900941216     C     CTRFGS        BEGSR
783000941216     C                   SETOFF                                       90
783100941216     C                   CLEAR                   DSLV50
783200941216     C                   MOVEL     KNMUS         D50PRU
783300941216     C                   MOVEL     VIDFIL        D50FGS
783400941216     C                   CALL      'FNLV50R'
783500941216     C                   PARM                    DSLV50
783600941216     C*
783700941216     C     D50ERR        IFNE      ' '
783800941216     C                   MOVEL     D50MSG        VIDMSG
783900941216     C                   SETON                                        499028
784000941216     C                   GOTO      ENDCTF
784100941216     C                   ENDIF
784200070314     c
784300070314     c                   EXSR      DECOFGS
784400941216     C*
784500941216     C     ENDCTF        ENDSR
784600941216      *-----------------------------------------------------***********
784700941216      * GESTIONE CODICE CLIENTE DI ATRA FILIALE
784800941216      *-----------------------------------------------------***********
784900941216     C     GESKCA        BEGSR
785000941216     C*
785100941216     C* E' PER FORZA SEGUE FATTURA
785200020417     C                   SETON                                        01
785300941217     C                   MOVEL     TAS(4)        SCETAS
785400941216     C*
785500050120     C                   EXSR      PREPA06
785600941219     C                   MOVEL     *ZEROS        VIDCTR
785700950421     C                   CLEAR                   VIDMSG
785800030507      * Recupero i dati del cliente se non è un 9999
785900030507     c                   ExSr      Sr_RecDati
786000950510     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE CLIENTE DELL'AREA
786100950510     C*  SENZA TARIFFE
786200950510     C                   MOVEL     ACORAG        DESKSC
786300950203     C*
786400950203     C* 1    B O L L A
786500950203    1C  N05              DO
786600941216     C*
786700941216     C     FORKCA        TAG
786800050317     C                   WRITE     LRA2T06                                      TESTATA
786900050317     C                   WRITE     LRA2d66                                      CODICE CLIEN
787000941219     C*
787100050317     C                   WRITE     LRA2Z06                                      PIEDI
787200950421     C*
787300950421     C     VIDMSG        IFNE      *BLANKS
787400950421     C                   SETON                                        28
787500950421     C                   ENDIF
787600941216     C*
787700050317     C*                  EXFMT     LRA2C06                                      TASSAZIONE
787800050317     C                   write     LRA2C06                                      TASSAZIONE
787900050317     C                   read(E)   fnlra2d
788000050317     C
788100050317     C* Spengo indicatori di posizionamento cursore
788200941217     C                   SETOFF                                       902841
788300941217     C                   SETOFF                                       985253
788400941217     C                   SETOFF                                       554249
788500941217     C                   SETOFF                                       505145
788600990915     C                   SETOFF                                       48
788700050317     c
788800050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
788900050317     c                   if        %error
789000050317     c                   eval      *inkl='1'
789100050317     C  n94              UNLOCK    FNARB01L
789200050317     C   94              UNLOCK    FNblp00f
789300050317     C                   UNLOCK    FIAR601L
789400050317     c                   goto      endgeskca
789500050317     c                   endif
789600941216     C* F12= RITORNO
789700950203     C     *INKL         IFEQ      *ON
789800050317     C                   UNLOCK    FIAR601L
789900950203     C                   ELSE
790000941216     C*
790100950203     C                   EXSR      CTRKCA
790200950203     C   90              GOTO      FORKCA
790300941216     C* CONTROLLO TASSAZIONE
790400941217     C                   EXSR      CONTR6
790500950121     C  NKF
790600941219     COR 90              GOTO      FORKCA
790700060317     c*
790800060317     c                   if        §7lmtv = 'S'
790900060317     c                   exsr      ges_mtv
791000060317     c                   if        olv85f12 = '1'
791100060317     c                   GoTo      Forkca
791200060317     c                   endif
791300060317     c                   endif
791400941217     C**
791500941217     C* AGGIORNAMENTO
791600941217     C**
791700050310     C  n94              EXSR      AGGIORARB
791800050310     C   94              EXSR      AGGIORblp
791900941217     C   91              GOTO      FORKCA
792000950203    2C                   ENDIF
792100950203    1C                   ENDDO
792200950203     C*
792300950203     C* E S T E N S I O N E   B O L L A
792400950203    1C   05              DO
792500950203     C*
792600950203     C     FORKC2        TAG
792700950203     C* WRITE TESTATA
792800050317     C                   WRITE     LRA2T06                                      TESTATA
792900050317     C                   WRITE     LRA2d66                                      COD CLIENTE
793000950203     C* 2 BOLLA
793100050317     C*                  EXFMT     LRA2D07                                      TASSAZIONE
793200050317     C                   write     LRA2D07                                      TASSAZIONE
793300050317     C                   read(E)   fnlra2d
793400050317     c
793500950203     c* spengo indicatori di posizionamento cursore
793600950203     C                   SETOFF                                       727475
793700950203     C                   SETOFF                                       9028
793800040119     c                   Setoff                                       717376
793900040119     c                   Setoff                                       777879
794000041129     c                   Setoff                                       808182
794100041129     c                   Setoff                                       83
794200050317     c
794300050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
794400050317     c                   if        %error
794500050317     c                   eval      *inkl='1'
794600050317     C  n94              UNLOCK    FNARB01L
794700050317     C   94              UNLOCK    FNblp00f
794800050317     C                   UNLOCK    FIAR601L
794900050317     c                   goto      endgeskca
795000050317     c                   endif
795100050120     C* F12 - RITORNO
795200950203     C     *INKL         IFEQ      *ON
795300990923     C   KL              UNLOCK    FIAR601L
795400950203     C                   ELSE
795500950203     C*
795600950203     C* ENTER - CONTROLLI
795700990913     C                   EXSR      CONTR7
795800950203     C  NKF
795900950203     COR 90              GOTO      FORKC2
796000060317     c*
796100060317     c                   if        §7lmtv = 'S'
796200060317     c                   exsr      ges_mtv
796300060317     c                   if        olv85f12 = '1'
796400060317     c                   GoTo      Forkc2
796500060317     c                   endif
796600060317     c                   endif
796700950203     C**
796800950203     C* CONFERMA TASSAZIONE AGGIORNO BOLLE
796900050310     C  n94              EXSR      AGGIORARB
797000050310     C   94              EXSR      AGGIORblp
797100950203     C   91              GOTO      FORKC2
797200950203    2C                   ENDIF
797300950203    1C                   ENDDO
797400941216     C*
797500050317     C     ENDGESKCA     ENDSR
797600941216     C*
797700950203     C* CONTROLLO CODICE TARIFFA ------------------------------------*
797800950203     C     CTRKCA        BEGSR
797900050317     C                   READ      LRA2D66                                32
798000950203     C*
798100050128     C                   CLEAR                   fnlv59ds
798200060511     C***                MOVEL     'S'           ilv59CFO
798300060511     c* non devo fare solo controllo formale, per non impostare un cod
798400060511     c*  tariffa valido per altre selezioni quindi imposto il controllo
798500060511     c*  totale
798600060511     C                   MOVEL     'A'           ilv59tbo
798700060511      * Se bolla monovaria carico tutte le tariffe ma propongo la partenza
798800060511     C                   IF        §3ASVA <> *BLANK
798900060511     C                   movel     ' '           ilv59tbo
799000060511     C                   movel     'P'           ilv59prp
799100060511     C                   ENDIF
799200060511      *
799300060511     C                   MOVEL     COMKSC        ilv59KSC
799400060511     C                   MOVEL     KCI           ilv59KCI
799500060511     C                   MOVEL     WFIE          ilv59FIE
799600060511     C                   MOVEL     ARBDSP        ilv59DSP
799700060511     C                   MOVEL     ARBTSP        ilv59TSP
799800050128     C                   MOVEL     VIDCTR        ilv59CTR
799900060511     C                   MOVEL     'S'           ilv59CON
800000050128     C                   CALL      'FNLV59R'
800100050128     C                   PARM                    fnlv59ds
800200950203     C*
800300060511     C* VEDO SE C'E' STATA LA RICERCA
800400060511     C     olv59ERR      IFEQ      'R'
800500060511     C                   MOVEL     olv59CTR      VIDCTR
800600060511     C                   SETON                                        5590
800700060511     C                   else
800800060511     C*
800900050128    3C     olv59ERR      IFNE      *BLANKS
801000050128     C                   MOVEL     olv59msg      VIDMSG
801100950203     C                   SETON                                        289055
801200950203    3C                   ENDIF
801300060511    3C                   ENDIF
801400950203     C*
801500950203     C* DESCRIZIONE CODICE TARIFFA
801600050128     C  N28              MOVEL     olv59DFS      DE2CTR
801700021121
801800021121      * Bolla FedEx e il cliente codificato non ha tariffe FedEx
801900021121If  1c                   If        O27Ntw = 'FED'
802000021121      * non posso accettare una tariffa documenti se il peso della
802100021121      * bolla superare il limite previsto in tabella "DFE"
802200021121     c                   Move      Vidctr        DueCtr
802300021121If  2c                   If        ArbPkf > §DfePkgD and
802400021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
802500021121     c                   Eval      *In55 = *On
802600021121     c                   Eval      *In90 = *On
802700021121     c                   Eval      *In28 = *On
802800021121     c                   Eval      Vidmsg = Msg(58)
802900021121     c                   Eval      %Subst(VidMsg:59:8) = (%editc(§DfePkgd: '4'))
803000021121    2c                   EndIf
803100021121    1c                   EndIf
803200021121
803300950203     C                   ENDSR
803400950203     C*
803500941216     C* CARICO LA VARIA IN SFL ---------------------------------------*
803600941216     C     CARVAR        BEGSR
803700941216     C*
803800941216     C     WSV1          IFNE      ' '
803900011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
804000011018     C     ARBFBR        IFEQ      'S'
804100011018     C     WSV1          ANDEQ     'N'
804200011018     C     WVA1          ANDEQ     88888888
804300011018     C                   MOVEL     '1'           FLGVRN            1
804400011018     C                   ENDIF
804500990909     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
804600990909     C     WSV1          LOOKUP    SVA                                    31
804700990909     C     *IN31         IFEQ      *ON
804800941216     C                   ADD       1             NRR
804900050317     C     NRR           CHAIN     LRA2S06                            32
805000941217     C                   MOVEL     WSV1          V6CSV1
805100941217     C                   Z-ADD     WVA1          V6CVA1
805200990910     C                   Z-ADD     WVA1          VARVA1
805300050317     C   32              WRITE     LRA2S06
805400050317     C  N32              UPDATE    LRA2S06
805500990909     C                   ENDIF
805600941216     C                   ENDIF
805700011003     C                   ENDSR
805800040119
805900040119      *---------------------------------------------------------------*
806000040119      * CARICO LA VARIA DELLA SECONDA BOLLA
806100040119      *---------------------------------------------------------------*
806200040119     c     CarVar1       BegSr
806300040119
806400040119if  1c                   If        wsv1 <> *Blanks
806500040119      * Carico la varia solo se fa parte del totale imponibile
806600040119     c     wsv1          Lookup    sva                                    31
806700040119if  2c                   If        *In31
806800040119     c                   add       1             ContaSv
806900040119     c                   add       wva1          TotImv
807000040119
807100040119s   3c                   Select
807200040119     c                   When      ContaSv = 1
807300040119     c                   Movel     wsv1          Vi2Sv1
807400040119     c                   Z-add     wva1          Vi2Va1
807500040119     c                   Z-add     wva1          Va2Va1
807600040119     c                   When      ContaSv = 2
807700040119     c                   Movel     wsv1          Vi2Sv2
807800040119     c                   Z-add     wva1          Vi2Va2
807900040119     c                   Z-add     wva1          Va2Va2
808000040119     c                   When      ContaSv = 3
808100040119     c                   Movel     wsv1          Vi2Sv3
808200040119     c                   Z-add     wva1          Vi2Va3
808300040119     c                   Z-add     wva1          Va2Va3
808400040119     c                   When      ContaSv = 4
808500040119     c                   Movel     wsv1          Vi2Sv4
808600040119     c                   Z-add     wva1          Vi2Va4
808700040119     c                   Z-add     wva1          Va2Va4
808800041129     c                   When      ContaSv = 5
808900041129     c                   Movel     wsv1          Vi2Sv5
809000041129     c                   Z-add     wva1          Vi2Va5
809100041129     c                   Z-add     wva1          Va2Va5
809200041129     c                   When      ContaSv = 6
809300041129     c                   Movel     wsv1          Vi2Sv6
809400041129     c                   Z-add     wva1          Vi2Va6
809500041129     c                   Z-add     wva1          Va2Va6
809600040119    3c                   EndSl
809700040119
809800040119    2c                   EndIf
809900040119
810000040119    1c                   EndIf
810100040119
810200040119     c                   EndSr
810300011003      **
810400011003      *--- TASSAZIONE VARIA 'N' --------------------------------------*
810500011003     C     TASSAN        BEGSR
810600011003      *
810700011003     C                   CLEAR                   DTAS
810800011003     C                   CLEAR                   DTASV
810900011003     C                   MOVEL     VIDKLP        TASKSC
811000011003     C                   MOVE      VIDKSC        TASKSC
811100011003     C                   MOVEL     VIDCTR        TASCTR
811200011003     C                   Z-ADD     ARBPKF        TASPKF
811300160530     C                   Z-ADD     ARBPKb        TASPKb
811400050714     C**                 MOVEL     arBLNP        TASLNP
811500050714     C                   MOVEL     VUNICOLNP     TASLNP
811600011003     C                   MOVEL     ARBCTS        TASCTS
811700011003     C                   MOVEL     ARBDSP        TASDSP
811800011003     C                   MOVEL     ARBDSP        TASDCT
811900011003     C                   MOVEL     ARBVLF        TASVLF
812000160530     C                   MOVEL     ARBfvF        TASfvf
812100160530     c                   z-add     arbvlc        tasvlc
812200160530     c                   z-add     arbncr        tasncr
812300011003     C                   MOVEL     ARBTSP        TASTSP
812400011003     C     WBOL          IFEQ      '1'
812500011003     C                   MOVEL     §3ATB1        TASTBL
812600011003     C                   ELSE
812700011003     C                   MOVEL     §3ATB2        TASTBL
812800011003     C                   ENDIF
812900011003     C                   MOVEL     ARBLNA        TASLNA
813000011003     C                   MOVEL     ARBNCL        TASNCL
813100160530     C                   MOVEL     ARBNCL        TASNCLb
813200011003     C                   MOVEL     ARBQFT        TASQFT
813300011003     C     WBOL          IFEQ      '1'
813400011003     C     §3AFCA        ANDEQ     1
813500011003     C     WBOL          OREQ      '2'
813600011003     C     §3AFCA        ANDEQ     2
813700011003     C                   MOVEL     AR9CAS        TASCAS
813800011003     C                   MOVEL     AR9VCA        TASVCA
813900011003     C                   MOVEL     AR9TIC        TASTIC
814000011003     C                   ENDIF
814100011003     C                   MOVEL     ARBIAS        TASIAS
814200011003     C                   MOVEL     ARBVAS        TASVAS
814300011003     C                   MOVEL     ARBCAD        TASCAD
814400011003     C                   MOVEL     ARBNZD        TASNZD
814500011003     C                   MOVEL     ARBFIN        TASFIN
814600050715     C                   MOVEL     Vunicocam     TASCAM
814700050715     C**                 MOVEL     ARBCAM        TASCAM
814800050714     C**                 MOVEL     ARBNZM        TASNZM
814900050714     C                   MOVEL     Vuniconzm     TASNZM
815000050715     C                   MOVEL     Vunicofap     TASFAP
815100050715     C**                 MOVEL     ARBFAP        TASFAP
815200011003     C                   MOVEL     ARBTC1        TASTC1
815300011003     C                   MOVEL     ARBTC2        TASTC2
815400011003     C     WDDT          IFEQ      'C'                                          SI DDT
815500011003     C     WDDT          OREQ      'S'
815600031028     c     wddt          oreq      'P'
815700031028     c     wddt          oreq      'Q'
815800011003     C                   MOVEL     'S'           TASDDT
815900011003     C                   ENDIF
816000060925     c*****              movel     arbfbr        tasflo
816100060925     c                   clear                   dtas01
816200060925     c                   movel     arbfbr        §asfbr
816300060925     c                   movel     arbcca        §ascca
816400060925     c                   eval      tasflo = dtas01
816500011003      **
816600011003      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
816700011003      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
816800020222     c                   if        lnantw <> 'EEX' or lnantw <> 'EUP' or
816900020222     c                             lnantw <> 'FED' or lnantw <> 'DPD'
817000060123     C***                MOVEL     ARBFDN        TASFDN
817100060123     C                   MOVEL     VunicoFDN     TASFDN
817200011003     C                   ENDIF
817300011003      **
817400011003      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
817500050715     c* Se bolla dirottata, e posso utilizzare i dirottamenti
817600050715     c*  prendo quello già calcolato
817700050715     c                   if        Vunicomct<>'  ' and
817800050715     c                             (§vpodir='A' or §vpodir='E')
817900050715     c                   movel     Vunicomct     tasmct
818000050715     c                   else
818100050715     c
818200011003     C                   CLEAR                   DSSI95
818300011003     C                   MOVEL     ARBLOM        I95LOC
818400050715     C                   MOVEL     Vunicocam     I95CAP
818500050715     C**                 MOVEL     ARBCAM        I95CAP
818600050714     C***                MOVEL     ARBNZM        I95NAR
818700050714     C                   MOVEL     vuniconzm     I95NAR
818800011003     C                   MOVEL     '3'           I95TCN
818900011003     C                   MOVEL     ARBDSP        I95DAT
819000011003     C     §3AFCA        IFNE      0
819100011003     C                   MOVEL     'S'           I95FCA
819200011003     C                   ENDIF
819300011003     C                   MOVEL     §3ATB1        I95TPO
819400020228      * Se network FedEx passo flag apposito per dire di prendere i dati
819500020228      * dalla tabella della nazione
819600160530     c     lnpntw        ifeq      'FED'
819700020228     c                   eval      i95fi1 = 'S'
819800020228     c                   endif
819900020228
820000011003     C                   CALL      'TISI95R'
820100011003     C                   PARM                    DSSI95
820200011003     C     O95ERR        IFEQ      *BLANKS
820300011003     C                   MOVEL     O95CTS        TASMCT
820400011003     C                   ENDIF
820500050715     C                   ENDIF
820600050715     C*********          MOVEL     'S'           WCAL13            1
820700011003      * ELABORO E CHIUDO CON LR IL PGM
820800011113     C                   MOVEL     ' '           TASTLA
820900011003      *
821000020930     c* imposto peso vdl e colli rilevati
821100030203     c                   z-add     arbncp        Tasncp
821200030203     c                   z-add     arbpkc        Taspkc
821300030204
821400030204      * Bancali
821500030205     c                   If        %Subst(ArbGva:1:1) = 'B'
821600030204     c                   Z-Add     VidBan        TasBan
821700030205     c                   EndIf
821800030728      * Colli originali
821900030728     c                   If        %Subst(ArbGva:1:1) = 'O'
822000030728     c                   Z-Add     VidNcl        TasNcl
822100030728     c                   EndIf
822200050804     c
822300050804     c                   eval      parca  = ' '
822400050804     c                   movel     paramt        kpjbu
822500050804      *
822600020930     C*
822700011003     C                   CALL      'TNSF20R'
822800011003     C                   PARM                    KPJBA
822900011003     C                   PARM                    DTAS
823000011003     C                   PARM                    DTASV
823100011003      *
823200011018      * Tassazione andata buon fine
823300011003     C     TASERR        IFEQ      *BLANKS
823400011018      *
823500011018      * Controllo la divisa della tassazione con la divisa del video
823600011018      * se non corrispondono devo convertire la varia 'N' nella divisa
823700011018      * del video
823800011018     C     TASDIV        IFNE      VIDDIV
823900011018      * aggancio la tabella CV per sicurezza
824000011018     C                   CLEAR                   DSCV
824100011018     C                   MOVEL     'CV'          COD
824200011018     C                   MOVEL(P)  VIDDIV        KEY
824300011018     C                   EXSR      CTRTAB
824400011018     C  N32              MOVEL     TBLUNI        DSCV
824500011018      *
824600011018     C                   CLEAR                   DSYEUR
824700011018     C                   MOVEL     TASDIV        YECDVI
824800011018     C                   MOVEL     VIDDIV        YECDVO
824900011018     C                   Z-ADD     TASIMV        YECIMI
825000011018     C     §CVFDC        IFEQ      'S'
825100011018     C                   MOVE      '3'           YECDCO
825200011018     C                   ELSE
825300011018     C                   MOVE      '0'           YECDCO
825400011018     C                   ENDIF
825500011018     C                   CALL      'YEURCO'
825600011018     C                   PARM                    DSYEUR
825700011018     C     YECESI        IFEQ      ' '
825800011018     C                   Z-ADD     YECIMO        TASIMV
825900011018     C                   ELSE
826000011018     C                   Z-ADD     88888888      TASIMV
826100011018     C                   ENDIF
826200011018     C                   ENDIF
826300011018      *
826400011010     C                   Z-ADD     TASIMV        V6CVA1
826500011010     C                   Z-ADD     TASIMV        VARVA1
826600011003     C                   ENDIF
826700011003      *
826800011003     C                   ENDSR
826900941217     C*
827000941217     C* CARICO LE NOTE -----------------------------------------------*
827100941217     C     NOTE          BEGSR
827200060217     C                   CLEAR                   DSLV19
827300060217     C                   MOVEL     'T'           D19ftr
827400941217     C                   MOVEL     ARBAAS        D19AAS
827500941217     C                   MOVEL     ARBLNP        D19LNP
827600941217     C                   MOVEL     ARBNRS        D19NRS
827700941217     C                   MOVEL     ARBNSP        D19NSP
827800050110     C                   MOVEL     VIDNT1        D19NT1
827900050110     C                   MOVEL     VIDNT2        D19NT2
828000941217     C*
828100941217    1C     D19NT1        IFNE      *BLANKS
828200941217     C     D19NT2        ORNE      *BLANKS
828300941217     C                   MOVEL     '8'           D19TRC
828400941217     C                   CALL      'FNLV19R'
828500941217     C                   PARM                    DSLV19
828600960118    1C                   ENDIF
828700960118     C* SE, CHIAMATO, NON C'E' ERRORE E LA SECONDA NOTA E' VUOTA
828800960118     C*  LA CANCELLO SE PRIMA C'ERA
828900960118    2C     D19ERR        IFEQ      *BLANKS
829000960118     C     D19NT1        ANDNE     *BLANKS
829100960118     C     D19NT2        ANDEQ     *BLANKS
829200960118     C                   MOVEL     '9'           D19TRC
829300960118     C                   CLEAR                   D19NT1
829400960118     C                   CLEAR                   D19NT2
829500960118     C                   CALL      'FNLV19R'
829600960118     C                   PARM                    DSLV19
829700960118     C*
829800960118     C                   ELSE
829900941217     C*
830000941217     C* SE TORNATO CODICE DI ERRORE  O ENTRAMBE VUOTE
830100941217     C* RICHIAMO 2 VOLTE IL PGMUNA VOLTA PER NOTA
830200960118    2C     D19ERR        IFNE      *BLANKS
830300941217     C     D19NT1        OREQ      *BLANKS
830400941217     C     D19NT2        ANDEQ     *BLANKS
830500941217     C* NOTA 1
830600941217     C                   MOVEL     '8'           D19TRC
830700941217     C                   CLEAR                   D19NT2
830800941217     C                   CALL      'FNLV19R'
830900941217     C                   PARM                    DSLV19
831000941217     C* NOTA 2
831100941217     C                   MOVEL     '9'           D19TRC
831200050110     C                   MOVEL     VIDNT2        D19NT1
831300941217     C                   CLEAR                   D19NT2
831400941217     C                   CALL      'FNLV19R'
831500941217     C                   PARM                    DSLV19
831600960118    2C                   ENDIF
831700960118    1C                   ENDIF
831800941217     C*
831900941217     C                   ENDSR
832000941217     C*
832100050114     C*--- ALLOCO OGGETTI per sede -----------------------------------*
832200170116     C**   ALLOC         BEGSR
832300170116     C**                 MOVE      ')'           VAR               7
832400170116     C**                 MOVE      '))'          VAR2              8
832500170116     C**                 Z-ADD     48            LUNG             15 5
832600941217     C*
832700170116     C**                 MOVEA     SFILE         C20(13)
832800170116     C**                 MOVEA     SFILE         C21(13)
832900170116     C**                 MOVEA     SFILE         C22(13)
833000170116     C**                 MOVEA     C22           SOVR(1)
833100170116     C**                 MOVEA     C20           SALC(1)
833200170116     C***                MOVEA     C21           SDLC(1)
833300050114     C*
833400170116    1C**   §7LFFI        IFEQ      'S'
833500941217     C* CHECK OBJ E ADDPFM
833600170116     C**                 Z-ADD     046           PARMBR
833700170116     c***                move      046           $mbr
833800170116     C***                MOVEL     SFILE         FISICO
833900170116     C**                 CALL      'TRUL50C'
834000170116     C**                 PARM                    FISICO           10
834100170116     C**                 PARM                    MBRFIS           10
834200170116     C**                 PARM                    LIB              10
834300170116     C**                 PARM                    LOGICO           10
834400170116     C**                 PARM                    MBRLOG           10
834500170116     C**                 PARM                    ULFLG             1
834600941217     C** OVRDBF
834700170116     c***                movel     $mbr          var
834800170116     C**                 MOVE      VAR           SOVR
834900170116     C**                 MOVEL     *BLANKS       COMMAN          130
835000170116     C**                 MOVEA     SOVR(1)       COMMAN
835100170116     C**                 CALL      'QCMDEXC'
835200170116     C**                 PARM                    COMMAN
835300170116     C**                 PARM                    LUNG
835400941217     C* ALLOCO OGGETTO
835500170116     c**                 movel     $mbr          var2
835600170116     C**                 MOVE      VAR2          SALC
835700170116     C**                 MOVEL     *BLANKS       COMMAN
835800170116     C**                 MOVEA     SALC(1)       COMMAN
835900170116     C**                 CALL      'QCMDEXC'                            91
836000170116     C**                 PARM                    COMMAN
836100170116     C**                 PARM                    LUNG
836200170116    1C**                 END
836300941217     C*
836400050114     C* 91 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
836500941217     C*
836600170116     C**   *IN91         IFEQ      *ON
836700170116     C**                 SETON                                        28
836800170116     C**                 MOVEL     MSG(22)       VIDMSG
836900170116     C**                 ENDIF
837000941217     C*
837100170116     C**                 ENDSR
837200941217     C*---------------------------------------------------------------
837300941217     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
837400941217     C*---------------------------------------------------------------
837500170116     C**   DLCSED        BEGSR
837600170116     c**                 move      046           $mbr
837700170116     c**                 movel     $mbr          var2
837800170116     C**                 MOVE      VAR2          SDLC(1)
837900170116     C**                 MOVEL     *BLANKS       COMMAN
838000170116     C**                 MOVEA     SDLC(1)       COMMAN
838100170116     C**                 CALL      'QCMDEXC'                            91
838200170116     C**                 PARM                    COMMAN
838300170116     C**                 PARM                    LUNG
838400170116     C**                 ENDSR
838500941217     C*---------------------------------------------------------------
838600941217     C* STORICIZZO LA TASSAZIONE
838700941217     C*---------------------------------------------------------------
838800941217     C     STOARB        BEGSR
838900990924     C                   MOVEL     AR6TRC        ARBTRC
839000990924     C                   Z-ADD     AR6KSC        ARBKSC
839100990924     C                   Z-ADD     AR6CTR        ARBCTR
839200990913     C                   MOVEL     AR6DIV        ARBDIV
839300941217     C                   MOVEL     AR6POR        ARBPOR
839400941217     C                   MOVEL     AR6SV1        ARBSV1
839500941217     C                   MOVEL     AR6VA1        ARBVA1
839600941217     C                   MOVEL     AR6SV2        ARBSV2
839700941217     C                   MOVEL     AR6VA2        ARBVA2
839800941217     C                   MOVEL     AR6SV3        ARBSV3
839900941217     C                   MOVEL     AR6VA3        ARBVA3
840000941217     C                   MOVEL     AR6IMV        ARBIMV
840100941217     C                   MOVEL     AR6ALI        ARBALI
840200941217     C                   MOVEL     AR6CEI        ARBCEI
840300941217     C                   MOVEL     AR6IFT        ARBIFT
840400941217     C                   MOVEL     AR6DFT        ARBDFT
840500941217     C                   MOVEL     AR6NFT        ARBNFT
840600941217     C                   MOVEL     AR6FIV        ARBFIV
840700991014     C                   MOVEL     AR6FIM        ARBFIM
840800990913     C                   WRITE     FIARBT00
840900040122      * 1   B O L L A
841000990913     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
841100941221    1C     WAR7          IFEQ      'E'
841200990913    1C     ARBTRC        ANDEQ     '1'
841300990913     C                   MOVEL     '1'           WTRC
841400990913     C     KARB4         SETLL     FIAR7000
841500990913     C     KARB4         READE     FIAR7000                               32
841600941217     C*
841700941220    2C     *IN32         DOWEQ     *OFF
841800941217     C                   MOVEL     AR7SVN        ARBSVN
841900941217     C                   MOVEL     AR7VAN        ARBVAN
842000990913     C                   WRITE     FIARBU00
842100941217     C*
842200990913     C     KARB4         READE     FIAR7000                               32
842300941217    2C                   ENDDO
842400941217     C                   ENDIF
842500040122      * E S T E N S I O N E   B O L L A
842600040122     C* SE HO DELLE ALTRE VARIE LE STORICIZZO NEL FILE FIARBU0F
842700040122    1C     WAR71         IFEQ      'E'
842800040122    1C     ARBTRC        ANDEQ     '2'
842900040122     C                   MOVEL     '2'           WTRC
843000040122     C     KARB4         SETLL     FIAR7000
843100040122     C     KARB4         READE     FIAR7000                               32
843200040122     C*
843300040122    2C     *IN32         DOWEQ     *OFF
843400040122     C                   MOVEL     AR7SVN        ARBSVN
843500040122     C                   MOVEL     AR7VAN        ARBVAN
843600040122     C                   WRITE     FIARBU00
843700040122     C*
843800040122     C     KARB4         READE     FIAR7000                               32
843900040122    2C                   ENDDO
844000040122    1C                   ENDIF
844100941217     C*
844200941217     C                   ENDSR
844300941217     C*
844400941217     C* CONTROLLO DIVISA ---------------------------------------------*
844500941217     C     CTRVLT        BEGSR
844600941217     C                   CLEAR                   WINTER
844700950104     C                   CLEAR                   VIDMSG
844800011011      * La divisa è obbligatoria se è inserito un importo
844900011011     C     W0133         IFGT      *ZEROS
845000011011     C     WVLT          ANDEQ     *BLANKS
845100011011     C                   MOVEL     MSG(54)       VIDMSG
845200011011     C                   SETON                                        90
845300011011     C                   GOTO      ENDVLT
845400011011     C                   ENDIF
845500941217     C*
845600941217     C                   MOVEL     'CV'          COD
845700941217     C* RICERCA
845800941217     C     '?'           SCAN      WVLT                                   90
845900941217     C     *IN90         IFEQ      *ON
846000941217     C                   MOVEL     '?'           WINTER
846100941217     C                   ELSE
846200941217     C                   MOVEL(P)  WVLT          KEY
846300941217     C                   ENDIF
846400941217     C*
846500941217     C                   EXSR      CTRTAB
846600941217     C* ERRORE
846700941217     C     WINTER        IFEQ      '?'
846800941217     C                   MOVEL     §KEY          WVLT
846900941217     C                   ELSE
847000941217     C* ERRORE
847100941217    1C     *IN32         IFEQ      *ON
847200941217     C                   MOVEL     MSG(19)       VIDMSG
847300950104     C                   SETON                                        90
847400941217     C                   GOTO      ENDVLT
847500941217    1C                   ENDIF
847600941217    1C                   ENDIF
847700941217     C*
847800941217     C                   MOVEL     TBLUNI        DSCV
847900011002      *
848000011002      * Controllo se la divisa immessa è scaduta o no
848100011002     C     ARBDSP        IFGT      §CVDLI
848200011106     C     W0133         ANDGT     *ZEROS
848300011002     C                   MOVEL     MSG(23)       VIDMSG
848400011002     C                   SETON                                        90
848500011002     C                   GOTO      ENDVLT
848600011002     C                   ENDIF
848700941217     C*
848800941217     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
848900941217    1C     §CVFDC        IFNE      'S'
849000941217     C                   MOVE      W0133         W0033             3 3
849100941217    2C     W0033         IFNE      0
849200941217     C                   MOVEL     MSG(20)       VIDMSG
849300950104     C                   SETON                                        90
849400941217     C                   GOTO      ENDVLT
849500941217    2C                   ENDIF
849600941217    1C                   ENDIF
849700011002      *
849800011002      * Controllo il numero di decimali immessi
849900011011    1C     §CVFDC        IFEQ      'S'
850000011002     C     W0133         ANDNE     *ZEROS
850100011011      *
850200011011    2C                   SELECT
850300011011     C     §CVDEC        WHENEQ    3
850400011011     C     §CVDEC        WHENEQ    2
850500011011     C                   MOVE      W0133         W0010             1 0
850600011011    3C     W0010         IFNE      *ZEROS
850700011011     C                   MOVEA     MSG(24)       K78
850800011011     C                   MOVE      §CVDEC        WCVDEC            2
850900011011     C                   MOVEA     WCVDEC        K78(29)
851000011011     C                   MOVEA     K78           VIDMSG
851100011011     C                   SETON                                        90
851200011011    3C                   ENDIF
851300011011     C     §CVDEC        WHENEQ    1
851400011011     C                   MOVE      W0133         W0020             2 0
851500011011    3C     W0020         IFNE      *ZEROS
851600011011     C                   MOVEA     MSG(24)       K78
851700011011     C                   MOVE      §CVDEC        WCVDEC            2
851800011011     C                   MOVEA     WCVDEC        K78(29)
851900011011     C                   MOVEA     K78           VIDMSG
852000011011     C                   SETON                                        90
852100011011    3C                   ENDIF
852200011011     C     §CVDEC        WHENEQ    0
852300011011    2C                   ENDSL
852400011002      *
852500011011    1C                   ENDIF
852600941217     C*
852700941217     C     ENDVLT        ENDSR
852800941219     C**************************************************************************
852900941219     C*  DECODIFICA CONSEGNE PARTICOLARI
853000941219     C**************************************************************************
853100941219     C     DECCPA        BEGSR
853200941219     C                   CLEAR                   V1DTC1
853300050113     C                   CLEAR                   V3DTC1
853400941219     C     ARBTC1        IFNE      *BLANKS
853500941219     C                   MOVEL     '1P'          COD
853600941219     C                   MOVEL(P)  ARBTC1        KEY
853700941219     C     KTAB          CHAIN     TABEL                              32
853800941219     C  N32              MOVEL     TBLUNI        V1DTC1
853900050113     C  N32              MOVEL     TBLUNI        V3DTC1
854000941219     C                   ENDIF
854100941219     C*
854200941219     C                   CLEAR                   V1DTC2
854300050113     C                   CLEAR                   V3DTC2
854400941219     C     ARBTC2        IFNE      *BLANKS
854500941219     C                   MOVEL     '1P'          COD
854600941219     C                   MOVEL(P)  ARBTC2        KEY
854700941219     C     KTAB          CHAIN     TABEL                              32
854800941219     C  N32              MOVEL     TBLUNI        V1DTC2
854900050113     C  N32              MOVEL     TBLUNI        V3DTC2
855000941219     C                   ENDIF
855100941219     C                   ENDSR
855200990914     C**************************************************************************
855300990914     C*  REPERIMENTO TABELLA GEI
855400990914     C**************************************************************************
855500990914     C     RIEGEI        BEGSR
855600991117     C                   CLEAR                   DSBS02
855700991117     C                   CLEAR                   DGEI
855800991117     C                   MOVEL     'C'           T02MOD
855900991117     C                   MOVEL     KNSIF         T02SIF
856000991117     C                   MOVEL     'GEI'         T02COD
856100991117     C                   MOVEL     ARBDIV        T02KE1
856200991117     C                   CALL      'TIBS02R'
856300991117     C                   PARM                    KPJBA
856400991117     C                   PARM                    DSBS02
856500991117     C     T02ERR        IFEQ      *BLANKS
856600991117     C                   MOVEL     T02UNI        DGEI
856700991117     C                   ENDIF
856800990914     C                   ENDSR
856900040513     C**************************************************************************
857000040513     C*  Aggiornamento IAS/VAS/QFT su fnblp
857100040513     C**************************************************************************
857200040513     C     AGG_blp       BEGSR
857300040513     c     karb2         chain     fnblp01l
857400040513     c                   if        %found(fnblp01l)
857500040513     c                   eval      blpqft = arbqft
857600040513     c                   eval      blpias = arbias
857700040513     c                   eval      blpvas = arbvas
857800040513     c                   update    fnblp000
857900040513     c                   endif
858000040513     c                   ENDSR
858100060317     C**************************************************************************
858200060317     C*  Richiamo pgm di richiesta/controllo motivazione variazione
858300060317     C**************************************************************************
858400060317     C     ges_mtv       BEGSR
858500060317     c                   clear                   fnlv85ds
858600060317     c                   movel     'V'           ilv85tel
858700060317     c                   movel     ilra2cvb      ilv85cvb
858800060317     c                   movel     fnlv85ds      kpjbu
858900060317     c                   call      'FNLV85R'
859000060317     c                   parm                    kpjba
859100060317     c                   movel     kpjbu         fnlv85ds
859200060317     c                   endsr
859300060317     C**************************************************************************
859400060317     C*  Richiamo pgm per storicizzazione motivazione variazione
859500060317     C**************************************************************************
859600060317     C     sto_mtv       BEGSR
859700060317     c                   clear                   fnlv85ds
859800060317     c                   movel     'L'           ilv85tla
859900060317     c                   movel     'R'           ilv85tel
860000060317     c                   movel     ilra2cvb      ilv85cvb
860100060317     c                   z-add     arbdtv        ilv85dac
860200060317     c                   z-add     arborv        ilv85orc
860300060317     c                   z-add     vidaas        ilv85aas
860400060317     c                   z-add     vidlnp        ilv85lnp
860500060317     c                   z-add     vidnrs        ilv85nrs
860600060317     c                   z-add     vidnsp        ilv85nsp
860700060317     c                   movel     fnlv85ds      kpjbu
860800060317     c                   call      'FNLV85R'
860900060317     c                   parm                    kpjba
861000060317     c                   endsr
861100130416     C**************************************************************************
861200130416     C* Invio tassazioniee fattura immediata a pda se bolla in distinta
861300130416     C**************************************************************************
861400131119     C*    sr_pda        BEGSR
861500131119     c*                  if        §7lpda='S'
861600131119     c*                  z-add     arbifp        w0030             3 0
861700131119     c*    w0030         chain     azorg01l
861800131119     c*                  if        %found(azorg01l)
861900131119     c*                  eval      og148=orgde8
862000131119     c*                  if        §ogpdacon<>' ' and arbndc>0 and arbdcm=0
862100131119     c*                  clear                   fidg42ds
862200131119     c*                  eval      CO42FGS=arbifp
862300131119     c*                  eval      CO42NDC=arbndc
862400131119     c*                  eval      CO42DDC=arbddc
862500131119     c*                  eval      CO42AAS=arbaas
862600131119     c*                  eval      CO42LNP=arblnp
862700131119     c*                  eval      CO42NRS=arbnrs
862800131119     c*                  eval      CO42NSP=arbnsp
862900131119     c*                  eval      CO42CMT='N'
863000131119     c*                  movel     fidg42ds      kpjbu
863100131119     c*                  call      'FIDG42R'
863200131119     c*                  parm                    kpjba
863300131119     c*                  endif
863400131119     c*                  endif
863500131119     c*                  endif
863600131119     C*                  ENDSR
863700050310     Ofnblpfis  E            AGGblpfis
863800050310     O                       arbvas
863900050310     O                       arbias
864000050310     O                       arbqft
864100991206     C**************************************************************************
864200011011**
864300911115SEGUE  FATTURA
864400911114FATTURAZIONE IMMEDIATA
864500911028NON ESISTONO TARIFFE
864600911115    SEGUE  FATTURA
864700911029**
864800050310 ***  TASSAZIONE  PORTI  ASSEGNATI  ***
864900050329 * TASSAZIONE ANTICIPATA  P.ASSEGNATI *
865000050310 ***  MANUTENZIONE  BOLLE   ARRIVI  ***
865100030613**  cmA4
865200030613OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
865300111221 SHARE(*YES) USRDTA('Tass.PA A4') OVRSCOPE(*CALLLVL) SECURE(*YES)
865400030613**  cmA5
865500030613OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
865600111221 SHARE(*YES) USRDTA('Tass.PA A5') OVRSCOPE(*CALLLVL) secure(*YES)
865700071001** OVR
865800151013OVRDBF FILE(FIAR531C) TOFILE(
865900071001OVRDBF FILE(TITAS30C) TOFILE(
866000941216**         MSG
866100941216Tipo incasso C/Assegno errato
866200941216Cliente non autorizzato al tipo incasso C/A indicato
866300941216Tipo incasso non utilizzabile se C/Assegno in divisa estera                   03
866400941216Filiale Cliente inesistente
866500941216Codice non ammesso se cliente di altra filiale                                05
866600941216Codice cliente errato o inesistente
866700941216Codice cliente non utilizzabile: bloccato in Piano dei Conti
866800941216Codice cliente di altra filiale: F9 per forzare l'immissione
866900941216Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 09
867000941216Codice tariffa inesistente
867100941227Non sono state trovate tariffe per il codice utilizzato per tassare           11
867200040119§§ovvigione a zero: non indicata la percentuale in tariffa                    12
867300941217Errore in tassazione                                                          13
867400941217Se modificata la divisa, ridigitare il relativo importo                       14
867500011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR.TEL.IN SEDE X AUTOR.  15
867600941217Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  16
867700011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  17
867800011204Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 18
867900941217Valuta errata                                                                 19
868000941217Non ammessi importi con decimali per la valuta indicata                       20
868100040119§§ immessa la divisa immettere anche l'importo o eliminare entrambi           21
868200941217Non e' possibile al momento eseguire la variazione: attendere e riprovare     22
868300011002Divisa scaduta                                                                23
868400011002Ammessi importi con massimo __ decimali per la divisa indicata                24
868500941219Codice errato                                                                 25
868600040119§§dice inutilizzabile                                                         26
868700040119§§ria 1 Errata                                                                27
868800040119§§ria "G" non ammessa                                                         28
868900040119§§ria "G" obbligatoria                                                        29
869000941219Immettere la varia "G" o cambiare tipo bolla per non far pagare le provvig.   30
869100040119§§dice esenzione IVA errato
869200941221Imponibile obbligatorio se fatturazione immediata
869300950104Q.ta'da fatturare = 0 per tariffa a valore:ENTER per prendere importo da assic33
869400950104Inserire la quantita' fatturare  per tariffa a valore                         34
869500950104Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic35
869600980526Per tariffa a quantita' inserire la quantita' da fatturare: ENTER per forzare 36
869700991014Modificato importo da assic. per tariffa a valore:azzerare la tassazione      37
869800950509Importo da assicurare obbligatorio come indicato in tariffa                   38
869900960710Record bolla attualmente in uso: attendere e riprovare!!                      39
870000960710Fine scorrimento
870100970605Non trovato il numeratore delle fatture: tassazione non effettuabile          41
870200040119§§r bolle export con VARIA "N", tassare la spedizione con F14 !!              42
870300011128Importo da Assicurare non ammesso                                             43
870400981008Per codice 9999 la linea cliente deve essere uguale alla linea arrivo bolla   44
870500040119§§possibile fare fattura immediata a cliente di altro network                 45
870600060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 46
870700990928Per codice cliente 9999 senza tariffa è obbligatoria la divisa xxx            47
870800991124Divisa obbligatoria se immesso almeno un importo di tassazione                48
870900000911Non so possono fare segue Fattura su clienti poste                            49
871000050120Per Tassazione multipla non ammesso codice cliente senza tariffe              50
871100020625Codice tariffa proposto da bollettazione                                      51
871200011126Tipo incasso non utilizzabile per la divisa del contrassegno                  52
871300011008Manca la varia 'N'                                                            53
871400011011Divisa obbligatoria                                                           54
871500011203Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       55
871600011203Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                56
871700011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  57
871800021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            58
871900030804Ampiezza massima subfile raggiunta. Non sono visualizzate tutte le spedizioni 59
872000040121Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "
872100040930Codice cliente non utilizzabile                                               61
872200050112Presenti Opzioni: premere ENTER o eliminarle per premere F1-Conferma totale   62
872300050125Tassazione multipla di bolle con codici clienti/tariffa diversi:ENTER x forzar63
872400050120Per tassazione Multipla impossibile selezionare tariffa che preveda FATT.IMMED64
872500050120Per Tassazione Multipla impossibile selezionare codice cliente 9999           65
872600050120Errori in tassazione bolle : tassarle con opzione '1'                         66
872700050120Errori di allocazione record bolla e di tassazione: riprovare con opzione '1' 67
872800050125Cliente con cod.tariffa diversi per tipo servizio
872900050126PROGRAMMA  AL  MOMENTO  NON  UTILIZZABILE !!!!!                               69
873000050126Impossibile tassazione multipla per bolle FEDEX                               70
873100050203Impossibile tassazione multipla per bolle con network differenti              71
873200050203Impossibile tassazione multipla per bolle monovaria e non                     72
873300050203Impossibile tassazione multipla x spedizioni con tipo bolla e network diversi 73
873400050310In tassazione P.A. in PARTENZA, impossibile selezionare codice cliente 99999  74
873500051013Tassazione P.A.in PARTENZA:tariffa che prevede FATT.IMMEDIATA non selezionabil75
873600050310F15-forzatura fattura immediata non ammessa per tassazione in partenza        76
873700050318Impossibile tassazione multipla per bolle in assegnato solo per spese di C/ASS77
873800050804ATTENZIONE! Bolla NON Manutenzionata.Uscita automatica x 50 sec di inattività
873900050926Porto obblogatorio per bolla dirottata                                        79
874000051013F15-forzatura fattura immediata non ammessa per codice  ESTERO                80
874100051013Codice ESTERO : tariffa che prevede FATT.IMMEDIATA non selezionabile          81
874200051013Fattura immediata non ammessa per codice  ESTERO                              82
874300051024Impossibile tassaz.multipla:manca ImpAssicurare x tariffa con obbligo inserim.83
874400051024Impossibile tassaz.multipla:presenti ImpAssicurare non ammessi x il CodTariffa84
