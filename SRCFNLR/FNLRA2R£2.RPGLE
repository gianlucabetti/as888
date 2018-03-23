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
002500050110     FFNARB07L  IF   E           K DISK    RENAME(FNARB000:FNARB007)
002600050309     c* Tassazione P.A. in partenza
002700050309     FFNBLP00f  uF   E             DISK    rename(fnblp000:fnblpfis)
002800050309     f                                     prefix(ARB:3)
002900050310     FFNblp07L  iF   E           K DISK    RENAME(FNblp000:FNblp007)
003000050309     f                                     prefix(ARB:3)
003100050309     F                                     INFDS(blp07)
003200050309     f**
003300020911     fFnlbl01l  if   e           k Disk
003400040513     fFnblp01l  uf   e           k Disk
003500050309     F                                     INFDS(blp01)
003600071001     fFiar501l  if A e           k Disk
003700151013     f*Tita430c  uf A e           k disk    usropn
003800071001     fTitas30c  if   e           k disk    usropn
003900071001     f                                     prefix(S_)
004000151013     fFiar531c  uf A e           k Disk    usropn
004100151013     f                                     rename(fiar5000:fiar500S)
004200151013     f                                     rename(fiar5p00:fiar5p0S)
004300030613      **
004400990907     FFIARBT0F  O  A E           K DISK    USROPN
004500170116     FFIARBT2T  O  A E           K DISK    USROPN RENAME(FIARBT00:FIARBTTT)
004600941217     F**
004700990907     FFIARBU0F  O  A E           K DISK    USROPN
004800170116     FFIARBU2T  O  A E           K DISK    USROPN RENAME(FIARBU00:FIARBUTT)
004900030722     F**
005000030613
005100050309     D blp07           DS
005200050309     D  blp07nrr             397    400B 0
005300050309     D blp01           DS
005400050309     D  blp01nrr             397    400B 0
005500060123     D** L1              S              3  0 DIM(30)                            FIL DELL'AS
005600920319     D L6              S              3  0 DIM(30)                              FIL GESTITE
005700050523     d
005800941217     D K78             S              1    DIM(78)                              COMODO
005900941217     D* VARIE DI COMODO
006000941217     D WSV             S              1    DIM(21)                              SIGLA  VARIE
006100990907     D WVA             S             11  3 DIM(21)                              VALORE VARIE
006200050113     D*
006300150120     D SVA             S              1    DIM(100)                             VARIE IN TOT.I
006400050113     D*
006500050127     D* CODICI ASSEGNATI PREIMPOSTATI SE C'E' TASSAz MULTIPLA DI COMODO
006600050125     D TAS9KC          S             10    DIM(20)                              SIGLA  VARIE
006700050125     D TAS9K           S              7    DIM(20)                              SIGLA  VARIE
006800050125     D TAS9Nk          S              7  0 DIM(20)                              VALORE VARIE
006900050125     D TAS9N           S              7  0 DIM(20)                              VALORE VARIE
007000050112     D TAS9T           S              1    DIM(3)                               VALORE VARIE
007100050128     D* schiere cod.tariffa del cliente
007200170103     D ctrw            S              3    DIM(16)                              VARIE IN TOT.I
007300170103     D fiew            S              1    DIM(16)                              VARIE IN TOT.I
007400170103     D tspw            S              1    DIM(16)                              VARIE IN TOT.I
007500170103     D bapw            S              1    DIM(16)                              VARIE IN TOT.I
007600170103     D tfpw            S              1    DIM(16)                              VARIE IN TOT.I
007700170103     D valw            S              1    DIM(16)                              VARIE IN TOT.I
007800050113     D* schiere del sfl
007900990909     D*
008000050113     D Sksflcon        S              1    DIM(9902)                            VARIE IN TOT.I
008100050113     D Sksflsen        S              1    DIM(9902)                            VARIE IN TOT.I
008200050113     D Skiopz          S              1    DIM(9902)                            VARIE IN TOT.I
008300911218     D*
008400020805     D SOVR            S             48    DIM(1)                               DI COMODO SED
008500020805     D SALC            S             48    DIM(1)                               DI COMODO SED
008600020805     D SDLC            S             48    DIM(1)                               DI COMODO SED
008700050126     d
008800050309     D TBSTO           S              2    DIM(25)
008900050309     D CBOASS          S              2    DIM(25)
009000050309     D CBOASS2         S              2    DIM(25)
009100050309     d cborec          s              2    dim(25)                              sk cbo recupero
009200020715     d tbl             s              2    dim(20)                              sk tbl con ass.
009300020703
009400941217     D*
009500941217     D TAS             S             22    DIM(4) CTDATA PERRCD(1)              DESCRIZ TASSAZ
009600050310     D TES             S             39    DIM(3) CTDATA PERRCD(1)              TESTATE
009700941217     D*
009800170116     D*C20             S              1    DIM(48) CTDATA PERRCD(48)
009900170116     D*C21             S              1    DIM(48) CTDATA PERRCD(48)
010000170116     D*C22             S              1    DIM(48) CTDATA PERRCD(48)
010100030613      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
010200030613     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
010300030613     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
010400071001     d OVR             s             60    dim(02) ctdata perrcd(1)
010500051024     D MSG             S             78    DIM(84) CTDATA PERRCD(1)
010600011127
010700011220     d sav_geimf       s                   like(§geimf)
010800011203     d w0173           s             17  3
010900011127     d tot_importi     s             17  3
011000011128     d forza           s              1
011100011203     d sav_vidias      s                   like(vidias)
011200040513     d sv1_vidias      s                   like(vidias)
011300040513     d sv1_vidvas      s                   like(vidvas)
011400040513     d sv1_vidqft      s                   like(vidqft)
011500020222     d lnpntw          s              3
011600020222     d lnantw          s              3
011700020222     d clintw          s              3
011800050125     d wcodtar         s              3
011900050125     d wcodtar1        s                   Like(wcodtar)
012000050125     d wcodtar2        s                   Like(wcodtar)
012100050125     d w003a           s                   Like(wcodtar)
012200020805     d $mbr            s              6
012300020911     d Kfgs            s                   Like(FvvFgs)
012400021121     d DueCtr          s              2  0
012500021121     d Wctr            s                   Like(VidCtr)
012600030708     d kAr5Trd         s                   Like(Ar5Trd)
012700030506     d kNtcApl         s                   Like(NtcApl) Inz('C')
012800030506     d kNtcNk1         s                   Like(NtcNk1)
012900030506     d kNtcNk2         s                   Like(NtcNk2)
013000030506     d KntcTnt         s                   Like(NtcTnt) Inz('DC')
013100030506     d sav_vidksc      s                   Like(vidksc)
013200030605     d sav_indsin      s                   Like(indsin)
013300050126     d wtas9rec        s                   Like(rec)
013400050126     d Wtas9ntw        s                   Like(§ogntw)
013500050203     d Wtas9sva        s                   Like(§3asva)
013600050112     d wforzatas9      s              1
013700050112     d wcontabolle     s              7  0
013800050112     d wsalvaconta     s                   like(wcontabolle)
013900050112     d wsflpieno2      s              1
014000050110     d wsflpieno22     s              1
014100050110     d wtiposfl        s              1
014200050111     d wscelta         s              1
014300110629     d wemd            s              1
014400150107     d wpincode        s              1
014500040119     d va2va2          s                   Like(Ar6Va1)
014600040119     d va2va3          s                   Like(Ar6Va1)
014700040119     d va2va4          s                   Like(Ar6Va1)
014800041129     d va2va5          s                   Like(Ar6Va1)
014900041129     d va2va6          s                   Like(Ar6Va1)
015000040119     d war61           s              1
015100040119     d war71           s              1
015200050310     d wesistear6      s              1
015300050310     d wesistear7      s              1
015400050310     d wtiporecar6     s                   like(ar6trc)
015500050121     d WERR            s              2
015600050318     d errmult         s              3
015700050203     d Waggerr         s              1
015800050209     d Wusaaca         s              1
015900050314     d Wscarta         s              1
016000050128     d aa              s              3  0
016100061107     d w0040           s              4  0
016200040119     d contasv         s              2  0
016300040119     d totimv          s                   Like(Ar6Imv)
016400061107     d wcdfis          s                   Like(arbcpi)
016500071030     d wpariva         s                   Like(arbcpi)
016600040119     d wesvar1         s              1
016700040120     d wvar3ai         s              1
016800060123     d wVaria          s              1
016900071001     d wlib            s             10
017000020117
017100100120     d Posiz           s              2  0
017200100120     d c               s              3  0
017300050309     d XX              s              3  0
017400050309     d II              s              3  0
017500050309     d ZZ              s              3  0
017600050201     d Wsfact          s                   Like(vsfact)
017700070924     d Savimv          s                   Like(ar6imv)
017800070924     d PesVolpkcn      s                   Like(Taspkcn)
017900070924     d PesVolSpc       s                   Like(TasSpc)
018000070924     d PesVolIMV       s                   Like(Ar6IMV)
018100070924     d Tassatoimv      s                   Like(Ar6IMV)
018200011127
018300921130     D*
018400050309     D* PASSO A INTERROGAZIONE BOLLE ARRIVI              - FNLR36R -
018500161102     D*PARAM           DS
018600161102     D* GIA                    1     12  0
018700161102     D* RICH                  13     13
018800161102     D* VsfAAS                14     17  0
018900161102     D* VsfLNP                18     20  0
019000161102     D* VsfNRS                21     22  0
019100161102     D* VsfNSP                23     29  0
019200161102     D* PARFLG                31     31
019300050309     D* RICHIAMO PGM INTERROGAZ.BOLLE PARTENZE            - FNLS04R -
019400161102     D*PARAM3          DS
019500161102     D* PA3AAS                 1      4  0
019600161102     D* PA3LNP                 5      7  0
019700161102     D* PA3NRS                 8      9  0
019800161102     D* PA3NSP                10     16  0
019900161102     D* PA3FL2                18     18
020000161102     D* PA3RSU                23     42
020100161102     D* PA3FLG                31     31
020200161103
020300161103     d fnlru6ds      e DS
020400161103
020500161102     D* RICHIAMO PGM INTERROGAZ.BOLLE unica               - FNLRU6R -
020600161102     D PARAMu6ds       DS
020700161102     d* CAMPI DI OUTPUT:
020800161102     d* . Key spedizione
020900161102     D  pa1AAS                14     17  0
021000161102     D  pa1LNP                18     20  0
021100161102     D  pa1NRS                21     22  0
021200161102     D  pa1NSP                23     29  0
021300161102     d* . Flag '1'=Premuto F3=Fine
021400161102     D  PA1F03                30     30
021500161102     d* CAMPI DI INPUT:
021600161102     d* . Modalità di richiamo
021700161102     D  PA1FLG                31     31
021800161102     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F14=Giac.
021900161102     d*                                in interrogazione della bolla ed evitare così
022000161102     d*                                errore di chiamata ricorsiva
022100161102     D  PA1GIA               144    144
022200161102     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
022300161102     d  pa1rma               149    163
022400161102     d  pa1xco               164    164
022500161102     d* non utilizzato
022600161102     d  pa1flo               165    256
022700161102     D PARAMU6ds1      DS
022800161102     d** INPUT
022900161102     D  pa1AAS_               14     17  0
023000161102     D  pa1LNP_               18     20  0
023100161102     D  pa1NRS_               21     22  0
023200161102     D  pa1NSP_               23     29  0
023300161102     d** OUTPUT
023400161102     D  PA1F03_               30     30
023500161102     d** INPUT
023600161102     D  PA1FLG_               31     31
023700161102     D*    SE RICH DA PGM GIAC. = 1
023800161102     D  PA1GIA_              144    144
023900161102     d** OUTPUT
024000161102     d  pa1rma_              149    163                                         x F22=PerfectOrder
024100161102     d  pa1xco_              164    164                                         x F22=PerfectOrder
024200161102     d** INPUT
024300161102     d*   Tipo lancio: "C" -           CHIUSO CON LR
024400161102     d*                "L" - ELABORO E CHIUDO CON LR
024500161102     d*                " " - ELABORO E CHIUDO IN RETRN
024600161102     d  partla               165    165
024700161102     d  parflo               166    256
024800921130      *
024900941220     D                 DS
025000941220     D  EMME                   1      1    INZ('M')
025100020805     d  $Simfel                2      4  0
025200020805     D  PARMBR                 5      7  0
025300941220     D  MBRFIS                 1     10
025400941216     D***
025500941216     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
025600941216     D***
025700941216     D WLBDAT          DS                  INZ
025800941216     D  G02DAT                 1      8  0
025900941216     D  G02INV                 9     16  0
026000941216     D  G02ERR                17     17
026100941216     D  G02TGI                18     22  0
026200900516     D MMGG            DS
026300900516     D  MM                     1      2  0
026400900516     D  GG                     3      4  0
026500901116     D                 DS
026600911025     D  VIDLNP                 1      3  0
026700911025     D  VIDNRS                 4      5  0
026800911025     D  VIDNSP                 6     12  0
026900911025     D  SEL1                   1     12  0
027000911028     D                 DS
027100941216     D  ARBAAS                 1      4  0
027200941216     D  ARBMGS                 5      8  0
027300941216     D  ARBDSP                 1      8  0
027400900517     D CA02DS          DS
027500900517     D  PRIMA                378    379B 0
027600911024     D KPJBA         E DS
027700930209     D  LIB                   92    101
027800941222     D DSARBD        E DS
027900941222     D DSARBG        E DS
028000990907     D DARBT         E DS
028100941222     D DSARBK        E DS
028200941217     D DSCV          E DS
028300941217     D DSCC          E DS
028400941217     D DS15          E DS
028500930211     D DS3A          E DS
028600030217     d ds3aw         e ds                  Extname(ds3a)
028700030217     d                                     Prefix(w_)
028800020715     d dstb          e ds
028900990907     D DGEI          E DS
029000990915     D DGED          E DS
029100020527     D DSBL4A        E DS
029200151127     D DSBL4m        E DS
029300151013     D Dar5fat       E DS
029400151127     D Dar5gen       E DS
029500990923     D DS$2          E DS
029600060529     D*DSBL4F        E DS
029700060529     D DSBL4I        E DS
029800911211     D DS7L          E DS
029900150108     D DS7R          E DS
030000020725     D DYIV          E DS
030100000202     D OG143         E DS
030200070314     D OG148         E DS
030300021121     d Ddfe          e ds
030400050714     d DVPO          e ds
030500991028     D DSTA01        E DS
030600160927     D DTAS          E DS                  inz
030700060925      ** DS Flag operativi DS DTAS
030800060925     d Dtas01        e ds
030900060925
031000990907     D DTASV         E DS
031100030613     D  VSV                    1     20    DIM(20)                              SIGLA  VARIE
031200030613     D  VVA                   21    140P 3 DIM(20)                              VALORE VARIE
031300030613     D  VES                  141    160    DIM(20)                              ESENZ  VARIE
031400011002      *
031500011002     D DSCW          E DS
031600090604     d ds5e          e ds
031700011002      *
031800990907     D* DS PER FNLV04R - PER CALCOLO TOTALE FATTURA
031900990907     D DSLV04        E DS                  EXTNAME(FNLV04DS)
032000941216     D*
032100990907     D* DS PER FNLV16R - CONTROLLO TASSAZIONE
032200990907     D DSLV16        E DS                  EXTNAME(FNLV16DS)
032300040119     d dlv1601       e ds
032400941216     D*
032500060217     D* DS PER FNLV19R - SCRITTURA E AGGIORNAMENTO FiAR400F
032600941216     D DSLV19        E DS                  EXTNAME(FNLV19DS)
032700941216     D*
032800050128     D* DS PER FNLV50R - controllo p.o. gestione
032900941217     D DSLV50        E DS                  EXTNAME(FNLV50DS)
033000941217     D*
033100050128     D* DS PER FNLV59R - nuovo pgm di CARICAMENTO E CONTROLLO CODICI TARIFFA
033200050128     D fnlv59ds      E DS
033300941216     D*
033400971117     D* DS PER FNLV53R - CARICAMENTO DATI DI UN FOGLIO
033500971117     D DSLV53        E DS                  EXTNAME(FNLV53DS)
033600971117     D*
033700030718     D* DS PER stampa LDV
033800130320     D*fnlsb5DS      E DS
033900030613      *
034000050714     D* DS controllo se bolla dirottata
034100050714     D FNLV58DS      E DS
034200060123     D wFnlv58DS     E DS                  extname(fnlv58ds) prefix(W)
034300060317     d* Ds richiesta/aggiornamento motivo variazione
034400060317     D FNLV85DS      E DS
034500050714      *
034600030613      * DS RICHIESTA STAMPANTI ricevuta in input
034700030613     D Trul90DS      E DS
034800050519      * DS per controllo riempimento schiera
034900050519     D Trul0sDS      E DS
035000030613      * DS PER TRUL90R - RICHIESTA STAMPANTI eseguita dal pgm
035100030613     D WTrul90DS     E DS                  ExtName(Trul90DS)  Prefix(W)
035200030613      *
035300030613      * DS PER TISI95R - CONTROLLO CAP
035400970710     D DSSI95        E DS                  EXTNAME(TISI95DS)
035500970710     D DSLV13        E DS                  EXTNAME(FNLV13DS)
035600941217      *
035700050804     D PARAMT          DS
035800050804     D  PARCA                256    256
035900050804      *
036000941216     D* DS PER TRUL06R - CARICAMENTO £X
036100941216     D DSUL06        E DS                  EXTNAME(TRUL06DS)
036200941216     D  LIN                    1     90  0
036300941216     D                                     DIM(30)                              SKI COMODO
036400050317     D* DS PER FNLRA2R - DATI DI ENTRATA
036500050317     D DSLRA2        E DS                  EXTNAME(FNLRA2DS)
036600941216      *
036700941216     D* DS PER FNLR48R - VARIAZIONE BOLLE
036800941216     D DSLR48        E DS                  EXTNAME(FNLR48DS)
036900941216      *
037000941216     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
037100941216     D DSLR66        E DS                  EXTNAME(FNLR66DS)
037200990914     D* DS PER YEURCO - EURO CONVERTITORE
037300990914     D DSYEUR        E DS                  EXTNAME(YEURCODS)
037400991117     D* DS PER TIBS02R - RICERCA TABELLA
037500991117     D DSBS02        E DS                  EXTNAME(TIBS02DS)
037600061107     D* DS PER YCO602R - RICERCA codice fiscale
037700061107     D YCO600DS      E DS
037800011127
037900020222     d trul22ds      e ds
038000160226     d trul27ds1     e ds
038100160226     d trulc7ds      e ds
038200030203     d dAr5Ban       e ds
038300030708     d dAr5Bnb       e ds
038400130924     d dAr5emd       e ds
038500030506
038600030506     D TIBS69DS      E DS
038700030506     D ds_cnaco      E DS                  extname(CNACO00F)
038800030506     D ds_cnind      E DS                  extname(CNIND00F)
038900030506     D ds_cnclp      E DS                  extname(CNCLP00F)
039000030506     D ds_fncls      E DS                  extname(FNCLS00F)
039100130416      * DS PER FIDG42R -
039200131119     D*Fidg42ds      E DS
039300981116      *
039400911028     D CNCR80        E DS
039500911024     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
039600000000     D  TCU                  398    697
039700000000     D                                     DIM(50)                              TAB FLAG CNT
039800000000     D  KCU                  698    847P 0
039900000000     D                                     DIM(50)                              TAB CAPO CONTO
040000000000     D                                     PACKEVEN
040100911028     D TCUDS           DS
040200911028     D  F1                     1      1
040300911028     D  F3                     3      3
040400911028     D  F2                     2      2
040500911028     D  F4                     4      4
040600911028     D  F56                    5      6
040700011128
040800011128     D                SDS
040900011128     D  NOMPGM                 1     10
041000090428
041100090428     d
041200090428     d  pjsocieta      s              3
041300090428     d  pjkcc          s              6
041400090428     d  pjksc          s              8
041500090428     d  pjesito        s             10I 0
041600100119     d
041700100119     D                 DS
041800100119     D  VIDISD                 1      2
041900100119     D  VIDCPD                 3     16
042000100119     D  VIDPID                 1     16
042100100119     D                 DS
042200100119     D  WISO                   1      2
042300100119     D  WCPI                   3     16
042400100119     D  WPIVA                  1     16
042500090428     d
042600090428     D chkContoIncassiAttribuire...
042700090428     D                 PR            10i 0
042800090428     D  pjsocieta                     3
042900090428     D  pjkcc                         6
043000090428     D  pjksc                         8     const
043100090428     D  pjesito                      10I 0
043200020725
043300020725      * Costanti
043400020725      * - Società chiave per tabella "YIV"
043500020725     D C_Soc           c                   const('201')
043600060117     D C_dirotta       c                   const('DIROTTATA, partita da')
043700060117     D C_cambio        c                   const('CAMBIO PORTO,parte da')
043800060117     D C_legame        c                   const('Bolla LEGATA,parte da')
043900071001     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
044000151013     I*TITA4000      92
044100151013     I*TITA4P00      93
044200151013     Ifiar500S      92
044300151013     Ifiar5p0s      93
044400071001     ITITAs000      92
044500071001     ITITAs010      92
044600071001     ITITAsP00      93
044700011128
044800900518     C****************************************************************
044900900518     C*  RIEPILOGO INDICATORI
045000900518     C***************************************************************
045100050121     C* 01    - F1  SEGUE FATTURA
045200960302     C* 02    - VISULIZZA BENE LA NATURA MERCE
045300120208     C* 03    - Abilitazione stampa DDT con F8
045400050112     C* 04    - TASSAZIONE MULTIPLA: proteggo campi nel video
045500911029     C* 05    - DOPPIA BOLLA CON ESTENSIONE
045600950421     C* 06    - PROTEGGO Q.TA' DA FATTURARE
045700990913     C* 07    - PROTEGGO DIVISA TASSAZIONE
045800030729     C* 08    - Visualizzo Colli Originali
045900941217     C* 09    - IMMESSA ALMENO UNA VARIA
046000050314     C* 10    - proteggo i campi del D03 quanndo lo emetto insieme al d04
046100911211     C* 11    - PGM RICHIAMATO
046200911211     C* 12    - PGM RICHIAMATO - RITASSAZIONE PORTI ASSEGNATI
046300050216     C* 13    - metto in HI le bolle sfl senza codice con codice "S"
046400050126     C* 14    - Visualizzo raggruppamento se c'e'
046500010703     C* 15    - VISUALIZZAZIONE LINEA ARRIVO DPD
046600920813     C* 16    - CAMBIO FILIALE
046700050314     C* 17    - p.o. 2 livello
046800930430     C* 18    - POSIZIONAMENTO CURSORE SU SUBFILE
046900930430     C* 19    - CAMPO SCELTA PROTETTO E NON VISUALIZZATO SU SUBFILE
047000050110     C* 20    - sfldsp su lsa1s02
047100050110     C* 21    - sfldsp su lsa1s22
047200050715     C* 22    - Bolla dirottata
047300120125     C* 23    -
047400971117     C* 25    - PROTEGGO I CONVENUTI SE IMMESSI DALLA PARTENZA
047500050317     C* 27    - SFLDSP E SFL CLR SU LRA2C06
047600941217     C* 28    - EMISSIONE VIDMSG
047700050114     C* 29    - CLIENTE SENZA TARIFFE
047800941220     C* 30    - LETTURA FNARB
047900941220     C* 31/33 - COMODO
048000911023     C* 34    - LETTURA SUBFILE
048100911023     C* 35    - PULIZIA SUBFILE
048200911111     C* 36    - SFLNEXCHG
048300050314     C* 38    - LETTURA FnAR600F
048400941220     C* 39    - COMODO
048500950104     C* 40-56 - POSIZIONAMENTO CURSORE PER ERRORI VIDMSG
048600950104     C* 58    - ERRORE: NON TROVATI ASSEGNATI O C/A DA TASSARE/ABILIT
048700071001     C* 70-83 - ERRORI ANCORA CON VIDMSG
048800900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
048900930430     C* 91    - NON E' POSSIBILE ESEGUIRE VARIAZ. MEMBRO ALLOCATO
049000151013     C* 92-93 - indicatori di lettura file fiar531c
049100050309     C* 94    - ON - TASSAZIONE PORTI ASSEGNATI IN PARTENZA
049200050314     C* 95    - ALLOCAZIONE record FILE
049300981008     C* 96    - OBBLIGATORIO TASSARE CON TARIFFA DI CARTELLO
049400981008     C* 97    - INDICATORE DI CHANGE SU IMPORTO DA ASSICURARE
049500941217     C* 98    - RI SU VALUTA IMP DA ASSIC SE PRESA IN AUTOMATICO DA NAZ
049600960227     C* 99    - BOLLA LOCALE
049700900518     C*****************************************************************
049800000000     C     *ENTRY        PLIST
049900000000     C                   PARM                    KPJBA
050000030613     C                   PARM                    TRUL90DS
050100030613      *
050200030613      * Il numero dei parametri ricevuti è testato, in caso di modifica
050300030613      *  ricercare la stringa "%PARMS"
050400070703     C                   IF        %PARMS = 2 and d90psl<>*blanks
050500030613     C                   movel     TRUL90DS      WTRUL90DS
050600030613     c                   ENDIF
050700030613      *
050800050317     C                   MOVEL     KPJBU         DSLRA2
050900940223     C*---------------------------------------------------------------*
051000130320     C***  WFINE         IFEQ      '1'
051100130320     C***                GOTO      FINE
051200130320     C***                ENDIF
051300941216     C*
051400050317     C* TIPO LANCIO, ILRA2TLA "C" -           CHIUSO CON LR
051500050317     C* TIPO LANCIO, ILRA2TLA "L" - ELABORO E CHIUDO CON LR
051600050317     C* TIPO LANCIO, ILRA2TLA " " - ELABORO E CHIUDO IN RETRN
051700941216     C*
051800050317    1C     ILRA2TLA      IFNE      'C'
051900070319     c
052000070319     C* SE pgm richiamato e cambiata filiale in gestione, ricarico i dati
052100070319     c                   if        ilra2fgs>0 and ilra2fgs<>vidfil
052200070319     c                   z-add     ilra2fgs      vidfil
052300070319     c                   exsr      DECOfgs
052400070319     c                   endif
052500070319     c
052600941227     C* VEDO SE PASSATA LA KEY SPEDIZIONE
052700050317     C     ILRA2NSP      IFGT      0
052800941227     C                   SETON                                        11
052900050317     C                   MOVEL     ILRA2AAS      VIDAAS
053000050317     C                   MOVEL     ILRA2LNP      VIDLNP
053100050317     C                   MOVEL     ILRA2NRS      VIDNRS
053200050317     C                   MOVEL     ILRA2NSP      VIDNSP
053300070314     c
053400941227     C                   ELSE
053500941227     C                   SETOFF                                       11
053600941227     C                   ENDIF
053700050309     C** tassazione in arrivo
053800050309     C  n94              MOVE      TES(1)        VIDTES
053900050309     C** tassazione in partenza
054000050309     C   94              MOVE      TES(2)        VIDTES
054100050309     c*
054200050804     C     WAPRE         IFEQ      ' '
054300030506     c                   Open      Tfntc01l
054400990907     C                   OPEN      FIARBT0F
054401170116     C                   OPEN      FIARBT2t
054500990907     C                   OPEN      FIARBU0F
054501170116     C                   OPEN      FIARBU2t
054600941217     C                   OPEN      CNNUM00F
054700941217     C                   OPEN      TNTAM01L
054800990817     C                   OPEN      TITPT01L
054900050804     C                   MOVEL     '1'           WAPRE             1
055000050804     C                   ENDIF
055100941216     C*
055200930422     C*
055300911025     C     INIZIO        TAG
055400050216     c                   clear                   wvisualdue
055500911025     C* PULIZIA CAMPI
055600941217    2C     *IN11         IFEQ      *OFF
055700941216     C                   MOVEL     DATEU         VIDAAS
055800911025     C                   MOVEL     *ZEROS        VIDLNP
055900911025     C                   MOVEL     *ZEROS        VIDNRS
056000911025     C                   MOVEL     *ZEROS        VIDNSP
056100911028     C                   MOVEL     *ZEROS        VIDLP2
056200020703     c                   eval      vidbrc = 'N'
056300911205     C                   MOVEL     'N'           VIDBRN
056400911203     C                   MOVEL     'S'           VIDBAB
056500050110     C                   MOVEL     'S'           VIDsfl
056600050110     c                   clear                   vidrsd
056700050318     c
056800050318     C     WIDMSG        IFNE      *BLANKS
056900050318     C                   MOVEL     WIDMSG        VIDMSG
057000050318     C                   SETON                                        28
057100050318     C                   ENDIF
057200911025     C*
057300900509     C     FOR01         TAG
057400050317     C                   EXFMT     LRA2D01
057500941216     C**
057600050110     C** F3  - FINE LAVORO
057700050318     c                   if        nousa='S'
057800050318     c                   goto      fine
057900050318     c                   endif
058000050318     c
058100900509     C   KC              GOTO      FINE
058200941216     C**
058300941216     C* SPENGO GLI INDICATORI DI POSIZIONAMENTO CURSORE
058400941216     C                   SETOFF                                       4928
058500941216     C**
058600050110     C** F7  - INTERROGAZIONE BOLLE ARRIVI
058700050309     c                   if        *inkg
058800050309     c                   EXSR      INTBOLLE
058900911025     C                   GOTO      FOR01
059000941217    3C                   END
059100941217    2C                   END
059200941216     C**
059300050110     C** F18- CAMBIO FILIALE
059400920813     C   KS              SETON                                        16
059500920813     C   KS              GOTO      FOR01
059600941216     C*
059700050113     C* 16 ON - CONTROLLO FILIALE IN GESTIONE
059800941216     C   16              EXSR      CTRFGS
059900941216     C   16
060000941216     CAN 90              GOTO      FOR01
060100941216     C**
060200900509     C* CONTROLLI FORMATO1
060300900509     C                   EXSR      CONTR
060400170505     C** 90              GOTO      FOR01
060401170505     c                   if        *in90
060402170505     c                   if        not *in11
060403170505     C   90              GOTO      FOR01
060404170505     c                   else
060405170505     c   90              eval      olra2msg='Spedizione allocata da un altro -
060406170505     c                             lavoro'
060407170505     c   90              eval      olra2err='1'
060408170505     c   90              goto      fine
060409170505     c                   endif
060410170505     c                   endif
060500901115     C*
060600911025     C* CARICO SOLO LA SPEDIZIONE SCELTA
060700050112     C                   Z-ADD     0             NRRS2             4 0
060800050112     C                   Z-ADD     0             NRRC2             4 0
060900911025     C*
061000941217    2C     SEL1          IFNE      0
061100940331     C*
061200940331     C* IMPOSTO LA CHIAVE PER CHAIN
061300050112     C                   MOVEL     VIDAAS        VsfAAS
061400050112     C                   MOVEL     VIDLNP        VsfLNP
061500050112     C                   MOVEL     VIDNRS        VsfNRS
061600050112     C                   MOVEL     VIDNSP        VsfNSP
061700050714     C* VERIFICO SE BOLLA dirottata
061800050714     c                   EXSR      CHIAMAlv58
061900050309     c
062000050113     c* Aggancio record a di ar4 per eventuale codice
062100050113     c                   clear                   vsfaks
062200050113     c                   clear                   vsfact
062300050309     c
062400060217     c* Aggancio fiar4 unico per prendere codice di tassazione
062500050113    3c                   if        not *in11
062600050113     C                   MOVEL     'A'           WTRC
062700060217     C     KARB4         CHAIN(N)  Fiar401L                           32
062800050113     C  N32              MOVEL     AR4NOT        DSBL4A
062900050113     C   32              CLEAR                   DSBL4A
063000050113    4C     §B4aks        IFGT      *ZEROS
063100050113     c                   movel     §b4aks        vsfaks
063200050113     c                   movel     §b4act        vsfact
063300050113    4c                   endif
063400050113    3c                   endif
063500050111     C* Tassazione singola
063600050111     c                   eval      wscelta='1'
063700050110     C                   EXSR      ELAB
063800911029     C*
063900911029     C  N11              GOTO      INIZIO
064000941217    3C   11              DO
064100050804     c                   if        %error
064200050804     c* Errore : uscita automatica
064300050804     c                   eval      olra2msg=msg(78)
064400050804     c                   eval      olra2err='1'
064500050804     c                   else
064600050317     C                   MOVEL     ARBFIV        OLRA2FIV
064700050317     C                   MOVEL     ARBNFT        OLRA2NFT
064800050317     C                   MOVEL     ARBDFT        OLRA2DFT
064900050804     c                   endif
065000911031     C                   GOTO      FINE
065100941217    3C                   END
065200911028     C**
065300941217   X2C                   ELSE
065400050216     c
065500050216     c                   EXSR      CARICabolle
065600911023     C*
065700901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
065800050112    3C     NRRS2         IFEQ      0
065900050112    3C     NRRC2         ANDEQ     0
066000911128     C                   SETON                                        58
066100911128     C                   GOTO      FOR01
066200941217    3C                   END
066300911025     C**
066400911025     C** SUBFILE
066500911025     C**
066600050110     C* IN BASE ALLA RICHIESTA DELLA 1 VIDEATA EMETTO SFL CON CODICE O SENZA
066700050125     c* se un dei 2 sfl è vuoto visualizzo quello pieno a prescindere
066800050125     c* dalla richiesta
066900050125     c                   select
067000050125     c                   when      nrrs2=0
067100050125     c                   eval      wtiposfl='C'
067200050125     c                   when      nrrc2=0
067300050125     c                   eval      wtiposfl='S'
067400050125     C                   when      VIDSFL='C'
067500050110     c                   eval      wtiposfl='C'
067600050125     c                   other
067700050110     c                   eval      wtiposfl='S'
067800050125     c                   endsl
067900050216     c* flag per visualizzare entrambi i sfl se pieni
068000050125     C
068100050125     C                   Z-ADD     1             NRRC2
068200050125     C                   Z-ADD     1             NRRS2
068300050125     C                   Z-ADD     1             REC
068400050216     c                   clear                   wvisualdue        1
068500050110     c
068600050110     c                   dow       *inkc=*off and *inkl=*off
068700050110     c                   if        wtiposfl='C'
068800050216     c
068900050110     c                   exsr      SFLCON
069000050216     c
069100050216     c* Se premuto f3 o F12  senza parzial si RSD e non ho ancora visualiz
069200050216     c*  zato entrambi i sfl pieni, emetto l'altro
069300050216     c                   if        not *in20 and not *in21 and
069400050216     c                             wvisualdue=' '  and
069500050216     c                             (*inkc or (*inkl and vidrsd=*blanks))
069600050216     c                   eval      *inkc='0'
069700050216     c                   eval      *inkl='0'
069800050216     c                   eval      wvisualdue='S'
069900050216     c                   eval      wtiposfl='S'
070000050216     c                   endif
070100050216     c
070200050110     c                   else
070300050216     c
070400050216   i c                   exsr      SFLSENZA
070500050216     c
070600050216     c* Se premuto f3 o F12  senza parzial si RSD e non ho ancora visualiz
070700050216     c*  zato entrambi i sfl pieni, emetto l'altro
070800050216     c                   if        not *in20 and not *in21 and
070900050216     c                             wvisualdue=' '  and
071000050216     c                             (*inkc or (*inkl and vidrsd=*blanks))
071100050216     c                   eval      *inkc='0'
071200050216     c                   eval      *inkl='0'
071300050216     c                   eval      wvisualdue='S'
071400050216     c                   eval      wtiposfl='C'
071500050216     c                   endif
071600050216     c
071700050110     c                   endif
071800050216     c
071900050216     c* Se non premuto F3 o F12 ricarico sfl
072000050216     c                   if        not *inkc and not *inkl and whotassato='S'
072100050216     c                   exsr      CARICAbolle
072200050216     c                   clear                   whotassato
072300050216     c                   endif
072400050216     c
072500050110     c                   enddo
072600050110     c*
072700050110     C* F3 - FINE
072800050110     C   KC              GOTO      FINE
072900050110     C* F12 - RITORNO
073000061201     c                   if        *inkl=*on
073100061201     c                   clear                   widmsg
073200061201     C                   GOTO      INIZIO
073300061201     c                   endif
073400050110     C
073500960710    2C                   END
073600960710    1C                   END
073700911023     C*
073800000000     C     FINE          TAG
073900941216     C*
074000050317     C     ILRA2TLA      IFNE      ' '
074100920113     C*
074200930423     C* CALL SOLO PER CHIUDERE I FILE
074300990907     C                   CLEAR                   DTAS
074400941216     C                   MOVEL     'C'           TASTLA
074500941216     C                   CALL      'TNSF20R'
074600930423     C                   PARM                    KPJBA
074700990907     C                   PARM                    DTAS
074800990907     C                   PARM                    DTASV
074900050314     c
075000971117     C                   CLEAR                   DSLV53
075100971117     C                   MOVEL     'C'           D53TLA
075200971117     C                   CALL      'FNLV53R'
075300971117     C                   PARM                    DSLV53
075400050201     C*
075500050201     C                   CLEAR                   fnlv59ds
075600050201     C                   MOVEL     'C'           ilv59tla
075700050201     C                   CALL      'FNLV59R'
075800050201     C                   PARM                    fnlv59ds
075900970710     C*
076000970710     C     WCAL13        IFEQ      'S'
076100970710     C                   MOVEL     'C'           I13TLA
076200970710     C                   CALL      'FNLV13R'
076300970710     C                   PARM                    KPJBA
076400970710     C                   PARM                    DSLV13
076500970710     C                   PARM                    DSSI95
076600970710     C                   ENDIF
076700971105     C**
076800971105     C                   CLEAR                   DSSI95
076900971105     C                   MOVEL     'C'           I95TLA
077000971105     C                   CALL      'TISI95R'
077100971105     C                   PARM                    DSSI95
077200930505     C*
077300990907     C                   CLEAR                   DSLV04
077400941216     C                   MOVEL     'C'           D04TLA
077500990913     C                   CALL      'FNLV04R'
077600990907     C                   PARM                    DSLV04
077700940224     C*
077800941216     C                   CLEAR                   DSUL06
077900941216     C                   MOVEL     'C'           D06TLA
078000941216     C                   MOVEL     DSUL06        KPJBU
078100940224     C                   CALL      'TRUL06R'
078200940224     C                   PARM                    KPJBA
078300020222
078400160226     c                   clear                   trul27ds1
078500020222     c                   eval      i27tla = 'C'
078600160226     c                   call      'TRUL27R1'
078700160226     c                   parm                    trul27ds1
078800050714
078900160226     c                   clear                   trulc7ds
079000160226     c                   eval      ic7tla = 'C'
079100160226     c                   call      'TRULC7R'
079200160226     c                   parm                    trulc7ds
079300160226
079400050714     c                   clear                   fnlv58ds
079500050714     c                   eval      ilv58tla = 'C'
079600050714     c                   call      'FNLV58R'
079700050714     c                   parm                    fnlv58ds
079800130416
079900131119     c*                  clear                   fidg42ds
080000131119     C*                  MOVEL     'C'           co42tla
080100131119     c*                  movel     fidg42ds      kpjbu
080200131119     c*                  call      'FIDG42R'
080300131119     c*                  parm                    kpjba
080400161102
080500161102     c                   eval      partla='C'
080600161102     c                   eval      kpjbu=paramu6ds1
080700161102     c                   call      'FNLRU6R1'
080800161102     c                   parm                    kpjba
080900020222
081000941216     C*
081100941216     C                   SETON                                        LR
081200941216     C                   ELSE
081300950120     C*
081400950120     C* PASSO IL NUMERO FATTURA PER RETRN
081500050317     C   11              MOVEL     DSLRA2        KPJBU
081600941216     C                   RETURN
081700941216     C                   ENDIF
081800950120     C*
081900950120     C* PASSO IL NUMERO FATTURA PER LR
082000050317     C   11              MOVEL     DSLRA2        KPJBU
082100911028     C*
082200911028     C*--- IMPOSTAZIONI INIZIALI -------------------------------------*
082300941216     C     *INZSR        BEGSR
082400941217     C*§ MI SALVO LA KPJBU
082500941217     C                   MOVEL     KPJBU         W256A           256
082600050110     C                   CLEAR                   WINTER            1
082700941217     C*
082800010703     C                   SETOFF                                       15
082900010703     C*
083000941216     C                   Z-ADD     1             CODUT             1 0
083100941216     C                   CALL      'X§PARUT'
083200941216     C                   PARM                    UT§DSE
083300941216     C                   MOVEL     RAGUT         VIDDSA
083400941216     C                   MOVEL     REC80         CNCR80
083500941216     C*--- RICERCA CAPOCONTI------------------------------------------*
083600941216     C                   DO        50            X                 3 0
083700941216     C                   MOVE      TCU(X)        TCUDS
083800941216     C     F56           CABNE     'CG'          END1
083900941216     C     F4            COMP      '1'                                    21
084000941216     C     F4            COMP      '2'                                    22
084100941216     C     F4            COMP      '3'                                    23
084200941216     C     F4            COMP      '6'                                    27
084300941216     C** 1 CLIENTI   21
084400941216     C** 2 FORNITORI 22
084500941216     C** 3 AGENTI    23
084600941216     C     F3            COMP      '0'                                242425
084700941216     C     F3            COMP      'I'                                    26
084800941216     C** 0 ITALIA   25
084900941216     C** 1 ESTERO   24
085000941216     ** I CAPO CONTO IVA
085100941216     C   21
085200941216     CAN 24              Z-ADD     KCU(X)        KCE               4 0
085300941216     C   21
085400941216     CAN 25              Z-ADD     KCU(X)        KCI               4 0
085500941216     C   22
085600941216     CAN 24              Z-ADD     KCU(X)        KFE               4 0
085700941216     C   22
085800941216     CAN 25              Z-ADD     KCU(X)        KFI               4 0
085900941216     C   23
086000941216     CAN 24              Z-ADD     KCU(X)        KAE               4 0
086100941216     C   23
086200941216     CAN 25              Z-ADD     KCU(X)        KAI               4 0
086300941216     C   26              Z-ADD     KCU(X)        KIVA              4 0
086400941216     C   27              Z-ADD     KCU(X)        KBNA              4 0
086500941216     C     END1          TAG
086600941216     C                   END
086700941216     C                   SETOFF                                       24
086800020805
086900020805     c                   movel     simfel        $mbr
087000020805     c                   eval      $simfel = simfel
087100941216     C**
087200941216     C* KLIST
087300941216     C**
087400970527     C     KFVV          KLIST                                                  *
087500970527     C                   KFLD                    KNPG                           *
087600970527     C                   KFLD                    W0050                          *
087700020911     C                   KFLD                    Kfgs
087800941217     C     KCAP          KLIST
087900941217     C                   KFLD                    WNAR
088000941217     C                   KFLD                    WCAP
088100941216     C     KARB          KLIST
088200941216     C                   KFLD                    VIDAAS
088300941216     C                   KFLD                    VIDLNP
088400941216     C                   KFLD                    VIDNRS
088500941216     C                   KFLD                    VIDNSP
088600941216     C     KARB2         KLIST
088700050112     C                   KFLD                    VsfAAS
088800050112     C                   KFLD                    VsfLNP
088900050112     C                   KFLD                    VsfNRS
089000050112     C                   KFLD                    VsfNSP
089100941216     C     KARB4         KLIST
089200050112     C                   KFLD                    VsfAAS
089300050112     C                   KFLD                    VsfLNP
089400050112     C                   KFLD                    VsfNRS
089500050112     C                   KFLD                    VsfNSP
089600941216     C                   KFLD                    WTRC
089700050309     C     KARB44        KLIST
089800050309     C                   KFLD                    arbAAS
089900050309     C                   KFLD                    arbLNP
090000050309     C                   KFLD                    arbNRS
090100050309     C                   KFLD                    arbNSP
090200050309     C                   KFLD                    WTRC
090300050309     C     Kblp44        KLIST
090400050309     C                   KFLD                    blpAAS
090500050309     C                   KFLD                    blpLNP
090600050309     C                   KFLD                    blpNRS
090700050309     C                   KFLD                    blpNSP
090800050309     C                   KFLD                    WTRC
090900941216     C     KTAB          KLIST
091000941216     C                   KFLD                    CODUT
091100941216     C                   KFLD                    COD               2
091200941216     C                   KFLD                    KEY               8
091300990929     C     KTAB1         KLIST
091400990929     C                   KFLD                    CODUT
091500990929     C                   KFLD                    COD               2
091600941216     C     KACO          KLIST
091700941216     C                   KFLD                    CODUT
091800941216     C                   KFLD                    KCI
091900941216     C                   KFLD                    COMKSC            7 0
092000941216     C     KTAM3         KLIST                                                  *
092100941216     C                   KFLD                    COMKSC                         *
092200941216     C                   KFLD                    COMCTR                         *
092300941216     C                   KFLD                    COMPRG                         *
092400941216     C     KTPT          KLIST                                                  *
092500941216     C                   KFLD                    COMKSC                         *
092600941216     C                   KFLD                    COMCTR                         *
092700941216     C                   KFLD                    COMPRG                         *
092800990817     C                   KFLD                    FTC               2            *
092900941216     C     KNUM          KLIST
093000941216     C                   KFLD                    DATAAS            2 0
093100941216     C                   KFLD                    RGS               1 0
093200941216     C                   KFLD                    FIV               3 0
093300020911
093400020911     c     Kblp          Klist
093500020911     c                   Kfld                    LblAao
093600020911     c                   Kfld                    LblLpo
093700020911     c                   Kfld                    LblNro
093800020911     c                   Kfld                    LblNso
093900030203
094000030203     c     kFiar5        Klist
094100050112     c                   Kfld                    VsfAas
094200050112     c                   Kfld                    VsfLnp
094300050112     c                   Kfld                    VsfNrs
094400050112     c                   Kfld                    VsfNsp
094500030203     c                   Kfld                    kAr5Trd
094600070924     c     kAR5          Klist
094700070924     c                   Kfld                    ARbAas
094800070924     c                   Kfld                    ARbLnp
094900070924     c                   Kfld                    ARbNrs
095000070924     c                   Kfld                    ARBNsp
095100070924     c                   Kfld                    kAr5Trd
095200030506
095300030506     c     kNtc          Klist
095400030506     c                   Kfld                    kNtcApl
095500030506     c                   Kfld                    kNtcNk1
095600030506     c                   Kfld                    kNtcNk2
095700030506     c                   Kfld                    kNtcTnt
095800020222
095900941216     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
096000941216     C*
096100911029     C                   Z-ADD     2             RGS
096200990817     C                   MOVEL     'C '          FTC
096300911029     C*
096400970527     C                   MOVEL     4             KNPG
096500970527     C*
096600020415     C     *LIKE         DEFINE    §CVDEC        COMDEC
096700011113     C     *LIKE         DEFINE    §CVDEC        DECSAV
096800991028     C     *LIKE         DEFINE    FVVNPG        KNPG
096900960208     C     *LIKE         DEFINE    VIDKLP        WKLP
097000960208     C     *LIKE         DEFINE    ARBQFT        VARQFT
097100950104     C     *LIKE         DEFINE    ARBIAS        VARIAS
097200941217     C     *LIKE         DEFINE    ARBVAS        WVLT
097300941217     C     *LIKE         DEFINE    ARBVAS        VARVAS
097400990930     C     *LIKE         DEFINE    ARBIAS        SAVIAS
097500990930     C     *LIKE         DEFINE    ARBVAS        SAVVAS
097600990930     C     *LIKE         DEFINE    ARBQFT        SAVQFT
097700990930     C     *LIKE         DEFINE    ARBVMD        SAVVMD
097800990930     C     *LIKE         DEFINE    ARBVAD        SAVVAD
097900990930     C     *LIKE         DEFINE    ARBKSC        KSCSAV
098000990930     C     *LIKE         DEFINE    ARBCTR        CTRSAV
098100050201     C     *LIKE         DEFINE    olv59TKS      WTKS
098200941217     C     *LIKE         DEFINE    TAMFIE        WFIE
098300941217     C     *LIKE         DEFINE    TAMPPA        WPPA
098400941217     C     *LIKE         DEFINE    TAMMPA        WMPA
098500941217 >   C     *LIKE         DEFINE    TAMCTR        COMCTR
098600941217     C     *LIKE         DEFINE    TAMPRG        COMPRG
098700941217     C     *LIKE         DEFINE    SOVR(1)       SOARBT
098800941217     C     *LIKE         DEFINE    SOVR(1)       SAARBT
098900941217     C     *LIKE         DEFINE    SOVR(1)       SDARBT
099000941216     C     *LIKE         DEFINE    AR6SV1        WSV1
099100941216     C     *LIKE         DEFINE    AR6VA1        WVA1
099200990910     C     *LIKE         DEFINE    AR6VA1        VA2VA1
099300990910     C     *LIKE         DEFINE    AR6POR        VARPOR
099400941216     C     *LIKE         DEFINE    AR4TRC        WTRC
099500941216     C     *LIKE         DEFINE    TBLKUT        §KUT
099600911028     C     *LIKE         DEFINE    TBLCOD        §COD
099700911028     C     *LIKE         DEFINE    TBLKEY        §KEY
099800941217     C     *LIKE         DEFINE    ARBNZD        WNAR
099900941217     C     *LIKE         DEFINE    ARBCAD        WCAP
100000950121     C     *LIKE         DEFINE    ARBQFT        WQFT
100100950121     C     *LIKE         DEFINE    ARBIAS        WIAS
100200950121     C     *LIKE         DEFINE    ARBVAS        WVAS
100300990923     C     *LIKE         DEFINE    AR6DIV        WDIV
100400010319     C     *LIKE         DEFINE    VIDMSG        WIDMSG
100500011107      *
100600011107     C     *LIKE         DEFINE    DSCW          WDSCW
100700011107     C                   MOVEL     *BLANKS       WESITO            1
100800011107     C                   MOVEL     *BLANKS       DATA10           10
100900011107     C                   Z-ADD     *ZEROS        DATA80            8 0
101000940223     C*
101100911028     C* GIRO UDATE
101200920108     C                   MOVE      UDATE         DATAAS            2 0
101300941216     C                   TIME                    W0140            14 0
101400941216     C* UDATE IN GGMMAAAA
101500941216     C                   MOVE      W0140         WDTGIO            8 0
101600941216     C*
101700941216     C* UDATE IN AAAAMMGG
101800941216     C                   Z-ADD     WDTGIO        G02DAT
101900941216     C                   MOVEL     *BLANK        G02ERR
102000941216     C                   CALL      'XSRDA8'
102100941216     C                   PARM                    WLBDAT
102200941216     C                   MOVEL     G02INV        DATEU             8 0
102300941216     C*
102400941216     C* VEDO SE SONO SIMFEL O REMOTO
102500941216     C*
102600050318     c                   clear                   nousa             1
102700020513     c     simtpp        ifeq      '2'
102800020513     c     simtpp        oreq      *blanks
102900020513     c                   movel     simpou        vidfil
103000941216     C                   SETON                                        17
103100941216     C                   ELSE
103200941216     C                   MOVEL     SIMFEL        VIDFIL
103300941216     C                   END
103400050318     c                   movel     knmus         w003a
103500050318     c*
103600050401     c***                if        w003a<>'EDP' and vidfil<>28 and vidfil<>6
103700050401     c***                movel     msg(69)       widmsg
103800050401     c***                eval      nousa='S'
103900050401     c***                endif
104000941217     C*
104100050317     C                   MOVEL     W256A         DSLRA2
104200050309     C* VERIFICO SE SI TRATA DI TASSAZIONE IN PARTENZA
104300050309     C                   MOVE      W256A         WTIPOTASSA        1
104400050309     C
104500050309     C                   IF        WTIPOTASSA='P'
104600050309     C                   SETON                                        94
104700050309     C                   ENDIF
104800941217     C*
104900941217     C* 11 ON  - PGM RICHIAMATO
105000050317     C     iLRA2NSP      IFGT      0
105100941217     C                   SETON                                        11
105200941217     C* SE PASSATA ANCHE FIL IN GESTIONE LA IMPOSTO
105300050317     C     ILRA2FGS      IFGT      *ZEROS
105400050317     C                   MOVEL     ILRA2FGS      VIDFIL
105500941217     C                   END
105600941217     C                   END
105700070314     c
105800070314     c                   EXSR      DECOFGS
105900070314     c
106000070314     C* CARICO tabelle
106100941217     C                   EXSR      CARTAB
106200930427     C*
106300050309     C* SE PROGRAMMA NON RICHIAMATO E NON È TASSAZIONE IN PARTENZA
106400050309     C*                             CHIEDO STAMPANTI
106500070703     C**   *IN11         IFEQ      *OFF
106600070703     C**   *IN94         ANDEQ     *OFF
106700070703     c* oppure se richiamato in arrivo ma non passata la DS stampanti
106800130320     c****               if        (*in11=*off and *in94=*off) or
106900130320     c****                         (*in94=*off and %parms=2 and d90pss=*blanks)
107000130320     c****                         or (*in94=*off and %parms<2)
107100070703     c
107200130320     C****               CLEAR                   WTrul90DS
107300941217     C*
107400130320     C****               MOVEL     'S'           wd90RSB
107500130320     C****               MOVEL     'FAT999'      wd90MDB
107600130320     C****               MOVEL     'FAT99'       wd90MDB4
107700130320     C****               MOVEL     'FAT99'       wd90MDB5
107800130320     C****               CALL      'TRUL90R'
107900130320     C****               PARM                    KPJBA
108000130320     C***                PARM                    WTrul90DS
108100941217     C**
108200941217     C* F3 - FINE
108300130320     C***  wd90F3        IFEQ      '1'
108400130320     C****               MOVEL     '1'           WFINE             1
108500130320     C****               ENDIF
108600030613      *
108700030613      * OVRPRTF BOLLE A4
108800130320     C****               MOVEA     wD90PRB4      CMA4(30)
108900130320     C****               MOVEA     wD90MDB4      CMA4(52)
109000030613     C*
109100130320     C****               Z-ADD     130           LUNG             15 5
109200130320     C****               CLEAR                   COMMAN          130
109300130320     C****               MOVEA     CMA4          COMMAN
109400130320     C****               CALL      'QCMDEXC'                            91
109500130320     C****               PARM                    COMMAN
109600130320     C****               PARM                    LUNG
109700030613      *
109800030613      * OVRPRTF BOLLE A5
109900130320     C****               MOVEA     wD90PRB5      CMA5(30)
110000130320     C****               MOVEA     wD90MDB5      CMA5(52)
110100030613     C*
110200130320     C****               Z-ADD     130           LUNG
110300130320     C****               CLEAR                   COMMAN
110400130320     C****               MOVEA     CMA5          COMMAN
110500130320     C****               CALL      'QCMDEXC'                            91
110600130320     C****               PARM                    COMMAN
110700130320     C***                PARM                    LUNG
110800941217     C*
110900130320     C****               ENDIF
111000071001      * Se ambiente prova
111100071001     c                   If        %subst(knsif:7:1) = 'P'
111200071001     c                   Eval      wlib = 'GAITRAGRPS'
111300071001     c                   Else
111400071001      * Se ambiente buono FILTRA201
111500071001     c                   Eval      wlib = 'GAITRAGRU '
111600071001     c                   EndIf
111700071001     c
111800071001     c                   Eval      lung = 80
111900170116     c                   Clear                   comman          130
112000071001     c                   Movel(p)  OVR(1)        comman
112100071001     c                   Eval      %Subst(comman:30:10) = wlib
112200071001     c                   Eval      comman =
112300151013     c                             %trim(comman) + '/FIAR531C)'
112400151013     c                   Call      'QCMDEXC'                            92
112500071001     c                   Parm                    comman
112600170116     c                   Parm                    lung             15 5
112700151013     c  n92              Open      FIAR531C
112800071001     c
112900071001     c                   Clear                   comman
113000071001     c                   Movel(p)  OVR(2)        comman
113100071001     c                   Eval      %Subst(comman:30:10) = wlib
113200071001     c                   Eval      comman =
113300071001     c                             %trim(comman) + '/TITAS30C)'
113400071001     c                   Call      'QCMDEXC'                            92
113500071001     c                   Parm                    comman
113600071001     c                   Parm                    lung
113700071001     c  n92              Open      TITAS30C
113800071001      *
113900941217     C                   MOVEL     W256A         KPJBU
114000941217     C*
114100941216     C                   ENDSR
114200070314     C*
114300070314     c* Decodifiche e caricamente della filiale in gestione ---------*
114400070314     c     DECOFGS       BEGSR
114500070314     C     VIDFIL        CHAIN     AZORG                              32
114600070314     C  N32              MOVEL     ORGDES        DESFIL
114700070314     c
114800120125     c*  permetto eventuale stampa DDT con F8
114900130320     c**n94              seton                                        03
115000070314     c
115100070314     C*
115200070314     C* CARICO LE LNA DELLA FILIALE IN GESTIONE
115300070314     C                   EXSR      CARL6
115400070314     c
115500070314     c                   ENDSR
115600920813     C*
115700920813     C* CARICO L6 ------------------------------------------***********
115800920813     C     CARL6         BEGSR
115900941216     C*
116000941216     C                   CLEAR                   DSUL06
116100941216     C                   MOVE      '£6'          D06COD
116200941217     C                   MOVEL     VIDFIL        D06KEY
116300941216     C                   MOVEL     'L'           D06TLA
116400941216     C                   MOVEL     DSUL06        KPJBU
116500941216     C                   CALL      'TRUL06R'
116600941216     C                   PARM                    KPJBA
116700941216     C                   MOVEL     KPJBU         DSUL06
116800941216     C                   MOVEA     LIN           L6
116900920813     C                   ENDSR
117000930209     C*
117100930209     C* CARICO TABELLA VARIABILI ARRIVI ------------------------------*
117200930209     C     CARTAB        BEGSR
117300941217     C                   CLEAR                   WINTER
117400941216     C*
117500990913     C                   MOVEL     '$2'          COD
117600990913     C                   MOVEL(P)  '1'           KEY
117700990913     C                   EXSR      CTRTAB
117800990913     C  N32              MOVEL     TBLUNI        DS$2
117900990913     C   32              CLEAR                   DS$2
118000990909     C**
118100990909     C* CARICO TABELLE DELLE VARIE PER CONTROLLI
118200990909     C**
118300050309     C                   Z-ADD     0             II
118400990909     C                   MOVEL     'CC'          COD
118500990929     C     KTAB1         CHAIN     TABEL                              32
118600990909    1C     *IN32         DOWEQ     *OFF
118700990909    2C     TBLFLG        IFEQ      ' '
118800990909     C                   MOVEL     TBLKEY        W005A             5
118900990909    3C     W005A         IFEQ      'VARIE'
119000990909     C                   MOVEL     TBLUNI        DSCC
119100990909     C* SIGLA VARIA
119200990909    4C     §CCFL6        IFEQ      'S'
119300990910     C                   ADD       1             II
119400990909     C                   MOVE      TBLKEY        SVA(II)
119500990909    4C                   ENDIF
119600990909    3C                   ENDIF
119700990909    2C                   ENDIF
119800990909     C**
119900990929     C     KTAB1         READE     TABEL                                  32
120000990909    1C                   ENDDO
120100991117     C* REPERIMENTO DIVISA DELLA MONETA DI CONTO
120200991117     C                   CLEAR                   DSBS02
120300991117     C                   CLEAR                   DGED
120400991117     C                   MOVEL     'C'           T02MOD
120500991117     C                   MOVEL     KNSIF         T02SIF
120600991117     C                   MOVEL     'GED'         T02COD
120700991117     C                   MOVEL     '1'           T02KE1
120800991117     C                   CALL      'TIBS02R'
120900991117     C                   PARM                    KPJBA
121000991117     C                   PARM                    DSBS02
121100991117     C     T02ERR        IFEQ      *BLANKS
121200991117     C                   MOVEL     T02UNI        DGED
121300991117     C                   ENDIF
121400011002     C* REPERIMENTO importi DELLA MONETA DI CONTO
121500011002     C                   CLEAR                   DSBS02
121600011002     C                   CLEAR                   DGEI
121700011002     C                   MOVEL     'C'           T02MOD
121800011002     C                   MOVEL     KNSIF         T02SIF
121900011002     C                   MOVEL     'GEI'         T02COD
122000011002     C                   MOVEL     §GEDCN        T02KE1
122100011002     C                   CALL      'TIBS02R'
122200011002     C                   PARM                    KPJBA
122300011002     C                   PARM                    DSBS02
122400011002     C     T02ERR        IFEQ      *BLANKS
122500011002     C                   MOVEL     T02UNI        DGEI
122600011220     c                   eval      sav_geimf = §geimf
122700011002     C                   ENDIF
122800011113      * Tabella CV della moneta di conto
122900011113     C                   CLEAR                   DSCV
123000011113     C                   MOVEL     'CV'          COD
123100011113     C                   MOVEL(P)  §GEDCN        KEY
123200011113     C                   EXSR      CTRTAB
123300011113     C     *IN32         IFEQ      *OFF
123400011113     C                   MOVEL     TBLUNI        DSCV
123500011113     C                   MOVEL     §CVDEC        DECSAV
123600011113     C                   ENDIF
123700020715
123800050309      * Carico sk con tipi bolla che prevedono l'assicurazione
123900020715
124000020715     c                   eval      cod = 'TB'
124100020715     c                   clear                   key
124200020715     c                   clear                   xx
124300050309     C                   clear                   II
124400020715     c     ktab1         setll     tabel00f
124500020715     c                   do        *hival
124600020715     c     ktab1         reade     tabel00f
124700020715      * fine file
124800020715     c                   if        %eof(tabel00f)
124900020715     c                   leave
125000020715     c                   endif
125100020715      * record annullato
125200020715     c                   if        tblflg <> *blanks
125300020715     c                   iter
125400020715     c                   endif
125500020715     c                   movel     tbluni        dstb
125600050309     c* Assicurazione
125700020715     c                   if        §tbfas = '1'
125800020715     c                   add       1             xx
125900020715     c                   eval      tbl(xx) = tblkey
126000020715     c                   endif
126100050309     c* Assegnati storno
126200050309     C                   if        §tbfcb = '0'
126300050309     C                   ADD       1             II
126400050309     C                   MOVEl     TBLKEY        TBSTO(II)
126500050309     C                   ENDIF
126600020715     c                   enddo
126700050523     c
126800050523     c* controllo riempimento skiere
126900050523     c                   clear                   trul0sds
127000050523     c                   eval      i0sski='TBSTO'
127100050523     c                   eval      i0sele=%elem(tbsto)
127200050523     c                   eval      i0spie=ii
127300050523     c                   eval      i0sfile='TABEL00F  '
127400050523     c                   eval      i0scod=' TB  '
127500050523     c                   eval      i0ssif=knsif
127600050523     c                   eval      i0spgm='FNLRA2R'
127700050523     c                   movel     trul0sds      kpjbu
127800050523     c                   call      'TRUL0SR'
127900050523     c                   parm                    kpjba
128000050523     c
128100050523     c                   clear                   trul0sds
128200050523     c                   eval      i0sski='TBL'
128300050523     c                   eval      i0sele=%elem(tbl)
128400050523     c                   eval      i0spie=xx
128500050523     c                   eval      i0sfile='TABEL00F  '
128600050523     c                   eval      i0scod=' TB  '
128700050523     c                   eval      i0ssif=knsif
128800050523     c                   eval      i0spgm='FNLRA2R'
128900050523     c                   movel     trul0sds      kpjbu
129000050523     c                   call      'TRUL0SR'
129100050523     c                   parm                    kpjba
129200050523     c
129300050309     C**
129400050309     C* CARICO codici bolla assegnato  e recuepro
129500050309     C**
129600050309     C                   Z-ADD     0             II
129700050309     C                   Z-ADD     0             ZZ
129800050309     C                   Z-ADD     0             XX
129900060117     C                   Z-ADD     0             yy                4 0
130000050309     C                   MOVEL     '3A'          COD
130100050309     C     KTAB1         CHAIN     TABEL                              32
130200050309    1C     *IN32         DOWEQ     *OFF
130300050309    2C     TBLFLG        IFEQ      ' '
130400050309     C                   MOVEL     TBLUNI        DS3a
130500050309     C                   MOVEL     §3ATB1        UNOTB1            1
130600050309     C                   MOVEL     §3ATB2        UNOTB2            1
130700050309    3C                   IF        UNOTB1='A' or UNOTB2='A'
130800050309     c* Controllo se si tratta di assegnato contabilizzabile
130900050309     c                   if        unotb1='A'
131000050309     c                   movel(P)  §3atb1        wtbl              2
131100050309     c                   else
131200050309     c                   movel(P)  §3atb2        wtbl
131300050309     c                   endif
131400050309     c
131500050309     C     wtbl          lookup    tbsto                                  34
131600050309    4C                   IF        not *in34
131700050309     C                   ADD       1             II
131800050309     C                   MOVEL     TBLKEY        cboass(II)
131900060117     c
132000050309     c* Se l'assegnato + 2 bolla, lo memorizzo a parte
132100050309     c                   if        §3atb2<>*blanks
132200050309     C                   ADD       1             zz
132300050309     C                   MOVEL     TBLKEY        cboass2(zz)
132400050309     c                   endif
132500050309     c
132600050309    4C                   ENDIF
132700050309    3C                   ENDIF
132800050309     c* cod bolla di recupero
132900050309     c                   if        §3arbl = 'R'
133000050309     c                   add       1             xx
133100050309     c                   eval      cborec(xx) = tblkey
133200050309     c                   endif
133300050309    2C                   ENDIF
133400050309     C**
133500050309     C     KTAB1         READE     TABEL                                  32
133600050309    1C                   ENDDO
133700050519     c* controllo riempimento skiere
133800050519     c                   clear                   trul0sds
133900050523     c                   eval      i0sski='CBOASS'
134000050523     c                   eval      i0sele=%elem(cboass)
134100050519     c                   eval      i0spie=ii
134200050519     c                   eval      i0sfile='TABEL00F  '
134300050519     c                   eval      i0scod=' 3A  '
134400050519     c                   eval      i0ssif=knsif
134500050519     c                   eval      i0spgm='FNLRA2R'
134600050519     c                   movel     trul0sds      kpjbu
134700050519     c                   call      'TRUL0SR'
134800050519     c                   parm                    kpjba
134900050523     c
135000050523     c                   clear                   trul0sds
135100050523     c                   eval      i0sski='CBOASS2'
135200050523     c                   eval      i0sele=%elem(cboass2)
135300050523     c                   eval      i0spie=zz
135400050523     c                   eval      i0sfile='TABEL00F  '
135500050523     c                   eval      i0scod=' 3A  '
135600050523     c                   eval      i0ssif=knsif
135700050523     c                   eval      i0spgm='FNLRA2R'
135800050523     c                   movel     trul0sds      kpjbu
135900050523     c                   call      'TRUL0SR'
136000050523     c                   parm                    kpjba
136100050523     c
136200050523     c                   clear                   trul0sds
136300050523     c                   eval      i0sski='CBOREC'
136400050523     c                   eval      i0sele=%elem(cborec)
136500050523     c                   eval      i0spie=xx
136600050523     c                   eval      i0sfile='TABEL00F  '
136700050523     c                   eval      i0scod=' 3A  '
136800050523     c                   eval      i0ssif=knsif
136900050523     c                   eval      i0spgm='FNLRA2R'
137000050523     c                   movel     trul0sds      kpjbu
137100050523     c                   call      'TRUL0SR'
137200050523     c                   parm                    kpjba
137300021121
137400021121      * Aggancio tabella default tariffe fedex
137500021121     c                   Clear                   Dsbs02
137600021121     c                   Movel     'C'           T02Mod
137700021121     c                   Movel     KNSIF         T02Sif
137800021121     c                   Movel     'DFE'         T02Cod
137900021121     c                   Movel     'FED'         T02Ke1
138000021121     c                   Call      'TIBS02R'
138100021121     c                   Parm                    Kpjba
138200021121     c                   Parm                    Dsbs02
138300021121     c                   If        T02Err = *Blanks
138400021121     c                   Movel     T02Uni        DDfe
138500021121     c                   Else
138600021121     c                   Clear                   DDfe
138700021121     c                   EndIf
138800050714     c
138900050714     c* Verifico se devo tenere conto degli assegnati dirottati per
139000050714     c*  tassare
139100050714     c
139200050714      * Aggancio tabella "VPO" per sapere se tenere conto dei dirottamenti
139300050714     c*
139400050714     C                   CLEAR                   dsbs02
139500050714     c                   clear                   dvpo
139600050714     C                   MOVEL     'C'           T02MOD
139700050714     C                   MOVEL     KNSIF         T02SIF
139800050714     C                   MOVEL     'VPO'         T02COD
139900050714     c                   movel(P)  'VPO'         t02ke1
140000050714     C                   CALL      'TIBS02R'
140100050714     C                   PARM                    KPJBA
140200050714     C                   PARM                    dsbs02
140300050714    2C                   IF        T02ERR = *BLANKS
140400050714     C                   MOVEL     T02uni        dvpo
140500050714    2C                   ENDIF
140600050714     c
140700930209     C                   ENDSR
140800921229     C*
140900050309     c* Richiamo pgm di interrogazine bolle -----------------------------*
141000050309     c     INTBOLLE      begsr
141100050309     c* F7 - richiamo per selezione
141200050309    1c                   if        *inkg
141300050309     c* In arrivo
141400161102    2c***                if        not *in94
141500161102     C****               CLEAR                   PARAM
141600161102     C***                MOVEL     '2'           PARFLG
141700161102     C*****              MOVEL     PARAM         KPJBU
141800161102     C********           CALL      'FNLR36R'
141900161102     C********           PARM                    KPJBA
142000161102     C********           MOVEL     KPJBU         PARAM
142100050309     C* SE PASSATA KEY LA IMPOSTO
142200161102     C***  VsfNSP        IFGT      0
142300161102     C***                MOVEL     VsfAAS        VIDAAS
142400161102     C****               MOVEL     VsfLNP        VIDLNP
142500161102     C****               MOVEL     VsfNRS        VIDNRS
142600161102     C****               MOVEL     VsfNSP        VIDNSP
142700161102     C****               END
142800161102   x2c*****              else
142900050309     c* in partenza
143000161102     C****               CLEAR                   PARAM3
143100161102     C****               MOVEL     '1'           PA3FLG
143200161102     C**********         MOVEL     PARAM3        KPJBU
143300161102     C*********          CALL      'FNLS04R'
143400161102     C**********         PARM                    KPJBA
143500161102     C***********        MOVEL     KPJBU         PARAM3
143600050309     C* SE PASSATA KEY LA IMPOSTO
143700161102     C***  PA3NSP        IFGT      0
143800161102     C********           MOVEL     PA3AAS        vidaas
143900161102     C************       MOVEL     PA3LNP        vidLNP
144000161102     C***********        MOVEL     PA3NRS        vidNRS
144100161102     C***********        MOVEL     PA3NSP        vidNSP
144200161102     C**********         END
144300161102    2c************       endif
144400161102     c                   clear                   paramu6ds
144500161103     c                   clear                   fnlru6ds
144600161102     c                   eval      pa1flg='2'
144700161102     c                   eval      kpjbu=paramu6ds
144800161102     c   94              eval      ILRU6SSP='N'
144900161103     c  n94              eval      ILRU6SSP='1'
145000161103     c                   eval      ilru6lna=vidfil
145100161102     c                   call      'FNLRU6R'
145200161102     c                   parm                    kpjba
145300161103     c                   parm                    fnlru6ds
145400161102     c                   movel     kpjbu         paramu6ds
145500161102     c                   if        pa1nsp>0
145600161102     C                   MOVEL     PA1AAS        vidaas
145700161102     C                   MOVEL     PA1LNP        vidLNP
145800161102     C                   MOVEL     PA1NRS        vidNRS
145900161102     C                   MOVEL     PA1NSP        vidNSP
146000161102     c                   endif
146100050309     c*
146200050309     c* Int.bolle singola
146300050309   x1c                   else
146400050309     c*
146500161102    2c****               if        not *in94
146600161102     C******             MOVEL     '1'           PARFLG
146700161102     C**********         MOVEL     PARAM         KPJBU
146800161102     C************       CALL      'FNLR36R'
146900161102     C***********        PARM                    KPJBA
147000161102   x2c********           else
147100161102     c********           clear                   param3
147200161102     C********           MOVEL     VsfAAS        PA3AAS
147300161102     C********           MOVEL     VsfLnp        PA3LNP
147400161102     C********           MOVEL     VsfNRS        PA3NRS
147500161102     C********           MOVEL     VsfNSP        PA3NSP
147600161102     C********           MOVEL     ' '           PA3FL2
147700161102     C********           MOVEL     Viddsa        PA3RSU
147800161102     C********           MOVEL     PARAM3        KPJBU
147900161102     C********           CALL      'FNLS05R'
148000161102     C********           PARM                    KPJBA
148100161102    2c********           endif
148200161102     c                   clear                   paramu6ds1
148300161102     c                   eval      pa1aas_=vsfaas
148400161102     c                   eval      pa1lnp_=vsflnp
148500161102     c                   eval      pa1nrs_=vsfnrs
148600161102     c                   eval      pa1nsp_=vsfnsp
148700161103     c                   eval      kpjbu=paramu6ds1
148800161102     c                   call      'FNLRU6R1'
148900161102     c                   parm                    kpjba
149000050309    1c                   endif
149100050309     c                   ENDSR
149200050309     c
149300900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
149400900509     C     CONTR         BEGSR
149500941216     C                   SETOFF                                       9030
149600941216     C* P E R   D A T A   S P E D I Z I O N E
149700941216    1C     SEL1          IFNE      0
149800050309     c* In arrivo
149900050309    2c                   if        not *in94
150000941216     C*
150100050309     C     KARB          CHAIN     FNARB000                           3034
150200050309     C* inesistente
150300911127     C   30              SETON                                        4190
150400911127     C   30              GOTO      ENDCTR
150500050309     c* Allocata da un altro lavoro
150600050309     C   34              SETON                                        4690
150700050309     C   34              GOTO      ENDCTR
150800911029     C*
150900050309     C* GIA' TASSATA
151000050309     c                   if        not *in11
151100050309     C     ARBACA        IFEQ      'S'
151200911127     C                   SETON                                        4090
151300911127     C                   GOTO      ENDCTR
151400911028     C                   END
151500911028     C*
151600050309     C* SPEDIZIONE CON LINEA ARRIVO NON gestita
151700911028     C     ARBLNA        LOOKUP    L6                                     30
151800911028     C  N30              SETON                                        4390
151900911028     C  N30              GOTO      ENDCTR
152000911217     C                   END
152100050309     c*
152200050309   x2c                   else
152300050309     c* In partenza
152400050309     C*
152500050309     C     Karb          CHAIN(n)  FNBLP000                           30
152600050309     C* inesistente
152700050309     C   30              SETON                                        4190
152800050309     C   30              GOTO      ENDCTR
152900161102     c* Bolla già partita: devo testare FT1 e non FT2 perchè la bolla scritta sia in blp che
153000161102     c*  in ARB ha FT1  flaggato ma non FT2 perchè ancora da trasmettere in sede
153100161102     c                   if        blpft1<>' '
153200050311     C                   SETON                                        4790
153300050311     C                   GOTO      ENDCTR
153400050311    3C                   ENDIF
153500050309     C*
153600050309     C* CONTROLLO SE SPEDIZIONE IN ASSEGNATO contabilizzabile
153700050309     c     blpcbo        lookup    cboass                                 34
153800050309    3c                   if        not *in34
153900050309     C                   SETON                                        4490
154000050309     C                   GOTO      ENDCTR
154100050309    3C                   ENDIF
154200050309     C
154300050309     c* chaino record di tassazione
154400050309     c     blpcbo        lookup    cboass2                                34
154500050309    3c                   if        *in34
154600050309     C                   MOVEL     '2'           WTRC
154700050309     c                   else
154800050309     C                   MOVEL     '1'           WTRC
154900050309    3c                   endif
155000050309     C     Kblp44        CHAIN(N)  FIAR6000                           38
155100050309    3c                   if        not *in38
155200050309     c* Se presenti importi di tassazione: non tassabile in partenza
155300050309    4c                   if        ar6por>0 or ar6sv1<>*blanks or
155400050309     c                             ar6cei<>*blanks
155500050309     C                   SETON                                        4590
155600050309     C                   GOTO      ENDCTR
155700050309    4c                   endif
155800050309     c
155900050309     C* GIA' TASSATA  se presete codice S.F.
156000050309     c                   move      ar6ksc        wfineksc          4
156100050309    4C                   if        wfineksc<>'9999'
156200050309     C                   SETON                                        4090
156300050309     C                   GOTO      ENDCTR
156400050309    4C                   END
156500050309    3C                   END
156600050309     C*
156700050309    3C  N11              DO
156800050309     C* SPEDIZIONE CON LINEA ARRIVO NON ABILITATA
156900050309     C     blpLNA        LOOKUP    L6                                     30
157000050309     C  N30              SETON                                        4390
157100050309     C  N30              GOTO      ENDCTR
157200050309    3C                   END
157300050309     c*
157400050309     c* Per tassazione in partenza aggancio fnblp00f in update
157500050309     c*  che ha i campi ridenominati come arb
157600050309     c     blp01nrr      chain     fnblpfis                             30
157700050309     c                   if        *in30
157800050309     c                   seton                                        9046
157900050309     c                   endif
158000050309    2c                   endif
158100941216     C*
158200941216   X1C                   ELSE
158300911028     C*
158400941216     C* T U T T E   S P E D I Z I O N I
158500911028     C     VIDLP2        IFNE      0
158600911028     C     VIDLP2        CHAIN     AZORG                              30
158700911028     C* LINEA ANNULLATA
158800911028     C  N30ORGFVA        IFNE      *BLANKS
158900911028     C                   SETON                                        30
159000911028     C                   END
159100911028     C* NON E' FILIALE O AGENZIA
159200911028     C  N30ORGFAG        IFNE      'A'
159300911127     C     ORGFAG        ANDNE     'F'
159400911028     C                   SETON                                        30
159500911028     C                   END
159600911028     C* ERRORE
159700911028     C   30              SETON                                        4290
159800911028     C   30              GOTO      ENDCTR
159900911028     C                   END
160000911028     C*
160100941216    1C                   END
160200911023     C*
160300900514     C     ENDCTR        ENDSR
160400050216     c*
160500050216     c* Operazioni per caricamento sfl ----------------------------------*
160600050216     c     CARICAbolle   begsr
160700050216     c* Pulizia sfl
160800050216     C                   EXSR      PULSF
160900050309      *----
161000050309      * controllo se immessa ragione sociale destinatario
161100050309      *----
161200050309     c                   if        vidrsd<>*blanks
161300050309     c     ' '           checkr    vidrsd        wposizrsd         2 0
161400050309     c                   endif
161500050216     c
161600050216     C                   Z-ADD     0             NRRS2             4 0
161700050216     C                   Z-ADD     0             NRRC2             4 0
161800050216     C                   Z-ADD     0             NRR               4 0
161900050216     c                   Clear                   wsflpieno2
162000050216     c                   Clear                   wsflpieno22
162100050309     c                   clear                   sksflcon
162200050309     c                   clear                   sksflsen
162300050311     C                   Z-ADD     1             savRECc2
162400050311     C                   Z-ADD     1             savRECs2
162500050309     C** CARICO SUBFILE in ARRIVO
162600050216     C                   SETOFF                                       302021
162700050216     C                   Z-ADD     L6(1)         COMFIL
162800050216     C                   Z-ADD     1             D                 3 0
162900050309     C
163000050309     C                   IF        NOT *IN94
163100050216     C     COMFIL        SETLL     FNARB007
163200050309     C                   EXSR      CARSFARR
163300050309     C                   ELSE
163400050309     C     COMFIL        SETLL     FNBLP007
163500050309     C                   EXSR      CARSFPART
163600050309     C                   ENDIF
163700050216     c
163800050216     c                   if        nrrs2=0
163900050216     c                   seton                                        20
164000050216     c                   endif
164100050216     c                   if        NRRC2=0
164200050216     c                   seton                                        21
164300050216     c                   endif
164400050216     c                   endsr
164500900515     C*
164600050309     C*--- CARICO SUBFILE IN ARRIVO ----------------------------------*
164700050309     C     CARSFARR      BEGSR
164800911028     C*
164900050110    0C                   DOW       not *in30
165000911028     C                   SETON                                            31
165100950807    1C     *IN31         DOWEQ     *ON
165200050110     C     COMFIL        READE     FNARB007                               31
165300911028     C*
165400911029     C   31              ADD       1             D
165500950807    2C   31L6(D)         IFNE      0
165600941217     C                   Z-ADD     L6(D)         COMFIL            3 0
165700050110     C     COMFIL        SETLL     FNARB007
165800950807   X2C                   ELSE
165900911028     C                   SETON                                        30
166000911028     C                   SETOFF                                       31
166100950807    2C                   ENDIF
166200950807    1C                   ENDDO
166300911024     C*
166400050110     c* solo se non ho finito di leggere tutte le bolle per la £6
166500050110     c                   if        not *in30
166600911203     C                   SETOFF                                       3233
166700911203     C                   MOVEL     ' '           ABB               1
166800050110      *----
166900050110     C* FLAG ASSEGNATO = BLANKS
167000050110      *----
167100050110    1C     ARBACA        IFNE      *BLANKS
167200911203     C                   SETON                                        32
167300950807    1C                   END
167400050110      *----
167500050110      * Ragione sociale destinatario
167600050110      *----
167700050110     c                   if        vidrsd<>*blanks
167800050110     C     wposizrsd     SUBST(P)  ARBRSD        COMRAG           15
167900050110      * Se rag.soc. letta > video riposiziono solo per data con SETGT
168000050111     C     COMRAG        IFne      VidRSD
168100050110     c                   seton                                        32
168200050110     c                   endif
168300050110     c                   endif
168400020703      *----
168500020703      * BOLLE DI RECUPERO
168600020703      *----
168700050110     c                   if        not *in32
168800020703     c                             and vidbrc <> 'N'
168900050309     c     arbcbo        lookup    cborec                                 33
169000020703      * E - SOLO NON RECUPERO
169100020703     c                   if        *in33 and vidbrc = 'E'
169200020703     c                   eval      *in32 = *on
169300020703     c                   endif
169400020703      * S - SOLO RECUPERO
169500020703     c                   if        not *in33 and vidbrc = 'S'
169600020703     c                   eval      *in32 = *on
169700020703     c                   endif
169800020703     c                   endif
169900050110     C*----
170000911203     C* BOLLE DI RIENTRO
170100050110     C*----
170200050110     C
170300050110    1C  N32VIDBRN        IFNE      'N'
170400911203     C* E - SOLO LE NON DI RIENTRO
170500950807    2C     VIDBRN        IFEQ      'E'
170600961112    3C     ARBFBR        IFEQ      'S'
170700911203     C                   SETON                                        32
170800950807    3C                   END
170900911203     C*
171000950807   X2C                   ELSE
171100911203     C* S - SOLO DI RIENTRO
171200961112    3C     ARBFBR        IFNE      'S'
171300911203     C                   SETON                                        32
171400950807    3C                   END
171500950807    2C                   END
171600950807    1C                   END
171700050110     C*----
171800911203     C* BOLLE ABBINATE
171900050110     C*----
172000050110     C
172100050110    1C  N32VIDBAB        IFNE      'N'
172200940620     C* VEDO SE BOLLA DI FV ABBINATO:
172300060123     C**** ARBLNP        LOOKUP    L1                                     99
172400060123     c
172500060123     c* Vedo se locale
172600060123     c                   if        arbtfp=arbtfa  or
172700060123     c                             arblnp=arblna
172800060123     c                   seton                                        99
172900060123     c                   else
173000060123     c                   setoff                                       99
173100060123     c                   endif
173200940620     C* 1) DAL FLAG ABILITAZIONE ARRIVI(ARBAMA=3)
173300950807    2C                   SELECT
173400950807     C     ARBAMA        WHENEQ    3
173500911205     C                   MOVEL     'A'           ABB
173600911205     C*
173700950807     C* 2) DAL FLAG ABILITAZIONE PARTENZE (ARBAMP=1)
173800950807     C     ARBAMA        WHENEQ    1
173900950807     C     ARBAMP        ANDEQ     1
174000911203     C                   MOVEL     'A'           ABB
174100950807     C*
174200950807     C* 3) SE LOCALE SEMPRE ABBINATO
174300950807     C     *IN99         WHENEQ    *ON
174400950807     C                   MOVEL     'A'           ABB
174500940620     C*
174600950807   X2C                   OTHER
174700971117     C* REPERISCO INFORMAZIONE SE FOGLIO E' ABBINATO O MENO
174800971117     C                   CLEAR                   DSLV53
174900971117     C                   MOVEL     'A'           D53TFO
175000971117     C                   MOVEL     ARBNFV        D53NFV
175100111130     C                   MOVEL     ARBtfp        D53FGS
175200971117     C                   MOVEL     1             D53NPG
175300040108     C                   MOVEL     'B'           D53ABB
175400971117     C                   MOVEL     ARBLNA        D53LNA
175500971117     C                   MOVEL     ARBTFA        D53TFA
175600971117     C                   CALL      'FNLV53R'
175700971117     C                   PARM                    DSLV53
175800971117     C     D53ERR        IFEQ      ' '
175900971117     C     D53NFA        ANDGT     0
176000971117     C                   MOVEL     'A'           ABB
176100971117     C                   ENDIF
176200921217     C*
176300950807    2C                   ENDSL
176400911203     C* S - SOLO ABBINATE
176500950807    2C     VIDBAB        IFEQ      'S'
176600950807    3C     ABB           IFNE      'A'
176700911203     C                   SETON                                        32
176800950807    3C                   END
176900911203     C*
177000950807   X2C                   ELSE
177100911203     C* E - SOLO NON ABBINATE
177200950807    3C     ABB           IFNE      ' '
177300911203     C                   SETON                                        32
177400950807    3C                   ENDIF
177500950807    2C                   ENDIF
177600950807    1C                   ENDIF
177700050110     C*----
177800911203     C* LINEA DI PARTENZA
177900050110     C*----
178000950807    1C     VIDLP2        IFNE      0
178100911203     C     VIDLP2        ANDNE     ARBLNP
178200911203     C                   SETON                                        32
178300950807    1C                   ENDIF
178400911203     C*
178500050110     C  N32              EXSR      RIEMPI                                        CARICO BOLL
178600050110     c                   endif
178700911025     C*
178800050110    0C                   ENDDO
178900900515     C                   ENDSR
179000050309     C*
179100050309     C*--- CARICO SUBFILE IN PARTENZA --------------------------------*
179200050309     C     CARSFPART     BEGSR
179300050309     C*
179400050309    0C                   DOW       not *in30
179500050309     C                   SETON                                            31
179600050309    1C     *IN31         DOWEQ     *ON
179700050309     C     COMFIL        READE     FNblp007                               31
179800050309     C*
179900050309     C   31              ADD       1             D
180000050309    2C   31L6(D)         IFNE      0
180100050309     C                   Z-ADD     L6(D)         COMFIL            3 0
180200050309     C     COMFIL        SETLL     FNblp007
180300050309   X2C                   ELSE
180400050309     C                   SETON                                        30
180500050309     C                   SETOFF                                       31
180600050309    2C                   ENDIF
180700050309    1C                   ENDDO
180800050309     C*
180900050309     c* solo se non ho finito di leggere tutte le bolle per la £6
181000050309     c                   if        not *in30
181100050314     c                   setoff                                       3233
181200050314     c                   exsr      SCARTAPAR
181300050314     c                   if        wscarta='S'
181400050314     c                   seton                                        32
181500050314     c                   endif
181600050309      *----
181700050309      * Ragione sociale destinatario
181800050309      *----
181900050309     c  n32              if        vidrsd<>*blanks
182000050309     C     wposizrsd     SUBST(P)  ARBRSD        COMRAG           15
182100050309      * Se rag.soc. letta > video riposiziono solo per data con SETGT
182200050309     C     COMRAG        IFne      VidRSD
182300050309     c                   seton                                        32
182400050309     c                   endif
182500050309     c                   endif
182600050309      *----
182700050309      * BOLLE DI RECUPERO
182800050309      *----
182900050309     c                   if        not *in32
183000050309     c                             and vidbrc <> 'N'
183100050309     c     ARBcbo        lookup    cborec                                 33
183200050309      * E - SOLO NON RECUPERO
183300050309     c                   if        *in33 and vidbrc = 'E'
183400050309     c                   eval      *in32 = *on
183500050309     c                   endif
183600050309      * S - SOLO RECUPERO
183700050309     c                   if        not *in33 and vidbrc = 'S'
183800050309     c                   eval      *in32 = *on
183900050309     c                   endif
184000050309     c                   endif
184100050309     C*----
184200050309     C* BOLLE DI RIENTRO
184300050309     C*----
184400050309     C
184500050309    1C  N32VIDBRN        IFNE      'N'
184600050309     C* E - SOLO LE NON DI RIENTRO
184700050309    2C     VIDBRN        IFEQ      'E'
184800050309    3C     ARBFBR        IFEQ      'S'
184900050309     C                   SETON                                        32
185000050309    3C                   END
185100050309     C*
185200050309   X2C                   ELSE
185300050309     C* S - SOLO DI RIENTRO
185400050309    3C     ARBFBR        IFNE      'S'
185500050309     C                   SETON                                        32
185600050309    3C                   END
185700050309    2C                   END
185800050309    1C                   END
185900050309     C*----
186000050309     C* LINEA DI PARTENZA
186100050309     C*----
186200050309    1C     VIDLP2        IFNE      0
186300050309     C     VIDLP2        ANDNE     ARBLNP
186400050309     C                   SETON                                        32
186500050309    1C                   ENDIF
186600050309     C*
186700050309     C  N32              EXSR      RIEMPI
186800050309     c                   endif
186900050309     C*
187000050309    0C                   ENDDO
187100050309     C                   ENDSR
187200050314     c*
187300050314     c* Scarto di bolle specifico per partenza ---------------------------*
187400050314     c     SCARTAPAR     BEGSR
187500050314     C                   clear                   wscarta
187600050314      *----
187700050314     C* scarto bolle franco o assegnati storno o assegnati già tassati
187800050314      *----
187900050314    1C     ARBcbo        lookup    cboass                                 34
188000050314     c                   if        not *in34
188100050314     C                   eval      wscarta='S'
188200050314    1C                   END
188300050314     c* chaino record di tassazione
188400050314     c     ARBcbo        lookup    cboass2                                34
188500050314     c                   if        *in34
188600050314     C                   MOVEL     '2'           WTRC
188700050314     c                   else
188800050314     C                   MOVEL     '1'           WTRC
188900050314     c                   endif
189000050314     C     Karb44        CHAIN(N)  FIAR6000
189100050314     c                   if        %found(fiar601l)
189200050314     c* Se presente AR6 non eseguo tassazione
189300050314     C                   eval      wscarta='S'
189400050314     C                   END
189500050321     c* Se nel frattempo è "partita", non carico
189600050321     c                   if        arbft2<>' '
189700050314     C                   eval      wscarta='S'
189800050314     C                   END
189900050314     c                   endsr
190000900515     C*
190100900515     C* EMISSIONE SUBFILE -------------------------------------------
190200911025     C     RIEMPI        BEGSR
190300050112     C                   MOVEL     *BLANKS       VsfSCE
190400050318     C                   MOVEL     *BLANKS       Vsfdop
190500050113     C                   MOVEL     *BLANKS       Vsfaks
190600050113     C                   MOVEL     *BLANKS       Vsfact
190700050113     C                   MOVEL     *BLANKS       Vs2aks
190800050124     C                   MOVEL     *BLANKS       Vs2act
190900050310     C   94              z-add     blp07nrr      Vsfnrr
191000050112     C                   MOVEL     ARBLNP        VsfLNP
191100050112     C                   MOVEL     ARBLNA        VsfLNA
191200911023     C                   MOVEL     ARBMGS        MMGG
191300050112     C                   MOVEL     GG            VsfDSP
191400050112     C                   MOVE      MM            VsfDSP
191500050112     C                   MOVE      ARBAAS        VsfAAS
191600900516     C*
191700050112     C                   MOVEL     ARBNSP        VsfNSP
191800050112     C                   MOVEL     ARBind        Vsfind
191900050112     c                   if        arbnzd<>*blanks
192000050112     C                   MOVEL     ARBnzd        Vsfprd
192100050112     c                   else
192200050112     C                   MOVEL(P)  ARBprd        Vsfprd
192300050112     c                   endif
192400050112     C                   MOVEL     ARBRSM        VsfRSM
192500050112     C                   MOVEL     ARBNRS        VsfNRS
192600050112     C                   MOVEL     ARBtsp        Vsftsp
192700050112     c* Campi di lunghezza diversa nei 2 sfl
192800050112     C                   MOVEL     ARBRSD        Vs2RSD
192900050112     C                   MOVEL     ARBlod        Vs2lod
193000050112     C                   MOVEL     ARBRSD        Vc2RSD
193100050112     C                   MOVEL     ARBlod        Vc2lod
193200050110     c
193300050110     c* controllo se con codice o con "*" o senza codice
193400050110     C                   MOVEL     'A'           WTRC
193500060217     C     KARB4         CHAIN(N)  Fiar401L                           32
193600050110     C  N32              MOVEL     AR4NOT        DSBL4A
193700050110     C   32              CLEAR                   DSBL4A
193800911113     C* COD BOLLA
193900911113     C                   MOVEL     '3A'          COD
194000911113     C                   MOVEL     *BLANKS       KEY
194100941216     C                   MOVEL     ARBCBO        KEY
194200911113     C     KTAB          CHAIN     TABEL                              34
194300911113     C  N34              MOVEL     TBLUNI        DS3A
194400911113     C                   MOVEL     §3ATB1        UNOTB1            1
194500050203     C                   MOVEL     §3Asva        vsfsva
194600051024     c
194700051024     c* Memorizzo se esiste importo da assicurare solo per tipi bolla che
194800051024     c*  lo prevedono solo per 1°bolla
194900051024     c     §3atb1        lookup    tbl                                    34
195000051024     c                   clear                   vsfesiias
195100051024     c                   if        *in34 and unotb1='A' and arbcbo<>'FW'
195200051024     c                   if        arbias>0
195300051024     c                   eval      vsfesiias='S'
195400051024     c                   else
195500051024     c                   eval      vsfesiias='N'
195600051024     c                   endif
195700051024     c                   endif
195800930430     C* TIPO BOLLA
195900911113     C     UNOTB1        IFEQ      'A'
196000050112     C                   MOVEL     §3ATB1        VsfTBL
196100930430     C*
196200911113     C                   ELSE
196300930430     C*
196400050112     C                   MOVEL     §3ATB2        VsfTBL
196500050318     C*
196600050318     c                   if        §3atb2<>*blanks
196700050318     c                   eval      vsfdop='S'
196800050318     c                   endif
196900941216     C*
197000930430     C* SE NON 'C'E 2 BOLLA E DEVO TASSARE ASSEGNATI ABILITO SUBITO
197100050309     C  N94§3ATB2        IFEQ      '  '
197200941217     C     KARB2         CHAIN     FNARB000                           34
197300930430     C  N34              MOVEL     'S'           ARBACA
197400941217     C  N34              UPDATE    FNARB000
197500930430     C                   END
197600911113     C                   END
197700941216     C* A S S E G N A T O
197800941216     C*
197900050309     C***  ARBACA        IFEQ      ' '
198000050309     C                   IF        NOT *IN94 AND ARBACA=' '  OR *IN94
198100050309     C
198200961112     C     ARBFBR        IFEQ      'S'
198300050112     C                   eval      vsffbr='Rientro'                              RIENTRO
198400961112     C                   ELSE
198500050112     C                   CLEAR                   VsfFBR
198600961112     C                   ENDIF
198700941216     C* PRENDO LE NOTE
198800941216     C                   MOVEL     '8'           WTRC
198900060217     C     KARB4         CHAIN(n)  FiAR4000                           32
199000050309     C
199100050112     C  N32              MOVEL     AR4NOT        VsfNOT
199200050112     C   32              CLEAR                   VsfNOT
199300050112     c* codice in bl4a
199400050112     c                   if        §b4aks>0
199500050112     c                   movel     §b4aks        vs2aks
199600050112     c                   movel     §b4aks        vsfaks
199700050113     c                   movel     §b4act        vsfact
199800050124     c                   movel     §b4act        vs2act
199900050112     c                   movel     arblod        w012a            12
200000050112     c                   eval      vc2lod=(w012a+' '+vs2aks)
200100050112     c                   endif
200200050714     c* Vedo se bolla dirottata
200300050714     c                   EXSR      CHIAMAlv58
200400941216     C*
200500050110     c* sfl con codice
200600050202     c                   if        §b4aks>0 and §b4af1<>'S'
200700050309     c                   If        NRRC2>= 9990
200800050110     c                   Eval      wsflpieno22= '1'
200900050309     C                   ELSE
201000050309     C                   ADD       1             NRRC2
201100050317     C                   WRITE     LRA2SC2
201200050309     c                   EndIf
201300050110     c
201400050110     c                   else
201500050110     c* sfl senza codice
201600050125     c* se con codice , metto in HI
201700050125     c                   if        vsfaks<>*blanks
201800050125     c                   seton                                        13
201900050125     c                   endif
202000050125     c
202100050309     c                   If        NRRS2>= 9990
202200050110     c                   Eval      wsflpieno2= '1'
202300050309     C                   ELSE
202400050309     C                   ADD       1             NRRS2
202500050317     C                   WRITE     LRA2SS2
202600050309     c                   EndIf
202700050125     c
202800050125     c                   setoff                                       13
202900050110     c                   endif
203000050110     c
203100941216     C                   ENDIF
203200930430     C*
203300900515     C                   ENDSR
203400050714     c
203500050714     c*
203600050714     c* Richiamo a pgmm di controllo bolla dirottata---------------------*
203700050714     c     CHIAMAlv58    BEGSR
203800050714     c* Imposto i campi come quelli della bolla,
203900050714     c                   eval      VUNICOlnp=vsflnp
204000050714     c                   eval      VUNICOtfp=arbtfp
204100050714     c                   eval      VUNICOnzm=arbnzm
204200050714     c                   eval      VUNICOcam=arbcam
204300050714     c                   eval      VUNICOfap=arbfap
204400060123     c                   eval      VUNICOfdn=arbfdn
204500050714     c* Rimane vuoto il cod tassazione mittente perchè non c'è in bolla
204600050714     c                   clear                   VUNICOmct
204700050715     c                   clear                   Vsfdir
204800050714     c**
204900050714     c**
205000050714     c*  Se devo considerare i dirottamenti e c'e' legame,
205100050714     c*  allora li imposto = alla bolla mamma
205200050715    1c                   if        §vpodir='A' or §vpodir='E'
205300050714     c
205400050714     c                   clear                   fnlv58ds
205500050714     c                   eval      ilv58tle='1'
205600050714     c                   eval      ilv58aas=arbaas
205700050714     c                   eval      ilv58lnp=arblnp
205800050714     c                   eval      ilv58nrs=arbnrs
205900050714     c                   eval      ilv58nsp=arbnsp
206000050714     c                   eval      ilv58por=vsftbl
206100050714     c                   call      'FNLV58R'
206200050714     c                   parm                    fnlv58ds
206300050714     c
206400050926    2c                   if        olv58nsp>0  and (olv58oumfi='O ' or
206500050926     c                             olv58oumfi='MP')
206600050715     c                   eval      vsfdir='S'
206700060117     c                   eval      v1dleg=C_Dirotta
206800060117     c*
206900060117   x2c                   else
207000060117     c* Verifico se cambio porto
207100060117     c                   eval      ilv58tle='9'
207200060117     c                   call      'FNLV58R'
207300060117     c                   parm                    fnlv58ds
207400060117     c
207500060117    3c                   if        olv58nsp>0  and (olv58oumfi='O ' or
207600060117     c                             olv58oumfi='MP')
207700060117     c                   eval      vsfdir='C'
207800060117     c                   eval      v1dleg=C_Cambio
207900060117     c* Se la bolla trovata non è l'originale, potrebbe essere una dirotta
208000060117     c*  ta per cui tasso dall'originale
208100060117    4c                   if        olv58nsp>0  and olv58oumfi='MP'
208200060117     c                   eval      ilv58tle=' '
208300060117     c                   call      'FNLV58R'
208400060117     c                   parm                    fnlv58ds
208500060117    4c                   endif
208600060123   x3c                   else
208700060123     c* Verifico se bolla legata da giacenza
208800060123     c                   eval      ilv58tle='G'
208900060123     c                   call      'FNLV58R'
209000060123     c                   parm                    fnlv58ds
209100060123     c
209200060123    4c                   if        olv58nsp>0  and (olv58oumfi='O ' or
209300060123     c                             olv58oumfi='MP')
209400060123     c                   eval      vsfdir='G'
209500060123     c                   eval      v1dleg=C_Legame
209600060123     c* Se la bolla trovata non è l'originale, potrebbe essere una dirotta
209700060123     c*  ta per cui cerco l'eventuale mamma dirottata
209800060123    5c                   if        olv58nsp>0  and olv58oumfi='MP'
209900060123     c                   movel     fnlv58ds      wfnlv58ds
210000060123     c                   clear                   fnlv58ds
210100060123     c                   eval      ilv58tle='1'
210200060123     c                   eval      ilv58aas=wolv58aas
210300060123     c                   eval      ilv58lnp=wolv58lnp
210400060123     c                   eval      ilv58nrs=wolv58nrs
210500060123     c                   eval      ilv58nsp=wolv58nsp
210600060123     c                   call      'FNLV58R'
210700060123     c                   parm                    fnlv58ds
210800060123    6c                   if        olv58nsp>0  and (olv58oumfi='O ' or
210900060123     c                             olv58oumfi='MP')
211000060123   x6c                   else
211100060123     c* se non trovato nulla reimposto i dati nella DS
211200060123     c                   movel     wfnlv58ds     fnlv58ds
211300060123    6c                   endif
211400060123    5c                   endif
211500060123    4c                   endif
211600060123    3c                   endif
211700060117    2c                   endif
211800060117     c
211900060117    2c                   if        vsfdir<>' '
212000050715     c                   eval      VUNICOlnp=olv58lnp
212100050714     c                   eval      VUNICOtfp=olv58tfp
212200050714     c                   eval      VUNICOmct=olv58mct
212300050714     c                   eval      VUNICOfap=olv58fap
212400050714     c                   eval      VUNICOcam=olv58cam
212500050714     c                   eval      VUNICOnzm=olv58nzm
212600060123     c                   eval      VUNICOfdn=olv58fdn
212700050715    2c                   endif
212800050715    1c                   endif
212900050714     c
213000050714     c                   ENDSR
213100900515     C*
213200900515     C* PULIZIA SUBFILE ---------------------------------------------
213300900515     C     PULSF         BEGSR
213400050110     C                   SETON                                        352021
213500050317     C                   WRITE     LRA2CC2
213600050317     C                   WRITE     LRA2CS2
213700050110     C                   SETOFF                                       352021
213800900515     C                   ENDSR
213900911025     C*
214000050110     C* GESTIONE sfl assegnati CON codice ---------------------------*
214100050110     c     SFLCON        BEGSR
214200050216     c* Flag che mi dice se ho tassato qualche cosa per ricaricare i sfl
214300050216     c                   clear                   whotassato        1
214400050110     C
214500050110      * se i record scritti nel sfl degli assegnati supera i 9900 emetto msg di errore
214600050110If  1c                   If        wsflpieno22<> *Blanks
214700050110     c                   Eval      *In18 = *On
214800050110     c                   Eval      *In28 = *On
214900050110     c                   Eval      VidMsg = Msg(59)
215000050317     c     NRRC2         Chain     LRA2SC2                            34
215100050110     c                   Eval      *In36 = *On
215200050317     c                   Update    LRA2SC2
215300050110     c                   Eval      *In36 = *Off
215400050110     c                   Eval      *In18 = *Off
215500050110    1c                   EndIf
215600050127     c
215700050127     c                   if        savrecc2>0
215800050114     c                   z-add     savrecc2      rec
215900050127     c                   else
216000050127     c                   z-add     1             rec
216100050127     c                   endif
216200050110     C
216300050112     C     FORCTC2       TAG
216400050317     C                   WRITE     LRA2ZC2
216500050317     C   21              WRITE     LRA2Dc2
216600050317     C                   EXFMT     LRA2CC2
216700050110     C**
216800050110     C                   SETOFF                                       2890
216900050110     C* F3 - FINE
217000050110     C   KC              GOTO      ENDSFLCON
217100050110     C**
217200050110     C* F12 - RITORNO
217300050110     C   KL              GOTO      ENDSFLCON
217400050111     c* F8 - Assegnati senza codice
217500050111     c                   if        *inkh
217600050114     C                   Z-ADD     PRIMA         SAVRECc2          4 0
217700050110     c                   eval      wtiposfl='S'
217800050110     c                   goto      endsflcon
217900050110     c                   endif
218000061122     c
218100061123     c* Se sfl vuoto, permetto solo F8, F12, F3
218200061123     c                   if        *in21
218300061123     c                   goto      forctc2
218400061123     c                   endif
218500050113     c* Leggo in readc il sfl per caricarmi la schiera delle opzioni
218600050317     c                   readc     LRA2sc2                                34
218700050113     c                   dow       not *in34
218800050113     c                   eval      sksflcon(nrrc2)=vsfsce
218900050126     c* se scelta a ' ' --> aggiorno sfl spegnendo indicatori di posiz e RI
219000050126     c                   if        vsfsce=' '
219100050126     c                   setoff                                       1816
219200050317     c                   update    LRA2sc2
219300050126     c                   endif
219400050317     c                   readc     LRA2sc2                                34
219500050113     c                   enddo
219600050127     c
219700050127     C* SALVO IL POSIZIONAMENTO CURSORE
219800050127     c                   if        prima=0
219900050127     c                   z-add     nrrc2         savrecc2
220000050127     c                   else
220100050127     C                   Z-ADD     PRIMA         SAVRECc2          4 0
220200050127     c                   endif
220300050113     C*
220400050113     C* SE PREMUto F1 impossibile impostare opzioni
220500050113    2c                   if        *inka
220600050113     C                   MOVEA     SKSFLCON      skiopz
220700050113     c                   sorta     skiopz
220800050113     c
220900050113     c                   if        skiopz(9902)<>*blanks
221000050113     C                   MOVEL     MSG(62)       VIDMSG
221100050113     C                   SETON                                        2890
221200050113     c                   goto      forctc2
221300050113     C                   ENDIF
221400050121     c
221500050121     c* F1 - conferma totale leggo tutto il sfl e elaboro con 'X'
221600050121     c                   exsr      CONFTOTALE
221700050121     c
221800050121     c                   exsr      SFLERROR
221900050121     c
222000050121     C  N90              Z-ADD     SAVRECc2      REC
222100050121     C                   GOTO      FORCTC2
222200050121     C                   ENDIF
222300050110     C**
222400050113     c                   movea     sksflcon      skiopz
222500050112     c
222600050112     C* LETTURA SUBFILE  : opzione 5 - interrogazione
222700050112     c                   exsr      Opzione5
222800050114     C**
222900050114     C* LETTURA SUBFILE  : opzione 1 - singola
223000050114     c                   exsr      Opzione1
223100050120     C**
223200050120     C* LETTURA SUBFILE  : opzione X - singola senza far vedere le videate
223300050120     c                   exsr      OpzioneX
223400050110     C**
223500050112     C* LETTURA SUBFILE  : opzione 9 - tassazione multipla
223600050112     c                   exsr      Opzione9
223700050113     c                   movea     skiopz        sksflcon
223800050112     c   90              goto      forctc2
223900050110     C*
224000050110     C* ERRORE
224100050120     c                   exsr      SFLERROR
224200050110     C*
224300050113     C  N90              Z-ADD     SAVRECc2      REC
224400050112     C                   GOTO      FORCTC2
224500050110     C     endsflcon     ENDSR
224600050110     C*
224700050110     C* GESTIONE sfl assegnati SENZA codice -------------------------*
224800050110     c     SFLSENZA      BEGSR
224900050216     c* Flag che mi dice se ho tassato qualche cosa per ricaricare i sfl
225000050216     c                   clear                   whotassato
225100050110     C
225200050110      * se i record scritti nel sfl degli assegnati supera i 9900 emetto msg di errore
225300050110If  1c                   If        wsflpieno2<> *Blanks
225400050110     c                   Eval      *In18 = *On
225500050110     c                   Eval      *In28 = *On
225600050110     c                   Eval      VidMsg = Msg(59)
225700050317     c     NRRS2         Chain     LRA2SS2                            34
225800050110     c                   Eval      *In36 = *On
225900050317     c                   Update    LRA2SS2
226000050110     c                   Eval      *In36 = *Off
226100050110     c                   Eval      *In18 = *Off
226200050110    1c                   EndIf
226300050127     c
226400050127     c                   if        savrecs2>0
226500050114     c                   z-add     savrecs2      rec
226600050127     c                   else
226700050127     c                   z-add     1             rec
226800050127     c                   endif
226900050110     C
227000050113     C     FORCTs2       TAG
227100050317     C                   WRITE     LRA2Zs2
227200050317     C   20              WRITE     LRA2DS2
227300050317     C                   EXFMT     LRA2CS2
227400050110     C**
227500050110     C                   SETOFF                                       2890
227600050110     C* F3 - FINE
227700050110     C   KC              GOTO      ENDSFLSENZA
227800050110     C**
227900050110     C* F12 - RITORNO
228000050110     C   KL              GOTO      ENDSFLSENZA
228100050111     c* F8 - Assegnati con   codice
228200050111     c                   if        *inkh
228300050114     C                   Z-ADD     PRIMA         savrecs2
228400050110     c                   eval      wtiposfl='C'
228500050110     c                   goto      endsflSENZA
228600050110     c                   endif
228700061123     c
228800061123     c* Se sfl vuoto, permetto solo F8, F12, F3
228900061123     c                   if        *in20
229000061123     c                   goto      forcts2
229100061123     c                   endif
229200061123     c
229300050113     c* Leggo in readc il sfl per caricarmi la schiera delle opzioni
229400050317     c                   readc     LRA2ss2                                34
229500050113     c                   dow       not *in34
229600050113     c                   eval      sksflsen(nrrs2)=vsfsce
229700050126     c                   if        vsfsce=' '
229800050127     c                   setoff                                       1816
229900050317     c                   update    LRA2ss2
230000050126     c                   endif
230100050317     c                   readc     LRA2ss2                                34
230200050113     c                   enddo
230300050110     C**
230400050110     C* SALVO IL POSIZIONAMENTO CURSORE
230500050127     c                   if        prima>0
230600050113     C                   Z-ADD     PRIMA         SAVRECs2          4 0
230700050127     c                   else
230800050127     c                   z-add     nrrs2         savrecs2
230900050127     c                   endif
231000050127     c
231100050113     c                   movea     sksflsen      skiopz
231200050113     c
231300050113     C* LETTURA SUBFILE  : opzione 5 - interrogazione
231400050113     c                   exsr      Opzione5
231500050114     c*
231600050114     C* LETTURA SUBFILE  : opzione 1 - tassazione singola
231700050114     c                   exsr      Opzione1
231800050113     C**
231900050113     C* LETTURA SUBFILE  : opzione 9 - tassazione multipla
232000050113     c                   exsr      Opzione9
232100050113     c                   movea     skiopz        sksflsen
232200050113     c   90              goto      forcts2
232300050113     c
232400050110     C* ERRORE
232500050120     c                   EXSR      SFLERROR
232600050110     C*
232700050113     C  N90              Z-ADD     SAVRECs2      REC
232800050113     C                   GOTO      FORCTs2
232900050110     C     endsflsenza   ENDSR
233000050121     c*
233100050926     c* Conferma totale tassazione tutte le bolle sflcon con la 'X' -------*
233200050121     c     CONFTOTALE    BEGSR
233300050121     c                   z-aDD     1             nrrc2
233400050317     C     nrrc2         chain     LRA2sc2                            34
233500050121     c
233600050121    1C     *IN34         DOWEQ     *Off
233700050121     c
233800050121     c* Leggo bolla per poi aggiornarla
233900050311     C  n94KARB2         CHAIN     FNARB000                           3295
234000050311     c   94vsfnrr        chain     fnblpfis                           3295
234100050121     c                   z-add     nrrc2         pp
234200050121     c                   movel     'X'           vsfsce
234300050121     c
234400050121     c                   EXSR      ELAARB
234500050121     C*
234600050121     c                   add       1             nrrc2
234700050317     C     nrrc2         chain     LRA2sc2                            34
234800050121    1c                   enddo
234900050121     c
235000050121     c                   ENDSR
235100050112     c*
235200050112     c* Opzione 5 del sfl: visualizzazione bolla ------------------------*
235300050112     c     OPZIONE5      BEGSR
235400050113     c                   z-aDD     1             PP                5 0
235500050113     C     '5'           lookup    skiopz(PP)                             34
235600050112     c
235700050113    1C     *IN34         DOWEQ     *On
235800050113     c
235900050113     c                   exsr      CHAINSFL
236000050309     c
236100050309     c                   exsr      INTBOLLE
236200050113     c*
236300050113     c
236400050926     c* imposto il flag di ignorare arbaca
236500050209     c                   eval      wusaaca='N'
236600050311     c                   clear                   waggiobol
236700050113     c                   exsr      AGGIOSFL
236800050209     c                   eval      wusaaca=' '
236900050113     c
237000050113     c                   add       1             PP
237100050113     C     '5'           lookup    skiopz(PP)                             34
237200050112    1c                   enddo
237300050113     c
237400050112     c                   ENDSR
237500050114     c*
237600050114     c* Opzione 1 del sfl: tassazione singola ---------------------------*
237700050114     c     OPZIONE1      BEGSR
237800050114     c                   z-aDD     1             PP                5 0
237900050114     C     '1'           lookup    skiopz(PP)                             34
238000050114     c
238100050114    1C     *IN34         DOWEQ     *On
238200050114     c
238300050114     c* Leggo bolla per poi aggiornarla
238400050114     c                   exsr      LEGARB
238500050121     c
238600050121     c                   exsr      ELAARB
238700050114     C*
238800050330     c* Se  c'e' stato invio automatico devo per forza riemettere il sfl
238900050330     c                   if        %error
239000050330     c                   setoff                                       34
239100050330     c                   else
239200050114     c                   add       1             PP
239300050114     C     '1'           lookup    skiopz(PP)                             34
239400050330     c                   endif
239500050114    1c                   enddo
239600050114     c
239700050114     c                   ENDSR
239800050120     c*
239900050120     c* Opzione X del sfl: tassazione singola senza videate -------------*
240000050120     c     OPZIONEX      BEGSR
240100050120     c                   z-aDD     1             PP                5 0
240200050120     C     'X'           lookup    skiopz(PP)                             34
240300050120     c
240400050120    1C     *IN34         DOWEQ     *On
240500050120     c
240600050120     c* Leggo bolla per poi aggiornarla
240700050120     c                   exsr      LEGARB
240800050121     c
240900050121     c                   exsr      ELAARB
241000050120     C*
241100050120     c                   add       1             PP
241200050120     C     'X'           lookup    skiopz(PP)                             34
241300050120    1c                   enddo
241400050120     c
241500050120     c                   ENDSR
241600050112     c*
241700050112     c* Opzione 9 del sfl: Tassazione multimpla -------------------------*
241800050112     c     OPZIONE9      BEGSR
241900050112     c* Devo fare 2 giri di lettura:
242000050112     c
242100050112     c* 1-Memorizzare codice di tassazione e vedere se uguale su tutte le
242200050112     c*   bolle. se <> dare errore forzabile. se cambia num bolle col 9
242300050112     c*   rifare controllo
242400050125     c                   clear                   tas9kc
242500050125     c                   clear                   tas9k
242600050112     c                   clear                   tas9n
242700050125     c                   clear                   tas9nk
242800050112     c                   clear                   tas9t
242900051024     c                   clear                   tas9siias         1
243000051024     c                   clear                   tas9noias         1
243100050112     c                   clear                   wcontabolle
243200050126     c                   clear                   wtas9ntw
243300050203     c                   clear                   wtas9sva
243400050318     c                   clear                   errmult
243500050113     c
243600050113     c                   z-aDD     1             PP                5 0
243700050113     C     '9'           lookup    skiopz(PP)                             34
243800050113     c
243900050113    1C     *IN34         DOWEQ     *On
244000050113     c
244100050203     c                   clear                   waggerr
244200050112     c                   add       1             wcontabolle       7 0
244300050113     c* NRR della pagina da visualizzare a video
244400050126     c                   z-add     PP            wtas9rec
244500050113     c                   exsr      CHAINSFL
244600050126     c
244700050126     c* se tipo servizio FEDEX , errore vincolante
244800160226     c* richiamo bolla x bolla trul27r1
244900160226     c                   clear                   trul27ds1
245000050126     c                   eval      i27tsp = vsftsp
245100050126     c                   eval      i27lna = vsflna
245200050714     c****               eval      i27lnp = vsflnp
245300050714     c                   eval      i27lnp=  VUNICOlnp
245400050714     c
245500050126     c                   if        vsfaks<>*blanks
245600050126     c                   movel     vsfaks        i27cli
245700050126     c                   endif
245800050126
245900160226     c                   call      'TRUL27R1'
246000160226     c                   parm                    trul27ds1
246100050126    2c                   if        o27ntw='FED'
246200050126     c                   seton                                        1890
246300050318     c                   eval      errmult='ERR'
246400050126     c                   eval      wtas9ntw='FED'
246500050126    3c                   if        errrec=0
246600050126     c                   z-add     pp            errrec
246700050318     c                   eval      errmult='NTW'
246800050126    3c                   endif
246900050203     c                   eval      waggerr='S'
247000050126   x2c                   else
247100050126     c
247200050203     c* Devono avere stesso NTW le bolle in tassazione multipla
247300050203    3c                   if        wtas9ntw='   '
247400050203     c                   eval      wtas9ntw=o27ntw
247500050126   x3c                   else
247600050126    4c                   if        o27ntw<>wtas9ntw
247700050126     c                   seton                                        1890
247800050318     c                   eval      errmult='ERR'
247900050126    5c                   if        errrec=0
248000050126     c                   z-add     pp            errrec
248100050318     c                   eval      errmult='NTW'
248200050126    5c                   endif
248300050203     c                   eval      waggerr='S'
248400050126    4c                   endif
248500050126    3c                   endif
248600050126    2c                   endif
248700050126     c
248800050203     c* Devono essere tutte monovaria o non monovaria
248900050203    2c                   if        wcontabolle=1
249000050203     c                   eval      wtas9sva=vsfsva
249100050203   x2c                   else
249200050203    3c                   if        vsfsva<>wtas9sva
249300050318     c                   eval      errmult='ERR'
249400050203     c                   seton                                        1890
249500050203    4c                   if        errrec=0
249600050203     c                   z-add     pp            errrec
249700050318     c                   eval      errmult='SVA'
249800050203    4c                   endif
249900050203     c                   eval      waggerr='S'
250000050203    3c                   endif
250100050203    2c                   endif
250200050318     c* Se bolla doppia, non posso fare tassazione multipla
250300050318    2c                   if        vsfdop='S'
250400050318     c                   eval      errmult='ERR'
250500050318     c                   seton                                        1890
250600050318    3c                   if        errrec=0
250700050318     c                   z-add     pp            errrec
250800050318     c                   eval      errmult='DOP'
250900050318    4c                   endif
251000050318     c                   eval      waggerr='S'
251100050318    2c                   endif
251200050203     c
251300050203     c                   if        waggerr='S'
251400050203     c                   exsr      aggiosfl
251500050203     c                   endif
251600050112     c
251700050112     c* Memorizzo il tipo servizio delle bolle
251800050112     c     vsftsp        lookup    tas9t                                  32
251900050113    3c                   if        not *in32
252000050112     c                   z-add     1             aa                3 0
252100050112     c     ' '           lookup    tas9t(AA)                              32
252200050112     c                   if        *in32
252300050112     c                   movel     vsftsp        tas9t(aa)
252400050112     c                   endif
252500050113    3c                   endif
252600050112     c
252700050112    3c                   if        vsfaks<>*blanks
252800050125     c* Memorizzo sia ksc+ctr che ksc da solo
252900050112     c                   z-add     1             aa                3 0
253000050113     c                   movel     vsfaks        w010a            10
253100050113     c                   move      vsfact        w010a            10
253200050125     c     w010a         lookup    tas9kc(AA)                             32
253300050112    4c                   if        *in32
253400050113     c                   add       1             tas9n(aa)
253500050112   x4c                   else
253600050112     c                   z-add     1             aa                3 0
253700050125     c     '          '  lookup    tas9kc(aa)                             32
253800050112    5c                   if        *in32
253900050125     c                   eval      tas9kc(aa)=w010a
254000050113     c                   z-add     1             tas9n(aa)
254100050112    5c                   endif
254200050112    4c                   endif
254300050125     c* solo codice
254400050125     c                   z-add     1             aa                3 0
254500050125     c     vsfaks        lookup    tas9k(AA)                              32
254600050125    4c                   if        *in32
254700050125     c                   add       1             tas9nk(aa)
254800050125   x4c                   else
254900050125     c                   z-add     1             aa                3 0
255000050125     c     '       '     lookup    tas9k(aa)                              32
255100050125    5c                   if        *in32
255200050125     c                   eval      tas9k(aa)=vsfaks
255300050125     c                   z-add     1             tas9nk(aa)
255400050125    5c                   endif
255500050125    4c                   endif
255600050112    3c                   endif
255700051024     c* Memorizzo se almeno una bolla     ha importo da assicurare
255800051024     c                   if        vsfesiias='S'
255900051024     c                   eval      tas9siias='S'
256000051024     c                   endif
256100051024     c* Memorizzo se almeno una bolla non ha importo da assicurare
256200051024     c                   if        vsfesiias='N'
256300051024     c                   eval      tas9noias='S'
256400051024     c                   endif
256500050112     c
256600050113     c                   add       1             PP
256700050113     C     '9'           lookup    skiopz(PP)                             34
256800050112    1c                   enddo
256900050126     c
257000050126     c* Se trovati errori vincolanti riemetto bideata
257100050126    0c                   if        *in90
257200050126     c                   seton                                        28
257300050203     c                   select
257400050318     c                   when      errmult='ERR'
257500050203     c                   eval      vidmsg=msg(73)
257600050203     c
257700050318     c                   when      errmult='SVA'
257800050203     c                   eval      vidmsg=msg(72)
257900050318     c
258000050318     c                   when      errmult='DOP'
258100050318     c                   eval      vidmsg=msg(77)
258200050203     c
258300050203     c                   when      wtas9ntw='FED'
258400050126     c                   eval      vidmsg=msg(70)
258500050203     c                   other
258600050126     c                   eval      vidmsg=msg(71)
258700050203     c                   endsl
258800050126     c                   z-add     errrec        rec
258900050126     c                   clear                   errrec
259000050126   x0c                   else
259100050112     c
259200050112     c* Se variato numero bolle torno a dare msg forzabile se + codici
259300050112    1c                   if        wcontabolle>0
259400050112     c
259500050112    2c                   if        wcontabolle<>wsalvaconta
259600050112     c                   clear                   wforzatas9
259700050112    2c                   endif
259800050112     c
259900050112     c                   eval      wsalvaconta=wcontabolle
260000050125     c* Errore forzabile  se scelte bolle con <> codici
260100050125    2c                   if        tas9kc(2)<>*blanks  and wforzatas9=' '
260200050125    3c                   select
260300050125     c* sicuramente errore se ci sono 3 <> cli+tar
260400050125     c                   when      tas9kc(3)<>*blanks
260500050125     c                   seton                                        2890
260600050125     c* Se c'è più di un codcliente errore
260700050125     c                   when      tas9k(2)<>*blanks
260800050112     c                   seton                                        2890
260900050125     c                   other
261000050125     c* se entrambi i 2 codici hanno il cod tariffa --> errore
261100050125     c                   move      tas9kc(1)     wcodtar1
261200050125     c                   move      tas9kc(2)     wcodtar2
261300050125    4c                   if        wcodtar1<>*blanks and wcodtar2<>*blanks
261400050125     c                   seton                                        2890
261500050125    4c                   endif
261600050125    3c                   endsl
261700050125     c
261800050125     c
261900050125    4c                   if        *in90
262000050112     c                   eval      vidmsg=msg(63)
262100050112     c                   eval      wforzatas9='1'
262200050126     c                   z-add     wtas9rec      rec
262300050125    4c                   endif
262400050125     c
262500050112    2c                   endif
262600050112     c
262700050112     c* 2-Procedo alla tassazione se non emesso msg forzabile
262800050113    2c                   if        not *in90
262900050112     c
263000050113     c                   z-aDD     1             PP                5 0
263100050113     C     '9'           lookup    skiopz(PP)                             34
263200050113     c
263300050113    3C     *IN34         ifeq      *On
263400050121     c* Leggo la prima bolla per proporre videata
263500050121     c                   exsr      CHAINSFL
263600050121     c
263700050310     C  n94KARB2         CHAIN(N)  FNARB000                           32
263800050310     C   94vsfnrr        CHAIN(N)  FNblpfis                           32
263900050112     C
264000050112     c                   eval      wscelta=vsfsce
264100050112     C  N32              EXSR      ELAB                                          ELABORO
264200050113    3C                   ENDiF
264300050110     c
264400050113    2c                   endif
264500050113    1c                   endif
264600050126    0c                   endif
264700050112     c
264800050112     c                   ENDSR
264900050113     c*
265000050113     c* Chain su2 2 sfl a seconda di dove sono -------------------------*
265100050113     c     CHAINSFL      BEGSR
265200050113    2c                   if        wtiposfl='C'
265300050113     c                   z-add     pp            nrrc2
265400050317     c     nrrc2         chain     LRA2sc2
265500050113     c                   else
265600050113     c                   z-add     pp            nrrs2
265700050317     c     nrrs2         chain     LRA2ss2
265800050113    2c                   endif
265900050113     c                   ENDSR
266000050113     c*
266100050113     c* chain su sfl e lettura fnarb000 ----------------------------------
266200050113     c     LEGARB        BEGSR
266300050113     c                   exsr      CHAINSFL
266400050113     c
266500050310     c* In Arrivo
266600050310     c                   if        not *in94
266700050113     C     KARB2         CHAIN     FNARB000                           3295
266800050310     c                   else
266900050314     c* In Partenza
267000050310     c     vsfnrr        chain     fnblpfis                           3295
267100050310     c                   endif
267200050113     c
267300050113     c                   ENDSR
267400050113     c*
267500050113     c* Aggiornamento dei 2 sfl ------------------------------------------
267600050113     c     AGGIOSFL      BEGSR
267700050310     c* In Arrivo
267800050121     C* proteggo bolla già elaborata  se non c'è errore
267900050209     C*  e se posso controllare il flag arbaca (no per opz 5)
268000050330     c  n94              if        wusaaca=' '
268100050121     C  n18ARBACA        IFNE      ' '
268200050113     C                   SETON                                        19
268300050216     c                   eval      whotassato='S'
268400050113     C                   END
268500050209     C                   ENDif
268600050310     c
268700050310     c* In partenza
268800050310     c                   if        *in94 and waggiobol='S'
268900050310     C                   SETON                                        19
269000050310     c                   eval      whotassato='S'
269100050310     C                   ENDif
269200050310     c
269300050114     c
269400050114     c* Per 18 on - errore --> imposto la E nel campo scelta del sfl
269500050114     c                   if        not *in18
269600050330     c* pulisco solo se no invio automatico
269700050330     c                   if        not %error
269800050113     C                   MOVEL     *BLANKS       VsfSCE
269900050330     c                   endif
270000050330     c
270100050114     c                   else
270200050114     C                   MOVEL     'E'           VsfSCE
270300050114     c                   endif
270400050114     c
270500050113    2c                   if        wtiposfl='C'
270600050317     C                   UPDATE    LRA2SC2
270700050113     c                   else
270800050215     c* Accendo il 13 se con codice
270900050215     c                   if        vsfaks<>*blanks
271000050215     c                   seton                                        13
271100050215     c                   endif
271200050215     c
271300050317     C                   UPDATE    LRA2Ss2
271400050113     c                   endif
271500050215     c
271600050113     C                   SETOFF                                       191816
271700050215     C                   SETOFF                                       13
271800050113     c
271900050113     c* pulisco relativa posizione dell'opzione
272000050330     c* solo se non c'e' stato inivio automatico
272100050330     c                   if        not %error
272200050113     c                   clear                   skiopz(pp)
272300050330     c                   endif
272400050113     c                   ENDSR
272500050121     c*
272600050314     c* Elaboro bolla in arb se non allocata e se trovata---------------*
272700050121     c     ELAARB        BEGSR
272800050121     C* SE IL RECORD E' ALLOCATO --> ERRORE
272900050121    1C     *IN95         IFEQ      *ON
273000050121     C                   SETON                                        18
273100050121     C                   MOVEL     '1'           WERR
273200050121     C* MEMORIZZO IL 1 RECORD IN ERRORE
273300050121     C     ERRREC        IFEQ      0
273400050121     C                   z-add     pp            ERRREC            4 0
273500050121     C                   ENDIF
273600050121     C*
273700050121   X1C                   ELSE
273800050121     C
273900050121    2c                   if        not *in32
274000050314     c* Se nel frattempo la spedizione è stata tassata, non proseguo
274100050314     c   94              exsr      SCARTAPAR
274200050314   2ac                   if        not *in94 and arbaca=' '  or
274300050314     c                             *in94 and wscarta=' '
274400050314     c                   eval      wscelta=vsfsce
274500050121     C                   EXSR      ELAB                                          ELABORO
274600050121     c
274700050121     c* 90 on - Errori in tassazione
274800050121     c*         solo per opzione X (anche se per 1 e 9 non dovrebbe mai
274900050121     c*         accadere che il 90 e il 91 qui siano accesi)
275000050121    3c                   if        (*in90 or *in91)  and  wscelta='X'
275100050310     c  n94              unlock    fnarb01l
275200050310     c   94              unlock    fnblp00f
275300050121     c
275400050121     C   90              MOVE      '2'           WERR
275500050121    4c                   if        *in91
275600050121     c                   move      werr          wuno              1
275700050121    5c                   if        wuno=' '
275800050121     C                   MOVE      '3'           WERR
275900050121    5c                   endif
276000050121    4c                   endif
276100050121     c
276200050121     C                   SETON                                        18
276300050121     C                   SETOff                                       289091
276400050121     C* MEMORIZZO IL 1 RECORD IN ERRORE
276500050121    4C     ERRREC        IFEQ      0
276600050121     C                   z-add     pp            ERRREC            4 0
276700050121    4C                   ENDIF
276800050121    3c                   endif
276900050314   2ac                   else
277000050314     c   94              eval      waggiobol='S'
277100050314     c  n94              unlock    fnarb01l
277200050314     c   94              unlock    fnblp00f
277300050314   2ac                   endif
277400050314   x2c                   else
277500050314     c   94              eval      waggiobol='S'
277600050121    2c                   endif
277700050121     c
277800050121    1c                   endif
277900050121     c
278000050310     c   94              eval      wusaaca='N'
278100050121     c                   exsr      AGGIOSFL
278200050121     c
278300050121     c                   ENDSR
278400050112     c*
278500050112     c* Opzione 9 del sfl: leggo le bolle ed aggiorno -------------------*
278600050112     c     ELABopzione9  BEGSR
278700050121     c                   z-add     1             PP
278800050113     C     '9'           lookup    skiopz(PP)                             34
278900050113    1c                   dow       *in34
279000050311     C                   CLEAR                   WAGGIOBOL
279100050113     c* Leggo bolla per poi aggiornarla
279200050113     c                   exsr      LEGARB
279300050113     C*
279400050113     C* SE IL RECORD E' ALLOCATO --> ERRORE
279500050113    2C     *IN95         IFEQ      *ON
279600050113     C                   SETON                                        18
279700050120     C                   MOVEL     '1'           WERR
279800050113     C* MEMRORIZZO IL 1 RECORD IN ERRORE
279900050113     C     ERRREC        IFEQ      0
280000050114     C                   z-add     pp            ERRREC            4 0
280100050113     C                   ENDIF
280200050113     C*
280300050113   X2C                   ELSE
280400050113     c
280500050314     c* Se nel frattempo le bolle sono state tassate --> scarto
280600050314     c  n32
280700050314     can 94              exsr      SCARTAPAR
280800050311    3c                   if        not *in32  and arbaca=' ' AND not *IN94 or
280900050314     c                             *in94 and not *in32 and wscarta=' '
281000050314     c* codice bolle per impostare in cod.Variazione bolla
281100050120    4c                   if        not *in11
281200050113     C                   MOVEL     '3A'          COD
281300050113     C                   MOVEL(P)  ARBCBO        KEY
281400050113     C     KTAB          CHAIN     TABEL                              32
281500050113     C  N32              MOVEL     TBLUNI        DS3A
281600050113     C   32              CLEAR                   DS3A
281700050113     C                   MOVEL     §3ADES        DESCBO
281800050113     C                   MOVEL     §3ATB1        TIPTB1
281900050113     c*
282000050120    5C     TIPTB1        IFEQ      'A'
282100050317     C                   MOVEL     'TB'          ILRA2CVB
282200050113     C                   ELSE
282300050317     C                   MOVEL     '2B'          ILRA2CVB
282400050120    5C                   END
282500050113     C*
282600050113     C* PRENDO TIPO CODICE VARIAZIONE BOLLA
282700050113     C                   MOVEL     '7L'          COD
282800050113     C                   MOVEL     *BLANKS       KEY
282900050317     C                   MOVEL     ILRA2CVB      KEY
283000050113     C     KTAB          CHAIN     TABEL                              32
283100050113     C  N32              MOVEL     TBLUNI        DS7L
283200050120     C   32              MOVEL     *BLANKS       DS7L
283300050120    4c                   endif
283400050113     C
283500050120     C                   EXSR      PREPA06                                       ELABORO
283600050926     c
283700060117     c* Tasso il  porto se devo considerare le bolle dirottate/cambio p
283800060117     c                   if        vsfdir<>' '
283900050926     c                   movel     'S'           Memsoloporto      1
284000050926     C                   MOVEL     '1'           WBOL              1
284100050926     C                   MOVEL     'N'           WCANCN            1
284200050926     C                   EXSR      TASSAZ
284300050926     c                   movel     ' '           Memsoloporto      1
284400050926     c                   endif
284500050120
284600050310     C  n94              EXSR      AGGIORARB                                     ELABORO
284700050310     C   94              EXSR      AGGIORblp                                     ELABORO
284800050314     c                   else
284900050314     c* Se bolla ora da scartare, nel sfl la proteggo come se fosse tassat
285000050314     c   94              eval      waggiobol='S'
285100050120    3c                   endif
285200050120    2c                   endif
285300050113     c
285400050113     c                   exsr      AGGIOSFL
285500050113     c
285600050113     c                   add       1             PP
285700050113     C     '9'           lookup    skiopz(PP)                             34
285800050113    1c                   enddo
285900050113     c
286000050112     c                   ENDSR
286100050120     c*
286200050120     c* Emissione msg di errore su sfl ------------------------------------*
286300050120     c     SFLERROR      BEGSR
286400050120    1C                   IF        werr<>*blanks
286500050120     c                   select
286600050120    1C     WERR          wheneq    '1 '
286700050120     C                   MOVEL     MSG(39)       VIDMSG
286800050120    1C     WERR          wheneq    ' 2'
286900050120     C                   MOVEL     MSG(66)       VIDMSG
287000050120    1C     WERR          wheneq    ' 3'
287100050120     C                   MOVEL     MSG(22)       VIDMSG
287200050120   x1C                   other
287300050120     C                   MOVEL     MSG(67)       VIDMSG
287400050120    1c                   endsl
287500050120     c
287600050120     C                   Z-ADD     ERRREC        REC
287700050120     C                   SETON                                        2890
287800050120     C                   CLEAR                   WERR
287900050120     C                   CLEAR                   errrec
288000050120    1C                   ENDIF
288100050120     c                   ENDSR
288200050112     c*
288300911028     C*--- ELABORAZIONE BOLLA ----------------------------------------*
288400911028     C     ELAB          BEGSR
288500011015      *
288600011015     C                   CLEAR                   FLGVRN
288700911114     C* CARICO DATI BOLLA
288800911114     C                   EXSR      CARBOL
288900911028     C**
289000941219     C* PULIZIA DEL FORMATO CHE DEVO EMETTERE
289100050317     C                   CLEAR                   LRA2D04
289200010319     C     WIDMSG        IFNE      *BLANKS
289300010319     C                   MOVEL     WIDMSG        VIDMSG
289400010319     C                   SETON                                        28
289500010319     C                   ENDIF
289600941221     C*
289700941221     C     FOR03         TAG
289800050317     c                   setoff                                       1004
289900050113     c
290000050113     c                   if        wscelta='9'
290100050113     c                   seton                                        04
290200050113     c                   endif
290300030507      * Recupero i dati del cliente
290400030507     c                   ExSr      Sr_RecDati
290500941219     C*
290600911127     C                   MOVEL     *BLANKS       DESKSC
290700911127     C                   MOVEL     COMDES        DESKSC
290800050112     c*
290900050112     c* Videate diverse per tassazione singola o tassazione multimpla
291000050112     c                   if        wscelta<>'X'
291100050112     C
291200050112     c                   if        wscelta='1'
291300050317     C                   WRITE     LRA2TS3
291400050317     C                   WRITE     LRA2Z03
291500050317     C                   write     LRA2D03
291600050317     C                   read(E)   fnLRA2d
291700050112     c                   endif
291800050317     C*
291900050112     c                   if        wscelta='9'
292000050317     C                   WRITE     LRA2Tm3
292100050317     C                   WRITE     LRA2Z03
292200050317     C                   EXFMT     LRA2D03
292300050112     c                   endif
292400050112     c
292500050317     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
292600050317     C                   EXSR      INZD03
292700050314     c*
292800050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
292900050314     c                   if        %error
293000050317     c                   eval      *inkl='1'
293100050314     C  n94              UNLOCK    FNARB01L
293200050314     C   94              UNLOCK    FNblp00f
293300050317     c                   goto      endela
293400050112     c                   endif
293500050314     c                   endif
293600941219     C* F 3 - FINE
293700050310     C* F12 - RITORNO
293800050310     c   kl
293900050310     cor kc              if        not *in94
294000050310     C                   UNLOCK    FNARB01L
294100050310     c                   else
294200050310     C                   UNLOCK    FNblp00f
294300050310     c                   endif
294400941219     C   KC              GOTO      FINE
294500911028     C   KL              GOTO      ENDELA
294600911114     C*
294700050112     C* F7  - RICERCA CODICE CLIENTE
294800911203     C   KG              DO
294900050926     C* NON AMMESSA SE CODICE DI ALTRA FILIale
295000941216     C                   MOVEL     VIDDSA        PARDUT           30
295100911114     C                   MOVEL     *BLANKS       DESCDS           48
295200911114     C                   MOVEL     DESKSC        DESCDS
295300911114     C* PARSTA=9 ESCLUDO ANNULLATI
295400911114     C                   Z-ADD     9             PARSTA
295500911115     C                   Z-ADD     KCI           CCC               4 0
295600000202     C                   CLEAR                   PARFLR
295700000202     C                   CLEAR                   PARNUM
295800000202     C                   CLEAR                   PARKCM
295900000202     C                   CLEAR                   PARKSM
296000000202     C                   CLEAR                   PARKDM
296100000202     C                   MOVEL     VIDKLP        PARFLR
296200980703     C                   CLEAR                   PARDIT
296300000202     C                   CALL      'XALFA3BR'
296400911114     C                   PARM                    PARDUT
296500911114     C                   PARM                    CODUT
296600911114     C                   PARM                    DESCDS
296700911114     C                   PARM                    CCC
296800911114     C                   PARM                    PARSTA            1 0
296900961016     C                   PARM                    PARFLR           90
297000980703     C                   PARM                    PARDIT            3
297100000202     C                   PARM                    PARNUM            2 0
297200000202     C                   PARM                    PARKCM           80
297300000202     C                   PARM                    PARKSM          140
297400000202     C                   PARM                    PARKDM           60
297500050317     C                   PARM      'S'           PAResci           1
297600050317     C                   PARM      '  '          PARERR            2
297700050317     c*
297800050317     c* Passato tempo max 50 secondi di attesa su ricerca alfabetica:
297900050317     c*                      torno al sfl
298000050317     c                   if        parERR='SP' AND PARSTA=-1
298100050317     c                   eval      *inkl='1'
298200050317     C  n94              UNLOCK    FNARB01L
298300050317     C   94              UNLOCK    FNblp00f
298400050317     c                   goto      endela
298500050317     c                   endif
298600050317     c* Nessuna selezione:
298700911115     C     PARSTA        IFEQ      -1
298800911128     C*
298900911128     C     TIPTB1        IFEQ      'A'
299000911203     C                   MOVEL     ARBKSC        VIDKLP
299100911203     C                   MOVE      ARBKSC        VIDKSC
299200911128     C                   ELSE
299300960208     C                   MOVEL     ARBLNA        VIDKLP
299400911203     C                   MOVE      9999          VIDKSC
299500911128     C                   END
299600960208     C* SE IL CODICE E' 9999 --> PROPONGO IN KLP QUANTO MEMORIZZATO
299700960208     C*  NELLA ROUTINE CARBOL
299800960208     C     VIDKSC        IFEQ      9999
299900960208     C                   MOVEL     WKLP          VIDKLP
300000960208     C                   ENDIF
300100030507      * salvo il codice cliente impostato
300200030507     c                   Eval      Sav_VidKsc = VidKsc
300300911128     C*
300400911115     C                   ELSE
300500911128     C*
300600000202     C                   MOVEL     PARKSM        W0070             7 0
300700000202     C                   MOVEL     W0070         VIDKLP
300800000202     C                   MOVE      W0070         VIDKSC
300900911115     C                   END
301000911114     C                   GOTO      FOR03
301100911114     C                   END
301200911114     C* CONTROLLI CAMPI
301300911114     C                   EXSR      CONTR3
301400050120     c* Se conferma con X imposto la "E" di errore ed esco dal sfl
301500050120     c                   if        *in90
301600050120     c                   if        wscelta='X'
301700050120     c                   goto      endela
301800050120     c                   else
301900050120     C                   GOTO      FOR03
302000050120     c                   endif
302100050120     c                   endif
302200911114     C***
302300050113     C*  GESTISCO TASSAZIONE di  CLIENTE CHE NON HA TARIFFE
302400941216     C***
302500050202     C                   SETOFF                                       96
302600050112     C     WTKS          ifeq      *BLANKS
302700941216     C                   SETON                                        29
302800050202     C                   SETON                                        29
302900941216     C                   EXSR      GESKCA
303000050317     c* Se errore per supero tempo mak di attesa: torno al sfl
303100050317     c                   if        *inkl and %error
303200050317     c                   goto      endela
303300050317     c                   endif
303400050317     c
303500941216     C   KL              GOTO      FOR03
303600941216     C*
303700941216   X1C                   ELSE
303800941216     C***
303900050114     C* CLIENTE CODIFICATO  CON TARIFFE
304000941216     C***
304100050202     C                   SETOFF                                       2901
304200941216     C*
304300941216    2C     VIDKSC        IFNE      9999
304400941216     C                   MOVEL     ACORAG        DESKSC
304500010327     C     WIDMSG        IFNE      *BLANKS
304600010327     C                   MOVEL     WIDMSG        VIDMSG
304700010327     C                   SETON                                        28
304800010327     C                   ENDIF
304900920319     C*
305000911028     C* EMETTO FORMATO4
305100911028     C     FOR04         TAG
305200050113     C                   SETOFF                                       0126
305300941221     C                   CLEAR                   DESTAS
305400050113     c
305500030507      * Recupero i dati del cliente
305600030507     c                   ExSr      Sr_RecDati
305700911028     C**
305800050112     c                   if        wscelta<>'X'
305900050317     c                   seton                                        10
306000050112     c                   if        wscelta='1'
306100050330     C                   WRITE     LRA2Ts3                                      TESTATA
306200050330     C                   WRITE     LRA2D03                                      COD CLIENTE
306300050330     C                   WRITE     LRA2Z04                                      PIEDI
306400050317     C                   write     LRA2D04                                      COD TARIFFA
306500050317     C                   read(E)   fnLRA2d                                      COD TARIFFA
306600050112     c                   endif
306700050317     C
306800050112     c                   if        wscelta='9'
306900050317     C                   WRITE     LRA2TM3                                      TESTATA
307000050317     C                   WRITE     LRA2D03                                      COD CLIENTE
307100050317     C                   WRITE     LRA2Z04                                      PIEDI
307200050317     C                   EXFMT     LRA2D04                                      COD TARIFFA
307300050112     c                   endif
307400050317     c
307500050317     C* SPENGO INDICATORI DI TIPO RECORD
307600050317     C                   EXSR      INZD04
307700050317     C
307800050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
307900050314     c                   if        %error
308000050314     c                   eval      *inkl='1'
308100050314     C  n94              UNLOCK    FNARB01L
308200050314     C   94              UNLOCK    FNblp00f
308300050314     c                   goto      endela
308400050314     c                   endif
308500050112     c                   endif
308600941219     C**
308700050112     C* F12 - RITORNO
308800941221     C     *INKL         IFEQ      *ON
308900941221     C* PULIZIA DEL FORMATO CHE DEVO EMETTERE
309000050317     C                   CLEAR                   LRA2D04
309100941221     C                   GOTO      FOR03
309200941221     C                   ENDIF
309300050310     c* F15 - forzatura fattura immediata
309400050310     c*       non ammessa per tassazione in partenza
309500050310     c                   if        *in94 and *inkp
309600050310     C                   MOVEL     MSG(76)       VIDMSG
309700050310     C                   SETON                                        289055
309800050310     C                   GOTO      FOR04
309900050310     c                   endif
310000051013     c* F15 - forzatura fattura immediata
310100051013     c*       non ammessa per cliente estero
310200051013     c                   if        *inkp and (clintw='EEX' or clintw='FED'or
310300051013     c                             clintw='DPD')
310400051013     C                   MOVEL     MSG(80)       VIDMSG
310500051013     C                   SETON                                        289055
310600051013     C                   GOTO      FOR04
310700051013     c                   endif
310800911028     C* ENTER - CONTROLLI
310900911028     C                   EXSR      CONTR4
311000050120     c
311100050120     c                   if        *in90
311200050120     c                   if        wscelta='X'
311300050120     c                   goto      endela
311400050120     c                   else
311500050120     C                   GOTO      FOR04
311600050120     c                   endif
311700050120     c                   endif
311800050120     c
311900050112     c* Per tassazione multipla, enter riemetto videata , F1 confermo
312000050112     c                   if        wscelta='9' and not *inka
312100050112     c                   goto      for04
312200050112     c                   endif
312300050112     c* F1 - conferma
312400050112     c                   if        wscelta='9' and *inka
312500050120     c                   seton                                        01
312600050112     c                   exsr      ELABopzione9
312700050120     c                   goto      endela
312800050112     c                   endif
312900050112     c*
313000941217     C* VEDO SE IL CLIENTE ERA S.F.
313100941216    3C     TAMFTS        IFEQ      '1'
313200941217     C                   MOVEL     TAS(1)        DESTAS
313300941217     C                   ELSE
313400941217     C                   MOVEL     TAS(2)        DESTAS
313500941216    3C                   END
313600941216     C*
313700950203     C* F1   = SEGUE FATTURA
313800050223     C* F15  = FATTURA IMMEDIATA
313900941217     C* ENTER=QUELLO CHE E'
314000941217     C   KA              SETON                                        01
314100050223     C   KP              SETOFF                                       01
314200941220     C  NKA
314300050223     CANNKPTAMFTS        IFEQ      '1'
314400941217     C                   SETON                                        01
314500941217     C                   ENDIF
314600941216   X2C                   ELSE
314700911028     C**
314800911028     C* LASCIATO 9999 PROPONGO VIDEATA PER TASSAZIONE
314900911030     C                   MOVEL     *BLANKS       DESKSC
315000050112     C* Tassazione con tariffa di cartello obbligatoria
315100020222      * ricerco tariffa di cartello
315200160226     c                   clear                   trul27ds1
315300160226     c                   Eval      I27Pkg = ArbPkf
315400160226     c                   eval      i27tsp = arbtsp
315500160226     c                   eval      i27lna = arblna
315600160226     c                   eval      i27lnp = VUNICOLNP
315700160226     c
315800160226     c                   movel     vidklp        i27cli
315900160226     c                   move      vidksc        i27cli
316000160226
316100160226     c                   call      'TRUL27R1'
316200160226     c                   parm                    trul27ds1
316300160226     c
316400160226     c                   clear                   trulc7ds
316500160226     c                   eval      ic7tntw=o27ntw
316600160226     c                   call      'TRULC7R'
316700160226     c                   parm                    trulc7ds
316800160226     c
316900160226     c****               eval      tszksc = o27ksc
317000160226     c*****              move      o27ctr        tszctr
317100160226     c
317200160226     c                   eval      tszksc = oc7kscc
317300160226     c                   move      oc7ctrc       tszctr
317400020222
317500050112     C                   SETON                                        96
317600930430     C*
317700050120     C**   FOR05         TAG
317800030507      * Se cliente 9999 pulisco i campi a video
317900030506     c                   If        VidKsc = 9999
318000030506     c                   Clear                   VidVia
318100030506     c                   Clear                   VidLoc
318200050114     c                   Clear                   VidCae
318300030506     c                   Clear                   VidPrv
318400030506     c                   Clear                   VidNota
318500050114     c                   Clear                   V6dVia
318600050114     c                   Clear                   V6dLoc
318700050114     c                   Clear                   V6dCae
318800050114     c                   Clear                   V6dNota
318900030506     c                   Eval      *In46 = *Off
319000030507      * salvo il codice cliente impostato
319100030507     c                   Eval      Sav_VidKsc = VidKsc
319200030605      * salco le spese di incasso
319300030605     c                   eval      Sav_indsin = Indsin
319400030506     c                   EndIf
319500030506
319600050317     C**                 WRITE     LRA2T03                                      TESTATA
319700050317     C**                 WRITE     LRA2T04                                      COD CLIENTE
319800050317     C**                 WRITE     LRA2Z05                                      PIEDI
319900050317     C**N96              EXFMT     LRA2D05                                      CODICE TASSA
320000050317     C** 96              WRITE     LRA2D05                                      CODICE TASSA
320100941219     C*
320200941217     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
320300050113     C**                 SETOFF                                       452890
320400050113     C**                 SETOFF                                       70
320500941221     C* F12 - RITORNO
320600050112     C**   *INKL         IFEQ      *ON
320700941221     C* PULIZIA DEL FORMATO CHE DEVO EMETTERE
320800050317     C**                 CLEAR                   LRA2D04
320900050112     C**                 GOTO      FOR03
321000050112     C**                 ENDIF
321100911028     C*
321200911028     C* ENTER - CONTROLLI
321300050113    4C**   TSZKSC        IFNE      0
321400050113     C**                 EXSR      CONTR5
321500050113     C** 90              GOTO      FOR05
321600050113    4C**                 END
321700050113     c
321800050113    2C                   END
321900911115     C**
322000911115     C* TASSAZIONE SCELTA
322100941217     C     *IN01         IFEQ      *OFF
322200941217     C                   MOVEL     TAS(2)        SCETAS
322300941216     C                   ELSE
322400941217     C                   MOVEL     TAS(4)        SCETAS
322500941216     C                   ENDIF
322600911204     C*
322700050120     C                   EXSR      PREPA06
322800941216     C*
322900950104     C                   CLEAR                   VIDMSG
323000911028     C*
323100911028     C* A S S E G N A T I   N O R M A L I
323200941216    2C  N05              DO
323300941216     C**
323400950104     C     *IN01         IFEQ      *OFF
323500950104     C     VIDKSC        ANDNE     9999
323600950104     C     TSZKSC        ORGT      0
323700950104     C     VIDKSC        ANDEQ     9999
323800970606     C                   MOVEL     'N'           WCANCN            1
323900980512     C                   MOVEL     '1'           WBOL              1
324000011018      * Se è una bolla di reso con varia 'N' 88888888 ed è fattura immediata
324100011126      * tasso la varia 'N'
324200011018     C     FLGVRN        IFEQ      '1'
324300011010     C     *IN01         ANDEQ     *OFF
324400050120      * leggo il sfl delle varie
324500011010     C                   DO        20            NRR
324600050317     C     NRR           CHAIN     LRA2S06                            32
324700011010     C     V6CSV1        IFEQ      'N'
324800011010     C     V6CVA1        ANDEQ     88888888
324900011010     C                   EXSR      TASSAN
325000050317     C  N32              UPDATE    LRA2S06
325100011010     C                   ENDIF
325200011010     C                   ENDDO
325300011010     C                   ENDIF
325400011010      *
325500070924     c                   clear                   PesVolPkcn
325600070924     c                   clear                   PesVolSpc
325700070924     c                   clear                   PesVolImv
325800070924     c                   clear                   Tassatoimv
325900930225     C                   EXSR      TASSAZ
326000070924     c* Se totale imponibile, memorizzo i dati usati dalla fatturaz.
326100070924     c                   if        vidimv>0
326200070924     c                   eval      PesVolPkcn=taspkcn
326300070924     c                   eval      PesVolSpc=tasspc
326400070924     c                   eval      PesVolIMV=vidimv
326500070924     c                   endif
326600070924     c
326700050926     c                   else
326800060117     c* Propongo il porto se devo considerare le bolle dirottate/cambio p
326900060117     c                   if        vsfdir<>' '
327000050926     c                   movel     'S'           Memsoloporto      1
327100050926     C                   MOVEL     'N'           WCANCN            1
327200050926     C                   MOVEL     '1'           WBOL              1
327300050926     C                   EXSR      TASSAZ
327400050926     c                   movel     ' '           Memsoloporto      1
327500050926     c                   endif
327600941216     C                   ENDIF
327700941216     C*
327800941216     C* VIDEATA DI TASSAZIONE
327900941216     C     FOR06         TAG
328000941221     C* WRITE TESTATA
328100050120     c                   if        wscelta<>'X'
328200050317     C                   WRITE     LRA2T06                                      TESTATA
328300941221     C     VIDKSC        IFEQ      9999
328400050317     C                   WRITE     LRA2D06                                      COD X TASSAZ
328500941221     C                   ELSE
328600050317     C                   WRITE     LRA2D66                                      COD CLIENTE
328700941221     C                   ENDIF
328800050317     C  n03              WRITE     LRA2Z06                                      PIEDI
328900050317     c   03              Write     LRA2z06p                                     Piedi x Picking
329000941223     C*
329100941221     C     VIDMSG        IFNE      *BLANKS
329200941221     C                   SETON                                        28
329300950421     C                   ENDIF
329400941221     C* 1 BOLLA
329500050317     C*                  EXFMT     LRA2C06                                      TASSAZIONE
329600050317     C                   write     LRA2C06                                      TASSAZIONE
329700050317     C                   read(E)   fnlra2d
329800050120     c                   endif
329900941217     C*
330000941217     C                   SETOFF                                       902841
330100941217     C                   SETOFF                                       985253
330200941217     C                   SETOFF                                       554249
330300941217     C                   SETOFF                                       505145
330400011127     C                   SETOFF                                       4847
330500050317     c
330600050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
330700050317     c                   if        %error
330800050317     c                   eval      *inkl='1'
330900050317     C  n94              UNLOCK    FNARB01L
331000050317     C   94              UNLOCK    FNblp00f
331100050317     C                   UNLOCK    FIAR601L
331200050317     c                   goto      endela
331300050317     c                   endif
331400050113     C* F12 - RITORNO
331500990910     C* DISALLOCO FIAR6
331600990910     C   KL              UNLOCK    FIAR601L
331700941217    3C   KLVIDKSC        IFNE      9999
331800941216     C                   GOTO      FOR04
331900941216     C                   ELSE
332000981008     C* SE 96 ON DEVO RITRONARE INDIETRO DI 2 VIDEATE
332100050317     C**   *IN96         IFEQ      *ON
332200050317     C                   CLEAR                   LRA2D04
332300981008     C                   GOTO      FOR03
332400050120     C**                 ELSE
332500050120     C**                 GOTO      FOR05
332600050317    3C**                 ENDIF
332700981008    3C                   ENDIF
332800911114     C*
332900911028     C* ENTER - CONTROLLI
333000911204     C                   EXSR      CONTR6
333100911114     C*
333200911115     C* L'IMPONIBILE E' OBBLIGATORIO PER FATTURAZIONE IMMEDIATA
333300911115     C  N01
333400911115     CANN90VIDIMV        IFEQ      0
333500941221     C                   MOVEL     MSG(32)       VIDMSG
333600950104     C                   SETON                                        4990
333700941217    3C                   END
333800050120     c
333900050120     c                   if        wscelta='X'
334000050120     c   90              goto      endela
334100050120     c                   else
334200050120     c
334300031028     c                   If        Not *In03 and
334400031028     c                             (Not *InKf or *In90)
334500031028     c                   GoTo      For06
334600031028     c                   EndIf
334700031028     c                   If        *In03 and
334800031028     c                             ((Not *InKf and Not *InKh) or *In90)
334900031028     c                   GoTo      For06
335000031028     c                   EndIf
335100050120     c                   EndIf
335200060317     c*
335300060317     c                   if        §7lmtv = 'S'
335400060317     c                   exsr      ges_mtv
335500060317     c                   if        olv85f12 = '1'
335600060317     c                   GoTo      For06
335700060317     c                   endif
335800060317     c                   endif
335900911218     C**
336000911218     C* CONFERMA TASSAZIONE AGGIORNO BOLLE
336100911218     C**
336200050310     C  n94              EXSR      AGGIORARB
336300050310     C   94              EXSR      AGGIORblp
336400050120     c*
336500050120     c                   if        *in91
336600050120     c                   if        wscelta='X'
336700050120     c                   goto      endela
336800050120     c                   else
336900050120     C                   GOTO      FOR06
337000050120     c                   endif
337100050120     c                   endif
337200050120     c
337300941217    2C                   ENDDO
337400911028     C*
337500911028     C* E S T E N S I O N E   B O L L A
337600941217    2C   05              DO
337700911115     C* PER SEGUE FATTURA O FATTURAZIONE E' COMUNQUE OBBLIGATORIO
337800990913     C*  L'IMPORTO DELLA VARIA 'G'
337900980512     C                   MOVEL     '2'           WBOL
338000980512     C                   EXSR      TASSAZ
338100930503     C*
338200911028     C*
338300911028     C     FOR07         TAG
338400941221     C* WRITE TESTATA
338500050317     C                   WRITE     LRA2T06                                      TESTATA
338600941221     C     VIDKSC        IFEQ      9999
338700050317     C                   WRITE     LRA2D06                                      COD X TASSAZ
338800941221     C                   ELSE
338900050317     C                   WRITE     LRA2D66                                      COD CLIENTE
339000941221     C                   ENDIF
339100941221     C* 2 BOLLA
339200991025     C     FOR77         TAG
339300050317     C*                  EXFMT     LRA2D07                                      TASSAZIONE
339400050317     C                   write     LRA2D07                                      TASSAZIONE
339500050317     C                   read(E)   fnlra2d
339600050317     c
339700050317     c* Spengo indicatori di posizionamento cursore
339800941219     C                   SETOFF                                       727475
339900990916     C                   SETOFF                                       902848
340000040119     c                   Setoff                                       717376
340100040119     c                   Setoff                                       777879
340200041129     c                   Setoff                                       808182
340300041129     c                   Setoff                                       83
340400050317     c
340500050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
340600050317     c                   if        %error
340700050317     c                   eval      *inkl='1'
340800050317     C  n94              UNLOCK    FNARB01L
340900050317     C   94              UNLOCK    FNblp00f
341000050317     C                   UNLOCK    FIAR601L
341100050317     c                   goto      endela
341200050317     c                   endif
341300050113     C* F12 - RITORNO
341400990910     C   KL
341500990910     CANN38              UNLOCK    FIAR601L
341600990913     C   KLVIDKSC        IFNE      9999
341700941217     C                   GOTO      FOR04
341800941217     C                   ELSE
341900981008     C     *IN96         IFEQ      *ON
342000050317     C                   CLEAR                   LRA2D04
342100981008     C                   GOTO      FOR03
342200050120     C**                 ELSE
342300050120     C**                 GOTO      FOR05
342400941217     C                   ENDIF
342500981008     C                   ENDIF
342600911114     C*
342700911028     C* ENTER - CONTROLLI
342800990913     C                   EXSR      CONTR7
342900050120     c
343000050120     c                   if        wscelta ='X'
343100050120     c   90              goto      endela
343200050120     c                   else
343300991025     C   28              GOTO      FOR77
343400950121     C  NKF
343500911028     COR 90              GOTO      FOR07
343600050120     c                   endif
343700060317     c*
343800060317     c                   if        §7lmtv = 'S'
343900060317     c                   exsr      ges_mtv
344000060317     c                   if        olv85f12 = '1'
344100060317     c                   GoTo      For07
344200060317     c                   endif
344300060317     c                   endif
344400911218     C**
344500911218     C* CONFERMA TASSAZIONE AGGIORNO BOLLE
344600050310     C  n94              EXSR      AGGIORARB
344700050310     C   94              EXSR      AGGIORblp
344800050120     c
344900050120    3c                   if        *in91
345000050120    4c                   if        wscelta='X'
345100050120     c                   goto      endela
345200050120     c                   else
345300911218     C   91              GOTO      FOR07
345400050120    4C                   ENDif
345500050120    3C                   ENDif
345600050120
345700050120    2C                   END
345800941217    1C                   END
345900911028     C*
346000911028     C     ENDELA        ENDSR
346100050317     C*
346200050317     C*--- SPENGO INDICATORI DI POSIZIONAMENTO CURSORE FORMATO D03----*
346300050317     c     INZD03        begsr
346400050317     c                   setoff                                       289055
346500050317     c                   setoff                                       404142
346600050317     c                   ENDSR
346700050317     C*
346800050317     C*--- SPENGO INDICATORI DI POSIZIONAMENTO CURSORE FORMATO D03----*
346900050317     c     INZD04        begsr
347000050317     C                   SETOFF                                       289055
347100050317     c                   ENDSR
347200911114     C*
347300911114     C*--- CARICO BOLLA ----------------------------------------------*
347400020527     C     CARBOL        BEGSR
347500050715     C                   SETOFF                                       121422
347600941220     C                   SETOFF                                       262901
347700050113     C*
347800941217     C                   CLEAR                   VARVAS
347900941217     C                   CLEAR                   WINTER
348000941217     C                   MOVEL     0             CIN               1 0
348100941217     C                   MOVEL     0             DIE               1 0
348200950104     C                   MOVEL     0             UND               1 0
348300941217     C                   Z-ADD     0             SAVKSC            7 0
348400950104     C                   CLEAR                   VARQFT
348500010327     C                   CLEAR                   WORM
348600050310     C                   CLEAR                   Waggiobol         1
348700010327     C                   CLEAR                   WIDMSG
348800030521     c                   Clear                   sav_VidKsc
348900030605     c                   Clear                   sav_indsin
349000050201     c                   Clear                   Wsfact
349100030709      * pulisco anche savtrc
349200030709     c                   Clear                   savctr
349300030729
349400030729     c                   Clear                   VidBan
349500030729     c                   Clear                   VidCor
349600030729     c                   Eval      *In08 = *Off
349700050111     c                   move      'S'           wstessogas        1
349800070924     c                   Clear                   PesVolPkcn
349900070924     c                   Clear                   PesVolSpc
350000070924     c                   Clear                   PesVolImv
350100070924     c                   Clear                   Tassatoimv
350200941219     C**
350300941219     C* PULISCO I FORMATI
350400050317     C                   CLEAR                   LRA2D03
350500050317     C                   CLEAR                   LRA2d06
350600050317     C                   CLEAR                   LRA2d66
350700050317     C                   CLEAR                   LRA2D07
350800050310     c* Reimposto l'indicatore di tassazione in partenza
350900050310     C                   IF        WTIPOTASSA='P'
351000050310     C                   SETON                                        94
351100050310     C                   ENDIF
351200050715     c**
351300050715     c* Bolla dirottata
351400050715     c**
351500060117     c                   if        Vsfdir<>' '
351600050715     c                   seton                                        22
351700050715     c                   endif
351800050111     C**
351900050111     C* VALUTA DAL DESTINATARIO
352000050111     C**
352100050111     C                   MOVEL     '15'          COD
352200050111     C                   MOVEL(P)  ARBNZD        KEY
352300050111     C                   EXSR      CTRTAB
352400050111     C  N32              MOVEL     TBLUNI        DS15
352500050111     C   32              CLEAR                   DS15
352600050111     C                   MOVEL     §15VLT        DESVLT            3
352700050111     C* BOLLA ITALIA O ESTERA
352800050111    1C     §15ITA        IFEQ      'I'
352900050111     C                   MOVEL     'I'           WDESIE            1
353000050111     C                   ELSE
353100050111     C                   MOVEL     'E'           WDESIE            1
353200050111    1C                   ENDIF
353300050111     C**
353400050111     C* Ragione sociale destinatario
353500050111     C**
353600060628     C                   MOVEL     *blanks       V1cRSD
353700050111     C* vedo se c'e' 2 parte della descrizione
353800050111     C                   MOVEL     'D'           WTRC
353900060217     C     Karb4         CHAIN(N)  Fiar401l                           32
354000050111     C* TROVATA
354100050112    1C     *IN32         IFEQ      *OFF
354200050112     C                   MOVE      ar4NOT        V1cRSD
354300050112    1C                   ENDIF
354400050112     C                   MOVEL     arbRSD        V1cRSD
354500050111     C                   MOVEL     ARBNAS        W005A             5
354600050805     C                   MOVEL     ARBlod        v1dlod
354700050805     C                   MOVEL     ARBlom        v1dlom
354800050118     c
354900050118     c                   if        arbnzd<>*blanks
355000050118     c                   movel     arbnzd        vidprd
355100050118     c                   else
355200050118     c                   movel(p)  arbprd        vidprd
355300050118     c                   endif
355400050715     c                   if        Vuniconzm<>*blanks
355500050715     c                   movel     Vuniconzm     vidprm
355600050118     c                   else
355700050118     c                   movel(p)  arbprm        vidprm
355800050118     c                   endif
355900050111     C**
356000050111     C* CODICE BOLLA
356100050111     C**
356200050111     C                   MOVEL     '3A'          COD
356300050111     C                   MOVEL(P)  ARBCBO        KEY
356400050111     C     KTAB          CHAIN     TABEL                              32
356500050111     C  N32              MOVEL     TBLUNI        DS3A
356600050111     C   32              CLEAR                   DS3A
356700050111     C                   MOVEL     §3ADES        DESCBO
356800050111     c*
356900050111     C                   MOVEL     §3ATB1        TIPTB1
357000050112    1C     TIPTB1        IFEQ      'A'
357100050311     C  n94              MOVEL     ARBKSC        VIDKLP
357200050311     C   94              MOVEL     ARBlna        VIDKLP
357300050111     C                   MOVE      ARBKSC        VIDKSC
357400050111     c*
357500050111     C* SE PGM NON RICHIAMATO VEDO SE PRESENTE CODICE DA O.R.M.
357600050112    2C     *IN11         IFEQ      *OFF
357700050317     C                   MOVEL     'TB'          ILRA2CVB
357800050111     c
357900050113     c* Non vale per tassaz. multipla
358000050113     c     wscelta       ifne      '9'
358100050113    3C     vsfaks        andgt     *ZEROS
358200050113     C                   MOVEL     vsfaks        VIDKLP
358300050113     C                   MOVE      vsfaks        VIDKSC
358400050111     C* Memorizzo che ho proposto da bollettazione
358500050111     C                   MOVEL     'S'           WORM              1
358600050112    3c                   endif
358700050111     c
358800050112    2C                   ENDIF
358900050112   x1C                   ELSE
359000050111     C                   MOVEL     ARBLNA        VIDKLP
359100050111     C                   MOVE      9999          VIDKSC
359200050317     C  N11              MOVEL     '2B'          ILRA2CVB
359300050112    1C                   END
359400050113     c
359500050113     c* Per tassazione multipla, vedo se c'e' codice
359600050125    1c                   if        wscelta='9' and tas9kc(1)<>*blanks
359700050113     c* Cerco il codice prevalente
359800050113     c                   z-add     1             aa
359900050113     c                   z-add     0             wmaggiore
360000050113     c                   clear                   wcodcli
360100050113     c                   clear                   wcodtar
360200050125     c* Propongo il cod cliente più presente, e poi di quello il cod.tar
360300050125     c* più presente se c'e'
360400050125    2c                   dow       tas9nk(aa)>0
360500050125    3c                   if        tas9nk(aa)>wmaggiore
360600050125     c                   movel     tas9k(aa)     wcodcli           7
360700050125     c                   z-add     tas9nk(aa)    wmaggiore         7 0
360800050125    3c                   endif
360900050125     c                   add       1             aa
361000050125    2c                   enddo
361100050125     c* Cerco il cod tariffa
361200050125     c                   z-add     1             aa
361300050125     c                   z-add     0             wmaggiore
361400050125    2c                   dow       tas9n(aa)>0
361500050125     c                   movel     tas9kc(aa)    w007a
361600050125     c                   move      tas9kc(aa)    w003a
361700050125    3c                   if        w007a=wcodcli  and w003a<>*blanks
361800050125    4c                   if        tas9n(aa)>wmaggiore
361900050125     c                   move      tas9kC(aa)    wcodtar
362000050113     c                   z-add     tas9n(aa)     wmaggiore         7 0
362100050125    4c                   endif
362200050125    3c                   endif
362300050113     c                   add       1             aa
362400050125    2c                   enddo
362500050113     c
362600050113     C                   MOVEL     wcodcli       VIDKLP
362700050113     C                   MOVE      wcodcli       VIDKSC
362800050113     c                   movel     'S'           worm
362900050113     c                   endif
363000050111     C*
363100050111     C* PRENDO TIPO CODICE VARIAZIONE BOLLA
363200050111     C                   MOVEL     '7L'          COD
363300050111     C                   MOVEL     *BLANKS       KEY
363400050317     C                   MOVEL     ILRA2CVB      KEY
363500050111     C     KTAB          CHAIN     TABEL                              32
363600050111     C  N32              MOVEL     TBLUNI        DS7L
363700050111     C   32              MOVEL     *BLANKS       DS7L
363800050111     C*
363900050112    1C     §7LRTA        IFEQ      'S'
364000050111     C                   SETON                                        12
364100050112    1C                   END
364200050111     c
364300050111     C                   MOVEL     ARBRSD        COMDES            8
364400050112     C*
364500050112      * Se è una bolla di rientro cerco il codice del mittente della mamma originale
364600050112     c                   Clear                   V1cccm
364700050112If  1c                   If        Arbfbr = 'S'
364800050112     c     Karb2         Chain     Fnlbl01l
364900050112If  2c                   If        %Found(Fnlbl01l)
365000050112     c     Kblp          Chain(N)  Fnblp01l
365100050112If  3c                   If        %found(Fnblp01l)
365200050112     c                   Clear                   Ds3aw
365300050112     c                   Movel     '3A'          Cod
365400050112     c                   Movel(p)  BlpCbo        Key
365500050112     c                   Exsr      CtrTab
365600050112If  4c                   If        Not *In32
365700050112     c                   Movel     TblUni        Ds3aw
365800050112    4c                   EndIf
365900050112     c                   Movel     w_§3atb1      UnoTb1
366000050112If  4c                   If        UnoTb1 = 'F' and BlpKsc <> *Zeros
366100050112     c                   Movel     BlpKsc        V1cccm
366200050112    4c                   EndIf
366300050112If  4c                   If        UnoTb1 = 'A' and BlpCcm <> *Zeros
366400050112     c                   Movel     BlpCcm        V1cccm
366500050112    4c                   EndIf
366600050112    3c                   EndIf
366700050112    2c                   EndIf
366800050112    1c                   EndIf
366900050126     c
367000050126      * ricerco tariffa di cartello + flag italia/estero
367100050126     c                   exsr      tarcar
367200050126     c                   eval      wfie = o27fie
367300050111     c*
367400050111     c* Tutti i campi sottostanti li carico solo per tassazione
367500050111     c*  bolla singola
367600050112    1c                   if        wscelta='9'
367700050112     c                   seton                                        04
367800050126   x1c                   else
367900050112     c                   setoff                                       04
368000941217     C**
368100941217     C* DIVISA MITTENTE
368200941217     C**
368300960528     C                   MOVEL     '15'          COD
368400050714     C**                 MOVEL(P)  ARBNZM        KEY
368500050714     C                   MOVEL(P)  Vuniconzm     KEY
368600941217     C                   EXSR      CTRTAB
368700941217     C  N32              MOVEL     TBLUNI        DS15
368800941217     C   32              CLEAR                   DS15
368900941217     C                   MOVEL     §15VLT        MITVLT            3
369000050112    2C     §15ITA        IFEQ      'I'
369100941217     C                   MOVEL     'I'           WMITIE            1
369200941217     C                   ELSE
369300941217     C                   MOVEL     'E'           WMITIE            1
369400050112    2C                   ENDIF
369500060223     c* Campo di comodo del cod tariffa impostato da fiar4
369600050201     c                   eval      Wsfact=vsfact
369700050111     C**
369800050714     C* LINEA DI partenza  UNICA
369900050111     C**
370000050714     C***  ARBLNP        CHAIN     AZORG01L                           32
370100050714     C     VUNICOLNP     CHAIN     AZORG01L                           32
370200941216     C*
370300020222     c                   eval      og143 = orgde3
370400020222
370500020222     C                   eval      lnpntw = §ogntw
370600980704     C                   MOVEL     ORGDIT        LNPDIT            3
370700941217     C**
370800911218     C* LINEA DI ARRIVO
370900941217     C**
371000911114     C     ARBLNA        CHAIN     AZORG                              32
371100911114     C  N32              MOVEL     ORGDES        DESLNA
371200911114     C   32              MOVEL     *BLANKS       DESLNA
371300070312     C   32              clear                   orgde3
371400070312     C   32              clear                   orgde8
371500020222
371600020222     c                   eval      og143 = orgde3
371700020222
371800020222     c                   eval      lnantw = §ogntw
371900971230     C                   MOVEL     ORGDIT        LNADIT            3
372000980704     C                   MOVEL     ORGDE8        OG148
372100050112    2C     §OGLPR        IFGT      *ZEROS
372200980704     C                   MOVEL     §OGLPR        LNALPR            3 0
372300980704     C                   ELSE
372400980704     C                   Z-ADD     ARBLNA        LNALPR
372500050112    2C                   ENDIF
372600070312     c* Memorizzo se GEO attivata, per scartare la stampa bolla
372700070312     c
372800941217     C**
372900911114     C* DATA SPEDIZIONE
373000941217     C**
373100941216     C                   MOVEL     '3'           G02ERR
373200941216     C                   Z-ADD     ARBDSP        G02INV
373300941216     C                   CALL      'XSRDA8'
373400941216     C                   PARM                    WLBDAT
373500941216     C*
373600941216     C                   Z-ADD     G02DAT        VIDDSP
373700941217     C**
373800941216     C* TIPO SERVIZIO
373900941217     C**
374000090604     c                   clear                   ds5e
374100941216     C                   MOVEL(P)  '5E'          COD
374200941216     C                   MOVEL     ARBTSP        KEY
374300941216     C     KTAB          CHAIN     TABEL                              32
374400090604     C  N32              MOVEL     TBLUNI        ds5e
374500090604     C  N32              MOVEL     §5ed08        V1DTSP
374600941216     C   32              CLEAR                   V1DTSP
374700960302     C* LA NATURA MERCE CHE INZIA CON 'BOLLA XXX COLLI' VIENE
374800960302     C*  EVIDENZIATA
374900960302     C     W005A         IFEQ      'BOLLA'
375000960302     C                   SETON                                        02
375100960302     C                   ELSE
375200960302     C                   SETOFF                                       02
375300960302     C                   ENDIF
375400941217     C**
375500941217     C* CONSEGNE PARTICOLARI
375600941217     C**
375700941219     C                   EXSR      DECCPA
375800050126     c**
375900050126     c* Se Raggruppamento c'e' lo visualizzo
376000050126     c**
376100130221     c                   if        arbxco >*zeros
376200050126     c                   seton                                        14
376300050126     c                   endif
376400050111     c*
376500911211     C*
376600911211     C   12              MOVEL     ARBLNA        VIDKLP
376700911211     C   12              MOVEL     9999          VIDKSC
376800050310     C   12              MOVE      TES(3)        VIDTES
376900050111     c
377000960208     C* SE CODICE GENERICO 99999 E 2 LINEA DISTRIBUZIONE PROPONGO
377100960208     C*  NON LA LINEA DI ARRIVO MA LA LINEA ORIGINALE
377200971230     C*  SOLO SE LNA BARTOLINI
377300050216     c
377400050216     C                   MOVEL     VIDKLP        WKLP
377500050216     C                   CLEAR                   WTISI             1
377600980704     C* 2 LINEA DISTRIBUZIONE
377700050216     C     VIDKSC        IFEQ      9999
377800050216     C     LNADIT        ANDEQ     'BAR'
377900050216     C     LNPDIT        ANDEQ     'BAR'
378000050216     C                   MOVE      ARBGVA        W001A             1
378100050216     C     W001A         IFEQ      'D'
378200050216     C                   MOVEL     'S'           WTISI             1
378300050216     C                   ENDIF
378400050216     C                   ENDIF
378500980704     C**
378600980704     C* SDI PER BAR O VICEVERSA MA NON EXPORT
378700050216     C     LNADIT        IFNE      LNPDIT
378800050216     c     lnantw        andne     'EEX'
378900050216     c     lnantw        andne     'EUP'
379000050216     c     lnantw        andne     'FED'
379100050216     c     lnantw        andne     'DPD'
379200050216     C                   MOVEL     'S'           WTISI
379300050216     C                   ENDIF
379400980704     C**
379500970710     C* CALL AL TISI95R PER VEDERE COSA PRENDERE
379600050112    1C     WTISI         IFEQ      'S'
379700970710     C                   CLEAR                   DSSI95
379800970710     C                   CLEAR                   DSLV13
379900970710     C                   MOVEL     ARBNZD        I95NAR
380000970710     C                   MOVEL     ARBLOD        I95LOC
380100970710     C                   MOVEL     ARBCAD        I95CAP
380200970710     C                   MOVEL     ARBPRD        I95PRV
380300970710     C                   MOVEL     ARBFFD        I95FFD
380400970710     C                   Z-ADD     ARBPKF        I95LKG
380500970710     C                   Z-ADD     ARBVLF        I95LMC
380600050112    2c                   if        lnpntw = 'PPT'
380700050714     C                   Z-ADD     VUNICOLNP     I95TFP
380800000623     C                   ELSE
380900050714     C                   Z-ADD     VUNICOTFP     I95TFP
381000050112    2C                   ENDIF
381100970710     C                   MOVEL     ARBTSP        I95TSP
381200970710     C                   MOVEL     '3'           I95TCN
381300971103     C                   MOVEL     ARBDSP        I95DAT
381400980602     C                   MOVEL     'S'           I95FRE
381500050112    2C     §3AFCA        IFNE      0
381600971201     C                   MOVEL     'S'           I95FCA
381700050112    2C                   ENDIF
381800971201     C                   MOVEL     §3ATB1        I95TPO
381900971201     C**
382000970710     C                   CALL      'FNLV13R'
382100970710     C                   PARM                    KPJBA
382200970710     C                   PARM                    DSLV13
382300970710     C                   PARM                    DSSI95
382400980704     C** NON C'E' ERRORE
382500050112    2c                   if        vidksc=9999
382600050112    3C     O13ERR        IFEQ      ' '
382700980704     C**
382800980704     C* PER 2 LINEA DISTRIBUZIONE
382900050112    4C     LNADIT        IFEQ      'BAR'
383000980704     C     LNPDIT        ANDEQ     'BAR'
383100970710     C                   MOVEL     O95CLA        VIDKLP
383200050112    4C                   ENDIF
383300980704     C* DA SDI PER BARTOLINI
383400050112    4C     LNPDIT        IFEQ      'SDI'
383500980704     C     LNADIT        ANDEQ     'BAR'
383600980704     C* --> UTILIZZO O50CLO SE E' LINEA SDI
383700980704     C     O95CLO        CHAIN     AZORG01L                           32
383800050112    5C     *IN32         IFEQ      *OFF
383900980704     C     ORGDIT        ANDEQ     'SDI'
384000980704     C                   MOVEL     O95CLO        VIDKLP
384100980704     C                   ELSE
384200980704     C                   Z-ADD     LNALPR        VIDKLP
384300050112    5C                   ENDIF
384400050112    4C                   ENDIF
384500980704     C* DA BARTOLINI IN SDI
384600050112    4C     LNPDIT        IFEQ      'BAR'
384700980704     C     LNADIT        ANDEQ     'SDI'
384800980704     C* --> UTILIZZO O50CLO SE E' LINEA SDI
384900980704     C     O95CLA        CHAIN     AZORG01L                           32
385000050112    5C     *IN32         IFEQ      *OFF
385100980704     C     ORGDIT        ANDEQ     'BAR'
385200980704     C                   MOVEL     O95CLA        VIDKLP
385300980704     C                   ELSE
385400980704     C                   Z-ADD     LNALPR        VIDKLP
385500050112    5C                   ENDIF
385600050112    4C                   ENDIF
385700980704     C**
385800050112   x3C                   ELSE
385900970710     C                   MOVEL     ARBLNA        VIDKLP
386000050112    3C                   ENDIF
386100050112    2C                   ENDIF
386200970710     C                   MOVEL     'S'           WCAL13            1
386300960208     C*
386400960208     C                   MOVEL     VIDKLP        WKLP
386500960208     C*
386600050112    1C                   ENDIF
386700911203     C*
386800911114     C                   MOVEL     *BLANKS       DESTAS
386900970409     C* SI NO DDT
387000970409     C                   MOVEL     'A'           WTRC
387100060217     C     KARB4         CHAIN(n)  FiAR4000                           32
387200031028     c                   If        Not *In32
387300031028     c                   Movel     Ar4Not        dsbl4a
387400031028     c                   Move      §B4Abm        wddt              1
387500031028     c                   EndIf
387600970409     C   32              MOVEL     'S'           WDDT
387700970409     C     WDDT          IFEQ      'S'
387800970409     C     WDDT          OREQ      'C'
387900031028     c     wddt          oreq      'P'
388000031028     c     wddt          oreq      'Q'
388100970409     C                   MOVEL     'DDT'         V1CDDT
388200970409     C                   ELSE
388300970409     C                   CLEAR                   V1CDDT
388400970409     C                   ENDIF
388500941216     C*
388600941216     C* PRENDO LE NOTE
388700941216     C                   MOVEL     '8'           WTRC
388800060217     C     KARB4         CHAIN(n)  FiAR4000                           32
388900941220     C  N32              MOVEL     AR4NOT        VIDNT1
389000941220     C   32              CLEAR                   VIDNT1
389100941216     C                   MOVEL     '9'           WTRC
389200060217     C     KARB4         CHAIN(n)  FiAR4000                           32
389300941220     C  N32              MOVEL     AR4NOT        VIDNT2
389400941220     C   32              CLEAR                   VIDNT2
389500941216     C*
389600950121     C                   MOVEL     ARBQFT        WQFT
389700950121     C                   MOVEL     ARBIAS        WIAS
389800950121     C                   MOVEL     ARBVAS        WVAS
389900941216     C                   CLEAR                   VIDTIC
390000941216     C                   CLEAR                   VIDCAS
390100941216     C                   CLEAR                   VIDVCA
390200941216     C* CHAIN SU C/ASSEGNO
390300941216     C     §3AFCA        IFNE      0
390400051115     C     KARB2         CHAIN     FiAR9000                           32
390500941216     C*
390600941220     C     *IN32         IFEQ      *OFF
390700941216     C                   MOVEL     AR9TIC        VIDTIC
390800941216     C                   MOVEL     AR9CAS        VIDCAS
390900941216     C                   MOVEL     AR9VCA        VIDVCA
391000941216     C                   ENDIF
391100941216     C                   ENDIF
391200911114     C*
391300010703     C                   CLEAR                   VIDRTN
391400060529     C**                 MOVE      'F'           WTRC
391500060529     C                   MOVE      'I'           WTRC
391600060217     C     KARB4         CHAIN(n)  FiAR401L                           33
391700010703     C     *IN33         IFEQ      *OFF
391800060529     C                   CLEAR                   DSBL4I
391900060529     C                   MOVEL     AR4NOT        DSBL4I
392000060529     C     §B4ida        IFNE      *BLANKS
392100010702     C                   SETON                                        15
392200060529     C                   MOVEL     §B4ida        VIDRTN
392300010702     C                   ENDIF
392400010702     C                   ENDIF
392500030728      * Colli
392600030728     c                   Z-Add     ArbNcl        VidNcl
392700030203
392800030203      * Cerco il record dei bancali x passare il n. dei bancali alla tassazione
392900030708      * Record 'BAN'
393000030708     c                   If        %Subst(ArbGva:1:1) = 'B'
393100030203     c                   Clear                   dAr5Ban
393200030708     c                   Eval      kAr5Trd = 'BAN'
393300071001     c     kFiar5        Chain     Fiar501l
393400030203     c                   If        %Found(Fiar501l)
393500030203     c                   Movel     Ar5Uni        dAr5Ban
393600030728     c     §Ar5Nba       Add       §Ar5Nb2       VidBan
393700030203     c                   EndIf
393800030708     c                   EndIf
393900030729
394000030729      * Cerco il record 'BNB'
394100030708     c                   If        %Subst(ArbGva:1:1) = 'O'
394200030708     c                   Clear                   dAr5Bnb
394300030708     c                   Eval      kAr5Trd = 'BNB'
394400071001     c     kFiar5        Chain     Fiar501l
394500030708     c                   If        %Found(Fiar501l)
394600030708     c                   Movel     Ar5Uni        dAr5Bnb
394700030728     c     §Ar5bNba      Add       §Ar5bNb2      VidBan
394800030729     c                   Z-Add     §Ar5bCor      VidCor
394900030729     c                   Eval      *In08 = *On
395000030708     c                   EndIf
395100030709     c                   EndIf
395200110629      * Memorizzo se presente tipo rek. "EMD" per passarlo alla tassazione
395300130924     c                   Clear                   dAr5emd
395400110629     c                   clear                   wemd
395500110629     c                   Eval      kAr5Trd = 'EMD'
395600130924     c     kFiar5        chain     Fiar501l
395700130924     c                   if        %found(fiar501l)
395800130924     c                   Movel     Ar5Uni        dAr5emd
395900130924     c                   select
396000130924     c                   when      §ar5mmp='S' and §ar5smp=' '
396100110629     c                   eval      wemd='S'
396200130924     c                   when      §ar5mmp='S' and §ar5smp='S'
396300130924     c                   eval      wemd='E'
396400130924     c                   when      §ar5smp='S'
396500130924     c                   eval      wemd='X'
396600130924     c                   endsl
396700110629     c                   endif
396800150107     C* Verifico presenza Pin Code
396900150108     c                   clear                   ds7r
397000150108     c                   clear                   wpincode
397100150108     c                   if        arbgma<>*blanks
397200150108     C                   MOVEL     '7R'          COD
397300150108     C                   MOVEL(p)  ARBgma        KEY
397400150108     C     KTAB          CHAIN     TABEL
397500150108     c                   if        %found(tabel00f)
397600150108     C                   MOVEL     TBLUNI        DS7R
397700150108     c                   endif
397800150108     c                   endif
397900150108     c                   if        §7rpincode='S'
398000150107     c                   eval      wpincode='S'
398100150107     c                   endif
398200010702     C*
398300050112     c                   ENDIF
398400100119     C*
398500911114     C                   ENDSR
398600030507      *---------------------------------------------------------------*
398700030507      * Recupero i dati del cliente                                   *
398800030507      *---------------------------------------------------------------*
398900030507     c     Sr_RecDati    BegSr
399000100120     c                   clear                   wpiva
399100030507
399200030507      * Se non è un cliente 9999 cerco i dati anagrafici e li imposto a video
399300030507If  1c                   If        VidKsc <> 9999
399400030507      * Cliente variato
399500030507If  2c                   If        VidKsc <> Sav_VidKsc
399600030507     c                   Movel     VidKlp        ComKsc
399700030507     c                   Move      VidKsc        ComKsc
399800030507     c                   Exsr      Sr_AcoKsc
399900030507
400000030507     c                   Clear                   VidVia
400100050114     c                   Clear                   V6dVia
400200030507     c                   Clear                   VidLoc
400300050114     c                   Clear                   V6dLoc
400400050114     c                   Clear                   VidCae
400500050114     c                   Clear                   V6dCae
400600030507     c                   Clear                   VidPrv
400700030507     c                   Clear                   VidNota
400800050114     c                   Clear                   V6dNota
400900030507If  3c                   If        O69Err <> '1' and AcoFlg = *Blanks
401000030507     c                   Movel     AcoRag        ComDes
401100030507     c                   Movel     IndVia        VidVia
401200030507     c                   Movel     IndCit        VidLoc
401300050114     c                   movel     IndCae        VidCae
401400050114     c
401500050114     c                   Movel     IndVia        V6dVia
401600050114     c                   Movel     IndCit        V6dLoc
401700050114     c                   movel     IndCae        V6dCae
401800030507     c                   If        IndPrv <> *Blanks
401900030507     c                   Movel     IndPrv        VidPrv
402000030507     c                   Else
402100030507     c                   Movel     IndSta        VidPrv
402200030507     c                   EndIf
402300100120     c* Partita IVA
402400100120     c                   movel     indiva        wpiva
402500030507      * Nota
402600030507     c                   Movel     Kci           kNtcNk1
402700030507     c                   Move      ComKsc        kNtcNk1
402800030507     c     Kntc          Setll     Tfntc01l
402900030507Do  4c                   Do        *Hival
403000030507     c     Kntc          Reade     Tfntc01l
403100030507     c                   If        %Eof(Tfntc01l)
403200030507     c                   Leave
403300030507     c                   EndIf
403400030507     c                   If        NtcFlt <> 'A'
403500030507     c                   Movel     NtcRnt        VidNota
403600050114     c                   Movel     NtcRnt        V6dNota
403700030507     c                   Leave
403800030507     c                   EndIf
403900030507    4c                   EndDo
404000030507
404100030507    3c                   EndIf
404200030507      * salvo il codice cliente impostato
404300030507     c                   Eval      Sav_VidKsc = VidKsc
404400061122      * salvo le spese di incasso solo se codice impostato e senza errore
404500061122     c                   if        comksc >0  and o69err<>'1'
404600030605     c                   Eval      Sav_indsin = indsin
404700030507    2c                   EndIf
404800061122    2c                   EndIf
404900030507      * Se è un cliente 9999 pulisco i campi del video
405000030507   x1c                   Else
405100030507     c                   Clear                   VidVia
405200030507     c                   Clear                   VidLoc
405300050114     c                   Clear                   VidCae
405400030507     c                   Clear                   VidPrv
405500030507     c                   Clear                   VidNota
405600050114     c                   Clear                   V6dVia
405700050114     c                   Clear                   V6dLoc
405800050114     c                   Clear                   V6dCae
405900050114     c                   Clear                   V6dNota
406000100120     c*
406100100120     C* CARICO LA PARTITA IVA: sempre da ARBCPI
406200100120     c                   movel     arbcpi        wpiva
406300030507    1c                   EndIf
406400100120     c
406500100120     C     'PRIVATO'     SCAN      WPIVA         posiz                    30
406600100120     c                   clear                   vidpid
406700100120     c                   select
406800100120     c* solo numeri
406900100120     c* "PRIVATO senza iso
407000100120     c     *in30         wheneq    *off
407100100120     c     wiso          andge     *zeros
407200100120     c     *in30         oreq      *on
407300100120     c     posiz         andlt     3
407400100120     c                   movel     wpiva         vidcpd
407500100120     c* iso + numeri
407600100120     c* iso + "PRIVATO"
407700100120     c     *in30         wheneq    *off
407800100120     c     posiz         orgt      2
407900100120     c                   movel     wpiva         vidpid
408000100120     c                   endsl
408100100120     C*
408200030507
408300030507      * Condiziono l'uscita della costante 'Desc.'
408400050114     c                   If         Vidnota <> *Blanks
408500030507     c                   Eval      *In46 = *On
408600030507     c                   Else
408700030507     c                   Eval      *In46 = *Off
408800030507     c                   EndIf
408900030507
409000030507     c                   EndSr
409100030506      *---------------------------------------------------------------*
409200030506      * Ricerco i dati del cliente fatturazione                       *
409300030506      *---------------------------------------------------------------*
409400030506     c     Sr_AcoKsc     BegSr
409500030506
409600030506     C                   clear                   TIBS69DS
409700030506     C                   z-add     comksc        I69kac
409800030506     C                   z-add     comksc        I69kin
409900030506     C                   call      'TIBS69R'
410000030506     C                   parm                    tibs69DS
410100030506     C                   parm                    ds_cnaco
410200030506     C                   parm                    ds_cnind
410300030506     C                   parm                    ds_cnclp
410400030506     C                   parm                    ds_fncls
410500030506
410600030506     c                   EndSr
410700911114     C*
410800911114     C*--- CONTROLLO CAMPI VARIATI -----------------------------------*
410900911114     C     CONTR3        BEGSR
411000941217     C                   SETOFF                                       90
411100941216     C                   CLEAR                   WTKS
411200010327     C                   CLEAR                   WIDMSG
411300930422     C*
411400930422     C                   MLLZO     1             VIDKSC
411500941216     C*
411600941216     C                   CLEAR                   DSLR66
411700050112     C                   MOVEL     VsfNSP        D66NSP
411800050112     C                   MOVEL     VsfAAS        D66AAS
411900050112     C                   MOVEL     VsfLNP        D66LNP
412000050112     C                   MOVEL     VsfNRS        D66NRS
412100941216     C**
412200941216     C* CAUSALE VARIAZIONE BOLLE
412300941216     C**
412400941219     C                   CLEAR                   V3DCVB
412500941216    1C     VIDCVB        IFNE      *BLANKS
412600941216     C*
412700941217     C     '?'           SCAN      VIDCVB                                 90
412800941216     C* RICERCA
412900941216    2C     *IN90         IFEQ      *ON
413000941216     C                   MOVEL     'V'           D66GES
413100941216     C                   MOVEL     'A'           D66TBO
413200941216     C*
413300941216     C                   MOVEL     DSLR66        KPJBU
413400941216     C                   CALL      'FNLR66R'
413500941216     C                   PARM                    KPJBA
413600941216     C                   MOVEL     KPJBU         DSLR66
413700941216     C                   MOVEL     D66CVB        VIDCVB
413800941216     C                   MOVEL     D66DES        V3DCVB
413900941219     C                   GOTO      ENDCT3
414000941216   X2C                   ELSE
414100941216     C* CONTROLLO
414200941216     C                   CLEAR                   DSLR66
414300941216     C                   MOVEL     'C'           D66GES
414400941216     C                   MOVEL     'A'           D66TBO
414500941216     C                   MOVEL     VIDCVB        D66CVB
414600941216     C*
414700941216     C                   MOVEL     DSLR66        KPJBU
414800941216     C                   CALL      'FNLR66R'
414900941216     C                   PARM                    KPJBA
415000941216     C                   MOVEL     KPJBU         DSLR66
415100941216     C                   MOVEL     D66CVB        VIDCVB
415200941216     C                   MOVEL     D66DES        V3DCVB
415300941216     C*
415400941216     C* ERRORE
415500941216    3C     D66MSG        IFNE      *BLANKS
415600941217     C                   MOVEL     D66MSG        VIDMSG                         CAMPO MESSAGGI
415700941216     C                   SETON                                        284090
415800941216     C                   GOTO      ENDCT3
415900941216    3C                   ENDIF
416000941219    2C                   ENDIF
416100941219     C*
416200941219     C* RICHIAMO PGM DI VARIAZIONE SE IMPOSTATO IL CODICE
416300941219     C                   UNLOCK    FNARB01L
416400941219     C*
416500941219     C                   CLEAR                   DSLR48
416600050112     C                   MOVEL     VsfAAS        D48AAS
416700050112     C                   MOVEL     VsfLNP        D48LNP
416800050112     C                   MOVEL     VsfNRS        D48NRS
416900050112     C                   MOVEL     VsfNSP        D48NSP
417000050112     C                   MOVEL     VidFIL        D48FGS
417100941219     C                   MOVEL     ARBCBO        D48CBO
417200071031     C                   MOVEL     vidcvb        D48CVB
417300941222     C                   MOVEL     'A'           D48TBO
417400950619     C                   MOVEL     'S'           D48F12
417500950619     C                   MOVEL     'V'           D48FFR
417600941219     C                   MOVEL     DSLR48        KPJBU
417700941219     C                   CALL      'FNLR48R'
417800941219     C                   PARM                    KPJBA
417900941222     C                   PARM                    DSARBD
418000941222     C                   PARM                    DSARBK
418100990923     C                   PARM                    DARBT
418200941222     C                   PARM                    DSARBG
418300030613     C                   PARM                    TRUL90DS
418400941219     C*
418500941219     C                   MOVEL     KPJBU         DSLR48
418600950123     C*
418700950123     C     KARB2         CHAIN     FNARB000                           32
418800051115     C     KARB2         CHAIN     FiAR9000                           32
418900941219     C* ERRORE
419000941219     C     D48ERR        IFEQ      '1'
419100941219     C                   MOVEL     D48MSG        VIDMSG
419200941219     C                   SETON                                        289040
419300941219     C                   ELSE
419400941219     C                   CLEAR                   VIDCVB
419500941219     C                   CLEAR                   V3DCVB
419600941216    1C                   ENDIF
419700941219     C*
419800950221     C                   SETON                                        9042
419900941219     C                   GOTO      ENDCT3
420000941219    1C                   ENDIF
420100941216     C* CONTROLLO
420200941216    1C     VIDKLP        IFEQ      0
420300941216     C                   MOVEL     MSG(4)        VIDMSG
420400941216     C                   SETON                                        419028
420500941216     C                   GOTO      ENDCT3
420600941216    1C                   END
420700941216     C*
420800941216     C                   MOVEL     VIDKLP        COMKSC
420900941216     C                   MOVE      VIDKSC        COMKSC
421000941216     C*
421100030506     c                   Exsr      Sr_AcoKsc
421200941216     C* ERRORE
421300030506     c     O69Err        Ifeq      '1'
421400030506     c     AcoFlg        Orne      *Blanks
421500941216     C     VIDKSC        OREQ      8888
421600941216     C                   MOVEL     MSG(6)        VIDMSG
421700941216     C                   SETON                                        429028
421800941216     C                   GOTO      ENDCT3
421900941216    2C                   ENDIF
422000941216     C*
422100130320    2C***  ACOABL        IFEQ      '8'
422200130320    2C***  ACOABL        OREQ      '*'
422300130320    2C     ACOABL        ifne      ' '
422400941216     C                   MOVEL     MSG(7)        VIDMSG
422500941216     C                   SETON                                        429028
422600941216     C                   GOTO      ENDCT3
422700941216    2C                   END
422800090428     c
422900090428     c                   movel     c_soc         pjsocieta
423000090428     c                   move      *zeros        pjkcc
423100090428     c                   move      kci           pjkcc
423200090428     c                   if        chkContoIncassiAttribuire(pjSocieta:pjKcc:
423300090428     c                             '0'+%editc(comksc:'X'):pjesito) <> 0
423400090428     C                   MOVEL     MSG(6)        VIDMSG
423500090428     C                   SETON                                        429028
423600090428     C                   GOTO      ENDCT3
423700090428     c                   endif
423800941216     C*
423900941216     C* CARICO LE TARIFFE DEL CLIENTE
424000981008    2C     VIDKSC        IFNE      9999
424100941216     C                   EXSR      CARCTR
424200981008   X2C                   ELSE
424300941216     C* CODICE 9999
424400050120     c* non ammesso per tassazione multipla o tassazione singola con 'X'
424500050120     c                   if        wscelta='9' or wscelta='X'
424600050120     C                   MOVEL     MSG(65)       VIDMSG
424700050112     C                   SETON                                        429028
424800050112     C                   GOTO      ENDCT3
424900050112     c                   endif
425000050310     c* Non ammesso per tassazione in partenza
425100050310     c                   if        *in94
425200050310     C                   MOVEL     MSG(74)       VIDMSG
425300050310     C                   SETON                                        429028
425400050310     C                   GOTO      ENDCT3
425500050310     c                   endif
425600050112     c
425700941216     C                   MOVEL     '9'           WTKS
425800981008     C* SE 9999 LA LNP CODICE DEVE ESSERE = ALLA LNA DELLA BOLLA
425900981008    3C     VIDKLP        IFNE      ARBLNA
426000981008     C                   MOVEL     MSG(44)       VIDMSG
426100981008     C                   SETON                                        419028
426200981008     C                   GOTO      ENDCT3
426300981008    3C                   END
426400981008    2C                   END
426500050120     c* Per tassazione multipla o tassazione singola con 'X'
426600050120     c*                         non accetto cliente senza tariffe
426700050120     c                   if        (wscelta='9' and wtks=' ') or
426800050120     c                             (wscelta='X' and wtks=' ')
426900050120     C                   MOVEL     MSG(50)       VIDMSG
427000050120     C                   SETON                                        429028
427100050120     C                   GOTO      ENDCT3
427200050120     c                   endif
427300941216     C*
427400941216     C** SALVO ULTIMO CODICE IMPOSTATO
427500941216    3C     COMKSC        IFNE      SAVKSC
427600941216     C                   CLEAR                   CIN
427700941216     C                   CLEAR                   DIE
427800941217     C                   MOVEL     COMKSC        SAVKSC            7 0
427900000202     C* VEDO SE CLIENTE DPD
428000000202     C                   MOVEL     COMKSC        W0030             3 0
428100000202     C* CLIENTE DPD
428200000202     C     W0030         CHAIN     AZORG01L                           32
428300020222     c                   eval      og143 = orgde3
428400020222     c                   eval      clintw = §ogntw
428500941216    3C                   END
428600000911     C* SE CIENTE S.F. POSTE, NON ACCETTO IL CODICE
428700020222     c                   if        clintw = 'PPT'
428800000911     C                   MOVEL     MSG(49)       VIDMSG
428900000911     C                   SETON                                        419028
429000000911     C                   GOTO      ENDCT3
429100000911     C                   ENDIF
429200040930     c* Se cliente con ntw "LOG o "XXX" non posso utilizzare
429300040930     c                   if        clintw = 'LOG'  or clintw='XXX'
429400040930     C                   MOVEL     MSG(61)       VIDMSG
429500040930     C                   SETON                                        419028
429600040930     C                   GOTO      ENDCT3
429700040930     C                   ENDIF
429800051013     c                   if        vidksc=9999 and (clintw='FED'or
429900051013     c                             clintw='EEX' or clintw='DPD')
430000051013     C                   MOVEL     MSG(82)       VIDMSG
430100051013     C                   SETON                                        284190
430200051013     C                   GOTO      endct3
430300051013     c                   endif
430400911114     C*
430500911114     C     ENDCT3        ENDSR
430600911028     C*
430700911028     C*--- CARICO CODICI TARIFFA -------------------------------------*
430800911028     C     CARCTR        BEGSR
430900911028     C                   SETOFF                                       01
431000021121
431100021121      * Carico nuovamente la tariffa di cartello + flag italia/estero
431200021121     c                   Exsr      Tarcar
431300021121     c                   eval      wfie = o27fie
431400021121
431500911028     C                   MOVEL     *BLANKS       DE2CTR
431600050128     C                   CLEAR                   fnlv59ds
431700941216     C*
431800050128     C                   MOVEL     'A'           ilv59tbo
431900050128      * Se bolla monovaria carico tutte le tariffe ma propongo la partenza
432000040511     C                   IF        §3ASVA <> *BLANK
432100050128     C                   movel     ' '           ilv59tbo
432200050128     C                   movel     'P'           ilv59prp
432300040511     C                   ENDIF
432400040511      *
432500050128     C                   MOVEL     COMKSC        ilv59KSC
432600050128     C                   MOVEL     KCI           ilv59KCI
432700050128     C                   MOVEL     WFIE          ilv59FIE
432800160401     c                   select
432900160401     c                   when      o27ntw='DPD'
433000160401     C                   MOVEL     'D'           ilv59ta3
433100160401     c                   when      o27ntw='EEX'
433200160401     C                   MOVEL     'E'           ilv59ta3
433300160401     c                   other
433400160401     C                   MOVEL     wfie          ilv59ta3
433500160401     c                   endsl
433600160401     c
433700050128     C                   MOVEL     ARBDSP        ilv59DSP
433800050125     c                   if        wscelta<>'9'
433900050128     C                   MOVEL     ARBTSP        ilv59TSP
434000050125     c                   else
434100050128     c                   movel     tas9t(1)      ilv59tsp
434200050125     c                   endif
434300050128     C                   MOVEL     'S'           ilv59RIC
434400010327     C* SE HO CODICE TARIFFA DA O.R.M. E A VIDEO E' STATO MANTENUTO
434500010327     C* IL CODICE CLIENTE DA ORM LO PASSO AL PGM
434600050113     c                   if        worm='S'
434700050201     c                   if        wscelta<>'9' and Wsfact>=*zeros
434800050113     C                   MOVEL     VIDKLP        W007a             7
434900050113     C                   MOVE      VIDKSC        W007a
435000050113     C     vsfaks        IFEQ      W007a
435100050201     C                   MOVE      Wsfact        ilv59CTR
435200020527     C                   MOVEL     MSG(51)       WIDMSG
435300020527     C                   ENDIF
435400050113     C                   ENDif
435500050113     c*
435600050113     c                   if        wscelta='9'  and wcodtar>=*zeros
435700050113     C                   MOVEL     VIDKLP        W007a             7
435800050113     C                   MOVE      VIDKSC        W007a
435900050113     C     wcodcli       IFEQ      W007a
436000050128     C                   MOVE      wcodtar       ilv59CTR
436100050113     C                   MOVEL     MSG(51)       WIDMSG
436200050113     C                   ENDIF
436300050113     C                   ENDif
436400050113     c*
436500050113     c                   endif
436600010327     C*
436700050128     C                   CALL      'FNLV59R'
436800050128     C                   PARM                    fnlv59ds
436900050124     c
437000050124     c* Se trovato errore ed impostato il cod.tariffa lo ignoro
437100050128     c*  e faccio proporre il cod. tariffa dal pgm fnlv59r
437200050124     c                   if        wscelta='X'  and
437300050128     c                             olv59err<>*blanks  and ilv59ctr>=*zeros
437400050128     c                   clear                   ilv59CTR
437500050201     c                   clear                   Wsfact
437600050124     c                   clear                   widmsg
437700050128     C                   CALL      'FNLV59R'
437800050128     C                   PARM                    FNLV59DS
437900050124     c                   endif
438000050125     c*
438100050128     C                   MOVEL     olv59CTR      VIDCTR
438200050128     C                   MOVEL     olv59DFS      DE2CTR
438300050125     c
438400050125     c* Per tassazione multipla se sono presenti bolle con più tipi servizio
438500050125     c*  e non proposto da bolla il cod tariffa, vedo se il tipo servizio
438600050125     c*  Mi restituisce un codtariffa <> --> se si errore forzabile
438700050125    1c                   if        wscelta='9' and wcodtar=*blanks  and
438800050201     c                             tas9t(2)<>*blanks  and olv59ct3<>*blanks
438900050201     c                   eval      wcodtar1=olv59ctr
439000050201     c                   clear                   ilv59ctr
439100050201     c                   movel     tas9t(2)      ilv59tsp
439200050201     C                   CALL      'FNLV59R'
439300050201     C                   PARM                    fnlv59ds
439400050201    2c                   if        olv59ctr<>wcodtar1
439500050125     C                   MOVEL     MSG(68)       WIDMSG
439600050125   x2c                   else
439700050125    3c                   if        tas9t(3)<>*blanks
439800050201     c                   clear                   ilv59ctr
439900050201     c                   movel     tas9t(3)      ilv59tsp
440000050201     C                   CALL      'FNLV59R'
440100050201     C                   PARM                    fnlv59ds
440200050201    4c                   if        olv59ctr<>wcodtar1
440300050125     C                   MOVEL     MSG(68)       WIDMSG
440400050125    4c                   endif
440500050125    3c                   endif
440600050125    2c                   endif
440700050125    1c                   endif
440800941216     C*
440900050128     C     olv59TKS      IFNE      ' '
441000941219     C                   MOVEL     'S'           WTKS
441100941216     C                   ENDIF
441200941216     C*
441300050128     C                   MOVEL     olv59CT2      VIDCT2
441400050128     C                   MOVEL     olv59CT3      VIDCT3
441500050128     C                   MOVEL     olv59CT4      VIDCT4
441600050128     C                   MOVEL     olv59CT5      VIDCT5
441700050128     C                   MOVEL     olv59ES2      VIDES2
441800050128     C                   MOVEL     olv59ES3      VIDES3
441900050128     C                   MOVEL     olv59ES4      VIDES4
442000050128     C                   MOVEL     olv59ES5      VIDES5
442100911028     C*
442200911028     C                   ENDSR
442300020222      *---------------------------------------------------------------*
442400160226      * Cerco flag caricamento tariffe cliente
442500020222      *---------------------------------------------------------------*
442600020222     c     tarcar        begsr
442700020222
442800160226     c                   clear                   trul27ds1
442900021121     c                   Eval      I27Pkg = ArbPkf
443000020222     c                   eval      i27tsp = arbtsp
443100020222     c                   eval      i27lna = arblna
443200050714     c***                eval      i27lnp = arblnp
443300050714     c                   eval      i27lnp = VUNICOLNP
443400050714     c
443500020222     c                   movel     vidklp        i27cli
443600020222     c                   move      vidksc        i27cli
443700020222
443800160226     c                   call      'TRUL27R1'
443900160226     c                   parm                    trul27ds1
444000020222
444100020222     c                   endsr
444200020222
444300911028     C*
444400991013     C* CARICO LE VARIE DEL VIDEO IN SCHIERA DI COMODO ---------------*
444500991013     C     CARWAR        BEGSR
444600991013     C                   CLEAR                   WINL
444700991013     C                   CLEAR                   WSV
444800991013     C                   CLEAR                   WVA
444900991013     C                   CLEAR                   C
445000991013    2C                   DO        20            NRR
445100050317     C     NRR           CHAIN     LRA2S06                            32
445200991013     C*
445300991013    3C     *IN32         IFEQ      *OFF
445400991013     C     V6CSV1        ANDNE     *BLANKS
445500991013     C                   ADD       1             C
445600991013     C                   MOVEL     V6CSV1        WSV(C)
445700991013     C                   Z-ADD     V6CVA1        WVA(C)
445800991013    3C                   ENDIF
445900991013    2C                   ENDDO
446000991013     C* SE RICHIESTA IL CONTROLLO ESISTENZA DELL'INOLTRO PROCEDO
446100991013     C     WCKINL        IFEQ      '1'
446200991013     C                   Z-ADD     1             II
446300991013     C     §$2FIN        LOOKUP    WSV(II)                                32
446400991013     C   32WVA(II)       IFGT      *ZEROS
446500991013     C                   MOVE      '1'           WINL              1
446600991013     C                   ENDIF
446700991013     C                   ENDIF
446800991013     C                   ENDSR
446900040121      *---------------------------------------------------------------*
447000040121      * Carico le varie della seconda bolla in una sk di comodo
447100040121      *---------------------------------------------------------------*
447200040121     c     Carwar1       Begsr
447300040121
447400040121     c                   Clear                   wsv
447500040121     c                   Clear                   wva
447600040121     c                   Clear                   c
447700040121
447800040121     c                   If        vi2sv1 <> *Blanks
447900050401     c                   add       1             c
448000050401     c                   Movel     vi2Sv1        wsv(c)
448100050401     c                   Z-add     vi2va1        wva(c)
448200040121     c                   EndIf
448300040121     c                   If        vi2sv2 <> *Blanks
448400050401     c                   add       1             c
448500050401     c                   Movel     vi2Sv2        wsv(c)
448600050401     c                   Z-add     vi2va2        wva(c)
448700040121     c                   EndIf
448800040121     c                   If        vi2sv3 <> *Blanks
448900050401     c                   add       1             c
449000050401     c                   Movel     vi2Sv3        wsv(c)
449100050401     c                   Z-add     vi2va3        wva(c)
449200040121     c                   EndIf
449300040121     c                   If        vi2sv4 <> *Blanks
449400050401     c                   add       1             c
449500050401     c                   Movel     vi2Sv4        wsv(c)
449600050401     c                   Z-add     vi2va4        wva(c)
449700040121     c                   EndIf
449800041129     c                   If        vi2sv5 <> *Blanks
449900050401     c                   add       1             c
450000050401     c                   Movel     vi2Sv5        wsv(c)
450100050401     c                   Z-add     vi2va5        wva(c)
450200041129     c                   EndIf
450300041129     c                   If        vi2sv6 <> *Blanks
450400050401     c                   add       1             c
450500050401     c                   Movel     vi2Sv6        wsv(c)
450600050401     c                   Z-add     vi2va6        wva(c)
450700041129     c                   EndIf
450800040121
450900040121     c                   EndSr
451000050112     c
451100911028     C*--- CONTROLLO CODICE TARIFFA ----------------------------------*
451200911028     C     CONTR4        BEGSR
451300911028     C                   SETOFF                                       90
451400941216     C*
451500941216     C* CONTROLLO CODICE TARIFFA
451600050128     C                   CLEAR                   fnlv59ds
451700050128     C                   MOVEL     'A'           ilv59TBO
451800050128      * se bolla monovaria carico tutte le tariffe ma propongo
451900040511     C                   IF        §3ASVA <> *BLANK
452000050128     C                   movel     ' '           ilv59tbO
452100040511     C                   ENDIF
452200040511      *
452300050128     C                   MOVEL     ARBDSP        ilv59DSP
452400050128     c                   if        wscelta<>'9'
452500050128     C                   MOVEL     ARBTSP        ilv59TSP
452600050128     c                   else
452700050128     c                   movel     tas9t(1)      ilv59tsp
452800050128     c                   endif
452900050128     C                   MOVEL     KCI           ilv59KCI
453000050128     C                   MOVEL     VIDCTR        ilv59CTR
453100050128     C                   MOVEL     COMKSC        ilv59KSC
453200050128     C                   MOVEL     WFIE          ilv59FIE
453300050128     C                   MOVEL     'S'           ilv59CON
453400050201     C                   CALL      'FNLV59R'
453500050201     C                   PARM                    fnlv59ds
453600950121     C* VEDO SE C'E' STATA LA RICERCA
453700050128     C     olv59ERR      IFEQ      'R'
453800050128     C                   MOVEL     olv59CTR      VIDCTR
453900950121     C                   SETON                                        5590
454000950121     C                   GOTO      ENDCT4
454100950121     C                   ENDIF
454200941216     C*
454300050128     C     olv59ERR      IFNE      *BLANKS
454400050128     C                   MOVEL     olv59MSG      VIDMSG
454500941216     C                   SETON                                        289055
454600941216     C                   GOTO      ENDCT4
454700941216     C                   ENDIF
454800941216     C*
454900941216     C* DESCRIZIONE CODICE TARIFFA
455000050128     C                   MOVEL     olv59DFS      DE2CTR
455100050128     c* cod.tariffa cliente + attributi
455200050128     c                   movea     olv59ctt      ctrw
455300050128     c                   movea     olv59at1      fiew
455400050128     c                   movea     olv59at2      tspw
455500050128     c                   movea     olv59at3      bapw
455600050128     c                   movea     olv59at4      tfpw
455700050128     c                   movea     olv59at5      valw
455800941216     C*
455900941216     C* SE TARIFFA TROVATA -- CHAIN SU TNTAM E PRENDO DESCRIZIONE VAR
456000050128     C                   MOVEL     olv59CTR      COMCTR
456100050128     C                   MOVEL     olv59PRG      COMPRG
456200941216     C*
456300941220     C     KTAM3         CHAIN     TNTAM000                           32
456400941220     C     *IN32         IFEQ      *ON
456500941216     C                   MOVEL     MSG(10)       VIDMSG
456600941216     C                   SETON                                        289055
456700941216     C                   GOTO      ENDCT4
456800941216     C                   ENDIF
456900941216     C*
457000950421     C     TAMDCV        IFNE      *BLANKS
457100941216     C                   MOVEL     TAMDCV        DE2CTR
457200950421     C                   END
457300991028     C                   MOVEL     TAMFLO        DSTA01
457400050112     c
457500050114     C* Se tassazione multimpla impossibile scegliere codice con fattura
457600050112     c*  immediata impostato in tariffa
457700050124     c                   if        (tamfts='2' and wscelta='9') or
457800050124     c                             (tamfts='2' and wscelta='X')
457900050112     C                   MOVEL     MSG(64)       VIDMSG
458000050112     C                   SETON                                        289055
458100050112     C                   GOTO      endct4
458200050112     c                   endif
458300050310     C* Se tassazione in partenza impossibile scegliere codice con fattura
458400050310     c*  immediata impostato in tariffa
458500050310     c                   if        tamfts='2' and *in94
458600050310     C                   MOVEL     MSG(75)       VIDMSG
458700050310     C                   SETON                                        289055
458800050310     C                   GOTO      endct4
458900050310     c                   endif
459000051013     C* Se tassazione codice estero impossibile scegliere codice con fattura
459100051013     c*  immediata impostato in tariffa
459200051013     c                   if        tamfts='2' and (clintw='FED' or
459300051013     c                             clintw='EEX' or clintw='DPD')
459400051013     C                   MOVEL     MSG(81)       VIDMSG
459500051013     C                   SETON                                        289055
459600051013     C                   GOTO      endct4
459700051013     c                   endif
459800050128     c*
459900050128     c*
460000050128    1c                   if        wscelta='X'
460100050201     c                   if        worm='S' and Wsfact>=*zeros
460200050124     C                   MOVEL     VIDKLP        W007a             7
460300050124     C                   MOVE      VIDKSC        W007a
460400050124     c                   endif
460500050124     c
460600050128    2c                   select
460700050128     c* Se cod tariffa preimpostato --> ok
460800050201     c                   when      worm='S'   and  Wsfact>=*zeros and
460900050124     c                             vsfaks=w007a
461000050128     c* Se presente una sola tariffa --> ok
461100050128     c                   when      ctrw(2)=*blanks
461200050128   x2c                   other
461300050128     c* Per monovaria ok anche se c'e' solo una tariffa Partenza
461400050128     c                   clear                   wconta            3 0
461500050128    3c                   if        §3asva<>' '
461600050128     c                   z-add     1             aa
461700050128    4c                   dow       ctrw(aa)<>*blanks
461800050128     c                   if        bapw(aa)<>'A'
461900050128     c                   add       1             wconta
462000050128     c                   endif
462100050128     c                   add       1             aa
462200050128    4c                   enddo
462300050128    3c                   endif
462400050128     c
462500050128     c* WCONTA=0 bolla non monovaria  o
462600050128     c*          bolla     monovaria con nessuna    tariffa partenza
462700050128     c* WCONTA=1 bolla     monovaria con una sola   tariffa Partenza --> ok
462800050128     c* WCONTA>1 bolla     monovaria con più di una tariffa Partenza --> ok
462900050128    3C                   IF        wconta>1 or wconta=0
463000050128     c* Per essere ok devo avere una sola tariffa dello stesso tipo
463100050128     c*  servizio della bolla da tassare
463200050128    4C                   IF        ARBTSP=TAMTSP
463300050128     C                   Z-ADD     1             AA
463400050128     c     ARBTSP        LOOKUP    TSPW(AA)                               32
463500050128     c
463600050128    5C                   dow       *IN32 and not *in90
463700050128    6C                   if        CTrW(AA)<>VIDCTR
463800050128     c
463900050128     c* Per monovaria con più tariffe Part errore solo se tariffa "P"
464000050128    7c                   if        (wconta>1 and bapw(aa)<>'A') or
464100050128     c                              wconta=0
464200050128     C                   SETON                                        2890
464300050128    7c                   endif
464400050128    6c                   endif
464500050128     c
464600050128     c                   add       1             aa
464700050128     c
464800050128     c     ARBTSP        LOOKUP    TSPW(AA)                               32
464900050128    5c                   enddo
465000050128     C*
465100050128   x4c                   else
465200050124     C                   SETON                                        2890
465300050128    4c                   endif
465400050128    3c                   endif
465500050128    2c                   endsl
465600050128    1c                   endif
465700050128     c
465800050128     C   90              GOTO      endct4
465900051024     C*
466000051024     c* Per tassazione multipla verifico se c'e' obbligo di inserimento o
466100051024     c*  non immissione importo da assicurare: se c'e' controllo presenza
466200051024     c*  IAS sulle bolle
466300051024     c                   if        wscelta='9'
466400051024     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
466500051024     C     KTPT          CHAIN     TITPT000                           32
466600051024     C*
466700051024    3C     *IN32         IFEQ      *ON
466800051024     C     TPTATB        ORNE      ' '
466900051024     C     TPTAPL        ORNE      'P'
467000051024     c                   if        tamfis='3'  and tas9noias='S'
467100051024     C                   MOVEL     MSG(83)       VIDMSG
467200051024     C                   SETON                                        289055
467300051024     C                   GOTO      endct4
467400051024     c                   endif
467500051024     c                   if        tamfis='2'  and tas9siias='S'
467600051024     C                   MOVEL     MSG(84)       VIDMSG
467700051024     C                   SETON                                        289055
467800051024     C                   GOTO      endct4
467900051024     c                   endif
468000051024     c                   endif
468100051024     c                   endif
468200911028     C*
468300911028     C     ENDCT4        ENDSR
468400911028     C*
468500050120     C*--- Prepara CAMPI TASSAZIONE ----------------------------------*
468600050120     C     PREPA06       BEGSR
468700050120     C                   SETOFF                                       052506
468800050120     C                   SETOFF                                       07
468900050120     C                   MOVEL     *BLANKS       DESCEI
469000050120     C                   CLEAR                   WAR6
469100050120     C                   CLEAR                   WAR7
469200050120     c                   Clear                   war61
469300050120     c                   Clear                   war71
469400050120     c                   Clear                   wesvar1
469500050120     c                   Clear                   ContaSv
469600050120     c                   Clear                   Totimv
469700050926     c                   Clear                   NOeliminaporto    1
469800070924     c                   Clear                   Savimv
469900050120     C* IMPOSTO I CAMPI DELL'IMPORTO DA ASSIC E Q.TA FATTURARE
470000050120     c                   if        wscelta<>'9'
470100050120     C                   MOVEL     WQFT          VIDQFT
470200050120     C                   MOVEL     WIAS          VIDIAS
470300050120     C                   MOVEL     WIAS          VARIAS
470400050120     C                   MOVEL     WVAS          VIDVAS
470500050120     C                   MOVEL     WVAS          VARVAS
470600050120     c                   else
470700050120     C                   MOVEL     arbqft        VIDQFT
470800050120     C                   MOVEL     arbias        VIDIAS
470900050120     C                   MOVEL     arbias        VARIAS
471000050120     C                   MOVEL     arbvas        VIDVAS
471100050120     C                   MOVEL     arbvas        VARVAS
471200050120     c                   endif
471300050120     C*
471400050120      * Se l'importo da assicurare è a 0 e non è stata impostata la divisa
471500050120    1C     VIDIAS        IFEQ      *ZEROS
471600050120     C     VIDVAS        ANDEQ     *BLANKS
471700050120      * prima provo ad impostare la divisa della nazione del mittente
471800050120      * se è estero
471900050120     C     MITVLT        IFNE      *BLANKS
472000050714     C     WMITIE        ANDEQ     'E'
472100050120     C                   MOVEL     MITVLT        VIDVAS
472200050120     C                   MOVEL     MITVLT        VARVAS
472300050120    1C                   ELSE
472400050120      * altrimenti imposto la §GEDCN
472500050120     C                   MOVEL     §GEDCN        VIDVAS
472600050120     C                   MOVEL     §GEDCN        VARVAS
472700050120     C                   ENDIF
472800050120     C                   ENDIF
472900050120     c                   eval      sv1_vidias = vidias
473000050120     c                   eval      sv1_vidvas = vidvas
473100050120     c                   eval      sv1_vidqft = vidqft
473200050120      *
473300050120     C* SE FATTURA IMMEDIATA PROTEGGO LA DIVISA
473400050120     C     *IN01         IFEQ      *OFF
473500050120     C     VIDKSC        ANDNE     9999
473600050120     C     TSZKSC        ORGT      0
473700050120     C     VIDKSC        ANDEQ     9999
473800050120     C                   SETON                                        07
473900050120     C                   ENDIF
474000050120     C* SE OBBLIGO TASSAZIONE CON TARIFFA DI CARTELLO, PROTEGGO I CAMPI
474100050120     C   96              SETON                                        25
474200050120     C* PROTEGGO QUANTITA' DA FATTURARE SE FLAG Q.TA' DA FATTURARE = 3
474300050120     C* E SE CLIENTE ESISTENTE IN TARIFFA
474400050120     C     *IN29         IFEQ      *OFF
474500050120     C                   MOVEL     VIDCTR        W001A
474600050120     C     W001A         IFEQ      '4'
474700050120     C     TAMFVD        ANDEQ     '3'
474800050120     C                   SETON                                        06
474900050120     C                   END
475000050120     C                   END
475100050120     C*
475200050120     C                   CLEAR                   FIAR6000
475300050120     C                   CLEAR                   FIAR7000
475400050120     C*
475500050120     C* VEDO SE ESITONO I FILE DELLA TASSAZIONE
475600050120     C                   MOVEL     '1'           WTRC
475700050120     C     KARB4         CHAIN     FIAR6000                           32
475800050120     C  N32              MOVEL     'E'           WAR6              1
475900050120     C     KARB4         CHAIN(N)  FIAR7000                           32
476000050120     C  N32              MOVEL     'E'           WAR7              1
476100050120     C* PULIZIA SFL
476200050120     C                   SETON                                        27
476300050317     C                   WRITE     LRA2C06
476400050120     C                   SETOFF                                       27
476500050120     C*
476600050120     C                   CLEAR                   V6CSV1
476700050120     C                   CLEAR                   V6CEV1
476800050120     C                   CLEAR                   V6CVA1
476900050120     C                   CLEAR                   VARVA1
477000050120     C                   DO        20            NRR
477100050317     C                   WRITE     LRA2S06
477200050120     C                   ENDDO
477300050120     C*
477400050120     C                   MOVEL     §3ATB1        TIPTB1            1
477500050120     C* 1  B O L L A   A S S E G N A T O
477600050120    1C     TIPTB1        IFEQ      'A'
477700050120     C*
477800050120     C*  PRENDO I CAMPI DALLA BOLLA DELLA TASSAZIONE SE:
477900050120     C*
478000050120     C* 1) E' 1 TASSAZIONE
478100050120     C* 2) E' PASSAGGIO DA SF A FATT IMMEDIATA
478200050120     C* 3) SE PASSAGGIO DA FATT IMMEDIATA A SF SE CONVENUTI STABILITI
478300050120     C*    DALLA PARTENZA
478400050120     C*
478500050120    2C     *IN12         IFEQ      *OFF
478600050120     C*
478700050120    2C     *IN12         OREQ      *ON
478800050120     C     AR6DFT        ANDEQ     0
478900050120     C*
479000050120    2C     *IN12         OREQ      *ON
479100050120     C     AR6DFT        ANDGT     0
479200050120     C     AR6FIM        ANDEQ     'P'
479300050120     C                   MOVEL     AR6DIV        VIDDIV
479400050120     C                   MOVEL     AR6DIV        WDIV
479500050120     C                   Z-ADD     AR6POR        VIDPOR
479600050120     C                   Z-ADD     AR6POR        VARPOR
479700050120     C                   Z-ADD     AR6IMV        VIDIMV
479800050120     C                   MOVEL     AR6CEI        VIDCEI
479900070924     c                   eval      savimv=ar6imv
480000050120     C* SFL DELLE VARIE
480100050120     C                   Z-ADD     0             NRR
480200050120     C                   MOVEL     AR6SV1        WSV1
480300050120     C                   Z-ADD     AR6VA1        WVA1
480400050120     C                   EXSR      CARVAR
480500050120     C                   MOVEL     AR6SV2        WSV1
480600050120     C                   Z-ADD     AR6VA2        WVA1
480700050120     C                   EXSR      CARVAR
480800050120    4C     AR6SV3        IFNE      ' '
480900050120     C                   MOVEL     AR6SV3        WSV1
481000050120     C                   Z-ADD     AR6VA3        WVA1
481100050120     C                   EXSR      CARVAR
481200050120     C*
481300050120     C* VISTO CHE C'E' LA 3 VARIA VEDO SE CE NE SONO ANCORA
481400050120     C     KARB4         SETLL     FIAR7000
481500050120     C     KARB4         READE(N)  FIAR7000                               32
481600050120    5C     *IN32         DOWEQ     *OFF
481700050120     C                   MOVEL     AR7SVN        WSV1
481800050120     C                   MOVEL     AR7VAN        WVA1
481900050120     C                   EXSR      CARVAR
482000050120     C     KARB4         READE(N)  FIAR7000                               32
482100050120    5C                   ENDDO
482200050120    4C                   ENDIF
482300050120     C*
482400050120     C* DECODIFICA ESENZIONE
482500050120    3C     VIDCEI        IFNE      ' '
482600050120     C                   MOVEL     'EI'          COD
482700050120     C                   MOVEL     *BLANKS       KEY
482800050120     C                   MOVEL     VIDCEI        KEY
482900050120     C     KTAB          CHAIN     TABEL                              32
483000050120     C  N32              MOVEL     TBLUNI        DESCEI
483100050120    3C                   END
483200050120     C*
483300050120     C* VEDO SE PROTEGGERE I CONVENUTI
483400050120    3C     *IN12         IFEQ      *OFF
483500050120    4C     AR6IMV        IFGT      0
483600050120     C                   SETON                                        25
483700050120    4C                   ENDIF
483800050120     C*
483900050120   X3C                   ELSE
484000050120     C*
484100050120     C     AR6FIM        IFEQ      'P'
484200050120     C                   SETON                                        25
484300050120    4C                   ENDIF
484400050120    3C                   ENDIF
484500050120     C*
484600050120   X2C                   ELSE
484700050120     C*
484800050120     C                   MOVE      *BLANKS       VIDDIV
484900050120     C                   MOVE      *BLANKS       WDIV
485000050120     C                   Z-ADD     0             VIDPOR
485100050120     C                   Z-ADD     0             VARPOR
485200050120     C                   Z-ADD     0             VIDIMV
485300050120     C                   MOVEL     ' '           VIDCEI
485400050120     C* PULISCO ANCHE IL SFL
485500050120     C                   DO        20            NRR
485600050317     C     NRR           CHAIN     LRA2S06                            32
485700050120     C                   CLEAR                   V6CSV1
485800050120     C                   CLEAR                   V6CVA1
485900050120     C                   CLEAR                   VARVA1
486000050120     C                   CLEAR                   V6CEV1
486100050317     C  N32              UPDATE    LRA2S06
486200050120     C                   ENDDO
486300050120    2C                   END
486400050120     C*
486500050120     C     *IN25         IFEQ      *ON
486600050120     C                   DO        20            NRR
486700050317     C     NRR           CHAIN     LRA2S06                            32
486800050317     C  N32              UPDATE    LRA2S06
486900050120     C                   ENDDO
487000050120     C                   ENDIF
487100050120     C*
487200050120   X1C                   ELSE
487300050120     C*
487400050120     C                   MOVEL     §3ATB2        TIPTB2            1
487500050120     C* 2  B O L L A   A S S E G N A T O
487600050120    2C     TIPTB2        IFEQ      'A'
487700050120     C                   SETON                                        05
487800050120
487900050120     c                   Clear                   DE2CEI
488000050120     c                   Clear                   wdiv
488100050120     c                   Clear                   vi2va1
488200050120     c                   Clear                   vi2va2
488300050120     c                   Clear                   vi2va3
488400050120     c                   Clear                   vi2va4
488500050120     c                   Clear                   vi2va5
488600050120     c                   Clear                   vi2va6
488700050120     c                   Clear                   vi2sv1
488800050120     c                   Clear                   vi2sv2
488900050120     c                   Clear                   vi2sv3
489000050120     c                   Clear                   vi2sv4
489100050120     c                   Clear                   vi2sv5
489200050120     c                   Clear                   vi2sv6
489300050120     c                   Clear                   vi2es1
489400050120     c                   Clear                   vi2es2
489500050120     c                   Clear                   vi2es3
489600050120     c                   Clear                   vi2es4
489700050120     c                   Clear                   vi2es5
489800050120     c                   Clear                   vi2es6
489900050120     C*
490000071029     C*
490100071029     C                   CLEAR                   FIAR6000
490200071029     C                   CLEAR                   FIAR7000
490300050120     C                   MOVEL     '2'           WTRC
490400050120     C     KARB4         CHAIN     FIAR6000                           38
490500050120     c  n38              Eval      war61 = 'E'
490600050120      * Verifico se ci sono altre varie
490700050120     c     Karb4         Setll     Fiar701l
490800050120     c                   If        %Equal(Fiar701l)
490900050120     c                   Eval      war71 = 'E'
491000050120     c                   EndIf
491100050120     C*
491200050120     C**!!!              MOVEL     'G'           VI2SV1
491300050120     C**!!!              MOVEL     *BLANKS       VI2CEI
491400050120     C**!!!              MOVEL     *BLANKS       DE2CEI
491500050120     C**!!!              MOVE      *BLANKS       VI2DIV
491600050120     C**!!!              MOVE      *BLANKS       WDIV
491700050120     C**!!!              Z-ADD     0             VI2VA1
491800050120     C**!!!              Z-ADD     0             VA2VA1
491900050120     C*
492000050120    3C**!!!*IN38         IFEQ      *OFF
492100050120     C*
492200050120     C* RIEMPO I CAMPI DELLA TASSAZIONE SE:
492300050120     C* 1) E' 1 TASSAZIONE
492400050120     C* 2) E' RITASSAZIONE CON CONVENUTI DALLA PARTENZA
492500050120    4C**!!!*IN12         IFEQ      *OFF
492600050120     C**!!!AR6VA1        ANDGT     0
492700050120     C*
492800050120     C**!!!*IN12         OREQ      *ON
492900050120     C**!!!AR6FIM        ANDEQ     'P'
493000050120     C**!!!AR6VA1        ANDGT     0
493100050120    3c                   If        War61 = 'E' and (Not *In12 or
493200050120     c                             (*In12 and Ar6Fim = 'P'))
493300050120
493400050120     C                   MOVEL     AR6DIV        VI2DIV
493500050120     C                   MOVEL     AR6DIV        WDIV
493600050120     C**!!!              MOVEL     AR6SV1        VI2SV1
493700050120     C**!!!              Z-ADD     AR6VA1        VI2VA1
493800050120     C**!!!              Z-ADD     AR6VA1        VA2VA1
493900050120     c                   Movel     Ar6Sv1        wsv1
494000050120     c                   Z-add     Ar6Va1        wva1
494100050120     c                   ExSr      CarVar1
494200050120     c                   Movel     Ar6Sv2        wsv1
494300050120     c                   Z-add     Ar6Va2        wva1
494400050120     c                   ExSr      CarVar1
494500050120     c                   Movel     Ar6Sv3        wsv1
494600050120     c                   Z-add     Ar6Va3        wva1
494700050120     c                   ExSr      CarVar1
494800050120      * Verifico se ci sono altre varie
494900050120     c     Karb4         Setll     Fiar701l
495000050120Do  4c                   Do        *Hival
495100050120     c     Karb4         Reade(n)  Fiar701l
495200050120     c                   If        %Eof(Fiar701l)
495300050120     c                   Leave
495400050120     c                   EndIf
495500050120     c                   Movel     Ar7Svn        wsv1
495600050120     c                   Z-add     Ar7Van        wva1
495700050120     c                   ExSr      CarVar1
495800050120    4c                   EndDo
495900050120
496000050120     C**!!!              SETON                                        25
496100050120    4C**!!!              END
496200050120     C*
496300050120    4C     AR6CEI        IFNE      ' '
496400050120    5C**!!!*IN12         IFEQ      *OFF
496500050120     C*
496600050120     C**!!!*IN12         OREQ      *ON
496700050120     C**!!!AR6FIM        ANDEQ     'P'
496800050120     C                   MOVEL     AR6CEI        VI2CEI
496900050120     C                   MOVEL     'EI'          COD
497000050120     C                   MOVEL     *BLANKS       KEY
497100050120     C                   MOVEL     VI2CEI        KEY
497200050120     C     KTAB          CHAIN     TABEL                              32
497300050120     C  N32              MOVEL     TBLUNI        DE2CEI
497400050120     C*!!32              MOVEL     *BLANKS       DE2CEI
497500050120    5C**!!!              END
497600050120    4C                   END
497700050120      * Controllo se devo proteggere i campi x i convenuti
497800050120    4c                   If        Not *In12 and TotImv > 0
497900050120     c                   Eval      *In25 = *On
498000050120    4c                   EndIf
498100050120    4c                   If        *In12 and Ar6Fim = 'P'
498200050120     c                   Eval      *In25 = *On
498300050120    4c                   EndIf
498400050120     C*
498500050120    3c                   EndIf
498600050120     C*
498700050120    2C                   END
498800050120    1C                   END
498900050120     C*
499000911028     C                   ENDSR
499100911028     C*
499200990913      *--- TASSAZIONE BOLLA ------------------------------------------*
499300911028     C     TASSAZ        BEGSR
499400990909     C                   CLEAR                   DTAS
499500990909     C                   CLEAR                   DTASV
499600950104     C                   CLEAR                   VIDMSG
499700060123     C***                CLEAR                   WFIN
499800060123     C***                CLEAR                   WVRV
499900020415     C                   CLEAR                   Wtassa            1
500000030605     C                   CLEAR                   Wtassad           1
500100030618     C                   CLEAR                   itassa           11 3
500200050926     C                   CLEAR                   NOEliminaporto
500300911028     C*
500400941217    1C     VIDKSC        IFEQ      9999
500500931109     C                   MOVE      TSZKSC        TASKSC
500600941217     C                   MOVEL     TSZCTR        TASCTR
500700931109     C*
500800931109     C                   ELSE
500900911122     C*
501000931109     C                   MOVEL     VIDKLP        TASKSC
501100931109     C                   MOVE      VIDKSC        TASKSC
501200941217     C                   MOVEL     VIDCTR        TASCTR
501300941217    2C                   ENDIF
501400941217     C*
501500941217     C                   Z-ADD     ARBPKF        TASPKF
501600160530     C                   Z-ADD     ARBPKb        TASPKb
501700050714     C***                MOVEL     ARBLNP        TASLNP
501800050714     C                   MOVEL     VUNICOLNP     TASLNP
501900941217     C                   MOVEL     ARBCTS        TASCTS
502000941217     C                   MOVEL     ARBDSP        TASDSP
502100970414     C                   MOVEL     ARBDSP        TASDCT
502200941217     C                   MOVEL     ARBVLF        TASVLF
502300160530     C                   MOVEL     ARBfvf        TASfvf
502400160530     c                   z-add     arbvlc        tasvlc
502500160530     c                   z-add     arbncr        tasncr
502600941217     C                   MOVEL     ARBTSP        TASTSP
502700980512     C     WBOL          IFEQ      '1'
502800941217     C                   MOVEL     §3ATB1        TASTBL
502900990913     C                   MOVEL     VIDDIV        TASDIV
503000990913     C                   Z-ADD     VIDIMV        TASIMV
503100980512     C                   ELSE
503200980512     C                   MOVEL     §3ATB2        TASTBL
503300990913     C                   MOVEL     VI2DIV        TASDIV
503400040119     C**!!!              Z-ADD     VI2VA1        TASIMV
503500040119     c                   Z-add     TotImv        TasImv
503600980512     C                   ENDIF
503700941217     C                   MOVEL     ARBLNA        TASLNA
503800941217     C                   MOVEL     ARBNCL        TASNCL
503900160530     C                   MOVEL     ARBNCL        TASNCLb
504000990909     C                   Z-ADD     VIDPOR        TASPOR
504100941217     C                   MOVEL     VIDQFT        TASQFT
504200980512     C     WBOL          IFEQ      '1'
504300980512     C     §3AFCA        ANDEQ     1
504400980512     C     WBOL          OREQ      '2'
504500980512     C     §3AFCA        ANDEQ     2
504600941217     C                   MOVEL     AR9CAS        TASCAS
504700941217     C                   MOVEL     AR9VCA        TASVCA
504800941217     C                   MOVEL     AR9TIC        TASTIC
504900980512     C                   ENDIF
505000941217     C                   MOVEL     VIDIAS        TASIAS
505100941217     C                   MOVEL     VIDVAS        TASVAS
505200941217     C                   MOVEL     ARBCAD        TASCAD
505300990909     C                   MOVEL     ARBNZD        TASNZD
505400941217     C                   MOVEL     ARBFIN        TASFIN
505500050715     C                   MOVEL     Vunicocam     TASCAM
505600050715     C**                 MOVEL     ARBCAM        TASCAM
505700050714     C**                 MOVEL     ARBNZM        TASNZM
505800050714     C                   MOVEL     Vuniconzm     TASNZM
505900050715     C                   MOVEL     Vunicofap     TASFAP
506000050715     C**                 MOVEL     ARBFAP        TASFAP
506100941217     C                   MOVEL     ARBTC1        TASTC1
506200941217     C                   MOVEL     ARBTC2        TASTC2
506300970409     C     WDDT          IFEQ      'C'                                          SI DDT
506400970409     C     WDDT          OREQ      'S'
506500031028     c     wddt          oreq      'P'
506600031028     c     wddt          oreq      'Q'
506700970409     C                   MOVEL     'S'           TASDDT
506800970409     C                   ENDIF
506900060424     c* flag bolla di reso in TASFLO
507000060925     c****               movel     arbfbr        tasflo
507100060925     c                   clear                   dtas01
507200060925     c                   movel     arbfbr        §asfbr
507300060925     c                   movel     arbcca        §ascca
507400110629     c* avviso al destinatario
507500110629     c                   movel     wemd          §asemd
507600150107     c* pin code
507700150107     c                   if        wpincode='S'
507800150107     c                   movel     'S'           §aspin
507900150107     c                   endif
508000060925     c                   eval      tasflo = dtas01
508100151127     c* Packing list e orm telefonici solo per la prima bolla in quanto la seconda
508200151127     c* è solo per tassare una specifica varia
508300151127     C     WBOL          IFEQ      '1'
508400151127     c* PACKING LIST
508500151127     c                   Eval      kAr5Trd = 'GEN'
508600151127     c     kFiar5        Chain     Fiar501l
508700151127     c                   if        %found(fiar501l)
508800151127     c                   movel     ar5uni        dar5gen
508900151127     c                   if        §ar5alx='S' or §ar5als='S'
509000151127     c                   eval      tasspl='S'
509100151127     c                   endif
509200151127     c                   endif
509300151127     c* ADDEBITO ORM TELEFONICI (diritto di chiamata)
509400151127     C                   MOVEL     'M'           WTRC
509500151127     C     KARB4         CHAIN(N)  FiAR401l
509600151127     c                   if        %found(fiar401l)
509700151127     c                   movel     ar4not        dsbl4m
509800151127     c                   if        §b4ado='S'
509900151127     c                   eval      tasprt='S'
510000151127     c                   endif
510100151127     c                   endif
510200151127
510300151127     c                   endif
510400961114     C**
510500961114     C* SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
510600961114     C*                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
510700020222     c                   if        lnantw <> 'EEX' or lnantw <> 'EUP' or
510800020222     c                             lnantw <> 'FED' or lnantw <> 'DPD'
510900060123     C***                MOVEL     ARBFDN        TASFDN
511000060123     C                   MOVEL     vunicoFDN     TASFDN
511100961114     C                   ENDIF
511200961114     C**
511300970710     C* CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
511400050715     c* Se bolla dirottata, e posso utilizzare i dirottamenti
511500050715     c*  prendo quello già calcolato
511600050715     c                   if        Vunicomct<>'  ' and
511700050715     c                             (§vpodir='A' or §vpodir='E')
511800050715     c                   movel     Vunicomct     tasmct
511900050715     c                   else
512000050715     c
512100970710     C                   CLEAR                   DSSI95
512200970710     C                   MOVEL     ARBLOM        I95LOC
512300050715     C                   MOVEL     Vunicocam     I95CAP
512400050715     C**                 MOVEL     ARBCAM        I95CAP
512500050714     C**                 MOVEL     ARBNZM        I95NAR
512600050714     C                   MOVEL     Vuniconzm     I95NAR
512700970710     C                   MOVEL     '3'           I95TCN
512800971103     C                   MOVEL     ARBDSP        I95DAT
512900971201     C     §3AFCA        IFNE      0
513000971201     C                   MOVEL     'S'           I95FCA
513100971201     C                   ENDIF
513200971201     C                   MOVEL     §3ATB1        I95TPO
513300020228      * Se network FedEx passo flag apposito per dire di prendere i dati
513400020228      * dalla tabella della nazione
513500020228     c     lnpntw        ifeq      'FED'
513600020228     c                   eval      i95fi1 = 'S'
513700020228     c                   endif
513800020228
513900971105     C                   CALL      'TISI95R'
514000970710     C                   PARM                    DSSI95
514100971105     C     O95ERR        IFEQ      *BLANKS
514200970710     C                   MOVEL     O95CTS        TASMCT
514300951027     C                   ENDIF
514400050715     C                   ENDIF
514500050715     c
514600050715     C*****              MOVEL     'S'           WCAL13            1
514700941219     C* CARICO LE VARIE
514800980512     C     WBOL          IFEQ      '1'
514900941219    2C                   DO        20            NRR
515000050317     C     NRR           CHAIN     LRA2S06                            32
515100030624      * SE ESISTE LA VARIA "&" CON TUTTI 8888888 NON LA PASSO AL TNSF20R
515200030624      * MA PASSO UN FLAG  DI RICHIESTA CALCOLO VARIA "&"
515300030624     C                   Z-ADD     NRR           C
515400030624     C                   IF        V6CSV1 = '&' AND V6CVA1 = 88888888
515500030624     C                   EVAL      TASFCAA = 'S'
515600030624     C                   ELSE
515700941219     C                   MOVEL     V6CSV1        VSV(C)
515800970606     C                   MOVEL     V6CEV1        VES(C)
515900060123     C***  V6CSV1        IFEQ      §$2FIN
516000060123     C***                MOVE      '1'           WFIN              1
516100060123     C***                ELSE
516200060123     C***  V6CSV1        IFEQ      §$2VRV
516300060123     C***                MOVE      '1'           WVRV              1
516400060123     C***                ENDIF
516500060123     C***                ENDIF
516600030624     C                   ENDIF
516700020415     c* se devo tassare una specifica varia(da tabella 3A) e c'e' gia'
516800020415     c*  non richiamo la tassazione
516900040119     c**!!!              if        §3asva<>*blanks and v6csv1=§3asva
517000040119     c                   if        §3asva<>*blanks and v6csv1=§3asva and
517100040119     c                             Tasimv > 0
517200020415     c                   eval      wtassa='N'
517300030618      * mi salvo l'importo della varia
517400030618     c                   eval      itassa=v6cva1
517500020415     c                   endif
517600030605     c* se esiste già la varia "D" non la ricalcolo
517700030605     c*
517800030605     c                   if        v6csv1='D'
517900030605     c                   eval      wtassad='N'
518000030605     c                   endif
518100030925     c* se il valore del diritto di fatturazione in anagrafica è uguale a zero non lo calcolo
518200030925     c*
518300030925     c                   if        sav_indsin = 0
518400030925     c                   eval      wtassad='N'
518500030925     c                   endif
518600970606     C* SE NON LA DEVO PASSARE, LA PASSO VUOTA LASCIANDO IL FLAG IMPOST
518700970606     C*   PER NON PERDERE LA POSIZIONE CHE SI E' TENUTA MEMORIZZATA
518800970606     C     WCANCN        IFEQ      'N'
518900970606     C     V6CSV1        ORNE      'N'
519000030624     C*
519100941219     C                   Z-ADD     V6CVA1        VVA(C)
519200030624      *
519300970606     C                   ENDIF
519400030624     C                   IF        V6CSV1 = '&' AND V6CVA1 = 88888888
519500030624     C                   z-add     *zeros        vva(c)
519600030624     C                   endif
519700941219    2C                   ENDDO
519800980512     C                   ELSE
519900040121      * carico varie seconda bolla
520000980512     C                   MOVEL     'G'           TASSVA
520100040119     c                   Eval      TascVag = 'S'
520200980608     C     VI2VA1        IFGT      0
520300980608     C                   MOVEL     VI2SV1        VSV(1)
520400980608     C                   Z-ADD     VI2VA1        VVA(1)
520500040121     c                   Movel     Vi2Es1        ves(1)
520600980608     C                   ENDIF
520700040119     c                   If        Vi2Va2 > *Zeros
520800040119     c                   Movel     Vi2Sv2        Vsv(2)
520900040119     c                   Z-add     Vi2Va2        Vva(2)
521000040121     c                   Movel     Vi2Es2        ves(2)
521100040119     c                   EndIf
521200040119     c                   If        Vi2Va3 > *Zeros
521300040119     c                   Movel     Vi2Sv3        Vsv(3)
521400040119     c                   Z-add     Vi2Va3        Vva(3)
521500040121     c                   Movel     Vi2Es3        ves(3)
521600040119     c                   EndIf
521700040119     c                   If        Vi2Va4 > *Zeros
521800040119     c                   Movel     Vi2Sv4        Vsv(4)
521900040119     c                   Z-add     Vi2Va4        Vva(4)
522000040121     c                   Movel     Vi2Es4        ves(4)
522100040119     c                   EndIf
522200041129     c                   If        Vi2Va5 > *Zeros
522300041129     c                   Movel     Vi2Sv5        Vsv(5)
522400041129     c                   Z-add     Vi2Va5        Vva(5)
522500041129     c                   Movel     Vi2Es5        ves(5)
522600041129     c                   EndIf
522700041129     c                   If        Vi2Va6 > *Zeros
522800041129     c                   Movel     Vi2Sv6        Vsv(6)
522900041129     c                   Z-add     Vi2Va6        Vva(6)
523000041129     c                   Movel     Vi2Es6        ves(6)
523100041129     c                   EndIf
523200040126      * se segue fattura imposto wtassad = 'N' in questo modo il tnsf20r non mi
523300040126      * tassa la varia 'D'
523400040126     c                   If        *In01
523500040126     c                   Eval      wtassad = 'N'
523600040126     c                   EndIf
523700980512     C                   ENDIF
523800011204
523900030203     c                   z-add     arbncp        Tasncp
524000030203     c                   z-add     arbpkc        Taspkc
524100030203
524200030203      * Bancali
524300030205     c                   If        %Subst(ArbGva:1:1) = 'B'
524400030203     c                   Z-Add     VidBan        TasBan
524500030205     c                   EndIf
524600030728      * Colli originali
524700030728     c                   If        %Subst(ArbGva:1:1) = 'O'
524800030729     c                   Z-Add     VidCor        TasNcl
524900030728     c                   EndIf
525000011204
525100941217     C* ELABORO E CHIUDO CON LR IL PGM
525200011113     C                   MOVEL     ' '           TASTLA
525300941217     C*
525400020415     c                   if        wtassa=' '
525500020415     c* se devo tassare solo una varia, da tipo bolla, la imposto
525600020415     c                   if        §3asva<>*blanks
525700020415     c                   eval      tassva=§3asva
525800020415     c                   endif
525900030620     c*
526000030604     c                   endif
526100030604      * per fattura immediata passo al TNSF20R la varia  "D" e l'imorto del diritto di fatturazione
526200030926      * da sommare che prendo CNIND (INDSIN) solo se è diverso da zero
526300030604      *
526400030926     c                   if         wtassad = ' '
526500030605     c                   move      sav_indsin    tasivdf
526600030604      *
526700030604     c                   eval      tassvdf = 'D'
526800030926     c                   endif
526900030604      *
527000030605     c                   if        wtassa='N' and wtassad = ' '
527100030605     c* se ho già tassata la singola varia devo sommare il solo diritto di fatturazione se imponibil
527200030618     c* maggiore di zero oppure se imponibile a zero ho la singola varia valorizzata
527300040119     c**!!!              if        tasimv > 0  or (tasimv = 0 and itassa > 0)
527400040119     c                   if        tasimv > 0
527500030604     c                   eval      tassva='D'
527600030605     c                   else
527700030605     c                   eval      wtassad = 'N'
527800030605     c                   endif
527900030604     c                   endif
528000031124     c* in caso di tassato fino all'imponibile valorizzo la singola varia da calcolare se tassva
528100031124     c* uguale a  blank
528200031124     c                   if        wtassad = ' ' and tasimv > 0 and tassva = ' '
528300030625     c                   eval      tassva = 'D'
528400030625     c                   endif
528500020415     c**
528600030605     c                   if        wtassa = 'N' and wtassad = 'N'
528700030605     c                   goto      endtas
528800030605     c                   endif
528900050804     c
529000050804     c                   eval      parca  = ' '
529100050804     c                   movel     paramt        kpjbu
529200030605     c*
529300941217     C                   CALL      'TNSF20R'
529400941217     C                   PARM                    KPJBA
529500990909     C                   PARM                    DTAS
529600990909     C                   PARM                    DTASV
529700941217     C*
529800941217    1C     TASERR        IFNE      *BLANKS
529900980608     C     TASMSG        IFNE      *BLANKS
530000980608     C                   MOVEL(P)  TASMSG        VIDMSG
530100980608     C                   ELSE
530200980608     C                   MOVEL     MSG(13)       VIDMSG
530300980608     C                   ENDIF
530400990927     C                   SETON                                        4890
530500990915     C                   SETOFF                                       07
530600990105     C**
530700990105    2C     *IN96         IFEQ      *ON
530800990105     C     *IN25         ANDEQ     *ON
530900990105     C                   SETOFF                                       25
531000990105     C* SPROTEGGO LE VARIE
531100990105    3C                   DO        20            NRR
531200050317     C     NRR           CHAIN     LRA2S06                            32
531300050317     C  N32              UPDATE    LRA2S06
531400990105    3C                   ENDDO
531500990105    2C                   ENDIF
531600990105     C**
531700941217   X1C                   ELSE
531800950420     C*
531900980512     C     WBOL          IFEQ      '1'
532000050926     c
532100990914     C                   MOVEL     TASDIV        VIDDIV
532200991014     C                   MOVEL     TASDIV        WDIV
532300050926     c* Se dirottamento prendo solo il porto
532400060123   1ac****               if        Memsoloporto='S' and §3atb1<>'AR'
532500060123   1ac                   if        Memsoloporto='S'
532600050926     c* se calcolato me lo memorizzo
532700050926     c                   if        taspor>0
532800050926     c                   eval      NOeliminaporto='S'
532900050926     c                   endif
533000060123  x  c                   movel     taspor        vidpor
533100060123  c
533200060123     C* Vedo se nelle varie è presente Inoltro/Disagiata/Isola
533300060123     c                   movel     §$2fin        wvaria
533400060123     c                   exsr      Cervaria
533500060123     c                   movel     §$2vrk        wvaria
533600060123     c                   exsr      Cervaria
533700060123     c                   movel     §$2vrj        wvaria
533800060123     c                   exsr      Cervaria
533900050926     c
534000060123   1ac                   else
534100950420     C                   MOVEL     TASQFT        VIDQFT
534200060123     C**                 CLEAR                   W001A
534300060123     C**   VIDIMV        IFGT      0
534400060123     C**                 MOVE      '1'           W001A             1
534500060123     C**                 ENDIF
534600911028     C                   Z-ADD     0             VIDIMV
534700941217     C* CARICO LE VARIE
534800941217    2C                   DO        20            NRR
534900050317     C     NRR           CHAIN     LRA2S06                            32
535000060123     C                   MOVEL     VSV(NRR)      V6CSV1
535100060123     C                   Z-ADD     VVA(NRR)      V6CVA1
535200060123     C                   MOVEL     VES(NRR)      V6CEV1
535300991214     C* SE TIPO BOLLA "AR" E INOLTRO E ISTAT NON PRESENTI NON CARICO
535400991214     C* LA RELATIVA VARIA
535500060123     C**                 SELECT
535600060123     C**   VSV(NRR)      WHENEQ    §$2FIN
535700060123     C**   WFIN          IFEQ      '1'
535800060123     C**   §3ATB1        ORNE      'AR'
535900060123     C**                 MOVEL     VSV(NRR)      V6CSV1
536000060123     C**                 Z-ADD     VVA(NRR)      V6CVA1
536100060123     C**                 MOVEL     VES(NRR)      V6CEV1
536200060123     C**                 ELSE
536300060123     C**                 CLEAR                   V6CSV1
536400060123     C**                 CLEAR                   V6CVA1
536500060123     C**                 CLEAR                   V6CEV1
536600060123     C**                 CLEAR                   VSV(NRR)
536700060123     C**                 CLEAR                   VVA(NRR)
536800060123     C**                 CLEAR                   VES(NRR)
536900060123     C**                 ENDIF
537000060123     C**   VSV(NRR)      WHENEQ    §$2VRV
537100060123     C**   WVRV          IFEQ      '1'
537200060123     C**   §3ATB1        ORNE      'AR'
537300060123     C**                 MOVEL     VSV(NRR)      V6CSV1
537400060123     C**                 Z-ADD     VVA(NRR)      V6CVA1
537500060123     C**                 MOVEL     VES(NRR)      V6CEV1
537600060123     C**                 ELSE
537700060123     C**                 CLEAR                   V6CSV1
537800060123     C**                 CLEAR                   V6CVA1
537900060123     C**                 CLEAR                   V6CEV1
538000060123     C**                 CLEAR                   VSV(NRR)
538100060123     C**                 CLEAR                   VVA(NRR)
538200060123     C**                 CLEAR                   VES(NRR)
538300060123     C**                 ENDIF
538400060123     C**                 OTHER
538500060123     C**                 MOVEL     VSV(NRR)      V6CSV1
538600060123     C**                 Z-ADD     VVA(NRR)      V6CVA1
538700060123     C**                 MOVEL     VES(NRR)      V6CEV1
538800060123     C**                 ENDSL
538900060123     c
539000050317     C   32              WRITE     LRA2S06
539100050317     C  N32              UPDATE    LRA2S06
539200941217    2C                   ENDDO
539300941217     C*
539400991119     C* SE CODICE BOLLA "AR" NON DEVO CALCOLARE TRASPORTO
539500920213     C*  QUINDI ANCHE TOTALE IMPONIBILE: DA INSERIRE MANUALMENTE
539600060123    2C**   §3ATB1        IFNE      'AR'
539700060123     C**   §3ATB2        ANDNE     'AR'
539800060123     C**   W001A         OREQ      '1'
539900991214     C                   Z-ADD     TASIMV        VIDIMV
540000060123     C**                 ENDIF
540100991119     C* SE C'ERA GIA' LO RIPRENDO PERCHE' PUO' ESSERE STATO CONVERTITO
540200060123    2C**   §3ATB1        IFNE      'AR'
540300060123     C**   §3ATB2        ANDNE     'AR'
540400060123     C**   VIDPOR        ORGT      *ZEROS
540500990915     C                   Z-ADD     TASPOR        VIDPOR
540600991214     C* SE LA BOLLA E' "AR" RICALCOLO IL TOTALE IMPONIBILE
540700060123     C**   W001A         IFEQ      *BLANKS
540800060123     C**   §3ATB1        ANDEQ     'AR'
540900060123     C**                 CLEAR                   DSLV16
541000060123     C**                 MOVEL     'T'           D17FIM
541100060123     C**                 Z-ADD     VIDPOR        D17POR
541200060123     C**                 CALL      'FNLV16R'
541300060123     C**                 PARM                    DSLV16
541400060123     C**                 PARM                    DTASV
541500060123     C**                 Z-ADD     O17IMV        VIDIMV
541600060123     C**                 ENDIF
541700060123    2C**                 ENDIF
541800050926   1aC                   ENDIF
541900060123     c
542000060123     C* SECONDA BOLLA
542100050926   x1C                   ELSE                                                   WBOL = 2
542200040119     c                   Clear                   ContaSv
542300040121      * carico le varie seconda bolla
542400040119Do  2c                   Do        20            c
542500040119If  3c                   If        Vsv(c) <> §$2Iva and
542600040119     c                             Vsv(c) <> §$2Bol
542700040119     c                   Add       1             ContaSv
542800040119S   4c                   Select
542900040119     c                   When      ContaSv = 1
543000040119     c                   Movel     Vsv(c)        Vi2Sv1
543100040119     c                   Z-add     Vva(c)        Vi2Va1
543200040121     c                   Movel     Ves(c)        Vi2Es1
543300040119     c                   When      ContaSv = 2
543400040119     c                   Movel     Vsv(c)        Vi2Sv2
543500040119     c                   Z-add     Vva(c)        Vi2Va2
543600040121     c                   Movel     Ves(c)        Vi2Es2
543700040119     c                   When      ContaSv = 3
543800040119     c                   Movel     Vsv(c)        Vi2Sv3
543900040119     c                   Z-add     Vva(c)        Vi2Va3
544000040121     c                   Movel     Ves(c)        Vi2Es3
544100040119     c                   When      ContaSv = 4
544200040119     c                   Movel     Vsv(c)        Vi2Sv4
544300040119     c                   Z-add     Vva(c)        Vi2Va4
544400040121     c                   Movel     Ves(c)        Vi2Es4
544500041129     c                   When      ContaSv = 5
544600041129     c                   Movel     Vsv(c)        Vi2Sv5
544700041129     c                   Z-add     Vva(c)        Vi2Va5
544800041129     c                   Movel     Ves(c)        Vi2Es5
544900041129     c                   When      ContaSv = 6
545000041129     c                   Movel     Vsv(c)        Vi2Sv6
545100041129     c                   Z-add     Vva(c)        Vi2Va6
545200041129     c                   Movel     Ves(c)        Vi2Es6
545300040119    4c                   EndSl
545400040119    3c                   EndIf
545500040119    2c                   EndDo
545600040119     c                   Movel     TasDiv        Vi2Div
545700040119     c                   Movel     TasDiv        wdiv
545800040122     c                   Z-add     TasImv        TotImv
545900040119     C**!!!              Z-ADD     1             C
546000040119     C**!!!TASSVA        LOOKUP    VSV(C)                                 57
546100040119     C*!!57              Z-ADD     VVA(C)        VI2VA1
546200040119     C*!!57              MOVEL     VSV(C)        VI2SV1
546300040119     C*!!57              MOVEL     TASDIV        VI2DIV
546400040119     C*!!57              Z-ADD     VVA(1)        VA2VA1
546500040119     C*!!57              MOVEL     TASDIV        WDIV
546600040119     C*!!57VES(C)        IFNE      ' '
546700040119     C**!!!VI2CEI        ANDEQ     ' '
546800040119     C**!!!              MOVEL     VES(C)        VI2CEI
546900040119     C**!!!              ENDIF
547000980512     C                   ENDIF
547100030625      *
547200030625      * nel caso di calcolo varia "D" con imponibile > di 0 ricalcolo il totale imponibile
547300030625      *
547400030625     c                   if        tassva = 'D'
547500030625     C                   CLEAR                   DSLV16
547600030625     C                   MOVEL     'T'           D17FIM
547700030625     C                   Z-ADD     VIDPOR        D17POR
547800030625     C                   CALL      'FNLV16R'
547900030625     C                   PARM                    DSLV16
548000030625     C                   PARM                    DTASV
548100030625     C                   Z-ADD     O17IMV        VIDIMV
548200030625     c                   endif
548300980512     C*
548400030620    1C                   ENDIF
548500030605     C     endtas        ENDSR
548600911028     C*
548700060123     c* Carico le varie Inoltro/Disagiata/Isola quando tasso bolle leg
548800060123     c*  sia per Fattura Immediata che segue fattura -----------------*
548900060123
549000060123     c     CerVaria      BEGSR
549100060123     c                   z-add     1             xx
549200060123     c     Wvaria        lookup    vsv(XX)                                32
549300060123     c                   if        *in32
549400060123     c
549500060123    2C                   DO        20            NRR
549600060123     C     NRR           CHAIN     LRA2S06                            32
549700060123     c                   if        v6CSV1=' '
549800060123     C                   MOVEL     VSV(xx)       V6CSV1
549900060123     C                   Z-ADD     VVA(xx)       V6CVA1
550000060123     C                   MOVEL     VES(xx)       V6CEV1
550100060123     c                   eval      NOeliminaporto='S'
550200060123     C   32              WRITE     LRA2S06
550300060123     C  N32              UPDATE    LRA2S06
550400060123     c                   leave
550500060123     c                   endif
550600060123     c                   enddo
550700060123     c
550800060123     c                   endif
550900060123     c
551000060123     c                   ENDSR
551100911028     C*--- CONTROLLI CAMPI TASSAZIONE --------------------------------*
551200990916     C     CONTR6        BEGSR
551300911030     C                   SETOFF                                       90
551400941221     C                   CLEAR                   VIDMSG
551500930422     C*
551600930422     C                   MLLZO     1             VIDPOR
551700930422     C                   MLLZO     1             VIDIMV
551800991028     C* PRENDO I DATI DELLA DIVISA DELLA TARIFFA
551900991028     C                   MOVEL     'CV'          COD
552000991028     C                   MOVEL(P)  §TADIV        KEY
552100991028     C                   EXSR      CTRTAB
552200991028     C                   MOVEL     TBLUNI        DSCV
552300011003     C                   MOVEL     §CVDEC        COMDEC
552400950120     C*
552500011127     C*** I M P O R T O  D A  A S S I C U R A R E  ***
552600950720     C     *IN97         IFEQ      *ON
552700950720     C                   MOVEL     '1'           WIN97             1
552800950720     C                   ENDIF
552900950120     C*
553000950120     C                   MOVEL     VIDVAS        WVLT
553100950120     C                   Z-ADD     VIDIAS        W0133            13 3
553200950120     C* CONTROLLO LA DIVISA
553300950120     C                   EXSR      CTRVLT
553400950120     C                   MOVEL     WVLT          VIDVAS
553500950120     C* ERRORE
553600950120     C   90              SETON                                        41
553700950120     C   90              GOTO      ENDCT6
553800950120     C*
553900950120     C* SE VARIATA LA VALUTA FACCIO RIDIGITARE L'IMPORTO
554000950120    1C     VIDVAS        IFNE      VARVAS
554100950720     C     WIN97         ANDEQ     *BLANKS
554200950120     C*
554300011002    2C     VIDVAS        IFNE      *BLANKS
554400011002     C     VIDIAS        ORNE      0
554500950120     C                   MOVEL     MSG(14)       VIDMSG
554600950120     C                   SETON                                        4290
554700950120     C                   GOTO      ENDCT6
554800011002    2C                   ENDIF
554900950120     C*
555000950120    1C                   ENDIF
555100950120     C*
555200950120     C                   MOVEL     VIDVAS        VARVAS
555300950720     C                   CLEAR                   WIN97
555400011128
555500011128      * pulisco il flag x la forzatura importo da assicurare forzabile
555600011203     c                   if        vidias <> sav_vidias
555700011128     c                   clear                   forza
555800011128     c                   endif
555900011127
556000020222      * CONTROLLO IMPORTO DA ASSICURARE
556100011128      *---------------------------------------
556200011203  1b c                   if        vidias <> 0
556300011203     c                   eval      sav_vidias = vidias
556400011127      * Controllo l'importo da assicurare tramite il trul22r
556500011127     c                   clear                   trul22ds
556600011127     c                   eval      i22tla = 'L'
556700011127     c                   eval      i22cbo = arbcbo
556800011127     c                   eval      i22tsp = arbtsp
556900050714     c*****              eval      i22lnp = arblnp
557000050714     c*****              eval      i22NZM = arbnzm
557100050714     c                   eval      i22lnp = VUNICOLNP
557200050714     c                   eval      i22nzm = VUNICONZM
557300011127     c                   eval      i22lna = arblna
557400011127     c                   eval      i22nzd = arbnzd
557500011128     c                   movel     vidklp        i22ksc
557600011128     c                   move      vidksc        i22ksc
557700011127     c                   eval      i22ctr = vidctr
557800011203     c                   eval      i22imp = vidias
557900011203     c                   eval      i22div = vidvas
558000011128     c                   eval      i22ute = knmus
558100011128     c                   eval      i22pgm = nompgm
558200011127     c                   call      'TRUL22R'
558300011127     c                   parm                    trul22ds
558400011128      * se ho tutti i flag impostati non posso immettere l'importo
558500011127      * da assicurare
558600011128  2b c                   if        o22fmn <> *blanks and o22fx1 <> *blanks
558700011128     c                             and o22fx2 <> *blanks
558800011127     c                   eval      vidmsg = msg(43)
558900011127     c                   eval      *in42 = *on
559000011127     c                   eval      *in90 = *on
559100011128     c                   goto      endct6
559200011127  2e c                   endif
559300011203      * controllo che non sia minore del limite previsto
559400011128  2b c                   if        o22fmn <> *blanks
559500011128     c                   eval      vidmsg = msg(46)
559600011128     c                   eval      *in42 = *on
559700011128     c                   eval      *in90 = *on
559800011128     c                   goto      endct6
559900011128  2e c                   endif
560000011128      * controllo che non sia maggiore del limite previsto
560100011128      * non forzabile
560200011128  2b c                   if        o22fx2 <> *blanks
560300011203     c                   eval      vidmsg = msg(15)
560400011210     c                   eval      %subst (vidmsg:38:14) = (%editc(o22lx2: '4'))
560500011128     c                   eval      *in42 = *on
560600011128     c                   eval      *in90 = *on
560700011128     c                   goto      endct6
560800011128  2e c                   endif
560900011128      * controllo che non sia maggiore del limite previsto
561000011128      * forzabile
561100011128  2b c                   if        o22fx1 <> *blanks and forza = *blanks
561200011128     c                   eval      forza = '1'
561300011203     c                   eval      vidmsg = msg(57)
561400011210     c                   eval      %subst (vidmsg:28:14) = (%editc(o22lx1: '4'))
561500011128     c                   eval      *in42 = *on
561600011128     c                   eval      *in90 = *on
561700011128     c                   goto      endct6
561800011128  2e c                   endif
561900011129  1e c                   endif
562000011128
562100950120     C*
562200950120     C* SE HA TARIFFE, CONTROLLO
562300950120     C*
562400950120     C* SE CLIENTE NON E' 9999 --> CONTROLLO L'IMPORTO DA ASSICURARE
562500950120    1C     VIDKSC        IFNE      9999
562600950120     C     *IN29         ANDEQ     *OFF
562700950120     C*
562800950421     C* SE FATTURA IMMEDIATA, TARIFFA A VALORE E FLAG = '3':
562900950421     C* SE VARIATO IMPORTO DA ASSICURARE OBBLIGO AZZERAMENTO TRASPORTO
563000950421     C* E IMPONIBILE SE DIVERSI DA 0.
563100950421     C                   MOVEL     VIDCTR        W001A
563200950421     C     *IN01         IFEQ      *OFF
563300950421     C     W001A         ANDEQ     '4'
563400950421     C     TAMFVD        ANDEQ     '3'
563500950720     C     VIDIAS        ANDNE     VARIAS
563600991013     C* Carico varie del video in schiera per vedere se c'è l'inoltro
563700991013     C                   MOVEL     '1'           WCKINL            1
563800991013     C                   EXSR      CARWAR
563900950720     C*
564000950421     C     VIDPOR        IFNE      *ZEROS
564100950421     C     VIDIMV        ORNE      *ZEROS
564200991013     C     WINL          OREQ      '1'
564300950421     C                   MOVEL     MSG(37)       VIDMSG
564400950421     C                   SETON                                        4990
564500950421     C                   GOTO      ENDCT6
564600950720     C                   ENDIF
564700950720     C                   ENDIF
564800950720     C*
564900950720     C* PULISCO CAMPI D FORZATURA
565000011002    2C     W0133         IFNE      VARIAS
565100950720     C                   CLEAR                   DIE
565200950720     C                   CLEAR                   CIN
565300950720    2C                   ENDIF
565400950720     C*
565500950720     C* SE <> 0 CONTROLLO CHE NON SUPERI IL VALORE IN TARIFFA
565600011002    2C     W0133         IFGT      0
565700950720     C     DIE           ANDNE     1
565800950720     C     TAMFIS        ORNE      '0'
565900011002     C                   Z-ADD     W0133         VARIAS
566000020715
566100020715      * Non controlla da tariffa se obbligatorio nel caso di tipo bolla non previsto
566200020715     c     §3atb1        Lookup    tbl                                    32
566300030728      * non deve controllare anche x le bolle 'FW'- recapito c/assegni
566400030728      * le bolle 'FW' sono tipo bolla 'A2'
566500030728     c                   If        ArbCbo = 'FW'
566600030728     c                   Eval      *In32 = *Off
566700030728     c                   EndIf
566800020715
566900020715   2Ac                   If        *in32 = *On
567000950720     C*
567100950720     C* SOLO SE E' ASSICURAZIONE ANCHE PER GLI ARRIVI ( <> P )
567200990817     C     KTPT          CHAIN     TITPT000                           32
567300950720     C*
567400950720    3C     *IN32         IFEQ      *ON
567500950720     C     TPTATB        ORNE      ' '
567600950720     C     TPTAPL        ORNE      'P'
567700950120     C* TAMFIS=1 OBBLIGATORIO     FORZABILE --> CON ENTER PRENDO MAX
567800950120     C*                                         IMPORTO DA TARIFFA
567900950720    4C     TAMFIS        IFEQ      '1'
568000950120     C     CIN           ANDNE     1
568100011002     C     W0133         ANDEQ     0
568200950120     C                   MOVEL     MSG(16)       VIDMSG
568300950120     C                   SETON                                        4290
568400950120     C                   ADD       1             CIN
568500950120     C                   GOTO      ENDCT6
568600950720    4C                   ENDIF
568700950120     C*
568800950120     C* TAMFIS=3 OBBLIGATORIO NON FORZABILE --> ERRORE SEMPRE
568900950720    4C     TAMFIS        IFEQ      '3'
569000011002     C     W0133         ANDEQ     0
569100950509     C                   MOVEL     MSG(38)       VIDMSG
569200950120     C                   SETON                                        4290
569300950120     C                   GOTO      ENDCT6
569400950720    4C                   ENDIF
569500950120     C*
569600950120     C* TAMFIS=2 NON INSERIBILE             --> ERRORE SEMPRE
569700950720    4C     TAMFIS        IFEQ      '2'
569800011002     C     W0133         ANDNE     0
569900950120     C                   MOVEL     MSG(17)       VIDMSG
570000950120     C                   SETON                                        4290
570100950120     C                   GOTO      ENDCT6
570200950720    4C                   ENDIF
570300950720    3C                   ENDIF
570400950720     C* SE C'E' VALORE MERCE:
570500950720    3C  N32TPTATB        IFEQ      ' '
570600950720     C     TPTAPL        ANDNE     'P'
570700011002     C     W0133         ANDGT     0
570800950720     C     DIE           ANDNE     1
570900950720     C     TPTVLM        ANDGT     0
571000950120     C*
571100950120     C* VALORE A COLLO
571200950120    4C     TPTFVM        IFEQ      '1'
571300991028     C     ARBNCL        MULT      TPTVLM        TPTVAL           13 3
571400950120   X4C                   ELSE
571500950120    5C     TPTFVM        IFEQ      '2'
571600991028     C                   Z-ADD     TPTVLM        TPTVAL
571700950120   X5C                   ELSE
571800950120     C     ARBPKF        ADD       0,9           W0060             6 0
571900991028     C     W0060         MULT      TPTVLM        TPTVAL
572000950120    5C                   END
572100950120    4C                   END
572200950120     C*
572300011204     C                   Z-ADD     VIDIAS        VIDVAL           13 3
572400991028    5C     §TADIV        IFNE      VIDVAS
572500991028     C                   CLEAR                   DSYEUR
572600991028     C                   MOVEL     VIDVAS        YECDVI
572700991028     C                   MOVEL     §TADIV        YECDVO
572800991028     C                   Z-ADD     VIDIAS        YECIMI
572900011003     C                   MOVE      COMDEC        YECDCO
573000991028     C*
573100991028     C                   CALL      'YEURCO'
573200991028     C                   PARM                    DSYEUR
573300991028     C*
573400991028    6C     YECESI        IFEQ      ' '
573500991028     C                   Z-ADD     YECIMO        VIDVAL
573600991028    6C                   ENDIF
573700991028    5C                   ENDIF
573800991028     C***
573900991028    4C     VIDVAL        IFGT      TPTVAL
574000950120     C                   ADD       1             DIE
574100011204     c                   eval      vidmsg = msg(18)
574200950120     C* PREPARO MESSAGGIO
574300011204     C                   SETON                                        4290
574400011204     C                   GOTO      ENDCT6
574500011204    4C                   ENDIF
574600950120     C*
574700950120    3C                   ENDIF
574800020715   2ac                   EndIf
574900950120    2C                   ENDIF
575000950120    1C                   ENDIF
575100950120     C***
575200011127     C*** Q U A N T I T A'  D A  F A T T U R A R E  ***
575300950120     C* CONTROLLO SOLO PER S.F.
575400950120    0C     *IN01         IFEQ      *ON
575500950120     C                   MOVEL     VIDCTR        W001A             1
575600980525     C**
575700980525     C** FLAG FORZATURA QTA DA FATTURARE
575800980525     C     VIDCTR        IFNE      SAVCTR
575900980525     C     *LIKE         DEFINE    VIDCTR        SAVCTR
576000980525     C                   CLEAR                   DUE
576100980525     C                   MOVEL     VIDCTR        SAVCTR
576200980525     C                   ENDIF
576300950120     C*
576400950120     C* SE MODIFICATA, PULISCO IL FLAG DI CONTROLLO
576500950120    1C     VIDQFT        IFNE      VARQFT
576600950120     C                   CLEAR                   UND
576700950120     C                   MOVEL     VIDQFT        VARQFT
576800950120    1C                   ENDIF
576900950120     C*
577000950120     C* CONTROLLO LA Q.TA DA FATTURARE CON TARIFFE
577100950120    1C     *IN29         IFEQ      *OFF
577200950120     C     W001A         ANDEQ     '4'
577300950120     C*
577400950120    2C     VIDQFT        IFEQ      0
577500950120     C*
577600950120     C* TAMFVD = '1' ASSUMO VALORE DA ASSICURARE SE NON IMMESSO
577700950120    3C     TAMFVD        IFEQ      '1'
577800950120     C     UND           ANDEQ     0
577900950120     C                   MOVEL     MSG(33)       VIDMSG
578000950120     C                   SETON                                        5690
578100950120     C                   MOVEL     1             UND
578200950120     C                   GOTO      ENDCT6
578300950120    3C                   ENDIF
578400961007     C* TAMFVD = '2'  O  '4' INSERIRE IN BOLLETTAZIONE
578500950120    3C     TAMFVD        IFEQ      '2'
578600961007    3C     TAMFVD        OREQ      '4'
578700950120     C                   MOVEL     MSG(34)       VIDMSG
578800950120     C                   SETON                                        5690
578900950120     C                   GOTO      ENDCT6
579000950120    3C                   ENDIF
579100950120     C*
579200950120   X2C                   ELSE
579300950120     C* TAMFVD = '3' ASSUME SEMPRE IMPORTO DA ASSICURARE: DA NON INSER
579400950120    3C     TAMFVD        IFEQ      '3'
579500950120     C                   MOVEL     MSG(35)       VIDMSG
579600950120     C                   SETON                                        5690
579700950120     C                   GOTO      ENDCT6
579800950120    3C                   ENDIF
579900950120    2C                   ENDIF
580000950120    1C                   ENDIF
580100950120     c* TARIFFA A QUANTITA'
580200950120    1C     W001A         IFEQ      '5'
580300950120     C     VIDQFT        ANDEQ     0
580400980525     C     DUE           ANDEQ     ' '
580500950120     C                   MOVEL     MSG(36)       VIDMSG
580600950120     C                   SETON                                        5690
580700980525     C                   MOVEL     '1'           DUE               1
580800950120     C                   GOTO      ENDCT6
580900950120    1C                   ENDIF
581000980525    0C                   ENDIF
581100011128      ***
581200011128      *** T A S S A Z I O N E ***
581300950120     C** PRIMA DI CONTROLLARE LA TASSAZIONE SE NON C'E' TOT
581400950120     C**  IMPONIBILE, RITASSO
581500990915     C* SE C'E' IL TOTALE IMPONIBILE RICHIAMO PER CONVERTIRE EVENTUAL-
581600990915     C* MENTE GLI IMPORTI
581700950120     C     *IN01         IFEQ      *OFF
581800950120     C     VIDKSC        ANDNE     9999
581900950120     C     TSZKSC        ORGT      0
582000950120     C     VIDKSC        ANDEQ     9999
582100990915     C* Se divisa sprotetta significa che manca tariffa: in questo caso
582200990915     C* controllo che la divisa inserita sia = alla divisa della moneta
582300990915     C*  di conto SE CLIENTE 9999
582400990915     C     *IN07         IFEQ      *OFF
582500990915     C     VIDKSC        ANDEQ     9999
582600990915     C     VIDDIV        ANDNE     §GEDCN
582700990915     C                   SETON                                        4890
582800990915     C                   MOVEA     MSG(47)       K78
582900990928     C                   MOVEA     §GEDCN        K78(64)
583000990915     C                   MOVEA     K78           VIDMSG
583100990915     C                   GOTO      ENDCT6
583200990915     C                   ENDIF
583300970606     C                   MOVEL     'N'           WCANCN            1
583400980512     C                   MOVEL     '1'           WBOL
583500950120     C                   EXSR      TASSAZ
583600950120     C*
583700950121     C   90              GOTO      ENDCT6
583800950120     C                   ENDIF
583900050926     c* Impossibile eliminare il porto se bolladirottata con porto
584000050926     c*  calcolato
584100050926     c                   if        NOeliminaporto='S' and  vidpor=0
584200050926     C                   SETON                                        4990
584300050926     C                   MOVEL     MSG(79)       VIDMSG
584400050926     C                   GOTO      ENDCT6
584500050926     c                   endif
584600941216     C**
584700991124     C* SE ABBLENCATA DIVISA ERRORE SE ESISTE ALMENO UN IMPORTO DI
584800991124     C* TASSAZIONE
584900991124    1C     *IN07         IFEQ      *OFF
585000991124     C     VIDDIV        ANDNE     WDIV
585100991124     C     VIDDIV        ANDEQ     *BLANKS
585200991124    2C     VIDPOR        IFGT      *ZEROS
585300991125     C                   SETON                                        4890
585400991124     C                   MOVEL     MSG(48)       VIDMSG
585500991124     C                   GOTO      ENDCT6
585600991124   X2C                   ELSE
585700991124    3C                   DO        20            NRR
585800050317     C     NRR           CHAIN     LRA2S06                            30
585900991124    4C     V6CVA1        IFGT      *ZEROS
586000991125     C                   SETON                                        4890
586100991124     C                   MOVEL     MSG(48)       VIDMSG
586200991124     C                   GOTO      ENDCT6
586300991124    4C                   ENDIF
586400991124    3C                   ENDDO
586500991124    2C                   ENDIF
586600991124    1C                   ENDIF
586700990910     C* PER CONTROLLARE LA TASSAZIONE RICHIAMO PGM FNLV16R
586800991013     C* SE LA DIVISA E' MODIFICATA RICHIAMO L'LV16 SOLO PER IL
586900991013     C* CONTROLLO DELLA DIVISA
587000991013    0C     *IN07         IFEQ      *OFF
587100991013     C     VIDDIV        ANDNE     WDIV
587200991026     C     WDIV          ANDNE     *BLANKS
587300100119     C                   CLEAR                   DLV1601
587400991013     C                   CLEAR                   DSLV16
587500991013     C                   CLEAR                   DTASV
587600991013     C                   MOVEL     'D'           D17FIM
587700991013     C                   MOVE      VIDDIV        D17DIV
587800991014     C     *IN01         IFEQ      *OFF
587900991014     C     VIDKSC        ANDNE     9999
588000991014     C     TSZKSC        ORGT      0
588100991014     C     VIDKSC        ANDEQ     9999
588200991014     C                   Z-ADD     DATEU         D17DSP
588300991014     C                   ELSE
588400991013     C                   Z-ADD     ARBDSP        D17DSP
588500991013     C                   END
588600991013     C                   CALL      'FNLV16R'
588700991013     C                   PARM                    DSLV16
588800991013     C                   PARM                    DTASV
588900991014     C                   MOVEL     D17DIV        VIDDIV
589000991013   X0C                   ELSE
589100990910     C                   CLEAR                   DSLV16
589200100119     C                   CLEAR                   DLV1601
589300990910     C                   CLEAR                   DTASV
589400941216     C                   MOVEL     'A'           D17TBO
589500950302     C                   MOVEL     VIDKLP        D17KSC
589600950302     C                   MOVE      VIDKSC        D17KSC
589700991014     C     *IN01         IFEQ      *OFF
589800991014     C     VIDKSC        ANDNE     9999
589900991014     C     TSZKSC        ORGT      0
590000991014     C     VIDKSC        ANDEQ     9999
590100991014     C                   Z-ADD     DATEU         D17DSP
590200991014     C                   ELSE
590300991014     C                   Z-ADD     ARBDSP        D17DSP
590400991014     C                   END
590500050714     c* serve solo per i franchi ...ma imposto lo stesso
590600050714     c*
590700050714     C***                MOVEL     ARBLNP        D17LNP
590800050714     C                   MOVEL     VUNICOLNP     D17LNP
590900941216     C                   MOVEL     ARBLNA        D17LNA
591000941216     C                   MOVEL     §3ATB1        D17TBL
591100941216     C                   MOVEL     §3ARBL        D17RBL
591200941216     C                   Z-ADD     VIDIAS        D17IAS
591300941216     C                   Z-ADD     VIDQFT        D17QFT
591400990910     C                   MOVEL     VIDDIV        D17DIV
591500990914     C                   MOVEL     WDIV          D17DVP
591600941216     C                   Z-ADD     VIDPOR        D17POR
591700941216     C                   Z-ADD     VIDIMV        D17IMV
591800950104    1C     §3AFCA        IFEQ      1
591900941216     C                   MOVEL     'S'           D17FCA
592000941217    1C                   ENDIF
592100990915     C*
592200941221     C     *IN01         IFEQ      *ON
592300941216     C                   MOVEL     'C'           D17FIM
592400941221     C                   ELSE
592500990915     C                   MOVEL     'E'           D17FIM
592600941221     C                   ENDIF
592700941216     C                   MOVEL     VIDCEI        D17CEI
592800100119
592900100119     C* SE IL TIPO BOLLA PREVEDE SOLO UNA VARIA, LA PASSO
593000100119     c                   Movel     §3asva        §d17Sva
593100100120     c
593200100119     c* Imposto il codice iso del destinatario e il flag di controllo in base all'esenz.IVA
593300100120     c                   if        not *in01
593400100119     c                   Eval      §d17ctriso='S'
593500100119     c                   Eval      §d17iso=vidisd
593600100119
593700100119     c                   if        vidpid<>*blanks and vidisd=*blanks
593800100119     c                   eval      §d17iso='IT'
593900100119     c                   endif
594000100120     c                   endif
594100100119
594200100119     c                   Movel     Dlv1601       D17Flo
594300100119     c
594400941216     C*
594500941216     C                   CLEAR                   C
594600970605     C                   CLEAR                   WVARN
594700030604     C                   CLEAR                   WVAR3A
594800040120     c                   Clear                   wvar3ai
594900941217    1C                   DO        20            NRR
595000050317     C     NRR           CHAIN     LRA2S06                            32
595100941220    2C     *IN32         IFEQ      *OFF
595200941216     C                   ADD       1             C
595300970605     C*
595400941216     C                   MOVEL     V6CSV1        VSV(C)
595500941216     C                   MOVEL     V6CVA1        VVA(C)
595600970605     C* MEMORIZZO SE C'E' LA VARIA N
595700970605     C     V6CSV1        IFEQ      'N'
595800970605     C                   Z-ADD     NRR           WVARN             5 0
595900970605     C                   ENDIF
596000030604     C* MEMORIZZO SE C'E' LA VARIA DELLA BOLLA NEL CASO DI BOLLE CON SINGOLA VARIA
596100030604     C                   IF        §3ASVA <> *BLANKS AND V6CSV1 = §3ASVA
596200030604     C                   MOVEL     'S'           WVAR3A            1
596300040120      * memorizzo anche se valorizzata
596400040120     c                   If        v6cva1 > 0
596500040120     c                   Eval      wvar3ai = 'S'
596600040120     c                   EndIf
596700030604     C                   ENDIF
596800941217    2C                   ENDIF
596900941217    1C                   ENDDO
597000941216     C*
597100990910     C                   CALL      'FNLV16R'
597200990910     C                   PARM                    DSLV16
597300990910     C                   PARM                    DTASV
597400941216     C*
597500941216     C                   MOVEL     D17CEI        VIDCEI
597600990923     C                   MOVEL     O17DEI        DESCEI
597700991011     C                   MOVEL     D17DIV        VIDDIV
597800941221     C*
597900990927     C     D17FIM        IFEQ      'E'
598000991014     C     O17ERR        ANDEQ     *BLANKS
598100030604      * OPPURE SE BOLLA CON 1 SOLA VARIA VALORIZZO L'IMPONIBILE ANCHE SE SI TRATTA DI SEGUE FATTURA
598200030604      * SOLO SE LA VARIA ESISTE
598300040120     C**!!!§3ASVA        ORNE      *BLANKS
598400040120     C**!!!WVAR3A        ANDEQ     'S'
598500040120     C**!!!O17ERR        ANDEQ     *BLANKS
598600040120     C**!!!D17FIM        ANDEQ     'C'
598700990910     C                   Z-ADD     O17IMV        VIDIMV
598800990927     C                   ENDIF
598900030604      * se bolla con singola varia e la varia non  c'è azzero l'imponibile per i segue fattura
599000030604     C     §3ASVA        IFNE      *BLANKS
599100030604     C     WVAR3A        ANDNE     'S'
599200030604     C     D17FIM        ANDEQ     'C'
599300030604     C                   Z-ADD     *ZEROS        VIDIMV
599400030604     C                   ENDIF
599500030604     c
599600950421     C* SE ERRORE TASSAZIONE SPROTEGGO CAMPI VIDEO PER POTERLI
599700950421     C* CORREGGERE
599800990910     C     O17ERR        IFNE      *BLANKS
599900950421     C                   SETOFF                                       25
600000950421     C                   END
600100941216     C* VARIE
600200941217    1C                   DO        20            NRR
600300050317     C     NRR           CHAIN     LRA2S06                            32
600400941216     C                   MOVEL     VSV(NRR)      V6CSV1
600500941216     C                   MOVEL     VES(NRR)      V6CEV1
600600941216     C* SE C'E' ERRORE SU VARIA, ACCENDO INDICATORE DI POSIZIONAMENTO
600700990910    2C     O17ERV        IFEQ      NRR
600800941221     C                   SETON                                        5290
600900941216     C                   ELSE
601000941216     C                   SETOFF                                       52
601100941217    2C                   ENDIF
601200050317     C  N32              UPDATE    LRA2S06
601300050317     C   32              WRITE     LRA2S06
601400941217    1C                   ENDDO
601500991014    0C                   ENDIF
601600941216     C*
601700941216     C                   SETOFF                                       52
601800941216     C*  ERRORE
601900990910    1C     O17ERR        IFNE      *BLANKS
602000941221     C                   SETON                                        90
602100941216     C*
602200990910    2C     O17MSG        IFNE      *BLANKS
602300990910     C                   MOVEL     O17MSG        VIDMSG
602400941216    2C                   ENDIF
602500941216     C**
602600941216     C* ESENZIONE IVA
602700990910    2C     O17ERR        IFEQ      '1'
602800941221     C                   SETON                                        51
602900941216    2C                   ENDIF
603000941216     C**
603100941216     C* IMPONIBILE
603200990910     C     O17ERR        IFEQ      '5'
603300941221     C                   SETON                                        50
603400941216    2C                   ENDIF
603500990916     C* DIVISA
603600990916     C     O17ERR        IFEQ      '7'
603700990916     C                   SETON                                        48
603800990916    2C                   ENDIF
603900991012     C* PORTO
604000991012     C     O17ERR        IFEQ      '8'
604100991012     C                   SETON                                        49
604200991012    2C                   ENDIF
604300941216     C**
604400941221     C                   GOTO      ENDCT6
604500941217    1C                   ENDIF
604600941221     C*
604700941221     C* PER COD 9999 CON VARIA "R" OBBLIGATORIO IMPORTO DA ASSIC
604800941221     C* SE C'E' "S" IN D17FAS INDICA QUESTO TIPO DI ERRORE
604900990910    1C     O17FAS        IFEQ      'S'
605000050317     C     1             CHAIN     LRA2S06                            32
605100941221     C                   MOVEL     MSG(9)        VIDMSG
605200941221     C                   SETON                                        5290
605300050317     C                   UPDATE    LRA2S06
605400941221     C                   GOTO      ENDCT6
605500970605    1C                   ENDIF
605600040120      * se tipo bolla 'monovaria' e la varia non è esposta o è negata
605700040120      * non posso avere altre varia con importo > 0
605800040120if  1c                   If        §3asva <> *Blanks
605900040120do  2c                   Do        20            nrr
606000050317     c     nrr           Chain     LRA2s06                            32
606100040120     c                   Eval      *In52 = *Off
606200040121if  3c                   If        wvar3a <> *Blanks  and wvar3ai = *Blanks and
606300040121     c                             V6cSv1 <> *Blanks and V6cVa1 > *Zeros
606400040120     c                   Eval      *In52 = *On
606500040120     c                   Eval      *In90 = *On
606600040121     c                   Eval      VidMsg = Msg(60)
606700040121     c                   Eval      %subst(VidMsg:75:1) = §3asva
606800040120    3c                   EndIf
606900040121if  3c                   If        wvar3a = *Blanks  and
607000040121     c                             V6cSv1 <> *Blanks and V6cVa1 > *Zeros
607100040120     c                   Eval      *In52 = *On
607200040120     c                   Eval      *In90 = *On
607300040121     c                   Eval      VidMsg = Msg(60)
607400040121     c                   Eval      %subst(VidMsg:75:1) = §3asva
607500040120    3c                   EndIf
607600050317     c  n32              Update    LRA2s06
607700040127     c   90              Goto      Endct6
607800040120    2c                   EndDo
607900040120    1c                   EndIf
608000990914     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
608100990914    1C  N07VIDDIV        IFNE      WDIV
608200990914     C* CONVERSIONE PORTO
608300990914    2C     VIDPOR        IFEQ      VARPOR
608400990914     C     VIDPOR        ANDGT     *ZEROS
608500990914     C     WDIV          ANDNE     *BLANKS
608600990914     C                   CLEAR                   DSYEUR
608700990914     C                   MOVEL     WDIV          YECDVI
608800990914     C                   Z-ADD     VIDPOR        YECIMI
608900990914     C                   MOVEL     VIDDIV        YECDVO
609000990928     C     O17FDC        IFEQ      'S'
609100990914     C                   MOVEL     '3'           YECDCO
609200990928     C                   ELSE
609300990928     C                   CLEAR                   YECDCO
609400990928     C                   ENDIF
609500990914     C                   CALL      'YEURCO'
609600990914     C                   PARM                    DSYEUR
609700990914     C                   Z-ADD     YECIMO        VIDPOR
609800991013     C                   SETON                                        49
609900990914    2C                   END
610000990914     C* CONVERSIONE VARIE
610100990914    2C                   DO        20            NRR
610200050317     C     NRR           CHAIN     LRA2S06                            32
610300990914    3C     V6CVA1        IFEQ      VARVA1
610400990914     C     V6CVA1        ANDGT     *ZEROS
610500990928     C     WDIV          ANDNE     *BLANKS
610600011019      *
610700011019      * Se la bolla non è tassata fino all'imponibile e c'è la varia 'N' 88888888
610800011019      * non devo convertire la varia 'N' 88888888
610900011019     C     VIDIMV        IFEQ      *ZEROS
611000011019     C     FLGVRN        ANDEQ     '1'
611100011019     C     V6CSV1        ANDEQ     'N'
611200011019     C     V6CVA1        ANDEQ     88888888
611300050317     C  N32              UPDATE    LRA2S06
611400011019     C                   ITER
611500011019     C                   ENDIF
611600030624      * Se la bolla non è tassata fino all'imponibile e c'è la varia '&' 88888888
611700030624      * non devo convertire la varia '&' 88888888
611800030624     C     VIDIMV        IFEQ      *ZEROS
611900030624     C     V6CSV1        ANDEQ     '&'
612000030624     C     V6CVA1        ANDEQ     88888888
612100050317     C  N32              UPDATE    LRA2S06
612200030624     C                   ITER
612300030624     C                   ENDIF
612400011019      *
612500990914     C                   CLEAR                   DSYEUR
612600990914     C                   MOVEL     WDIV          YECDVI
612700990928     C                   Z-ADD     V6CVA1        YECIMI
612800990914     C                   MOVEL     VIDDIV        YECDVO
612900990928     C     O17FDC        IFEQ      'S'
613000990914     C                   MOVEL     '3'           YECDCO
613100990928     C                   ELSE
613200990928     C                   CLEAR                   YECDCO
613300990928     C                   ENDIF
613400990914     C                   CALL      'YEURCO'
613500990914     C                   PARM                    DSYEUR
613600990914     C                   Z-ADD     YECIMO        V6CVA1
613700991013     C                   SETON                                        53
613800990914    3C                   ENDIF
613900990914     C                   Z-ADD     V6CVA1        VARVA1
614000050317     C  N32              UPDATE    LRA2S06
614100990914    2C                   ENDDO
614200990915     C* SE TOTALE IMPONIBILE E' IMPOSTATO LO AZZERO E RICHIAMO
614300990915     C* FNLV16R PER RICALCOLARLO IN BASE ALLA NUOVA DIVISA
614400990915     C     VIDIMV        IFGT      *ZEROS
614500990915     C     WDIV          ANDNE     *BLANKS
614600990915     C                   MOVEL     'T'           D17FIM
614700990915     C                   MOVEL     *ZEROS        D17IMV
614800990927     C                   Z-ADD     VIDPOR        D17POR
614900991014     C                   CLEAR                   C
615000991014    1C                   DO        20            NRR
615100050317     C     NRR           CHAIN     LRA2S06                            32
615200991014    2C     *IN32         IFEQ      *OFF
615300991014     C                   ADD       1             C
615400991014     C                   MOVEL     V6CSV1        VSV(C)
615500991014     C                   MOVEL     V6CVA1        VVA(C)
615600991014    2C                   ENDIF
615700991014    1C                   ENDDO
615800990915     C                   CALL      'FNLV16R'
615900990915     C                   PARM                    DSLV16
616000990915     C                   PARM                    DTASV
616100990915     C                   Z-ADD     O17IMV        VIDIMV
616200990915     C                   ENDIF
616300990914     C                   Z-ADD     VIDPOR        VARPOR
616400990914     C                   MOVEL     VIDDIV        WDIV
616500991013     C                   SETON                                        90
616600991013     C                   GOTO      ENDCT6
616700990929   X1C                   ELSE
616800990929     C* SE NON VARIATA LA DIVISA DEVO COMUNQUE MEMORIZZARE EVENTUALI
616900990929     C* VARIAZIONI EFFETTUATE SUGLI IMPORTI DI TASSAZIONE
617000990929     C                   Z-ADD     VIDPOR        VARPOR
617100990929    2C                   DO        20            NRR
617200050317     C     NRR           CHAIN     LRA2S06                            32
617300990929     C                   Z-ADD     V6CVA1        VARVA1
617400050317     C  N32              UPDATE    LRA2S06
617500990929    2C                   ENDDO
617600990914    1C                   ENDIF
617700020222
617800011018      * Se è bolla di reso con varia 'N' 88888888 deve esserci la varia 'N'
617900011018     C                   SETOFF                                       47
618000011018    1C     FLGVRN        IFEQ      '1'
618100011008    2C                   DO        20            NRR
618200050317     C     NRR           CHAIN     LRA2S06                            32
618300011008    3C     *IN32         IFEQ      *OFF
618400011018     C                   SETON                                        47
618500011018    4C     V6CSV1        IFEQ      'N'
618600011018     C                   SETOFF                                       47
618700011008     C                   LEAVE
618800011018    4C                   ENDIF
618900011008    3C                   ENDIF
619000011008    2C                   ENDDO
619100011018    2C     *IN47         IFEQ      '1'
619200050317     C     1             CHAIN     LRA2S06                            32
619300050317     C  N32              UPDATE    LRA2S06
619400011008     C                   MOVEL     MSG(53)       VIDMSG
619500011106     C                   SETON                                        90
619600011008     C                   GOTO      ENDCT6
619700011008    2C                   ENDIF
619800011008    1C                   ENDIF
619900011127      *
620000011127      * Controllo che la somma degli importi immessi non superi il valore
620100011127      * impostato in §GEIMF
620200011127      *      prima controllo il totale imponibile
620300011127  1b c                   if        vidimv <> 0
620400011127     c                   eval      w0173 = vidimv
620500011127      *      controllo se devo convertire il totale imponibile
620600011127  2b c                   if        viddiv <> §gedcn
620700011127     c                   clear                   dsyeur
620800011127     c                   eval      yecdvi = viddiv
620900011127     c                   eval      yecdvo = §gedcn
621000011127     c                   eval      yecimi = vidimv
621100011127     c                   move      decsav        yecdco
621200011127     c                   call      'YEURCO'
621300011127     c                   parm                    dsyeur
621400011127  3b c                   if        yecesi = ' '
621500011127     c                   eval      w0173 = yecimo
621600011127  3e c                   endif
621700011127  2e c                   endif
621800011127      *      controllo se l'imponibile supera il limite fatturabile
621900011220  2b c                   if        w0173 > sav_geimf
622000011127     c                   eval      vidmsg = msg(55)
622100011220     c                   eval      %subst (vidmsg:54:14)
622200011220     c                              = (%editc(sav_geimf: '4'))
622300011203     c                   eval      %subst (vidmsg:69:3) = §gedcn
622400011127     c                   eval      *in49 = *on
622500011127     c                   eval      *in90 = *on
622600011127     c                   goto      endct6
622700011127  2e c                   endif
622800011127  1x c                   else
622900011127      *      altrimenti controllo tutti gli altri importi
623000011127     c                   eval      tot_importi = vidpor
623100011127      *      leggo le varie
623200011127  2b c                   do        20            nrr
623300050317     c     nrr           chain     LRA2s06                            32
623400011127  3b c                   if        *in32 = *off and v6cva1 <> 0
623500011127  4b c                   if        flgvrn = '1' and v6cva1 = 88888888
623600011127     c                             and v6csv1 = 'N'
623700011127     c                   iter
623800011127  4e c                   endif
623900030624  4b c                   if        v6cva1 = 88888888
624000030624     c                             and v6csv1 = '&'
624100030624     c                   iter
624200030624  4e c                   endif
624300011127     c                   eval      tot_importi = tot_importi + v6cva1
624400011127  3e c                   endif
624500011127  2e c                   enddo
624600011127     c                   eval      w0173 = tot_importi
624700011127      *      controllo se devo convertire il totale importi
624800011127  2b c                   if        viddiv <> §gedcn
624900011127     c                   clear                   dsyeur
625000011127     c                   eval      yecdvi = viddiv
625100011127     c                   eval      yecdvo = §gedcn
625200011127     c                   eval      yecimi = tot_importi
625300011127     c                   move      decsav        yecdco
625400011127     c                   call      'YEURCO'
625500011127     c                   parm                    dsyeur
625600011127  3b c                   if        yecesi = ' '
625700011127     c                   eval      w0173 = yecimo
625800011127  3e c                   endif
625900011127  2e c                   endif
626000011127      *      controllo se l'imponibile supera il limite fatturabile
626100011220  2b c                   if        w0173 > sav_geimf
626200011127     c                   eval      vidmsg = msg(55)
626300011220     c                   eval      %subst (vidmsg:54:14)
626400011220     c                             = (%editc(sav_geimf: '4'))
626500011203     c                   eval      %subst (vidmsg:69:3) = §gedcn
626600011127     c                   eval      *in49 = *on
626700011127     c                   eval      *in90 = *on
626800011127     c                   goto      endct6
626900011127  2e c                   endif
627000011127  1e c                   endif
627100970605     C*
627200941217     C     ENDCT6        ENDSR
627300911028     C*
627400911028     C*--- CONTROLLI CAMPI TASSAZIONE ESTENSIONE BOLLA ---------------*
627500911028     C     CONTR7        BEGSR
627600911030     C                   SETOFF                                       90
627700040121     C**!!!              MOVEL     ' '           CEISV3            1
627800920319     C*
627900040119     C**!!!VI2VA1        IFEQ      0
628000040119     C**!!!              MOVEL     MSG(30)       VIDMSG
628100040119     C**!!!              SETON                                        749028
628200040119     C**!!!              GOTO      ENDCT7
628300040119     C**!!!              END
628400990923     C* Se divisa sprotetta significa che manca tariffa: in questo caso
628500990923     C* controllo che la divisa inserita sia = alla divisa della moneta
628600990923     C*  di conto SE CLIENTE 9999
628700990923     C     *IN07         IFEQ      *OFF
628800990923     C     TSZKSC        ANDGT     0
628900990923     C     VIDKSC        ANDEQ     9999
629000990929     C     VI2DIV        ANDNE     §GEDCN
629100990929     C                   SETON                                        489028
629200990923     C                   MOVEA     MSG(47)       K78
629300990928     C                   MOVEA     §GEDCN        K78(64)
629400990923     C                   MOVEA     K78           VIDMSG
629500990923     C                   GOTO      ENDCT7
629600990923     C                   ENDIF
629700990915     C* DEMANDO IL RESTO DEI CONTROLLI A PGM FNLV16R
629800990915     C                   CLEAR                   DSLV16
629900100119     C                   CLEAR                   DLV1601
630000990915     C                   CLEAR                   DTASV
630100990915     C                   MOVEL     'A'           D17TBO
630200990915     C                   MOVEL     VIDKLP        D17KSC
630300990915     C                   MOVE      VIDKSC        D17KSC
630400991014     C     *IN01         IFEQ      *OFF
630500991014     C     VIDKSC        ANDNE     9999
630600991014     C     TSZKSC        ORGT      0
630700991014     C     VIDKSC        ANDEQ     9999
630800991014     C                   Z-ADD     DATEU         D17DSP
630900991014     C                   ELSE
631000991014     C                   Z-ADD     ARBDSP        D17DSP
631100991014     C                   END
631200050714     C****               MOVEL     ARBLNP        D17LNP
631300050714     C                   MOVEL     VUNICOLNP     D17LNP
631400990915     C                   MOVEL     ARBLNA        D17LNA
631500990915     C                   MOVEL     §3ATB2        D17TBL
631600990915     C                   MOVEL     §3ARBL        D17RBL
631700990915     C                   MOVEL     VI2DIV        D17DIV
631800990915     C                   MOVEL     WDIV          D17DVP
631900990928    1C     §3AFCA        IFEQ      2
632000990915     C                   MOVEL     'S'           D17FCA
632100990915    1C                   ENDIF
632200990915     C                   MOVEL     'E'           D17FIM
632300040123     c                   Movel     §3asva        §d17Sva
632400040119      * passo che devo controllare la seconda bolla
632500040119     c                   Eval      §d17Tb2 = 'S'
632600100120     c
632700100119     c* Imposto il codice iso del destinatario e il flag di controllo in base all'esenz.IVA
632800100120     c                   if        not *in01
632900100119     c                   Eval      §d17ctriso='S'
633000100119     c                   Eval      §d17iso=vidisd
633100100119
633200100119     c                   if        vidpid<>*blanks and vidisd=*blanks
633300100119     c                   eval      §d17iso='IT'
633400100119     c                   endif
633500100120     c                   endif
633600100120     c
633700040119     c                   Movel     dlv1601       D17Flo
633800990915     C                   MOVEL     VI2CEI        D17CEI
633900990915     C                   MOVEL     VI2SV1        VSV(1)
634000040119     C**!!!              MOVEL     VI2VA1        VVA(1)
634100040119     c                   Z-add     Vi2Va1        vva(1)
634200040121     c                   Movel     Vi2Es1        ves(1)
634300040119     c                   Movel     Vi2Sv2        vsv(2)
634400040121     c                   Movel     Vi2Es2        ves(2)
634500040119     c                   Z-add     Vi2Va2        vva(2)
634600040119     c                   MOVEL     Vi2Sv3        vsv(3)
634700040119     c                   Z-add     Vi2Va3        vva(3)
634800040121     c                   Movel     Vi2Es3        ves(3)
634900040119     c                   MOVEL     Vi2Sv4        vsv(4)
635000040119     c                   Z-add     Vi2Va4        vva(4)
635100040121     c                   Movel     Vi2Es4        ves(4)
635200041129     c                   MOVEL     Vi2Sv5        vsv(5)
635300041129     c                   Z-add     Vi2Va5        vva(5)
635400041129     c                   Movel     Vi2Es5        ves(5)
635500041129     c                   MOVEL     Vi2Sv6        vsv(6)
635600041129     c                   Z-add     Vi2Va6        vva(6)
635700041129     c                   Movel     Vi2Es6        ves(6)
635800990915     C                   CALL      'FNLV16R'
635900990915     C                   PARM                    DSLV16
636000990915     C                   PARM                    DTASV
636100040119
636200990915     C                   MOVEL     D17CEI        VI2CEI
636300990915     C                   MOVEL     O17DEI        DE2CEI
636400991011     C                   MOVEL     D17DIV        VI2DIV
636500040119     C**!!!              MOVEL     VSV(1)        VI2SV1
636600040121     C**!!!              MOVEL     VES(1)        CEISV3
636700040119      * Ricarico le varie
636800040119     c                   Movel     Vsv(1)        Vi2Sv1
636900040119     c                   Z-add     Vva(1)        Vi2Va1
637000040121     c                   Movel     Ves(1)        Vi2Es1
637100040119     c                   Movel     Vsv(2)        Vi2Sv2
637200040119     c                   Z-add     Vva(2)        Vi2Va2
637300040121     c                   Movel     Ves(2)        Vi2Es2
637400040119     c                   Movel     Vsv(3)        Vi2Sv3
637500040119     c                   Z-add     Vva(3)        Vi2Va3
637600040121     c                   Movel     Ves(3)        Vi2Es3
637700040119     c                   Movel     Vsv(4)        Vi2Sv4
637800040119     c                   Z-add     Vva(4)        Vi2Va4
637900040121     c                   Movel     Ves(4)        Vi2Es4
638000041129     c                   Movel     Vsv(5)        Vi2Sv5
638100041129     c                   Z-add     Vva(5)        Vi2Va5
638200041129     c                   Movel     Ves(5)        Vi2Es5
638300041129     c                   Movel     Vsv(6)        Vi2Sv6
638400041129     c                   Z-add     Vva(6)        Vi2Va6
638500041129     c                   Movel     Ves(6)        Vi2Es6
638600040119      * Controllo se caricata almeno una varia
638700041129do  1c                   Do        6             c
638800040122     c                   If        vsv(c) <> *Blanks
638900040119     c                   Eval      wesvar1 = 'S'
639000040119     c                   EndIf
639100040119    1c                   EndDo
639200040119
639300990915     C     O17ERR        IFNE      *BLANKS
639400990915     C                   SETON                                        90
639500990915     C     O17MSG        IFNE      *BLANKS
639600990915     C                   MOVEL     O17MSG        VIDMSG
639700990915     C                   SETON                                        28
639800990915     C                   ENDIF
639900990915     C     O17ERR        IFEQ      '6'
640000040119     c                   Select
640100040119     c                   When      O17Erv = 1
640200990915     C                   SETON                                        72
640300040119     c                   When      O17Erv = 2
640400040119     C                   SETON                                        71
640500040119     c                   When      O17Erv = 3
640600040119     C                   SETON                                        76
640700040119     c                   When      O17Erv = 4
640800040119     C                   SETON                                        78
640900041129     c                   When      O17Erv = 5
641000041129     C                   SETON                                        80
641100041129     c                   When      O17Erv = 6
641200041129     C                   SETON                                        82
641300040119     c                   EndSl
641400990915     C                   ENDIF
641500990915     C     O17ERR        IFEQ      '1'
641600990915     C                   SETON                                        75
641700990915     C                   ENDIF
641800990916     C     O17ERR        IFEQ      '7'
641900990916     C                   SETON                                        48
642000990916     C                   ENDIF
642100990916     C   90              GOTO      ENDCT7
642200990915     C                   ENDIF
642300990916     C* SE VARIATA DIVISA DEVO CONVERTIRE GLI IMPORTI NON MODIFICATI
642400990916    1C  N07VI2DIV        IFNE      WDIV
642500990916     C* CONVERSIONE VARIA
642600990916    3C     VI2VA1        IFEQ      VA2VA1
642700990916     C     VI2VA1        ANDGT     *ZEROS
642800990928     C     WDIV          ANDNE     *BLANKS
642900990916     C                   CLEAR                   DSYEUR
643000990916     C                   MOVEL     WDIV          YECDVI
643100990916     C                   Z-ADD     VI2VA1        YECIMI
643200990916     C                   MOVEL     VI2DIV        YECDVO
643300990928     C     O17FDC        IFEQ      'S'
643400990916     C                   MOVEL     '3'           YECDCO
643500990928     C                   ELSE
643600990928     C                   CLEAR                   YECDCO
643700990928     C                   ENDIF
643800990916     C                   CALL      'YEURCO'
643900990916     C                   PARM                    DSYEUR
644000990916     C                   Z-ADD     YECIMO        VI2VA1
644100990916     C                   SETON                                        7490
644200990916    3C                   ENDIF
644300040119     C**!!!              Z-ADD     VI2VA1        VA2VA1
644400040119     C**!!!              Z-ADD     VI2VA1        VVA(1)
644500040119     C**!!!              MOVEL     VI2DIV        WDIV
644600990916     C   90              GOTO      ENDCT7
644700040119      * Conversione Varia 2
644800040119    2c                   IF        Vi2Va2 = Va2Va2 and Vi2Va2 > *Zeros and
644900040119     c                             wdiv <> *Blanks
645000040119     c                   Clear                   DSYEUR
645100040119     c                   Movel     wdiv          YECDVI
645200040119     c                   Z-add     Vi2Va2        YECIMI
645300040119     c                   Movel     Vi2Div        YECDVO
645400040119     c                   If        O17Fdc = 'S'
645500040119     c                   Movel     '3'           YECDCO
645600040119     c                   EndIf
645700040119     c                   Call      'YEURCO'
645800040119     c                   Parm                    DSYEUR
645900040119     c                   Z-add     YECIMO        Vi2Va2
646000040119     c                   Eval      *In90 = *On
646100040119     c                   Eval      *In73 = *On
646200040119    2c                   EndIf
646300040119     c   90              Goto      ENDCT7
646400040119      * Conversione Varia 3
646500040119    2c                   IF        Vi2Va3 = Va2Va3 and Vi2Va3 > *Zeros and
646600040119     c                             wdiv <> *Blanks
646700040119     c                   Clear                   DSYEUR
646800040119     c                   Movel     wdiv          YECDVI
646900040119     c                   Z-add     Vi2Va3        YECIMI
647000040119     c                   Movel     Vi2Div        YECDVO
647100040119     c                   If        O17Fdc = 'S'
647200040119     c                   Movel     '3'           YECDCO
647300040119     c                   EndIf
647400040119     c                   Call      'YEURCO'
647500040119     c                   Parm                    DSYEUR
647600040119     c                   Z-add     YECIMO        Vi2Va3
647700040119     c                   Eval      *In90 = *On
647800040119     c                   Eval      *In77 = *On
647900040119    2c                   EndIf
648000040119     c   90              Goto      ENDCT7
648100040119      * Conversione Varia 4
648200040119    2c                   IF        Vi2Va4 = Va2Va4 and Vi2Va4 > *Zeros and
648300040119     c                             wdiv <> *Blanks
648400040119     c                   Clear                   DSYEUR
648500040119     c                   Movel     wdiv          YECDVI
648600040119     c                   Z-add     Vi2Va4        YECIMI
648700040119     c                   Movel     Vi2Div        YECDVO
648800040119     c                   If        O17Fdc = 'S'
648900040119     c                   Movel     '3'           YECDCO
649000040119     c                   EndIf
649100040119     c                   Call      'YEURCO'
649200040119     c                   Parm                    DSYEUR
649300040119     c                   Z-add     YECIMO        Vi2Va4
649400040119     c                   Eval      *In90 = *On
649500040119     c                   Eval      *In79 = *On
649600040119    2c                   EndIf
649700041129     c   90              Goto      ENDCT7
649800041129      * Conversione Varia 5
649900041129    2c                   IF        Vi2Va5 = Va2Va5 and Vi2Va5 > *Zeros and
650000041129     c                             wdiv <> *Blanks
650100041129     c                   Clear                   DSYEUR
650200041129     c                   Movel     wdiv          YECDVI
650300041129     c                   Z-add     Vi2Va5        YECIMI
650400041129     c                   Movel     Vi2Div        YECDVO
650500041129     c                   If        O17Fdc = 'S'
650600041129     c                   Movel     '3'           YECDCO
650700041129     c                   EndIf
650800041129     c                   Call      'YEURCO'
650900041129     c                   Parm                    DSYEUR
651000041129     c                   Z-add     YECIMO        Vi2Va5
651100041129     c                   Eval      *In90 = *On
651200041129     c                   Eval      *In81 = *On
651300041129    2c                   EndIf
651400041129     c   90              Goto      ENDCT7
651500041129      * Conversione Varia 6
651600041129    2c                   IF        Vi2Va6 = Va2Va6 and Vi2Va6 > *Zeros and
651700041129     c                             wdiv <> *Blanks
651800041129     c                   Clear                   DSYEUR
651900041129     c                   Movel     wdiv          YECDVI
652000041129     c                   Z-add     Vi2Va6        YECIMI
652100041129     c                   Movel     Vi2Div        YECDVO
652200041129     c                   If        O17Fdc = 'S'
652300041129     c                   Movel     '3'           YECDCO
652400041129     c                   EndIf
652500041129     c                   Call      'YEURCO'
652600041129     c                   Parm                    DSYEUR
652700041129     c                   Z-add     YECIMO        Vi2Va6
652800041129     c                   Eval      *In90 = *On
652900041129     c                   Eval      *In82 = *On
653000041129    2c                   EndIf
653100041129     c   90              Goto      ENDCT7
653200990916    1C                   ENDIF
653300040121
653400040121     c                   Z-add     Vi2Va1        Va2Va1
653500040121     c                   Z-add     Vi2Va2        Va2Va2
653600040121     c                   Z-add     Vi2Va3        Va2Va3
653700040121     c                   Z-add     Vi2Va4        Va2Va4
653800041129     c                   Z-add     Vi2Va5        Va2Va5
653900041129     c                   Z-add     Vi2Va6        Va2Va6
654000040121     c                   Z-add     Vi2Va1        Vva(1)
654100040121     c                   Z-add     Vi2Va2        Vva(2)
654200040121     c                   Z-add     Vi2Va3        Vva(3)
654300040121     c                   Z-add     Vi2Va4        Vva(4)
654400041129     c                   Z-add     Vi2Va5        Vva(5)
654500041129     c                   Z-add     Vi2Va6        Vva(6)
654600040119     c                   Movel     Vi2Div        Wdiv
654700040121     c   90              Goto      ENDCT7
654800040121
654900011128      * Controllo che l'importo immesso non superi il valore
655000011128      * impostato in §GEIMF
655100040119     c**!!!              eval      w0173 = vi2va1
655200040122     c                   xfoot     vva           w0173
655300011128      *      controllo se devo convertire l'importo
655400011128  2b c                   if        vi2div <> §gedcn
655500011128     c                   clear                   dsyeur
655600011128     c                   eval      yecdvi = vi2div
655700011128     c                   eval      yecdvo = §gedcn
655800040119     c**!!!              eval      yecimi = vi2va1
655900040119     c                   eval      yecimi = w0173
656000011128     c                   move      decsav        yecdco
656100011128     c                   call      'YEURCO'
656200011128     c                   parm                    dsyeur
656300011128  3b c                   if        yecesi = ' '
656400011128     c                   eval      w0173 = yecimo
656500011128  3e c                   endif
656600011128  2e c                   endif
656700011128      *      controllo se l'importo supera il limite fatturabile
656800011220  2b c                   if        w0173 > sav_geimf
656900011129     c                   eval      vidmsg = msg(56)
657000011220     c                   eval      %subst (vidmsg:45:14)
657100011220     c                              = (%editc(sav_geimf: '4'))
657200011203     c                   eval      %subst (vidmsg:60:3) = §gedcn
657300040119     c**!!!              eval      *in74 = *on
657400011128     c                   eval      *in28 = *on
657500011128     c                   eval      *in90 = *on
657600011128     c                   goto      endct7
657700011128  2e c                   endif
657800040122
657900040122     c                   Z-add     w0173         TotImv
658000940223     C*
658100941217     C     ENDCT7        ENDSR
658200911028     C*
658300911028     C*--- AGGIORNO BOLLE --------------------------------------------*
658400050310     C     AGGIORARB     BEGSR
658500941217     C                   SETOFF                                       09
658600941217     C* GIRO UDATE
658700941217     C                   MOVE      UDATE         DATAAS            2 0
658800941217     C                   TIME                    W0140            14 0
658900941217     C* UDATE IN GGMMAAAA
659000941217     C                   MOVE      W0140         WDTGIO            8 0
659100941217     C*
659200941217     C* UDATE IN AAAAMMGG
659300941217     C                   Z-ADD     WDTGIO        G02DAT
659400941217     C                   MOVEL     *BLANK        G02ERR
659500941217     C                   CALL      'XSRDA8'
659600941217     C                   PARM                    WLBDAT
659700941217     C                   MOVEL     G02INV        DATEU             8 0
659800941217     C*
659900920409     C                   Z-ADD     DATEU         ARBDTV
660000920409     C                   TIME                    ARBORV
660100941216     C                   MOVEL     KNMUS         ARBPRU
660200140120     c                   z-add     arbdtv        OLRA2DTV
660300140120     c                   z-add     arborv        OLRA2orv
660400140120     c                   movel     arbpru        OLRA2pru
660500050317     C                   MOVEL     ILRA2CVB      ARBCVB
660600990924     C                   CLEAR                   ARBFST
660700990924     C                   CLEAR                   ARBDDS
660800941217     C*
660900941217     C* E S T E N S I O N E   B O L L A
661000941217     C*
661100950203    1C   05              DO
661200990930     C* ESEGUO CLEAR CAMPI CHE PER 2A BOLLA NON DEVONO ESSERE IMPOSTATI
661300990930     C* (CAMPI DEL FILE DI STORICIZZAZIONE E TRASMISSIONE)
661400990930     C* PRIMA DI CLEARARLI LI SALVO PERCHè SONO ANCHE CAMPI DI FNARB
661500990930     C* CHE POI ANDRO' AD AGGIORNARE
661600990930     C* DEVO SALVARE ANCHE KSC E CTR PERCHE' SU ARB SONO RIFERITI ALL
661700990930     C* 1A BOLLA MENTRE SU ARBT SONO RIFERITI ALLA 2A BOLLA
661800990930     C                   Z-ADD     ARBQFT        SAVQFT
661900990930     C                   Z-ADD     ARBVMD        SAVVMD
662000990930     C                   MOVEL     ARBVAD        SAVVAD
662100990930     C                   MOVEL     ARBIAS        SAVIAS
662200990930     C                   MOVEL     ARBVAS        SAVVAS
662300990930     C                   Z-ADD     ARBKSC        KSCSAV
662400990930     C                   Z-ADD     ARBCTR        CTRSAV
662500990924     C                   CLEAR                   ARBQFT
662600990924     C                   CLEAR                   ARBVMD
662700990924     C                   CLEAR                   ARBVAD
662800990924     C                   CLEAR                   ARBIAS
662900990924     C                   CLEAR                   ARBVAS
663000941217     C* INVIO PER SEDE
663100170116     C****               MOVEL     'FIARBT0T'    SFILE             8
663200941217     C* ALLOCO OGGETTI
663300170116     C***                EXSR      ALLOC
663400170116     C***91              GOTO      ENDAGG
663500941217     C*
663600040121     C* SOLO SE FATTURA IMMEDIATA
663700040121    2C     *IN01         IFEQ      *OFF
663800040121     C                   SETON                                        32
663900040121     C* CONTROLLO SE RITASSAZIONE E SE ESISTENTE
664000040122    3C   12AR6NFT        IFNE      0
664100040121     C                   SETOFF                                       32
664200040121     c                   Z-add     Ar6Nft        wnft
664300040121     c                   Z-add     Ar6Fiv        fiv
664400040121    3C                   END
664500040121     C* LIBRO IVA
664600040121    3C   32              DO
664700040121     C                   clear                   DSBS02
664800040121     C                   clear                   DYIV
664900040121     C                   movel     'C'           T02mod
665000040121     C                   movel     KNSIF         T02sif
665100040121     C                   movel     'YIV'         T02cod
665200040121     C                   movel     C_Soc         T02ke1
665300040121     C                   movel     ARBLNA        T02ke2
665400040121     C                   call      'TIBS02R'
665500040121     C                   parm                    KPJBA
665600040121     C                   parm                    DSBS02
665700040121    4C                   IF        T02err = *blanks
665800040121     C                   movel     T02uni        DYIV
665900040126     C                   MOVEL     §IVLVF        FIV
666000040121     C                   ELSE
666100040126     C                   MOVEL     ARBLNA        FIV
666200040121    4C                   END
666300040121     C**
666400040121     C* NUMERO FATTURA
666500040121     C     KNUM          CHAIN     CNNUM000                           32
666600040121     C* ERRORE : MANCA IL NUMERATORE DELLE FATTURE
666700070222    4C     *IN32         IFEQ      *ON
666800040121     C                   MOVEL     MSG(41)       VIDMSG
666900070222    5C     §7LFFI        ifeq      'S'
667000170116     C****               EXSR      DLCSED
667100070222    5C                   ENDIF
667200040121     C**
667300040121     C                   SETON                                        9128
667400040121     C                   GOTO      ENDAGG
667500070222    4C                   ENDIF
667600040121     C*
667700040121     C                   ADD       1             NUMNUM
667800040121     C                   Z-ADD     NUMNUM        wnft
667900040121     C                   MOVE      DATEU         NUMDTP
668000040121     C                   UPDATE    CNNUM000
668100040122    3C                   ENDDO
668200040122     C* SE ERA S.F. E DIVENTA FATTURA IMMEDITATA --> SE GIA' CONSEGNATA
668300040122     C*  DIVENTA RITORNO ALL'INCASSO
668400040122    3C   12ARBDCM        IFGT      0
668500040122     C                   MOVEL     'R'           ARBICA
668600040122    3C                   ENDIF
668700040121    2C                   ENDIF
668800040121      * carico le varie in una sk di comodo
668900040122     c                   ExSr      CarWar1
669000040122     C* SE FATTURA IMMEDIATA CALCOLO IL TOTALE FATTURA:
669100040122     C* LO FACCIO ORA PERCHE' BOLLO E IVA SONO DIVENTATE VARIE
669200040122     C* E LE DEVO QUINDI CARICARE NELLA SCHIERA DI COMODO
669300040122    2C     *IN01         IFEQ      *OFF
669400040122     C                   MOVEL     VI2DIV        ARBDIV
669500040122     C                   EXSR      RIEGEI
669600040122     C                   CLEAR                   DSLV04
669700040122     C                   MOVEL     'T'           D04FIM
669800040122     C                   Z-ADD     DATEU         D04DFT
669900131001      * se c'è già la data fattura devo passarla
670000131001      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
670100131001      *  di nuovo una fattura immediata)
670200131001     c                   if        ar6dft <> 0
670300131001     c                   eval      d04dft = ar6dft
670400131001     c                   endif
670500040122     C                   MOVEL     §GEAED        D04AED
670600040122     C                   Z-ADD     §GEAVL        D04VAR
670700040122     c                   Z-add     TotImv        D04Imv
670800040122     C                   MOVEL     VI2CEI        D04CEI
670900040122     C                   MOVEL     AR9TIC        D04TIC
671000040122     C                   MOVEL     AR9VCA        D04VCA
671100040122     C                   Z-ADD     AR9CAS        D04CAS
671200040122     C                   MOVEL     '2'           D04SPE
671300040122     C                   MOVEL     §3ATB2        D04TBL
671400040122     C                   MOVEL     VI2DIV        D04DIV
671500040122     C                   MOVEL     '1'           UNO
671600040122     C*
671700040122     C                   CALL      'FNLV04R'
671800040122     C                   PARM                    DSLV04
671900040122     C                   PARM                    DTASV
672000040122     C*
672100040122     C     D04BOL        IFGT      *ZEROS
672200040122     C                   ADD       1             C
672300040122     C                   MOVEL     §$2BOL        WSV(C)
672400040122     C                   Z-ADD     D04BOL        WVA(C)
672500040122     C                   ENDIF
672600040122     C     D04IVA        IFGT      *ZEROS
672700040122     C                   ADD       1             C
672800040122     C                   MOVEL     §$2IVA        WSV(C)
672900040122     C                   Z-ADD     D04IVA        WVA(C)
673000040122     C                   ENDIF
673100040122    2C                   ENDIF
673200040122     C*
673300040122     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
673400040122    2C     C             IFGT      0
673500040122     C                   SETON                                        09
673600040122     C*
673700040122     C* INVIO PER SEDE
673800040122     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
673900170116    3C***  C             IFGT      3
674000040122     C* MI SALVO I CAMPI DELL'FIARBT
674100170116     C***                MOVEL     SOVR(1)       SOARBT
674200170116     C***                MOVEL     SALC(1)       SAARBT
674300170116     C***                MOVEL     SDLC(1)       SDARBT
674400040122     C*
674500170116     C***                MOVEL     'FIARBU0T'    SFILE
674600040122     C* ALLOCO OGGETTI
674700170116     C****               EXSR      ALLOC
674800040122     C* REIMPOSTO I CAMPI
674900170116    4C***  *IN91         IFEQ      *ON
675000170116     C***                MOVEL     SOARBT        SOVR(1)
675100170116     C***                MOVEL     SAARBT        SALC(1)
675200170116     C***                MOVEL     SDARBT        SDLC(1)
675300170116     C**   §7LFFI        ifeq      'S'
675400170116     C***                EXSR      DLCSED
675500170116    5C***                ENDIF
675600040122     C**
675700170116     C****               GOTO      ENDAGG
675800170116    4C****               ENDIF
675900170116    3C****               ENDIF
676000040122    2C                   ENDIF
676100040122     C*
676200040122     C                   MOVEL     '2'           AR6TRC
676300040122     C* SE DA STORICIZZARE ALL'ARRIVO  MEMORIZZO
676400040122     C     §7LFSA        IFEQ      'S'
676500040122     C                   EXSR      STOARB
676600040122     C                   ENDIF
676700060317     c* storicizzazione motivazione
676800060317     c     §7lmtv        ifeq      'S'
676900060317     c                   exsr      sto_mtv
677000060317     c                   endif
677100040122
677200040122     C                   MOVEL     '2'           ARBTRC
677300040122     C**
677400040122     C* INVIO VARIAZIONE --> FIARBU0T
677500040122     C**
677600040122    2C     *IN09         IFEQ      *ON
677700040122     C     C             ANDGT     3
677800040122     C                   Z-ADD     4             C
677900040122     C*
678000040122    3C     §7LFFI        IFEQ      'S'
678100040122     C                   Z-ADD     4             C
678200170116     C****               OPEN      FIARBU0T
678300040122     C*
678400040122     C* NE SCRIVO FIN TANTO CHE CE NE SONO
678500040122    4C     WSV(C)        DOWNE     ' '
678600040122     C                   MOVEL     WSV(C)        ARBSVN
678700040122     C                   Z-ADD     WVA(C)        ARBVAN
678800040122     C                   WRITE     FIARBUTT
678900040122     C                   ADD       1             C
679000040122    4C                   ENDDO
679100040122     C*
679200170116     C****               CLOSE     FIARBU0T
679300040122     C* DISALLOCO OGGETTO MEMBRO PER SEDE
679400170116     C*****              EXSR      DLCSED                                       FIARBU0T
679500040122    3C                   ENDIF
679600040122     C* REIMPOSTO I CAMPI
679700170116     C***                MOVEL     SOARBT        SOVR(1)
679800170116     C***                MOVEL     SAARBT        SALC(1)
679900170116     C***                MOVEL     SDARBT        SDLC(1)
680000040122    2C                   ENDIF
680100040122
680200040122     C                   MOVEL     'S'           ARBACA
680300040122     C                   MOVEL     VIDKLP        ARBKSC
680400040122     C                   MOVE      VIDKSC        ARBKSC
680500040122    2C     VIDKSC        IFNE      9999
680600040122     C                   MOVEL     VIDCTR        ARBCTR
680700040122     C                   ELSE
680800040122     C                   CLEAR                   ARBCTR
680900040122    2C                   ENDIF
681000040122     C                   CLEAR                   ARBPOR
681100040122     C                   Z-ADD     0             ARBALI
681200040122     C                   Z-ADD     0             ARBIFT
681300040122     C                   MOVEL     VI2DIV        ARBDIV
681400040122     C                   MOVEL     WSV(1)        ARBSV1
681500040122     C                   Z-ADD     WVA(1)        ARBVA1
681600040122     C                   MOVEL     WSV(2)        ARBSV2
681700040122     C                   Z-ADD     WVA(2)        ARBVA2
681800040122     C                   MOVEL     WSV(3)        ARBSV3
681900040122     C                   Z-ADD     WVA(3)        ARBVA3
682000040122
682100040122     C                   Z-ADD     TotImv        ARBIMV
682200040122     C                   MOVEL     VI2CEI        ARBCEI
682300040122     C                   MOVEL     AR6FIM        ARBFIM
682400040122
682500040122     C* S E G U E   F A T T U R A
682600040122    2C     *IN01         IFEQ      *ON
682700040122     C                   Z-ADD     0             ARBNFT
682800040122     C                   Z-ADD     0             ARBFIV
682900040122     C                   Z-ADD     0             ARBDFT
683000040122     C* SE SI TRATTA DI UN RITORNO ALL'INCASSO --> CHIUDO L'INCASSO
683100040122     C*   E LA BOLLA
683200040122    3C     ARBDCM        IFGT      0
683300040122     C     ARBICA        ANDEQ     'R'
683400040122     C*
683500040122     C* SE IL C/A E' INCASSATO E MANCA IL NUMERO MANDATO INTROITO
683600040122     C*  LO IMPOSTO
683700040122     C                   MOVEL     'S'           WAGG              1
683800040122     C*
683900040122     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
684000040122     C*  UN NUMERO DISTINTA ED E' CHIUSA
684100040122    4C     ARBNDC        IFGT      0
684200040122     c                   Z-Add     ArbIfp        Kfgs
684300040122     C                   Z-ADD     ARBNDC        W0050             5 0
684400040122     C     KFVV          CHAIN     FNFVV01L                           39
684500040122     C     *IN39         IFEQ      *OFF
684600040122     C     FVVFCF        ANDEQ     'S'
684700040122     C                   CLEAR                   WAGG
684800040122     C                   ENDIF
684900040122    4C                   ENDIF
685000040122     C*
685100040122    4C     WAGG          IFEQ      'S'
685200040122     C                   MOVEL     'Y'           ARBICA
685300040122     C*
685400040122    5C     ARBNDC        IFEQ      0
685500040122     C                   Z-ADD     8888888       ARBNDC
685600040122     C                   Z-ADD     ARBDCM        ARBDDC
685700080516    5C     ARBNMI        IFEQ      0
685800080516     C                   Z-ADD     8888888       ARBNMI
685900080516    5C                   ENDIF
686000080516     c                   else
686100080516     c* Se c'e' numero distinta aperto ma non ci sono incassi di C/AAA
686200080516     c*  imposto 8888888 per non creare problemi in chiusura distinta
686300080516     c                   if        arbicc<>'R'  and arbicc<>' '
686400080516     C                   Z-ADD     8888888       ARBNDC
686500080516    5C     ARBNMI        IFEQ      0
686600080516     C                   Z-ADD     8888888       ARBNMI
686700080516    5C                   ENDIF
686800080516     c                   endif
686900080516     c
687000040122    5C                   ENDIF
687100040122   X4C                   ELSE
687200040122     C                   MOVEL     'S'           ARBICA
687300040122    4C                   ENDIF
687400040122     C*
687500040122    3C                   ENDIF
687600040122
687700040122     C* F A T T U R A   I M M E D I A T A  --> TOTALE FATTURA
687800040122   x2C                   ELSE
687900040122     C* DATA FATTURA
688000040122     C                   MOVEL     FIV           ARBFIV
688100040122      * se c'è già la data fattura devo passarla
688200040122      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
688300040122      *  di nuovo una fattura immediata)
688400040122     c                   if        ar6dft <> 0
688500040122     c                   eval      arbdft = ar6dft
688600040122     c                   else
688700040122     C                   MOVEL     DATEU         ARBDFT                          DATA FATTUR
688800040122     c                   endif
688900040122     C                   Z-ADD     WNFT          ARBNFT
689000040122     C                   Z-ADD     D04ALI        ARBALI
689100040122     C                   Z-ADD     D04IFT        ARBIFT
689200040122     C                   Z-ADD     D04IMV        ARBIMV
689300040122    3C                   SELECT
689400040122     C     ARBVA1        WHENGT    0
689500040122     C     ARBSV1        ANDNE     §$2BOL
689600040122     C     ARBSV1        ANDNE     §$2IVA
689700040122     C                   ADD       D04POR        ARBVA1
689800040122     C     ARBVA2        WHENGT    0
689900040122     C     ARBSV2        ANDNE     §$2BOL
690000040122     C     ARBSV2        ANDNE     §$2IVA
690100040122     C                   ADD       D04POR        ARBVA2
690200040122     C     ARBVA3        WHENGT    0
690300040122     C                   ADD       D04POR        ARBVA3
690400040122    3C                   ENDSL
690500040122    2C                   ENDIF
690600040122     C*
690700050114     C     §7LFFI        ifeq      'S'
690800170116     C****               OPEN      FIARBT0T
690900040122     C                   WRITE     FIARBTTT
691000170116     C****               CLOSE     FIARBT0T
691100040122     C* DISALLOCO OGGETTO
691200170116     C****               EXSR      DLCSED                                       FIARBT0T
691300040122    2C                   END
691400050114     C* A G G I O R N O
691500990924     C                   Z-ADD     ARBKSC        AR6KSC
691600990924     C                   Z-ADD     ARBCTR        AR6CTR
691700990924     C                   MOVEL     ARBDIV        AR6DIV
691800990924     C                   Z-ADD     ARBPOR        AR6POR
691900990924     C                   MOVEL     ARBSV1        AR6SV1
692000990924     C                   MOVEL     ARBSV2        AR6SV2
692100990924     C                   MOVEL     ARBSV3        AR6SV3
692200990924     C                   Z-ADD     ARBVA1        AR6VA1
692300990924     C                   Z-ADD     ARBVA2        AR6VA2
692400990924     C                   Z-ADD     ARBVA3        AR6VA3
692500990924     C                   Z-ADD     ARBIMV        AR6IMV
692600990924     C                   Z-ADD     ARBALI        AR6ALI
692700990924     C                   MOVEL     ARBCEI        AR6CEI
692800990924     C                   Z-ADD     ARBIFT        AR6IFT
692900990924     C                   Z-ADD     ARBFIV        AR6FIV
693000990924     C                   Z-ADD     ARBNFT        AR6NFT
693100990924     C                   Z-ADD     ARBDFT        AR6DFT
693200050314     c*
693300050314     c* update o write di ar6/ar7  : imposto campi di comodo
693400050314     c                   eval      wesistear6=war61
693500050314     c                   eval      wesistear7=war71
693600050314     c                   eval      wtiporecar6='2'
693700050314     c                   exsr      SCRIVIar6ar7
693800950403     C*
693900990930     C* PRIMA DI AGGIORNARE FNARB RISPOSTO I CAMPI SALVATI NEI CAMPI
694000990930     C* DEL FILE
694100990930     C                   Z-ADD     SAVQFT        ARBQFT
694200990930     C                   Z-ADD     SAVVMD        ARBVMD
694300990930     C                   MOVEL     SAVVAD        ARBVAD
694400990930     C                   Z-ADD     SAVIAS        ARBIAS
694500990930     C                   MOVEL     SAVVAS        ARBVAS
694600990930     C                   Z-ADD     KSCSAV        ARBKSC
694700990930     C                   Z-ADD     CTRSAV        ARBCTR
694800990930     C                   UPDATE    FNARB000
694900131119     c***                if        ar6nft>0
695000131119     c***                exsr      sr_pda
695100131119     c***                endif
695200990428    1C                   END
695300920409     C**
695400920409     C** A S S E G N A T I   1   B O L L A
695500940223    1C  N05              DO
695600941217     C*
695700941217     C* INVIO PER SEDE
695800170116     C****               MOVEL     'FIARBT0T'    SFILE
695900941217     C* ALLOCO OGGETTI
696000170116     C****               EXSR      ALLOC
696100170116     C** 91              GOTO      ENDAGG
696200990108     C**
696300990302     C** FATTURA IMMEDIATA
696400990108     C* VEDO SE RIASSEGNARE NUMERO FATTURA
696500990108    2C     *IN01         IFEQ      *OFF
696600990108     C                   SETON                                        32
696700990302    3C   12AR6NFT        IFNE      0
696800990108     C                   SETOFF                                       32
696900990302     C                   Z-ADD     AR6NFT        WNFT
697000990302     C                   Z-ADD     AR6FIV        FIV
697100990108    3C                   END
697200990108     C*
697300990108    3C   32              DO
697400990108     C* LIBRO IVA
697500020725     C                   clear                   DSBS02
697600020725     C                   clear                   DYIV
697700020725     C                   movel     'C'           T02mod
697800020725     C                   movel     KNSIF         T02sif
697900020725     C                   movel     'YIV'         T02cod
698000020725     C                   movel     C_Soc         T02ke1
698100020725     C                   movel     ARBLNA        T02ke2
698200020725     C                   call      'TIBS02R'
698300020725     C                   parm                    KPJBA
698400020725     C                   parm                    DSBS02
698500020725    4C                   IF        T02err = *blanks
698600020725     C                   movel     T02uni        DYIV
698700990302     C                   MOVEL     §IVLVF        FIV
698800990108     C                   ELSE
698900040121     C                   MOVEL     ARBLNA        FIV
699000990108    4C                   END
699100990108     C* NUMERO FATTURA
699200990108     C     KNUM          CHAIN     CNNUM000                           32
699300990108     C* ERRORE : MANCA IL NUMERATORE DELLE FATTURE
699400070222    4C     *IN32         IFEQ      *ON
699500990108     C                   MOVEL     MSG(41)       VIDMSG
699600170116    5C***  §7LFFI        ifeq      'S'
699700170116     C***                EXSR      DLCSED
699800170116    5C***                ENDIF
699900990108     C*
700000990108     C                   SETON                                        9128
700100990108     C                   GOTO      ENDAGG
700200070222    4C                   ENDIF
700300990108     C*
700400990114     C     *LIKE         DEFINE    ARBNFV        WNFT
700500990108     C                   ADD       1             NUMNUM
700600990114     C                   Z-ADD     NUMNUM        WNFT
700700990108     C                   MOVE      DATEU         NUMDTP
700800990108     C                   UPDATE    CNNUM000
700900990108    3C                   END
701000990108    C**
701100990108     C* SE ERA S.F. E DIVENTA FATTURA IMMEDITATA --> SE GIA' CONSEGNATA
701200990108     C*  DIVENTA RITORNO ALL'INCASSO
701300990108    3C   12ARBDCM        IFGT      0
701400990108     C                   MOVEL     'R'           ARBICA
701500990108    3C                   END
701600990108    2C                   END
701700941217     C*
701800941217     C* CARICO LE VARIE IMPOSTATE IN UNA SCHIERA DI COMODO
701900991013     C                   MOVEL     *BLANKS       WCKINL            1
702000991013     C                   EXSR      CARWAR
702100990924     C* SE FATTURA IMMEDIATA CALCOLO IL TOTALE FATTURA:
702200990924     C* LO FACCIO ORA PERCHE' BOLLO E IVA SONO DIVENTATE VARIE
702300990924     C* E LE DEVO QUINDI CARICARE NELLA SCHIERA DI COMODO
702400990924    2C     *IN01         IFEQ      *OFF
702500990924     C                   MOVEL     VIDDIV        ARBDIV
702600990924     C                   EXSR      RIEGEI
702700990924     C                   CLEAR                   DSLV04
702800990924     C                   MOVEL     'T'           D04FIM
702900990924     C                   Z-ADD     DATEU         D04DFT
703000131001      * se c'è già la data fattura devo passarla
703100131001      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
703200131001      *  di nuovo una fattura immediata)
703300131001     c                   if        ar6dft <> 0
703400131001     c                   eval      d04dft = ar6dft
703500131001     c                   endif
703600990924     C                   MOVEL     §GEAED        D04AED
703700990924     C                   Z-ADD     §GEAVL        D04VAR
703800990924     C                   Z-ADD     VIDIMV        D04IMV
703900990924     C                   MOVEL     VIDCEI        D04CEI
704000990924     C                   MOVEL     AR9TIC        D04TIC
704100990924     C                   MOVEL     AR9VCA        D04VCA
704200990924     C                   Z-ADD     AR9CAS        D04CAS
704300990924     C                   MOVEL     '1'           D04SPE
704400990924     C                   MOVEL     §3ATB1        D04TBL
704500990924     C                   MOVEL     VIDDIV        D04DIV
704600990924     C                   Z-ADD     VIDPOR        D04POR
704700990924     C                   MOVEL     '1'           UNO               1
704800990924     C*
704900990924     C                   CALL      'FNLV04R'
705000990924     C                   PARM                    DSLV04
705100990924     C                   PARM                    DTASV
705200990924     C*
705300990924     C     D04BOL        IFGT      *ZEROS
705400990924     C                   ADD       1             C
705500990924     C                   MOVEL     §$2BOL        WSV(C)
705600990924     C                   Z-ADD     D04BOL        WVA(C)
705700991001     C                   ENDIF
705800990924     C     D04IVA        IFGT      *ZEROS
705900990924     C                   ADD       1             C
706000990924     C                   MOVEL     §$2IVA        WSV(C)
706100990924     C                   Z-ADD     D04IVA        WVA(C)
706200990924     C                   ENDIF
706300990924    2C                   ENDIF
706400941217     C*
706500941217     C* SE NE HO TROVATA ALMENO UNA ACCENDO INDICATORE 09
706600990108    2C     C             IFGT      0
706700941217     C                   SETON                                        09
706800941217     C*
706900941217     C* INVIO PER SEDE
707000941217     C* SOLO SE HO DELLE VARIE E NE HO PIU' DI 3
707100170116    3C**   C             IFGT      3
707200990913     C* MI SALVO I CAMPI DELL'FIARBT
707300170116     C***                MOVEL     SOVR(1)       SOARBT
707400170116     C***                MOVEL     SALC(1)       SAARBT
707500170116     C***                MOVEL     SDLC(1)       SDARBT
707600941217     C*
707700170116     C***                MOVEL     'FIARBU0T'    SFILE
707800941217     C* ALLOCO OGGETTI
707900170116     C***                EXSR      ALLOC
708000990108     C* REIMPOSTO I CAMPI
708100170116    4C**   *IN91         IFEQ      *ON
708200170116     C**                 MOVEL     SOARBT        SOVR(1)
708300170116     C**                 MOVEL     SAARBT        SALC(1)
708400170116     C**                 MOVEL     SDARBT        SDLC(1)
708500170116     C**   §7LFFI        ifeq      'S'
708600170116     C**                 EXSR      DLCSED
708700170116    5C**                 ENDIF
708800990108     C**
708900170116     C**                 GOTO      ENDAGG
709000170116    4C**                 ENDIF
709100170116    3C**                 ENDIF
709200990108    2C                   ENDIF
709300941217     C*
709400990924     C                   MOVEL     '1'           AR6TRC
709500941217     C* SE DA STORICIZZARE ALL'ARRIVO  MEMORIZZO
709600990108    2C     §7LFSA        IFEQ      'S'
709700941217     C                   EXSR      STOARB
709800990108    2C                   ENDIF
709900060317     c*
710000060317     c* storicizzazione motivazione
710100060317     c     §7lmtv        ifeq      'S'
710200060317     c                   exsr      sto_mtv
710300060317     c                   endif
710400060317     c*
710500990924     C                   MOVEL     '1'           ARBTRC
710600990924     C*
710700990108     C**
710800990913     C* INVIO VARIAZIONE --> FIARBU0T
710900941217     C**
711000990108    2C     *IN09         IFEQ      *ON
711100941217     C     C             ANDGT     3
711200941217     C                   Z-ADD     4             C
711300941217     C*
711400990108    3C     §7LFFI        IFEQ      'S'
711500941217     C                   Z-ADD     4             C
711600170116     C***                OPEN      FIARBU0T
711700941217     C*
711800941217     C* NE SCRIVO FIN TANTO CHE CE NE SONO
711900990108    4C     WSV(C)        DOWNE     ' '
712000941217     C                   MOVEL     WSV(C)        ARBSVN
712100941217     C                   Z-ADD     WVA(C)        ARBVAN
712200990913     C                   WRITE     FIARBUTT
712300941217     C                   ADD       1             C
712400990108    4C                   ENDDO
712500941217     C*
712600170116     C***                CLOSE     FIARBU0T
712700941217     C* DISALLOCO OGGETTO MEMBRO PER SEDE
712800170116     C***                EXSR      DLCSED                                       FIARBU0T
712900990108    3C                   ENDIF
713000941217     C* REIMPOSTO I CAMPI
713100170116     C***                MOVEL     SOARBT        SOVR(1)
713200170116     C***                MOVEL     SAARBT        SALC(1)
713300170116     C***                MOVEL     SDARBT        SDLC(1)
713400990108    2C                   ENDIF
713500941217     C*
713600941217     C                   MOVEL     'S'           ARBACA
713700941217     C                   Z-ADD     VIDKSC        ARBKSC
713800941217     C                   MOVEL     VIDKLP        ARBKSC
713900011113     C                   Z-ADD     VIDIAS        ARBIAS
714000991025     C                   MOVEL     VIDVAS        ARBVAS
714100011015      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
714200011015     C     VIDIAS        IFEQ      *ZEROS
714300011015     C                   CLEAR                   ARBVAS
714400011015     C                   ENDIF
714500011015      *
714600941217     C                   MOVEL     VIDQFT        ARBQFT
714700941217     C     VIDKSC        IFNE      9999
714800941217     C                   MOVEL     VIDCTR        ARBCTR
714900941217     C                   ELSE
715000941217     C                   CLEAR                   ARBCTR
715100941217     C                   ENDIF
715200020930     c* se ho tassatocon il peso vld imposto iL flaG tasfpf
715300990913     C                   MOVEL     VIDDIV        ARBDIV
715400990924     C                   Z-ADD     VIDPOR        ARBPOR
715500941217     C                   MOVEL     WSV(1)        ARBSV1
715600990924     C                   Z-ADD     WVA(1)        ARBVA1
715700941217     C                   MOVEL     WSV(2)        ARBSV2
715800990924     C                   Z-ADD     WVA(2)        ARBVA2
715900941217     C                   MOVEL     WSV(3)        ARBSV3
716000990924     C                   Z-ADD     WVA(3)        ARBVA3
716100941217     C*
716200070924     c* Memorizzo il totale imponibile per la scrittura del fiar5
716300070924     c                   z-add     vidimv        Tassatoimv
716400990924     C                   Z-ADD     VIDIMV        ARBIMV
716500941217     C                   MOVEL     VIDCEI        ARBCEI
716600991014     C                   MOVEL     AR6FIM        ARBFIM
716700941217     C                   CLEAR                   ARBALI
716800941217     C                   CLEAR                   ARBIFT
716900941217     C*
717000941217     C* S E G U E   F A T T U R A
717100941217    2C     *IN01         IFEQ      *ON
717200941217     C                   Z-ADD     0             ARBFIV
717300941217     C                   Z-ADD     0             ARBDFT
717400941217     C                   Z-ADD     0             ARBNFT
717500941217     C*
717600941217     C* GLI A3 S.F. LI CONSIDERO SUBITO COME CONSEGNATI
717700941217     C*
717800941217    3C     §3ATB1        IFEQ      'A3'
717900941217     C* IMPOSTO I CAMPI DELLA CHIUSURA
718000941217     C                   MOVEL     'Y'           ARBICC
718100960320     C                   MOVEL     'S'           ARBICA
718200941217     C                   Z-ADD     DATEU         ARBDCM
718300941217     C                   MOVEL     ARBORV        ARBHMC
718400941217     C                   CLEAR                   ARBFT1
718500941217     C                   CLEAR                   ARBDT1
718600941217     C                   MOVEL     *ALL'9'       ARBNDC
718700941217     C                   Z-ADD     DATEU         ARBDDC
718800941217     C                   MOVEL     'S'           ARBFDC
718900941217     C                   MOVEL     *ALL'9'       ARBNMI
719000941217     C                   Z-ADD     DATEU         ARBDMI
719100941217     C                   MOVEL     *ALL'9'       ARBCUC
719200941217     C                   CLEAR                   ARBFBC
719300941217    3C                   ENDIF
719400950120     C*
719500950120     C* SE SI TRATTA DI UN RITORNO ALL'INCASSO --> CHIUDO L'INCASSO
719600950120     C*   E LA BOLLA
719700990428    3C   12ARBDCM        IFGT      0
719800960704     C* SE IL C/A E' INCASSATO E MANCA IL NUMERO MANDATO INTROITO
719900960704     C*  LO IMPOSTO
720000960909     C                   MOVEL     'S'           WAGG              1
720100970528     C* NON AGGIORNO COL MANDATO DI INTROITO SE SULLA BOLLA C'E'
720200970528     C*  UN NUMERO DISTINTA ED E' CHIUSA
720300990428    4C     ARBNDC        IFGT      0
720400020911     c                   Z-Add     ArbIfp        Kfgs
720500970528     C                   Z-ADD     ARBNDC        W0050             5 0
720600020911     C     KFVV          CHAIN     FNFVV01L                           39
720700990428    5C     *IN39         IFEQ      *OFF
720800970528     C     FVVFCF        ANDEQ     'S'
720900970528     C                   CLEAR                   WAGG
721000990428    5C                   ENDIF
721100990428    4C                   ENDIF
721200960909     C*
721300990428    4C     WAGG          IFEQ      'S'
721400971023     C                   MOVEL     'Y'           ARBICA
721500990428     C     ARBNDC        IFEQ      0
721600960704     C                   Z-ADD     8888888       ARBNDC
721700080516     C     ARBNMI        IFEQ      0
721800080516     C                   Z-ADD     8888888       ARBNMI
721900080516     C                   ENDIF
722000080516     c                   else
722100080516     c* Se c'e' numero distinta aperto ma non ci sono incassi di C/Ass
722200080516     c*  imposto 8888888 per non creare problemi in chiusura distinta
722300080516     c                   if        arbicc<>'R'  and arbicc<>' '
722400080516     C                   Z-ADD     8888888       ARBNDC
722500080516     C     ARBNMI        IFEQ      0
722600080516     C                   Z-ADD     8888888       ARBNMI
722700080516     C                   ENDIF
722800080516     c                   endif
722900990428     C                   ENDIF
723000080516     c
723100990428     C     ARBDDC        IFEQ      0
723200960704     C                   Z-ADD     ARBDCM        ARBDDC
723300990428     C                   ENDIF
723400990428   X4C                   ELSE
723500971023     C* SE DISTINTA CHIUSA MA SENZA MANDATO DI INTROITO METTO S
723600971023     C                   MOVEL     'S'           ARBICA
723700990428    4C                   ENDIF
723800990428    3C                   ENDIF
723900941217     C*
724000990428   X2C                   ELSE
724100941217     C* F A T T U R A   I M M E D I A T A  --> TOTALE FATTURA
724200990924     C* DATA FATTURA
724300990924     C                   MOVEL     FIV           ARBFIV
724400011204      * se c'è già la data fattura devo passarla
724500011204      * (caso di modifica da fattura immediata a segue fattura e invece viene fatta
724600011204      *  di nuovo una fattura immediata)
724700011204     c                   if        ar6dft <> 0
724800011204     c                   eval      arbdft = ar6dft
724900011204     c                   else
725000031118      * se la data del giorno è il 31/12/2003 devo impostare 30/12/2003
725100040116     c**!!!              If        dateu = 20031231
725200040116     c**!!!              Movel     20031230      Arbdft
725300040116     c**!!!              Else
725400990924     C                   MOVEL     DATEU         ARBDFT                          DATA FATTUR
725500040116     c**!!!              EndIf
725600011204     c                   endif
725700990924     C                   Z-ADD     WNFT          ARBNFT
725800990924     C                   Z-ADD     D04ALI        ARBALI
725900990924     C                   Z-ADD     D04IFT        ARBIFT
726000990924     C                   Z-ADD     D04IMV        ARBIMV
726100990924     C*
726200990924    3C                   SELECT
726300990924     C     ARBPOR        WHENGT    0
726400990924     C                   Z-ADD     D04POR        ARBPOR
726500990924     C     ARBVA1        WHENGT    0
726600990924     C     ARBSV1        ANDNE     §$2BOL
726700990924     C     ARBSV1        ANDNE     §$2IVA
726800990924     C                   ADD       D04POR        ARBVA1
726900990924     C     ARBVA2        WHENGT    0
727000990924     C     ARBSV2        ANDNE     §$2BOL
727100990924     C     ARBSV2        ANDNE     §$2IVA
727200990924     C                   ADD       D04POR        ARBVA2
727300990924     C     ARBVA3        WHENGT    0
727400990924     C                   ADD       D04POR        ARBVA3
727500990924    3C                   ENDSL
727600990108    2C                   ENDIF
727700941217     C*
727800050114      * Aggiorno direttamente il BLP ma solo se sono stati modificati
727900040513      * l'imp.da assicurare o la qtà da fatturare
728000040513  2b c                   if        sv1_vidias <> vidias or sv1_vidvas <> vidvas
728100040513     c                             or
728200040513     c                             sv1_vidqft <> vidqft
728300040513     c                   exsr      Agg_BLP
728400040513  2e c                   endif
728500050114     C* Invio variazione a SEDE
728600990428    2C     §7LFFI        IFEQ      'S'
728700170116     C***                OPEN      FIARBT0T
728800990913     C                   WRITE     FIARBTTT
728900170116     C***                CLOSE     FIARBT0T
729000941217     C* DISALLOCO OGGETTO MEMBRO PER SEDE
729100170116     C***                EXSR      DLCSED
729200990428    2C                   END
729300941217     C**
729400941217     C* A G G I O R N O
729500941217     C**
729600990924     C                   Z-ADD     ARBKSC        AR6KSC
729700990924     C                   Z-ADD     ARBCTR        AR6CTR
729800941217     C                   MOVE      ARBALI        AR6ALI
729900941217     C                   MOVE      ARBIFT        AR6IFT
730000941217     C                   MOVE      ARBIMV        AR6IMV
730100990913     C                   MOVEL     ARBDIV        AR6DIV
730200941217     C                   MOVE      ARBPOR        AR6POR
730300941217     C                   MOVE      ARBSV1        AR6SV1
730400941217     C                   MOVE      ARBVA1        AR6VA1
730500941217     C                   MOVE      ARBSV2        AR6SV2
730600941217     C                   MOVE      ARBVA2        AR6VA2
730700941217     C                   MOVE      ARBSV3        AR6SV3
730800941217     C                   MOVE      ARBVA3        AR6VA3
730900941217     C                   MOVEL     ARBCEI        AR6CEI
731000941217     C                   MOVEL     ARBDFT        AR6DFT
731100941221     C                   MOVEL     ARBNFT        AR6NFT
731200941217     C                   MOVEL     ARBFIV        AR6FIV
731300050314     c* update o write di ar6/ar7  : imposto campi di comodo
731400050314     c                   eval      wesistear6=war6
731500050314     c                   eval      wesistear7=war7
731600050314     c                   eval      wtiporecar6='1'
731700050314     c                   exsr      SCRIVIar6ar7
731800070924     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
731900070924     c*  per la fatturazione se inserito o variato totale imponibile
732000070925     c                   if        (ar6imv>0 and ar6imv<>savimv) or
732100070925     c                             ar6imv=0
732200151118     c                   exsr      Regpesivol
732300070924     c                   endif
732400941217     C*
732500941217     C                   UPDATE    FNARB000
732600131119     c***                if        ar6nft>0
732700131119     c***                exsr      sr_pda
732800131119     c***                endif
732900940224    1C                   ENDDO
733000941217     C**
733100941217     C* AGGIORNO LE NOTE
733200941217     C**
733300090507     c                   if        wscelta<>'9'
733400941217     C                   EXSR      NOTE
733500090507     c                   endif
733600911029     C*
733700120508     c
733800120508     c                   clear                   wcdfis
733900120508     c                   clear                   wpariva
734000120508     c* Se fattura immediata reperisco codice fiscale se cli italia
734100120508    1c                   if        not *in01  and arbnzd=*blanks
734200120508     c                   move      ar6ksc        w0040
734300120508     c* Non Codificato --> cerco col pgm YCO601R
734400120508    2c                   if        w0040=9999
734500120508     c                   clear                   yco600ds
734600120508     c
734700120508     c                   eval      i600ela='R'
734800120508     c                   eval      i600aCF='SI'
734900120508     c                   eval      i600aPI='SI'
735000120508     c     *iso          movel     20000101      i600dti
735100120508     c     *iso          movel     29991231      i600dtf
735200120508     c                   eval      i600rsc=arbrsd
735300120508     c                   eval      i600ind=arbind
735400120508     c                   eval      i600cap=arbcad
735500120508     c                   eval      i600loc=arblod
735600120508     c                   eval      i600prv=arbprd
735700120508     c                   movel     yco600ds      kpjbu
735800120508     c                   call      'YCO601R'
735900120508     c                   parm                    kpjba
736000120508     c
736100120508     c                   movel     kpjbu         yco600ds
736200120508    3c                   if        o600err=*blanks
736300120508     c                   movel     o600cdfi      wcdfis
736400120508     c* non prendo la partita iva se inizia per $$
736500120508     c                   if        %subst(o600piva:1:2)<>'$$'
736600120508     c                   movel     o600piva      wpariva
736700120508    3c                   endif
736800120508    3c                   endif
736900120508     c
737000120508   x2c                   else
737100120508     c* Se codificato reperisco dall'anagrafica se c'e'
737200120508     c                   eval      comksc=ar6ksc
737300120508     c                   exsr      SR_Acoksc
737400120508    3c                   if        indcdf<>*blanks
737500120508     c                   movel     indcdf        wcdfis
737600120508     c                   endif
737700120508     c                   if        indiva<>*blanks and %subst(indiva:1:2)<>'$$'
737800120508     c                   movel     indiva        wpariva
737900120508    3c                   endif
738000120508     c
738100120508    3c                   endif
738200120508    2c                   endif
738300120508     c
738400120508     c* Chaino record Q del codice fiscale
738500120508    2c                   if        wcdfis<>*blanks
738600120508     c* chain sul record per vedere se c'e' già CDFISC del mittente
738700120508     C                   MOVEL     'Q'           WTRC
738800120508     C     KARB4         CHAIN(N)  Fiar401L                           32
738900120508     c
739000120508     c* richiamo fnlv19r per scrittura
739100120508     c                   clear                   dslv19
739200120508     c                   movel     'E'           d19agg
739300120508     C                   MOVEL     'T'           D19ftr
739400120508     C                   MOVEL     ARBAAS        D19AAS
739500120508     C                   MOVEL     ARBLNP        D19LNP
739600120508     C                   MOVEL     ARBNRS        D19NRS
739700120508     C                   MOVEL     ARBNSP        D19NSP
739800120508     C  n32              MOVEL     ar4not        D19NT1
739900120508     c                   move      wcdfis        d19nt1
740000120508     C*
740100120508     C                   MOVEL     'Q'           D19TRC
740200120508     C                   CALL      'FNLV19R'
740300120508     C                   PARM                    DSLV19
740400120508    2c                   endif
740500120508     c
740600120508     c* Chaino record P e fnarb per la partita iva
740700120508    2c                   if        wpariva<>*blanks
740800120508     C
740900120508     c* Se non ho trovato il cod fiscale verifico se c'e' già in bolla
741000120508     c                   if        wcdfis=*blANKS
741100120508     C                   MOVEL     'Q'           WTRC
741200120508     C     KARB4         CHAIN(N)  Fiar401L                           32
741300120508     c  n32              move      ar4not        wcdfis
741400120508     C                   ENDIF
741500120508     c
741600120508     c* Preparo la trasmissione a sede chiamando in modo nascosto
741700120508     c*  FNLR48R --> non lo faccio se ritassazione perchè diventa chiamata ricorsiva
741800120508     c                   if        not *in12
741900120508     C                   CLEAR                   DSLR48
742000120508     C                   CLEAR                   DSarbd
742100120508     C                   CLEAR                   DSarbk
742200120508     C                   CLEAR                   DarbT
742300120508     C                   CLEAR                   DSarbG
742400120508     c* Imposto codice fiscale e partita iva
742500120508     c                   movel     wcdfis        §bdcpi
742600120508     c                   movel     wpariva       §bdrsd
742700120508     c
742800120508     C                   MOVEL     VsfAAS        D48AAS
742900120508     C                   MOVEL     VsfLNP        D48LNP
743000120508     C                   MOVEL     VsfNRS        D48NRS
743100120508     C                   MOVEL     VsfNSP        D48NSP
743200120508     C                   MOVEL     VidFIL        D48FGS
743300120508     C                   MOVEL     ARBCBO        D48CBO
743400120508     C                   MOVEL     'FI'          D48CVB
743500120508     C                   MOVEL     'A'           D48TBO
743600120508     C                   MOVEL     'S'           D48F12
743700120508     C                   MOVEL     'E'           D48FFR
743800120508     C                   MOVEL     'D'           D48trc
743900120508     C                   MOVEL     DSLR48        KPJBU
744000120508     C                   CALL      'FNLR48R'
744100120508     C                   PARM                    KPJBA
744200120508     C                   PARM                    DSARBD
744300120508     C                   PARM                    DSARBK
744400120508     C                   PARM                    DARBT
744500120508     C                   PARM                    DSARBG
744600120508     C                   PARM                    TRUL90DS
744700120508     c                   endif
744800120508     C*
744900120508     c
745000120508     c* chain sul record per vedere se c'e' già CDFISC del mittente
745100120508     C                   MOVEL     'P'           WTRC
745200120508     C     KARB4         CHAIN     Fiar401L                           32
745300120508    3c                   if        %found(fiar401l)
745400120508     c
745500120508     c* richiamo fnlv19r per scrittura
745600120508     c                   clear                   dslv19
745700120508     c* solo filiale, in sede non  c'e' il record
745800120508     c                   movel     ' '           d19agg
745900120508     C                   MOVEL     'T'           D19ftr
746000120508     C                   MOVEL     ARBAAS        D19AAS
746100120508     C                   MOVEL     ARBLNP        D19LNP
746200120508     C                   MOVEL     ARBNRS        D19NRS
746300120508     C                   MOVEL     ARBNSP        D19NSP
746400120508     C  n32              MOVEL     ar4not        D19NT1
746500120508     c                   move      wpariva       d19nt1
746600120508     C*
746700120508     C                   MOVEL     'P'           D19TRC
746800120508     C                   CALL      'FNLV19R'
746900120508     C                   PARM                    DSLV19
747000120508    3c                   endif
747100120508     c
747200120508     c     karb2         chain     fnarb01l
747300120508     c                   if        %found(fnarb01l)
747400120508     c                   movel     wpariva       arbcpi
747500120508     c                   update    fnarb000
747600120508    2c                   endif
747700120508     c
747800120508    1c                   endif
747900940223     C*
748000990913     C     AR6NFT        IFNE      0
748100000322     C*
748200120127      * stampo se F8
748300120127i2  2c                   if        *InKh
748400130320     C****               CLEAR                   fnlsb5DS
748500941217     C*
748600941217     C* CHIAMO PGM PER STAMPA FATTURA ASSEGNATO
748700000322     C* (AD ESCLUSIONE DEI "DDT NO" NON ANCORA STAMPATI)
748800130320     C** 12              MOVEL     'V'           DB0RIS
748900130320     C**N12              MOVEL     ' '           DB0RIS
749000130320     C***                MOVEL     'A'           DB0TBO
749100130320     C***                MOVEL     ARBAAS        DB0AAS
749200130320     C***                MOVEL     ARBLNP        DB0LNP
749300130320     C***                MOVEL     ARBNRS        DB0NRS
749400130320     C***                MOVEL     ARBNSP        DB0NSP
749500130320     C****               CALL      WD90PSL
749600130320     C****               PARM                    fnlsb5DS
749700120127      * flaggo FiAR400F come stampato
749800130320     c**                 Clear                   dsBl4a
749900130320     c**                 Eval      Wtrc = 'A'
750000130320     c**   kArb4         Chain     Fiar401l
750100130320     c**                 If        %Found(Fiar401l)
750200130320     c**                 Movel     Ar4Not        dsBl4a
750300130320     c**                 Select
750400130320     c**                 When      §b4Abm = 'N' and §b4Abm = 'Y'
750500130320     c**                 Eval      §B4Abm = 'J'
750600130320     c**                 When      §b4Abm = 'C'
750700130320     c***                Eval      §B4Abm = 'Q'
750800130320     c***                When      §b4Abm = 'S'
750900130320     c***                Eval      §B4Abm = 'P'
751000130320     c***                When      §b4Abm = 'K'
751100130320     c***                Eval      §B4Abm = 'W'
751200130320     c**                 Endsl
751300130320     c***                Movel     dsbl4a        Ar4not
751400130320     c***                Update    Fiar4000
751500130320     c**                 EndIf
751600031111    2c                   EndIf
751700070312     c
751800031111
751900990428    1C                   END
752000911211     C*
752100911218     C     ENDAGG        ENDSR
752200070924     c*
752300071001     C*--- Registro peso e volume usati perfatturaz. in FIAR5 --------*
752400071001     C     REGPesivol    BEGSR
752500151013     c                   clear                   dar5fat
752600151013     c                   Eval      kar5trd='FAT'
752700151013     c                   setoff                                       9293
752800151013     c     kfiar5        Chain     Fiar531c
752900151013     c                   If        %Found(Fiar531c)
753000151013     c                   Movel     ar5uni        dar5fat
753100070928     c                   endif
753200070928     c* Se tassata
753300070928    1c                   if        ar6imv>0
753400070928     c* Volume
753401170420     c                   movel     arbvlf        §ar5vltas
753402170420     c                   movel     arbfvf        §ar5fvtas
753500160530     c                   if        tasvlt>0
753501170420     c                             and PesVolIMV=Tassatoimv
753600160530     c                   z-add     tasvlt        §ar5vltas
753700160530     c                   movel     tasfvt        §ar5fvtas
753800170420     c***                else
753900170420     c***                movel     arbvlf        §ar5vltas
754000170420     c***                movel     arbfvf        §ar5fvtas
754100160530     c                   endif
754200070928     c* Peso
754300151013     c                   movel     arbpkf        §ar5pktas
754400151013     c                   movel     'R'           §ar5fptas
754500070928     c
754600070928     c* Se il totale imponibile aggiornato senza arrotondamento
754700070928     c*  è = a quello memorizzato al momento del richiamo al
754800070928     c*   TNSF20R--> memorizzo se usato peso VDL (-tara)
754900070928    2c                   if        PesVolIMV=Tassatoimv  and
755000070928     c                             PesVolspc='S'
755100151013     c                   movel     PesVolPkcn    §ar5pktas
755200070928    3c                   if        arbncp=arbncl
755300151013     c                   movel     'T'           §ar5fptas
755400070928     c                   else
755500151013     c                   movel     'Z'           §ar5fptas
755600070928    3c                   endif
755700070928     c
755800070928    2c                   endif
755900151013     c                   eval      §AR5DATAS=%dec(%date())
756000151013     c                   eval      §AR5HMTAS=%dec(%time())
756100151013     c                   eval      §AR5PRTAS=knmus
756200151013     c                   eval      §AR5NJTAS=knmtd
756300070928     c*
756400070928    1c                   endif
756500070928     c
756600151013      * aggiorno Fiar5 record "FAT"
756700151013if  1c                   If        %Found(Fiar531c)
756800151013     c* Se ho eliminato la tassazione  deleto
756900151013     c                   if        ar6imv=0
757000151013     c   92              delete    fiar500s
757100151013     c   93              delete    fiar5p0s
757200151013     c                   else
757300151013     c                   Movel(p)  dar5fat       Ar5uni
757400151013     c                   eval      ar5dac=%dec(%date())
757500151013     c                   eval      ar5orc=%dec(%time())
757600151013     c   92              Update    Fiar500s
757700151013     c   93              Update    Fiar5p0s
757800151013     c                   endif
757900151013      * scrivo   Fiar5 record "FAT"
758000070928   x1c                   Else
758100151013    2c                   if        ar6imv>0
758200151013     c                   Clear                   Fiar500s
758300151013     c                   Clear                   Fiar5p0s
758400151013     c                   Eval      Ar5Aas = ArbAas
758500151013     c                   Eval      Ar5Lnp = ArbLnp
758600151013     c                   Eval      Ar5Nrs = ArbNrs
758700151013     c                   Eval      Ar5Nsp = ArbNsp
758800151013     c                   Eval      Ar5Trd = 'FAT'
758900151013     c                   Movel(p)  dar5fat       Ar5uni
759000151013     c                   eval      ar5dac=%dec(%date())
759100151013     c                   eval      ar5orc=%dec(%time())
759200151013     c                   Eval      AR5FT1='T'
759300151013     c                   Eval      AR5DT1=99999999
759400151013     c                   Eval      AR5FT2='T'
759500151013     c                   Eval      AR5DT2=99999999
759600151013     c                   Eval      AR5FT3='T'
759700151013     c                   Eval      AR5DT3=99999999
759800151013     c* Verifico dove di trova TITAS per la write di fiar5
759900151013     c                   setoff                                       9293
760000151013     c     karb2         chain     titaS30c
760100160104     c                   if        %found(titas30c)
760200151013     c   92              Write     Fiar500s
760300151013     c   93              Write     Fiar5p0s
760400160104     c                   else
760500160104     c                   Write     Fiar500s
760600160104     c                   endif
760700070924    2c                   EndIf
760800070928    1c                   EndIf
760900070924     c
761000070924     c                   ENDSR
761100070924     c*
761200050310     C*--- AGGIORNO fnblp00f -----------------------------------------*
761300050310     C     AGGIORblp     BEGSR
761400050310     C*
761500050310     C* E S T E N S I O N E   B O L L A
761600050310     C*
761700050310    1C                   if        *in05
761800050310      * Carico le varie in una sk di comodo
761900050310     c                   ExSr      CarWar1
762000050310     C* A G G I O R N O
762100050310     C                   MOVEL     VIDKLP        AR6KSC
762200050310     C                   MOVE      VIDKSC        AR6KSC
762300050310     C                   MOVEL     VIDCTR        AR6CTR
762400050310     C                   CLEAR                   AR6POR
762500050310     C                   MOVEL     VI2DIV        AR6DIV
762600050310     C                   MOVEL     WSV(1)        AR6SV1
762700050310     C                   Z-ADD     WVA(1)        AR6VA1
762800050310     C                   MOVEL     WSV(2)        AR6SV2
762900050310     C                   Z-ADD     WVA(2)        AR6VA2
763000050310     C                   MOVEL     WSV(3)        AR6SV3
763100050310     C                   Z-ADD     WVA(3)        AR6VA3
763200050310
763300050310     C                   Z-ADD     TotImv        AR6IMV
763400050310     C                   MOVEL     VI2CEI        AR6CEI
763500050310     C                   Z-ADD     0             AR6NFT
763600050310     C                   Z-ADD     0             AR6FIV
763700050310     C                   Z-ADD     0             AR6DFT
763800050310     C                   Z-ADD     0             AR6ALI
763900050310     C                   Z-ADD     0             AR6IFT
764000050310     c*
764100050310     c* update o write di ar6/ar7  : imposto campi di comodo
764200050310     c                   eval      wesistear6=war61
764300050310     c                   eval      wesistear7=war71
764400050310     c                   eval      wtiporecar6='2'
764500050310     c                   exsr      SCRIVIar6ar7
764600050310     c
764700050310     c                   unlock    fnblp00f
764800050310    1C                   ENDif
764900050310     C**
765000050310     C** A S S E G N A T I   1   B O L L A
765100050310    1C                   if        not *in05
765200050310     C*
765300050310     C* CARICO LE VARIE IMPOSTATE IN UNA SCHIERA DI COMODO
765400050310     C                   MOVEL     *BLANKS       WCKINL            1
765500050310     C                   EXSR      CARWAR
765600050310     C**
765700050310     C* A G G I O R N O
765800050310     C**
765900050310     C                   Z-ADD     VIDKSC        AR6KSC
766000050310     C                   MOVEL     VIDKLP        AR6KSC
766100050310     C                   Z-ADD     VIDIAS        ARBIAS
766200050310     C                   MOVEL     VIDVAS        ARBVAS
766300050310      * Se l'importo da assicurare è a 0 non devo scrivere la divisa
766400050310     C     VIDIAS        IFEQ      *ZEROS
766500050310     C                   CLEAR                   ARBVAS
766600050310     C                   ENDIF
766700050310      *
766800050310     C                   MOVEL     VIDQFT        ARBQFT
766900050310     C                   MOVEL     VIDCTR        AR6CTR
767000050310     c* se ho tassato con il peso vld imposto iL flaG tasfpf
767100050310     C                   MOVEL     VIDDIV        AR6DIV
767200050310     C                   Z-ADD     VIDPOR        AR6POR
767300050310     C                   MOVEL     WSV(1)        AR6SV1
767400050310     C                   Z-ADD     WVA(1)        AR6VA1
767500050310     C                   MOVEL     WSV(2)        AR6SV2
767600050310     C                   Z-ADD     WVA(2)        AR6VA2
767700050310     C                   MOVEL     WSV(3)        AR6SV3
767800050318     C                   Z-ADD     WVA(3)        AR6VA3
767900050310     C*
768000050310     C                   Z-ADD     VIDIMV        AR6IMV
768100050310     C                   MOVEL     VIDCEI        AR6CEI
768200050310     C                   CLEAR                   AR6ALI
768300050310     C                   CLEAR                   AR6IFT
768400050310     C                   Z-ADD     0             AR6FIV
768500050310     C                   Z-ADD     0             AR6DFT
768600050310     C                   Z-ADD     0             AR6NFT
768700050310     c* update o write di ar6/ar7  : imposto campi di comodo
768800050310     c                   eval      wesistear6=war6
768900050310     c                   eval      wesistear7=war7
769000050310     c                   eval      wtiporecar6='1'
769100050310     c                   exsr      SCRIVIar6ar7
769200151013     c* Se tassazione fino all'imponibile, aggiorno peso e volume usati
769300151013     c*  per la fatturazione se inserito o variato totale imponibile
769400151013     c                   if        (ar6imv>0 and ar6imv<>savimv) or
769500151013     c                             ar6imv=0
769600151118     c                   exsr      Regpesivol
769700151013     c                   endif
769800050310     c
769900050310     c                   except    aggblpfis
770000050310    1C                   ENDIF
770100050311     C
770200050311     C                   EVAL      WAGGIOBOL='S'
770300050310     C**
770400050310     C* AGGIORNO LE NOTE
770500050310     C**
770600090507     c                   if        wscelta<>'9'
770700050310     C                   EXSR      NOTE
770800090507     c                   endif
770900050310     C*
771000050310     C                   ENDSR
771100050310     c*
771200050310     c* Scrittura comuni fira6 fiar7 per fnarb e fnblp --------------------*
771300050310     c     SCRIVIar6ar7  begsr
771400050310     C* CANCELLO LE VARIE NELL'FIAR7
771500050310    2C     Wesistear7    IFEQ      'E'
771600050310     C                   MOVEL     wtiporecar6   WTRC
771700050310     C     KARB4         SETLL     FIAR7000
771800050310     C     KARB4         READE     FIAR7000                               32
771900050310     C*
772000050310    3C     *IN32         DOWEQ     *OFF
772100050310     C                   DELETE    FIAR7000
772200050310     C     KARB4         READE     FIAR7000                               32
772300050310    3C                   ENDDO
772400050310    2C                   ENDIF
772500050310     C*
772600050310     C* SE HO PIU' DI 3 VARIE LE MEMORIZZO
772700050310    2C     WSV(4)        IFNE      ' '
772800050310     C                   Z-ADD     4             C
772900050310     C                   CLEAR                   FIAR7000
773000050310     C                   MOVEL     ARBAAS        AR7AAS
773100050310     C                   MOVEL     ARBLNP        AR7LNP
773200050310     C                   MOVEL     ARBNRS        AR7NRS
773300050310     C                   MOVEL     ARBNSP        AR7NSP
773400050310     C                   MOVEL     wtiporecar6   AR7TRC
773500050310    3C     WSV(C)        DOWNE     ' '
773600050310     C                   MOVEL     WSV(C)        AR7SVN
773700050310     C                   MOVEL     WVA(C)        AR7VAN
773800050310     C                   WRITE     FIAR7000
773900050310     C                   ADD       1             C
774000050310    3C                   ENDDO
774100050310    2C                   ENDIF
774200050310
774300050310    2C     wesistear6    IFEQ      'E'
774400050310     C                   UPDATE    FIAR6000
774500050310     C                   ELSE
774600050310     C                   CLEAR                   AR6ATB
774700050310     C                   Z-ADD     ARBAAS        AR6AAS
774800050310     C                   Z-ADD     ARBLNP        AR6LNP
774900050310     C                   Z-ADD     ARBNRS        AR6NRS
775000050310     C                   Z-ADD     ARBNSP        AR6NSP
775100050310     C                   MOVEL     wtiporecar6   AR6TRC
775200050310     c                   Clear                   Ar6Fim
775300050310     C                   WRITE     FIAR6000
775400050310    2C                   ENDIF
775500050310     c                   ENDSR
775600940223     C*
775700940223     C* RICERCA E CONTROLLO RECORD IN TABELLA ------------------------*
775800940223     C     CTRTAB        BEGSR
775900940223     C* 90 ON - RICERCA
776000940223     C     WINTER        IFEQ      '?'
776100940223     C                   MOVEL     CODUT         §KUT
776200940223     C                   MOVEL     COD           §COD
776300991011     C                   CLEAR                   §KEY
776400940223     C                   CALL      'X§TABER'
776500940223     C                   PARM                    §KUT
776600940223     C                   PARM                    §COD
776700940223     C                   PARM                    §KEY
776800940223     C                   PARM                    §DES             30
776900940223     C                   SETON                                        90
777000940223     C*
777100940223     C                   ELSE
777200940223     C*
777300940223     C     KTAB          CHAIN     TABEL                              32
777400940223     C  N32TBLFLG        IFNE      ' '
777500940223     C                   SETON                                        32
777600940223     C                   END
777700940223     C                   END
777800940223     C                   ENDSR
777900941216      *-----------------------------------------------------***********
778000941216      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
778100941216      *-----------------------------------------------------***********
778200941216     C     CTRFGS        BEGSR
778300941216     C                   SETOFF                                       90
778400941216     C                   CLEAR                   DSLV50
778500941216     C                   MOVEL     KNMUS         D50PRU
778600941216     C                   MOVEL     VIDFIL        D50FGS
778700941216     C                   CALL      'FNLV50R'
778800941216     C                   PARM                    DSLV50
778900941216     C*
779000941216     C     D50ERR        IFNE      ' '
779100941216     C                   MOVEL     D50MSG        VIDMSG
779200941216     C                   SETON                                        499028
779300941216     C                   GOTO      ENDCTF
779400941216     C                   ENDIF
779500070314     c
779600070314     c                   EXSR      DECOFGS
779700941216     C*
779800941216     C     ENDCTF        ENDSR
779900941216      *-----------------------------------------------------***********
780000941216      * GESTIONE CODICE CLIENTE DI ATRA FILIALE
780100941216      *-----------------------------------------------------***********
780200941216     C     GESKCA        BEGSR
780300941216     C*
780400941216     C* E' PER FORZA SEGUE FATTURA
780500020417     C                   SETON                                        01
780600941217     C                   MOVEL     TAS(4)        SCETAS
780700941216     C*
780800050120     C                   EXSR      PREPA06
780900941219     C                   MOVEL     *ZEROS        VIDCTR
781000950421     C                   CLEAR                   VIDMSG
781100030507      * Recupero i dati del cliente se non è un 9999
781200030507     c                   ExSr      Sr_RecDati
781300950510     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE CLIENTE DELL'AREA
781400950510     C*  SENZA TARIFFE
781500950510     C                   MOVEL     ACORAG        DESKSC
781600950203     C*
781700950203     C* 1    B O L L A
781800950203    1C  N05              DO
781900941216     C*
782000941216     C     FORKCA        TAG
782100050317     C                   WRITE     LRA2T06                                      TESTATA
782200050317     C                   WRITE     LRA2d66                                      CODICE CLIEN
782300941219     C*
782400050317     C                   WRITE     LRA2Z06                                      PIEDI
782500950421     C*
782600950421     C     VIDMSG        IFNE      *BLANKS
782700950421     C                   SETON                                        28
782800950421     C                   ENDIF
782900941216     C*
783000050317     C*                  EXFMT     LRA2C06                                      TASSAZIONE
783100050317     C                   write     LRA2C06                                      TASSAZIONE
783200050317     C                   read(E)   fnlra2d
783300050317     C
783400050317     C* Spengo indicatori di posizionamento cursore
783500941217     C                   SETOFF                                       902841
783600941217     C                   SETOFF                                       985253
783700941217     C                   SETOFF                                       554249
783800941217     C                   SETOFF                                       505145
783900990915     C                   SETOFF                                       48
784000050317     c
784100050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
784200050317     c                   if        %error
784300050317     c                   eval      *inkl='1'
784400050317     C  n94              UNLOCK    FNARB01L
784500050317     C   94              UNLOCK    FNblp00f
784600050317     C                   UNLOCK    FIAR601L
784700050317     c                   goto      endgeskca
784800050317     c                   endif
784900941216     C* F12= RITORNO
785000950203     C     *INKL         IFEQ      *ON
785100050317     C                   UNLOCK    FIAR601L
785200950203     C                   ELSE
785300941216     C*
785400950203     C                   EXSR      CTRKCA
785500950203     C   90              GOTO      FORKCA
785600941216     C* CONTROLLO TASSAZIONE
785700941217     C                   EXSR      CONTR6
785800950121     C  NKF
785900941219     COR 90              GOTO      FORKCA
786000060317     c*
786100060317     c                   if        §7lmtv = 'S'
786200060317     c                   exsr      ges_mtv
786300060317     c                   if        olv85f12 = '1'
786400060317     c                   GoTo      Forkca
786500060317     c                   endif
786600060317     c                   endif
786700941217     C**
786800941217     C* AGGIORNAMENTO
786900941217     C**
787000050310     C  n94              EXSR      AGGIORARB
787100050310     C   94              EXSR      AGGIORblp
787200941217     C   91              GOTO      FORKCA
787300950203    2C                   ENDIF
787400950203    1C                   ENDDO
787500950203     C*
787600950203     C* E S T E N S I O N E   B O L L A
787700950203    1C   05              DO
787800950203     C*
787900950203     C     FORKC2        TAG
788000950203     C* WRITE TESTATA
788100050317     C                   WRITE     LRA2T06                                      TESTATA
788200050317     C                   WRITE     LRA2d66                                      COD CLIENTE
788300950203     C* 2 BOLLA
788400050317     C*                  EXFMT     LRA2D07                                      TASSAZIONE
788500050317     C                   write     LRA2D07                                      TASSAZIONE
788600050317     C                   read(E)   fnlra2d
788700050317     c
788800950203     c* spengo indicatori di posizionamento cursore
788900950203     C                   SETOFF                                       727475
789000950203     C                   SETOFF                                       9028
789100040119     c                   Setoff                                       717376
789200040119     c                   Setoff                                       777879
789300041129     c                   Setoff                                       808182
789400041129     c                   Setoff                                       83
789500050317     c
789600050317     c* Passato tempo max 50 secondi di attesa videata: torno al sfl
789700050317     c                   if        %error
789800050317     c                   eval      *inkl='1'
789900050317     C  n94              UNLOCK    FNARB01L
790000050317     C   94              UNLOCK    FNblp00f
790100050317     C                   UNLOCK    FIAR601L
790200050317     c                   goto      endgeskca
790300050317     c                   endif
790400050120     C* F12 - RITORNO
790500950203     C     *INKL         IFEQ      *ON
790600990923     C   KL              UNLOCK    FIAR601L
790700950203     C                   ELSE
790800950203     C*
790900950203     C* ENTER - CONTROLLI
791000990913     C                   EXSR      CONTR7
791100950203     C  NKF
791200950203     COR 90              GOTO      FORKC2
791300060317     c*
791400060317     c                   if        §7lmtv = 'S'
791500060317     c                   exsr      ges_mtv
791600060317     c                   if        olv85f12 = '1'
791700060317     c                   GoTo      Forkc2
791800060317     c                   endif
791900060317     c                   endif
792000950203     C**
792100950203     C* CONFERMA TASSAZIONE AGGIORNO BOLLE
792200050310     C  n94              EXSR      AGGIORARB
792300050310     C   94              EXSR      AGGIORblp
792400950203     C   91              GOTO      FORKC2
792500950203    2C                   ENDIF
792600950203    1C                   ENDDO
792700941216     C*
792800050317     C     ENDGESKCA     ENDSR
792900941216     C*
793000950203     C* CONTROLLO CODICE TARIFFA ------------------------------------*
793100950203     C     CTRKCA        BEGSR
793200050317     C                   READ      LRA2D66                                32
793300950203     C*
793400050128     C                   CLEAR                   fnlv59ds
793500060511     C***                MOVEL     'S'           ilv59CFO
793600060511     c* non devo fare solo controllo formale, per non impostare un cod
793700060511     c*  tariffa valido per altre selezioni quindi imposto il controllo
793800060511     c*  totale
793900060511     C                   MOVEL     'A'           ilv59tbo
794000060511      * Se bolla monovaria carico tutte le tariffe ma propongo la partenza
794100060511     C                   IF        §3ASVA <> *BLANK
794200060511     C                   movel     ' '           ilv59tbo
794300060511     C                   movel     'P'           ilv59prp
794400060511     C                   ENDIF
794500060511      *
794600060511     C                   MOVEL     COMKSC        ilv59KSC
794700060511     C                   MOVEL     KCI           ilv59KCI
794800060511     C                   MOVEL     WFIE          ilv59FIE
794900060511     C                   MOVEL     ARBDSP        ilv59DSP
795000060511     C                   MOVEL     ARBTSP        ilv59TSP
795100050128     C                   MOVEL     VIDCTR        ilv59CTR
795200060511     C                   MOVEL     'S'           ilv59CON
795300050128     C                   CALL      'FNLV59R'
795400050128     C                   PARM                    fnlv59ds
795500950203     C*
795600060511     C* VEDO SE C'E' STATA LA RICERCA
795700060511     C     olv59ERR      IFEQ      'R'
795800060511     C                   MOVEL     olv59CTR      VIDCTR
795900060511     C                   SETON                                        5590
796000060511     C                   else
796100060511     C*
796200050128    3C     olv59ERR      IFNE      *BLANKS
796300050128     C                   MOVEL     olv59msg      VIDMSG
796400950203     C                   SETON                                        289055
796500950203    3C                   ENDIF
796600060511    3C                   ENDIF
796700950203     C*
796800950203     C* DESCRIZIONE CODICE TARIFFA
796900050128     C  N28              MOVEL     olv59DFS      DE2CTR
797000021121
797100021121      * Bolla FedEx e il cliente codificato non ha tariffe FedEx
797200021121If  1c                   If        O27Ntw = 'FED'
797300021121      * non posso accettare una tariffa documenti se il peso della
797400021121      * bolla superare il limite previsto in tabella "DFE"
797500021121     c                   Move      Vidctr        DueCtr
797600021121If  2c                   If        ArbPkf > §DfePkgD and
797700021121     c                             DueCtr >= §DfeDalD and DueCtr <= §DfeAlD
797800021121     c                   Eval      *In55 = *On
797900021121     c                   Eval      *In90 = *On
798000021121     c                   Eval      *In28 = *On
798100021121     c                   Eval      Vidmsg = Msg(58)
798200021121     c                   Eval      %Subst(VidMsg:59:8) = (%editc(§DfePkgd: '4'))
798300021121    2c                   EndIf
798400021121    1c                   EndIf
798500021121
798600950203     C                   ENDSR
798700950203     C*
798800941216     C* CARICO LA VARIA IN SFL ---------------------------------------*
798900941216     C     CARVAR        BEGSR
799000941216     C*
799100941216     C     WSV1          IFNE      ' '
799200011018      * Controllo se è una bolla di reso e se ha la varia 'N' 88888888
799300011018     C     ARBFBR        IFEQ      'S'
799400011018     C     WSV1          ANDEQ     'N'
799500011018     C     WVA1          ANDEQ     88888888
799600011018     C                   MOVEL     '1'           FLGVRN            1
799700011018     C                   ENDIF
799800990909     C* CARICO SOLO SE VARIA FA PARTE DEL TOTALE IMPONIBILE
799900990909     C     WSV1          LOOKUP    SVA                                    31
800000990909     C     *IN31         IFEQ      *ON
800100941216     C                   ADD       1             NRR
800200050317     C     NRR           CHAIN     LRA2S06                            32
800300941217     C                   MOVEL     WSV1          V6CSV1
800400941217     C                   Z-ADD     WVA1          V6CVA1
800500990910     C                   Z-ADD     WVA1          VARVA1
800600050317     C   32              WRITE     LRA2S06
800700050317     C  N32              UPDATE    LRA2S06
800800990909     C                   ENDIF
800900941216     C                   ENDIF
801000011003     C                   ENDSR
801100040119
801200040119      *---------------------------------------------------------------*
801300040119      * CARICO LA VARIA DELLA SECONDA BOLLA
801400040119      *---------------------------------------------------------------*
801500040119     c     CarVar1       BegSr
801600040119
801700040119if  1c                   If        wsv1 <> *Blanks
801800040119      * Carico la varia solo se fa parte del totale imponibile
801900040119     c     wsv1          Lookup    sva                                    31
802000040119if  2c                   If        *In31
802100040119     c                   add       1             ContaSv
802200040119     c                   add       wva1          TotImv
802300040119
802400040119s   3c                   Select
802500040119     c                   When      ContaSv = 1
802600040119     c                   Movel     wsv1          Vi2Sv1
802700040119     c                   Z-add     wva1          Vi2Va1
802800040119     c                   Z-add     wva1          Va2Va1
802900040119     c                   When      ContaSv = 2
803000040119     c                   Movel     wsv1          Vi2Sv2
803100040119     c                   Z-add     wva1          Vi2Va2
803200040119     c                   Z-add     wva1          Va2Va2
803300040119     c                   When      ContaSv = 3
803400040119     c                   Movel     wsv1          Vi2Sv3
803500040119     c                   Z-add     wva1          Vi2Va3
803600040119     c                   Z-add     wva1          Va2Va3
803700040119     c                   When      ContaSv = 4
803800040119     c                   Movel     wsv1          Vi2Sv4
803900040119     c                   Z-add     wva1          Vi2Va4
804000040119     c                   Z-add     wva1          Va2Va4
804100041129     c                   When      ContaSv = 5
804200041129     c                   Movel     wsv1          Vi2Sv5
804300041129     c                   Z-add     wva1          Vi2Va5
804400041129     c                   Z-add     wva1          Va2Va5
804500041129     c                   When      ContaSv = 6
804600041129     c                   Movel     wsv1          Vi2Sv6
804700041129     c                   Z-add     wva1          Vi2Va6
804800041129     c                   Z-add     wva1          Va2Va6
804900040119    3c                   EndSl
805000040119
805100040119    2c                   EndIf
805200040119
805300040119    1c                   EndIf
805400040119
805500040119     c                   EndSr
805600011003      **
805700011003      *--- TASSAZIONE VARIA 'N' --------------------------------------*
805800011003     C     TASSAN        BEGSR
805900011003      *
806000011003     C                   CLEAR                   DTAS
806100011003     C                   CLEAR                   DTASV
806200011003     C                   MOVEL     VIDKLP        TASKSC
806300011003     C                   MOVE      VIDKSC        TASKSC
806400011003     C                   MOVEL     VIDCTR        TASCTR
806500011003     C                   Z-ADD     ARBPKF        TASPKF
806600160530     C                   Z-ADD     ARBPKb        TASPKb
806700050714     C**                 MOVEL     arBLNP        TASLNP
806800050714     C                   MOVEL     VUNICOLNP     TASLNP
806900011003     C                   MOVEL     ARBCTS        TASCTS
807000011003     C                   MOVEL     ARBDSP        TASDSP
807100011003     C                   MOVEL     ARBDSP        TASDCT
807200011003     C                   MOVEL     ARBVLF        TASVLF
807300160530     C                   MOVEL     ARBfvF        TASfvf
807400160530     c                   z-add     arbvlc        tasvlc
807500160530     c                   z-add     arbncr        tasncr
807600011003     C                   MOVEL     ARBTSP        TASTSP
807700011003     C     WBOL          IFEQ      '1'
807800011003     C                   MOVEL     §3ATB1        TASTBL
807900011003     C                   ELSE
808000011003     C                   MOVEL     §3ATB2        TASTBL
808100011003     C                   ENDIF
808200011003     C                   MOVEL     ARBLNA        TASLNA
808300011003     C                   MOVEL     ARBNCL        TASNCL
808400160530     C                   MOVEL     ARBNCL        TASNCLb
808500011003     C                   MOVEL     ARBQFT        TASQFT
808600011003     C     WBOL          IFEQ      '1'
808700011003     C     §3AFCA        ANDEQ     1
808800011003     C     WBOL          OREQ      '2'
808900011003     C     §3AFCA        ANDEQ     2
809000011003     C                   MOVEL     AR9CAS        TASCAS
809100011003     C                   MOVEL     AR9VCA        TASVCA
809200011003     C                   MOVEL     AR9TIC        TASTIC
809300011003     C                   ENDIF
809400011003     C                   MOVEL     ARBIAS        TASIAS
809500011003     C                   MOVEL     ARBVAS        TASVAS
809600011003     C                   MOVEL     ARBCAD        TASCAD
809700011003     C                   MOVEL     ARBNZD        TASNZD
809800011003     C                   MOVEL     ARBFIN        TASFIN
809900050715     C                   MOVEL     Vunicocam     TASCAM
810000050715     C**                 MOVEL     ARBCAM        TASCAM
810100050714     C**                 MOVEL     ARBNZM        TASNZM
810200050714     C                   MOVEL     Vuniconzm     TASNZM
810300050715     C                   MOVEL     Vunicofap     TASFAP
810400050715     C**                 MOVEL     ARBFAP        TASFAP
810500011003     C                   MOVEL     ARBTC1        TASTC1
810600011003     C                   MOVEL     ARBTC2        TASTC2
810700011003     C     WDDT          IFEQ      'C'                                          SI DDT
810800011003     C     WDDT          OREQ      'S'
810900031028     c     wddt          oreq      'P'
811000031028     c     wddt          oreq      'Q'
811100011003     C                   MOVEL     'S'           TASDDT
811200011003     C                   ENDIF
811300060925     c*****              movel     arbfbr        tasflo
811400060925     c                   clear                   dtas01
811500060925     c                   movel     arbfbr        §asfbr
811600060925     c                   movel     arbcca        §ascca
811700060925     c                   eval      tasflo = dtas01
811800011003      **
811900011003      * SE BOLLA EXPORT--> NON PASSO MAI IL FLAG CONSEGNA A MAGAZZINO
812000011003      *                    IN MODO CHE TASSI SEMPRE LA VARIA 'U'
812100020222     c                   if        lnantw <> 'EEX' or lnantw <> 'EUP' or
812200020222     c                             lnantw <> 'FED' or lnantw <> 'DPD'
812300060123     C***                MOVEL     ARBFDN        TASFDN
812400060123     C                   MOVEL     VunicoFDN     TASFDN
812500011003     C                   ENDIF
812600011003      **
812700011003      * CHAIN SU CAPPARIO PER PRENDERE COD TASSAZIONE MITTENTE
812800050715     c* Se bolla dirottata, e posso utilizzare i dirottamenti
812900050715     c*  prendo quello già calcolato
813000050715     c                   if        Vunicomct<>'  ' and
813100050715     c                             (§vpodir='A' or §vpodir='E')
813200050715     c                   movel     Vunicomct     tasmct
813300050715     c                   else
813400050715     c
813500011003     C                   CLEAR                   DSSI95
813600011003     C                   MOVEL     ARBLOM        I95LOC
813700050715     C                   MOVEL     Vunicocam     I95CAP
813800050715     C**                 MOVEL     ARBCAM        I95CAP
813900050714     C***                MOVEL     ARBNZM        I95NAR
814000050714     C                   MOVEL     vuniconzm     I95NAR
814100011003     C                   MOVEL     '3'           I95TCN
814200011003     C                   MOVEL     ARBDSP        I95DAT
814300011003     C     §3AFCA        IFNE      0
814400011003     C                   MOVEL     'S'           I95FCA
814500011003     C                   ENDIF
814600011003     C                   MOVEL     §3ATB1        I95TPO
814700020228      * Se network FedEx passo flag apposito per dire di prendere i dati
814800020228      * dalla tabella della nazione
814900160530     c     lnpntw        ifeq      'FED'
815000020228     c                   eval      i95fi1 = 'S'
815100020228     c                   endif
815200020228
815300011003     C                   CALL      'TISI95R'
815400011003     C                   PARM                    DSSI95
815500011003     C     O95ERR        IFEQ      *BLANKS
815600011003     C                   MOVEL     O95CTS        TASMCT
815700011003     C                   ENDIF
815800050715     C                   ENDIF
815900050715     C*********          MOVEL     'S'           WCAL13            1
816000011003      * ELABORO E CHIUDO CON LR IL PGM
816100011113     C                   MOVEL     ' '           TASTLA
816200011003      *
816300020930     c* imposto peso vdl e colli rilevati
816400030203     c                   z-add     arbncp        Tasncp
816500030203     c                   z-add     arbpkc        Taspkc
816600030204
816700030204      * Bancali
816800030205     c                   If        %Subst(ArbGva:1:1) = 'B'
816900030204     c                   Z-Add     VidBan        TasBan
817000030205     c                   EndIf
817100030728      * Colli originali
817200030728     c                   If        %Subst(ArbGva:1:1) = 'O'
817300030728     c                   Z-Add     VidNcl        TasNcl
817400030728     c                   EndIf
817500050804     c
817600050804     c                   eval      parca  = ' '
817700050804     c                   movel     paramt        kpjbu
817800050804      *
817900020930     C*
818000011003     C                   CALL      'TNSF20R'
818100011003     C                   PARM                    KPJBA
818200011003     C                   PARM                    DTAS
818300011003     C                   PARM                    DTASV
818400011003      *
818500011018      * Tassazione andata buon fine
818600011003     C     TASERR        IFEQ      *BLANKS
818700011018      *
818800011018      * Controllo la divisa della tassazione con la divisa del video
818900011018      * se non corrispondono devo convertire la varia 'N' nella divisa
819000011018      * del video
819100011018     C     TASDIV        IFNE      VIDDIV
819200011018      * aggancio la tabella CV per sicurezza
819300011018     C                   CLEAR                   DSCV
819400011018     C                   MOVEL     'CV'          COD
819500011018     C                   MOVEL(P)  VIDDIV        KEY
819600011018     C                   EXSR      CTRTAB
819700011018     C  N32              MOVEL     TBLUNI        DSCV
819800011018      *
819900011018     C                   CLEAR                   DSYEUR
820000011018     C                   MOVEL     TASDIV        YECDVI
820100011018     C                   MOVEL     VIDDIV        YECDVO
820200011018     C                   Z-ADD     TASIMV        YECIMI
820300011018     C     §CVFDC        IFEQ      'S'
820400011018     C                   MOVE      '3'           YECDCO
820500011018     C                   ELSE
820600011018     C                   MOVE      '0'           YECDCO
820700011018     C                   ENDIF
820800011018     C                   CALL      'YEURCO'
820900011018     C                   PARM                    DSYEUR
821000011018     C     YECESI        IFEQ      ' '
821100011018     C                   Z-ADD     YECIMO        TASIMV
821200011018     C                   ELSE
821300011018     C                   Z-ADD     88888888      TASIMV
821400011018     C                   ENDIF
821500011018     C                   ENDIF
821600011018      *
821700011010     C                   Z-ADD     TASIMV        V6CVA1
821800011010     C                   Z-ADD     TASIMV        VARVA1
821900011003     C                   ENDIF
822000011003      *
822100011003     C                   ENDSR
822200941217     C*
822300941217     C* CARICO LE NOTE -----------------------------------------------*
822400941217     C     NOTE          BEGSR
822500060217     C                   CLEAR                   DSLV19
822600060217     C                   MOVEL     'T'           D19ftr
822700941217     C                   MOVEL     ARBAAS        D19AAS
822800941217     C                   MOVEL     ARBLNP        D19LNP
822900941217     C                   MOVEL     ARBNRS        D19NRS
823000941217     C                   MOVEL     ARBNSP        D19NSP
823100050110     C                   MOVEL     VIDNT1        D19NT1
823200050110     C                   MOVEL     VIDNT2        D19NT2
823300941217     C*
823400941217    1C     D19NT1        IFNE      *BLANKS
823500941217     C     D19NT2        ORNE      *BLANKS
823600941217     C                   MOVEL     '8'           D19TRC
823700941217     C                   CALL      'FNLV19R'
823800941217     C                   PARM                    DSLV19
823900960118    1C                   ENDIF
824000960118     C* SE, CHIAMATO, NON C'E' ERRORE E LA SECONDA NOTA E' VUOTA
824100960118     C*  LA CANCELLO SE PRIMA C'ERA
824200960118    2C     D19ERR        IFEQ      *BLANKS
824300960118     C     D19NT1        ANDNE     *BLANKS
824400960118     C     D19NT2        ANDEQ     *BLANKS
824500960118     C                   MOVEL     '9'           D19TRC
824600960118     C                   CLEAR                   D19NT1
824700960118     C                   CLEAR                   D19NT2
824800960118     C                   CALL      'FNLV19R'
824900960118     C                   PARM                    DSLV19
825000960118     C*
825100960118     C                   ELSE
825200941217     C*
825300941217     C* SE TORNATO CODICE DI ERRORE  O ENTRAMBE VUOTE
825400941217     C* RICHIAMO 2 VOLTE IL PGMUNA VOLTA PER NOTA
825500960118    2C     D19ERR        IFNE      *BLANKS
825600941217     C     D19NT1        OREQ      *BLANKS
825700941217     C     D19NT2        ANDEQ     *BLANKS
825800941217     C* NOTA 1
825900941217     C                   MOVEL     '8'           D19TRC
826000941217     C                   CLEAR                   D19NT2
826100941217     C                   CALL      'FNLV19R'
826200941217     C                   PARM                    DSLV19
826300941217     C* NOTA 2
826400941217     C                   MOVEL     '9'           D19TRC
826500050110     C                   MOVEL     VIDNT2        D19NT1
826600941217     C                   CLEAR                   D19NT2
826700941217     C                   CALL      'FNLV19R'
826800941217     C                   PARM                    DSLV19
826900960118    2C                   ENDIF
827000960118    1C                   ENDIF
827100941217     C*
827200941217     C                   ENDSR
827300941217     C*
827400050114     C*--- ALLOCO OGGETTI per sede -----------------------------------*
827500170116     C**   ALLOC         BEGSR
827600170116     C**                 MOVE      ')'           VAR               7
827700170116     C**                 MOVE      '))'          VAR2              8
827800170116     C**                 Z-ADD     48            LUNG             15 5
827900941217     C*
828000170116     C**                 MOVEA     SFILE         C20(13)
828100170116     C**                 MOVEA     SFILE         C21(13)
828200170116     C**                 MOVEA     SFILE         C22(13)
828300170116     C**                 MOVEA     C22           SOVR(1)
828400170116     C**                 MOVEA     C20           SALC(1)
828500170116     C***                MOVEA     C21           SDLC(1)
828600050114     C*
828700170116    1C**   §7LFFI        IFEQ      'S'
828800941217     C* CHECK OBJ E ADDPFM
828900170116     C**                 Z-ADD     046           PARMBR
829000170116     c***                move      046           $mbr
829100170116     C***                MOVEL     SFILE         FISICO
829200170116     C**                 CALL      'TRUL50C'
829300170116     C**                 PARM                    FISICO           10
829400170116     C**                 PARM                    MBRFIS           10
829500170116     C**                 PARM                    LIB              10
829600170116     C**                 PARM                    LOGICO           10
829700170116     C**                 PARM                    MBRLOG           10
829800170116     C**                 PARM                    ULFLG             1
829900941217     C** OVRDBF
830000170116     c***                movel     $mbr          var
830100170116     C**                 MOVE      VAR           SOVR
830200170116     C**                 MOVEL     *BLANKS       COMMAN          130
830300170116     C**                 MOVEA     SOVR(1)       COMMAN
830400170116     C**                 CALL      'QCMDEXC'
830500170116     C**                 PARM                    COMMAN
830600170116     C**                 PARM                    LUNG
830700941217     C* ALLOCO OGGETTO
830800170116     c**                 movel     $mbr          var2
830900170116     C**                 MOVE      VAR2          SALC
831000170116     C**                 MOVEL     *BLANKS       COMMAN
831100170116     C**                 MOVEA     SALC(1)       COMMAN
831200170116     C**                 CALL      'QCMDEXC'                            91
831300170116     C**                 PARM                    COMMAN
831400170116     C**                 PARM                    LUNG
831500170116    1C**                 END
831600941217     C*
831700050114     C* 91 ON - MEMBRO ALLOCATO MESSAGGIO DI ATTENDERE
831800941217     C*
831900170116     C**   *IN91         IFEQ      *ON
832000170116     C**                 SETON                                        28
832100170116     C**                 MOVEL     MSG(22)       VIDMSG
832200170116     C**                 ENDIF
832300941217     C*
832400170116     C**                 ENDSR
832500941217     C*---------------------------------------------------------------
832600941217     C* DISALLOCO OGGETTO MEMBRO PER LA SEDE
832700941217     C*---------------------------------------------------------------
832800170116     C**   DLCSED        BEGSR
832900170116     c**                 move      046           $mbr
833000170116     c**                 movel     $mbr          var2
833100170116     C**                 MOVE      VAR2          SDLC(1)
833200170116     C**                 MOVEL     *BLANKS       COMMAN
833300170116     C**                 MOVEA     SDLC(1)       COMMAN
833400170116     C**                 CALL      'QCMDEXC'                            91
833500170116     C**                 PARM                    COMMAN
833600170116     C**                 PARM                    LUNG
833700170116     C**                 ENDSR
833800941217     C*---------------------------------------------------------------
833900941217     C* STORICIZZO LA TASSAZIONE
834000941217     C*---------------------------------------------------------------
834100941217     C     STOARB        BEGSR
834200990924     C                   MOVEL     AR6TRC        ARBTRC
834300990924     C                   Z-ADD     AR6KSC        ARBKSC
834400990924     C                   Z-ADD     AR6CTR        ARBCTR
834500990913     C                   MOVEL     AR6DIV        ARBDIV
834600941217     C                   MOVEL     AR6POR        ARBPOR
834700941217     C                   MOVEL     AR6SV1        ARBSV1
834800941217     C                   MOVEL     AR6VA1        ARBVA1
834900941217     C                   MOVEL     AR6SV2        ARBSV2
835000941217     C                   MOVEL     AR6VA2        ARBVA2
835100941217     C                   MOVEL     AR6SV3        ARBSV3
835200941217     C                   MOVEL     AR6VA3        ARBVA3
835300941217     C                   MOVEL     AR6IMV        ARBIMV
835400941217     C                   MOVEL     AR6ALI        ARBALI
835500941217     C                   MOVEL     AR6CEI        ARBCEI
835600941217     C                   MOVEL     AR6IFT        ARBIFT
835700941217     C                   MOVEL     AR6DFT        ARBDFT
835800941217     C                   MOVEL     AR6NFT        ARBNFT
835900941217     C                   MOVEL     AR6FIV        ARBFIV
836000991014     C                   MOVEL     AR6FIM        ARBFIM
836100990913     C                   WRITE     FIARBT00
836200040122      * 1   B O L L A
836300990913     C* SE HO DELLE ATRE VARIE LE STORICIZZO NEL FILE FIARBU0F
836400941221    1C     WAR7          IFEQ      'E'
836500990913    1C     ARBTRC        ANDEQ     '1'
836600990913     C                   MOVEL     '1'           WTRC
836700990913     C     KARB4         SETLL     FIAR7000
836800990913     C     KARB4         READE     FIAR7000                               32
836900941217     C*
837000941220    2C     *IN32         DOWEQ     *OFF
837100941217     C                   MOVEL     AR7SVN        ARBSVN
837200941217     C                   MOVEL     AR7VAN        ARBVAN
837300990913     C                   WRITE     FIARBU00
837400941217     C*
837500990913     C     KARB4         READE     FIAR7000                               32
837600941217    2C                   ENDDO
837700941217     C                   ENDIF
837800040122      * E S T E N S I O N E   B O L L A
837900040122     C* SE HO DELLE ALTRE VARIE LE STORICIZZO NEL FILE FIARBU0F
838000040122    1C     WAR71         IFEQ      'E'
838100040122    1C     ARBTRC        ANDEQ     '2'
838200040122     C                   MOVEL     '2'           WTRC
838300040122     C     KARB4         SETLL     FIAR7000
838400040122     C     KARB4         READE     FIAR7000                               32
838500040122     C*
838600040122    2C     *IN32         DOWEQ     *OFF
838700040122     C                   MOVEL     AR7SVN        ARBSVN
838800040122     C                   MOVEL     AR7VAN        ARBVAN
838900040122     C                   WRITE     FIARBU00
839000040122     C*
839100040122     C     KARB4         READE     FIAR7000                               32
839200040122    2C                   ENDDO
839300040122    1C                   ENDIF
839400941217     C*
839500941217     C                   ENDSR
839600941217     C*
839700941217     C* CONTROLLO DIVISA ---------------------------------------------*
839800941217     C     CTRVLT        BEGSR
839900941217     C                   CLEAR                   WINTER
840000950104     C                   CLEAR                   VIDMSG
840100011011      * La divisa è obbligatoria se è inserito un importo
840200011011     C     W0133         IFGT      *ZEROS
840300011011     C     WVLT          ANDEQ     *BLANKS
840400011011     C                   MOVEL     MSG(54)       VIDMSG
840500011011     C                   SETON                                        90
840600011011     C                   GOTO      ENDVLT
840700011011     C                   ENDIF
840800941217     C*
840900941217     C                   MOVEL     'CV'          COD
841000941217     C* RICERCA
841100941217     C     '?'           SCAN      WVLT                                   90
841200941217     C     *IN90         IFEQ      *ON
841300941217     C                   MOVEL     '?'           WINTER
841400941217     C                   ELSE
841500941217     C                   MOVEL(P)  WVLT          KEY
841600941217     C                   ENDIF
841700941217     C*
841800941217     C                   EXSR      CTRTAB
841900941217     C* ERRORE
842000941217     C     WINTER        IFEQ      '?'
842100941217     C                   MOVEL     §KEY          WVLT
842200941217     C                   ELSE
842300941217     C* ERRORE
842400941217    1C     *IN32         IFEQ      *ON
842500941217     C                   MOVEL     MSG(19)       VIDMSG
842600950104     C                   SETON                                        90
842700941217     C                   GOTO      ENDVLT
842800941217    1C                   ENDIF
842900941217    1C                   ENDIF
843000941217     C*
843100941217     C                   MOVEL     TBLUNI        DSCV
843200011002      *
843300011002      * Controllo se la divisa immessa è scaduta o no
843400011002     C     ARBDSP        IFGT      §CVDLI
843500011106     C     W0133         ANDGT     *ZEROS
843600011002     C                   MOVEL     MSG(23)       VIDMSG
843700011002     C                   SETON                                        90
843800011002     C                   GOTO      ENDVLT
843900011002     C                   ENDIF
844000941217     C*
844100941217     C* VEDO SE AMMESSI IMPORTI DECIMALI NELL'IMPORTO
844200941217    1C     §CVFDC        IFNE      'S'
844300941217     C                   MOVE      W0133         W0033             3 3
844400941217    2C     W0033         IFNE      0
844500941217     C                   MOVEL     MSG(20)       VIDMSG
844600950104     C                   SETON                                        90
844700941217     C                   GOTO      ENDVLT
844800941217    2C                   ENDIF
844900941217    1C                   ENDIF
845000011002      *
845100011002      * Controllo il numero di decimali immessi
845200011011    1C     §CVFDC        IFEQ      'S'
845300011002     C     W0133         ANDNE     *ZEROS
845400011011      *
845500011011    2C                   SELECT
845600011011     C     §CVDEC        WHENEQ    3
845700011011     C     §CVDEC        WHENEQ    2
845800011011     C                   MOVE      W0133         W0010             1 0
845900011011    3C     W0010         IFNE      *ZEROS
846000011011     C                   MOVEA     MSG(24)       K78
846100011011     C                   MOVE      §CVDEC        WCVDEC            2
846200011011     C                   MOVEA     WCVDEC        K78(29)
846300011011     C                   MOVEA     K78           VIDMSG
846400011011     C                   SETON                                        90
846500011011    3C                   ENDIF
846600011011     C     §CVDEC        WHENEQ    1
846700011011     C                   MOVE      W0133         W0020             2 0
846800011011    3C     W0020         IFNE      *ZEROS
846900011011     C                   MOVEA     MSG(24)       K78
847000011011     C                   MOVE      §CVDEC        WCVDEC            2
847100011011     C                   MOVEA     WCVDEC        K78(29)
847200011011     C                   MOVEA     K78           VIDMSG
847300011011     C                   SETON                                        90
847400011011    3C                   ENDIF
847500011011     C     §CVDEC        WHENEQ    0
847600011011    2C                   ENDSL
847700011002      *
847800011011    1C                   ENDIF
847900941217     C*
848000941217     C     ENDVLT        ENDSR
848100941219     C**************************************************************************
848200941219     C*  DECODIFICA CONSEGNE PARTICOLARI
848300941219     C**************************************************************************
848400941219     C     DECCPA        BEGSR
848500941219     C                   CLEAR                   V1DTC1
848600050113     C                   CLEAR                   V3DTC1
848700941219     C     ARBTC1        IFNE      *BLANKS
848800941219     C                   MOVEL     '1P'          COD
848900941219     C                   MOVEL(P)  ARBTC1        KEY
849000941219     C     KTAB          CHAIN     TABEL                              32
849100941219     C  N32              MOVEL     TBLUNI        V1DTC1
849200050113     C  N32              MOVEL     TBLUNI        V3DTC1
849300941219     C                   ENDIF
849400941219     C*
849500941219     C                   CLEAR                   V1DTC2
849600050113     C                   CLEAR                   V3DTC2
849700941219     C     ARBTC2        IFNE      *BLANKS
849800941219     C                   MOVEL     '1P'          COD
849900941219     C                   MOVEL(P)  ARBTC2        KEY
850000941219     C     KTAB          CHAIN     TABEL                              32
850100941219     C  N32              MOVEL     TBLUNI        V1DTC2
850200050113     C  N32              MOVEL     TBLUNI        V3DTC2
850300941219     C                   ENDIF
850400941219     C                   ENDSR
850500990914     C**************************************************************************
850600990914     C*  REPERIMENTO TABELLA GEI
850700990914     C**************************************************************************
850800990914     C     RIEGEI        BEGSR
850900991117     C                   CLEAR                   DSBS02
851000991117     C                   CLEAR                   DGEI
851100991117     C                   MOVEL     'C'           T02MOD
851200991117     C                   MOVEL     KNSIF         T02SIF
851300991117     C                   MOVEL     'GEI'         T02COD
851400991117     C                   MOVEL     ARBDIV        T02KE1
851500991117     C                   CALL      'TIBS02R'
851600991117     C                   PARM                    KPJBA
851700991117     C                   PARM                    DSBS02
851800991117     C     T02ERR        IFEQ      *BLANKS
851900991117     C                   MOVEL     T02UNI        DGEI
852000991117     C                   ENDIF
852100990914     C                   ENDSR
852200040513     C**************************************************************************
852300040513     C*  Aggiornamento IAS/VAS/QFT su fnblp
852400040513     C**************************************************************************
852500040513     C     AGG_blp       BEGSR
852600040513     c     karb2         chain     fnblp01l
852700040513     c                   if        %found(fnblp01l)
852800040513     c                   eval      blpqft = arbqft
852900040513     c                   eval      blpias = arbias
853000040513     c                   eval      blpvas = arbvas
853100040513     c                   update    fnblp000
853200040513     c                   endif
853300040513     c                   ENDSR
853400060317     C**************************************************************************
853500060317     C*  Richiamo pgm di richiesta/controllo motivazione variazione
853600060317     C**************************************************************************
853700060317     C     ges_mtv       BEGSR
853800060317     c                   clear                   fnlv85ds
853900060317     c                   movel     'V'           ilv85tel
854000060317     c                   movel     ilra2cvb      ilv85cvb
854100060317     c                   movel     fnlv85ds      kpjbu
854200060317     c                   call      'FNLV85R'
854300060317     c                   parm                    kpjba
854400060317     c                   movel     kpjbu         fnlv85ds
854500060317     c                   endsr
854600060317     C**************************************************************************
854700060317     C*  Richiamo pgm per storicizzazione motivazione variazione
854800060317     C**************************************************************************
854900060317     C     sto_mtv       BEGSR
855000060317     c                   clear                   fnlv85ds
855100060317     c                   movel     'L'           ilv85tla
855200060317     c                   movel     'R'           ilv85tel
855300060317     c                   movel     ilra2cvb      ilv85cvb
855400060317     c                   z-add     arbdtv        ilv85dac
855500060317     c                   z-add     arborv        ilv85orc
855600060317     c                   z-add     vidaas        ilv85aas
855700060317     c                   z-add     vidlnp        ilv85lnp
855800060317     c                   z-add     vidnrs        ilv85nrs
855900060317     c                   z-add     vidnsp        ilv85nsp
856000060317     c                   movel     fnlv85ds      kpjbu
856100060317     c                   call      'FNLV85R'
856200060317     c                   parm                    kpjba
856300060317     c                   endsr
856400130416     C**************************************************************************
856500130416     C* Invio tassazioniee fattura immediata a pda se bolla in distinta
856600130416     C**************************************************************************
856700131119     C*    sr_pda        BEGSR
856800131119     c*                  if        §7lpda='S'
856900131119     c*                  z-add     arbifp        w0030             3 0
857000131119     c*    w0030         chain     azorg01l
857100131119     c*                  if        %found(azorg01l)
857200131119     c*                  eval      og148=orgde8
857300131119     c*                  if        §ogpdacon<>' ' and arbndc>0 and arbdcm=0
857400131119     c*                  clear                   fidg42ds
857500131119     c*                  eval      CO42FGS=arbifp
857600131119     c*                  eval      CO42NDC=arbndc
857700131119     c*                  eval      CO42DDC=arbddc
857800131119     c*                  eval      CO42AAS=arbaas
857900131119     c*                  eval      CO42LNP=arblnp
858000131119     c*                  eval      CO42NRS=arbnrs
858100131119     c*                  eval      CO42NSP=arbnsp
858200131119     c*                  eval      CO42CMT='N'
858300131119     c*                  movel     fidg42ds      kpjbu
858400131119     c*                  call      'FIDG42R'
858500131119     c*                  parm                    kpjba
858600131119     c*                  endif
858700131119     c*                  endif
858800131119     c*                  endif
858900131119     C*                  ENDSR
859000050310     Ofnblpfis  E            AGGblpfis
859100050310     O                       arbvas
859200050310     O                       arbias
859300050310     O                       arbqft
859400991206     C**************************************************************************
859500011011**
859600911115SEGUE  FATTURA
859700911114FATTURAZIONE IMMEDIATA
859800911028NON ESISTONO TARIFFE
859900911115    SEGUE  FATTURA
860000911029**
860100050310 ***  TASSAZIONE  PORTI  ASSEGNATI  ***
860200050329 * TASSAZIONE ANTICIPATA  P.ASSEGNATI *
860300050310 ***  MANUTENZIONE  BOLLE   ARRIVI  ***
861000030613**  cmA4
861100030613OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
861200111221 SHARE(*YES) USRDTA('Tass.PA A4') OVRSCOPE(*CALLLVL) SECURE(*YES)
861300030613**  cmA5
861400030613OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
861500111221 SHARE(*YES) USRDTA('Tass.PA A5') OVRSCOPE(*CALLLVL) secure(*YES)
861600071001** OVR
861700151013OVRDBF FILE(FIAR531C) TOFILE(
861800071001OVRDBF FILE(TITAS30C) TOFILE(
861900941216**         MSG
862000941216Tipo incasso C/Assegno errato
862100941216Cliente non autorizzato al tipo incasso C/A indicato
862200941216Tipo incasso non utilizzabile se C/Assegno in divisa estera                   03
862300941216Filiale Cliente inesistente
862400941216Codice non ammesso se cliente di altra filiale                                05
862500941216Codice cliente errato o inesistente
862600941216Codice cliente non utilizzabile: bloccato in Piano dei Conti
862700941216Codice cliente di altra filiale: F9 per forzare l'immissione
862800941216Immessa Varia "R" per 8888 o 9999 :obbligatorio l'imp.Assicur.Se 0 da tariffa 09
862900941216Codice tariffa inesistente
863000941227Non sono state trovate tariffe per il codice utilizzato per tassare           11
863100040119§§ovvigione a zero: non indicata la percentuale in tariffa                    12
863200941217Errore in tassazione                                                          13
863300941217Se modificata la divisa, ridigitare il relativo importo                       14
863400011210Importo da Ass. > del limite imposto xxxxxxxxxx,xxx EUR.TEL.IN SEDE X AUTOR.  15
863500941217Importo da Assicurare = 0 : ENTER per prendere il massimo importo da Tariffa  16
863600011205Importo da Assicurare da non immettere, come indicato nella tariffa del cli.  17
863700011204Importo da Assicurare SUPERIORE al mandato esposto in tariffa:ENTER X FORZARE 18
863800941217Valuta errata                                                                 19
863900941217Non ammessi importi con decimali per la valuta indicata                       20
864000040119§§ immessa la divisa immettere anche l'importo o eliminare entrambi           21
864100941217Non e' possibile al momento eseguire la variazione: attendere e riprovare     22
864200011002Divisa scaduta                                                                23
864300011002Ammessi importi con massimo __ decimali per la divisa indicata                24
864400941219Codice errato                                                                 25
864500040119§§dice inutilizzabile                                                         26
864600040119§§ria 1 Errata                                                                27
864700040119§§ria "G" non ammessa                                                         28
864800040119§§ria "G" obbligatoria                                                        29
864900941219Immettere la varia "G" o cambiare tipo bolla per non far pagare le provvig.   30
865000040119§§dice esenzione IVA errato
865100941221Imponibile obbligatorio se fatturazione immediata
865200950104Q.ta'da fatturare = 0 per tariffa a valore:ENTER per prendere importo da assic33
865300950104Inserire la quantita' fatturare  per tariffa a valore                         34
865400950104Q.ta'da fatturare non inseribile per tariffa a valore:assunto importo da assic35
865500980526Per tariffa a quantita' inserire la quantita' da fatturare: ENTER per forzare 36
865600991014Modificato importo da assic. per tariffa a valore:azzerare la tassazione      37
865700950509Importo da assicurare obbligatorio come indicato in tariffa                   38
865800960710Record bolla attualmente in uso: attendere e riprovare!!                      39
865900960710Fine scorrimento
866000970605Non trovato il numeratore delle fatture: tassazione non effettuabile          41
866100040119§§r bolle export con VARIA "N", tassare la spedizione con F14 !!              42
866200011128Importo da Assicurare non ammesso                                             43
866300981008Per codice 9999 la linea cliente deve essere uguale alla linea arrivo bolla   44
866400040119§§possibile fare fattura immediata a cliente di altro network                 45
866500060221Importo da assicurare INFERIORE a quanto previsto dalla legge                 46
866600990928Per codice cliente 9999 senza tariffa è obbligatoria la divisa xxx            47
866700991124Divisa obbligatoria se immesso almeno un importo di tassazione                48
866800000911Non so possono fare segue Fattura su clienti poste                            49
866900050120Per Tassazione multipla non ammesso codice cliente senza tariffe              50
867000020625Codice tariffa proposto da bollettazione                                      51
867100011126Tipo incasso non utilizzabile per la divisa del contrassegno                  52
867200011008Manca la varia 'N'                                                            53
867300011011Divisa obbligatoria                                                           54
867400011203Gli importi inseriti SUPERANO il limite fatturabile, xxxxxxxxxx,xxx xxx       55
867500011203Importo varia SUPERA il limite fatturabile, xxxxxxxxxx,xxx xxx                56
867600011210Importo da Assicurare > di xxxxxxxxxx,xxx EUR: VERIFICARE. ENTER PER FORZARE  57
867700021121Non ammessa tariffa FedEx Documenti: peso superiore a Kg. xxxxxx,x            58
867800030804Ampiezza massima subfile raggiunta. Non sono visualizzate tutte le spedizioni 59
867900040121Spedizione "monovaria". Non inserire altre varie se non immessa la varia " "
868000040930Codice cliente non utilizzabile                                               61
868100050112Presenti Opzioni: premere ENTER o eliminarle per premere F1-Conferma totale   62
868200050125Tassazione multipla di bolle con codici clienti/tariffa diversi:ENTER x forzar63
868300050120Per tassazione Multipla impossibile selezionare tariffa che preveda FATT.IMMED64
868400050120Per Tassazione Multipla impossibile selezionare codice cliente 9999           65
868500050120Errori in tassazione bolle : tassarle con opzione '1'                         66
868600050120Errori di allocazione record bolla e di tassazione: riprovare con opzione '1' 67
868700050125Cliente con cod.tariffa diversi per tipo servizio
868800050126PROGRAMMA  AL  MOMENTO  NON  UTILIZZABILE !!!!!                               69
868900050126Impossibile tassazione multipla per bolle FEDEX                               70
869000050203Impossibile tassazione multipla per bolle con network differenti              71
869100050203Impossibile tassazione multipla per bolle monovaria e non                     72
869200050203Impossibile tassazione multipla x spedizioni con tipo bolla e network diversi 73
869300050310In tassazione P.A. in PARTENZA, impossibile selezionare codice cliente 99999  74
869400051013Tassazione P.A.in PARTENZA:tariffa che prevede FATT.IMMEDIATA non selezionabil75
869500050310F15-forzatura fattura immediata non ammessa per tassazione in partenza        76
869600050318Impossibile tassazione multipla per bolle in assegnato solo per spese di C/ASS77
869700050804ATTENZIONE! Bolla NON Manutenzionata.Uscita automatica x 50 sec di inattività
869800050926Porto obblogatorio per bolla dirottata                                        79
869900051013F15-forzatura fattura immediata non ammessa per codice  ESTERO                80
870000051013Codice ESTERO : tariffa che prevede FATT.IMMEDIATA non selezionabile          81
870100051013Fattura immediata non ammessa per codice  ESTERO                              82
870200051024Impossibile tassaz.multipla:manca ImpAssicurare x tariffa con obbligo inserim.83
870300051024Impossibile tassaz.multipla:presenti ImpAssicurare non ammessi x il CodTariffa84
