000100111214     /*END
000200060920      *---------------------------------------------------------------*
000300090128      * Da FITR400F crea FLTR700F e FIBARxxxV                         *
000400111214      *       e aggiorna FNMSA10F e FIMSC00F                          *
000500060920      * per: Smistacolli VanDerLande                                  *
000600060920      *      Postazioni Disk C                                        *
000700060920      *      Rilevatore Peso Volume Pallet                            *
000800060920      *      Picking diretto                                          *
000900060920      *---------------------------------------------------------------*
001000060920
001100060920     h decedit('0,') datedit(*YMD.)
001200160511     h dftactgrp(*no) bnddir('TRUL')
001300040422
001400060920      *---------------------------------------------------------------*
001500060920
001600060920     fAZORG01L  if   e           k disk
001700060920     fTNTBE01L  if   e           k disk
001800060920     fFNFVV01L  if   e           k disk
001900160502     fFNFVV08L  if   e           k disk    usropn
002000160502     f                                     rename( FNFVV000 : FNFVV008 )
002100090128     fFITR602L  if   e           k disk    usropn
002200060920     fFLTR901L  if   e           k disk    usropn
002300060920      *
002400090213     fFITR401L  Uf   e           K disk
002500060920      *
002600080407     fFNMSA13L  Uf A e           k disk
002700161213     fFNMSA03L  Uf A e           k disk    rename( FNMSA000 : FNMSA003 )
002800161213     f                                     prefix( SA0 : 3 )
002900080708     fFIMSC01L  Uf A e           k disk
003000060920      *
003100080108     fFIBAR00F  O    e             disk
003200060920     fFLTR700F  O    e             disk
003300160215     fFIBARDPD0FO    e             disk
003400040422
003500060920      *---------------------------------------------------------------*
003600060920
003700060920      *
003800060920      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
003900060920      *
004000060920      * - Numero Pistola Spunta colli
004100160502     d c_npsTRANSP     c                   const(46)
004200060920     d C_npsSTD        c                   const(49)
004300060920     d C_npsRPVP       c                   const(48)
004400060920     d C_npsPRES       c                   const(50)
004500060920      *
004600060920      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
004700060920      *
004800060920      * - Layout
004900160707     d $MSL            s                   dim( 80) inz  like(§MSLpep)
005000160707     d $MSLn           s                   dim( 80) inz  like(§MSLent)
005100160707     d $MSLa           s                   dim( 80) inz  like(§MSLapl)
005200161129     d $MSLa5          s                   dim( 80) inz  like(§MSLap5)
005300160707     d $MSLe           s                   dim( 80) inz  like(§MSLpke)
005400161213     d $MSLw           s                   dim( 80) inz  like(§MSLpwf)
005500060920      * - Civette
005600080619     d $MSCdat         s             15    dim(100) inz                         *segnacollo
005700060926     d $MSCpes         s                   dim(100) inz  like(§MSCpes)          *peso
005800060926     d $MSCvol         s                   dim(100) inz  like(§MSCvol)          *volume
005900060921      * - di comodo (per "raggruppare" campi legati)
006000060921     d ds_PEP          ds                  inz
006100060921     d   wPEP001                      1  0 inz
006200060921     d   wPEP002                      1  0 inz
006300060921     d   wPEP003                      1  0 inz
006400060921     d   wPEP004                      1  0 inz
006500060921     d   wPEP005                      1  0 inz
006600060921     d   wPEP006                      1  0 inz
006700060921     d   wPEP007                      1  0 inz
006800060921     d   wPEP008                      1  0 inz
006900060921     d   wPEP009                      1  0 inz
007000060921     d   wPEP010                      1  0 inz
007100060921     d   wPEP011                      1  0 inz
007200060921     d   wPEP012                      1  0 inz
007300060921     d   wPEP013                      1  0 inz
007400060921     d   wPEP014                      1  0 inz
007500060921     d   wPEP015                      1  0 inz
007600060921     d   wPEP016                      1  0 inz
007700060921     d   wPEP017                      1  0 inz
007800060921     d   wPEP018                      1  0 inz
007900060921     d   wPEP019                      1  0 inz
008000060921     d   wPEP020                      1  0 inz
008100060921     d  $PEP                   1     20  0 dim(20)
008200060921     d ds_PAD          ds                  inz
008300060921     d   wPAD001                      1  0 inz
008400060921     d   wPAD002                      1  0 inz
008500060921     d   wPAD003                      1  0 inz
008600060921     d   wPAD004                      1  0 inz
008700060921     d   wPAD005                      1  0 inz
008800060921     d   wPAD006                      1  0 inz
008900060921     d   wPAD007                      1  0 inz
009000060921     d   wPAD008                      1  0 inz
009100060921     d   wPAD009                      1  0 inz
009200060921     d   wPAD010                      1  0 inz
009300060921     d   wPAD011                      1  0 inz
009400060921     d   wPAD012                      1  0 inz
009500060921     d   wPAD013                      1  0 inz
009600060921     d   wPAD014                      1  0 inz
009700060921     d   wPAD015                      1  0 inz
009800060921     d   wPAD016                      1  0 inz
009900060921     d   wPAD017                      1  0 inz
010000060921     d   wPAD018                      1  0 inz
010100060921     d   wPAD019                      1  0 inz
010200060921     d   wPAD020                      1  0 inz
010300060921     d   wPAD021                      1  0 inz
010400060921     d   wPAD022                      1  0 inz
010500060921     d   wPAD023                      1  0 inz
010600060921     d   wPAD024                      1  0 inz
010700060921     d   wPAD025                      1  0 inz
010800060921     d   wPAD026                      1  0 inz
010900060921     d   wPAD027                      1  0 inz
011000060921     d   wPAD028                      1  0 inz
011100060921     d   wPAD029                      1  0 inz
011200060921     d   wPAD030                      1  0 inz
011300060921     d   wPAD031                      1  0 inz
011400060921     d   wPAD032                      1  0 inz
011500060921     d   wPAD033                      1  0 inz
011600060921     d   wPAD034                      1  0 inz
011700060921     d   wPAD035                      1  0 inz
011800060921     d   wPAD036                      1  0 inz
011900060921     d   wPAD037                      1  0 inz
012000060921     d   wPAD038                      1  0 inz
012100060921     d   wPAD039                      1  0 inz
012200060921     d   wPAD040                      1  0 inz
012300060921     d   wPAD041                      1  0 inz
012400060921     d   wPAD042                      1  0 inz
012500060921     d   wPAD043                      1  0 inz
012600060921     d   wPAD044                      1  0 inz
012700060921     d   wPAD045                      1  0 inz
012800060921     d   wPAD046                      1  0 inz
012900060921     d   wPAD047                      1  0 inz
013000060921     d   wPAD048                      1  0 inz
013100060921     d   wPAD049                      1  0 inz
013200060921     d   wPAD050                      1  0 inz
013300060921     d   wPAD051                      1  0 inz
013400060921     d   wPAD052                      1  0 inz
013500060921     d   wPAD053                      1  0 inz
013600060921     d   wPAD054                      1  0 inz
013700060921     d   wPAD055                      1  0 inz
013800060921     d   wPAD056                      1  0 inz
013900060921     d   wPAD057                      1  0 inz
014000060921     d   wPAD058                      1  0 inz
014100060921     d   wPAD059                      1  0 inz
014200060921     d   wPAD060                      1  0 inz
014300060921     d  $PAD                   1     60  0 dim(60)
014400060920      *
014500060920      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
014600060920      *
014700060920      * - Parametri per richiamo al pgm di controllo profilo utenti
014800060920     d TIBS34ds      e ds
014900060920      * - Ds di riferimento al file esterno AzUte00F
015000060920     d AZUTEds       e ds                  extname(AZUTE00F)
015100060920      * - Ds per dati organigramma
015200060920     d DDatiUte      e ds
015300160606      *
015400160606      * -?150ª descrizione dell'organigramma?
015500160606     d Og150         e ds                  qualified  inz
015600060920      *
015700060921      * - Tab. "MFP" = P.O. applicazioni installate
015800060921     d dMFP          e ds                  inz
015900060920      * - Tab. "MSL" = Layout Macchinone Smistacolli
016000060920     d dMSL          e ds                  inz
016100060920      * - Tab. "MSC" = Paccchi Civetta
016200060920     d dMSC          e ds                  inz
016300060925      * - - Composizione segnacollo civetta
016400080619     d   dsSEGNAC             31     45    inz(*zeros)
016500060920      * - Tab. "MPV" = Blocco caricamento volume e/o peso
016600060920     d dMVP          e ds                  inz
016700060920     d  MVP_V_TIMI             2     13  0 inz
016800060920     d  MVP_V_TIMF            14     25  0 inz
016900060920     d  MVP_P_TIMI            27     38  0 inz
017000060920     d  MVP_P_TIMF            39     50  0 inz
017100160325     d  MVP_V_TIMI_R          52     63  0 inz
017200160325     d  MVP_V_TIMF_R          64     75  0 inz
017300160325     d  MVP_P_TIMI_R          77     88  0 inz
017400160325     d  MVP_P_TIMF_R          89    100  0 inz
017500160325     d  MVP_P_TIMI_T         102    113  0 inz
017600160325     d  MVP_P_TIMF_T         114    125  0 inz
017700060920      * - Tab. "TBA" = Tipo Bancali
017800060920     d dTBA          e ds                  inz
017900060920      *
018000060920      * - Calcolo Check-Digit
018100060920     d TRUL28ds      e ds                  inz
018200060920      *
018300060920      * - Campi Formato Record
018400090128     d FITR4ds         ds                  inz
018500061012     d  TR4ctl                 1      1
018600061012     d  TR4npl                 2      3
018700061012     d  TR4nfv                 4      8
018800061012     d  TR4tim                 9     22
018900061012     d  TR4ms1                23     27
019000061012     d  TR4msx                28     28
019100061012     d  TR4ms2                29     31
019200061012     d  TR4cod                32     36
019300061012     d  TR4020                37     38
019400061012     d  TR4f01                39     39
019500061012     d  TR4pic                40     44
019600061012     d  TR4f02                45     45
019700061012     d  TR4pgr                46     51
019800061012     d  TR4f03                52     52
019900061012     d  TR4pod                53     55
020000061012     d  TR4f04                56     56
020100061012     d  TR4ods                57     57
020200061012     d  TR4f05                58     58
020300061012     d  TR4pad                59     61
020400061012     d  TR4f06                62     62
020500061012     d  TR4dts                63     63
020600061012     d  TR4f07                64     64
020700061012     d  TR4ps1                65     65
020800061012     d  TR4pn1                66     67
020900061012     d  TR4pd1                68     97
021000061012     d  TR4f08                98     98
021100061012     d  TR4ps2                99     99
021200061012     d  TR4pn2               100    101
021300061012     d  TR4pd2               102    130
021400061012     d**???                  131    131
021500061012     d  TR4f09               132    132
021600061012     d  TR4ps3               133    133
021700061012     d  TR4pn3               134    135
021800061012     d  TR4pd3               136    165
021900061012     d  TR4f10               166    166
022000061012     d  TR4ps4               167    167
022100061012     d  TR4pn4               168    169
022200061012     d  TR4pd4               170    199
022300061012     d    TR4dm1             181    184
022400061012     d    TR4dm2             185    188
022500061012     d    TR4dm3             189    192
022600061012     d   TR4dm               181    192
022700061012     d  TR4f11               200    200
022800061012     d  TR4pep               201    214
022900061012     d  TR4f12               215    215
023000061012     d  TR4pes               216    216
023100061012     d  TR4f13               217    217
023200061012     d  TR4pup               218    231
023300061012     d  TR4f14               232    232
023400061012     d  TR4pus               233    233
023500061012     d  TR4f15               234    234
023600061012     d  TR4crc               235    236
023700060920      *
023800060920      * - Segnacollo
023900060920     d wBarCode        ds                  inz
024000060920     d  wBar_xx1               1      5    inz
024100060920     d  wBar_xx2               6      6    inz
024200060920     d  wBar_LNP               7      9    inz
024300060920     d  wBar_LNA              10     12    inz
024400060920     d  wBar_NRS              13     14    inz
024500060920     d  wBar_NSC              15     21    inz
024600060920     d  wBar_ZNC              22     23    inz
024700060920     d   wBar_BAR              7     23    inz
024800060920     d  wBar_CHK              24     24    inz
024900060920     d    wBar_BarCo           7     24    inz
025000060920     d  wBar_xx3              25     25    inz
025100060920      *
025200060920     d wBarSped        ds                  inz
025300060920     d  wSpe_xx1               1      9    inz
025400060920     d  wSpe_AA2              10     11    inz
025500060920     d  wSpe_LNP              12     14    inz
025600060920     d  wSpe_NRS              15     16    inz
025700060920     d  wSpe_NSP              17     23    inz
025800060920     d  wSpe_CHK              24     24    inz
025900060920     d   wSpe_BarCo           10     24    inz
026000060920     d  wSpe_xx2              25     25    inz
026100060920      *
026200090128      * - Segnacollo x FITR6
026300060920     d kTR6who15       ds            15    inz
026400060920     d  BARlnp                             inz
026500060920     d  BARlna                             inz
026600060920     d  BARnrs                             inz
026700060920     d  BARnsc                             inz
026800060920      *
026900060920      * - Controllo data
027000060920     d WLBda8          ds                  inz
027100060920     d  G08dat                        8  0 inz
027200060920     d  G08inv                        8  0 inz
027300060928     d  G08err                        1    inz('3')
027400060920     d  G08tgi                        5  0 inz
027500060920      *
027600060920     d dsTIM           ds            14    inz                                  *data + ora
027700060920     d  dsDAT                         8  0 inz                                  *data (aaaa+mm+gg)
027800060920     d  dsORA                         4  0 inz                                  *ora  (hh+mm)
027900060920      *
028000060920      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
028100060920      *
028200060920      * - Parametri ricevuti
028300060920     d PARfil          s              3                                         *p.o. che spunta
028400060920     d wPO             s                   like(ORGfil)
028500060920      *
028600060920      * - Flags booleani
028700060920     d $CK0err         s              1    inz(*off)
028800060920     d $CK1err         s              1    inz(*off)
028900060920     d $CK2err         s              1    inz(*off)
029000060926     d $okPD1          s              1    inz(*off)
029100060926     d $okPD2          s              1    inz(*off)
029200060920     d $BarCiv         s              1    inz(*off)
029300060920      *
029400060920      * - Indici di schiera
029500060920     d xx              s              5  0 inz
029600060920      *
029700060926      * Campi chiave riferiti al data base
029800060920     d KSAapl          s                   like(MSAapl)    inz
029900060920     d KSAdtl          s                   like(MSAdtl)    inz
030000060920     d KSAorl          s                   like(MSAorl)    inz
030100070725     d KSAnpg          s                   like(MSAnpg)    inz
030200060920     d KSCdtl          s                   like(MSCdtl)    inz
030300080708     d KSCIorl         s                   like(MSCorl)    inz
030400060920     d KSCseg          s                   like(MSCseg)    inz
030500161213      *//* -?Nuovi campi (chiave, ma non ancora) riferiti al data base?
030600161213     d*// KSAimp          s                   like(SA0imp)    inz
030700161213     d*// KSAlav          s                   like(SA0lav)    inz
030800060926      *
030900060926      * Altre avriabili riferite al data base
031000060926     d wBARnfv         s                   like(BARnfv)    inz
031100060926     d wBARnpg         s                   like(BARnpg)    inz
031200060926     d wBARpuc         s                   like(BARpuc)    inz
031300060926     d wBARvuc         s                   like(BARvuc)    inz
031400060926      *
031500060921      * Variabili per conteggio dati da aggiornare in FNMSA00F
031600060920     d wPES            s                   like(MSApes)    inz
031700060920     d wVOL            s                   like(MSAvol)    inz
031800060926     d wODSdiv         s              1  0 inz                                  like(MSAodsdiv) inz
031900060926     d wODSdnr         s              1  0 inz                                  like(MSAodsdnr) inz
032000060926     d wODSdna         s              1  0 inz                                  like(MSAodsdna) inz
032100060926     d wODSftd         s              1  0 inz                                  like(MSAodsftd) inz
032200060926     d wODSids         s              1  0 inz                                  like(MSAodsids) inz
032300060926     d wODSuds         s              1  0 inz                                  like(MSAodsuds) inz
032400060926     d wODSdne         s              1  0 inz                                  like(MSAodsdne) inz
032500060926     d wODSciv         s              1  0 inz                                  like(MSAodsciv) inz
032600060926     d wODSdnq         s              1  0 inz                                  like(MSAodsdnq) inz
032700060926     d wODSust         s              1  0 inz                                  like(MSAodsust) inz
032800060926     d wDTSnts         s              1  0 inz                                  like(MSAdtsnts) inz
032900060926     d wDTStok         s              1  0 inz                                  like(MSAdtstok) inz
033000060926     d wDTSila         s              1  0 inz                                  like(MSAdtsila) inz
033100060926     d wDTSnrl         s              1  0 inz                                  like(MSAdtsnrl) inz
033200060926     d wDTSndf         s              1  0 inz                                  like(MSAdtsndf) inz
033300060926     d wPS1dnr         s              1  0 inz                                  like(MSAps1dnr) inz
033400060926     d wPS1dts         s              1  0 inz                                  like(MSAps1dts) inz
033500060926     d wPS1dtl         s              1  0 inz                                  like(MSAps1dtl) inz
033600060926     d wPS1nsc         s              1  0 inz                                  like(MSAps1nsc) inz
033700060926     d wPS1nre         s              1  0 inz                                  like(MSAps1nre) inz
033800060926     d wPS1dok         s              1  0 inz                                  like(MSAps1dok) inz
033900060926     d wPS1dno         s              1  0 inz                                  like(MSAps1dno) inz
034000060926     d wPS2dnr         s              1  0 inz                                  like(MSAps2dnr) inz
034100060926     d wPS2dts         s              1  0 inz                                  like(MSAps2dts) inz
034200060926     d wPS2dtl         s              1  0 inz                                  like(MSAps2dtl) inz
034300060926     d wPS2nsc         s              1  0 inz                                  like(MSAps2nsc) inz
034400060926     d wPS2nre         s              1  0 inz                                  like(MSAps2nre) inz
034500060926     d wPS2dok         s              1  0 inz                                  like(MSAps2dok) inz
034600060926     d wPS2dno         s              1  0 inz                                  like(MSAps2dno) inz
034700060926     d wPESnes         s              1  0 inz                                  like(MSApesnes) inz
034800060926     d wPESind         s              1  0 inz                                  like(MSApesind) inz
034900060926     d wPESune         s              1  0 inz                                  like(MSApesune) inz
035000060926     d wPUSnes         s              1  0 inz                                  like(MSApusnes) inz
035100060926     d wPUSsor         s              1  0 inz                                  like(MSApussor) inz
035200060926     d wPUSlos         s              1  0 inz                                  like(MSApuslos) inz
035300060926     d wCSD            s              1  0 inz                                  like(MSAcsd)    inz
035400060926     d wCCP            s              1  0 inz                                  like(MSAccp)    inz
035500060926     d wCCV            s              1  0 inz                                  like(MSAccv)    inz
035600060926     d wNLS            s              1  0 inz                                  like(MSAnls)    inz
035700060926     d wNLP            s              1  0 inz                                  like(MSAnlp)    inz
035800161213     d wPWF            s              1  0 inz                                  like(MSApwf)    inz
035900060920      *
036000060920      * - Campi di comodo
036100060926     d wPD0ok          s              3    inz
036200060926     d wPD1ok          s              3    inz
036300060926     d wPD2ok          s              3    inz
036400060926     d wTR7pgr         s              6    inz
036500060926     d wTR7pad2        s              2    inz
036600060926     d wTR4pad         s              3    inz
036700060926     d wSPU_time       s             12  0 inz
036800060920     d wScan           s              5  0 inz
036900060920     d wScan1          s              5  0 inz
037000060920     d wScan2          s              5  0 inz
037100060920     d wDM1            s              4  3 inz
037200060920     d wDM2            s              4  3 inz
037300060920     d wDM3            s              4  3 inz
037400060920     d w003            s              3  0 inz
037500060920     d w006            s              6  0 inz
037600061009     d w096            s              9  6 inz
037700080108     d w140            s             14  0 inz
037800080619     d wtr7bar         s             15    inz
037900160502
038000160502       //--------------------------------------------------------------
038100160502       //?Definizione prototipi procedure.                             ?
038200160502       //--------------------------------------------------------------
038300160502
038400160511       // -?Caricamento £x?
038500160511     d ds_L1           ds                  inz
038600160511     d   sk_L1                        3    dim(30)  inz
038700160511     d TRUL06ds      e ds                  inz
038800160511     d KPJBA         e ds                  inz
038900160511      /copy gaitrasrc/srcProtoPR,TRUL06R
039000160502
039100060920      *
039200060920      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
039300060920      *
039400160502       // -?File FNFVV08L?
039500160502     d keyFNFVV08    e ds                  extname( FNFVV08L : *key )
039600160502     d                                     prefix( k_ )   inz
039700161213      * - FNMSA13L e FNMSA03L
039800070725     c     K05MSA03      klist                                                  *fnmsa03l
039900070725     c                   kfld                    KSAapl                          -tipo applicazione
040000060920     c                   kfld                    wPO                             -punto operativo
040100060920     c                   kfld                    KSAdtl                          -data
040200060920     c                   kfld                    KSAorl                          -ora
040300070725     c                   kfld                    wBARnpg                         -categoria foglio
040400080619      * - FIMSC01L
040500080619     c     K04IMSC01     klist                                                  *fimsc01l
040600080619     c                   kfld                    wPO                             -punto operativo
040700080619     c                   kfld                    KSCdtl                          -data
040800080619     c                   kfld                    KSCIorl                         -ora
040900080619     c                   kfld                    KSCseg                          -segnacollo
041000060920      * - FNFVV01L
041100060921     c     K03FVV01      klist                                                  *fnfvv01l
041200060921     c                   kfld                    wBARnpg                         -categoria foglio
041300060921     c                   kfld                    FVVnfv                          -numero foglio
041400060920     c                   kfld                    wPO                             -filiale gestione
041500060920      * - FLTR901L
041600060921     c     K04TR901      klist
041700060920     c                   kfld                    wPO
041800060920     c                   kfld                    wBarNPG
041900060921     c                   kfld                    TR9nfv
042000060921     c                   kfld                    TR9pep
042100060920      * - TNTBE01L
042200060921     c     K02TBE01      klist                                                  *tntbe01l
042300060920     c                   kfld                    TBEcod                          -tabella
042400060920     c                   kfld                    TBEke1                          -chiave uno
042500090129      * - FITR602L
042600090129     c     K02tr6        klist
042700090129     c                   kfld                    tr4fil
042800090129     c                   kfld                    tr4pc
042900090129     c                   kfld                    ktr6who15
043000060920
043100060920      *===============================================================*
043200060920      *?  I N D I C A T O R I                                        ?*
043300060920      *---------------------------------------------------------------*
043400060920      *  10 - Comodo                                                  *
043500060920      *  11 -   "                                                     *
043600060920      *  12 -   "                                                     *
043700060920      *  88 - Utilizzare *** SOLO *** per write FLTR7                 *
043800060920      *  96 - Utilizzare *** SOLO *** per correttezza check digit     *
043900060920      *  97 - Utilizzare *** SOLO *** per test baia entrata tabellata *
044000090128      *  98 - Utilizzare *** SOLO *** per letture record FITR4        *
044100060920      *  99 - Utilizzare *** SOLO *** per correttezza barcode         *
044200060920      *===============================================================*
044300060920
044400060920      *===============================================================*
044500060920      *?  N O T E                                                    ?*
044600060920      *---------------------------------------------------------------*
044700060920      * Il campo TR4020 indica il tipo di impianto da cui si ricevono *
044800060920      *   i rcd:                                                      *
044900060920      * · "20" VDL                                                    *
045000060920      * · "21" DKTC                                                   *
045100060920      * · "22" RPVP - Tipo non gestito                                *
045200060920      * · "23" PICKING VDL PC VDL                                     *
045300170125      * · "24" PICKING NO VDL - Barcode BRT                           *
045400060920      * · "25" PICKING NO VDL - Barcode spedizione                    *
045500170125      * · "26" PICKING VDL PC NON VDL - Barcode BRT                   *
045600060920      * · "27" PICKING VDL PC NON VDL - Barcode spedizione            *
045700170125      * · "36" PICKING VDL PC NON VDL - Barcode Picking DPD           *
045800060920      * · "2C" RPVP - Tipo 'C' collo peso lordo                       *
045900060920      * · "2K" RPVP - Tipo 'K' collo peso netto                       *
046000160502      * · "2T" TRANSPALLET Pesatore WiFi                              *
046100060920      *===============================================================*
046200031222
046300060920     c     *Entry        plist
046400060920     c                   parm                    PARfil                         *filiale che spunta
046500090128     c                   parm                    flag              1
046600090128     c                   parm                    parspu            1
046700090128
046800090128     c                   clear                   flag
046900090128     c                   clear                   parspu
047000060920      *
047100060920      * Operazioni Iniziali
047200060920     c                   exsr      RoutInz
047300060920      *
047400060920      * Ciclo di lettura spunte
047500090213     c     parfil        setll     fitr4
047600090213     c     parfil        reade     FITR4                                  98
047700060920      *
047800000000do  1c                   DOW       NOT *in98
047900060920      *
048000060920      * - Valorizzazione campi del file storico (FLTR700F)
048100060920     c                   exsr      sr_RieTR7
048200060920      *
048300060920      * - Scrittura dello STORICO
048400060920     c                   WRITE     FLTR7000                             88
048500060926      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
048600160610      * - Scrittura del Segnacollo DPD spuntato senza rec. bolla
048700160610      *   · SE LETTURA SCANNER NON VALIDA        E
048800160610      *   · SE LETTURA PISTOLA = "CHI SONO" DPD
048900160629      *   (solo per Tipo Lavorazione = 5)
049000160610     c                   if             *in88  = *off
049100160610     c                             and  TR7cod = *zeros
049200160629     c                             and  TR7ctl = '5'
049300160610     c                             and (%scan('????' : TR7pd1)
049400160610     c                                         > *zeros       OR
049500160610     c                                  TR7pd1 = *blanks      OR
049600160610     c                                  wPD1ok = 'ERR')
049700160610     c                             and  %subst(TR7pd2:  1: 2) = ' %'
049800160610     c                             and  %subst(TR7pd2: 30: 1) = '#'
049900160610     c                             and  %subst(TR7pd2: 10: 4) <>
050000160610     c                                           '0000'
050100160610     c                   exsr      Wrt_BARDPD
050200160610     c                   endif
050300160610      *
050400060920      * - Scrittura dei SALDI
050500060925if  2c                   if             *in88  = *off
050600060920     c                             and  TR4cod = *zeros
050700060920     c                             and (TR4ctl = '2'
050800161214     c                              or  TR4ctl = '3'
050900060920     c                              or  TR4ctl = '5'
051000060920     c                              or  TR4ctl = '6')
051100060920     c                   exsr      Wrt_SAL
051200060920e   2c                   endif
051300060920      *
051400060920      * - Cancellazione record
051500090128     c                   DELETE    FITR4
051600060926      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051700060920      * - Lettura record successivo spunte
051800090213     c     parfil        reade     FITR4                                  98
051900060920      *
052000060920e   1c                   ENDDO
052100060920      *
052200060920      * Fine
052300090128     c                   feod      fibar00f
052400160219     c                   feod      FIBARDPD0F
052500090128     c                   shtdn                                        95
052600090128     c                   if        not *in95
052700090128     c                   seton                                        rt
052800090128     c                   else
052900090128     c                   eval      flag='1'
053000090128     c                   seton                                        lr
053100090128     c                   endif
053200060921
053300060921      *---------------------------------------------------------------*
053400060921      *?Operazioni iniziali                                          ?*
053500060921      *---------------------------------------------------------------*
053600060921     c     RoutInz       BEGSR
053700060921      *
053800060921      * Reperimento dati job
053900060921     c                   exsr      DatiJob
054000060921      *
054100060921     c                   move      PARfil        wPO                            *parametro numerico
054200160606      *
054300160606     c     wPO           chain     AZORG
054400160606if  1c                   if        %found(AZORG01L)
054500160606     c                   eval      Og150 = ORGdf0
054600160606e   1c                   endif
054700060921      *
054800060921      * Caricamento tabelle occorrenti
054900060925     c                   exsr      CarTAB
055000060921      *
055100060921     c                   ENDSR
055200060921
055300060921      *---------------------------------------------------------------*
055400060921      *?Reperimento dati del job (Utente/Operativi)                  ?*
055500060921      *---------------------------------------------------------------*
055600060921     c     DatiJob       BEGSR
055700060921      *
055800060921     c     *dtaara       define    §azute        AZUTEds
055900060921     c     *dtaara       define    §datiute      dDatiUte
056000060921      *
056100060921     c                   in(E)     *dtaara
056200060921     c                   if        %error or RSUT = *blanks
056300060928     c                   clear                   TIBS34ds
056400060921     c                   call      'TIBS34R'
056500060921     c                   parm                    TIBS34ds
056600060921     c                   in        *dtaara
056700060921     c                   endif
056800060921      *
056900060921     c                   ENDSR
057000060921
057100060921      *---------------------------------------------------------------*
057200060921      *?Caricamento tabelle occorrenti                               ?*
057300060921      *---------------------------------------------------------------*
057400060921     c     CarTAB        BEGSR
057500060921      *
057600060921      * Tab. MVP = Blocco caricamento peso e/o volume
057700060921     c                   clear                   dMVP
057800060921     c                   eval      TBEcod    = 'MVP'
057900060921     c                   movel(p)  wPO           TBEke1
058000060921     c     K02TBE01      chain     TNTBE000
058100060921if  1c                   if             %found(TNTBE01L)
058200060921     c                             and  TBEatb = *blanks
058300060921     c                   movel     TBEuni        dMVP
058400060921e   1c                   endif
058500060921      *
058600060921      * Tab. MSL = Layout macchinone smistacolli
058700060921     c                   clear                   $MSL
058800060921     c                   clear                   $MSLn
058900060921     c                   clear                   $MSLa
059000161129     c                   clear                   $MSLa5
059100060921     c                   clear                   $MSLe
059200161213     c                   clear                   $MSLw
059300060921     c                   clear                   xx
059400060921     c                   eval      TBEcod    = 'MSL'
059500060921     c                   movel(p)  wPO           TBEke1
059600060921     c     K02TBE01      setll     TNTBE000
059700060921     c     K02TBE01      reade     TNTBE000
059800060921do  1c                   DOW       NOT %eof(TNTBE01L)
059900060921      * - Solo rec. non annullati e del p.o.
060000060921if  2c                   if             TBEatb = *blanks
060100060921     c                             and  TBEke1 = PARfil
060200060921     c                   add       1             xx
060300060921     c                   movel     TBEuni        dMSL
060400060921     c                   eval      $MSL(xx)  = §MSLpep                          *filiale + entrata
060500060921     c                   eval      $MSLn(xx) = §MSLent                          *n° entrance point
060600161129     c                   eval      $MSLa(xx) = §MSLapl                          *TipoApplic. Cat.2
060700161129     c                   eval      $MSLa5(xx) = §MSLap5                         *TipoApplic. Cat.5
060800161129     c                   eval      $MSLe(xx) = §MSLpke                          *S=Entrata Picking
060900161213     c                   eval      $MSLw(xx) = §MSLpwf                          *W=Pistola Wi-Fi
061000060921e   2c                   endif
061100060921     c     K02TBE01      reade     TNTBE000
061200060921e   1c                   ENDDO
061300060921      *
061400060921      * Tab. MSC = Pacchi civetta
061500060921     c                   clear                   $MSCdat
061600060921     c                   clear                   $MSCpes
061700060921     c                   clear                   $MSCvol
061800060921     c                   clear                   xx
061900060921     c                   eval      TBEcod    = 'MSC'
062000060921     c     TBEcod        setll     TNTBE000
062100060921     c     TBEcod        reade     TNTBE000
062200060921do  1c                   DOW       NOT %eof(TNTBE01L)
062300060921if  2c                   if        TBEatb    = *blanks
062400060921     c                   add       1             xx
062500060921     c                   movel     TBEuni        dMSC
062600060921     c                   movel     dsSEGNAC      $MSCdat(xx)                    *segnacollo
062700060921     c                   eval      $MSCpes(xx) = §MSCpes                        *peso
062800060921     c                   eval      $MSCvol(xx) = §MSCvol                        *volume
062900060921e   2c                   endif
063000060921     c     TBEcod        reade     TNTBE000
063100060921e   1c                   ENDDO
063200060921      *
063300060921      * Tipo applicazione principale del p.o.
063400060921     c                   clear                   dMFP
063500060921     c                   eval      TBEcod    = 'MFP'
063600060921     c                   movel(p)  wPO           TBEke1
063700060921     c     K02TBE01      chain     TNTBE000
063800060921if  1c                   if             %found(TNTBE01L)
063900060921     c                             and  TBEatb = *blanks
064000060921     c                   movel     TBEuni        dMFP
064100060921e   1c                   endif
064200060921      *
064300060921      * Tara bancali per RPVP
064400060921     c                   clear                   dTBA
064500060921     c                   eval      TBEcod    = 'TBA'
064600060921     c                   movel(p)  '01'          TBEke1
064700060921     c     K02TBE01      chain     TNTBE000
064800060921if  1c                   if             %found(TNTBE01L)
064900060921     c                             and  TBEatb = *blanks
065000060921     c                   movel     TBEuni        dTBA
065100060921e   1c                   endif
065200160502      *
065300160502      /free
065400160502
065500160502         // -?Reperimento (temporaneamente) Data & Ora correnti?
065600160502         dsTIM = %subst( %char( %dec( %timestamp() ) )
065700160502                         : 1 : 14 );
065800160502
065900160511         // -?Reperimento Filiali dell'Area ("£1")?
066000160511         clear  sk_L1;
066100160511         clear  KPJBU;
066200160511         clear  TRUL06ds;
066300160511         D06cod = '£1';
066400160511         D06key = PARfil;
066500160511         D06tla = 'L';
066600160511         D06drf = dsDAT;
066700160511         KPJBU  = TRUL06ds;
066800160511         trul06r ( KPJBA );
066900160511         TRUL06ds = KPJBU;
067000160511         ds_L1  = D06lia;
067100160502
067200160502      /end-free
067300060921      *
067400060921     c                   ENDSR
067500060920
067600060920      *---------------------------------------------------------------*
067700060920      *?Caricamento dati nel record di FLTR7                         ?*
067800060920      *---------------------------------------------------------------*
067900060920     c     sr_RieTR7     BEGSR
068000060920      *
068100060920     c                   eval      *in88   = *off
068200060920     c                   eval      *in99   = *off
068300060920     c                   eval      *in96   = *off
068400060920     c                   eval      $CK0err = *off
068500060920     c                   eval      $CK1err = *off
068600060920     c                   eval      $CK2err = *off
068700060926     c                   clear                   wPD0ok
068800060926     c                   clear                   wPD1ok
068900060926     c                   clear                   wPD2ok
069000060920     c                   clear                   BarCo
069100060920      *
069200060920      * Suddivide il formato record nei campi della DS
069300090128     c                   movel     TR4da1        FITR4ds
069400060920      *
069500060920      * Imposta i campi fissi del barcode
069600070212     c                   eval      BARerr  = ' '
069700070212     c                   eval      BARman  = ' '
069800070212     c                   eval      BARdup  = ' '
069900070212     c                   eval      BARtap  = ' '
070000060920     c                   clear                   BARcan
070100060920     c                   clear                   BAR0af
070200031222      *
070300060920      * Valorizza i campi del file storico
070400060920     c                   clear                   FLTR7000
070500060920     c                   eval      TR7ctl  = TR4ctl
070600060920     c                   eval      TR7npl  = TR4npl
070700060920     c                   eval      TR7nfv  = TR4nfv
070800060920     c                   eval      TR7mil  = TR4ms1 + TR4ms2
070900060920     c                   eval      TR7cod  = TR4cod
071000060920     c                   eval      TR7020  = TR4020
071100060920     c                   eval      TR7pic  = TR4pic
071200060920     c                   eval      TR7pgr  = TR4pgr
071300060920     c                   eval      TR7pod  = TR4pod
071400060920     c                   eval      TR7ods  = TR4ods
071500060920     c                   eval      TR7pad  = TR4pad
071600060920     c                   eval      TR7dts  = TR4dts
071700060920     c                   eval      TR7ps1  = TR4ps1
071800060920     c                   eval      TR7pn1  = TR4pn1
071900060920     c                   eval      TR7pd1  = TR4pd1
072000060920     c                   eval      TR7ps2  = TR4ps2
072100060920     c                   eval      TR7pn2  = TR4pn2
072200060928     c                   move      TR4pd2        TR7pd2
072300060920     c                   eval      TR7ps3  = TR4ps3
072400060920     c                   eval      TR7pn3  = TR4pn3
072500060920     c                   eval      TR7pd3  = TR4pd3
072600060920     c                   eval      TR7ps4  = TR4ps4
072700060920     c                   eval      TR7pn4  = TR4pn4
072800060920     c                   eval      TR7pd4  = TR4pd4
072900060920     c                   eval      TR7pep  = TR4pep
073000060920     c                   eval      TR7pes  = TR4pes
073100060920     c                   eval      TR7pup  = TR4pup
073200060920     c                   eval      TR7pus  = TR4pus
073300060920     c                   eval      TR7crc  = TR4crc
073400060928     c                   eval      TR7vol  = *zeros
073500031223      * P.O. che spunta
073600060925     c                   move      wPO           TR7fil
073700031222      *
073800060920      * Controlla data e ora spunta, se non corretto utilizza *DATE
073900060920     c                   exsr      Ctr_TIME
074000160502      *
074100160502      * -?Gestione apparato TRANSPALLET: desunzione Tipo e Numero F.V.?
074200160502     c                   if        TR4020  = '2T'
074300160502     c                   exsr      sr_Transpallet
074400160502     c                   endif
074500031222      *
074600060920      * Imposta la data e ora spunta
074700060920     c                   movel     TR7tim        BARdfs
074800060920     c                   move      TR7tim        w006
074900060925     c                   movel     w006          BARhms
075000071203     c                   movel     TR7tim        wSPU_time
075100070212      * Controlla se dati numerici per millisecondi
075200070212     c                   eval      *in10   = *off
075300070212     c                   testn                   tr4ms2               10
075400070212if  1c                   if             *in10 = *off
075500070212     c                             or   %subst(tr4ms2:3:1) < *zero
075600070212     c                   movel     000           BARmis
075700070212     c                   else
075800070212     c                   movel     tr4ms2        BARmis
075900070212     c                   endif
076000070212      *
076100070212      * Profilo utente
076200070212     c                   eval      BARpru='VDL'+ parfil
076300031222      *
076400060920      * Controlla validità lettura scanner
076500060920     c                   move      TR4pd1        wBarCode
076600060920     c                   move      TR4pd1        wBarSped
076700060920     c                   exsr      Ctr_BarCod
076800060926     c                   eval      wPD1ok  = wPD0ok
076900060920     c                   eval      $CK1err = $CK0err
077000031222      *
077100060920      * Controlla validità lettura laser gun
077200060920     c                   move      TR4pd2        wBarCode
077300060920     c                   move      TR4pd2        wBarSped
077400060920     c                   exsr      Ctr_BarCod
077500060926     c                   eval      wPD2ok  = wPD0ok
077600060920     c                   eval      $CK2err = $CK0err
077700031222      *
077800031222      * Ai fini del caricamento spunte utilizzo la lettura dello
077900060920      * scanner tranne nel caso sia "blank" o contenga "???" od 8 zeri.
078000031222      * Ma in ogni caso memorizzo i dati del barcode solo se "validi"
078100041005      * a tutti gli effetti.
078200060920      * Se trovo errori nel barcode accendo *in99.
078300060920      * Se l'errore è dovuto al check digit accendo *in96.
078400060920     c                   movel     *zeros        wBarCode
078500060920     c                   eval      wScan1  = %scan('??' : TR4pd1)
078600060920     c                   eval      wScan2  = %scan('??' : TR4pd2)
078700031222      *
078800060920sel 1c                   SELECT
078900060920      * - Controlla validità lettura Scanner
079000060920w   1c                   WHEN           wScan1  = *zeros
079100060920     c                             and  TR4pd1 <> *blanks
079200060920     c                             and  %subst(TR4pd1:22:8) <> *zeros
079300060926if  2c                   if        wPD1ok  = 'BAR'
079400060920     c                   move      TR4pd1        wBarCode
079500060920x   2c                   else
079600060920     c                   eval      *in96   = ($CK1err = *on)
079700060920     c                   eval      *in99   = *on
079800060920e   2c                   endif
079900060920      * - Controlla validità lettura LaserGun
080000060920w   1c                   WHEN           wScan2  = *zeros
080100060920     c                             and  TR4pd2 <> *blanks
080200060920     c                             and  %subst(TR4pd2:22:8) <> *zeros
080300060926     c                   move      TR4pd2        wBarCode                       ?????
080400060926if  2c                   if        wPD2ok  = 'BAR'
080500060920     c                   move      TR4pd2        wBarCode
080600060920x   2c                   else
080700060920     c                   eval      *in96   = ($CK2err = *on)
080800060920     c                   eval      *in99   = *on
080900060920e   2c                   endif
081000060920      * - ? ? ?
081100060920x   1c                   OTHER
081200060920     c                   eval      *in99   = *on
081300060920e   1c                   ENDSL
081400031222      *
081500060920      * Memorizza barcode elaborato per spunta nello storico
081600060920     c                   move      wBAR_bar      TR7bar
081700031222      *
081800060920      * Imposta campi BARCODE
081900060920     c                   if        *in99   = *off
082000060920     c                   movel     wBAR_lnp      BARlnp
082100060920     c                   movel     wBAR_lna      BARlna
082200060920     c                   movel     wBAR_nrs      BARnrs
082300060920     c                   movel     wBAR_nsc      BARnsc
082400060920     c                   movel     wBAR_znc      BARznc
082500060920     c                   movel     wBAR_chk      BARctr
082600060920     c                   endif
082700031222      *
082800060920      * Controlla se è un pacco TEST
082900060920     c                   eval      $BarCiv = *off
083000060920if  1c                   if        TR7bar > *zeros
083100080619     c                   eval      wtr7bar = %subst(tr7bar:1:15)
083200080619     c     wTR7bar       lookup    $MSCdat                                11
083300060920     c                   eval      $BarCiv = *in11
083400060920e   1c                   endif
083500040421      *
083600060920      * Controlla il numero foglio
083700060920      * e memorizza in campo di comodo per saldi
083800060920     c                   eval      *in10   = *off
083900060920     c                   testn                   TR4nfv               10
084000060920if  1c                   if             *in10  = *on
084100060920     c                             and  %subst(TR4nfv: 5: 1) >= *zero
084200060920     c                             and  TR4nfv > *zeros
084300060920     c                   clear                   BARnfv
084400060920     c                   move      TR4nfv        BARnfv
084500060920     c                   eval      wBARnfv = BARnfv
084600060920x   1c                   else
084700060920     c                   eval      *in99   = *on
084800060920e   1c                   endif
084900050831      *
085000060920      * Controlla la categoria foglio
085100060920      * e memorizza in campo di comodo x saldi
085200060920if  1c                   if        TR4ctl  > *zeros
085300060920     c                   move      TR4ctl        BARnpg
085400060920x   1c                   else
085500060920     c                   clear                   BARnpg
085600060920e   1c                   endif
085700060920     c                   eval      wBARnpg = BARnpg
085800060920      *
085900060920      * Calcola il volume
086000060920     c                   clear                   wDM1
086100060920     c                   clear                   wDM2
086200060920     c                   clear                   wDM3
086300061009     c                   clear                   w096
086400060920     c                   clear                   BARvuc
086500060920     c                   eval      *in10   = *off
086600060925     c                   testn                   TR4dm                10
086700060920if  1c                   if             *in10  = *on
086800060920     c                             and  %subst(TR4dm :12: 1) >= *zero
086900060920     c                   move      TR4dm1        wDM1
087000060920     c                   move      TR4dm2        wDM2
087100060920     c                   move      TR4dm3        wDM3
087200060920e   1c                   endif
087300060920      *
087400060920      * sviluppa il volume
087500061009      * e testa se di solo 1 cifra intera...
087600060920if  1c                   if             wDM1   > *zeros
087700060920     c                             and  wDM2   > *zeros
087800060920     c                             and  wDM3   > *zeros
087900061009     c                   eval      w096    = wDM1 * wDM2 * wDM3
088000061009     c                   if        w096    < 10
088100061009     c                   z-add     w096          BARvuc
088200061009     c                   else
088300061009     c                   clear                   BARvuc
088400061009     c                   endif
088500061009     c                   move      BARvuc        TR7vol
088600060920e   1c                   endif
088700060920      *
088800060920      * controlla il peso
088900060925     c                   eval      wTR7pgr = %xlate(' ':'0':TR7pgr)
089000060920     c                   eval      *in10   = *off
089100060925     c                   testn                   wTR7pgr              10
089200060925if  1c                   if             *in10   = *on
089300060925     c                             and  wTR7pgr > *zeros
089400060925     c                             and  %subst(wTR7pgr : 6: 1) >= *zero
089500060925     c                   movel     wTR7pgr       w006
089600071011      * - RPVP peso in decagrammi, ALTRI impianti peso in grammi
089700071011w   1c                   IF             TR4020 = '22'
089800071011     c                             or   TR4020 = '2C'
089900071011     C                             or   TR4020 = '2K'
090000071011     c                   eval      BARpuc  = w006 / 100
090100071011x   1c                   else
090200071011     c                   eval      BARpuc  = w006 / 1000
090300071011e   1c                   endif
090400071011      *
090500060920x   1c                   else
090600060920     c                   clear                   BARpuc
090700060920e   1c                   endif
090800031222      *
090900031222      *  Per RPVP TIPO "K" collo netto tolgo la tara a peso e volume
091000060920if  1c                   IF        TR4020  = '2K'
091100060920if  2c                   if        BARpuc  > §TBApkg
091200060920     c                   eval      BARpuc  = BARpuc - §TBApkg
091300060920x   2c                   else
091400060920     c                   clear                   BARpuc
091500060920e   2c                   endif
091600060920if  2c                   if        BARvuc  > §TBAvol
091700060920     c                   eval      BARvuc  = BARvuc - §TBAvol
091800060920x   2c                   else
091900050721     c                   clear                   BARvuc
092000060920e   2c                   endif
092100060920e   1c                   ENDIF
092200041220      *
092300160325      *?Blocco caricamento peso e/o volume, gestito da tabella MVP?
092400160325     c                   exsr      sr_BloccoMVP
092500040421      *
092600060920      * Memorizza in campo di comodo x saldi  il peso e il volume
092700060920     c                   eval      wBARpuc = BARpuc
092800060920     c                   eval      wBARvuc = BARvuc
092900031222      *
093000060920      * Imposta Pistole
093100060920sel 1c                   SELECT
093200060920      * - VDL - pistola standard
093300060920w   1c                   WHEN      TR4020  = '20'
093400060920     c                   eval      BARnps  = C_npsSTD
093500070212     c                   eval      BARtap  = 'V'
093600170125      * - DISK C - barcode LaserGun BRT pistola standard
093700060920w   1c                   WHEN           TR4020  = '21'
093800060926     c                             and  wPD2ok  = 'BAR'
093900060920     c                   eval      BARnps  = C_npsSTD
094000070212     c                   eval      BARtap  = 'V'
094100170125      * - DISK C - barcode LaserGun non BRT pistola "presunta"
094200060920w   1c                   WHEN           TR4020  = '21'
094300060926     c                             and  wPD2ok <> 'BAR'
094400060920     c                   eval      BARnps  = C_npsPRES
094500070212     c                   eval      BARtap  = 'V'
094600060920      * - PICKING - pistola standard
094700060920w   1c                   WHEN           TR4020 = '23'
094800060920     c                             or   TR4020 = '24'
094900060920     c                             or   TR4020 = '25'
095000060920     c                             or   TR4020 = '26'
095001170128     c                             or   TR4020 = '36'
095100060920     c                             or   TR4020 = '27'
095200060920     c                   eval      BARnps  = C_npsSTD
095300070212     c                   eval      BARtap  = 'V'
095400060920      * - RPVP - pistola apposita
095500060920w   1c                   WHEN           TR4020 = '22'
095600060920     c                             or   TR4020 = '2C'
095700060920     C                             or   TR4020 = '2K'
095800060920     c                   eval      BARnps  = C_npsRPVP
095900070212     c                   eval      BARtap  = 'R'
096000160502      * -?TRANSPALLET pesatore WiFi - pistola apposita?
096100160502w   1c                   WHEN      TR4020  = '2T'
096200160502     c                   eval      BARnps  = c_npsTRANSP
096300160502     c                   eval      BARtap  = 'T'
096400060921      * - Per sicurezza...
096500060920x   1c                   OTHER
096600060925     c                   eval      BARnps  = C_npsSTD
096700070212     c                   eval      BARtap  = 'V'
096800060920e   1c                   ENDSL
096900060920      *
097000060920      * Gestione BarCode
097100060920sel 1c                   SELECT
097200060920      * - Per lettura non valida causata dal check digit
097300060920      *   imposta il flag spunta errata = "D" e
097400060920      *   NON scrive il barcode
097500060920w   1c                   WHEN      *in96   = *on
097600060920     c                   eval      TR7crc  = ' D'
097700060920      * - Per lettura non valida NON causata dal check digit
097800160502      *  ?o per lettura di TRANSPALLET senza reperimento del F.V.?
097900060920      *   imposta il flag spunta errata = "E" e
098000060920      *   NON scrive il barcode
098100060920w   1c                   WHEN      *in99   = *on
098200060920     c                   eval      TR7crc  = ' E'
098300060920      * - Per codice record inizio o fine lavorazione
098400060920      *   NON scrive il barcode
098500060920w   1c                   WHEN      TR7cod <> *zeros
098600060920      * - Per tipo foglio viaggio non valido
098700060920      *   NON scrive il barcode
098800060920w   1c                   WHEN      TR7ctl  < '1'
098900060920      * - Per pacco di test peso/volume
099000060920      *   NON scrive il barcode
099100060920w   1c                   WHEN      $BarCiv = *on
099200060920      * > SCRIVE barcode
099300060920x   1c                   OTHER
099400060920      *                  _______________
099500060920     c                   WRITE     BARCO
099600090212     c                   eval      parspu='S'
099700060920      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
099800160606      * - Se Spunta di categoria "2-Arrivi"  e
099900160705      *   se filiale abilitata  e
100000170125      *   se record da PICKING VDL PC NON VDL spunta barcode BRT
100100160606      *    o record da PICKING VDL PC VDL e il collo risulta SMISTATO
100200160705      *    o record da PICKING NO VDL
100300160606      *   verifica se scrivere anche la spunta "4-uscita"
100400160606if  2c                   IF        BARnpg = 2
100500160705     c                             and   Og150.§OGagSpUs = 'S'
100600160606     c                             and   (TR7020 = '26'
100601170128     c                                or  TR7020 = '36'
100700160519     c                                or (TR7020 = '23' and TR7pus = '1')
100800160705     c                                or  TR7020 = '24')
100900160606      *
101000060920     c                   clear                   BARnfv
101100060920     c                   clear                   BARnpg
101200060920     c                   clear                   BARnps
101300060920     c                   clear                   BARvuc
101400060920     c                   clear                   BARpuc
101500160518      *
101600160606sel 3c                   select
101700170128w   3c                   when      TR7020  = '26' or
101701170128w   3c                             TR7020  = '36'
101800060920     c                   exsr      BarCo_PK26
101900160606w   3c                   when      TR7020  = '24'
102000160518     c                   exsr      BarCo_PK24
102100160518x   3c                   other
102200060920     c                   exsr      BarCo_PK23
102300160518e   3c                   endSL
102400060920      * - Se FV impostato scrivo spunta imposto pistola "presunta"
102500060920if  3c                   if        BARnfv  > *zeros
102600060920     c                   eval      BARnps  = C_npsPRES
102700060920      *                  _______________
102800060920     c                   WRITE     BARCO
102900090212     c                   eval      parspu='S'
103000060920      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
103100060920e   3c                   endif
103200160606      *
103300060920e   2c                   ENDIF
103400040421      *
103500060920e   1c                   ENDSL
103600060920      *
103700060920     c                   ENDSR
103800160502
103900160502      /free
104000160502
104100160502       //--------------------------------------------------------------
104200160502       //?Gestione apparato TRANSPALLET: desunzione Tipo e Numero F.V. ?
104300160502       //--------------------------------------------------------------
104400160502       BEGSR  sr_Transpallet;
104500160502
104600160502         // -?Tipo F.V.?
104700160511         evalR wBarCode = TR4pd1;
104800160511
104900160502         if  wBar_LNP = PARfil  or
105000160511             %lookup( wBar_LNP : sk_L1 ) > *zero;
105100160502           TR4ctl = '5';
105200160502         else;
105300160502           TR4ctl = '2';
105400160502         endif;
105500160502         TR7ctl = TR4ctl;
105600160502
105700160502         // -?Numero F.V.?
105800160502         dsTIM    = TR7tim;
105900160502
106000160502         if  Not %open( FNFVV08L );
106100160502           open  FNFVV08L;
106200160502         endif;
106300160502
106400160502         clear  keyFNFVV08;
106500160502         k_FVVfgs = wPO;
106600160502         k_FVVnpg = %int( TR7ctl );
106700160502         k_FVVdfv = dsDAT;
106800160502         setll  %kds( keyFNFVV08 )  FNFVV008;
106900160502         reade  %kds( keyFNFVV08 )  FNFVV008;
107000160502
107100160502         doW  Not %eof( FNFVV08L )  and
107200160502              ( FVVatb <> *blank    or
107300160502                FVVfcf <> *blank );
107400160502           reade  %kds( keyFNFVV08 )  FNFVV008;
107500160502         enddo;
107600160502
107700160502         if  %eof( FNFVV08L );
107800160502           TR4nfv = *all'9';
107900160502           *in99  = *on;
108000160502         else;
108100160502           TR4nfv = %editc( FVVnfv : 'X' );
108200160502         endif;
108300160502         TR7nfv = TR4nfv;
108400160502
108500160502       ENDSR;
108600160502
108700160502      /end-free
108800160325
108900160325      *---------------------------------------------------------------*
109000160325      *?Blocco caricamento peso e/o volume, gestito da tabella MVP   ?*
109100160325      *---------------------------------------------------------------*
109200160325     c     sr_BloccoMVP  BEGSR
109300160325      *
109400160325sel 0c                   Select
109500160325      * -?RPVP?
109600160325w   0c                   When           TR4020     = '22'
109700160325     c                             or   TR4020     = '2C'
109800160325     c                             or   TR4020     = '2K'
109900160325      * -?Volume?
110000160325if  1c                   if             §MVPvblR   = 'S'
110100160325     c                             and  BARvuc     > *zeros
110200160325     c                             and  wSPU_time >= MVP_V_timI_R
110300160325     c                             and  wSPU_time <= MVP_V_timF_R
110400160325     c                   clear                   BARvuc
110500160325e   1c                   endif
110600160325      * -?Peso?
110700160325if  1c                   if             §MVPpblR   = 'S'
110800160325     c                             and  BARpuc     > *zeros
110900160325     c                             and  wSPU_time >= MVP_P_timI_R
111000160325     c                             and  wSPU_time <= MVP_P_timF_R
111100160325     c                   clear                   BARpuc
111200160325e   1c                   endif
111300160325      * -?TRANSPALLET?
111400160325w   0c                   When           TR4020     = '2T'
111500160325      * -?Peso?
111600160325if  1c                   if             §MVPpblT   = 'S'
111700160325     c                             and  BARpuc     > *zeros
111800160325     c                             and  wSPU_time >= MVP_P_timI_T
111900160325     c                             and  wSPU_time <= MVP_P_timF_T
112000160325     c                   clear                   BARpuc
112100160325e   1c                   endif
112200160325      *
112300160325      * -?VDL?
112400160325x   0c                   Other
112500160325      * -?Volume?
112600160325if  1c                   if             §MVPvbl    = 'S'
112700160325     c                             and  BARvuc     > *zeros
112800160325     c                             and  wSPU_time >= MVP_V_timI
112900160325     c                             and  wSPU_time <= MVP_V_timF
113000160325     c                   clear                   BARvuc
113100160325e   1c                   endif
113200160325      * -?Peso?
113300160325if  1c                   if             §MVPpbl    = 'S'
113400160325     c                             and  BARpuc     > *zeros
113500160325     c                             and  wSPU_time >= MVP_P_timI
113600160325     c                             and  wSPU_time <= MVP_P_timF
113700160325     c                   clear                   BARpuc
113800160325e   1c                   endif
113900160325      *
114000160325e   0c                   EndSl
114100160325      *
114200160325     c                   ENDSR
114300060920
114400060920      *---------------------------------------------------------------*
114500080128      *?Controllo data (non ora) di spunta                           ?*
114600060920      *---------------------------------------------------------------*
114700060920     c     Ctr_Time      BEGSR
114800031222      *
114900070212      * Controllo campi data e ora spunta da portare su FIBAR
115000060920     c                   eval      *in10   = *off
115100060920     c                   eval      *in12   = *off
115200060920     c                   reset                   dsTIM
115300060920     c                   testn                   TR4tim               10
115400080109if  1c                   IF             *in10  = *on
115500060920     c                             and  %subst(TR4tim:14:1) >= *zero
115600060920     c                             and  TR4tim > *zeros
115700060920     c                   movel     TR4tim        dsTIM
115800080109      * - Controllo data
115900111214      *   (NON può essere precedente di oltre 2 giorni quella attuale)
116000060920     c                   reset                   WLBda8
116100111214if 1Bc                   If        dsDAT   <=
116200111214     c                             %int( %subst( %char( %dec(
116300111214     c                                           %timestamp() ) )
116400111214     c                                           : 1 : 8 ) )     AND
116500111214     c                             dsDAT   >=
116600111214     c                             %int( %subst( %char( %dec(
116700111214     c                                           %timestamp() - %days(2) ) )
116800111214     c                                           : 1 : 8 ) )
116900060920     c                   eval      G08inv  = dsDAT
117000060920     c                   call      'XSRDA8'
117100060920     c                   parm                    WLBda8
117200060920if  2c                   if        G08err  = *off
117300060920     c                   move      TR4tim        TR7tim
117400060920     c                   eval      *in12   = *on
117500060920e   2c                   endif
117600111214e  1Bc                   EndIf
117700080109e   1c                   ENDIF
117800060920      *
117900080128if  1c                   if        NOT *in12
118000080109     c                   time                    w140
118100080109e   1c                   endif
118200080109      *
118300060920      * Data non corretta
118400060928if  1c                   if        NOT *in12
118500060920     c                   move      dsTIM         TR7tim
118600031222      * imposto i primi otto caratteri del campo con data odierna
118700080108     c                   clear                   WLBda8
118800080108     c                   move      w140          G08dat
118900080108     c                   call      'XSRDA8'
119000080108     c                   parm                    WLBda8
119100080108     c                   movel     G08inv        TR7tim
119200060920e   1c                   endif
119300031222      *
119400031222     c                   ENDSR
119500060920
119600060920      *---------------------------------------------------------------*
119700060920      *?Controllo dei campi del BarCode                              ?*
119800060920      *---------------------------------------------------------------*
119900060920     c     Ctr_BarCod    BEGSR
120000031222      *
120100031222      * Flag validità BARCODE, può valere:
120200170125      *  "BAR" --> Barcode BRT
120300031222      *  "SPE" --> Barcode spedizione
120400031222      *  "ERR" --> Barcode errato
120500060926     c                   clear                   wPD0ok
120600041005      *
120700041005      * Flag validità CHECK DIGIT viene calcolato solo se il barcode
120800041005      * ha superato tutte le specifiche per "BAR"
120900060925     c                   eval      $CK0err = *off
121000031222      *
121100060920      * Se barcode più lungo di 18 non può essere BAR e neppure SPE:
121200060920      * testo 19esima posizione per letture LaserGun di 19 char
121300060920if  1c                   if             wBAR_xx1 <> *blanks
121400060920     c                             or  (wBAR_xx2 <> '#'
121500060920     c                              and wBAR_xx2 <> *blanks)
121600060926     c                   eval      wPD0ok  = 'ERR'
121700060920     c                   LEAVESR
121800060920e   1c                   endif
121900041014      *
122000060920      * Controlla se dati numerici
122100060920     c                   eval      *in10   = *off
122200060920     c                   testn                   wBAR_BarCo           10
122300060920if  1c                   if             *in10   = *off
122400060920     c                             or   %subst(wBAR_BarCo:18:1) < *zero
122500060926     c                   eval      wPD0ok  = 'ERR'
122600060920     c                   goto      EndBarCod
122700060920e   1c                   endif
122800031222      *
122900060920      * Controlla se campi valorizzati
123000060920sel 1c                   select
123100060920w   1c                   when      wBAR_lnp = *zeros
123200060926     c                   eval      wPD0ok   = 'ERR'
123300060920w   1c                   when      wBAR_lna = *zeros
123400060926     c                   eval      wPD0ok   = 'ERR'
123500060920w   1c                   when      wBAR_nsc = *zeros
123600060926     c                   eval      wPD0ok   = 'ERR'
123700060920e   1c                   endsl
123800060926     c     wPD0ok        cabeq     'ERR'         EndBarCod
123900031222      *
124000060920      * Controlla validità linea di partenza
124100060920     c                   clear                   ORGfag
124200060920     c                   movel     wBAR_lnp      ORGfil
124300060920     c     ORGfil        chain     AZORG
124400060920if  1c                   if        NOT  %found(AZORG01L)
124500060920     c                             or  (ORGfag <> 'A'
124600060920     c                              and ORGfag <> 'F')
124700060926     c                   eval      wPD0ok  = 'ERR'
124800060920     c                   goto      EndBarCod
124900060920e   1c                   endif
125000031222      *
125100060920      * Controlla validità linea di arrivo
125200060920     c                   clear                   ORGfag
125300060920     c                   movel     wBAR_lna      ORGfil
125400060920     c     ORGfil        chain     AZORG
125500060920if  1c                   if        NOT  %found(AZORG01L)
125600060920     c                             or  (ORGfag <> 'A'
125700060920     c                              and ORGfag <> 'F')
125800060926     c                   eval      wPD0ok  = 'ERR'
125900060920     c                   goto      EndBarCod
126000060920e   1c                   endif
126100041005      *
126200060920      * Controlla validità check digit
126300060925     c                   clear                   TRUL28ds
126400060920     c                   eval      I28tla  = 'C'
126500060920     c                   eval      I28mod  = 'BAR'
126600060920     c                   eval      I28cod  = wBAR_bar
126700060920     c                   call      'TRUL28R1'
126800060920     c                   parm                    TRUL28ds
126900060920if  1c                   if             O28err <> *blanks
127000060920     c                             or   O28ckd <> wBAR_chk
127100060926     c                   eval      wPD0ok  = 'ERR'
127200060920     c                   eval      $CK0err = *on
127300060920     c                   goto      EndBarCod
127400060920e   1c                   endif
127500060920      *
127600060920     c     EndBarCod     tag
127700041005      *
127800060920      * SE ok imposto flag, ALTRIMENTI verifico se spedizione
127900060926if  1c                   if        wPD0ok  = *blanks
128000060926     c                   eval      wPD0ok  = 'BAR'
128100060920x   1c                   else
128200060920     c                   exsr      Ctr_BarSpe
128300060920e   1c                   endif
128400041014      *
128500031222     c                   ENDSR
128600060920
128700060920      *---------------------------------------------------------------*
128800060920      *?Controllo dei campi del BarCode Spedizione                   ?*
128900060920      *---------------------------------------------------------------*
129000060920     c     Ctr_BarSpe    BEGSR
129100031222      *
129200060920      * Flag validità BARCODE, può valere:
129300170125      *  "BAR" --> Barcode BRT
129400060920      *  "SPE" --> Barcode spedizione
129500060920      *  "ERR" --> Barcode errato
129600060926     c                   clear                   wPD0ok
129700041014      *
129800060920      * Se barcode più lungo di 15 non può essere BAR e neppure SPE:
129900060920      * testo 19esima posizione per letture LaserGun di 19 char
130000060920if  1c                   if        wSPE_xx1  <> *blanks
130100060926     c                   eval      wPD0ok  = 'ERR'
130200060920     c                   LEAVESR
130300060920e   1c                   endif
130400031222      *
130500060920      * Controlla se dati numerici
130600060920     c                   eval      *in10   = *off
130700060920     c                   testn                   wSPE_BarCo           10
130800060920if  1c                   if             *in10 = *off
130900060920     c                             or   %subst(wSPE_BarCo:15:1) < *zero
131000060926     c                   eval      wPD0ok  = 'ERR'
131100060920     c                   leavesr
131200060920e   1c                   endif
131300031222      *
131400031222      * controlla se campi valorizzati
131500060920sel 1c                   select
131600060920w   1c                   when      WSPE_LNP = *zeros
131700060926     c                   eval      wPD0ok   = 'ERR'
131800060920     c                   leavesr
131900060920w   1c                   when      WSPE_NSP = *zeros
132000060926     c                   eval      wPD0ok   = 'ERR'
132100060920     c                   leavesr
132200060920e   1c                   endsl
132300031222      *
132400031222      * controllo validità linea di partenza
132500060920     c                   clear                   ORGfag
132600060920     c                   movel     wSPE_lnp      ORGfil
132700060920     c     ORGfil        chain     AZORG
132800060920if  1c                   if        NOT  %found(AZORG01L)
132900060920     c                             or  (ORGfag <> 'A'
133000060920     c                              and ORGfag <> 'F')
133100060926     c                   eval      wPD0ok  = 'ERR'
133200060920     c                   leavesr
133300060920e   1c                   endif
133400031222      *
133500031222      *  Se ok imposto flag barcode spedizione
133600060926     c                   if        wPD0ok  = *blanks
133700060926     c                   eval      wPD0ok  = 'SPE'
133800060920     c                   endif
133900041014      *
134000031222     c                   ENDSR
134100060921
134200060921      *---------------------------------------------------------------*
134300060921      *?Verifica se scrivere spunta uscita x PICKING VDL - PC VDL    ?*
134400060921      *---------------------------------------------------------------*
134500060921     c     BarCo_PK23    BEGSR
134600060921      *
134700060921      * Apro file colli
134800090129     c                   if        NOT %open(FITR602L)
134900090129     c                   open      FITR602L
135000060921     c                   endif
135100040421      *
135200040422      * Per la ricerca della baia utilizzo gli ultimi 2 byte, il campo
135300040422      * VDL è lungo 3 e sui nostri file 2.
135400040422      * Se il 1° dei 2 byte è blank imposto 0, questo perchè nei file
135500060921      * colli e piano trovo, ad esempio, "01" mentre VDL passa " 1"
135600060921     c                   eval      wTR7pad2 = %subst(TR7pad:2:2)
135700060920     c                   if        %subst(wTR7pad2:1:1) = ' '
135800040422     c                   movel     '0'           wTR7pad2
135900040422     c                   endif
136000040421      *
136100060921      *  Aggancio file colli
136200090129     c     K02tr6        chain     FITR6
136300040422      *
136400040422      *  Se trovato e la baia del file colli coincide con quella VDL
136500060921      *  creo la spunta con il FV del file colli
136600060921      *  - Tranne se FV IMA per cui non creiamo spunta ma memorizzo
136700060921      *    la baia per velocizzare i controlli sui record successivi
136800060921      *  - Tranne se Distinta messaggerie
136900090129if  2c                   If             %found(FITR602L)
137000060921     c                             and  TR6bau = wTR7pad2
137100050831      *
137200060921sel 3c                   select
137300060921w   3c                   when      TR6fl3   = 'M'
137400060921w   3c                   when      TR6npg  <>  3
137500060921     c                   z-add     TR6nfv        BARnfv
137600060921     c                   z-add     TR6npg        BARnpg
137700060921e   3c                   endsl
137800040429      *
137900060921x   2c                   Else
138000040422      *
138100040607      *  In caso contrario cerco la baia VDL nel piano di smistamento:
138200040607      *   se trovata creo la spunta "uscita", impostando il FV trovato nel
138300060921      *   file piani, a meno che sia "non letti" o IMA o defluenza.
138400161129      *   Eseguo setll/read, perchè cerco un F.V. tra quelli presenti in baia,
138500161111      *   scartando la baia "non letti" ed eventuali FV defluenza ed IMA
138600161129      *   (ora quelli IMA sono ANCHE?misti?a quelli consegna - stessa baia).
138700161111      *   NON memorizzo (come già NON faceva prima) le baie "non letti",
138800161111      *   defluenza ed IMA...
138900040422      *
139000040422      *  Apro file piano di smistamento
139100060921if  3c                   if        NOT %open(FLTR901L)
139200060921     c                   open      FLTR901L
139300060921e   3c                   endif
139400040421      *
139500060921     c                   movel(p)  wTR7pad2      TR9pep
139600060921     c                   z-add     wBARnfv       TR9nfv
139700161111     c     K04TR901      setll     FLTR9000
139800161111     c     K04TR901      reade     FLTR9000
139900161111do  3c                   DoW       NOT  %eof(FLTR901L)
140000161111     c                             and (TR9bnl = 'S'
140100161111     c                              or  TR9npg <> 4)
140200161111     c     K04TR901      reade     FLTR9000
140300161111e   3c                   EndDo
140400161111if  3c                   if        NOT  %eof(FLTR901L)
140500161111     c                   z-add     TR9nfv        BARnfv
140600161111     c                   z-add     TR9npg        BARnpg
140700161111e   3c                   endif
140800040422      *
140900060921e   2c                   EndIf
141000040422      *
141100040421     c                   ENDSR
141200060921
141300060921      *---------------------------------------------------------------*
141400060921      *?Verifica se scrivere spunta uscita x PICKING VDL - PC NO VDL ?*
141500060921      *---------------------------------------------------------------*
141600060921     c     BarCo_PK26    BEGSR
141700060921      *
141800040504      * Apro file colli
141900090129if  1c                   if        NOT %open(FITR602L)
142000090129     c                   open      FITR602L
142100060921e   1c                   endif
142200040504      *
142300040504      * Aggancio file colli
142400090129     c     K02tr6        chain     FITR6
142500040504      *
142600060921      * Se non trovato non creo spunta
142700060921      * Se distinta messaggerie non creo spunta
142800060921      * Se trovato e la postazione non è IMA creo la spunta con il FV
142900060921      *    del file colli
143000090129if  1c                   if             %found(FITR602L)
143100060921     c                             and  TR6npg <>  3
143200060921     c                             and  TR6fl3 <> 'M'
143300040504     c                   z-add     TR6nfv        BARnfv
143400040504     c                   z-add     TR6npg        BARnpg
143500060921e   1c                   endif
143600040504      *
143700040504     c                   ENDSR
143800060921
143900160518      *---------------------------------------------------------------*
144000160518      *?Verifica se scrivere spunta 4 x PICKING NO VDL e filiale abilitata *
144100160518      *---------------------------------------------------------------*
144200160518     c     BarCo_PK24    BEGSR
144300160518      *
144400160518      * Apro file colli
144500160518if  1c                   if        NOT %open(FITR602L)
144600160518     c                   open      FITR602L
144700160518e   1c                   endif
144800160518      *
144900160518      * Aggancio file colli
145000160518     c     K02tr6        chain     FITR6
145100160518      *
145200160518      * Se non trovato non creo spunta
145300160518      * Se distinta messaggerie non creo spunta
145400160518      * Se trovato e la postazione non è IMA creo la spunta con il FV
145500160518      *    del file colli
145600160518if  1c                   if             %found(FITR602L)
145700160518     c                             and  TR6npg <>  3
145800160518     c                             and  TR6fl3 <> 'M'
145900160518     c                   z-add     TR6nfv        BARnfv
146000160518     c                   z-add     TR6npg        BARnpg
146100160518e   1c                   endif
146200160518      *
146300160518     c                   ENDSR
146400160518
146500060921      *---------------------------------------------------------------*
146600060921      *?Scrittura SALDI                                              ?*
146700060921      *---------------------------------------------------------------*
146800060920     c     Wrt_SAL       BEGSR
146900031222      *
147000031222      * Reperisco data e l'ora di lettura da TR7TIM che è già stato
147100031222      *  controllato e "normalizzato"
147200080128      * -> In realtà: solo la data è stata controllata e reimpostata...
147300060921     c                   movel     TR7tim        dsTIM
147400060921     c                   movel     dsDAT         KSAdtl
147500080128     c***                z-add     dsORA         KSAorl
147600080128      * - Controllo ora
147700160502if  1c                   IF            %subst( %trim( %editw(
147800080128     c                                 dsORA:'0    ' ) ) :1:2 ) <  '00'
147900080128     c                             or  %subst( %trim( %editw(
148000080128     c                                 dsORA:'0    ' ) ) :1:2 ) >  '23'
148100080128     c                             or  %subst( %trim( %editw(
148200080128     c                                 dsORA:'0    ' ) ) :3:2 ) <  '00'
148300080128     c                             or  %subst( %trim( %editw(
148400080128     c                                 dsORA:'0    ' ) ) :3:2 ) >  '59'
148500080128      * - - Ora  non corretta
148600080128      *     imposto il campo con ora/minuti correnti
148700080128     c                   time                    w140
148800080128     c                   movel     w140          KSAorl
148900080128     c                   ELSE
149000080128     c                   z-add     dsORA         KSAorl
149100080128e   1c                   ENDIF
149200080619      * - memorizzo ora/minuti/secondi da tr7tim per FIMSC00F
149300080619     c                   eval      ksciorl = %dec(%subst(tr7tim:9:6):6:0)
149400160502      *
149500160502      * -?Tipo Applicazione?
149600160502     c                   clear                   xx
149700160502     c                   eval      *in97   = *off
149800160502if  1c                   If        TR4020  = '2T'
149900160502      *
150000160502      * -?Tipo Applicazione per TRANSPALLET pesatore = "T" (chiodato)?
150100160502     c                   eval      KSAapl  = 'T'
150200160502      *
150300160502x   1c                   Else
150400021008      *
150500060921      * Cerco il tipo applicazione dell'entrata corrente:
150600060921      *   se non trovo l'entrata, prendo l'applicazione principale per
150700060921      *   il P.O. di tabella MFP
150800060921     c                   eval      xx      = 1
150900060921     c     TR4pep        lookup    $MSL(xx)                               97
151000161129     c                   select
151100161129     c                   when      *in97  and  TR4ctl <> '5'
151200161129     c                   eval      KSAapl  = $MSLa(xx)
151300161129     c                   when      *in97  and  TR4ctl =  '5'
151400161129     c                   eval      KSAapl  = $MSLa5(xx)
151500161129     c                   when      Not *in97
151600161129     c                   eval      KSAapl  = §MFPapl
151700161129     c                   endsl
151800160502      *
151900160502e   1c                   EndIf
152000161213      *
152100161213      *//* -?Tipo Lavorazione:?
152200161213     c*//                   clear                   KSAlav
152300161213sel 1c*//                   select
152400161213      *//* - -?Entrata?
152500161213w   1c*//                   when      TR4ctl = '5'
152600161213     c*//                   eval      KSAlav = TR4ctl
152700161213      *//* - -?Arrivi?
152800161213w   1c*//                   when      TR4ctl = '2'  and  TR4npl <> *zeros
152900161213     c*//                   eval      KSAlav = TR4ctl
153000161213      *//* - -?Picking?
153100161213w   1c*//                   when      TR4ctl = '2'  and  TR4npl =  *zeros
153200161213     c*//*//?v. IF__1 successiva:?                  and  $MSLe(xx) = 'S'
153300161213     c*//                   eval      KSAlav = 'K'
153400161213      *//* - -?Inventario?
153500161213w   1c*//                   when      TR4ctl = '3'
153600161213     c*//                   eval      KSAlav = TR4ctl
153700161213     c*//                   endsl
153800161213      *//*
153900161213      *//* -?Tipo Impianto (fisico):?
154000161213     c*//                   clear                   KSAimp
154100161213sel 1c*//                   select
154200161213      *//* - -?Transpallet?
154300161213w   1c*//                   when      TR4020  =  '2T'
154400161213     c*//                   eval      KSAimp  =  'T'
154500161213      *//* - -?Postazione Entrata NON trovata in tab. "MSL"?
154600161213w   1c*//                   when      *in97   =  *off
154700161213     c*//                   eval      KSAimp  =  §MFPapl
154800161213      *//* - -?Wi-Fi?
154900161213w   1c*//                   when      $MSLw(xx)  = 'W'
155000161213     c*//                   eval      KSAimp  =  'W'
155100161213      *//* - -?Van Der Lande?
155200161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'V')  or
155300161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'V')
155400161213     c*//                   eval      KSAimp  =  'V'
155500161213      *//* - -?Fissi?
155600161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'I')  or
155700161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'I')
155800161213     c*//                   eval      KSAimp  =  'I'
155900161213      *//* - -?Rulliera?
156000161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'R')  or
156100161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'R')
156200161213     c*//                   eval      KSAimp  =  'R'
156300161213      *//* - -?RPVP?
156400161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'P')  or
156500161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'P')
156600161213     c*//                   eval      KSAimp  =  'P'
156700161213      *//* - -?...altrimenti?
156800161213w   1c*//                   other
156900161213     c*//                   eval      KSAimp  =  §MFPapl
157000161213e   1c*//                   endsl
157100031224      *
157200031224      * Elaboro saldi
157300031224      *  Per applicazione PICKING solo la postazione è banco entrata
157400060921if  1c                   IF             KSAapl   <> 'K'
157500060921     c                             or  (KSAapl    = 'K'
157600060921     c                              and *in97     = *on
157700060921     c                              and $MSLe(xx) = 'S')
157800031224      *
157900060921      * Controllo se lettura effettuata da SCANNER:
158000060921      * scarto se scanner ha letto "???" e laser Gun non ha letto nulla
158100060926     c                   eval      $okPD1  = *on
158200060926     c                   eval      wScan1  = %scan('????' : TR4pd1)
158300060926if  2c                   if             wScan1  > *zeros
158400060921     c                             and  TR4pd2  = *blanks
158500060926     c                   eval      $okPD1  = *off
158600060921e   2c                   endif
158700060921      *
158800060921      * Controllo se lettura effettuata da LASER GUN
158900060921      * scarto barcode non corretti tranne se errore "????"
159000060926     c                   eval      $okPD2  = *on
159100060926     c                   eval      wScan2 = %scan('????' : TR4pd2)
159200060926if  2c                   if             wScan2  = *zeros
159300060926     c                             and  wPD2ok <> 'BAR'
159400060926     c                   eval      $okPD2  = *off
159500060921e   2c                   endif
159600060921      *
159700161213      * Scrivo/Aggiorno saldi letture (FNMSA10F)
159800080407     c     K05MSA03      chain     FNMSA000
159900070725if  2c                   If        not %found(FNMSA13L)
160000080407     c                   clear                   FNMSA000
160100080407     c                   eval      MSAapl  = KSAapl
160200080407     c                   eval      MSAfil  = wPO
160300080407     c                   eval      MSAdtl  = KSAdtl
160400080407     c                   eval      MSAorl  = KSAorl
160500080407     c                   eval      MSAnpg  = wBARnpg
160600060922      * - Reperisco la data del F.V.
160700060922     c                   exsr      Rep_FVV
160800080407     c                   eval      MSAdfv  = FVVdfv
160900170125      * -?Imposto lavorazione picking (se NON picking DPD)?
161000170125     c                   If        tr4NPL= '00'  and  tr4020 <> '36'
161100151228     c                   z-add     999           MSAtcl
161200151228     c                   Else
161300151228     c                   clear                   MSAtcl
161400151228     c                   EndIf
161500151228      *
161600060922     c                   exsr      sr_RieMSA1
161700060922      *                  __________________
161800080407     c                   write(e)  FNMSA000
161900060922      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
162000060922x   2c                   Else
162100060922     c                   exsr      sr_RieMSA1
162200060922      *                  __________________
162300080407     c                   update(e) FNMSA000
162400060922      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
162500060922e   2c                   EndIf
162600161213      *
162700161213      * -?Scrivo/Aggiorno NUOVI saldi letture (FNMSA00F)?
162800161213     c     K05MSA03      chain     FNMSA003
162900161213if  2c                   If        not %found(FNMSA03L)
163000161213     c                   clear                   FNMSA003
163100161213     c                   eval      SA0apl  = MSAapl
163200161213     c                   eval      SA0fil  = MSAfil
163300161213     c                   eval      SA0dtl  = MSAdtl
163400161213     c                   eval      SA0orl  = MSAorl
163500161213     c                   eval      SA0npg  = MSAnpg
163600161213     c                   eval      SA0dfv  = MSAdfv
163700161213     c                   exsr      sr_RieMSA0
163800161213      *                  __________________
163900161213     c                   write(e)  FNMSA003
164000161213      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
164100161213x   2c                   Else
164200161213     c                   exsr      sr_RieMSA0
164300161213      *                  __________________
164400161213     c                   update(e) FNMSA003
164500161213      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
164600161213e   2c                   EndIf
164700060921      *
164800060921      * Scrive saldi letture civette
164900060921if  2c                   If        $BarCiv = *on
165000060921     c                   eval      KSCdtl  = KSAdtl
165100060921     c                   movel     TR7bar        KSCseg
165200080619      * - Scrivo il nuovo file per le letture civette
165300080619     c     K04IMSC01     chain     FIMSC000
165400080619if  3c                   if        NOT %found(FIMSC01L)
165500080619     C                   clear                   FIMSC000
165600080708     c                   eval      MSCfil  = wPO
165700080708     c                   eval      MSCdtl  = KSCdtl
165800080708     c                   eval      MSCorl  = KSCIorl
165900080708     c                   eval      MSCseg  = KSCseg
166000080619     c                   exsr      sr_RieIMSC
166100080619      *                  __________________
166200080619     c                   write     FIMSC000
166300080619      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
166400080619x   3c                   else
166500080619     c                   exsr      sr_RieIMSC
166600080619      *                  __________________
166700080619     c                   update    FIMSC000
166800080619      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
166900080619e   3c                   endif
167000060921e   2c                   EndIf
167100031224      *
167200060921e   1c                   ENDIF
167300031224      *
167400000000     c                   ENDSR
167500160215
167600160215      *---------------------------------------------------------------*
167700160215      *?Scrittura Segnacollo DPD suntato SENZA record bolla          ?*
167800160215      *---------------------------------------------------------------*
167900160215     c     Wrt_BARDPD    BEGSR
168000160215      *
168100160215     c                   clear                   BARDPD000
168200160215      *
168300160216     c                   eval      BARDPDpd1 = %subst(TR7pd2: 2: 28)
168400160216     c                   eval      BARDPDprn = %subst(TR7pd2: 10: 14)
168500160215     c                   eval      BARDPDfgs = wPO
168600160216     c                   eval      BARDPDdfs = %int( %subst( TR7tim :
168700160215     c                                                       1 : 8 ) )
168800160216     c                   eval      BARDPDhms = %int( %subst( TR7tim :
168900160215     c                                                       9 : 6 ) )
169000160215      *                  ___________________
169100160215     c                   write     BARDPD000
169200160215      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
169300160215     c                   ENDSR
169400060921
169500060921      *---------------------------------------------------------------*
169600060922      *?Reperimento data F.V. da FNFVV00F                            ?*
169700060921      *---------------------------------------------------------------*
169800060922     c     Rep_FVV       BEGSR
169900060921      *
170000060921      * Recupera la data foglio
170100060921     c                   z-add     wBARnfv       FVVnfv
170200060921     c     K03FVV01      chain     FNFVV000
170300060921if  1c                   if        NOT %found(FNFVV01L)
170400060921     c                   clear                   FVVdfv
170500060921e   1c                   endif
170600160510if  1c                   if        FVVdfv = *zero
170700160510     c                   eval      dsTIM  = %subst( %char( %dec(
170800160510     c                                         %timestamp() ) )
170900160510     c                                         : 1 : 14 )
171000160510     c                   eval      FVVdfv = dsDAT
171100160510e   1c                   endif
171200060921      *
171300000000     c                   ENDSR
171400080619
171500080619      *---------------------------------------------------------------*
171600080619      *?Impostazione campi record di FIMSC                           ?*
171700080619      *---------------------------------------------------------------*
171800080619     c     sr_RieIMSC    BEGSR
171900080619      *
172000080619      * Imposta campi record
172100080708     c                   eval      MSCpes  = wPES                               *peso
172200080708     c                   eval      MSCvol  = wVOL                               *volume
172300080619      * - Baia d'entrata
172400080708     c   97              move      $MSLn(xx)     MSCpep                         *baia entrata
172500080708     c  N97              clear                   MSCpep
172600080619      *
172700080619     c                   ENDSR
172800060921
172900060921      *---------------------------------------------------------------*
173000060921      *?Pulizia delle variabili di lavoro                            ?*
173100060921      *---------------------------------------------------------------*
173200060921     c     sr_ClrFld     BEGSR
173300060921      *
173400060921     c                   clear                   wPES
173500060921     c                   clear                   wVOL
173600060921     c                   clear                   wODSdiv
173700060921     c                   clear                   wODSdnr
173800060921     c                   clear                   wODSdna
173900060921     c                   clear                   wODSftd
174000060921     c                   clear                   wODSids
174100060921     c                   clear                   wODSuds
174200060921     c                   clear                   wODSdne
174300060921     c                   clear                   wODSciv
174400060921     c                   clear                   wODSdnq
174500060921     c                   clear                   wODSust
174600060921     c                   clear                   wDTSnts
174700060921     c                   clear                   wDTStok
174800060921     c                   clear                   wDTSila
174900060921     c                   clear                   wDTSnrl
175000060921     c                   clear                   wDTSndf
175100060921     c                   clear                   wPS1dnr
175200060921     c                   clear                   wPS1dts
175300060921     c                   clear                   wPS1dtl
175400060921     c                   clear                   wPS1nsc
175500060921     c                   clear                   wPS1nre
175600060921     c                   clear                   wPS1dok
175700060921     c                   clear                   wPS1dno
175800060921     c                   clear                   wPS2dnr
175900060921     c                   clear                   wPS2dts
176000060921     c                   clear                   wPS2dtl
176100060921     c                   clear                   wPS2nsc
176200060921     c                   clear                   wPS2nre
176300060921     c                   clear                   wPS2dok
176400060921     c                   clear                   wPS2dno
176500060921     c                   clear                   wPESnes
176600060921     c                   clear                   wPESind
176700060921     c                   clear                   wPESune
176800060921     c                   clear                   wPUSnes
176900060921     c                   clear                   wPUSsor
177000060921     c                   clear                   wPUSlos
177100060922     c                   clear                   $PEP
177200060921     c                   clear                   $PAD
177300060921     c                   clear                   wCSD
177400060921     c                   clear                   wCCP
177500060921     c                   clear                   wCCV
177600060921     c                   clear                   wNLS
177700060921     c                   clear                   wNLP
177800161213     c                   clear                   wPWF
177900060921      *
178000060921     c                   ENDSR
178100060922
178200060922      *---------------------------------------------------------------*
178300060922      *?Impostazione campi record di FNMSA10F                        ?*
178400060922      *---------------------------------------------------------------*
178500060922     c     sr_RieMSA1    BEGSR
178600060922      *
178700060922      * Azzera le variabili di lavoro
178800060922     c                   exsr      sr_ClrFld
178900060922      *
179000060922      * Imposta campi di lavoro per il file FNMSA10F
179100060922     c                   exsr      sr_FldMSA1
179200060922      *
179300060922      * Aggiorna campi nel record di FNMSA10F
179400060922     c                   exsr      sr_RecMSA1
179500060922      *
179600060922     c                   ENDSR
179700060922
179800060922      *---------------------------------------------------------------*
179900060922      *?Impostazione delle variabili di lavoro per FNMSA00F          ?*
180000060922      *---------------------------------------------------------------*
180100060922     c     sr_FldMSA1    BEGSR
180200060922      *
180300061006      *?Peso  - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
180400060922     c                   eval      wPES    = wBARpuc
180500061006      *
180600061006      *?Volume  - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
180700060922     c                   eval      wVOL    = wBARvuc
180800061006      *
180900061006      *?Parcel entrance point - - - - - - - - - - - - - - - - - - - -?*
181000061006      *
181100061006      * colli caricati su ciascun banco
181200061006      * rilevati dalla fotocellula (NON inaspettatamente)
181300061006      * & usciti correttamente
181400061006     c                   eval      xx      = 1
181500061006     c     TR4pep        lookup    $MSL(xx)                               97
181600061011sel 1c                   select
181700061011      * - valore nullo (TR4pep = *blanks)
181800061011w   1c                   when           *in97      = *on
181900061011     c                             and  $MSLn(xx) <= *zeros
182000061019      * - VDL: collo NON caricato
182100061019w   1c                   when           KSAapl  = 'V'
182200061019     c                             and (TR4pus  = '2'
182300061019     c                              or (TR4pes <> '1'
182400061019     c                              and TR4pes <> '2'))
182500061019      * - VDL: collo caricato in baia '20'
182600061019w   1c                   when           KSAapl  = 'V'
182700061019     c                             and  TR4pus <> '2'
182800061019     c                             and  TR4pes  = '2'
182900061019     c                   eval      $PEP(20)   = 1                               NON TROVATO
183000061011      * - valore valido
183100061011w   1c                   when           *in97      = *on
183200061011     c                             and  $MSLn(xx)  > *zeros
183300061011     c                             and  $MSLn(xx)  < %elem($PEP)
183400061006     c                   eval      $PEP( $MSLn(xx) ) = 1                        TROVATO
183500061011x   1c                   other
183600061006      * - nessun valore conosciuto --> entrance point n° 20
183700061006     c                   eval      $PEP(20)   = 1                               NON TROVATO
183800061011e   1c                   endsl
183900061006      *
184000061006      *?Parcel actual destination - - - - - - - - - - - - - - - - - -?*
184100061006      *
184200151130      * -?Se  VDL & collo smistato correttamente  o  Non VDL?
184300151130if  Ac                   If             KSAapl  = 'V'
184400151130     c                             and  TR4pus <> '2'
184500151130     c                             OR   KSAapl <> 'V'
184600061006     c                   eval      wTR4pad = %xlate(' ':'0':TR4pad)             *trasforma *b in *z
184700061006     c                   eval      *in10   = *off
184800061006     c                   testn                   wTR4pad              10
184900061006if  1c                   if        *in10   = *on                                *campo numerico
185000061006     c                   movel     wTR4pad       w003
185100061006x   1c                   else
185200061006     c                   clear                   w003
185300061006e   1c                   endif
185400061006if  1c                   if             w003    > *zeros
185500061006     c                             and  w003   <= %elem($PAD)
185600061006     c                   eval      $PAD(w003) = 1
185700061006x   1c                   else
185800061006      * - nessun valore conosciuto --> actual destination n° 60
185900061006     c                   eval      $PAD(60)   = 1
186000061006e   1c                   endif
186100151130e   Ac                   EndIf
186200060922      *
186300060922      *?Original destination state  - - - - - - - - - - - - - - - - -?*
186400060922      *
186500060922sel 1c                   select
186600060922      * diverted
186700060922     c                   when      TR4ods  = '1'
186800060922     c                   eval      wODSdiv = 1
186900060922      * destination not reached
187000060926      *   ?» *CHG «?- Overflow per errori macchina ('2')
187100060926     c                   when           TR4ods  = '2'
187200060926     c                             and (KSAapl <> 'V'
187300060922     c                             OR   KSAapl  = 'V'
187400060926     c                             and  TR4pus  = '0')
187500060922     c                   eval      wODSdnr = 1
187600060922      * destination not available
187700060926      *   ?» *CHG «?- Overflow per uscita piena    ('3')
187800060926     c                   when           TR4ods  = '3'
187900060926     c                             and (KSAapl <> 'V'
188000060922     c                             OR   KSAapl  = 'V'
188100060926     c                             and  TR4pus  = '0')
188200060922     c                   eval      wODSdna = 1
188300060922      * failed to diverted
188400060926      *   ?» *CHG «?- Overflow per errori macchina ('4')
188500060926     c                   when           TR4ods  = '4'
188600060926     c                             and (KSAapl <> 'V'
188700060922     c                             OR   KSAapl  = 'V'
188800060926     c                             and  TR4pus  = '0')
188900060922     c                   eval      wODSftd = 1
189000060922      * invalid destination
189100060926      *   ?» *CHG «?- Overflow per errori macchina ('5')
189200060926     c                   when           TR4ods  = '5'
189300060926     c                             and (KSAapl <> 'V'
189400060922     c                             OR   KSAapl  = 'V'
189500060926     c                             and  TR4pus  = '0')
189600060922     c                   eval      wODSids = 1
189700060922      * unreachable destination
189800060926      *   ?» *CHG «?- Overflow per errori macchina ('6')
189900060926     c                   when           TR4ods  = '6'
190000060926     c                             and (KSAapl <> 'V'
190100060922     c                             OR   KSAapl  = 'V'
190200060926     c                             and  TR4pus  = '0')
190300060922     c                   eval      wODSuds = 1
190400060922      * destination not received
190500060926      *   ?» *CHG «?- Overflow per errori macchina ('7')
190600060926     c                   when           TR4ods  = '7'
190700060926     c                             and (KSAapl <> 'V'
190800060922     c                             OR   KSAapl  = 'V'
190900060926     c                             and  TR4pus  = '0')
191000060922     c                   eval      wODSdne = 1
191100060922      * condition invalid
191200060926      *   ?» *CHG «?- Overflow per errori macchina ('8')
191300060926     c                   when           TR4ods  = '8'
191400060926     c                             and (KSAapl <> 'V'
191500060922     c                             OR   KSAapl  = 'V'
191600060926     c                             and  TR4pus  = '0')
191700060922     c                   eval      wODSciv = 1
191800060922      * destination not requested
191900060926      *   ?» *CHG «?- Overflow per errori macchina ('9')
192000060926     c                   when           TR4ods  = '9'
192100060926     c                             and (KSAapl <> 'V'
192200060922     c                             OR   KSAapl  = 'V'
192300060926     c                             and  TR4pus  = '0')
192400060922     c                   eval      wODSdnq = 1
192500060922      * unknown state
192600061018      *   ?» *CHG «?- Overflow per errori macchina ('0')
192700061018     c                   when           TR4ods  = '0'
192800061018     c                             and (KSAapl <> 'V'
192900061018     c                             OR   KSAapl  = 'V'
193000061018     c                             and  TR4pus  = '0')
193100060922     c                   eval      wODSust = 1
193200060922      * nessun valore conosciuto --> unknown state
193300061018      *   ?» *CHG «?- Overflow per errori macchina (???)
193400060922     c                   other
193500061018     c                   if            (KSAapl <> 'V'
193600061018     c                             OR   KSAapl  = 'V'
193700061018     c                             and  TR4pus  = '0')
193800060922     c                   eval      wODSust = 1
193900061018     c                   endif
194000060922e   1c                   endsl
194100060922      *
194200060922      *?Destination translate state - - - - - - - - - - - - - - - - -?*
194300060922      *
194400060922sel 1c                   select
194500060922      * no translation state
194600060922     c                   when      TR4dts  = '0'
194700060922     c                   eval      wDTSnts = 1
194800060922      * translation OK
194900060922     c                   when      TR4dts  = '1'
195000060922     c                   eval      wDTStok = 1
195100060922      * illegal label
195200060922     c                   when      TR4dts  = '2'
195300060922     c                   eval      wDTSila = 1
195400060922      * no read label
195500060922     c                   when      TR4dts  = '3'
195600060922     c                   eval      wDTSnrl = 1
195700060922      * no destination found
195800060922     c                   when      TR4dts  = '4'
195900060922     c                   eval      wDTSndf = 1
196000060922      * nessun valore conosciuto --> no destination found
196100060922     c                   other
196200060922     c                   eval      wDTSndf = 1
196300060922e   1c                   endsl
196400060922      *
196500060922      *?Parcel scanner data MACCHINONE  - - - - - - - - - - - - - - -?*
196600060922      *
196700060922sel 1c                   select
196800060922      * data non read yeat
196900060922     c                   when      TR4ps1  = '1'
197000060922     c                   eval      wPS1dnr = 1
197100060922      * data too short
197200060922     c                   when      TR4ps1  = '2'
197300060922     c                   eval      wPS1dts = 1
197400060922      * data too long
197500060922     c                   when      TR4ps1  = '3'
197600060922     c                   eval      wPS1dtl = 1
197700060922      * no scan
197800060922     c                   when      TR4ps1  = '4'
197900060922     c                   eval      wPS1nsc = 1
198000060922      * no read
198100060922     c                   when      TR4ps1  = '5'
198200060922     c                   eval      wPS1nre = 1
198300060922      * data OK
198400060922     c                   when      TR4ps1  = '6'
198500060922     c                   eval      wPS1dok = 1
198600060922      * data not OK
198700060922     c                   when      TR4ps1  = '7'
198800060922     c                   eval      wPS1dno = 1
198900060922      * nessun valore conosciuto --> data not OK
199000060922     c                   other
199100060922     c                   eval      wPS1dno = 1
199200060922e   1c                   endsl
199300060922      *
199400060922      *?Parcel scanner data PISTOLA - - - - - - - - - - - - - - - - -?*
199500060922      *
199600060922sel 1c                   select
199700060922      * data non read yeat
199800060922     c                   when      TR4ps2  = '1'
199900060922     c                   eval      wPS2dnr = 1
200000060922      * data too short
200100060922     c                   when      TR4ps2  = '2'
200200060922     c                   eval      wPS2dts = 1
200300060922      * data too long
200400060922     c                   when      TR4ps2  = '3'
200500060922     c                   eval      wPS2dtl = 1
200600060922      * no scan
200700060922     c                   when      TR4ps2  = '4'
200800060922     c                   eval      wPS2nsc = 1
200900060922      * no read
201000060922     c                   when      TR4ps2  = '5'
201100060922     c                   eval      wPS2nre = 1
201200060922      * data OK
201300060922     c                   when      TR4ps2  = '6'
201400060922     c                   eval      wPS2dok = 1
201500060922      * data not OK
201600060922     c                   when      TR4ps2  = '7'
201700060922     c                   eval      wPS2dno = 1
201800060922      * nessun valore conosciuto --> data not OK
201900060922     c                   other
202000060922     c                   eval      wPS2dno = 1
202100060922e   1c                   endsl
202200060922      *
202300060922      *?Parcel entrance state - - - - - - - - - - - - - - - - - - - -?*
202400060922      *
202500060922sel 1c                   select
202600060922      * no entrance state
202700060922     c                   when      TR4pes  = '0'
202800060922     c                   eval      wPESnes = 1
202900060922      * inducted
203000060926      *   ?» *CHG «?- Caricati (1)
203100060926     c                   when           TR4pes  = '1'
203200060926     c                             and (KSAapl <> 'V'
203300060922     c                             OR   KSAapl  = 'V'
203400060922     c                             and (TR4pus  = '0'
203500060926     c                              or  TR4pus  = '1'))
203600061011     c*** anche se da baia 20:     and  *in97   = *on
203700060922     c                   eval      wPESind = 1
203800061003      *   ?» *CHG «?- Caricati (2)
203900061003     c                   when           TR4pes  = '2'
204000061003if  2c                             and  KSAapl  = 'V'
204100061003     c                             and (TR4pus  = '0'
204200061003     c                              or  TR4pus  = '1')
204300061011     c*** anche se da baia 20:     and  *in97   = *on
204400061003     c                   eval      wPESind = 1
204500060922      * unexpected
204600060929     c                   when      TR4pes  = '2'
204700060922     c                   eval      wPESune = 1
204800060922      * nessun valore conosciuto --> unexpected
204900060922     c                   other
205000060922     c                   eval      wPESune = 1
205100060922e   1c                   endsl
205200060922      *
205300060922      *?Parcel exit state - - - - - - - - - - - - - - - - - - - - - -?*
205400060922      *
205500060922sel 1c                   select
205600060922      * no exit state
205700060922     c                   when      TR4pus  = '0'
205800060926     c                   eval      wPUSnes = 1
205900060922      * sorted
206000060926      *   ?» *CHG «?- Smistati correttamente
206100060926     c                   when           TR4pus  = '1'
206200060926     c                             and (KSAapl <> 'V'
206300060926     c                             OR   KSAapl  = 'V'
206400061005     c                             and  TR4pes <> '0')
206500060922     c                   eval      wPUSsor = 1
206600060922      * lost
206700060922     c                   when      TR4pus  = '2'
206800060922     c                   eval      wPUSlos = 1
206900060922      * nessun valore conosciuto --> lost
207000060922     c                   other
207100060922     c                   eval      wPUSlos = 1
207200060922e   1c                   endsl
207300060922      *
207400060922      *?Colli scanner datalogic - - - - - - - - - - - - - - - - - - -?*
207500060922      *
207600060922if  1c                   if             TR4ps1  = '6'
207700060922     c                             and  TR4ps2 <> '6'
207800060922     c                   eval      wCSD    = 1
207900060922e   1c                   endif
208000060922      *
208100060922      *?Colli con peso  - - - - - - - - - - - - - - - - - - - - - - -?*
208200060922      *
208300060922if  1c                   if             TR4ods = '1'
208400060922     c                             and  wPES   > *zeros
208500160510     c                             and (TR4020 <> '2T'
208600160510     c                              or  TR4nfv <> *all'9')
208700060922     c                   eval      wCCP    = 1
208800060922e   1c                   endif
208900060922      *
209000060922      *?Colli con volume  - - - - - - - - - - - - - - - - - - - - - -?*
209100060922      *
209200060922if  1c                   if             TR4ods = '1'
209300060922     c                             and  wVOL   > *zeros
209400060922     c                   eval      wCCV    = 1
209500060922e   1c                   endif
209600060922      *
209700060922      *?Non letti scanner datalogic - - - - - - - - - - - - - - - - -?*
209800060922      *
209900060926if  1c                   if        $okPD1  = *off
210000060922     c                   eval      wNLS    = 1
210100060922e   1c                   endif
210200060922      *
210300060922      *?Non letti pistola - - - - - - - - - - - - - - - - - - - - - -?*
210400060922      *
210500060922sel 1c                   select
210600060922      *
210700060922      * VDL e PICKING VDL CON PC VDL:
210800060922      *   contiene i colli letti SIA da scanner CHE da pistola
210900170125      *   SE entrambi i barcode sono BRT
211000060922     c                   when          (TR4020  = '20'
211100060922     c                              or  TR4020  = '23')
211200060922     c                             and  TR4ps1  = '6'
211300060922     c                             and  TR4ps2  = '6'
211400060926     c                             and  $okPD1  = *on
211500060926     c                             and  $okPD2  = *on
211600060922     c                   eval      wNLP    = 1
211700060922      *
211800060922      * VDL e PICKING VDL PC VDL:
211900060922      *   altri casi NON valorizzo
212000060922     c                   when           TR4020  = '20'
212100060922     c                             or   TR4020  = '23'
212200060922     c                   clear                   wNLP
212300060922      *
212400060922      * DISK C:
212500170125      *   contiene "CHI SONO" (non BRT)
212600060922     c                   when      TR4020  = '21'
212700060926if  2c                   if        $okPD2  = *off
212800060922     c                   eval      wNLP    = 1
212900060922e   2c                   endif
213000060922      *
213100060922      * PICKING NON VDL e VDL CON PC NON VDL:
213200170125      *   contiene barcode BRT
213300060922     c                   when           TR4020  = '24'
213400060922     c                             or   TR4020  = '26'
213401170128     c                             or   TR4020  = '36'
213500060922     c                   clear                   wNLP
213600060922      *
213700060922      * PICKING NON VDL e VDL CON PC NON VDL:
213800060922      *   barcode spedizione NON valorizzo
213900060922     c                   when           TR4020  = '25'
214000060922     c                             or   TR4020  = '27'
214100060922     c                   eval      wNLP    = 1
214200060922      *
214300060922      * RPVP:
214400060922      *   NON valorizzo
214500060922     c                   when           TR4020  = '22'
214600060922     c                             or   TR4020  = '2C'
214700060922     c                             or   TR4020  = '2K'
214800060922     c                   clear                   wNLP
214900060922      *
215000060922     c                   other
215100060922     c                   clear                   wNLP
215200060922      *
215300060922e   1c                   endsl
215400161213      *
215500161213      *?Numero Letture Pistole Wi-Fi  - - - - - - - - - - - - - - - -?*
215600161213if  1c                   if             *in97      = *on
215700161213     c                             and  $MSLw(xx)  = 'W'
215800170118     c                             and  TR4pus    <> '2'
215900161213     c                   eval      wPWF    = 1
216000161213e   1c                   endif
216100060922      *
216200060922     c                   ENDSR
216300060922
216400060922      *---------------------------------------------------------------*
216500060922      *?Aggiornamento campi nel rec. FNMSA10F                        ?*
216600060922      *---------------------------------------------------------------*
216700060922     c     sr_RecMSA1    BEGSR
216800060922      *
216900080407     c                   add       wPES          MSApes
217000080407     c                   add       wVOL          MSAvol
217100080407     c                   add       wODSdiv       MSAodsdiv
217200080407     c                   add       wODSdnr       MSAodsdnr
217300080407     c                   add       wODSdna       MSAodsdna
217400080407     c                   add       wODSftd       MSAodsftd
217500080407     c                   add       wODSids       MSAodsids
217600080407     c                   add       wODSuds       MSAodsuds
217700080407     c                   add       wODSdne       MSAodsdne
217800080407     c                   add       wODSciv       MSAodsciv
217900080407     c                   add       wODSdnq       MSAodsdnq
218000080407     c                   add       wODSust       MSAodsust
218100080407     c                   add       wDTSnts       MSAdtsnts
218200080407     c                   add       wDTStok       MSAdtstok
218300080407     c                   add       wDTSila       MSAdtsila
218400080407     c                   add       wDTSnrl       MSAdtsnrl
218500080407     c                   add       wDTSndf       MSAdtsndf
218600080407     c                   add       wPS1dnr       MSAps1dnr
218700080407     c                   add       wPS1dts       MSAps1dts
218800080407     c                   add       wPS1dtl       MSAps1dtl
218900080407     c                   add       wPS1nsc       MSAps1nsc
219000080407     c                   add       wPS1nre       MSAps1nre
219100080407     c                   add       wPS1dok       MSAps1dok
219200080407     c                   add       wPS1dno       MSAps1dno
219300080407     c                   add       wPS2dnr       MSAps2dnr
219400080407     c                   add       wPS2dts       MSAps2dts
219500080407     c                   add       wPS2dtl       MSAps2dtl
219600080407     c                   add       wPS2nsc       MSAps2nsc
219700080407     c                   add       wPS2nre       MSAps2nre
219800080407     c                   add       wPS2dok       MSAps2dok
219900080407     c                   add       wPS2dno       MSAps2dno
220000080407     c                   add       wPESnes       MSApesnes
220100080407     c                   add       wPESind       MSApesind
220200080407     c                   add       wPESune       MSApesune
220300080407     c                   add       wPUSnes       MSApusnes
220400080407     c                   add       wPUSsor       MSApussor
220500080407     c                   add       wPUSlos       MSApuslos
220600080407     c                   add       $PEP(01)      MSApep001
220700080407     c                   add       $PEP(02)      MSApep002
220800080407     c                   add       $PEP(03)      MSApep003
220900080407     c                   add       $PEP(04)      MSApep004
221000080407     c                   add       $PEP(05)      MSApep005
221100080407     c                   add       $PEP(06)      MSApep006
221200080407     c                   add       $PEP(07)      MSApep007
221300080407     c                   add       $PEP(08)      MSApep008
221400080407     c                   add       $PEP(09)      MSApep009
221500080407     c                   add       $PEP(10)      MSApep010
221600080407     c                   add       $PEP(11)      MSApep011
221700080407     c                   add       $PEP(12)      MSApep012
221800080407     c                   add       $PEP(13)      MSApep013
221900080407     c                   add       $PEP(14)      MSApep014
222000080407     c                   add       $PEP(15)      MSApep015
222100080407     c                   add       $PEP(16)      MSApep016
222200080407     c                   add       $PEP(17)      MSApep017
222300080407     c                   add       $PEP(18)      MSApep018
222400080407     c                   add       $PEP(19)      MSApep019
222500080407     c                   add       $PEP(20)      MSApep020
222600080407     c                   add       $PAD(01)      MSApad001
222700080407     c                   add       $PAD(02)      MSApad002
222800080407     c                   add       $PAD(03)      MSApad003
222900080407     c                   add       $PAD(04)      MSApad004
223000080407     c                   add       $PAD(05)      MSApad005
223100080407     c                   add       $PAD(06)      MSApad006
223200080407     c                   add       $PAD(07)      MSApad007
223300080407     c                   add       $PAD(08)      MSApad008
223400080407     c                   add       $PAD(09)      MSApad009
223500080407     c                   add       $PAD(10)      MSApad010
223600080407     c                   add       $PAD(11)      MSApad011
223700080407     c                   add       $PAD(12)      MSApad012
223800080407     c                   add       $PAD(13)      MSApad013
223900080407     c                   add       $PAD(14)      MSApad014
224000080407     c                   add       $PAD(15)      MSApad015
224100080407     c                   add       $PAD(16)      MSApad016
224200080407     c                   add       $PAD(17)      MSApad017
224300080407     c                   add       $PAD(18)      MSApad018
224400080407     c                   add       $PAD(19)      MSApad019
224500080407     c                   add       $PAD(20)      MSApad020
224600080407     c                   add       $PAD(21)      MSApad021
224700080407     c                   add       $PAD(22)      MSApad022
224800080407     c                   add       $PAD(23)      MSApad023
224900080407     c                   add       $PAD(24)      MSApad024
225000080407     c                   add       $PAD(25)      MSApad025
225100080407     c                   add       $PAD(26)      MSApad026
225200080407     c                   add       $PAD(27)      MSApad027
225300080407     c                   add       $PAD(28)      MSApad028
225400080407     c                   add       $PAD(29)      MSApad029
225500080407     c                   add       $PAD(30)      MSApad030
225600080407     c                   add       $PAD(31)      MSApad031
225700080407     c                   add       $PAD(32)      MSApad032
225800080407     c                   add       $PAD(33)      MSApad033
225900080407     c                   add       $PAD(34)      MSApad034
226000080407     c                   add       $PAD(35)      MSApad035
226100080407     c                   add       $PAD(36)      MSApad036
226200080407     c                   add       $PAD(37)      MSApad037
226300080407     c                   add       $PAD(38)      MSApad038
226400080407     c                   add       $PAD(39)      MSApad039
226500080407     c                   add       $PAD(40)      MSApad040
226600080407     c                   add       $PAD(41)      MSApad041
226700080407     c                   add       $PAD(42)      MSApad042
226800080407     c                   add       $PAD(43)      MSApad043
226900080407     c                   add       $PAD(44)      MSApad044
227000080407     c                   add       $PAD(45)      MSApad045
227100080407     c                   add       $PAD(46)      MSApad046
227200080407     c                   add       $PAD(47)      MSApad047
227300080407     c                   add       $PAD(48)      MSApad048
227400080407     c                   add       $PAD(49)      MSApad049
227500080407     c                   add       $PAD(50)      MSApad050
227600080407     c                   add       $PAD(51)      MSApad051
227700080407     c                   add       $PAD(52)      MSApad052
227800080407     c                   add       $PAD(53)      MSApad053
227900080407     c                   add       $PAD(54)      MSApad054
228000080407     c                   add       $PAD(55)      MSApad055
228100080407     c                   add       $PAD(56)      MSApad056
228200080407     c                   add       $PAD(57)      MSApad057
228300080407     c                   add       $PAD(58)      MSApad058
228400080407     c                   add       $PAD(59)      MSApad059
228500080407     c                   add       $PAD(60)      MSApad060
228600080407     c                   add       wCSD          MSAcsd
228700080407     c                   add       wCCP          MSAccp
228800080407     c                   add       wCCV          MSAccv
228900080407     c                   add       wNLS          MSAnls
229000080407     c                   add       wNLP          MSAnlp
229100161213     c*//?non ancora:?   add       wPWF          MSApwf
229200060922      *
229300060922     c                   ENDSR
229400161213
229500161213      *---------------------------------------------------------------*
229600161213      *?Impostazione campi record di FNMSA00F                        ?*
229700161213      *---------------------------------------------------------------*
229800161213     c     sr_RieMSA0    BEGSR
229900161213      *
230000161213     c                   eval      SA0pes    = MSApes
230100161213     c                   eval      SA0vol    = MSAvol
230200161213     c                   eval      SA0odsDiv = MSAodsDiv
230300161213     c                   eval      SA0odsDnr = MSAodsDnr
230400161213     c                   eval      SA0odsDna = MSAodsDna
230500161213     c                   eval      SA0odsFtd = MSAodsFtd
230600161213     c                   eval      SA0odsIds = MSAodsIds
230700161213     c                   eval      SA0odsUds = MSAodsUds
230800161213     c                   eval      SA0odsDne = MSAodsDne
230900161213     c                   eval      SA0odsCiv = MSAodsCiv
231000161213     c                   eval      SA0odsDnq = MSAodsDnq
231100161213     c                   eval      SA0odsUst = MSAodsUst
231200161213     c                   eval      SA0dtsNts = MSAdtsNts
231300161213     c                   eval      SA0dtsTok = MSAdtsTok
231400161213     c                   eval      SA0dtsIla = MSAdtsIla
231500161213     c                   eval      SA0dtsNrl = MSAdtsNrl
231600161213     c                   eval      SA0dtsNdf = MSAdtsNdf
231700161213     c                   eval      SA0ps1Dnr = MSAps1Dnr
231800161213     c                   eval      SA0ps1Dts = MSAps1Dts
231900161213     c                   eval      SA0ps1Dtl = MSAps1Dtl
232000161213     c                   eval      SA0ps1Nsc = MSAps1Nsc
232100161213     c                   eval      SA0ps1Nre = MSAps1Nre
232200161213     c                   eval      SA0ps1Dok = MSAps1Dok
232300161213     c                   eval      SA0ps1Dno = MSAps1Dno
232400161213     c                   eval      SA0ps2Dnr = MSAps2Dnr
232500161213     c                   eval      SA0ps2Dts = MSAps2Dts
232600161213     c                   eval      SA0ps2Dtl = MSAps2Dtl
232700161213     c                   eval      SA0ps2Nsc = MSAps2Nsc
232800161213     c                   eval      SA0ps2Nre = MSAps2Nre
232900161213     c                   eval      SA0ps2Dok = MSAps2Dok
233000161213     c                   eval      SA0ps2Dno = MSAps2Dno
233100161213     c                   eval      SA0pesNes = MSApesNes
233200161213     c                   eval      SA0pesInd = MSApesInd
233300161213     c                   eval      SA0pesUne = MSApesUne
233400161213     c                   eval      SA0pusNes = MSApusNes
233500161213     c                   eval      SA0pusSor = MSApusSor
233600161213     c                   eval      SA0pusLos = MSApusLos
233700161213     c                   eval      SA0pep001 = MSApep001
233800161213     c                   eval      SA0pep002 = MSApep002
233900161213     c                   eval      SA0pep003 = MSApep003
234000161213     c                   eval      SA0pep004 = MSApep004
234100161213     c                   eval      SA0pep005 = MSApep005
234200161213     c                   eval      SA0pep006 = MSApep006
234300161213     c                   eval      SA0pep007 = MSApep007
234400161213     c                   eval      SA0pep008 = MSApep008
234500161213     c                   eval      SA0pep009 = MSApep009
234600161213     c                   eval      SA0pep010 = MSApep010
234700161213     c                   eval      SA0pep011 = MSApep011
234800161213     c                   eval      SA0pep012 = MSApep012
234900161213     c                   eval      SA0pep013 = MSApep013
235000161213     c                   eval      SA0pep014 = MSApep014
235100161213     c                   eval      SA0pep015 = MSApep015
235200161213     c                   eval      SA0pep016 = MSApep016
235300161213     c                   eval      SA0pep017 = MSApep017
235400161213     c                   eval      SA0pep018 = MSApep018
235500161213     c                   eval      SA0pep019 = MSApep019
235600161213     c                   eval      SA0pep020 = MSApep020
235700161213     c                   eval      SA0pad001 = MSApad001
235800161213     c                   eval      SA0pad002 = MSApad002
235900161213     c                   eval      SA0pad003 = MSApad003
236000161213     c                   eval      SA0pad004 = MSApad004
236100161213     c                   eval      SA0pad005 = MSApad005
236200161213     c                   eval      SA0pad006 = MSApad006
236300161213     c                   eval      SA0pad007 = MSApad007
236400161213     c                   eval      SA0pad008 = MSApad008
236500161213     c                   eval      SA0pad009 = MSApad009
236600161213     c                   eval      SA0pad010 = MSApad010
236700161213     c                   eval      SA0pad011 = MSApad011
236800161213     c                   eval      SA0pad012 = MSApad012
236900161213     c                   eval      SA0pad013 = MSApad013
237000161213     c                   eval      SA0pad014 = MSApad014
237100161213     c                   eval      SA0pad015 = MSApad015
237200161213     c                   eval      SA0pad016 = MSApad016
237300161213     c                   eval      SA0pad017 = MSApad017
237400161213     c                   eval      SA0pad018 = MSApad018
237500161213     c                   eval      SA0pad019 = MSApad019
237600161213     c                   eval      SA0pad020 = MSApad020
237700161213     c                   eval      SA0pad021 = MSApad021
237800161213     c                   eval      SA0pad022 = MSApad022
237900161213     c                   eval      SA0pad023 = MSApad023
238000161213     c                   eval      SA0pad024 = MSApad024
238100161213     c                   eval      SA0pad025 = MSApad025
238200161213     c                   eval      SA0pad026 = MSApad026
238300161213     c                   eval      SA0pad027 = MSApad027
238400161213     c                   eval      SA0pad028 = MSApad028
238500161213     c                   eval      SA0pad029 = MSApad029
238600161213     c                   eval      SA0pad030 = MSApad030
238700161213     c                   eval      SA0pad031 = MSApad031
238800161213     c                   eval      SA0pad032 = MSApad032
238900161213     c                   eval      SA0pad033 = MSApad033
239000161213     c                   eval      SA0pad034 = MSApad034
239100161213     c                   eval      SA0pad035 = MSApad035
239200161213     c                   eval      SA0pad036 = MSApad036
239300161213     c                   eval      SA0pad037 = MSApad037
239400161213     c                   eval      SA0pad038 = MSApad038
239500161213     c                   eval      SA0pad039 = MSApad039
239600161213     c                   eval      SA0pad040 = MSApad040
239700161213     c                   eval      SA0pad041 = MSApad041
239800161213     c                   eval      SA0pad042 = MSApad042
239900161213     c                   eval      SA0pad043 = MSApad043
240000161213     c                   eval      SA0pad044 = MSApad044
240100161213     c                   eval      SA0pad045 = MSApad045
240200161213     c                   eval      SA0pad046 = MSApad046
240300161213     c                   eval      SA0pad047 = MSApad047
240400161213     c                   eval      SA0pad048 = MSApad048
240500161213     c                   eval      SA0pad049 = MSApad049
240600161213     c                   eval      SA0pad050 = MSApad050
240700161213     c                   eval      SA0pad051 = MSApad051
240800161213     c                   eval      SA0pad052 = MSApad052
240900161213     c                   eval      SA0pad053 = MSApad053
241000161213     c                   eval      SA0pad054 = MSApad054
241100161213     c                   eval      SA0pad055 = MSApad055
241200161213     c                   eval      SA0pad056 = MSApad056
241300161213     c                   eval      SA0pad057 = MSApad057
241400161213     c                   eval      SA0pad058 = MSApad058
241500161213     c                   eval      SA0pad059 = MSApad059
241600161213     c                   eval      SA0pad060 = MSApad060
241700161213     c                   eval      SA0tcl    = MSAtcl
241800161213     c                   eval      SA0csd    = MSAcsd
241900161213     c                   eval      SA0ccp    = MSAccp
242000161213     c                   eval      SA0ccv    = MSAccv
242100161213     c                   eval      SA0nls    = MSAnls
242200161213     c                   eval      SA0nlp    = MSAnlp
242300161213      *
242400161213     c                   eval      SA0pwf   += wPWF
242500161213     c*//·               eval      SA0lav    = KSAlav
242600161213     c*//·               eval      SA0imp    = KSAimp
242700161213      *
242800161213     c                   ENDSR
