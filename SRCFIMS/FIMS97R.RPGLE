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
002200171107     fFLTR902L  if   e           k disk    usropn
002300060920      *
002400090213     fFITR401L  Uf   e           K disk
002500060920      *
002600161213     fFNMSA03L  Uf A e           k disk    rename( FNMSA000 : FNMSA003 )
002700080708     fFIMSC01L  Uf A e           k disk
002800060920      *
002900080108     fFIBAR00F  O    e             disk
003000060920     fFLTR700F  O    e             disk
003100160215     fFIBARDPD0FO    e             disk
003200040422
003300060920      *---------------------------------------------------------------*
003400060920
003500060920      *
003600060920      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
003700060920      *
003800060920      * - Numero Pistola Spunta colli
003900160502     d c_npsTRANSP     c                   const(46)
004000060920     d C_npsSTD        c                   const(49)
004100060920     d C_npsRPVP       c                   const(48)
004200060920     d C_npsPRES       c                   const(50)
004300060920      *
004400060920      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
004500060920      *
004600060920      * - Layout
004700160707     d $MSL            s                   dim( 80) inz  like(§MSLpep)
004800160707     d $MSLn           s                   dim( 80) inz  like(§MSLent)
004900160707     d $MSLa           s                   dim( 80) inz  like(§MSLapl)
005000161129     d $MSLa5          s                   dim( 80) inz  like(§MSLap5)
005100160707     d $MSLe           s                   dim( 80) inz  like(§MSLpke)
005200161213     d $MSLw           s                   dim( 80) inz  like(§MSLpwf)
005300060920      * - Civette
005400080619     d $MSCdat         s             15    dim(100) inz                         *segnacollo
005500060926     d $MSCpes         s                   dim(100) inz  like(§MSCpes)          *peso
005600060926     d $MSCvol         s                   dim(100) inz  like(§MSCvol)          *volume
005700060921      * - di comodo (per "raggruppare" campi legati)
005800060921     d ds_PEP          ds                  inz
005900060921     d   wPEP001                      1  0 inz
006000060921     d   wPEP002                      1  0 inz
006100060921     d   wPEP003                      1  0 inz
006200060921     d   wPEP004                      1  0 inz
006300060921     d   wPEP005                      1  0 inz
006400060921     d   wPEP006                      1  0 inz
006500060921     d   wPEP007                      1  0 inz
006600060921     d   wPEP008                      1  0 inz
006700060921     d   wPEP009                      1  0 inz
006800060921     d   wPEP010                      1  0 inz
006900060921     d   wPEP011                      1  0 inz
007000060921     d   wPEP012                      1  0 inz
007100060921     d   wPEP013                      1  0 inz
007200060921     d   wPEP014                      1  0 inz
007300060921     d   wPEP015                      1  0 inz
007400060921     d   wPEP016                      1  0 inz
007500060921     d   wPEP017                      1  0 inz
007600060921     d   wPEP018                      1  0 inz
007700060921     d   wPEP019                      1  0 inz
007800060921     d   wPEP020                      1  0 inz
007900060921     d  $PEP                   1     20  0 dim(20)
008000060921     d ds_PAD          ds                  inz
008100060921     d   wPAD001                      1  0 inz
008200060921     d   wPAD002                      1  0 inz
008300060921     d   wPAD003                      1  0 inz
008400060921     d   wPAD004                      1  0 inz
008500060921     d   wPAD005                      1  0 inz
008600060921     d   wPAD006                      1  0 inz
008700060921     d   wPAD007                      1  0 inz
008800060921     d   wPAD008                      1  0 inz
008900060921     d   wPAD009                      1  0 inz
009000060921     d   wPAD010                      1  0 inz
009100060921     d   wPAD011                      1  0 inz
009200060921     d   wPAD012                      1  0 inz
009300060921     d   wPAD013                      1  0 inz
009400060921     d   wPAD014                      1  0 inz
009500060921     d   wPAD015                      1  0 inz
009600060921     d   wPAD016                      1  0 inz
009700060921     d   wPAD017                      1  0 inz
009800060921     d   wPAD018                      1  0 inz
009900060921     d   wPAD019                      1  0 inz
010000060921     d   wPAD020                      1  0 inz
010100060921     d   wPAD021                      1  0 inz
010200060921     d   wPAD022                      1  0 inz
010300060921     d   wPAD023                      1  0 inz
010400060921     d   wPAD024                      1  0 inz
010500060921     d   wPAD025                      1  0 inz
010600060921     d   wPAD026                      1  0 inz
010700060921     d   wPAD027                      1  0 inz
010800060921     d   wPAD028                      1  0 inz
010900060921     d   wPAD029                      1  0 inz
011000060921     d   wPAD030                      1  0 inz
011100060921     d   wPAD031                      1  0 inz
011200060921     d   wPAD032                      1  0 inz
011300060921     d   wPAD033                      1  0 inz
011400060921     d   wPAD034                      1  0 inz
011500060921     d   wPAD035                      1  0 inz
011600060921     d   wPAD036                      1  0 inz
011700060921     d   wPAD037                      1  0 inz
011800060921     d   wPAD038                      1  0 inz
011900060921     d   wPAD039                      1  0 inz
012000060921     d   wPAD040                      1  0 inz
012100060921     d   wPAD041                      1  0 inz
012200060921     d   wPAD042                      1  0 inz
012300060921     d   wPAD043                      1  0 inz
012400060921     d   wPAD044                      1  0 inz
012500060921     d   wPAD045                      1  0 inz
012600060921     d   wPAD046                      1  0 inz
012700060921     d   wPAD047                      1  0 inz
012800060921     d   wPAD048                      1  0 inz
012900060921     d   wPAD049                      1  0 inz
013000060921     d   wPAD050                      1  0 inz
013100060921     d   wPAD051                      1  0 inz
013200060921     d   wPAD052                      1  0 inz
013300060921     d   wPAD053                      1  0 inz
013400060921     d   wPAD054                      1  0 inz
013500060921     d   wPAD055                      1  0 inz
013600060921     d   wPAD056                      1  0 inz
013700060921     d   wPAD057                      1  0 inz
013800060921     d   wPAD058                      1  0 inz
013900060921     d   wPAD059                      1  0 inz
014000060921     d   wPAD060                      1  0 inz
014100060921     d  $PAD                   1     60  0 dim(60)
014200060920      *
014300060920      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
014400060920      *
014500060920      * - Parametri per richiamo al pgm di controllo profilo utenti
014600060920     d TIBS34ds      e ds
014700060920      * - Ds di riferimento al file esterno AzUte00F
014800060920     d AZUTEds       e ds                  extname(AZUTE00F)
014900060920      * - Ds per dati organigramma
015000060920     d DDatiUte      e ds
015100160606      *
015200160606      * -?150ª descrizione dell'organigramma?
015300160606     d Og150         e ds                  qualified  inz
015400060920      *
015500060921      * - Tab. "MFP" = P.O. applicazioni installate
015600060921     d dMFP          e ds                  inz
015700060920      * - Tab. "MSL" = Layout Macchinone Smistacolli
015800060920     d dMSL          e ds                  inz
015900060920      * - Tab. "MSC" = Paccchi Civetta
016000060920     d dMSC          e ds                  inz
016100060925      * - - Composizione segnacollo civetta
016200080619     d   dsSEGNAC             31     45    inz(*zeros)
016300060920      * - Tab. "MPV" = Blocco caricamento volume e/o peso
016400060920     d dMVP          e ds                  inz
016500060920     d  MVP_V_TIMI             2     13  0 inz
016600060920     d  MVP_V_TIMF            14     25  0 inz
016700060920     d  MVP_P_TIMI            27     38  0 inz
016800060920     d  MVP_P_TIMF            39     50  0 inz
016900160325     d  MVP_V_TIMI_R          52     63  0 inz
017000160325     d  MVP_V_TIMF_R          64     75  0 inz
017100160325     d  MVP_P_TIMI_R          77     88  0 inz
017200160325     d  MVP_P_TIMF_R          89    100  0 inz
017300160325     d  MVP_P_TIMI_T         102    113  0 inz
017400160325     d  MVP_P_TIMF_T         114    125  0 inz
017500060920      * - Tab. "TBA" = Tipo Bancali
017600060920     d dTBA          e ds                  inz
017700060920      *
017800060920      * - Calcolo Check-Digit
017900060920     d TRUL28ds      e ds                  inz
018000060920      *
018100060920      * - Campi Formato Record
018200090128     d FITR4ds         ds                  inz
018300061012     d  TR4ctl                 1      1
018400061012     d  TR4npl                 2      3
018500061012     d  TR4nfv                 4      8
018600061012     d  TR4tim                 9     22
018700061012     d  TR4ms1                23     27
018800061012     d  TR4msx                28     28
018900061012     d  TR4ms2                29     31
019000061012     d  TR4cod                32     36
019100061012     d  TR4020                37     38
019200061012     d  TR4f01                39     39
019300061012     d  TR4pic                40     44
019400061012     d  TR4f02                45     45
019500061012     d  TR4pgr                46     51
019600061012     d  TR4f03                52     52
019700061012     d  TR4pod                53     55
019800061012     d  TR4f04                56     56
019900061012     d  TR4ods                57     57
020000061012     d  TR4f05                58     58
020100061012     d  TR4pad                59     61
020200061012     d  TR4f06                62     62
020300061012     d  TR4dts                63     63
020400061012     d  TR4f07                64     64
020500061012     d  TR4ps1                65     65
020600061012     d  TR4pn1                66     67
020700061012     d  TR4pd1                68     97
020800061012     d  TR4f08                98     98
020900061012     d  TR4ps2                99     99
021000061012     d  TR4pn2               100    101
021100061012     d  TR4pd2               102    130
021200061012     d**???                  131    131
021300061012     d  TR4f09               132    132
021400061012     d  TR4ps3               133    133
021500061012     d  TR4pn3               134    135
021600061012     d  TR4pd3               136    165
021700061012     d  TR4f10               166    166
021800061012     d  TR4ps4               167    167
021900061012     d  TR4pn4               168    169
022000061012     d  TR4pd4               170    199
022100061012     d    TR4dm1             181    184
022200061012     d    TR4dm2             185    188
022300061012     d    TR4dm3             189    192
022400061012     d   TR4dm               181    192
022500061012     d  TR4f11               200    200
022600061012     d  TR4pep               201    214
022700061012     d  TR4f12               215    215
022800061012     d  TR4pes               216    216
022900061012     d  TR4f13               217    217
023000061012     d  TR4pup               218    231
023100061012     d  TR4f14               232    232
023200061012     d  TR4pus               233    233
023300061012     d  TR4f15               234    234
023400061012     d  TR4crc               235    236
023500060920      *
023600060920      * - Segnacollo
023700060920     d wBarCode        ds                  inz
023800060920     d  wBar_xx1               1      5    inz
023900060920     d  wBar_xx2               6      6    inz
024000060920     d  wBar_LNP               7      9    inz
024100060920     d  wBar_LNA              10     12    inz
024200060920     d  wBar_NRS              13     14    inz
024300060920     d  wBar_NSC              15     21    inz
024400060920     d  wBar_ZNC              22     23    inz
024500060920     d   wBar_BAR              7     23    inz
024600060920     d  wBar_CHK              24     24    inz
024700060920     d    wBar_BarCo           7     24    inz
024800060920     d  wBar_xx3              25     25    inz
024900060920      *
025000060920     d wBarSped        ds                  inz
025100060920     d  wSpe_xx1               1      9    inz
025200060920     d  wSpe_AA2              10     11    inz
025300060920     d  wSpe_LNP              12     14    inz
025400060920     d  wSpe_NRS              15     16    inz
025500060920     d  wSpe_NSP              17     23    inz
025600060920     d  wSpe_CHK              24     24    inz
025700060920     d   wSpe_BarCo           10     24    inz
025800060920     d  wSpe_xx2              25     25    inz
025900060920      *
026000090128      * - Segnacollo x FITR6
026100060920     d kTR6who15       ds            15    inz
026200060920     d  BARlnp                             inz
026300060920     d  BARlna                             inz
026400060920     d  BARnrs                             inz
026500060920     d  BARnsc                             inz
026600060920      *
026700060920      * - Controllo data
026800060920     d WLBda8          ds                  inz
026900060920     d  G08dat                        8  0 inz
027000060920     d  G08inv                        8  0 inz
027100060928     d  G08err                        1    inz('3')
027200060920     d  G08tgi                        5  0 inz
027300060920      *
027400060920     d dsTIM           ds            14    inz                                  *data + ora
027500060920     d  dsDAT                         8  0 inz                                  *data (aaaa+mm+gg)
027600060920     d  dsORA                         4  0 inz                                  *ora  (hh+mm)
027700060920      *
027800060920      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
027900060920      *
028000060920      * - Parametri ricevuti
028100060920     d PARfil          s              3                                         *p.o. che spunta
028200060920     d wPO             s                   like(ORGfil)
028300060920      *
028400060920      * - Flags booleani
028500060920     d $CK0err         s              1    inz(*off)
028600060920     d $CK1err         s              1    inz(*off)
028700060920     d $CK2err         s              1    inz(*off)
028800060926     d $okPD1          s              1    inz(*off)
028900060926     d $okPD2          s              1    inz(*off)
029000060920     d $BarCiv         s              1    inz(*off)
029100060920      *
029200060920      * - Indici di schiera
029300060920     d xx              s              5  0 inz
029400060920      *
029500060926      * Campi chiave riferiti al data base
029600060920     d KSAapl          s                   like(MSAapl)    inz
029700060920     d KSAdtl          s                   like(MSAdtl)    inz
029800060920     d KSAorl          s                   like(MSAorl)    inz
029900070725     d KSAnpg          s                   like(MSAnpg)    inz
030000060920     d KSCdtl          s                   like(MSCdtl)    inz
030100080708     d KSCIorl         s                   like(MSCorl)    inz
030200060920     d KSCseg          s                   like(MSCseg)    inz
030300161213      *//* -?Nuovi campi (chiave, ma non ancora) riferiti al data base?
030400170302     d*// KSAimp          s                   like(MSAimp)    inz
030500170302     d*// KSAlav          s                   like(MSAlav)    inz
030600060926      *
030700060926      * Altre avriabili riferite al data base
030800060926     d wBARnfv         s                   like(BARnfv)    inz
030900060926     d wBARnpg         s                   like(BARnpg)    inz
031000060926     d wBARpuc         s                   like(BARpuc)    inz
031100060926     d wBARvuc         s                   like(BARvuc)    inz
031200060926      *
031300060921      * Variabili per conteggio dati da aggiornare in FNMSA00F
031400060920     d wPES            s                   like(MSApes)    inz
031500060920     d wVOL            s                   like(MSAvol)    inz
031600060926     d wODSdiv         s              1  0 inz                                  like(MSAodsdiv) inz
031700060926     d wODSdnr         s              1  0 inz                                  like(MSAodsdnr) inz
031800060926     d wODSdna         s              1  0 inz                                  like(MSAodsdna) inz
031900060926     d wODSftd         s              1  0 inz                                  like(MSAodsftd) inz
032000060926     d wODSids         s              1  0 inz                                  like(MSAodsids) inz
032100060926     d wODSuds         s              1  0 inz                                  like(MSAodsuds) inz
032200060926     d wODSdne         s              1  0 inz                                  like(MSAodsdne) inz
032300060926     d wODSciv         s              1  0 inz                                  like(MSAodsciv) inz
032400060926     d wODSdnq         s              1  0 inz                                  like(MSAodsdnq) inz
032500060926     d wODSust         s              1  0 inz                                  like(MSAodsust) inz
032600060926     d wDTSnts         s              1  0 inz                                  like(MSAdtsnts) inz
032700060926     d wDTStok         s              1  0 inz                                  like(MSAdtstok) inz
032800060926     d wDTSila         s              1  0 inz                                  like(MSAdtsila) inz
032900060926     d wDTSnrl         s              1  0 inz                                  like(MSAdtsnrl) inz
033000060926     d wDTSndf         s              1  0 inz                                  like(MSAdtsndf) inz
033100060926     d wPS1dnr         s              1  0 inz                                  like(MSAps1dnr) inz
033200060926     d wPS1dts         s              1  0 inz                                  like(MSAps1dts) inz
033300060926     d wPS1dtl         s              1  0 inz                                  like(MSAps1dtl) inz
033400060926     d wPS1nsc         s              1  0 inz                                  like(MSAps1nsc) inz
033500060926     d wPS1nre         s              1  0 inz                                  like(MSAps1nre) inz
033600060926     d wPS1dok         s              1  0 inz                                  like(MSAps1dok) inz
033700060926     d wPS1dno         s              1  0 inz                                  like(MSAps1dno) inz
033800060926     d wPS2dnr         s              1  0 inz                                  like(MSAps2dnr) inz
033900060926     d wPS2dts         s              1  0 inz                                  like(MSAps2dts) inz
034000060926     d wPS2dtl         s              1  0 inz                                  like(MSAps2dtl) inz
034100060926     d wPS2nsc         s              1  0 inz                                  like(MSAps2nsc) inz
034200060926     d wPS2nre         s              1  0 inz                                  like(MSAps2nre) inz
034300060926     d wPS2dok         s              1  0 inz                                  like(MSAps2dok) inz
034400060926     d wPS2dno         s              1  0 inz                                  like(MSAps2dno) inz
034500060926     d wPESnes         s              1  0 inz                                  like(MSApesnes) inz
034600060926     d wPESind         s              1  0 inz                                  like(MSApesind) inz
034700060926     d wPESune         s              1  0 inz                                  like(MSApesune) inz
034800060926     d wPUSnes         s              1  0 inz                                  like(MSApusnes) inz
034900060926     d wPUSsor         s              1  0 inz                                  like(MSApussor) inz
035000060926     d wPUSlos         s              1  0 inz                                  like(MSApuslos) inz
035100060926     d wCSD            s              1  0 inz                                  like(MSAcsd)    inz
035200060926     d wCCP            s              1  0 inz                                  like(MSAccp)    inz
035300060926     d wCCV            s              1  0 inz                                  like(MSAccv)    inz
035400060926     d wNLS            s              1  0 inz                                  like(MSAnls)    inz
035500060926     d wNLP            s              1  0 inz                                  like(MSAnlp)    inz
035600161213     d wPWF            s              1  0 inz                                  like(MSApwf)    inz
035700060920      *
035800060920      * - Campi di comodo
035900060926     d wPD0ok          s              3    inz
036000060926     d wPD1ok          s              3    inz
036100060926     d wPD2ok          s              3    inz
036200060926     d wTR7pgr         s              6    inz
036300060926     d wTR7pad2        s              2    inz
036400060926     d wTR4pad         s              3    inz
036500060926     d wSPU_time       s             12  0 inz
036600060920     d wScan           s              5  0 inz
036700060920     d wScan1          s              5  0 inz
036800060920     d wScan2          s              5  0 inz
036900060920     d wDM1            s              4  3 inz
037000060920     d wDM2            s              4  3 inz
037100060920     d wDM3            s              4  3 inz
037200060920     d w003            s              3  0 inz
037300060920     d w006            s              6  0 inz
037400061009     d w096            s              9  6 inz
037500080108     d w140            s             14  0 inz
037600080619     d wtr7bar         s             15    inz
037700160502
037800160502       //--------------------------------------------------------------
037900160502       //?Definizione prototipi procedure.                             ?
038000160502       //--------------------------------------------------------------
038100160502
038200160511       // -?Caricamento £x?
038300160511     d ds_L1           ds                  inz
038400160511     d   sk_L1                        3    dim(30)  inz
038500160511     d TRUL06ds      e ds                  inz
038600160511     d KPJBA         e ds                  inz
038700160511      /copy gaitrasrc/srcProtoPR,TRUL06R
038800160502
038900060920      *
039000060920      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
039100060920      *
039200160502       // -?File FNFVV08L?
039300160502     d keyFNFVV08    e ds                  extname( FNFVV08L : *key )
039400160502     d                                     prefix( k_ )   inz
039500170302      * - FNMSA03L
039600070725     c     K05MSA03      klist                                                  *fnmsa03l
039700070725     c                   kfld                    KSAapl                          -tipo applicazione
039800060920     c                   kfld                    wPO                             -punto operativo
039900060920     c                   kfld                    KSAdtl                          -data
040000060920     c                   kfld                    KSAorl                          -ora
040100070725     c                   kfld                    wBARnpg                         -categoria foglio
040200080619      * - FIMSC01L
040300080619     c     K04IMSC01     klist                                                  *fimsc01l
040400080619     c                   kfld                    wPO                             -punto operativo
040500080619     c                   kfld                    KSCdtl                          -data
040600080619     c                   kfld                    KSCIorl                         -ora
040700080619     c                   kfld                    KSCseg                          -segnacollo
040800060920      * - FNFVV01L
040900060921     c     K03FVV01      klist                                                  *fnfvv01l
041000060921     c                   kfld                    wBARnpg                         -categoria foglio
041100060921     c                   kfld                    FVVnfv                          -numero foglio
041200060920     c                   kfld                    wPO                             -filiale gestione
041300171107      * - FLTR902L
041400171107     c     K05TR902      klist
041500060920     c                   kfld                    wPO
041600060920     c                   kfld                    wBarNPG
041700060921     c                   kfld                    TR9nfv
041800171107     c                   kfld                    TR9tps
041900060921     c                   kfld                    TR9pep
042000060920      * - TNTBE01L
042100060921     c     K02TBE01      klist                                                  *tntbe01l
042200060920     c                   kfld                    TBEcod                          -tabella
042300060920     c                   kfld                    TBEke1                          -chiave uno
042400090129      * - FITR602L
042500090129     c     K02tr6        klist
042600090129     c                   kfld                    tr4fil
042700090129     c                   kfld                    tr4pc
042800090129     c                   kfld                    ktr6who15
042900060920
043000060920      *===============================================================*
043100060920      *?  I N D I C A T O R I                                        ?*
043200060920      *---------------------------------------------------------------*
043300060920      *  10 - Comodo                                                  *
043400060920      *  11 -   "                                                     *
043500060920      *  12 -   "                                                     *
043600060920      *  88 - Utilizzare *** SOLO *** per write FLTR7                 *
043700060920      *  96 - Utilizzare *** SOLO *** per correttezza check digit     *
043800060920      *  97 - Utilizzare *** SOLO *** per test baia entrata tabellata *
043900090128      *  98 - Utilizzare *** SOLO *** per letture record FITR4        *
044000060920      *  99 - Utilizzare *** SOLO *** per correttezza barcode         *
044100060920      *===============================================================*
044200060920
044300060920      *===============================================================*
044400060920      *?  N O T E                                                    ?*
044500060920      *---------------------------------------------------------------*
044600060920      * Il campo TR4020 indica il tipo di impianto da cui si ricevono *
044700060920      *   i rcd:                                                      *
044800060920      * · "20" VDL                                                    *
044900060920      * · "21" DKTC                                                   *
045000060920      * · "22" RPVP - Tipo non gestito                                *
045100060920      * · "23" PICKING VDL PC VDL                                     *
045200170125      * · "24" PICKING NO VDL - Barcode BRT                           *
045300060920      * · "25" PICKING NO VDL - Barcode spedizione                    *
045400170125      * · "26" PICKING VDL PC NON VDL - Barcode BRT                   *
045500060920      * · "27" PICKING VDL PC NON VDL - Barcode spedizione            *
045600170125      * · "36" PICKING VDL PC NON VDL - Barcode Picking DPD           *
045700060920      * · "2C" RPVP - Tipo 'C' collo peso lordo                       *
045800060920      * · "2K" RPVP - Tipo 'K' collo peso netto                       *
045900160502      * · "2T" TRANSPALLET Pesatore WiFi                              *
046000060920      *===============================================================*
046100031222
046200060920     c     *Entry        plist
046300060920     c                   parm                    PARfil                         *filiale che spunta
046400090128     c                   parm                    flag              1
046500090128     c                   parm                    parspu            1
046600090128
046700090128     c                   clear                   flag
046800090128     c                   clear                   parspu
046900060920      *
047000060920      * Operazioni Iniziali
047100060920     c                   exsr      RoutInz
047200060920      *
047300060920      * Ciclo di lettura spunte
047400090213     c     parfil        setll     fitr4
047500090213     c     parfil        reade     FITR4                                  98
047600060920      *
047700000000do  1c                   DOW       NOT *in98
047800060920      *
047900060920      * - Valorizzazione campi del file storico (FLTR700F)
048000060920     c                   exsr      sr_RieTR7
048100060920      *
048200060920      * - Scrittura dello STORICO
048300060920     c                   WRITE     FLTR7000                             88
048400060926      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
048500160610      * - Scrittura del Segnacollo DPD spuntato senza rec. bolla
048600160610      *   · SE LETTURA SCANNER NON VALIDA        E
048700160610      *   · SE LETTURA PISTOLA = "CHI SONO" DPD
048800160629      *   (solo per Tipo Lavorazione = 5)
048900160610     c                   if             *in88  = *off
049000160610     c                             and  TR7cod = *zeros
049100160629     c                             and  TR7ctl = '5'
049200160610     c                             and (%scan('????' : TR7pd1)
049300160610     c                                         > *zeros       OR
049400160610     c                                  TR7pd1 = *blanks      OR
049500160610     c                                  wPD1ok = 'ERR')
049600160610     c                             and  %subst(TR7pd2:  1: 2) = ' %'
049700160610     c                             and  %subst(TR7pd2: 30: 1) = '#'
049800160610     c                             and  %subst(TR7pd2: 10: 4) <>
049900160610     c                                           '0000'
050000160610     c                   exsr      Wrt_BARDPD
050100160610     c                   endif
050200160610      *
050300060920      * - Scrittura dei SALDI
050400060925if  2c                   if             *in88  = *off
050500060920     c                             and  TR4cod = *zeros
050600060920     c                             and (TR4ctl = '2'
050700161214     c                              or  TR4ctl = '3'
050800060920     c                              or  TR4ctl = '5'
050900060920     c                              or  TR4ctl = '6')
051000060920     c                   exsr      Wrt_SAL
051100060920e   2c                   endif
051200060920      *
051300060920      * - Cancellazione record
051400090128     c                   DELETE    FITR4
051500060926      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051600060920      * - Lettura record successivo spunte
051700090213     c     parfil        reade     FITR4                                  98
051800060920      *
051900060920e   1c                   ENDDO
052000060920      *
052100060920      * Fine
052200090128     c                   feod      fibar00f
052300160219     c                   feod      FIBARDPD0F
052400090128     c                   shtdn                                        95
052500090128     c                   if        not *in95
052600090128     c                   seton                                        rt
052700090128     c                   else
052800090128     c                   eval      flag='1'
052900090128     c                   seton                                        lr
053000090128     c                   endif
053100060921
053200060921      *---------------------------------------------------------------*
053300060921      *?Operazioni iniziali                                          ?*
053400060921      *---------------------------------------------------------------*
053500060921     c     RoutInz       BEGSR
053600060921      *
053700060921      * Reperimento dati job
053800060921     c                   exsr      DatiJob
053900060921      *
054000060921     c                   move      PARfil        wPO                            *parametro numerico
054100160606      *
054200160606     c     wPO           chain     AZORG
054300160606if  1c                   if        %found(AZORG01L)
054400160606     c                   eval      Og150 = ORGdf0
054500160606e   1c                   endif
054600060921      *
054700060921      * Caricamento tabelle occorrenti
054800060925     c                   exsr      CarTAB
054900060921      *
055000060921     c                   ENDSR
055100060921
055200060921      *---------------------------------------------------------------*
055300060921      *?Reperimento dati del job (Utente/Operativi)                  ?*
055400060921      *---------------------------------------------------------------*
055500060921     c     DatiJob       BEGSR
055600060921      *
055700060921     c     *dtaara       define    §azute        AZUTEds
055800060921     c     *dtaara       define    §datiute      dDatiUte
055900060921      *
056000060921     c                   in(E)     *dtaara
056100060921     c                   if        %error or RSUT = *blanks
056200060928     c                   clear                   TIBS34ds
056300060921     c                   call      'TIBS34R'
056400060921     c                   parm                    TIBS34ds
056500060921     c                   in        *dtaara
056600060921     c                   endif
056700060921      *
056800060921     c                   ENDSR
056900060921
057000060921      *---------------------------------------------------------------*
057100060921      *?Caricamento tabelle occorrenti                               ?*
057200060921      *---------------------------------------------------------------*
057300060921     c     CarTAB        BEGSR
057400060921      *
057500060921      * Tab. MVP = Blocco caricamento peso e/o volume
057600060921     c                   clear                   dMVP
057700060921     c                   eval      TBEcod    = 'MVP'
057800060921     c                   movel(p)  wPO           TBEke1
057900060921     c     K02TBE01      chain     TNTBE000
058000060921if  1c                   if             %found(TNTBE01L)
058100060921     c                             and  TBEatb = *blanks
058200060921     c                   movel     TBEuni        dMVP
058300060921e   1c                   endif
058400060921      *
058500060921      * Tab. MSL = Layout macchinone smistacolli
058600060921     c                   clear                   $MSL
058700060921     c                   clear                   $MSLn
058800060921     c                   clear                   $MSLa
058900161129     c                   clear                   $MSLa5
059000060921     c                   clear                   $MSLe
059100161213     c                   clear                   $MSLw
059200060921     c                   clear                   xx
059300060921     c                   eval      TBEcod    = 'MSL'
059400060921     c                   movel(p)  wPO           TBEke1
059500060921     c     K02TBE01      setll     TNTBE000
059600060921     c     K02TBE01      reade     TNTBE000
059700060921do  1c                   DOW       NOT %eof(TNTBE01L)
059800060921      * - Solo rec. non annullati e del p.o.
059900060921if  2c                   if             TBEatb = *blanks
060000060921     c                             and  TBEke1 = PARfil
060100060921     c                   add       1             xx
060200060921     c                   movel     TBEuni        dMSL
060300060921     c                   eval      $MSL(xx)  = §MSLpep                          *filiale + entrata
060400060921     c                   eval      $MSLn(xx) = §MSLent                          *n° entrance point
060500161129     c                   eval      $MSLa(xx) = §MSLapl                          *TipoApplic. Cat.2
060600161129     c                   eval      $MSLa5(xx) = §MSLap5                         *TipoApplic. Cat.5
060700161129     c                   eval      $MSLe(xx) = §MSLpke                          *S=Entrata Picking
060800161213     c                   eval      $MSLw(xx) = §MSLpwf                          *W=Pistola Wi-Fi
060900060921e   2c                   endif
061000060921     c     K02TBE01      reade     TNTBE000
061100060921e   1c                   ENDDO
061200060921      *
061300060921      * Tab. MSC = Pacchi civetta
061400060921     c                   clear                   $MSCdat
061500060921     c                   clear                   $MSCpes
061600060921     c                   clear                   $MSCvol
061700060921     c                   clear                   xx
061800060921     c                   eval      TBEcod    = 'MSC'
061900060921     c     TBEcod        setll     TNTBE000
062000060921     c     TBEcod        reade     TNTBE000
062100060921do  1c                   DOW       NOT %eof(TNTBE01L)
062200060921if  2c                   if        TBEatb    = *blanks
062300060921     c                   add       1             xx
062400060921     c                   movel     TBEuni        dMSC
062500060921     c                   movel     dsSEGNAC      $MSCdat(xx)                    *segnacollo
062600060921     c                   eval      $MSCpes(xx) = §MSCpes                        *peso
062700060921     c                   eval      $MSCvol(xx) = §MSCvol                        *volume
062800060921e   2c                   endif
062900060921     c     TBEcod        reade     TNTBE000
063000060921e   1c                   ENDDO
063100060921      *
063200060921      * Tipo applicazione principale del p.o.
063300060921     c                   clear                   dMFP
063400060921     c                   eval      TBEcod    = 'MFP'
063500060921     c                   movel(p)  wPO           TBEke1
063600060921     c     K02TBE01      chain     TNTBE000
063700060921if  1c                   if             %found(TNTBE01L)
063800060921     c                             and  TBEatb = *blanks
063900060921     c                   movel     TBEuni        dMFP
064000060921e   1c                   endif
064100060921      *
064200060921      * Tara bancali per RPVP
064300060921     c                   clear                   dTBA
064400060921     c                   eval      TBEcod    = 'TBA'
064500060921     c                   movel(p)  '01'          TBEke1
064600060921     c     K02TBE01      chain     TNTBE000
064700060921if  1c                   if             %found(TNTBE01L)
064800060921     c                             and  TBEatb = *blanks
064900060921     c                   movel     TBEuni        dTBA
065000060921e   1c                   endif
065100160502      *
065200160502      /free
065300160502
065400160502         // -?Reperimento (temporaneamente) Data & Ora correnti?
065500160502         dsTIM = %subst( %char( %dec( %timestamp() ) )
065600160502                         : 1 : 14 );
065700160502
065800160511         // -?Reperimento Filiali dell'Area ("£1")?
065900160511         clear  sk_L1;
066000160511         clear  KPJBU;
066100160511         clear  TRUL06ds;
066200160511         D06cod = '£1';
066300160511         D06key = PARfil;
066400160511         D06tla = 'L';
066500160511         D06drf = dsDAT;
066600160511         KPJBU  = TRUL06ds;
066700160511         trul06r ( KPJBA );
066800160511         TRUL06ds = KPJBU;
066900160511         ds_L1  = D06lia;
067000160502
067100160502      /end-free
067200060921      *
067300060921     c                   ENDSR
067400060920
067500060920      *---------------------------------------------------------------*
067600060920      *?Caricamento dati nel record di FLTR7                         ?*
067700060920      *---------------------------------------------------------------*
067800060920     c     sr_RieTR7     BEGSR
067900060920      *
068000060920     c                   eval      *in88   = *off
068100060920     c                   eval      *in99   = *off
068200060920     c                   eval      *in96   = *off
068300060920     c                   eval      $CK0err = *off
068400060920     c                   eval      $CK1err = *off
068500060920     c                   eval      $CK2err = *off
068600060926     c                   clear                   wPD0ok
068700060926     c                   clear                   wPD1ok
068800060926     c                   clear                   wPD2ok
068900060920     c                   clear                   BarCo
069000060920      *
069100060920      * Suddivide il formato record nei campi della DS
069200090128     c                   movel     TR4da1        FITR4ds
069300060920      *
069400060920      * Imposta i campi fissi del barcode
069500070212     c                   eval      BARerr  = ' '
069600070212     c                   eval      BARman  = ' '
069700070212     c                   eval      BARdup  = ' '
069800070212     c                   eval      BARtap  = ' '
069900060920     c                   clear                   BARcan
070000060920     c                   clear                   BAR0af
070100031222      *
070200060920      * Valorizza i campi del file storico
070300060920     c                   clear                   FLTR7000
070400060920     c                   eval      TR7ctl  = TR4ctl
070500060920     c                   eval      TR7npl  = TR4npl
070600060920     c                   eval      TR7nfv  = TR4nfv
070700060920     c                   eval      TR7mil  = TR4ms1 + TR4ms2
070800060920     c                   eval      TR7cod  = TR4cod
070900060920     c                   eval      TR7020  = TR4020
071000060920     c                   eval      TR7pic  = TR4pic
071100060920     c                   eval      TR7pgr  = TR4pgr
071200060920     c                   eval      TR7pod  = TR4pod
071300060920     c                   eval      TR7ods  = TR4ods
071400060920     c                   eval      TR7pad  = TR4pad
071500060920     c                   eval      TR7dts  = TR4dts
071600060920     c                   eval      TR7ps1  = TR4ps1
071700060920     c                   eval      TR7pn1  = TR4pn1
071800060920     c                   eval      TR7pd1  = TR4pd1
071900060920     c                   eval      TR7ps2  = TR4ps2
072000060920     c                   eval      TR7pn2  = TR4pn2
072100060928     c                   move      TR4pd2        TR7pd2
072200060920     c                   eval      TR7ps3  = TR4ps3
072300060920     c                   eval      TR7pn3  = TR4pn3
072400060920     c                   eval      TR7pd3  = TR4pd3
072500060920     c                   eval      TR7ps4  = TR4ps4
072600060920     c                   eval      TR7pn4  = TR4pn4
072700060920     c                   eval      TR7pd4  = TR4pd4
072800060920     c                   eval      TR7pep  = TR4pep
072900060920     c                   eval      TR7pes  = TR4pes
073000060920     c                   eval      TR7pup  = TR4pup
073100060920     c                   eval      TR7pus  = TR4pus
073200060920     c                   eval      TR7crc  = TR4crc
073300060928     c                   eval      TR7vol  = *zeros
073400031223      * P.O. che spunta
073500060925     c                   move      wPO           TR7fil
073600031222      *
073700060920      * Controlla data e ora spunta, se non corretto utilizza *DATE
073800060920     c                   exsr      Ctr_TIME
073900160502      *
074000160502      * -?Gestione apparato TRANSPALLET: desunzione Tipo e Numero F.V.?
074100160502     c                   if        TR4020  = '2T'
074200160502     c                   exsr      sr_Transpallet
074300160502     c                   endif
074400031222      *
074500060920      * Imposta la data e ora spunta
074600060920     c                   movel     TR7tim        BARdfs
074700060920     c                   move      TR7tim        w006
074800060925     c                   movel     w006          BARhms
074900071203     c                   movel     TR7tim        wSPU_time
075000070212      * Controlla se dati numerici per millisecondi
075100070212     c                   eval      *in10   = *off
075200070212     c                   testn                   tr4ms2               10
075300070212if  1c                   if             *in10 = *off
075400070212     c                             or   %subst(tr4ms2:3:1) < *zero
075500070212     c                   movel     000           BARmis
075600070212     c                   else
075700070212     c                   movel     tr4ms2        BARmis
075800070212     c                   endif
075900070212      *
076000070212      * Profilo utente
076100070212     c                   eval      BARpru='VDL'+ parfil
076200031222      *
076300060920      * Controlla validità lettura scanner
076400060920     c                   move      TR4pd1        wBarCode
076500060920     c                   move      TR4pd1        wBarSped
076600060920     c                   exsr      Ctr_BarCod
076700060926     c                   eval      wPD1ok  = wPD0ok
076800060920     c                   eval      $CK1err = $CK0err
076900031222      *
077000060920      * Controlla validità lettura laser gun
077100060920     c                   move      TR4pd2        wBarCode
077200060920     c                   move      TR4pd2        wBarSped
077300060920     c                   exsr      Ctr_BarCod
077400060926     c                   eval      wPD2ok  = wPD0ok
077500060920     c                   eval      $CK2err = $CK0err
077600031222      *
077700031222      * Ai fini del caricamento spunte utilizzo la lettura dello
077800060920      * scanner tranne nel caso sia "blank" o contenga "???" od 8 zeri.
077900031222      * Ma in ogni caso memorizzo i dati del barcode solo se "validi"
078000041005      * a tutti gli effetti.
078100060920      * Se trovo errori nel barcode accendo *in99.
078200060920      * Se l'errore è dovuto al check digit accendo *in96.
078300060920     c                   movel     *zeros        wBarCode
078400060920     c                   eval      wScan1  = %scan('??' : TR4pd1)
078500060920     c                   eval      wScan2  = %scan('??' : TR4pd2)
078600031222      *
078700060920sel 1c                   SELECT
078800060920      * - Controlla validità lettura Scanner
078900060920w   1c                   WHEN           wScan1  = *zeros
079000060920     c                             and  TR4pd1 <> *blanks
079100060920     c                             and  %subst(TR4pd1:22:8) <> *zeros
079200060926if  2c                   if        wPD1ok  = 'BAR'
079300060920     c                   move      TR4pd1        wBarCode
079400060920x   2c                   else
079500060920     c                   eval      *in96   = ($CK1err = *on)
079600060920     c                   eval      *in99   = *on
079700060920e   2c                   endif
079800060920      * - Controlla validità lettura LaserGun
079900060920w   1c                   WHEN           wScan2  = *zeros
080000060920     c                             and  TR4pd2 <> *blanks
080100060920     c                             and  %subst(TR4pd2:22:8) <> *zeros
080200060926     c                   move      TR4pd2        wBarCode                       ?????
080300060926if  2c                   if        wPD2ok  = 'BAR'
080400060920     c                   move      TR4pd2        wBarCode
080500060920x   2c                   else
080600060920     c                   eval      *in96   = ($CK2err = *on)
080700060920     c                   eval      *in99   = *on
080800060920e   2c                   endif
080900060920      * - ? ? ?
081000060920x   1c                   OTHER
081100060920     c                   eval      *in99   = *on
081200060920e   1c                   ENDSL
081300031222      *
081400060920      * Memorizza barcode elaborato per spunta nello storico
081500060920     c                   move      wBAR_bar      TR7bar
081600031222      *
081700060920      * Imposta campi BARCODE
081800060920     c                   if        *in99   = *off
081900060920     c                   movel     wBAR_lnp      BARlnp
082000060920     c                   movel     wBAR_lna      BARlna
082100060920     c                   movel     wBAR_nrs      BARnrs
082200060920     c                   movel     wBAR_nsc      BARnsc
082300060920     c                   movel     wBAR_znc      BARznc
082400060920     c                   movel     wBAR_chk      BARctr
082500060920     c                   endif
082600031222      *
082700060920      * Controlla se è un pacco TEST
082800060920     c                   eval      $BarCiv = *off
082900060920if  1c                   if        TR7bar > *zeros
083000080619     c                   eval      wtr7bar = %subst(tr7bar:1:15)
083100080619     c     wTR7bar       lookup    $MSCdat                                11
083200060920     c                   eval      $BarCiv = *in11
083300060920e   1c                   endif
083400040421      *
083500060920      * Controlla il numero foglio
083600060920      * e memorizza in campo di comodo per saldi
083700060920     c                   eval      *in10   = *off
083800060920     c                   testn                   TR4nfv               10
083900060920if  1c                   if             *in10  = *on
084000060920     c                             and  %subst(TR4nfv: 5: 1) >= *zero
084100060920     c                             and  TR4nfv > *zeros
084200060920     c                   clear                   BARnfv
084300060920     c                   move      TR4nfv        BARnfv
084400060920     c                   eval      wBARnfv = BARnfv
084500060920x   1c                   else
084600060920     c                   eval      *in99   = *on
084700060920e   1c                   endif
084800050831      *
084900060920      * Controlla la categoria foglio
085000060920      * e memorizza in campo di comodo x saldi
085100060920if  1c                   if        TR4ctl  > *zeros
085200060920     c                   move      TR4ctl        BARnpg
085300060920x   1c                   else
085400060920     c                   clear                   BARnpg
085500060920e   1c                   endif
085600060920     c                   eval      wBARnpg = BARnpg
085700060920      *
085800060920      * Calcola il volume
085900060920     c                   clear                   wDM1
086000060920     c                   clear                   wDM2
086100060920     c                   clear                   wDM3
086200061009     c                   clear                   w096
086300060920     c                   clear                   BARvuc
086400060920     c                   eval      *in10   = *off
086500060925     c                   testn                   TR4dm                10
086600060920if  1c                   if             *in10  = *on
086700060920     c                             and  %subst(TR4dm :12: 1) >= *zero
086800060920     c                   move      TR4dm1        wDM1
086900060920     c                   move      TR4dm2        wDM2
087000060920     c                   move      TR4dm3        wDM3
087100060920e   1c                   endif
087200060920      *
087300060920      * sviluppa il volume
087400061009      * e testa se di solo 1 cifra intera...
087500060920if  1c                   if             wDM1   > *zeros
087600060920     c                             and  wDM2   > *zeros
087700060920     c                             and  wDM3   > *zeros
087800061009     c                   eval      w096    = wDM1 * wDM2 * wDM3
087900061009     c                   if        w096    < 10
088000061009     c                   z-add     w096          BARvuc
088100061009     c                   else
088200061009     c                   clear                   BARvuc
088300061009     c                   endif
088400061009     c                   move      BARvuc        TR7vol
088500060920e   1c                   endif
088600060920      *
088700060920      * controlla il peso
088800060925     c                   eval      wTR7pgr = %xlate(' ':'0':TR7pgr)
088900060920     c                   eval      *in10   = *off
089000060925     c                   testn                   wTR7pgr              10
089100060925if  1c                   if             *in10   = *on
089200060925     c                             and  wTR7pgr > *zeros
089300060925     c                             and  %subst(wTR7pgr : 6: 1) >= *zero
089400060925     c                   movel     wTR7pgr       w006
089500071011      * - RPVP peso in decagrammi, ALTRI impianti peso in grammi
089600071011w   1c                   IF             TR4020 = '22'
089700071011     c                             or   TR4020 = '2C'
089800071011     C                             or   TR4020 = '2K'
089900071011     c                   eval      BARpuc  = w006 / 100
090000071011x   1c                   else
090100071011     c                   eval      BARpuc  = w006 / 1000
090200071011e   1c                   endif
090300071011      *
090400060920x   1c                   else
090500060920     c                   clear                   BARpuc
090600060920e   1c                   endif
090700031222      *
090800031222      *  Per RPVP TIPO "K" collo netto tolgo la tara a peso e volume
090900060920if  1c                   IF        TR4020  = '2K'
091000060920if  2c                   if        BARpuc  > §TBApkg
091100060920     c                   eval      BARpuc  = BARpuc - §TBApkg
091200060920x   2c                   else
091300060920     c                   clear                   BARpuc
091400060920e   2c                   endif
091500060920if  2c                   if        BARvuc  > §TBAvol
091600060920     c                   eval      BARvuc  = BARvuc - §TBAvol
091700060920x   2c                   else
091800050721     c                   clear                   BARvuc
091900060920e   2c                   endif
092000060920e   1c                   ENDIF
092100041220      *
092200160325      *?Blocco caricamento peso e/o volume, gestito da tabella MVP?
092300160325     c                   exsr      sr_BloccoMVP
092400040421      *
092500060920      * Memorizza in campo di comodo x saldi  il peso e il volume
092600060920     c                   eval      wBARpuc = BARpuc
092700060920     c                   eval      wBARvuc = BARvuc
092800031222      *
092900060920      * Imposta Pistole
093000060920sel 1c                   SELECT
093100060920      * - VDL - pistola standard
093200060920w   1c                   WHEN      TR4020  = '20'
093300060920     c                   eval      BARnps  = C_npsSTD
093400070212     c                   eval      BARtap  = 'V'
093500170125      * - DISK C - barcode LaserGun BRT pistola standard
093600060920w   1c                   WHEN           TR4020  = '21'
093700060926     c                             and  wPD2ok  = 'BAR'
093800060920     c                   eval      BARnps  = C_npsSTD
093900070212     c                   eval      BARtap  = 'V'
094000170125      * - DISK C - barcode LaserGun non BRT pistola "presunta"
094100060920w   1c                   WHEN           TR4020  = '21'
094200060926     c                             and  wPD2ok <> 'BAR'
094300060920     c                   eval      BARnps  = C_npsPRES
094400070212     c                   eval      BARtap  = 'V'
094500060920      * - PICKING - pistola standard
094600060920w   1c                   WHEN           TR4020 = '23'
094700060920     c                             or   TR4020 = '24'
094800060920     c                             or   TR4020 = '25'
094900060920     c                             or   TR4020 = '26'
095000170128     c                             or   TR4020 = '36'
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
100700170128     c                                or  TR7020 = '36'
100800160519     c                                or (TR7020 = '23' and TR7pus = '1')
100900160705     c                                or  TR7020 = '24')
101000160606      *
101100060920     c                   clear                   BARnfv
101200060920     c                   clear                   BARnpg
101300060920     c                   clear                   BARnps
101400060920     c                   clear                   BARvuc
101500060920     c                   clear                   BARpuc
101600160518      *
101700160606sel 3c                   select
101800170128w   3c                   when      TR7020  = '26' or
101900170128w   3c                             TR7020  = '36'
102000060920     c                   exsr      BarCo_PK26
102100160606w   3c                   when      TR7020  = '24'
102200160518     c                   exsr      BarCo_PK24
102300160518x   3c                   other
102400060920     c                   exsr      BarCo_PK23
102500160518e   3c                   endSL
102600060920      * - Se FV impostato scrivo spunta imposto pistola "presunta"
102700060920if  3c                   if        BARnfv  > *zeros
102800060920     c                   eval      BARnps  = C_npsPRES
102900060920      *                  _______________
103000060920     c                   WRITE     BARCO
103100090212     c                   eval      parspu='S'
103200060920      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
103300060920e   3c                   endif
103400160606      *
103500060920e   2c                   ENDIF
103600040421      *
103700060920e   1c                   ENDSL
103800060920      *
103900060920     c                   ENDSR
104000160502
104100160502      /free
104200160502
104300160502       //--------------------------------------------------------------
104400160502       //?Gestione apparato TRANSPALLET: desunzione Tipo e Numero F.V. ?
104500160502       //--------------------------------------------------------------
104600160502       BEGSR  sr_Transpallet;
104700160502
104800160502         // -?Tipo F.V.?
104900160511         evalR wBarCode = TR4pd1;
105000160511
105100160502         if  wBar_LNP = PARfil  or
105200160511             %lookup( wBar_LNP : sk_L1 ) > *zero;
105300160502           TR4ctl = '5';
105400160502         else;
105500160502           TR4ctl = '2';
105600160502         endif;
105700160502         TR7ctl = TR4ctl;
105800160502
105900160502         // -?Numero F.V.?
106000160502         dsTIM    = TR7tim;
106100160502
106200160502         if  Not %open( FNFVV08L );
106300160502           open  FNFVV08L;
106400160502         endif;
106500160502
106600160502         clear  keyFNFVV08;
106700160502         k_FVVfgs = wPO;
106800160502         k_FVVnpg = %int( TR7ctl );
106900160502         k_FVVdfv = dsDAT;
107000160502         setll  %kds( keyFNFVV08 )  FNFVV008;
107100160502         reade  %kds( keyFNFVV08 )  FNFVV008;
107200160502
107300160502         doW  Not %eof( FNFVV08L )  and
107400160502              ( FVVatb <> *blank    or
107500160502                FVVfcf <> *blank );
107600160502           reade  %kds( keyFNFVV08 )  FNFVV008;
107700160502         enddo;
107800160502
107900160502         if  %eof( FNFVV08L );
108000160502           TR4nfv = *all'9';
108100160502           *in99  = *on;
108200160502         else;
108300160502           TR4nfv = %editc( FVVnfv : 'X' );
108400160502         endif;
108500160502         TR7nfv = TR4nfv;
108600160502
108700160502       ENDSR;
108800160502
108900160502      /end-free
109000160325
109100160325      *---------------------------------------------------------------*
109200160325      *?Blocco caricamento peso e/o volume, gestito da tabella MVP   ?*
109300160325      *---------------------------------------------------------------*
109400160325     c     sr_BloccoMVP  BEGSR
109500160325      *
109600160325sel 0c                   Select
109700160325      * -?RPVP?
109800160325w   0c                   When           TR4020     = '22'
109900160325     c                             or   TR4020     = '2C'
110000160325     c                             or   TR4020     = '2K'
110100160325      * -?Volume?
110200160325if  1c                   if             §MVPvblR   = 'S'
110300160325     c                             and  BARvuc     > *zeros
110400160325     c                             and  wSPU_time >= MVP_V_timI_R
110500160325     c                             and  wSPU_time <= MVP_V_timF_R
110600160325     c                   clear                   BARvuc
110700160325e   1c                   endif
110800160325      * -?Peso?
110900160325if  1c                   if             §MVPpblR   = 'S'
111000160325     c                             and  BARpuc     > *zeros
111100160325     c                             and  wSPU_time >= MVP_P_timI_R
111200160325     c                             and  wSPU_time <= MVP_P_timF_R
111300160325     c                   clear                   BARpuc
111400160325e   1c                   endif
111500160325      * -?TRANSPALLET?
111600160325w   0c                   When           TR4020     = '2T'
111700160325      * -?Peso?
111800160325if  1c                   if             §MVPpblT   = 'S'
111900160325     c                             and  BARpuc     > *zeros
112000160325     c                             and  wSPU_time >= MVP_P_timI_T
112100160325     c                             and  wSPU_time <= MVP_P_timF_T
112200160325     c                   clear                   BARpuc
112300160325e   1c                   endif
112400160325      *
112500160325      * -?VDL?
112600160325x   0c                   Other
112700160325      * -?Volume?
112800160325if  1c                   if             §MVPvbl    = 'S'
112900160325     c                             and  BARvuc     > *zeros
113000160325     c                             and  wSPU_time >= MVP_V_timI
113100160325     c                             and  wSPU_time <= MVP_V_timF
113200160325     c                   clear                   BARvuc
113300160325e   1c                   endif
113400160325      * -?Peso?
113500160325if  1c                   if             §MVPpbl    = 'S'
113600160325     c                             and  BARpuc     > *zeros
113700160325     c                             and  wSPU_time >= MVP_P_timI
113800160325     c                             and  wSPU_time <= MVP_P_timF
113900160325     c                   clear                   BARpuc
114000160325e   1c                   endif
114100160325      *
114200160325e   0c                   EndSl
114300160325      *
114400160325     c                   ENDSR
114500060920
114600060920      *---------------------------------------------------------------*
114700080128      *?Controllo data (non ora) di spunta                           ?*
114800060920      *---------------------------------------------------------------*
114900060920     c     Ctr_Time      BEGSR
115000031222      *
115100070212      * Controllo campi data e ora spunta da portare su FIBAR
115200060920     c                   eval      *in10   = *off
115300060920     c                   eval      *in12   = *off
115400060920     c                   reset                   dsTIM
115500060920     c                   testn                   TR4tim               10
115600080109if  1c                   IF             *in10  = *on
115700060920     c                             and  %subst(TR4tim:14:1) >= *zero
115800060920     c                             and  TR4tim > *zeros
115900060920     c                   movel     TR4tim        dsTIM
116000080109      * - Controllo data
116100111214      *   (NON può essere precedente di oltre 2 giorni quella attuale)
116200060920     c                   reset                   WLBda8
116300111214if 1Bc                   If        dsDAT   <=
116400111214     c                             %int( %subst( %char( %dec(
116500111214     c                                           %timestamp() ) )
116600111214     c                                           : 1 : 8 ) )     AND
116700111214     c                             dsDAT   >=
116800111214     c                             %int( %subst( %char( %dec(
116900111214     c                                           %timestamp() - %days(2) ) )
117000111214     c                                           : 1 : 8 ) )
117100060920     c                   eval      G08inv  = dsDAT
117200060920     c                   call      'XSRDA8'
117300060920     c                   parm                    WLBda8
117400060920if  2c                   if        G08err  = *off
117500060920     c                   move      TR4tim        TR7tim
117600060920     c                   eval      *in12   = *on
117700060920e   2c                   endif
117800111214e  1Bc                   EndIf
117900080109e   1c                   ENDIF
118000060920      *
118100080128if  1c                   if        NOT *in12
118200080109     c                   time                    w140
118300080109e   1c                   endif
118400080109      *
118500060920      * Data non corretta
118600060928if  1c                   if        NOT *in12
118700060920     c                   move      dsTIM         TR7tim
118800031222      * imposto i primi otto caratteri del campo con data odierna
118900080108     c                   clear                   WLBda8
119000080108     c                   move      w140          G08dat
119100080108     c                   call      'XSRDA8'
119200080108     c                   parm                    WLBda8
119300080108     c                   movel     G08inv        TR7tim
119400060920e   1c                   endif
119500031222      *
119600031222     c                   ENDSR
119700060920
119800060920      *---------------------------------------------------------------*
119900060920      *?Controllo dei campi del BarCode                              ?*
120000060920      *---------------------------------------------------------------*
120100060920     c     Ctr_BarCod    BEGSR
120200031222      *
120300031222      * Flag validità BARCODE, può valere:
120400170125      *  "BAR" --> Barcode BRT
120500031222      *  "SPE" --> Barcode spedizione
120600031222      *  "ERR" --> Barcode errato
120700060926     c                   clear                   wPD0ok
120800041005      *
120900041005      * Flag validità CHECK DIGIT viene calcolato solo se il barcode
121000041005      * ha superato tutte le specifiche per "BAR"
121100060925     c                   eval      $CK0err = *off
121200031222      *
121300060920      * Se barcode più lungo di 18 non può essere BAR e neppure SPE:
121400060920      * testo 19esima posizione per letture LaserGun di 19 char
121500060920if  1c                   if             wBAR_xx1 <> *blanks
121600060920     c                             or  (wBAR_xx2 <> '#'
121700060920     c                              and wBAR_xx2 <> *blanks)
121800060926     c                   eval      wPD0ok  = 'ERR'
121900060920     c                   LEAVESR
122000060920e   1c                   endif
122100041014      *
122200060920      * Controlla se dati numerici
122300060920     c                   eval      *in10   = *off
122400060920     c                   testn                   wBAR_BarCo           10
122500060920if  1c                   if             *in10   = *off
122600060920     c                             or   %subst(wBAR_BarCo:18:1) < *zero
122700060926     c                   eval      wPD0ok  = 'ERR'
122800060920     c                   goto      EndBarCod
122900060920e   1c                   endif
123000031222      *
123100060920      * Controlla se campi valorizzati
123200060920sel 1c                   select
123300060920w   1c                   when      wBAR_lnp = *zeros
123400060926     c                   eval      wPD0ok   = 'ERR'
123500060920w   1c                   when      wBAR_lna = *zeros
123600060926     c                   eval      wPD0ok   = 'ERR'
123700060920w   1c                   when      wBAR_nsc = *zeros
123800060926     c                   eval      wPD0ok   = 'ERR'
123900060920e   1c                   endsl
124000060926     c     wPD0ok        cabeq     'ERR'         EndBarCod
124100031222      *
124200060920      * Controlla validità linea di partenza
124300060920     c                   clear                   ORGfag
124400060920     c                   movel     wBAR_lnp      ORGfil
124500060920     c     ORGfil        chain     AZORG
124600060920if  1c                   if        NOT  %found(AZORG01L)
124700060920     c                             or  (ORGfag <> 'A'
124800060920     c                              and ORGfag <> 'F')
124900060926     c                   eval      wPD0ok  = 'ERR'
125000060920     c                   goto      EndBarCod
125100060920e   1c                   endif
125200031222      *
125300060920      * Controlla validità linea di arrivo
125400060920     c                   clear                   ORGfag
125500060920     c                   movel     wBAR_lna      ORGfil
125600060920     c     ORGfil        chain     AZORG
125700060920if  1c                   if        NOT  %found(AZORG01L)
125800060920     c                             or  (ORGfag <> 'A'
125900060920     c                              and ORGfag <> 'F')
126000060926     c                   eval      wPD0ok  = 'ERR'
126100060920     c                   goto      EndBarCod
126200060920e   1c                   endif
126300041005      *
126400060920      * Controlla validità check digit
126500060925     c                   clear                   TRUL28ds
126600060920     c                   eval      I28tla  = 'C'
126700060920     c                   eval      I28mod  = 'BAR'
126800060920     c                   eval      I28cod  = wBAR_bar
126900060920     c                   call      'TRUL28R1'
127000060920     c                   parm                    TRUL28ds
127100060920if  1c                   if             O28err <> *blanks
127200060920     c                             or   O28ckd <> wBAR_chk
127300060926     c                   eval      wPD0ok  = 'ERR'
127400060920     c                   eval      $CK0err = *on
127500060920     c                   goto      EndBarCod
127600060920e   1c                   endif
127700060920      *
127800060920     c     EndBarCod     tag
127900041005      *
128000060920      * SE ok imposto flag, ALTRIMENTI verifico se spedizione
128100060926if  1c                   if        wPD0ok  = *blanks
128200060926     c                   eval      wPD0ok  = 'BAR'
128300060920x   1c                   else
128400060920     c                   exsr      Ctr_BarSpe
128500060920e   1c                   endif
128600041014      *
128700031222     c                   ENDSR
128800060920
128900060920      *---------------------------------------------------------------*
129000060920      *?Controllo dei campi del BarCode Spedizione                   ?*
129100060920      *---------------------------------------------------------------*
129200060920     c     Ctr_BarSpe    BEGSR
129300031222      *
129400060920      * Flag validità BARCODE, può valere:
129500170125      *  "BAR" --> Barcode BRT
129600060920      *  "SPE" --> Barcode spedizione
129700060920      *  "ERR" --> Barcode errato
129800060926     c                   clear                   wPD0ok
129900041014      *
130000060920      * Se barcode più lungo di 15 non può essere BAR e neppure SPE:
130100060920      * testo 19esima posizione per letture LaserGun di 19 char
130200060920if  1c                   if        wSPE_xx1  <> *blanks
130300060926     c                   eval      wPD0ok  = 'ERR'
130400060920     c                   LEAVESR
130500060920e   1c                   endif
130600031222      *
130700060920      * Controlla se dati numerici
130800060920     c                   eval      *in10   = *off
130900060920     c                   testn                   wSPE_BarCo           10
131000060920if  1c                   if             *in10 = *off
131100060920     c                             or   %subst(wSPE_BarCo:15:1) < *zero
131200060926     c                   eval      wPD0ok  = 'ERR'
131300060920     c                   leavesr
131400060920e   1c                   endif
131500031222      *
131600031222      * controlla se campi valorizzati
131700060920sel 1c                   select
131800060920w   1c                   when      WSPE_LNP = *zeros
131900060926     c                   eval      wPD0ok   = 'ERR'
132000060920     c                   leavesr
132100060920w   1c                   when      WSPE_NSP = *zeros
132200060926     c                   eval      wPD0ok   = 'ERR'
132300060920     c                   leavesr
132400060920e   1c                   endsl
132500031222      *
132600031222      * controllo validità linea di partenza
132700060920     c                   clear                   ORGfag
132800060920     c                   movel     wSPE_lnp      ORGfil
132900060920     c     ORGfil        chain     AZORG
133000060920if  1c                   if        NOT  %found(AZORG01L)
133100060920     c                             or  (ORGfag <> 'A'
133200060920     c                              and ORGfag <> 'F')
133300060926     c                   eval      wPD0ok  = 'ERR'
133400060920     c                   leavesr
133500060920e   1c                   endif
133600031222      *
133700031222      *  Se ok imposto flag barcode spedizione
133800060926     c                   if        wPD0ok  = *blanks
133900060926     c                   eval      wPD0ok  = 'SPE'
134000060920     c                   endif
134100041014      *
134200031222     c                   ENDSR
134300060921
134400060921      *---------------------------------------------------------------*
134500060921      *?Verifica se scrivere spunta uscita x PICKING VDL - PC VDL    ?*
134600060921      *---------------------------------------------------------------*
134700060921     c     BarCo_PK23    BEGSR
134800060921      *
134900060921      * Apro file colli
135000090129     c                   if        NOT %open(FITR602L)
135100090129     c                   open      FITR602L
135200060921     c                   endif
135300040421      *
135400040422      * Per la ricerca della baia utilizzo gli ultimi 2 byte, il campo
135500040422      * VDL è lungo 3 e sui nostri file 2.
135600040422      * Se il 1° dei 2 byte è blank imposto 0, questo perchè nei file
135700060921      * colli e piano trovo, ad esempio, "01" mentre VDL passa " 1"
135800060921     c                   eval      wTR7pad2 = %subst(TR7pad:2:2)
135900060920     c                   if        %subst(wTR7pad2:1:1) = ' '
136000040422     c                   movel     '0'           wTR7pad2
136100040422     c                   endif
136200040421      *
136300060921      *  Aggancio file colli
136400090129     c     K02tr6        chain     FITR6
136500040422      *
136600040422      *  Se trovato e la baia del file colli coincide con quella VDL
136700060921      *  creo la spunta con il FV del file colli
136800060921      *  - Tranne se FV IMA per cui non creiamo spunta ma memorizzo
136900060921      *    la baia per velocizzare i controlli sui record successivi
137000060921      *  - Tranne se Distinta messaggerie
137100090129if  2c                   If             %found(FITR602L)
137200060921     c                             and  TR6bau = wTR7pad2
137300050831      *
137400060921sel 3c                   select
137500060921w   3c                   when      TR6fl3   = 'M'
137600060921w   3c                   when      TR6npg  <>  3
137700060921     c                   z-add     TR6nfv        BARnfv
137800060921     c                   z-add     TR6npg        BARnpg
137900060921e   3c                   endsl
138000040429      *
138100060921x   2c                   Else
138200040422      *
138300040607      *  In caso contrario cerco la baia VDL nel piano di smistamento:
138400040607      *   se trovata creo la spunta "uscita", impostando il FV trovato nel
138500060921      *   file piani, a meno che sia "non letti" o IMA o defluenza.
138600161129      *   Eseguo setll/read, perchè cerco un F.V. tra quelli presenti in baia,
138700161111      *   scartando la baia "non letti" ed eventuali FV defluenza ed IMA
138800161129      *   (ora quelli IMA sono ANCHE?misti?a quelli consegna - stessa baia).
138900161111      *   NON memorizzo (come già NON faceva prima) le baie "non letti",
139000161111      *   defluenza ed IMA...
139100040422      *
139200040422      *  Apro file piano di smistamento
139300171107if  3c                   if        NOT %open(FLTR902L)
139400171107     c                   open      FLTR902L
139500060921e   3c                   endif
139600040421      *
139700060921     c                   movel(p)  wTR7pad2      TR9pep
139800060921     c                   z-add     wBARnfv       TR9nfv
139900171107     c                   eval      TR9tps = 'V'
140000171107     c     K05TR902      setll     FLTR9000
140100171107     c     K05TR902      reade     FLTR9000
140200171107do  3c                   DoW       NOT  %eof(FLTR902L)
140300161111     c                             and (TR9bnl = 'S'
140400161111     c                              or  TR9npg <> 4)
140500171107     c     K05TR902      reade     FLTR9000
140600161111e   3c                   EndDo
140700171107if  3c                   if        NOT  %eof(FLTR902L)
140800161111     c                   z-add     TR9nfv        BARnfv
140900161111     c                   z-add     TR9npg        BARnpg
141000161111e   3c                   endif
141100040422      *
141200060921e   2c                   EndIf
141300040422      *
141400040421     c                   ENDSR
141500060921
141600060921      *---------------------------------------------------------------*
141700060921      *?Verifica se scrivere spunta uscita x PICKING VDL - PC NO VDL ?*
141800060921      *---------------------------------------------------------------*
141900060921     c     BarCo_PK26    BEGSR
142000060921      *
142100040504      * Apro file colli
142200090129if  1c                   if        NOT %open(FITR602L)
142300090129     c                   open      FITR602L
142400060921e   1c                   endif
142500040504      *
142600040504      * Aggancio file colli
142700090129     c     K02tr6        chain     FITR6
142800040504      *
142900060921      * Se non trovato non creo spunta
143000060921      * Se distinta messaggerie non creo spunta
143100060921      * Se trovato e la postazione non è IMA creo la spunta con il FV
143200060921      *    del file colli
143300090129if  1c                   if             %found(FITR602L)
143400060921     c                             and  TR6npg <>  3
143500060921     c                             and  TR6fl3 <> 'M'
143600040504     c                   z-add     TR6nfv        BARnfv
143700040504     c                   z-add     TR6npg        BARnpg
143800060921e   1c                   endif
143900040504      *
144000040504     c                   ENDSR
144100060921
144200160518      *---------------------------------------------------------------*
144300160518      *?Verifica se scrivere spunta 4 x PICKING NO VDL e filiale abilitata *
144400160518      *---------------------------------------------------------------*
144500160518     c     BarCo_PK24    BEGSR
144600160518      *
144700160518      * Apro file colli
144800160518if  1c                   if        NOT %open(FITR602L)
144900160518     c                   open      FITR602L
145000160518e   1c                   endif
145100160518      *
145200160518      * Aggancio file colli
145300160518     c     K02tr6        chain     FITR6
145400160518      *
145500160518      * Se non trovato non creo spunta
145600160518      * Se distinta messaggerie non creo spunta
145700160518      * Se trovato e la postazione non è IMA creo la spunta con il FV
145800160518      *    del file colli
145900160518if  1c                   if             %found(FITR602L)
146000160518     c                             and  TR6npg <>  3
146100160518     c                             and  TR6fl3 <> 'M'
146200160518     c                   z-add     TR6nfv        BARnfv
146300160518     c                   z-add     TR6npg        BARnpg
146400160518e   1c                   endif
146500160518      *
146600160518     c                   ENDSR
146700160518
146800060921      *---------------------------------------------------------------*
146900060921      *?Scrittura SALDI                                              ?*
147000060921      *---------------------------------------------------------------*
147100060920     c     Wrt_SAL       BEGSR
147200031222      *
147300031222      * Reperisco data e l'ora di lettura da TR7TIM che è già stato
147400031222      *  controllato e "normalizzato"
147500080128      * -> In realtà: solo la data è stata controllata e reimpostata...
147600060921     c                   movel     TR7tim        dsTIM
147700060921     c                   movel     dsDAT         KSAdtl
147800080128     c***                z-add     dsORA         KSAorl
147900080128      * - Controllo ora
148000160502if  1c                   IF            %subst( %trim( %editw(
148100080128     c                                 dsORA:'0    ' ) ) :1:2 ) <  '00'
148200080128     c                             or  %subst( %trim( %editw(
148300080128     c                                 dsORA:'0    ' ) ) :1:2 ) >  '23'
148400080128     c                             or  %subst( %trim( %editw(
148500080128     c                                 dsORA:'0    ' ) ) :3:2 ) <  '00'
148600080128     c                             or  %subst( %trim( %editw(
148700080128     c                                 dsORA:'0    ' ) ) :3:2 ) >  '59'
148800080128      * - - Ora  non corretta
148900080128      *     imposto il campo con ora/minuti correnti
149000080128     c                   time                    w140
149100080128     c                   movel     w140          KSAorl
149200080128     c                   ELSE
149300080128     c                   z-add     dsORA         KSAorl
149400080128e   1c                   ENDIF
149500080619      * - memorizzo ora/minuti/secondi da tr7tim per FIMSC00F
149600080619     c                   eval      ksciorl = %dec(%subst(tr7tim:9:6):6:0)
149700160502      *
149800160502      * -?Tipo Applicazione?
149900160502     c                   clear                   xx
150000160502     c                   eval      *in97   = *off
150100160502if  1c                   If        TR4020  = '2T'
150200160502      *
150300160502      * -?Tipo Applicazione per TRANSPALLET pesatore = "T" (chiodato)?
150400160502     c                   eval      KSAapl  = 'T'
150500160502      *
150600160502x   1c                   Else
150700021008      *
150800060921      * Cerco il tipo applicazione dell'entrata corrente:
150900060921      *   se non trovo l'entrata, prendo l'applicazione principale per
151000060921      *   il P.O. di tabella MFP
151100060921     c                   eval      xx      = 1
151200060921     c     TR4pep        lookup    $MSL(xx)                               97
151300161129     c                   select
151400161129     c                   when      *in97  and  TR4ctl <> '5'
151500161129     c                   eval      KSAapl  = $MSLa(xx)
151600161129     c                   when      *in97  and  TR4ctl =  '5'
151700161129     c                   eval      KSAapl  = $MSLa5(xx)
151800161129     c                   when      Not *in97
151900161129     c                   eval      KSAapl  = §MFPapl
152000161129     c                   endsl
152100160502      *
152200160502e   1c                   EndIf
152300161213      *
152400161213      *//* -?Tipo Lavorazione:?
152500161213     c*//                   clear                   KSAlav
152600161213sel 1c*//                   select
152700161213      *//* - -?Entrata?
152800161213w   1c*//                   when      TR4ctl = '5'
152900161213     c*//                   eval      KSAlav = TR4ctl
153000161213      *//* - -?Arrivi?
153100161213w   1c*//                   when      TR4ctl = '2'  and  TR4npl <> *zeros
153200161213     c*//                   eval      KSAlav = TR4ctl
153300161213      *//* - -?Picking?
153400161213w   1c*//                   when      TR4ctl = '2'  and  TR4npl =  *zeros
153500161213     c*//*//?v. IF__1 successiva:?                  and  $MSLe(xx) = 'S'
153600161213     c*//                   eval      KSAlav = 'K'
153700161213      *//* - -?Inventario?
153800161213w   1c*//                   when      TR4ctl = '3'
153900161213     c*//                   eval      KSAlav = TR4ctl
154000161213     c*//                   endsl
154100161213      *//*
154200161213      *//* -?Tipo Impianto (fisico):?
154300161213     c*//                   clear                   KSAimp
154400161213sel 1c*//                   select
154500161213      *//* - -?Transpallet?
154600161213w   1c*//                   when      TR4020  =  '2T'
154700161213     c*//                   eval      KSAimp  =  'T'
154800161213      *//* - -?Postazione Entrata NON trovata in tab. "MSL"?
154900161213w   1c*//                   when      *in97   =  *off
155000161213     c*//                   eval      KSAimp  =  §MFPapl
155100161213      *//* - -?Wi-Fi?
155200161213w   1c*//                   when      $MSLw(xx)  = 'W'
155300161213     c*//                   eval      KSAimp  =  'W'
155400161213      *//* - -?Van Der Lande?
155500161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'V')  or
155600161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'V')
155700161213     c*//                   eval      KSAimp  =  'V'
155800161213      *//* - -?Fissi?
155900161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'I')  or
156000161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'I')
156100161213     c*//                   eval      KSAimp  =  'I'
156200161213      *//* - -?Rulliera?
156300161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'R')  or
156400161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'R')
156500161213     c*//                   eval      KSAimp  =  'R'
156600161213      *//* - -?RPVP?
156700161213w   1c*//                   when      (TR4ctl =  '5'  and  $MSLa5(xx) = 'P')  or
156800161213     c*//                             (TR4ctl <> '5'  and  $MSLa(xx)  = 'P')
156900161213     c*//                   eval      KSAimp  =  'P'
157000161213      *//* - -?...altrimenti?
157100161213w   1c*//                   other
157200161213     c*//                   eval      KSAimp  =  §MFPapl
157300161213e   1c*//                   endsl
157400031224      *
157500031224      * Elaboro saldi
157600031224      *  Per applicazione PICKING solo la postazione è banco entrata
157700060921if  1c                   IF             KSAapl   <> 'K'
157800060921     c                             or  (KSAapl    = 'K'
157900060921     c                              and *in97     = *on
158000060921     c                              and $MSLe(xx) = 'S')
158100031224      *
158200060921      * Controllo se lettura effettuata da SCANNER:
158300060921      * scarto se scanner ha letto "???" e laser Gun non ha letto nulla
158400060926     c                   eval      $okPD1  = *on
158500060926     c                   eval      wScan1  = %scan('????' : TR4pd1)
158600060926if  2c                   if             wScan1  > *zeros
158700060921     c                             and  TR4pd2  = *blanks
158800060926     c                   eval      $okPD1  = *off
158900060921e   2c                   endif
159000060921      *
159100060921      * Controllo se lettura effettuata da LASER GUN
159200060921      * scarto barcode non corretti tranne se errore "????"
159300060926     c                   eval      $okPD2  = *on
159400060926     c                   eval      wScan2 = %scan('????' : TR4pd2)
159500060926if  2c                   if             wScan2  = *zeros
159600060926     c                             and  wPD2ok <> 'BAR'
159700060926     c                   eval      $okPD2  = *off
159800060921e   2c                   endif
159900060921      *
160000170302      * Scrivo/Aggiorno saldi letture (FNMSA00F)
160100170302     c     K05MSA03      chain     FNMSA003
160200170302if  2c                   If        not %found(FNMSA03L)
160300170302     c                   clear                   FNMSA003
160400080407     c                   eval      MSAapl  = KSAapl
160500080407     c                   eval      MSAfil  = wPO
160600080407     c                   eval      MSAdtl  = KSAdtl
160700080407     c                   eval      MSAorl  = KSAorl
160800080407     c                   eval      MSAnpg  = wBARnpg
160900060922      * - Reperisco la data del F.V.
161000060922     c                   exsr      Rep_FVV
161100080407     c                   eval      MSAdfv  = FVVdfv
161200170125      * -?Imposto lavorazione picking (se NON picking DPD)?
161300170125     c                   If        tr4NPL= '00'  and  tr4020 <> '36'
161400151228     c                   z-add     999           MSAtcl
161500151228     c                   Else
161600151228     c                   clear                   MSAtcl
161700151228     c                   EndIf
161800151228      *
161900170302     c                   exsr      sr_RieMSA
162000060922      *                  __________________
162100170302     c                   write(e)  FNMSA003
162200060922      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
162300060922x   2c                   Else
162400170302     c                   exsr      sr_RieMSA
162500060922      *                  __________________
162600170302     c                   update(e) FNMSA003
162700060922      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
162800060922e   2c                   EndIf
162900060921      *
163000060921      * Scrive saldi letture civette
163100060921if  2c                   If        $BarCiv = *on
163200060921     c                   eval      KSCdtl  = KSAdtl
163300060921     c                   movel     TR7bar        KSCseg
163400080619      * - Scrivo il nuovo file per le letture civette
163500080619     c     K04IMSC01     chain     FIMSC000
163600080619if  3c                   if        NOT %found(FIMSC01L)
163700080619     C                   clear                   FIMSC000
163800080708     c                   eval      MSCfil  = wPO
163900080708     c                   eval      MSCdtl  = KSCdtl
164000080708     c                   eval      MSCorl  = KSCIorl
164100080708     c                   eval      MSCseg  = KSCseg
164200080619     c                   exsr      sr_RieIMSC
164300080619      *                  __________________
164400080619     c                   write     FIMSC000
164500080619      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
164600080619x   3c                   else
164700080619     c                   exsr      sr_RieIMSC
164800080619      *                  __________________
164900080619     c                   update    FIMSC000
165000080619      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
165100080619e   3c                   endif
165200060921e   2c                   EndIf
165300031224      *
165400060921e   1c                   ENDIF
165500031224      *
165600000000     c                   ENDSR
165700160215
165800160215      *---------------------------------------------------------------*
165900160215      *?Scrittura Segnacollo DPD suntato SENZA record bolla          ?*
166000160215      *---------------------------------------------------------------*
166100160215     c     Wrt_BARDPD    BEGSR
166200160215      *
166300160215     c                   clear                   BARDPD000
166400160215      *
166500160216     c                   eval      BARDPDpd1 = %subst(TR7pd2: 2: 28)
166600160216     c                   eval      BARDPDprn = %subst(TR7pd2: 10: 14)
166700160215     c                   eval      BARDPDfgs = wPO
166800160216     c                   eval      BARDPDdfs = %int( %subst( TR7tim :
166900160215     c                                                       1 : 8 ) )
167000160216     c                   eval      BARDPDhms = %int( %subst( TR7tim :
167100160215     c                                                       9 : 6 ) )
167200160215      *                  ___________________
167300160215     c                   write     BARDPD000
167400160215      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
167500160215     c                   ENDSR
167600060921
167700060921      *---------------------------------------------------------------*
167800060922      *?Reperimento data F.V. da FNFVV00F                            ?*
167900060921      *---------------------------------------------------------------*
168000060922     c     Rep_FVV       BEGSR
168100060921      *
168200060921      * Recupera la data foglio
168300060921     c                   z-add     wBARnfv       FVVnfv
168400060921     c     K03FVV01      chain     FNFVV000
168500060921if  1c                   if        NOT %found(FNFVV01L)
168600060921     c                   clear                   FVVdfv
168700060921e   1c                   endif
168800160510if  1c                   if        FVVdfv = *zero
168900160510     c                   eval      dsTIM  = %subst( %char( %dec(
169000160510     c                                         %timestamp() ) )
169100160510     c                                         : 1 : 14 )
169200160510     c                   eval      FVVdfv = dsDAT
169300160510e   1c                   endif
169400060921      *
169500000000     c                   ENDSR
169600080619
169700080619      *---------------------------------------------------------------*
169800080619      *?Impostazione campi record di FIMSC                           ?*
169900080619      *---------------------------------------------------------------*
170000080619     c     sr_RieIMSC    BEGSR
170100080619      *
170200080619      * Imposta campi record
170300080708     c                   eval      MSCpes  = wPES                               *peso
170400080708     c                   eval      MSCvol  = wVOL                               *volume
170500080619      * - Baia d'entrata
170600080708     c   97              move      $MSLn(xx)     MSCpep                         *baia entrata
170700080708     c  N97              clear                   MSCpep
170800080619      *
170900080619     c                   ENDSR
171000060921
171100060921      *---------------------------------------------------------------*
171200060921      *?Pulizia delle variabili di lavoro                            ?*
171300060921      *---------------------------------------------------------------*
171400060921     c     sr_ClrFld     BEGSR
171500060921      *
171600060921     c                   clear                   wPES
171700060921     c                   clear                   wVOL
171800060921     c                   clear                   wODSdiv
171900060921     c                   clear                   wODSdnr
172000060921     c                   clear                   wODSdna
172100060921     c                   clear                   wODSftd
172200060921     c                   clear                   wODSids
172300060921     c                   clear                   wODSuds
172400060921     c                   clear                   wODSdne
172500060921     c                   clear                   wODSciv
172600060921     c                   clear                   wODSdnq
172700060921     c                   clear                   wODSust
172800060921     c                   clear                   wDTSnts
172900060921     c                   clear                   wDTStok
173000060921     c                   clear                   wDTSila
173100060921     c                   clear                   wDTSnrl
173200060921     c                   clear                   wDTSndf
173300060921     c                   clear                   wPS1dnr
173400060921     c                   clear                   wPS1dts
173500060921     c                   clear                   wPS1dtl
173600060921     c                   clear                   wPS1nsc
173700060921     c                   clear                   wPS1nre
173800060921     c                   clear                   wPS1dok
173900060921     c                   clear                   wPS1dno
174000060921     c                   clear                   wPS2dnr
174100060921     c                   clear                   wPS2dts
174200060921     c                   clear                   wPS2dtl
174300060921     c                   clear                   wPS2nsc
174400060921     c                   clear                   wPS2nre
174500060921     c                   clear                   wPS2dok
174600060921     c                   clear                   wPS2dno
174700060921     c                   clear                   wPESnes
174800060921     c                   clear                   wPESind
174900060921     c                   clear                   wPESune
175000060921     c                   clear                   wPUSnes
175100060921     c                   clear                   wPUSsor
175200060921     c                   clear                   wPUSlos
175300060922     c                   clear                   $PEP
175400060921     c                   clear                   $PAD
175500060921     c                   clear                   wCSD
175600060921     c                   clear                   wCCP
175700060921     c                   clear                   wCCV
175800060921     c                   clear                   wNLS
175900060921     c                   clear                   wNLP
176000161213     c                   clear                   wPWF
176100060921      *
176200060921     c                   ENDSR
176300060922
176400060922      *---------------------------------------------------------------*
176500170302      *?Impostazione campi record di FNMSA00F                        ?*
176600060922      *---------------------------------------------------------------*
176700170302     c     sr_RieMSA     BEGSR
176800060922      *
176900060922      * Azzera le variabili di lavoro
177000060922     c                   exsr      sr_ClrFld
177100060922      *
177200170302      * Imposta campi di lavoro per il file FNMSA00F
177300170302     c                   exsr      sr_FldMSA
177400060922      *
177500170302      * Aggiorna campi nel record di FNMSA00F
177600170302     c                   exsr      sr_RecMSA
177700060922      *
177800060922     c                   ENDSR
177900060922
178000060922      *---------------------------------------------------------------*
178100060922      *?Impostazione delle variabili di lavoro per FNMSA00F          ?*
178200060922      *---------------------------------------------------------------*
178300170302     c     sr_FldMSA     BEGSR
178400060922      *
178500061006      *?Peso  - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
178600060922     c                   eval      wPES    = wBARpuc
178700061006      *
178800061006      *?Volume  - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
178900060922     c                   eval      wVOL    = wBARvuc
179000061006      *
179100061006      *?Parcel entrance point - - - - - - - - - - - - - - - - - - - -?*
179200061006      *
179300061006      * colli caricati su ciascun banco
179400061006      * rilevati dalla fotocellula (NON inaspettatamente)
179500061006      * & usciti correttamente
179600061006     c                   eval      xx      = 1
179700061006     c     TR4pep        lookup    $MSL(xx)                               97
179800061011sel 1c                   select
179900061011      * - valore nullo (TR4pep = *blanks)
180000061011w   1c                   when           *in97      = *on
180100061011     c                             and  $MSLn(xx) <= *zeros
180200061019      * - VDL: collo NON caricato
180300061019w   1c                   when           KSAapl  = 'V'
180400061019     c                             and (TR4pus  = '2'
180500061019     c                              or (TR4pes <> '1'
180600061019     c                              and TR4pes <> '2'))
180700061019      * - VDL: collo caricato in baia '20'
180800061019w   1c                   when           KSAapl  = 'V'
180900061019     c                             and  TR4pus <> '2'
181000061019     c                             and  TR4pes  = '2'
181100061019     c                   eval      $PEP(20)   = 1                               NON TROVATO
181200061011      * - valore valido
181300061011w   1c                   when           *in97      = *on
181400061011     c                             and  $MSLn(xx)  > *zeros
181500061011     c                             and  $MSLn(xx)  < %elem($PEP)
181600061006     c                   eval      $PEP( $MSLn(xx) ) = 1                        TROVATO
181700061011x   1c                   other
181800061006      * - nessun valore conosciuto --> entrance point n° 20
181900061006     c                   eval      $PEP(20)   = 1                               NON TROVATO
182000061011e   1c                   endsl
182100061006      *
182200061006      *?Parcel actual destination - - - - - - - - - - - - - - - - - -?*
182300061006      *
182400151130      * -?Se  VDL & collo smistato correttamente  o  Non VDL?
182500151130if  Ac                   If             KSAapl  = 'V'
182600151130     c                             and  TR4pus <> '2'
182700151130     c                             OR   KSAapl <> 'V'
182800061006     c                   eval      wTR4pad = %xlate(' ':'0':TR4pad)             *trasforma *b in *z
182900061006     c                   eval      *in10   = *off
183000061006     c                   testn                   wTR4pad              10
183100061006if  1c                   if        *in10   = *on                                *campo numerico
183200061006     c                   movel     wTR4pad       w003
183300061006x   1c                   else
183400061006     c                   clear                   w003
183500061006e   1c                   endif
183600061006if  1c                   if             w003    > *zeros
183700061006     c                             and  w003   <= %elem($PAD)
183800061006     c                   eval      $PAD(w003) = 1
183900061006x   1c                   else
184000061006      * - nessun valore conosciuto --> actual destination n° 60
184100061006     c                   eval      $PAD(60)   = 1
184200061006e   1c                   endif
184300151130e   Ac                   EndIf
184400060922      *
184500060922      *?Original destination state  - - - - - - - - - - - - - - - - -?*
184600060922      *
184700060922sel 1c                   select
184800060922      * diverted
184900060922     c                   when      TR4ods  = '1'
185000060922     c                   eval      wODSdiv = 1
185100060922      * destination not reached
185200060926      *   ?» *CHG «?- Overflow per errori macchina ('2')
185300060926     c                   when           TR4ods  = '2'
185400060926     c                             and (KSAapl <> 'V'
185500060922     c                             OR   KSAapl  = 'V'
185600060926     c                             and  TR4pus  = '0')
185700060922     c                   eval      wODSdnr = 1
185800060922      * destination not available
185900060926      *   ?» *CHG «?- Overflow per uscita piena    ('3')
186000060926     c                   when           TR4ods  = '3'
186100060926     c                             and (KSAapl <> 'V'
186200060922     c                             OR   KSAapl  = 'V'
186300060926     c                             and  TR4pus  = '0')
186400060922     c                   eval      wODSdna = 1
186500060922      * failed to diverted
186600060926      *   ?» *CHG «?- Overflow per errori macchina ('4')
186700060926     c                   when           TR4ods  = '4'
186800060926     c                             and (KSAapl <> 'V'
186900060922     c                             OR   KSAapl  = 'V'
187000060926     c                             and  TR4pus  = '0')
187100060922     c                   eval      wODSftd = 1
187200060922      * invalid destination
187300060926      *   ?» *CHG «?- Overflow per errori macchina ('5')
187400060926     c                   when           TR4ods  = '5'
187500060926     c                             and (KSAapl <> 'V'
187600060922     c                             OR   KSAapl  = 'V'
187700060926     c                             and  TR4pus  = '0')
187800060922     c                   eval      wODSids = 1
187900060922      * unreachable destination
188000060926      *   ?» *CHG «?- Overflow per errori macchina ('6')
188100060926     c                   when           TR4ods  = '6'
188200060926     c                             and (KSAapl <> 'V'
188300060922     c                             OR   KSAapl  = 'V'
188400060926     c                             and  TR4pus  = '0')
188500060922     c                   eval      wODSuds = 1
188600060922      * destination not received
188700060926      *   ?» *CHG «?- Overflow per errori macchina ('7')
188800060926     c                   when           TR4ods  = '7'
188900060926     c                             and (KSAapl <> 'V'
189000060922     c                             OR   KSAapl  = 'V'
189100060926     c                             and  TR4pus  = '0')
189200060922     c                   eval      wODSdne = 1
189300060922      * condition invalid
189400060926      *   ?» *CHG «?- Overflow per errori macchina ('8')
189500060926     c                   when           TR4ods  = '8'
189600060926     c                             and (KSAapl <> 'V'
189700060922     c                             OR   KSAapl  = 'V'
189800060926     c                             and  TR4pus  = '0')
189900060922     c                   eval      wODSciv = 1
190000060922      * destination not requested
190100060926      *   ?» *CHG «?- Overflow per errori macchina ('9')
190200060926     c                   when           TR4ods  = '9'
190300060926     c                             and (KSAapl <> 'V'
190400060922     c                             OR   KSAapl  = 'V'
190500060926     c                             and  TR4pus  = '0')
190600060922     c                   eval      wODSdnq = 1
190700060922      * unknown state
190800061018      *   ?» *CHG «?- Overflow per errori macchina ('0')
190900061018     c                   when           TR4ods  = '0'
191000061018     c                             and (KSAapl <> 'V'
191100061018     c                             OR   KSAapl  = 'V'
191200061018     c                             and  TR4pus  = '0')
191300060922     c                   eval      wODSust = 1
191400060922      * nessun valore conosciuto --> unknown state
191500061018      *   ?» *CHG «?- Overflow per errori macchina (???)
191600060922     c                   other
191700061018     c                   if            (KSAapl <> 'V'
191800061018     c                             OR   KSAapl  = 'V'
191900061018     c                             and  TR4pus  = '0')
192000060922     c                   eval      wODSust = 1
192100061018     c                   endif
192200060922e   1c                   endsl
192300060922      *
192400060922      *?Destination translate state - - - - - - - - - - - - - - - - -?*
192500060922      *
192600060922sel 1c                   select
192700060922      * no translation state
192800060922     c                   when      TR4dts  = '0'
192900060922     c                   eval      wDTSnts = 1
193000060922      * translation OK
193100060922     c                   when      TR4dts  = '1'
193200060922     c                   eval      wDTStok = 1
193300060922      * illegal label
193400060922     c                   when      TR4dts  = '2'
193500060922     c                   eval      wDTSila = 1
193600060922      * no read label
193700060922     c                   when      TR4dts  = '3'
193800060922     c                   eval      wDTSnrl = 1
193900060922      * no destination found
194000060922     c                   when      TR4dts  = '4'
194100060922     c                   eval      wDTSndf = 1
194200060922      * nessun valore conosciuto --> no destination found
194300060922     c                   other
194400060922     c                   eval      wDTSndf = 1
194500060922e   1c                   endsl
194600060922      *
194700060922      *?Parcel scanner data MACCHINONE  - - - - - - - - - - - - - - -?*
194800060922      *
194900060922sel 1c                   select
195000060922      * data non read yeat
195100060922     c                   when      TR4ps1  = '1'
195200060922     c                   eval      wPS1dnr = 1
195300060922      * data too short
195400060922     c                   when      TR4ps1  = '2'
195500060922     c                   eval      wPS1dts = 1
195600060922      * data too long
195700060922     c                   when      TR4ps1  = '3'
195800060922     c                   eval      wPS1dtl = 1
195900060922      * no scan
196000060922     c                   when      TR4ps1  = '4'
196100060922     c                   eval      wPS1nsc = 1
196200060922      * no read
196300060922     c                   when      TR4ps1  = '5'
196400060922     c                   eval      wPS1nre = 1
196500060922      * data OK
196600060922     c                   when      TR4ps1  = '6'
196700060922     c                   eval      wPS1dok = 1
196800060922      * data not OK
196900060922     c                   when      TR4ps1  = '7'
197000060922     c                   eval      wPS1dno = 1
197100060922      * nessun valore conosciuto --> data not OK
197200060922     c                   other
197300060922     c                   eval      wPS1dno = 1
197400060922e   1c                   endsl
197500060922      *
197600060922      *?Parcel scanner data PISTOLA - - - - - - - - - - - - - - - - -?*
197700060922      *
197800060922sel 1c                   select
197900060922      * data non read yeat
198000060922     c                   when      TR4ps2  = '1'
198100060922     c                   eval      wPS2dnr = 1
198200060922      * data too short
198300060922     c                   when      TR4ps2  = '2'
198400060922     c                   eval      wPS2dts = 1
198500060922      * data too long
198600060922     c                   when      TR4ps2  = '3'
198700060922     c                   eval      wPS2dtl = 1
198800060922      * no scan
198900060922     c                   when      TR4ps2  = '4'
199000060922     c                   eval      wPS2nsc = 1
199100060922      * no read
199200060922     c                   when      TR4ps2  = '5'
199300060922     c                   eval      wPS2nre = 1
199400060922      * data OK
199500060922     c                   when      TR4ps2  = '6'
199600060922     c                   eval      wPS2dok = 1
199700060922      * data not OK
199800060922     c                   when      TR4ps2  = '7'
199900060922     c                   eval      wPS2dno = 1
200000060922      * nessun valore conosciuto --> data not OK
200100060922     c                   other
200200060922     c                   eval      wPS2dno = 1
200300060922e   1c                   endsl
200400060922      *
200500060922      *?Parcel entrance state - - - - - - - - - - - - - - - - - - - -?*
200600060922      *
200700060922sel 1c                   select
200800060922      * no entrance state
200900060922     c                   when      TR4pes  = '0'
201000060922     c                   eval      wPESnes = 1
201100060922      * inducted
201200060926      *   ?» *CHG «?- Caricati (1)
201300060926     c                   when           TR4pes  = '1'
201400060926     c                             and (KSAapl <> 'V'
201500060922     c                             OR   KSAapl  = 'V'
201600060922     c                             and (TR4pus  = '0'
201700060926     c                              or  TR4pus  = '1'))
201800061011     c*** anche se da baia 20:     and  *in97   = *on
201900060922     c                   eval      wPESind = 1
202000061003      *   ?» *CHG «?- Caricati (2)
202100061003     c                   when           TR4pes  = '2'
202200061003if  2c                             and  KSAapl  = 'V'
202300061003     c                             and (TR4pus  = '0'
202400061003     c                              or  TR4pus  = '1')
202500061011     c*** anche se da baia 20:     and  *in97   = *on
202600061003     c                   eval      wPESind = 1
202700060922      * unexpected
202800060929     c                   when      TR4pes  = '2'
202900060922     c                   eval      wPESune = 1
203000060922      * nessun valore conosciuto --> unexpected
203100060922     c                   other
203200060922     c                   eval      wPESune = 1
203300060922e   1c                   endsl
203400060922      *
203500060922      *?Parcel exit state - - - - - - - - - - - - - - - - - - - - - -?*
203600060922      *
203700060922sel 1c                   select
203800060922      * no exit state
203900060922     c                   when      TR4pus  = '0'
204000060926     c                   eval      wPUSnes = 1
204100060922      * sorted
204200060926      *   ?» *CHG «?- Smistati correttamente
204300060926     c                   when           TR4pus  = '1'
204400060926     c                             and (KSAapl <> 'V'
204500060926     c                             OR   KSAapl  = 'V'
204600061005     c                             and  TR4pes <> '0')
204700060922     c                   eval      wPUSsor = 1
204800060922      * lost
204900060922     c                   when      TR4pus  = '2'
205000060922     c                   eval      wPUSlos = 1
205100060922      * nessun valore conosciuto --> lost
205200060922     c                   other
205300060922     c                   eval      wPUSlos = 1
205400060922e   1c                   endsl
205500060922      *
205600060922      *?Colli scanner datalogic - - - - - - - - - - - - - - - - - - -?*
205700060922      *
205800060922if  1c                   if             TR4ps1  = '6'
205900060922     c                             and  TR4ps2 <> '6'
206000060922     c                   eval      wCSD    = 1
206100060922e   1c                   endif
206200060922      *
206300060922      *?Colli con peso  - - - - - - - - - - - - - - - - - - - - - - -?*
206400060922      *
206500060922if  1c                   if             TR4ods = '1'
206600060922     c                             and  wPES   > *zeros
206700160510     c                             and (TR4020 <> '2T'
206800160510     c                              or  TR4nfv <> *all'9')
206900060922     c                   eval      wCCP    = 1
207000060922e   1c                   endif
207100060922      *
207200060922      *?Colli con volume  - - - - - - - - - - - - - - - - - - - - - -?*
207300060922      *
207400060922if  1c                   if             TR4ods = '1'
207500060922     c                             and  wVOL   > *zeros
207600060922     c                   eval      wCCV    = 1
207700060922e   1c                   endif
207800060922      *
207900060922      *?Non letti scanner datalogic - - - - - - - - - - - - - - - - -?*
208000060922      *
208100060926if  1c                   if        $okPD1  = *off
208200060922     c                   eval      wNLS    = 1
208300060922e   1c                   endif
208400060922      *
208500060922      *?Non letti pistola - - - - - - - - - - - - - - - - - - - - - -?*
208600060922      *
208700060922sel 1c                   select
208800060922      *
208900060922      * VDL e PICKING VDL CON PC VDL:
209000060922      *   contiene i colli letti SIA da scanner CHE da pistola
209100170125      *   SE entrambi i barcode sono BRT
209200060922     c                   when          (TR4020  = '20'
209300060922     c                              or  TR4020  = '23')
209400060922     c                             and  TR4ps1  = '6'
209500060922     c                             and  TR4ps2  = '6'
209600060926     c                             and  $okPD1  = *on
209700060926     c                             and  $okPD2  = *on
209800060922     c                   eval      wNLP    = 1
209900060922      *
210000060922      * VDL e PICKING VDL PC VDL:
210100060922      *   altri casi NON valorizzo
210200060922     c                   when           TR4020  = '20'
210300060922     c                             or   TR4020  = '23'
210400060922     c                   clear                   wNLP
210500060922      *
210600060922      * DISK C:
210700170125      *   contiene "CHI SONO" (non BRT)
210800060922     c                   when      TR4020  = '21'
210900060926if  2c                   if        $okPD2  = *off
211000060922     c                   eval      wNLP    = 1
211100060922e   2c                   endif
211200060922      *
211300060922      * PICKING NON VDL e VDL CON PC NON VDL:
211400170125      *   contiene barcode BRT
211500060922     c                   when           TR4020  = '24'
211600060922     c                             or   TR4020  = '26'
211700170128     c                             or   TR4020  = '36'
211800060922     c                   clear                   wNLP
211900060922      *
212000060922      * PICKING NON VDL e VDL CON PC NON VDL:
212100060922      *   barcode spedizione NON valorizzo
212200060922     c                   when           TR4020  = '25'
212300060922     c                             or   TR4020  = '27'
212400060922     c                   eval      wNLP    = 1
212500060922      *
212600060922      * RPVP:
212700060922      *   NON valorizzo
212800060922     c                   when           TR4020  = '22'
212900060922     c                             or   TR4020  = '2C'
213000060922     c                             or   TR4020  = '2K'
213100060922     c                   clear                   wNLP
213200060922      *
213300060922     c                   other
213400060922     c                   clear                   wNLP
213500060922      *
213600060922e   1c                   endsl
213700161213      *
213800161213      *?Numero Letture Pistole Wi-Fi  - - - - - - - - - - - - - - - -?*
213900161213if  1c                   if             *in97      = *on
214000161213     c                             and  $MSLw(xx)  = 'W'
214100170118     c                             and  TR4pus    <> '2'
214200161213     c                   eval      wPWF    = 1
214300161213e   1c                   endif
214400060922      *
214500060922     c                   ENDSR
214600060922
214700060922      *---------------------------------------------------------------*
214800170302      *?Aggiornamento campi nel rec. FNMSA00F                        ?*
214900060922      *---------------------------------------------------------------*
215000170302     c     sr_RecMSA     BEGSR
215100060922      *
215200080407     c                   add       wPES          MSApes
215300080407     c                   add       wVOL          MSAvol
215400080407     c                   add       wODSdiv       MSAodsdiv
215500080407     c                   add       wODSdnr       MSAodsdnr
215600080407     c                   add       wODSdna       MSAodsdna
215700080407     c                   add       wODSftd       MSAodsftd
215800080407     c                   add       wODSids       MSAodsids
215900080407     c                   add       wODSuds       MSAodsuds
216000080407     c                   add       wODSdne       MSAodsdne
216100080407     c                   add       wODSciv       MSAodsciv
216200080407     c                   add       wODSdnq       MSAodsdnq
216300080407     c                   add       wODSust       MSAodsust
216400080407     c                   add       wDTSnts       MSAdtsnts
216500080407     c                   add       wDTStok       MSAdtstok
216600080407     c                   add       wDTSila       MSAdtsila
216700080407     c                   add       wDTSnrl       MSAdtsnrl
216800080407     c                   add       wDTSndf       MSAdtsndf
216900080407     c                   add       wPS1dnr       MSAps1dnr
217000080407     c                   add       wPS1dts       MSAps1dts
217100080407     c                   add       wPS1dtl       MSAps1dtl
217200080407     c                   add       wPS1nsc       MSAps1nsc
217300080407     c                   add       wPS1nre       MSAps1nre
217400080407     c                   add       wPS1dok       MSAps1dok
217500080407     c                   add       wPS1dno       MSAps1dno
217600080407     c                   add       wPS2dnr       MSAps2dnr
217700080407     c                   add       wPS2dts       MSAps2dts
217800080407     c                   add       wPS2dtl       MSAps2dtl
217900080407     c                   add       wPS2nsc       MSAps2nsc
218000080407     c                   add       wPS2nre       MSAps2nre
218100080407     c                   add       wPS2dok       MSAps2dok
218200080407     c                   add       wPS2dno       MSAps2dno
218300080407     c                   add       wPESnes       MSApesnes
218400080407     c                   add       wPESind       MSApesind
218500080407     c                   add       wPESune       MSApesune
218600080407     c                   add       wPUSnes       MSApusnes
218700080407     c                   add       wPUSsor       MSApussor
218800080407     c                   add       wPUSlos       MSApuslos
218900080407     c                   add       $PEP(01)      MSApep001
219000080407     c                   add       $PEP(02)      MSApep002
219100080407     c                   add       $PEP(03)      MSApep003
219200080407     c                   add       $PEP(04)      MSApep004
219300080407     c                   add       $PEP(05)      MSApep005
219400080407     c                   add       $PEP(06)      MSApep006
219500080407     c                   add       $PEP(07)      MSApep007
219600080407     c                   add       $PEP(08)      MSApep008
219700080407     c                   add       $PEP(09)      MSApep009
219800080407     c                   add       $PEP(10)      MSApep010
219900080407     c                   add       $PEP(11)      MSApep011
220000080407     c                   add       $PEP(12)      MSApep012
220100080407     c                   add       $PEP(13)      MSApep013
220200080407     c                   add       $PEP(14)      MSApep014
220300080407     c                   add       $PEP(15)      MSApep015
220400080407     c                   add       $PEP(16)      MSApep016
220500080407     c                   add       $PEP(17)      MSApep017
220600080407     c                   add       $PEP(18)      MSApep018
220700080407     c                   add       $PEP(19)      MSApep019
220800080407     c                   add       $PEP(20)      MSApep020
220900080407     c                   add       $PAD(01)      MSApad001
221000080407     c                   add       $PAD(02)      MSApad002
221100080407     c                   add       $PAD(03)      MSApad003
221200080407     c                   add       $PAD(04)      MSApad004
221300080407     c                   add       $PAD(05)      MSApad005
221400080407     c                   add       $PAD(06)      MSApad006
221500080407     c                   add       $PAD(07)      MSApad007
221600080407     c                   add       $PAD(08)      MSApad008
221700080407     c                   add       $PAD(09)      MSApad009
221800080407     c                   add       $PAD(10)      MSApad010
221900080407     c                   add       $PAD(11)      MSApad011
222000080407     c                   add       $PAD(12)      MSApad012
222100080407     c                   add       $PAD(13)      MSApad013
222200080407     c                   add       $PAD(14)      MSApad014
222300080407     c                   add       $PAD(15)      MSApad015
222400080407     c                   add       $PAD(16)      MSApad016
222500080407     c                   add       $PAD(17)      MSApad017
222600080407     c                   add       $PAD(18)      MSApad018
222700080407     c                   add       $PAD(19)      MSApad019
222800080407     c                   add       $PAD(20)      MSApad020
222900080407     c                   add       $PAD(21)      MSApad021
223000080407     c                   add       $PAD(22)      MSApad022
223100080407     c                   add       $PAD(23)      MSApad023
223200080407     c                   add       $PAD(24)      MSApad024
223300080407     c                   add       $PAD(25)      MSApad025
223400080407     c                   add       $PAD(26)      MSApad026
223500080407     c                   add       $PAD(27)      MSApad027
223600080407     c                   add       $PAD(28)      MSApad028
223700080407     c                   add       $PAD(29)      MSApad029
223800080407     c                   add       $PAD(30)      MSApad030
223900080407     c                   add       $PAD(31)      MSApad031
224000080407     c                   add       $PAD(32)      MSApad032
224100080407     c                   add       $PAD(33)      MSApad033
224200080407     c                   add       $PAD(34)      MSApad034
224300080407     c                   add       $PAD(35)      MSApad035
224400080407     c                   add       $PAD(36)      MSApad036
224500080407     c                   add       $PAD(37)      MSApad037
224600080407     c                   add       $PAD(38)      MSApad038
224700080407     c                   add       $PAD(39)      MSApad039
224800080407     c                   add       $PAD(40)      MSApad040
224900080407     c                   add       $PAD(41)      MSApad041
225000080407     c                   add       $PAD(42)      MSApad042
225100080407     c                   add       $PAD(43)      MSApad043
225200080407     c                   add       $PAD(44)      MSApad044
225300080407     c                   add       $PAD(45)      MSApad045
225400080407     c                   add       $PAD(46)      MSApad046
225500080407     c                   add       $PAD(47)      MSApad047
225600080407     c                   add       $PAD(48)      MSApad048
225700080407     c                   add       $PAD(49)      MSApad049
225800080407     c                   add       $PAD(50)      MSApad050
225900080407     c                   add       $PAD(51)      MSApad051
226000080407     c                   add       $PAD(52)      MSApad052
226100080407     c                   add       $PAD(53)      MSApad053
226200080407     c                   add       $PAD(54)      MSApad054
226300080407     c                   add       $PAD(55)      MSApad055
226400080407     c                   add       $PAD(56)      MSApad056
226500080407     c                   add       $PAD(57)      MSApad057
226600080407     c                   add       $PAD(58)      MSApad058
226700080407     c                   add       $PAD(59)      MSApad059
226800080407     c                   add       $PAD(60)      MSApad060
226900080407     c                   add       wCSD          MSAcsd
227000080407     c                   add       wCCP          MSAccp
227100080407     c                   add       wCCV          MSAccv
227200080407     c                   add       wNLS          MSAnls
227300080407     c                   add       wNLP          MSAnlp
227400170302     c                   eval      MSApwf     += wPWF
227500170302     c*//·?NON così/qui:?eval      MSAlav      = KSAlav
227600170302     c*//·?NON così/qui:?eval      MSAimp      = KSAimp
227700060922      *
227800060922     c                   ENDSR
