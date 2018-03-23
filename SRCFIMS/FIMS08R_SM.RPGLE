000100100308     /*END
000200060830      *---------------------------------------------------------------*
000300060831      *?Letture VdL/CML - Visualizzazione e Stampa                   ?*
000400060830      *---------------------------------------------------------------*
000500060830
000600040316     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060830
000800060830      *---------------------------------------------------------------*
000900060830
001000170201     fFNMSA02L  if   e           k disk
001100060601     fAZORG01L  if   e           k disk
001200060601     fTNTBE01L  if   e           k disk
001300060601     fTNTBE02L  if   e           k disk
001400060901     f                                     rename(TNTBE000:TNTBE02)
001500060929     fFIMS08D   cf   e             workstn
001600060929     f                                     sfile(MS08S01:S01nrr)
001700060929     fFIMS08P   o    e             printer oflind(*in01)
001800060601     f                                     usropn
001900060830
002000060830      *---------------------------------------------------------------*
002100060830
002200060830      *
002300060831      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002400060830      *
002500080108     d $MaxEle         c                   const(7500)
002600080102     d $MaxEl2         c                   const(7000)
002700060830      *
002800060831      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
002900060830      *
003000060929      * Decodifiche video
003100060830     d $Dvid           s             15    dim( 5) ctdata perrcd(1)
003200060830      *
003300060929      * Entrance Point
003400060929     d $MSL            s             14    dim(20) inz
003500060929      * Nr. Entrance Point
003600060929     d $Nmsl           s              2  0 dim(20) inz
003700170802      * Flag Entrata Picking
003800170802     d $Pke            s                   like(§MSLpke)
003900170802     d                                     dim(20) inz
004000060830      *
004100060830      * Entrate
004200060830     d $V3ent          s                   like(V3ent01) dim(20)  inz
004300060830     d $V3ent_ds       ds                  inz
004400060830     d   V3ent01                           inz
004500060830     d   V3ent02                           inz
004600060830     d   V3ent03                           inz
004700060830     d   V3ent04                           inz
004800060830     d   V3ent05                           inz
004900060830     d   V3ent06                           inz
005000060830     d   V3ent07                           inz
005100060830     d   V3ent08                           inz
005200060830     d   V3ent09                           inz
005300060830     d   V3ent10                           inz
005400060830     d   V3ent11                           inz
005500060830     d   V3ent12                           inz
005600060830     d   V3ent13                           inz
005700060830     d   V3ent14                           inz
005800060830     d   V3ent15                           inz
005900060830     d   V3ent16                           inz
006000060830     d   V3ent17                           inz
006100060830     d   V3ent18                           inz
006200060830     d   V3ent19                           inz
006300060830     d   V3ent20                           inz
006400060830      *
006500060830      * Colli per entrata
006600060830     d $V2SM_ds        ds                  inz
006700060830     d   V2Sm01                            inz
006800060830     d   V2Sm02                            inz
006900060830     d   V2Sm03                            inz
007000060830     d   V2Sm04                            inz
007100060830     d   V2Sm05                            inz
007200060830     d   V2Sm06                            inz
007300060830     d   V2Sm07                            inz
007400060830     d   V2Sm08                            inz
007500060830     d   V2Sm09                            inz
007600060830     d   V2Sm10                            inz
007700060830     d   V2Sm11                            inz
007800060830     d   V2Sm12                            inz
007900060830     d   V2Sm13                            inz
008000060830     d   V2Sm14                            inz
008100060830     d   V2Sm15                            inz
008200060830     d   V2Sm16                            inz
008300060830     d   V2Sm17                            inz
008400060830     d   V2Sm18                            inz
008500060830     d   V2Sm19                            inz
008600060830     d   V2Sm20                            inz
008700060929     d  $V2SM                  1     60s 0 dim(20)  inz
008800060904     d $DECm_ds        ds                  inz
008900060830     d   DECm01                            inz
009000060830     d   DECm02                            inz
009100060830     d   DECm03                            inz
009200060830     d   DECm04                            inz
009300060830     d   DECm05                            inz
009400060830     d   DECm06                            inz
009500060830     d   DECm07                            inz
009600060830     d   DECm08                            inz
009700060830     d   DECm09                            inz
009800060830     d   DECm10                            inz
009900060830     d   DECm11                            inz
010000060830     d   DECm12                            inz
010100060830     d   DECm13                            inz
010200060830     d   DECm14                            inz
010300060830     d   DECm15                            inz
010400060830     d   DECm16                            inz
010500060830     d   DECm17                            inz
010600060830     d   DECm18                            inz
010700060830     d   DECm19                            inz
010800060830     d   DECm20                            inz
010900060904     d  $DECm                  1     40s 0 dim(20)  inz
011000060904     d $DECe_ds        ds                  inz
011100060904     d   DECe01                            inz
011200060904     d   DECe02                            inz
011300060904     d   DECe03                            inz
011400060904     d   DECe04                            inz
011500060904     d   DECe05                            inz
011600060904     d   DECe06                            inz
011700060904     d   DECe07                            inz
011800060904     d   DECe08                            inz
011900060904     d   DECe09                            inz
012000060904     d   DECe10                            inz
012100060904     d   DECe11                            inz
012200060904     d   DECe12                            inz
012300060904     d   DECe13                            inz
012400060904     d   DECe14                            inz
012500060904     d   DECe15                            inz
012600060904     d   DECe16                            inz
012700060904     d   DECe17                            inz
012800060904     d   DECe18                            inz
012900060904     d   DECe19                            inz
013000060904     d   DECe20                            inz
013100060904     d  $DECe                  1    100s 0 dim(20)  inz
013200060830     d $V3DExx_ds      ds                  inz
013300060830     d   V3De01                            inz
013400060830     d   V3De02                            inz
013500060830     d   V3De03                            inz
013600060830     d   V3De04                            inz
013700060830     d   V3De05                            inz
013800060830     d   V3De06                            inz
013900060830     d   V3De07                            inz
014000060830     d   V3De08                            inz
014100060830     d   V3De09                            inz
014200060830     d   V3De10                            inz
014300060830     d   V3De11                            inz
014400060830     d   V3De12                            inz
014500060830     d   V3De13                            inz
014600060830     d   V3De14                            inz
014700060830     d   V3De15                            inz
014800060830     d   V3De16                            inz
014900060830     d   V3De17                            inz
015000060830     d   V3De18                            inz
015100060830     d   V3De19                            inz
015200060830     d   V3De20                            inz
015300060904     d  $V3DE                  1    100s 0 dim(20)  inz
015400060830      *
015500060830      * Media colli per entrata
015600060830     d $V3DMxx_ds      ds                  inz
015700060830     d   V3Dm01                            inz
015800060830     d   V3Dm02                            inz
015900060830     d   V3Dm03                            inz
016000060830     d   V3Dm04                            inz
016100060830     d   V3Dm05                            inz
016200060830     d   V3Dm06                            inz
016300060830     d   V3Dm07                            inz
016400060830     d   V3Dm08                            inz
016500060830     d   V3Dm09                            inz
016600060830     d   V3Dm10                            inz
016700060830     d   V3Dm11                            inz
016800060830     d   V3Dm12                            inz
016900060830     d   V3Dm13                            inz
017000060830     d   V3Dm14                            inz
017100060830     d   V3Dm15                            inz
017200060830     d   V3Dm16                            inz
017300060830     d   V3Dm17                            inz
017400060830     d   V3Dm18                            inz
017500060830     d   V3Dm19                            inz
017600060830     d   V3Dm20                            inz
017700060904     d  $V3DM                  1     40s 0 dim(20)  inz
017800060831      *
017900060831      * Schiere memorizzazione dati correnti
018000060831      * - Data
018100060929     d $DTL            s              8  0 dim($MaxEle)
018200060831      * - Orario
018300060929     d $ORL            s              4  0 dim($MaxEle)
018400060831      * - Totale colli
018500060929     d $NCL            s              5  0 dim($MaxEle)
018600060831      * - Dettaglio colli x entrata
018700060831     d $PEPxx_ds       ds                  occurs($MaxEle)  inz
018800060904     d   $PEPxx                       5  0 dim(20)          inz
018900060831     ***d Mpe1            s              5  0 dim($MaxEle)                         *entrata 1
019000060831     ***d Mpe2            s              5  0 dim($MaxEle)                         *entrata 2
019100060831     ***d Mpe3            s              5  0 dim($MaxEle)                         *entrata 3
019200060831     ***d Mpe4            s              5  0 dim($MaxEle)                         *entrata 4
019300060831     ***d Mpe5            s              5  0 dim($MaxEle)                         *entrata 5
019400060831     ***d Mpe6            s              5  0 dim($MaxEle)                         *entrata 6
019500060831     ***d Mpe7            s              5  0 dim($MaxEle)                         *entrata 7
019600060831     ***d Mpe8            s              5  0 dim($MaxEle)                         *entrata 8
019700060831     ***d Mpe9            s              5  0 dim($MaxEle)                         *entrata 9
019800060831     ***d Mpe0            s              5  0 dim($MaxEle)                         *entrata 10
019900060831     ***d Mpe11           s              5  0 dim($MaxEle)                         *entrata 1
020000060831     ***d Mpe12           s              5  0 dim($MaxEle)                         *entrata 2
020100060831     ***d Mpe13           s              5  0 dim($MaxEle)                         *entrata 3
020200060831     ***d Mpe14           s              5  0 dim($MaxEle)                         *entrata 4
020300060831     ***d Mpe15           s              5  0 dim($MaxEle)                         *entrata 5
020400060831     ***d Mpe16           s              5  0 dim($MaxEle)                         *entrata 6
020500060831     ***d Mpe17           s              5  0 dim($MaxEle)                         *entrata 7
020600060831     ***d Mpe18           s              5  0 dim($MaxEle)                         *entrata 8
020700060831     ***d Mpe19           s              5  0 dim($MaxEle)                         *entrata 9
020800060831     ***d Mpe20           s              5  0 dim($MaxEle)                         *entrata 10
020900060831      * - Dettaglio colli x uscita
021000060831     d $PADxx_ds       ds                  occurs($MaxEle)  inz
021100060904     d   $PADxx                       5  0 dim(60)          inz
021200060831     ***d Mpa1            s              5  0 dim($MaxEle)                         *uscita 1
021300060831     ***d Mpa2            s              5  0 dim($MaxEle)                         *uscita 2
021400060831     ***d Mpa3            s              5  0 dim($MaxEle)                         *uscita 3
021500060831     ***d Mpa4            s              5  0 dim($MaxEle)                         *uscita 4
021600060831     ***d Mpa5            s              5  0 dim($MaxEle)                         *uscita 5
021700060831     ***d Mpa6            s              5  0 dim($MaxEle)                         *uscita 6
021800060831     ***d Mpa7            s              5  0 dim($MaxEle)                         *uscita 7
021900060831     ***d Mpa8            s              5  0 dim($MaxEle)                         *uscita 8
022000060831     ***d Mpa9            s              5  0 dim($MaxEle)                         *uscita 9
022100060831     ***d Mpa10           s              5  0 dim($MaxEle)                         *uscita 10
022200060831     ***d Mpa11           s              5  0 dim($MaxEle)                         *uscita 11
022300060831     ***d Mpa12           s              5  0 dim($MaxEle)                         *uscita 12
022400060831     ***d Mpa13           s              5  0 dim($MaxEle)                         *uscita 13
022500060831     ***d Mpa14           s              5  0 dim($MaxEle)                         *uscita 14
022600060831     ***d Mpa15           s              5  0 dim($MaxEle)                         *uscita 15
022700060831     ***d Mpa16           s              5  0 dim($MaxEle)                         *uscita 16
022800060831     ***d Mpa17           s              5  0 dim($MaxEle)                         *uscita 17
022900060831     ***d Mpa18           s              5  0 dim($MaxEle)                         *uscita 18
023000060831     ***d Mpa19           s              5  0 dim($MaxEle)                         *uscita 19
023100060831     ***d Mpa20           s              5  0 dim($MaxEle)                         *uscita 20
023200060831     ***d Mpa21           s              5  0 dim($MaxEle)                         *uscita 21
023300060831     ***d Mpa22           s              5  0 dim($MaxEle)                         *uscita 22
023400060831     ***d Mpa23           s              5  0 dim($MaxEle)                         *uscita 23
023500060831     ***d Mpa24           s              5  0 dim($MaxEle)                         *uscita 24
023600060831     ***d Mpa25           s              5  0 dim($MaxEle)                         *uscita 25
023700060831     ***d Mpa26           s              5  0 dim($MaxEle)                         *uscita 26
023800060831     ***d Mpa27           s              5  0 dim($MaxEle)                         *uscita 27
023900060831     ***d Mpa28           s              5  0 dim($MaxEle)                         *uscita 28
024000060831     ***d Mpa29           s              5  0 dim($MaxEle)                         *uscita 29
024100060831     ***d Mpa30           s              5  0 dim($MaxEle)                         *uscita 30
024200060831     ***d Mpa31           s              5  0 dim($MaxEle)                         *uscita 31
024300060831     ***d Mpa32           s              5  0 dim($MaxEle)                         *uscita 32
024400060831     ***d Mpa33           s              5  0 dim($MaxEle)                         *uscita 33
024500060831     ***d Mpa34           s              5  0 dim($MaxEle)                         *uscita 34
024600060831     ***d Mpa35           s              5  0 dim($MaxEle)                         *uscita 35
024700060831     ***d Mpa36           s              5  0 dim($MaxEle)                         *uscita 36
024800060831     ***d Mpa37           s              5  0 dim($MaxEle)                         *uscita 37
024900060831     ***d Mpa38           s              5  0 dim($MaxEle)                         *uscita 38
025000060831     ***d Mpa39           s              5  0 dim($MaxEle)                         *uscita 39
025100060831     ***d Mpa40           s              5  0 dim($MaxEle)                         *uscita 40
025200060831     ***d Mpa41           s              5  0 dim($MaxEle)                         *uscita 41
025300060831     ***d Mpa42           s              5  0 dim($MaxEle)                         *uscita 42
025400060831     ***d Mpa43           s              5  0 dim($MaxEle)                         *uscita 43
025500060831     ***d Mpa44           s              5  0 dim($MaxEle)                         *uscita 44
025600060831     ***d Mpa45           s              5  0 dim($MaxEle)                         *uscita 45
025700060831     ***d Mpa46           s              5  0 dim($MaxEle)                         *uscita 46
025800060831     ***d Mpa47           s              5  0 dim($MaxEle)                         *uscita 47
025900060831     ***d Mpa48           s              5  0 dim($MaxEle)                         *uscita 48
026000060831     ***d Mpa49           s              5  0 dim($MaxEle)                         *uscita 49
026100060831     ***d Mpa50           s              5  0 dim($MaxEle)                         *uscita 50
026200060831     ***d Mpa51           s              5  0 dim($MaxEle)                         *uscita 51
026300060831     ***d Mpa52           s              5  0 dim($MaxEle)                         *uscita 52
026400060831     ***d Mpa53           s              5  0 dim($MaxEle)                         *uscita 53
026500060831     ***d Mpa54           s              5  0 dim($MaxEle)                         *uscita 54
026600060831     ***d Mpa55           s              5  0 dim($MaxEle)                         *uscita 55
026700060831     ***d Mpa56           s              5  0 dim($MaxEle)                         *uscita 56
026800060831     ***d Mpa57           s              5  0 dim($MaxEle)                         *uscita 57
026900060831     ***d Mpa58           s              5  0 dim($MaxEle)                         *uscita 58
027000060831     ***d Mpa59           s              5  0 dim($MaxEle)                         *uscita 59
027100060831     ***d Mpa60           s              5  0 dim($MaxEle)                         *uscita 60
027200060929      * per Statistiche
027300060831     d $pesind         s              5  0 dim($MaxEle)                         *caricati da
027400060831     d $pussor         s              5  0 dim($MaxEle)                         *smistati da
027500060831     d $puslos         s              5  0 dim($MaxEle)                         *smistati da
027600060831     d $pusnes         s              5  0 dim($MaxEle)                         *smistati da
027700060831     d $csd            s              5  0 dim($MaxEle)                         *letti da scanner
027800060831     d $ps2dok         s              5  0 dim($MaxEle)                         *letti da pistola
027900060831     d $ccv            s              5  0 dim($MaxEle)                         *letti con volume
028000060831     d $ccp            s              5  0 dim($MaxEle)                         *letti con peso
028100060831     d $nls            s              5  0 dim($MaxEle)                         *non letti scanner
028200060831     d $nlp            s              5  0 dim($MaxEle)                         *non letti pistola
028300060831     d $xnomis         s              5  0 dim($MaxEle)                         *C:non misurati
028400060831     d $xnopes         s              5  0 dim($MaxEle)                         *C:non pesati
028500060831     d $odsdna         s              5  0 dim($MaxEle)                         *of x uscita piena
028600060831     d $errmac         s              5  0 dim($MaxEle)                         *of x errori machine
028700060831     d $pesind2        s              5  0 dim($MaxEle)                         »caricati da
028800060831     d $pussor2        s              5  0 dim($MaxEle)                         »smistati da
028900060831     d $odsdna2        s              5  0 dim($MaxEle)                         »of x uscita piena
029000060831     d $errmac2        s              5  0 dim($MaxEle)                         »of x errori machine
029100060831      *
029200060929      * Schiere memorizzazione dati precedenti
029300060929      * - Data         - precedente
029400060831     d $Pdtl           s              8  0 dim($MaxEle)                         *PRECEDENTE data
029500060929      * - Orario       - precedente
029600060831     d $Porl           s              4  0 dim($MaxEle)                         *PRECEDENTE ora
029700060929      * - Totale colli - precedente
029800060831     d $Pncl           s              5  0 dim($MaxEle)                         *PRECEDENTE colli
029900060831      *
030000060929      * Schiere griglia
030100060929      * - Dettaglio
030200060831     d GDper           s             12  0 dim($MaxEle)
030300060929      * - Riepilogo
030400060831     d GRper           s             12  0 dim(500)
030500060929      * - Unione
030600080909     d $Gtip           s              1    dim($MaxEl2)  inz                    *tipo griglia
030700080909     d $Gdtl           s              8  0 dim($MaxEl2)  inz                    *data
030800080909     d $Gorl           s              4  0 dim($MaxEl2)  inz                    *ora
030900060830      *
031000060929      * Schiere video
031100060929      * - Descrizione
031200060830     d $Vdes           s                   like(V2Sdes)  dim($MaxEl2)  inz
031300060929      * - Tipo Riga
031400060830     d $Vtip           s              1    dim($MaxEl2)  inz
031500060929      * - Data
031600060830     d $Vdtl           s              8  0 dim($MaxEl2)  inz
031700060929      * - Orario
031800060901     d $Vorl           s              4  0 dim($MaxEl2)  inz
031900060929      * - Numero colli
032000060830     d $Vncl           s              5  0 dim($MaxEl2)  inz
032100060929      * - Minuti lavorati
032200060830     d $VminLav        s              4  0 dim($MaxEl2)  inz
032300060830      *
032400060830      * Entrate
032500060830     d $VPExx_ds       ds                  occurs(20)    inz
032600060904     d   $VPExx                            like(V3De01)  dim($MaxEl2)  inz
032700060830     ***d vpe1            S              5  0 DIM(6000)                            *entrata 1
032800060830     ***d vpe2            S              5  0 DIM(6000)                            *entrata 2
032900060830     ***d vpe3            S              5  0 DIM(6000)                            *entrata 3
033000060830     ***d vpe4            S              5  0 DIM(6000)                            *entrata 4
033100060830     ***d vpe5            S              5  0 DIM(6000)                            *entrata 5
033200060830     ***d vpe6            S              5  0 DIM(6000)                            *entrata 6
033300060830     ***d vpe7            S              5  0 DIM(6000)                            *entrata 7
033400060830     ***d vpe8            S              5  0 DIM(6000)                            *entrata 8
033500060830     ***d vpe9            S              5  0 DIM(6000)                            *entrata 9
033600060830     ***d vpe0            S              5  0 DIM(6000)                            *entrata 10
033700060830     ***d vpe11           S              5  0 DIM(6000)                            *entrata 1
033800060830     ***d vpe12           S              5  0 DIM(6000)                            *entrata 2
033900060830     ***d vpe13           S              5  0 DIM(6000)                            *entrata 3
034000060830     ***d vpe14           S              5  0 DIM(6000)                            *entrata 4
034100060830     ***d vpe15           S              5  0 DIM(6000)                            *entrata 5
034200060830     ***d vpe16           S              5  0 DIM(6000)                            *entrata 6
034300060830     ***d vpe17           S              5  0 DIM(6000)                            *entrata 7
034400060830     ***d vpe18           S              5  0 DIM(6000)                            *entrata 8
034500060830     ***d vpe19           S              5  0 DIM(6000)                            *entrata 9
034600060830     ***d vpe20           S              5  0 DIM(6000)                            *entrata 10
034700060830      *
034800060830      * Minuti lavorati per entrata
034900060830     d $VMLAVxx_ds     ds                  occurs(20)    inz
035000060904     d   $VMLAVxx                          like(V2Sorl)  dim($MaxEl2)  inz
035100060830     ***d vmlav01         S              4  0 DIM(6000)                            *mm lavorati ent
035200060830     ***d vmlav02         S              4  0 DIM(6000)                            *mm lavorati ent
035300060830     ***d vmlav03         S              4  0 DIM(6000)                            *mm lavorati ent
035400060830     ***d vmlav04         S              4  0 DIM(6000)                            *mm lavorati ent
035500060830     ***d vmlav05         S              4  0 DIM(6000)                            *mm lavorati ent
035600060830     ***d vmlav06         S              4  0 DIM(6000)                            *mm lavorati ent
035700060830     ***d vmlav07         S              4  0 DIM(6000)                            *mm lavorati ent
035800060830     ***d vmlav08         S              4  0 DIM(6000)                            *mm lavorati ent
035900060830     ***d vmlav09         S              4  0 DIM(6000)                            *mm lavorati ent
036000060830     ***d vmlav10         S              4  0 DIM(6000)                            *mm lavorati ent
036100060830     ***d vmlav11         S              4  0 DIM(6000)                            *mm lavorati ent
036200060830     ***d vmlav12         S              4  0 DIM(6000)                            *mm lavorati ent
036300060830     ***d vmlav13         S              4  0 DIM(6000)                            *mm lavorati ent
036400060830     ***d vmlav14         S              4  0 DIM(6000)                            *mm lavorati ent
036500060830     ***d vmlav15         S              4  0 DIM(6000)                            *mm lavorati ent
036600060830     ***d vmlav16         S              4  0 DIM(6000)                            *mm lavorati ent
036700060830     ***d vmlav17         S              4  0 DIM(6000)                            *mm lavorati ent
036800060830     ***d vmlav18         S              4  0 DIM(6000)                            *mm lavorati ent
036900060830     ***d vmlav19         S              4  0 DIM(6000)                            *mm lavorati ent
037000060830     ***d vmlav20         S              4  0 DIM(6000)                            *mm lavorati ent
037100060830      *
037200060830      * Colli per Uscita
037300060831     d $VPAxx_ds       ds                  occurs($MaxEl2)  inz
037400060904     d   $VPAxx                            like(V3Du01)     dim(60)  inz
037500060831     d $VPAD_ds        ds                  inz
037600060831     d   V3Du01                            inz
037700060831     d   V3Du02                            inz
037800060831     d   V3Du03                            inz
037900060831     d   V3Du04                            inz
038000060831     d   V3Du05                            inz
038100060831     d   V3Du06                            inz
038200060831     d   V3Du07                            inz
038300060831     d   V3Du08                            inz
038400060831     d   V3Du09                            inz
038500060831     d   V3Du10                            inz
038600060831     d   V3Du11                            inz
038700060831     d   V3Du12                            inz
038800060831     d   V3Du13                            inz
038900060831     d   V3Du14                            inz
039000060831     d   V3Du15                            inz
039100060831     d   V3Du16                            inz
039200060831     d   V3Du17                            inz
039300060831     d   V3Du18                            inz
039400060831     d   V3Du19                            inz
039500060831     d   V3Du20                            inz
039600060831     d   V3Du21                            inz
039700060831     d   V3Du22                            inz
039800060831     d   V3Du23                            inz
039900060831     d   V3Du24                            inz
040000060831     d   V3Du25                            inz
040100060831     d   V3Du26                            inz
040200060831     d   V3Du27                            inz
040300060831     d   V3Du28                            inz
040400060831     d   V3Du29                            inz
040500060831     d   V3Du30                            inz
040600060831     d   V3Du31                            inz
040700060831     d   V3Du32                            inz
040800060831     d   V3Du33                            inz
040900060831     d   V3Du34                            inz
041000060831     d   V3Du35                            inz
041100060831     d   V3Du36                            inz
041200060831     d   V3Du37                            inz
041300060831     d   V3Du38                            inz
041400060831     d   V3Du39                            inz
041500060831     d   V3Du40                            inz
041600060831     d   V3Du41                            inz
041700060831     d   V3Du42                            inz
041800060831     d   V3Du43                            inz
041900060831     d   V3Du44                            inz
042000060831     d   V3Du45                            inz
042100060831     d   V3Du46                            inz
042200060831     d   V3Du47                            inz
042300060831     d   V3Du48                            inz
042400060831     d   V3Du49                            inz
042500060831     d   V3Du50                            inz
042600060831     d   V3Du51                            inz
042700060831     d   V3Du52                            inz
042800060831     d   V3Du53                            inz
042900060831     d   V3Du54                            inz
043000060831     d   V3Du55                            inz
043100060831     d   V3Du56                            inz
043200060831     d   V3Du57                            inz
043300060831     d   V3Du58                            inz
043400060831     d   V3Du59                            inz
043500060831     d   V3Du60                            inz
043600060831     ***d Vpa1            s              5  0 dim($MaxEl2)                         *uscita 1
043700060831     ***d Vpa2            s              5  0 dim($MaxEl2)                         *uscita 2
043800060831     ***d Vpa3            s              5  0 dim($MaxEl2)                         *uscita 3
043900060831     ***d Vpa4            s              5  0 dim($MaxEl2)                         *uscita 4
044000060831     ***d Vpa5            s              5  0 dim($MaxEl2)                         *uscita 5
044100060831     ***d Vpa6            s              5  0 dim($MaxEl2)                         *uscita 6
044200060831     ***d Vpa7            s              5  0 dim($MaxEl2)                         *uscita 7
044300060831     ***d Vpa8            s              5  0 dim($MaxEl2)                         *uscita 8
044400060831     ***d Vpa9            s              5  0 dim($MaxEl2)                         *uscita 9
044500060831     ***d Vpa10           s              5  0 dim($MaxEl2)                         *uscita 10
044600060831     ***d Vpa11           s              5  0 dim($MaxEl2)                         *uscita 11
044700060831     ***d Vpa12           s              5  0 dim($MaxEl2)                         *uscita 12
044800060831     ***d Vpa13           s              5  0 dim($MaxEl2)                         *uscita 13
044900060831     ***d Vpa14           s              5  0 dim($MaxEl2)                         *uscita 14
045000060831     ***d Vpa15           s              5  0 dim($MaxEl2)                         *uscita 15
045100060831     ***d Vpa16           s              5  0 dim($MaxEl2)                         *uscita 16
045200060831     ***d Vpa17           s              5  0 dim($MaxEl2)                         *uscita 17
045300060831     ***d Vpa18           s              5  0 dim($MaxEl2)                         *uscita 18
045400060831     ***d Vpa19           s              5  0 dim($MaxEl2)                         *uscita 19
045500060831     ***d Vpa20           s              5  0 dim($MaxEl2)                         *uscita 20
045600060831     ***d Vpa21           s              5  0 dim($MaxEl2)                         *uscita 21
045700060831     ***d Vpa22           s              5  0 dim($MaxEl2)                         *uscita 22
045800060831     ***d Vpa23           s              5  0 dim($MaxEl2)                         *uscita 23
045900060831     ***d Vpa24           s              5  0 dim($MaxEl2)                         *uscita 24
046000060831     ***d Vpa25           s              5  0 dim($MaxEl2)                         *uscita 25
046100060831     ***d Vpa26           s              5  0 dim($MaxEl2)                         *uscita 26
046200060831     ***d Vpa27           s              5  0 dim($MaxEl2)                         *uscita 27
046300060831     ***d Vpa28           s              5  0 dim($MaxEl2)                         *uscita 28
046400060831     ***d Vpa29           s              5  0 dim($MaxEl2)                         *uscita 29
046500060831     ***d Vpa30           s              5  0 dim($MaxEl2)                         *uscita 30
046600060831     ***d Vpa31           s              5  0 dim($MaxEl2)                         *uscita 31
046700060831     ***d Vpa32           s              5  0 dim($MaxEl2)                         *uscita 32
046800060831     ***d Vpa33           s              5  0 dim($MaxEl2)                         *uscita 33
046900060831     ***d Vpa34           s              5  0 dim($MaxEl2)                         *uscita 34
047000060831     ***d Vpa35           s              5  0 dim($MaxEl2)                         *uscita 35
047100060831     ***d Vpa36           s              5  0 dim($MaxEl2)                         *uscita 36
047200060831     ***d Vpa37           s              5  0 dim($MaxEl2)                         *uscita 37
047300060831     ***d Vpa38           s              5  0 dim($MaxEl2)                         *uscita 38
047400060831     ***d Vpa39           s              5  0 dim($MaxEl2)                         *uscita 39
047500060831     ***d Vpa40           s              5  0 dim($MaxEl2)                         *uscita 40
047600060831     ***d Vpa41           s              5  0 dim($MaxEl2)                         *uscita 41
047700060831     ***d Vpa42           s              5  0 dim($MaxEl2)                         *uscita 42
047800060831     ***d Vpa43           s              5  0 dim($MaxEl2)                         *uscita 43
047900060831     ***d Vpa44           s              5  0 dim($MaxEl2)                         *uscita 44
048000060831     ***d Vpa45           s              5  0 dim($MaxEl2)                         *uscita 45
048100060831     ***d Vpa46           s              5  0 dim($MaxEl2)                         *uscita 46
048200060831     ***d Vpa47           s              5  0 dim($MaxEl2)                         *uscita 47
048300060831     ***d Vpa48           s              5  0 dim($MaxEl2)                         *uscita 48
048400060831     ***d Vpa49           s              5  0 dim($MaxEl2)                         *uscita 49
048500060831     ***d Vpa50           s              5  0 dim($MaxEl2)                         *uscita 50
048600060831     ***d Vpa51           s              5  0 dim($MaxEl2)                         *uscita 51
048700060831     ***d Vpa52           s              5  0 dim($MaxEl2)                         *uscita 52
048800060831     ***d Vpa53           s              5  0 dim($MaxEl2)                         *uscita 53
048900060831     ***d Vpa54           s              5  0 dim($MaxEl2)                         *uscita 54
049000060831     ***d Vpa55           s              5  0 dim($MaxEl2)                         *uscita 55
049100060831     ***d Vpa56           s              5  0 dim($MaxEl2)                         *uscita 56
049200060831     ***d Vpa57           s              5  0 dim($MaxEl2)                         *uscita 57
049300060831     ***d Vpa58           s              5  0 dim($MaxEl2)                         *uscita 58
049400060831     ***d Vpa59           s              5  0 dim($MaxEl2)                         *uscita 59
049500060831     ***d Vpa60           s              5  0 dim($MaxEl2)                         *uscita 60
049600060929      * Calcolo dettaglio
049700060831     d Vpnc            s              5  0 dim($MaxEl2)                         *prg colli -corr-
049800060831     d VPpnc           s              5  0 dim($MaxEl2)                         *prg colli -prec-
049900060831     d Vpsp            s              3  0 dim($MaxEl2)                         *% scostamento
050000060929      * per Statistiche
050100060831     d Vpesind         s              5  0 dim($MaxEl2)                         *caricati da
050200060831     d Vpussor         s              5  0 dim($MaxEl2)                         *smistati da
050300060831     d Vpuslos         s              5  0 dim($MaxEl2)                         *smistati da
050400060831     d Vpusnes         s              5  0 dim($MaxEl2)                         *smistati da
050500060831     d Vcsd            s              5  0 dim($MaxEl2)                         *letti da scanner
050600060831     d Vps2dok         s              5  0 dim($MaxEl2)                         *letti da pistola
050700060831     d Vccv            s              5  0 dim($MaxEl2)                         *letti con volume
050800060831     d Vccp            s              5  0 dim($MaxEl2)                         *letti con peso
050900060831     d Vnls            s              5  0 dim($MaxEl2)                         *non letti scanner
051000060831     d Vnlp            s              5  0 dim($MaxEl2)                         *non letti pistola
051100060831     d Vxnomis         s              5  0 dim($MaxEl2)                         *C:non misurati
051200060831     d Vxnopes         s              5  0 dim($MaxEl2)                         *C:non pesati
051300060831     d Vodsdna         s              5  0 dim($MaxEl2)                         *of x uscita piena
051400060831     d Verrmac         s              5  0 dim($MaxEl2)                         *of x errori machine
051500060831     d Vchi            s              5  0 dim($MaxEl2)                         *non letti scanner
051600060831     d Vldv            s              5  0 dim($MaxEl2)                         *non letti scanner
051700060831     d Vbar            s              5  0 dim($MaxEl2)                         *non letti scanner
051800060904     d VbarP           s              5  0 dim($MaxEl2)                         *non letti scanner
051900060831     d Vpesind2        s              5  0 dim($MaxEl2)                         »caricati
052000060831     d Vpussor2        s              5  0 dim($MaxEl2)                         »smistati correttam.
052100060831     d Vodsdna2        s              5  0 dim($MaxEl2)                         »of x uscita piena
052200060831     d Verrmac2        s              5  0 dim($MaxEl2)                         »of x errori machine
052300060830      *
052400060831      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
052500060830      *
052600060830      * - Parametri
052700060830     d KPJBA         e ds                                                       *architettura
052800060830     d FIMS00ds      e ds                                                       *ds di procedura
052900060830      *
053000060830      * - Parametri per richiamo al pgm di controllo profilo utenti
053100060830     d Tibs34Ds      e ds                  inz
053200060830      * - Ds di riferimento al file esterno AzUte00F
053300060830     d AzuteDs       e ds                  ExtName(AzUte00F)
053400060830      * - Ds per dati organigramma
053500060830     d DDatiUte      e ds
053600060830      *
053700060830      * - Reperimento tabelle
053800060830     d TIBS02ds      e ds                  inz
053900060830     d   T02mod      e                     inz('C')
054000060830      * - Tab. MSL = Layout macchinone smistacolli
054100060830     d dMSL          e ds                  inz
054200060830      * - Tab. MTP = Macchinone tipo applicazione
054300060830     d dMTP          e ds                  inz
054400060830      * - Obiettivi di Produttività di un Impianto Automatico Smistacolli
054500060830     d dKPI          e ds                  inz
054600061002      *
054700061002     d Status         sds           333
054800061002     d   SDSpgm          *proc
054900060830      *
055000060830     d                 ds
055100060830     d Wtempo                          z   inz(Z'1901-01-01-01.00.00.000000')
055200060830     d  Wdata                          d   datfmt(*ISO) overlay(Wtempo)
055300060830     d  Wora                           t   timfmt(*ISO) overlay(Wtempo:12)
055400060929     d Wtempo1                         z   inz(z'1901-01-01-01.00.00.000000')
055500060929     d Wtempo2                         z   inz(z'1901-01-01-01.00.00.000000')
055600060830     d W00tli                          z   inz(*loval)
055700060830     d W00tlf                          z   inz
055800060830      *
055900060830      * - Controllo/Inversione data
056000060830     d WLBDAT          ds                  inz
056100060830     d  G08dat                        8  0 inz
056200060830     d  G08inv                        8  0 inz
056300060830     d  G08err                        1    inz('3')
056400060830     d  G08tgi                        5  0 inz
056500060830      *
056600060830      * - Calcolo data (8)
056700060830     d WGIda8          ds                  inz
056800060830     d  GioDat                        8  0 inz
056900060830     d  GioInv                        8  0 inz
057000060830     d  GioTgi                        5  0 inz
057100060830      *
057200060831      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
057300060830      *
057400060830      * - Flags booleani
057500060830     d $Fine           S              1    inz(*off)                            *fine programma
057600060830     d $FNMSA          s              1    inz(*off)                            *esistenza dati
057700061002     ***d $FNMSApr        s              1    inz(*off)                            *esist. dati PREC
057800060831     d $RecOK          S              1    inz(*off)                            *validità record
057900080909     d $TooManyRec     s               n   inz(*off)
058000060929      *
058100060830     d $Video          s              1    inz('1')                             *video in gestione
058200060830      *
058300060830      * - Indici di schiera / Contatori
058400060830     d $nro            s              2  0 inz
058500061006     d xw              s              5  0 inz
058600060830     d xx              s              5  0 inz
058700060830     d xi              s              5  0 inz
058800060830     d  MEMxi          s              5  0 inz
058900060830     d  MEMgd          s              5  0 inz
059000060830     d  xm             s              5  0 inz
059100060830     d xj              s              5  0 inz
059200060831     d xg              s              5  0 inz
059300060831     d  MEMxg          s              5  0 inz
059400060830     d xv              s              5  0 inz
059500060830     d  MEMxv          s              5  0 inz
059600060904     d pj              s              5  0 inz
059700060904     d pk              s              5  0 inz
059800060904     d ppj             s              5  0 inz
059900060904     d ppk             s              5  0 inz
060000060830      *
060100060830      * - Parametri per QCMDEXC
060200060831     d Qcmd            s             80    inz('WRKSPLF   ')
060300060901     d Qlen            s             15  5 inz(80)
060400060830      *
060500060830      * - Campi di comodo
060600060830     d WdateISO        s               d   datfmt(*iso) inz(*loval)
060700060830     d D00dfvPR        s                   like(D00dfv) inz
060800060830     d D00dfvGG        s                   like(G08tgi) inz
060900061018      * - Parametri per analisi della produttività di un
061000060929      *   impianto automatico smistacolli
061100061006     d WnclOb          s              7  0 inz                                  tot colli lavorabili
061200061024      * - Campo per ripartizione colli della post.20 su tutte le altre
061300061024     d wRipColli       s                   like(V3DE20) inz
061400061024     d wTotColli       s                   like(V3DE20) inz
061500060830      *
061600060929     d S01nrr          s                   like(C01rcd) inz                     *n° rel sf
061700060929     d LASTnrr         s                   like(C01rcd) inz                     *ultimo nrr del sfl
061800060929     d SAVnrr          s                   like(C01rcd) inz                     *memo   nrr del sfl
061900060830      *
062000060830     d n14             s             14  0 inz
062100060830     d n8              s              8  0 inz
062200060830     d n6              s              6  0 inz
062300060830     d n4              s              4  0 inz
062400060830      *
062500040609     d wDays           s              3  0 inz
062600060831     d DatCor          s              8  0 inz                                  *data correrte a/m/g
062700060831     d ULTmsa          s              5  0 inz                                  *ultimo elemento
062800060831     d ULTmsaPR        s              5  0 inz                                  *ultimo elemento PRE
062900060831     d PrimaData       s             12  0 inz                                  *prima  data + ora
063000060831     d UltimData       s             12  0 inz                                  *ultima data + ora
063100060831     d Ldata           s              8  0 inz                                  *data di lavoro
063200060831     d Lora            s              4  0 inz                                  *ora  di lavoro
063300060831     d Lmin            s              2  0 inz                                  *mm   di lavoro
063400060831     d Lpergri         s             12  0 inz                                  *periodo di lavoro
063500060831     d Lpergri2        s             12  0 inz                                  *periodo di lavoro
063600060831     d LpergriP        s             12  0 inz                                  *periodo di lavoro
063700060831     d Lpermem         s             12  0 inz                                  *periodo di lavoro
063800060831     d DEPtip          s              1    inz                                  *dep tipo griglia
063900060831     d MinLav          s              4  0 inz                                  *mm lavorati  totali
064000060831     d wMinLav         s              4  0 inz                                  *di lavoro
064100060831     d MirLav          s              4  0 inz                                  *mm lavorati  totali
064200060831     d wMirLav         s              4  0 inz                                  *di lavoro
064300060831     d MinTra          s              4  0 inz                                  *mm trascorsi totali
064400060901     d MirTra          s              4  0 inz                                  *mm trascorsi totali
064500060831     d MEMtco          s              5  0 inz                                  *pacchi
064600060831     d MEMtlm          s              4  0 inz                                  *mm lavorati
064700060831     d MEMtcl          s              6  1 inz                                  *Pacchi/TempoLav
064800060831     d MEMtkp          s              6  1 inz                                  *KPI obiettivo
064900060831     d MEMnco          s              5  0 inz                                  *pacchi
065000060831     d MEMncl          s              6  1 inz                                  *Pacchi/TempoLav
065100060831     d MEMttm          s              4  0 inz                                  *mm trascorsi
065200060831     d MEMrco          s              5  0 inz                                  *pacchi
065300060831     d MEMrlm          s              4  0 inz                                  *mm lavorati
065400060831     d MEMrcl          s              6  1 inz                                  *Pacchi/TempoLav
065500060831     d MEMrkp          s              6  1 inz                                  *KPI obiettivo
065600060831     d MEMrtm          s              4  0 inz                                  *mm trascorsi totali
065700060929     d §PNCatt         s             11  3 inz                                  *prg colli attuale
065800060929     d §PNCpre         s             11  3 inz                                  *prg colli preced
065900060929     d §PNCpsp         s              3  0 inz                                  *% scostamento
066000060929     d §PNCgap         s             18  6 inz
066100060831     d §min            s              4  0 inz                                  *minuti totali
066200060831     d §gg             s              2  0 inz                                  *giorni trascors9
066300060831     d §hh             s              2  0 inz                                  *ore    trascorse
066400060831     d §mm             s              2  0 inz                                  *minuti trascorsi
066500070510      * - Campi per la ripartizione dei colli in post.20
066600061002     d W_prod_rel      s             18 15 inz
066700071123     d W_loaded_B      s              5  0 inz
066800061205
066900070510      * - Campi di comodo
067000061205     d w0050           s              5  0 inz
067100070510     d w0091           s              9  1 inz
067200060830      *
067300060831      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
067400060830      *
067500060830      * - archivio TNTBE01L
067600060830     c     K05TBE01      klist
067700060830     c                   kfld                    TBEcod                          -tabella
067800060830     c                   kfld                    TBEke1                          -chiave uno
067900060830     c                   kfld                    TBEke2                          -chiave due
068000060830     c                   kfld                    TBElin                          -lingua
068100060830     c                   kfld                    TBEsif                          -s.informativo
068200060830     c     K02TBE01      klist
068300060830     c                   kfld                    TBEcod                          -tabella
068400060830     c                   kfld                    TBEke1                          -chiave uno
068500060830      * - archivio TNTBE02L
068600060830     c     K04TBE02      klist
068700060830     c                   kfld                    TBEcod                          -tabella
068800060830     c                   kfld                    TBElin                          -lingua
068900060830     c                   kfld                    TBEsif                          -s.informativo
069000060830     c                   kfld                    TBEke1                          -chiave uno
069100170201      * - archivio FNMSA02L
069200170201     c     K04MSA02      klist
069300060830     c                   kfld                    MSAapl                          -punto operativo
069400060830     c                   kfld                    MSAfil                          -punto operativo
069500060830     c                   kfld                    MSAdfv                          -data foglio
069600060830     c                   kfld                    MSAnpg                          -categoria foglio
069700060830
069800060830      *===============================================================*
069900060831      *  ?I N D I C A T O R I?                                        *
070000060830      *---------------------------------------------------------------*
070100061002      *  28 - Emissione messaggio di errore a video                   *
070200060830      *  30 - Ripulisce il subfile                                    *
070300060830      *  31 - NON emette il subfile                                   *
070400060929      *  32 - Record modificato nel subfile (sflnxtchg)               *
070500060929      *  33 - Attiva il subfile-end                                   *
070600060929      *  40 - Evidenzia dati nel sfl        (totali)                  *
070700060830      *  41 - Evidenzia % scostamento KPI   (V2CTPS)                  *
070800060830      *  42 - Evidenzia % scostamento KPI   (V2CRPS)                  *
070900060830      *  43 - Evidenzia % scostamento KPI   (V2CNPS)                  *
071000060830      *===============================================================*
071100060830
071200060830     c     *Entry        plist
071300060830     c                   parm                    KPJBA
071400060830     c                   parm                    FIMS00ds
071500060830      * Operazioni Iniziali
071600060929     c                   exsr      RoutInz
071700060830      *
071800060830      * Inizializzazione SubFile
071900060929     c                   exsr      InzS01
072000060830      *
072100060830      * Gestione video
072200060830do  1c                   DOU       $Fine     = *on
072300060830      * - SubFile vuoto
072400060929cas 2c     $Video        caseq     '1'           GesV01
072500060830      * - SubFile con dati
072600060929     c     $Video        caseq     '2'           GesS01
072700060929      * - Videata di dettaglio   (opzione 5)
072800060929     c     $Video        caseq     '3'           GesD01
072900060929      * - Videate di statistiche (opzione 7)
073000060929     c     $Video        caseq     '4'           GesD0234
073100060929     c     $Video        caseq     '5'           GesD0234
073200060929     c     $Video        caseq     '6'           GesD0234
073300060830e   1c                   endcs
073400060830e   1c                   ENDDO
073500060830      *
073600060830      * Fine
073700060830     c                   eval      *inLR     = *on
073800060830
073900060830      *---------------------------------------------------------------*
074000060831      *?Operazioni Iniziali                                          ?*
074100060830      *---------------------------------------------------------------*
074200060929     c     RoutInz       BEGSR
074300060830      *
074400060830      * Reperimento dati job
074500060830     c                   exsr      DatiJob
074600060830      *
074700060830      * Reperimento data corrente
074800060830     c                   time                    n14                            *Ora (6) + Data (8)
074900060830     c                   move      n14           n8                             *Data corrente g/m/a
075000060830     c                   clear                   WLBdat
075100060830     c                   eval      G08dat    = n8
075200060830     c                   call      'XSRDA8'
075300060830     c                   parm                    WLBdat
075400060830     c                   eval      DATcor    = G08inv                           *Data corrente a/m/g
075500060830      *
075600060830      * Calcola il giorno precedente utile alla data foglio richiesta
075700060830     c                   reset                   WLBdat
075800060830     c                   eval      G08inv    = D00dfv
075900060830     c                   call      'XSRDA8'
076000060830     c                   parm                    WLBdat
076100060830     c                   eval      D00dfvGG = G08tgi
076200060830      * - Ciclo per trovare il primo giorno precedente nel quale il
076300060830      *   macchinone ha lavorato
076400060831     c                   eval      xx        = 1
076500060831do  1c                   DOW       xx       <= 10
076600060831     c                   eval      GioTgi    = D00dfvGG - xx                    *gg data - "x" gg
076700060830     c                   clear                   GioDat
076800060830     c                   clear                   GioInv
076900060830     c                   call      'XSRGI8'
077000060830     c                   parm                    WGIda8
077100060830     c                   eval      D00dfvPR  = GioInv                           *data gg precedente
077200060830      * - Lettura archivio
077300060830     c                   eval      MSAapl    = D00apl
077400060830     c                   eval      MSAfil    = D00fil
077500060901     c                   eval      MSAdfv    = D00dfvPR
077600060830     c                   eval      MSAnpg    = D00npg
077700170201     c     K04MSA02      setll     FNMSA000
077800170201if  2c                   if        %equal(FNMSA02L)                             *trovati dati
077900060901     c                   leave                                                  *esce dal ciclo
078000060830e   2c                   endif
078100060901     c                   eval      xx        = xx + 1                           *incremento gg meno
078200060830e   1c                   ENDDO
078300060830      *
078400060830      * Caricamento tabelle occorrenti
078500060830     c                   exsr      CarTab
078600060830      *
078700060830      * Imposta la testata
078800061002     c                   exsr      ImpT01
078900060830      *
079000060830      * Memorizzazione dei dati del file in schiere
079100060830     c                   exsr      sr_MemoSch
079200060830      *
079300060830     c                   ENDSR
079400060830
079500060830      *---------------------------------------------------------------*
079600060831      *?Reperimento dati del job (Utente/Operativi)                  ?*
079700060830      *---------------------------------------------------------------*
079800060830     c     DatiJob       BEGSR
079900060830      *
080000060830     c     *dtaara       define    §azute        azuteds
080100060830     c     *dtaara       define    §datiute      ddatiute
080200060830      *
080300060830     c                   in(E)     *dtaara
080400060830     c                   IF        %Error or RSUT = *blanks
080500060929     c                   clear                   Tibs34Ds
080600060830     c                   call      'TIBS34R'
080700060830     c                   parm                    Tibs34Ds
080800060830     c                   in        *dtaara
080900060830     c                   ENDIF
081000060830      *
081100060830     c                   ENDSR
081200060830
081300060830      *---------------------------------------------------------------*
081400060831      *?Caricamento tabelle occorrenti                               ?*
081500060830      *---------------------------------------------------------------*
081600060830     c     CarTab        BEGSR
081700060830      *
081800060830      * Layout macchinone smistacolli
081900060830     c                   clear                   TBEcod                         *tabella
082000060929     c                   eval      TBEke1    = *zeros                           *chiave uno
082100060830     c                   move      'MSL'         TBEke1
082200060830     c                   clear                   TBEke2                         *chiave due
082300060830     c                   clear                   TBElin                         *lingua
082400060830     c                   clear                   TBEsif                         *s.i. di GRUPPO
082500060830     c     K05TBE01      chain     TNTBE000
082600060830if  1c                   if        NOT %found(TNTBE01L)
082700060830     c                   eval      TBEsif    = KNSIF                            *s.i. di AZIENDA
082800060830     c     K05TBE01      chain     TNTBE000
082900060830e   1c                   endif
083000060830      *
083100060830     c*** già così:      clear                   $msl
083200060830     c*** già così:      clear                   $Nmsl
083300170802     c*** già così:      clear                   $pke
083400060831     c                   clear                   xx
083500060830     c                   eval      TBEcod    = 'MSL'                            *tabella
083600060830     c                   clear                   TBElin                         *lingua
083700060830     c                   movel(p)  D00fil        TBEke1                         *chiave uno
083800060901     c     K04TBE02      setll     TNTBE02
083900060901     c     K04TBE02      reade     TNTBE02
084000060830do  1c                   DOW       NOT  %eof(TNTBE02L)
084100060831     c                             and  xx   < 20
084200060904     c                   movel     TBEuni        dMSL
084300161013if  2c                   if              TBEatb  = *blanks
084400161025     c                             and ((D00apl  = §MSLap5  and
084500161025     c                                   D00npg  = 5)     or
084600161025     c                                  (D00apl  = §MSLapl  and
084700161205     c                                   D00npg  = 2))
084800140307if  3c                   if        §MSLent < 20
084900060831     c                   add       1             xx
085000140307     c*//                eval      $msl(xx)  = §MSLpep                          *entrance point
085100140307     c*//                eval      $Nmsl(xx) = §MSLent                          *n° entrance point
085200140307     c                   eval      $msl(§MSLent)  = §MSLpep                     *entrance point
085300140307     c                   eval      $Nmsl(§MSLent) = §MSLent                     *n° entrance point
085400170802     c                   eval      $pke(§MSLent)  = §MSLpke                     *S=entrata picking
085500140307e   3c                   endif
085600060830e   2c                   endif
085700060901     c     K04TBE02      reade     TNTBE02
085800060830e   1c                   ENDDO
085900060830      *
086000060830      * Obiettivi di Produttività di un Impianto Automatico Smistacolli
086100060830     c                   clear                   dKPI
086200060830     c                   reset                   TIBS02ds
086300060830     c                   eval      T02sif    = KNSIF
086400060830     c                   eval      T02cod    = 'KPI'
086500060830     c                   movel(p)  D00npg        T02ke1
086600060830     c                   movel(p)  D00apl        T02ke2
086700060830     c                   call      'TIBS02R'
086800060830     c                   parm                    KPJBA
086900060830     c                   parm                    TIBS02ds
087000060830     c                   if        T02err    = *blanks
087100060830     c                   movel     T02uni        dKPI
087200060830     c                   endif
087300060830      *
087400060830     c                   ENDSR
087500060830
087600060830      *---------------------------------------------------------------*
087700060831      *?Impostazione video testata                                   ?*
087800060830      *---------------------------------------------------------------*
087900061002     c     ImpT01        BEGSR
088000061002      *
088100061002      * Nome programma
088200061002     c                   eval      V1Tpgm    = SDSpgm
088300060830      *
088400060830      * P.O.
088500060830     c                   eval      V0Cfil    = D00fil
088600060830     c     D00fil        chain     AZORG
088700060830if  1c                   if        %found(AZORG01L)
088800060830     c                   movel     ORGdes        V0Dfil
088900060830e   1c                   endif
089000060830      * Data F.V.
089100060830     c                   reset                   WLBdat
089200060830     c                   eval      G08inv    = D00dfv
089300060830     c                   call      'XSRDA8'
089400060901     c                   parm                    WLBdat
089500060830     c                   eval      V0Cdfv    = G08dat
089600060830      * Categoria F.V.
089700060830     c                   eval      V0Cnpg    = D00npg
089800060830if  1c                   if        D00npg    = 2
089900060830     c                   movel     $Dvid(1)      V0Dnpg
090000060830x   1c                   else
090100060830     c                   movel     $Dvid(2)      V0Dnpg
090200060830e   1c                   endif
090300060830      *
090400060830      * Tipo applicazione
090500060830     c                   eval      V0Capl    = D00apl
090600060830     c                   clear                   dMTP
090700060830     c                   reset                   TIBS02ds
090800060830     c                   eval      T02sif    = KNSIF
090900060830     c                   eval      T02cod    = 'MTP'
091000060830     c                   movel(p)  V0Capl        T02ke1
091100060830     c                   call      'TIBS02R'
091200060830     c                   parm                    KPJBA
091300060830     c                   parm                    TIBS02ds
091400060830     c                   if        T02err    = *blanks
091500060830     c                   movel     T02uni        dMTP
091600060830     c                   endif
091700060830     c                   movel     §MTPdes       V0Dapl
091800060830      *
091900060830     c                   ENDSR
092000060830
092100060830      *---------------------------------------------------------------*
092200060831      *?Inizializzazione subfile                                     ?*
092300060830      *---------------------------------------------------------------*
092400060929     c     InzS01        BEGSR
092500060830      *
092600060830      * Pulizia subfile
092700060830     c                   seton                                        3031
092800060929     c                   write     MS08C01
092900060830     c                   setoff                                       31
093000060830      *
093100060929      * Reimposta variabili di lavoro
093200061002     c                   eval      *in28   = *off
093300060929     c                   eval      *in40   = *off
093400060830     c                   eval      $Video  = '1'
093500060929     c                   clear                   S01nrr
093600060830      *
093700170201      * SE reperiti dati in FNMSA00F:
093800060830if  1c                   IF        $FNMSA = *on
093900060830      *
094000060830      * - Impostazione di alcuni valori statistici calcolati da
094100060830      *   emettere con il sflctl
094200060929     c                   exsr      RieC01
094300060830      * - Caricamento del sfl
094400060830do  2c                   do        MEMxv         xv
094500060929     c                   exsr      WrtS01
094600060830e   2c                   enddo
094700060929     c                   eval      LASTnrr = S01nrr
094800060830      *
094900060830e   1c                   ENDIF
095000080909      /free
095100080909       // Eventuale visualizzazione del messaggio
095200080909             select;
095300080909               // - Errore nel caricamento dei dati (troppi)
095400080909               when  $TooManyRec = *on;
095500080909                 *in28  = *on;
095600080909                 V1Dmsg = 'ERRORE (leggi messaggio sovrastante).   +
095700080909                           Contattare il CED.';
095800080909                 V1txt1 = 'IMPOSSIBILE visualizzare i dati di tutte le +
095900080909                           letture';
096000080909                 V1txt2 = 'dalle ore ' + %editw( $orl(1) : '0 :  ' )
096100080909                        + ' del giorno '
096200080909                        + %subst( %editc( $dtl(1):'X' ) : 7 : 2 )
096300080909                        + '/'
096400080909                        + %subst( %editc( $dtl(1):'X' ) : 5 : 2 )
096500080909                        + '/'
096600080909                        + %subst( %editc( $dtl(1):'X' ) : 1 : 4 );
096700080909               // - Nessun dato caricato
096800080909               when  $FNMSA = *off;
096900080909                 V1txt1 = 'Non ci sono dati da visualizzare';
097000080909                 clear  V1txt2;
097100080909             endsl;
097200080909      /end-free
097300060830      *
097400060929      * Impostazione emissione sfile
097500060929if  1c                   if        S01nrr  > *zeros
097600060830     c                   eval      $Video  = '2'
097700060929     c                   eval      *in30   = *off                               *SflDsp
097800060929     c                   eval      *in33   = *on                                *SflEnd
097900060830e   1c                   endif
098000060830      *
098100060830     c                   ENDSR
098200060830
098300060830      *---------------------------------------------------------------*
098400060831      *?Impostazione campi nel subfile control                       ?*
098500060830      *---------------------------------------------------------------*
098600060929     c     RieC01        BEGSR
098700060830      *
098800060830      * Progressivi ultimo turno:
098900060830      * - Colli
099000060830     c                   eval      V2Ctco  = MEMtco
099100060830      * - Tempo lavorato
099200060830     c                   eval      V2Ctlm  = MEMtlm
099300060830      * - KPI effettivo
099400060830     c                   eval      V2Ctcl  = MEMtcl
099500060830      * - KPI obiettivo
099600060830     c                   eval      V2Ctkp  = MEMtkp
099700060830      * - % scostamento KPI
099800060830     c                   if        MEMtkp <> *zeros
099900070510     c                   eval(h)   w0091   = ((MEMtcl * 100) / MEMtkp)
100000060830     c                                     - 100
100100060830     c                   else
100200070510     c                   clear                   w0091
100300060830     c                   endif
100400070510if  1c                   if        w0091   > 999,9
100500070510     c                   eval      V2Ctps  = 999,9
100600070510x   1c                   else
100700070510     c                   eval      V2Ctps  = w0091
100800070510e   1c                   endif
100900060830     c                   eval      *in41   = (V2Ctps < (§KPIpsc*(-1)))
101000060830      * - Tempo trascorso
101100060830     c                   eval      V2Cttm  = MEMttm
101200060830      *
101300060830      * Progressivi ultimi xx minuti
101400060830      * - Minuti ultimo riepilogo
101500060830     c                   eval      V2Cmtr  = D00mtr
101600060830      * - Colli
101700060830     c                   eval      V2Crco  = MEMrco
101800060830      * - Tempo lavorato
101900060830     c                   eval      V2Crlm  = MEMrlm
102000060830      * - KPI effettivo
102100060830     c                   eval      V2Crcl  = MEMrcl
102200060830      * - KPI obiettivo
102300060830     c                   eval      V2Crkp  = MEMrkp
102400060830      * - % scostamento KPI
102500060830     c                   if        MEMrkp <> *zeros
102600060830     c                   eval      V2Crps = ((MEMrcl * 100) / MEMrkp)
102700060830     c                                    - 100
102800060830     c                   else
102900060830     c                   clear                   V2Crps
103000060830     c                   endif
103100060830     c                   eval      *in42  = (V2Crps < (§KPIpsc*(-1)))
103200060830      * - Tempo trascorso
103300060830     c                   eval      V2Crtm  = MEMrtm
103400060829      *
103500060830     c                   ENDSR
103600060830
103700060830      *---------------------------------------------------------------*
103800060929      *?Scrittura singolo record nel sfl S01                         ?*
103900060830      *---------------------------------------------------------------*
104000060929     c     WrtS01        BEGSR
104100060830      *
104200060929     c                   clear                   MS08S01
104300060830      *
104400060830      * Campi hidden
104500060830     c                   eval      V2Hv    = xv                                 *indice video HIDDEN
104600060830     c                   eval      V2Hdtl  = $Vdtl(xv)
104700060830      *
104800060830      * Campi di i/o
104900060830     c*** già così       clear                   V2Sopz
105000060830     c                   eval      V2Sdes  = $Vdes(xv)
105100060901     c                   eval      V2Sorl  = $Vorl(xv)
105200060830     c                   eval      V2Spnc  = Vpnc(xv)
105300060901     c                   eval      V2Spsp  = Vpsp(xv)
105400060830      *
105500061002      * - Numero colli/minuto
105600060830if  1c                   if        $VminLav(xv)  > *zeros
105700061002     c                   eval(h)   V2Sncl  =  $Vncl(xv) / $VminLav(xv)
105800060830e   1c                   endif
105900060830      *
106000061002      * - Colli/Minuto caricati per entrata
106100061024      *   La ripartizione dei colli/minuto per entrata nel rec. di
106200061024      *   totale RIE/GEN funzione meglio se gestita come nella subr.
106300061024      *   GesD01...  Quindi DIFFERENZIO:
106400061024if  0c                   IF           ($Vtip(xv) <> 'R'                         *Riep
106500061024     c                             and $Vtip(xv) <> 'G')                        *Gen
106600061024      *
106700060904     c*** già così:      clear                   $V2SM
106800060830     c                   clear                   xx
106900060830do  1c                   DOW       xx         <  %elem($VMLAVxx_ds)
107000060831     c                   eval      xx         =  xx + 1
107100060830     c     xx            occur     $VMLAVxx_ds
107200060830if  2c                   if        $VMLAVxx(xv)  > *zeros
107300060904     c     xx            occur     $VPExx_ds
107400061002     c                   eval(h)   $V2SM(xx)  =  $VPExx(xv) / $VMLAVxx(xv)
107500060830e   2c                   endif
107600060830e   1c                   ENDDO
107700060904     c*** già compresa:  movea     $V2SM         $V2SM_ds
107800061002      *
107900061024x   0c                   ELSE
108000061024      *
108100061024     c*** già così:      clear                   $V2SM
108200061024      * - - Colli caricati per entrata
108300061024     c                   clear                   $V3DE
108400061024     c                   clear                   xx
108500061024do  2c                   DOW       xx         <  %elem($VPExx_ds)
108600061024     c                   eval      xx         =  xx + 1
108700061024     c     xx            occur     $VMLAVxx_ds
108800061024if  3c                   if        $VMLAVxx(xv)  > *zeros
108900061024     c     xx            occur     $VPExx_ds
109000061024     c                   eval      $V3DE(xx)  =  $VPExx(xv)
109100061024e   3c                   endif
109200061024e   2c                   ENDDO
109300061024      * - - Ripartizione dei colli della post.20 tra gli altri vari
109400061024      *     banchi SE riga di riepilogo o totale
109500061024      *    ?N.B.?- Il totale dei colli (in base a cui ripartire quelli
109600061024      *     della post.20 su tutte le altre post.) va calcolato ora,
109700061024      *     PRIMA di modificare la schiera $V3de con la subroutine
109800061024      *     "RipartB20det" (cioè PRIMA di iniziarne la ripartizione).
109900061024     c                   eval      wRipColli = %xfoot($V3de) - $V3de(20)
110000061024if  2c                   if        wRipColli >  *zeros
110100061024     c                   exsr      RipartB20det
110200061024e   2c                   endif
110300061024      * - - Media entrate in base ai minuti lavorati per entrata
110400061024     c                   clear                   $V3DM
110500061024     c                   clear                   xx
110600061024do  2c                   DOW       xx         <  %elem($VMLAVxx_ds)
110700061024     c                   eval      xx         =  xx + 1
110800061024     c     xx            occur     $VMLAVxx_ds
110900061024if  3c                   if        $VMLAVxx(xv)  > *zeros
111000061024      * - - Utilizzo il numero colli per postazione già "allineato" dalla
111100061024      *     subr. "RipartB20det"
111200061024      *     (i dati "originali" hanno ancora i colli nella post.20)
111300061205     c**!!!              eval(h)   $V3DM(xx)  =  $V3DE(xx) / $VMLAVxx(xv)
111400061205     c                   eval(h)   w0050      =  $V3DE(xx) / $VMLAVxx(xv)
111500061205     c                   if        w0050 > 99
111600061205     c                   eval      $v3dm(xx) = 99
111700061205     c                   else
111800061205     c                   eval      $v3dm(xx) = w0050
111900061205     c                   endif
112000061205
112100061024e   3c                   endif
112200061024e   2c                   ENDDO
112300061024      *
112400061024     c                   z-add     $V3DM         $V2SM
112500061024      *
112600061024e   0c                   ENDIF
112700060830      *
112800060830      * Impostazione indicatori a video
112900060830      * - Riga di riepilogo in alta intensità
113000061002     c                   eval      *in40   =   ($Vtip(xv) = 'R'
113100061002     c                                       or $Vtip(xv) = 'G')
113200060830      *
113300060830      * Scrittura record sfl
113400060929     c                   add       1             S01nrr
113500061002     c                   eval      C01rcd  = S01nrr
113600060929     c                   WRITE     MS08S01
113700060830      *
113800060830     c                   ENDSR
113900061024
114000061024      *---------------------------------------------------------------*
114100061024      *?Ripartizione entrate della post.20 in tutte le altre postaz. ?*
114200061024      *---------------------------------------------------------------*
114300061024     c     RipartB20det  BEGSR
114400061024      *
114500061024      * Calcolo Numero Totale Colli per postazione/periodo
114600061024     c                   eval      wTotColli  =  %xfoot($V3de)
114700061024      *
114800061024      * Calcolo proporzione con cui ripartire i colli della post.20
114900061024      *   su tutte le altre postazioni utilizzate
115000061024     c                   DO        19            xw
115100061024      *
115200061024      * - produttività relativa del banco di ingresso
115300061024     c                   eval(h)   W_prod_rel =  $V3de(xw) / wRipColli
115400061024      *
115500061024      * - num.colli rilevati inaspettatamente (ed usciti correttamente)
115600061024      *   da attribuire al banco di ingresso in esame
115700061024     c                   eval(h)   W_loaded_B =  $V3de(20) * W_prod_rel
115800061024      *
115900061024      * - ricalcolo del numero colli caricati su ciascun banco
116000071123     c                   eval      $V3de(xw)  =  $V3de(xw) + W_loaded_B
116100061024      *
116200061024     c                   ENDDO
116300061024      *
116400061024     c                   clear                   $V3DE(20)
116500061024      *
116600061024      * Controllo non risulti un totale colli "ripartito" superiore
116700061024      *   al totale colli originale (per gli arrotondamenti...)
116800061024      *   nel qual caso: tolgo i colli in eccesso dalla 1ª postazione
116900061024      *   con il più alto numero di colli
117000061024if  1c                   IF        wTotColli  <> %xfoot($V3de)
117100061024      *
117200061024      * - ricerca della postazione con il numero maggiore di colli
117300061024     c                   clear                   wRipColli
117400061024     c                   clear                   xw
117500061024do  2c                   Dow       xw         <  %elem($V3de)
117600061024     c                   add       1             xw
117700061024if  3c                   if             wRipColli  =  *zeros
117800061024     c                             or  (wRipColli  >  *zeros
117900061024     c                              and $V3de(xw)  >  $V3de(wRipColli))
118000061024     c                   eval      wRipColli  =  xw
118100061024e   3c                   endif
118200061024e   2c                   EndDo
118300061024      * - sistemazione numero totale colli nella post. con il numero
118400061024      *   maggiore di colli
118500061024     c                   eval      xw         =  wRipColli
118600061024     c                   eval      wRipColli  =  wTotColli
118700061024     c                                        -  %xfoot($V3de)
118800061024     c                   eval      $V3de(xw)  =  $V3de(xw)
118900061024     c                                        +  wRipColli
119000061024      *
119100061024e   1c                   ENDIF
119200061024      *
119300061024     c                   ENDSR
119400060830
119500060830      *---------------------------------------------------------------*
119600060831      *?Gestione prima videta                                        ?*
119700060830      *---------------------------------------------------------------*
119800060929     c     GesV01        BEGSR
119900060830      *
120000060830     c                   eval      *in30 = *on
120100060830      *
120200060830      * Emissione testata & piede
120300061002     c                   write     MS08T01                                      *testata
120400061002     c                   write     MS08T02
120500061002     c                   write     MS08Z02                                      *funzioni
120600060830      * Emissione sfl vuoto
120700060929     c                   write     MS08V01
120800060929     c                   exfmt     MS08C01
120900060830      *
121000060830sel 1c                   select
121100060831      * F3 = Fine programma
121200060830w   1c                   when      *inKC
121300060830     c                   exsr      F03ges
121400060830      * F12 = Ritorno
121500060830w   1c                   when      *inKL
121600060830     c                   exsr      F12ges
121700060830      *
121800060830e   1c                   endsl
121900060830      *
122000060830     c                   ENDSR
122100060830
122200060830      *---------------------------------------------------------------*
122300060831      *?Tasto funzionale F03 -> Fine programma                       ?*
122400060830      *---------------------------------------------------------------*
122500060830     c     F03ges        BEGSR
122600060830      *
122700060830     c                   eval      $Fine  = *on
122800060830     c                   eval      D00F03 = *on
122900060830      *
123000060830     c                   ENDSR
123100060830
123200060830      *---------------------------------------------------------------*
123300060831      *?Tasto funzionale F12 -> Ritorno                              ?*
123400060830      *---------------------------------------------------------------*
123500060830     c     F12ges        BEGSR
123600060830      *
123700060830     c                   eval      $Fine  = *on
123800060830     c                   eval      D00F12 = *on
123900060830      *
124000060830     c                   ENDSR
124100060830
124200060830      *---------------------------------------------------------------*
124300060831      *?Gestione seconda videata (subfile)                           ?*
124400060830      *---------------------------------------------------------------*
124500060929     c     GesS01        BEGSR
124600060830      *
124700060830      * Emissione testata
124800061002     c                   write     MS08T01
124900061002     c                   write     MS08T02
125000060830      * Emissione piede con tasti funzionali
125100061002     c                   write     MS08Z01
125200060830      * Emissione subfile
125300060929     c                   exfmt     MS08C01
125400080909     c                   clear                   V1Dmsg
125500061002     c                   eval      *in28 = *off
125600060929     c                   eval      *in40 = *off                                 *caratteri video
125700060829      *
125800060829sel 1c                   select
125900060831      * F3 = Fine programma
126000060829w   1c                   when      *inkc
126100060829     c                   exsr      F03ges
126200060831      * F5 = Ricarica
126300060829w   1c                   when      *inke
126400060829     c                   exsr      F05ges
126500060829      * F12 = Ritorno
126600060829w   1c                   when      *inkl
126700060829     c                   exsr      F12ges
126800060829      * F17 = Inizio dati
126900060829w   1c                   when      *inkr
127000060829     c                   exsr      F17ges
127100060829      * F18 = Fine dati
127200060829w   1c                   when      *inks
127300060829     c                   exsr      F18ges
127400060829      * F21 = Stampa dati
127500060829w   1c                   when      *inkv
127600060829     c                   exsr      F21ges
127700060829      * F22 = Coda di stampa
127800060829w   1c                   when      *inkw
127900060829     c                   exsr      F22ges
128000060830      * Enter = Gestione opzioni
128100060829x   1c                   other
128200061002     c                   exsr      OpzS01
128300061002if  2c*** superfluo:     if        *in28
128400060830     c*** superfluo:     leavesr
128500060830e   2c*** superfluo:     endif
128600060829e   1c                   endsl
128700060829      *
128800060830     c                   ENDSR
128900060830
129000060830      *---------------------------------------------------------------*
129100061002      *?Tasto funzionale F05 -> Ricarica sfl                         ?*
129200060830      *---------------------------------------------------------------*
129300060830     c     F05ges        BEGSR
129400060830      *
129500060830      * Memorizzazione dei dati del file in schiere
129600060830     c                   exsr      sr_MemoSch
129700060830      *
129800060830      * Inizializzazione sfl
129900060929     c                   exsr      InzS01
130000060830      *
130100060830     c                   ENDSR
130200060830
130300060830      *---------------------------------------------------------------*
130400060831      *?Tasto funzionale F17 -> Inizio dati                          ?*
130500060830      *---------------------------------------------------------------*
130600060830     c     F17ges        BEGSR
130700060830      *
130800060929     c                   eval      S01nrr  = 1
130900061002     c                   eval      C01rcd  = 1                                  *posizionamento
131000060929     c     S01nrr        chain     MS08S01
131100060830      *
131200060830     c                   ENDSR
131300060830
131400060830      *---------------------------------------------------------------*
131500060831      *?Tasto funzionale F18 -> Fine dati                            ?*
131600060830      *---------------------------------------------------------------*
131700060830     c     F18ges        BEGSR
131800060830      *
131900060929     c                   eval      S01nrr  = LASTnrr
132000061002     c                   eval      C01rcd  = LASTnrr
132100060929     c     S01nrr        chain     MS08S01
132200060830      *
132300060830     c                   ENDSR
132400060830
132500060830      *---------------------------------------------------------------*
132600060831      *?Tasto funzionale F21 -> Stampa dati                          ?*
132700060830      *---------------------------------------------------------------*
132800060830     c     F21ges        BEGSR
132900060830      *
133000060830      * Apertura file di stampa
133100060929     c                   open      FIMS08P
133200060830      *
133300060830      * PARAMETRI DI LANCIO  - - - - - - - - - - - - - - - - - - - - -*
133400060830      *
133500060830      * Impostazione dei campi per i parametri di lancio
133600060830      * - P.O.
133700060830     c                   eval      PACfil  = V0Cfil
133800060830     c                   eval      PADfil  = V0Dfil
133900060830      * - Tipo applicazione
134000060830     c                   eval      PACapl  = V0Capl
134100060830     c                   eval      PADapl  = §MTPdes
134200060830      * - Data foglio
134300060830     c                   eval      PACdfv  = V0Cdfv
134400060830      * - Categoria foglio
134500060830     c                   eval      PACnpg  = V0Cnpg
134600060830     c                   eval      PADnpg  = V0Dnpg
134700060830      * - Minuti di dettaglio
134800060830     c                   eval      PACmtd  = D00mtd
134900060830      * - Minuti di riepilogo
135000060830     c                   eval      PACmtr  = D00mtr
135100060830      * - Orario di lettura: dalle / alle
135200060830     c                   eval      PAColi  = D00oli
135300060830     c                   eval      PAColf  = D00olf
135400060830      *
135500060830      * Stampa della testata
135600060929     c                   write     MS08PT
135700060830      * Stampa dei parametri di lancio
135800060929     c                   write     MS08PA
135900060830      * Stampa nro entrate e descrizione, caricate in tabella MSL
136000060830     c                   eval      xw      = 1
136100060830     c                   DOW            xw        <= 20
136200060830     c                             and  $Nmsl(xw) <> *zeros
136300060830     c                   clear                   ENdent
136400060830     c                   eval      ENcent  = $Nmsl(xw)
136500060830     c                   eval      ENdent  = $msl(xw)
136600060929     c                   write     MS08EN
136700060830     c                   eval      xw      = xw + 1
136800060830     c                   ENDDO
136900060830     c
137000060830      *
137100060830      * PROSPETTO ENTRATE  - - - - - - - - - - - - - - - - - - - - - -*
137200060830      *
137300060830      * RIstampa testata (in pagina successiva)
137400060929     c                   write     MS08PT
137500060830      * Stampa dati statistici
137600060830     c                   eval      STCtco  = V2Ctco
137700060830     c                   eval      STCtlm  = V2Ctlm
137800060830     c                   eval      STCtcl  = V2Ctcl
137900060830     c                   eval      STCttm  = V2Cttm
138000060830     c                   eval      STCmtr  = V2Cmtr
138100060830     c                   eval      STCrco  = V2Crco
138200060830     c                   eval      STCrlm  = V2Crlm
138300060830     c                   eval      STCrcl  = V2Crcl
138400060830     c                   eval      STCrtm  = V2Crtm
138500061002      *
138600060830     c                   eval      STCtkp  = V2Ctkp
138700060830     c                   eval      STCtps  = V2Ctps
138800060830     c                   eval      STCrkp  = V2Crkp
138900060830     c                   eval      STCrps  = V2Crps
139000061002      *
139100060929     c                   write     MS08ST                                       *dati statistici
139200060830      *
139300060830      * Stampa intestazione
139400060929     c                   write     MS08PI
139500060830     c                   eval      *in01   = *off
139600060830      *
139700060830      * Ciclo di lettura e stampa dei dati del sfile
139800060929do  1c                   DO        LASTnrr       S01nrr
139900060830      *
140000060929     c     S01nrr        chain     MS08S01
140100060830      *
140200060830      * - impostazione dei campi di dettaglio
140300060830     c                   eval      DECdes  = V2Sdes
140400060830     c                   eval      DECorl  = V2Sorl
140500060830     c                   eval      DECpnc  = V2Spnc
140600060830     c                   eval      DECpsp  = V2Spsp
140700060830     c                   eval      DECncl  = V2Sncl
140800060830      *
140900061005     c                   z-add     $V2SM         $DECm
141000060830      *
141100060904     c*** già così:      clear                   $DECe
141200060830     c                   clear                   xx
141300060830do  2c                   DOW       xx         <  %elem($VPExx_ds)
141400060831     c                   eval      xx         =  xx + 1
141500061005     c     xx            occur     $VPExx_ds
141600061005     c                   eval      $DECe(xx)  =  $VPExx(V2Hv)
141700060830e   2c                   ENDDO
141800060904     c*** già compresa:  movea     $DECe         $DECe_ds
141900060830      *
142000060830      * Test OverFlow
142100060830if  2c                   if        *in01
142200060929     c                   write     MS08RI                                       *riga chiusura
142300060929     c                   write     MS08PT                                       *testata
142400060929     c                   write     MS08PI                                       *intestazione
142500060830     c                   eval      *in01      =  *off
142600060830e   2c                   endif
142700060830      *
142800060830      * Stampa riga di dettaglio
142900060929     c                   write     MS08DE
143000060830      *
143100060830e   1c                   ENDDO
143200060830      *
143300060830      * Stampa riga di chiusura
143400060929     c                   write     MS08RI
143500060830      *
143600060830      * PROSPETTO USCITE (no x tipo PICKING) - - - - - - - - - - - - -*
143700060830      *
143800060830if  1c                   if        §MTPtds   <> 'K'
143900060830      *
144000060830      * Stampa testata
144100060929     c                   write     MS08PT                                       *testata
144200060830      *
144300060830      * Ciclo di lettura e stampa dei dati del sfile
144400060929do  2c                   DO        LASTnrr       S01nrr
144500060830      *
144600060929     c     S01nrr        chain     MS08S01
144700060830      *
144800060830      * Elaborazione  delle sole righe RIEPILOGO e GENERALE
144900060830if  3c                   IF             $Vtip(V2Hv) = 'R'
145000060830     c                             or   $Vtip(V2Hv) = 'G'
145100060830      *
145200060830      * - impostazione dei campi di dettaglio
145300060830     c                   eval      D2Cdes  = V2Sdes
145400060901     c                   eval      D2Corl  = V2Sorl
145500060831     c     V2Hv          occur     $VPAxx_ds
145600060831     c                   eval      D2Cu01  = $VPAxx( 1)
145700060831     c                   eval      D2Cu02  = $VPAxx( 2)
145800060831     c                   eval      D2Cu03  = $VPAxx( 3)
145900060831     c                   eval      D2Cu04  = $VPAxx( 4)
146000060831     c                   eval      D2Cu05  = $VPAxx( 5)
146100060831     c                   eval      D2Cu06  = $VPAxx( 6)
146200060831     c                   eval      D2Cu07  = $VPAxx( 7)
146300060831     c                   eval      D2Cu08  = $VPAxx( 8)
146400060831     c                   eval      D2Cu09  = $VPAxx( 9)
146500060831     c                   eval      D2Cu10  = $VPAxx(10)
146600060831     c                   eval      D2Cu11  = $VPAxx(11)
146700060831     c                   eval      D2Cu12  = $VPAxx(12)
146800060831     c                   eval      D2Cu13  = $VPAxx(13)
146900060831     c                   eval      D2Cu14  = $VPAxx(14)
147000060831     c                   eval      D2Cu15  = $VPAxx(15)
147100060831     c                   eval      D2Cu16  = $VPAxx(16)
147200060831     c                   eval      D2Cu17  = $VPAxx(17)
147300060831     c                   eval      D2Cu18  = $VPAxx(18)
147400060831     c                   eval      D2Cu19  = $VPAxx(19)
147500060831     c                   eval      D2Cu20  = $VPAxx(20)
147600060831     c                   eval      D2Cu21  = $VPAxx(21)
147700060831     c                   eval      D2Cu22  = $VPAxx(22)
147800060831     c                   eval      D2Cu23  = $VPAxx(23)
147900060831     c                   eval      D2Cu24  = $VPAxx(24)
148000060831     c                   eval      D2Cu25  = $VPAxx(25)
148100060831     c                   eval      D2Cu26  = $VPAxx(26)
148200060831     c                   eval      D2Cu27  = $VPAxx(27)
148300060831     c                   eval      D2Cu28  = $VPAxx(28)
148400060831     c                   eval      D2Cu29  = $VPAxx(29)
148500060831     c                   eval      D2Cu30  = $VPAxx(30)
148600060831     c                   eval      D2Cu31  = $VPAxx(31)
148700060831     c                   eval      D2Cu32  = $VPAxx(32)
148800060831     c                   eval      D2Cu33  = $VPAxx(33)
148900060831     c                   eval      D2Cu34  = $VPAxx(34)
149000060831     c                   eval      D2Cu35  = $VPAxx(35)
149100060831     c                   eval      D2Cu36  = $VPAxx(36)
149200060831     c                   eval      D2Cu37  = $VPAxx(37)
149300060831     c                   eval      D2Cu38  = $VPAxx(38)
149400060831     c                   eval      D2Cu39  = $VPAxx(39)
149500060831     c                   eval      D2Cu40  = $VPAxx(40)
149600060831     c                   eval      D2Cu41  = $VPAxx(41)
149700060831     c                   eval      D2Cu42  = $VPAxx(42)
149800060831     c                   eval      D2Cu43  = $VPAxx(43)
149900060831     c                   eval      D2Cu44  = $VPAxx(44)
150000060831     c                   eval      D2Cu45  = $VPAxx(45)
150100060831     c                   eval      D2Cu46  = $VPAxx(46)
150200060831     c                   eval      D2Cu47  = $VPAxx(47)
150300060831     c                   eval      D2Cu48  = $VPAxx(48)
150400060831     c                   eval      D2Cu49  = $VPAxx(49)
150500060831     c                   eval      D2Cu50  = $VPAxx(50)
150600060831     c                   eval      D2Cu51  = $VPAxx(51)
150700060831     c                   eval      D2Cu52  = $VPAxx(52)
150800060831     c                   eval      D2Cu53  = $VPAxx(53)
150900060831     c                   eval      D2Cu54  = $VPAxx(54)
151000060831     c                   eval      D2Cu55  = $VPAxx(55)
151100060831     c                   eval      D2Cu56  = $VPAxx(56)
151200060831     c                   eval      D2Cu57  = $VPAxx(57)
151300060831     c                   eval      D2Cu58  = $VPAxx(58)
151400060831     c                   eval      D2Cu59  = $VPAxx(59)
151500060831     c                   eval      D2Cu60  = $VPAxx(60)
151600060830      *
151700060830      * Test OverFlow
151800060830if  4c                   if        *in01
151900060929     c                   write     MS08PT
152000060830     c                   eval      *in01   = *off
152100060830e   4c                   endif
152200060830      *
152300060830      * Stampa riga di dettaglio
152400060929     c                   write     MS08D2
152500060830      *
152600060830e   3c                   ENDIF
152700060830      *
152800060830e   2c                   ENDDO
152900060830      *
153000060830      * Stampa riga di chiusura
153100060929     c                   write     MS08RI
153200060830      *
153300060830e   1c                   ENDIF
153400060830      *
153500060830      * PROSPETTO STATISTICHE  - - - - - - - - - - - - - - - - - - - -*
153600060830      *
153700060830      * Stampa testata
153800060929     c                   write     MS08PT                                       *testata
153900060830      *
154000060830      * Ciclo di lettura e stampa dei dati del sfile
154100060929do  1c                   DO        LASTnrr       S01nrr
154200060830      *
154300060929     c     S01nrr        chain     MS08S01
154400060830      *
154500060830      * Elaborazione delle sole righe RIEPILOGO e GENERALE
154600060830if  2c                   IF             $Vtip(V2Hv) = 'R'
154700060830     c                             or   $Vtip(V2Hv) = 'G'
154800060830      *
154900060830sel 3c                   SELECT
155000060830      *
155100060830      * Stampa per dettaglio standard
155200060830w   3c                   WHEN      §MTPtds = *blanks
155300060830     c                   eval      D3Cdes  = V2Sdes
155400060830     c                   eval      D3Corl  = V2Sorl
155500060830      * - Statistiche LAVORAZIONE
155600060830     c                   eval      D3Ccla  = VPesInd(V2Hv)
155700060830     c                   eval      D3Csor  = VPusSor(V2Hv)
155800060830     c                   eval      D3Cdna  = VOdsDna(V2Hv)
155900060830     c                   eval      D3Cerr  = VErrMac(V2Hv)
156000060830      * - Statistiche LETTURE
156100060830     c                   eval      D3Ccsd  = Vcsd(V2Hv)
156200060830     c                   eval      D3Cps2  = Vps2dok(V2Hv)
156300060830     c                   eval      D3Cccv  = Vccv(V2Hv)
156400060830     c                   eval      D3Cccp  = Vccp(V2Hv)
156500060830     c                   eval      D3Cnls  = Vnls(V2Hv)
156600060830     c                   eval      D3Cnlp  = Vnlp(V2Hv)
156700060830     c                   eval      D3Cnmi  = Vxnomis(V2Hv)
156800060830     c                   eval      D3Cnpe  = Vxnopes(V2Hv)
156900060830      * - Test OverFlow
157000060830if  4c                   if        *in01
157100060929     c                   write     MS08PT
157200060830     c                   eval      *in01   = *off
157300060830e   4c                   endif
157400060830      * - Stampa riga di dettaglio
157500060929     c                   write     MS08D3
157600060830      *
157700060830      * Stampa per dettaglio tipo Disk C
157800060830w   3c                   WHEN      §MTPtds = 'D'
157900060830     c                   eval      D4Cdes  = V2Sdes
158000060830     c                   eval      D4Corl  = V2Sorl
158100060830      * - Statistiche LAVORAZIONE
158200060830     c                   eval      D4Cind  = VPesInd(V2Hv)
158300060830     c                   eval      D4Csor  = VPusSor(V2Hv)
158400060830     c                   eval      D4Clos  = VPusLos(V2Hv)
158500060830     c                   eval      D4Cnes  = VPusNes(V2Hv)
158600060830      * - Statistiche LETTURA
158700060830     c                   eval      D4Cbar  = Vbar(V2Hv)
158800060830     c                   eval      D4Cchi  = Vchi(V2Hv)
158900060830      * - Test OverFlow
159000060830if  4c                   if        *in01
159100060929     c                   write     MS08PT                                       *testata
159200060830     c                   eval      *in01   = *off
159300060830e   4c                   endif
159400060830      * - Stampa riga di dettaglio
159500060929     c                   write     MS08D4
159600060830      *
159700060830      * Stampa per dettaglio tipo PICKING
159800060830w   3c                   WHEN      §MTPtds = 'K'
159900060830     c                   eval      D5Cdes  = V2Sdes
160000060830     c                   eval      D5Corl  = V2Sorl
160100060830      * - Statistiche LAVORAZIONE
160200060830     c                   eval      D5Cind  = VPesInd(V2Hv)
160300060830      * - Statistiche LETTURA
160400060830     c                   eval      D5Cbar  = VbarP(V2Hv)
160500060830     c                   eval      D5Cldv  = Vldv(V2Hv)
160600060830     c                   eval      D5Cerr  = VPusLos(V2Hv)
160700060830      * - Test OverFlow
160800060830if  4c                   if        *in01
160900060929     c                   write     MS08PT                                       *testata
161000060830     c                   eval      *in01   = *off
161100060830e   4c                   endif
161200060830      * - Stampa riga di dettaglio
161300060929     c                   write     MS08D5
161400060830      *
161500060830e   3c                   ENDSL
161600060830      *
161700060830e   2c                   ENDIF
161800060830      *
161900060830e   1c                   ENDDO                                                  *fine sfile
162000060830      *
162100060830      * Stampa riga di chiusura
162200060929     c                   write     MS08RI
162300060830      *
162400060830      * Stampa riga di fine prospetto
162500060929     c                   write     MS08FI
162600060830      *
162700060830      * Chiusura file di stampa
162800060929     c                   close     FIMS08P
162900060830      *
163000060830     c                   ENDSR
163100060830
163200060830      *---------------------------------------------------------------*
163300060831      *?Tasto funzionale F22 -> Coda di stampa                       ?*
163400060830      *---------------------------------------------------------------*
163500060830     c     F22ges        BEGSR
163600060830      *
163700060830      * esegue il comando di gestione coda di stampa associata
163800060830     c                   call      'QCMDEXC'
163900060830     c                   parm      'WRKSPLF  '   Qcmd
164000060901     c                   parm      80            Qlen
164100060830      *
164200060830     c                   ENDSR
164300060830
164400060830      *---------------------------------------------------------------*
164500060831      *?Gestione opzioni della seconda videata (subfile)             ?*
164600060830      *---------------------------------------------------------------*
164700061002     c     OpzS01        BEGSR
164800060830      *
164900060830      * Ciclo di lettura del sfl
165000060929     c                   eval      S01nrr = 1
165100060929     c                   readc     MS08S01
165200060830      *
165300060929do  1c                   DOW       NOT %eof(FIMS08D)
165400060830      *
165500060830      * - Gestione opzioni
165600060830     c                   clear                   SAVnrr
165700060830sel 2c                   select
165800060830      *   5 = Dettaglio
165900060830w   2c                   when      V2Sopz = '5 '
166000060929     c                   eval      SAVnrr = S01nrr
166100060830     c                   exsr      O05ges
166200060830      *   7 = Statistiche
166300060830w   2c                   when      V2Sopz = '7 '
166400060929     c                   eval      SAVnrr = S01nrr
166500060830     c                   exsr      O07ges
166600060830e   2c                   endsl
166700060830      *
166800060830      * - Riposizionamento nel sfl
166900060830if  2c                   if        SAVnrr > *zeros
167000060929     c                   eval      S01nrr = SAVnrr
167100060929     c     S01nrr        chain     MS08S01
167200061002     c                   eval      *in28  = *off
167300060830     c                   clear                   V2Sopz
167400061002     c                   exsr      UpdS01
167500060830e   2c                   endif
167600060830      *
167700060929     c                   readc     MS08S01
167800060830      *
167900060830e   1c                   ENDDO
168000060830      *
168100060830     c                   ENDSR
168200060830
168300060830      *---------------------------------------------------------------*
168400060831      *?Opzione 5 -> Dettaglio dati                                  ?*
168500060830      *---------------------------------------------------------------*
168600060830     c     O05ges        BEGSR
168700060830      *
168800060830     c                   eval      $Video = '3'
168900060830      *
169000060830      * Inizializzazione videata
169100061002     c                   exsr      InzD01
169200060830      *
169300060830     c                   ENDSR
169400060830
169500060830      *---------------------------------------------------------------*
169600060831      *?Opzione 7 -> Statistiche                                     ?*
169700060830      *---------------------------------------------------------------*
169800060830     c     O07ges        BEGSR
169900060830      *
170000060830      * Test del tipo applicazione per decidere quale dettaglio
170100060830      *   statistiche emettere
170200060830sel 1c                   select
170300060830      * - Standard
170400060830w   1c                   when      §MTPtds = *blanks
170500060830     c                   eval      $Video  = '4'
170600061002     c                   exsr      InzD02
170700060830      * - Disk C
170800060830w   1c                   when      §MTPtds = 'D'
170900060830     c                   eval      $Video  = '5'
171000061002     c                   exsr      InzD03
171100060830      * - Picking
171200060830w   1c                   when      §MTPtds = 'K'
171300060830     c                   eval      $Video  = '6'
171400061002     c                   exsr      InzD04
171500060830      *
171600060830e   1c                   endsl
171700060830      *
171800060830     c                   ENDSR
171900060830
172000060830      *---------------------------------------------------------------*
172100061002      *?Aggiornamento riga del subfile                               ?*
172200060830      *---------------------------------------------------------------*
172300061002     c     UpdS01        BEGSR
172400060830      *
172500060830      * Impostazione indicatori per dspatr
172600060830      * - Evidenziazione riga di riepilogo
172700061002     c                   eval      *in40  = (V2Sdes <> *blanks)
172800060830      *
172900061002     c                   eval      C01rcd = S01nrr
173000061002     c*** NO sflnxtchg!  eval      *in32  = *on
173100060929     c                   UPDATE    MS08S01
173200060830      *
173300060830     c                   ENDSR
173400060830
173500060830      *---------------------------------------------------------------*
173600061002      *?Inizializzazione videata col DETTAGLIO                       ?*
173700060830      *---------------------------------------------------------------*
173800061002     c     InzD01        BEGSR
173900060830      *
174000061002     c                   clear                   MS08D01
174100060830      *
174200060830      * campi da sfile
174300061002     c                   eval      V3Ddes = V2Sdes
174400061002     c                   eval      V3Dorl = V2Sorl
174500060830      *
174600060830     c                   if        §KPIpba    <> *zeros
174700061002     c                   eval      V3txtE =  'E N T R A T E   '
174800060830     c                                    + '(MEDIA obiettivo = '
174900060830     c                                    + %trim(%editw(§KPIpba:'0 '))
175000060830     c                                    + ' colli/min.)'
175100060830     c                   else
175200061002     c                   eval      V3txtE = 'E N T R A T E   '
175300060830     c                   endif
175400060830      *
175500060830      * Nomi entrate (non la 20ª)
175600060904     c                   eval      $nro       =  1
175700060904     c                   DOW       $nro       <  %elem($Nmsl)
175800060904     c                   eval      xw         =  1
175900060901     c     $nro          lookup    $Nmsl(xw)                              97
176000170802     c                   If        *in97             and
176100170802     c                             ( D00apl  <> 'K'  or
176200170802     c                              (D00apl   = 'K'  and
176300170802     c                               $pke(xw) = 'S') )
176400060830     c   97              move      $msl(xw)      $V3ent($nro)
176500170802     c                   EndIf
176600060904     c                   eval      $nro       =  $nro + 1
176700060830     c                   ENDDO
176800060830     c                   movea     $V3ent        $V3ent_ds
176900060830      *
177000060830      * Impostazione indice schiera
177100060830     c                   eval      xv         =  V2hv
177200061024      *
177300060830      * Campi da schiere:
177400061024      *
177500060830      * - Entrate
177600060904     c*** già così:      clear                   $V3DE
177700060830     c                   clear                   xx
177800060830do  1c                   DOW       xx         <  %elem($VPExx_ds)
177900060831     c                   eval      xx         =  xx + 1
178000060830     c     xx            occur     $VPExx_ds
178100060830     c                   eval      $V3DE(xx)  =  $VPExx(xv)
178200060830e   1c                   ENDDO
178300060904     c*** già compresa:  movea     $V3DE         $V3DExx_ds
178400061024      *
178500061024      * - - Ripartizione dei colli della post.20 tra gli altri vari
178600061024      *     banchi SE riga di riepilogo o totale
178700061024      *    ?N.B.?- Il totale dei colli (in base a cui ripartire quelli
178800061024      *     della post.20 su tutte le altre post.) va calcolato ora,
178900061024      *     PRIMA di modificare la schiera $V3de con la subroutine
179000061024      *     "RipartB20det" (cioè PRIMA di iniziarne la ripartizione).
179100061024     c                   eval      wRipColli = %xfoot($V3de) - $V3de(20)
179200061024if  2c                   if           ($Vtip(xv) =  'R'                         *Riep
179300061024     c                             or  $Vtip(xv) =  'G')                        *Gen
179400061024     c                             and wRipColli >  *zeros
179500061024     c                   exsr      RipartB20det
179600061024e   2c                   endif
179700011005      *
179800060830      * - Media entrate in base ai minuti lavorati per entrata
179900060830if  1c                   IF        $Vtip(xv)  <> 'M'                            *no dettaglio
180000060830      *
180100060904     c*** già così:      clear                   $V3DM
180200060830     c                   clear                   xx
180300060830do  2c                   DOW       xx         <  %elem($VMLAVxx_ds)
180400060831     c                   eval      xx         =  xx + 1
180500060830     c     xx            occur     $VMLAVxx_ds
180600060904if  3c                   if        $VMLAVxx(xv)  > *zeros
180700061024      * - - Utilizzo il numero colli per postazione già "allineato" dalla
180800061024      *     subr. "RipartB20det"
180900061024      *     (i dati "originali" hanno ancora i colli nella post.20)
181000061024     c***  xx            occur     $VPExx_ds
181100061024     c***                eval      $V3DM(xx)  =  $VPExx(xv) / $VMLAVxx(xv)
181200061205     c**!!!              eval(h)   $V3DM(xx)  =  $V3DE(xx) / $VMLAVxx(xv)
181300061205     c                   eval(h)   w0050      =  $V3DE(xx) / $VMLAVxx(xv)
181400061205     c                   if        w0050 > 99
181500061205     c                   eval      $v3dm(xx) = 99
181600061205     c                   else
181700061205     c                   eval      $v3dm(xx) = w0050
181800061205     c                   endif
181900061205
182000060904e   3c                   endif
182100060830e   2c                   ENDDO
182200060904     c*** già compresa:  movea     $V3DM         $V3DMxx_ds
182300060830      *
182400000000e   1c                   ENDIF
182500060830      *
182600060831      * - Uscite
182700060831     c     xv            occur     $VPAxx_ds
182800060901     c                   eval      V3Du01  =  $VPAxx( 1)
182900060901     c                   eval      V3Du02  =  $VPAxx( 2)
183000060901     c                   eval      V3Du03  =  $VPAxx( 3)
183100060901     c                   eval      V3Du04  =  $VPAxx( 4)
183200060901     c                   eval      V3Du05  =  $VPAxx( 5)
183300060901     c                   eval      V3Du06  =  $VPAxx( 6)
183400060901     c                   eval      V3Du07  =  $VPAxx( 7)
183500060901     c                   eval      V3Du08  =  $VPAxx( 8)
183600060901     c                   eval      V3Du09  =  $VPAxx( 9)
183700060901     c                   eval      V3Du10  =  $VPAxx(10)
183800060901     c                   eval      V3Du11  =  $VPAxx(11)
183900060901     c                   eval      V3Du12  =  $VPAxx(12)
184000060901     c                   eval      V3Du13  =  $VPAxx(13)
184100060901     c                   eval      V3Du14  =  $VPAxx(14)
184200060901     c                   eval      V3Du15  =  $VPAxx(15)
184300060901     c                   eval      V3Du16  =  $VPAxx(16)
184400060901     c                   eval      V3Du17  =  $VPAxx(17)
184500060901     c                   eval      V3Du18  =  $VPAxx(18)
184600060901     c                   eval      V3Du19  =  $VPAxx(19)
184700060901     c                   eval      V3Du20  =  $VPAxx(20)
184800060901     c                   eval      V3Du21  =  $VPAxx(21)
184900060901     c                   eval      V3Du22  =  $VPAxx(22)
185000060901     c                   eval      V3Du23  =  $VPAxx(23)
185100060901     c                   eval      V3Du24  =  $VPAxx(24)
185200060901     c                   eval      V3Du25  =  $VPAxx(25)
185300060901     c                   eval      V3Du26  =  $VPAxx(26)
185400060901     c                   eval      V3Du27  =  $VPAxx(27)
185500060901     c                   eval      V3Du28  =  $VPAxx(28)
185600060901     c                   eval      V3Du29  =  $VPAxx(29)
185700060901     c                   eval      V3Du30  =  $VPAxx(30)
185800060901     c                   eval      V3Du31  =  $VPAxx(31)
185900060901     c                   eval      V3Du32  =  $VPAxx(32)
186000060901     c                   eval      V3Du33  =  $VPAxx(33)
186100060901     c                   eval      V3Du34  =  $VPAxx(34)
186200060901     c                   eval      V3Du35  =  $VPAxx(35)
186300060901     c                   eval      V3Du36  =  $VPAxx(36)
186400060901     c                   eval      V3Du37  =  $VPAxx(37)
186500060901     c                   eval      V3Du38  =  $VPAxx(38)
186600060901     c                   eval      V3Du39  =  $VPAxx(39)
186700060901     c                   eval      V3Du40  =  $VPAxx(40)
186800060901     c                   eval      V3Du41  =  $VPAxx(41)
186900060901     c                   eval      V3Du42  =  $VPAxx(42)
187000060901     c                   eval      V3Du43  =  $VPAxx(43)
187100060901     c                   eval      V3Du44  =  $VPAxx(44)
187200060901     c                   eval      V3Du45  =  $VPAxx(45)
187300060901     c                   eval      V3Du46  =  $VPAxx(46)
187400060901     c                   eval      V3Du47  =  $VPAxx(47)
187500060901     c                   eval      V3Du48  =  $VPAxx(48)
187600060901     c                   eval      V3Du49  =  $VPAxx(49)
187700060901     c                   eval      V3Du50  =  $VPAxx(50)
187800060901     c                   eval      V3Du51  =  $VPAxx(51)
187900060901     c                   eval      V3Du52  =  $VPAxx(52)
188000060901     c                   eval      V3Du53  =  $VPAxx(53)
188100060901     c                   eval      V3Du54  =  $VPAxx(54)
188200060901     c                   eval      V3Du55  =  $VPAxx(55)
188300060901     c                   eval      V3Du56  =  $VPAxx(56)
188400060901     c                   eval      V3Du57  =  $VPAxx(57)
188500060901     c                   eval      V3Du58  =  $VPAxx(58)
188600060901     c                   eval      V3Du59  =  $VPAxx(59)
188700060901     c                   eval      V3Du60  =  $VPAxx(60)
188800060830      *
188900060830     c                   ENDSR
189000060830
189100060831      *---------------------------------------------------------------*
189200060831      *?Gestione terza videata (dettaglio dati)                      ?*
189300060831      *---------------------------------------------------------------*
189400060929     c     GesD01        BEGSR
189500060831      *
189600060831      * Emissione testata e descrizione tasti funzionali
189700061002     c                   write     MS08T01
189800061003     c                   write     MS08Z02
189900060831      * Emissione videata (già inizializzata)
190000061002     c                   exfmt     MS08D01
190100060831      *
190200060831sel 1c                   select
190300060831      * F3 = Fine programma
190400060831w   1c                   when      *inKC
190500060831     c                   exsr      F03ges
190600060831      * F12 = Ritorno
190700060831w   1c                   when      *inKL
190800060831     c                   eval      $Video  = '2'
190900060831      *
191000060831e   1c                   endsl
191100060831      *
191200060831     c                   ENDSR
191300060831
191400060831      *---------------------------------------------------------------*
191500061002      *?Inizializzazione videata STATISTICHE STANDARD                ?*
191600060831      *---------------------------------------------------------------*
191700061002     c     InzD02        BEGSR
191800060831      *
191900061002     c                   clear                   MS08D02
192000060831      *
192100060831     c                   eval      V4Ddes  = V2Sdes
192200060831     c                   eval      V4Dorl  = V2Sorl
192300060831      *
192400060831      * Statistiche LAVORAZIONE
192500060831     c                   eval      V4Dcla  = Vpesind(V2Hv)
192600060831     c                   eval      V4Dsor  = Vpussor(V2Hv)
192700060831     c                   eval      V4Ddna  = Vodsdna(V2Hv)
192800060831     c                   eval      V4Derr  = Verrmac(V2Hv)
192900060831      *
193000060831      * Statistiche LETTURE
193100060831     c                   eval      V4Dcsd  = Vcsd(V2Hv)
193200060831     c                   eval      V4Dps2  = Vps2dok(V2Hv)
193300060831     c                   eval      V4Dccv  = Vccv(V2Hv)
193400060831     c                   eval      V4Dccp  = Vccp(V2Hv)
193500060831     c                   eval      V4Dnls  = Vnls(V2Hv)
193600060831     c                   eval      V4Dnlp  = Vnlp(V2Hv)
193700060831     c                   eval      V4Dnmi  = Vxnomis(V2Hv)
193800060831     c                   eval      V4Dnpe  = Vxnopes(V2Hv)
193900060831      *
194000060831     c                   ENDSR
194100060831
194200060831      *---------------------------------------------------------------*
194300061002      *?Inizializzazione videata STATISTICHE DISK C                  ?*
194400060831      *---------------------------------------------------------------*
194500061002     c     InzD03        BEGSR
194600060831      *
194700061002     c                   clear                   MS08D03
194800060831      *
194900060831     c                   eval      V5Ddes  = V2Sdes
195000060831     c                   eval      V5Dorl  = V2Sorl
195100060831      *
195200060831      * Statistiche LAVORAZIONE
195300060831     c                   eval      V5Dind  = Vpesind(V2Hv)
195400060831     c                   eval      V5Dsor  = Vpussor(V2Hv)
195500060901     c                   eval      V5Dlos  = Vpuslos(V2Hv)
195600060831     c                   eval      V5Dnes  = Vpusnes(V2Hv)
195700060831      *
195800060831      * Statistiche LETTURE
195900060831     c                   eval      V5Dchi  = Vchi(V2Hv)
196000060831     c                   eval      V5Dbar  = Vbar(V2Hv)
196100060831      *
196200060831     c                   ENDSR
196300060831
196400060831      *---------------------------------------------------------------*
196500061002      *?Inizializzazione videata STATISTICHE PICKING                 ?*
196600060831      *---------------------------------------------------------------*
196700061002     c     InzD04        BEGSR
196800060831      *
196900061002     c                   clear                   MS08D04
197000060831      *
197100060831     c                   eval      V6Ddes  = V2Sdes
197200060831     c                   eval      V6Dorl  = V2Sorl
197300060831      *
197400060831      * Statistiche LAVORAZIONE
197500060831     c                   eval      V6Dind  = Vpesind(V2Hv)
197600060831      *
197700060831      * Statistiche LETTURE
197800060831     c                   eval      V6Dbar  = VbarP(V2Hv)
197900060831     c                   eval      V6Dldv  = Vldv(V2Hv)
198000060831     c                   eval      V6Derr  = Vpuslos(V2Hv)
198100060831      *
198200060831     c                   ENDSR
198300060831
198400060831      *---------------------------------------------------------------*
198500061002      *?Gestione videate STATISTICHE (STANDARD / DISK C / PICKING)   ?*
198600060831      *---------------------------------------------------------------*
198700060929     c     GesD0234      BEGSR
198800060831      *
198900061003      * Emissione testata e descrizione tasti funzionali
199000061002     c                   write     MS08T01
199100061003     c                   write     MS08Z02
199200061003      * Emissione videata (già inizializzata)
199300060831sel 1c                   select
199400060831w   1c                   when      $Video  = '4'
199500061002     c                   exfmt     MS08D02
199600060831w   1c                   when      $Video  = '5'
199700061002     c                   exfmt     MS08D03
199800061002w   1c                   when      $Video  = '6'
199900061002     c                   exfmt     MS08D04
200000060831e   1c                   endsl
200100060831      *
200200060831sel 1c                   select
200300060831      * F3 = Fine programma
200400060831w   1c                   when      *inKC
200500060831     c                   exsr      F03ges
200600060831      * F12 = Ritorno
200700060831w   1c                   when      *inKL
200800060831     c                   eval      $Video  = '2'
200900060831      *
201000060831e   1c                   endsl
201100060831      *
201200060831     c                   ENDSR
201300060831
201400060831      *---------------------------------------------------------------*
201500060831      *?Memorizzazione dati del file in schiere                      ?*
201600060831      *---------------------------------------------------------------*
201700060831     c     sr_MemoSch    BEGSR
201800080909      /free
201900080909           reset  $TooManyRec;
202000080909      /end-free
202100060831      *
202200060831      * Memorizzazione schiere dal file SALDI LETTURE
202300060831     c                   exsr      sr_MEMmsa
202400060831      *
202500060831      * SE reperiti dati in FNMSA00F:
202600060831if  1c                   IF        $FNMSA = *on
202700060831      *
202800060831      * Memorizzazione schiere dal file SALDI LETTURE x giorno prec.
202900060831     c                   exsr      sr_MEMmsaPR
203000080909      /free
203100080909           if  $TooManyRec = *on;
203200080909             leavesr;
203300080909           endif;
203400080909      /end-free
203500060831      *
203600060831      * Creazione GRIGLIA di rappresentazione
203700060831     c                   exsr      sr_CrtGriglia
203800080909      /free
203900080909           if  $TooManyRec = *on;
204000080909             leavesr;
204100080909           endif;
204200080909      /end-free
204300060831      *
204400060831      * Valorizzazione schiere video dalla griglia di rappresentazione
204500060831     c                   exsr      sr_ValScVid
204600080909      /free
204700080909           if  $TooManyRec = *on;
204800080909             leavesr;
204900080909           endif;
205000080909      /end-free
205100060831      *
205200060831      * Calcolo valori statistici
205300060831     c                   exsr      sr_CalStVid
205400060831      *
205500000000e   1c                   ENDIF
205600060831      *
205700060831     c                   ENDSR
205800060831
205900060831      *---------------------------------------------------------------*
206000060831      *?Memorizzazione dati dal file SALDI LETTURE VDL               ?*
206100060831      *---------------------------------------------------------------*
206200060831     c     sr_MEMmsa     BEGSR
206300060831      *
206400060831      * Pulizia delle variabili di lavoro
206500060831     c                   eval      $FNMSA  = *off
206600060831     c                   clear                   xi
206700060831     c                   clear                   $dtl
206800060831     c                   clear                   $orl
206900060831     c                   clear                   $ncl
207000061006     c***                clear                   $PEPxx_ds
207100061006     c***                clear                   $PEPxx
207200061006     c                   clear                   xw
207300061006do  1c                   DOW       xw       < %elem($PEPxx_ds)
207400061006     c                   add       1             xw
207500061006     c     xw            occur     $PEPxx_ds
207600061006     c                   clear                   $PEPxx
207700061006e   1c                   ENDDO
207800060831     ***c                   clear                   Mpe1
207900060831     ***c                   clear                   Mpe2
208000060831     ***c                   clear                   Mpe3
208100060831     ***c                   clear                   Mpe4
208200060831     ***c                   clear                   Mpe5
208300060831     ***c                   clear                   Mpe6
208400060831     ***c                   clear                   Mpe7
208500060831     ***c                   clear                   Mpe8
208600060831     ***c                   clear                   Mpe9
208700060831     ***c                   clear                   Mpe0
208800060831     ***c                   clear                   Mpe11
208900060831     ***c                   clear                   Mpe12
209000060831     ***c                   clear                   Mpe13
209100060831     ***c                   clear                   Mpe14
209200060831     ***c                   clear                   Mpe15
209300060831     ***c                   clear                   Mpe16
209400060831     ***c                   clear                   Mpe17
209500060831     ***c                   clear                   Mpe18
209600060831     ***c                   clear                   Mpe19
209700060831     ***c                   clear                   Mpe20
209800061006     c***                clear                   $PADxx_ds
209900061006     c***                clear                   $PADxx
210000061006     c                   clear                   xw
210100061006do  1c                   DOW       xw       < %elem($PADxx_ds)
210200061006     c                   add       1             xw
210300061006     c     xw            occur     $PADxx_ds
210400061006     c                   clear                   $PADxx
210500061006e   1c                   ENDDO
210600060831     ***c                   clear                   Mpa1
210700060831     ***c                   clear                   Mpa2
210800060831     ***c                   clear                   Mpa3
210900060831     ***c                   clear                   Mpa4
211000060831     ***c                   clear                   Mpa5
211100060831     ***c                   clear                   Mpa6
211200060831     ***c                   clear                   Mpa7
211300060831     ***c                   clear                   Mpa8
211400060831     ***c                   clear                   Mpa9
211500060831     ***c                   clear                   Mpa10
211600060831     ***c                   clear                   Mpa11
211700060831     ***c                   clear                   Mpa12
211800060831     ***c                   clear                   Mpa13
211900060831     ***c                   clear                   Mpa14
212000060831     ***c                   clear                   Mpa15
212100060831     ***c                   clear                   Mpa16
212200060831     ***c                   clear                   Mpa17
212300060831     ***c                   clear                   Mpa18
212400060831     ***c                   clear                   Mpa19
212500060831     ***c                   clear                   Mpa20
212600060831     ***c                   clear                   Mpa21
212700060831     ***c                   clear                   Mpa22
212800060831     ***c                   clear                   Mpa23
212900060831     ***c                   clear                   Mpa24
213000060831     ***c                   clear                   Mpa25
213100060831     ***c                   clear                   Mpa26
213200060831     ***c                   clear                   Mpa27
213300060831     ***c                   clear                   Mpa28
213400060831     ***c                   clear                   Mpa29
213500060831     ***c                   clear                   Mpa30
213600060831     ***c                   clear                   Mpa31
213700060831     ***c                   clear                   Mpa32
213800060831     ***c                   clear                   Mpa33
213900060831     ***c                   clear                   Mpa34
214000060831     ***c                   clear                   Mpa35
214100060831     ***c                   clear                   Mpa36
214200060831     ***c                   clear                   Mpa37
214300060831     ***c                   clear                   Mpa38
214400060831     ***c                   clear                   Mpa39
214500060831     ***c                   clear                   Mpa40
214600060831     ***c                   clear                   Mpa41
214700060831     ***c                   clear                   Mpa42
214800060831     ***c                   clear                   Mpa43
214900060831     ***c                   clear                   Mpa44
215000060831     ***c                   clear                   Mpa45
215100060831     ***c                   clear                   Mpa46
215200060831     ***c                   clear                   Mpa47
215300060831     ***c                   clear                   Mpa48
215400060831     ***c                   clear                   Mpa49
215500060831     ***c                   clear                   Mpa50
215600060831     ***c                   clear                   Mpa51
215700060831     ***c                   clear                   Mpa52
215800060831     ***c                   clear                   Mpa53
215900060831     ***c                   clear                   Mpa54
216000060831     ***c                   clear                   Mpa55
216100060831     ***c                   clear                   Mpa56
216200060831     ***c                   clear                   Mpa57
216300060831     ***c                   clear                   Mpa58
216400060831     ***c                   clear                   Mpa59
216500060831     ***c                   clear                   Mpa60
216600060831     c                   clear                   $pesind
216700060831     c                   clear                   $pussor
216800060831     c                   clear                   $puslos
216900060831     c                   clear                   $pusnes
217000060831     c                   clear                   $csd
217100060831     c                   clear                   $ps2dok
217200060831     c                   clear                   $ccv
217300060831     c                   clear                   $ccp
217400060831     c                   clear                   $nls
217500060831     c                   clear                   $nlp
217600060831     c                   clear                   $xnomis
217700060831     c                   clear                   $xnopes
217800060831     c                   clear                   $odsdna
217900060831     c                   clear                   $errmac
218000060831     c                   clear                   $pesind2
218100060831     c                   clear                   $pussor2
218200060831     c                   clear                   $odsdna2
218300060831     c                   clear                   $errmac2
218400060831     c                   clear                   ULTmsa
218500040405     c                   reset                   W00tli
218600040405     c                   reset                   W00tlf
218700060831      *
218800060831      * Ciclo di lettura dati del foglio da caricare
218900060831     c                   eval      MSAapl  = D00apl
219000060831     c                   eval      MSAfil  = D00fil
219100060831     c                   eval      MSAdfv  = D00dfv
219200060831     c                   eval      MSAnpg  = D00npg
219300170201     c     K04MSA02      setll     FNMSA000
219400060831      *
219500060831do  1c                   DO        *hival
219600060831      *
219700170201     c     K04MSA02      reade     FNMSA000
219800060831      *
219900170201if  2c                   if        %eof(FNMSA02L)
220000060831     c                   leave
220100060831e   2c                   endif
220200060831      *
220300060831     c                   exsr      ChkMSA
220400060831if  2c                   if        $RecOK  = *off
220500060831     c                   iter
220600060831e   2c                   endif
220700080909      /free
220800080909       // - TROPPI dati !!!
220900080909           if  xi >= %elem($dtl);
221000080909             $TooManyRec = *on;
221100080909             $FNMSA      = *off;
221200080909             leavesr;
221300080909           endif;
221400080909      /end-free
221500060831      *
221600060831     c                   eval      $FNMSA  = *on
221700060831     c                   eval      xi      = xi + 1
221800060831      *
221900060831     c                   eval      $dtl(xi)     = MSAdtl
222000060831     c                   eval      $orl(xi)     = MSAorl
222100060831     c                   eval      $ncl(xi)     = MSApep001                     *colli lavorati
222200060831     c                                          + MSApep002
222300060831     c                                          + MSApep003
222400060831     c                                          + MSApep004
222500060831     c                                          + MSApep005
222600060831     c                                          + MSApep006
222700060831     c                                          + MSApep007
222800060831     c                                          + MSApep008
222900060831     c                                          + MSApep009
223000060831     c                                          + MSApep010
223100060831     c                                          + MSApep011
223200060831     c                                          + MSApep012
223300060831     c                                          + MSApep013
223400060831     c                                          + MSApep014
223500060831     c                                          + MSApep015
223600060831     c                                          + MSApep016
223700060831     c                                          + MSApep017
223800060831     c                                          + MSApep018
223900060831     c                                          + MSApep019
224000061024     c                                          + MSApep020                     ANCHE post.20!
224100060831      * - dettaglio entrate
224200060831     c     xi            occur     $PEPxx_ds
224300060901     c                   eval      $PEPxx( 1)   = MSApep001
224400060901     c                   eval      $PEPxx( 2)   = MSApep002
224500060901     c                   eval      $PEPxx( 3)   = MSApep003
224600060901     c                   eval      $PEPxx( 4)   = MSApep004
224700060901     c                   eval      $PEPxx( 5)   = MSApep005
224800060901     c                   eval      $PEPxx( 6)   = MSApep006
224900060901     c                   eval      $PEPxx( 7)   = MSApep007
225000060901     c                   eval      $PEPxx( 8)   = MSApep008
225100060901     c                   eval      $PEPxx( 9)   = MSApep009
225200060901     c                   eval      $PEPxx(10)   = MSApep010
225300060901     c                   eval      $PEPxx(11)   = MSApep011
225400060901     c                   eval      $PEPxx(12)   = MSApep012
225500060901     c                   eval      $PEPxx(13)   = MSApep013
225600060901     c                   eval      $PEPxx(14)   = MSApep014
225700060901     c                   eval      $PEPxx(15)   = MSApep015
225800060901     c                   eval      $PEPxx(16)   = MSApep016
225900060901     c                   eval      $PEPxx(17)   = MSApep017
226000060901     c                   eval      $PEPxx(18)   = MSApep018
226100060901     c                   eval      $PEPxx(19)   = MSApep019
226200060901     c                   eval      $PEPxx(20)   = MSApep020
226300060831      * - dettaglio uscite
226400060831     c     xi            occur     $PADxx_ds
226500060901     c                   eval      $PADxx( 1)   = MSApad001
226600060901     c                   eval      $PADxx( 2)   = MSApad002
226700060901     c                   eval      $PADxx( 3)   = MSApad003
226800060901     c                   eval      $PADxx( 4)   = MSApad004
226900060901     c                   eval      $PADxx( 5)   = MSApad005
227000060901     c                   eval      $PADxx( 6)   = MSApad006
227100060901     c                   eval      $PADxx( 7)   = MSApad007
227200060901     c                   eval      $PADxx( 8)   = MSApad008
227300060901     c                   eval      $PADxx( 9)   = MSApad009
227400060901     c                   eval      $PADxx(10)   = MSApad010
227500060901     c                   eval      $PADxx(11)   = MSApad011
227600060901     c                   eval      $PADxx(12)   = MSApad012
227700060901     c                   eval      $PADxx(13)   = MSApad013
227800060901     c                   eval      $PADxx(14)   = MSApad014
227900060901     c                   eval      $PADxx(15)   = MSApad015
228000060901     c                   eval      $PADxx(16)   = MSApad016
228100060901     c                   eval      $PADxx(17)   = MSApad017
228200060901     c                   eval      $PADxx(18)   = MSApad018
228300060901     c                   eval      $PADxx(19)   = MSApad019
228400060901     c                   eval      $PADxx(20)   = MSApad020
228500060901     c                   eval      $PADxx(21)   = MSApad021
228600060901     c                   eval      $PADxx(22)   = MSApad022
228700060901     c                   eval      $PADxx(23)   = MSApad023
228800060901     c                   eval      $PADxx(24)   = MSApad024
228900060901     c                   eval      $PADxx(25)   = MSApad025
229000060901     c                   eval      $PADxx(26)   = MSApad026
229100060901     c                   eval      $PADxx(27)   = MSApad027
229200060901     c                   eval      $PADxx(28)   = MSApad028
229300060901     c                   eval      $PADxx(29)   = MSApad029
229400060901     c                   eval      $PADxx(30)   = MSApad030
229500060901     c                   eval      $PADxx(31)   = MSApad031
229600060901     c                   eval      $PADxx(32)   = MSApad032
229700060901     c                   eval      $PADxx(33)   = MSApad033
229800060901     c                   eval      $PADxx(34)   = MSApad034
229900060901     c                   eval      $PADxx(35)   = MSApad035
230000060901     c                   eval      $PADxx(36)   = MSApad036
230100060901     c                   eval      $PADxx(37)   = MSApad037
230200060901     c                   eval      $PADxx(38)   = MSApad038
230300060901     c                   eval      $PADxx(39)   = MSApad039
230400060901     c                   eval      $PADxx(40)   = MSApad040
230500060901     c                   eval      $PADxx(41)   = MSApad041
230600060901     c                   eval      $PADxx(42)   = MSApad042
230700060901     c                   eval      $PADxx(43)   = MSApad043
230800060901     c                   eval      $PADxx(44)   = MSApad044
230900060901     c                   eval      $PADxx(45)   = MSApad045
231000060901     c                   eval      $PADxx(46)   = MSApad046
231100060901     c                   eval      $PADxx(47)   = MSApad047
231200060901     c                   eval      $PADxx(48)   = MSApad048
231300060901     c                   eval      $PADxx(49)   = MSApad049
231400060901     c                   eval      $PADxx(50)   = MSApad050
231500060901     c                   eval      $PADxx(51)   = MSApad051
231600060901     c                   eval      $PADxx(52)   = MSApad052
231700060901     c                   eval      $PADxx(53)   = MSApad053
231800060901     c                   eval      $PADxx(54)   = MSApad054
231900060901     c                   eval      $PADxx(55)   = MSApad055
232000060901     c                   eval      $PADxx(56)   = MSApad056
232100060901     c                   eval      $PADxx(57)   = MSApad057
232200060901     c                   eval      $PADxx(58)   = MSApad058
232300060901     c                   eval      $PADxx(59)   = MSApad059
232400060901     c                   eval      $PADxx(60)   = MSApad060
232500060831      *
232600060831      * Per statistiche:
232700060831      * - Lavorazione
232800060831     c                   eval      $pesind(xi)  = MSApesind
232900060831     c                   eval      $pussor(xi)  = MSApussor
233000060831     c                   eval      $puslos(xi)  = MSApuslos
233100060831     c                   eval      $pusnes(xi)  = MSApusnes
233200060831      * - Letture
233300060831     c                   eval      $csd(xi)     = MSAcsd                        *letti da scanner
233400060901     c                   eval      $ps2dok(xi)  = MSAps2dok                     *letti da pistola
233500060831     c                   eval      $ccv(xi)     = MSAccv                        *letti con volume
233600060831     c                   eval      $ccp(xi)     = MSAccp                        *letti con peso
233700060831      * - Anomalie
233800060831     c                   eval      $nls(xi)     = MSAnls                        *non letti scanner
233900060831     c                   eval      $nlp(xi)     = MSAnlp                        *non letti pistola
234000060831     c                   eval      $xnomis(xi)  = MSApussor - MSAccv            *C: non misurati
234100060831     c                   eval      $xnopes(xi)  = MSApussor - MSAccp            *C: non pesati
234200060831     c                   eval      $odsdna(xi)  = MSAodsdna                     *of x uscita piena
234300060831     c                   eval      $errmac(xi)  = MSApesind                     *of x errori machine
234400060831     c                                          - MSApussor
234500060831     c                                          - MSAodsdna
234600060608      *?*NEW?
234700060831     c                   eval      $pesind2(xi) = MSAps1dnr                     »caricati da VdL
234800060831     c                   eval      $pussor2(xi) = MSAps1dts                     »smistati da VdL
234900060831     c                   eval      $odsdna2(xi) = MSAps1dtl                     »of x uscita piena
235000060831     c                   eval      $errmac2(xi) = MSAps1nsc                     »of x errori machine
235100060831      *
235200060831e   1c                   ENDDO
235300060831      *
235400060831     c                   eval      ULTmsa  = xi
235500060831      *
235600060831     c                   ENDSR
235700060831
235800060831      *---------------------------------------------------------------*
235900060831      *?Memorizzazione dati dal file SALDI LETTURE VDL x giorno prec.?*
236000060831      *---------------------------------------------------------------*
236100060831     c     sr_MEMmsaPR   BEGSR
236200060831      *
236300060831      * Pulizia delle variabili di lavoro
236400061002     ***c                   eval      $FNMSApr     = *off
236500060831     c                   clear                   xi
236600060831     c                   clear                   $Pdtl                          *schiere memo
236700060831     c                   clear                   $Porl
236800060831     c                   clear                   $Pncl
236900060831     c                   clear                   ULTmsaPR
237000040405     c                   reset                   W00tli
237100040405     c                   reset                   W00tlf
237200060831      *
237300060831      * Ciclo di lettura dati del foglio del GIORNO PRECEDENTE da
237400060831      *   caricare
237500060831     c                   eval      MSAapl  = D00apl
237600060831     c                   eval      MSAfil  = D00fil
237700060831     c                   eval      MSAdfv  = D00dfvPR
237800060831     c                   eval      MSAnpg  = D00npg
237900170201     c     K04MSA02      setll     FNMSA000
238000170201     c     K04MSA02      reade     FNMSA000
238100060831      *
238200170201do  1c                   DOW       NOT %eof(FNMSA02L)
238300060831      *
238400060831     c                   exsr      ChkMSA
238500060831if  2c                   if        $RecOK  = *on
238600060831      *
238700080909      /free
238800080909       // - TROPPI dati !!!
238900080909           if  xi >= %elem($Pdtl);
239000080909             $TooManyRec = *on;
239100080909             $FNMSA      = *off;
239200080909             leavesr;
239300080909           endif;
239400080909      /end-free
239500061002     ***c                   eval      $FNMSApr     = *on
239600060831     c                   eval      xi      = xi + 1
239700060831     c                   eval      $Pdtl(xi)    = MSAdtl
239800060831     c                   eval      $Porl(xi)    = MSAorl
239900060831     c                   eval      $Pncl(xi)    = MSApep001                     *colli lavorati
240000060831     c                                          + MSApep002
240100060831     c                                          + MSApep003
240200060831     c                                          + MSApep004
240300060831     c                                          + MSApep005
240400060831     c                                          + MSApep006
240500060831     c                                          + MSApep007
240600060831     c                                          + MSApep008
240700060831     c                                          + MSApep009
240800060831     c                                          + MSApep010
240900060831     c                                          + MSApep011
241000060831     c                                          + MSApep012
241100060831     c                                          + MSApep013
241200060831     c                                          + MSApep014
241300060831     c                                          + MSApep015
241400060831     c                                          + MSApep016
241500060831     c                                          + MSApep017
241600060831     c                                          + MSApep018
241700060831     c                                          + MSApep019
241800070626     c                                          + MSApep020                     ANCHE post.20!
241900060831      *
242000060831e   2c                   endif
242100060831      *
242200170201     c     K04MSA02      reade     FNMSA000
242300060831      *
242400060831e   1c                   ENDDO
242500060831      *
242600060831     c                   eval      ULTmsaPR     = xi
242700060831      *
242800060831     c                   ENDSR
242900060831
243000060831      *---------------------------------------------------------------*
243100060831      *?Controlla validità record FNMSA                              ?*
243200060831      *---------------------------------------------------------------*
243300060831     c     ChkMSA        BEGSR
243400060831      *
243500060831     c                   reset                   $RecOk
243600040316      *
243700060831      * Parzializzazione (eventuale) per orario di lettura
243800060831if  1c                   IF            (D00oli <> *zeros
243900040316     c                              or  D00olf <> *zeros)
244000060831      *
244100040405      * Impostazione data+ora iniziale e finale di selezione
244200060831if  2c                   IF        W00tli  = *loval
244300060831      *
244400060831sel 3c                   select
244500040609      * - data richiesta
244600060831w   3c                   when      MSAdfv  = D00dfv
244700040609     c     *ISO          move      D00dli        Wdata
244800060831     c                   eval      n6      = D00oli * 100
244900040609     c     *ISO          move      n6            Wora
245000040609     c                   movel     Wtempo        W00tli                         *periodo INIZIALE
245100040609     c     *ISO          move      D00dlf        Wdata
245200060831     c                   eval      n6      = D00olf * 100
245300040609     c     *ISO          move      n6            Wora
245400040609     c                   movel     Wtempo        W00tlf                         *periodo FINALE
245500040609      * - data precedente (per confronto)
245600060831w   3c                   when      MSAdfv  = D00dfvPR
245700040609      * - - conto i giorni di differenza
245800040609     c                   clear                   Wdays
245900040609     c                   reset                   Wtempo
246000040609     c                   reset                   Wtempo2
246100040609     c     *ISO          move      D00dfv        Wdata
246200040609     c                   movel     Wtempo        Wtempo2
246300040609     c     *ISO          move      D00dfvPR      Wdata
246400060831do  4c                   dow       Wtempo  < Wtempo2
246500040609     c                   add       1             Wdays
246600040609     c                   adddur    1:*d          Wdata
246700040609e   4c                   enddo
246800040609      * - - imposto le date corrispondenti a quelle iniziale e finale
246900040609     c     *ISO          move      D00dli        Wdata
247000040609     c                   subdur    wDays:*d      Wdata
247100060831     c                   eval      n6      = D00oli * 100
247200040609     c     *ISO          move      n6            Wora
247300040609     c                   movel     Wtempo        W00tli                         *periodo INIZIALE
247400040609     c     *ISO          move      D00dlf        Wdata
247500040609     c                   subdur    wDays:*d      Wdata
247600060831     c                   eval      n6      = D00olf * 100
247700040609     c     *ISO          move      n6            Wora
247800040609     c                   movel     Wtempo        W00tlf                         *periodo FINALE
247900060831e   3c                   endsl
248000060831      *
248100060831e   2c                   ENDIF
248200040405      * Test orario
248300040405     c                   z-add     MSAdtl        n8
248400040405     c                   movel     MSAorl        n6
248500040405     c     *ISO          move      n8            Wdata
248600040405     c     *ISO          move      n6            Wora
248700040405if  2c                   if            (Wtempo <  W00tli
248800040405     c                              or  Wtempo >  W00tlf)
248900060831     c                   leavesr
249000040405e   2c                   endif
249100060831      *
249200060831e   1c                   ENDIF
249300040316      *
249400060831     c                   eval      $RecOK  = *on
249500060831      *
249600060831     c                   ENDSR
249700060831
249800060831      *---------------------------------------------------------------*
249900060831      *?Creazione griglia di rappresentazione                        ?*
250000060831      *---------------------------------------------------------------*
250100060831     c     sr_CrtGriglia BEGSR
250200060831      *
250300060831      * Pulizia delle variabili di lavoro
250400060831     c                   clear                   GDper                          *dettaglio
250500060831     c                   clear                   GRper                          *riepilogo
250600080116     c                   clear                   $Gtip                          *unione
250700080116     c                   clear                   $Gdtl
250800080116     c                   clear                   $Gorl
250900060831     c                   clear                   xi
251000060831     c                   clear                   MEMxi
251100060831     c                   clear                   MEMgd
251200060831     c                   clear                   xj
251300060831     c                   clear                   xg
251400060831     c                   clear                   MEMxg
251500060831      *
251600060831      * Crea la griglia di dettaglio - - - - - - - - - - - - - - - - -*
251700060831      *
251800060831      * imposta la PRIMA data + ora
251900060831     c                   movel     $dtl(1)       PrimaData
252000060831     c                   move      $orl(1)       PrimaData
252100060831      *
252200060831      * imposta l'ULTIMA data + ora
252300060831     c                   movel     $dtl(ULTmsa)  UltimData
252400060831     c                   move      $orl(ULTmsa)  UltimData
252500060831      *
252600060831      * imposta la PRIMA data e la PRIMA ora di LAVORO
252700060831     c                   eval      Ldata   = $dtl(1)
252800060831     c                   eval      Lora    = $orl(1)
252900060831      *
253000060831      * ciclo fino a fine periodo
253100060831     c                   clear                   xi
253200060831do  1c                   DOW       PrimaData <= UltimData
253300080909      /free
253400080909       // - TROPPI dati !!!
253500080909           if  xi >= %elem(GDper);
253600080909             $TooManyRec = *on;
253700080909             $FNMSA      = *off;
253800080909             leavesr;
253900080909           endif;
254000080909      /end-free
254100060831      * - aggiunge elemento in schiera
254200060831     c                   add       1             xi
254300060831     c                   movel     Ldata         GDper(xi)
254400060831     c                   move      Lora          GDper(xi)
254500060831      * - aggiunge i minuti di dettaglio richiesti
254600060831     c                   eval      n8      = Ldata
254700060831     c                   eval      n4      = Lora
254800060831     c                   eval      Lmin    = D00mtd
254900060831     c                   exsr      AddMinDet
255000060831     c                   eval      Ldata   = n8
255100060831     c                   eval      Lora    = n4
255200060831      * - incrementa la PRIMA data + ora
255300060831     c                   movel     Ldata         PrimaData
255400060831     c                   move      Lora          PrimaData
255500000000e   1c                   ENDDO
255600060831     c                   eval      MEMgd   = xi
255700060831      *
255800060831      * Crea la griglia di riepilogo - - - - - - - - - - - - - - - - -*
255900060831      *
256000060831      * aggiunge alla PRIMA data + ora i mm di riepilogo richiesti
256100060831     c                   eval      n8      = $dtl(1)
256200060831     c                   eval      n4      = $orl(1)
256300060831     c                   eval      Lmin    = D00mtr
256400060831     c                   exsr      AddMinDet
256500060831     c                   movel     n8            PrimaData
256600060831     c                   move      n4            PrimaData
256700060831      *
256800060831      * imposta la PRIMA data e la PRIMA ora di LAVORO
256900060831     c                   eval      Ldata   = n8
257000060831     c                   eval      Lora    = n4
257100060831      *
257200060831      * imposta l'ULTIMA data + ora
257300060831     c                   movel     $dtl(ULTmsa)  UltimData
257400060831     c                   move      $orl(ULTmsa)  UltimData
257500060831      *
257600060831      * ciclo fino a fine periodo
257700060831     c                   clear                   xi
257800060831do  1c                   DOW       PrimaData <= UltimData
257900080909      /free
258000080909       // - TROPPI dati !!!
258100080909           if  xi >= %elem(GRper);
258200080909             $TooManyRec = *on;
258300080909             $FNMSA      = *off;
258400080909             leavesr;
258500080909           endif;
258600080909      /end-free
258700060831      * aggiunge elemento in schiera
258800060831     c                   add       1             xi
258900060831     c                   movel     Ldata         GRper(xi)
259000060831     c                   move      Lora          GRper(xi)
259100060831      * aggiunge i minuti di riepilogo richiesti
259200060831     c                   eval      n8      = Ldata
259300060831     c                   eval      n4      = Lora
259400060901     c                   eval      LMin    = D00mtr
259500060831     c                   exsr      AddMinDet
259600060831     c                   eval      Ldata   = n8
259700060831     c                   eval      Lora    = n4
259800060831      * incrementa la PRIMA data + ora
259900060901     c                   movel     Ldata         PrimaData
260000060901     c                   move      Lora          PrimaData
260100000000e   1c                   ENDDO
260200060831     c                   eval      MEMxi   = xi
260300060831      *
260400060831      * Crea la griglia di unione  - - - - - - - - - - - - - - - - - -*
260500060831      *
260600060831     c                   eval      xi      = 1                                  *indice x riepilogo
260700060831     c                   eval      xj      = 1                                  *indice x dettaglio
260800060831     c                   clear                   xg                             *indice x unione
260900060831      *
261000060831      * imposta il tipo griglia per il dettaglio
261100060831if  1c                   if        D00mtd  = 1                                  *dettaglio minimo
261200060831     c                   eval      DEPtip  = 'M'
261300060831x   1c                   else                                                   *dettaglio sintetico
261400060831     c                   eval      DEPtip  = 'D'
261500060831e   1c                   endif
261600060831      *
261700060831      * lettura griglia riepilogo
261800060830do  1c                   DO        MEMxi         xi
261900060831      *
262000060831      * - lettura griglia dettaglio
262100060831do  2c     xj            DO        MEMgd         xx
262200060831      * - - memorizza in GRIGLIA i minuti di dettaglio griglia compresi
262300060831      *     nel mm di riepilogo
262400060831if  3c                   IF        GDper(xx)  > *zeros
262500060831if  4c                   if        GDper(xx) <= GRper(xi)
262600060831     c                   add       1             xg
262700080116     c                   eval      $Gtip(xg)  = DEPtip                          *tipo elemento
262800080116     c                   movel     GDper(xx)     $Gdtl(xg)                      *GRIGLIA data -dett-
262900080116     c                   move      GDper(xx)     $Gorl(xg)                      *GRIGLIA ora  -dett-
263000060831x   4c                   else
263100060831     c                   leave
263200060831e   4c                   endif
263300000000e   3c                   ENDIF
263400060831e   2c                   ENDDO
263500060831     c                   eval      xj         = xx
263600060831      *
263700060831      * memorizza in GRIGLIA il minuto di riepilogo
263800060831     c                   add       1             xg
263900080116     c                   eval      $Gtip(xg)  = 'R'                             *tipo elemento
264000080116     c                   movel     GRper(xi)     $Gdtl(xg)                      *GRIGLIA data -riep-
264100080116     c                   move      GRper(xi)     $Gorl(xg)                      *GRIGLIA ora  -riep-
264200060831      *
264300060831e   1c                   ENDDO
264400060831      *
264500060831      * memorizza in GRIGLIA i minuti di dettaglio non compresi nell'
264600060831      *   ultimo riepilogo
264700060831do  1c     xj            do        MEMgd         xx
264800060831     c                   add       1             xg
264900080116     c                   eval      $Gtip(xg)  = DEPtip                          *tipo elemento
265000080116     c                   movel     GDper(xx)     $Gdtl(xg)                      *GRIGLIA data -dett-
265100080116     c                   move      GDper(xx)     $Gorl(xg)                      *GRIGLIA ora  -dett-
265200060831e   1c                   enddo
265300060831      *
265400060831      * memorizza in GRIGLIA il minuto di totale generale
265500060831     c                   add       1             xg
265600080116     c                   eval      $Gtip(xg)  = 'G'                             *tipo elemento
265700080116     c                   eval      $Gdtl(xg)  = $Gdtl(xg-1)                     *GRIGLIA data -gen-
265800080116     c                   eval      $Gorl(xg)  = $Gorl(xg-1)                     *GRIGLIA ora  -gen-
265900060831     c                   eval      MEMxg      = xg
266000060831      *
266100060831     c                   ENDSR
266200060831
266300060831      *---------------------------------------------------------------*
266400060831      *?Aggiunge i minuti di dettaglio                               ?*
266500060831      *---------------------------------------------------------------*
266600060831     c     AddMinDet     BEGSR
266700060831      *
266800060831     c                   clear                   n6
266900060831     c                   movel     n4            n6
267000060831     c     *ISO          move      n8            Wdata
267100060831     c     *ISO          move      n6            Wora
267200060831     c                   adddur    Lmin:*MN      Wtempo
267300060831     c     *ISO          move      Wdata         n8
267400060831     c     *ISO          move      Wora          n6
267500060831     c                   movel     n6            n4
267600060831      *
267700000000     c                   ENDSR
267800060831
267900060831      *---------------------------------------------------------------*
268000060831      *?Valorizza le schiere video dalla griglia di rappresentazione ?*
268100060831      *---------------------------------------------------------------*
268200060901     c     sr_ValScVid   BEGSR
268300060831      *
268400060831      * Pulizia delle variabili di lavoro
268500060830     c                   clear                   $Vdes
268600060830     c                   clear                   $Vtip
268700060830     c                   clear                   $Vdtl
268800060830     c                   clear                   $Vorl
268900060831     c                   clear                   $Vncl
269000060831     c                   clear                   $VminLav
269100061006     c***                clear                   $VPExx_ds
269200061006     c***                clear                   $VPExx
269300061006     c                   clear                   xw
269400061006do  1c                   DOW       xw       < %elem($VPExx_ds)
269500061006     c                   add       1             xw
269600061006     c     xw            occur     $VPExx_ds
269700061006     c                   clear                   $VPExx
269800061006e   1c                   ENDDO
269900060831     ***c                   CLEAR                   vpe1
270000060831     ***c                   CLEAR                   vpe2
270100060831     ***c                   CLEAR                   vpe3
270200060831     ***c                   CLEAR                   vpe4
270300060831     ***c                   CLEAR                   vpe5
270400060831     ***c                   CLEAR                   vpe6
270500060831     ***c                   CLEAR                   vpe7
270600060831     ***c                   CLEAR                   vpe8
270700060831     ***c                   CLEAR                   vpe9
270800060831     ***c                   CLEAR                   vpe0
270900060831     ***c                   CLEAR                   vpe11
271000060831     ***c                   CLEAR                   vpe12
271100060831     ***c                   CLEAR                   vpe13
271200060831     ***c                   CLEAR                   vpe14
271300060831     ***c                   CLEAR                   vpe15
271400060831     ***c                   CLEAR                   vpe16
271500060831     ***c                   CLEAR                   vpe17
271600060831     ***c                   CLEAR                   vpe18
271700060831     ***c                   CLEAR                   vpe19
271800060831     ***c                   CLEAR                   vpe20
271900061006     c***                clear                   $VMLAVxx_ds
272000061006     c***                clear                   $VMLAVxx
272100061006     c                   clear                   xw
272200061006do  1c                   DOW       xw       < %elem($VMLAVxx_ds)
272300061006     c                   add       1             xw
272400061006     c     xw            occur     $VMLAVxx_ds
272500061006     c                   clear                   $VMLAVxx
272600061006e   1c                   ENDDO
272700060831     ***c                   CLEAR                   vmlav01
272800060831     ***c                   CLEAR                   vmlav02
272900060831     ***c                   CLEAR                   vmlav03
273000060831     ***c                   CLEAR                   vmlav04
273100060831     ***c                   CLEAR                   vmlav05
273200060831     ***c                   CLEAR                   vmlav06
273300060831     ***c                   CLEAR                   vmlav07
273400060831     ***c                   CLEAR                   vmlav08
273500060831     ***c                   CLEAR                   vmlav09
273600060831     ***c                   CLEAR                   vmlav10
273700060831     ***c                   CLEAR                   vmlav11
273800060831     ***c                   CLEAR                   vmlav12
273900060831     ***c                   CLEAR                   vmlav13
274000060831     ***c                   CLEAR                   vmlav14
274100060831     ***c                   CLEAR                   vmlav15
274200060831     ***c                   CLEAR                   vmlav16
274300060831     ***c                   CLEAR                   vmlav17
274400060831     ***c                   CLEAR                   vmlav18
274500060831     ***c                   CLEAR                   vmlav19
274600060831     ***c                   CLEAR                   vmlav20
274700061006     c***                clear                   $VPAxx_ds
274800061006     c***                clear                   $VPAxx
274900061006     c                   clear                   xw
275000061006do  1c                   DOW       xw       < %elem($VPAxx_ds)
275100061006     c                   add       1             xw
275200061006     c     xw            occur     $VPAxx_ds
275300061006     c                   clear                   $VPAxx
275400061006e   1c                   ENDDO
275500060831     ***c                   CLEAR                   vpa1
275600060831     ***c                   CLEAR                   vpa2
275700060831     ***c                   CLEAR                   vpa3
275800060831     ***c                   CLEAR                   vpa4
275900060831     ***c                   CLEAR                   vpa5
276000060831     ***c                   CLEAR                   vpa6
276100060831     ***c                   CLEAR                   vpa7
276200060831     ***c                   CLEAR                   vpa8
276300060831     ***c                   CLEAR                   vpa9
276400060831     ***c                   CLEAR                   vpa10
276500060831     ***c                   CLEAR                   vpa11
276600060831     ***c                   CLEAR                   vpa12
276700060831     ***c                   CLEAR                   vpa13
276800060831     ***c                   CLEAR                   vpa14
276900060831     ***c                   CLEAR                   vpa15
277000060831     ***c                   CLEAR                   vpa16
277100060831     ***c                   CLEAR                   vpa17
277200060831     ***c                   CLEAR                   vpa18
277300060831     ***c                   CLEAR                   vpa19
277400060831     ***c                   CLEAR                   vpa20
277500060831     ***c                   CLEAR                   vpa21
277600060831     ***c                   CLEAR                   vpa22
277700060831     ***c                   CLEAR                   vpa23
277800060831     ***c                   CLEAR                   vpa24
277900060831     ***c                   CLEAR                   vpa25
278000060831     ***c                   CLEAR                   vpa26
278100060831     ***c                   CLEAR                   vpa27
278200060831     ***c                   CLEAR                   vpa28
278300060831     ***c                   CLEAR                   vpa29
278400060831     ***c                   CLEAR                   vpa30
278500060831     ***c                   CLEAR                   vpa31
278600060831     ***c                   CLEAR                   vpa32
278700060831     ***c                   CLEAR                   vpa33
278800060831     ***c                   CLEAR                   vpa34
278900060831     ***c                   CLEAR                   vpa35
279000060831     ***c                   CLEAR                   vpa36
279100060831     ***c                   CLEAR                   vpa37
279200060831     ***c                   CLEAR                   vpa38
279300060831     ***c                   CLEAR                   vpa39
279400060831     ***c                   CLEAR                   vpa40
279500060831     ***c                   CLEAR                   vpa41
279600060831     ***c                   CLEAR                   vpa42
279700060831     ***c                   CLEAR                   vpa43
279800060831     ***c                   CLEAR                   vpa44
279900060831     ***c                   CLEAR                   vpa45
280000060831     ***c                   CLEAR                   vpa46
280100060831     ***c                   CLEAR                   vpa47
280200060831     ***c                   CLEAR                   vpa48
280300060831     ***c                   CLEAR                   vpa49
280400060831     ***c                   CLEAR                   vpa50
280500060831     ***c                   CLEAR                   vpa51
280600060831     ***c                   CLEAR                   vpa52
280700060831     ***c                   CLEAR                   vpa53
280800060831     ***c                   CLEAR                   vpa54
280900060831     ***c                   CLEAR                   vpa55
281000060831     ***c                   CLEAR                   vpa56
281100060831     ***c                   CLEAR                   vpa57
281200060831     ***c                   CLEAR                   vpa58
281300060831     ***c                   CLEAR                   vpa59
281400060831     ***c                   CLEAR                   vpa60
281500060831     c                   clear                   Vpnc                           *di calcolo
281600060831     c                   clear                   Vppnc
281700060831     c                   clear                   Vpsp
281800060831     c                   clear                   Vpesind                        *per statistiche
281900060831     c                   clear                   Vpussor
282000060831     c                   clear                   Vpuslos
282100060831     c                   clear                   Vpusnes
282200060831     c                   clear                   Vldv
282300060831     c                   clear                   Vchi
282400060831     c                   clear                   Vbar
282500060831     c                   clear                   VbarP
282600060831     c                   clear                   Vcsd
282700060831     c                   clear                   Vps2dok
282800060831     c                   clear                   Vccv
282900060831     c                   clear                   Vccp
283000060831     c                   clear                   Vnls
283100060831     c                   clear                   Vnlp
283200060831     c                   clear                   Vxnomis
283300060831     c                   clear                   Vxnopes
283400060831     c                   clear                   Vodsdna
283500060831     c                   clear                   Verrmac
283600060608     c                   clear                   Vpesind2
283700060608     c                   clear                   Vpussor2
283800060608     c                   clear                   Vodsdna2
283900060608     c                   clear                   Verrmac2
284000060831     c                   clear                   xg
284100060831     c                   clear                   xv
284200060831     c                   clear                   MEMxv
284300061006     c*** dopo:          clear                   WnclOb                         *colli obiettivo
284400060904     c                   eval      xi        = 1
284500060904     c                   eval      xj        = 1
284600060904     c                   eval      pj        = 1
284700060904     c                   eval      pk        = 1
284800060904     c                   eval      ppj       = 1
284900060904     c                   eval      ppk       = 1
285000060831      *
285100060831      * Ciclo di lettura GRIGIA  - - - - - - - - - - - - - - - - - - -*
285200060831      *
285300060831do  1c                   DO        MEMxg         xg
285400060830      *
285500080909      /free
285600080909       // - TROPPI dati !!!
285700080909           if  xv >= %elem($Vtip);
285800080909             $TooManyRec = *on;
285900080909             $FNMSA      = *off;
286000080909             leavesr;
286100080909           endif;
286200080909      /end-free
286300060830      * imposta già alcuni dati
286400060831     c                   add       1             xv                             *indice VIDEO
286500060830      * - tipo griglia
286600080116     c                   eval      $Vtip(xv) = $Gtip(xg)
286700060830sel 2c                   select
286800060830      * - - dettaglio
286900080116w   2c                   when           $Gtip(xg) = 'D'
287000080116     c                             or   $Gtip(xg) = 'M'
287100060830     c                   clear                   $Vdes(xv)
287200060830      * - - riepilogo
287300080116if  2c                   when      $Gtip(xg) = 'R'
287400060830     c                   eval      $Vdes(xv) = $Dvid(3)
287500060830      * - - totale generale
287600080116if  2c                   when      $Gtip(xg) = 'G'
287700060830     c                   eval      $Vdes(xv) = $Dvid(4)
287800060830e   2c                   endsl
287900060830      * - data lettura (no in totale generale)
288000080116if  2c                   IF        $Gtip(xg) <>'G'
288100060929     c                   reset                   WLBdat
288200080116     c                   eval      G08inv    = $Gdtl(xg)
288300060830     c                   call      'XSRDA8'
288400060901     c                   parm                    WLBdat
288500060901     c                   eval      $Vdtl(xv) = G08dat                           *data lettura
288600080116     c                   eval      $Vorl(xv) = $Gorl(xg)                        *ora  lettura
288700060830x   2c                   ELSE
288800060830     c                   clear                   $Vdtl(xv)
288900060830     c                   clear                   $Vorl(xv)
289000060830e   2c                   ENDIF
289100060831      *
289200060831      * - imposta data + ora griglia
289300080116     c                   movel     $Gdtl(xg)     LperGri
289400080116     c                   move      $Gorl(xg)     LperGri
289500030328      *
289600060831      * - imposta data + ora griglia
289700030328      *   per i calcoli sul giorno lavorativo precedente
289800030328     c                   exsr      X_lPerGriP
289900060831      *
290000060831sel 2c                   SELECT
290100060831      *
290200060831      * - valorizza riga DETTAGLIO minimo  - - - - - - - - - - - - - -*
290300080116w   2c                   WHEN      $Gtip(xg) = 'M'
290400060831do  3c     xi            DO        ULTmsa        xx
290500060831     c                   movel     $dtl(xx)      LperMem
290600060831     c                   move      $orl(xx)      LperMem
290700080116if  4c                   IF             $Gdtl(xg) = $dtl(xx)
290800080116     c                             and  $Gorl(xg) = $orl(xx)
290900080116     c                   eval      xm = xx
291000060831     c                   exsr      AddScVid
291100060831     c                   leave
291200000000x   4c                   ELSE
291300080116if  5c                   if        LperMem   > LperGri
291400060831     c                   leave
291500060831e   5c                   endif
291600000000e   4c                   ENDIF
291700000000e   3c                   ENDDO
291800080116     c                   eval      xi  = xx
291900060831      *
292000060831      * - valorizza riga DETTAGLIO sintetico - - - - - - - - - - - - -*
292100080116w   2c                   WHEN      $Gtip(xg) = 'D'
292200060831do  3c     xi            DO        ULTmsa        xx
292300060831     c                   movel     $dtl(xx)      LperMem
292400060831     c                   move      $orl(xx)      LperMem
292500080116if  4c                   IF        LperMem   > *zeros
292600080116if  5c                   if        LperMem  <= LperGri
292700080116     c                   eval      xm = xx
292800060831     c                   exsr      AddScVid
292900060831x   5c                   else
293000060831     c                   leave
293100060831e   5c                   endif
293200000000e   4c                   ENDIF
293300000000e   3c                   ENDDO
293400080116     c                   eval      xi  = xx
293500060831      *
293600060831      * - valorizza riga RIEPILOGO - - - - - - - - - - - - - - - - - -*
293700080116w   2c                   WHEN      $Gtip(xg) = 'R'
293800060831do  3c     xj            DO        ULTmsa        xx
293900060831     c                   movel     $dtl(xx)      LperMem
294000060831     c                   move      $orl(xx)      LperMem
294100080116if  4c                   IF        LperMem   > *zeros
294200080116if  5c                   if        LperMem  <= LperGri
294300080116     c                   eval      xm = xx
294400060831     c                   exsr      AddScVid                                     *aggiorna elemento
294500060831x   5c                   else
294600060831     c                   leave
294700060831e   5c                   endif
294800000000e   4c                   ENDIF
294900000000e   3c                   ENDDO
295000080116     c                   eval      xj  = xx
295100060831      *
295200060831      * - valorizza riga GENERALE  - - - - - - - - - - - - - - - - - -*
295300080116w   2c                   WHEN      $Gtip(xg) = 'G'
295400060831do  3c                   DO        ULTmsa        xx
295500080116     c                   eval      xm = xx
295600060831     c                   exsr      AddScVid                                     *aggiorna elemento
295700000000e   3c                   ENDDO
295800061006     c                   clear                   WnclOb                         *colli obiettivo
295900060904     c*** NO banco 20:   do        20            xw
296000061006     c                   do        19            xw
296100060904     c     xw            occur     $VMLAVxx_ds
296200080116     c                   eval      WnclOb    = WnclOb                           *colli obiettivo
296300080116     c                                       + (§KPIpba * $VMLAVxx(xv))
296400061002e   3c                   enddo
296500060831      *
296600060831e   2c                   ENDSL
296700060831      *
296800060831      * calcola il progressivo pacchi DETTAGLIO  - - - - - - - - - - -*
296900060831      *
297000080116if  2c                   IF             $Gtip(xg) = 'M'                         *minimo o sintetico
297100080116     c                             or   $Gtip(xg) = 'D'
297200060831      *
297300060831      * - corrente
297400060831do  3c                   DO        ULTmsa        xx
297500060831     c                   movel     $dtl(xx)      LperMem
297600060831     c                   move      $orl(xx)      LperMem
297700080116if  4c                   if        LperMem   > *zeros
297800080116if  5c                   if        LperMem  <= LperGri
297900060831     ***c                   eval      xm       = xx
298000060831     ***c                   exsr      AddScVid2
298100060831     c                   add       $ncl(xx)      Vpnc(xv)                       *progressivo colli
298200060831x   5c                   else
298300060831     c                   leave
298400060831e   5c                   endif
298500060831e   4c                   endif
298600000000e   3c                   ENDDO
298700060831      * - precedente
298800060831do  3c                   DO        ULTmsaPR      xx
298900060831     c                   movel     $Pdtl(xx)     LperMem
299000060831     c                   move      $Porl(xx)     LperMem
299100080116if  4c                   if        LperMem   > *zeros
299200080116if  5c                   if        LperMem  <= LperGriP
299300060831     ***c                   eval      xm       = xx
299400060831     ***c                   exsr      AddScVid3
299500060831     c                   add       $Pncl(xx)     VPpnc(xv)                     *progr.colli PREC.
299600060831x   5c                   else
299700060831     c                   leave
299800060831e   5c                   endif
299900060831e   4c                   endif
300000000000e   3c                   ENDDO
300100060831      *
300200000000e   2c                   ENDIF
300300060831      *
300400060831      * calcola il progressivo pacchi RIEPILOGO  - - - - - - - - - - -*
300500060831      *
300600080116if  2c                   IF        $Gtip(xg) = 'R'
300700060831      * - corrente
300800060831do  3c     Pj            DO        ULTmsa        xx
300900060831     c                   movel     $dtl(xx)      LperMem
301000060831     c                   move      $orl(xx)      LperMem
301100080116if  4c                   if        LperMem   > *zeros
301200080116if  5c                   if        LperMem  <= LperGri
301300060831     ***c                   eval      xm       = xx
301400060831     ***c                   exsr      AddScVid2
301500060831     c                   add       $ncl(xx)      Vpnc(xv)                       *progressivo colli
301600060831x   5c                   else
301700060831     c                   leave
301800060831e   5c                   endif
301900060831e   4c                   endif
302000000000e   3c                   ENDDO
302100080116     c                   eval      Pj = xx
302200060831      * - precedente
302300060831do  3c     PPj           DO        ULTmsaPR      xx
302400060831     c                   movel     $Pdtl(xx)     LperMem
302500060831     c                   move      $Porl(xx)     LperMem
302600080116if  4c                   if        LperMem   > *zeros
302700080116if  5c                   if        LperMem  <= LperGriP
302800060831     ***c                   eval      xm       = xx
302900060831     ***c                   exsr      AddScVid3
303000060831     c                   add       $Pncl(xx)     VPpnc(xv)                     *progr.colli PREC.
303100060831x   5c                   else
303200060831     c                   leave
303300060831e   5c                   endif
303400060831e   4c                   endif
303500000000e   3c                   ENDDO
303600080116     c                   eval      PPj = xx
303700000000e   2c                   ENDIF
303800060831      *
303900060831      * calcola il progressivo pacchi GENERALE - - - - - - - - - - - -*
304000060831      *
304100080116if  2c                   IF        $Gtip(xg) = 'G'
304200060831      * - corrente
304300060831do  3c     Pk            DO        ULTmsa        xx
304400060831     c                   movel     $dtl(xx)      LperMem
304500060831     c                   move      $orl(xx)      LperMem
304600080116if  4c                   if        LperMem   > *zeros
304700080116if  5c                   if        LperMem  <= LperGri
304800060831     ***c                   eval      xm       = xx
304900060831     ***c                   exsr      AddScVid2
305000060831     c                   add       $ncl(xx)      Vpnc(xv)                       *progressivo colli
305100060831x   5c                   else
305200060831     c                   leave
305300060831e   5c                   endif
305400060831e   4c                   endif
305500000000e   3c                   ENDDO
305600080116     c                   eval      Pk  = xx
305700060831      * - precedente
305800060831do  3c     PPk           DO        ULTmsaPR      xx
305900060831     c                   movel     $Pdtl(xx)     LperMem
306000060831     c                   move      $Porl(xx)     LperMem
306100080116if  4c                   if        LperMem   > *zeros
306200080116if  5c                   if        LperMem  <= LperGriP
306300060831     ***c                   eval      xm       = xx
306400060831     ***c                   exsr      AddScVid3
306500060831     c                   add       $Pncl(xx)     VPpnc(xv)                     *progr.colli PREC.
306600060831x   5c                   else
306700060831     c                   leave
306800060831e   5c                   endif
306900060831e   4c                   endif
307000000000e   3c                   ENDDO
307100080116     c                   eval      PPk = xx
307200000000e   2c                   ENDIF
307300060831      *
307400060831e   1c                   ENDDO
307500060831      *
307600060831      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
307700060831      *
307800060831     c                   eval      MEMxv    = xv
307900060831      *
308000060831      * calcola la % scostamento DETTAGLIO / RIEPILOGO / GENERALE  - -*
308100060831      *
308200060831do  1c                   DO        MEMxv         xv
308300060831if  2c                   if             Vpnc(xv)  > *zeros
308400060831     c                             or   VPpnc(xv) > *zeros
308500061002     c                   eval      §PNCatt  = Vpnc(xv)                          *prg colli attuali
308600061002     c                   eval      §PNCpre  = VPpnc(xv)                         *prg colli attuali
308700061002     c                   clear                   §PNCpsp                        *% scostamento
308800060831     c                   exsr      Scostam                                      *CALCOLA PERCENTUALE
308900061002     c                   eval      Vpsp(xv) = §PNCpsp                           *% scostamento
309000060831e   2c                   endif
309100060831e   1c                   ENDDO
309200060831      *
309300000000     c                   ENDSR
309400060831
309500060831      *---------------------------------------------------------------*
309600060831      *?Imposta il giorno e l'orario limite per confronto            ?*
309700060831      *?in base a quelli - limite - in esame                         ?*
309800060831      *---------------------------------------------------------------*
309900030328     c     X_lPerGriP    BEGSR
310000030328      *
310100060831     c                   clear                   LperGriP
310200060831     c                   movel     $Pdtl(1)      LperMem
310300060831     c                   move      $Porl(1)      LperMem
310400030328      *
310500080116sel 1c                   SELECT
310600080116      *
310700080116      * se non ho giorni precedenti => ripulisco
310800080116w   1c                   WHEN      $Pdtl(1)   = *zeros
310900080116     c                   clear                   LperGriP
311000080116      *
311100030328      * se sto elaborando la data di "partenza" (prima di mezzanotte)
311200080116w   1c                   WHEN      $Gdtl(xg)  = $Gdtl(1)
311300080116      *  => considero la prima data di confronto disponibile
311400080116      *     (del 1° elemento) SE PRECEDENTE !!!
311500080116if  2c                   if        $Pdtl(1)   < $Gdtl(1)
311600080116     c                   movel     $Pdtl(1)      LperGriP
311700080116x   2c                   else
311800080116      *  => calcolo la data precedente a quella nel 1° elemento
311900080116      *     SE UGUALE !!!
312000080116     c                   move      $Pdtl(1)      WdateISO
312100080116     c                   subdur    1:*d          WdateISO
312200080116     c                   move      WdateISO      n8
312300080116     c                   movel     n8            LperGriP
312400080116e   2c                   endif
312500080116     c                   move      $Gorl(xg)     LperGriP
312600080116      *
312700030328      * se sto elaborando la data successiva    (dopo  la mezzanotte)
312800080116x   1c                   OTHER
312900030328      *  => considero la data successiva alla prima data di confronto
313000080116      *     (del 1° elemento) SE PRECEDENTE !!!
313100080116if  2c                   if        $Pdtl(1)   < $Gdtl(1)
313200060831     c                   move      $Pdtl(1)      WdateISO
313300030328     c                   adddur    1:*d          WdateISO
313400030328     c                   move      WdateISO      n8
313500060831     c                   movel     n8            LperGriP
313600080116x   2c                   else
313700080116      *  => mantengo la prima data di confronto
313800080116     c                   move      $Pdtl(1)      WdateISO
313900080116     c                   move      WdateISO      n8
314000080116     c                   movel     n8            LperGriP
314100080116e   2c                   endif
314200080116     c                   move      $Gorl(xg)     LperGriP
314300080116      *
314400080116e   1c                   ENDSL
314500030328      *
314600030328     c                   ENDSR
314700060830
314800060830      *---------------------------------------------------------------*
314900060831      *?Aggiornamento schiere video (1^ parte)                       ?*
315000060830      *---------------------------------------------------------------*
315100060830     c     AddScVid      BEGSR
315200060830      *
315300061002     c                   add       $NCL(xm)      $Vncl(xv)                      *colli
315400060830     c                   add       1             $VminLav(xv)                   *mm lavorati totali
315500060831      * Entrate
315600060831     c     xm            occur     $PEPxx_ds                                    *entrate
315700060904     c                   clear                   xw
315800080116do  1c                   DOW       xw         < %elem($VPExx_ds)
315900060904     c                   add       1             xw
316000060904     c     xw            occur     $VPExx_ds
316100060904     c                   add       $PEPxx(xw)    $VPExx(xv)                     *colli x entrata/min
316200060904if  2c                   if        $PEPxx(xw) > *zeros
316300060904     c     xw            occur     $VMLAVxx_ds
316400060831     c                   add       1             $VMLAVxx(xv)                   *mm lavorati x entr.
316500060831e   2c                   endif
316600060831e   1c                   ENDDO
316700060831     ***c                   ADD       mpe1(xm)      vpe1(xv)
316800060831     ***c                   IF        mpe1(xm) > *zeros
316900060831     ***c                   ADD       1             vmlav01(xv)
317000060831     ***c                   ENDIF
317100060831     ***c                   ADD       mpe2(xm)      vpe2(xv)
317200060831     ***c                   IF        mpe2(xm) > *zeros
317300060831     ***c                   ADD       1             vmlav02(xv)
317400060831     ***c                   ENDIF
317500060831     ***c                   ADD       mpe3(xm)      vpe3(xv)
317600060831     ***c                   IF        mpe3(xm) > *zeros
317700060831     ***c                   ADD       1             vmlav03(xv)
317800060831     ***c                   ENDIF
317900060831     ***c                   ADD       mpe4(xm)      vpe4(xv)
318000060831     ***c                   IF        mpe4(xm) > *zeros
318100060831     ***c                   ADD       1             vmlav04(xv)
318200060831     ***c                   ENDIF
318300060831     ***c                   ADD       mpe5(xm)      vpe5(xv)
318400060831     ***c                   IF        mpe5(xm) > *zeros
318500060831     ***c                   ADD       1             vmlav05(xv)
318600060831     ***c                   ENDIF
318700060831     ***c                   ADD       mpe6(xm)      vpe6(xv)
318800060831     ***c                   IF        mpe6(xm) > *zeros
318900060831     ***c                   ADD       1             vmlav06(xv)
319000060831     ***c                   ENDIF
319100060831     ***c                   ADD       mpe7(xm)      vpe7(xv)
319200060831     ***c                   IF        mpe7(xm) > *zeros
319300060831     ***c                   ADD       1             vmlav07(xv)
319400060831     ***c                   ENDIF
319500060831     ***c                   ADD       mpe8(xm)      vpe8(xv)
319600060831     ***c                   IF        mpe8(xm) > *zeros
319700060831     ***c                   ADD       1             vmlav08(xv)
319800060831     ***c                   ENDIF
319900060831     ***c                   ADD       mpe9(xm)      vpe9(xv)
320000060831     ***c                   IF        mpe9(xm) > *zeros
320100060831     ***c                   ADD       1             vmlav09(xv)
320200060831     ***c                   ENDIF
320300060831     ***c                   ADD       mpe0(xm)      vpe0(xv)
320400060831     ***c                   IF        mpe0(xm) > *zeros
320500060831     ***c                   ADD       1             vmlav10(xv)
320600060831     ***c                   ENDIF
320700060831     ***c                   ADD       mpe11(xm)     vpe11(xv)
320800060831     ***c                   IF        mpe11(xm) > *zeros
320900060831     ***c                   ADD       1             vmlav11(xv)
321000060831     ***c                   ENDIF
321100060831     ***c                   ADD       mpe12(xm)     vpe12(xv)
321200060831     ***c                   IF        mpe12(xm) > *zeros
321300060831     ***c                   ADD       1             vmlav12(xv)
321400060831     ***c                   ENDIF
321500060831     ***c                   ADD       mpe13(xm)     vpe13(xv)
321600060831     ***c                   IF        mpe13(xm) > *zeros
321700060831     ***c                   ADD       1             vmlav13(xv)
321800060831     ***c                   ENDIF
321900060831     ***c                   ADD       mpe14(xm)     vpe14(xv)
322000060831     ***c                   IF        mpe14(xm) > *zeros
322100060831     ***c                   ADD       1             vmlav14(xv)
322200060831     ***c                   ENDIF
322300060831     ***c                   ADD       mpe15(xm)     vpe15(xv)
322400060831     ***c                   IF        mpe15(xm) > *zeros
322500060831     ***c                   ADD       1             vmlav15(xv)
322600060831     ***c                   ENDIF
322700060831     ***c                   ADD       mpe16(xm)     vpe16(xv)
322800060831     ***c                   IF        mpe16(xm) > *zeros
322900060831     ***c                   ADD       1             vmlav16(xv)
323000060831     ***c                   ENDIF
323100060831     ***c                   ADD       mpe17(xm)     vpe17(xv)
323200060831     ***c                   IF        mpe17(xm) > *zeros
323300060831     ***c                   ADD       1             vmlav17(xv)
323400060831     ***c                   ENDIF
323500060831     ***c                   ADD       mpe18(xm)     vpe18(xv)
323600060831     ***c                   IF        mpe18(xm) > *zeros
323700060831     ***c                   ADD       1             vmlav18(xv)
323800060831     ***c                   ENDIF
323900060831     ***c                   ADD       mpe19(xm)     vpe19(xv)
324000060831     ***c                   IF        mpe19(xm) > *zeros
324100060831     ***c                   ADD       1             vmlav19(xv)
324200060831     ***c                   ENDIF
324300060831     ***c                   ADD       mpe20(xm)     vpe20(xv)
324400060831     ***c                   IF        mpe20(xm) > *zeros
324500060831     ***c                   ADD       1             vmlav20(xv)
324600060831     ***c                   ENDIF
324700060831      * Uscite
324800060904     c     xm            occur     $PADxx_ds
324900060904     c     xv            occur     $VPAxx_ds
325000060904     c                   clear                   xw
325100080116do  1c                   dow       xw         < %elem($PADxx)
325200060904     c                   add       1             xw
325300060904     c                   add       $PADxx(xw)    $VPAxx(xw)
325400060831e   1c                   enddo
325500060831     ***c                   ADD       mpa1(xm)      vpa1(xv)                       *uscite
325600060831     ***c                   ADD       mpa2(xm)      vpa2(xv)
325700060831     ***c                   ADD       mpa3(xm)      vpa3(xv)
325800060831     ***c                   ADD       mpa4(xm)      vpa4(xv)
325900060831     ***c                   ADD       mpa5(xm)      vpa5(xv)
326000060831     ***c                   ADD       mpa6(xm)      vpa6(xv)
326100060831     ***c                   ADD       mpa7(xm)      vpa7(xv)
326200060831     ***c                   ADD       mpa8(xm)      vpa8(xv)
326300060831     ***c                   ADD       mpa9(xm)      vpa9(xv)
326400060831     ***c                   ADD       mpa10(xm)     vpa10(xv)
326500060831     ***c                   ADD       mpa11(xm)     vpa11(xv)
326600060831     ***c                   ADD       mpa12(xm)     vpa12(xv)
326700060831     ***c                   ADD       mpa13(xm)     vpa13(xv)
326800060831     ***c                   ADD       mpa14(xm)     vpa14(xv)
326900060831     ***c                   ADD       mpa15(xm)     vpa15(xv)
327000060831     ***c                   ADD       mpa16(xm)     vpa16(xv)
327100060831     ***c                   ADD       mpa17(xm)     vpa17(xv)
327200060831     ***c                   ADD       mpa18(xm)     vpa18(xv)
327300060831     ***c                   ADD       mpa19(xm)     vpa19(xv)
327400060831     ***c                   ADD       mpa20(xm)     vpa20(xv)
327500060831     ***c                   ADD       mpa21(xm)     vpa21(xv)
327600060831     ***c                   ADD       mpa22(xm)     vpa22(xv)
327700060831     ***c                   ADD       mpa23(xm)     vpa23(xv)
327800060831     ***c                   ADD       mpa24(xm)     vpa24(xv)
327900060831     ***c                   ADD       mpa25(xm)     vpa25(xv)
328000060831     ***c                   ADD       mpa26(xm)     vpa26(xv)
328100060831     ***c                   ADD       mpa27(xm)     vpa27(xv)
328200060831     ***c                   ADD       mpa28(xm)     vpa28(xv)
328300060831     ***c                   ADD       mpa29(xm)     vpa29(xv)
328400060831     ***c                   ADD       mpa30(xm)     vpa30(xv)
328500060831     ***c                   ADD       mpa31(xm)     vpa31(xv)
328600060831     ***c                   ADD       mpa32(xm)     vpa32(xv)
328700060831     ***c                   ADD       mpa33(xm)     vpa33(xv)
328800060831     ***c                   ADD       mpa34(xm)     vpa34(xv)
328900060831     ***c                   ADD       mpa35(xm)     vpa35(xv)
329000060831     ***c                   ADD       mpa36(xm)     vpa36(xv)
329100060831     ***c                   ADD       mpa37(xm)     vpa37(xv)
329200060831     ***c                   ADD       mpa38(xm)     vpa38(xv)
329300060831     ***c                   ADD       mpa39(xm)     vpa39(xv)
329400060831     ***c                   ADD       mpa40(xm)     vpa40(xv)
329500060831     ***c                   ADD       mpa41(xm)     vpa41(xv)
329600060831     ***c                   ADD       mpa42(xm)     vpa42(xv)
329700060831     ***c                   ADD       mpa43(xm)     vpa43(xv)
329800060831     ***c                   ADD       mpa44(xm)     vpa44(xv)
329900060831     ***c                   ADD       mpa45(xm)     vpa45(xv)
330000060831     ***c                   ADD       mpa46(xm)     vpa46(xv)
330100060831     ***c                   ADD       mpa47(xm)     vpa47(xv)
330200060831     ***c                   ADD       mpa48(xm)     vpa48(xv)
330300060831     ***c                   ADD       mpa49(xm)     vpa49(xv)
330400060831     ***c                   ADD       mpa50(xm)     vpa50(xv)
330500060831     ***c                   ADD       mpa51(xm)     vpa51(xv)
330600060831     ***c                   ADD       mpa52(xm)     vpa52(xv)
330700060831     ***c                   ADD       mpa53(xm)     vpa53(xv)
330800060831     ***c                   ADD       mpa54(xm)     vpa54(xv)
330900060831     ***c                   ADD       mpa55(xm)     vpa55(xv)
331000060831     ***c                   ADD       mpa56(xm)     vpa56(xv)
331100060831     ***c                   ADD       mpa57(xm)     vpa57(xv)
331200060831     ***c                   ADD       mpa58(xm)     vpa58(xv)
331300060831     ***c                   ADD       mpa59(xm)     vpa59(xv)
331400060831     ***c                   ADD       mpa60(xm)     vpa60(xv)
331500060831      * per statistiche
331600060831     c                   add       $pesind(xm)   Vpesind(xv)
331700060831     c                   add       $pussor(xm)   Vpussor(xv)
331800060831     c                   add       $puslos(xm)   Vpuslos(xv)
331900060831     c                   add       $pusnes(xm)   Vpusnes(xv)
332000060831     c                   add       $csd(xm)      Vcsd(xv)
332100060831     c                   add       $ps2dok(xm)   Vps2dok(xv)
332200060831     c                   add       $ccv(xm)      Vccv(xv)
332300060831     c                   add       $ccp(xm)      Vccp(xv)
332400060831     c                   add       $nls(xm)      Vnls(xv)
332500060831     c                   add       $nlp(xm)      Vnlp(xv)
332600031224      * Barcode Chi sono
332700060831     c                   eval      Vchi(xv)   = Vchi(xv)
332800060831     c                                        + $nlp(xm)
332900060831     c                                        + $pusnes(xm)
333000060831     c                                        + $puslos(xm)
333100031224      * Barcode LdV x picking
333200060831     c                   eval      Vldv(xv)   = Vldv(xv)
333300060831     c                                        + $nlp(xm)
333400020521      * Barcode Bartolini
333500060831     c                   eval      Vbar(xv)   = Vbar(xv)
333600060831     c                                        + $pesind(xm)
333700060831     c                                        - $nlp(xm)
333800060831     c                                        - $pusnes(xm)
333900060831     c                                        - $puslos(xm)
334000031224      * Barcode Bartolini x picking
334100060831     c                   eval      VbarP(xv)  = VbarP(xv)
334200060831     c                                        + $pesind(xm)
334300060831     c                                        - $nlp(xm)
334400060831     c                                        - $puslos(xm)
334500020521      *
334600060831     c                   add       $xnomis(xm)   Vxnomis(xv)
334700060831     c                   add       $xnopes(xm)   Vxnopes(xv)
334800060831     c                   add       $odsdna(xm)   Vodsdna(xv)
334900060831     c                   add       $errmac(xm)   Verrmac(xv)
335000060608      *?*NEW?
335100060831     c                   add       $pesind2(xm)  Vpesind2(xv)
335200060831     c                   add       $pussor2(xm)  Vpussor2(xv)
335300060831     c                   add       $odsdna2(xm)  Vodsdna2(xv)
335400060831     c                   add       $errmac2(xm)  Verrmac2(xv)
335500060831      *
335600000000     c                   ENDSR
335700060831     ***
335800060831     *** *---------------------------------------------------------------*
335900060831     *** *?Aggiornamento schiere video (2^ parte)                       ?*
336000060831     *** *---------------------------------------------------------------*
336100060831     ***c     AddScVid2     BEGSR
336200060831     *** *
336300060831     ***c                   add       $ncl(xm)      Vpnc(xv)                      *progr.colli
336400060831     *** *
336500060831     ***c                   ENDSR
336600060831     ***
336700060831     *** *---------------------------------------------------------------*
336800060831     *** *?Aggiornamento schiere video (3^ parte)                       ?*
336900060831     *** *---------------------------------------------------------------*
337000060831     ***c     AddScVid3     BEGSR
337100060831     *** *
337200060831     ***c                   add       $pncl(xm)     Vppnc(xv)                     *progr.colli PREC.
337300060831     *** *
337400060831     ***c                   ENDSR
337500060831
337600060831      *---------------------------------------------------------------*
337700060831      *?Calcolo dei valori statistici                                ?*
337800060831      *---------------------------------------------------------------*
337900060901     c     sr_CalStVid   BEGSR
338000060831      *
338100060831      * Pulizia delle variabili di lavoro
338200060831     c                   clear                   MEMtco                         *ultimo turno
338300060831     c                   clear                   MEMtlm
338400060831     c                   clear                   MEMtcl
338500060831     c                   clear                   MEMtkp
338600060831     c                   clear                   MEMttm
338700060831     c                   clear                   MinLav
338800060831     c                   clear                   wMinLav
338900060831     c                   clear                   MinTra
339000060831     c                   clear                   MEMrco                         *ultimo riepilogo
339100060831     c                   clear                   MEMrlm
339200060831     c                   clear                   MEMrcl
339300060831     c                   clear                   MEMrkp
339400060831     c                   clear                   MEMrtm
339500060831     c                   clear                   MirLav
339600060831     c                   clear                   wMirLav
339700060901     c                   clear                   MirTra
339800060831      *
339900060831      * Progressivi ULTIMO TURNO
340000060831      *
340100060831      * Ciclo di elaborazione di tutti i dati di dettaglio memorizzati
340200060831do  1c                   do        ULTmsa        xi
340300060831      * - Pacchi
340400060831     c                   add       $ncl(xi)      MEMtco
340500060831      * - Minuti lavorati
340600060831     c                   add       1             MinLav
340700060831e   1c                   enddo
340800060831      *
340900060831      * Tempo lavorato (HH:MM)
341000060831     c                   eval      §min    = MinLav
341100060831     c                   clear                   §hh
341200060831     c                   clear                   §mm
341300060831     c                   exsr      CalHml
341400060831     c                   movel     §hh           MEMtlm
341500060831     c                   move      §mm           MEMtlm
341600060831      *
341700060831      * Pacchi / Tempo lavorato
341800060831if   c                   if        MinLav  = *zeros
341900060831     c                   eval      wMinLav = 1
342000060831x   1c                   else
342100060831     c                   eval      wMinLav = MinLav
342200060831e   1c                   endif
342300060831     c                   eval(h)   MEMtcl  = MEMtco / wMinLav
342400060831     c                   eval(h)   MEMtkp  = WnclOb / wMinLav
342500060831      *
342600060831      * Tempo trascorso
342700060901     c                   eval      n8      = $dtl(1)                            *data INIZIALE
342800060904     c                   eval      n6      = $orl(1) * 100                      *ora  INIZIALE
342900060831     c     *ISO          move      n8            Wdata
343000060831     c     *ISO          move      n6            Wora
343100060831     c                   movel     Wtempo        Wtempo1                        *periodo INIZIALE
343200060901     c                   eval      n8      = $dtl(ULTmsa)                       *data FINALE
343300060904     c                   eval      n6      = $orl(ULTmsa) * 100                 *ora  FINALE
343400060831     c     *ISO          move      n8            Wdata
343500060831     c     *ISO          move      n6            Wora
343600060831     c                   movel     Wtempo        Wtempo2                        *periodo FINALE
343700060831     c     Wtempo1       subdur    Wtempo2       MinTra:*mn                     *mm trascorsi totali
343800060831if  1c                   if        MinTra  < *zeros
343900060831     c                   eval      MinTra  = MinTra * (-1)
344000060831e   1c                   endif
344100060831      * Tempo trascorso (HH:MM)
344200060831     c                   eval      §min    = MinTra
344300060831     c                   clear                   §hh
344400060831     c                   clear                   §mm
344500060831     c                   exsr      CalHml
344600060831     c                   movel     §hh           MEMttm
344700060831     c                   move      §mm           MEMttm
344800060831      *
344900060831      * Progressivi ULTIMI MM RICHIESTI
345000060831      *
345100060831      * reperisce la data + ora corrente
345200060831     c                   time                    n14                            *Ora (6)+ Data (8)
345300060831     c                   movel     n14           n4                             *Ora (4)
345400060831     c                   move      n14           n8                             *Data (8) in g/m/a
345500060831     c                   clear                   WLBdat
345600060831     c                   eval      G08dat  = n8
345700060831     c                   call      'XSRDA8'
345800060831     c                   parm                    WLBdat
345900060831     c                   eval      n8      = G08inv                             *Data (8) in a/m/g
346000060831     c                   movel     n8            LperGri2
346100060831     c                   move      n4            LperGri2                       *periodo AL
346200060831      *
346300060831      * sottrae alla data + ora corrente i mm di riepilogo richiesti
346400060831     c                   eval      Lmin    = D00mtr
346500060831     c                   exsr      SubMinRie
346600060831     c                   movel     n8            LperGri
346700060831     c                   move      n4            LperGri                        *periodo DAL
346800060831      *
346900060831      * legge i dati memorizzati del periodo calcolato (data+ora corrente - 30 mm)
347000060831do  1c                   DO        ULTmsa        xi
347100060831     c                   movel     $dtl(xi)      LperMem                        *periodo memorizzato
347200060831     c                   move      $orl(xi)      LperMem
347300060831if  2c                   IF             LperMem >= LperGri
347400060831     c                             and  LperMem <= LperGri2
347500060831      * - Pacchi
347600060831     c                   add       $ncl(xi)      MEMrco
347700060831      * - Minuti lavorati
347800060831     c                   add       1             MirLav
347900060831e   2c                   endif
348000060831e   1c                   enddo
348100060831      *
348200060831      * Tempo lavorato (HH:MM)
348300060831     c                   eval      §min    = MirLav
348400060831     c                   clear                   §hh
348500060831     c                   clear                   §mm
348600060831     c                   exsr      CalHMl
348700060831     c                   movel     §hh           MEMrlm
348800060831     c                   move      §mm           MEMrlm
348900060831      *
349000060831      * Pacchi / Tempo lavorato
349100060831if   c                   if        MirLav  = *zeros
349200060831     c                   eval      wMirLav = 1
349300060831x   1c                   else
349400060831     c                   eval      wMirLav = MirLav
349500060831e   1c                   endif
349600061006     c                   eval(h)   MEMrcl  = MEMrco / wMirLav
349700061006     c*** come MEMTKP:   eval(h)   MEMrkp  = WnclOb / wMirLav
349800060831      *
349900060831      * Tempo trascorso
350000060831     c                   move      D00mtr        MEMrtm
350100060901     c                   eval      MirTra  = D00mtr
350200060901      * Tempo trascorso (HH:MM)
350300060901     c                   eval      §min    = MirTra
350400060901     c                   clear                   §hh
350500060901     c                   clear                   §mm
350600060901     c                   exsr      CalHml
350700060901     c                   movel     §hh           MEMrtm
350800060901     c                   move      §mm           MEMrtm
350900060831      *
351000060831     c                   ENDSR
351100060831
351200060831      *---------------------------------------------------------------*
351300060831      *?Calcolo della percentuale di scostamento                     ?*
351400060831      *?INPUT  --> §pncatt - > "X" attuali                           ?*
351500060831      *?           §pncpre - > "X" precedenti                        ?*
351600060831      *?OUTPUT --> §pncpsp - > % di scostamento                      ?*
351700060831      *---------------------------------------------------------------*
351800060831     c     Scostam       BEGSR
351900060831      *
352000060831      *---
352100060831      * (attuali - precedenti) = gap
352200060831      * (gap x 100) : attuali  = % scostamento
352300060831      *---
352400060831      *
352500060831      * NB: se l'attuale è uguale a zero, lo imposta a uno per non dare errore
352600060929if  1c                   if        §PNCatt  = *zeros
352700060929     c                   eval      §PNCatt  = 1
352800060831e   1c                   endif
352900060901      *
353000060901      * Differenza in termini percentuali
353100060929     c                   eval      §PNCgap  = ((§PNCatt - §PNCpre)
353200060901     c                                        * 100)
353300060929     c                                        / §PNCatt
353400060929     c                   z-add     §PNCgap       §PNCpsp
353500060831      *
353600060901      * Sistemazione della % scostamento se più di 3 cifre
353700060831sel 1c                   select
353800060929w   1c                   when      §PNCgap >= +1000                              *$ positiva
353900060929     c                   eval      §PNCpsp  = *hival
354000060929w   2c                   when      §PNCgap <= -1000                              *% negativa
354100060929     c                   eval      §PNCpsp  = *loval
354200060831e   1c                   endsl
354300060831      *
354400060831     c                   ENDSR
354500060831
354600060831      *---------------------------------------------------------------*
354700060831      *?Calcola le ore e i minuti da minuti totali                   ?*
354800060831      *?INPUT  --> §min: minuti totali trascorsi                     ?*
354900060831      *?OUTPUT --> §gg:  giorni trascorsi                            ?*
355000060831      *?           §hh:  ore    trascorse                            ?*
355100060831      *?           §mm:  minuti trascorsi                            ?*
355200060831      *---------------------------------------------------------------*
355300060831     c     CalHml        BEGSR
355400060831      *
355500060831     c                   clear                   Wresto            5 0
355600060831if  1c                   if        §min  >= 1440                                *i mm formano gg
355700060831     c     §min          div       1440          §gg                            *GIORNI
355800060831     c                   mvr                     Wresto                         *ore/minuti di resto
355900060831x   1c                   else                                                   *i mm non formano gg
356000060831     c                   eval      Wresto = §min                                *ore/minuti di resto
356100060831e   1c                   endif
356200060831     c     Wresto        div       60            §hh                            *ORE
356300060831     c                   mvr                     §mm                            *MINUTI
356400060831      *
356500000000     c                   ENDSR
356600060831
356700060831      *---------------------------------------------------------------*
356800060831      *?Sottrae alla data + ora corrente i mm di riepilogo richiesti ?*
356900060831      *---------------------------------------------------------------*
357000060831     c     SubMinRie     BEGSR
357100060831      *
357200060831     c                   clear                   n6
357300060831     c                   movel     n4            n6
357400060831     c     *ISO          move      n8            Wdata
357500060831     c     *ISO          move      n6            Wora
357600060831     c                   subdur    Lmin:*MN      Wtempo
357700060831     c     *ISO          move      Wdata         n8
357800060831     c     *ISO          move      Wora          n6
357900060831     c                   movel     n6            n4
358000060831      *
358100060831     c                   ENDSR
358200060830
358300060830      *---------------------------------------------------------------*
358400060830
358500060830** - $DVID
358600000000Arrivi         1
358700000000Entrata        2
358800000000RIEP           3
358900000000GEN            4
359000060830               5
