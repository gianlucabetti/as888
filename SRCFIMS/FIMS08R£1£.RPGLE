000100100308     /*END
000200060830      *---------------------------------------------------------------*
000300060831      *?Letture VdL/CML - Visualizzazione e Stampa                   ?*
000400060830      *---------------------------------------------------------------*
000500060830
000600040316     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060830
000800060830      *---------------------------------------------------------------*
000900060830
001000170201     fFNMSA12L  if   e           k disk
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
003700060830      *
003800060830      * Entrate
003900060830     d $V3ent          s                   like(V3ent01) dim(20)  inz
004000060830     d $V3ent_ds       ds                  inz
004100060830     d   V3ent01                           inz
004200060830     d   V3ent02                           inz
004300060830     d   V3ent03                           inz
004400060830     d   V3ent04                           inz
004500060830     d   V3ent05                           inz
004600060830     d   V3ent06                           inz
004700060830     d   V3ent07                           inz
004800060830     d   V3ent08                           inz
004900060830     d   V3ent09                           inz
005000060830     d   V3ent10                           inz
005100060830     d   V3ent11                           inz
005200060830     d   V3ent12                           inz
005300060830     d   V3ent13                           inz
005400060830     d   V3ent14                           inz
005500060830     d   V3ent15                           inz
005600060830     d   V3ent16                           inz
005700060830     d   V3ent17                           inz
005800060830     d   V3ent18                           inz
005900060830     d   V3ent19                           inz
006000060830     d   V3ent20                           inz
006100060830      *
006200060830      * Colli per entrata
006300060830     d $V2SM_ds        ds                  inz
006400060830     d   V2Sm01                            inz
006500060830     d   V2Sm02                            inz
006600060830     d   V2Sm03                            inz
006700060830     d   V2Sm04                            inz
006800060830     d   V2Sm05                            inz
006900060830     d   V2Sm06                            inz
007000060830     d   V2Sm07                            inz
007100060830     d   V2Sm08                            inz
007200060830     d   V2Sm09                            inz
007300060830     d   V2Sm10                            inz
007400060830     d   V2Sm11                            inz
007500060830     d   V2Sm12                            inz
007600060830     d   V2Sm13                            inz
007700060830     d   V2Sm14                            inz
007800060830     d   V2Sm15                            inz
007900060830     d   V2Sm16                            inz
008000060830     d   V2Sm17                            inz
008100060830     d   V2Sm18                            inz
008200060830     d   V2Sm19                            inz
008300060830     d   V2Sm20                            inz
008400060929     d  $V2SM                  1     60s 0 dim(20)  inz
008500060904     d $DECm_ds        ds                  inz
008600060830     d   DECm01                            inz
008700060830     d   DECm02                            inz
008800060830     d   DECm03                            inz
008900060830     d   DECm04                            inz
009000060830     d   DECm05                            inz
009100060830     d   DECm06                            inz
009200060830     d   DECm07                            inz
009300060830     d   DECm08                            inz
009400060830     d   DECm09                            inz
009500060830     d   DECm10                            inz
009600060830     d   DECm11                            inz
009700060830     d   DECm12                            inz
009800060830     d   DECm13                            inz
009900060830     d   DECm14                            inz
010000060830     d   DECm15                            inz
010100060830     d   DECm16                            inz
010200060830     d   DECm17                            inz
010300060830     d   DECm18                            inz
010400060830     d   DECm19                            inz
010500060830     d   DECm20                            inz
010600060904     d  $DECm                  1     40s 0 dim(20)  inz
010700060904     d $DECe_ds        ds                  inz
010800060904     d   DECe01                            inz
010900060904     d   DECe02                            inz
011000060904     d   DECe03                            inz
011100060904     d   DECe04                            inz
011200060904     d   DECe05                            inz
011300060904     d   DECe06                            inz
011400060904     d   DECe07                            inz
011500060904     d   DECe08                            inz
011600060904     d   DECe09                            inz
011700060904     d   DECe10                            inz
011800060904     d   DECe11                            inz
011900060904     d   DECe12                            inz
012000060904     d   DECe13                            inz
012100060904     d   DECe14                            inz
012200060904     d   DECe15                            inz
012300060904     d   DECe16                            inz
012400060904     d   DECe17                            inz
012500060904     d   DECe18                            inz
012600060904     d   DECe19                            inz
012700060904     d   DECe20                            inz
012800060904     d  $DECe                  1    100s 0 dim(20)  inz
012900060830     d $V3DExx_ds      ds                  inz
013000060830     d   V3De01                            inz
013100060830     d   V3De02                            inz
013200060830     d   V3De03                            inz
013300060830     d   V3De04                            inz
013400060830     d   V3De05                            inz
013500060830     d   V3De06                            inz
013600060830     d   V3De07                            inz
013700060830     d   V3De08                            inz
013800060830     d   V3De09                            inz
013900060830     d   V3De10                            inz
014000060830     d   V3De11                            inz
014100060830     d   V3De12                            inz
014200060830     d   V3De13                            inz
014300060830     d   V3De14                            inz
014400060830     d   V3De15                            inz
014500060830     d   V3De16                            inz
014600060830     d   V3De17                            inz
014700060830     d   V3De18                            inz
014800060830     d   V3De19                            inz
014900060830     d   V3De20                            inz
015000060904     d  $V3DE                  1    100s 0 dim(20)  inz
015100060830      *
015200060830      * Media colli per entrata
015300060830     d $V3DMxx_ds      ds                  inz
015400060830     d   V3Dm01                            inz
015500060830     d   V3Dm02                            inz
015600060830     d   V3Dm03                            inz
015700060830     d   V3Dm04                            inz
015800060830     d   V3Dm05                            inz
015900060830     d   V3Dm06                            inz
016000060830     d   V3Dm07                            inz
016100060830     d   V3Dm08                            inz
016200060830     d   V3Dm09                            inz
016300060830     d   V3Dm10                            inz
016400060830     d   V3Dm11                            inz
016500060830     d   V3Dm12                            inz
016600060830     d   V3Dm13                            inz
016700060830     d   V3Dm14                            inz
016800060830     d   V3Dm15                            inz
016900060830     d   V3Dm16                            inz
017000060830     d   V3Dm17                            inz
017100060830     d   V3Dm18                            inz
017200060830     d   V3Dm19                            inz
017300060830     d   V3Dm20                            inz
017400060904     d  $V3DM                  1     40s 0 dim(20)  inz
017500060831      *
017600060831      * Schiere memorizzazione dati correnti
017700060831      * - Data
017800060929     d $DTL            s              8  0 dim($MaxEle)
017900060831      * - Orario
018000060929     d $ORL            s              4  0 dim($MaxEle)
018100060831      * - Totale colli
018200060929     d $NCL            s              5  0 dim($MaxEle)
018300060831      * - Dettaglio colli x entrata
018400060831     d $PEPxx_ds       ds                  occurs($MaxEle)  inz
018500060904     d   $PEPxx                       5  0 dim(20)          inz
018600060831     ***d Mpe1            s              5  0 dim($MaxEle)                         *entrata 1
018700060831     ***d Mpe2            s              5  0 dim($MaxEle)                         *entrata 2
018800060831     ***d Mpe3            s              5  0 dim($MaxEle)                         *entrata 3
018900060831     ***d Mpe4            s              5  0 dim($MaxEle)                         *entrata 4
019000060831     ***d Mpe5            s              5  0 dim($MaxEle)                         *entrata 5
019100060831     ***d Mpe6            s              5  0 dim($MaxEle)                         *entrata 6
019200060831     ***d Mpe7            s              5  0 dim($MaxEle)                         *entrata 7
019300060831     ***d Mpe8            s              5  0 dim($MaxEle)                         *entrata 8
019400060831     ***d Mpe9            s              5  0 dim($MaxEle)                         *entrata 9
019500060831     ***d Mpe0            s              5  0 dim($MaxEle)                         *entrata 10
019600060831     ***d Mpe11           s              5  0 dim($MaxEle)                         *entrata 1
019700060831     ***d Mpe12           s              5  0 dim($MaxEle)                         *entrata 2
019800060831     ***d Mpe13           s              5  0 dim($MaxEle)                         *entrata 3
019900060831     ***d Mpe14           s              5  0 dim($MaxEle)                         *entrata 4
020000060831     ***d Mpe15           s              5  0 dim($MaxEle)                         *entrata 5
020100060831     ***d Mpe16           s              5  0 dim($MaxEle)                         *entrata 6
020200060831     ***d Mpe17           s              5  0 dim($MaxEle)                         *entrata 7
020300060831     ***d Mpe18           s              5  0 dim($MaxEle)                         *entrata 8
020400060831     ***d Mpe19           s              5  0 dim($MaxEle)                         *entrata 9
020500060831     ***d Mpe20           s              5  0 dim($MaxEle)                         *entrata 10
020600060831      * - Dettaglio colli x uscita
020700060831     d $PADxx_ds       ds                  occurs($MaxEle)  inz
020800060904     d   $PADxx                       5  0 dim(60)          inz
020900060831     ***d Mpa1            s              5  0 dim($MaxEle)                         *uscita 1
021000060831     ***d Mpa2            s              5  0 dim($MaxEle)                         *uscita 2
021100060831     ***d Mpa3            s              5  0 dim($MaxEle)                         *uscita 3
021200060831     ***d Mpa4            s              5  0 dim($MaxEle)                         *uscita 4
021300060831     ***d Mpa5            s              5  0 dim($MaxEle)                         *uscita 5
021400060831     ***d Mpa6            s              5  0 dim($MaxEle)                         *uscita 6
021500060831     ***d Mpa7            s              5  0 dim($MaxEle)                         *uscita 7
021600060831     ***d Mpa8            s              5  0 dim($MaxEle)                         *uscita 8
021700060831     ***d Mpa9            s              5  0 dim($MaxEle)                         *uscita 9
021800060831     ***d Mpa10           s              5  0 dim($MaxEle)                         *uscita 10
021900060831     ***d Mpa11           s              5  0 dim($MaxEle)                         *uscita 11
022000060831     ***d Mpa12           s              5  0 dim($MaxEle)                         *uscita 12
022100060831     ***d Mpa13           s              5  0 dim($MaxEle)                         *uscita 13
022200060831     ***d Mpa14           s              5  0 dim($MaxEle)                         *uscita 14
022300060831     ***d Mpa15           s              5  0 dim($MaxEle)                         *uscita 15
022400060831     ***d Mpa16           s              5  0 dim($MaxEle)                         *uscita 16
022500060831     ***d Mpa17           s              5  0 dim($MaxEle)                         *uscita 17
022600060831     ***d Mpa18           s              5  0 dim($MaxEle)                         *uscita 18
022700060831     ***d Mpa19           s              5  0 dim($MaxEle)                         *uscita 19
022800060831     ***d Mpa20           s              5  0 dim($MaxEle)                         *uscita 20
022900060831     ***d Mpa21           s              5  0 dim($MaxEle)                         *uscita 21
023000060831     ***d Mpa22           s              5  0 dim($MaxEle)                         *uscita 22
023100060831     ***d Mpa23           s              5  0 dim($MaxEle)                         *uscita 23
023200060831     ***d Mpa24           s              5  0 dim($MaxEle)                         *uscita 24
023300060831     ***d Mpa25           s              5  0 dim($MaxEle)                         *uscita 25
023400060831     ***d Mpa26           s              5  0 dim($MaxEle)                         *uscita 26
023500060831     ***d Mpa27           s              5  0 dim($MaxEle)                         *uscita 27
023600060831     ***d Mpa28           s              5  0 dim($MaxEle)                         *uscita 28
023700060831     ***d Mpa29           s              5  0 dim($MaxEle)                         *uscita 29
023800060831     ***d Mpa30           s              5  0 dim($MaxEle)                         *uscita 30
023900060831     ***d Mpa31           s              5  0 dim($MaxEle)                         *uscita 31
024000060831     ***d Mpa32           s              5  0 dim($MaxEle)                         *uscita 32
024100060831     ***d Mpa33           s              5  0 dim($MaxEle)                         *uscita 33
024200060831     ***d Mpa34           s              5  0 dim($MaxEle)                         *uscita 34
024300060831     ***d Mpa35           s              5  0 dim($MaxEle)                         *uscita 35
024400060831     ***d Mpa36           s              5  0 dim($MaxEle)                         *uscita 36
024500060831     ***d Mpa37           s              5  0 dim($MaxEle)                         *uscita 37
024600060831     ***d Mpa38           s              5  0 dim($MaxEle)                         *uscita 38
024700060831     ***d Mpa39           s              5  0 dim($MaxEle)                         *uscita 39
024800060831     ***d Mpa40           s              5  0 dim($MaxEle)                         *uscita 40
024900060831     ***d Mpa41           s              5  0 dim($MaxEle)                         *uscita 41
025000060831     ***d Mpa42           s              5  0 dim($MaxEle)                         *uscita 42
025100060831     ***d Mpa43           s              5  0 dim($MaxEle)                         *uscita 43
025200060831     ***d Mpa44           s              5  0 dim($MaxEle)                         *uscita 44
025300060831     ***d Mpa45           s              5  0 dim($MaxEle)                         *uscita 45
025400060831     ***d Mpa46           s              5  0 dim($MaxEle)                         *uscita 46
025500060831     ***d Mpa47           s              5  0 dim($MaxEle)                         *uscita 47
025600060831     ***d Mpa48           s              5  0 dim($MaxEle)                         *uscita 48
025700060831     ***d Mpa49           s              5  0 dim($MaxEle)                         *uscita 49
025800060831     ***d Mpa50           s              5  0 dim($MaxEle)                         *uscita 50
025900060831     ***d Mpa51           s              5  0 dim($MaxEle)                         *uscita 51
026000060831     ***d Mpa52           s              5  0 dim($MaxEle)                         *uscita 52
026100060831     ***d Mpa53           s              5  0 dim($MaxEle)                         *uscita 53
026200060831     ***d Mpa54           s              5  0 dim($MaxEle)                         *uscita 54
026300060831     ***d Mpa55           s              5  0 dim($MaxEle)                         *uscita 55
026400060831     ***d Mpa56           s              5  0 dim($MaxEle)                         *uscita 56
026500060831     ***d Mpa57           s              5  0 dim($MaxEle)                         *uscita 57
026600060831     ***d Mpa58           s              5  0 dim($MaxEle)                         *uscita 58
026700060831     ***d Mpa59           s              5  0 dim($MaxEle)                         *uscita 59
026800060831     ***d Mpa60           s              5  0 dim($MaxEle)                         *uscita 60
026900060929      * per Statistiche
027000060831     d $pesind         s              5  0 dim($MaxEle)                         *caricati da
027100060831     d $pussor         s              5  0 dim($MaxEle)                         *smistati da
027200060831     d $puslos         s              5  0 dim($MaxEle)                         *smistati da
027300060831     d $pusnes         s              5  0 dim($MaxEle)                         *smistati da
027400060831     d $csd            s              5  0 dim($MaxEle)                         *letti da scanner
027500060831     d $ps2dok         s              5  0 dim($MaxEle)                         *letti da pistola
027600060831     d $ccv            s              5  0 dim($MaxEle)                         *letti con volume
027700060831     d $ccp            s              5  0 dim($MaxEle)                         *letti con peso
027800060831     d $nls            s              5  0 dim($MaxEle)                         *non letti scanner
027900060831     d $nlp            s              5  0 dim($MaxEle)                         *non letti pistola
028000060831     d $xnomis         s              5  0 dim($MaxEle)                         *C:non misurati
028100060831     d $xnopes         s              5  0 dim($MaxEle)                         *C:non pesati
028200060831     d $odsdna         s              5  0 dim($MaxEle)                         *of x uscita piena
028300060831     d $errmac         s              5  0 dim($MaxEle)                         *of x errori machine
028400060831     d $pesind2        s              5  0 dim($MaxEle)                         »caricati da
028500060831     d $pussor2        s              5  0 dim($MaxEle)                         »smistati da
028600060831     d $odsdna2        s              5  0 dim($MaxEle)                         »of x uscita piena
028700060831     d $errmac2        s              5  0 dim($MaxEle)                         »of x errori machine
028800060831      *
028900060929      * Schiere memorizzazione dati precedenti
029000060929      * - Data         - precedente
029100060831     d $Pdtl           s              8  0 dim($MaxEle)                         *PRECEDENTE data
029200060929      * - Orario       - precedente
029300060831     d $Porl           s              4  0 dim($MaxEle)                         *PRECEDENTE ora
029400060929      * - Totale colli - precedente
029500060831     d $Pncl           s              5  0 dim($MaxEle)                         *PRECEDENTE colli
029600060831      *
029700060929      * Schiere griglia
029800060929      * - Dettaglio
029900060831     d GDper           s             12  0 dim($MaxEle)
030000060929      * - Riepilogo
030100060831     d GRper           s             12  0 dim(500)
030200060929      * - Unione
030300080909     d $Gtip           s              1    dim($MaxEl2)  inz                    *tipo griglia
030400080909     d $Gdtl           s              8  0 dim($MaxEl2)  inz                    *data
030500080909     d $Gorl           s              4  0 dim($MaxEl2)  inz                    *ora
030600060830      *
030700060929      * Schiere video
030800060929      * - Descrizione
030900060830     d $Vdes           s                   like(V2Sdes)  dim($MaxEl2)  inz
031000060929      * - Tipo Riga
031100060830     d $Vtip           s              1    dim($MaxEl2)  inz
031200060929      * - Data
031300060830     d $Vdtl           s              8  0 dim($MaxEl2)  inz
031400060929      * - Orario
031500060901     d $Vorl           s              4  0 dim($MaxEl2)  inz
031600060929      * - Numero colli
031700060830     d $Vncl           s              5  0 dim($MaxEl2)  inz
031800060929      * - Minuti lavorati
031900060830     d $VminLav        s              4  0 dim($MaxEl2)  inz
032000060830      *
032100060830      * Entrate
032200060830     d $VPExx_ds       ds                  occurs(20)    inz
032300060904     d   $VPExx                            like(V3De01)  dim($MaxEl2)  inz
032400060830     ***d vpe1            S              5  0 DIM(6000)                            *entrata 1
032500060830     ***d vpe2            S              5  0 DIM(6000)                            *entrata 2
032600060830     ***d vpe3            S              5  0 DIM(6000)                            *entrata 3
032700060830     ***d vpe4            S              5  0 DIM(6000)                            *entrata 4
032800060830     ***d vpe5            S              5  0 DIM(6000)                            *entrata 5
032900060830     ***d vpe6            S              5  0 DIM(6000)                            *entrata 6
033000060830     ***d vpe7            S              5  0 DIM(6000)                            *entrata 7
033100060830     ***d vpe8            S              5  0 DIM(6000)                            *entrata 8
033200060830     ***d vpe9            S              5  0 DIM(6000)                            *entrata 9
033300060830     ***d vpe0            S              5  0 DIM(6000)                            *entrata 10
033400060830     ***d vpe11           S              5  0 DIM(6000)                            *entrata 1
033500060830     ***d vpe12           S              5  0 DIM(6000)                            *entrata 2
033600060830     ***d vpe13           S              5  0 DIM(6000)                            *entrata 3
033700060830     ***d vpe14           S              5  0 DIM(6000)                            *entrata 4
033800060830     ***d vpe15           S              5  0 DIM(6000)                            *entrata 5
033900060830     ***d vpe16           S              5  0 DIM(6000)                            *entrata 6
034000060830     ***d vpe17           S              5  0 DIM(6000)                            *entrata 7
034100060830     ***d vpe18           S              5  0 DIM(6000)                            *entrata 8
034200060830     ***d vpe19           S              5  0 DIM(6000)                            *entrata 9
034300060830     ***d vpe20           S              5  0 DIM(6000)                            *entrata 10
034400060830      *
034500060830      * Minuti lavorati per entrata
034600060830     d $VMLAVxx_ds     ds                  occurs(20)    inz
034700060904     d   $VMLAVxx                          like(V2Sorl)  dim($MaxEl2)  inz
034800060830     ***d vmlav01         S              4  0 DIM(6000)                            *mm lavorati ent
034900060830     ***d vmlav02         S              4  0 DIM(6000)                            *mm lavorati ent
035000060830     ***d vmlav03         S              4  0 DIM(6000)                            *mm lavorati ent
035100060830     ***d vmlav04         S              4  0 DIM(6000)                            *mm lavorati ent
035200060830     ***d vmlav05         S              4  0 DIM(6000)                            *mm lavorati ent
035300060830     ***d vmlav06         S              4  0 DIM(6000)                            *mm lavorati ent
035400060830     ***d vmlav07         S              4  0 DIM(6000)                            *mm lavorati ent
035500060830     ***d vmlav08         S              4  0 DIM(6000)                            *mm lavorati ent
035600060830     ***d vmlav09         S              4  0 DIM(6000)                            *mm lavorati ent
035700060830     ***d vmlav10         S              4  0 DIM(6000)                            *mm lavorati ent
035800060830     ***d vmlav11         S              4  0 DIM(6000)                            *mm lavorati ent
035900060830     ***d vmlav12         S              4  0 DIM(6000)                            *mm lavorati ent
036000060830     ***d vmlav13         S              4  0 DIM(6000)                            *mm lavorati ent
036100060830     ***d vmlav14         S              4  0 DIM(6000)                            *mm lavorati ent
036200060830     ***d vmlav15         S              4  0 DIM(6000)                            *mm lavorati ent
036300060830     ***d vmlav16         S              4  0 DIM(6000)                            *mm lavorati ent
036400060830     ***d vmlav17         S              4  0 DIM(6000)                            *mm lavorati ent
036500060830     ***d vmlav18         S              4  0 DIM(6000)                            *mm lavorati ent
036600060830     ***d vmlav19         S              4  0 DIM(6000)                            *mm lavorati ent
036700060830     ***d vmlav20         S              4  0 DIM(6000)                            *mm lavorati ent
036800060830      *
036900060830      * Colli per Uscita
037000060831     d $VPAxx_ds       ds                  occurs($MaxEl2)  inz
037100060904     d   $VPAxx                            like(V3Du01)     dim(60)  inz
037200060831     d $VPAD_ds        ds                  inz
037300060831     d   V3Du01                            inz
037400060831     d   V3Du02                            inz
037500060831     d   V3Du03                            inz
037600060831     d   V3Du04                            inz
037700060831     d   V3Du05                            inz
037800060831     d   V3Du06                            inz
037900060831     d   V3Du07                            inz
038000060831     d   V3Du08                            inz
038100060831     d   V3Du09                            inz
038200060831     d   V3Du10                            inz
038300060831     d   V3Du11                            inz
038400060831     d   V3Du12                            inz
038500060831     d   V3Du13                            inz
038600060831     d   V3Du14                            inz
038700060831     d   V3Du15                            inz
038800060831     d   V3Du16                            inz
038900060831     d   V3Du17                            inz
039000060831     d   V3Du18                            inz
039100060831     d   V3Du19                            inz
039200060831     d   V3Du20                            inz
039300060831     d   V3Du21                            inz
039400060831     d   V3Du22                            inz
039500060831     d   V3Du23                            inz
039600060831     d   V3Du24                            inz
039700060831     d   V3Du25                            inz
039800060831     d   V3Du26                            inz
039900060831     d   V3Du27                            inz
040000060831     d   V3Du28                            inz
040100060831     d   V3Du29                            inz
040200060831     d   V3Du30                            inz
040300060831     d   V3Du31                            inz
040400060831     d   V3Du32                            inz
040500060831     d   V3Du33                            inz
040600060831     d   V3Du34                            inz
040700060831     d   V3Du35                            inz
040800060831     d   V3Du36                            inz
040900060831     d   V3Du37                            inz
041000060831     d   V3Du38                            inz
041100060831     d   V3Du39                            inz
041200060831     d   V3Du40                            inz
041300060831     d   V3Du41                            inz
041400060831     d   V3Du42                            inz
041500060831     d   V3Du43                            inz
041600060831     d   V3Du44                            inz
041700060831     d   V3Du45                            inz
041800060831     d   V3Du46                            inz
041900060831     d   V3Du47                            inz
042000060831     d   V3Du48                            inz
042100060831     d   V3Du49                            inz
042200060831     d   V3Du50                            inz
042300060831     d   V3Du51                            inz
042400060831     d   V3Du52                            inz
042500060831     d   V3Du53                            inz
042600060831     d   V3Du54                            inz
042700060831     d   V3Du55                            inz
042800060831     d   V3Du56                            inz
042900060831     d   V3Du57                            inz
043000060831     d   V3Du58                            inz
043100060831     d   V3Du59                            inz
043200060831     d   V3Du60                            inz
043300060831     ***d Vpa1            s              5  0 dim($MaxEl2)                         *uscita 1
043400060831     ***d Vpa2            s              5  0 dim($MaxEl2)                         *uscita 2
043500060831     ***d Vpa3            s              5  0 dim($MaxEl2)                         *uscita 3
043600060831     ***d Vpa4            s              5  0 dim($MaxEl2)                         *uscita 4
043700060831     ***d Vpa5            s              5  0 dim($MaxEl2)                         *uscita 5
043800060831     ***d Vpa6            s              5  0 dim($MaxEl2)                         *uscita 6
043900060831     ***d Vpa7            s              5  0 dim($MaxEl2)                         *uscita 7
044000060831     ***d Vpa8            s              5  0 dim($MaxEl2)                         *uscita 8
044100060831     ***d Vpa9            s              5  0 dim($MaxEl2)                         *uscita 9
044200060831     ***d Vpa10           s              5  0 dim($MaxEl2)                         *uscita 10
044300060831     ***d Vpa11           s              5  0 dim($MaxEl2)                         *uscita 11
044400060831     ***d Vpa12           s              5  0 dim($MaxEl2)                         *uscita 12
044500060831     ***d Vpa13           s              5  0 dim($MaxEl2)                         *uscita 13
044600060831     ***d Vpa14           s              5  0 dim($MaxEl2)                         *uscita 14
044700060831     ***d Vpa15           s              5  0 dim($MaxEl2)                         *uscita 15
044800060831     ***d Vpa16           s              5  0 dim($MaxEl2)                         *uscita 16
044900060831     ***d Vpa17           s              5  0 dim($MaxEl2)                         *uscita 17
045000060831     ***d Vpa18           s              5  0 dim($MaxEl2)                         *uscita 18
045100060831     ***d Vpa19           s              5  0 dim($MaxEl2)                         *uscita 19
045200060831     ***d Vpa20           s              5  0 dim($MaxEl2)                         *uscita 20
045300060831     ***d Vpa21           s              5  0 dim($MaxEl2)                         *uscita 21
045400060831     ***d Vpa22           s              5  0 dim($MaxEl2)                         *uscita 22
045500060831     ***d Vpa23           s              5  0 dim($MaxEl2)                         *uscita 23
045600060831     ***d Vpa24           s              5  0 dim($MaxEl2)                         *uscita 24
045700060831     ***d Vpa25           s              5  0 dim($MaxEl2)                         *uscita 25
045800060831     ***d Vpa26           s              5  0 dim($MaxEl2)                         *uscita 26
045900060831     ***d Vpa27           s              5  0 dim($MaxEl2)                         *uscita 27
046000060831     ***d Vpa28           s              5  0 dim($MaxEl2)                         *uscita 28
046100060831     ***d Vpa29           s              5  0 dim($MaxEl2)                         *uscita 29
046200060831     ***d Vpa30           s              5  0 dim($MaxEl2)                         *uscita 30
046300060831     ***d Vpa31           s              5  0 dim($MaxEl2)                         *uscita 31
046400060831     ***d Vpa32           s              5  0 dim($MaxEl2)                         *uscita 32
046500060831     ***d Vpa33           s              5  0 dim($MaxEl2)                         *uscita 33
046600060831     ***d Vpa34           s              5  0 dim($MaxEl2)                         *uscita 34
046700060831     ***d Vpa35           s              5  0 dim($MaxEl2)                         *uscita 35
046800060831     ***d Vpa36           s              5  0 dim($MaxEl2)                         *uscita 36
046900060831     ***d Vpa37           s              5  0 dim($MaxEl2)                         *uscita 37
047000060831     ***d Vpa38           s              5  0 dim($MaxEl2)                         *uscita 38
047100060831     ***d Vpa39           s              5  0 dim($MaxEl2)                         *uscita 39
047200060831     ***d Vpa40           s              5  0 dim($MaxEl2)                         *uscita 40
047300060831     ***d Vpa41           s              5  0 dim($MaxEl2)                         *uscita 41
047400060831     ***d Vpa42           s              5  0 dim($MaxEl2)                         *uscita 42
047500060831     ***d Vpa43           s              5  0 dim($MaxEl2)                         *uscita 43
047600060831     ***d Vpa44           s              5  0 dim($MaxEl2)                         *uscita 44
047700060831     ***d Vpa45           s              5  0 dim($MaxEl2)                         *uscita 45
047800060831     ***d Vpa46           s              5  0 dim($MaxEl2)                         *uscita 46
047900060831     ***d Vpa47           s              5  0 dim($MaxEl2)                         *uscita 47
048000060831     ***d Vpa48           s              5  0 dim($MaxEl2)                         *uscita 48
048100060831     ***d Vpa49           s              5  0 dim($MaxEl2)                         *uscita 49
048200060831     ***d Vpa50           s              5  0 dim($MaxEl2)                         *uscita 50
048300060831     ***d Vpa51           s              5  0 dim($MaxEl2)                         *uscita 51
048400060831     ***d Vpa52           s              5  0 dim($MaxEl2)                         *uscita 52
048500060831     ***d Vpa53           s              5  0 dim($MaxEl2)                         *uscita 53
048600060831     ***d Vpa54           s              5  0 dim($MaxEl2)                         *uscita 54
048700060831     ***d Vpa55           s              5  0 dim($MaxEl2)                         *uscita 55
048800060831     ***d Vpa56           s              5  0 dim($MaxEl2)                         *uscita 56
048900060831     ***d Vpa57           s              5  0 dim($MaxEl2)                         *uscita 57
049000060831     ***d Vpa58           s              5  0 dim($MaxEl2)                         *uscita 58
049100060831     ***d Vpa59           s              5  0 dim($MaxEl2)                         *uscita 59
049200060831     ***d Vpa60           s              5  0 dim($MaxEl2)                         *uscita 60
049300060929      * Calcolo dettaglio
049400060831     d Vpnc            s              5  0 dim($MaxEl2)                         *prg colli -corr-
049500060831     d VPpnc           s              5  0 dim($MaxEl2)                         *prg colli -prec-
049600060831     d Vpsp            s              3  0 dim($MaxEl2)                         *% scostamento
049700060929      * per Statistiche
049800060831     d Vpesind         s              5  0 dim($MaxEl2)                         *caricati da
049900060831     d Vpussor         s              5  0 dim($MaxEl2)                         *smistati da
050000060831     d Vpuslos         s              5  0 dim($MaxEl2)                         *smistati da
050100060831     d Vpusnes         s              5  0 dim($MaxEl2)                         *smistati da
050200060831     d Vcsd            s              5  0 dim($MaxEl2)                         *letti da scanner
050300060831     d Vps2dok         s              5  0 dim($MaxEl2)                         *letti da pistola
050400060831     d Vccv            s              5  0 dim($MaxEl2)                         *letti con volume
050500060831     d Vccp            s              5  0 dim($MaxEl2)                         *letti con peso
050600060831     d Vnls            s              5  0 dim($MaxEl2)                         *non letti scanner
050700060831     d Vnlp            s              5  0 dim($MaxEl2)                         *non letti pistola
050800060831     d Vxnomis         s              5  0 dim($MaxEl2)                         *C:non misurati
050900060831     d Vxnopes         s              5  0 dim($MaxEl2)                         *C:non pesati
051000060831     d Vodsdna         s              5  0 dim($MaxEl2)                         *of x uscita piena
051100060831     d Verrmac         s              5  0 dim($MaxEl2)                         *of x errori machine
051200060831     d Vchi            s              5  0 dim($MaxEl2)                         *non letti scanner
051300060831     d Vldv            s              5  0 dim($MaxEl2)                         *non letti scanner
051400060831     d Vbar            s              5  0 dim($MaxEl2)                         *non letti scanner
051500060904     d VbarP           s              5  0 dim($MaxEl2)                         *non letti scanner
051600060831     d Vpesind2        s              5  0 dim($MaxEl2)                         »caricati
051700060831     d Vpussor2        s              5  0 dim($MaxEl2)                         »smistati correttam.
051800060831     d Vodsdna2        s              5  0 dim($MaxEl2)                         »of x uscita piena
051900060831     d Verrmac2        s              5  0 dim($MaxEl2)                         »of x errori machine
052000060830      *
052100060831      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
052200060830      *
052300060830      * - Parametri
052400060830     d KPJBA         e ds                                                       *architettura
052500060830     d FIMS00ds      e ds                                                       *ds di procedura
052600060830      *
052700060830      * - Parametri per richiamo al pgm di controllo profilo utenti
052800060830     d Tibs34Ds      e ds                  inz
052900060830      * - Ds di riferimento al file esterno AzUte00F
053000060830     d AzuteDs       e ds                  ExtName(AzUte00F)
053100060830      * - Ds per dati organigramma
053200060830     d DDatiUte      e ds
053300060830      *
053400060830      * - Reperimento tabelle
053500060830     d TIBS02ds      e ds                  inz
053600060830     d   T02mod      e                     inz('C')
053700060830      * - Tab. MSL = Layout macchinone smistacolli
053800060830     d dMSL          e ds                  inz
053900060830      * - Tab. MTP = Macchinone tipo applicazione
054000060830     d dMTP          e ds                  inz
054100060830      * - Obiettivi di Produttività di un Impianto Automatico Smistacolli
054200060830     d dKPI          e ds                  inz
054300061002      *
054400061002     d Status         sds           333
054500061002     d   SDSpgm          *proc
054600060830      *
054700060830     d                 ds
054800060830     d Wtempo                          z   inz(Z'1901-01-01-01.00.00.000000')
054900060830     d  Wdata                          d   datfmt(*ISO) overlay(Wtempo)
055000060830     d  Wora                           t   timfmt(*ISO) overlay(Wtempo:12)
055100060929     d Wtempo1                         z   inz(z'1901-01-01-01.00.00.000000')
055200060929     d Wtempo2                         z   inz(z'1901-01-01-01.00.00.000000')
055300060830     d W00tli                          z   inz(*loval)
055400060830     d W00tlf                          z   inz
055500060830      *
055600060830      * - Controllo/Inversione data
055700060830     d WLBDAT          ds                  inz
055800060830     d  G08dat                        8  0 inz
055900060830     d  G08inv                        8  0 inz
056000060830     d  G08err                        1    inz('3')
056100060830     d  G08tgi                        5  0 inz
056200060830      *
056300060830      * - Calcolo data (8)
056400060830     d WGIda8          ds                  inz
056500060830     d  GioDat                        8  0 inz
056600060830     d  GioInv                        8  0 inz
056700060830     d  GioTgi                        5  0 inz
056800060830      *
056900060831      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
057000060830      *
057100060830      * - Flags booleani
057200060830     d $Fine           S              1    inz(*off)                            *fine programma
057300060830     d $FNMSA          s              1    inz(*off)                            *esistenza dati
057400061002     ***d $FNMSApr        s              1    inz(*off)                            *esist. dati PREC
057500060831     d $RecOK          S              1    inz(*off)                            *validità record
057600080909     d $TooManyRec     s               n   inz(*off)
057700060929      *
057800060830     d $Video          s              1    inz('1')                             *video in gestione
057900060830      *
058000060830      * - Indici di schiera / Contatori
058100060830     d $nro            s              2  0 inz
058200061006     d xw              s              5  0 inz
058300060830     d xx              s              5  0 inz
058400060830     d xi              s              5  0 inz
058500060830     d  MEMxi          s              5  0 inz
058600060830     d  MEMgd          s              5  0 inz
058700060830     d  xm             s              5  0 inz
058800060830     d xj              s              5  0 inz
058900060831     d xg              s              5  0 inz
059000060831     d  MEMxg          s              5  0 inz
059100060830     d xv              s              5  0 inz
059200060830     d  MEMxv          s              5  0 inz
059300060904     d pj              s              5  0 inz
059400060904     d pk              s              5  0 inz
059500060904     d ppj             s              5  0 inz
059600060904     d ppk             s              5  0 inz
059700060830      *
059800060830      * - Parametri per QCMDEXC
059900060831     d Qcmd            s             80    inz('WRKSPLF   ')
060000060901     d Qlen            s             15  5 inz(80)
060100060830      *
060200060830      * - Campi di comodo
060300060830     d WdateISO        s               d   datfmt(*iso) inz(*loval)
060400060830     d D00dfvPR        s                   like(D00dfv) inz
060500060830     d D00dfvGG        s                   like(G08tgi) inz
060600061018      * - Parametri per analisi della produttività di un
060700060929      *   impianto automatico smistacolli
060800061006     d WnclOb          s              7  0 inz                                  tot colli lavorabili
060900061024      * - Campo per ripartizione colli della post.20 su tutte le altre
061000061024     d wRipColli       s                   like(V3DE20) inz
061100061024     d wTotColli       s                   like(V3DE20) inz
061200060830      *
061300060929     d S01nrr          s                   like(C01rcd) inz                     *n° rel sf
061400060929     d LASTnrr         s                   like(C01rcd) inz                     *ultimo nrr del sfl
061500060929     d SAVnrr          s                   like(C01rcd) inz                     *memo   nrr del sfl
061600060830      *
061700060830     d n14             s             14  0 inz
061800060830     d n8              s              8  0 inz
061900060830     d n6              s              6  0 inz
062000060830     d n4              s              4  0 inz
062100060830      *
062200040609     d wDays           s              3  0 inz
062300060831     d DatCor          s              8  0 inz                                  *data correrte a/m/g
062400060831     d ULTmsa          s              5  0 inz                                  *ultimo elemento
062500060831     d ULTmsaPR        s              5  0 inz                                  *ultimo elemento PRE
062600060831     d PrimaData       s             12  0 inz                                  *prima  data + ora
062700060831     d UltimData       s             12  0 inz                                  *ultima data + ora
062800060831     d Ldata           s              8  0 inz                                  *data di lavoro
062900060831     d Lora            s              4  0 inz                                  *ora  di lavoro
063000060831     d Lmin            s              2  0 inz                                  *mm   di lavoro
063100060831     d Lpergri         s             12  0 inz                                  *periodo di lavoro
063200060831     d Lpergri2        s             12  0 inz                                  *periodo di lavoro
063300060831     d LpergriP        s             12  0 inz                                  *periodo di lavoro
063400060831     d Lpermem         s             12  0 inz                                  *periodo di lavoro
063500060831     d DEPtip          s              1    inz                                  *dep tipo griglia
063600060831     d MinLav          s              4  0 inz                                  *mm lavorati  totali
063700060831     d wMinLav         s              4  0 inz                                  *di lavoro
063800060831     d MirLav          s              4  0 inz                                  *mm lavorati  totali
063900060831     d wMirLav         s              4  0 inz                                  *di lavoro
064000060831     d MinTra          s              4  0 inz                                  *mm trascorsi totali
064100060901     d MirTra          s              4  0 inz                                  *mm trascorsi totali
064200060831     d MEMtco          s              5  0 inz                                  *pacchi
064300060831     d MEMtlm          s              4  0 inz                                  *mm lavorati
064400060831     d MEMtcl          s              6  1 inz                                  *Pacchi/TempoLav
064500060831     d MEMtkp          s              6  1 inz                                  *KPI obiettivo
064600060831     d MEMnco          s              5  0 inz                                  *pacchi
064700060831     d MEMncl          s              6  1 inz                                  *Pacchi/TempoLav
064800060831     d MEMttm          s              4  0 inz                                  *mm trascorsi
064900060831     d MEMrco          s              5  0 inz                                  *pacchi
065000060831     d MEMrlm          s              4  0 inz                                  *mm lavorati
065100060831     d MEMrcl          s              6  1 inz                                  *Pacchi/TempoLav
065200060831     d MEMrkp          s              6  1 inz                                  *KPI obiettivo
065300060831     d MEMrtm          s              4  0 inz                                  *mm trascorsi totali
065400060929     d §PNCatt         s             11  3 inz                                  *prg colli attuale
065500060929     d §PNCpre         s             11  3 inz                                  *prg colli preced
065600060929     d §PNCpsp         s              3  0 inz                                  *% scostamento
065700060929     d §PNCgap         s             18  6 inz
065800060831     d §min            s              4  0 inz                                  *minuti totali
065900060831     d §gg             s              2  0 inz                                  *giorni trascors9
066000060831     d §hh             s              2  0 inz                                  *ore    trascorse
066100060831     d §mm             s              2  0 inz                                  *minuti trascorsi
066200070510      * - Campi per la ripartizione dei colli in post.20
066300061002     d W_prod_rel      s             18 15 inz
066400071123     d W_loaded_B      s              5  0 inz
066500061205
066600070510      * - Campi di comodo
066700061205     d w0050           s              5  0 inz
066800070510     d w0091           s              9  1 inz
066900060830      *
067000060831      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
067100060830      *
067200060830      * - archivio TNTBE01L
067300060830     c     K05TBE01      klist
067400060830     c                   kfld                    TBEcod                          -tabella
067500060830     c                   kfld                    TBEke1                          -chiave uno
067600060830     c                   kfld                    TBEke2                          -chiave due
067700060830     c                   kfld                    TBElin                          -lingua
067800060830     c                   kfld                    TBEsif                          -s.informativo
067900060830     c     K02TBE01      klist
068000060830     c                   kfld                    TBEcod                          -tabella
068100060830     c                   kfld                    TBEke1                          -chiave uno
068200060830      * - archivio TNTBE02L
068300060830     c     K04TBE02      klist
068400060830     c                   kfld                    TBEcod                          -tabella
068500060830     c                   kfld                    TBElin                          -lingua
068600060830     c                   kfld                    TBEsif                          -s.informativo
068700060830     c                   kfld                    TBEke1                          -chiave uno
068800170201      * - archivio FNMSA12L
068900170201     c     K04MSA12      klist
069000060830     c                   kfld                    MSAapl                          -punto operativo
069100060830     c                   kfld                    MSAfil                          -punto operativo
069200060830     c                   kfld                    MSAdfv                          -data foglio
069300060830     c                   kfld                    MSAnpg                          -categoria foglio
069400060830
069500060830      *===============================================================*
069600060831      *  ?I N D I C A T O R I?                                        *
069700060830      *---------------------------------------------------------------*
069800061002      *  28 - Emissione messaggio di errore a video                   *
069900060830      *  30 - Ripulisce il subfile                                    *
070000060830      *  31 - NON emette il subfile                                   *
070100060929      *  32 - Record modificato nel subfile (sflnxtchg)               *
070200060929      *  33 - Attiva il subfile-end                                   *
070300060929      *  40 - Evidenzia dati nel sfl        (totali)                  *
070400060830      *  41 - Evidenzia % scostamento KPI   (V2CTPS)                  *
070500060830      *  42 - Evidenzia % scostamento KPI   (V2CRPS)                  *
070600060830      *  43 - Evidenzia % scostamento KPI   (V2CNPS)                  *
070700060830      *===============================================================*
070800060830
070900060830     c     *Entry        plist
071000060830     c                   parm                    KPJBA
071100060830     c                   parm                    FIMS00ds
071200060830      * Operazioni Iniziali
071300060929     c                   exsr      RoutInz
071400060830      *
071500060830      * Inizializzazione SubFile
071600060929     c                   exsr      InzS01
071700060830      *
071800060830      * Gestione video
071900060830do  1c                   DOU       $Fine     = *on
072000060830      * - SubFile vuoto
072100060929cas 2c     $Video        caseq     '1'           GesV01
072200060830      * - SubFile con dati
072300060929     c     $Video        caseq     '2'           GesS01
072400060929      * - Videata di dettaglio   (opzione 5)
072500060929     c     $Video        caseq     '3'           GesD01
072600060929      * - Videate di statistiche (opzione 7)
072700060929     c     $Video        caseq     '4'           GesD0234
072800060929     c     $Video        caseq     '5'           GesD0234
072900060929     c     $Video        caseq     '6'           GesD0234
073000060830e   1c                   endcs
073100060830e   1c                   ENDDO
073200060830      *
073300060830      * Fine
073400060830     c                   eval      *inLR     = *on
073500060830
073600060830      *---------------------------------------------------------------*
073700060831      *?Operazioni Iniziali                                          ?*
073800060830      *---------------------------------------------------------------*
073900060929     c     RoutInz       BEGSR
074000060830      *
074100060830      * Reperimento dati job
074200060830     c                   exsr      DatiJob
074300060830      *
074400060830      * Reperimento data corrente
074500060830     c                   time                    n14                            *Ora (6) + Data (8)
074600060830     c                   move      n14           n8                             *Data corrente g/m/a
074700060830     c                   clear                   WLBdat
074800060830     c                   eval      G08dat    = n8
074900060830     c                   call      'XSRDA8'
075000060830     c                   parm                    WLBdat
075100060830     c                   eval      DATcor    = G08inv                           *Data corrente a/m/g
075200060830      *
075300060830      * Calcola il giorno precedente utile alla data foglio richiesta
075400060830     c                   reset                   WLBdat
075500060830     c                   eval      G08inv    = D00dfv
075600060830     c                   call      'XSRDA8'
075700060830     c                   parm                    WLBdat
075800060830     c                   eval      D00dfvGG = G08tgi
075900060830      * - Ciclo per trovare il primo giorno precedente nel quale il
076000060830      *   macchinone ha lavorato
076100060831     c                   eval      xx        = 1
076200060831do  1c                   DOW       xx       <= 10
076300060831     c                   eval      GioTgi    = D00dfvGG - xx                    *gg data - "x" gg
076400060830     c                   clear                   GioDat
076500060830     c                   clear                   GioInv
076600060830     c                   call      'XSRGI8'
076700060830     c                   parm                    WGIda8
076800060830     c                   eval      D00dfvPR  = GioInv                           *data gg precedente
076900060830      * - Lettura archivio
077000060830     c                   eval      MSAapl    = D00apl
077100060830     c                   eval      MSAfil    = D00fil
077200060901     c                   eval      MSAdfv    = D00dfvPR
077300060830     c                   eval      MSAnpg    = D00npg
077400170201     c     K04MSA12      setll     FNMSA000
077500170201if  2c                   if        %equal(FNMSA12L)                             *trovati dati
077600060901     c                   leave                                                  *esce dal ciclo
077700060830e   2c                   endif
077800060901     c                   eval      xx        = xx + 1                           *incremento gg meno
077900060830e   1c                   ENDDO
078000060830      *
078100060830      * Caricamento tabelle occorrenti
078200060830     c                   exsr      CarTab
078300060830      *
078400060830      * Imposta la testata
078500061002     c                   exsr      ImpT01
078600060830      *
078700060830      * Memorizzazione dei dati del file in schiere
078800060830     c                   exsr      sr_MemoSch
078900060830      *
079000060830     c                   ENDSR
079100060830
079200060830      *---------------------------------------------------------------*
079300060831      *?Reperimento dati del job (Utente/Operativi)                  ?*
079400060830      *---------------------------------------------------------------*
079500060830     c     DatiJob       BEGSR
079600060830      *
079700060830     c     *dtaara       define    §azute        azuteds
079800060830     c     *dtaara       define    §datiute      ddatiute
079900060830      *
080000060830     c                   in(E)     *dtaara
080100060830     c                   IF        %Error or RSUT = *blanks
080200060929     c                   clear                   Tibs34Ds
080300060830     c                   call      'TIBS34R'
080400060830     c                   parm                    Tibs34Ds
080500060830     c                   in        *dtaara
080600060830     c                   ENDIF
080700060830      *
080800060830     c                   ENDSR
080900060830
081000060830      *---------------------------------------------------------------*
081100060831      *?Caricamento tabelle occorrenti                               ?*
081200060830      *---------------------------------------------------------------*
081300060830     c     CarTab        BEGSR
081400060830      *
081500060830      * Layout macchinone smistacolli
081600060830     c                   clear                   TBEcod                         *tabella
081700060929     c                   eval      TBEke1    = *zeros                           *chiave uno
081800060830     c                   move      'MSL'         TBEke1
081900060830     c                   clear                   TBEke2                         *chiave due
082000060830     c                   clear                   TBElin                         *lingua
082100060830     c                   clear                   TBEsif                         *s.i. di GRUPPO
082200060830     c     K05TBE01      chain     TNTBE000
082300060830if  1c                   if        NOT %found(TNTBE01L)
082400060830     c                   eval      TBEsif    = KNSIF                            *s.i. di AZIENDA
082500060830     c     K05TBE01      chain     TNTBE000
082600060830e   1c                   endif
082700060830      *
082800060830     c*** già così:      clear                   $msl
082900060830     c*** già così:      clear                   $Nmsl
083000060831     c                   clear                   xx
083100060830     c                   eval      TBEcod    = 'MSL'                            *tabella
083200060830     c                   clear                   TBElin                         *lingua
083300060830     c                   movel(p)  D00fil        TBEke1                         *chiave uno
083400060901     c     K04TBE02      setll     TNTBE02
083500060901     c     K04TBE02      reade     TNTBE02
083600060830do  1c                   DOW       NOT  %eof(TNTBE02L)
083700060831     c                             and  xx   < 20
083800060904     c                   movel     TBEuni        dMSL
083900161013if  2c                   if              TBEatb  = *blanks
084000161025     c                             and ((D00apl  = §MSLap5  and
084100161025     c                                   D00npg  = 5)     or
084200161025     c                                  (D00apl  = §MSLapl  and
084300161205     c                                   D00npg  = 2))
084400140307if  3c                   if        §MSLent < 20
084500060831     c                   add       1             xx
084600140307     c*//                eval      $msl(xx)  = §MSLpep                          *entrance point
084700140307     c*//                eval      $Nmsl(xx) = §MSLent                          *n° entrance point
084800140307     c                   eval      $msl(§MSLent)  = §MSLpep                     *entrance point
084900140307     c                   eval      $Nmsl(§MSLent) = §MSLent                     *n° entrance point
085000140307e   3c                   endif
085100060830e   2c                   endif
085200060901     c     K04TBE02      reade     TNTBE02
085300060830e   1c                   ENDDO
085400060830      *
085500060830      * Obiettivi di Produttività di un Impianto Automatico Smistacolli
085600060830     c                   clear                   dKPI
085700060830     c                   reset                   TIBS02ds
085800060830     c                   eval      T02sif    = KNSIF
085900060830     c                   eval      T02cod    = 'KPI'
086000060830     c                   movel(p)  D00npg        T02ke1
086100060830     c                   movel(p)  D00apl        T02ke2
086200060830     c                   call      'TIBS02R'
086300060830     c                   parm                    KPJBA
086400060830     c                   parm                    TIBS02ds
086500060830     c                   if        T02err    = *blanks
086600060830     c                   movel     T02uni        dKPI
086700060830     c                   endif
086800060830      *
086900060830     c                   ENDSR
087000060830
087100060830      *---------------------------------------------------------------*
087200060831      *?Impostazione video testata                                   ?*
087300060830      *---------------------------------------------------------------*
087400061002     c     ImpT01        BEGSR
087500061002      *
087600061002      * Nome programma
087700061002     c                   eval      V1Tpgm    = SDSpgm
087800060830      *
087900060830      * P.O.
088000060830     c                   eval      V0Cfil    = D00fil
088100060830     c     D00fil        chain     AZORG
088200060830if  1c                   if        %found(AZORG01L)
088300060830     c                   movel     ORGdes        V0Dfil
088400060830e   1c                   endif
088500060830      * Data F.V.
088600060830     c                   reset                   WLBdat
088700060830     c                   eval      G08inv    = D00dfv
088800060830     c                   call      'XSRDA8'
088900060901     c                   parm                    WLBdat
089000060830     c                   eval      V0Cdfv    = G08dat
089100060830      * Categoria F.V.
089200060830     c                   eval      V0Cnpg    = D00npg
089300060830if  1c                   if        D00npg    = 2
089400060830     c                   movel     $Dvid(1)      V0Dnpg
089500060830x   1c                   else
089600060830     c                   movel     $Dvid(2)      V0Dnpg
089700060830e   1c                   endif
089800060830      *
089900060830      * Tipo applicazione
090000060830     c                   eval      V0Capl    = D00apl
090100060830     c                   clear                   dMTP
090200060830     c                   reset                   TIBS02ds
090300060830     c                   eval      T02sif    = KNSIF
090400060830     c                   eval      T02cod    = 'MTP'
090500060830     c                   movel(p)  V0Capl        T02ke1
090600060830     c                   call      'TIBS02R'
090700060830     c                   parm                    KPJBA
090800060830     c                   parm                    TIBS02ds
090900060830     c                   if        T02err    = *blanks
091000060830     c                   movel     T02uni        dMTP
091100060830     c                   endif
091200060830     c                   movel     §MTPdes       V0Dapl
091300060830      *
091400060830     c                   ENDSR
091500060830
091600060830      *---------------------------------------------------------------*
091700060831      *?Inizializzazione subfile                                     ?*
091800060830      *---------------------------------------------------------------*
091900060929     c     InzS01        BEGSR
092000060830      *
092100060830      * Pulizia subfile
092200060830     c                   seton                                        3031
092300060929     c                   write     MS08C01
092400060830     c                   setoff                                       31
092500060830      *
092600060929      * Reimposta variabili di lavoro
092700061002     c                   eval      *in28   = *off
092800060929     c                   eval      *in40   = *off
092900060830     c                   eval      $Video  = '1'
093000060929     c                   clear                   S01nrr
093100060830      *
093200170201      * SE reperiti dati in FNMSA10F:
093300060830if  1c                   IF        $FNMSA = *on
093400060830      *
093500060830      * - Impostazione di alcuni valori statistici calcolati da
093600060830      *   emettere con il sflctl
093700060929     c                   exsr      RieC01
093800060830      * - Caricamento del sfl
093900060830do  2c                   do        MEMxv         xv
094000060929     c                   exsr      WrtS01
094100060830e   2c                   enddo
094200060929     c                   eval      LASTnrr = S01nrr
094300060830      *
094400060830e   1c                   ENDIF
094500080909      /free
094600080909       // Eventuale visualizzazione del messaggio
094700080909             select;
094800080909               // - Errore nel caricamento dei dati (troppi)
094900080909               when  $TooManyRec = *on;
095000080909                 *in28  = *on;
095100080909                 V1Dmsg = 'ERRORE (leggi messaggio sovrastante).   +
095200080909                           Contattare il CED.';
095300080909                 V1txt1 = 'IMPOSSIBILE visualizzare i dati di tutte le +
095400080909                           letture';
095500080909                 V1txt2 = 'dalle ore ' + %editw( $orl(1) : '0 :  ' )
095600080909                        + ' del giorno '
095700080909                        + %subst( %editc( $dtl(1):'X' ) : 7 : 2 )
095800080909                        + '/'
095900080909                        + %subst( %editc( $dtl(1):'X' ) : 5 : 2 )
096000080909                        + '/'
096100080909                        + %subst( %editc( $dtl(1):'X' ) : 1 : 4 );
096200080909               // - Nessun dato caricato
096300080909               when  $FNMSA = *off;
096400080909                 V1txt1 = 'Non ci sono dati da visualizzare';
096500080909                 clear  V1txt2;
096600080909             endsl;
096700080909      /end-free
096800060830      *
096900060929      * Impostazione emissione sfile
097000060929if  1c                   if        S01nrr  > *zeros
097100060830     c                   eval      $Video  = '2'
097200060929     c                   eval      *in30   = *off                               *SflDsp
097300060929     c                   eval      *in33   = *on                                *SflEnd
097400060830e   1c                   endif
097500060830      *
097600060830     c                   ENDSR
097700060830
097800060830      *---------------------------------------------------------------*
097900060831      *?Impostazione campi nel subfile control                       ?*
098000060830      *---------------------------------------------------------------*
098100060929     c     RieC01        BEGSR
098200060830      *
098300060830      * Progressivi ultimo turno:
098400060830      * - Colli
098500060830     c                   eval      V2Ctco  = MEMtco
098600060830      * - Tempo lavorato
098700060830     c                   eval      V2Ctlm  = MEMtlm
098800060830      * - KPI effettivo
098900060830     c                   eval      V2Ctcl  = MEMtcl
099000060830      * - KPI obiettivo
099100060830     c                   eval      V2Ctkp  = MEMtkp
099200060830      * - % scostamento KPI
099300060830     c                   if        MEMtkp <> *zeros
099400070510     c                   eval(h)   w0091   = ((MEMtcl * 100) / MEMtkp)
099500060830     c                                     - 100
099600060830     c                   else
099700070510     c                   clear                   w0091
099800060830     c                   endif
099900070510if  1c                   if        w0091   > 999,9
100000070510     c                   eval      V2Ctps  = 999,9
100100070510x   1c                   else
100200070510     c                   eval      V2Ctps  = w0091
100300070510e   1c                   endif
100400060830     c                   eval      *in41   = (V2Ctps < (§KPIpsc*(-1)))
100500060830      * - Tempo trascorso
100600060830     c                   eval      V2Cttm  = MEMttm
100700060830      *
100800060830      * Progressivi ultimi xx minuti
100900060830      * - Minuti ultimo riepilogo
101000060830     c                   eval      V2Cmtr  = D00mtr
101100060830      * - Colli
101200060830     c                   eval      V2Crco  = MEMrco
101300060830      * - Tempo lavorato
101400060830     c                   eval      V2Crlm  = MEMrlm
101500060830      * - KPI effettivo
101600060830     c                   eval      V2Crcl  = MEMrcl
101700060830      * - KPI obiettivo
101800060830     c                   eval      V2Crkp  = MEMrkp
101900060830      * - % scostamento KPI
102000060830     c                   if        MEMrkp <> *zeros
102100060830     c                   eval      V2Crps = ((MEMrcl * 100) / MEMrkp)
102200060830     c                                    - 100
102300060830     c                   else
102400060830     c                   clear                   V2Crps
102500060830     c                   endif
102600060830     c                   eval      *in42  = (V2Crps < (§KPIpsc*(-1)))
102700060830      * - Tempo trascorso
102800060830     c                   eval      V2Crtm  = MEMrtm
102900060829      *
103000060830     c                   ENDSR
103100060830
103200060830      *---------------------------------------------------------------*
103300060929      *?Scrittura singolo record nel sfl S01                         ?*
103400060830      *---------------------------------------------------------------*
103500060929     c     WrtS01        BEGSR
103600060830      *
103700060929     c                   clear                   MS08S01
103800060830      *
103900060830      * Campi hidden
104000060830     c                   eval      V2Hv    = xv                                 *indice video HIDDEN
104100060830     c                   eval      V2Hdtl  = $Vdtl(xv)
104200060830      *
104300060830      * Campi di i/o
104400060830     c*** già così       clear                   V2Sopz
104500060830     c                   eval      V2Sdes  = $Vdes(xv)
104600060901     c                   eval      V2Sorl  = $Vorl(xv)
104700060830     c                   eval      V2Spnc  = Vpnc(xv)
104800060901     c                   eval      V2Spsp  = Vpsp(xv)
104900060830      *
105000061002      * - Numero colli/minuto
105100060830if  1c                   if        $VminLav(xv)  > *zeros
105200061002     c                   eval(h)   V2Sncl  =  $Vncl(xv) / $VminLav(xv)
105300060830e   1c                   endif
105400060830      *
105500061002      * - Colli/Minuto caricati per entrata
105600061024      *   La ripartizione dei colli/minuto per entrata nel rec. di
105700061024      *   totale RIE/GEN funzione meglio se gestita come nella subr.
105800061024      *   GesD01...  Quindi DIFFERENZIO:
105900061024if  0c                   IF           ($Vtip(xv) <> 'R'                         *Riep
106000061024     c                             and $Vtip(xv) <> 'G')                        *Gen
106100061024      *
106200060904     c*** già così:      clear                   $V2SM
106300060830     c                   clear                   xx
106400060830do  1c                   DOW       xx         <  %elem($VMLAVxx_ds)
106500060831     c                   eval      xx         =  xx + 1
106600060830     c     xx            occur     $VMLAVxx_ds
106700060830if  2c                   if        $VMLAVxx(xv)  > *zeros
106800060904     c     xx            occur     $VPExx_ds
106900061002     c                   eval(h)   $V2SM(xx)  =  $VPExx(xv) / $VMLAVxx(xv)
107000060830e   2c                   endif
107100060830e   1c                   ENDDO
107200060904     c*** già compresa:  movea     $V2SM         $V2SM_ds
107300061002      *
107400061024x   0c                   ELSE
107500061024      *
107600061024     c*** già così:      clear                   $V2SM
107700061024      * - - Colli caricati per entrata
107800061024     c                   clear                   $V3DE
107900061024     c                   clear                   xx
108000061024do  2c                   DOW       xx         <  %elem($VPExx_ds)
108100061024     c                   eval      xx         =  xx + 1
108200061024     c     xx            occur     $VMLAVxx_ds
108300061024if  3c                   if        $VMLAVxx(xv)  > *zeros
108400061024     c     xx            occur     $VPExx_ds
108500061024     c                   eval      $V3DE(xx)  =  $VPExx(xv)
108600061024e   3c                   endif
108700061024e   2c                   ENDDO
108800061024      * - - Ripartizione dei colli della post.20 tra gli altri vari
108900061024      *     banchi SE riga di riepilogo o totale
109000061024      *    ?N.B.?- Il totale dei colli (in base a cui ripartire quelli
109100061024      *     della post.20 su tutte le altre post.) va calcolato ora,
109200061024      *     PRIMA di modificare la schiera $V3de con la subroutine
109300061024      *     "RipartB20det" (cioè PRIMA di iniziarne la ripartizione).
109400061024     c                   eval      wRipColli = %xfoot($V3de) - $V3de(20)
109500061024if  2c                   if        wRipColli >  *zeros
109600061024     c                   exsr      RipartB20det
109700061024e   2c                   endif
109800061024      * - - Media entrate in base ai minuti lavorati per entrata
109900061024     c                   clear                   $V3DM
110000061024     c                   clear                   xx
110100061024do  2c                   DOW       xx         <  %elem($VMLAVxx_ds)
110200061024     c                   eval      xx         =  xx + 1
110300061024     c     xx            occur     $VMLAVxx_ds
110400061024if  3c                   if        $VMLAVxx(xv)  > *zeros
110500061024      * - - Utilizzo il numero colli per postazione già "allineato" dalla
110600061024      *     subr. "RipartB20det"
110700061024      *     (i dati "originali" hanno ancora i colli nella post.20)
110800061205     c**!!!              eval(h)   $V3DM(xx)  =  $V3DE(xx) / $VMLAVxx(xv)
110900061205     c                   eval(h)   w0050      =  $V3DE(xx) / $VMLAVxx(xv)
111000061205     c                   if        w0050 > 99
111100061205     c                   eval      $v3dm(xx) = 99
111200061205     c                   else
111300061205     c                   eval      $v3dm(xx) = w0050
111400061205     c                   endif
111500061205
111600061024e   3c                   endif
111700061024e   2c                   ENDDO
111800061024      *
111900061024     c                   z-add     $V3DM         $V2SM
112000061024      *
112100061024e   0c                   ENDIF
112200060830      *
112300060830      * Impostazione indicatori a video
112400060830      * - Riga di riepilogo in alta intensità
112500061002     c                   eval      *in40   =   ($Vtip(xv) = 'R'
112600061002     c                                       or $Vtip(xv) = 'G')
112700060830      *
112800060830      * Scrittura record sfl
112900060929     c                   add       1             S01nrr
113000061002     c                   eval      C01rcd  = S01nrr
113100060929     c                   WRITE     MS08S01
113200060830      *
113300060830     c                   ENDSR
113400061024
113500061024      *---------------------------------------------------------------*
113600061024      *?Ripartizione entrate della post.20 in tutte le altre postaz. ?*
113700061024      *---------------------------------------------------------------*
113800061024     c     RipartB20det  BEGSR
113900061024      *
114000061024      * Calcolo Numero Totale Colli per postazione/periodo
114100061024     c                   eval      wTotColli  =  %xfoot($V3de)
114200061024      *
114300061024      * Calcolo proporzione con cui ripartire i colli della post.20
114400061024      *   su tutte le altre postazioni utilizzate
114500061024     c                   DO        19            xw
114600061024      *
114700061024      * - produttività relativa del banco di ingresso
114800061024     c                   eval(h)   W_prod_rel =  $V3de(xw) / wRipColli
114900061024      *
115000061024      * - num.colli rilevati inaspettatamente (ed usciti correttamente)
115100061024      *   da attribuire al banco di ingresso in esame
115200061024     c                   eval(h)   W_loaded_B =  $V3de(20) * W_prod_rel
115300061024      *
115400061024      * - ricalcolo del numero colli caricati su ciascun banco
115500071123     c                   eval      $V3de(xw)  =  $V3de(xw) + W_loaded_B
115600061024      *
115700061024     c                   ENDDO
115800061024      *
115900061024     c                   clear                   $V3DE(20)
116000061024      *
116100061024      * Controllo non risulti un totale colli "ripartito" superiore
116200061024      *   al totale colli originale (per gli arrotondamenti...)
116300061024      *   nel qual caso: tolgo i colli in eccesso dalla 1ª postazione
116400061024      *   con il più alto numero di colli
116500061024if  1c                   IF        wTotColli  <> %xfoot($V3de)
116600061024      *
116700061024      * - ricerca della postazione con il numero maggiore di colli
116800061024     c                   clear                   wRipColli
116900061024     c                   clear                   xw
117000061024do  2c                   Dow       xw         <  %elem($V3de)
117100061024     c                   add       1             xw
117200061024if  3c                   if             wRipColli  =  *zeros
117300061024     c                             or  (wRipColli  >  *zeros
117400061024     c                              and $V3de(xw)  >  $V3de(wRipColli))
117500061024     c                   eval      wRipColli  =  xw
117600061024e   3c                   endif
117700061024e   2c                   EndDo
117800061024      * - sistemazione numero totale colli nella post. con il numero
117900061024      *   maggiore di colli
118000061024     c                   eval      xw         =  wRipColli
118100061024     c                   eval      wRipColli  =  wTotColli
118200061024     c                                        -  %xfoot($V3de)
118300061024     c                   eval      $V3de(xw)  =  $V3de(xw)
118400061024     c                                        +  wRipColli
118500061024      *
118600061024e   1c                   ENDIF
118700061024      *
118800061024     c                   ENDSR
118900060830
119000060830      *---------------------------------------------------------------*
119100060831      *?Gestione prima videta                                        ?*
119200060830      *---------------------------------------------------------------*
119300060929     c     GesV01        BEGSR
119400060830      *
119500060830     c                   eval      *in30 = *on
119600060830      *
119700060830      * Emissione testata & piede
119800061002     c                   write     MS08T01                                      *testata
119900061002     c                   write     MS08T02
120000061002     c                   write     MS08Z02                                      *funzioni
120100060830      * Emissione sfl vuoto
120200060929     c                   write     MS08V01
120300060929     c                   exfmt     MS08C01
120400060830      *
120500060830sel 1c                   select
120600060831      * F3 = Fine programma
120700060830w   1c                   when      *inKC
120800060830     c                   exsr      F03ges
120900060830      * F12 = Ritorno
121000060830w   1c                   when      *inKL
121100060830     c                   exsr      F12ges
121200060830      *
121300060830e   1c                   endsl
121400060830      *
121500060830     c                   ENDSR
121600060830
121700060830      *---------------------------------------------------------------*
121800060831      *?Tasto funzionale F03 -> Fine programma                       ?*
121900060830      *---------------------------------------------------------------*
122000060830     c     F03ges        BEGSR
122100060830      *
122200060830     c                   eval      $Fine  = *on
122300060830     c                   eval      D00F03 = *on
122400060830      *
122500060830     c                   ENDSR
122600060830
122700060830      *---------------------------------------------------------------*
122800060831      *?Tasto funzionale F12 -> Ritorno                              ?*
122900060830      *---------------------------------------------------------------*
123000060830     c     F12ges        BEGSR
123100060830      *
123200060830     c                   eval      $Fine  = *on
123300060830     c                   eval      D00F12 = *on
123400060830      *
123500060830     c                   ENDSR
123600060830
123700060830      *---------------------------------------------------------------*
123800060831      *?Gestione seconda videata (subfile)                           ?*
123900060830      *---------------------------------------------------------------*
124000060929     c     GesS01        BEGSR
124100060830      *
124200060830      * Emissione testata
124300061002     c                   write     MS08T01
124400061002     c                   write     MS08T02
124500060830      * Emissione piede con tasti funzionali
124600061002     c                   write     MS08Z01
124700060830      * Emissione subfile
124800060929     c                   exfmt     MS08C01
124900080909     c                   clear                   V1Dmsg
125000061002     c                   eval      *in28 = *off
125100060929     c                   eval      *in40 = *off                                 *caratteri video
125200060829      *
125300060829sel 1c                   select
125400060831      * F3 = Fine programma
125500060829w   1c                   when      *inkc
125600060829     c                   exsr      F03ges
125700060831      * F5 = Ricarica
125800060829w   1c                   when      *inke
125900060829     c                   exsr      F05ges
126000060829      * F12 = Ritorno
126100060829w   1c                   when      *inkl
126200060829     c                   exsr      F12ges
126300060829      * F17 = Inizio dati
126400060829w   1c                   when      *inkr
126500060829     c                   exsr      F17ges
126600060829      * F18 = Fine dati
126700060829w   1c                   when      *inks
126800060829     c                   exsr      F18ges
126900060829      * F21 = Stampa dati
127000060829w   1c                   when      *inkv
127100060829     c                   exsr      F21ges
127200060829      * F22 = Coda di stampa
127300060829w   1c                   when      *inkw
127400060829     c                   exsr      F22ges
127500060830      * Enter = Gestione opzioni
127600060829x   1c                   other
127700061002     c                   exsr      OpzS01
127800061002if  2c*** superfluo:     if        *in28
127900060830     c*** superfluo:     leavesr
128000060830e   2c*** superfluo:     endif
128100060829e   1c                   endsl
128200060829      *
128300060830     c                   ENDSR
128400060830
128500060830      *---------------------------------------------------------------*
128600061002      *?Tasto funzionale F05 -> Ricarica sfl                         ?*
128700060830      *---------------------------------------------------------------*
128800060830     c     F05ges        BEGSR
128900060830      *
129000060830      * Memorizzazione dei dati del file in schiere
129100060830     c                   exsr      sr_MemoSch
129200060830      *
129300060830      * Inizializzazione sfl
129400060929     c                   exsr      InzS01
129500060830      *
129600060830     c                   ENDSR
129700060830
129800060830      *---------------------------------------------------------------*
129900060831      *?Tasto funzionale F17 -> Inizio dati                          ?*
130000060830      *---------------------------------------------------------------*
130100060830     c     F17ges        BEGSR
130200060830      *
130300060929     c                   eval      S01nrr  = 1
130400061002     c                   eval      C01rcd  = 1                                  *posizionamento
130500060929     c     S01nrr        chain     MS08S01
130600060830      *
130700060830     c                   ENDSR
130800060830
130900060830      *---------------------------------------------------------------*
131000060831      *?Tasto funzionale F18 -> Fine dati                            ?*
131100060830      *---------------------------------------------------------------*
131200060830     c     F18ges        BEGSR
131300060830      *
131400060929     c                   eval      S01nrr  = LASTnrr
131500061002     c                   eval      C01rcd  = LASTnrr
131600060929     c     S01nrr        chain     MS08S01
131700060830      *
131800060830     c                   ENDSR
131900060830
132000060830      *---------------------------------------------------------------*
132100060831      *?Tasto funzionale F21 -> Stampa dati                          ?*
132200060830      *---------------------------------------------------------------*
132300060830     c     F21ges        BEGSR
132400060830      *
132500060830      * Apertura file di stampa
132600060929     c                   open      FIMS08P
132700060830      *
132800060830      * PARAMETRI DI LANCIO  - - - - - - - - - - - - - - - - - - - - -*
132900060830      *
133000060830      * Impostazione dei campi per i parametri di lancio
133100060830      * - P.O.
133200060830     c                   eval      PACfil  = V0Cfil
133300060830     c                   eval      PADfil  = V0Dfil
133400060830      * - Tipo applicazione
133500060830     c                   eval      PACapl  = V0Capl
133600060830     c                   eval      PADapl  = §MTPdes
133700060830      * - Data foglio
133800060830     c                   eval      PACdfv  = V0Cdfv
133900060830      * - Categoria foglio
134000060830     c                   eval      PACnpg  = V0Cnpg
134100060830     c                   eval      PADnpg  = V0Dnpg
134200060830      * - Minuti di dettaglio
134300060830     c                   eval      PACmtd  = D00mtd
134400060830      * - Minuti di riepilogo
134500060830     c                   eval      PACmtr  = D00mtr
134600060830      * - Orario di lettura: dalle / alle
134700060830     c                   eval      PAColi  = D00oli
134800060830     c                   eval      PAColf  = D00olf
134900060830      *
135000060830      * Stampa della testata
135100060929     c                   write     MS08PT
135200060830      * Stampa dei parametri di lancio
135300060929     c                   write     MS08PA
135400060830      * Stampa nro entrate e descrizione, caricate in tabella MSL
135500060830     c                   eval      xw      = 1
135600060830     c                   DOW            xw        <= 20
135700060830     c                             and  $Nmsl(xw) <> *zeros
135800060830     c                   clear                   ENdent
135900060830     c                   eval      ENcent  = $Nmsl(xw)
136000060830     c                   eval      ENdent  = $msl(xw)
136100060929     c                   write     MS08EN
136200060830     c                   eval      xw      = xw + 1
136300060830     c                   ENDDO
136400060830     c
136500060830      *
136600060830      * PROSPETTO ENTRATE  - - - - - - - - - - - - - - - - - - - - - -*
136700060830      *
136800060830      * RIstampa testata (in pagina successiva)
136900060929     c                   write     MS08PT
137000060830      * Stampa dati statistici
137100060830     c                   eval      STCtco  = V2Ctco
137200060830     c                   eval      STCtlm  = V2Ctlm
137300060830     c                   eval      STCtcl  = V2Ctcl
137400060830     c                   eval      STCttm  = V2Cttm
137500060830     c                   eval      STCmtr  = V2Cmtr
137600060830     c                   eval      STCrco  = V2Crco
137700060830     c                   eval      STCrlm  = V2Crlm
137800060830     c                   eval      STCrcl  = V2Crcl
137900060830     c                   eval      STCrtm  = V2Crtm
138000061002      *
138100060830     c                   eval      STCtkp  = V2Ctkp
138200060830     c                   eval      STCtps  = V2Ctps
138300060830     c                   eval      STCrkp  = V2Crkp
138400060830     c                   eval      STCrps  = V2Crps
138500061002      *
138600060929     c                   write     MS08ST                                       *dati statistici
138700060830      *
138800060830      * Stampa intestazione
138900060929     c                   write     MS08PI
139000060830     c                   eval      *in01   = *off
139100060830      *
139200060830      * Ciclo di lettura e stampa dei dati del sfile
139300060929do  1c                   DO        LASTnrr       S01nrr
139400060830      *
139500060929     c     S01nrr        chain     MS08S01
139600060830      *
139700060830      * - impostazione dei campi di dettaglio
139800060830     c                   eval      DECdes  = V2Sdes
139900060830     c                   eval      DECorl  = V2Sorl
140000060830     c                   eval      DECpnc  = V2Spnc
140100060830     c                   eval      DECpsp  = V2Spsp
140200060830     c                   eval      DECncl  = V2Sncl
140300060830      *
140400061005     c                   z-add     $V2SM         $DECm
140500060830      *
140600060904     c*** già così:      clear                   $DECe
140700060830     c                   clear                   xx
140800060830do  2c                   DOW       xx         <  %elem($VPExx_ds)
140900060831     c                   eval      xx         =  xx + 1
141000061005     c     xx            occur     $VPExx_ds
141100061005     c                   eval      $DECe(xx)  =  $VPExx(V2Hv)
141200060830e   2c                   ENDDO
141300060904     c*** già compresa:  movea     $DECe         $DECe_ds
141400060830      *
141500060830      * Test OverFlow
141600060830if  2c                   if        *in01
141700060929     c                   write     MS08RI                                       *riga chiusura
141800060929     c                   write     MS08PT                                       *testata
141900060929     c                   write     MS08PI                                       *intestazione
142000060830     c                   eval      *in01      =  *off
142100060830e   2c                   endif
142200060830      *
142300060830      * Stampa riga di dettaglio
142400060929     c                   write     MS08DE
142500060830      *
142600060830e   1c                   ENDDO
142700060830      *
142800060830      * Stampa riga di chiusura
142900060929     c                   write     MS08RI
143000060830      *
143100060830      * PROSPETTO USCITE (no x tipo PICKING) - - - - - - - - - - - - -*
143200060830      *
143300060830if  1c                   if        §MTPtds   <> 'K'
143400060830      *
143500060830      * Stampa testata
143600060929     c                   write     MS08PT                                       *testata
143700060830      *
143800060830      * Ciclo di lettura e stampa dei dati del sfile
143900060929do  2c                   DO        LASTnrr       S01nrr
144000060830      *
144100060929     c     S01nrr        chain     MS08S01
144200060830      *
144300060830      * Elaborazione  delle sole righe RIEPILOGO e GENERALE
144400060830if  3c                   IF             $Vtip(V2Hv) = 'R'
144500060830     c                             or   $Vtip(V2Hv) = 'G'
144600060830      *
144700060830      * - impostazione dei campi di dettaglio
144800060830     c                   eval      D2Cdes  = V2Sdes
144900060901     c                   eval      D2Corl  = V2Sorl
145000060831     c     V2Hv          occur     $VPAxx_ds
145100060831     c                   eval      D2Cu01  = $VPAxx( 1)
145200060831     c                   eval      D2Cu02  = $VPAxx( 2)
145300060831     c                   eval      D2Cu03  = $VPAxx( 3)
145400060831     c                   eval      D2Cu04  = $VPAxx( 4)
145500060831     c                   eval      D2Cu05  = $VPAxx( 5)
145600060831     c                   eval      D2Cu06  = $VPAxx( 6)
145700060831     c                   eval      D2Cu07  = $VPAxx( 7)
145800060831     c                   eval      D2Cu08  = $VPAxx( 8)
145900060831     c                   eval      D2Cu09  = $VPAxx( 9)
146000060831     c                   eval      D2Cu10  = $VPAxx(10)
146100060831     c                   eval      D2Cu11  = $VPAxx(11)
146200060831     c                   eval      D2Cu12  = $VPAxx(12)
146300060831     c                   eval      D2Cu13  = $VPAxx(13)
146400060831     c                   eval      D2Cu14  = $VPAxx(14)
146500060831     c                   eval      D2Cu15  = $VPAxx(15)
146600060831     c                   eval      D2Cu16  = $VPAxx(16)
146700060831     c                   eval      D2Cu17  = $VPAxx(17)
146800060831     c                   eval      D2Cu18  = $VPAxx(18)
146900060831     c                   eval      D2Cu19  = $VPAxx(19)
147000060831     c                   eval      D2Cu20  = $VPAxx(20)
147100060831     c                   eval      D2Cu21  = $VPAxx(21)
147200060831     c                   eval      D2Cu22  = $VPAxx(22)
147300060831     c                   eval      D2Cu23  = $VPAxx(23)
147400060831     c                   eval      D2Cu24  = $VPAxx(24)
147500060831     c                   eval      D2Cu25  = $VPAxx(25)
147600060831     c                   eval      D2Cu26  = $VPAxx(26)
147700060831     c                   eval      D2Cu27  = $VPAxx(27)
147800060831     c                   eval      D2Cu28  = $VPAxx(28)
147900060831     c                   eval      D2Cu29  = $VPAxx(29)
148000060831     c                   eval      D2Cu30  = $VPAxx(30)
148100060831     c                   eval      D2Cu31  = $VPAxx(31)
148200060831     c                   eval      D2Cu32  = $VPAxx(32)
148300060831     c                   eval      D2Cu33  = $VPAxx(33)
148400060831     c                   eval      D2Cu34  = $VPAxx(34)
148500060831     c                   eval      D2Cu35  = $VPAxx(35)
148600060831     c                   eval      D2Cu36  = $VPAxx(36)
148700060831     c                   eval      D2Cu37  = $VPAxx(37)
148800060831     c                   eval      D2Cu38  = $VPAxx(38)
148900060831     c                   eval      D2Cu39  = $VPAxx(39)
149000060831     c                   eval      D2Cu40  = $VPAxx(40)
149100060831     c                   eval      D2Cu41  = $VPAxx(41)
149200060831     c                   eval      D2Cu42  = $VPAxx(42)
149300060831     c                   eval      D2Cu43  = $VPAxx(43)
149400060831     c                   eval      D2Cu44  = $VPAxx(44)
149500060831     c                   eval      D2Cu45  = $VPAxx(45)
149600060831     c                   eval      D2Cu46  = $VPAxx(46)
149700060831     c                   eval      D2Cu47  = $VPAxx(47)
149800060831     c                   eval      D2Cu48  = $VPAxx(48)
149900060831     c                   eval      D2Cu49  = $VPAxx(49)
150000060831     c                   eval      D2Cu50  = $VPAxx(50)
150100060831     c                   eval      D2Cu51  = $VPAxx(51)
150200060831     c                   eval      D2Cu52  = $VPAxx(52)
150300060831     c                   eval      D2Cu53  = $VPAxx(53)
150400060831     c                   eval      D2Cu54  = $VPAxx(54)
150500060831     c                   eval      D2Cu55  = $VPAxx(55)
150600060831     c                   eval      D2Cu56  = $VPAxx(56)
150700060831     c                   eval      D2Cu57  = $VPAxx(57)
150800060831     c                   eval      D2Cu58  = $VPAxx(58)
150900060831     c                   eval      D2Cu59  = $VPAxx(59)
151000060831     c                   eval      D2Cu60  = $VPAxx(60)
151100060830      *
151200060830      * Test OverFlow
151300060830if  4c                   if        *in01
151400060929     c                   write     MS08PT
151500060830     c                   eval      *in01   = *off
151600060830e   4c                   endif
151700060830      *
151800060830      * Stampa riga di dettaglio
151900060929     c                   write     MS08D2
152000060830      *
152100060830e   3c                   ENDIF
152200060830      *
152300060830e   2c                   ENDDO
152400060830      *
152500060830      * Stampa riga di chiusura
152600060929     c                   write     MS08RI
152700060830      *
152800060830e   1c                   ENDIF
152900060830      *
153000060830      * PROSPETTO STATISTICHE  - - - - - - - - - - - - - - - - - - - -*
153100060830      *
153200060830      * Stampa testata
153300060929     c                   write     MS08PT                                       *testata
153400060830      *
153500060830      * Ciclo di lettura e stampa dei dati del sfile
153600060929do  1c                   DO        LASTnrr       S01nrr
153700060830      *
153800060929     c     S01nrr        chain     MS08S01
153900060830      *
154000060830      * Elaborazione delle sole righe RIEPILOGO e GENERALE
154100060830if  2c                   IF             $Vtip(V2Hv) = 'R'
154200060830     c                             or   $Vtip(V2Hv) = 'G'
154300060830      *
154400060830sel 3c                   SELECT
154500060830      *
154600060830      * Stampa per dettaglio standard
154700060830w   3c                   WHEN      §MTPtds = *blanks
154800060830     c                   eval      D3Cdes  = V2Sdes
154900060830     c                   eval      D3Corl  = V2Sorl
155000060830      * - Statistiche LAVORAZIONE
155100060830     c                   eval      D3Ccla  = VPesInd(V2Hv)
155200060830     c                   eval      D3Csor  = VPusSor(V2Hv)
155300060830     c                   eval      D3Cdna  = VOdsDna(V2Hv)
155400060830     c                   eval      D3Cerr  = VErrMac(V2Hv)
155500060830      * - Statistiche LETTURE
155600060830     c                   eval      D3Ccsd  = Vcsd(V2Hv)
155700060830     c                   eval      D3Cps2  = Vps2dok(V2Hv)
155800060830     c                   eval      D3Cccv  = Vccv(V2Hv)
155900060830     c                   eval      D3Cccp  = Vccp(V2Hv)
156000060830     c                   eval      D3Cnls  = Vnls(V2Hv)
156100060830     c                   eval      D3Cnlp  = Vnlp(V2Hv)
156200060830     c                   eval      D3Cnmi  = Vxnomis(V2Hv)
156300060830     c                   eval      D3Cnpe  = Vxnopes(V2Hv)
156400060830      * - Test OverFlow
156500060830if  4c                   if        *in01
156600060929     c                   write     MS08PT
156700060830     c                   eval      *in01   = *off
156800060830e   4c                   endif
156900060830      * - Stampa riga di dettaglio
157000060929     c                   write     MS08D3
157100060830      *
157200060830      * Stampa per dettaglio tipo Disk C
157300060830w   3c                   WHEN      §MTPtds = 'D'
157400060830     c                   eval      D4Cdes  = V2Sdes
157500060830     c                   eval      D4Corl  = V2Sorl
157600060830      * - Statistiche LAVORAZIONE
157700060830     c                   eval      D4Cind  = VPesInd(V2Hv)
157800060830     c                   eval      D4Csor  = VPusSor(V2Hv)
157900060830     c                   eval      D4Clos  = VPusLos(V2Hv)
158000060830     c                   eval      D4Cnes  = VPusNes(V2Hv)
158100060830      * - Statistiche LETTURA
158200060830     c                   eval      D4Cbar  = Vbar(V2Hv)
158300060830     c                   eval      D4Cchi  = Vchi(V2Hv)
158400060830      * - Test OverFlow
158500060830if  4c                   if        *in01
158600060929     c                   write     MS08PT                                       *testata
158700060830     c                   eval      *in01   = *off
158800060830e   4c                   endif
158900060830      * - Stampa riga di dettaglio
159000060929     c                   write     MS08D4
159100060830      *
159200060830      * Stampa per dettaglio tipo PICKING
159300060830w   3c                   WHEN      §MTPtds = 'K'
159400060830     c                   eval      D5Cdes  = V2Sdes
159500060830     c                   eval      D5Corl  = V2Sorl
159600060830      * - Statistiche LAVORAZIONE
159700060830     c                   eval      D5Cind  = VPesInd(V2Hv)
159800060830      * - Statistiche LETTURA
159900060830     c                   eval      D5Cbar  = VbarP(V2Hv)
160000060830     c                   eval      D5Cldv  = Vldv(V2Hv)
160100060830     c                   eval      D5Cerr  = VPusLos(V2Hv)
160200060830      * - Test OverFlow
160300060830if  4c                   if        *in01
160400060929     c                   write     MS08PT                                       *testata
160500060830     c                   eval      *in01   = *off
160600060830e   4c                   endif
160700060830      * - Stampa riga di dettaglio
160800060929     c                   write     MS08D5
160900060830      *
161000060830e   3c                   ENDSL
161100060830      *
161200060830e   2c                   ENDIF
161300060830      *
161400060830e   1c                   ENDDO                                                  *fine sfile
161500060830      *
161600060830      * Stampa riga di chiusura
161700060929     c                   write     MS08RI
161800060830      *
161900060830      * Stampa riga di fine prospetto
162000060929     c                   write     MS08FI
162100060830      *
162200060830      * Chiusura file di stampa
162300060929     c                   close     FIMS08P
162400060830      *
162500060830     c                   ENDSR
162600060830
162700060830      *---------------------------------------------------------------*
162800060831      *?Tasto funzionale F22 -> Coda di stampa                       ?*
162900060830      *---------------------------------------------------------------*
163000060830     c     F22ges        BEGSR
163100060830      *
163200060830      * esegue il comando di gestione coda di stampa associata
163300060830     c                   call      'QCMDEXC'
163400060830     c                   parm      'WRKSPLF  '   Qcmd
163500060901     c                   parm      80            Qlen
163600060830      *
163700060830     c                   ENDSR
163800060830
163900060830      *---------------------------------------------------------------*
164000060831      *?Gestione opzioni della seconda videata (subfile)             ?*
164100060830      *---------------------------------------------------------------*
164200061002     c     OpzS01        BEGSR
164300060830      *
164400060830      * Ciclo di lettura del sfl
164500060929     c                   eval      S01nrr = 1
164600060929     c                   readc     MS08S01
164700060830      *
164800060929do  1c                   DOW       NOT %eof(FIMS08D)
164900060830      *
165000060830      * - Gestione opzioni
165100060830     c                   clear                   SAVnrr
165200060830sel 2c                   select
165300060830      *   5 = Dettaglio
165400060830w   2c                   when      V2Sopz = '5 '
165500060929     c                   eval      SAVnrr = S01nrr
165600060830     c                   exsr      O05ges
165700060830      *   7 = Statistiche
165800060830w   2c                   when      V2Sopz = '7 '
165900060929     c                   eval      SAVnrr = S01nrr
166000060830     c                   exsr      O07ges
166100060830e   2c                   endsl
166200060830      *
166300060830      * - Riposizionamento nel sfl
166400060830if  2c                   if        SAVnrr > *zeros
166500060929     c                   eval      S01nrr = SAVnrr
166600060929     c     S01nrr        chain     MS08S01
166700061002     c                   eval      *in28  = *off
166800060830     c                   clear                   V2Sopz
166900061002     c                   exsr      UpdS01
167000060830e   2c                   endif
167100060830      *
167200060929     c                   readc     MS08S01
167300060830      *
167400060830e   1c                   ENDDO
167500060830      *
167600060830     c                   ENDSR
167700060830
167800060830      *---------------------------------------------------------------*
167900060831      *?Opzione 5 -> Dettaglio dati                                  ?*
168000060830      *---------------------------------------------------------------*
168100060830     c     O05ges        BEGSR
168200060830      *
168300060830     c                   eval      $Video = '3'
168400060830      *
168500060830      * Inizializzazione videata
168600061002     c                   exsr      InzD01
168700060830      *
168800060830     c                   ENDSR
168900060830
169000060830      *---------------------------------------------------------------*
169100060831      *?Opzione 7 -> Statistiche                                     ?*
169200060830      *---------------------------------------------------------------*
169300060830     c     O07ges        BEGSR
169400060830      *
169500060830      * Test del tipo applicazione per decidere quale dettaglio
169600060830      *   statistiche emettere
169700060830sel 1c                   select
169800060830      * - Standard
169900060830w   1c                   when      §MTPtds = *blanks
170000060830     c                   eval      $Video  = '4'
170100061002     c                   exsr      InzD02
170200060830      * - Disk C
170300060830w   1c                   when      §MTPtds = 'D'
170400060830     c                   eval      $Video  = '5'
170500061002     c                   exsr      InzD03
170600060830      * - Picking
170700060830w   1c                   when      §MTPtds = 'K'
170800060830     c                   eval      $Video  = '6'
170900061002     c                   exsr      InzD04
171000060830      *
171100060830e   1c                   endsl
171200060830      *
171300060830     c                   ENDSR
171400060830
171500060830      *---------------------------------------------------------------*
171600061002      *?Aggiornamento riga del subfile                               ?*
171700060830      *---------------------------------------------------------------*
171800061002     c     UpdS01        BEGSR
171900060830      *
172000060830      * Impostazione indicatori per dspatr
172100060830      * - Evidenziazione riga di riepilogo
172200061002     c                   eval      *in40  = (V2Sdes <> *blanks)
172300060830      *
172400061002     c                   eval      C01rcd = S01nrr
172500061002     c*** NO sflnxtchg!  eval      *in32  = *on
172600060929     c                   UPDATE    MS08S01
172700060830      *
172800060830     c                   ENDSR
172900060830
173000060830      *---------------------------------------------------------------*
173100061002      *?Inizializzazione videata col DETTAGLIO                       ?*
173200060830      *---------------------------------------------------------------*
173300061002     c     InzD01        BEGSR
173400060830      *
173500061002     c                   clear                   MS08D01
173600060830      *
173700060830      * campi da sfile
173800061002     c                   eval      V3Ddes = V2Sdes
173900061002     c                   eval      V3Dorl = V2Sorl
174000060830      *
174100060830     c                   if        §KPIpba    <> *zeros
174200061002     c                   eval      V3txtE =  'E N T R A T E   '
174300060830     c                                    + '(MEDIA obiettivo = '
174400060830     c                                    + %trim(%editw(§KPIpba:'0 '))
174500060830     c                                    + ' colli/min.)'
174600060830     c                   else
174700061002     c                   eval      V3txtE = 'E N T R A T E   '
174800060830     c                   endif
174900060830      *
175000060830      * Nomi entrate (non la 20ª)
175100060904     c                   eval      $nro       =  1
175200060904     c                   DOW       $nro       <  %elem($Nmsl)
175300060904     c                   eval      xw         =  1
175400060901     c     $nro          lookup    $Nmsl(xw)                              97
175500060830     c   97              move      $msl(xw)      $V3ent($nro)
175600060904     c                   eval      $nro       =  $nro + 1
175700060830     c                   ENDDO
175800060830     c                   movea     $V3ent        $V3ent_ds
175900060830      *
176000060830      * Impostazione indice schiera
176100060830     c                   eval      xv         =  V2hv
176200061024      *
176300060830      * Campi da schiere:
176400061024      *
176500060830      * - Entrate
176600060904     c*** già così:      clear                   $V3DE
176700060830     c                   clear                   xx
176800060830do  1c                   DOW       xx         <  %elem($VPExx_ds)
176900060831     c                   eval      xx         =  xx + 1
177000060830     c     xx            occur     $VPExx_ds
177100060830     c                   eval      $V3DE(xx)  =  $VPExx(xv)
177200060830e   1c                   ENDDO
177300060904     c*** già compresa:  movea     $V3DE         $V3DExx_ds
177400061024      *
177500061024      * - - Ripartizione dei colli della post.20 tra gli altri vari
177600061024      *     banchi SE riga di riepilogo o totale
177700061024      *    ?N.B.?- Il totale dei colli (in base a cui ripartire quelli
177800061024      *     della post.20 su tutte le altre post.) va calcolato ora,
177900061024      *     PRIMA di modificare la schiera $V3de con la subroutine
178000061024      *     "RipartB20det" (cioè PRIMA di iniziarne la ripartizione).
178100061024     c                   eval      wRipColli = %xfoot($V3de) - $V3de(20)
178200061024if  2c                   if           ($Vtip(xv) =  'R'                         *Riep
178300061024     c                             or  $Vtip(xv) =  'G')                        *Gen
178400061024     c                             and wRipColli >  *zeros
178500061024     c                   exsr      RipartB20det
178600061024e   2c                   endif
178700011005      *
178800060830      * - Media entrate in base ai minuti lavorati per entrata
178900060830if  1c                   IF        $Vtip(xv)  <> 'M'                            *no dettaglio
179000060830      *
179100060904     c*** già così:      clear                   $V3DM
179200060830     c                   clear                   xx
179300060830do  2c                   DOW       xx         <  %elem($VMLAVxx_ds)
179400060831     c                   eval      xx         =  xx + 1
179500060830     c     xx            occur     $VMLAVxx_ds
179600060904if  3c                   if        $VMLAVxx(xv)  > *zeros
179700061024      * - - Utilizzo il numero colli per postazione già "allineato" dalla
179800061024      *     subr. "RipartB20det"
179900061024      *     (i dati "originali" hanno ancora i colli nella post.20)
180000061024     c***  xx            occur     $VPExx_ds
180100061024     c***                eval      $V3DM(xx)  =  $VPExx(xv) / $VMLAVxx(xv)
180200061205     c**!!!              eval(h)   $V3DM(xx)  =  $V3DE(xx) / $VMLAVxx(xv)
180300061205     c                   eval(h)   w0050      =  $V3DE(xx) / $VMLAVxx(xv)
180400061205     c                   if        w0050 > 99
180500061205     c                   eval      $v3dm(xx) = 99
180600061205     c                   else
180700061205     c                   eval      $v3dm(xx) = w0050
180800061205     c                   endif
180900061205
181000060904e   3c                   endif
181100060830e   2c                   ENDDO
181200060904     c*** già compresa:  movea     $V3DM         $V3DMxx_ds
181300060830      *
181400000000e   1c                   ENDIF
181500060830      *
181600060831      * - Uscite
181700060831     c     xv            occur     $VPAxx_ds
181800060901     c                   eval      V3Du01  =  $VPAxx( 1)
181900060901     c                   eval      V3Du02  =  $VPAxx( 2)
182000060901     c                   eval      V3Du03  =  $VPAxx( 3)
182100060901     c                   eval      V3Du04  =  $VPAxx( 4)
182200060901     c                   eval      V3Du05  =  $VPAxx( 5)
182300060901     c                   eval      V3Du06  =  $VPAxx( 6)
182400060901     c                   eval      V3Du07  =  $VPAxx( 7)
182500060901     c                   eval      V3Du08  =  $VPAxx( 8)
182600060901     c                   eval      V3Du09  =  $VPAxx( 9)
182700060901     c                   eval      V3Du10  =  $VPAxx(10)
182800060901     c                   eval      V3Du11  =  $VPAxx(11)
182900060901     c                   eval      V3Du12  =  $VPAxx(12)
183000060901     c                   eval      V3Du13  =  $VPAxx(13)
183100060901     c                   eval      V3Du14  =  $VPAxx(14)
183200060901     c                   eval      V3Du15  =  $VPAxx(15)
183300060901     c                   eval      V3Du16  =  $VPAxx(16)
183400060901     c                   eval      V3Du17  =  $VPAxx(17)
183500060901     c                   eval      V3Du18  =  $VPAxx(18)
183600060901     c                   eval      V3Du19  =  $VPAxx(19)
183700060901     c                   eval      V3Du20  =  $VPAxx(20)
183800060901     c                   eval      V3Du21  =  $VPAxx(21)
183900060901     c                   eval      V3Du22  =  $VPAxx(22)
184000060901     c                   eval      V3Du23  =  $VPAxx(23)
184100060901     c                   eval      V3Du24  =  $VPAxx(24)
184200060901     c                   eval      V3Du25  =  $VPAxx(25)
184300060901     c                   eval      V3Du26  =  $VPAxx(26)
184400060901     c                   eval      V3Du27  =  $VPAxx(27)
184500060901     c                   eval      V3Du28  =  $VPAxx(28)
184600060901     c                   eval      V3Du29  =  $VPAxx(29)
184700060901     c                   eval      V3Du30  =  $VPAxx(30)
184800060901     c                   eval      V3Du31  =  $VPAxx(31)
184900060901     c                   eval      V3Du32  =  $VPAxx(32)
185000060901     c                   eval      V3Du33  =  $VPAxx(33)
185100060901     c                   eval      V3Du34  =  $VPAxx(34)
185200060901     c                   eval      V3Du35  =  $VPAxx(35)
185300060901     c                   eval      V3Du36  =  $VPAxx(36)
185400060901     c                   eval      V3Du37  =  $VPAxx(37)
185500060901     c                   eval      V3Du38  =  $VPAxx(38)
185600060901     c                   eval      V3Du39  =  $VPAxx(39)
185700060901     c                   eval      V3Du40  =  $VPAxx(40)
185800060901     c                   eval      V3Du41  =  $VPAxx(41)
185900060901     c                   eval      V3Du42  =  $VPAxx(42)
186000060901     c                   eval      V3Du43  =  $VPAxx(43)
186100060901     c                   eval      V3Du44  =  $VPAxx(44)
186200060901     c                   eval      V3Du45  =  $VPAxx(45)
186300060901     c                   eval      V3Du46  =  $VPAxx(46)
186400060901     c                   eval      V3Du47  =  $VPAxx(47)
186500060901     c                   eval      V3Du48  =  $VPAxx(48)
186600060901     c                   eval      V3Du49  =  $VPAxx(49)
186700060901     c                   eval      V3Du50  =  $VPAxx(50)
186800060901     c                   eval      V3Du51  =  $VPAxx(51)
186900060901     c                   eval      V3Du52  =  $VPAxx(52)
187000060901     c                   eval      V3Du53  =  $VPAxx(53)
187100060901     c                   eval      V3Du54  =  $VPAxx(54)
187200060901     c                   eval      V3Du55  =  $VPAxx(55)
187300060901     c                   eval      V3Du56  =  $VPAxx(56)
187400060901     c                   eval      V3Du57  =  $VPAxx(57)
187500060901     c                   eval      V3Du58  =  $VPAxx(58)
187600060901     c                   eval      V3Du59  =  $VPAxx(59)
187700060901     c                   eval      V3Du60  =  $VPAxx(60)
187800060830      *
187900060830     c                   ENDSR
188000060830
188100060831      *---------------------------------------------------------------*
188200060831      *?Gestione terza videata (dettaglio dati)                      ?*
188300060831      *---------------------------------------------------------------*
188400060929     c     GesD01        BEGSR
188500060831      *
188600060831      * Emissione testata e descrizione tasti funzionali
188700061002     c                   write     MS08T01
188800061003     c                   write     MS08Z02
188900060831      * Emissione videata (già inizializzata)
189000061002     c                   exfmt     MS08D01
189100060831      *
189200060831sel 1c                   select
189300060831      * F3 = Fine programma
189400060831w   1c                   when      *inKC
189500060831     c                   exsr      F03ges
189600060831      * F12 = Ritorno
189700060831w   1c                   when      *inKL
189800060831     c                   eval      $Video  = '2'
189900060831      *
190000060831e   1c                   endsl
190100060831      *
190200060831     c                   ENDSR
190300060831
190400060831      *---------------------------------------------------------------*
190500061002      *?Inizializzazione videata STATISTICHE STANDARD                ?*
190600060831      *---------------------------------------------------------------*
190700061002     c     InzD02        BEGSR
190800060831      *
190900061002     c                   clear                   MS08D02
191000060831      *
191100060831     c                   eval      V4Ddes  = V2Sdes
191200060831     c                   eval      V4Dorl  = V2Sorl
191300060831      *
191400060831      * Statistiche LAVORAZIONE
191500060831     c                   eval      V4Dcla  = Vpesind(V2Hv)
191600060831     c                   eval      V4Dsor  = Vpussor(V2Hv)
191700060831     c                   eval      V4Ddna  = Vodsdna(V2Hv)
191800060831     c                   eval      V4Derr  = Verrmac(V2Hv)
191900060831      *
192000060831      * Statistiche LETTURE
192100060831     c                   eval      V4Dcsd  = Vcsd(V2Hv)
192200060831     c                   eval      V4Dps2  = Vps2dok(V2Hv)
192300060831     c                   eval      V4Dccv  = Vccv(V2Hv)
192400060831     c                   eval      V4Dccp  = Vccp(V2Hv)
192500060831     c                   eval      V4Dnls  = Vnls(V2Hv)
192600060831     c                   eval      V4Dnlp  = Vnlp(V2Hv)
192700060831     c                   eval      V4Dnmi  = Vxnomis(V2Hv)
192800060831     c                   eval      V4Dnpe  = Vxnopes(V2Hv)
192900060831      *
193000060831     c                   ENDSR
193100060831
193200060831      *---------------------------------------------------------------*
193300061002      *?Inizializzazione videata STATISTICHE DISK C                  ?*
193400060831      *---------------------------------------------------------------*
193500061002     c     InzD03        BEGSR
193600060831      *
193700061002     c                   clear                   MS08D03
193800060831      *
193900060831     c                   eval      V5Ddes  = V2Sdes
194000060831     c                   eval      V5Dorl  = V2Sorl
194100060831      *
194200060831      * Statistiche LAVORAZIONE
194300060831     c                   eval      V5Dind  = Vpesind(V2Hv)
194400060831     c                   eval      V5Dsor  = Vpussor(V2Hv)
194500060901     c                   eval      V5Dlos  = Vpuslos(V2Hv)
194600060831     c                   eval      V5Dnes  = Vpusnes(V2Hv)
194700060831      *
194800060831      * Statistiche LETTURE
194900060831     c                   eval      V5Dchi  = Vchi(V2Hv)
195000060831     c                   eval      V5Dbar  = Vbar(V2Hv)
195100060831      *
195200060831     c                   ENDSR
195300060831
195400060831      *---------------------------------------------------------------*
195500061002      *?Inizializzazione videata STATISTICHE PICKING                 ?*
195600060831      *---------------------------------------------------------------*
195700061002     c     InzD04        BEGSR
195800060831      *
195900061002     c                   clear                   MS08D04
196000060831      *
196100060831     c                   eval      V6Ddes  = V2Sdes
196200060831     c                   eval      V6Dorl  = V2Sorl
196300060831      *
196400060831      * Statistiche LAVORAZIONE
196500060831     c                   eval      V6Dind  = Vpesind(V2Hv)
196600060831      *
196700060831      * Statistiche LETTURE
196800060831     c                   eval      V6Dbar  = VbarP(V2Hv)
196900060831     c                   eval      V6Dldv  = Vldv(V2Hv)
197000060831     c                   eval      V6Derr  = Vpuslos(V2Hv)
197100060831      *
197200060831     c                   ENDSR
197300060831
197400060831      *---------------------------------------------------------------*
197500061002      *?Gestione videate STATISTICHE (STANDARD / DISK C / PICKING)   ?*
197600060831      *---------------------------------------------------------------*
197700060929     c     GesD0234      BEGSR
197800060831      *
197900061003      * Emissione testata e descrizione tasti funzionali
198000061002     c                   write     MS08T01
198100061003     c                   write     MS08Z02
198200061003      * Emissione videata (già inizializzata)
198300060831sel 1c                   select
198400060831w   1c                   when      $Video  = '4'
198500061002     c                   exfmt     MS08D02
198600060831w   1c                   when      $Video  = '5'
198700061002     c                   exfmt     MS08D03
198800061002w   1c                   when      $Video  = '6'
198900061002     c                   exfmt     MS08D04
199000060831e   1c                   endsl
199100060831      *
199200060831sel 1c                   select
199300060831      * F3 = Fine programma
199400060831w   1c                   when      *inKC
199500060831     c                   exsr      F03ges
199600060831      * F12 = Ritorno
199700060831w   1c                   when      *inKL
199800060831     c                   eval      $Video  = '2'
199900060831      *
200000060831e   1c                   endsl
200100060831      *
200200060831     c                   ENDSR
200300060831
200400060831      *---------------------------------------------------------------*
200500060831      *?Memorizzazione dati del file in schiere                      ?*
200600060831      *---------------------------------------------------------------*
200700060831     c     sr_MemoSch    BEGSR
200800080909      /free
200900080909           reset  $TooManyRec;
201000080909      /end-free
201100060831      *
201200060831      * Memorizzazione schiere dal file SALDI LETTURE
201300060831     c                   exsr      sr_MEMmsa
201400060831      *
201500060831      * SE reperiti dati in FNMSA00F:
201600060831if  1c                   IF        $FNMSA = *on
201700060831      *
201800060831      * Memorizzazione schiere dal file SALDI LETTURE x giorno prec.
201900060831     c                   exsr      sr_MEMmsaPR
202000080909      /free
202100080909           if  $TooManyRec = *on;
202200080909             leavesr;
202300080909           endif;
202400080909      /end-free
202500060831      *
202600060831      * Creazione GRIGLIA di rappresentazione
202700060831     c                   exsr      sr_CrtGriglia
202800080909      /free
202900080909           if  $TooManyRec = *on;
203000080909             leavesr;
203100080909           endif;
203200080909      /end-free
203300060831      *
203400060831      * Valorizzazione schiere video dalla griglia di rappresentazione
203500060831     c                   exsr      sr_ValScVid
203600080909      /free
203700080909           if  $TooManyRec = *on;
203800080909             leavesr;
203900080909           endif;
204000080909      /end-free
204100060831      *
204200060831      * Calcolo valori statistici
204300060831     c                   exsr      sr_CalStVid
204400060831      *
204500000000e   1c                   ENDIF
204600060831      *
204700060831     c                   ENDSR
204800060831
204900060831      *---------------------------------------------------------------*
205000060831      *?Memorizzazione dati dal file SALDI LETTURE VDL               ?*
205100060831      *---------------------------------------------------------------*
205200060831     c     sr_MEMmsa     BEGSR
205300060831      *
205400060831      * Pulizia delle variabili di lavoro
205500060831     c                   eval      $FNMSA  = *off
205600060831     c                   clear                   xi
205700060831     c                   clear                   $dtl
205800060831     c                   clear                   $orl
205900060831     c                   clear                   $ncl
206000061006     c***                clear                   $PEPxx_ds
206100061006     c***                clear                   $PEPxx
206200061006     c                   clear                   xw
206300061006do  1c                   DOW       xw       < %elem($PEPxx_ds)
206400061006     c                   add       1             xw
206500061006     c     xw            occur     $PEPxx_ds
206600061006     c                   clear                   $PEPxx
206700061006e   1c                   ENDDO
206800060831     ***c                   clear                   Mpe1
206900060831     ***c                   clear                   Mpe2
207000060831     ***c                   clear                   Mpe3
207100060831     ***c                   clear                   Mpe4
207200060831     ***c                   clear                   Mpe5
207300060831     ***c                   clear                   Mpe6
207400060831     ***c                   clear                   Mpe7
207500060831     ***c                   clear                   Mpe8
207600060831     ***c                   clear                   Mpe9
207700060831     ***c                   clear                   Mpe0
207800060831     ***c                   clear                   Mpe11
207900060831     ***c                   clear                   Mpe12
208000060831     ***c                   clear                   Mpe13
208100060831     ***c                   clear                   Mpe14
208200060831     ***c                   clear                   Mpe15
208300060831     ***c                   clear                   Mpe16
208400060831     ***c                   clear                   Mpe17
208500060831     ***c                   clear                   Mpe18
208600060831     ***c                   clear                   Mpe19
208700060831     ***c                   clear                   Mpe20
208800061006     c***                clear                   $PADxx_ds
208900061006     c***                clear                   $PADxx
209000061006     c                   clear                   xw
209100061006do  1c                   DOW       xw       < %elem($PADxx_ds)
209200061006     c                   add       1             xw
209300061006     c     xw            occur     $PADxx_ds
209400061006     c                   clear                   $PADxx
209500061006e   1c                   ENDDO
209600060831     ***c                   clear                   Mpa1
209700060831     ***c                   clear                   Mpa2
209800060831     ***c                   clear                   Mpa3
209900060831     ***c                   clear                   Mpa4
210000060831     ***c                   clear                   Mpa5
210100060831     ***c                   clear                   Mpa6
210200060831     ***c                   clear                   Mpa7
210300060831     ***c                   clear                   Mpa8
210400060831     ***c                   clear                   Mpa9
210500060831     ***c                   clear                   Mpa10
210600060831     ***c                   clear                   Mpa11
210700060831     ***c                   clear                   Mpa12
210800060831     ***c                   clear                   Mpa13
210900060831     ***c                   clear                   Mpa14
211000060831     ***c                   clear                   Mpa15
211100060831     ***c                   clear                   Mpa16
211200060831     ***c                   clear                   Mpa17
211300060831     ***c                   clear                   Mpa18
211400060831     ***c                   clear                   Mpa19
211500060831     ***c                   clear                   Mpa20
211600060831     ***c                   clear                   Mpa21
211700060831     ***c                   clear                   Mpa22
211800060831     ***c                   clear                   Mpa23
211900060831     ***c                   clear                   Mpa24
212000060831     ***c                   clear                   Mpa25
212100060831     ***c                   clear                   Mpa26
212200060831     ***c                   clear                   Mpa27
212300060831     ***c                   clear                   Mpa28
212400060831     ***c                   clear                   Mpa29
212500060831     ***c                   clear                   Mpa30
212600060831     ***c                   clear                   Mpa31
212700060831     ***c                   clear                   Mpa32
212800060831     ***c                   clear                   Mpa33
212900060831     ***c                   clear                   Mpa34
213000060831     ***c                   clear                   Mpa35
213100060831     ***c                   clear                   Mpa36
213200060831     ***c                   clear                   Mpa37
213300060831     ***c                   clear                   Mpa38
213400060831     ***c                   clear                   Mpa39
213500060831     ***c                   clear                   Mpa40
213600060831     ***c                   clear                   Mpa41
213700060831     ***c                   clear                   Mpa42
213800060831     ***c                   clear                   Mpa43
213900060831     ***c                   clear                   Mpa44
214000060831     ***c                   clear                   Mpa45
214100060831     ***c                   clear                   Mpa46
214200060831     ***c                   clear                   Mpa47
214300060831     ***c                   clear                   Mpa48
214400060831     ***c                   clear                   Mpa49
214500060831     ***c                   clear                   Mpa50
214600060831     ***c                   clear                   Mpa51
214700060831     ***c                   clear                   Mpa52
214800060831     ***c                   clear                   Mpa53
214900060831     ***c                   clear                   Mpa54
215000060831     ***c                   clear                   Mpa55
215100060831     ***c                   clear                   Mpa56
215200060831     ***c                   clear                   Mpa57
215300060831     ***c                   clear                   Mpa58
215400060831     ***c                   clear                   Mpa59
215500060831     ***c                   clear                   Mpa60
215600060831     c                   clear                   $pesind
215700060831     c                   clear                   $pussor
215800060831     c                   clear                   $puslos
215900060831     c                   clear                   $pusnes
216000060831     c                   clear                   $csd
216100060831     c                   clear                   $ps2dok
216200060831     c                   clear                   $ccv
216300060831     c                   clear                   $ccp
216400060831     c                   clear                   $nls
216500060831     c                   clear                   $nlp
216600060831     c                   clear                   $xnomis
216700060831     c                   clear                   $xnopes
216800060831     c                   clear                   $odsdna
216900060831     c                   clear                   $errmac
217000060831     c                   clear                   $pesind2
217100060831     c                   clear                   $pussor2
217200060831     c                   clear                   $odsdna2
217300060831     c                   clear                   $errmac2
217400060831     c                   clear                   ULTmsa
217500040405     c                   reset                   W00tli
217600040405     c                   reset                   W00tlf
217700060831      *
217800060831      * Ciclo di lettura dati del foglio da caricare
217900060831     c                   eval      MSAapl  = D00apl
218000060831     c                   eval      MSAfil  = D00fil
218100060831     c                   eval      MSAdfv  = D00dfv
218200060831     c                   eval      MSAnpg  = D00npg
218300170201     c     K04MSA12      setll     FNMSA000
218400060831      *
218500060831do  1c                   DO        *hival
218600060831      *
218700170201     c     K04MSA12      reade     FNMSA000
218800060831      *
218900170201if  2c                   if        %eof(FNMSA12L)
219000060831     c                   leave
219100060831e   2c                   endif
219200060831      *
219300060831     c                   exsr      ChkMSA
219400060831if  2c                   if        $RecOK  = *off
219500060831     c                   iter
219600060831e   2c                   endif
219700080909      /free
219800080909       // - TROPPI dati !!!
219900080909           if  xi >= %elem($dtl);
220000080909             $TooManyRec = *on;
220100080909             $FNMSA      = *off;
220200080909             leavesr;
220300080909           endif;
220400080909      /end-free
220500060831      *
220600060831     c                   eval      $FNMSA  = *on
220700060831     c                   eval      xi      = xi + 1
220800060831      *
220900060831     c                   eval      $dtl(xi)     = MSAdtl
221000060831     c                   eval      $orl(xi)     = MSAorl
221100060831     c                   eval      $ncl(xi)     = MSApep001                     *colli lavorati
221200060831     c                                          + MSApep002
221300060831     c                                          + MSApep003
221400060831     c                                          + MSApep004
221500060831     c                                          + MSApep005
221600060831     c                                          + MSApep006
221700060831     c                                          + MSApep007
221800060831     c                                          + MSApep008
221900060831     c                                          + MSApep009
222000060831     c                                          + MSApep010
222100060831     c                                          + MSApep011
222200060831     c                                          + MSApep012
222300060831     c                                          + MSApep013
222400060831     c                                          + MSApep014
222500060831     c                                          + MSApep015
222600060831     c                                          + MSApep016
222700060831     c                                          + MSApep017
222800060831     c                                          + MSApep018
222900060831     c                                          + MSApep019
223000061024     c                                          + MSApep020                     ANCHE post.20!
223100060831      * - dettaglio entrate
223200060831     c     xi            occur     $PEPxx_ds
223300060901     c                   eval      $PEPxx( 1)   = MSApep001
223400060901     c                   eval      $PEPxx( 2)   = MSApep002
223500060901     c                   eval      $PEPxx( 3)   = MSApep003
223600060901     c                   eval      $PEPxx( 4)   = MSApep004
223700060901     c                   eval      $PEPxx( 5)   = MSApep005
223800060901     c                   eval      $PEPxx( 6)   = MSApep006
223900060901     c                   eval      $PEPxx( 7)   = MSApep007
224000060901     c                   eval      $PEPxx( 8)   = MSApep008
224100060901     c                   eval      $PEPxx( 9)   = MSApep009
224200060901     c                   eval      $PEPxx(10)   = MSApep010
224300060901     c                   eval      $PEPxx(11)   = MSApep011
224400060901     c                   eval      $PEPxx(12)   = MSApep012
224500060901     c                   eval      $PEPxx(13)   = MSApep013
224600060901     c                   eval      $PEPxx(14)   = MSApep014
224700060901     c                   eval      $PEPxx(15)   = MSApep015
224800060901     c                   eval      $PEPxx(16)   = MSApep016
224900060901     c                   eval      $PEPxx(17)   = MSApep017
225000060901     c                   eval      $PEPxx(18)   = MSApep018
225100060901     c                   eval      $PEPxx(19)   = MSApep019
225200060901     c                   eval      $PEPxx(20)   = MSApep020
225300060831      * - dettaglio uscite
225400060831     c     xi            occur     $PADxx_ds
225500060901     c                   eval      $PADxx( 1)   = MSApad001
225600060901     c                   eval      $PADxx( 2)   = MSApad002
225700060901     c                   eval      $PADxx( 3)   = MSApad003
225800060901     c                   eval      $PADxx( 4)   = MSApad004
225900060901     c                   eval      $PADxx( 5)   = MSApad005
226000060901     c                   eval      $PADxx( 6)   = MSApad006
226100060901     c                   eval      $PADxx( 7)   = MSApad007
226200060901     c                   eval      $PADxx( 8)   = MSApad008
226300060901     c                   eval      $PADxx( 9)   = MSApad009
226400060901     c                   eval      $PADxx(10)   = MSApad010
226500060901     c                   eval      $PADxx(11)   = MSApad011
226600060901     c                   eval      $PADxx(12)   = MSApad012
226700060901     c                   eval      $PADxx(13)   = MSApad013
226800060901     c                   eval      $PADxx(14)   = MSApad014
226900060901     c                   eval      $PADxx(15)   = MSApad015
227000060901     c                   eval      $PADxx(16)   = MSApad016
227100060901     c                   eval      $PADxx(17)   = MSApad017
227200060901     c                   eval      $PADxx(18)   = MSApad018
227300060901     c                   eval      $PADxx(19)   = MSApad019
227400060901     c                   eval      $PADxx(20)   = MSApad020
227500060901     c                   eval      $PADxx(21)   = MSApad021
227600060901     c                   eval      $PADxx(22)   = MSApad022
227700060901     c                   eval      $PADxx(23)   = MSApad023
227800060901     c                   eval      $PADxx(24)   = MSApad024
227900060901     c                   eval      $PADxx(25)   = MSApad025
228000060901     c                   eval      $PADxx(26)   = MSApad026
228100060901     c                   eval      $PADxx(27)   = MSApad027
228200060901     c                   eval      $PADxx(28)   = MSApad028
228300060901     c                   eval      $PADxx(29)   = MSApad029
228400060901     c                   eval      $PADxx(30)   = MSApad030
228500060901     c                   eval      $PADxx(31)   = MSApad031
228600060901     c                   eval      $PADxx(32)   = MSApad032
228700060901     c                   eval      $PADxx(33)   = MSApad033
228800060901     c                   eval      $PADxx(34)   = MSApad034
228900060901     c                   eval      $PADxx(35)   = MSApad035
229000060901     c                   eval      $PADxx(36)   = MSApad036
229100060901     c                   eval      $PADxx(37)   = MSApad037
229200060901     c                   eval      $PADxx(38)   = MSApad038
229300060901     c                   eval      $PADxx(39)   = MSApad039
229400060901     c                   eval      $PADxx(40)   = MSApad040
229500060901     c                   eval      $PADxx(41)   = MSApad041
229600060901     c                   eval      $PADxx(42)   = MSApad042
229700060901     c                   eval      $PADxx(43)   = MSApad043
229800060901     c                   eval      $PADxx(44)   = MSApad044
229900060901     c                   eval      $PADxx(45)   = MSApad045
230000060901     c                   eval      $PADxx(46)   = MSApad046
230100060901     c                   eval      $PADxx(47)   = MSApad047
230200060901     c                   eval      $PADxx(48)   = MSApad048
230300060901     c                   eval      $PADxx(49)   = MSApad049
230400060901     c                   eval      $PADxx(50)   = MSApad050
230500060901     c                   eval      $PADxx(51)   = MSApad051
230600060901     c                   eval      $PADxx(52)   = MSApad052
230700060901     c                   eval      $PADxx(53)   = MSApad053
230800060901     c                   eval      $PADxx(54)   = MSApad054
230900060901     c                   eval      $PADxx(55)   = MSApad055
231000060901     c                   eval      $PADxx(56)   = MSApad056
231100060901     c                   eval      $PADxx(57)   = MSApad057
231200060901     c                   eval      $PADxx(58)   = MSApad058
231300060901     c                   eval      $PADxx(59)   = MSApad059
231400060901     c                   eval      $PADxx(60)   = MSApad060
231500060831      *
231600060831      * Per statistiche:
231700060831      * - Lavorazione
231800060831     c                   eval      $pesind(xi)  = MSApesind
231900060831     c                   eval      $pussor(xi)  = MSApussor
232000060831     c                   eval      $puslos(xi)  = MSApuslos
232100060831     c                   eval      $pusnes(xi)  = MSApusnes
232200060831      * - Letture
232300060831     c                   eval      $csd(xi)     = MSAcsd                        *letti da scanner
232400060901     c                   eval      $ps2dok(xi)  = MSAps2dok                     *letti da pistola
232500060831     c                   eval      $ccv(xi)     = MSAccv                        *letti con volume
232600060831     c                   eval      $ccp(xi)     = MSAccp                        *letti con peso
232700060831      * - Anomalie
232800060831     c                   eval      $nls(xi)     = MSAnls                        *non letti scanner
232900060831     c                   eval      $nlp(xi)     = MSAnlp                        *non letti pistola
233000060831     c                   eval      $xnomis(xi)  = MSApussor - MSAccv            *C: non misurati
233100060831     c                   eval      $xnopes(xi)  = MSApussor - MSAccp            *C: non pesati
233200060831     c                   eval      $odsdna(xi)  = MSAodsdna                     *of x uscita piena
233300060831     c                   eval      $errmac(xi)  = MSApesind                     *of x errori machine
233400060831     c                                          - MSApussor
233500060831     c                                          - MSAodsdna
233600060608      *?*NEW?
233700060831     c                   eval      $pesind2(xi) = MSAps1dnr                     »caricati da VdL
233800060831     c                   eval      $pussor2(xi) = MSAps1dts                     »smistati da VdL
233900060831     c                   eval      $odsdna2(xi) = MSAps1dtl                     »of x uscita piena
234000060831     c                   eval      $errmac2(xi) = MSAps1nsc                     »of x errori machine
234100060831      *
234200060831e   1c                   ENDDO
234300060831      *
234400060831     c                   eval      ULTmsa  = xi
234500060831      *
234600060831     c                   ENDSR
234700060831
234800060831      *---------------------------------------------------------------*
234900060831      *?Memorizzazione dati dal file SALDI LETTURE VDL x giorno prec.?*
235000060831      *---------------------------------------------------------------*
235100060831     c     sr_MEMmsaPR   BEGSR
235200060831      *
235300060831      * Pulizia delle variabili di lavoro
235400061002     ***c                   eval      $FNMSApr     = *off
235500060831     c                   clear                   xi
235600060831     c                   clear                   $Pdtl                          *schiere memo
235700060831     c                   clear                   $Porl
235800060831     c                   clear                   $Pncl
235900060831     c                   clear                   ULTmsaPR
236000040405     c                   reset                   W00tli
236100040405     c                   reset                   W00tlf
236200060831      *
236300060831      * Ciclo di lettura dati del foglio del GIORNO PRECEDENTE da
236400060831      *   caricare
236500060831     c                   eval      MSAapl  = D00apl
236600060831     c                   eval      MSAfil  = D00fil
236700060831     c                   eval      MSAdfv  = D00dfvPR
236800060831     c                   eval      MSAnpg  = D00npg
236900170201     c     K04MSA12      setll     FNMSA000
237000170201     c     K04MSA12      reade     FNMSA000
237100060831      *
237200170201do  1c                   DOW       NOT %eof(FNMSA12L)
237300060831      *
237400060831     c                   exsr      ChkMSA
237500060831if  2c                   if        $RecOK  = *on
237600060831      *
237700080909      /free
237800080909       // - TROPPI dati !!!
237900080909           if  xi >= %elem($Pdtl);
238000080909             $TooManyRec = *on;
238100080909             $FNMSA      = *off;
238200080909             leavesr;
238300080909           endif;
238400080909      /end-free
238500061002     ***c                   eval      $FNMSApr     = *on
238600060831     c                   eval      xi      = xi + 1
238700060831     c                   eval      $Pdtl(xi)    = MSAdtl
238800060831     c                   eval      $Porl(xi)    = MSAorl
238900060831     c                   eval      $Pncl(xi)    = MSApep001                     *colli lavorati
239000060831     c                                          + MSApep002
239100060831     c                                          + MSApep003
239200060831     c                                          + MSApep004
239300060831     c                                          + MSApep005
239400060831     c                                          + MSApep006
239500060831     c                                          + MSApep007
239600060831     c                                          + MSApep008
239700060831     c                                          + MSApep009
239800060831     c                                          + MSApep010
239900060831     c                                          + MSApep011
240000060831     c                                          + MSApep012
240100060831     c                                          + MSApep013
240200060831     c                                          + MSApep014
240300060831     c                                          + MSApep015
240400060831     c                                          + MSApep016
240500060831     c                                          + MSApep017
240600060831     c                                          + MSApep018
240700060831     c                                          + MSApep019
240800070626     c                                          + MSApep020                     ANCHE post.20!
240900060831      *
241000060831e   2c                   endif
241100060831      *
241200170201     c     K04MSA12      reade     FNMSA000
241300060831      *
241400060831e   1c                   ENDDO
241500060831      *
241600060831     c                   eval      ULTmsaPR     = xi
241700060831      *
241800060831     c                   ENDSR
241900060831
242000060831      *---------------------------------------------------------------*
242100060831      *?Controlla validità record FNMSA                              ?*
242200060831      *---------------------------------------------------------------*
242300060831     c     ChkMSA        BEGSR
242400060831      *
242500060831     c                   reset                   $RecOk
242600040316      *
242700060831      * Parzializzazione (eventuale) per orario di lettura
242800060831if  1c                   IF            (D00oli <> *zeros
242900040316     c                              or  D00olf <> *zeros)
243000060831      *
243100040405      * Impostazione data+ora iniziale e finale di selezione
243200060831if  2c                   IF        W00tli  = *loval
243300060831      *
243400060831sel 3c                   select
243500040609      * - data richiesta
243600060831w   3c                   when      MSAdfv  = D00dfv
243700040609     c     *ISO          move      D00dli        Wdata
243800060831     c                   eval      n6      = D00oli * 100
243900040609     c     *ISO          move      n6            Wora
244000040609     c                   movel     Wtempo        W00tli                         *periodo INIZIALE
244100040609     c     *ISO          move      D00dlf        Wdata
244200060831     c                   eval      n6      = D00olf * 100
244300040609     c     *ISO          move      n6            Wora
244400040609     c                   movel     Wtempo        W00tlf                         *periodo FINALE
244500040609      * - data precedente (per confronto)
244600060831w   3c                   when      MSAdfv  = D00dfvPR
244700040609      * - - conto i giorni di differenza
244800040609     c                   clear                   Wdays
244900040609     c                   reset                   Wtempo
245000040609     c                   reset                   Wtempo2
245100040609     c     *ISO          move      D00dfv        Wdata
245200040609     c                   movel     Wtempo        Wtempo2
245300040609     c     *ISO          move      D00dfvPR      Wdata
245400060831do  4c                   dow       Wtempo  < Wtempo2
245500040609     c                   add       1             Wdays
245600040609     c                   adddur    1:*d          Wdata
245700040609e   4c                   enddo
245800040609      * - - imposto le date corrispondenti a quelle iniziale e finale
245900040609     c     *ISO          move      D00dli        Wdata
246000040609     c                   subdur    wDays:*d      Wdata
246100060831     c                   eval      n6      = D00oli * 100
246200040609     c     *ISO          move      n6            Wora
246300040609     c                   movel     Wtempo        W00tli                         *periodo INIZIALE
246400040609     c     *ISO          move      D00dlf        Wdata
246500040609     c                   subdur    wDays:*d      Wdata
246600060831     c                   eval      n6      = D00olf * 100
246700040609     c     *ISO          move      n6            Wora
246800040609     c                   movel     Wtempo        W00tlf                         *periodo FINALE
246900060831e   3c                   endsl
247000060831      *
247100060831e   2c                   ENDIF
247200040405      * Test orario
247300040405     c                   z-add     MSAdtl        n8
247400040405     c                   movel     MSAorl        n6
247500040405     c     *ISO          move      n8            Wdata
247600040405     c     *ISO          move      n6            Wora
247700040405if  2c                   if            (Wtempo <  W00tli
247800040405     c                              or  Wtempo >  W00tlf)
247900060831     c                   leavesr
248000040405e   2c                   endif
248100060831      *
248200060831e   1c                   ENDIF
248300040316      *
248400060831     c                   eval      $RecOK  = *on
248500060831      *
248600060831     c                   ENDSR
248700060831
248800060831      *---------------------------------------------------------------*
248900060831      *?Creazione griglia di rappresentazione                        ?*
249000060831      *---------------------------------------------------------------*
249100060831     c     sr_CrtGriglia BEGSR
249200060831      *
249300060831      * Pulizia delle variabili di lavoro
249400060831     c                   clear                   GDper                          *dettaglio
249500060831     c                   clear                   GRper                          *riepilogo
249600080116     c                   clear                   $Gtip                          *unione
249700080116     c                   clear                   $Gdtl
249800080116     c                   clear                   $Gorl
249900060831     c                   clear                   xi
250000060831     c                   clear                   MEMxi
250100060831     c                   clear                   MEMgd
250200060831     c                   clear                   xj
250300060831     c                   clear                   xg
250400060831     c                   clear                   MEMxg
250500060831      *
250600060831      * Crea la griglia di dettaglio - - - - - - - - - - - - - - - - -*
250700060831      *
250800060831      * imposta la PRIMA data + ora
250900060831     c                   movel     $dtl(1)       PrimaData
251000060831     c                   move      $orl(1)       PrimaData
251100060831      *
251200060831      * imposta l'ULTIMA data + ora
251300060831     c                   movel     $dtl(ULTmsa)  UltimData
251400060831     c                   move      $orl(ULTmsa)  UltimData
251500060831      *
251600060831      * imposta la PRIMA data e la PRIMA ora di LAVORO
251700060831     c                   eval      Ldata   = $dtl(1)
251800060831     c                   eval      Lora    = $orl(1)
251900060831      *
252000060831      * ciclo fino a fine periodo
252100060831     c                   clear                   xi
252200060831do  1c                   DOW       PrimaData <= UltimData
252300080909      /free
252400080909       // - TROPPI dati !!!
252500080909           if  xi >= %elem(GDper);
252600080909             $TooManyRec = *on;
252700080909             $FNMSA      = *off;
252800080909             leavesr;
252900080909           endif;
253000080909      /end-free
253100060831      * - aggiunge elemento in schiera
253200060831     c                   add       1             xi
253300060831     c                   movel     Ldata         GDper(xi)
253400060831     c                   move      Lora          GDper(xi)
253500060831      * - aggiunge i minuti di dettaglio richiesti
253600060831     c                   eval      n8      = Ldata
253700060831     c                   eval      n4      = Lora
253800060831     c                   eval      Lmin    = D00mtd
253900060831     c                   exsr      AddMinDet
254000060831     c                   eval      Ldata   = n8
254100060831     c                   eval      Lora    = n4
254200060831      * - incrementa la PRIMA data + ora
254300060831     c                   movel     Ldata         PrimaData
254400060831     c                   move      Lora          PrimaData
254500000000e   1c                   ENDDO
254600060831     c                   eval      MEMgd   = xi
254700060831      *
254800060831      * Crea la griglia di riepilogo - - - - - - - - - - - - - - - - -*
254900060831      *
255000060831      * aggiunge alla PRIMA data + ora i mm di riepilogo richiesti
255100060831     c                   eval      n8      = $dtl(1)
255200060831     c                   eval      n4      = $orl(1)
255300060831     c                   eval      Lmin    = D00mtr
255400060831     c                   exsr      AddMinDet
255500060831     c                   movel     n8            PrimaData
255600060831     c                   move      n4            PrimaData
255700060831      *
255800060831      * imposta la PRIMA data e la PRIMA ora di LAVORO
255900060831     c                   eval      Ldata   = n8
256000060831     c                   eval      Lora    = n4
256100060831      *
256200060831      * imposta l'ULTIMA data + ora
256300060831     c                   movel     $dtl(ULTmsa)  UltimData
256400060831     c                   move      $orl(ULTmsa)  UltimData
256500060831      *
256600060831      * ciclo fino a fine periodo
256700060831     c                   clear                   xi
256800060831do  1c                   DOW       PrimaData <= UltimData
256900080909      /free
257000080909       // - TROPPI dati !!!
257100080909           if  xi >= %elem(GRper);
257200080909             $TooManyRec = *on;
257300080909             $FNMSA      = *off;
257400080909             leavesr;
257500080909           endif;
257600080909      /end-free
257700060831      * aggiunge elemento in schiera
257800060831     c                   add       1             xi
257900060831     c                   movel     Ldata         GRper(xi)
258000060831     c                   move      Lora          GRper(xi)
258100060831      * aggiunge i minuti di riepilogo richiesti
258200060831     c                   eval      n8      = Ldata
258300060831     c                   eval      n4      = Lora
258400060901     c                   eval      LMin    = D00mtr
258500060831     c                   exsr      AddMinDet
258600060831     c                   eval      Ldata   = n8
258700060831     c                   eval      Lora    = n4
258800060831      * incrementa la PRIMA data + ora
258900060901     c                   movel     Ldata         PrimaData
259000060901     c                   move      Lora          PrimaData
259100000000e   1c                   ENDDO
259200060831     c                   eval      MEMxi   = xi
259300060831      *
259400060831      * Crea la griglia di unione  - - - - - - - - - - - - - - - - - -*
259500060831      *
259600060831     c                   eval      xi      = 1                                  *indice x riepilogo
259700060831     c                   eval      xj      = 1                                  *indice x dettaglio
259800060831     c                   clear                   xg                             *indice x unione
259900060831      *
260000060831      * imposta il tipo griglia per il dettaglio
260100060831if  1c                   if        D00mtd  = 1                                  *dettaglio minimo
260200060831     c                   eval      DEPtip  = 'M'
260300060831x   1c                   else                                                   *dettaglio sintetico
260400060831     c                   eval      DEPtip  = 'D'
260500060831e   1c                   endif
260600060831      *
260700060831      * lettura griglia riepilogo
260800060830do  1c                   DO        MEMxi         xi
260900060831      *
261000060831      * - lettura griglia dettaglio
261100060831do  2c     xj            DO        MEMgd         xx
261200060831      * - - memorizza in GRIGLIA i minuti di dettaglio griglia compresi
261300060831      *     nel mm di riepilogo
261400060831if  3c                   IF        GDper(xx)  > *zeros
261500060831if  4c                   if        GDper(xx) <= GRper(xi)
261600060831     c                   add       1             xg
261700080116     c                   eval      $Gtip(xg)  = DEPtip                          *tipo elemento
261800080116     c                   movel     GDper(xx)     $Gdtl(xg)                      *GRIGLIA data -dett-
261900080116     c                   move      GDper(xx)     $Gorl(xg)                      *GRIGLIA ora  -dett-
262000060831x   4c                   else
262100060831     c                   leave
262200060831e   4c                   endif
262300000000e   3c                   ENDIF
262400060831e   2c                   ENDDO
262500060831     c                   eval      xj         = xx
262600060831      *
262700060831      * memorizza in GRIGLIA il minuto di riepilogo
262800060831     c                   add       1             xg
262900080116     c                   eval      $Gtip(xg)  = 'R'                             *tipo elemento
263000080116     c                   movel     GRper(xi)     $Gdtl(xg)                      *GRIGLIA data -riep-
263100080116     c                   move      GRper(xi)     $Gorl(xg)                      *GRIGLIA ora  -riep-
263200060831      *
263300060831e   1c                   ENDDO
263400060831      *
263500060831      * memorizza in GRIGLIA i minuti di dettaglio non compresi nell'
263600060831      *   ultimo riepilogo
263700060831do  1c     xj            do        MEMgd         xx
263800060831     c                   add       1             xg
263900080116     c                   eval      $Gtip(xg)  = DEPtip                          *tipo elemento
264000080116     c                   movel     GDper(xx)     $Gdtl(xg)                      *GRIGLIA data -dett-
264100080116     c                   move      GDper(xx)     $Gorl(xg)                      *GRIGLIA ora  -dett-
264200060831e   1c                   enddo
264300060831      *
264400060831      * memorizza in GRIGLIA il minuto di totale generale
264500060831     c                   add       1             xg
264600080116     c                   eval      $Gtip(xg)  = 'G'                             *tipo elemento
264700080116     c                   eval      $Gdtl(xg)  = $Gdtl(xg-1)                     *GRIGLIA data -gen-
264800080116     c                   eval      $Gorl(xg)  = $Gorl(xg-1)                     *GRIGLIA ora  -gen-
264900060831     c                   eval      MEMxg      = xg
265000060831      *
265100060831     c                   ENDSR
265200060831
265300060831      *---------------------------------------------------------------*
265400060831      *?Aggiunge i minuti di dettaglio                               ?*
265500060831      *---------------------------------------------------------------*
265600060831     c     AddMinDet     BEGSR
265700060831      *
265800060831     c                   clear                   n6
265900060831     c                   movel     n4            n6
266000060831     c     *ISO          move      n8            Wdata
266100060831     c     *ISO          move      n6            Wora
266200060831     c                   adddur    Lmin:*MN      Wtempo
266300060831     c     *ISO          move      Wdata         n8
266400060831     c     *ISO          move      Wora          n6
266500060831     c                   movel     n6            n4
266600060831      *
266700000000     c                   ENDSR
266800060831
266900060831      *---------------------------------------------------------------*
267000060831      *?Valorizza le schiere video dalla griglia di rappresentazione ?*
267100060831      *---------------------------------------------------------------*
267200060901     c     sr_ValScVid   BEGSR
267300060831      *
267400060831      * Pulizia delle variabili di lavoro
267500060830     c                   clear                   $Vdes
267600060830     c                   clear                   $Vtip
267700060830     c                   clear                   $Vdtl
267800060830     c                   clear                   $Vorl
267900060831     c                   clear                   $Vncl
268000060831     c                   clear                   $VminLav
268100061006     c***                clear                   $VPExx_ds
268200061006     c***                clear                   $VPExx
268300061006     c                   clear                   xw
268400061006do  1c                   DOW       xw       < %elem($VPExx_ds)
268500061006     c                   add       1             xw
268600061006     c     xw            occur     $VPExx_ds
268700061006     c                   clear                   $VPExx
268800061006e   1c                   ENDDO
268900060831     ***c                   CLEAR                   vpe1
269000060831     ***c                   CLEAR                   vpe2
269100060831     ***c                   CLEAR                   vpe3
269200060831     ***c                   CLEAR                   vpe4
269300060831     ***c                   CLEAR                   vpe5
269400060831     ***c                   CLEAR                   vpe6
269500060831     ***c                   CLEAR                   vpe7
269600060831     ***c                   CLEAR                   vpe8
269700060831     ***c                   CLEAR                   vpe9
269800060831     ***c                   CLEAR                   vpe0
269900060831     ***c                   CLEAR                   vpe11
270000060831     ***c                   CLEAR                   vpe12
270100060831     ***c                   CLEAR                   vpe13
270200060831     ***c                   CLEAR                   vpe14
270300060831     ***c                   CLEAR                   vpe15
270400060831     ***c                   CLEAR                   vpe16
270500060831     ***c                   CLEAR                   vpe17
270600060831     ***c                   CLEAR                   vpe18
270700060831     ***c                   CLEAR                   vpe19
270800060831     ***c                   CLEAR                   vpe20
270900061006     c***                clear                   $VMLAVxx_ds
271000061006     c***                clear                   $VMLAVxx
271100061006     c                   clear                   xw
271200061006do  1c                   DOW       xw       < %elem($VMLAVxx_ds)
271300061006     c                   add       1             xw
271400061006     c     xw            occur     $VMLAVxx_ds
271500061006     c                   clear                   $VMLAVxx
271600061006e   1c                   ENDDO
271700060831     ***c                   CLEAR                   vmlav01
271800060831     ***c                   CLEAR                   vmlav02
271900060831     ***c                   CLEAR                   vmlav03
272000060831     ***c                   CLEAR                   vmlav04
272100060831     ***c                   CLEAR                   vmlav05
272200060831     ***c                   CLEAR                   vmlav06
272300060831     ***c                   CLEAR                   vmlav07
272400060831     ***c                   CLEAR                   vmlav08
272500060831     ***c                   CLEAR                   vmlav09
272600060831     ***c                   CLEAR                   vmlav10
272700060831     ***c                   CLEAR                   vmlav11
272800060831     ***c                   CLEAR                   vmlav12
272900060831     ***c                   CLEAR                   vmlav13
273000060831     ***c                   CLEAR                   vmlav14
273100060831     ***c                   CLEAR                   vmlav15
273200060831     ***c                   CLEAR                   vmlav16
273300060831     ***c                   CLEAR                   vmlav17
273400060831     ***c                   CLEAR                   vmlav18
273500060831     ***c                   CLEAR                   vmlav19
273600060831     ***c                   CLEAR                   vmlav20
273700061006     c***                clear                   $VPAxx_ds
273800061006     c***                clear                   $VPAxx
273900061006     c                   clear                   xw
274000061006do  1c                   DOW       xw       < %elem($VPAxx_ds)
274100061006     c                   add       1             xw
274200061006     c     xw            occur     $VPAxx_ds
274300061006     c                   clear                   $VPAxx
274400061006e   1c                   ENDDO
274500060831     ***c                   CLEAR                   vpa1
274600060831     ***c                   CLEAR                   vpa2
274700060831     ***c                   CLEAR                   vpa3
274800060831     ***c                   CLEAR                   vpa4
274900060831     ***c                   CLEAR                   vpa5
275000060831     ***c                   CLEAR                   vpa6
275100060831     ***c                   CLEAR                   vpa7
275200060831     ***c                   CLEAR                   vpa8
275300060831     ***c                   CLEAR                   vpa9
275400060831     ***c                   CLEAR                   vpa10
275500060831     ***c                   CLEAR                   vpa11
275600060831     ***c                   CLEAR                   vpa12
275700060831     ***c                   CLEAR                   vpa13
275800060831     ***c                   CLEAR                   vpa14
275900060831     ***c                   CLEAR                   vpa15
276000060831     ***c                   CLEAR                   vpa16
276100060831     ***c                   CLEAR                   vpa17
276200060831     ***c                   CLEAR                   vpa18
276300060831     ***c                   CLEAR                   vpa19
276400060831     ***c                   CLEAR                   vpa20
276500060831     ***c                   CLEAR                   vpa21
276600060831     ***c                   CLEAR                   vpa22
276700060831     ***c                   CLEAR                   vpa23
276800060831     ***c                   CLEAR                   vpa24
276900060831     ***c                   CLEAR                   vpa25
277000060831     ***c                   CLEAR                   vpa26
277100060831     ***c                   CLEAR                   vpa27
277200060831     ***c                   CLEAR                   vpa28
277300060831     ***c                   CLEAR                   vpa29
277400060831     ***c                   CLEAR                   vpa30
277500060831     ***c                   CLEAR                   vpa31
277600060831     ***c                   CLEAR                   vpa32
277700060831     ***c                   CLEAR                   vpa33
277800060831     ***c                   CLEAR                   vpa34
277900060831     ***c                   CLEAR                   vpa35
278000060831     ***c                   CLEAR                   vpa36
278100060831     ***c                   CLEAR                   vpa37
278200060831     ***c                   CLEAR                   vpa38
278300060831     ***c                   CLEAR                   vpa39
278400060831     ***c                   CLEAR                   vpa40
278500060831     ***c                   CLEAR                   vpa41
278600060831     ***c                   CLEAR                   vpa42
278700060831     ***c                   CLEAR                   vpa43
278800060831     ***c                   CLEAR                   vpa44
278900060831     ***c                   CLEAR                   vpa45
279000060831     ***c                   CLEAR                   vpa46
279100060831     ***c                   CLEAR                   vpa47
279200060831     ***c                   CLEAR                   vpa48
279300060831     ***c                   CLEAR                   vpa49
279400060831     ***c                   CLEAR                   vpa50
279500060831     ***c                   CLEAR                   vpa51
279600060831     ***c                   CLEAR                   vpa52
279700060831     ***c                   CLEAR                   vpa53
279800060831     ***c                   CLEAR                   vpa54
279900060831     ***c                   CLEAR                   vpa55
280000060831     ***c                   CLEAR                   vpa56
280100060831     ***c                   CLEAR                   vpa57
280200060831     ***c                   CLEAR                   vpa58
280300060831     ***c                   CLEAR                   vpa59
280400060831     ***c                   CLEAR                   vpa60
280500060831     c                   clear                   Vpnc                           *di calcolo
280600060831     c                   clear                   Vppnc
280700060831     c                   clear                   Vpsp
280800060831     c                   clear                   Vpesind                        *per statistiche
280900060831     c                   clear                   Vpussor
281000060831     c                   clear                   Vpuslos
281100060831     c                   clear                   Vpusnes
281200060831     c                   clear                   Vldv
281300060831     c                   clear                   Vchi
281400060831     c                   clear                   Vbar
281500060831     c                   clear                   VbarP
281600060831     c                   clear                   Vcsd
281700060831     c                   clear                   Vps2dok
281800060831     c                   clear                   Vccv
281900060831     c                   clear                   Vccp
282000060831     c                   clear                   Vnls
282100060831     c                   clear                   Vnlp
282200060831     c                   clear                   Vxnomis
282300060831     c                   clear                   Vxnopes
282400060831     c                   clear                   Vodsdna
282500060831     c                   clear                   Verrmac
282600060608     c                   clear                   Vpesind2
282700060608     c                   clear                   Vpussor2
282800060608     c                   clear                   Vodsdna2
282900060608     c                   clear                   Verrmac2
283000060831     c                   clear                   xg
283100060831     c                   clear                   xv
283200060831     c                   clear                   MEMxv
283300061006     c*** dopo:          clear                   WnclOb                         *colli obiettivo
283400060904     c                   eval      xi        = 1
283500060904     c                   eval      xj        = 1
283600060904     c                   eval      pj        = 1
283700060904     c                   eval      pk        = 1
283800060904     c                   eval      ppj       = 1
283900060904     c                   eval      ppk       = 1
284000060831      *
284100060831      * Ciclo di lettura GRIGIA  - - - - - - - - - - - - - - - - - - -*
284200060831      *
284300060831do  1c                   DO        MEMxg         xg
284400060830      *
284500080909      /free
284600080909       // - TROPPI dati !!!
284700080909           if  xv >= %elem($Vtip);
284800080909             $TooManyRec = *on;
284900080909             $FNMSA      = *off;
285000080909             leavesr;
285100080909           endif;
285200080909      /end-free
285300060830      * imposta già alcuni dati
285400060831     c                   add       1             xv                             *indice VIDEO
285500060830      * - tipo griglia
285600080116     c                   eval      $Vtip(xv) = $Gtip(xg)
285700060830sel 2c                   select
285800060830      * - - dettaglio
285900080116w   2c                   when           $Gtip(xg) = 'D'
286000080116     c                             or   $Gtip(xg) = 'M'
286100060830     c                   clear                   $Vdes(xv)
286200060830      * - - riepilogo
286300080116if  2c                   when      $Gtip(xg) = 'R'
286400060830     c                   eval      $Vdes(xv) = $Dvid(3)
286500060830      * - - totale generale
286600080116if  2c                   when      $Gtip(xg) = 'G'
286700060830     c                   eval      $Vdes(xv) = $Dvid(4)
286800060830e   2c                   endsl
286900060830      * - data lettura (no in totale generale)
287000080116if  2c                   IF        $Gtip(xg) <>'G'
287100060929     c                   reset                   WLBdat
287200080116     c                   eval      G08inv    = $Gdtl(xg)
287300060830     c                   call      'XSRDA8'
287400060901     c                   parm                    WLBdat
287500060901     c                   eval      $Vdtl(xv) = G08dat                           *data lettura
287600080116     c                   eval      $Vorl(xv) = $Gorl(xg)                        *ora  lettura
287700060830x   2c                   ELSE
287800060830     c                   clear                   $Vdtl(xv)
287900060830     c                   clear                   $Vorl(xv)
288000060830e   2c                   ENDIF
288100060831      *
288200060831      * - imposta data + ora griglia
288300080116     c                   movel     $Gdtl(xg)     LperGri
288400080116     c                   move      $Gorl(xg)     LperGri
288500030328      *
288600060831      * - imposta data + ora griglia
288700030328      *   per i calcoli sul giorno lavorativo precedente
288800030328     c                   exsr      X_lPerGriP
288900060831      *
289000060831sel 2c                   SELECT
289100060831      *
289200060831      * - valorizza riga DETTAGLIO minimo  - - - - - - - - - - - - - -*
289300080116w   2c                   WHEN      $Gtip(xg) = 'M'
289400060831do  3c     xi            DO        ULTmsa        xx
289500060831     c                   movel     $dtl(xx)      LperMem
289600060831     c                   move      $orl(xx)      LperMem
289700080116if  4c                   IF             $Gdtl(xg) = $dtl(xx)
289800080116     c                             and  $Gorl(xg) = $orl(xx)
289900080116     c                   eval      xm = xx
290000060831     c                   exsr      AddScVid
290100060831     c                   leave
290200000000x   4c                   ELSE
290300080116if  5c                   if        LperMem   > LperGri
290400060831     c                   leave
290500060831e   5c                   endif
290600000000e   4c                   ENDIF
290700000000e   3c                   ENDDO
290800080116     c                   eval      xi  = xx
290900060831      *
291000060831      * - valorizza riga DETTAGLIO sintetico - - - - - - - - - - - - -*
291100080116w   2c                   WHEN      $Gtip(xg) = 'D'
291200060831do  3c     xi            DO        ULTmsa        xx
291300060831     c                   movel     $dtl(xx)      LperMem
291400060831     c                   move      $orl(xx)      LperMem
291500080116if  4c                   IF        LperMem   > *zeros
291600080116if  5c                   if        LperMem  <= LperGri
291700080116     c                   eval      xm = xx
291800060831     c                   exsr      AddScVid
291900060831x   5c                   else
292000060831     c                   leave
292100060831e   5c                   endif
292200000000e   4c                   ENDIF
292300000000e   3c                   ENDDO
292400080116     c                   eval      xi  = xx
292500060831      *
292600060831      * - valorizza riga RIEPILOGO - - - - - - - - - - - - - - - - - -*
292700080116w   2c                   WHEN      $Gtip(xg) = 'R'
292800060831do  3c     xj            DO        ULTmsa        xx
292900060831     c                   movel     $dtl(xx)      LperMem
293000060831     c                   move      $orl(xx)      LperMem
293100080116if  4c                   IF        LperMem   > *zeros
293200080116if  5c                   if        LperMem  <= LperGri
293300080116     c                   eval      xm = xx
293400060831     c                   exsr      AddScVid                                     *aggiorna elemento
293500060831x   5c                   else
293600060831     c                   leave
293700060831e   5c                   endif
293800000000e   4c                   ENDIF
293900000000e   3c                   ENDDO
294000080116     c                   eval      xj  = xx
294100060831      *
294200060831      * - valorizza riga GENERALE  - - - - - - - - - - - - - - - - - -*
294300080116w   2c                   WHEN      $Gtip(xg) = 'G'
294400060831do  3c                   DO        ULTmsa        xx
294500080116     c                   eval      xm = xx
294600060831     c                   exsr      AddScVid                                     *aggiorna elemento
294700000000e   3c                   ENDDO
294800061006     c                   clear                   WnclOb                         *colli obiettivo
294900060904     c*** NO banco 20:   do        20            xw
295000061006     c                   do        19            xw
295100060904     c     xw            occur     $VMLAVxx_ds
295200080116     c                   eval      WnclOb    = WnclOb                           *colli obiettivo
295300080116     c                                       + (§KPIpba * $VMLAVxx(xv))
295400061002e   3c                   enddo
295500060831      *
295600060831e   2c                   ENDSL
295700060831      *
295800060831      * calcola il progressivo pacchi DETTAGLIO  - - - - - - - - - - -*
295900060831      *
296000080116if  2c                   IF             $Gtip(xg) = 'M'                         *minimo o sintetico
296100080116     c                             or   $Gtip(xg) = 'D'
296200060831      *
296300060831      * - corrente
296400060831do  3c                   DO        ULTmsa        xx
296500060831     c                   movel     $dtl(xx)      LperMem
296600060831     c                   move      $orl(xx)      LperMem
296700080116if  4c                   if        LperMem   > *zeros
296800080116if  5c                   if        LperMem  <= LperGri
296900060831     ***c                   eval      xm       = xx
297000060831     ***c                   exsr      AddScVid2
297100060831     c                   add       $ncl(xx)      Vpnc(xv)                       *progressivo colli
297200060831x   5c                   else
297300060831     c                   leave
297400060831e   5c                   endif
297500060831e   4c                   endif
297600000000e   3c                   ENDDO
297700060831      * - precedente
297800060831do  3c                   DO        ULTmsaPR      xx
297900060831     c                   movel     $Pdtl(xx)     LperMem
298000060831     c                   move      $Porl(xx)     LperMem
298100080116if  4c                   if        LperMem   > *zeros
298200080116if  5c                   if        LperMem  <= LperGriP
298300060831     ***c                   eval      xm       = xx
298400060831     ***c                   exsr      AddScVid3
298500060831     c                   add       $Pncl(xx)     VPpnc(xv)                     *progr.colli PREC.
298600060831x   5c                   else
298700060831     c                   leave
298800060831e   5c                   endif
298900060831e   4c                   endif
299000000000e   3c                   ENDDO
299100060831      *
299200000000e   2c                   ENDIF
299300060831      *
299400060831      * calcola il progressivo pacchi RIEPILOGO  - - - - - - - - - - -*
299500060831      *
299600080116if  2c                   IF        $Gtip(xg) = 'R'
299700060831      * - corrente
299800060831do  3c     Pj            DO        ULTmsa        xx
299900060831     c                   movel     $dtl(xx)      LperMem
300000060831     c                   move      $orl(xx)      LperMem
300100080116if  4c                   if        LperMem   > *zeros
300200080116if  5c                   if        LperMem  <= LperGri
300300060831     ***c                   eval      xm       = xx
300400060831     ***c                   exsr      AddScVid2
300500060831     c                   add       $ncl(xx)      Vpnc(xv)                       *progressivo colli
300600060831x   5c                   else
300700060831     c                   leave
300800060831e   5c                   endif
300900060831e   4c                   endif
301000000000e   3c                   ENDDO
301100080116     c                   eval      Pj = xx
301200060831      * - precedente
301300060831do  3c     PPj           DO        ULTmsaPR      xx
301400060831     c                   movel     $Pdtl(xx)     LperMem
301500060831     c                   move      $Porl(xx)     LperMem
301600080116if  4c                   if        LperMem   > *zeros
301700080116if  5c                   if        LperMem  <= LperGriP
301800060831     ***c                   eval      xm       = xx
301900060831     ***c                   exsr      AddScVid3
302000060831     c                   add       $Pncl(xx)     VPpnc(xv)                     *progr.colli PREC.
302100060831x   5c                   else
302200060831     c                   leave
302300060831e   5c                   endif
302400060831e   4c                   endif
302500000000e   3c                   ENDDO
302600080116     c                   eval      PPj = xx
302700000000e   2c                   ENDIF
302800060831      *
302900060831      * calcola il progressivo pacchi GENERALE - - - - - - - - - - - -*
303000060831      *
303100080116if  2c                   IF        $Gtip(xg) = 'G'
303200060831      * - corrente
303300060831do  3c     Pk            DO        ULTmsa        xx
303400060831     c                   movel     $dtl(xx)      LperMem
303500060831     c                   move      $orl(xx)      LperMem
303600080116if  4c                   if        LperMem   > *zeros
303700080116if  5c                   if        LperMem  <= LperGri
303800060831     ***c                   eval      xm       = xx
303900060831     ***c                   exsr      AddScVid2
304000060831     c                   add       $ncl(xx)      Vpnc(xv)                       *progressivo colli
304100060831x   5c                   else
304200060831     c                   leave
304300060831e   5c                   endif
304400060831e   4c                   endif
304500000000e   3c                   ENDDO
304600080116     c                   eval      Pk  = xx
304700060831      * - precedente
304800060831do  3c     PPk           DO        ULTmsaPR      xx
304900060831     c                   movel     $Pdtl(xx)     LperMem
305000060831     c                   move      $Porl(xx)     LperMem
305100080116if  4c                   if        LperMem   > *zeros
305200080116if  5c                   if        LperMem  <= LperGriP
305300060831     ***c                   eval      xm       = xx
305400060831     ***c                   exsr      AddScVid3
305500060831     c                   add       $Pncl(xx)     VPpnc(xv)                     *progr.colli PREC.
305600060831x   5c                   else
305700060831     c                   leave
305800060831e   5c                   endif
305900060831e   4c                   endif
306000000000e   3c                   ENDDO
306100080116     c                   eval      PPk = xx
306200000000e   2c                   ENDIF
306300060831      *
306400060831e   1c                   ENDDO
306500060831      *
306600060831      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
306700060831      *
306800060831     c                   eval      MEMxv    = xv
306900060831      *
307000060831      * calcola la % scostamento DETTAGLIO / RIEPILOGO / GENERALE  - -*
307100060831      *
307200060831do  1c                   DO        MEMxv         xv
307300060831if  2c                   if             Vpnc(xv)  > *zeros
307400060831     c                             or   VPpnc(xv) > *zeros
307500061002     c                   eval      §PNCatt  = Vpnc(xv)                          *prg colli attuali
307600061002     c                   eval      §PNCpre  = VPpnc(xv)                         *prg colli attuali
307700061002     c                   clear                   §PNCpsp                        *% scostamento
307800060831     c                   exsr      Scostam                                      *CALCOLA PERCENTUALE
307900061002     c                   eval      Vpsp(xv) = §PNCpsp                           *% scostamento
308000060831e   2c                   endif
308100060831e   1c                   ENDDO
308200060831      *
308300000000     c                   ENDSR
308400060831
308500060831      *---------------------------------------------------------------*
308600060831      *?Imposta il giorno e l'orario limite per confronto            ?*
308700060831      *?in base a quelli - limite - in esame                         ?*
308800060831      *---------------------------------------------------------------*
308900030328     c     X_lPerGriP    BEGSR
309000030328      *
309100060831     c                   clear                   LperGriP
309200060831     c                   movel     $Pdtl(1)      LperMem
309300060831     c                   move      $Porl(1)      LperMem
309400030328      *
309500080116sel 1c                   SELECT
309600080116      *
309700080116      * se non ho giorni precedenti => ripulisco
309800080116w   1c                   WHEN      $Pdtl(1)   = *zeros
309900080116     c                   clear                   LperGriP
310000080116      *
310100030328      * se sto elaborando la data di "partenza" (prima di mezzanotte)
310200080116w   1c                   WHEN      $Gdtl(xg)  = $Gdtl(1)
310300080116      *  => considero la prima data di confronto disponibile
310400080116      *     (del 1° elemento) SE PRECEDENTE !!!
310500080116if  2c                   if        $Pdtl(1)   < $Gdtl(1)
310600080116     c                   movel     $Pdtl(1)      LperGriP
310700080116x   2c                   else
310800080116      *  => calcolo la data precedente a quella nel 1° elemento
310900080116      *     SE UGUALE !!!
311000080116     c                   move      $Pdtl(1)      WdateISO
311100080116     c                   subdur    1:*d          WdateISO
311200080116     c                   move      WdateISO      n8
311300080116     c                   movel     n8            LperGriP
311400080116e   2c                   endif
311500080116     c                   move      $Gorl(xg)     LperGriP
311600080116      *
311700030328      * se sto elaborando la data successiva    (dopo  la mezzanotte)
311800080116x   1c                   OTHER
311900030328      *  => considero la data successiva alla prima data di confronto
312000080116      *     (del 1° elemento) SE PRECEDENTE !!!
312100080116if  2c                   if        $Pdtl(1)   < $Gdtl(1)
312200060831     c                   move      $Pdtl(1)      WdateISO
312300030328     c                   adddur    1:*d          WdateISO
312400030328     c                   move      WdateISO      n8
312500060831     c                   movel     n8            LperGriP
312600080116x   2c                   else
312700080116      *  => mantengo la prima data di confronto
312800080116     c                   move      $Pdtl(1)      WdateISO
312900080116     c                   move      WdateISO      n8
313000080116     c                   movel     n8            LperGriP
313100080116e   2c                   endif
313200080116     c                   move      $Gorl(xg)     LperGriP
313300080116      *
313400080116e   1c                   ENDSL
313500030328      *
313600030328     c                   ENDSR
313700060830
313800060830      *---------------------------------------------------------------*
313900060831      *?Aggiornamento schiere video (1^ parte)                       ?*
314000060830      *---------------------------------------------------------------*
314100060830     c     AddScVid      BEGSR
314200060830      *
314300061002     c                   add       $NCL(xm)      $Vncl(xv)                      *colli
314400060830     c                   add       1             $VminLav(xv)                   *mm lavorati totali
314500060831      * Entrate
314600060831     c     xm            occur     $PEPxx_ds                                    *entrate
314700060904     c                   clear                   xw
314800080116do  1c                   DOW       xw         < %elem($VPExx_ds)
314900060904     c                   add       1             xw
315000060904     c     xw            occur     $VPExx_ds
315100060904     c                   add       $PEPxx(xw)    $VPExx(xv)                     *colli x entrata/min
315200060904if  2c                   if        $PEPxx(xw) > *zeros
315300060904     c     xw            occur     $VMLAVxx_ds
315400060831     c                   add       1             $VMLAVxx(xv)                   *mm lavorati x entr.
315500060831e   2c                   endif
315600060831e   1c                   ENDDO
315700060831     ***c                   ADD       mpe1(xm)      vpe1(xv)
315800060831     ***c                   IF        mpe1(xm) > *zeros
315900060831     ***c                   ADD       1             vmlav01(xv)
316000060831     ***c                   ENDIF
316100060831     ***c                   ADD       mpe2(xm)      vpe2(xv)
316200060831     ***c                   IF        mpe2(xm) > *zeros
316300060831     ***c                   ADD       1             vmlav02(xv)
316400060831     ***c                   ENDIF
316500060831     ***c                   ADD       mpe3(xm)      vpe3(xv)
316600060831     ***c                   IF        mpe3(xm) > *zeros
316700060831     ***c                   ADD       1             vmlav03(xv)
316800060831     ***c                   ENDIF
316900060831     ***c                   ADD       mpe4(xm)      vpe4(xv)
317000060831     ***c                   IF        mpe4(xm) > *zeros
317100060831     ***c                   ADD       1             vmlav04(xv)
317200060831     ***c                   ENDIF
317300060831     ***c                   ADD       mpe5(xm)      vpe5(xv)
317400060831     ***c                   IF        mpe5(xm) > *zeros
317500060831     ***c                   ADD       1             vmlav05(xv)
317600060831     ***c                   ENDIF
317700060831     ***c                   ADD       mpe6(xm)      vpe6(xv)
317800060831     ***c                   IF        mpe6(xm) > *zeros
317900060831     ***c                   ADD       1             vmlav06(xv)
318000060831     ***c                   ENDIF
318100060831     ***c                   ADD       mpe7(xm)      vpe7(xv)
318200060831     ***c                   IF        mpe7(xm) > *zeros
318300060831     ***c                   ADD       1             vmlav07(xv)
318400060831     ***c                   ENDIF
318500060831     ***c                   ADD       mpe8(xm)      vpe8(xv)
318600060831     ***c                   IF        mpe8(xm) > *zeros
318700060831     ***c                   ADD       1             vmlav08(xv)
318800060831     ***c                   ENDIF
318900060831     ***c                   ADD       mpe9(xm)      vpe9(xv)
319000060831     ***c                   IF        mpe9(xm) > *zeros
319100060831     ***c                   ADD       1             vmlav09(xv)
319200060831     ***c                   ENDIF
319300060831     ***c                   ADD       mpe0(xm)      vpe0(xv)
319400060831     ***c                   IF        mpe0(xm) > *zeros
319500060831     ***c                   ADD       1             vmlav10(xv)
319600060831     ***c                   ENDIF
319700060831     ***c                   ADD       mpe11(xm)     vpe11(xv)
319800060831     ***c                   IF        mpe11(xm) > *zeros
319900060831     ***c                   ADD       1             vmlav11(xv)
320000060831     ***c                   ENDIF
320100060831     ***c                   ADD       mpe12(xm)     vpe12(xv)
320200060831     ***c                   IF        mpe12(xm) > *zeros
320300060831     ***c                   ADD       1             vmlav12(xv)
320400060831     ***c                   ENDIF
320500060831     ***c                   ADD       mpe13(xm)     vpe13(xv)
320600060831     ***c                   IF        mpe13(xm) > *zeros
320700060831     ***c                   ADD       1             vmlav13(xv)
320800060831     ***c                   ENDIF
320900060831     ***c                   ADD       mpe14(xm)     vpe14(xv)
321000060831     ***c                   IF        mpe14(xm) > *zeros
321100060831     ***c                   ADD       1             vmlav14(xv)
321200060831     ***c                   ENDIF
321300060831     ***c                   ADD       mpe15(xm)     vpe15(xv)
321400060831     ***c                   IF        mpe15(xm) > *zeros
321500060831     ***c                   ADD       1             vmlav15(xv)
321600060831     ***c                   ENDIF
321700060831     ***c                   ADD       mpe16(xm)     vpe16(xv)
321800060831     ***c                   IF        mpe16(xm) > *zeros
321900060831     ***c                   ADD       1             vmlav16(xv)
322000060831     ***c                   ENDIF
322100060831     ***c                   ADD       mpe17(xm)     vpe17(xv)
322200060831     ***c                   IF        mpe17(xm) > *zeros
322300060831     ***c                   ADD       1             vmlav17(xv)
322400060831     ***c                   ENDIF
322500060831     ***c                   ADD       mpe18(xm)     vpe18(xv)
322600060831     ***c                   IF        mpe18(xm) > *zeros
322700060831     ***c                   ADD       1             vmlav18(xv)
322800060831     ***c                   ENDIF
322900060831     ***c                   ADD       mpe19(xm)     vpe19(xv)
323000060831     ***c                   IF        mpe19(xm) > *zeros
323100060831     ***c                   ADD       1             vmlav19(xv)
323200060831     ***c                   ENDIF
323300060831     ***c                   ADD       mpe20(xm)     vpe20(xv)
323400060831     ***c                   IF        mpe20(xm) > *zeros
323500060831     ***c                   ADD       1             vmlav20(xv)
323600060831     ***c                   ENDIF
323700060831      * Uscite
323800060904     c     xm            occur     $PADxx_ds
323900060904     c     xv            occur     $VPAxx_ds
324000060904     c                   clear                   xw
324100080116do  1c                   dow       xw         < %elem($PADxx)
324200060904     c                   add       1             xw
324300060904     c                   add       $PADxx(xw)    $VPAxx(xw)
324400060831e   1c                   enddo
324500060831     ***c                   ADD       mpa1(xm)      vpa1(xv)                       *uscite
324600060831     ***c                   ADD       mpa2(xm)      vpa2(xv)
324700060831     ***c                   ADD       mpa3(xm)      vpa3(xv)
324800060831     ***c                   ADD       mpa4(xm)      vpa4(xv)
324900060831     ***c                   ADD       mpa5(xm)      vpa5(xv)
325000060831     ***c                   ADD       mpa6(xm)      vpa6(xv)
325100060831     ***c                   ADD       mpa7(xm)      vpa7(xv)
325200060831     ***c                   ADD       mpa8(xm)      vpa8(xv)
325300060831     ***c                   ADD       mpa9(xm)      vpa9(xv)
325400060831     ***c                   ADD       mpa10(xm)     vpa10(xv)
325500060831     ***c                   ADD       mpa11(xm)     vpa11(xv)
325600060831     ***c                   ADD       mpa12(xm)     vpa12(xv)
325700060831     ***c                   ADD       mpa13(xm)     vpa13(xv)
325800060831     ***c                   ADD       mpa14(xm)     vpa14(xv)
325900060831     ***c                   ADD       mpa15(xm)     vpa15(xv)
326000060831     ***c                   ADD       mpa16(xm)     vpa16(xv)
326100060831     ***c                   ADD       mpa17(xm)     vpa17(xv)
326200060831     ***c                   ADD       mpa18(xm)     vpa18(xv)
326300060831     ***c                   ADD       mpa19(xm)     vpa19(xv)
326400060831     ***c                   ADD       mpa20(xm)     vpa20(xv)
326500060831     ***c                   ADD       mpa21(xm)     vpa21(xv)
326600060831     ***c                   ADD       mpa22(xm)     vpa22(xv)
326700060831     ***c                   ADD       mpa23(xm)     vpa23(xv)
326800060831     ***c                   ADD       mpa24(xm)     vpa24(xv)
326900060831     ***c                   ADD       mpa25(xm)     vpa25(xv)
327000060831     ***c                   ADD       mpa26(xm)     vpa26(xv)
327100060831     ***c                   ADD       mpa27(xm)     vpa27(xv)
327200060831     ***c                   ADD       mpa28(xm)     vpa28(xv)
327300060831     ***c                   ADD       mpa29(xm)     vpa29(xv)
327400060831     ***c                   ADD       mpa30(xm)     vpa30(xv)
327500060831     ***c                   ADD       mpa31(xm)     vpa31(xv)
327600060831     ***c                   ADD       mpa32(xm)     vpa32(xv)
327700060831     ***c                   ADD       mpa33(xm)     vpa33(xv)
327800060831     ***c                   ADD       mpa34(xm)     vpa34(xv)
327900060831     ***c                   ADD       mpa35(xm)     vpa35(xv)
328000060831     ***c                   ADD       mpa36(xm)     vpa36(xv)
328100060831     ***c                   ADD       mpa37(xm)     vpa37(xv)
328200060831     ***c                   ADD       mpa38(xm)     vpa38(xv)
328300060831     ***c                   ADD       mpa39(xm)     vpa39(xv)
328400060831     ***c                   ADD       mpa40(xm)     vpa40(xv)
328500060831     ***c                   ADD       mpa41(xm)     vpa41(xv)
328600060831     ***c                   ADD       mpa42(xm)     vpa42(xv)
328700060831     ***c                   ADD       mpa43(xm)     vpa43(xv)
328800060831     ***c                   ADD       mpa44(xm)     vpa44(xv)
328900060831     ***c                   ADD       mpa45(xm)     vpa45(xv)
329000060831     ***c                   ADD       mpa46(xm)     vpa46(xv)
329100060831     ***c                   ADD       mpa47(xm)     vpa47(xv)
329200060831     ***c                   ADD       mpa48(xm)     vpa48(xv)
329300060831     ***c                   ADD       mpa49(xm)     vpa49(xv)
329400060831     ***c                   ADD       mpa50(xm)     vpa50(xv)
329500060831     ***c                   ADD       mpa51(xm)     vpa51(xv)
329600060831     ***c                   ADD       mpa52(xm)     vpa52(xv)
329700060831     ***c                   ADD       mpa53(xm)     vpa53(xv)
329800060831     ***c                   ADD       mpa54(xm)     vpa54(xv)
329900060831     ***c                   ADD       mpa55(xm)     vpa55(xv)
330000060831     ***c                   ADD       mpa56(xm)     vpa56(xv)
330100060831     ***c                   ADD       mpa57(xm)     vpa57(xv)
330200060831     ***c                   ADD       mpa58(xm)     vpa58(xv)
330300060831     ***c                   ADD       mpa59(xm)     vpa59(xv)
330400060831     ***c                   ADD       mpa60(xm)     vpa60(xv)
330500060831      * per statistiche
330600060831     c                   add       $pesind(xm)   Vpesind(xv)
330700060831     c                   add       $pussor(xm)   Vpussor(xv)
330800060831     c                   add       $puslos(xm)   Vpuslos(xv)
330900060831     c                   add       $pusnes(xm)   Vpusnes(xv)
331000060831     c                   add       $csd(xm)      Vcsd(xv)
331100060831     c                   add       $ps2dok(xm)   Vps2dok(xv)
331200060831     c                   add       $ccv(xm)      Vccv(xv)
331300060831     c                   add       $ccp(xm)      Vccp(xv)
331400060831     c                   add       $nls(xm)      Vnls(xv)
331500060831     c                   add       $nlp(xm)      Vnlp(xv)
331600031224      * Barcode Chi sono
331700060831     c                   eval      Vchi(xv)   = Vchi(xv)
331800060831     c                                        + $nlp(xm)
331900060831     c                                        + $pusnes(xm)
332000060831     c                                        + $puslos(xm)
332100031224      * Barcode LdV x picking
332200060831     c                   eval      Vldv(xv)   = Vldv(xv)
332300060831     c                                        + $nlp(xm)
332400020521      * Barcode Bartolini
332500060831     c                   eval      Vbar(xv)   = Vbar(xv)
332600060831     c                                        + $pesind(xm)
332700060831     c                                        - $nlp(xm)
332800060831     c                                        - $pusnes(xm)
332900060831     c                                        - $puslos(xm)
333000031224      * Barcode Bartolini x picking
333100060831     c                   eval      VbarP(xv)  = VbarP(xv)
333200060831     c                                        + $pesind(xm)
333300060831     c                                        - $nlp(xm)
333400060831     c                                        - $puslos(xm)
333500020521      *
333600060831     c                   add       $xnomis(xm)   Vxnomis(xv)
333700060831     c                   add       $xnopes(xm)   Vxnopes(xv)
333800060831     c                   add       $odsdna(xm)   Vodsdna(xv)
333900060831     c                   add       $errmac(xm)   Verrmac(xv)
334000060608      *?*NEW?
334100060831     c                   add       $pesind2(xm)  Vpesind2(xv)
334200060831     c                   add       $pussor2(xm)  Vpussor2(xv)
334300060831     c                   add       $odsdna2(xm)  Vodsdna2(xv)
334400060831     c                   add       $errmac2(xm)  Verrmac2(xv)
334500060831      *
334600000000     c                   ENDSR
334700060831     ***
334800060831     *** *---------------------------------------------------------------*
334900060831     *** *?Aggiornamento schiere video (2^ parte)                       ?*
335000060831     *** *---------------------------------------------------------------*
335100060831     ***c     AddScVid2     BEGSR
335200060831     *** *
335300060831     ***c                   add       $ncl(xm)      Vpnc(xv)                      *progr.colli
335400060831     *** *
335500060831     ***c                   ENDSR
335600060831     ***
335700060831     *** *---------------------------------------------------------------*
335800060831     *** *?Aggiornamento schiere video (3^ parte)                       ?*
335900060831     *** *---------------------------------------------------------------*
336000060831     ***c     AddScVid3     BEGSR
336100060831     *** *
336200060831     ***c                   add       $pncl(xm)     Vppnc(xv)                     *progr.colli PREC.
336300060831     *** *
336400060831     ***c                   ENDSR
336500060831
336600060831      *---------------------------------------------------------------*
336700060831      *?Calcolo dei valori statistici                                ?*
336800060831      *---------------------------------------------------------------*
336900060901     c     sr_CalStVid   BEGSR
337000060831      *
337100060831      * Pulizia delle variabili di lavoro
337200060831     c                   clear                   MEMtco                         *ultimo turno
337300060831     c                   clear                   MEMtlm
337400060831     c                   clear                   MEMtcl
337500060831     c                   clear                   MEMtkp
337600060831     c                   clear                   MEMttm
337700060831     c                   clear                   MinLav
337800060831     c                   clear                   wMinLav
337900060831     c                   clear                   MinTra
338000060831     c                   clear                   MEMrco                         *ultimo riepilogo
338100060831     c                   clear                   MEMrlm
338200060831     c                   clear                   MEMrcl
338300060831     c                   clear                   MEMrkp
338400060831     c                   clear                   MEMrtm
338500060831     c                   clear                   MirLav
338600060831     c                   clear                   wMirLav
338700060901     c                   clear                   MirTra
338800060831      *
338900060831      * Progressivi ULTIMO TURNO
339000060831      *
339100060831      * Ciclo di elaborazione di tutti i dati di dettaglio memorizzati
339200060831do  1c                   do        ULTmsa        xi
339300060831      * - Pacchi
339400060831     c                   add       $ncl(xi)      MEMtco
339500060831      * - Minuti lavorati
339600060831     c                   add       1             MinLav
339700060831e   1c                   enddo
339800060831      *
339900060831      * Tempo lavorato (HH:MM)
340000060831     c                   eval      §min    = MinLav
340100060831     c                   clear                   §hh
340200060831     c                   clear                   §mm
340300060831     c                   exsr      CalHml
340400060831     c                   movel     §hh           MEMtlm
340500060831     c                   move      §mm           MEMtlm
340600060831      *
340700060831      * Pacchi / Tempo lavorato
340800060831if   c                   if        MinLav  = *zeros
340900060831     c                   eval      wMinLav = 1
341000060831x   1c                   else
341100060831     c                   eval      wMinLav = MinLav
341200060831e   1c                   endif
341300060831     c                   eval(h)   MEMtcl  = MEMtco / wMinLav
341400060831     c                   eval(h)   MEMtkp  = WnclOb / wMinLav
341500060831      *
341600060831      * Tempo trascorso
341700060901     c                   eval      n8      = $dtl(1)                            *data INIZIALE
341800060904     c                   eval      n6      = $orl(1) * 100                      *ora  INIZIALE
341900060831     c     *ISO          move      n8            Wdata
342000060831     c     *ISO          move      n6            Wora
342100060831     c                   movel     Wtempo        Wtempo1                        *periodo INIZIALE
342200060901     c                   eval      n8      = $dtl(ULTmsa)                       *data FINALE
342300060904     c                   eval      n6      = $orl(ULTmsa) * 100                 *ora  FINALE
342400060831     c     *ISO          move      n8            Wdata
342500060831     c     *ISO          move      n6            Wora
342600060831     c                   movel     Wtempo        Wtempo2                        *periodo FINALE
342700060831     c     Wtempo1       subdur    Wtempo2       MinTra:*mn                     *mm trascorsi totali
342800060831if  1c                   if        MinTra  < *zeros
342900060831     c                   eval      MinTra  = MinTra * (-1)
343000060831e   1c                   endif
343100060831      * Tempo trascorso (HH:MM)
343200060831     c                   eval      §min    = MinTra
343300060831     c                   clear                   §hh
343400060831     c                   clear                   §mm
343500060831     c                   exsr      CalHml
343600060831     c                   movel     §hh           MEMttm
343700060831     c                   move      §mm           MEMttm
343800060831      *
343900060831      * Progressivi ULTIMI MM RICHIESTI
344000060831      *
344100060831      * reperisce la data + ora corrente
344200060831     c                   time                    n14                            *Ora (6)+ Data (8)
344300060831     c                   movel     n14           n4                             *Ora (4)
344400060831     c                   move      n14           n8                             *Data (8) in g/m/a
344500060831     c                   clear                   WLBdat
344600060831     c                   eval      G08dat  = n8
344700060831     c                   call      'XSRDA8'
344800060831     c                   parm                    WLBdat
344900060831     c                   eval      n8      = G08inv                             *Data (8) in a/m/g
345000060831     c                   movel     n8            LperGri2
345100060831     c                   move      n4            LperGri2                       *periodo AL
345200060831      *
345300060831      * sottrae alla data + ora corrente i mm di riepilogo richiesti
345400060831     c                   eval      Lmin    = D00mtr
345500060831     c                   exsr      SubMinRie
345600060831     c                   movel     n8            LperGri
345700060831     c                   move      n4            LperGri                        *periodo DAL
345800060831      *
345900060831      * legge i dati memorizzati del periodo calcolato (data+ora corrente - 30 mm)
346000060831do  1c                   DO        ULTmsa        xi
346100060831     c                   movel     $dtl(xi)      LperMem                        *periodo memorizzato
346200060831     c                   move      $orl(xi)      LperMem
346300060831if  2c                   IF             LperMem >= LperGri
346400060831     c                             and  LperMem <= LperGri2
346500060831      * - Pacchi
346600060831     c                   add       $ncl(xi)      MEMrco
346700060831      * - Minuti lavorati
346800060831     c                   add       1             MirLav
346900060831e   2c                   endif
347000060831e   1c                   enddo
347100060831      *
347200060831      * Tempo lavorato (HH:MM)
347300060831     c                   eval      §min    = MirLav
347400060831     c                   clear                   §hh
347500060831     c                   clear                   §mm
347600060831     c                   exsr      CalHMl
347700060831     c                   movel     §hh           MEMrlm
347800060831     c                   move      §mm           MEMrlm
347900060831      *
348000060831      * Pacchi / Tempo lavorato
348100060831if   c                   if        MirLav  = *zeros
348200060831     c                   eval      wMirLav = 1
348300060831x   1c                   else
348400060831     c                   eval      wMirLav = MirLav
348500060831e   1c                   endif
348600061006     c                   eval(h)   MEMrcl  = MEMrco / wMirLav
348700061006     c*** come MEMTKP:   eval(h)   MEMrkp  = WnclOb / wMirLav
348800060831      *
348900060831      * Tempo trascorso
349000060831     c                   move      D00mtr        MEMrtm
349100060901     c                   eval      MirTra  = D00mtr
349200060901      * Tempo trascorso (HH:MM)
349300060901     c                   eval      §min    = MirTra
349400060901     c                   clear                   §hh
349500060901     c                   clear                   §mm
349600060901     c                   exsr      CalHml
349700060901     c                   movel     §hh           MEMrtm
349800060901     c                   move      §mm           MEMrtm
349900060831      *
350000060831     c                   ENDSR
350100060831
350200060831      *---------------------------------------------------------------*
350300060831      *?Calcolo della percentuale di scostamento                     ?*
350400060831      *?INPUT  --> §pncatt - > "X" attuali                           ?*
350500060831      *?           §pncpre - > "X" precedenti                        ?*
350600060831      *?OUTPUT --> §pncpsp - > % di scostamento                      ?*
350700060831      *---------------------------------------------------------------*
350800060831     c     Scostam       BEGSR
350900060831      *
351000060831      *---
351100060831      * (attuali - precedenti) = gap
351200060831      * (gap x 100) : attuali  = % scostamento
351300060831      *---
351400060831      *
351500060831      * NB: se l'attuale è uguale a zero, lo imposta a uno per non dare errore
351600060929if  1c                   if        §PNCatt  = *zeros
351700060929     c                   eval      §PNCatt  = 1
351800060831e   1c                   endif
351900060901      *
352000060901      * Differenza in termini percentuali
352100060929     c                   eval      §PNCgap  = ((§PNCatt - §PNCpre)
352200060901     c                                        * 100)
352300060929     c                                        / §PNCatt
352400060929     c                   z-add     §PNCgap       §PNCpsp
352500060831      *
352600060901      * Sistemazione della % scostamento se più di 3 cifre
352700060831sel 1c                   select
352800060929w   1c                   when      §PNCgap >= +1000                              *$ positiva
352900060929     c                   eval      §PNCpsp  = *hival
353000060929w   2c                   when      §PNCgap <= -1000                              *% negativa
353100060929     c                   eval      §PNCpsp  = *loval
353200060831e   1c                   endsl
353300060831      *
353400060831     c                   ENDSR
353500060831
353600060831      *---------------------------------------------------------------*
353700060831      *?Calcola le ore e i minuti da minuti totali                   ?*
353800060831      *?INPUT  --> §min: minuti totali trascorsi                     ?*
353900060831      *?OUTPUT --> §gg:  giorni trascorsi                            ?*
354000060831      *?           §hh:  ore    trascorse                            ?*
354100060831      *?           §mm:  minuti trascorsi                            ?*
354200060831      *---------------------------------------------------------------*
354300060831     c     CalHml        BEGSR
354400060831      *
354500060831     c                   clear                   Wresto            5 0
354600060831if  1c                   if        §min  >= 1440                                *i mm formano gg
354700060831     c     §min          div       1440          §gg                            *GIORNI
354800060831     c                   mvr                     Wresto                         *ore/minuti di resto
354900060831x   1c                   else                                                   *i mm non formano gg
355000060831     c                   eval      Wresto = §min                                *ore/minuti di resto
355100060831e   1c                   endif
355200060831     c     Wresto        div       60            §hh                            *ORE
355300060831     c                   mvr                     §mm                            *MINUTI
355400060831      *
355500000000     c                   ENDSR
355600060831
355700060831      *---------------------------------------------------------------*
355800060831      *?Sottrae alla data + ora corrente i mm di riepilogo richiesti ?*
355900060831      *---------------------------------------------------------------*
356000060831     c     SubMinRie     BEGSR
356100060831      *
356200060831     c                   clear                   n6
356300060831     c                   movel     n4            n6
356400060831     c     *ISO          move      n8            Wdata
356500060831     c     *ISO          move      n6            Wora
356600060831     c                   subdur    Lmin:*MN      Wtempo
356700060831     c     *ISO          move      Wdata         n8
356800060831     c     *ISO          move      Wora          n6
356900060831     c                   movel     n6            n4
357000060831      *
357100060831     c                   ENDSR
357200060830
357300060830      *---------------------------------------------------------------*
357400060830
357500060830** - $DVID
357600000000Arrivi         1
357700000000Entrata        2
357800000000RIEP           3
357900000000GEN            4
358000060830               5
