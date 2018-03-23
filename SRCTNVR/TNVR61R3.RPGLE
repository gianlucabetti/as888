000100990812      *--------------------------------------------------------------------------------------------*
000200990812      * Trascodifica tntasv in TITASV
000300990812      *--------------------------------------------------------------------------------------------*
000400990407     h DECEDIT('0,') DATEDIT(*DMY.)
000500990812      *--------------------------------------------------------------------------------------------*
000600990812      * Data base
000700990812      *--------------------------------------------------------------------------------------------*
000800990812     ftitasv0f  IF A E           K DISK
000900990812     ftita7v0f  o    E             DISK
001000990812      *
001100990812     ftntasv0f  IF   E             DISK    prefix(o)
001200990812     ftnta7v1l  IF   E           K DISK    prefix(o)
001300990609     fprtf198   O    F  198        PRINTER  oflind(*INOF)
001400990812      *--------------------------------------------------------------------------------------------*
001500990812      * Data structure
001600990812      *--------------------------------------------------------------------------------------------*
001700990812     d wnrr            s              4b 0                                      *lettura tntasv0f
001800990812     d comopor         S                   LIKE(otaspor)                        *lettura tntas00f
001900990609     d comoinl         S                   LIKE(otasPOR)
002000990609     d comodif         S                   LIKE(otasPOR)
002100990609     d comores         S                   LIKE(otasPOR)
002200990812      *
002300990728     d TIBS69DS      E DS
002400990728     d DS_cnaco      E DS                  extname(CNACO00F)
002500990728     d DS_cnind      E DS                  extname(CNIND00F)
002600990728     d DS_cnclp      E DS                  extname(CNCLP00F)
002700990728     d DS_fncls      E DS                  extname(FNCLS00F)
002800990812      *--------------------------------------------------------------------------------------------*
002900990812      * Main lines
003000990812      *--------------------------------------------------------------------------------------------*
003100990722     c     *entry        plist
003200990728     c                   parm                    numero            8
003300990728     c
003400990812     c                   clear                   titasV00
003500990722     c
003600990812     c                   if        numero > *zeros
003700990726     c                   movel     numero        w0080             8 0
003800990726     c                   z-add     w0080         wnrr
003900990812     c     wnrr          chain     tntasV0f                           99
004000990722     c                   endif
004100990722     c
004200990812      * se non passato numero o numero non trovato, leggo dall'inizio
004300990812     c                   if        numero <= *zeros  OR *in99
004400990812     c                   READ      tntasV0f
004500990722     c                   endif
004600990812
004700990407do  1c                   DOW       NOT *in99
004800990812
004900990812     c                   clear                   titasv00
005000990812
005100990812      * Imposto i campi delle varie, perche' quelle che rimangono vuote
005200990812      *  devono essere riempite con gli altri valori
005300990812     c                   eval      tasva1 = otasva1
005400990812     c                   eval      tasva2 = otasva2
005500990812     c                   eval      tasva3 = otasva3
005600990812     c                   eval      tassv1 = otassv1
005700990812     c                   eval      tassv2 = otassv2
005800990812     c                   eval      tassv3 = otassv3
005900990812      * imposto i campi eliminati
006000990812      * anticipata
006100990812    2c                   if        otasant > *zeros
006200990812     c                   eval      ta7svn = 'T'
006300990812     c                   z-add     otasant       ta7van
006400990608     c                   EXSR      write7
006500990609    2c                   endif
006600990708     c
006700990812      * bollo
006800990812    2c                   if        otasbol > *zeros
006900990812     c                   eval      ta7svn = '6'
007000990812     c                   z-add     otasbol       ta7van
007100990608     c                   EXSR      write7
007200990609    2c                   endif
007300990812     c
007400990812      * iva
007500990812    2c                   if        otasiva > *zeros
007600990812     c                   eval      ta7svn = '8'
007700990812     c                   z-add     otasiva       ta7van
007800990608     c                   EXSR      write7
007900990609    2c                   endif
008000990812      * porto
008100990812    2c                   IF        otaspor > 99999999
008200990812      * stampo intanto
008300990609     c                   except    bolla
008400990812     c                   eval      taspor = 99999000
008500990812     c                   eval      comopor = otaspor - 99999000
008600990812     c                   eval      comoinl = otasinl + comopor
008700990609
008800990812    3c                   IF        comoinl > 99999999
008900990812     c                   eval      ta7svn = '2'
009000990812     c                   z-add     99999000      ta7van
009100990609     c                   EXSR      write7
009200990812     c                   eval      comores = comoinl - 99999000
009300990812     c                   eval      comodif = otasdif + comores
009400990812      *
009500990812    4c                   IF        comodif > 99999999
009600990812      * aggiorno dir.fisso com'e'
009700990812    5c                   If        otasdif > *zeros
009800990609     c                   eval      ta7svn='H'
009900990812     c                   z-add     otasdif       ta7van
010000990609     c                   EXSR      write7
010100990812    5c                   Endif
010200990812     c                   except    bollanoag                                    * stampo
010300990812   x4c                   ELSE
010400990812     c                   eval      ta7svn='H'
010500990812     c                   z-add     comodif       ta7van
010600990812     c                   EXSR      write7
010700990812    4c                   ENDIF
010800990812      *
010900990812   x3c                   ELSE
011000990812     c                   eval      ta7svn = '2'
011100990812     c                   z-add     comoinl       ta7van
011200990609     c                   EXSR      write7
011300990812      * dif.fisso
011400990812    4c                   if        otasdif > *zeros
011500990812     c                   eval      ta7svn = 'H'
011600990812     c                   z-add     otasdif       ta7van
011700990609     c                   EXSR      write7
011800990609    4c                   endif
011900990812    3c                   ENDIF
012000990812      *
012100990812   x2c                   ELSE
012200990812     c                   z-add     otaspor       taspor
012300990812      * inoltro
012400990812    3c                   if        otasinl > *zeros
012500990812     c                   eval      ta7svn = '2'
012600990812     c                   z-add     otasinl       ta7van
012700990609     c                   EXSR      write7
012800990609    3c                   endif
012900990812      * dir.fisso
013000990812    3c                   if        otasdif > *zeros
013100990812     c                   eval      ta7svn = 'H'
013200990812     c                   z-add     otasdif       ta7van
013300990609     c                   EXSR      write7
013400990609    3c                   endif
013500990812    2c                   ENDIF
013600990812      *
013700990812     c                   eval      tasccm = otasccm
013800990812     c                   eval      tasctr = otasctr
013900990812     c                   eval      tasfin = otasfin
014000990812     c                   eval      tastsp = otastsp
014100990812     c                   eval      tasftc = otasftc
014200990812     c                   eval      tastc2 = otastc2
014300990812     c                   eval      tasfap = otasfap
014400990812     c                   eval      tasaas = otasaas
014500990812     c                   eval      taslnp = otaslnp
014600990812     c                   eval      tasnrs = otasnrs
014700990812     c                   eval      tasnsp = otasnsp
014800990812     c                   eval      tastbl = otastbl
014900990812     c                   eval      tasksc = otasksc
015000990812     C                   eval      taspkb = otaspkb
015100990812     c                   eval      taspkf = otaspkb
015200990812     c                   eval      tasfpf = 'R'
015300990812     c                   eval      tasfpb = 'R'
015400990812     c                   eval      tasvlf = otasvlf
015500990812     c                   eval      tasfvf = otasfvf
015600990812     c                   eval      tasfvb = otasfvb
015700990812     c                   eval      tasvlb = otasvlb
015800990812     c                   eval      tasqft = otasqft
015900990812     c                   eval      tasffd = otasffd
016000990812     c                   eval      tasfca = otasfca
016100990812     c                   eval      tasnzm = otasnzm
016200990812     c                   eval      tascam = otascam
016300990812     c                   eval      tasnzd = otasnzd
016400990812     c                   eval      tascad = otascad
016500990812     c                   eval      tasvas = otasvas
016600990812     c                   eval      tasias = otasias
016700990812     c                   eval      tasfei = otasfei
016800990812     c                   eval      tasfts = otasfts
016900990812     c                   eval      tasfic = otasfic
017000990812     c                   eval      tasdtv = otasdtv
017100990812     c                   eval      tasorv = otasorv
017200990812     c                   eval      taspru = otaspru
017300990812      *
017400990812      * totale imponibile per 1 bolla testo il flag per tas10 e tasp
017500990812      *  se ci sono degli importi lo metto
017600990812    2c                   if        otasfti='1'
017700990812     C                   EVAL      tasimv = (otaspor  +  otasinl  +  otasdif  +
017800990812     c                                       otasva1  +  otasva2  +  otasva3)
017900990812    2c                   endif
018000990812      * se le varie sono già tutte valorizzate vado a recuperarne delle altre
018100990812      * nel file combinato TNTA7 e le scrivo nel nuovo file
018200990812      *
018300990812    2c                   if        otassv3 <> *blanks
018400990812     c     ktas          setll     tnta7v1l
018500990812     c     ktas          reade     tnta7v1l
018600990812    3c                   dow       not %EOF
018700990812      *
018800990812    4c                   if        otasfti='1'
018900990812     c                   add       ota7van       tasimv
019000990812    4c                   endif
019100990812      *
019200990812     c                   eval      ta7aas = ota7aas
019300990812     c                   eval      ta7lnp = ota7lnp
019400990812     c                   eval      ta7nrs = ota7nrs
019500990812     c                   eval      ta7nsp = ota7nsp
019600990812     c                   eval      ta7tbl = ota7tbl
019700990812     c                   eval      ta7svn = ota7svn
019800990812     c                   eval      ta7van = ota7van
019900990812     c                   eval      ta7dtv = otasdtv
020000990812     c                   eval      ta7orv = otasorv
020100990812     c                   eval      ta7pru = otaspru
020200990812     c                   write     tita7V00                             95
020300990812      *
020400990812     c     ktas          reade     tnta7v1l
020500990609    3c                   enddo
020600990609    2c                   endif
020700990812      * MITTENTE e DESTINATARIO
020800990608     c                   move      otasksc       w0040             4 0
020900990812      *  franco
021000990812    2c                   IF        %subst(Otastbl:1:1) = 'F'
021100990812     c                   eval      tasrsd = otasrsc
021200990812    3c                   if        w0040 <> 8888
021300990812     c                   eval      tasccm = otasksc
021400990812   x3c                   else
021500990812     c                   z-add     8888          tasccm
021600990812     c                   movel     otaslnp       tasccm
021700990812     c                   eval      tasrsc = otasrsd
021800990812     c                   eval      tasinm = otasind
021900990812     c                   eval      tasloc = otaslod
022000990812     c                   eval      tasprv = otasprv
022100990812     c                   eval      tascap = otascap
022200990812     c                   eval      tasnaz = otasnaz
022300990812     c                   eval      taspiv = otaspiv
022400990621     c                   endif
022500990812      *
022600990812    3c                   ELSE
022700990812      *  assegnato
022800990812    3c                   if        otasccm = *zeros
022900990812     c                   z-add     8888          tasccm
023000990812     c                   movel     otaslnp       tasccm
023100990812     c                   endif
023200990812     c                   eval      tasrsc = otasrsc                             Mittente
023300990812     c                   eval      tasrsd = otasrsd                             Destinatario
023400990812    2c                   ENDIF
023500990812
023600990812     c                   write     titasV00                             95
023700990812
023800990812     c                   READ      tntasV0f                               99
023900990812
024000990407e   1c                   ENDDO
024100990812
024200990728     c     fine          tag
024300990812
024400990812     c                   EVAL      *INLR = *ON
024500990812      *--------------------------------------------------------------------------------------------*
024600990812      * *inzsr - operazioni iniziali
024700990812      *--------------------------------------------------------------------------------------------*
024800990722     c     *inzsr        begsr
024900990812      *
025000990608     c     ktas          KLIST                                                  *tntas30c
025100990608     c                   KFLD                    otasaas                         -anno
025200990608     c                   KFLD                    otaslnp                         -linea partenza
025300990608     c                   KFLD                    otasnrs                         -serie
025400990608     c                   KFLD                    otasnsp                         -spedizione
025500990812     c                   KFLD                    otasdtv                         -data variazione
025600990812     c                   KFLD                    otasorv                         -ora  variazione
025700990812     c                   KFLD                    otaspru                         -utente variazione
025800990812      *
025900990407     c                   ENDSR
026000990812      *--------------------------------------------------------------------------------------------*
026100990812      * scrivo le varie in tita7
026200990812      *--------------------------------------------------------------------------------------------*
026300990608     C     write7        BEGSR
026400990812      *
026500990708     c                   select
026600990812
026700990812     c                   when      tassv1 = *blanks
026800990812     c                   eval      tassv1 =ta7svn
026900990812     c                   eval      tasva1 =ta7van
027000990812
027100990812     c                   when      tassv2 = *blanks
027200990812     c                   eval      tassv2 = ta7svn
027300990812     c                   eval      tasva2 = ta7van
027400990708
027500990812     c                   when      tassv3 = *blanks
027600990812     c                   eval      tassv3 = ta7svn
027700990812     c                   eval      tasva3 = ta7van
027800990708
027900990708     c                   other
028000990812     c                   eval      ta7aas = otasaas
028100990812     c                   eval      ta7lnp = otaslnp
028200990812     c                   eval      ta7nrs = otasnrs
028300990812     c                   eval      ta7nsp = otasnsp
028400990812     c                   eval      ta7tbl = otastbl
028500990812     c                   eval      ta7dtv = otasdtv
028600990812     c                   eval      ta7orv = otasorv
028700990812     c                   eval      ta7pru = otaspru
028800990812     c                   write     tita7v00                             95
028900990812
029000990812     c                   endsl
029100990812      *
029200990608     c                   ENDSR
029300990812      *--------------------------------------------------------------------------------------------*
029400990609     Oprtf198   E            BOLLA       1
029500990609     o                                              'Bolla con porto>99.999.999'
029600990609     o                       otasaas       Z  +   1
029700990609     o                       otaslnp       Z  +   1
029800990609     o                       otasnrs       Z  +   1
029900990609     o                       otasnsp       Z  +   1
030000990609     o                       otastbl          +   1
030100990609     o                       otaspor       2  +   1
030200990609     o                       otasinl       2  +   1
030300990609     o                       otasdif       2  +   1
030400990609     o                       otassv1          +   1
030500990609     o                       otasva1       2  +   1
030600990609     o                       otassv2          +   1
030700990609     o                       otasva2       2  +   1
030800990609     o                       otassv3          +   1
030900990609     o                       otasva3       2  +   1
031000990609     O          E            BOLLAnoag   1
031100990609     o                                              '                          '
031200990609     o                       otasaas       Z  +   1
031300990609     o                       otaslnp       Z  +   1
031400990609     o                       otasnrs       Z  +   1
031500990609     o                       otasnsp       Z  +   1
031600990609     o                       otastbl          +   1
031700990609     o                       otaspor       2  +   1
031800990609     o                       otasinl       2  +   1
031900990609     o                       otasdif       2  +   1
032000990609     o                       otassv1          +   1
032100990609     o                       otasva1       2  +   1
032200990609     o                       otassv2          +   1
032300990609     o                       otasva2       2  +   1
032400990609     o                       otassv3          +   1
032500990609     o                       otasva3       2  +   1
032600990609     o                                        +   1 'Importo non aggiornato:'
032700990609     o                       comores       2  +   1
