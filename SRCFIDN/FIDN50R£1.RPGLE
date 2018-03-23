000100980506      *------------------------------------------------------------------------*
000200990204      *   CALCOLO  RESPONSABILITA'                                             *
000300980506      *------------------------------------------------------------------------*
000400980506     H DECEDIT('0,') DATEDIT(*DMY.)
000500980506      *--------------------------------------------
000600990129     FFNDCT01L  UF   E           K DISK
000700990129     FFNDCD01L  IF   E           K DISK
000800990208     FFNDCF01L  IF   E           K DISK
000900990129     FTABEL00F  IF   E           k DISK
001000990305     FTNTBE02L  IF   E           k DISK
001100080714     Ffnblp01l  IF   E           k DISK
001200090120     Ffnarb01l  IF   E           k DISK    prefix(blp)
001300080714     Ffnanm02l  IF   E           k DISK
001400990204     Ffnart27l  IF   E           k DISK
001500070131     Ffnbrv07l  IF   E           k DISK
001600070711     Ffnbrve1l  IF   E           k DISK
001700070711     F                                     RENAME(Fnbrv000:FNBRVE1)
001800070711     f                                     prefix(E_)
001900040907     Ffnfgv01l  IF   E           k DISK
002000040907     Ffnfgw01l  IF   E           k DISK
002100990204     FAZCLN01L  IF   E           k DISK
002200990304     FAZORG01L  IF   E           k DISK
002300990305     FFNDET01L  IF   E           k DISK
002400990216     FFNDCR02L  UF A E           k DISK
002500090302     Ffiar501L  IF   E           k DISK
002600980506      *--------------------------------------------
002700990129      * schiere
002800990129      *--------------------------------------------
002900990208     D MSG             S             78    DIM(8) CTDATA PERRCD(1)              MSG VIDEO
003000100914     D OFIL            S             13    dim(2510)
003100100914     D OPER            S              5  2 dim(2510)
003200100914     D OTCR            S              1    dim(2510)
003300100914     D OTRE            S              4    dim(2510)
003400990209     D
003500990208     D NPS             S              2    dim(90)
003600990208     D NPSNO           S              2    dim(90)
003700990209     D CATARR          S              1    dim(4)
003800990211     D CATPAR          S              1    dim(4)
003900990330     D AMSPU           S              1    dim(30)
004000990330     D AVSPU           S              1    dim(30)
004100050615     D Noanomspu       S              1    dim(50)
004200990305     D ANIDD           S              3  0 dim(50)
004300990210     D
004400990209     D WAAVNPG         S              1    dim(90)
004500021120     D WAAVAS          S              3    dim(90)
004600000322     D WAAVFGS         S              3  0 dim(90)
004700990209     D WAAVDFV         S              8  0 dim(90)
004800990209     D WAAVSCA         S             12  0 dim(90)
004900990209     D WAAVDEF         S              1    dim(90)
005000990210     D WAAVSPG         S              1    dim(90)
005100000322     D WAAVNPS         S              2  0 dim(90)
005200100609     d
005300100609     D  RESPO          s              1    dim(10)
005400100609     D  RESPC          s              5  2 dim(10)
005500040907     D                 DS
005600040907     D  FFV                    1    897
005700040907     D                                     DIM(299)
005800040907    +D  FGVFFV                 1    240
005900040907    +D  FGVFF2               241    447
006000040907    +D  W1FF3                448    687
006100040907    +D  W1FF4                688    897
006200040907     D                 DS
006300040907    +D  FLP                    1    897
006400040907     D                                     DIM(299)
006500040907    +D  FGVFLP                 1    240
006600040907    +D  FGVFL2               241    447
006700040907    +D  W1FL3                448    687
006800040907    +D  W1FL4                688    897
006900040907     D                 DS
007000980507      *--------------------------------------------
007100990129      * DS esterne
007200990129      *--------------------------------------------
007300980507     D KPJBA         E DS
007400980508     D CNCR80        E DS
007500980507     D UT§DSE0F      E DS
007600990128     D FIDN50DS      E DS
007700990128     D TIBS02DS      E DS
007800990129     D DDCT01        E DS
007900990216     D DDCR01        E DS
008000080714     D DS1B          E DS
008100080714     D DS2A          E DS
008200990129     D DTAD          E DS
008300990208     D DS7J          E DS
008400990305     D DS7C          E DS
008500990210     D DS3E          E DS
008600990210     D DSTD          E DS
008700010910     D DGEDDN        E DS
008800080903     D OG143         E DS
008900080903     D OG148         E DS
009000021120     D OG140         E DS
009100990304     D DTRS          E DS
009200100609     D  RESPOf                 1     10    dim(10)
009300100609     D  RESPCf                11     60  2 dim(10)
009400100609     D  RESPOa                70     79    dim(10)
009500100609     D  RESPCa                80    129  2 dim(10)
009600100610     D  RESPOt               130    139    dim(10)
009700100610     D  RESPCt               140    189  2 dim(10)
009800990208     D DSLV53        E DS                  EXTNAME(fnlv53ds)
009900990208     D DSLV55        E DS                  EXTNAME(fnlv55ds)
010000090303     d
010100090303     D Dar5gen       E DS
010200990129      *--------------------------------------------
010300990129      * DS interne
010400990129      *--------------------------------------------
010500990208     D                 DS
010600990129     D  dctaac                 1      4  0
010700990129     D  dctmgc                 5      8  0
010800990208     D  dctdca                 1      8  0
010900990129     D clnmat          DS
011000990208     D  MAT                    1     31    dim(31)
011100990129     D clnpom          DS
011200990208     D  pom                    1     31    dim(31)
011300990209     D                 DS
011400070131     D  brvdcs                 1      8  0
011500070131     D  brvhcs                 9     14  0
011600990209     D  brvscar                1     12  0
011700990209     D                 DS
011800990210     D  wdcs                   1      8  0
011900070131     D  whcs                   9     12  0
012000990210     D  WSCAR                  1     12  0
012100990311     D                 DS
012200990311     D  wEXPdcs                1      8  0
012300070131     D  wEXPhcs                9     12  0
012400990311     D  WEXPSCAR               1     12  0
012500990208     D                 DS
012600990208     D  kaaa                   1      4  0
012700990208     D  kmmm                   5      6  0
012800990208     D  kgg                    7      8  0
012900990208     D  kDATA                  1      8  0
013000090120     D                 DS
013100090120     D  blpaas                 1      4  0
013200090120     D  blpmgs                 5      8  0
013300090120     D  blpdsp                 1      8  0
013400990216     D                 DS
013500990216     D  WPOR                   1      3  0
013600990216     D  WSEGNADAN              4     10
013700090324     D  WTER                  11     13  0
013800090324     D  WOKEY                  1     13
013900990129      *--------------------------------------------
014000990129      * definizione campi
014100990129      *--------------------------------------------
014200990208     D WDATAFASE       S               D   DATFMT(*iso)
014300990208     D WDATAANM        S               D   DATFMT(*iso)
014400050204     d Datadmy         s               d   datfmt(*dmy)
014500990212     D                 DS
014600990212     D WTEMPO                          Z   inz(Z'1999-01-01-01.00.00.000000')
014700990212     D  WDATA                          D   DATFMT(*iso) OVERLAY(WTEMPO)
014800990212     D  WORA                           T   TIMFMT(*iso) OVERLAY(WTEMPO:12)
014900990211     D
015000130513     D areater         S                   LIKE(orgcar)
015100130513     D nfvpar          S                   LIKE(wnfv)
015200060309     D fgspar          S                   LIKE(wfgs)
015300060309     D brvas           S                   LIKE(§OGAEX)
015400000322     D WSAVDCH         S                   LIKE(anmdch)
015500990305     D WSAVCCH         S                   LIKE(anmcch)
015600990305     D WSAVDAO         S                   LIKE(anmdao)
015700990305     D WCOD            S                   LIKE(t02cod)
015800990304     D WLIN            S                   LIKE(t02lin)
015900990304     D WSIF            S                   LIKE(t02sif)
016000990304     D WKE1            S                   LIKE(t02ke1)
016100070711     D SAVWKE1         S                   LIKE(t02ke1)
016200990209     D WKE2            S                   LIKE(t02ke2)
016300990129     D WNCN            S                   LIKE(dctncn)
016400990129     D WCOLLODAN       S                   LIKE(dctncn)
016500990129     D WPER            S                   LIKE(dctper)
016600090303     D Percen          S                   LIKE(dctper)
016700990129     D KTFA            S                   LIKE(clntfa)
016800990129     D KTFP            S                   LIKE(clntfp)
016900000321     D FLPTFA          S                   LIKE(clntfp)
017000990129     D WTCR            S                   LIKE(dcttcr)
017100990129     D cod             S                   LIKE(tblcod)
017200990129     D key             S                   LIKE(tblkey)
017300990204     D SAVKPJBU        S                   LIKE(kpjbu)
017400990210     D WDFV            S                   LIKE(d53dfv)
017500990311     D WEXPDFV         S                   LIKE(d53dfv)
017600990210     D WSPG            S                   LIKE(d53spg)
017700990210     D WFGS            S                   LIKE(d53fgs)
017800040907     D WNFV            S                   LIKE(d53nfv)
017900070711     D WNFVA           S                   LIKE(BRVNFV)
018000990311     D WEXPFGS         S                   LIKE(d53fgs)
018100990210     D WNPG            S                   LIKE(d53npg)
018200990311     D WEXPNPG         S                   LIKE(d53npg)
018300990212     D WERR            S              1
018400990329     D WRIT            S              1
018500990205     D WDATRIFER       S                   LIKE(d53dfv)
018600990208     D WFLP            S                   LIKE(artflp)
018700070821     D WNPGSPUAN       S                   LIKE(BRVNPG)
018800070821     D WFLPSPUAN       S                   LIKE(artflp)
018900990309     D WFGSSPUAN       S                   LIKE(artflp)
019000990305     D WCLIP           S                   LIKE(artflp)
019100990305     D WCLIA           S                   LIKE(artflp)
019200990208     D WDCTLNPC        S                   LIKE(dctlnp)
019300990210     D WDEF            S                   LIKE(d53def)
019400990311     D WEXPDEF         S                   LIKE(d53def)
019500990210     D WPRECNPG        S                   LIKE(brvnpg)
019600990210     D WPRECFGS        S                   LIKE(brvfgs)
019700990210     D WPRECSPG        S                   LIKE(d53spg)
019800990304     D LNPFL1          S                   LIKE(orgfl1)
019900080903     D lnpntw          S                   LIKE(§ogntw)
020000150604     D lnantw          S                   LIKE(§ogntw)
020100131018     D lnpMAMntw       S                   LIKE(§ogntw)
020200990304     D LNAFL1          S                   LIKE(orgfl1)
020300100610     D lnptfp          S                   LIKE(i50tfp)
020400990210     D WPREC200        S             20  0
020500000322     D WSTESSOAS       S              1
020600060301     D Wautogen        S              1
020700000322     D WOK             S              1
020800000322     D JJ              S              3  0
020900990305     D WAPERT          S              1
021000050204     d wggmmaa         s              6
021100050204     d wgioset         s              1
021200090422     d soloCAN         s              1
021300131018     d Wes_mem         s              1
021400090303     d ktrd            s                   like(ar5trd) inz('GEN')
021500990129      *----------------------------------------------------------
021600990129      *  RIEPILOGO INDICATORI
021700980908      *----------------------------------------------------------
021800990204     C                   MOVEL     KPJBU         FIDN50DS
021900990209     C                   clear                   ofil
022000990209     C                   clear                   oper
022100990209     C                   clear                   otcr
022200051010     C                   clear                   Wricalcolo
022300131018     C                   clear                   Wes_mem
022400990128     c**
022500990128     c** C A L C O L O   L A   R E S P O N S A B I L I T A'
022600990128     c**
022700990129    1c     I50MOD        ifeq      'C'
022800990129     c                   clear                   o50por
022900990129     c                   clear                   o50per
023000990129     c                   clear                   o50car
023100990129     c                   clear                   o50tcr
023200990129     c                   clear                   o50msg
023300990129     c                   clear                   o50err
023400990128     c** chain in update la testata
023500990128     c     kdct          chain     fndct01l                           3099
023600990128     c* non trovata C.A.
023700990129    2c     *in30         ifeq      *on
023800990129     c                   movel     msg(2)        o50msg
023900990128     c                   movel     'E'           o50err
024000990128     c                   goto      fine
024100990129    2c                   endif
024200990128     c* record allocato: impossibile l'aggiornamento
024300990129    2c     *in99         ifeq      *on
024400990129     c                   movel     msg(3)        o50msg
024500990128     c                   movel     'E'           o50err
024600990128     c                   goto      fine
024700990129    2c                   endif
024800990129     c* IMPOSTO I COLLI DANNEGGIATI MAMCANTI: se manca il dato imposto
024900990129     c*   tutti i colli della bolla
025000990208    2c     dctncn        ifeq      0
025100990129     c     i50ncl        andeq     0
025200990208     c                   movel     msg(4)        o50msg
025300990129     c                   movel     'E'           o50err
025400990129     c                   goto      fine
025500990129     c***
025600990129   x2c                   else
025700990208    3c     dctncn        ifgt      0
025800990129     c                   z-add     dctncn        wncn
025900990129     c                   else
026000990129     c                   z-add     i50ncl        wncn
026100990129    3c                   endif
026200990129    2c                   endif
026300071018     c
026400071018     c* Conto quanti record ci sono nel file delle responsabilità
026500071018     c                   clear                   wcollidcr         6 2
026600071018     c     kdct          chain(N)  fndcr02l                           30
026700071018    2c     *in30         doweq     *off
026800071018     c     dcratb        ifeq      ' '
026900071018     c                   add       dcrper        wcollidcr
027000071018     c                   endif
027100071018     c
027200071018     c     kdct          reade(N)  fndcr02l                               30
027300071018    2c                   enddo
027400071018     c**
027500071018     c** Se si tratta di calcolo manuale controllo solo se devo adeguar
027600071018     c** il numero colli di DCR con DCD
027700071018     c                   if        dctcar='M'
027800071018     c                   if        wcollidcr<>wncn
027900071018     c                   exsr      AllineaDCRDCD
028000071018     c                   endif
028100071018     c
028200071018     c                   goto      FINE
028300071018     c                   endif
028400990129     c**
028500990129     c                   movel     dctflo        ddct01
028600990208     c                   clear                   wdctlnpc
028700990208     c     §dctlnpc      ifgt      *zeros
028800990208     c                   movel     §dctlnpc      wdctlnpc
028900990208     c                   endif
029000990304     c
029100050914     c* Verifico se lnp c.a. bolla mamma estera --> IMPORT
029200050914     C     i50lnpMAM     chain     azorg01l                           39
029300990304     c   39              clear                   orgfl1
029400080903     c   39              clear                   og143
029500990304     c                   eval      LNPFL1=orgfl1
029600131018     c                   eval      lnpMAMntw=§ogntw
029700131018
029800131018     c* Verifico ntw della bolla
029900131018     C     i50lnp        chain     azorg01l                           39
030000131018     c  n39              movel     orgde3        og143
030100131018     c   39              clear                   og143
030200131018     c                   eval      lnpntw=§ogntw
030300990304     c
030400990304     c* Verifico se lna estera --> EXPORT
030500990304     C     i50lna        chain     azorg01l                           39
030600990304     c   39              clear                   orgfl1
030700150604     c   39              clear                   og143
030800150604     c   39              clear                   orgde8
030900990308     c   39              clear                   orgfel
031000990304     c
031100150604     c  n39              movel     orgde3        og143
031200990304     c                   movel     orgde8        OG148
031300990304     c                   eval      LNAFL1=orgfl1
031400150604     c                   eval      LNAntw=§ogntw
031500990304     c**
031600990304     c* A seconda del tipo anomalia cerco la responsab.
031700990304     C                   clear                   tibs02ds
031800990304     C                   MOVEL     'C'           t02mod
031900990304     C                   MOVEL     knsif         t02sif
032000990304     C                   MOVEL     'TAD'         t02cod
032100990304     C                   MOVEL     dcttad        t02ke1
032200990304     C                   CALL      'TIBS02R'
032300990304     C                   PARM                    KPJBA
032400990304     C                   PARM                    TIBS02DS
032500990305     c
032600990305   1aC                   IF        t02err = *BLANKS
032700990305     C                   movel     T02uni        DTAD
032800090120
032900090120     c* chaino la bolla in partenza per prendere il cod trattamento merce
033000090120     c                   clear                   ds1b
033100090120     c                   clear                   blpctm
033200090120     c                   clear                   blpdsp
033300090120     c     kblp          chain     fnblp01l
033400090120     c                   if        not %found(fnblp01l)
033500090120     c     kblp          chain     fnarb01l
033600090120     c                   endif
033700090120     c                   if        blpctm<>*blanks
033800090120     c                   movel     '1B'          cod
033900090120     c                   movel     blpctm        key
034000090120     c     ktab          chain     tabel
034100090120     c                   if        %found(tabel00f)
034200090120     c                   movel     tbluni        ds1b
034300090120     c                   endif
034400090120     c                   endif
034500090302     c
034600090302     c* Verifico se bolla con merce di valore
034700090302     c     kar5          chain(N)  fiar501l
034800090302     c                   if        %found(fiar501l)
034900090302     c                   movel     ar5uni        dar5gen
035000090302     c                   else
035100090302     c                   clear                   dar5gen
035200090302     c                   endif
035300990305     c**
035400990305     c* 1) Se si tratta di evento : "SMARRIMENTO AUTOTRASPORTATORE"
035500990305     c** colpa di chi apre
035600990305    2c                   if        dctnev>0
035700990305     c     keve          chain     fndet01l                           34
035800990305    3c                   if        not *in34  AND
035900990305     c                             dettad='15'
036000990305    4c                   if        lnpfl1='E'
036100990305     c                   MOVEL(p)  'SMARRAUT_E'  wke1
036200990305   x4c                   else
036300990305     c                   MOVEL(p)  'SMARRAUT'    wke1
036400990305    4c                   endif
036500990308     c
036600990308     c                   z-add     wncn          wcollodan
036700990308     c                   clear                   wsegnadan
036800990305     c                   exsr      MEMRES
036900990305     c                   exsr      WRIRES
037000990305     c                   goto      fine
037100990305    3c                   endif
037200990305    2c                   endif
037300990308     c**
037400990308     c* 2) RESPONSABILITA' PARTNER
037500990305     c**
037600990308     c                   if        dctres='P'
037700990308     c                   if        lnpfl1='E'
037800990308     c                   MOVEL(p)  'COLPAPART_E' wke1
037900990308     c                   else
038000990308     c                   MOVEL(p)  'COLPAARRIVO' wke1
038100990308     c                   move      '_E  '        wke1
038200990308     c                   endif
038300990308     c
038400990308     c                   z-add     wncn          wcollodan
038500990308     c                   clear                   wsegnadan
038600990308     c                   exsr      MEMRES
038700990308     c                   exsr      WRIRES
038800990308     c                   goto      fine
038900990308     c                   endif
039000990308     c**
039100990308     c* 3) C.A. APERTA DALLA PARTENZA
039200990128     c**
039300000322     c* Se bolla locale lnp=lna ma franco senza riserva, dopo consegna,
039400000322     c*  come se fosse aperta lo stesso in partenza
039500050914    2c     i50fil        ifeq      i50lnpMAM
039600050914     c     i50lnpMAM     andeq     i50lna
039700000322     c     §tadtico      andeq     'C'
039800000322     c     dctcrc        andne     'S'
039900000322     c     §dctport      andeq     'F'
040000000322     c                   eval      wapert='P'
040100000323     c                   z-add     wncn          wcollodan
040200000323     c                   clear                   wsegnadan
040300000322     c                   exsr      CLIAPERTURA
040400000322     c                   exsr      MEMRES
040500000322     c                   exsr      WRIRES
040600000322     c                   goto      fine
040700000322     c                   endif
040800000322     c
040900050914     c* Verifico se il P.O. di apertura c.a. = a lnp bolla mamma se legata
041000050914    2c     i50fil        ifeq      i50lnpMAM
041100050914     c     i50lnpMAM     andne     i50lna
041200990304     c
041300990305    3c                   select
041400990304     c* 1) Spedizione consegnata con riserva
041500990304     c                   when      §tadtico='C'  AND
041600990304     c                             dctCRC='S'
041700990304     c
041800131018     c****               exsr      ERRAPERTURA
041900131021     c* Se si tratta di bolle dirottata lascio la responsab. errore di apertura
042000131021     c                   if        blpcca='1'
042100131021     c                   exsr      ERRAPERTURA
042200131021     c                   else
042300131021     c* altrimenti vado sulle spunte
042400131018     c                   exsr      elabora
042500131021     c                   endif
042600990304     c
042700990304     c* 2) Spedizione consegnata senza riserva
042800990304     c                   when      §tadtico='C'  AND
042900990304     c                             dctcrc<>'S'
043000990304     c* Assegnato--> errore
043100990305    4c                   if        §dctport='A'
043200990304     c                   exsr      ERRAPERTURA
043300990304     c                   else
043400990304     c* Franco --> Colpa del mittente
043500990305     c                   eval      wapert='P'
043600990304     c                   exsr      CLIAPERTURA
043700990305    4c                   endif
043800990304     c
043900990305     c* 3) Spedizione aperta in partenza ma non consegnata per i colli in CA
044000990305     c                   when      §tadtico<>'C'
044100990304     c
044200990305    4c                   if        lnpfl1='E'
044300990304     c                   MOVEL(p)  'APERTAPAR_E' wke1
044400990304     c                   else
044500990211     c                   MOVEL(p)  'APERTAPAR'   wke1
044600990305    4c                   endif
044700990305    3c                   endsl
044800990304     c
044900990129     c                   z-add     wncn          wcollodan
045000990216     c                   clear                   wsegnadan
045100990129     c**
045200131018     c                   if        wes_mem=' '
045300990129     c                   exsr      MEMRES
045400131018     c                   endif
045500990129     c**
045600990129     c     o50err        ifeq      'E'
045700990129     c                   goto      fine
045800990129     c                   endif
045900990129     c**
046000990129   x2c                   else
046100990305     c**
046200990305     c** Sono in ARRIVO: controllo solo le spedizioni consegnate senza RIS
046300990305     c
046400990305    3c                   if        §tadtico='C'  AND
046500990310     c                             dctcrc<>'S'   AND
046600990310    4c                             §dctport='A'
046700990305     c* Assegnato --> Colpa del mittente
046800990305     c                   eval      wapert='A'
046900990305     c                   exsr      CLIAPERTURA
047000990305     c
047100990308     c                   z-add     wncn          wcollodan
047200990308     c                   clear                   wsegnadan
047300990308     c
047400990305     c                   exsr      MEMRES
047500990305     c
047600990305   x3c                   else
047700130307     c*
047800130307     c                   exsr      elabora
047900990128     C**
048000990305    3c                   endif
048100990129    2c                   endif
048200990305  x1ac                   else
048300990305     c* ERRORE --> NON TROVATO TIPO ANOMALIA
048400990305     c                   movel     msg(1)        o50msg
048500990305     c                   goto      FINE
048600990305   1ac                   endif
048700990129    1c                   endif
048800990208      **
048900071018     c** Memorizzo la responsabilita'
049000990216     c                   if        o50err=' '
049100990208     c                   exsr      WRIRES
049200990216     c                   endif
049300990128     c     fine          tag
049400990128     c**
049500990208      * Chiudo pgm aperti
049600131018     c                   clear                   tibs02ds
049700990128     c                   movel     'C'           t02tla
049800990128     c                   call      'TIBS02R'
049900990128     c                   parm                    kpjba
050000990128     c                   parm                    tibs02ds
050100990208     c**
050200990208     c                   clear                   dslv53
050300990208     c                   movel     'C'           d53tla
050400990208     c                   call      'FNLV53R'
050500990208     c                   parm                    dslv53
050600990208     c**
050700990208     c                   clear                   dslv55
050800990208     c                   movel     'C'           d55tla
050900990208     c                   call      'FNLV55R'
051000990208     c                   parm                    dslv55
051100990128     c**
051200990212     C                   MOVEL     fidn50ds      kpjbu
051300980506     C                   EVAL      *INLR = *ON
051400130307      *****************************************************************
051500130307     c
051600130307     c     Elabora       BEGSR
051700130307     c**
051800130307     c** 3) Se non c'e' il dettaglio colli e non c'e' riserva in bolla
051900130307     c** --> non calcolo la responsabilita'
052000130307     c     kdct          chain     fndcd01l                           30
052100130307    4c     *in30         doweq     *off
052200130307     c     dcdatb        andne     ' '
052300130307     c     kdct          reade     fndcd01l                               30
052400130307    4c                   enddo
052500130307     c**
052600130307    4c     *in30         ifeq      *on
052700130307     c                   z-add     wncn          wcollodan
052800130307     c                   clear                   wsegnadan
052900130307     c                   exsr      CT_RISERVA
053000130307     c     o50err        ifeq      *blanks
053100130307     c                   exsr      WRIRES
053200130307     c                   endif
053300130307     c                   goto      fine
053400130307    4c                   endif
053500130307     c***
053600130307     c* leggo fndcd per MANCANZE PER vedere se hanno aperto in ritardo la CA
053700130307     c                   clear                   writ
053800130307    4c     §tadragr      ifeq      'M'
053900130307     c     lnafl1        andne     'E'
054000130307     c
054100130307     c     kdct          chain     fndcd01l                           30
054200130307    5c     *in30         doweq     *off
054300130307     c     writ          andne     'N'
054400130307     c** escludo se annullata e
054500130307    6c     dcdatb        ifeq      ' '
054600130307     c     dcddch        andeq     dctdch
054700130307     c*
054800130307     c* non trovato dett.bolla --> errore
054900130307     c     kdcd          chain     fnart27l                           34
055000130307     c*
055100130307    7c     *in34         ifeq      *on
055200130307     c                   movel     msg(7)        o50msg
055300130307     c                   movel     'E'           o50err
055400130307     c                   goto      fine
055500130307    7c                   endif
055600130307     c**
055700130307     c                   eval      wcollodan=1
055800130307     c                   movel     dcdnsc        wsegnadan         7
055900130307     c
056000130307     c                   exsr      RITARDO
056100130307    6c                   endif
056200130307     c**
056300130307     c     kdct          reade     fndcd01l                               30
056400130307    5c                   enddo
056500130307    4c                   endif
056600130307      *
056700130307     c***
056800130307     c* leggo fndcd
056900130307     c     kdct          chain     fndcd01l                           30
057000130307    5c     *in30         doweq     *off
057100130307     c** escludo se annullata e
057200130307    6c     dcdatb        ifeq      ' '
057300130307     c     dcddch        andeq     dctdch
057400130307     c*
057500130307     c* non trovato dett.bolla --> errore
057600130307     c     kdcd          chain     fnart27l                           34
057700130307     c*
057800130307    7c     *in34         ifeq      *on
057900130307     c                   movel     msg(7)        o50msg
058000130307     c                   movel     'E'           o50err
058100130307     c                   goto      fine
058200130307    7c                   endif
058300130307     c**
058400130307     c                   eval      wcollodan=1
058500130307     c                   movel     dcdnsc        wsegnadan         7
058600130307     c
058700130307    7c     §tadragr      ifeq      'M'
058800130307     c                   exsr      MANCANZE
058900130307     c                   else
059000130307     c*
059100130307     c                   exsr      AMMAVARIE
059200130307    7c                   endif
059300130307    6c                   endif
059400130307     c**
059500130307     c     kdct          reade     fndcd01l                               30
059600130307    5c                   enddo
059700130307     c                   ENDSR
059800070711      *****************************************************************
059900070711      * Verifico se doppia spunta per ammanchi/avarie
060000070711      *****************************************************************
060100070711     c     CTRDOPSPU     begsr
060200080714     c* non effettuo il controllo se sonon in entrata e si tratta di
060300080714     c**  bolla con disk c
060400081021     c                   if        wnpgspuan<>5 or §1bdkc=' '
060500070711     c
060600070711     c     kbrve         chain     fnbrve1l
060700070711    1c                   if        %found(fnbrve1l) and E_brvcan=' '
060800070711     c
060900070713     c                   eval      savwke1=wke1
061000070711     c**
061100070711     c                   clear                   wsif
061200070711     c                   clear                   wlin
061300070711     c                   eval      wcod='TRS'
061400070711     c                   eval      wke2='99991231       '
061500070821     c                   eval      wke1='A_CAN_DOPARR   '
061600070711     c
061700070711     c                   clear                   WOK
061800070711     c     ktbe          setgt     tntbe02l
061900070711     c     ktbe2         readpe    tntbe02l                               39
062000070711    2c                   dow       not *in39  and  WOK<>'S'
062100070711     c                   if        tbeatb=' '
062200070711     c                   movel     tbeke2        w0080             8 0
062300070711     c     w0080         ifle      dctdca
062400070711     c                   eval      wok='S'
062500070711     c                   seton                                        39
062600070711     c                   endif
062700070711     c                   endif
062800070711     c
062900070711     c  N39ktbe2         readpe    tntbe02l                               39
063000070711    2c                   enddo
063100070711     c**
063200070711     c**
063300070711     c** Non trovato record in tabella responsabilita'
063400070711     c** procedo con l'attuale calcolo della responsabilità
063500070711    2C                   IF        wok=' '
063600070713     c                   eval      wke1=savwke1
063700070711   x2c                   else
063800070711     c* vado avanti con il nuovo codice di responsabilità
063900070711     c                   movel     tbeuni        DTRS
064000070713     c* In WFLP memorizzo il p.o. che ha rilevato l'anomalia
064100070821     c                   eval      wflp=wfgsspuan
064200090831     c                   movel     'A'           wtipoflp
064300070711    2c                   endif
064400070711    1c                   endif
064500070711     c
064600080714     c                   endif
064700070711     c                   ENDSR
064800990129      *****************************************************************
064900990129      * Cerco record e memorizzo la responsabilita'
065000990129      *****************************************************************
065100990129     c     MEMRES        begsr
065200990217     C                   clear                   wpor
065300130513     C                   clear                   areater
065400131018     c                   eval      wes_mem=' '
065500990304     c**
065600990304     c                   clear                   wsif
065700990304     c                   clear                   wlin
065800990304     c                   eval      wcod='TRS'
065900990304     c                   eval      wke2='99991231       '
066000990304     c
066100990304     c                   clear                   WOK
066200990304     c     ktbe          setgt     tntbe02l
066300990304     c     ktbe2         readpe    tntbe02l                               39
066400990304     c                   dow       not *in39  and  WOK<>'S'
066500990304     c                   if        tbeatb=' '
066600990304     c                   movel     tbeke2        w0080             8 0
066700990304     c     w0080         ifle      dctdca
066800990304     c                   eval      wok='S'
066900990308     c                   seton                                        39
067000990304     c                   endif
067100990304     c                   endif
067200990304     c
067300990308     c  N39ktbe2         readpe    tntbe02l                               39
067400990304     c                   enddo
067500990129     c**
067600990129     c**
067700990129     c** Non trovato record in tabella responsabilita'
067800070711    1C                   IF        wok=' '
067900990208     c                   movel     msg(5)        o50msg
068000990129     c                   movel     'E'           o50err
068100070711   x1c                   else
068200990129     c*
068300990304     c                   movel     tbeuni        dtrs
068400070711     c* Se si tratta do codice che controlla se "fa il furbo", per
068500070711     c*  ammanchi e avarie, efettuo il controllo
068600070711     c*  se già in linea il nuovo codice atribuzione reponsabilità
068700070711     c                   if        §trsdopspu='S'
068800070711     c                   exsr      CTRDOPSPU
068900070711     c                   endif
069000100609
069100100610     c* Se il codice prevede differenziazione responsab. tra Franchi / Assegnati / triangolazioni
069200100610     c                   clear                   respo
069300100610     c                   clear                   respc
069400100610     c
069500100610     c*  imposto la tabella per triangolazioni se c'e' e se lo  la bolla
069600100610    2c                   if        respot(1) <>' '
069700100610     c
069800100610     c* Per franchi
069900100610    3c                   if        §dctport='F'
070000100610     c* Terminal di partenza lnp C.A. e terminal di partenza lin ksc a confronto
070100100610     c                   clear                   dslv55
070200100610     c                   movel     'P'           d55tpt
070300100610     c                   z-add     wdctlnpc      d55lin
070400100610     c                   if        blpdsp>0
070500100610     c                   z-add     blpdsp        d55drf
070600100610     c                   else
070700100610     c                   z-add     dctdca        d55drf
070800100610     c                   endif
070900100610     c                   call      'FNLV55R'
071000100610     c                   parm                    dslv55
071100100610     c                   movel     d55tfp        lnptfp
071200100610     c*
071300100610     c                   z-add     wclip         d55lin
071400100610     c                   call      'FNLV55R'
071500100610     c                   parm                    dslv55
071600100610    4c                   if        d55tfp<>lnptfp
071700100610     c                   movea     respot        respo
071800100610     c                   movea     respct        respc
071900130513     c
072000130513     c*Se sono diversi devo verificare che anche le rispettive aree di appartenenza
072100130513     c*  siano diverse
072200130513     C     lnptfp        chain     azorg01l                           39
072300130513    5c                   if        %found(azorg01l)
072400130513     c                   eval      areater=orgcar
072500130513     C     d55tfp        chain     azorg01l                           39
072600130513    6c                   if        %found(azorg01l) and orgcar<>areater
072700130513     c                   movea     respot        respo
072800130513     c                   movea     respct        respc
072900130513    6c                   endif
073000130513    5c                   endif
073100100610    4c                   endif
073200100610     c
073300100610   x3c                   else
073400100610     c                   clear                   dslv55
073500100610     c                   movel     'A'           d55tpt
073600100610     c                   z-add     wclia         d55lin
073700100610     c                   if        blpdsp>0
073800100610     c                   z-add     blpdsp        d55drf
073900100610     c                   else
074000100610     c                   z-add     dctdca        d55drf
074100100610     c                   endif
074200100610     c                   call      'FNLV55R'
074300100610     c                   parm                    dslv55
074400100610     c*
074500100610    4c                   if        i50tfa<>d55tfa
074600130513     c*Se sono diversi devo verificare che anche le rispettive aree di appartenenza
074700130513     c*  siano diverse
074800130513     C     i50tfa        chain     azorg01l                           39
074900130513    5c                   if        %found(azorg01l)
075000130513     c                   eval      areater=orgcar
075100130513     C     d55tfa        chain     azorg01l                           39
075200130513    6c                   if        %found(azorg01l) and orgcar<>areater
075300100610     c                   movea     respot        respo
075400100610     c                   movea     respct        respc
075500130513    6c                   endif
075600130513    5c                   endif
075700130513    4c                   endif
075800130513     c
075900100610    3c                   endif
076000100610    2c                   endif
076100100610     c
076200100610    2c                   if        respo(1) = ' '
076300100610     c* Per Assegnati
076400100610     c* Terminal di arrivo lna C.A. e terminal di arrivo lin ksc a confronto
076500100610     c
076600100610     c*  imposto le tabelle di fil e codici giuste, altrimenti la prima (quella dei franchi)
076700100610    3c                   if        respoa(1) <>' ' and §dctport='A'
076800100609     c                   movea     respoa        respo
076900100609     c                   movea     respca        respc
077000100609     c                   else
077100100609     c                   movea     respof        respo
077200100609     c                   movea     respcf        respc
077300100610    3c                   endif
077400100610    2c                   endif
077500990129     c*
077600990218     c                   z-add     1             xx                4 0
077700070711    2c     respo(xx)     downe     *blanks
077800040907     c*
077900040907     c                   if        respo(xx) = 'V' and wnfv=0
078000040907     c                   eval      respo(xx) = 'A'
078100040907     c                   endif
078200990303     c
078300990129     c                   select
078400990308     c** PO e' "F" --> terminal partenza p.o. apertura c.a.
078500990303     c
078600990208     c     respo(xx)     wheneq    'F'
078700990303     c                   clear                   dslv55
078800990303     c                   movel     'P'           d55tpt
078900990303     c                   z-add     dctfil        d55lin
079000090325     c                   if        blpdsp>0
079100090325     c                   z-add     blpdsp        d55drf
079200090325     c                   else
079300090325     c                   z-add     dctdca        d55drf
079400090325     c                   endif
079500990303     c                   call      'FNLV55R'
079600990303     c                   parm                    dslv55
079700990303     c                   z-add     d55tfp        wpor
079800090324     c                   z-add     d55tfp        wter
079900990303     c
080000990129     c* se = a lnp dct e' P altrimenti e' "T"
080100990308     c     dctfil        ifeq      wdctlnpc
080200990129     c                   movel     'P'           wtcr
080300990308     c                   else
080400990308     c     dctfil        ifeq      dctlna
080500990308     c                   movel     'A'           wtcr
080600990129     c                   else
080700990129     c                   movel     'T'           wtcr
080800990129     c                   endif
080900990308     c                   endif
081000990303     c
081100990308     c** PO e' "C" --> p.o. apertura c.a.
081200990308     c     respo(xx)     wheneq    'C'
081300990308     c                   z-add     dctfil        wpor
081400090831     c                   clear                   wtipoflp
081500990308     c
081600990308     c* se = a lnp dct e' P altrimenti e' "T"
081700990308     c     dctfil        ifeq      wdctlnpc
081800990308     c                   movel     'P'           wtcr
081900990308     c                   else
082000990308     c     dctfil        ifeq      dctlna
082100990308     c                   movel     'A'           wtcr
082200990308     c                   else
082300990308     c                   movel     'T'           wtcr
082400990308     c                   endif
082500990308     c                   endif
082600090324     c
082700090324     c                   EXSR      CallTER
082800990308     c
082900090120     c** PO e' "P" --> teminal partenza di lnp bolla originale
083000990208     c     respo(xx)     wheneq    'P'
083100990303     c                   clear                   dslv55
083200990303     c                   movel     'P'           d55tpt
083300990303     c                   z-add     wdctlnpc      d55lin
083400090120     c                   if        blpdsp>0
083500090120     c                   z-add     blpdsp        d55drf
083600090120     c                   else
083700090120     c                   z-add     dctdca        d55drf
083800090120     c                   endif
083900990303     c                   call      'FNLV55R'
084000990303     c                   parm                    dslv55
084100990303     c                   z-add     d55tfp        wpor
084200990129     c                   movel     'P'           wtcr
084300090324     c                   z-add     d55tfp        wter
084400131018
084500131018     c** PO e' "O" -->  lnp bolla originale
084600131018     c     respo(xx)     wheneq    'O'
084700131018     c                   clear                   dslv55
084800131018     c                   movel     'P'           d55tpt
084900131018     c                   z-add     wdctlnpc      d55lin
085000131018     c                   if        blpdsp>0
085100131018     c                   z-add     blpdsp        d55drf
085200131018     c                   else
085300131018     c                   z-add     dctdca        d55drf
085400131018     c                   endif
085500131018     c                   call      'FNLV55R'
085600131018     c                   parm                    dslv55
085700131018     c                   z-add     wdctlnpc      wpor
085800131018     c                   movel     'P'           wtcr
085900131018     c                   z-add     d55tfp        wter
086000990303     c
086100090324     c** PO e' "1" --> terminal partenza del codice cliente mitt o dest
086200990305     c     respo(xx)     wheneq    '1'
086300990305     c                   clear                   dslv55
086400990305     c                   movel     'P'           d55tpt
086500990305     c                   if        wclip>0
086600990305     c                   z-add     wclip         d55lin
086700990305     c                   movel     'P'           wtcr
086800990305     c                   else
086900990305     c                   z-add     wclia         d55lin
087000990305     c                   movel     'A'           wtcr
087100990305     c                   endif
087200990305     c
087300090325     c                   if        blpdsp>0
087400090325     c                   z-add     blpdsp        d55drf
087500090325     c                   else
087600090325     c                   z-add     dctdca        d55drf
087700090325     c                   endif
087800990305     c                   call      'FNLV55R'
087900990305     c                   parm                    dslv55
088000990305     c                   z-add     d55tfp        wpor
088100090324     c                   z-add     d55tfp        wter
088200990305     c
088300990309     c** PO e' "2" --> p.o. codice cliente mitt o dest
088400990309     c     respo(xx)     wheneq    '2'
088500990309     c                   if        wclip>0
088600990309     c                   z-add     wclip         wpor
088700990309     c                   movel     'P'           wtcr
088800990309     c                   else
088900990309     c                   z-add     wclia         wpor
089000990309     c                   movel     'A'           wtcr
089100990309     c                   endif
089200090831     c
089300090831     c                   clear                   wtipoflp
089400090324     c                   exsr      CallTER
089500990309     c
089600990303     c** PO e' "A" --> terminal arrivo di lna bolla
089700990208     c     respo(xx)     wheneq    'A'
089800000321     c                   if        flptfa>0
089900000321     c                   movel     flptfa        wpor
090000000321     c                   else
090100990205     c                   movel     i50tfa        wpor
090200000321     c                   endif
090300990129     c                   movel     'A'           wtcr
090400090324     c                   movel     wpor          wter
090500990303     c
090600990303     c** PO e' "R" --> terminal partenza di lna bolla
090700990303     c     respo(xx)     wheneq    'R'
090800990303     c                   clear                   dslv55
090900990303     c                   movel     'P'           d55tpt
091000990303     c                   z-add     i50lna        d55lin
091100090325     c                   if        blpdsp>0
091200090325     c                   z-add     blpdsp        d55drf
091300090325     c                   else
091400090325     c                   z-add     dctdca        d55drf
091500090325     c                   endif
091600990303     c                   call      'FNLV55R'
091700990303     c                   parm                    dslv55
091800990303     c                   z-add     d55tfp        wpor
091900990303     c                   movel     'A'           wtcr
092000090324     c                   z-add     d55tfp        wter
092100990309     c
092200990309     c** PO e' "S" --> terminal partenza di p.o. disguido
092300990309     c     respo(xx)     wheneq    'S'
092400990309     c                   clear                   dslv55
092500990309     c                   movel     'P'           d55tpt
092600990309     c                   z-add     wfgs          d55lin
092700090325     c                   if        blpdsp>0
092800090325     c                   z-add     blpdsp        d55drf
092900090325     c                   else
093000090325     c                   z-add     dctdca        d55drf
093100090325     c                   endif
093200990309     c                   call      'FNLV55R'
093300990309     c                   parm                    dslv55
093400990309     c                   z-add     d55tfp        wpor
093500990309     c                   movel     'T'           wtcr
093600090324     c                   z-add     d55tfp        wter
093700990309     c
093800990304     c** PO e' "X" --> linea di arrivo  bolla
093900990308     c     respo(xx)     wheneq    'X'
094000990304     c                   movel     i50lna        wpor
094100990304     c                   movel     'A'           wtcr
094200090324     c                   movel     i50tfa        wter
094300990303     c
094400070713     c** PO e' "T" --> P.O. passato in wflp
094500990208     c     respo(xx)     wheneq    'T'
094600060301     c                   movel     wflp          wpor
094700070713     c                   select
094800070713     c                   when      wflp=i50tfp or wflp=i50lnp
094900060301     c                   movel     'P'           wtcr
095000090324     c                   movel     i50tfp        wter
095100070713     c                   when      wflp=i50tfa or wflp=i50lna
095200070713     c                   movel     'A'           wtcr
095300090324     c                   movel     i50tfa        wter
095400070713     c                   other
095500990129     c                   movel     'T'           wtcr
095600090324     c                   exsr      CallTER
095700070713     c                   endsl
095800081204     c
095900081204     c** PO e' "N" --> P.O. passato in wfgsspuan
096000081204     c     respo(xx)     wheneq    'N'
096100081204     c                   movel     wfgsspuan     wpor
096200081204     c                   select
096300081204     c                   when      wfgsspuan=i50tfp or wfgsspuan=i50lnp
096400081204     c                   movel     'P'           wtcr
096500090324     c                   movel     i50tfp        wter
096600081204     c                   when      wfgsspuan=i50tfa or wfgsspuan=i50lna
096700081204     c                   movel     'A'           wtcr
096800090324     c                   movel     i50tfa        wter
096900081204     c                   other
097000081204     c                   movel     'T'           wtcr
097100090324     c                   exsr      CallTER
097200081204     c                   endsl
097300990303     c
097400990304     c** PO e' "L" --> terminal partenza linea partenza bolla
097500990208     c     respo(xx)     wheneq    'L'
097600990205     c                   movel     i50tfp        wpor
097700090324     c                   movel     i50tfp        wter
097800990129     c* se = a lnp dct e' P altrimenti e' "T"
097900990208     c     i50lnp        ifeq      wdctlnpc
098000990129     c                   movel     'P'           wtcr
098100990129     c                   else
098200990129     c                   movel     'T'           wtcr
098300990129     c                   endif
098400990304     c
098500990304     c** PO e' "E" --> Linea partenza bolla
098600990304     c     respo(xx)     wheneq    'E'
098700990304     c                   movel     i50lnp        wpor
098800090324     c                   movel     i50tfp        wter
098900990304     c* se = a lnp dct e' P altrimenti e' "T"
099000990304     c     i50lnp        ifeq      wdctlnpc
099100990304     c                   movel     'P'           wtcr
099200990304     c                   else
099300990304     c                   movel     'T'           wtcr
099400990304     c                   endif
099500040907     c
099600040907     c** PO e' "V" --> p.o. scarico foglio viaggio
099700040907     c     respo(xx)     wheneq    'V'
099800040907     c     kfgv          chain     fnfgv01l
099900040907    1c                   if        %found(fnfgv01l)
100000040907    2c                   if        i50lna=fgvlna
100100040907     c                   movel     i50lna        wpor
100200040907     c                   movel     'A'           wtcr
100300090324     c                   movel     i50tfa        wter
100400040907   x2c                   else
100500040907     c     kfgv          chain     fnfgw01l
100600040907if  3c                   if        (not %found(FNFGW01L)
100700040907     c                              or  FGWatb <> *blanks)
100800040907     C                   clear                   FNFGW000
100900040907e   3C                   endif
101000040907     C                   MOVEL     FGWFF3        W1FF3
101100040907     C                   MOVEL     FGWFF4        W1FF4
101200040907     C                   MOVEL     FGWFL3        W1FL3
101300040907     C                   MOVEL     FGWFL4        W1FL4
101400040907     c*
101500040907     c                   z-add     1             yy
101600040907     c                   movel     i50lna        w003a             3
101700040907     c     w003a         lookup    ffv(yy)                                39
101800040907    3c                   if        *in39
101900040907     c                   movel     flp(yy)       wpor
102000040907    4c                   if        wpor=i50tfa
102100040907     c                   movel     'A'           wtcr
102200090324     c                   movel     i50tfa        wter
102300040907     c                   else
102400040907     c                   movel     'T'           wtcr
102500090828     C** SE TRANSITO, la filiale dis carico è sempre un terminal per cui
102600090828     c*  non importa calcolarlo: prendo flp(yy)
102700090828     c                   movel     flp(yy)       wter
102800090828     c***                exsr      CallTER
102900040907    4c                   endif
103000040907    3c                   endif
103100040907    2c                   endif
103200040907    1c                   endif
103300990304     c
103400990129     c                   endsl
103500090325     c
103600090325     c* Se p.o. estero--> terminal = se stesso perchè la responsab
103700090325     c*  deve andare al distretto internazionale
103800090325     C     wpor          chain     azorg01l                           39
103900090325     c  n39              movel     orgde3        og143
104000090325     c   39              clear                   og143
104100090325     c*
104200090325     c                   if        §ogntw='EEX' or  §ogntW= 'DPD' or  §ogntw
104300090325     c                             = 'FED'
104400090325     c                   eval      wter=wpor
104500090325     c                   endif
104600090325     c
104700990304     c** CERCO nella skiera se l'ho gia' impostato
104800990216     c*  p.o. + segnacollo
104900990129     c                   z-add     1             yy
105000990217     c                   if        wpor=0
105100090324     c                   clear                   w013a
105200090324     c                   movel     '000'         w013a
105300090324     c                   move      '000'         w013a
105400990217     c                   else
105500090324     c                   movel     wokey         w013a
105600990217     c                   endif
105700990217     c
105800090324     c     w013a         lookup    ofil(yy)                               31
105900990129     c     *in31         ifeq      *off
106000990129     c                   z-add     1             yy
106100990216     c
106200090324     c                   clear                   w013a            13
106300090324     c     w013a         lookup    ofil(yy)                               31
106400990129     c                   endif
106500990129     c**
106600990216     c                   movel     wokey         ofil(yy)
106700990208     c                   movel     wtcr          otcr(yy)
106800990916     c
106900990916     c** Responsabilita' per ammanchi e mancanze o anche per avarie se
107000990916     c**  non e' impostato ilsuo codic eresponsabilita'
107100990916     c                   if        §tadragr<>'V' or §trstrev=*blanks
107200990304     c                   movel     §trstre       otre(yy)
107300990916     c                   else
107400990916     c                   movel     §trstrev      otre(yy)
107500990916     c                   endif
107600090303     c* Se colli di valore la moltiplico in base a tabella STD
107700090303     c* Verifico  se il p.o. responsabile è estero: in questo caso non triplico
107800090325     c*  nemmeno per avarie
107900090303     c                   eval      percen=respc(xx)
108000090303     c
108100090303     c                   if        §ar5bva<>' '
108200090303     c                   if        §ogntw<>'EEX' and §ogntW<>'DPD' and §ogntw
108300090325     c                             <>'FED'  and §tadragr<>'V'
108400090303     c                   eval      percen=respc(xx)*§stdrsval
108500090303     c                   endif
108600090303     c                   endif
108700090303     c
108800990129     c** calcolo la % di reponsabilita' sui colli passati per questo calcol
108900090303     c                   eval(h)   Wper=((percen   *wcollodan)/100)
109000990209     c                   add       wper          oper(YY)
109100990129     c
109200990129     c                   add       1             xx
109300070711    2c                   enddo
109400070711    1c                   endif
109500000321     c
109600000321     c                   clear                   flptfa
109700131018     c                   eval      wes_mem='S'
109800990129     c                   ENDSR
109900090324      *****************************************************************
110000090324      * Determino terminal p.o. responsabile
110100090324      *****************************************************************
110200090324     c     CallTER       BEGSR
110300090324     c                   clear                   dslv55
110400090831     c
110500090831     c* Se non ho impostato il tipo di wflp per transito imposto
110600090831     c*    sempre "A"
110700090831     c                   if        wtipoflp=' '
110800090831     c
110900090324     c                   if        wtcr='P'
111000090324     c                   movel     'P'           d55tpt
111100090324     c                   else
111200090324     c                   movel     'A'           d55tpt
111300090324     c                   endif
111400090831     c
111500090831     c                   else
111600090831     c                   movel     wtipoflp      d55tpt
111700090831     c                   endif
111800090831     c
111900090831     c                   if        d55tpt='A'
112000090831     c                   movel     i50lnp        d55lnp
112100090831     c                   endif
112200090324     c                   z-add     wpor          d55lin
112300090325     c                   if        blpdsp>0
112400090325     c                   z-add     blpdsp        d55drf
112500090325     c                   else
112600090325     c                   z-add     dctdca        d55drf
112700090325     c                   endif
112800090324     c                   call      'FNLV55R'
112900090324     c                   parm                    dslv55
113000090324     c* se errore calcolo terminal a oggi
113100090324     c                   if        d55err<>' '
113200090324     c                   z-add     dateu         d55drf
113300090324     c                   call      'FNLV55R'
113400090324     c                   parm                    dslv55
113500090324     c                   endif
113600090324     c
113700090831     c                   if        D55TPT='P'
113800090324     c                   z-add     d55tfp        wter
113900090324     c                   else
114000090324     c                   z-add     d55tfa        wter
114100090324     c                   endif
114200090324     c                   ENDSR
114300990304      *****************************************************************
114400990304      * Causali di calcolo per errore in apertura partenza
114500990304      *****************************************************************
114600990304     c     CLIAPERTURA   begsr
114700990305     c                   clear                   wclip
114800990305     c                   clear                   wclia
114900990305     c
115000100216     c* Divido i codici respons. tra ammanchi, avarie e mancanze per questo
115100100216     c*  tipo di responsabilità
115200090102    1c                   if        §tadragr='M'
115300090102     c                   eval      wke1='APERTAPARCLIM  '
115400090102   x1c                   else
115500090102     c
115600090102     c* Per avarie e ammanchi cambia dal 7/1/09
115700100216     c*  cambia ancora dividendo la responsabilità tra ammanchi e avarie
115800100216    2c***                if        lnpfl1='E'
115900100216     c***                eval      wke1='APERTAPARCLI   '
116000100216     C***                else
116100100216     c
116200100216    3c                   if        §tadragr='A'
116300100216     c                   eval      wke1='APERTAPARCLIA  '
116400100216     c                   else
116500100216     c                   eval      wke1='APERTAPARCLIV  '
116600100216    3c                   endif
116700100216    2c****               endif
116800100216    1c                   endif
116900990305     c
117000990305     c* Essendo in PARTENZA linea del cliente mittente, se non c'e'
117100990305     c*  linea partenza bolla
117200100216    3c                   if        wapert='P'
117300990305     c                   if        i50ksm<>0
117400990305     c                   movel     i50ksm        wclip
117500990305     c                   else
117600990305     c                   eval      wclip=i50lnp
117700990305     c                   endif
117800100216    3c                   endif
117900990305     c
118000990305     c* Essendo in ARRIVO   linea del cliente destinatario, se non c'e'
118100990305     c*  linea arrivo bolla
118200100216    3c                   if        wapert='A'
118300990305     c                   if        i50ksd<>0
118400990305     c                   movel     i50ksd        wclia
118500990305     c                   else
118600990305     c                   eval      wclia=i50lna
118700990305     c                   endif
118800990305     c                   endif
118900990304     c                   endsr
119000990304      *****************************************************************
119100990304      * Causali di calcolo per errore in apertura partenza
119200990304      *****************************************************************
119300990304     c     ERRAPERTURA   begsr
119400990304     c                   if        lnpfl1='E'
119500990308     c                   eval      wke1='APERTAPARERR_E '
119600990304     c                   else
119700990308     c                   eval      wke1='APERTAPARERR   '
119800990304     c                   endif
119900990304     c                   endsr
120000990129      *****************************************************************
120100990129      * Scrivo la responsabilita'
120200990129      *****************************************************************
120300990129     c     WRIRES        begsr
120400051010     c                   clear                   WNoscrivi
120500051010     c* se non ho più le spunte e mi servono per il calcolo della reponsab
120600051010     c*  non ricalcolo se c'e già (calcolo solo se è la prima volta)
120700051010     c
120800990209     c**
120900990209     c** cancello tutti i record presenti in fndcr
121000990216     c     kdct          chain     fndcr02l                           30
121100071018    1c     *in30         doweq     *off
121200990209     c     dcratb        ifeq      ' '
121300990209     c                   movel     'A'           dcratb
121400051010     c***                clear                   dcrftr
121500051010     c                   movel     dateu         dcrdtr
121600990209     c                   endif
121700051010     c* Solo se devo ricalcolare
121800051010     c                   if        WRicalcolo<>'N'
121900990209     c                   update    fndcr000
122000051010     c                   else
122100051010     c                   unlock    fndcr02l
122200051010     c                   movel     'N'           WNoscrivi         1
122300051010     c                   endif
122400990209     c**
122500990216     c     kdct          reade     fndcr02l                               30
122600071018    1c                   enddo
122700990209     c**
122800990129     c** scrivo i file e la ds di output
122900071018    1c                   if        WNoscrivi<>'N'
123000990129     c                   z-add     1             xx
123100071018    2c     ofil(xx)      downe     *blanks
123200990129     c     xx            ifeq      1
123300990129     c* DS
123400090324     c                   movel     ofil(xx)      o50por
123500990129     c                   z-add     oper(xx)      o50per
123600990129     c                   movel     otcr(xx)      o50tcr
123700990129     c                   endif
123800090324     c
123900090324     c                   movel     ofil(xx)      wokey
124000990129     c**
124100990216     c
124200990216     c     kdcr          chain     fndcr02l                           30
124300990129     c                   clear                   fndcr000
124400990129     c                   movel     dctaac        dcraac
124500990129     c                   movel     dctfil        dcrfil
124600990129     c                   movel     dctnca        dcrnca
124700990129     c                   z-add     oper(xx)      dcrper
124800990129     c                   movel     otcr(xx)      dcrtcr
124900090324     c                   movel     wpor          dcrpor
125000090324     c                   movel     wter          §dcrter
125100090324     c                   move      wsegnadan     §dcrnsc
125200090324     c                   movel     otre(xx)      §dcrtre
125300990216     c                   movel(p)  ddcr01        dcrflo
125400051010     c                   movel     dateu         dcrdtr
125500990129     c**
125600990129     c  n30              update    fndcr000
125700990129     c   30              write     fndcr000
125800990129     c**
125900990129     c                   add       1             xx
126000071018    2c                   enddo
126100071018   x1c                   else
126200071018     c
126300071018     c* Se i colli in DCR non corrispondono più a quella della C.A.
126400071018     c* tengo i colli predsenti in DCD e basta
126500071018    2c                   if        wncn<>wcollidcr
126600071018     c                   EXSR      AllineaDCRDCD
126700071018    2C                   ENDIF
126800071018     c
126900071018    1c                   endif
127000990129     c**
127100990129     c                   movel     'A'           dctcar
127200051010     c                   movel     'A'           o50car
127300990129     c                   update    fndct000
127400051010     c                   clear                   WRicalcolo
127500051010     c                   clear                   WNoscrivi
127600990129     c                   ENDSR
127700071018      *****************************************************************
127800071018      * Sistemo DCD allineato a DCR
127900071018      *****************************************************************
128000071018     c     AllineaDCRDCD BEGSR
128100071018     c
128200071018     c     kdct          chain     fndcr02l                           30
128300071018    3c     *in30         doweq     *off
128400071018    4c                   if        dcratb=' ' and dcrseg>*zeros
128500071018     c                   movel     dcrseg        w0070             7 0
128600071018     c     kdcr2         chain     fndcd01l
128700071018    5c                   if        not %found(fndcd01l) or dcdatb<>' '
128800071018     c                   movel     'A'           dcratb
128900071018     c                   movel     dateu         dcrdtr
129000071018     c                   update    fndcr000
129100071018    5c                   endif
129200071018    4c                   endif
129300071018     c
129400071018     c* Se non c'e' il segnacollo ed inserito solo 1 record,
129500071018     c*  sistemo altrimenti non ci riesco
129600071018    4c                   if        dcratb=' ' and dcrseg<=*zeros
129700071018     c                             and dcrper=wcollidcr
129800071018     c                   z-add     wncn          dcrper
129900071018     c                   movel     dateu         dcrdtr
130000071018     c                   update    fndcr000
130100071018    4c                   endif
130200071018     c
130300071018     c     kdct          reade     fndcr02l                               30
130400071018    3c                   enddo
130500071018     c                   ENDSR
130600990129      *****************************************************************
130700990129      * controllo se c'e' riserva in bolla in mancanza di dett CA o spunte
130800990129      *****************************************************************
130900990208     c     CT_RISERVA    begsr
131000990129     c                   clear                   wriserva          1
131100990129     c**
131200990204     c     dctcrc        ifeq      'S'
131300990211     c                   MOVEL(p)  'RISERVANODET'wke1
131400990129     c                   else
131500990129     C** 3) NON C'E' DETT.C.A. NON C'E'RISERVA --> non posso calcolare
131600990129     c*     NON C'E' SPUNTA    NON C'E'RISERVA --> non posso calcolare
131700990217     c                   MOVEL(p)  'NOCALCOLO'   wke1
131800051010     c                   movel     'N'           WRicalcolo        1
131900990129    2c                   endif
132000990217     c
132100990217     c                   exsr      MEMRES
132200990129     c                   ENDSR
132300990329      *****************************************************************
132400990329      * RESPONSABILITA' PER RITARDO APETURA C:A: MANCANZE
132500990329      *****************************************************************
132600990329     c     RITARDO       begsr
132700990329     c                   clear                   wsavdao
132800990329     c                   clear                   wsavcch
132900990329     c                   clear                   wsavdch
133000990329     c
133100990329     c* tutti i colli devono essere in ritardo, altrimenti va bene
133200990518     c     kdcd          chain     fnanm02l                           30
133300990329    1c     *in30         doweq     *off
133400990329     c                   if        anmatb=' '
133500990329     c     anmcaa        lookup    anidd                                  34
133600990329     c                   if        *in34 AND
133700990329     c                             anmdao>wsavdao
133800990329     c                   eval      wsavdao=anmdao
133900990329     c                   eval      wsavcch=anmcch
134000990329     c                   eval      wsavdch=anmdch
134100990329     c                   endif
134200990329     c                   endif
134300990329     c**
134400990518     c     kdcd          reade     fnanm02l                               30
134500990329    1c                   enddo
134600990329     c**
134700990329     c* l'anomalia idd trovata deve essere chiusa
134800990329    1c     wsavdch       ifgt      0
134900990329     c**
135000990514     c* anomalia chiusa con pratica idd --> vedo se aperta 2 giorni dopo
135100990329     c*   escluso i festivi
135200990329    2c     wsavcch       ifeq      'PR'
135300990329     c* cerco la fase 10 di apertura
135400990329     c     kdct          chain     fndcf01l                           31
135500990329    3c     *in31         doweq     *off
135600990329     c     dcffca        andne     10
135700990329     c     kdct          reade     fndcf01l                               31
135800990329    3c                   enddo
135900990329     c** trovata la fase 10 --> controllo
136000990518    3c  n31dcfdfc        ifgt      wsavdch
136100990329     c* controllo di quanti giorni
136200990518     c     *iso          move      wsavdch       wdataanm
136300990329     c     *iso          move      dcfdfc        wdatafase
136400990329     c                   clear                   wgiorni           2 0
136500990329     c                   adddur    1:*D          wdataanm
136600990329    4c     wdataanm      dowle     wdatafase
136700990329     c* controllo se e' festivo
136800990329     C     *iso          move      wdataanm      kdata
136900990329     C                   Z-ADD     dctlna        KTFA
137000150604     c* per lna estera o DPd uso i50tfa
137100150604     c                   if        lnafl1='E' or lnantw='DPD'
137200150604     C                   Z-ADD     i50tfa        KTFA
137300150604     c                   endif
137400990329     C                   clear                   KTFP
137500990329     C     KCLN          CHAIN     AZCLN01L                           34
137600990329    5C     *IN34         IFEQ      *OFF
137700990329     C     MAT(KGG)      andne     'F'
137800990329     C     POM(KGG)      andne     'F'
137900990329     C     *IN34         oreq      *ON
138000990329     C                   add       1             wgiorni
138100990329    5C                   ENDIF
138200990329     c**
138300990329     c                   adddur    1:*D          wdataanm
138400990329    4c                   enddo
138500990329     c**
138600120209     c* In base alla data spedizione tengo conto di 2/3 giorni o 7/8 giorni
138700120209   4ac                   if        blpdsp>0 and blpdsp>=20120130
138800120209     c                             and blpdsp<=20120214
138900120209     c
139000120209    4c     wgiorni       ifgt      7
139100120209     c     §ogeid        andeq     ' '
139200120209     c* Per chi fa l'IDD il giorno dopo c'e' un giorno in piu' per aprire
139300120209     c     wgiorni       orgt      8
139400120209     c     §ogeid        andeq     'S'
139500120209     c                   eval      writ='R'
139600120209     c                   else
139700120209     c                   eval      writ='N'
139800120209    4c                   endif
139900120209     c
140000120209  x4ac                   else
140100120209     c
140200990514    4c     wgiorni       ifgt      2
140300990329     c     §ogeid        andeq     ' '
140400990329     c* Per chi fa l'IDD il giorno dopo c'e' un giorno in piu' per aprire
140500990514     c     wgiorni       orgt      3
140600990329     c     §ogeid        andeq     'S'
140700990329     c                   eval      writ='R'
140800990329     c                   else
140900990329     c                   eval      writ='N'
141000120209    4c                   endif
141100990329     c*
141200120209   4ac                   endif
141300120209     c
141400120209    3c                   endif
141500990329    2c                   endif
141600990329    1c                   endif
141700990329     c
141800990329     c                   endsr
141900990129      *****************************************************************
142000990129      * RESPONSABILITA' PER MANCANZE
142100990129      *****************************************************************
142200990129     c     MANCANZE      begsr
142300990305     c                   clear                   wsavdao
142400990305     c                   clear                   wsavcch
142500990305     c                   clear                   wsavdch
142600000321     c                   clear                   flptfa
142700000321     c                   clear                   writardo          1
142800090512     c                   clear                   wnfv
142900090828     c                   clear                   wter
143000090828     c                   clear                   wtipoflp          1
143100990305     c
143200990205     c* In questa routine il collo danneggiato da controllare e' sempre "1"
143300990129     C* controllo l'anomalia IDD3 se c'e'
143400990129     c*  data immissione C.A. --> responsabilita' arrivo
143500990518     c     kdcd          chain     fnanm02l                           30
143600990129    1c     *in30         doweq     *off
143700990305     c                   if        anmatb=' '
143800990305     c     anmcaa        lookup    anidd                                  34
143900990305     c                   if        *in34 AND
144000990305     c                             anmdao>wsavdao
144100990305     c                   eval      wsavdao=anmdao
144200990305     c                   eval      wsavcch=anmcch
144300990305     c                   eval      wsavdch=anmdch
144400990305     c                   endif
144500990305     c                   endif
144600990129     c**
144700990518     c     kdcd          reade     fnanm02l                               30
144800990129    1c                   enddo
144900990129     c**
145000990129     c* l'anomalia idd trovata deve essere chiusa
145100990329    1c     wsavdch       ifgt      0
145200990129     c* anomalia chiusa annullata --> colpa della partenza
145300990305    2c     wsavcch       ifeq      'AN'
145400990211     c                   MOVEL(p)  'IDDANN'      wke1
145500990129     c                   exsr      MEMRES
145600990208     c                   goto      endmanca
145700990129   x2c                   else
145800990129     c**
145900990129     c* anomalia chiusa con pratica idd --> vedo se aperta il giorno dopo
146000990129     c*   escluso i festivi
146100990331     c* All'export non imputo l'apertura in ritardo perche' non puo' essere
146200990331     c*  colpa del partner
146300990305    3c     wsavcch       ifeq      'PR'
146400990331     c     writ          andeq     'R'
146500990331     c     lnafl1        andne     'E'
146600000321     c* Per ora emorizzo solo che e' c.a. aerta in ritardo poi vedo
146700000321     c                   eval      writardo='S'
146800990129     c*
146900990129    3c                   endif
147000990129    2c                   endif
147100990129    1c                   endif
147200990129     c**
147300990208     c**  SPUNTE: vedo se ci sono
147400990205     c                   movel     'S'           wesistspu         1
147500070131     c**   kdcd          setll     fnbrv07l                               34
147600061020     c**n34              movel     'N'           wesistspu
147700061020     c* Dico che non esistono spunte se non trovate o se trovate ma
147800061020     c*  flaggate (significa che abbiamo tenuto solo le spunte con BRVCAN
147900061020     c*  impostato)
148000070131     c     kdcd          chain     fnbrv07l
148100070131     c                   if        not %found(fnbrv07l) or brvnvr<>' '
148200061020     c                   movel     'N'           wesistspu         1
148300061020     c                   endif
148400990204     c**
148500990205     c** PARTO COMUNQUE DAL DETT.COLLI POI IN CASO DI NECESSITA'
148600990205     c**  VALUTO LE SPUNTE
148700990205     c**
148800990208     c** TERMINAL DI PARTENZA di artflp
148900990208     c                   clear                   wflp
149000990205     c     artflp        ifgt      0
149100990208     c                   movel     'P'           d55tpt
149200990205     c                   z-add     artflp        d55lin
149300990205     c                   z-add     artlnp        d55lnp
149400990205     c                   select
149500990205     c     artdut        whengt    0
149600990205     c                   z-add     artdut        d55drf
149700990205     c     artdet        whengt    0
149800990205     c                   z-add     artdet        d55drf
149900990205     c     artdfv        whengt    0
150000990208     c                   z-add     artdfv        d55drf
150100990205     c                   other
150200990205     c                   z-add     dctdca        d55drf
150300990205     c                   endsl
150400990209     c                   call      'FNLV55R'
150500990205     c                   parm                    dslv55
150600990205     c*
150700990205     c                   z-add     d55tfp        wflp
150800090828     c                   movel     'P'           wtipoflp
150900000321     c*
151000000321     c** TERMINAL DI ARRIVO tra artflp e artlna
151100000321     c                   movel     'A'           d55tpt
151200000321     c                   z-add     artflp        d55lnp
151300000321     c                   z-add     artlna        d55lin
151400000321     c                   clear                   d55tfa
151500000321     c                   call      'FNLV55R'
151600000321     c                   parm                    dslv55
151700000321     c*
151800000321     c                   z-add     d55tfa        flptfa
151900990208     c                   endif
152000990308     c
152100990308     c* Se la data di arrivo pistola 85 > data apertura c.a. non la conseder
152200990308     c     artdam        ifgt      0
152300990308     c     artnap        andeq     85
152400990401     c     artdam        andge     dctdca
152500990308     c                   clear                   artdam
152600990308     c                   clear                   artnap
152700990308     c                   endif
152800990310     c
152900990310     c* Per EXPORT  ignoro sempre la pistola 85 --> errore bartolini di aper
153000990310     c     artdam        ifgt      0
153100990310     c     artnap        andeq     85
153200990310     c     lnafl1        andeq     'E'
153300990310     c                   clear                   artdam
153400990310     c                   clear                   artnap
153500990310     c                   endif
153600990205     c***
153700990205     c* DATA ARRIVO : se c'e' il collo e' arrivato
153800990208    1c     artdam        ifgt      0
153900990210     c                   clear                   wfgs
154000990205     C* verifico se dopo l'arrivo e' stato spuntato da altri P.O.
154100990208    2c     wesistspu     ifeq      'S'
154200990205     c                   z-add     artdam        wdatrifer
154300990205     c                   exsr      SPUARR
154400051010     c                   else
154500051010     c                   movel     'N'           WRicalcolo
154600990208    2c                   endif
154700990205     c***
154800990210     c* Se WFGS    >0 e non e' lna della CA cerco suo terminal di arrivo
154900990210    2c     wfgs          ifgt      0
155000990308     c
155100990311     c* Se bolla export e c'e' spunta defluenza con data>=data arrivo     nr
155200990311     c*  --> colpa partner
155300990311     c                   if        lnafl1='E'  AND
155400990311     c                             wEXPdef='S' AND
155500990311     c                             WEXPDFV>=artdam
155600990311     c
155700990310     c                   MOVEL(p)  'SPUNTADEFL_E'wke1
155800990308     c                   exsr      MEMRES
155900990308     c                   goto      endmanca
156000990308     c                   endif
156100990329
156200990329     c* Se fgs e' terminal arrivo bolla --> colpa sua
156300990329     c                   select
156400990329    3c     wfgs          wheneq    i50lna
156500990329     c                   MOVEL(p)  'ARRIVOLNA'   wke1
156600000321     c* Vedo se considerare il ritardo
156700000321     c                   exsr      AQUALE
156800990329     c                   goto      endmanca
156900990305     c* Se fgs e' terminal arrivo bolla --> colpa sua
157000990329    3c     wfgs          wheneq    i50tfa
157100000321     c* clearo terminal tenendo conto del p.o. di transito perche' non
157200000321     c*  mi serve
157300000321     c                   clear                   flptfa
157400990308     c
157500990305     c                   MOVEL(p)  'ARRIVOSI'    wke1
157600000321     c* Vedo se considerare il ritardo
157700000321     c                   exsr      AQUALE
157800990305     c                   goto      endmanca
157900990329   x3c                   other
158000990305     c
158100990210     c                   EXSR      terspunta
158200990311     c
158300990305     c*** Se si tratta comunque di p.o. dell'area di arrivo-->
158400990329     c***  imputo al p.o.
158500990205     c***  altrimenti e' DISGUIDO
158600990305    4c     wflp          ifeq      i50tfa
158700990305     c                   MOVEL(p)  'ARRIVOAREA'  wke1
158800990329     c                   eval      wflp=wfgs
158900090831     c                   movel     'A'           wtipoflp
159000000321     c* Vedo se considerare il ritardo
159100000321     c                   exsr      AQUALE
159200990208     c                   goto      endmanca
159300990305   x4c                   else
159400990211     c                   MOVEL(p)  'DISGUIDO'    wke1
159500000321     c* Vedo se considerare il ritardo
159600000321     c                   exsr      AQUALE
159700990208     c                   goto      endmanca
159800990305    4c                   endif
159900990329    3c                   endsl
160000990205     c**
160100040907     c* non trovate altre spunte
160200990208   x2c                   else
160300990305     c                   MOVEL(p)  'ARRAREA_'    wke1
160400990305     c                   MOVE      'NOSPU  '     wke1
160500000321     c* Vedo se considerare il ritardo
160600000321     c                   exsr      AQUALE
160700990208     c                   goto      endmanca
160800990208    2c                   endif
160900990208    1c                   endif
161000990205     c**
161100990205     c* DATA USCITA TRANSITO: se c'e' con pistola reale e' uscito dal trans
161200990208    1c     artdut        ifgt      0
161300990208     c                   movel     artput        w002a             2
161400990208     c     w002a         lookup    nps                                    34
161500990205     c** 34 off - pistola reale
161600990208    2c     *in34         ifeq      *off
161700990211     c                   MOVEL(p)  'TRADUT_SINO' wke1
161800000321     c* Vedo se considerare il ritardo
161900000321     c                   exsr      AQUALE
162000990208     c                   goto      endmanca
162100990205     c**
162200990208   x2c                   else
162300990205     c** Se si tratta di pistola completamente fasulla, senza altra
162400990205     c**  spunta, cerco nelle spunte un eventuale altro disguido
162500990205     c**  altrimenti e' colpa del transito
162600990205     c**
162700990208     c                   movel     artput        w002a             2
162800990208     c     w002a         lookup    npsno                                  31
162900990205     c** SPUNTA FASULLA DA SPUNTA REALE --> colpa del transito
163000990208    3c     *in31         ifeq      *off
163100990211     c                   MOVEL(p)  'TRADET_'     wke1
163200990205     c                   MOVE      'SINONO  '    wke1
163300000321     c* Vedo se considerare il ritardo
163400000321     c                   exsr      AQUALE
163500990208     c                   goto      endmanca
163600990208   x3c                   else
163700990205     c** Cerco un'altra spunta reale
163800990210     c                   clear                   wfgs
163900990208    4c     wesistspu     ifeq      'S'
164000990205     c                   clear                   wdatrifer
164100990210     c                   clear                   wfgs
164200990205     c                   exsr      SPUARR
164300990205     c**
164400990210     c* Se WFGS    >0 e non e' lnp della CA cerco suo terminal di arrivo
164500990210    5c     wfgs          ifgt      0
164600990210     c     wfgs          andne     dctlnp
164700990210     c     wfgs          andne     i50tfp
164800990210     c                   EXSR      TERSPUNTA
164900990205     c**
165000020312     C** SE SI TRATTA DI SPUNTA PARTENZA, LA CONSIDERO COME UN SI/NO
165100020312     C     wnpg          ifeq      1
165200020312     c                   MOVEL(p)  'TRADUT_SINO' wke1
165300020312     c                   else
165400990211     c                   MOVEL(p)  'DISGUIDO'    wke1
165500020312     c                   endif
165600000321     c* Vedo se considerare il ritardo
165700000321     c                   exsr      AQUALE
165800990208     c                   goto      endmanca
165900990205     c**
166000990208    5c                   endif
166100051010     c                   else
166200051010     c                   movel     'N'           WRicalcolo
166300990208    4c                   endif
166400990205     c**
166500990208    3c                   endif
166600990208    2c                   endif
166700990208    1c                   endif
166800990205     c**
166900990208     c**
167000990208     c* DATA ENTRATA TRANSITO: controllo se non c'e' data uscita o fasulla
167100990208     c*                        senza altra spunte o solo della lnp
167200990208    1c     artdet        ifgt      0
167300990208     c                   movel     artpet        w002a             2
167400990208     c     w002a         lookup    nps                                    34
167500060301     c* Pistola reale
167600060301    2c     *in34         ifeq      *off
167700990211     c                   MOVEL(p)  'TRADET_'     wke1
167800990205     c                   MOVE      'SINONO  '    wke1
167900000321     c* Vedo se considerare il ritardo
168000000321     c                   exsr      AQUALE
168100990208     c                   goto      endmanca
168200060301     c
168300060301   x2c                   else
168400060301     c
168500060301     c** Cerco un'altra spunta reale
168600060301     c                   clear                   wfgs
168700060301    4c     wesistspu     ifeq      'S'
168800060301     c                   clear                   wdatrifer
168900060301     c                   clear                   wfgs
169000060301     c                   exsr      SPUARR
169100060301     c**
169200060301     c* Se WFGS    >0 e non e' lnp della CA cerco suo terminal di arrivo
169300060301    5c     wfgs          ifgt      0
169400060301     c     wfgs          andne     dctlnp
169500060301     c     wfgs          andne     i50tfp
169600060301     c                   EXSR      TERSPUNTA
169700060301     c**
169800060301     C** SE SI TRATTA DI SPUNTA PARTENZA, LA CONSIDERO COME UN SI/NO
169900060301     C     wnpg          ifeq      1
170000060301     c                   MOVEL(p)  'TRADUT_SINO' wke1
170100060301     c                   else
170200060301     c                   MOVEL(p)  'DISGUIDO'    wke1
170300060301     c                   endif
170400060301     c* Vedo se considerare il ritardo
170500060301     c                   exsr      AQUALE
170600060301     c                   goto      endmanca
170700060301     c**
170800060301    5c                   endif
170900060301    4c                   endif
171000060301    2c                   endif
171100990208    1c                   endif
171200990205     c**
171300990205     c** DATA PARTENZA
171400990208    1c     artdfv        ifgt      0
171500060301     c
171600060301     c* Verifico quale è l'ultima spunta effettuata in partenza
171700060301    2c     wesistspu     ifeq      'S'
171800060309     c* Mi salvo la spunta partenza
171900060309     c                   movel     wfgs          fgspar
172000060309     c                   movel     wnfv          nfvpar
172100060309     c
172200060301     c                   z-add     artdfv        wdatrifer
172300060301     c                   eval      wautogen='N'
172400060301     c                   exsr      SPUARR
172500060301     c                   eval      wautogen=' '
172600060301     c
172700060301     c* Se l'ultima spunta non autogenerata è non categoria partenza-->
172800060301     c* colpa del p.o.
172900060301     c                   if        wnpg<>0
173000060301     c                   if        (wnpg<>1 and  wnpg<>3 and wnpg<>5)
173100090511     c                             or (wnpg=3 and wspg<>'P')  or
173200090511     c* oppure se la data della spunta > data partenza collo (solo IMP)
173300090511     c                             wdfv>artdfv
173400060301     c                   MOVEL(p)  'SPUNOPART'   wke1
173500060301     c                   eval      wflp=wfgs
173600090831     c                   movel     'P'           wtipoflp
173700060301     c                   exsr      AQUALE
173800060301     c                   goto      endmanca
173900060301     c                   endif
174000060301     c                   endif
174100060301     c                   endif
174200060301     c
174300990208     c                   movel     artnpp        w002a             2
174400990208     c     w002a         lookup    nps                                    34
174500990205     c*  partenza effettiva
174600990208    2c     *in34         ifeq      *off
174700060301     c
174800060301     c* Se è categoria partenza ma è di un secondo livello-->
174900060301     c* colpa del terminal
175000060301     c                   if        wesistspu='S' and wfgs<>0
175100060301     c                   clear                   dslv55
175200060301     c                   movel     'P'           d55tpt
175300060301     c                   z-add     wfgs          d55lin
175400060301     c                   z-add     wdfv          d55drf
175500060301     c                   call      'FNLV55R'
175600060301     c                   parm                    dslv55
175700060301     c                   if        d55tfp<>wfgs
175800060301     c                   MOVEL(p)  'SPUNOTER'    wke1
175900060301     c                   eval      wflp=wfgs
176000090831     c                   movel     'P'           wtipoflp
176100060301     c                   exsr      AQUALE
176200060301     c                   goto      endmanca
176300060301     c                   endif
176400060301     c                   endif
176500060309     c
176600060309     c* Reimosto il foglio partenza che mi serve per il calcolo della respo
176700060309     c                   eval                    wfgs= fgspar
176800060309     c                   eval                    wnfv= nfvpar
176900990205     c*
177000990205     c* se NON C'E' TRANSITO e' colpa della partenza
177100990208    3c     artflp        ifeq      0
177200990211     c                   MOVEL(p)  'PART_SINO'   wke1
177300000321     c* Vedo se considerare il ritardo
177400000321     c                   exsr      AQUALE
177500990208     c                   goto      endmanca
177600990205     c                   else
177700990205     c*        C'E' TRANSITO: e' colpa del transito
177800990211     c                   MOVEL(p)  'PART_'       wke1
177900990205     c                   MOVE      'SINONONO  '  wke1
178000000321     c* Vedo se considerare il ritardo
178100000321     c                   exsr      AQUALE
178200990208     c                   goto      endmanca
178300990208    3c                   endif
178400990205     c***
178500990208   x2c                   else
178600990208     c                   movel     'AUT'         wnppnps           3
178700990208    2c                   endif
178800990208   x1c                   else
178900990205     c                   movel     'AUT'         wnppnps
179000990208    1c                   endif
179100990205     c**
179200990205     c** partenza non effettiva  o = "0"
179300990208    1c     wnppnps       ifeq      'AUT'
179400990303     c
179500990303     c* Causale diversa per punta partenza fasulla
179600990303     c                   movel     artnpp        w002a             2
179700000321     c     w002a         lookup    npsno                                  35
179800990303     c
179900990208    2c     artflp        ifeq      0
180000990303     c
180100000321     c* pistola 88 /89 /90
180200990304    3c                   if        *in35
180300990304    4c                   if        lnpfl1='E'
180400990304     c                   MOVEL(p)  'PART_NONO_E' wke1
180500990304     c                   else
180600990304     c                   MOVEL(p)  'PART_NONO'   wke1
180700990304    4c                   endif
180800000321     c                   z-add     50            wresarr           3 0
180900990304     c
181000990304   x3c                   else
181100000321     c* pistola 98 /96
181200990304     c                   MOVEL(p)  'PART_NONO9X' wke1
181300990304    3c                   endif
181400990304     c
181500000321     c                   exsr      AQUALE
181600990208     c                   goto      endmanca
181700990303     c
181800990304   x2c                   else
181900990205     c*        C'E' TRANSITO: e' colpa del transito
182000990211     c                   MOVEL(P)  'PART_'       wke1
182100990304    3c                   if        *in35
182200990311    4c                   if        lnpfl1='E'
182300990304     c                   MOVE      'NONONONO_E'  wke1
182400990304     c                   else
182500990304     c                   MOVE      'NONONONO  '  wke1
182600990304    4c                   endif
182700000321     c                   z-add     50            wresarr           3 0
182800990304     c
182900990304   x3c                   else
183000990304     c                   MOVE      'NONONONO9X'  wke1
183100990304    3c                   endif
183200990304     c
183300000321     c                   exsr      AQUALE
183400990208     c                   goto      endmanca
183500990208    2c                   endif
183600990208    1c                   endif
183700990205     c***
183800990208     c     endmanca      endsr
183900000321      *****************************************************************
184000000321      * Vedo se dare la colpa prevista oper il ritardo
184100000321      *****************************************************************
184200000321     c     AQUALE        BEGSR
184300000321     c* Il ritardo non e' mai previsto a carico dell'estero
184400000321     c                   if        writardo='S'
184500000321     c* ritardo tutta colpa dell'arrivo
184600000321     c                   if        wresarr=0
184700000321     c                   MOVEL(p)  'IDDRITARDO'  wke1
184800000321     c                   endif
184900000321     c* ritardo colpa al 50%
185000000321     c                   if        wresarr=50
185100000321     c                   MOVEL(p)  'IDDRITARDO50'wke1
185200000321     c                   endif
185300000321     c
185400000321     c                   endif
185500000321     c
185600000321     c                   exsr      MEMRES
185700000321     c                   clear                   wresarr
185800000321     c                   ENDSR
185900990210      *****************************************************************
186000990210      * RESPONSABILITA' PER MANCANZE
186100990210      *****************************************************************
186200990210     c     TERSPUNTA     BEGSR
186300990210     c* per categoria "1" --> tengo fgs altrimenti cerco terminal arrivo
186400990210    1c     wnpg          ifeq      1
186500990210     c                   z-add     wfgs          wflp
186600090831     c                   movel     'P'           wtipoflp
186700990210     c                   else
186800990210     c                   clear                   dslv55
186900990210     c* Cerco il terminal di partenza per categoria partenza
187000990210     c                   if        wnpg=5   OR
187100990210     c                             wnpg=3 and wspg='P'
187200990210     c                   movel     'P'           d55tpt
187300990210     c                   else
187400990210     c                   movel     'A'           d55tpt
187500990210     c                   endif
187600990210     c                   movel     wfgs          d55lin
187700990210     c                   z-add     artlnp        d55lnp
187800990210     c                   z-add     wdfv          d55drf
187900990210     c                   call      'FNLV55R'
188000990210     c                   parm                    dslv55
188100990210     c**
188200990920     c                   if        d55tpt='P'
188300990920     c                   z-add     d55tfp        wflp
188400090831     c                   movel     'P'           wtipoflp
188500990920     c                   else
188600990210     c                   z-add     d55tfa        wflp
188700090831     c                   movel     'A'           wtipoflp
188800990210    1c                   endif
188900990920    1c                   endif
189000990210     c                   ENDSR
189100990129      *****************************************************************
189200990129      * RESPONSABILITA' PER AMMANCHI E AVARIE
189300990129      *****************************************************************
189400990209     c     AMMAVARIE     BEGSR
189500000321     c                   clear                   flptfa
189600090421     c                   clear                   SoloCAN
189700090828     c                   clear                   wter
189800090828     c                   clear                   wtipoflp          1
189900090828     c
190000990129     c* se il collo non ha spunte non posso calcolare se non c'e' riserva
190100070131     c**   kdcd          setll     fnbrv07l                               34
190200070131     c     kdcd          chain     fnbrv07l
190300090421     c*****              if        not %found(fnbrv07l) or brvnvr<>' '
190400110923    1c                   if        not %found(fnbrv07l)
190500061020     c                   setoff                                       34
190600110923   x1c                   else
190700090421     c* Presenti solo spunte con l'anomalia
190800110923    2c                   if        brvnvr<>' '
190900090421     c                   eval      soloCAN='S'
191000110923     c                   endif
191100110923     c
191200061020     c                   seton                                        34
191300110923    2c                   endif
191400061020     c
191500990209    1c     *in34         ifeq      *off
191600990208     c* controllo se c'e' riserva
191700990208     c                   exsr      CT_RISERVA
191800990208     c                   goto      endammava
191900990209   x1c                   else
192000990304     c
192100990209     c** Carico per ogni fgs la spunta con data piu' bassa che ha l'anomalia
192200990209     c                   EXSR      LETBRV
192300990304     c
192400990209     c** Se non c'e' nessuna spunta :
192500990210    2c     wnpg          ifeq      0
192600990209     c* se non c'e' nemmeno sul dett.colli --> NO_CAN
192700990331     c* se     c'e'         sul dett.colli --> A_CAN_NOSPU
192800050615     c* se artcan=ad una delle anomalie "fasulle" come se non ci fosse
192900050615     c     artcan        lookup    Noanomspu                              31
193000990209    3c     artcan        ifeq      ' '
193100050615     c     *in31         oreq      *on
193200990304     c                   movel(P)  'A_NO_CAN'    wke1
193300990209     c                   else
193400990331     c                   movel(P)  'A_CAN_NOSPU' wke1
193500990209    3c                   endif
193600990209     c
193700990209     c                   exsr      MEMRES
193800990209     c                   goto      endammava
193900990209    2c                   endif
194000990209     c**
194100090102     c** Se c'e' la spunta categoria part in partenza :colpa PART_SI
194200990209     c**  escludo le spunte di defluenza
194300081204    2c     wfgs          ifeq      i50tfp
194400090102    2c     wfgs          oreq      i50lnp
194500081204    3c     wnpg          ifeq      1
194600990210     c     wdef          andeq     ' '
194700080714     c     wnpg          oreq      5
194800080903     c* se si tratta di una spedizione import DPD, la colpa viene data
194900080903     c*  al partner
195000081204    4c                   if        lnpntw='DPD'
195100080903     c                   MOVEL(p)  'COLPAPART_E' wke1
195200080903     c                   else
195300990304     c                   movel(P)  'A_PART_SI'   wke1
195400081204    4c                   endif
195500990209     c                   exsr      MEMRES
195600990209     c                   goto      endammava
195700081204    3c                   endif
195800080714    2c                   endif
195900990210     c
196000090422     c** controllo categoria della 1°spunta con anomalia
196100990210     c                   movel     wnpg          w001a
196200990210     c     w001a         lookup    catarr                                 34
196300990210    2c   34wnpg          ifeq      1
196400990210     C     wdef          andeq     ' '
196500990210     c                   setoff                                       34
196600990210    2c                   endif
196700080513     c
196800080513     c                   if        wnpg=5
196900080513     c                   seton                                        34
197000080513     c                   endif
197100990210     c
197200990212     c** NON E' Categoria  ARRIVI  o ANOMALIA INSERITA ERRONEMANETE
197300990216     c**                 __> colpa sua
197400990212    2c                   if        not *in34   OR
197500990212     c                             werr = 'E'
197600990211     c                   movel     'S'           wsianom           1
197700990210     c                   EXSR      RESPOAAV
197800990210     c                   goto      endammava
197900990210     c
198000990210   x2c                   else
198100090423     c
198200090423     c                   eval      wNPGspuan=wnpg
198300090423     c                   eval      wflpspuan=wflp
198400090423     c                   eval      wfgsspuan=wfgs
198500990305     c
198600080903     c**     E' Categoria  ARRIVI/ENTRATA:
198700990210     c**     se spara un secondo livello --> colpa del terminal arrivo
198800990210     c**     se spara un primo   livello --> colpa del suo precedente
198900990210     c** Il suo precedente e' quello che ha la data < o a parita' di data
199000990210     c*   data/ora carcamento <
199100990211    3c                   if        wfgs<>i50tfa
199200990210     c                   exsr      terspunta
199300990210     c
199400990211    4c                   if        wflp<>wfgs
199500990921     c                   eval      zz=1
199600990921     c     wfgs          lookup    waavfgs(zz)                            34
199700990921     c                   movel     wnpg          w001a
199800990921     c* se non e' la 1 spunta del 2 livello --> e' colpa del 2 livello
199900090831     c*  per me qui c'e' un errore: se  il secondo livello non è la
200000070712     c*  linea di arrivo ma un altro secondo livello
200100080404     c*  la colpa finisce alla linea di arrivo lo stesso
200200080404     c*  4/4/08--> imposto WFLP col p.o. che ha la colpa  cambio il
200300080404     c*            flag tipo rsponsabilità al codice A_2LIV_TARD in "T"
200400081204    5c     *in34         ifeq      *off
200500990921     c     waavsca(zz)   orne      wscar
200600990921     c     waavfgs(zz)   orne      wfgs
200700990921     c     waavnpg(zz)   orne      w001a
200800990921     c     waavdfv(zz)   orne      wdfv
200900080404     c                   eval      wflp=wfgs
201000090831     c                   movel     'A'           wtipoflp
201100990921     c                   eval      wke1='A_2LIV_TARD    '
201200990921     c                   exsr      MEMRES
201300990921     c                   goto      endammava
201400081204   x5c                   else
201500990921     c
201600070711     c** Altrimenti e' colpa del terminal
201700070711     c*  prima di dare la colpa al terminal verifico se ho fatto "il furbo"
201800070711     c*  in fnbrve
201900070713     c**** kbrve         chain     fnbrve1l
202000070713     c****               if        %found(fnbrve1l) and E_brvcan=' '
202100070713     c****               eval      wke1='DA_CREARE      '
202200070713     c****               exsr      MEMRES
202300070711     c* se trovato, esco dal pgm ho attribuito la responsabilità
202400070713     c****               if        o50err=' '
202500070713     c****               goto      endammava
202600070713     c****               else
202700070711     c* Se non trovato record valido, procedo come ora
202800070713     c****               clear                   o50err
202900070713     c****               clear                   o50msg
203000070713     c****               endif
203100070713     c****               endif
203200070711     c
203300070711     c
203400081211     c                   eval      wfgsspuan=wfgs
203500090831     c                   movel     'A'           wtipoflp
203600081211
203700081204    6c                   if        wflp=i50tfa
203800990304     c                   eval      wke1='A_ARR_NOSPUTER '
203900990210     c                   exsr      MEMRES
204000990210     c                   goto      endammava
204100990210     c                   else
204200990304     c                   eval      wke1='A_TRA_NOSPUTER '
204300990210     c                   exsr      MEMRES
204400990210     c                   goto      endammava
204500081204    6c                   endif
204600081204    5c                   endif
204700081204    4c                   endif
204800990921    3c                   endif
204900990210     c**
205000090421     c*
205100990210     c                   clear                   WPREC200
205200990210     c                   clear                   WPRECNPG
205300990210     c                   clear                   WPRECFGS
205400990210     c                   clear                   WPRECSPG
205500990305     c
205600990305     c                   exsr      terspunta
205700070821     c                   eval      wNPGspuan=wnpg
205800070821     c                   eval      wflpspuan=wflp
205900990309     c                   eval      wfgsspuan=wfgs
206000090421     c
206100090421     c* Se ci sono solo le spunte con anomalie, le considerzioni del
206200090421     c*  precedente le devo fare sulla bolla e non sulle spunte
206300090421   3ac                   if        SoloCAN='S'
206400090421     c                   exsr      DaBolla
206500090421     c
206600090421     c
206700090421  x3ac                   else
206800090421     c* Cerco il precedente dalle spunte
206900090421     c                   movel     wdfv          w0200
207000090421     c                   move      wscar         w0200            20 0
207100990305     c
207200990210     c                   eval      zz=1
207300990210    3c     waavdfv(zz)   dowgt     0
207400990210     c                   movel     waavdfv(zz)   waav200          20 0
207500990210     c                   move      waavsca(zz)   waav200          20 0
207600990210     c*
207700990211     c* Escludo se waav20 e fgs sono il p.o. che ha sparato l'anomalia
207800990211     c                   movel     wnpg          w001a
207900990211     c     waavsca(zz)   ifne      wscar
208000990211     c     waavfgs(zz)   orne      wfgs
208100990211     c     waavnpg(zz)   orne      w001a
208200990211     c     waavdfv(zz)   orne      wdfv
208300990211     c
208400990210    4c     waav200       ifle      w0200
208500990210     c     waav200       andgt     wprec200
208600990210     C                   MOVEL     Waav200       wprec200
208700990210     c                   movel     waavnpg(zz)   wprecnpg
208800990210     c                   movel     waavfgs(zz)   wprecfgs
208900990210     c                   movel     waavspg(zz)   wprecspg
209000990210    4c                   endif
209100990211    4c                   endif
209200990210     c                   add       1             zz
209300990210    3c                   enddo
209400081204     c
209500990210     c** Se non ho trovato il precedente --> e' colpa della partenza
209600990210    3c     wprecnpg      ifeq      0
209700990308     c                   movel(P)  'A_PART_NOSI' wke1
209800990210     c                   exsr      MEMRES
209900990210     c                   goto      endammava
210000990210   x3c                   else
210100990211     c**  altrimenti cerco il suo terminal e attribuisco
210200990210     c**  Imposto i campi della spunta a cui devo dare la responsabilita'
210300990210     c                   eval      wnpg=wprecnpg
210400990210     c                   eval      wfgs=wprecfgs
210500990210     c                   eval      wspg=wprecspg
210600990210     c                   movel     wprec200      wdfv
210700990210     c                   move      wprec200      wscar
210800990210     c**
210900990211     c                   movel     'N'           wsianom
211000990210     c                   EXSR      RESPOAAV
211100990210     c                   goto      endammava
211200090421   3ac                   endif
211300090421    3c                   endif
211400990210    2c                   endif
211500990210     c
211600990209    1c                   endif
211700990129     c
211800990208     c     endammava     endsr
211900090421      *****************************************************************
212000090421      * ATTRIBUZIONE RESPONSABILITÀ AMMANCHI E AVARIE DDA BOLLA
212100090422      *  IN PRESENZA DELLE SOLE SPUNTE CON ANOMALIA
212200090421      *****************************************************************
212300090421     c     DaBolla       BEGSR
212400090421     c                   clear                   wscar
212500090421     c
212600090421     c* Se chi ha messo l'anomalia è la partenza --> colpa della partenza
212700090422     c* (l'ha già fatto sopra...però controllo solo le categorie 1 5 e IMP
212800090422     c*  qui daccio rientrare anche le altre categorie...)
212900090421     c                   select
213000090421    1c                   when      wfgsspuan=i50tfp or  wfgsspuan=i50lnp or
213100090421     c                             wflpspuan=i50tfp
213200090421     c                   movel(P)  'A_PART_NOSI' wke1
213300090421     c                   exsr      MEMRES
213400090421     c
213500090421    1c                   when      wfgsspuan=i50tfa
213600090421     c                   clear                   wfgs
213700090421     c* c'e' transito
213800090421    2c                   if        artflp>0
213900090421     c* Se c'e' la data uscita transito, imposto come categoria la 1-partenza
214000090421    3c                   if        artdut>0
214100090421     c                   eval      wfgs=artflp
214200090422     c                   eval      wflp=artflp
214300090421     c                   eval      wdfv=artdut
214400090421     c                   eval      wnpg=1
214500090421     c                   eval      wspg=' '
214600090421   x3c                   else
214700090421    4c                   if        artdet>0
214800090421     c                   eval      wfgs=artflp
214900090421     c                   eval      wdfv=artdet
215000090421     c                   eval      wnpg=2
215100090421     c                   eval      wspg=' '
215200090421     c                   exsr      TERSPUNTA
215300090421    4c                   endif
215400090421    3c                   endif
215500090422     c* Se il p.o. di transito è un terminal--> colpa sua
215600090422     c* se è un secondo livello --> colpa del suo terminal
215700090422     c                   eval      wke1='A_TRA_NOSI     '
215800090422     c                   exsr      MEMRES
215900090421    2c                   endif
216000090421     c
216100090421     c*  Se ancora vuoto --> colpa alla partenza
216200090421    2c                   if        wfgs=0
216300090421     c                   movel(P)  'A_PART_NOSI' wke1
216400090421     c                   exsr      MEMRES
216500090421    2c                   endif
216600090421     c
216700090422     c* Se è un transito ed è un terminal  --> colpa alla partenza
216800090422     c* (è per forza un terminal perchè se non lo fosse sarebbe già rientrato
216900090422     c*  sopra nella routine "A_TRA_NOSPUTER")
217000090422     c*  il realtà faccio rientrare tutti i caso mancanti (ma dovrebbe essere solo
217100090422     c*  questo)
217200090422    1c                   other
217300090421     c                   movel(P)  'A_PART_NOSI' wke1
217400090421     c                   exsr      MEMRES
217500090421     c                   endSL
217600090421     c                   ENDSR
217700990210      *****************************************************************
217800990210      * LETTURA DELLE SPUNTE PER CERCARE QUELLE CON ANOMALIA
217900990210      *****************************************************************
218000990210     c     RESPOAAV      BEGSR
218100990211    1c     wfgs          ifne      dctlna
218200990210     c                   exsr      terspunta
218300990210     c**
218400990210     c** Spuntato in arrivo dopo lo scarico
218500990211    2c     wflp          ifeq      i50tfa
218600990211    3c     wsianom       ifeq      'S'
218700990316     c
218800990316     c* Ci sono 2 casi:
218900990316     c* 1) chi ha sparato e il terminal di arrivo
219000990316    4c                   if        wfgs=i50tfa
219100990316     c                   if        werr=' '
219200990316     c                   eval      wke1='A_TER_DOPOSCAR '
219300990316     c                   else
219400990316     c                   eval      wke1='A_ERRTERSPUNTA '
219500990316     c                   endif
219600990316   x4c                   else
219700990316     c
219800990316     c* 2) Chi ha sparato e' un 2 livello della stessa area --> disguido
219900990316     c                   eval      wflp=wfgs
220000990316    5c                   if        werr=' '
220100990316     c                   eval      wke1='A_TRA_DOPOSCAR '
220200990316     c                   else
220300990316     c                   eval      wke1='A_ERRTRASPUNTA '
220400990316    5c                   endif
220500990316    4c                   endif
220600990316    c
220700990216     c
220800990316   x3c                   else
220900990305     c
221000990305     c* Se il terminal di arrivo del precedente = al terminal di chi ha
221100990305     c*  rilevato l'anomalia, vuol dire che ha scaricato tardi la pistola
221200081211    4c                   if        wflp=wflpspuan
221300081211    5c                   if        wfgs=wfgsspuan
221400990305     c                   eval      wke1='A_ARR_TARD     '
221500081211   x5c                   else
221600990309     c* Disguido in area
221700990309     c
221800990309     c                   eval      wke1='A_ARR_DISGAREA '
221900081211    5c                   endif
222000990309     c
222100081211   x4c                   else
222200990304     c                   eval      wke1='A_ARR_NOSI     '
222300081211    4c                   endif
222400990305     c
222500990305    3c                   endif
222600090831     c                   movel     'A'           wtipoflp
222700990210     c                   exsr      MEMRES
222800990305     c
222900990211   x2c                   else
223000990305     c
223100990211     c** Spuntato o dalla partenza
223200990211    3c     wflp          ifeq      i50tfp
223300990211    4c     wsianom       ifeq      'S'
223400990304     c                   eval      wke1='A_PART_SI      '
223500990211     c                   else
223600990304     c                   eval      wke1='A_PART_NOSI    '
223700990211    4c                   endif
223800090831     c                   movel     'P'           wtipoflp
223900990211     c                   exsr      MEMRES
224000990305     c
224100990211   x3c                   else
224200990305     c
224300990211     c** Spuntato al transito/disguido dopo lo scarico
224400990211    4c     wsianom       ifeq      'S'
224500990216     c                   if        werr=' '
224600990304     c                   eval      wke1='A_TRA_DOPOSCAR '
224700990216     c                   else
224800990304     c                   eval      wke1='A_ERRTRASPUNTA '
224900990216     c                   endif
225000990216     c
225100990211     c                   else
225200990304     c                   eval      wke1='A_TRA_NOSI     '
225300990211    4c                   endif
225400090831     c                   movel     'A'           wtipoflp
225500990210     c                   exsr      MEMRES
225600990211    3c                   endif
225700990211    2c                   endif
225800990211   x1c                   else
225900990210     c** Spuntato in arrivo dopo lo scarico
226000990211    2c     wsianom       ifeq      'S'
226100990216     c                   if        werr=' '
226200990304     c                   eval      wke1='A_ARR_DOPOSCAR '
226300990211     c                   else
226400990304     c                   eval      wke1='A_ERRARRSPUNTA '
226500990216    2c                   endif
226600990216     c
226700990216     c                   else
226800990305     c
226900990305     c* Se il terminal di arrivo del precedente = al terminal di chi ha
227000990305     c*  rilevato l'anomalia, vuol dire che ha scaricato tardi la pistola
227100990331     c                   if        wfgs=wfgsspuan
227200990305     c                   eval      wke1='A_ARR_TARD     '
227300990305     c                   else
227400990304     c                   eval      wke1='A_ARR_NOSI     '
227500990211    2c                   endif
227600990305    2c                   endif
227700990305     c
227800090831     c                   movel     'A'           wtipoflp
227900990210     c                   exsr      MEMRES
228000990210    3c                   endif
228100990211    1c                   ENDSR
228200990209      *****************************************************************
228300990209      * LETTURA DELLE SPUNTE PER CERCARE QUELLE CON ANOMALIA
228400990209      *****************************************************************
228500990209     c     LETBRV        BEGSR
228600990209     c                   z-add     1             zz                3 0
228700990210     c* Skiere in cui memorizzo per P.O. la spunta con data  piu' bassa
228800090428     c*  a parita di data, privilegiando le categorie ARR tenendo conto
228900990211     c*  delle data/ora scarico + tolleranza
229000990209     c                   clear                   waavnpg
229100990209     c                   clear                   waavdfv
229200990209     c                   clear                   waavfgs
229300000322     c                   clear                   waavAS
229400990209     c                   clear                   waavsca
229500990209     c                   clear                   waavdef
229600990210     c                   clear                   waavspg
229700000322     c                   clear                   waavnps
229800990209     c
229900990209     c* Campi in cui memorizzo il primo che ha rilevato l'anomalia
230000990210     c                   clear                   wnpg
230100990210     c                   clear                   wdfv
230200990210     c                   clear                   wfgs
230300990210     c                   clear                   wscar
230400990210     c                   clear                   wdef
230500990210     c                   clear                   wspg
230600990212     c                   clear                   werr
230700070711     c                   clear                   wnfva
230800000322     c                   clear                   brvas
230900990209     c* Campi di comodo
231000990209     c
231100070131     c     kdcd          setll     fnbrv07l
231200070131     c     kdcd          reade     fnbrv07l                               34
231300990209    1c     *in34         doweq     *off
231400000229     c* se data e ora scarico uguale a zeros muovo la data manutenzione spunte
231500000229      *
231600000229     c                   if        brvscar = 0
231700000229     c                   eval      brvdcs = brvdfs
231800000229     c                   eval      brvhcs = brvhms
231900000229     c                   endif
232000000229      *
232100990212     c                   clear                   w001a             1
232200990212     c                   clear                   wcarica           1
232300990209     c* Escludo le pistole completamente fasulle
232400990316     c*  Includo il transito come 89
232500990209     c                   movel     brvnps        w002a
232600990209     c     w002a         lookup    npsno                                  31
232700990209    3c     *in31         ifeq      *off
232800990316     c     brvnps        oreq      89
232900990209     c* Prendo la data
233000990209     c                   clear                   dslv53
233100990209     c                   movel     brvnpg        d53npg
233200000209     c                   z-add     brvnfv        d53nfv
233300050218     c**!!!              movel     simfel        d53fel
233400990209     c                   movel     brvfgs        d53fgs
233500990209     c                   movel     brvfle        d53flf
233600990209     c                   call      'FNLV53R'
233700990209     c                   parm                    dslv53
233800990209     c**
233900990209     c** se non trovata data ignoro
234000990209    4c     d53err        ifeq      *blanks
234100050204     c* Se data foglio = sabato, la imposto di lunedì
234200050204     c                   Clear                   wgioset
234300050204     c     *iso          Movel     d53dfv        datadmy
234400050204     c     *dmy          Movel     datadmy       w0060             6 0
234500050204     c                   Movel     w0060         wggmmaa
234600050204     c                   Call      'XGIOSE1'
234700050204     c                   Parm                    wggmmaa
234800050204     c                   Parm                    wgioset
234900050204     c
235000050204     c     *iso          move      d53dfv        wdataanm
235100130122     c
235200130122    5c                   if        wgioset='6'
235300050204     c                   adddur    2:*D          wdataanm
235400130122    5c                   endif
235500130122     c
235600130122     c                   setoff                                       34
235700130122    5c                   dow       not *in34
235800050204     c* controllo se e' festivo
235900050204     C     *iso          move      wdataanm      kdata
236000050204     C                   Z-ADD     68            KTFp
236100050204     C                   clear                   KTFa
236200050204     C     KCLN          CHAIN     AZCLN01L                           34
236300050204    7C     *IN34         IFEQ      *OFF
236400050204     C     MAT(KGG)      andne     ' '
236500050204     C     *IN34         oreq      *OFF
236600050204     C     POM(KGG)      andne     ' '
236700050204     c**
236800050204     c                   adddur    1:*D          wdataanm
236900050204     c                   else
237000050204     c                   seton                                        34
237100050204    7c                   endif
237200130122    5c                   enddo
237300050204     c
237400050204     c     *iso          movel     wdataanm      d53dfv
237500050204     c
237600050204     c
237700990209     c                   eval      zz=1
237800990209     c     brvfgs        lookup    waavfgs(zz)                            31
237900990209    5c                   if        not *in31
238000990209     c                   eval      zz=1
238100990209     c     000           lookup    waavfgs(zz)                            31
238200990209     c                   eval      waavfgs(zz)=brvfgs
238300990209    5c                   endif
238400000322     c
238500000322     c** prendo as di appartenenza
238600000322     c**  per cercare altro p.o. gestione su stesso AS
238700000322     c                   clear                   wstessoas
238800000322     c
238900000322     c     brvfgs        chain     azorg01l                           33
239000021120     c  n33              movel     orgde0        og140
239100021120     c  n33              movel     §ogaex        brvas
239200000322     c   33              movel     brvfgs        brvas
239300000322     c
239400000322    5c                   if        not *in33
239500041006     c* Se il p.o. spunta è la linea di arrivo o il terminal di arrivo
239600041006     c*  ignoro il fatto dello stesso AS
239700041006     c                   if        brvfgs<>i50lna and brvfgs<>i50tfa  and
239800041006     c                             brvfgs<>i50lnp
239900000322     c                   eval      jj=1
240000000322     c     brvas         lookup    waavas(jj)                             33
240100090428     c* elaboro anche se stesso p.o. per ignorare spunta 89 se poi c'e' reale
240200090428     c***                dow       *in33 and brvfgs=waavfgs(jj)  or
240300090428     c                   dow       *In33 and waavfgs(jj)=i50lna  or
240400041006     c                             *In33 and waavfgs(jj)=i50tfa  or
240500041006     c                             *In33 and waavfgs(jj)=i50lnp
240600000322     c                   eval      JJ=1+jj
240700021120     c     brvas         lookup    waavas(jj)                             33
240800000322     c                   enddo
240900000322     c* Trovato!!
241000000322     c   33              movel     'S'           wstessoas
241100041006     c
241200000322    5c                   endif
241300041006    5c                   endif
241400000322     c
241500000322     c** Se trovo spunta di altra filiale gestione ma stesso as e quella
241600000322     c**  letta e' pistola 89 --> la ignoro
241700000322   4ac                   if        wstessoas='S' and brvnps<>89 OR
241800000322     c                             wstessoas=' '
241900000322     c
242000000322    5c                   if        wstessoas='S' AND waavnps(jj)=89
242100000322     c** sostituisco la spunta e faccio fina che la precedente non ci
242200000322     c**  sia
242300000322     c                   eval      zz=jj
242400000322     c                   eval      waavfgs(zz)=brvfgs
242500000322     c                   clear                   waavdfv(zz)
242600000322     c                   clear                   waavnpg(zz)
242700000322     c                   clear                   waavspg(zz)
242800000322     c                   clear                   waavsca(zz)
242900000322    5c                   endif
243000990507     c
243100990507     c** Imposto campi di comodo
243200990507     c* campi nuova spunta letta
243300050207     c                   movel     brvnps        wnewnps           2 0
243400050207     c                   movel     brvnpg        wnewnpg           1
243500990211     c                   movel     d53def        wnewdef           1
243600990507     c                   movel     d53dfv        wnewdfv           8 0
243700990507     c                   movel     d53spg        wnewspg           1
243800990507     c                   movel     brvscar       wnewscar         12 0
243900990507     c* campi vecchia spunta memorizzata
244000990211     c                   movel     waavnpg(zz)   woldnpg           1
244100990211     c                   movel     waavdef(zz)   wolddef           1
244200990211     c                   movel     waavdfv(zz)   wolddfv
244300990507     c                   movel     waavspg(zz)   woldspg           1
244400050207     c                   movel     waavnps(zz)   woldnps           2 0
244500990507     c                   movel     waavsca(zz)   woldscar         12 0
244600040907     c                   EXSR      SCEGLIxav
244700990507     c
244800990507     c** spunta da caricare
244900990211    5c                   if        wcarica='S'
245000990209     c                   movel     brvnpg        waavnpg(zz)
245100990210     c                   eval      waavdfv(zz)=d53dfv
245200990209     c                   eval      waavsca(zz)=brvscar
245300990209     c                   eval      waavdef(zz)=d53def
245400990210     c                   eval      waavspg(zz)=d53spg
245500000322     c                   eval      waavnps(zz)=brvnps
245600000322     c                   eval      waavAS(zz)=brvas
245700990210    5c                   endif
245800990209     c**
245900990209     c** C'E' ANOMALIA:  tengo la spunta con la data piu' bassa
246000990209   5ac     brvcan        ifne      *blanks
246100990316     c     brvnps        andne     89
246200050615     c     brvcan        lookup    Noanomspu                              31
246300050615   5bc                   if        not *in31
246400990210     c                   clear                   WCARICA
246500990209     c**
246600990211     c* Controllo quale delle 2 spunte e' cat.arrivi
246700990507     c* reimposto solo i campi old perche' la NEW e' la stessa spunta letta
246800990211     c                   movel     wnpg          woldnpg           1
246900990211     c                   movel     wdef          wolddef           1
247000990507     c                   movel     wspg          woldspg
247100990211     c                   movel     wdfv          wolddfv           8 0
247200990507     c                   movel     wscar         woldscar         12 0
247300990507     c
247400040907     c                   EXSR      SCEGLIxav
247500990210     c
247600990210    5c                   if        wcarica='S'
247700990210     c                   eval      wdfv   =d53dfv
247800990210     c                   eval      wnpg   =brvnpg
247900990210     c                   eval      wscar  =brvscar
248000990210     c                   eval      wdef   =d53def
248100990210     c                   eval      wfgs   =brvfgs
248200990210     c                   eval      wspg   =d53spg
248300070711     c                   eval      wNFVa  =d53NFV
248400990212     c                   clear                   werr
248500990212     c* verifico per gli ammanchi se e' stata inserita l'anomalia sbagliata
248600990212     c*  -> se c'e' solo questa o e' la piu' piccola e' colpa sua
248700990212     c                   movel     brvnpg        w001a
248800990212     c     w001a         lookup    catarr                                 31
248900990315    6c   31brvnpg        ifeq      1
249000990212     c     d53def        andeq     ' '
249100990212     c                   setoff                                       31
249200990315    6c                   endif
249300990212     c
249400990315    6c                   if        *in31
249500990330    7c                   if        §tadragr='A'
249600990330     c     brvcan        lookup    amspu                                  34
249700990212     c** Anomalia non giusta --> memorizzo
249800990330     c  N34              eval      werr='E'
249900990315    7c                   endif
250000990330    7c                   if        §tadragr='V'
250100990330     c     brvcan        lookup    avspu                                  34
250200990330     c  N34              eval      werr='E'
250300990315    7c                   endif
250400990212    6c                   endif
250500990212     c
250600990209    5c                   endif
250700050615   5bc                   endif
250800050615   5ac                   endif
250900000322     c
251000000322  x4ac                   else
251100000322     c                   if        waavfgs(zz)<>0   AND waavdfv(zz)=0
251200000322     c                   clear                   waavfgs(zz)
251300000322     c                   endif
251400000322     c
251500000322   4ac                   endif
251600990209     c
251700990209    4c                   endif
251800990209    3c                   endif
251900990209     c**
252000070131     c     kdcd          reade     fnbrv07l                               34
252100990209    1c                   enddo
252200990209     c                   ENDSR
252300990507      *****************************************************************
252400040907      *  vedo quale delle 2 spunte tenere per ammanchi avarie
252500990507      *****************************************************************
252600040907     c     SCEGLIXAV     BEGSR
252700990507     c* determino se sono spunte arrivi
252800990507     c                   EXSR      DETERARR
252900990507     c
253000990507    5c                   SELECT
253100990507     c* Data OLD non presente --> carico
253200990507     c                   WHEN      wolddfv=0
253300990507     c                   eval      wcarica='S'
253400990507     c
253500990507     c* Data NEW minore data OLD --> carico solo se la old e' arrivi con
253600990507     c*      scarico minore
253700990507    5c                   WHEN      wnewDFV<wolddfv
253800990507     c* se la spunta con data minore e' una categoria partenza e quella con
253900990507     c*    data maggiore e' una arrivi, tengo quella arrivi se la data
254000990507     c*    di scarico e' minore
254100990507     c     wnewnpg       lookup    catpar                                 31
254200990507    6c   31              if        wnewnpg='3' and wnewspg<>'P'
254300990507     c                   setoff                                       31
254400990507    6c                   endif
254500990507     c**
254600990507     c** La spunta scaricata non e' partenza --> la tengo
254700990507    6c                   if        not *in31  OR
254800990507     c                             *in31  and woldarr<>'A'
254900990507     c                   eval      wcarica='S'
255000990507   x6c                   else
255100071002     c* la spunta scaricata e' partenza con pistola autogenerata 89
255200071002     c*  tengo sempre la spunta arrivi altrimenti
255300071002     c*  controllo data ora scarico
255400071002   6ac                   if        wnewnps<>89
255500071002     c
255600990507    7c     wnewscar      iflt      woldscar
255700990507     c                   movel     wnewscar      w0080             8 0
255800990507     c                   move      wnewscar      w0040             4 0
255900990507     c                   EXSR      ADDTOLLER
256000990507     c                   movel     w0080         w0120
256100990507     c                   move      w0040         w0120            12 0
256200990507    8c     w0120         iflt      woldscar
256300990507     c                   eval      wcarica ='S'
256400990507    8c                   endif
256500990507    7c                   endif
256600071002   6ac                   endif
256700071002    6c                   endif
256800990507     c
256900990507     c* Se date uguali --> prendo quella con caricamento piu' basso o la
257000990507     c*                    categoria arrivi se rientra nei minuti di tollera
257100050207     c*                    Se sono due spunte arrivi tengo la pist.reale
257200990507     c
257300990507    5c                   WHEN      wnewDFV=wolddfv
257400990507     c                   EXSR      TIENIARR
257500990507     c
257600990507     c* Se la data e' > verifico se sto scaricando una arrivi e ho memorizza
257700990507     c*  to una partenza
257800990507    5c                   WHEN      wnewDFV>wolddfv AND wnewarr='A'
257900990507     c     woldnpg       lookup    catpar                                 31
258000990507    6c   31              if        woldnpg='3' and woldspg<>'P'
258100990507     c                   setoff                                       31
258200990507    6c                   endif
258300990507     c
258400990507     c** La spunta memorizzata deve essere una partenza
258500990507    6c                   if        *in31
258600990507     c
258700990507     c* la spunta scaricata e' partenza --> controllo data ora scarico
258800990507    7c     wnewscar      ifle      woldscar
258900990507     c                   eval      wcarica ='S'
259000990507     c                   else
259100990507     c                   movel     woldscar      w0080             8 0
259200990507     c                   move      woldscar      w0040             4 0
259300990507     c                   EXSR      ADDTOLLER
259400990507     c                   movel     w0080         w0120
259500990507     c                   move      w0040         w0120            12 0
259600990507    8c     wnewscar      ifle      w0120
259700990507     c                   eval      wcarica ='S'
259800990507    8c                   endif
259900990507    7c                   endif
260000990507    6c                   endif
260100071002     c
260200071002     c** La spunta memorizzata è partenza autogenerata con pistola 89
260300071002     c** e la spunta arrivi è pistola reale
260400071002     c** tengo sempre la spunta arrivi
260500071002    6c                   if        *in31  and woldnps=89
260600071002     c                   eval      wcarica ='S'
260700071002    6c                   endif
260800990507     c
260900990507    5c                   endsl
261000990507     c**
261100990507     c                   ENDSR
261200040907      *****************************************************************
261300040907      *  Vedo quale delle 2 spunte tenere per MANCANZE
261400040907      *****************************************************************
261500040907     c     SCEGLIXMA     BEGSR
261600040907     c* determino se sono spunte arrivi
261700040907     c                   EXSR      DETERARR
261800040907     c
261900040907    5c                   SELECT
262000040907     c* Data NEW minore data OLD --> carico solo se la old e' arrivi con
262100040907     c*      scarico minore
262200040907    5c                   WHEN      wnewDFV<wolddfv  and woldarr='A'
262300040907     c* se la spunta con data minore e' una categoria partenza e quella con
262400040907     c*    data maggiore e' una arrivi, tengo quella arrivi se la data
262500040907     c*    di scarico e' minore
262600040907     c     wnewnpg       lookup    catpar                                 31
262700040907    6c   31              if        wnewnpg='3' and wnewspg<>'P'
262800040907     c                   setoff                                       31
262900040907    6c                   endif
263000040907     c**
263100040907     c* la spunta scaricata e' partenza --> controllo data ora scarico
263200040907    6c                   if        *in31
263300040907    7c     wnewscar      ifge      woldscar
263400040907     c                   eval      wcarica ='S'
263500040907     c                   else
263600040907     c                   movel     wnewscar      w0080             8 0
263700040907     c                   move      wnewscar      w0040             4 0
263800040907     c                   EXSR      ADDTOLLER
263900040907     c                   movel     w0080         w0120
264000040907     c                   move      w0040         w0120            12 0
264100040907    8c     w0120         ifge      woldscar
264200040907     c                   eval      wcarica ='S'
264300040907    8c                   endif
264400040907    7c                   endif
264500040907    6c                   endif
264600040907     c
264700040907     c* Se date uguali --> prendo quella con caricamento piu' alto  o la
264800040907     c*                    categoria part.  se rientra nei minuti di tollera
264900040907     c
265000040907    5c                   WHEN      wnewDFV=wolddfv
265100040907     c
265200040907    6c     wnewscar      ifgt      woldscar
265300040907     c     woldarr       andeq     wnewarr
265400040907     c                   eval      wcarica='S'
265500040907   x6c                   else
265600040907     c**
265700040907     c* Valuto quale tenere con i minuti di tolleranza
265800040907     c     wnewnpg       lookup    catpar                                 31
265900040907    7c   31              if        wnewnpg='3' and wnewspg<>'P'
266000040907     c                   setoff                                       31
266100040907    7c                   endif
266200040907     c
266300040907    7c                   if        *in31
266400040907    8c     wnewscar      ifge      woldscar
266500040907     c                   eval      wcarica='S'
266600040907   x8c                   else
266700040907     c                   movel     wnewscar      w0080             8 0
266800040907     c                   move      wnewscar      w0040             4 0
266900040907     c                   EXSR      ADDTOLLER
267000040907     c                   movel     w0080         w0120
267100040907     c                   move      w0040         w0120
267200040907    9c     w0120         ifge      woldscar
267300040907     c                   eval      wcarica='S'
267400040907    9c                   endif
267500040907    8c                   endif
267600040907    7c                   endif
267700040907    c
267800040907     c     woldnpg       lookup    catpar                                 31
267900040907    7c   32              if        wnewnpg='3' and wnewspg<>'P'
268000040907     c                   setoff                                       32
268100040907    7c                   endif
268200040907    7c                   if        *in32
268300040907    8c     wnewscar      ifgt      woldscar
268400040907     c                   movel     woldscar      w0080             8 0
268500040907     c                   move      woldscar      w0040             4 0
268600040907     c                   EXSR      ADDTOLLER
268700040907     c                   movel     w0080         w0120
268800040907     c                   move      w0040         w0120            12 0
268900040907    9c     wnewscar      ifgt      w0120
269000040907     c                   eval      wcarica ='S'
269100040907    9c                   endif
269200040907    8c                   endif
269300040907    7c                   endif
269400040907     c* Se ne vecchia nè nuova partenza, tengo il caricamento + alto
269500040907    7c                   if        not *in31 and not *in32
269600040907    8c     wnewscar      ifgt      woldscar
269700040907     c                   eval      wcarica ='S'
269800040907    8c                   endif
269900040907    7c                   endif
270000040907     c
270100040907    6c                   endif
270200040907     c
270300040907     c* Se la data e' > verifico se sto scaricando una arrivi e ho memorizza
270400040907     c*  to una partenza
270500040907    5c                   WHEN      wnewDFV>wolddfv AND wnewarr=' '
270600040907     c                   eval      wcarica ='S'
270700040907     c
270800040907    5c                   WHEN      wnewDFV>wolddfv AND wnewarr='A'
270900040907     c     woldnpg       lookup    catpar                                 31
271000040907    6c   31              if        woldnpg='3' and woldspg<>'P'
271100040907     c                   setoff                                       31
271200040907    6c                   endif
271300040907     c
271400040907     c** La spunta memorizzata deve essere una partenza
271500040907    6c                   if        *in31
271600040907    7c     wnewscar      ifgt      woldscar
271700040907     c                   movel     woldscar      w0080             8 0
271800040907     c                   move      woldscar      w0040             4 0
271900040907     c                   EXSR      ADDTOLLER
272000040907     c                   movel     w0080         w0120
272100040907     c                   move      w0040         w0120            12 0
272200040907    8c     wnewscar      ifgt      w0120
272300040907     c                   eval      wcarica ='S'
272400040907    8c                   endif
272500040907    7c                   endif
272600040907     c
272700040907     c* la spunta scaricata e' partenza --> controllo data ora scarico
272800040907   x6c                   else
272900040907     c* Se la vecchia non è partenza, carico
273000040907     c                   eval      wcarica ='S'
273100040907    6c                   endif
273200040907     c
273300040907    5c                   endsl
273400040907     c**
273500040907     c                   ENDSR
273600990211      *****************************************************************
273700990211      *   TENGO LA SPUNTA ARRIVI CONSIDERANDO I MINUTI DI TOLLERANZA
273800990211      *****************************************************************
273900990211     c     TIENIARR      BEGSR
274000050207     c* se sono 2 cat.arrivi -stessa data e c'e' una pistola fasulla, tengo
274100050207     c*  la pistola reale a prescindere dallo scarico
274200050207     c                   setoff                                       3132
274300050207    0c                   if        woldarr=wnewarr and woldarr='A'
274400050207     c                   movel     wnewnps       w002a
274500050207     c     w002a         lookup    npsno                                  31
274600050207     c                   movel     woldnps       w002a
274700050207     c     w002a         lookup    npsno                                  32
274800090105
274900090105     c* se la vecchia è fasulla, e la nuova  no, tengo la NUOVA
275000090105     c                   if        *in32 and  not *in31
275100090105     c                   eval      wcarica='S'
275200090105     c                   goto      endtieni
275300090105     c                   endif
275400090105     c* se la nuova è fasulla, e la vecchia no, tengo la vecchia
275500090105     c                   if        *in31 and  not *in32
275600090105     c                   goto      endtieni
275700090105     c                   endif
275800090105
275900090105   x0c                   else
276000090105     c
276100090105     c* oppure se una entrata e una arrivi fasulla --> tengo l'entrata
276200090105     c                   movel     wnewnps       w002a
276300090105     c     w002a         lookup    npsno                                  31
276400090105     c                   movel     woldnps       w002a
276500090105     c     w002a         lookup    npsno                                  32
276600090105     c* se la vecchia è fasulla arrivi, e la nuova no entrata, tengo laNUOVA
276700090105    1c                   if        *in32 and  not *in31  and
276800090105     c                             woldarr='A' and wnewarr='5'
276900090105     c                   eval      wcarica='S'
277000090105     c                   goto      endtieni
277100090105    1c                   endif
277200090105     c* se la nuova è fasulla arrivi, e la vecchia no entrata, tengo la vecchia
277300090105    1c                   if        *in31 and  not *in32     and
277400090105     c                             wnewarr='A' and woldarr='5'
277500090105     c                   goto      endtieni
277600090105    1c                   endif
277700090105
277800090105    0c                   endif
277900050207     c
278000050207     c
278100050207     c* Se sono entrambe non arrivi o entrambe arrivi tengo lo scarico + bas
278200990211    1c     wnewscar      iflt      woldscar
278300990507     c     woldarr       andeq     wnewarr
278400990211     c                   eval      wcarica='S'
278500050207     c                   goto      endtieni
278600050207    1c                   endif
278700990211     c**
278800990211     c* Valuto quale tenere con i minuti di tolleranza
278900990507    2c                   if        wnewarr='A'
279000090702    3c     wnewscar      ifge      woldscar
279100990211     c                   movel     woldscar      w0080             8 0
279200990211     c                   move      woldscar      w0040             4 0
279300990211     c                   EXSR      ADDTOLLER
279400990211     c                   movel     w0080         w0120
279500990211     c                   move      w0040         w0120
279600990211    4c     wnewscar      ifle      w0120
279700990211     c                   eval      wcarica='S'
279800990211    4c                   endif
279900990316     c
280000990316     c                   else
280100990316     c                   eval      wcarica='S'
280200990211    3c                   endif
280300990211    2c                   endif
280400990211    c
280500990507    2c                   if        woldarr='A'
280600090702    3c     wnewscar      iflt      woldscar
280700990211     c                   movel     wnewscar      w0080             8 0
280800990211     c                   move      wnewscar      w0040             4 0
280900990211     c                   EXSR      ADDTOLLER
281000990211     c                   movel     w0080         w0120
281100990211     c                   move      w0040         w0120            12 0
281200990507    4c     w0120         iflt      woldscar
281300990211     c                   eval      wcarica ='S'
281400990211    4c                   endif
281500990211    3c                   endif
281600990211    2c                   endif
281700990211     c
281800990211     c
281900050207     c     endtieni      ENDSR
282000990211      *****************************************************************
282100990211      *   DETERMINO QUELE DELLE SPUNTE E' CAT.ARRIVI
282200990211      *****************************************************************
282300990211     c     DETERARR      BEGSR
282400990507     c                   clear                   WOLDARR
282500990507     c                   clear                   WNEWARR
282600990211     c
282700990211     c     wnewnpg       lookup    catarr                                 31
282800990211     c   31              if        wnewnpg='1' and wnewdef=' '
282900990211     c                   setoff                                       31
283000990211     c                   endif
283100990507     c   31              movel     'A'           wnewarr           1
283200090105     c  n31              if        wnewnpg='5'
283300090105     c                   movel     '5'           wnewarr           1
283400090105     c                   endif
283500990211     c
283600990211     c     woldnpg       lookup    catarr                                 31
283700990211     c   31              if        woldnpg ='1' and wolddef   =' '
283800990211     c                   setoff                                       31
283900990211     c                   endif
284000990507     c   31              movel     'A'           woldarr           1
284100090105     c  n31              if        woldnpg='5'
284200090105     c                   movel     '5'           woldarr           1
284300090105     c                   endif
284400990507     c
284500990211     c                   ENDSR
284600990205      *****************************************************************
284700990211      *   SOMMO MINUTI DI TOLLERANZA
284800990205      *****************************************************************
284900990211     C     ADDTOLLER     BEGSR
285000990211     c                   clear                   w0060             6 0
285100990211     c                   movel     w0040         w0060
285200990211     c     *iso          move      w0080         wdata
285300990211     c     *iso          move      w0060         wora
285400990212     c                   adddur    §stdmtl:*mn   wtempo
285500990211     c     *iso          move      wdata         w0080
285600990211     c     *iso          move      wora          w0060
285700990211     c                   movel     w0060         w0040
285800990211     c
285900990211     c                   ENDSR
286000990211      *****************************************************************
286100990211      *   CONTROLLO SE IL COLLO ARRIVATO E' STATO spuntato da altre parti
286200990211      *****************************************************************
286300990211     C     SPUARR        BEGSR
286400990210     c                   clear                   wdfv
286500990210     c                   clear                   wdcs
286600990210     c                   clear                   whcs
286700990210     c                   clear                   wfgs
286800990210     c                   clear                   wnpg
286900990210     c                   clear                   wspg
287000990308     c                   clear                   wdef
287100040907     c                   clear                   wnfv
287200990311     c                   clear                   wEXPdfv
287300990311     c                   clear                   wEXPfgs
287400990311     c                   clear                   wEXPnpg
287500990311     c                   clear                   wEXPdef
287600990311     c                   clear                   wEXPscar
287700990205     c* cerco la spunta del collo con la data piu' alta e se > data arrivo
287800070131     c     kdcd          chain     fnbrv07l                           34
287900990311    1c     *in34         doweq     *off
288000060301     c
288100060301     c* Escludo, se richiesto, spunte autogenerate
288200060301   1ac                   if        (brvscar>0 and wautogen='N') or
288300060301     c                             wautogen=' '
288400000229     c* se data e ora scarico uguale a zeros muovo la data manutenzione spunte
288500000229      *
288600000229     c                   if        brvscar = 0
288700000229     c                   eval      brvdcs = brvdfs
288800000229     c                   eval      brvhcs = brvhms
288900000229     c                   endif
289000040906     c                   clear                   wcarica
289100000229      *
289200990205     c* escludo le pistole completamente fasulle, che non nascono nemmmeno
289300990205     c*   da un'altra spunta
289400990208     c                   movel     brvnps        w002a             2
289500990208     c     w002a         lookup    npsno                                  31
289600990311    2c     *in31         ifeq      *off
289700990205     c                   clear                   dslv53
289800990205     c                   movel     brvnpg        d53npg
289900000209     c                   z-add     brvnfv        d53nfv
290000050218     c**!!!              movel     simfel        d53fel
290100990205     c                   movel     brvfgs        d53fgs
290200990205     c                   movel     brvfle        d53flf
290300990205     c                   call      'FNLV53R'
290400990205     c                   parm                    dslv53
290500990205     c** se non trovata data ignoro
290600990311    3c     d53err        ifeq      *blanks
290700990205     c     d53dfv        andge     wdatrifer
290800040906     c**
290900040907     c** Imposto campi di comodo  se stesso p.o. spunta  per vedere se tener
291000040907     c*   spunta partenza o spunta arrivi
291100040907     c                   if        brvfgs=wfgs
291200040906     c* campi nuova spunta letta
291300040906     c                   movel     brvnpg        wnewnpg           1
291400040906     c                   movel     d53def        wnewdef           1
291500040906     c                   movel     d53dfv        wnewdfv           8 0
291600040906     c                   movel     d53spg        wnewspg           1
291700040906     c                   movel     brvscar       wnewscar         12 0
291800040906     c* campi vecchia spunta memorizzata
291900040906     c                   movel     wnpg          woldnpg           1
292000040906     c                   movel     wdef          wolddef           1
292100040906     c                   movel     wdfv          wolddfv
292200040906     c                   movel     wspg          woldspg           1
292300040906     c                   movel     wscar         woldscar         12 0
292400040907     c                   EXSR      SCEGLIxma
292500040907     c                   else
292600040907    4c     d53dfv        ifgt      wdfv
292700040907     c     d53dfv        oreq      wdfv
292800040907     c     brvscar       andgt     wscar
292900040907     c                   eval      wcarica='S'
293000040907     c                   endif
293100040907     c                   endif
293200040906     c
293300040906     c** spunta da caricare
293400040906    5c                   if        wcarica='S'
293500990205     c**
293600040906    4c***  d53dfv        ifgt      wdfv
293700040906     c***  d53dfv        oreq      wdfv
293800040906     c***  brvscar       andgt     wscar
293900990210     c                   movel     d53dfv        wdfv
294000990308     c                   movel     d53def        wdef
294100990210     c                   movel     d53spg        wspg
294200990210     c                   movel     brvfgs        wfgs
294300990210     c                   movel     brvnpg        wnpg
294400040907     c                   z-add     brvnfv        wnfv
294500990210     c                   movel     brvscar       wscar
294600990311    4c                   endif
294700990311     c
294800040906     c* Se Export memorizzo la spunta defluenza con data >
294900990311    4c                   if        brvnpg=1 and d53def='S' and
295000990311     c                             lnafl1='E'
295100990311     c
295200990311    5c     d53dfv        ifgt      wEXPdfv
295300990311     c     d53dfv        oreq      wEXPdfv
295400990311     c     brvscar       andgt     wEXPscar
295500990311     c                   movel     d53dfv        wEXPdfv
295600990311     c                   movel     d53def        wEXPdef
295700990311     c                   movel     brvfgs        wEXPfgs
295800990311     c                   movel     brvnpg        wEXPnpg
295900990311     c                   movel     brvscar       wEXPscar
296000990311    5c                   endif
296100990311    4c                   endif
296200990311    c
296300990311    3c                   endif
296400990311    2c                   endif
296500060301   1ac                   endif
296600990205     c**
296700070131     c     kdcd          reade     fnbrv07l                               34
296800990311    1c                   enddo
296900990205     c
297000990205     c                   endsr
297100980506      *****************************************************************
297200980506      *   ROUTINE INIZIALE
297300980506      *****************************************************************
297400980506     C     *INZSR        BEGSR
297500980506      *
297600980506     C     *ENTRY        PLIST
297700980506     C                   PARM                    KPJBA
297800990204     C                   MOVEL     KPJBU         savkpjbu
297900980506      *
298000980506     C                   Z-ADD     1             CODUT
298100980506     C                   CALL      'X§PARUT'
298200980507     C                   PARM                    UT§DSE0F
298300980508     C                   MOVEL     REC80         CNCR80
298400990310     c**
298500990310     c* carico al primo posto al pistola 90
298600990311     c                   eval      npsno(1)='90'
298700990204     c**
298800990204     c* carico la tabella delle pistole fasulle
298900990204     c                   z-add     1             xx
299000990310     c                   z-add     2             yy                4 0
299100990204     c                   movel     '7J'          cod
299200990208     c     ktab2         chain     tabel                              31
299300990204     c     *in31         doweq     *off
299400990204     c                   movel     tbluni        ds7j
299500990204     c     §7jrps        ifeq      'A'
299600990204     C                   MOVEL     TBLKEY        NPS(XX)
299700990204     C                   ADD       1             XX
299800990204     C                   ENDIF
299900990303     c* Carico la tabella delle piu' fasulle 88 89 con priorita' 40
300000990205     c     §7jord        ifeq      040
300100990205     C                   MOVEL     TBLKEY        NPSNO(YY)
300200990205     C                   ADD       1             yy
300300990205     C                   ENDIF
300400990204     c**
300500990204     c     ktab2         reade     tabel                                  31
300600990204     C                   ENDDO
300700990209     c**
300800990305     c* Carico la tabella delle anomalie idd per la procedura danni
300900990305     c                   z-add     1             xx
301000990305     c                   movel     '7C'          cod
301100990305     c     ktab2         chain     tabel                              31
301200990305     c     *in31         doweq     *off
301300990305     c                   movel     tbluni        ds7c
301400990305     c     §7cdan        ifeq      'S'
301500990305     C                   MOVEL     TBLKEY        ANIDD(XX)
301600990305     C                   ADD       1             XX
301700990305     C                   ENDIF
301800990305     c**
301900990305     c     ktab2         reade     tabel                                  31
302000990305     C                   ENDDO
302100990211     c* Carico la tabella delle categorie che sono considerate ARRIVI
302200990210     c*   la defluenza adesso non la considero
302300990211     c                   eval      catarr(1)='2'
302400990211     c                   eval      catarr(2)='6'
302500990211     c* Carico la tabella delle categorie che sono considerate PARTENZA
302600990507     c                   eval      catpar(1)='1'
302700990211     c                   eval      catpar(2)='5'
302800990211     c                   eval      catpar(3)='3'
302900990209     c**
303000990210     c* carico la tabella dei tipi anomalie spunte gli "Ammanco"
303100990210     c                   z-add     1             xx
303200050615     c                   z-add     1             yy
303300050615     c                   z-add     1             zz
303400990210     c                   movel     '3E'          cod
303500990210     c     ktab2         chain     tabel                              31
303600990210     c     *in31         doweq     *off
303700990210     c                   movel     tbluni        ds3e
303800990330     c     §3efta        ifne      'V'
303900990330     C                   MOVEL     TBLKEY        AMSPU(XX)
304000990210     C                   ADD       1             XX
304100990210     C                   ENDIF
304200990330     c     §3efta        ifne      'A'
304300050615     C                   MOVEL     TBLKEY        AVSPU(yy)
304400050615     C                   ADD       1             yy
304500990330     C                   ENDIF
304600050615     c* Carico le anomalie spunte che di fatto non sono anomalie ma
304700050615     c*  servono per la chiusura distinta
304800050615     c                   if        §3Efta='R'
304900050615     C                   MOVEL     TBLKEY        NOANOMSPU(zz)
305000050615     C                   ADD       1             zz
305100050615     c                   endif
305200990210     c**
305300990210     c     ktab2         reade     tabel                                  31
305400990210     C                   ENDDO
305500010907      *
305600010907      * recupero la moneta corrente
305700010907     C                   CLEAR                   TIBS02DS
305800010907     C                   MOVEL     'C'           T02MOD
305900010907     C                   MOVEL     KNSIF         T02SIF
306000010907     C                   MOVEL     'GED'         T02COD
306100010907     C                   MOVEL     'DANNI'       T02KE1
306200010907     C                   CALL      'TIBS02R'
306300010907     C                   PARM                    KPJBA
306400010907     C                   PARM                    TIBS02DS
306500010910     C                   MOVEL     T02UNI        DGEDDN
306600990210     c**
306700990210     c* A seconda del tipo anomalia cerco la responsab.
306800990210     C                   clear                   tibs02ds
306900990210     C                   MOVEL     'C'           t02mod
307000990210     C                   MOVEL     knsif         t02sif
307100990210     C                   MOVEL     'STD'         t02cod
307200990210     C                   MOVEL(p)  '1'           t02ke1
307300010910     C                   MOVEL(P)  §gedDBA       t02ke2
307400990210     C                   CALL      'TIBS02R'
307500990210     C                   PARM                    KPJBA
307600990210     C                   PARM                    TIBS02DS
307700990210     c* non ci sono errori --> prendo i minuti di tolleranza per scarico spu
307800990210     c                   if        t02err=' '
307900990210     c                   movel     t02uni        dstd
308000990210     c                   else
308100990210     c                   eval      §stdmtl = 30
308200990210     c                   endif
308300051010     c
308400051010     c                   time                    wtempo
308500051010     c     *iso          move      wdata         dateu             8 0
308600051010     c
308700990209      * Accesso fndct fndcd
308800990129     c     kdct          klist
308900990128     c                   kfld                    i50aac
309000990128     c                   kfld                    i50fil
309100990128     c                   kfld                    i50nca
309200080714     c     kblp          klist
309300080714     c                   kfld                    dctaas
309400080714     c                   kfld                    dctlnp
309500080714     c                   kfld                    dctnrs
309600080714     c                   kfld                    dctnsp
309700090303     c     kar5          klist
309800090303     c                   kfld                    dctaas
309900090303     c                   kfld                    dctlnp
310000090303     c                   kfld                    dctnrs
310100090303     c                   kfld                    dctnsp
310200090303     c                   kfld                    ktrd
310300071018     c     kdcr2         klist
310400071018     c                   kfld                    i50aac
310500071018     c                   kfld                    i50fil
310600071018     c                   kfld                    i50nca
310700071018     c                   kfld                    w0070
310800990129     c     kdcr          klist
310900990129     c                   kfld                    i50aac
311000990129     c                   kfld                    i50fil
311100990129     c                   kfld                    i50nca
311200990216     c                   kfld                    wsegnadan
311300990216     c                   kfld                    wpor
311400990129     c     kTAB          klist
311500990129     c                   kfld                    codut
311600990129     c                   kfld                    cod
311700990129     c                   kfld                    key
311800990208     c     kTAB2         klist
311900990208     c                   kfld                    codut
312000990208     c                   kfld                    cod
312100990304     c     KTBE          klist
312200990304     c                   kfld                    wcod
312300990304     c                   kfld                    wlin
312400990304     c                   kfld                    wsif
312500990304     c                   kfld                    wke1
312600990304     c                   kfld                    wke2
312700990304     c     KTBE2         klist
312800990304     c                   kfld                    wcod
312900990304     c                   kfld                    wlin
313000990304     c                   kfld                    wsif
313100990304     c                   kfld                    wke1
313200990129     c     kDCD          klist
313300990129     c                   kfld                    dcdfls
313400990129     c                   kfld                    dcdlna
313500990129     c                   kfld                    dcdnrs
313600990208     c                   kfld                    dcdnsc
313700070711     c     kBRVE         klist
313800070821     c                   kfld                    wnpgspuan
313900070711     c                   kfld                    wnfvA
314000070821     c                   kfld                    wfgsspuan
314100070711     c                   kfld                    dcdfls
314200070711     c                   kfld                    dcdlna
314300070711     c                   kfld                    dcdnrs
314400070711     c                   kfld                    dcdnsc
314500990129     C     KCLN          KLIST
314600990129     C                   KFLD                    KTFP
314700990129     C                   KFLD                    KTFA
314800990129     C                   KFLD                    KAAA              4 0
314900990129     C                   KFLD                    KMMM              2 0
315000990305     c     keve          klist
315100990305     c                   kfld                    dctaae
315200990305     c                   kfld                    dctnev
315300990204     C                   MOVEL     savKPJBU      kpjbu
315400040907     c     kfgv          klist
315500040907     c                   kfld                    wnfv
315600040907     c                   kfld                    wfgs
315700980717      *
315800980506     C                   ENDSR
315900980508      *****************************************************************
316000980508** MSG  (Lungh. 78)                                                          *
316100990208Tipo anomalia inesistente                                                          1
316200990208C.A. inesistente                                                                   2
316300990216Impossibile aggiornare la C.A.: allocata da altro lavoro                           3
316400990208Impossibile calcolare Responsabilita':mancano dett.colli CA/Spunte/Riserva         4
316500990208Manca la tabella per il calcolo della responsabilita': AVVISARE IL CED !!!         5
316600990208Manca "num.colli danneggiati"e"num.colli della bolla":impossibile il calcolo       6
316700990208Non trovato dettaglio colli bolla per la C.A.                                      7
316800990208Tipo spedizione <> "A" e la responsabilita'non e'della PARTENZA: IMPOSSIBILE!!     8
