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
007400990128     D FIDN50DS      E DS
007500990128     D TIBS02DS      E DS
007600990129     D DDCT01        E DS
007700990216     D DDCR01        E DS
007800080714     D DS1B          E DS
007900080714     D DS2A          E DS
008000990129     D DTAD          E DS
008100990208     D DS7J          E DS
008200990305     D DS7C          E DS
008300990210     D DS3E          E DS
008400990210     D DSTD          E DS
008500010910     D DGEDDN        E DS
008600080903     D OG143         E DS
008700080903     D OG148         E DS
008800021120     D OG140         E DS
008900990304     D DTRS          E DS
009000100609     D  RESPOf                 1     10    dim(10)
009100100609     D  RESPCf                11     60  2 dim(10)
009200100609     D  RESPOa                70     79    dim(10)
009300100609     D  RESPCa                80    129  2 dim(10)
009400100610     D  RESPOt               130    139    dim(10)
009500100610     D  RESPCt               140    189  2 dim(10)
009600990208     D DSLV53        E DS                  EXTNAME(fnlv53ds)
009700990208     D DSLV55        E DS                  EXTNAME(fnlv55ds)
009800090303     d
009900090303     D Dar5gen       E DS
010000170413     d dvpodeco      e ds
010100990129      *--------------------------------------------
010200990129      * DS interne
010300990129      *--------------------------------------------
010400990208     D                 DS
010500990129     D  dctaac                 1      4  0
010600990129     D  dctmgc                 5      8  0
010700990208     D  dctdca                 1      8  0
010800990129     D clnmat          DS
010900990208     D  MAT                    1     31    dim(31)
011000990129     D clnpom          DS
011100990208     D  pom                    1     31    dim(31)
011200990209     D                 DS
011300070131     D  brvdcs                 1      8  0
011400070131     D  brvhcs                 9     14  0
011500990209     D  brvscar                1     12  0
011600990209     D                 DS
011700990210     D  wdcs                   1      8  0
011800070131     D  whcs                   9     12  0
011900990210     D  WSCAR                  1     12  0
012000990311     D                 DS
012100990311     D  wEXPdcs                1      8  0
012200070131     D  wEXPhcs                9     12  0
012300990311     D  WEXPSCAR               1     12  0
012400990208     D                 DS
012500990208     D  kaaa                   1      4  0
012600990208     D  kmmm                   5      6  0
012700990208     D  kgg                    7      8  0
012800990208     D  kDATA                  1      8  0
012900090120     D                 DS
013000090120     D  blpaas                 1      4  0
013100090120     D  blpmgs                 5      8  0
013200090120     D  blpdsp                 1      8  0
013300990216     D                 DS
013400990216     D  WPOR                   1      3  0
013500990216     D  WSEGNADAN              4     10
013600090324     D  WTER                  11     13  0
013700090324     D  WOKEY                  1     13
013800990129      *--------------------------------------------
013900990129      * definizione campi
014000990129      *--------------------------------------------
014100990208     D WDATAFASE       S               D   DATFMT(*iso)
014200990208     D WDATAANM        S               D   DATFMT(*iso)
014300050204     d Datadmy         s               d   datfmt(*dmy)
014400990212     D                 DS
014500990212     D WTEMPO                          Z   inz(Z'1999-01-01-01.00.00.000000')
014600990212     D  WDATA                          D   DATFMT(*iso) OVERLAY(WTEMPO)
014700990212     D  WORA                           T   TIMFMT(*iso) OVERLAY(WTEMPO:12)
014800990211     D
014900130513     D areater         S                   LIKE(orgcar)
015000130513     D nfvpar          S                   LIKE(wnfv)
015100060309     D fgspar          S                   LIKE(wfgs)
015200060309     D brvas           S                   LIKE(§OGAEX)
015300000322     D WSAVDCH         S                   LIKE(anmdch)
015400990305     D WSAVCCH         S                   LIKE(anmcch)
015500990305     D WSAVDAO         S                   LIKE(anmdao)
015600990305     D WCOD            S                   LIKE(t02cod)
015700990304     D WLIN            S                   LIKE(t02lin)
015800990304     D WSIF            S                   LIKE(t02sif)
015900990304     D WKE1            S                   LIKE(t02ke1)
016000070711     D SAVWKE1         S                   LIKE(t02ke1)
016100990209     D WKE2            S                   LIKE(t02ke2)
016200990129     D WNCN            S                   LIKE(dctncn)
016300990129     D WCOLLODAN       S                   LIKE(dctncn)
016400990129     D WPER            S                   LIKE(dctper)
016500090303     D Percen          S                   LIKE(dctper)
016600990129     D KTFA            S                   LIKE(clntfa)
016700990129     D KTFP            S                   LIKE(clntfp)
016800000321     D FLPTFA          S                   LIKE(clntfp)
016900990129     D WTCR            S                   LIKE(dcttcr)
017000990129     D cod             S                   LIKE(tblcod)
017100990129     D key             S                   LIKE(tblkey)
017200990204     D SAVKPJBU        S                   LIKE(kpjbu)
017300990210     D WDFV            S                   LIKE(d53dfv)
017400990311     D WEXPDFV         S                   LIKE(d53dfv)
017500990210     D WSPG            S                   LIKE(d53spg)
017600990210     D WFGS            S                   LIKE(d53fgs)
017700040907     D WNFV            S                   LIKE(d53nfv)
017800070711     D WNFVA           S                   LIKE(BRVNFV)
017900990311     D WEXPFGS         S                   LIKE(d53fgs)
018000990210     D WNPG            S                   LIKE(d53npg)
018100990311     D WEXPNPG         S                   LIKE(d53npg)
018200990212     D WERR            S              1
018300990329     D WRIT            S              1
018400990205     D WDATRIFER       S                   LIKE(d53dfv)
018500990208     D WFLP            S                   LIKE(artflp)
018600070821     D WNPGSPUAN       S                   LIKE(BRVNPG)
018700070821     D WFLPSPUAN       S                   LIKE(artflp)
018800990309     D WFGSSPUAN       S                   LIKE(artflp)
018900990305     D WCLIP           S                   LIKE(artflp)
019000990305     D WCLIA           S                   LIKE(artflp)
019100990208     D WDCTLNPC        S                   LIKE(dctlnp)
019200990210     D WDEF            S                   LIKE(d53def)
019300990311     D WEXPDEF         S                   LIKE(d53def)
019400990210     D WPRECNPG        S                   LIKE(brvnpg)
019500990210     D WPRECFGS        S                   LIKE(brvfgs)
019600990210     D WPRECSPG        S                   LIKE(d53spg)
019700990304     D LNPFL1          S                   LIKE(orgfl1)
019800080903     D lnpntw          S                   LIKE(§ogntw)
019900150604     D lnantw          S                   LIKE(§ogntw)
020000131018     D lnpMAMntw       S                   LIKE(§ogntw)
020100990304     D LNAFL1          S                   LIKE(orgfl1)
020200100610     D lnptfp          S                   LIKE(i50tfp)
020300990210     D WPREC200        S             20  0
020400000322     D WSTESSOAS       S              1
020500060301     D Wautogen        S              1
020600000322     D WOK             S              1
020700000322     D JJ              S              3  0
020800990305     D WAPERT          S              1
020900050204     d wggmmaa         s              6
021000050204     d wgioset         s              1
021100090422     d soloCAN         s              1
021200131018     d Wes_mem         s              1
021300090303     d ktrd            s                   like(ar5trd) inz('GEN')
021400990129      *----------------------------------------------------------
021500990129      *  RIEPILOGO INDICATORI
021600980908      *----------------------------------------------------------
021700990204     C                   MOVEL     KPJBU         FIDN50DS
021800990209     C                   clear                   ofil
021900990209     C                   clear                   oper
022000990209     C                   clear                   otcr
022100051010     C                   clear                   Wricalcolo
022200131018     C                   clear                   Wes_mem
022300990128     c**
022400990128     c** C A L C O L O   L A   R E S P O N S A B I L I T A'
022500990128     c**
022600990129    1c     I50MOD        ifeq      'C'
022700990129     c                   clear                   o50por
022800990129     c                   clear                   o50per
022900990129     c                   clear                   o50car
023000990129     c                   clear                   o50tcr
023100990129     c                   clear                   o50msg
023200990129     c                   clear                   o50err
023300990128     c** chain in update la testata
023400990128     c     kdct          chain     fndct01l                           3099
023500990128     c* non trovata C.A.
023600990129    2c     *in30         ifeq      *on
023700990129     c                   movel     msg(2)        o50msg
023800990128     c                   movel     'E'           o50err
023900990128     c                   goto      fine
024000990129    2c                   endif
024100990128     c* record allocato: impossibile l'aggiornamento
024200990129    2c     *in99         ifeq      *on
024300990129     c                   movel     msg(3)        o50msg
024400990128     c                   movel     'E'           o50err
024500990128     c                   goto      fine
024600990129    2c                   endif
024700990129     c* IMPOSTO I COLLI DANNEGGIATI MAMCANTI: se manca il dato imposto
024800990129     c*   tutti i colli della bolla
024900990208    2c     dctncn        ifeq      0
025000990129     c     i50ncl        andeq     0
025100990208     c                   movel     msg(4)        o50msg
025200990129     c                   movel     'E'           o50err
025300990129     c                   goto      fine
025400990129     c***
025500990129   x2c                   else
025600990208    3c     dctncn        ifgt      0
025700990129     c                   z-add     dctncn        wncn
025800990129     c                   else
025900990129     c                   z-add     i50ncl        wncn
026000990129    3c                   endif
026100990129    2c                   endif
026200071018     c
026300071018     c* Conto quanti record ci sono nel file delle responsabilità
026400071018     c                   clear                   wcollidcr         6 2
026500071018     c     kdct          chain(N)  fndcr02l                           30
026600071018    2c     *in30         doweq     *off
026700071018     c     dcratb        ifeq      ' '
026800071018     c                   add       dcrper        wcollidcr
026900071018     c                   endif
027000071018     c
027100071018     c     kdct          reade(N)  fndcr02l                               30
027200071018    2c                   enddo
027300071018     c**
027400071018     c** Se si tratta di calcolo manuale controllo solo se devo adeguar
027500071018     c** il numero colli di DCR con DCD
027600071018     c                   if        dctcar='M'
027700071018     c                   if        wcollidcr<>wncn
027800071018     c                   exsr      AllineaDCRDCD
027900071018     c                   endif
028000071018     c
028100071018     c                   goto      FINE
028200071018     c                   endif
028300990129     c**
028400990129     c                   movel     dctflo        ddct01
028500990208     c                   clear                   wdctlnpc
028600990208     c     §dctlnpc      ifgt      *zeros
028700990208     c                   movel     §dctlnpc      wdctlnpc
028800990208     c                   endif
028900990304     c
029000050914     c* Verifico se lnp c.a. bolla mamma estera --> IMPORT
029100050914     C     i50lnpMAM     chain     azorg01l                           39
029200990304     c   39              clear                   orgfl1
029300080903     c   39              clear                   og143
029400990304     c                   eval      LNPFL1=orgfl1
029500131018     c                   eval      lnpMAMntw=§ogntw
029600131018
029700131018     c* Verifico ntw della bolla
029800131018     C     i50lnp        chain     azorg01l                           39
029900131018     c  n39              movel     orgde3        og143
030000131018     c   39              clear                   og143
030100131018     c                   eval      lnpntw=§ogntw
030200990304     c
030300990304     c* Verifico se lna estera --> EXPORT
030400990304     C     i50lna        chain     azorg01l                           39
030500990304     c   39              clear                   orgfl1
030600150604     c   39              clear                   og143
030700150604     c   39              clear                   orgde8
030800990308     c   39              clear                   orgfel
030900990304     c
031000150604     c  n39              movel     orgde3        og143
031100990304     c                   movel     orgde8        OG148
031200990304     c                   eval      LNAFL1=orgfl1
031300150604     c                   eval      LNAntw=§ogntw
031400990304     c**
031500990304     c* A seconda del tipo anomalia cerco la responsab.
031600990304     C                   clear                   tibs02ds
031700990304     C                   MOVEL     'C'           t02mod
031800990304     C                   MOVEL     knsif         t02sif
031900990304     C                   MOVEL     'TAD'         t02cod
032000990304     C                   MOVEL     dcttad        t02ke1
032100990304     C                   CALL      'TIBS02R'
032200990304     C                   PARM                    KPJBA
032300990304     C                   PARM                    TIBS02DS
032400990305     c
032500990305   1aC                   IF        t02err = *BLANKS
032600990305     C                   movel     T02uni        DTAD
032700090120
032800090120     c* chaino la bolla in partenza per prendere il cod trattamento merce
032900090120     c                   clear                   ds1b
033000090120     c                   clear                   blpctm
033100090120     c                   clear                   blpdsp
033200090120     c     kblp          chain     fnblp01l
033300090120     c                   if        not %found(fnblp01l)
033400090120     c     kblp          chain     fnarb01l
033500090120     c                   endif
033600090120     c                   if        blpctm<>*blanks
033700090120     c                   movel     '1B'          cod
033800090120     c                   movel     blpctm        key
033900090120     c     ktab          chain     tabel
034000090120     c                   if        %found(tabel00f)
034100090120     c                   movel     tbluni        ds1b
034200090120     c                   endif
034300090120     c                   endif
034400090302     c
034500090302     c* Verifico se bolla con merce di valore
034600090302     c     kar5          chain(N)  fiar501l
034700090302     c                   if        %found(fiar501l)
034800090302     c                   movel     ar5uni        dar5gen
034900090302     c                   else
035000090302     c                   clear                   dar5gen
035100090302     c                   endif
035200990305     c**
035300990305     c* 1) Se si tratta di evento : "SMARRIMENTO AUTOTRASPORTATORE"
035400990305     c** colpa di chi apre
035500990305    2c                   if        dctnev>0
035600990305     c     keve          chain     fndet01l                           34
035700990305    3c                   if        not *in34  AND
035800990305     c                             dettad='15'
035900990305    4c                   if        lnpfl1='E'
036000990305     c                   MOVEL(p)  'SMARRAUT_E'  wke1
036100990305   x4c                   else
036200990305     c                   MOVEL(p)  'SMARRAUT'    wke1
036300990305    4c                   endif
036400990308     c
036500990308     c                   z-add     wncn          wcollodan
036600990308     c                   clear                   wsegnadan
036700990305     c                   exsr      MEMRES
036800990305     c                   exsr      WRIRES
036900990305     c                   goto      fine
037000990305    3c                   endif
037100990305    2c                   endif
037200990308     c**
037300990308     c* 2) RESPONSABILITA' PARTNER
037400990305     c**
037500990308     c                   if        dctres='P'
037600990308     c                   if        lnpfl1='E'
037700990308     c                   MOVEL(p)  'COLPAPART_E' wke1
037800990308     c                   else
037900990308     c                   MOVEL(p)  'COLPAARRIVO' wke1
038000990308     c                   move      '_E  '        wke1
038100990308     c                   endif
038200990308     c
038300990308     c                   z-add     wncn          wcollodan
038400990308     c                   clear                   wsegnadan
038500990308     c                   exsr      MEMRES
038600990308     c                   exsr      WRIRES
038700990308     c                   goto      fine
038800990308     c                   endif
038900990308     c**
039000990308     c* 3) C.A. APERTA DALLA PARTENZA
039100990128     c**
039200000322     c* Se bolla locale lnp=lna ma franco senza riserva, dopo consegna,
039300000322     c*  come se fosse aperta lo stesso in partenza
039400050914    2c     i50fil        ifeq      i50lnpMAM
039500050914     c     i50lnpMAM     andeq     i50lna
039600000322     c     §tadtico      andeq     'C'
039700000322     c     dctcrc        andne     'S'
039800000322     c     §dctport      andeq     'F'
039900000322     c                   eval      wapert='P'
040000000323     c                   z-add     wncn          wcollodan
040100000323     c                   clear                   wsegnadan
040200000322     c                   exsr      CLIAPERTURA
040300000322     c                   exsr      MEMRES
040400000322     c                   exsr      WRIRES
040500000322     c                   goto      fine
040600000322     c                   endif
040700000322     c
040800050914     c* Verifico se il P.O. di apertura c.a. = a lnp bolla mamma se legata
040900050914    2c     i50fil        ifeq      i50lnpMAM
041000050914     c     i50lnpMAM     andne     i50lna
041100990304     c
041200990305    3c                   select
041300990304     c* 1) Spedizione consegnata con riserva
041400990304     c                   when      §tadtico='C'  AND
041500990304     c                             dctCRC='S'
041600990304     c
041700131018     c****               exsr      ERRAPERTURA
041800131021     c* Se si tratta di bolle dirottata lascio la responsab. errore di apertura
041900131021     c                   if        blpcca='1'
042000131021     c                   exsr      ERRAPERTURA
042100131021     c                   else
042200131021     c* altrimenti vado sulle spunte
042300131018     c                   exsr      elabora
042400131021     c                   endif
042500990304     c
042600990304     c* 2) Spedizione consegnata senza riserva
042700990304     c                   when      §tadtico='C'  AND
042800990304     c                             dctcrc<>'S'
042900990304     c* Assegnato--> errore
043000990305    4c                   if        §dctport='A'
043100990304     c                   exsr      ERRAPERTURA
043200990304     c                   else
043300990304     c* Franco --> Colpa del mittente
043400990305     c                   eval      wapert='P'
043500990304     c                   exsr      CLIAPERTURA
043600990305    4c                   endif
043700990304     c
043800990305     c* 3) Spedizione aperta in partenza ma non consegnata per i colli in CA
043900990305     c                   when      §tadtico<>'C'
044000990304     c
044100990305    4c                   if        lnpfl1='E'
044200990304     c                   MOVEL(p)  'APERTAPAR_E' wke1
044300990304     c                   else
044400990211     c                   MOVEL(p)  'APERTAPAR'   wke1
044500990305    4c                   endif
044600990305    3c                   endsl
044700990304     c
044800990129     c                   z-add     wncn          wcollodan
044900990216     c                   clear                   wsegnadan
045000990129     c**
045100131018     c                   if        wes_mem=' '
045200990129     c                   exsr      MEMRES
045300131018     c                   endif
045400990129     c**
045500990129     c     o50err        ifeq      'E'
045600990129     c                   goto      fine
045700990129     c                   endif
045800990129     c**
045900990129   x2c                   else
046000990305     c**
046100990305     c** Sono in ARRIVO: controllo solo le spedizioni consegnate senza RIS
046200990305     c
046300990305    3c                   if        §tadtico='C'  AND
046400990310     c                             dctcrc<>'S'   AND
046500990310    4c                             §dctport='A'
046600990305     c* Assegnato --> Colpa del mittente
046700990305     c                   eval      wapert='A'
046800990305     c                   exsr      CLIAPERTURA
046900990305     c
047000990308     c                   z-add     wncn          wcollodan
047100990308     c                   clear                   wsegnadan
047200990308     c
047300990305     c                   exsr      MEMRES
047400990305     c
047500990305   x3c                   else
047600130307     c*
047700130307     c                   exsr      elabora
047800990128     C**
047900990305    3c                   endif
048000990129    2c                   endif
048100990305  x1ac                   else
048200990305     c* ERRORE --> NON TROVATO TIPO ANOMALIA
048300990305     c                   movel     msg(1)        o50msg
048400990305     c                   goto      FINE
048500990305   1ac                   endif
048600990129    1c                   endif
048700990208      **
048800071018     c** Memorizzo la responsabilita'
048900990216     c                   if        o50err=' '
049000990208     c                   exsr      WRIRES
049100990216     c                   endif
049200990128     c     fine          tag
049300990128     c**
049400990208      * Chiudo pgm aperti
049500131018     c                   clear                   tibs02ds
049600990128     c                   movel     'C'           t02tla
049700990128     c                   call      'TIBS02R'
049800990128     c                   parm                    kpjba
049900990128     c                   parm                    tibs02ds
050000990208     c**
050100990208     c                   clear                   dslv53
050200990208     c                   movel     'C'           d53tla
050300990208     c                   call      'FNLV53R'
050400990208     c                   parm                    dslv53
050500990208     c**
050600990208     c                   clear                   dslv55
050700990208     c                   movel     'C'           d55tla
050800990208     c                   call      'FNLV55R'
050900990208     c                   parm                    dslv55
051000990128     c**
051100990212     C                   MOVEL     fidn50ds      kpjbu
051200980506     C                   EVAL      *INLR = *ON
051300130307      *****************************************************************
051400130307     c
051500130307     c     Elabora       BEGSR
051600130307     c**
051700130307     c** 3) Se non c'e' il dettaglio colli e non c'e' riserva in bolla
051800130307     c** --> non calcolo la responsabilita'
051900130307     c     kdct          chain     fndcd01l                           30
052000130307    4c     *in30         doweq     *off
052100130307     c     dcdatb        andne     ' '
052200130307     c     kdct          reade     fndcd01l                               30
052300130307    4c                   enddo
052400130307     c**
052500130307    4c     *in30         ifeq      *on
052600130307     c                   z-add     wncn          wcollodan
052700130307     c                   clear                   wsegnadan
052800130307     c                   exsr      CT_RISERVA
052900130307     c     o50err        ifeq      *blanks
053000130307     c                   exsr      WRIRES
053100130307     c                   endif
053200130307     c                   goto      fine
053300130307    4c                   endif
053400130307     c***
053500130307     c* leggo fndcd per MANCANZE PER vedere se hanno aperto in ritardo la CA
053600130307     c                   clear                   writ
053700130307    4c     §tadragr      ifeq      'M'
053800130307     c     lnafl1        andne     'E'
053900130307     c
054000130307     c     kdct          chain     fndcd01l                           30
054100130307    5c     *in30         doweq     *off
054200130307     c     writ          andne     'N'
054300130307     c** escludo se annullata e
054400130307    6c     dcdatb        ifeq      ' '
054500130307     c     dcddch        andeq     dctdch
054600130307     c*
054700130307     c* non trovato dett.bolla --> errore
054800130307     c     kdcd          chain     fnart27l                           34
054900130307     c*
055000130307    7c     *in34         ifeq      *on
055100130307     c                   movel     msg(7)        o50msg
055200130307     c                   movel     'E'           o50err
055300130307     c                   goto      fine
055400130307    7c                   endif
055500130307     c**
055600130307     c                   eval      wcollodan=1
055700130307     c                   movel     dcdnsc        wsegnadan         7
055800130307     c
055900130307     c                   exsr      RITARDO
056000130307    6c                   endif
056100130307     c**
056200130307     c     kdct          reade     fndcd01l                               30
056300130307    5c                   enddo
056400130307    4c                   endif
056500130307      *
056600130307     c***
056700130307     c* leggo fndcd
056800130307     c     kdct          chain     fndcd01l                           30
056900130307    5c     *in30         doweq     *off
057000130307     c** escludo se annullata e
057100130307    6c     dcdatb        ifeq      ' '
057200130307     c     dcddch        andeq     dctdch
057300130307     c*
057400130307     c* non trovato dett.bolla --> errore
057500130307     c     kdcd          chain     fnart27l                           34
057600130307     c*
057700130307    7c     *in34         ifeq      *on
057800130307     c                   movel     msg(7)        o50msg
057900130307     c                   movel     'E'           o50err
058000130307     c                   goto      fine
058100130307    7c                   endif
058200130307     c**
058300130307     c                   eval      wcollodan=1
058400130307     c                   movel     dcdnsc        wsegnadan         7
058500130307     c
058600130307    7c     §tadragr      ifeq      'M'
058700130307     c                   exsr      MANCANZE
058800130307     c                   else
058900130307     c*
059000130307     c                   exsr      AMMAVARIE
059100130307    7c                   endif
059200130307    6c                   endif
059300130307     c**
059400130307     c     kdct          reade     fndcd01l                               30
059500130307    5c                   enddo
059600130307     c                   ENDSR
059700070711      *****************************************************************
059800070711      * Verifico se doppia spunta per ammanchi/avarie
059900070711      *****************************************************************
060000070711     c     CTRDOPSPU     begsr
060100080714     c* non effettuo il controllo se sonon in entrata e si tratta di
060200080714     c**  bolla con disk c
060300081021     c                   if        wnpgspuan<>5 or §1bdkc=' '
060400070711     c
060500070711     c     kbrve         chain     fnbrve1l
060600070711    1c                   if        %found(fnbrve1l) and E_brvcan=' '
060700070711     c
060800070713     c                   eval      savwke1=wke1
060900070711     c**
061000070711     c                   clear                   wsif
061100070711     c                   clear                   wlin
061200070711     c                   eval      wcod='TRS'
061300070711     c                   eval      wke2='99991231       '
061400070821     c                   eval      wke1='A_CAN_DOPARR   '
061500070711     c
061600070711     c                   clear                   WOK
061700070711     c     ktbe          setgt     tntbe02l
061800070711     c     ktbe2         readpe    tntbe02l                               39
061900070711    2c                   dow       not *in39  and  WOK<>'S'
062000070711     c                   if        tbeatb=' '
062100070711     c                   movel     tbeke2        w0080             8 0
062200070711     c     w0080         ifle      dctdca
062300070711     c                   eval      wok='S'
062400070711     c                   seton                                        39
062500070711     c                   endif
062600070711     c                   endif
062700070711     c
062800070711     c  N39ktbe2         readpe    tntbe02l                               39
062900070711    2c                   enddo
063000070711     c**
063100070711     c**
063200070711     c** Non trovato record in tabella responsabilita'
063300070711     c** procedo con l'attuale calcolo della responsabilità
063400070711    2C                   IF        wok=' '
063500070713     c                   eval      wke1=savwke1
063600070711   x2c                   else
063700070711     c* vado avanti con il nuovo codice di responsabilità
063800070711     c                   movel     tbeuni        DTRS
063900070713     c* In WFLP memorizzo il p.o. che ha rilevato l'anomalia
064000070821     c                   eval      wflp=wfgsspuan
064100090831     c                   movel     'A'           wtipoflp
064200070711    2c                   endif
064300070711    1c                   endif
064400070711     c
064500080714     c                   endif
064600070711     c                   ENDSR
064700990129      *****************************************************************
064800990129      * Cerco record e memorizzo la responsabilita'
064900990129      *****************************************************************
065000990129     c     MEMRES        begsr
065100990217     C                   clear                   wpor
065200130513     C                   clear                   areater
065300131018     c                   eval      wes_mem=' '
065400990304     c**
065500990304     c                   clear                   wsif
065600990304     c                   clear                   wlin
065700990304     c                   eval      wcod='TRS'
065800990304     c                   eval      wke2='99991231       '
065900990304     c
066000990304     c                   clear                   WOK
066100990304     c     ktbe          setgt     tntbe02l
066200990304     c     ktbe2         readpe    tntbe02l                               39
066300990304     c                   dow       not *in39  and  WOK<>'S'
066400990304     c                   if        tbeatb=' '
066500990304     c                   movel     tbeke2        w0080             8 0
066600990304     c     w0080         ifle      dctdca
066700990304     c                   eval      wok='S'
066800990308     c                   seton                                        39
066900990304     c                   endif
067000990304     c                   endif
067100990304     c
067200990308     c  N39ktbe2         readpe    tntbe02l                               39
067300990304     c                   enddo
067400990129     c**
067500990129     c**
067600990129     c** Non trovato record in tabella responsabilita'
067700070711    1C                   IF        wok=' '
067800990208     c                   movel     msg(5)        o50msg
067900990129     c                   movel     'E'           o50err
068000070711   x1c                   else
068100990129     c*
068200990304     c                   movel     tbeuni        dtrs
068300070711     c* Se si tratta do codice che controlla se "fa il furbo", per
068400070711     c*  ammanchi e avarie, efettuo il controllo
068500070711     c*  se già in linea il nuovo codice atribuzione reponsabilità
068600070711     c                   if        §trsdopspu='S'
068700070711     c                   exsr      CTRDOPSPU
068800070711     c                   endif
068900100609
069000100610     c* Se il codice prevede differenziazione responsab. tra Franchi / Assegnati / triangolazioni
069100100610     c                   clear                   respo
069200100610     c                   clear                   respc
069300100610     c
069400100610     c*  imposto la tabella per triangolazioni se c'e' e se lo  la bolla
069500100610    2c                   if        respot(1) <>' '
069600100610     c
069700100610     c* Per franchi
069800100610    3c                   if        §dctport='F'
069900100610     c* Terminal di partenza lnp C.A. e terminal di partenza lin ksc a confronto
070000100610     c                   clear                   dslv55
070100100610     c                   movel     'P'           d55tpt
070200100610     c                   z-add     wdctlnpc      d55lin
070300100610     c                   if        blpdsp>0
070400100610     c                   z-add     blpdsp        d55drf
070500100610     c                   else
070600100610     c                   z-add     dctdca        d55drf
070700100610     c                   endif
070800100610     c                   call      'FNLV55R'
070900100610     c                   parm                    dslv55
071000100610     c                   movel     d55tfp        lnptfp
071100100610     c*
071200100610     c                   z-add     wclip         d55lin
071300100610     c                   call      'FNLV55R'
071400100610     c                   parm                    dslv55
071500100610    4c                   if        d55tfp<>lnptfp
071600100610     c                   movea     respot        respo
071700100610     c                   movea     respct        respc
071800130513     c
071900130513     c*Se sono diversi devo verificare che anche le rispettive aree di appartenenza
072000130513     c*  siano diverse
072100130513     C     lnptfp        chain     azorg01l                           39
072200130513    5c                   if        %found(azorg01l)
072300130513     c                   eval      areater=orgcar
072400130513     C     d55tfp        chain     azorg01l                           39
072500130513    6c                   if        %found(azorg01l) and orgcar<>areater
072600130513     c                   movea     respot        respo
072700130513     c                   movea     respct        respc
072800130513    6c                   endif
072900130513    5c                   endif
073000100610    4c                   endif
073100100610     c
073200100610   x3c                   else
073300100610     c                   clear                   dslv55
073400100610     c                   movel     'A'           d55tpt
073500100610     c                   z-add     wclia         d55lin
073600100610     c                   if        blpdsp>0
073700100610     c                   z-add     blpdsp        d55drf
073800100610     c                   else
073900100610     c                   z-add     dctdca        d55drf
074000100610     c                   endif
074100100610     c                   call      'FNLV55R'
074200100610     c                   parm                    dslv55
074300100610     c*
074400100610    4c                   if        i50tfa<>d55tfa
074500130513     c*Se sono diversi devo verificare che anche le rispettive aree di appartenenza
074600130513     c*  siano diverse
074700130513     C     i50tfa        chain     azorg01l                           39
074800130513    5c                   if        %found(azorg01l)
074900130513     c                   eval      areater=orgcar
075000130513     C     d55tfa        chain     azorg01l                           39
075100130513    6c                   if        %found(azorg01l) and orgcar<>areater
075200100610     c                   movea     respot        respo
075300100610     c                   movea     respct        respc
075400130513    6c                   endif
075500130513    5c                   endif
075600130513    4c                   endif
075700130513     c
075800100610    3c                   endif
075900100610    2c                   endif
076000100610     c
076100100610    2c                   if        respo(1) = ' '
076200100610     c* Per Assegnati
076300100610     c* Terminal di arrivo lna C.A. e terminal di arrivo lin ksc a confronto
076400100610     c
076500100610     c*  imposto le tabelle di fil e codici giuste, altrimenti la prima (quella dei franchi)
076600100610    3c                   if        respoa(1) <>' ' and §dctport='A'
076700100609     c                   movea     respoa        respo
076800100609     c                   movea     respca        respc
076900100609     c                   else
077000100609     c                   movea     respof        respo
077100100609     c                   movea     respcf        respc
077200100610    3c                   endif
077300100610    2c                   endif
077400990129     c*
077500990218     c                   z-add     1             xx                4 0
077600070711    2c     respo(xx)     downe     *blanks
077700040907     c*
077800040907     c                   if        respo(xx) = 'V' and wnfv=0
077900040907     c                   eval      respo(xx) = 'A'
078000040907     c                   endif
078100990303     c
078200990129     c                   select
078300990308     c** PO e' "F" --> terminal partenza p.o. apertura c.a.
078400990303     c
078500990208     c     respo(xx)     wheneq    'F'
078600990303     c                   clear                   dslv55
078700990303     c                   movel     'P'           d55tpt
078800990303     c                   z-add     dctfil        d55lin
078900090325     c                   if        blpdsp>0
079000090325     c                   z-add     blpdsp        d55drf
079100090325     c                   else
079200090325     c                   z-add     dctdca        d55drf
079300090325     c                   endif
079400990303     c                   call      'FNLV55R'
079500990303     c                   parm                    dslv55
079600990303     c                   z-add     d55tfp        wpor
079700090324     c                   z-add     d55tfp        wter
079800990303     c
079900990129     c* se = a lnp dct e' P altrimenti e' "T"
080000990308     c     dctfil        ifeq      wdctlnpc
080100990129     c                   movel     'P'           wtcr
080200990308     c                   else
080300990308     c     dctfil        ifeq      dctlna
080400990308     c                   movel     'A'           wtcr
080500990129     c                   else
080600990129     c                   movel     'T'           wtcr
080700990129     c                   endif
080800990308     c                   endif
080900990303     c
081000990308     c** PO e' "C" --> p.o. apertura c.a.
081100990308     c     respo(xx)     wheneq    'C'
081200990308     c                   z-add     dctfil        wpor
081300090831     c                   clear                   wtipoflp
081400990308     c
081500990308     c* se = a lnp dct e' P altrimenti e' "T"
081600990308     c     dctfil        ifeq      wdctlnpc
081700990308     c                   movel     'P'           wtcr
081800990308     c                   else
081900990308     c     dctfil        ifeq      dctlna
082000990308     c                   movel     'A'           wtcr
082100990308     c                   else
082200990308     c                   movel     'T'           wtcr
082300990308     c                   endif
082400990308     c                   endif
082500090324     c
082600090324     c                   EXSR      CallTER
082700990308     c
082800090120     c** PO e' "P" --> teminal partenza di lnp bolla originale
082900990208     c     respo(xx)     wheneq    'P'
083000990303     c                   clear                   dslv55
083100990303     c                   movel     'P'           d55tpt
083200990303     c                   z-add     wdctlnpc      d55lin
083300090120     c                   if        blpdsp>0
083400090120     c                   z-add     blpdsp        d55drf
083500090120     c                   else
083600090120     c                   z-add     dctdca        d55drf
083700090120     c                   endif
083800990303     c                   call      'FNLV55R'
083900990303     c                   parm                    dslv55
084000990303     c                   z-add     d55tfp        wpor
084100990129     c                   movel     'P'           wtcr
084200090324     c                   z-add     d55tfp        wter
084300131018
084400131018     c** PO e' "O" -->  lnp bolla originale
084500131018     c     respo(xx)     wheneq    'O'
084600131018     c                   clear                   dslv55
084700131018     c                   movel     'P'           d55tpt
084800131018     c                   z-add     wdctlnpc      d55lin
084900131018     c                   if        blpdsp>0
085000131018     c                   z-add     blpdsp        d55drf
085100131018     c                   else
085200131018     c                   z-add     dctdca        d55drf
085300131018     c                   endif
085400131018     c                   call      'FNLV55R'
085500131018     c                   parm                    dslv55
085600131018     c                   z-add     wdctlnpc      wpor
085700131018     c                   movel     'P'           wtcr
085800131018     c                   z-add     d55tfp        wter
085900990303     c
086000090324     c** PO e' "1" --> terminal partenza del codice cliente mitt o dest
086100990305     c     respo(xx)     wheneq    '1'
086200990305     c                   clear                   dslv55
086300990305     c                   movel     'P'           d55tpt
086400990305     c                   if        wclip>0
086500990305     c                   z-add     wclip         d55lin
086600990305     c                   movel     'P'           wtcr
086700990305     c                   else
086800990305     c                   z-add     wclia         d55lin
086900990305     c                   movel     'A'           wtcr
087000990305     c                   endif
087100990305     c
087200090325     c                   if        blpdsp>0
087300090325     c                   z-add     blpdsp        d55drf
087400090325     c                   else
087500090325     c                   z-add     dctdca        d55drf
087600090325     c                   endif
087700990305     c                   call      'FNLV55R'
087800990305     c                   parm                    dslv55
087900990305     c                   z-add     d55tfp        wpor
088000090324     c                   z-add     d55tfp        wter
088100990305     c
088200990309     c** PO e' "2" --> p.o. codice cliente mitt o dest
088300990309     c     respo(xx)     wheneq    '2'
088400990309     c                   if        wclip>0
088500990309     c                   z-add     wclip         wpor
088600990309     c                   movel     'P'           wtcr
088700990309     c                   else
088800990309     c                   z-add     wclia         wpor
088900990309     c                   movel     'A'           wtcr
089000990309     c                   endif
089100090831     c
089200090831     c                   clear                   wtipoflp
089300090324     c                   exsr      CallTER
089400990309     c
089500990303     c** PO e' "A" --> terminal arrivo di lna bolla
089600990208     c     respo(xx)     wheneq    'A'
089700000321     c                   if        flptfa>0
089800000321     c                   movel     flptfa        wpor
089900000321     c                   else
090000990205     c                   movel     i50tfa        wpor
090100000321     c                   endif
090200990129     c                   movel     'A'           wtcr
090300090324     c                   movel     wpor          wter
090400990303     c
090500990303     c** PO e' "R" --> terminal partenza di lna bolla
090600990303     c     respo(xx)     wheneq    'R'
090700990303     c                   clear                   dslv55
090800990303     c                   movel     'P'           d55tpt
090900990303     c                   z-add     i50lna        d55lin
091000090325     c                   if        blpdsp>0
091100090325     c                   z-add     blpdsp        d55drf
091200090325     c                   else
091300090325     c                   z-add     dctdca        d55drf
091400090325     c                   endif
091500990303     c                   call      'FNLV55R'
091600990303     c                   parm                    dslv55
091700990303     c                   z-add     d55tfp        wpor
091800990303     c                   movel     'A'           wtcr
091900090324     c                   z-add     d55tfp        wter
092000990309     c
092100990309     c** PO e' "S" --> terminal partenza di p.o. disguido
092200990309     c     respo(xx)     wheneq    'S'
092300990309     c                   clear                   dslv55
092400990309     c                   movel     'P'           d55tpt
092500990309     c                   z-add     wfgs          d55lin
092600090325     c                   if        blpdsp>0
092700090325     c                   z-add     blpdsp        d55drf
092800090325     c                   else
092900090325     c                   z-add     dctdca        d55drf
093000090325     c                   endif
093100990309     c                   call      'FNLV55R'
093200990309     c                   parm                    dslv55
093300990309     c                   z-add     d55tfp        wpor
093400990309     c                   movel     'T'           wtcr
093500090324     c                   z-add     d55tfp        wter
093600990309     c
093700990304     c** PO e' "X" --> linea di arrivo  bolla
093800990308     c     respo(xx)     wheneq    'X'
093900990304     c                   movel     i50lna        wpor
094000990304     c                   movel     'A'           wtcr
094100090324     c                   movel     i50tfa        wter
094200990303     c
094300070713     c** PO e' "T" --> P.O. passato in wflp
094400990208     c     respo(xx)     wheneq    'T'
094500060301     c                   movel     wflp          wpor
094600070713     c                   select
094700070713     c                   when      wflp=i50tfp or wflp=i50lnp
094800060301     c                   movel     'P'           wtcr
094900090324     c                   movel     i50tfp        wter
095000070713     c                   when      wflp=i50tfa or wflp=i50lna
095100070713     c                   movel     'A'           wtcr
095200090324     c                   movel     i50tfa        wter
095300070713     c                   other
095400990129     c                   movel     'T'           wtcr
095500090324     c                   exsr      CallTER
095600070713     c                   endsl
095700081204     c
095800081204     c** PO e' "N" --> P.O. passato in wfgsspuan
095900081204     c     respo(xx)     wheneq    'N'
096000081204     c                   movel     wfgsspuan     wpor
096100081204     c                   select
096200081204     c                   when      wfgsspuan=i50tfp or wfgsspuan=i50lnp
096300081204     c                   movel     'P'           wtcr
096400090324     c                   movel     i50tfp        wter
096500081204     c                   when      wfgsspuan=i50tfa or wfgsspuan=i50lna
096600081204     c                   movel     'A'           wtcr
096700090324     c                   movel     i50tfa        wter
096800081204     c                   other
096900081204     c                   movel     'T'           wtcr
097000090324     c                   exsr      CallTER
097100081204     c                   endsl
097200990303     c
097300990304     c** PO e' "L" --> terminal partenza linea partenza bolla
097400990208     c     respo(xx)     wheneq    'L'
097500990205     c                   movel     i50tfp        wpor
097600090324     c                   movel     i50tfp        wter
097700990129     c* se = a lnp dct e' P altrimenti e' "T"
097800990208     c     i50lnp        ifeq      wdctlnpc
097900990129     c                   movel     'P'           wtcr
098000990129     c                   else
098100990129     c                   movel     'T'           wtcr
098200990129     c                   endif
098300990304     c
098400990304     c** PO e' "E" --> Linea partenza bolla
098500990304     c     respo(xx)     wheneq    'E'
098600990304     c                   movel     i50lnp        wpor
098700090324     c                   movel     i50tfp        wter
098800990304     c* se = a lnp dct e' P altrimenti e' "T"
098900990304     c     i50lnp        ifeq      wdctlnpc
099000990304     c                   movel     'P'           wtcr
099100990304     c                   else
099200990304     c                   movel     'T'           wtcr
099300990304     c                   endif
099400040907     c
099500040907     c** PO e' "V" --> p.o. scarico foglio viaggio
099600040907     c     respo(xx)     wheneq    'V'
099700040907     c     kfgv          chain     fnfgv01l
099800040907    1c                   if        %found(fnfgv01l)
099900040907    2c                   if        i50lna=fgvlna
100000040907     c                   movel     i50lna        wpor
100100040907     c                   movel     'A'           wtcr
100200090324     c                   movel     i50tfa        wter
100300040907   x2c                   else
100400040907     c     kfgv          chain     fnfgw01l
100500040907if  3c                   if        (not %found(FNFGW01L)
100600040907     c                              or  FGWatb <> *blanks)
100700040907     C                   clear                   FNFGW000
100800040907e   3C                   endif
100900040907     C                   MOVEL     FGWFF3        W1FF3
101000040907     C                   MOVEL     FGWFF4        W1FF4
101100040907     C                   MOVEL     FGWFL3        W1FL3
101200040907     C                   MOVEL     FGWFL4        W1FL4
101300040907     c*
101400040907     c                   z-add     1             yy
101500040907     c                   movel     i50lna        w003a             3
101600040907     c     w003a         lookup    ffv(yy)                                39
101700040907    3c                   if        *in39
101800040907     c                   movel     flp(yy)       wpor
101900040907    4c                   if        wpor=i50tfa
102000040907     c                   movel     'A'           wtcr
102100090324     c                   movel     i50tfa        wter
102200040907     c                   else
102300040907     c                   movel     'T'           wtcr
102400090828     C** SE TRANSITO, la filiale dis carico è sempre un terminal per cui
102500090828     c*  non importa calcolarlo: prendo flp(yy)
102600090828     c                   movel     flp(yy)       wter
102700090828     c***                exsr      CallTER
102800040907    4c                   endif
102900040907    3c                   endif
103000040907    2c                   endif
103100040907    1c                   endif
103200990304     c
103300990129     c                   endsl
103400090325     c
103500090325     c* Se p.o. estero--> terminal = se stesso perchè la responsab
103600090325     c*  deve andare al distretto internazionale
103700090325     C     wpor          chain     azorg01l                           39
103800090325     c  n39              movel     orgde3        og143
103900090325     c   39              clear                   og143
104000090325     c*
104100090325     c                   if        §ogntw='EEX' or  §ogntW= 'DPD' or  §ogntw
104200090325     c                             = 'FED'
104300090325     c                   eval      wter=wpor
104400090325     c                   endif
104500090325     c
104600990304     c** CERCO nella skiera se l'ho gia' impostato
104700990216     c*  p.o. + segnacollo
104800990129     c                   z-add     1             yy
104900990217     c                   if        wpor=0
105000090324     c                   clear                   w013a
105100090324     c                   movel     '000'         w013a
105200090324     c                   move      '000'         w013a
105300990217     c                   else
105400090324     c                   movel     wokey         w013a
105500990217     c                   endif
105600990217     c
105700090324     c     w013a         lookup    ofil(yy)                               31
105800990129     c     *in31         ifeq      *off
105900990129     c                   z-add     1             yy
106000990216     c
106100090324     c                   clear                   w013a            13
106200090324     c     w013a         lookup    ofil(yy)                               31
106300990129     c                   endif
106400990129     c**
106500990216     c                   movel     wokey         ofil(yy)
106600990208     c                   movel     wtcr          otcr(yy)
106700990916     c
106800990916     c** Responsabilita' per ammanchi e mancanze o anche per avarie se
106900990916     c**  non e' impostato ilsuo codic eresponsabilita'
107000990916     c                   if        §tadragr<>'V' or §trstrev=*blanks
107100990304     c                   movel     §trstre       otre(yy)
107200990916     c                   else
107300990916     c                   movel     §trstrev      otre(yy)
107400990916     c                   endif
107500090303     c* Se colli di valore la moltiplico in base a tabella STD
107600090303     c* Verifico  se il p.o. responsabile è estero: in questo caso non triplico
107700090325     c*  nemmeno per avarie
107800090303     c                   eval      percen=respc(xx)
107900090303     c
108000090303     c                   if        §ar5bva<>' '
108100090303     c                   if        §ogntw<>'EEX' and §ogntW<>'DPD' and §ogntw
108200090325     c                             <>'FED'  and §tadragr<>'V'
108300090303     c                   eval      percen=respc(xx)*§stdrsval
108400090303     c                   endif
108500090303     c                   endif
108600090303     c
108700990129     c** calcolo la % di reponsabilita' sui colli passati per questo calcol
108800090303     c                   eval(h)   Wper=((percen   *wcollodan)/100)
108900990209     c                   add       wper          oper(YY)
109000990129     c
109100990129     c                   add       1             xx
109200070711    2c                   enddo
109300070711    1c                   endif
109400000321     c
109500000321     c                   clear                   flptfa
109600131018     c                   eval      wes_mem='S'
109700990129     c                   ENDSR
109800090324      *****************************************************************
109900090324      * Determino terminal p.o. responsabile
110000090324      *****************************************************************
110100090324     c     CallTER       BEGSR
110200090324     c                   clear                   dslv55
110300090831     c
110400090831     c* Se non ho impostato il tipo di wflp per transito imposto
110500090831     c*    sempre "A"
110600090831     c                   if        wtipoflp=' '
110700090831     c
110800090324     c                   if        wtcr='P'
110900090324     c                   movel     'P'           d55tpt
111000090324     c                   else
111100090324     c                   movel     'A'           d55tpt
111200090324     c                   endif
111300090831     c
111400090831     c                   else
111500090831     c                   movel     wtipoflp      d55tpt
111600090831     c                   endif
111700090831     c
111800090831     c                   if        d55tpt='A'
111900090831     c                   movel     i50lnp        d55lnp
112000090831     c                   endif
112100090324     c                   z-add     wpor          d55lin
112200090325     c                   if        blpdsp>0
112300090325     c                   z-add     blpdsp        d55drf
112400090325     c                   else
112500090325     c                   z-add     dctdca        d55drf
112600090325     c                   endif
112700090324     c                   call      'FNLV55R'
112800090324     c                   parm                    dslv55
112900090324     c* se errore calcolo terminal a oggi
113000090324     c                   if        d55err<>' '
113100090324     c                   z-add     dateu         d55drf
113200090324     c                   call      'FNLV55R'
113300090324     c                   parm                    dslv55
113400090324     c                   endif
113500090324     c
113600090831     c                   if        D55TPT='P'
113700090324     c                   z-add     d55tfp        wter
113800090324     c                   else
113900090324     c                   z-add     d55tfa        wter
114000090324     c                   endif
114100090324     c                   ENDSR
114200990304      *****************************************************************
114300990304      * Causali di calcolo per errore in apertura partenza
114400990304      *****************************************************************
114500990304     c     CLIAPERTURA   begsr
114600990305     c                   clear                   wclip
114700990305     c                   clear                   wclia
114800990305     c
114900100216     c* Divido i codici respons. tra ammanchi, avarie e mancanze per questo
115000100216     c*  tipo di responsabilità
115100090102    1c                   if        §tadragr='M'
115200090102     c                   eval      wke1='APERTAPARCLIM  '
115300090102   x1c                   else
115400090102     c
115500090102     c* Per avarie e ammanchi cambia dal 7/1/09
115600100216     c*  cambia ancora dividendo la responsabilità tra ammanchi e avarie
115700100216    2c***                if        lnpfl1='E'
115800100216     c***                eval      wke1='APERTAPARCLI   '
115900100216     C***                else
116000100216     c
116100100216    3c                   if        §tadragr='A'
116200100216     c                   eval      wke1='APERTAPARCLIA  '
116300100216     c                   else
116400100216     c                   eval      wke1='APERTAPARCLIV  '
116500100216    3c                   endif
116600100216    2c****               endif
116700100216    1c                   endif
116800990305     c
116900990305     c* Essendo in PARTENZA linea del cliente mittente, se non c'e'
117000990305     c*  linea partenza bolla
117100100216    3c                   if        wapert='P'
117200990305     c                   if        i50ksm<>0
117300990305     c                   movel     i50ksm        wclip
117400990305     c                   else
117500990305     c                   eval      wclip=i50lnp
117600990305     c                   endif
117700100216    3c                   endif
117800990305     c
117900990305     c* Essendo in ARRIVO   linea del cliente destinatario, se non c'e'
118000990305     c*  linea arrivo bolla
118100100216    3c                   if        wapert='A'
118200990305     c                   if        i50ksd<>0
118300990305     c                   movel     i50ksd        wclia
118400990305     c                   else
118500990305     c                   eval      wclia=i50lna
118600990305     c                   endif
118700990305     c                   endif
118800990304     c                   endsr
118900990304      *****************************************************************
119000990304      * Causali di calcolo per errore in apertura partenza
119100990304      *****************************************************************
119200990304     c     ERRAPERTURA   begsr
119300990304     c                   if        lnpfl1='E'
119400990308     c                   eval      wke1='APERTAPARERR_E '
119500990304     c                   else
119600990308     c                   eval      wke1='APERTAPARERR   '
119700990304     c                   endif
119800990304     c                   endsr
119900990129      *****************************************************************
120000990129      * Scrivo la responsabilita'
120100990129      *****************************************************************
120200990129     c     WRIRES        begsr
120300051010     c                   clear                   WNoscrivi
120400051010     c* se non ho più le spunte e mi servono per il calcolo della reponsab
120500051010     c*  non ricalcolo se c'e già (calcolo solo se è la prima volta)
120600051010     c
120700990209     c**
120800990209     c** cancello tutti i record presenti in fndcr
120900990216     c     kdct          chain     fndcr02l                           30
121000071018    1c     *in30         doweq     *off
121100990209     c     dcratb        ifeq      ' '
121200990209     c                   movel     'A'           dcratb
121300051010     c***                clear                   dcrftr
121400051010     c                   movel     dateu         dcrdtr
121500990209     c                   endif
121600051010     c* Solo se devo ricalcolare
121700051010     c                   if        WRicalcolo<>'N'
121800990209     c                   update    fndcr000
121900051010     c                   else
122000051010     c                   unlock    fndcr02l
122100051010     c                   movel     'N'           WNoscrivi         1
122200051010     c                   endif
122300990209     c**
122400990216     c     kdct          reade     fndcr02l                               30
122500071018    1c                   enddo
122600990209     c**
122700990129     c** scrivo i file e la ds di output
122800071018    1c                   if        WNoscrivi<>'N'
122900990129     c                   z-add     1             xx
123000071018    2c     ofil(xx)      downe     *blanks
123100990129     c     xx            ifeq      1
123200990129     c* DS
123300090324     c                   movel     ofil(xx)      o50por
123400990129     c                   z-add     oper(xx)      o50per
123500990129     c                   movel     otcr(xx)      o50tcr
123600990129     c                   endif
123700090324     c
123800090324     c                   movel     ofil(xx)      wokey
123900990129     c**
124000990216     c
124100990216     c     kdcr          chain     fndcr02l                           30
124200990129     c                   clear                   fndcr000
124300990129     c                   movel     dctaac        dcraac
124400990129     c                   movel     dctfil        dcrfil
124500990129     c                   movel     dctnca        dcrnca
124600990129     c                   z-add     oper(xx)      dcrper
124700990129     c                   movel     otcr(xx)      dcrtcr
124800090324     c                   movel     wpor          dcrpor
124900090324     c                   movel     wter          §dcrter
125000090324     c                   move      wsegnadan     §dcrnsc
125100090324     c                   movel     otre(xx)      §dcrtre
125200170413     c
125300170502     c                   if        writardo='S' and dctdca> §vpoca
125400170413     c                   eval      §dcrf1='R'
125500170413     c                   else
125600170413     c                   clear                   §dcrf1
125700170413     c                   endif
125800170413     c
125900990216     c                   movel(p)  ddcr01        dcrflo
126000051010     c                   movel     dateu         dcrdtr
126100990129     c**
126200990129     c  n30              update    fndcr000
126300990129     c   30              write     fndcr000
126400990129     c**
126500990129     c                   add       1             xx
126600071018    2c                   enddo
126700071018   x1c                   else
126800071018     c
126900071018     c* Se i colli in DCR non corrispondono più a quella della C.A.
127000071018     c* tengo i colli predsenti in DCD e basta
127100071018    2c                   if        wncn<>wcollidcr
127200071018     c                   EXSR      AllineaDCRDCD
127300071018    2C                   ENDIF
127400071018     c
127500071018    1c                   endif
127600990129     c**
127700990129     c                   movel     'A'           dctcar
127800051010     c                   movel     'A'           o50car
127900990129     c                   update    fndct000
128000051010     c                   clear                   WRicalcolo
128100051010     c                   clear                   WNoscrivi
128200990129     c                   ENDSR
128300071018      *****************************************************************
128400071018      * Sistemo DCD allineato a DCR
128500071018      *****************************************************************
128600071018     c     AllineaDCRDCD BEGSR
128700071018     c
128800071018     c     kdct          chain     fndcr02l                           30
128900071018    3c     *in30         doweq     *off
129000071018    4c                   if        dcratb=' ' and dcrseg>*zeros
129100071018     c                   movel     dcrseg        w0070             7 0
129200071018     c     kdcr2         chain     fndcd01l
129300071018    5c                   if        not %found(fndcd01l) or dcdatb<>' '
129400071018     c                   movel     'A'           dcratb
129500071018     c                   movel     dateu         dcrdtr
129600071018     c                   update    fndcr000
129700071018    5c                   endif
129800071018    4c                   endif
129900071018     c
130000071018     c* Se non c'e' il segnacollo ed inserito solo 1 record,
130100071018     c*  sistemo altrimenti non ci riesco
130200071018    4c                   if        dcratb=' ' and dcrseg<=*zeros
130300071018     c                             and dcrper=wcollidcr
130400071018     c                   z-add     wncn          dcrper
130500071018     c                   movel     dateu         dcrdtr
130600071018     c                   update    fndcr000
130700071018    4c                   endif
130800071018     c
130900071018     c     kdct          reade     fndcr02l                               30
131000071018    3c                   enddo
131100071018     c                   ENDSR
131200990129      *****************************************************************
131300990129      * controllo se c'e' riserva in bolla in mancanza di dett CA o spunte
131400990129      *****************************************************************
131500990208     c     CT_RISERVA    begsr
131600990129     c                   clear                   wriserva          1
131700990129     c**
131800990204     c     dctcrc        ifeq      'S'
131900990211     c                   MOVEL(p)  'RISERVANODET'wke1
132000990129     c                   else
132100990129     C** 3) NON C'E' DETT.C.A. NON C'E'RISERVA --> non posso calcolare
132200990129     c*     NON C'E' SPUNTA    NON C'E'RISERVA --> non posso calcolare
132300990217     c                   MOVEL(p)  'NOCALCOLO'   wke1
132400051010     c                   movel     'N'           WRicalcolo        1
132500990129    2c                   endif
132600990217     c
132700990217     c                   exsr      MEMRES
132800990129     c                   ENDSR
132900990329      *****************************************************************
133000990329      * RESPONSABILITA' PER RITARDO APETURA C:A: MANCANZE
133100990329      *****************************************************************
133200990329     c     RITARDO       begsr
133300990329     c                   clear                   wsavdao
133400990329     c                   clear                   wsavcch
133500990329     c                   clear                   wsavdch
133600990329     c
133700990329     c* tutti i colli devono essere in ritardo, altrimenti va bene
133800990518     c     kdcd          chain     fnanm02l                           30
133900990329    1c     *in30         doweq     *off
134000990329     c                   if        anmatb=' '
134100990329     c     anmcaa        lookup    anidd                                  34
134200990329     c                   if        *in34 AND
134300990329     c                             anmdao>wsavdao
134400990329     c                   eval      wsavdao=anmdao
134500990329     c                   eval      wsavcch=anmcch
134600990329     c                   eval      wsavdch=anmdch
134700990329     c                   endif
134800990329     c                   endif
134900990329     c**
135000990518     c     kdcd          reade     fnanm02l                               30
135100990329    1c                   enddo
135200990329     c**
135300990329     c* l'anomalia idd trovata deve essere chiusa
135400990329    1c     wsavdch       ifgt      0
135500990329     c**
135600990514     c* anomalia chiusa con pratica idd --> vedo se aperta 2 giorni dopo
135700990329     c*   escluso i festivi
135800990329    2c     wsavcch       ifeq      'PR'
135900990329     c* cerco la fase 10 di apertura
136000990329     c     kdct          chain     fndcf01l                           31
136100990329    3c     *in31         doweq     *off
136200990329     c     dcffca        andne     10
136300990329     c     kdct          reade     fndcf01l                               31
136400990329    3c                   enddo
136500990329     c** trovata la fase 10 --> controllo
136600990518    3c  n31dcfdfc        ifgt      wsavdch
136700990329     c* controllo di quanti giorni
136800990518     c     *iso          move      wsavdch       wdataanm
136900990329     c     *iso          move      dcfdfc        wdatafase
137000990329     c                   clear                   wgiorni           2 0
137100990329     c                   adddur    1:*D          wdataanm
137200990329    4c     wdataanm      dowle     wdatafase
137300990329     c* controllo se e' festivo
137400990329     C     *iso          move      wdataanm      kdata
137500990329     C                   Z-ADD     dctlna        KTFA
137600150604     c* per lna estera o DPd uso i50tfa
137700150604     c                   if        lnafl1='E' or lnantw='DPD'
137800150604     C                   Z-ADD     i50tfa        KTFA
137900150604     c                   endif
138000990329     C                   clear                   KTFP
138100990329     C     KCLN          CHAIN     AZCLN01L                           34
138200990329    5C     *IN34         IFEQ      *OFF
138300990329     C     MAT(KGG)      andne     'F'
138400990329     C     POM(KGG)      andne     'F'
138500990329     C     *IN34         oreq      *ON
138600990329     C                   add       1             wgiorni
138700990329    5C                   ENDIF
138800990329     c**
138900990329     c                   adddur    1:*D          wdataanm
139000990329    4c                   enddo
139100990329     c**
139200120209     c* In base alla data spedizione tengo conto di 2/3 giorni o 7/8 giorni
139300120209   4ac                   if        blpdsp>0 and blpdsp>=20120130
139400120209     c                             and blpdsp<=20120214
139500120209     c
139600120209    4c     wgiorni       ifgt      7
139700120209     c     §ogeid        andeq     ' '
139800120209     c* Per chi fa l'IDD il giorno dopo c'e' un giorno in piu' per aprire
139900120209     c     wgiorni       orgt      8
140000120209     c     §ogeid        andeq     'S'
140100120209     c                   eval      writ='R'
140200120209     c                   else
140300120209     c                   eval      writ='N'
140400120209    4c                   endif
140500120209     c
140600120209  x4ac                   else
140700120209     c
140800990514    4c     wgiorni       ifgt      2
140900990329     c     §ogeid        andeq     ' '
141000990329     c* Per chi fa l'IDD il giorno dopo c'e' un giorno in piu' per aprire
141100990514     c     wgiorni       orgt      3
141200990329     c     §ogeid        andeq     'S'
141300990329     c                   eval      writ='R'
141400990329     c                   else
141500990329     c                   eval      writ='N'
141600120209    4c                   endif
141700990329     c*
141800120209   4ac                   endif
141900120209     c
142000120209    3c                   endif
142100990329    2c                   endif
142200990329    1c                   endif
142300990329     c
142400990329     c                   endsr
142500990129      *****************************************************************
142600990129      * RESPONSABILITA' PER MANCANZE
142700990129      *****************************************************************
142800990129     c     MANCANZE      begsr
142900990305     c                   clear                   wsavdao
143000990305     c                   clear                   wsavcch
143100990305     c                   clear                   wsavdch
143200000321     c                   clear                   flptfa
143300000321     c                   clear                   writardo          1
143400090512     c                   clear                   wnfv
143500090828     c                   clear                   wter
143600090828     c                   clear                   wtipoflp          1
143700990305     c
143800990205     c* In questa routine il collo danneggiato da controllare e' sempre "1"
143900990129     C* controllo l'anomalia IDD3 se c'e'
144000990129     c*  data immissione C.A. --> responsabilita' arrivo
144100990518     c     kdcd          chain     fnanm02l                           30
144200990129    1c     *in30         doweq     *off
144300990305     c                   if        anmatb=' '
144400990305     c     anmcaa        lookup    anidd                                  34
144500990305     c                   if        *in34 AND
144600990305     c                             anmdao>wsavdao
144700990305     c                   eval      wsavdao=anmdao
144800990305     c                   eval      wsavcch=anmcch
144900990305     c                   eval      wsavdch=anmdch
145000990305     c                   endif
145100990305     c                   endif
145200990129     c**
145300990518     c     kdcd          reade     fnanm02l                               30
145400990129    1c                   enddo
145500990129     c**
145600990129     c* l'anomalia idd trovata deve essere chiusa
145700990329    1c     wsavdch       ifgt      0
145800990129     c* anomalia chiusa annullata --> colpa della partenza
145900990305    2c     wsavcch       ifeq      'AN'
146000990211     c                   MOVEL(p)  'IDDANN'      wke1
146100990129     c                   exsr      MEMRES
146200990208     c                   goto      endmanca
146300990129   x2c                   else
146400990129     c**
146500990129     c* anomalia chiusa con pratica idd --> vedo se aperta il giorno dopo
146600990129     c*   escluso i festivi
146700990331     c* All'export non imputo l'apertura in ritardo perche' non puo' essere
146800990331     c*  colpa del partner
146900990305    3c     wsavcch       ifeq      'PR'
147000990331     c     writ          andeq     'R'
147100990331     c     lnafl1        andne     'E'
147200000321     c* Per ora emorizzo solo che e' c.a. aerta in ritardo poi vedo
147300000321     c                   eval      writardo='S'
147400990129     c*
147500990129    3c                   endif
147600990129    2c                   endif
147700990129    1c                   endif
147800990129     c**
147900990208     c**  SPUNTE: vedo se ci sono
148000990205     c                   movel     'S'           wesistspu         1
148100070131     c**   kdcd          setll     fnbrv07l                               34
148200061020     c**n34              movel     'N'           wesistspu
148300061020     c* Dico che non esistono spunte se non trovate o se trovate ma
148400061020     c*  flaggate (significa che abbiamo tenuto solo le spunte con BRVCAN
148500061020     c*  impostato)
148600070131     c     kdcd          chain     fnbrv07l
148700070131     c                   if        not %found(fnbrv07l) or brvnvr<>' '
148800061020     c                   movel     'N'           wesistspu         1
148900061020     c                   endif
149000990204     c**
149100990205     c** PARTO COMUNQUE DAL DETT.COLLI POI IN CASO DI NECESSITA'
149200990205     c**  VALUTO LE SPUNTE
149300990205     c**
149400990208     c** TERMINAL DI PARTENZA di artflp
149500990208     c                   clear                   wflp
149600990205     c     artflp        ifgt      0
149700990208     c                   movel     'P'           d55tpt
149800990205     c                   z-add     artflp        d55lin
149900990205     c                   z-add     artlnp        d55lnp
150000990205     c                   select
150100990205     c     artdut        whengt    0
150200990205     c                   z-add     artdut        d55drf
150300990205     c     artdet        whengt    0
150400990205     c                   z-add     artdet        d55drf
150500990205     c     artdfv        whengt    0
150600990208     c                   z-add     artdfv        d55drf
150700990205     c                   other
150800990205     c                   z-add     dctdca        d55drf
150900990205     c                   endsl
151000990209     c                   call      'FNLV55R'
151100990205     c                   parm                    dslv55
151200990205     c*
151300990205     c                   z-add     d55tfp        wflp
151400090828     c                   movel     'P'           wtipoflp
151500000321     c*
151600000321     c** TERMINAL DI ARRIVO tra artflp e artlna
151700000321     c                   movel     'A'           d55tpt
151800000321     c                   z-add     artflp        d55lnp
151900000321     c                   z-add     artlna        d55lin
152000000321     c                   clear                   d55tfa
152100000321     c                   call      'FNLV55R'
152200000321     c                   parm                    dslv55
152300000321     c*
152400000321     c                   z-add     d55tfa        flptfa
152500990208     c                   endif
152600990308     c
152700990308     c* Se la data di arrivo pistola 85 > data apertura c.a. non la conseder
152800990308     c     artdam        ifgt      0
152900990308     c     artnap        andeq     85
153000990401     c     artdam        andge     dctdca
153100990308     c                   clear                   artdam
153200990308     c                   clear                   artnap
153300990308     c                   endif
153400990310     c
153500990310     c* Per EXPORT  ignoro sempre la pistola 85 --> errore bartolini di aper
153600990310     c     artdam        ifgt      0
153700990310     c     artnap        andeq     85
153800990310     c     lnafl1        andeq     'E'
153900990310     c                   clear                   artdam
154000990310     c                   clear                   artnap
154100990310     c                   endif
154200990205     c***
154300990205     c* DATA ARRIVO : se c'e' il collo e' arrivato
154400990208    1c     artdam        ifgt      0
154500990210     c                   clear                   wfgs
154600990205     C* verifico se dopo l'arrivo e' stato spuntato da altri P.O.
154700990208    2c     wesistspu     ifeq      'S'
154800990205     c                   z-add     artdam        wdatrifer
154900990205     c                   exsr      SPUARR
155000051010     c                   else
155100051010     c                   movel     'N'           WRicalcolo
155200990208    2c                   endif
155300990205     c***
155400990210     c* Se WFGS    >0 e non e' lna della CA cerco suo terminal di arrivo
155500990210    2c     wfgs          ifgt      0
155600990308     c
155700990311     c* Se bolla export e c'e' spunta defluenza con data>=data arrivo     nr
155800990311     c*  --> colpa partner
155900990311     c                   if        lnafl1='E'  AND
156000990311     c                             wEXPdef='S' AND
156100990311     c                             WEXPDFV>=artdam
156200990311     c
156300990310     c                   MOVEL(p)  'SPUNTADEFL_E'wke1
156400990308     c                   exsr      MEMRES
156500990308     c                   goto      endmanca
156600990308     c                   endif
156700990329
156800990329     c* Se fgs e' terminal arrivo bolla --> colpa sua
156900990329     c                   select
157000990329    3c     wfgs          wheneq    i50lna
157100990329     c                   MOVEL(p)  'ARRIVOLNA'   wke1
157200000321     c* Vedo se considerare il ritardo
157300000321     c                   exsr      AQUALE
157400990329     c                   goto      endmanca
157500990305     c* Se fgs e' terminal arrivo bolla --> colpa sua
157600990329    3c     wfgs          wheneq    i50tfa
157700000321     c* clearo terminal tenendo conto del p.o. di transito perche' non
157800000321     c*  mi serve
157900000321     c                   clear                   flptfa
158000990308     c
158100990305     c                   MOVEL(p)  'ARRIVOSI'    wke1
158200000321     c* Vedo se considerare il ritardo
158300000321     c                   exsr      AQUALE
158400990305     c                   goto      endmanca
158500990329   x3c                   other
158600990305     c
158700990210     c                   EXSR      terspunta
158800990311     c
158900990305     c*** Se si tratta comunque di p.o. dell'area di arrivo-->
159000990329     c***  imputo al p.o.
159100990205     c***  altrimenti e' DISGUIDO
159200990305    4c     wflp          ifeq      i50tfa
159300990305     c                   MOVEL(p)  'ARRIVOAREA'  wke1
159400990329     c                   eval      wflp=wfgs
159500090831     c                   movel     'A'           wtipoflp
159600000321     c* Vedo se considerare il ritardo
159700000321     c                   exsr      AQUALE
159800990208     c                   goto      endmanca
159900990305   x4c                   else
160000990211     c                   MOVEL(p)  'DISGUIDO'    wke1
160100000321     c* Vedo se considerare il ritardo
160200000321     c                   exsr      AQUALE
160300990208     c                   goto      endmanca
160400990305    4c                   endif
160500990329    3c                   endsl
160600990205     c**
160700040907     c* non trovate altre spunte
160800990208   x2c                   else
160900990305     c                   MOVEL(p)  'ARRAREA_'    wke1
161000990305     c                   MOVE      'NOSPU  '     wke1
161100000321     c* Vedo se considerare il ritardo
161200000321     c                   exsr      AQUALE
161300990208     c                   goto      endmanca
161400990208    2c                   endif
161500990208    1c                   endif
161600990205     c**
161700990205     c* DATA USCITA TRANSITO: se c'e' con pistola reale e' uscito dal trans
161800990208    1c     artdut        ifgt      0
161900990208     c                   movel     artput        w002a             2
162000990208     c     w002a         lookup    nps                                    34
162100990205     c** 34 off - pistola reale
162200990208    2c     *in34         ifeq      *off
162300990211     c                   MOVEL(p)  'TRADUT_SINO' wke1
162400000321     c* Vedo se considerare il ritardo
162500000321     c                   exsr      AQUALE
162600990208     c                   goto      endmanca
162700990205     c**
162800990208   x2c                   else
162900990205     c** Se si tratta di pistola completamente fasulla, senza altra
163000990205     c**  spunta, cerco nelle spunte un eventuale altro disguido
163100990205     c**  altrimenti e' colpa del transito
163200990205     c**
163300990208     c                   movel     artput        w002a             2
163400990208     c     w002a         lookup    npsno                                  31
163500990205     c** SPUNTA FASULLA DA SPUNTA REALE --> colpa del transito
163600990208    3c     *in31         ifeq      *off
163700990211     c                   MOVEL(p)  'TRADET_'     wke1
163800990205     c                   MOVE      'SINONO  '    wke1
163900000321     c* Vedo se considerare il ritardo
164000000321     c                   exsr      AQUALE
164100990208     c                   goto      endmanca
164200990208   x3c                   else
164300990205     c** Cerco un'altra spunta reale
164400990210     c                   clear                   wfgs
164500990208    4c     wesistspu     ifeq      'S'
164600990205     c                   clear                   wdatrifer
164700990210     c                   clear                   wfgs
164800990205     c                   exsr      SPUARR
164900990205     c**
165000990210     c* Se WFGS    >0 e non e' lnp della CA cerco suo terminal di arrivo
165100990210    5c     wfgs          ifgt      0
165200990210     c     wfgs          andne     dctlnp
165300990210     c     wfgs          andne     i50tfp
165400990210     c                   EXSR      TERSPUNTA
165500990205     c**
165600020312     C** SE SI TRATTA DI SPUNTA PARTENZA, LA CONSIDERO COME UN SI/NO
165700020312     C     wnpg          ifeq      1
165800020312     c                   MOVEL(p)  'TRADUT_SINO' wke1
165900020312     c                   else
166000990211     c                   MOVEL(p)  'DISGUIDO'    wke1
166100020312     c                   endif
166200000321     c* Vedo se considerare il ritardo
166300000321     c                   exsr      AQUALE
166400990208     c                   goto      endmanca
166500990205     c**
166600990208    5c                   endif
166700051010     c                   else
166800051010     c                   movel     'N'           WRicalcolo
166900990208    4c                   endif
167000990205     c**
167100990208    3c                   endif
167200990208    2c                   endif
167300990208    1c                   endif
167400990205     c**
167500990208     c**
167600990208     c* DATA ENTRATA TRANSITO: controllo se non c'e' data uscita o fasulla
167700990208     c*                        senza altra spunte o solo della lnp
167800990208    1c     artdet        ifgt      0
167900990208     c                   movel     artpet        w002a             2
168000990208     c     w002a         lookup    nps                                    34
168100060301     c* Pistola reale
168200060301    2c     *in34         ifeq      *off
168300990211     c                   MOVEL(p)  'TRADET_'     wke1
168400990205     c                   MOVE      'SINONO  '    wke1
168500000321     c* Vedo se considerare il ritardo
168600000321     c                   exsr      AQUALE
168700990208     c                   goto      endmanca
168800060301     c
168900060301   x2c                   else
169000060301     c
169100060301     c** Cerco un'altra spunta reale
169200060301     c                   clear                   wfgs
169300060301    4c     wesistspu     ifeq      'S'
169400060301     c                   clear                   wdatrifer
169500060301     c                   clear                   wfgs
169600060301     c                   exsr      SPUARR
169700060301     c**
169800060301     c* Se WFGS    >0 e non e' lnp della CA cerco suo terminal di arrivo
169900060301    5c     wfgs          ifgt      0
170000060301     c     wfgs          andne     dctlnp
170100060301     c     wfgs          andne     i50tfp
170200060301     c                   EXSR      TERSPUNTA
170300060301     c**
170400060301     C** SE SI TRATTA DI SPUNTA PARTENZA, LA CONSIDERO COME UN SI/NO
170500060301     C     wnpg          ifeq      1
170600060301     c                   MOVEL(p)  'TRADUT_SINO' wke1
170700060301     c                   else
170800060301     c                   MOVEL(p)  'DISGUIDO'    wke1
170900060301     c                   endif
171000060301     c* Vedo se considerare il ritardo
171100060301     c                   exsr      AQUALE
171200060301     c                   goto      endmanca
171300060301     c**
171400060301    5c                   endif
171500060301    4c                   endif
171600060301    2c                   endif
171700990208    1c                   endif
171800990205     c**
171900990205     c** DATA PARTENZA
172000990208    1c     artdfv        ifgt      0
172100060301     c
172200060301     c* Verifico quale è l'ultima spunta effettuata in partenza
172300060301    2c     wesistspu     ifeq      'S'
172400060309     c* Mi salvo la spunta partenza
172500060309     c                   movel     wfgs          fgspar
172600060309     c                   movel     wnfv          nfvpar
172700060309     c
172800060301     c                   z-add     artdfv        wdatrifer
172900060301     c                   eval      wautogen='N'
173000060301     c                   exsr      SPUARR
173100060301     c                   eval      wautogen=' '
173200060301     c
173300060301     c* Se l'ultima spunta non autogenerata è non categoria partenza-->
173400060301     c* colpa del p.o.
173500060301     c                   if        wnpg<>0
173600060301     c                   if        (wnpg<>1 and  wnpg<>3 and wnpg<>5)
173700090511     c                             or (wnpg=3 and wspg<>'P')  or
173800090511     c* oppure se la data della spunta > data partenza collo (solo IMP)
173900090511     c                             wdfv>artdfv
174000060301     c                   MOVEL(p)  'SPUNOPART'   wke1
174100060301     c                   eval      wflp=wfgs
174200090831     c                   movel     'P'           wtipoflp
174300060301     c                   exsr      AQUALE
174400060301     c                   goto      endmanca
174500060301     c                   endif
174600060301     c                   endif
174700060301     c                   endif
174800060301     c
174900990208     c                   movel     artnpp        w002a             2
175000990208     c     w002a         lookup    nps                                    34
175100990205     c*  partenza effettiva
175200990208    2c     *in34         ifeq      *off
175300060301     c
175400060301     c* Se è categoria partenza ma è di un secondo livello-->
175500060301     c* colpa del terminal
175600060301     c                   if        wesistspu='S' and wfgs<>0
175700060301     c                   clear                   dslv55
175800060301     c                   movel     'P'           d55tpt
175900060301     c                   z-add     wfgs          d55lin
176000060301     c                   z-add     wdfv          d55drf
176100060301     c                   call      'FNLV55R'
176200060301     c                   parm                    dslv55
176300060301     c                   if        d55tfp<>wfgs
176400060301     c                   MOVEL(p)  'SPUNOTER'    wke1
176500060301     c                   eval      wflp=wfgs
176600090831     c                   movel     'P'           wtipoflp
176700060301     c                   exsr      AQUALE
176800060301     c                   goto      endmanca
176900060301     c                   endif
177000060301     c                   endif
177100060309     c
177200060309     c* Reimosto il foglio partenza che mi serve per il calcolo della respo
177300060309     c                   eval                    wfgs= fgspar
177400060309     c                   eval                    wnfv= nfvpar
177500990205     c*
177600990205     c* se NON C'E' TRANSITO e' colpa della partenza
177700990208    3c     artflp        ifeq      0
177800990211     c                   MOVEL(p)  'PART_SINO'   wke1
177900000321     c* Vedo se considerare il ritardo
178000000321     c                   exsr      AQUALE
178100990208     c                   goto      endmanca
178200990205     c                   else
178300990205     c*        C'E' TRANSITO: e' colpa del transito
178400990211     c                   MOVEL(p)  'PART_'       wke1
178500990205     c                   MOVE      'SINONONO  '  wke1
178600000321     c* Vedo se considerare il ritardo
178700000321     c                   exsr      AQUALE
178800990208     c                   goto      endmanca
178900990208    3c                   endif
179000990205     c***
179100990208   x2c                   else
179200990208     c                   movel     'AUT'         wnppnps           3
179300990208    2c                   endif
179400990208   x1c                   else
179500990205     c                   movel     'AUT'         wnppnps
179600990208    1c                   endif
179700990205     c**
179800990205     c** partenza non effettiva  o = "0"
179900990208    1c     wnppnps       ifeq      'AUT'
180000990303     c
180100990303     c* Causale diversa per punta partenza fasulla
180200990303     c                   movel     artnpp        w002a             2
180300000321     c     w002a         lookup    npsno                                  35
180400990303     c
180500990208    2c     artflp        ifeq      0
180600990303     c
180700000321     c* pistola 88 /89 /90
180800990304    3c                   if        *in35
180900990304    4c                   if        lnpfl1='E'
181000990304     c                   MOVEL(p)  'PART_NONO_E' wke1
181100990304     c                   else
181200990304     c                   MOVEL(p)  'PART_NONO'   wke1
181300990304    4c                   endif
181400000321     c                   z-add     50            wresarr           3 0
181500990304     c
181600990304   x3c                   else
181700000321     c* pistola 98 /96
181800990304     c                   MOVEL(p)  'PART_NONO9X' wke1
181900990304    3c                   endif
182000990304     c
182100000321     c                   exsr      AQUALE
182200990208     c                   goto      endmanca
182300990303     c
182400990304   x2c                   else
182500990205     c*        C'E' TRANSITO: e' colpa del transito
182600990211     c                   MOVEL(P)  'PART_'       wke1
182700990304    3c                   if        *in35
182800990311    4c                   if        lnpfl1='E'
182900990304     c                   MOVE      'NONONONO_E'  wke1
183000990304     c                   else
183100990304     c                   MOVE      'NONONONO  '  wke1
183200990304    4c                   endif
183300000321     c                   z-add     50            wresarr           3 0
183400990304     c
183500990304   x3c                   else
183600990304     c                   MOVE      'NONONONO9X'  wke1
183700990304    3c                   endif
183800990304     c
183900000321     c                   exsr      AQUALE
184000990208     c                   goto      endmanca
184100990208    2c                   endif
184200990208    1c                   endif
184300990205     c***
184400990208     c     endmanca      endsr
184500000321      *****************************************************************
184600000321      * Vedo se dare la colpa prevista oper il ritardo
184700000321      *****************************************************************
184800000321     c     AQUALE        BEGSR
184900000321     c* Il ritardo non e' mai previsto a carico dell'estero
185000000321     c                   if        writardo='S'
185100000321     c* ritardo tutta colpa dell'arrivo
185200170413     c*  ignoro dal 01/05/2017 e memorizzo flag a parte
185300170413     c                   if        dctdca<=§vpoca
185400170411     c                   if        wresarr=0
185500170411     c                   MOVEL(p)  'IDDRITARDO'  wke1
185600170411     c                   endif
185700170413     c                   endif
185800170413
185900000321     c* ritardo colpa al 50%
186000000321     c                   if        wresarr=50
186100000321     c                   MOVEL(p)  'IDDRITARDO50'wke1
186200000321     c                   endif
186300000321     c
186400000321     c                   endif
186500000321     c
186600000321     c                   exsr      MEMRES
186700000321     c                   clear                   wresarr
186800000321     c                   ENDSR
186900990210      *****************************************************************
187000990210      * RESPONSABILITA' PER MANCANZE
187100990210      *****************************************************************
187200990210     c     TERSPUNTA     BEGSR
187300990210     c* per categoria "1" --> tengo fgs altrimenti cerco terminal arrivo
187400990210    1c     wnpg          ifeq      1
187500990210     c                   z-add     wfgs          wflp
187600090831     c                   movel     'P'           wtipoflp
187700990210     c                   else
187800990210     c                   clear                   dslv55
187900990210     c* Cerco il terminal di partenza per categoria partenza
188000990210     c                   if        wnpg=5   OR
188100990210     c                             wnpg=3 and wspg='P'
188200990210     c                   movel     'P'           d55tpt
188300990210     c                   else
188400990210     c                   movel     'A'           d55tpt
188500990210     c                   endif
188600990210     c                   movel     wfgs          d55lin
188700990210     c                   z-add     artlnp        d55lnp
188800990210     c                   z-add     wdfv          d55drf
188900990210     c                   call      'FNLV55R'
189000990210     c                   parm                    dslv55
189100990210     c**
189200990920     c                   if        d55tpt='P'
189300990920     c                   z-add     d55tfp        wflp
189400090831     c                   movel     'P'           wtipoflp
189500990920     c                   else
189600990210     c                   z-add     d55tfa        wflp
189700090831     c                   movel     'A'           wtipoflp
189800990210    1c                   endif
189900990920    1c                   endif
190000990210     c                   ENDSR
190100990129      *****************************************************************
190200990129      * RESPONSABILITA' PER AMMANCHI E AVARIE
190300990129      *****************************************************************
190400990209     c     AMMAVARIE     BEGSR
190500000321     c                   clear                   flptfa
190600090421     c                   clear                   SoloCAN
190700090828     c                   clear                   wter
190800090828     c                   clear                   wtipoflp          1
190900090828     c
191000990129     c* se il collo non ha spunte non posso calcolare se non c'e' riserva
191100070131     c**   kdcd          setll     fnbrv07l                               34
191200070131     c     kdcd          chain     fnbrv07l
191300090421     c*****              if        not %found(fnbrv07l) or brvnvr<>' '
191400110923    1c                   if        not %found(fnbrv07l)
191500061020     c                   setoff                                       34
191600110923   x1c                   else
191700090421     c* Presenti solo spunte con l'anomalia
191800110923    2c                   if        brvnvr<>' '
191900090421     c                   eval      soloCAN='S'
192000110923     c                   endif
192100110923     c
192200061020     c                   seton                                        34
192300110923    2c                   endif
192400061020     c
192500990209    1c     *in34         ifeq      *off
192600990208     c* controllo se c'e' riserva
192700990208     c                   exsr      CT_RISERVA
192800990208     c                   goto      endammava
192900990209   x1c                   else
193000990304     c
193100990209     c** Carico per ogni fgs la spunta con data piu' bassa che ha l'anomalia
193200990209     c                   EXSR      LETBRV
193300990304     c
193400990209     c** Se non c'e' nessuna spunta :
193500990210    2c     wnpg          ifeq      0
193600990209     c* se non c'e' nemmeno sul dett.colli --> NO_CAN
193700990331     c* se     c'e'         sul dett.colli --> A_CAN_NOSPU
193800050615     c* se artcan=ad una delle anomalie "fasulle" come se non ci fosse
193900050615     c     artcan        lookup    Noanomspu                              31
194000990209    3c     artcan        ifeq      ' '
194100050615     c     *in31         oreq      *on
194200990304     c                   movel(P)  'A_NO_CAN'    wke1
194300990209     c                   else
194400990331     c                   movel(P)  'A_CAN_NOSPU' wke1
194500990209    3c                   endif
194600990209     c
194700990209     c                   exsr      MEMRES
194800990209     c                   goto      endammava
194900990209    2c                   endif
195000990209     c**
195100090102     c** Se c'e' la spunta categoria part in partenza :colpa PART_SI
195200990209     c**  escludo le spunte di defluenza
195300081204    2c     wfgs          ifeq      i50tfp
195400090102    2c     wfgs          oreq      i50lnp
195500081204    3c     wnpg          ifeq      1
195600990210     c     wdef          andeq     ' '
195700080714     c     wnpg          oreq      5
195800080903     c* se si tratta di una spedizione import DPD, la colpa viene data
195900080903     c*  al partner
196000081204    4c                   if        lnpntw='DPD'
196100080903     c                   MOVEL(p)  'COLPAPART_E' wke1
196200080903     c                   else
196300990304     c                   movel(P)  'A_PART_SI'   wke1
196400081204    4c                   endif
196500990209     c                   exsr      MEMRES
196600990209     c                   goto      endammava
196700081204    3c                   endif
196800080714    2c                   endif
196900990210     c
197000090422     c** controllo categoria della 1°spunta con anomalia
197100990210     c                   movel     wnpg          w001a
197200990210     c     w001a         lookup    catarr                                 34
197300990210    2c   34wnpg          ifeq      1
197400990210     C     wdef          andeq     ' '
197500990210     c                   setoff                                       34
197600990210    2c                   endif
197700080513     c
197800080513     c                   if        wnpg=5
197900080513     c                   seton                                        34
198000080513     c                   endif
198100990210     c
198200990212     c** NON E' Categoria  ARRIVI  o ANOMALIA INSERITA ERRONEMANETE
198300990216     c**                 __> colpa sua
198400990212    2c                   if        not *in34   OR
198500990212     c                             werr = 'E'
198600990211     c                   movel     'S'           wsianom           1
198700990210     c                   EXSR      RESPOAAV
198800990210     c                   goto      endammava
198900990210     c
199000990210   x2c                   else
199100090423     c
199200090423     c                   eval      wNPGspuan=wnpg
199300090423     c                   eval      wflpspuan=wflp
199400090423     c                   eval      wfgsspuan=wfgs
199500990305     c
199600080903     c**     E' Categoria  ARRIVI/ENTRATA:
199700990210     c**     se spara un secondo livello --> colpa del terminal arrivo
199800990210     c**     se spara un primo   livello --> colpa del suo precedente
199900990210     c** Il suo precedente e' quello che ha la data < o a parita' di data
200000990210     c*   data/ora carcamento <
200100990211    3c                   if        wfgs<>i50tfa
200200990210     c                   exsr      terspunta
200300990210     c
200400990211    4c                   if        wflp<>wfgs
200500990921     c                   eval      zz=1
200600990921     c     wfgs          lookup    waavfgs(zz)                            34
200700990921     c                   movel     wnpg          w001a
200800990921     c* se non e' la 1 spunta del 2 livello --> e' colpa del 2 livello
200900090831     c*  per me qui c'e' un errore: se  il secondo livello non è la
201000070712     c*  linea di arrivo ma un altro secondo livello
201100080404     c*  la colpa finisce alla linea di arrivo lo stesso
201200080404     c*  4/4/08--> imposto WFLP col p.o. che ha la colpa  cambio il
201300080404     c*            flag tipo rsponsabilità al codice A_2LIV_TARD in "T"
201400081204    5c     *in34         ifeq      *off
201500990921     c     waavsca(zz)   orne      wscar
201600990921     c     waavfgs(zz)   orne      wfgs
201700990921     c     waavnpg(zz)   orne      w001a
201800990921     c     waavdfv(zz)   orne      wdfv
201900080404     c                   eval      wflp=wfgs
202000090831     c                   movel     'A'           wtipoflp
202100990921     c                   eval      wke1='A_2LIV_TARD    '
202200990921     c                   exsr      MEMRES
202300990921     c                   goto      endammava
202400081204   x5c                   else
202500990921     c
202600070711     c** Altrimenti e' colpa del terminal
202700070711     c*  prima di dare la colpa al terminal verifico se ho fatto "il furbo"
202800070711     c*  in fnbrve
202900070713     c**** kbrve         chain     fnbrve1l
203000070713     c****               if        %found(fnbrve1l) and E_brvcan=' '
203100070713     c****               eval      wke1='DA_CREARE      '
203200070713     c****               exsr      MEMRES
203300070711     c* se trovato, esco dal pgm ho attribuito la responsabilità
203400070713     c****               if        o50err=' '
203500070713     c****               goto      endammava
203600070713     c****               else
203700070711     c* Se non trovato record valido, procedo come ora
203800070713     c****               clear                   o50err
203900070713     c****               clear                   o50msg
204000070713     c****               endif
204100070713     c****               endif
204200070711     c
204300070711     c
204400081211     c                   eval      wfgsspuan=wfgs
204500090831     c                   movel     'A'           wtipoflp
204600081211
204700081204    6c                   if        wflp=i50tfa
204800990304     c                   eval      wke1='A_ARR_NOSPUTER '
204900990210     c                   exsr      MEMRES
205000990210     c                   goto      endammava
205100990210     c                   else
205200990304     c                   eval      wke1='A_TRA_NOSPUTER '
205300990210     c                   exsr      MEMRES
205400990210     c                   goto      endammava
205500081204    6c                   endif
205600081204    5c                   endif
205700081204    4c                   endif
205800990921    3c                   endif
205900990210     c**
206000090421     c*
206100990210     c                   clear                   WPREC200
206200990210     c                   clear                   WPRECNPG
206300990210     c                   clear                   WPRECFGS
206400990210     c                   clear                   WPRECSPG
206500990305     c
206600990305     c                   exsr      terspunta
206700070821     c                   eval      wNPGspuan=wnpg
206800070821     c                   eval      wflpspuan=wflp
206900990309     c                   eval      wfgsspuan=wfgs
207000090421     c
207100090421     c* Se ci sono solo le spunte con anomalie, le considerzioni del
207200090421     c*  precedente le devo fare sulla bolla e non sulle spunte
207300090421   3ac                   if        SoloCAN='S'
207400090421     c                   exsr      DaBolla
207500090421     c
207600090421     c
207700090421  x3ac                   else
207800090421     c* Cerco il precedente dalle spunte
207900090421     c                   movel     wdfv          w0200
208000090421     c                   move      wscar         w0200            20 0
208100990305     c
208200990210     c                   eval      zz=1
208300990210    3c     waavdfv(zz)   dowgt     0
208400990210     c                   movel     waavdfv(zz)   waav200          20 0
208500990210     c                   move      waavsca(zz)   waav200          20 0
208600990210     c*
208700990211     c* Escludo se waav20 e fgs sono il p.o. che ha sparato l'anomalia
208800990211     c                   movel     wnpg          w001a
208900990211     c     waavsca(zz)   ifne      wscar
209000990211     c     waavfgs(zz)   orne      wfgs
209100990211     c     waavnpg(zz)   orne      w001a
209200990211     c     waavdfv(zz)   orne      wdfv
209300990211     c
209400990210    4c     waav200       ifle      w0200
209500990210     c     waav200       andgt     wprec200
209600990210     C                   MOVEL     Waav200       wprec200
209700990210     c                   movel     waavnpg(zz)   wprecnpg
209800990210     c                   movel     waavfgs(zz)   wprecfgs
209900990210     c                   movel     waavspg(zz)   wprecspg
210000990210    4c                   endif
210100990211    4c                   endif
210200990210     c                   add       1             zz
210300990210    3c                   enddo
210400081204     c
210500990210     c** Se non ho trovato il precedente --> e' colpa della partenza
210600990210    3c     wprecnpg      ifeq      0
210700990308     c                   movel(P)  'A_PART_NOSI' wke1
210800990210     c                   exsr      MEMRES
210900990210     c                   goto      endammava
211000990210   x3c                   else
211100990211     c**  altrimenti cerco il suo terminal e attribuisco
211200990210     c**  Imposto i campi della spunta a cui devo dare la responsabilita'
211300990210     c                   eval      wnpg=wprecnpg
211400990210     c                   eval      wfgs=wprecfgs
211500990210     c                   eval      wspg=wprecspg
211600990210     c                   movel     wprec200      wdfv
211700990210     c                   move      wprec200      wscar
211800990210     c**
211900990211     c                   movel     'N'           wsianom
212000990210     c                   EXSR      RESPOAAV
212100990210     c                   goto      endammava
212200090421   3ac                   endif
212300090421    3c                   endif
212400990210    2c                   endif
212500990210     c
212600990209    1c                   endif
212700990129     c
212800990208     c     endammava     endsr
212900090421      *****************************************************************
213000090421      * ATTRIBUZIONE RESPONSABILITÀ AMMANCHI E AVARIE DDA BOLLA
213100090422      *  IN PRESENZA DELLE SOLE SPUNTE CON ANOMALIA
213200090421      *****************************************************************
213300090421     c     DaBolla       BEGSR
213400090421     c                   clear                   wscar
213500090421     c
213600090421     c* Se chi ha messo l'anomalia è la partenza --> colpa della partenza
213700090422     c* (l'ha già fatto sopra...però controllo solo le categorie 1 5 e IMP
213800090422     c*  qui daccio rientrare anche le altre categorie...)
213900090421     c                   select
214000090421    1c                   when      wfgsspuan=i50tfp or  wfgsspuan=i50lnp or
214100090421     c                             wflpspuan=i50tfp
214200090421     c                   movel(P)  'A_PART_NOSI' wke1
214300090421     c                   exsr      MEMRES
214400090421     c
214500090421    1c                   when      wfgsspuan=i50tfa
214600090421     c                   clear                   wfgs
214700090421     c* c'e' transito
214800090421    2c                   if        artflp>0
214900090421     c* Se c'e' la data uscita transito, imposto come categoria la 1-partenza
215000090421    3c                   if        artdut>0
215100090421     c                   eval      wfgs=artflp
215200090422     c                   eval      wflp=artflp
215300090421     c                   eval      wdfv=artdut
215400090421     c                   eval      wnpg=1
215500090421     c                   eval      wspg=' '
215600090421   x3c                   else
215700090421    4c                   if        artdet>0
215800090421     c                   eval      wfgs=artflp
215900090421     c                   eval      wdfv=artdet
216000090421     c                   eval      wnpg=2
216100090421     c                   eval      wspg=' '
216200090421     c                   exsr      TERSPUNTA
216300090421    4c                   endif
216400090421    3c                   endif
216500090422     c* Se il p.o. di transito è un terminal--> colpa sua
216600090422     c* se è un secondo livello --> colpa del suo terminal
216700090422     c                   eval      wke1='A_TRA_NOSI     '
216800090422     c                   exsr      MEMRES
216900090421    2c                   endif
217000090421     c
217100090421     c*  Se ancora vuoto --> colpa alla partenza
217200090421    2c                   if        wfgs=0
217300090421     c                   movel(P)  'A_PART_NOSI' wke1
217400090421     c                   exsr      MEMRES
217500090421    2c                   endif
217600090421     c
217700090422     c* Se è un transito ed è un terminal  --> colpa alla partenza
217800090422     c* (è per forza un terminal perchè se non lo fosse sarebbe già rientrato
217900090422     c*  sopra nella routine "A_TRA_NOSPUTER")
218000090422     c*  il realtà faccio rientrare tutti i caso mancanti (ma dovrebbe essere solo
218100090422     c*  questo)
218200090422    1c                   other
218300090421     c                   movel(P)  'A_PART_NOSI' wke1
218400090421     c                   exsr      MEMRES
218500090421     c                   endSL
218600090421     c                   ENDSR
218700990210      *****************************************************************
218800990210      * LETTURA DELLE SPUNTE PER CERCARE QUELLE CON ANOMALIA
218900990210      *****************************************************************
219000990210     c     RESPOAAV      BEGSR
219100990211    1c     wfgs          ifne      dctlna
219200990210     c                   exsr      terspunta
219300990210     c**
219400990210     c** Spuntato in arrivo dopo lo scarico
219500990211    2c     wflp          ifeq      i50tfa
219600990211    3c     wsianom       ifeq      'S'
219700990316     c
219800990316     c* Ci sono 2 casi:
219900990316     c* 1) chi ha sparato e il terminal di arrivo
220000990316    4c                   if        wfgs=i50tfa
220100990316     c                   if        werr=' '
220200990316     c                   eval      wke1='A_TER_DOPOSCAR '
220300990316     c                   else
220400990316     c                   eval      wke1='A_ERRTERSPUNTA '
220500990316     c                   endif
220600990316   x4c                   else
220700990316     c
220800990316     c* 2) Chi ha sparato e' un 2 livello della stessa area --> disguido
220900990316     c                   eval      wflp=wfgs
221000990316    5c                   if        werr=' '
221100990316     c                   eval      wke1='A_TRA_DOPOSCAR '
221200990316     c                   else
221300990316     c                   eval      wke1='A_ERRTRASPUNTA '
221400990316    5c                   endif
221500990316    4c                   endif
221600990316    c
221700990216     c
221800990316   x3c                   else
221900990305     c
222000990305     c* Se il terminal di arrivo del precedente = al terminal di chi ha
222100990305     c*  rilevato l'anomalia, vuol dire che ha scaricato tardi la pistola
222200081211    4c                   if        wflp=wflpspuan
222300081211    5c                   if        wfgs=wfgsspuan
222400990305     c                   eval      wke1='A_ARR_TARD     '
222500081211   x5c                   else
222600990309     c* Disguido in area
222700990309     c
222800990309     c                   eval      wke1='A_ARR_DISGAREA '
222900081211    5c                   endif
223000990309     c
223100081211   x4c                   else
223200990304     c                   eval      wke1='A_ARR_NOSI     '
223300081211    4c                   endif
223400990305     c
223500990305    3c                   endif
223600090831     c                   movel     'A'           wtipoflp
223700990210     c                   exsr      MEMRES
223800990305     c
223900990211   x2c                   else
224000990305     c
224100990211     c** Spuntato o dalla partenza
224200990211    3c     wflp          ifeq      i50tfp
224300990211    4c     wsianom       ifeq      'S'
224400990304     c                   eval      wke1='A_PART_SI      '
224500990211     c                   else
224600990304     c                   eval      wke1='A_PART_NOSI    '
224700990211    4c                   endif
224800090831     c                   movel     'P'           wtipoflp
224900990211     c                   exsr      MEMRES
225000990305     c
225100990211   x3c                   else
225200990305     c
225300990211     c** Spuntato al transito/disguido dopo lo scarico
225400990211    4c     wsianom       ifeq      'S'
225500990216     c                   if        werr=' '
225600990304     c                   eval      wke1='A_TRA_DOPOSCAR '
225700990216     c                   else
225800990304     c                   eval      wke1='A_ERRTRASPUNTA '
225900990216     c                   endif
226000990216     c
226100990211     c                   else
226200990304     c                   eval      wke1='A_TRA_NOSI     '
226300990211    4c                   endif
226400090831     c                   movel     'A'           wtipoflp
226500990210     c                   exsr      MEMRES
226600990211    3c                   endif
226700990211    2c                   endif
226800990211   x1c                   else
226900990210     c** Spuntato in arrivo dopo lo scarico
227000990211    2c     wsianom       ifeq      'S'
227100990216     c                   if        werr=' '
227200990304     c                   eval      wke1='A_ARR_DOPOSCAR '
227300990211     c                   else
227400990304     c                   eval      wke1='A_ERRARRSPUNTA '
227500990216    2c                   endif
227600990216     c
227700990216     c                   else
227800990305     c
227900990305     c* Se il terminal di arrivo del precedente = al terminal di chi ha
228000990305     c*  rilevato l'anomalia, vuol dire che ha scaricato tardi la pistola
228100990331     c                   if        wfgs=wfgsspuan
228200990305     c                   eval      wke1='A_ARR_TARD     '
228300990305     c                   else
228400990304     c                   eval      wke1='A_ARR_NOSI     '
228500990211    2c                   endif
228600990305    2c                   endif
228700990305     c
228800090831     c                   movel     'A'           wtipoflp
228900990210     c                   exsr      MEMRES
229000990210    3c                   endif
229100990211    1c                   ENDSR
229200990209      *****************************************************************
229300990209      * LETTURA DELLE SPUNTE PER CERCARE QUELLE CON ANOMALIA
229400990209      *****************************************************************
229500990209     c     LETBRV        BEGSR
229600990209     c                   z-add     1             zz                3 0
229700990210     c* Skiere in cui memorizzo per P.O. la spunta con data  piu' bassa
229800090428     c*  a parita di data, privilegiando le categorie ARR tenendo conto
229900990211     c*  delle data/ora scarico + tolleranza
230000990209     c                   clear                   waavnpg
230100990209     c                   clear                   waavdfv
230200990209     c                   clear                   waavfgs
230300000322     c                   clear                   waavAS
230400990209     c                   clear                   waavsca
230500990209     c                   clear                   waavdef
230600990210     c                   clear                   waavspg
230700000322     c                   clear                   waavnps
230800990209     c
230900990209     c* Campi in cui memorizzo il primo che ha rilevato l'anomalia
231000990210     c                   clear                   wnpg
231100990210     c                   clear                   wdfv
231200990210     c                   clear                   wfgs
231300990210     c                   clear                   wscar
231400990210     c                   clear                   wdef
231500990210     c                   clear                   wspg
231600990212     c                   clear                   werr
231700070711     c                   clear                   wnfva
231800000322     c                   clear                   brvas
231900990209     c* Campi di comodo
232000990209     c
232100070131     c     kdcd          setll     fnbrv07l
232200070131     c     kdcd          reade     fnbrv07l                               34
232300990209    1c     *in34         doweq     *off
232400000229     c* se data e ora scarico uguale a zeros muovo la data manutenzione spunte
232500000229      *
232600000229     c                   if        brvscar = 0
232700000229     c                   eval      brvdcs = brvdfs
232800000229     c                   eval      brvhcs = brvhms
232900000229     c                   endif
233000000229      *
233100990212     c                   clear                   w001a             1
233200990212     c                   clear                   wcarica           1
233300990209     c* Escludo le pistole completamente fasulle
233400990316     c*  Includo il transito come 89
233500990209     c                   movel     brvnps        w002a
233600990209     c     w002a         lookup    npsno                                  31
233700990209    3c     *in31         ifeq      *off
233800990316     c     brvnps        oreq      89
233900990209     c* Prendo la data
234000990209     c                   clear                   dslv53
234100990209     c                   movel     brvnpg        d53npg
234200000209     c                   z-add     brvnfv        d53nfv
234300990209     c                   movel     brvfgs        d53fgs
234400990209     c                   movel     brvfle        d53flf
234500990209     c                   call      'FNLV53R'
234600990209     c                   parm                    dslv53
234700990209     c**
234800990209     c** se non trovata data ignoro
234900990209    4c     d53err        ifeq      *blanks
235000050204     c* Se data foglio = sabato, la imposto di lunedì
235100050204     c                   Clear                   wgioset
235200050204     c     *iso          Movel     d53dfv        datadmy
235300050204     c     *dmy          Movel     datadmy       w0060             6 0
235400050204     c                   Movel     w0060         wggmmaa
235500050204     c                   Call      'XGIOSE1'
235600050204     c                   Parm                    wggmmaa
235700050204     c                   Parm                    wgioset
235800050204     c
235900050204     c     *iso          move      d53dfv        wdataanm
236000130122     c
236100130122    5c                   if        wgioset='6'
236200050204     c                   adddur    2:*D          wdataanm
236300130122    5c                   endif
236400130122     c
236500130122     c                   setoff                                       34
236600130122    5c                   dow       not *in34
236700050204     c* controllo se e' festivo
236800050204     C     *iso          move      wdataanm      kdata
236900050204     C                   Z-ADD     68            KTFp
237000050204     C                   clear                   KTFa
237100050204     C     KCLN          CHAIN     AZCLN01L                           34
237200050204    7C     *IN34         IFEQ      *OFF
237300050204     C     MAT(KGG)      andne     ' '
237400050204     C     *IN34         oreq      *OFF
237500050204     C     POM(KGG)      andne     ' '
237600050204     c**
237700050204     c                   adddur    1:*D          wdataanm
237800050204     c                   else
237900050204     c                   seton                                        34
238000050204    7c                   endif
238100130122    5c                   enddo
238200050204     c
238300050204     c     *iso          movel     wdataanm      d53dfv
238400050204     c
238500050204     c
238600990209     c                   eval      zz=1
238700990209     c     brvfgs        lookup    waavfgs(zz)                            31
238800990209    5c                   if        not *in31
238900990209     c                   eval      zz=1
239000990209     c     000           lookup    waavfgs(zz)                            31
239100990209     c                   eval      waavfgs(zz)=brvfgs
239200990209    5c                   endif
239300000322     c
239400000322     c** prendo as di appartenenza
239500000322     c**  per cercare altro p.o. gestione su stesso AS
239600000322     c                   clear                   wstessoas
239700000322     c
239800000322     c     brvfgs        chain     azorg01l                           33
239900021120     c  n33              movel     orgde0        og140
240000021120     c  n33              movel     §ogaex        brvas
240100000322     c   33              movel     brvfgs        brvas
240200000322     c
240300000322    5c                   if        not *in33
240400041006     c* Se il p.o. spunta è la linea di arrivo o il terminal di arrivo
240500041006     c*  ignoro il fatto dello stesso AS
240600041006     c                   if        brvfgs<>i50lna and brvfgs<>i50tfa  and
240700041006     c                             brvfgs<>i50lnp
240800000322     c                   eval      jj=1
240900000322     c     brvas         lookup    waavas(jj)                             33
241000090428     c* elaboro anche se stesso p.o. per ignorare spunta 89 se poi c'e' reale
241100090428     c***                dow       *in33 and brvfgs=waavfgs(jj)  or
241200090428     c                   dow       *In33 and waavfgs(jj)=i50lna  or
241300041006     c                             *In33 and waavfgs(jj)=i50tfa  or
241400041006     c                             *In33 and waavfgs(jj)=i50lnp
241500000322     c                   eval      JJ=1+jj
241600021120     c     brvas         lookup    waavas(jj)                             33
241700000322     c                   enddo
241800000322     c* Trovato!!
241900000322     c   33              movel     'S'           wstessoas
242000041006     c
242100000322    5c                   endif
242200041006    5c                   endif
242300000322     c
242400000322     c** Se trovo spunta di altra filiale gestione ma stesso as e quella
242500000322     c**  letta e' pistola 89 --> la ignoro
242600000322   4ac                   if        wstessoas='S' and brvnps<>89 OR
242700000322     c                             wstessoas=' '
242800000322     c
242900000322    5c                   if        wstessoas='S' AND waavnps(jj)=89
243000000322     c** sostituisco la spunta e faccio fina che la precedente non ci
243100000322     c**  sia
243200000322     c                   eval      zz=jj
243300000322     c                   eval      waavfgs(zz)=brvfgs
243400000322     c                   clear                   waavdfv(zz)
243500000322     c                   clear                   waavnpg(zz)
243600000322     c                   clear                   waavspg(zz)
243700000322     c                   clear                   waavsca(zz)
243800000322    5c                   endif
243900990507     c
244000990507     c** Imposto campi di comodo
244100990507     c* campi nuova spunta letta
244200050207     c                   movel     brvnps        wnewnps           2 0
244300050207     c                   movel     brvnpg        wnewnpg           1
244400990211     c                   movel     d53def        wnewdef           1
244500990507     c                   movel     d53dfv        wnewdfv           8 0
244600990507     c                   movel     d53spg        wnewspg           1
244700990507     c                   movel     brvscar       wnewscar         12 0
244800990507     c* campi vecchia spunta memorizzata
244900990211     c                   movel     waavnpg(zz)   woldnpg           1
245000990211     c                   movel     waavdef(zz)   wolddef           1
245100990211     c                   movel     waavdfv(zz)   wolddfv
245200990507     c                   movel     waavspg(zz)   woldspg           1
245300050207     c                   movel     waavnps(zz)   woldnps           2 0
245400990507     c                   movel     waavsca(zz)   woldscar         12 0
245500040907     c                   EXSR      SCEGLIxav
245600990507     c
245700990507     c** spunta da caricare
245800990211    5c                   if        wcarica='S'
245900990209     c                   movel     brvnpg        waavnpg(zz)
246000990210     c                   eval      waavdfv(zz)=d53dfv
246100990209     c                   eval      waavsca(zz)=brvscar
246200990209     c                   eval      waavdef(zz)=d53def
246300990210     c                   eval      waavspg(zz)=d53spg
246400000322     c                   eval      waavnps(zz)=brvnps
246500000322     c                   eval      waavAS(zz)=brvas
246600990210    5c                   endif
246700990209     c**
246800990209     c** C'E' ANOMALIA:  tengo la spunta con la data piu' bassa
246900990209   5ac     brvcan        ifne      *blanks
247000990316     c     brvnps        andne     89
247100050615     c     brvcan        lookup    Noanomspu                              31
247200050615   5bc                   if        not *in31
247300990210     c                   clear                   WCARICA
247400990209     c**
247500990211     c* Controllo quale delle 2 spunte e' cat.arrivi
247600990507     c* reimposto solo i campi old perche' la NEW e' la stessa spunta letta
247700990211     c                   movel     wnpg          woldnpg           1
247800990211     c                   movel     wdef          wolddef           1
247900990507     c                   movel     wspg          woldspg
248000990211     c                   movel     wdfv          wolddfv           8 0
248100990507     c                   movel     wscar         woldscar         12 0
248200990507     c
248300040907     c                   EXSR      SCEGLIxav
248400990210     c
248500990210    5c                   if        wcarica='S'
248600990210     c                   eval      wdfv   =d53dfv
248700990210     c                   eval      wnpg   =brvnpg
248800990210     c                   eval      wscar  =brvscar
248900990210     c                   eval      wdef   =d53def
249000990210     c                   eval      wfgs   =brvfgs
249100990210     c                   eval      wspg   =d53spg
249200070711     c                   eval      wNFVa  =d53NFV
249300990212     c                   clear                   werr
249400990212     c* verifico per gli ammanchi se e' stata inserita l'anomalia sbagliata
249500990212     c*  -> se c'e' solo questa o e' la piu' piccola e' colpa sua
249600990212     c                   movel     brvnpg        w001a
249700990212     c     w001a         lookup    catarr                                 31
249800990315    6c   31brvnpg        ifeq      1
249900990212     c     d53def        andeq     ' '
250000990212     c                   setoff                                       31
250100990315    6c                   endif
250200990212     c
250300990315    6c                   if        *in31
250400990330    7c                   if        §tadragr='A'
250500990330     c     brvcan        lookup    amspu                                  34
250600990212     c** Anomalia non giusta --> memorizzo
250700990330     c  N34              eval      werr='E'
250800990315    7c                   endif
250900990330    7c                   if        §tadragr='V'
251000990330     c     brvcan        lookup    avspu                                  34
251100990330     c  N34              eval      werr='E'
251200990315    7c                   endif
251300990212    6c                   endif
251400990212     c
251500990209    5c                   endif
251600050615   5bc                   endif
251700050615   5ac                   endif
251800000322     c
251900000322  x4ac                   else
252000000322     c                   if        waavfgs(zz)<>0   AND waavdfv(zz)=0
252100000322     c                   clear                   waavfgs(zz)
252200000322     c                   endif
252300000322     c
252400000322   4ac                   endif
252500990209     c
252600990209    4c                   endif
252700990209    3c                   endif
252800990209     c**
252900070131     c     kdcd          reade     fnbrv07l                               34
253000990209    1c                   enddo
253100990209     c                   ENDSR
253200990507      *****************************************************************
253300040907      *  vedo quale delle 2 spunte tenere per ammanchi avarie
253400990507      *****************************************************************
253500040907     c     SCEGLIXAV     BEGSR
253600990507     c* determino se sono spunte arrivi
253700990507     c                   EXSR      DETERARR
253800990507     c
253900990507    5c                   SELECT
254000990507     c* Data OLD non presente --> carico
254100990507     c                   WHEN      wolddfv=0
254200990507     c                   eval      wcarica='S'
254300990507     c
254400990507     c* Data NEW minore data OLD --> carico solo se la old e' arrivi con
254500990507     c*      scarico minore
254600990507    5c                   WHEN      wnewDFV<wolddfv
254700990507     c* se la spunta con data minore e' una categoria partenza e quella con
254800990507     c*    data maggiore e' una arrivi, tengo quella arrivi se la data
254900990507     c*    di scarico e' minore
255000990507     c     wnewnpg       lookup    catpar                                 31
255100990507    6c   31              if        wnewnpg='3' and wnewspg<>'P'
255200990507     c                   setoff                                       31
255300990507    6c                   endif
255400990507     c**
255500990507     c** La spunta scaricata non e' partenza --> la tengo
255600990507    6c                   if        not *in31  OR
255700990507     c                             *in31  and woldarr<>'A'
255800990507     c                   eval      wcarica='S'
255900990507   x6c                   else
256000071002     c* la spunta scaricata e' partenza con pistola autogenerata 89
256100071002     c*  tengo sempre la spunta arrivi altrimenti
256200071002     c*  controllo data ora scarico
256300071002   6ac                   if        wnewnps<>89
256400071002     c
256500990507    7c     wnewscar      iflt      woldscar
256600990507     c                   movel     wnewscar      w0080             8 0
256700990507     c                   move      wnewscar      w0040             4 0
256800990507     c                   EXSR      ADDTOLLER
256900990507     c                   movel     w0080         w0120
257000990507     c                   move      w0040         w0120            12 0
257100990507    8c     w0120         iflt      woldscar
257200990507     c                   eval      wcarica ='S'
257300990507    8c                   endif
257400990507    7c                   endif
257500071002   6ac                   endif
257600071002    6c                   endif
257700990507     c
257800990507     c* Se date uguali --> prendo quella con caricamento piu' basso o la
257900990507     c*                    categoria arrivi se rientra nei minuti di tollera
258000050207     c*                    Se sono due spunte arrivi tengo la pist.reale
258100990507     c
258200990507    5c                   WHEN      wnewDFV=wolddfv
258300990507     c                   EXSR      TIENIARR
258400990507     c
258500990507     c* Se la data e' > verifico se sto scaricando una arrivi e ho memorizza
258600990507     c*  to una partenza
258700990507    5c                   WHEN      wnewDFV>wolddfv AND wnewarr='A'
258800990507     c     woldnpg       lookup    catpar                                 31
258900990507    6c   31              if        woldnpg='3' and woldspg<>'P'
259000990507     c                   setoff                                       31
259100990507    6c                   endif
259200990507     c
259300990507     c** La spunta memorizzata deve essere una partenza
259400990507    6c                   if        *in31
259500990507     c
259600990507     c* la spunta scaricata e' partenza --> controllo data ora scarico
259700990507    7c     wnewscar      ifle      woldscar
259800990507     c                   eval      wcarica ='S'
259900990507     c                   else
260000990507     c                   movel     woldscar      w0080             8 0
260100990507     c                   move      woldscar      w0040             4 0
260200990507     c                   EXSR      ADDTOLLER
260300990507     c                   movel     w0080         w0120
260400990507     c                   move      w0040         w0120            12 0
260500990507    8c     wnewscar      ifle      w0120
260600990507     c                   eval      wcarica ='S'
260700990507    8c                   endif
260800990507    7c                   endif
260900990507    6c                   endif
261000071002     c
261100071002     c** La spunta memorizzata è partenza autogenerata con pistola 89
261200071002     c** e la spunta arrivi è pistola reale
261300071002     c** tengo sempre la spunta arrivi
261400071002    6c                   if        *in31  and woldnps=89
261500071002     c                   eval      wcarica ='S'
261600071002    6c                   endif
261700990507     c
261800990507    5c                   endsl
261900990507     c**
262000990507     c                   ENDSR
262100040907      *****************************************************************
262200040907      *  Vedo quale delle 2 spunte tenere per MANCANZE
262300040907      *****************************************************************
262400040907     c     SCEGLIXMA     BEGSR
262500040907     c* determino se sono spunte arrivi
262600040907     c                   EXSR      DETERARR
262700040907     c
262800040907    5c                   SELECT
262900040907     c* Data NEW minore data OLD --> carico solo se la old e' arrivi con
263000040907     c*      scarico minore
263100040907    5c                   WHEN      wnewDFV<wolddfv  and woldarr='A'
263200040907     c* se la spunta con data minore e' una categoria partenza e quella con
263300040907     c*    data maggiore e' una arrivi, tengo quella arrivi se la data
263400040907     c*    di scarico e' minore
263500040907     c     wnewnpg       lookup    catpar                                 31
263600040907    6c   31              if        wnewnpg='3' and wnewspg<>'P'
263700040907     c                   setoff                                       31
263800040907    6c                   endif
263900040907     c**
264000040907     c* la spunta scaricata e' partenza --> controllo data ora scarico
264100040907    6c                   if        *in31
264200040907    7c     wnewscar      ifge      woldscar
264300040907     c                   eval      wcarica ='S'
264400040907     c                   else
264500040907     c                   movel     wnewscar      w0080             8 0
264600040907     c                   move      wnewscar      w0040             4 0
264700040907     c                   EXSR      ADDTOLLER
264800040907     c                   movel     w0080         w0120
264900040907     c                   move      w0040         w0120            12 0
265000040907    8c     w0120         ifge      woldscar
265100040907     c                   eval      wcarica ='S'
265200040907    8c                   endif
265300040907    7c                   endif
265400040907    6c                   endif
265500040907     c
265600040907     c* Se date uguali --> prendo quella con caricamento piu' alto  o la
265700040907     c*                    categoria part.  se rientra nei minuti di tollera
265800040907     c
265900040907    5c                   WHEN      wnewDFV=wolddfv
266000040907     c
266100040907    6c     wnewscar      ifgt      woldscar
266200040907     c     woldarr       andeq     wnewarr
266300040907     c                   eval      wcarica='S'
266400040907   x6c                   else
266500040907     c**
266600040907     c* Valuto quale tenere con i minuti di tolleranza
266700040907     c     wnewnpg       lookup    catpar                                 31
266800040907    7c   31              if        wnewnpg='3' and wnewspg<>'P'
266900040907     c                   setoff                                       31
267000040907    7c                   endif
267100040907     c
267200040907    7c                   if        *in31
267300040907    8c     wnewscar      ifge      woldscar
267400040907     c                   eval      wcarica='S'
267500040907   x8c                   else
267600040907     c                   movel     wnewscar      w0080             8 0
267700040907     c                   move      wnewscar      w0040             4 0
267800040907     c                   EXSR      ADDTOLLER
267900040907     c                   movel     w0080         w0120
268000040907     c                   move      w0040         w0120
268100040907    9c     w0120         ifge      woldscar
268200040907     c                   eval      wcarica='S'
268300040907    9c                   endif
268400040907    8c                   endif
268500040907    7c                   endif
268600040907    c
268700040907     c     woldnpg       lookup    catpar                                 31
268800040907    7c   32              if        wnewnpg='3' and wnewspg<>'P'
268900040907     c                   setoff                                       32
269000040907    7c                   endif
269100040907    7c                   if        *in32
269200040907    8c     wnewscar      ifgt      woldscar
269300040907     c                   movel     woldscar      w0080             8 0
269400040907     c                   move      woldscar      w0040             4 0
269500040907     c                   EXSR      ADDTOLLER
269600040907     c                   movel     w0080         w0120
269700040907     c                   move      w0040         w0120            12 0
269800040907    9c     wnewscar      ifgt      w0120
269900040907     c                   eval      wcarica ='S'
270000040907    9c                   endif
270100040907    8c                   endif
270200040907    7c                   endif
270300040907     c* Se ne vecchia nè nuova partenza, tengo il caricamento + alto
270400040907    7c                   if        not *in31 and not *in32
270500040907    8c     wnewscar      ifgt      woldscar
270600040907     c                   eval      wcarica ='S'
270700040907    8c                   endif
270800040907    7c                   endif
270900040907     c
271000040907    6c                   endif
271100040907     c
271200040907     c* Se la data e' > verifico se sto scaricando una arrivi e ho memorizza
271300040907     c*  to una partenza
271400040907    5c                   WHEN      wnewDFV>wolddfv AND wnewarr=' '
271500040907     c                   eval      wcarica ='S'
271600040907     c
271700040907    5c                   WHEN      wnewDFV>wolddfv AND wnewarr='A'
271800040907     c     woldnpg       lookup    catpar                                 31
271900040907    6c   31              if        woldnpg='3' and woldspg<>'P'
272000040907     c                   setoff                                       31
272100040907    6c                   endif
272200040907     c
272300040907     c** La spunta memorizzata deve essere una partenza
272400040907    6c                   if        *in31
272500040907    7c     wnewscar      ifgt      woldscar
272600040907     c                   movel     woldscar      w0080             8 0
272700040907     c                   move      woldscar      w0040             4 0
272800040907     c                   EXSR      ADDTOLLER
272900040907     c                   movel     w0080         w0120
273000040907     c                   move      w0040         w0120            12 0
273100040907    8c     wnewscar      ifgt      w0120
273200040907     c                   eval      wcarica ='S'
273300040907    8c                   endif
273400040907    7c                   endif
273500040907     c
273600040907     c* la spunta scaricata e' partenza --> controllo data ora scarico
273700040907   x6c                   else
273800040907     c* Se la vecchia non è partenza, carico
273900040907     c                   eval      wcarica ='S'
274000040907    6c                   endif
274100040907     c
274200040907    5c                   endsl
274300040907     c**
274400040907     c                   ENDSR
274500990211      *****************************************************************
274600990211      *   TENGO LA SPUNTA ARRIVI CONSIDERANDO I MINUTI DI TOLLERANZA
274700990211      *****************************************************************
274800990211     c     TIENIARR      BEGSR
274900050207     c* se sono 2 cat.arrivi -stessa data e c'e' una pistola fasulla, tengo
275000050207     c*  la pistola reale a prescindere dallo scarico
275100050207     c                   setoff                                       3132
275200050207    0c                   if        woldarr=wnewarr and woldarr='A'
275300050207     c                   movel     wnewnps       w002a
275400050207     c     w002a         lookup    npsno                                  31
275500050207     c                   movel     woldnps       w002a
275600050207     c     w002a         lookup    npsno                                  32
275700090105
275800090105     c* se la vecchia è fasulla, e la nuova  no, tengo la NUOVA
275900090105     c                   if        *in32 and  not *in31
276000090105     c                   eval      wcarica='S'
276100090105     c                   goto      endtieni
276200090105     c                   endif
276300090105     c* se la nuova è fasulla, e la vecchia no, tengo la vecchia
276400090105     c                   if        *in31 and  not *in32
276500090105     c                   goto      endtieni
276600090105     c                   endif
276700090105
276800090105   x0c                   else
276900090105     c
277000090105     c* oppure se una entrata e una arrivi fasulla --> tengo l'entrata
277100090105     c                   movel     wnewnps       w002a
277200090105     c     w002a         lookup    npsno                                  31
277300090105     c                   movel     woldnps       w002a
277400090105     c     w002a         lookup    npsno                                  32
277500090105     c* se la vecchia è fasulla arrivi, e la nuova no entrata, tengo laNUOVA
277600090105    1c                   if        *in32 and  not *in31  and
277700090105     c                             woldarr='A' and wnewarr='5'
277800090105     c                   eval      wcarica='S'
277900090105     c                   goto      endtieni
278000090105    1c                   endif
278100090105     c* se la nuova è fasulla arrivi, e la vecchia no entrata, tengo la vecchia
278200090105    1c                   if        *in31 and  not *in32     and
278300090105     c                             wnewarr='A' and woldarr='5'
278400090105     c                   goto      endtieni
278500090105    1c                   endif
278600090105
278700090105    0c                   endif
278800050207     c
278900050207     c
279000050207     c* Se sono entrambe non arrivi o entrambe arrivi tengo lo scarico + bas
279100990211    1c     wnewscar      iflt      woldscar
279200990507     c     woldarr       andeq     wnewarr
279300990211     c                   eval      wcarica='S'
279400050207     c                   goto      endtieni
279500050207    1c                   endif
279600990211     c**
279700990211     c* Valuto quale tenere con i minuti di tolleranza
279800990507    2c                   if        wnewarr='A'
279900090702    3c     wnewscar      ifge      woldscar
280000990211     c                   movel     woldscar      w0080             8 0
280100990211     c                   move      woldscar      w0040             4 0
280200990211     c                   EXSR      ADDTOLLER
280300990211     c                   movel     w0080         w0120
280400990211     c                   move      w0040         w0120
280500990211    4c     wnewscar      ifle      w0120
280600990211     c                   eval      wcarica='S'
280700990211    4c                   endif
280800990316     c
280900990316     c                   else
281000990316     c                   eval      wcarica='S'
281100990211    3c                   endif
281200990211    2c                   endif
281300990211    c
281400990507    2c                   if        woldarr='A'
281500090702    3c     wnewscar      iflt      woldscar
281600990211     c                   movel     wnewscar      w0080             8 0
281700990211     c                   move      wnewscar      w0040             4 0
281800990211     c                   EXSR      ADDTOLLER
281900990211     c                   movel     w0080         w0120
282000990211     c                   move      w0040         w0120            12 0
282100990507    4c     w0120         iflt      woldscar
282200990211     c                   eval      wcarica ='S'
282300990211    4c                   endif
282400990211    3c                   endif
282500990211    2c                   endif
282600990211     c
282700990211     c
282800050207     c     endtieni      ENDSR
282900990211      *****************************************************************
283000990211      *   DETERMINO QUELE DELLE SPUNTE E' CAT.ARRIVI
283100990211      *****************************************************************
283200990211     c     DETERARR      BEGSR
283300990507     c                   clear                   WOLDARR
283400990507     c                   clear                   WNEWARR
283500990211     c
283600990211     c     wnewnpg       lookup    catarr                                 31
283700990211     c   31              if        wnewnpg='1' and wnewdef=' '
283800990211     c                   setoff                                       31
283900990211     c                   endif
284000990507     c   31              movel     'A'           wnewarr           1
284100090105     c  n31              if        wnewnpg='5'
284200090105     c                   movel     '5'           wnewarr           1
284300090105     c                   endif
284400990211     c
284500990211     c     woldnpg       lookup    catarr                                 31
284600990211     c   31              if        woldnpg ='1' and wolddef   =' '
284700990211     c                   setoff                                       31
284800990211     c                   endif
284900990507     c   31              movel     'A'           woldarr           1
285000090105     c  n31              if        woldnpg='5'
285100090105     c                   movel     '5'           woldarr           1
285200090105     c                   endif
285300990507     c
285400990211     c                   ENDSR
285500990205      *****************************************************************
285600990211      *   SOMMO MINUTI DI TOLLERANZA
285700990205      *****************************************************************
285800990211     C     ADDTOLLER     BEGSR
285900990211     c                   clear                   w0060             6 0
286000990211     c                   movel     w0040         w0060
286100990211     c     *iso          move      w0080         wdata
286200990211     c     *iso          move      w0060         wora
286300990212     c                   adddur    §stdmtl:*mn   wtempo
286400990211     c     *iso          move      wdata         w0080
286500990211     c     *iso          move      wora          w0060
286600990211     c                   movel     w0060         w0040
286700990211     c
286800990211     c                   ENDSR
286900990211      *****************************************************************
287000990211      *   CONTROLLO SE IL COLLO ARRIVATO E' STATO spuntato da altre parti
287100990211      *****************************************************************
287200990211     C     SPUARR        BEGSR
287300990210     c                   clear                   wdfv
287400990210     c                   clear                   wdcs
287500990210     c                   clear                   whcs
287600990210     c                   clear                   wfgs
287700990210     c                   clear                   wnpg
287800990210     c                   clear                   wspg
287900990308     c                   clear                   wdef
288000040907     c                   clear                   wnfv
288100990311     c                   clear                   wEXPdfv
288200990311     c                   clear                   wEXPfgs
288300990311     c                   clear                   wEXPnpg
288400990311     c                   clear                   wEXPdef
288500990311     c                   clear                   wEXPscar
288600990205     c* cerco la spunta del collo con la data piu' alta e se > data arrivo
288700070131     c     kdcd          chain     fnbrv07l                           34
288800990311    1c     *in34         doweq     *off
288900060301     c
289000060301     c* Escludo, se richiesto, spunte autogenerate
289100060301   1ac                   if        (brvscar>0 and wautogen='N') or
289200060301     c                             wautogen=' '
289300000229     c* se data e ora scarico uguale a zeros muovo la data manutenzione spunte
289400000229      *
289500000229     c                   if        brvscar = 0
289600000229     c                   eval      brvdcs = brvdfs
289700000229     c                   eval      brvhcs = brvhms
289800000229     c                   endif
289900040906     c                   clear                   wcarica
290000000229      *
290100990205     c* escludo le pistole completamente fasulle, che non nascono nemmmeno
290200990205     c*   da un'altra spunta
290300990208     c                   movel     brvnps        w002a             2
290400990208     c     w002a         lookup    npsno                                  31
290500990311    2c     *in31         ifeq      *off
290600990205     c                   clear                   dslv53
290700990205     c                   movel     brvnpg        d53npg
290800000209     c                   z-add     brvnfv        d53nfv
290900990205     c                   movel     brvfgs        d53fgs
291000990205     c                   movel     brvfle        d53flf
291100990205     c                   call      'FNLV53R'
291200990205     c                   parm                    dslv53
291300990205     c** se non trovata data ignoro
291400990311    3c     d53err        ifeq      *blanks
291500990205     c     d53dfv        andge     wdatrifer
291600040906     c**
291700040907     c** Imposto campi di comodo  se stesso p.o. spunta  per vedere se tener
291800040907     c*   spunta partenza o spunta arrivi
291900040907     c                   if        brvfgs=wfgs
292000040906     c* campi nuova spunta letta
292100040906     c                   movel     brvnpg        wnewnpg           1
292200040906     c                   movel     d53def        wnewdef           1
292300040906     c                   movel     d53dfv        wnewdfv           8 0
292400040906     c                   movel     d53spg        wnewspg           1
292500040906     c                   movel     brvscar       wnewscar         12 0
292600040906     c* campi vecchia spunta memorizzata
292700040906     c                   movel     wnpg          woldnpg           1
292800040906     c                   movel     wdef          wolddef           1
292900040906     c                   movel     wdfv          wolddfv
293000040906     c                   movel     wspg          woldspg           1
293100040906     c                   movel     wscar         woldscar         12 0
293200040907     c                   EXSR      SCEGLIxma
293300040907     c                   else
293400040907    4c     d53dfv        ifgt      wdfv
293500040907     c     d53dfv        oreq      wdfv
293600040907     c     brvscar       andgt     wscar
293700040907     c                   eval      wcarica='S'
293800040907     c                   endif
293900040907     c                   endif
294000040906     c
294100040906     c** spunta da caricare
294200040906    5c                   if        wcarica='S'
294300990205     c**
294400040906    4c***  d53dfv        ifgt      wdfv
294500040906     c***  d53dfv        oreq      wdfv
294600040906     c***  brvscar       andgt     wscar
294700990210     c                   movel     d53dfv        wdfv
294800990308     c                   movel     d53def        wdef
294900990210     c                   movel     d53spg        wspg
295000990210     c                   movel     brvfgs        wfgs
295100990210     c                   movel     brvnpg        wnpg
295200040907     c                   z-add     brvnfv        wnfv
295300990210     c                   movel     brvscar       wscar
295400990311    4c                   endif
295500990311     c
295600040906     c* Se Export memorizzo la spunta defluenza con data >
295700990311    4c                   if        brvnpg=1 and d53def='S' and
295800990311     c                             lnafl1='E'
295900990311     c
296000990311    5c     d53dfv        ifgt      wEXPdfv
296100990311     c     d53dfv        oreq      wEXPdfv
296200990311     c     brvscar       andgt     wEXPscar
296300990311     c                   movel     d53dfv        wEXPdfv
296400990311     c                   movel     d53def        wEXPdef
296500990311     c                   movel     brvfgs        wEXPfgs
296600990311     c                   movel     brvnpg        wEXPnpg
296700990311     c                   movel     brvscar       wEXPscar
296800990311    5c                   endif
296900990311    4c                   endif
297000990311    c
297100990311    3c                   endif
297200990311    2c                   endif
297300060301   1ac                   endif
297400990205     c**
297500070131     c     kdcd          reade     fnbrv07l                               34
297600990311    1c                   enddo
297700990205     c
297800990205     c                   endsr
297900980506      *****************************************************************
298000980506      *   ROUTINE INIZIALE
298100980506      *****************************************************************
298200980506     C     *INZSR        BEGSR
298300980506      *
298400980506     C     *ENTRY        PLIST
298500980506     C                   PARM                    KPJBA
298600990204     C                   MOVEL     KPJBU         savkpjbu
298700170421
298800170421     c                   z-add     1             codut             1 0
298900990310     c**
299000990310     c* carico al primo posto al pistola 90
299100990311     c                   eval      npsno(1)='90'
299200990204     c**
299300990204     c* carico la tabella delle pistole fasulle
299400990204     c                   z-add     1             xx
299500990310     c                   z-add     2             yy                4 0
299600990204     c                   movel     '7J'          cod
299700990208     c     ktab2         chain     tabel                              31
299800990204     c     *in31         doweq     *off
299900990204     c                   movel     tbluni        ds7j
300000990204     c     §7jrps        ifeq      'A'
300100990204     C                   MOVEL     TBLKEY        NPS(XX)
300200990204     C                   ADD       1             XX
300300990204     C                   ENDIF
300400990303     c* Carico la tabella delle piu' fasulle 88 89 con priorita' 40
300500990205     c     §7jord        ifeq      040
300600990205     C                   MOVEL     TBLKEY        NPSNO(YY)
300700990205     C                   ADD       1             yy
300800990205     C                   ENDIF
300900990204     c**
301000990204     c     ktab2         reade     tabel                                  31
301100990204     C                   ENDDO
301200990209     c**
301300990305     c* Carico la tabella delle anomalie idd per la procedura danni
301400990305     c                   z-add     1             xx
301500990305     c                   movel     '7C'          cod
301600990305     c     ktab2         chain     tabel                              31
301700990305     c     *in31         doweq     *off
301800990305     c                   movel     tbluni        ds7c
301900990305     c     §7cdan        ifeq      'S'
302000990305     C                   MOVEL     TBLKEY        ANIDD(XX)
302100990305     C                   ADD       1             XX
302200990305     C                   ENDIF
302300990305     c**
302400990305     c     ktab2         reade     tabel                                  31
302500990305     C                   ENDDO
302600990211     c* Carico la tabella delle categorie che sono considerate ARRIVI
302700990210     c*   la defluenza adesso non la considero
302800990211     c                   eval      catarr(1)='2'
302900990211     c                   eval      catarr(2)='6'
303000990211     c* Carico la tabella delle categorie che sono considerate PARTENZA
303100990507     c                   eval      catpar(1)='1'
303200990211     c                   eval      catpar(2)='5'
303300990211     c                   eval      catpar(3)='3'
303400990209     c**
303500990210     c* carico la tabella dei tipi anomalie spunte gli "Ammanco"
303600990210     c                   z-add     1             xx
303700050615     c                   z-add     1             yy
303800050615     c                   z-add     1             zz
303900990210     c                   movel     '3E'          cod
304000990210     c     ktab2         chain     tabel                              31
304100990210     c     *in31         doweq     *off
304200990210     c                   movel     tbluni        ds3e
304300990330     c     §3efta        ifne      'V'
304400990330     C                   MOVEL     TBLKEY        AMSPU(XX)
304500990210     C                   ADD       1             XX
304600990210     C                   ENDIF
304700990330     c     §3efta        ifne      'A'
304800050615     C                   MOVEL     TBLKEY        AVSPU(yy)
304900050615     C                   ADD       1             yy
305000990330     C                   ENDIF
305100050615     c* Carico le anomalie spunte che di fatto non sono anomalie ma
305200170516     c*  servono per la chiusura distinta o come segnalazione
305300170516     c                   if        §3Efta='R' or §3Efta='S'
305400050615     C                   MOVEL     TBLKEY        NOANOMSPU(zz)
305500050615     C                   ADD       1             zz
305600050615     c                   endif
305700990210     c**
305800990210     c     ktab2         reade     tabel                                  31
305900990210     C                   ENDDO
306000010907      *
306100010907      * recupero la moneta corrente
306200010907     C                   CLEAR                   TIBS02DS
306300010907     C                   MOVEL     'C'           T02MOD
306400010907     C                   MOVEL     KNSIF         T02SIF
306500010907     C                   MOVEL     'GED'         T02COD
306600010907     C                   MOVEL     'DANNI'       T02KE1
306700010907     C                   CALL      'TIBS02R'
306800010907     C                   PARM                    KPJBA
306900010907     C                   PARM                    TIBS02DS
307000010910     C                   MOVEL     T02UNI        DGEDDN
307100990210     c**
307200990210     c* A seconda del tipo anomalia cerco la responsab.
307300990210     C                   clear                   tibs02ds
307400990210     C                   MOVEL     'C'           t02mod
307500990210     C                   MOVEL     knsif         t02sif
307600990210     C                   MOVEL     'STD'         t02cod
307700990210     C                   MOVEL(p)  '1'           t02ke1
307800010910     C                   MOVEL(P)  §gedDBA       t02ke2
307900990210     C                   CALL      'TIBS02R'
308000990210     C                   PARM                    KPJBA
308100990210     C                   PARM                    TIBS02DS
308200990210     c* non ci sono errori --> prendo i minuti di tolleranza per scarico spu
308300990210     c                   if        t02err=' '
308400990210     c                   movel     t02uni        dstd
308500990210     c                   else
308600990210     c                   eval      §stdmtl = 30
308700990210     c                   endif
308800170413      * Carico la VPODECO
308900170413     c                   clear                   tibs02ds
309000170413     c                   clear                   dvpodeco
309100170413     c                   eval      t02mod = 'C'
309200170413     c                   eval      t02sif = knsif
309300170413     c                   eval      t02cod = 'VPO'
309400170413     c                   eval      t02ke1 = 'DECO'
309500170413     c                   call      'TIBS02R'
309600170413     c                   parm                    kpjba
309700170413     c                   parm                    tibs02ds
309800170413     c                   if        t02err = *blanks
309900170413     c                   eval      dvpodeco = t02uni
310000170413     c                   else
310100170413     c                   eval      §vpoca  =20170501
310200170413     c                   endif
310300170413     c
310400051010     c
310500051010     c                   time                    wtempo
310600051010     c     *iso          move      wdata         dateu             8 0
310700051010     c
310800990209      * Accesso fndct fndcd
310900990129     c     kdct          klist
311000990128     c                   kfld                    i50aac
311100990128     c                   kfld                    i50fil
311200990128     c                   kfld                    i50nca
311300080714     c     kblp          klist
311400080714     c                   kfld                    dctaas
311500080714     c                   kfld                    dctlnp
311600080714     c                   kfld                    dctnrs
311700080714     c                   kfld                    dctnsp
311800090303     c     kar5          klist
311900090303     c                   kfld                    dctaas
312000090303     c                   kfld                    dctlnp
312100090303     c                   kfld                    dctnrs
312200090303     c                   kfld                    dctnsp
312300090303     c                   kfld                    ktrd
312400071018     c     kdcr2         klist
312500071018     c                   kfld                    i50aac
312600071018     c                   kfld                    i50fil
312700071018     c                   kfld                    i50nca
312800071018     c                   kfld                    w0070
312900990129     c     kdcr          klist
313000990129     c                   kfld                    i50aac
313100990129     c                   kfld                    i50fil
313200990129     c                   kfld                    i50nca
313300990216     c                   kfld                    wsegnadan
313400990216     c                   kfld                    wpor
313500990129     c     kTAB          klist
313600990129     c                   kfld                    codut
313700990129     c                   kfld                    cod
313800990129     c                   kfld                    key
313900990208     c     kTAB2         klist
314000990208     c                   kfld                    codut
314100990208     c                   kfld                    cod
314200990304     c     KTBE          klist
314300990304     c                   kfld                    wcod
314400990304     c                   kfld                    wlin
314500990304     c                   kfld                    wsif
314600990304     c                   kfld                    wke1
314700990304     c                   kfld                    wke2
314800990304     c     KTBE2         klist
314900990304     c                   kfld                    wcod
315000990304     c                   kfld                    wlin
315100990304     c                   kfld                    wsif
315200990304     c                   kfld                    wke1
315300990129     c     kDCD          klist
315400990129     c                   kfld                    dcdfls
315500990129     c                   kfld                    dcdlna
315600990129     c                   kfld                    dcdnrs
315700990208     c                   kfld                    dcdnsc
315800070711     c     kBRVE         klist
315900070821     c                   kfld                    wnpgspuan
316000070711     c                   kfld                    wnfvA
316100070821     c                   kfld                    wfgsspuan
316200070711     c                   kfld                    dcdfls
316300070711     c                   kfld                    dcdlna
316400070711     c                   kfld                    dcdnrs
316500070711     c                   kfld                    dcdnsc
316600990129     C     KCLN          KLIST
316700990129     C                   KFLD                    KTFP
316800990129     C                   KFLD                    KTFA
316900990129     C                   KFLD                    KAAA              4 0
317000990129     C                   KFLD                    KMMM              2 0
317100990305     c     keve          klist
317200990305     c                   kfld                    dctaae
317300990305     c                   kfld                    dctnev
317400990204     C                   MOVEL     savKPJBU      kpjbu
317500040907     c     kfgv          klist
317600040907     c                   kfld                    wnfv
317700040907     c                   kfld                    wfgs
317800980717      *
317900980506     C                   ENDSR
318000980508      *****************************************************************
318100980508** MSG  (Lungh. 78)                                                          *
318200990208Tipo anomalia inesistente                                                          1
318300990208C.A. inesistente                                                                   2
318400990216Impossibile aggiornare la C.A.: allocata da altro lavoro                           3
318500990208Impossibile calcolare Responsabilita':mancano dett.colli CA/Spunte/Riserva         4
318600990208Manca la tabella per il calcolo della responsabilita': AVVISARE IL CED !!!         5
318700990208Manca "num.colli danneggiati"e"num.colli della bolla":impossibile il calcolo       6
318800990208Non trovato dettaglio colli bolla per la C.A.                                      7
318900990208Tipo spedizione <> "A" e la responsabilita'non e'della PARTENZA: IMPOSSIBILE!!     8
