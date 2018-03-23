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
003400180103     D Opose           S             10    dim(2510)
003500990209     D
003600990208     D NPS             S              2    dim(90)
003700990208     D NPSNO           S              2    dim(90)
003800990209     D CATARR          S              1    dim(4)
003900990211     D CATPAR          S              1    dim(4)
004000990330     D AMSPU           S              1    dim(30)
004100990330     D AVSPU           S              1    dim(30)
004200050615     D Noanomspu       S              1    dim(50)
004300990305     D ANIDD           S              3  0 dim(50)
004400990210     D
004500990209     D WAAVNPG         S              1    dim(90)
004600021120     D WAAVAS          S              3    dim(90)
004700000322     D WAAVFGS         S              3  0 dim(90)
004800990209     D WAAVDFV         S              8  0 dim(90)
004900990209     D WAAVSCA         S             12  0 dim(90)
005000990209     D WAAVDEF         S              1    dim(90)
005100990210     D WAAVSPG         S              1    dim(90)
005200000322     D WAAVNPS         S              2  0 dim(90)
005300100609     d
005400100609     D  RESPO          s              1    dim(10)
005500100609     D  RESPC          s              5  2 dim(10)
005600040907     D                 DS
005700040907     D  FFV                    1    897
005800040907     D                                     DIM(299)
005900040907    +D  FGVFFV                 1    240
006000040907    +D  FGVFF2               241    447
006100040907    +D  W1FF3                448    687
006200040907    +D  W1FF4                688    897
006300040907     D                 DS
006400040907    +D  FLP                    1    897
006500040907     D                                     DIM(299)
006600040907    +D  FGVFLP                 1    240
006700040907    +D  FGVFL2               241    447
006800040907    +D  W1FL3                448    687
006900040907    +D  W1FL4                688    897
007000040907     D                 DS
007100980507      *--------------------------------------------
007200990129      * DS esterne
007300990129      *--------------------------------------------
007400980507     D KPJBA         E DS
007500990128     D FIDN50DS      E DS
007600990128     D TIBS02DS      E DS
007700990129     D DDCT01        E DS
007800990216     D DDCR01        E DS
007900080714     D DS1B          E DS
008000080714     D DS2A          E DS
008100990129     D DTAD          E DS
008200990208     D DS7J          E DS
008300990305     D DS7C          E DS
008400990210     D DS3E          E DS
008500990210     D DSTD          E DS
008600010910     D DGEDDN        E DS
008700080903     D OG143         E DS
008800080903     D OG148         E DS
008900170627     D tp_OG148      E DS                  extname(og148) prefix(tp)
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
010200170413     d dvpodeco      e ds
010300990129      *--------------------------------------------
010400990129      * DS interne
010500990129      *--------------------------------------------
010600990208     D                 DS
010700990129     D  dctaac                 1      4  0
010800990129     D  dctmgc                 5      8  0
010900990208     D  dctdca                 1      8  0
011000990129     D clnmat          DS
011100990208     D  MAT                    1     31    dim(31)
011200990129     D clnpom          DS
011300990208     D  pom                    1     31    dim(31)
011400990209     D                 DS
011500070131     D  brvdcs                 1      8  0
011600070131     D  brvhcs                 9     14  0
011700990209     D  brvscar                1     12  0
011800990209     D                 DS
011900990210     D  wdcs                   1      8  0
012000070131     D  whcs                   9     12  0
012100990210     D  WSCAR                  1     12  0
012200990311     D                 DS
012300990311     D  wEXPdcs                1      8  0
012400070131     D  wEXPhcs                9     12  0
012500990311     D  WEXPSCAR               1     12  0
012600990208     D                 DS
012700990208     D  kaaa                   1      4  0
012800990208     D  kmmm                   5      6  0
012900990208     D  kgg                    7      8  0
013000990208     D  kDATA                  1      8  0
013100090120     D                 DS
013200090120     D  blpaas                 1      4  0
013300090120     D  blpmgs                 5      8  0
013400090120     D  blpdsp                 1      8  0
013500990216     D                 DS
013600990216     D  WPOR                   1      3  0
013700990216     D  WSEGNADAN              4     10
013800090324     D  WTER                  11     13  0
013900180103     D  Wpose                  1     10
014000180103     D  WOKEY                  1     13
014100990129      *--------------------------------------------
014200990129      * definizione campi
014300990129      *--------------------------------------------
014400990208     D WDATAFASE       S               D   DATFMT(*iso)
014500990208     D WDATAANM        S               D   DATFMT(*iso)
014600050204     d Datadmy         s               d   datfmt(*dmy)
014700990212     D                 DS
014800990212     D WTEMPO                          Z   inz(Z'1999-01-01-01.00.00.000000')
014900990212     D  WDATA                          D   DATFMT(*iso) OVERLAY(WTEMPO)
015000990212     D  WORA                           T   TIMFMT(*iso) OVERLAY(WTEMPO:12)
015100990211     D
015200130513     D areater         S                   LIKE(orgcar)
015300130513     D nfvpar          S                   LIKE(wnfv)
015400060309     D fgspar          S                   LIKE(wfgs)
015500060309     D brvas           S                   LIKE(§OGAEX)
015600000322     D WSAVDCH         S                   LIKE(anmdch)
015700990305     D WSAVCCH         S                   LIKE(anmcch)
015800990305     D WSAVDAO         S                   LIKE(anmdao)
015900990305     D WCOD            S                   LIKE(t02cod)
016000990304     D WLIN            S                   LIKE(t02lin)
016100990304     D WSIF            S                   LIKE(t02sif)
016200990304     D WKE1            S                   LIKE(t02ke1)
016300070711     D SAVWKE1         S                   LIKE(t02ke1)
016400990209     D WKE2            S                   LIKE(t02ke2)
016500990129     D WNCN            S                   LIKE(dctncn)
016600990129     D WCOLLODAN       S                   LIKE(dctncn)
016700990129     D WPER            S                   LIKE(dctper)
016800090303     D Percen          S                   LIKE(dctper)
016900990129     D KTFA            S                   LIKE(clntfa)
017000990129     D KTFP            S                   LIKE(clntfp)
017100000321     D FLPTFA          S                   LIKE(clntfp)
017200990129     D WTCR            S                   LIKE(dcttcr)
017300990129     D cod             S                   LIKE(tblcod)
017400990129     D key             S                   LIKE(tblkey)
017500990204     D SAVKPJBU        S                   LIKE(kpjbu)
017600990210     D WDFV            S                   LIKE(d53dfv)
017700990311     D WEXPDFV         S                   LIKE(d53dfv)
017800990210     D WSPG            S                   LIKE(d53spg)
017900990210     D WFGS            S                   LIKE(d53fgs)
018000040907     D WNFV            S                   LIKE(d53nfv)
018100070711     D WNFVA           S                   LIKE(BRVNFV)
018200990311     D WEXPFGS         S                   LIKE(d53fgs)
018300990210     D WNPG            S                   LIKE(d53npg)
018400990311     D WEXPNPG         S                   LIKE(d53npg)
018500990212     D WERR            S              1
018600990329     D WRIT            S              1
018700990205     D WDATRIFER       S                   LIKE(d53dfv)
018800990208     D WFLP            S                   LIKE(artflp)
018900070821     D WNPGSPUAN       S                   LIKE(BRVNPG)
019000070821     D WFLPSPUAN       S                   LIKE(artflp)
019100990309     D WFGSSPUAN       S                   LIKE(artflp)
019200990305     D WCLIP           S                   LIKE(artflp)
019300990305     D WCLIA           S                   LIKE(artflp)
019400990208     D WDCTLNPC        S                   LIKE(dctlnp)
019500990210     D WDEF            S                   LIKE(d53def)
019600990311     D WEXPDEF         S                   LIKE(d53def)
019700990210     D WPRECNPG        S                   LIKE(brvnpg)
019800990210     D WPRECFGS        S                   LIKE(brvfgs)
019900990210     D WPRECSPG        S                   LIKE(d53spg)
020000990304     D LNPFL1          S                   LIKE(orgfl1)
020100080903     D lnpntw          S                   LIKE(§ogntw)
020200150604     D lnantw          S                   LIKE(§ogntw)
020300131018     D lnpMAMntw       S                   LIKE(§ogntw)
020400990304     D LNAFL1          S                   LIKE(orgfl1)
020500100610     D lnptfp          S                   LIKE(i50tfp)
020600990210     D WPREC200        S             20  0
020700000322     D WSTESSOAS       S              1
020800060301     D Wautogen        S              1
020900000322     D WOK             S              1
021000000322     D JJ              S              3  0
021100990305     D WAPERT          S              1
021200050204     d wggmmaa         s              6
021300050204     d wgioset         s              1
021400090422     d soloCAN         s              1
021500131018     d Wes_mem         s              1
021600090303     d ktrd            s                   like(ar5trd) inz('GEN')
021700990129      *----------------------------------------------------------
021800990129      *  RIEPILOGO INDICATORI
021900980908      *----------------------------------------------------------
022000990204     C                   MOVEL     KPJBU         FIDN50DS
022100990209     C                   clear                   ofil
022200990209     C                   clear                   oper
022300990209     C                   clear                   otcr
022400051010     C                   clear                   Wricalcolo
022500131018     C                   clear                   Wes_mem
022600990128     c**
022700990128     c** C A L C O L O   L A   R E S P O N S A B I L I T A'
022800990128     c**
022900990129    1c     I50MOD        ifeq      'C'
023000990129     c                   clear                   o50por
023100990129     c                   clear                   o50per
023200990129     c                   clear                   o50car
023300990129     c                   clear                   o50tcr
023400990129     c                   clear                   o50msg
023500990129     c                   clear                   o50err
023600990128     c** chain in update la testata
023700990128     c     kdct          chain     fndct01l                           3099
023800990128     c* non trovata C.A.
023900990129    2c     *in30         ifeq      *on
024000990129     c                   movel     msg(2)        o50msg
024100990128     c                   movel     'E'           o50err
024200990128     c                   goto      fine
024300990129    2c                   endif
024400990128     c* record allocato: impossibile l'aggiornamento
024500990129    2c     *in99         ifeq      *on
024600990129     c                   movel     msg(3)        o50msg
024700990128     c                   movel     'E'           o50err
024800990128     c                   goto      fine
024900990129    2c                   endif
025000990129     c* IMPOSTO I COLLI DANNEGGIATI MAMCANTI: se manca il dato imposto
025100990129     c*   tutti i colli della bolla
025200990208    2c     dctncn        ifeq      0
025300990129     c     i50ncl        andeq     0
025400990208     c                   movel     msg(4)        o50msg
025500990129     c                   movel     'E'           o50err
025600990129     c                   goto      fine
025700990129     c***
025800990129   x2c                   else
025900990208    3c     dctncn        ifgt      0
026000990129     c                   z-add     dctncn        wncn
026100990129     c                   else
026200990129     c                   z-add     i50ncl        wncn
026300990129    3c                   endif
026400990129    2c                   endif
026500071018     c
026600071018     c* Conto quanti record ci sono nel file delle responsabilità
026700071018     c                   clear                   wcollidcr         6 2
026800071018     c     kdct          chain(N)  fndcr02l                           30
026900071018    2c     *in30         doweq     *off
027000071018     c     dcratb        ifeq      ' '
027100071018     c                   add       dcrper        wcollidcr
027200071018     c                   endif
027300071018     c
027400071018     c     kdct          reade(N)  fndcr02l                               30
027500071018    2c                   enddo
027600071018     c**
027700071018     c** Se si tratta di calcolo manuale controllo solo se devo adeguar
027800071018     c** il numero colli di DCR con DCD
027900071018     c                   if        dctcar='M'
028000071018     c                   if        wcollidcr<>wncn
028100071018     c                   exsr      AllineaDCRDCD
028200071018     c                   endif
028300071018     c
028400071018     c                   goto      FINE
028500071018     c                   endif
028600990129     c**
028700990129     c                   movel     dctflo        ddct01
028800990208     c                   clear                   wdctlnpc
028900990208     c     §dctlnpc      ifgt      *zeros
029000990208     c                   movel     §dctlnpc      wdctlnpc
029100990208     c                   endif
029200990304     c
029300050914     c* Verifico se lnp c.a. bolla mamma estera --> IMPORT
029400050914     C     i50lnpMAM     chain     azorg01l                           39
029500990304     c   39              clear                   orgfl1
029600080903     c   39              clear                   og143
029700990304     c                   eval      LNPFL1=orgfl1
029800131018     c                   eval      lnpMAMntw=§ogntw
029900131018
030000131018     c* Verifico ntw della bolla
030100131018     C     i50lnp        chain     azorg01l                           39
030200131018     c  n39              movel     orgde3        og143
030300131018     c   39              clear                   og143
030400131018     c                   eval      lnpntw=§ogntw
030500990304     c
030600990304     c* Verifico se lna estera --> EXPORT
030700990304     C     i50lna        chain     azorg01l                           39
030800990304     c   39              clear                   orgfl1
030900150604     c   39              clear                   og143
031000150604     c   39              clear                   orgde8
031100990308     c   39              clear                   orgfel
031200990304     c
031300150604     c  n39              movel     orgde3        og143
031400990304     c                   movel     orgde8        OG148
031500990304     c                   eval      LNAFL1=orgfl1
031600150604     c                   eval      LNAntw=§ogntw
031700990304     c**
031800990304     c* A seconda del tipo anomalia cerco la responsab.
031900990304     C                   clear                   tibs02ds
032000990304     C                   MOVEL     'C'           t02mod
032100990304     C                   MOVEL     knsif         t02sif
032200990304     C                   MOVEL     'TAD'         t02cod
032300990304     C                   MOVEL     dcttad        t02ke1
032400990304     C                   CALL      'TIBS02R'
032500990304     C                   PARM                    KPJBA
032600990304     C                   PARM                    TIBS02DS
032700990305     c
032800990305   1aC                   IF        t02err = *BLANKS
032900990305     C                   movel     T02uni        DTAD
033000090120
033100090120     c* chaino la bolla in partenza per prendere il cod trattamento merce
033200090120     c                   clear                   ds1b
033300090120     c                   clear                   blpctm
033400090120     c                   clear                   blpdsp
033500090120     c     kblp          chain     fnblp01l
033600090120     c                   if        not %found(fnblp01l)
033700090120     c     kblp          chain     fnarb01l
033800090120     c                   endif
033900090120     c                   if        blpctm<>*blanks
034000090120     c                   movel     '1B'          cod
034100090120     c                   movel     blpctm        key
034200090120     c     ktab          chain     tabel
034300090120     c                   if        %found(tabel00f)
034400090120     c                   movel     tbluni        ds1b
034500090120     c                   endif
034600090120     c                   endif
034700090302     c
034800090302     c* Verifico se bolla con merce di valore
034900090302     c     kar5          chain(N)  fiar501l
035000090302     c                   if        %found(fiar501l)
035100090302     c                   movel     ar5uni        dar5gen
035200090302     c                   else
035300090302     c                   clear                   dar5gen
035400090302     c                   endif
035500990305     c**
035600990305     c* 1) Se si tratta di evento : "SMARRIMENTO AUTOTRASPORTATORE"
035700990305     c** colpa di chi apre
035800990305    2c                   if        dctnev>0
035900990305     c     keve          chain     fndet01l                           34
036000990305    3c                   if        not *in34  AND
036100990305     c                             dettad='15'
036200990305    4c                   if        lnpfl1='E'
036300990305     c                   MOVEL(p)  'SMARRAUT_E'  wke1
036400990305   x4c                   else
036500990305     c                   MOVEL(p)  'SMARRAUT'    wke1
036600990305    4c                   endif
036700990308     c
036800990308     c                   z-add     wncn          wcollodan
036900990308     c                   clear                   wsegnadan
037000990305     c                   exsr      MEMRES
037100990305     c                   exsr      WRIRES
037200990305     c                   goto      fine
037300990305    3c                   endif
037400990305    2c                   endif
037500990308     c**
037600990308     c* 2) RESPONSABILITA' PARTNER
037700990305     c**
037800990308     c                   if        dctres='P'
037900990308     c                   if        lnpfl1='E'
038000990308     c                   MOVEL(p)  'COLPAPART_E' wke1
038100990308     c                   else
038200990308     c                   MOVEL(p)  'COLPAARRIVO' wke1
038300990308     c                   move      '_E  '        wke1
038400990308     c                   endif
038500990308     c
038600990308     c                   z-add     wncn          wcollodan
038700990308     c                   clear                   wsegnadan
038800990308     c                   exsr      MEMRES
038900990308     c                   exsr      WRIRES
039000990308     c                   goto      fine
039100990308     c                   endif
039200990308     c**
039300990308     c* 3) C.A. APERTA DALLA PARTENZA
039400990128     c**
039500000322     c* Se bolla locale lnp=lna ma franco senza riserva, dopo consegna,
039600000322     c*  come se fosse aperta lo stesso in partenza
039700050914    2c     i50fil        ifeq      i50lnpMAM
039800050914     c     i50lnpMAM     andeq     i50lna
039900000322     c     §tadtico      andeq     'C'
040000000322     c     dctcrc        andne     'S'
040100000322     c     §dctport      andeq     'F'
040200000322     c                   eval      wapert='P'
040300000323     c                   z-add     wncn          wcollodan
040400000323     c                   clear                   wsegnadan
040500000322     c                   exsr      CLIAPERTURA
040600000322     c                   exsr      MEMRES
040700000322     c                   exsr      WRIRES
040800000322     c                   goto      fine
040900000322     c                   endif
041000000322     c
041100050914     c* Verifico se il P.O. di apertura c.a. = a lnp bolla mamma se legata
041200050914    2c     i50fil        ifeq      i50lnpMAM
041300050914     c     i50lnpMAM     andne     i50lna
041400990304     c
041500990305    3c                   select
041600990304     c* 1) Spedizione consegnata con riserva
041700990304     c                   when      §tadtico='C'  AND
041800990304     c                             dctCRC='S'
041900990304     c
042000131018     c****               exsr      ERRAPERTURA
042100131021     c* Se si tratta di bolle dirottata lascio la responsab. errore di apertura
042200131021     c                   if        blpcca='1'
042300131021     c                   exsr      ERRAPERTURA
042400131021     c                   else
042500131021     c* altrimenti vado sulle spunte
042600131018     c                   exsr      elabora
042700131021     c                   endif
042800990304     c
042900990304     c* 2) Spedizione consegnata senza riserva
043000990304     c                   when      §tadtico='C'  AND
043100990304     c                             dctcrc<>'S'
043200990304     c* Assegnato--> errore
043300990305    4c                   if        §dctport='A'
043400990304     c                   exsr      ERRAPERTURA
043500990304     c                   else
043600990304     c* Franco --> Colpa del mittente
043700990305     c                   eval      wapert='P'
043800990304     c                   exsr      CLIAPERTURA
043900990305    4c                   endif
044000990304     c
044100990305     c* 3) Spedizione aperta in partenza ma non consegnata per i colli in CA
044200990305     c                   when      §tadtico<>'C'
044300990304     c
044400990305    4c                   if        lnpfl1='E'
044500990304     c                   MOVEL(p)  'APERTAPAR_E' wke1
044600990304     c                   else
044700990211     c                   MOVEL(p)  'APERTAPAR'   wke1
044800990305    4c                   endif
044900990305    3c                   endsl
045000990304     c
045100990129     c                   z-add     wncn          wcollodan
045200990216     c                   clear                   wsegnadan
045300990129     c**
045400131018     c                   if        wes_mem=' '
045500990129     c                   exsr      MEMRES
045600131018     c                   endif
045700990129     c**
045800990129     c     o50err        ifeq      'E'
045900990129     c                   goto      fine
046000990129     c                   endif
046100990129     c**
046200990129   x2c                   else
046300990305     c**
046400990305     c** Sono in ARRIVO: controllo solo le spedizioni consegnate senza RIS
046500990305     c
046600990305    3c                   if        §tadtico='C'  AND
046700990310     c                             dctcrc<>'S'   AND
046800990310    4c                             §dctport='A'
046900990305     c* Assegnato --> Colpa del mittente
047000990305     c                   eval      wapert='A'
047100990305     c                   exsr      CLIAPERTURA
047200990305     c
047300990308     c                   z-add     wncn          wcollodan
047400990308     c                   clear                   wsegnadan
047500990308     c
047600990305     c                   exsr      MEMRES
047700990305     c
047800990305   x3c                   else
047900130307     c*
048000130307     c                   exsr      elabora
048100990128     C**
048200990305    3c                   endif
048300990129    2c                   endif
048400990305  x1ac                   else
048500990305     c* ERRORE --> NON TROVATO TIPO ANOMALIA
048600990305     c                   movel     msg(1)        o50msg
048700990305     c                   goto      FINE
048800990305   1ac                   endif
048900990129    1c                   endif
049000990208      **
049100071018     c** Memorizzo la responsabilita'
049200990216     c                   if        o50err=' '
049300990208     c                   exsr      WRIRES
049400990216     c                   endif
049500990128     c     fine          tag
049600990128     c**
049700990208      * Chiudo pgm aperti
049800131018     c                   clear                   tibs02ds
049900990128     c                   movel     'C'           t02tla
050000990128     c                   call      'TIBS02R'
050100990128     c                   parm                    kpjba
050200990128     c                   parm                    tibs02ds
050300990208     c**
050400990208     c                   clear                   dslv53
050500990208     c                   movel     'C'           d53tla
050600990208     c                   call      'FNLV53R'
050700990208     c                   parm                    dslv53
050800990208     c**
050900990208     c                   clear                   dslv55
051000990208     c                   movel     'C'           d55tla
051100990208     c                   call      'FNLV55R'
051200990208     c                   parm                    dslv55
051300990128     c**
051400990212     C                   MOVEL     fidn50ds      kpjbu
051500980506     C                   EVAL      *INLR = *ON
051600130307      *****************************************************************
051700130307     c
051800130307     c     Elabora       BEGSR
051900130307     c**
052000130307     c** 3) Se non c'e' il dettaglio colli e non c'e' riserva in bolla
052100130307     c** --> non calcolo la responsabilita'
052200130307     c     kdct          chain     fndcd01l                           30
052300130307    4c     *in30         doweq     *off
052400130307     c     dcdatb        andne     ' '
052500130307     c     kdct          reade     fndcd01l                               30
052600130307    4c                   enddo
052700130307     c**
052800130307    4c     *in30         ifeq      *on
052900130307     c                   z-add     wncn          wcollodan
053000130307     c                   clear                   wsegnadan
053100130307     c                   exsr      CT_RISERVA
053200130307     c     o50err        ifeq      *blanks
053300130307     c                   exsr      WRIRES
053400130307     c                   endif
053500130307     c                   goto      fine
053600130307    4c                   endif
053700130307     c***
053800130307     c* leggo fndcd per MANCANZE PER vedere se hanno aperto in ritardo la CA
053900130307     c                   clear                   writ
054000130307    4c     §tadragr      ifeq      'M'
054100130307     c     lnafl1        andne     'E'
054200130307     c
054300130307     c     kdct          chain     fndcd01l                           30
054400130307    5c     *in30         doweq     *off
054500130307     c     writ          andne     'N'
054600130307     c** escludo se annullata e
054700130307    6c     dcdatb        ifeq      ' '
054800130307     c     dcddch        andeq     dctdch
054900130307     c*
055000130307     c* non trovato dett.bolla --> errore
055100130307     c     kdcd          chain     fnart27l                           34
055200130307     c*
055300130307    7c     *in34         ifeq      *on
055400130307     c                   movel     msg(7)        o50msg
055500130307     c                   movel     'E'           o50err
055600130307     c                   goto      fine
055700130307    7c                   endif
055800130307     c**
055900130307     c                   eval      wcollodan=1
056000130307     c                   movel     dcdnsc        wsegnadan         7
056100130307     c
056200130307     c                   exsr      RITARDO
056300130307    6c                   endif
056400130307     c**
056500130307     c     kdct          reade     fndcd01l                               30
056600130307    5c                   enddo
056700130307    4c                   endif
056800130307      *
056900130307     c***
057000130307     c* leggo fndcd
057100130307     c     kdct          chain     fndcd01l                           30
057200130307    5c     *in30         doweq     *off
057300130307     c** escludo se annullata e
057400130307    6c     dcdatb        ifeq      ' '
057500130307     c     dcddch        andeq     dctdch
057600130307     c*
057700130307     c* non trovato dett.bolla --> errore
057800130307     c     kdcd          chain     fnart27l                           34
057900130307     c*
058000130307    7c     *in34         ifeq      *on
058100130307     c                   movel     msg(7)        o50msg
058200130307     c                   movel     'E'           o50err
058300130307     c                   goto      fine
058400130307    7c                   endif
058500130307     c**
058600130307     c                   eval      wcollodan=1
058700130307     c                   movel     dcdnsc        wsegnadan         7
058800130307     c
058900130307    7c     §tadragr      ifeq      'M'
059000130307     c                   exsr      MANCANZE
059100130307     c                   else
059200130307     c*
059300130307     c                   exsr      AMMAVARIE
059400130307    7c                   endif
059500130307    6c                   endif
059600130307     c**
059700130307     c     kdct          reade     fndcd01l                               30
059800130307    5c                   enddo
059900130307     c                   ENDSR
060000070711      *****************************************************************
060100070711      * Verifico se doppia spunta per ammanchi/avarie
060200070711      *****************************************************************
060300070711     c     CTRDOPSPU     begsr
060400080714     c* non effettuo il controllo se sonon in entrata e si tratta di
060500080714     c**  bolla con disk c
060600081021     c                   if        wnpgspuan<>5 or §1bdkc=' '
060700070711     c
060800070711     c     kbrve         chain     fnbrve1l
060900070711    1c                   if        %found(fnbrve1l) and E_brvcan=' '
061000070711     c
061100070713     c                   eval      savwke1=wke1
061200070711     c**
061300070711     c                   clear                   wsif
061400070711     c                   clear                   wlin
061500070711     c                   eval      wcod='TRS'
061600070711     c                   eval      wke2='99991231       '
061700070821     c                   eval      wke1='A_CAN_DOPARR   '
061800070711     c
061900070711     c                   clear                   WOK
062000070711     c     ktbe          setgt     tntbe02l
062100070711     c     ktbe2         readpe    tntbe02l                               39
062200070711    2c                   dow       not *in39  and  WOK<>'S'
062300070711     c                   if        tbeatb=' '
062400070711     c                   movel     tbeke2        w0080             8 0
062500070711     c     w0080         ifle      dctdca
062600070711     c                   eval      wok='S'
062700070711     c                   seton                                        39
062800070711     c                   endif
062900070711     c                   endif
063000070711     c
063100070711     c  N39ktbe2         readpe    tntbe02l                               39
063200070711    2c                   enddo
063300070711     c**
063400070711     c**
063500070711     c** Non trovato record in tabella responsabilita'
063600070711     c** procedo con l'attuale calcolo della responsabilità
063700070711    2C                   IF        wok=' '
063800070713     c                   eval      wke1=savwke1
063900070711   x2c                   else
064000070711     c* vado avanti con il nuovo codice di responsabilità
064100070711     c                   movel     tbeuni        DTRS
064200070713     c* In WFLP memorizzo il p.o. che ha rilevato l'anomalia
064300070821     c                   eval      wflp=wfgsspuan
064400090831     c                   movel     'A'           wtipoflp
064500070711    2c                   endif
064600070711    1c                   endif
064700070711     c
064800080714     c                   endif
064900070711     c                   ENDSR
065000990129      *****************************************************************
065100990129      * Cerco record e memorizzo la responsabilita'
065200990129      *****************************************************************
065300990129     c     MEMRES        begsr
065400990217     C                   clear                   wpor
065500130513     C                   clear                   areater
065600131018     c                   eval      wes_mem=' '
065700990304     c**
065800990304     c                   clear                   wsif
065900990304     c                   clear                   wlin
066000990304     c                   eval      wcod='TRS'
066100990304     c                   eval      wke2='99991231       '
066200990304     c
066300990304     c                   clear                   WOK
066400990304     c     ktbe          setgt     tntbe02l
066500990304     c     ktbe2         readpe    tntbe02l                               39
066600990304     c                   dow       not *in39  and  WOK<>'S'
066700990304     c                   if        tbeatb=' '
066800990304     c                   movel     tbeke2        w0080             8 0
066900990304     c     w0080         ifle      dctdca
067000990304     c                   eval      wok='S'
067100990308     c                   seton                                        39
067200990304     c                   endif
067300990304     c                   endif
067400990304     c
067500990308     c  N39ktbe2         readpe    tntbe02l                               39
067600990304     c                   enddo
067700990129     c**
067800990129     c**
067900990129     c** Non trovato record in tabella responsabilita'
068000070711    1C                   IF        wok=' '
068100990208     c                   movel     msg(5)        o50msg
068200990129     c                   movel     'E'           o50err
068300070711   x1c                   else
068400990129     c*
068500990304     c                   movel     tbeuni        dtrs
068600070711     c* Se si tratta do codice che controlla se "fa il furbo", per
068700070711     c*  ammanchi e avarie, efettuo il controllo
068800070711     c*  se già in linea il nuovo codice atribuzione reponsabilità
068900070711     c                   if        §trsdopspu='S'
069000070711     c                   exsr      CTRDOPSPU
069100070711     c                   endif
069200100609
069300100610     c* Se il codice prevede differenziazione responsab. tra Franchi / Assegnati / triangolazioni
069400100610     c                   clear                   respo
069500100610     c                   clear                   respc
069600100610     c
069700100610     c*  imposto la tabella per triangolazioni se c'e' e se lo  la bolla
069800100610    2c                   if        respot(1) <>' '
069900100610     c
070000100610     c* Per franchi
070100100610    3c                   if        §dctport='F'
070200100610     c* Terminal di partenza lnp C.A. e terminal di partenza lin ksc a confronto
070300100610     c                   clear                   dslv55
070400100610     c                   movel     'P'           d55tpt
070500100610     c                   z-add     wdctlnpc      d55lin
070600100610     c                   if        blpdsp>0
070700100610     c                   z-add     blpdsp        d55drf
070800100610     c                   else
070900100610     c                   z-add     dctdca        d55drf
071000100610     c                   endif
071100100610     c                   call      'FNLV55R'
071200100610     c                   parm                    dslv55
071300100610     c                   movel     d55tfp        lnptfp
071400100610     c*
071500100610     c                   z-add     wclip         d55lin
071600100610     c                   call      'FNLV55R'
071700100610     c                   parm                    dslv55
071800100610    4c                   if        d55tfp<>lnptfp
071900100610     c                   movea     respot        respo
072000100610     c                   movea     respct        respc
072100130513     c
072200130513     c*Se sono diversi devo verificare che anche le rispettive aree di appartenenza
072300130513     c*  siano diverse
072400130513     C     lnptfp        chain     azorg01l                           39
072500130513    5c                   if        %found(azorg01l)
072600130513     c                   eval      areater=orgcar
072700130513     C     d55tfp        chain     azorg01l                           39
072800130513    6c                   if        %found(azorg01l) and orgcar<>areater
072900130513     c                   movea     respot        respo
073000130513     c                   movea     respct        respc
073100130513    6c                   endif
073200130513    5c                   endif
073300100610    4c                   endif
073400100610     c
073500100610   x3c                   else
073600100610     c                   clear                   dslv55
073700100610     c                   movel     'A'           d55tpt
073800100610     c                   z-add     wclia         d55lin
073900100610     c                   if        blpdsp>0
074000100610     c                   z-add     blpdsp        d55drf
074100100610     c                   else
074200100610     c                   z-add     dctdca        d55drf
074300100610     c                   endif
074400100610     c                   call      'FNLV55R'
074500100610     c                   parm                    dslv55
074600100610     c*
074700100610    4c                   if        i50tfa<>d55tfa
074800130513     c*Se sono diversi devo verificare che anche le rispettive aree di appartenenza
074900130513     c*  siano diverse
075000130513     C     i50tfa        chain     azorg01l                           39
075100130513    5c                   if        %found(azorg01l)
075200130513     c                   eval      areater=orgcar
075300130513     C     d55tfa        chain     azorg01l                           39
075400130513    6c                   if        %found(azorg01l) and orgcar<>areater
075500100610     c                   movea     respot        respo
075600100610     c                   movea     respct        respc
075700130513    6c                   endif
075800130513    5c                   endif
075900130513    4c                   endif
076000130513     c
076100100610    3c                   endif
076200100610    2c                   endif
076300100610     c
076400100610    2c                   if        respo(1) = ' '
076500100610     c* Per Assegnati
076600100610     c* Terminal di arrivo lna C.A. e terminal di arrivo lin ksc a confronto
076700100610     c
076800100610     c*  imposto le tabelle di fil e codici giuste, altrimenti la prima (quella dei franchi)
076900100610    3c                   if        respoa(1) <>' ' and §dctport='A'
077000100609     c                   movea     respoa        respo
077100100609     c                   movea     respca        respc
077200100609     c                   else
077300100609     c                   movea     respof        respo
077400100609     c                   movea     respcf        respc
077500100610    3c                   endif
077600100610    2c                   endif
077700990129     c*
077800990218     c                   z-add     1             xx                4 0
077900070711    2c     respo(xx)     downe     *blanks
078000040907     c*
078100040907     c                   if        respo(xx) = 'V' and wnfv=0
078200040907     c                   eval      respo(xx) = 'A'
078300040907     c                   endif
078400990303     c
078500990129     c                   select
078600990308     c** PO e' "F" --> terminal partenza p.o. apertura c.a.
078700990303     c
078800990208     c     respo(xx)     wheneq    'F'
078900990303     c                   clear                   dslv55
079000990303     c                   movel     'P'           d55tpt
079100990303     c                   z-add     dctfil        d55lin
079200090325     c                   if        blpdsp>0
079300090325     c                   z-add     blpdsp        d55drf
079400090325     c                   else
079500090325     c                   z-add     dctdca        d55drf
079600090325     c                   endif
079700990303     c                   call      'FNLV55R'
079800990303     c                   parm                    dslv55
079900990303     c                   z-add     d55tfp        wpor
080000090324     c                   z-add     d55tfp        wter
080100990303     c
080200990129     c* se = a lnp dct e' P altrimenti e' "T"
080300990308     c     dctfil        ifeq      wdctlnpc
080400990129     c                   movel     'P'           wtcr
080500990308     c                   else
080600990308     c     dctfil        ifeq      dctlna
080700990308     c                   movel     'A'           wtcr
080800990129     c                   else
080900990129     c                   movel     'T'           wtcr
081000990129     c                   endif
081100990308     c                   endif
081200990303     c
081300990308     c** PO e' "C" --> p.o. apertura c.a.
081400990308     c     respo(xx)     wheneq    'C'
081500990308     c                   z-add     dctfil        wpor
081600090831     c                   clear                   wtipoflp
081700990308     c
081800990308     c* se = a lnp dct e' P altrimenti e' "T"
081900990308     c     dctfil        ifeq      wdctlnpc
082000990308     c                   movel     'P'           wtcr
082100990308     c                   else
082200990308     c     dctfil        ifeq      dctlna
082300990308     c                   movel     'A'           wtcr
082400990308     c                   else
082500990308     c                   movel     'T'           wtcr
082600990308     c                   endif
082700990308     c                   endif
082800090324     c
082900090324     c                   EXSR      CallTER
083000990308     c
083100090120     c** PO e' "P" --> teminal partenza di lnp bolla originale
083200990208     c     respo(xx)     wheneq    'P'
083300990303     c                   clear                   dslv55
083400990303     c                   movel     'P'           d55tpt
083500990303     c                   z-add     wdctlnpc      d55lin
083600090120     c                   if        blpdsp>0
083700090120     c                   z-add     blpdsp        d55drf
083800090120     c                   else
083900090120     c                   z-add     dctdca        d55drf
084000090120     c                   endif
084100990303     c                   call      'FNLV55R'
084200990303     c                   parm                    dslv55
084300990303     c                   z-add     d55tfp        wpor
084400990129     c                   movel     'P'           wtcr
084500090324     c                   z-add     d55tfp        wter
084600131018
084700131018     c** PO e' "O" -->  lnp bolla originale
084800131018     c     respo(xx)     wheneq    'O'
084900131018     c                   clear                   dslv55
085000131018     c                   movel     'P'           d55tpt
085100131018     c                   z-add     wdctlnpc      d55lin
085200131018     c                   if        blpdsp>0
085300131018     c                   z-add     blpdsp        d55drf
085400131018     c                   else
085500131018     c                   z-add     dctdca        d55drf
085600131018     c                   endif
085700131018     c                   call      'FNLV55R'
085800131018     c                   parm                    dslv55
085900131018     c                   z-add     wdctlnpc      wpor
086000131018     c                   movel     'P'           wtcr
086100131018     c                   z-add     d55tfp        wter
086200990303     c
086300090324     c** PO e' "1" --> terminal partenza del codice cliente mitt o dest
086400990305     c     respo(xx)     wheneq    '1'
086500990305     c                   clear                   dslv55
086600990305     c                   movel     'P'           d55tpt
086700990305     c                   if        wclip>0
086800990305     c                   z-add     wclip         d55lin
086900990305     c                   movel     'P'           wtcr
087000990305     c                   else
087100990305     c                   z-add     wclia         d55lin
087200990305     c                   movel     'A'           wtcr
087300990305     c                   endif
087400990305     c
087500090325     c                   if        blpdsp>0
087600090325     c                   z-add     blpdsp        d55drf
087700090325     c                   else
087800090325     c                   z-add     dctdca        d55drf
087900090325     c                   endif
088000990305     c                   call      'FNLV55R'
088100990305     c                   parm                    dslv55
088200990305     c                   z-add     d55tfp        wpor
088300090324     c                   z-add     d55tfp        wter
088400990305     c
088500990309     c** PO e' "2" --> p.o. codice cliente mitt o dest
088600990309     c     respo(xx)     wheneq    '2'
088700990309     c                   if        wclip>0
088800990309     c                   z-add     wclip         wpor
088900990309     c                   movel     'P'           wtcr
089000990309     c                   else
089100990309     c                   z-add     wclia         wpor
089200990309     c                   movel     'A'           wtcr
089300990309     c                   endif
089400090831     c
089500090831     c                   clear                   wtipoflp
089600090324     c                   exsr      CallTER
089700990309     c
089800990303     c** PO e' "A" --> terminal arrivo di lna bolla
089900990208     c     respo(xx)     wheneq    'A'
090000000321     c                   if        flptfa>0
090100000321     c                   movel     flptfa        wpor
090200000321     c                   else
090300990205     c                   movel     i50tfa        wpor
090400000321     c                   endif
090500990129     c                   movel     'A'           wtcr
090600090324     c                   movel     wpor          wter
090700990303     c
090800990303     c** PO e' "R" --> terminal partenza di lna bolla
090900990303     c     respo(xx)     wheneq    'R'
091000990303     c                   clear                   dslv55
091100990303     c                   movel     'P'           d55tpt
091200990303     c                   z-add     i50lna        d55lin
091300090325     c                   if        blpdsp>0
091400090325     c                   z-add     blpdsp        d55drf
091500090325     c                   else
091600090325     c                   z-add     dctdca        d55drf
091700090325     c                   endif
091800990303     c                   call      'FNLV55R'
091900990303     c                   parm                    dslv55
092000990303     c                   z-add     d55tfp        wpor
092100990303     c                   movel     'A'           wtcr
092200090324     c                   z-add     d55tfp        wter
092300990309     c
092400990309     c** PO e' "S" --> terminal partenza di p.o. disguido
092500990309     c     respo(xx)     wheneq    'S'
092600990309     c                   clear                   dslv55
092700990309     c                   movel     'P'           d55tpt
092800990309     c                   z-add     wfgs          d55lin
092900090325     c                   if        blpdsp>0
093000090325     c                   z-add     blpdsp        d55drf
093100090325     c                   else
093200090325     c                   z-add     dctdca        d55drf
093300090325     c                   endif
093400990309     c                   call      'FNLV55R'
093500990309     c                   parm                    dslv55
093600990309     c                   z-add     d55tfp        wpor
093700990309     c                   movel     'T'           wtcr
093800090324     c                   z-add     d55tfp        wter
093900990309     c
094000990304     c** PO e' "X" --> linea di arrivo  bolla
094100990308     c     respo(xx)     wheneq    'X'
094200990304     c                   movel     i50lna        wpor
094300990304     c                   movel     'A'           wtcr
094400090324     c                   movel     i50tfa        wter
094500990303     c
094600070713     c** PO e' "T" --> P.O. passato in wflp
094700990208     c     respo(xx)     wheneq    'T'
094800060301     c                   movel     wflp          wpor
094900070713     c                   select
095000070713     c                   when      wflp=i50tfp or wflp=i50lnp
095100060301     c                   movel     'P'           wtcr
095200090324     c                   movel     i50tfp        wter
095300070713     c                   when      wflp=i50tfa or wflp=i50lna
095400070713     c                   movel     'A'           wtcr
095500090324     c                   movel     i50tfa        wter
095600070713     c                   other
095700990129     c                   movel     'T'           wtcr
095800090324     c                   exsr      CallTER
095900070713     c                   endsl
096000081204     c
096100081204     c** PO e' "N" --> P.O. passato in wfgsspuan
096200081204     c     respo(xx)     wheneq    'N'
096300081204     c                   movel     wfgsspuan     wpor
096400081204     c                   select
096500081204     c                   when      wfgsspuan=i50tfp or wfgsspuan=i50lnp
096600081204     c                   movel     'P'           wtcr
096700090324     c                   movel     i50tfp        wter
096800081204     c                   when      wfgsspuan=i50tfa or wfgsspuan=i50lna
096900081204     c                   movel     'A'           wtcr
097000090324     c                   movel     i50tfa        wter
097100081204     c                   other
097200081204     c                   movel     'T'           wtcr
097300090324     c                   exsr      CallTER
097400081204     c                   endsl
097500990303     c
097600990304     c** PO e' "L" --> terminal partenza linea partenza bolla
097700990208     c     respo(xx)     wheneq    'L'
097800990205     c                   movel     i50tfp        wpor
097900090324     c                   movel     i50tfp        wter
098000990129     c* se = a lnp dct e' P altrimenti e' "T"
098100990208     c     i50lnp        ifeq      wdctlnpc
098200990129     c                   movel     'P'           wtcr
098300990129     c                   else
098400990129     c                   movel     'T'           wtcr
098500990129     c                   endif
098600990304     c
098700990304     c** PO e' "E" --> Linea partenza bolla
098800990304     c     respo(xx)     wheneq    'E'
098900990304     c                   movel     i50lnp        wpor
099000090324     c                   movel     i50tfp        wter
099100990304     c* se = a lnp dct e' P altrimenti e' "T"
099200990304     c     i50lnp        ifeq      wdctlnpc
099300990304     c                   movel     'P'           wtcr
099400990304     c                   else
099500990304     c                   movel     'T'           wtcr
099600990304     c                   endif
099700040907     c
099800040907     c** PO e' "V" --> p.o. scarico foglio viaggio
099900040907     c     respo(xx)     wheneq    'V'
100000040907     c     kfgv          chain     fnfgv01l
100100040907    1c                   if        %found(fnfgv01l)
100200040907    2c                   if        i50lna=fgvlna
100300040907     c                   movel     i50lna        wpor
100400040907     c                   movel     'A'           wtcr
100500090324     c                   movel     i50tfa        wter
100600040907   x2c                   else
100700040907     c     kfgv          chain     fnfgw01l
100800040907if  3c                   if        (not %found(FNFGW01L)
100900040907     c                              or  FGWatb <> *blanks)
101000040907     C                   clear                   FNFGW000
101100040907e   3C                   endif
101200040907     C                   MOVEL     FGWFF3        W1FF3
101300040907     C                   MOVEL     FGWFF4        W1FF4
101400040907     C                   MOVEL     FGWFL3        W1FL3
101500040907     C                   MOVEL     FGWFL4        W1FL4
101600040907     c*
101700040907     c                   z-add     1             yy
101800040907     c                   movel     i50lna        w003a             3
101900040907     c     w003a         lookup    ffv(yy)                                39
102000040907    3c                   if        *in39
102100040907     c                   movel     flp(yy)       wpor
102200040907    4c                   if        wpor=i50tfa
102300040907     c                   movel     'A'           wtcr
102400090324     c                   movel     i50tfa        wter
102500040907     c                   else
102600040907     c                   movel     'T'           wtcr
102700090828     C** SE TRANSITO, la filiale dis carico è sempre un terminal per cui
102800090828     c*  non importa calcolarlo: prendo flp(yy)
102900090828     c                   movel     flp(yy)       wter
103000090828     c***                exsr      CallTER
103100040907    4c                   endif
103200040907    3c                   endif
103300040907    2c                   endif
103400040907    1c                   endif
103500990304     c
103600990129     c                   endsl
103700090325     c
103800090325     c* Se p.o. estero--> terminal = se stesso perchè la responsab
103900090325     c*  deve andare al distretto internazionale
104000090325     C     wpor          chain     azorg01l                           39
104100090325     c  n39              movel     orgde3        og143
104200090325     c   39              clear                   og143
104300090325     c*
104400090325     c                   if        §ogntw='EEX' or  §ogntW= 'DPD' or  §ogntw
104500090325     c                             = 'FED'
104600090325     c                   eval      wter=wpor
104700090325     c                   endif
104800090325     c
104900990304     c** CERCO nella skiera se l'ho gia' impostato
105000990216     c*  p.o. + segnacollo
105100990129     c                   z-add     1             yy
105200990217     c                   if        wpor=0
105300090324     c                   clear                   w013a
105400090324     c                   movel     '000'         w013a
105500090324     c                   move      '000'         w013a
105600990217     c                   else
105700090324     c                   movel     wokey         w013a
105800990217     c                   endif
105900990217     c
106000090324     c     w013a         lookup    ofil(yy)                               31
106100990129     c     *in31         ifeq      *off
106200990129     c                   z-add     1             yy
106300990216     c
106400090324     c                   clear                   w013a            13
106500090324     c     w013a         lookup    ofil(yy)                               31
106600990129     c                   endif
106700990129     c**
106800990216     c                   movel     wokey         ofil(yy)
106900990208     c                   movel     wtcr          otcr(yy)
107000990916     c
107100990916     c** Responsabilita' per ammanchi e mancanze o anche per avarie se
107200990916     c**  non e' impostato ilsuo codic eresponsabilita'
107300990916     c                   if        §tadragr<>'V' or §trstrev=*blanks
107400990304     c                   movel     §trstre       otre(yy)
107500990916     c                   else
107600990916     c                   movel     §trstrev      otre(yy)
107700990916     c                   endif
107800090303     c* Se colli di valore la moltiplico in base a tabella STD
107900090303     c* Verifico  se il p.o. responsabile è estero: in questo caso non triplico
108000090325     c*  nemmeno per avarie
108100090303     c                   eval      percen=respc(xx)
108200090303     c
108300090303     c                   if        §ar5bva<>' '
108400090303     c                   if        §ogntw<>'EEX' and §ogntW<>'DPD' and §ogntw
108500090325     c                             <>'FED'  and §tadragr<>'V'
108600090303     c                   eval      percen=respc(xx)*§stdrsval
108700090303     c                   endif
108800090303     c                   endif
108900090303     c
109000990129     c** calcolo la % di reponsabilita' sui colli passati per questo calcol
109100090303     c                   eval(h)   Wper=((percen   *wcollodan)/100)
109200990209     c                   add       wper          oper(YY)
109300990129     c
109400990129     c                   add       1             xx
109500070711    2c                   enddo
109600070711    1c                   endif
109700000321     c
109800000321     c                   clear                   flptfa
109900131018     c                   eval      wes_mem='S'
110000990129     c                   ENDSR
110100090324      *****************************************************************
110200090324      * Determino terminal p.o. responsabile
110300090324      *****************************************************************
110400090324     c     CallTER       BEGSR
110500090324     c                   clear                   dslv55
110600090831     c
110700090831     c* Se non ho impostato il tipo di wflp per transito imposto
110800090831     c*    sempre "A"
110900090831     c                   if        wtipoflp=' '
111000090831     c
111100090324     c                   if        wtcr='P'
111200090324     c                   movel     'P'           d55tpt
111300090324     c                   else
111400090324     c                   movel     'A'           d55tpt
111500090324     c                   endif
111600090831     c
111700090831     c                   else
111800090831     c                   movel     wtipoflp      d55tpt
111900090831     c                   endif
112000090831     c
112100090831     c                   if        d55tpt='A'
112200090831     c                   movel     i50lnp        d55lnp
112300090831     c                   endif
112400090324     c                   z-add     wpor          d55lin
112500090325     c                   if        blpdsp>0
112600090325     c                   z-add     blpdsp        d55drf
112700090325     c                   else
112800090325     c                   z-add     dctdca        d55drf
112900090325     c                   endif
113000090324     c                   call      'FNLV55R'
113100090324     c                   parm                    dslv55
113200090324     c* se errore calcolo terminal a oggi
113300090324     c                   if        d55err<>' '
113400090324     c                   z-add     dateu         d55drf
113500090324     c                   call      'FNLV55R'
113600090324     c                   parm                    dslv55
113700090324     c                   endif
113800090324     c
113900090831     c                   if        D55TPT='P'
114000090324     c                   z-add     d55tfp        wter
114100090324     c                   else
114200090324     c                   z-add     d55tfa        wter
114300090324     c                   endif
114400090324     c                   ENDSR
114500990304      *****************************************************************
114600990304      * Causali di calcolo per errore in apertura partenza
114700990304      *****************************************************************
114800990304     c     CLIAPERTURA   begsr
114900990305     c                   clear                   wclip
115000990305     c                   clear                   wclia
115100990305     c
115200100216     c* Divido i codici respons. tra ammanchi, avarie e mancanze per questo
115300100216     c*  tipo di responsabilità
115400090102    1c                   if        §tadragr='M'
115500090102     c                   eval      wke1='APERTAPARCLIM  '
115600090102   x1c                   else
115700090102     c
115800090102     c* Per avarie e ammanchi cambia dal 7/1/09
115900100216     c*  cambia ancora dividendo la responsabilità tra ammanchi e avarie
116000100216    2c***                if        lnpfl1='E'
116100100216     c***                eval      wke1='APERTAPARCLI   '
116200100216     C***                else
116300100216     c
116400100216    3c                   if        §tadragr='A'
116500100216     c                   eval      wke1='APERTAPARCLIA  '
116600100216     c                   else
116700100216     c                   eval      wke1='APERTAPARCLIV  '
116800100216    3c                   endif
116900100216    2c****               endif
117000100216    1c                   endif
117100990305     c
117200990305     c* Essendo in PARTENZA linea del cliente mittente, se non c'e'
117300990305     c*  linea partenza bolla
117400100216    3c                   if        wapert='P'
117500990305     c                   if        i50ksm<>0
117600990305     c                   movel     i50ksm        wclip
117700990305     c                   else
117800990305     c                   eval      wclip=i50lnp
117900990305     c                   endif
118000100216    3c                   endif
118100990305     c
118200990305     c* Essendo in ARRIVO   linea del cliente destinatario, se non c'e'
118300990305     c*  linea arrivo bolla
118400100216    3c                   if        wapert='A'
118500990305     c                   if        i50ksd<>0
118600990305     c                   movel     i50ksd        wclia
118700990305     c                   else
118800990305     c                   eval      wclia=i50lna
118900990305     c                   endif
119000990305     c                   endif
119100990304     c                   endsr
119200990304      *****************************************************************
119300990304      * Causali di calcolo per errore in apertura partenza
119400990304      *****************************************************************
119500990304     c     ERRAPERTURA   begsr
119600990304     c                   if        lnpfl1='E'
119700990308     c                   eval      wke1='APERTAPARERR_E '
119800990304     c                   else
119900990308     c                   eval      wke1='APERTAPARERR   '
120000990304     c                   endif
120100990304     c                   endsr
120200990129      *****************************************************************
120300990129      * Scrivo la responsabilita'
120400990129      *****************************************************************
120500990129     c     WRIRES        begsr
120600051010     c                   clear                   WNoscrivi
120700180103     c                   clear                   aa                3 0
120800051010     c* se non ho più le spunte e mi servono per il calcolo della reponsab
120900051010     c*  non ricalcolo se c'e già (calcolo solo se è la prima volta)
121000051010     c
121100990209     c**
121200990209     c** cancello tutti i record presenti in fndcr
121300990216     c     kdct          chain     fndcr02l                           30
121400071018    1c     *in30         doweq     *off
121500990209     c     dcratb        ifeq      ' '
121600990209     c                   movel     'A'           dcratb
121700051010     c***                clear                   dcrftr
121800051010     c                   movel     dateu         dcrdtr
121900990209     c                   endif
122000051010     c* Solo se devo ricalcolare
122100051010     c                   if        WRicalcolo<>'N'
122200990209     c                   update    fndcr000
122300051010     c                   else
122400051010     c                   unlock    fndcr02l
122500051010     c                   movel     'N'           WNoscrivi         1
122600051010     c                   endif
122700990209     c**
122800990216     c     kdct          reade     fndcr02l                               30
122900071018    1c                   enddo
123000990209     c**
123100990129     c** scrivo i file e la ds di output
123200071018    1c                   if        WNoscrivi<>'N'
123300990129     c                   z-add     1             xx
123400071018    2c     ofil(xx)      downe     *blanks
123500990129     c     xx            ifeq      1
123600990129     c* DS
123700090324     c                   movel     ofil(xx)      o50por
123800990129     c                   z-add     oper(xx)      o50per
123900990129     c                   movel     otcr(xx)      o50tcr
124000990129     c                   endif
124100090324     c
124200090324     c                   movel     ofil(xx)      wokey
124300180103     c** Verifico se fil + segnacollo già presente: in questo caso
124400180103     c*  vado in ADD come % e scelgo terminal com maggiore %
124500180103     c                   clear                   wgiaesi           1
124600180103     c                   if        %lookup(wpose:opose)=0
124700180103     c                   add       1             aa
124800180103     c                   eval      opose(aa)=wpose
124900180103     c                   else
125000180103     c                   eval      wgiaesi='S'
125100180103     c                   endif
125200990129     c**
125300990216     c
125400990216     c     kdcr          chain     fndcr02l                           30
125500180103     c
125600180103     c                   if        wgiaesi=' ' or  dcratb<>' '
125700990129     c                   clear                   fndcr000
125800180103     c                   endif
125900180103     c
126000990129     c                   movel     dctaac        dcraac
126100990129     c                   movel     dctfil        dcrfil
126200990129     c                   movel     dctnca        dcrnca
126300180103     c                   if        oper(xx)>=dcrper
126400180103     c                   add       oper(xx)      dcrper
126500180103     c***                z-add     oper(xx)      dcrper
126600990129     c                   movel     otcr(xx)      dcrtcr
126700090324     c                   movel     wpor          dcrpor
126800090324     c                   movel     wter          §dcrter
126900090324     c                   move      wsegnadan     §dcrnsc
127000090324     c                   movel     otre(xx)      §dcrtre
127100180103     c                   else
127200180103     c                   add       oper(xx)      dcrper
127300180103     c                   endif
127400170413     c
127500170502     c                   if        writardo='S' and dctdca> §vpoca
127600170413     c                   eval      §dcrf1='R'
127700170413     c                   else
127800170413     c                   clear                   §dcrf1
127900170413     c                   endif
128000170413     c
128100990216     c                   movel(p)  ddcr01        dcrflo
128200051010     c                   movel     dateu         dcrdtr
128300990129     c**
128400990129     c  n30              update    fndcr000
128500990129     c   30              write     fndcr000
128600990129     c**
128700990129     c                   add       1             xx
128800071018    2c                   enddo
128900071018   x1c                   else
129000071018     c
129100071018     c* Se i colli in DCR non corrispondono più a quella della C.A.
129200071018     c* tengo i colli predsenti in DCD e basta
129300071018    2c                   if        wncn<>wcollidcr
129400071018     c                   EXSR      AllineaDCRDCD
129500071018    2C                   ENDIF
129600071018     c
129700071018    1c                   endif
129800990129     c**
129900990129     c                   movel     'A'           dctcar
130000051010     c                   movel     'A'           o50car
130100990129     c                   update    fndct000
130200051010     c                   clear                   WRicalcolo
130300051010     c                   clear                   WNoscrivi
130400990129     c                   ENDSR
130500071018      *****************************************************************
130600071018      * Sistemo DCD allineato a DCR
130700071018      *****************************************************************
130800071018     c     AllineaDCRDCD BEGSR
130900071018     c
131000071018     c     kdct          chain     fndcr02l                           30
131100071018    3c     *in30         doweq     *off
131200071018    4c                   if        dcratb=' ' and dcrseg>*zeros
131300071018     c                   movel     dcrseg        w0070             7 0
131400071018     c     kdcr2         chain     fndcd01l
131500071018    5c                   if        not %found(fndcd01l) or dcdatb<>' '
131600071018     c                   movel     'A'           dcratb
131700071018     c                   movel     dateu         dcrdtr
131800071018     c                   update    fndcr000
131900071018    5c                   endif
132000071018    4c                   endif
132100071018     c
132200071018     c* Se non c'e' il segnacollo ed inserito solo 1 record,
132300071018     c*  sistemo altrimenti non ci riesco
132400071018    4c                   if        dcratb=' ' and dcrseg<=*zeros
132500071018     c                             and dcrper=wcollidcr
132600071018     c                   z-add     wncn          dcrper
132700071018     c                   movel     dateu         dcrdtr
132800071018     c                   update    fndcr000
132900071018    4c                   endif
133000071018     c
133100071018     c     kdct          reade     fndcr02l                               30
133200071018    3c                   enddo
133300071018     c                   ENDSR
133400990129      *****************************************************************
133500990129      * controllo se c'e' riserva in bolla in mancanza di dett CA o spunte
133600990129      *****************************************************************
133700990208     c     CT_RISERVA    begsr
133800990129     c                   clear                   wriserva          1
133900990129     c**
134000990204     c     dctcrc        ifeq      'S'
134100990211     c                   MOVEL(p)  'RISERVANODET'wke1
134200990129     c                   else
134300990129     C** 3) NON C'E' DETT.C.A. NON C'E'RISERVA --> non posso calcolare
134400990129     c*     NON C'E' SPUNTA    NON C'E'RISERVA --> non posso calcolare
134500990217     c                   MOVEL(p)  'NOCALCOLO'   wke1
134600051010     c                   movel     'N'           WRicalcolo        1
134700990129    2c                   endif
134800990217     c
134900990217     c                   exsr      MEMRES
135000990129     c                   ENDSR
135100990329      *****************************************************************
135200990329      * RESPONSABILITA' PER RITARDO APETURA C:A: MANCANZE
135300990329      *****************************************************************
135400990329     c     RITARDO       begsr
135500990329     c                   clear                   wsavdao
135600990329     c                   clear                   wsavcch
135700990329     c                   clear                   wsavdch
135800990329     c
135900990329     c* tutti i colli devono essere in ritardo, altrimenti va bene
136000990518     c     kdcd          chain     fnanm02l                           30
136100990329    1c     *in30         doweq     *off
136200990329     c                   if        anmatb=' '
136300990329     c     anmcaa        lookup    anidd                                  34
136400990329     c                   if        *in34 AND
136500990329     c                             anmdao>wsavdao
136600990329     c                   eval      wsavdao=anmdao
136700990329     c                   eval      wsavcch=anmcch
136800990329     c                   eval      wsavdch=anmdch
136900990329     c                   endif
137000990329     c                   endif
137100990329     c**
137200990518     c     kdcd          reade     fnanm02l                               30
137300990329    1c                   enddo
137400990329     c**
137500990329     c* l'anomalia idd trovata deve essere chiusa
137600990329    1c     wsavdch       ifgt      0
137700990329     c**
137800990514     c* anomalia chiusa con pratica idd --> vedo se aperta 2 giorni dopo
137900990329     c*   escluso i festivi
138000990329    2c     wsavcch       ifeq      'PR'
138100990329     c* cerco la fase 10 di apertura
138200990329     c     kdct          chain     fndcf01l                           31
138300990329    3c     *in31         doweq     *off
138400990329     c     dcffca        andne     10
138500990329     c     kdct          reade     fndcf01l                               31
138600990329    3c                   enddo
138700990329     c** trovata la fase 10 --> controllo
138800990518    3c  n31dcfdfc        ifgt      wsavdch
138900990329     c* controllo di quanti giorni
139000990518     c     *iso          move      wsavdch       wdataanm
139100990329     c     *iso          move      dcfdfc        wdatafase
139200990329     c                   clear                   wgiorni           2 0
139300990329     c                   adddur    1:*D          wdataanm
139400990329    4c     wdataanm      dowle     wdatafase
139500990329     c* controllo se e' festivo
139600990329     C     *iso          move      wdataanm      kdata
139700990329     C                   Z-ADD     dctlna        KTFA
139800150604     c* per lna estera o DPd uso i50tfa
139900150604     c                   if        lnafl1='E' or lnantw='DPD'
140000150604     C                   Z-ADD     i50tfa        KTFA
140100150604     c                   endif
140200990329     C                   clear                   KTFP
140300990329     C     KCLN          CHAIN     AZCLN01L                           34
140400990329    5C     *IN34         IFEQ      *OFF
140500990329     C     MAT(KGG)      andne     'F'
140600990329     C     POM(KGG)      andne     'F'
140700990329     C     *IN34         oreq      *ON
140800990329     C                   add       1             wgiorni
140900990329    5C                   ENDIF
141000990329     c**
141100990329     c                   adddur    1:*D          wdataanm
141200990329    4c                   enddo
141300990329     c**
141400120209     c* In base alla data spedizione tengo conto di 2/3 giorni o 7/8 giorni
141500120209   4ac                   if        blpdsp>0 and blpdsp>=20120130
141600120209     c                             and blpdsp<=20120214
141700120209     c
141800120209    4c     wgiorni       ifgt      7
141900120209     c     §ogeid        andeq     ' '
142000120209     c* Per chi fa l'IDD il giorno dopo c'e' un giorno in piu' per aprire
142100120209     c     wgiorni       orgt      8
142200120209     c     §ogeid        andeq     'S'
142300120209     c                   eval      writ='R'
142400120209     c                   else
142500120209     c                   eval      writ='N'
142600120209    4c                   endif
142700120209     c
142800120209  x4ac                   else
142900120209     c
143000990514    4c     wgiorni       ifgt      2
143100990329     c     §ogeid        andeq     ' '
143200990329     c* Per chi fa l'IDD il giorno dopo c'e' un giorno in piu' per aprire
143300990514     c     wgiorni       orgt      3
143400990329     c     §ogeid        andeq     'S'
143500990329     c                   eval      writ='R'
143600990329     c                   else
143700990329     c                   eval      writ='N'
143800120209    4c                   endif
143900990329     c*
144000120209   4ac                   endif
144100120209     c
144200120209    3c                   endif
144300990329    2c                   endif
144400990329    1c                   endif
144500990329     c
144600990329     c                   endsr
144700990129      *****************************************************************
144800990129      * RESPONSABILITA' PER MANCANZE
144900990129      *****************************************************************
145000990129     c     MANCANZE      begsr
145100990305     c                   clear                   wsavdao
145200990305     c                   clear                   wsavcch
145300990305     c                   clear                   wsavdch
145400000321     c                   clear                   flptfa
145500000321     c                   clear                   writardo          1
145600090512     c                   clear                   wnfv
145700090828     c                   clear                   wter
145800090828     c                   clear                   wtipoflp          1
145900990305     c
146000990205     c* In questa routine il collo danneggiato da controllare e' sempre "1"
146100990129     C* controllo l'anomalia IDD3 se c'e'
146200990129     c*  data immissione C.A. --> responsabilita' arrivo
146300990518     c     kdcd          chain     fnanm02l                           30
146400990129    1c     *in30         doweq     *off
146500990305     c                   if        anmatb=' '
146600990305     c     anmcaa        lookup    anidd                                  34
146700990305     c                   if        *in34 AND
146800990305     c                             anmdao>wsavdao
146900990305     c                   eval      wsavdao=anmdao
147000990305     c                   eval      wsavcch=anmcch
147100990305     c                   eval      wsavdch=anmdch
147200990305     c                   endif
147300990305     c                   endif
147400990129     c**
147500990518     c     kdcd          reade     fnanm02l                               30
147600990129    1c                   enddo
147700990129     c**
147800990129     c* l'anomalia idd trovata deve essere chiusa
147900990329    1c     wsavdch       ifgt      0
148000990129     c* anomalia chiusa annullata --> colpa della partenza
148100990305    2c     wsavcch       ifeq      'AN'
148200990211     c                   MOVEL(p)  'IDDANN'      wke1
148300990129     c                   exsr      MEMRES
148400990208     c                   goto      endmanca
148500990129   x2c                   else
148600990129     c**
148700990129     c* anomalia chiusa con pratica idd --> vedo se aperta il giorno dopo
148800990129     c*   escluso i festivi
148900990331     c* All'export non imputo l'apertura in ritardo perche' non puo' essere
149000990331     c*  colpa del partner
149100990305    3c     wsavcch       ifeq      'PR'
149200990331     c     writ          andeq     'R'
149300990331     c     lnafl1        andne     'E'
149400170906     c* Per ora memorizzo solo che e' c.a. aperta in ritardo poi vedo
149500000321     c                   eval      writardo='S'
149600990129     c*
149700990129    3c                   endif
149800990129    2c                   endif
149900990129    1c                   endif
150000990129     c**
150100990208     c**  SPUNTE: vedo se ci sono
150200990205     c                   movel     'S'           wesistspu         1
150300070131     c**   kdcd          setll     fnbrv07l                               34
150400061020     c**n34              movel     'N'           wesistspu
150500061020     c* Dico che non esistono spunte se non trovate o se trovate ma
150600061020     c*  flaggate (significa che abbiamo tenuto solo le spunte con BRVCAN
150700061020     c*  impostato)
150800070131     c     kdcd          chain     fnbrv07l
150900070131     c                   if        not %found(fnbrv07l) or brvnvr<>' '
151000061020     c                   movel     'N'           wesistspu         1
151100061020     c                   endif
151200990204     c**
151300990205     c** PARTO COMUNQUE DAL DETT.COLLI POI IN CASO DI NECESSITA'
151400990205     c**  VALUTO LE SPUNTE
151500990205     c**
151600990208     c** TERMINAL DI PARTENZA di artflp
151700990208     c                   clear                   wflp
151800990205     c     artflp        ifgt      0
151900990208     c                   movel     'P'           d55tpt
152000990205     c                   z-add     artflp        d55lin
152100990205     c                   z-add     artlnp        d55lnp
152200990205     c                   select
152300990205     c     artdut        whengt    0
152400990205     c                   z-add     artdut        d55drf
152500990205     c     artdet        whengt    0
152600990205     c                   z-add     artdet        d55drf
152700990205     c     artdfv        whengt    0
152800990208     c                   z-add     artdfv        d55drf
152900990205     c                   other
153000990205     c                   z-add     dctdca        d55drf
153100990205     c                   endsl
153200990209     c                   call      'FNLV55R'
153300990205     c                   parm                    dslv55
153400990205     c*
153500990205     c                   z-add     d55tfp        wflp
153600090828     c                   movel     'P'           wtipoflp
153700000321     c*
153800000321     c** TERMINAL DI ARRIVO tra artflp e artlna
153900000321     c                   movel     'A'           d55tpt
154000000321     c                   z-add     artflp        d55lnp
154100000321     c                   z-add     artlna        d55lin
154200000321     c                   clear                   d55tfa
154300000321     c                   call      'FNLV55R'
154400000321     c                   parm                    dslv55
154500000321     c*
154600000321     c                   z-add     d55tfa        flptfa
154700990208     c                   endif
154800990308     c
154900990308     c* Se la data di arrivo pistola 85 > data apertura c.a. non la conseder
155000990308     c     artdam        ifgt      0
155100990308     c     artnap        andeq     85
155200990401     c     artdam        andge     dctdca
155300990308     c                   clear                   artdam
155400990308     c                   clear                   artnap
155500990308     c                   endif
155600990310     c
155700990310     c* Per EXPORT  ignoro sempre la pistola 85 --> errore bartolini di aper
155800990310     c     artdam        ifgt      0
155900990310     c     artnap        andeq     85
156000990310     c     lnafl1        andeq     'E'
156100990310     c                   clear                   artdam
156200990310     c                   clear                   artnap
156300990310     c                   endif
156400990205     c***
156500990205     c* DATA ARRIVO : se c'e' il collo e' arrivato
156600990208    1c     artdam        ifgt      0
156700990210     c                   clear                   wfgs
156800990205     C* verifico se dopo l'arrivo e' stato spuntato da altri P.O.
156900990208    2c     wesistspu     ifeq      'S'
157000990205     c                   z-add     artdam        wdatrifer
157100990205     c                   exsr      SPUARR
157200051010     c                   else
157300051010     c                   movel     'N'           WRicalcolo
157400990208    2c                   endif
157500990205     c***
157600990210     c* Se WFGS    >0 e non e' lna della CA cerco suo terminal di arrivo
157700990210    2c     wfgs          ifgt      0
157800990308     c
157900990311     c* Se bolla export e c'e' spunta defluenza con data>=data arrivo     nr
158000990311     c*  --> colpa partner
158100990311     c                   if        lnafl1='E'  AND
158200990311     c                             wEXPdef='S' AND
158300990311     c                             WEXPDFV>=artdam
158400990311     c
158500990310     c                   MOVEL(p)  'SPUNTADEFL_E'wke1
158600990308     c                   exsr      MEMRES
158700990308     c                   goto      endmanca
158800990308     c                   endif
158900990329
159000990329     c* Se fgs e' terminal arrivo bolla --> colpa sua
159100990329     c                   select
159200990329    3c     wfgs          wheneq    i50lna
159300990329     c                   MOVEL(p)  'ARRIVOLNA'   wke1
159400000321     c* Vedo se considerare il ritardo
159500000321     c                   exsr      AQUALE
159600990329     c                   goto      endmanca
159700990305     c* Se fgs e' terminal arrivo bolla --> colpa sua
159800990329    3c     wfgs          wheneq    i50tfa
159900000321     c* clearo terminal tenendo conto del p.o. di transito perche' non
160000000321     c*  mi serve
160100000321     c                   clear                   flptfa
160200990308     c
160300990305     c                   MOVEL(p)  'ARRIVOSI'    wke1
160400000321     c* Vedo se considerare il ritardo
160500000321     c                   exsr      AQUALE
160600990305     c                   goto      endmanca
160700990329   x3c                   other
160800990305     c
160900990210     c                   EXSR      terspunta
161000990311     c
161100990305     c*** Se si tratta comunque di p.o. dell'area di arrivo-->
161200990329     c***  imputo al p.o.
161300990205     c***  altrimenti e' DISGUIDO
161400990305    4c     wflp          ifeq      i50tfa
161500990305     c                   MOVEL(p)  'ARRIVOAREA'  wke1
161600990329     c                   eval      wflp=wfgs
161700090831     c                   movel     'A'           wtipoflp
161800000321     c* Vedo se considerare il ritardo
161900000321     c                   exsr      AQUALE
162000990208     c                   goto      endmanca
162100990305   x4c                   else
162200990211     c                   MOVEL(p)  'DISGUIDO'    wke1
162300000321     c* Vedo se considerare il ritardo
162400000321     c                   exsr      AQUALE
162500990208     c                   goto      endmanca
162600990305    4c                   endif
162700990329    3c                   endsl
162800990205     c**
162900040907     c* non trovate altre spunte
163000990208   x2c                   else
163100990305     c                   MOVEL(p)  'ARRAREA_'    wke1
163200990305     c                   MOVE      'NOSPU  '     wke1
163300000321     c* Vedo se considerare il ritardo
163400000321     c                   exsr      AQUALE
163500990208     c                   goto      endmanca
163600990208    2c                   endif
163700990208    1c                   endif
163800990205     c**
163900990205     c* DATA USCITA TRANSITO: se c'e' con pistola reale e' uscito dal trans
164000990208    1c     artdut        ifgt      0
164100990208     c                   movel     artput        w002a             2
164200990208     c     w002a         lookup    nps                                    34
164300990205     c** 34 off - pistola reale
164400990208    2c     *in34         ifeq      *off
164500990211     c                   MOVEL(p)  'TRADUT_SINO' wke1
164600000321     c* Vedo se considerare il ritardo
164700000321     c                   exsr      AQUALE
164800990208     c                   goto      endmanca
164900990205     c**
165000990208   x2c                   else
165100990205     c** Se si tratta di pistola completamente fasulla, senza altra
165200990205     c**  spunta, cerco nelle spunte un eventuale altro disguido
165300990205     c**  altrimenti e' colpa del transito
165400990205     c**
165500990208     c                   movel     artput        w002a             2
165600990208     c     w002a         lookup    npsno                                  31
165700990205     c** SPUNTA FASULLA DA SPUNTA REALE --> colpa del transito
165800990208    3c     *in31         ifeq      *off
165900990211     c                   MOVEL(p)  'TRADET_'     wke1
166000990205     c                   MOVE      'SINONO  '    wke1
166100000321     c* Vedo se considerare il ritardo
166200000321     c                   exsr      AQUALE
166300990208     c                   goto      endmanca
166400990208   x3c                   else
166500990205     c** Cerco un'altra spunta reale
166600990210     c                   clear                   wfgs
166700990208    4c     wesistspu     ifeq      'S'
166800990205     c                   clear                   wdatrifer
166900990210     c                   clear                   wfgs
167000990205     c                   exsr      SPUARR
167100990205     c**
167200990210     c* Se WFGS    >0 e non e' lnp della CA cerco suo terminal di arrivo
167300990210    5c     wfgs          ifgt      0
167400990210     c     wfgs          andne     dctlnp
167500990210     c     wfgs          andne     i50tfp
167600990210     c                   EXSR      TERSPUNTA
167700990205     c**
167800020312     C** SE SI TRATTA DI SPUNTA PARTENZA, LA CONSIDERO COME UN SI/NO
167900020312     C     wnpg          ifeq      1
168000020312     c                   MOVEL(p)  'TRADUT_SINO' wke1
168100020312     c                   else
168200990211     c                   MOVEL(p)  'DISGUIDO'    wke1
168300020312     c                   endif
168400000321     c* Vedo se considerare il ritardo
168500000321     c                   exsr      AQUALE
168600990208     c                   goto      endmanca
168700990205     c**
168800990208    5c                   endif
168900051010     c                   else
169000051010     c                   movel     'N'           WRicalcolo
169100990208    4c                   endif
169200990205     c**
169300990208    3c                   endif
169400990208    2c                   endif
169500990208    1c                   endif
169600990205     c**
169700990208     c**
169800990208     c* DATA ENTRATA TRANSITO: controllo se non c'e' data uscita o fasulla
169900990208     c*                        senza altra spunte o solo della lnp
170000990208    1c     artdet        ifgt      0
170100990208     c                   movel     artpet        w002a             2
170200990208     c     w002a         lookup    nps                                    34
170300060301     c* Pistola reale
170400060301    2c     *in34         ifeq      *off
170500990211     c                   MOVEL(p)  'TRADET_'     wke1
170600990205     c                   MOVE      'SINONO  '    wke1
170700000321     c* Vedo se considerare il ritardo
170800000321     c                   exsr      AQUALE
170900990208     c                   goto      endmanca
171000060301     c
171100060301   x2c                   else
171200060301     c
171300060301     c** Cerco un'altra spunta reale
171400060301     c                   clear                   wfgs
171500060301    4c     wesistspu     ifeq      'S'
171600060301     c                   clear                   wdatrifer
171700060301     c                   clear                   wfgs
171800060301     c                   exsr      SPUARR
171900060301     c**
172000060301     c* Se WFGS    >0 e non e' lnp della CA cerco suo terminal di arrivo
172100060301    5c     wfgs          ifgt      0
172200060301     c     wfgs          andne     dctlnp
172300060301     c     wfgs          andne     i50tfp
172400060301     c                   EXSR      TERSPUNTA
172500060301     c**
172600060301     C** SE SI TRATTA DI SPUNTA PARTENZA, LA CONSIDERO COME UN SI/NO
172700060301     C     wnpg          ifeq      1
172800060301     c                   MOVEL(p)  'TRADUT_SINO' wke1
172900060301     c                   else
173000060301     c                   MOVEL(p)  'DISGUIDO'    wke1
173100060301     c                   endif
173200060301     c* Vedo se considerare il ritardo
173300060301     c                   exsr      AQUALE
173400060301     c                   goto      endmanca
173500060301     c**
173600060301    5c                   endif
173700060301    4c                   endif
173800060301    2c                   endif
173900990208    1c                   endif
174000990205     c**
174100990205     c** DATA PARTENZA
174200990208    1c     artdfv        ifgt      0
174300060301     c
174400060301     c* Verifico quale è l'ultima spunta effettuata in partenza
174500060301    2c     wesistspu     ifeq      'S'
174600060309     c* Mi salvo la spunta partenza
174700060309     c                   movel     wfgs          fgspar
174800060309     c                   movel     wnfv          nfvpar
174900060309     c
175000060301     c                   z-add     artdfv        wdatrifer
175100060301     c                   eval      wautogen='N'
175200060301     c                   exsr      SPUARR
175300060301     c                   eval      wautogen=' '
175400060301     c
175500060301     c* Se l'ultima spunta non autogenerata è non categoria partenza-->
175600060301     c* colpa del p.o.
175700060301     c                   if        wnpg<>0
175800060301     c                   if        (wnpg<>1 and  wnpg<>3 and wnpg<>5)
175900090511     c                             or (wnpg=3 and wspg<>'P')  or
176000090511     c* oppure se la data della spunta > data partenza collo (solo IMP)
176100090511     c                             wdfv>artdfv
176200060301     c                   MOVEL(p)  'SPUNOPART'   wke1
176300060301     c                   eval      wflp=wfgs
176400090831     c                   movel     'P'           wtipoflp
176500060301     c                   exsr      AQUALE
176600060301     c                   goto      endmanca
176700060301     c                   endif
176800060301     c                   endif
176900060301     c                   endif
177000060301     c
177100990208     c                   movel     artnpp        w002a             2
177200990208     c     w002a         lookup    nps                                    34
177300990205     c*  partenza effettiva
177400990208    2c     *in34         ifeq      *off
177500060301     c
177600060301     c* Se è categoria partenza ma è di un secondo livello-->
177700060301     c* colpa del terminal
177800170627     c* esclusi i terminal con "autogenerazione daqualsiasi spunta entrata"
177900060301     c                   if        wesistspu='S' and wfgs<>0
178000060301     c                   clear                   dslv55
178100060301     c                   movel     'P'           d55tpt
178200060301     c                   z-add     wfgs          d55lin
178300060301     c                   z-add     wdfv          d55drf
178400060301     c                   call      'FNLV55R'
178500060301     c                   parm                    dslv55
178600060301     c                   if        d55tfp<>wfgs
178700170627     c* Verifico da organigramma che autogenerazione spunta partenza ha il terminal
178800170906     c                   clear                   tp_og148
178900170906     c     d55tfp        chain     azorg01l
179000170906     c                   if        %found(azorg01l)
179100170906     c                   eval      tp_og148=orgde8
179200170906     c                   endif
179300170906     c                   if        tp§ogags<>'S'
179400170906     c                   MOVEL(p)  'SPUNOTER'    wke1
179500060301     c                   eval      wflp=wfgs
179600090831     c                   movel     'P'           wtipoflp
179700060301     c                   exsr      AQUALE
179800060301     c                   goto      endmanca
179900060301     c                   endif
180000170906     c                   endif
180100060301     c                   endif
180200060309     c
180300060309     c* Reimosto il foglio partenza che mi serve per il calcolo della respo
180400060309     c                   eval                    wfgs= fgspar
180500060309     c                   eval                    wnfv= nfvpar
180600990205     c*
180700990205     c* se NON C'E' TRANSITO e' colpa della partenza
180800990208    3c     artflp        ifeq      0
180900170906     c                   if        writardo=' '
181000990211     c                   MOVEL(p)  'PART_SINO'   wke1
181100170906     c                   else
181200170906     c                   MOVEL(p)  'PART_RSINO'  wke1
181300170906     c                   endif
181400000321     c* Vedo se considerare il ritardo
181500000321     c                   exsr      AQUALE
181600990208     c                   goto      endmanca
181700990205     c                   else
181800990205     c*        C'E' TRANSITO: e' colpa del transito
181900170906     c                   if        writardo=' '
182000990211     c                   MOVEL(p)  'PART_'       wke1
182100990205     c                   MOVE      'SINONONO  '  wke1
182200170906     c                   else
182300170906     c                   MOVEL(p)  'PART_'       wke1
182400170906     c                   MOVE      'RSINONONO '  wke1
182500170906     c                   endif
182600000321     c* Vedo se considerare il ritardo
182700000321     c                   exsr      AQUALE
182800990208     c                   goto      endmanca
182900990208    3c                   endif
183000990205     c***
183100990208   x2c                   else
183200990208     c                   movel     'AUT'         wnppnps           3
183300990208    2c                   endif
183400990208   x1c                   else
183500990205     c                   movel     'AUT'         wnppnps
183600990208    1c                   endif
183700990205     c**
183800990205     c** partenza non effettiva  o = "0"
183900990208    1c     wnppnps       ifeq      'AUT'
184000990303     c
184100990303     c* Causale diversa per punta partenza fasulla
184200990303     c                   movel     artnpp        w002a             2
184300000321     c     w002a         lookup    npsno                                  35
184400990303     c
184500990208    2c     artflp        ifeq      0
184600990303     c
184700000321     c* pistola 88 /89 /90
184800990304    3c                   if        *in35
184900990304    4c                   if        lnpfl1='E'
185000990304     c                   MOVEL(p)  'PART_NONO_E' wke1
185100990304     c                   else
185200990304     c                   MOVEL(p)  'PART_NONO'   wke1
185300990304    4c                   endif
185400000321     c                   z-add     50            wresarr           3 0
185500990304     c
185600990304   x3c                   else
185700000321     c* pistola 98 /96
185800990304     c                   MOVEL(p)  'PART_NONO9X' wke1
185900990304    3c                   endif
186000990304     c
186100000321     c                   exsr      AQUALE
186200990208     c                   goto      endmanca
186300990303     c
186400990304   x2c                   else
186500990205     c*        C'E' TRANSITO: e' colpa del transito
186600990211     c                   MOVEL(P)  'PART_'       wke1
186700990304    3c                   if        *in35
186800990311    4c                   if        lnpfl1='E'
186900990304     c                   MOVE      'NONONONO_E'  wke1
187000990304     c                   else
187100990304     c                   MOVE      'NONONONO  '  wke1
187200990304    4c                   endif
187300000321     c                   z-add     50            wresarr           3 0
187400990304     c
187500990304   x3c                   else
187600990304     c                   MOVE      'NONONONO9X'  wke1
187700990304    3c                   endif
187800990304     c
187900000321     c                   exsr      AQUALE
188000990208     c                   goto      endmanca
188100990208    2c                   endif
188200990208    1c                   endif
188300990205     c***
188400990208     c     endmanca      endsr
188500000321      *****************************************************************
188600000321      * Vedo se dare la colpa prevista oper il ritardo
188700000321      *****************************************************************
188800000321     c     AQUALE        BEGSR
188900000321     c* Il ritardo non e' mai previsto a carico dell'estero
189000000321     c                   if        writardo='S'
189100000321     c* ritardo tutta colpa dell'arrivo
189200170413     c*  ignoro dal 01/05/2017 e memorizzo flag a parte
189300170413     c                   if        dctdca<=§vpoca
189400170411     c                   if        wresarr=0
189500170411     c                   MOVEL(p)  'IDDRITARDO'  wke1
189600170411     c                   endif
189700170413     c                   endif
189800170413
189900000321     c* ritardo colpa al 50%
190000000321     c                   if        wresarr=50
190100000321     c                   MOVEL(p)  'IDDRITARDO50'wke1
190200000321     c                   endif
190300000321     c
190400000321     c                   endif
190500000321     c
190600000321     c                   exsr      MEMRES
190700000321     c                   clear                   wresarr
190800000321     c                   ENDSR
190900990210      *****************************************************************
191000990210      * RESPONSABILITA' PER MANCANZE
191100990210      *****************************************************************
191200990210     c     TERSPUNTA     BEGSR
191300990210     c* per categoria "1" --> tengo fgs altrimenti cerco terminal arrivo
191400990210    1c     wnpg          ifeq      1
191500990210     c                   z-add     wfgs          wflp
191600090831     c                   movel     'P'           wtipoflp
191700990210     c                   else
191800990210     c                   clear                   dslv55
191900990210     c* Cerco il terminal di partenza per categoria partenza
192000990210     c                   if        wnpg=5   OR
192100990210     c                             wnpg=3 and wspg='P'
192200990210     c                   movel     'P'           d55tpt
192300990210     c                   else
192400990210     c                   movel     'A'           d55tpt
192500990210     c                   endif
192600990210     c                   movel     wfgs          d55lin
192700990210     c                   z-add     artlnp        d55lnp
192800990210     c                   z-add     wdfv          d55drf
192900990210     c                   call      'FNLV55R'
193000990210     c                   parm                    dslv55
193100990210     c**
193200990920     c                   if        d55tpt='P'
193300990920     c                   z-add     d55tfp        wflp
193400090831     c                   movel     'P'           wtipoflp
193500990920     c                   else
193600990210     c                   z-add     d55tfa        wflp
193700090831     c                   movel     'A'           wtipoflp
193800990210    1c                   endif
193900990920    1c                   endif
194000990210     c                   ENDSR
194100990129      *****************************************************************
194200990129      * RESPONSABILITA' PER AMMANCHI E AVARIE
194300990129      *****************************************************************
194400990209     c     AMMAVARIE     BEGSR
194500000321     c                   clear                   flptfa
194600090421     c                   clear                   SoloCAN
194700090828     c                   clear                   wter
194800090828     c                   clear                   wtipoflp          1
194900090828     c
195000990129     c* se il collo non ha spunte non posso calcolare se non c'e' riserva
195100070131     c**   kdcd          setll     fnbrv07l                               34
195200070131     c     kdcd          chain     fnbrv07l
195300090421     c*****              if        not %found(fnbrv07l) or brvnvr<>' '
195400110923    1c                   if        not %found(fnbrv07l)
195500061020     c                   setoff                                       34
195600110923   x1c                   else
195700090421     c* Presenti solo spunte con l'anomalia
195800110923    2c                   if        brvnvr<>' '
195900090421     c                   eval      soloCAN='S'
196000110923     c                   endif
196100110923     c
196200061020     c                   seton                                        34
196300110923    2c                   endif
196400061020     c
196500990209    1c     *in34         ifeq      *off
196600990208     c* controllo se c'e' riserva
196700990208     c                   exsr      CT_RISERVA
196800990208     c                   goto      endammava
196900990209   x1c                   else
197000990304     c
197100990209     c** Carico per ogni fgs la spunta con data piu' bassa che ha l'anomalia
197200990209     c                   EXSR      LETBRV
197300990304     c
197400990209     c** Se non c'e' nessuna spunta :
197500990210    2c     wnpg          ifeq      0
197600990209     c* se non c'e' nemmeno sul dett.colli --> NO_CAN
197700990331     c* se     c'e'         sul dett.colli --> A_CAN_NOSPU
197800050615     c* se artcan=ad una delle anomalie "fasulle" come se non ci fosse
197900050615     c     artcan        lookup    Noanomspu                              31
198000990209    3c     artcan        ifeq      ' '
198100050615     c     *in31         oreq      *on
198200990304     c                   movel(P)  'A_NO_CAN'    wke1
198300990209     c                   else
198400990331     c                   movel(P)  'A_CAN_NOSPU' wke1
198500990209    3c                   endif
198600990209     c
198700990209     c                   exsr      MEMRES
198800990209     c                   goto      endammava
198900990209    2c                   endif
199000990209     c**
199100090102     c** Se c'e' la spunta categoria part in partenza :colpa PART_SI
199200990209     c**  escludo le spunte di defluenza
199300081204    2c     wfgs          ifeq      i50tfp
199400090102    2c     wfgs          oreq      i50lnp
199500081204    3c     wnpg          ifeq      1
199600990210     c     wdef          andeq     ' '
199700080714     c     wnpg          oreq      5
199800080903     c* se si tratta di una spedizione import DPD, la colpa viene data
199900080903     c*  al partner
200000081204    4c                   if        lnpntw='DPD'
200100080903     c                   MOVEL(p)  'COLPAPART_E' wke1
200200080903     c                   else
200300990304     c                   movel(P)  'A_PART_SI'   wke1
200400081204    4c                   endif
200500990209     c                   exsr      MEMRES
200600990209     c                   goto      endammava
200700081204    3c                   endif
200800080714    2c                   endif
200900990210     c
201000090422     c** controllo categoria della 1°spunta con anomalia
201100990210     c                   movel     wnpg          w001a
201200990210     c     w001a         lookup    catarr                                 34
201300990210    2c   34wnpg          ifeq      1
201400990210     C     wdef          andeq     ' '
201500990210     c                   setoff                                       34
201600990210    2c                   endif
201700080513     c
201800080513     c                   if        wnpg=5
201900080513     c                   seton                                        34
202000080513     c                   endif
202100990210     c
202200990212     c** NON E' Categoria  ARRIVI  o ANOMALIA INSERITA ERRONEMANETE
202300990216     c**                 __> colpa sua
202400990212    2c                   if        not *in34   OR
202500990212     c                             werr = 'E'
202600990211     c                   movel     'S'           wsianom           1
202700990210     c                   EXSR      RESPOAAV
202800990210     c                   goto      endammava
202900990210     c
203000990210   x2c                   else
203100090423     c
203200090423     c                   eval      wNPGspuan=wnpg
203300090423     c                   eval      wflpspuan=wflp
203400090423     c                   eval      wfgsspuan=wfgs
203500990305     c
203600080903     c**     E' Categoria  ARRIVI/ENTRATA:
203700990210     c**     se spara un secondo livello --> colpa del terminal arrivo
203800990210     c**     se spara un primo   livello --> colpa del suo precedente
203900990210     c** Il suo precedente e' quello che ha la data < o a parita' di data
204000990210     c*   data/ora carcamento <
204100990211    3c                   if        wfgs<>i50tfa
204200990210     c                   exsr      terspunta
204300990210     c
204400990211    4c                   if        wflp<>wfgs
204500990921     c                   eval      zz=1
204600990921     c     wfgs          lookup    waavfgs(zz)                            34
204700990921     c                   movel     wnpg          w001a
204800990921     c* se non e' la 1 spunta del 2 livello --> e' colpa del 2 livello
204900090831     c*  per me qui c'e' un errore: se  il secondo livello non è la
205000070712     c*  linea di arrivo ma un altro secondo livello
205100080404     c*  la colpa finisce alla linea di arrivo lo stesso
205200080404     c*  4/4/08--> imposto WFLP col p.o. che ha la colpa  cambio il
205300080404     c*            flag tipo rsponsabilità al codice A_2LIV_TARD in "T"
205400081204    5c     *in34         ifeq      *off
205500990921     c     waavsca(zz)   orne      wscar
205600990921     c     waavfgs(zz)   orne      wfgs
205700990921     c     waavnpg(zz)   orne      w001a
205800990921     c     waavdfv(zz)   orne      wdfv
205900080404     c                   eval      wflp=wfgs
206000090831     c                   movel     'A'           wtipoflp
206100990921     c                   eval      wke1='A_2LIV_TARD    '
206200990921     c                   exsr      MEMRES
206300990921     c                   goto      endammava
206400081204   x5c                   else
206500990921     c
206600070711     c** Altrimenti e' colpa del terminal
206700070711     c*  prima di dare la colpa al terminal verifico se ho fatto "il furbo"
206800070711     c*  in fnbrve
206900070713     c**** kbrve         chain     fnbrve1l
207000070713     c****               if        %found(fnbrve1l) and E_brvcan=' '
207100070713     c****               eval      wke1='DA_CREARE      '
207200070713     c****               exsr      MEMRES
207300070711     c* se trovato, esco dal pgm ho attribuito la responsabilità
207400070713     c****               if        o50err=' '
207500070713     c****               goto      endammava
207600070713     c****               else
207700070711     c* Se non trovato record valido, procedo come ora
207800070713     c****               clear                   o50err
207900070713     c****               clear                   o50msg
208000070713     c****               endif
208100070713     c****               endif
208200070711     c
208300070711     c
208400081211     c                   eval      wfgsspuan=wfgs
208500090831     c                   movel     'A'           wtipoflp
208600081211
208700081204    6c                   if        wflp=i50tfa
208800990304     c                   eval      wke1='A_ARR_NOSPUTER '
208900990210     c                   exsr      MEMRES
209000990210     c                   goto      endammava
209100990210     c                   else
209200990304     c                   eval      wke1='A_TRA_NOSPUTER '
209300990210     c                   exsr      MEMRES
209400990210     c                   goto      endammava
209500081204    6c                   endif
209600081204    5c                   endif
209700081204    4c                   endif
209800990921    3c                   endif
209900990210     c**
210000090421     c*
210100990210     c                   clear                   WPREC200
210200990210     c                   clear                   WPRECNPG
210300990210     c                   clear                   WPRECFGS
210400990210     c                   clear                   WPRECSPG
210500990305     c
210600990305     c                   exsr      terspunta
210700070821     c                   eval      wNPGspuan=wnpg
210800070821     c                   eval      wflpspuan=wflp
210900990309     c                   eval      wfgsspuan=wfgs
211000090421     c
211100090421     c* Se ci sono solo le spunte con anomalie, le considerzioni del
211200090421     c*  precedente le devo fare sulla bolla e non sulle spunte
211300090421   3ac                   if        SoloCAN='S'
211400090421     c                   exsr      DaBolla
211500090421     c
211600090421     c
211700090421  x3ac                   else
211800090421     c* Cerco il precedente dalle spunte
211900090421     c                   movel     wdfv          w0200
212000090421     c                   move      wscar         w0200            20 0
212100990305     c
212200990210     c                   eval      zz=1
212300990210    3c     waavdfv(zz)   dowgt     0
212400990210     c                   movel     waavdfv(zz)   waav200          20 0
212500990210     c                   move      waavsca(zz)   waav200          20 0
212600990210     c*
212700990211     c* Escludo se waav20 e fgs sono il p.o. che ha sparato l'anomalia
212800990211     c                   movel     wnpg          w001a
212900990211     c     waavsca(zz)   ifne      wscar
213000990211     c     waavfgs(zz)   orne      wfgs
213100990211     c     waavnpg(zz)   orne      w001a
213200990211     c     waavdfv(zz)   orne      wdfv
213300990211     c
213400990210    4c     waav200       ifle      w0200
213500990210     c     waav200       andgt     wprec200
213600990210     C                   MOVEL     Waav200       wprec200
213700990210     c                   movel     waavnpg(zz)   wprecnpg
213800990210     c                   movel     waavfgs(zz)   wprecfgs
213900990210     c                   movel     waavspg(zz)   wprecspg
214000990210    4c                   endif
214100990211    4c                   endif
214200990210     c                   add       1             zz
214300990210    3c                   enddo
214400081204     c
214500990210     c** Se non ho trovato il precedente --> e' colpa della partenza
214600990210    3c     wprecnpg      ifeq      0
214700990308     c                   movel(P)  'A_PART_NOSI' wke1
214800990210     c                   exsr      MEMRES
214900990210     c                   goto      endammava
215000990210   x3c                   else
215100990211     c**  altrimenti cerco il suo terminal e attribuisco
215200990210     c**  Imposto i campi della spunta a cui devo dare la responsabilita'
215300990210     c                   eval      wnpg=wprecnpg
215400990210     c                   eval      wfgs=wprecfgs
215500990210     c                   eval      wspg=wprecspg
215600990210     c                   movel     wprec200      wdfv
215700990210     c                   move      wprec200      wscar
215800990210     c**
215900990211     c                   movel     'N'           wsianom
216000990210     c                   EXSR      RESPOAAV
216100990210     c                   goto      endammava
216200090421   3ac                   endif
216300090421    3c                   endif
216400990210    2c                   endif
216500990210     c
216600990209    1c                   endif
216700990129     c
216800990208     c     endammava     endsr
216900090421      *****************************************************************
217000090421      * ATTRIBUZIONE RESPONSABILITÀ AMMANCHI E AVARIE DDA BOLLA
217100090422      *  IN PRESENZA DELLE SOLE SPUNTE CON ANOMALIA
217200090421      *****************************************************************
217300090421     c     DaBolla       BEGSR
217400090421     c                   clear                   wscar
217500090421     c
217600090421     c* Se chi ha messo l'anomalia è la partenza --> colpa della partenza
217700090422     c* (l'ha già fatto sopra...però controllo solo le categorie 1 5 e IMP
217800090422     c*  qui daccio rientrare anche le altre categorie...)
217900090421     c                   select
218000090421    1c                   when      wfgsspuan=i50tfp or  wfgsspuan=i50lnp or
218100090421     c                             wflpspuan=i50tfp
218200090421     c                   movel(P)  'A_PART_NOSI' wke1
218300090421     c                   exsr      MEMRES
218400090421     c
218500090421    1c                   when      wfgsspuan=i50tfa
218600090421     c                   clear                   wfgs
218700090421     c* c'e' transito
218800090421    2c                   if        artflp>0
218900090421     c* Se c'e' la data uscita transito, imposto come categoria la 1-partenza
219000090421    3c                   if        artdut>0
219100090421     c                   eval      wfgs=artflp
219200090422     c                   eval      wflp=artflp
219300090421     c                   eval      wdfv=artdut
219400090421     c                   eval      wnpg=1
219500090421     c                   eval      wspg=' '
219600090421   x3c                   else
219700090421    4c                   if        artdet>0
219800090421     c                   eval      wfgs=artflp
219900090421     c                   eval      wdfv=artdet
220000090421     c                   eval      wnpg=2
220100090421     c                   eval      wspg=' '
220200090421     c                   exsr      TERSPUNTA
220300090421    4c                   endif
220400090421    3c                   endif
220500090422     c* Se il p.o. di transito è un terminal--> colpa sua
220600090422     c* se è un secondo livello --> colpa del suo terminal
220700090422     c                   eval      wke1='A_TRA_NOSI     '
220800090422     c                   exsr      MEMRES
220900090421    2c                   endif
221000090421     c
221100090421     c*  Se ancora vuoto --> colpa alla partenza
221200090421    2c                   if        wfgs=0
221300090421     c                   movel(P)  'A_PART_NOSI' wke1
221400090421     c                   exsr      MEMRES
221500090421    2c                   endif
221600090421     c
221700090422     c* Se è un transito ed è un terminal  --> colpa alla partenza
221800090422     c* (è per forza un terminal perchè se non lo fosse sarebbe già rientrato
221900090422     c*  sopra nella routine "A_TRA_NOSPUTER")
222000090422     c*  il realtà faccio rientrare tutti i caso mancanti (ma dovrebbe essere solo
222100090422     c*  questo)
222200090422    1c                   other
222300090421     c                   movel(P)  'A_PART_NOSI' wke1
222400090421     c                   exsr      MEMRES
222500090421     c                   endSL
222600090421     c                   ENDSR
222700990210      *****************************************************************
222800990210      * LETTURA DELLE SPUNTE PER CERCARE QUELLE CON ANOMALIA
222900990210      *****************************************************************
223000990210     c     RESPOAAV      BEGSR
223100990211    1c     wfgs          ifne      dctlna
223200990210     c                   exsr      terspunta
223300990210     c**
223400990210     c** Spuntato in arrivo dopo lo scarico
223500990211    2c     wflp          ifeq      i50tfa
223600990211    3c     wsianom       ifeq      'S'
223700990316     c
223800990316     c* Ci sono 2 casi:
223900990316     c* 1) chi ha sparato e il terminal di arrivo
224000990316    4c                   if        wfgs=i50tfa
224100990316     c                   if        werr=' '
224200990316     c                   eval      wke1='A_TER_DOPOSCAR '
224300990316     c                   else
224400990316     c                   eval      wke1='A_ERRTERSPUNTA '
224500990316     c                   endif
224600990316   x4c                   else
224700990316     c
224800990316     c* 2) Chi ha sparato e' un 2 livello della stessa area --> disguido
224900990316     c                   eval      wflp=wfgs
225000990316    5c                   if        werr=' '
225100990316     c                   eval      wke1='A_TRA_DOPOSCAR '
225200990316     c                   else
225300990316     c                   eval      wke1='A_ERRTRASPUNTA '
225400990316    5c                   endif
225500990316    4c                   endif
225600990316    c
225700990216     c
225800990316   x3c                   else
225900990305     c
226000990305     c* Se il terminal di arrivo del precedente = al terminal di chi ha
226100990305     c*  rilevato l'anomalia, vuol dire che ha scaricato tardi la pistola
226200081211    4c                   if        wflp=wflpspuan
226300081211    5c                   if        wfgs=wfgsspuan
226400990305     c                   eval      wke1='A_ARR_TARD     '
226500081211   x5c                   else
226600990309     c* Disguido in area
226700990309     c
226800990309     c                   eval      wke1='A_ARR_DISGAREA '
226900081211    5c                   endif
227000990309     c
227100081211   x4c                   else
227200990304     c                   eval      wke1='A_ARR_NOSI     '
227300081211    4c                   endif
227400990305     c
227500990305    3c                   endif
227600090831     c                   movel     'A'           wtipoflp
227700990210     c                   exsr      MEMRES
227800990305     c
227900990211   x2c                   else
228000990305     c
228100990211     c** Spuntato o dalla partenza
228200990211    3c     wflp          ifeq      i50tfp
228300990211    4c     wsianom       ifeq      'S'
228400990304     c                   eval      wke1='A_PART_SI      '
228500990211     c                   else
228600990304     c                   eval      wke1='A_PART_NOSI    '
228700990211    4c                   endif
228800090831     c                   movel     'P'           wtipoflp
228900990211     c                   exsr      MEMRES
229000990305     c
229100990211   x3c                   else
229200990305     c
229300990211     c** Spuntato al transito/disguido dopo lo scarico
229400990211    4c     wsianom       ifeq      'S'
229500990216     c                   if        werr=' '
229600990304     c                   eval      wke1='A_TRA_DOPOSCAR '
229700990216     c                   else
229800990304     c                   eval      wke1='A_ERRTRASPUNTA '
229900990216     c                   endif
230000990216     c
230100990211     c                   else
230200990304     c                   eval      wke1='A_TRA_NOSI     '
230300990211    4c                   endif
230400090831     c                   movel     'A'           wtipoflp
230500990210     c                   exsr      MEMRES
230600990211    3c                   endif
230700990211    2c                   endif
230800990211   x1c                   else
230900990210     c** Spuntato in arrivo dopo lo scarico
231000990211    2c     wsianom       ifeq      'S'
231100990216     c                   if        werr=' '
231200990304     c                   eval      wke1='A_ARR_DOPOSCAR '
231300990211     c                   else
231400990304     c                   eval      wke1='A_ERRARRSPUNTA '
231500990216    2c                   endif
231600990216     c
231700990216     c                   else
231800990305     c
231900990305     c* Se il terminal di arrivo del precedente = al terminal di chi ha
232000990305     c*  rilevato l'anomalia, vuol dire che ha scaricato tardi la pistola
232100990331     c                   if        wfgs=wfgsspuan
232200990305     c                   eval      wke1='A_ARR_TARD     '
232300990305     c                   else
232400990304     c                   eval      wke1='A_ARR_NOSI     '
232500990211    2c                   endif
232600990305    2c                   endif
232700990305     c
232800090831     c                   movel     'A'           wtipoflp
232900990210     c                   exsr      MEMRES
233000990210    3c                   endif
233100990211    1c                   ENDSR
233200990209      *****************************************************************
233300990209      * LETTURA DELLE SPUNTE PER CERCARE QUELLE CON ANOMALIA
233400990209      *****************************************************************
233500990209     c     LETBRV        BEGSR
233600990209     c                   z-add     1             zz                3 0
233700990210     c* Skiere in cui memorizzo per P.O. la spunta con data  piu' bassa
233800090428     c*  a parita di data, privilegiando le categorie ARR tenendo conto
233900990211     c*  delle data/ora scarico + tolleranza
234000990209     c                   clear                   waavnpg
234100990209     c                   clear                   waavdfv
234200990209     c                   clear                   waavfgs
234300000322     c                   clear                   waavAS
234400990209     c                   clear                   waavsca
234500990209     c                   clear                   waavdef
234600990210     c                   clear                   waavspg
234700000322     c                   clear                   waavnps
234800990209     c
234900990209     c* Campi in cui memorizzo il primo che ha rilevato l'anomalia
235000990210     c                   clear                   wnpg
235100990210     c                   clear                   wdfv
235200990210     c                   clear                   wfgs
235300990210     c                   clear                   wscar
235400990210     c                   clear                   wdef
235500990210     c                   clear                   wspg
235600990212     c                   clear                   werr
235700070711     c                   clear                   wnfva
235800000322     c                   clear                   brvas
235900990209     c* Campi di comodo
236000990209     c
236100070131     c     kdcd          setll     fnbrv07l
236200070131     c     kdcd          reade     fnbrv07l                               34
236300990209    1c     *in34         doweq     *off
236400000229     c* se data e ora scarico uguale a zeros muovo la data manutenzione spunte
236500000229      *
236600000229     c                   if        brvscar = 0
236700000229     c                   eval      brvdcs = brvdfs
236800000229     c                   eval      brvhcs = brvhms
236900000229     c                   endif
237000000229      *
237100990212     c                   clear                   w001a             1
237200990212     c                   clear                   wcarica           1
237300990209     c* Escludo le pistole completamente fasulle
237400990316     c*  Includo il transito come 89
237500990209     c                   movel     brvnps        w002a
237600990209     c     w002a         lookup    npsno                                  31
237700990209    3c     *in31         ifeq      *off
237800990316     c     brvnps        oreq      89
237900990209     c* Prendo la data
238000990209     c                   clear                   dslv53
238100990209     c                   movel     brvnpg        d53npg
238200000209     c                   z-add     brvnfv        d53nfv
238300990209     c                   movel     brvfgs        d53fgs
238400990209     c                   movel     brvfle        d53flf
238500990209     c                   call      'FNLV53R'
238600990209     c                   parm                    dslv53
238700990209     c**
238800990209     c** se non trovata data ignoro
238900990209    4c     d53err        ifeq      *blanks
239000050204     c* Se data foglio = sabato, la imposto di lunedì
239100050204     c                   Clear                   wgioset
239200050204     c     *iso          Movel     d53dfv        datadmy
239300050204     c     *dmy          Movel     datadmy       w0060             6 0
239400050204     c                   Movel     w0060         wggmmaa
239500050204     c                   Call      'XGIOSE1'
239600050204     c                   Parm                    wggmmaa
239700050204     c                   Parm                    wgioset
239800050204     c
239900050204     c     *iso          move      d53dfv        wdataanm
240000130122     c
240100130122    5c                   if        wgioset='6'
240200050204     c                   adddur    2:*D          wdataanm
240300130122    5c                   endif
240400130122     c
240500130122     c                   setoff                                       34
240600130122    5c                   dow       not *in34
240700050204     c* controllo se e' festivo
240800050204     C     *iso          move      wdataanm      kdata
240900050204     C                   Z-ADD     68            KTFp
241000050204     C                   clear                   KTFa
241100050204     C     KCLN          CHAIN     AZCLN01L                           34
241200050204    7C     *IN34         IFEQ      *OFF
241300050204     C     MAT(KGG)      andne     ' '
241400050204     C     *IN34         oreq      *OFF
241500050204     C     POM(KGG)      andne     ' '
241600050204     c**
241700050204     c                   adddur    1:*D          wdataanm
241800050204     c                   else
241900050204     c                   seton                                        34
242000050204    7c                   endif
242100130122    5c                   enddo
242200050204     c
242300050204     c     *iso          movel     wdataanm      d53dfv
242400050204     c
242500050204     c
242600990209     c                   eval      zz=1
242700990209     c     brvfgs        lookup    waavfgs(zz)                            31
242800990209    5c                   if        not *in31
242900990209     c                   eval      zz=1
243000990209     c     000           lookup    waavfgs(zz)                            31
243100990209     c                   eval      waavfgs(zz)=brvfgs
243200990209    5c                   endif
243300000322     c
243400000322     c** prendo as di appartenenza
243500000322     c**  per cercare altro p.o. gestione su stesso AS
243600000322     c                   clear                   wstessoas
243700000322     c
243800000322     c     brvfgs        chain     azorg01l                           33
243900021120     c  n33              movel     orgde0        og140
244000021120     c  n33              movel     §ogaex        brvas
244100000322     c   33              movel     brvfgs        brvas
244200000322     c
244300000322    5c                   if        not *in33
244400041006     c* Se il p.o. spunta è la linea di arrivo o il terminal di arrivo
244500041006     c*  ignoro il fatto dello stesso AS
244600041006     c                   if        brvfgs<>i50lna and brvfgs<>i50tfa  and
244700041006     c                             brvfgs<>i50lnp
244800000322     c                   eval      jj=1
244900000322     c     brvas         lookup    waavas(jj)                             33
245000090428     c* elaboro anche se stesso p.o. per ignorare spunta 89 se poi c'e' reale
245100090428     c***                dow       *in33 and brvfgs=waavfgs(jj)  or
245200090428     c                   dow       *In33 and waavfgs(jj)=i50lna  or
245300041006     c                             *In33 and waavfgs(jj)=i50tfa  or
245400041006     c                             *In33 and waavfgs(jj)=i50lnp
245500000322     c                   eval      JJ=1+jj
245600021120     c     brvas         lookup    waavas(jj)                             33
245700000322     c                   enddo
245800000322     c* Trovato!!
245900000322     c   33              movel     'S'           wstessoas
246000041006     c
246100000322    5c                   endif
246200041006    5c                   endif
246300000322     c
246400000322     c** Se trovo spunta di altra filiale gestione ma stesso as e quella
246500000322     c**  letta e' pistola 89 --> la ignoro
246600000322   4ac                   if        wstessoas='S' and brvnps<>89 OR
246700000322     c                             wstessoas=' '
246800000322     c
246900000322    5c                   if        wstessoas='S' AND waavnps(jj)=89
247000000322     c** sostituisco la spunta e faccio fina che la precedente non ci
247100000322     c**  sia
247200000322     c                   eval      zz=jj
247300000322     c                   eval      waavfgs(zz)=brvfgs
247400000322     c                   clear                   waavdfv(zz)
247500000322     c                   clear                   waavnpg(zz)
247600000322     c                   clear                   waavspg(zz)
247700000322     c                   clear                   waavsca(zz)
247800000322    5c                   endif
247900990507     c
248000990507     c** Imposto campi di comodo
248100990507     c* campi nuova spunta letta
248200050207     c                   movel     brvnps        wnewnps           2 0
248300050207     c                   movel     brvnpg        wnewnpg           1
248400990211     c                   movel     d53def        wnewdef           1
248500990507     c                   movel     d53dfv        wnewdfv           8 0
248600990507     c                   movel     d53spg        wnewspg           1
248700990507     c                   movel     brvscar       wnewscar         12 0
248800990507     c* campi vecchia spunta memorizzata
248900990211     c                   movel     waavnpg(zz)   woldnpg           1
249000990211     c                   movel     waavdef(zz)   wolddef           1
249100990211     c                   movel     waavdfv(zz)   wolddfv
249200990507     c                   movel     waavspg(zz)   woldspg           1
249300050207     c                   movel     waavnps(zz)   woldnps           2 0
249400990507     c                   movel     waavsca(zz)   woldscar         12 0
249500040907     c                   EXSR      SCEGLIxav
249600990507     c
249700990507     c** spunta da caricare
249800990211    5c                   if        wcarica='S'
249900990209     c                   movel     brvnpg        waavnpg(zz)
250000990210     c                   eval      waavdfv(zz)=d53dfv
250100990209     c                   eval      waavsca(zz)=brvscar
250200990209     c                   eval      waavdef(zz)=d53def
250300990210     c                   eval      waavspg(zz)=d53spg
250400000322     c                   eval      waavnps(zz)=brvnps
250500000322     c                   eval      waavAS(zz)=brvas
250600990210    5c                   endif
250700990209     c**
250800990209     c** C'E' ANOMALIA:  tengo la spunta con la data piu' bassa
250900990209   5ac     brvcan        ifne      *blanks
251000990316     c     brvnps        andne     89
251100050615     c     brvcan        lookup    Noanomspu                              31
251200050615   5bc                   if        not *in31
251300990210     c                   clear                   WCARICA
251400990209     c**
251500990211     c* Controllo quale delle 2 spunte e' cat.arrivi
251600990507     c* reimposto solo i campi old perche' la NEW e' la stessa spunta letta
251700990211     c                   movel     wnpg          woldnpg           1
251800990211     c                   movel     wdef          wolddef           1
251900990507     c                   movel     wspg          woldspg
252000990211     c                   movel     wdfv          wolddfv           8 0
252100990507     c                   movel     wscar         woldscar         12 0
252200990507     c
252300040907     c                   EXSR      SCEGLIxav
252400990210     c
252500990210    5c                   if        wcarica='S'
252600990210     c                   eval      wdfv   =d53dfv
252700990210     c                   eval      wnpg   =brvnpg
252800990210     c                   eval      wscar  =brvscar
252900990210     c                   eval      wdef   =d53def
253000990210     c                   eval      wfgs   =brvfgs
253100990210     c                   eval      wspg   =d53spg
253200070711     c                   eval      wNFVa  =d53NFV
253300990212     c                   clear                   werr
253400990212     c* verifico per gli ammanchi se e' stata inserita l'anomalia sbagliata
253500990212     c*  -> se c'e' solo questa o e' la piu' piccola e' colpa sua
253600990212     c                   movel     brvnpg        w001a
253700990212     c     w001a         lookup    catarr                                 31
253800990315    6c   31brvnpg        ifeq      1
253900990212     c     d53def        andeq     ' '
254000990212     c                   setoff                                       31
254100990315    6c                   endif
254200990212     c
254300990315    6c                   if        *in31
254400990330    7c                   if        §tadragr='A'
254500990330     c     brvcan        lookup    amspu                                  34
254600990212     c** Anomalia non giusta --> memorizzo
254700990330     c  N34              eval      werr='E'
254800990315    7c                   endif
254900990330    7c                   if        §tadragr='V'
255000990330     c     brvcan        lookup    avspu                                  34
255100990330     c  N34              eval      werr='E'
255200990315    7c                   endif
255300990212    6c                   endif
255400990212     c
255500990209    5c                   endif
255600050615   5bc                   endif
255700050615   5ac                   endif
255800000322     c
255900000322  x4ac                   else
256000000322     c                   if        waavfgs(zz)<>0   AND waavdfv(zz)=0
256100000322     c                   clear                   waavfgs(zz)
256200000322     c                   endif
256300000322     c
256400000322   4ac                   endif
256500990209     c
256600990209    4c                   endif
256700990209    3c                   endif
256800990209     c**
256900070131     c     kdcd          reade     fnbrv07l                               34
257000990209    1c                   enddo
257100990209     c                   ENDSR
257200990507      *****************************************************************
257300040907      *  vedo quale delle 2 spunte tenere per ammanchi avarie
257400990507      *****************************************************************
257500040907     c     SCEGLIXAV     BEGSR
257600990507     c* determino se sono spunte arrivi
257700990507     c                   EXSR      DETERARR
257800990507     c
257900990507    5c                   SELECT
258000990507     c* Data OLD non presente --> carico
258100990507     c                   WHEN      wolddfv=0
258200990507     c                   eval      wcarica='S'
258300990507     c
258400990507     c* Data NEW minore data OLD --> carico solo se la old e' arrivi con
258500990507     c*      scarico minore
258600990507    5c                   WHEN      wnewDFV<wolddfv
258700990507     c* se la spunta con data minore e' una categoria partenza e quella con
258800990507     c*    data maggiore e' una arrivi, tengo quella arrivi se la data
258900990507     c*    di scarico e' minore
259000990507     c     wnewnpg       lookup    catpar                                 31
259100990507    6c   31              if        wnewnpg='3' and wnewspg<>'P'
259200990507     c                   setoff                                       31
259300990507    6c                   endif
259400990507     c**
259500990507     c** La spunta scaricata non e' partenza --> la tengo
259600990507    6c                   if        not *in31  OR
259700990507     c                             *in31  and woldarr<>'A'
259800990507     c                   eval      wcarica='S'
259900990507   x6c                   else
260000071002     c* la spunta scaricata e' partenza con pistola autogenerata 89
260100071002     c*  tengo sempre la spunta arrivi altrimenti
260200071002     c*  controllo data ora scarico
260300071002   6ac                   if        wnewnps<>89
260400071002     c
260500990507    7c     wnewscar      iflt      woldscar
260600990507     c                   movel     wnewscar      w0080             8 0
260700990507     c                   move      wnewscar      w0040             4 0
260800990507     c                   EXSR      ADDTOLLER
260900990507     c                   movel     w0080         w0120
261000990507     c                   move      w0040         w0120            12 0
261100990507    8c     w0120         iflt      woldscar
261200990507     c                   eval      wcarica ='S'
261300990507    8c                   endif
261400990507    7c                   endif
261500071002   6ac                   endif
261600071002    6c                   endif
261700990507     c
261800990507     c* Se date uguali --> prendo quella con caricamento piu' basso o la
261900990507     c*                    categoria arrivi se rientra nei minuti di tollera
262000050207     c*                    Se sono due spunte arrivi tengo la pist.reale
262100990507     c
262200990507    5c                   WHEN      wnewDFV=wolddfv
262300990507     c                   EXSR      TIENIARR
262400990507     c
262500990507     c* Se la data e' > verifico se sto scaricando una arrivi e ho memorizza
262600990507     c*  to una partenza
262700990507    5c                   WHEN      wnewDFV>wolddfv AND wnewarr='A'
262800990507     c     woldnpg       lookup    catpar                                 31
262900990507    6c   31              if        woldnpg='3' and woldspg<>'P'
263000990507     c                   setoff                                       31
263100990507    6c                   endif
263200990507     c
263300990507     c** La spunta memorizzata deve essere una partenza
263400990507    6c                   if        *in31
263500990507     c
263600990507     c* la spunta scaricata e' partenza --> controllo data ora scarico
263700990507    7c     wnewscar      ifle      woldscar
263800990507     c                   eval      wcarica ='S'
263900990507     c                   else
264000990507     c                   movel     woldscar      w0080             8 0
264100990507     c                   move      woldscar      w0040             4 0
264200990507     c                   EXSR      ADDTOLLER
264300990507     c                   movel     w0080         w0120
264400990507     c                   move      w0040         w0120            12 0
264500990507    8c     wnewscar      ifle      w0120
264600990507     c                   eval      wcarica ='S'
264700990507    8c                   endif
264800990507    7c                   endif
264900990507    6c                   endif
265000071002     c
265100071002     c** La spunta memorizzata è partenza autogenerata con pistola 89
265200071002     c** e la spunta arrivi è pistola reale
265300071002     c** tengo sempre la spunta arrivi
265400071002    6c                   if        *in31  and woldnps=89
265500071002     c                   eval      wcarica ='S'
265600071002    6c                   endif
265700990507     c
265800990507    5c                   endsl
265900990507     c**
266000990507     c                   ENDSR
266100040907      *****************************************************************
266200040907      *  Vedo quale delle 2 spunte tenere per MANCANZE
266300040907      *****************************************************************
266400040907     c     SCEGLIXMA     BEGSR
266500040907     c* determino se sono spunte arrivi
266600040907     c                   EXSR      DETERARR
266700040907     c
266800040907    5c                   SELECT
266900040907     c* Data NEW minore data OLD --> carico solo se la old e' arrivi con
267000040907     c*      scarico minore
267100040907    5c                   WHEN      wnewDFV<wolddfv  and woldarr='A'
267200040907     c* se la spunta con data minore e' una categoria partenza e quella con
267300040907     c*    data maggiore e' una arrivi, tengo quella arrivi se la data
267400040907     c*    di scarico e' minore
267500040907     c     wnewnpg       lookup    catpar                                 31
267600040907    6c   31              if        wnewnpg='3' and wnewspg<>'P'
267700040907     c                   setoff                                       31
267800040907    6c                   endif
267900040907     c**
268000040907     c* la spunta scaricata e' partenza --> controllo data ora scarico
268100040907    6c                   if        *in31
268200040907    7c     wnewscar      ifge      woldscar
268300040907     c                   eval      wcarica ='S'
268400040907     c                   else
268500040907     c                   movel     wnewscar      w0080             8 0
268600040907     c                   move      wnewscar      w0040             4 0
268700040907     c                   EXSR      ADDTOLLER
268800040907     c                   movel     w0080         w0120
268900040907     c                   move      w0040         w0120            12 0
269000040907    8c     w0120         ifge      woldscar
269100040907     c                   eval      wcarica ='S'
269200040907    8c                   endif
269300040907    7c                   endif
269400040907    6c                   endif
269500040907     c
269600040907     c* Se date uguali --> prendo quella con caricamento piu' alto  o la
269700040907     c*                    categoria part.  se rientra nei minuti di tollera
269800040907     c
269900040907    5c                   WHEN      wnewDFV=wolddfv
270000040907     c
270100040907    6c     wnewscar      ifgt      woldscar
270200040907     c     woldarr       andeq     wnewarr
270300040907     c                   eval      wcarica='S'
270400040907   x6c                   else
270500040907     c**
270600040907     c* Valuto quale tenere con i minuti di tolleranza
270700040907     c     wnewnpg       lookup    catpar                                 31
270800040907    7c   31              if        wnewnpg='3' and wnewspg<>'P'
270900040907     c                   setoff                                       31
271000040907    7c                   endif
271100040907     c
271200040907    7c                   if        *in31
271300040907    8c     wnewscar      ifge      woldscar
271400040907     c                   eval      wcarica='S'
271500040907   x8c                   else
271600040907     c                   movel     wnewscar      w0080             8 0
271700040907     c                   move      wnewscar      w0040             4 0
271800040907     c                   EXSR      ADDTOLLER
271900040907     c                   movel     w0080         w0120
272000040907     c                   move      w0040         w0120
272100040907    9c     w0120         ifge      woldscar
272200040907     c                   eval      wcarica='S'
272300040907    9c                   endif
272400040907    8c                   endif
272500040907    7c                   endif
272600040907    c
272700040907     c     woldnpg       lookup    catpar                                 31
272800040907    7c   32              if        wnewnpg='3' and wnewspg<>'P'
272900040907     c                   setoff                                       32
273000040907    7c                   endif
273100040907    7c                   if        *in32
273200040907    8c     wnewscar      ifgt      woldscar
273300040907     c                   movel     woldscar      w0080             8 0
273400040907     c                   move      woldscar      w0040             4 0
273500040907     c                   EXSR      ADDTOLLER
273600040907     c                   movel     w0080         w0120
273700040907     c                   move      w0040         w0120            12 0
273800040907    9c     wnewscar      ifgt      w0120
273900040907     c                   eval      wcarica ='S'
274000040907    9c                   endif
274100040907    8c                   endif
274200040907    7c                   endif
274300040907     c* Se ne vecchia nè nuova partenza, tengo il caricamento + alto
274400040907    7c                   if        not *in31 and not *in32
274500040907    8c     wnewscar      ifgt      woldscar
274600040907     c                   eval      wcarica ='S'
274700040907    8c                   endif
274800040907    7c                   endif
274900040907     c
275000040907    6c                   endif
275100040907     c
275200040907     c* Se la data e' > verifico se sto scaricando una arrivi e ho memorizza
275300040907     c*  to una partenza
275400040907    5c                   WHEN      wnewDFV>wolddfv AND wnewarr=' '
275500040907     c                   eval      wcarica ='S'
275600040907     c
275700040907    5c                   WHEN      wnewDFV>wolddfv AND wnewarr='A'
275800040907     c     woldnpg       lookup    catpar                                 31
275900040907    6c   31              if        woldnpg='3' and woldspg<>'P'
276000040907     c                   setoff                                       31
276100040907    6c                   endif
276200040907     c
276300040907     c** La spunta memorizzata deve essere una partenza
276400040907    6c                   if        *in31
276500040907    7c     wnewscar      ifgt      woldscar
276600040907     c                   movel     woldscar      w0080             8 0
276700040907     c                   move      woldscar      w0040             4 0
276800040907     c                   EXSR      ADDTOLLER
276900040907     c                   movel     w0080         w0120
277000040907     c                   move      w0040         w0120            12 0
277100040907    8c     wnewscar      ifgt      w0120
277200040907     c                   eval      wcarica ='S'
277300040907    8c                   endif
277400040907    7c                   endif
277500040907     c
277600040907     c* la spunta scaricata e' partenza --> controllo data ora scarico
277700040907   x6c                   else
277800040907     c* Se la vecchia non è partenza, carico
277900040907     c                   eval      wcarica ='S'
278000040907    6c                   endif
278100040907     c
278200040907    5c                   endsl
278300040907     c**
278400040907     c                   ENDSR
278500990211      *****************************************************************
278600990211      *   TENGO LA SPUNTA ARRIVI CONSIDERANDO I MINUTI DI TOLLERANZA
278700990211      *****************************************************************
278800990211     c     TIENIARR      BEGSR
278900050207     c* se sono 2 cat.arrivi -stessa data e c'e' una pistola fasulla, tengo
279000050207     c*  la pistola reale a prescindere dallo scarico
279100050207     c                   setoff                                       3132
279200050207    0c                   if        woldarr=wnewarr and woldarr='A'
279300050207     c                   movel     wnewnps       w002a
279400050207     c     w002a         lookup    npsno                                  31
279500050207     c                   movel     woldnps       w002a
279600050207     c     w002a         lookup    npsno                                  32
279700090105
279800090105     c* se la vecchia è fasulla, e la nuova  no, tengo la NUOVA
279900090105     c                   if        *in32 and  not *in31
280000090105     c                   eval      wcarica='S'
280100090105     c                   goto      endtieni
280200090105     c                   endif
280300090105     c* se la nuova è fasulla, e la vecchia no, tengo la vecchia
280400090105     c                   if        *in31 and  not *in32
280500090105     c                   goto      endtieni
280600090105     c                   endif
280700090105
280800090105   x0c                   else
280900090105     c
281000090105     c* oppure se una entrata e una arrivi fasulla --> tengo l'entrata
281100090105     c                   movel     wnewnps       w002a
281200090105     c     w002a         lookup    npsno                                  31
281300090105     c                   movel     woldnps       w002a
281400090105     c     w002a         lookup    npsno                                  32
281500090105     c* se la vecchia è fasulla arrivi, e la nuova no entrata, tengo laNUOVA
281600090105    1c                   if        *in32 and  not *in31  and
281700090105     c                             woldarr='A' and wnewarr='5'
281800090105     c                   eval      wcarica='S'
281900090105     c                   goto      endtieni
282000090105    1c                   endif
282100090105     c* se la nuova è fasulla arrivi, e la vecchia no entrata, tengo la vecchia
282200090105    1c                   if        *in31 and  not *in32     and
282300090105     c                             wnewarr='A' and woldarr='5'
282400090105     c                   goto      endtieni
282500090105    1c                   endif
282600090105
282700090105    0c                   endif
282800050207     c
282900050207     c
283000050207     c* Se sono entrambe non arrivi o entrambe arrivi tengo lo scarico + bas
283100990211    1c     wnewscar      iflt      woldscar
283200990507     c     woldarr       andeq     wnewarr
283300990211     c                   eval      wcarica='S'
283400050207     c                   goto      endtieni
283500050207    1c                   endif
283600990211     c**
283700990211     c* Valuto quale tenere con i minuti di tolleranza
283800990507    2c                   if        wnewarr='A'
283900090702    3c     wnewscar      ifge      woldscar
284000990211     c                   movel     woldscar      w0080             8 0
284100990211     c                   move      woldscar      w0040             4 0
284200990211     c                   EXSR      ADDTOLLER
284300990211     c                   movel     w0080         w0120
284400990211     c                   move      w0040         w0120
284500990211    4c     wnewscar      ifle      w0120
284600990211     c                   eval      wcarica='S'
284700990211    4c                   endif
284800990316     c
284900990316     c                   else
285000990316     c                   eval      wcarica='S'
285100990211    3c                   endif
285200990211    2c                   endif
285300990211    c
285400990507    2c                   if        woldarr='A'
285500090702    3c     wnewscar      iflt      woldscar
285600990211     c                   movel     wnewscar      w0080             8 0
285700990211     c                   move      wnewscar      w0040             4 0
285800990211     c                   EXSR      ADDTOLLER
285900990211     c                   movel     w0080         w0120
286000990211     c                   move      w0040         w0120            12 0
286100990507    4c     w0120         iflt      woldscar
286200990211     c                   eval      wcarica ='S'
286300990211    4c                   endif
286400990211    3c                   endif
286500990211    2c                   endif
286600990211     c
286700990211     c
286800050207     c     endtieni      ENDSR
286900990211      *****************************************************************
287000990211      *   DETERMINO QUELE DELLE SPUNTE E' CAT.ARRIVI
287100990211      *****************************************************************
287200990211     c     DETERARR      BEGSR
287300990507     c                   clear                   WOLDARR
287400990507     c                   clear                   WNEWARR
287500990211     c
287600990211     c     wnewnpg       lookup    catarr                                 31
287700990211     c   31              if        wnewnpg='1' and wnewdef=' '
287800990211     c                   setoff                                       31
287900990211     c                   endif
288000990507     c   31              movel     'A'           wnewarr           1
288100090105     c  n31              if        wnewnpg='5'
288200090105     c                   movel     '5'           wnewarr           1
288300090105     c                   endif
288400990211     c
288500990211     c     woldnpg       lookup    catarr                                 31
288600990211     c   31              if        woldnpg ='1' and wolddef   =' '
288700990211     c                   setoff                                       31
288800990211     c                   endif
288900990507     c   31              movel     'A'           woldarr           1
289000090105     c  n31              if        woldnpg='5'
289100090105     c                   movel     '5'           woldarr           1
289200090105     c                   endif
289300990507     c
289400990211     c                   ENDSR
289500990205      *****************************************************************
289600990211      *   SOMMO MINUTI DI TOLLERANZA
289700990205      *****************************************************************
289800990211     C     ADDTOLLER     BEGSR
289900990211     c                   clear                   w0060             6 0
290000990211     c                   movel     w0040         w0060
290100990211     c     *iso          move      w0080         wdata
290200990211     c     *iso          move      w0060         wora
290300990212     c                   adddur    §stdmtl:*mn   wtempo
290400990211     c     *iso          move      wdata         w0080
290500990211     c     *iso          move      wora          w0060
290600990211     c                   movel     w0060         w0040
290700990211     c
290800990211     c                   ENDSR
290900990211      *****************************************************************
291000990211      *   CONTROLLO SE IL COLLO ARRIVATO E' STATO spuntato da altre parti
291100990211      *****************************************************************
291200990211     C     SPUARR        BEGSR
291300990210     c                   clear                   wdfv
291400990210     c                   clear                   wdcs
291500990210     c                   clear                   whcs
291600990210     c                   clear                   wfgs
291700990210     c                   clear                   wnpg
291800990210     c                   clear                   wspg
291900990308     c                   clear                   wdef
292000040907     c                   clear                   wnfv
292100990311     c                   clear                   wEXPdfv
292200990311     c                   clear                   wEXPfgs
292300990311     c                   clear                   wEXPnpg
292400990311     c                   clear                   wEXPdef
292500990311     c                   clear                   wEXPscar
292600990205     c* cerco la spunta del collo con la data piu' alta e se > data arrivo
292700070131     c     kdcd          chain     fnbrv07l                           34
292800990311    1c     *in34         doweq     *off
292900060301     c
293000060301     c* Escludo, se richiesto, spunte autogenerate
293100060301   1ac                   if        (brvscar>0 and wautogen='N') or
293200060301     c                             wautogen=' '
293300000229     c* se data e ora scarico uguale a zeros muovo la data manutenzione spunte
293400000229      *
293500000229     c                   if        brvscar = 0
293600000229     c                   eval      brvdcs = brvdfs
293700000229     c                   eval      brvhcs = brvhms
293800000229     c                   endif
293900040906     c                   clear                   wcarica
294000000229      *
294100990205     c* escludo le pistole completamente fasulle, che non nascono nemmmeno
294200990205     c*   da un'altra spunta
294300990208     c                   movel     brvnps        w002a             2
294400990208     c     w002a         lookup    npsno                                  31
294500990311    2c     *in31         ifeq      *off
294600990205     c                   clear                   dslv53
294700990205     c                   movel     brvnpg        d53npg
294800000209     c                   z-add     brvnfv        d53nfv
294900990205     c                   movel     brvfgs        d53fgs
295000990205     c                   movel     brvfle        d53flf
295100990205     c                   call      'FNLV53R'
295200990205     c                   parm                    dslv53
295300990205     c** se non trovata data ignoro
295400990311    3c     d53err        ifeq      *blanks
295500990205     c     d53dfv        andge     wdatrifer
295600040906     c**
295700040907     c** Imposto campi di comodo  se stesso p.o. spunta  per vedere se tener
295800040907     c*   spunta partenza o spunta arrivi
295900040907     c                   if        brvfgs=wfgs
296000040906     c* campi nuova spunta letta
296100040906     c                   movel     brvnpg        wnewnpg           1
296200040906     c                   movel     d53def        wnewdef           1
296300040906     c                   movel     d53dfv        wnewdfv           8 0
296400040906     c                   movel     d53spg        wnewspg           1
296500040906     c                   movel     brvscar       wnewscar         12 0
296600040906     c* campi vecchia spunta memorizzata
296700040906     c                   movel     wnpg          woldnpg           1
296800040906     c                   movel     wdef          wolddef           1
296900040906     c                   movel     wdfv          wolddfv
297000040906     c                   movel     wspg          woldspg           1
297100040906     c                   movel     wscar         woldscar         12 0
297200040907     c                   EXSR      SCEGLIxma
297300040907     c                   else
297400040907    4c     d53dfv        ifgt      wdfv
297500040907     c     d53dfv        oreq      wdfv
297600040907     c     brvscar       andgt     wscar
297700040907     c                   eval      wcarica='S'
297800040907     c                   endif
297900040907     c                   endif
298000040906     c
298100040906     c** spunta da caricare
298200040906    5c                   if        wcarica='S'
298300990205     c**
298400040906    4c***  d53dfv        ifgt      wdfv
298500040906     c***  d53dfv        oreq      wdfv
298600040906     c***  brvscar       andgt     wscar
298700990210     c                   movel     d53dfv        wdfv
298800990308     c                   movel     d53def        wdef
298900990210     c                   movel     d53spg        wspg
299000990210     c                   movel     brvfgs        wfgs
299100990210     c                   movel     brvnpg        wnpg
299200040907     c                   z-add     brvnfv        wnfv
299300990210     c                   movel     brvscar       wscar
299400990311    4c                   endif
299500990311     c
299600040906     c* Se Export memorizzo la spunta defluenza con data >
299700990311    4c                   if        brvnpg=1 and d53def='S' and
299800990311     c                             lnafl1='E'
299900990311     c
300000990311    5c     d53dfv        ifgt      wEXPdfv
300100990311     c     d53dfv        oreq      wEXPdfv
300200990311     c     brvscar       andgt     wEXPscar
300300990311     c                   movel     d53dfv        wEXPdfv
300400990311     c                   movel     d53def        wEXPdef
300500990311     c                   movel     brvfgs        wEXPfgs
300600990311     c                   movel     brvnpg        wEXPnpg
300700990311     c                   movel     brvscar       wEXPscar
300800990311    5c                   endif
300900990311    4c                   endif
301000990311    c
301100990311    3c                   endif
301200990311    2c                   endif
301300060301   1ac                   endif
301400990205     c**
301500070131     c     kdcd          reade     fnbrv07l                               34
301600990311    1c                   enddo
301700990205     c
301800990205     c                   endsr
301900980506      *****************************************************************
302000980506      *   ROUTINE INIZIALE
302100980506      *****************************************************************
302200980506     C     *INZSR        BEGSR
302300980506      *
302400980506     C     *ENTRY        PLIST
302500980506     C                   PARM                    KPJBA
302600990204     C                   MOVEL     KPJBU         savkpjbu
302700170421
302800170421     c                   z-add     1             codut             1 0
302900990310     c**
303000990310     c* carico al primo posto al pistola 90
303100990311     c                   eval      npsno(1)='90'
303200990204     c**
303300990204     c* carico la tabella delle pistole fasulle
303400990204     c                   z-add     1             xx
303500990310     c                   z-add     2             yy                4 0
303600990204     c                   movel     '7J'          cod
303700990208     c     ktab2         chain     tabel                              31
303800990204     c     *in31         doweq     *off
303900990204     c                   movel     tbluni        ds7j
304000990204     c     §7jrps        ifeq      'A'
304100990204     C                   MOVEL     TBLKEY        NPS(XX)
304200990204     C                   ADD       1             XX
304300990204     C                   ENDIF
304400990303     c* Carico la tabella delle piu' fasulle 88 89 con priorita' 40
304500990205     c     §7jord        ifeq      040
304600990205     C                   MOVEL     TBLKEY        NPSNO(YY)
304700990205     C                   ADD       1             yy
304800990205     C                   ENDIF
304900990204     c**
305000990204     c     ktab2         reade     tabel                                  31
305100990204     C                   ENDDO
305200990209     c**
305300990305     c* Carico la tabella delle anomalie idd per la procedura danni
305400990305     c                   z-add     1             xx
305500990305     c                   movel     '7C'          cod
305600990305     c     ktab2         chain     tabel                              31
305700990305     c     *in31         doweq     *off
305800990305     c                   movel     tbluni        ds7c
305900990305     c     §7cdan        ifeq      'S'
306000990305     C                   MOVEL     TBLKEY        ANIDD(XX)
306100990305     C                   ADD       1             XX
306200990305     C                   ENDIF
306300990305     c**
306400990305     c     ktab2         reade     tabel                                  31
306500990305     C                   ENDDO
306600990211     c* Carico la tabella delle categorie che sono considerate ARRIVI
306700990210     c*   la defluenza adesso non la considero
306800990211     c                   eval      catarr(1)='2'
306900990211     c                   eval      catarr(2)='6'
307000990211     c* Carico la tabella delle categorie che sono considerate PARTENZA
307100990507     c                   eval      catpar(1)='1'
307200990211     c                   eval      catpar(2)='5'
307300990211     c                   eval      catpar(3)='3'
307400990209     c**
307500990210     c* carico la tabella dei tipi anomalie spunte gli "Ammanco"
307600990210     c                   z-add     1             xx
307700050615     c                   z-add     1             yy
307800050615     c                   z-add     1             zz
307900990210     c                   movel     '3E'          cod
308000990210     c     ktab2         chain     tabel                              31
308100990210     c     *in31         doweq     *off
308200990210     c                   movel     tbluni        ds3e
308300990330     c     §3efta        ifne      'V'
308400990330     C                   MOVEL     TBLKEY        AMSPU(XX)
308500990210     C                   ADD       1             XX
308600990210     C                   ENDIF
308700990330     c     §3efta        ifne      'A'
308800050615     C                   MOVEL     TBLKEY        AVSPU(yy)
308900050615     C                   ADD       1             yy
309000990330     C                   ENDIF
309100050615     c* Carico le anomalie spunte che di fatto non sono anomalie ma
309200170516     c*  servono per la chiusura distinta o come segnalazione
309300170516     c                   if        §3Efta='R' or §3Efta='S'
309400050615     C                   MOVEL     TBLKEY        NOANOMSPU(zz)
309500050615     C                   ADD       1             zz
309600050615     c                   endif
309700990210     c**
309800990210     c     ktab2         reade     tabel                                  31
309900990210     C                   ENDDO
310000010907      *
310100010907      * recupero la moneta corrente
310200010907     C                   CLEAR                   TIBS02DS
310300010907     C                   MOVEL     'C'           T02MOD
310400010907     C                   MOVEL     KNSIF         T02SIF
310500010907     C                   MOVEL     'GED'         T02COD
310600010907     C                   MOVEL     'DANNI'       T02KE1
310700010907     C                   CALL      'TIBS02R'
310800010907     C                   PARM                    KPJBA
310900010907     C                   PARM                    TIBS02DS
311000010910     C                   MOVEL     T02UNI        DGEDDN
311100990210     c**
311200990210     c* A seconda del tipo anomalia cerco la responsab.
311300990210     C                   clear                   tibs02ds
311400990210     C                   MOVEL     'C'           t02mod
311500990210     C                   MOVEL     knsif         t02sif
311600990210     C                   MOVEL     'STD'         t02cod
311700990210     C                   MOVEL(p)  '1'           t02ke1
311800010910     C                   MOVEL(P)  §gedDBA       t02ke2
311900990210     C                   CALL      'TIBS02R'
312000990210     C                   PARM                    KPJBA
312100990210     C                   PARM                    TIBS02DS
312200990210     c* non ci sono errori --> prendo i minuti di tolleranza per scarico spu
312300990210     c                   if        t02err=' '
312400990210     c                   movel     t02uni        dstd
312500990210     c                   else
312600990210     c                   eval      §stdmtl = 30
312700990210     c                   endif
312800170413      * Carico la VPODECO
312900170413     c                   clear                   tibs02ds
313000170413     c                   clear                   dvpodeco
313100170413     c                   eval      t02mod = 'C'
313200170413     c                   eval      t02sif = knsif
313300170413     c                   eval      t02cod = 'VPO'
313400170413     c                   eval      t02ke1 = 'DECO'
313500170413     c                   call      'TIBS02R'
313600170413     c                   parm                    kpjba
313700170413     c                   parm                    tibs02ds
313800170413     c                   if        t02err = *blanks
313900170413     c                   eval      dvpodeco = t02uni
314000170413     c                   else
314100170413     c                   eval      §vpoca  =20170501
314200170413     c                   endif
314300170413     c
314400051010     c
314500051010     c                   time                    wtempo
314600051010     c     *iso          move      wdata         dateu             8 0
314700051010     c
314800990209      * Accesso fndct fndcd
314900990129     c     kdct          klist
315000990128     c                   kfld                    i50aac
315100990128     c                   kfld                    i50fil
315200990128     c                   kfld                    i50nca
315300080714     c     kblp          klist
315400080714     c                   kfld                    dctaas
315500080714     c                   kfld                    dctlnp
315600080714     c                   kfld                    dctnrs
315700080714     c                   kfld                    dctnsp
315800090303     c     kar5          klist
315900090303     c                   kfld                    dctaas
316000090303     c                   kfld                    dctlnp
316100090303     c                   kfld                    dctnrs
316200090303     c                   kfld                    dctnsp
316300090303     c                   kfld                    ktrd
316400071018     c     kdcr2         klist
316500071018     c                   kfld                    i50aac
316600071018     c                   kfld                    i50fil
316700071018     c                   kfld                    i50nca
316800071018     c                   kfld                    w0070
316900990129     c     kdcr          klist
317000990129     c                   kfld                    i50aac
317100990129     c                   kfld                    i50fil
317200990129     c                   kfld                    i50nca
317300990216     c                   kfld                    wsegnadan
317400990216     c                   kfld                    wpor
317500990129     c     kTAB          klist
317600990129     c                   kfld                    codut
317700990129     c                   kfld                    cod
317800990129     c                   kfld                    key
317900990208     c     kTAB2         klist
318000990208     c                   kfld                    codut
318100990208     c                   kfld                    cod
318200990304     c     KTBE          klist
318300990304     c                   kfld                    wcod
318400990304     c                   kfld                    wlin
318500990304     c                   kfld                    wsif
318600990304     c                   kfld                    wke1
318700990304     c                   kfld                    wke2
318800990304     c     KTBE2         klist
318900990304     c                   kfld                    wcod
319000990304     c                   kfld                    wlin
319100990304     c                   kfld                    wsif
319200990304     c                   kfld                    wke1
319300990129     c     kDCD          klist
319400990129     c                   kfld                    dcdfls
319500990129     c                   kfld                    dcdlna
319600990129     c                   kfld                    dcdnrs
319700990208     c                   kfld                    dcdnsc
319800070711     c     kBRVE         klist
319900070821     c                   kfld                    wnpgspuan
320000070711     c                   kfld                    wnfvA
320100070821     c                   kfld                    wfgsspuan
320200070711     c                   kfld                    dcdfls
320300070711     c                   kfld                    dcdlna
320400070711     c                   kfld                    dcdnrs
320500070711     c                   kfld                    dcdnsc
320600990129     C     KCLN          KLIST
320700990129     C                   KFLD                    KTFP
320800990129     C                   KFLD                    KTFA
320900990129     C                   KFLD                    KAAA              4 0
321000990129     C                   KFLD                    KMMM              2 0
321100990305     c     keve          klist
321200990305     c                   kfld                    dctaae
321300990305     c                   kfld                    dctnev
321400990204     C                   MOVEL     savKPJBU      kpjbu
321500040907     c     kfgv          klist
321600040907     c                   kfld                    wnfv
321700040907     c                   kfld                    wfgs
321800980717      *
321900980506     C                   ENDSR
322000980508      *****************************************************************
322100980508** MSG  (Lungh. 78)                                                          *
322200990208Tipo anomalia inesistente                                                          1
322300990208C.A. inesistente                                                                   2
322400990216Impossibile aggiornare la C.A.: allocata da altro lavoro                           3
322500990208Impossibile calcolare Responsabilita':mancano dett.colli CA/Spunte/Riserva         4
322600990208Manca la tabella per il calcolo della responsabilita': AVVISARE IL CED !!!         5
322700990208Manca "num.colli danneggiati"e"num.colli della bolla":impossibile il calcolo       6
322800990208Non trovato dettaglio colli bolla per la C.A.                                      7
322900990208Tipo spedizione <> "A" e la responsabilita'non e'della PARTENZA: IMPOSSIBILE!!     8
